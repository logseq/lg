open Yojson.Safe.Util

type document = {
  text : string;
  analysis : (Cljml.Language_service.t, Cljml.Error.t) result;
}

let documents = Hashtbl.create 16
let workspace_documents = Hashtbl.create 32
let workspace_sources = Hashtbl.create 32
let workspace_index = ref None

let analyze_document uri text =
  {
    text;
    analysis = Cljml.Language_service.analyze ~filename:uri text;
  }

let path_of_file_uri uri =
  if String.starts_with ~prefix:"file://" uri then
    String.sub uri 7 (String.length uri - 7)
  else uri

let read_file path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in channel) (fun () ->
      really_input_string channel (in_channel_length channel))

let excluded_directory name =
  name = "_build" || name = "_opam" || name = "node_modules"
  || name = ".git" || (String.length name > 0 && name.[0] = '.')

let rec cljml_files path =
  if Sys.is_directory path then
    Sys.readdir path |> Array.to_list
    |> List.filter (fun name -> not (excluded_directory name))
    |> List.concat_map (fun name -> cljml_files (Filename.concat path name))
  else if Filename.check_suffix path ".cljml" then [ path ]
  else []

let rebuild_workspace ?changed_uri () =
  let sources =
    Hashtbl.fold
      (fun uri disk_source sources ->
        let source =
          Hashtbl.find_opt documents uri
          |> Option.map (fun document -> document.text)
          |> Option.value ~default:disk_source
        in
        (uri, source) :: sources)
      workspace_sources []
  in
  let indexed =
    match (!workspace_index, changed_uri) with
    | Some index, Some uri ->
        let source = List.assoc uri sources in
        Cljml.Language_service.update_workspace_index index ~filename:uri ~source
        |> Result.map fst
    | _ -> Cljml.Language_service.create_workspace_index sources
  in
  match indexed with
  | Error _ ->
      workspace_index := None;
      Hashtbl.clear workspace_documents;
      List.iter
        (fun (uri, source) ->
          let document = analyze_document uri source in
          Hashtbl.replace workspace_documents uri document;
          if Hashtbl.mem documents uri then Hashtbl.replace documents uri document)
        sources
  | Ok index ->
      workspace_index := Some index;
      Hashtbl.clear workspace_documents;
      List.iter
        (fun (uri, text) ->
          match Cljml.Language_service.workspace_analysis index uri with
          | None -> (
              match Cljml.Language_service.workspace_error index uri with
              | None -> ()
              | Some error ->
                  let document = { text; analysis = Error error } in
                  Hashtbl.replace workspace_documents uri document;
                  if Hashtbl.mem documents uri then
                    Hashtbl.replace documents uri document)
          | Some analysis ->
          let document = { text; analysis = Ok analysis } in
          Hashtbl.replace workspace_documents uri document;
          if Hashtbl.mem documents uri then Hashtbl.replace documents uri document)
        sources

let index_workspace root_uri =
  Hashtbl.clear workspace_sources;
  workspace_index := None;
  path_of_file_uri root_uri |> cljml_files
  |> List.iter (fun path ->
         let uri = "file://" ^ path in
         Hashtbl.replace workspace_sources uri (read_file path));
  rebuild_workspace ()

let find_document uri =
  match Hashtbl.find_opt documents uri with
  | Some _ as document -> document
  | None -> Hashtbl.find_opt workspace_documents uri

let all_documents () =
  let combined = Hashtbl.copy workspace_documents in
  Hashtbl.iter (Hashtbl.replace combined) documents;
  combined

let find_substring text pattern =
  let pattern_length = String.length pattern in
  let rec loop index =
    if index + pattern_length > String.length text then None
    else if String.sub text index pattern_length = pattern then Some index
    else loop (index + 1)
  in
  if pattern_length = 0 then Some 0 else loop 0

let parse_integer text start =
  let rec finish index =
    if index < String.length text then
      match text.[index] with '0' .. '9' -> finish (index + 1) | _ -> index
    else index
  in
  let stop = finish start in
  if stop = start then None
  else int_of_string_opt (String.sub text start (stop - start))

let diagnostic_position message =
  let line =
    match find_substring message "line " with
    | None -> 0
    | Some index ->
        parse_integer message (index + 5)
        |> Option.map (fun line -> max 0 (line - 1))
        |> Option.value ~default:0
  in
  let start_character, end_character =
    match find_substring message "characters " with
    | None -> (0, 1)
    | Some index -> (
        let start_index = index + 11 in
        match parse_integer message start_index with
        | None -> (0, 1)
        | Some start_character ->
            let rec find_dash index =
              if index >= String.length message then None
              else if message.[index] = '-' then Some index
              else find_dash (index + 1)
            in
            let end_character =
              match find_dash start_index with
              | None -> start_character + 1
              | Some dash ->
                  parse_integer message (dash + 1)
                  |> Option.value ~default:(start_character + 1)
            in
            (start_character, max (start_character + 1) end_character))
  in
  (line, start_character, end_character)

let position line character =
  `Assoc [ ("line", `Int line); ("character", `Int character) ]

let diagnostic ?(severity = 1) message =
  let line, start_character, end_character = diagnostic_position message in
  `Assoc
    [ ( "range",
        `Assoc
          [ ("start", position line start_character);
            ("end", position line end_character) ] );
      ("severity", `Int severity);
      ("source", `String "cljml");
      ("message", `String message) ]

let diagnostics document =
  match document.analysis with
  | Ok analysis ->
      List.map
        (fun (item : Cljml.Compiler.diagnostic) ->
          match item.severity with
          | `Warning -> diagnostic ~severity:2 item.message)
        (Cljml.Language_service.diagnostics analysis)
  | Error err -> [ diagnostic err.message ]

let write_packet json =
  let body = Yojson.Safe.to_string json in
  Printf.printf "Content-Length: %d\r\n\r\n%s%!" (String.length body) body

let publish_diagnostics uri diagnostics =
  write_packet
    (`Assoc
      [ ("jsonrpc", `String "2.0");
        ("method", `String "textDocument/publishDiagnostics");
        ( "params",
          `Assoc
            [ ("uri", `String uri);
              ("diagnostics", `List diagnostics) ] ) ])

let read_packet () =
  let rec read_headers content_length =
    match input_line stdin with
    | exception End_of_file -> None
    | line ->
        let line = String.trim line in
        if line = "" then content_length
        else
          let prefix = "content-length:" in
          let lowercase = String.lowercase_ascii line in
          let content_length =
            if String.starts_with ~prefix lowercase then
              String.sub line (String.length prefix)
                (String.length line - String.length prefix)
              |> String.trim |> int_of_string_opt
            else content_length
          in
          read_headers content_length
  in
  match read_headers None with
  | None -> None
  | Some length -> Some (really_input_string stdin length |> Yojson.Safe.from_string)

let response id result =
  write_packet
    (`Assoc
      [ ("jsonrpc", `String "2.0"); ("id", id); ("result", result) ])

let error_response id code message =
  write_packet
    (`Assoc
      [ ("jsonrpc", `String "2.0");
        ("id", id);
        ( "error",
          `Assoc [ ("code", `Int code); ("message", `String message) ] ) ])

let initialize_result =
  `Assoc
    [ ( "capabilities",
        `Assoc
          [ ("textDocumentSync", `Int 1);
            ("hoverProvider", `Bool true);
            ("definitionProvider", `Bool true);
            ("documentFormattingProvider", `Bool true);
            ("referencesProvider", `Bool true);
            ("documentHighlightProvider", `Bool true);
            ("renameProvider", `Assoc [ ("prepareProvider", `Bool true) ]);
            ("documentSymbolProvider", `Bool true);
            ("workspaceSymbolProvider", `Bool true);
            ( "completionProvider",
              `Assoc [ ("triggerCharacters", `List []) ] ) ] );
      ( "serverInfo",
        `Assoc
          [ ("name", `String "cljml"); ("version", `String "0.1") ] ) ]

let document_uri params =
  params |> member "textDocument" |> member "uri" |> to_string

let line_start_offset text target_line =
  let rec loop offset line =
    if line = target_line then Some offset
    else if offset >= String.length text then None
    else if text.[offset] = '\n' then loop (offset + 1) (line + 1)
    else loop (offset + 1) line
  in
  if target_line < 0 then None else loop 0 0

let offset_of_position text line character =
  match line_start_offset text line with
  | None -> String.length text
  | Some start ->
      let rec loop offset utf16_units =
        if
          offset >= String.length text || text.[offset] = '\n'
          || utf16_units >= character
        then offset
        else
          let decoded = String.get_utf_8_uchar text offset in
          let byte_length = max 1 (Uchar.utf_decode_length decoded) in
          let codepoint = Uchar.utf_decode_uchar decoded |> Uchar.to_int in
          let units = if codepoint > 0xFFFF then 2 else 1 in
          loop (offset + byte_length) (utf16_units + units)
      in
      loop start 0

let position_of_offset text target =
  let rec loop offset line character =
    if offset >= target || offset >= String.length text then
      position line character
    else if text.[offset] = '\n' then loop (offset + 1) (line + 1) 0
    else
      let decoded = String.get_utf_8_uchar text offset in
      let byte_length = max 1 (Uchar.utf_decode_length decoded) in
      let codepoint = Uchar.utf_decode_uchar decoded |> Uchar.to_int in
      let units = if codepoint > 0xFFFF then 2 else 1 in
      loop (offset + byte_length) line (character + units)
  in
  loop 0 0 0

let range_of_offsets text start_offset end_offset =
  `Assoc
    [ ("start", position_of_offset text start_offset);
      ("end", position_of_offset text end_offset) ]

let range_of_location text location =
  range_of_offsets text location.Location.loc_start.Lexing.pos_cnum
    location.loc_end.Lexing.pos_cnum

let document_position params document =
  let position = params |> member "position" in
  let line = position |> member "line" |> to_int in
  let character = position |> member "character" |> to_int in
  offset_of_position document.text line character

let hover_result document offset =
  match document.analysis with
  | Error _ -> `Null
  | Ok analysis -> (
      match Cljml.Language_service.hover analysis ~offset with
      | None -> `Null
      | Some hover ->
          `Assoc
            [ ( "contents",
                `Assoc
                  [ ("kind", `String "plaintext");
                    ("value", `String hover.contents) ] );
              ( "range",
                range_of_offsets document.text hover.range.start_offset
                  hover.range.end_offset ) ])

let definition_result uri document offset =
  match document.analysis with
  | Error _ -> `Null
  | Ok analysis -> (
      match Cljml.Language_service.definition analysis ~offset with
      | None -> `Null
      | Some location ->
          let filename = location.Location.loc_start.Lexing.pos_fname in
          let definition_uri =
            if String.starts_with ~prefix:"file://" filename then filename
            else if filename = "" then uri
            else "file://" ^ filename
          in
          let definition_text =
            find_document definition_uri
            |> Option.map (fun document -> document.text)
            |> Option.value ~default:document.text
          in
          `Assoc
            [ ("uri", `String definition_uri);
              ("range", range_of_location definition_text location) ])

let completion_result document offset =
  match document.analysis with
  | Error _ -> `List []
  | Ok analysis ->
      Cljml.Language_service.completions analysis ~offset
      |> List.map (fun (item : Cljml.Language_service.completion_item) ->
             `Assoc
               [ ("label", `String item.label);
                 ("kind", `Int 6);
                 ("detail", `String item.detail) ])
      |> fun items ->
      `Assoc [ ("isIncomplete", `Bool false); ("items", `List items) ]

let formatting_result document =
  match Cljml.Formatter.format document.text with
  | Error _ -> `List []
  | Ok formatted when formatted = document.text -> `List []
  | Ok formatted ->
      `List
        [
          `Assoc
            [ ( "range",
                range_of_offsets document.text 0 (String.length document.text) );
              ("newText", `String formatted) ];
        ]

let location_json uri text (range : Cljml.Ast.source_span) =
  `Assoc
    [ ("uri", `String uri);
      ( "range",
        range_of_offsets text range.start_offset range.end_offset ) ]

let semantic_documents uri document =
  if Hashtbl.mem workspace_sources uri then workspace_documents
  else
    let local = Hashtbl.create 1 in
    Hashtbl.add local uri document;
    local

let references_result uri document offset =
  match document.analysis with
  | Error _ -> `List []
  | Ok analysis -> (
      match Cljml.Language_service.value_uid_at analysis ~offset with
      | None -> `List []
      | Some uid ->
          Hashtbl.fold
            (fun uri document locations ->
              match document.analysis with
              | Error _ -> locations
              | Ok analysis ->
                  Cljml.Language_service.references_to_uid analysis uid
                  |> List.map (location_json uri document.text)
                  |> List.rev_append locations)
            (semantic_documents uri document) []
          |> List.rev |> fun locations -> `List locations)

let highlights_result document offset =
  match document.analysis with
  | Error _ -> `List []
  | Ok analysis ->
      Cljml.Language_service.references analysis ~offset
      |> List.map (fun (range : Cljml.Ast.source_span) ->
             `Assoc
               [ ( "range",
                   range_of_offsets document.text range.start_offset
                     range.end_offset );
                 ("kind", `Int 1) ])
      |> fun highlights -> `List highlights

let prepare_rename_result document offset =
  match document.analysis with
  | Error _ -> `Null
  | Ok analysis -> (
      match Cljml.Language_service.prepare_rename analysis ~offset with
      | None -> `Null
      | Some range ->
          let placeholder =
            String.sub document.text range.start_offset
              (range.end_offset - range.start_offset)
          in
          `Assoc
            [ ( "range",
                range_of_offsets document.text range.start_offset range.end_offset );
              ("placeholder", `String placeholder) ])

let rename_result uri document offset new_name =
  match document.analysis with
  | Error _ -> `Null
  | Ok analysis -> (
      match Cljml.Language_service.value_uid_at analysis ~offset with
      | None -> `Null
      | Some uid ->
          if not (Cljml.Language_service.valid_rename_name new_name) then `Null
          else
            let changes =
              Hashtbl.fold
                (fun uri document changes ->
                  match document.analysis with
                  | Error _ -> changes
                  | Ok analysis ->
                      let edits =
                        Cljml.Language_service.references_to_uid analysis uid
                        |> List.map (fun (range : Cljml.Ast.source_span) ->
                               `Assoc
                                 [ ( "range",
                                     range_of_offsets document.text
                                       range.start_offset range.end_offset );
                                   ("newText", `String new_name) ])
                      in
                      if edits = [] then changes else (uri, `List edits) :: changes)
                (semantic_documents uri document) []
            in
            `Assoc [ ("changes", `Assoc (List.rev changes)) ])

let symbol_kind = function
  | `Module -> 2
  | `Type -> 5
  | `Interface -> 11
  | `Function -> 12
  | `Variable -> 13

let rec document_symbol_json text (symbol : Cljml.Language_service.document_symbol) =
  `Assoc
    [ ("name", `String symbol.name);
      ("kind", `Int (symbol_kind symbol.kind));
      ( "range",
        range_of_offsets text symbol.range.start_offset symbol.range.end_offset );
      ( "selectionRange",
        range_of_offsets text symbol.selection_range.start_offset
          symbol.selection_range.end_offset );
      ("children", `List (List.map (document_symbol_json text) symbol.children)) ]

let document_symbols_result document =
  match document.analysis with
  | Error _ -> `List []
  | Ok analysis ->
      Cljml.Language_service.document_symbols analysis
      |> List.map (document_symbol_json document.text)
      |> fun symbols -> `List symbols

let rec matching_workspace_symbols uri text query
    (symbol : Cljml.Language_service.document_symbol) =
  let children =
    List.concat_map (matching_workspace_symbols uri text query) symbol.children
  in
  if find_substring (String.lowercase_ascii symbol.name) query = None then children
  else
    `Assoc
      [ ("name", `String symbol.name);
        ("kind", `Int (symbol_kind symbol.kind));
        ("location", location_json uri text symbol.selection_range) ]
    :: children

let workspace_symbols_result query =
  let query = String.lowercase_ascii query in
  Hashtbl.fold
    (fun uri document symbols ->
      match document.analysis with
      | Error _ -> symbols
      | Ok analysis ->
          Cljml.Language_service.document_symbols analysis
          |> List.concat_map
               (matching_workspace_symbols uri document.text query)
          |> List.rev_append symbols)
    (all_documents ()) []
  |> List.rev |> fun symbols -> `List symbols

let handle_notification method_ params =
  match method_ with
  | "textDocument/didOpen" ->
      let document = params |> member "textDocument" in
      let uri = document |> member "uri" |> to_string in
      let text = document |> member "text" |> to_string in
      let document = analyze_document uri text in
      Hashtbl.replace documents uri document;
      if Hashtbl.mem workspace_sources uri then rebuild_workspace ~changed_uri:uri ();
      publish_diagnostics uri (diagnostics document)
  | "textDocument/didChange" ->
      let uri = document_uri params in
      let changes = params |> member "contentChanges" |> to_list in
      (match changes with
      | change :: _ ->
          let text = change |> member "text" |> to_string in
          let document = analyze_document uri text in
          Hashtbl.replace documents uri document;
          if Hashtbl.mem workspace_sources uri then
            rebuild_workspace ~changed_uri:uri ();
          publish_diagnostics uri (diagnostics document)
      | [] -> ())
  | "textDocument/didSave" ->
      let uri = document_uri params in
      let text =
        match params |> member "text" with
        | `String text -> Some text
        | _ -> Hashtbl.find_opt documents uri |> Option.map (fun doc -> doc.text)
      in
      Option.iter
        (fun text ->
          let document = analyze_document uri text in
          Hashtbl.replace documents uri document;
          if Hashtbl.mem workspace_sources uri then
            rebuild_workspace ~changed_uri:uri ();
          publish_diagnostics uri (diagnostics document))
        text
  | "textDocument/didClose" ->
      let uri = document_uri params in
      Hashtbl.remove documents uri;
      if Hashtbl.mem workspace_sources uri then rebuild_workspace ~changed_uri:uri ();
      publish_diagnostics uri []
  | "initialized" | "exit" -> ()
  | _ -> ()

let rec loop shutdown_requested =
  match read_packet () with
  | None -> ()
  | Some json ->
      let method_ = json |> member "method" |> to_string_option in
      let id = json |> member "id" in
      let params = json |> member "params" in
      (match (method_, id) with
      | Some "initialize", (`Int _ | `String _) ->
          (match params |> member "rootUri" with
          | `String root_uri -> index_workspace root_uri
          | _ -> ());
          response id initialize_result;
          loop shutdown_requested
      | Some "shutdown", (`Int _ | `String _) ->
          response id `Null;
          loop true
      | Some ("textDocument/hover" as method_), (`Int _ | `String _)
      | Some ("textDocument/definition" as method_), (`Int _ | `String _)
      | Some ("textDocument/completion" as method_), (`Int _ | `String _) ->
          let uri = document_uri params in
          let result =
            match find_document uri with
            | None -> `Null
            | Some document ->
                let offset = document_position params document in
                (match method_ with
                | "textDocument/hover" -> hover_result document offset
                | "textDocument/definition" ->
                    definition_result uri document offset
                | "textDocument/completion" ->
                    completion_result document offset
                | _ -> assert false)
          in
          response id result;
          loop shutdown_requested
      | Some "textDocument/formatting", (`Int _ | `String _) ->
          let uri = document_uri params in
          let result =
            match find_document uri with
            | None -> `List []
            | Some document -> formatting_result document
          in
          response id result;
          loop shutdown_requested
      | Some ("textDocument/references" as method_), (`Int _ | `String _)
      | Some ("textDocument/documentHighlight" as method_), (`Int _ | `String _)
      | Some ("textDocument/prepareRename" as method_), (`Int _ | `String _)
      | Some ("textDocument/rename" as method_), (`Int _ | `String _) ->
          let uri = document_uri params in
          let result =
            match find_document uri with
            | None -> `Null
            | Some document ->
                let offset = document_position params document in
                (match method_ with
                | "textDocument/references" ->
                    references_result uri document offset
                | "textDocument/documentHighlight" ->
                    highlights_result document offset
                | "textDocument/prepareRename" ->
                    prepare_rename_result document offset
                | "textDocument/rename" ->
                    let new_name = params |> member "newName" |> to_string in
                    rename_result uri document offset new_name
                | _ -> assert false)
          in
          response id result;
          loop shutdown_requested
      | Some "textDocument/documentSymbol", (`Int _ | `String _) ->
          let uri = document_uri params in
          let result =
            match find_document uri with
            | None -> `List []
            | Some document -> document_symbols_result document
          in
          response id result;
          loop shutdown_requested
      | Some "workspace/symbol", (`Int _ | `String _) ->
          let query = params |> member "query" |> to_string in
          response id (workspace_symbols_result query);
          loop shutdown_requested
      | Some "exit", `Null -> if shutdown_requested then () else exit 1
      | Some method_, `Null ->
          handle_notification method_ params;
          loop shutdown_requested
      | Some method_, (`Int _ | `String _) ->
          error_response id (-32601) ("unsupported request " ^ method_);
          loop shutdown_requested
      | _ -> loop shutdown_requested)

let run () = loop false
