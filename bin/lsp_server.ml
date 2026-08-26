open Yojson.Safe.Util

type document = {
  text : string;
  analysis : (Lg.Language_service.t, Lg.Error.t) result;
  recovered_analysis : Lg.Language_service.t option;
}

let documents = Hashtbl.create 16
let workspace_documents = Hashtbl.create 32
let workspace_sources = Hashtbl.create 32
let workspace_index = ref None
let supports_dynamic_watched_files = ref false

let analyze_document uri text =
  let analysis = Lg.Language_service.analyze ~filename:uri text in
  {
    text;
    analysis;
    recovered_analysis =
      (match analysis with
      | Ok analysis -> Some analysis
      | Error _ -> Lg.Language_service.recover_completed_prefix ~filename:uri text);
  }

let semantic_analysis document =
  match document.analysis with
  | Ok analysis -> Some analysis
  | Error _ -> document.recovered_analysis

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

let rec lg_files path =
  if Sys.is_directory path then
    Sys.readdir path |> Array.to_list
    |> List.filter (fun name -> not (excluded_directory name))
    |> List.concat_map (fun name -> lg_files (Filename.concat path name))
  else if Filename.check_suffix path ".cljc" then [ path ]
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
        Lg.Language_service.update_workspace_index index ~filename:uri ~source
    | _ ->
        Lg.Language_service.create_workspace_index sources
        |> Result.map (fun index -> (index, List.map fst sources))
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
        sources;
      List.map fst sources
  | Ok (index, affected) ->
      workspace_index := Some index;
      Hashtbl.clear workspace_documents;
      List.iter
        (fun (uri, text) ->
          match Lg.Language_service.workspace_analysis index uri with
          | None -> (
              match Lg.Language_service.workspace_error index uri with
              | None -> ()
              | Some error ->
                  let recovered = analyze_document uri text in
                  let document = { recovered with analysis = Error error } in
                  Hashtbl.replace workspace_documents uri document;
                  if Hashtbl.mem documents uri then
                    Hashtbl.replace documents uri document)
          | Some analysis ->
          let document = { text; analysis = Ok analysis; recovered_analysis = Some analysis } in
          Hashtbl.replace workspace_documents uri document;
          if Hashtbl.mem documents uri then Hashtbl.replace documents uri document)
        sources;
      affected

let refresh_workspace_document index uri =
  match Hashtbl.find_opt workspace_sources uri with
  | None -> Hashtbl.remove workspace_documents uri
  | Some disk_text ->
      let text =
        Hashtbl.find_opt documents uri
        |> Option.map (fun document -> document.text)
        |> Option.value ~default:disk_text
      in
      let analysis =
        match Lg.Language_service.workspace_analysis index uri with
        | Some analysis -> Ok analysis
        | None -> (
            match Lg.Language_service.workspace_error index uri with
            | Some error -> Error error
            | None -> Lg.Language_service.analyze ~filename:uri text)
      in
      let recovered_analysis =
        match analysis with
        | Ok analysis -> Some analysis
        | Error _ -> Lg.Language_service.recover_completed_prefix ~filename:uri text
      in
      let document = { text; analysis; recovered_analysis } in
      Hashtbl.replace workspace_documents uri document;
      if Hashtbl.mem documents uri then Hashtbl.replace documents uri document

let remove_workspace_source uri =
  Hashtbl.remove workspace_sources uri;
  match !workspace_index with
  | None -> rebuild_workspace ()
  | Some index -> (
      match Lg.Language_service.remove_workspace_file index ~filename:uri with
      | Error _ -> rebuild_workspace ()
      | Ok (index, affected) ->
          workspace_index := Some index;
          List.iter
            (fun affected_uri ->
              if affected_uri = uri then
                Hashtbl.remove workspace_documents affected_uri
              else refresh_workspace_document index affected_uri)
            affected;
          affected)

let update_watched_workspace_file uri change_type =
  if change_type = 3 then remove_workspace_source uri
  else
    let path = path_of_file_uri uri in
    if Filename.check_suffix path ".cljc" && Sys.file_exists path then (
      Hashtbl.replace workspace_sources uri (read_file path);
      rebuild_workspace ~changed_uri:uri ())
    else []

let index_workspace root_uri =
  Hashtbl.clear workspace_sources;
  workspace_index := None;
  path_of_file_uri root_uri |> lg_files
  |> List.iter (fun path ->
         let uri = "file://" ^ path in
         Hashtbl.replace workspace_sources uri (read_file path));
  ignore (rebuild_workspace ())

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

let position line character =
  `Assoc [ ("line", `Int line); ("character", `Int character) ]

let position_coordinates_of_offset text target =
  let rec loop offset line character =
    if offset >= target || offset >= String.length text then
      (line, character)
    else if text.[offset] = '\n' then loop (offset + 1) (line + 1) 0
    else
      let decoded = String.get_utf_8_uchar text offset in
      let byte_length = max 1 (Uchar.utf_decode_length decoded) in
      let codepoint = Uchar.utf_decode_uchar decoded |> Uchar.to_int in
      let units = if codepoint > 0xFFFF then 2 else 1 in
      loop (offset + byte_length) line (character + units)
  in
  loop 0 0 0

let position_of_offset text target =
  let line, character = position_coordinates_of_offset text target in
  position line character

let range_of_offsets text start_offset end_offset =
  `Assoc
    [ ("start", position_of_offset text start_offset);
      ("end", position_of_offset text end_offset) ]

let range_of_location text location =
  range_of_offsets text location.Location.loc_start.Lexing.pos_cnum
    location.loc_end.Lexing.pos_cnum

let diagnostic_range text = function
  | Some location -> range_of_location text location
  | None -> range_of_offsets text 0 (min 1 (String.length text))

let source_text_for_uri uri =
  match find_document uri with
  | Some document -> Some document.text
  | None -> (
      match Hashtbl.find_opt workspace_sources uri with
      | Some source -> Some source
      | None ->
          let path = path_of_file_uri uri in
          if Sys.file_exists path && not (Sys.is_directory path) then
            try Some (read_file path) with Sys_error _ -> None
          else None)

let line_start_range (location : Location.t) =
  let line = max 0 (location.loc_start.Lexing.pos_lnum - 1) in
  `Assoc [ ("start", position line 0); ("end", position line 0) ]

let related_range location =
  let uri = location.Location.loc_start.Lexing.pos_fname in
  match source_text_for_uri uri with
  | Some source -> range_of_location source location
  | None ->
      (* A byte column is not a valid LSP UTF-16 column. If the originating
         source is unavailable, preserve the exact line without inventing a
         misleading character range. *)
      line_start_range location

let diagnostic_phase = function
  | `Lexing -> "lexing"
  | `Parsing -> "parsing"
  | `Semantic -> "semantic"
  | `Lowering -> "lowering"
  | `Ocaml -> "ocaml"
  | `Infrastructure -> "infrastructure"

let rec type_term_json = function
  | Lg.Error.Type_atom name ->
      `Assoc [ ("kind", `String "atom"); ("name", `String name) ]
  | Type_application (name, arguments) ->
      `Assoc
        [ ("kind", `String "application");
          ("name", `String name);
          ("arguments", `List (List.map type_term_json arguments)) ]
  | Type_function (parameters, return_type) ->
      `Assoc
        [ ("kind", `String "function");
          ("parameters", `List (List.map type_term_json parameters));
          ("returnType", type_term_json return_type) ]
  | Type_tuple items ->
      `Assoc
        [ ("kind", `String "tuple");
          ("items", `List (List.map type_term_json items)) ]
  | Type_record fields ->
      `Assoc
        [ ("kind", `String "record");
          ( "fields",
            `List
              (List.map
                 (fun (name, ty) ->
                   `Assoc [ ("name", `String name); ("type", type_term_json ty) ])
                 fields) ) ]

let type_path_json = function
  | Lg.Error.Type_argument index ->
      `Assoc [ ("kind", `String "typeArgument"); ("index", `Int index) ]
  | Function_parameter index ->
      `Assoc [ ("kind", `String "functionParameter"); ("index", `Int index) ]
  | Function_return -> `Assoc [ ("kind", `String "functionReturn") ]
  | Tuple_item index ->
      `Assoc [ ("kind", `String "tupleItem"); ("index", `Int index) ]
  | Record_field name ->
      `Assoc [ ("kind", `String "recordField"); ("name", `String name) ]

let type_context_json = function
  | Lg.Error.Conditional_branch ->
      `Assoc [ ("kind", `String "conditionalBranch") ]
  | Record_property { record_name; property_name } ->
      `Assoc
        [ ("kind", `String "recordProperty");
          ("record", `String record_name);
          ("property", `String property_name) ]
  | Call_argument { callee; index } ->
      `Assoc
        [ ("kind", `String "callArgument");
          ("callee", `String callee);
          ("index", `Int index) ]
  | Protocol_argument { protocol; method_name; index } ->
      `Assoc
        [ ("kind", `String "protocolArgument");
          ("protocol", `String protocol);
          ("method", `String method_name);
          ("index", `Int index) ]
  | Annotation -> `Assoc [ ("kind", `String "annotation") ]
  | Host_boundary { callee; index } ->
      `Assoc
        [ ("kind", `String "hostBoundary");
          ("callee", `String callee);
          ("index", `Int index) ]

let type_mismatch_json (mismatch : Lg.Error.type_mismatch) =
  `Assoc
    [ ("context", type_context_json mismatch.context);
      ("expected", type_term_json mismatch.expected);
      ("actual", type_term_json mismatch.actual);
      ( "difference",
        `Assoc
          [ ("path", `List (List.map type_path_json mismatch.difference.path));
            ("expected", type_term_json mismatch.difference.expected);
            ("actual", type_term_json mismatch.difference.actual) ] ) ]

let diagnostic text ?(severity = 1) ?location ?code ?phase ?title
    ?(related = []) ?(hints = []) ?type_mismatch message =
  let identity =
    match (code, phase) with
    | Some code, Some phase ->
        let data =
          [ ("phase", `String (diagnostic_phase phase)) ]
          @ Option.fold ~none:[]
              ~some:(fun mismatch ->
                [ ("typeMismatch", type_mismatch_json mismatch) ])
              type_mismatch
        in
        [ ("code", `String code);
          ("data", `Assoc data) ]
    | _ -> []
  in
  let message =
    String.concat "\n\n"
      (Option.to_list title @ [ message ]
      @ List.map (fun hint -> "Hint: " ^ hint) hints)
  in
  let related_information =
    match related with
    | [] -> []
    | related ->
        [
          ( "relatedInformation",
            `List
              (List.map
                 (fun (related : Lg.Error.related) ->
                   let uri = related.location.Location.loc_start.Lexing.pos_fname in
                   `Assoc
                     [
                       ( "location",
                         `Assoc
                           [
                             ("uri", `String uri);
                             ("range", related_range related.location);
                           ] );
                       ("message", `String related.message);
                     ])
                 related) );
        ]
  in
  `Assoc
    ([ ("range", diagnostic_range text location);
       ("severity", `Int severity);
       ("source", `String "lg");
       ("message", `String message) ]
    @ identity @ related_information)

let diagnostics document =
  match document.analysis with
  | Ok analysis ->
      List.map
        (fun (item : Lg.Compiler.diagnostic) ->
          match item.severity with
          | `Warning ->
              diagnostic document.text ~severity:2 ?location:item.location
                ~code:item.code ~phase:item.phase item.message)
        (Lg.Language_service.diagnostics analysis)
  | Error err ->
      [ diagnostic document.text ?location:err.location ~code:err.code
          ~phase:err.phase ~title:err.title ~related:err.related ~hints:err.hints
          ?type_mismatch:err.type_mismatch err.message ]

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

let publish_current_diagnostics uri =
  match find_document uri with
  | None -> publish_diagnostics uri []
  | Some document -> publish_diagnostics uri (diagnostics document)

let rebuild_and_publish uri =
  let affected =
    if Hashtbl.mem workspace_sources uri then
      rebuild_workspace ~changed_uri:uri ()
    else [ uri ]
  in
  let affected = if List.mem uri affected then affected else uri :: affected in
  List.iter publish_current_diagnostics affected

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

let register_watched_files () =
  write_packet
    (`Assoc
      [ ("jsonrpc", `String "2.0");
        ("id", `String "lg-watch-lg-files");
        ("method", `String "client/registerCapability");
        ( "params",
          `Assoc
            [ ( "registrations",
                `List
                  [ `Assoc
                      [ ("id", `String "lg-watch-lg-files");
                        ( "method",
                          `String "workspace/didChangeWatchedFiles" );
                        ( "registerOptions",
                          `Assoc
                            [ ( "watchers",
                                `List
                                  [ `Assoc
                                      [ ( "globPattern",
                                          `String "**/*.cljc" );
                                        ("kind", `Int 7) ] ] ) ] ) ] ] ) ] ) ])

let initialize_result =
  `Assoc
    [ ( "capabilities",
        `Assoc
          [ ("textDocumentSync", `Int 1);
            ("hoverProvider", `Bool true);
            ("definitionProvider", `Bool true);
            ("documentFormattingProvider", `Bool true);
            ("codeActionProvider", `Bool true);
            ("referencesProvider", `Bool true);
            ("documentHighlightProvider", `Bool true);
            ("renameProvider", `Assoc [ ("prepareProvider", `Bool true) ]);
            ("documentSymbolProvider", `Bool true);
            ("workspaceSymbolProvider", `Bool true);
            ( "semanticTokensProvider",
              `Assoc
                [ ( "legend",
                    `Assoc
                      [ ( "tokenTypes",
                          `List
                            (List.map
                               (fun token_type -> `String token_type)
                               [ "namespace";
                                 "type";
                                 "function";
                                 "variable";
                                 "parameter";
                                 "property";
                                 "enumMember";
                                 "interface";
                                 "method";
                                 "keyword";
                                 "string";
                                 "number" ] ) );
                        ("tokenModifiers", `List []) ] );
                  ("full", `Bool true) ] );
            ( "completionProvider",
              `Assoc [ ("triggerCharacters", `List []) ] );
            ( "signatureHelpProvider",
              `Assoc
                [ ( "triggerCharacters",
                    `List [ `String " "; `String "(" ] ) ] ) ] );
      ( "serverInfo",
        `Assoc
          [ ("name", `String "lg"); ("version", `String "0.1") ] ) ]

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

let document_position params document =
  let position = params |> member "position" in
  let line = position |> member "line" |> to_int in
  let character = position |> member "character" |> to_int in
  offset_of_position document.text line character

let hover_result document offset =
  match semantic_analysis document with
  | None -> `Null
  | Some analysis -> (
      match Lg.Language_service.hover analysis ~offset with
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

let signature_help_result document offset =
  match semantic_analysis document with
  | None -> `Null
  | Some analysis -> (
      match Lg.Language_service.signature_help analysis ~offset with
      | None -> `Null
      | Some signature ->
          `Assoc
            [ ( "signatures",
                `List
                  [ `Assoc
                      [ ("label", `String signature.label);
                        ( "parameters",
                          `List
                            (List.map
                               (fun label ->
                                 `Assoc [ ("label", `String label) ])
                               signature.parameters) ) ] ] );
              ("activeSignature", `Int 0);
              ("activeParameter", `Int signature.active_parameter) ])

let definition_result uri document offset =
  match semantic_analysis document with
  | None -> `Null
  | Some analysis -> (
      match Lg.Language_service.definition analysis ~offset with
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
  match semantic_analysis document with
  | None -> `List []
  | Some analysis ->
      Lg.Language_service.completions analysis ~offset
      |> List.map (fun (item : Lg.Language_service.completion_item) ->
             `Assoc
               [ ("label", `String item.label);
                 ("kind", `Int 6);
                 ("detail", `String item.detail) ])
      |> fun items ->
      `Assoc [ ("isIncomplete", `Bool false); ("items", `List items) ]

let formatting_result document =
  match Lg.Formatter.format document.text with
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

let code_actions_result uri document =
  match document.analysis with
  | Ok _ -> `List []
  | Error error ->
      error.fixes
      |> List.map (fun (fix : Lg.Error.fix) ->
             `Assoc
               [ ("title", `String fix.title);
                 ("kind", `String "quickfix");
                 ("isPreferred", `Bool true);
                 ( "edit",
                   `Assoc
                     [ ( "changes",
                         `Assoc
                           [ ( uri,
                               `List
                                 (List.map
                                    (fun (edit : Lg.Error.text_edit) ->
                                      `Assoc
                                        [ ( "range",
                                            range_of_location document.text
                                              edit.location );
                                          ("newText", `String edit.replacement) ])
                                    fix.edits) ) ] ) ] ) ])
      |> fun actions -> `List actions

let location_json uri text (range : Lg.Ast.source_span) =
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
  match semantic_analysis document with
  | None -> `List []
  | Some analysis -> (
      match Lg.Language_service.semantic_key_at analysis ~offset with
      | None -> `List []
      | Some key ->
          Hashtbl.fold
            (fun uri document locations ->
              match semantic_analysis document with
              | None -> locations
              | Some analysis ->
                  Lg.Language_service.references_to_key analysis key
                  |> List.map (location_json uri document.text)
                  |> List.rev_append locations)
            (semantic_documents uri document) []
          |> List.rev |> fun locations -> `List locations)

let highlights_result document offset =
  match semantic_analysis document with
  | None -> `List []
  | Some analysis ->
      Lg.Language_service.references analysis ~offset
      |> List.map (fun (range : Lg.Ast.source_span) ->
             `Assoc
               [ ( "range",
                   range_of_offsets document.text range.start_offset
                     range.end_offset );
                 ("kind", `Int 1) ])
      |> fun highlights -> `List highlights

let prepare_rename_result document offset =
  match semantic_analysis document with
  | None -> `Null
  | Some analysis -> (
      match Lg.Language_service.prepare_rename analysis ~offset with
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
  match semantic_analysis document with
  | None -> `Null
  | Some analysis -> (
      match Lg.Language_service.semantic_key_at analysis ~offset with
      | None -> `Null
      | Some key ->
          if not (Lg.Language_service.valid_rename_name new_name) then `Null
          else
            let changes =
              Hashtbl.fold
                (fun uri document changes ->
                  match semantic_analysis document with
                  | None -> changes
                  | Some analysis ->
                      let edits =
                        Lg.Language_service.references_to_key analysis key
                        |> List.map (fun (range : Lg.Ast.source_span) ->
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
  | `Method -> 6
  | `Field -> 8
  | `Constructor -> 9
  | `Interface -> 11
  | `Function -> 12
  | `Variable -> 13

let rec document_symbol_json text (symbol : Lg.Language_service.document_symbol) =
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
  match semantic_analysis document with
  | None -> `List []
  | Some analysis ->
      Lg.Language_service.document_symbols analysis
      |> List.map (document_symbol_json document.text)
      |> fun symbols -> `List symbols

let rec matching_workspace_symbols uri text query
    (symbol : Lg.Language_service.document_symbol) =
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
      match semantic_analysis document with
      | None -> symbols
      | Some analysis ->
          Lg.Language_service.document_symbols analysis
          |> List.concat_map
               (matching_workspace_symbols uri document.text query)
          |> List.rev_append symbols)
    (all_documents ()) []
  |> List.rev |> fun symbols -> `List symbols

let semantic_token_type = function
  | `Namespace -> 0
  | `Type -> 1
  | `Function -> 2
  | `Variable -> 3
  | `Parameter -> 4
  | `Property -> 5
  | `Enum_member -> 6
  | `Interface -> 7
  | `Method -> 8
  | `Keyword -> 9
  | `String -> 10
  | `Number -> 11

let semantic_token_segments text
    (token : Lg.Language_service.semantic_token) =
  let rec loop segment_start offset segments =
    if offset >= token.range.end_offset then
      if segment_start < offset then (segment_start, offset, token.kind) :: segments
      else segments
    else if text.[offset] = '\n' then
      let segments =
        if segment_start < offset then
          (segment_start, offset, token.kind) :: segments
        else segments
      in
      loop (offset + 1) (offset + 1) segments
    else
      let decoded = String.get_utf_8_uchar text offset in
      loop segment_start
        (offset + max 1 (Uchar.utf_decode_length decoded))
        segments
  in
  loop token.range.start_offset token.range.start_offset [] |> List.rev

let semantic_tokens_result document =
  match semantic_analysis document with
  | None -> `Assoc [ ("data", `List []) ]
  | Some analysis ->
      let segments =
        Lg.Language_service.semantic_tokens analysis
        |> List.concat_map (semantic_token_segments document.text)
      in
      let _, _, reversed_data =
        List.fold_left
          (fun (previous_line, previous_character, data)
               (start_offset, end_offset, kind) ->
            let line, character =
              position_coordinates_of_offset document.text start_offset
            in
            let end_line, end_character =
              position_coordinates_of_offset document.text end_offset
            in
            let length =
              if end_line = line then end_character - character else 0
            in
            let delta_line = line - previous_line in
            let delta_start =
              if delta_line = 0 then character - previous_character else character
            in
            ( line,
              character,
              0 :: semantic_token_type kind :: length :: delta_start :: delta_line
              :: data ))
          (0, 0, []) segments
      in
      `Assoc
        [ ( "data",
            `List
              (List.rev_map (fun value -> `Int value) reversed_data) ) ]

let handle_notification method_ params =
  match method_ with
  | "textDocument/didOpen" ->
      let document = params |> member "textDocument" in
      let uri = document |> member "uri" |> to_string in
      let text = document |> member "text" |> to_string in
      let document = analyze_document uri text in
      Hashtbl.replace documents uri document;
      rebuild_and_publish uri
  | "textDocument/didChange" ->
      let uri = document_uri params in
      let changes = params |> member "contentChanges" |> to_list in
      (match changes with
      | change :: _ ->
          let text = change |> member "text" |> to_string in
          let document = analyze_document uri text in
          Hashtbl.replace documents uri document;
          rebuild_and_publish uri
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
          rebuild_and_publish uri)
        text
  | "textDocument/didClose" ->
      let uri = document_uri params in
      Hashtbl.remove documents uri;
      let affected =
        if Hashtbl.mem workspace_sources uri then
          rebuild_workspace ~changed_uri:uri ()
        else []
      in
      List.iter
        (fun affected_uri ->
          if affected_uri <> uri then publish_current_diagnostics affected_uri)
        affected;
      publish_diagnostics uri []
  | "workspace/didChangeWatchedFiles" ->
      params |> member "changes" |> to_list
      |> List.concat_map (fun change ->
             let uri = change |> member "uri" |> to_string in
             let change_type = change |> member "type" |> to_int in
             update_watched_workspace_file uri change_type)
      |> List.sort_uniq String.compare
      |> List.iter publish_current_diagnostics
  | "initialized" ->
      if !supports_dynamic_watched_files then register_watched_files ()
  | "exit" -> ()
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
          supports_dynamic_watched_files :=
            (params |> member "capabilities" |> member "workspace"
           |> member "didChangeWatchedFiles" |> member "dynamicRegistration"
           |> to_bool_option)
            = Some true;
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
      | Some ("textDocument/completion" as method_), (`Int _ | `String _)
      | Some ("textDocument/signatureHelp" as method_), (`Int _ | `String _) ->
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
                | "textDocument/signatureHelp" ->
                    signature_help_result document offset
                | _ -> `Null)
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
      | Some "textDocument/codeAction", (`Int _ | `String _) ->
          let uri = document_uri params in
          let result =
            match find_document uri with
            | None -> `List []
            | Some document -> code_actions_result uri document
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
                | _ -> `Null)
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
      | Some "textDocument/semanticTokens/full", (`Int _ | `String _) ->
          let uri = document_uri params in
          let result =
            match find_document uri with
            | None -> `Assoc [ ("data", `List []) ]
            | Some document -> semantic_tokens_result document
          in
          response id result;
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
