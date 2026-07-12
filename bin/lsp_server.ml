open Yojson.Safe.Util

let documents = Hashtbl.create 16

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

let diagnostic message =
  let line, start_character, end_character = diagnostic_position message in
  `Assoc
    [ ( "range",
        `Assoc
          [ ("start", position line start_character);
            ("end", position line end_character) ] );
      ("severity", `Int 1);
      ("source", `String "cljml");
      ("message", `String message) ]

let diagnostics uri text =
  match Cljml.Compiler.compile_string_with_filename ~filename:uri text with
  | Ok _ -> []
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
          [ ("textDocumentSync", `Int 1) ] );
      ( "serverInfo",
        `Assoc
          [ ("name", `String "cljml"); ("version", `String "0.1") ] ) ]

let document_uri params =
  params |> member "textDocument" |> member "uri" |> to_string

let handle_notification method_ params =
  match method_ with
  | "textDocument/didOpen" ->
      let document = params |> member "textDocument" in
      let uri = document |> member "uri" |> to_string in
      let text = document |> member "text" |> to_string in
      Hashtbl.replace documents uri text;
      publish_diagnostics uri (diagnostics uri text)
  | "textDocument/didChange" ->
      let uri = document_uri params in
      let changes = params |> member "contentChanges" |> to_list in
      (match changes with
      | change :: _ ->
          let text = change |> member "text" |> to_string in
          Hashtbl.replace documents uri text;
          publish_diagnostics uri (diagnostics uri text)
      | [] -> ())
  | "textDocument/didSave" ->
      let uri = document_uri params in
      let text =
        match params |> member "text" with
        | `String text -> Some text
        | _ -> Hashtbl.find_opt documents uri
      in
      Option.iter
        (fun text ->
          Hashtbl.replace documents uri text;
          publish_diagnostics uri (diagnostics uri text))
        text
  | "textDocument/didClose" ->
      let uri = document_uri params in
      Hashtbl.remove documents uri;
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
          response id initialize_result;
          loop shutdown_requested
      | Some "shutdown", (`Int _ | `String _) ->
          response id `Null;
          loop true
      | Some "exit", `Null -> if shutdown_requested then () else exit 1
      | Some method_, `Null ->
          handle_notification method_ params;
          loop shutdown_requested
      | Some method_, (`Int _ | `String _) ->
          error_response id (-32601) ("unsupported request " ^ method_);
          loop shutdown_requested
      | _ -> loop shutdown_requested)

let run () = loop false
