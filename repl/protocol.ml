let protocol_version = 1
let max_frame_bytes = 8 * 1024 * 1024

type source_request = {
  id : string;
  source : string;
  expected_namespace : string option;
  filename : string option;
}

type lookup_request = {
  id : string;
  symbol : string;
  expected_namespace : string option;
}

type completions_request = {
  id : string;
  prefix : string;
  expected_namespace : string option;
}

type request =
  | Evaluate of source_request
  | Type_of of source_request
  | Lookup of lookup_request
  | Completions of completions_request
  | Describe of { id : string }
  | Close of { id : string }

type status = Done | Failed | Unsupported

type target = Native_bytecode

type diagnostic_phase =
  | Lexing
  | Parsing
  | Semantic
  | Lowering
  | Ocaml
  | Infrastructure
  | Protocol

type lookup = {
  name : string;
  namespace : string;
  type_name : string option;
  file : string option;
  line : int option;
  column : int option;
}

type completion = {
  candidate : string;
  type_name : string option;
}

type response =
  | Description of {
      id : string;
      protocol_version : int;
      target : target;
      namespace : string;
      interrupt_supported : bool;
    }
  | Stdout of {
      id : string;
      text : string;
    }
  | Stderr of {
      id : string;
      text : string;
    }
  | Value of {
      id : string;
      rendered : string;
      type_name : string;
    }
  | Definition of {
      id : string;
      name : string;
      type_name : string;
    }
  | Summary of {
      id : string;
      text : string;
    }
  | Namespace of {
      id : string;
      namespace : string;
    }
  | Type_result of {
      id : string;
      type_name : string;
    }
  | Lookup_result of {
      id : string;
      result : lookup option;
    }
  | Completions_result of {
      id : string;
      candidates : completion list;
    }
  | Diagnostic of {
      id : string;
      code : string;
      phase : diagnostic_phase;
      message : string;
      location : string option;
    }
  | Status of {
      id : string;
      namespace : string;
      status : status;
    }

let field name fields = List.assoc_opt name fields

let object_fields = function
  | `Assoc fields -> Ok fields
  | _ -> Error "protocol message must be a JSON object"

let required_string name fields =
  match field name fields with
  | Some (`String value) -> Ok value
  | Some _ -> Error ("protocol field " ^ name ^ " must be a string")
  | None -> Error ("protocol message is missing field " ^ name)

let required_int name fields =
  match field name fields with
  | Some (`Int value) -> Ok value
  | Some _ -> Error ("protocol field " ^ name ^ " must be an integer")
  | None -> Error ("protocol message is missing field " ^ name)

let required_bool name fields =
  match field name fields with
  | Some (`Bool value) -> Ok value
  | Some _ -> Error ("protocol field " ^ name ^ " must be a boolean")
  | None -> Error ("protocol message is missing field " ^ name)

let optional_string name fields =
  match field name fields with
  | None | Some `Null -> Ok None
  | Some (`String value) -> Ok (Some value)
  | Some _ -> Error ("protocol field " ^ name ^ " must be a string or null")

let optional_int name fields =
  match field name fields with
  | None | Some `Null -> Ok None
  | Some (`Int value) -> Ok (Some value)
  | Some _ -> Error ("protocol field " ^ name ^ " must be an integer or null")

let validate_version fields =
  match field "version" fields with
  | Some (`Int version) when version = protocol_version -> Ok ()
  | Some (`Int version) ->
      Error
        (Printf.sprintf "unsupported REPL protocol version %d" version)
  | Some _ -> Error "protocol field version must be an integer"
  | None -> Error "protocol message is missing field version"

let source_request fields =
  match
    ( required_string "id" fields,
      required_string "source" fields,
      optional_string "expected_namespace" fields,
      optional_string "filename" fields )
  with
  | Ok id, Ok source, Ok expected_namespace, Ok filename ->
      Ok { id; source; expected_namespace; filename }
  | Error message, _, _, _ | _, Error message, _, _
  | _, _, Error message, _ | _, _, _, Error message -> Error message

let query_request field_name fields make =
  match
    ( required_string "id" fields,
      required_string field_name fields,
      optional_string "expected_namespace" fields )
  with
  | Ok id, Ok query, Ok expected_namespace -> make id query expected_namespace
  | Error message, _, _ | _, Error message, _ | _, _, Error message ->
      Error message

let request_to_json request =
  let base op id =
    [
      ("version", `Int protocol_version);
      ("op", `String op);
      ("id", `String id);
    ]
  in
  let source_fields op (request : source_request) =
    base op request.id
    @ [ ("source", `String request.source) ]
    @ (match request.expected_namespace with
      | None -> []
      | Some namespace -> [ ("expected_namespace", `String namespace) ])
    @
    (match request.filename with
    | None -> []
    | Some filename -> [ ("filename", `String filename) ])
  in
  let query_fields op field_name id query expected_namespace =
    base op id @ [ (field_name, `String query) ]
    @
    match expected_namespace with
    | None -> []
    | Some namespace -> [ ("expected_namespace", `String namespace) ]
  in
  `Assoc
    (match request with
    | Evaluate request -> source_fields "evaluate" request
    | Type_of request -> source_fields "type_of" request
    | Lookup request ->
        query_fields "lookup" "symbol" request.id request.symbol
          request.expected_namespace
    | Completions request ->
        query_fields "completions" "prefix" request.id request.prefix
          request.expected_namespace
    | Describe { id } -> base "describe" id
    | Close { id } -> base "close" id)

let request_of_json json =
  match object_fields json with
  | Error _ as error -> error
  | Ok fields -> (
      match (validate_version fields, required_string "op" fields) with
      | Error message, _ | _, Error message -> Error message
      | Ok (), Ok op -> (
          match op with
          | "evaluate" -> Result.map (fun value -> Evaluate value) (source_request fields)
          | "type_of" -> Result.map (fun value -> Type_of value) (source_request fields)
          | "lookup" ->
              query_request "symbol" fields (fun id symbol expected_namespace ->
                  Ok (Lookup { id; symbol; expected_namespace }))
          | "completions" ->
              query_request "prefix" fields (fun id prefix expected_namespace ->
                  Ok (Completions { id; prefix; expected_namespace }))
          | "describe" ->
              Result.map (fun id -> Describe { id })
                (required_string "id" fields)
          | "close" ->
              Result.map (fun id -> Close { id }) (required_string "id" fields)
          | _ -> Error ("unknown REPL protocol operation " ^ op)))

let status_name = function
  | Done -> "done"
  | Failed -> "failed"
  | Unsupported -> "unsupported"

let status_of_name = function
  | "done" -> Ok Done
  | "failed" -> Ok Failed
  | "unsupported" -> Ok Unsupported
  | name -> Error ("unknown REPL status " ^ name)

let target_name = function Native_bytecode -> "native-bytecode"

let target_of_name = function
  | "native-bytecode" -> Ok Native_bytecode
  | name -> Error ("unknown REPL target " ^ name)

let diagnostic_phase_name = function
  | Lexing -> "lexing"
  | Parsing -> "parsing"
  | Semantic -> "semantic"
  | Lowering -> "lowering"
  | Ocaml -> "ocaml"
  | Infrastructure -> "infrastructure"
  | Protocol -> "protocol"

let diagnostic_phase_of_name = function
  | "lexing" -> Ok Lexing
  | "parsing" -> Ok Parsing
  | "semantic" -> Ok Semantic
  | "lowering" -> Ok Lowering
  | "ocaml" -> Ok Ocaml
  | "infrastructure" -> Ok Infrastructure
  | "protocol" -> Ok Protocol
  | name -> Error ("unknown diagnostic phase " ^ name)

let optional_json_string name = function
  | None -> []
  | Some value -> [ (name, `String value) ]

let optional_json_int name = function
  | None -> []
  | Some value -> [ (name, `Int value) ]

let lookup_to_json lookup =
  `Assoc
    ([ ("name", `String lookup.name); ("namespace", `String lookup.namespace) ]
    @ optional_json_string "type" lookup.type_name
    @ optional_json_string "file" lookup.file
    @ optional_json_int "line" lookup.line
    @ optional_json_int "column" lookup.column)

let completion_to_json completion =
  `Assoc
    ([ ("candidate", `String completion.candidate) ]
    @ optional_json_string "type" completion.type_name)

let lookup_of_json json =
  match object_fields json with
  | Error _ as error -> error
  | Ok fields -> (
      match
        ( required_string "name" fields,
          required_string "namespace" fields,
          optional_string "type" fields,
          optional_string "file" fields,
          optional_int "line" fields,
          optional_int "column" fields )
      with
      | Ok name, Ok namespace, Ok type_name, Ok file, Ok line, Ok column ->
          Ok { name; namespace; type_name; file; line; column }
      | Error message, _, _, _, _, _ | _, Error message, _, _, _, _
      | _, _, Error message, _, _, _ | _, _, _, Error message, _, _
      | _, _, _, _, Error message, _ | _, _, _, _, _, Error message ->
          Error message)

let completion_of_json json =
  match object_fields json with
  | Error _ as error -> error
  | Ok fields -> (
      match (required_string "candidate" fields, optional_string "type" fields) with
      | Ok candidate, Ok type_name -> Ok { candidate; type_name }
      | Error message, _ | _, Error message -> Error message)

let rec map_result function_ values =
  match values with
  | [] -> Ok []
  | value :: rest ->
      Result.bind (function_ value) (fun value ->
          Result.map (fun rest -> value :: rest) (map_result function_ rest))

let response_to_json response =
  let base tag id =
    [
      ("version", `Int protocol_version);
      ("tag", `String tag);
      ("id", `String id);
    ]
  in
  `Assoc
    (match response with
    | Description
        { id; protocol_version; target; namespace; interrupt_supported } ->
        base "description" id
        @ [
            ("protocol_version", `Int protocol_version);
            ("target", `String (target_name target));
            ("namespace", `String namespace);
            ("interrupt_supported", `Bool interrupt_supported);
          ]
    | Stdout { id; text } -> base "stdout" id @ [ ("text", `String text) ]
    | Stderr { id; text } -> base "stderr" id @ [ ("text", `String text) ]
    | Value { id; rendered; type_name } ->
        base "value" id
        @ [ ("rendered", `String rendered); ("type", `String type_name) ]
    | Definition { id; name; type_name } ->
        base "definition" id
        @ [ ("name", `String name); ("type", `String type_name) ]
    | Summary { id; text } -> base "summary" id @ [ ("text", `String text) ]
    | Namespace { id; namespace } ->
        base "namespace" id @ [ ("namespace", `String namespace) ]
    | Type_result { id; type_name } ->
        base "type" id @ [ ("type", `String type_name) ]
    | Lookup_result { id; result } ->
        base "lookup" id
        @ [ ("result", Option.fold ~none:`Null ~some:lookup_to_json result) ]
    | Completions_result { id; candidates } ->
        base "completions" id
        @ [ ("candidates", `List (List.map completion_to_json candidates)) ]
    | Diagnostic { id; code; phase; message; location } ->
        base "diagnostic" id
        @ [
            ("code", `String code);
            ("phase", `String (diagnostic_phase_name phase));
            ("message", `String message);
          ]
        @
        (match location with
        | None -> []
        | Some location -> [ ("location", `String location) ])
    | Status { id; namespace; status } ->
        base "status" id
        @ [
            ("namespace", `String namespace);
            ("status", `String (status_name status));
          ])

let response_of_json json =
  let pair first second make =
    match (first, second) with
    | Ok first, Ok second -> Ok (make first second)
    | Error message, _ | _, Error message -> Error message
  in
  match object_fields json with
  | Error _ as error -> error
  | Ok fields -> (
      match
        ( validate_version fields,
          required_string "tag" fields,
          required_string "id" fields )
      with
      | Error message, _, _ | _, Error message, _ | _, _, Error message ->
          Error message
      | Ok (), Ok tag, Ok id -> (
          match tag with
          | "description" -> (
              match
                ( required_int "protocol_version" fields,
                  required_string "target" fields,
                  required_string "namespace" fields,
                  required_bool "interrupt_supported" fields )
              with
              | ( Ok protocol_version,
                  Ok target_name,
                  Ok namespace,
                  Ok interrupt_supported ) ->
                  Result.map
                    (fun target ->
                      Description
                        {
                          id;
                          protocol_version;
                          target;
                          namespace;
                          interrupt_supported;
                        })
                    (target_of_name target_name)
              | Error message, _, _, _ | _, Error message, _, _
              | _, _, Error message, _ | _, _, _, Error message ->
                  Error message)
          | "stdout" ->
              Result.map (fun text -> Stdout { id; text })
                (required_string "text" fields)
          | "stderr" ->
              Result.map (fun text -> Stderr { id; text })
                (required_string "text" fields)
          | "value" ->
              pair (required_string "rendered" fields)
                (required_string "type" fields)
                (fun rendered type_name -> Value { id; rendered; type_name })
          | "definition" ->
              pair (required_string "name" fields)
                (required_string "type" fields)
                (fun name type_name -> Definition { id; name; type_name })
          | "summary" ->
              Result.map (fun text -> Summary { id; text })
                (required_string "text" fields)
          | "namespace" ->
              Result.map (fun namespace -> Namespace { id; namespace })
                (required_string "namespace" fields)
          | "type" ->
              Result.map (fun type_name -> Type_result { id; type_name })
                (required_string "type" fields)
          | "lookup" -> (
              match field "result" fields with
              | Some `Null -> Ok (Lookup_result { id; result = None })
              | Some json ->
                  Result.map
                    (fun result -> Lookup_result { id; result = Some result })
                    (lookup_of_json json)
              | None -> Error "protocol message is missing field result")
          | "completions" -> (
              match field "candidates" fields with
              | Some (`List values) ->
                  Result.map
                    (fun candidates -> Completions_result { id; candidates })
                    (map_result completion_of_json values)
              | Some _ -> Error "protocol field candidates must be a list"
              | None -> Error "protocol message is missing field candidates")
          | "diagnostic" -> (
              match
                ( required_string "code" fields,
                  required_string "phase" fields,
                  required_string "message" fields,
                  optional_string "location" fields )
              with
              | Ok code, Ok phase_name, Ok message, Ok location ->
                  Result.map
                    (fun phase ->
                      Diagnostic { id; code; phase; message; location })
                    (diagnostic_phase_of_name phase_name)
              | Error message, _, _, _ | _, Error message, _, _
              | _, _, Error message, _ | _, _, _, Error message ->
                  Error message)
          | "status" ->
              Result.bind
                (pair (required_string "namespace" fields)
                   (required_string "status" fields)
                   (fun namespace status -> (namespace, status)))
                (fun (namespace, status) ->
                  Result.map
                    (fun status -> Status { id; namespace; status })
                    (status_of_name status))
          | _ -> Error ("unknown REPL protocol response tag " ^ tag)))

let write_frame output json =
  let payload = Yojson.Safe.to_string json in
  let length = String.length payload in
  if length > max_frame_bytes then Error "REPL frame exceeds maximum size"
  else
    try
      output_byte output ((length lsr 24) land 0xff);
      output_byte output ((length lsr 16) land 0xff);
      output_byte output ((length lsr 8) land 0xff);
      output_byte output (length land 0xff);
      output_string output payload;
      flush output;
      Ok ()
    with Sys_error message -> Error ("unable to write REPL frame: " ^ message)

let read_frame input =
  let next_byte () = try Ok (input_byte input) with End_of_file -> Error () in
  match next_byte () with
  | Error () -> Ok None
  | Ok first -> (
      match (next_byte (), next_byte (), next_byte ()) with
      | Ok second, Ok third, Ok fourth ->
          let length =
            (first lsl 24) lor (second lsl 16) lor (third lsl 8) lor fourth
          in
          if length > max_frame_bytes then
            Error "REPL frame exceeds maximum size"
          else
            (try
               let payload = really_input_string input length in
               try Ok (Some (Yojson.Safe.from_string payload))
               with Yojson.Json_error message ->
                 Error ("invalid REPL JSON frame: " ^ message)
             with End_of_file -> Error "truncated REPL frame payload")
      | _ -> Error "truncated REPL frame header")

let write_request output request = write_frame output (request_to_json request)

let read_request input =
  Result.bind (read_frame input) (function
    | None -> Ok None
    | Some json -> Result.map Option.some (request_of_json json))

let write_response output response =
  write_frame output (response_to_json response)

let read_response input =
  Result.bind (read_frame input) (function
    | None -> Ok None
    | Some json -> Result.map Option.some (response_of_json json))
