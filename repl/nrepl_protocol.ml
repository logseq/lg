module Bencode = Nrepl_bencode

type eval_request = {
  id : string option;
  session : string option;
  code : string;
  namespace : string option;
  file : string option;
  line : int option;
  column : int option;
}

type load_file_request = {
  id : string option;
  session : string option;
  contents : string;
  file_path : string option;
  file_name : string option;
}

type lookup_request = {
  id : string option;
  session : string option;
  symbol : string;
  namespace : string option;
}

type completions_request = {
  id : string option;
  session : string option;
  prefix : string;
  namespace : string option;
}

type request =
  | Describe of { id : string option }
  | Clone of {
      id : string option;
      source_session : string option;
    }
  | Close of {
      id : string option;
      session : string;
    }
  | Eval of eval_request
  | Load_file of load_file_request
  | Lookup of lookup_request
  | Completions of completions_request
  | Stdin of {
      id : string option;
      session : string option;
      input : string;
    }
  | Unknown of {
      id : string option;
      session : string option;
      op : string;
    }

type output_channel = Stdout | Stderr

type status =
  | Done
  | Unknown_op
  | Eval_error
  | Session_not_found
  | Clone_error
  | Stdin_unsupported
  | Server_error

type lookup_info = {
  name : string;
  namespace : string option;
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
      id : string option;
      ops : string list;
      lg_version : string;
    }
  | New_session of {
      id : string option;
      session : string;
    }
  | Output of {
      id : string option;
      session : string option;
      channel : output_channel;
      text : string;
    }
  | Value of {
      id : string option;
      session : string option;
      value : string;
      namespace : string;
      type_name : string option;
    }
  | Lookup_result of {
      id : string option;
      session : string option;
      info : lookup_info;
    }
  | Completions_result of {
      id : string option;
      session : string option;
      candidates : completion list;
    }
  | Error of {
      id : string option;
      session : string option;
      error_type : string;
      message : string;
    }
  | Status of {
      id : string option;
      session : string option;
      statuses : status list;
    }

let fields = function
  | Bencode.Dictionary fields -> Ok fields
  | _ -> Error "nREPL request must be a bencode dictionary"

let field name fields = List.assoc_opt name fields

let required_string name fields =
  match field name fields with
  | Some (Bencode.Byte_string value) -> Ok value
  | Some _ -> Error ("nREPL field " ^ name ^ " must be a byte string")
  | None -> Error ("nREPL request is missing field " ^ name)

let optional_string name fields =
  match field name fields with
  | None -> Ok None
  | Some (Bencode.Byte_string value) -> Ok (Some value)
  | Some _ -> Error ("nREPL field " ^ name ^ " must be a byte string")

let optional_int name fields =
  match field name fields with
  | None -> Ok None
  | Some (Bencode.Integer value)
    when Int64.compare value 0L >= 0
         && Int64.compare value (Int64.of_int max_int) <= 0 ->
      Ok (Some (Int64.to_int value))
  | Some (Bencode.Integer _) ->
      Error ("nREPL field " ^ name ^ " is outside the supported integer range")
  | Some _ -> Error ("nREPL field " ^ name ^ " must be an integer")

let ( let* ) = Result.bind

let request_of_bencode value =
  let* fields = fields value in
  let* op = required_string "op" fields in
  let* id = optional_string "id" fields in
  let* session = optional_string "session" fields in
  match op with
  | "describe" -> Ok (Describe { id })
  | "clone" -> Ok (Clone { id; source_session = session })
  | "close" -> (
      match session with
      | Some session -> Ok (Close { id; session })
      | None -> Error "nREPL close request is missing field session")
  | "eval" ->
      let* code = required_string "code" fields in
      let* namespace = optional_string "ns" fields in
      let* file = optional_string "file" fields in
      let* line = optional_int "line" fields in
      let* column = optional_int "column" fields in
      Ok (Eval { id; session; code; namespace; file; line; column })
  | "load-file" ->
      let* contents = required_string "file" fields in
      let* file_path = optional_string "file-path" fields in
      let* file_name = optional_string "file-name" fields in
      Ok (Load_file { id; session; contents; file_path; file_name })
  | "lookup" ->
      let* symbol = required_string "sym" fields in
      let* namespace = optional_string "ns" fields in
      Ok (Lookup { id; session; symbol; namespace })
  | "completions" ->
      let* prefix = required_string "prefix" fields in
      let* namespace = optional_string "ns" fields in
      Ok (Completions { id; session; prefix; namespace })
  | "stdin" ->
      let* input = required_string "stdin" fields in
      Ok (Stdin { id; session; input })
  | op -> Ok (Unknown { id; session; op })

let bytes value = Bencode.Byte_string value

let optional_field name = function
  | None -> []
  | Some value -> [ (name, bytes value) ]

let common_fields id session =
  optional_field "id" id @ optional_field "session" session

let optional_int_field name = function
  | None -> []
  | Some value -> [ (name, Bencode.Integer (Int64.of_int value)) ]

let lookup_fields info =
  [ ("name", bytes info.name) ]
  @ optional_field "ns" info.namespace
  @ optional_field "type" info.type_name
  @ optional_field "file" info.file
  @ optional_int_field "line" info.line
  @ optional_int_field "column" info.column

let completion_value completion =
  Bencode.Dictionary
    ([ ("candidate", bytes completion.candidate) ]
    @ optional_field "type" completion.type_name)

let status_name = function
  | Done -> "done"
  | Unknown_op -> "unknown-op"
  | Eval_error -> "eval-error"
  | Session_not_found -> "session-not-found"
  | Clone_error -> "clone-error"
  | Stdin_unsupported -> "stdin-unsupported"
  | Server_error -> "server-error"

let response_to_bencode = function
  | Description { id; ops; lg_version } ->
      Bencode.Dictionary
        (optional_field "id" id
        @ [
            ( "ops",
              Bencode.Dictionary
                (List.map (fun op -> (op, Bencode.Dictionary [])) ops) );
            ("versions", Bencode.Dictionary [ ("lg", bytes lg_version) ]);
            ("status", Bencode.List [ bytes "done" ]);
          ])
  | New_session { id; session } ->
      Bencode.Dictionary
        (optional_field "id" id
        @ [
            ("new-session", bytes session);
            ("status", Bencode.List [ bytes "done" ]);
          ])
  | Output { id; session; channel; text } ->
      let field = match channel with Stdout -> "out" | Stderr -> "err" in
      Bencode.Dictionary (common_fields id session @ [ (field, bytes text) ])
  | Value { id; session; value; namespace; type_name } ->
      Bencode.Dictionary
        (common_fields id session
        @ [ ("value", bytes value); ("ns", bytes namespace) ]
        @ optional_field "lg/type" type_name)
  | Lookup_result { id; session; info } ->
      Bencode.Dictionary
        (common_fields id session
        @ [ ("info", Bencode.Dictionary (lookup_fields info)) ])
  | Completions_result { id; session; candidates } ->
      Bencode.Dictionary
        (common_fields id session
        @ [ ("completions", Bencode.List (List.map completion_value candidates)) ])
  | Error { id; session; error_type; message } ->
      Bencode.Dictionary
        (common_fields id session
        @ [ ("ex", bytes error_type); ("err", bytes message) ])
  | Status { id; session; statuses } ->
      Bencode.Dictionary
        (common_fields id session
        @ [
            ( "status",
              Bencode.List
                (List.map (fun status -> bytes (status_name status)) statuses) );
          ])
