val protocol_version : int
val max_frame_bytes : int

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
  | Load_file of source_request
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

val request_to_json : request -> Yojson.Safe.t
val request_of_json : Yojson.Safe.t -> (request, string) result
val response_to_json : response -> Yojson.Safe.t
val response_of_json : Yojson.Safe.t -> (response, string) result

val write_request : out_channel -> request -> (unit, string) result
val read_request : in_channel -> (request option, string) result
val write_response : out_channel -> response -> (unit, string) result
val read_response : in_channel -> (response option, string) result
