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

val request_of_bencode : Nrepl_bencode.t -> (request, string) result
val response_to_bencode : response -> Nrepl_bencode.t
