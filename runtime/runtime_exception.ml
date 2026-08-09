exception Exception_info of string * Runtime_dynamic.t * exn option

let ex_info message data = Exception_info (message, data, None)

let ex_info_with_cause message data cause =
  Exception_info (message, data, Some cause)

let create message = Exception_info (message, Runtime_dynamic.nil, None)

let integer_overflow function_name =
  let data =
    Runtime_dynamic.map
      [
        ( Runtime_dynamic.keyword ":fn",
          Runtime_dynamic.string function_name );
      ]
  in
  Exception_info ("Integer overflow", data, None)

let unsafe_integer_arguments function_name x_safe y_safe =
  let data =
    Runtime_dynamic.map
      [
        (Runtime_dynamic.keyword ":x-int?", Runtime_dynamic.bool x_safe);
        (Runtime_dynamic.keyword ":y-int?", Runtime_dynamic.bool y_safe);
      ]
  in
  Exception_info
    (function_name ^ " called with non-safe-integer arguments", data, None)

let throw exception_ = raise exception_

let message = function
  | Exception_info (message, _, _) -> Some message
  | Failure message | Invalid_argument message -> Some message
  | exception_ -> Some (Printexc.to_string exception_)

let data = function
  | Exception_info (_, data, _) -> data
  | _ -> Runtime_dynamic.nil

let cause = function
  | Exception_info (_, _, cause) -> cause
  | _ -> None
