exception Exception_info of string * Runtime_dynamic.t * exn option

let ex_info message data = Exception_info (message, data, None)

let ex_info_with_cause message data cause =
  Exception_info (message, data, Some cause)

let create message = Exception_info (message, Runtime_dynamic.nil, None)
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
