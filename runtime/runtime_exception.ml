exception Exception_info of string * Runtime_dynamic.t

let ex_info message data = Exception_info (message, data)
let create message = Exception_info (message, Runtime_dynamic.nil)
let throw exception_ = raise exception_

let message = function
  | Exception_info (message, _) -> Some message
  | Failure message | Invalid_argument message -> Some message
  | exception_ -> Some (Printexc.to_string exception_)

let data = function
  | Exception_info (_, data) -> data
  | _ -> Runtime_dynamic.nil
