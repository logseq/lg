exception Exception_info of string * Runtime_dynamic.t

let ex_info message data = Exception_info (message, data)
let throw exception_ = raise exception_

let data = function
  | Exception_info (_, data) -> data
  | _ -> Runtime_dynamic.nil
