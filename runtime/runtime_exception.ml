exception Exception_info of string * Lg_edn_backend.t * exn option

let ex_info message data = Exception_info (message, data, None)

let ex_info_with_cause message data cause =
  Exception_info (message, data, Some cause)

let create message = Exception_info (message, Lg_edn_backend.Nil, None)

let integer_overflow function_name =
  let data =
    Lg_edn_backend.Map
      (Array.of_list
      [
        (Lg_edn_backend.Keyword "fn", Lg_edn_backend.String function_name);
      ])
  in
  Exception_info ("Integer overflow", data, None)

let unsafe_integer_arguments function_name x_safe y_safe =
  let data =
    Lg_edn_backend.Map
      (Array.of_list
      [
        (Lg_edn_backend.Keyword "x-int?", Lg_edn_backend.Bool x_safe);
        (Lg_edn_backend.Keyword "y-int?", Lg_edn_backend.Bool y_safe);
      ])
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
  | _ -> Lg_edn_backend.Nil

let cause = function
  | Exception_info (_, _, cause) -> cause
  | _ -> None
