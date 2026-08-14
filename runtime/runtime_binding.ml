let bind variable value body =
  let previous = Runtime_reference.deref variable in
  ignore (Runtime_reference.reset variable value);
  Runtime_reference.enter_binding variable;
  match body () with
  | result ->
      ignore (Runtime_reference.reset variable previous);
      Runtime_reference.leave_binding variable;
      result
  | exception error ->
      ignore (Runtime_reference.reset variable previous);
      Runtime_reference.leave_binding variable;
      raise error

let capture variable =
  if Runtime_reference.is_bound variable then
    Some (Runtime_reference.deref variable)
  else None

let with_capture variable captured body =
  match captured with None -> body () | Some value -> bind variable value body
