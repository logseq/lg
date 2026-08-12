let bind variable value body =
  let previous = Runtime_reference.deref variable in
  ignore (Runtime_reference.reset variable value);
  match body () with
  | result ->
      ignore (Runtime_reference.reset variable previous);
      result
  | exception error ->
      ignore (Runtime_reference.reset variable previous);
      raise error
