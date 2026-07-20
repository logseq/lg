let bind variable value body =
  let previous = !variable in
  variable := value;
  match body () with
  | result ->
      variable := previous;
      result
  | exception error ->
      variable := previous;
      raise error
