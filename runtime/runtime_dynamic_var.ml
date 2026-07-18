let bind variable value body =
  let previous = !variable in
  variable := value;
  Fun.protect ~finally:(fun () -> variable := previous) body
