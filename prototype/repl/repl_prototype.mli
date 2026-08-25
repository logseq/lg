type t

val create_from_stdlib :
  state_path:string -> (t, Lg.Compiler.compile_error) result

val eval :
  t -> string -> (string, Lg.Compiler.compile_error) result
