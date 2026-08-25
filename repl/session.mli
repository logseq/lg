type value = {
  rendered : string;
  type_name : string;
}

type definition = {
  name : string;
  type_name : string;
}

type outcome =
  | Value of value
  | Definition of definition
  | Namespace of string
  | Summary of string

type evaluation = {
  outcome : outcome;
  namespace : string;
}

type t

val create_from_stdlib :
  state_path:string -> (t, Lg.Compiler.compile_error) result

val namespace : t -> string
val prompt : t -> string
val eval : t -> string -> (evaluation, Lg.Compiler.compile_error) result
val type_of : t -> string -> (string, Lg.Compiler.compile_error) result
