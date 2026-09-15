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

type completion = {
  candidate : string;
  type_name : string option;
}

type lookup = {
  name : string;
  namespace : string;
  type_name : string option;
  file : string option;
  line : int option;
  column : int option;
}

type t

val create_from_state :
  include_directories:string list ->
  state_path:string ->
  bootstrap_module:string ->
  (t, Lg.Compiler.compile_error) result

val create_from_stdlib :
  state_path:string -> (t, Lg.Compiler.compile_error) result

val namespace : t -> string
val prompt : t -> string
val eval :
  ?filename:string ->
  t ->
  string ->
  (evaluation, Lg.Compiler.compile_error) result

val load_source :
  ?filename:string ->
  t ->
  string ->
  (evaluation, Lg.Compiler.compile_error) result

val eval_files :
  t -> string list -> (int, Lg.Compiler.compile_error) result

val type_of : t -> string -> (string, Lg.Compiler.compile_error) result

val lookup :
  t -> string -> (lookup option, Lg.Compiler.compile_error) result

val completions :
  t -> string -> (completion list, Lg.Compiler.compile_error) result
