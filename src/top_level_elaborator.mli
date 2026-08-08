val function_is_recursive : string -> string -> Ast.form list -> bool

val expression_references_declaration :
  Compiler_environment.t -> Semantic_ir.t -> bool

val compile :
  string ->
  Compiler_environment.t ->
  int ->
  Ast.form ->
  (string * Compiler_environment.t * int * Lowered.compiled_item, Error.t)
  result
