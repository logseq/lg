type expression_result = (Types.typed_expr, Error.t) result

type t = {
  compile_call :
    string ->
    Compiler_environment.t ->
    string ->
    Ast.form list ->
    expression_result;
  compile_args_for :
    string ->
    Compiler_environment.t ->
    Ast.form list ->
    (Types.typed_expr list, Error.t) result;
}

val argument_compatible : Types.ty -> Types.ty -> bool
val contains_unresolved_type : Types.ty -> bool

val dynamic_unpack :
  Compiler_environment.t ->
  Types.ty ->
  Semantic_ir.t ->
  (Semantic_ir.t, Error.t) result

val pack_dynamic_value :
  Compiler_environment.t ->
  Semantic_type.ty ->
  Types.typed_expr ->
  (Semantic_ir.t, Error.t) result

val pack_constrained_value :
  ?row_type_name:string ->
  Compiler_environment.t ->
  Types.ty ->
  Types.typed_expr ->
  (Semantic_ir.t, Error.t) result

val adapt_value_to_type :
  Compiler_environment.t ->
  Types.ty ->
  Types.typed_expr ->
  (Semantic_ir.t, Error.t) result

val adapt_protocol_witness_result :
  Compiler_environment.t ->
  expected:Types.ty ->
  actual:Types.ty ->
  Semantic_ir.t ->
  (Types.typed_expr, Error.t) result

val create :
  compile_expr:(string ->
                Compiler_environment.t ->
                Ast.form ->
                expression_result) ->
  t
