type expression_result = (Types.typed_expr, Error.t) result
type call = string -> Compiler_environment.t -> Ast.form list -> expression_result
type forms = Ast.form list -> expression_result

type t = {
  compile_list : call;
  compile_list_star : call;
  compile_list_of : forms;
  compile_vector_of : forms;
  compile_conj : call;
  compile_cons : call;
  compile_subvec : call;
  compile_nth : call;
  compile_get : call;
  compile_find : call;
  compile_assoc : call;
  compile_dissoc : call;
  compile_merge : call;
  compile_hash_map : call;
  compile_update : call;
  compile_select_keys : call;
  compile_contains : call;
  compile_keys : call;
  compile_vals : call;
}

val create :
  compile_expr:(string ->
                Compiler_environment.t ->
                Ast.form ->
                expression_result) ->
  pack_dynamic_value:(Compiler_environment.t ->
                      Types.ty ->
                      Types.typed_expr ->
                      (Semantic_ir.t, Error.t) result) ->
  dynamic_unpack:(Compiler_environment.t ->
                  Types.ty ->
                  Semantic_ir.t ->
                  (Semantic_ir.t, Error.t) result) ->
  t
