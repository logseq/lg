module Env = Compiler_environment
type expression_result = (Types.typed_expr, Error.t) result
type call = string -> Env.t -> Ast.form list -> expression_result
type t = {
  compile_apply : call;
  compile_static_fnil : call;
  compile_static_comp : call;
  compile_static_partial : call;
  compile_static_juxt : call;
}
val compile_args_for :
  ('a -> 'b -> 'c -> ('d, 'e) result) ->
  'a -> 'b -> 'c list -> ('d list, 'e) result
val create :
  compile_expr:(string -> Env.t -> Ast.form -> expression_result) ->
  dynamic_unpack:(Env.t ->
                  Types.ty ->
                  Semantic_ir.t -> (Semantic_ir.t, Error.t) result) ->
  pack_dynamic_value:(Env.t ->
                      Types.ty ->
                      Types.typed_expr -> (Semantic_ir.t, Error.t) result) ->
  pack_constrained_value:(Env.t ->
                          Types.ty ->
                          Types.typed_expr -> (Semantic_ir.t, Error.t) result) ->
  plan_and_emit_argument:(Env.t ->
                          expected:Types.ty ->
                          Types.typed_expr -> (Semantic_ir.t, Error.t) result) ->
  t
