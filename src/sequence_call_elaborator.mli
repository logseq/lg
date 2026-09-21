module Env = Compiler_environment
type expression_result = (Types.typed_expr, Error.t) result
type call = string -> Env.t -> Ast.form list -> expression_result
type named_call =
    string -> Env.t -> string -> Ast.form list -> expression_result
type t = {
  compile_sort_by : call;
  compile_mapcat : call;
  compile_repeatedly : call;
  compile_reductions : call;
  compile_map_indexed : call;
  compile_mapv : call;
  compile_reduce_kv : call;
  compile_some : call;
  compile_map_call : call;
  compile_keep : call;
  compile_filter : call;
  compile_reduce : call;
}
val reduce_kv_counter : int ref
val has_source_name : string -> string -> bool
val compile_args_for :
  ('a -> 'b -> 'c -> ('d, 'e) result) ->
  'a -> 'b -> 'c list -> ('d list, 'e) result
val returns_truthy_value : Types.ty -> bool
val truthy_call :
  Types.ty -> Semantic_ir.t -> Semantic_ir.t list -> Semantic_ir.t
val normalize_truthy_function : Types.typed_expr -> Types.typed_expr
val create :
  compile_expr:(string ->
                Env.t ->
                Ast.form -> expression_result) ->
  pack_dynamic_value:(Env.t ->
                      Types.ty ->
                      Types.typed_expr -> (Semantic_ir.t, Error.t) result) ->
  dynamic_unpack:(Env.t ->
                  Types.ty ->
                  Semantic_ir.t -> (Semantic_ir.t, Error.t) result) ->
  pack_constrained_value:(Env.t ->
                          Types.ty ->
                          Types.typed_expr -> (Semantic_ir.t, Error.t) result) ->
  t
