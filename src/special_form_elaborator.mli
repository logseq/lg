val has_source_name : string -> string -> bool
module Env = Compiler_environment
type expression_result = (Types.typed_expr, Error.t) result
type type_result = (Types.ty, Error.t) result
val loop_counter : int ref
val destructuring_value_counter : int ref
type t = {
  compile_vector : string -> Env.t -> Ast.form list -> expression_result;
  compile_map :
    string -> Env.t -> (Ast.form * Ast.form) list -> expression_result;
  compile_if :
    string -> Env.t -> Ast.form -> Ast.form -> Ast.form -> expression_result;
  compile_if_let :
    string -> Env.t -> Ast.form -> Ast.form -> Ast.form -> expression_result;
  compile_if_some :
    string -> Env.t -> Ast.form -> Ast.form -> Ast.form -> expression_result;
  compile_some_thread :
    string -> Env.t -> Ast.form -> Ast.form -> expression_result;
  compile_when_let :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_when_some :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_let_some :
    string -> Env.t -> Ast.form -> Ast.form -> Ast.form -> expression_result;
  compile_logical :
    string -> Env.t -> [ `And | `Or ] -> Ast.form list -> expression_result;
  compile_match :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_body :
    string -> Env.t -> string -> Ast.form list -> expression_result;
  compile_try : string -> Env.t -> Ast.form list -> expression_result;
  loop_branch_type : Types.ty -> Types.ty -> type_result;
  compile_recur :
    string ->
    Env.t -> string -> Types.ty list -> Ast.form list -> expression_result;
  compile_loop_tail :
    string ->
    Env.t -> string -> Types.ty list -> Ast.form -> expression_result;
  compile_loop_tail_body :
    string ->
    Env.t -> string -> Types.ty list -> Ast.form list -> expression_result;
  compile_loop :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_let :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
}
val compile_args_for :
  ('a -> 'b -> 'c -> ('d, 'e) result) ->
  'a -> 'b -> 'c list -> ('d list, 'e) result
val located_pattern :
  (Source_node_id.t * Warnings.loc) option ->
  Semantic_ir.pattern -> Semantic_ir.pattern
val located_form_pattern :
  Ast.form -> Semantic_ir.pattern -> Semantic_ir.pattern
val capability_pattern : string -> Types.ty -> Semantic_ir.pattern
val has_capability : Types.ty -> bool
val has_protocol_constraint : Protocol_id.t -> Types.ty -> bool
val narrow_type_predicates :
  string -> Env.t -> Ast.form -> Ast.form -> Ast.form
val narrow_false_scalar_predicates :
  string -> Env.t -> Ast.form -> Ast.form -> Ast.form
val narrow_false_fn_predicates :
  string -> Env.t -> Ast.form -> Ast.form -> Ast.form
val narrow_false_instance_predicates :
  string -> Env.t -> Ast.form -> Ast.form -> Ast.form
val false_nil_predicate_names : Ast.form -> string list
val narrow_non_nil_name : string -> Env.t -> string -> Ast.form -> Ast.form
val narrow_false_nil_predicates :
  string -> Env.t -> Ast.form -> Ast.form -> Ast.form
val unwrap_option_storage : Types.typed_expr -> Types.typed_expr
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
  argument_compatible:(Types.ty -> Types.ty -> bool) -> t
