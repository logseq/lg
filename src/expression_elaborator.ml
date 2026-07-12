open Ast
open Types
open Expression_support

module Env = Compiler_environment

let rec compile_expr scope (env : Env.t) form =
  match compile_expr_unlocated scope env form with
  | Error _ as err -> err
  | Ok expression -> (
      match Source_context.find form with
      | None -> Ok expression
      | Some location ->
          let node_id = Source_node_id.of_location location in
          Ok
            { expression with
              ocaml_expr = Ocaml_ir.Located (node_id, location, expression.ocaml_expr);
            })

and compile_expr_unlocated scope (env : Env.t) = function
  | FInt value -> Ok (typed_ir TInt (Ocaml_ir.Int value))
  | FFloat value -> Ok (typed_ir TFloat (Ocaml_ir.Float value))
  | FChar value -> Ok (typed_ir TChar (Ocaml_ir.Char value))
  | FString value -> Ok (typed_ir TString (Ocaml_ir.String value))
  | FBool value -> Ok (typed_ir TBool (Ocaml_ir.Bool value))
  | FKeyword keyword -> Ok (typed_ir TKeyword (Ocaml_ir.String keyword))
  | FSymbol name -> (
      match Env.find_opt (Names.scoped_key scope name) env with
      | Some { ty = TFn ([], return_ty); _ } when is_constructor_name name ->
          Ok (typed_ir return_ty (Ocaml_ir.Constructor (name, None)))
      | Some binding -> Ok (typed_ir binding.ty (Ocaml_ir.Ident binding.ocaml_name))
      | None when name = "None" ->
          Ok (typed_ir (TOcaml_app ("option", [ TAny ])) (Ocaml_ir.Constructor (name, None)))
      | None -> Error.error ("unknown symbol " ^ name))
  | FVector forms -> compile_vector scope env forms
  | FMap pairs -> compile_map scope env pairs
  | FList (FSymbol "loop" :: bindings :: body_forms) ->
      compile_loop scope env bindings body_forms
  | FList (FSymbol "recur" :: _) ->
      Error.error "recur is only valid in a loop tail position"
  | FList (FSymbol "let" :: bindings :: body_forms) ->
      compile_let scope env bindings body_forms
  | FList (FSymbol "fn" :: params :: body_forms) ->
      compile_fn scope env params body_forms
  | FList (FSymbol "do" :: body_forms) ->
      compile_body scope env "do requires at least one form" body_forms
  | FList [ FKeyword keyword; target ] ->
      compile_call scope env "get" [ target; FKeyword keyword ]
  | FList (FKeyword _ :: _) -> Error.error "keyword lookup expects one argument"
  | FList (FSymbol "if" :: condition :: then_form :: else_form :: []) ->
      compile_if scope env condition then_form else_form
  | FList (FSymbol "if-not" :: condition :: then_form :: else_form :: []) ->
      compile_if_not scope env condition then_form else_form
  | FList (FSymbol "when" :: condition :: body_forms) ->
      compile_when scope env condition body_forms
  | FList (FSymbol "cond" :: clauses) -> compile_cond scope env clauses
  | FList (FSymbol "match" :: target :: clauses) ->
      compile_match scope env target clauses
  | FList (FSymbol "try" :: forms) -> compile_try scope env forms
  | FList (FSymbol name :: args) -> compile_call scope env name args
  | FList [] -> Error.error "empty list is not callable"
  | FList _ -> Error.error "call head must be a symbol"

and compile_vector scope env forms =
  (Lazy.force context).special_forms.compile_vector scope env forms

and compile_map scope env pairs =
  (Lazy.force context).special_forms.compile_map scope env pairs

and compile_if scope env condition then_form else_form =
  (Lazy.force context).special_forms.compile_if scope env condition then_form else_form

and compile_if_not scope env condition then_form else_form =
  (Lazy.force context).special_forms.compile_if_not scope env condition then_form else_form

and compile_when scope env condition body_forms =
  (Lazy.force context).special_forms.compile_when scope env condition body_forms

and compile_cond scope env clauses =
  (Lazy.force context).special_forms.compile_cond scope env clauses

and compile_match scope env target_form clauses =
  (Lazy.force context).special_forms.compile_match scope env target_form clauses

and compile_body scope env empty_error forms =
  (Lazy.force context).special_forms.compile_body scope env empty_error forms

and compile_try scope env forms =
  (Lazy.force context).special_forms.compile_try scope env forms

and loop_branch_type left right =
  (Lazy.force context).special_forms.loop_branch_type left right

and compile_recur scope env loop_name param_tys arg_forms =
  (Lazy.force context).special_forms.compile_recur scope env loop_name param_tys arg_forms

and compile_loop_tail scope env loop_name param_tys form =
  (Lazy.force context).special_forms.compile_loop_tail scope env loop_name param_tys form

and compile_loop_tail_body scope env loop_name param_tys forms =
  (Lazy.force context).special_forms.compile_loop_tail_body scope env loop_name param_tys forms

and compile_loop scope env bindings body_forms =
  (Lazy.force context).special_forms.compile_loop scope env bindings body_forms

and compile_let scope env bindings body_forms =
  (Lazy.force context).special_forms.compile_let scope env bindings body_forms
and prepare_fn ?(param_type_overrides = []) scope env params body_forms =
  let lookup_function_ty name =
    match lookup_function scope env name with
    | Ok fn -> Ok fn.ty
    | Error _ as err -> err
  in
  Function_elaborator.prepare ~param_type_overrides ~lookup_function_ty
    ~compile_body scope env params body_forms

and fn_code ?(row_param_type_names = []) parts =
  Function_elaborator.fn_code ~row_param_type_names parts

and compile_fn ?(param_type_overrides = []) scope env params body_forms =
  match prepare_fn ~param_type_overrides scope env params body_forms with
  | Error _ as err -> err
  | Ok parts -> Ok (fn_code parts)

and compile_call scope env name arg_forms =
  (Lazy.force context).calls.compile_call scope env name arg_forms

and context : Elaboration_context.t Lazy.t =
  lazy (Elaboration_context.create ~compile_expr)

let compile_args_for scope env arg_forms =
  (Lazy.force context).calls.compile_args_for scope env arg_forms
