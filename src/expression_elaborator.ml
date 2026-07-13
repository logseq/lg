open Ast
open Types
open Expression_support

module Env = Compiler_environment

let rec compile_expr scope (env : Env.t) form =
  match compile_expr_unlocated scope env form with
  | Error error ->
      Error
        (Error.with_location_if_missing (Source_context.find form) error)
  | Ok expression -> (
      match Source_context.find form with
      | None -> Ok expression
      | Some location ->
          let node_id = Source_node_id.of_location location in
          Ok
            { expression with
              semantic_expr = Semantic_ir.Located (node_id, location, expression.semantic_expr);
            })

and compile_expr_unlocated scope (env : Env.t) = function
  | FInt value -> Ok (typed_ir TInt (Semantic_ir.Int value))
  | FFloat value -> Ok (typed_ir TFloat (Semantic_ir.Float value))
  | FChar value -> Ok (typed_ir TChar (Semantic_ir.Char value))
  | FString value -> Ok (typed_ir TString (Semantic_ir.String value))
  | FBool value -> Ok (typed_ir TBool (Semantic_ir.Bool value))
  | FKeyword keyword -> Ok (typed_ir TKeyword (Semantic_ir.String keyword))
  | FSymbol name -> (
      match Env.find_opt (Names.scoped_key scope name) env with
      | Some { ty = TFn ([], return_ty); ocaml_name; _ }
        when is_constructor_name name ->
          Ok (typed_ir return_ty (Semantic_ir.Constructor (ocaml_name, None)))
      | Some binding -> Ok (typed_ir binding.ty (Semantic_ir.Ident binding.ocaml_name))
      | None when name = "None" ->
          Ok (typed_ir (TOcaml_app ("option", [ TUnknown ])) (Semantic_ir.Constructor (name, None)))
      | None -> Error.error ("unknown symbol " ^ name))
  | FVector forms -> compile_vector scope env forms
  | FMap pairs -> compile_map scope env pairs
  | FList (FSymbol "loop" :: bindings :: body_forms) ->
      compile_loop scope env bindings body_forms
  | FList (FSymbol "recur" :: _) ->
      Error.error "recur is only valid in a loop tail position"
  | FList (FSymbol "let" :: bindings :: body_forms) ->
      compile_let scope env bindings body_forms
  | FList (FSymbol "->" :: value :: steps) ->
      compile_thread scope env `First value steps
  | FList (FSymbol "->>" :: value :: steps) ->
      compile_thread scope env `Last value steps
  | FList (FSymbol "if-let" :: binding :: then_form :: else_form :: []) ->
      compile_if_let scope env binding then_form else_form
  | FList (FSymbol "if-let" :: _) ->
      Error.error "if-let requires [name option], then, and else"
  | FList (FSymbol "when-let" :: binding :: body_forms) ->
      compile_when_let scope env binding body_forms
  | FList (FSymbol "let-some" :: bindings :: then_form :: else_form :: []) ->
      compile_let_some scope env bindings then_form else_form
  | FList (FSymbol "let-some" :: _) ->
      Error.error "let-some requires bindings, then, and else"
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

and compile_thread scope env position value steps =
  let rec expand value = function
    | [] -> compile_expr scope env value
    | FSymbol name :: rest -> expand (FList [ FSymbol name; value ]) rest
    | FList (FSymbol name :: args) :: rest ->
        let args =
          match position with
          | `First -> value :: args
          | `Last -> args @ [ value ]
        in
        expand (FList (FSymbol name :: args)) rest
    | _ -> Error.error "threading steps must be symbols or call forms"
  in
  expand value steps

and compile_map scope env pairs =
  (Lazy.force context).special_forms.compile_map scope env pairs

and compile_if scope env condition then_form else_form =
  (Lazy.force context).special_forms.compile_if scope env condition then_form else_form

and compile_if_not scope env condition then_form else_form =
  (Lazy.force context).special_forms.compile_if_not scope env condition then_form else_form

and compile_if_let scope env binding then_form else_form =
  (Lazy.force context).special_forms.compile_if_let scope env binding then_form
    else_form

and compile_when_let scope env binding body_forms =
  (Lazy.force context).special_forms.compile_when_let scope env binding body_forms

and compile_let_some scope env bindings then_form else_form =
  (Lazy.force context).special_forms.compile_let_some scope env bindings then_form
    else_form

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

and prepare_recursive_fn ~ocaml_name scope env source_name return_ty params
    body_forms =
  match Destructure.parse_param_specs params with
  | Error _ as err -> err
  | Ok specs ->
      let explicit_param_tys =
        List.map (fun (spec : Destructure.param_spec) -> spec.explicit_ty) specs
      in
      if List.exists Option.is_none explicit_param_tys then
        Error.error "recursive defn parameters require type annotations"
      else
        let param_tys = List.map Option.get explicit_param_tys in
        let self_binding =
          Types.binding ocaml_name (TFn (param_tys, return_ty))
        in
        let env = Env.add (Names.scoped_key scope source_name) self_binding env in
        match
          prepare_fn ~param_type_overrides:(List.map Option.some param_tys) scope
            env params body_forms
        with
        | Error _ as err -> err
        | Ok parts ->
            if
              Types.assignable ~policy:Host_boundary ~expected:return_ty
                ~actual:parts.body.ty
            then Ok parts
            else
              Error.error
                ("recursive defn " ^ source_name ^ " must return "
               ^ Types.source_name return_ty)

and fn_code ?(row_param_type_names = []) parts =
  Function_elaborator.fn_code ~row_param_type_names parts

and compile_fn ?(param_type_overrides = []) scope env params body_forms =
  match prepare_fn ~param_type_overrides scope env params body_forms with
  | Error _ as err -> err
  | Ok parts when unresolved_contextual_type parts.body.ty ->
      Error.error "empty list requires a contextual element type"
  | Ok parts -> Ok (fn_code parts)

and compile_call scope env name arg_forms =
  (Lazy.force context).calls.compile_call scope env name arg_forms

and context : Elaboration_context.t Lazy.t =
  lazy (Elaboration_context.create ~compile_expr)

let compile_args_for scope env arg_forms =
  (Lazy.force context).calls.compile_args_for scope env arg_forms
