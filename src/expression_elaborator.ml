open Ast
open Types
open Expression_support
module Env = Compiler_environment

type multi_arity_clause = {
  params : Ast.form;
  body_forms : Ast.form list;
  fixed_count : int;
  rest_index : int option;
  initial_arity : fn_arity;
}

type prepared_multi_arity_clause = {
  target_name : string;
  parts : Expression_support.compiled_fn_parts;
  row_param_types : string option list;
}

type prepared_multi_arity_fn = {
  clauses : prepared_multi_arity_clause list;
  expr : typed_expr;
}

let some_thread_counter = ref 0
let condp_counter = ref 0
let callable_set_counter = ref 0
let dynamic_case_counter = ref 0
let callable_expression_counter = ref 0
let dotimes_counter = ref 0

let rec compile_expr scope (env : Env.t) form =
  match compile_expr_unlocated scope env form with
  | Error error ->
      Error (Error.with_location_if_missing (Source_context.find form) error)
  | Ok expression -> (
      match Source_context.find form with
      | None -> Ok expression
      | Some location ->
          let node_id = Source_node_id.of_location location in
          Ok
            {
              expression with
              semantic_expr =
                Semantic_ir.Located (node_id, location, expression.semantic_expr);
            })

and compile_expr_unlocated scope (env : Env.t) = function
  | FInt value -> Ok (typed_ir TInt (Semantic_ir.Int value))
  | FFloat value -> Ok (typed_ir TFloat (Semantic_ir.Float value))
  | FChar value -> Ok (typed_ir TChar (Semantic_ir.Char value))
  | FString value -> Ok (typed_ir TString (Semantic_ir.String value))
  | FRegex value ->
      Ok (typed_ir TRegex (Semantic_ir.String ("\000lg-regex:" ^ value)))
  | FBool value -> Ok (typed_ir TBool (Semantic_ir.Bool value))
  | FKeyword keyword -> Ok (typed_ir TKeyword (Semantic_ir.String keyword))
  | FSymbol "nil" -> Ok (typed_ir TNil (Semantic_ir.Constructor ("None", None)))
  | FSymbol "js/Error"
    when Env.target env = Target.Melange
         || Env.target env = Target.Js_of_ocaml ->
      Ok (typed_ir TUnit Semantic_ir.Unit)
  | FSymbol name when String.length name > 1 && name.[0] = '@' ->
      let reference_name = String.sub name 1 (String.length name - 1) in
      compile_expr scope env (FList [ FSymbol "deref"; FSymbol reference_name ])
  | FSymbol name -> (
      match Env.find_opt (Names.scoped_key scope name) env with
      | Some { ty = TFn ([], return_ty); ocaml_name; _ }
        when is_constructor_name name ->
          Ok (typed_ir return_ty (Semantic_ir.Constructor (ocaml_name, None)))
      | Some { ty = TRef value_ty; ocaml_name; dynamic_var = true; _ } ->
          Ok
            (typed_ir value_ty
               (Semantic_ir.Prefix ("!", Semantic_ir.Ident ocaml_name)))
      | Some binding ->
          Ok (typed_ir binding.ty (Semantic_ir.Ident binding.ocaml_name))
      | None when name = "None" ->
          Ok
            (typed_ir
               (TOcaml_app ("option", [ TUnknown ]))
               (Semantic_ir.Constructor (name, None)))
      | None -> (
          match lookup_function scope env name with
          | Ok function_ -> Ok function_
          | Error _ -> Error.error ("unknown symbol " ^ name)))
  | FCoreSymbol core_symbol ->
      lookup_function scope env (Ast.core_symbol_qualified_name core_symbol)
  | FVector forms -> compile_vector scope env forms
  | FMap pairs -> compile_map scope env pairs
  | FList
      (FSymbol ("dotimes" | "clojure.core/dotimes" | "cljs.core/dotimes")
      :: FVector [ FSymbol index_name; limit ] :: body_forms) ->
      incr dotimes_counter;
      let limit_name =
        "__lg_dotimes_limit_" ^ string_of_int !dotimes_counter
      in
      let loop_index_name =
        if index_name = "_" then
          "__lg_dotimes_index_" ^ string_of_int !dotimes_counter
        else index_name
      in
      let next_index = FList [ FSymbol "inc"; FSymbol loop_index_name ] in
      let recur = FList [ FSymbol "recur"; next_index ] in
      let body = FList (FSymbol "do" :: body_forms @ [ recur ]) in
      compile_expr scope env
        (FList
           [
             FSymbol "let";
             FVector [ FSymbol limit_name; limit ];
             FList
               [
                 FSymbol "loop";
                 FVector [ FSymbol loop_index_name; FInt 0 ];
                 FList
                   [
                     FSymbol "if";
                     FList
                       [
                         FSymbol "<";
                         FSymbol loop_index_name;
                         FSymbol limit_name;
                       ];
                     body;
                     FSymbol "nil";
                   ];
               ];
           ])
  | FList
      (FSymbol ("dotimes" | "clojure.core/dotimes" | "cljs.core/dotimes")
      :: _) ->
      Error.error "dotimes expects [name count] and optional body forms"
  | FList (FSymbol "loop" :: bindings :: body_forms) ->
      compile_loop scope env bindings body_forms
  | FList (FSymbol "recur" :: _) ->
      Error.error "recur is only valid in a loop tail position"
  | FList (FSymbol "let" :: bindings :: body_forms) ->
      compile_let scope env bindings body_forms
  | FList (FSymbol ("->" | "clojure.core/->" | "cljs.core/->") :: value :: steps) ->
      compile_thread scope env `First value steps
  | FList
      (FSymbol ("->>" | "clojure.core/->>" | "cljs.core/->>") :: value :: steps)
    ->
      compile_thread scope env `Last value steps
  | FList (FSymbol "cond->" :: value :: clauses) ->
      compile_cond_thread scope env `First value clauses
  | FList (FSymbol "cond->>" :: value :: clauses) ->
      compile_cond_thread scope env `Last value clauses
  | FList (FSymbol "some->" :: value :: steps) ->
      compile_some_thread scope env `First value steps
  | FList (FSymbol "some->>" :: value :: steps) ->
      compile_some_thread scope env `Last value steps
  | FList [ FSymbol "if-let"; binding; then_form; else_form ] ->
      compile_if_let scope env binding then_form else_form
  | FList [ FSymbol "if-some"; binding; then_form; else_form ] ->
      compile_if_some scope env binding then_form else_form
  | FList (FSymbol "if-let" :: _) ->
      Error.error "if-let requires [name option], then, and else"
  | FList (FSymbol "if-some" :: _) ->
      Error.error "if-some requires [name option], then, and else"
  | FList (FSymbol "when-let" :: binding :: body_forms) ->
      compile_when_let scope env binding body_forms
  | FList (FSymbol "when-some" :: binding :: body_forms) ->
      compile_when_some scope env binding body_forms
  | FList [ FSymbol "let-some"; bindings; then_form; else_form ] ->
      compile_let_some scope env bindings then_form else_form
  | FList (FSymbol "let-some" :: _) ->
      Error.error "let-some requires bindings, then, and else"
  | FList
      (FSymbol "fn" :: FSymbol name :: (FVector _ as params) :: body_forms) ->
      compile_named_fn scope env name params body_forms
  | FList (FSymbol "fn" :: params :: body_forms) ->
      compile_fn scope env params body_forms
  | FList (FSymbol "new" :: FSymbol type_name :: args) ->
      compile_call scope env (type_name ^ ".") args
  | FList
      [ FSymbol ".isArray"; FList [ FSymbol ".getClass"; value ] ] ->
      compile_expr scope env (FList [ FSymbol "array-value?"; value ])
  | FList [ FSymbol "quote"; value ] -> compile_quoted scope env value
  | FList (FSymbol "quote" :: _) -> Error.error "quote expects one form"
  | FList (FSymbol "do" :: body_forms) ->
      compile_body scope env "do requires at least one form" body_forms
  | FList [ FKeyword keyword; target ] ->
      compile_call scope env "clojure.core/get" [ target; FKeyword keyword ]
  | FList [ FKeyword keyword; target; default ] ->
      compile_call scope env "clojure.core/get"
        [ target; FKeyword keyword; default ]
  | FList (FKeyword _ :: _) -> Error.error "keyword lookup expects one argument"
  | FList [ FSymbol "if"; condition; then_form; else_form ] ->
      compile_if scope env condition then_form else_form
  | FList [ FSymbol "if"; condition; then_form ] ->
      compile_if scope env condition then_form (FSymbol "nil")
  | FList [ FSymbol "if-not"; condition; then_form; else_form ] ->
      compile_if_not scope env condition then_form else_form
  | FList (FSymbol "when" :: condition :: body_forms) ->
      compile_when scope env condition body_forms
  | FList (FSymbol "when-not" :: condition :: body_forms) ->
      compile_when scope env (FList [ FSymbol "not"; condition ]) body_forms
  | FList (FSymbol "cond" :: clauses) -> compile_cond scope env clauses
  | FList (FSymbol "condp" :: predicate :: target :: clauses) ->
      compile_condp scope env predicate target clauses
  | FList (FSymbol "case" :: target :: clauses) ->
      compile_case scope env target clauses
  | FList [ FSymbol "case" ] -> Error.error "case expects a target"
  | FList (FSymbol "doseq" :: bindings :: body_forms) ->
      compile_doseq scope env bindings body_forms
  | FList [ FSymbol "for"; bindings; body ] ->
      compile_for scope env bindings body
  | FList (FSymbol "for" :: _) ->
      Error.error "for expects a binding vector and body"
  | FList (FSymbol "and" :: forms) -> compile_logical scope env `And forms
  | FList (FSymbol "or" :: forms) -> compile_logical scope env `Or forms
  | FList (FSymbol "match" :: target :: clauses) ->
      compile_match scope env target clauses
  | FList (FSymbol "try" :: forms) -> compile_try scope env forms
  | FList
      [ FList (FSymbol ("hash-set" | "sorted-set") :: element_forms); key_form ]
    ->
      incr callable_set_counter;
      let suffix = string_of_int !callable_set_counter in
      let key_name = "__lg_callable_set_key_" ^ suffix in
      let elements =
        List.mapi
          (fun index form ->
            ( "__lg_callable_set_element_" ^ suffix ^ "_" ^ string_of_int index,
              form ))
          element_forms
      in
      let body =
        List.fold_right
          (fun (element_name, _) otherwise ->
            FList
              [
                FSymbol "if";
                FList [ FSymbol "="; FSymbol key_name; FSymbol element_name ];
                FSymbol element_name;
                otherwise;
              ])
          elements (FSymbol "nil")
      in
      let bindings =
        FSymbol key_name :: key_form
        :: List.concat_map (fun (name, form) -> [ FSymbol name; form ]) elements
      in
      compile_expr scope env (FList [ FSymbol "let"; FVector bindings; body ])
  | FList (FSymbol name :: args) -> (
      match Env.find_macro ~scope name env with
      | None -> (
          match Env.find_inline_macro ~scope name env with
          | Some definition -> (
          match Macro_expander.expand ~compiler_env:env definition args with
          | Error _ as err -> err
          | Ok expanded -> compile_expr scope env expanded)
          | None -> compile_call scope env name args)
      | Some definition -> (
          match Macro_expander.expand ~compiler_env:env definition args with
          | Error _ as err -> err
          | Ok expanded -> compile_expr scope env expanded))
  | FList (FCoreSymbol core_symbol :: args) ->
      compile_call scope env (Ast.core_symbol_qualified_name core_symbol) args
  | FList [] -> compile_call scope env "list" []
  | FList (function_form :: arguments) ->
      incr callable_expression_counter;
      let function_name =
        "__lg_callable_expression_" ^ string_of_int !callable_expression_counter
      in
      compile_expr scope env
        (FList
           [
             FSymbol "let";
             FVector [ FSymbol function_name; function_form ];
             FList (FSymbol function_name :: arguments);
           ])

and compile_vector scope env forms =
  (Lazy.force context).special_forms.compile_vector scope env forms

and compile_quoted scope env = function
  | FSymbol symbol -> Ok (typed_ir TSymbol (Semantic_ir.String symbol))
  | FVector forms ->
      compile_expr scope env
        (FVector (List.map (fun form -> FList [ FSymbol "quote"; form ]) forms))
  | FList forms ->
      compile_expr scope env
        (FList
           (FSymbol "list"
           :: List.map (fun form -> FList [ FSymbol "quote"; form ]) forms))
  | FMap pairs ->
      compile_expr scope env
        (FMap
           (pairs
           |> List.map (fun (key, value) ->
               (key, FList [ FSymbol "quote"; value ]))))
  | form -> compile_expr scope env form

and compile_thread scope env position value steps =
  let rec expand value = function
    | [] -> compile_expr scope env value
    | FSymbol name :: rest -> expand (FList [ FSymbol name; value ]) rest
    | (FKeyword _ as keyword) :: rest -> expand (FList [ keyword; value ]) rest
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

and compile_cond_thread scope env position value clauses =
  let operator = match position with `First -> "->" | `Last -> "->>" in
  let has_name name expected =
    name = expected || String.ends_with ~suffix:("/" ^ expected) name
  in
  let is_array_conversion = function
    | FSymbol name | FList [ FSymbol name ] ->
        has_name name "array-from" || has_name name "into-array"
    | _ -> false
  in
  let array_normalization_proves_array current condition step =
    match condition with
    | FList
        [ FSymbol "not"; FList [ FSymbol predicate; candidate ] ]
      when (has_name predicate "array?" || has_name predicate "array-value?")
           && is_array_conversion step ->
        candidate = current
    | _ -> false
  in
  let rec expand current = function
    | [] -> Ok current
    | condition :: step :: rest ->
        incr some_thread_counter;
        let name =
          "__lg_cond_thread_value_" ^ string_of_int !some_thread_counter
        in
        let threaded = FList [ FSymbol operator; FSymbol name; step ] in
        let unchanged =
          if array_normalization_proves_array current condition step then
            FList
              [
                FSymbol "__lg_dynamic-narrow";
                threaded;
                FSymbol name;
              ]
          else FSymbol name
        in
        Result.map
          (fun continuation ->
            FList
              [ FSymbol "let"; FVector [ FSymbol name; current ]; continuation ])
          (expand
             (FList
                [
                  FSymbol "if";
                  condition;
                  threaded;
                  unchanged;
                ])
             rest)
    | [ _ ] -> Error.error (operator ^ " requires condition/step pairs")
  in
  Result.bind (expand value clauses) (compile_expr scope env)

and compile_some_thread scope env position value steps =
  let thread value = function
    | FSymbol name -> Ok (FList [ FSymbol name; value ])
    | FKeyword _ as keyword -> Ok (FList [ keyword; value ])
    | FList (FSymbol name :: args) ->
        let args =
          match position with
          | `First -> value :: args
          | `Last -> args @ [ value ]
        in
        Ok (FList (FSymbol name :: args))
    | _ ->
        Error.error
          ((match position with `First -> "some->" | `Last -> "some->>")
         ^ " steps must be symbols or call forms")
  in
  let option_payload_type = function
    | TNullable payload_ty -> Some payload_ty
    | TNil -> Some TUnknown
    | TOcaml_app ("option", [ payload_ty ]) ->
        Some (lg_metadata_type_for_ocaml_payload payload_ty)
    | TOcaml "option" -> Some TUnknown
    | _ -> None
  in
  let nil = typed_ir TNil (Semantic_ir.Constructor ("None", None)) in
  let merge_with_nil threaded =
    match merge_branch_expressions threaded nil with
    | Some merged -> Ok merged
    | None -> Error.error "some-> result cannot be made nullable"
  in
  let rec continue env current = function
    | [] -> Ok current
    | step :: rest -> (
        incr some_thread_counter;
        let source_name =
          "__lg_some_thread_value_" ^ string_of_int !some_thread_counter
        in
        let ocaml_name = Names.sanitize_name source_name in
        let compile_rest binding_ty =
          let step_env =
            Env.add
              (Names.scoped_key scope source_name)
              (Types.binding ocaml_name binding_ty)
              env
          in
          match thread (FSymbol source_name) step with
          | Error _ as error -> error
          | Ok threaded -> (
              match compile_expr scope step_env threaded with
              | Error _ as error -> error
              | Ok threaded -> continue step_env threaded rest)
        in
        match option_payload_type current.ty with
        | Some payload_ty -> (
            match compile_rest payload_ty with
            | Error _ as error -> error
            | Ok threaded -> (
                match merge_with_nil threaded with
                | Error _ as error -> error
                | Ok (result_ty, some_code, none_code) ->
                    Ok
                      (typed_ir result_ty
                         (Semantic_ir.Match
                            ( current.semantic_expr,
                              [
                                ( Semantic_ir.PConstructor
                                    ("Some", Some (Semantic_ir.PVar ocaml_name)),
                                  some_code );
                                ( Semantic_ir.PConstructor ("None", None),
                                  none_code );
                              ] )))))
        | None when Types.is_dynamic current.ty -> (
            match compile_rest current.ty with
            | Error _ as error -> error
            | Ok threaded -> (
                match merge_with_nil threaded with
                | Error _ as error -> error
                | Ok (result_ty, some_code, none_code) ->
                    Ok
                      (typed_ir result_ty
                         (Semantic_ir.Let
                            ( [
                                ( Semantic_ir.PVar ocaml_name,
                                  current.semantic_expr );
                              ],
                              Semantic_ir.If
                                ( Semantic_ir.Apply
                                    ( Semantic_ir.Ident
                                        "Lg_runtime.Runtime_dynamic.is_nil",
                                      [ Semantic_ir.Ident ocaml_name ] ),
                                  none_code,
                                  some_code ) )))))
        | None -> (
            match compile_rest current.ty with
            | Error _ as error -> error
            | Ok threaded ->
                Ok
                  {
                    threaded with
                    semantic_expr =
                      Semantic_ir.Let
                        ( [
                            (Semantic_ir.PVar ocaml_name, current.semantic_expr);
                          ],
                          threaded.semantic_expr );
                  }))
  in
  match compile_expr scope env value with
  | Error _ as error -> error
  | Ok value -> continue env value steps

and compile_case scope env target clauses =
  let rec grouped_pattern = function
    | [] -> FSymbol "_"
    | [ pattern ] -> pattern
    | pattern :: rest -> FList [ FSymbol "or"; pattern; grouped_pattern rest ]
  in
  let pattern = function
    | FList patterns -> grouped_pattern patterns
    | pattern -> pattern
  in
  let rec pairs acc = function
    | [] ->
        List.rev
          (FList
             [
               FSymbol "throw";
               FList
                 [ FSymbol "ex-info"; FString "No matching clause"; FMap [] ];
             ]
          :: FSymbol "_" :: acc)
    | [ default ] -> List.rev (default :: FSymbol "_" :: acc)
    | constant :: result :: rest ->
        pairs (result :: pattern constant :: acc) rest
  in
  match compile_expr scope env target with
  | Error _ as error -> error
  | Ok target_expression when Types.is_dynamic target_expression.ty ->
      incr dynamic_case_counter;
      let target_name =
        "__lg_dynamic_case_target_" ^ string_of_int !dynamic_case_counter
      in
      let condition constant =
        let constants =
          match constant with FList forms -> forms | form -> [ form ]
        in
        match constants with
        | [] -> FBool false
        | first :: rest ->
            List.fold_left
              (fun condition constant ->
                FList
                  [
                    FSymbol "or";
                    condition;
                    FList [ FSymbol "="; FSymbol target_name; constant ];
                  ])
              (FList [ FSymbol "="; FSymbol target_name; first ])
              rest
      in
      let rec expand = function
        | [] ->
            FList
              [
                FSymbol "throw";
                FList
                  [ FSymbol "ex-info"; FString "No matching clause"; FMap [] ];
              ]
        | [ default ] -> default
        | constant :: result :: rest ->
            FList [ FSymbol "if"; condition constant; result; expand rest ]
      in
      compile_expr scope env
        (FList
           [
             FSymbol "let";
             FVector [ FSymbol target_name; target ];
             expand clauses;
           ])
  | Ok _ -> compile_match scope env target (pairs [] clauses)

and compile_doseq scope env bindings body_forms =
  let rec expand = function
    | [] -> Ok (FList (FSymbol "do" :: body_forms))
    | FKeyword ":let" :: FVector bindings :: rest ->
        Result.map
          (fun body -> FList [ FSymbol "let"; FVector bindings; body ])
          (expand rest)
    | FKeyword ":when" :: condition :: rest ->
        Result.map
          (fun body -> FList [ FSymbol "when"; condition; body ])
          (expand rest)
    | FKeyword ":while" :: _ -> Error.error "doseq :while is not supported yet"
    | ((FSymbol _ | FVector _ | FMap _) as pattern) :: collection :: rest ->
        Result.map
          (fun body ->
            FList
              [
                FSymbol "do";
                FList
                  [
                    FSymbol "run!";
                    FList [ FSymbol "fn"; FVector [ pattern ]; body ];
                    collection;
                  ];
                FSymbol "nil";
              ])
          (expand rest)
    | _ -> Error.error "doseq requires binding/collection pairs"
  in
  match (bindings, body_forms) with
  | FVector forms, _ :: _ -> (
      match expand forms with
      | Error _ as error -> error
      | Ok expanded -> compile_expr scope env expanded)
  | FVector _, [] -> Error.error "doseq requires a body"
  | _ -> Error.error "doseq bindings must be a vector"

and compile_for scope env bindings body =
  let erased_seqable_parameter = function
    | FSymbol name -> (
        match Env.find_opt (Names.scoped_key scope name) env with
        | Some binding -> (
            match Types.seqable_constraint_info binding.ty with
            | Some (_, (TUnknown | TVar _), (TUnknown | TVar _)) -> true
            | Some _ | None -> false)
        | None -> false)
    | _ -> false
  in
  let mapper_parameters pattern collection =
    match pattern with
    | (FVector _ | FMap _) when erased_seqable_parameter collection ->
        FVector [ FSymbol "^:dynamic"; pattern ]
    | _ -> FVector [ pattern ]
  in
  let rec produces_sequence = function
    | [] -> false
    | FKeyword ":when" :: _ -> true
    | FKeyword _ :: _ :: rest -> produces_sequence rest
    | (FSymbol _ | FVector _ | FMap _) :: _ :: _ -> true
    | _ -> false
  in
  let rec expand = function
    | [] -> Ok body
    | FKeyword ":let" :: FVector bindings :: rest ->
        Result.map
          (fun body -> FList [ FSymbol "let"; FVector bindings; body ])
          (expand rest)
    | FKeyword ":when" :: condition :: rest ->
        Result.map
          (fun body ->
            let when_true =
              if produces_sequence rest then body else FVector [ body ]
            in
            FList [ FSymbol "if"; condition; when_true; FVector [] ])
          (expand rest)
    | FKeyword ":while" :: _ -> Error.error "for :while is not supported yet"
    | ((FSymbol _ | FVector _ | FMap _) as pattern) :: collection :: rest ->
        Result.map
          (fun body ->
            let mapper =
              FList
                [ FSymbol "fn"; mapper_parameters pattern collection; body ]
            in
            let function_name =
              if produces_sequence rest then "mapcat" else "map"
            in
            FList [ FSymbol function_name; mapper; collection ])
          (expand rest)
    | _ -> Error.error "for requires binding/collection pairs"
  in
  match bindings with
  | FVector forms -> (
      match expand forms with
      | Error _ as error -> error
      | Ok expanded -> compile_expr scope env expanded)
  | _ -> Error.error "for bindings must be a vector"

and compile_map scope env pairs =
  (Lazy.force context).special_forms.compile_map scope env pairs

and compile_if scope env condition then_form else_form =
  (Lazy.force context).special_forms.compile_if scope env condition then_form
    else_form

and compile_if_not scope env condition then_form else_form =
  (Lazy.force context).special_forms.compile_if_not scope env condition
    then_form else_form

and compile_if_let scope env binding then_form else_form =
  (Lazy.force context).special_forms.compile_if_let scope env binding then_form
    else_form

and compile_if_some scope env binding then_form else_form =
  (Lazy.force context).special_forms.compile_if_some scope env binding then_form
    else_form

and compile_when_let scope env binding body_forms =
  (Lazy.force context).special_forms.compile_when_let scope env binding
    body_forms

and compile_when_some scope env binding body_forms =
  (Lazy.force context).special_forms.compile_when_some scope env binding
    body_forms

and compile_let_some scope env bindings then_form else_form =
  (Lazy.force context).special_forms.compile_let_some scope env bindings
    then_form else_form

and compile_when scope env condition body_forms =
  (Lazy.force context).special_forms.compile_when scope env condition body_forms

and compile_cond scope env clauses =
  (Lazy.force context).special_forms.compile_cond scope env clauses

and compile_condp scope env predicate target clauses =
  incr condp_counter;
  let target_name = "__lg_condp_target_" ^ string_of_int !condp_counter in
  let rec expand = function
    | [] ->
        Ok
          (FList
             [
               FSymbol "throw";
               FList
                 [
                   FSymbol "ex-info";
                   FString "No matching clause in condp";
                   FMap [];
                 ];
             ])
    | [ default ] -> Ok default
    | test :: expression :: rest ->
        Result.map
          (fun otherwise ->
            FList
              [
                FSymbol "if";
                FList [ predicate; test; FSymbol target_name ];
                expression;
                otherwise;
              ])
          (expand rest)
  in
  Result.bind (expand clauses) (fun body ->
      compile_expr scope env
        (FList [ FSymbol "let"; FVector [ FSymbol target_name; target ]; body ]))

and compile_logical scope env operator forms =
  (Lazy.force context).special_forms.compile_logical scope env operator forms

and compile_match scope env target_form clauses =
  (Lazy.force context).special_forms.compile_match scope env target_form clauses

and compile_body scope env empty_error forms =
  (Lazy.force context).special_forms.compile_body scope env empty_error forms

and compile_try scope env forms =
  (Lazy.force context).special_forms.compile_try scope env forms

and loop_branch_type left right =
  (Lazy.force context).special_forms.loop_branch_type left right

and compile_recur scope env loop_name param_tys arg_forms =
  (Lazy.force context).special_forms.compile_recur scope env loop_name param_tys
    arg_forms

and compile_loop_tail scope env loop_name param_tys form =
  (Lazy.force context).special_forms.compile_loop_tail scope env loop_name
    param_tys form

and compile_loop_tail_body scope env loop_name param_tys forms =
  (Lazy.force context).special_forms.compile_loop_tail_body scope env loop_name
    param_tys forms

and compile_loop scope env bindings body_forms =
  (Lazy.force context).special_forms.compile_loop scope env bindings body_forms

and compile_let scope env bindings body_forms =
  (Lazy.force context).special_forms.compile_let scope env bindings body_forms

and prepare_fn ?(param_type_overrides = []) ?variadic_rest_index
    ?(materialize_open_equality = false) ?recur_target scope env params
    body_forms =
  let lookup_function_ty = lookup_function_ty scope env in
  let compile_function_body =
    match recur_target with
    | None -> None
    | Some target_name ->
        Some
          (fun body_env param_tys forms ->
            compile_loop_tail_body scope body_env target_name param_tys forms)
  in
  let prepare param_type_overrides =
    Function_elaborator.prepare ~param_type_overrides ?variadic_rest_index
      ~materialize_open_equality ?compile_function_body ~lookup_function_ty
      ~compile_body scope env params body_forms
  in
  Result.bind (prepare param_type_overrides) (fun parts ->
      let record_values = Option.value parts.body.record_values ~default:[] in
      let refined = ref false in
      let param_type_overrides =
        parts.param_bindings
        |> List.mapi (fun index (_key, (binding : binding)) ->
               match List.nth_opt param_type_overrides index with
               | Some (Some ty) when not (Types.equal ty TUnknown) -> Some ty
               | _ -> (
                   match
                     List.find_opt
                       (fun (_field, value) ->
                         match Semantic_ir.unlocated value with
                         | Semantic_ir.Ident name -> name = binding.ocaml_name
                         | _ -> false)
                       record_values
                   with
                   | None -> None
                   | Some (field, _) ->
                       refined := true;
                       Some field.ty))
      in
      if !refined then prepare param_type_overrides else Ok parts)

and parse_multi_arity_clauses source_name forms =
  let rec parse_clause = function
    | FList (FVector raw_params :: body_forms) when body_forms <> [] -> (
        let rec split fixed = function
          | [] -> Ok (List.rev fixed, None)
          | FSymbol "&" :: [ (FSymbol _ as rest) ] ->
              Ok (List.rev fixed, Some [ rest ])
          | FSymbol "&" :: [ FSymbol annotation; (FSymbol _ as rest) ]
            when String.starts_with ~prefix:"^:" annotation ->
              Ok (List.rev fixed, Some [ FSymbol annotation; rest ])
          | FSymbol "&" :: _ ->
              Error.error
                ("defn " ^ source_name
               ^ " variadic arity requires one rest parameter")
          | form :: rest -> split (form :: fixed) rest
        in
        match split [] raw_params with
        | Error _ as err -> err
        | Ok (fixed_forms, rest_forms) -> (
            let fixed_params = FVector fixed_forms in
            match Destructure.parse_param_specs fixed_params with
            | Error _ as err -> err
            | Ok fixed_specs -> (
                let fixed_count = List.length fixed_specs in
                let params =
                  FVector (fixed_forms @ Option.value rest_forms ~default:[])
                in
                match Destructure.parse_param_specs params with
                | Error _ as err -> err
                | Ok specs ->
                    let rest_index =
                      Option.map (fun _ -> fixed_count) rest_forms
                    in
                    let fixed_param_tys =
                      List.map
                        (fun (spec : Destructure.param_spec) ->
                          Option.value spec.explicit_ty ~default:TUnknown)
                        fixed_specs
                    in
                    let explicit_rest_ty =
                      match rest_index with
                      | None -> None
                      | Some index -> (
                          match List.nth_opt specs index with
                          | None -> None
                          | Some (spec : Destructure.param_spec) ->
                              spec.explicit_ty)
                    in
                    Ok
                      {
                        params;
                        body_forms;
                        fixed_count;
                        rest_index;
                        initial_arity =
                          {
                            fixed_params = fixed_param_tys;
                            rest_param =
                              Option.map
                                (fun _ ->
                                  Option.value explicit_rest_ty
                                    ~default:TUnknown)
                                rest_index;
                            return_ty = TUnknown;
                          };
                      })))
    | FList [ (FVector _ as params_form) ] ->
        parse_clause (FList [ params_form; FSymbol "nil" ])
    | _ ->
        Error.error
          ("defn " ^ source_name
         ^ " multi-arity clauses must contain a parameter vector and body")
  in
  let rec parse acc = function
    | [] -> Ok (List.rev acc)
    | form :: rest -> (
        match parse_clause form with
        | Error _ as err -> err
        | Ok clause -> parse (clause :: acc) rest)
  in
  match parse [] forms with
  | Error _ as err -> err
  | Ok clauses ->
      let rec validate seen_fixed seen_variadic = function
        | [] -> Ok clauses
        | clause :: rest -> (
            match clause.rest_index with
            | None ->
                if seen_variadic then
                  Error.error
                    ("defn " ^ source_name ^ " variadic arity must be last")
                else if List.mem clause.fixed_count seen_fixed then
                  Error.error
                    ("defn " ^ source_name ^ " has duplicate arity "
                   ^ string_of_int clause.fixed_count)
                else validate (clause.fixed_count :: seen_fixed) false rest
            | Some _ ->
                if seen_variadic then
                  Error.error
                    ("defn " ^ source_name ^ " has multiple variadic arities")
                else if rest <> [] then
                  Error.error
                    ("defn " ^ source_name ^ " variadic arity must be last")
                else validate seen_fixed true rest)
      in
      validate [] false clauses

and multi_arity_target_name ocaml_name index (arity : fn_arity) =
  let kind =
    match arity.rest_param with None -> "arity" | Some _ -> "variadic"
  in
  ocaml_name ^ "__" ^ kind ^ "_"
  ^ string_of_int (List.length arity.fixed_params)
  ^ "_" ^ string_of_int index

and multi_arity_value targets =
  match targets with
  | [] -> Semantic_ir.Unit
  | target :: rest ->
      Semantic_ir.Tuple [ Semantic_ir.Ident target; multi_arity_value rest ]

and prepare_multi_arity_fn ~ocaml_name scope env source_name forms =
  match parse_multi_arity_clauses source_name forms with
  | Error _ as err -> err
  | Ok parsed_clauses ->
      let initial_arities =
        List.map (fun clause -> clause.initial_arity) parsed_clauses
      in
      let targets =
        List.mapi
          (fun index arity -> multi_arity_target_name ocaml_name index arity)
          initial_arities
      in
      let all_targets = targets in
      let minimum_fixed_count =
        parsed_clauses
        |> List.map (fun clause -> clause.fixed_count)
        |> List.fold_left min max_int
      in
      let rec form_conjoins name = function
        | FList (FSymbol "conj" :: FSymbol target :: _) when target = name ->
            true
        | FList forms | FVector forms -> List.exists (form_conjoins name) forms
        | FMap pairs ->
            List.exists
              (fun (key, value) ->
                form_conjoins name key || form_conjoins name value)
              pairs
        | _ -> false
      in
      let rec compile final_pass compiled arities clauses remaining_targets =
        match (clauses, remaining_targets) with
        | [], [] ->
            let clauses = List.rev compiled in
            let arity_for_count count =
              arities
              |> List.find_opt (fun (arity : fn_arity) ->
                     match arity.rest_param with
                     | None -> List.length arity.fixed_params = count
                     | Some _ -> List.length arity.fixed_params <= count)
            in
            let arities =
              List.map2
                (fun (parsed : multi_arity_clause) arity ->
                  match parsed.body_forms with
                  | [ FList (FSymbol name :: arguments) ]
                    when name = source_name
                         || name = Names.scoped_key scope source_name -> (
                      match arity_for_count (List.length arguments) with
                      | Some target ->
                          { arity with return_ty = target.return_ty }
                      | None -> arity)
                  | _ -> arity)
                parsed_clauses arities
            in
            let arity_index_for_count count =
              let rec find index = function
                | [] -> None
                | (arity : fn_arity) :: rest -> (
                    match arity.rest_param with
                    | None when List.length arity.fixed_params = count ->
                        Some index
                    | Some _ when List.length arity.fixed_params <= count ->
                        Some index
                    | None | Some _ -> find (index + 1) rest)
              in
              find 0 arities
            in
            let rec nullable_self_call_parameters acc = function
              | FList (FSymbol name :: arguments)
                when name = source_name
                     || name = Names.scoped_key scope source_name ->
                  let acc =
                    match arity_index_for_count (List.length arguments) with
                    | None -> acc
                    | Some arity_index ->
                        let rec collect parameter_index acc = function
                          | [] -> acc
                          | FSymbol "nil" :: rest ->
                              collect (parameter_index + 1)
                                ((arity_index, parameter_index) :: acc)
                                rest
                          | _ :: rest ->
                              collect (parameter_index + 1) acc rest
                        in
                        collect 0 acc arguments
                  in
                  List.fold_left nullable_self_call_parameters acc arguments
              | FList forms | FVector forms ->
                  List.fold_left nullable_self_call_parameters acc forms
              | FMap pairs ->
                  List.fold_left
                    (fun acc (key, value) ->
                      nullable_self_call_parameters
                        (nullable_self_call_parameters acc key)
                        value)
                    acc pairs
              | _ -> acc
            in
            let nullable_parameters =
              List.fold_left
                (fun acc (clause : multi_arity_clause) ->
                  List.fold_left nullable_self_call_parameters acc
                    clause.body_forms)
                [] parsed_clauses
            in
            let arities =
              List.mapi
                (fun arity_index (arity : fn_arity) ->
                  let fixed_params =
                    List.mapi
                      (fun parameter_index ty ->
                        if
                          List.mem (arity_index, parameter_index)
                            nullable_parameters
                        then
                          match ty with
                          | TNullable _ -> ty
                          | ty -> TNullable ty
                        else ty)
                      arity.fixed_params
                  in
                  { arity with fixed_params })
                arities
            in
            if not final_pass then
              compile true [] arities parsed_clauses all_targets
            else
              let ty = TOverloaded_fn arities in
              Ok
                {
                  clauses;
                  expr =
                    typed_ir ty
                      (multi_arity_value
                         (List.map (fun c -> c.target_name) clauses));
                }
        | clause :: rest, target_name :: rest_targets -> (
            let self_binding =
              Types.binding ~overload_targets:all_targets ocaml_name
                (TOverloaded_fn arities)
            in
            let clause_env =
              Env.add (Names.scoped_key scope source_name) self_binding env
            in
            let param_type_overrides =
              let params =
                match clause.params with FVector params -> params | _ -> []
              in
              let current_arity = List.nth arities (List.length compiled) in
              List.mapi
                (fun index param ->
                  match clause.rest_index with
                  | Some rest_index when index = rest_index -> (
                      match
                        (List.nth arities (List.length compiled)).rest_param
                      with
                      | Some TUnknown | None -> None
                      | Some element_ty -> Some (TSeq element_ty))
                  | _ -> (
                      match List.nth_opt current_arity.fixed_params index with
                      | Some (TNullable _ as ty) -> Some ty
                      | _ -> (
                          match param with
                          | FSymbol name
                            when index >= minimum_fixed_count
                                 && List.exists (form_conjoins name)
                                      clause.body_forms ->
                              Some (Types.dynamic_constraint TUnknown)
                          | _ -> None)))
                params
            in
            match
               prepare_fn ~param_type_overrides ~materialize_open_equality:true
                 ?variadic_rest_index:clause.rest_index ~recur_target:target_name
                 scope clause_env clause.params clause.body_forms
             with
            | Error _ as err -> err
            | Ok parts ->
                let param_tys =
                  List.map
                    (fun (_key, (binding : binding)) -> binding.ty)
                    parts.param_bindings
                in
                let fixed_params, rest_param =
                  match clause.rest_index with
                  | None -> (param_tys, None)
                  | Some index ->
                      let fixed =
                        List.filteri
                          (fun current _ -> current < index)
                          param_tys
                      in
                      let rest =
                        match List.nth param_tys index with
                        | TSeq element_ty -> element_ty
                        | ty -> ty
                      in
                      (fixed, Some rest)
                in
                let arity =
                  { fixed_params; rest_param; return_ty = parts.body.ty }
                in
                let current_index = List.length compiled in
                let arities =
                  List.mapi
                    (fun index current ->
                      if index = current_index then arity else current)
                    arities
                in
                let nullable_row_indices =
                  match Destructure.parse_param_specs clause.params with
                  | Error _ -> []
                  | Ok specs ->
                      specs
                      |> List.mapi (fun index spec ->
                           if spec.Destructure.destructured then Some index
                           else None)
                      |> List.filter_map Fun.id
                in
                let row_param_types =
                  row_param_type_names ~nullable_row_indices target_name
                    param_tys
                in
                compile final_pass
                  ({ target_name; parts; row_param_types } :: compiled)
                  arities rest rest_targets)
        | _ -> Error.error "internal error: multi-arity clause targets"
      in
      compile false [] initial_arities parsed_clauses targets

and lower_prepared_multi_arity (prepared : prepared_multi_arity_fn) =
  let targets = List.map (fun clause -> clause.target_name) prepared.clauses in
  let overload_row_param_types =
    List.map (fun clause -> clause.row_param_types) prepared.clauses
  in
  let row_items =
    List.concat_map
      (fun clause ->
        let param_tys =
          List.map
            (fun (_key, (binding : binding)) -> binding.ty)
            clause.parts.param_bindings
        in
        row_type_items clause.row_param_types param_tys)
      prepared.clauses
  in
  let recursive_bindings =
    List.map
      (fun clause ->
        let expression =
          fn_code ~row_param_type_names:clause.row_param_types clause.parts
        in
        ({
           name = clause.target_name;
           identity = None;
           expression = expression.semantic_expr;
         }
          : Lowered.recursive_value))
      prepared.clauses
  in
  (targets, overload_row_param_types, row_items, recursive_bindings)

and prepare_recursive_fn ~ocaml_name scope env source_name return_ty params
    body_forms =
  match Destructure.parse_param_specs params with
  | Error _ as err -> err
  | Ok specs -> (
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
        let env =
          Env.add (Names.scoped_key scope source_name) self_binding env
          |> Env.add ocaml_name self_binding
        in
        match
          prepare_fn
            ~param_type_overrides:(List.map Option.some param_tys)
            scope env params body_forms
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
                ^ Types.source_name return_ty))

and prepare_inferred_recursive_fn ~ocaml_name scope env source_name params
    body_forms =
  match Destructure.parse_param_specs params with
  | Error _ as err -> err
  | Ok specs -> (
      let param_type_overrides =
        List.map (fun (spec : Destructure.param_spec) -> spec.explicit_ty) specs
      in
      let param_tys =
        List.map
          (fun (spec : Destructure.param_spec) ->
            Option.value spec.explicit_ty ~default:TUnknown
            |> Function_elaborator.infer_named_record scope env)
          specs
      in
      let self_binding = Types.binding ocaml_name (TFn (param_tys, TUnknown)) in
      let provisional_env =
        Env.add (Names.scoped_key scope source_name) self_binding env
      in
      let inference_params =
        specs
        |> List.fold_left
             (fun params (spec : Destructure.param_spec) ->
               let ty =
                 Option.value spec.explicit_ty ~default:TUnknown
                 |> Function_elaborator.infer_named_record scope env
               in
               let params = (spec.source_name, ty) :: params in
               if spec.destructured then
                 Destructure.pattern_names spec.pattern
                 |> List.fold_left
                      (fun params name -> (name, TUnknown) :: params)
                      params
               else params)
             []
        |> List.rev
      in
      let lookup_function_ty = lookup_function_ty scope provisional_env in
      let lookup_protocol_constraint =
        Protocol.constraint_type scope provisional_env
      in
      let lookup_dynamic_key_record_type =
        Expression_support.dynamic_key_record_type provisional_env
      in
      let resolve_named_record =
        Function_elaborator.infer_named_record scope provisional_env
      in
      let explicitly_dynamic_params =
        specs
        |> List.filter_map (fun (spec : Destructure.param_spec) ->
               match spec.explicit_ty with
               | Some ty when Types.is_dynamic ty -> Some spec.source_name
               | Some _ | None -> None)
      in
      match
        Type_inference.infer_params ~explicitly_dynamic_params
          ~materialize_open_equality:true
          ~lookup_function_ty
          ~lookup_protocol_constraint ~lookup_dynamic_key_record_type
          ~resolve_named_record
          inference_params body_forms
      with
      | Error _ as err -> err
      | Ok inferred ->
          let inferred_param_tys =
            List.map
              (fun (spec : Destructure.param_spec) ->
                List.assoc_opt spec.source_name inferred
                |> Option.value ~default:TUnknown
                |> Function_elaborator.infer_named_record scope env)
              specs
          in
          let dynamic_param_tys =
            List.map
              (fun ty ->
                if
                  Option.is_some (Types.protocol_constraint_info ty)
                  && Option.is_some (Types.seqable_constraint_info ty)
                then Types.dynamic_constraint ty
                else ty)
              inferred_param_tys
          in
          let prepare_recursive_parts self_param_tys overrides =
            let prepare return_ty =
              let self_binding =
                Types.binding ocaml_name (TFn (self_param_tys, return_ty))
              in
              let env =
                Env.add (Names.scoped_key scope source_name) self_binding env
                |> Env.add ocaml_name self_binding
              in
              prepare_fn ~param_type_overrides:overrides
                ~materialize_open_equality:true ~recur_target:ocaml_name scope
                env params body_forms
            in
            Result.bind (prepare TUnknown) (fun provisional ->
                let requires_specialized_self_calls =
                  Semantic_ir.exists_identifier
                    (fun name ->
                      String.starts_with
                        ~prefix:"Lg_runtime.Runtime_dynamic" name)
                    provisional.body.semantic_expr
                in
                if
                  Types.equal provisional.body.ty TUnknown
                  || not requires_specialized_self_calls
                then Ok provisional
                else prepare provisional.body.ty)
          in
          if
            List.exists Types.is_dynamic dynamic_param_tys
            || List.exists2
                 (fun inferred dynamic -> not (Types.equal inferred dynamic))
                 inferred_param_tys dynamic_param_tys
          then
            let self_param_tys =
              List.map2
                (fun inferred dynamic ->
                  if Types.is_dynamic dynamic then dynamic else inferred)
                inferred_param_tys dynamic_param_tys
            in
            let overrides =
              List.map2
                (fun explicit inferred ->
                  match explicit with
                  | Some _ -> explicit
                  | None when Types.equal inferred TUnknown -> None
                  | None -> Some inferred)
                param_type_overrides dynamic_param_tys
            in
            prepare_recursive_parts self_param_tys overrides
          else
            prepare_recursive_parts inferred_param_tys param_type_overrides)

and fn_code ?(row_param_type_names = []) parts =
  Function_elaborator.fn_code ~row_param_type_names parts

and compile_fn ?(param_type_overrides = []) scope env params body_forms =
  match prepare_fn ~param_type_overrides scope env params body_forms with
  | Error _ as err -> err
  | Ok parts when unresolved_contextual_type parts.body.ty ->
      Error.error "empty list requires a contextual element type"
  | Ok parts -> Ok (fn_code parts)

and compile_named_fn scope env name params body_forms =
  match Destructure.parse_param_specs params with
  | Error _ as error -> error
  | Ok specs ->
      let parameter_tys =
        List.map
          (fun (spec : Destructure.param_spec) ->
            Option.value spec.explicit_ty ~default:TUnknown)
          specs
      in
      let ocaml_name =
        "__lg_named_fn_" ^ Names.sanitize_name name
      in
      let self_binding =
        Types.binding ocaml_name (TFn (parameter_tys, TUnknown))
      in
      let body_env =
        Env.add (Names.scoped_key scope name) self_binding env
      in
      Result.bind (compile_fn scope body_env params body_forms) (fun function_ ->
          match Semantic_ir.unlocated function_.semantic_expr with
          | Semantic_ir.Fun (patterns, body) ->
              Ok
                {
                  function_ with
                  semantic_expr =
                    Semantic_ir.annotate function_.ty
                      (Semantic_ir.LetRecIn
                         ( ocaml_name,
                           patterns,
                           body,
                           Semantic_ir.Ident ocaml_name ));
                }
          | _ -> Error.error "named fn requires a function body")

and compile_call scope env name arg_forms =
  (Lazy.force context).calls.compile_call scope env name arg_forms

and context : Elaboration_context.t Lazy.t =
  lazy (Elaboration_context.create ~compile_expr)

let compile_args_for scope env arg_forms =
  (Lazy.force context).calls.compile_args_for scope env arg_forms
