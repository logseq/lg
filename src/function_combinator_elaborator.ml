open Ast
open Types
open Expression_support
module Env = Compiler_environment

type expression_result = (typed_expr, Error.t) result
type call = string -> Env.t -> Ast.form list -> expression_result

type t = {
  compile_apply : call;
  compile_comp : call;
  compile_partial : call;
  compile_juxt : call;
}

let compile_args_for compile_expr scope env arg_forms =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | form :: rest -> (
        match compile_expr scope env form with
        | Ok expr -> loop (expr :: acc) rest
        | Error _ as err -> err)
  in
  loop [] arg_forms

let create ~compile_expr ~dynamic_unpack ~pack_dynamic_value
    ~pack_constrained_value ~adapt_value_to_type =
  let compile_args_for = compile_args_for compile_expr in
  let overloaded_apply_counter = ref 0 in
  let rec require_callable_value expression =
    match expression.ty with
    | TNullable inner | TOcaml_app ("option", [ inner ]) ->
        require_callable_value
          {
            expression with
            ty = inner;
            semantic_expr =
              apply "Option.get" [ expression.semantic_expr ];
          }
    | _ -> expression
  in
  let collection_to_list_expr env collection =
    Collection_capability.to_seq_expr env collection
    |> Result.map (fun (element_type, sequence) ->
        (element_type, apply "List.of_seq" [ sequence ]))
  in
  let rec concrete_sequence_element = function
    | TVector element | TList element | TSet element | TSeq element
    | TArray element ->
        Some element
    | TString -> Some TChar
    | TNullable inner | TOcaml_app ("option", [ inner ]) ->
        concrete_sequence_element inner
    | ty -> Types.seqable_constraint_element ty
  in
  let sequence_concat_element = function
    | TOverloaded_fn ({ return_ty = TSeq element_ty; _ } :: _ as arities)
      when List.for_all
             (fun (arity : fn_arity) ->
               Types.equal arity.return_ty (TSeq element_ty)
               && List.for_all
                    (fun parameter_ty ->
                      match Types.seqable_constraint_element parameter_ty with
                      | Some actual -> Types.equal actual element_ty
                      | None -> false)
                    arity.fixed_params
               &&
               match arity.rest_param with
               | None -> true
               | Some rest_ty -> (
                   match Types.seqable_constraint_element rest_ty with
                   | Some actual -> Types.equal actual element_ty
                   | None -> false))
             arities ->
        Some element_ty
    | TOverloaded_fn _ | TFn _ | _ -> None
  in
  let refine_sequence_concat_function fn fixed_args rest_item_ty =
    match sequence_concat_element fn.ty with
    | None -> fn
    | Some element_ty ->
        let actual_elements =
          List.filter_map
            (fun argument -> concrete_sequence_element argument.ty)
            fixed_args
          @ Option.to_list (concrete_sequence_element rest_item_ty)
        in
        let substitutions =
          List.fold_left
            (fun result actual ->
              Result.bind result (fun substitutions ->
                  Type_solver.unify substitutions element_ty actual))
            (Ok Type_solver.empty) actual_elements
        in
        (match substitutions with
        | Ok substitutions ->
            { fn with ty = Type_solver.apply substitutions fn.ty }
        | Error _ -> fn)
  in
  let compile_function_arg scope env = function
    | FSymbol name -> (
        match lookup_binding scope env name with
        | Ok binding ->
            let binding = Types.instantiate_binding binding in
            Ok
              (typed_ir binding.ty (binding_value_expression binding)
              |> require_callable_value)
        | Error _ ->
            lookup_function scope env name |> Result.map require_callable_value)
    | FKeyword keyword ->
        let dynamic = Types.dynamic_constraint TUnknown in
        let target_name = "__lg_keyword_function_target" in
        Ok
          (typed_ir (TFn ([ dynamic ], dynamic))
             (Semantic_ir.Fun
                ( [ Semantic_ir.PVar target_name ],
                  apply "Lg_runtime.Runtime_dynamic.get"
                    [ Semantic_ir.Ident target_name;
                      apply "Lg_runtime.Runtime_dynamic.keyword"
                        [ Semantic_ir.String keyword ];
                    ] )))
    | form ->
        compile_expr scope env form |> Result.map require_callable_value
  in
  let rec overloaded_projection expression index =
    if index = 0 then apply "fst" [ expression ]
    else overloaded_projection (apply "snd" [ expression ]) (index - 1)
  in
  let rec irrefutable_pattern = function
    | Semantic_ir.PAny | Semantic_ir.PVar _ -> true
    | Semantic_ir.PLocated (_, _, pattern)
    | Semantic_ir.PAlias (pattern, _)
    | Semantic_ir.PConstraint (pattern, _)
    | Semantic_ir.PTyped (pattern, _) ->
        irrefutable_pattern pattern
    | _ -> false
  in
  let rec list_pattern_coverage consumed = function
    | Semantic_ir.PLocated (_, _, pattern)
    | Semantic_ir.PAlias (pattern, _)
    | Semantic_ir.PConstraint (pattern, _)
    | Semantic_ir.PTyped (pattern, _) ->
        list_pattern_coverage consumed pattern
    | Semantic_ir.PList items -> Some (`Exact (consumed + List.length items))
    | Semantic_ir.PCons (_, tail) -> list_pattern_coverage (consumed + 1) tail
    | Semantic_ir.PAny | Semantic_ir.PVar _ -> Some (`At_least consumed)
    | _ -> None
  in
  let list_patterns_are_exhaustive cases =
    let coverages =
      List.filter_map
        (fun (pattern, _) -> list_pattern_coverage 0 pattern)
        cases
    in
    List.exists
      (function
        | `At_least minimum ->
            List.init minimum Fun.id
            |> List.for_all (fun length ->
                   List.exists
                     (function
                       | `Exact actual -> actual = length
                       | `At_least _ -> false)
                     coverages)
        | `Exact _ -> false)
      coverages
  in
  let apply_arity_match list_expr cases =
    match cases with
    | [ (pattern, expression) ] when irrefutable_pattern pattern -> expression
    | _ ->
        let cases =
          if
            List.exists (fun (pattern, _) -> irrefutable_pattern pattern) cases
            || list_patterns_are_exhaustive cases
          then cases
          else
            cases
            @ [
                ( Semantic_ir.PAny,
                  apply "invalid_arg"
                    [ Semantic_ir.String "wrong apply argument count" ] );
              ]
        in
        Semantic_ir.Match (list_expr, cases)
  in
  let variadic_function_matches expected_params expected_return rest_param
      actual_return =
    let unified =
      List.fold_left
        (fun result expected_param ->
          Result.bind result (fun substitutions ->
              Type_solver.unify substitutions rest_param expected_param))
        (Ok Type_solver.empty) expected_params
    in
    Result.is_ok
      (Result.bind unified (fun substitutions ->
           Type_solver.unify substitutions actual_return expected_return))
  in
  let prepare_apply_argument env ~expected_ty argument =
    match (expected_ty, argument.ty) with
    | ( TFn (expected_params, expected_return),
        TOverloaded_fn
          [
            {
              fixed_params = [];
              rest_param = Some rest_param;
              return_ty = actual_return;
            };
          ] )
      when variadic_function_matches expected_params expected_return rest_param
             actual_return ->
        let function_name = "__lg_apply_variadic_adapter_function" in
        let parameter_names =
          List.mapi
            (fun index _ ->
              "__lg_apply_variadic_adapter_argument_" ^ string_of_int index)
            expected_params
        in
        Ok
          (Semantic_ir.Let
             ( [ (Semantic_ir.PVar function_name, argument.semantic_expr) ],
               Semantic_ir.Fun
                 ( List.map
                     (fun name -> Semantic_ir.PVar name)
                     parameter_names,
                   Semantic_ir.Apply
                     ( overloaded_projection
                         (Semantic_ir.Ident function_name) 0,
                       [
                         apply "Lg_runtime.Runtime_seq.of_list"
                           [
                             Semantic_ir.List
                               (List.map
                                  (fun name -> Semantic_ir.Ident name)
                                  parameter_names);
                           ];
                       ] ) ) ))
    | ( TOverloaded_fn
          [
            {
              fixed_params = expected_fixed;
              rest_param = Some expected_rest;
              return_ty = expected_return;
            };
          ],
        TOverloaded_fn
          [
            {
              fixed_params = [];
              rest_param = Some actual_rest;
              return_ty = actual_return;
            };
          ] )
      when variadic_function_matches
             (expected_fixed @ [ expected_rest ])
             expected_return actual_rest actual_return ->
        let function_name = "__lg_apply_variadic_adapter_function" in
        let fixed_names =
          List.mapi
            (fun index _ ->
              "__lg_apply_variadic_adapter_argument_" ^ string_of_int index)
            expected_fixed
        in
        let rest_name = "__lg_apply_variadic_adapter_rest" in
        let all_arguments =
          Semantic_ir.Infix
            ( "@",
              Semantic_ir.List
                (List.map (fun name -> Semantic_ir.Ident name) fixed_names),
              apply "Lg_runtime.Runtime_seq.to_list"
                [ Semantic_ir.Ident rest_name ] )
        in
        Ok
          (Semantic_ir.Let
             ( [ (Semantic_ir.PVar function_name, argument.semantic_expr) ],
               Semantic_ir.Tuple
                 [
                   Semantic_ir.Fun
                     ( List.map
                         (fun name -> Semantic_ir.PVar name)
                         fixed_names
                       @ [ Semantic_ir.PVar rest_name ],
                       Semantic_ir.Apply
                         ( overloaded_projection
                             (Semantic_ir.Ident function_name) 0,
                           [
                             apply "Lg_runtime.Runtime_seq.of_list"
                               [ all_arguments ];
                           ] ) );
                   Semantic_ir.Unit;
                 ] ))
    | _ ->
    if
      Option.is_some (Types.protocol_constraint_info expected_ty)
      || Option.is_some (Types.seqable_constraint_info expected_ty)
    then pack_constrained_value env expected_ty argument
    else if Types.is_dynamic expected_ty then
      if
        Types.is_dynamic argument.ty
        || match argument.ty with TUnknown | TMeta _ | TVar _ -> true | _ -> false
      then Ok argument.semantic_expr
      else
        pack_dynamic_value env expected_ty argument
    else if Types.is_dynamic argument.ty then
      dynamic_unpack env expected_ty argument.semantic_expr
    else if
      Types.assignable ~policy:Host_boundary ~expected:expected_ty
        ~actual:argument.ty
    then Ok argument.semantic_expr
    else
      Error.error
        ("apply argument type mismatch: expected "
        ^ Types.source_name expected_ty
        ^ ", got "
        ^ Types.source_name argument.ty)
  in
  let compile_exact_apply env ~fn ~target ~fixed_args ~inner ~parameter_tys
      ~return_ty =
    let fixed_count = List.length fixed_args in
    if fixed_count > List.length parameter_tys then None
    else
      let fixed_parameter_tys =
        List.filteri (fun index _ -> index < fixed_count) parameter_tys
      in
      let remaining_parameter_tys = drop fixed_count parameter_tys in
      let argument_names =
        List.mapi
          (fun index _ -> "__lg_apply_argument_" ^ string_of_int index)
          remaining_parameter_tys
      in
      let rec prepare_fixed prepared expected arguments =
        match (expected, arguments) with
        | [], [] -> Ok (List.rev prepared)
        | expected_ty :: expected, argument :: arguments ->
            Result.bind
              (prepare_apply_argument env ~expected_ty argument)
              (fun expression ->
                prepare_fixed (expression :: prepared) expected arguments)
        | _ -> Error.error "internal apply argument mismatch"
      in
      let rec prepare_remaining prepared expected names =
        match (expected, names) with
        | [], [] -> Ok (List.rev prepared)
        | expected_ty :: expected, name :: names ->
            Result.bind
              (prepare_apply_argument env ~expected_ty
                 (typed_ir inner (Semantic_ir.Ident name)))
              (fun expression ->
                prepare_remaining (expression :: prepared) expected names)
        | _ -> Error.error "internal apply argument mismatch"
      in
      Some
        (Result.bind (prepare_fixed [] fixed_parameter_tys fixed_args)
           (fun fixed_arguments ->
             Result.map
               (fun remaining_arguments ->
                 ( Semantic_ir.PList
                     (List.map
                        (fun name -> Semantic_ir.PVar name)
                        argument_names),
                   typed_ir return_ty
                     (Semantic_ir.Apply
                        ( target fn.semantic_expr,
                          fixed_arguments @ remaining_arguments )) ))
               (prepare_remaining [] remaining_parameter_tys argument_names)))
  in
  let compile_variadic_apply env ~fn ~target ~fixed_args ~inner ~list_expr
      ~(arity : fn_arity) =
    match arity.rest_param with
    | None -> None
    | Some rest_ty ->
        let fixed_count = List.length arity.fixed_params in
        let given = List.length fixed_args in
        let rec prepare_fixed prepared expected arguments =
          match (expected, arguments) with
          | [], [] -> Ok (List.rev prepared)
          | expected_ty :: expected, argument :: arguments ->
              Result.bind
                (prepare_apply_argument env ~expected_ty argument)
                (fun expression ->
                  prepare_fixed (expression :: prepared) expected arguments)
          | _ -> Error.error "internal apply argument mismatch"
        in
        let adapt_rest_list list_expr =
          if Types.equal inner rest_ty then Ok list_expr
          else
            let item_name = "__lg_apply_rest_item" in
            match
              prepare_apply_argument env ~expected_ty:rest_ty
                (typed_ir inner (Semantic_ir.Ident item_name))
            with
            | Error _ as error -> error
            | Ok adapted -> (
                match Semantic_ir.unlocated adapted with
                | Semantic_ir.Ident name when String.equal name item_name ->
                    Ok list_expr
                | _ ->
                    Ok
                      (apply "List.map"
                         [
                           Semantic_ir.Fun
                             ([ Semantic_ir.PVar item_name ], adapted);
                           list_expr;
                         ]))
        in
        let rest_seq rest_list =
          apply "Lg_runtime.Runtime_seq.of_list" [ rest_list ]
        in
        let compiled =
          if given >= fixed_count then
            let direct_args =
              List.filteri (fun index _ -> index < fixed_count) fixed_args
            in
            let extra_args = drop fixed_count fixed_args in
            Result.bind
              (prepare_fixed [] arity.fixed_params direct_args)
              (fun fixed_arguments ->
                Result.bind
                  (prepare_fixed []
                     (List.init (List.length extra_args) (fun _ -> rest_ty))
                     extra_args)
                  (fun extra_arguments ->
                    Result.map
                      (fun rest_list ->
                        ( Semantic_ir.PAny,
                          typed_ir arity.return_ty
                            (Semantic_ir.Apply
                               ( target fn.semantic_expr,
                                 fixed_arguments
                                 @ [
                                     rest_seq
                                       (match extra_arguments with
                                       | [] -> rest_list
                                       | _ ->
                                           Semantic_ir.Infix
                                             ( "@",
                                               Semantic_ir.List extra_arguments,
                                               rest_list ));
                                   ] )) ))
                      (adapt_rest_list list_expr)))
          else
            let needed = fixed_count - given in
            let head_names =
              List.init needed (fun i -> "__lg_apply_head_" ^ string_of_int i)
            in
            let rest_name = "__lg_apply_rest" in
            let pattern =
              List.fold_right
                (fun name tail -> Semantic_ir.PCons (Semantic_ir.PVar name, tail))
                head_names (Semantic_ir.PVar rest_name)
            in
            Result.bind
              (prepare_fixed []
                 (List.filteri (fun index _ -> index < given)
                    arity.fixed_params)
                 fixed_args)
              (fun fixed_arguments ->
                Result.bind
                  (prepare_fixed []
                     (drop given arity.fixed_params)
                     (List.map
                        (fun name -> typed_ir inner (Semantic_ir.Ident name))
                        head_names))
                  (fun head_arguments ->
                    Result.map
                      (fun rest_list ->
                        ( pattern,
                          typed_ir arity.return_ty
                            (Semantic_ir.Apply
                               ( target fn.semantic_expr,
                                 fixed_arguments @ head_arguments
                                 @ [ rest_seq rest_list ] )) ))
                      (adapt_rest_list (Semantic_ir.Ident rest_name))))
        in
        Some compiled
  in
    let compile_apply scope env arg_forms =
      let rec split_last acc = function
        | [] -> None
        | [ last ] -> Some (List.rev acc, last)
        | item :: rest -> split_last (item :: acc) rest
      in
      match arg_forms with
      | fn_form :: rest -> (
          match split_last [] rest with
          | None -> Error.error "apply expects function and collection"
          | Some (fixed_forms, collection_form) -> (
              match
                ( compile_args_for scope env fixed_forms,
                  compile_expr scope env collection_form )
              with
              | (Error _ as err), _ -> err
              | _, (Error _ as err) -> err
              | Ok fixed_args, Ok collection -> (
                  match collection_to_list_expr env collection with
                  | Error _ ->
                      Error.error
                        ("apply expects a seqable value, got "
                        ^ Types.source_name collection.ty)
                  | Ok (inner, list_expr) -> (
                      match fn_form with
                      | FSymbol "str" ->
                          let value_name = "__lg_apply_str_value" in
                          let stringify_value =
                            Semantic_ir.Fun
                              ( [ Semantic_ir.PVar value_name ],
                                Codegen.stringify_expr_ir ~pr:false
                                (typed_ir inner (Semantic_ir.Ident value_name))
                            )
                          in
                          let collection_text =
                            apply "String.concat"
                            [
                              Semantic_ir.String "";
                                apply "List.map" [ stringify_value; list_expr ];
                              ]
                          in
                          let parts =
                          List.map
                            (Codegen.stringify_expr_ir ~pr:false)
                            fixed_args
                            @ [ collection_text ]
                          in
                          Ok (typed_ir TString (Codegen.concat_expr parts))
                    | FSymbol "+"
                      when Types.equal inner TInt
                           && List.for_all
                                (fun argument -> Types.equal argument.ty TInt)
                                fixed_args ->
                        let values =
                          match fixed_args with
                          | [] -> list_expr
                          | _ ->
                              Semantic_ir.Infix
                                ( "@",
                                  Semantic_ir.List
                                    (List.map
                                       (fun argument -> argument.semantic_expr)
                                       fixed_args),
                                  list_expr )
                        in
                        Ok
                          (typed_ir TInt
                             (apply "List.fold_left"
                                [
                                  Semantic_ir.Fun
                                    ( [ Semantic_ir.PVar "left";
                                        Semantic_ir.PVar "right";
                                      ],
                                      Semantic_ir.Infix
                                        ( "+",
                                          Semantic_ir.Ident "left",
                                          Semantic_ir.Ident "right" ) );
                                  Semantic_ir.Int 0;
                                  values;
                                ]))
                    | FSymbol ("pr" | "clojure.core/pr") -> (
                        match lookup_binding scope env "*out*" with
                        | Error _ ->
                            Error.error "pr requires a bound *out* writer"
                        | Ok writer ->
                            let value_name = "__lg_apply_pr_value" in
                            let render_value =
                              Semantic_ir.Fun
                                ( [ Semantic_ir.PVar value_name ],
                                  Codegen.stringify_expr_ir ~pr:true
                                    (typed_ir inner
                                       (Semantic_ir.Ident value_name)) )
                            in
                            let collection_texts =
                              apply "List.map" [ render_value; list_expr ]
                            in
                            let fixed_texts =
                              Semantic_ir.List
                                (List.map
                                   (Codegen.stringify_expr_ir ~pr:true)
                                   fixed_args)
                            in
                            let texts =
                              match fixed_args with
                              | [] -> collection_texts
                              | _ ->
                                  Semantic_ir.Infix
                                    ("@", fixed_texts, collection_texts)
                            in
                            Ok
                              (typed_ir TUnit
                                 (apply "Lg_runtime.Runtime_print.write"
                                    [
                                      Semantic_ir.Ident writer.ocaml_name;
                                      apply "String.concat"
                                        [ Semantic_ir.String " "; texts ];
                                    ])))
                      | _ -> (
                        match compile_function_arg scope env fn_form with
                        | Error _ as err -> err
                        | Ok fn -> (
                          let fn =
                            refine_sequence_concat_function fn fixed_args inner
                          in
                          match fn.ty with
                          | TFn ([ TInt; TInt ], TInt)
                            when Types.equal inner TInt
                                   && List.for_all
                                        (fun arg -> Types.equal arg.ty TInt)
                                        fixed_args ->
                          let values_expr =
                            match fixed_args with
                            | [] -> list_expr
                            | _ ->
                                Semantic_ir.Infix
                                  ( "@",
                                    Semantic_ir.List
                                            (List.map
                                               (fun arg -> arg.semantic_expr)
                                               fixed_args),
                                    list_expr )
                          in
                          Ok
                            (typed_ir TInt
                               (apply "List.fold_left"
                                        [
                                          fn.semantic_expr;
                                          Semantic_ir.Int 0;
                                          values_expr;
                                        ]))
                          | TFn ([ TInt; TInt ], TInt) ->
                                Error.error
                                  "apply currently supports int binary reducers"
                          | TFn (parameter_tys, return_ty) -> (
                              match
                                compile_exact_apply env ~fn
                                  ~target:(fun expression -> expression)
                                    ~fixed_args ~inner ~parameter_tys ~return_ty
                              with
                                | None ->
                                    Error.error
                                      "apply has too many fixed arguments"
                              | Some result ->
                                  Result.map
                                    (fun (pattern, result) ->
                                      typed_ir return_ty
                                        (Semantic_ir.Match
                                           ( list_expr,
                                               [
                                                 (pattern, result.semantic_expr);
                                               ( Semantic_ir.PAny,
                                                 apply "invalid_arg"
                                                     [
                                                       Semantic_ir.String
                                                         "wrong apply argument \
                                                          count";
                                                     ] );
                                             ] )))
                                    result)
                          | TOverloaded_fn arities ->
                              incr overloaded_apply_counter;
                              let function_name =
                                "__lg_apply_overloaded_function_"
                                ^ string_of_int !overloaded_apply_counter
                              in
                              let stable_fn =
                                {
                                  fn with
                                  semantic_expr =
                                    Semantic_ir.Ident function_name;
                                }
                              in
                              let compiled =
                                arities
                                |> List.mapi (fun index arity ->
                                       match arity.rest_param with
                                       | Some _ ->
                                           compile_variadic_apply env
                                             ~fn:stable_fn
                                             ~target:(fun expression ->
                                               overloaded_projection expression
                                                 index)
                                             ~fixed_args ~inner ~list_expr
                                             ~arity
                                       | None ->
                                           compile_exact_apply env ~fn:stable_fn
                                             ~target:(fun expression ->
                                              overloaded_projection expression
                                                index)
                                             ~fixed_args ~inner
                                             ~parameter_tys:arity.fixed_params
                                             ~return_ty:arity.return_ty)
                                |> List.filter_map Fun.id
                              in
                              let rec collect cases return_ty = function
                                | [] -> Ok (List.rev cases, return_ty)
                                | result :: rest -> (
                                    match result with
                                    | Error _ as error -> error
                                    | Ok (pattern, expression) -> (
                                        match return_ty with
                                          | None ->
                                              collect
                                                [ (pattern, expression) ]
                                                (Some expression.ty) rest
                                          | Some ty
                                            when Types.equal ty expression.ty ->
                                              collect
                                                ((pattern, expression) :: cases)
                                                return_ty rest
                                        | Some _ ->
                                            Error.error
                                                "apply overloads must return \
                                                 the same type"))
                              in
                              Result.bind (collect [] None compiled)
                                (fun (cases, return_ty) ->
                                  match (cases, return_ty) with
                                    | [], _ ->
                                        Error.error
                                          "apply has no matching function arity"
                                    | _, None ->
                                        Error.error
                                          "apply has no matching function arity"
                                  | cases, Some return_ty ->
                                      Ok
                                        (typed_ir return_ty
                                           (Semantic_ir.Let
                                              ( [
                                                  ( Semantic_ir.PVar
                                                      function_name,
                                                    fn.semantic_expr );
                                                ],
                                                apply_arity_match list_expr
                                                  (List.map
                                                     (fun (pattern, expression) ->
                                                       ( pattern,
                                                         expression.semantic_expr
                                                       ))
                                                     cases) ))))
                          | fn_type
                            when Types.is_dynamic fn_type
                                 || (match fn_type with
                                    | TUnknown | TMeta _ | TVar _ -> true
                                    | _ -> false) ->
                              Error.error
                                "apply requires a statically typed function; \
                                 define a closed sum type for multiple function \
                                 shapes"
                          | _ -> Error.error "apply expects a function"))))))
      | _ -> Error.error "apply expects function and collection"
    and compile_comp scope env arg_forms =
      match arg_forms with
      | [] -> Error.error "comp expects at least 1 function"
      | _ -> (
          let compiled =
            arg_forms
            |> List.fold_left
                 (fun acc form ->
                   match acc with
                   | Error _ as err -> err
                   | Ok fns -> (
                       match compile_function_arg scope env form with
                       | Error _ as err -> err
                       | Ok fn -> Ok (fn :: fns)))
                 (Ok [])
            |> Result.map List.rev
          in
          match compiled with
          | Error _ as err -> err
          | Ok fns -> (
              let concrete_seqable_element = function
                | TArray element | TList element | TVector element
                | TSet element | TSeq element ->
                    Some element
                | ty -> Types.next_seq_element ty
              in
              let rec unify_assignable substitutions ~expected ~actual =
                let expected = Type_solver.apply substitutions expected in
                let actual = Type_solver.apply substitutions actual in
                match Type_solver.unify substitutions expected actual with
                | Ok substitutions -> Ok substitutions
                | Error _ -> (
                    match (expected, actual) with
                    | expected, actual -> (
                        match
                          ( Types.seqable_constraint_info expected,
                            concrete_seqable_element actual )
                        with
                        | Some (_, expected_element, _), Some actual_element ->
                            (match
                               Type_solver.unify substitutions expected_element
                                 actual_element
                             with
                            | Ok substitutions -> Ok substitutions
                            | Error _ ->
                                Error.error "incompatible sequence elements")
                        | _ -> (
                            match (expected, actual) with
                            | TFn (expected_params, expected_return),
                              TFn (actual_params, actual_return)
                              when List.length expected_params
                                   = List.length actual_params ->
                                Result.bind
                                  (unify_function_parameters substitutions
                                     expected_params actual_params)
                                  (fun substitutions ->
                                    unify_assignable substitutions
                                      ~expected:expected_return
                                      ~actual:actual_return)
                            | TOverloaded_fn expected_arities,
                              TOverloaded_fn actual_arities
                              when List.length expected_arities
                                   = List.length actual_arities ->
                                List.fold_left2
                                  (fun result expected_arity actual_arity ->
                                    Result.bind result (fun substitutions ->
                                        unify_assignable_arity substitutions
                                          expected_arity actual_arity))
                                  (Ok substitutions) expected_arities
                                  actual_arities
                            | _ -> Error.error "incompatible function types")))
              and unify_function_parameters substitutions expected actual =
                List.fold_left2
                  (fun result expected_parameter actual_parameter ->
                    Result.bind result (fun substitutions ->
                        unify_assignable substitutions
                          ~expected:actual_parameter
                          ~actual:expected_parameter))
                  (Ok substitutions) expected actual
              and unify_assignable_arity substitutions expected actual =
                if
                  List.length expected.fixed_params
                  <> List.length actual.fixed_params
                then Error.error "incompatible function arities"
                else
                  Result.bind
                    (unify_function_parameters substitutions
                       expected.fixed_params actual.fixed_params)
                    (fun substitutions ->
                      let rest =
                        match (expected.rest_param, actual.rest_param) with
                        | None, None -> Ok substitutions
                        | Some expected, Some actual ->
                            unify_assignable substitutions ~expected:actual
                              ~actual:expected
                        | _ -> Error.error "incompatible function arities"
                      in
                      Result.bind rest (fun substitutions ->
                          unify_assignable substitutions
                            ~expected:expected.return_ty
                            ~actual:actual.return_ty))
              in
              let unary_type fn =
                match fn.ty with
                | TFn ([ arg ], ret) -> Ok (arg, ret)
                | TFn _ -> Error.error "comp expects unary functions"
                | _ -> Error.error "comp expects functions"
              in
              let rec unify_chain substitutions = function
                | [] | [ _ ] -> Ok substitutions
                | left :: (right :: _ as rest) ->
                    Result.bind (unary_type left) (fun (left_arg, _) ->
                        Result.bind (unary_type right) (fun (_, right_ret) ->
                            match
                              unify_assignable substitutions
                                ~expected:left_arg ~actual:right_ret
                            with
                            | Ok substitutions ->
                                unify_chain substitutions rest
                            | Error _ ->
                                Error.error
                                  ("comp function types do not line up: "
                                 ^ Types.source_name left_arg ^ " and "
                                 ^ Types.source_name right_ret)))
              in
              match unify_chain Type_solver.empty fns with
              | Error _ as err -> err
              | Ok substitutions -> (
                  match (unary_type (List.hd fns), unary_type (List.hd (List.rev fns))) with
                  | Error _ as err, _ | _, (Error _ as err) -> err
                  | Ok (_, ret_ty), Ok (arg_ty, _) ->
                      let arg_ty = Type_solver.apply substitutions arg_ty in
                      let ret_ty = Type_solver.apply substitutions ret_ty in
                      let rec compose expression = function
                        | [] -> Ok expression
                        | fn :: rest ->
                            Result.bind (unary_type fn) (fun (parameter, return_ty) ->
                                let parameter =
                                  Type_solver.apply substitutions parameter
                                in
                                let return_ty =
                                  Type_solver.apply substitutions return_ty
                                in
                                Result.bind
                                  (adapt_value_to_type env parameter expression)
                                  (fun argument ->
                                    compose
                                      (typed_ir return_ty
                                         (Semantic_ir.Apply
                                            (fn.semantic_expr, [ argument ])))
                                      rest))
                      in
                      Result.map
                        (fun inner ->
                          typed_ir (TFn ([ arg_ty ], ret_ty))
                            (Semantic_ir.Fun
                               ( [ Semantic_ir.PVar "x" ],
                                 inner.semantic_expr )))
                        (compose
                           (typed_ir arg_ty (Semantic_ir.Ident "x"))
                           (List.rev fns)))))
    
    and compile_partial scope env arg_forms =
      match arg_forms with
      | fn_form :: fixed_forms -> (
          match (compile_function_arg scope env fn_form, compile_args_for scope env fixed_forms) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok fixed_args -> (
              match fn.ty with
              | TFn (param_tys, ret) when List.length fixed_args < List.length param_tys ->
                  let fixed_tys = List.map (fun arg -> arg.ty) fixed_args in
                  let expected_fixed_tys = param_tys |> List.filteri (fun index _ -> index < List.length fixed_tys) in
                  if List.for_all2 Types.equal fixed_tys expected_fixed_tys then
                    let remaining_tys = drop (List.length fixed_args) param_tys in
                    let remaining_names =
                      remaining_tys |> List.mapi (fun index _ -> "arg" ^ string_of_int index)
                    in
                    let remaining_exprs =
                      remaining_names |> List.map (fun name -> Semantic_ir.Ident name)
                    in
                    Ok
                      (typed_ir (TFn (remaining_tys, ret))
                         (Semantic_ir.Fun
                            ( List.map (fun name -> Semantic_ir.PVar name) remaining_names,
                              Semantic_ir.Apply
                                ( fn.semantic_expr,
                                  List.map (fun arg -> arg.semantic_expr) fixed_args
                                  @ remaining_exprs ))))
                  else Error.error "partial fixed arguments do not match function"
              | TFn _ -> Error.error "partial requires fewer arguments than function arity"
              | _ -> Error.error "partial expects a function"))
      | _ -> Error.error "partial expects a function"
    
    and compile_juxt scope env arg_forms =
      let compile_fns =
        arg_forms
        |> List.fold_left
             (fun acc form ->
               match acc with
               | Error _ as err -> err
               | Ok fns -> (
                   match compile_function_arg scope env form with
                   | Error _ as err -> err
                   | Ok fn -> Ok (fn :: fns)))
             (Ok [])
        |> Result.map List.rev
      in
      match compile_fns with
      | Error _ as err -> err
      | Ok [] -> Error.error "juxt expects at least 1 function"
      | Ok fns -> (
          let rec collect arg_ty ret_ty exprs = function
            | [] -> Ok (arg_ty, ret_ty, List.rev exprs)
            | fn :: rest -> (
                match fn.ty with
                | TFn ([ current_arg ], current_ret)
                  when option_for_all
                         (fun arg_ty ->
                           Types.assignable ~policy:Host_boundary ~expected:arg_ty
                             ~actual:current_arg)
                         arg_ty
                       && option_for_all
                            (fun ret_ty ->
                            Types.assignable ~policy:Host_boundary
                              ~expected:ret_ty ~actual:current_ret)
                            ret_ty ->
                    collect (Some current_arg) (Some current_ret)
                    (Semantic_ir.Apply
                       (fn.semantic_expr, [ Semantic_ir.Ident "x" ])
                    :: exprs)
                      rest
                | TFn ([ current_arg ], _)
                  when option_for_all
                         (fun arg_ty ->
                           Types.assignable ~policy:Host_boundary ~expected:arg_ty
                             ~actual:current_arg)
                         arg_ty ->
                    Error.error "juxt functions must return the same type"
              | TFn _ ->
                  Error.error
                    "juxt functions must accept the same argument type"
                | _ -> Error.error "juxt expects functions")
          in
          match collect None None [] fns with
          | Error _ as err -> err
          | Ok (Some arg_ty, Some ret_ty, exprs) ->
              Ok
              (typed_ir
                 (TFn ([ arg_ty ], TVector ret_ty))
                   (Semantic_ir.Fun
                      ( [ Semantic_ir.PVar "x" ],
                        apply "Rrbvec.of_list" [ Semantic_ir.List exprs ] )))
          | Ok _ -> Error.error "juxt expects at least 1 function")
  in
  {
    compile_apply;
    compile_comp;
    compile_partial;
    compile_juxt;
  }
