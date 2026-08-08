open Ast
open Types
open Expression_support
module Env = Compiler_environment

type expression_result = (typed_expr, Error.t) result
type call = string -> Env.t -> Ast.form list -> expression_result

type named_call =
  string -> Env.t -> string -> Ast.form list -> expression_result

type t = {
  compile_apply : call;
  compile_comp : call;
  compile_partial : call;
  compile_constantly : call;
  compile_predicate_combinator : named_call;
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
    ~pack_constrained_value =
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
  let compile_function_arg scope env = function
    | FSymbol name ->
        lookup_function scope env name |> Result.map require_callable_value
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
  let prepare_apply_argument env ~expected_ty argument =
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
                      | FSymbol ("concat" | "clojure.core/concat") ->
                          let collection_name = "__lg_apply_concat_collection" in
                          let collection =
                            typed_ir inner (Semantic_ir.Ident collection_name)
                          in
                          let collect_fixed () =
                            let rec collect sequences = function
                              | [] -> Ok (List.rev sequences)
                              | collection :: rest -> (
                                  match
                                    Collection_capability.to_seq_expr env
                                      collection
                                  with
                                  | Error _ ->
                                      Error.error
                                        "apply concat expects collections"
                                  | Ok (element_ty, sequence) ->
                                      collect
                                        ((element_ty, sequence) :: sequences)
                                        rest)
                            in
                            collect [] fixed_args
                          in
                          (match
                             ( collect_fixed (),
                               Collection_capability.to_seq_expr env collection )
                          with
                          | (Error _ as error), _ -> error
                          | _, Error _ ->
                              Error.error
                                ("apply concat expects collections, got "
                               ^ Types.source_name inner)
                          | Ok fixed_sequences, Ok (element_ty, sequence) ->
                              if
                                List.for_all
                                  (fun (fixed_element_ty, _) ->
                                    Types.assignable ~policy:Host_boundary
                                      ~expected:element_ty
                                      ~actual:fixed_element_ty)
                                  fixed_sequences
                              then
                                let rest_sequences =
                                  apply "List.map"
                                    [
                                      Semantic_ir.Fun
                                        ( [
                                            Semantic_ir.PVar collection_name;
                                          ],
                                          sequence );
                                      list_expr;
                                    ]
                                in
                                let sequences =
                                  match fixed_sequences with
                                  | [] -> rest_sequences
                                  | _ ->
                                      Semantic_ir.Infix
                                        ( "@",
                                          Semantic_ir.List
                                            (List.map snd fixed_sequences),
                                          rest_sequences )
                                in
                                Ok
                                  (typed_ir (TSeq element_ty)
                                     (apply
                                        "Lg_runtime.Runtime_seq.concat"
                                        [ sequences ]))
                              else
                                Error.error
                                  "apply concat element types must match")
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
                      | FSymbol ("distinct?" | "clojure.core/distinct?") ->
                          if
                            List.for_all
                              (fun argument ->
                                Types.equal inner TUnknown
                                || Types.assignable ~policy:Host_boundary
                                     ~expected:inner ~actual:argument.ty)
                              fixed_args
                          then
                            let values_expr =
                              match fixed_args with
                              | [] -> list_expr
                              | _ ->
                                  Semantic_ir.Infix
                                    ( "@",
                                      Semantic_ir.List
                                        (List.map
                                         (fun argument ->
                                           argument.semantic_expr)
                                           fixed_args),
                                      list_expr )
                            in
                            if
                              Types.is_dynamic inner
                              || match inner with
                                 | TUnknown | TMeta _ | TVar _ -> true
                                 | _ -> false
                            then
                              let equality =
                                if Types.is_dynamic inner then
                                  "Lg_runtime.Runtime_dynamic.equal"
                                else
                                  "Lg_runtime.Runtime_static_value.equal"
                              in
                              Ok
                                (typed_ir TBool
                                   (apply
                                      "Lg_runtime.Runtime_seq.all_distinct"
                                      [
                                        Semantic_ir.Ident equality;
                                        values_expr;
                                      ]))
                            else
                              Ok
                                (typed_ir TBool
                                   (Semantic_ir.Let
                                    ( [
                                        ( Semantic_ir.PVar
                                            "__lg_apply_values",
                                          values_expr );
                                      ],
                                        Semantic_ir.Infix
                                          ( "=",
                                            apply "List.length"
                                            [
                                              apply "List.sort_uniq"
                                                [
                                                  Semantic_ir.Ident
                                                      "Stdlib.compare";
                                                    Semantic_ir.Ident
                                                      "__lg_apply_values";
                                                ];
                                            ],
                                            apply "List.length"
                                            [
                                              Semantic_ir.Ident
                                                "__lg_apply_values";
                                            ] ) )))
                          else
                            Error.error
                              "apply distinct? arguments must have the same type"
                      | _ -> (
                        match compile_function_arg scope env fn_form with
                        | Error _ as err -> err
                        | Ok fn -> (
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
                                                Semantic_ir.Match
                                                  ( list_expr,
                                                    List.map
                                                        (fun
                                                          (pattern, expression)
                                                           ->
                                                          ( pattern,
                                                            expression.semantic_expr
                                                          ))
                                                      cases
                                                    @ [
                                                        ( Semantic_ir.PAny,
                                                          apply "invalid_arg"
                                                            [
                                                              Semantic_ir.String
                                                                "wrong apply \
                                                                 argument count";
                                                            ] );
                                                      ] ) ))))
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
              let rec check_chain = function
                | [] -> Error.error "comp expects at least 1 function"
                | [ fn ] -> (
                    match fn.ty with
                    | TFn ([ arg ], ret) -> Ok (arg, ret)
                    | TFn _ -> Error.error "comp expects unary functions"
                    | _ -> Error.error "comp expects functions")
                | left :: (right :: _ as rest) -> (
                    match (left.ty, right.ty) with
                  | ( TFn ([ left_arg ], _left_ret),
                      TFn ([ _right_arg ], right_ret) )
                      when Types.equal left_arg right_ret ->
                      check_chain rest
                      |> Result.map (fun (arg, _ret) ->
                            match List.hd fns with
                          | { ty = TFn ([ _ ], final_ret); _ } ->
                              (arg, final_ret)
                            | _ -> (arg, right_ret))
                  | TFn _, TFn _ ->
                      Error.error "comp function types do not line up"
                    | _ -> Error.error "comp expects functions")
              in
              match check_chain fns with
              | Error _ as err -> err
              | Ok (arg_ty, ret_ty) ->
                  let inner =
                    List.rev fns
                    |> List.fold_left
                         (fun expression fn ->
                           Semantic_ir.Apply (fn.semantic_expr, [ expression ]))
                         (Semantic_ir.Ident "x")
                  in
                  Ok
                    (typed_ir (TFn ([ arg_ty ], ret_ty))
                       (Semantic_ir.Fun ([ Semantic_ir.PVar "x" ], inner)))))
    
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
    
    and compile_constantly scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok [ value ] ->
          let arity : fn_arity =
            {
              fixed_params = [];
              rest_param = Some (Types.dynamic_constraint TUnknown);
              return_ty = value.ty;
            }
          in
          Ok
            (typed_ir (TOverloaded_fn [ arity ])
               (Semantic_ir.Tuple
                  [
                    Semantic_ir.Fun
                      ([ Semantic_ir.PAny ], value.semantic_expr);
                    Semantic_ir.Unit;
                  ]))
      | Ok _ -> Error.error "constantly expects 1 arguments"
    
    and compile_predicate_combinator scope env name arg_forms =
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
      | Ok [] -> Error.error (name ^ " expects at least 1 predicate")
      | Ok fns -> (
          let rec collect arg_ty exprs = function
            | [] -> Ok (arg_ty, List.rev exprs)
            | fn :: rest -> (
                match fn.ty with
                | TFn ([ current_arg ], TBool)
                when option_for_all
                       (fun arg_ty -> Types.equal arg_ty current_arg)
                       arg_ty ->
                    collect (Some current_arg)
                    (Semantic_ir.Apply
                       (fn.semantic_expr, [ Semantic_ir.Ident "x" ])
                    :: exprs)
                      rest
                | TFn _ ->
                  Error.error
                    (name ^ " expects predicates with the same argument type")
                | _ -> Error.error (name ^ " expects predicates"))
          in
          match collect None [] fns with
          | Error _ as err -> err
          | Ok (None, _) -> Error.error (name ^ " expects at least 1 predicate")
          | Ok (Some arg_ty, exprs) ->
              let op = if name = "every-pred" then "&&" else "||" in
              let body =
                match exprs with
                | [] -> Semantic_ir.Bool (name = "every-pred")
                | first :: rest ->
                    List.fold_left
                      (fun acc expr -> Semantic_ir.Infix (op, acc, expr))
                      first rest
              in
              Ok
              (typed_ir
                 (TFn ([ arg_ty ], TBool))
                   (Semantic_ir.Fun ([ Semantic_ir.PVar "x" ], body))))
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
    compile_constantly;
    compile_predicate_combinator;
    compile_juxt;
  }
