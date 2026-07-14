open Ast
open Types
open Expression_support

module Env = Compiler_environment

type expression_result = (typed_expr, Error.t) result
type call = string -> Env.t -> Ast.form list -> expression_result
type named_call = string -> Env.t -> string -> Ast.form list -> expression_result

type t = {
  compile_sort_by : call;
  compile_mapcat : call;
  compile_repeatedly : call;
  compile_reductions : call;
  compile_split_with : call;
  compile_partition_by : call;
  compile_run_bang : call;
  compile_map_indexed : call;
  compile_filterv : call;
  compile_mapv : call;
  compile_reduce_kv : call;
  compile_some : call;
  compile_sequence_bool_predicate : named_call;
  compile_map_call : call;
  compile_filter : call;
  compile_reduce : call;
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

let returns_truthy_value = function
  | TBool | TUnknown | TVar _ -> true
  | ty -> Types.is_dynamic ty

let truthy_call return_ty fn arguments =
  let call = Semantic_ir.Apply (fn, arguments) in
  if Types.equal return_ty TBool then call
  else
    Semantic_ir.Apply
      (Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.truthy", [ call ])

let create ~compile_expr =
  let special_forms : Special_form_elaborator.t =
    Special_form_elaborator.create ~compile_expr
  in
  let compile_body = special_forms.compile_body in
  let compile_function_arg scope env = function
    | FSymbol name -> lookup_function scope env name
    | form -> compile_expr scope env form
  in
  let compile_function_arg_for_collection scope env element_ty = function
    | FKeyword keyword ->
        let item_name = "__lg_keyword_function_item" in
        let binding = Types.binding item_name element_ty in
        let function_env =
          Env.add (Names.scoped_key scope item_name) binding env
        in
        compile_expr scope function_env
          (FList [ FKeyword keyword; FSymbol item_name ])
        |> Result.map (fun body ->
               typed_ir (TFn ([ element_ty ], body.ty))
                 (Semantic_ir.Fun
                    ([ Semantic_ir.PVar item_name ], body.semantic_expr)))
    | FList
        (FSymbol "fn" :: (FVector [ _ ] as params) :: body_forms) ->
        let lookup_function_ty name =
          match lookup_function scope env name with
          | Ok fn -> Ok fn.ty
          | Error _ as error -> error
        in
        Function_elaborator.prepare
          ~param_type_overrides:[ Some element_ty ] ~lookup_function_ty
          ~compile_body scope env params body_forms
        |> Result.map Function_elaborator.fn_code
    | FSymbol name -> (
        match lookup_function scope env name with
        | Ok function_ -> Ok function_
        | Error _ ->
            let item_name = "__lg_protocol_function_item" in
            let binding = Types.binding item_name element_ty in
            let function_env =
              Env.add (Names.scoped_key scope item_name) binding env
            in
            compile_expr scope function_env
              (FList [ FSymbol name; FSymbol item_name ])
            |> Result.map (fun body ->
                   typed_ir (TFn ([ element_ty ], body.ty))
                     (Semantic_ir.Fun
                        ([ Semantic_ir.PVar item_name ], body.semantic_expr))))
    | form -> compile_function_arg scope env form
  in
  let compile_reducer scope env accumulator_ty element_ty = function
    | FList
        (FSymbol "fn" :: FVector [ FSymbol accumulator; FSymbol element ]
        :: body_forms) ->
        let accumulator_binding =
          Types.binding (Names.sanitize_name accumulator) accumulator_ty
        in
        let element_binding =
          Types.binding (Names.sanitize_name element) element_ty
        in
        let function_env =
          env
          |> Env.add (Names.scoped_key scope accumulator) accumulator_binding
          |> Env.add (Names.scoped_key scope element) element_binding
        in
        let pattern binding ty =
          match ty with
          | TNamed_record record ->
              Semantic_ir.PConstraint
                (Semantic_ir.PVar binding.ocaml_name, record.type_name)
          | _ -> Semantic_ir.PVar binding.ocaml_name
        in
        compile_body scope function_env "function body requires at least one form"
          body_forms
        |> Result.map (fun body ->
               typed_ir (TFn ([ accumulator_ty; element_ty ], body.ty))
                 (Semantic_ir.Fun
                    ( [ pattern accumulator_binding accumulator_ty;
                        pattern element_binding element_ty ],
                      body.semantic_expr )))
    | FList
        (FSymbol "fn" :: (FVector [ _accumulator; _element ] as params)
        :: body_forms) ->
        let lookup_function_ty name =
          match lookup_function scope env name with
          | Ok fn -> Ok fn.ty
          | Error _ as error -> error
        in
        Function_elaborator.prepare
          ~param_type_overrides:[ Some accumulator_ty; Some element_ty ]
          ~lookup_function_ty ~compile_body scope env params body_forms
        |> Result.map Function_elaborator.fn_code
    | form -> compile_function_arg scope env form
  in
  let compile_kv_reducer scope env accumulator_ty key_ty value_ty = function
    | FList
        (FSymbol "fn" :: (FVector [ _accumulator; _key; _value ] as params)
        :: body_forms) ->
        let lookup_function_ty name =
          match lookup_function scope env name with
          | Ok fn -> Ok fn.ty
          | Error _ as error -> error
        in
        Function_elaborator.prepare
          ~param_type_overrides:
            [ Some accumulator_ty; Some key_ty; Some value_ty ]
          ~lookup_function_ty ~compile_body scope env params body_forms
        |> Result.map Function_elaborator.fn_code
    | form -> compile_function_arg scope env form
  in
    let rec collection_to_list_expr collection =
      Core_sequence_transform.collection_to_list_expr collection
    
    and collection_from_list_expr collection_ty list_expr =
      Core_sequence_transform.collection_from_list_expr collection_ty list_expr
    
    and comparable_type = function
      | TInt | TString | TSymbol | TKeyword | TBool | TUnknown -> true
      | _ -> false
    
    and compile_sort_by scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok collection -> (
              match (fn.ty, collection_to_list_expr collection) with
              | TFn ([ param_ty ], key_ty), Ok (inner, list_expr)
                when Types.equal param_ty inner && comparable_type key_ty ->
                  Ok
                    (typed_ir (TList inner)
                       (apply "List.sort"
                          [ Semantic_ir.Fun
                              ( [ Semantic_ir.PVar "left"; Semantic_ir.PVar "right" ],
                                apply "Stdlib.compare"
                                  [ Semantic_ir.Apply (fn.semantic_expr, [ Semantic_ir.Ident "left" ]);
                                    Semantic_ir.Apply (fn.semantic_expr, [ Semantic_ir.Ident "right" ]) ] );
                            list_expr ]))
              | TFn ([ param_ty ], _), Ok (inner, _) when not (Types.equal param_ty inner) ->
                  Error.error "sort-by key function must match collection elements"
              | TFn _, Ok _ -> Error.error "sort-by key function must return a comparable value"
              | _, Ok _ -> Error.error "sort-by expects a function"
              | _, Error _ -> Error.error "sort-by expects a collection"))
      | _ -> Error.error "sort-by expects function and collection"
    
    and compile_mapcat scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match compile_expr scope env collection_form with
          | Error _ as error -> error
          | Ok collection -> (
              match Collection_capability.to_seq_expr env collection with
              | Error _ ->
                  Error.error
                    ("mapcat expects a collection, got "
                   ^ Types.source_name collection.ty)
              | Ok (inner, sequence) -> (
                  match
                    compile_function_arg_for_collection scope env inner fn_form
                  with
                  | Error _ as error -> error
                  | Ok ({ ty = TFn ([ param_ty ], return_ty); _ } as fn)
                    when Types.assignable ~policy:Host_boundary
                           ~expected:param_ty ~actual:inner ->
                      let item_name = "__lg_mapcat_item" in
                      let result =
                        typed_ir return_ty
                          (Semantic_ir.Apply
                             (fn.semantic_expr, [ Semantic_ir.Ident item_name ]))
                      in
                      (match Collection_capability.to_seq_expr env result with
                      | Error _ ->
                          Error.error
                            ("mapcat function must return a collection, got "
                           ^ Types.source_name return_ty)
                      | Ok (result_inner, result_sequence) ->
                          Ok
                            (typed_ir (TSeq result_inner)
                               (apply "Lg_runtime.Runtime_seq.flat_map"
                                  [ Semantic_ir.Fun
                                      ( [ Semantic_ir.PVar item_name ],
                                        result_sequence );
                                    sequence;
                                  ])))
                  | Ok { ty = TFn _; _ } ->
                      Error.error
                        "mapcat function argument type does not match collection"
                  | Ok _ -> Error.error "mapcat expects a function")))
      | _ -> Error.error "mapcat expects function and collection"
    
    and compile_repeatedly scope env arg_forms =
      match arg_forms with
      | count_form :: fn_form :: [] -> (
          match (compile_expr scope env count_form, compile_function_arg scope env fn_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok count, Ok fn -> (
              if not (Types.equal count.ty TInt) then Error.error "repeatedly count must be int"
              else
                match fn.ty with
                | TFn ([], ret) ->
                    let body =
                      Semantic_ir.If
                        ( Semantic_ir.Infix ("<=", Semantic_ir.Ident "n", Semantic_ir.Int 0),
                          Semantic_ir.Ident "acc",
                          apply "repeatedly"
                            [ Semantic_ir.Cons
                                ( Semantic_ir.Apply (fn.semantic_expr, []),
                                  Semantic_ir.Ident "acc" );
                              Semantic_ir.Infix ("-", Semantic_ir.Ident "n", Semantic_ir.Int 1) ] )
                    in
                    Ok
                      (typed_ir (TList ret)
                         (Semantic_ir.LetRec
                            ( "repeatedly",
                              [ Semantic_ir.PVar "acc"; Semantic_ir.PVar "n" ],
                              body,
                              [ Semantic_ir.List []; count.semantic_expr ] )))
                | TFn _ -> Error.error "repeatedly expects a zero-argument function"
                | _ -> Error.error "repeatedly expects a function"))
      | _ -> Error.error "repeatedly expects count and function"
    
    and compile_reductions scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok collection -> (
              match (fn.ty, collection_to_list_expr collection) with
              | TFn ([ acc_ty; item_ty ], ret), Ok (inner, list_expr)
                when Types.equal acc_ty inner && Types.equal item_ty inner && Types.equal ret inner ->
                  let reductions_body =
                    Semantic_ir.Match
                      ( Semantic_ir.Ident "xs",
                        [ (Semantic_ir.PList [], apply "List.rev" [ Semantic_ir.Ident "acc" ]);
                          ( Semantic_ir.PCons (Semantic_ir.PVar "item", Semantic_ir.PVar "tail"),
                            Semantic_ir.Let
                              ( [ ( Semantic_ir.PVar "next",
                                    Semantic_ir.Apply
                                      ( fn.semantic_expr,
                                        [ Semantic_ir.Ident "current"; Semantic_ir.Ident "item" ] ) ) ],
                                apply "reductions"
                                  [ Semantic_ir.Ident "next";
                                    Semantic_ir.Cons
                                      (Semantic_ir.Ident "next", Semantic_ir.Ident "acc");
                                    Semantic_ir.Ident "tail" ] ) ) ] )
                  in
                  Ok
                    (typed_ir (TList inner)
                       (Semantic_ir.Match
                          ( list_expr,
                            [ (Semantic_ir.PList [], Semantic_ir.List []);
                              ( Semantic_ir.PCons (Semantic_ir.PVar "first", Semantic_ir.PVar "rest"),
                                Semantic_ir.LetRec
                                  ( "reductions",
                                    [ Semantic_ir.PVar "current";
                                      Semantic_ir.PVar "acc";
                                      Semantic_ir.PVar "xs" ],
                                    reductions_body,
                                    [ Semantic_ir.Ident "first";
                                      Semantic_ir.List [ Semantic_ir.Ident "first" ];
                                      Semantic_ir.Ident "rest" ] ) ) ] )))
              | TFn _, Ok _ -> Error.error "reductions function type does not match collection"
              | _, Ok _ -> Error.error "reductions expects a function"
              | _, Error _ -> Error.error "reductions expects a collection"))
      | fn_form :: init_form :: collection_form :: [] -> (
          match
            ( compile_function_arg scope env fn_form,
              compile_expr scope env init_form,
              compile_expr scope env collection_form )
          with
          | (Error _ as err), _, _ -> err
          | _, (Error _ as err), _ -> err
          | _, _, (Error _ as err) -> err
          | Ok fn, Ok init, Ok collection -> (
              match (fn.ty, collection_to_list_expr collection) with
              | TFn ([ acc_ty; item_ty ], ret), Ok (inner, list_expr)
                when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
                  let reductions_body =
                    Semantic_ir.Match
                      ( Semantic_ir.Ident "xs",
                        [ (Semantic_ir.PList [], apply "List.rev" [ Semantic_ir.Ident "acc" ]);
                          ( Semantic_ir.PCons (Semantic_ir.PVar "item", Semantic_ir.PVar "rest"),
                            Semantic_ir.Let
                              ( [ ( Semantic_ir.PVar "next",
                                    Semantic_ir.Apply
                                      ( fn.semantic_expr,
                                        [ Semantic_ir.Ident "current"; Semantic_ir.Ident "item" ] ) ) ],
                                apply "reductions"
                                  [ Semantic_ir.Ident "next";
                                    Semantic_ir.Cons
                                      (Semantic_ir.Ident "next", Semantic_ir.Ident "acc");
                                    Semantic_ir.Ident "rest" ] ) ) ] )
                  in
                  Ok
                    (typed_ir (TList init.ty)
                       (Semantic_ir.LetRec
                          ( "reductions",
                            [ Semantic_ir.PVar "current";
                              Semantic_ir.PVar "acc";
                              Semantic_ir.PVar "xs" ],
                            reductions_body,
                            [ init.semantic_expr;
                              Semantic_ir.List [ init.semantic_expr ];
                              list_expr ] )))
              | TFn _, Ok _ -> Error.error "reductions function type does not match init and collection"
              | _, Ok _ -> Error.error "reductions expects a function"
              | _, Error _ -> Error.error "reductions expects a collection"))
      | _ -> Error.error "reductions expects function, optional init, and collection"
    
    and compile_split_with scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok collection -> (
              match (fn.ty, collection_to_list_expr collection) with
              | TFn ([ param_ty ], return_ty), Ok (inner, list_expr)
                when Types.equal param_ty inner
                     && returns_truthy_value return_ty ->
                  let split_body =
                    Semantic_ir.Match
                      ( Semantic_ir.Ident "rest",
                        [ ( Semantic_ir.PCons (Semantic_ir.PVar "item", Semantic_ir.PVar "tail"),
                            Semantic_ir.If
                              ( truthy_call return_ty fn.semantic_expr
                                  [ Semantic_ir.Ident "item" ],
                                apply "split"
                                  [ Semantic_ir.Cons
                                      (Semantic_ir.Ident "item", Semantic_ir.Ident "prefix");
                                    Semantic_ir.Ident "tail" ],
                                Semantic_ir.Tuple
                                  [ apply "List.rev" [ Semantic_ir.Ident "prefix" ];
                                    Semantic_ir.Ident "rest" ] ) );
                          ( Semantic_ir.PAny,
                            Semantic_ir.Tuple
                              [ apply "List.rev" [ Semantic_ir.Ident "prefix" ];
                                Semantic_ir.Ident "rest" ] ) ] )
                  in
                  let pair_expr =
                    Semantic_ir.LetRec
                      ( "split",
                        [ Semantic_ir.PVar "prefix"; Semantic_ir.PVar "rest" ],
                        split_body,
                        [ Semantic_ir.List []; list_expr ] )
                  in
                  Ok
                    (typed_ir (TVector collection.ty)
                       (Semantic_ir.Let
                          ( [ (Semantic_ir.PVar "pair", pair_expr) ],
                            apply "Rrbvec.of_list"
                              [ Semantic_ir.List
                                  [ collection_from_list_expr collection.ty
                                      (apply "fst" [ Semantic_ir.Ident "pair" ]);
                                    collection_from_list_expr collection.ty
                                      (apply "snd" [ Semantic_ir.Ident "pair" ]) ] ] )))
              | TFn _, Ok _ -> Error.error "split-with expects a predicate matching collection elements"
              | _, Ok _ -> Error.error "split-with expects a function"
              | _, Error _ -> Error.error "split-with expects a collection"))
      | _ -> Error.error "split-with expects function and collection"
    
    and compile_partition_by scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok collection -> (
              match (fn.ty, collection_to_list_expr collection) with
              | TFn ([ param_ty ], key_ty), Ok (inner, list_expr) when Types.equal param_ty inner ->
                  ignore key_ty;
                  let finish_call =
                    apply "finish" [ Semantic_ir.Ident "groups"; Semantic_ir.Ident "current" ]
                  in
                  let start_new_group =
                    Semantic_ir.Let
                      ( [ ( Semantic_ir.PVar "groups",
                            Semantic_ir.Match
                              ( Semantic_ir.Ident "current",
                                [ (Semantic_ir.PList [], Semantic_ir.Ident "groups");
                                  ( Semantic_ir.PAny,
                                    Semantic_ir.Cons
                                      ( apply "List.rev" [ Semantic_ir.Ident "current" ],
                                        Semantic_ir.Ident "groups" ) ) ] ) ) ],
                        apply "partition"
                          [ Semantic_ir.Ident "groups";
                            Semantic_ir.List [ Semantic_ir.Ident "item" ];
                            Semantic_ir.Constructor ("Some", Some (Semantic_ir.Ident "key"));
                            Semantic_ir.Ident "rest" ] )
                  in
                  let partition_body =
                    Semantic_ir.Match
                      ( Semantic_ir.Ident "xs",
                        [ (Semantic_ir.PList [], finish_call);
                          ( Semantic_ir.PCons (Semantic_ir.PVar "item", Semantic_ir.PVar "rest"),
                            Semantic_ir.Let
                              ( [ ( Semantic_ir.PVar "key",
                                    Semantic_ir.Apply (fn.semantic_expr, [ Semantic_ir.Ident "item" ]) ) ],
                                Semantic_ir.Match
                                  ( Semantic_ir.Ident "current_key",
                                    [ ( Semantic_ir.PConstructor ("Some", Some (Semantic_ir.PVar "previous")),
                                        Semantic_ir.If
                                          ( Semantic_ir.Infix
                                              ( "=", Semantic_ir.Ident "previous",
                                                Semantic_ir.Ident "key" ),
                                            apply "partition"
                                              [ Semantic_ir.Ident "groups";
                                                Semantic_ir.Cons
                                                  ( Semantic_ir.Ident "item",
                                                    Semantic_ir.Ident "current" );
                                                Semantic_ir.Ident "current_key";
                                                Semantic_ir.Ident "rest" ],
                                            start_new_group ) );
                                      (Semantic_ir.PAny, start_new_group) ] ) ) ) ] )
                  in
                  let finish_body =
                    Semantic_ir.Match
                      ( Semantic_ir.Ident "current",
                        [ (Semantic_ir.PList [], apply "List.rev" [ Semantic_ir.Ident "groups" ]);
                          ( Semantic_ir.PAny,
                            apply "List.rev"
                              [ Semantic_ir.Cons
                                  ( apply "List.rev" [ Semantic_ir.Ident "current" ],
                                    Semantic_ir.Ident "groups" ) ] ) ] )
                  in
                  Ok
                    (typed_ir (TList (TList inner))
                       (Semantic_ir.LetRecIn
                          ( "finish",
                            [ Semantic_ir.PVar "groups"; Semantic_ir.PVar "current" ],
                            finish_body,
                            Semantic_ir.LetRec
                              ( "partition",
                                [ Semantic_ir.PVar "groups";
                                  Semantic_ir.PVar "current";
                                  Semantic_ir.PVar "current_key";
                                  Semantic_ir.PVar "xs" ],
                                partition_body,
                                [ Semantic_ir.List [];
                                  Semantic_ir.List [];
                                  Semantic_ir.Constructor ("None", None);
                                  list_expr ] ) )))
              | TFn _, Ok _ -> Error.error "partition-by function type does not match collection"
              | _, Ok _ -> Error.error "partition-by expects a function"
              | _, Error _ -> Error.error "partition-by expects a collection"))
      | _ -> Error.error "partition-by expects function and collection"
    
    and compile_run_bang scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match compile_expr scope env collection_form with
          | Error _ as error -> error
          | Ok collection -> (
              match Collection_capability.to_seq_expr env collection with
              | Error _ -> Error.error "run! expects a collection"
              | Ok (inner, sequence) -> (
                  match
                    compile_function_arg_for_collection scope env inner fn_form
                  with
                  | Error _ as error -> error
                  | Ok ({ ty = TFn ([ param_ty ], _); _ } as fn)
                    when Types.assignable ~policy:Host_boundary
                           ~expected:param_ty ~actual:inner ->
                      Ok
                        (typed_ir TUnit
                           (Semantic_ir.Let
                              ( [ ( Semantic_ir.PUnit,
                                    apply "Seq.iter"
                                      [ Semantic_ir.Fun
                                          ( [ Semantic_ir.PVar "item" ],
                                            apply "ignore"
                                              [ Semantic_ir.Apply
                                                  ( fn.semantic_expr,
                                                    [ Semantic_ir.Ident
                                                        "item" ] ) ] );
                                        sequence ] ) ],
                                Semantic_ir.Unit )))
                  | Ok { ty = TFn _; _ } ->
                      Error.error
                        "run! function type does not match collection"
                  | Ok _ -> Error.error "run! expects a function")))
      | _ -> Error.error "run! expects function and collection"
    
    and compile_map_indexed scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok collection -> (
              match (fn.ty, collection_to_list_expr collection) with
              | TFn ([ TInt; item_ty ], ret), Ok (inner, list_expr) when Types.equal item_ty inner ->
                  Ok
                    (typed_ir (TList ret)
                       (apply "List.mapi"
                          [ Semantic_ir.Fun
                              ( [ Semantic_ir.PVar "index"; Semantic_ir.PVar "item" ],
                                Semantic_ir.Apply
                                  ( fn.semantic_expr,
                                    [ Semantic_ir.Ident "index"; Semantic_ir.Ident "item" ] ) );
                            list_expr ]))
              | TFn _, Ok _ -> Error.error "map-indexed function type does not match collection"
              | _, Ok _ -> Error.error "map-indexed expects a function"
              | _, Error _ -> Error.error "map-indexed expects a collection"))
      | _ -> Error.error "map-indexed expects function and collection"
    
    and compile_filterv scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok collection -> (
              match (fn.ty, collection_to_list_expr collection) with
              | TFn ([ param_ty ], TBool), Ok (inner, list_expr) when Types.equal param_ty inner ->
                  Ok
                    (typed_ir (TVector inner)
                       (apply "Rrbvec.of_list"
                          [ apply "List.filter" [ fn.semantic_expr; list_expr ] ]))
              | TFn _, Ok _ ->
                  Error.error "filterv expects a predicate matching collection elements"
              | _, Ok _ -> Error.error "filterv expects a function"
              | _, Error _ -> Error.error "filterv expects a collection"))
      | _ -> Error.error "filterv expects function and collection"
    
    and compile_mapv scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match compile_expr scope env collection_form with
          | Error _ as error -> error
          | Ok collection -> (
              match Collection_capability.to_seq_expr env collection with
              | Error _ -> Error.error "mapv expects a collection"
              | Ok (inner, sequence) -> (
                  match
                    compile_function_arg_for_collection scope env inner fn_form
                  with
                  | Error _ as error -> error
                  | Ok ({ ty = TFn ([ param_ty ], ret); _ } as fn)
                    when Types.assignable ~policy:Host_boundary
                           ~expected:param_ty ~actual:inner ->
                      Ok
                        (typed_ir (TVector ret)
                           (apply "Rrbvec.of_list"
                              [ apply "List.of_seq"
                                  [ apply "Lg_runtime.Runtime_seq.map"
                                      [ fn.semantic_expr; sequence ] ] ]))
                  | Ok { ty = TFn _; _ } ->
                      Error.error
                        "mapv function type does not match collection"
                  | Ok _ -> Error.error "mapv expects a function")))
      | _ -> Error.error "mapv expects function and collection"
    
    and compile_reduce_kv scope env arg_forms =
      match arg_forms with
      | fn_form :: init_form :: collection_form :: [] -> (
          match
            ( compile_expr scope env init_form,
              compile_expr scope env collection_form )
          with
          | (Error _ as error), _ -> error
          | _, (Error _ as error) -> error
          | Ok init, Ok collection ->
              let compile_for key_ty value_ty entries =
                match
                  compile_kv_reducer scope env init.ty key_ty value_ty fn_form
                with
                | Error _ as error -> error
                | Ok fn -> (
                    match fn.ty with
                    | TFn ([ accumulator_ty; actual_key; actual_value ], result)
                      when Types.assignable ~policy:Host_boundary
                             ~expected:accumulator_ty ~actual:init.ty
                           && Types.assignable ~policy:Host_boundary
                                ~expected:actual_key ~actual:key_ty
                           && Types.assignable ~policy:Host_boundary
                                ~expected:actual_value ~actual:value_ty
                           && Types.assignable ~policy:Host_boundary
                                ~expected:init.ty ~actual:result ->
                        Ok
                          (typed_ir init.ty
                             (apply "List.fold_left"
                                [ Semantic_ir.Fun
                                    ( [ Semantic_ir.PVar "accumulator";
                                        Semantic_ir.PTuple
                                          [ Semantic_ir.PVar "key";
                                            Semantic_ir.PVar "value" ] ],
                                      Semantic_ir.Apply
                                        ( fn.semantic_expr,
                                          [ Semantic_ir.Ident "accumulator";
                                            Semantic_ir.Ident "key";
                                            Semantic_ir.Ident "value" ] ) );
                                  init.semantic_expr;
                                  entries ]))
                    | TFn _ ->
                        Error.error
                          "reduce-kv function type does not match collection"
                    | _ -> Error.error "reduce-kv expects a function")
              in
              (match collection.ty with
              | TVector value_ty ->
                  let entries =
                    apply "List.mapi"
                      [ Semantic_ir.Fun
                          ( [ Semantic_ir.PVar "index";
                              Semantic_ir.PVar "value" ],
                            Semantic_ir.Tuple
                              [ Semantic_ir.Ident "index";
                                Semantic_ir.Ident "value" ] );
                        apply "Rrbvec.to_list" [ collection.semantic_expr ] ]
                  in
                  compile_for TInt value_ty entries
              | map_type -> (
                  match Types.dynamic_map_types map_type with
                  | Some (key_ty, value_ty) ->
                      compile_for key_ty value_ty collection.semantic_expr
                  | None ->
                      Error.error "reduce-kv expects a vector or map")))
      | _ -> Error.error "reduce-kv expects function, init, and vector"
    
    and compile_some scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok collection -> (
              match (fn.ty, collection_to_list_expr collection) with
              | TFn ([ param_ty ], return_ty), Ok (inner, list_expr)
                when Types.equal param_ty inner ->
                  let result_ty, present_result =
                    match return_ty with
                    | TOcaml_app ("option", [ _ ]) | TOcaml "option" ->
                        (return_ty, Semantic_ir.Ident "result")
                    | _ ->
                        ( TOcaml_app ("option", [ return_ty ]),
                          Semantic_ir.Constructor
                            ("Some", Some (Semantic_ir.Ident "result")) )
                  in
                  let recurse =
                    apply "find_truthy" [ Semantic_ir.Ident "rest" ]
                  in
                  let body =
                    Semantic_ir.Match
                      ( Semantic_ir.Ident "values",
                        [ ( Semantic_ir.PList [],
                            Semantic_ir.Constructor ("None", None) );
                          ( Semantic_ir.PCons
                              (Semantic_ir.PVar "item", Semantic_ir.PVar "rest"),
                            Semantic_ir.Let
                              ( [ ( Semantic_ir.PVar "result",
                                    Semantic_ir.Apply
                                      ( fn.semantic_expr,
                                        [ Semantic_ir.Ident "item" ] ) ) ],
                                Semantic_ir.If
                                  ( truthiness_expression return_ty
                                      (Semantic_ir.Ident "result"),
                                    present_result,
                                    recurse ) ) );
                        ] )
                  in
                  Ok
                    (typed_ir result_ty
                       (Semantic_ir.LetRecIn
                          ( "find_truthy",
                            [ Semantic_ir.PVar "values" ],
                            body,
                            Semantic_ir.Apply
                              (Semantic_ir.Ident "find_truthy", [ list_expr ]) )))
              | TFn _, Ok _ -> Error.error "some function type must match collection elements"
              | _, Ok _ -> Error.error "some expects a function"
              | _, Error _ -> Error.error "some expects a collection"))
      | _ -> Error.error "some expects function and collection"
    
    and compile_sequence_bool_predicate scope env name arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match compile_expr scope env collection_form with
          | Error _ as error -> error
          | Ok collection -> (
              match Collection_capability.to_seq_expr env collection with
              | Error _ -> Error.error (name ^ " expects a seqable value")
              | Ok (inner, sequence) -> (
                  match
                    compile_function_arg_for_collection scope env inner fn_form
                  with
                  | Error _ as error -> error
                  | Ok ({ ty = TFn ([ param_ty ], TBool); _ } as predicate)
                    when Types.assignable ~policy:Host_boundary
                           ~expected:param_ty ~actual:inner ->
                      let predicate_expr =
                        constrain_record_function_argument_expr predicate inner
                      in
                      let predicate_expr =
                        if name = "not-any?" then
                          Semantic_ir.Fun
                            ( [ Semantic_ir.PVar "item" ],
                              Semantic_ir.Prefix
                                ( "not",
                                  Semantic_ir.Apply
                                    ( predicate_expr,
                                      [ Semantic_ir.Ident "item" ] ) ) )
                        else predicate_expr
                      in
                      let result =
                        apply "Lg_runtime.Runtime_seq.for_all"
                          [ predicate_expr; sequence ]
                      in
                      let result =
                        if name = "not-every?" then
                          Semantic_ir.Prefix ("not", result)
                        else result
                      in
                      Ok (typed_ir TBool result)
                  | Ok { ty = TFn _; _ } ->
                      let collection_name =
                        match collection.ty with
                        | TList _ -> "list"
                        | TVector _ -> "vector"
                        | TSet _ -> "set"
                        | _ -> "sequence"
                      in
                      Error.error
                        (name ^ " expects a predicate matching "
                       ^ collection_name ^ " elements")
                  | Ok _ -> Error.error (name ^ " expects a function"))))
      | _ -> Error.error (name ^ " expects function and collection")
    
    and compile_map_call scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match compile_expr scope env collection_form with
          | Error _ as err -> err
          | Ok collection -> (
              match Collection_capability.to_seq_expr env collection with
              | Error _ ->
                  Error.error
                    ("map expects a seqable value, got "
                    ^ Types.source_name collection.ty)
              | Ok (inner, sequence) -> (
                  match
                    compile_function_arg_for_collection scope env inner fn_form
                  with
                  | Error _ as err -> err
                  | Ok ({ ty = TFn ([ param_ty ], ret); _ } as fn)
                    when Types.assignable ~policy:Host_boundary ~expected:param_ty
                           ~actual:inner ->
                      Ok
                        (typed_ir (TSeq ret)
                           (apply "Lg_runtime.Runtime_seq.map"
                              [ fn.semantic_expr; sequence ]))
                  | Ok { ty = TFn _; _ } ->
                      Error.error "map function argument type does not match sequence"
                  | Ok _ -> Error.error "map expects a function")))
      | _ -> Error.error "map expects function and collection"
    
    and compile_filter scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match compile_expr scope env collection_form with
          | Error _ as err -> err
          | Ok collection -> (
              match Collection_capability.to_seq_expr env collection with
              | Error _ -> Error.error "filter expects a seqable value"
              | Ok (inner, sequence) -> (
                  match
                    compile_function_arg_for_collection scope env inner fn_form
                  with
                  | Error _ as err -> err
                  | Ok fn -> (
                      match fn.ty with
                      | TFn ([ param_ty ], TBool)
                        when Types.assignable ~policy:Host_boundary
                               ~expected:param_ty ~actual:inner ->
                          Ok
                            (typed_ir (TSeq inner)
                               (apply "Lg_runtime.Runtime_seq.filter"
                                  [ fn.semantic_expr; sequence ]))
                      | TFn _ ->
                          Error.error
                            "filter expects a predicate matching sequence elements"
                      | _ -> Error.error "filter expects a function"))))
      | _ -> Error.error "filter expects function and collection"
    
    and compile_reduce scope env arg_forms =
      match arg_forms with
      | fn_form :: init_form :: collection_form :: [] -> (
          match (compile_expr scope env init_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok init, Ok collection -> (
              match Collection_capability.to_seq_expr env collection with
              | Error _ -> Error.error "reduce expects a seqable value"
              | Ok (inner, sequence) -> (
                  match compile_reducer scope env init.ty inner fn_form with
                  | Error _ as err -> err
                  | Ok fn -> (
                      match fn.ty with
                      | TFn ([ acc_ty; item_ty ], TNullable reduced_type)
                        when Types.equal init.ty TNil
                             && Types.equal acc_ty TNil
                             && (Types.equal item_ty inner
                                || Types.equal inner TUnknown
                                || Types.assignable ~policy:Host_boundary
                                     ~expected:item_ty ~actual:inner) -> (
                          match Types.reduced_element reduced_type with
                          | None ->
                              Error.error
                                "nullable reduce result must contain a reduced value"
                          | Some result_type ->
                              let accumulator = Semantic_ir.Ident "accumulator" in
                              let item = Semantic_ir.Ident "item" in
                              let reduced_value = "reduced_value" in
                              let nullable_result = TNullable result_type in
                              let adapted_fn =
                                typed_ir
                                  (TFn
                                     ( [ nullable_result; item_ty ],
                                       Types.reduced nullable_result ))
                                  (Semantic_ir.Fun
                                     ( [ Semantic_ir.PVar "accumulator";
                                         Semantic_ir.PVar "item" ],
                                       Semantic_ir.Match
                                         ( Semantic_ir.Apply
                                             ( fn.semantic_expr,
                                               [ accumulator; item ] ),
                                           [ ( Semantic_ir.PConstructor
                                                 ("None", None),
                                               Semantic_ir.Apply
                                                 ( Semantic_ir.Ident
                                                     "Lg_runtime.Runtime_reduced.continue",
                                                   [ Semantic_ir.Constructor
                                                       ("None", None) ] ) );
                                             ( Semantic_ir.PConstructor
                                                 ( "Some",
                                                   Some
                                                     (Semantic_ir.PVar
                                                        reduced_value) ),
                                               Semantic_ir.Apply
                                                 ( Semantic_ir.Ident
                                                     "Lg_runtime.Runtime_reduced.reduced",
                                                   [ Semantic_ir.Constructor
                                                       ( "Some",
                                                         Some
                                                           (Semantic_ir.Apply
                                                              ( Semantic_ir.Ident
                                                                  "Lg_runtime.Runtime_reduced.unreduced",
                                                                [ Semantic_ir.Ident
                                                                    reduced_value ] )) ) ] ) ) ] ) ))
                              in
                              Ok
                                (typed_ir nullable_result
                                   (Collection_capability.reduce_expr env
                                      ~short_circuit:true adapted_fn
                                      { init with ty = nullable_result }
                                      collection sequence)))
                      | TFn ([ acc_ty; item_ty ], ret)
                        when Expression_support.branch_types_compatible acc_ty
                               init.ty
                             && (Types.equal item_ty inner
                                || Types.equal inner TUnknown
                                || Types.assignable ~policy:Host_boundary
                                     ~expected:item_ty ~actual:inner)
                             && Expression_support.branch_types_compatible ret
                                  init.ty
                             && Option.is_none (Types.reduced_element ret) ->
                          Ok
                            (typed_ir init.ty
                               (Collection_capability.reduce_expr env fn init
                                  collection sequence))
                      | TFn ([ acc_ty; item_ty ], ret)
                        when Expression_support.branch_types_compatible acc_ty
                               init.ty
                             && (Types.equal item_ty inner
                                || Types.equal inner TUnknown
                                || Types.assignable ~policy:Host_boundary
                                     ~expected:item_ty ~actual:inner)
                             && (match Types.reduced_element ret with
                                | Some (TNullable _) -> false
                                | Some reduced_ty ->
                                    Expression_support.branch_types_compatible
                                      reduced_ty init.ty
                                | None -> false) ->
                          Ok
                            (typed_ir init.ty
                               (Collection_capability.reduce_expr env
                                  ~short_circuit:true fn init collection sequence))
                      | TFn ([ acc_ty; item_ty ], ret)
                        when Expression_support.branch_types_compatible acc_ty
                               init.ty
                             && (Types.equal item_ty inner
                                || Types.equal inner TUnknown
                                || Types.assignable ~policy:Host_boundary
                                     ~expected:item_ty ~actual:inner) -> (
                          match Types.reduced_element ret with
                          | Some (TNullable result_ty)
                            when Expression_support.branch_types_compatible
                                   result_ty init.ty ->
                              let nullable_init = TNullable init.ty in
                              let accumulator = Semantic_ir.Ident "accumulator" in
                              let item = Semantic_ir.Ident "item" in
                              let value = Semantic_ir.Ident "value" in
                              let adapted_fn =
                                typed_ir
                                  (TFn
                                     ( [ nullable_init; item_ty ],
                                       Types.reduced nullable_init ))
                                  (Semantic_ir.Fun
                                     ( [ Semantic_ir.PVar "accumulator";
                                         Semantic_ir.PVar "item" ],
                                       Semantic_ir.Match
                                         ( accumulator,
                                           [ ( Semantic_ir.PConstructor
                                                 ("None", None),
                                               Semantic_ir.Apply
                                                 ( Semantic_ir.Ident
                                                     "Lg_runtime.Runtime_reduced.reduced",
                                                   [ Semantic_ir.Constructor
                                                       ("None", None) ] ) );
                                             ( Semantic_ir.PConstructor
                                                 ( "Some",
                                                   Some
                                                     (Semantic_ir.PVar "value") ),
                                               Semantic_ir.Apply
                                                 ( fn.semantic_expr,
                                                   [ value; item ] ) );
                                           ] ) ))
                              in
                              let nullable_init_expr =
                                {
                                  init with
                                  ty = nullable_init;
                                  semantic_expr =
                                    Semantic_ir.Constructor
                                      ("Some", Some init.semantic_expr);
                                }
                              in
                              Ok
                                (typed_ir nullable_init
                                   (Collection_capability.reduce_expr env
                                      ~short_circuit:true adapted_fn
                                      nullable_init_expr collection sequence))
                          | _ -> Error.error "reduced value must match init")
                      | TFn _ ->
                          Error.error
                            ("reduce function type does not match init and sequence: fn="
                           ^ Types.source_name fn.ty ^ ", init="
                           ^ Types.source_name init.ty ^ ", sequence="
                           ^ Types.source_name inner)
                      | _ -> Error.error "reduce expects a function"))))
      | _ -> Error.error "reduce expects function, init, and collection"
    
  in
  { compile_sort_by; compile_mapcat; compile_repeatedly; compile_reductions; compile_split_with; compile_partition_by; compile_run_bang; compile_map_indexed; compile_filterv; compile_mapv; compile_reduce_kv; compile_some; compile_sequence_bool_predicate; compile_map_call; compile_filter; compile_reduce }
