open Ast
open Types
open Expression_support

module Env = Compiler_environment

let compile_args_for compile_expr scope env arg_forms =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | form :: rest -> (
        match compile_expr scope env form with
        | Ok expr -> loop (expr :: acc) rest
        | Error _ as err -> err)
  in
  loop [] arg_forms

let make compile_expr =
  let compile_args_for = compile_args_for compile_expr in
  let compile_body = Special_form_elaborator.compile_body ~compile_expr in
  let compile_function_arg scope env = function
    | FSymbol name -> lookup_function scope env name
    | form -> compile_expr scope env form
  in
  let compile_function_arg_for_collection scope env element_ty = function
    | FList (FSymbol "fn" :: FVector [ FSymbol name ] :: body_forms) ->
        let binding = Types.binding (Names.sanitize_name name) element_ty in
        let function_env = Env.add (Names.scoped_key scope name) binding env in
        compile_body scope function_env "function body requires at least one form"
          body_forms
        |> Result.map (fun body ->
               let pattern =
                 match element_ty with
                 | TNamed_record record ->
                     Ocaml_ir.PConstraint
                       (Ocaml_ir.PVar binding.ocaml_name, record.type_name)
                 | _ -> Ocaml_ir.PVar binding.ocaml_name
               in
               typed_ir (TFn ([ element_ty ], body.ty))
                 (Ocaml_ir.Fun ([ pattern ], body.ocaml_expr)))
    | form -> compile_function_arg scope env form
  in
    let rec collection_to_list_expr collection =
      Core_sequence_transform.collection_to_list_expr collection
    
    and collection_from_list_expr collection_ty list_expr =
      Core_sequence_transform.collection_from_list_expr collection_ty list_expr
    
    and comparable_type = function
      | TInt | TString | TSymbol | TKeyword | TBool | TAny -> true
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
                          [ Ocaml_ir.Fun
                              ( [ Ocaml_ir.PVar "left"; Ocaml_ir.PVar "right" ],
                                apply "Stdlib.compare"
                                  [ Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "left" ]);
                                    Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "right" ]) ] );
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
          match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok collection -> (
              match (fn.ty, collection_to_list_expr collection) with
              | TFn ([ param_ty ], TList ret_inner), Ok (inner, list_expr)
                when Types.equal param_ty inner ->
                  Ok
                    (typed_ir (TList ret_inner)
                       (apply "List.concat"
                          [ apply "List.map" [ fn.ocaml_expr; list_expr ] ]))
              | TFn ([ param_ty ], TVector ret_inner), Ok (inner, list_expr)
                when Types.equal param_ty inner ->
                  Ok
                    (typed_ir (TList ret_inner)
                       (apply "List.concat"
                          [ apply "List.map"
                              [ Ocaml_ir.Fun
                                  ( [ Ocaml_ir.PVar "item" ],
                                    apply "Rrbvec.to_list"
                                      [ Ocaml_ir.Apply
                                          (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]) ] );
                                list_expr ] ]))
              | TFn ([ param_ty ], TSet ret_inner), Ok (inner, list_expr)
                when Types.equal param_ty inner -> (
                  match Types.set_module_name ret_inner with
                  | Error _ as err -> err
                  | Ok set_module ->
                      Ok
                        (typed_ir (TList ret_inner)
                           (apply "List.concat"
                              [ apply "List.map"
                                  [ Ocaml_ir.Fun
                                      ( [ Ocaml_ir.PVar "item" ],
                                        apply (set_module ^ ".elements")
                                          [ Ocaml_ir.Apply
                                              (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]) ] );
                                    list_expr ] ])))
              | TFn ([ param_ty ], _), Ok (inner, _) when not (Types.equal param_ty inner) ->
                  Error.error "mapcat function argument type does not match collection"
              | TFn _, Ok _ -> Error.error "mapcat function must return a collection"
              | _, Ok _ -> Error.error "mapcat expects a function"
              | _, Error _ -> Error.error "mapcat expects a collection"))
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
                      Ocaml_ir.If
                        ( Ocaml_ir.Infix ("<=", Ocaml_ir.Ident "n", Ocaml_ir.Int 0),
                          Ocaml_ir.Ident "acc",
                          apply "repeatedly"
                            [ Ocaml_ir.Cons
                                ( Ocaml_ir.Apply (fn.ocaml_expr, []),
                                  Ocaml_ir.Ident "acc" );
                              Ocaml_ir.Infix ("-", Ocaml_ir.Ident "n", Ocaml_ir.Int 1) ] )
                    in
                    Ok
                      (typed_ir (TList ret)
                         (Ocaml_ir.LetRec
                            ( "repeatedly",
                              [ Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "n" ],
                              body,
                              [ Ocaml_ir.List []; count.ocaml_expr ] )))
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
                    Ocaml_ir.Match
                      ( Ocaml_ir.Ident "xs",
                        [ (Ocaml_ir.PList [], apply "List.rev" [ Ocaml_ir.Ident "acc" ]);
                          ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "tail"),
                            Ocaml_ir.Let
                              ( [ ( Ocaml_ir.PVar "next",
                                    Ocaml_ir.Apply
                                      ( fn.ocaml_expr,
                                        [ Ocaml_ir.Ident "current"; Ocaml_ir.Ident "item" ] ) ) ],
                                apply "reductions"
                                  [ Ocaml_ir.Ident "next";
                                    Ocaml_ir.Cons
                                      (Ocaml_ir.Ident "next", Ocaml_ir.Ident "acc");
                                    Ocaml_ir.Ident "tail" ] ) ) ] )
                  in
                  Ok
                    (typed_ir (TList inner)
                       (Ocaml_ir.Match
                          ( list_expr,
                            [ (Ocaml_ir.PList [], Ocaml_ir.List []);
                              ( Ocaml_ir.PCons (Ocaml_ir.PVar "first", Ocaml_ir.PVar "rest"),
                                Ocaml_ir.LetRec
                                  ( "reductions",
                                    [ Ocaml_ir.PVar "current";
                                      Ocaml_ir.PVar "acc";
                                      Ocaml_ir.PVar "xs" ],
                                    reductions_body,
                                    [ Ocaml_ir.Ident "first";
                                      Ocaml_ir.List [ Ocaml_ir.Ident "first" ];
                                      Ocaml_ir.Ident "rest" ] ) ) ] )))
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
                    Ocaml_ir.Match
                      ( Ocaml_ir.Ident "xs",
                        [ (Ocaml_ir.PList [], apply "List.rev" [ Ocaml_ir.Ident "acc" ]);
                          ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "rest"),
                            Ocaml_ir.Let
                              ( [ ( Ocaml_ir.PVar "next",
                                    Ocaml_ir.Apply
                                      ( fn.ocaml_expr,
                                        [ Ocaml_ir.Ident "current"; Ocaml_ir.Ident "item" ] ) ) ],
                                apply "reductions"
                                  [ Ocaml_ir.Ident "next";
                                    Ocaml_ir.Cons
                                      (Ocaml_ir.Ident "next", Ocaml_ir.Ident "acc");
                                    Ocaml_ir.Ident "rest" ] ) ) ] )
                  in
                  Ok
                    (typed_ir (TList init.ty)
                       (Ocaml_ir.LetRec
                          ( "reductions",
                            [ Ocaml_ir.PVar "current";
                              Ocaml_ir.PVar "acc";
                              Ocaml_ir.PVar "xs" ],
                            reductions_body,
                            [ init.ocaml_expr;
                              Ocaml_ir.List [ init.ocaml_expr ];
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
              | TFn ([ param_ty ], TBool), Ok (inner, list_expr) when Types.equal param_ty inner ->
                  let split_body =
                    Ocaml_ir.Match
                      ( Ocaml_ir.Ident "rest",
                        [ ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "tail"),
                            Ocaml_ir.If
                              ( Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]),
                                apply "split"
                                  [ Ocaml_ir.Cons
                                      (Ocaml_ir.Ident "item", Ocaml_ir.Ident "prefix");
                                    Ocaml_ir.Ident "tail" ],
                                Ocaml_ir.Tuple
                                  [ apply "List.rev" [ Ocaml_ir.Ident "prefix" ];
                                    Ocaml_ir.Ident "rest" ] ) );
                          ( Ocaml_ir.PAny,
                            Ocaml_ir.Tuple
                              [ apply "List.rev" [ Ocaml_ir.Ident "prefix" ];
                                Ocaml_ir.Ident "rest" ] ) ] )
                  in
                  let pair_expr =
                    Ocaml_ir.LetRec
                      ( "split",
                        [ Ocaml_ir.PVar "prefix"; Ocaml_ir.PVar "rest" ],
                        split_body,
                        [ Ocaml_ir.List []; list_expr ] )
                  in
                  Ok
                    (typed_ir (TVector collection.ty)
                       (Ocaml_ir.Let
                          ( [ (Ocaml_ir.PVar "pair", pair_expr) ],
                            apply "Rrbvec.of_list"
                              [ Ocaml_ir.List
                                  [ collection_from_list_expr collection.ty
                                      (apply "fst" [ Ocaml_ir.Ident "pair" ]);
                                    collection_from_list_expr collection.ty
                                      (apply "snd" [ Ocaml_ir.Ident "pair" ]) ] ] )))
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
                    apply "finish" [ Ocaml_ir.Ident "groups"; Ocaml_ir.Ident "current" ]
                  in
                  let start_new_group =
                    Ocaml_ir.Let
                      ( [ ( Ocaml_ir.PVar "groups",
                            Ocaml_ir.Match
                              ( Ocaml_ir.Ident "current",
                                [ (Ocaml_ir.PList [], Ocaml_ir.Ident "groups");
                                  ( Ocaml_ir.PAny,
                                    Ocaml_ir.Cons
                                      ( apply "List.rev" [ Ocaml_ir.Ident "current" ],
                                        Ocaml_ir.Ident "groups" ) ) ] ) ) ],
                        apply "partition"
                          [ Ocaml_ir.Ident "groups";
                            Ocaml_ir.List [ Ocaml_ir.Ident "item" ];
                            Ocaml_ir.Constructor ("Some", Some (Ocaml_ir.Ident "key"));
                            Ocaml_ir.Ident "rest" ] )
                  in
                  let partition_body =
                    Ocaml_ir.Match
                      ( Ocaml_ir.Ident "xs",
                        [ (Ocaml_ir.PList [], finish_call);
                          ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "rest"),
                            Ocaml_ir.Let
                              ( [ ( Ocaml_ir.PVar "key",
                                    Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]) ) ],
                                Ocaml_ir.Match
                                  ( Ocaml_ir.Ident "current_key",
                                    [ ( Ocaml_ir.PConstructor ("Some", Some (Ocaml_ir.PVar "previous")),
                                        Ocaml_ir.If
                                          ( Ocaml_ir.Infix
                                              ( "=", Ocaml_ir.Ident "previous",
                                                Ocaml_ir.Ident "key" ),
                                            apply "partition"
                                              [ Ocaml_ir.Ident "groups";
                                                Ocaml_ir.Cons
                                                  ( Ocaml_ir.Ident "item",
                                                    Ocaml_ir.Ident "current" );
                                                Ocaml_ir.Ident "current_key";
                                                Ocaml_ir.Ident "rest" ],
                                            start_new_group ) );
                                      (Ocaml_ir.PAny, start_new_group) ] ) ) ) ] )
                  in
                  let finish_body =
                    Ocaml_ir.Match
                      ( Ocaml_ir.Ident "current",
                        [ (Ocaml_ir.PList [], apply "List.rev" [ Ocaml_ir.Ident "groups" ]);
                          ( Ocaml_ir.PAny,
                            apply "List.rev"
                              [ Ocaml_ir.Cons
                                  ( apply "List.rev" [ Ocaml_ir.Ident "current" ],
                                    Ocaml_ir.Ident "groups" ) ] ) ] )
                  in
                  Ok
                    (typed_ir (TList (TList inner))
                       (Ocaml_ir.LetRecIn
                          ( "finish",
                            [ Ocaml_ir.PVar "groups"; Ocaml_ir.PVar "current" ],
                            finish_body,
                            Ocaml_ir.LetRec
                              ( "partition",
                                [ Ocaml_ir.PVar "groups";
                                  Ocaml_ir.PVar "current";
                                  Ocaml_ir.PVar "current_key";
                                  Ocaml_ir.PVar "xs" ],
                                partition_body,
                                [ Ocaml_ir.List [];
                                  Ocaml_ir.List [];
                                  Ocaml_ir.Constructor ("None", None);
                                  list_expr ] ) )))
              | TFn _, Ok _ -> Error.error "partition-by function type does not match collection"
              | _, Ok _ -> Error.error "partition-by expects a function"
              | _, Error _ -> Error.error "partition-by expects a collection"))
      | _ -> Error.error "partition-by expects function and collection"
    
    and compile_run_bang scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok collection -> (
              match (fn.ty, collection_to_list_expr collection) with
              | TFn ([ param_ty ], _ret), Ok (inner, list_expr) when Types.equal param_ty inner ->
                  Ok
                    (typed_ir TUnit
                       (Ocaml_ir.Let
                          ( [ ( Ocaml_ir.PUnit,
                                apply "List.iter"
                                  [ Ocaml_ir.Fun
                                      ( [ Ocaml_ir.PVar "item" ],
                                        apply "ignore"
                                          [ Ocaml_ir.Apply
                                              (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]) ] );
                                    list_expr ] ) ],
                            Ocaml_ir.Unit )))
              | TFn _, Ok _ -> Error.error "run! function type does not match collection"
              | _, Ok _ -> Error.error "run! expects a function"
              | _, Error _ -> Error.error "run! expects a collection"))
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
                          [ Ocaml_ir.Fun
                              ( [ Ocaml_ir.PVar "index"; Ocaml_ir.PVar "item" ],
                                Ocaml_ir.Apply
                                  ( fn.ocaml_expr,
                                    [ Ocaml_ir.Ident "index"; Ocaml_ir.Ident "item" ] ) );
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
                          [ apply "List.filter" [ fn.ocaml_expr; list_expr ] ]))
              | TFn _, Ok _ ->
                  Error.error "filterv expects a predicate matching collection elements"
              | _, Ok _ -> Error.error "filterv expects a function"
              | _, Error _ -> Error.error "filterv expects a collection"))
      | _ -> Error.error "filterv expects function and collection"
    
    and compile_mapv scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok collection -> (
              match (fn.ty, collection_to_list_expr collection) with
              | TFn ([ param_ty ], ret), Ok (inner, list_expr) when Types.equal param_ty inner ->
                  Ok
                    (typed_ir (TVector ret)
                       (apply "Rrbvec.of_list"
                          [ apply "List.map" [ fn.ocaml_expr; list_expr ] ]))
              | TFn _, Ok _ -> Error.error "mapv function type does not match collection"
              | _, Ok _ -> Error.error "mapv expects a function"
              | _, Error _ -> Error.error "mapv expects a collection"))
      | _ -> Error.error "mapv expects function and collection"
    
    and compile_reduce_kv scope env arg_forms =
      match arg_forms with
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
              match (fn.ty, collection.ty) with
              | TFn ([ acc_ty; TInt; item_ty ], ret), TVector inner
                when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
                  Ok
                    (typed_ir init.ty
                       (apply "List.fold_left"
                          [ Ocaml_ir.Fun
                              ( [ Ocaml_ir.PVar "acc";
                                  Ocaml_ir.PTuple
                                    [ Ocaml_ir.PVar "index"; Ocaml_ir.PVar "item" ] ],
                                Ocaml_ir.Apply
                                  ( fn.ocaml_expr,
                                    [ Ocaml_ir.Ident "acc";
                                      Ocaml_ir.Ident "index";
                                      Ocaml_ir.Ident "item" ] ) );
                            init.ocaml_expr;
                            apply "List.mapi"
                              [ Ocaml_ir.Fun
                                  ( [ Ocaml_ir.PVar "index"; Ocaml_ir.PVar "item" ],
                                    Ocaml_ir.Tuple
                                      [ Ocaml_ir.Ident "index"; Ocaml_ir.Ident "item" ] );
                                apply "Rrbvec.to_list" [ collection.ocaml_expr ] ] ]))
              | TFn _, TVector _ -> Error.error "reduce-kv function type does not match vector"
              | _, TVector _ -> Error.error "reduce-kv expects a function"
              | _ -> Error.error "reduce-kv expects a vector"))
      | _ -> Error.error "reduce-kv expects function, init, and vector"
    
    and compile_some scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok collection -> (
              match (fn.ty, collection_to_list_expr collection) with
              | TFn ([ param_ty ], TBool), Ok (inner, list_expr) when Types.equal param_ty inner ->
                  Ok (typed_ir TBool (apply "List.exists" [ fn.ocaml_expr; list_expr ]))
              | TFn _, Ok _ -> Error.error "some expects a predicate matching collection elements"
              | _, Ok _ -> Error.error "some expects a function"
              | _, Error _ -> Error.error "some expects a collection"))
      | _ -> Error.error "some expects function and collection"
    
    and compile_sequence_bool_predicate scope env name arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok collection -> (
              let build all_expr =
                match name with
                | "every?" -> all_expr
                | "not-any?" -> all_expr
                | "not-every?" -> Ocaml_ir.Prefix ("not", all_expr)
                | _ -> all_expr
              in
              let predicate_expr =
                match name with
                | "not-any?" ->
                    Ocaml_ir.Fun
                      ( [ Ocaml_ir.PVar "item" ],
                        Ocaml_ir.Prefix
                          ("not", Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ])) )
                | _ -> fn.ocaml_expr
              in
              match (fn.ty, collection.ty) with
              | TFn ([ param_ty ], TBool), TList inner when Types.equal param_ty inner ->
                  let all_expr = apply "List.for_all" [ predicate_expr; collection.ocaml_expr ] in
                  Ok (typed_ir TBool (build all_expr))
              | TFn _, TList _ -> Error.error (name ^ " expects a predicate matching list elements")
              | _, TList _ -> Error.error (name ^ " expects a function")
              | TFn ([ param_ty ], TBool), TVector inner when Types.equal param_ty inner ->
                  let all_expr = apply "Rrbvec.for_all" [ predicate_expr; collection.ocaml_expr ] in
                  Ok (typed_ir TBool (build all_expr))
              | TFn _, TVector _ ->
                  Error.error (name ^ " expects a predicate matching vector elements")
              | _, TVector _ -> Error.error (name ^ " expects a function")
              | TFn ([ param_ty ], TBool), TSet inner
                when Types.compatible ~expected:param_ty ~actual:inner ->
                  Types.set_module_name inner
                  |> Result.map (fun set_module ->
                         let fn_expr = constrain_record_function_argument_expr fn inner in
                         let predicate_expr =
                           match name with
                           | "not-any?" ->
                               Ocaml_ir.Fun
                                 ( [ Ocaml_ir.PVar "item" ],
                                   Ocaml_ir.Prefix
                                     ( "not",
                                       Ocaml_ir.Apply (fn_expr, [ Ocaml_ir.Ident "item" ]) ) )
                           | _ -> fn_expr
                         in
                         let all_expr =
                           apply "List.for_all"
                             [ predicate_expr;
                               apply (set_module ^ ".elements") [ collection.ocaml_expr ] ]
                         in
                         typed_ir TBool (build all_expr))
              | TFn _, TSet _ -> Error.error (name ^ " expects a predicate matching set elements")
              | _, TSet _ -> Error.error (name ^ " expects a function")
              | _ -> Error.error (name ^ " expects a list, vector, or set")))
      | _ -> Error.error (name ^ " expects function and collection")
    
    and compile_map_call scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match compile_expr scope env collection_form with
          | Error _ as err -> err
          | Ok collection ->
              let fn =
                match collection.ty with
                | TList inner | TVector inner | TSet inner ->
                    compile_function_arg_for_collection scope env inner fn_form
                | _ -> compile_function_arg scope env fn_form
              in
              (match fn with
              | Error _ as err -> err
              | Ok fn -> (
              match (fn.ty, collection.ty) with
              | TFn ([ param_ty ], ret), TList inner when Types.equal param_ty inner ->
                  Ok
                    (typed_ir (TList ret)
                       (apply "List.map" [ fn.ocaml_expr; collection.ocaml_expr ]))
              | TFn _, TList _ -> Error.error "map function argument type does not match list"
              | _, TList _ -> Error.error "map expects a function"
              | TFn ([ param_ty ], ret), TVector inner when Types.equal param_ty inner ->
                  Ok
                    (typed_ir (TVector ret)
                       (apply "Rrbvec.map" [ fn.ocaml_expr; collection.ocaml_expr ]))
              | TFn _, TVector _ -> Error.error "map function argument type does not match vector"
              | _, TVector _ -> Error.error "map expects a function"
              | TFn ([ param_ty ], ret), TSet inner
                when Types.compatible ~expected:param_ty ~actual:inner ->
                  Result.bind (Types.set_module_name ret) (fun result_module ->
                      Types.set_module_name inner
                      |> Result.map (fun source_module ->
                             let fn_expr = constrain_record_function_argument_expr fn inner in
                             typed_ir (TSet ret)
                               (apply (result_module ^ ".of_list")
                                  [ apply "List.map"
                                      [ fn_expr;
                                        apply (source_module ^ ".elements")
                                          [ collection.ocaml_expr ] ] ])))
              | TFn _, TSet _ -> Error.error "map function argument type does not match set"
              | _, TSet _ -> Error.error "map expects a function"
              | _ -> Error.error "map expects a list, vector, or set")))
      | _ -> Error.error "map expects function and collection"
    
    and compile_filter scope env arg_forms =
      match arg_forms with
      | fn_form :: collection_form :: [] -> (
          match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok collection -> (
              match (fn.ty, collection.ty) with
              | TFn ([ param_ty ], TBool), TList inner when Types.equal param_ty inner ->
                  Ok
                    (typed_ir collection.ty
                       (apply "List.filter" [ fn.ocaml_expr; collection.ocaml_expr ]))
              | TFn _, TList _ -> Error.error "filter expects a predicate matching list elements"
              | _, TList _ -> Error.error "filter expects a function"
              | TFn ([ param_ty ], TBool), TVector inner when Types.equal param_ty inner ->
                  Ok
                    (typed_ir collection.ty
                       (apply "Rrbvec.filter" [ fn.ocaml_expr; collection.ocaml_expr ]))
              | TFn _, TVector _ -> Error.error "filter expects a predicate matching vector elements"
              | _, TVector _ -> Error.error "filter expects a function"
              | TFn ([ param_ty ], TBool), TSet inner
                when Types.compatible ~expected:param_ty ~actual:inner ->
                  Types.set_module_name inner
                  |> Result.map (fun set_module ->
                         let fn_expr = constrain_record_function_argument_expr fn inner in
                         typed_ir collection.ty
                           (apply (set_module ^ ".of_list")
                              [ apply "List.filter"
                                  [ fn_expr;
                                    apply (set_module ^ ".elements")
                                      [ collection.ocaml_expr ] ] ]))
              | TFn _, TSet _ -> Error.error "filter expects a predicate matching set elements"
              | _, TSet _ -> Error.error "filter expects a function"
              | _ -> Error.error "filter expects a list, vector, or set"))
      | _ -> Error.error "filter expects function and collection"
    
    and compile_reduce scope env arg_forms =
      match arg_forms with
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
              match (fn.ty, collection.ty) with
              | TFn ([ acc_ty; item_ty ], ret), TList inner
                when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
                  Ok
                    (typed_ir init.ty
                       (apply "List.fold_left"
                          [ fn.ocaml_expr; init.ocaml_expr; collection.ocaml_expr ]))
              | TFn _, TList _ -> Error.error "reduce function type does not match init and list"
              | _, TList _ -> Error.error "reduce expects a function"
              | TFn ([ acc_ty; item_ty ], ret), TVector inner
                when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
                 Ok
                    (typed_ir init.ty
                       (apply "Rrbvec.fold_left"
                          [ fn.ocaml_expr; init.ocaml_expr; collection.ocaml_expr ]))
              | TFn _, TVector _ -> Error.error "reduce function type does not match init and vector"
              | _, TVector _ -> Error.error "reduce expects a function"
              | TFn ([ acc_ty; item_ty ], ret), TSet inner
                when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
                  Types.set_module_name inner
                  |> Result.map (fun set_module ->
                         typed_ir init.ty
                           (apply "List.fold_left"
                              [ fn.ocaml_expr;
                                init.ocaml_expr;
                                apply (set_module ^ ".elements")
                                  [ collection.ocaml_expr ] ]))
              | TFn _, TSet _ -> Error.error "reduce function type does not match init and set"
              | _, TSet _ -> Error.error "reduce expects a function"
              | _ -> Error.error "reduce expects a list, vector, or set"))
      | _ -> Error.error "reduce expects function, init, and collection"
    
    and compile_apply scope env arg_forms =
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
                ( compile_function_arg scope env fn_form,
                  compile_args_for scope env fixed_forms,
                  compile_expr scope env collection_form )
              with
              | (Error _ as err), _, _ -> err
              | _, (Error _ as err), _ -> err
              | _, _, (Error _ as err) -> err
              | Ok fn, Ok fixed_args, Ok collection -> (
                  match collection_to_list_expr collection with
                  | Error _ -> Error.error "apply expects a list, vector, or set"
                  | Ok (inner, list_expr) -> (
                      match fn.ty with
                      | TFn ([ TInt; TInt ], TInt)
                        when Types.equal inner TInt
                             && List.for_all (fun arg -> Types.equal arg.ty TInt) fixed_args ->
                          let values_expr =
                            match fixed_args with
                            | [] -> list_expr
                            | _ ->
                                Ocaml_ir.Infix
                                  ( "@",
                                    Ocaml_ir.List
                                      (List.map (fun arg -> arg.ocaml_expr) fixed_args),
                                    list_expr )
                          in
                          Ok
                            (typed_ir TInt
                               (apply "List.fold_left"
                                  [ fn.ocaml_expr; Ocaml_ir.Int 0; values_expr ]))
                      | TFn ([ TInt; TInt ], TInt) ->
                          Error.error "apply currently supports int binary reducers"
                      | TFn _ -> Error.error "apply currently supports int binary reducers"
                      | _ -> Error.error "apply expects a function"))))
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
                    | TFn ([ left_arg ], _left_ret), TFn ([ _right_arg ], right_ret)
                      when Types.equal left_arg right_ret ->
                        check_chain rest |> Result.map (fun (arg, _ret) ->
                            match List.hd fns with
                            | { ty = TFn ([ _ ], final_ret); _ } -> (arg, final_ret)
                            | _ -> (arg, right_ret))
                    | TFn _, TFn _ -> Error.error "comp function types do not line up"
                    | _ -> Error.error "comp expects functions")
              in
              match check_chain fns with
              | Error _ as err -> err
              | Ok (arg_ty, ret_ty) ->
                  let inner =
                    List.rev fns
                    |> List.fold_left
                         (fun expression fn ->
                           Ocaml_ir.Apply (fn.ocaml_expr, [ expression ]))
                         (Ocaml_ir.Ident "x")
                  in
                  Ok
                    (typed_ir (TFn ([ arg_ty ], ret_ty))
                       (Ocaml_ir.Fun ([ Ocaml_ir.PVar "x" ], inner)))))
    
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
                      remaining_names |> List.map (fun name -> Ocaml_ir.Ident name)
                    in
                    Ok
                      (typed_ir (TFn (remaining_tys, ret))
                         (Ocaml_ir.Fun
                            ( List.map (fun name -> Ocaml_ir.PVar name) remaining_names,
                              Ocaml_ir.Apply
                                ( fn.ocaml_expr,
                                  List.map (fun arg -> arg.ocaml_expr) fixed_args
                                  @ remaining_exprs ))))
                  else Error.error "partial fixed arguments do not match function"
              | TFn _ -> Error.error "partial requires fewer arguments than function arity"
              | _ -> Error.error "partial expects a function"))
      | _ -> Error.error "partial expects a function"
    
    and compile_identity scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok [ arg ] -> Ok arg
      | Ok _ -> Error.error "identity expects 1 arguments"
    
    and compile_constantly scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok [ value ] ->
          Ok
            (typed_ir (TFn ([ TAny ], value.ty))
               (Ocaml_ir.Fun ([ Ocaml_ir.PAny ], value.ocaml_expr)))
      | Ok _ -> Error.error "constantly expects 1 arguments"
    
    and compile_complement scope env arg_forms =
      match arg_forms with
      | [ fn_form ] -> (
          match compile_function_arg scope env fn_form with
          | Error _ as err -> err
          | Ok fn -> (
              match fn.ty with
              | TFn ([ arg_ty ], TBool) ->
                  Ok
                    (typed_ir (TFn ([ arg_ty ], TBool))
                       (Ocaml_ir.Fun
                          ( [ Ocaml_ir.PVar "x" ],
                            Ocaml_ir.Prefix
                              ("not", Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "x" ])) )))
              | TFn _ -> Error.error "complement expects a predicate"
              | _ -> Error.error "complement expects a function"))
      | _ -> Error.error "complement expects 1 function"
    
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
                  when option_for_all (fun arg_ty -> Types.equal arg_ty current_arg) arg_ty ->
                    collect (Some current_arg)
                      (Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "x" ]) :: exprs)
                      rest
                | TFn _ ->
                    Error.error (name ^ " expects predicates with the same argument type")
                | _ -> Error.error (name ^ " expects predicates"))
          in
          match collect None [] fns with
          | Error _ as err -> err
          | Ok (None, _) -> Error.error (name ^ " expects at least 1 predicate")
          | Ok (Some arg_ty, exprs) ->
              let op = if name = "every-pred" then "&&" else "||" in
              let body =
                match exprs with
                | [] -> Ocaml_ir.Bool (name = "every-pred")
                | first :: rest ->
                    List.fold_left
                      (fun acc expr -> Ocaml_ir.Infix (op, acc, expr))
                      first rest
              in
              Ok
                (typed_ir (TFn ([ arg_ty ], TBool))
                   (Ocaml_ir.Fun ([ Ocaml_ir.PVar "x" ], body))))
    
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
                           Types.compatible ~expected:arg_ty ~actual:current_arg)
                         arg_ty
                       && option_for_all
                            (fun ret_ty ->
                              Types.compatible ~expected:ret_ty ~actual:current_ret)
                            ret_ty ->
                    collect (Some current_arg) (Some current_ret)
                      (Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "x" ]) :: exprs)
                      rest
                | TFn ([ current_arg ], _)
                  when option_for_all
                         (fun arg_ty ->
                           Types.compatible ~expected:arg_ty ~actual:current_arg)
                         arg_ty ->
                    Error.error "juxt functions must return the same type"
                | TFn _ -> Error.error "juxt functions must accept the same argument type"
                | _ -> Error.error "juxt expects functions")
          in
          match collect None None [] fns with
          | Error _ as err -> err
          | Ok (Some arg_ty, Some ret_ty, exprs) ->
              Ok
                (typed_ir (TFn ([ arg_ty ], TVector ret_ty))
                   (Ocaml_ir.Fun
                      ( [ Ocaml_ir.PVar "x" ],
                        apply "Rrbvec.of_list" [ Ocaml_ir.List exprs ] )))
          | Ok _ -> Error.error "juxt expects at least 1 function")
    
    and compile_distinct_question scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok ([] | [ _ ]) -> Ok (typed_ir TBool (Ocaml_ir.Bool true))
      | Ok (first :: _ as args) ->
          if List.for_all (fun arg -> Types.equal first.ty arg.ty) args then
            Ok
              (typed_ir TBool
                 (Ocaml_ir.Infix
                    ( "=",
                      apply "List.length"
                        [ apply "List.sort_uniq"
                            [ Ocaml_ir.Ident "compare";
                              Ocaml_ir.List (List.map (fun arg -> arg.ocaml_expr) args) ] ],
                      Ocaml_ir.Int (List.length args) )))
          else Error.error "distinct? arguments must have the same type"
    
    and compile_compare scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok [ left; right ] ->
          if not (Types.equal left.ty right.ty) then
            Error.error "compare arguments must have the same type"
          else if not (comparable_type left.ty) then
            Error.error "compare expects comparable arguments"
          else
            Ok
              (typed_ir TInt
                 (apply "Stdlib.compare" [ left.ocaml_expr; right.ocaml_expr ]))
      | Ok _ -> Error.error "compare expects 2 arguments"
    
    and compile_key_extreme scope env name arg_forms =
      match arg_forms with
      | fn_form :: value_forms when value_forms <> [] -> (
          match (compile_function_arg scope env fn_form, compile_args_for scope env value_forms) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok values -> (
              let first = List.hd values in
              if not (List.for_all (fun value -> Types.equal first.ty value.ty) values) then
                Error.error (name ^ " values must have the same type")
              else
                match fn.ty with
                | TFn ([ arg_ty ], key_ty)
                  when Types.compatible ~expected:arg_ty ~actual:first.ty
                       && comparable_type key_ty ->
                    let rest = List.tl values in
                    let compare_op = if name = "max-key" then ">" else "<" in
                    let expr =
                      match rest with
                      | [] -> first.ocaml_expr
                      | _ ->
                          Ocaml_ir.Let
                            ( [ (Ocaml_ir.PVar "key_fn", fn.ocaml_expr);
                                ( Ocaml_ir.PVar "choose",
                                  Ocaml_ir.Fun
                                    ( [ Ocaml_ir.PVar "best"; Ocaml_ir.PVar "item" ],
                                      Ocaml_ir.If
                                        ( Ocaml_ir.Infix
                                            ( compare_op,
                                              apply "Stdlib.compare"
                                                [ Ocaml_ir.Apply
                                                    ( Ocaml_ir.Ident "key_fn",
                                                      [ Ocaml_ir.Ident "item" ] );
                                                  Ocaml_ir.Apply
                                                    ( Ocaml_ir.Ident "key_fn",
                                                      [ Ocaml_ir.Ident "best" ] ) ],
                                              Ocaml_ir.Int 0 ),
                                          Ocaml_ir.Ident "item",
                                          Ocaml_ir.Ident "best" ) ) ) ],
                              apply "List.fold_left"
                                [ Ocaml_ir.Ident "choose";
                                  first.ocaml_expr;
                                  Ocaml_ir.List (List.map (fun value -> value.ocaml_expr) rest) ] )
                    in
                    Ok (typed_ir first.ty expr)
                | TFn _ -> Error.error (name ^ " expects a key function matching values")
                | _ -> Error.error (name ^ " expects a function")))
      | _ -> Error.error (name ^ " expects function and values")
    
    and compile_hash_set scope env arg_forms =
      match arg_forms with
      | [] -> Error.error "empty hash-set requires a type annotation"
      | first :: rest -> (
          match compile_expr scope env first with
          | Error _ as err -> err
          | Ok first_expr ->
              let rec loop values = function
                | [] ->
                    Result.bind (Types.set_module_name first_expr.ty) (fun set_module ->
                           let rec coerce_values acc = function
                             | [] -> Ok (List.rev acc)
                             | value :: rest ->
                                 Result.bind (coerce_set_element first_expr.ty value)
                                   (fun value -> coerce_values (value :: acc) rest)
                           in
                           coerce_values [] (List.rev values)
                           |> Result.map (fun values ->
                                  typed_ir (TSet first_expr.ty)
                                    (Ocaml_ir.Apply
                                       ( Ocaml_ir.Ident (set_module ^ ".of_list"),
                                         [ Ocaml_ir.List values ] ))))
                | form :: rest -> (
                    match compile_expr scope env form with
                    | Error _ as err -> err
                    | Ok expr ->
                        if Types.same_shape first_expr.ty expr.ty then
                          loop (expr :: values) rest
                        else Error.error "hash-set elements must all have the same type")
              in
              loop [ first_expr ] rest)
    
    and compile_set_of arg_forms =
      match arg_forms with
      | [ FKeyword keyword ] -> (
          match Type_annotation.of_keyword keyword with
          | Error _ -> Error.error ("unknown set element type " ^ keyword)
          | Ok element_ty ->
              Types.set_module_name element_ty
              |> Result.map (fun set_module ->
                     typed_ir (TSet element_ty) (Ocaml_ir.Ident (set_module ^ ".empty"))))
      | _ -> Error.error "set-of expects one type keyword"
    
    and compile_disj scope env arg_forms =
      match arg_forms with
      | collection_form :: value_forms -> (
          match compile_expr scope env collection_form with
          | Error _ as err -> err
          | Ok collection -> (
              match collection.ty with
              | TSet inner ->
                  let rec remove_values expression = function
                    | [] -> Ok (typed_ir collection.ty expression)
                    | value_form :: rest -> (
                        match compile_expr scope env value_form with
                        | Error _ as err -> err
                        | Ok value ->
                            if Types.same_shape inner value.ty then
                              Result.bind (Types.set_module_name inner)
                                (fun set_module ->
                                  Result.bind (coerce_set_element inner value) (fun value ->
                                         remove_values
                                           (Ocaml_ir.Apply
                                              ( Ocaml_ir.Ident (set_module ^ ".remove"),
                                                [ value; expression ] ))
                                           rest))
                            else Error.error "disj value type must match set element type")
                  in
                  remove_values collection.ocaml_expr value_forms
              | _ -> Error.error "disj expects a set"))
      | [] -> Error.error "disj expects a set"
    
  in
  (compile_sort_by, compile_mapcat, compile_repeatedly, compile_reductions, compile_split_with, compile_partition_by, compile_run_bang, compile_map_indexed, compile_filterv, compile_mapv, compile_reduce_kv, compile_some, compile_sequence_bool_predicate, compile_map_call, compile_filter, compile_reduce, compile_apply, compile_comp, compile_partial, compile_identity, compile_constantly, compile_complement, compile_predicate_combinator, compile_juxt, compile_distinct_question, compile_compare, compile_key_extreme, compile_hash_set, compile_set_of, compile_disj)

let compile_sort_by ~compile_expr =
  let (compile_sort_by, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_sort_by

let compile_mapcat ~compile_expr =
  let (_, compile_mapcat, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_mapcat

let compile_repeatedly ~compile_expr =
  let (_, _, compile_repeatedly, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_repeatedly

let compile_reductions ~compile_expr =
  let (_, _, _, compile_reductions, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_reductions

let compile_split_with ~compile_expr =
  let (_, _, _, _, compile_split_with, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_split_with

let compile_partition_by ~compile_expr =
  let (_, _, _, _, _, compile_partition_by, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_partition_by

let compile_run_bang ~compile_expr =
  let (_, _, _, _, _, _, compile_run_bang, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_run_bang

let compile_map_indexed ~compile_expr =
  let (_, _, _, _, _, _, _, compile_map_indexed, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_map_indexed

let compile_filterv ~compile_expr =
  let (_, _, _, _, _, _, _, _, compile_filterv, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_filterv

let compile_mapv ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, compile_mapv, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_mapv

let compile_reduce_kv ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, compile_reduce_kv, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_reduce_kv

let compile_some ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, compile_some, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_some

let compile_sequence_bool_predicate ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, compile_sequence_bool_predicate, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_sequence_bool_predicate

let compile_map_call ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, compile_map_call, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_map_call

let compile_filter ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_filter, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_filter

let compile_reduce ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_reduce, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_reduce

let compile_apply ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_apply, _, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_apply

let compile_comp ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_comp, _, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_comp

let compile_partial ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_partial, _, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_partial

let compile_identity ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_identity, _, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_identity

let compile_constantly ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_constantly, _, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_constantly

let compile_complement ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_complement, _, _, _, _, _, _, _, _) = make compile_expr in
  compile_complement

let compile_predicate_combinator ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_predicate_combinator, _, _, _, _, _, _, _) = make compile_expr in
  compile_predicate_combinator

let compile_juxt ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_juxt, _, _, _, _, _, _) = make compile_expr in
  compile_juxt

let compile_distinct_question ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_distinct_question, _, _, _, _, _) = make compile_expr in
  compile_distinct_question

let compile_compare ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_compare, _, _, _, _) = make compile_expr in
  compile_compare

let compile_key_extreme ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_key_extreme, _, _, _) = make compile_expr in
  compile_key_extreme

let compile_hash_set ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_hash_set, _, _) = make compile_expr in
  compile_hash_set

let compile_set_of ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_set_of, _) = make compile_expr in
  compile_set_of

let compile_disj ~compile_expr =
  let (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, compile_disj) = make compile_expr in
  compile_disj
