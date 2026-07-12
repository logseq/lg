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
    
  in
  { compile_sort_by; compile_mapcat; compile_repeatedly; compile_reductions; compile_split_with; compile_partition_by; compile_run_bang; compile_map_indexed; compile_filterv; compile_mapv; compile_reduce_kv; compile_some; compile_sequence_bool_predicate; compile_map_call; compile_filter; compile_reduce }
