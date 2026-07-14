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
                     Semantic_ir.PConstraint
                       (Semantic_ir.PVar binding.ocaml_name, record.type_name)
                 | _ -> Semantic_ir.PVar binding.ocaml_name
               in
               typed_ir (TFn ([ element_ty ], body.ty))
                 (Semantic_ir.Fun ([ pattern ], body.semantic_expr)))
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
                          [ apply "List.map" [ fn.semantic_expr; list_expr ] ]))
              | TFn ([ param_ty ], TVector ret_inner), Ok (inner, list_expr)
                when Types.equal param_ty inner ->
                  Ok
                    (typed_ir (TList ret_inner)
                       (apply "List.concat"
                          [ apply "List.map"
                              [ Semantic_ir.Fun
                                  ( [ Semantic_ir.PVar "item" ],
                                    apply "Rrbvec.to_list"
                                      [ Semantic_ir.Apply
                                          (fn.semantic_expr, [ Semantic_ir.Ident "item" ]) ] );
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
                                  [ Semantic_ir.Fun
                                      ( [ Semantic_ir.PVar "item" ],
                                        apply (set_module ^ ".elements")
                                          [ Semantic_ir.Apply
                                              (fn.semantic_expr, [ Semantic_ir.Ident "item" ]) ] );
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
              | TFn ([ param_ty ], TBool), Ok (inner, list_expr) when Types.equal param_ty inner ->
                  let split_body =
                    Semantic_ir.Match
                      ( Semantic_ir.Ident "rest",
                        [ ( Semantic_ir.PCons (Semantic_ir.PVar "item", Semantic_ir.PVar "tail"),
                            Semantic_ir.If
                              ( Semantic_ir.Apply (fn.semantic_expr, [ Semantic_ir.Ident "item" ]),
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
          match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok collection -> (
              match (fn.ty, collection_to_list_expr collection) with
              | TFn ([ param_ty ], _ret), Ok (inner, list_expr) when Types.equal param_ty inner ->
                  Ok
                    (typed_ir TUnit
                       (Semantic_ir.Let
                          ( [ ( Semantic_ir.PUnit,
                                apply "List.iter"
                                  [ Semantic_ir.Fun
                                      ( [ Semantic_ir.PVar "item" ],
                                        apply "ignore"
                                          [ Semantic_ir.Apply
                                              (fn.semantic_expr, [ Semantic_ir.Ident "item" ]) ] );
                                    list_expr ] ) ],
                            Semantic_ir.Unit )))
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
          match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok fn, Ok collection -> (
              match (fn.ty, collection_to_list_expr collection) with
              | TFn ([ param_ty ], ret), Ok (inner, list_expr) when Types.equal param_ty inner ->
                  Ok
                    (typed_ir (TVector ret)
                       (apply "Rrbvec.of_list"
                          [ apply "List.map" [ fn.semantic_expr; list_expr ] ]))
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
                          [ Semantic_ir.Fun
                              ( [ Semantic_ir.PVar "acc";
                                  Semantic_ir.PTuple
                                    [ Semantic_ir.PVar "index"; Semantic_ir.PVar "item" ] ],
                                Semantic_ir.Apply
                                  ( fn.semantic_expr,
                                    [ Semantic_ir.Ident "acc";
                                      Semantic_ir.Ident "index";
                                      Semantic_ir.Ident "item" ] ) );
                            init.semantic_expr;
                            apply "List.mapi"
                              [ Semantic_ir.Fun
                                  ( [ Semantic_ir.PVar "index"; Semantic_ir.PVar "item" ],
                                    Semantic_ir.Tuple
                                      [ Semantic_ir.Ident "index"; Semantic_ir.Ident "item" ] );
                                apply "Rrbvec.to_list" [ collection.semantic_expr ] ] ]))
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
                  Ok (typed_ir TBool (apply "List.exists" [ fn.semantic_expr; list_expr ]))
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
                | "not-every?" -> Semantic_ir.Prefix ("not", all_expr)
                | _ -> all_expr
              in
              let predicate_expr =
                match name with
                | "not-any?" ->
                    Semantic_ir.Fun
                      ( [ Semantic_ir.PVar "item" ],
                        Semantic_ir.Prefix
                          ("not", Semantic_ir.Apply (fn.semantic_expr, [ Semantic_ir.Ident "item" ])) )
                | _ -> fn.semantic_expr
              in
              match (fn.ty, collection.ty) with
              | TFn ([ param_ty ], TBool), TList inner when Types.equal param_ty inner ->
                  let all_expr = apply "List.for_all" [ predicate_expr; collection.semantic_expr ] in
                  Ok (typed_ir TBool (build all_expr))
              | TFn _, TList _ -> Error.error (name ^ " expects a predicate matching list elements")
              | _, TList _ -> Error.error (name ^ " expects a function")
              | TFn ([ param_ty ], TBool), TVector inner when Types.equal param_ty inner ->
                  let all_expr = apply "Rrbvec.for_all" [ predicate_expr; collection.semantic_expr ] in
                  Ok (typed_ir TBool (build all_expr))
              | TFn _, TVector _ ->
                  Error.error (name ^ " expects a predicate matching vector elements")
              | _, TVector _ -> Error.error (name ^ " expects a function")
              | TFn ([ param_ty ], TBool), TSet inner
                when Types.assignable ~policy:Host_boundary ~expected:param_ty ~actual:inner ->
                  Types.set_module_name inner
                  |> Result.map (fun set_module ->
                         let fn_expr = constrain_record_function_argument_expr fn inner in
                         let predicate_expr =
                           match name with
                           | "not-any?" ->
                               Semantic_ir.Fun
                                 ( [ Semantic_ir.PVar "item" ],
                                   Semantic_ir.Prefix
                                     ( "not",
                                       Semantic_ir.Apply (fn_expr, [ Semantic_ir.Ident "item" ]) ) )
                           | _ -> fn_expr
                         in
                         let all_expr =
                           apply "List.for_all"
                             [ predicate_expr;
                               apply (set_module ^ ".elements") [ collection.semantic_expr ] ]
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
          | Ok collection -> (
              match Core_sequence_transform.collection_to_seq_expr collection with
              | Error _ -> Error.error "map expects a seqable value"
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
                           (apply "Cljml.Runtime_seq.map"
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
              match Core_sequence_transform.collection_to_seq_expr collection with
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
                               (apply "Cljml.Runtime_seq.filter"
                                  [ fn.semantic_expr; sequence ]))
                      | TFn _ ->
                          Error.error
                            "filter expects a predicate matching sequence elements"
                      | _ -> Error.error "filter expects a function"))))
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
                          [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]))
              | TFn _, TList _ -> Error.error "reduce function type does not match init and list"
              | _, TList _ -> Error.error "reduce expects a function"
              | TFn ([ acc_ty; item_ty ], ret), TVector inner
                when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
                 Ok
                    (typed_ir init.ty
                       (apply "Rrbvec.fold_left"
                          [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]))
              | TFn _, TVector _ -> Error.error "reduce function type does not match init and vector"
              | _, TVector _ -> Error.error "reduce expects a function"
              | TFn ([ acc_ty; item_ty ], ret), TSet inner
                when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
                  Types.set_module_name inner
                  |> Result.map (fun set_module ->
                         typed_ir init.ty
                           (apply "List.fold_left"
                              [ fn.semantic_expr;
                                init.semantic_expr;
                                apply (set_module ^ ".elements")
                                  [ collection.semantic_expr ] ]))
              | TFn _, TSet _ -> Error.error "reduce function type does not match init and set"
              | _, TSet _ -> Error.error "reduce expects a function"
              | _ -> Error.error "reduce expects a list, vector, or set"))
      | _ -> Error.error "reduce expects function, init, and collection"
    
  in
  { compile_sort_by; compile_mapcat; compile_repeatedly; compile_reductions; compile_split_with; compile_partition_by; compile_run_bang; compile_map_indexed; compile_filterv; compile_mapv; compile_reduce_kv; compile_some; compile_sequence_bool_predicate; compile_map_call; compile_filter; compile_reduce }
