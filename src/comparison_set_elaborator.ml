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
  let compile_function_arg scope env = function
    | FSymbol name -> lookup_function scope env name
    | form -> compile_expr scope env form
  in
  let comparable_type = function
    | TInt | TString | TSymbol | TKeyword | TBool | TAny -> true
    | _ -> false
  in
    let compile_distinct_question scope env arg_forms =
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
  (compile_distinct_question, compile_compare, compile_key_extreme, compile_hash_set, compile_set_of, compile_disj)

let compile_distinct_question ~compile_expr =
  let (compile_distinct_question, _, _, _, _, _) = make compile_expr in
  compile_distinct_question

let compile_compare ~compile_expr =
  let (_, compile_compare, _, _, _, _) = make compile_expr in
  compile_compare

let compile_key_extreme ~compile_expr =
  let (_, _, compile_key_extreme, _, _, _) = make compile_expr in
  compile_key_extreme

let compile_hash_set ~compile_expr =
  let (_, _, _, compile_hash_set, _, _) = make compile_expr in
  compile_hash_set

let compile_set_of ~compile_expr =
  let (_, _, _, _, compile_set_of, _) = make compile_expr in
  compile_set_of

let compile_disj ~compile_expr =
  let (_, _, _, _, _, compile_disj) = make compile_expr in
  compile_disj
