open Ast
open Types
open Expression_support

module Env = Compiler_environment

type expression_result = (typed_expr, Error.t) result
type call = string -> Env.t -> Ast.form list -> expression_result
type named_call = string -> Env.t -> string -> Ast.form list -> expression_result
type forms = Ast.form list -> expression_result

type t = {
  compile_distinct_question : call;
  compile_compare : call;
  compile_key_extreme : named_call;
  compile_hash_set : call;
  compile_set_of : forms;
  compile_disj : call;
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
  let compile_args_for = compile_args_for compile_expr in
  let compile_function_arg scope env = function
    | FSymbol name -> lookup_function scope env name
    | form -> compile_expr scope env form
  in
  let rec comparable_type = function
    | TInt | TFloat | TString | TSymbol | TKeyword | TBool | TUnknown | TVar _ ->
        true
    | TNullable inner | TOcaml_app ("option", [ inner ]) ->
        comparable_type inner
    | _ -> false
  in
    let compile_distinct_question scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok ([] | [ _ ]) -> Ok (typed_ir TBool (Semantic_ir.Bool true))
      | Ok (first :: _ as args) ->
          if List.for_all (fun arg -> Types.equal first.ty arg.ty) args then
            Ok
              (typed_ir TBool
                 (Semantic_ir.Infix
                    ( "=",
                      apply "List.length"
                        [ apply "List.sort_uniq"
                            [ Semantic_ir.Ident "compare";
                              Semantic_ir.List (List.map (fun arg -> arg.semantic_expr) args) ] ],
                      Semantic_ir.Int (List.length args) )))
          else Error.error "distinct? arguments must have the same type"
    
    and compile_compare scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok [ left; right ]
        when Types.is_dynamic left.ty && Types.is_dynamic right.ty ->
          Ok
            (typed_ir TInt
               (apply "Lg_runtime.Runtime_dynamic.compare"
                  [ left.semantic_expr; right.semantic_expr ]))
      | Ok [ left; right ] ->
          if not (Types.equal left.ty right.ty) then
            Error.error
              ("compare arguments must have the same type: "
              ^ Types.source_name left.ty ^ " and "
              ^ Types.source_name right.ty)
          else if not (comparable_type left.ty) then
            Error.error "compare expects comparable arguments"
          else
            Ok
              (typed_ir TInt
                 (apply "Stdlib.compare" [ left.semantic_expr; right.semantic_expr ]))
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
                  when Types.assignable ~policy:Host_boundary ~expected:arg_ty
                         ~actual:first.ty
                       && comparable_type key_ty ->
                    let rest = List.tl values in
                    let compare_op = if name = "max-key" then ">" else "<" in
                    let expr =
                      match rest with
                      | [] -> first.semantic_expr
                      | _ ->
                          Semantic_ir.Let
                            ( [ (Semantic_ir.PVar "key_fn", fn.semantic_expr);
                                ( Semantic_ir.PVar "choose",
                                  Semantic_ir.Fun
                                    ( [ Semantic_ir.PVar "best"; Semantic_ir.PVar "item" ],
                                      Semantic_ir.If
                                        ( Semantic_ir.Infix
                                            ( compare_op,
                                              apply "Stdlib.compare"
                                                [ Semantic_ir.Apply
                                                    ( Semantic_ir.Ident "key_fn",
                                                      [ Semantic_ir.Ident "item" ] );
                                                  Semantic_ir.Apply
                                                    ( Semantic_ir.Ident "key_fn",
                                                      [ Semantic_ir.Ident "best" ] ) ],
                                              Semantic_ir.Int 0 ),
                                          Semantic_ir.Ident "item",
                                          Semantic_ir.Ident "best" ) ) ) ],
                              apply "List.fold_left"
                                [ Semantic_ir.Ident "choose";
                                  first.semantic_expr;
                                  Semantic_ir.List (List.map (fun value -> value.semantic_expr) rest) ] )
                    in
                    Ok (typed_ir first.ty expr)
                | TFn _ -> Error.error (name ^ " expects a key function matching values")
                | _ -> Error.error (name ^ " expects a function")))
      | _ -> Error.error (name ^ " expects function and values")
    
    and compile_hash_set scope env arg_forms =
      match arg_forms with
      | [] ->
          Ok
            (typed_ir (TSet TUnknown)
               (Semantic_ir.Ident "Lg_runtime.Runtime_poly_set.empty"))
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
                                    (Semantic_ir.Apply
                                       ( Semantic_ir.Ident (set_module ^ ".of_list"),
                                         [ Semantic_ir.List values ] ))))
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
                     typed_ir (TSet element_ty) (Semantic_ir.Ident (set_module ^ ".empty"))))
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
                                           (Semantic_ir.Apply
                                              ( Semantic_ir.Ident (set_module ^ ".remove"),
                                                [ value; expression ] ))
                                           rest))
                            else Error.error "disj value type must match set element type")
                  in
                  remove_values collection.semantic_expr value_forms
              | _ -> Error.error "disj expects a set"))
      | [] -> Error.error "disj expects a set"
    
  in
  { compile_distinct_question; compile_compare; compile_key_extreme; compile_hash_set; compile_set_of; compile_disj }
