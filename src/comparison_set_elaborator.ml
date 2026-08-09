open Ast
open Types
open Expression_support
module Env = Compiler_environment

type expression_result = (typed_expr, Error.t) result
type call = string -> Env.t -> Ast.form list -> expression_result

type forms = Ast.form list -> expression_result
type env_forms = Env.t -> Ast.form list -> expression_result

type t = {
  compile_compare : call;
  compile_hash_set : call;
  compile_set_of : env_forms;
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
  let rec comparable_type = function
    | TInt | TFloat | TString | TSymbol | TKeyword | TBool | TUnknown | TMeta _ | TVar _
      ->
        true
    | TNullable inner | TOcaml_app ("option", [ inner ]) ->
        comparable_type inner
    | _ -> false
  in
  let nullable_inner = function
    | TNullable inner | TOcaml_app ("option", [ inner ]) -> Some inner
    | _ -> None
  in
  let non_concrete_compare_error () =
    Error.error
      "compare expects one concrete comparable type; define a closed sum type \
       and match its cases explicitly for a heterogeneous domain"
  in
    let compile_compare scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok [ left; right ] ->
          let comparable =
            match (nullable_inner left.ty, nullable_inner right.ty) with
            | Some left_inner, None when Types.equal left_inner right.ty ->
                Some
                  ( left.semantic_expr,
                    Semantic_ir.Constructor
                      ("Some", Some right.semantic_expr),
                    left_inner )
            | None, Some right_inner when Types.equal left.ty right_inner ->
                Some
                  ( Semantic_ir.Constructor
                      ("Some", Some left.semantic_expr),
                    right.semantic_expr,
                    right_inner )
            | _ when Types.equal left.ty right.ty ->
                Some (left.semantic_expr, right.semantic_expr, left.ty)
            | _ -> None
          in
          (match comparable with
          | None
            when Option.is_some (Types.seqable_constraint_info left.ty)
                 && Option.is_some (Types.seqable_constraint_info right.ty) ->
              non_concrete_compare_error ()
          | None ->
            Error.error
              ("compare arguments must have the same type: "
           ^ Types.source_name left.ty ^ " and " ^ Types.source_name right.ty)
          | Some (left, right, ty) -> (
              match
                Core_protocols.find_comparable ty
                  (Compiler_environment.protocols env)
              with
              | Some
                  {
                    ty = TFn ([ left_ty; right_ty ], return_ty);
                    ocaml_name;
                    _;
                  }
                when Types.assignable ~policy:Host_boundary ~expected:left_ty
                       ~actual:ty
                     && Types.assignable ~policy:Host_boundary
                          ~expected:right_ty ~actual:ty
                     && (Types.equal return_ty TInt
                        || Types.equal return_ty (TOcaml "int")
                        || match return_ty with
                           | TUnknown | TMeta _ | TVar _ -> true
                           | _ -> false) ->
                  Ok
                    (typed_ir TInt
                       (apply ocaml_name [ left; right ]))
              | Some _ ->
                  Error.error
                    "IComparable/-compare has an invalid signature"
              | None when not (comparable_type ty) ->
                  non_concrete_compare_error ()
              | None ->
                  Ok
                    (typed_ir TInt
                       (apply "Stdlib.compare" [ left; right ])))
            )
      | Ok _ -> Error.error "compare expects 2 arguments"
    and compile_hash_set scope env arg_forms =
      match arg_forms with
      | [] -> (
          match Compiler_environment.expected_type env with
          | Some (TSet element_ty as set_ty)
            when not (Type_solver.is_open element_ty) ->
              Result.map
                (fun set_module ->
                  typed_ir set_ty
                    (Semantic_ir.Ident (set_module ^ ".empty")))
                (set_module_name env element_ty)
          | Some _ | None ->
              Ok
                (typed_ir (TSet TUnknown)
                   (Semantic_ir.Ident "Lg_runtime.Runtime_poly_set.empty")))
      | _ :: _ -> (
          match compile_args_for scope env arg_forms with
          | Error _ as err -> err
          | Ok [] -> Error.error "hash-set expects elements"
          | Ok exprs ->
              Result.bind
                (merge_collection_value_types "set" exprs)
                (fun element_ty ->
                  Result.bind (set_module_name env element_ty)
                    (fun set_module ->
                      let rec coerce_values acc = function
                        | [] -> Ok (List.rev acc)
                        | value :: rest ->
                            let coerced =
                              if Types.equal element_ty value.ty then
                                Ok value.semantic_expr
                              else
                                match element_ty with
                                | TNullable _
                                | TOcaml_app ("option", [ _ ]) ->
                                    Ok
                                      (coerce_expression_to_type element_ty
                                         value.ty value.semantic_expr)
                                | _ -> coerce_set_element element_ty value
                            in
                            Result.bind coerced (fun value ->
                                coerce_values (value :: acc) rest)
                      in
                      coerce_values [] exprs
                      |> Result.map (fun values ->
                             typed_ir (TSet element_ty)
                               (Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      (set_module ^ ".of_list"),
                                    [ Semantic_ir.List values ] ))))))
    and compile_set_of env arg_forms =
      match arg_forms with
      | [ FKeyword keyword ] -> (
          match Type_annotation.of_keyword keyword with
          | Error _ -> Error.error ("unknown set element type " ^ keyword)
          | Ok element_ty ->
              let set_module =
                match element_ty with
                | TOcaml "int" -> Ok "Lg_runtime.Core_set.Int_set"
                | TOcaml name
                  when not (String.contains name '.')
                       && not
                            (String.starts_with ~prefix:"__lg_record:" name) -> (
                    match
                      Type_registry.find_by_emitted_name name
                        (Compiler_environment.types env)
                    with
                    | Some { kind = Type_registry.Variant; _ } ->
                        Ok "Lg_runtime.Runtime_poly_set"
                    | Some _ | None ->
                        Error.error
                          ("sets require a generated comparator for "
                         ^ Types.source_name element_ty))
                | _ -> set_module_name env element_ty
              in
              set_module
              |> Result.map (fun set_module ->
                typed_ir (TSet element_ty)
                  (Semantic_ir.Ident (set_module ^ ".empty"))))
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
                                Result.bind (coerce_set_element inner value)
                                  (fun value ->
                                         remove_values
                                           (Semantic_ir.Apply
                                         ( Semantic_ir.Ident
                                             (set_module ^ ".remove"),
                                                [ value; expression ] ))
                                           rest))
                          else
                            Error.error
                              "disj value type must match set element type")
                  in
                  remove_values collection.semantic_expr value_forms
              | _ -> Error.error "disj expects a set"))
      | [] -> Error.error "disj expects a set"
  in
  {
    compile_compare;
    compile_hash_set;
    compile_set_of;
    compile_disj;
  }
