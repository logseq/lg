open Ast
open Types
open Expression_support

module Env = Compiler_environment

type expression_result = (typed_expr, Error.t) result
type call = string -> Env.t -> Ast.form list -> expression_result
type forms = Ast.form list -> expression_result

type t = {
  compile_list : call;
  compile_list_star : call;
  compile_range : call;
  compile_list_of : forms;
  compile_vector_of : forms;
  compile_conj : call;
  compile_cons : call;
  compile_subvec : call;
  compile_nth : call;
  compile_get : call;
  compile_assoc : call;
  compile_dissoc : call;
  compile_merge : call;
  compile_hash_map : call;
  compile_update : call;
  compile_select_keys : call;
  compile_contains : call;
  compile_keys : call;
  compile_vals : call;
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
  let special_forms : Special_form_elaborator.t =
    Special_form_elaborator.create ~compile_expr
  in
  let compile_map = special_forms.compile_map in
  let compile_function_arg scope env = function
    | FSymbol name -> lookup_function scope env name
    | form -> compile_expr scope env form
  in
    let compile_list scope env forms =
      match forms with
      | [] -> Ok (typed_ir (TList TUnknown) (Semantic_ir.List []))
      | first :: rest -> (
          match compile_expr scope env first with
          | Error _ as err -> err
          | Ok first_expr ->
              let rec loop acc = function
                | [] ->
                    let values =
                      List.rev acc |> List.map (fun expr -> expr.semantic_expr)
                    in
                    Ok (typed_ir (TList first_expr.ty) (Semantic_ir.List values))
                | form :: rest -> (
                    match compile_expr scope env form with
                    | Error _ as err -> err
                    | Ok expr ->
                        if Types.equal first_expr.ty expr.ty then loop (expr :: acc) rest
                        else Error.error "list elements must all have the same type")
              in
              loop [ first_expr ] rest)
    
    and compile_list_star scope env arg_forms =
      match List.rev arg_forms with
      | [] -> Error.error "list* expects values and final collection"
      | final_form :: prefix_forms_rev -> (
          match compile_expr scope env final_form with
          | Error _ as err -> err
          | Ok final -> (
              match Core_sequence_transform.collection_to_list_expr final with
              | Error _ -> Error.error "list* final argument must be a collection"
              | Ok (inner, final_list_expr) -> (
                  let prefix_forms = List.rev prefix_forms_rev in
                  match compile_args_for scope env prefix_forms with
                  | Error _ as err -> err
                  | Ok prefix_args ->
                      if List.for_all (fun arg -> Types.equal inner arg.ty) prefix_args then
                        let list_expr =
                          match prefix_args with
                          | [] -> final_list_expr
                          | _ ->
                              Semantic_ir.Infix
                                ( "@",
                                  Semantic_ir.List
                                    (List.map (fun arg -> arg.semantic_expr) prefix_args),
                                  final_list_expr )
                        in
                        Ok (typed_ir (TList inner) list_expr)
                      else Error.error "list* value type must match final collection element type")))
    
    and compile_range scope env arg_forms =
      let literal_zero = function FInt 0 -> true | _ -> false in
      let finite_range start stop step =
        typed_ir (TSeq TInt)
          (apply "Lg_runtime.Runtime_seq.range_until" [ start; stop; step ])
      in
      match arg_forms with
      | [] ->
          Ok
            (typed_ir (TSeq TInt)
               (apply "Lg_runtime.Runtime_seq.range"
                  [ Semantic_ir.Int 0; Semantic_ir.Int 1 ]))
      | [ end_form ] -> (
          match compile_expr scope env end_form with
          | Error _ as err -> err
          | Ok end_expr ->
              if Types.equal end_expr.ty TInt then
                Ok
                  (finite_range (Semantic_ir.Int 0) end_expr.semantic_expr
                     (Semantic_ir.Int 1))
              else Error.error "range arguments must be int")
      | [ start_form; end_form ] -> (
          match (compile_expr scope env start_form, compile_expr scope env end_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok start_expr, Ok end_expr ->
              if Types.equal start_expr.ty TInt && Types.equal end_expr.ty TInt then
                Ok
                  (finite_range start_expr.semantic_expr end_expr.semantic_expr
                     (Semantic_ir.Int 1))
              else Error.error "range arguments must be int")
      | [ start_form; end_form; step_form ] ->
          if literal_zero step_form then Error.error "range step cannot be 0"
          else (
            match
              ( compile_expr scope env start_form,
                compile_expr scope env end_form,
                compile_expr scope env step_form )
            with
            | (Error _ as err), _, _ -> err
            | _, (Error _ as err), _ -> err
            | _, _, (Error _ as err) -> err
            | Ok start_expr, Ok end_expr, Ok step_expr ->
                if
                  Types.equal start_expr.ty TInt && Types.equal end_expr.ty TInt
                  && Types.equal step_expr.ty TInt
                then
                  Ok
                    (finite_range start_expr.semantic_expr end_expr.semantic_expr
                       step_expr.semantic_expr)
                else Error.error "range arguments must be int")
      | _ -> Error.error "range expects zero to three arguments"
    
    and compile_list_of arg_forms =
      match arg_forms with
      | [ FKeyword keyword ] -> (
          match Type_annotation.of_keyword keyword with
          | Error _ as err -> err
          | Ok element_ty -> Ok (typed_ir (TList element_ty) (Semantic_ir.List [])))
      | _ -> Error.error "list-of expects one type keyword"
    
    and compile_vector_of arg_forms =
      match arg_forms with
      | [ FKeyword keyword ] -> (
          match Type_annotation.of_keyword keyword with
          | Error _ as err -> err
          | Ok element_ty ->
              Ok (typed_ir (TVector element_ty) (Semantic_ir.Ident "Rrbvec.empty")))
      | _ -> Error.error "vector-of expects one type keyword"
    
    and compile_conj scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok (collection :: values) when values <> [] ->
          let add_value collection value =
            match collection.ty with
            | TList inner when Types.equal inner value.ty ->
                Ok
                  (typed_ir collection.ty
                     (Semantic_ir.Cons (value.semantic_expr, collection.semantic_expr)))
            | TList _ -> Error.error "conj value type must match list element type"
            | TVector inner when Types.equal inner value.ty ->
                Ok
                  (typed_ir collection.ty
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Rrbvec.push_back",
                          [ collection.semantic_expr; value.semantic_expr ] )))
            | TVector _ -> Error.error "conj value type must match vector element type"
            | TSet inner when Types.same_shape inner value.ty ->
                Result.bind (Types.set_module_name inner) (fun set_module ->
                       coerce_set_element inner value
                       |> Result.map (fun value ->
                              typed_ir collection.ty
                                (Semantic_ir.Apply
                                   ( Semantic_ir.Ident (set_module ^ ".add"),
                                     [ value; collection.semantic_expr ] ))))
            | TSet _ -> Error.error "conj value type must match set element type"
            | _ -> Error.error "conj expects a list, vector, or set"
          in
          values
          |> List.fold_left
               (fun acc value ->
                 match acc with
                 | Error _ as err -> err
                 | Ok collection -> add_value collection value)
               (Ok collection)
      | Ok _ -> Error.error "conj expects collection and values"
    
    and compile_cons scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok [ value; collection ] -> (
          match collection.ty with
          | TList inner when Types.equal inner value.ty ->
              Ok
                (typed_ir collection.ty
                   (Semantic_ir.Cons (value.semantic_expr, collection.semantic_expr)))
          | TList _ -> Error.error "cons value type must match list element type"
          | _ -> Error.error "cons expects a value and list")
      | Ok _ -> Error.error "cons expects value and list"
    
    and compile_subvec scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok [ vector; start ] -> (
          match (vector.ty, start.ty) with
          | TVector _, TInt ->
              Ok
                (typed_ir vector.ty
                   (Semantic_ir.Apply
                      ( Semantic_ir.Ident "Option.get",
                        [ Semantic_ir.Apply
                            ( Semantic_ir.Ident "Rrbvec.subvec",
                              [ vector.semantic_expr;
                                start.semantic_expr;
                                Semantic_ir.Apply
                                  (Semantic_ir.Ident "Rrbvec.length", [ vector.semantic_expr ]) ] ) ] )))
          | TVector _, _ -> Error.error "subvec indexes must be int"
          | _ -> Error.error "subvec expects a vector")
      | Ok [ vector; start; stop ] -> (
          match (vector.ty, start.ty, stop.ty) with
          | TVector _, TInt, TInt ->
              Ok
                (typed_ir vector.ty
                   (Semantic_ir.Apply
                      ( Semantic_ir.Ident "Option.get",
                        [ Semantic_ir.Apply
                            ( Semantic_ir.Ident "Rrbvec.subvec",
                              [ vector.semantic_expr; start.semantic_expr; stop.semantic_expr ] ) ] )))
          | TVector _, _, _ -> Error.error "subvec indexes must be int"
          | _ -> Error.error "subvec expects a vector")
      | Ok _ -> Error.error "subvec expects vector, start, and optional stop"
    
    and compile_nth scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok [ collection; index ] ->
          if not (Types.equal index.ty TInt) then
            Error.error "nth index must be int"
          else Collection_capability.nth_expr env collection index
      | Ok [ collection; index; default ] ->
          if not (Types.equal index.ty TInt) then
            Error.error "nth index must be int"
          else (
            match Collection_capability.to_seq_expr env collection with
            | Error _ -> Error.error "nth with default expects a seqable value"
            | Ok (inner, sequence) ->
                if not (Types.equal inner default.ty) then
                  Error.error "nth default must match collection element type"
                else
                  Ok
                    (typed_ir inner
                       (Semantic_ir.Match
                          ( apply "Lg_runtime.Runtime_seq.nth_opt"
                              [ index.semantic_expr; sequence ],
                            [ ( Semantic_ir.PConstructor
                                  ("Some", Some (Semantic_ir.PVar "value")),
                                Semantic_ir.Ident "value" );
                              (Semantic_ir.PConstructor ("None", None), default.semantic_expr) ] ))))
      | Ok _ -> Error.error "nth expects 2 or 3 arguments"
    
    and compile_get scope env arg_forms =
      match arg_forms with
      | [ target_form; FKeyword keyword ] -> (
          match compile_expr scope env target_form with
          | Error _ as err -> err
          | Ok target -> (
              match target.ty with
              | TRecord fields | TNamed_record { fields; nominal = false; _ } -> (
                  match find_field keyword fields with
                  | Some field ->
                      Ok
                        (typed_ir field.ty
                           (Structural_map.field_expr target field))
                  | None -> Error.error ("unknown field " ^ keyword))
              | TNamed_record { fields; nominal = true; _ } -> (
                  match find_field keyword fields with
                  | Some field ->
                      Ok
                        (typed_ir field.ty
                           (Structural_map.field_expr target field))
                  | None ->
                      Error.error
                        ("unknown record field " ^ Names.keyword_source_name keyword))
              | ty when is_ocaml_owned_type ty ->
                  Ok
                    (typed_ir TUnknown
                       (Semantic_ir.Field
                          (target.semantic_expr, Names.keyword_to_ocaml_name keyword)))
              | _ -> Error.error "get expects a map"))
      | [ target_form; index_form ] -> (
          match (compile_expr scope env target_form, compile_expr scope env index_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok target, Ok index -> (
              match (target.ty, index.ty) with
              | TVector inner, TInt ->
                  Ok
                    (typed_ir inner
                       (apply "Rrbvec.nth" [ target.semantic_expr; index.semantic_expr ]))
              | TVector _, _ -> Error.error "get vector index must be int"
              | _ -> Error.error "get key must be a keyword"))
      | [ target_form; FKeyword keyword; default_form ] -> (
          match
            (compile_expr scope env target_form, compile_expr scope env default_form)
          with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok target, Ok default -> (
              match target.ty with
              | TRecord fields | TNamed_record { fields; _ } -> (
                  match find_field keyword fields with
                  | Some field when Types.equal field.ty default.ty ->
                      Ok
                        (typed_ir field.ty
                           (Structural_map.field_expr target field))
                  | Some field ->
                      Error.error
                        ("get default for " ^ keyword ^ " must be " ^ source_name field.ty)
                  | None -> Ok default)
              | _ -> Error.error "get expects a map"))
      | [ target_form; index_form; default_form ] -> (
          match
            ( compile_expr scope env target_form,
              compile_expr scope env index_form,
              compile_expr scope env default_form )
          with
          | (Error _ as err), _, _ -> err
          | _, (Error _ as err), _ -> err
          | _, _, (Error _ as err) -> err
          | Ok target, Ok index, Ok default -> (
              match (target.ty, index.ty) with
              | TVector inner, TInt when Types.equal inner default.ty ->
                  Ok
                    (typed_ir inner
                       (Semantic_ir.Match
                          ( apply "Rrbvec.nth_opt" [ target.semantic_expr; index.semantic_expr ],
                            [ ( Semantic_ir.PConstructor ("Some", Some (Semantic_ir.PVar "value")),
                                Semantic_ir.Ident "value" );
                              (Semantic_ir.PConstructor ("None", None), default.semantic_expr) ] )))
              | TVector _, TInt -> Error.error "get default for vector must match element type"
              | TVector _, _ -> Error.error "get vector index must be int"
              | _ -> Error.error "get key must be a keyword"))
      | _ -> Error.error "get expects 2 or 3 arguments"
    
    and compile_assoc scope env arg_forms =
      match arg_forms with
      | target_form :: pair_forms ->
          let rec compile_record_pairs acc = function
            | [] -> Ok (List.rev acc)
            | FKeyword keyword :: value_form :: rest -> (
                match compile_expr scope env value_form with
                | Error _ as err -> err
                | Ok value -> compile_record_pairs ((keyword, value) :: acc) rest)
            | _ -> Error.error "assoc expects map followed by keyword/value pairs"
          in
          let rec compile_vector_pairs acc = function
            | [] -> Ok (List.rev acc)
            | index_form :: value_form :: rest -> (
                match
                  ( compile_expr scope env index_form,
                    compile_expr scope env value_form )
                with
                | (Error _ as err), _ -> err
                | _, (Error _ as err) -> err
                | Ok index, Ok value -> compile_vector_pairs ((index, value) :: acc) rest)
            | _ -> Error.error "assoc expects collection followed by key/value pairs"
          in
          (match compile_expr scope env target_form with
          | Error _ as err -> err
          | Ok target -> (
              if pair_forms = [] || List.length pair_forms mod 2 <> 0 then
                match target.ty with
                | TRecord _ | TNamed_record _ ->
                    Error.error "assoc expects map followed by keyword/value pairs"
                | TVector _ -> Error.error "assoc expects vector followed by index/value pairs"
                | _ -> Error.error "assoc expects collection followed by key/value pairs"
              else
                match target.ty with
                | TRecord _ | TNamed_record _ -> (
                    match compile_record_pairs [] pair_forms with
                    | Error _ as err -> err
                    | Ok pairs -> Structural_map.assoc_many target pairs)
                | TVector inner -> (
                    match compile_vector_pairs [] pair_forms with
                    | Error _ as err -> err
                    | Ok pairs ->
                        let rec apply_pairs expr = function
                          | [] -> Ok expr
                          | (index, value) :: rest ->
                              if not (Types.equal index.ty TInt) then
                                Error.error "assoc vector index must be int"
                              else if not (Types.equal value.ty inner) then
                                Error.error "assoc vector value must match element type"
                              else
                                apply_pairs
                                  (apply "Rrbvec.set"
                                     [ expr; index.semantic_expr; value.semantic_expr ])
                                  rest
                        in
                        (match apply_pairs target.semantic_expr pairs with
                        | Error _ as err -> err
                        | Ok expr -> Ok (typed_ir target.ty expr)))
                | _ -> Error.error "assoc expects a map or vector"))
      | _ -> Error.error "assoc expects collection followed by key/value pairs"
    
    and compile_dissoc scope env arg_forms =
      match arg_forms with
      | target_form :: key_forms -> (
          let rec parse_keys acc = function
            | [] -> Ok (List.rev acc)
            | FKeyword keyword :: rest -> parse_keys (keyword :: acc) rest
            | _ -> Error.error "dissoc expects map followed by keywords"
          in
          match compile_expr scope env target_form with
          | Error _ as err -> err
          | Ok target -> (
              match parse_keys [] key_forms with
              | Error _ as err -> err
              | Ok keywords -> Structural_map.dissoc_many target keywords))
      | _ -> Error.error "dissoc expects map followed by keywords"
    
    and compile_merge scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok maps -> Structural_map.merge maps
    
    and compile_hash_map scope env arg_forms =
      let rec parse_pairs acc = function
        | [] -> Ok (List.rev acc)
        | FKeyword keyword :: value_form :: rest ->
            parse_pairs ((FKeyword keyword, value_form) :: acc) rest
        | _ -> Error.error "hash-map expects keyword/value pairs"
      in
      if arg_forms = [] || List.length arg_forms mod 2 <> 0 then
        Error.error "hash-map expects keyword/value pairs"
      else
        match parse_pairs [] arg_forms with
        | Error _ as err -> err
        | Ok pairs -> compile_map scope env pairs
    
    and compile_update scope env arg_forms =
      match arg_forms with
      | target_form :: FKeyword keyword :: fn_form :: extra_forms -> (
          match
            ( compile_expr scope env target_form,
              compile_function_arg scope env fn_form,
              compile_args_for scope env extra_forms )
          with
          | (Error _ as err), _, _ -> err
          | _, (Error _ as err), _ -> err
          | _, _, (Error _ as err) -> err
          | Ok target, Ok fn, Ok extra_args -> (
              match target.ty with
              | TRecord fields | TNamed_record { fields; _ } -> (
                  match find_field keyword fields with
                  | None -> Error.error ("cannot update unknown field " ^ keyword)
                  | Some field -> (
                      match fn.ty with
                      | TFn (param_tys, ret)
                        when List.length param_tys = List.length extra_args + 1
                             && Types.assignable ~policy:Host_boundary ~expected:(List.hd param_tys)
                                  ~actual:field.ty
                             && List.for_all2
                                  (fun expected arg -> Types.assignable ~policy:Host_boundary ~expected ~actual:arg.ty)
                                  (drop 1 param_tys) extra_args
                             && Types.equal ret field.ty ->
                          let value_expr =
                            Semantic_ir.Apply
                              ( fn.semantic_expr,
                                Structural_map.field_expr target field
                                :: List.map (fun arg -> arg.semantic_expr) extra_args )
                          in
                          Structural_map.update_value target fields keyword ret value_expr
                      | TFn (_param_tys, ret) when not (Types.equal ret field.ty) ->
                          Error.error
                            (Printf.sprintf "cannot update %s as %s because it is already %s"
                               keyword (source_name ret) (source_name field.ty))
                      | TFn _ ->
                          Error.error
                            "update function arguments do not match field and extra arguments"
                      | _ -> Error.error "update expects a function"))
              | _ -> Error.error "update expects a map"))
      | target_form :: index_form :: fn_form :: extra_forms -> (
          match
            ( compile_expr scope env target_form,
              compile_expr scope env index_form,
              compile_function_arg scope env fn_form,
              compile_args_for scope env extra_forms )
          with
          | (Error _ as err), _, _, _ -> err
          | _, (Error _ as err), _, _ -> err
          | _, _, (Error _ as err), _ -> err
          | _, _, _, (Error _ as err) -> err
          | Ok target, Ok index, Ok fn, Ok extra_args -> (
              match (target.ty, index.ty) with
              | TVector inner, TInt -> (
                  match fn.ty with
                  | TFn (param_tys, ret)
                    when List.length param_tys = List.length extra_args + 1
                         && Types.assignable ~policy:Host_boundary ~expected:(List.hd param_tys) ~actual:inner
                         && List.for_all2
                              (fun expected arg -> Types.assignable ~policy:Host_boundary ~expected ~actual:arg.ty)
                              (drop 1 param_tys) extra_args
                         && Types.equal ret inner ->
                      let old_expr =
                        apply "Rrbvec.nth" [ target.semantic_expr; index.semantic_expr ]
                      in
                      let value_expr =
                        Semantic_ir.Apply
                          (fn.semantic_expr, old_expr :: List.map (fun arg -> arg.semantic_expr) extra_args)
                      in
                      Ok
                        (typed_ir target.ty
                           (apply "Rrbvec.set"
                              [ target.semantic_expr; index.semantic_expr; value_expr ]))
                  | TFn (_param_tys, ret) when not (Types.equal ret inner) ->
                      Error.error
                        ("cannot update vector element as " ^ source_name ret
                       ^ " because it is already " ^ source_name inner)
                  | TFn _ ->
                      Error.error
                        "update function arguments do not match vector element and extra arguments"
                  | _ -> Error.error "update expects a function")
              | TVector _, _ -> Error.error "update vector index must be int"
              | _ -> Error.error "update expects a map or vector"))
      | _ -> Error.error "update expects collection, key/index, function, and optional arguments"
    
    and compile_select_keys scope env arg_forms =
      match arg_forms with
      | [ target_form; FVector key_forms ] -> (
          let rec parse_keywords acc = function
            | [] -> Ok (List.rev acc)
            | FKeyword keyword :: rest -> parse_keywords (keyword :: acc) rest
            | _ -> Error.error "select-keys expects a vector of keywords"
          in
          match (compile_expr scope env target_form, parse_keywords [] key_forms) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok target, Ok keywords -> (
              match target.ty with
              | TRecord fields | TNamed_record { fields; _ } ->
                  Structural_map.select_keys target fields keywords
              | _ -> Error.error "select-keys expects a map"))
      | [ _; _ ] -> Error.error "select-keys expects a vector of keywords"
      | _ -> Error.error "select-keys expects map and key vector"
    
    and compile_contains scope env arg_forms =
      let compile_collection_contains target value =
        match (target.ty, value.ty) with
        | TSet inner, _ when Types.same_shape inner value.ty ->
            Result.bind (Types.set_module_name inner) (fun set_module ->
                   coerce_set_element inner value
                   |> Result.map (fun value ->
                          typed_ir TBool
                            (Semantic_ir.Apply
                               (Semantic_ir.Ident (set_module ^ ".mem"),
                                [ value; target.semantic_expr ]))))
        | TSet _, _ -> Error.error "contains? value type must match set element type"
        | TVector _, TInt ->
            Ok
              (typed_ir TBool
                 (Semantic_ir.Infix
                    ( "&&",
                      Semantic_ir.Infix (">=", value.semantic_expr, Semantic_ir.Int 0),
                      Semantic_ir.Infix
                        ( "<",
                          value.semantic_expr,
                          Semantic_ir.Apply
                            (Semantic_ir.Ident "Rrbvec.length", [ target.semantic_expr ]) ) )))
        | TVector _, _ -> Error.error "contains? vector index must be int"
        | _ -> Error.error "contains? expects a map, set, or vector"
      in
      match arg_forms with
      | target_form :: FKeyword keyword :: [] -> (
          match compile_expr scope env target_form with
          | Error _ as err -> err
          | Ok target -> (
              match target.ty with
              | TRecord fields | TNamed_record { fields; _ } ->
                  Ok
                    (typed_ir TBool
                       (Semantic_ir.Bool (Option.is_some (find_field keyword fields))))
              | _ ->
                  compile_collection_contains target
                    (typed_ir TKeyword (Semantic_ir.String keyword))))
      | target_form :: value_form :: [] -> (
          match (compile_expr scope env target_form, compile_expr scope env value_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok target, Ok value -> compile_collection_contains target value)
      | _ -> Error.error "contains? expects collection and key"
    
    and compile_keys scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok [ target ] -> (
          match target.ty with
          | TRecord fields | TNamed_record { fields; _ } ->
              Ok
                (typed_ir (TVector TKeyword)
                   (Semantic_ir.Apply
                      ( Semantic_ir.Ident "Rrbvec.of_list",
                        [ Semantic_ir.List
                            (fields
                            |> List.map (fun (field : field) ->
                                   Semantic_ir.String field.keyword)) ] )))
          | _ -> Error.error "keys expects a map")
      | Ok _ -> Error.error "keys expects 1 arguments"
    
    and compile_vals scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok [ target ] -> (
          match target.ty with
          | TRecord [] | TNamed_record { fields = []; _ } ->
              Error.error "vals requires a non-empty map"
          | TRecord (first :: rest) | TNamed_record { fields = first :: rest; _ } ->
              if List.for_all (fun (field : field) -> Types.equal first.ty field.ty) rest then
                Ok
                  (typed_ir (TVector first.ty)
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Rrbvec.of_list",
                          [ Semantic_ir.List
                              ((first :: rest)
                              |> List.map (fun (field : field) ->
                                     Structural_map.field_expr target field)) ] )))
              else Error.error "vals requires all map values to have the same type"
          | _ -> Error.error "vals expects a map")
      | Ok _ -> Error.error "vals expects 1 arguments"
    
  in
  { compile_list; compile_list_star; compile_range; compile_list_of; compile_vector_of; compile_conj; compile_cons; compile_subvec; compile_nth; compile_get; compile_assoc; compile_dissoc; compile_merge; compile_hash_map; compile_update; compile_select_keys; compile_contains; compile_keys; compile_vals }
