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
  compile_find : call;
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

let rec dynamicize_unknown = function
  | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
  | TNullable ty -> TNullable (dynamicize_unknown ty)
  | TOcaml_app ("option", [ ty ]) ->
      TOcaml_app ("option", [ dynamicize_unknown ty ])
  | TOcaml_app (name, arguments) ->
      TOcaml_app (name, List.map dynamicize_unknown arguments)
  | TTuple items -> TTuple (List.map dynamicize_unknown items)
  | TArray ty -> TArray (dynamicize_unknown ty)
  | TRef ty -> TRef (dynamicize_unknown ty)
  | TList ty -> TList (dynamicize_unknown ty)
  | TVector ty -> TVector (dynamicize_unknown ty)
  | TSet ty -> TSet (dynamicize_unknown ty)
  | TSeq ty -> TSeq (dynamicize_unknown ty)
  | TRecord fields ->
      TRecord
        (List.map
           (fun (field : field) ->
             { field with ty = dynamicize_unknown field.ty })
           fields)
  | TFn (parameters, return_ty) ->
      TFn (List.map dynamicize_unknown parameters, dynamicize_unknown return_ty)
  | ty -> ty

let create ~compile_expr ~pack_dynamic_value ~dynamic_unpack =
  let compile_args_for = compile_args_for compile_expr in
  let pack_dynamic_scalar value =
    if
      Types.is_dynamic value.ty
      || Types.equal value.ty TUnknown
      || match value.ty with TVar _ -> true | _ -> false
    then Ok value.semantic_expr
    else
      let constructor =
        match value.ty with
        | TInt -> Some "int"
        | TFloat -> Some "float"
        | TChar -> Some "char"
        | TString -> Some "string"
        | TSymbol -> Some "symbol"
        | TKeyword -> Some "keyword"
        | TBool -> Some "bool"
        | _ -> None
      in
      match constructor with
      | Some constructor ->
          Ok
            (Semantic_ir.Apply
               ( Semantic_ir.Ident ("Lg_runtime.Runtime_dynamic." ^ constructor),
                 [ value.semantic_expr ] ))
      | None -> Error.error "value cannot cross a dynamic boundary"
  in
  let inferred_field_type env keyword =
    let candidates =
      Env.fold
        (fun _ binding candidates ->
          match binding.ty with
          | TNamed_record { fields; _ } -> (
              match find_field keyword fields with
              | Some field
                when not (List.exists (Types.equal field.ty) candidates) ->
                  field.ty :: candidates
              | Some _ | None -> candidates)
          | _ -> candidates)
        env []
    in
    match candidates with [ ty ] -> Some ty | _ -> None
  in
  let resolve_keyword_alias scope env = function
    | FSymbol name as form -> (
        match lookup_binding scope env name with
        | Ok { constant_keyword = Some keyword; _ } -> FKeyword keyword
        | Ok _ | Error _ -> form)
    | form -> form
  in
  let unwrap_protocol_value value =
    let rec unwrap ty expression =
      match Types.protocol_constraint_info ty with
      | None -> (ty, expression)
      | Some (_, _, value_ty) ->
          let expression =
            match Semantic_ir.unlocated expression with
            | Semantic_ir.Ident _ -> expression
            | _ ->
                Semantic_ir.Apply (Semantic_ir.Ident "snd", [ expression ])
          in
          unwrap value_ty expression
    in
    let ty, semantic_expr = unwrap value.ty value.semantic_expr in
    { value with ty; semantic_expr }
  in
  let special_forms : Special_form_elaborator.t =
    Special_form_elaborator.create ~compile_expr
  in
  let compile_map = special_forms.compile_map in
  let compile_function_arg scope env = function
    | FSymbol name -> lookup_function scope env name
    | form -> compile_expr scope env form
  in
    let compile_deftype_method scope env record method_name args =
    match
      lookup_deftype_method scope env record method_name (List.length args)
    with
      | Error _ -> None
    | Ok binding -> (
        match binding.ty with
          | TFn (parameter_tys, return_ty)
            when List.length parameter_tys = List.length args ->
              let rec prepare prepared parameter_tys args =
                match (parameter_tys, args) with
                | [], [] -> Some (List.rev prepared)
                | expected :: parameter_tys, argument :: args ->
                    let prepared_argument =
                      if
                        (Types.equal expected TUnknown
                        || match expected with TVar _ -> true | _ -> false)
                        && Types.equal argument.ty TNil
                        &&
                      match return_ty with
                        | TNullable _ | TOcaml_app ("option", [ _ ]) -> true
                      | _ -> false
                      then Some (argument.semantic_expr, argument.ty)
                      else if
                        Types.is_dynamic expected
                        || Types.equal expected TUnknown
                        || match expected with TVar _ -> true | _ -> false
                      then
                        Option.map
                          (fun expression ->
                          (expression, Types.dynamic_constraint TUnknown))
                          (pack_plain_dynamic_value argument)
                      else Some (argument.semantic_expr, argument.ty)
                    in
                    Option.bind prepared_argument (fun argument ->
                        prepare (argument :: prepared) parameter_tys args)
                | _ -> None
              in
              Option.map
                (fun prepared ->
                  let arguments = List.map fst prepared in
                  let actual_tys = List.map snd prepared in
                  let return_ty =
                    Types.instantiate_type ~templates:parameter_tys
                      ~actuals:actual_tys return_ty
                  in
                  let crossed_dynamic_boundary =
                    List.exists2
                      (fun expected actual ->
                        (Types.equal expected TUnknown
                        || match expected with TVar _ -> true | _ -> false)
                        && Types.is_dynamic actual)
                      parameter_tys actual_tys
                  in
                  let return_ty =
                  if crossed_dynamic_boundary then dynamicize_unknown return_ty
                    else return_ty
                  in
                  typed_ir return_ty
                    (Semantic_ir.Apply
                       (Semantic_ir.Ident binding.ocaml_name, arguments)))
                (prepare [] parameter_tys args)
          | _ -> None)
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
                    let values = List.rev acc in
                    if
                      List.for_all
                        (fun value -> Types.equal first_expr.ty value.ty)
                        values
                    then
                      Ok
                        (typed_ir (TList first_expr.ty)
                           (Semantic_ir.List
                            (List.map (fun value -> value.semantic_expr) values)))
                    else
                      let rec pack packed = function
                        | [] -> Ok (List.rev packed)
                        | value :: rest -> (
                          match
                            pack_dynamic_value env
                              (Types.dynamic_constraint TUnknown)
                              value
                          with
                          | Error (error : Error.t) ->
                              Error
                                {
                                  error with
                                  message =
                                    error.message
                                    ^ " while packing a list element";
                                }
                          | Ok value -> pack (value :: packed) rest)
                      in
                      Result.map
                        (fun values ->
                        typed_ir
                          (Types.dynamic_constraint TUnknown)
                            (apply "Lg_runtime.Runtime_dynamic.list"
                               [ Semantic_ir.List values ]))
                        (pack [] values)
                | form :: rest -> (
                    match compile_expr scope env form with
                    | Error _ as err -> err
                    | Ok expr -> loop (expr :: acc) rest)
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
                    if
                      List.for_all
                        (fun arg -> Types.equal inner arg.ty)
                        prefix_args
                    then
                        let list_expr =
                          match prefix_args with
                          | [] -> final_list_expr
                          | _ ->
                              Semantic_ir.Infix
                                ( "@",
                                  Semantic_ir.List
                                  (List.map
                                     (fun arg -> arg.semantic_expr)
                                     prefix_args),
                                  final_list_expr )
                        in
                        Ok (typed_ir (TList inner) list_expr)
                    else
                      Error.error
                        "list* value type must match final collection element \
                         type")))
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
        match
          (compile_expr scope env start_form, compile_expr scope env end_form)
        with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok start_expr, Ok end_expr ->
            if Types.equal start_expr.ty TInt && Types.equal end_expr.ty TInt
            then
                Ok
                  (finite_range start_expr.semantic_expr end_expr.semantic_expr
                     (Semantic_ir.Int 1))
              else Error.error "range arguments must be int")
    | [ start_form; end_form; step_form ] -> (
          if literal_zero step_form then Error.error "range step cannot be 0"
        else
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
                Types.equal start_expr.ty TInt
                && Types.equal end_expr.ty TInt
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
        | Ok element_ty ->
            Ok (typed_ir (TList element_ty) (Semantic_ir.List [])))
      | _ -> Error.error "list-of expects one type keyword"
    and compile_vector_of arg_forms =
      match arg_forms with
      | [ FKeyword keyword ] -> (
          match Type_annotation.of_keyword keyword with
          | Error _ as err -> err
          | Ok element_ty ->
            Ok
              (typed_ir (TVector element_ty) (Semantic_ir.Ident "Rrbvec.empty"))
        )
      | _ -> Error.error "vector-of expects one type keyword"
    and compile_conj scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok (collection :: values) when values <> [] ->
          let add_value collection value =
            match collection.ty with
            | TList (TUnknown | TVar _) ->
                Ok
                  (typed_ir (TList value.ty)
                     (Semantic_ir.Cons
                        (value.semantic_expr, collection.semantic_expr)))
            | TList inner when Types.equal inner value.ty ->
                Ok
                  (typed_ir collection.ty
                   (Semantic_ir.Cons
                      (value.semantic_expr, collection.semantic_expr)))
          | TList inner when Types.is_dynamic inner && Types.is_dynamic value.ty
            ->
                Ok
                  (typed_ir collection.ty
                     (Semantic_ir.Cons
                        (value.semantic_expr, collection.semantic_expr)))
          | TList _ ->
              Error.error "conj value type must match list element type"
            | TVector (TUnknown | TVar _) ->
                Ok
                  (typed_ir (TVector value.ty)
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Rrbvec.push_back",
                          [ collection.semantic_expr; value.semantic_expr ] )))
            | TVector inner when Types.equal inner value.ty ->
                Ok
                  (typed_ir collection.ty
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Rrbvec.push_back",
                          [ collection.semantic_expr; value.semantic_expr ] )))
            | TVector inner
              when Types.is_dynamic inner && Types.is_dynamic value.ty ->
                Ok
                  (typed_ir collection.ty
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Rrbvec.push_back",
                          [ collection.semantic_expr; value.semantic_expr ] )))
          | TVector _ ->
              Error.error "conj value type must match vector element type"
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
          match Collection_capability.to_seq_expr env collection with
          | Ok (inner, sequence) when Types.same_shape inner value.ty ->
              Ok
                (typed_ir (TSeq inner)
                   (Semantic_ir.Apply
                      ( Semantic_ir.Ident "Seq.cons",
                        [ value.semantic_expr; sequence ] )))
          | Ok _ -> Error.error "cons value type must match sequence element type"
          | Error _ ->
              Error.error
                ("cons expects a value and seqable collection, got "
               ^ Types.source_name collection.ty))
      | Ok _ -> Error.error "cons expects a value and seqable collection"
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
                      [
                        Semantic_ir.Apply
                            ( Semantic_ir.Ident "Rrbvec.subvec",
                            [
                              vector.semantic_expr;
                                start.semantic_expr;
                                Semantic_ir.Apply
                                ( Semantic_ir.Ident "Rrbvec.length",
                                  [ vector.semantic_expr ] );
                            ] );
                      ] )))
          | TVector _, _ -> Error.error "subvec indexes must be int"
          | _ -> Error.error "subvec expects a vector")
      | Ok [ vector; start; stop ] -> (
          match (vector.ty, start.ty, stop.ty) with
          | TVector _, TInt, TInt ->
              Ok
                (typed_ir vector.ty
                   (Semantic_ir.Apply
                      ( Semantic_ir.Ident "Option.get",
                      [
                        Semantic_ir.Apply
                            ( Semantic_ir.Ident "Rrbvec.subvec",
                            [
                              vector.semantic_expr;
                              start.semantic_expr;
                              stop.semantic_expr;
                            ] );
                      ] )))
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
    | Ok [ collection; index; default ] -> (
          if not (Types.equal index.ty TInt) then
            Error.error "nth index must be int"
        else
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
                          [
                            ( Semantic_ir.PConstructor
                                  ("Some", Some (Semantic_ir.PVar "value")),
                                Semantic_ir.Ident "value" );
                            ( Semantic_ir.PConstructor ("None", None),
                              default.semantic_expr );
                          ] ))))
      | Ok _ -> Error.error "nth expects 2 or 3 arguments"
    and compile_get scope env arg_forms =
      let arg_forms =
        match arg_forms with
        | [ target; key ] -> [ target; resolve_keyword_alias scope env key ]
        | forms -> forms
      in
      match arg_forms with
      | [ target_form; FKeyword keyword ] -> (
          match compile_expr scope env target_form with
          | Error _ as err -> err
          | Ok target -> (
              let target = unwrap_protocol_value target in
              match target.ty with
              | TNullable record_ty | TOcaml_app ("option", [ record_ty ]) -> (
                  match record_ty with
                  | TRecord fields | TNamed_record { fields; _ } -> (
                    match find_field keyword fields with
                    | None -> (
                        let record_name = "__lg_optional_record" in
                        let record =
                          typed_ir record_ty (Semantic_ir.Ident record_name)
                        in
                        match
                          Structural_map.extension_get record fields keyword
                        with
                        | None -> Error.error ("unknown field " ^ keyword)
                        | Some lookup ->
                            Ok
                              (typed_ir lookup.ty
                                 (Semantic_ir.Match
                                    ( target.semantic_expr,
                                      [
                                        ( Semantic_ir.PConstructor
                                            ("None", None),
                                          Semantic_ir.Ident
                                            "Lg_runtime.Runtime_dynamic.nil" );
                                        ( Semantic_ir.PConstructor
                                            ( "Some",
                                              Some
                                                (Semantic_ir.PVar record_name)
                                            ),
                                          lookup.semantic_expr );
                                      ] ))))
                    | Some field ->
                      let record_name = "__lg_optional_record" in
                      let record =
                        typed_ir record_ty (Semantic_ir.Ident record_name)
                      in
                        let field_value =
                          Structural_map.field_expr record field
                        in
                      let result_ty, present =
                        match field.ty with
                        | TNullable _ | TOcaml_app ("option", _) ->
                            (field.ty, field_value)
                        | _ ->
                            ( TNullable field.ty,
                              Semantic_ir.Constructor
                                ("Some", Some field_value) )
                      in
                      Ok
                        (typed_ir result_ty
                           (Semantic_ir.Match
                              ( target.semantic_expr,
                                  [
                                    ( Semantic_ir.PConstructor ("None", None),
                                    Semantic_ir.Constructor ("None", None) );
                                  ( Semantic_ir.PConstructor
                                      ( "Some",
                                        Some (Semantic_ir.PVar record_name) ),
                                  present );
                                ] ))))
                  | ty when Types.is_dynamic ty ->
                      let value_name = "__lg_optional_dynamic" in
                      Ok
                        (typed_ir ty
                           (Semantic_ir.Match
                              ( target.semantic_expr,
                                [
                                  ( Semantic_ir.PConstructor ("None", None),
                                    Semantic_ir.Ident
                                      "Lg_runtime.Runtime_dynamic.nil" );
                                  ( Semantic_ir.PConstructor
                                      ( "Some",
                                        Some (Semantic_ir.PVar value_name) ),
                                    apply "Lg_runtime.Runtime_dynamic.get"
                                      [
                                        Semantic_ir.Ident value_name;
                                        apply
                                          "Lg_runtime.Runtime_dynamic.keyword"
                                          [ Semantic_ir.String keyword ];
                                      ] );
                                ] )))
                  | _ -> Error.error "get expects a map")
              | TRecord fields | TNamed_record { fields; nominal = false; _ } -> (
                  match find_field keyword fields with
                  | Some field ->
                      Ok
                        (typed_ir field.ty
                           (Structural_map.field_expr target field))
                  | None -> (
                      match
                        Structural_map.extension_get target fields keyword
                      with
                      | Some result -> Ok result
                      | None -> Error.error ("unknown field " ^ keyword)))
              | TNamed_record { fields; nominal = true; _ } -> (
                  match find_field keyword fields with
                  | Some field ->
                      Ok
                        (typed_ir field.ty
                           (Structural_map.field_expr target field))
                  | None -> (
                      match
                        Structural_map.extension_get target fields keyword
                      with
                      | Some result -> Ok result
                      | None -> (
                          match target.ty with
                          | TNamed_record record -> (
                              let key =
                                typed_ir TKeyword
                                  (Semantic_ir.String keyword)
                              in
                              match
                                match
                                  compile_deftype_method scope env record
                                    "valAt" [ target; key ]
                                with
                                | Some _ as result -> result
                                | None ->
                                    compile_deftype_method scope env record
                                      "-lookup" [ target; key ]
                              with
                              | Some result -> Ok result
                              | None ->
                                  Error.error
                                    ("unknown record field "
                                   ^ Names.keyword_source_name keyword))
                          | _ -> assert false)))
              | ty when Types.is_dynamic ty ->
                  Ok
                    (typed_ir ty
                       (apply "Lg_runtime.Runtime_dynamic.get"
                        [
                          target.semantic_expr;
                            apply "Lg_runtime.Runtime_dynamic.keyword"
                              [ Semantic_ir.String keyword ];
                          ]))
              | TUnknown | TVar _ ->
                  let field_ty =
                  Option.value
                    (inferred_field_type env keyword)
                      ~default:TUnknown
                  in
                  Ok
                    (typed_ir field_ty
                       (Semantic_ir.Field
                        ( target.semantic_expr,
                          Names.keyword_to_ocaml_name keyword )))
              | TOcaml_app ("Lg_runtime.Runtime_map.t", [ _key_ty; value_ty ]) ->
                  Ok
                    (typed_ir (TNullable value_ty)
                       (apply "Lg_runtime.Runtime_map.get_option"
                        [ target.semantic_expr; Semantic_ir.String keyword ]))
              | ty when is_ocaml_owned_type ty ->
                  Ok
                    (typed_ir TUnknown
                       (Semantic_ir.Field
                        ( target.semantic_expr,
                          Names.keyword_to_ocaml_name keyword )))
              | _ -> Error.error "get expects a map"))
      | [ target_form; index_form ] -> (
        match
          (compile_expr scope env target_form, compile_expr scope env index_form)
        with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok target, Ok index -> (
              let target = unwrap_protocol_value target in
              match (target.ty, index.ty) with
              | TVector inner, TInt ->
                  Ok
                    (typed_ir inner
                     (apply "Rrbvec.nth"
                        [ target.semantic_expr; index.semantic_expr ]))
              | TVector _, _ -> Error.error "get vector index must be int"
              | TNamed_record record, _ -> (
                let concrete_fields =
                  record.fields
                  |> List.filter (fun (field : field) ->
                      (not (Types.is_record_extension_field field))
                      && not
                        (Types.is_dynamic field.ty
                        || Types.equal field.ty TUnknown
                        || Types.equal field.ty TMap_keys
                        || match field.ty with TVar _ -> true | _ -> false))
                in
                let groups =
                  List.fold_left
                    (fun groups (field : field) ->
                      match
                        List.find_opt
                          (fun (ty, _) -> Types.equal ty field.ty)
                          groups
                      with
                      | None -> (field.ty, [ field ]) :: groups
                      | Some (ty, fields) ->
                          (ty, field :: fields)
                          :: List.filter
                               (fun (candidate, _) ->
                                 not (Types.equal candidate ty))
                               groups)
                    [] concrete_fields
                in
                let keyed_projection =
                  match (groups, index.ty) with
                  | [ (result_ty, fields) ], TKeyword ->
                      Some (result_ty, fields, index.semantic_expr)
                  | [ (result_ty, fields) ], ty when Types.is_dynamic ty ->
                      Some
                        ( result_ty,
                          fields,
                          apply "Lg_runtime.Runtime_dynamic.as_keyword"
                            [ index.semantic_expr ] )
                  | _ -> None
                in
                match keyed_projection with
                | Some (result_ty, fields, key) ->
                    let cases =
                      List.map
                        (fun (field : field) ->
                          ( Semantic_ir.PString field.keyword,
                            Structural_map.field_expr target field ))
                        fields
                      @ [
                          ( Semantic_ir.PAny,
                            apply "invalid_arg"
                              [
                                Semantic_ir.String
                                  "record key does not select a compatible \
                                   field";
                              ] );
                        ]
                    in
                    Ok (typed_ir result_ty (Semantic_ir.Match (key, cases)))
                | None -> (
                    match
                  match
                       compile_deftype_method scope env record "valAt"
                         [ target; index ]
                     with
                    | Some _ as result -> result
                    | None ->
                        compile_deftype_method scope env record "-lookup"
                            [ target; index ]
                  with
                  | Some result -> Ok result
                    | None -> Error.error "get key must be a keyword"))
              | _ -> (
                  match Types.dynamic_map_types target.ty with
                  | Some (key_ty, value_ty)
                    when Types.assignable ~policy:Host_boundary ~expected:key_ty
                           ~actual:index.ty ->
                      let key =
                        if
                          Types.is_dynamic key_ty
                          && not (Types.is_dynamic index.ty)
                        then pack_dynamic_value env key_ty index
                        else Ok index.semantic_expr
                      in
                      Result.map
                        (fun key ->
                          typed_ir (TNullable value_ty)
                            (apply "Lg_runtime.Runtime_map.get_option"
                               [ target.semantic_expr; key ]))
                        key
                  | None
                    when Types.equal target.ty TUnknown
                       || match target.ty with TVar _ -> true | _ -> false ->
                      Ok
                        (typed_ir (TNullable TUnknown)
                           (apply "Lg_runtime.Runtime_map.get_option"
                              [ target.semantic_expr; index.semantic_expr ]))
                  | _ ->
                      Error.error
                        ("get key type " ^ source_name index.ty
                       ^ " is not supported for " ^ source_name target.ty))))
      | [ target_form; FKeyword keyword; default_form ] -> (
          match
          ( compile_expr scope env target_form,
            compile_expr scope env default_form )
          with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok target, Ok default -> (
              let target = unwrap_protocol_value target in
              match target.ty with
              | TRecord fields | TNamed_record { fields; nominal = false; _ } -> (
                  match find_field keyword fields with
                  | Some field when Types.equal field.ty default.ty ->
                      Ok
                        (typed_ir field.ty
                           (Structural_map.field_expr target field))
                  | Some field ->
                      Error.error
                      ("get default for " ^ keyword ^ " must be "
                     ^ source_name field.ty)
                  | None -> Ok default)
              | TNamed_record record -> (
                  match find_field keyword record.fields with
                  | Some field when Types.equal field.ty default.ty ->
                      Ok
                        (typed_ir field.ty
                           (Structural_map.field_expr target field))
                  | Some _ ->
                      Error.error
                        ("get default for " ^ keyword ^ " has incompatible type")
                | None -> (
                      let key = typed_ir TKeyword (Semantic_ir.String keyword) in
                    match
                      match
                            compile_deftype_method scope env record "valAt"
                              [ target; key; default ]
                          with
                          | Some _ as result -> result
                          | None ->
                              compile_deftype_method scope env record "-lookup"
                            [ target; key; default ]
                       with
                      | Some result -> Ok result
                      | None -> Ok default))
              | _ -> (
                  match Types.dynamic_map_types target.ty with
                | Some (_key_ty, value_ty) when default_form = FSymbol "nil" ->
                      Ok
                        (typed_ir (TNullable value_ty)
                           (apply "Lg_runtime.Runtime_map.get_option"
                            [ target.semantic_expr; Semantic_ir.String keyword ]))
                  | Some (_key_ty, value_ty)
                  when Types.assignable ~policy:Host_boundary ~expected:value_ty
                         ~actual:default.ty ->
                      Ok
                        (typed_ir value_ty
                           (apply "Lg_runtime.Runtime_map.get_default"
                            [
                              target.semantic_expr;
                                Semantic_ir.String keyword;
                                default.semantic_expr;
                              ]))
                  | _ -> Error.error "get expects a map")))
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
              let target = unwrap_protocol_value target in
              match (target.ty, index.ty) with
              | TVector inner, TInt when Types.equal inner default.ty ->
                  Ok
                    (typed_ir inner
                       (Semantic_ir.Match
                        ( apply "Rrbvec.nth_opt"
                            [ target.semantic_expr; index.semantic_expr ],
                          [
                            ( Semantic_ir.PConstructor
                                ("Some", Some (Semantic_ir.PVar "value")),
                                Semantic_ir.Ident "value" );
                            ( Semantic_ir.PConstructor ("None", None),
                              default.semantic_expr );
                          ] )))
            | TVector _, TInt ->
                Error.error "get default for vector must match element type"
              | TVector _, _ -> Error.error "get vector index must be int"
              | TNamed_record record, _ -> (
                  match
                  match
                       compile_deftype_method scope env record "valAt"
                         [ target; index; default ]
                     with
                    | Some _ as result -> result
                    | None ->
                        compile_deftype_method scope env record "-lookup"
                        [ target; index; default ]
                  with
                  | Some result -> Ok result
                  | None -> Error.error "get key must be a keyword")
              | _ -> (
                  match Types.dynamic_map_types target.ty with
                  | Some (key_ty, value_ty)
                    when default_form = FSymbol "nil"
                         && Types.assignable ~policy:Host_boundary
                              ~expected:key_ty ~actual:index.ty ->
                      Ok
                        (typed_ir (TNullable value_ty)
                           (apply "Lg_runtime.Runtime_map.get_option"
                              [ target.semantic_expr; index.semantic_expr ]))
                  | Some (key_ty, value_ty)
                    when Types.assignable ~policy:Host_boundary ~expected:key_ty
                           ~actual:index.ty
                         && Types.assignable ~policy:Host_boundary
                              ~expected:value_ty ~actual:default.ty ->
                      Ok
                        (typed_ir value_ty
                           (apply "Lg_runtime.Runtime_map.get_default"
                            [
                              target.semantic_expr;
                                index.semantic_expr;
                                default.semantic_expr;
                              ]))
                  | None
                    when Types.equal target.ty TUnknown
                       || match target.ty with TVar _ -> true | _ -> false ->
                      if default_form = FSymbol "nil" then
                        Ok
                          (typed_ir (TNullable TUnknown)
                             (apply "Lg_runtime.Runtime_map.get_option"
                                [ target.semantic_expr; index.semantic_expr ]))
                      else
                        Ok
                          (typed_ir default.ty
                             (apply "Lg_runtime.Runtime_map.get_default"
                              [
                                target.semantic_expr;
                                  index.semantic_expr;
                                  default.semantic_expr;
                                ]))
                  | _ ->
                      Error.error
                        ("get key type " ^ source_name index.ty
                       ^ " is not supported for " ^ source_name target.ty))))
      | _ -> Error.error "get expects 2 or 3 arguments"
    and compile_find scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok [ target; key ] -> (
          match Types.dynamic_map_types target.ty with
          | Some (key_ty, value_ty)
            when Types.assignable ~policy:Host_boundary ~expected:key_ty
                   ~actual:key.ty ->
              Ok
                (typed_ir
                   (TOcaml_app ("option", [ TTuple [ key_ty; value_ty ] ]))
                   (apply "Lg_runtime.Runtime_map.find"
                      [ target.semantic_expr; key.semantic_expr ]))
          | None
            when Types.equal target.ty TUnknown
               || match target.ty with TVar _ -> true | _ -> false ->
              Ok
                (typed_ir
                 (TOcaml_app ("option", [ TTuple [ key.ty; TUnknown ] ]))
                   (apply "Lg_runtime.Runtime_map.find"
                      [ target.semantic_expr; key.semantic_expr ]))
          | _ -> Error.error "find expects a map and key")
      | Ok _ -> Error.error "find expects 2 arguments"
    and compile_assoc scope env arg_forms =
      match arg_forms with
    | target_form :: pair_forms -> (
          let rec resolve_pair_keys = function
            | key :: value :: rest ->
                resolve_keyword_alias scope env key
                :: value :: resolve_pair_keys rest
            | forms -> forms
          in
          let pair_forms = resolve_pair_keys pair_forms in
          let rec compile_record_pairs acc = function
            | [] -> Ok (List.rev acc)
            | FKeyword keyword :: value_form :: rest -> (
                match compile_expr scope env value_form with
                | Error _ as err -> err
                | Ok value -> compile_record_pairs ((keyword, value) :: acc) rest)
            | _ -> Error.error "assoc expects map followed by keyword/value pairs"
          in
          let adapt_dynamic_fields fields pairs =
            let rec adapt adapted = function
              | [] -> Ok (List.rev adapted)
              | (keyword, value) :: rest -> (
                  match find_field keyword fields with
                  | Some field
                    when Types.is_dynamic field.ty
                         && not (Types.is_dynamic value.ty) ->
                      Result.bind (pack_dynamic_value env field.ty value)
                        (fun expression ->
                          adapt
                            ((keyword, typed_ir field.ty expression) :: adapted)
                            rest)
                  | Some _ | None ->
                      adapt ((keyword, value) :: adapted) rest)
            in
            adapt [] pairs
          in
          let rec assoc_record_pairs target = function
            | [] -> Ok target
            | (keyword, value) :: rest -> (
                let fields =
                  match target.ty with
                  | TRecord fields | TNamed_record { fields; _ } -> fields
                  | _ -> []
                in
                match find_field keyword fields with
                | None
                  when Option.is_some
                         (Types.find_record_extension_field fields) ->
                    let dynamic = Types.dynamic_constraint TUnknown in
                    Result.bind (pack_dynamic_value env dynamic value)
                      (fun value ->
                        match
                          Structural_map.extension_assoc target fields keyword
                            value
                        with
                        | Some target -> assoc_record_pairs target rest
                        | None -> assert false)
                | _ ->
                    Result.bind (Structural_map.assoc target fields keyword value)
                      (fun target -> assoc_record_pairs target rest))
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
              | Ok index, Ok value ->
                  compile_vector_pairs ((index, value) :: acc) rest)
          | _ ->
              Error.error "assoc expects collection followed by key/value pairs"
        in
        let compile_dynamic_pairs ?result_ty target_expr pairs =
          let dynamic = Types.dynamic_constraint TUnknown in
          let result_ty = Option.value result_ty ~default:dynamic in
          let rec pack_pairs packed = function
            | [] -> Ok (List.rev packed)
            | (key, value) :: rest ->
                Result.bind (pack_dynamic_value env dynamic key) (fun key ->
                    Result.bind (pack_dynamic_value env dynamic value)
                      (fun value -> pack_pairs ((key, value) :: packed) rest))
          in
          Result.map
            (fun pairs ->
              let expression =
                List.fold_left
                  (fun map (key, value) ->
                    apply "Lg_runtime.Runtime_dynamic.assoc" [ map; key; value ])
                  target_expr pairs
              in
              typed_ir result_ty expression)
            (pack_pairs [] pairs)
          in
        match compile_expr scope env target_form with
          | Error _ as err -> err
          | Ok target -> (
              let target = unwrap_protocol_value target in
              if pair_forms = [] || List.length pair_forms mod 2 <> 0 then
                match target.ty with
                | TRecord _ | TNamed_record _ ->
                  Error.error
                    "assoc expects map followed by keyword/value pairs"
              | TVector _ ->
                  Error.error
                    "assoc expects vector followed by index/value pairs"
              | _ ->
                  Error.error
                    "assoc expects collection followed by key/value pairs"
              else
                match target.ty with
                | TNamed_record ({ nominal = true; _ } as record) -> (
                    match compile_vector_pairs [] pair_forms with
                    | Error _ as err -> err
                    | Ok [] -> assert false
                  | Ok ((first_key, first_value) :: remaining_pairs) -> (
                        let lookup_method current key value =
                          match
                            compile_deftype_method scope env record "assoc"
                              [ current; key; value ]
                          with
                          | Some _ as result -> result
                          | None ->
                              compile_deftype_method scope env record "-assoc"
                                [ current; key; value ]
                        in
                        let rec apply_pairs current = function
                          | [] -> Ok current
                          | (key, value) :: rest -> (
                              match lookup_method current key value with
                              | None ->
                                  Error.error
                                    "assoc expects an associative deftype"
                              | Some updated ->
                                apply_pairs { updated with ty = target.ty } rest
                            )
                        in
                      match lookup_method target first_key first_value with
                        | Some updated ->
                          apply_pairs
                            { updated with ty = target.ty }
                              remaining_pairs
                        | None -> (
                            match compile_record_pairs [] pair_forms with
                            | Error _ as err -> err
                            | Ok pairs ->
                                Result.bind
                                  (adapt_dynamic_fields record.fields pairs)
                                  (assoc_record_pairs target))
                      )
                  )
                | TRecord fields | TNamed_record { fields; _ } -> (
                    match compile_record_pairs [] pair_forms with
                    | Error _ as err -> err
                    | Ok pairs ->
                        Result.bind (adapt_dynamic_fields fields pairs)
                          (assoc_record_pairs target))
                | TVector _ -> (
                    match compile_vector_pairs [] pair_forms with
                    | Error _ as err -> err
                  | Ok pairs -> (
                        let rec apply_pairs vector_ty expr = function
                          | [] -> Ok (vector_ty, expr)
                          | (index, value) :: rest ->
                              let inner =
                                match vector_ty with
                                | TVector inner -> inner
                                | _ -> assert false
                              in
                              if not (Types.equal index.ty TInt) then
                                Error.error "assoc vector index must be int"
                              else if
                                not
                                  (Types.assignable ~policy:Host_boundary
                                     ~expected:inner ~actual:value.ty)
                              then
                                Error.error
                                  "assoc vector value must match element type"
                              else
                                let vector_ty =
                                  if Types.equal inner value.ty then vector_ty
                                  else TVector value.ty
                                in
                                apply_pairs vector_ty
                                  (apply "Rrbvec.set"
                                   [
                                     expr;
                                     index.semantic_expr;
                                     value.semantic_expr;
                                   ])
                                  rest
                        in
                      match apply_pairs target.ty target.semantic_expr pairs with
                        | Error _ as err -> err
                        | Ok (result_ty, expr) -> Ok (typed_ir result_ty expr)))
              | target_ty
                when Types.is_dynamic target_ty
                     || Types.equal target_ty TUnknown
                     || match target_ty with TVar _ -> true | _ -> false -> (
                  match compile_vector_pairs [] pair_forms with
                  | Error _ as err -> err
                  | Ok [] -> assert false
                  | Ok pairs ->
                      if Types.is_dynamic target_ty then
                        compile_dynamic_pairs ~result_ty:target_ty
                          target.semantic_expr pairs
                      else compile_dynamic_pairs target.semantic_expr pairs
                  )
              | (TNullable (TRecord fields)
                | TOcaml_app ("option", [ TRecord fields ])) -> (
                  match compile_record_pairs [] pair_forms with
                  | Error _ as err -> err
                  | Ok pairs ->
                      Result.bind (adapt_dynamic_fields fields pairs)
                        (fun pairs ->
                          let initial_fields =
                            fields
                            |> List.map (fun (field : field) ->
                                   Option.map
                                     (fun (_, value) ->
                                       (field.ocaml_name, value.semantic_expr))
                                     (List.find_opt
                                        (fun (keyword, _) ->
                                          keyword = field.keyword)
                                        pairs))
                          in
                          if List.exists Option.is_none initial_fields then
                            Error.error
                              "assoc cannot create a nullable record without all fields"
                          else
                            let initial =
                              initial_fields |> List.filter_map Fun.id
                            in
                            let value_name = "__lg_assoc_record" in
                            let target =
                              typed_ir (TRecord fields)
                                (Semantic_ir.Match
                                   ( target.semantic_expr,
                                     [
                                       ( Semantic_ir.PConstructor
                                           ("None", None),
                                         Semantic_ir.Record (initial, None) );
                                       ( Semantic_ir.PConstructor
                                           ( "Some",
                                             Some
                                               (Semantic_ir.PVar value_name) ),
                                         Semantic_ir.Ident value_name );
                                     ] ))
                            in
                            Structural_map.assoc_many target pairs))
              | (TNullable inner | TOcaml_app ("option", [ inner ]))
                when Option.is_some (Types.dynamic_map_types inner) -> (
                  match
                    ( Types.dynamic_map_types inner,
                      compile_vector_pairs [] pair_forms )
                  with
                  | _, (Error _ as err) -> err
                  | _, Ok [] -> assert false
                  | None, Ok _ -> assert false
                  | Some (key_ty, value_ty), Ok pairs ->
                      if
                        not
                          (List.for_all
                             (fun (key, value) ->
                               Types.assignable ~policy:Host_boundary
                                 ~expected:key_ty ~actual:key.ty
                               && Types.assignable ~policy:Host_boundary
                                    ~expected:value_ty ~actual:value.ty)
                             pairs)
                      then
                        Error.error
                          "assoc key/value types do not match the nullable map"
                      else
                        let value_name = "__lg_assoc_map" in
                        let map =
                          Semantic_ir.Match
                            ( target.semantic_expr,
                              [
                                ( Semantic_ir.PConstructor ("None", None),
                                  Semantic_ir.Ident
                                    "Lg_runtime.Runtime_map.empty" );
                                ( Semantic_ir.PConstructor
                                    ("Some", Some (Semantic_ir.PVar value_name)),
                                  Semantic_ir.Ident value_name );
                              ] )
                        in
                        let expression =
                          List.fold_left
                            (fun map (key, value) ->
                              apply "Lg_runtime.Runtime_map.assoc"
                                [ map; key.semantic_expr; value.semantic_expr ])
                            map pairs
                        in
                        Ok (typed_ir inner expression))
              | (TNullable inner | TOcaml_app ("option", [ inner ]))
                when Types.is_dynamic inner || Types.equal inner TUnknown
                     || match inner with TVar _ -> true | _ -> false -> (
                  match compile_vector_pairs [] pair_forms with
                  | Error _ as err -> err
                  | Ok [] -> assert false
                  | Ok pairs ->
                      let value_name = "__lg_assoc_value" in
                      let target_expr =
                        Semantic_ir.Match
                          ( target.semantic_expr,
                            [
                              ( Semantic_ir.PConstructor ("None", None),
                                Semantic_ir.Ident
                                  "Lg_runtime.Runtime_dynamic.nil" );
                              ( Semantic_ir.PConstructor
                                  ("Some", Some (Semantic_ir.PVar value_name)),
                                Semantic_ir.Ident value_name );
                            ] )
                      in
                      if Types.is_dynamic inner then
                        compile_dynamic_pairs ~result_ty:inner target_expr pairs
                      else compile_dynamic_pairs target_expr pairs)
                | target_ty -> (
                    let dynamic_target =
                      Option.is_some (Types.dynamic_map_types target_ty)
                    in
                    if not dynamic_target then
                    Error.error
                      ("assoc expects a map or vector, got "
                      ^ Types.source_name target_ty)
                    else
                      match compile_vector_pairs [] pair_forms with
                      | Error _ as err -> err
                      | Ok [] -> assert false
                      | Ok ((first_key, first_value) :: _ as pairs) ->
                          let expression =
                            List.fold_left
                              (fun map (key, value) ->
                                apply "Lg_runtime.Runtime_map.assoc"
                                  [ map; key.semantic_expr; value.semantic_expr ])
                              target.semantic_expr pairs
                          in
                          Ok
                            (typed_ir
                               (Types.dynamic_map first_key.ty first_value.ty)
                               expression))))
      | _ -> Error.error "assoc expects collection followed by key/value pairs"
    and compile_dissoc scope env arg_forms =
      match arg_forms with
      | target_form :: key_forms -> (
          match compile_expr scope env target_form with
          | Error _ as err -> err
          | Ok target -> (
              match target.ty with
              | TRecord _ | TNamed_record _ ->
                  let rec parse_keywords acc = function
                    | [] -> Ok (List.rev acc)
                    | FKeyword keyword :: rest ->
                        parse_keywords (keyword :: acc) rest
                  | _ -> Error.error "dissoc expects map followed by keywords"
                  in
                  let rec dissoc_keywords target = function
                    | [] -> Ok target
                    | keyword :: rest ->
                        let fields =
                          match target.ty with
                          | TRecord fields | TNamed_record { fields; _ } -> fields
                          | _ -> []
                        in
                        let result =
                          match find_field keyword fields with
                          | None -> (
                              match
                                Structural_map.extension_dissoc target fields
                                  keyword
                              with
                              | Some target -> Ok target
                              | None ->
                                  Error.error
                                    ("cannot dissoc unknown field " ^ keyword))
                          | Some _ -> Structural_map.dissoc target fields keyword
                        in
                        Result.bind result (fun target ->
                            dissoc_keywords target rest)
                  in
                  Result.bind (parse_keywords [] key_forms)
                    (dissoc_keywords target)
              | target_ty
                when Option.is_some (Types.dynamic_map_types target_ty)
                     || Types.equal target_ty TUnknown
                   || match target_ty with TVar _ -> true | _ -> false ->
                  Result.map
                    (fun keys ->
                      typed_ir target.ty
                        (List.fold_left
                           (fun map key ->
                             apply "Lg_runtime.Runtime_map.dissoc"
                               [ map; key.semantic_expr ])
                           target.semantic_expr keys))
                    (compile_args_for scope env key_forms)
              | target_ty when Types.is_dynamic target_ty ->
                Result.bind (compile_args_for scope env key_forms) (fun keys ->
                      let rec pack packed = function
                        | [] -> Ok (List.rev packed)
                        | key :: rest -> (
                            match pack_dynamic_scalar key with
                            | Error _ as error -> error
                            | Ok key -> pack (key :: packed) rest)
                      in
                      Result.map
                        (fun keys ->
                          typed_ir target.ty
                            (List.fold_left
                               (fun map key ->
                                 apply "Lg_runtime.Runtime_dynamic.dissoc"
                                   [ map; key ])
                               target.semantic_expr keys))
                        (pack [] keys))
              | _ -> Error.error "dissoc expects a map"))
      | _ -> Error.error "dissoc expects map followed by keywords"
    and compile_merge scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
    | Ok maps
      when List.for_all
             (fun map ->
               match map.ty with
               | TRecord _ | TNamed_record _ -> true
               | _ -> false)
             maps ->
        Structural_map.merge maps
    | Ok maps ->
        let dynamic = Types.dynamic_constraint TUnknown in
        let rec pack packed = function
          | [] -> Ok (List.rev packed)
          | map :: rest ->
              Result.bind (pack_dynamic_value env dynamic map) (fun map ->
                  pack (map :: packed) rest)
        in
        Result.map
          (fun maps ->
            typed_ir dynamic
              (apply "Lg_runtime.Runtime_dynamic.merge"
                 [ Semantic_ir.List maps ]))
          (pack [] maps)
    and compile_hash_map scope env arg_forms =
      let rec parse_pairs acc = function
        | [] -> Ok (List.rev acc)
      | key_form :: value_form :: rest ->
          parse_pairs ((key_form, value_form) :: acc) rest
        | _ -> Error.error "hash-map expects keyword/value pairs"
      in
      if arg_forms = [] then
        Ok
        (typed_ir
           (Types.dynamic_map TUnknown TUnknown)
             (Semantic_ir.Ident "Lg_runtime.Runtime_map.empty"))
      else if List.length arg_forms mod 2 <> 0 then
        Error.error "hash-map expects keyword/value pairs"
      else
        match parse_pairs [] arg_forms with
        | Error _ as err -> err
      | Ok pairs
        when List.for_all
               (fun (key, _value) ->
                 match key with FKeyword _ -> true | _ -> false)
               pairs ->
          compile_map scope env pairs
      | Ok _ ->
          let dynamic = Types.dynamic_constraint TUnknown in
          Result.bind (compile_args_for scope env arg_forms) (fun arguments ->
              let rec pack_entries entries = function
                | [] -> Ok (List.rev entries)
                | key :: value :: rest ->
                    Result.bind (pack_dynamic_value env dynamic key) (fun key ->
                        Result.bind (pack_dynamic_value env dynamic value)
                          (fun value ->
                            pack_entries
                              (Semantic_ir.Tuple [ key; value ] :: entries)
                              rest))
                | [ _ ] -> Error.error "hash-map expects keyword/value pairs"
              in
              Result.map
                (fun entries ->
                  typed_ir dynamic
                    (Semantic_ir.Apply
                       ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.map",
                         [ Semantic_ir.List entries ] )))
                (pack_entries [] arguments))
    and compile_update scope env arg_forms =
    let nested_update_value = "__lg_nested_update_value" in
    let arg_forms =
      match arg_forms with
      | target :: key :: FSymbol "update" :: nested_args ->
          let updater =
            FList
              [
                FSymbol "fn";
                FVector [ FSymbol nested_update_value ];
                FList
                  (FSymbol "update" :: FSymbol nested_update_value
                 :: nested_args);
              ]
          in
          [ target; key; updater ]
      | _ -> arg_forms
    in
    let dynamic = Types.dynamic_constraint TUnknown in
    let prepare expected argument =
      if Types.is_dynamic expected then pack_dynamic_value env expected argument
      else if Types.is_dynamic argument.ty then
        dynamic_unpack env expected argument.semantic_expr
      else if
        Types.assignable ~policy:Host_boundary ~expected ~actual:argument.ty
      then Ok argument.semantic_expr
      else
        Error.error
          ("update called with incompatible arguments: expected "
         ^ Types.source_name expected ^ ", got "
          ^ Types.source_name argument.ty)
    in
    let instantiate_updater param_tys return_ty extra_args =
      let templates = drop 1 param_tys in
      let actuals = List.map (fun argument -> argument.ty) extra_args in
      let instantiate ty = Types.instantiate_type ~templates ~actuals ty in
      (List.map instantiate param_tys, instantiate return_ty)
    in
    let rec prepare_updater_arguments prepared expected arguments =
      match (expected, arguments) with
      | [], [] -> Ok (List.rev prepared)
      | expected :: expected_rest, argument :: argument_rest ->
          Result.bind (prepare expected argument) (fun argument ->
              prepare_updater_arguments (argument :: prepared) expected_rest
                argument_rest)
      | _ -> Error.error "update function argument count mismatch"
    in
    let compile_dynamic target index fn extra_args =
      Result.bind (pack_dynamic_value env dynamic index) (fun key ->
          let old_value =
            typed_ir dynamic
              (apply "Lg_runtime.Runtime_dynamic.get"
                 [ target.semantic_expr; key ])
          in
          match fn.ty with
          | TFn (parameter_tys, return_ty)
            when List.length parameter_tys = List.length extra_args + 1 ->
              let parameter_tys, return_ty =
                instantiate_updater parameter_tys return_ty extra_args
              in
              let arguments = old_value :: extra_args in
              Result.bind
                (prepare_updater_arguments [] parameter_tys arguments)
                (fun arguments ->
                  let result =
                    typed_ir return_ty
                      (Semantic_ir.Apply (fn.semantic_expr, arguments))
                  in
                  Result.map
                    (fun result ->
                      typed_ir target.ty
                        (apply "Lg_runtime.Runtime_dynamic.assoc"
                           [ target.semantic_expr; key; result ]))
                    (pack_dynamic_value env dynamic result))
          | _ -> Error.error "update expects a function")
    in
    let compile_extension target fields keyword fn extra_args =
      match Types.find_record_extension_field fields with
      | None -> Error.error ("cannot update unknown field " ^ keyword)
      | Some extension_field -> (
          let old_value =
            typed_ir dynamic
              (apply "Lg_runtime.Runtime_map.get_default"
                 [
                   Structural_map.field_expr target extension_field;
                   Semantic_ir.String keyword;
                   Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.nil";
                 ])
          in
          match fn.ty with
          | TFn (parameter_tys, return_ty)
            when List.length parameter_tys = List.length extra_args + 1 ->
              let parameter_tys, return_ty =
                instantiate_updater parameter_tys return_ty extra_args
              in
              Result.bind
                (prepare_updater_arguments [] parameter_tys
                   (old_value :: extra_args))
                (fun arguments ->
                  let result =
                    typed_ir return_ty
                      (Semantic_ir.Apply (fn.semantic_expr, arguments))
                  in
                  Result.bind (pack_dynamic_value env dynamic result)
                    (fun result ->
                      match
                        Structural_map.extension_assoc target fields keyword
                          result
                      with
                      | Some target -> Ok target
                      | None -> assert false))
          | _ -> Error.error "update expects a function")
    in
      match arg_forms with
      | target_form :: FKeyword keyword :: fn_form :: extra_forms -> (
          let with_context context = function
            | Ok _ as result -> result
            | Error (error : Error.t) ->
                Error { error with message = error.message ^ " " ^ context }
          in
          match
            ( compile_expr scope env target_form
              |> with_context ("while compiling update target " ^ keyword),
              compile_function_arg scope env fn_form
              |> with_context ("while compiling updater for " ^ keyword),
              compile_args_for scope env extra_forms
              |> with_context ("while compiling update arguments for " ^ keyword)
            )
          with
          | (Error _ as err), _, _ -> err
          | _, (Error _ as err), _ -> err
          | _, _, (Error _ as err) -> err
          | Ok target, Ok fn, Ok extra_args -> (
            let target = unwrap_protocol_value target in
            let updater_row_type =
              match fn_form with
              | FSymbol name -> (
                  match lookup_binding scope env name with
                  | Ok binding ->
                      List.nth_opt binding.row_param_types 0 |> Option.join
                  | Error _ -> None)
              | _ -> None
            in
              match target.ty with
              | TRecord fields | TNamed_record { fields; _ } -> (
                  match find_field keyword fields with
                  | None ->
                      compile_extension target fields keyword fn extra_args
                  | Some field -> (
                      match fn.ty with
                      | TFn (param_tys, ret)
                      when List.length param_tys = List.length extra_args + 1 ->
                        let param_tys, ret =
                          instantiate_updater param_tys ret extra_args
                        in
                        let param_tys, ret =
                          if Types.is_dynamic field.ty
                          then
                            ( List.map dynamicize_unknown param_tys,
                              dynamicize_unknown ret )
                          else (param_tys, ret)
                          in
                        if
                          (not (Types.is_dynamic field.ty))
                          && not (Types.equal ret field.ty)
                        then
                          Error.error
                            (Printf.sprintf
                               "cannot update %s as %s because it is already %s"
                               keyword (source_name ret) (source_name field.ty))
                        else
                          let old_value =
                            typed_ir field.ty
                              (Structural_map.field_expr target field)
                          in
                          let prepare_old expected argument =
                            match (expected, argument.ty, updater_row_type) with
                            | TRecord row_fields, dynamic_ty, Some type_name
                              when Types.is_dynamic dynamic_ty ->
                                let rec unpack_fields unpacked = function
                                  | [] -> Ok (List.rev unpacked)
                                  | (row_field : field) :: rest ->
                                      let value =
                                        apply "Lg_runtime.Runtime_dynamic.get"
                                          [
                                            argument.semantic_expr;
                                            apply
                                              "Lg_runtime.Runtime_dynamic.keyword"
                                              [
                                                Semantic_ir.String
                                                  row_field.keyword;
                                              ];
                                          ]
                                      in
                                      Result.bind
                                        (dynamic_unpack env row_field.ty value)
                                        (fun value ->
                                          unpack_fields
                                            ((row_field.ocaml_name, value)
                                            :: unpacked)
                                            rest)
                                in
                                Result.map
                                  (fun fields ->
                                    Semantic_ir.Record (fields, Some type_name))
                                  (unpack_fields [] row_fields)
                            | _ -> prepare expected argument
                          in
                          let rec prepare_all prepared expected arguments =
                            match (expected, arguments) with
                            | [], [] -> Ok (List.rev prepared)
                            | ( expected :: expected_rest,
                                argument :: argument_rest ) -> (
                                let prepare_argument =
                                  if prepared = [] then prepare_old else prepare
                                in
                                match prepare_argument expected argument with
                                | Error _ when prepared <> [] ->
                          Error.error
                                      "update function arguments do not match \
                                       field and extra arguments"
                                | Error _ as error -> error
                                | Ok argument ->
                                    prepare_all (argument :: prepared)
                                      expected_rest argument_rest)
                            | _ ->
                                Error.error
                                  "update function argument count mismatch"
                          in
                          Result.bind
                            (prepare_all [] param_tys (old_value :: extra_args))
                            (fun arguments ->
                              let result =
                                typed_ir ret
                                  (Semantic_ir.Apply
                                     (fn.semantic_expr, arguments))
                              in
                              let stored =
                                if Types.is_dynamic field.ty then
                                  pack_dynamic_value env field.ty result
                                else if Types.equal ret field.ty then
                                  Ok result.semantic_expr
                                else
                                  Error.error
                                    (Printf.sprintf
                                       "cannot update %s as %s because it is \
                                        already %s"
                                       keyword (source_name ret)
                                       (source_name field.ty))
                              in
                              Result.bind stored (fun stored ->
                                  Structural_map.update_value target fields
                                    keyword field.ty stored))
                    | TFn _ ->
                        Error.error "update function argument count mismatch"
                      | _ -> Error.error "update expects a function"))
            | target_ty when Types.is_dynamic target_ty ->
                let index = typed_ir TKeyword (Semantic_ir.String keyword) in
                compile_dynamic target index fn extra_args
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
              let target = unwrap_protocol_value target in
              match (target.ty, index.ty) with
              | TVector inner, TInt -> (
                  match fn.ty with
                  | TFn (param_tys, ret)
                  when List.length param_tys = List.length extra_args + 1 ->
                    let param_tys, ret =
                      instantiate_updater param_tys ret extra_args
                    in
                    if
                      Types.assignable ~policy:Host_boundary
                        ~expected:(List.hd param_tys) ~actual:inner
                         && List.for_all2
                           (fun expected arg ->
                             Types.assignable ~policy:Host_boundary ~expected
                               ~actual:arg.ty)
                              (drop 1 param_tys) extra_args
                      && Types.equal ret inner
                    then
                      let old_expr =
                        apply "Rrbvec.nth"
                          [ target.semantic_expr; index.semantic_expr ]
                      in
                      let value_expr =
                        Semantic_ir.Apply
                          ( fn.semantic_expr,
                            old_expr
                            :: List.map
                                 (fun arg -> arg.semantic_expr)
                                 extra_args )
                      in
                      Ok
                        (typed_ir target.ty
                           (apply "Rrbvec.set"
                              [
                                target.semantic_expr;
                                index.semantic_expr;
                                value_expr;
                              ]))
                    else
                      Error.error
                        "update function arguments do not match vector element \
                         and extra arguments"
                  | TFn (_param_tys, ret) when not (Types.equal ret inner) ->
                      Error.error
                        ("cannot update vector element as " ^ source_name ret
                       ^ " because it is already " ^ source_name inner)
                  | TFn _ ->
                      Error.error
                      "update function arguments do not match vector element \
                       and extra arguments"
                  | _ -> Error.error "update expects a function")
              | TVector _, _ -> Error.error "update vector index must be int"
            | target_ty, _ when Types.is_dynamic target_ty ->
                compile_dynamic target index fn extra_args
              | _ -> Error.error "update expects a map or vector"))
    | _ ->
        Error.error
          "update expects collection, key/index, function, and optional \
           arguments"
    and compile_select_keys scope env arg_forms =
      match arg_forms with
      | [ target_form; FVector key_forms ] -> (
          let rec parse_keywords acc = function
            | [] -> Ok (List.rev acc)
            | FKeyword keyword :: rest -> parse_keywords (keyword :: acc) rest
            | _ -> Error.error "select-keys expects a vector of keywords"
          in
        match
          (compile_expr scope env target_form, parse_keywords [] key_forms)
        with
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
      | TOcaml_app ("Lg_runtime.Runtime_transient.set", [ element_type ]), _
          when Types.equal element_type TUnknown
               || Types.same_shape element_type value.ty ->
            Ok
              (typed_ir TBool
                 (Semantic_ir.Apply
                  ( Semantic_ir.Ident "Lg_runtime.Runtime_transient.set_mem",
                      [ target.semantic_expr; value.semantic_expr ] )))
        | TOcaml_app ("Lg_runtime.Runtime_transient.set", _), _ ->
            Error.error
              "contains? value type must match transient set element type"
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
        | TMap_keys, TKeyword ->
            Ok
              (typed_ir TBool
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Core_set.String_set.mem",
                      [ value.semantic_expr; target.semantic_expr ] )))
        | TMap_keys, _ -> Error.error "contains? map key must be a keyword"
        | target_ty, _ when Types.is_dynamic target_ty ->
            Result.map
              (fun value ->
                typed_ir TBool
                  (Semantic_ir.Apply
                   ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.contains",
                       [ target.semantic_expr; value ] )))
              (pack_dynamic_scalar value)
        | target_ty, _ -> (
            match Types.dynamic_map_types target_ty with
            | Some (key_ty, _)
              when Types.assignable ~policy:Host_boundary ~expected:key_ty
                     ~actual:value.ty ->
                Ok
                  (typed_ir TBool
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_map.mem",
                          [ target.semantic_expr; value.semantic_expr ] )))
            | None
              when Types.equal target_ty TUnknown
                 || match target_ty with TVar _ -> true | _ -> false ->
                Ok
                  (typed_ir TBool
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_map.mem",
                          [ target.semantic_expr; value.semantic_expr ] )))
            | _ ->
                Error.error
                  ("contains? expects a map, set, or vector, got "
                 ^ source_name target.ty))
      in
      match arg_forms with
    | [ target_form; FKeyword keyword ] -> (
          match compile_expr scope env target_form with
          | Error _ as err -> err
          | Ok target -> (
              match target.ty with
              | TRecord fields | TNamed_record { fields; _ } ->
                  if Option.is_some (find_field keyword fields) then
                    Ok (typed_ir TBool (Semantic_ir.Bool true))
                  else (
                    match
                      Structural_map.extension_contains target fields keyword
                    with
                    | Some result -> Ok result
                    | None -> Ok (typed_ir TBool (Semantic_ir.Bool false)))
              | _ ->
                  compile_collection_contains target
                    (typed_ir TKeyword (Semantic_ir.String keyword))))
    | [ target_form; value_form ] -> (
        match
          (compile_expr scope env target_form, compile_expr scope env value_form)
        with
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
              let visible_fields = Types.record_constructor_fields fields in
              let declared_keys =
                Semantic_ir.List
                  (List.map
                     (fun (field : field) ->
                       Semantic_ir.String field.keyword)
                     visible_fields)
              in
              let keys =
                match Types.find_record_extension_field fields with
                | None -> declared_keys
                | Some extension_field ->
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "List.append",
                        [
                          declared_keys;
                          Semantic_ir.Apply
                            ( Semantic_ir.Ident "List.map",
                              [
                                Semantic_ir.Ident "fst";
                                Structural_map.field_expr target extension_field;
                              ] );
                        ] )
              in
              Ok
                (typed_ir (TVector TKeyword)
                   (Semantic_ir.Apply
                      ( Semantic_ir.Ident "Rrbvec.of_list",
                        [ keys ] )))
          | _ -> Error.error "keys expects a map")
      | Ok _ -> Error.error "keys expects 1 arguments"
    and compile_vals scope env arg_forms =
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok [ target ] -> (
          match target.ty with
          | TRecord [] | TNamed_record { fields = []; _ } ->
              Error.error "vals requires a non-empty map"
        | TRecord (first :: rest) | TNamed_record { fields = first :: rest; _ }
          ->
            if
              List.for_all
                (fun (field : field) -> Types.equal first.ty field.ty)
                rest
            then
                Ok
                  (typed_ir (TVector first.ty)
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Rrbvec.of_list",
                        [
                          Semantic_ir.List
                            (first :: rest
                              |> List.map (fun (field : field) ->
                                Structural_map.field_expr target field));
                        ] )))
            else
              Error.error "vals requires all map values to have the same type"
          | _ -> Error.error "vals expects a map")
      | Ok _ -> Error.error "vals expects 1 arguments"
  in
  {
    compile_list;
    compile_list_star;
    compile_range;
    compile_list_of;
    compile_vector_of;
    compile_conj;
    compile_cons;
    compile_subvec;
    compile_nth;
    compile_get;
    compile_find;
    compile_assoc;
    compile_dissoc;
    compile_merge;
    compile_hash_map;
    compile_update;
    compile_select_keys;
    compile_contains;
    compile_keys;
    compile_vals;
  }
