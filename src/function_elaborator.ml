open Types
open Expression_support
module Env = Compiler_environment

let unique_named_records records =
  List.fold_left
    (fun unique record ->
      if
        List.exists
          (fun existing -> Type_id.equal existing.type_id record.type_id)
          unique
      then unique
      else record :: unique)
    [] records

let record_inference_compatible env ~allow_expected_dynamic expected_fields
    actual_fields =
  expected_fields
  |> List.for_all (fun (expected : field) ->
         match find_field expected.keyword actual_fields with
         | None ->
             Option.is_some (Types.find_record_extension_field actual_fields)
         | Some actual ->
             let expected_dynamic_compatible =
               match Types.dynamic_constraint_info expected.ty with
               | Some capability when not (Types.equal capability TUnknown) ->
                   Types.equal capability actual.ty
                   || Types.row_compatible ~expected:capability
                        ~actual:actual.ty
               | Some _ -> allow_expected_dynamic
               | None -> false
             in
             let expected_protocol_compatible =
               match Types.protocol_constraint_info expected.ty with
               | Some (protocol_id, _, _) ->
                   Protocol.type_satisfies env protocol_id actual.ty
               | None -> false
             in
             Types.is_dynamic actual.ty
             || (match actual.ty with TUnknown | TVar _ -> true | _ -> false)
             || (match expected.ty with TUnknown | TVar _ -> true | _ -> false)
             || expected_dynamic_compatible
             || expected_protocol_compatible
             || Types.equal expected.ty actual.ty
             || Types.row_compatible ~expected:expected.ty ~actual:actual.ty)

let rec infer_named_record ?(allow_dynamic_fields = false) scope env = function
  | TNullable inner ->
      TNullable (infer_named_record ~allow_dynamic_fields scope env inner)
  | TOcaml_app ("option", [ inner ]) ->
      TOcaml_app
        ("option", [ infer_named_record ~allow_dynamic_fields scope env inner ])
  | ty when Types.is_dynamic ty -> (
      match Types.dynamic_constraint_info ty with
      | None -> assert false
      | Some capability -> (
          match
            infer_named_record ~allow_dynamic_fields:true scope env capability
          with
          | TNamed_record _ as record -> record
          | capability -> Types.dynamic_constraint capability))
  | ty when Option.is_some (Types.protocol_constraint_info ty) -> (
      match Types.protocol_constraint_info ty with
      | None -> assert false
      | Some (_, _, value_ty) ->
          Types.protocol_constraint_with_value ty
            (infer_named_record ~allow_dynamic_fields:true scope env value_ty))
  | TOcaml name when String.starts_with ~prefix:"__lg_record:" name ->
      let source_name =
        String.sub name
          (String.length "__lg_record:")
          (String.length name - String.length "__lg_record:")
      in
      Resolver.lookup_record_type scope env source_name
      |> Result.map (fun record -> TNamed_record record)
      |> Result.value ~default:(TOcaml name)
  | TRecord fields -> (
      let fields =
        List.map
          (fun (field : field) ->
            { field with ty = infer_named_record scope env field.ty })
          fields
      in
      let inferred = TRecord fields in
      let candidates =
        Env.filter_map
          (fun key (binding : binding) ->
            if String.starts_with ~prefix:"__record/" key then
              match binding.ty with
              | TNamed_record record ->
                  if
                    record_inference_compatible env
                      ~allow_expected_dynamic:allow_dynamic_fields fields
                      record.fields
                    || Types.row_compatible ~expected:(TRecord fields)
                         ~actual:binding.ty
                  then Some record
                  else None
              | _ -> None
            else None)
          env
        |> unique_named_records
      in
      let direct_match_count record =
        fields
        |> List.fold_left
             (fun count field ->
               match find_field field.keyword record.fields with
               | Some actual when not (Types.is_record_extension_field actual) ->
                   count + 1
               | Some _ | None -> count)
             0
      in
      let best_direct_matches =
        candidates
        |> List.fold_left
             (fun best record -> max best (direct_match_count record))
             0
      in
      let candidates =
        if best_direct_matches = 0 then candidates
        else
          List.filter
            (fun record -> direct_match_count record = best_direct_matches)
            candidates
      in
      match candidates with [ record ] -> TNamed_record record | _ -> inferred)
  | inferred -> inferred

let rec pattern_constraint_type = function
  | TUnknown | TVar _ -> TOcaml "_"
  | TNullable ty -> TNullable (pattern_constraint_type ty)
  | TOcaml_app (name, arguments) ->
      TOcaml_app (name, List.map pattern_constraint_type arguments)
  | TTuple items -> TTuple (List.map pattern_constraint_type items)
  | TArray ty -> TArray (pattern_constraint_type ty)
  | TRef ty -> TRef (pattern_constraint_type ty)
  | TList ty -> TList (pattern_constraint_type ty)
  | TVector ty -> TVector (pattern_constraint_type ty)
  | TSet ty -> TSet (pattern_constraint_type ty)
  | TSeq ty -> TSeq (pattern_constraint_type ty)
  | TFn (parameters, return_type) ->
      TFn
        ( List.map pattern_constraint_type parameters,
          pattern_constraint_type return_type )
  | ty -> ty

let prepare ?(param_type_overrides = []) ?variadic_rest_index
    ?compile_function_body ~lookup_function_ty ~compile_body scope env params
    body_forms =
  match Destructure.parse_param_specs params with
  | Error _ as err -> err
  | Ok specs -> (
      let inference_params =
        specs
        |> List.mapi (fun index (spec : Destructure.param_spec) ->
            let explicit_ty = Option.value spec.explicit_ty ~default:TUnknown in
               let param_ty =
                 match List.nth_opt param_type_overrides index with
                 | Some (Some ty) when not (Types.equal ty TUnknown) -> ty
                 | _ -> explicit_ty
               in
               let destructured =
                 if spec.destructured then
                   Destructure.pattern_names spec.pattern
                   |> List.map (fun name -> (name, TUnknown))
                 else []
               in
               (spec.source_name, param_ty) :: destructured)
        |> List.concat
      in
      let lookup_protocol_constraint = Protocol.constraint_type scope env in
      match
        Type_inference.infer_params ~lookup_function_ty
          ~lookup_protocol_constraint inference_params body_forms
      with
      | Error _ as err -> err
      | Ok inferred -> (
          let inferred =
            List.map
                (fun (name, ty) -> (name, infer_named_record scope env ty))
              inferred
          in
          let lookup_inferred name =
            inferred |> List.assoc_opt name |> Option.value ~default:TUnknown
          in
          let infer_spec_ty (spec : Destructure.param_spec) =
            if spec.destructured then
              Destructure.infer_pattern_type spec.pattern lookup_inferred
            else Ok (lookup_inferred spec.source_name)
          in
          let rec build acc = function
            | [] -> Ok (List.rev acc)
            | spec :: rest -> (
                match infer_spec_ty spec with
                | Error _ as err -> err
                | Ok ty -> build ((spec, ty) :: acc) rest)
          in
          match build [] specs with
          | Error _ as err -> err
          | Ok typed_specs -> (
              let typed_specs =
                typed_specs
                |> List.mapi (fun index (spec, inferred_ty) ->
                       let inferred_ty =
                         if Some index = variadic_rest_index then
                           match Types.seqable_constraint_element inferred_ty with
                           | Some element_ty -> TSeq element_ty
                           | None -> (
                               match inferred_ty with
                               | TUnknown -> TSeq TUnknown
                               | ty -> ty)
                         else inferred_ty
                       in
                       match List.nth_opt param_type_overrides index with
                    | Some (Some TUnknown) | None | Some None -> (
                        match spec.Destructure.explicit_ty with
                        | Some ty -> (spec, infer_named_record scope env ty)
                           | None -> (spec, inferred_ty))
                    | Some (Some ty) -> (spec, ty))
              in
              let param_bindings =
                typed_specs
                |> List.map (fun ((spec : Destructure.param_spec), ty) ->
                       ( Names.scoped_key scope spec.source_name,
                         Types.binding spec.ocaml_name ty ))
              in
              let param_identities =
                typed_specs
                |> List.map (fun ((spec : Destructure.param_spec), _) ->
                       spec.identity)
              in
              let param_targets =
                typed_specs
                |> List.map (fun ((spec : Destructure.param_spec), ty) ->
                       (spec, typed_ir ty (Semantic_ir.Ident spec.ocaml_name)))
              in
              let destructured_bindings =
                let rec loop acc = function
                  | [] -> Ok (List.rev acc)
                  | (spec, target) :: rest -> (
                      if not spec.Destructure.destructured then loop acc rest
                      else
                        match
                          Destructure.bind_pattern ~env target spec.pattern
                        with
                        | Error _ as err -> err
                        | Ok bindings ->
                            loop (List.rev_append bindings acc) rest)
                in
                loop [] param_targets
              in
              match destructured_bindings with
              | Error _ as err -> err
              | Ok destructured_bindings -> (
                  let local_bindings =
                    destructured_bindings
                    |> List.map (fun (binding : Destructure.local_binding) ->
                           ( Names.scoped_key scope binding.source_name,
                             Types.binding binding.ocaml_name binding.ty ))
                  in
                  let env =
                    env
                    |> Env.add_bindings param_bindings
                    |> Env.add_bindings local_bindings
                  in
                  let body_forms =
                    match body_forms with
                    | [] -> [ Ast.FSymbol "nil" ]
                    | _ -> body_forms
                  in
                  let compiled_body =
                    match compile_function_body with
                    | Some compile ->
                        compile env
                          (List.map (fun (_spec, ty) -> ty) typed_specs)
                          body_forms
                    | None ->
                        compile_body scope env
                          "function body requires at least one form" body_forms
                  in
                  match compiled_body with
                  | Error _ as err -> err
                  | Ok body ->
                      Ok
                        {
                          param_bindings;
                          param_identities;
                          destructured_bindings;
                          body;
                        }))))

let fn_code ?(row_param_type_names = []) parts =
  let param_names =
    parts.param_bindings |> List.map (fun (_key, binding) -> binding.ocaml_name)
  in
  let param_tys =
    parts.param_bindings
    |> List.map (fun (_key, (binding : binding)) -> binding.ty)
  in
  let rec capability_pattern name ty =
    match Types.protocol_constraint_info ty with
    | Some (protocol_id, _, value_ty) ->
        Semantic_ir.PTuple
          [
            Semantic_ir.PVar (Types.protocol_witness_name name protocol_id);
            capability_pattern name value_ty;
          ]
    | None -> (
        match ty with
        | TOcaml_app (constraint_name, [ _element_ty; value_ty ])
          when constraint_name = Types.seqable_constraint_name
               || constraint_name = Types.optional_seqable_constraint_name
               || constraint_name = Types.optional_sequential_constraint_name ->
            Semantic_ir.PTuple
              [
                Semantic_ir.PVar
                  (if constraint_name = Types.seqable_constraint_name then
                     name ^ "__seq"
                   else name ^ "__seq_optional");
                capability_pattern name value_ty;
              ]
        | _ -> Semantic_ir.PVar name)
  in
  let param_patterns =
    List.map2 (fun name ty -> (name, ty)) param_names param_tys
    |> List.mapi (fun index (name, ty) ->
           let pattern =
             if
               Option.is_some (Types.protocol_constraint_info ty)
               || Option.is_some (Types.seqable_constraint_element ty)
             then
               Semantic_ir.PConstraint
                 ( capability_pattern name ty,
                   Types.ocaml_name (pattern_constraint_type ty) )
             else
               match List.nth_opt row_param_type_names index with
               | Some (Some type_name) ->
                   Semantic_ir.PConstraint (Semantic_ir.PVar name, type_name)
            | _ -> (
                match ty with
                | TRecord _ -> Semantic_ir.PVar name
               | _ -> (
                   match param_constraint_name ty with
                   | Some type_name ->
                        Semantic_ir.PConstraint
                          (Semantic_ir.PVar name, type_name)
                    | None -> Semantic_ir.PVar name))
           in
           match List.nth_opt parts.param_identities index |> Option.join with
           | None -> pattern
           | Some (node_id, location) ->
               Semantic_ir.PLocated (node_id, location, pattern))
  in
  let body_expr =
    match parts.destructured_bindings with
    | [] -> parts.body.semantic_expr
    | bindings ->
        Semantic_ir.Let
          ( List.map
              (fun (binding : Destructure.local_binding) ->
                let pattern =
                  if
                    Option.is_some (Types.protocol_constraint_info binding.ty)
                    || Option.is_some
                         (Types.seqable_constraint_element binding.ty)
                  then capability_pattern binding.ocaml_name binding.ty
                  else Semantic_ir.PVar binding.ocaml_name
                in
                let pattern =
                  match binding.identity with
                  | None -> pattern
                  | Some (node_id, location) ->
                      Semantic_ir.PLocated (node_id, location, pattern)
                in
                (pattern, binding.semantic_expr))
              bindings,
            parts.body.semantic_expr )
  in
  let return_param_index =
    match
      ( parts.destructured_bindings,
        Semantic_ir.unlocated parts.body.semantic_expr )
    with
    | [], Semantic_ir.Ident returned_name ->
        param_names
        |> List.mapi (fun index name -> (index, name))
        |> List.find_opt (fun (_index, name) -> name = returned_name)
        |> Option.map fst
    | _ -> None
  in
  {
    (typed_ir
       (TFn (param_tys, parts.body.ty))
       (Semantic_ir.Fun (param_patterns, body_expr)))
    with
    return_param_index;
  }
