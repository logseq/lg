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

let same_host_wrapper expected actual =
  let same_outer =
    match (expected, actual) with
    | TArray _, TArray _ | TRef _, TRef _ | TNullable _, TNullable _ -> true
    | TOcaml_app (expected_name, expected_args),
      TOcaml_app (actual_name, actual_args) ->
        expected_name = actual_name
        && List.length expected_args = List.length actual_args
    | TTuple expected_items, TTuple actual_items ->
        List.length expected_items = List.length actual_items
    | _ -> false
  in
  same_outer
  && Types.assignable ~policy:Host_boundary ~expected ~actual

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
             let open_type_compatible =
               match Type_solver.unify Type_solver.empty expected.ty actual.ty with
               | Ok _ -> true
               | Error _ -> false
             in
             let expected_protocol_compatible =
               match Types.protocol_constraint_info expected.ty with
               | Some (protocol_id, _, _) ->
                   Protocol.type_satisfies env protocol_id actual.ty
               | None -> false
             in
            let expected_seqable_compatible =
              Option.is_some (Types.seqable_constraint_info expected.ty)
               && Collection_capability.accepts_seqable env actual.ty
            in
            let expected_contains_compatible =
              Option.is_some (Types.contains_constraint_info expected.ty)
              && Collection_capability.accepts_contains actual.ty
            in
             Types.is_dynamic actual.ty
             || (match actual.ty with TUnknown | TMeta _ | TVar _ -> true | _ -> false)
             || (match expected.ty with TUnknown | TMeta _ | TVar _ -> true | _ -> false)
             || expected_dynamic_compatible
             || open_type_compatible
             || expected_protocol_compatible
             || expected_seqable_compatible
             || expected_contains_compatible
             || Types.equal expected.ty actual.ty
             || same_host_wrapper expected.ty actual.ty
             || Types.row_compatible ~expected:expected.ty ~actual:actual.ty)

let rec infer_named_record ?(allow_dynamic_fields = false) scope env = function
  | TNamed_record record as ty -> (
      match
        Resolver.lookup_record_type scope env (Type_id.to_string record.type_id)
      with
      | Ok canonical -> (
          match Types.refresh_named_record canonical ty with
          | TNamed_record refreshed when String.contains record.type_name '.' ->
              TNamed_record
                {
                  refreshed with
                  type_name = record.type_name;
                  set_module_name = record.set_module_name;
                }
          | refreshed -> refreshed)
      | Error _ -> ty)
  | TNullable inner ->
      TNullable (infer_named_record ~allow_dynamic_fields scope env inner)
  | TArray inner ->
      TArray (infer_named_record ~allow_dynamic_fields scope env inner)
  | TRef inner -> TRef (infer_named_record ~allow_dynamic_fields scope env inner)
  | TList inner ->
      TList (infer_named_record ~allow_dynamic_fields scope env inner)
  | TVector inner ->
      TVector (infer_named_record ~allow_dynamic_fields scope env inner)
  | TSet inner -> TSet (infer_named_record ~allow_dynamic_fields scope env inner)
  | TSeq inner -> TSeq (infer_named_record ~allow_dynamic_fields scope env inner)
  | TFn (parameters, return_ty) ->
      TFn
        ( List.map
            (infer_named_record ~allow_dynamic_fields scope env)
            parameters,
          infer_named_record ~allow_dynamic_fields scope env return_ty )
  | TOverloaded_fn arities ->
      TOverloaded_fn
        (List.map
           (fun (arity : fn_arity) ->
             {
               fixed_params =
                 List.map
                   (infer_named_record ~allow_dynamic_fields scope env)
                   arity.fixed_params;
               rest_param =
                 Option.map
                   (infer_named_record ~allow_dynamic_fields scope env)
                   arity.rest_param;
               return_ty =
                 infer_named_record ~allow_dynamic_fields scope env
                   arity.return_ty;
             })
           arities)
  | TTuple items ->
      TTuple
        (List.map (infer_named_record ~allow_dynamic_fields scope env) items)
  | TOcaml_app ("option", [ inner ]) ->
      TOcaml_app
        ("option", [ infer_named_record ~allow_dynamic_fields scope env inner ])
  | ty when Types.is_dynamic ty -> (
      match Types.dynamic_constraint_info ty with
      | None -> assert false
      | Some capability -> (
          let open_record_capability =
            match capability with
            | TRecord fields ->
                Option.is_some (Types.find_record_extension_field fields)
            | _ -> false
          in
          let capability =
            match capability with
            | TRecord fields when open_record_capability ->
                TRecord
                  (List.map
                     (fun (field : field) ->
                       {
                         field with
                         ty =
                           infer_named_record ~allow_dynamic_fields:true scope
                             env field.ty;
                       })
                     fields)
            | capability ->
                infer_named_record ~allow_dynamic_fields:true scope env
                  capability
          in
          if open_record_capability then Types.dynamic_constraint capability
          else
            match Types.constraint_value_type capability with
            | TNamed_record _ -> capability
            | _ -> Types.dynamic_constraint capability))
  | ty when Option.is_some (Types.protocol_constraint_info ty) -> (
      match Types.protocol_constraint_info ty with
      | None -> assert false
      | Some (_, _, value_ty) ->
          Types.protocol_constraint_with_value ty
            (infer_named_record ~allow_dynamic_fields:true scope env value_ty))
  | TOcaml_app (name, [ TRecord fields; container ])
    when name = Types.seqable_constraint_name
         || name = Types.optional_seqable_constraint_name
         || name = Types.optional_sequential_constraint_name ->
      let fields =
        List.map
          (fun (field : field) ->
            {
              field with
              ty =
                infer_named_record ~allow_dynamic_fields scope env field.ty;
            })
          fields
      in
      TOcaml_app
        ( name,
          [
            TRecord fields;
            infer_named_record ~allow_dynamic_fields scope env container;
          ] )
  | TOcaml_app (name, arguments) ->
      let arguments =
        List.map
          (infer_named_record ~allow_dynamic_fields scope env)
          arguments
      in
      let record_name =
        if String.starts_with ~prefix:"__lg_record_app:" name then
          String.sub name
            (String.length "__lg_record_app:")
            (String.length name - String.length "__lg_record_app:")
        else name
      in
      (match Resolver.lookup_type_declaration scope env record_name with
      | Some
          {
            kind = Alias;
            type_parameters;
            manifest = Some manifest;
            _;
          }
        when List.length type_parameters = List.length arguments ->
          let substitutions = List.combine type_parameters arguments in
          Types.substitute_type_variables
            (Type_solver.of_list
               (List.map
               (fun (parameter, argument) ->
                 (Type_solver.Declared parameter, argument))
               substitutions))
            manifest
          |> infer_named_record ~allow_dynamic_fields scope env
      | Some { kind = Alias; _ } -> TOcaml_app (name, arguments)
      | Some { kind = (Record | Variant); _ } | None ->
      (match Resolver.lookup_record_type scope env record_name with
      | Ok record
        when List.length record.type_parameters = List.length arguments ->
          TNamed_record { record with type_arguments = arguments }
      | Ok _ | Error _ -> TOcaml_app (name, arguments)))
  | TOcaml name as ty ->
      let record_prefix = "__lg_record:" in
      let source_name =
        if String.starts_with ~prefix:record_prefix name then
          String.sub name (String.length record_prefix)
            (String.length name - String.length record_prefix)
        else name
      in
      (match Resolver.lookup_record_type scope env source_name with
      | Ok record -> TNamed_record record
      | Error _ ->
          (match Resolver.lookup_type_declaration scope env source_name with
          | Some
              {
                kind = Alias;
                type_parameters = [];
                manifest = Some manifest;
                _;
              } ->
              infer_named_record ~allow_dynamic_fields scope env manifest
          | Some { kind = Alias; _ } -> ty
          | Some { kind = Variant; type_id; _ } ->
              let type_name = Names.sanitize_name (Type_id.name type_id) in
              let owner = Type_id.owner type_id |> String.concat "." in
              let owner_is_module =
                owner <> ""
                && Module_registry.mem_module
                     (Module_id.create ~owner:[] ~name:owner)
                     (Env.modules env)
              in
              if
                (String.contains source_name '.'
                || String.contains source_name '/')
                && owner_is_module
              then
                TOcaml (Type_registry.emitted_name ~scope:owner type_name)
              else TOcaml type_name
          | Some { kind = Record; _ } | None ->
              Env.find_record_binding
                (fun _ (binding : binding) ->
                     match binding.ty with
                     | TNamed_record record
                       when String.equal record.type_name source_name ->
                       Some (TNamed_record record)
                     | _ -> None)
                env
              |> Option.value ~default:ty))
  | TRecord fields -> (
      let fields =
        List.map
          (fun (field : field) ->
            { field with ty = infer_named_record scope env field.ty })
          fields
      in
      let inferred = TRecord fields in
      let candidates =
        Env.filter_record_bindings
          (fun key (binding : binding) ->
            if String.starts_with ~prefix:"__record/" key then
              match binding.ty with
              | TNamed_record record ->
                  let compatible =
                    record_inference_compatible env
                      ~allow_expected_dynamic:allow_dynamic_fields fields
                      record.fields
                    || Types.row_compatible ~expected:(TRecord fields)
                         ~actual:binding.ty
                  in
                  if compatible
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
      match candidates with
      | [ record ] ->
          let matched_fields =
            record.fields
            |> List.filter_map (fun (template : field) ->
                   match find_field template.keyword fields with
                   | Some (actual : field)
                     when not (Types.equal actual.ty TUnknown) ->
                       Some (template.ty, actual.ty)
                   | Some _ | None -> None)
          in
          let templates, actuals = List.split matched_fields in
          Types.instantiate_type ~templates ~actuals (TNamed_record record)
      | _ -> inferred)
  | inferred -> inferred

let rec collapse_static_record_protocols env ty =
  match Types.protocol_constraint_info ty with
  | Some (protocol_id, _, value_ty) ->
      let value_ty = collapse_static_record_protocols env value_ty in
      (match value_ty with
      | TNamed_record _ when Protocol.type_satisfies env protocol_id value_ty ->
          value_ty
      | _ -> Types.protocol_constraint_with_value ty value_ty)
  | None -> (
      match ty with
      | TVector element_ty ->
          TVector (collapse_static_record_protocols env element_ty)
      | TRecord fields ->
          TRecord
            (List.map
               (fun (field : field) ->
                 {
                   field with
                   ty = collapse_static_record_protocols env field.ty;
                 })
               fields)
      | ty -> ty)

let rec protocol_witness_constraint_type receiver_ty = function
  | TUnknown | TMeta _ -> TOcaml "_"
  | TVar _ as ty -> ty
  | TNullable ty -> TNullable (protocol_witness_constraint_type receiver_ty ty)
  | TOcaml_app (name, arguments) ->
      TOcaml_app
        (name, List.map (protocol_witness_constraint_type receiver_ty) arguments)
  | TTuple items ->
      TTuple (List.map (protocol_witness_constraint_type receiver_ty) items)
  | TArray ty -> TArray (protocol_witness_constraint_type receiver_ty ty)
  | TRef ty -> TRef (protocol_witness_constraint_type receiver_ty ty)
  | TList ty -> TList (protocol_witness_constraint_type receiver_ty ty)
  | TVector ty -> TVector (protocol_witness_constraint_type receiver_ty ty)
  | TSet ty -> TSet (protocol_witness_constraint_type receiver_ty ty)
  | TSeq ty -> TSeq (protocol_witness_constraint_type receiver_ty ty)
  | TFn (parameters, return_type) ->
      let parameters =
        match parameters with
        | _receiver :: rest ->
            pattern_constraint_type (Types.constraint_value_type receiver_ty)
            :: List.map (protocol_witness_constraint_type receiver_ty) rest
        | [] -> []
      in
      TFn
        (parameters, protocol_witness_constraint_type receiver_ty return_type)
  | TOverloaded_fn arities ->
      TOverloaded_fn
        (List.map
           (fun arity ->
             let fixed_params =
               match arity.fixed_params with
               | _receiver :: rest ->
                   pattern_constraint_type
                     (Types.constraint_value_type receiver_ty)
                   :: List.map
                        (protocol_witness_constraint_type receiver_ty)
                        rest
               | [] -> []
             in
             {
               fixed_params;
               rest_param =
                 Option.map
                   (protocol_witness_constraint_type receiver_ty)
                   arity.rest_param;
               return_ty =
                 protocol_witness_constraint_type receiver_ty arity.return_ty;
             })
           arities)
  | ty -> ty

and pattern_constraint_type = function
  | TUnknown | TMeta _ | TVar _ -> TOcaml "_"
  | TNullable ty -> TNullable (pattern_constraint_type ty)
  | TOcaml_app (name, [ witness_ty; value_ty ])
    when Option.is_some (Types.protocol_constraint_id name) ->
      TOcaml_app
        ( name,
          [
            protocol_witness_constraint_type value_ty witness_ty;
            pattern_constraint_type value_ty;
          ] )
  | TOcaml_app (name, arguments) ->
      TOcaml_app (name, List.map pattern_constraint_type arguments)
  | TTuple items -> TTuple (List.map pattern_constraint_type items)
  | TArray ty ->
      TArray (pattern_constraint_type (Types.constraint_value_type ty))
  | TRef ty -> TRef (pattern_constraint_type ty)
  | TList ty -> TList (pattern_constraint_type ty)
  | TVector ty -> TVector (pattern_constraint_type ty)
  | TSet ty -> TSet (pattern_constraint_type ty)
  | TSeq ty -> TSeq (pattern_constraint_type ty)
  | TRecord fields as ty ->
      if Types.is_homogeneous_record fields then ty else TOcaml "_"
  | TFn (parameters, return_type) ->
      TFn
        ( List.map pattern_constraint_type parameters,
          pattern_constraint_type return_type )
  | TOverloaded_fn arities ->
      TOverloaded_fn
        (List.map
           (fun arity ->
             {
               fixed_params =
                 List.map pattern_constraint_type arity.fixed_params;
               rest_param = Option.map pattern_constraint_type arity.rest_param;
               return_ty = pattern_constraint_type arity.return_ty;
             })
           arities)
  | TNamed_record record ->
      TNamed_record
        {
          record with
          type_arguments =
            List.map pattern_constraint_type record.type_arguments;
          fields =
            List.map
              (fun (field : field) ->
                { field with ty = pattern_constraint_type field.ty })
              record.fields;
        }
  | ty -> ty

let array_storage_type = function
  | TArray element_ty -> TArray (Types.constraint_value_type element_ty)
  | ty -> ty

let rec contains_open_type = function
  | TUnknown | TMeta _ | TVar _ -> true
  | TNullable ty | TArray ty | TRef ty | TList ty | TVector ty | TSet ty
  | TSeq ty ->
      contains_open_type ty
  | TOcaml_app (_, arguments) | TTuple arguments ->
      List.exists contains_open_type arguments
  | TFn (parameters, return_ty) ->
      List.exists contains_open_type parameters || contains_open_type return_ty
  | TOverloaded_fn arities ->
      List.exists
        (fun arity ->
          List.exists contains_open_type arity.fixed_params
          || (match arity.rest_param with
             | Some ty -> contains_open_type ty
             | None -> false)
          || contains_open_type arity.return_ty)
        arities
  | TRecord fields ->
      List.exists (fun (field : field) -> contains_open_type field.ty) fields
  | TNamed_record record ->
      List.exists contains_open_type record.type_arguments
      || List.exists
           (fun (field : field) -> contains_open_type field.ty)
           record.fields
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TOcaml _ ->
      false

let rec contains_structural_record = function
  | TRecord _ -> true
  | TNullable ty | TArray ty | TRef ty | TList ty | TVector ty | TSet ty
  | TSeq ty ->
      contains_structural_record ty
  | TOcaml_app (_, arguments) | TTuple arguments ->
      List.exists contains_structural_record arguments
  | TFn (parameters, return_ty) ->
      List.exists contains_structural_record parameters
      || contains_structural_record return_ty
  | TOverloaded_fn arities ->
      List.exists
        (fun arity ->
          List.exists contains_structural_record arity.fixed_params
          || Option.fold ~none:false ~some:contains_structural_record
               arity.rest_param
          || contains_structural_record arity.return_ty)
        arities
  | TNamed_record _ | TInt | TFloat | TChar | TString | TRegex | TMap_keys
  | TSymbol | TKeyword | TBool | TUnit | TNil | TUnknown | TMeta _ | TVar _ | TOcaml _ ->
      false

let shared_parameter_variables inferred =
  inferred
  |> List.fold_left
       (fun counts (_name, ty) ->
         Type_solver.variables ty
         |> List.fold_left
              (fun counts variable ->
                let count = List.assoc_opt variable counts |> Option.value ~default:0 in
                (variable, count + 1) :: List.remove_assoc variable counts)
              counts)
       []
  |> List.filter_map (fun (variable, count) ->
         if count > 1 then Some variable else None)

let reconcile_shared_parameter_variables original resolved =
  let shared = shared_parameter_variables original in
  let substitutions =
    List.fold_left2
      (fun substitutions (_name, template) (_name, actual) ->
        match Type_solver.unify substitutions template actual with
        | Ok substitutions -> substitutions
        | Error _ -> substitutions)
      Type_solver.empty original resolved
    |> Type_solver.filter (fun variable _ty -> List.mem variable shared)
  in
  List.map
    (fun (name, ty) -> (name, Type_solver.apply substitutions ty))
    resolved

let rec apply_row_constraint_type row_type_name = function
  | TRecord _ -> TOcaml (Types.ocaml_record_type_name row_type_name)
  | TNamed_record { nominal = false; _ } ->
      TOcaml (Types.ocaml_record_type_name row_type_name)
  | TOcaml_app (name, [ TRecord _; container ])
    when name = Types.seqable_constraint_name
         || name = Types.optional_seqable_constraint_name
         || name = Types.optional_sequential_constraint_name ->
      TOcaml_app
        (name, [ TOcaml (Types.ocaml_record_type_name row_type_name); container ])
  | TNullable ty -> TNullable (apply_row_constraint_type row_type_name ty)
  | TOcaml_app (name, arguments) ->
      TOcaml_app
        (name, List.map (apply_row_constraint_type row_type_name) arguments)
  | TTuple items ->
      TTuple (List.map (apply_row_constraint_type row_type_name) items)
  | TArray ty -> TArray (apply_row_constraint_type row_type_name ty)
  | TRef ty -> TRef (apply_row_constraint_type row_type_name ty)
  | TList ty -> TList (apply_row_constraint_type row_type_name ty)
  | TVector ty -> TVector (apply_row_constraint_type row_type_name ty)
  | TSet ty -> TSet (apply_row_constraint_type row_type_name ty)
  | TSeq ty -> TSeq (apply_row_constraint_type row_type_name ty)
  | TFn (parameters, return_type) ->
      TFn
        ( List.map (apply_row_constraint_type row_type_name) parameters,
          apply_row_constraint_type row_type_name return_type )
  | ty -> ty

let rec replace_post_result result_name = function
  | Ast.FSymbol "%" -> Ast.FSymbol result_name
  | Ast.FList forms ->
      Ast.FList (List.map (replace_post_result result_name) forms)
  | Ast.FVector forms ->
      Ast.FVector (List.map (replace_post_result result_name) forms)
  | Ast.FMap pairs ->
      Ast.FMap
        (List.map
           (fun (key, value) ->
             ( replace_post_result result_name key,
               replace_post_result result_name value ))
           pairs)
  | form -> form

let normalize_prepost_body = function
  | Ast.FMap pairs :: body_forms as original ->
      let clauses keyword =
        pairs
        |> List.find_map (function
             | Ast.FKeyword key, Ast.FVector forms when key = keyword ->
                 Some forms
             | _ -> None)
      in
      let pre = clauses ":pre" in
      let post = clauses ":post" in
      if Option.is_none pre && Option.is_none post then original
      else
        let assertions forms =
          List.map
            (fun form -> Ast.FList [ Ast.FSymbol "assert"; form ])
            forms
        in
        let pre = Option.value pre ~default:[] |> assertions in
        let post = Option.value post ~default:[] in
        if post = [] then pre @ body_forms
        else
          let result_name = "__lg_post_result" in
          let result =
            match body_forms with
            | [] -> Ast.FSymbol "nil"
            | [ form ] -> form
            | forms -> Ast.FList (Ast.FSymbol "do" :: forms)
          in
          let post =
            post
            |> List.map (replace_post_result result_name)
            |> assertions
          in
          pre
          @ [
              Ast.FList
                [
                  Ast.FSymbol "let";
                  Ast.FVector [ Ast.FSymbol result_name; result ];
                  Ast.FList
                    (Ast.FSymbol "do" :: post
                   @ [ Ast.FSymbol result_name ]);
                ];
            ]
  | body_forms -> body_forms

let prepare ?(param_type_overrides = []) ?variadic_rest_index
    ?(materialize_open_equality = false) ?(refine_open_overrides = false)
    ?compile_function_body ?compile_default
    ~lookup_function_ty ~compile_body scope env params body_forms =
  match Macro_expander.expand_all_forms ~scope ~compiler_env:env body_forms with
  | Error _ as error -> error
  | Ok body_forms ->
      let body_forms = normalize_prepost_body body_forms in
      match Destructure.parse_param_specs params with
      | Error _ as err -> err
      | Ok specs -> (
      let inference_params =
        specs
        |> List.mapi (fun index (spec : Destructure.param_spec) ->
            let param_ty =
              match spec.explicit_ty with
              | Some ty when not (Types.equal ty TUnknown) ->
                  infer_named_record scope env ty
              | _ -> (
                  match List.nth_opt param_type_overrides index with
                  | Some (Some ty) when not (Types.equal ty TUnknown) -> ty
                  | _ -> Type_solver.fresh ())
            in
            let param_ty =
              if Some index = variadic_rest_index then
                match param_ty with
                | TMeta _ -> TSeq param_ty
                | TUnknown -> TSeq (Type_solver.fresh ())
                | ty -> ty
              else param_ty
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
      let lookup_dynamic_key_record_type =
        Expression_support.dynamic_key_record_type env
      in
      let resolve_named_record = infer_named_record scope env in
      let lookup_function_ty name =
        Result.map resolve_named_record (lookup_function_ty name)
      in
      match
        Type_inference.infer_params ~materialize_open_equality
          ~lookup_function_ty
          ~lookup_protocol_constraint ~lookup_dynamic_key_record_type
          ~resolve_named_record
          inference_params body_forms
      with
      | Error _ as err -> err
      | Ok inferred -> (
          let rec matches_parameter_as_option name = function
            | Ast.FList
                (Ast.FSymbol "match" :: Ast.FSymbol target :: clauses)
              when String.equal name target ->
                List.exists
                  (function
                    | Ast.FList (Ast.FSymbol "Some" :: _)
                    | Ast.FSymbol "None" ->
                        true
                    | _ -> false)
                  clauses
            | Ast.FList (Ast.FSymbol "fn" :: _) -> false
            | Ast.FList forms | Ast.FVector forms ->
                List.exists (matches_parameter_as_option name) forms
            | Ast.FMap pairs ->
                List.exists
                  (fun (key, value) ->
                    matches_parameter_as_option name key
                    || matches_parameter_as_option name value)
                  pairs
            | Ast.FInt _ | Ast.FFloat _ | Ast.FChar _ | Ast.FString _
            | Ast.FRegex _ | Ast.FBool _ | Ast.FKeyword _ | Ast.FSymbol _
            | Ast.FCoreSymbol _ ->
                false
          in
          let option_matched_parameters =
            specs
            |> List.filter_map (fun (spec : Destructure.param_spec) ->
                   if
                     List.exists
                       (matches_parameter_as_option spec.source_name)
                       body_forms
                   then Some spec.source_name
                   else None)
          in
          let destructured_sources =
            specs
            |> List.filter_map (fun (spec : Destructure.param_spec) ->
                   if spec.destructured then Some spec.source_name else None)
          in
          let rec directly_accesses_field parameter = function
            | Ast.FList
                [ Ast.FSymbol field_access; Ast.FSymbol target ]
              when String.starts_with ~prefix:".-" field_access
                   && String.equal parameter target ->
                true
            | Ast.FList forms | Ast.FVector forms ->
                List.exists (directly_accesses_field parameter) forms
            | Ast.FMap pairs ->
                List.exists
                  (fun (key, value) ->
                    directly_accesses_field parameter key
                    || directly_accesses_field parameter value)
                  pairs
            | Ast.FInt _ | Ast.FFloat _ | Ast.FChar _ | Ast.FString _
            | Ast.FRegex _ | Ast.FBool _ | Ast.FKeyword _ | Ast.FSymbol _
            | Ast.FCoreSymbol _ ->
                false
          in
          let directly_accessed_parameters =
            specs
            |> List.filter_map (fun (spec : Destructure.param_spec) ->
                   if
                     List.exists
                       (directly_accesses_field spec.source_name)
                       body_forms
                   then Some spec.source_name
                   else None)
          in
          let infer_structural_fields fields =
            TRecord
              (List.map
                 (fun (field : field) ->
                   {
                     field with
                     ty = infer_named_record scope env field.ty;
                   })
                 fields)
          in
          let infer_parameter_type name ty =
            if List.mem name destructured_sources then
              match ty with
              | TRecord fields -> infer_structural_fields fields
              | TNullable (TRecord fields) ->
                  TNullable (infer_structural_fields fields)
              | TOcaml_app ("option", [ TRecord fields ]) ->
                  TOcaml_app ("option", [ infer_structural_fields fields ])
              | ty -> infer_named_record scope env ty
            else
              match ty with
              | TNullable (TRecord fields) ->
                  let structural = infer_structural_fields fields in
                  if List.mem name directly_accessed_parameters then
                    TNullable (infer_named_record scope env structural)
                  else TNullable structural
              | TOcaml_app ("option", [ TRecord fields ]) ->
                  let structural = infer_structural_fields fields in
                  if List.mem name directly_accessed_parameters then
                    TOcaml_app
                      ("option", [ infer_named_record scope env structural ])
                  else TOcaml_app ("option", [ structural ])
              | ty -> infer_named_record scope env ty
          in
          let resolved_inferred =
            List.map
                (fun (name, ty) -> (name, infer_parameter_type name ty))
              inferred
          in
          let inferred =
            reconcile_shared_parameter_variables inferred resolved_inferred
          in
          let lookup_inferred name =
            inferred |> List.assoc_opt name |> Option.value ~default:TUnknown
          in
          let rec refine_destructured_type pattern ty =
            let refine_from_local name ty =
              let inferred_ty = lookup_inferred name in
              Type_solver.unify Type_solver.empty ty inferred_ty
              |> Result.map (fun substitutions ->
                     Type_solver.apply substitutions ty)
              |> Result.value ~default:ty
            in
            match (pattern, ty) with
            | Ast.FSymbol name, ty -> refine_from_local name ty
            | ( Ast.FList
                  [
                    Ast.FSymbol "__type-hint";
                    Ast.FSymbol _;
                    pattern;
                  ],
                ty ) ->
                refine_destructured_type pattern ty
            | Ast.FMap pairs, TRecord fields -> (
                match Destructure.parse_map_pattern pairs with
                | Error _ -> ty
                | Ok parsed ->
                    let fields =
                      List.map
                        (fun (field : field) ->
                          if Types.is_static_record_source_field field then
                            match parsed.as_name with
                            | Some name ->
                                { field with ty = refine_from_local name field.ty }
                            | None -> field
                          else
                            match
                              List.find_opt
                                (fun binding ->
                                  binding.Destructure.keyword = field.keyword)
                                parsed.field_bindings
                            with
                            | None -> field
                            | Some binding ->
                                {
                                  field with
                                  ty =
                                    refine_destructured_type
                                      binding.binding_pattern field.ty;
                                })
                        fields
                    in
                    TRecord fields)
            | _, ty -> ty
          in
          let infer_spec_ty (spec : Destructure.param_spec) =
            if spec.destructured then
              Result.map
                (fun ty ->
                  let ty = infer_named_record scope env ty in
                  refine_destructured_type spec.pattern ty)
                (Destructure.infer_pattern_type spec.pattern lookup_inferred)
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
                |> List.mapi (fun index ((spec : Destructure.param_spec), inferred_ty) ->
                       let inferred_ty =
                         match inferred_ty with
                         | TMap_keys -> Types.dynamic_constraint TMap_keys
                         | TRecord fields ->
                             TRecord
                               (List.map
                                  (fun (field : field) ->
                                    {
                                      field with
                                      ty = infer_named_record scope env field.ty;
                                    })
                                  fields)
                         | ty -> ty
                       in
                       let inferred_ty =
                         collapse_static_record_protocols env inferred_ty
                       in
                       let inferred_ty =
                         infer_named_record scope env inferred_ty
                       in
                       let inferred_ty =
                         match inferred_ty with
                         | TNullable inner
                         | TOcaml_app ("option", [ inner ])
                           when Types.is_dynamic inner
                                && not
                                     (List.mem spec.source_name
                                        option_matched_parameters) ->
                             inner
                         | ty -> ty
                       in
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
                       match spec.Destructure.explicit_ty with
                       | Some ty when not (Types.equal ty TUnknown) ->
                           let explicit_ty = infer_named_record scope env ty in
                           let ty =
                             match (explicit_ty, inferred_ty) with
                             | ( TNamed_record explicit,
                                 TNamed_record inferred )
                               when Type_id.equal explicit.type_id
                                      inferred.type_id ->
                                 inferred_ty
                             | TNamed_record _, inferred
                               when Option.is_some
                                      (Types.protocol_constraint_info inferred)
                               ->
                                 let value_ty =
                                   Type_inference.refine_type explicit_ty
                                     (Types.constraint_value_type inferred)
                                 in
                                 (match
                                    Types.protocol_constraint_info inferred
                                  with
                                 | Some (protocol_id, _, _)
                                   when Protocol.type_satisfies env protocol_id
                                          value_ty ->
                                     value_ty
                                 | Some _ ->
                                     Types.protocol_constraint_with_value
                                       inferred value_ty
                                 | None -> assert false)
                             | ( TFn
                                   ([ TUnknown; TUnknown ], TOcaml "int"),
                                 TFn ([ _; _ ], _) ) ->
                                 Type_inference.refine_type explicit_ty
                                   inferred_ty
                             | _ -> explicit_ty
                           in
                           (spec, ty)
                       | _ -> (
                           match List.nth_opt param_type_overrides index with
                           | Some (Some ty) ->
                               let ty = infer_named_record scope env ty in
                               if Types.equal ty TUnknown then
                                 (spec, inferred_ty)
                               else if Types.equal ty TMap_keys then
                                 (spec,
                                  Type_inference.refine_type ty inferred_ty)
                               else if
                                 (refine_open_overrides
                                 && (contains_open_type ty
                                    || contains_structural_record ty))
                                 || (Option.is_some
                                       (Types.seqable_constraint_info ty)
                                    && match inferred_ty with
                                       | TOcaml_app
                                           ( "Lg_runtime.Runtime_transient.map",
                                             [ _; _ ] ) ->
                                           true
                                       | inferred
                                         when Types.is_dynamic inferred -> (
                                           match
                                             Types.dynamic_constraint_info
                                               inferred
                                           with
                                           | Some
                                               (TOcaml_app
                                                  ( "Lg_runtime.Runtime_transient.map",
                                                    [ _; _ ] )) ->
                                               true
                                           | Some _ | None -> false)
                                       | _ -> false)
                               then
                                 let refined =
                                   match (ty, inferred_ty) with
                                   | TNamed_record _, inferred
                                     when Option.is_some
                                            (Types.protocol_constraint_info
                                               inferred) ->
                                       Type_inference.refine_type ty
                                         (Types.constraint_value_type inferred)
                                   | TRecord _, TNamed_record record
                                     when Types.row_compatible ~expected:ty
                                            ~actual:inferred_ty ->
                                       Type_inference.refine_type ty
                                         (TRecord record.fields)
                                   | _ ->
                                       Type_inference.refine_type ty inferred_ty
                                 in
                                 (spec, refined)
                               else
                                 (spec, ty)
                           | None | Some None -> (spec, inferred_ty)))
              in
              let typed_specs =
                List.map
                  (fun (spec, ty) -> (spec, array_storage_type ty))
                  typed_specs
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
                          Destructure.bind_pattern ?compile_default ~env target
                            spec.pattern
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
                        compile_body scope
                          (Env.with_source_macros_expanded true env)
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
    |> List.map array_storage_type
  in
  let sequence_first_arguments =
    match Semantic_ir.unlocated parts.body.semantic_expr with
    | Semantic_ir.Apply
        (Semantic_ir.Ident "Lg_runtime.Runtime_seq.first", arguments) ->
        Some arguments
    | _ -> None
  in
  let param_tys, return_ty, body_semantic_expr =
    match (parts.body.ty, sequence_first_arguments) with
    | TUnknown, Some arguments ->
        let rec tie index reversed = function
          | [] -> None
          | TOcaml_app (name, [ TUnknown; value_ty ]) :: rest
            when name = Types.seqable_constraint_name
                 || name = Types.optional_seqable_constraint_name
                 || name = Types.optional_sequential_constraint_name ->
              let element_ty = Type_solver.fresh () in
              Some
                ( List.rev_append reversed
                    (TOcaml_app (name, [ element_ty; value_ty ]) :: rest),
                  element_ty )
          | ty :: rest -> tie (index + 1) (ty :: reversed) rest
        in
        (match tie 0 [] param_tys with
        | Some (param_tys, element_ty) ->
            let return_ty = TNullable element_ty in
            ( param_tys,
              return_ty,
              Semantic_ir.annotate return_ty
                (Semantic_ir.Apply
                   ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.first_opt",
                     arguments )) )
        | None -> (param_tys, parts.body.ty, parts.body.semantic_expr))
    | return_ty, _ ->
        (param_tys, array_storage_type return_ty, parts.body.semantic_expr)
  in
  let rec capability_pattern ?value_type name ty =
    match Types.protocol_constraint_info ty with
    | Some (protocol_id, _, value_ty) ->
        Semantic_ir.PTuple
          [
            Semantic_ir.PVar (Types.protocol_witness_name name protocol_id);
            capability_pattern ?value_type name value_ty;
          ]
    | None -> (
        match Types.truthy_constraint_info ty with
        | Some value_ty ->
            Semantic_ir.PTuple
              [
                Semantic_ir.PVar (name ^ "__truthy");
                capability_pattern ?value_type name value_ty;
              ]
        | None -> (
            match Types.nil_predicate_constraint_info ty with
            | Some value_ty ->
                Semantic_ir.PTuple
                  [
                    Semantic_ir.PVar (name ^ "__nil");
                    capability_pattern ?value_type name value_ty;
                  ]
            | None -> (
            match Types.printable_constraint_info ty with
            | Some value_ty ->
                Semantic_ir.PTuple
                  [
                    Semantic_ir.PVar (name ^ "__print");
                    capability_pattern ?value_type name value_ty;
                  ]
            | None -> (
                match Types.hashable_constraint_info ty with
                | Some value_ty ->
                    Semantic_ir.PTuple
                      [
                        Semantic_ir.PVar (name ^ "__hash");
                        capability_pattern ?value_type name value_ty;
                      ]
                | None -> (
                    match Types.comparable_constraint_info ty with
                    | Some value_ty ->
                        Semantic_ir.PTuple
                          [
                            Semantic_ir.PVar (name ^ "__compare");
                            capability_pattern ?value_type name value_ty;
                          ]
                    | None -> (
                match Types.symbol_predicate_constraint_info ty with
                | Some value_ty ->
                    Semantic_ir.PTuple
                      [
                        Semantic_ir.PVar (name ^ "__symbol");
                        capability_pattern ?value_type name value_ty;
                      ]
                | None -> (
        match Types.contains_constraint_info ty with
        | Some (_, value_ty) ->
            Semantic_ir.PTuple
              [
                Semantic_ir.PVar (name ^ "__contains");
                capability_pattern ?value_type name value_ty;
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
                capability_pattern ?value_type name value_ty;
              ]
        | _ ->
            let pattern = Semantic_ir.PVar name in
            Option.fold ~none:pattern
              ~some:(fun type_name ->
                if String.equal type_name "_" then pattern
                else Semantic_ir.PConstraint (pattern, type_name))
              value_type)
        )))))))
  in
  let param_patterns =
    List.map2 (fun name ty -> (name, ty)) param_names param_tys
    |> List.mapi (fun index (name, ty) ->
           let row_type_name =
             List.nth_opt row_param_type_names index |> Option.join
           in
           let pattern =
             if
               Option.is_some (Types.protocol_constraint_info ty)
               || Option.is_some (Types.seqable_constraint_element ty)
               || Option.is_some (Types.truthy_constraint_info ty)
               || Option.is_some (Types.nil_predicate_constraint_info ty)
               || Option.is_some (Types.printable_constraint_info ty)
               || Option.is_some (Types.hashable_constraint_info ty)
               || Option.is_some (Types.comparable_constraint_info ty)
               || Option.is_some
                    (Types.symbol_predicate_constraint_info ty)
               || Option.is_some (Types.contains_constraint_info ty)
             then
               let pattern_ty =
                 match row_type_name with
                 | Some type_name -> apply_row_constraint_type type_name ty
                 | None -> ty
               in
               let value_type =
                 pattern_ty |> Types.constraint_value_type
                 |> pattern_constraint_type |> Types.ocaml_name
               in
               capability_pattern ~value_type name ty
             else
               match row_type_name with
               | Some type_name ->
                   Semantic_ir.PConstraint
                     ( Semantic_ir.PVar name,
                       Types.ocaml_name (apply_row_constraint_type type_name ty) )
            | _ -> (
                match ty with
                | TRecord _ -> Semantic_ir.PVar name
               | _ -> (
                   match param_constraint_name ty with
                   | Some _ ->
                        Semantic_ir.PConstraint
                          ( Semantic_ir.PVar name,
                            Types.ocaml_name (pattern_constraint_type ty) )
                   | None -> Semantic_ir.PVar name))
           in
           match List.nth_opt parts.param_identities index |> Option.join with
           | None -> pattern
           | Some (node_id, location) ->
               Semantic_ir.PLocated (node_id, location, pattern))
  in
  let body_expr =
    match parts.destructured_bindings with
    | [] -> body_semantic_expr
    | bindings ->
        Semantic_ir.Let
          ( List.map
              (fun (binding : Destructure.local_binding) ->
                let pattern =
                  if
                    Option.is_some (Types.protocol_constraint_info binding.ty)
                    || Option.is_some
                         (Types.seqable_constraint_element binding.ty)
                    || Option.is_some
                         (Types.truthy_constraint_info binding.ty)
                    || Option.is_some
                         (Types.nil_predicate_constraint_info binding.ty)
                    || Option.is_some
                         (Types.printable_constraint_info binding.ty)
                    || Option.is_some
                         (Types.hashable_constraint_info binding.ty)
                    || Option.is_some
                         (Types.comparable_constraint_info binding.ty)
                    || Option.is_some
                         (Types.symbol_predicate_constraint_info binding.ty)
                    || Option.is_some
                         (Types.contains_constraint_info binding.ty)
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
            body_semantic_expr )
  in
  let return_param_index =
    let returned_name =
      match Semantic_ir.unlocated parts.body.semantic_expr with
      | Semantic_ir.Ident name -> Some name
      | Semantic_ir.Sequence expressions -> (
          match List.rev expressions with
          | expression :: _ -> (
              match Semantic_ir.unlocated expression with
              | Semantic_ir.Ident name -> Some name
              | _ -> None)
          | [] -> None)
      | _ -> None
    in
    match (parts.destructured_bindings, returned_name) with
    | [], Some returned_name ->
        param_names
        |> List.mapi (fun index name -> (index, name))
        |> List.find_opt (fun (_index, name) -> name = returned_name)
        |> Option.map fst
    | _ -> None
  in
  {
    (typed_ir
       (TFn (param_tys, return_ty))
       (Semantic_ir.Fun (param_patterns, body_expr)))
    with
    return_param_index;
  }
