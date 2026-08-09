open Ast
open Types

let rec string_assoc_opt name = function
  | [] -> None
  | (candidate, value) :: rest ->
      if String.equal name candidate then Some value
      else string_assoc_opt name rest

let string_mem_assoc name entries =
  Option.is_some (string_assoc_opt name entries)

let rec string_remove_assoc name = function
  | [] -> []
  | ((candidate, _) as entry) :: rest ->
      if String.equal name candidate then rest
      else entry :: string_remove_assoc name rest

let rec string_mem name = function
  | [] -> false
  | candidate :: _ when String.equal name candidate -> true
  | _ :: rest -> string_mem name rest

let replace_param name ty params =
  params
  |> List.map (fun (param_name, param_ty) ->
         if String.equal param_name name then (param_name, ty)
         else (param_name, param_ty))

let has_source_name name expected =
  name = expected || String.ends_with ~suffix:("/" ^ expected) name

let deduplicate_protocol_constraints ty =
  let rec deduplicate seen ty =
    match Types.dynamic_constraint_info ty with
    | Some capability -> Types.dynamic_constraint (deduplicate seen capability)
    | None -> (
        match Types.protocol_constraint_info ty with
        | Some (protocol_id, _, value_ty) ->
            if List.exists (Protocol_id.equal protocol_id) seen then
              deduplicate seen value_ty
            else
              Types.protocol_constraint_with_value ty
                (deduplicate (protocol_id :: seen) value_ty)
        | None -> ty)
  in
  deduplicate [] ty

let rec refine_type existing inferred =
  match (existing, inferred) with
  | TUnknown, inferred -> inferred
  | existing, TUnknown -> existing
  | existing, inferred
    when Types.is_dynamic existing && Types.is_dynamic inferred ->
      let existing_capability =
        Types.dynamic_constraint_info existing |> Option.value ~default:TUnknown
      in
      let inferred_capability =
        Types.dynamic_constraint_info inferred |> Option.value ~default:TUnknown
      in
      Types.dynamic_constraint
        (refine_type existing_capability inferred_capability)
  | existing, inferred
    when Types.is_dynamic existing
         && Option.fold ~none:false ~some:Type_solver.is_open
              (Types.dynamic_constraint_info existing) ->
      inferred
  | existing, inferred
    when Types.is_dynamic inferred
         && Option.fold ~none:false ~some:Type_solver.is_open
              (Types.dynamic_constraint_info inferred) ->
      existing
  | existing, inferred
    when Option.is_some (Types.symbol_predicate_constraint_info existing)
         && Types.is_dynamic inferred ->
      existing
  | existing, inferred
    when Types.is_dynamic existing
         && Option.is_some (Types.symbol_predicate_constraint_info inferred) ->
      inferred
  | _, inferred
    when Option.is_some (Types.symbol_predicate_constraint_info inferred) ->
      inferred
  | existing, inferred when Types.is_dynamic existing ->
      let capability =
        Types.dynamic_constraint_info existing |> Option.value ~default:TUnknown
      in
      Types.dynamic_constraint (refine_type capability inferred)
  | (TOcaml_app (name, [ element_ty; value_ty ]) as existing), inferred
    when Types.is_dynamic inferred
         && (name = Types.seqable_constraint_name
            || name = Types.optional_seqable_constraint_name
            || name = Types.optional_sequential_constraint_name) ->
      (match Types.dynamic_constraint_info inferred with
      | Some
          (TOcaml_app ("Lg_runtime.Runtime_transient.map", [ _; _ ])) ->
          inferred
      | Some _ | None ->
          let value_ty = refine_type value_ty inferred in
          if Types.equal value_ty (Types.constraint_value_type existing) then
            existing
          else TOcaml_app (name, [ element_ty; value_ty ]))
  | existing, inferred when Types.is_dynamic inferred ->
      let capability =
        Types.dynamic_constraint_info inferred |> Option.value ~default:TUnknown
      in
      Types.dynamic_constraint (refine_type existing capability)
  | existing, inferred
    when Option.is_some (Types.truthy_constraint_info existing)
         && Option.is_some (Types.truthy_constraint_info inferred) ->
      let existing_value = Types.truthy_constraint_info existing |> Option.get in
      let inferred_value = Types.truthy_constraint_info inferred |> Option.get in
      Types.truthy_constraint (refine_type existing_value inferred_value)
  | existing, inferred -> (
      match
        ( Types.protocol_constraint_info existing,
          Types.protocol_constraint_info inferred )
      with
      | ( Some (existing_id, _, existing_value),
          Some (inferred_id, _, inferred_value) )
        when Protocol_id.equal existing_id inferred_id ->
          Types.protocol_constraint_with_value existing
            (refine_type existing_value inferred_value)
      | _ -> refine_nonmatching_type existing inferred)

and refine_nonmatching_type existing inferred =
  match (existing, inferred) with
  | existing, inferred
    when Option.is_some (Types.truthy_constraint_info existing) ->
      let value_ty = Types.truthy_constraint_info existing |> Option.get in
      Types.truthy_constraint (refine_type value_ty inferred)
  | existing, inferred
    when Option.is_some (Types.contains_constraint_info existing)
         && Option.is_some (Types.contains_constraint_info inferred) ->
      let existing_key, existing_value =
        Types.contains_constraint_info existing |> Option.get
      in
      let inferred_key, inferred_value =
        Types.contains_constraint_info inferred |> Option.get
      in
      Types.contains_constraint_with_value
        (refine_type existing_key inferred_key)
        (refine_type existing_value inferred_value)
  | existing, inferred
    when Option.is_some (Types.contains_constraint_info existing) ->
      let key_ty, value_ty =
        Types.contains_constraint_info existing |> Option.get
      in
      Types.contains_constraint_with_value key_ty
        (refine_type value_ty inferred)
  | existing, inferred
    when Option.is_some (Types.contains_constraint_info inferred) ->
      let key_ty, value_ty =
        Types.contains_constraint_info inferred |> Option.get
      in
      Types.contains_constraint_with_value key_ty
        (refine_type existing value_ty)
  | TMap_keys, ((TRecord _ | TNamed_record _) as map_ty)
  | ((TRecord _ | TNamed_record _) as map_ty), TMap_keys ->
      map_ty
  | TMap_keys, inferred when Option.is_some (Types.dynamic_map_types inferred) ->
      inferred
  | existing, TMap_keys when Option.is_some (Types.dynamic_map_types existing) ->
      existing
  | TMap_keys, inferred
    when Option.is_some (Types.seqable_constraint_info inferred) ->
      Types.dynamic_map TKeyword (Types.dynamic_constraint TUnknown)
  | existing, TMap_keys
    when Option.is_some (Types.seqable_constraint_info existing) ->
      Types.dynamic_map TKeyword (Types.dynamic_constraint TUnknown)
  | existing, inferred
    when Option.is_some (Types.seqable_constraint_info existing)
         && Option.is_some (Types.dynamic_map_types inferred) ->
      inferred
  | existing, inferred
    when Option.is_some (Types.dynamic_map_types existing)
         && Option.is_some (Types.seqable_constraint_info inferred) ->
      existing
  | (TOcaml_app ("Lg_runtime.Runtime_transient.map", [ _; _ ]) as existing),
    inferred
    when Option.is_some (Types.seqable_constraint_info inferred) ->
      existing
  | existing,
    (TOcaml_app ("Lg_runtime.Runtime_transient.map", [ _; _ ]) as inferred)
    when Option.is_some (Types.seqable_constraint_info existing) ->
      inferred
  | existing, TFn ([ key_ty ], value_ty)
    when Option.is_some (Types.dynamic_map_types existing) ->
      let map_key_ty, map_value_ty = Option.get (Types.dynamic_map_types existing) in
      Types.dynamic_map
        (refine_type map_key_ty key_ty)
        (refine_type map_value_ty value_ty)
  | TFn ([ key_ty ], value_ty), inferred
    when Option.is_some (Types.dynamic_map_types inferred) ->
      let map_key_ty, map_value_ty = Option.get (Types.dynamic_map_types inferred) in
      Types.dynamic_map
        (refine_type map_key_ty key_ty)
        (refine_type map_value_ty value_ty)
  | existing, inferred
    when Option.is_some (Types.protocol_constraint_info existing) -> (
      match Types.protocol_constraint_info existing with
      | Some (_, _, value_ty) ->
          Types.protocol_constraint_with_value existing
            (refine_type value_ty inferred)
      | None -> existing)
  | existing, inferred
    when Option.is_some (Types.protocol_constraint_info inferred) -> (
      match existing with
      | TNamed_record _ ->
          Types.protocol_constraint_with_value inferred existing
      | TUnknown | TMeta _ | TVar _ -> (
          match Types.protocol_constraint_info inferred with
          | Some (_, _, value_ty) ->
              Types.protocol_constraint_with_value inferred
                (refine_type existing value_ty)
          | None -> existing)
      | _ -> existing)
  | (TNullable _ | TOcaml_app ("option", [ _ ])), inferred
    when Option.is_some (Types.seqable_constraint_info inferred) ->
      let kind, element_ty, value_ty =
        Types.seqable_constraint_info inferred |> Option.get
      in
      let value_ty = refine_type existing value_ty in
      (match kind with
      | `Required | `Optional ->
          Types.optional_seqable_constraint element_ty value_ty
      | `Optional_sequential ->
          Types.optional_sequential_constraint element_ty value_ty)
  | existing, (TNullable _ | TOcaml_app ("option", [ _ ]))
    when Option.is_some (Types.seqable_constraint_info existing) ->
      let kind, element_ty, value_ty =
        Types.seqable_constraint_info existing |> Option.get
      in
      let value_ty = refine_type value_ty inferred in
      (match kind with
      | `Required | `Optional ->
          Types.optional_seqable_constraint element_ty value_ty
      | `Optional_sequential ->
          Types.optional_sequential_constraint element_ty value_ty)
  | TNullable existing, TNullable inferred ->
      Types.normalize_nullable (TNullable (refine_type existing inferred))
  | TNullable existing, TOcaml_app ("option", [ inferred ])
  | TOcaml_app ("option", [ existing ]), TNullable inferred ->
      TOcaml_app ("option", [ refine_type existing inferred ])
  | (TNullable existing | TOcaml_app ("option", [ existing ])), inferred
    when Option.is_some (Types.protocol_constraint_info inferred) ->
      Types.normalize_nullable (TNullable (refine_type existing inferred))
  | TNullable existing, inferred ->
      Types.normalize_nullable (TNullable (refine_type existing inferred))
  | ( TOcaml_app (existing_name, existing_args),
      TOcaml_app (inferred_name, inferred_args) )
    when existing_name = inferred_name
         && List.length existing_args = List.length inferred_args ->
      TOcaml_app
        (existing_name, List.map2 refine_type existing_args inferred_args)
  | TList existing, inferred
    when Option.is_some (Types.seqable_constraint_element inferred) ->
      TList
        (refine_type existing
           (Option.get (Types.seqable_constraint_element inferred)))
  | TVector existing, inferred
    when Option.is_some (Types.seqable_constraint_element inferred) ->
      TVector
        (refine_type existing
           (Option.get (Types.seqable_constraint_element inferred)))
  | existing, TVector inferred
    when Option.is_some (Types.seqable_constraint_element existing) ->
      TVector
        (refine_type
           (Option.get (Types.seqable_constraint_element existing))
           inferred)
  | TSeq existing, inferred
    when Option.is_some (Types.seqable_constraint_element inferred) ->
      TSeq
        (refine_type existing
           (Option.get (Types.seqable_constraint_element inferred)))
  | TOcaml_app (name, [ existing ]), inferred
    when name = Types.next_seq_type_name
         && Option.is_some (Types.seqable_constraint_element inferred) ->
      Types.next_seq
        (refine_type existing
           (Option.get (Types.seqable_constraint_element inferred)))
  | TArray existing, TArray inferred -> TArray (refine_type existing inferred)
  | TRef existing, TRef inferred -> TRef (refine_type existing inferred)
  | TList existing, TList inferred -> TList (refine_type existing inferred)
  | TVector existing, TVector inferred ->
      TVector (refine_type existing inferred)
  | TSet existing, TSet inferred -> TSet (refine_type existing inferred)
  | TFn ([ predicate_arg ], TBool), TSet element
  | TSet element, TFn ([ predicate_arg ], TBool) ->
      TSet (refine_type element predicate_arg)
  | TSeq existing, TSeq inferred -> TSeq (refine_type existing inferred)
  | TNamed_record existing, TNamed_record inferred
    when Type_id.equal existing.type_id inferred.type_id ->
      let type_arguments =
        if
          List.length existing.type_arguments
          = List.length inferred.type_arguments
        then List.map2 refine_type existing.type_arguments inferred.type_arguments
        else existing.type_arguments
      in
      TNamed_record
        {
          existing with
          type_arguments;
          fields = merge_record_fields existing.fields inferred.fields;
        }
  | (TRecord _ as structural), (TNamed_record _ as named)
    when Types.row_compatible ~expected:structural ~actual:named ->
      named
  | (TNamed_record _ as named), (TRecord _ as structural)
    when Types.row_compatible ~expected:structural ~actual:named ->
      named
  | TRecord existing, TRecord inferred ->
      TRecord (merge_record_fields existing inferred)
  | (TMeta _ | TVar _), inferred -> inferred
  | existing, (TMeta _ | TVar _) -> existing
  | ( TFn (existing_params, existing_return),
      TFn (inferred_params, inferred_return) )
    when List.length existing_params = List.length inferred_params ->
      TFn
        ( List.map2 refine_type existing_params inferred_params,
          refine_type existing_return inferred_return )
  | existing, _ -> existing

and merge_record_fields existing inferred =
  List.fold_left
    (fun fields (inferred_field : field) ->
      match find_field inferred_field.keyword fields with
      | None -> fields @ [ inferred_field ]
      | Some existing_field ->
          List.map
            (fun (field : field) ->
              if field.keyword = inferred_field.keyword then
                {
                  existing_field with
                  ty = refine_type existing_field.ty inferred_field.ty;
                }
              else field)
            fields)
    existing inferred

let refine_returned_seqable_vector params branch other_ty =
  match (branch, other_ty) with
  | FSymbol name, TVector other_element -> (
      match string_assoc_opt name params with
      | Some current -> (
          match Types.seqable_constraint_info current with
          | Some (`Required, current_element, (TUnknown | TMeta _ | TVar _)) ->
              let element_ty = refine_type current_element other_element in
              let element_ty =
                match element_ty with
                | TUnknown | TMeta _ | TVar _ -> Types.dynamic_constraint TUnknown
                | ty -> ty
              in
              replace_param name
                (TVector element_ty) params
          | Some (`Required, _, _)
          | Some ((`Optional | `Optional_sequential), _, _)
          | None -> params)
      | None -> params)
  | _ -> params

let same_refinable_wrapper left right =
  match (left, right) with
  | TNullable _, TNullable _
  | TNullable _, TOcaml_app ("option", [ _ ])
  | TOcaml_app ("option", [ _ ]), TNullable _
  | TArray _, TArray _
  | TRef _, TRef _
  | TList _, TList _
  | TVector _, TVector _
  | TSet _, TSet _
  | TSeq _, TSeq _
  | TRecord _, TRecord _ ->
      true
  | TOcaml_app (left_name, left_args), TOcaml_app (right_name, right_args) ->
      left_name = right_name && List.length left_args = List.length right_args
  | TTuple left, TTuple right -> List.length left = List.length right
  | TFn (left_params, _), TFn (right_params, _) ->
      List.length left_params = List.length right_params
  | _ -> false

let is_edn_value_type = Edn_value_elaborator.is_value_type

let edn_function_argument_compatible expected actual =
  is_edn_value_type actual
  && Option.is_some (Types.seqable_constraint_info expected)

let edn_function_call_compatible callee call =
  match (callee, call) with
  | TFn (callee_params, _), TFn (call_params, _)
    when List.length callee_params = List.length call_params ->
      List.for_all2
        (fun expected actual ->
          Result.is_ok (Type_solver.unify [] expected actual)
          || edn_function_argument_compatible expected actual)
        callee_params call_params
  | _ -> false

let rec constrain_symbol expected_ty params name =
  match string_assoc_opt name params with
  | None -> Ok params
  | Some (TFn _ as existing_ty)
    when (match expected_ty with TFn _ -> true | _ -> false) ->
      let scheme = Type_solver.generalize existing_ty in
      if
        List.exists
          (function Declared_variable _ -> true | Inferred_variable _ -> false)
          scheme.quantified
      then
        let instantiated = Type_solver.instantiate scheme in
        (match Type_solver.unify [] instantiated expected_ty with
        | Ok _ -> Ok params
        | Error _
          when edn_function_call_compatible instantiated expected_ty ->
            Ok params
        | Error _ ->
            Error.error
              (name ^ " called with incompatible arguments: expected "
             ^ Types.source_name instantiated ^ ", got "
             ^ Types.source_name expected_ty))
      else constrain_monomorphic_symbol expected_ty params name existing_ty
  | Some existing_ty ->
      constrain_monomorphic_symbol expected_ty params name existing_ty

and constrain_monomorphic_symbol expected_ty params name existing_ty =
      let substitutions =
        Type_solver.unify [] existing_ty expected_ty
        |> Result.value ~default:[]
      in
      let params =
        List.map
          (fun (param_name, param_ty) ->
            (param_name, Type_solver.apply substitutions param_ty))
          params
      in
      let existing_ty = Type_solver.apply substitutions existing_ty in
      let expected_ty = Type_solver.apply substitutions expected_ty in
      let refined = refine_type existing_ty expected_ty in
      Ok (replace_param name refined params)

let constrain_truthy_symbol params name =
  match string_assoc_opt name params with
  | Some ty when Option.is_some (Types.truthy_constraint_info ty) -> Ok params
  | Some TUnknown ->
      Ok
        (replace_param name
           (Types.truthy_constraint (Type_solver.fresh ()))
           params)
  | Some ((TMeta _ | TVar _) as value_ty) ->
      Ok (replace_param name (Types.truthy_constraint value_ty) params)
  | Some _ | None -> Ok params

let constrain_printable_symbol params name =
  match string_assoc_opt name params with
  | Some ty when Option.is_some (Types.printable_constraint_info ty) ->
      Ok params
  | Some TUnknown ->
      Ok
        (replace_param name
           (Types.printable_constraint (Type_solver.fresh ()))
           params)
  | Some ((TMeta _ | TVar _) as value_ty) ->
      Ok (replace_param name (Types.printable_constraint value_ty) params)
  | Some _ | None -> Ok params

let constrain_symbol_predicate params name =
  match string_assoc_opt name params with
  | Some ty
    when Option.is_some (Types.symbol_predicate_constraint_info ty) ->
      Ok params
  | Some TUnknown ->
      Ok
        (replace_param name
           (Types.symbol_predicate_constraint (Type_solver.fresh ()))
           params)
  | Some ((TMeta _ | TVar _) as value_ty) ->
      Ok
        (replace_param name
           (Types.symbol_predicate_constraint value_ty)
           params)
  | Some _ | None -> Ok params

let rec materialize_dynamic_unknown = function
  | TUnknown -> Types.dynamic_constraint TUnknown
  | ty when Types.is_dynamic ty -> ty
  | (TVar _ as type_parameter) -> type_parameter
  | TNullable inner -> TNullable (materialize_dynamic_unknown inner)
  | TArray inner -> TArray (materialize_dynamic_unknown inner)
  | TRef inner -> TRef (materialize_dynamic_unknown inner)
  | TList inner -> TList (materialize_dynamic_unknown inner)
  | TVector inner -> TVector (materialize_dynamic_unknown inner)
  | TSet inner -> TSet (materialize_dynamic_unknown inner)
  | TSeq inner -> TSeq (materialize_dynamic_unknown inner)
  | TOcaml_app (name, arguments) ->
      TOcaml_app (name, List.map materialize_dynamic_unknown arguments)
  | TRecord fields ->
      TRecord
        (List.map
           (fun (field : field) ->
             { field with ty = materialize_dynamic_unknown field.ty })
           fields)
  | TFn (parameters, return_ty) ->
      TFn
        ( List.map materialize_dynamic_unknown parameters,
          materialize_dynamic_unknown return_ty )
  | ty -> ty

let rec stored_value_type ty =
  match Types.protocol_constraint_info ty with
  | Some (_, _, value_ty) -> stored_value_type value_ty
  | None -> ty

let record_field_type params receiver keyword =
  match string_assoc_opt receiver params with
  | None -> None
  | Some receiver_ty -> (
      match Types.record_fields receiver_ty with
      | None -> None
      | Some fields ->
          Option.map
            (fun (field : field) -> field.ty)
            (Types.find_field keyword fields))

let record_ref_field_value_type params receiver keyword =
  match record_field_type params receiver keyword with
  | Some (TRef value_ty) -> Some value_ty
  | Some _ | None -> None

let record_mutable_field_value_type params receiver keyword =
  match string_assoc_opt receiver params with
  | None -> None
  | Some receiver_ty -> (
      match Types.record_fields receiver_ty with
      | None -> None
      | Some fields -> (
          match Types.find_field keyword fields with
          | Some { mutable_ = true; ty; _ } -> Some ty
          | Some _ | None -> None))

let rec assoc_root_symbol = function
  | FSymbol name -> Some name
  | FList
      (FSymbol ("assoc" | "clojure.core/assoc") :: target :: _) ->
      assoc_root_symbol target
  | _ -> None

let constrain_comparable_symbol params name =
  match string_assoc_opt name params with
  | Some (TNullable _ | TOcaml_app ("option", [ _ ])) ->
      Ok (replace_param name (Types.dynamic_constraint TUnknown) params)
  | _ -> Ok params

let constrain_seqable element_ty params name =
  let rec add_constraint = function
    | TUnknown | TMeta _ | TVar _ -> Types.seqable_constraint element_ty
    | TMap_keys ->
        Types.dynamic_map TKeyword (Types.dynamic_constraint TUnknown)
    | TRecord _ as map_ty ->
        Types.dynamic_constraint
          (Types.seqable_constraint_with_value
             (Types.seqable_constraint element_ty)
             map_ty)
    | TOcaml_app (constraint_name, [ existing_element; value_ty ])
      when constraint_name = Types.seqable_constraint_name
           || constraint_name = Types.optional_seqable_constraint_name
           || constraint_name = Types.optional_sequential_constraint_name ->
        let element_ty =
          if Types.equal existing_element TUnknown then element_ty
          else if Types.is_dynamic existing_element then existing_element
          else refine_type existing_element element_ty
        in
        if constraint_name = Types.seqable_constraint_name then
          Types.seqable_constraint_with_value element_ty value_ty
        else if constraint_name = Types.optional_seqable_constraint_name then
          Types.optional_seqable_constraint element_ty value_ty
        else Types.optional_sequential_constraint element_ty value_ty
    | TNamed_record { type_parameters = [ parameter ]; _ } as record_ty ->
        Types.substitute_type_variables
          [ (Type_solver.Declared parameter, element_ty) ]
          record_ty
    | TVector existing_element ->
        TVector (refine_type existing_element element_ty)
    | TList existing_element ->
        TList (refine_type existing_element element_ty)
    | TSeq existing_element ->
        TSeq (refine_type existing_element element_ty)
    | TSet existing_element ->
        TSet (refine_type existing_element element_ty)
    | TArray existing_element ->
        TArray (refine_type existing_element element_ty)
    | existing when Option.is_some (Types.truthy_constraint_info existing) ->
        Types.truthy_constraint
          (add_constraint (Option.get (Types.truthy_constraint_info existing)))
    | existing
      when Option.is_some (Types.nil_predicate_constraint_info existing) ->
        Types.nil_predicate_constraint
          (add_constraint
             (Option.get (Types.nil_predicate_constraint_info existing)))
    | existing when Option.is_some (Types.printable_constraint_info existing) ->
        Types.printable_constraint
          (add_constraint
             (Option.get (Types.printable_constraint_info existing)))
    | existing
      when Option.is_some (Types.symbol_predicate_constraint_info existing) ->
        Types.symbol_predicate_constraint
          (add_constraint
             (Option.get (Types.symbol_predicate_constraint_info existing)))
    | existing when Option.is_some (Types.contains_constraint_info existing) ->
        let key_ty, value_ty =
          Option.get (Types.contains_constraint_info existing)
        in
        Types.contains_constraint_with_value key_ty (add_constraint value_ty)
    | existing -> (
        match Types.protocol_constraint_info existing with
        | Some (_, _, value_ty) ->
            Types.protocol_constraint_with_value existing
              (add_constraint value_ty)
        | None -> existing)
  in
  match string_assoc_opt name params with
  | None -> Ok params
  | Some existing -> Ok (replace_param name (add_constraint existing) params)

let constrain_contains key_ty params name =
  let add_constraint = function
    | TUnknown | TMeta _ | TVar _ -> Types.contains_constraint key_ty
    | existing -> (
        match Types.contains_constraint_info existing with
        | Some (existing_key, value_ty) ->
            Types.contains_constraint_with_value
              (refine_type existing_key key_ty)
              value_ty
        | None ->
            Types.contains_constraint_with_value key_ty existing)
  in
  match string_assoc_opt name params with
  | None -> Ok params
  | Some existing -> Ok (replace_param name (add_constraint existing) params)

let constrain_optional_seqable ?(sequential = false) element_ty params name =
  let make_optional element_ty value_ty =
    if sequential then Types.optional_sequential_constraint element_ty value_ty
    else Types.optional_seqable_constraint element_ty value_ty
  in
  let rec add_constraint = function
    | (TUnknown | TMeta _ | TVar _) as value_ty ->
        make_optional element_ty (TNullable value_ty)
    | (TNullable _ | TOcaml_app ("option", [ _ ])) as value_ty ->
        make_optional element_ty value_ty
    | TOcaml_app (constraint_name, [ existing_element; value_ty ])
      when constraint_name = Types.seqable_constraint_name
           || constraint_name = Types.optional_seqable_constraint_name
           || constraint_name = Types.optional_sequential_constraint_name ->
        let element_ty =
          if Types.equal existing_element TUnknown then element_ty
          else existing_element
        in
        if constraint_name = Types.seqable_constraint_name then
          Types.seqable_constraint_with_value element_ty value_ty
        else if constraint_name = Types.optional_seqable_constraint_name then
          Types.optional_seqable_constraint element_ty value_ty
        else Types.optional_sequential_constraint element_ty value_ty
    | existing -> (
        match Types.protocol_constraint_info existing with
        | Some (_, _, value_ty) ->
            Types.protocol_constraint_with_value existing
              (add_constraint value_ty)
        | None -> existing)
  in
  match string_assoc_opt name params with
  | None -> Ok params
  | Some existing -> Ok (replace_param name (add_constraint existing) params)

let add_record_field_constraint name keyword field_ty params =
  let merge_nested_fields fields inferred_fields =
    let rec same_open_shape left right =
      Types.equal left right
      ||
      match (left, right) with
      | (TUnknown | TMeta _ | TVar _), _
      | _, (TUnknown | TMeta _ | TVar _) ->
          true
      | TNullable left, TNullable right
      | TNullable left, TOcaml_app ("option", [ right ])
      | TOcaml_app ("option", [ left ]), TNullable right ->
          same_open_shape left right
      | TArray left, TArray right
      | TRef left, TRef right
      | TList left, TList right
      | TVector left, TVector right
      | TSet left, TSet right
      | TSeq left, TSeq right ->
          same_open_shape left right
      | TOcaml_app (left_name, left_args), TOcaml_app (right_name, right_args) ->
          left_name = right_name
          && List.length left_args = List.length right_args
          && List.for_all2 same_open_shape left_args right_args
      | TTuple left, TTuple right ->
          List.length left = List.length right
          && List.for_all2 same_open_shape left right
      | _ -> false
    in
    let statically_seqable = function
      | TRecord _ | TNamed_record _ | TMap_keys
      | TOcaml_app ("Lg_runtime.Runtime_map.t", [ _; _ ]) ->
          true
      | _ -> false
    in
    let merge_nested fields (inferred : field) =
      match find_field inferred.keyword fields with
      | None -> Ok (inferred :: fields)
      | Some existing when Types.equal existing.ty inferred.ty -> Ok fields
      | Some existing
        when (match existing.ty with
             | TUnknown | TMeta _ | TVar _ -> true
             | _ -> false) ->
          Ok
            (inferred
            :: List.filter
                 (fun field -> field.keyword <> inferred.keyword)
                 fields)
      | Some _
        when (match inferred.ty with
             | TUnknown | TMeta _ | TVar _ -> true
             | _ -> false) ->
          Ok fields
      | Some existing when same_open_shape existing.ty inferred.ty ->
          Ok
            ( { inferred with ty = refine_type existing.ty inferred.ty }
            :: List.filter
                 (fun field -> field.keyword <> inferred.keyword)
                 fields )
      | Some existing
        when statically_seqable existing.ty
             && Option.is_some
                  (Types.seqable_constraint_info inferred.ty) ->
          Ok fields
      | Some existing
        when statically_seqable inferred.ty
             && Option.is_some
                  (Types.seqable_constraint_info existing.ty) ->
          Ok
            (inferred
            :: List.filter
                 (fun field -> field.keyword <> inferred.keyword)
                 fields)
      | Some existing ->
          Error.error
            ("cannot infer " ^ inferred.keyword ^ " as "
           ^ Types.source_name inferred.ty ^ " because it is already "
            ^ Types.source_name existing.ty)
    in
    List.fold_left
      (fun result inferred ->
        Result.bind result (fun fields -> merge_nested fields inferred))
      (Ok fields) inferred_fields
  in
  let merge_fields fields =
    let directly_seqable = function
      | TList _ | TVector _ | TSeq _
      | TOcaml_app ("Lg_runtime.Runtime_map.t", [ _; _ ]) ->
          true
      | _ -> false
    in
    let replace_field_type ty =
      Ok
        (make_field keyword ty
        :: List.filter
             (fun candidate -> candidate.keyword <> keyword)
             fields)
    in
    match find_field keyword fields with
    | None -> Ok (make_field keyword field_ty :: fields)
    | Some field when Types.equal field.ty field_ty -> Ok fields
    | Some field -> (
        match (field.ty, field_ty) with
        | (TUnknown | TMeta _ | TVar _), field_ty ->
            Ok
              (make_field keyword field_ty
              :: List.filter
                   (fun candidate -> candidate.keyword <> keyword)
                   fields)
        | _, (TUnknown | TMeta _ | TVar _) -> Ok fields
        | TRef TUnknown, TRef value_ty ->
            Ok
              (make_field keyword (TRef value_ty)
              :: List.filter
                   (fun candidate -> candidate.keyword <> keyword)
                   fields)
        | TRef _, TRef TUnknown -> Ok fields
        | ( TNullable (TRecord existing_fields),
            TNullable (TRecord inferred_fields) ) ->
            Result.bind
              (merge_nested_fields existing_fields inferred_fields)
              (fun nested_fields ->
                Ok
                  (make_field keyword (TNullable (TRecord nested_fields))
                  :: List.filter
                       (fun candidate -> candidate.keyword <> keyword)
                       fields))
        | TRecord existing_fields, TNullable (TRecord inferred_fields) ->
            Result.map
              (fun nested_fields ->
                make_field keyword (TNullable (TRecord nested_fields))
                :: List.filter
                     (fun candidate -> candidate.keyword <> keyword)
                     fields)
              (merge_nested_fields existing_fields inferred_fields)
        | TNullable (TRecord existing_fields), TRecord inferred_fields ->
            Result.map
              (fun nested_fields ->
                make_field keyword (TNullable (TRecord nested_fields))
                :: List.filter
                     (fun candidate -> candidate.keyword <> keyword)
                     fields)
              (merge_nested_fields existing_fields inferred_fields)
        | TNamed_record _ as existing, TNullable (TRecord inferred_fields)
          when Types.row_compatible ~expected:(TRecord inferred_fields)
                 ~actual:existing ->
            replace_field_type (TNullable existing)
        | ( TNullable (TNamed_record _ as existing),
            (TRecord inferred_fields | TNullable (TRecord inferred_fields)) )
          when Types.row_compatible ~expected:(TRecord inferred_fields)
                 ~actual:existing ->
            Ok fields
        | existing, inferred
          when Types.is_dynamic existing || Types.is_dynamic inferred
               || Option.is_some (Types.protocol_constraint_info existing)
               || Option.is_some (Types.protocol_constraint_info inferred) ->
            Ok
              (make_field keyword (refine_type existing inferred)
              :: List.filter
                   (fun candidate -> candidate.keyword <> keyword)
                   fields)
        | existing, inferred when same_refinable_wrapper existing inferred ->
            Ok
              (make_field keyword (refine_type existing inferred)
              :: List.filter
                   (fun candidate -> candidate.keyword <> keyword)
                   fields)
        | (TNamed_record { type_parameters = [ parameter ]; _ } as existing),
          inferred
          when Option.is_some (Types.seqable_constraint_element inferred) ->
            let element_ty =
              Option.get (Types.seqable_constraint_element inferred)
            in
            Ok
              (make_field keyword
                 (Types.substitute_type_variables
                    [ (Type_solver.Declared parameter, element_ty) ]
                    existing)
              :: List.filter
                   (fun candidate -> candidate.keyword <> keyword)
                   fields)
        | ((TRecord _ | TNamed_record _) as existing), inferred
          when Option.is_some (Types.seqable_constraint_info inferred) ->
            Ok
              (make_field keyword existing
              :: List.filter
                   (fun candidate -> candidate.keyword <> keyword)
                   fields)
        | concrete, seqable
          when directly_seqable concrete
               && Option.is_some (Types.seqable_constraint_info seqable) ->
            replace_field_type (refine_type concrete seqable)
        | seqable, concrete
          when directly_seqable concrete
               && Option.is_some (Types.seqable_constraint_info seqable) ->
            replace_field_type (refine_type concrete seqable)
        | existing, ((TRecord _ | TNamed_record _) as inferred)
          when Option.is_some (Types.seqable_constraint_info existing) ->
            Ok
              (make_field keyword inferred
              :: List.filter
                   (fun candidate -> candidate.keyword <> keyword)
                   fields)
        | TMap_keys, inferred
          when Option.is_some (Types.seqable_constraint_info inferred) ->
            Ok
              (make_field keyword (refine_type TMap_keys inferred)
              :: List.filter
                   (fun candidate -> candidate.keyword <> keyword)
                   fields)
        | existing, TMap_keys
          when Option.is_some (Types.seqable_constraint_info existing) ->
            Ok
              (make_field keyword (refine_type existing TMap_keys)
              :: List.filter
                   (fun candidate -> candidate.keyword <> keyword)
                   fields)
        | _ ->
            Error.error
              ("cannot infer " ^ keyword ^ " as " ^ Types.source_name field_ty
             ^ " because it is already " ^ Types.source_name field.ty))
  in
  let rec add_constraint = function
    | TUnknown | TMeta _ | TVar _ -> Ok (TRecord [ make_field keyword field_ty ])
    | TMap_keys ->
        Ok
          (Types.dynamic_map TKeyword
             (Types.dynamic_constraint TUnknown))
    | existing when Option.is_some (Types.contains_constraint_info existing) ->
        let key_ty, value_ty =
          Types.contains_constraint_info existing |> Option.get
        in
        Result.map
          (Types.contains_constraint_with_value key_ty)
          (add_constraint value_ty)
    | TNullable inner ->
        Result.map (fun inner -> TNullable inner) (add_constraint inner)
    | TOcaml_app ("option", [ inner ]) ->
        Result.map
          (fun inner -> TOcaml_app ("option", [ inner ]))
          (add_constraint inner)
    | TRecord fields ->
        Result.map (fun fields -> TRecord fields) (merge_fields fields)
    | TNamed_record record as record_ty -> (
        match Types.find_field keyword record.fields with
        | None -> Ok record_ty
        | Some field -> (
            let constrained_parameters =
              Type_solver.variables field.ty
              |> List.filter (function
                   | Type_solver.Declared name ->
                       string_mem name record.type_parameters
                   | Type_solver.Metavariable _ -> false)
            in
            let inferred_ty =
              match
                (field.ty, Types.seqable_constraint_element field_ty)
              with
              | ( TNamed_record
                    { type_parameters = [ parameter ]; _ } as named,
                  Some element_ty ) ->
                  Types.substitute_type_variables
                    [ (Type_solver.Declared parameter, element_ty) ]
                    named
              | (TRecord _ | TNamed_record _ | TMap_keys), Some _ ->
                  field.ty
              | _ -> stored_value_type field_ty
            in
            match (constrained_parameters, inferred_ty) with
            | [], inferred_ty
              when (match field.ty with
                   | TUnknown | TMeta _ | TVar _ -> true
                   | _ -> false)
                   &&
                   not
                     (match inferred_ty with
                     | TUnknown | TMeta _ | TVar _ -> true
                     | _ -> false) ->
                Result.map
                  (fun fields -> TNamed_record { record with fields })
                  (merge_fields record.fields)
            | [], _ -> Ok record_ty
            | _, (TUnknown | TMeta _ | TVar _) -> Ok record_ty
            | _,
              inferred_ty
              when (match field.ty with
                   | TOcaml_app ("Lg_runtime.Runtime_map.t", [ _; _ ]) ->
                       true
                   | _ -> false)
                   && Option.is_some
                        (Types.seqable_constraint_info inferred_ty) ->
                Ok record_ty
            | _, inferred_ty -> (
            match Type_solver.unify [] field.ty inferred_ty with
            | Ok substitutions ->
                Ok (Type_solver.apply substitutions record_ty)
            | Error _
              when (match field.ty with TNamed_record _ -> true | _ -> false)
                   && Types.row_compatible ~expected:field.ty
                        ~actual:
                          (match inferred_ty with
                          | TNullable ty -> ty
                          | ty -> ty) ->
                Ok record_ty
            | Error _ ->
                Error.error
                  ("cannot infer " ^ keyword ^ " as "
                 ^ Types.source_name inferred_ty ^ " because it is already "
                 ^ Types.source_name field.ty))))
    | ty when Option.is_some (Types.truthy_constraint_info ty) ->
        let value_ty = Types.truthy_constraint_info ty |> Option.get in
        let value_ty =
          match value_ty with
          | TNullable _ | TOcaml_app ("option", [ _ ]) -> value_ty
          | value_ty -> TNullable value_ty
        in
        Result.map Types.truthy_constraint (add_constraint value_ty)
    | ty when Option.is_some (Types.nil_predicate_constraint_info ty) ->
        let value_ty = Types.nil_predicate_constraint_info ty |> Option.get in
        let value_ty =
          match value_ty with
          | TNullable _ | TOcaml_app ("option", [ _ ]) -> value_ty
          | value_ty -> TNullable value_ty
        in
        add_constraint value_ty
    | ty when Types.is_dynamic ty ->
        let capability =
          Types.dynamic_constraint_info ty |> Option.value ~default:TUnknown
        in
        Result.map Types.dynamic_constraint (add_constraint capability)
    | ty -> (
        match Types.protocol_constraint_info ty with
        | Some (_, _, value_ty) ->
            Result.map
              (Types.protocol_constraint_with_value ty)
              (add_constraint value_ty)
        | None -> Ok ty)
  in
  match string_assoc_opt name params with
  | None -> Ok params
  | Some existing_ty ->
      Result.map
        (fun ty -> replace_param name ty params)
        (add_constraint existing_ty)

let rec numeric_form_type params = function
  | FInt _ -> TInt
  | FFloat _ -> TFloat
  | FSymbol name -> string_assoc_opt name params |> Option.value ~default:TUnknown
  | FList (FSymbol ("+" | "-" | "*" | "/" | "max" | "min") :: args) ->
      let types = List.map (numeric_form_type params) args in
      if List.exists (Types.equal TFloat) types then TFloat
      else if List.exists (Types.equal TInt) types then TInt
      else TUnknown
  | _ -> TUnknown

let rec inferred_form_type params = function
  | FInt _ -> TInt
  | FFloat _ -> TFloat
  | FChar _ -> TChar
  | FString _ -> TString
  | FBool _ -> TBool
  | FKeyword _ -> TKeyword
  | FList
      [
        FSymbol ("quote" | "clojure.core/quote");
        FSymbol _;
      ] ->
      TSymbol
  | FList [ FSymbol ("atom" | "volatile!"); FVector [] ] ->
      TRef (TVector TUnknown)
  | FList [ FSymbol ("atom" | "volatile!"); FSymbol "nil" ] ->
      TRef (TNullable TUnknown)
  | FList (FSymbol "delay" :: body_forms) -> (
      match List.rev body_forms with
      | result :: _ ->
          TOcaml_app ("Lazy.t", [ inferred_form_type params result ])
      | [] -> TOcaml_app ("Lazy.t", [ TUnknown ]))
  | FList [ FSymbol "deref"; FSymbol reference ] -> (
      match string_assoc_opt reference params with
      | Some (TRef value_ty) -> value_ty
      | Some (TOcaml_app ("Lazy.t", [ value_ty ])) -> value_ty
      | Some _ | None -> TUnknown)
  | FSymbol name -> string_assoc_opt name params |> Option.value ~default:TUnknown
  | FList [ FSymbol field_access; FSymbol receiver ]
    when String.starts_with ~prefix:".-" field_access ->
      let keyword =
        ":" ^ String.sub field_access 2 (String.length field_access - 2)
      in
      record_field_type params receiver keyword
      |> Option.value ~default:TUnknown
  | FList [ FKeyword keyword; FSymbol receiver ] ->
      record_field_type params receiver keyword
      |> Option.value ~default:TUnknown
  | FList (FSymbol ("+" | "-" | "*" | "/" | "max" | "min") :: _) as form ->
      numeric_form_type params form
  | FList [ FSymbol "ordering-compare"; _; _ ] -> TOcaml "int"
  | FList [ FSymbol "as-ordering"; FSymbol fn ] -> (
      match string_assoc_opt fn params with
      | Some (TFn (parameter_tys, _)) ->
          TFn (parameter_tys, TOcaml "int")
      | _ -> TUnknown)
  | FList [ FSymbol "count"; _ ] -> TInt
  | FList
      [
        FSymbol ("with-meta" | "clojure.core/with-meta" | "cljs.core/with-meta");
        FMap pairs;
        _metadata;
      ] ->
      let homogeneous_type forms =
        match List.map (inferred_form_type params) forms with
        | [] -> TUnknown
        | first :: rest
          when List.for_all (fun ty -> Types.equal first ty) rest ->
            first
        | _ -> TUnknown
      in
      let keys, values = List.split pairs in
      Types.dynamic_map (homogeneous_type keys) (homogeneous_type values)
  | FList
      [
        FSymbol ("with-meta" | "clojure.core/with-meta" | "cljs.core/with-meta");
        value;
        _metadata;
      ] ->
      inferred_form_type params value
  | FList
      [ FSymbol ("meta" | "clojure.core/meta" | "cljs.core/meta"); _value ] ->
      TOcaml "Lg_edn_backend.t"
  | FList (FSymbol ("str" | "clojure.core/str") :: _) -> TString
  | FList [ FSymbol "first"; FSymbol receiver ] -> (
      match string_assoc_opt receiver params with
      | Some ty -> (
          match Types.seqable_constraint_element ty with
          | Some element_ty -> element_ty
          | None -> (
              match Types.next_seq_element ty with
              | Some element_ty -> element_ty
              | None -> if Types.is_dynamic ty then ty else TUnknown))
      | None -> TUnknown)
  | FList [ FSymbol "next"; FSymbol receiver ] -> (
      match string_assoc_opt receiver params with
      | Some receiver_ty -> (
          match Types.seqable_constraint_element receiver_ty with
          | Some element_ty -> TSeq element_ty
          | None -> (
              match Types.next_seq_element receiver_ty with
              | Some element_ty -> TSeq element_ty
              | None -> TUnknown))
      | None -> TUnknown)
  | FList (FSymbol "list" :: values) -> (
      match List.map (inferred_form_type params) values with
      | [] -> TList TUnknown
      | first :: rest
        when List.for_all (fun ty -> Types.equal first ty) rest ->
          TList first
      | _ -> TList (Types.dynamic_constraint TUnknown))
  | FList (FSymbol "hash-set" :: values) -> (
      match List.map (inferred_form_type params) values with
      | [] -> TSet TUnknown
      | first :: rest
        when List.for_all (fun ty -> Types.equal first ty) rest ->
          TSet first
      | _ -> TSet (Types.dynamic_constraint TUnknown))
  | FList [ FSymbol operation; _ ]
    when String.equal operation "Array.length" ->
      TInt
  | FList
      (FSymbol (("cond->" | "cond->>") as operator) :: value :: clauses) ->
      let thread value step =
        match step with
        | FSymbol name -> FList [ FSymbol name; value ]
        | FKeyword _ as keyword -> FList [ keyword; value ]
        | FList (FSymbol name :: arguments) ->
            if operator = "cond->" then
              FList (FSymbol name :: value :: arguments)
            else FList ((FSymbol name :: arguments) @ [ value ])
        | form -> form
      in
      let rec result_type current_form current_ty = function
        | _condition :: step :: rest ->
            let current_form = thread current_form step in
            let step_ty = inferred_form_type params current_form in
            let current_ty =
              if Types.equal step_ty TUnknown then current_ty else step_ty
            in
            result_type current_form current_ty rest
        | _ -> current_ty
      in
      result_type value (inferred_form_type params value) clauses
  | FList [ FSymbol operation; FSymbol array; _from; _to ]
    when String.equal operation "Array.sub" -> (
      match string_assoc_opt array params with
      | Some (TArray _ as ty) | Some (TOcaml_app ("array", [ _ ]) as ty) -> ty
      | Some _ | None -> TArray TUnknown)
  | FList
      [ FSymbol operation; FSymbol array; _index ]
    when has_source_name operation "aget"
         || has_source_name operation "unsafe-aget" -> (
      match string_assoc_opt array params with
      | Some (TArray element_ty | TOcaml_app ("array", [ element_ty ])) ->
          element_ty
      | _ -> TUnknown)
  | FList [ FSymbol "nth"; FSymbol collection; _index ] -> (
      match string_assoc_opt collection params with
      | Some
          (TList element_ty | TVector element_ty | TSet element_ty
          | TSeq element_ty | TArray element_ty
          | TOcaml_app (("list" | "List.t" | "Seq.t" | "Seq" | "array"),
              [ element_ty ])) ->
          element_ty
      | Some collection_ty ->
          Types.seqable_constraint_element collection_ty
          |> Option.value ~default:TUnknown
      | None -> TUnknown)
  | FList
      [
        FSymbol ("get" | "clojure.core/get");
        FSymbol target;
        _key;
        default;
      ] ->
      let default_ty = inferred_form_type params default in
      (match
         string_assoc_opt target params
         |> Option.map Types.constraint_value_type
         |> fun target_ty -> Option.bind target_ty Types.dynamic_map_types
       with
      | Some (_, value_ty) -> refine_type value_ty default_ty
      | None -> default_ty)
  | FList
      [
        FSymbol ("get" | "clojure.core/get");
        FSymbol target;
        _key;
      ] -> (
      match
        string_assoc_opt target params
        |> Option.map Types.constraint_value_type
        |> fun target_ty -> Option.bind target_ty Types.dynamic_map_types
      with
      | Some (_, value_ty) -> TNullable value_ty
      | None -> TUnknown)
  | FList (FSymbol "conj" :: target :: values) ->
      let value_tys = List.map (inferred_form_type params) values in
      let refine_element element_ty =
        List.fold_left refine_type element_ty value_tys
      in
      (match inferred_form_type params target with
      | TList element_ty -> TList (refine_element element_ty)
      | TVector element_ty -> TVector (refine_element element_ty)
      | TSet element_ty -> TSet (refine_element element_ty)
      | TSeq element_ty -> TSeq (refine_element element_ty)
      | target_ty -> target_ty)
  | FList
      [
        FSymbol ("if-some" | "if-let");
        FVector [ FSymbol binding; option_form ];
        then_form;
        else_form;
      ] ->
      let payload_ty =
        match inferred_form_type params option_form with
        | TNullable payload | TOcaml_app ("option", [ payload ]) -> payload
        | _ -> TUnknown
      in
      let branch_params =
        (binding, payload_ty) :: string_remove_assoc binding params
      in
      refine_type
        (inferred_form_type branch_params then_form)
        (inferred_form_type params else_form)
  | FList (FSymbol ("get" | "clojure.core/get") :: _) -> TUnknown
  | FMap pairs ->
      let homogeneous_type forms =
        match List.map (inferred_form_type params) forms with
        | [] -> Some TUnknown
        | first :: rest
          when List.for_all (fun ty -> Types.equal first ty) rest ->
            Some first
        | _ -> None
      in
      if
        List.for_all
          (fun (key, _value) ->
            match key with FKeyword _ -> true | _ -> false)
          pairs
      then
        TRecord
          (List.map
             (fun (key, value) ->
               match key with
               | FKeyword keyword ->
                   make_field keyword (inferred_form_type params value)
               | _ -> assert false)
             pairs)
      else
        let keys, values = List.split pairs in
        (match (homogeneous_type keys, homogeneous_type values) with
        | Some key_ty, Some value_ty -> Types.dynamic_map key_ty value_ty
        | _ -> TUnknown)
  | FList (_function :: FSymbol receiver :: _) -> (
      match string_assoc_opt receiver params with
      | Some ty when Types.is_dynamic ty -> ty
      | _ -> TUnknown)
  | _ -> TUnknown

let rec returned_vector_type params = function
  | FVector items ->
      let item_tys = List.map (inferred_form_type params) items in
      let element_ty =
        match item_tys with
        | [] -> TUnknown
        | first :: rest
          when List.for_all (fun ty -> Types.equal first ty) rest ->
            first
        | _ -> Types.dynamic_constraint TUnknown
      in
      Some (TVector element_ty)
  | FList [ FSymbol "subvec"; collection; _ ]
  | FList [ FSymbol "subvec"; collection; _; _ ] -> (
      match returned_vector_type params collection with
      | Some vector_ty -> Some vector_ty
      | None -> Some (TVector (Types.dynamic_constraint TUnknown)))
  | FList [ FSymbol ("if" | "if-not"); _condition; then_form; else_form ] -> (
      match
        ( returned_vector_type params then_form,
          returned_vector_type params else_form )
      with
      | Some left, Some right -> Some (refine_type left right)
      | Some vector_ty, None | None, Some vector_ty -> Some vector_ty
      | None, None -> None)
  | FList (FSymbol "loop" :: _bindings :: body_forms) -> (
      match List.rev body_forms with
      | result :: _ -> returned_vector_type params result
      | [] -> None)
  | FList (FSymbol ("let" | "let*" | "do") :: forms) -> (
      match List.rev forms with
      | result :: _ -> returned_vector_type params result
      | [] -> None)
  | form -> (
      match inferred_form_type params form with
      | TVector _ as vector_ty -> Some vector_ty
      | _ -> None)

let select_fn_arity arities argument_count =
  match
    List.find_opt
      (fun (arity : fn_arity) ->
        Option.is_none arity.rest_param
        && List.length arity.fixed_params = argument_count)
      arities
  with
  | Some arity -> Some arity
  | None ->
      List.find_opt
        (fun (arity : fn_arity) ->
          Option.is_some arity.rest_param
          && argument_count >= List.length arity.fixed_params)
        arities

let rec inferred_call_return_type ~lookup_function_ty params = function
  | FList [ FSymbol "reduce"; _reducer; FMap []; _collection ] ->
      Types.dynamic_map (Type_solver.fresh ()) (Type_solver.fresh ())
  | FList [ FSymbol "reduce"; _reducer; init; _collection ] ->
      inferred_form_type params init
  | FList
      [
        FSymbol ("into" | "clojure.core/into");
        target;
        source;
      ] ->
      let infer form =
        match inferred_form_type params form with
        | TUnknown ->
            inferred_call_return_type ~lookup_function_ty params form
        | ty -> ty
      in
      let target_ty = infer target in
      let source_ty =
        match infer source with
        | TUnknown ->
            returned_vector_type params source
            |> Option.value ~default:TUnknown
        | ty -> ty
      in
      let source_element =
        match source_ty with
        | TVector element_ty | TList element_ty | TSet element_ty
        | TArray element_ty | TSeq element_ty ->
            Some element_ty
        | source_ty -> Types.seqable_constraint_element source_ty
      in
      (match (target_ty, source_element) with
      | TVector (TUnknown | TMeta _ | TVar _), Some element_ty ->
          TVector element_ty
      | TList (TUnknown | TMeta _ | TVar _), Some element_ty ->
          TList element_ty
      | TSet (TUnknown | TMeta _ | TVar _), Some element_ty ->
          TSet element_ty
      | TArray (TUnknown | TMeta _ | TVar _), Some element_ty ->
          TArray element_ty
      | _ -> target_ty)
  | FList (callee :: arguments) ->
      let actual_tys = List.map (inferred_form_type params) arguments in
      let instantiate parameter_tys return_ty =
        if List.length parameter_tys <> List.length actual_tys then TUnknown
        else
          Types.instantiate_type ~templates:parameter_tys ~actuals:actual_tys
            return_ty
      in
      let callee_ty =
        match callee with
        | FSymbol function_name -> lookup_function_ty function_name
        | FList _ as call ->
            Ok (inferred_call_return_type ~lookup_function_ty params call)
        | form -> Ok (inferred_form_type params form)
      in
      (match callee_ty with
      | Ok (TFn (parameter_tys, return_ty)) ->
          instantiate parameter_tys return_ty
      | Ok (TOverloaded_fn arities) -> (
          match select_fn_arity arities (List.length arguments) with
          | None -> TUnknown
          | Some arity ->
              let parameter_tys =
                arity.fixed_params
                @
                match arity.rest_param with
                | None -> []
                | Some rest_ty ->
                    List.init
                      (List.length arguments - List.length arity.fixed_params)
                      (fun _ -> rest_ty)
              in
              instantiate parameter_tys arity.return_ty)
      | Ok _ | Error _ -> TUnknown)
  | _ -> TUnknown

let inferred_form_or_call_type ~lookup_function_ty params form =
  match inferred_form_type params form with
  | TUnknown -> inferred_call_return_type ~lookup_function_ty params form
  | ty -> ty

let rec form_checks_reduced name = function
  | FList [ FSymbol predicate; FSymbol candidate ] ->
      candidate = name
      && (predicate = "reduced?"
         || String.ends_with ~suffix:"/reduced?" predicate)
  | FList forms | FVector forms -> List.exists (form_checks_reduced name) forms
  | FMap pairs ->
      List.exists
        (fun (key, value) ->
          form_checks_reduced name key || form_checks_reduced name value)
        pairs
  | _ -> false

let constrain_maybe_reduced_callbacks params forms =
  let rec visit params = function
    | FList (FSymbol binding_form :: FVector bindings :: body_forms)
      when binding_form = "let" || binding_form = "let*"
           || String.ends_with ~suffix:"/let" binding_form
           || String.ends_with ~suffix:"/let*" binding_form ->
        let rec visit_bindings params = function
          | FSymbol local_name :: FList (FSymbol fn_name :: args) :: rest ->
              let params =
                if List.exists (form_checks_reduced local_name) body_forms then
                  constrain_symbol
                    (TFn
                       ( List.map (fun _ -> TUnknown) args,
                         Types.maybe_reduced_callback_result TUnknown ))
                    params fn_name
                  |> Result.value ~default:params
                else params
              in
              visit_bindings params rest
          | _ :: _ :: rest -> visit_bindings params rest
          | _ -> params
        in
        let params = visit_bindings params bindings in
        List.fold_left visit params body_forms
    | FList nested | FVector nested -> List.fold_left visit params nested
    | FMap pairs ->
        List.fold_left
          (fun params (key, value) -> visit (visit params key) value)
          params pairs
    | _ -> params
  in
  List.fold_left visit params forms

let rec rewrite_simple_aliases aliases = function
  | FSymbol name as form -> (
      match string_assoc_opt name aliases with
      | Some ((FSymbol _ | FKeyword _) as alias) ->
          rewrite_simple_aliases (string_remove_assoc name aliases) alias
      | _ -> form)
  | FList (FSymbol binding_form :: FVector bindings :: body_forms)
    when binding_form = "let" || binding_form = "let*"
         || binding_form = "loop"
         || String.ends_with ~suffix:"/let" binding_form
         || String.ends_with ~suffix:"/let*" binding_form ->
      let rec rewrite_bindings aliases rewritten = function
        | FSymbol name :: value :: rest ->
            let value = rewrite_simple_aliases aliases value in
            rewrite_bindings (string_remove_assoc name aliases)
              (value :: FSymbol name :: rewritten)
              rest
        | rest -> (aliases, List.rev_append rewritten rest)
      in
      let body_aliases, bindings = rewrite_bindings aliases [] bindings in
      FList
        (FSymbol binding_form :: FVector bindings
        :: List.map (rewrite_simple_aliases body_aliases) body_forms)
  | FList (FSymbol "fn" :: FVector parameters :: body_forms) ->
      let aliases =
        List.fold_left
          (fun aliases -> function
            | FSymbol name -> string_remove_assoc name aliases
            | _ -> aliases)
          aliases parameters
      in
      FList
        (FSymbol "fn" :: FVector parameters
        :: List.map (rewrite_simple_aliases aliases) body_forms)
  | FList
      (FSymbol "fn" :: FSymbol function_name :: FVector parameters
      :: body_forms) ->
      let aliases = string_remove_assoc function_name aliases in
      let aliases =
        List.fold_left
          (fun aliases -> function
            | FSymbol name -> string_remove_assoc name aliases
            | _ -> aliases)
          aliases parameters
      in
      FList
        (FSymbol "fn" :: FSymbol function_name :: FVector parameters
        :: List.map (rewrite_simple_aliases aliases) body_forms)
  | FList forms -> FList (List.map (rewrite_simple_aliases aliases) forms)
  | FVector forms -> FVector (List.map (rewrite_simple_aliases aliases) forms)
  | FMap pairs ->
      FMap
        (List.map
           (fun (key, value) ->
             ( rewrite_simple_aliases aliases key,
               rewrite_simple_aliases aliases value ))
           pairs)
  | form -> form

let infer_params ?expected_return_ty ?(materialize_open_equality = false)
    ?(observe_call = fun _ _ _ -> ())
    ~lookup_function_ty
    ~lookup_protocol_constraint ~lookup_dynamic_key_record_type
    ~resolve_named_record params body_forms =
  let lookup_loop_initializer_type =
    let lookup = lookup_function_ty in
    fun name ->
      match lookup name with
      | Ok _ as result -> result
      | Error _ as error when String.ends_with ~suffix:"." name ->
          let type_name = String.sub name 0 (String.length name - 1) in
          (match
             resolve_named_record (TOcaml ("__lg_record:" ^ type_name))
           with
          | TNamed_record record ->
              Ok
                (TFn
                   ( List.map
                       (fun (field : field) -> field.ty)
                       (Types.record_constructor_fields record.fields),
                     TNamed_record record ))
          | _ -> error)
      | Error _ as error -> error
  in
  let inferred_binding_form_type params form =
    let direct_ty = inferred_form_type params form in
    if Type_solver.is_open direct_ty then
      inferred_call_return_type ~lookup_function_ty params form
    else direct_ty
  in
  let branch_depth = ref 0 in
  let branch_hint_symbols = ref [] in
  let with_branch inference =
    incr branch_depth;
    match inference () with
    | result ->
        decr branch_depth;
        result
    | exception exn ->
        decr branch_depth;
        raise exn
  in
  let restore_branch_hints base inferred previous_hints =
    let new_hints =
      List.filter
        (fun name -> not (string_mem name previous_hints))
        !branch_hint_symbols
    in
    List.map
      (fun (name, base_ty) ->
        let inferred_ty =
          string_assoc_opt name inferred |> Option.value ~default:base_ty
        in
        if string_mem name new_hints then
          let restored_ty =
            match
              ( Types.truthy_constraint_info base_ty,
                Types.truthy_constraint_info inferred_ty )
            with
            | Some _, Some _ -> inferred_ty
            | _ -> base_ty
          in
          (name, restored_ty)
        else (name, inferred_ty))
      base
  in
  let rec guarded_protocol_receivers = function
    | FList
        [ FSymbol "satisfies?"; FSymbol _protocol_name; FSymbol receiver ] ->
        [ receiver ]
    | FList (FSymbol ("and" | "or") :: forms) ->
        List.concat_map guarded_protocol_receivers forms
    | _ -> []
  in
  let restore_guarded_protocol_receivers base inferred condition =
    let guarded = guarded_protocol_receivers condition in
    let rec optionalize_seqable = function
      | TOcaml_app (name, [ element_ty; value_ty ])
        when name = Types.seqable_constraint_name ->
          Types.optional_seqable_constraint element_ty value_ty
      | ty -> (
          match Types.protocol_constraint_info ty with
          | Some (_, _, value_ty) ->
              Types.protocol_constraint_with_value ty
                (optionalize_seqable value_ty)
          | None -> ty)
    in
    List.map
      (fun (name, base_ty) ->
        let inferred_ty =
          string_assoc_opt name inferred |> Option.value ~default:base_ty
        in
        if not (string_mem name guarded) then (name, inferred_ty)
        else
          let restored_ty =
            match Types.protocol_constraint_info base_ty with
            | None -> base_ty
            | Some (base_protocol, _, base_value_ty) ->
                let inferred_value_ty =
                  match Types.protocol_constraint_info inferred_ty with
                  | Some (inferred_protocol, _, inferred_value_ty)
                    when Protocol_id.equal base_protocol inferred_protocol ->
                      inferred_value_ty
                  | Some _ -> base_value_ty
                  | None -> inferred_ty
                in
                Types.protocol_constraint_with_value base_ty
                  (refine_type base_value_ty inferred_value_ty
                  |> optionalize_seqable)
          in
          (name, restored_ty))
      base
  in
  let restore_branch_evidence base inferred previous_hints condition =
    restore_branch_hints base inferred previous_hints
    |> fun inferred ->
    restore_guarded_protocol_receivers base inferred condition
  in
  let fresh_type_variable _prefix =
    Type_solver.fresh ()
  in
  let freshen_call_type name ty =
    let _ = name in
    let substitutions =
      Type_solver.variables ty
      |> List.map (fun variable -> (variable, fresh_type_variable "call"))
    in
    Type_solver.apply substitutions ty
  in
  let branch_expected_type params expected branch other =
    let branch_is_nullable =
      match inferred_form_type params branch with
      | TNullable _ | TOcaml_app ("option", [ _ ]) -> true
      | _ -> false
    in
    match (other, expected) with
    | FSymbol "nil", (TNullable payload | TOcaml_app ("option", [ payload ]))
      when not branch_is_nullable ->
        payload
    | _ -> expected
  in
  let rec infer_expected expected_ty params = function
    | FSymbol name -> constrain_symbol expected_ty params name
    | FList (FSymbol "tuple" :: items) -> (
        match expected_ty with
        | TTuple item_tys when List.length item_tys = List.length items ->
            List.fold_left2
              (fun result item_ty item ->
                Result.bind result (fun params ->
                    infer_expected item_ty params item))
              (Ok params) item_tys items
        | _ -> infer_all params items)
    | FList [ FSymbol "nth"; collection; index ] ->
        Result.bind (infer_sequence_form expected_ty params collection)
          (fun params -> infer_expected TInt params index)
    | FList (FSymbol "conj" :: target :: values) -> (
        match expected_ty with
        | TList element_ty | TVector element_ty | TSet element_ty
        | TSeq element_ty ->
            Result.bind (infer_expected expected_ty params target)
              (fun params -> infer_expected_all element_ty params values)
        | _ -> infer_all params (target :: values))
    | FList
        (FSymbol "fn" :: FSymbol _name :: (FVector _ as fn_params)
        :: body_forms) ->
        infer_expected expected_ty params
          (FList (FSymbol "fn" :: fn_params :: body_forms))
    | FList (FSymbol "fn" :: (FVector _ as fn_params) :: body_forms) -> (
        match (expected_ty, Destructure.parse_param_specs fn_params) with
        | TFn (parameter_tys, return_ty), Ok specs
          when List.length parameter_tys = List.length specs ->
            let local_bindings =
              List.map2
                (fun (spec : Destructure.param_spec) parameter_ty ->
                  let parameter_ty =
                    match parameter_ty with
                    | TUnknown | TMeta _ | TVar _ -> TUnknown
                    | ty -> ty
                  in
                  let destructured =
                    if spec.destructured then
                      let item_ty =
                        match parameter_ty with
                        | TVector item_ty when Types.is_dynamic item_ty ->
                            item_ty
                        | _ -> TUnknown
                      in
                      Destructure.pattern_names spec.pattern
                      |> List.map (fun name -> (name, item_ty))
                    else []
                  in
                  (spec.source_name, parameter_ty) :: destructured)
                specs parameter_tys
              |> List.concat
            in
            let local_names = List.map fst local_bindings in
            let shadowed =
              List.filter (fun (name, _) -> string_mem name local_names) params
            in
            let function_params =
              local_bindings
              @ List.filter
                  (fun (name, _) -> not (string_mem name local_names))
                  params
            in
            let infer_body =
              match List.rev body_forms with
              | [] -> Ok function_params
              | result :: reversed_prefix ->
                  Result.bind
                    (infer_all function_params (List.rev reversed_prefix))
                    (fun params ->
                      match return_ty with
                      | TUnknown | TMeta _ | TVar _ -> infer_form params result
                      | ty -> infer_expected ty params result)
            in
            Result.map
              (fun inferred ->
                shadowed
                @ List.filter
                    (fun (name, _) -> not (string_mem name local_names))
                    inferred)
              infer_body
        | _ -> infer_all params body_forms)
    | FList [ FSymbol "if"; condition; then_form; else_form ] ->
        Result.bind (infer_truthy params condition) (fun params ->
            let previous_hints = !branch_hint_symbols in
            let then_expected =
              branch_expected_type params expected_ty then_form else_form
            in
            Result.bind
              (with_branch (fun () ->
                   infer_expected then_expected params then_form))
              (fun inferred ->
                let else_expected =
                  branch_expected_type inferred expected_ty else_form then_form
                in
                Result.map
                  (fun inferred ->
                    restore_branch_evidence params inferred previous_hints
                      condition)
                  (with_branch (fun () ->
                       infer_expected else_expected inferred else_form))))
    | FList [ FSymbol "if"; condition; then_form ] ->
        Result.bind (infer_truthy params condition) (fun params ->
            let previous_hints = !branch_hint_symbols in
            let then_expected =
              branch_expected_type params expected_ty then_form (FSymbol "nil")
            in
            Result.map
              (fun inferred ->
                restore_branch_evidence params inferred previous_hints
                  condition)
              (with_branch (fun () ->
                   infer_expected then_expected params then_form)))
    | FList [ FSymbol "if-not"; condition; then_form; else_form ] ->
        Result.bind (infer_truthy params condition) (fun params ->
            let previous_hints = !branch_hint_symbols in
            let then_expected =
              branch_expected_type params expected_ty then_form else_form
            in
            Result.bind
              (with_branch (fun () ->
                   infer_expected then_expected params then_form))
              (fun inferred ->
                let else_expected =
                  branch_expected_type inferred expected_ty else_form then_form
                in
                Result.map
                  (fun inferred ->
                    restore_branch_evidence params inferred previous_hints
                      condition)
                  (with_branch (fun () ->
                       infer_expected else_expected inferred else_form))))
    | ( FList
          [
            FSymbol ("if-some" | "if-let");
            FVector [ FSymbol _binding; _option_form ];
            _then_form;
            else_form;
          ] as form ) ->
        Result.bind (infer_form params form) (fun params ->
            infer_expected expected_ty params else_form)
    | FList [ FSymbol "Some"; value ] -> (
        match expected_ty with
        | TNullable value_ty | TOcaml_app ("option", [ value_ty ]) ->
            infer_expected value_ty params value
        | _ -> infer_form params value)
    | FList [ FSymbol ("atom" | "volatile!"); value ] -> (
        match expected_ty with
        | TRef value_ty -> infer_expected value_ty params value
        | _ -> infer_form params value)
    | FVector values -> (
        let element_ty =
          match expected_ty with
          | TVector element_ty -> Some element_ty
          | _ -> Types.seqable_constraint_element expected_ty
        in
        match element_ty with
        | Some element_ty -> infer_expected_all element_ty params values
        | None -> infer_all params values)
    | FList [ FSymbol "weak-ref"; value ] -> (
        match Types.weak_element expected_ty with
        | Some value_ty -> infer_expected value_ty params value
        | None -> infer_form params value)
    | FList [ FSymbol "weak-deref"; reference ] -> (
        match expected_ty with
        | TNullable value_ty | TOcaml_app ("option", [ value_ty ]) ->
            infer_expected (Types.weak_type value_ty) params reference
        | _ -> infer_form params reference)
    | FList (FSymbol "delay" :: body_forms) -> (
        match List.rev body_forms with
        | result :: reversed_prefix ->
            Result.bind (infer_all params (List.rev reversed_prefix)) (fun params ->
                infer_expected expected_ty params result)
        | [] -> Ok params)
    | FList [ FSymbol "deref"; FSymbol reference ] -> (
        match string_assoc_opt reference params with
        | Some (TOcaml_app ("Lazy.t", [ _ ])) ->
            constrain_symbol
              (TOcaml_app ("Lazy.t", [ expected_ty ]))
              params reference
        | Some _ | None -> constrain_symbol (TRef expected_ty) params reference)
    | FList (FSymbol let_name :: bindings :: body_forms)
      when let_name = "let" || let_name = "let*"
           || String.ends_with ~suffix:"/let" let_name
           || String.ends_with ~suffix:"/let*" let_name ->
        infer_let ~expected_body:expected_ty params bindings body_forms
    | FList (FSymbol ("assoc" | "clojure.core/assoc") :: target :: pairs) -> (
        match Types.record_fields expected_ty with
        | None -> infer_assoc params target pairs
        | Some expected_fields ->
            let rec assigned_keywords assigned = function
              | FKeyword keyword :: _value :: rest ->
                  assigned_keywords (keyword :: assigned) rest
              | _key :: _value :: rest -> assigned_keywords assigned rest
              | _ -> assigned
            in
            let assigned = assigned_keywords [] pairs in
            let preserved =
              List.filter
                (fun (field : field) ->
                  not (string_mem field.keyword assigned))
                expected_fields
            in
            let infer_target =
              match target with
              | FSymbol name ->
                  List.fold_left
                    (fun result (field : field) ->
                      Result.bind result (fun params ->
                          add_record_field_constraint name field.keyword
                            field.ty params))
                    (Ok params) preserved
              | target when preserved = [] -> infer_form params target
              | target -> infer_expected (TRecord preserved) params target
            in
            Result.bind infer_target (fun params ->
                infer_assoc ~constrain_assigned:false params target pairs))
    | FList [ FSymbol operation; array; from; length ]
      when String.equal operation "Array.sub"
           && (match expected_ty with
              | TArray _ | TOcaml_app ("array", [ _ ]) -> true
              | _ -> false) ->
        let element_ty =
          match expected_ty with
          | TArray element_ty | TOcaml_app ("array", [ element_ty ]) ->
              element_ty
          | _ -> assert false
        in
        Result.bind (infer_expected (TArray element_ty) params array)
          (fun params ->
            Result.bind (infer_expected TInt params from) (fun params ->
                infer_expected TInt params length))
    | FList
        (FSymbol (("cond->" | "cond->>") as operator) :: value :: clauses) ->
        let thread position step =
          match step with
          | FSymbol name -> FList [ FSymbol name; value ]
          | FKeyword _ as keyword -> FList [ keyword; value ]
          | FList (FSymbol name :: arguments) ->
              if position = `First then
                FList (FSymbol name :: value :: arguments)
              else FList ((FSymbol name :: arguments) @ [ value ])
          | form -> form
        in
        let position = if operator = "cond->" then `First else `Last in
        let rec infer_clauses params = function
          | [] -> Ok params
          | condition :: step :: rest ->
              Result.bind (infer_truthy params condition) (fun params ->
                  Result.bind
                    (infer_expected expected_ty params (thread position step))
                    (fun params -> infer_clauses params rest))
          | [ form ] -> infer_form params form
        in
        infer_clauses params clauses
    | FList [ FSymbol operation; FSymbol array; index ]
      when has_source_name operation "aget"
           || has_source_name operation "unsafe-aget" -> (
        match constrain_symbol (TArray expected_ty) params array with
        | Error _ as error -> error
        | Ok params ->
            let index_ty = inferred_form_type params index in
            infer_expected
              (if Types.equal index_ty TFloat then TFloat else TInt)
              params index)
    | FList [ FSymbol operation; FSymbol name ]
      when string_mem_assoc name params
           && (has_source_name operation "keys"
              || has_source_name operation "vals") ->
        let element_ty =
          Types.seqable_constraint_element expected_ty
          |> Option.value ~default:(fresh_type_variable "map_projection")
        in
        let other_ty = fresh_type_variable "map_projection" in
        let map_ty =
          if has_source_name operation "keys" then
            Types.dynamic_map element_ty other_ty
          else Types.dynamic_map other_ty element_ty
        in
        constrain_symbol map_ty params name
    | FList (FSymbol name :: args) when string_mem_assoc name params -> (
        let parameter_types =
          List.mapi
            (fun index argument ->
              let argument_ty = inferred_form_type params argument in
              let argument_ty =
                if Types.equal argument_ty TUnknown then
                  inferred_call_return_type ~lookup_function_ty params argument
                else argument_ty
              in
              match argument_ty with
              | TUnknown ->
                  fresh_type_variable
                    ("call_" ^ Names.sanitize_name name ^ "_"
                   ^ string_of_int index)
              | ty -> ty)
            args
        in
        match
          constrain_symbol (TFn (parameter_types, expected_ty)) params name
        with
        | Error _ as error -> error
        | Ok params ->
            List.fold_left2
              (fun result expected argument ->
                Result.bind result (fun params ->
                    infer_expected expected params argument))
              (Ok params) parameter_types args)
    | FList (FSymbol ("+" | "-" | "*" | "/" | "max" | "min") :: args)
      when Types.equal expected_ty TInt || Types.equal expected_ty TFloat ->
        infer_expected_all expected_ty params args
    | FList
        [ FKeyword nested_keyword; FList [ FKeyword keyword; FSymbol name ] ] ->
        add_record_field_constraint name keyword
          (TRecord [ make_field nested_keyword expected_ty ])
          params
    | FList [ FKeyword keyword; FSymbol name ] ->
        add_record_field_constraint name keyword expected_ty params
    | FList [ FKeyword keyword; FSymbol name; default ]
      when Option.is_some (Types.printable_constraint_info expected_ty) ->
        let field_ty = inferred_form_type params default in
        Result.bind
          (add_record_field_constraint name keyword (TNullable field_ty) params)
          (fun params -> infer_expected field_ty params default)
    | FList [ FKeyword keyword; FSymbol name; default ] ->
        Result.bind
          (add_record_field_constraint name keyword (TNullable expected_ty) params)
          (fun params -> infer_expected expected_ty params default)
    | FList
        [
          FKeyword keyword;
          FList
            [ FSymbol "first"; collection ];
        ] ->
        let target_ty = TRecord [ make_field keyword expected_ty ] in
        infer_sequence_form target_ty params collection
      | FList
          [
            FSymbol "first";
            FSymbol collection;
          ] ->
        constrain_seqable expected_ty params collection
    | FList [ FSymbol "first"; collection ] ->
        infer_sequence_form expected_ty params collection
    | FList [ FSymbol field_access; FSymbol name ]
      when String.starts_with ~prefix:".-" field_access ->
        let keyword =
          ":"
          ^ String.sub field_access 2 (String.length field_access - 2)
        in
        let field_ty =
          if Types.is_dynamic expected_ty then TUnknown else expected_ty
        in
        add_record_field_constraint name keyword field_ty params
    | FList [ FSymbol "get"; FSymbol name; FKeyword keyword ] ->
        add_record_field_constraint name keyword expected_ty params
    | FList
        [
          FSymbol ("get" | "clojure.core/get");
          target;
          key;
          default;
        ] ->
        Result.bind (infer_form params target) (fun params ->
            Result.bind (infer_form params key) (fun params ->
                infer_expected expected_ty params default))
    | FList [ FSymbol ("get" | "clojure.core/get"); FSymbol target; key ] -> (
        let record_ty = lookup_dynamic_key_record_type expected_ty in
        match record_ty with
        | Some record_ty ->
            let constrain_target =
              match string_assoc_opt target params with
              | Some inferred_ty
                when Types.is_dynamic inferred_ty ->
                  let capability =
                    Types.dynamic_constraint_info inferred_ty
                    |> Option.value ~default:TUnknown
                  in
                  Ok
                    (replace_param target
                       (refine_type capability record_ty)
                       params)
              | Some _ | None -> constrain_symbol record_ty params target
            in
            Result.bind constrain_target (fun params ->
                infer_expected TKeyword params key)
        | None ->
            let value_ty =
              match expected_ty with
              | TNullable inner | TOcaml_app ("option", [ inner ]) -> inner
              | ty -> ty
            in
            (match
               Option.bind
                 (string_assoc_opt target params)
                 Types.dynamic_map_types
             with
            | Some (key_ty, existing_value_ty) ->
                let value_ty = refine_type existing_value_ty value_ty in
                Result.bind
                  (constrain_symbol
                     (Types.dynamic_map key_ty value_ty)
                     params target)
                  (fun params -> infer_expected key_ty params key)
            | None ->
                let key_ty =
                  match inferred_form_type params key with
                  | TUnknown -> fresh_type_variable "map_key"
                  | ty -> ty
                in
                Result.bind
                  (constrain_symbol
                     (Types.dynamic_map key_ty value_ty)
                     params target)
                  (fun params -> infer_expected key_ty params key)))
    | FList [ FSymbol "get"; target; key ] ->
        let target_ty = inferred_form_type params target in
        if match target_ty with TVector _ -> true | _ -> false then
          Result.bind
            (infer_expected (TVector expected_ty) params target)
            (fun params -> infer_expected TInt params key)
        else
          let key_ty =
            match inferred_form_type params key with
            | TUnknown -> fresh_type_variable "map_key"
            | ty -> ty
          in
          let value_ty =
            match expected_ty with
            | TNullable inner | TOcaml_app ("option", [ inner ]) -> inner
            | ty -> ty
          in
          Result.bind
            (infer_expected (Types.dynamic_map key_ty value_ty) params target)
            (fun params -> infer_expected key_ty params key)
    | FMap pairs when Option.is_some (Types.dynamic_map_types expected_ty) ->
        let key_ty, value_ty =
          Option.get (Types.dynamic_map_types expected_ty)
        in
        pairs
        |> List.fold_left
             (fun result (key, value) ->
               Result.bind result (fun params ->
                   Result.bind (infer_expected key_ty params key) (fun params ->
                       infer_expected value_ty params value)))
             (Ok params)
    | FMap pairs when Option.is_some (Types.record_fields expected_ty) ->
        let fields =
          Types.record_fields expected_ty |> Option.value ~default:[]
        in
        pairs
        |> List.fold_left
             (fun result (key, value) ->
               Result.bind result (fun params ->
                   match key with
                   | FKeyword keyword -> (
                       match Types.find_field keyword fields with
                       | Some field -> infer_expected field.ty params value
                       | None -> infer_form params value)
                   | key ->
                       Result.bind (infer_form params key) (fun params ->
                           infer_form params value)))
             (Ok params)
    | FMap pairs when Types.is_dynamic expected_ty ->
        pairs
        |> List.fold_left
             (fun result (key, value) ->
               match result with
               | Error _ as error -> error
               | Ok params -> (
                   match infer_expected expected_ty params key with
                   | Error _ as error -> error
                   | Ok params -> infer_expected expected_ty params value))
             (Ok params)
    | (FList [ FSymbol "contains?"; _target; _key ] as form) ->
        infer_form params form
    | FList (FSymbol name :: args) -> (
        let form = FList (FSymbol name :: args) in
        let infer_call parameter_tys return_ty =
          if List.length parameter_tys <> List.length args then
            infer_form params form
          else
            let return_ty_for_unification =
              match (return_ty, expected_ty) with
              | ( (TNullable payload_ty
                  | TOcaml_app ("option", [ payload_ty ])),
                  expected_ty )
                when not
                       (match expected_ty with
                       | TNullable _ | TOcaml_app ("option", [ _ ]) -> true
                       | _ -> false) ->
                  payload_ty
              | return_ty, _ -> return_ty
            in
            let expected_return_ty =
              match (expected_ty, parameter_tys, return_ty_for_unification) with
              | ( TOcaml_app (name, [ element_ty; _ ]),
                  [ TArray parameter_ty ],
                  TSeq return_ty )
                when String.equal name Types.seqable_constraint_name
                     && Result.is_ok
                          (Type_solver.unify [] parameter_ty return_ty) ->
                  TSeq element_ty
              | _ -> expected_ty
            in
            match
              Type_solver.unify [] return_ty_for_unification expected_return_ty
            with
            | Error _ -> infer_form params form
            | Ok substitutions ->
                let substitutions =
                  List.fold_left2
                    (fun substitutions parameter_ty argument ->
                      let actual_ty = inferred_form_type params argument in
                      if
                        Types.equal actual_ty TUnknown
                        || Types.is_dynamic actual_ty
                        || match actual_ty with TMeta _ | TVar _ -> true | _ -> false
                      then substitutions
                      else
                        Type_solver.unify substitutions parameter_ty actual_ty
                        |> Result.value ~default:substitutions)
                    substitutions parameter_tys args
                in
                let parameter_tys =
                  List.map (Type_solver.apply substitutions) parameter_tys
                in
                List.fold_left2
                  (fun result expected argument ->
                    Result.bind result (fun params ->
                        infer_expected expected params argument))
                  (Ok params) parameter_tys args
        in
        match Result.map (freshen_call_type name) (lookup_function_ty name) with
        | Ok (TFn (parameter_tys, return_ty)) ->
            infer_call parameter_tys return_ty
        | Ok (TOverloaded_fn arities) -> (
            match select_fn_arity arities (List.length args) with
            | None -> infer_form params form
            | Some arity ->
                let parameter_tys =
                  arity.fixed_params
                  @
                  match arity.rest_param with
                  | None -> []
                  | Some rest_ty ->
                      List.init
                        (List.length args - List.length arity.fixed_params)
                        (fun _ -> rest_ty)
                in
                infer_call parameter_tys arity.return_ty)
        | _ -> infer_form params form)
    | form -> infer_form params form
  and infer_all params forms =
    let rec loop params = function
      | [] -> Ok params
      | form :: rest -> (
          match infer_form params form with
          | Error _ as err -> err
          | Ok params -> loop params rest)
    in
    loop params forms
  and infer_expected_all expected_ty params forms =
    let rec loop params = function
      | [] -> Ok params
      | form :: rest -> (
          match infer_expected expected_ty params form with
          | Error _ as err -> err
          | Ok params -> loop params rest)
    in
    loop params forms
  and infer_truthy params = function
    | FList (FSymbol "and" :: conditions) ->
        let optionalize_guard params = function
          | FSymbol name -> (
              match string_assoc_opt name params with
              | Some ty -> (
                  match Types.truthy_constraint_info ty with
                  | Some (TNullable _ | TOcaml_app ("option", [ _ ])) ->
                      params
                  | Some value_ty ->
                      replace_param name
                        (Types.truthy_constraint (TNullable value_ty))
                        params
                  | None -> params)
              | None -> params)
          | _ -> params
        in
        let rec infer_conditions params = function
          | [] -> Ok params
          | [ condition ] -> infer_truthy params condition
          | condition :: rest ->
              Result.bind (infer_truthy params condition) (fun params ->
                  infer_conditions (optionalize_guard params condition) rest)
        in
        infer_conditions params conditions
    | FList (FSymbol "or" :: conditions) ->
        (match List.rev conditions with
        | [] -> Ok params
        | last :: reversed_prefix ->
            Result.bind (infer_truthy params last) (fun params ->
                let result_ty = inferred_form_type params last in
                List.fold_left
                  (fun result condition ->
                    Result.bind result (fun params ->
                        match condition with
                        | FList [ FKeyword keyword; FSymbol name ] ->
                            add_record_field_constraint name keyword
                              (TNullable result_ty) params
                        | condition -> infer_truthy params condition))
                  (Ok params) (List.rev reversed_prefix)))
    | FSymbol name -> constrain_truthy_symbol params name
    | FList (FSymbol name :: args) when string_mem_assoc name params ->
        let parameter_types = List.map (inferred_form_type params) args in
        constrain_symbol (TFn (parameter_types, TBool)) params name
    | FList [ FKeyword keyword; FSymbol name ] ->
        add_record_field_constraint name keyword
          (TNullable (Type_solver.fresh ()))
          params
    | FList [ FKeyword keyword; FSymbol name; default ] ->
        let field_ty = inferred_form_type params default in
        Result.bind
          (add_record_field_constraint name keyword (TNullable field_ty) params)
          (fun params -> infer_expected field_ty params default)
    | FList
        [ FKeyword nested_keyword; FList [ FKeyword keyword; FSymbol name ] ] ->
        add_record_field_constraint name keyword
          (TRecord
             [ make_field nested_keyword (Types.dynamic_constraint TUnknown) ])
          params
    | form -> infer_form params form
  and infer_collection params = function
    | FSymbol name -> constrain_seqable TUnknown params name
    | form -> infer_form params form
  and infer_generator_bindings params bindings body_forms =
    let rec infer_bindings params = function
      | [] -> infer_all params body_forms
      | FKeyword ":let" :: FVector bindings :: rest ->
          let values =
            bindings
            |> List.mapi (fun index form -> (index, form))
            |> List.filter_map (fun (index, form) ->
                if index mod 2 = 1 then Some form else None)
          in
          Result.bind (infer_all params values) (fun params ->
              infer_bindings params rest)
      | FKeyword (":when" | ":while") :: condition :: rest ->
          Result.bind (infer_truthy params condition) (fun params ->
              infer_bindings params rest)
      | ((FSymbol _ | FVector _ | FMap _) as pattern) :: collection :: rest ->
          let local_names = Destructure.pattern_names pattern in
          let local_types =
            List.map
              (fun name ->
                ( name,
                  match pattern with
                  | FSymbol _ -> TUnknown
                  | FVector _ | FMap _ -> Type_solver.fresh ()
                  | _ -> TUnknown ))
              local_names
          in
          let local_params =
            local_types
            @ List.filter
                (fun (name, _) -> not (string_mem name local_names))
                params
          in
          Result.bind (infer_bindings local_params rest) (fun inferred ->
              let lookup_local_ty name =
                string_assoc_opt name inferred
                |> Option.value ~default:TUnknown
              in
              let element_ty =
                Destructure.infer_generator_pattern_type pattern lookup_local_ty
                |> Result.value ~default:TUnknown
              in
              let outer_params =
                List.map
                  (fun (name, ty) ->
                    if string_mem name local_names then (name, ty)
                    else
                      ( name,
                        string_assoc_opt name inferred
                        |> Option.value ~default:ty ))
                  params
              in
              infer_sequence_form element_ty outer_params collection)
      | _ -> infer_all params body_forms
    in
    match bindings with
    | FVector forms -> infer_bindings params forms
    | _ -> infer_all params body_forms
  and infer_known_call name params args =
    let member_name =
      match String.rindex_opt name '/' with
      | None -> name
      | Some index ->
          String.sub name (index + 1) (String.length name - index - 1)
    in
    if member_name = "->" || member_name = "->>" then
      match args with
      | [] -> Ok params
      | value :: steps ->
          let thread value step =
            match step with
            | FSymbol name -> FList [ FSymbol name; value ]
            | FList (function_ :: arguments) ->
                if member_name = "->" then
                  FList (function_ :: value :: arguments)
                else FList (function_ :: arguments @ [ value ])
            | step -> FList [ step; value ]
          in
          let rec infer_steps params threaded_value = function
            | [] -> Ok params
            | step :: rest ->
                let threaded = thread threaded_value step in
                let inference_form =
                  match step with
                  | FList _ -> thread value step
                  | _ -> threaded
                in
                Result.bind (infer_form params inference_form) (fun params ->
                    infer_steps params threaded rest)
          in
          Result.bind (infer_form params value) (fun params ->
              infer_steps params value steps)
    else if
      String.starts_with ~prefix:"->" member_name
    then
      infer_expected_all (Types.dynamic_constraint TUnknown) params args
    else
    let function_ty =
      Result.map (freshen_call_type name) (lookup_function_ty name)
    in
    match function_ty with
      | Ok (TFn (param_tys, _ret)) when List.length param_tys = List.length args
        ->
        let callback_element_candidates =
          List.fold_left2
            (fun candidates expected argument ->
              match expected with
              | TFn (expected_params, _) ->
                  let actual_params =
                    inferred_function_parameter_types params argument
                  in
                  if List.length expected_params = List.length actual_params then
                    List.fold_left2
                      (fun candidates expected actual ->
                        let candidate =
                          match expected with
                          | TUnknown | TMeta _ | TVar _ -> actual
                          | expected -> expected
                        in
                        if
                          Types.equal candidate TUnknown
                          || (match candidate with
                             | TMeta _ | TVar _ -> true
                             | _ -> false)
                          || List.exists (Types.equal candidate) candidates
                        then candidates
                        else candidate :: candidates)
                      candidates expected_params actual_params
                  else candidates
              | _ -> candidates)
            [] param_tys args
        in
        let callback_element =
          match callback_element_candidates with
          | [ candidate ] -> Some candidate
          | [] | _ :: _ :: _ -> None
        in
        let param_tys =
          List.map
            (function
              | TOcaml_app
                  ( constraint_name,
                    [ (TUnknown | TMeta _ | TVar _); value_ty ] )
                when (constraint_name = Types.seqable_constraint_name
                     || constraint_name
                        = Types.optional_seqable_constraint_name
                     || constraint_name
                        = Types.optional_sequential_constraint_name)
                     && Option.is_some callback_element ->
                  TOcaml_app
                    ( constraint_name,
                      [ Option.get callback_element; value_ty ] )
              | ty -> ty)
            param_tys
        in
        let substitutions =
          List.fold_left2
            (fun substitutions expected arg ->
              let actual =
                match inferred_form_type params arg with
                | TUnknown -> (
                    match arg with
                    | FSymbol symbol -> (
                        match string_assoc_opt symbol params with
                        | Some ty -> ty
                        | None ->
                            lookup_function_ty symbol
                            |> Result.value ~default:TUnknown)
                    | _ ->
                        inferred_call_return_type ~lookup_function_ty params arg)
                | ty -> ty
              in
              let unresolved =
                Types.equal actual TUnknown
                || match actual with TMeta _ | TVar _ -> true | _ -> false
              in
              if Types.is_dynamic actual then
                Type_solver.variables expected
                |> List.fold_left
                     (fun substitutions variable ->
                       Type_solver.force substitutions variable
                         (Types.dynamic_constraint TUnknown))
                     substitutions
              else if unresolved then substitutions
              else
                Types.infer_type_substitutions substitutions
                  ~template:expected ~actual)
            [] param_tys args
        in
        let param_tys =
          List.map
            (Types.substitute_type_variables substitutions)
            param_tys
        in
        List.fold_left2
          (fun acc expected_ty arg ->
            match acc with
            | Error _ as err -> err
            | Ok params -> infer_expected expected_ty params arg)
          (Ok params) param_tys args
    | Ok (TOverloaded_fn arities) -> (
        match select_fn_arity arities (List.length args) with
        | None -> infer_all params args
        | Some arity ->
            let fixed_count = List.length arity.fixed_params in
            let expected_tys =
              arity.fixed_params
              @
              match arity.rest_param with
              | None -> []
              | Some rest_ty ->
                    List.init
                      (List.length args - fixed_count)
                      (fun _ -> rest_ty)
            in
            let substitutions =
              let inferred_argument_type = function
                | FList (FSymbol name :: arguments) -> (
                    match
                      Result.map (freshen_call_type name)
                        (lookup_function_ty name)
                    with
                    | Ok (TFn (parameter_tys, return_ty))
                      when List.length parameter_tys
                           = List.length arguments ->
                        Types.instantiate_type ~templates:parameter_tys
                          ~actuals:
                            (List.map (inferred_form_type params) arguments)
                          return_ty
                    | Ok (TOverloaded_fn arities) -> (
                        match select_fn_arity arities (List.length arguments) with
                        | Some arity ->
                            Types.instantiate_type
                              ~templates:arity.fixed_params
                              ~actuals:
                                (List.map (inferred_form_type params) arguments)
                              arity.return_ty
                        | None -> TUnknown)
                    | _ -> TUnknown)
                | form -> inferred_form_type params form
              in
              List.fold_left2
                (fun substitutions expected arg ->
                  let actual = inferred_argument_type arg in
                  if
                    Types.equal actual TUnknown || Types.is_dynamic actual
                    || match actual with TMeta _ | TVar _ -> true | _ -> false
                  then substitutions
                  else
                    Types.infer_type_substitutions substitutions
                      ~template:expected ~actual)
                [] expected_tys args
            in
            let expected_tys =
              List.map
                (Types.substitute_type_variables substitutions)
                expected_tys
            in
            List.fold_left2
              (fun acc expected_ty arg ->
                match acc with
                | Error _ as err -> err
                | Ok params -> infer_expected expected_ty params arg)
              (Ok params) expected_tys args)
    | _ -> infer_all params args
  and inferred_unary_function_param params = function
    | FKeyword keyword -> TRecord [ make_field keyword TUnknown ]
    | FSymbol name -> (
        match string_assoc_opt name params with
        | Some (TFn ([ param_ty ], _)) -> param_ty
        | Some _ | None -> (
            match lookup_function_ty name with
            | Ok (TFn ([ param_ty ], _)) -> param_ty
            | _ -> TUnknown))
    | FList
        (FSymbol "fn" :: (FVector _ as params_form) :: body_forms) -> (
        match Destructure.parse_param_specs params_form with
        | Ok [ (spec : Destructure.param_spec) ] -> (
            match spec.explicit_ty with
            | Some ty when not (Types.equal ty TUnknown) ->
                resolve_named_record ty
            | _ when not spec.destructured -> (
                match infer_all [ (spec.source_name, TUnknown) ] body_forms with
                | Ok inferred ->
                    string_assoc_opt spec.source_name inferred
                    |> Option.value ~default:TUnknown
                | Error _ -> TUnknown)
            | _ ->
                let dynamic = Types.dynamic_constraint TUnknown in
                let pattern_params =
                  Destructure.pattern_names spec.pattern
                  |> List.map (fun name -> (name, dynamic))
                in
                (match infer_all pattern_params body_forms with
                | Ok inferred ->
                    Destructure.infer_pattern_type spec.pattern (fun name ->
                        string_assoc_opt name inferred
                        |> Option.value ~default:dynamic)
                    |> Result.value ~default:TUnknown
                | Error _ -> TUnknown))
        | Ok _ | Error _ -> TUnknown)
    | _ -> TUnknown
  and inferred_function_parameter_types params = function
    | FSymbol name -> (
        let function_ty =
          match string_assoc_opt name params with
          | Some ty -> Ok ty
          | None -> lookup_function_ty name
        in
        match function_ty with
        | Ok (TFn (parameter_tys, _)) -> parameter_tys
        | Ok _ | Error _ -> [])
    | FList
        (FSymbol "fn" :: FVector parameter_forms :: body_forms) -> (
        match Destructure.parse_param_specs (FVector parameter_forms) with
        | Error _ -> []
        | Ok specs ->
            let local_bindings =
              specs
              |> List.concat_map (fun (spec : Destructure.param_spec) ->
                     let source_ty =
                       Option.value spec.explicit_ty ~default:TUnknown
                       |> resolve_named_record
                     in
                     let destructured =
                       if spec.destructured then
                         Destructure.pattern_names spec.pattern
                         |> List.map (fun name -> (name, TUnknown))
                       else []
                     in
                     (spec.source_name, source_ty) :: destructured)
            in
            let local_names = List.map fst local_bindings in
            let local_params =
              local_bindings
              @ List.filter
                  (fun (name, _) -> not (string_mem name local_names))
                  params
            in
            let inferred =
              infer_all local_params body_forms
              |> Result.value ~default:local_params
            in
            List.map
              (fun (spec : Destructure.param_spec) ->
                string_assoc_opt spec.source_name inferred
                |> Option.value ~default:TUnknown)
              specs)
    | _ -> []
  and callback_compares_destructured_values = function
    | FList (FSymbol "fn" :: (FVector _ as params_form) :: body_forms) -> (
        match Destructure.parse_param_specs params_form with
        | Ok [ (spec : Destructure.param_spec) ] when spec.destructured ->
            let names = Destructure.pattern_names spec.pattern in
            let rec compares = function
              | FList (FSymbol "=" :: operands) ->
                  List.length operands >= 2
                  && List.for_all
                       (function
                         | FSymbol name -> string_mem name names
                         | _ -> false)
                       operands
              | FList (FSymbol "fn" :: _) -> false
              | FList forms | FVector forms -> List.exists compares forms
              | FMap pairs ->
                  List.exists
                    (fun (key, value) -> compares key || compares value)
                    pairs
              | _ -> false
            in
            List.exists compares body_forms
        | Ok _ | Error _ -> false)
    | _ -> false
  and callback_checks_runtime_type = function
    | FList (FSymbol "fn" :: (FVector _ as params_form) :: body_forms) -> (
        match Destructure.parse_param_specs params_form with
        | Ok [ (spec : Destructure.param_spec) ] when not spec.destructured ->
            let rec checks = function
              | FList
                  [
                    FSymbol "instance?";
                    FSymbol _type_name;
                    FSymbol value;
                  ] ->
                  String.equal value spec.source_name
              | FList (FSymbol "fn" :: _) -> false
              | FList forms | FVector forms -> List.exists checks forms
              | FMap pairs ->
                  List.exists
                    (fun (key, value) -> checks key || checks value)
                    pairs
              | _ -> false
            in
            List.exists checks body_forms
        | Ok _ | Error _ -> false)
    | _ -> false
  and infer_sequence_form element_ty params = function
    | FSymbol name -> constrain_seqable element_ty params name
    | FList [ FKeyword keyword; FSymbol name ] ->
        add_record_field_constraint name keyword
          (Types.seqable_constraint element_ty)
          params
    | FList
        (FSymbol ("map" | "mapv") :: FSymbol "vector" :: collections)
      when List.length collections >= 2
           && (match element_ty with TVector _ -> true | _ -> false) ->
        let item_ty =
          match element_ty with TVector item_ty -> item_ty | _ -> assert false
        in
        List.fold_left
          (fun result collection ->
            Result.bind result (fun params ->
                infer_sequence_form item_ty params collection))
          (Ok params) collections
    | FList [ FSymbol "partition-by"; function_form; collection ] ->
        let source_element_ty =
          match element_ty with
          | TList item_ty | TSeq item_ty -> item_ty
          | _ -> inferred_unary_function_param params function_form
        in
        Result.bind
          (infer_sequence_form source_element_ty params collection)
          (fun params ->
            infer_expected
              (TFn
                 ( [ source_element_ty ],
                   fresh_type_variable "partition_key" ))
              params function_form)
    | FList
        [
          FSymbol
            ("filter" | "remove" | "take-while" | "drop-while");
          predicate;
          collection;
        ] ->
        Result.bind (infer_sequence_form element_ty params collection)
          (fun params ->
            infer_expected (TFn ([ element_ty ], TUnknown)) params predicate)
    | (FList (FSymbol _ :: _) as form) ->
        infer_expected (Types.seqable_constraint element_ty) params form
    | form ->
        infer_expected (Types.seqable_constraint element_ty) params form
  and inferred_literal_collection_item params = function
    | FVector forms | FList (FSymbol "list" :: forms) -> (
        let item_types = List.map (inferred_form_type params) forms in
        match item_types with
        | [] -> TUnknown
        | first :: rest
          when not (match first with TUnknown | TMeta _ | TVar _ -> true | _ -> false)
               && List.for_all (Types.equal first) rest ->
            first
        | _ -> TUnknown)
    | _ -> TUnknown
  and inferred_map_indexed_item params = function
    | FSymbol name -> (
        match string_assoc_opt name params with
        | Some (TFn ([ TInt; item_ty ], _)) -> item_ty
        | Some _ | None -> (
            match lookup_function_ty name with
            | Ok (TFn ([ TInt; item_ty ], _)) -> item_ty
            | _ -> TUnknown))
    | FList
        (FSymbol "fn"
        :: FVector [ FSymbol index; FSymbol item ]
        :: body_forms) -> (
        match infer_all [ (index, TInt); (item, TUnknown) ] body_forms with
        | Ok inferred ->
            string_assoc_opt item inferred |> Option.value ~default:TUnknown
        | Error _ -> TUnknown)
    | _ -> TUnknown
  and inferred_reducer_types outer_params accumulator_ty = function
    | FSymbol name -> (
        match lookup_function_ty name with
        | Ok (TFn ([ accumulator_ty; item_ty ], _)) ->
            (accumulator_ty, item_ty)
        | _ -> (accumulator_ty, TUnknown))
    | FList
        (FSymbol "fn"
        :: FVector [ FSymbol accumulator; FSymbol item ]
        :: body_forms) -> (
        let reducer_params =
          [ (accumulator, accumulator_ty); (item, TUnknown) ]
          @ List.filter
              (fun (name, ty) ->
                name <> accumulator && name <> item
                && not (Type_solver.is_open ty)
                && not (Types.is_dynamic ty))
              outer_params
        in
        let rec reducer_returned_vector_type params = function
          | FList
              (FSymbol ("let" | "let*") :: FVector bindings :: body_forms) ->
              let rec infer_bindings params = function
                | FSymbol name :: value :: rest ->
                    let ty = inferred_form_type params value in
                    infer_bindings
                      ((name, ty) :: string_remove_assoc name params)
                      rest
                | _ :: _ :: rest -> infer_bindings params rest
                | _ -> params
              in
              let body_params = infer_bindings params bindings in
              (match List.rev body_forms with
              | result :: _ -> reducer_returned_vector_type body_params result
              | [] -> None)
          | form -> returned_vector_type params form
        in
        match
          infer_all reducer_params body_forms
         with
        | Ok inferred ->
            let inferred_accumulator_ty =
              string_assoc_opt accumulator inferred
              |> Option.value ~default:accumulator_ty
            in
            let inferred_accumulator_ty =
              match List.rev body_forms with
              | result :: _ -> (
                  match reducer_returned_vector_type inferred result with
                  | Some returned_ty ->
                      refine_type inferred_accumulator_ty returned_ty
                  | None -> inferred_accumulator_ty)
              | [] -> inferred_accumulator_ty
            in
            ( inferred_accumulator_ty,
              string_assoc_opt item inferred |> Option.value ~default:TUnknown
            )
        | Error _ -> (accumulator_ty, TUnknown))
    | _ -> (accumulator_ty, TUnknown)
  and inferred_kv_reducer_types params init = function
    | FSymbol name -> (
        match lookup_function_ty name with
        | Ok (TFn ([ _; key_ty; value_ty ], _)) -> (key_ty, value_ty)
        | _ -> (TUnknown, TUnknown))
    | FList
        (FSymbol "fn"
        :: FVector [ accumulator; FSymbol key; FSymbol value ]
        :: body_forms) -> (
        let accumulator_ty = inferred_form_type params init in
        let reducer_params =
          [ (key, TUnknown); (value, TUnknown) ]
          @
          match accumulator with
          | FSymbol name -> [ (name, accumulator_ty) ]
          | _ -> []
        in
        match infer_all reducer_params body_forms with
        | Ok inferred ->
            ( string_assoc_opt key inferred
              |> Option.value ~default:TUnknown,
              string_assoc_opt value inferred
              |> Option.value ~default:TUnknown )
        | Error _ -> (TUnknown, TUnknown))
    | _ -> (TUnknown, TUnknown)
  and infer_let ?expected_body params bindings body_forms =
    let infer_body params body_forms =
      match (expected_body, List.rev body_forms) with
      | Some expected, last :: reversed_prefix ->
          Result.bind (infer_all params (List.rev reversed_prefix)) (fun params ->
              infer_expected expected params last)
      | Some _, [] -> Ok params
      | None, _ -> infer_all params body_forms
    in
    match bindings with
    | FVector forms -> (
        let rec macro_slots slots = function
          | FSymbol name :: FList [ FSymbol "volatile!"; FSymbol "nil" ] :: rest
            ->
              macro_slots (name :: slots) rest
          | _ :: _ :: rest -> macro_slots slots rest
          | _ -> slots
        in
        let slots = macro_slots [] forms in
        let rec local_names names = function
          | pattern :: _value :: rest ->
              local_names
                (List.rev_append (Destructure.pattern_names pattern) names)
                rest
          | _ -> List.rev names
        in
        let provisional_names = local_names [] forms |> List.sort_uniq String.compare in
        let infer_local_function_type scope_params = function
          | FList
              (FSymbol "fn" :: FVector parameters :: body_forms)
          | FList
              (FSymbol "fn" :: FSymbol _ :: FVector parameters :: body_forms)
            -> (
              match Destructure.parse_param_specs (FVector parameters) with
              | Error _ -> TUnknown
              | Ok specs ->
                  let parameter_bindings =
                    specs
                    |> List.concat_map (fun (spec : Destructure.param_spec) ->
                           let parameter_ty =
                             Option.value spec.explicit_ty ~default:TUnknown
                             |> resolve_named_record
                           in
                           let destructured =
                             if spec.destructured then
                               Destructure.pattern_names spec.pattern
                               |> List.map (fun name -> (name, TUnknown))
                             else []
                           in
                           (spec.source_name, parameter_ty) :: destructured)
                  in
                  let local_names = List.map fst parameter_bindings in
                  let function_params =
                    parameter_bindings
                    @ List.filter
                        (fun (name, _) -> not (string_mem name local_names))
                        scope_params
                  in
                  (match infer_all function_params body_forms with
                  | Error _ -> TUnknown
                  | Ok inferred ->
                      let parameter_tys =
                        List.map
                          (fun (spec : Destructure.param_spec) ->
                            match spec.explicit_ty with
                            | Some ty when not (Types.equal ty TUnknown) ->
                                resolve_named_record ty
                            | _ when spec.destructured ->
                                Destructure.infer_pattern_type spec.pattern
                                  (fun name ->
                                    string_assoc_opt name inferred
                                    |> Option.value ~default:TUnknown)
                                |> Result.value ~default:TUnknown
                            | _ ->
                                string_assoc_opt spec.source_name inferred
                                |> Option.value ~default:TUnknown)
                          specs
                      in
                      let return_ty =
                        match List.rev body_forms with
                        | result :: _ -> inferred_form_type inferred result
                        | [] -> TNil
                      in
                      TFn (parameter_tys, return_ty)))
          | _ -> TUnknown
        in
        let inferred_initializer_type scope_params value =
          let inferred_ty =
            match value with
            | FList [ FSymbol "reduce"; reducer; init; _collection ] ->
                let accumulator_ty =
                  match init with
                  | FMap [] ->
                      Types.dynamic_map (Type_solver.fresh ())
                        (Type_solver.fresh ())
                  | _ ->
                      returned_vector_type scope_params init
                      |> Option.value
                           ~default:(inferred_form_type scope_params init)
                in
                let inferred_accumulator_ty, _ =
                  inferred_reducer_types scope_params accumulator_ty reducer
                in
                refine_type accumulator_ty inferred_accumulator_ty
            | _ -> inferred_form_type scope_params value
          in
          match inferred_ty with
          | TUnknown -> (
              match value with
              | FList (FSymbol function_name :: arguments) -> (
                  let actual_tys =
                    List.map (inferred_form_type scope_params) arguments
                  in
                  match
                    Result.map (freshen_call_type function_name)
                      (lookup_function_ty function_name)
                  with
                  | Ok (TFn (parameter_tys, return_ty))
                    when List.length parameter_tys = List.length arguments ->
                      Types.instantiate_type ~templates:parameter_tys
                        ~actuals:actual_tys return_ty
                  | Ok (TOverloaded_fn arities) -> (
                      match select_fn_arity arities (List.length arguments) with
                      | Some arity ->
                          let parameter_tys =
                            arity.fixed_params
                            @
                            match arity.rest_param with
                            | None -> []
                            | Some rest_ty ->
                                List.init
                                  (List.length arguments
                                  - List.length arity.fixed_params)
                                  (fun _ -> rest_ty)
                          in
                          Types.instantiate_type ~templates:parameter_tys
                            ~actuals:actual_tys arity.return_ty
                      | None -> infer_local_function_type scope_params value)
                  | Ok _ | Error _ ->
                      infer_local_function_type scope_params value)
              | _ -> infer_local_function_type scope_params value)
          | ty -> ty
        in
        let rec initializer_type name = function
          | FSymbol candidate :: value :: _ when String.equal name candidate ->
              inferred_initializer_type params value
          | _ :: _ :: rest -> initializer_type name rest
          | _ -> TUnknown
        in
        let provisional_params =
          List.map
            (fun name -> (name, initializer_type name forms))
            provisional_names
          @ List.filter
              (fun (name, _) -> not (string_mem name provisional_names))
              params
        in
        let inferred_locals =
          lazy
            (infer_body provisional_params body_forms
            |> Result.value ~default:provisional_params)
        in
        let lookup_inferred_local name =
          string_assoc_opt name (Lazy.force inferred_locals)
          |> Option.value ~default:TUnknown
        in
        let rec infer_slot_writes params = function
          | FList [ FSymbol ("vreset!" | "reset!"); FSymbol slot; value ]
            when string_mem slot slots ->
              infer_expected (Types.dynamic_constraint TUnknown) params value
          | FList forms | FVector forms ->
              List.fold_left
                (fun result form ->
                  Result.bind result (fun params ->
                      infer_slot_writes params form))
                (Ok params) forms
          | FMap pairs ->
              List.fold_left
                (fun result (key, value) ->
                  Result.bind result (fun params ->
                      Result.bind (infer_slot_writes params key) (fun params ->
                          infer_slot_writes params value)))
                (Ok params) pairs
          | _ -> Ok params
        in
        let rec infer_values params = function
          | [] -> Ok params
          | (FVector _ as pattern) :: FSymbol source :: rest -> (
              let element_ty =
                match
                  Destructure.infer_pattern_type pattern lookup_inferred_local
                with
                | Ok (TVector element_ty) -> element_ty
                | Ok _ | Error _ -> TUnknown
              in
              match constrain_seqable element_ty params source with
              | Error _ as err -> err
              | Ok params -> infer_values params rest)
          | (FMap _ as pattern) :: FSymbol source :: rest -> (
              let map_ty =
                Destructure.infer_pattern_type pattern lookup_inferred_local
                |> Result.value
                     ~default:(Types.dynamic_constraint TUnknown)
              in
              match constrain_symbol map_ty params source
              with
              | Error _ as error -> error
              | Ok params -> infer_values params rest)
          | (FMap _ as pattern)
            :: (FList [ FSymbol "deref"; FSymbol _ ] as source)
            :: rest -> (
              let map_ty =
                Destructure.infer_pattern_type pattern lookup_inferred_local
                |> Result.value
                     ~default:(Types.dynamic_constraint TUnknown)
              in
              match infer_expected (TNullable map_ty) params source with
              | Error _ as error -> error
              | Ok params -> infer_values params rest)
          | FSymbol _name :: value_form :: rest -> (
              match infer_form params value_form with
              | Error _ as err -> err
              | Ok params -> infer_values params rest)
          | _pattern :: value_form :: rest -> (
              match infer_form params value_form with
              | Error _ as error -> error
              | Ok params -> infer_values params rest)
          | [ _ ] -> Ok params
        in
        match infer_values params forms with
        | Error _ as err -> err
        | Ok params -> (
            match
              List.fold_left
                (fun result form ->
                  Result.bind result (fun params ->
                      infer_slot_writes params form))
                (Ok params) body_forms
            with
            | Error _ as error -> error
            | Ok params -> (
                let rec simple_bindings bindings = function
                  | [] -> Some (List.rev bindings)
                  | FSymbol name :: value :: rest ->
                      simple_bindings ((name, value) :: bindings) rest
                  | _ -> None
                in
                match simple_bindings [] forms with
                | None ->
                    let inferred_locals = Lazy.force inferred_locals in
                    let outer_params =
                      List.filter
                        (fun (name, _) ->
                          not (string_mem name provisional_names))
                        inferred_locals
                    in
                    let rec propagate params = function
                      | [] -> Ok params
                      | pattern :: value :: rest ->
                          let expected =
                            Destructure.infer_pattern_type pattern
                              lookup_inferred_local
                            |> Result.value ~default:TUnknown
                          in
                          let infer_value =
                            match (pattern, value, expected) with
                            | FVector _, FSymbol source, TVector element_ty ->
                                constrain_seqable element_ty params source
                            | _, _, (TUnknown | TMeta _ | TVar _) ->
                                infer_form params value
                            | _, _, expected ->
                                infer_expected expected params value
                          in
                          Result.bind infer_value (fun params ->
                              propagate params rest)
                      | [ _ ] -> Ok params
                    in
                    Result.bind (infer_values outer_params forms) (fun params ->
                        propagate params forms)
                | Some bindings ->
                    let body_forms =
                      List.map (rewrite_simple_aliases bindings) body_forms
                    in
                    let local_names = List.map fst bindings in
                    let outer_params =
                      List.filter
                        (fun (name, _) -> not (string_mem name local_names))
                        params
                    in
                    let rec initial_locals locals = function
                      | [] -> List.rev locals
                      | (name, value) :: rest ->
                          let ty =
                            inferred_initializer_type
                              (List.rev_append locals outer_params) value
                          in
                          let ty =
                            match ty with
                            | TArray element_ty when Types.is_dynamic element_ty ->
                                TArray
                                  (fresh_type_variable
                                     ("let_array_" ^ Names.sanitize_name name))
                            | ty when Types.equal ty TUnknown ->
                                fresh_type_variable "let"
                            | ty -> ty
                          in
                          initial_locals ((name, ty) :: locals) rest
                    in
                    let local_params = initial_locals [] bindings in
                    Result.bind
                      (infer_all (local_params @ outer_params)
                         (List.map snd bindings))
                      (fun binding_params ->
                      Result.bind
                      (infer_body binding_params body_forms)
                      (fun inferred ->
                        let rec propagate params = function
                          | [] -> Ok params
                          | ( name,
                              FList
                                [
                                  FSymbol field_access;
                                  FSymbol receiver;
                                ] )
                            :: rest
                            when String.starts_with ~prefix:".-" field_access
                                 && string_mem_assoc receiver params ->
                              let expected =
                                string_assoc_opt name params
                                |> Option.value ~default:TUnknown
                              in
                              let keyword =
                                ":"
                                ^ String.sub field_access 2
                                    (String.length field_access - 2)
                              in
                              (match expected with
                              | TUnknown | TMeta _ | TVar _ ->
                                  propagate params rest
                              | _ ->
                                  Result.bind
                                    (add_record_field_constraint receiver
                                       keyword expected params)
                                    (fun params -> propagate params rest))
                          | (name, value) :: rest ->
                              let expected =
                                string_assoc_opt name params
                                |> Option.value ~default:TUnknown
                              in
                              (match expected with
                              | TUnknown | TMeta _ | TVar _ -> propagate params rest
                              | expected ->
                                  Result.bind
                                    (infer_expected expected params value)
                                    (fun params -> propagate params rest))
                        in
                        Result.map
                          (fun inferred ->
                            List.map
                              (fun (name, ty) ->
                                if string_mem name local_names then (name, ty)
                                else
                                  ( name,
                                    string_assoc_opt name inferred
                                    |> Option.value ~default:ty ))
                              params)
                          (propagate inferred (List.rev bindings)))))))
    | _ -> infer_all params body_forms
  and infer_assoc ?(constrain_assigned = true) params target pairs =
    let target_name = assoc_root_symbol target in
    let rec infer_pairs params = function
      | [] -> Ok params
      | FKeyword keyword :: value_form :: rest -> (
          match infer_form params value_form with
          | Error _ as err -> err
          | Ok params -> (
              match target_name with
              | Some name -> (
                  let field_ty =
                    match value_form with
                    | FSymbol value_name -> (
                        match string_assoc_opt value_name params with
                        | Some TUnknown ->
                            fresh_type_variable
                              ("assoc_" ^ Names.sanitize_name value_name)
                        | Some ((TMeta _ | TVar _) as ty) -> ty
                        | Some ty -> ty
                        | None -> (
                            match lookup_function_ty value_name with
                            | Ok ty -> ty
                            | Error _ ->
                                inferred_form_or_call_type ~lookup_function_ty
                                  params value_form))
                    | _ ->
                        inferred_form_or_call_type ~lookup_function_ty params
                          value_form
                  in
                  let params =
                    match value_form with
                    | FSymbol value_name when string_mem_assoc value_name params
                      ->
                        constrain_symbol field_ty params value_name
                    | _ -> Ok params
                  in
                  match params with
                  | Error _ as error -> error
                  | Ok params when not constrain_assigned ->
                      infer_pairs params rest
                  | Ok params -> (
                      match
                        add_record_field_constraint name keyword field_ty params
                      with
                  | Error _ as err -> err
                      | Ok params -> infer_pairs params rest))
              | None -> infer_pairs params rest))
      | key_form :: value_form :: rest -> (
          let expected_pair =
            match inferred_form_type params target with
            | TVector element_ty -> Some (TInt, element_ty)
            | target_ty -> Types.dynamic_map_types target_ty
          in
          match expected_pair with
          | Some (key_ty, value_ty) ->
              Result.bind (infer_expected key_ty params key_form) (fun params ->
                  Result.bind
                    (infer_expected value_ty params value_form)
                    (fun params -> infer_pairs params rest))
          | None ->
              Result.bind
                (infer_all params [ key_form; value_form ])
                (fun params -> infer_pairs params rest))
      | forms -> infer_all params forms
    in
    let infer_target =
      match (target, pairs) with
      | FSymbol _, (FKeyword _ :: _ | []) -> infer_form params target
      | FSymbol name, key_form :: value_form :: _ -> (
          let target_ty =
            string_assoc_opt name params |> Option.value ~default:TUnknown
          in
          match
            ( inferred_form_type params key_form,
              Types.seqable_constraint_element target_ty,
              Types.dynamic_map_types target_ty )
          with
          | TInt, Some element_ty, None ->
              let value_ty =
                inferred_form_or_call_type ~lookup_function_ty params value_form
              in
              let element_ty =
                if Types.is_dynamic element_ty then
                  match value_ty with
                  | TUnknown | TMeta _ | TVar _ ->
                      fresh_type_variable "assoc_vector"
                  | value_ty -> value_ty
                else refine_type element_ty value_ty
              in
              constrain_symbol
                (TVector element_ty) params name
          | _, _, Some _ ->
              let concrete_or_dynamic form =
                match
                  inferred_form_or_call_type ~lookup_function_ty params form
                with
                | TUnknown -> Type_solver.fresh ()
                | ((TMeta _ | TVar _) as type_parameter) -> type_parameter
                | ty -> ty
              in
              constrain_symbol
                (Types.dynamic_map
                   (concrete_or_dynamic key_form)
                   (concrete_or_dynamic value_form))
                params name
          | _, _, None ->
              constrain_symbol (Types.dynamic_constraint TUnknown) params name)
      | FSymbol name, _ ->
          constrain_symbol (Types.dynamic_constraint TUnknown) params name
      | _ -> infer_form params target
    in
    match infer_target with
    | Error _ as err -> err
    | Ok params -> infer_pairs params pairs
  and infer_assoc_in params target keys value =
    Result.bind (infer_all params (keys @ [ value ])) (fun params ->
        let params =
          keys
          |> List.fold_left
               (fun result key ->
                 Result.bind result (fun params ->
                     match key with
                     | FKeyword _ -> Ok params
                     | key ->
                         infer_expected
                           (Type_solver.fresh ())
                           params key))
               (Ok params)
        in
        Result.bind params (fun params ->
        let value_ty =
          match inferred_form_type params value with
          | TUnknown | TMeta _ | TVar _ -> Type_solver.fresh ()
          | ty -> ty
        in
        Result.bind (infer_expected value_ty params value) (fun params ->
        let target_ty =
          List.fold_right
            (fun key nested_ty ->
              match key with
              | FKeyword keyword ->
                  TRecord [ make_field keyword nested_ty ]
              | key ->
                  let key_ty =
                    match inferred_form_type params key with
                    | TUnknown | TMeta _ | TVar _ -> Type_solver.fresh ()
                    | ty -> ty
                  in
                  Types.dynamic_map key_ty nested_ty)
            keys value_ty
        in
        infer_expected target_ty params target)))
  and infer_match params target clauses =
    let pattern_type = function
      | FInt _ -> Some TInt
      | FString _ -> Some TString
      | FKeyword _ -> Some TKeyword
      | FBool _ -> Some TBool
      | _ -> None
    in
    let rec refine_pattern pattern ty =
      match pattern with
      | FSymbol "_" -> (ty, [])
      | FSymbol name -> (
          match lookup_function_ty name with
          | Ok (TFn ([], return_ty)) -> (return_ty, [])
          | Ok ((TOcaml _ | TOcaml_app _ | TNamed_record _) as return_ty) ->
              (return_ty, [])
          | Ok _ | Error _ -> (ty, [ (name, ty) ]))
      | FList [ FSymbol "Some"; payload_pattern ] ->
          let payload_ty =
            match ty with
            | TNullable payload_ty | TOcaml_app ("option", [ payload_ty ]) ->
                payload_ty
            | _ -> fresh_type_variable "pattern_option"
          in
          let payload_ty, bindings =
            refine_pattern payload_pattern payload_ty
          in
          (TNullable payload_ty, bindings)
      | FList (FSymbol "tuple" :: item_patterns) ->
          let item_tys =
            match ty with
            | TTuple item_tys
              when List.length item_tys = List.length item_patterns ->
                item_tys
            | _ ->
                List.map
                  (fun _ -> fresh_type_variable "pattern_tuple")
                  item_patterns
          in
          let refined = List.map2 refine_pattern item_patterns item_tys in
          (TTuple (List.map fst refined), List.concat_map snd refined)
      | FList (FSymbol constructor :: payload_patterns) -> (
          match lookup_function_ty constructor with
          | Ok (TFn (payload_tys, return_ty))
            when List.length payload_tys = List.length payload_patterns ->
              let refined =
                List.map2 refine_pattern payload_patterns payload_tys
              in
              let refined_payload_tys = List.map fst refined in
              let substitutions =
                List.fold_left2
                  (fun substitutions template actual ->
                    Type_solver.unify substitutions template actual
                    |> Result.value ~default:substitutions)
                  [] payload_tys refined_payload_tys
              in
              ( Type_solver.apply substitutions return_ty,
                List.concat_map snd refined )
          | Ok _ | Error _ -> (ty, []))
      | _ -> (ty, [])
    in
    let variant_pattern = function
      | FSymbol constructor -> (
          match lookup_function_ty constructor with
          | Ok (TFn ([], return_ty)) -> Some (return_ty, [])
          | Ok ((TOcaml _ | TOcaml_app _ | TNamed_record _) as return_ty) ->
              Some (return_ty, [])
          | Ok _ | Error _ -> None)
      | FList [ FSymbol "Some"; payload_pattern ] ->
          let payload_ty, bindings =
            refine_pattern payload_pattern
              (fresh_type_variable "pattern_option")
          in
          Some (TNullable payload_ty, bindings)
      | FList (FSymbol "tuple" :: payload_patterns) ->
          let refined =
            List.map
              (fun pattern ->
                refine_pattern pattern
                  (fresh_type_variable "pattern_tuple"))
              payload_patterns
          in
          Some (TTuple (List.map fst refined), List.concat_map snd refined)
      | FList (FSymbol constructor :: payload_patterns) -> (
          match lookup_function_ty constructor with
          | Ok (TFn (payload_tys, return_ty))
            when List.length payload_tys = List.length payload_patterns ->
              let refined =
                List.map2 refine_pattern payload_patterns payload_tys
              in
              let refined_payload_tys = List.map fst refined in
              let bindings = List.concat_map snd refined in
              let substitutions =
                List.fold_left2
                  (fun substitutions template actual ->
                    Type_solver.unify substitutions template actual
                    |> Result.value ~default:substitutions)
                  [] payload_tys refined_payload_tys
              in
              Some (Type_solver.apply substitutions return_ty, bindings)
          | Ok _ | Error _ -> None)
      | _ -> None
    in
    let target_needs_inference params =
      Type_solver.is_open (inferred_form_type params target)
    in
    let constructor_symbol name =
      let segments =
        name |> String.split_on_char '/'
        |> List.concat_map (String.split_on_char '.')
      in
      match List.rev segments with
      | segment :: _ when String.length segment > 0 ->
          let first = segment.[0] in
          first >= 'A' && first <= 'Z'
      | _ -> false
    in
    let zero_arity_constructor name =
      constructor_symbol name
      ||
      match lookup_function_ty name with
      | Ok (TFn ([], _))
      | Ok (TOcaml _ | TOcaml_app _ | TNamed_record _) ->
          true
      | Ok _ | Error _ -> false
    in
    let infer_variant_clause params expected_ty bindings result =
      Result.bind (infer_expected expected_ty params target) (fun params ->
          let local_names = List.map fst bindings in
          let shadowed =
            List.filter (fun (name, _) -> string_mem name local_names) params
          in
          let branch_params =
            bindings
            @ List.filter
                (fun (name, _) -> not (string_mem name local_names))
                params
          in
          Result.bind (infer_form branch_params result) (fun inferred ->
              let substitutions =
                List.fold_left
                  (fun substitutions (name, initial_ty) ->
                    match string_assoc_opt name inferred with
                    | None -> substitutions
                    | Some inferred_ty ->
                        Type_solver.unify substitutions initial_ty inferred_ty
                        |> Result.value ~default:substitutions)
                  [] bindings
              in
              let expected_ty = Type_solver.apply substitutions expected_ty in
              let params =
                shadowed
                @ List.filter
                    (fun (name, _) -> not (string_mem name local_names))
                    inferred
              in
              infer_expected expected_ty params target))
    in
    let infer_option_clause params binding result =
      let initial_payload_ty =
        match inferred_form_type params target with
        | TNullable inner | TOcaml_app ("option", [ inner ]) -> inner
        | _ -> fresh_type_variable "option"
      in
      let shadowed = string_assoc_opt binding params in
      let branch_params =
        (binding, initial_payload_ty) :: string_remove_assoc binding params
      in
      match infer_form branch_params result with
      | Error _ as error -> error
      | Ok branch_params -> (
          let payload_ty =
            string_assoc_opt binding branch_params
            |> Option.value ~default:initial_payload_ty
          in
          let params = string_remove_assoc binding branch_params in
          let params =
            match shadowed with
            | None -> params
            | Some ty -> (binding, ty) :: params
          in
          match payload_ty with
          | TUnknown -> infer_form params target
          | payload_ty -> infer_expected (TNullable payload_ty) params target)
    in
    let rec infer_clauses params = function
      | [] -> Ok params
      | [ form ] -> infer_form params form
      | FList [ FSymbol "Some"; FSymbol binding ] :: result :: rest
        when not (zero_arity_constructor binding) -> (
          match infer_option_clause params binding result with
          | Error _ as error -> error
          | Ok params -> infer_clauses params rest)
      | pattern :: result :: rest -> (
          let inferred =
            match pattern_type pattern with
            | Some expected_ty ->
                Result.bind
                  (infer_expected expected_ty params target)
                  (fun params -> infer_form params result)
            | None -> (
                match
                  if target_needs_inference params then
                    variant_pattern pattern
                  else None
                with
                | Some (expected_ty, bindings) ->
                    infer_variant_clause params expected_ty bindings result
                | None ->
                    Result.bind (infer_form params target) (fun params ->
                        infer_form params result))
          in
          match inferred with
          | Error _ as err -> err
          | Ok params -> infer_clauses params rest)
    in
    Result.bind (infer_clauses params clauses) (fun params ->
        let rec result_forms results = function
          | _pattern :: result :: rest ->
              result_forms (result :: results) rest
          | _ -> List.rev results
        in
        let results = result_forms [] clauses in
        let result_types =
          results
          |> List.map (inferred_form_type params)
          |> List.filter (fun ty ->
                 not (Types.equal ty TUnknown)
                 &&
                 match ty with TMeta _ | TVar _ -> false | _ -> true)
        in
        let concrete_types =
          result_types
          |> List.fold_left
               (fun unique ty ->
                 if List.exists (Types.equal ty) unique then unique
                 else ty :: unique)
               []
        in
        let requires_dynamic =
          List.exists Types.is_dynamic result_types
          || List.length concrete_types > 1
          || List.exists
               (function
                 | FSymbol name -> (
                     match string_assoc_opt name params with
                     | Some TUnknown -> true
                     | _ -> false)
                 | _ -> false)
               results
        in
        if not requires_dynamic then Ok params
        else
          results
          |> List.fold_left
               (fun result -> function
                 | FSymbol name ->
                     Result.bind result (fun params ->
                         constrain_symbol
                           (Types.dynamic_constraint TUnknown)
                           params name)
                 | _ -> result)
               (Ok params))
  and update_signature updater extra_argument_count =
    match lookup_function_ty updater with
    | Ok (TFn (field_ty :: extra_tys, return_ty))
      when List.length extra_tys = extra_argument_count ->
        Some (field_ty, extra_tys, return_ty)
    | Ok (TOverloaded_fn arities) ->
        Option.bind
          (select_fn_arity arities (extra_argument_count + 1))
          (fun arity ->
            match arity.fixed_params with
            | field_ty :: extra_tys
              when Option.is_none arity.rest_param
                   && List.length extra_tys = extra_argument_count ->
                Some (field_ty, extra_tys, arity.return_ty)
            | _ -> None)
    | Ok _ | Error _ -> None
  and updated_value_type field_ty return_ty =
    match field_ty with
    | TNullable inner | TOcaml_app ("option", [ inner ])
      when not (Types.equal return_ty TUnknown)
           && Types.assignable ~policy:Host_boundary ~expected:inner
                ~actual:return_ty ->
        refine_type inner return_ty
    | _ -> refine_type field_ty return_ty
  and infer_form params = function
    | FList (FCoreSymbol core_symbol :: arguments) ->
        let name =
          match core_symbol with
          | Core_update -> Ast.core_symbol_qualified_name core_symbol
          | _ -> Ast.core_symbol_name core_symbol
        in
        infer_form params
          (FList (FSymbol name :: arguments))
    | FList [ FSymbol "->Eduction"; transducer; collection ] ->
        Result.bind
          (Core_form_expansion.apply_transducer collection transducer)
          (infer_form params)
    | FList [ FSymbol "__type-hint"; FSymbol annotation; value ] -> (
        match Type_annotation.of_param_annotation annotation with
        | Error _ as error -> error
        | Ok hinted_ty when !branch_depth = 0 ->
            let hinted_ty = resolve_named_record hinted_ty in
            (match value with
            | FSymbol name -> constrain_symbol hinted_ty params name
            | value -> infer_expected hinted_ty params value)
        | Ok _hinted_ty ->
            (match value with
            | FSymbol name when not (string_mem name !branch_hint_symbols) ->
                branch_hint_symbols := name :: !branch_hint_symbols
            | _ -> ());
            infer_form params value)
    | FList
        (FSymbol "record" :: FSymbol record_type_name :: field_forms) ->
        let record_fields =
          resolve_named_record (TOcaml record_type_name)
          |> Type_solver.generalize |> Type_solver.instantiate
          |> Types.record_fields
        in
        List.fold_left
          (fun result field_form ->
            Result.bind result (fun params ->
                match field_form with
                | FList [ FSymbol field_name; value ] -> (
                    match
                      Option.bind record_fields (fun fields ->
                          Types.find_field (":" ^ field_name) fields)
                    with
                    | Some field -> infer_expected field.ty params value
                    | None -> infer_form params value)
                | _ -> Ok params))
          (Ok params) field_forms
    | FList (FSymbol "record" :: _record_type :: field_forms) ->
        infer_all params
          (List.filter_map
             (function
               | FList [ FSymbol _field_name; value ] -> Some value
               | _ -> None)
             field_forms)
    | FList
        [
          FSymbol ("if-some" | "if-let");
          FVector [ FSymbol binding; option_form ];
          then_form;
          else_form;
        ] -> (
        let inferred_option_ty =
          inferred_binding_form_type params option_form
        in
        let initial_payload_ty =
          match inferred_option_ty with
          | TNullable inner | TOcaml_app ("option", [ inner ]) -> inner
          | _ -> fresh_type_variable "option"
        in
        let shadowed = string_assoc_opt binding params in
        let branch_params =
          (binding, initial_payload_ty) :: string_remove_assoc binding params
        in
        let infer_then_branch =
          match (then_form, inferred_form_type branch_params else_form) with
          | ( FList (FSymbol "conj" :: FSymbol target :: values),
              (TList element_ty | TSeq element_ty) )
            when String.equal target binding ->
              Result.bind
                (constrain_symbol (TSeq element_ty) branch_params binding)
                (fun branch_params ->
                  infer_expected_all element_ty branch_params values)
          | ( FList (FSymbol "conj" :: FSymbol target :: values),
              ((TVector element_ty | TSet element_ty) as collection_ty) )
            when String.equal target binding ->
              Result.bind
                (constrain_symbol collection_ty branch_params binding)
                (fun branch_params ->
                  infer_expected_all element_ty branch_params values)
          | _ -> (
              match inferred_form_type params else_form with
              | ty
                when Types.is_dynamic ty
                     || match ty with
                        | TUnknown | TMeta _ | TVar _ -> true
                        | _ -> false ->
                  infer_form branch_params then_form
              | expected_ty ->
                  infer_expected expected_ty branch_params then_form)
        in
        match infer_then_branch with
        | Error _ as error -> error
        | Ok branch_params ->
            let payload_ty =
              string_assoc_opt binding branch_params
              |> Option.value ~default:initial_payload_ty
            in
            let params = string_remove_assoc binding branch_params in
            let params =
              match shadowed with
              | None -> params
              | Some ty -> (binding, ty) :: params
            in
            let infer_option =
              match payload_ty with
              | TUnknown -> infer_form params option_form
              | payload_ty ->
                  let expected_ty =
                    match option_form with
                    | FList
                        [
                          FSymbol "first";
                          _collection;
                        ] ->
                        payload_ty
                    | _ when (match payload_ty with TSeq _ -> true | _ -> false) ->
                        let element_ty =
                          match payload_ty with
                          | TSeq element_ty -> element_ty
                          | _ -> assert false
                        in
                        Types.next_seq element_ty
                    | _ -> TNullable payload_ty
                  in
                  infer_expected expected_ty params option_form
            in
            Result.bind infer_option (fun params -> infer_form params else_form)
        )
    | FList
        (FSymbol ("when-some" | "when-let")
        :: FVector [ FSymbol binding; option_form ]
        :: body_forms) -> (
        let initial_payload_ty =
          match inferred_binding_form_type params option_form with
          | TNullable inner | TOcaml_app ("option", [ inner ]) -> inner
          | _ -> fresh_type_variable "option"
        in
        let shadowed = string_assoc_opt binding params in
        let branch_params =
          (binding, initial_payload_ty) :: string_remove_assoc binding params
        in
        match infer_all branch_params body_forms with
        | Error _ as error -> error
        | Ok branch_params ->
            let payload_ty =
              string_assoc_opt binding branch_params
              |> Option.value ~default:initial_payload_ty
            in
            let params = string_remove_assoc binding branch_params in
            let params =
              match shadowed with
              | None -> params
              | Some ty -> (binding, ty) :: params
            in
            (match payload_ty with
            | TUnknown -> infer_form params option_form
            | payload_ty ->
                let expected_ty =
                  match option_form with
                  | FList
                      [
                        FSymbol "first";
                        _collection;
                      ] ->
                      payload_ty
                  | _ -> TNullable payload_ty
                in
                infer_expected expected_ty params option_form))
    | FList
        [
          FSymbol
            ("with-meta" | "clojure.core/with-meta" | "cljs.core/with-meta");
          value;
          metadata;
        ] ->
        Result.bind (infer_form params value) (fun params ->
            infer_form params metadata)
    | (FList (FSymbol ("and" | "or") :: _) as form) ->
        infer_truthy params form
    | FList
        [ FSymbol ("meta" | "clojure.core/meta" | "cljs.core/meta"); value ] ->
        infer_form params value
    | FList [ FSymbol predicate; FSymbol value ]
      when has_source_name predicate "symbol?" ->
        constrain_symbol_predicate params value
    | FList [ FSymbol predicate; FSymbol value ]
      when List.exists
             (has_source_name predicate)
             [
               "keyword?";
               "string?";
               "int?";
               "number?";
               "array?";
               "vector?";
               "list?";
               "seq?";
               "set?";
               "map?";
               "fn?";
               "coll?";
             ] ->
        constrain_symbol (Types.dynamic_constraint TUnknown) params value
    | FList
        [
          FSymbol ("name" | "namespace" | "hash");
          FSymbol value;
        ] ->
        constrain_symbol (Types.dynamic_constraint TUnknown) params value
    | FList
        [ FSymbol ("compare" | "ordering-compare"); FSymbol left; FSymbol right ] -> (
        match (string_assoc_opt left params, string_assoc_opt right params) with
        | Some (TUnknown | TMeta _ | TVar _),
          Some (TUnknown | TMeta _ | TVar _) ->
            infer_expected_all
              (fresh_type_variable "comparison")
              params [ FSymbol left; FSymbol right ]
        | _ -> (
            match constrain_comparable_symbol params left with
            | Error _ as error -> error
            | Ok params -> constrain_comparable_symbol params right))
    | FList [ FSymbol ("identical?" | ".equals"); left; right ] ->
        Result.bind (infer_all params [ left; right ]) (fun params ->
            let left_ty = inferred_form_type params left in
            let right_ty = inferred_form_type params right in
            let identity_ty =
              if not (Type_solver.is_open left_ty) then left_ty
              else if not (Type_solver.is_open right_ty) then right_ty
              else fresh_type_variable "identical-value"
            in
            infer_expected_all identity_ty params [ left; right ])
    | FList [ FSymbol "int"; FSymbol value ] ->
        constrain_symbol (Types.dynamic_constraint TUnknown) params value
    | FList
        (FSymbol (".valAt" | ".containsKey" | ".entryAt")
        :: FList [ FSymbol field_access; FSymbol receiver ]
        :: key_form :: remaining)
      when String.starts_with ~prefix:".-" field_access ->
        let keyword =
          ":"
          ^ String.sub field_access 2 (String.length field_access - 2)
        in
        let key_ty =
          match inferred_form_type params key_form with
          | TUnknown | TMeta _ | TVar _ -> Types.dynamic_constraint TUnknown
          | ty -> ty
        in
        Result.bind
          (add_record_field_constraint receiver keyword
             (Types.dynamic_map key_ty TUnknown)
             params)
          (fun params -> infer_all params (key_form :: remaining))
    | FList
        (FSymbol (".valAt" | ".containsKey" | ".entryAt")
        :: FSymbol target :: arguments) ->
        Result.bind
          (constrain_symbol (Types.dynamic_constraint TUnknown) params target)
          (fun params -> infer_all params arguments)
    | FList [ FSymbol field_access; FSymbol name ]
      when String.starts_with ~prefix:".-" field_access ->
        let keyword =
          ":"
          ^ String.sub field_access 2 (String.length field_access - 2)
        in
        add_record_field_constraint name keyword TUnknown params
    | FList [ FSymbol "instance?"; FSymbol type_name; FSymbol value ] -> (
        match resolve_named_record (TOcaml type_name) with
        | TNamed_record _ as record_ty ->
            constrain_symbol record_ty params value
        | _ -> Ok params)
    | FList [ FSymbol operation; FSymbol name ]
      when string_mem_assoc name params
           && (has_source_name operation "keys"
              || has_source_name operation "vals") ->
        let key_ty = fresh_type_variable "map_key" in
        let value_ty = fresh_type_variable "map_value" in
        constrain_symbol (Types.dynamic_map key_ty value_ty) params name
    | FList (FSymbol name :: arguments) when string_mem_assoc name params -> (
        let parameter_tys =
          List.mapi
            (fun index argument ->
              let argument_ty = inferred_form_type params argument in
              let argument_ty =
                if Types.equal argument_ty TUnknown then
                  inferred_call_return_type ~lookup_function_ty params argument
                else argument_ty
              in
              match argument_ty with
              | TUnknown ->
                  fresh_type_variable
                    ("call_" ^ Names.sanitize_name name ^ "_"
                   ^ string_of_int index)
              | ty -> ty)
            arguments
        in
        match constrain_symbol (TFn (parameter_tys, TUnknown)) params name with
        | Error _ as err -> err
        | Ok params ->
            List.fold_left2
              (fun result expected argument ->
                Result.bind result (fun params ->
                    infer_expected expected params argument))
              (Ok params) parameter_tys arguments)
    | FList [ FSymbol "deref"; FList [ FKeyword keyword; FSymbol name ] ] ->
        add_record_field_constraint name keyword (TRef TUnknown) params
    | FList
        [
          FSymbol ("weak-deref" | "weak-clear!");
          FList [ FKeyword keyword; FSymbol name ];
        ] ->
        add_record_field_constraint name keyword
          (Types.weak_type TUnknown) params
    | FList [ FSymbol ("weak-deref" | "weak-clear!"); FSymbol name ] ->
        constrain_symbol (Types.weak_type TUnknown) params name
    | FList [ FSymbol "weak-ref"; value ] -> infer_form params value
    | FList
        [ FSymbol "compare-and-set!"; reference; old_value; new_value ] -> (
        match inferred_form_type params reference with
        | TRef referenced_ty ->
            Result.bind (infer_expected referenced_ty params old_value)
              (fun params ->
                infer_expected referenced_ty params new_value)
        | _ -> infer_all params [ reference; old_value; new_value ])
    | FList
        [ FSymbol ("reset!" | "vreset!"); FSymbol reference; value ]
      when
        (match string_assoc_opt reference params with
        | Some (TRef _ | TUnknown | TMeta _ | TVar _) | None -> false
        | Some _ -> true) ->
        infer_form params value
    | FList
        [ FSymbol ("reset!" | "vreset!"); FSymbol reference; value ] ->
        let value_ty = inferred_form_type params value in
        let referenced_ty =
          match string_assoc_opt reference params with
          | Some (TRef (TNullable _)) -> TNullable value_ty
          | Some (TRef (TOcaml_app ("option", [ _ ]))) ->
              TOcaml_app ("option", [ value_ty ])
          | Some _ | None -> value_ty
        in
        Result.bind
          (constrain_symbol (TRef referenced_ty) params reference)
          (fun params ->
            match value_ty with
            | TUnknown | TMeta _ | TVar _ -> infer_form params value
            | value_ty -> infer_expected value_ty params value)
    | FList
        [
          FSymbol ("swap!" | "vswap!");
          FSymbol reference;
          FSymbol "conj";
          value;
        ] -> (
        let element_ty =
          match inferred_form_type params value with
          | TUnknown -> Type_solver.fresh ()
          | ty -> ty
        in
        let collection_ty =
          match string_assoc_opt reference params with
          | Some (TRef (TVector _)) -> Some (TVector element_ty)
          | Some (TRef (TList _)) -> Some (TList element_ty)
          | Some (TRef (TSet _)) -> Some (TSet element_ty)
          | Some (TRef (TSeq _)) -> Some (TSeq element_ty)
          | Some _ | None -> None
        in
        match collection_ty with
        | None -> infer_form params value
        | Some collection_ty ->
            Result.bind
              (constrain_symbol (TRef collection_ty) params reference)
              (fun params -> infer_expected element_ty params value))
    | FList
        [
          FSymbol ("swap!" | "vswap!");
          FSymbol reference;
          FSymbol "assoc!";
          key;
          value;
        ] ->
        let inferred_or_dynamic form =
          match inferred_form_type params form with
          | TUnknown | TMeta _ | TVar _ -> Type_solver.fresh ()
          | ty -> ty
        in
        let key_ty = inferred_or_dynamic key in
        let value_ty = inferred_or_dynamic value in
        let reference_ty =
          TRef
            (TOcaml_app
               ( "Lg_runtime.Runtime_transient.map",
                 [ key_ty; value_ty ] ))
        in
        Result.bind (constrain_symbol reference_ty params reference)
          (fun params ->
            Result.bind (infer_expected key_ty params key) (fun params ->
                infer_expected value_ty params value))
    | FList
        [
          FSymbol ("swap!" | "vswap!");
          FSymbol reference;
          FSymbol "conj!";
          value;
        ] ->
        let element_ty =
          match inferred_form_type params value with
          | TUnknown | TMeta _ | TVar _ -> Type_solver.fresh ()
          | ty -> ty
        in
        let reference_ty =
          TRef
            (TOcaml_app
               ("Lg_runtime.Runtime_transient.vector", [ element_ty ]))
        in
        Result.bind (constrain_symbol reference_ty params reference)
          (fun params -> infer_expected element_ty params value)
    | FList
        (FSymbol ("swap!" | "vswap!") :: reference :: update_fn
       :: arguments) ->
        Result.bind (infer_form params reference) (fun params ->
            Result.bind (infer_form params update_fn) (fun params ->
                let expected = inferred_form_type params reference in
                let update_ty = inferred_form_type params update_fn in
                let expected_argument =
                  match (expected, update_ty) with
                  | TRef _, TFn _ -> TUnknown
                  | expected, _ when not (Types.is_dynamic expected) -> TUnknown
                  | _ -> Types.dynamic_constraint TUnknown
                in
                List.fold_left
                  (fun result argument ->
                    Result.bind result (fun params ->
                        if Types.equal expected_argument TUnknown then
                          infer_form params argument
                        else infer_expected expected_argument params argument))
                  (Ok params) arguments))
    | FList [ FSymbol "dissoc!"; FSymbol collection; key ] ->
        let key_ty =
          match inferred_form_type params key with
          | TUnknown | TMeta _ | TVar _ -> Types.dynamic_constraint TUnknown
          | ty -> ty
        in
        let value_ty = Types.dynamic_constraint TUnknown in
        let transient_map =
          TOcaml_app
            ( "Lg_runtime.Runtime_transient.map",
              [ key_ty; value_ty ] )
        in
        Result.bind
          (constrain_symbol (Types.dynamic_constraint transient_map) params
             collection)
          (fun params -> infer_expected key_ty params key)
    | FList
        [
          FSymbol "__deftype-field-set!";
          FKeyword keyword;
          FSymbol receiver;
          value;
        ] -> (
        match record_mutable_field_value_type params receiver keyword with
        | Some ty -> infer_expected ty params value
        | None -> infer_form params value)
    | FList
        [ FSymbol "vreset!"; FList [ FKeyword keyword; FSymbol name ]; value ]
      -> (
        match record_ref_field_value_type params name keyword with
        | Some ty -> infer_expected ty params value
        | None -> (
            match value with
            | FList [ FSymbol "Some"; FSymbol value_name ] ->
                let payload_ty =
                  match string_assoc_opt value_name params with
                  | Some (TUnknown | TMeta _ | TVar _) | None ->
                      fresh_type_variable
                        ("option_" ^ Names.sanitize_name value_name)
                  | Some ty -> ty
                in
                Result.bind
                  (add_record_field_constraint name keyword
                     (TRef (TNullable payload_ty)) params)
                  (fun params ->
                    constrain_symbol payload_ty params value_name)
            | _ ->
                let value_ty = inferred_form_type params value in
                Result.bind
                  (add_record_field_constraint name keyword (TRef value_ty)
                     params)
                  (fun params -> infer_form params value)))
    | FList [ FSymbol "nil?"; value ] ->
        let inferred_ty = inferred_form_type params value in
        let expected_ty =
          match inferred_ty with
          | TNullable ((TUnknown | TMeta _ | TVar _) as payload_ty) ->
              TNullable (Types.nil_predicate_constraint payload_ty)
          | TOcaml_app
              ("option", [ (TUnknown | TMeta _ | TVar _) as payload_ty ]) ->
              TOcaml_app
                ("option", [ Types.nil_predicate_constraint payload_ty ])
          | TUnknown -> Types.nil_predicate_constraint (Type_solver.fresh ())
          | (TMeta _ | TVar _) as value_ty ->
              Types.nil_predicate_constraint value_ty
          | ty -> ty
        in
        let nil_predicate_optional_seqable =
          Option.bind
            (Types.nil_predicate_constraint_info inferred_ty)
            (fun ty ->
              match Types.seqable_constraint_info ty with
              | Some ((`Optional | `Optional_sequential), _, _) -> Some ty
              | Some (`Required, _, _) | None -> None)
        in
        (match (value, inferred_ty) with
        | FSymbol name, _
          when Option.is_some nil_predicate_optional_seqable ->
            Ok
              (replace_param name
                 (Option.get nil_predicate_optional_seqable)
                 params)
        | FSymbol name, ty
          when (match Types.seqable_constraint_info ty with
               | Some ((`Optional | `Optional_sequential), _, _) -> true
               | Some (`Required, _, _) | None -> false) ->
            Ok (replace_param name ty params)
        | ( FSymbol name,
            ((TNullable _ | TOcaml_app ("option", [ _ ])) as ty) ) ->
            Ok (replace_param name ty params)
        | FSymbol name, ty
          when (match ty with
               | TUnknown | TMeta _ | TVar _ -> false
               | _ -> true) ->
            Ok (replace_param name (TNullable ty) params)
        | _ -> infer_expected expected_ty params value)
    | FList [ FSymbol "count"; collection ] -> (
        match collection with
        | FSymbol name -> constrain_seqable TUnknown params name
        | form ->
            infer_expected (Types.seqable_constraint TUnknown) params form)
    | FList [ FSymbol "hash-unordered-coll"; FSymbol collection ] ->
        constrain_seqable TUnknown params collection
    | FList (FSymbol "merge" :: maps) ->
        infer_expected_all (Types.dynamic_constraint TUnknown) params maps
    | FList
        (FSymbol ("update" | "clojure.core/update")
        :: target :: key
        :: (FSymbol ("update" | "clojure.core/update")
           | FCoreSymbol Core_update)
        :: nested_arguments) ->
        let nested_value = "__lg_nested_update_value" in
        let params =
          match (target, key) with
          | FSymbol target, FKeyword keyword ->
              let field_ty =
                match nested_arguments with
                | nested_key :: FSymbol updater :: extra_arguments -> (
                    match
                      update_signature updater (List.length extra_arguments)
                    with
                    | Some (updater_field_ty, _, return_ty) ->
                        let key_ty =
                          inferred_form_type params nested_key
                          |> materialize_dynamic_unknown
                        in
                        let value_ty =
                          updated_value_type updater_field_ty return_ty
                        in
                        Types.dynamic_map key_ty value_ty
                    | None -> Types.dynamic_constraint TUnknown)
                | _ -> Types.dynamic_constraint TUnknown
              in
              add_record_field_constraint target keyword
                field_ty params
          | _ -> Ok params
        in
        Result.bind params (fun params ->
            infer_form params
              (FList
                 [
                   FCoreSymbol Core_update;
                   target;
                   key;
                   FList
                     [
                       FSymbol "fn";
                       FVector [ FSymbol nested_value ];
                       FList
                         (FCoreSymbol Core_update :: FSymbol nested_value
                        :: nested_arguments);
                     ];
                 ]))
    | FList
        (FSymbol ("update" | "clojure.core/update")
        :: FSymbol target
        :: (FInt _ as index)
        :: FSymbol updater
        :: extra_arguments) -> (
        match lookup_function_ty updater with
        | Ok (TFn (element_ty :: extra_tys, return_ty))
          when List.length extra_tys = List.length extra_arguments
               && Types.equal (refine_type element_ty return_ty) element_ty ->
            Result.bind (constrain_symbol (TVector element_ty) params target)
              (fun params ->
                Result.bind (infer_expected TInt params index) (fun params ->
                    List.fold_left2
                      (fun result expected argument ->
                        Result.bind result (fun params ->
                            infer_expected expected params argument))
                      (Ok params) extra_tys extra_arguments))
        | Ok _ | Error _ -> infer_all params extra_arguments)
    | FList
        (FSymbol ("update" | "clojure.core/update")
        :: FSymbol target
        :: FKeyword keyword
        :: FSymbol updater
        :: extra_arguments) -> (
        let signature =
          match (updater, extra_arguments) with
          | ("conj" | "clojure.core/conj"), _ :: _ ->
              let element_ty =
                extra_arguments
                |> List.map (inferred_form_type params)
                |> List.fold_left refine_type TUnknown
                |> stored_value_type
              in
              let collection_ty =
                match
                  string_assoc_opt target params
                  |> fun target_ty ->
                  Option.bind target_ty (fun target_ty ->
                         match Types.constraint_value_type target_ty with
                         | TRecord fields | TNamed_record { fields; _ } ->
                             Option.map
                               (fun (field : field) -> field.ty)
                               (Types.find_field keyword fields)
                         | _ -> None)
                with
                | Some (TList _) -> TList element_ty
                | Some (TSeq _) -> TSeq element_ty
                | Some (TSet _) -> TSet element_ty
                | Some (TVector _) | Some _ | None -> TVector element_ty
              in
              Some
                ( collection_ty,
                  List.map (fun _ -> element_ty) extra_arguments,
                  collection_ty )
          | _ -> update_signature updater (List.length extra_arguments)
        in
        match signature with
        | None -> infer_all params extra_arguments
        | Some (field_ty, extra_tys, return_ty) -> (
            let field_ty = updated_value_type field_ty return_ty in
            match
              add_record_field_constraint target keyword field_ty params
            with
            | Error _ as error -> error
            | Ok params ->
                List.fold_left2
                  (fun result expected argument ->
                    Result.bind result (fun params ->
                        infer_expected expected params argument))
                  (Ok params) extra_tys extra_arguments))
    | FList
        (FSymbol ("update" | "clojure.core/update")
        :: FSymbol target
        :: key
        :: updater
        :: extra_arguments) ->
        let infer_inline_updater_return = function
          | FList
              (FSymbol "fn" :: FVector parameter_forms :: body_forms) -> (
              let parameter_names =
                List.filter_map
                  (function FSymbol name -> Some name | _ -> None)
                  parameter_forms
              in
              if List.length parameter_names <> List.length parameter_forms then
                TUnknown
              else
                let function_params =
                  List.map
                    (fun name -> (name, Type_solver.fresh ()))
                    parameter_names
                  @ params
                in
                match infer_all function_params body_forms with
                | Error _ -> TUnknown
                | Ok inferred ->
                    (match List.rev body_forms with
                    | result :: _ -> inferred_form_type inferred result
                    | [] -> TNil))
          | _ -> TUnknown
        in
        let target_ty, key_ty =
          match string_assoc_opt target params with
          | Some (TVector _ as ty) -> (ty, TInt)
          | _ -> (
              match infer_inline_updater_return updater with
              | return_ty
                when not
                       (match return_ty with
                       | TUnknown | TMeta _ | TVar _ -> true
                       | _ -> false) ->
                  let key_ty =
                    match inferred_form_type params key with
                    | TUnknown | TMeta _ | TVar _ -> Type_solver.fresh ()
                    | ty -> ty
                  in
                  (Types.dynamic_map key_ty return_ty, key_ty)
              | _ ->
                  let dynamic = Types.dynamic_constraint TUnknown in
                  (dynamic, dynamic))
        in
        Result.bind (constrain_symbol target_ty params target) (fun params ->
            Result.bind (infer_expected key_ty params key) (fun params ->
                infer_all params (updater :: extra_arguments)))
    | FList [ FSymbol ("get" | "clojure.core/get"); FSymbol target; key ]
      when match key with FKeyword _ -> false | _ -> true -> (
        let infer_map () =
          let key_ty =
            match inferred_form_type params key with
            | TUnknown | TMeta _ | TVar _ -> Type_solver.fresh ()
            | ty -> ty
          in
          Result.bind
            (constrain_symbol
               (Types.dynamic_map key_ty (Type_solver.fresh ()))
               params target)
            (fun params -> infer_expected key_ty params key)
        in
        match
          string_assoc_opt target params
          |> Option.map Types.constraint_value_type
        with
        | Some (TNamed_record _) -> infer_expected TKeyword params key
        | Some map_ty -> (
            match Types.dynamic_map_types map_ty with
            | Some (key_ty, _) -> infer_expected key_ty params key
            | None -> infer_map ())
        | None -> infer_map ())
    | FList [ FSymbol operation; FSymbol array ]
      when String.equal operation "Array.length" ->
        let element_ty =
          match string_assoc_opt array params with
          | Some (TArray element_ty | TOcaml_app ("array", [ element_ty ])) ->
              element_ty
          | _ -> fresh_type_variable ("array_" ^ Names.sanitize_name array)
        in
        constrain_symbol (TArray element_ty) params array
    | FList [ FSymbol operation; FSymbol array; from; length ]
      when String.equal operation "Array.sub" ->
        let element_ty =
          match string_assoc_opt array params with
          | Some (TArray element_ty | TOcaml_app ("array", [ element_ty ])) ->
              element_ty
          | _ -> fresh_type_variable ("array_" ^ Names.sanitize_name array)
        in
        Result.bind (constrain_symbol (TArray element_ty) params array)
          (fun params ->
            Result.bind (infer_expected TInt params from) (fun params ->
                infer_expected TInt params length))
    | FList [ FSymbol operation; FSymbol array; index ]
      when has_source_name operation "aget"
           || has_source_name operation "unsafe-aget" -> (
        let element_ty =
          match string_assoc_opt array params with
          | Some (TArray element_ty | TOcaml_app ("array", [ element_ty ])) ->
              element_ty
          | _ -> fresh_type_variable ("array_" ^ Names.sanitize_name array)
        in
        match constrain_symbol (TArray element_ty) params array with
        | Error _ as error -> error
        | Ok params ->
            let index_ty = inferred_form_type params index in
            infer_expected
              (if Types.equal index_ty TFloat then TFloat else TInt)
              params index)
    | FList [ FSymbol "nth"; FSymbol collection; index ] ->
        Result.bind (constrain_seqable TUnknown params collection)
          (fun params -> infer_expected TInt params index)
    | FList [ FSymbol "sequential?"; FSymbol collection ] ->
        constrain_optional_seqable ~sequential:true
          TUnknown params collection
    | FList [ FSymbol "empty"; FSymbol collection ] -> (
        match string_assoc_opt collection params with
        | Some (TUnknown | TMeta _ | TVar _) ->
            constrain_symbol (Types.dynamic_constraint TUnknown) params
              collection
        | Some ty when Option.is_some (Types.seqable_constraint_info ty) ->
            Ok
              (replace_param collection (Types.dynamic_constraint ty) params)
        | Some _ | None -> Ok params)
    | FList [ FSymbol "satisfies?"; FSymbol protocol_name; FSymbol receiver ]
      -> (
        match lookup_protocol_constraint protocol_name with
        | None -> Error.error ("unknown protocol " ^ protocol_name)
        | Some constraint_ty ->
            constrain_symbol
              (Types.guarded_protocol_constraint constraint_ty)
              params receiver)
    | FList
        [
          FSymbol (("uncurried-call" | "uncurried-compare") as name);
          FSymbol fn;
          left;
          right;
        ] ->
        let left_ty = inferred_form_type params left in
        let right_ty = inferred_form_type params right in
        let return_ty =
          match string_assoc_opt fn params with
          | Some (TFn (_, (TUnknown | TMeta _ | TVar _)))
            when name = "uncurried-compare" ->
              TOcaml "int"
          | Some (TFn (_, return_ty)) -> return_ty
          | _ ->
              if name = "uncurried-compare" then TOcaml "int" else TUnknown
        in
        if name = "uncurried-compare" then
          let value_ty =
            match string_assoc_opt fn params with
            | Some (TFn ([ left; right ], _))
              when Types.equal left right
                   && not (Types.equal left TUnknown)
                   && (match left with TMeta _ | TVar _ -> false | _ -> true) ->
                left
            | _ -> (
                match (left_ty, right_ty) with
                | ty, _
                  when not (Types.equal ty TUnknown)
                       && (match ty with TMeta _ | TVar _ -> false | _ -> true) ->
                    ty
                | _, ty
                  when not (Types.equal ty TUnknown)
                       && (match ty with TMeta _ | TVar _ -> false | _ -> true) ->
                    ty
                | _ -> fresh_type_variable "ordering_value")
          in
          Result.bind
            (constrain_symbol
               (TFn ([ value_ty; value_ty ], return_ty))
               params fn)
            (fun params -> infer_expected_all value_ty params [ left; right ])
        else
          constrain_symbol
            (TFn ([ left_ty; right_ty ], return_ty))
            params fn
    | FList [ FSymbol "as-ordering"; FSymbol fn ] ->
        let fn_ty =
          match string_assoc_opt fn params with
          | Some (TFn (parameter_tys, (TOcaml "int" as return_ty))) ->
              TFn (parameter_tys, return_ty)
          | Some (TFn (parameter_tys, _)) -> TFn (parameter_tys, TInt)
          | _ ->
              let value_ty = fresh_type_variable "ordering_value" in
              TFn ([ value_ty; value_ty ], TInt)
        in
        constrain_symbol fn_ty params fn
    | FList [ FSymbol "seq-uncons"; FSymbol collection ] ->
        constrain_symbol
          (TSeq (fresh_type_variable "seq_uncons_element"))
          params collection
    | FList
        [
          FSymbol (("first" | "seq" | "rest" | "next") as operation);
          FSymbol collection;
        ] ->
        constrain_seqable
          (fresh_type_variable
             ("sequence_" ^ Names.sanitize_name operation ^ "_element"))
          params collection
    | FList
        [
          FSymbol ("first" | "seq" | "rest" | "next");
          FList [ FKeyword keyword; FSymbol record ];
        ] ->
        add_record_field_constraint record keyword
          (Types.seqable_constraint
             (fresh_type_variable
                ("sequence_" ^ Names.sanitize_name keyword ^ "_element")))
          params
    | FList
        [
          FSymbol (("first" | "seq" | "rest" | "next") as operation);
          collection;
        ] ->
        infer_expected
          (Types.seqable_constraint
             (fresh_type_variable
                ("sequence_" ^ Names.sanitize_name operation ^ "_element")))
          params collection
    | FList [ FSymbol "re-matches"; expression; source ] -> (
        match infer_expected TRegex params expression with
        | Error _ as error -> error
        | Ok params -> infer_expected TString params source)
    | FList (FSymbol "list" :: values) -> (
        match infer_all params values with
        | Error _ as error -> error
        | Ok params ->
            let value_types = List.map (inferred_form_type params) values in
            let heterogeneous =
              match value_types with
              | [] | [ _ ] -> false
              | first :: rest ->
                  List.exists (fun ty -> not (Types.equal first ty)) rest
            in
            if not heterogeneous then Ok params
            else
              List.fold_left2
                (fun result value ty ->
                  match (result, value) with
                  | (Error _ as error), _ -> error
                  | Ok params, FSymbol name ->
                      constrain_symbol
                        (materialize_dynamic_unknown ty)
                        params name
                  | Ok params, _ -> Ok params)
                (Ok params) values value_types)
    | FList (FSymbol "prn" :: values) ->
        List.fold_left
          (fun result value ->
            Result.bind result (fun params ->
                match value with
                | FSymbol name when string_mem_assoc name params ->
                    constrain_printable_symbol params name
                | _ ->
                    infer_expected
                      (Types.printable_constraint (Type_solver.fresh ()))
                      params value))
          (Ok params) values
    | FList (FSymbol "apply" :: FSymbol ("pr" | "clojure.core/pr") :: arguments)
      -> (
        match List.rev arguments with
        | FSymbol collection :: _ ->
            constrain_seqable
              (Types.printable_constraint (Type_solver.fresh ()))
              params collection
        | _ -> infer_all params arguments)
    | FList
        (FSymbol "apply" :: FSymbol "mapv" :: FSymbol "vector"
        :: fixed_and_rest)
      when List.length fixed_and_rest >= 2 ->
        let reversed = List.rev fixed_and_rest in
        let rest_collection = List.hd reversed in
        let fixed_collections = List.rev (List.tl reversed) in
        let element_ty = fresh_type_variable "zip_element" in
        let infer_collection params = function
          | FSymbol name -> constrain_symbol (TVector element_ty) params name
          | form -> infer_form params form
        in
        Result.bind
          (List.fold_left
             (fun result collection ->
               Result.bind result (fun params ->
                   infer_collection params collection))
             (Ok params) fixed_collections)
          (fun params ->
            match rest_collection with
            | FSymbol name ->
                constrain_seqable (TVector element_ty) params name
            | form -> infer_form params form)
    | FList (FSymbol "apply" :: function_form :: arguments) ->
        let is_dynamic_function_type ty =
          Types.is_dynamic ty
          || match ty with TUnknown | TMeta _ | TVar _ -> true | _ -> false
        in
        let dynamic_function =
          match function_form with
          | FSymbol name -> (
              match string_assoc_opt name params with
              | Some ty -> is_dynamic_function_type ty
              | None -> (
                  match lookup_function_ty name with
                  | Ok ty -> is_dynamic_function_type ty
                  | Error _ -> false))
          | _ ->
              is_dynamic_function_type
                (inferred_form_type params function_form)
        in
        if dynamic_function then
          let params =
            match function_form with
            | FSymbol name -> (
                match string_assoc_opt name params with
                | Some (TUnknown | TMeta _ | TVar _) ->
                    constrain_symbol (Types.dynamic_constraint TUnknown) params
                      name
                | _ -> Ok params)
            | _ -> Ok params
          in
          Result.bind params (fun params ->
              match List.rev arguments with
              | collection :: reversed_fixed ->
                  let dynamic = Types.dynamic_constraint TUnknown in
                  Result.bind
                    (infer_expected_all dynamic params (List.rev reversed_fixed))
                    (fun params ->
                      match collection with
                      | FSymbol name -> constrain_seqable dynamic params name
                      | collection -> infer_form params collection)
              | [] -> Ok params)
        else (
          match List.rev arguments with
          | FSymbol collection :: reversed_fixed ->
              let fixed_count = List.length reversed_fixed in
              let fixed_arguments = List.rev reversed_fixed in
              let rec drop count values =
                if count <= 0 then values
                else
                  match values with
                  | [] -> []
                  | _ :: rest -> drop (count - 1) rest
              in
              let rec take count values =
                if count <= 0 then []
                else
                  match values with
                  | [] -> []
                  | value :: rest -> value :: take (count - 1) rest
              in
              let remaining_parameters = function
                | TFn (parameter_tys, _) -> drop fixed_count parameter_tys
                | TOverloaded_fn arities ->
                    arities
                    |> List.concat_map (fun arity ->
                           if fixed_count <= List.length arity.fixed_params then
                             drop fixed_count arity.fixed_params
                             @ Option.to_list arity.rest_param
                           else Option.to_list arity.rest_param)
                | _ -> []
              in
              let function_ty =
                match function_form with
                | FSymbol name -> (
                    match string_assoc_opt name params with
                    | Some ty -> ty
                    | None ->
                        lookup_function_ty name
                        |> Result.value ~default:TUnknown)
                | form -> inferred_form_type params form
              in
              let fixed_parameter_types =
                match function_ty with
                | TFn (parameter_tys, _)
                  when fixed_count <= List.length parameter_tys ->
                    take fixed_count parameter_tys
                | TOverloaded_fn arities ->
                    arities
                    |> List.find_map (fun arity ->
                           let declared_count =
                             List.length arity.fixed_params
                           in
                           if fixed_count <= declared_count then
                             Some (take fixed_count arity.fixed_params)
                           else
                             Option.map
                               (fun rest_ty ->
                                 arity.fixed_params
                                 @ List.init (fixed_count - declared_count)
                                     (fun _ -> rest_ty))
                               arity.rest_param)
                    |> Option.value
                         ~default:
                           (List.init fixed_count (fun _ -> TUnknown))
                | _ -> List.init fixed_count (fun _ -> TUnknown)
              in
              Result.bind
                (List.fold_left2
                   (fun result expected argument ->
                     Result.bind result (fun params ->
                         infer_expected expected params argument))
                   (Ok params) fixed_parameter_types fixed_arguments)
                (fun params ->
                  let element_ty =
                    match remaining_parameters function_ty with
                    | [] -> TUnknown
                    | first :: rest
                      when List.for_all (Types.equal first) rest ->
                        first
                    | _ -> Types.dynamic_constraint TUnknown
                  in
                  constrain_seqable element_ty params collection)
          | _ -> infer_all params arguments)
    | FList [ FSymbol ("map" | "mapv" | "keep"); fn; collection ] ->
        let inferred_element_ty = inferred_unary_function_param params fn in
        let inferred_element_ty =
          match inferred_element_ty with
          | TUnknown | TMeta _ | TVar _ ->
              inferred_literal_collection_item params collection
          | ty -> ty
        in
        let element_ty =
          match inferred_element_ty with
          | TUnknown | TMeta _ | TVar _ -> fresh_type_variable "unary_map_item"
          | ty -> ty
        in
        let infer_collection =
          match collection with
          | FList [ FKeyword keyword; FSymbol name ] ->
              add_record_field_constraint name keyword
                (Types.seqable_constraint element_ty)
                params
          | form -> infer_sequence_form element_ty params form
        in
        Result.bind infer_collection
          (fun params ->
            match fn with
            | FSymbol name ->
                constrain_symbol
                  (TFn
                     ( [ element_ty ],
                       fresh_type_variable "unary_map_result" ))
                  params name
            | form -> infer_form params form)
    | FList [ FSymbol "mapcat"; fn; collection ] ->
        let inferred_element_ty = inferred_unary_function_param params fn in
        let element_ty =
          match inferred_element_ty with
          | TUnknown | TMeta _ | TVar _ ->
              fresh_type_variable "mapcat_item"
          | ty -> ty
        in
        Result.bind (infer_sequence_form element_ty params collection)
          (fun params ->
            infer_expected
              (TFn
                 ( [ element_ty ],
                   Types.seqable_constraint
                     (fresh_type_variable "mapcat_result") ))
              params fn)
    | FList [ FSymbol "map-indexed"; fn; FSymbol collection ] ->
        let inferred_item_ty = inferred_map_indexed_item params fn in
        let item_ty =
          match inferred_item_ty with
          | TUnknown | TMeta _ | TVar _ -> fresh_type_variable "map_indexed_item"
          | ty -> ty
        in
        Result.bind (constrain_seqable item_ty params collection) (fun params ->
            match fn with
            | FSymbol name ->
                constrain_symbol
                  (TFn
                     ( [ TInt; item_ty ],
                       fresh_type_variable "map_indexed_result" ))
                  params name
            | form -> infer_form params form)
    | FList [ FSymbol "group-by"; fn; collection ] ->
        let inferred_element_ty = inferred_unary_function_param params fn in
        let element_ty =
          match inferred_element_ty with
          | TUnknown | TMeta _ | TVar _ -> fresh_type_variable "group_by_item"
          | ty -> ty
        in
        Result.bind (infer_sequence_form element_ty params collection)
          (fun params ->
            match fn with
            | FSymbol name ->
                constrain_symbol
                  (TFn
                     ( [ element_ty ],
                       fresh_type_variable "group_by_key" ))
                  params name
            | form -> infer_form params form)
    | FList [ FSymbol "filterv"; predicate; collection ] ->
        let inferred_element_ty =
          inferred_unary_function_param params predicate
        in
        let element_ty =
          match inferred_element_ty with
          | TUnknown | TMeta _ | TVar _ -> fresh_type_variable "filterv_item"
          | ty -> ty
        in
        Result.bind (infer_sequence_form element_ty params collection)
          (fun params ->
            infer_expected (TFn ([ element_ty ], TBool)) params predicate)
    | FList (FSymbol ("map" | "mapv") :: fn :: collection_forms)
      when List.length collection_forms >= 2 -> (
        let collection_element_ty collection =
          let collection_ty = inferred_form_type params collection in
          match Types.next_seq_element collection_ty with
          | Some element_ty -> element_ty
          | None -> (
              match Types.seqable_constraint_element collection_ty with
              | Some element_ty -> element_ty
              | None -> TUnknown)
        in
        let callback_tys = inferred_function_parameter_types params fn in
        let element_tys =
          collection_forms
          |> List.mapi (fun index collection ->
                 match collection_element_ty collection with
                 | TUnknown | TMeta _ | TVar _ ->
                     List.nth_opt callback_tys index
                     |> Option.value ~default:TUnknown
                 | element_ty -> element_ty)
        in
        let element_tys =
          match (fn, element_tys) with
          | FSymbol "vector", element_tys
            when List.for_all
                   (function TUnknown | TMeta _ | TVar _ -> true | _ -> false)
                   element_tys ->
              let element_ty = fresh_type_variable "vector_element" in
              List.map (fun _ -> element_ty) element_tys
          | _ -> element_tys
        in
        let function_ty = TFn (element_tys, TUnknown) in
        let rec constrain_collections params element_tys collections =
          match (element_tys, collections) with
          | [], [] -> Ok params
          | element_ty :: element_tys, collection :: collections ->
              Result.bind
                (infer_sequence_form element_ty params collection)
                (fun params ->
                  constrain_collections params element_tys collections)
          | _ -> assert false
        in
        Result.bind (infer_expected function_ty params fn) (fun params ->
            constrain_collections params element_tys collection_forms))
    | FList
        [
          FSymbol
            ("filter" | "remove" | "take-while" | "drop-while" | "some");
          fn;
          collection;
        ] ->
        let dynamic_predicate =
          match fn with
          | FSymbol name -> (
              match string_assoc_opt name params with
              | Some ty -> Types.is_dynamic ty
              | None -> (
                  match lookup_function_ty name with
                  | Ok (TFn ([ parameter_ty ], _)) ->
                      Types.is_dynamic parameter_ty
                  | Ok _ | Error _ -> false))
          | _ -> false
        in
        let element_ty =
          if dynamic_predicate then Types.dynamic_constraint TUnknown
          else inferred_unary_function_param params fn
        in
        let element_ty =
          if
            (not dynamic_predicate)
            && Types.is_dynamic element_ty
            && not (callback_compares_destructured_values fn)
            && not (callback_checks_runtime_type fn)
          then
            TUnknown
          else element_ty
        in
        Result.bind
          (infer_sequence_form element_ty params collection)
          (fun params ->
            match fn with
            | FSymbol name -> (
                match string_assoc_opt name params with
                | Some (TUnknown | TMeta _ | TVar _) ->
                    constrain_symbol
                      (TFn ([ element_ty ], TUnknown))
                      params name
                | Some _ | None -> infer_form params fn)
            | _ -> infer_expected (TFn ([ element_ty ], TUnknown)) params fn)
    | FList [ FSymbol "reduce"; reducer; init; collection ] -> (
        let declared_accumulator_ty, declared_element_ty =
          match reducer with
          | FSymbol name -> (
              let reducer_ty =
                match string_assoc_opt name params with
                | Some ty -> Ok ty
                | None -> lookup_function_ty name
              in
              match reducer_ty with
              | Ok (TFn ([ accumulator_ty; element_ty ], _)) ->
                  (accumulator_ty, element_ty)
              | Ok (TOverloaded_fn arities) -> (
                  match
                    List.find_opt
                      (fun (arity : fn_arity) ->
                        Option.is_none arity.rest_param
                        && List.length arity.fixed_params = 2)
                      arities
                  with
                  | Some { fixed_params = [ accumulator_ty; element_ty ]; _ }
                    ->
                      (accumulator_ty, element_ty)
                  | Some _ | None -> (TUnknown, TUnknown))
              | Ok _ | Error _ -> (TUnknown, TUnknown))
          | _ -> (TUnknown, TUnknown)
        in
        let accumulator_ty =
          match declared_accumulator_ty with
          | TUnknown | TMeta _ | TVar _ -> (
              match init with
              | FMap [] | FList [ FSymbol "hash-map" ] ->
                  Types.dynamic_map (Type_solver.fresh ())
                    (Type_solver.fresh ())
              | _ ->
                  returned_vector_type params init
                  |> Option.value ~default:(inferred_form_type params init))
          | ty -> ty
        in
        let inferred_accumulator_ty, inferred_element_ty =
          inferred_reducer_types params accumulator_ty reducer
        in
        let accumulator_ty =
          refine_type accumulator_ty inferred_accumulator_ty
        in
        Result.bind (infer_expected accumulator_ty params init) (fun params ->
            let element_ty =
              match (declared_element_ty, collection) with
              | ( (TUnknown | TMeta _ | TVar _),
                  FList
                    [
                      FSymbol "partition-by";
                      function_form;
                      _source_collection;
                    ] ) ->
                  let partition_item_ty =
                    inferred_unary_function_param params function_form
                  in
                  let reducer_item_ty =
                    Types.seqable_constraint_element inferred_element_ty
                    |> Option.value ~default:TUnknown
                  in
                  TList (refine_type partition_item_ty reducer_item_ty)
              | (TUnknown | TMeta _ | TVar _), _ -> (
                  match collection with
                  | FSymbol collection -> (
                      match string_assoc_opt collection params with
                      | Some collection_ty -> (
                          match
                            Types.seqable_constraint_element collection_ty
                          with
                          | Some (TUnknown | TMeta _ | TVar _) | None ->
                              inferred_element_ty
                          | Some element_ty -> element_ty)
                      | None -> inferred_element_ty)
                  | _ -> inferred_element_ty)
              | ty, _ -> ty
            in
            Result.bind (infer_sequence_form element_ty params collection)
              (fun params ->
                infer_expected
                  (TFn ([ accumulator_ty; element_ty ], TUnknown))
                  params reducer)))
    | FList [ FSymbol "reduce"; reducer; collection ] ->
        let declared_element_ty =
          match reducer with
          | FSymbol name -> (
              let reducer_ty =
                match string_assoc_opt name params with
                | Some ty -> Ok ty
                | None -> lookup_function_ty name
              in
              match reducer_ty with
              | Ok (TFn ([ _accumulator_ty; element_ty ], _)) -> element_ty
              | Ok (TOverloaded_fn arities) -> (
                  match
                    List.find_opt
                      (fun (arity : fn_arity) ->
                        Option.is_none arity.rest_param
                        && List.length arity.fixed_params = 2)
                      arities
                  with
                  | Some { fixed_params = [ _accumulator_ty; element_ty ]; _ }
                    ->
                      element_ty
                  | Some _ | None -> TUnknown)
              | Ok _ | Error _ -> TUnknown)
          | _ -> TUnknown
        in
        infer_sequence_form declared_element_ty params collection
    | FList [ FSymbol "sort"; FSymbol collection ] ->
        constrain_seqable (Types.dynamic_constraint TUnknown) params collection
    | FList [ FSymbol "sort"; _comparator; FSymbol collection ] ->
        constrain_seqable (Types.dynamic_constraint TUnknown) params collection
    | FList [ FSymbol "repeatedly"; FSymbol count; function_form ] ->
        Result.bind (infer_expected TInt params (FSymbol count)) (fun params ->
            infer_form params function_form)
    | FList
        [ FSymbol ("take" | "drop"); FSymbol count; collection_form ] ->
        Result.bind (infer_expected TInt params (FSymbol count)) (fun params ->
            infer_form params collection_form)
    | FList (FSymbol ("+" | "-" | "*" | "/" | "max" | "min") :: args) ->
        let expected_ty =
          if
            List.exists
              (fun arg -> Types.equal (numeric_form_type params arg) TFloat)
              args
          then TFloat
          else TInt
        in
        infer_expected_all expected_ty params args
    | FList
        [
          FSymbol ("zero?" | "pos?" | "neg?");
          arg;
        ] ->
        let arg_ty =
          match inferred_form_type params arg with
          | TUnknown ->
              inferred_call_return_type ~lookup_function_ty params arg
          | ty -> ty
        in
        infer_expected
          (if Types.equal arg_ty (TOcaml "int") then TOcaml "int" else TInt)
          params arg
    | FList [ FSymbol "double"; arg ] ->
        let expected_ty =
          if Types.equal (inferred_form_type params arg) TFloat then TFloat
          else TInt
        in
        infer_expected expected_ty params arg
    | FList (FSymbol ("==" | "<" | "<=" | ">" | ">=") :: args) ->
        let expected_ty =
          if
            List.exists
              (fun arg -> Types.equal (numeric_form_type params arg) TFloat)
              args
          then TFloat
          else TInt
        in
        infer_expected_all expected_ty params args
    | FList (FSymbol "=" :: args) ->
        let expected_ty =
          let concrete =
            args
            |> List.filter_map (fun arg ->
                   match inferred_form_type params arg with
                   | TUnknown | TMeta _ | TVar _ -> None
                   | ty when Types.is_dynamic ty -> None
                   | ty -> Some ty)
            |> List.fold_left
                 (fun unique ty ->
                   if List.exists (Types.equal ty) unique then unique
                   else ty :: unique)
                 []
          in
          let has_dynamic_argument =
            List.exists
              (fun arg -> Types.is_dynamic (inferred_form_type params arg))
              args
          in
          match concrete with
          | [ ty ] -> ty
          | _ :: _ :: _
            when List.exists
                   (function
                     | FSymbol name -> string_mem_assoc name params
                     | _ -> false)
                   args ->
              Types.dynamic_constraint TUnknown
          | ty :: _ -> ty
          | [] when has_dynamic_argument ->
              Types.dynamic_constraint TUnknown
          | [] ->
              args
              |> List.find_map (fun arg ->
                     match inferred_form_type params arg with
                     | (TMeta _ | TVar _) as ty -> Some ty
                     | _ -> None)
              |> Option.value ~default:(fresh_type_variable "equality")
        in
        if Types.equal expected_ty TSymbol then
          List.fold_left
            (fun result arg ->
              Result.bind result (fun params ->
                  match arg with
                  | FSymbol name -> (
                      match string_assoc_opt name params with
                      | Some (TUnknown | TMeta _ | TVar _) ->
                          constrain_symbol_predicate params name
                      | Some _ | None -> infer_expected TSymbol params arg)
                  | _ -> infer_expected TSymbol params arg))
            (Ok params) args
        else infer_expected_all expected_ty params args
    | FList
        [
          FSymbol "select-keys";
          FList [ FKeyword keyword; FSymbol record ];
          FSymbol keys;
        ] ->
        let key_ty = fresh_type_variable "select_keys_key" in
        let value_ty = fresh_type_variable "select_keys_value" in
        Result.bind (constrain_seqable key_ty params keys) (fun params ->
            add_record_field_constraint record keyword
              (Types.dynamic_map key_ty value_ty)
              params)
    | FList [ FSymbol "select-keys"; FSymbol target; FSymbol keys ] ->
        let key_ty = fresh_type_variable "select_keys_key" in
        let value_ty = fresh_type_variable "select_keys_value" in
        Result.bind (constrain_seqable key_ty params keys) (fun params ->
            constrain_symbol (Types.dynamic_map key_ty value_ty) params target)
    | FList
        [ FKeyword nested_keyword; FList [ FKeyword keyword; FSymbol name ] ] ->
        add_record_field_constraint name keyword
          (TRecord
             [ make_field nested_keyword (Types.dynamic_constraint TUnknown) ])
          params
    | FList
        [
          FKeyword keyword;
          FList
            [ FSymbol "first"; collection ];
        ] ->
        infer_sequence_form
          (TRecord
             [
               make_field keyword
                 (fresh_type_variable
                    ("projected_" ^ Names.sanitize_name keyword));
             ])
          params collection
    | FList [ FKeyword keyword; FSymbol name ] ->
        let field_ty =
          match string_assoc_opt name params with
          | Some ty
            when Option.is_some (Types.contains_constraint_info ty) ->
              TNullable
                (fresh_type_variable
                   ("field_" ^ Names.sanitize_name keyword))
          | Some _ | None ->
              fresh_type_variable
                ("field_" ^ Names.sanitize_name keyword)
        in
        add_record_field_constraint name keyword field_ty params
    | FList [ FKeyword keyword; FSymbol name; default ] ->
        let field_ty = inferred_form_type params default in
        Result.bind (add_record_field_constraint name keyword field_ty params)
          (fun params -> infer_expected field_ty params default)
    | FList
        [
          FSymbol "contains?";
          FList [ FKeyword keyword; FSymbol name ];
          key;
        ] ->
        let dynamic = Types.dynamic_constraint TUnknown in
        Result.bind
          (add_record_field_constraint name keyword
             (Types.dynamic_map dynamic dynamic)
             params)
          (fun params -> infer_expected dynamic params key)
    | FList [ FSymbol "contains?"; FSymbol name; (FKeyword _ as key) ] -> (
        match constrain_contains TKeyword params name with
        | Error _ as err -> err
        | Ok params -> infer_expected TKeyword params key)
    | FList [ FSymbol "contains?"; FSymbol name; key ] -> (
        let target_ty =
          match string_assoc_opt name params with
          | Some ty -> ty
          | None -> (
              match lookup_function_ty name with
              | Ok (TRef value_ty) -> value_ty
              | Ok ty -> ty
              | Error _ -> TUnknown)
        in
        let collection_ty, key_ty =
          match target_ty with
          | TSet element_ty -> (TSet element_ty, element_ty)
          | TVector element_ty -> (TVector element_ty, TInt)
          | collection_ty -> (
              match Types.dynamic_map_types collection_ty with
              | Some (key_ty, _) -> (collection_ty, key_ty)
              | None ->
                  let key_ty =
                    match inferred_form_type params key with
                    | TUnknown | TMeta _ | TVar _ ->
                        fresh_type_variable "contains_key"
                    | key_ty -> key_ty
                  in
                  (Types.contains_constraint key_ty, key_ty))
        in
        let infer_collection =
          match Types.contains_constraint_info collection_ty with
          | Some _ -> constrain_contains key_ty params name
          | None -> infer_expected collection_ty params (FSymbol name)
        in
        match infer_collection with
        | Error _ as error -> error
        | Ok params -> infer_expected key_ty params key)
    | FList [ FSymbol "contains?"; target; key ] ->
        let target_ty =
          match target with
          | FList _ ->
              inferred_call_return_type ~lookup_function_ty params target
          | _ -> inferred_form_type params target
        in
        let concrete_key_ty =
          match target_ty with
          | TSet element_ty -> Some element_ty
          | TVector _ -> Some TInt
          | TMap_keys -> Some TKeyword
          | target_ty -> (
              match Types.contains_constraint_info target_ty with
              | Some (key_ty, _) -> Some key_ty
              | None -> Option.map fst (Types.dynamic_map_types target_ty))
        in
        let key_ty =
          match concrete_key_ty with
          | Some key_ty -> key_ty
          | None -> (
              match inferred_form_type params key with
              | TUnknown | TMeta _ | TVar _ ->
                  fresh_type_variable "contains_key"
              | key_ty -> key_ty)
        in
        let infer_target =
          match concrete_key_ty with
          | Some _ -> infer_form params target
          | None ->
              infer_expected (Types.contains_constraint key_ty) params target
        in
        Result.bind infer_target (fun params -> infer_expected key_ty params key)
    | FList
        [ FSymbol ("get-in" | "clojure.core/get-in"); target; FVector keys ] ->
        infer_form params (Core_form_expansion.get_in target keys None)
    | FList
        [
          FSymbol ("get-in" | "clojure.core/get-in");
          target;
          FVector keys;
          default;
        ] ->
        infer_form params
          (Core_form_expansion.get_in target keys (Some default))
    | FList
        [
          FSymbol ("assoc-in" | "clojure.core/assoc-in");
          target;
          FVector keys;
          value;
        ] ->
        infer_assoc_in params target keys value
    | FList
        (FSymbol ("update-in" | "clojure.core/update-in")
        :: target :: FVector (_ :: _ as keys) :: function_form
        :: argument_forms) ->
        infer_form params
          (Core_form_expansion.update_in target keys function_form argument_forms)
    | FList
        (FSymbol ("dissoc" | "clojure.core/dissoc" | "cljs.core/dissoc")
        :: target :: keys) ->
        (match
           Types.dynamic_map_types (inferred_form_type params target)
         with
        | Some (key_ty, _) ->
            Result.bind (infer_form params target) (fun params ->
                infer_expected_all key_ty params keys)
        | None -> infer_all params (target :: keys))
    | FList (FSymbol ("assoc" | "clojure.core/assoc") :: target :: pairs) ->
        infer_assoc params target pairs
    | FList (FSymbol "subvec" :: collection :: indexes)
      when List.length indexes = 1 || List.length indexes = 2 ->
        let element_ty =
          match inferred_form_type params collection with
          | TVector element_ty -> element_ty
          | _ -> Type_solver.fresh ()
        in
        Result.bind
          (infer_expected (TVector element_ty) params collection)
          (fun params -> infer_expected_all TInt params indexes)
    | FList [ FSymbol ("transient" | "persistent!"); collection ] ->
        infer_expected (Types.dynamic_constraint TUnknown) params collection
    | FList (FSymbol "conj" :: target :: values) -> (
        let inferred_value_type value =
          match inferred_form_type params value with
          | (TUnknown | TMeta _ | TVar _) as unresolved -> (
              match value with
              | FList (FSymbol name :: arguments) -> (
                  match lookup_function_ty name with
                  | Ok (TFn (parameters, return_ty))
                    when List.length parameters = List.length arguments ->
                      return_ty
                  | Ok (TOverloaded_fn arities) -> (
                      match select_fn_arity arities (List.length arguments) with
                      | Some arity -> arity.return_ty
                      | None -> unresolved)
                  | Ok _ | Error _ -> unresolved)
              | _ -> unresolved)
          | ty -> ty
        in
        let element_ty =
          values
          |> List.find_map (fun value ->
                 match inferred_value_type value with
                 | TUnknown -> None
                 | ty -> Some (stored_value_type ty))
          |> Option.value ~default:TUnknown
        in
        let collection_ty =
          match inferred_form_type params target with
          | TList _ -> TList element_ty
          | TSeq _ -> TSeq element_ty
          | TOcaml_app (name, [ _ ]) when name = Types.next_seq_type_name ->
              TSeq element_ty
          | TSet _ -> (
              match Types.set_module_name element_ty with
              | Ok _ -> TSet element_ty
              | Error _ -> Types.dynamic_constraint (TSet TUnknown))
          | TFn ([ predicate_arg ], TBool) ->
              TSet (refine_type element_ty predicate_arg)
          | TVector _ -> TVector element_ty
          | TNil -> TList element_ty
          | TUnknown | TMeta _ | TVar _ -> TVector element_ty
          | _ -> Types.dynamic_constraint TUnknown
        in
        match infer_expected collection_ty params target with
        | Error _ as error -> error
        | Ok params ->
            let value_ty =
              if Types.is_dynamic collection_ty then
                Types.dynamic_constraint TUnknown
              else element_ty
            in
            infer_expected_all value_ty params values)
    | FList [ FSymbol "reduce-kv"; reducer; init; FSymbol name ] -> (
        let key_ty, value_ty = inferred_kv_reducer_types params init reducer in
        let unresolved = function
          | TUnknown | TMeta _ | TVar _ -> fresh_type_variable "map"
          | ty -> ty
        in
        match
          infer_expected
            (Types.dynamic_map
               (unresolved key_ty)
               (unresolved value_ty))
            params (FSymbol name)
        with
        | Error _ as error -> error
        | Ok params -> infer_all params [ reducer; init ])
    | FList [ FSymbol "reduce-kv"; reducer; init; collection ]
      when
        (match inferred_form_type params collection with
        | TUnknown | TMeta _ | TVar _ -> true
        | _ -> false) -> (
        let key_ty, value_ty = inferred_kv_reducer_types params init reducer in
        let unresolved = function
          | TUnknown | TMeta _ | TVar _ -> fresh_type_variable "map"
          | ty -> ty
        in
        match
          infer_expected
            (Types.dynamic_map
               (unresolved key_ty)
               (unresolved value_ty))
            params collection
        with
        | Error _ as error -> error
        | Ok params -> infer_all params [ reducer; init ])
    | FList (FSymbol "str" :: args) ->
        List.fold_left
          (fun result arg ->
            Result.bind result (fun params ->
                match arg with
                | FSymbol name when string_mem_assoc name params ->
                    constrain_printable_symbol params name
                | _ ->
                    infer_expected
                      (Types.printable_constraint (Type_solver.fresh ()))
                      params arg))
          (Ok params) args
    | FList [ FSymbol "ex-info"; message; data ] ->
        Result.bind (infer_expected TString params message) (fun params ->
            infer_expected (Types.dynamic_constraint TUnknown) params data)
    | FList [ FSymbol "if"; condition; then_form; else_form ] -> (
        match infer_truthy params condition with
        | Error _ as err -> err
        | Ok params ->
            let previous_hints = !branch_hint_symbols in
            (
            match with_branch (fun () -> infer_form params then_form) with
            | Error _ as err -> err
            | Ok inferred ->
                Result.map
                  (fun inferred ->
                    let inferred =
                      restore_branch_evidence params inferred previous_hints
                        condition
                    in
                    let then_ty =
                      returned_vector_type inferred then_form
                      |> Option.value
                           ~default:(inferred_form_type inferred then_form)
                    in
                    let else_ty =
                      returned_vector_type inferred else_form
                      |> Option.value
                           ~default:(inferred_form_type inferred else_form)
                    in
                    inferred
                    |> fun params ->
                    refine_returned_seqable_vector params then_form else_ty
                    |> fun params ->
                    refine_returned_seqable_vector params else_form then_ty)
                  (with_branch (fun () -> infer_form inferred else_form))))
    | FList [ FSymbol "if"; condition; then_form ] -> (
        match infer_truthy params condition with
        | Error _ as err -> err
        | Ok params ->
            let previous_hints = !branch_hint_symbols in
            Result.map
              (fun inferred ->
                restore_branch_evidence params inferred previous_hints
                  condition)
              (with_branch (fun () -> infer_form params then_form)))
    | FList [ FSymbol "if-not"; condition; then_form; else_form ] -> (
        match infer_truthy params condition with
        | Error _ as err -> err
        | Ok params ->
            let previous_hints = !branch_hint_symbols in
            (
            match with_branch (fun () -> infer_form params then_form) with
            | Error _ as err -> err
            | Ok inferred ->
                Result.map
                  (fun inferred ->
                    restore_branch_evidence params inferred previous_hints
                      condition)
                  (with_branch (fun () -> infer_form inferred else_form))))
    | FList (FSymbol "when" :: condition :: body_forms) -> (
        match infer_truthy params condition with
        | Error _ as err -> err
        | Ok params ->
            let previous_hints = !branch_hint_symbols in
            (match condition with
            | FSymbol name when not (string_mem name !branch_hint_symbols) ->
                branch_hint_symbols := name :: !branch_hint_symbols
            | _ -> ());
            Result.map
              (fun inferred ->
                restore_branch_evidence params inferred previous_hints
                  condition)
              (with_branch (fun () -> infer_all params body_forms)))
    | FList (FSymbol "cond" :: clauses) ->
        let rec infer_clauses params = function
          | [] -> Ok params
          | [ form ] -> infer_form params form
          | FKeyword ":else" :: value_form :: rest -> (
              match infer_form params value_form with
              | Error _ as err -> err
              | Ok params -> infer_clauses params rest)
          | FBool true :: value_form :: _ -> infer_form params value_form
          | test_form :: value_form :: rest -> (
              match infer_truthy params test_form with
              | Error _ as err -> err
              | Ok params -> (
                  match infer_form params value_form with
                  | Error _ as err -> err
                  | Ok params -> infer_clauses params rest))
        in
        infer_clauses params clauses
    | FList (FSymbol "try" :: forms) ->
        let is_catch = function
          | FList (FSymbol "catch" :: _) -> true
          | _ -> false
        in
        let body_forms, catch_forms =
          List.partition (fun form -> not (is_catch form)) forms
        in
        let catch_bodies =
          catch_forms
          |> List.filter_map (function
               | FList (FSymbol "catch" :: _pattern :: body_forms) ->
                   Some body_forms
               | _ -> None)
        in
        let all_forms = body_forms @ List.concat catch_bodies in
        Result.bind (infer_all params all_forms) (fun params ->
            let rec last = function
              | [] -> None
              | [ form ] -> Some form
              | _ :: rest -> last rest
            in
            let result_forms =
              List.filter_map last (body_forms :: catch_bodies)
            in
            let result_types =
              List.map (inferred_form_type params) result_forms
            in
            let has_unknown =
              List.exists
                (function TUnknown | TMeta _ | TVar _ -> true | _ -> false)
                result_types
            in
            let concrete_types =
              result_types
              |> List.filter (fun ty ->
                     not (Types.equal ty TUnknown)
                     && match ty with TMeta _ | TVar _ -> false | _ -> true)
              |> List.fold_left
                   (fun unique ty ->
                     if List.exists (Types.equal ty) unique then unique
                     else ty :: unique)
                   []
            in
            if
              List.length concrete_types > 1
              || (has_unknown && concrete_types <> [])
              || List.exists Types.is_dynamic concrete_types
            then
              infer_expected_all
                (Types.dynamic_constraint TUnknown)
                params result_forms
            else Ok params)
    | FList (FSymbol "match" :: target :: clauses) ->
        infer_match params target clauses
    | FList (FSymbol "case" :: target :: clauses) ->
        let rec grouped_pattern = function
          | [] -> FSymbol "_"
          | [ pattern ] -> pattern
          | pattern :: rest ->
              FList [ FSymbol "or"; pattern; grouped_pattern rest ]
        in
        let pattern = function
          | FList patterns -> grouped_pattern patterns
          | pattern -> pattern
        in
        let rec pairs acc = function
          | [] -> List.rev (FSymbol "nil" :: FSymbol "_" :: acc)
          | [ default ] -> List.rev (default :: FSymbol "_" :: acc)
          | constant :: result :: rest ->
              pairs (result :: pattern constant :: acc) rest
        in
        infer_match params target (pairs [] clauses)
    | FList
        [
          FSymbol "partition-by";
          FKeyword keyword;
          FSymbol collection;
        ] ->
        constrain_seqable
          (TRecord
             [ make_field keyword (Types.dynamic_constraint TUnknown) ])
          params collection
    | FList
        [
          FSymbol "partition-by";
          (FList (FSymbol "fn" :: _) as function_form);
          FSymbol collection;
        ] ->
        constrain_seqable
          (inferred_unary_function_param params function_form)
          params collection
    | FList [ FSymbol ("set" | "dorun" | "doall"); collection ] ->
        infer_collection params collection
    | FList
        [
          FSymbol
            "take-nth";
          count;
          collection;
        ] -> (
        match infer_expected TInt params count with
        | Error _ as err -> err
        | Ok params -> infer_collection params collection)
    | FList
        [
          FSymbol "run!";
          (FList (FSymbol "fn" :: _fn_params :: _body_forms) as function_form);
          collection;
        ] ->
        let element_ty = inferred_unary_function_param params function_form in
        Result.bind (infer_sequence_form element_ty params collection)
          (fun params -> infer_form params function_form)
    | FList
        [
          FSymbol "pr-sequential-writer";
          writer;
          printer;
          prefix;
          separator;
          suffix;
          opts;
          collection;
        ] ->
        Result.bind (infer_expected (TOcaml "Buffer.t") params writer)
          (fun params ->
            Result.bind (infer_form params printer) (fun params ->
                Result.bind (infer_expected TString params prefix) (fun params ->
                    Result.bind
                      (infer_expected TString params separator)
                      (fun params ->
                        Result.bind
                          (infer_expected TString params suffix)
                          (fun params ->
                            Result.bind (infer_form params opts) (fun params ->
                                match collection with
                                | FSymbol name ->
                                    constrain_seqable TUnknown params name
                                | _ -> infer_collection params collection))))))
    | FList (FSymbol ("doseq" | "for") :: bindings :: body_forms) ->
        infer_generator_bindings params bindings body_forms
    | FList (FSymbol (("cond->" | "cond->>") as operator) :: value :: clauses)
      ->
        let thread step =
          match step with
          | FSymbol name -> FList [ FSymbol name; value ]
          | FKeyword _ as keyword -> FList [ keyword; value ]
          | FList (FSymbol name :: arguments) ->
              if operator = "cond->" then
                FList (FSymbol name :: value :: arguments)
              else FList ((FSymbol name :: arguments) @ [ value ])
          | form -> form
        in
        let rec infer_clauses params = function
          | [] -> infer_form params value
          | condition :: step :: rest ->
              Result.bind (infer_truthy params condition) (fun params ->
                  Result.bind
                    (infer_form params (thread step))
                    (fun params -> infer_clauses params rest))
          | [ form ] -> infer_form params form
        in
        infer_clauses params clauses
    | FList (FSymbol "do" :: body_forms) -> infer_all params body_forms
    | FList (FSymbol "loop" :: FVector bindings :: body_forms) -> (
        let rec pairs acc = function
          | [] -> Some (List.rev acc)
          | FSymbol local :: value :: rest -> pairs ((local, value) :: acc) rest
          | _ -> None
        in
        match pairs [] bindings with
        | None -> infer_all params body_forms
        | Some bindings ->
            Result.bind
              (infer_all params (List.map snd bindings))
              (fun params ->
            let initializer_type value =
              match inferred_form_type params value with
              | TUnknown ->
                  inferred_call_return_type
                    ~lookup_function_ty:lookup_loop_initializer_type params
                    value
              | ty -> ty
            in
            let local_params =
              bindings
              |> List.map (fun (local, value) ->
                     let ty = initializer_type value in
                     let ty =
                       if Types.equal ty TUnknown then
                         fresh_type_variable
                           ("loop_" ^ Names.sanitize_name local)
                       else ty
                     in
                     (local, ty))
            in
            let equality_first_locals =
              let rec collect locals = function
                | FList (FSymbol "=" :: operands) ->
                    List.fold_left
                      (fun locals -> function
                        | FList
                            [
                              FSymbol "first";
                              FSymbol local;
                            ] ->
                            if string_mem local locals then locals
                            else local :: locals
                        | _ -> locals)
                      locals operands
                | FList (FSymbol ("fn" | "loop") :: _) -> locals
                | FList forms | FVector forms ->
                    List.fold_left collect locals forms
                | FMap pairs ->
                    List.fold_left
                      (fun locals (key, value) ->
                        collect (collect locals key) value)
                      locals pairs
                | _ -> locals
              in
              if materialize_open_equality then
                List.fold_left collect [] body_forms
              else []
            in
            let local_params =
              List.map
                (fun (local, ty) ->
                  if string_mem local equality_first_locals then
                    ( local,
                      Types.optional_seqable_constraint
                        (Types.dynamic_constraint TUnknown) ty )
                  else (local, ty))
                local_params
            in
                match infer_all (local_params @ params) body_forms with
            | Error _ as error -> error
                | Ok inferred -> (
                let rec recur_arguments = function
                  | FList (FSymbol "recur" :: args) -> [ args ]
                  | FList (FSymbol ("loop" | "fn") :: _) -> []
                  | FList forms | FVector forms ->
                      List.concat_map recur_arguments forms
                  | FMap pairs ->
                      pairs
                      |> List.concat_map (fun (key, value) ->
                             recur_arguments key @ recur_arguments value)
                  | _ -> []
                in
                let constrain_recur params args =
                  match (local_params, args) with
                      | locals, args when List.length locals = List.length args
                        ->
                      List.fold_left2
                        (fun result (local, _) arg ->
                          Result.bind result (fun params ->
                              let expected =
                                string_assoc_opt local params
                                |> Option.value ~default:TUnknown
                              in
                              Result.bind
                                (infer_expected expected params arg)
                                (fun params ->
                                  let actual =
                                    match arg with
                                    | FList (FSymbol name :: args) -> (
                                        match lookup_function_ty name with
                                        | Ok (TFn (param_tys, return_ty))
                                          when List.length param_tys
                                               = List.length args ->
                                            Types.instantiate_type
                                              ~templates:param_tys
                                              ~actuals:
                                                (List.map
                                                       (inferred_form_type
                                                          params)
                                                   args)
                                              return_ty
                                            | _ -> inferred_form_type params arg
                                            )
                                    | arg -> inferred_form_type params arg
                                  in
                                  constrain_symbol actual params local)))
                        (Ok params) locals args
                  | _ -> Ok params
                in
                let inferred =
                  body_forms
                  |> List.concat_map recur_arguments
                  |> List.fold_left
                       (fun result args ->
                         Result.bind result (fun params ->
                             constrain_recur params args))
                       (Ok inferred)
                in
                    match inferred with
                | Error _ as error -> error
                | Ok inferred ->
                let original_names = List.map fst params in
                let originals =
                  original_names
                  |> List.map (fun name ->
                         ( name,
                           string_assoc_opt name inferred
                           |> Option.value
                                ~default:
                                  (string_assoc_opt name params
                                  |> Option.value ~default:TUnknown) ))
                in
                bindings
                |> List.fold_left
                     (fun result (local, value) ->
                       match (result, value) with
                       | (Error _ as error), _ -> error
                       | ( Ok params,
                           FList
                             [
                               FSymbol field_access;
                               FSymbol receiver;
                             ] )
                         when String.starts_with ~prefix:".-" field_access
                              && string_mem_assoc receiver params ->
                           let expected =
                             string_assoc_opt local inferred
                             |> Option.value ~default:TUnknown
                           in
                           let keyword =
                             ":"
                             ^ String.sub field_access 2
                                 (String.length field_access - 2)
                           in
                           (match expected with
                           | TUnknown | TMeta _ | TVar _ -> Ok params
                           | _ ->
                               add_record_field_constraint receiver keyword
                                 expected params)
                       | Ok params, FSymbol source
                         when string_mem_assoc source params ->
                           let local_ty =
                             string_assoc_opt local inferred
                             |> Option.value ~default:TUnknown
                           in
                           constrain_symbol local_ty params source
                       | ( Ok params,
                           FList
                             [
                               FSymbol ("seq" | "rest" | "next");
                               FSymbol source;
                             ] )
                         when string_mem_assoc source params ->
                           let local_ty =
                             string_assoc_opt local inferred
                             |> Option.value ~default:TUnknown
                           in
                           let element_ty =
                             match local_ty with
                             | ty when Types.is_dynamic ty -> Some ty
                             | TSeq inner | TList inner | TVector inner
                             | TArray inner | TSet inner ->
                                 Some inner
                             | TOcaml_app (("Seq.t" | "Seq"), [ inner ]) ->
                                 Some inner
                             | ty -> Types.seqable_constraint_element ty
                           in
                           Option.fold ~none:(Ok params)
                             ~some:(fun element_ty ->
                               constrain_seqable element_ty params source)
                             element_ty
                     | Ok params, _ -> Ok params)
                     (Ok originals))))
    | FList (FSymbol let_name :: bindings :: body_forms)
      when let_name = "let" || let_name = "let*"
           || String.ends_with ~suffix:"/let" let_name
           || String.ends_with ~suffix:"/let*" let_name ->
        let bindings =
          match bindings with
          | FVector forms ->
              FVector (Destructure.normalize_binding_type_hints forms)
          | bindings -> bindings
        in
        Result.bind (infer_let params bindings body_forms) (fun params ->
            let rec parse_aliases aliases = function
              | [] -> aliases
              | FSymbol name :: value :: rest ->
                  parse_aliases ((name, value) :: aliases) rest
              | _ :: _ :: rest -> parse_aliases aliases rest
              | [ _ ] -> aliases
            in
            let aliases =
              match bindings with
              | FVector forms -> parse_aliases [] forms
              | _ -> []
            in
            let body_aliases =
              List.filter
                (fun (name, _) ->
                  match
                    rewrite_simple_aliases aliases (FSymbol name)
                  with
                  | FSymbol source -> not (string_mem_assoc source params)
                  | FKeyword _ -> true
                  | _ -> false)
                aliases
            in
            let rewritten_body_forms =
              List.map (rewrite_simple_aliases body_aliases) body_forms
            in
            let rec infer_alias_constraints params = function
              | FList
                  (FSymbol apply_name :: FSymbol function_name :: arguments)
                when apply_name = "apply"
                     || String.ends_with ~suffix:"/apply" apply_name -> (
                  match string_assoc_opt function_name aliases with
                  | Some function_form ->
                      infer_form params
                        (FList
                           (FSymbol apply_name :: function_form :: arguments))
                  | None -> Ok params)
              | FList [ FSymbol reduce_name; reducer; init; FSymbol collection ]
                when reduce_name = "reduce"
                     || String.ends_with ~suffix:"/reduce" reduce_name -> (
                  match string_assoc_opt collection aliases with
                  | Some value ->
                      infer_form params
                        (FList [ FSymbol reduce_name; reducer; init; value ])
                  | None -> Ok params)
              | FList forms | FVector forms ->
                  List.fold_left
                    (fun result form ->
                      Result.bind result (fun params ->
                          infer_alias_constraints params form))
                    (Ok params) forms
              | FMap pairs ->
                  List.fold_left
                    (fun result (key, value) ->
                      Result.bind result (fun params ->
                          Result.bind (infer_alias_constraints params key)
                            (fun params -> infer_alias_constraints params value)))
                    (Ok params) pairs
              | _ -> Ok params
            in
            let binding_values =
              match bindings with
              | FVector forms ->
                  forms
                  |> List.mapi (fun index form -> (index, form))
                  |> List.filter_map (fun (index, form) ->
                      if index mod 2 = 1 then Some form else None)
              | _ -> []
            in
            Result.bind
              (if rewritten_body_forms = body_forms then Ok params
               else infer_all params rewritten_body_forms)
              (fun params ->
                List.fold_left
                  (fun result form ->
                    Result.bind result (fun params ->
                        infer_alias_constraints params form))
                  (Ok params)
                  (binding_values @ rewritten_body_forms)))
    | FList
        (FSymbol "fn" :: FSymbol _function_name
        :: (FVector _ as fn_params) :: body_forms) ->
        infer_form params (FList (FSymbol "fn" :: fn_params :: body_forms))
    | FList (FSymbol "fn" :: (FVector _ as fn_params) :: body_forms) -> (
        match Destructure.parse_param_specs fn_params with
        | Error _ -> infer_all params body_forms
        | Ok specs ->
            let local_bindings =
              specs
              |> List.concat_map (fun (spec : Destructure.param_spec) ->
                     let source_ty =
                       Option.value spec.explicit_ty ~default:TUnknown
                       |> resolve_named_record
                     in
                     let destructured =
                       if spec.destructured then
                         Destructure.pattern_names spec.pattern
                         |> List.map (fun name -> (name, TUnknown))
                       else []
                     in
                     (spec.source_name, source_ty) :: destructured)
            in
            let local_names = List.map fst local_bindings in
            let shadowed =
              params
              |> List.filter (fun (name, _) -> string_mem name local_names)
            in
            let local_params =
              local_bindings
              @ List.filter
                  (fun (name, _) -> not (string_mem name local_names))
                  params
            in
            let rec infer_local remaining local_params =
              Result.bind (infer_all local_params body_forms) (fun inferred ->
                  if remaining = 0 then Ok inferred
                  else
                    let stable =
                      List.length local_params = List.length inferred
                      && List.for_all2
                           (fun (left_name, left_ty) (right_name, right_ty) ->
                             left_name = right_name
                             && Types.equal left_ty right_ty)
                           local_params inferred
                    in
                    if stable then Ok inferred
                    else infer_local (remaining - 1) inferred)
            in
            Result.map
              (fun inferred ->
                shadowed
                @ List.filter
                    (fun (name, _) -> not (string_mem name local_names))
                    inferred)
              (infer_local 3 local_params))
    | FList
        [
          FSymbol ("into" | "clojure.core/into");
          target;
          FSymbol "cat";
          (FSymbol source_name as source);
        ] ->
        let element_ty =
          match string_assoc_opt source_name params with
          | Some source_ty -> (
              match Types.seqable_constraint_element source_ty with
              | Some ty
                when not (Types.equal ty TUnknown)
                     && (match ty with TMeta _ | TVar _ -> false | _ -> true) ->
                  ty
              | Some _ | None -> Types.dynamic_constraint TUnknown)
          | None -> Types.dynamic_constraint TUnknown
        in
        Result.bind (infer_form params target) (fun params ->
            infer_sequence_form element_ty params source)
    | FList
        [
          FSymbol ("into" | "clojure.core/into");
          target;
          transducer;
          source;
        ] ->
        Result.bind
          (Core_form_expansion.apply_transducer source transducer)
          (fun transformed -> infer_all params [ target; transformed ])
    | FList
        (FSymbol ("interleave" | "clojure.core/interleave") :: collections) ->
        let element_ty =
          collections
          |> List.find_map (fun collection ->
                 let element_ty =
                   match collection with
                   | FList [ FSymbol "repeat"; value ]
                   | FList [ FSymbol "repeat"; _; value ] ->
                       Some (inferred_form_type params value)
                   | _ ->
                       inferred_form_type params collection
                       |> Types.next_seq_element
                 in
                 match element_ty with
                 | Some (TUnknown | TMeta _ | TVar _) | None -> None
                 | Some element_ty -> Some element_ty)
          |> Option.value ~default:(Type_solver.fresh ())
        in
        List.fold_left
          (fun result collection ->
            Result.bind result (fun params ->
                match collection with
                | FSymbol name -> constrain_seqable element_ty params name
                | collection -> infer_sequence_form element_ty params collection))
          (Ok params) collections
    | FList (FList [ FKeyword keyword; FSymbol receiver ] :: arguments) -> (
        let infer_unknown_field () =
          let parameter_tys =
            List.mapi
              (fun index argument ->
                match inferred_form_type params argument with
                | TUnknown ->
                    fresh_type_variable
                      ("field_call_" ^ Names.sanitize_name receiver ^ "_"
                     ^ string_of_int index)
                | ty -> ty)
              arguments
          in
          Result.bind
            (add_record_field_constraint receiver keyword
               (TFn (parameter_tys, TUnknown)) params)
            (fun params ->
              List.fold_left2
                (fun result expected argument ->
                  Result.bind result (fun params ->
                      infer_expected expected params argument))
                (Ok params) parameter_tys arguments)
        in
        match
          match string_assoc_opt receiver params with
          | None -> None
          | Some ty -> Types.record_fields ty
        with
        | None -> infer_unknown_field ()
        | Some fields -> (
            match Types.find_field keyword fields with
            | Some { ty = TFn (parameter_tys, _); _ }
              when List.length parameter_tys = List.length arguments ->
                let rec infer_arguments params expected actual =
                  match (expected, actual) with
                  | [], [] -> Ok params
                  | expected_ty :: expected, argument :: actual ->
                      Result.bind (infer_expected expected_ty params argument)
                        (fun params -> infer_arguments params expected actual)
                  | _ -> assert false
                in
                infer_arguments params parameter_tys arguments
            | _ -> infer_unknown_field ()))
    | FList (FSymbol name :: args) ->
        observe_call name args (List.map (inferred_form_type params) args);
        infer_known_call name params args
    | FVector forms -> infer_all params forms
    | FMap pairs ->
        if
          List.exists
            (fun (key, _value) ->
              match key with FKeyword _ -> false | _ -> true)
            pairs
        then
          pairs
          |> List.fold_left
               (fun result (key, value) ->
                 Result.bind result (fun params ->
                     Result.bind (infer_form params key) (fun params ->
                         infer_form params value)))
               (Ok params)
        else
          pairs
          |> List.fold_left
               (fun result (_key, value) ->
                 Result.bind result (fun params -> infer_form params value))
               (Ok params)
    | FList forms -> infer_all params forms
    | FInt _ | FFloat _ | FChar _ | FString _ | FRegex _ | FBool _ | FKeyword _
    | FSymbol _ | FCoreSymbol _ ->
        Ok params
  in
  let rec propagate_record_ref_writes params = function
    | FList
        [
          FSymbol ("vreset!" | "reset!");
          FList [ FKeyword keyword; FSymbol receiver ];
          FList [ FSymbol "Some"; FSymbol value ];
        ]
      when string_mem_assoc receiver params && string_mem_assoc value params -> (
        match record_ref_field_value_type params receiver keyword with
        | Some (TNullable payload_ty | TOcaml_app ("option", [ payload_ty ])) ->
            constrain_symbol payload_ty params value
        | Some _ | None -> Ok params)
    | FList
        [
          FSymbol "__deftype-field-set!";
          FKeyword keyword;
          FSymbol receiver;
          FList [ FSymbol "Some"; FSymbol value ];
        ]
      when string_mem_assoc receiver params && string_mem_assoc value params -> (
        match record_mutable_field_value_type params receiver keyword with
        | Some (TNullable payload_ty | TOcaml_app ("option", [ payload_ty ])) ->
            constrain_symbol payload_ty params value
        | Some _ | None -> Ok params)
    | FList (FSymbol ("fn" | "let" | "let*" | "loop") :: _) -> Ok params
    | FList forms | FVector forms ->
        List.fold_left
          (fun result form ->
            Result.bind result (fun params ->
                propagate_record_ref_writes params form))
          (Ok params) forms
    | FMap pairs ->
        List.fold_left
          (fun result (key, value) ->
            Result.bind result (fun params ->
                Result.bind (propagate_record_ref_writes params key)
                  (fun params -> propagate_record_ref_writes params value)))
          (Ok params) pairs
    | FInt _ | FFloat _ | FChar _ | FString _ | FRegex _ | FBool _ | FKeyword _
    | FSymbol _ | FCoreSymbol _ ->
        Ok params
  in
  let same_params left right =
    List.length left = List.length right
    && List.for_all2
         (fun (left_name, left_ty) (right_name, right_ty) ->
           left_name = right_name && Types.equal left_ty right_ty)
         left right
  in
  let rec stabilize remaining params =
    branch_hint_symbols := [];
    let infer_body =
      match (expected_return_ty, List.rev body_forms) with
      | Some expected, result :: reversed_prefix ->
          Result.bind
            (infer_all params (List.rev reversed_prefix))
            (fun params -> infer_expected expected params result)
      | Some _, [] | None, _ -> infer_all params body_forms
    in
    Result.bind infer_body (fun inferred ->
        Result.bind
          (List.fold_left
             (fun result form ->
               Result.bind result (fun inferred ->
                   propagate_record_ref_writes inferred form))
             (Ok inferred) body_forms)
          (fun inferred ->
        let inferred =
          List.map
            (fun (name, ty) -> (name, deduplicate_protocol_constraints ty))
            inferred
        in
        if remaining = 0 || same_params params inferred then Ok inferred
        else stabilize (remaining - 1) inferred))
  in
  Result.map
    (fun inferred ->
      let names, types = List.split inferred in
      match (Type_solver.generalize (TTuple types)).body with
      | TTuple generalized -> List.combine names generalized
      | _ -> assert false)
    (stabilize 3 (constrain_maybe_reduced_callbacks params body_forms))
