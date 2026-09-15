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

let host_record_type = function
  | TOcaml type_name -> (
      match Ocaml_signature.record_type type_name with
      | Ok (TNamed_record record) -> Some (TNamed_record record)
      | Ok _ | Error _ -> None)
  | TOcaml_app (type_name, arguments) -> (
      match Ocaml_signature.record_type type_name with
      | Ok (TNamed_record record)
        when List.length record.type_parameters = List.length arguments ->
          let substitutions =
            List.combine record.type_parameters arguments
            |> List.map (fun (parameter, argument) ->
                   (Type_solver.Declared parameter, argument))
            |> Type_solver.of_list
          in
          Some (Type_solver.apply substitutions (TNamed_record record))
      | Ok _ | Error _ -> None)
  | _ -> None

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
  | TPoly_variant left, TPoly_variant right ->
      (match Variant_row.merge (fun left right -> Some (refine_type left right)) left right with
       | Some row -> TPoly_variant {row with bound = left.bound}
       | None -> existing)
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
  | (TConstraint (Seqable_constraint constraint_) as existing), inferred
    when Types.is_dynamic inferred ->
      (match Types.dynamic_constraint_info inferred with
      | Some
          (TOcaml_app ("Lg_runtime.Runtime_transient.map", [ _; _ ])) ->
          inferred
      | Some _ | None ->
          let value_ty = refine_type constraint_.storage inferred in
          if Types.equal value_ty (Types.constraint_value_type existing) then
            existing
          else
            TConstraint
              (Seqable_constraint { constraint_ with storage = value_ty }))
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
  | existing, inferred
    when Option.is_some (Types.hashable_constraint_info existing)
         && Option.is_some (Types.hashable_constraint_info inferred) ->
      Types.hashable_constraint
        (refine_type
           (Types.hashable_constraint_info existing |> Option.get)
           (Types.hashable_constraint_info inferred |> Option.get))
  | existing, inferred
    when Option.is_some (Types.printable_constraint_info existing)
         && Option.is_some (Types.printable_constraint_info inferred) ->
      Types.printable_constraint
        (refine_type
           (Types.printable_constraint_info existing |> Option.get)
           (Types.printable_constraint_info inferred |> Option.get))
  | existing, inferred
    when Option.is_some (Types.exception_data_constraint_info existing)
         && Option.is_some (Types.exception_data_constraint_info inferred) ->
      Types.exception_data_constraint
        (refine_type
           (Types.exception_data_constraint_info existing |> Option.get)
           (Types.exception_data_constraint_info inferred |> Option.get))
  | existing, inferred
    when Option.is_some (Types.comparable_constraint_info existing)
         && Option.is_some (Types.comparable_constraint_info inferred) ->
      Types.comparable_constraint
        (refine_type
           (Types.comparable_constraint_info existing |> Option.get)
           (Types.comparable_constraint_info inferred |> Option.get))
  | existing, inferred
    when Option.is_some (Types.array_index_constraint_info existing)
         && Option.is_some (Types.array_index_constraint_info inferred) ->
      Types.array_index_constraint
        (refine_type
           (Types.array_index_constraint_info existing |> Option.get)
           (Types.array_index_constraint_info inferred |> Option.get))
  | existing, inferred -> (
      match
        ( Types.seqable_constraint_info existing,
          Types.seqable_constraint_info inferred )
      with
      | ( Some (existing_kind, existing_element, existing_value),
          Some (inferred_kind, inferred_element, inferred_value) ) ->
          let element_ty = refine_type existing_element inferred_element in
          let value_ty = refine_type existing_value inferred_value in
          let kind =
            match (existing_kind, inferred_kind) with
            | `Required, _ | _, `Required -> `Required
            | `Optional_sequential, _ | _, `Optional_sequential ->
                `Optional_sequential
            | `Optional, `Optional -> `Optional
          in
          (match kind with
          | `Required ->
              Types.seqable_constraint_with_value element_ty value_ty
          | `Optional -> Types.optional_seqable_constraint element_ty value_ty
          | `Optional_sequential ->
              Types.optional_sequential_constraint element_ty value_ty)
      | _ -> (
          match
            ( Types.protocol_constraint_info existing,
              Types.protocol_constraint_info inferred )
          with
          | ( Some (existing_id, _, existing_value),
              Some (inferred_id, _, inferred_value) )
            when Protocol_id.equal existing_id inferred_id ->
              Types.protocol_constraint_with_value existing
                (refine_type existing_value inferred_value)
          | _ -> refine_nonmatching_type existing inferred))

and open_seqable_constraint ty =
  match Types.seqable_constraint_info ty with
  | Some (_, element_ty, storage_ty) ->
      Type_solver.is_open element_ty && Type_solver.is_open storage_ty
  | None -> false

and refine_nonmatching_type existing inferred =
  match (existing, inferred) with
  | existing, TString when open_seqable_constraint existing -> TString
  | TString, inferred when open_seqable_constraint inferred -> TString
  | existing, inferred
    when Option.is_some (Types.truthy_constraint_info existing) ->
      let value_ty = Types.truthy_constraint_info existing |> Option.get in
      Types.truthy_constraint (refine_type value_ty inferred)
  | existing, inferred
    when Option.is_some (Types.hashable_constraint_info existing) ->
      Types.hashable_constraint
        (refine_type
           (Types.hashable_constraint_info existing |> Option.get)
           inferred)
  | existing, inferred
    when Option.is_some (Types.printable_constraint_info existing)
         && Option.is_none (Types.printable_constraint_info inferred) ->
      let value_ty = Types.printable_constraint_info existing |> Option.get in
      if not (Type_solver.is_open inferred) && Types.same_shape value_ty inferred then
        refine_type value_ty inferred
      else Types.printable_constraint (refine_type value_ty inferred)
  | existing, inferred
    when Option.is_some (Types.printable_constraint_info inferred)
         && Option.is_none (Types.printable_constraint_info existing) ->
      let value_ty = Types.printable_constraint_info inferred |> Option.get in
      if not (Type_solver.is_open existing) && Types.same_shape existing value_ty then
        refine_type existing value_ty
      else Types.printable_constraint (refine_type existing value_ty)
  | existing, inferred
    when Option.is_some (Types.printable_constraint_info existing) ->
      Types.printable_constraint
        (refine_type
           (Types.printable_constraint_info existing |> Option.get)
           inferred)
  | existing, inferred
    when Option.is_some (Types.printable_constraint_info inferred) ->
      Types.printable_constraint
        (refine_type existing
           (Types.printable_constraint_info inferred |> Option.get))
  | existing, inferred
    when Option.is_some (Types.exception_data_constraint_info existing)
         && Option.is_none (Types.exception_data_constraint_info inferred) ->
      let value_ty = Types.exception_data_constraint_info existing |> Option.get in
      if Types.same_shape value_ty inferred then refine_type value_ty inferred
      else Types.exception_data_constraint (refine_type value_ty inferred)
  | existing, inferred
    when Option.is_some (Types.exception_data_constraint_info inferred)
         && Option.is_none (Types.exception_data_constraint_info existing) ->
      let value_ty = Types.exception_data_constraint_info inferred |> Option.get in
      if Types.same_shape existing value_ty then refine_type existing value_ty
      else Types.exception_data_constraint (refine_type existing value_ty)
  | existing, inferred
    when Option.is_some (Types.comparable_constraint_info existing) ->
      Types.comparable_constraint
        (refine_type
           (Types.comparable_constraint_info existing |> Option.get)
           inferred)
  | existing, inferred
    when Option.is_some (Types.array_index_constraint_info existing) ->
      Types.array_index_constraint
        (refine_type
           (Types.array_index_constraint_info existing |> Option.get)
           inferred)
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
  | TNullable existing, TOcaml_app ("option", [ inferred ]) ->
      Types.normalize_nullable (TNullable (refine_type existing inferred))
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
    when Types.is_next_seq_type_name name
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
  | TTuple existing, TTuple inferred
    when List.length existing = List.length inferred
         && not (List.exists Type_solver.is_open inferred) ->
      TTuple (List.map2 refine_type existing inferred)
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
    when inferred_row_compatible structural named ->
      named
  | (TNamed_record _ as named), (TRecord _ as structural)
    when inferred_row_compatible structural named ->
      named
  | (TRecord _ as structural), host
    when Option.is_some (host_record_type host) ->
      let named = Option.get (host_record_type host) in
      refine_type structural named
  | host, (TRecord _ as structural)
    when Option.is_some (host_record_type host) ->
      let named = Option.get (host_record_type host) in
      refine_type named structural
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

and inferred_row_compatible structural named =
  match (structural, named) with
  | TRecord fields, TNamed_record record ->
      let fields =
        List.map
          (fun (field : field) ->
            match Types.find_field field.keyword record.fields with
            | None -> field
            | Some actual -> { field with ty = refine_type field.ty actual.ty })
          fields
      in
      Types.row_compatible ~expected:(TRecord fields) ~actual:named
  | _ -> false

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
      | Some (TVector element) ->
          replace_param name (TVector (refine_type element other_element)) params
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
  if
    Option.is_some (Types.seqable_constraint_info left)
    && Option.is_some (Types.seqable_constraint_info right)
  then true
  else
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
  | TConstraint left, TConstraint right ->
      Types.constraint_compatible
        (fun ~expected:_ ~actual:_ -> true)
        left right
  | TTuple left, TTuple right -> List.length left = List.length right
  | TFn (left_params, _), TFn (right_params, _) ->
      List.length left_params = List.length right_params
  | _ -> false

let is_edn_value_type = Edn_value_elaborator.is_value_type

let edn_function_argument_compatible expected actual =
  let sequence_element = function
    | TSeq element | TList element | TVector element | TSet element
    | TArray element ->
        Some element
    | ty -> Types.next_seq_element ty
  in
  let directly_seqable = function
    | TList _ | TVector _ | TSet _ | TSeq _ | TArray _ | TString -> true
    | TOcaml_app ("Lg_runtime.Runtime_map.t", [ _; _ ]) -> true
    | ty ->
        is_edn_value_type ty
        || Option.is_some (Types.seqable_constraint_info ty)
  in
  match (Types.next_seq_element expected, sequence_element expected, sequence_element actual) with
  | _, Some expected_element, Some actual_element
    when (match expected with TSeq _ -> true | _ -> Option.is_some (Types.next_seq_element expected)) ->
      Types.assignable ~policy:Host_boundary ~expected:expected_element
        ~actual:actual_element
  | _ -> (
  match (Types.seqable_constraint_info expected, actual) with
  | Some ((`Optional | `Optional_sequential), _, _), TNil -> true
  | ( Some ((`Optional | `Optional_sequential), _, _),
      (TNullable inner | TOcaml_app ("option", [ inner ])) ) ->
      directly_seqable inner
  | Some _, actual -> directly_seqable actual
  | None, _ -> false)

let edn_function_call_compatible callee call =
  match (callee, call) with
  | TFn (callee_params, _), TFn (call_params, _)
    when List.length callee_params = List.length call_params ->
      List.for_all2
        (fun expected actual ->
          Result.is_ok (Type_solver.unify Type_solver.empty expected actual)
          || edn_function_argument_compatible expected actual)
        callee_params call_params
  | _ -> false

let fn_arity_of_function = function
  | TFn (fixed_params, return_ty) ->
      Some { fixed_params; rest_param = None; return_ty }
  | _ -> None

let function_types_unify left right =
  Result.is_ok (Type_solver.unify Type_solver.empty left right)
  || edn_function_call_compatible left right

let should_accumulate_overloaded_function_call existing_ty expected_ty =
  not (Types.equal existing_ty expected_ty)

let same_fixed_arity left right =
  List.length left.fixed_params = List.length right.fixed_params
  && Option.is_none left.rest_param && Option.is_none right.rest_param

let same_function_arity_shape left right =
  same_fixed_arity left right
  && Types.equal left.return_ty right.return_ty

let add_overloaded_arity arities arity =
  if
    List.exists
      (fun existing ->
        same_function_arity_shape existing arity
        && List.for_all2 Types.equal existing.fixed_params arity.fixed_params)
      arities
  then arities
  else arities @ [ arity ]

let overload_incompatible_function_call existing_ty expected_ty =
  match (fn_arity_of_function existing_ty, fn_arity_of_function expected_ty) with
  | Some existing_arity, Some expected_arity
    when same_fixed_arity existing_arity expected_arity ->
      Some (TOverloaded_fn [ existing_arity; expected_arity ])
  | _ -> None

let can_accumulate_overloaded_function_parameter name =
  String.equal name "eq" || String.equal name "every-fn"

let rec constrain_symbol expected_ty params name =
  match string_assoc_opt name params with
  | Some (TOverloaded_fn arities)
    when (match expected_ty with TFn _ -> true | _ -> false) -> (
      match fn_arity_of_function expected_ty with
      | Some expected_arity ->
          if can_accumulate_overloaded_function_parameter name then
            Ok
              (replace_param name
                 (TOverloaded_fn (add_overloaded_arity arities expected_arity))
                 params)
          else constrain_monomorphic_symbol expected_ty params name
                 (TOverloaded_fn arities)
      | None -> Ok params)
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
        (match
           Type_solver.unify Type_solver.empty instantiated expected_ty
         with
        | Ok _ -> Ok params
        | Error _
          when edn_function_call_compatible instantiated expected_ty ->
            Ok params
        | Error _ -> (
            match
              if can_accumulate_overloaded_function_parameter name then
                overload_incompatible_function_call instantiated expected_ty
              else None
            with
            | Some overloaded -> Ok (replace_param name overloaded params)
            | None ->
                Error.error
                  (name ^ " called with incompatible arguments: expected "
                 ^ Types.source_name instantiated ^ ", got "
                 ^ Types.source_name expected_ty)))
      else constrain_monomorphic_symbol expected_ty params name existing_ty
  | Some existing_ty ->
      constrain_monomorphic_symbol expected_ty params name existing_ty

and constrain_monomorphic_symbol expected_ty params name existing_ty =
  match
    if can_accumulate_overloaded_function_parameter name then
      overload_incompatible_function_call existing_ty expected_ty
    else None
  with
  | Some overloaded
    when should_accumulate_overloaded_function_call existing_ty expected_ty ->
      Ok (replace_param name overloaded params)
  | _ ->
      let substitutions =
        Type_solver.unify Type_solver.empty existing_ty expected_ty
        |> Result.value ~default:Type_solver.empty
      in
      let params =
        List.map
          (fun (param_name, param_ty) ->
            ( param_name,
              Type_solver.apply substitutions param_ty
              |> Types.deduplicate_protocol_constraints ))
          params
      in
      let existing_ty =
        Type_solver.apply substitutions existing_ty
        |> Types.deduplicate_protocol_constraints
      in
      let expected_ty =
        Type_solver.apply substitutions expected_ty
        |> Types.deduplicate_protocol_constraints
      in
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

let constrain_exception_data_symbol params name =
  match string_assoc_opt name params with
  | Some ty when Option.is_some (Types.exception_data_constraint_info ty) ->
      Ok params
  | Some TUnknown ->
      Ok
        (replace_param name
           (Types.exception_data_constraint (Type_solver.fresh ()))
           params)
  | Some ((TMeta _ | TVar _) as value_ty) ->
      Ok (replace_param name (Types.exception_data_constraint value_ty) params)
  | Some _ | None -> Ok params

let constrain_hashable_symbol params name =
  match string_assoc_opt name params with
  | Some ty when Option.is_some (Types.hashable_constraint_info ty) -> Ok params
  | Some TUnknown ->
      Ok
        (replace_param name
           (Types.hashable_constraint (Type_solver.fresh ()))
           params)
  | Some ((TMeta _ | TVar _) as value_ty) ->
      Ok (replace_param name (Types.hashable_constraint value_ty) params)
  | Some _ | None -> Ok params

let constrain_comparable_symbol params name =
  match string_assoc_opt name params with
  | Some ty when Option.is_some (Types.comparable_constraint_info ty) ->
      Ok params
  | Some TUnknown ->
      Ok
        (replace_param name
           (Types.comparable_constraint (Type_solver.fresh ()))
           params)
  | Some ((TMeta _ | TVar _) as value_ty) ->
      Ok (replace_param name (Types.comparable_constraint value_ty) params)
  | Some _ | None -> Ok params

let constrain_array_index_symbol params name =
  match string_assoc_opt name params with
  | Some ty when Option.is_some (Types.array_index_constraint_info ty) ->
      Ok params
  | Some TUnknown ->
      Ok
        (replace_param name
           (Types.array_index_constraint (Type_solver.fresh ()))
           params)
  | Some ((TMeta _ | TVar _) as value_ty) ->
      Ok
        (replace_param name
           (Types.array_index_constraint value_ty)
           params)
  | Some ty ->
      let value_ty = Types.constraint_value_type ty in
      if Types.equal value_ty TInt || Types.equal value_ty TFloat then
        Ok (replace_param name (Types.array_index_constraint value_ty) params)
      else
        Error.error
          ("array index requires int or float, got " ^ Types.source_name value_ty)
  | None -> Ok params

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
