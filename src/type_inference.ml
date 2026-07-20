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
      match Types.protocol_constraint_info inferred with
      | Some (_, _, value_ty) ->
          Types.protocol_constraint_with_value inferred
            (refine_type existing value_ty)
      | None -> existing)
  | TNullable existing, TNullable inferred ->
      Types.normalize_nullable (TNullable (refine_type existing inferred))
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
  | TRecord existing, TRecord inferred ->
      TRecord (merge_record_fields existing inferred)
  | TVar _, inferred -> inferred
  | existing, TVar _ -> existing
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
          | Some (`Required, current_element, (TUnknown | TVar _)) ->
              let element_ty = refine_type current_element other_element in
              let element_ty =
                match element_ty with
                | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
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

let constrain_symbol expected_ty params name =
  match string_assoc_opt name params with
  | None -> Ok params
  | Some (TFn (parameter_tys, return_ty))
    when (match expected_ty with TFn _ -> true | _ -> false)
         &&
         (List.exists
            (function
              | TVar name -> String.starts_with ~prefix:"let_fn_" name
              | _ -> false)
            parameter_tys
         || match return_ty with
            | TVar name -> String.starts_with ~prefix:"let_fn_" name
            | _ -> false) ->
      Ok params
  | Some existing_ty ->
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
      Ok (replace_param name (refine_type existing_ty expected_ty) params)

let rec materialize_dynamic_unknown = function
  | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
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

let rec assoc_root_symbol = function
  | FSymbol name -> Some name
  | FList
      (FSymbol ("assoc" | "clojure.core/assoc" | "clojure.lang.RT/assoc")
      :: target :: _) ->
      assoc_root_symbol target
  | _ -> None

let constrain_comparable_symbol params name =
  match string_assoc_opt name params with
  | Some (TNullable _ | TOcaml_app ("option", [ _ ])) ->
      Ok (replace_param name (Types.dynamic_constraint TUnknown) params)
  | _ -> Ok params

let constrain_seqable element_ty params name =
  let rec add_constraint = function
    | TUnknown | TVar _ -> Types.seqable_constraint element_ty
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
          else refine_type existing_element element_ty
        in
        if constraint_name = Types.seqable_constraint_name then
          Types.seqable_constraint_with_value element_ty value_ty
        else if constraint_name = Types.optional_seqable_constraint_name then
          Types.optional_seqable_constraint element_ty value_ty
        else Types.optional_sequential_constraint element_ty value_ty
    | TNamed_record { type_parameters = [ parameter ]; _ } as record_ty ->
        Types.substitute_type_variables [ (parameter, element_ty) ] record_ty
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

let constrain_optional_seqable ?(sequential = false) element_ty params name =
  let make_optional element_ty value_ty =
    if sequential then Types.optional_sequential_constraint element_ty value_ty
    else Types.optional_seqable_constraint element_ty value_ty
  in
  let rec add_constraint = function
    | TUnknown | TVar _ -> make_optional element_ty TUnknown
    | TNullable _ | TOcaml_app ("option", [ _ ]) ->
        Types.dynamic_constraint TUnknown
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
    let merge_nested fields (inferred : field) =
      match find_field inferred.keyword fields with
      | None -> Ok (inferred :: fields)
      | Some existing when Types.equal existing.ty inferred.ty -> Ok fields
      | Some existing when Types.equal existing.ty TUnknown ->
          Ok
            (inferred
            :: List.filter
                 (fun field -> field.keyword <> inferred.keyword)
                 fields)
      | Some _ when Types.equal inferred.ty TUnknown -> Ok fields
      | Some existing
        when (match existing.ty with
             | TRecord _ | TNamed_record _ ->
                 Option.is_some
                   (Types.seqable_constraint_info inferred.ty)
             | TMap_keys ->
                 Option.is_some
                   (Types.seqable_constraint_info inferred.ty)
             | _ -> false) ->
          Ok fields
      | Some existing
        when (match inferred.ty with
             | TRecord _ | TNamed_record _ ->
                 Option.is_some
                   (Types.seqable_constraint_info existing.ty)
             | TMap_keys ->
                 Option.is_some
                   (Types.seqable_constraint_info existing.ty)
             | _ -> false) ->
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
    match find_field keyword fields with
    | None -> Ok (make_field keyword field_ty :: fields)
    | Some field when Types.equal field.ty field_ty -> Ok fields
    | Some field -> (
        match (field.ty, field_ty) with
        | (TUnknown | TVar _), field_ty ->
            Ok
              (make_field keyword field_ty
              :: List.filter
                   (fun candidate -> candidate.keyword <> keyword)
                   fields)
        | _, (TUnknown | TVar _) -> Ok fields
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
                make_field keyword (TRecord nested_fields)
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
            Ok fields
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
                    [ (parameter, element_ty) ] existing)
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
    | TUnknown | TVar _ -> Ok (TRecord [ make_field keyword field_ty ])
    | TMap_keys ->
        Ok
          (Types.dynamic_map TKeyword
             (Types.dynamic_constraint TUnknown))
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
              |> List.filter (fun name -> string_mem name record.type_parameters)
            in
            let inferred_ty =
              match
                (field.ty, Types.seqable_constraint_element field_ty)
              with
              | ( TNamed_record
                    { type_parameters = [ parameter ]; _ } as named,
                  Some element_ty ) ->
                  Types.substitute_type_variables
                    [ (parameter, element_ty) ] named
              | (TRecord _ | TNamed_record _ | TMap_keys), Some _ ->
                  field.ty
              | _ -> stored_value_type field_ty
            in
            match (constrained_parameters, inferred_ty) with
            | [], _ -> Ok record_ty
            | _, (TUnknown | TVar _) -> Ok record_ty
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
  | FList [ FSymbol "deref"; FSymbol reference ] -> (
      match string_assoc_opt reference params with
      | Some (TRef value_ty) -> value_ty
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
  | FList [ FSymbol ("inc" | "dec" | "count"); _ ] -> TInt
  | FList [ FSymbol ("first" | "second" | "last"); FSymbol receiver ] -> (
      let normalize = function
        | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
        | ty -> ty
      in
      match string_assoc_opt receiver params with
      | Some ty -> (
          match Types.seqable_constraint_element ty with
          | Some element_ty -> normalize element_ty
          | None -> (
              match Types.next_seq_element ty with
              | Some element_ty -> normalize element_ty
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
  | FList [ FSymbol operation; _ ]
    when String.equal operation "Array.length"
         || has_source_name operation "alength" ->
      TInt
  | FList [ FSymbol operation; FSymbol array; _from; _to ]
    when String.equal operation "Array.sub"
         || has_source_name operation "aslice" -> (
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
  | FList [ FSymbol "__lg_dynamic-narrow"; expected; _value ] ->
      inferred_form_type params expected
  | FList (FSymbol ("get" | "clojure.core/get") :: _) -> TUnknown
  | FList (_function :: FSymbol receiver :: _) -> (
      match string_assoc_opt receiver params with
      | Some ty when Types.is_dynamic ty -> ty
      | _ -> TUnknown)
  | FList [ FSymbol "not"; _ ] -> TBool
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

let infer_params ?(explicitly_dynamic_params = [])
    ?(materialize_open_equality = false) ?(observe_call = fun _ _ _ -> ())
    ~lookup_function_ty
    ~lookup_protocol_constraint ~lookup_dynamic_key_record_type
    ~resolve_named_record params body_forms =
  let next_type_variable = ref 0 in
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
        if string_mem name new_hints then (name, base_ty)
        else
          ( name,
            string_assoc_opt name inferred |> Option.value ~default:base_ty ))
      base
  in
  let rec guarded_protocol_receivers = function
    | FList
        [ FSymbol "satisfies?"; FSymbol _protocol_name; FSymbol receiver ] ->
        [ receiver ]
    | FList (FSymbol ("and" | "or" | "not") :: forms) ->
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
  let fresh_type_variable prefix =
    let index = !next_type_variable in
    incr next_type_variable;
    TVar (prefix ^ "_" ^ string_of_int index)
  in
  let freshen_call_type name ty =
    let prefix = "call_" ^ Names.sanitize_name name in
    let substitutions =
      Type_solver.variables ty
      |> List.map (fun variable ->
             ( variable,
               fresh_type_variable
                 (prefix ^ "_" ^ Names.sanitize_name variable) ))
    in
    Type_solver.apply substitutions ty
  in
  let rec infer_expected expected_ty params = function
    | FSymbol name -> constrain_symbol expected_ty params name
    | FList
        [ FSymbol "__lg_dynamic"; FList [ FKeyword keyword; FSymbol name ] ] ->
        add_record_field_constraint name keyword
          (fresh_type_variable
             ("dynamic_field_" ^ Names.keyword_to_ocaml_name keyword))
          params
    | FList [ FSymbol "__lg_dynamic"; value ] -> infer_form params value
    | FList [ FSymbol "__lg_dynamic-narrow"; expected; value ] ->
        infer_all params [ expected; value ]
    | FList
        [
          FSymbol operation;
          FList [ FSymbol "__lg_dynamic"; target ];
          index;
        ]
      when has_source_name operation "aget"
           || has_source_name operation "unsafe-aget" ->
        Result.bind (infer_form params target) (fun params ->
            infer_expected (Types.dynamic_constraint TUnknown) params index)
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
                    | TUnknown | TVar _ -> TUnknown
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
                      | TUnknown | TVar _ -> infer_form params result
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
            Result.bind
              (with_branch (fun () ->
                   infer_expected expected_ty params then_form))
              (fun inferred ->
                Result.map
                  (fun inferred ->
                    restore_branch_evidence params inferred previous_hints
                      condition)
                  (with_branch (fun () ->
                       infer_expected expected_ty inferred else_form))))
    | FList [ FSymbol "if"; condition; then_form ] ->
        Result.bind (infer_truthy params condition) (fun params ->
            let previous_hints = !branch_hint_symbols in
            Result.map
              (fun inferred ->
                restore_branch_evidence params inferred previous_hints
                  condition)
              (with_branch (fun () ->
                   infer_expected expected_ty params then_form)))
    | FList [ FSymbol "if-not"; condition; then_form; else_form ] ->
        Result.bind (infer_truthy params condition) (fun params ->
            let previous_hints = !branch_hint_symbols in
            Result.bind
              (with_branch (fun () ->
                   infer_expected expected_ty params then_form))
              (fun inferred ->
                Result.map
                  (fun inferred ->
                    restore_branch_evidence params inferred previous_hints
                      condition)
                  (with_branch (fun () ->
                       infer_expected expected_ty inferred else_form))))
    | FList [ FSymbol "Some"; value ] -> (
        match expected_ty with
        | TNullable value_ty | TOcaml_app ("option", [ value_ty ]) ->
            infer_expected value_ty params value
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
    | FList [ FSymbol "deref"; FSymbol reference ] ->
        constrain_symbol (TRef expected_ty) params reference
    | FList (FSymbol let_name :: bindings :: body_forms)
      when let_name = "let" || let_name = "let*"
           || String.ends_with ~suffix:"/let" let_name
           || String.ends_with ~suffix:"/let*" let_name ->
        infer_let ~expected_body:expected_ty params bindings body_forms
    | FList
        (FSymbol ("assoc" | "clojure.core/assoc" | "clojure.lang.RT/assoc")
        :: target :: pairs) -> (
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
    | FList [ FSymbol name; collection ]
      when (has_source_name name "array-from"
           || has_source_name name "into-array"
           || has_source_name name "to-array")
           && (match expected_ty with TArray _ -> true | _ -> false) -> (
        let element_ty =
          match expected_ty with TArray element_ty -> element_ty | _ -> assert false
        in
        match collection with
        | FSymbol name -> constrain_seqable element_ty params name
        | collection -> infer_form params collection)
    | FList [ FSymbol operation; array; from; length ]
      when (String.equal operation "Array.sub"
           || has_source_name operation "aslice")
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
        | Ok params -> infer_expected TInt params index)
    | FList [ FSymbol operation; FSymbol collection ]
      when has_source_name operation "vec" ->
        let element_ty =
          match expected_ty with
          | TVector inner -> inner
          | _ -> Types.dynamic_constraint TUnknown
        in
        constrain_seqable element_ty params collection
    | FList [ FSymbol operation; keys; values ]
      when has_source_name operation "zipmap" ->
        let key_ty, value_ty =
          Option.value (Types.dynamic_map_types expected_ty)
            ~default:
              ( Types.dynamic_constraint TUnknown,
                Types.dynamic_constraint TUnknown )
        in
        let constrain_collection element_ty params = function
          | FSymbol name -> constrain_seqable element_ty params name
          | form -> infer_form params form
        in
        Result.bind (constrain_collection key_ty params keys) (fun params ->
            constrain_collection value_ty params values)
    | FList [ FSymbol operation; FSymbol name ]
      when string_mem_assoc name params
           && (has_source_name operation "keys"
              || has_source_name operation "vals") ->
        let dynamic = Types.dynamic_constraint TUnknown in
        constrain_symbol (Types.dynamic_map dynamic dynamic) params name
    | FList (FSymbol name :: args) when string_mem_assoc name params -> (
        let parameter_types =
          List.mapi
            (fun index argument ->
              match inferred_form_type params argument with
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
    | FList [ FKeyword keyword; FSymbol name ] ->
        add_record_field_constraint name keyword expected_ty params
    | FList [ FKeyword keyword; FSymbol name; default ] ->
        Result.bind
          (add_record_field_constraint name keyword expected_ty params)
          (fun params -> infer_expected expected_ty params default)
    | FList
        [
          FKeyword keyword;
          FList
            [ FSymbol ("first" | "second" | "last"); collection ];
        ] ->
        let target_ty = TRecord [ make_field keyword expected_ty ] in
        infer_sequence_form target_ty params collection
      | FList
          [
            FSymbol ("first" | "second" | "last");
            FSymbol collection;
          ] ->
        constrain_seqable expected_ty params collection
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
                when Types.is_dynamic inferred_ty
                     && not (string_mem target explicitly_dynamic_params) ->
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
            infer_form params key)
    | FList [ FSymbol "get"; target; key ] ->
        let target_ty = inferred_form_type params target in
        if match target_ty with TVector _ -> true | _ -> false then
          Result.bind
            (infer_expected (TVector expected_ty) params target)
            (fun params -> infer_expected TInt params key)
        else
          let key_ty =
            inferred_form_type params key |> materialize_dynamic_unknown
          in
          let value_ty =
            match expected_ty with
            | TNullable inner | TOcaml_app ("option", [ inner ]) -> inner
            | ty -> ty
          in
          Result.bind
            (infer_expected (Types.dynamic_map key_ty value_ty) params target)
            (fun params -> infer_expected key_ty params key)
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
            match
              Type_solver.unify [] return_ty_for_unification expected_ty
            with
            | Error _ -> infer_form params form
            | Ok substitutions ->
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
    | FList (FSymbol ("and" | "or") :: conditions) ->
        List.fold_left
          (fun result condition ->
            Result.bind result (fun params -> infer_truthy params condition))
          (Ok params) conditions
    | FSymbol name ->
        constrain_symbol (Types.dynamic_constraint TUnknown) params name
    | FList (FSymbol name :: args) when string_mem_assoc name params ->
        let parameter_types = List.map (inferred_form_type params) args in
        constrain_symbol (TFn (parameter_types, TBool)) params name
    | FList [ FKeyword keyword; FSymbol name ] ->
        add_record_field_constraint name keyword
          (Types.dynamic_constraint TUnknown)
          params
    | FList [ FKeyword keyword; FSymbol name; default ] ->
        let field_ty = inferred_form_type params default in
        Result.bind (add_record_field_constraint name keyword field_ty params)
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
          let dynamic = Types.dynamic_constraint TUnknown in
          let local_params =
            List.map (fun name -> (name, dynamic)) local_names
            @ List.filter
                (fun (name, _) -> not (string_mem name local_names))
                params
          in
          Result.bind (infer_bindings local_params rest) (fun inferred ->
              let element_ty =
                Destructure.infer_pattern_type pattern (fun name ->
                    string_assoc_opt name inferred
                    |> Option.value ~default:dynamic)
                |> Result.value ~default:dynamic
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
    let materialize_unresolved_argument expected =
      match Types.constraint_value_type expected with
      | TRecord _ -> true
      | ty -> Option.is_some (Types.dynamic_map_types ty)
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
                          | TUnknown | TVar _ -> actual
                          | expected -> expected
                        in
                        if
                          Types.equal candidate TUnknown
                          || (match candidate with
                             | TVar _ -> true
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
                    [ (TUnknown | TVar _); value_ty ] )
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
              let actual = inferred_form_type params arg in
              let unresolved =
                Types.equal actual TUnknown
                || match actual with TVar _ -> true | _ -> false
              in
              if
                unresolved
                && match arg with FSymbol _ -> true | _ -> false
              then substitutions
              else if
                Types.is_dynamic actual
                || (unresolved && materialize_unresolved_argument expected)
              then
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
                    || match actual with TVar _ -> true | _ -> false
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
            | Some ty when not (Types.equal ty TUnknown) -> ty
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
              | FList (FSymbol ("=" | "not=") :: operands) ->
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
  and infer_sequence_form element_ty params = function
    | FSymbol name -> constrain_seqable element_ty params name
    | FList [ FKeyword keyword; FSymbol name ] ->
        add_record_field_constraint name keyword
          (Types.seqable_constraint element_ty)
          params
    | FList [ FSymbol operation; array ]
      when String.equal operation "array-seq"
           || has_source_name operation "array-to-seq" ->
        infer_expected (TArray element_ty) params array
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
          when not (match first with TUnknown | TVar _ -> true | _ -> false)
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
  and inferred_reducer_item params init = function
    | FSymbol name -> (
        match lookup_function_ty name with
        | Ok (TFn ([ _; item_ty ], _)) -> item_ty
        | _ -> TUnknown)
    | FList
        (FSymbol "fn"
        :: FVector [ FSymbol accumulator; FSymbol item ]
        :: body_forms) -> (
        let accumulator_ty = inferred_form_type params init in
        match
          infer_all
            [ (accumulator, accumulator_ty); (item, TUnknown) ]
            body_forms
         with
        | Ok inferred ->
            string_assoc_opt item inferred |> Option.value ~default:TUnknown
        | Error _ -> TUnknown)
    | _ -> TUnknown
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
                            | Some ty when not (Types.equal ty TUnknown) -> ty
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
          match inferred_form_type scope_params value with
          | TUnknown -> infer_local_function_type scope_params value
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
                            match expected with
                            | TUnknown | TVar _ -> infer_form params value
                            | expected -> infer_expected expected params value
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
                            if Types.equal ty TUnknown then
                              TVar ("let_" ^ Names.sanitize_name name)
                            else ty
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
                          | (name, value) :: rest ->
                              let expected =
                                string_assoc_opt name params
                                |> Option.value ~default:TUnknown
                              in
                              (match expected with
                              | TUnknown | TVar _ -> propagate params rest
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
                        | Some (TVar _ as ty) -> ty
                        | Some ty -> ty
                        | None -> (
                            match lookup_function_ty value_name with
                            | Ok ty -> ty
                            | Error _ -> inferred_form_type params value_form))
                    | _ -> inferred_form_type params value_form
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
      | forms ->
          infer_expected_all (Types.dynamic_constraint TUnknown) params forms
    in
    let infer_target =
      match (target, pairs) with
      | FSymbol _name, (FKeyword _ :: _ | []) -> infer_form params target
      | FSymbol name, key_form :: value_form :: _ -> (
          match
            Option.bind (string_assoc_opt name params) Types.dynamic_map_types
          with
          | Some _ ->
              let concrete_or_dynamic form =
                match inferred_form_type params form with
                | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
                | ty -> ty
              in
              constrain_symbol
                (Types.dynamic_map
                   (concrete_or_dynamic key_form)
                   (concrete_or_dynamic value_form))
                params name
          | None ->
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
                           (Types.dynamic_constraint TUnknown)
                           params key))
               (Ok params)
        in
        Result.bind params (fun params ->
        let value_ty =
          match inferred_form_type params value with
          | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
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
                    | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
                    | ty -> ty
                  in
                  Types.dynamic_map key_ty
                    (Types.dynamic_constraint nested_ty))
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
    let infer_option_clause params binding result =
      let initial_payload_ty =
        match inferred_form_type params target with
        | TNullable inner | TOcaml_app ("option", [ inner ]) -> inner
        | _ -> TVar ("option_" ^ Names.sanitize_name binding)
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
      | FList [ FSymbol "Some"; FSymbol binding ] :: result :: rest -> (
          match infer_option_clause params binding result with
          | Error _ as error -> error
          | Ok params -> infer_clauses params rest)
      | pattern :: result :: rest -> (
          let params =
            match pattern_type pattern with
            | Some expected_ty -> infer_expected expected_ty params target
            | None -> infer_form params target
          in
          match params with
          | Error _ as err -> err
          | Ok params -> (
              match infer_form params result with
              | Error _ as err -> err
              | Ok params -> infer_clauses params rest))
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
                 match ty with TVar _ -> false | _ -> true)
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
                     | Some (TUnknown | TVar _) -> true
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
        [ FSymbol "__lg_dynamic"; FList [ FKeyword keyword; FSymbol name ] ] ->
        add_record_field_constraint name keyword
          (fresh_type_variable
             ("dynamic_field_" ^ Names.keyword_to_ocaml_name keyword))
          params
    | FList [ FSymbol "__lg_dynamic"; value ] -> infer_form params value
    | FList [ FSymbol "__lg_dynamic-narrow"; expected; value ] ->
        infer_all params [ expected; value ]
    | FList (FSymbol "record" :: _record_type :: field_forms) ->
        let values =
          List.filter_map
            (function
              | FList [ FSymbol _field_name; value ] -> Some value | _ -> None)
            field_forms
        in
        infer_all params values
    | FList
        [
          FSymbol ("if-some" | "if-let");
          FVector [ FSymbol binding; option_form ];
          then_form;
          else_form;
        ] -> (
        let initial_payload_ty =
          match inferred_form_type params option_form with
          | TNullable inner | TOcaml_app ("option", [ inner ]) -> inner
          | _ -> TVar ("option_" ^ Names.sanitize_name binding)
        in
        let shadowed = string_assoc_opt binding params in
        let branch_params =
          (binding, initial_payload_ty) :: string_remove_assoc binding params
        in
        match infer_form branch_params then_form with
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
                          FSymbol ("first" | "second" | "last");
                          _collection;
                        ] ->
                        payload_ty
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
          match inferred_form_type params option_form with
          | TNullable inner | TOcaml_app ("option", [ inner ]) -> inner
          | _ -> TVar ("option_" ^ Names.sanitize_name binding)
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
                        FSymbol ("first" | "second" | "last");
                        _collection;
                      ] ->
                      payload_ty
                  | _ -> TNullable payload_ty
                in
                infer_expected expected_ty params option_form))
    | FList [ FSymbol "with-meta"; FSymbol value; metadata ] -> (
        match
          constrain_symbol (Types.dynamic_constraint TUnknown) params value
        with
        | Error _ as error -> error
        | Ok params ->
            infer_expected (Types.dynamic_constraint TUnknown) params metadata)
    | FList (FSymbol ("and" | "or") :: conditions) ->
        List.fold_left
          (fun result condition ->
            Result.bind result (fun params -> infer_truthy params condition))
          (Ok params) conditions
    | FList [ FSymbol "meta"; FSymbol value ] ->
        constrain_symbol (Types.dynamic_constraint TUnknown) params value
    | FList
        [
          FSymbol ("every?" | "not-any?" | "not-every?");
          FSymbol predicate;
          FSymbol collection;
        ] ->
        let collection_element =
          Option.bind (string_assoc_opt collection params)
            Types.seqable_constraint_element
        in
        let predicate_type =
          if
            List.exists
              (has_source_name predicate)
              [
                "symbol?";
                "keyword?";
                "string?";
                "int?";
                "number?";
                "boolean?";
                "array?";
                "vector?";
                "list?";
                "seq?";
                "set?";
                "map?";
                "fn?";
                "coll?";
              ]
          then Types.dynamic_constraint TUnknown
          else
            match string_assoc_opt predicate params with
            | Some (TSet ((TUnknown | TVar _) as element_ty)) ->
                Option.value collection_element ~default:element_ty
            | Some (TSet element_ty) -> element_ty
            | Some (TFn ([ parameter_type ], _)) -> parameter_type
            | Some ty when Types.is_dynamic ty ->
                Types.dynamic_constraint TUnknown
            | Some _ | None -> (
                match lookup_function_ty predicate with
                | Ok (TFn ([ parameter_type ], _)) -> parameter_type
                | _ -> TUnknown)
        in
        Result.bind (constrain_seqable predicate_type params collection)
          (fun params ->
            match string_assoc_opt predicate params with
            | Some (TSet _) ->
                constrain_symbol (TSet predicate_type) params predicate
            | Some _ | None -> Ok params)
    | FList [ FSymbol predicate; FSymbol value ]
      when List.exists
             (has_source_name predicate)
             [
               "symbol?";
               "keyword?";
               "string?";
               "int?";
               "number?";
               "boolean?";
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
          FSymbol ("name" | "namespace" | "hash" | "class" | "type");
          FSymbol value;
        ] ->
        constrain_symbol (Types.dynamic_constraint TUnknown) params value
    | FList [ FSymbol ".compareTo"; FSymbol left; FSymbol right ] -> (
        match
          constrain_symbol (Types.dynamic_constraint TUnknown) params left
        with
        | Error _ as error -> error
        | Ok params ->
            constrain_symbol (Types.dynamic_constraint TUnknown) params right)
    | FList [ FSymbol "compare"; FSymbol left; FSymbol right ] -> (
        match constrain_comparable_symbol params left with
        | Error _ as error -> error
        | Ok params -> constrain_comparable_symbol params right)
    | FList [ FSymbol ("identical?" | ".equals"); left; right ] ->
        infer_expected_all
          (Types.dynamic_constraint TUnknown)
          params [ left; right ]
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
          | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
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
    | FList [ FSymbol "instance?"; FSymbol _type_name; FSymbol value ] ->
        constrain_symbol (Types.dynamic_constraint TUnknown) params value
    | FList [ FSymbol operation; FSymbol name ]
      when string_mem_assoc name params
           && (has_source_name operation "keys"
              || has_source_name operation "vals") ->
        let dynamic = Types.dynamic_constraint TUnknown in
        constrain_symbol (Types.dynamic_map dynamic dynamic) params name
    | FList [ FSymbol operation; keys; values ]
      when has_source_name operation "zipmap" ->
        let dynamic = Types.dynamic_constraint TUnknown in
        let constrain_collection params = function
          | FSymbol name -> constrain_seqable dynamic params name
          | form -> infer_form params form
        in
        Result.bind (constrain_collection params keys) (fun params ->
            constrain_collection params values)
    | FList (FSymbol name :: arguments) when string_mem_assoc name params -> (
        let parameter_tys =
          List.mapi
            (fun index argument ->
              match inferred_form_type params argument with
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
            | TUnknown | TVar _ -> infer_form params value
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
          | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
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
          | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
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
          | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
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
          | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
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
    | FList [ FSymbol operation; left; right ]
      when has_source_name operation "subset?" ->
        let element_ty =
          match (inferred_form_type params left, inferred_form_type params right) with
          | TSet element_ty, _ | _, TSet element_ty -> element_ty
          | _ -> fresh_type_variable "set_subset_item"
        in
        let constrain_operand params = function
          | FSymbol name -> constrain_seqable element_ty params name
          | form -> infer_expected (TSet element_ty) params form
        in
        Result.bind (constrain_operand params left) (fun params ->
            constrain_operand params right)
    | FList [ FSymbol operation; left; right ]
      when has_source_name operation "union"
           || has_source_name operation "intersection"
           || has_source_name operation "difference" ->
        let set_element form =
          match inferred_form_type params form with
          | TSet element_ty -> Some element_ty
          | _ -> None
        in
        let element_ty =
          match (set_element left, set_element right) with
          | Some ((TUnknown | TVar _) as element_ty), None
          | None, Some ((TUnknown | TVar _) as element_ty) ->
              element_ty
          | Some element_ty, _ | _, Some element_ty -> element_ty
          | None, None -> fresh_type_variable "set_operation_item"
        in
        let constrain_operand params = function
          | FSymbol name -> constrain_symbol (TSet element_ty) params name
          | form -> infer_expected (TSet element_ty) params form
        in
        Result.bind (constrain_operand params left) (fun params ->
            constrain_operand params right)
    | FList
        [
          FSymbol ("vreset!" | "reset!");
          FList
            [
              FSymbol "__deftype-field-ref";
              FKeyword keyword;
              FSymbol receiver;
            ];
          value;
        ] -> (
        match record_ref_field_value_type params receiver keyword with
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
                  | Some (TUnknown | TVar _) | None ->
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
    | FList [ FSymbol ("nil?" | "some?"); FSymbol value ] ->
        constrain_symbol (TOcaml_app ("option", [ TUnknown ])) params value
    | FList [ FSymbol "count"; FSymbol collection ] ->
        constrain_seqable TUnknown params collection
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
        let signature = update_signature updater (List.length extra_arguments) in
        match signature with
        | None -> infer_all params extra_arguments
        | Some (field_ty, extra_tys, return_ty) -> (
            let field_ty = updated_value_type field_ty return_ty in
            let row_updater =
              match field_ty with
              | TRecord _ | TNamed_record _ -> true
              | _ -> false
            in
            let field_ty =
              if row_updater then Types.dynamic_constraint field_ty
              else field_ty
            in
            let extra_tys =
              if row_updater then List.map materialize_dynamic_unknown extra_tys
              else extra_tys
            in
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
        let target_ty, key_ty =
          match string_assoc_opt target params with
          | Some (TVector _ as ty) -> (ty, TInt)
          | _ ->
              let dynamic = Types.dynamic_constraint TUnknown in
              (dynamic, dynamic)
        in
        Result.bind (constrain_symbol target_ty params target) (fun params ->
            Result.bind (infer_expected key_ty params key) (fun params ->
                infer_all params (updater :: extra_arguments)))
    | FList [ FSymbol "get"; FSymbol target; key ]
      when match key with FKeyword _ -> false | _ -> true -> (
        match
          string_assoc_opt target params
          |> Option.map Types.constraint_value_type
        with
        | Some (TNamed_record _) -> infer_expected TKeyword params key
        | _ ->
            let dynamic = Types.dynamic_constraint TUnknown in
            let params =
              match string_assoc_opt target params with
              | Some ty when Option.is_some (Types.seqable_constraint_info ty) ->
                  replace_param target dynamic params
              | Some _ | None -> params
            in
            match constrain_symbol dynamic params target with
            | Error _ as error -> error
            | Ok params -> infer_expected dynamic params key)
    | FList [ FSymbol operation; FSymbol array ]
      when String.equal operation "Array.length"
           || has_source_name operation "alength" ->
        let element_ty =
          match string_assoc_opt array params with
          | Some (TArray element_ty | TOcaml_app ("array", [ element_ty ])) ->
              element_ty
          | _ -> fresh_type_variable ("array_" ^ Names.sanitize_name array)
        in
        constrain_symbol (TArray element_ty) params array
    | FList [ FSymbol operation; FSymbol array; from; length ]
      when String.equal operation "Array.sub"
           || has_source_name operation "aslice" ->
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
        | Ok params -> infer_expected TInt params index)
    | FList
        [
          FSymbol operation;
          FList [ FSymbol "__lg_dynamic"; target ];
          index;
        ]
      when has_source_name operation "aget"
           || has_source_name operation "unsafe-aget" ->
        Result.bind (infer_form params target) (fun params ->
            infer_expected (Types.dynamic_constraint TUnknown) params index)
    | FList [ FSymbol "nth"; FSymbol collection; index ] ->
        Result.bind (constrain_seqable TUnknown params collection)
          (fun params -> infer_expected TInt params index)
    | FList [ FSymbol "array-seq"; FSymbol array ] ->
        constrain_symbol (TArray TUnknown) params array
    | FList [ FSymbol "array-seq"; FSymbol array; index ] -> (
        match constrain_symbol (TArray TUnknown) params array with
        | Error _ as error -> error
        | Ok params -> infer_expected TInt params index)
    | FList [ FSymbol ("into-array" | "to-array"); FSymbol collection ] ->
        constrain_seqable TUnknown params collection
    | FList [ FSymbol "seqable?"; FSymbol collection ] ->
        constrain_optional_seqable TUnknown params collection
    | FList [ FSymbol "sequential?"; FSymbol collection ] ->
        constrain_optional_seqable ~sequential:true
          (Types.dynamic_constraint TUnknown)
          params collection
    | FList [ FSymbol "not-empty"; FSymbol collection ] ->
        constrain_optional_seqable TUnknown params collection
    | FList [ FSymbol "empty"; FSymbol collection ] -> (
        match string_assoc_opt collection params with
        | Some (TUnknown | TVar _) ->
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
        [ FSymbol "asort!"; FSymbol comparator; FSymbol array ] ->
        let comparator_ty =
          match string_assoc_opt comparator params with
          | Some ty -> Some ty
          | None -> Result.to_option (lookup_function_ty comparator)
        in
        let element_ty =
          match comparator_ty with
          | Some (TFn ([ left; right ], TInt))
            when Types.equal left right
                 && not (Types.equal left TUnknown)
                 && (match left with TVar _ -> false | _ -> true) ->
              left
          | _ -> TUnknown
        in
        Result.bind (constrain_symbol (TArray element_ty) params array)
          (fun params ->
            if string_mem_assoc comparator params then
              constrain_symbol (TFn ([ element_ty; element_ty ], TInt)) params
                comparator
            else Ok params)
    | FList
        [
          FSymbol (("uncurried-call" | "uncurried-compare") as name);
          FSymbol fn;
          left;
          right;
        ] ->
        let return_ty = if name = "uncurried-compare" then TInt else TUnknown in
        constrain_symbol
          (TFn
             ( [
                 inferred_form_type params left; inferred_form_type params right;
               ],
               return_ty ))
          params fn
    | FList
        [ FSymbol "vec"; FSymbol collection ] ->
        constrain_seqable (Types.dynamic_constraint TUnknown) params collection
    | FList
        [
          FSymbol
            ("first" | "second" | "last" | "seq" | "rest" | "next" | "empty?");
          FSymbol collection;
        ] ->
        constrain_seqable TUnknown params collection
    | FList
        [
          FSymbol
            ("first" | "second" | "last" | "seq" | "rest" | "next" | "empty?");
          FList [ FKeyword keyword; FSymbol record ];
        ] ->
        add_record_field_constraint record keyword
          (Types.dynamic_constraint TUnknown)
          params
    | FList [ FSymbol ("nthnext" | "nthrest"); FSymbol collection; count ] -> (
        match infer_expected TInt params count with
        | Error _ as err -> err
        | Ok params -> constrain_seqable TUnknown params collection)
    | FList [ FSymbol ".toString"; value; radix ] -> (
        match infer_expected TInt params value with
        | Error _ as err -> err
        | Ok params -> infer_expected TInt params radix)
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
        infer_expected_all (Types.dynamic_constraint TUnknown) params values
    | FList (FSymbol "apply" :: FSymbol ("pr" | "clojure.core/pr") :: arguments)
      -> (
        match List.rev arguments with
        | FSymbol collection :: _ ->
            constrain_seqable
              (Types.dynamic_constraint TUnknown)
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
          || match ty with TUnknown | TVar _ -> true | _ -> false
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
          match List.rev arguments with
          | collection :: reversed_fixed ->
              let dynamic = Types.dynamic_constraint TUnknown in
              Result.bind
                (infer_expected_all dynamic params (List.rev reversed_fixed))
                (fun params ->
                  match collection with
                  | FSymbol name -> constrain_seqable dynamic params name
                  | collection -> infer_form params collection)
          | [] -> Ok params
        else (
          match List.rev arguments with
          | FSymbol collection :: reversed_fixed ->
              let fixed_count = List.length reversed_fixed in
              let rec drop count values =
                if count <= 0 then values
                else
                  match values with
                  | [] -> []
                  | _ :: rest -> drop (count - 1) rest
              in
              let remaining_parameters = function
                | TFn (parameter_tys, _) -> drop fixed_count parameter_tys
                | TOverloaded_fn arities ->
                    arities
                    |> List.concat_map (fun arity ->
                           if fixed_count > List.length arity.fixed_params then
                             []
                           else
                             drop fixed_count arity.fixed_params
                             @ Option.to_list arity.rest_param)
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
              let element_ty =
                match remaining_parameters function_ty with
                | [] -> TUnknown
                | first :: rest
                  when List.for_all (Types.equal first) rest ->
                    first
                | _ -> Types.dynamic_constraint TUnknown
              in
              constrain_seqable element_ty params collection
          | _ -> infer_all params arguments)
    | FList [ FSymbol ("map" | "mapv" | "keep"); fn; collection ] ->
        let inferred_element_ty = inferred_unary_function_param params fn in
        let inferred_element_ty =
          match inferred_element_ty with
          | TUnknown | TVar _ ->
              inferred_literal_collection_item params collection
          | ty -> ty
        in
        let element_ty =
          match inferred_element_ty with
          | TUnknown | TVar _ -> fresh_type_variable "unary_map_item"
          | ty -> ty
        in
        let infer_collection =
          match collection with
          | FList [ FKeyword keyword; FSymbol name ] ->
              add_record_field_constraint name keyword
                (Types.dynamic_constraint TUnknown)
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
    | FList [ FSymbol "map-indexed"; fn; FSymbol collection ] ->
        let inferred_item_ty = inferred_map_indexed_item params fn in
        let item_ty =
          match inferred_item_ty with
          | TUnknown | TVar _ -> fresh_type_variable "map_indexed_item"
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
          | TUnknown | TVar _ -> fresh_type_variable "group_by_item"
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
          | TUnknown | TVar _ -> fresh_type_variable "filterv_item"
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
                 | TUnknown | TVar _ ->
                     List.nth_opt callback_tys index
                     |> Option.value ~default:TUnknown
                 | element_ty -> element_ty)
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
              | Some ty ->
                  Types.is_dynamic ty
                  || Types.equal ty TUnknown
                  || (match ty with TVar _ -> true | _ -> false)
              | None -> false)
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
                | Some (TUnknown | TVar _) ->
                    constrain_symbol
                      (Types.dynamic_constraint TUnknown)
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
          | TUnknown | TVar _ -> inferred_form_type params init
          | ty -> ty
        in
        Result.bind (infer_expected accumulator_ty params init) (fun params ->
            let inferred_element_ty =
              inferred_reducer_item params init reducer
            in
            let element_ty =
              match declared_element_ty with
              | TUnknown | TVar _ -> (
                  match collection with
                  | FSymbol collection -> (
                      match string_assoc_opt collection params with
                      | Some collection_ty -> (
                          match
                            Types.seqable_constraint_element collection_ty
                          with
                          | Some (TUnknown | TVar _) | None ->
                              inferred_element_ty
                          | Some element_ty -> element_ty)
                      | None -> inferred_element_ty)
                  | _ -> inferred_element_ty)
              | ty -> ty
            in
            Result.bind (infer_sequence_form element_ty params collection)
              (fun params ->
                let accumulator_ty = inferred_form_type params init in
                infer_expected
                  (TFn ([ accumulator_ty; element_ty ], TUnknown))
                  params reducer)))
    | FList [ FSymbol "sort"; FSymbol collection ] ->
        constrain_seqable (Types.dynamic_constraint TUnknown) params collection
    | FList [ FSymbol "sort"; _comparator; FSymbol collection ] ->
        constrain_seqable (Types.dynamic_constraint TUnknown) params collection
    | FList [ FSymbol ("rand-nth" | "shuffle"); FSymbol collection ] ->
        constrain_seqable (Types.dynamic_constraint TUnknown) params collection
    | FList [ FSymbol ("distinct" | "dedupe"); FSymbol collection ] ->
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
        (FSymbol
           ( "bit-and" | "bit-or" | "bit-xor" | "unchecked-add"
           | "unchecked-add-int" | "unchecked-subtract"
           | "unchecked-subtract-int" | "unchecked-multiply"
          | "unchecked-multiply-int" )
        :: args) ->
        infer_expected_all TInt params args
    | FList
        [
          FSymbol
            ( "inc" | "dec" | "zero?" | "pos?" | "neg?" | "even?" | "odd?"
            | "nat-int?" | "pos-int?" | "neg-int?" | "bit-not" | "unchecked-inc"
            | "unchecked-inc-int" | "unchecked-dec" | "unchecked-dec-int"
            | "unchecked-negate" | "unchecked-negate-int" );
          arg;
        ] ->
        infer_expected TInt params arg
    | FList
        [
          FSymbol
            ( "quot" | "rem" | "mod" | "bit-shift-left" | "bit-shift-right"
            | "bit-set" | "bit-clear" | "bit-flip" | "bit-test"
            | "bit-shift-right-zero-fill" | "hash-combine"
            | "clojure.lang.Util/hashCombine" | "unchecked-divide-int"
            | "unchecked-remainder-int" );
          left;
          right;
        ] -> (
        match infer_expected TInt params left with
        | Error _ as err -> err
        | Ok params -> infer_expected TInt params right)
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
    | FList [ FSymbol "not"; arg ] -> infer_truthy params arg
    | FList (FSymbol ("=" | "not=") :: args) ->
        let expected_ty =
          let concrete =
            args
            |> List.filter_map (fun arg ->
                   match inferred_form_type params arg with
                   | TUnknown | TVar _ -> None
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
          let has_unresolved_symbol =
            List.exists
              (function
                | FSymbol name -> (
                    match string_assoc_opt name params with
                    | Some (TUnknown | TVar _) -> true
                    | Some _ | None -> false)
                | _ -> false)
              args
          in
          match concrete with
          | [ _ ] when materialize_open_equality && has_unresolved_symbol ->
              Types.dynamic_constraint TUnknown
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
                     | TVar _ as ty -> Some ty
                     | _ -> None)
              |> Option.value ~default:(fresh_type_variable "equality")
        in
        infer_expected_all expected_ty params args
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
            [ FSymbol ("first" | "second" | "last"); collection ];
        ] ->
        infer_sequence_form (TRecord [ make_field keyword TUnknown ]) params
          collection
    | FList [ FKeyword keyword; FSymbol name ] ->
        add_record_field_constraint name keyword TUnknown params
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
        match infer_expected TMap_keys params (FSymbol name) with
        | Error _ as err -> err
        | Ok params -> infer_expected TKeyword params key)
    | FList [ FSymbol "contains?"; FSymbol name; key ] -> (
        let dynamic = Types.dynamic_constraint TUnknown in
        match infer_expected dynamic params (FSymbol name) with
        | Error _ as error -> error
        | Ok params -> infer_expected dynamic params key)
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
        (match inferred_form_type params target with
        | TUnknown | TVar _ ->
            let dynamic = Types.dynamic_constraint TUnknown in
            Result.bind (infer_expected dynamic params target) (fun params ->
                infer_expected_all dynamic params keys)
        | _ -> infer_all params (target :: keys))
    | FList
        (FSymbol ("assoc" | "clojure.core/assoc" | "clojure.lang.RT/assoc")
        :: target :: pairs) ->
        infer_assoc params target pairs
    | FList [ FSymbol ("transient" | "persistent!"); collection ] ->
        infer_expected (Types.dynamic_constraint TUnknown) params collection
    | FList (FSymbol "conj" :: target :: values) -> (
        let inferred_value_type value =
          match inferred_form_type params value with
          | (TUnknown | TVar _) as unresolved -> (
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
        let unresolved name = function
          | TUnknown | TVar _ -> TVar name
          | ty -> ty
        in
        match
          infer_expected
            (Types.dynamic_map
               (unresolved "map_key" key_ty)
               (unresolved "map_value" value_ty))
            params (FSymbol name)
        with
        | Error _ as error -> error
        | Ok params -> infer_all params [ reducer; init ])
    | FList [ FSymbol "reduce-kv"; reducer; init; collection ]
      when
        (match inferred_form_type params collection with
        | TUnknown | TVar _ -> true
        | _ -> false) -> (
        let key_ty, value_ty = inferred_kv_reducer_types params init reducer in
        let unresolved name = function
          | TUnknown | TVar _ -> TVar name
          | ty -> ty
        in
        match
          infer_expected
            (Types.dynamic_map
               (unresolved "map_key" key_ty)
               (unresolved "map_value" value_ty))
            params collection
        with
        | Error _ as error -> error
        | Ok params -> infer_all params [ reducer; init ])
    | FList (FSymbol "str" :: args) ->
        List.fold_left
          (fun result arg ->
            Result.bind result (fun params ->
                match inferred_form_type params arg with
                | TUnknown | TVar _ ->
                    infer_expected
                      (Types.dynamic_constraint TUnknown)
                      params arg
                | _ -> infer_form params arg))
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
        | Ok params -> infer_all params body_forms)
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
                (function TUnknown | TVar _ -> true | _ -> false)
                result_types
            in
            let concrete_types =
              result_types
              |> List.filter (fun ty ->
                     not (Types.equal ty TUnknown)
                     && match ty with TVar _ -> false | _ -> true)
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
          FSymbol "split-with";
          FList (FSymbol "fn" :: _fn_params :: [ body_form ]);
          collection;
        ] -> (
        match infer_truthy params body_form with
        | Error _ as err -> err
        | Ok params -> infer_collection params collection)
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
    | FList [ FSymbol ("set" | "butlast" | "dorun" | "doall"); collection ] ->
        infer_collection params collection
    | FList
        [
          FSymbol
            ( "take-last" | "drop-last" | "take-nth" | "split-at"
            | "bounded-count" );
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
            let local_params =
              bindings
              |> List.map (fun (local, value) ->
                     let ty = inferred_form_type params value in
                     let ty =
                       if Types.equal ty TUnknown then
                         fresh_type_variable
                           ("loop_" ^ Names.sanitize_name local)
                       else ty
                     in
                     (local, ty))
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
                       | Ok params, FSymbol source
                         when string_mem_assoc source params ->
                           let local_ty =
                             string_assoc_opt local inferred
                             |> Option.value ~default:TUnknown
                           in
                           constrain_symbol local_ty params source
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
                     && (match ty with TVar _ -> false | _ -> true) ->
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
        let dynamic = Types.dynamic_constraint TUnknown in
        List.fold_left
          (fun result collection ->
            Result.bind result (fun params ->
                match collection with
                | FSymbol name -> constrain_seqable dynamic params name
                | collection -> infer_form params collection))
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
          let dynamic = Types.dynamic_constraint TUnknown in
          pairs
          |> List.fold_left
               (fun result (key, value) ->
                 Result.bind result (fun params ->
                     Result.bind (infer_expected dynamic params key)
                       (fun params -> infer_expected dynamic params value)))
               (Ok params)
        else
          let infer_values =
            pairs
            |> List.fold_left
                 (fun acc (_key, value) ->
                   match acc with
                   | Error _ as err -> err
                   | Ok params -> infer_form params value)
                 (Ok params)
          in
          Result.bind infer_values (fun params ->
              let requires_dynamic_map =
                List.exists
                  (fun (_key, value) ->
                    match inferred_form_type params value with
                    | ty when Types.is_dynamic ty -> true
                    | TNil | TNullable _ | TOcaml_app ("option", _) -> true
                    | _ -> false)
                  pairs
              in
              if not requires_dynamic_map then Ok params
              else
                let dynamic = Types.dynamic_constraint TUnknown in
                List.fold_left
                  (fun result (_key, value) ->
                    Result.bind result (fun params ->
                        infer_expected dynamic params value))
                  (Ok params) pairs)
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
          FSymbol ("vreset!" | "reset!");
          FList
            [
              FSymbol "__deftype-field-ref";
              FKeyword keyword;
              FSymbol receiver;
            ];
          FList [ FSymbol "Some"; FSymbol value ];
        ]
      when string_mem_assoc receiver params && string_mem_assoc value params -> (
        match record_ref_field_value_type params receiver keyword with
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
    Result.bind (infer_all params body_forms) (fun inferred ->
        Result.bind
          (List.fold_left
             (fun result form ->
               Result.bind result (fun inferred ->
                   propagate_record_ref_writes inferred form))
             (Ok inferred) body_forms)
          (fun inferred ->
        let inferred =
          List.map
            (fun (name, ty) ->
              let ty =
                if string_mem name !branch_hint_symbols then
                  match ty with
                  | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
                  | ty -> ty
                else ty
              in
              (name, deduplicate_protocol_constraints ty))
            inferred
        in
        if remaining = 0 || same_params params inferred then Ok inferred
        else stabilize (remaining - 1) inferred))
  in
  stabilize 3 (constrain_maybe_reduced_callbacks params body_forms)
