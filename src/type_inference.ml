open Ast
open Types

let replace_param name ty params =
  params
  |> List.map (fun (param_name, param_ty) ->
         if param_name = name then (param_name, ty) else (param_name, param_ty))

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
      TNullable (refine_type existing inferred)
  | ( TOcaml_app (existing_name, existing_args),
      TOcaml_app (inferred_name, inferred_args) )
    when existing_name = inferred_name
         && List.length existing_args = List.length inferred_args ->
      TOcaml_app
        (existing_name, List.map2 refine_type existing_args inferred_args)
  | TArray existing, TArray inferred -> TArray (refine_type existing inferred)
  | TRef existing, TRef inferred -> TRef (refine_type existing inferred)
  | TList existing, TList inferred -> TList (refine_type existing inferred)
  | TVector existing, TVector inferred ->
      TVector (refine_type existing inferred)
  | TSet existing, TSet inferred -> TSet (refine_type existing inferred)
  | TSeq existing, TSeq inferred -> TSeq (refine_type existing inferred)
  | TVar _, inferred -> inferred
  | existing, TVar _ -> existing
  | ( TFn (existing_params, existing_return),
      TFn (inferred_params, inferred_return) )
    when List.length existing_params = List.length inferred_params ->
      TFn
        ( List.map2 refine_type existing_params inferred_params,
          refine_type existing_return inferred_return )
  | existing, _ -> existing

let constrain_symbol expected_ty params name =
  match List.assoc_opt name params with
  | None -> Ok params
  | Some existing_ty ->
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

let constrain_comparable_symbol params name =
  match List.assoc_opt name params with
  | Some (TNullable _ | TOcaml_app ("option", [ _ ])) ->
      Ok (replace_param name (Types.dynamic_constraint TUnknown) params)
  | _ -> Ok params

let constrain_seqable element_ty params name =
  let rec add_constraint = function
    | TUnknown | TVar _ -> Types.seqable_constraint element_ty
    | TRecord _ as map_ty -> Types.dynamic_constraint map_ty
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
  match List.assoc_opt name params with
  | None -> Ok params
  | Some existing -> Ok (replace_param name (add_constraint existing) params)

let constrain_optional_seqable ?(sequential = false) element_ty params name =
  let make_optional element_ty value_ty =
    if sequential then Types.optional_sequential_constraint element_ty value_ty
    else Types.optional_seqable_constraint element_ty value_ty
  in
  let rec add_constraint = function
    | TUnknown | TVar _ -> make_optional element_ty TUnknown
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
  match List.assoc_opt name params with
  | None -> Ok params
  | Some existing -> Ok (replace_param name (add_constraint existing) params)

let add_record_field_constraint name keyword field_ty params =
  let merge_fields fields =
    match find_field keyword fields with
    | None -> Ok (make_field keyword field_ty :: fields)
    | Some field when Types.equal field.ty field_ty -> Ok fields
    | Some field -> (
        match (field.ty, field_ty) with
        | TUnknown, field_ty ->
            Ok
              (make_field keyword field_ty
              :: List.filter
                   (fun candidate -> candidate.keyword <> keyword)
                   fields)
        | _, TUnknown -> Ok fields
        | TRef TUnknown, TRef value_ty ->
            Ok
              (make_field keyword (TRef value_ty)
              :: List.filter
                   (fun candidate -> candidate.keyword <> keyword)
                   fields)
        | ( TNullable (TRecord existing_fields),
            TNullable (TRecord inferred_fields) ) ->
            let merge_nested fields (inferred : field) =
              match find_field inferred.keyword fields with
              | None -> Ok (inferred :: fields)
              | Some existing when Types.equal existing.ty inferred.ty ->
                  Ok fields
              | Some existing when Types.equal existing.ty TUnknown ->
                  Ok
                    (inferred
                    :: List.filter
                         (fun field -> field.keyword <> inferred.keyword)
                         fields)
              | Some _ when Types.equal inferred.ty TUnknown -> Ok fields
              | Some existing ->
                  Error.error
                    ("cannot infer " ^ inferred.keyword ^ " as "
                    ^ Types.source_name inferred.ty
                    ^ " because it is already "
                   ^ Types.source_name existing.ty)
            in
            Result.bind
              (List.fold_left
                 (fun result inferred ->
                   Result.bind result (fun fields ->
                       merge_nested fields inferred))
                 (Ok existing_fields) inferred_fields)
              (fun nested_fields ->
                Ok
                  (make_field keyword (TNullable (TRecord nested_fields))
                  :: List.filter
                       (fun candidate -> candidate.keyword <> keyword)
                       fields))
        | existing, inferred
          when Types.is_dynamic existing || Types.is_dynamic inferred
               || Option.is_some (Types.protocol_constraint_info existing)
               || Option.is_some (Types.protocol_constraint_info inferred) ->
            Ok
              (make_field keyword (refine_type existing inferred)
              :: List.filter
                   (fun candidate -> candidate.keyword <> keyword)
                   fields)
        | _ ->
            Error.error
              ("cannot infer " ^ keyword ^ " as " ^ Types.source_name field_ty
             ^ " because it is already " ^ Types.source_name field.ty))
  in
  let rec add_constraint = function
    | TUnknown -> Ok (TRecord [ make_field keyword field_ty ])
    | TRecord fields ->
        Result.map (fun fields -> TRecord fields) (merge_fields fields)
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
  match List.assoc_opt name params with
  | None -> Ok params
  | Some existing_ty ->
      Result.map
        (fun ty -> replace_param name ty params)
        (add_constraint existing_ty)

let rec numeric_form_type params = function
  | FInt _ -> TInt
  | FFloat _ -> TFloat
  | FSymbol name -> List.assoc_opt name params |> Option.value ~default:TUnknown
  | FList (FSymbol ("+" | "-" | "*" | "/" | "max" | "min") :: args) ->
      let types = List.map (numeric_form_type params) args in
      if List.exists (Types.equal TFloat) types then TFloat
      else if List.exists (Types.equal TInt) types then TInt
      else TUnknown
  | _ -> TUnknown

let inferred_form_type params = function
  | FInt _ -> TInt
  | FFloat _ -> TFloat
  | FChar _ -> TChar
  | FString _ -> TString
  | FBool _ -> TBool
  | FKeyword _ -> TKeyword
  | FSymbol name -> List.assoc_opt name params |> Option.value ~default:TUnknown
  | FList (FSymbol ("+" | "-" | "*" | "/" | "max" | "min") :: _) as form ->
      numeric_form_type params form
  | FList (_function :: FSymbol receiver :: _) -> (
      match List.assoc_opt receiver params with
      | Some ty when Types.is_dynamic ty -> ty
      | _ -> TUnknown)
  | FList [ FSymbol "not"; _ ] -> TBool
  | _ -> TUnknown

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

let infer_params ~lookup_function_ty ~lookup_protocol_constraint params
    body_forms =
  let next_type_variable = ref 0 in
  let fresh_type_variable prefix =
    let index = !next_type_variable in
    incr next_type_variable;
    TVar (prefix ^ "_" ^ string_of_int index)
  in
  let rec infer_expected expected_ty params = function
    | FSymbol name -> constrain_symbol expected_ty params name
    | FList [ FSymbol ("aget" | "unsafe-aget"); FSymbol array; index ] -> (
        match constrain_symbol (TArray expected_ty) params array with
        | Error _ as error -> error
        | Ok params -> infer_expected TInt params index)
    | FList (FSymbol name :: args) when List.mem_assoc name params -> (
        let parameter_types = List.map (inferred_form_type params) args in
        match
          constrain_symbol (TFn (parameter_types, expected_ty)) params name
        with
        | Error _ as error -> error
        | Ok params -> infer_all params args)
    | FList (FSymbol ("+" | "-" | "*" | "/" | "max" | "min") :: args)
      when Types.equal expected_ty TInt || Types.equal expected_ty TFloat ->
        infer_expected_all expected_ty params args
    | FList [ FKeyword keyword; FSymbol name ] ->
        add_record_field_constraint name keyword expected_ty params
    | FList [ FSymbol "get"; FSymbol name; FKeyword keyword ] ->
        add_record_field_constraint name keyword expected_ty params
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
    | FList (FSymbol name :: args) when List.mem_assoc name params ->
        let parameter_types = List.map (inferred_form_type params) args in
        constrain_symbol (TFn (parameter_types, TBool)) params name
    | FList [ FKeyword keyword; FSymbol name ] ->
        add_record_field_constraint name keyword
          (Types.dynamic_constraint TUnknown)
          params
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
      | (FSymbol _ | FVector _ | FMap _) :: collection :: rest ->
          Result.bind (infer_collection params collection) (fun params ->
              infer_bindings params rest)
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
    match lookup_function_ty name with
      | Ok (TFn (param_tys, _ret)) when List.length param_tys = List.length args
        ->
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
            List.fold_left2
              (fun acc expected_ty arg ->
                match acc with
                | Error _ as err -> err
                | Ok params -> infer_expected expected_ty params arg)
              (Ok params) expected_tys args)
    | _ -> infer_all params args
  and inferred_unary_function_param _params = function
    | FSymbol name -> (
        match lookup_function_ty name with
        | Ok (TFn ([ param_ty ], _)) -> param_ty
        | _ -> TUnknown)
    | FList (FSymbol "fn" :: FVector [ FSymbol name ] :: body_forms) -> (
        match infer_all [ (name, TUnknown) ] body_forms with
        | Ok inferred ->
            List.assoc_opt name inferred |> Option.value ~default:TUnknown
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
            List.assoc_opt item inferred |> Option.value ~default:TUnknown
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
            ( List.assoc_opt key inferred
              |> Option.value ~default:TUnknown,
              List.assoc_opt value inferred
              |> Option.value ~default:TUnknown )
        | Error _ -> (TUnknown, TUnknown))
    | _ -> (TUnknown, TUnknown)
  and infer_let params bindings body_forms =
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
        let rec infer_slot_writes params = function
          | FList [ FSymbol ("vreset!" | "reset!"); FSymbol slot; value ]
            when List.mem slot slots ->
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
          | FVector _ :: FSymbol source :: rest -> (
              match constrain_seqable TUnknown params source with
              | Error _ as err -> err
              | Ok params -> infer_values params rest)
          | FMap _ :: FSymbol source :: rest -> (
              match
                constrain_symbol
                  (Types.dynamic_constraint TUnknown)
                  params source
              with
              | Error _ as error -> error
              | Ok params -> infer_values params rest)
          | FSymbol _name :: value_form :: rest -> (
              match infer_form params value_form with
              | Error _ as err -> err
              | Ok params -> infer_values params rest)
          | _ -> Ok params
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
            | Ok params -> infer_all params body_forms))
    | _ -> infer_all params body_forms
  and infer_assoc params target pairs =
    let rec infer_pairs params = function
      | [] -> Ok params
      | FKeyword keyword :: value_form :: rest -> (
          match infer_form params value_form with
          | Error _ as err -> err
          | Ok params -> (
              match target with
              | FSymbol name -> (
                  let field_ty =
                    match value_form with
                    | FSymbol value_name -> (
                        match lookup_function_ty value_name with
                        | Ok ty -> ty
                        | Error _ -> (
                            match inferred_form_type params value_form with
                            | TUnknown when List.mem_assoc value_name params ->
                                fresh_type_variable
                                  ("assoc_" ^ Names.sanitize_name value_name)
                            | ty -> ty))
                    | _ -> inferred_form_type params value_form
                  in
                  let params =
                    match value_form with
                    | FSymbol value_name when List.mem_assoc value_name params
                      ->
                        constrain_symbol field_ty params value_name
                    | _ -> Ok params
                  in
                  match params with
                  | Error _ as error -> error
                  | Ok params -> (
                      match
                        add_record_field_constraint name keyword field_ty params
                      with
                  | Error _ as err -> err
                      | Ok params -> infer_pairs params rest))
              | _ -> infer_pairs params rest))
      | forms ->
          infer_expected_all (Types.dynamic_constraint TUnknown) params forms
    in
    let infer_target =
      match (target, pairs) with
      | FSymbol _name, (FKeyword _ :: _ | []) -> infer_form params target
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
      let shadowed = List.assoc_opt binding params in
      let branch_params =
        (binding, initial_payload_ty) :: List.remove_assoc binding params
      in
      match infer_form branch_params result with
      | Error _ as error -> error
      | Ok branch_params -> (
          let payload_ty =
            List.assoc_opt binding branch_params
            |> Option.value ~default:initial_payload_ty
          in
          let params = List.remove_assoc binding branch_params in
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
    infer_clauses params clauses
  and infer_form params = function
    | FList [ FSymbol "__type-hint"; FSymbol annotation; value ] -> (
        match Type_annotation.of_param_annotation annotation with
        | Error _ as error -> error
        | Ok hinted_ty -> (
            match value with
            | FSymbol name -> constrain_symbol hinted_ty params name
            | value -> infer_form params value))
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
        let shadowed = List.assoc_opt binding params in
        let branch_params =
          (binding, initial_payload_ty) :: List.remove_assoc binding params
        in
        match infer_form branch_params then_form with
        | Error _ as error -> error
        | Ok branch_params ->
            let payload_ty =
              List.assoc_opt binding branch_params
              |> Option.value ~default:initial_payload_ty
            in
            let params = List.remove_assoc binding branch_params in
            let params =
              match shadowed with
              | None -> params
              | Some ty -> (binding, ty) :: params
            in
            let infer_option =
              match payload_ty with
              | TUnknown -> infer_form params option_form
              | payload_ty ->
                  infer_expected (TNullable payload_ty) params option_form
            in
            Result.bind infer_option (fun params -> infer_form params else_form)
        )
    | FList [ FSymbol "with-meta"; FSymbol value; metadata ] -> (
        match
          constrain_symbol (Types.dynamic_constraint TUnknown) params value
        with
        | Error _ as error -> error
        | Ok params ->
            infer_expected (Types.dynamic_constraint TUnknown) params metadata)
    | FList [ FSymbol "meta"; FSymbol value ] ->
        constrain_symbol (Types.dynamic_constraint TUnknown) params value
    | FList
        [
          FSymbol "if-let";
          FVector [ _binding; FList (FSymbol function_name :: arguments) ];
          then_form;
          else_form;
        ]
      when List.mem_assoc function_name params -> (
        let parameter_types = List.map (inferred_form_type params) arguments in
        match
           constrain_symbol
             (TFn (parameter_types, TNullable TUnknown))
             params function_name
         with
        | Error _ as error -> error
        | Ok params -> infer_all params (arguments @ [ then_form; else_form ]))
    | FList
        [
          FSymbol ("every?" | "not-any?" | "not-every?");
          FSymbol predicate;
          FSymbol collection;
        ] ->
        let predicate_type =
          if
            List.mem predicate
              [
                "symbol?";
                "keyword?";
                "string?";
                "int?";
                "number?";
                "boolean?";
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
            match lookup_function_ty predicate with
            | Ok (TFn ([ parameter_type ], _)) -> parameter_type
            | _ -> TUnknown
        in
        constrain_seqable predicate_type params collection
    | FList
        [
          FSymbol
            ( "symbol?" | "keyword?" | "string?" | "int?" | "number?"
            | "boolean?" | "vector?" | "list?" | "seq?" | "set?" | "map?"
            | "fn?" | "coll?" );
          FSymbol value;
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
    | FList [ FSymbol "instance?"; FSymbol _type_name; FSymbol value ] ->
        constrain_symbol (Types.dynamic_constraint TUnknown) params value
    | FList (FSymbol name :: arguments) when List.mem_assoc name params -> (
        let parameter_tys = List.map (inferred_form_type params) arguments in
        match constrain_symbol (TFn (parameter_tys, TUnknown)) params name with
        | Error _ as err -> err
        | Ok params -> infer_all params arguments)
    | FList [ FSymbol "deref"; FList [ FKeyword keyword; FSymbol name ] ] ->
        add_record_field_constraint name keyword (TRef TUnknown) params
    | FList
        [ FSymbol "vreset!"; FList [ FKeyword keyword; FSymbol name ]; value ]
      ->
        add_record_field_constraint name keyword
          (TRef (inferred_form_type params value))
          params
    | FList [ FSymbol ("nil?" | "some?"); FSymbol value ] ->
        constrain_symbol (TOcaml_app ("option", [ TUnknown ])) params value
    | FList [ FSymbol "count"; FSymbol collection ] ->
        constrain_seqable TUnknown params collection
    | FList [ FSymbol "hash-unordered-coll"; FSymbol collection ] ->
        constrain_seqable TUnknown params collection
    | FList (FSymbol "merge" :: maps) ->
        infer_expected_all (Types.dynamic_constraint TUnknown) params maps
    | FList
        (FSymbol "update"
        :: target :: key
        :: FSymbol "update"
        :: nested_arguments) ->
        let nested_value = "__lg_nested_update_value" in
        let params =
          match (target, key) with
          | FSymbol target, FKeyword keyword ->
              add_record_field_constraint target keyword
                (Types.dynamic_constraint TUnknown)
                params
          | _ -> Ok params
        in
        Result.bind params (fun params ->
            infer_form params
              (FList
                 [
                   FSymbol "update";
                   target;
                   key;
                   FList
                     [
                       FSymbol "fn";
                       FVector [ FSymbol nested_value ];
                       FList
                         (FSymbol "update" :: FSymbol nested_value
                        :: nested_arguments);
                     ];
                 ]))
    | FList
        (FSymbol "update"
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
        (FSymbol "update"
        :: FSymbol target
        :: FKeyword keyword
        :: FSymbol updater
        :: extra_arguments) -> (
        let signature =
          match lookup_function_ty updater with
          | Ok (TFn (field_ty :: extra_tys, return_ty))
            when List.length extra_tys = List.length extra_arguments ->
              Some (field_ty, extra_tys, return_ty)
          | Ok (TOverloaded_fn arities) ->
              Option.bind
                (select_fn_arity arities (List.length extra_arguments + 1))
                (fun arity ->
                  match arity.fixed_params with
                  | field_ty :: extra_tys
                    when Option.is_none arity.rest_param
                         && List.length extra_tys = List.length extra_arguments
                    ->
                      Some (field_ty, extra_tys, arity.return_ty)
                  | _ -> None)
          | Ok _ | Error _ -> None
        in
        match signature with
        | None -> infer_all params extra_arguments
        | Some (field_ty, extra_tys, return_ty) -> (
            let field_ty = refine_type field_ty return_ty in
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
    | FList [ FSymbol "get"; FSymbol target; key ]
      when match key with FKeyword _ -> false | _ -> true -> (
        let dynamic = Types.dynamic_constraint TUnknown in
        match constrain_symbol dynamic params target with
        | Error _ as error -> error
        | Ok params -> infer_expected dynamic params key)
    | FList [ FSymbol ("aget" | "unsafe-aget"); FSymbol array; index ] -> (
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
    | FList [ FSymbol "satisfies?"; FSymbol protocol_name; FSymbol receiver ]
      -> (
        match lookup_protocol_constraint protocol_name with
        | None -> Error.error ("unknown protocol " ^ protocol_name)
        | Some constraint_ty -> constrain_symbol constraint_ty params receiver)
    | FList [ FSymbol "asort!"; FSymbol comparator; _ ] ->
        constrain_symbol (TFn ([ TUnknown; TUnknown ], TInt)) params comparator
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
    | FList (FSymbol "apply" :: FSymbol ("pr" | "clojure.core/pr") :: arguments)
      -> (
        match List.rev arguments with
        | FSymbol collection :: _ ->
            constrain_seqable
              (Types.dynamic_constraint TUnknown)
              params collection
        | _ -> infer_all params arguments)
    | FList (FSymbol "apply" :: _function :: arguments) -> (
        match List.rev arguments with
        | FSymbol collection :: _ ->
            constrain_seqable TUnknown params collection
        | _ -> infer_all params arguments)
    | FList [ FSymbol ("map" | "mapv"); fn; FSymbol collection ] ->
        let element_ty = inferred_unary_function_param params fn in
        constrain_seqable element_ty params collection
    | FList (FSymbol ("map" | "mapv") :: fn :: collection_forms)
      when List.length collection_forms >= 2 -> (
        let rec constrain_collections params = function
          | [] -> Ok params
          | FSymbol collection :: rest -> (
              match constrain_seqable TUnknown params collection with
              | Error _ as error -> error
              | Ok params -> constrain_collections params rest)
          | form :: rest -> (
              match infer_form params form with
              | Error _ as error -> error
              | Ok params -> constrain_collections params rest)
        in
        match infer_form params fn with
        | Error _ as error -> error
        | Ok params -> constrain_collections params collection_forms)
    | FList
        [
          FSymbol ("filter" | "remove" | "take-while" | "drop-while");
          fn;
          FSymbol collection;
        ] ->
        let element_ty = inferred_unary_function_param params fn in
        constrain_seqable element_ty params collection
    | FList
        [
          FSymbol ("map" | "mapv");
          _fn;
          FList [ FKeyword keyword; FSymbol collection ];
        ] ->
        add_record_field_constraint collection keyword
          (Types.dynamic_constraint TUnknown)
          params
    | FList [ FSymbol "reduce"; reducer; init; FSymbol collection ] -> (
        let element_ty = inferred_reducer_item params init reducer in
        match constrain_seqable element_ty params collection with
        | Error _ as error -> error
        | Ok params -> infer_form params reducer)
    | FList
        [
          FSymbol "reduce";
          reducer;
          init;
          FList [ FKeyword keyword; FSymbol collection ];
        ] -> (
        let element_ty = inferred_reducer_item params init reducer in
        match
          add_record_field_constraint collection keyword
            (Types.seqable_constraint element_ty)
            params
        with
        | Error _ as error -> error
        | Ok params -> infer_form params reducer)
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
            |> List.find_map (fun arg ->
                   match inferred_form_type params arg with
                   | TUnknown | TVar _ -> None
                   | ty when Types.is_dynamic ty -> None
                   | ty -> Some ty)
          in
          match concrete with
          | Some ty -> ty
          | None ->
              args
              |> List.find_map (fun arg ->
                     match inferred_form_type params arg with
                     | TVar _ as ty -> Some ty
                     | _ -> None)
              |> Option.value ~default:(fresh_type_variable "equality")
        in
        infer_expected_all expected_ty params args
    | FList
        [ FKeyword nested_keyword; FList [ FKeyword keyword; FSymbol name ] ] ->
        add_record_field_constraint name keyword
          (TRecord
             [ make_field nested_keyword (Types.dynamic_constraint TUnknown) ])
          params
    | FList [ FKeyword keyword; FSymbol name ] ->
        add_record_field_constraint name keyword TUnknown params
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
    | FList (FSymbol ("assoc" | "clojure.core/assoc") :: target :: pairs) ->
        infer_assoc params target pairs
    | FList (FSymbol "conj" :: FSymbol target :: values) -> (
        let element_ty =
          values
          |> List.find_map (fun value ->
                 match inferred_form_type params value with
                 | TUnknown -> None
                 | ty -> Some ty)
          |> Option.value ~default:TUnknown
        in
        match constrain_symbol (TVector element_ty) params target with
        | Error _ as error -> error
        | Ok params -> infer_all params values)
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
    | FList (FSymbol "str" :: args) ->
        List.fold_left
          (fun result arg ->
            Result.bind result (fun params ->
                match inferred_form_type params arg with
                | TUnknown -> infer_expected TString params arg
                | _ -> infer_form params arg))
          (Ok params) args
    | FList [ FSymbol "if"; condition; then_form; else_form ] -> (
        match infer_truthy params condition with
        | Error _ as err -> err
        | Ok params -> (
            match infer_form params then_form with
            | Error _ as err -> err
            | Ok params -> infer_form params else_form))
    | FList [ FSymbol "if"; condition; then_form ] -> (
        match infer_truthy params condition with
        | Error _ as err -> err
        | Ok params -> infer_form params then_form)
    | FList [ FSymbol "if-not"; condition; then_form; else_form ] -> (
        match infer_truthy params condition with
        | Error _ as err -> err
        | Ok params -> (
            match infer_form params then_form with
            | Error _ as err -> err
            | Ok params -> infer_form params else_form))
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
          | test_form :: value_form :: rest -> (
              match infer_truthy params test_form with
              | Error _ as err -> err
              | Ok params -> (
                  match infer_form params value_form with
                  | Error _ as err -> err
                  | Ok params -> infer_clauses params rest))
        in
        infer_clauses params clauses
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
          FList (FSymbol "fn" :: _fn_params :: body_forms);
          collection;
        ] -> (
        match infer_all params body_forms with
        | Error _ as err -> err
        | Ok params -> infer_collection params collection)
    | FList [ FSymbol ("butlast" | "dorun" | "doall"); collection ] ->
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
          FList (FSymbol "fn" :: _fn_params :: body_forms);
          collection;
        ] -> (
        match infer_all params body_forms with
        | Error _ as err -> err
        | Ok params -> infer_collection params collection)
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
                                List.assoc_opt local params
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
                           List.assoc_opt name inferred
                           |> Option.value
                                ~default:
                                  (List.assoc_opt name params
                                  |> Option.value ~default:TUnknown) ))
                in
                bindings
                |> List.fold_left
                     (fun result (local, value) ->
                       match (result, value) with
                       | (Error _ as error), _ -> error
                       | Ok params, FSymbol source
                         when List.mem_assoc source params ->
                           let local_ty =
                             List.assoc_opt local inferred
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
            let rec rewrite_aliases aliases = function
              | FSymbol name as form -> (
                  match List.assoc_opt name aliases with
                  | Some ((FSymbol _ | FKeyword _) as alias) ->
                      rewrite_aliases (List.remove_assoc name aliases) alias
                  | _ -> form)
              | FList
                  (FSymbol binding_form :: FVector nested_bindings :: body_forms)
                when binding_form = "let" || binding_form = "let*"
                     || binding_form = "loop"
                     || String.ends_with ~suffix:"/let" binding_form
                     || String.ends_with ~suffix:"/let*" binding_form ->
                  let rec rewrite_bindings aliases rewritten = function
                    | FSymbol name :: value :: rest ->
                        let value = rewrite_aliases aliases value in
                        rewrite_bindings (List.remove_assoc name aliases)
                          (value :: FSymbol name :: rewritten)
                          rest
                    | rest -> (aliases, List.rev_append rewritten rest)
                  in
                  let body_aliases, nested_bindings =
                    rewrite_bindings aliases [] nested_bindings
                  in
                  FList
                    (FSymbol binding_form :: FVector nested_bindings
                    :: List.map (rewrite_aliases body_aliases) body_forms)
              | FList (FSymbol "fn" :: FVector parameters :: body_forms) ->
                  let aliases =
                    List.fold_left
                      (fun aliases -> function
                        | FSymbol name -> List.remove_assoc name aliases
                        | _ -> aliases)
                      aliases parameters
                  in
                  FList
                    (FSymbol "fn" :: FVector parameters
                    :: List.map (rewrite_aliases aliases) body_forms)
              | FList forms -> FList (List.map (rewrite_aliases aliases) forms)
              | FVector forms ->
                  FVector (List.map (rewrite_aliases aliases) forms)
              | FMap pairs ->
                  FMap
                    (List.map
                       (fun (key, value) ->
                         ( rewrite_aliases aliases key,
                           rewrite_aliases aliases value ))
                       pairs)
              | form -> form
            in
            let rewritten_body_forms =
              List.map (rewrite_aliases aliases) body_forms
            in
            let rec infer_alias_constraints params = function
              | FList [ FSymbol reduce_name; reducer; init; FSymbol collection ]
                when reduce_name = "reduce"
                     || String.ends_with ~suffix:"/reduce" reduce_name -> (
                  match List.assoc_opt collection aliases with
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
    | FList (FSymbol "fn" :: _params :: body_forms) ->
        infer_all params body_forms
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
    | FList (FSymbol name :: args) -> infer_known_call name params args
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
        pairs
        |> List.fold_left
             (fun acc (_key, value) ->
               match acc with
               | Error _ as err -> err
               | Ok params -> infer_form params value)
             (Ok params)
    | FList forms -> infer_all params forms
    | FInt _ | FFloat _ | FChar _ | FString _ | FRegex _ | FBool _ | FKeyword _
    | FSymbol _ ->
        Ok params
  in
  infer_all (constrain_maybe_reduced_callbacks params body_forms) body_forms
