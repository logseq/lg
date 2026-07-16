open Ast
open Types

let replace_param name ty params =
  params
  |> List.map (fun (param_name, param_ty) ->
         if param_name = name then (param_name, ty) else (param_name, param_ty))

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
  match List.assoc_opt name params with
  | None -> Ok params
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

let record_ref_field_value_type params receiver keyword =
  match List.assoc_opt receiver params with
  | None -> None
  | Some receiver_ty -> (
      match Types.record_fields receiver_ty with
      | None -> None
      | Some fields -> (
          match Types.find_field keyword fields with
          | Some { ty = TRef value_ty; _ } -> Some value_ty
          | Some _ | None -> None))

let rec assoc_root_symbol = function
  | FSymbol name -> Some name
  | FList (FSymbol ("assoc" | "clojure.core/assoc") :: target :: _) ->
      assoc_root_symbol target
  | _ -> None

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
    | TNamed_record { type_parameters = [ parameter ]; _ } as record_ty ->
        Types.substitute_type_variables [ (parameter, element_ty) ] record_ty
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
        | TRef _, TRef TUnknown -> Ok fields
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
        | existing, inferred when same_refinable_wrapper existing inferred ->
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
    | TUnknown | TVar _ -> Ok (TRecord [ make_field keyword field_ty ])
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
  | FList [ FSymbol ("first" | "second" | "last"); FSymbol receiver ] -> (
      let normalize = function
        | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
        | ty -> ty
      in
      match List.assoc_opt receiver params with
      | Some ty -> (
          match Types.seqable_constraint_element ty with
          | Some element_ty -> normalize element_ty
          | None -> (
              match Types.next_seq_element ty with
              | Some element_ty -> normalize element_ty
              | None -> if Types.is_dynamic ty then ty else TUnknown))
      | None -> TUnknown)
  | FList (FSymbol ("get" | "clojure.core/get") :: _) -> TUnknown
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

let rec rewrite_simple_aliases aliases = function
  | FSymbol name as form -> (
      match List.assoc_opt name aliases with
      | Some ((FSymbol _ | FKeyword _) as alias) ->
          rewrite_simple_aliases (List.remove_assoc name aliases) alias
      | _ -> form)
  | FList (FSymbol binding_form :: FVector bindings :: body_forms)
    when binding_form = "let" || binding_form = "let*"
         || binding_form = "loop"
         || String.ends_with ~suffix:"/let" binding_form
         || String.ends_with ~suffix:"/let*" binding_form ->
      let rec rewrite_bindings aliases rewritten = function
        | FSymbol name :: value :: rest ->
            let value = rewrite_simple_aliases aliases value in
            rewrite_bindings (List.remove_assoc name aliases)
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
            | FSymbol name -> List.remove_assoc name aliases
            | _ -> aliases)
          aliases parameters
      in
      FList
        (FSymbol "fn" :: FVector parameters
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

let infer_params ~lookup_function_ty ~lookup_protocol_constraint
    ~lookup_dynamic_key_record_type params body_forms =
  let next_type_variable = ref 0 in
  let fresh_type_variable prefix =
    let index = !next_type_variable in
    incr next_type_variable;
    TVar (prefix ^ "_" ^ string_of_int index)
  in
  let rec infer_expected expected_ty params = function
    | FSymbol name -> constrain_symbol expected_ty params name
    | FList [ FSymbol "if"; condition; then_form; else_form ] ->
        Result.bind (infer_truthy params condition) (fun params ->
            Result.bind (infer_expected expected_ty params then_form)
              (fun params -> infer_expected expected_ty params else_form))
    | FList [ FSymbol "if"; condition; then_form ] ->
        Result.bind (infer_truthy params condition) (fun params ->
            infer_expected expected_ty params then_form)
    | FList [ FSymbol "if-not"; condition; then_form; else_form ] ->
        Result.bind (infer_truthy params condition) (fun params ->
            Result.bind (infer_expected expected_ty params then_form)
              (fun params -> infer_expected expected_ty params else_form))
    | FList [ FSymbol "Some"; value ] -> (
        match expected_ty with
        | TNullable value_ty | TOcaml_app ("option", [ value_ty ]) ->
            infer_expected value_ty params value
        | _ -> infer_form params value)
    | FList [ FSymbol "weak-ref"; value ] -> (
        match Types.weak_element expected_ty with
        | Some value_ty -> infer_expected value_ty params value
        | None -> infer_form params value)
    | FList [ FSymbol "weak-deref"; reference ] -> (
        match expected_ty with
        | TNullable value_ty | TOcaml_app ("option", [ value_ty ]) ->
            infer_expected (Types.weak_type value_ty) params reference
        | _ -> infer_form params reference)
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
                  not (List.mem field.keyword assigned))
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
                infer_assoc params target pairs))
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
    | FList [ FSymbol ("aget" | "unsafe-aget"); FSymbol array; index ] -> (
        match constrain_symbol (TArray expected_ty) params array with
        | Error _ as error -> error
        | Ok params -> infer_expected TInt params index)
    | FList (FSymbol name :: args) when List.mem_assoc name params -> (
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
    | FList [ FSymbol ("get" | "clojure.core/get"); FSymbol target; key ] -> (
        match lookup_dynamic_key_record_type expected_ty with
        | Some record_ty ->
            Result.bind (constrain_symbol record_ty params target) (fun params ->
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
    let function_ty = lookup_function_ty name in
    match function_ty with
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
            let substitutions =
              let inferred_argument_type = function
                | FList (FSymbol name :: arguments) -> (
                    match lookup_function_ty name with
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
  and inferred_unary_function_param _params = function
    | FSymbol name -> (
        match lookup_function_ty name with
        | Ok (TFn ([ param_ty ], _)) -> param_ty
        | _ -> TUnknown)
    | FList
        (FSymbol "fn" :: (FVector _ as params_form) :: body_forms) -> (
        match Destructure.parse_param_specs params_form with
        | Ok [ (spec : Destructure.param_spec) ] -> (
            match spec.explicit_ty with
            | Some ty when not (Types.equal ty TUnknown) -> ty
            | _ when not spec.destructured -> (
                match infer_all [ (spec.source_name, TUnknown) ] body_forms with
                | Ok inferred ->
                    List.assoc_opt spec.source_name inferred
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
                        List.assoc_opt name inferred
                        |> Option.value ~default:dynamic)
                    |> Result.value ~default:TUnknown
                | Error _ -> TUnknown))
        | Ok _ | Error _ -> TUnknown)
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
        let provisional_params =
          List.map (fun name -> (name, TUnknown)) provisional_names
          @ List.filter
              (fun (name, _) -> not (List.mem name provisional_names))
              params
        in
        let inferred_locals =
          infer_body provisional_params body_forms
          |> Result.value ~default:provisional_params
        in
        let lookup_inferred_local name =
          List.assoc_opt name inferred_locals
          |> Option.value ~default:TUnknown
        in
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
                    let outer_params =
                      List.filter
                        (fun (name, _) -> not (List.mem name provisional_names))
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
                        (fun (name, _) -> not (List.mem name local_names))
                        params
                    in
                    let rec initial_locals locals = function
                      | [] -> List.rev locals
                      | (name, value) :: rest ->
                          let ty =
                            inferred_form_type
                              (List.rev_append locals outer_params)
                              value
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
                                List.assoc_opt name params
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
                            List.filter
                              (fun (name, _) ->
                                not (List.mem name local_names))
                              inferred)
                          (propagate inferred (List.rev bindings)))))))
    | _ -> infer_all params body_forms
  and infer_assoc params target pairs =
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
              | None -> infer_pairs params rest))
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
                     match List.assoc_opt name params with
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
        | Ok _hinted_ty -> infer_form params value)
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
            match lookup_function_ty predicate with
            | Ok (TFn ([ parameter_type ], _)) -> parameter_type
            | _ -> TUnknown
        in
        constrain_seqable predicate_type params collection
    | FList
        [
          FSymbol
            ( "symbol?" | "keyword?" | "string?" | "int?" | "number?"
            | "boolean?" | "array?" | "vector?" | "list?" | "seq?" | "set?"
            | "map?" | "fn?" | "coll?" );
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
    | FList [ FSymbol field_access; FSymbol name ]
      when String.starts_with ~prefix:".-" field_access ->
        let keyword =
          ":"
          ^ String.sub field_access 2 (String.length field_access - 2)
        in
        add_record_field_constraint name keyword TUnknown params
    | FList [ FSymbol "instance?"; FSymbol _type_name; FSymbol value ] ->
        constrain_symbol (Types.dynamic_constraint TUnknown) params value
    | FList (FSymbol name :: arguments) when List.mem_assoc name params -> (
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
        | None ->
            add_record_field_constraint name keyword
              (TRef (inferred_form_type params value))
              params)
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
          match List.assoc_opt target params with
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
        let dynamic = Types.dynamic_constraint TUnknown in
        match constrain_symbol dynamic params target with
        | Error _ as error -> error
        | Ok params -> infer_expected dynamic params key)
    | FList [ FSymbol ("aget" | "unsafe-aget"); FSymbol array; index ] -> (
        match constrain_symbol (TArray TUnknown) params array with
        | Error _ as error -> error
        | Ok params -> infer_expected TInt params index)
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
    | FList [ FSymbol "satisfies?"; FSymbol protocol_name; FSymbol receiver ]
      -> (
        match lookup_protocol_constraint protocol_name with
        | None -> Error.error ("unknown protocol " ^ protocol_name)
        | Some constraint_ty -> constrain_symbol constraint_ty params receiver)
    | FList
        [ FSymbol "asort!"; FSymbol comparator; FSymbol array ] ->
        let comparator_ty =
          match List.assoc_opt comparator params with
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
            if List.mem_assoc comparator params then
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
              match List.assoc_opt name params with
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
                    match List.assoc_opt name params with
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
        let element_ty =
          if Types.is_dynamic element_ty then TUnknown else element_ty
        in
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
          match concrete with
          | [ ty ] -> ty
          | _ :: _ :: _
            when List.exists
                   (function
                     | FSymbol name -> List.mem_assoc name params
                     | _ -> false)
                   args ->
              Types.dynamic_constraint TUnknown
          | ty :: _ -> ty
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
    | FList [ FSymbol ("transient" | "persistent!"); collection ] ->
        infer_expected (Types.dynamic_constraint TUnknown) params collection
    | FList (FSymbol "conj" :: target :: values) -> (
        let element_ty =
          values
          |> List.find_map (fun value ->
                 match inferred_form_type params value with
                 | TUnknown -> None
                 | ty -> Some (stored_value_type ty))
          |> Option.value ~default:TUnknown
        in
        let collection_ty =
          match inferred_form_type params target with
          | TList _ -> TList element_ty
          | TSet _ -> TSet element_ty
          | TVector _ -> TVector element_ty
          | _ -> TVector element_ty
        in
        match infer_expected collection_ty params target with
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
            let rewritten_body_forms =
              List.map (rewrite_simple_aliases aliases) body_forms
            in
            let rec infer_alias_constraints params = function
              | FList
                  (FSymbol apply_name :: FSymbol function_name :: arguments)
                when apply_name = "apply"
                     || String.ends_with ~suffix:"/apply" apply_name -> (
                  match List.assoc_opt function_name aliases with
                  | Some function_form ->
                      infer_form params
                        (FList
                           (FSymbol apply_name :: function_form :: arguments))
                  | None -> Ok params)
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
              |> List.filter (fun (name, _) -> List.mem name local_names)
            in
            let local_params =
              local_bindings
              @ List.filter
                  (fun (name, _) -> not (List.mem name local_names))
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
                    (fun (name, _) -> not (List.mem name local_names))
                    inferred)
              (infer_local 3 local_params))
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
          match List.assoc_opt receiver params with
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
    Result.bind (infer_all params body_forms) (fun inferred ->
        let inferred =
          List.map
            (fun (name, ty) ->
              (name, deduplicate_protocol_constraints ty))
            inferred
        in
        if remaining = 0 || same_params params inferred then Ok inferred
        else stabilize (remaining - 1) inferred)
  in
  stabilize 3 (constrain_maybe_reduced_callbacks params body_forms)
