open Semantic_type

type substitutions = (string * ty) list

type conflict = {
  left : ty;
  right : ty;
}

let rec string_assoc_opt name = function
  | [] -> None
  | (candidate, value) :: rest ->
      if String.equal name candidate then Some value
      else string_assoc_opt name rest

let string_mem_assoc name substitutions =
  Option.is_some (string_assoc_opt name substitutions)

let rec string_remove_assoc name = function
  | [] -> []
  | ((candidate, _) as entry) :: rest ->
      if String.equal name candidate then rest
      else entry :: string_remove_assoc name rest

let rec string_mem name = function
  | [] -> false
  | candidate :: _ when String.equal name candidate -> true
  | _ :: rest -> string_mem name rest

let rec has_applicable_substitution substitutions = function
  | TVar name -> string_mem_assoc name substitutions
  | TNullable inner | TArray inner | TRef inner | TList inner | TVector inner
  | TSet inner | TSeq inner ->
      has_applicable_substitution substitutions inner
  | TOcaml_app (_, arguments) | TTuple arguments ->
      List.exists (has_applicable_substitution substitutions) arguments
  | TFn (parameters, return_ty) ->
      List.exists (has_applicable_substitution substitutions) parameters
      || has_applicable_substitution substitutions return_ty
  | TOverloaded_fn arities ->
      List.exists
        (fun arity ->
          List.exists
            (has_applicable_substitution substitutions)
            arity.fixed_params
          || Option.fold ~none:false
               ~some:(has_applicable_substitution substitutions)
               arity.rest_param
          || has_applicable_substitution substitutions arity.return_ty)
        arities
  | TRecord fields ->
      List.exists
        (fun (field : field) ->
          has_applicable_substitution substitutions field.ty)
        fields
  | TNamed_record { type_arguments; fields; _ } ->
      List.exists (has_applicable_substitution substitutions) type_arguments
      || List.exists
           (fun (field : field) ->
             has_applicable_substitution substitutions field.ty)
           fields
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TUnknown | TOcaml _ ->
      false

let rec apply substitutions ty =
  match substitutions with
  | [] -> ty
  | _ when not (has_applicable_substitution substitutions ty) -> ty
  | _ ->
      let apply_ty = apply substitutions in
      match ty with
      | TVar name -> (
          match string_assoc_opt name substitutions with
          | None -> ty
          | Some replacement -> apply substitutions replacement)
      | TNullable inner -> TNullable (apply_ty inner)
      | TOcaml_app (name, arguments) ->
          TOcaml_app (name, List.map apply_ty arguments)
      | TTuple items -> TTuple (List.map apply_ty items)
      | TArray inner -> TArray (apply_ty inner)
      | TRef inner -> TRef (apply_ty inner)
      | TList inner -> TList (apply_ty inner)
      | TVector inner -> TVector (apply_ty inner)
      | TSet inner -> TSet (apply_ty inner)
      | TSeq inner -> TSeq (apply_ty inner)
      | TFn (parameters, return_ty) ->
          TFn (List.map apply_ty parameters, apply_ty return_ty)
      | TOverloaded_fn arities ->
          TOverloaded_fn
            (List.map
               (fun arity ->
                 {
                   fixed_params = List.map apply_ty arity.fixed_params;
                   rest_param = Option.map apply_ty arity.rest_param;
                   return_ty = apply_ty arity.return_ty;
                 })
               arities)
      | TRecord fields ->
          TRecord
            (List.map
               (fun (field : field) -> { field with ty = apply_ty field.ty })
               fields)
      | TNamed_record record ->
          TNamed_record
            {
              record with
              type_arguments = List.map apply_ty record.type_arguments;
              fields =
                List.map
                  (fun (field : field) -> { field with ty = apply_ty field.ty })
                  record.fields;
            }
      | ( TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol
        | TKeyword | TBool | TUnit | TNil | TUnknown | TOcaml _ ) as concrete ->
          concrete

let rec occurs name ty =
  match ty with
  | TVar candidate -> candidate = name
  | TNullable inner | TArray inner | TRef inner | TList inner | TVector inner
  | TSet inner | TSeq inner ->
      occurs name inner
  | TOcaml_app (_, arguments) | TTuple arguments ->
      List.exists (occurs name) arguments
  | TFn (parameters, return_ty) ->
      List.exists (occurs name) parameters || occurs name return_ty
  | TOverloaded_fn arities ->
      List.exists
        (fun arity ->
          List.exists (occurs name) arity.fixed_params
          || Option.fold ~none:false ~some:(occurs name) arity.rest_param
          || occurs name arity.return_ty)
        arities
  | TRecord fields ->
      List.exists (fun (field : field) -> occurs name field.ty) fields
  | TNamed_record { type_arguments; fields; _ } ->
      List.exists (occurs name) type_arguments
      || List.exists (fun (field : field) -> occurs name field.ty) fields
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TUnknown | TOcaml _ ->
      false

let bind substitutions name ty =
  let ty = apply substitutions ty in
  if ty = TVar name then Ok substitutions
  else if ty = TUnknown then Ok substitutions
  else if occurs name ty then Error { left = TVar name; right = ty }
  else
    let replacement = [ (name, ty) ] in
    Ok
      ((name, ty)
      :: string_remove_assoc name
           (List.map
              (fun (variable, existing) ->
                (variable, apply replacement existing))
              substitutions))

let rec variables ty =
  let union left right =
    List.fold_left
      (fun names name -> if string_mem name names then names else name :: names)
      left right
  in
  let variables_all types =
    List.fold_left (fun names ty -> union names (variables ty)) [] types
  in
  match ty with
  | TVar name -> [ name ]
  | TNullable inner | TArray inner | TRef inner | TList inner | TVector inner
  | TSet inner | TSeq inner ->
      variables inner
  | TOcaml_app (_, arguments) | TTuple arguments -> variables_all arguments
  | TFn (parameters, return_ty) -> variables_all (return_ty :: parameters)
  | TOverloaded_fn arities ->
      arities
      |> List.concat_map (fun arity ->
             arity.return_ty :: arity.fixed_params
             @ Option.to_list arity.rest_param)
      |> variables_all
  | TRecord fields ->
      fields |> List.map (fun (field : field) -> field.ty) |> variables_all
  | TNamed_record { type_arguments; fields; _ } ->
      variables_all
        (type_arguments @ List.map (fun (field : field) -> field.ty) fields)
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TUnknown | TOcaml _ ->
      []

let force substitutions name ty =
  let replacement = [ (name, ty) ] in
  (name, ty)
  :: string_remove_assoc name
       (List.map
          (fun (variable, existing) ->
            (variable, apply replacement existing))
          substitutions)

let matching_fields left right =
  left
  |> List.filter_map (fun (left_field : field) ->
         right
         |> List.find_opt (fun (right_field : field) ->
                left_field.keyword = right_field.keyword)
         |> Option.map (fun right_field -> (left_field.ty, right_field.ty)))

let rec unify substitutions left right =
  let left = apply substitutions left in
  let right = apply substitutions right in
  if left == right then Ok substitutions
  else
    match (left, right) with
    | TInt, TInt
    | TFloat, TFloat
    | TChar, TChar
    | TString, TString
    | TRegex, TRegex
    | TMap_keys, TMap_keys
    | TSymbol, TSymbol
    | TKeyword, TKeyword
    | TBool, TBool
    | TUnit, TUnit
    | TNil, TNil ->
        Ok substitutions
    | TOcaml left_name, TOcaml right_name when String.equal left_name right_name ->
        Ok substitutions
    | TUnknown, _ | _, TUnknown -> Ok substitutions
    | TVar name, ty | ty, TVar name -> bind substitutions name ty
    | TNullable left, TNullable right
    | TArray left, TArray right
    | TRef left, TRef right
    | TList left, TList right
    | TVector left, TVector right
    | TSet left, TSet right
    | TSeq left, TSeq right ->
        unify substitutions left right
    | TOcaml_app (left_name, left_args), TOcaml_app (right_name, right_args)
      when left_name = right_name && List.length left_args = List.length right_args
      ->
        unify_lists substitutions left_args right_args
    | TTuple left_items, TTuple right_items
      when List.length left_items = List.length right_items ->
        unify_lists substitutions left_items right_items
    | TFn (left_params, left_return), TFn (right_params, right_return)
      when List.length left_params = List.length right_params ->
        Result.bind
          (unify_lists substitutions left_params right_params)
          (fun substitutions -> unify substitutions left_return right_return)
    | TOverloaded_fn left_arities, TOverloaded_fn right_arities
      when List.length left_arities = List.length right_arities ->
        List.fold_left2
          (fun result left_arity right_arity ->
            Result.bind result (fun substitutions ->
                unify_arities substitutions left_arity right_arity))
          (Ok substitutions) left_arities right_arities
    | TNamed_record left_record, TNamed_record right_record
      when Type_id.equal left_record.type_id right_record.type_id
           && List.length left_record.type_arguments
              = List.length right_record.type_arguments ->
        Result.bind
          (unify_lists substitutions left_record.type_arguments
             right_record.type_arguments)
          (fun substitutions ->
            let fields =
              matching_fields left_record.fields right_record.fields
            in
            List.fold_left
              (fun result (left, right) ->
                Result.bind result (fun substitutions ->
                    unify substitutions left right))
              (Ok substitutions) fields)
    | (TRecord left_fields | TNamed_record { fields = left_fields; _ }),
      (TRecord right_fields | TNamed_record { fields = right_fields; _ }) ->
        let fields = matching_fields left_fields right_fields in
        List.fold_left
          (fun result (left, right) ->
            Result.bind result (fun substitutions ->
                unify substitutions left right))
          (Ok substitutions) fields
    | _ -> Error { left; right }

and unify_lists substitutions left right =
  List.fold_left2
    (fun result left right ->
      Result.bind result (fun substitutions -> unify substitutions left right))
    (Ok substitutions) left right

and unify_arities substitutions left right =
  if List.length left.fixed_params <> List.length right.fixed_params then
    Error
      {
        left = TOverloaded_fn [ left ];
        right = TOverloaded_fn [ right ];
      }
  else
    Result.bind
      (unify_lists substitutions left.fixed_params right.fixed_params)
      (fun substitutions ->
        let rest =
          match (left.rest_param, right.rest_param) with
          | None, None -> Ok substitutions
          | Some left, Some right -> unify substitutions left right
          | _ ->
              Error
                {
                  left = TOverloaded_fn [ left ];
                  right = TOverloaded_fn [ right ];
                }
        in
        Result.bind rest (fun substitutions ->
            unify substitutions left.return_ty right.return_ty))

let infer substitutions ~template ~actual = unify substitutions template actual

let infer_all substitutions ~templates ~actuals =
  if List.length templates <> List.length actuals then Ok substitutions
  else unify_lists substitutions templates actuals
