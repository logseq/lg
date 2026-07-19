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

module Type_identity_table = Hashtbl.Make (struct
  type t = ty

  let equal left right = left == right
  let hash = Hashtbl.hash
end)

let rec map_preserving_identity map = function
  | [] as values -> values
  | head :: tail as values ->
      let mapped_head = map head in
      let mapped_tail = map_preserving_identity map tail in
      if mapped_head == head && mapped_tail == tail then values
      else mapped_head :: mapped_tail

let apply substitutions ty =
  match substitutions with
  | [] -> ty
  | _ ->
      let cache = Type_identity_table.create 32 in
      let rec apply_ty ty =
        match Type_identity_table.find_opt cache ty with
        | Some mapped -> mapped
        | None ->
            let mapped = apply_uncached ty in
            Type_identity_table.add cache ty mapped;
            mapped
      and apply_uncached ty =
        let apply_inner build inner =
          let mapped = apply_ty inner in
          if mapped == inner then ty else build mapped
        in
        let apply_field (field : field) =
          let field_ty = apply_ty field.ty in
          if field_ty == field.ty then field else { field with ty = field_ty }
        in
        match ty with
        | TVar name -> (
            match string_assoc_opt name substitutions with
            | None -> ty
            | Some replacement -> apply_ty replacement)
        | TNullable inner -> apply_inner (fun inner -> TNullable inner) inner
        | TOcaml_app (name, arguments) ->
            let mapped = map_preserving_identity apply_ty arguments in
            if mapped == arguments then ty else TOcaml_app (name, mapped)
        | TTuple items ->
            let mapped = map_preserving_identity apply_ty items in
            if mapped == items then ty else TTuple mapped
        | TArray inner -> apply_inner (fun inner -> TArray inner) inner
        | TRef inner -> apply_inner (fun inner -> TRef inner) inner
        | TList inner -> apply_inner (fun inner -> TList inner) inner
        | TVector inner -> apply_inner (fun inner -> TVector inner) inner
        | TSet inner -> apply_inner (fun inner -> TSet inner) inner
        | TSeq inner -> apply_inner (fun inner -> TSeq inner) inner
        | TFn (parameters, return_ty) ->
            let mapped_parameters =
              map_preserving_identity apply_ty parameters
            in
            let mapped_return = apply_ty return_ty in
            if mapped_parameters == parameters && mapped_return == return_ty then
              ty
            else TFn (mapped_parameters, mapped_return)
        | TOverloaded_fn arities ->
            let apply_optional = function
              | None as value -> value
              | Some inner as value ->
                  let mapped = apply_ty inner in
                  if mapped == inner then value else Some mapped
            in
            let apply_arity arity =
              let fixed_params =
                map_preserving_identity apply_ty arity.fixed_params
              in
              let rest_param = apply_optional arity.rest_param in
              let return_ty = apply_ty arity.return_ty in
              if
                fixed_params == arity.fixed_params
                && rest_param == arity.rest_param
                && return_ty == arity.return_ty
              then arity
              else { fixed_params; rest_param; return_ty }
            in
            let mapped = map_preserving_identity apply_arity arities in
            if mapped == arities then ty else TOverloaded_fn mapped
        | TRecord fields ->
            let mapped = map_preserving_identity apply_field fields in
            if mapped == fields then ty else TRecord mapped
        | TNamed_record record ->
            let type_arguments =
              map_preserving_identity apply_ty record.type_arguments
            in
            let fields = map_preserving_identity apply_field record.fields in
            if
              type_arguments == record.type_arguments && fields == record.fields
            then ty
            else TNamed_record { record with type_arguments; fields }
        | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol
        | TKeyword | TBool | TUnit | TNil | TUnknown | TOcaml _ ->
            ty
      in
      apply_ty ty

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
    | TNullable left, TOcaml_app ("option", [ right ])
    | TOcaml_app ("option", [ left ]), TNullable right ->
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
