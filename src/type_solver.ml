open Semantic_type

type variable =
  | Metavariable of int
  | Declared of string

type substitutions = (variable * ty) list

type conflict = {
  left : ty;
  right : ty;
}

let next_metavariable = ref 0

let fresh ?location () =
  let id = !next_metavariable in
  incr next_metavariable;
  TMeta { id; location }

let rec variable_assoc_opt variable = function
  | [] -> None
  | (candidate, value) :: rest ->
      if candidate = variable then Some value
      else variable_assoc_opt variable rest

let rec variable_remove_assoc variable = function
  | [] -> []
  | ((candidate, _) as entry) :: rest ->
      if candidate = variable then rest
      else entry :: variable_remove_assoc variable rest

let rec variable_mem variable = function
  | [] -> false
  | candidate :: _ when candidate = variable -> true
  | _ :: rest -> variable_mem variable rest

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
      let visiting = ref [] in
      let rec apply_ty ty =
        match Type_identity_table.find_opt cache ty with
        | Some mapped -> mapped
        | None ->
            let mapped = apply_uncached ty in
            Type_identity_table.add cache ty mapped;
            mapped
      and apply_replacement variable original replacement =
        if variable_mem variable !visiting then original
        else
          let previous = !visiting in
          visiting := variable :: previous;
          let mapped = apply_ty replacement in
          visiting := previous;
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
        | TMeta { id; _ } -> (
            match variable_assoc_opt (Metavariable id) substitutions with
            | None -> ty
            | Some (TMeta replacement) when replacement.id = id -> ty
            | Some replacement ->
                apply_replacement (Metavariable id) ty replacement)
        | TVar name -> (
            match variable_assoc_opt (Declared name) substitutions with
            | None -> ty
            | Some (TVar candidate) when String.equal candidate name -> ty
            | Some replacement ->
                apply_replacement (Declared name) ty replacement)
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

let rec occurs variable ty =
  match ty with
  | TMeta { id; _ } -> variable = Metavariable id
  | TVar name -> variable = Declared name
  | TNullable inner | TArray inner | TRef inner | TList inner | TVector inner
  | TSet inner | TSeq inner ->
      occurs variable inner
  | TOcaml_app (_, arguments) | TTuple arguments ->
      List.exists (occurs variable) arguments
  | TFn (parameters, return_ty) ->
      List.exists (occurs variable) parameters || occurs variable return_ty
  | TOverloaded_fn arities ->
      List.exists
        (fun arity ->
          List.exists (occurs variable) arity.fixed_params
          || Option.fold ~none:false ~some:(occurs variable) arity.rest_param
          || occurs variable arity.return_ty)
        arities
  | TRecord fields ->
      List.exists (fun (field : field) -> occurs variable field.ty) fields
  | TNamed_record { type_arguments; fields; _ } ->
      List.exists (occurs variable) type_arguments
      || List.exists (fun (field : field) -> occurs variable field.ty) fields
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TUnknown | TOcaml _ ->
      false

let bind substitutions variable ty =
  let ty = apply substitutions ty in
  let variable_ty =
    match variable with
    | Metavariable id -> TMeta { id; location = None }
    | Declared name -> TVar name
  in
  let same_variable =
    match (variable, ty) with
    | Metavariable id, TMeta meta -> id = meta.id
    | Declared name, TVar candidate -> name = candidate
    | _ -> false
  in
  if same_variable then Ok substitutions
  else if ty = TUnknown then Ok substitutions
  else if occurs variable ty then Error { left = variable_ty; right = ty }
  else
    let replacement = [ (variable, ty) ] in
    Ok
      ((variable, ty)
      :: variable_remove_assoc variable
           (List.map
              (fun (existing_variable, existing) ->
                (existing_variable, apply replacement existing))
              substitutions))

let bind_meta substitutions meta ty =
  bind substitutions (Metavariable meta.id) ty

let rec variables ty =
  let union left right =
    List.fold_left
      (fun variables variable ->
        if variable_mem variable variables then variables
        else variable :: variables)
      left right
  in
  let variables_all types =
    List.fold_left (fun names ty -> union names (variables ty)) [] types
  in
  match ty with
  | TMeta { id; _ } -> [ Metavariable id ]
  | TVar name -> [ Declared name ]
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

let conflict_is_occurs conflict =
  List.exists
    (fun variable -> occurs variable conflict.right)
    (variables conflict.left)
  || List.exists
       (fun variable -> occurs variable conflict.left)
       (variables conflict.right)

let rec is_open = function
  | TUnknown | TMeta _ | TVar _ -> true
  | TNullable inner | TArray inner | TRef inner | TList inner | TVector inner
  | TSet inner | TSeq inner ->
      is_open inner
  | TOcaml_app (_, arguments) | TTuple arguments -> List.exists is_open arguments
  | TFn (parameters, return_ty) -> List.exists is_open (return_ty :: parameters)
  | TOverloaded_fn arities ->
      List.exists
        (fun arity ->
          List.exists is_open
            (arity.return_ty :: arity.fixed_params
            @ Option.to_list arity.rest_param))
        arities
  | TRecord fields -> List.exists (fun (field : field) -> is_open field.ty) fields
  | TNamed_record { type_arguments; _ } -> List.exists is_open type_arguments
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TOcaml _ ->
      false

let force substitutions variable ty =
  let replacement = [ (variable, ty) ] in
  (variable, ty)
  :: variable_remove_assoc variable
       (List.map
          (fun (existing_variable, existing) ->
            (existing_variable, apply replacement existing))
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
    | TMeta meta, ty | ty, TMeta meta -> bind_meta substitutions meta ty
    | TVar name, ty | ty, TVar name ->
        bind substitutions (Declared name) ty
    | ( TOcaml_app ("__lg_truthy_constraint", [ left ]),
        TOcaml_app ("__lg_truthy_constraint", [ right ]) ) ->
        unify substitutions left right
    | TOcaml_app ("__lg_truthy_constraint", [ value_ty ]), ty
    | ty, TOcaml_app ("__lg_truthy_constraint", [ value_ty ]) ->
        unify substitutions value_ty ty
    | ( TOcaml_app ("__lg_printable_constraint", [ left ]),
        TOcaml_app ("__lg_printable_constraint", [ right ]) ) ->
        unify substitutions left right
    | TOcaml_app ("__lg_printable_constraint", [ value_ty ]), ty
    | ty, TOcaml_app ("__lg_printable_constraint", [ value_ty ]) ->
        unify substitutions value_ty ty
    | ( TOcaml_app ("__lg_symbol_predicate_constraint", [ left ]),
        TOcaml_app ("__lg_symbol_predicate_constraint", [ right ]) ) ->
        unify substitutions left right
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
    | ( TOcaml_app
          ("Lg_runtime.Runtime_map.t", [ key_ty; value_ty ]),
        (TRecord fields | TNamed_record { fields; nominal = false; _ }) )
    | ( (TRecord fields | TNamed_record { fields; nominal = false; _ }),
        TOcaml_app
          ("Lg_runtime.Runtime_map.t", [ key_ty; value_ty ]) ) ->
        Result.bind (unify substitutions key_ty TKeyword)
          (fun substitutions ->
            List.fold_left
              (fun result (field : field) ->
                Result.bind result (fun substitutions ->
                    unify substitutions value_ty field.ty))
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

let generalize ty =
  let declared_names =
    variables ty
    |> List.filter_map (function
         | Declared name -> Some name
         | Metavariable _ -> None)
  in
  let inferred_name id =
    let rec available suffix =
      let name =
        "g" ^ string_of_int id
        ^ if suffix = 0 then "" else "_" ^ string_of_int suffix
      in
      if List.mem name declared_names then available (suffix + 1) else name
    in
    available 0
  in
  let quantified, substitutions =
    variables ty
    |> List.fold_left
         (fun (quantified, substitutions) -> function
           | Declared name ->
               (Declared_variable name :: quantified, substitutions)
           | Metavariable id ->
               let name = inferred_name id in
               ( Inferred_variable { metavariable_id = id; name }
                 :: quantified,
                 (Metavariable id, TVar name) :: substitutions ))
         ([], [])
  in
  let quantified =
    List.rev quantified
  in
  { quantified; body = apply substitutions ty }

let instantiate scheme =
  let substitutions =
    List.map
      (function
        | Declared_variable name -> (Declared name, fresh ())
        | Inferred_variable { name; _ } -> (Declared name, fresh ()))
      scheme.quantified
  in
  apply substitutions scheme.body

let canonical_scheme_body scheme =
  let variable_name = function
    | Declared_variable name -> ("declared", name)
    | Inferred_variable { name; _ } -> ("inferred", name)
  in
  let substitutions =
    List.mapi
      (fun index variable ->
        let category, name = variable_name variable in
        ( Declared name,
          TVar
            ("__lg_scheme_" ^ category ^ "_" ^ string_of_int index) ))
      scheme.quantified
  in
  apply substitutions scheme.body
