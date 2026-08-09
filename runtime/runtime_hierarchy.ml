type value = Lg_edn_backend.t

let nil = Lg_edn_backend.Nil
let equal = Runtime_edn.equal

let empty_map = Lg_edn_backend.Map [||]

let make () =
  Lg_edn_backend.Map
    [|
      (Lg_edn_backend.Keyword "parents", empty_map);
      (Lg_edn_backend.Keyword "descendants", empty_map);
      (Lg_edn_backend.Keyword "ancestors", empty_map);
    |]

let map_entries = function
  | Lg_edn_backend.Map entries -> entries
  | _ -> invalid_arg "hierarchy relation must be an EDN map"

let map_get entries key =
  entries
  |> Array.find_map (fun (candidate, value) ->
         if equal candidate key then Some value else None)

let relation hierarchy field =
  let entries = map_entries hierarchy in
  match map_get entries (Lg_edn_backend.Keyword field) with
  | Some value -> map_entries value
  | None -> [||]

let set_values = function
  | Lg_edn_backend.Set values -> values
  | Lg_edn_backend.Nil -> [||]
  | _ -> invalid_arg "hierarchy relation value must be an EDN set"

let values_for entries key =
  map_get entries key |> Option.map set_values |> Option.value ~default:[||]

let contains values value = Array.exists (equal value) values

let append_unique values additions =
  Array.fold_left
    (fun values value ->
      if contains values value then values else Array.append values [| value |])
    values additions

let assoc entries key value =
  let replaced = ref false in
  let entries =
    Array.map
      (fun (candidate, current) ->
        if equal candidate key then (
          replaced := true;
          (candidate, value))
        else (candidate, current))
      entries
  in
  if !replaced then entries else Array.append entries [| (key, value) |]

let dissoc entries key =
  entries |> Array.to_list
  |> List.filter (fun (candidate, _) -> not (equal candidate key))
  |> Array.of_list

let with_relations parents descendants ancestors =
  Lg_edn_backend.Map
    [|
      (Lg_edn_backend.Keyword "parents", Lg_edn_backend.Map parents);
      (Lg_edn_backend.Keyword "descendants", Lg_edn_backend.Map descendants);
      (Lg_edn_backend.Keyword "ancestors", Lg_edn_backend.Map ancestors);
    |]

let non_empty_set values =
  if Array.length values = 0 then nil else Lg_edn_backend.Set values

let parents hierarchy tag =
  non_empty_set (values_for (relation hierarchy "parents") tag)

let ancestors hierarchy tag =
  non_empty_set (values_for (relation hierarchy "ancestors") tag)

let descendants hierarchy tag =
  non_empty_set (values_for (relation hierarchy "descendants") tag)

let is_vector = function
  | Lg_edn_backend.Vector _ | Lg_edn_backend.Int4_vector _
  | Lg_edn_backend.Int4_array _ | Lg_edn_backend.Int_vector _ ->
      true
  | _ -> false

let rec isa hierarchy child parent =
  equal child parent
  || contains (values_for (relation hierarchy "ancestors") child) parent
  ||
  if is_vector child && is_vector parent then
    match
      (Runtime_edn.sequence_values child, Runtime_edn.sequence_values parent)
    with
    | Some child_values, Some parent_values ->
        Array.length child_values = Array.length parent_values
        && Array.for_all2 (isa hierarchy) child_values parent_values
    | _ -> false
  else false

let named = function
  | Lg_edn_backend.Keyword _ | Lg_edn_backend.Symbol _ -> true
  | _ -> false

let qualified = function
  | Lg_edn_backend.Keyword name | Lg_edn_backend.Symbol name ->
      String.contains name '/'
  | _ -> false

let validate_global tag parent =
  if not (qualified parent) then
    invalid_arg "derive parent must be namespace-qualified";
  if not (qualified tag) then
    invalid_arg "derive tag must be namespace-qualified"

let transitive_update relation_entries source sources target targets =
  let affected =
    append_unique [| source |] (values_for sources source)
  in
  let additions =
    append_unique [| target |] (values_for targets target)
  in
  Array.fold_left
    (fun entries key ->
      let values = append_unique (values_for targets key) additions in
      assoc entries key (Lg_edn_backend.Set values))
    relation_entries affected

let derive hierarchy tag parent =
  if equal tag parent then invalid_arg "cannot derive a tag from itself";
  if not (named tag) then invalid_arg "derive tag must be a named EDN value";
  if not (named parent) then
    invalid_arg "derive parent must be a named EDN value";
  let parent_map = relation hierarchy "parents" in
  let descendant_map = relation hierarchy "descendants" in
  let ancestor_map = relation hierarchy "ancestors" in
  if contains (values_for parent_map tag) parent then hierarchy
  else if contains (values_for ancestor_map tag) parent then
    invalid_arg "derive parent is already an ancestor"
  else if contains (values_for ancestor_map parent) tag then
    invalid_arg "cyclic derivation"
  else
    let parent_map =
      assoc parent_map tag
        (Lg_edn_backend.Set
           (append_unique (values_for parent_map tag) [| parent |]))
    in
    let original_ancestor_map = ancestor_map in
    let ancestor_map =
      transitive_update ancestor_map tag descendant_map parent ancestor_map
    in
    let descendant_map =
      transitive_update descendant_map parent original_ancestor_map tag
        descendant_map
    in
    with_relations parent_map descendant_map ancestor_map

let underive hierarchy tag parent =
  let parent_map = relation hierarchy "parents" in
  let direct = values_for parent_map tag in
  if not (contains direct parent) then hierarchy
  else
    let remaining =
      direct |> Array.to_list
      |> List.filter (fun candidate -> not (equal candidate parent))
      |> Array.of_list
    in
    let parent_map =
      if Array.length remaining = 0 then dissoc parent_map tag
      else assoc parent_map tag (Lg_edn_backend.Set remaining)
    in
    Array.fold_left
      (fun hierarchy (child, parents) ->
        Array.fold_left
          (fun hierarchy parent -> derive hierarchy child parent)
          hierarchy (set_values parents))
      (make ()) parent_map

let global = ref (make ())

let global_isa child parent = isa !global child parent
let global_parents tag = parents !global tag
let global_ancestors tag = ancestors !global tag
let global_descendants tag = descendants !global tag

let global_derive tag parent =
  validate_global tag parent;
  global := derive !global tag parent;
  None

let global_underive tag parent =
  global := underive !global tag parent;
  None
