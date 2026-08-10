type value = Lg_edn_backend.t

type partition = Atom | Map | Set | Sequential

let nil = Lg_edn_backend.Nil

let triple left right both =
  Lg_edn_backend.Vector [| left; right; both |]

let unpack_triple = function
  | Lg_edn_backend.Vector [| left; right; both |] -> (left, right, both)
  | _ -> invalid_arg "clojure.data diff result must contain three values"

let atom_diff left right =
  if Runtime_edn.equal left right then triple nil nil left
  else triple left right nil

let partition = function
  | Lg_edn_backend.Map _ -> Map
  | Lg_edn_backend.Set _ -> Set
  | ( Lg_edn_backend.List _ | Lg_edn_backend.Vector _
    | Lg_edn_backend.Int4_vector _ | Lg_edn_backend.Int4_array _
    | Lg_edn_backend.Int_vector _ ) ->
      Sequential
  | _ -> Atom

let equality_partition_tag value =
  match partition value with
  | Atom -> 0
  | Map -> 1
  | Set -> 2
  | Sequential -> 3

let contains_key entries key =
  Array.exists
    (fun (entry_key, _) -> Runtime_edn.equal entry_key key)
    entries

let get entries key =
  entries
  |> Array.find_map (fun (entry_key, value) ->
         if Runtime_edn.equal entry_key key then Some value else None)
  |> Option.value ~default:nil

let add_unique keys key =
  if List.exists (Runtime_edn.equal key) keys then keys else key :: keys

let non_empty_map entries =
  match entries with
  | [] -> nil
  | entries -> Lg_edn_backend.Map (Array.of_list entries)

let non_empty_set values =
  match values with
  | [] -> nil
  | values -> Lg_edn_backend.Set (Array.of_list values)

let sequence_values value =
  match Runtime_edn.sequence_values value with
  | Some values -> values
  | None -> invalid_arg "clojure.data expected a sequential EDN value"

let rec diff left right =
  if Runtime_edn.equal left right then triple nil nil left
  else
    let left_partition = partition left in
    if left_partition <> partition right then atom_diff left right
    else
      match left_partition with
      | Map -> diff_maps left right
      | Sequential -> diff_sequential left right
      | Set -> diff_sets left right
      | Atom -> atom_diff left right

and diff_maps left right =
  match (left, right) with
  | Lg_edn_backend.Map left_entries, Lg_edn_backend.Map right_entries ->
      let keys =
        Array.fold_left
          (fun keys (key, _) -> add_unique keys key)
          [] left_entries
        |> fun reversed ->
        Array.fold_left
          (fun keys (key, _) -> add_unique keys key)
          reversed right_entries
        |> List.rev
      in
      let left_only, right_only, common =
        List.fold_left
          (fun (left_only, right_only, common) key ->
            let left_value = get left_entries key in
            let right_value = get right_entries key in
            let left_diff, right_diff, both = diff left_value right_value |> unpack_triple in
            let in_left = contains_key left_entries key in
            let in_right = contains_key right_entries key in
            let same =
              in_left && in_right
              && ((not (Runtime_edn.is_nil both))
                 || (Runtime_edn.is_nil left_value
                    && Runtime_edn.is_nil right_value))
            in
            let left_only =
              if
                in_left
                && ((not (Runtime_edn.is_nil left_diff)) || not same)
              then (key, left_diff) :: left_only
              else left_only
            in
            let right_only =
              if
                in_right
                && ((not (Runtime_edn.is_nil right_diff)) || not same)
              then (key, right_diff) :: right_only
              else right_only
            in
            let common = if same then (key, both) :: common else common in
            (left_only, right_only, common))
          ([], [], []) keys
      in
      triple
        (non_empty_map (List.rev left_only))
        (non_empty_map (List.rev right_only))
        (non_empty_map (List.rev common))
  | _ -> atom_diff left right

and diff_sequential left right =
  let left_values = sequence_values left in
  let right_values = sequence_values right in
  let indexed values =
    Array.mapi
      (fun index value -> (Lg_edn_backend.Small_int index, value))
      values
  in
  let left_only, right_only, common =
    diff_maps
      (Lg_edn_backend.Map (indexed left_values))
      (Lg_edn_backend.Map (indexed right_values))
    |> unpack_triple
  in
  triple (vectorize left_only) (vectorize right_only) (vectorize common)

and vectorize = function
  | Lg_edn_backend.Nil -> nil
  | Lg_edn_backend.Map entries ->
      let indexed =
        Array.map
          (fun (key, value) ->
            match key with
            | Lg_edn_backend.Small_int index -> (index, value)
            | Lg_edn_backend.Int index -> (Int64.to_int index, value)
            | _ ->
                invalid_arg
                  "clojure.data sequential diff index must be an integer")
          entries
      in
      let max_index =
        Array.fold_left
          (fun maximum (index, _) -> max maximum index)
          0 indexed
      in
      let values = Array.make (max_index + 1) nil in
      Array.iter (fun (index, value) -> values.(index) <- value) indexed;
      Lg_edn_backend.Vector values
  | _ -> invalid_arg "clojure.data sequential diff must be a map"

and diff_sets left right =
  match (left, right) with
  | Lg_edn_backend.Set left_values, Lg_edn_backend.Set right_values ->
      let contains values value = Array.exists (Runtime_edn.equal value) values in
      let difference values other =
        values |> Array.to_list
        |> List.filter (fun value -> not (contains other value))
      in
      let intersection =
        left_values |> Array.to_list
        |> List.filter (contains right_values)
      in
      triple
        (non_empty_set (difference left_values right_values))
        (non_empty_set (difference right_values left_values))
        (non_empty_set intersection)
  | _ -> atom_diff left right

let diff_similar left right =
  match partition left with
  | Map -> diff_maps left right
  | Sequential -> diff_sequential left right
  | Set -> diff_sets left right
  | Atom -> atom_diff left right
