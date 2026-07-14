let equality_partition_protocol = "clojure.data/EqualityPartition"
let diff_protocol = "clojure.data/Diff"

let nil = Runtime_dynamic.nil
let is_nil value = value.Runtime_dynamic.payload = Runtime_dynamic.Nil

let triple left right both =
  Runtime_dynamic.vector (Rrbvec.of_list [ left; right; both ])

let unpack_triple value =
  match List.of_seq (Runtime_dynamic.to_seq value) with
  | [ left; right; both ] -> (left, right, both)
  | _ -> invalid_arg "clojure.data diff result must contain three values"

let atom_diff left right =
  if Runtime_dynamic.equal left right then triple nil nil left
  else triple left right nil

let partition value =
  if Runtime_dynamic.has_protocol value equality_partition_protocol then
    Runtime_dynamic.invoke value equality_partition_protocol
      "equality-partition" []
  else
    let name =
      match value.Runtime_dynamic.payload with
      | Runtime_dynamic.Set _ -> ":set"
      | Runtime_dynamic.List | Runtime_dynamic.Vector | Runtime_dynamic.Seq ->
          ":sequential"
      | Runtime_dynamic.Map _ -> ":map"
      | _ -> ":atom"
    in
    Runtime_dynamic.keyword name

let contains_key entries key =
  List.exists
    (fun (entry_key, _) -> Runtime_dynamic.equal entry_key key)
    entries

let get entries key =
  entries
  |> List.find_opt (fun (entry_key, _) -> Runtime_dynamic.equal entry_key key)
  |> Option.map snd |> Option.value ~default:nil

let add_unique keys key =
  if List.exists (Runtime_dynamic.equal key) keys then keys else keys @ [ key ]

let non_empty_map entries =
  match entries with [] -> nil | _ -> Runtime_dynamic.map entries

let non_empty_set values =
  match values with
  | [] -> nil
  | _ -> Runtime_dynamic.set (List.to_seq values)

let rec diff left right =
  if Runtime_dynamic.equal left right then triple nil nil left
  else
    let left_partition = partition left in
    let right_partition = partition right in
    if not (Runtime_dynamic.equal left_partition right_partition) then
      atom_diff left right
    else if Runtime_dynamic.has_protocol left diff_protocol then
      Runtime_dynamic.invoke left diff_protocol "diff-similar" [ right ]
    else
      match left_partition.Runtime_dynamic.payload with
      | Runtime_dynamic.Keyword ":map" -> diff_maps left right
      | Runtime_dynamic.Keyword ":sequential" -> diff_sequential left right
      | Runtime_dynamic.Keyword ":set" -> diff_sets left right
      | _ -> atom_diff left right

and diff_maps left right =
  match (left.Runtime_dynamic.payload, right.Runtime_dynamic.payload) with
  | Runtime_dynamic.Map left_entries, Runtime_dynamic.Map right_entries ->
      let keys =
        List.fold_left
          (fun keys (key, _) -> add_unique keys key)
          (List.map fst left_entries) right_entries
      in
      let left_only, right_only, common =
        List.fold_left
          (fun (left_only, right_only, common) key ->
            let left_value = get left_entries key in
            let right_value = get right_entries key in
            let left_diff, right_diff, both =
              unpack_triple (diff left_value right_value)
            in
            let in_left = contains_key left_entries key in
            let in_right = contains_key right_entries key in
            let same =
              in_left && in_right
              && ((not (is_nil both))
                 || (is_nil left_value && is_nil right_value))
            in
            let left_only =
              if in_left && ((not (is_nil left_diff)) || not same) then
                (key, left_diff) :: left_only
              else left_only
            in
            let right_only =
              if in_right && ((not (is_nil right_diff)) || not same) then
                (key, right_diff) :: right_only
              else right_only
            in
            let common =
              if same then (key, both) :: common else common
            in
            (left_only, right_only, common))
          ([], [], []) keys
      in
      triple
        (non_empty_map (List.rev left_only))
        (non_empty_map (List.rev right_only))
        (non_empty_map (List.rev common))
  | _ -> atom_diff left right

and diff_sequential left right =
  let left_values = List.of_seq (Runtime_dynamic.to_seq left) in
  let right_values = List.of_seq (Runtime_dynamic.to_seq right) in
  let indexed values =
    List.mapi (fun index value -> (Runtime_dynamic.int index, value)) values
  in
  let left_map = Runtime_dynamic.map (indexed left_values) in
  let right_map = Runtime_dynamic.map (indexed right_values) in
  let left_only, right_only, common =
    unpack_triple (diff_maps left_map right_map)
  in
  triple (vectorize left_only) (vectorize right_only) (vectorize common)

and vectorize value =
  match value.Runtime_dynamic.payload with
  | Runtime_dynamic.Nil -> nil
  | Runtime_dynamic.Map entries ->
      let indexed =
        entries
        |> List.map (fun (key, value) ->
               match key.Runtime_dynamic.payload with
               | Runtime_dynamic.Int index -> (index, value)
               | _ -> invalid_arg "clojure.data sequential diff index must be int")
      in
      let max_index =
        List.fold_left (fun maximum (index, _) -> max maximum index) 0 indexed
      in
      let values = Array.make (max_index + 1) nil in
      List.iter (fun (index, value) -> values.(index) <- value) indexed;
      Runtime_dynamic.vector (Rrbvec.of_list (Array.to_list values))
  | _ -> invalid_arg "clojure.data sequential diff must be a map"

and diff_sets left right =
  match (left.Runtime_dynamic.payload, right.Runtime_dynamic.payload) with
  | Runtime_dynamic.Set left_values, Runtime_dynamic.Set right_values ->
      let difference values other =
        List.filter
          (fun value ->
            not (List.exists (Runtime_dynamic.equal value) other))
          values
      in
      let intersection =
        List.filter
          (fun value -> List.exists (Runtime_dynamic.equal value) right_values)
          left_values
      in
      triple
        (non_empty_set (difference left_values right_values))
        (non_empty_set (difference right_values left_values))
        (non_empty_set intersection)
  | _ -> atom_diff left right
