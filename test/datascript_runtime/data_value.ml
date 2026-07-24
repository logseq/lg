type entity_ref =
  | Entity_id of int
  | Temp_id of string
  | Current_tx
  | Ident of string
  | Lookup_ref of string * t

and t =
  | Nil
  | Int of int
  | Float of float
  | String of string
  | Symbol of string
  | Bool of bool
  | Keyword of string
  | Uuid of string
  | Instant of int
  | Regex of string
  | Ref of int
  | List of t list
  | Vector of t list
  | Map of (t * t) list
  | Set of t list
  | Tuple of t option list
  | Tx_ref
  | Ref_to of entity_ref

let tuple_of_vector values = Tuple (Rrbvec.to_list values)
let set_of_vector values = Set (Rrbvec.to_list values)
let vector_of_vector values = Vector (Rrbvec.to_list values)
let vector_of_vector_with convert values =
  Vector
    (Rrbvec.fold_right
       (fun value converted -> convert value :: converted)
       values [])

let is_nil = function Nil -> true | _ -> false

let map_of_keyword_map values =
  Map
    (Lg_runtime.Runtime_map.to_list values
    |> List.map (fun (key, value) -> (Keyword key, value)))

let map_of_keyword_map_with convert values =
  Map
    (Lg_runtime.Runtime_map.to_list values
    |> List.map (fun (key, value) -> (Keyword key, convert value)))

let keyword_map_get key = function
  | Map entries ->
      List.find_map
        (function
          | Keyword candidate, value when String.equal candidate key -> Some value
          | _ -> None)
        entries
  | _ -> None

let keyword_map_value = function
  | Map entries ->
      List.fold_left
        (fun result (key, value) ->
          match (result, key) with
          | Some values, Keyword key ->
              Some (Lg_runtime.Runtime_map.assoc values key value)
          | _ -> None)
        (Some Lg_runtime.Runtime_map.empty)
        entries
  | _ -> None

let string_vector values =
  Vector (values |> Rrbvec.to_list |> List.map (fun value -> String value))

let temp_id_vector values =
  Vector
    (values |> Rrbvec.to_list
    |> List.map (fun value -> Ref_to (Temp_id value)))

let tuple_items = function
  | Tuple values -> Some (Rrbvec.of_list values)
  | _ -> None

let keyword_value = function
  | Keyword value -> Some value
  | _ -> None

let bool_value = function Bool value -> Some value | _ -> None

let sequential_items = function
  | List values | Vector values -> Some (Rrbvec.of_list values)
  | _ -> None

let set_items = function
  | Set values -> Some (Rrbvec.of_list values)
  | _ -> None

let entity_ref_value = function
  | Ref_to entity_ref -> Some entity_ref
  | Int eid | Ref eid -> Some (Entity_id eid)
  | _ -> None

let ref_value = function Ref eid -> Some eid | _ -> None

let tuple_parts attrs ref_attrs value =
  match value with
  | Tuple items ->
      let attrs = Rrbvec.to_list attrs in
      let ref_attrs = Rrbvec.to_list ref_attrs in
      if List.length attrs <> List.length items then
        invalid_arg "tuple value has the wrong arity"
      else (attrs, ref_attrs, items)
  | _ -> invalid_arg "expected a DataScript tuple value"

let tuple_entity_refs attrs ref_attrs value =
  let attrs, ref_attrs, items = tuple_parts attrs ref_attrs value in
  List.fold_left2
    (fun refs attr item ->
      if List.mem attr ref_attrs then
        match Option.bind item entity_ref_value with
        | Some entity_ref -> entity_ref :: refs
        | None -> invalid_arg "tuple ref item must be an entity reference"
      else refs)
    [] attrs items
  |> List.rev |> Rrbvec.of_list

let resolve_tuple_refs attrs ref_attrs eids value =
  let attrs, ref_attrs, items = tuple_parts attrs ref_attrs value in
  let eids = ref (Rrbvec.to_list eids) in
  let resolve_item attr item =
    if List.mem attr ref_attrs then
      match (!eids, item) with
      | eid :: rest, Some _ ->
          eids := rest;
          Some (Ref eid)
      | [], Some _ -> invalid_arg "tuple ref resolution is missing an entity id"
      | _, None -> None
    else item
  in
  let resolved = Tuple (List.map2 resolve_item attrs items) in
  if !eids <> [] then invalid_arg "tuple ref resolution has extra entity ids";
  resolved

let keyword_items value =
  let values =
    match value with
    | List values | Vector values -> Some values
    | _ -> None
  in
  Option.bind values (fun values ->
      let rec collect keywords = function
        | [] -> Some (Rrbvec.of_list (List.rev keywords))
        | Keyword value :: rest -> collect (value :: keywords) rest
        | _ -> None
      in
      collect [] values)

let rec list_equal equal left right =
  match (left, right) with
  | [], [] -> true
  | left :: left_rest, right :: right_rest ->
      equal left right && list_equal equal left_rest right_rest
  | [], _ | _, [] -> false

let option_equal equal left right =
  match (left, right) with
  | None, None -> true
  | Some left, Some right -> equal left right
  | None, Some _ | Some _, None -> false

let sequence = function
  | List values | Vector values ->
      Some (List.map (fun value -> Some value) values)
  | Tuple values -> Some values
  | _ -> None

let rec equal left right =
  match (left, right) with
  | Nil, Nil | Tx_ref, Tx_ref -> true
  | Int left, Int right | Ref left, Ref right | Instant left, Instant right ->
      left = right
  | Int left, Ref right | Ref left, Int right -> left = right
  | Float left, Float right -> Float.equal left right
  | Int left, Float right | Ref left, Float right ->
      Float.equal (float_of_int left) right
  | Float left, Int right | Float left, Ref right ->
      Float.equal left (float_of_int right)
  | String left, String right
  | Symbol left, Symbol right
  | Keyword left, Keyword right
  | Uuid left, Uuid right
  | Regex left, Regex right ->
      String.equal left right
  | Bool left, Bool right -> Bool.equal left right
  | Map left, Map right -> map_equal left right
  | Set left, Set right -> set_equal left right
  | Ref_to left, Ref_to right -> entity_ref_equal left right
  | _ -> (
      match (sequence left, sequence right) with
      | Some left, Some right -> list_equal (option_equal equal) left right
      | _ -> false)

and map_equal left right =
  List.length left = List.length right
  && List.for_all
       (fun (left_key, left_value) ->
         List.exists
           (fun (right_key, right_value) ->
             equal left_key right_key && equal left_value right_value)
           right)
       left

and set_equal left right =
  List.length left = List.length right
  && List.for_all
       (fun left_value -> List.exists (equal left_value) right)
       left

and entity_ref_equal left right =
  match (left, right) with
  | Entity_id left, Entity_id right -> left = right
  | Temp_id left, Temp_id right | Ident left, Ident right -> String.equal left right
  | Current_tx, Current_tx -> true
  | Lookup_ref (left_attr, left_value), Lookup_ref (right_attr, right_value) ->
      String.equal left_attr right_attr && equal left_value right_value
  | _ -> false

let combine_hash seed value = (seed * 33) lxor value

let ordered_hash values =
  List.fold_left (fun result value -> combine_hash result value) 1 values

let unordered_hash values =
  List.fold_left (fun result value -> result + (value lxor (value lsl 16))) 0 values

let rec hash = function
  | Nil -> 0
  | Int value | Ref value -> Hashtbl.hash (float_of_int value)
  | Float value -> Hashtbl.hash value
  | String value -> Hashtbl.hash (0, value)
  | Symbol value -> Hashtbl.hash (1, value)
  | Bool value -> Hashtbl.hash (2, value)
  | Keyword value -> Hashtbl.hash (3, value)
  | Uuid value -> Hashtbl.hash (4, value)
  | Instant value -> Hashtbl.hash (5, value)
  | Regex value -> Hashtbl.hash (6, value)
  | List values | Vector values -> ordered_hash (List.map hash values)
  | Tuple values ->
      ordered_hash
        (List.map (function None -> 0 | Some value -> hash value) values)
  | Map entries ->
      unordered_hash
        (List.map
           (fun (key, value) -> ordered_hash [ hash key; hash value ])
           entries)
  | Set values -> unordered_hash (List.map hash values)
  | Tx_ref -> Hashtbl.hash 7
  | Ref_to entity_ref -> Hashtbl.hash (8, hash_entity_ref entity_ref)

and hash_entity_ref = function
  | Entity_id value -> Hashtbl.hash (0, value)
  | Temp_id value -> Hashtbl.hash (1, value)
  | Current_tx -> Hashtbl.hash 2
  | Ident value -> Hashtbl.hash (3, value)
  | Lookup_ref (attr, value) -> Hashtbl.hash (4, attr, hash value)

let identifier_offset value =
  if
    String.length value > 0
    && (String.unsafe_get value 0 = ':' || String.unsafe_get value 0 = '\'')
  then 1
  else 0

let compare_string_slice left left_start left_length right right_start
    right_length =
  let shared_length = min left_length right_length in
  let rec loop index =
    if index = shared_length then Int.compare left_length right_length
    else
      let compared =
        Char.compare
          (String.unsafe_get left (left_start + index))
          (String.unsafe_get right (right_start + index))
      in
      if compared = 0 then loop (index + 1) else compared
  in
  loop 0

let compare_identifier left right =
  if String.equal left right then 0
  else
    let left_offset = identifier_offset left in
    let right_offset = identifier_offset right in
    let left_separator =
      String.index_from_opt left left_offset '/'
    in
    let right_separator =
      String.index_from_opt right right_offset '/'
    in
    let left_namespace_length =
      match left_separator with
      | None -> 0
      | Some separator -> separator - left_offset
    in
    let right_namespace_length =
      match right_separator with
      | None -> 0
      | Some separator -> separator - right_offset
    in
    let namespace =
      compare_string_slice left left_offset left_namespace_length right
        right_offset right_namespace_length
    in
    if namespace <> 0 then namespace
    else
      let left_name_start =
        match left_separator with
        | None -> left_offset
        | Some separator -> separator + 1
      in
      let right_name_start =
        match right_separator with
        | None -> right_offset
        | Some separator -> separator + 1
      in
      compare_string_slice left left_name_start
        (String.length left - left_name_start)
        right right_name_start
        (String.length right - right_name_start)

let rec compare_list compare left right =
  let length = Int.compare (List.length left) (List.length right) in
  if length <> 0 then length
  else
    match (left, right) with
    | [], [] -> 0
    | left :: left_rest, right :: right_rest ->
        let current = compare left right in
        if current <> 0 then current else compare_list compare left_rest right_rest
    | [], _ | _, [] -> assert false

let compare_option left right =
  match (left, right) with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some left, Some right -> compare left right

let rank = function
  | Nil -> 0
  | Keyword _ -> 1
  | Symbol _ -> 2
  | Map _ -> 3
  | Set _ -> 4
  | List _ | Vector _ | Tuple _ -> 5
  | Bool _ -> 6
  | Int _ | Float _ | Ref _ -> 7
  | String _ -> 8
  | Regex _ -> 9
  | Instant _ -> 10
  | Uuid _ -> 11
  | Tx_ref -> 12
  | Ref_to _ -> 13

let compare left right =
  match (left, right) with
  | Int left, Int right | Ref left, Ref right | Instant left, Instant right ->
      Int.compare left right
  | Int left, Ref right | Ref left, Int right -> Int.compare left right
  | Float left, Float right -> Float.compare left right
  | Int left, Float right | Ref left, Float right ->
      Float.compare (float_of_int left) right
  | Float left, Int right | Float left, Ref right ->
      Float.compare left (float_of_int right)
  | String left, String right
  | Uuid left, Uuid right
  | Regex left, Regex right ->
      String.compare left right
  | Symbol left, Symbol right | Keyword left, Keyword right ->
      compare_identifier left right
  | Bool left, Bool right -> Bool.compare left right
  | Map _, Map _ | Set _, Set _ -> Int.compare (hash left) (hash right)
  | Ref_to left, Ref_to right -> Stdlib.compare left right
  | _ -> (
      match (sequence left, sequence right) with
      | Some left, Some right -> compare_list compare_option left right
      | _ -> Int.compare (rank left) (rank right))
