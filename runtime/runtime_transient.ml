let ensure_active active =
  if not active then invalid_arg "transient used after persistent!"

type key_kind = Generic | Dynamic

type 'key key_operations = {
  hash : 'key -> int;
  equal : 'key -> 'key -> bool;
}

let generic_key_operations () =
  {
    hash = Runtime_static_value.hash;
    equal = Runtime_static_value.equal;
  }

let dynamic_key_operations =
  { hash = Runtime_dynamic.hash; equal = Runtime_dynamic.equal }

type 'value set = {
  mutable buckets : 'value list array;
  mutable size : int;
  mutable key_kind : key_kind option;
  mutable key_operations : 'value key_operations option;
  mutable active : bool;
}

let set_empty () =
  {
    buckets = Array.make 16 [];
    size = 0;
    key_kind = None;
    key_operations = None;
    active = true;
  }

let activate_set_key_operations set kind operations =
  match (set.key_kind, set.key_operations) with
  | None, None ->
      set.key_kind <- Some kind;
      set.key_operations <- Some operations;
      operations
  | Some existing_kind, Some existing_operations when existing_kind = kind ->
      existing_operations
  | Some _, Some _ -> invalid_arg "transient set element type changed"
  | _ -> invalid_arg "invalid transient set key state"

let set_bucket_index operations buckets value =
  (operations.hash value land max_int) mod Array.length buckets

let set_add_without_resize set operations value =
  let index = set_bucket_index operations set.buckets value in
  if not (List.exists (operations.equal value) set.buckets.(index)) then (
    set.buckets.(index) <- value :: set.buckets.(index);
    set.size <- set.size + 1)

let resize_set_if_needed set operations =
  if set.size * 4 > Array.length set.buckets * 3 then (
    let old_buckets = set.buckets in
    set.buckets <- Array.make (Array.length set.buckets * 2) [];
    set.size <- 0;
    Array.iter
      (List.iter (set_add_without_resize set operations))
      old_buckets)

let set_add_by kind operations set value =
  ensure_active set.active;
  let operations = activate_set_key_operations set kind operations in
  set_add_without_resize set operations value;
  resize_set_if_needed set operations;
  set

let set_of_list values =
  List.fold_left
    (set_add_by Generic (generic_key_operations ()))
    (set_empty ()) values

let set_of_list_dynamic values =
  List.fold_left (set_add_by Dynamic dynamic_key_operations) (set_empty ())
    values

let set_mem set value =
  ensure_active set.active;
  let operations =
    activate_set_key_operations set Generic (generic_key_operations ())
  in
  let index = set_bucket_index operations set.buckets value in
  List.exists (operations.equal value) set.buckets.(index)

let set_mem_dynamic set value =
  ensure_active set.active;
  let operations =
    activate_set_key_operations set Dynamic dynamic_key_operations
  in
  let index = set_bucket_index operations set.buckets value in
  List.exists (operations.equal value) set.buckets.(index)

let set_count set =
  ensure_active set.active;
  set.size

let set_add set value =
  set_add_by Generic (generic_key_operations ()) set value

let set_add_dynamic set value =
  set_add_by Dynamic dynamic_key_operations set value

let set_to_seq set =
  ensure_active set.active;
  let values = Array.to_seq set.buckets |> Seq.flat_map List.to_seq in
  set.active <- false;
  values

type 'value vector = {
  mutable reversed : 'value list;
  mutable active : bool;
}

let vector_empty () = { reversed = []; active = true }

let vector_of_list values = { reversed = List.rev values; active = true }

let vector_count vector =
  ensure_active vector.active;
  List.length vector.reversed

let vector_nth vector index =
  ensure_active vector.active;
  let reversed_index = List.length vector.reversed - index - 1 in
  if reversed_index < 0 then invalid_arg "nth index out of bounds"
  else List.nth vector.reversed reversed_index

let vector_add vector value =
  ensure_active vector.active;
  vector.reversed <- value :: vector.reversed;
  vector

let vector_assoc vector index value =
  ensure_active vector.active;
  let length = List.length vector.reversed in
  if index < 0 || index > length then invalid_arg "assoc! index out of bounds"
  else if index = length then vector_add vector value
  else (
    let reversed_index = length - index - 1 in
    vector.reversed <-
      List.mapi
        (fun current existing ->
          if current = reversed_index then value else existing)
        vector.reversed;
    vector)

let vector_persistent vector =
  ensure_active vector.active;
  let result = Rrbvec.of_list (List.rev vector.reversed) in
  vector.active <- false;
  result

type ('key, 'value) map = {
  mutable buckets : ('key * 'value) list array;
  mutable size : int;
  mutable key_kind : key_kind option;
  mutable key_operations : 'key key_operations option;
  mutable active : bool;
}

let map_empty () =
  {
    buckets = Array.make 16 [];
    size = 0;
    key_kind = None;
    key_operations = None;
    active = true;
  }

let activate_key_operations map kind operations =
  match (map.key_kind, map.key_operations) with
  | None, None ->
      map.key_kind <- Some kind;
      map.key_operations <- Some operations;
      operations
  | Some existing_kind, Some existing_operations when existing_kind = kind ->
      existing_operations
  | Some _, Some _ -> invalid_arg "transient map key type changed"
  | _ -> invalid_arg "invalid transient map key state"

let bucket_index operations buckets key =
  (operations.hash key land max_int) mod Array.length buckets

let add_without_resize map operations key value =
  let index = bucket_index operations map.buckets key in
  let rec replace prefix = function
    | [] ->
        map.size <- map.size + 1;
        List.rev_append prefix [ (key, value) ]
    | (existing_key, _) :: rest when operations.equal key existing_key ->
        List.rev_append prefix ((key, value) :: rest)
    | entry :: rest -> replace (entry :: prefix) rest
  in
  map.buckets.(index) <- replace [] map.buckets.(index)

let resize_if_needed map operations =
  if map.size * 4 > Array.length map.buckets * 3 then (
    let old_buckets = map.buckets in
    map.buckets <- Array.make (Array.length map.buckets * 2) [];
    map.size <- 0;
    Array.iter
      (List.iter (fun (key, value) ->
           add_without_resize map operations key value))
      old_buckets)

let map_assoc_by kind operations map key value =
  ensure_active map.active;
  let operations = activate_key_operations map kind operations in
  add_without_resize map operations key value;
  resize_if_needed map operations;
  map

let map_assoc map key value =
  map_assoc_by Generic (generic_key_operations ()) map key value

let map_assoc_dynamic
    (map : (Runtime_dynamic.t, 'value) map)
    (key : Runtime_dynamic.t) value =
  map_assoc_by Dynamic dynamic_key_operations map key value

let map_of_list entries =
  List.fold_left
    (fun map (key, value) -> map_assoc map key value)
    (map_empty ()) entries

let map_of_list_dynamic entries =
  List.fold_left
    (fun map (key, value) -> map_assoc_dynamic map key value)
    (map_empty ()) entries

let map_count map =
  ensure_active map.active;
  map.size

let map_get_option_by kind operations map key =
  ensure_active map.active;
  let operations = activate_key_operations map kind operations in
  let index = bucket_index operations map.buckets key in
  map.buckets.(index)
  |> List.find_opt (fun (existing_key, _) -> operations.equal key existing_key)
  |> Option.map snd

let map_get_option map key =
  map_get_option_by Generic (generic_key_operations ()) map key

let map_get_option_dynamic
    (map : (Runtime_dynamic.t, 'value) map)
    (key : Runtime_dynamic.t) =
  map_get_option_by Dynamic dynamic_key_operations map key

let map_get_default map key default =
  match map_get_option map key with Some value -> value | None -> default

let map_get_default_dynamic map key default =
  match map_get_option_dynamic map key with
  | Some value -> value
  | None -> default

let map_dissoc_by kind operations map key =
  ensure_active map.active;
  let operations = activate_key_operations map kind operations in
  let index = bucket_index operations map.buckets key in
  let rec remove removed prefix = function
    | [] ->
        if removed then map.size <- map.size - 1;
        List.rev prefix
    | (existing_key, _) :: rest when operations.equal key existing_key ->
        remove true prefix rest
    | entry :: rest -> remove removed (entry :: prefix) rest
  in
  map.buckets.(index) <- remove false [] map.buckets.(index);
  map

let map_dissoc map key =
  map_dissoc_by Generic (generic_key_operations ()) map key

let map_dissoc_dynamic
    (map : (Runtime_dynamic.t, 'value) map)
    (key : Runtime_dynamic.t) =
  map_dissoc_by Dynamic dynamic_key_operations map key

let map_persistent_by kind operations assoc map =
  ensure_active map.active;
  ignore (activate_key_operations map kind operations);
  let result =
    Array.fold_left
      (fun result entries ->
        List.fold_left
          (fun result (key, value) -> assoc result key value)
          result entries)
      Runtime_map.empty map.buckets
  in
  map.active <- false;
  result

let map_persistent map =
  map_persistent_by Generic (generic_key_operations ()) Runtime_map.assoc map

let map_persistent_dynamic
    (map : (Runtime_dynamic.t, 'value) map) =
  map_persistent_by Dynamic dynamic_key_operations Runtime_map.assoc_dynamic map
