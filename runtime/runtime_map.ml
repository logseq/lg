type 'key node =
  | Empty
  | Leaf of int * ('key * int) list
  | Branch of int * 'key node array

type ('key, 'value) t = {
  index : 'key node;
  entries : ('key * 'value) option Rrbvec.t;
  size : int;
}

type 'key operations = {
  hash : 'key -> int;
  equal : 'key -> 'key -> bool;
}

let empty = { index = Empty; entries = Rrbvec.empty; size = 0 }

let dynamic_key_equal left right =
  Runtime_dynamic.equal left right || Runtime_dynamic.equal right left

let dynamic_operations =
  { hash = Runtime_dynamic.hash; equal = dynamic_key_equal }

let generic_operations =
  {
    hash = Runtime_static_value.hash;
    equal = Runtime_static_value.equal;
  }

let slot hash shift = (hash lsr shift) land 31

let bitmap_position bitmap bit =
  Runtime_int.popcount_32 (bitmap land (bit - 1))

let array_insert values index value =
  let length = Array.length values in
  Array.init (length + 1) (fun current ->
      if current < index then values.(current)
      else if current = index then value
      else values.(current - 1))

let replace_position equal key position positions =
  let rec replace prefix = function
    | [] -> List.rev ((key, position) :: prefix)
    | (existing_key, _) :: rest when equal key existing_key ->
        List.rev_append prefix ((key, position) :: rest)
    | entry :: rest -> replace (entry :: prefix) rest
  in
  replace [] positions

let find_position_in_hash operations index key key_hash =
  let rec find shift = function
    | Empty -> None
    | Leaf (existing_hash, positions) ->
        if key_hash <> existing_hash then None
        else
          List.find_opt
            (fun (existing_key, _) -> operations.equal key existing_key)
            positions
          |> Option.map snd
    | Branch (bitmap, children) ->
        let bit = 1 lsl slot key_hash shift in
        if bitmap land bit = 0 then None
        else find (shift + 5) children.(bitmap_position bitmap bit)
  in
  find 0 index

let find_position_in operations index key =
  find_position_in_hash operations index key (operations.hash key)

let insert_position_hash operations index key key_hash position =
  let rec insert shift = function
    | Empty -> Leaf (key_hash, [ (key, position) ])
    | Leaf (existing_hash, positions) as unchanged ->
        if key_hash = existing_hash then
          Leaf
            (existing_hash, replace_position operations.equal key position positions)
        else
          let existing_slot = slot existing_hash shift in
          let key_slot = slot key_hash shift in
          if existing_slot = key_slot then
            Branch (1 lsl existing_slot, [| insert (shift + 5) unchanged |])
          else
            let existing_bit = 1 lsl existing_slot in
            let key_bit = 1 lsl key_slot in
            let inserted = Leaf (key_hash, [ (key, position) ]) in
            let children =
              if existing_slot < key_slot then [| unchanged; inserted |]
              else [| inserted; unchanged |]
            in
            Branch (existing_bit lor key_bit, children)
    | Branch (bitmap, children) as unchanged ->
        let bit = 1 lsl slot key_hash shift in
        let child_position = bitmap_position bitmap bit in
        if bitmap land bit = 0 then
          Branch
            ( bitmap lor bit,
              array_insert children child_position
                (Leaf (key_hash, [ (key, position) ])) )
        else
          let child = insert (shift + 5) children.(child_position) in
          if child == children.(child_position) then unchanged
          else
            let updated = Array.copy children in
            updated.(child_position) <- child;
            Branch (bitmap, updated)
  in
  insert 0 index

let insert_position operations index key position =
  insert_position_hash operations index key (operations.hash key) position

let find_position_in_entries operations entries key =
  let length = Rrbvec.length entries in
  let rec find index =
    if index = length then None
    else
      match Rrbvec.nth entries index with
      | Some (existing_key, _) when operations.equal key existing_key ->
          Some index
      | Some _ | None -> find (index + 1)
  in
  find 0

let index_entries operations entries =
  Rrbvec.fold_left
    (fun (index, position) entry ->
      let index =
        match entry with
        | Some (key, _) -> insert_position operations index key position
        | None -> index
      in
      (index, position + 1))
    (Empty, 0) entries
  |> fst

let find_position operations map key =
  match map.index with
  | Empty when map.size > 0 ->
      find_position_in_entries operations map.entries key
  | Empty | Leaf _ | Branch _ ->
      find_position_in operations map.index key

let assoc_by operations map key value =
  let key_hash = operations.hash key in
  let existing_position =
    match map.index with
    | Empty when map.size > 0 ->
        find_position_in_entries operations map.entries key
    | Empty | Leaf _ | Branch _ ->
        find_position_in_hash operations map.index key key_hash
  in
  match existing_position with
  | Some position ->
      { map with entries = Rrbvec.set map.entries position (Some (key, value)) }
  | None ->
      let position = Rrbvec.length map.entries in
      let entries = Rrbvec.push_back map.entries (Some (key, value)) in
      {
        index =
          insert_position_hash operations
            (match map.index with
            | Empty when map.size > 0 ->
                index_entries operations map.entries
            | Empty | Leaf _ | Branch _ -> map.index)
            key key_hash position;
        entries;
        size = map.size + 1;
      }

let assoc map key value = assoc_by generic_operations map key value
let assoc_dynamic map key value = assoc_by dynamic_operations map key value

let assoc_small_string map key value =
  match map.index with
  | Leaf _ | Branch _ -> assoc map key value
  | Empty ->
      let length = Rrbvec.length map.entries in
      let rec find index =
        if index = length then None
        else
          match Rrbvec.nth map.entries index with
          | Some (existing_key, _) when String.equal key existing_key ->
              Some index
          | Some _ | None -> find (index + 1)
      in
      (match find 0 with
      | Some position ->
          {
            map with
            entries =
              Rrbvec.set map.entries position (Some (key, value));
          }
      | None ->
          {
            index = Empty;
            entries =
              Rrbvec.push_back map.entries (Some (key, value));
            size = map.size + 1;
          })

let of_list entries =
  List.fold_left (fun map (key, value) -> assoc map key value) empty entries

let of_list_dynamic entries =
  List.fold_left
    (fun map (key, value) -> assoc_dynamic map key value)
    empty entries

let zipmap_by assoc keys values =
  let rec build map keys values =
    match (keys (), values ()) with
    | Seq.Cons (key, remaining_keys), Seq.Cons (value, remaining_values) ->
        build (assoc map key value) remaining_keys remaining_values
    | Seq.Nil, _ | _, Seq.Nil -> map
  in
  build empty keys values

let zipmap keys values = zipmap_by assoc keys values
let zipmap_dynamic keys values = zipmap_by assoc_dynamic keys values

let array_remove values index =
  let length = Array.length values in
  Array.init (length - 1) (fun current ->
      if current < index then values.(current) else values.(current + 1))

let remove_position operations index key =
  let key_hash = operations.hash key in
  let rec remove shift = function
    | Empty -> Empty
    | Leaf (existing_hash, positions) as unchanged ->
        if key_hash <> existing_hash then unchanged
        else
          let remaining =
            List.filter
              (fun (existing_key, _) ->
                not (operations.equal key existing_key))
              positions
          in
          if List.length remaining = List.length positions then unchanged
          else if remaining = [] then Empty
          else Leaf (existing_hash, remaining)
    | Branch (bitmap, children) as unchanged ->
        let bit = 1 lsl slot key_hash shift in
        if bitmap land bit = 0 then unchanged
        else
          let child_position = bitmap_position bitmap bit in
          let child = remove (shift + 5) children.(child_position) in
          if child == children.(child_position) then unchanged
          else
            match child with
            | Empty ->
                if Array.length children = 1 then Empty
                else
                  Branch
                    (bitmap land lnot bit, array_remove children child_position)
            | Leaf _ | Branch _ ->
                let updated = Array.copy children in
                updated.(child_position) <- child;
                Branch (bitmap, updated)
  in
  remove 0 index

let dissoc_by operations map key =
  match find_position operations map key with
  | None -> map
  | Some position ->
      {
        index =
          (match map.index with
          | Empty -> Empty
          | Leaf _ | Branch _ ->
              remove_position operations map.index key);
        entries = Rrbvec.set map.entries position None;
        size = map.size - 1;
      }

let dissoc map key = dissoc_by generic_operations map key
let dissoc_dynamic map key = dissoc_by dynamic_operations map key

let find_by operations map key =
  Option.bind (find_position operations map key) (fun position ->
      Rrbvec.nth map.entries position)

let find map key = find_by generic_operations map key
let find_dynamic map key = find_by dynamic_operations map key

let get_option_by operations map key =
  find_by operations map key |> Option.map snd

let get_option map key = get_option_by generic_operations map key
let get_option_dynamic map key = get_option_by dynamic_operations map key

let get_default map key default =
  match get_option map key with Some value -> value | None -> default

let get_default_dynamic map key default =
  match get_option_dynamic map key with Some value -> value | None -> default

let get_option_default map key default =
  match get_option map key with Some value -> Some value | None -> default

let get_option_default_dynamic map key default =
  match get_option_dynamic map key with Some value -> Some value | None -> default

let mem map key = Option.is_some (get_option map key)
let mem_dynamic map key = Option.is_some (get_option_dynamic map key)

let group_by key_fn sequence =
  Seq.fold_left
    (fun groups item ->
      let key = key_fn item in
      let items = get_default groups key Rrbvec.empty in
      assoc groups key (Rrbvec.push_back items item))
    empty sequence

let with_record_metadata map key metadata =
  if Runtime_dynamic.is_nil metadata then dissoc map key
  else assoc map key metadata

let select_keys_by find assoc map keys =
  Seq.fold_left
    (fun selected key ->
      match find map key with
      | Some (existing_key, value) -> assoc selected existing_key value
      | None -> selected)
    empty keys

let select_keys map keys = select_keys_by find assoc map keys

let select_keys_dynamic map keys =
  select_keys_by find_dynamic assoc_dynamic map keys

let select_options lookup keys =
  Seq.fold_left
    (fun selected key ->
      match lookup key with
      | Some value -> assoc selected key value
      | None -> selected)
    empty keys

let count map = map.size

let fold_left fn accumulator map =
  Rrbvec.fold_left
    (fun accumulator entry ->
      match entry with Some entry -> fn accumulator entry | None -> accumulator)
    accumulator map.entries

let merge left right =
  fold_left (fun result (key, value) -> assoc result key value) left right

let to_list map =
  let length = Rrbvec.length map.entries in
  let rec collect index entries =
    if index = length then List.rev entries
    else
      match Rrbvec.nth map.entries index with
      | Some entry -> collect (index + 1) (entry :: entries)
      | None -> collect (index + 1) entries
  in
  collect 0 []

let to_seq map =
  let length = Rrbvec.length map.entries in
  let rec next index () =
    if index >= length then Seq.Nil
    else
      match Rrbvec.nth map.entries index with
      | Some entry -> Seq.Cons (entry, next (index + 1))
      | None -> next (index + 1) ()
  in
  next 0

let first_opt map =
  match to_seq map () with
  | Seq.Cons (entry, _) -> Some entry
  | Seq.Nil -> None
