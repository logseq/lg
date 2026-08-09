type ('key, 'value) leaf = {
  hash : int;
  key : 'key;
  value : 'value;
  sequence_index : int;
}

type ('key, 'value) slot =
  | Leaf of ('key, 'value) leaf
  | Child of ('key, 'value) node

and ('key, 'value) node =
  | Bitmap_indexed_node of {
      bitmap : int;
      slots : ('key, 'value) slot array;
    }
  | Array_node of {
      count : int;
      children : ('key, 'value) node option array;
    }
  | Hash_collision_node of {
      hash : int;
      entries : ('key, 'value) leaf array;
    }

type ('key, 'value) t = {
  root : ('key, 'value) node option;
  sequence : ('key * 'value) option Rrbvec.t;
  size : int;
  metadata : Lg_edn_backend.t option;
}

type 'key operations = {
  hash : 'key -> int;
  equal : 'key -> 'key -> bool;
}

let empty =
  { root = None; sequence = Rrbvec.empty; size = 0; metadata = None }

let dynamic_key_equal left right =
  Runtime_dynamic.equal left right || Runtime_dynamic.equal right left

let dynamic_operations =
  { hash = Runtime_dynamic.hash; equal = dynamic_key_equal }

let generic_operations =
  {
    hash = Runtime_static_value.hash;
    equal = Runtime_static_value.equal;
  }

let mask hash shift = (hash lsr shift) land 0x1f
let bit_position hash shift = 1 lsl mask hash shift

let bitmap_index bitmap bit =
  Runtime_int.popcount_32 (bitmap land (bit - 1))

let array_insert values index value =
  let length = Array.length values in
  Array.init (length + 1) (fun current ->
      if current < index then values.(current)
      else if current = index then value
      else values.(current - 1))

let array_remove values index =
  let length = Array.length values in
  Array.init (length - 1) (fun current ->
      if current < index then values.(current) else values.(current + 1))

let array_replace values index value =
  let updated = Array.copy values in
  updated.(index) <- value;
  updated

let singleton_node shift (leaf : ('key, 'value) leaf) =
  Bitmap_indexed_node
    {
      bitmap = bit_position leaf.hash shift;
      slots = [| Leaf leaf |];
    }

let rec merge_leaves shift
    (left : ('key, 'value) leaf)
    (right : ('key, 'value) leaf) =
  if left.hash = right.hash then
    Hash_collision_node
      {
        hash = left.hash;
        entries = [| left; right |];
      }
  else
    let left_slot = mask left.hash shift in
    let right_slot = mask right.hash shift in
    let left_bit = 1 lsl left_slot in
    let right_bit = 1 lsl right_slot in
    if left_bit = right_bit then
      Bitmap_indexed_node
        {
          bitmap = left_bit;
          slots = [| Child (merge_leaves (shift + 5) left right) |];
        }
    else
      Bitmap_indexed_node
        {
          bitmap = left_bit lor right_bit;
          slots =
            if left_slot < right_slot then [| Leaf left; Leaf right |]
            else [| Leaf right; Leaf left |];
        }

let rec merge_node_and_leaf shift node node_hash
    (leaf : ('key, 'value) leaf) =
  let node_slot = mask node_hash shift in
  let leaf_slot = mask leaf.hash shift in
  let node_bit = 1 lsl node_slot in
  let leaf_bit = 1 lsl leaf_slot in
  if node_bit = leaf_bit then
    Bitmap_indexed_node
      {
        bitmap = node_bit;
        slots =
          [| Child (merge_node_and_leaf (shift + 5) node node_hash leaf) |];
      }
  else
    Bitmap_indexed_node
      {
        bitmap = node_bit lor leaf_bit;
        slots =
          if node_slot < leaf_slot then [| Child node; Leaf leaf |]
          else [| Leaf leaf; Child node |];
      }

let find_collision_entry equal key entries =
  let rec find index =
    if index = Array.length entries then None
    else
      let existing = entries.(index) in
      if equal key existing.key then Some (index, existing)
      else find (index + 1)
  in
  find 0

type assoc_change = Unchanged | Added | Replaced of int

let rec assoc_node operations shift (leaf : ('key, 'value) leaf) = function
  | Bitmap_indexed_node { bitmap; slots } as node ->
      let bit = bit_position leaf.hash shift in
      let index = bitmap_index bitmap bit in
      if bitmap land bit = 0 then
        if Array.length slots >= 16 then
          let children = Array.make 32 None in
          let slot_index = ref 0 in
          for branch = 0 to 31 do
            let branch_bit = 1 lsl branch in
            if bitmap land branch_bit <> 0 then (
              children.(branch) <-
                Some
                  (match slots.(!slot_index) with
                  | Leaf existing -> singleton_node (shift + 5) existing
                  | Child child -> child);
              incr slot_index)
          done;
          let branch = mask leaf.hash shift in
          children.(branch) <- Some (singleton_node (shift + 5) leaf);
          (Array_node { count = Array.length slots + 1; children }, Added)
        else
          ( Bitmap_indexed_node
              {
                bitmap = bitmap lor bit;
                slots = array_insert slots index (Leaf leaf);
              },
            Added )
      else
        (match slots.(index) with
        | Leaf existing when operations.equal leaf.key existing.key ->
            if existing.value == leaf.value then (node, Unchanged)
            else
              ( Bitmap_indexed_node
                  {
                    bitmap;
                    slots =
                      array_replace slots index
                        (Leaf { existing with value = leaf.value });
                  },
                Replaced existing.sequence_index )
        | Leaf existing ->
            ( Bitmap_indexed_node
                {
                  bitmap;
                  slots =
                    array_replace slots index
                      (Child (merge_leaves (shift + 5) existing leaf));
                },
              Added )
        | Child child ->
            let updated, change = assoc_node operations (shift + 5) leaf child in
            if updated == child then (node, change)
            else
              ( Bitmap_indexed_node
                  {
                    bitmap;
                    slots = array_replace slots index (Child updated);
                  },
                change ))
  | Array_node { count; children } as node ->
      let index = mask leaf.hash shift in
      (match children.(index) with
      | None ->
          ( Array_node
              {
                count = count + 1;
                children =
                  array_replace children index
                    (Some (singleton_node (shift + 5) leaf));
              },
            Added )
      | Some child ->
          let updated, change = assoc_node operations (shift + 5) leaf child in
          if updated == child then (node, change)
          else
            ( Array_node
                {
                  count;
                  children = array_replace children index (Some updated);
                },
              change ))
  | Hash_collision_node collision as node ->
      if collision.hash <> leaf.hash then
        (merge_node_and_leaf shift node collision.hash leaf, Added)
      else
        (match
           find_collision_entry operations.equal leaf.key collision.entries
         with
        | Some (index, existing) ->
            if existing.value == leaf.value then (node, Unchanged)
            else
              ( Hash_collision_node
                  {
                    collision with
                    entries =
                      array_replace collision.entries index
                        { existing with value = leaf.value };
                  },
                Replaced existing.sequence_index )
        | None ->
            ( Hash_collision_node
                {
                  collision with
                  entries =
                    Array.append collision.entries [| leaf |];
                },
              Added ))

let assoc_by_hash operations map key key_hash value =
  let leaf =
    {
      hash = key_hash;
      key;
      value;
      sequence_index = Rrbvec.length map.sequence;
    }
  in
  match map.root with
  | None ->
      {
        root = Some (singleton_node 0 leaf);
        sequence = Rrbvec.push_back map.sequence (Some (key, value));
        size = 1;
        metadata = map.metadata;
      }
  | Some original_root ->
      let root, change = assoc_node operations 0 leaf original_root in
      (match change with
      | Unchanged -> map
      | Added ->
        {
          root = Some root;
          sequence = Rrbvec.push_back map.sequence (Some (key, value));
          size = map.size + 1;
          metadata = map.metadata;
        }
      | Replaced sequence_index ->
          let sequence_key =
            match Rrbvec.nth map.sequence sequence_index with
            | Some (existing_key, _) -> existing_key
            | None -> key
          in
          {
            map with
            root = Some root;
            sequence =
              Rrbvec.set map.sequence sequence_index
                (Some (sequence_key, value));
          })

let assoc_by operations map key value =
  assoc_by_hash operations map key (operations.hash key) value

let assoc map key value = assoc_by generic_operations map key value

let assoc_hashed map key key_hash value =
  assoc_by_hash generic_operations map key key_hash value

let assoc_dynamic map key value = assoc_by dynamic_operations map key value
let assoc_small_string map key value = assoc map key value

let rec find_node operations shift hash key = function
  | Bitmap_indexed_node { bitmap; slots } ->
      let bit = bit_position hash shift in
      if bitmap land bit = 0 then None
      else
        (match slots.(bitmap_index bitmap bit) with
        | Leaf leaf ->
            if leaf.hash = hash && operations.equal key leaf.key then
              Some (leaf.key, leaf.value)
            else None
        | Child child -> find_node operations (shift + 5) hash key child)
  | Array_node { children; _ } ->
      Option.bind children.(mask hash shift) (fun child ->
          find_node operations (shift + 5) hash key child)
  | Hash_collision_node collision ->
      if collision.hash <> hash then None
      else
        Option.map
          (fun (_, existing) -> (existing.key, existing.value))
          (find_collision_entry operations.equal key collision.entries)

let find_by_hash operations map key hash =
  Option.bind map.root (find_node operations 0 hash key)

let find_by operations map key =
  find_by_hash operations map key (operations.hash key)

let find map key = find_by generic_operations map key
let find_dynamic map key = find_by dynamic_operations map key

let get_option_by operations map key =
  Option.map snd (find_by operations map key)

let get_option map key = get_option_by generic_operations map key
let get_option_dynamic map key = get_option_by dynamic_operations map key

let get_exn map key =
  match get_option map key with
  | Some value -> value
  | None -> invalid_arg "Runtime_map.get_exn: key not found"

let get_default map key default =
  Option.value (get_option map key) ~default

let get_default_dynamic map key default =
  Option.value (get_option_dynamic map key) ~default

let get_option_default map key default =
  match get_option map key with Some value -> Some value | None -> default

let get_option_default_dynamic map key default =
  match get_option_dynamic map key with
  | Some value -> Some value
  | None -> default

let mem map key = Option.is_some (get_option map key)
let mem_dynamic map key = Option.is_some (get_option_dynamic map key)
let lookup = get_option
let lookup_default = get_default
let contains_key = mem
let find_entry = find
let conj_entry map (key, value) = assoc map key value

let pack_array_node excluded children =
  let bitmap = ref 0 in
  let slots = ref [] in
  Array.iteri
    (fun index child ->
      if index <> excluded then
        match child with
        | None -> ()
        | Some child ->
            bitmap := !bitmap lor (1 lsl index);
            slots := Child child :: !slots)
    children;
  Bitmap_indexed_node
    { bitmap = !bitmap; slots = Array.of_list (List.rev !slots) }

let rec without operations shift hash key = function
  | Bitmap_indexed_node { bitmap; slots } as node ->
      let bit = bit_position hash shift in
      if bitmap land bit = 0 then (Some node, None)
      else
        let index = bitmap_index bitmap bit in
        (match slots.(index) with
        | Leaf leaf ->
            if leaf.hash <> hash || not (operations.equal key leaf.key) then
              (Some node, None)
            else if Array.length slots = 1 then
              (None, Some leaf.sequence_index)
            else
              ( Some
                  (Bitmap_indexed_node
                     {
                       bitmap = bitmap land lnot bit;
                       slots = array_remove slots index;
                     }),
                Some leaf.sequence_index )
        | Child child ->
            let updated, removed = without operations (shift + 5) hash key child in
            if Option.is_none removed then (Some node, None)
            else
              (match updated with
              | Some updated ->
                  ( Some
                      (Bitmap_indexed_node
                         {
                           bitmap;
                           slots = array_replace slots index (Child updated);
                         }),
                    removed )
              | None when Array.length slots = 1 -> (None, removed)
              | None ->
                  ( Some
                      (Bitmap_indexed_node
                         {
                           bitmap = bitmap land lnot bit;
                           slots = array_remove slots index;
                         }),
                    removed )))
  | Array_node { count; children } as node ->
      let index = mask hash shift in
      (match children.(index) with
      | None -> (Some node, None)
      | Some child ->
          let updated, removed = without operations (shift + 5) hash key child in
          if Option.is_none removed then (Some node, None)
          else
            (match updated with
            | Some updated ->
                ( Some
                    (Array_node
                       {
                         count;
                         children =
                           array_replace children index (Some updated);
                       }),
                  removed )
            | None when count <= 8 ->
                (Some (pack_array_node index children), removed)
            | None ->
                ( Some
                    (Array_node
                       {
                         count = count - 1;
                         children = array_replace children index None;
                       }),
                  removed )))
  | Hash_collision_node collision as node ->
      if collision.hash <> hash then (Some node, None)
      else
        (match find_collision_entry operations.equal key collision.entries with
        | None -> (Some node, None)
        | Some (_, leaf) when Array.length collision.entries = 1 ->
            (None, Some leaf.sequence_index)
        | Some (index, leaf) ->
            ( Some
                (Hash_collision_node
                   {
                     collision with
                     entries = array_remove collision.entries index;
                   }),
              Some leaf.sequence_index ))

let dissoc_by operations map key =
  match map.root with
  | None -> map
  | Some root ->
      let root, removed = without operations 0 (operations.hash key) key root in
      (match removed with
      | None -> map
      | Some sequence_index ->
          {
            map with
            root;
            sequence = Rrbvec.set map.sequence sequence_index None;
            size = map.size - 1;
          })

let dissoc map key = dissoc_by generic_operations map key
let dissoc_dynamic map key = dissoc_by dynamic_operations map key

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

let fold_left fn accumulator map =
  Rrbvec.fold_left
    (fun accumulator entry ->
      match entry with
      | Some entry -> fn accumulator entry
      | None -> accumulator)
    accumulator map.sequence

let reduce_protocol map fn accumulator = fold_left fn accumulator map

let kv_reduce fn accumulator map =
  fold_left
    (fun accumulator (key, value) -> fn accumulator key value)
    accumulator map

let kv_reduce_protocol map fn accumulator = kv_reduce fn accumulator map

let equiv_by operations value_equal left right =
  left == right
  || (left.size = right.size
     && fold_left
          (fun equal (key, value) ->
            equal
            &&
            match find_by operations right key with
            | Some (_, right_value) -> value_equal value right_value
            | None -> false)
          true left)

let equiv left right =
  equiv_by generic_operations Runtime_static_value.equal left right

let to_list map =
  fold_left (fun entries entry -> entry :: entries) [] map |> List.rev

let to_seq map =
  let length = Rrbvec.length map.sequence in
  let rec next index () =
    if index = length then Seq.Nil
    else
      match Rrbvec.nth map.sequence index with
      | Some entry -> Seq.Cons (entry, next (index + 1))
      | None -> next (index + 1) ()
  in
  next 0

let first_opt map =
  match to_seq map () with
  | Seq.Cons (entry, _) -> Some entry
  | Seq.Nil -> None

let count map = map.size

let with_metadata map metadata =
  let metadata =
    match metadata with Lg_edn_backend.Nil -> None | metadata -> Some metadata
  in
  let unchanged =
    match (map.metadata, metadata) with
    | None, None -> true
    | Some current, Some replacement -> current == replacement
    | None, Some _ | Some _, None -> false
  in
  if unchanged then map else { map with metadata }

let metadata map =
  Option.value map.metadata ~default:Lg_edn_backend.Nil

let empty_like map = { empty with metadata = map.metadata }

let merge left right =
  fold_left (fun result (key, value) -> assoc result key value) left right

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
