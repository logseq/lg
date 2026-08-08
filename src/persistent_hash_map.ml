module type Key = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
end

module Make (Key : Key) = struct
  type key = Key.t

  type 'value leaf = {
    hash : int;
    key : key;
    value : 'value;
  }

  type 'value slot = Leaf of 'value leaf | Child of 'value node

  and 'value node =
    | Bitmap_indexed of {
        bitmap : int;
        slots : 'value slot array;
      }
    | Hash_collision of {
        hash : int;
        entries : (key * 'value) array;
      }

  type 'value t = {
    count : int;
    root : 'value node option;
  }

  let empty = { count = 0; root = None }
  let cardinal map = map.count
  let normalized_hash key = Key.hash key land max_int
  let mask hash shift = (hash lsr shift) land 0x1f
  let bit_position hash shift = 1 lsl mask hash shift

  let bit_count value =
    let value = value - ((value lsr 1) land 0x55555555) in
    let value =
      (value land 0x33333333) + ((value lsr 2) land 0x33333333)
    in
    let value = (value + (value lsr 4)) land 0x0f0f0f0f in
    let value = value + (value lsr 8) in
    let value = value + (value lsr 16) in
    value land 0x3f

  let slot_index bitmap bit = bit_count (bitmap land (bit - 1))

  let insert_slot slots index slot =
    let length = Array.length slots in
    let updated = Array.make (length + 1) slot in
    Array.blit slots 0 updated 0 index;
    Array.blit slots index updated (index + 1) (length - index);
    updated

  let remove_slot slots index =
    let length = Array.length slots in
    let updated = Array.make (length - 1) slots.(if index = 0 then 1 else 0) in
    Array.blit slots 0 updated 0 index;
    Array.blit slots (index + 1) updated index (length - index - 1);
    updated

  let replace_slot slots index slot =
    let updated = Array.copy slots in
    updated.(index) <- slot;
    updated

  let rec merge_leaves shift left right =
    if left.hash = right.hash then
      Hash_collision
        {
          hash = left.hash;
          entries = [| (left.key, left.value); (right.key, right.value) |];
        }
    else
      let left_bit = bit_position left.hash shift in
      let right_bit = bit_position right.hash shift in
      if left_bit = right_bit then
        Bitmap_indexed
          {
            bitmap = left_bit;
            slots = [| Child (merge_leaves (shift + 5) left right) |];
          }
      else
        Bitmap_indexed
          {
            bitmap = left_bit lor right_bit;
            slots =
              if left_bit < right_bit then [| Leaf left; Leaf right |]
              else [| Leaf right; Leaf left |];
          }

  let rec merge_node_and_leaf shift node node_hash leaf =
    let node_bit = bit_position node_hash shift in
    let leaf_bit = bit_position leaf.hash shift in
    if node_bit = leaf_bit then
      Bitmap_indexed
        {
          bitmap = node_bit;
          slots =
            [| Child (merge_node_and_leaf (shift + 5) node node_hash leaf) |];
        }
    else
      Bitmap_indexed
        {
          bitmap = node_bit lor leaf_bit;
          slots =
            if node_bit < leaf_bit then [| Child node; Leaf leaf |]
            else [| Leaf leaf; Child node |];
        }

  let rec assoc shift leaf = function
    | Bitmap_indexed { bitmap; slots } as node ->
        let bit = bit_position leaf.hash shift in
        if bitmap land bit = 0 then
          ( Bitmap_indexed
              {
                bitmap = bitmap lor bit;
                slots = insert_slot slots (slot_index bitmap bit) (Leaf leaf);
              },
            true )
        else
          let index = slot_index bitmap bit in
          (match slots.(index) with
          | Leaf existing when Key.equal existing.key leaf.key ->
              if existing.value == leaf.value then (node, false)
              else
                ( Bitmap_indexed
                    {
                      bitmap;
                      slots = replace_slot slots index (Leaf leaf);
                    },
                  false )
          | Leaf existing ->
              ( Bitmap_indexed
                  {
                    bitmap;
                    slots =
                      replace_slot slots index
                        (Child (merge_leaves (shift + 5) existing leaf));
                  },
                true )
          | Child child ->
              let child, added = assoc (shift + 5) leaf child in
              ( Bitmap_indexed
                  {
                    bitmap;
                    slots = replace_slot slots index (Child child);
                  },
                added ))
    | Hash_collision { hash; entries } as node ->
        if hash <> leaf.hash then
          (merge_node_and_leaf shift node hash leaf, true)
        else
          let matching =
            Array.to_seq entries
            |> Seq.find_index (fun (key, _) -> Key.equal key leaf.key)
          in
          (match matching with
          | Some index ->
              if snd entries.(index) == leaf.value then (node, false)
              else
                ( Hash_collision
                    {
                      hash;
                      entries =
                        Array.mapi
                          (fun current entry ->
                            if current = index then (leaf.key, leaf.value)
                            else entry)
                          entries;
                    },
                  false )
          | None ->
              ( Hash_collision
                  { hash; entries = Array.append entries [| (leaf.key, leaf.value) |] },
                true ))

  let add key value map =
    let leaf = { hash = normalized_hash key; key; value } in
    match map.root with
    | None ->
        {
          count = 1;
          root =
            Some
              (Bitmap_indexed
                 {
                   bitmap = bit_position leaf.hash 0;
                   slots = [| Leaf leaf |];
                 });
        }
    | Some root ->
        let root, added = assoc 0 leaf root in
        { count = (if added then map.count + 1 else map.count); root = Some root }

  let rec find_node shift hash key = function
    | Bitmap_indexed { bitmap; slots } ->
        let bit = bit_position hash shift in
        if bitmap land bit = 0 then None
        else
          (match slots.(slot_index bitmap bit) with
          | Leaf leaf ->
              if leaf.hash = hash && Key.equal leaf.key key then Some leaf.value
              else None
          | Child child -> find_node (shift + 5) hash key child)
    | Hash_collision collision ->
        if collision.hash <> hash then None
        else
          collision.entries |> Array.to_seq
          |> Seq.find_map (fun (entry_key, value) ->
                 if Key.equal entry_key key then Some value else None)

  let find_opt key map =
    match map.root with
    | None -> None
    | Some root -> find_node 0 (normalized_hash key) key root

  let mem key map = Option.is_some (find_opt key map)

  let rec without shift hash key = function
    | Bitmap_indexed { bitmap; slots } as node ->
        let bit = bit_position hash shift in
        if bitmap land bit = 0 then (Some node, false)
        else
          let index = slot_index bitmap bit in
          (match slots.(index) with
          | Leaf leaf ->
              if leaf.hash <> hash || not (Key.equal leaf.key key) then
                (Some node, false)
              else if Array.length slots = 1 then (None, true)
              else
                ( Some
                    (Bitmap_indexed
                       {
                         bitmap = bitmap land lnot bit;
                         slots = remove_slot slots index;
                       }),
                  true )
          | Child child -> (
              match without (shift + 5) hash key child with
              | Some updated, removed ->
                  ( Some
                      (Bitmap_indexed
                         {
                           bitmap;
                           slots = replace_slot slots index (Child updated);
                         }),
                    removed )
              | None, true ->
                  if Array.length slots = 1 then (None, true)
                  else
                    ( Some
                        (Bitmap_indexed
                           {
                             bitmap = bitmap land lnot bit;
                             slots = remove_slot slots index;
                           }),
                      true )
              | None, false -> assert false))
    | Hash_collision { hash = collision_hash; entries } as node ->
        if collision_hash <> hash then (Some node, false)
        else
          let remaining =
            entries |> Array.to_list
            |> List.filter (fun (entry_key, _) -> not (Key.equal entry_key key))
            |> Array.of_list
          in
          if Array.length remaining = Array.length entries then (Some node, false)
          else if Array.length remaining = 0 then (None, true)
          else
            (Some (Hash_collision { hash = collision_hash; entries = remaining }), true)

  let remove key map =
    match map.root with
    | None -> map
    | Some root ->
        let root, removed = without 0 (normalized_hash key) key root in
        if removed then { count = map.count - 1; root } else map

  let update key update_value map =
    match update_value (find_opt key map) with
    | None -> remove key map
    | Some value -> add key value map

  let rec fold_node f node state =
    match node with
    | Bitmap_indexed { slots; _ } ->
        Array.fold_left
          (fun state -> function
            | Leaf leaf -> f leaf.key leaf.value state
            | Child child -> fold_node f child state)
          state slots
    | Hash_collision { entries; _ } ->
        Array.fold_left (fun state (key, value) -> f key value state) state entries

  let fold f map state =
    match map.root with None -> state | Some root -> fold_node f root state

  let rec find_map_node f = function
    | Bitmap_indexed { slots; _ } ->
        let rec visit index =
          if index = Array.length slots then None
          else
            match slots.(index) with
            | Leaf leaf -> (
                match f leaf.key leaf.value with
                | Some _ as result -> result
                | None -> visit (index + 1))
            | Child child -> (
                match find_map_node f child with
                | Some _ as result -> result
                | None -> visit (index + 1))
        in
        visit 0
    | Hash_collision { entries; _ } ->
        entries |> Array.to_seq
        |> Seq.find_map (fun (key, value) -> f key value)

  let find_map f map =
    match map.root with None -> None | Some root -> find_map_node f root

  let bindings map = fold (fun key value result -> (key, value) :: result) map []
end
