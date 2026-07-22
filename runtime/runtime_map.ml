type 'key node =
  | Empty
  | Node of {
      left : 'key node;
      key : 'key;
      positions : ('key * int) list;
      right : 'key node;
      height : int;
      size : int;
    }

type ('key, 'value) t = {
  index : 'key node;
  entries : ('key * 'value) option Rrbvec.t;
  size : int;
}

type 'key operations = {
  compare : 'key -> 'key -> int;
  equal : 'key -> 'key -> bool;
}

let empty = { index = Empty; entries = Rrbvec.empty; size = 0 }

let dynamic_key_equal left right =
  Runtime_dynamic.equal left right || Runtime_dynamic.equal right left

let dynamic_key_compare left right =
  if dynamic_key_equal left right then 0
  else
    match Runtime_dynamic.compare left right with
    | comparison when comparison <> 0 -> comparison
    | _ -> Int.compare (Runtime_dynamic.hash left) (Runtime_dynamic.hash right)
    | exception Invalid_argument _ ->
        Int.compare (Runtime_dynamic.hash left) (Runtime_dynamic.hash right)

let dynamic_operations =
  { compare = dynamic_key_compare; equal = dynamic_key_equal }

let polymorphic_key_equal left right =
  if
    Runtime_dynamic.is_runtime_dynamic left
    && Runtime_dynamic.is_runtime_dynamic right
  then dynamic_key_equal (Obj.magic left) (Obj.magic right)
  else left = right

let polymorphic_key_compare left right =
  if
    Runtime_dynamic.is_runtime_dynamic left
    && Runtime_dynamic.is_runtime_dynamic right
  then dynamic_key_compare (Obj.magic left) (Obj.magic right)
  else Stdlib.compare left right

let generic_operations =
  { compare = polymorphic_key_compare; equal = polymorphic_key_equal }

let height = function Empty -> 0 | Node node -> node.height
let node_count = function Empty -> 0 | Node node -> node.size

let make left key positions right =
  Node
    {
      left;
      key;
      positions;
      right;
      height = 1 + max (height left) (height right);
      size = node_count left + List.length positions + node_count right;
    }

let balance left key positions right =
  let left_height = height left in
  let right_height = height right in
  if left_height > right_height + 2 then
    match left with
    | Empty -> invalid_arg "invalid persistent map balance"
    | Node left_node ->
        if height left_node.left >= height left_node.right then
          make left_node.left left_node.key left_node.positions
            (make left_node.right key positions right)
        else (
          match left_node.right with
          | Empty -> invalid_arg "invalid persistent map balance"
          | Node pivot ->
              make
                (make left_node.left left_node.key left_node.positions
                   pivot.left)
                pivot.key pivot.positions
                (make pivot.right key positions right))
  else if right_height > left_height + 2 then
    match right with
    | Empty -> invalid_arg "invalid persistent map balance"
    | Node right_node ->
        if height right_node.right >= height right_node.left then
          make
            (make left key positions right_node.left)
            right_node.key right_node.positions right_node.right
        else (
          match right_node.left with
          | Empty -> invalid_arg "invalid persistent map balance"
          | Node pivot ->
              make
                (make left key positions pivot.left)
                pivot.key pivot.positions
                (make pivot.right right_node.key right_node.positions
                   right_node.right))
  else make left key positions right

let replace_position equal key position positions =
  let rec replace prefix = function
    | [] -> List.rev ((key, position) :: prefix)
    | (existing_key, _) :: rest when equal key existing_key ->
        List.rev_append prefix ((key, position) :: rest)
    | entry :: rest -> replace (entry :: prefix) rest
  in
  replace [] positions

let find_position_in operations index key =
  let rec find = function
    | Empty -> None
    | Node node ->
        let comparison = operations.compare key node.key in
        if comparison < 0 then find node.left
        else if comparison > 0 then find node.right
        else
          List.find_opt
            (fun (existing_key, _) -> operations.equal key existing_key)
            node.positions
          |> Option.map snd
  in
  find index

let insert_position operations index key position =
  let rec insert = function
    | Empty -> make Empty key [ (key, position) ] Empty
    | Node node as unchanged ->
        let comparison = operations.compare key node.key in
        if comparison < 0 then
          let left = insert node.left in
          if left == node.left then unchanged
          else balance left node.key node.positions node.right
        else if comparison > 0 then
          let right = insert node.right in
          if right == node.right then unchanged
          else balance node.left node.key node.positions right
        else
          let positions =
            replace_position operations.equal key position node.positions
          in
          make node.left node.key positions node.right
  in
  insert index

let assoc_by operations map key value =
  match find_position_in operations map.index key with
  | Some position ->
      { map with entries = Rrbvec.set map.entries position (Some (key, value)) }
  | None ->
      let position = Rrbvec.length map.entries in
      {
        index = insert_position operations map.index key position;
        entries = Rrbvec.push_back map.entries (Some (key, value));
        size = map.size + 1;
      }

let assoc map key value = assoc_by generic_operations map key value
let assoc_dynamic map key value = assoc_by dynamic_operations map key value

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

let rec remove_min = function
  | Empty -> invalid_arg "remove_min called on an empty map"
  | Node { left = Empty; key; positions; right; _ } ->
      (key, positions, right)
  | Node node ->
      let key, positions, left = remove_min node.left in
      (key, positions, balance left node.key node.positions node.right)

let merge left right =
  match (left, right) with
  | Empty, index | index, Empty -> index
  | _ ->
      let key, positions, right = remove_min right in
      balance left key positions right

let remove_position operations index key =
  let rec remove = function
    | Empty -> Empty
    | Node node as unchanged ->
        let comparison = operations.compare key node.key in
        if comparison < 0 then
          let left = remove node.left in
          if left == node.left then unchanged
          else balance left node.key node.positions node.right
        else if comparison > 0 then
          let right = remove node.right in
          if right == node.right then unchanged
          else balance node.left node.key node.positions right
        else
          let positions =
            List.filter
              (fun (existing_key, _) ->
                not (operations.equal key existing_key))
              node.positions
          in
          if List.length positions = List.length node.positions then unchanged
          else
            match positions with
            | [] -> merge node.left node.right
            | (representative, _) :: _ ->
                make node.left representative positions node.right
  in
  remove index

let dissoc_by operations map key =
  match find_position_in operations map.index key with
  | None -> map
  | Some position ->
      {
        index = remove_position operations map.index key;
        entries = Rrbvec.set map.entries position None;
        size = map.size - 1;
      }

let dissoc map key = dissoc_by generic_operations map key
let dissoc_dynamic map key = dissoc_by dynamic_operations map key

let find_by operations map key =
  Option.bind (find_position_in operations map.index key) (fun position ->
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

let count map = map.size

let fold_left fn accumulator map =
  Rrbvec.fold_left
    (fun accumulator entry ->
      match entry with Some entry -> fn accumulator entry | None -> accumulator)
    accumulator map.entries

let to_list map =
  fold_left (fun entries entry -> entry :: entries) [] map |> List.rev

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

let first_exn map =
  match to_seq map () with
  | Seq.Cons (entry, _) -> entry
  | Seq.Nil -> invalid_arg "first called on an empty map"
