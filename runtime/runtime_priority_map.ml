module Priority_map = Map.Make (Int)

type 'item bucket = ('item, unit) Runtime_map.t

type 'item t = {
  priority_to_items : 'item bucket Priority_map.t;
  item_to_priority : ('item, int) Runtime_map.t;
}

let empty =
  {
    priority_to_items = Priority_map.empty;
    item_to_priority = Runtime_map.empty;
  }

let count map = Runtime_map.count map.item_to_priority
let contains map item = Runtime_map.contains_key map.item_to_priority item
let lookup map item = Runtime_map.lookup map.item_to_priority item

let add_to_bucket priority item priority_to_items =
  let bucket =
    Priority_map.find_opt priority priority_to_items
    |> Option.value ~default:Runtime_map.empty
    |> fun bucket -> Runtime_map.assoc bucket item ()
  in
  Priority_map.add priority bucket priority_to_items

let remove_from_bucket priority item priority_to_items =
  match Priority_map.find_opt priority priority_to_items with
  | None -> priority_to_items
  | Some bucket ->
      let bucket = Runtime_map.dissoc bucket item in
      if Runtime_map.count bucket = 0 then
        Priority_map.remove priority priority_to_items
      else Priority_map.add priority bucket priority_to_items

let assoc map item priority =
  match lookup map item with
  | Some current_priority when current_priority = priority -> map
  | Some current_priority ->
      {
        priority_to_items =
          map.priority_to_items
          |> remove_from_bucket current_priority item
          |> add_to_bucket priority item;
        item_to_priority =
          Runtime_map.assoc map.item_to_priority item priority;
      }
  | None ->
      {
        priority_to_items =
          add_to_bucket priority item map.priority_to_items;
        item_to_priority =
          Runtime_map.assoc map.item_to_priority item priority;
      }

let dissoc map item =
  match lookup map item with
  | None -> map
  | Some priority ->
      {
        priority_to_items =
          remove_from_bucket priority item map.priority_to_items;
        item_to_priority = Runtime_map.dissoc map.item_to_priority item;
      }

let first_bucket_item bucket =
  Runtime_map.kv_reduce
    (fun found item () -> match found with Some _ -> found | None -> Some item)
    None bucket

let peek map =
  match Priority_map.min_binding_opt map.priority_to_items with
  | None -> None
  | Some (priority, bucket) ->
      Option.map (fun item -> (item, priority)) (first_bucket_item bucket)
