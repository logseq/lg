type ('key, 'value) bucket = ('key, 'value) Runtime_map.t list

type ('key, 'value) t = {
  buckets : (int, ('key, 'value) bucket) Runtime_map.t;
  size : int;
}

let empty = { buckets = Runtime_map.empty; size = 0 }

let rec bucket_mem value = function
  | [] -> false
  | current :: rest ->
      Runtime_map.equiv value current || bucket_mem value rest

let bucket set hash =
  Runtime_map.get_default set.buckets hash []

let mem value set =
  bucket_mem value (bucket set (Runtime_map.hash value))

let add value set =
  let hash = Runtime_map.hash value in
  let values = bucket set hash in
  if bucket_mem value values then set
  else
    {
      buckets = Runtime_map.assoc set.buckets hash (value :: values);
      size = set.size + 1;
    }

let remove value set =
  let hash = Runtime_map.hash value in
  let values = bucket set hash in
  let remaining =
    List.filter (fun current -> not (Runtime_map.equiv value current)) values
  in
  if List.length remaining = List.length values then set
  else
    {
      buckets =
        (match remaining with
        | [] -> Runtime_map.dissoc set.buckets hash
        | _ -> Runtime_map.assoc set.buckets hash remaining);
      size = set.size - 1;
    }

let of_list values = List.fold_left (fun set value -> add value set) empty values
let of_seq values = Seq.fold_left (fun set value -> add value set) empty values

let fold fn set initial =
  Runtime_map.fold_left
    (fun accumulator (_, values) ->
      List.fold_left
        (fun accumulator value -> fn value accumulator)
        accumulator values)
    initial set.buckets

let elements set = fold (fun value values -> value :: values) set []
let cardinal set = set.size
let is_empty set = set.size = 0

let min_elt set =
  match elements set with value :: _ -> value | [] -> raise Not_found

let max_elt = min_elt

let subset left right = fold (fun value result -> result && mem value right) left true

let equal left right =
  cardinal left = cardinal right && subset left right

let union left right = fold add left right

let inter left right =
  fold
    (fun value result -> if mem value right then add value result else result)
    left empty

let diff left right =
  fold
    (fun value result -> if mem value right then result else add value result)
    left empty
