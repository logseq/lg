type 'value t = 'value list

let empty = []

let rec mem value = function
  | [] -> false
  | current :: rest ->
      Runtime_static_value.equal value current || mem value rest

let add value set =
  if mem value set then set else value :: set

let remove value set =
  List.filter
    (fun current -> not (Runtime_static_value.equal value current))
    set

let of_list values = List.fold_left (fun set value -> add value set) empty values

let of_seq values = Seq.fold_left (fun set value -> add value set) empty values

let elements set = set
let cardinal = List.length

let subset left right = List.for_all (fun value -> mem value right) left

let equal left right =
  cardinal left = cardinal right && subset left right

let union left right = List.fold_left (fun set value -> add value set) right left

let inter left right = List.filter (fun value -> mem value right) left

let diff left right = List.filter (fun value -> not (mem value right)) left
