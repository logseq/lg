type 'value t = 'value list

let empty = []

let rec mem value = function
  | [] -> false
  | current :: rest ->
      let comparison = Stdlib.compare value current in
      comparison = 0 || (comparison > 0 && mem value rest)

let add value set =
  let rec insert reversed = function
    | [] -> List.rev_append reversed [ value ]
    | (current :: rest as remaining) ->
        let comparison = Stdlib.compare value current in
        if comparison = 0 then List.rev_append reversed remaining
        else if comparison < 0 then
          List.rev_append reversed (value :: remaining)
        else insert (current :: reversed) rest
  in
  insert [] set

let remove value set =
  List.filter (fun current -> Stdlib.compare value current <> 0) set

let of_list values = List.fold_left (fun set value -> add value set) empty values

let of_seq values = Seq.fold_left (fun set value -> add value set) empty values

let elements set = set

let subset left right = List.for_all (fun value -> mem value right) left

let union left right = List.fold_left (fun set value -> add value set) right left

let inter left right = List.filter (fun value -> mem value right) left

let diff left right = List.filter (fun value -> not (mem value right)) left
