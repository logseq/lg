type 'value t = 'value Seq.t list

let empty = []

let rec sequence_equal left right =
  match (left (), right ()) with
  | Seq.Nil, Seq.Nil -> true
  | Seq.Cons (left, left_rest), Seq.Cons (right, right_rest) ->
      Runtime_static_value.equal left right
      && sequence_equal left_rest right_rest
  | Seq.Nil, Seq.Cons _ | Seq.Cons _, Seq.Nil -> false

let rec mem value = function
  | [] -> false
  | current :: rest -> sequence_equal value current || mem value rest

let add value set = if mem value set then set else value :: set

let remove value set =
  List.filter (fun current -> not (sequence_equal value current)) set

let of_list values = List.fold_left (fun set value -> add value set) empty values
let of_seq values = Seq.fold_left (fun set value -> add value set) empty values
let elements set = set
let cardinal = List.length
let is_empty = function [] -> true | _ -> false
let min_elt = function value :: _ -> value | [] -> raise Not_found

let fold fn set initial =
  List.fold_left (fun accumulator value -> fn value accumulator) initial set

let subset left right = List.for_all (fun value -> mem value right) left

let equal left right =
  cardinal left = cardinal right && subset left right

let union left right = List.fold_left (fun set value -> add value set) right left
let inter left right = List.filter (fun value -> mem value right) left
let diff left right = List.filter (fun value -> not (mem value right)) left
