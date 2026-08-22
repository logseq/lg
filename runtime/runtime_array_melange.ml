external map : 'a array -> ('a -> 'b) -> 'b array = "map" [@@mel.send]

external call0 : (unit -> 'a) -> int -> 'a = "call" [@@mel.send]

external call2 : ('a -> 'b -> 'c) -> int -> 'a -> 'b -> 'c = "call"
  [@@mel.send]

external sort_raw : 'a array -> ('a -> 'a -> int) -> 'a array = "sort"
  [@@mel.send]

external make_uninitialized : int -> 'a array = "Array" [@@mel.new]

let sort values compare =
  ignore (sort_raw values (fun left right -> compare left right))

external reduce_raw :
  'a array -> ('b -> 'a -> int -> 'a array -> 'b) -> 'b -> 'b = "reduce"
  [@@mel.send]

let fold_left fn initial values =
  reduce_raw values
    (fun accumulator value _index _values -> call2 fn 0 accumulator value)
    initial

let splice values cut_from cut_to splice_from splice_to inserted =
  let left_length = splice_from - cut_from in
  let inserted_length = Array.length inserted in
  let right_length = cut_to - splice_to in
  let result = make_uninitialized (left_length + inserted_length + right_length) in
  for index = 0 to left_length - 1 do
    Array.unsafe_set result index
      (Array.unsafe_get values (cut_from + index))
  done;
  for index = 0 to inserted_length - 1 do
    Array.unsafe_set result (left_length + index)
      (Array.unsafe_get inserted index)
  done;
  for index = 0 to right_length - 1 do
    Array.unsafe_set result (left_length + inserted_length + index)
      (Array.unsafe_get values (splice_to + index))
  done;
  result

let splice_one values cut_from cut_to splice_from splice_to value =
  splice values cut_from cut_to splice_from splice_to [| value |]
