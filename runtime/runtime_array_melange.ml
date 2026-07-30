external map : 'a array -> ('a -> 'b) -> 'b array = "map" [@@mel.send]

external call0 : (unit -> 'a) -> int -> 'a = "call" [@@mel.send]

external call2 : ('a -> 'b -> 'c) -> int -> 'a -> 'b -> 'c = "call"
  [@@mel.send]

external sort_raw : 'a array -> ('a -> 'a -> int) -> 'a array = "sort"
  [@@mel.send]

let sort values compare =
  ignore (sort_raw values (fun left right -> compare left right))

external reduce_raw :
  'a array -> ('b -> 'a -> int -> 'a array -> 'b) -> 'b -> 'b = "reduce"
  [@@mel.send]

let fold_left fn initial values =
  reduce_raw values
    (fun accumulator value _index _values -> call2 fn 0 accumulator value)
    initial

let binary_search_left compare values right key =
  let rec search left right =
    if left > right then left
    else
      let middle = left + ((right - left) / 2) in
      if call2 compare 0 (Array.unsafe_get values middle) key < 0 then
        search (middle + 1) right
      else search left (middle - 1)
  in
  float_of_int (search 0 right)

let binary_search_right compare values right key =
  let rec search left right =
    if left > right then left
    else
      let middle = left + ((right - left) / 2) in
      if call2 compare 0 (Array.unsafe_get values middle) key > 0 then
        search left (middle - 1)
      else search (middle + 1) right
  in
  float_of_int (search 0 right)
