let safe_compare left right =
  if left == right then 0
  else
    try Stdlib.compare left right with
    | Invalid_argument _ -> 0

let compare_option compare left right =
  match (left, right) with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some left, Some right -> compare left right
