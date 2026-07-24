let equal left right =
  left == right
  ||
  try left = right with Invalid_argument _ -> false

let hash value = Hashtbl.hash value
