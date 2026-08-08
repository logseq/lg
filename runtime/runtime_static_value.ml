let equal left right =
  try left = right with Invalid_argument _ -> left == right

let hash value = Hashtbl.hash value

let consume value = ignore value
