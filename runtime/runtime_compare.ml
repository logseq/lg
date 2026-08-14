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

let compare_vector compare left right =
  let rec loop left_values right_values =
    match (left_values (), right_values ()) with
    | Seq.Nil, Seq.Nil -> 0
    | Seq.Nil, Seq.Cons _ -> -1
    | Seq.Cons _, Seq.Nil -> 1
    | Seq.Cons (left, left_rest), Seq.Cons (right, right_rest) ->
        let result = compare left right in
        if result = 0 then loop left_rest right_rest else result
  in
  loop (Rrbvec.to_seq left) (Rrbvec.to_seq right)
