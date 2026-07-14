type 'a t = {
  value : 'a;
  reduced : bool;
}

let continue value = { value; reduced = false }
let reduced value = { value; reduced = true }
let is_reduced result = result.reduced
let unreduced result = result.value

let rec fold_list reducer accumulator = function
  | [] -> accumulator
  | item :: rest ->
      let result = reducer accumulator item in
      if result.reduced then result.value
      else fold_list reducer result.value rest

let fold_vector reducer accumulator values =
  let length = Rrbvec.length values in
  let rec loop accumulator index =
    if index >= length then accumulator
    else
      let result = reducer accumulator (Rrbvec.nth values index) in
      if result.reduced then result.value
      else loop result.value (index + 1)
  in
  loop accumulator 0

let fold_array reducer accumulator values =
  let length = Array.length values in
  let rec loop accumulator index =
    if index >= length then accumulator
    else
      let result = reducer accumulator values.(index) in
      if result.reduced then result.value
      else loop result.value (index + 1)
  in
  loop accumulator 0

let fold_string reducer accumulator value =
  let length = String.length value in
  let rec loop accumulator index =
    if index >= length then accumulator
    else
      let result = reducer accumulator value.[index] in
      if result.reduced then result.value
      else loop result.value (index + 1)
  in
  loop accumulator 0

let rec fold_seq reducer accumulator values =
  match values () with
  | Seq.Nil -> accumulator
  | Seq.Cons (item, rest) ->
      let result = reducer accumulator item in
      if result.reduced then result.value
      else fold_seq reducer result.value rest
