type t = { numerator : int; denominator : int }

let rec gcd left right =
  if right = 0 then Int.abs left else gcd right (left mod right)

let make numerator denominator =
  if denominator = 0 then invalid_arg "ratio denominator cannot be zero";
  let numerator, denominator =
    if denominator < 0 then (-numerator, -denominator)
    else (numerator, denominator)
  in
  if numerator = 0 then { numerator = 0; denominator = 1 }
  else
    let divisor = gcd numerator denominator in
    { numerator = numerator / divisor; denominator = denominator / divisor }

let of_string source =
  match String.split_on_char '/' source with
  | [ numerator; denominator ] -> (
      match (int_of_string_opt numerator, int_of_string_opt denominator) with
      | Some numerator, Some denominator -> make numerator denominator
      | _ -> invalid_arg ("invalid ratio literal: " ^ source))
  | _ -> invalid_arg ("invalid ratio literal: " ^ source)

let of_int value = make value 1

let negate value =
  if value.numerator = 0 then value
  else { value with numerator = -value.numerator }

let add left right =
  make
    ((left.numerator * right.denominator)
    + (right.numerator * left.denominator))
    (left.denominator * right.denominator)

let subtract left right = add left (negate right)

let multiply left right =
  make
    (left.numerator * right.numerator)
    (left.denominator * right.denominator)

let divide left right =
  make
    (left.numerator * right.denominator)
    (left.denominator * right.numerator)

let to_string value =
  if value.denominator = 1 then string_of_int value.numerator
  else string_of_int value.numerator ^ "/" ^ string_of_int value.denominator

let to_int value = value.numerator / value.denominator
let to_float value = float_of_int value.numerator /. float_of_int value.denominator

let equal left right =
  left.numerator = right.numerator && left.denominator = right.denominator

let is_ratio value = value.denominator <> 1

let abs value =
  if value.numerator < 0 then { value with numerator = -value.numerator }
  else value

let compare left right =
  Int.compare
    (left.numerator * right.denominator)
    (right.numerator * left.denominator)

let max left right = if compare left right > 0 then left else right
let min left right = if compare left right < 0 then left else right
