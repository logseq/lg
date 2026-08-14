external of_float_unchecked : float -> int = "%identity"
external to_float_unchecked : int -> float = "%identity"

let binary operation left right =
  of_float_unchecked
    (operation (to_float_unchecked left) (to_float_unchecked right))

let add = binary ( +. )
let subtract = binary ( -. )
let multiply = binary ( *. )
let negate value = of_float_unchecked (-. (to_float_unchecked value))
let int32 value = value lor 0
let logical_shift_right value count = value lsr count

let abs_unchecked value =
  of_float_unchecked (Float.abs (to_float_unchecked value))

let is_safe_float_integer value =
  Float.is_integer value && Float.abs value <= 9007199254740991.
