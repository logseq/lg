let min_subnormal = Int64.float_of_bits 1L

let max_number left right =
  if Float.is_nan left then left
  else if Float.is_nan right then right
  else if left > right then left
  else right

let min_number left right =
  if Float.is_nan left then left
  else if Float.is_nan right then right
  else if left < right then left
  else right

let max_nullable left right =
  let left_number = Option.value left ~default:0.0 in
  let right_number = Option.value right ~default:0.0 in
  if Float.is_nan left_number then left
  else if Float.is_nan right_number then right
  else if left_number > right_number then left
  else right

let min_nullable left right =
  let left_number = Option.value left ~default:0.0 in
  let right_number = Option.value right ~default:0.0 in
  if Float.is_nan left_number then left
  else if Float.is_nan right_number then right
  else if left_number < right_number then left
  else right

let encoded_exponent value =
  let bits = Int64.bits_of_float value in
  Int64.(to_int (logand (shift_right_logical bits 52) 0x7ffL))

let get_exponent value =
  if Float.is_nan value || not (Float.is_finite value) then 1024
  else
    let encoded = encoded_exponent value in
    if encoded = 0 then -1023 else encoded - 1023

let next_after start direction =
  if Float.is_nan start || Float.is_nan direction then start +. direction
  else if Float.equal start direction then direction
  else if Float.equal start 0.0 then Float.copy_sign min_subnormal direction
  else
    let bits = Int64.bits_of_float start in
    let increase_bits = (start > 0.0) = (direction > start) in
    let adjacent_bits =
      if increase_bits then Int64.add bits 1L else Int64.sub bits 1L
    in
    Int64.float_of_bits adjacent_bits

let ulp value =
  if Float.is_nan value then value
  else if not (Float.is_finite value) then Float.infinity
  else if encoded_exponent value = 0 then min_subnormal
  else
    let exponent = get_exponent value - 52 in
    if exponent >= -1022 then
      Int64.float_of_bits
        Int64.(shift_left (of_int (exponent + 1023)) 52)
    else Int64.float_of_bits Int64.(shift_left 1L (exponent + 1074))

let scalb value scale_factor =
  if Float.equal value 0.0 || not (Float.is_finite value) then value
  else
    let scale_factor = max (-2099) (min 2099 scale_factor) in
    let scale_up = Float.pow 2.0 512.0 in
    let scale_down = Float.pow 2.0 (-512.0) in
    let rec scale result remaining =
      if remaining > 512 then scale (result *. scale_up) (remaining - 512)
      else if remaining < -512 then
        scale (result *. scale_down) (remaining + 512)
      else result *. Float.pow 2.0 (float_of_int remaining)
    in
    scale value scale_factor
