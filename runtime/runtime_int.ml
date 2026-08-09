let hash_combine seed hash_value =
  let open Int32 in
  let seed = of_int seed in
  let hash = of_int hash_value in
  let mixed =
    add hash
      (add (-1640531527l)
         (add (shift_left seed 6) (shift_right seed 2)))
  in
  to_int (logxor seed mixed)

let popcount_32 value =
  let value = value - ((value lsr 1) land 0x55555555) in
  let value = (value land 0x33333333) + ((value lsr 2) land 0x33333333) in
  let value = (value + (value lsr 4)) land 0x0f0f0f0f in
  ((value * 0x01010101) lsr 24) land 0xff

let clojure_mod left right =
  let remainder = left mod right in
  if remainder = 0 || (remainder > 0) = (right > 0) then remainder
  else remainder + right

let int_quot = ( / )
let int_rem = ( mod )
let int_max = Stdlib.max
let int_min = Stdlib.min
let int_zero value = value = 0
let int_positive value = value > 0
let int_negative value = value < 0
let int_even value = value mod 2 = 0
let int_odd value = value mod 2 <> 0
let bit_and left right = left land right
let bit_or left right = left lor right
let bit_xor left right = left lxor right
let shift_left value count = value lsl count
let shift_right value count = value asr count
let logical_shift_right value count = value lsr count

let int32 value = Int32.of_int value |> Int32.to_int

let shift_left_32 value count =
  Int32.shift_left (Int32.of_int value) (count land 31) |> Int32.to_int

let logical_shift_right_32 value count =
  Int64.of_int value
  |> Int64.logand 0xffffffffL
  |> fun value -> Int64.shift_right_logical value (count land 31)
  |> Int64.to_int

let format_hex value width =
  if width < 0 || width > Sys.max_string_length then
    invalid_arg "hex width is out of range";
  let value = Int64.of_int value in
  let digits = "0123456789abcdef" in
  let rec encode value encoded =
    if value = 0L then encoded
    else
      let digit = Int64.logand value 15L |> Int64.to_int in
      encode (Int64.shift_right_logical value 4) (digits.[digit] :: encoded)
  in
  let encoded = if value = 0L then [ '0' ] else encode value [] in
  let encoded = String.of_seq (List.to_seq encoded) in
  let padding = max 0 (width - String.length encoded) in
  String.make padding '0' ^ encoded
