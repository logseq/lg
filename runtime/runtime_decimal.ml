type rounding_mode =
  | Up
  | Down
  | Ceiling
  | Floor
  | Half_up
  | Half_down
  | Half_even
  | Unnecessary

type context = { precision : int; rounding_mode : rounding_mode }

type t = {
  negative : bool;
  digits : string;
  scale : int;
}

let current_context = ref None

let all_zero source start =
  let rec loop index =
    index >= String.length source
    || (source.[index] = '0' && loop (index + 1))
  in
  loop start

let strip_leading_zeros digits =
  let rec first_nonzero index =
    if index >= String.length digits - 1 || digits.[index] <> '0' then index
    else first_nonzero (index + 1)
  in
  let start = first_nonzero 0 in
  String.sub digits start (String.length digits - start)

let normalize value =
  let digits = strip_leading_zeros value.digits in
  let rec trailing index scale =
    if index > 0 && digits.[index] = '0' then trailing (index - 1) (scale - 1)
    else (index, scale)
  in
  let last, scale = trailing (String.length digits - 1) value.scale in
  let digits = String.sub digits 0 (last + 1) in
  if all_zero digits 0 then { negative = false; digits = "0"; scale = 0 }
  else { value with digits; scale }

let invalid source = invalid_arg ("invalid decimal literal: " ^ source)

let of_string source =
  let length = String.length source in
  if length = 0 then invalid source;
  let negative, start =
    match source.[0] with
    | '-' -> (true, 1)
    | '+' -> (false, 1)
    | _ -> (false, 0)
  in
  let exponent_index =
    let rec find index =
      if index >= length then length
      else if source.[index] = 'e' || source.[index] = 'E' then index
      else find (index + 1)
    in
    find start
  in
  let exponent =
    if exponent_index = length then 0
    else
      match
        int_of_string_opt
          (String.sub source (exponent_index + 1) (length - exponent_index - 1))
      with
      | Some exponent -> exponent
      | None -> invalid source
  in
  let coefficient = String.sub source start (exponent_index - start) in
  let decimal_index = String.index_opt coefficient '.' in
  let integer, fraction =
    match decimal_index with
    | None -> (coefficient, "")
    | Some index ->
        ( String.sub coefficient 0 index,
          String.sub coefficient (index + 1)
            (String.length coefficient - index - 1) )
  in
  let digits = integer ^ fraction in
  if digits = "" || not (String.for_all (function '0' .. '9' -> true | _ -> false) digits)
  then invalid source;
  normalize { negative; digits; scale = String.length fraction - exponent }

let of_int value = of_string (string_of_int value)

let of_float value =
  if not (Float.is_finite value) then
    invalid_arg "decimal expects a finite numeric value";
  of_string (string_of_float value)

let abs value = if value.negative then { value with negative = false } else value

let negate value =
  if String.equal value.digits "0" then value
  else { value with negative = not value.negative }

let pad_right source count =
  if count = 0 then source else source ^ String.make count '0'

let pad_left source length =
  let missing = length - String.length source in
  if missing <= 0 then source else String.make missing '0' ^ source

let compare_digits left right =
  let left = strip_leading_zeros left and right = strip_leading_zeros right in
  let length_comparison = Int.compare (String.length left) (String.length right) in
  if length_comparison <> 0 then length_comparison else String.compare left right

let add_digits left right =
  let length = max (String.length left) (String.length right) in
  let left = pad_left left length and right = pad_left right length in
  let result = Bytes.make (length + 1) '0' in
  let carry = ref 0 in
  for index = length - 1 downto 0 do
    let sum =
      (Char.code left.[index] - Char.code '0')
      + (Char.code right.[index] - Char.code '0')
      + !carry
    in
    Bytes.set result (index + 1) (Char.chr (Char.code '0' + (sum mod 10)));
    carry := sum / 10
  done;
  Bytes.set result 0 (Char.chr (Char.code '0' + !carry));
  strip_leading_zeros (Bytes.to_string result)

let subtract_digits left right =
  let length = max (String.length left) (String.length right) in
  let left = pad_left left length and right = pad_left right length in
  let result = Bytes.make length '0' in
  let borrow = ref 0 in
  for index = length - 1 downto 0 do
    let difference =
      (Char.code left.[index] - Char.code '0')
      - (Char.code right.[index] - Char.code '0')
      - !borrow
    in
    let digit, next_borrow =
      if difference < 0 then (difference + 10, 1) else (difference, 0)
    in
    Bytes.set result index (Char.chr (Char.code '0' + digit));
    borrow := next_borrow
  done;
  strip_leading_zeros (Bytes.to_string result)

let aligned_digits left right =
  let scale = max left.scale right.scale in
  ( pad_right left.digits (scale - left.scale),
    pad_right right.digits (scale - right.scale),
    scale )

let compare left right =
  let left = normalize left and right = normalize right in
  if left.negative <> right.negative then if left.negative then -1 else 1
  else
    let left_digits, right_digits, _ = aligned_digits left right in
    let magnitude = compare_digits left_digits right_digits in
    if left.negative then -magnitude else magnitude

let increment digits =
  let bytes = Bytes.of_string digits in
  let rec carry index =
    if index < 0 then "1" ^ Bytes.to_string bytes
    else if Bytes.get bytes index = '9' then (
      Bytes.set bytes index '0';
      carry (index - 1))
    else (
      Bytes.set bytes index (Char.chr (Char.code (Bytes.get bytes index) + 1));
      Bytes.to_string bytes)
  in
  carry (Bytes.length bytes - 1)

let round context value =
  let digit_count = String.length value.digits in
  if digit_count <= context.precision then value
  else
    let cut = digit_count - context.precision in
    let kept = String.sub value.digits 0 context.precision in
    let discarded = String.sub value.digits context.precision cut in
    let discarded_nonzero = not (all_zero discarded 0) in
    let first = discarded.[0] in
    let later_nonzero = not (all_zero discarded 1) in
    let last_kept_is_odd =
      (Char.code kept.[String.length kept - 1] - Char.code '0') mod 2 = 1
    in
    let increment_result =
      match context.rounding_mode with
      | Up -> discarded_nonzero
      | Down -> false
      | Ceiling -> (not value.negative) && discarded_nonzero
      | Floor -> value.negative && discarded_nonzero
      | Half_up -> first >= '5'
      | Half_down -> first > '5' || (first = '5' && later_nonzero)
      | Half_even ->
          first > '5'
          || (first = '5' && (later_nonzero || last_kept_is_odd))
      | Unnecessary ->
          if discarded_nonzero then invalid_arg "rounding necessary";
          false
    in
    let digits = if increment_result then increment kept else kept in
    normalize { value with digits; scale = value.scale - cut }

let apply_context value =
  match !current_context with None -> value | Some context -> round context value

let add left right =
  let left_digits, right_digits, scale = aligned_digits left right in
  let negative, digits =
    if left.negative = right.negative then
      (left.negative, add_digits left_digits right_digits)
    else
      match compare_digits left_digits right_digits with
      | 0 -> (false, "0")
      | comparison when comparison > 0 ->
          (left.negative, subtract_digits left_digits right_digits)
      | _ -> (right.negative, subtract_digits right_digits left_digits)
  in
  apply_context (normalize { negative; digits; scale })

let subtract left right = add left (negate right)

let multiply_digits left right =
  let left_length = String.length left and right_length = String.length right in
  let result = Array.make (left_length + right_length) 0 in
  for left_index = left_length - 1 downto 0 do
    let left_digit = Char.code left.[left_index] - Char.code '0' in
    for right_index = right_length - 1 downto 0 do
      let right_digit = Char.code right.[right_index] - Char.code '0' in
      let position = left_index + right_index + 1 in
      let total = result.(position) + (left_digit * right_digit) in
      result.(position) <- total mod 10;
      result.(position - 1) <- result.(position - 1) + (total / 10)
    done
  done;
  let buffer = Buffer.create (Array.length result) in
  let started = ref false in
  Array.iter
    (fun digit ->
      if digit <> 0 || !started then (
        started := true;
        Buffer.add_char buffer (Char.chr (Char.code '0' + digit))))
    result;
  if !started then Buffer.contents buffer else "0"

let multiply left right =
  apply_context
    (normalize
       {
         negative = left.negative <> right.negative;
         digits = multiply_digits left.digits right.digits;
         scale = left.scale + right.scale;
       })

let divide_digit numerator denominator =
  let rec loop quotient remainder =
    if compare_digits remainder denominator < 0 then (quotient, remainder)
    else loop (quotient + 1) (subtract_digits remainder denominator)
  in
  loop 0 numerator

let divide_integer numerator denominator =
  let quotient = Buffer.create (String.length numerator) in
  let remainder = ref "0" in
  String.iter
    (fun digit ->
      let next = strip_leading_zeros (!remainder ^ String.make 1 digit) in
      let quotient_digit, next_remainder = divide_digit next denominator in
      Buffer.add_char quotient (Char.chr (Char.code '0' + quotient_digit));
      remainder := next_remainder)
    numerator;
  (strip_leading_zeros (Buffer.contents quotient), !remainder)

let divide left right =
  if String.equal right.digits "0" then invalid_arg "division by zero";
  let scale_shift = right.scale - left.scale in
  let numerator, denominator =
    if scale_shift >= 0 then (pad_right left.digits scale_shift, right.digits)
    else (left.digits, pad_right right.digits (-scale_shift))
  in
  let integer, initial_remainder = divide_integer numerator denominator in
  let fraction = Buffer.create 16 in
  let seen = Hashtbl.create 16 in
  let rec fractional remainder =
    if String.equal remainder "0" then ()
    else if Hashtbl.mem seen remainder then
      invalid_arg "non-terminating decimal expansion"
    else (
      Hashtbl.add seen remainder ();
      let digit, remainder = divide_digit (pad_right remainder 1) denominator in
      Buffer.add_char fraction (Char.chr (Char.code '0' + digit));
      fractional remainder)
  in
  fractional initial_remainder;
  let fraction = Buffer.contents fraction in
  apply_context
    (normalize
       {
         negative = left.negative <> right.negative;
         digits = integer ^ fraction;
         scale = String.length fraction;
       })

let equal left right =
  let left = normalize left and right = normalize right in
  left.negative = right.negative
  && left.scale = right.scale
  && String.equal left.digits right.digits

let is_integer value = (normalize value).scale <= 0

let to_string value =
  let value = normalize value in
  let sign = if value.negative then "-" else "" in
  let length = String.length value.digits in
  if value.scale <= 0 then
    sign ^ value.digits ^ String.make (-value.scale) '0'
  else if value.scale < length then
    let integer_length = length - value.scale in
    sign ^ String.sub value.digits 0 integer_length ^ "."
    ^ String.sub value.digits integer_length value.scale
  else
    sign ^ "0." ^ String.make (value.scale - length) '0' ^ value.digits

let to_edn_string value = to_string value ^ "M"

let to_float value = float_of_string (to_string value)

let is_safe_integer value =
  is_integer value && Float.abs (to_float value) <= 9007199254740991.

let rounding_mode_of_string = function
  | "UP" -> Up
  | "DOWN" -> Down
  | "CEILING" -> Ceiling
  | "FLOOR" -> Floor
  | "HALF_UP" -> Half_up
  | "HALF_DOWN" -> Half_down
  | "HALF_EVEN" -> Half_even
  | "UNNECESSARY" -> Unnecessary
  | mode -> invalid_arg ("unsupported rounding mode: " ^ mode)

let with_context precision rounding_mode thunk =
  if precision <= 0 then invalid_arg "precision must be positive";
  let previous = !current_context in
  current_context := Some { precision; rounding_mode };
  Fun.protect ~finally:(fun () -> current_context := previous) thunk
