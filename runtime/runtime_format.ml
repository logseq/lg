type argument =
  | Text of string | Integer of int | Decimal of float
  | Boolean of bool | Character of char | Nil

let option convert = function None -> Nil | Some value -> convert value

let invalid message = invalid_arg ("format: " ^ message)

type decimal = { digits : string; point : int }

let decimal_of_float value =
  let rec shortest precision =
    let candidate = Printf.sprintf "%.*g" precision value in
    if precision = 17 || float_of_string candidate = value then candidate
    else shortest (precision + 1)
  in
  let source = shortest 2 in
  let mantissa, exponent = match String.index_opt source 'e' with
    | None -> source, 0
    | Some index -> String.sub source 0 index,
        int_of_string (String.sub source (index + 1) (String.length source - index - 1))
  in
  let point = Option.value ~default:(String.length mantissa) (String.index_opt mantissa '.') in
  let digits = String.concat "" (String.split_on_char '.' mantissa) in
  let leading = ref 0 in
  while !leading < String.length digits - 1 && digits.[!leading] = '0' do incr leading done;
  let digits = String.sub digits !leading (String.length digits - !leading) in
  if digits = "0" then {digits; point = 1}
  else {digits; point = point + exponent - !leading}

let round_decimal value keep =
  if keep >= String.length value.digits then value
  else if keep < 0 then {digits = "0"; point = 1}
  else
    let carry = value.digits.[keep] >= '5' in
    if keep = 0 then
      if carry then {digits = "1"; point = value.point + 1}
      else {digits = "0"; point = 1}
    else
      let digits = Bytes.of_string (String.sub value.digits 0 keep) in
      let index = ref (keep - 1) in
      if carry then (
        while !index >= 0 && Bytes.get digits !index = '9' do
          Bytes.set digits !index '0'; decr index
        done;
        if !index >= 0 then Bytes.set digits !index (Char.chr (Char.code (Bytes.get digits !index) + 1)));
      if carry && !index < 0 then {digits = "1" ^ Bytes.to_string digits; point = value.point + 1}
      else {value with digits = Bytes.to_string digits}

let decimal_digit value index =
  if index < 0 || index >= String.length value.digits then '0' else value.digits.[index]

let fixed_decimal value precision alternate =
  let whole = if value.point <= 0 then "0" else String.init value.point (decimal_digit value) in
  let fraction = String.init precision (fun index -> decimal_digit value (value.point + index)) in
  whole ^ (if precision > 0 || alternate then "." ^ fraction else "")

let scientific_decimal value precision alternate =
  let exponent = value.point - 1 in
  let mantissa = fixed_decimal {value with point = 1} precision alternate in
  mantissa ^ "e" ^ (if exponent < 0 then "-" else "+") ^ Printf.sprintf "%02d" (abs exponent)

let format_decimal kind precision alternate value =
  let decimal = decimal_of_float value in
  match kind with
  | 'f' -> fixed_decimal (round_decimal decimal (decimal.point + precision)) precision alternate
  | 'e' -> scientific_decimal (round_decimal decimal (precision + 1)) precision alternate
  | _ ->
      let precision = max 1 precision in
      let rounded = round_decimal decimal precision in
      let exponent = rounded.point - 1 in
      if exponent < -4 || exponent >= precision then scientific_decimal rounded (precision - 1) false
      else fixed_decimal rounded (max 0 (precision - rounded.point)) false

let text = function
  | Text value -> value | Integer value -> string_of_int value
  | Decimal value ->
      if Float.is_nan value then "NaN"
      else if value = infinity then "Infinity"
      else if value = neg_infinity then "-Infinity"
      else
        let decimal = decimal_of_float (abs_float value) in
        let exponent = decimal.point - 1 in
        let rendered =
          if exponent < -3 || exponent >= 7 then
            fixed_decimal {decimal with point = 1}
              (max 1 (String.length decimal.digits - 1)) false
            ^ "E" ^ string_of_int exponent
          else fixed_decimal decimal
            (max 1 (String.length decimal.digits - decimal.point)) false in
        (if Float.sign_bit value then "-" else "") ^ rendered
  | Boolean value -> string_of_bool value
  | Character value -> String.make 1 value | Nil -> "null"

let group_digits value =
  let boundary = min
    (Option.value ~default:(String.length value) (String.index_opt value '.'))
    (Option.value ~default:(String.length value) (String.index_opt value 'e')) in
  let buffer = Buffer.create (String.length value + 8) in
  String.iteri (fun index char ->
    if index > 0 && index < boundary && (boundary - index) mod 3 = 0 then
      Buffer.add_char buffer ',';
    Buffer.add_char buffer char) value;
  Buffer.contents buffer

let truncate_utf16 limit source =
  let rec loop index remaining =
    if index = String.length source || remaining = 0 then String.sub source 0 index
    else
      let scalar, next = Runtime_string.utf8_scalar_at source index in
      let units = if scalar > 0xffff then 2 else 1 in
      if remaining >= units then loop next (remaining - units)
      else
        let high = 0xd800 lor ((scalar - 0x10000) lsr 10) in
        (* Preserve a truncated UTF-16 surrogate as WTF-8 on byte-string targets. *)
        String.sub source 0 index ^ String.init 3 (function
          | 0 -> Char.chr (0xe0 lor (high lsr 12))
          | 1 -> Char.chr (0x80 lor ((high lsr 6) land 0x3f))
          | _ -> Char.chr (0x80 lor (high land 0x3f)))
  in loop 0 limit

let format_with_string_ops ~text_length ~truncate source arguments =
  let arguments = Array.of_list arguments in
  let length = String.length source in
  let output = Buffer.create length in
  Runtime_format_spec.iter source
    ~text:(Buffer.add_char output)
    ~conversion:(fun spec ->
      let conversion = spec.Runtime_format_spec.code in
      let kind = Char.lowercase_ascii conversion in
      let has flag = List.mem flag spec.flags in
      let width = spec.width and precision = spec.precision in
      let argument = match spec.argument with
        | None -> Nil
        | Some selected ->
            if selected >= Array.length arguments then invalid "missing argument";
            arguments.(selected)
      in
      let negative = match argument with
        | Integer n -> n < 0 | Decimal n -> Float.sign_bit n | _ -> false in
      let numeric = String.contains "doxefg" kind && argument <> Nil in
      let value = match kind, argument with
        | '%', _ -> "%" | 'n', _ -> "\n"
        | 'b', Nil | 'b', Boolean false -> "false" | 'b', _ -> "true"
        | 's', arg -> text arg | _, Nil -> "null"
        | 'c', Character char -> String.make 1 char
        | 'c', Integer n when Uchar.is_valid n ->
            let buffer = Buffer.create 4 in Buffer.add_utf_8_uchar buffer (Uchar.of_int n); Buffer.contents buffer
        | 'd', Integer n ->
            let value = Int64.to_string (Int64.abs (Int64.of_int n)) in
            if has ',' then group_digits value else value
        | 'o', Integer n -> Printf.sprintf "%Lo" (Int64.of_int n)
        | 'x', Integer n -> Printf.sprintf "%Lx" (Int64.of_int n)
        | ('e' | 'f' | 'g'), Decimal n ->
            if Float.is_nan n then "NaN" else if not (Float.is_finite n) then "Infinity"
            else
              let precision = Option.value ~default:6 precision in
              let value = format_decimal kind precision (has '#') (abs_float n) in
              if has ',' then group_digits value else value
        | _ -> invalid "argument type does not match conversion"
      in
      let value =
        if String.contains "sb" kind then match precision with
          | Some n when n < text_length value -> truncate n value | _ -> value
        else value
      in
      let signed = numeric && String.contains "defg" kind && value <> "NaN" in
      let prefix, suffix =
        if signed && negative && has '(' then "(", ")"
        else if signed && negative then "-", ""
        else if signed && has '+' then "+", ""
        else if signed && has ' ' then " ", ""
        else if numeric && has '#' && kind = 'x' then "0x", ""
        else if numeric && has '#' && kind = 'o' then "0", ""
        else "", ""
      in
      let needed = max 0 (Option.value ~default:0 width - text_length prefix - text_length value - text_length suffix) in
      let zero = numeric && has '0' && value <> "NaN" && value <> "Infinity" in
      let padding = String.make needed (if zero then '0' else ' ') in
      let result =
        if has '-' then prefix ^ value ^ suffix ^ padding
        else if zero then prefix ^ padding ^ value ^ suffix
        else padding ^ prefix ^ value ^ suffix
      in
      Buffer.add_string output (if conversion <> kind then String.uppercase_ascii result else result));
  Buffer.contents output

let format = format_with_string_ops
  ~text_length:Runtime_string.utf16_length ~truncate:truncate_utf16
