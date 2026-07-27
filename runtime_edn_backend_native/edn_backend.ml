module Melange_edn = Melange_edn_native

type t =
  | Nil
  | Bool of bool
  | String of string
  | Char of Uchar.t
  | Symbol of string
  | Keyword of string
  | Int of int64
  | Bigint of string
  | Float of float
  | Decimal of string
  | Ratio of string
  | Regex of string
  | List of t array
  | Vector of t array
  | Map of (t * t) array
  | Set of t array
  | Tagged of string * t

let rec of_edn (Melange_edn.Any value) =
  match value with
  | Melange_edn.Nil -> Nil
  | Melange_edn.Bool value -> Bool value
  | Melange_edn.String value -> String value
  | Melange_edn.Char value -> Char value
  | Melange_edn.Symbol value -> Symbol value
  | Melange_edn.Keyword value -> Keyword (Melange_edn.keyword_to_string value)
  | Melange_edn.Int value -> Int value
  | Melange_edn.Bigint value -> Bigint value
  | Melange_edn.Float value -> Float value
  | Melange_edn.Decimal value -> Decimal value
  | Melange_edn.Ratio value -> Ratio value
  | Melange_edn.Regex value -> Regex value
  | Melange_edn.List values -> List (Array.map of_edn values)
  | Melange_edn.Vector values -> Vector (Array.map of_edn values)
  | Melange_edn.Map entries ->
      Map (Array.map (fun (key, value) -> (of_edn key, of_edn value)) entries)
  | Melange_edn.Set values -> Set (Array.map of_edn values)
  | Melange_edn.Tagged (tag, value) -> Tagged (tag, of_edn value)

let rec to_edn = function
  | Nil -> Melange_edn.any Melange_edn.nil
  | Bool value -> Melange_edn.any (Melange_edn.bool value)
  | String value -> Melange_edn.any (Melange_edn.string value)
  | Char value -> Melange_edn.any (Melange_edn.char value)
  | Symbol value -> Melange_edn.any (Melange_edn.symbol value)
  | Keyword value -> Melange_edn.any (Melange_edn.keyword value)
  | Int value -> Melange_edn.any (Melange_edn.int value)
  | Bigint value -> Melange_edn.any (Melange_edn.bigint value)
  | Float value -> Melange_edn.any (Melange_edn.float value)
  | Decimal value -> Melange_edn.any (Melange_edn.decimal value)
  | Ratio value -> Melange_edn.any (Melange_edn.ratio value)
  | Regex value -> Melange_edn.any (Melange_edn.regex value)
  | List values ->
      Melange_edn.any (Melange_edn.list (Array.to_list (Array.map to_edn values)))
  | Vector values ->
      Melange_edn.any
        (Melange_edn.vector (Array.to_list (Array.map to_edn values)))
  | Map entries ->
      Melange_edn.any
        (Melange_edn.map
           (Array.to_list
              (Array.map (fun (key, value) -> (to_edn key, to_edn value)) entries)))
  | Set values ->
      Melange_edn.any (Melange_edn.set (Array.to_list (Array.map to_edn values)))
  | Tagged (tag, value) -> Melange_edn.any (Melange_edn.tagged tag (to_edn value))

let of_edn_string source = Melange_edn.of_edn_string source |> of_edn
let to_edn_string value = value |> to_edn |> Melange_edn.to_edn_string

let min_safe_json_integer = -9007199254740991.
let max_safe_json_integer = 9007199254740991.

let json_number value =
  if
    value >= min_safe_json_integer
    && value <= max_safe_json_integer
    && Float.is_integer value
  then Int (Int64.of_float value)
  else Float value

let rec of_json = function
  | `Null -> Nil
  | `Bool value -> Bool value
  | `String value -> String value
  | `Int value -> Int (Int64.of_int value)
  | `Intlit value -> Bigint value
  | `Float value -> json_number value
  | `List values -> Vector (Array.of_list (List.map of_json values))
  | `Assoc entries ->
      Map
        (Array.of_list
           (List.map
              (fun (key, value) -> (String key, of_json value))
              entries))

let of_json_string source = source |> Yojson.Safe.from_string |> of_json

let add_json_string buffer value =
  Yojson.Safe.write_string buffer value

let add_json_key buffer = function
  | String value | Symbol value -> add_json_string buffer value
  | Keyword value -> add_json_string buffer value
  | _ ->
      invalid_arg
        "EDN map contains a key that cannot be encoded as a JSON object name"

let add_json_char buffer value =
  value
  |> Melange_edn.char
  |> Melange_edn.any
  |> Melange_edn.to_edn_string
  |> add_json_string buffer

let add_json_float buffer value =
  match classify_float value with
  | FP_nan -> add_json_string buffer "NaN"
  | FP_infinite when value > 0. -> add_json_string buffer "Infinity"
  | FP_infinite -> add_json_string buffer "-Infinity"
  | FP_normal | FP_subnormal | FP_zero ->
      Buffer.add_string buffer (Yojson.Safe.to_string (`Float value))

let add_json_int buffer value =
  let min_safe_json_integer = -9007199254740991L in
  let max_safe_json_integer = 9007199254740991L in
  if value >= min_safe_json_integer && value <= max_safe_json_integer then
    Buffer.add_string buffer (Int64.to_string value)
  else add_json_string buffer (Int64.to_string value)

let rec add_json_value buffer = function
  | Nil -> Buffer.add_string buffer "null"
  | Bool true -> Buffer.add_string buffer "true"
  | Bool false -> Buffer.add_string buffer "false"
  | String value | Symbol value | Bigint value | Decimal value | Ratio value
  | Regex value ->
      add_json_string buffer value
  | Char value -> add_json_char buffer value
  | Keyword value -> add_json_string buffer (":" ^ value)
  | Int value -> add_json_int buffer value
  | Float value -> add_json_float buffer value
  | List values | Vector values | Set values ->
      add_json_array buffer values
  | Map entries -> add_json_object buffer entries
  | Tagged (tag, value) ->
      Buffer.add_string buffer "{\"tag\":";
      add_json_string buffer tag;
      Buffer.add_string buffer ",\"value\":";
      add_json_value buffer value;
      Buffer.add_char buffer '}'

and add_json_array buffer values =
  Buffer.add_char buffer '[';
  Array.iteri
    (fun index value ->
      if index > 0 then Buffer.add_char buffer ',';
      add_json_value buffer value)
    values;
  Buffer.add_char buffer ']'

and add_json_object buffer entries =
  Buffer.add_char buffer '{';
  Array.iteri
    (fun index (key, value) ->
      if index > 0 then Buffer.add_char buffer ',';
      add_json_key buffer key;
      Buffer.add_char buffer ':';
      add_json_value buffer value)
    entries;
  Buffer.add_char buffer '}'

let to_json_string value =
  let buffer = Buffer.create 256 in
  add_json_value buffer value;
  Buffer.contents buffer

let regex_valid pattern =
  let _ = Re.Perl.compile_pat pattern in
  true

let regex_find pattern source =
  Re.execp (Re.Perl.compile_pat pattern) source
