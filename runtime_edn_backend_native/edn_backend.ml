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
  | Json_source of string

type json = Yojson.Safe.t

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
  | Json_source source -> Melange_edn.of_edn_string source

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
let of_json_source source = Json_source source
let json_of_string source = Yojson.Safe.from_string source

let json_field json name =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt name fields with
      | Some value -> value
      | None -> invalid_arg ("missing JSON field " ^ name))
  | _ -> invalid_arg "expected JSON object"

let json_field_opt json name =
  match json with
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> invalid_arg "expected JSON object"

let json_array = function
  | `List values -> Array.of_list values
  | _ -> invalid_arg "expected JSON array"

let json_int = function
  | `Int value -> value
  | `Intlit value -> int_of_string value
  | `Float value when Float.is_integer value -> int_of_float value
  | _ -> invalid_arg "expected JSON integer"

let json_string = function
  | `String value -> value
  | _ -> invalid_arg "expected JSON string"

let json_is_null = function `Null -> true | _ -> false
let json_to_edn = of_json

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
  | Json_source source -> Buffer.add_string buffer source

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

type regex_match = { captures : string option array }

let regex_match groups =
  {
    captures =
      Array.init (Re.Group.nb_groups groups) (Re.Group.get_opt groups);
  }

let regex_find_groups ~pattern source =
  Re.exec_opt (Re.Perl.compile_pat pattern) source
  |> Option.map regex_match

let regex_matches_groups ~pattern source =
  Re.exec_opt
    (Re.compile (Re.whole_string (Re.Perl.re pattern)))
    source
  |> Option.map regex_match

let regex_all_groups ~pattern source =
  Re.all (Re.Perl.compile_pat pattern) source
  |> List.map regex_match
  |> Array.of_list

let replacement_text source replacement groups =
  let group_count = Re.Group.nb_groups groups in
  let buffer = Buffer.create (String.length replacement) in
  let add_group index =
    match Re.Group.get_opt groups index with
    | Some value -> Buffer.add_string buffer value
    | None -> ()
  in
  let rec loop index =
    if index >= String.length replacement then Buffer.contents buffer
    else if
      replacement.[index] = '$' && index + 1 < String.length replacement
    then
      match replacement.[index + 1] with
      | '$' ->
          Buffer.add_char buffer '$';
          loop (index + 2)
      | '&' ->
          add_group 0;
          loop (index + 2)
      | '`' ->
          let start = Re.Group.start groups 0 in
          Buffer.add_substring buffer source 0 start;
          loop (index + 2)
      | '\'' ->
          let stop = Re.Group.stop groups 0 in
          Buffer.add_substring buffer source stop (String.length source - stop);
          loop (index + 2)
      | '1' .. '9' as digit ->
          let first = Char.code digit - Char.code '0' in
          let capture, consumed =
            if index + 2 < String.length replacement then
              match replacement.[index + 2] with
              | '0' .. '9' as second ->
                  let candidate =
                    (first * 10) + Char.code second - Char.code '0'
                  in
                  if candidate < group_count then (candidate, 3)
                  else (first, 2)
              | _ -> (first, 2)
            else (first, 2)
          in
          if capture < group_count then add_group capture
          else (
            Buffer.add_char buffer '$';
            Buffer.add_char buffer digit);
          loop (index + consumed)
      | _ ->
          Buffer.add_char buffer replacement.[index];
          loop (index + 1)
    else (
      Buffer.add_char buffer replacement.[index];
      loop (index + 1))
  in
  loop 0

let regex_replace ~all ~pattern ~replacement source =
  let regex = Re.Perl.compile_pat pattern in
  Re.replace ~all regex
    ~f:(replacement_text source replacement)
    source

let drop_trailing_empty values =
  match values with
  | [ Some "" ] -> values
  | values ->
      let rec drop = function
        | Some "" :: rest -> drop rest
        | values -> values
      in
      values |> List.rev |> drop |> List.rev

let full_regex_split regex source =
  let rec captures groups index values =
    if index >= Re.Group.nb_groups groups then List.rev values
    else captures groups (index + 1) (Re.Group.get_opt groups index :: values)
  in
  let rec collect cursor values = function
    | [] ->
        List.rev
          (Some
             (String.sub source cursor (String.length source - cursor))
          :: values)
    | groups :: rest ->
        let start = Re.Group.start groups 0 in
        let stop = Re.Group.stop groups 0 in
        let text = Some (String.sub source cursor (start - cursor)) in
        let captures = captures groups 1 [] in
        collect stop (List.rev_append captures (text :: values)) rest
  in
  collect 0 [] (Re.all regex source)

let limited_regex_split regex limit source =
  let rec collect cursor remaining values = function
    | _ when remaining = 1 ->
        List.rev
          (Some
             (String.sub source cursor (String.length source - cursor))
          :: values)
    | [] ->
        List.rev
          (Some
             (String.sub source cursor (String.length source - cursor))
          :: values)
    | groups :: rest ->
        let start = Re.Group.start groups 0 in
        let stop = Re.Group.stop groups 0 in
        collect stop (remaining - 1)
          (Some (String.sub source cursor (start - cursor)) :: values)
          rest
  in
  collect 0 limit [] (Re.all regex source)

let regex_split ~pattern ~limit source =
  let regex = Re.Perl.compile_pat pattern in
  let values =
    match limit with
    | Some value when value > 0 -> limited_regex_split regex value source
    | _ -> full_regex_split regex source
  in
  let values =
    match limit with
    | Some value when value < 0 -> values
    | None | Some _ -> drop_trailing_empty values
  in
  Array.of_list values
