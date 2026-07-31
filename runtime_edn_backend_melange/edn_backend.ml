module Melange_edn = Melange_edn_melange

type t =
  | Nil
  | Bool of bool
  | String of string
  | Char of Uchar.t
  | Symbol of string
  | Keyword of string
  | Small_int of int
  | Int of int64
  | Bigint of string
  | Float of float
  | Decimal of string
  | Ratio of string
  | Regex of string
  | List of t array
  | Vector of t array
  | Int4_vector of int * int * t * int
  | Int_vector of int array
  | Map of (t * t) array
  | Set of t array
  | Tagged of string * t
  | Json_source of string

type json = Js.Json.t

let integer value =
  let narrowed = Int64.to_int value in
  if Int64.equal (Int64.of_int narrowed) value then Small_int narrowed
  else Int value

let rec of_edn (Melange_edn.Any value) =
  match value with
  | Melange_edn.Nil -> Nil
  | Melange_edn.Bool value -> Bool value
  | Melange_edn.String value -> String value
  | Melange_edn.Char value -> Char value
  | Melange_edn.Symbol value -> Symbol value
  | Melange_edn.Keyword value -> Keyword (Melange_edn.keyword_to_string value)
  | Melange_edn.Int value -> integer value
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
  | Small_int value -> Melange_edn.any (Melange_edn.int (Int64.of_int value))
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
  | Int4_vector (first, second, third, fourth) ->
      Melange_edn.any
        (Melange_edn.vector
           [
             Melange_edn.any (Melange_edn.int (Int64.of_int first));
             Melange_edn.any (Melange_edn.int (Int64.of_int second));
             to_edn third;
             Melange_edn.any (Melange_edn.int (Int64.of_int fourth));
           ])
  | Int_vector values ->
      Melange_edn.any
        (Melange_edn.vector
           (values
           |> Array.map (fun value ->
                  Melange_edn.any (Melange_edn.int (Int64.of_int value)))
           |> Array.to_list))
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
  then integer (Int64.of_float value)
  else Float value

let rec of_json json =
  match Js.Json.decodeString json with
  | Some value -> String value
  | None -> (
      match Js.Json.decodeNumber json with
      | Some value -> json_number value
      | None -> (
          match Js.Json.decodeArray json with
          | Some values -> Vector (Array.map of_json values)
          | None -> (
              match Js.Json.decodeObject json with
              | Some entries ->
                  Map
                    (Array.map
                       (fun (key, value) -> (String key, of_json value))
                       (Js.Dict.entries entries))
              | None -> (
                  match Js.Json.classify json with
                  | JSONNull -> Nil
                  | JSONFalse -> Bool false
                  | JSONTrue -> Bool true
                  | JSONString _ | JSONNumber _ | JSONArray _ | JSONObject _ ->
                      assert false))))

let of_json_string source = source |> Js.Json.parseExn |> of_json
let of_json_source source = Json_source source
let json_of_string = Js.Json.parseExn

let json_field json name =
  match Js.Json.decodeObject json with
  | Some fields -> (
      match Js.Dict.get fields name with
      | Some value -> value
      | None -> invalid_arg ("missing JSON field " ^ name))
  | None -> invalid_arg "expected JSON object"

let json_field_opt json name =
  match Js.Json.decodeObject json with
  | Some fields -> Js.Dict.get fields name
  | None -> invalid_arg "expected JSON object"

let json_array json =
  match Js.Json.decodeArray json with
  | Some values -> values
  | None -> invalid_arg "expected JSON array"

let with_json_array4 json f =
  match Js.Json.decodeArray json with
  | Some values when Array.length values = 4 ->
      f values.(0) values.(1) values.(2) values.(3)
  | _ -> invalid_arg "expected JSON array of length 4"

let json_int json =
  match Js.Json.decodeNumber json with
  | Some value when Float.is_integer value -> int_of_float value
  | _ -> invalid_arg "expected JSON integer"

let json_string json =
  match Js.Json.decodeString json with
  | Some value -> value
  | _ -> invalid_arg "expected JSON string"

let json_int_opt json =
  match Js.Json.decodeNumber json with
  | Some value when Float.is_integer value -> Some (int_of_float value)
  | _ -> None

let json_float_opt json =
  match Js.Json.decodeNumber json with
  | Some value when not (Float.is_integer value) -> Some value
  | _ -> None

let json_string_opt = Js.Json.decodeString

let json_bool_opt json =
  match Js.Json.classify json with
  | JSONFalse -> Some false
  | JSONTrue -> Some true
  | _ -> None

let json_array_opt = Js.Json.decodeArray

let json_is_null json = Js.Json.test json Null

let json_to_edn = of_json

let json_key = function
  | String value | Symbol value | Keyword value -> value
  | _ ->
      invalid_arg
        "EDN map contains a key that cannot be encoded as a JSON object name"

type json_writer = {
  mutable tokens : string array;
  chunks : string array;
}

let flush_json_writer writer =
  if Array.length writer.tokens > 0 then (
    let chunk = Js.Array.join ~sep:"" writer.tokens in
    ignore (Js.Array.push ~value:chunk writer.chunks);
    writer.tokens <- [||])

let add_json_token writer token =
  ignore (Js.Array.push ~value:token writer.tokens);
  if Array.length writer.tokens >= 8_192 then flush_json_writer writer

let add_json_string writer value =
  add_json_token writer (Js.Json.stringify (Js.Json.string value))

let add_json_char writer value =
  value
  |> Melange_edn.char
  |> Melange_edn.any
  |> Melange_edn.to_edn_string
  |> add_json_string writer

let add_json_float writer value =
  match classify_float value with
  | FP_nan -> add_json_string writer "NaN"
  | FP_infinite when value > 0. -> add_json_string writer "Infinity"
  | FP_infinite -> add_json_string writer "-Infinity"
  | FP_normal | FP_subnormal | FP_zero ->
      add_json_token writer (Js.Json.stringify (Js.Json.number value))

let add_json_int writer value =
  let min_safe_json_integer = -9007199254740991L in
  let max_safe_json_integer = 9007199254740991L in
  if value >= min_safe_json_integer && value <= max_safe_json_integer then
    add_json_token writer (Int64.to_string value)
  else add_json_string writer (Int64.to_string value)

let json_string_token value = Js.Json.stringify (Js.Json.string value)

let json_int_token value =
  let min_safe_json_integer = -9007199254740991L in
  let max_safe_json_integer = 9007199254740991L in
  if value >= min_safe_json_integer && value <= max_safe_json_integer then
    Int64.to_string value
  else json_string_token (Int64.to_string value)

let json_float_token value =
  match classify_float value with
  | FP_nan -> json_string_token "NaN"
  | FP_infinite when value > 0. -> json_string_token "Infinity"
  | FP_infinite -> json_string_token "-Infinity"
  | FP_normal | FP_subnormal | FP_zero ->
      Js.Json.stringify (Js.Json.number value)

let rec compact_json_value = function
  | Nil -> Some "null"
  | Bool true -> Some "true"
  | Bool false -> Some "false"
  | String value | Symbol value | Bigint value | Decimal value | Ratio value
  | Regex value ->
      Some (json_string_token value)
  | Keyword value -> Some (json_string_token (":" ^ value))
  | Small_int value -> Some (string_of_int value)
  | Int value -> Some (json_int_token value)
  | Float value -> Some (json_float_token value)
  | Vector values when Array.length values = 1 ->
      compact_json_value values.(0)
      |> Option.map (fun value -> "[" ^ value ^ "]")
  | Vector values when Array.length values = 2 -> (
      match
        (compact_json_value values.(0), compact_json_value values.(1))
      with
      | Some first, Some second -> Some ("[" ^ first ^ "," ^ second ^ "]")
      | _ -> None)
  | Char _ | List _ | Vector _ | Int4_vector _ | Int_vector _ | Map _
  | Set _ | Tagged _ | Json_source _ ->
      None

let rec add_json_value writer = function
  | Nil -> add_json_token writer "null"
  | Bool true -> add_json_token writer "true"
  | Bool false -> add_json_token writer "false"
  | String value | Symbol value | Bigint value | Decimal value | Ratio value
  | Regex value ->
      add_json_string writer value
  | Char value -> add_json_char writer value
  | Keyword value -> add_json_string writer (":" ^ value)
  | Small_int value -> add_json_token writer (string_of_int value)
  | Int value -> add_json_int writer value
  | Float value -> add_json_float writer value
  | List values | Set values ->
      add_json_token writer "[";
      Array.iteri
        (fun index value ->
          if index > 0 then add_json_token writer ",";
          add_json_value writer value)
        values;
      add_json_token writer "]"
  | Vector values ->
      add_json_token writer "[";
      Array.iteri
        (fun index value ->
          match value with
          | Int4_vector (first, second, third, fourth) -> (
              match compact_json_value third with
              | Some third ->
                  let prefix = if index > 0 then ",[" else "[" in
                  add_json_token writer
                    (prefix ^ string_of_int first ^ ","
                   ^ string_of_int second ^ "," ^ third ^ ","
                   ^ string_of_int fourth ^ "]")
              | None ->
                  if index > 0 then add_json_token writer ",";
                  add_json_value writer value)
          | value ->
              if index > 0 then add_json_token writer ",";
              add_json_value writer value)
        values;
      add_json_token writer "]"
  | Int4_vector (first, second, third, fourth) ->
      (match compact_json_value third with
      | Some third ->
          add_json_token writer
            ("[" ^ string_of_int first ^ "," ^ string_of_int second ^ ","
           ^ third ^ "," ^ string_of_int fourth ^ "]")
      | None ->
          add_json_token writer "[";
          add_json_token writer (string_of_int first);
          add_json_token writer ",";
          add_json_token writer (string_of_int second);
          add_json_token writer ",";
          add_json_value writer third;
          add_json_token writer ",";
          add_json_token writer (string_of_int fourth);
          add_json_token writer "]")
  | Int_vector values ->
      add_json_token writer "[";
      Array.iteri
        (fun index value ->
          if index > 0 then add_json_token writer ",";
          add_json_token writer (string_of_int value))
        values;
      add_json_token writer "]"
  | Map entries ->
      add_json_token writer "{";
      Array.iteri
        (fun index (key, value) ->
          if index > 0 then add_json_token writer ",";
          add_json_string writer (json_key key);
          add_json_token writer ":";
          add_json_value writer value)
        entries;
      add_json_token writer "}"
  | Tagged (tag, value) ->
      add_json_token writer "{\"tag\":";
      add_json_string writer tag;
      add_json_token writer ",\"value\":";
      add_json_value writer value;
      add_json_token writer "}"
  | Json_source source -> add_json_token writer source

let to_json_string value =
  let writer = { tokens = [||]; chunks = [||] } in
  add_json_value writer value;
  flush_json_writer writer;
  Js.Array.join ~sep:"" writer.chunks

let regex_valid pattern =
  let _ = Js.Re.fromString pattern in
  true

let regex_find pattern source =
  Js.Re.fromString pattern |> Js.Re.test ~str:source

type regex_match = { captures : string option array }

let regex_match result =
  {
    captures =
      Js.Re.captures result
      |> Array.map Js.Nullable.toOption;
  }

let regex_find_groups ~pattern source =
  Js.Re.fromString pattern
  |> Js.Re.exec ~str:source
  |> Option.map regex_match

let regex_matches_groups ~pattern source =
  match Js.Re.fromString pattern |> Js.Re.exec ~str:source with
  | None -> None
  | Some result ->
      let captures = Js.Re.captures result in
      let matched =
        Js.Nullable.toOption captures.(0)
        |> Option.value ~default:""
      in
      if Js.Re.index result = 0 && String.length matched = String.length source
      then Some (regex_match result)
      else None

let regex_all_groups ~pattern source =
  let regexp = Js.Re.fromStringWithFlags pattern ~flags:"g" in
  let rec collect matches =
    match Js.Re.exec ~str:source regexp with
    | None -> Array.of_list (List.rev matches)
    | Some result ->
        let value = regex_match result in
        let matched =
          value.captures.(0) |> Option.value ~default:""
        in
        if String.length matched = 0 then
          Js.Re.setLastIndex regexp (Js.Re.lastIndex regexp + 1);
        collect (value :: matches)
  in
  collect []

let regex_replace ~all ~pattern ~replacement source =
  let flags = if all then "g" else "" in
  let regexp = Js.Re.fromStringWithFlags pattern ~flags in
  Js.String.replaceByRe ~regexp ~replacement source

let drop_trailing_empty values =
  if Array.length values = 1 && values.(0) = Some "" then values
  else
    let rec last_nonempty index =
      if index < 0 then -1
      else
        match values.(index) with
        | Some "" -> last_nonempty (index - 1)
        | _ -> index
    in
    Array.sub values 0 (last_nonempty (Array.length values - 1) + 1)

let limited_regex_split pattern limit source =
  let regexp = Js.Re.fromStringWithFlags pattern ~flags:"g" in
  let rec collect cursor remaining values =
    if remaining = 1 then
      Array.of_list
        (List.rev
           (Some
              (String.sub source cursor (String.length source - cursor))
           :: values))
    else
      match Js.Re.exec ~str:source regexp with
      | None ->
          Array.of_list
            (List.rev
               (Some
                  (String.sub source cursor (String.length source - cursor))
               :: values))
      | Some result ->
          let start = Js.Re.index result in
          let matched =
            Js.Re.captures result
            |> fun captures -> Js.Nullable.toOption captures.(0)
            |> Option.value ~default:""
          in
          let stop = start + String.length matched in
          if String.length matched = 0 then
            Js.Re.setLastIndex regexp (Js.Re.lastIndex regexp + 1);
          collect stop (remaining - 1)
            (Some (String.sub source cursor (start - cursor)) :: values)
  in
  collect 0 limit []

let full_regex_split pattern source =
  let regexp = Js.Re.fromStringWithFlags pattern ~flags:"g" in
  let rec collect cursor values =
    match Js.Re.exec ~str:source regexp with
    | None ->
        Array.of_list
          (List.rev
             (Some
                (String.sub source cursor (String.length source - cursor))
             :: values))
    | Some result ->
        let start = Js.Re.index result in
        let captures = Js.Re.captures result in
        let matched =
          Js.Nullable.toOption captures.(0) |> Option.value ~default:""
        in
        let stop = start + String.length matched in
        let rec capture_values index result =
          if index >= Array.length captures then List.rev result
          else
            capture_values (index + 1)
              (Js.Nullable.toOption captures.(index) :: result)
        in
        let segment =
          Some (String.sub source cursor (start - cursor))
          :: capture_values 1 []
        in
        if String.length matched = 0 then
          Js.Re.setLastIndex regexp (Js.Re.lastIndex regexp + 1);
        collect stop (List.rev_append segment values)
  in
  collect 0 []

let regex_split ~pattern ~limit source =
  let values =
    match limit with
    | Some value when value > 0 -> limited_regex_split pattern value source
    | _ -> full_regex_split pattern source
  in
  match limit with
  | Some value when value < 0 -> values
  | None | Some _ -> drop_trailing_empty values
