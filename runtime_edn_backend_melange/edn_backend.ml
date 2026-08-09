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
  | Int4_array of int array * int array * t array * int array
  | Int_vector of int array
  | Map of (t * t) array
  | Set of t array
  | Tagged of string * t
  | Json_source of string

type json = Js.Json.t

type 'a string_map = (string, 'a) Js.Map.t

let string_map_create () = Js.Map.make ()
let string_map_find map key = Js.Map.get ~key map
let string_map_set map key value = ignore (Js.Map.set ~key ~value map)

type json_value =
  | Json_string_value of string
  | Json_int_value of int
  | Json_float_value of float
  | Json_bool_value of bool
  | Json_keyword_value of int
  | Json_edn_value of string
  | Json_positive_infinity
  | Json_negative_infinity
  | Json_nan
  | Invalid_json_value

type json_datom =
  | Json_string_datom of {
      json_datom_entity : int;
      json_datom_attribute : int;
      json_string_value : string;
      json_datom_tx : int;
    }
  | Json_int_datom of {
      json_datom_entity : int;
      json_datom_attribute : int;
      json_int_value : int;
      json_datom_tx : int;
    }
  | Json_float_datom of {
      json_datom_entity : int;
      json_datom_attribute : int;
      json_float_value : float;
      json_datom_tx : int;
    }
  | Json_bool_datom of {
      json_datom_entity : int;
      json_datom_attribute : int;
      json_bool_value : bool;
      json_datom_tx : int;
    }
  | Json_keyword_datom of {
      json_datom_entity : int;
      json_datom_attribute : int;
      json_keyword_value : int;
      json_datom_tx : int;
    }
  | Json_edn_datom of {
      json_datom_entity : int;
      json_datom_attribute : int;
      json_edn_value : string;
      json_datom_tx : int;
    }
  | Json_positive_infinity_datom of {
      json_datom_entity : int;
      json_datom_attribute : int;
      json_datom_tx : int;
    }
  | Json_negative_infinity_datom of {
      json_datom_entity : int;
      json_datom_attribute : int;
      json_datom_tx : int;
    }
  | Json_nan_datom of {
      json_datom_entity : int;
      json_datom_attribute : int;
      json_datom_tx : int;
    }
  | Invalid_json_value_datom of {
      json_datom_entity : int;
      json_datom_attribute : int;
      json_datom_tx : int;
    }
  | Invalid_json_datom

type json_database = {
  json_database_count : int;
  json_database_tx0 : int;
  json_database_max_eid : int;
  json_database_max_tx : int;
  json_database_schema : t;
  json_database_attrs : string array;
  json_database_keywords : string array;
  json_database_datoms : json_datom array;
  json_database_aevt : int array option;
  json_database_avet : int array option;
  json_database_branching_factor : int option;
  json_database_ref_type : string option;
}

type 'a edn_folder = {
  edn_nil : 'a;
  edn_bool : bool -> 'a;
  edn_string : string -> 'a;
  edn_char : Uchar.t -> 'a;
  edn_symbol : string -> 'a;
  edn_keyword : string -> 'a;
  edn_int : int64 -> 'a;
  edn_bigint : string -> 'a;
  edn_float : float -> 'a;
  edn_decimal : string -> 'a;
  edn_ratio : string -> 'a;
  edn_regex : string -> 'a;
  edn_list : 'a array -> 'a;
  edn_vector : 'a array -> 'a;
  edn_map : ('a * 'a) array -> 'a;
  edn_set : 'a array -> 'a;
  edn_tagged : string -> 'a -> 'a;
}

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
  | Int4_array (entities, attributes, values, txs) ->
      Melange_edn.any
        (Melange_edn.vector
           (Array.to_list
              (Array.mapi
                 (fun index value ->
                   Melange_edn.any
                     (Melange_edn.vector
                        [
                          Melange_edn.any
                            (Melange_edn.int (Int64.of_int entities.(index)));
                          Melange_edn.any
                            (Melange_edn.int (Int64.of_int attributes.(index)));
                          to_edn value;
                          Melange_edn.any
                            (Melange_edn.int (Int64.of_int txs.(index)));
                        ]))
                 values)))
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

let fold_edn_string folder source =
  let rec fold (Melange_edn.Any value) =
    match value with
    | Melange_edn.Nil -> folder.edn_nil
    | Melange_edn.Bool value -> folder.edn_bool value
    | Melange_edn.String value -> folder.edn_string value
    | Melange_edn.Char value -> folder.edn_char value
    | Melange_edn.Symbol value -> folder.edn_symbol value
    | Melange_edn.Keyword value ->
        folder.edn_keyword (Melange_edn.keyword_to_string value)
    | Melange_edn.Int value -> folder.edn_int value
    | Melange_edn.Bigint value -> folder.edn_bigint value
    | Melange_edn.Float value -> folder.edn_float value
    | Melange_edn.Decimal value -> folder.edn_decimal value
    | Melange_edn.Ratio value -> folder.edn_ratio value
    | Melange_edn.Regex value -> folder.edn_regex value
    | Melange_edn.List values -> folder.edn_list (Array.map fold values)
    | Melange_edn.Vector values -> folder.edn_vector (Array.map fold values)
    | Melange_edn.Map entries ->
        folder.edn_map
          (Array.map (fun (key, value) -> (fold key, fold value)) entries)
    | Melange_edn.Set values -> folder.edn_set (Array.map fold values)
    | Melange_edn.Tagged (tag, value) -> folder.edn_tagged tag (fold value)
  in
  source |> Melange_edn.of_edn_string |> fold

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

let json_datom_of_json json_datom_entity json_datom_attribute value
    json_datom_tx =
  match Js.Json.classify value with
  | JSONString json_string_value ->
      Json_string_datom
        {
          json_datom_entity;
          json_datom_attribute;
          json_string_value;
          json_datom_tx;
        }
  | JSONNumber value when Float.is_integer value ->
      Json_int_datom
        {
          json_datom_entity;
          json_datom_attribute;
          json_int_value = int_of_float value;
          json_datom_tx;
        }
  | JSONNumber json_float_value ->
      Json_float_datom
        {
          json_datom_entity;
          json_datom_attribute;
          json_float_value;
          json_datom_tx;
        }
  | JSONFalse ->
      Json_bool_datom
        {
          json_datom_entity;
          json_datom_attribute;
          json_bool_value = false;
          json_datom_tx;
        }
  | JSONTrue ->
      Json_bool_datom
        {
          json_datom_entity;
          json_datom_attribute;
          json_bool_value = true;
          json_datom_tx;
        }
  | JSONArray [| marker; value |] -> (
      match json_int_opt marker with
      | Some 0 -> (
          match json_int_opt value with
          | Some json_keyword_value ->
              Json_keyword_datom
                {
                  json_datom_entity;
                  json_datom_attribute;
                  json_keyword_value;
                  json_datom_tx;
                }
          | None ->
              Invalid_json_value_datom
                { json_datom_entity; json_datom_attribute; json_datom_tx })
      | Some 1 -> (
          match Js.Json.decodeString value with
          | Some json_edn_value ->
              Json_edn_datom
                {
                  json_datom_entity;
                  json_datom_attribute;
                  json_edn_value;
                  json_datom_tx;
                }
          | None ->
              Invalid_json_value_datom
                { json_datom_entity; json_datom_attribute; json_datom_tx })
      | _ ->
          Invalid_json_value_datom
            { json_datom_entity; json_datom_attribute; json_datom_tx })
  | JSONArray [| marker |] -> (
      match json_int_opt marker with
      | Some 2 ->
          Json_positive_infinity_datom
            { json_datom_entity; json_datom_attribute; json_datom_tx }
      | Some 3 ->
          Json_negative_infinity_datom
            { json_datom_entity; json_datom_attribute; json_datom_tx }
      | Some 4 ->
          Json_nan_datom
            { json_datom_entity; json_datom_attribute; json_datom_tx }
      | _ ->
          Invalid_json_value_datom
            { json_datom_entity; json_datom_attribute; json_datom_tx })
  | JSONNull | JSONArray _ | JSONObject _ ->
      Invalid_json_value_datom
        { json_datom_entity; json_datom_attribute; json_datom_tx }

let json_datom_of_json_fast json_datom_entity json_datom_attribute value
    json_datom_tx =
  match Js.typeof value with
  | "string" -> (
      match Js.Json.decodeString value with
      | Some json_string_value ->
          Json_string_datom
            {
              json_datom_entity;
              json_datom_attribute;
              json_string_value;
              json_datom_tx;
            }
      | None -> assert false)
  | "number" -> (
      match Js.Json.decodeNumber value with
      | Some value when Float.is_integer value ->
          Json_int_datom
            {
              json_datom_entity;
              json_datom_attribute;
              json_int_value = int_of_float value;
              json_datom_tx;
            }
      | Some json_float_value ->
          Json_float_datom
            {
              json_datom_entity;
              json_datom_attribute;
              json_float_value;
              json_datom_tx;
            }
      | None -> assert false)
  | _ ->
      json_datom_of_json json_datom_entity json_datom_attribute value
        json_datom_tx

let json_database_of_string source =
  let json = json_of_string source in
  let field name = json_field json name in
  let string_array name = field name |> json_array |> Array.map json_string in
  let optional_int_array name =
    let value = field name in
    if json_is_null value then None
    else
      let source = json_array value in
      let result = Array.make (Array.length source) 0 in
      for index = 0 to Array.length source - 1 do
        Js.Array.unsafe_set result index
          (json_int (Js.Array.unsafe_get source index));
        Js.Array.unsafe_set source index Js.Json.null
      done;
      Some result
  in
  let datom value =
    match json_array_opt value with
    | Some [| entity; attribute; value; tx |] ->
        json_datom_of_json_fast (json_int entity) (json_int attribute) value
          (json_int tx)
    | _ -> Invalid_json_datom
  in
  let datom_array value =
    let source = json_array value in
    let result = Array.make (Array.length source) Invalid_json_datom in
    for index = 0 to Array.length source - 1 do
      Js.Array.unsafe_set result index
        (datom (Js.Array.unsafe_get source index));
      Js.Array.unsafe_set source index Js.Json.null
    done;
    result
  in
  {
    json_database_count = field "count" |> json_int;
    json_database_tx0 = field "tx0" |> json_int;
    json_database_max_eid = field "max-eid" |> json_int;
    json_database_max_tx = field "max-tx" |> json_int;
    json_database_schema = field "schema" |> json_to_edn;
    json_database_attrs = string_array "attrs";
    json_database_keywords = string_array "keywords";
    json_database_datoms = field "eavt" |> datom_array;
    json_database_aevt = optional_int_array "aevt";
    json_database_avet = optional_int_array "avet";
    json_database_branching_factor =
      json_field_opt json "branching-factor" |> Option.map json_int;
    json_database_ref_type =
      json_field_opt json "ref-type" |> Option.map json_string;
  }

let json_key = function
  | String value | Symbol value | Keyword value -> value
  | _ ->
      invalid_arg
        "EDN map contains a key that cannot be encoded as a JSON object name"

type json_writer = {
  mutable tokens : string array;
  chunks : string array;
  string_tokens : (string, string) Js.Map.t;
}

let flush_json_writer writer =
  if Array.length writer.tokens > 0 then (
    let chunk = Js.Array.join ~sep:"" writer.tokens in
    ignore (Js.Array.push ~value:chunk writer.chunks);
    writer.tokens <- [||])

let add_json_token writer token =
  ignore (Js.Array.push ~value:token writer.tokens);
  if Array.length writer.tokens >= 4_096 then flush_json_writer writer

let json_escape_pattern = Js.Re.fromString "[\"\\\\\\x00-\\x1f]"

let json_string_token value =
  if Js.Re.test ~str:value json_escape_pattern then
    Js.Json.stringify (Js.Json.string value)
  else "\"" ^ value ^ "\""

let writer_json_string_token writer value =
  match Js.Map.get ~key:value writer.string_tokens with
  | Some token -> token
  | None ->
      let token = json_string_token value in
      ignore (Js.Map.set ~key:value ~value:token writer.string_tokens);
      token

let add_json_string writer value =
  add_json_token writer (writer_json_string_token writer value)

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

let common_small_int_tokens = Array.init 128 string_of_int
let common_int4_attribute_tokens =
  Array.init 128 (fun value -> "," ^ string_of_int value ^ ",")

let common_int4_tx_tokens =
  Array.init 128 (fun value -> "," ^ string_of_int value ^ "]")

let small_int_token value =
  if value >= 0 && value < Array.length common_small_int_tokens then
    Array.unsafe_get common_small_int_tokens value
  else string_of_int value

let int4_attribute_token value =
  if value >= 0 && value < Array.length common_int4_attribute_tokens then
    Array.unsafe_get common_int4_attribute_tokens value
  else "," ^ string_of_int value ^ ","

let int4_tx_token value =
  if value >= 0 && value < Array.length common_int4_tx_tokens then
    Array.unsafe_get common_int4_tx_tokens value
  else "," ^ string_of_int value ^ "]"

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

let rec compact_json_value writer = function
  | Nil -> Some "null"
  | Bool true -> Some "true"
  | Bool false -> Some "false"
  | String value | Symbol value | Bigint value | Decimal value | Ratio value
  | Regex value ->
      Some (writer_json_string_token writer value)
  | Keyword value ->
      Some (writer_json_string_token writer (":" ^ value))
  | Small_int value -> Some (small_int_token value)
  | Int value -> Some (json_int_token value)
  | Float value -> Some (json_float_token value)
  | Vector values when Array.length values = 1 ->
      compact_json_value writer values.(0)
      |> Option.map (fun value -> "[" ^ value ^ "]")
  | Vector values when Array.length values = 2 -> (
      match
        ( compact_json_value writer values.(0),
          compact_json_value writer values.(1) )
      with
      | Some first, Some second -> Some ("[" ^ first ^ "," ^ second ^ "]")
      | _ -> None)
  | Int_vector values when Array.length values = 2 ->
      Some ("[" ^ Js.Array.join ~sep:"," values ^ "]")
  | Char _ | List _ | Vector _ | Int4_vector _ | Int4_array _ | Int_vector _
  | Map _ | Set _ | Tagged _ | Json_source _ ->
      None

let int4_array_length entities attributes values txs =
  let length = Array.length values in
  if
    Array.length entities <> length
    || Array.length attributes <> length
    || Array.length txs <> length
  then invalid_arg "Int4_array columns must have equal lengths";
  length

let rec add_json_value writer = function
  | Nil -> add_json_token writer "null"
  | Bool true -> add_json_token writer "true"
  | Bool false -> add_json_token writer "false"
  | String value | Symbol value | Bigint value | Decimal value | Ratio value
  | Regex value ->
      add_json_string writer value
  | Char value -> add_json_char writer value
  | Keyword value -> add_json_string writer (":" ^ value)
  | Small_int value -> add_json_token writer (small_int_token value)
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
      for index = 0 to Array.length values - 1 do
        let value = values.(index) in
        match value with
        | Int4_vector (first, second, third, fourth) -> (
            match compact_json_value writer third with
            | Some third ->
              let prefix = if index > 0 then ",[" else "[" in
              add_json_token writer
                (prefix ^ small_int_token first ^ "," ^ small_int_token second
               ^ "," ^ third ^ "," ^ small_int_token fourth ^ "]")
            | None ->
              if index > 0 then add_json_token writer ",";
              add_json_value writer value)
        | value ->
            if index > 0 then add_json_token writer ",";
            add_json_value writer value
      done;
      add_json_token writer "]"
  | Int4_vector (first, second, third, fourth) ->
      (match compact_json_value writer third with
      | Some third ->
          add_json_token writer
            ("[" ^ small_int_token first ^ "," ^ small_int_token second ^ ","
           ^ third ^ "," ^ small_int_token fourth ^ "]")
      | None ->
          add_json_token writer "[";
          add_json_token writer (small_int_token first);
          add_json_token writer ",";
          add_json_token writer (small_int_token second);
          add_json_token writer ",";
          add_json_value writer third;
          add_json_token writer ",";
          add_json_token writer (small_int_token fourth);
          add_json_token writer "]")
  | Int4_array (entities, attributes, values, txs) ->
      add_json_int4_array writer entities attributes values txs
  | Int_vector values -> add_json_int_vector writer values
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

and add_json_int4_array writer entities attributes values txs =
  add_json_token writer "[";
  let length = int4_array_length entities attributes values txs in
  let row_tokens = Array.make 4_096 "" in
  let row_token_count = ref 0 in
  let flush_rows () =
    let count = !row_token_count in
    if count > 0 then (
      let rows =
        if count = Array.length row_tokens then row_tokens
        else Js.Array.slice ~start:0 ~end_:count row_tokens
      in
      add_json_token writer (Js.Array.join ~sep:"" rows);
      row_token_count := 0)
  in
  let previous_entity = ref 0 in
  let previous_entity_prefix = ref "" in
  let has_previous_entity = ref false in
  for index = 0 to length - 1 do
    let entity = Array.unsafe_get entities index in
    let entity_prefix =
      if !has_previous_entity && entity = !previous_entity then
        !previous_entity_prefix
      else
        let entity_token = small_int_token entity in
        let continued_prefix = ",[" ^ entity_token in
        let entity_prefix =
          if index > 0 then continued_prefix else "[" ^ entity_token
        in
        has_previous_entity := true;
        previous_entity := entity;
        previous_entity_prefix := continued_prefix;
        entity_prefix
    in
    let value = Array.unsafe_get values index in
    match compact_json_value writer value with
    | Some value ->
        Array.unsafe_set row_tokens !row_token_count
          (entity_prefix
         ^ int4_attribute_token (Array.unsafe_get attributes index)
         ^ value ^ int4_tx_token (Array.unsafe_get txs index));
        row_token_count := !row_token_count + 1;
        if !row_token_count = Array.length row_tokens then flush_rows ()
    | None ->
        flush_rows ();
        if index > 0 then add_json_token writer ",";
        add_json_value writer
          (Int4_vector
             ( entity,
               Array.unsafe_get attributes index,
               value,
               Array.unsafe_get txs index ))
  done;
  flush_rows ();
  add_json_token writer "]"

and add_json_int_vector writer values =
  add_json_token writer "[";
  let chunk_size = 8_192 in
  let chunk_count =
    (Array.length values + chunk_size - 1) / chunk_size
  in
  for chunk_index = 0 to chunk_count - 1 do
    let start = chunk_index * chunk_size in
    let end_ = min (start + chunk_size) (Array.length values) in
    let prefix = if chunk_index > 0 then "," else "" in
    let chunk = Js.Array.slice ~start ~end_ values in
    add_json_token writer (prefix ^ Js.Array.join ~sep:"," chunk)
  done;
  add_json_token writer "]"

let to_json_string value =
  let writer =
    { tokens = [||]; chunks = [||]; string_tokens = Js.Map.make () }
  in
  add_json_value writer value;
  flush_json_writer writer;
  Js.Array.join ~sep:"" writer.chunks

let regex_valid pattern =
  let _ = Js.Re.fromString pattern in
  true

let regex_valid_with_flags ~pattern ~flags =
  let _ = Js.Re.fromStringWithFlags pattern ~flags in
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

let regex_find_groups_with_flags ~pattern ~flags source =
  Js.Re.fromStringWithFlags pattern ~flags
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

let regex_matches_groups_with_flags ~pattern ~flags source =
  match Js.Re.fromStringWithFlags pattern ~flags |> Js.Re.exec ~str:source with
  | None -> None
  | Some result ->
      let captures = Js.Re.captures result in
      let matched =
        Js.Nullable.toOption captures.(0) |> Option.value ~default:""
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
