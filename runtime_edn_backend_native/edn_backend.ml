module Melange_edn = Melange_edn_native

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

type json = Yojson.Safe.t

type 'a string_map = (string, 'a) Hashtbl.t

let string_map_create () = Hashtbl.create 16
let string_map_find map key = Hashtbl.find_opt map key
let string_map_set map key value = Hashtbl.replace map key value

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

let rec of_json = function
  | `Null -> Nil
  | `Bool value -> Bool value
  | `String value -> String value
  | `Int value -> Small_int value
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

let with_json_array4 json f =
  match json with
  | `List [ first; second; third; fourth ] ->
      f first second third fourth
  | _ -> invalid_arg "expected JSON array of length 4"

let json_int = function
  | `Int value -> value
  | `Intlit value -> int_of_string value
  | `Float value when Float.is_integer value -> int_of_float value
  | _ -> invalid_arg "expected JSON integer"

let json_string = function
  | `String value -> value
  | _ -> invalid_arg "expected JSON string"

let json_int_opt = function
  | `Int value -> Some value
  | `Intlit value -> int_of_string_opt value
  | `Float value when Float.is_integer value -> Some (int_of_float value)
  | _ -> None

let json_float_opt = function
  | `Float value when not (Float.is_integer value) -> Some value
  | _ -> None

let json_string_opt = function `String value -> Some value | _ -> None
let json_bool_opt = function `Bool value -> Some value | _ -> None

let json_array_opt = function
  | `List values -> Some (Array.of_list values)
  | _ -> None

let json_is_null = function `Null -> true | _ -> false
let json_to_edn = of_json

let json_datom_of_json json_datom_entity json_datom_attribute value
    json_datom_tx =
  match value with
  | `String json_string_value ->
      Json_string_datom
        {
          json_datom_entity;
          json_datom_attribute;
          json_string_value;
          json_datom_tx;
        }
  | `Int json_int_value ->
      Json_int_datom
        {
          json_datom_entity;
          json_datom_attribute;
          json_int_value;
          json_datom_tx;
        }
  | `Intlit value -> (
      match int_of_string_opt value with
      | Some json_int_value ->
          Json_int_datom
            {
              json_datom_entity;
              json_datom_attribute;
              json_int_value;
              json_datom_tx;
            }
      | None ->
          Invalid_json_value_datom
            { json_datom_entity; json_datom_attribute; json_datom_tx })
  | `Float value when Float.is_integer value ->
      Json_int_datom
        {
          json_datom_entity;
          json_datom_attribute;
          json_int_value = int_of_float value;
          json_datom_tx;
        }
  | `Float json_float_value ->
      Json_float_datom
        {
          json_datom_entity;
          json_datom_attribute;
          json_float_value;
          json_datom_tx;
        }
  | `Bool json_bool_value ->
      Json_bool_datom
        {
          json_datom_entity;
          json_datom_attribute;
          json_bool_value;
          json_datom_tx;
        }
  | `List [ marker; value ] -> (
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
          match value with
          | `String json_edn_value ->
              Json_edn_datom
                {
                  json_datom_entity;
                  json_datom_attribute;
                  json_edn_value;
                  json_datom_tx;
                }
          | _ ->
              Invalid_json_value_datom
                { json_datom_entity; json_datom_attribute; json_datom_tx })
      | _ ->
          Invalid_json_value_datom
            { json_datom_entity; json_datom_attribute; json_datom_tx })
  | `List [ marker ] -> (
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
  | `Null | `List _ | `Assoc _ ->
      Invalid_json_value_datom
        { json_datom_entity; json_datom_attribute; json_datom_tx }

let read_json_space_if_present lexer lexbuf =
  let position = lexbuf.Lexing.lex_curr_pos in
  if position < lexbuf.Lexing.lex_buffer_len then
    match Bytes.get lexbuf.Lexing.lex_buffer position with
    | ' ' | '\t' | '\n' | '\r' | '/' ->
        Yojson.Safe.read_space lexer lexbuf
    | _ -> ()

let json_database_of_string source =
  let lexer = Yojson.Safe.init_lexer () in
  let lexbuf = Lexing.from_string source in
  let count = ref None in
  let tx0 = ref None in
  let max_eid = ref None in
  let max_tx = ref None in
  let schema = ref None in
  let attrs = ref None in
  let keywords = ref None in
  let datoms = ref None in
  let aevt = ref None in
  let avet = ref None in
  let branching_factor = ref None in
  let ref_type = ref None in
  let required name = function
    | Some value -> value
    | None -> invalid_arg ("missing JSON field " ^ name)
  in
  let read_separator lexer lexbuf =
    read_json_space_if_present lexer lexbuf;
    Yojson.Safe.read_array_sep lexer lexbuf;
    read_json_space_if_present lexer lexbuf
  in
  let read_datom lexer lexbuf =
    Yojson.Safe.read_lbr lexer lexbuf;
    read_json_space_if_present lexer lexbuf;
    match
      let json_datom_entity = Yojson.Safe.read_int lexer lexbuf in
      read_separator lexer lexbuf;
      let json_datom_attribute = Yojson.Safe.read_int lexer lexbuf in
      read_separator lexer lexbuf;
      let json_datom_value = Yojson.Safe.read_json lexer lexbuf in
      read_separator lexer lexbuf;
      let json_datom_tx = Yojson.Safe.read_int lexer lexbuf in
      read_json_space_if_present lexer lexbuf;
      Yojson.Safe.read_rbr lexer lexbuf;
      json_datom_of_json json_datom_entity json_datom_attribute
        json_datom_value json_datom_tx
    with
    | value -> value
    | exception Yojson__Common.End_of_array -> Invalid_json_datom
  in
  let initial_capacity () =
    match !count with Some value when value > 0 -> value | _ -> 16
  in
  let read_int_array lexer lexbuf =
    let values = ref (Array.make (initial_capacity ()) 0) in
    let add index lexer lexbuf =
      if index = Array.length !values then (
        let grown = Array.make (max 1 (index * 2)) 0 in
        Array.blit !values 0 grown 0 index;
        values := grown);
      (!values).(index) <- Yojson.Safe.read_int lexer lexbuf;
      index + 1
    in
    let length = Yojson.Safe.read_sequence add 0 lexer lexbuf in
    if length = Array.length !values then !values
    else Array.sub !values 0 length
  in
  let read_datom_array lexer lexbuf =
    let values =
      ref (Array.make (initial_capacity ()) Invalid_json_datom)
    in
    let add index lexer lexbuf =
      if index = Array.length !values then (
        let grown =
          Array.make (max 1 (index * 2)) Invalid_json_datom
        in
        Array.blit !values 0 grown 0 index;
        values := grown);
      (!values).(index) <- read_datom lexer lexbuf;
      index + 1
    in
    let length = Yojson.Safe.read_sequence add 0 lexer lexbuf in
    if length = Array.length !values then !values
    else Array.sub !values 0 length
  in
  let read_optional_int_array lexer lexbuf =
    if Yojson.Safe.read_null_if_possible lexer lexbuf then None
    else Some (read_int_array lexer lexbuf)
  in
  let read_field () name lexer lexbuf =
    match name with
    | "count" -> count := Some (Yojson.Safe.read_int lexer lexbuf)
    | "tx0" -> tx0 := Some (Yojson.Safe.read_int lexer lexbuf)
    | "max-eid" -> max_eid := Some (Yojson.Safe.read_int lexer lexbuf)
    | "max-tx" -> max_tx := Some (Yojson.Safe.read_int lexer lexbuf)
    | "schema" -> schema := Some (Yojson.Safe.read_json lexer lexbuf |> of_json)
    | "attrs" ->
        attrs := Some (Yojson.Safe.read_array Yojson.Safe.read_string lexer lexbuf)
    | "keywords" ->
        keywords :=
          Some (Yojson.Safe.read_array Yojson.Safe.read_string lexer lexbuf)
    | "eavt" ->
        datoms := Some (read_datom_array lexer lexbuf)
    | "aevt" -> aevt := Some (read_optional_int_array lexer lexbuf)
    | "avet" -> avet := Some (read_optional_int_array lexer lexbuf)
    | "branching-factor" ->
        branching_factor := Some (Yojson.Safe.read_int lexer lexbuf)
    | "ref-type" -> ref_type := Some (Yojson.Safe.read_string lexer lexbuf)
    | _ -> Yojson.Safe.skip_json lexer lexbuf
  in
  ignore (Yojson.Safe.read_fields read_field () lexer lexbuf);
  read_json_space_if_present lexer lexbuf;
  if not (Yojson.Safe.read_eof lexbuf) then
    invalid_arg "unexpected data after serialized database";
  {
    json_database_count = required "count" !count;
    json_database_tx0 = required "tx0" !tx0;
    json_database_max_eid = required "max-eid" !max_eid;
    json_database_max_tx = required "max-tx" !max_tx;
    json_database_schema = required "schema" !schema;
    json_database_attrs = required "attrs" !attrs;
    json_database_keywords = required "keywords" !keywords;
    json_database_datoms = required "eavt" !datoms;
    json_database_aevt = required "aevt" !aevt;
    json_database_avet = required "avet" !avet;
    json_database_branching_factor = !branching_factor;
    json_database_ref_type = !ref_type;
  }

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

let rec add_json_positive_int_digits buffer value =
  if value >= 10 then add_json_positive_int_digits buffer (value / 10);
  Buffer.add_char buffer (Char.chr (48 + (value mod 10)))

let add_json_small_int buffer value =
  if value = min_int then Buffer.add_string buffer (string_of_int value)
  else (
    let value =
      if value < 0 then (
        Buffer.add_char buffer '-';
        -value)
      else value
    in
    add_json_positive_int_digits buffer value)

let int4_array_length entities attributes values txs =
  let length = Array.length values in
  if
    Array.length entities <> length
    || Array.length attributes <> length
    || Array.length txs <> length
  then invalid_arg "Int4_array columns must have equal lengths";
  length

let rec add_json_value buffer = function
  | Nil -> Buffer.add_string buffer "null"
  | Bool true -> Buffer.add_string buffer "true"
  | Bool false -> Buffer.add_string buffer "false"
  | String value | Symbol value | Bigint value | Decimal value | Ratio value
  | Regex value ->
      add_json_string buffer value
  | Char value -> add_json_char buffer value
  | Keyword value -> add_json_string buffer (":" ^ value)
  | Small_int value -> add_json_small_int buffer value
  | Int value -> add_json_int buffer value
  | Float value -> add_json_float buffer value
  | List values | Vector values | Set values ->
      add_json_array buffer values
  | Int4_vector (first, second, third, fourth) ->
      Buffer.add_char buffer '[';
      add_json_small_int buffer first;
      Buffer.add_char buffer ',';
      add_json_small_int buffer second;
      Buffer.add_char buffer ',';
      add_json_value buffer third;
      Buffer.add_char buffer ',';
      add_json_small_int buffer fourth;
      Buffer.add_char buffer ']'
  | Int4_array (entities, attributes, values, txs) ->
      let _ = int4_array_length entities attributes values txs in
      Buffer.add_char buffer '[';
      Array.iteri
        (fun index value ->
          if index > 0 then Buffer.add_char buffer ',';
          Buffer.add_char buffer '[';
          add_json_small_int buffer entities.(index);
          Buffer.add_char buffer ',';
          add_json_small_int buffer attributes.(index);
          Buffer.add_char buffer ',';
          add_json_value buffer value;
          Buffer.add_char buffer ',';
          add_json_small_int buffer txs.(index);
          Buffer.add_char buffer ']')
        values;
      Buffer.add_char buffer ']'
  | Int_vector values ->
      Buffer.add_char buffer '[';
      Array.iteri
        (fun index value ->
          if index > 0 then Buffer.add_char buffer ',';
          add_json_small_int buffer value)
        values;
      Buffer.add_char buffer ']'
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

let estimated_json_capacity = function
  | Map entries ->
      Array.fold_left
        (fun capacity (_, value) ->
          match value with
          | Vector values -> capacity + (Array.length values * 32)
          | Int4_array (_, _, values, _) ->
              capacity + (Array.length values * 32)
          | Int_vector values -> capacity + (Array.length values * 8)
          | _ -> capacity)
        256 entries
  | _ -> 256

let to_json_string value =
  let buffer = Buffer.create (estimated_json_capacity value) in
  add_json_value buffer value;
  Buffer.contents buffer

let regex_valid pattern =
  let _ = Re.Perl.compile_pat pattern in
  true

let regex_options flags =
  flags |> String.to_seq
  |> Seq.fold_left
       (fun options -> function
         | 'i' -> `Caseless :: options
         | 'm' -> `Multiline :: options
         | 's' -> `Dotall :: options
         | 'd' | 'u' -> options
         | flag -> invalid_arg (Printf.sprintf "unsupported regex flag %c" flag))
       []

let regex_valid_with_flags ~pattern ~flags =
  let _ = Re.Perl.compile_pat ~opts:(regex_options flags) pattern in
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

let regex_find_groups_with_flags ~pattern ~flags source =
  Re.exec_opt (Re.Perl.compile_pat ~opts:(regex_options flags) pattern) source
  |> Option.map regex_match

let regex_matches_groups ~pattern source =
  Re.exec_opt
    (Re.compile (Re.whole_string (Re.Perl.re pattern)))
    source
  |> Option.map regex_match

let regex_matches_groups_with_flags ~pattern ~flags source =
  Re.exec_opt
    (Re.compile
       (Re.whole_string (Re.Perl.re ~opts:(regex_options flags) pattern)))
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

let regex_split_with_flags ~pattern ~flags ~limit source =
  let regex = Re.Perl.compile_pat ~opts:(regex_options flags) pattern in
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

let regex_split ~pattern ~limit source =
  regex_split_with_flags ~pattern ~flags:"" ~limit source
