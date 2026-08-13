type value = Lg_edn_backend.t
type reader = value -> value
type default_reader = string -> value -> value

let tag_parsers : (string * reader) list ref = ref []
let default_tag_parser : default_reader option ref = ref None

let register_tag_parser tag parser =
  let previous = List.assoc_opt tag !tag_parsers in
  tag_parsers := (tag, parser) :: List.remove_assoc tag !tag_parsers;
  previous

let deregister_tag_parser tag =
  let previous = List.assoc_opt tag !tag_parsers in
  tag_parsers := List.remove_assoc tag !tag_parsers;
  previous

let register_default_tag_parser parser =
  let previous = !default_tag_parser in
  default_tag_parser := Some parser;
  previous

let deregister_default_tag_parser () =
  let previous = !default_tag_parser in
  default_tag_parser := None;
  previous

let rec apply_tag_parsers value =
  let open Lg_edn_backend in
  match value with
  | List values -> List (Array.map apply_tag_parsers values)
  | Vector values -> Vector (Array.map apply_tag_parsers values)
  | Int4_vector (first, second, third, fourth) ->
      Int4_vector (first, second, apply_tag_parsers third, fourth)
  | Int4_array (entities, attributes, values, txs) ->
      Int4_array
        (entities, attributes, Array.map apply_tag_parsers values, txs)
  | Int_vector _ as value -> value
  | Map entries ->
      Map
        (Array.map
           (fun (key, value) ->
             (apply_tag_parsers key, apply_tag_parsers value))
           entries)
  | Set values -> Set (Array.map apply_tag_parsers values)
  | Tagged (tag, value) ->
      let value = apply_tag_parsers value in
      (match List.assoc_opt tag !tag_parsers with
      | Some parser -> parser value
      | None -> (
          match !default_tag_parser with
          | Some parser -> parser tag value
          | None -> Tagged (tag, value)))
  | Json_source source ->
      source |> Lg_edn_backend.of_json_string |> apply_tag_parsers
  | ( Nil | Bool _ | String _ | Char _ | Symbol _ | Keyword _ | Small_int _
    | Int _ | Bigint _ | Float _ | Decimal _ | Ratio _ | Regex _ ) as value ->
      value

let read_string source =
  source |> Lg_edn_backend.of_edn_string |> apply_tag_parsers

let write_string = Lg_edn_backend.to_edn_string
let read_json_string = Lg_edn_backend.of_json_string
let read_json_source = Lg_edn_backend.of_json_source
let write_json_string = Lg_edn_backend.to_json_string

let map_entries = function
  | Lg_edn_backend.Map entries -> Array.to_seq entries
  | _ -> invalid_arg "expected an EDN map"

let rec to_seq value =
  let open Lg_edn_backend in
  match value with
  | Nil -> Seq.empty
  | List values | Vector values | Set values -> Array.to_seq values
  | Int4_vector (first, second, third, fourth) ->
      [|
        Small_int first;
        Small_int second;
        third;
        Small_int fourth;
      |]
      |> Array.to_seq
  | Int4_array (entities, attributes, values, txs) ->
      Array.mapi
        (fun index value ->
          Int4_vector
            (entities.(index), attributes.(index), value, txs.(index)))
        values
      |> Array.to_seq
  | Int_vector values ->
      values |> Array.to_seq |> Seq.map (fun value -> Small_int value)
  | Map entries ->
      entries |> Array.to_seq
      |> Seq.map (fun (key, value) -> Vector [| key; value |])
  | String value ->
      value |> String.to_seq
      |> Seq.map (fun value -> Char (Uchar.of_char value))
  | Json_source source -> source |> of_json_string |> to_seq
  | Bool _ | Char _ | Symbol _ | Keyword _ | Small_int _ | Int _ | Bigint _
  | Float _ | Decimal _ | Ratio _ | Regex _ | Tagged _ ->
      invalid_arg "EDN value is not seqable"

let find_keyword keyword = function
  | Lg_edn_backend.Map entries ->
      let keyword =
        if String.starts_with ~prefix:":" keyword then
          String.sub keyword 1 (String.length keyword - 1)
        else keyword
      in
      entries
      |> Array.find_map (function
           | Lg_edn_backend.Keyword candidate, value
             when String.equal candidate keyword ->
               Some value
           | _ -> None)
  | _ -> invalid_arg "expected an EDN map"

let bool_value = function
  | Lg_edn_backend.Bool value -> value
  | _ -> invalid_arg "expected an EDN boolean"

let is_nil = function Lg_edn_backend.Nil -> true | _ -> false

let sequence_values = function
  | Lg_edn_backend.List values | Lg_edn_backend.Vector values -> Some values
  | Lg_edn_backend.Int4_vector (first, second, third, fourth) ->
      Some
        [|
          Lg_edn_backend.Small_int first;
          Lg_edn_backend.Small_int second;
          third;
          Lg_edn_backend.Small_int fourth;
        |]
  | Lg_edn_backend.Int_vector values ->
      Some (Array.map (fun value -> Lg_edn_backend.Small_int value) values)
  | Lg_edn_backend.Int4_array (entities, attributes, values, txs) ->
      let length = Array.length entities in
      if
        Array.length attributes <> length
        || Array.length values <> length
        || Array.length txs <> length
      then invalid_arg "EDN compact row columns must have equal lengths"
      else
        Some
          (Array.init length (fun index ->
               Lg_edn_backend.Int4_vector
                 ( entities.(index),
                   attributes.(index),
                   values.(index),
                   txs.(index) )))
  | _ -> None

let rec equal left right =
  let open Lg_edn_backend in
  match (left, right) with
  | Json_source source, right -> equal (of_json_string source) right
  | left, Json_source source -> equal left (of_json_string source)
  | Nil, Nil -> true
  | Bool left, Bool right -> Bool.equal left right
  | String left, String right
  | Symbol left, Symbol right
  | Keyword left, Keyword right
  | Bigint left, Bigint right
  | Decimal left, Decimal right
  | Ratio left, Ratio right
  | Regex left, Regex right ->
      String.equal left right
  | Char left, Char right -> Uchar.equal left right
  | Small_int left, Small_int right -> Int.equal left right
  | Int left, Int right -> Int64.equal left right
  | Small_int left, Int right | Int right, Small_int left ->
      Int64.equal (Int64.of_int left) right
  | Float left, Float right -> left = right
  | Tagged (left_tag, left_value), Tagged (right_tag, right_value) ->
      String.equal left_tag right_tag && equal left_value right_value
  | Set left, Set right ->
      Array.length left = Array.length right
      && Array.for_all
           (fun value -> Array.exists (equal value) right)
           left
  | Map left, Map right ->
      Array.length left = Array.length right
      && Array.for_all
           (fun (key, value) ->
             Array.exists
               (fun (other_key, other_value) ->
                 equal key other_key && equal value other_value)
               right)
           left
  | _ -> (
      match (sequence_values left, sequence_values right) with
      | Some left, Some right ->
          Array.length left = Array.length right
          && Array.for_all2 equal left right
      | _ -> false)

let rec contains collection key =
  let open Lg_edn_backend in
  match collection with
  | Nil -> false
  | Set values -> Array.exists (equal key) values
  | Map entries -> Array.exists (fun (entry_key, _) -> equal key entry_key) entries
  | List values | Vector values -> (
      match key with
      | Small_int index -> index >= 0 && index < Array.length values
      | Int index ->
          index >= 0L && index < Int64.of_int (Array.length values)
      | _ -> false)
  | Int4_vector _ | Int_vector _ | Int4_array _ -> (
      match sequence_values collection with
      | Some values -> (
          match key with
          | Small_int index -> index >= 0 && index < Array.length values
          | Int index ->
              index >= 0L && index < Int64.of_int (Array.length values)
          | _ -> false)
      | None -> false)
  | Json_source source -> contains (of_json_string source) key
  | Bool _ | String _ | Char _ | Symbol _ | Keyword _ | Small_int _ | Int _
  | Bigint _ | Float _ | Decimal _ | Ratio _ | Regex _ | Tagged _ ->
      false
