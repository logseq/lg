type value = Lg_edn_backend.t
type reader = value -> value

let tag_parsers : (string * reader) list ref = ref []

let register_tag_parser tag parser =
  let previous = List.assoc_opt tag !tag_parsers in
  tag_parsers := (tag, parser) :: List.remove_assoc tag !tag_parsers;
  previous

let rec apply_tag_parsers value =
  let open Lg_edn_backend in
  match value with
  | List values -> List (Array.map apply_tag_parsers values)
  | Vector values -> Vector (Array.map apply_tag_parsers values)
  | Int4_vector (first, second, third, fourth) ->
      Int4_vector (first, second, apply_tag_parsers third, fourth)
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
      Option.fold
        ~none:(Tagged (tag, value))
        ~some:(fun parser -> parser value)
        (List.assoc_opt tag !tag_parsers)
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
