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
  | ( Nil | Bool _ | String _ | Char _ | Symbol _ | Keyword _ | Int _
    | Bigint _ | Float _ | Decimal _ | Ratio _ | Regex _ ) as value ->
      value

let read_string source =
  source |> Lg_edn_backend.of_edn_string |> apply_tag_parsers

let write_string = Lg_edn_backend.to_edn_string
