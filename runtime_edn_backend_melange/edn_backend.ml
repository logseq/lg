module Melange_edn = Melange_edn_melange

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
let of_json_string source = Melange_edn_melange.of_json_string source |> of_edn
let to_json_string value = value |> to_edn |> Melange_edn_melange.to_json_string

let regex_valid pattern =
  let _ = Js.Re.fromString pattern in
  true

let regex_find pattern source =
  Js.Re.fromString pattern |> Js.Re.test ~str:source
