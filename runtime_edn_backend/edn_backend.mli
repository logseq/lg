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

val of_edn_string : string -> t
val to_edn_string : t -> string
val of_json_string : string -> t
val to_json_string : t -> string
val regex_valid : string -> bool
val regex_find : string -> string -> bool
