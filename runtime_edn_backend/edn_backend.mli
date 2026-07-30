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

type json

type regex_match = { captures : string option array }

val of_edn_string : string -> t
val to_edn_string : t -> string
val of_json_string : string -> t
val of_json_source : string -> t
val to_json_string : t -> string
val json_of_string : string -> json
val json_field : json -> string -> json
val json_field_opt : json -> string -> json option
val json_array : json -> json array
val with_json_array4 : json -> (json -> json -> json -> json -> 'a) -> 'a
val json_int : json -> int
val json_string : json -> string
val json_is_null : json -> bool
val json_to_edn : json -> t
val regex_valid : string -> bool
val regex_find : string -> string -> bool
val regex_find_groups : pattern:string -> string -> regex_match option
val regex_matches_groups : pattern:string -> string -> regex_match option
val regex_all_groups : pattern:string -> string -> regex_match array
val regex_replace :
  all:bool -> pattern:string -> replacement:string -> string -> string
val regex_split :
  pattern:string -> limit:int option -> string -> string option array
