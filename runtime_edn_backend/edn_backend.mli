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
  | Seq of t Seq.t
  | Vector of t array
  | Int4_vector of int * int * t * int
  | Int4_array of int array * int array * t array * int array
  | Int_vector of int array
  | Map of (t * t) array
  | Set of t array
  | Tagged of string * t
  | Json_source of string

type json

type 'a string_map

val string_map_create : unit -> 'a string_map
val string_map_find : 'a string_map -> string -> 'a option
val string_map_set : 'a string_map -> string -> 'a -> unit

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

type regex_match = { captures : string option array }

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

val of_edn_string : string -> t
val fold_edn_string : 'a edn_folder -> string -> 'a
val to_edn_string : t -> string
val of_json_string : string -> t
val of_json_source : string -> t
val to_json_string : t -> string
val json_of_string : string -> json
val json_database_of_string : string -> json_database
val json_field : json -> string -> json
val json_field_opt : json -> string -> json option
val json_array : json -> json array
val with_json_array4 : json -> (json -> json -> json -> json -> 'a) -> 'a
val json_int : json -> int
val json_string : json -> string
val json_int_opt : json -> int option
val json_float_opt : json -> float option
val json_string_opt : json -> string option
val json_bool_opt : json -> bool option
val json_array_opt : json -> json array option
val json_is_null : json -> bool
val json_to_edn : json -> t
val regex_valid : string -> bool
val regex_valid_with_flags : pattern:string -> flags:string -> bool
val regex_find : string -> string -> bool
val regex_find_groups : pattern:string -> string -> regex_match option
val regex_find_groups_with_flags :
  pattern:string -> flags:string -> string -> regex_match option
val regex_matches_groups : pattern:string -> string -> regex_match option
val regex_matches_groups_with_flags :
  pattern:string -> flags:string -> string -> regex_match option
val regex_all_groups : pattern:string -> string -> regex_match array
val regex_all_groups_with_flags :
  pattern:string -> flags:string -> string -> regex_match array
val regex_replace :
  all:bool -> pattern:string -> replacement:string -> string -> string
val regex_split :
  pattern:string -> limit:int option -> string -> string option array
val regex_split_with_flags :
  pattern:string -> flags:string -> limit:int option -> string ->
  string option array
