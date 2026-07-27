type entity_ref =
  | Entity_id of int
  | Temp_id of string
  | Auto_tempid of int
  | Current_tx
  | Ident of string
  | Lookup_ref of string * t

and t =
  | Nil
  | Int of int
  | Wide_int of int64
  | Float of float
  | String of string
  | Symbol of string
  | Bool of bool
  | Keyword of string
  | Uuid of string
  | Instant of int
  | Regex of string
  | Ref of int
  | List of t list
  | Vector of t list
  | Map of (t * t) list
  | Set of t list
  | Tuple of t option list
  | Tx_ref
  | Ref_to of entity_ref

val tuple_of_vector : t option Rrbvec.t -> t
val set_of_vector : t Rrbvec.t -> t
val vector_of_vector : t Rrbvec.t -> t
val list_of_vector : t Rrbvec.t -> t
val vector_of_vector_with : ('value -> t) -> 'value Rrbvec.t -> t
val is_nil : t -> bool
val add : t Rrbvec.t -> t option
val subtract : t Rrbvec.t -> t option
val multiply : t Rrbvec.t -> t option
val divide : t Rrbvec.t -> t option
val quotient : t Rrbvec.t -> t option
val remainder : t Rrbvec.t -> t option
val modulo : t Rrbvec.t -> t option
val maximum : t Rrbvec.t -> t option
val minimum : t Rrbvec.t -> t option
val range_value : t Rrbvec.t -> t option
val random_value : t Rrbvec.t -> t option
val random_int_value : t Rrbvec.t -> t option
val string_value : t Rrbvec.t -> t option
val pr_str : t Rrbvec.t -> t option
val print_str : t Rrbvec.t -> t option
val println_str : t Rrbvec.t -> t option
val prn_str : t Rrbvec.t -> t option
val substring : t Rrbvec.t -> t option
val increment : t -> t option
val decrement : t -> t option
val map_of_keyword_map : (string, t) Lg_runtime.Runtime_map.t -> t
val map_of_keyword_entries : (string * t) Rrbvec.t -> t
val map_of_keyword_map_with :
  ('value -> t) -> (string, 'value) Lg_runtime.Runtime_map.t -> t
val map_of_data_map : (t, t) Lg_runtime.Runtime_map.t -> t
val map_of_data_map_with :
  ('value -> t) -> (t, 'value) Lg_runtime.Runtime_map.t -> t
val map_get : t -> t -> t option
val get_or_default : t -> t -> t -> t option
val regex_pattern : t -> t option
val regex_find_value : t Rrbvec.t -> t option
val regex_matches_value : t Rrbvec.t -> t option
val regex_sequence_value : t Rrbvec.t -> t option
val string_blank : t -> bool
val string_includes : t -> t option -> bool option
val string_starts_with : t -> t option -> bool option
val string_ends_with : t -> t option -> bool option
val string_lower_case : t Rrbvec.t -> t option
val string_upper_case : t Rrbvec.t -> t option
val string_capitalize : t Rrbvec.t -> t option
val string_join : t Rrbvec.t -> t option
val string_index_of : t Rrbvec.t -> t option
val string_last_index_of : t Rrbvec.t -> t option
val string_reverse : t Rrbvec.t -> t option
val string_split_lines : t Rrbvec.t -> t option
val string_trim : t Rrbvec.t -> t option
val string_trim_newline : t Rrbvec.t -> t option
val string_trim_left : t Rrbvec.t -> t option
val string_trim_right : t Rrbvec.t -> t option
val string_replace : bool -> t Rrbvec.t -> t option
val string_split : t Rrbvec.t -> t option
val keyword_map_get : string -> t -> t option
val keyword_map_value : t -> (string, t) Lg_runtime.Runtime_map.t option
val keyword_map_entries : t -> (string * t) Rrbvec.t option
val map_entries : t -> (t * t) Rrbvec.t option
val string_vector : string Rrbvec.t -> t
val temp_id_vector : string Rrbvec.t -> t
val tuple_items : t -> t option Rrbvec.t option
val keyword_value : t -> string option
val keyword_from_values : t Rrbvec.t -> t option
val name_value : t -> t option
val namespace_value : t -> t option
val bool_value : t -> bool option
val sequential_items : t -> t Rrbvec.t option
val set_items : t -> t Rrbvec.t option
val count_value : t -> int option
val contains_key : t -> t -> bool option
val set_value : t -> t option
val entity_ref_value : t -> entity_ref option
val lookup_ref_value : t -> (string * t) option
val ref_value : t -> int option
val tuple_entity_refs : string Rrbvec.t -> string Rrbvec.t -> t -> entity_ref Rrbvec.t
val resolve_tuple_refs :
  string Rrbvec.t -> string Rrbvec.t -> int Rrbvec.t -> t -> t
val keyword_items : t -> string Rrbvec.t option
val equal : t -> t -> bool
val compare : t -> t -> int
val compare_query_values : t -> t -> int option
val hash : t -> int
val to_edn_string : t -> string
