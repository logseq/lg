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
val vector_of_vector_with : ('value -> t) -> 'value Rrbvec.t -> t
val is_nil : t -> bool
val add : t Rrbvec.t -> t option
val subtract : t Rrbvec.t -> t option
val multiply : t Rrbvec.t -> t option
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
val regex_pattern : t -> t option
val regex_find : t -> t -> bool option
val keyword_map_get : string -> t -> t option
val keyword_map_value : t -> (string, t) Lg_runtime.Runtime_map.t option
val keyword_map_entries : t -> (string * t) Rrbvec.t option
val map_entries : t -> (t * t) Rrbvec.t option
val string_vector : string Rrbvec.t -> t
val temp_id_vector : string Rrbvec.t -> t
val tuple_items : t -> t option Rrbvec.t option
val keyword_value : t -> string option
val bool_value : t -> bool option
val sequential_items : t -> t Rrbvec.t option
val set_items : t -> t Rrbvec.t option
val count_value : t -> int option
val entity_ref_value : t -> entity_ref option
val lookup_ref_value : t -> (string * t) option
val ref_value : t -> int option
val tuple_entity_refs : string Rrbvec.t -> string Rrbvec.t -> t -> entity_ref Rrbvec.t
val resolve_tuple_refs :
  string Rrbvec.t -> string Rrbvec.t -> int Rrbvec.t -> t -> t
val keyword_items : t -> string Rrbvec.t option
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val to_edn_string : t -> string
