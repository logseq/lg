type entity_ref =
  | Entity_id of int
  | Temp_id of string
  | Current_tx
  | Ident of string
  | Lookup_ref of string * t

and t =
  | Nil
  | Int of int
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
val map_of_keyword_map : (string, t) Lg_runtime.Runtime_map.t -> t
val map_of_keyword_map_with :
  ('value -> t) -> (string, 'value) Lg_runtime.Runtime_map.t -> t
val keyword_map_get : string -> t -> t option
val keyword_map_value : t -> (string, t) Lg_runtime.Runtime_map.t option
val string_vector : string Rrbvec.t -> t
val temp_id_vector : string Rrbvec.t -> t
val tuple_items : t -> t option Rrbvec.t option
val keyword_value : t -> string option
val bool_value : t -> bool option
val sequential_items : t -> t Rrbvec.t option
val set_items : t -> t Rrbvec.t option
val entity_ref_value : t -> entity_ref option
val ref_value : t -> int option
val tuple_entity_refs : string Rrbvec.t -> string Rrbvec.t -> t -> entity_ref Rrbvec.t
val resolve_tuple_refs :
  string Rrbvec.t -> string Rrbvec.t -> int Rrbvec.t -> t -> t
val keyword_items : t -> string Rrbvec.t option
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
