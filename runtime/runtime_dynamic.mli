(** Runtime representation and operations for dynamically typed values. *)

type _ nominal_tag = ..
type nominal = Nominal : 'a nominal_tag * 'a * Obj.t option -> nominal
type _ nominal_tag += Uuid_tag : Runtime_uuid.t nominal_tag
val dynamic_marker : unit ref
type 'a hash_trie =
  | Hash_empty
  | Hash_leaf of int * 'a list
  | Hash_branch of int * 'a hash_trie array
type t = {
  marker : unit ref;
  payload : payload;
  sequence : (unit -> t Seq.t) option;
  sequential : bool;
  metadata : t option;
  type_name : string option;
  nominal : nominal option;
  cached_hash : int option;
}
and payload =
    Nil
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Regex of string
  | Symbol of string
  | Keyword of string
  | Bool of bool
  | Array of t array
  | List
  | Vector of t Rrbvec.t
  | Seq
  | Set of set_payload
  | Map of map_payload
  | Opaque of string
and set_payload = { values : t list; set_index : t hash_trie; }
and map_payload = {
  entries : (t * t) Rrbvec.t;
  map_index : (t * int) hash_trie;
  size : int;
}
val make :
  ?sequence:(unit -> t Seq.t) ->
  ?sequential:bool ->
  ?metadata:t -> ?type_name:string -> ?cached_hash:int -> payload -> t
val with_metadata : t -> t -> t
val with_nominal : 'a nominal_tag -> 'a -> t -> t
val with_sequence : t -> (unit -> t Seq.t) -> t
val nominal : t -> nominal option
val unpack_nominal : 'a nominal_tag -> t -> 'a option
val nominal_metadata_is_original : t -> bool
val nil : t
val int : int -> t
val float : float -> t
val char : char -> t
val string : string -> t
val uuid : Runtime_uuid.t -> t
val as_uuid : t -> Runtime_uuid.t
val symbol : string -> t
val keyword : string -> t
val bool : bool -> t
val regex : string -> t
val unit : unit -> t
val as_unit : t -> unit
val opaque : string -> t
val metadata : t -> t
val str_float : float -> string
val pr_str_float : float -> string
val to_string : pr:bool -> t -> string
val to_string_payload : pr:bool -> t -> string
val to_seq : t -> t Seq.t
val first_value : t -> t
val ffirst_value : t -> t
val str : t -> string
val pr_str : t -> string
val list : t list -> t
val vector : t Rrbvec.t -> t
val vec_value : t -> t
val array : t array -> t
val array_copy : t -> t
val regex_match : string option list option -> t
val regex_group : string option -> t
val regex_match_sequence : string option list array -> t
val seq : t Seq.t -> t
val seq_cons : t -> t Seq.t -> t
val map : (t * t) list -> t
val map_entries : map_payload -> (t * t) list
val nominal_identity_equal : t -> t -> bool
val same_nominal_type : t -> t -> bool
val expand_record_extension_entries : (t * t) list -> (t * t) list
val equal : t -> t -> bool
val equal_payload : t -> t -> bool
val is_runtime_dynamic : 'a -> bool
val polymorphic_equal : 'a -> 'a -> bool
val polymorphic_hash : 'a -> int
val polymorphic_str : 'a -> string
val polymorphic_pr_str : 'a -> string
val equal_arguments : t list -> bool
val numeric_equal : t -> t -> bool
val numeric_equal_arguments : t list -> bool
val numeric_less : t -> t -> bool
val numeric_less_equal : t -> t -> bool
val numeric_greater : t -> t -> bool
val numeric_greater_equal : t -> t -> bool
val numeric_add_arguments : t list -> t
val numeric_subtract_arguments : t list -> t
val numeric_multiply_arguments : t list -> t
val numeric_divide_arguments : t list -> t
val payload_rank : payload -> int
val compare_sequences : t Seq.t -> t Seq.t -> int
val compare_entries : t * t -> t * t -> int
val compare_entry_lists : (t * t) list -> (t * t) list -> int
val compare_identifier : String.t -> String.t -> int
val compare : t -> t -> int
val sort : t -> t
val is_comparable : t -> bool
val set : t Seq.t -> t
val dissoc : t -> t -> t
val select_keys : t -> t Seq.t -> t
val vals : t -> t
val keys : t -> t
val array_get : t -> int -> t
val array_set : t -> int -> t -> unit
val array_unsafe_set : t -> int -> t -> unit
val vector_nth_opt : t -> int -> t option
val subvec_value : t -> int -> int -> t
val payload_get : t -> t -> t option
val get : t -> t -> t
val indexed_get : t -> t -> t
val get_default : t -> t -> t -> t
val contains : t -> t -> bool
val find : t -> t -> t option
val get_in : t -> t Seq.t -> t
val get_in_default : t -> t Seq.t -> t -> t
val entries : t -> (t * t) list
val map_without_keys : t -> t list -> t
val empty : t -> t
val butlast : t -> t
val pair : t -> t * t
val into : t -> t Seq.t -> t
val is_sequential : t -> bool
val is_seqable : t -> bool
val truthy : t -> bool
val is_nil : t -> bool
val is_symbol : t -> bool
val is_keyword : t -> bool
val is_string : t -> bool
val is_int : t -> bool
val is_float : t -> bool
val is_number : t -> bool
val is_zero : t -> bool
val is_positive : t -> bool
val is_negative : t -> bool
val is_bool : t -> bool
val is_array : t -> bool
val is_list : t -> bool
val is_vector : t -> bool
val is_seq : t -> bool
val is_map : t -> bool
val is_set : t -> bool
val is_coll : t -> bool
val is_instance : t -> string -> bool
val is_true : t -> bool
val is_false : t -> bool
val is_some : t -> bool
val identical : t -> t -> bool
val count_value : t -> int
val hash : t -> int
val hash_unordered_coll : t -> int
val as_transient : t -> t
val persistent : t -> t
val conj_bang : t -> t -> t
val dissoc_bang : t -> t -> t
val disj_bang : t -> t -> t
val set_union : t list -> t
val set_intersection : t list -> t
val set_difference : t -> t list -> t
val set_subset : t -> t -> bool
val as_int : t -> int
val to_int : t -> int
val as_float : t -> float
val as_char : t -> char
val as_string : t -> string
val as_symbol : t -> string
val as_keyword : t -> string
val as_bool : t -> bool
val as_identifier : t -> string
val as_named_identifier : t -> string
