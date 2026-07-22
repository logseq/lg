(** Runtime representation and operations for dynamically typed values. *)

type _ nominal_tag = ..
type nominal = Nominal : 'a nominal_tag * 'a -> nominal
type _ nominal_tag += Uuid_tag : Runtime_uuid.t nominal_tag
type _ nominal_tag += Host_tag : Obj.t nominal_tag
val dynamic_marker : unit ref
module Int_map : Map.S with type key = int
type t = {
  marker : unit ref;
  payload : payload;
  sequence : (unit -> t Seq.t) option;
  sequential : bool;
  protocols : protocol list;
  metadata : t option;
  type_name : string option;
  nominal : nominal option;
  associative : (t -> t -> t) option;
  lookup : (t -> t -> t) option;
  printer : (unit -> string) option;
}
and payload =
    Nil
  | Int of int64
  | Float of float
  | Char of char
  | String of string
  | Regex of string
  | Symbol of string
  | Keyword of string
  | Bool of bool
  | Function of Obj.t * (t list -> t)
  | Array of t array
  | List
  | Vector of t Rrbvec.t
  | Seq
  | Set of set_payload
  | Map of map_payload
  | Reference of dynamic_reference
  | Record of string * (string * (unit -> t)) list * (string * t) list
  | Opaque of string * (string * (unit -> t)) list
and dynamic_reference = { get : unit -> t; set : t -> t; }
and protocol = { id : string; methods : (string * (t list -> t)) list; }
and set_payload = { values : t list; index : t list Int_map.t; }
and map_payload = {
  entries : (t * t) Rrbvec.t;
  index : (t * int) list Int_map.t;
  size : int;
}
val protocol : string -> (string * (t list -> t)) list -> protocol
val wrong_protocol_method_arity : string -> 'a
val protocol_method_0 : string -> (unit -> 'a) -> string * ('b list -> 'a)
val protocol_method_1 : string -> ('a -> 'b) -> string * ('a list -> 'b)
val protocol_method_2 :
  string -> ('a -> 'a -> 'b) -> string * ('a list -> 'b)
val protocol_method_3 :
  string -> ('a -> 'a -> 'a -> 'b) -> string * ('a list -> 'b)
val protocol_method_4 :
  string -> ('a -> 'a -> 'a -> 'a -> 'b) -> string * ('a list -> 'b)
val protocol_method_5 :
  string -> ('a -> 'a -> 'a -> 'a -> 'a -> 'b) -> string * ('a list -> 'b)
val protocol_method_6 :
  string ->
  ('a -> 'a -> 'a -> 'a -> 'a -> 'a -> 'b) -> string * ('a list -> 'b)
val protocol_extensions : (string * string, t -> t) Hashtbl.t
val lookup_extensions : (string, t -> t -> t -> t) Hashtbl.t
val printer_extensions : (string, t -> string) Hashtbl.t
val record_packers : (string, Obj.t -> t) Hashtbl.t
val register_record_packer : string -> ('a -> t) -> unit
val pack_record : string -> 'a -> t
val register_protocol_extension : string -> string -> (t -> t) -> unit
val register_lookup_extension : string -> (t -> t -> t -> t) -> unit
val register_printer_extension : string -> (t -> string) -> unit
val lookup_extension : t -> (t -> t -> t -> t) option
val printer_extension : t -> (t -> string) option
val has_protocol_extension : t -> string -> bool
val protocol_extension : t -> string -> t option
val make :
  ?sequence:(unit -> t Seq.t) ->
  ?sequential:bool ->
  ?protocols:protocol list ->
  ?metadata:t -> ?type_name:string -> payload -> t
val with_protocols : t -> protocol list -> t
val with_metadata : t -> t -> t
val with_nominal : 'a nominal_tag -> 'a -> t -> t
val with_assoc : t -> (t -> t -> t) -> t
val with_lookup : t -> (t -> t -> t) -> t
val with_sequence : t -> (unit -> t Seq.t) -> t
val with_printer : t -> (unit -> string) -> t
val nominal : t -> nominal option
val unpack_nominal : 'a -> t -> Obj.t option
val nil : t
val int : int64 -> t
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
val function_with_identity : 'a -> (t list -> t) -> t
val function_ : (t list -> t) -> t
val wrong_function_arity : unit -> 'a
val function_adapter_1 : (t -> 'a) -> ('b -> t) -> ('a -> 'b) -> t
val function_adapter_2 :
  (t -> 'a) -> (t -> 'b) -> ('c -> t) -> ('a -> 'b -> 'c) -> t
val function_adapter_3 :
  (t -> 'a) ->
  (t -> 'b) -> (t -> 'c) -> ('d -> t) -> ('a -> 'b -> 'c -> 'd) -> t
val function_adapter_4 :
  (t -> 'a) ->
  (t -> 'b) ->
  (t -> 'c) -> (t -> 'd) -> ('e -> t) -> ('a -> 'b -> 'c -> 'd -> 'e) -> t
val function_adapter_5 :
  (t -> 'a) ->
  (t -> 'b) ->
  (t -> 'c) ->
  (t -> 'd) ->
  (t -> 'e) -> ('f -> t) -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> t
val function_adapter_6 :
  (t -> 'a) ->
  (t -> 'b) ->
  (t -> 'c) ->
  (t -> 'd) ->
  (t -> 'e) ->
  (t -> 'f) -> ('g -> t) -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> t
val is_function : t -> bool
val reference : (unit -> t) -> (t -> t) -> t
val runtime_vars : (string, t ref * t) Hashtbl.t
val register_var : string -> t -> unit
val requiring_resolve : string -> t option
val opaque : string -> (string * (unit -> t)) list -> t
val host : string -> 'a -> t
val as_host : String.t -> t -> 'a
val narrow_like : 'expected_type -> t -> 'expected_type
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
val seq : t Seq.t -> t
val map : (t * t) list -> t
val map_entries : map_payload -> (t * t) list
val record : string -> (t * t) list -> t
val lazy_record :
  string -> (string * (unit -> t)) list -> (string * t) list -> t
val find_protocol : t -> string -> protocol option
val find_protocol_method : t -> string -> string -> (t list -> t) option
val nominal_identity_equal : t -> t -> bool
val same_nominal_type : t -> t -> bool
val expand_record_extension_entries : (t * t) list -> (t * t) list
val equal : t -> t -> bool
val equal_payload : t -> t -> bool
val is_runtime_dynamic : 'a -> bool
val polymorphic_equal : 'a -> 'a -> bool
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
val equality_function : t
val inequality_function : t
val payload_rank : payload -> int
val compare_sequences : t Seq.t -> t Seq.t -> int
val compare_entries : t * t -> t * t -> int
val compare_entry_lists : (t * t) list -> (t * t) list -> int
val compare_identifier : String.t -> String.t -> int
val compare : t -> t -> int
val compare_int64 : t -> t -> int64
val unary_function : string -> (t -> t) -> t
val binary_function : string -> (t -> t -> t) -> t
val int_quot : int64 -> int64 -> int64
val int_rem : int64 -> int64 -> int64
val int_binary_function : string -> (int64 -> int64 -> int64) -> t
val quot_function : t
val rem_function : t
val clojure_mod : int64 -> int64 -> int64
val mod_function : t
val int_inc : int64 -> int64
val int_dec : int64 -> int64
val int_max : 'a -> 'a -> 'a
val int_min : 'a -> 'a -> 'a
val int_zero : int64 -> bool
val int_positive : int64 -> bool
val int_negative : int64 -> bool
val int_even : int64 -> bool
val int_odd : int64 -> bool
val int_compare : Int64.t -> Int64.t -> int
val numeric_unary_function :
  string -> (int64 -> int64) -> (float -> float) -> t
val inc_function : t
val dec_function : t
val numeric_predicate_function :
  string -> (int64 -> bool) -> (float -> bool) -> t
val zero_function : t
val positive_function : t
val negative_function : t
val integer_predicate_function : string -> (int64 -> bool) -> t
val even_function : t
val odd_function : t
val compare_function : t
val extremum_function : string -> (t -> t -> t) -> t
val max_function : t
val min_function : t
val rand_function : t
val rand_int_function : t
val sort : t -> t
val class_ : t -> t
val is_comparable : t -> bool
val set : t Seq.t -> t
val cons : t -> t -> t
val concat : t list -> t
val conj : t -> t -> t
val assoc : t -> t -> t -> t
val merge : t list -> t
val dissoc : t -> t -> t
val dissoc_function : t
val select_keys : t -> t Seq.t -> t
val vals : t -> t
val keys : t -> t
val array_get : t -> int -> t
val array_set : t -> int -> t -> unit
val array_unsafe_set : t -> int -> t -> unit
val vector_nth_opt : t -> int -> t option
val subvec_value : t -> int -> int -> t
val nominal_field_name : payload -> string option
val payload_get : t -> t -> t option
val get : t -> t -> t
val indexed_get : t -> t -> t
val get_default : t -> t -> t -> t
val contains : t -> t -> bool
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
val call : t -> t list -> t
val predicate_function : string -> (t -> bool) -> t
val is_true : t -> bool
val is_false : t -> bool
val is_some : t -> bool
val not_value : t -> bool
val true_function : t
val false_function : t
val nil_function : t
val some_function : t
val bool_not : bool -> bool
val not_function : t
val identical : t -> t -> bool
val identical_function : t
val identity_value : 'a -> 'a
val identity_function : t
val complement_value : t -> t
val complement_function : t
val strip_keyword_prefix : string -> string
val identifier_value : t -> string
val named_identifier_value : t -> string
val keyword_value : t -> t
val keyword_function : t
val identifier_name : t -> string
val identifier_namespace : t -> t
val name_value : t -> t
val name_function : t
val namespace_function : t
val meta_function : t
val type_function : t
val vector_function : t
val list_function : t
val set_function : t
val map_from_arguments : string -> t list -> t
val hash_map_function : t
val array_map_function : t
val count_value : t -> int
val count_function : t
val range_sequence : int64 -> int64 -> int64 -> t Seq.t
val range_function : t
val not_empty_function : t
val empty_predicate_value : t -> bool
val empty_predicate_function : t
val contains_function : t
val str_value : t -> string
val str_function : t
val subs_function : t
val get_function : t
val joined_string : pr:bool -> t list -> string
val pr_str_function : t
val print_str_function : t
val println_str_function : t
val prn_str_function : t
val escape_function : t
val dynamic_string_value : t -> string
val dynamic_string_unary : string -> (string -> string) -> t
val dynamic_string_predicate : string -> (string -> bool) -> t
val dynamic_string_binary : string -> (string -> string -> string) -> t
val dynamic_string_binary_predicate :
  string -> (string -> string -> bool) -> t
val string_blank_function : t
val string_includes_function : t
val string_starts_with_function : t
val string_ends_with_function : t
val string_lower_case_function : t
val string_upper_case_function : t
val string_capitalize_function : t
val string_join_function : t
val string_index_of_function : t
val string_last_index_of_function : t
val dynamic_string_ternary :
  string -> (string -> string -> string -> string) -> t
val string_replace_function : t
val string_replace_first_function : t
val string_reverse_function : t
val string_split_function : t
val string_split_lines_function : t
val string_trim_function : t
val string_trim_newline_function : t
val string_triml_function : t
val string_trimr_function : t
val deref : t -> t
val reset : t -> t -> t
val swap : t -> t -> t list -> t
val hash : t -> int
val hash_unordered_coll : t -> int
val group_by : ('a -> 'b) -> ('b -> t) -> ('a -> t) -> 'a Seq.t -> t
val has_protocol : t -> string -> bool
val invoke : t -> string -> string -> t list -> t
val as_transient : t -> t
val persistent : t -> t
val conj_bang : t -> t -> t
val assoc_bang : t -> t -> t -> t
val dissoc_bang : t -> t -> t
val disj_bang : t -> t -> t
val invoke_function : t -> t list -> t
val set_union : t list -> t
val set_intersection : t list -> t
val set_difference : t -> t list -> t
val set_subset : t -> t -> bool
val update_in : t -> t Seq.t -> t -> t list -> t
val update : t -> t -> t -> t list -> t
val update_function : t
val as_int : t -> int64
val to_int : t -> int64
val as_float : t -> float
val as_char : t -> char
val as_string : t -> string
val as_symbol : t -> string
val as_keyword : t -> string
val as_bool : t -> bool
val as_identifier : t -> string
val as_named_identifier : t -> string
