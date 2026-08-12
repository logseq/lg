type serialized_datom = {
  e : int;
  a : string;
  v : Data_value.t;
  tx : int;
}

type serialized_node = {
  keys : serialized_datom Rrbvec.t;
  addresses : int Rrbvec.t option;
}

type serialized_index = {
  address : int;
  shift : int;
  count : int;
}

type ref_type = Lg_runtime.Runtime_ref_type.t = Strong | Weak

type serialized_root = {
  schema :
    (string, (string, Data_value.t) Lg_runtime.Lg_map.t) Lg_runtime.Lg_map.t
    option;
  max_eid : int;
  max_tx : int;
  eavt : int;
  aevt : int;
  avet : int;
  eavt_metadata : serialized_index;
  aevt_metadata : serialized_index;
  avet_metadata : serialized_index;
  max_address : int;
  branching_factor : int;
  ref_type : ref_type;
}

type t =
  | Stored_node of serialized_node
  | Stored_root of serialized_root
  | Stored_tail of serialized_datom Rrbvec.t Rrbvec.t

val serialized_datom : int -> string -> Data_value.t -> int -> serialized_datom
val serialized_keyword_datom :
  int -> Lg_runtime.Runtime_keyword.t -> Data_value.t -> int -> serialized_datom
val serialized_node : serialized_datom Rrbvec.t -> int Rrbvec.t option -> serialized_node
val serialized_index : int -> int -> int -> serialized_index
val datom_e : serialized_datom -> int
val datom_a : serialized_datom -> string
val datom_v : serialized_datom -> Data_value.t
val datom_tx : serialized_datom -> int
val node_keys : serialized_node -> serialized_datom Rrbvec.t
val node_addresses : serialized_node -> int Rrbvec.t option
val index_address : serialized_index -> int
val index_shift : serialized_index -> int
val index_count : serialized_index -> int

val serialized_root :
  (string, (string, Data_value.t) Lg_runtime.Lg_map.t) Lg_runtime.Lg_map.t
  option ->
  int ->
  int ->
  int ->
  int ->
  int ->
  serialized_index ->
  serialized_index ->
  serialized_index ->
  int ->
  int ->
  ref_type ->
  serialized_root

val root_schema :
  serialized_root ->
  (string, (string, Data_value.t) Lg_runtime.Lg_map.t) Lg_runtime.Lg_map.t
  option

val root_max_eid : serialized_root -> int
val root_max_tx : serialized_root -> int
val root_eavt : serialized_root -> int
val root_aevt : serialized_root -> int
val root_avet : serialized_root -> int
val root_eavt_metadata : serialized_root -> serialized_index
val root_aevt_metadata : serialized_root -> serialized_index
val root_avet_metadata : serialized_root -> serialized_index
val root_max_address : serialized_root -> int
val root_branching_factor : serialized_root -> int
val root_ref_type : serialized_root -> ref_type
