type t = Lg_edn_backend.t

type schema =
  (string, (string, Data_value.t) Lg_runtime.Lg_map.t) Lg_runtime.Lg_map.t

val keyword_reference : int -> t
val encode_non_keyword : Data_value.t -> t
val decode_value : string Rrbvec.t -> t -> Data_value.t

type encoder

val create_encoder : unit -> encoder
val encode_value : encoder -> Data_value.t -> t
val encoder_keywords : encoder -> string Rrbvec.t
val attribute_index : string Rrbvec.t -> string -> int

val datom : int -> int -> t -> int -> t
val datom_entity : t -> int
val datom_attribute : t -> int
val datom_value : t -> t
val datom_tx : t -> int

val database :
  int ->
  int ->
  int ->
  int ->
  string ->
  string Rrbvec.t ->
  string Rrbvec.t ->
  t Rrbvec.t ->
  int Rrbvec.t option ->
  int Rrbvec.t option ->
  int ->
  Storage_value.ref_type ->
  t

val count : t -> int
val tx0 : t -> int
val max_eid : t -> int
val max_tx : t -> int
val schema_source : t -> string
val attrs : t -> string Rrbvec.t
val keywords : t -> string Rrbvec.t
val datoms : t -> t Rrbvec.t
val aevt : t -> int Rrbvec.t option
val avet : t -> int Rrbvec.t option
val branching_factor : t -> int
val ref_type : t -> Storage_value.ref_type

val schema_to_string : schema -> string
val schema_of_string : string -> schema
