type t = Lg_edn_backend.t
type format = Current | Legacy

type schema =
  (string, (string, Data_value.t) Lg_runtime.Lg_map.t) Lg_runtime.Lg_map.t
  option

val keyword_reference : int -> t
val encode_non_keyword : Data_value.t -> t
val encode_non_keyword_with : (t -> t) -> Data_value.t -> t
val decode_value : string Rrbvec.t -> t -> Data_value.t
val decode_value_with : (t -> t) -> string Rrbvec.t -> t -> Data_value.t
val data_value_of_edn_string : string -> Data_value.t

type encoder

val create_encoder : unit -> encoder
val encode_value : encoder -> Data_value.t -> t
val encode_value_with : encoder -> (t -> t) -> Data_value.t -> t
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

val database_with_schema :
  int ->
  int ->
  int ->
  int ->
  t ->
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
val schema_value : t -> t
val attrs : t -> string Rrbvec.t
val keywords : t -> string Rrbvec.t
val datoms : t -> t Rrbvec.t
val aevt : t -> int Rrbvec.t option
val avet : t -> int Rrbvec.t option
val branching_factor : t -> int
val ref_type : t -> Storage_value.ref_type

val schema_to_string :
  (string, (string, Data_value.t) Lg_runtime.Lg_map.t) Lg_runtime.Lg_map.t
  option ->
  string

val schema_of_string :
  string ->
  (string, (string, Data_value.t) Lg_runtime.Lg_map.t) Lg_runtime.Lg_map.t
  option

type datom_reader_value
type database_reader_value

val read_datom : string -> datom_reader_value
val reader_datom_entity : datom_reader_value -> int
val reader_datom_attribute : datom_reader_value -> string
val reader_datom_value : datom_reader_value -> Data_value.t
val reader_datom_transaction : datom_reader_value -> int
val reader_datom_added : datom_reader_value -> bool

val read_database : string -> database_reader_value
val reader_database_schema : database_reader_value -> schema
val reader_database_datoms :
  database_reader_value -> datom_reader_value Rrbvec.t

val schema_to_value :
  (string, (string, Data_value.t) Lg_runtime.Lg_map.t) Lg_runtime.Lg_map.t
  option ->
  t

val schema_of_value :
  t ->
  (string, (string, Data_value.t) Lg_runtime.Lg_map.t) Lg_runtime.Lg_map.t
  option
val format : t -> format
val as_legacy : t -> t
