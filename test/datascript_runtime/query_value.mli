type 'db result =
  | Entity of int
  | Attr of string
  | Value of Data_value.t
  | Database of 'db
  | Pull of Data_value.t
  | Added of bool

type 'db source =
  | Database_source of 'db
  | Relation_source of 'db result array Rrbvec.t

type 'db binding_value =
  | Scalar_binding of 'db result
  | Collection_binding of 'db binding_value Rrbvec.t

type ('db, 'rules) input =
  | Source_input of 'db source
  | Rules_input of 'rules
  | Binding_input of 'db binding_value

type 'db output =
  | Relation_output of 'db result array Rrbvec.t
  | Collection_output of 'db result Rrbvec.t
  | Scalar_output of 'db result option
  | Tuple_output of 'db result array option

type 'db relation
type ('db, 'rules) context

val database_source : 'db -> 'db source
val relation_source : 'db result array Rrbvec.t -> 'db source
val entity : int -> 'db result
val attr : string -> 'db result
val value : Data_value.t -> 'db result
val database : 'db -> 'db result
val pull : Data_value.t -> 'db result
val added : bool -> 'db result
val source_database : 'db source -> 'db option
val source_rows : 'db source -> 'db result array Rrbvec.t option
val result_value : 'db result -> Data_value.t option
val scalar_binding : 'db result -> 'db binding_value
val collection_binding : 'db binding_value Rrbvec.t -> 'db binding_value
val binding_result : 'db binding_value -> 'db result option

val binding_items :
  'db binding_value -> 'db binding_value Rrbvec.t option

val source_input : 'db source -> ('db, 'rules) input
val rules_input : 'rules -> ('db, 'rules) input
val binding_input : 'db binding_value -> ('db, 'rules) input
val input_source : ('db, 'rules) input -> 'db source option
val input_rules : ('db, 'rules) input -> 'rules option
val input_binding : ('db, 'rules) input -> 'db binding_value option
val relation_output : 'db result array Rrbvec.t -> 'db output
val collection_output : 'db result Rrbvec.t -> 'db output
val scalar_output : 'db result option -> 'db output
val tuple_output : 'db result array option -> 'db output

val output_relation :
  'db output -> 'db result array Rrbvec.t option

val output_collection : 'db output -> 'db result Rrbvec.t option
val output_scalar : 'db output -> 'db result option option
val output_tuple : 'db output -> 'db result array option option
val empty_row : unit -> 'db result array
val row_get : 'db result array -> int -> 'db result option
val project_row : 'db result array -> int array -> 'db result array

val join_rows :
  'db result array ->
  int array ->
  'db result array ->
  int array ->
  'db result array

val concat_rows : 'db result array -> 'db result array -> 'db result array

val product_rows :
  'db result array Rrbvec.t ->
  'db result array Rrbvec.t ->
  'db result array Rrbvec.t

val relation :
  (string, int) Lg_runtime.Lg_map.t ->
  'db result array Rrbvec.t ->
  (string, 'db) Lg_runtime.Lg_map.t ->
  'db relation

val index_attrs : string Rrbvec.t -> (string, int) Lg_runtime.Lg_map.t
val relation_attrs : 'db relation -> (string, int) Lg_runtime.Lg_map.t
val relation_rows : 'db relation -> 'db result array Rrbvec.t

val relation_lookup_databases :
  'db relation -> (string, 'db) Lg_runtime.Lg_map.t

val relation_result :
  'db relation -> string -> 'db result array -> 'db result option

val relation_lookup_database : 'db relation -> string -> 'db option
val relation_with_rows : 'db relation -> 'db result array Rrbvec.t -> 'db relation
val relation_append_rows : 'db relation -> 'db relation -> 'db relation
val equal_result : 'db result -> 'db result -> bool

val hash_join :
  ('db -> 'db result -> 'db result) ->
  'db relation ->
  'db relation ->
  'db relation

val context :
  'db relation Rrbvec.t ->
  (string, 'db source) Lg_runtime.Lg_map.t ->
  'rules ->
  ('db, 'rules) context

val context_relations : ('db, 'rules) context -> 'db relation Rrbvec.t

val context_sources :
  ('db, 'rules) context -> (string, 'db source) Lg_runtime.Lg_map.t

val context_rules : ('db, 'rules) context -> 'rules
