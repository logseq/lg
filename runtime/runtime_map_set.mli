type ('key, 'value) t

val empty : ('key, 'value) t
val mem : ('key, 'value) Runtime_map.t -> ('key, 'value) t -> bool
val add : ('key, 'value) Runtime_map.t -> ('key, 'value) t -> ('key, 'value) t
val remove : ('key, 'value) Runtime_map.t -> ('key, 'value) t -> ('key, 'value) t
val of_list : ('key, 'value) Runtime_map.t list -> ('key, 'value) t
val of_seq : ('key, 'value) Runtime_map.t Seq.t -> ('key, 'value) t
val elements : ('key, 'value) t -> ('key, 'value) Runtime_map.t list
val cardinal : ('key, 'value) t -> int
val is_empty : ('key, 'value) t -> bool
val min_elt : ('key, 'value) t -> ('key, 'value) Runtime_map.t
val max_elt : ('key, 'value) t -> ('key, 'value) Runtime_map.t

val fold :
  (('key, 'value) Runtime_map.t -> 'accumulator -> 'accumulator) ->
  ('key, 'value) t ->
  'accumulator ->
  'accumulator

val subset : ('key, 'value) t -> ('key, 'value) t -> bool
val equal : ('key, 'value) t -> ('key, 'value) t -> bool
val union : ('key, 'value) t -> ('key, 'value) t -> ('key, 'value) t
val inter : ('key, 'value) t -> ('key, 'value) t -> ('key, 'value) t
val diff : ('key, 'value) t -> ('key, 'value) t -> ('key, 'value) t
