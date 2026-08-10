type ('key, 'value) basic
type ('key, 'value) lru

val basic_of_map : ('key, 'value) Runtime_map.t -> ('key, 'value) basic
val basic_to_map : ('key, 'value) basic -> ('key, 'value) Runtime_map.t
val basic_lookup : ('key, 'value) basic -> 'key -> 'value option
val basic_lookup_default : ('key, 'value) basic -> 'key -> 'value -> 'value
val basic_contains : ('key, 'value) basic -> 'key -> bool
val basic_miss : ('key, 'value) basic -> 'key -> 'value -> ('key, 'value) basic
val basic_evict : ('key, 'value) basic -> 'key -> ('key, 'value) basic

val lru_of_map : int -> ('key, 'value) Runtime_map.t -> ('key, 'value) lru
val lru_to_map : ('key, 'value) lru -> ('key, 'value) Runtime_map.t
val lru_lookup : ('key, 'value) lru -> 'key -> 'value option
val lru_lookup_default : ('key, 'value) lru -> 'key -> 'value -> 'value
val lru_contains : ('key, 'value) lru -> 'key -> bool
val lru_hit : ('key, 'value) lru -> 'key -> ('key, 'value) lru
val lru_miss : ('key, 'value) lru -> 'key -> 'value -> ('key, 'value) lru
val lru_evict : ('key, 'value) lru -> 'key -> ('key, 'value) lru
val lru_seed :
  ('key, 'value) lru -> ('key, 'value) Runtime_map.t -> ('key, 'value) lru
