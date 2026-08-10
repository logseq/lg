type ('key, 'value) basic
type ('key, 'value) lru
type ('key, 'value) ttl

val basic_of_map : ('key, 'value) Runtime_map.t -> ('key, 'value) basic
val basic_to_map : ('key, 'value) basic -> ('key, 'value) Runtime_map.t
val basic_lookup : ('key, 'value) basic -> 'key -> 'value option
val basic_lookup_default : ('key, 'value) basic -> 'key -> 'value -> 'value
val basic_contains : ('key, 'value) basic -> 'key -> bool
val basic_miss : ('key, 'value) basic -> 'key -> 'value -> ('key, 'value) basic
val basic_evict : ('key, 'value) basic -> 'key -> ('key, 'value) basic

val ttl_of_map :
  int -> float -> ('key, 'value) Runtime_map.t -> ('key, 'value) ttl
val ttl_to_map : ('key, 'value) ttl -> ('key, 'value) Runtime_map.t
val ttl_lookup : ('key, 'value) ttl -> 'key -> float -> 'value option
val ttl_lookup_default :
  ('key, 'value) ttl -> 'key -> 'value -> float -> 'value
val ttl_contains : ('key, 'value) ttl -> 'key -> float -> bool
val ttl_miss :
  ('key, 'value) ttl -> 'key -> 'value -> float -> ('key, 'value) ttl
val ttl_evict : ('key, 'value) ttl -> 'key -> ('key, 'value) ttl
val ttl_seed :
  ('key, 'value) ttl -> float -> ('key, 'value) Runtime_map.t -> ('key, 'value) ttl

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
