type 'item t

val empty : 'item t
val count : 'item t -> int
val contains : 'item t -> 'item -> bool
val lookup : 'item t -> 'item -> int option
val assoc : 'item t -> 'item -> int -> 'item t
val dissoc : 'item t -> 'item -> 'item t
val peek : 'item t -> ('item * int) option
