val map : 'a array -> ('a -> 'b) -> 'b array
val call0 : (unit -> 'a) -> int -> 'a
val call2 : ('a -> 'b -> 'c) -> int -> 'a -> 'b -> 'c
val sort : 'a array -> ('a -> 'a -> int) -> unit
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
val splice : 'a array -> int -> int -> int -> int -> 'a array -> 'a array
val splice_one : 'a array -> int -> int -> int -> int -> 'a -> 'a array
