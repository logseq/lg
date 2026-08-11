type 'a t

val create : int -> 'a t
val append : 'a t -> 'a -> unit
val to_array : 'a t -> 'a array
