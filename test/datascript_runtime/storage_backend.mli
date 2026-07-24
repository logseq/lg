type t

val create :
  ((int * Storage_value.t) Rrbvec.t -> int Rrbvec.t -> unit) ->
  (int -> Storage_value.t option) ->
  (unit -> int Rrbvec.t) ->
  (int Rrbvec.t -> unit) ->
  t

val equal : t -> t -> bool
val store : t -> (int * Storage_value.t) Rrbvec.t -> int Rrbvec.t -> unit
val restore : t -> int -> Storage_value.t option
val list_addresses : t -> int Rrbvec.t
val delete : t -> int Rrbvec.t -> unit
