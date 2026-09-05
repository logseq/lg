type 'fn t

val create : ('a -> 'b) Ctypes.fn -> ('a -> 'b) -> ('a -> 'b) t
(** Retain a callback until explicit release. The C signature must match the
    registration function. Callbacks run on the calling OCaml thread. *)

val parameter_type : ('a -> 'b) Ctypes.fn -> ('a -> 'b) t Ctypes.typ
(** A typed function-pointer view that rejects released handles. *)

val release : 'fn t -> unit
(** Release once, after unregistering the callback from C. *)
