type 'a t

val adopt : release:('a Ctypes.ptr -> unit) -> 'a Ctypes.ptr -> 'a t
(** Adopt a non-NULL pointer with its matching deallocator. Ownership must not
    already belong to another handle or to the OCaml allocator. *)

val address : 'a t -> 'a Ctypes.ptr
(** Borrow the address of a live handle. Raises [Invalid_argument] after release. *)

val release : 'a t -> unit
(** Release once. Repeated calls are harmless. *)

val parameter_type : 'a Ctypes.typ -> 'a t Ctypes.typ
val result_type : release:('a Ctypes.ptr -> unit) -> 'a Ctypes.typ -> 'a t Ctypes.typ

val with_pointer :
  release:('a Ctypes.ptr -> unit) -> 'a Ctypes.ptr -> ('a t -> 'b) -> 'b
(** Release on normal return and exceptions, including if the handle escapes. *)
