type t = {
  id : int;
  store_fn : (int * Storage_value.t) Rrbvec.t -> int Rrbvec.t -> unit;
  restore_fn : int -> Storage_value.t option;
  list_addresses_fn : unit -> int Rrbvec.t;
  delete_fn : int Rrbvec.t -> unit;
}

let next_id = ref 0

let create store_fn restore_fn list_addresses_fn delete_fn =
  incr next_id;
  { id = !next_id; store_fn; restore_fn; list_addresses_fn; delete_fn }

let equal left right = left.id = right.id
let store backend = backend.store_fn
let restore backend = backend.restore_fn
let list_addresses backend = backend.list_addresses_fn ()
let delete backend = backend.delete_fn
