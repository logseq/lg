type 'value t = (unit -> 'value) ref

let empty () = ref (fun () -> invalid_arg "uninitialized macro slot")

let of_value value = ref (fun () -> value)

let set slot value =
  slot := (fun () -> value);
  value

let vreset slot value =
  slot := (fun () -> value);
  value

let get slot = !slot ()
