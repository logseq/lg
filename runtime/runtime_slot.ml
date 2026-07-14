type 'value t = (unit -> 'value) ref

let empty () = ref (fun () -> invalid_arg "uninitialized macro slot")

let set slot value =
  slot := (fun () -> value);
  value

let get slot = !slot ()
