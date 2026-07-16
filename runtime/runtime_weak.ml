type 'a t = { mutable read : unit -> 'a option }

let of_getter read = { read }
let get reference = reference.read ()
let clear reference = reference.read <- (fun () -> None)
