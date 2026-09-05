type 'fn live = {
  pointer : 'fn Ctypes.static_funptr;
  dispose : unit -> unit;
}

type 'fn t = { mutable live : 'fn live option }

let create (type a b) (signature : (a -> b) Ctypes.fn) callback =
  let module Pointer = (val Foreign.dynamic_funptr signature) in
  let handle = Pointer.of_fun callback in
  (* Both descriptors describe the same function-pointer ABI and signature. *)
  let pointer = Ctypes.coerce Pointer.t (Ctypes.static_funptr signature) handle in
  {live = Some {pointer; dispose = (fun () -> Pointer.free handle)}}

let parameter_type signature =
  Ctypes.view
    ~read:(fun _ -> invalid_arg "retained callbacks must be created explicitly")
    ~write:(fun handle -> match handle.live with
      | Some live -> live.pointer
      | None -> invalid_arg "foreign callback has been released")
    (Ctypes.static_funptr signature)

let release handle =
  match handle.live with
  | None -> ()
  | Some live ->
      handle.live <- None;
      live.dispose ()
