type 'a state = Live of 'a Ctypes.ptr | Released

type 'a t = {
  mutable state : 'a state;
  deallocate : 'a Ctypes.ptr -> unit;
}

let adopt ~release pointer =
  if Ctypes.is_null pointer then invalid_arg "cannot own a NULL pointer";
  {state = Live pointer; deallocate = release}

let address handle =
  match handle.state with
  | Live pointer -> pointer
  | Released -> invalid_arg "foreign pointer has been released"

let release handle =
  match handle.state with
  | Released -> ()
  | Live pointer ->
      handle.state <- Released;
      handle.deallocate pointer

let with_pointer ~release:deallocate pointer fn =
  let handle = adopt ~release:deallocate pointer in
  Fun.protect ~finally:(fun () -> release handle) (fun () -> fn handle)

let parameter_type ty =
  Ctypes.view ~read:(fun _ -> invalid_arg "owned pointer requires a deallocator")
    ~write:address (Ctypes.ptr ty)

let result_type ~release ty =
  Ctypes.view ~read:(adopt ~release) ~write:address (Ctypes.ptr ty)
