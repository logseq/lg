let write path text =
  let output = open_out path in
  Fun.protect ~finally:(fun () -> close_out output)
    (fun () -> output_string output text)

let () =
  let dir = Filename.temp_dir "lg-ffi-lifetime-" "" in
  Fun.protect ~finally:(fun () ->
    Array.iter (fun name -> Sys.remove (Filename.concat dir name)) (Sys.readdir dir);
    Unix.rmdir dir) (fun () ->
    let source = Filename.concat dir "fixture.c" in
    let library = Filename.concat dir "fixture.so" in
    write source {|#include <stdlib.h>
static int releases = 0;
int *create(int value) { int *p = malloc(sizeof(int)); if (p) *p = value; return p; }
void destroy(int *p) { releases++; free(p); }
int released(void) { return releases; }
static int (*callback)(int) = NULL;
void register_callback(int (*f)(int)) { callback = f; }
int invoke_callback(int value) { return callback(value); }
void unregister_callback(void) { callback = NULL; }
|};
    let command = Printf.sprintf "cc -shared -fPIC %s -o %s"
      (Filename.quote source) (Filename.quote library) in
    if Sys.command command <> 0 then failwith "C fixture compilation failed";
    let from = Dl.dlopen ~filename:library ~flags:[Dl.RTLD_NOW] in
    let open Ctypes in
    let create = Foreign.foreign ~from "create" (int @-> returning (ptr int)) in
    let destroy = Foreign.foreign ~from "destroy" (ptr int @-> returning void) in
    let released = Foreign.foreign ~from "released" (void @-> returning int) in
    let module Pointer = Lg_ffi.Owned_pointer in
    let handle = Pointer.adopt ~release:destroy (create 42) in
    assert (!@(Pointer.address handle) = 42);
    Pointer.release handle;
    Pointer.release handle;
    assert (released () = 1);
    (match Pointer.address handle with
     | _ -> failwith "released pointer remained accessible"
     | exception Invalid_argument _ -> ());
    let value = Pointer.with_pointer ~release:destroy (create 17)
      (fun handle -> !@(Pointer.address handle)) in
    assert (value = 17);
    assert (released () = 2);
    (match Pointer.with_pointer ~release:destroy (create 19)
      (fun _ -> failwith "body failure") with
     | _ -> failwith "scoped pointer swallowed exception"
     | exception Failure message -> assert (message = "body failure"));
    assert (released () = 3);
    let escaped = Pointer.with_pointer ~release:destroy (create 23) Fun.id in
    assert (released () = 4);
    (match Pointer.address escaped with
     | _ -> failwith "escaped scoped pointer remained accessible"
     | exception Invalid_argument _ -> ());
    (match Pointer.adopt ~release:destroy (from_voidp int null) with
     | _ -> failwith "NULL pointer was adopted"
     | exception Invalid_argument _ -> ());
    assert (released () = 4);
    let signature = int @-> returning int in
    let module Callback = Lg_ffi.Callback in
    let register = Foreign.foreign ~from "register_callback"
      (Callback.parameter_type signature @-> returning void) in
    let invoke = Foreign.foreign ~from "invoke_callback" (int @-> returning int) in
    let unregister = Foreign.foreign ~from "unregister_callback" (void @-> returning void) in
    let weak = Weak.create 1 in
    let make_handle () =
      let captured = ref 23 in
      Weak.set weak 0 (Some captured);
      Callback.create signature (fun x -> !captured - x)
    in
    let callback = make_handle () in
    register callback;
    Gc.full_major ();
    assert (Weak.check weak 0);
    assert (invoke 3 = 20);
    unregister ();
    Callback.release callback;
    Callback.release callback;
    Gc.full_major ();
    Gc.full_major ();
    assert (not (Weak.check weak 0));
    (match register callback with
     | () -> failwith "released callback was passed to C"
     | exception Invalid_argument _ -> ()))
