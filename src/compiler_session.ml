let compiler_libs_lock = Mutex.create ()

let run operation =
  Mutex.lock compiler_libs_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock compiler_libs_lock) operation
