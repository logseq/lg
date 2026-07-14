module Int_set = Set.Make (Int)

let iterations = 200_000
let samples = 7

let elapsed run =
  let started = Sys.time () in
  let result = run () in
  (Sys.time () -. started, result)

let measure run =
  ignore (run ());
  let measurements = Array.init samples (fun _ -> elapsed run) in
  let expected = snd measurements.(0) in
  Array.iter (fun (_, result) -> assert (result = expected)) measurements;
  let times = Array.map fst measurements in
  Array.sort Float.compare times;
  (times.(samples / 2), expected)

let persistent_set () =
  let result = ref Int_set.empty in
  for value = 0 to iterations - 1 do
    if not (Int_set.mem value !result) then
      result := Int_set.add value !result
  done;
  Int_set.cardinal !result

let transient_set () =
  let result = Lg_runtime.Runtime_transient.set_empty () in
  for value = 0 to iterations - 1 do
    if not (Lg_runtime.Runtime_transient.set_mem result value) then
      ignore (Lg_runtime.Runtime_transient.set_add result value)
  done;
  Lg_runtime.Runtime_transient.set_count result

let transient_set_with_freeze () =
  let result = Lg_runtime.Runtime_transient.set_empty () in
  for value = 0 to iterations - 1 do
    if not (Lg_runtime.Runtime_transient.set_mem result value) then
      ignore (Lg_runtime.Runtime_transient.set_add result value)
  done;
  result |> Lg_runtime.Runtime_transient.set_to_seq |> Int_set.of_seq
  |> Int_set.cardinal

let persistent_distinct () =
  let seen = ref Int_set.empty in
  let result = ref Rrbvec.empty in
  for index = 0 to iterations - 1 do
    let value = index mod (iterations / 2) in
    if not (Int_set.mem value !seen) then (
      seen := Int_set.add value !seen;
      result := Rrbvec.push_back !result value)
  done;
  Rrbvec.length !result

let transient_distinct () =
  let seen = Lg_runtime.Runtime_transient.set_empty () in
  let result = Lg_runtime.Runtime_transient.vector_empty () in
  for index = 0 to iterations - 1 do
    let value = index mod (iterations / 2) in
    if not (Lg_runtime.Runtime_transient.set_mem seen value) then (
      ignore (Lg_runtime.Runtime_transient.set_add seen value);
      ignore (Lg_runtime.Runtime_transient.vector_add result value))
  done;
  result |> Lg_runtime.Runtime_transient.vector_persistent |> Rrbvec.length

let () =
  let persistent_time, persistent_count = measure persistent_set in
  let transient_time, transient_count = measure transient_set in
  let freeze_time, freeze_count = measure transient_set_with_freeze in
  Printf.printf
    "persistent=%.6f transient=%.6f speedup=%.2f frozen=%.6f counts=%d:%d:%d\n"
    persistent_time transient_time (persistent_time /. transient_time) freeze_time
    persistent_count transient_count freeze_count;
  let persistent_distinct_time, persistent_distinct_count =
    measure persistent_distinct
  in
  let transient_distinct_time, transient_distinct_count =
    measure transient_distinct
  in
  Printf.printf
    "distinct persistent=%.6f transient=%.6f speedup=%.2f counts=%d:%d\n"
    persistent_distinct_time transient_distinct_time
    (persistent_distinct_time /. transient_distinct_time)
    persistent_distinct_count transient_distinct_count
