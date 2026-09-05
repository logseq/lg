let () =
  let before = Lg_runtime.Runtime_time.now () in
  Unix.sleepf 0.08;
  let elapsed = Lg_runtime.Runtime_time.now () -. before in
  if elapsed < 60. then
    failwith (Printf.sprintf "clock omitted elapsed waiting time: %.3f ms" elapsed);
  let previous = ref (Lg_runtime.Runtime_time.now ()) in
  for _ = 1 to 1000 do
    let current = Lg_runtime.Runtime_time.now () in
    assert (current >= !previous);
    previous := current
  done
