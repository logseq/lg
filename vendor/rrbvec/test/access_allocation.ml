let allocated_read_bytes vector =
  let n = Rrbvec.length vector in
  for i = 0 to n - 1 do ignore (Sys.opaque_identity (Rrbvec.nth vector i)) done;
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  for _ = 1 to 10 do
    for i = 0 to n - 1 do
      ignore (Sys.opaque_identity (Rrbvec.nth vector i))
    done
  done;
  Gc.allocated_bytes () -. before

let () =
  let regular = Rrbvec.init 6001 (fun i -> ref i) in
  let relaxed = Rrbvec.append regular (Rrbvec.init 60000 (fun i -> ref i)) in
  let sliced = Option.get (Rrbvec.subvec relaxed 17 64003) in
  let failures = ref [] in
  List.iter (fun (name, vector) ->
    let bytes = allocated_read_bytes vector in
    Printf.printf "%s: %.0f bytes for ten complete indexed reads\n%!" name bytes;
    (* Reading persistent values should not allocate per element or tree level.
       Allow fixed measurement overhead without relying on wall-clock timing. *)
    if bytes > 1024. then failures := name :: !failures)
    ["regular", regular; "relaxed", relaxed; "slice", sliced];
  if !failures <> [] then
    failwith ("Indexed reads allocate temporary traversal objects: " ^
              String.concat ", " (List.rev !failures))
