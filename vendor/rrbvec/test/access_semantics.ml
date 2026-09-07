let check vector expected =

  assert (Rrbvec.length vector = Array.length expected);
  Array.iteri (fun i value -> assert (Rrbvec.nth vector i == value)) expected;
  List.iter (fun i ->
    assert (Rrbvec.nth_opt vector i = None);
    let rejected = try ignore (Rrbvec.nth vector i); false
      with Invalid_argument _ -> true in
    assert rejected) [-1; Array.length expected; max_int]

let () =
  List.iter (fun n ->
    let values = Array.init n (fun i -> ref i) in
    let original = Rrbvec.of_array values in
    check original values;
    let extra = Array.init 71 (fun i -> ref (n + i)) in
    let joined = Rrbvec.append original (Rrbvec.of_array extra) in
    let joined_values = Array.append values extra in
    check joined joined_values;
    let front = ref (-1) in
    check (Rrbvec.push_front joined front) (Array.append [|front|] joined_values);
    let sliced = Option.get (Rrbvec.subvec joined 17 (n + 53)) in
    check sliced (Array.sub joined_values 17 (n + 36));
    List.iter (fun index ->
      let replacement = ref (-2) in
      let updated = Rrbvec.set joined index replacement in
      let expected = Array.copy joined_values in
      expected.(index) <- replacement;
      check updated expected;
      check joined joined_values) [0; (n + 71) / 2; n + 70];
    let last, popped = Option.get (Rrbvec.pop_back joined) in
    assert (last == joined_values.(n + 70));
    check popped (Array.sub joined_values 0 (n + 70));
    check original values)
    [0; 1; 31; 32; 33; 1023; 1024; 1025; 6001; 32767; 32768; 32769];
  let random = Random.State.make [|735; 2026|] in
  let current = ref Rrbvec.empty in
  let expected = ref [||] in
  let snapshots = ref [] in
  for step = 0 to 599 do
    let next = Array.init (1 + Random.State.int random 90) (fun i -> ref (step + i)) in
    if step mod 3 = 0 then begin
      current := Rrbvec.append (Rrbvec.of_array next) !current;
      expected := Array.append next !expected
    end else begin
      current := Rrbvec.append !current (Rrbvec.of_array next);
      expected := Array.append !expected next
    end;
    if step mod 7 = 0 then begin
      let start = Random.State.int random (Array.length !expected / 2 + 1) in
      let stop = Array.length !expected - Random.State.int random 10 in
      current := Option.get (Rrbvec.subvec !current start stop);
      expected := Array.sub !expected start (stop - start)
    end;
    check !current !expected;
    if step mod 50 = 0 then snapshots := (!current, !expected) :: !snapshots
  done;
  List.iter (fun (vector, values) -> check vector values) !snapshots;
  print_endline "Rrbvec indexed access preserves values, identity, bounds and snapshots"
