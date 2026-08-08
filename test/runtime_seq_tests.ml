module Sequence = Lg_runtime.Runtime_seq

let test_concat_does_not_accumulate_empty_suffixes () =
  let sequence = ref Seq.empty in
  for index = 1 to 100_000 do
    sequence := Sequence.concat [ Seq.return index; !sequence ];
    sequence :=
      match !sequence () with
      | Seq.Cons (_, tail) -> tail
      | Seq.Nil -> failwith "concat unexpectedly returned an empty sequence"
  done;
  assert (Sequence.is_empty !sequence)

let test_single_sequence_concat_reuses_its_input () =
  let source = Seq.return 1 in
  assert (Sequence.concat [ source ] == source)

let test_take_returns_empty_for_non_positive_counts () =
  let source = Sequence.repeat "value" in
  assert (Sequence.is_empty (Sequence.take 0 source));
  assert (Sequence.is_empty (Sequence.take (-2) source))

let test_drop_preserves_sequence_for_non_positive_counts () =
  let source = Sequence.of_list [ 1; 2; 3 ] in
  assert (Sequence.to_list (Sequence.drop 0 source) = [ 1; 2; 3 ]);
  assert (Sequence.to_list (Sequence.drop (-2) source) = [ 1; 2; 3 ])

let concat_pipeline_allocations count =
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  let remaining = ref (Sequence.of_list (List.init count Fun.id)) in
  let drop_one sequence =
    match sequence () with
    | Seq.Cons (_, tail) -> tail
    | Seq.Nil -> failwith "concat pipeline unexpectedly ended"
  in
  for _ = 1 to count do
    let source_tail = drop_one !remaining in
    let expanded =
      Sequence.concat [ Sequence.of_list [ 1; 2; 3 ]; source_tail ]
    in
    remaining := drop_one (drop_one (drop_one expanded))
  done;
  Gc.allocated_bytes () -. before

let test_concat_pipeline_allocates_linearly () =
  let small = concat_pipeline_allocations 1_000 in
  let large = concat_pipeline_allocations 2_000 in
  if large > small *. 3. then
    failwith
      (Printf.sprintf
         "concat pipeline allocation grew superlinearly: %.0f -> %.0f bytes"
         small large)

let () =
  test_single_sequence_concat_reuses_its_input ();
  test_take_returns_empty_for_non_positive_counts ();
  test_drop_preserves_sequence_for_non_positive_counts ();
  test_concat_does_not_accumulate_empty_suffixes ();
  test_concat_pipeline_allocates_linearly ()
