let expect_invalid operation =
  match operation () with
  | exception Invalid_argument _ -> ()
  | _ -> failwith "transient operation should fail after persistent!"

let test_set_lifecycle () =
  let set = Lg_runtime.Runtime_transient.set_empty () in
  ignore (Lg_runtime.Runtime_transient.set_add set 1);
  assert (Lg_runtime.Runtime_transient.set_count set = 1);
  let values = Lg_runtime.Runtime_transient.set_to_seq set |> List.of_seq in
  assert (values = [ 1 ]);
  expect_invalid (fun () -> Lg_runtime.Runtime_transient.set_add set 2);
  expect_invalid (fun () -> Lg_runtime.Runtime_transient.set_count set)

let test_vector_lifecycle () =
  let vector = Lg_runtime.Runtime_transient.vector_of_list [ 1 ] in
  ignore (Lg_runtime.Runtime_transient.vector_add vector 2);
  ignore (Lg_runtime.Runtime_transient.vector_assoc vector 0 3);
  assert (Lg_runtime.Runtime_transient.vector_count vector = 2);
  assert (Lg_runtime.Runtime_transient.vector_persistent vector = Rrbvec.of_list [ 3; 2 ]);
  expect_invalid (fun () -> Lg_runtime.Runtime_transient.vector_add vector 3);
  expect_invalid (fun () -> Lg_runtime.Runtime_transient.vector_count vector)

let test_map_lifecycle () =
  let map = Lg_runtime.Runtime_transient.map_of_list [ ("a", 1) ] in
  ignore (Lg_runtime.Runtime_transient.map_assoc map "b" 2);
  assert (Lg_runtime.Runtime_transient.map_count map = 2);
  let persistent = Lg_runtime.Runtime_transient.map_persistent map in
  assert (Lg_runtime.Runtime_map.get_option persistent "a" = Some 1);
  assert (Lg_runtime.Runtime_map.get_option persistent "b" = Some 2);
  expect_invalid (fun () -> Lg_runtime.Runtime_transient.map_assoc map "c" 3);
  expect_invalid (fun () -> Lg_runtime.Runtime_transient.map_count map)

let test_large_set_resize_is_stack_safe () =
  let item_count = 20_000 in
  let set = Lg_runtime.Runtime_transient.set_empty () in
  for value = 0 to item_count - 1 do
    ignore (Lg_runtime.Runtime_transient.set_add set value)
  done;
  assert (Lg_runtime.Runtime_transient.set_count set = item_count);
  let values = Lg_runtime.Runtime_transient.set_to_seq set in
  assert (Seq.length values = item_count)

let test_large_map_resize_is_stack_safe () =
  let item_count = 20_000 in
  let map = Lg_runtime.Runtime_transient.map_empty () in
  for key = 0 to item_count - 1 do
    ignore (Lg_runtime.Runtime_transient.map_assoc map key (key + 1))
  done;
  assert (Lg_runtime.Runtime_transient.map_count map = item_count);
  for key = 0 to item_count - 1 do
    assert (Lg_runtime.Runtime_transient.map_get_option map key = Some (key + 1))
  done

let () =
  test_set_lifecycle ();
  test_vector_lifecycle ();
  test_map_lifecycle ();
  test_large_set_resize_is_stack_safe ();
  test_large_map_resize_is_stack_safe ()
