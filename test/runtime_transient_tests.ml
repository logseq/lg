let expect_invalid operation =
  match operation () with
  | exception Invalid_argument _ -> ()
  | _ -> failwith "transient operation should fail after persistent!"

let test_set_lifecycle () =
  let set = Lg_runtime.Runtime_transient.set_empty () in
  ignore (Lg_runtime.Runtime_transient.set_add set 1);
  let values = Lg_runtime.Runtime_transient.set_to_seq set |> List.of_seq in
  assert (values = [ 1 ]);
  expect_invalid (fun () -> Lg_runtime.Runtime_transient.set_add set 2)

let test_vector_lifecycle () =
  let vector = Lg_runtime.Runtime_transient.vector_of_list [ 1 ] in
  ignore (Lg_runtime.Runtime_transient.vector_add vector 2);
  ignore (Lg_runtime.Runtime_transient.vector_assoc vector 0 3);
  assert (Lg_runtime.Runtime_transient.vector_persistent vector = Rrbvec.of_list [ 3; 2 ]);
  expect_invalid (fun () -> Lg_runtime.Runtime_transient.vector_add vector 3)

let test_map_lifecycle () =
  let map = Lg_runtime.Runtime_transient.map_of_list [ ("a", 1) ] in
  ignore (Lg_runtime.Runtime_transient.map_assoc map "b" 2);
  let persistent = Lg_runtime.Runtime_transient.map_persistent map in
  assert (Lg_runtime.Runtime_map.get_option persistent "a" = Some 1);
  assert (Lg_runtime.Runtime_map.get_option persistent "b" = Some 2);
  expect_invalid (fun () -> Lg_runtime.Runtime_transient.map_assoc map "c" 3)

let () =
  test_set_lifecycle ();
  test_vector_lifecycle ();
  test_map_lifecycle ()
