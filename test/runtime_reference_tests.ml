let test_replacement_observer_accepts_a_typed_root_update () =
  let root = Lg_runtime.Runtime_reference.of_value 1 in
  ignore
    (Lg_runtime.Runtime_reference.add_replacement_observer root (fun old_value
                                                                      new_value ->
         assert (old_value = 1);
         assert (new_value = 2);
         true));
  ignore (Lg_runtime.Runtime_reference.replace root 2);
  assert (Lg_runtime.Runtime_reference.deref root = 2)

let test_replacement_observer_failure_restores_last_known_good_value () =
  let root = Lg_runtime.Runtime_reference.of_value 10 in
  ignore
    (Lg_runtime.Runtime_reference.add_replacement_observer root
       (fun _old_value _new_value -> invalid_arg "candidate rejected"));
  (match Lg_runtime.Runtime_reference.replace root 20 with
  | exception Invalid_argument message -> assert (message = "candidate rejected")
  | exception exn -> raise exn
  | _ -> failwith "expected the replacement observer to reject the candidate");
  assert (Lg_runtime.Runtime_reference.deref root = 10)

let test_replacement_observer_can_be_disposed () =
  let root = Lg_runtime.Runtime_reference.of_value 1 in
  let cancel =
    Lg_runtime.Runtime_reference.observe_replacements root
      (fun _old_value _new_value -> false)
  in
  assert (cancel ());
  ignore (Lg_runtime.Runtime_reference.replace root 2);
  assert (Lg_runtime.Runtime_reference.deref root = 2)

let () =
  test_replacement_observer_accepts_a_typed_root_update ();
  test_replacement_observer_failure_restores_last_known_good_value ();
  test_replacement_observer_can_be_disposed ()
