let failure_message failure =
  let context =
    match failure.Lg_test_runtime.context with
    | [] -> ""
    | contexts -> String.concat " / " contexts ^ ": "
  in
  let message = if failure.message = "" then "" else failure.message ^ ": " in
  context ^ message ^ "expected " ^ failure.expected

let run_case (test_case : Lg_test_runtime.test_case) () =
  Lg_test_runtime.begin_case ();
  Lg_test_runtime.apply_fixtures
    (Lg_test_runtime.namespace_each_fixtures test_case.namespace)
    test_case.body;
  let _, failures = Lg_test_runtime.finish_case () in
  match failures with
  | [] -> ()
  | failures ->
      failures |> List.map failure_message |> String.concat "\n"
      |> Alcotest.fail

let speed_level (test_case : Lg_test_runtime.test_case) =
  if Lg_test_runtime.is_performance_case test_case then `Slow
  else `Quick

let run suite_name =
  let groups =
    Lg_test_runtime.grouped_cases ()
    |> List.map (fun (namespace, cases) ->
        let once_fixtures = Lg_test_runtime.namespace_once_fixtures namespace in
        let test_cases =
          match once_fixtures with
          | [] ->
              List.map
                (fun test_case ->
                  Alcotest.test_case test_case.Lg_test_runtime.name
                    (speed_level test_case)
                    (run_case test_case))
                cases
          | fixtures ->
              [
                Alcotest.test_case namespace `Quick (fun () ->
                    Lg_test_runtime.apply_fixtures fixtures (fun () ->
                        List.iter
                          (fun (test_case : Lg_test_runtime.test_case) ->
                            run_case test_case ())
                          cases));
              ]
        in
        (namespace, test_cases))
  in
  Alcotest.run suite_name groups
