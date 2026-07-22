let test_registration_order () =
  Lg_test_runtime.clear ();
  Lg_test_runtime.register "alpha" "first" (fun () -> ());
  Lg_test_runtime.register "beta" "second" (fun () -> ());
  Lg_test_runtime.register "alpha" "third" (fun () -> ());
  let names =
    Lg_test_runtime.cases ()
    |> List.map (fun test_case ->
        test_case.Lg_test_runtime.namespace ^ "/" ^ test_case.name)
  in
  Alcotest.(check (list string))
    "registration order"
    [ "alpha/first"; "beta/second"; "alpha/third" ]
    names;
  let groups =
    Lg_test_runtime.grouped_cases ()
    |> List.map (fun (namespace, cases) ->
        ( namespace,
          List.map (fun test_case -> test_case.Lg_test_runtime.name) cases ))
  in
  Alcotest.(check (list (pair string (list string))))
    "namespace groups"
    [ ("alpha", [ "first"; "third" ]); ("beta", [ "second" ]) ]
    groups

let test_assertion_contexts () =
  Lg_test_runtime.begin_case ();
  Lg_test_runtime.with_context "outer" (fun () ->
      Lg_test_runtime.with_context "inner" (fun () ->
          Lg_test_runtime.pass ();
          Lg_test_runtime.fail "(= 1 2)" "numbers differ"));
  let assertion_count, failures = Lg_test_runtime.finish_case () in
  Alcotest.(check int) "assertion count" 2 assertion_count;
  match failures with
  | [ failure ] ->
      Alcotest.(check (list string))
        "nested contexts" [ "outer"; "inner" ] failure.Lg_test_runtime.context;
      Alcotest.(check string) "expected form" "(= 1 2)" failure.expected;
      Alcotest.(check string) "message" "numbers differ" failure.message
  | _ -> Alcotest.fail "expected one assertion failure"

let test_contexts_restore_after_exceptions () =
  Lg_test_runtime.begin_case ();
  (try Lg_test_runtime.with_context "aborted" (fun () -> raise Exit)
   with Exit -> ());
  Lg_test_runtime.with_context "next" (fun () ->
      Lg_test_runtime.fail "false" "");
  let _, failures = Lg_test_runtime.finish_case () in
  match failures with
  | [ failure ] ->
      Alcotest.(check (list string))
        "restored context stack" [ "next" ] failure.Lg_test_runtime.context
  | _ -> Alcotest.fail "expected one assertion failure"

let test_fixtures_compose_in_declaration_order () =
  let events = ref [] in
  let fixture name body =
    events := !events @ [ name ^ "-before" ];
    body ();
    events := !events @ [ name ^ "-after" ]
  in
  Lg_test_runtime.apply_fixtures
    [ fixture "first"; fixture "second" ]
    (fun () -> events := !events @ [ "test" ]);
  Alcotest.(check (list string))
    "fixture order"
    [ "first-before"; "second-before"; "test"; "second-after"; "first-after" ]
    !events

let with_quick_mode value body =
  let variable = "LG_TEST_QUICK" in
  let previous = Sys.getenv_opt variable in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv variable (Option.value previous ~default:""))
    (fun () ->
      Unix.putenv variable value;
      body ())

let test_runtime_quick_mode_skips_only_performance_cases () =
  Lg_test_runtime.clear ();
  let executed = ref [] in
  let register name =
    Lg_test_runtime.register "suite" name (fun () ->
        executed := !executed @ [ name ])
  in
  register "ordinary test";
  register "query performance";
  register "lookup-perf";
  register "performant behavior";
  with_quick_mode "1" (fun () -> Lg_test_runtime.run "quick-selection");
  Alcotest.(check (list string))
    "only explicitly named performance cases are skipped"
    [ "ordinary test"; "performant behavior" ]
    !executed

let test_runtime_default_mode_runs_performance_cases () =
  Lg_test_runtime.clear ();
  let executed = ref [] in
  Lg_test_runtime.register "suite" "query performance" (fun () ->
      executed := [ "query performance" ]);
  with_quick_mode "0" (fun () -> Lg_test_runtime.run "default-selection");
  Alcotest.(check (list string))
    "run-tests remains complete by default"
    [ "query performance" ] !executed

let () =
  Alcotest.run "lg-test-runtime"
    [
      ( "registry",
        [
          Alcotest.test_case "preserves source order" `Quick
            test_registration_order;
        ] );
      ( "assertions",
        [
          Alcotest.test_case "tracks nested contexts" `Quick
            test_assertion_contexts;
          Alcotest.test_case "restores contexts after exceptions" `Quick
            test_contexts_restore_after_exceptions;
        ] );
      ( "fixtures",
        [
          Alcotest.test_case "compose in declaration order" `Quick
            test_fixtures_compose_in_declaration_order;
        ] );
      ( "selection",
        [
          Alcotest.test_case "quick mode skips performance cases" `Quick
            test_runtime_quick_mode_skips_only_performance_cases;
          Alcotest.test_case "default mode runs performance cases" `Quick
            test_runtime_default_mode_runs_performance_cases;
        ] );
    ]
