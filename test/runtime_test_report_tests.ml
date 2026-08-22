module Dynamic = Lg_runtime.Runtime_dynamic
module Report = Lg_runtime.Runtime_test_report

let test_report_discards_method_results_at_the_registry_boundary () =
  let called = ref false in
  let reporter = Dynamic.keyword ":app/custom" in
  let event_type = Dynamic.keyword ":pass" in
  let dispatch = Dynamic.vector (Rrbvec.of_list [ reporter; event_type ]) in
  let event = Dynamic.map [ (Dynamic.keyword ":type", event_type) ] in
  Report.register dispatch (fun _event -> called := true);
  let () = Report.report reporter event in
  assert !called

let () = test_report_discards_method_results_at_the_registry_boundary ()
