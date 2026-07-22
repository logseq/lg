type test_case = { namespace : string; name : string; body : unit -> unit }

type assertion_failure = {
  context : string list;
  expected : string;
  message : string;
}

let registered_cases = Queue.create ()
let once_fixtures = ref []
let each_fixtures = ref []
let testing_contexts = ref []
let assertion_count = ref 0
let assertion_failures = ref []

let register namespace name body =
  Queue.add { namespace; name; body } registered_cases

let cases () = registered_cases |> Queue.to_seq |> List.of_seq

let clear () =
  Queue.clear registered_cases;
  once_fixtures := [];
  each_fixtures := []

let register_once_fixture namespace fixture =
  let fixtures =
    List.assoc_opt namespace !once_fixtures |> Option.value ~default:[]
  in
  once_fixtures :=
    (namespace, fixtures @ [ fixture ])
    :: List.remove_assoc namespace !once_fixtures

let register_each_fixture namespace fixture =
  let fixtures =
    List.assoc_opt namespace !each_fixtures |> Option.value ~default:[]
  in
  each_fixtures :=
    (namespace, fixtures @ [ fixture ])
    :: List.remove_assoc namespace !each_fixtures

let namespace_once_fixtures namespace =
  List.assoc_opt namespace !once_fixtures |> Option.value ~default:[]

let namespace_each_fixtures namespace =
  List.assoc_opt namespace !each_fixtures |> Option.value ~default:[]

let apply_fixtures fixtures body =
  let wrapped =
    List.fold_right (fun fixture body () -> fixture body) fixtures body
  in
  wrapped ()

let begin_case () =
  testing_contexts := [];
  assertion_count := 0;
  assertion_failures := []

let pass () = incr assertion_count
let finish () = ()
let invoke body = body ()

let exception_message = function
  | Lg_runtime.Runtime_exception.Exception_info (message, _) -> message
  | Failure message | Invalid_argument message -> message
  | exn -> Printexc.to_string exn

let fail expected message =
  incr assertion_count;
  assertion_failures :=
    { context = List.rev !testing_contexts; expected; message }
    :: !assertion_failures

let finish_case () = (!assertion_count, List.rev !assertion_failures)

let with_context context body =
  testing_contexts := context :: !testing_contexts;
  match body () with
  | result ->
      testing_contexts := List.tl !testing_contexts;
      result
  | exception exn ->
      testing_contexts := List.tl !testing_contexts;
      raise exn

let grouped_cases () =
  cases ()
  |> List.fold_left
       (fun groups test_case ->
         match List.assoc_opt test_case.namespace groups with
         | None -> groups @ [ (test_case.namespace, [ test_case ]) ]
         | Some _ ->
             List.map
               (fun (namespace, cases) ->
                 if namespace = test_case.namespace then
                   (namespace, cases @ [ test_case ])
                 else (namespace, cases))
               groups)
       []
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)

let contains_substring source substring =
  let source_length = String.length source in
  let substring_length = String.length substring in
  let rec search index =
    index + substring_length <= source_length
    &&
    (String.sub source index substring_length = substring
    || search (index + 1))
  in
  substring_length = 0 || search 0

let is_performance_case test_case =
  contains_substring test_case.name "performance"
  || contains_substring test_case.name "-perf"

let quick_mode_requested () =
  Sys.getenv_opt "LG_TEST_QUICK" = Some "1"
  || Array.exists (String.equal "--quick-tests") Sys.argv

let selected_groups () =
  if not (quick_mode_requested ()) then grouped_cases ()
  else
    grouped_cases ()
    |> List.filter_map (fun (namespace, namespace_tests) ->
        match List.filter (Fun.negate is_performance_case) namespace_tests with
        | [] -> None
        | selected -> Some (namespace, selected))

let failure_message test_case failure =
  let context =
    match failure.context with
    | [] -> ""
    | contexts -> String.concat " / " contexts ^ ": "
  in
  let message = if failure.message = "" then "" else failure.message ^ ": " in
  test_case.namespace ^ "/" ^ test_case.name ^ ": " ^ context ^ message
  ^ "expected " ^ failure.expected

let run_case test_case =
  begin_case ();
  try
    apply_fixtures (namespace_each_fixtures test_case.namespace) test_case.body;
    let count, failures = finish_case () in
    (count, failures, None)
  with exn ->
    let count, failures = finish_case () in
    (count, failures, Some exn)

let run suite_name =
  let groups = selected_groups () in
  let tests = List.concat_map snd groups in
  let assertions = ref 0 in
  let failures = ref [] in
  let errors = ref [] in
  let run_test test_case =
    let count, test_failures, error = run_case test_case in
    assertions := !assertions + count;
    failures :=
      List.rev_append
        (List.map (failure_message test_case) test_failures)
        !failures;
    match error with
    | None -> ()
    | Some exn ->
        errors :=
          (test_case.namespace ^ "/" ^ test_case.name ^ ": "
         ^ Printexc.to_string exn)
          :: !errors
  in
  List.iter
    (fun (namespace, namespace_tests) ->
      try
        apply_fixtures (namespace_once_fixtures namespace) (fun () ->
            List.iter run_test namespace_tests)
      with exn ->
        errors := (namespace ^ " fixture: " ^ Printexc.to_string exn) :: !errors)
    groups;
  Printf.printf "Testing %s\n" suite_name;
  Printf.printf "Ran %d tests containing %d assertions.\n" (List.length tests)
    !assertions;
  Printf.printf "%d failures, %d errors.\n" (List.length !failures)
    (List.length !errors);
  List.iter (Printf.eprintf "FAIL: %s\n") (List.rev !failures);
  List.iter (Printf.eprintf "ERROR: %s\n") (List.rev !errors);
  if !failures <> [] || !errors <> [] then failwith "CljML tests failed"
