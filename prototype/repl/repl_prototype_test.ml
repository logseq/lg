let fail message = raise (Failure message)

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search index =
    if index + fragment_length > text_length then false
    else if String.sub text index fragment_length = fragment then true
    else search (index + 1)
  in
  fragment_length = 0 || search 0

let expect_ok = function
  | Ok output -> output
  | Error (error : Lg.Compiler.compile_error) ->
      fail ("unexpected REPL error: " ^ error.message)

let expect_error = function
  | Error _ -> ()
  | Ok output -> fail ("expected REPL error, got: " ^ output)

let expect_output output expected =
  if not (contains output expected) then
    fail
      (Printf.sprintf "expected REPL output to contain %S, got:\n%s" expected
         output)

let test_definitions_persist_between_evaluations session =
  Repl_prototype.eval session "(def answer 40)" |> expect_ok |> ignore;
  Repl_prototype.eval session "answer" |> expect_ok
  |> fun output -> expect_output output "40"

let test_functions_compile_and_run_in_later_evaluations session =
  Repl_prototype.eval session "(defn echo [value] value)"
  |> expect_ok |> ignore;
  Repl_prototype.eval session "(echo answer)" |> expect_ok
  |> fun output -> expect_output output "40"

let test_bare_expression_result_is_printed session =
  Repl_prototype.eval session "42" |> expect_ok
  |> fun output -> expect_output output "42"

let test_compile_failure_does_not_advance_session session =
  Repl_prototype.eval session "(def broken missing-value)"
  |> expect_error;
  Repl_prototype.eval session "broken" |> expect_error;
  Repl_prototype.eval session "answer" |> expect_ok
  |> fun output -> expect_output output "40"

let test_redefinition_is_visible_to_new_forms session =
  Repl_prototype.eval session "(def answer 41)" |> expect_ok |> ignore;
  Repl_prototype.eval session "answer" |> expect_ok
  |> fun output -> expect_output output "41"

let test_existing_functions_keep_their_static_binding session =
  Repl_prototype.eval session "(def captured 10)" |> expect_ok |> ignore;
  Repl_prototype.eval session "(defn read-captured [] captured)"
  |> expect_ok |> ignore;
  Repl_prototype.eval session "(def captured 20)" |> expect_ok |> ignore;
  Repl_prototype.eval session "(read-captured)" |> expect_ok
  |> fun output -> expect_output output "10"

let test_precompiled_stdlib_is_available session =
  Repl_prototype.eval session "(+ 40 2)" |> expect_ok
  |> fun output -> expect_output output "42"

let test_macro_definitions_persist_between_evaluations session =
  Repl_prototype.eval session "(defmacro preserve [value] value)"
  |> expect_ok |> ignore;
  Repl_prototype.eval session "(preserve 42)" |> expect_ok
  |> fun output -> expect_output output "42"

let test_runtime_reference_state_persists_between_evaluations session =
  Repl_prototype.eval session "(def counter (atom 1))" |> expect_ok |> ignore;
  Repl_prototype.eval session "(reset! counter 7)" |> expect_ok |> ignore;
  Repl_prototype.eval session "(deref counter)" |> expect_ok
  |> fun output -> expect_output output "7"

let () =
  if Array.length Sys.argv <> 2 then
    fail "expected the precompiled stdlib state path";
  let session =
    Repl_prototype.create_from_stdlib ~state_path:Sys.argv.(1) |> expect_ok
  in
  test_definitions_persist_between_evaluations session;
  test_functions_compile_and_run_in_later_evaluations session;
  test_bare_expression_result_is_printed session;
  test_compile_failure_does_not_advance_session session;
  test_redefinition_is_visible_to_new_forms session;
  test_existing_functions_keep_their_static_binding session;
  test_precompiled_stdlib_is_available session;
  test_macro_definitions_persist_between_evaluations session;
  test_runtime_reference_state_persists_between_evaluations session
