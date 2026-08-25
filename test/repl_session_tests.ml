module Reader = Lg_repl.Reader
module Session = Lg_repl.Session

let fail format = Printf.ksprintf failwith format

let expect_ok = function
  | Ok value -> value
  | Error (error : Lg.Compiler.compile_error) ->
      fail "unexpected REPL error: %s [%s]" error.message error.code

let expect_error = function
  | Error _ -> ()
  | Ok _ -> fail "expected a REPL error"

let expect_equal label expected actual =
  if not (String.equal expected actual) then
    fail "%s: expected %S, got %S" label expected actual

let expect_value ~value ~type_name evaluation =
  match evaluation.Session.outcome with
  | Session.Value actual ->
      expect_equal "rendered value" value actual.rendered;
      expect_equal "value type" type_name actual.type_name
  | Session.Definition _ | Session.Namespace _ | Session.Summary _ ->
      fail "expected a value result"

let expect_definition ~name ~type_name evaluation =
  match evaluation.Session.outcome with
  | Session.Definition actual ->
      expect_equal "definition name" name actual.name;
      expect_equal "definition type" type_name actual.type_name
  | Session.Value _ | Session.Namespace _ | Session.Summary _ ->
      fail "expected a definition result"

let expect_namespace expected evaluation =
  match evaluation.Session.outcome with
  | Session.Namespace actual -> expect_equal "namespace" expected actual
  | Session.Value _ | Session.Definition _ | Session.Summary _ ->
      fail "expected a namespace result"

let test_reader () =
  (match Reader.read " \n ; comment\n" with
  | Reader.Empty -> ()
  | Reader.Complete _ | Reader.Incomplete | Reader.Invalid _ ->
      fail "comments and whitespace should be empty input");
  (match Reader.read "(+ 1" with
  | Reader.Incomplete -> ()
  | Reader.Empty | Reader.Complete _ | Reader.Invalid _ ->
      fail "an unterminated list should be incomplete");
  (match Reader.read "\"unfinished" with
  | Reader.Incomplete -> ()
  | Reader.Empty | Reader.Complete _ | Reader.Invalid _ ->
      fail "an unterminated string should be incomplete");
  (match Reader.read "]" with
  | Reader.Invalid _ -> ()
  | Reader.Empty | Reader.Complete _ | Reader.Incomplete ->
      fail "an unmatched delimiter should be invalid");
  match Reader.read "(+ 1 2)\n(+ 3 4)" with
  | Reader.Complete { source; remaining } ->
      expect_equal "first complete form" "(+ 1 2)" source;
      expect_equal "remaining input" "(+ 3 4)" remaining
  | Reader.Empty | Reader.Incomplete | Reader.Invalid _ ->
      fail "two forms should return the first form and remaining input"

let test_persistent_values session =
  Session.eval session "(+ 40 2)" |> expect_ok
  |> expect_value ~value:"42" ~type_name:"int";
  Session.eval session "\"hello\"" |> expect_ok
  |> expect_value ~value:"\"hello\"" ~type_name:"string";
  Session.eval session "[1 2 3]" |> expect_ok
  |> expect_value ~value:"[1 2 3]" ~type_name:"vector<int>";
  Session.eval session "(def typed-answer 42)" |> expect_ok
  |> expect_definition ~name:"typed-answer" ~type_name:"int";
  Session.eval session "typed-answer" |> expect_ok
  |> expect_value ~value:"42" ~type_name:"int"

let test_macros_and_runtime_state session =
  Session.eval session "(defmacro preserve [value] value)"
  |> expect_ok |> ignore;
  Session.eval session "(preserve 8)" |> expect_ok
  |> expect_value ~value:"8" ~type_name:"int";
  Session.eval session "(def repl-counter (atom 1))" |> expect_ok |> ignore;
  Session.eval session "(reset! repl-counter 7)" |> expect_ok
  |> expect_value ~value:"7" ~type_name:"int";
  Session.eval session "(deref repl-counter)" |> expect_ok
  |> expect_value ~value:"7" ~type_name:"int"

let test_namespace_switching session =
  Session.eval session "(ns repl.alpha)" |> expect_ok
  |> expect_namespace "repl.alpha";
  expect_equal "current namespace" "repl.alpha" (Session.namespace session);
  expect_equal "namespace prompt" "repl.alpha=> " (Session.prompt session);
  Session.eval session "(def scoped-value 10)" |> expect_ok |> ignore;
  Session.eval session "(ns repl.beta)" |> expect_ok
  |> expect_namespace "repl.beta";
  Session.eval session "scoped-value" |> expect_error;
  Session.eval session "(def scoped-value 20)" |> expect_ok |> ignore;
  Session.eval session "scoped-value" |> expect_ok
  |> expect_value ~value:"20" ~type_name:"int";
  Session.eval session "(ns repl.alpha)" |> expect_ok
  |> expect_namespace "repl.alpha";
  Session.eval session "scoped-value" |> expect_ok
  |> expect_value ~value:"10" ~type_name:"int";
  Session.eval session
    "(ns repl.invalid (:require [missing.namespace :as missing]))"
  |> expect_error;
  expect_equal "namespace after failed switch" "repl.alpha"
    (Session.namespace session)

let test_static_type_queries_do_not_execute session =
  Session.type_of session "(+ 1 2)" |> expect_ok
  |> expect_equal "expression type" "int";
  Session.type_of session "[1 2]" |> expect_ok
  |> expect_equal "collection type" "vector<int>";
  Session.type_of session "(reset! user/repl-counter 99)" |> expect_ok
  |> expect_equal "effectful expression type" "int";
  Session.eval session "(deref user/repl-counter)" |> expect_ok
  |> expect_value ~value:"7" ~type_name:"int";
  Session.type_of session "missing-value" |> expect_error;
  expect_equal "namespace after failed type query" "repl.alpha"
    (Session.namespace session)

let test_compile_failure_does_not_advance_state session =
  Session.eval session "(def broken missing-value)" |> expect_error;
  Session.eval session "broken" |> expect_error;
  Session.eval session "scoped-value" |> expect_ok
  |> expect_value ~value:"10" ~type_name:"int"

let test_runtime_failure_does_not_terminate_session session =
  Session.eval session "(throw (ex-info \"boom\" {}))" |> expect_error;
  Session.eval session "(+ scoped-value 1)" |> expect_ok
  |> expect_value ~value:"11" ~type_name:"int"

let () =
  if Array.length Sys.argv <> 2 then
    fail "expected the precompiled stdlib state path";
  test_reader ();
  let session =
    Session.create_from_stdlib ~state_path:Sys.argv.(1) |> expect_ok
  in
  expect_equal "initial namespace" "user" (Session.namespace session);
  expect_equal "initial prompt" "user=> " (Session.prompt session);
  test_persistent_values session;
  test_macros_and_runtime_state session;
  test_namespace_switching session;
  test_static_type_queries_do_not_execute session;
  test_compile_failure_does_not_advance_state session;
  test_runtime_failure_does_not_terminate_session session
