module Hot_reload = Hot_reload_prototype

let fail format = Printf.ksprintf failwith format

let expect_equal label expected actual =
  if not (String.equal expected actual) then
    fail "%s: expected %S, got %S" label expected actual

let expect_int label expected actual =
  if expected <> actual then fail "%s: expected %d, got %d" label expected actual

let expect_ok = function
  | Ok value -> value
  | Error error ->
      fail "unexpected reload error at %d:%d: %s" error.Hot_reload.line
        error.column error.message

let expect_error (result : ('a, Hot_reload.reload_error) result) =
  match result with
  | Error error -> error
  | Ok _ -> fail "expected an error"

let create source = Hot_reload.create ~source |> expect_ok

let test_successful_reload_preserves_model_and_updates_existing_caller () =
  let session = create "Count: {count}" in
  Hot_reload.increment session;
  let caller () = Hot_reload.render session in
  expect_equal "view before reload" "Count: 1" (caller ());
  Hot_reload.reload session ~source:"Total: {count}" |> expect_ok |> ignore;
  expect_equal "existing caller after reload" "Total: 1" (caller ());
  expect_int "generation after reload" 1 (Hot_reload.generation session);
  expect_equal "committed source" "Total: {count}" (Hot_reload.source session)

let test_invalid_reload_keeps_last_good_view () =
  let session = create "Count: {count}" in
  Hot_reload.increment session;
  Hot_reload.reload session ~source:"Total: {count}" |> expect_ok |> ignore;
  let error =
    Hot_reload.reload session ~source:"Broken: {missing}" |> expect_error
  in
  expect_int "diagnostic line" 1 error.line;
  expect_int "diagnostic column" 9 error.column;
  expect_equal "last-good view" "Total: 1" (Hot_reload.render session);
  expect_int "generation after failed reload" 1 (Hot_reload.generation session);
  expect_equal "source after failed reload" "Total: {count}"
    (Hot_reload.source session)

let test_unchanged_source_does_not_publish_a_generation () =
  let session = create "Count: {count}" in
  (match Hot_reload.reload session ~source:"Count: {count}" |> expect_ok with
  | Hot_reload.Unchanged -> ()
  | Hot_reload.Reloaded -> fail "unchanged source published a reload");
  expect_int "unchanged generation" 0 (Hot_reload.generation session)

let test_source_must_reference_the_typed_model () =
  let error = Hot_reload.create ~source:"Static text" |> expect_error in
  expect_int "missing binding line" 1 error.line;
  expect_int "missing binding column" 1 error.column;
  expect_equal "missing binding diagnostic"
    "the prototype view must reference {count}" error.message

let test_oversized_source_is_rejected_without_replacing_the_view () =
  let session = create "Count: {count}" in
  let oversized = String.make 257 'x' ^ "{count}" in
  let error = Hot_reload.reload session ~source:oversized |> expect_error in
  expect_equal "oversized diagnostic" "view source exceeds 256 bytes"
    error.message;
  expect_equal "view after oversized reload" "Count: 0"
    (Hot_reload.render session);
  expect_int "generation after oversized reload" 0
    (Hot_reload.generation session)

let () =
  test_successful_reload_preserves_model_and_updates_existing_caller ();
  test_invalid_reload_keeps_last_good_view ();
  test_unchanged_source_does_not_publish_a_generation ();
  test_source_must_reference_the_typed_model ();
  test_oversized_source_is_rejected_without_replacing_the_view ()
