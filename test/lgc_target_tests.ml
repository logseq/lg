let fail message = raise (Failure message)

let expect_ok : ('a, Lg.Compiler.compile_error) result -> 'a = function
  | Ok value -> value
  | Error error ->
      fail ("unexpected compile error: " ^ error.Lg.Compiler.message)

let expect_error expected (result : ('a, Lg.Compiler.compile_error) result) =
  match result with
  | Error error when error.Lg.Compiler.message = expected -> ()
  | Error error ->
      fail
        (Printf.sprintf "expected error %S, got %S" expected
           error.Lg.Compiler.message)
  | Ok _ -> fail ("expected compile error: " ^ expected)

let contains source expected =
  let source_length = String.length source in
  let expected_length = String.length expected in
  let rec search offset =
    offset + expected_length <= source_length
    && (String.sub source offset expected_length = expected
       || search (offset + 1))
  in
  expected_length = 0 || search 0

let assert_contains source expected =
  if not (contains source expected) then
    fail (Printf.sprintf "expected generated source to contain %S" expected)

let assert_not_contains source unexpected =
  if contains source unexpected then
    fail
      (Printf.sprintf "expected generated source not to contain %S" unexpected)

let compile target source =
  Lg.Compiler.compile_string ~target source |> expect_ok

let test_reader_discard_omits_forms () =
  let generated =
    compile Lg.Target.Native
      {|
#_(def discarded-value missing-symbol)
(def retained-values [1 #_2 3])
|}
  in
  assert_not_contains generated "discarded_value";
  assert_contains generated "retained_values";
  assert_not_contains generated "missing_symbol"

let test_selects_each_target () =
  let source =
    {|
(def environment
  #?(:native "native-value"
     :melange "melange-value"
     :js "jsoo-value"))
|}
  in
  let cases =
    [
      (Lg.Target.Native, "native-value", [ "melange-value"; "jsoo-value" ]);
      (Lg.Target.Melange, "melange-value", [ "native-value"; "jsoo-value" ]);
      (Lg.Target.Js_of_ocaml, "jsoo-value", [ "native-value"; "melange-value" ]);
    ]
  in
  List.iter
    (fun (target, selected, unselected) ->
      let generated = compile target source in
      assert_contains generated selected;
      List.iter (assert_not_contains generated) unselected)
    cases

let test_supports_top_level_nested_and_default_conditionals () =
  let source =
    {|
#?(:native (def platform-value 40)
   :default (def platform-value 0))
(def values ["first" #?(:native "native-nested" :default "default-nested")])
(println (str platform-value ":" (nth values 1)))
|}
  in
  let generated = compile Lg.Target.Native source in
  assert_contains generated "40";
  assert_contains generated "native-nested";
  assert_not_contains generated "default-nested"

let test_does_not_elaborate_unselected_branches () =
  let source =
    {|
(def value
  #?(:native 42
     :melange missing-only-on-melange
     :js missing-only-on-jsoo))
|}
  in
  ignore (compile Lg.Target.Native source)

let test_native_is_the_default_compiler_target () =
  let generated =
    Lg.Compiler.compile_string
      {|(def value #?(:native "native-default" :default "fallback-default"))|}
    |> expect_ok
  in
  assert_contains generated "native-default";
  assert_not_contains generated "fallback-default"

let test_clj_and_cljs_compatibility_features () =
  let source =
    {|
(def environment #?(:clj "clj-value" :cljs "cljs-value"))
|}
  in
  let cases =
    [
      (Lg.Target.Native, "clj-value", "cljs-value");
      (Lg.Target.Melange, "cljs-value", "clj-value");
      (Lg.Target.Js_of_ocaml, "cljs-value", "clj-value");
    ]
  in
  List.iter
    (fun (target, selected, unselected) ->
      let generated = compile target source in
      assert_contains generated selected;
      assert_not_contains generated unselected)
    cases

let test_js_names_js_of_ocaml_target () =
  (match Lg.Target.of_string "js" with
  | Ok Lg.Target.Js_of_ocaml -> ()
  | _ -> fail "expected js to select the js-of-ocaml target");
  if Lg.Target.to_string Lg.Target.Js_of_ocaml <> "js" then
    fail "expected the js-of-ocaml target name to be js";
  let generated =
    compile Lg.Target.Js_of_ocaml
      {|(def environment #?(:js "js-value" :js-of-ocaml "legacy-value"))|}
  in
  assert_contains generated "js-value";
  assert_not_contains generated "legacy-value"

let test_javascript_targets_load_clj_macro_definitions () =
  let source =
    {|
(ns compat.macros)
#?(:clj (defn- emit-value [value] value))
#?(:clj (defmacro from-clj [value] (emit-value value)))
(def answer (from-clj 42))
|}
  in
  [ Lg.Target.Melange; Lg.Target.Js_of_ocaml ]
  |> List.iter (fun target ->
         let generated = compile target source in
         assert_contains generated "42")

let test_rejects_invalid_reader_conditionals () =
  Lg.Compiler.compile_string ~target:Lg.Target.Native
    {|(def value #?(:native 1 :melange))|}
  |> expect_error "reader conditional requires feature/form pairs";
  Lg.Compiler.compile_string ~target:Lg.Target.Native
    {|(def value #?(native 1 :default 2))|}
  |> expect_error "reader conditional feature must be a keyword";
  Lg.Compiler.compile_string ~target:Lg.Target.Native
    {|(def value #?(:native 1 :native 2))|}
  |> expect_error "duplicate reader conditional feature :native"

let test_omits_unmatched_reader_conditionals () =
  let generated =
    compile Lg.Target.Native
      {|
#?(:melange (def melange-only "melange-value"))
(def native-value "native-value")
|}
  in
  assert_contains generated "native-value";
  assert_not_contains generated "melange-value"

let tests =
  [
    ("reader discard omits forms", test_reader_discard_omits_forms);
    ("selects each target", test_selects_each_target);
    ( "supports top-level, nested, and default conditionals",
      test_supports_top_level_nested_and_default_conditionals );
    ( "does not elaborate unselected branches",
      test_does_not_elaborate_unselected_branches );
    ( "native is the default compiler target",
      test_native_is_the_default_compiler_target );
    ( "maps :cljs to JavaScript targets and :clj to Native",
      test_clj_and_cljs_compatibility_features );
    ("names js-of-ocaml target js", test_js_names_js_of_ocaml_target);
    ( "loads :clj macro definitions for JavaScript targets",
      test_javascript_targets_load_clj_macro_definitions );
    ( "omits unmatched reader conditionals",
      test_omits_unmatched_reader_conditionals );
    ( "rejects invalid reader conditionals",
      test_rejects_invalid_reader_conditionals );
  ]

let () =
  List.iter
    (fun (name, run) ->
      try run ()
      with exn ->
        Printf.eprintf "FAILED: %s\n%s\n" name (Printexc.to_string exn);
        exit 1)
    tests
