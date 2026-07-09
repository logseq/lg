let expect_ok = function
  | Ok value -> value
  | Error (err : Cljml.Compiler.compile_error) ->
      failwith ("expected successful compilation, got: " ^ err.message)

let expect_error expected = function
  | Ok value ->
      failwith ("expected compilation error, got OCaml output:\n" ^ value)
  | Error (err : Cljml.Compiler.compile_error) ->
      if err.message <> expected then
        failwith
          (Printf.sprintf "expected error %S, got %S" expected err.message)

let assert_equal_string expected actual =
  if actual <> expected then
    failwith (Printf.sprintf "expected:\n%s\nactual:\n%s" expected actual)

let write_file path contents =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let rec find_repo_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then failwith "could not find repo root" else find_repo_root parent

let repo_root () = find_repo_root (Sys.getcwd ())

let rrbvec_build_dir () = Filename.concat (repo_root ()) "_build/default/vendor/rrbvec"

let rrbvec_cmi_dir () = Filename.concat (rrbvec_build_dir ()) ".rrbvec.objs/byte"

let assert_ocaml_compiles name ocaml_source =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "cljml-tests" in
  let () =
    if not (Sys.file_exists dir) then Unix.mkdir dir 0o755
  in
  let ml_path = Filename.concat dir (name ^ ".ml") in
  write_file ml_path ocaml_source;
  let cmd =
    Printf.sprintf "ocamlc -I %s -I %s -c %s"
      (Filename.quote (rrbvec_build_dir ()))
      (Filename.quote (rrbvec_cmi_dir ()))
      (Filename.quote (Filename.basename ml_path))
  in
  match Sys.command ("cd " ^ Filename.quote dir ^ " && " ^ cmd) with
  | 0 -> ()
  | code ->
      failwith
        (Printf.sprintf "generated OCaml did not compile, exit code %d:\n%s" code
           ocaml_source)

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let length = in_channel_length ic in
      really_input_string ic length)

let assert_ocaml_runs name expected_output ocaml_source =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "cljml-tests" in
  let () =
    if not (Sys.file_exists dir) then Unix.mkdir dir 0o755
  in
  let ml_path = Filename.concat dir (name ^ ".ml") in
  let exe_path = Filename.concat dir name in
  let output_path = Filename.concat dir (name ^ ".out") in
  write_file ml_path ocaml_source;
  let compile_cmd =
    Printf.sprintf "cd %s && ocamlopt -I %s -I %s -o %s %s %s"
      (Filename.quote dir)
      (Filename.quote (rrbvec_build_dir ()))
      (Filename.quote (rrbvec_cmi_dir ()))
      (Filename.quote (Filename.basename exe_path))
      (Filename.quote (Filename.concat (rrbvec_build_dir ()) "rrbvec.cmxa"))
      (Filename.quote (Filename.basename ml_path))
  in
  let run_cmd =
    Printf.sprintf "%s > %s" (Filename.quote exe_path) (Filename.quote output_path)
  in
  match Sys.command compile_cmd with
  | code when code <> 0 ->
      failwith
        (Printf.sprintf "generated OCaml did not compile, exit code %d:\n%s" code
           ocaml_source)
  | _ -> (
      match Sys.command run_cmd with
      | code when code <> 0 ->
          failwith
            (Printf.sprintf "generated executable failed, exit code %d:\n%s" code
               ocaml_source)
      | _ ->
          let actual = read_file output_path in
          assert_equal_string expected_output actual)

let test_records_assoc_and_dissoc () =
  let source =
    {|
(def x {:name "Ada", :age 36})
(def y (assoc x :admin? true))
(def z (dissoc y :age))
|}
  in
  let expected =
    {|type t1 = {
  name : string;
  age : int;
}

let x : t1 = {
  name = "Ada";
  age = 36;
}

type t2 = {
  name : string;
  age : int;
  admin_ : bool;
}

let y : t2 = {
  name = x.name;
  age = x.age;
  admin_ = true;
}

type t3 = {
  name : string;
  admin_ : bool;
}

let z : t3 = {
  name = y.name;
  admin_ = y.admin_;
}
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_equal_string expected ocaml_source;
  assert_ocaml_compiles "records_assoc_and_dissoc" ocaml_source

let test_assoc_rejects_type_changes () =
  let source =
    {|
(def x {:name "Ada", :age 36})
(def y (assoc x :age "old"))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "cannot assoc :age as string because it is already int"

let test_dissoc_rejects_unknown_fields () =
  let source =
    {|
(def x {:name "Ada", :age 36})
(def y (dissoc x :admin?))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "cannot dissoc unknown field :admin?"

let test_map_rejects_duplicate_fields () =
  let source = {|(def x {:name "Ada", :name "Grace"})|} in
  Cljml.Compiler.compile_string source
  |> expect_error "duplicate field :name"

let test_print_outputs_record_values () =
  let source =
    {|
(def x {:name "Ada", :age 36})
(def y (assoc x :admin? true))
(def z (dissoc y :age))
(print z)
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "print_outputs_record_values" "{:name \"Ada\", :admin? true}\n"
    ocaml_source

let test_print_rejects_unknown_symbols () =
  Cljml.Compiler.compile_string {|(print missing)|}
  |> expect_error "unknown symbol missing"

let test_core_api_nested_calls_maps_and_vectors () =
  let source =
    {|
(def user {:name "Ada", :age 36, :admin? false})
(def ages [36 37 38])
(def next-age (+ (get user :age) 1))
(def updated (assoc user :admin? true))
(def label (str (get updated :name) ":" (get updated :admin?) ":" next-age ":" (count ages)))
(print label)
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "core_api_nested_calls_maps_and_vectors" "Ada:true:37:3\n"
    ocaml_source

let test_core_api_if_and_vector_ops () =
  let source =
    {|
(def xs (conj [1 2] 3))
(def status (if (= (count xs) 3) "ok" "bad"))
(print (str status ":" (first xs) ":" (nth xs 2)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "core_api_if_and_vector_ops" "ok:1:3\n" ocaml_source

let test_boolean_core_api () =
  let source = {|(print (str (not false) ":" (nil? nil) ":" (some? 1)))|} in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "boolean_core_api" "true:true:true\n" ocaml_source

let test_namespaces_resolve_qualified_and_current_symbols () =
  let source =
    {|
(ns people.core)
(def user {:name "Ada"})
(ns app.main)
(def label (str (get people.core/user :name) "!"))
(print label)
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "namespaces_resolve_qualified_and_current_symbols" "Ada!\n"
    ocaml_source

let test_namespaces_prevent_unqualified_symbol_collisions () =
  let source =
    {|
(ns first.core)
(def x 1)
(ns second.core)
(def x 2)
(print (str first.core/x ":" x))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "namespaces_prevent_unqualified_symbol_collisions" "1:2\n"
    ocaml_source

let test_namespace_require_aliases () =
  let source =
    {|
(ns people.core)
(def user {:name "Ada"})
(ns app.main
  (:require [people.core :as p]))
(print (get p/user :name))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "namespace_require_aliases" "Ada\n" ocaml_source

let test_vectors_reject_mixed_element_types () =
  Cljml.Compiler.compile_string {|(def xs [1 "two"])|}
  |> expect_error "vector elements must all have the same type"

let test_arithmetic_rejects_non_int_arguments () =
  Cljml.Compiler.compile_string {|(def x (+ 1 "two"))|}
  |> expect_error "expected int arguments for +"

let test_get_rejects_unknown_map_fields () =
  let source = {|(def user {:name "Ada"})(def x (get user :age))|} in
  Cljml.Compiler.compile_string source |> expect_error "unknown field :age"

let test_if_rejects_branch_type_mismatch () =
  Cljml.Compiler.compile_string {|(def x (if true 1 "one"))|}
  |> expect_error "if branches must have same type"

let test_let_defn_and_fn_values () =
  let source =
    {|
(ns app.functions)
(defn inc1 [x] (+ x 1))
(def add2 (fn [x] (+ x 2)))
(def result (let [base 10
                  bumped (inc1 base)]
              (add2 bumped)))
(print result)
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "let_defn_and_fn_values" "13\n" ocaml_source

let test_sequence_core_api_on_vectors () =
  let source =
    {|
(def xs [1 2 3])
(def mapped (map (fn [x] (+ x 1)) xs))
(def filtered (filter (fn [x] (> x 2)) mapped))
(def total (reduce (fn [acc x] (+ acc x)) 0 xs))
(def tail (rest xs))
(print (str (first mapped) ":" (nth mapped 2) ":" (count filtered) ":" total ":" (first tail) ":" (empty? tail)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sequence_core_api_on_vectors" "2:4:2:6:2:false\n" ocaml_source

let test_function_helpers () =
  let source =
    {|
(def add10 (partial + 10))
(def double (fn [x] (* x 2)))
(def add10-after-double (comp add10 double))
(def always-ok (constantly "ok"))
(print (str (add10-after-double 4) ":" (identity 7) ":" (always-ok false) ":" (apply + [1 2 3])))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "function_helpers" "18:7:ok:6\n" ocaml_source

let test_set_core_api () =
  let source =
    {|
(def xs (hash-set 1 2 2 3))
(def ys (disj xs 2))
(print (str (contains? xs 2) ":" (contains? ys 2) ":" (count ys)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "set_core_api" "true:false:2\n" ocaml_source

let test_let_rejects_odd_binding_forms () =
  Cljml.Compiler.compile_string {|(def x (let [a 1 b] a))|}
  |> expect_error "let bindings require an even number of forms"

let test_map_rejects_non_function_argument () =
  Cljml.Compiler.compile_string {|(def xs (map 1 [1 2]))|}
  |> expect_error "map expects a function"

let tests =
  [
    ("records, assoc, and dissoc generate typed OCaml", test_records_assoc_and_dissoc);
    ("assoc rejects changing an existing field type", test_assoc_rejects_type_changes);
    ("dissoc rejects unknown fields", test_dissoc_rejects_unknown_fields);
    ("map literals reject duplicate fields", test_map_rejects_duplicate_fields);
    ("print outputs record values", test_print_outputs_record_values);
    ("print rejects unknown symbols", test_print_rejects_unknown_symbols);
    ( "core api supports nested calls, maps, and vectors",
      test_core_api_nested_calls_maps_and_vectors );
    ("core api supports if and vector ops", test_core_api_if_and_vector_ops);
    ("boolean core api works", test_boolean_core_api);
    ( "namespaces resolve qualified and current symbols",
      test_namespaces_resolve_qualified_and_current_symbols );
    ( "namespaces prevent unqualified symbol collisions",
      test_namespaces_prevent_unqualified_symbol_collisions );
    ("namespace require aliases work", test_namespace_require_aliases);
    ("vectors reject mixed element types", test_vectors_reject_mixed_element_types);
    ("arithmetic rejects non-int arguments", test_arithmetic_rejects_non_int_arguments);
    ("get rejects unknown map fields", test_get_rejects_unknown_map_fields);
    ("if rejects branch type mismatch", test_if_rejects_branch_type_mismatch);
    ("let, defn, and fn values work", test_let_defn_and_fn_values);
    ("sequence core api works on vectors", test_sequence_core_api_on_vectors);
    ("function helpers work", test_function_helpers);
    ("set core api works", test_set_core_api);
    ("let rejects odd binding forms", test_let_rejects_odd_binding_forms);
    ("map rejects non-function argument", test_map_rejects_non_function_argument);
  ]

let () =
  List.iter
    (fun (name, run) ->
      try run ()
      with exn ->
        Printf.eprintf "FAILED: %s\n%s\n" name (Printexc.to_string exn);
        exit 1)
    tests
