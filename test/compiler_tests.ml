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

let expect_error_value expected = function
  | Ok _ -> failwith "expected compilation error, got successful result"
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

let test_hash_map_constructs_structural_maps () =
  let source =
    {|
(def user (hash-map :name "Ada" :age 36))
(println (str (:name user) ":" (:age user)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "hash_map_constructs_structural_maps" "Ada:36\n" ocaml_source

let test_hash_map_rejects_duplicate_fields () =
  Cljml.Compiler.compile_string {|(def x (hash-map :name "Ada" :name "Grace"))|}
  |> expect_error "duplicate field :name"

let test_hash_map_rejects_odd_key_value_forms () =
  Cljml.Compiler.compile_string {|(def x (hash-map :name "Ada" :age))|}
  |> expect_error "hash-map expects keyword/value pairs"

let test_println_outputs_record_values () =
  let source =
    {|
(def x {:name "Ada", :age 36})
(def y (assoc x :admin? true))
(def z (dissoc y :age))
(println z)
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "println_outputs_record_values" "{:name \"Ada\", :admin? true}\n"
    ocaml_source

let test_println_rejects_unknown_symbols () =
  Cljml.Compiler.compile_string {|(println missing)|}
  |> expect_error "unknown symbol missing"

let test_print_and_println_match_clojure_output () =
  let source =
    {|
(print "a")
(print "b")
(println "c")
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "print_and_println_match_clojure_output" "abc\n" ocaml_source

let test_core_api_nested_calls_maps_and_vectors () =
  let source =
    {|
(def user {:name "Ada", :age 36, :admin? false})
(def ages [36 37 38])
(def next-age (+ (get user :age) 1))
(def updated (assoc user :admin? true))
(def label (str (get updated :name) ":" (get updated :admin?) ":" next-age ":" (count ages)))
(println label)
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
(println (str status ":" (first xs) ":" (nth xs 2)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "core_api_if_and_vector_ops" "ok:1:3\n" ocaml_source

let test_boolean_core_api () =
  let source = {|(println (str (not false) ":" (nil? nil) ":" (some? 1)))|} in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "boolean_core_api" "true:true:true\n" ocaml_source

let test_namespaces_resolve_qualified_and_current_symbols () =
  let source =
    {|
(ns people.core)
(def user {:name "Ada"})
(ns app.main)
(def label (str (get people.core/user :name) "!"))
(println label)
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
(println (str first.core/x ":" x))
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
(println (get p/user :name))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "namespace_require_aliases" "Ada\n" ocaml_source

let test_namespace_require_refer () =
  let source =
    {|
(ns people.core)
(def user {:name "Ada"})
(defn shout [^:string name] (str name "!"))
(ns app.main
  (:require [people.core :refer [user shout]]))
(println (shout (:name user)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "namespace_require_refer" "Ada!\n" ocaml_source

let test_namespace_require_refer_rejects_unknown_symbol () =
  let source =
    {|
(ns people.core)
(def user {:name "Ada"})
(ns app.main
  (:require [people.core :refer [missing]]))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "cannot refer unknown symbol people.core/missing"

let test_keyword_lookup_syntax () =
  let source =
    {|
(def user {:name "Ada", :age 36})
(println (str (:name user) ":" (:age user)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "keyword_lookup_syntax" "Ada:36\n" ocaml_source

let test_typed_empty_vectors () =
  let source =
    {|
(def xs (vector-of :int))
(def ys (conj xs 42))
(println (str (empty? xs) ":" (count ys) ":" (first ys)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_empty_vectors" "true:1:42\n" ocaml_source

let test_vector_of_rejects_unknown_types () =
  Cljml.Compiler.compile_string {|(def xs (vector-of :record))|}
  |> expect_error "unknown vector element type :record"

let test_ocaml_module_require_aliases () =
  let source =
    {|
(ns host.demo
  (:require [ocaml.Stdlib :as std]
            [ocaml.String :as string]))
(def label (str (string/uppercase-ascii "ada") ":" (std/string-of-int 42)))
(println label)
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_module_require_aliases" "ADA:42\n" ocaml_source

let test_ocaml_module_require_refer () =
  let source =
    {|
(ns host.demo
  (:require [ocaml.Stdlib :refer [string-of-int]]
            [ocaml.String :refer [uppercase-ascii]]))
(def label (str (uppercase-ascii "ada") ":" (string-of-int 42)))
(println label)
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_module_require_refer" "ADA:42\n" ocaml_source

let test_typed_function_parameters () =
  let source =
    {|
(defn inc1 [^:int x] (+ x 1))
(defn greet [^:string name] (str "hi " name))
(def mapped (map (fn [^:int x] (+ x 1)) [1 2]))
(println (str (inc1 41) ":" (greet "Ada") ":" (nth mapped 1)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_function_parameters" "42:hi Ada:3\n" ocaml_source

let test_typed_function_parameters_reject_bad_calls () =
  let source =
    {|
(defn inc1 [^:int x] (+ x 1))
(def bad (inc1 "Ada"))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "inc1 called with incompatible arguments"

let test_typed_function_parameters_reject_bad_bodies () =
  Cljml.Compiler.compile_string {|(defn bad [^:string x] (+ x 1))|}
  |> expect_error "expected int arguments for +"

let test_unannotated_function_parameters_infer_from_body () =
  let source =
    {|
(defn inc1 [x] (+ x 1))
(defn flip [flag] (not flag))
(println (str (inc1 41) ":" (flip false)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "unannotated_function_parameters_infer_from_body" "42:true\n"
    ocaml_source

let test_unannotated_function_parameters_reject_bad_int_calls () =
  let source =
    {|
(defn inc1 [x] (+ x 1))
(def bad (inc1 "Ada"))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "inc1 called with incompatible arguments"

let test_unannotated_function_parameters_reject_bad_bool_calls () =
  let source =
    {|
(defn flip [flag] (not flag))
(def bad (flip 1))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "flip called with incompatible arguments"

let test_unannotated_function_parameters_infer_structural_map_fields () =
  let source =
    {|
(def user {:name "Ada", :age 36})
(defn next-age [person] (+ (:age person) 1))
(println (str (next-age user)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "unannotated_function_parameters_infer_structural_map_fields" "37\n"
    ocaml_source

let test_unannotated_function_parameters_reject_missing_structural_map_fields () =
  let source =
    {|
(def user {:name "Ada"})
(defn next-age [person] (+ (get person :age) 1))
(def bad (next-age user))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "next-age called with incompatible arguments"

let test_do_and_multi_form_bodies () =
  let source =
    {|
(defn inc-and-log [^:int x]
  (println (str "input:" x))
  (+ x 1))
(def result
  (let [base 41]
    (println "inside-let")
    (do
      (println "inside-do")
      (inc-and-log base))))
(println (str "result:" result))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "do_and_multi_form_bodies"
    "inside-let\ninside-do\ninput:41\nresult:42\n" ocaml_source

let test_fn_rejects_empty_body () =
  Cljml.Compiler.compile_string {|(def f (fn [x]))|}
  |> expect_error "function body requires at least one form"

let test_vectors_reject_mixed_element_types () =
  Cljml.Compiler.compile_string {|(def xs [1 "two"])|}
  |> expect_error "vector elements must all have the same type"

let test_keyword_values_print_as_keywords () =
  let source =
    {|
(println (str :admin? ":" (pr-str :admin?)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "keyword_values_print_as_keywords" ":admin?::admin?\n"
    ocaml_source

let test_keys_return_keyword_values () =
  let source =
    {|
(def user {:name "Ada", :age 36})
(println (pr-str (keys user)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "keys_return_keyword_values" "[:name :age]\n" ocaml_source

let test_vectors_reject_mixed_keyword_and_string_elements () =
  Cljml.Compiler.compile_string {|(def xs [:name "name"])|}
  |> expect_error "vector elements must all have the same type"

let test_arithmetic_rejects_non_int_arguments () =
  Cljml.Compiler.compile_string {|(def x (+ 1 "two"))|}
  |> expect_error "expected int arguments for +"

let test_arithmetic_core_arities () =
  let source =
    {|
(println (str (+) ":" (*) ":" (+ 1 2 3) ":" (- 5) ":" (- 10 3 2) ":" (/ 8 2 2)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "arithmetic_core_arities" "0:1:6:-5:5:2\n" ocaml_source

let test_integer_division_rejects_unsupported_arities () =
  Cljml.Compiler.compile_string {|(def x (/ 10))|}
  |> expect_error "/ expects at least 2 arguments"

let test_chained_comparisons () =
  let source =
    {|
(println (str (< 1 2 3) ":" (< 1 3 2) ":" (= 1 1 1) ":" (= 1 1 2)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "chained_comparisons" "true:false:true:false\n" ocaml_source

let test_get_rejects_unknown_map_fields () =
  let source = {|(def user {:name "Ada"})(def x (get user :age))|} in
  Cljml.Compiler.compile_string source |> expect_error "unknown field :age"

let test_get_supports_default_values () =
  let source =
    {|
(def user {:name "Ada", :age 36})
(println (str (get user :age 0) ":" (get user :admin? false)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "get_supports_default_values" "36:false\n" ocaml_source

let test_get_rejects_default_type_mismatch_for_known_fields () =
  Cljml.Compiler.compile_string {|(def x (get {:age 36} :age "unknown"))|}
  |> expect_error "get default for :age must be int"

let test_get_supports_vectors () =
  let source =
    {|
(def xs [10 20 30])
(println (str (get xs 1) ":" (get xs 9 99)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "get_supports_vectors" "20:99\n" ocaml_source

let test_get_rejects_vector_default_type_mismatch () =
  Cljml.Compiler.compile_string {|(def x (get [1 2] 9 "missing"))|}
  |> expect_error "get default for vector must match element type"

let test_assoc_supports_multiple_pairs () =
  let source =
    {|
(def user {:name "Ada"})
(def updated (assoc user :age 36 :admin? true))
(println (str (:name updated) ":" (:age updated) ":" (:admin? updated)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "assoc_supports_multiple_pairs" "Ada:36:true\n" ocaml_source

let test_assoc_rejects_odd_key_value_pairs () =
  Cljml.Compiler.compile_string {|(def bad (assoc {:name "Ada"} :age))|}
  |> expect_error "assoc expects map followed by keyword/value pairs"

let test_dissoc_supports_multiple_keys () =
  let source =
    {|
(def user {:name "Ada", :age 36, :admin? true})
(def slim (dissoc user :age :admin?))
(println (str (:name slim) ":" (count slim)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dissoc_supports_multiple_keys" "Ada:1\n" ocaml_source

let test_map_merge_update_and_select_keys () =
  let source =
    {|
(def user {:name "Ada", :age 36})
(def admin {:age 37, :admin? true})
(def merged (merge user admin))
(def updated (update merged :age inc))
(def selected (select-keys updated [:name :admin?]))
(println (str (:name selected) ":" (:admin? selected) ":" (:age updated) ":" (count selected)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "map_merge_update_and_select_keys" "Ada:true:38:2\n" ocaml_source

let test_merge_rejects_incompatible_overlapping_fields () =
  let source = {|(def bad (merge {:age 36} {:age "old"}))|} in
  Cljml.Compiler.compile_string source
  |> expect_error "cannot merge :age as string because it is already int"

let test_update_rejects_type_changes () =
  let source =
    {|
(defn stringify-age [x] (str x))
(def bad (update {:age 36} :age stringify-age))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "cannot update :age as string because it is already int"

let test_update_supports_extra_arguments () =
  let source =
    {|
(def user {:name "Ada", :age 36})
(def older (update user :age + 1))
(println (str (:name older) ":" (:age older)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "update_supports_extra_arguments" "Ada:37\n" ocaml_source

let test_update_rejects_extra_argument_type_mismatch () =
  let source = {|(def bad (update {:age 36} :age + "one"))|} in
  Cljml.Compiler.compile_string source
  |> expect_error "update function arguments do not match field and extra arguments"

let test_select_keys_rejects_unknown_fields () =
  Cljml.Compiler.compile_string {|(def bad (select-keys {:name "Ada"} [:age]))|}
  |> expect_error "cannot select unknown field :age"

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
(println result)
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
(println (str (first mapped) ":" (nth mapped 2) ":" (count filtered) ":" total ":" (first tail) ":" (empty? tail)))
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
(println (str (add10-after-double 4) ":" (identity 7) ":" (always-ok false) ":" (apply + [1 2 3])))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "function_helpers" "18:7:ok:6\n" ocaml_source

let test_set_core_api () =
  let source =
    {|
(def xs (hash-set 1 2 2 3))
(def ys (disj xs 2))
(println (str (contains? xs 2) ":" (contains? ys 2) ":" (count ys)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "set_core_api" "true:false:2\n" ocaml_source

let test_list_core_api () =
  let source =
    {|
(def xs (list 2 3))
(def ys (conj xs 1))
(def zs (cons 0 ys))
(def tail (rest zs))
(println (str (first zs) ":" (nth tail 1) ":" (count zs) ":" (empty? (rest (rest (rest (rest zs)))))))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "list_core_api" "0:2:4:true\n" ocaml_source

let test_sequence_core_api_on_lists () =
  let source =
    {|
(def xs (list 1 2 3))
(def mapped (map (fn [x] (+ x 1)) xs))
(def filtered (filter (fn [x] (> x 2)) mapped))
(def total (reduce (fn [acc x] (+ acc x)) 0 xs))
(println (str (first mapped) ":" (nth mapped 2) ":" (count filtered) ":" total))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sequence_core_api_on_lists" "2:4:2:6\n" ocaml_source

let test_empty_core_api () =
  let source =
    {|
(def xs (empty [1 2]))
(def ys (empty (list 1 2)))
(def zs (empty (hash-set 1 2)))
(def s (empty "Ada"))
(println (str (empty? xs) ":" (empty? ys) ":" (empty? zs) ":" (= s "")))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "empty_core_api" "true:true:true:true\n" ocaml_source

let test_empty_rejects_unsupported_values () =
  Cljml.Compiler.compile_string {|(def x (empty 1))|}
  |> expect_error "empty expects a collection or string"

let test_into_core_api () =
  let source =
    {|
(def xs (into [1] (list 2 3)))
(def ys (into (list-of :int) [1 2 3]))
(def zs (into (hash-set 1) [1 2 2 3]))
(println (str (pr-str xs) ":" (pr-str ys) ":" (pr-str zs)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "into_core_api" "[1 2 3]:(3 2 1):#{1 2 3}\n" ocaml_source

let test_into_rejects_element_type_mismatch () =
  Cljml.Compiler.compile_string {|(def x (into [1] ["two"]))|}
  |> expect_error "into source element type must match target element type"

let test_nth_supports_default_values () =
  let source =
    {|
(def xs [1 2])
(def ys (list 3 4))
(println (str (nth xs 5 99) ":" (nth ys 5 88)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nth_supports_default_values" "99:88\n" ocaml_source

let test_nth_rejects_default_type_mismatch () =
  Cljml.Compiler.compile_string {|(def x (nth [1 2] 5 "missing"))|}
  |> expect_error "nth default must match collection element type"

let test_typed_empty_lists () =
  let source =
    {|
(def xs (list-of :int))
(def ys (cons 42 xs))
(println (str (empty? xs) ":" (count ys) ":" (first ys)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_empty_lists" "true:1:42\n" ocaml_source

let test_lists_reject_mixed_element_types () =
  Cljml.Compiler.compile_string {|(def xs (list 1 "two"))|}
  |> expect_error "list elements must all have the same type"

let test_conj_rejects_list_type_mismatch () =
  Cljml.Compiler.compile_string {|(def xs (conj (list 1) "two"))|}
  |> expect_error "conj value type must match list element type"

let test_collection_positional_helpers () =
  let source =
    {|
(def xs [1 2 3])
(def ys (list 1 2 3))
(def xp (pop xs))
(def yp (pop ys))
(println (str (second xs) ":" (last xs) ":" (peek xs) ":" (count xp) ":" (last xp) ":"
              (second ys) ":" (last ys) ":" (peek ys) ":" (count yp) ":" (first yp)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "collection_positional_helpers" "2:3:3:2:2:2:3:1:2:2\n"
    ocaml_source

let test_peek_rejects_unsupported_collections () =
  Cljml.Compiler.compile_string {|(def x (peek (hash-set 1)))|}
  |> expect_error "peek expects a list or vector"

let test_let_rejects_odd_binding_forms () =
  Cljml.Compiler.compile_string {|(def x (let [a 1 b] a))|}
  |> expect_error "let bindings require an even number of forms"

let test_map_rejects_non_function_argument () =
  Cljml.Compiler.compile_string {|(def xs (map 1 [1 2]))|}
  |> expect_error "map expects a function"

let test_incremental_compilation_preserves_state () =
  let state = Cljml.Compiler.empty_state in
  let state, people_ocaml =
    Cljml.Compiler.compile_chunk state
      {|
(ns people.core)
(def user {:name "Ada", :age 36})
|}
    |> expect_ok
  in
  let _state, app_ocaml =
    Cljml.Compiler.compile_chunk state
      {|
(ns app.main
  (:require [people.core :as p]))
(def updated (assoc p/user :admin? true))
(println (str (:name updated) ":" (:admin? updated) ":" (:age updated)))
|}
    |> expect_ok
  in
  assert_ocaml_runs "incremental_compilation_preserves_state"
    "Ada:true:36\n" (people_ocaml ^ "\n\n" ^ app_ocaml)

let test_incremental_compilation_requires_prior_state () =
  Cljml.Compiler.compile_chunk Cljml.Compiler.empty_state
    {|
(ns app.main
  (:require [people.core :as p]))
(println (:name p/user))
|}
  |> expect_error_value "unknown symbol p/user"

let tests =
  [
    ("records, assoc, and dissoc generate typed OCaml", test_records_assoc_and_dissoc);
    ("assoc rejects changing an existing field type", test_assoc_rejects_type_changes);
    ("dissoc rejects unknown fields", test_dissoc_rejects_unknown_fields);
    ("map literals reject duplicate fields", test_map_rejects_duplicate_fields);
    ("hash-map constructs structural maps", test_hash_map_constructs_structural_maps);
    ("hash-map rejects duplicate fields", test_hash_map_rejects_duplicate_fields);
    ("hash-map rejects odd key value forms", test_hash_map_rejects_odd_key_value_forms);
    ("println outputs record values", test_println_outputs_record_values);
    ("println rejects unknown symbols", test_println_rejects_unknown_symbols);
    ("print and println match Clojure output", test_print_and_println_match_clojure_output);
    ( "core api supports nested calls, maps, and vectors",
      test_core_api_nested_calls_maps_and_vectors );
    ("core api supports if and vector ops", test_core_api_if_and_vector_ops);
    ("boolean core api works", test_boolean_core_api);
    ( "namespaces resolve qualified and current symbols",
      test_namespaces_resolve_qualified_and_current_symbols );
    ( "namespaces prevent unqualified symbol collisions",
      test_namespaces_prevent_unqualified_symbol_collisions );
    ("namespace require aliases work", test_namespace_require_aliases);
    ("namespace require refer works", test_namespace_require_refer);
    ( "namespace require refer rejects unknown symbols",
      test_namespace_require_refer_rejects_unknown_symbol );
    ("keyword lookup syntax works", test_keyword_lookup_syntax);
    ("typed empty vectors work", test_typed_empty_vectors);
    ("vector-of rejects unknown types", test_vector_of_rejects_unknown_types);
    ("ocaml module require aliases work", test_ocaml_module_require_aliases);
    ("ocaml module require refer works", test_ocaml_module_require_refer);
    ("typed function parameters work", test_typed_function_parameters);
    ( "typed function parameters reject bad calls",
      test_typed_function_parameters_reject_bad_calls );
    ( "typed function parameters reject bad bodies",
      test_typed_function_parameters_reject_bad_bodies );
    ( "unannotated function parameters infer from body",
      test_unannotated_function_parameters_infer_from_body );
    ( "unannotated function parameters reject bad int calls",
      test_unannotated_function_parameters_reject_bad_int_calls );
    ( "unannotated function parameters reject bad bool calls",
      test_unannotated_function_parameters_reject_bad_bool_calls );
    ( "unannotated function parameters infer structural map fields",
      test_unannotated_function_parameters_infer_structural_map_fields );
    ( "unannotated function parameters reject missing structural map fields",
      test_unannotated_function_parameters_reject_missing_structural_map_fields );
    ("do and multi-form bodies work", test_do_and_multi_form_bodies);
    ("fn rejects empty body", test_fn_rejects_empty_body);
    ("vectors reject mixed element types", test_vectors_reject_mixed_element_types);
    ("keyword values print as keywords", test_keyword_values_print_as_keywords);
    ("keys return keyword values", test_keys_return_keyword_values);
    ( "vectors reject mixed keyword and string elements",
      test_vectors_reject_mixed_keyword_and_string_elements );
    ("arithmetic rejects non-int arguments", test_arithmetic_rejects_non_int_arguments);
    ("arithmetic core arities work", test_arithmetic_core_arities);
    ( "integer division rejects unsupported arities",
      test_integer_division_rejects_unsupported_arities );
    ("chained comparisons work", test_chained_comparisons);
    ("get rejects unknown map fields", test_get_rejects_unknown_map_fields);
    ("get supports default values", test_get_supports_default_values);
    ( "get rejects default type mismatch for known fields",
      test_get_rejects_default_type_mismatch_for_known_fields );
    ("get supports vectors", test_get_supports_vectors);
    ( "get rejects vector default type mismatch",
      test_get_rejects_vector_default_type_mismatch );
    ("assoc supports multiple pairs", test_assoc_supports_multiple_pairs);
    ("assoc rejects odd key value pairs", test_assoc_rejects_odd_key_value_pairs);
    ("dissoc supports multiple keys", test_dissoc_supports_multiple_keys);
    ("map merge, update, and select-keys work", test_map_merge_update_and_select_keys);
    ( "merge rejects incompatible overlapping fields",
      test_merge_rejects_incompatible_overlapping_fields );
    ("update rejects type changes", test_update_rejects_type_changes);
    ("update supports extra arguments", test_update_supports_extra_arguments);
    ( "update rejects extra argument type mismatch",
      test_update_rejects_extra_argument_type_mismatch );
    ("select-keys rejects unknown fields", test_select_keys_rejects_unknown_fields);
    ("if rejects branch type mismatch", test_if_rejects_branch_type_mismatch);
    ("let, defn, and fn values work", test_let_defn_and_fn_values);
    ("sequence core api works on vectors", test_sequence_core_api_on_vectors);
    ("function helpers work", test_function_helpers);
    ("set core api works", test_set_core_api);
    ("list core api works", test_list_core_api);
    ("sequence core api works on lists", test_sequence_core_api_on_lists);
    ("empty core api works", test_empty_core_api);
    ("empty rejects unsupported values", test_empty_rejects_unsupported_values);
    ("into core api works", test_into_core_api);
    ("into rejects element type mismatch", test_into_rejects_element_type_mismatch);
    ("nth supports default values", test_nth_supports_default_values);
    ("nth rejects default type mismatch", test_nth_rejects_default_type_mismatch);
    ("typed empty lists work", test_typed_empty_lists);
    ("lists reject mixed element types", test_lists_reject_mixed_element_types);
    ("conj rejects list type mismatch", test_conj_rejects_list_type_mismatch);
    ("collection positional helpers work", test_collection_positional_helpers);
    ("peek rejects unsupported collections", test_peek_rejects_unsupported_collections);
    ("let rejects odd binding forms", test_let_rejects_odd_binding_forms);
    ("map rejects non-function argument", test_map_rejects_non_function_argument);
    ( "incremental compilation preserves state",
      test_incremental_compilation_preserves_state );
    ( "incremental compilation requires prior state",
      test_incremental_compilation_requires_prior_state );
  ]

let () =
  List.iter
    (fun (name, run) ->
      try run ()
      with exn ->
        Printf.eprintf "FAILED: %s\n%s\n" name (Printexc.to_string exn);
        exit 1)
    tests
