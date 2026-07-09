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

let typecheck_items source =
  match Cljml.Lexer.tokenize source with
  | Error (err : Cljml.Error.t) ->
      failwith ("expected successful lexing, got: " ^ err.message)
  | Ok tokens -> (
      match Cljml.Parser.parse tokens with
      | Error (err : Cljml.Error.t) ->
          failwith ("expected successful parsing, got: " ^ err.message)
      | Ok forms -> Cljml.Typecheck.compile_forms forms |> expect_ok)

let expect_structured_value_expression source =
  let rec find_value_expression = function
    | [] -> None
    | Cljml.Types.Value_binding { expression; _ } :: _ -> Some expression
    | Cljml.Types.Group items :: rest -> (
        match find_value_expression items with
        | Some _ as expression -> expression
        | None -> find_value_expression rest)
    | _ :: rest -> find_value_expression rest
  in
  match typecheck_items source |> find_value_expression with
  | Some (Cljml.Ocaml_ir.Raw _) ->
      failwith "expected a structured OCaml IR expression, got Raw"
  | Some _ -> ()
  | None -> failwith "expected a value binding"

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

let cljml_build_dir () = Filename.concat (repo_root ()) "_build/default/src"

let cljml_byte_cmi_dir () = Filename.concat (cljml_build_dir ()) ".cljml.objs/byte"

let cljml_native_cmi_dir () = Filename.concat (cljml_build_dir ()) ".cljml.objs/native"

let cljml_cmxa () = Filename.concat (cljml_build_dir ()) "cljml.cmxa"

let rrbvec_cmxa () = Filename.concat (rrbvec_build_dir ()) "rrbvec.cmxa"

let assert_ocaml_compiles name ocaml_source =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "cljml-tests" in
  let () =
    if not (Sys.file_exists dir) then Unix.mkdir dir 0o755
  in
  let ml_path = Filename.concat dir (name ^ ".ml") in
  write_file ml_path ocaml_source;
  let cmd =
    Printf.sprintf "ocamlc -I %s -I %s -I %s -I %s -c %s"
      (Filename.quote (rrbvec_build_dir ()))
      (Filename.quote (rrbvec_cmi_dir ()))
      (Filename.quote (cljml_build_dir ()))
      (Filename.quote (cljml_byte_cmi_dir ()))
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
    Printf.sprintf "cd %s && ocamlopt -I %s -I %s -I %s -I %s -I %s -o %s %s %s %s"
      (Filename.quote dir)
      (Filename.quote (rrbvec_build_dir ()))
      (Filename.quote (rrbvec_cmi_dir ()))
      (Filename.quote (cljml_build_dir ()))
      (Filename.quote (cljml_byte_cmi_dir ()))
      (Filename.quote (cljml_native_cmi_dir ()))
      (Filename.quote (Filename.basename exe_path))
      (Filename.quote (rrbvec_cmxa ()))
      (Filename.quote (cljml_cmxa ()))
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

let test_not_uses_static_clojure_truthiness () =
  let source =
    {|(println (str (not nil) ":" (not false) ":" (not 0) ":" (not "Ada") ":" (not [1])))|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "not_uses_static_clojure_truthiness" "true:true:false:false:false\n"
    ocaml_source

let test_type_predicates () =
  let source =
    {|
(println
  (str (int? 1) ":" (string? "Ada") ":" (keyword? :name) ":" (boolean? true) ":"
       (vector? [1]) ":" (list? (list 1)) ":" (set? (hash-set 1)) ":" (map? {:name "Ada"}) ":"
       (seq? (list 1)) ":" (seq? [1]) ":" (vector? (list 1)) ":" (map? [1])))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "type_predicates"
    "true:true:true:true:true:true:true:true:true:false:false:false\n" ocaml_source

let test_type_predicates_reject_wrong_arity () =
  Cljml.Compiler.compile_string {|(def x (vector? [1] [2]))|}
  |> expect_error "vector? expects 1 arguments"

let test_subs_core_api () =
  let source =
    {|
(println (str (subs "clojure" 3) ":" (subs "clojure" 1 4)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "subs_core_api" "jure:loj\n" ocaml_source

let test_subs_rejects_non_string_sources () =
  Cljml.Compiler.compile_string {|(def x (subs 123 1))|}
  |> expect_error "subs expects a string"

let test_subs_rejects_non_int_indexes () =
  Cljml.Compiler.compile_string {|(def x (subs "abc" "1"))|}
  |> expect_error "subs indexes must be int"

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

let test_ocaml_keyword_names_are_munged () =
  let source =
    {|
(def type 1)
(def module 2)
(defn bump [match] (+ match 1))
(def record {:type "person", :module "core"})
(println
  (let [object (bump type)]
    (str object ":" module ":" (:type record) ":" (:module record))))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_keyword_names_are_munged" "2:2:person:core\n" ocaml_source

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
(defn key-label [^:keyword key] (str key "!"))
(def mapped (map (fn [^:int x] (+ x 1)) [1 2]))
(println (str (inc1 41) ":" (greet "Ada") ":" (key-label :admin?) ":" (nth mapped 1)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_function_parameters" "42:hi Ada::admin?!:3\n" ocaml_source

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

let test_static_protocols_dispatch_by_receiver_type () =
  let source =
    {|
(defprotocol Labelled
  (label [x] :string))
(extend-type :int
  Labelled
  (label [x] (str "int:" x)))
(extend-type :string
  Labelled
  (label [x] (str "str:" x)))
(println (str (label 7) ":" (label "Ada")))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "static_protocols_dispatch_by_receiver_type"
    "int:7:str:Ada\n" ocaml_source

let test_static_protocols_reject_missing_implementation () =
  let source =
    {|
(defprotocol Labelled
  (label [x] :string))
(extend-type :int
  Labelled
  (label [x] (str "int:" x)))
(def bad (label true))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "no protocol implementation for label and bool"

let test_static_protocols_reject_return_type_mismatch () =
  let source =
    {|
(defprotocol Labelled
  (label [x] :string))
(extend-type :int
  Labelled
  (label [x] (+ x 1)))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "protocol method label must return string"

let test_static_protocols_work_through_namespace_aliases () =
  let source =
    {|
(ns labels.core)
(defprotocol Labelled
  (label [x] :string))
(extend-type :int
  Labelled
  (label [x] (str "int:" x)))
(ns app.main
  (:require [labels.core :as labels]))
(println (labels/label 9))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "static_protocols_work_through_namespace_aliases" "int:9\n"
    ocaml_source

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

let test_vals_return_homogeneous_values () =
  let source =
    {|
(def counts {:a 1, :b 2})
(def more-counts (assoc counts :c 3))
(println (str (pr-str (vals counts)) ":" (pr-str (vals more-counts)) ":"
              (pr-str (vals (assoc {:x 10} :y 20)))))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "vals_return_homogeneous_values" "[1 2]:[1 2 3]:[10 20]\n"
    ocaml_source

let test_vals_rejects_heterogeneous_values () =
  Cljml.Compiler.compile_string {|(def xs (vals {:name "Ada", :age 36}))|}
  |> expect_error "vals requires all map values to have the same type"

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

let test_not_equal_core_api () =
  let source =
    {|
(println (str (not= 1 2) ":" (not= "Ada" "Ada") ":" (not= :name :age) ":"
              (not= true true false) ":" (not= 1)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "not_equal_core_api" "true:false:true:true:false\n" ocaml_source

let test_not_equal_rejects_mixed_types () =
  Cljml.Compiler.compile_string {|(def x (not= 1 "1"))|}
  |> expect_error "not= arguments must have the same type"

let test_collection_equality_core_api () =
  let source =
    {|
(def ada {:name "Ada", :age 36})
(def ada2 {:name "Ada", :age 36})
(def grace {:name "Grace", :age 36})
(println (str (= [1 2] [1 2]) ":" (= (list 1 2) (list 2 1)) ":"
              (= (hash-set 2 1) (hash-set 1 2)) ":" (not= [1 2] [2 1]) ":"
              (= ada ada2) ":" (not= ada grace)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "collection_equality_core_api" "true:false:true:true:true:true\n" ocaml_source

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

let test_assoc_supports_vector_indexes () =
  let source =
    {|
(def xs [1 2 3])
(def ys (assoc xs 0 10 2 30))
(println (pr-str ys))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "assoc_supports_vector_indexes" "[10 2 30]\n" ocaml_source

let test_assoc_rejects_vector_value_type_mismatch () =
  Cljml.Compiler.compile_string {|(def x (assoc [1 2] 0 "one"))|}
  |> expect_error "assoc vector value must match element type"

let test_assoc_rejects_vector_non_int_indexes () =
  Cljml.Compiler.compile_string {|(def x (assoc [1 2] "0" 9))|}
  |> expect_error "assoc vector index must be int"

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

let test_update_supports_vector_indexes () =
  let source =
    {|
(def xs [1 2 3])
(def ys (update xs 1 + 40))
(println (pr-str ys))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "update_supports_vector_indexes" "[1 42 3]\n" ocaml_source

let test_update_rejects_vector_index_type_mismatch () =
  Cljml.Compiler.compile_string {|(def x (update [1 2] "0" inc))|}
  |> expect_error "update vector index must be int"

let test_select_keys_rejects_unknown_fields () =
  Cljml.Compiler.compile_string {|(def bad (select-keys {:name "Ada"} [:age]))|}
  |> expect_error "cannot select unknown field :age"

let test_contains_supports_vector_indexes () =
  let source =
    {|
(def xs [1 2])
(println (str (contains? xs 0) ":" (contains? xs 2) ":" (contains? xs -1)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "contains_supports_vector_indexes" "true:false:false\n" ocaml_source

let test_contains_rejects_vector_non_int_indexes () =
  Cljml.Compiler.compile_string {|(def x (contains? [1 2] "0"))|}
  |> expect_error "contains? vector index must be int"

let test_if_rejects_branch_type_mismatch () =
  Cljml.Compiler.compile_string {|(def x (if true 1 "one"))|}
  |> expect_error "if branches must have same type"

let test_conditional_forms_work () =
  let source =
    {|
(def status (if-not false "open" "closed"))
(def label
  (cond
    false "bad"
    (= status "open") "ready"
    :else "unknown"))
(when (= label "ready")
  (println "when-fired"))
(println (str status ":" label))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "conditional_forms_work" "when-fired\nopen:ready\n" ocaml_source

let test_if_not_rejects_branch_type_mismatch () =
  Cljml.Compiler.compile_string {|(def x (if-not true 1 "one"))|}
  |> expect_error "if-not branches must have same type"

let test_cond_rejects_missing_else () =
  Cljml.Compiler.compile_string {|(def x (cond false 1))|}
  |> expect_error "cond requires an :else branch"

let test_cond_rejects_branch_type_mismatch () =
  Cljml.Compiler.compile_string {|(def x (cond false 1 :else "one"))|}
  |> expect_error "cond branches must have same type"

let test_cond_rejects_non_bool_tests () =
  Cljml.Compiler.compile_string {|(def x (cond 1 "one" :else "fallback"))|}
  |> expect_error "cond tests must be bool"

let test_when_rejects_value_body () =
  Cljml.Compiler.compile_string {|(def x (when true 1))|}
  |> expect_error "when body must be unit or nil"

let test_conditional_forms_infer_bool_params () =
  let source =
    {|
(defn status [flag]
  (if-not flag "closed" "open"))
(def bad (status 1))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "status called with incompatible arguments"

let test_batched_core_functions_work () =
  let source =
    {|
(def xs [1 2 3])
(def ys (list 1 2 3))
(def user {:name "Ada"})
(def f (fn [x] x))
(println
  (str (zero? 0) ":" (pos? 3) ":" (neg? -1) ":" (even? 4) ":" (odd? 5) ":"
       (number? 1) ":" (number? "1") ":"
       (max 1 5 3) ":" (min 1 -2 3) ":" (quot 7 2) ":" (rem 7 2) ":" (mod -1 5) ":"
       (bit-and 7 3 1) ":" (bit-or 4 1 2) ":" (bit-xor 7 3) ":" (bit-not 0) ":"
       (bit-shift-left 1 3) ":" (bit-shift-right 8 1) ":"
       (fn? f) ":" (fn? 1) ":" (coll? xs) ":" (coll? "x") ":"
       (associative? user) ":" (associative? ys) ":" (indexed? xs) ":" (indexed? user) ":"
       (seqable? "abc") ":" (seqable? 1) ":" (counted? user) ":" (counted? f)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "batched_core_functions_work"
    "true:true:true:true:true:true:false:5:-2:3:1:4:1:7:4:-1:8:4:true:false:true:false:true:false:true:false:true:false:true:false\n"
    ocaml_source

let test_batched_core_functions_reject_non_int_arguments () =
  Cljml.Compiler.compile_string {|(def x (zero? "0"))|}
  |> expect_error "expected int arguments for zero?"

let test_batched_core_functions_reject_bad_arities () =
  Cljml.Compiler.compile_string {|(def x (quot 1))|}
  |> expect_error "quot expects 2 arguments"

let test_batched_core_functions_infer_int_params () =
  let source =
    {|
(defn shifted [x] (bit-shift-left x 1))
(def bad (shifted "1"))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "shifted called with incompatible arguments"

let test_batched_numeric_scalar_core_functions_work () =
  let source =
    {|
(println
  (str (integer? 1) ":" (integer? "1") ":"
       (nat-int? 0) ":" (nat-int? -1) ":" (nat-int? "0") ":"
       (pos-int? 1) ":" (pos-int? 0) ":"
       (neg-int? -1) ":" (neg-int? 0) ":"
       (boolean true) ":" (boolean false) ":" (boolean nil) ":" (boolean "x") ":"
       (bit-set 0 2) ":" (bit-clear 7 1) ":" (bit-flip 4 2) ":"
       (bit-test 4 2) ":" (bit-test 4 1) ":" (bit-shift-right-zero-fill -1 1) ":"
       (unchecked-add 1 2) ":" (unchecked-add-int 1 2) ":"
       (unchecked-subtract 5 3) ":" (unchecked-subtract-int 5 3) ":"
       (unchecked-multiply 3 4) ":" (unchecked-multiply-int 3 4) ":"
       (unchecked-divide-int 7 2) ":" (unchecked-remainder-int 7 2) ":"
       (unchecked-inc 4) ":" (unchecked-inc-int 4) ":"
       (unchecked-dec 4) ":" (unchecked-dec-int 4) ":"
       (unchecked-negate 4) ":" (unchecked-negate-int 4) ":"
       (name :user/name) ":" (name "Ada") ":" (keyword "admin?") ":" (keyword :ready)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "batched_numeric_scalar_core_functions_work"
    "true:false:true:false:false:true:false:true:false:true:false:false:true:4:5:0:true:false:4611686018427387903:3:3:2:2:12:12:3:1:5:5:3:3:-4:-4:name:Ada::admin?::ready\n"
    ocaml_source

let test_batched_numeric_scalar_core_functions_reject_non_int_bit_args () =
  Cljml.Compiler.compile_string {|(def x (bit-set 1 "2"))|}
  |> expect_error "expected int arguments for bit-set"

let test_batched_numeric_scalar_core_functions_reject_unchecked_arity () =
  Cljml.Compiler.compile_string {|(def x (unchecked-add 1))|}
  |> expect_error "unchecked-add expects 2 arguments"

let test_batched_numeric_scalar_core_functions_reject_bad_name_arg () =
  Cljml.Compiler.compile_string {|(def x (name 1))|}
  |> expect_error "name expects keyword, string, or symbol"

let test_batched_numeric_scalar_core_functions_infer_int_params () =
  let source =
    {|
(defn clear-second [x] (bit-clear x 1))
(def bad (clear-second "7"))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "clear-second called with incompatible arguments"

let test_clojure_string_namespace_batch_works () =
  let source =
    {|
(ns app.strings
  (:require [clojure.string :as str]))
(println
  (str/join "|"
    [(str/upper-case "ada")
     (str/lower-case "ADA")
     (str/capitalize "aDA")
     (str/reverse "abc")
     (str/trim "  hi  ")
     (str/triml "  left")
     (str/trimr "right  ")
     (str/trim-newline "line\n")
     (str/replace "banana" "na" "NA")
     (str/replace-first "banana" "na" "NA")
     (str/re-quote-replacement "$1")]))
(println
  (str (str/blank? "  ") ":" (str/includes? "clojure" "oj") ":"
       (str/starts-with? "clojure" "clo") ":" (str/ends-with? "clojure" "ure") ":"
       (str/index-of "banana" "na") ":" (str/last-index-of "banana" "na") ":"
       (pr-str (str/split "a,b,c" ",")) ":" (pr-str (str/split-lines "a\nb"))))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_string_namespace_batch_works"
    "ADA|ada|Ada|cba|hi|left|right|line|baNANA|baNAna|$1\ntrue:true:true:true:2:4:[\"a\" \"b\" \"c\"]:[\"a\" \"b\"]\n"
    ocaml_source

let test_clojure_string_namespace_refer_works () =
  let source =
    {|
(ns app.strings
  (:require [clojure.string :refer [upper-case trim]]))
(println (str (upper-case (trim " ada "))))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_string_namespace_refer_works" "ADA\n" ocaml_source

let test_clojure_string_namespace_rejects_bad_args () =
  Cljml.Compiler.compile_string
    {|
(ns app.strings
  (:require [clojure.string :as str]))
(def x (str/upper-case 1))
|}
  |> expect_error "str/upper-case called with incompatible arguments"

let test_clojure_string_namespace_rejects_unknown_refer () =
  Cljml.Compiler.compile_string
    {|
(ns app.strings
  (:require [clojure.string :refer [missing]]))
|}
  |> expect_error "cannot refer unknown symbol clojure.string/missing"

let test_batched_predicate_collection_core_functions_work () =
  let source =
    {|
(def xs [1 2 3 4 5])
(def split (split-at 2 xs))
(def splitw (split-with (fn [x] (< x 4)) xs))
(def parts (partition-by (fn [x] (even? x)) [1 3 2 4 5]))
(println
  (str (any? nil) ":" (rational? 1) ":" (rational? "1") ":"
       (ratio? 1) ":" (float? 1) ":" (double? 1) ":" (decimal? 1) ":"
       (simple-keyword? :name) ":" (simple-keyword? :user/name) ":"
       (qualified-keyword? :user/name) ":" (qualified-keyword? :name) ":"
       (ident? :name) ":" (simple-ident? :name) ":" (qualified-ident? :user/name) ":"
       (sequential? xs) ":" (sequential? (hash-set 1)) ":"
       (reversible? xs) ":" (reversible? (hash-set 1)) ":"
       (sorted? xs) ":" (bounded-count 3 xs) ":" (bounded-count 9 xs) ":"
       (pr-str (butlast xs)) ":" (pr-str (take-last 2 xs)) ":"
       (pr-str (drop-last 2 xs)) ":" (pr-str (take-nth 2 xs)) ":"
       (count split) ":" (pr-str (first split)) ":" (pr-str (second split)) ":"
       (pr-str (first splitw)) ":" (pr-str (second splitw)) ":"
       (count parts) ":" (count (first parts)) ":" (first (second parts)) ":"
       (nil? (dorun xs)) ":" (pr-str (doall xs))))
(run! (fn [^:int x] (println (str "item:" x))) [1 2])
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "batched_predicate_collection_core_functions_work"
    "true:true:false:false:false:false:false:true:false:true:false:true:true:true:true:false:true:false:false:3:5:[1 2 3 4]:[4 5]:[1 2 3]:[1 3 5]:2:[1 2]:[3 4 5]:[1 2 3]:[4 5]:3:2:2:true:[1 2 3 4 5]\nitem:1\nitem:2\n"
    ocaml_source

let test_batched_predicate_collection_core_functions_reject_bad_counts () =
  Cljml.Compiler.compile_string {|(def x (take-nth 0 [1 2]))|}
  |> expect_error "take-nth n must be positive"

let test_batched_predicate_collection_core_functions_reject_bad_predicates () =
  Cljml.Compiler.compile_string {|(def x (split-with (fn [^:string s] true) [1 2]))|}
  |> expect_error "split-with expects a predicate matching collection elements"

let test_batched_predicate_collection_core_functions_reject_bad_run_function () =
  Cljml.Compiler.compile_string {|(def x (run! (fn [^:string s] (println s)) [1 2]))|}
  |> expect_error "run! function type does not match collection"

let test_batched_predicate_collection_core_functions_infer_bool_params () =
  let source =
    {|
(defn prefix [flag xs] (split-with (fn [x] flag) xs))
(def bad (prefix 1 [1 2]))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "prefix called with incompatible arguments"

let test_batched_identifier_and_constructor_core_functions_work () =
  let source =
    {|
(def simple (symbol "ready"))
(def qualified (symbol "user" "name"))
(def kw (keyword qualified))
(def kw2 (keyword "user" "id"))
(def names (vector-of :symbol))
(def more-names (conj names simple qualified))
(def m1 (array-map :name "Ada" :age 36))
(def m2 (sorted-map :ready true))
(def s1 (sorted-set 3 1 2 2))
(def listed (list* 1 2 [3 4]))
(println
  (str (name qualified) ":" (namespace qualified) ":" (name kw) ":" (namespace kw) ":"
       (name kw2) ":" (namespace kw2) ":" (pr-str more-names) ":"
       (:name m1) ":" (:ready m2) ":" (pr-str s1) ":" (pr-str listed) ":"
       (symbol? simple) ":" (symbol? :ready) ":"
       (simple-symbol? simple) ":" (simple-symbol? qualified) ":"
       (qualified-symbol? qualified) ":" (qualified-symbol? simple) ":"
       (ident? simple) ":" (simple-ident? simple) ":" (qualified-ident? qualified)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "batched_identifier_and_constructor_core_functions_work"
    "name:user:name:user:id:user:[ready user/name]:Ada:true:#{1 2 3}:(1 2 3 4):true:false:true:false:true:false:true:true:true\n"
    ocaml_source

let test_batched_identifier_and_constructor_core_functions_reject_bad_symbol_args () =
  Cljml.Compiler.compile_string {|(def x (symbol 1))|}
  |> expect_error "symbol expects string, keyword, or symbol"

let test_batched_identifier_and_constructor_core_functions_reject_bad_keyword_args () =
  Cljml.Compiler.compile_string {|(def x (keyword "user" 1))|}
  |> expect_error "keyword namespace and name must be string, keyword, or symbol"

let test_batched_identifier_and_constructor_core_functions_reject_bad_namespace_args () =
  Cljml.Compiler.compile_string {|(def x (namespace 1))|}
  |> expect_error "namespace expects keyword or symbol"

let test_batched_identifier_and_constructor_core_functions_reject_bad_list_star_tail () =
  Cljml.Compiler.compile_string {|(def x (list* 1 2 3))|}
  |> expect_error "list* final argument must be a collection"

let test_batched_sequence_functions_work () =
  let source =
    {|
(def xs [1 2 3 4])
(def parts (partition 2 [1 2 3 4 5]))
(def all-parts (partition-all 2 [1 2 3 4 5]))
(println
  (str (pr-str (remove (fn [x] (even? x)) xs)) ":"
       (pr-str (take-while (fn [x] (< x 4)) xs)) ":"
       (pr-str (drop-while (fn [x] (< x 3)) xs)) ":"
       (pr-str (distinct [1 2 2 3])) ":"
       (pr-str (sort [3 1 2])) ":"
       (pr-str (concat [1 2] (list 3 4))) ":"
       (pr-str (vec (list 1 2))) ":"
       (pr-str (set [2 1 2])) ":"
       (pr-str (repeat 3 "x")) ":"
       (pr-str (repeatedly 3 (fn [] 7))) ":"
       (pr-str (interpose 0 [1 2 3])) ":"
       (pr-str (interleave [1 2] [3 4 5])) ":"
       (count parts) ":" (first (first parts)) ":" (first (second parts)) ":"
       (count all-parts) ":" (count (last all-parts)) ":"
       (pr-str (reductions (fn [acc x] (+ acc x)) 0 [1 2 3])) ":"
       (pr-str (dedupe [1 1 2 2 1])) ":"
       (pr-str (map-indexed (fn [i x] (+ i x)) [10 20])) ":"
       (pr-str (filterv (fn [x] (odd? x)) [1 2 3])) ":"
       (pr-str (mapv (fn [x] (inc x)) [1 2])) ":"
       (reduce-kv (fn [acc i x] (+ acc (+ i x))) 0 [10 20])))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "batched_sequence_functions_work"
    "[1 3]:[1 2 3]:[3 4]:[1 2 3]:(1 2 3):(1 2 3 4):[1 2]:#{1 2}:(\"x\" \"x\" \"x\"):(7 7 7):(1 0 2 0 3):(1 3 2 4):2:1:3:3:1:(0 1 3 6):[1 2 1]:(10 21):[1 3]:[2 3]:31\n"
    ocaml_source

let test_batched_sequence_functions_reject_type_mismatch () =
  Cljml.Compiler.compile_string {|(def x (concat [1] ["two"]))|}
  |> expect_error "concat element types must match"

let test_batched_sequence_functions_reject_bad_functions () =
  Cljml.Compiler.compile_string {|(def x (filterv (fn [^:string s] true) [1 2]))|}
  |> expect_error "filterv expects a predicate matching collection elements"

let test_batched_sequence_functions_reject_bad_counts () =
  Cljml.Compiler.compile_string {|(def x (repeat "3" 1))|}
  |> expect_error "repeat count must be int"

let test_batched_sequence_functions_reject_bad_partition_size () =
  Cljml.Compiler.compile_string {|(def x (partition 0 [1 2]))|}
  |> expect_error "partition size must be positive"

let test_batched_sequence_functions_reject_reduce_kv_non_vector () =
  Cljml.Compiler.compile_string
    {|(def x (reduce-kv (fn [acc i x] (+ acc x)) 0 (list 1 2)))|}
  |> expect_error "reduce-kv expects a vector"

let test_interleave_accepts_multiple_collections () =
  let source =
    {|
(def xs (interleave [1 2 3] (list 10 20) (hash-set 100 200 300)))
(println (pr-str xs))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "interleave_accepts_multiple_collections"
    "(1 10 100 2 20 200)\n" ocaml_source

let test_interleave_rejects_later_type_mismatches () =
  Cljml.Compiler.compile_string {|(def x (interleave [1] (list 2) ["three"]))|}
  |> expect_error "interleave element types must match"

let test_interleave_requires_two_collections () =
  Cljml.Compiler.compile_string {|(def x (interleave [1 2]))|}
  |> expect_error "interleave expects at least two collections"

let test_additional_sequence_helpers_work () =
  let source =
    {|
(def xs [1 2 3 4])
(def nested [[1 2] [3 4] [5 6]])
(println
  (str (pr-str (next xs)) ":"
       (pr-str (nthnext xs 2)) ":"
       (pr-str (nthrest xs 3)) ":"
       (ffirst nested) ":"
       (pr-str (fnext nested)) ":"
       (pr-str (nfirst nested)) ":"
       (count (nnext nested)) ":"
       (ffirst (nnext nested)) ":"
       (pr-str (rseq xs)) ":"
       (some (fn [x] (> x 3)) xs) ":"
       (some (fn [x] (> x 9)) xs) ":"
       (pr-str (reductions + [1 2 3 4]))))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "additional_sequence_helpers_work"
    "[2 3 4]:[3 4]:[4]:1:[3 4]:[2]:1:5:[4 3 2 1]:true:false:(1 3 6 10)\n"
    ocaml_source

let test_additional_sequence_helpers_reject_bad_counts () =
  Cljml.Compiler.compile_string {|(def x (nthnext [1 2] "1"))|}
  |> expect_error "nthnext count must be int"

let test_additional_sequence_helpers_reject_bad_some_predicate () =
  Cljml.Compiler.compile_string {|(def x (some (fn [x] (inc x)) [1 2]))|}
  |> expect_error "some expects a predicate matching collection elements"

let test_additional_sequence_helpers_reject_bad_reductions_arity () =
  Cljml.Compiler.compile_string {|(def x (reductions +))|}
  |> expect_error "reductions expects function, optional init, and collection"

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

let test_destructuring_in_let_and_functions () =
  let source =
    {|
(def user {:name "Ada", :age 36, :admin? true})
(def numbers [10 20 30])
(defn label [{:keys [name age] :as person}]
  (str name ":" (+ age 0) ":" (= person person)))
(defn first-two [[x y :as all]]
  (str (+ x 0) ":" (+ y 0) ":" (count all)))
(let [{:keys [name age] :as person} user
      [x y :as all] numbers]
  (println (str name ":" age ":" (:admin? person) ":" x ":" y ":" (count all)
                ":" (label user) ":" (first-two numbers))))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "destructuring_in_let_and_functions"
    "Ada:36:true:10:20:3:Ada:36:true:10:20:3\n" ocaml_source

let test_destructuring_supports_direct_keyword_bindings () =
  let source =
    {|
(def user {:name "Ada", :age 36})
(let [{display-name :name years :age} user]
  (println (str display-name ":" years)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "destructuring_supports_direct_keyword_bindings" "Ada:36\n"
    ocaml_source

let test_destructuring_supports_rest_and_defaults () =
  let source =
    {|
(def sparse {:name "Ada"})
(def full {:name "Grace", :age 37})
(def numbers [10 20 30 40])
(defn summarize [[x y & more :as all]]
  (str (+ x 0) ":" (+ y 0) ":" (count more) ":" (count all)))
(let [{:keys [name age] :or {age 0}} sparse
      {full-age :age missing-score :score :or {missing-score 100}} full
      [x & xs :as all] numbers]
  (println (str name ":" age ":" full-age ":" missing-score ":" x ":" (first xs) ":"
                (count xs) ":" (count all) ":" (summarize numbers))))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "destructuring_supports_rest_and_defaults"
    "Ada:0:37:100:10:20:3:4:10:20:2:4\n" ocaml_source

let test_destructuring_preserves_row_polymorphic_function_calls () =
  let source =
    {|
(def user {:name "Ada", :age 36, :admin? true})
(defn greeting [{:keys [name]}]
  (str "hi " name))
(println (greeting user))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "destructuring_preserves_row_polymorphic_function_calls" "hi Ada\n"
    ocaml_source

let test_row_polymorphic_functions_accept_different_map_shapes () =
  let source =
    {|
(def user {:name "Ada", :age 36})
(def pet {:name "Milo", :species "cat"})
(defn greeting [{:keys [name]}]
  (str "hi " name))
(println (str (greeting user) ":" (greeting pet)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "row_polymorphic_functions_accept_different_map_shapes"
    "hi Ada:hi Milo\n" ocaml_source

let test_destructuring_rejects_missing_map_fields () =
  let source =
    {|
(def user {:name "Ada"})
(defn next-age [{:keys [age]}] (+ age 1))
(def bad (next-age user))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "next-age called with incompatible arguments"

let test_destructuring_rejects_unsupported_let_sources () =
  Cljml.Compiler.compile_string {|(def x (let [{:keys [name]} [1 2]] name))|}
  |> expect_error "map destructuring expects a map"

let test_destructuring_rejects_bad_rest_binding () =
  Cljml.Compiler.compile_string {|(def x (let [[head &] [1 2]] head))|}
  |> expect_error "sequential destructuring & must be followed by a symbol"

let test_destructuring_rejects_bad_or_defaults () =
  Cljml.Compiler.compile_string
    {|(def x (let [{:keys [age] :or [age 0]} {:name "Ada"}] age))|}
  |> expect_error "map destructuring :or expects a map"

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
(println (str (add10-after-double 4) ":" (identity 7) ":" (always-ok false) ":"
              (apply + [1 2 3]) ":" (apply + (hash-set 1 2 3))))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "function_helpers" "18:7:ok:6:6\n" ocaml_source

let test_common_higher_order_helpers () =
  let source =
    {|
(def mapcat-list (mapcat (fn [x] (list x (inc x))) [1 2 3]))
(def mapcat-vector (mapcat (fn [x] [x (inc x)]) (list 1 2)))
(def sorted (sort-by (fn [x] (- 0 x)) [1 3 2]))
(def not-even? (complement (fn [x] (even? x))))
(def small-even? (every-pred (fn [x] (even? x)) (fn [x] (< x 10))))
(def odd-or-large? (some-fn (fn [x] (odd? x)) (fn [x] (> x 10))))
(def neighbors (juxt (fn [x] (dec x)) (fn [x] x) (fn [x] (inc x))))
(def piped (comp (fn [x] (+ x 1)) (fn [x] (* x 2)) (fn [x] (+ x 3))))
(println
  (str (pr-str mapcat-list) ":"
       (pr-str mapcat-vector) ":"
       (pr-str sorted) ":"
       (not-even? 3) ":" (not-even? 4) ":"
       (small-even? 8) ":" (small-even? 11) ":"
       (odd-or-large? 4) ":" (odd-or-large? 11) ":"
       (pr-str (neighbors 10)) ":"
       (piped 4) ":"
       (apply + 1 2 [3 4]) ":"
       (distinct? 1 2 3) ":" (distinct? 1 2 1) ":"
       (compare 1 2) ":" (compare "b" "a") ":"
       (max-key (fn [x] x) 1 4 2) ":"
       (min-key (fn [x] x) 1 4 2)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "common_higher_order_helpers"
    "(1 2 2 3 3 4):(1 2 2 3):(3 2 1):true:false:true:false:false:true:[9 10 11]:15:10:true:false:-1:1:4:1\n"
    ocaml_source

let test_common_higher_order_helpers_reject_bad_mapcat_result () =
  Cljml.Compiler.compile_string {|(def x (mapcat (fn [x] (inc x)) [1 2]))|}
  |> expect_error "mapcat function must return a collection"

let test_common_higher_order_helpers_reject_bad_predicates () =
  Cljml.Compiler.compile_string
    {|(def f (every-pred (fn [x] (inc x)) (fn [x] true)))|}
  |> expect_error "every-pred expects predicates with the same argument type"

let test_common_higher_order_helpers_reject_mixed_juxt_returns () =
  Cljml.Compiler.compile_string
    {|(def f (juxt (fn [x] (+ x 1)) (fn [x] (even? x))))|}
  |> expect_error "juxt functions must return the same type"

let test_common_higher_order_helpers_reject_compare_type_mismatch () =
  Cljml.Compiler.compile_string {|(def x (compare 1 "1"))|}
  |> expect_error "compare arguments must have the same type"

let test_apply_rejects_bad_set_reducers () =
  Cljml.Compiler.compile_string {|(def x (apply + (hash-set "a" "b")))|}
  |> expect_error "apply currently supports int binary reducers"

let test_set_core_api () =
  let source =
    {|
(def xs (hash-set 1 2 2 3))
(def ys (conj xs 4))
(def zs (conj ys 2))
(def same (disj zs))
(def slim (disj same 2 4))
(println (str (contains? xs 2) ":" (contains? slim 2) ":" (count ys) ":"
              (pr-str ys) ":" (pr-str same) ":" (pr-str slim)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "set_core_api" "true:false:4:#{1 2 3 4}:#{1 2 3 4}:#{1 3}\n"
    ocaml_source

let test_set_positional_sequence_helpers () =
  let source =
    {|
(def xs (hash-set 3 1 2))
(def tail (rest xs))
(def empty-tail (rest (set-of :int)))
(println
  (str (first xs) ":" (second xs) ":" (last xs) ":"
       (count tail) ":" (contains? tail 1) ":" (empty? empty-tail)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "set_positional_sequence_helpers" "1:2:3:2:false:true\n"
    ocaml_source

let test_set_positional_sequence_helpers_reject_non_collections () =
  Cljml.Compiler.compile_string {|(def x (first 1))|}
  |> expect_error "first expects a list, vector, or set"

let test_conj_rejects_set_type_mismatch () =
  Cljml.Compiler.compile_string {|(def xs (conj (hash-set 1) "two"))|}
  |> expect_error "conj value type must match set element type"

let test_disj_rejects_set_type_mismatch () =
  Cljml.Compiler.compile_string {|(def xs (disj (hash-set 1) 1 "two"))|}
  |> expect_error "disj value type must match set element type"

let test_set_sequence_core_api () =
  let source =
    {|
(def xs (hash-set 1 2 3))
(def all-positive? (every? (fn [x] (> x 0)) xs))
(def none-large? (not-any? (fn [x] (> x 10)) xs))
(def not-all-greater-than-one? (not-every? (fn [x] (> x 1)) xs))
(def total (reduce (fn [acc x] (+ acc x)) 0 xs))
(println (str all-positive? ":" none-large? ":" not-all-greater-than-one? ":" total))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "set_sequence_core_api" "true:true:true:6\n" ocaml_source

let test_set_sequence_predicates_reject_bad_predicates () =
  Cljml.Compiler.compile_string {|(def x (every? (fn [x] (+ x 1)) (hash-set 1 2)))|}
  |> expect_error "every? expects a predicate matching set elements"

let test_reduce_rejects_bad_set_reducers () =
  Cljml.Compiler.compile_string {|(def x (reduce (fn [acc x] (str acc x)) 0 (hash-set 1 2)))|}
  |> expect_error "reduce function type does not match init and set"

let test_set_map_and_filter_core_api () =
  let source =
    {|
(def xs (hash-set 1 2 3))
(def mapped (map (fn [x] (+ x 1)) xs))
(def filtered (filter (fn [x] (> x 2)) mapped))
(println (str (pr-str mapped) ":" (pr-str filtered)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "set_map_and_filter_core_api" "#{2 3 4}:#{3 4}\n" ocaml_source

let test_set_map_rejects_function_type_mismatch () =
  Cljml.Compiler.compile_string {|(def xs (map (fn [^:string x] x) (hash-set 1 2)))|}
  |> expect_error "map function argument type does not match set"

let test_set_filter_rejects_non_bool_predicates () =
  Cljml.Compiler.compile_string {|(def xs (filter (fn [x] (+ x 1)) (hash-set 1 2)))|}
  |> expect_error "filter expects a predicate matching set elements"

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

let test_range_core_api () =
  let source =
    {|
(println (str (pr-str (range 4)) ":" (pr-str (range 2 6)) ":"
              (pr-str (range 2 10 3)) ":" (pr-str (range 5 0 -2))))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "range_core_api" "(0 1 2 3):(2 3 4 5):(2 5 8):(5 3 1)\n"
    ocaml_source

let test_range_rejects_zero_step () =
  Cljml.Compiler.compile_string {|(def xs (range 1 10 0))|}
  |> expect_error "range step cannot be 0"

let test_range_rejects_non_int_arguments () =
  Cljml.Compiler.compile_string {|(def xs (range "4"))|}
  |> expect_error "range arguments must be int"

let test_take_and_drop_core_api () =
  let source =
    {|
(def xs [1 2 3 4])
(def ys (list 1 2 3 4))
(println (str (pr-str (take 2 xs)) ":" (pr-str (drop 2 xs)) ":"
              (pr-str (take 9 ys)) ":" (pr-str (drop 9 ys))))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "take_and_drop_core_api" "[1 2]:[3 4]:(1 2 3 4):()\n"
    ocaml_source

let test_take_and_drop_reject_non_int_counts () =
  Cljml.Compiler.compile_string {|(def x (take "2" [1 2]))|}
  |> expect_error "take count must be int"

let test_take_and_drop_reject_unsupported_collections () =
  Cljml.Compiler.compile_string {|(def x (drop 1 (hash-set 1)))|}
  |> expect_error "drop expects a list or vector"

let test_reverse_core_api () =
  let source =
    {|
(def xs [1 2 3])
(def ys (list 1 2 3))
(println (str (pr-str (reverse xs)) ":" (pr-str (reverse ys))))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "reverse_core_api" "[3 2 1]:(3 2 1)\n" ocaml_source

let test_reverse_rejects_unsupported_collections () =
  Cljml.Compiler.compile_string {|(def x (reverse (hash-set 1)))|}
  |> expect_error "reverse expects a list or vector"

let test_sequence_boolean_predicates () =
  let source =
    {|
(def xs [2 4 6])
(def ys (list 1 2 3))
(println (str (every? (fn [x] (> x 0)) xs) ":"
              (not-any? (fn [x] (> x 10)) xs) ":"
              (not-every? (fn [x] (> x 1)) ys)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sequence_boolean_predicates" "true:true:true\n" ocaml_source

let test_sequence_boolean_predicates_reject_non_bool_predicates () =
  Cljml.Compiler.compile_string {|(def x (every? (fn [x] (+ x 1)) [1 2]))|}
  |> expect_error "every? expects a predicate matching vector elements"

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

let test_typed_empty_sets () =
  let source =
    {|
(def xs (set-of :int))
(def ys (into xs [1 2 2 3]))
(def zs (disj ys 2))
(println (str (empty? xs) ":" (count ys) ":" (contains? ys 2) ":"
              (contains? zs 2) ":" (pr-str zs)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_empty_sets" "true:3:true:false:#{1 3}\n" ocaml_source

let test_set_of_rejects_unknown_types () =
  Cljml.Compiler.compile_string {|(def xs (set-of :record))|}
  |> expect_error "unknown set element type :record"

let test_keyword_type_annotations_for_empty_collections () =
  let source =
    {|
(def xs (conj (vector-of :keyword) :name))
(def ys (conj (list-of :keyword) :age))
(def zs (into (set-of :keyword) [:name :name :age]))
(println (str (pr-str xs) ":" (pr-str ys) ":" (contains? zs :age) ":" (pr-str zs)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "keyword_type_annotations_for_empty_collections"
    "[:name]:(:age):true:#{:age :name}\n" ocaml_source

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

let test_rest_is_empty_safe () =
  let source =
    {|
(def xs (rest (list-of :int)))
(def ys (rest (vector-of :int)))
(println (str (empty? xs) ":" (pr-str xs) ":" (empty? ys) ":" (pr-str ys)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "rest_is_empty_safe" "true:():true:[]\n" ocaml_source

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

let test_subvec_core_api () =
  let source =
    {|
(def xs [1 2 3 4])
(def tail (subvec xs 1))
(def middle (subvec xs 1 3))
(println (str (pr-str tail) ":" (pr-str middle)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "subvec_core_api" "[2 3 4]:[2 3]\n" ocaml_source

let test_subvec_rejects_non_vector_sources () =
  Cljml.Compiler.compile_string {|(def x (subvec (list 1 2) 0))|}
  |> expect_error "subvec expects a vector"

let test_subvec_rejects_non_int_indexes () =
  Cljml.Compiler.compile_string {|(def x (subvec [1 2] "0"))|}
  |> expect_error "subvec indexes must be int"

let test_peek_rejects_unsupported_collections () =
  Cljml.Compiler.compile_string {|(def x (peek (hash-set 1)))|}
  |> expect_error "peek expects a list or vector"

let test_let_rejects_odd_binding_forms () =
  Cljml.Compiler.compile_string {|(def x (let [a 1 b] a))|}
  |> expect_error "let bindings require an even number of forms"

let test_map_rejects_non_function_argument () =
  Cljml.Compiler.compile_string {|(def xs (map 1 [1 2]))|}
  |> expect_error "map expects a function"

let test_match_expression_works () =
  let source =
    {|
(defn describe [x]
  (match x
    0 "zero"
    1 "one"
    n (str "n=" n)))
(def empty-list-score (match (list-of :int) [] 0 _ 99))
(def one-list-score (match (list 7) [] 0 [x] x _ 99))
(def two-list-score (match (list 3 4) [] 0 [x] x [x y] (+ x y) _ 99))
(def many-list-score (match (list 1 2 3) [] 0 [x] x [x y] (+ x y) _ 99))
(def empty-vector-score (match (vector-of :int) [] 0 _ 99))
(def one-vector-score (match [7] [] 0 [x] x _ 99))
(def two-vector-score (match [3 4] [] 0 [x] x [x y] (+ x y) _ 99))
(def many-vector-score (match [1 2 3] [] 0 [x] x [x y] (+ x y) _ 99))
(println
  (str (describe 0) ":" (describe 2) ":"
       empty-list-score ":" one-list-score ":" two-list-score ":" many-list-score ":"
       empty-vector-score ":" one-vector-score ":" two-vector-score ":" many-vector-score))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "match_expression_works" "zero:n=2:0:7:7:99:0:7:7:99\n"
    ocaml_source

let test_match_rejects_branch_type_mismatch () =
  Cljml.Compiler.compile_string {|(def x (match 1 0 "zero" _ 1))|}
  |> expect_error "match branches must have same type"

let test_match_rejects_bad_clause_count () =
  Cljml.Compiler.compile_string {|(def x (match 1 0 "zero" _))|}
  |> expect_error "match requires pattern/result pairs"

let test_match_rejects_pattern_type_mismatch () =
  Cljml.Compiler.compile_string {|(def x (match 1 "1" 1 _ 0))|}
  |> expect_error "match pattern type must match target"

let test_match_infers_target_type_from_patterns () =
  Cljml.Compiler.compile_string
    {|
(defn describe [x]
  (match x
    0 "zero"
    n (str "n=" n)))
(def bad (describe "x"))
|}
  |> expect_error "describe called with incompatible arguments"

let test_module_definitions_work () =
  let source =
    {|
(module Math
  (def answer 42)
  (defn add2 [x] (+ x 2)))
(module User
  (def label "Ada")
  (module Name
    (defn greet [name] (str "hi " name))))
(println (str (Math/add2 Math/answer) ":" User/label ":" (User.Name/greet "Grace")))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_definitions_work" "44:Ada:hi Grace\n" ocaml_source

let test_module_definitions_reject_expressions () =
  Cljml.Compiler.compile_string
    {|
(module Math
  (println "side effect"))
|}
  |> expect_error "module forms must be def, defn, or module"

let test_incremental_compilation_preserves_modules () =
  let state = Cljml.Compiler.empty_state in
  let state, module_ocaml =
    Cljml.Compiler.compile_chunk state
      {|
(module Math
  (defn add2 [x] (+ x 2)))
|}
    |> expect_ok
  in
  let _state, app_ocaml =
    Cljml.Compiler.compile_chunk state
      {|
(println (Math/add2 40))
|}
    |> expect_ok
  in
  assert_ocaml_runs "incremental_compilation_preserves_modules" "42\n"
    (module_ocaml ^ "\n\n" ^ app_ocaml)

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

let test_incremental_compilation_preserves_protocols () =
  let state = Cljml.Compiler.empty_state in
  let state, protocol_ocaml =
    Cljml.Compiler.compile_chunk state
      {|
(defprotocol Labelled
  (label [x] :string))
(extend-type :int
  Labelled
  (label [x] (str "int:" x)))
|}
    |> expect_ok
  in
  let _state, call_ocaml =
    Cljml.Compiler.compile_chunk state
      {|
(println (label 42))
|}
    |> expect_ok
  in
  assert_ocaml_runs "incremental_compilation_preserves_protocols" "int:42\n"
    (protocol_ocaml ^ "\n\n" ^ call_ocaml)

let test_parsetree_backend_prints_runnable_ocaml () =
  let source =
    {|
(def user {:name "Ada", :age 36})
(println (str (:name user) ":" (:age user)))
|}
  in
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_prints_runnable_ocaml" "Ada:36\n" ocaml_source

let test_parsetree_backend_preserves_static_errors () =
  Cljml.Compiler.compile_parsetree {|(def x (+ 1 "two"))|}
  |> expect_error_value "expected int arguments for +"

let test_parsetree_backend_builds_native_record_items () =
  let structure =
    Cljml.Compiler.compile_parsetree {|(def user {:name "Ada", :age 36})|}
    |> expect_ok
  in
  match structure with
  | type_item :: value_item :: _ -> (
      match (type_item.pstr_desc, value_item.pstr_desc) with
      | Pstr_type _, Pstr_value _ ->
          if not (type_item.pstr_loc.loc_ghost && value_item.pstr_loc.loc_ghost) then
            failwith "expected native record structure items with ghost locations"
      | _ -> failwith "expected record type and value structure items")
  | _ -> failwith "expected record type and value structure items"

let test_parsetree_backend_builds_native_value_items () =
  let structure =
    Cljml.Compiler.compile_parsetree
      {|
(def answer 42)
(println answer)
|}
    |> expect_ok
  in
  match structure with
  | [ definition; effect_item ] -> (
      match (definition.pstr_desc, effect_item.pstr_desc) with
      | Pstr_value _, Pstr_value _ ->
          if not
               (definition.pstr_loc.loc_ghost && effect_item.pstr_loc.loc_ghost)
          then
            failwith "expected native value structure items with ghost locations"
      | _ -> failwith "expected definition and effect value structure items")
  | _ -> failwith "expected exactly two value structure items"

let test_parsetree_backend_builds_native_defn_items () =
  let structure =
    Cljml.Compiler.compile_parsetree
      {|(defn user-name [{:keys [name]}] name)|}
    |> expect_ok
  in
  match structure with
  | [ type_item; value_item ] -> (
      match (type_item.pstr_desc, value_item.pstr_desc) with
      | Pstr_type _, Pstr_value _ ->
          if not (type_item.pstr_loc.loc_ghost && value_item.pstr_loc.loc_ghost) then
            failwith "expected native defn structure items with ghost locations"
      | _ -> failwith "expected row type and function value structure items")
  | _ -> failwith "expected row type and function value structure items"

let test_parsetree_backend_builds_native_protocol_items () =
  let structure =
    Cljml.Compiler.compile_parsetree
      {|
(defprotocol Labelled
  (label [x] :string))
(extend-type :int
  Labelled
  (label [x] (str "int:" x)))
|}
    |> expect_ok
  in
  match structure with
  | [ item ] -> (
      match item.pstr_desc with
      | Pstr_value _ ->
          if not item.pstr_loc.loc_ghost then
            failwith "expected native protocol value item with a ghost location"
      | _ -> failwith "expected protocol implementation value item")
  | _ -> failwith "expected one protocol implementation value item"

let test_parsetree_backend_builds_native_module_items () =
  let structure =
    Cljml.Compiler.compile_parsetree
      {|
(module Math
  (def answer 42)
  (module Nested
    (def value 7)))
|}
    |> expect_ok
  in
  match structure with
  | [ module_item ] -> (
      match module_item.pstr_desc with
      | Pstr_module binding -> (
          match binding.pmb_expr.pmod_desc with
          | Pmod_structure [ value_item; nested_item ] ->
              if not
                   (module_item.pstr_loc.loc_ghost
                   && binding.pmb_expr.pmod_loc.loc_ghost
                   && value_item.pstr_loc.loc_ghost
                   && nested_item.pstr_loc.loc_ghost)
              then failwith "expected native nested module items with ghost locations"
          | _ -> failwith "expected value and nested module body items")
      | _ -> failwith "expected module structure item")
  | _ -> failwith "expected one module structure item"

let test_parsetree_backend_builds_native_scalar_expressions () =
  let structure =
    Cljml.Compiler.compile_parsetree {|(def answer 42)|} |> expect_ok
  in
  match structure with
  | [ item ] -> (
      match item.pstr_desc with
      | Pstr_value (_, [ binding ]) -> (
          match binding.pvb_expr.pexp_desc with
          | Pexp_constant _ ->
              if not binding.pvb_expr.pexp_loc.loc_ghost then
                failwith "expected native scalar expression with a ghost location"
          | _ -> failwith "expected scalar constant expression")
      | _ -> failwith "expected one scalar value binding")
  | _ -> failwith "expected one scalar value binding"

let test_parsetree_backend_builds_native_collection_expressions () =
  let structure =
    Cljml.Compiler.compile_parsetree
      {|
(def xs [1 2 3])
(def ys (list 4 5 6))
|}
    |> expect_ok
  in
  let value_expression (item : Parsetree.structure_item) =
    match item with
    | { pstr_desc = Pstr_value (_, [ binding ]); _ } -> binding.pvb_expr
    | _ -> failwith "expected one value binding"
  in
  match structure with
  | [ vector_item; list_item ] ->
      let vector_expr = value_expression vector_item in
      let list_expr = value_expression list_item in
      (match (vector_expr.pexp_desc, list_expr.pexp_desc) with
      | Pexp_apply _, Pexp_construct _ ->
          if not (vector_expr.pexp_loc.loc_ghost && list_expr.pexp_loc.loc_ghost) then
            failwith "expected native collection expressions with ghost locations"
      | _ -> failwith "expected vector application and list constructor expressions")
  | _ -> failwith "expected vector and list value bindings"

let test_parsetree_backend_builds_native_conditional_expressions () =
  let structure =
    Cljml.Compiler.compile_parsetree
      {|
(def answer (if true 42 0))
(def fallback (if-not false 7 9))
(when true (println "ready"))
|}
    |> expect_ok
  in
  let value_expression (item : Parsetree.structure_item) =
    match item with
    | { pstr_desc = Pstr_value (_, [ binding ]); _ } -> binding.pvb_expr
    | _ -> failwith "expected one value binding"
  in
  match structure with
  | [ if_item; if_not_item; when_item ] ->
      let expressions =
        List.map value_expression [ if_item; if_not_item; when_item ]
      in
      if
        not
          (List.for_all
             (fun (expression : Parsetree.expression) ->
               expression.pexp_loc.loc_ghost
               && match expression.pexp_desc with Pexp_ifthenelse _ -> true | _ -> false)
             expressions)
      then failwith "expected native conditional expressions with ghost locations"
  | _ -> failwith "expected three conditional value bindings"

let test_parsetree_backend_builds_native_function_expressions () =
  expect_structured_value_expression {|(defn identity-value [x] x)|}

let test_parsetree_backend_builds_native_sequence_expressions () =
  expect_structured_value_expression {|(def result (do 1 2 3))|}

let test_parsetree_backend_builds_native_let_expressions () =
  expect_structured_value_expression
    {|(def result (let [x 1 y (+ x 1)] (+ y 1)))|}

let test_parsetree_backend_builds_native_match_expressions () =
  expect_structured_value_expression
    {|(def result (match [1 2] [x y] (+ x y)))|}

let test_incremental_parsetree_backend_preserves_state () =
  let state = Cljml.Compiler.empty_state in
  let state, people_structure =
    Cljml.Compiler.compile_chunk_parsetree state
      {|
(ns people.core)
(def user {:name "Ada", :age 36})
|}
    |> expect_ok
  in
  let _state, app_structure =
    Cljml.Compiler.compile_chunk_parsetree state
      {|
(ns app.main
  (:require [people.core :as p]))
(println (str (:name p/user) ":" (:age p/user)))
|}
    |> expect_ok
  in
  let people_ocaml = Cljml.Compiler.print_parsetree people_structure in
  let app_ocaml = Cljml.Compiler.print_parsetree app_structure in
  assert_ocaml_runs "incremental_parsetree_backend_preserves_state" "Ada:36\n"
    (people_ocaml ^ "\n\n" ^ app_ocaml)

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
    ( "not uses static Clojure truthiness",
      test_not_uses_static_clojure_truthiness );
    ("type predicates work", test_type_predicates);
    ("type predicates reject wrong arity", test_type_predicates_reject_wrong_arity);
    ("subs core api works", test_subs_core_api);
    ("subs rejects non-string sources", test_subs_rejects_non_string_sources);
    ("subs rejects non-int indexes", test_subs_rejects_non_int_indexes);
    ( "namespaces resolve qualified and current symbols",
      test_namespaces_resolve_qualified_and_current_symbols );
    ( "namespaces prevent unqualified symbol collisions",
      test_namespaces_prevent_unqualified_symbol_collisions );
    ("ocaml keyword names are munged", test_ocaml_keyword_names_are_munged);
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
    ( "static protocols dispatch by receiver type",
      test_static_protocols_dispatch_by_receiver_type );
    ( "static protocols reject missing implementations",
      test_static_protocols_reject_missing_implementation );
    ( "static protocols reject return type mismatch",
      test_static_protocols_reject_return_type_mismatch );
    ( "static protocols work through namespace aliases",
      test_static_protocols_work_through_namespace_aliases );
    ("do and multi-form bodies work", test_do_and_multi_form_bodies);
    ("fn rejects empty body", test_fn_rejects_empty_body);
    ("vectors reject mixed element types", test_vectors_reject_mixed_element_types);
    ("keyword values print as keywords", test_keyword_values_print_as_keywords);
    ("keys return keyword values", test_keys_return_keyword_values);
    ("vals return homogeneous values", test_vals_return_homogeneous_values);
    ("vals rejects heterogeneous values", test_vals_rejects_heterogeneous_values);
    ( "vectors reject mixed keyword and string elements",
      test_vectors_reject_mixed_keyword_and_string_elements );
    ("arithmetic rejects non-int arguments", test_arithmetic_rejects_non_int_arguments);
    ("arithmetic core arities work", test_arithmetic_core_arities);
    ( "integer division rejects unsupported arities",
      test_integer_division_rejects_unsupported_arities );
    ("chained comparisons work", test_chained_comparisons);
    ("not= core api works", test_not_equal_core_api);
    ("not= rejects mixed types", test_not_equal_rejects_mixed_types);
    ("collection equality core api works", test_collection_equality_core_api);
    ("get rejects unknown map fields", test_get_rejects_unknown_map_fields);
    ("get supports default values", test_get_supports_default_values);
    ( "get rejects default type mismatch for known fields",
      test_get_rejects_default_type_mismatch_for_known_fields );
    ("get supports vectors", test_get_supports_vectors);
    ( "get rejects vector default type mismatch",
      test_get_rejects_vector_default_type_mismatch );
    ("assoc supports multiple pairs", test_assoc_supports_multiple_pairs);
    ("assoc rejects odd key value pairs", test_assoc_rejects_odd_key_value_pairs);
    ("assoc supports vector indexes", test_assoc_supports_vector_indexes);
    ( "assoc rejects vector value type mismatch",
      test_assoc_rejects_vector_value_type_mismatch );
    ("assoc rejects vector non-int indexes", test_assoc_rejects_vector_non_int_indexes);
    ("dissoc supports multiple keys", test_dissoc_supports_multiple_keys);
    ("map merge, update, and select-keys work", test_map_merge_update_and_select_keys);
    ( "merge rejects incompatible overlapping fields",
      test_merge_rejects_incompatible_overlapping_fields );
    ("update rejects type changes", test_update_rejects_type_changes);
    ("update supports extra arguments", test_update_supports_extra_arguments);
    ( "update rejects extra argument type mismatch",
      test_update_rejects_extra_argument_type_mismatch );
    ("update supports vector indexes", test_update_supports_vector_indexes);
    ( "update rejects vector index type mismatch",
      test_update_rejects_vector_index_type_mismatch );
    ("select-keys rejects unknown fields", test_select_keys_rejects_unknown_fields);
    ("contains supports vector indexes", test_contains_supports_vector_indexes);
    ("contains rejects vector non-int indexes", test_contains_rejects_vector_non_int_indexes);
    ("if rejects branch type mismatch", test_if_rejects_branch_type_mismatch);
    ("conditional forms work", test_conditional_forms_work);
    ("if-not rejects branch type mismatch", test_if_not_rejects_branch_type_mismatch);
    ("cond rejects missing else", test_cond_rejects_missing_else);
    ("cond rejects branch type mismatch", test_cond_rejects_branch_type_mismatch);
    ("cond rejects non-bool tests", test_cond_rejects_non_bool_tests);
    ("when rejects value body", test_when_rejects_value_body);
    ( "conditional forms infer bool params",
      test_conditional_forms_infer_bool_params );
    ("batched core functions work", test_batched_core_functions_work);
    ( "batched core functions reject non-int arguments",
      test_batched_core_functions_reject_non_int_arguments );
    ( "batched core functions reject bad arities",
      test_batched_core_functions_reject_bad_arities );
    ( "batched core functions infer int params",
      test_batched_core_functions_infer_int_params );
    ( "batched numeric/scalar core functions work",
      test_batched_numeric_scalar_core_functions_work );
    ( "batched numeric/scalar core functions reject non-int bit args",
      test_batched_numeric_scalar_core_functions_reject_non_int_bit_args );
    ( "batched numeric/scalar core functions reject unchecked arity",
      test_batched_numeric_scalar_core_functions_reject_unchecked_arity );
    ( "batched numeric/scalar core functions reject bad name arg",
      test_batched_numeric_scalar_core_functions_reject_bad_name_arg );
    ( "batched numeric/scalar core functions infer int params",
      test_batched_numeric_scalar_core_functions_infer_int_params );
    ( "clojure.string namespace batch works",
      test_clojure_string_namespace_batch_works );
    ( "clojure.string namespace refer works",
      test_clojure_string_namespace_refer_works );
    ( "clojure.string namespace rejects bad args",
      test_clojure_string_namespace_rejects_bad_args );
    ( "clojure.string namespace rejects unknown refer",
      test_clojure_string_namespace_rejects_unknown_refer );
    ( "batched predicate/collection core functions work",
      test_batched_predicate_collection_core_functions_work );
    ( "batched predicate/collection core functions reject bad counts",
      test_batched_predicate_collection_core_functions_reject_bad_counts );
    ( "batched predicate/collection core functions reject bad predicates",
      test_batched_predicate_collection_core_functions_reject_bad_predicates );
    ( "batched predicate/collection core functions reject bad run function",
      test_batched_predicate_collection_core_functions_reject_bad_run_function );
    ( "batched predicate/collection core functions infer bool params",
      test_batched_predicate_collection_core_functions_infer_bool_params );
    ( "batched identifier/constructor core functions work",
      test_batched_identifier_and_constructor_core_functions_work );
    ( "batched identifier/constructor core functions reject bad symbol args",
      test_batched_identifier_and_constructor_core_functions_reject_bad_symbol_args );
    ( "batched identifier/constructor core functions reject bad keyword args",
      test_batched_identifier_and_constructor_core_functions_reject_bad_keyword_args );
    ( "batched identifier/constructor core functions reject bad namespace args",
      test_batched_identifier_and_constructor_core_functions_reject_bad_namespace_args );
    ( "batched identifier/constructor core functions reject bad list* tail",
      test_batched_identifier_and_constructor_core_functions_reject_bad_list_star_tail );
    ("batched sequence functions work", test_batched_sequence_functions_work);
    ( "batched sequence functions reject type mismatch",
      test_batched_sequence_functions_reject_type_mismatch );
    ( "batched sequence functions reject bad functions",
      test_batched_sequence_functions_reject_bad_functions );
    ( "batched sequence functions reject bad counts",
      test_batched_sequence_functions_reject_bad_counts );
    ( "batched sequence functions reject bad partition size",
      test_batched_sequence_functions_reject_bad_partition_size );
    ( "batched sequence functions reject reduce-kv non-vector",
      test_batched_sequence_functions_reject_reduce_kv_non_vector );
    ( "interleave accepts multiple collections",
      test_interleave_accepts_multiple_collections );
    ( "interleave rejects later type mismatches",
      test_interleave_rejects_later_type_mismatches );
    ( "interleave requires two collections",
      test_interleave_requires_two_collections );
    ("additional sequence helpers work", test_additional_sequence_helpers_work);
    ( "additional sequence helpers reject bad counts",
      test_additional_sequence_helpers_reject_bad_counts );
    ( "additional sequence helpers reject bad some predicate",
      test_additional_sequence_helpers_reject_bad_some_predicate );
    ( "additional sequence helpers reject bad reductions arity",
      test_additional_sequence_helpers_reject_bad_reductions_arity );
    ("let, defn, and fn values work", test_let_defn_and_fn_values);
    ("destructuring works in let and functions", test_destructuring_in_let_and_functions);
    ( "destructuring supports direct keyword bindings",
      test_destructuring_supports_direct_keyword_bindings );
    ( "destructuring supports rest and defaults",
      test_destructuring_supports_rest_and_defaults );
    ( "destructuring preserves row polymorphic function calls",
      test_destructuring_preserves_row_polymorphic_function_calls );
    ( "row polymorphic functions accept different map shapes",
      test_row_polymorphic_functions_accept_different_map_shapes );
    ( "destructuring rejects missing map fields",
      test_destructuring_rejects_missing_map_fields );
    ( "destructuring rejects unsupported let sources",
      test_destructuring_rejects_unsupported_let_sources );
    ( "destructuring rejects bad rest binding",
      test_destructuring_rejects_bad_rest_binding );
    ( "destructuring rejects bad or defaults",
      test_destructuring_rejects_bad_or_defaults );
    ("sequence core api works on vectors", test_sequence_core_api_on_vectors);
    ("function helpers work", test_function_helpers);
    ("common higher-order helpers work", test_common_higher_order_helpers);
    ( "common higher-order helpers reject bad mapcat result",
      test_common_higher_order_helpers_reject_bad_mapcat_result );
    ( "common higher-order helpers reject bad predicates",
      test_common_higher_order_helpers_reject_bad_predicates );
    ( "common higher-order helpers reject mixed juxt returns",
      test_common_higher_order_helpers_reject_mixed_juxt_returns );
    ( "common higher-order helpers reject compare type mismatch",
      test_common_higher_order_helpers_reject_compare_type_mismatch );
    ("apply rejects bad set reducers", test_apply_rejects_bad_set_reducers);
    ("set core api works", test_set_core_api);
    ( "set positional sequence helpers work",
      test_set_positional_sequence_helpers );
    ( "set positional sequence helpers reject non-collections",
      test_set_positional_sequence_helpers_reject_non_collections );
    ("conj rejects set type mismatch", test_conj_rejects_set_type_mismatch);
    ("disj rejects set type mismatch", test_disj_rejects_set_type_mismatch);
    ("set sequence core api works", test_set_sequence_core_api);
    ( "set sequence predicates reject bad predicates",
      test_set_sequence_predicates_reject_bad_predicates );
    ("reduce rejects bad set reducers", test_reduce_rejects_bad_set_reducers);
    ("set map and filter core api works", test_set_map_and_filter_core_api);
    ( "set map rejects function type mismatch",
      test_set_map_rejects_function_type_mismatch );
    ( "set filter rejects non-bool predicates",
      test_set_filter_rejects_non_bool_predicates );
    ("list core api works", test_list_core_api);
    ("sequence core api works on lists", test_sequence_core_api_on_lists);
    ("range core api works", test_range_core_api);
    ("range rejects zero step", test_range_rejects_zero_step);
    ("range rejects non-int arguments", test_range_rejects_non_int_arguments);
    ("take and drop core api works", test_take_and_drop_core_api);
    ("take and drop reject non-int counts", test_take_and_drop_reject_non_int_counts);
    ( "take and drop reject unsupported collections",
      test_take_and_drop_reject_unsupported_collections );
    ("reverse core api works", test_reverse_core_api);
    ("reverse rejects unsupported collections", test_reverse_rejects_unsupported_collections);
    ("sequence boolean predicates work", test_sequence_boolean_predicates);
    ( "sequence boolean predicates reject non-bool predicates",
      test_sequence_boolean_predicates_reject_non_bool_predicates );
    ("empty core api works", test_empty_core_api);
    ("empty rejects unsupported values", test_empty_rejects_unsupported_values);
    ("into core api works", test_into_core_api);
    ("into rejects element type mismatch", test_into_rejects_element_type_mismatch);
    ("typed empty sets work", test_typed_empty_sets);
    ("set-of rejects unknown types", test_set_of_rejects_unknown_types);
    ( "keyword type annotations for empty collections work",
      test_keyword_type_annotations_for_empty_collections );
    ("nth supports default values", test_nth_supports_default_values);
    ("nth rejects default type mismatch", test_nth_rejects_default_type_mismatch);
    ("typed empty lists work", test_typed_empty_lists);
    ("rest is empty-safe", test_rest_is_empty_safe);
    ("lists reject mixed element types", test_lists_reject_mixed_element_types);
    ("conj rejects list type mismatch", test_conj_rejects_list_type_mismatch);
    ("collection positional helpers work", test_collection_positional_helpers);
    ("subvec core api works", test_subvec_core_api);
    ("subvec rejects non-vector sources", test_subvec_rejects_non_vector_sources);
    ("subvec rejects non-int indexes", test_subvec_rejects_non_int_indexes);
    ("peek rejects unsupported collections", test_peek_rejects_unsupported_collections);
    ("let rejects odd binding forms", test_let_rejects_odd_binding_forms);
    ("map rejects non-function argument", test_map_rejects_non_function_argument);
    ("match expression works", test_match_expression_works);
    ("match rejects branch type mismatch", test_match_rejects_branch_type_mismatch);
    ("match rejects bad clause count", test_match_rejects_bad_clause_count);
    ("match rejects pattern type mismatch", test_match_rejects_pattern_type_mismatch);
    ("match infers target type from patterns", test_match_infers_target_type_from_patterns);
    ("module definitions work", test_module_definitions_work);
    ("module definitions reject expressions", test_module_definitions_reject_expressions);
    ( "incremental compilation preserves modules",
      test_incremental_compilation_preserves_modules );
    ( "incremental compilation preserves state",
      test_incremental_compilation_preserves_state );
    ( "incremental compilation requires prior state",
      test_incremental_compilation_requires_prior_state );
    ( "incremental compilation preserves protocols",
      test_incremental_compilation_preserves_protocols );
    ( "parsetree backend prints runnable ocaml",
      test_parsetree_backend_prints_runnable_ocaml );
    ( "parsetree backend preserves static errors",
      test_parsetree_backend_preserves_static_errors );
    ( "parsetree backend builds native record items",
      test_parsetree_backend_builds_native_record_items );
    ( "parsetree backend builds native value items",
      test_parsetree_backend_builds_native_value_items );
    ( "parsetree backend builds native defn items",
      test_parsetree_backend_builds_native_defn_items );
    ( "parsetree backend builds native protocol items",
      test_parsetree_backend_builds_native_protocol_items );
    ( "parsetree backend builds native module items",
      test_parsetree_backend_builds_native_module_items );
    ( "parsetree backend builds native scalar expressions",
      test_parsetree_backend_builds_native_scalar_expressions );
    ( "parsetree backend builds native collection expressions",
      test_parsetree_backend_builds_native_collection_expressions );
    ( "parsetree backend builds native conditional expressions",
      test_parsetree_backend_builds_native_conditional_expressions );
    ( "parsetree backend builds native function expressions",
      test_parsetree_backend_builds_native_function_expressions );
    ( "parsetree backend builds native sequence expressions",
      test_parsetree_backend_builds_native_sequence_expressions );
    ( "parsetree backend builds native match expressions",
      test_parsetree_backend_builds_native_match_expressions );
    ( "parsetree backend builds native let expressions",
      test_parsetree_backend_builds_native_let_expressions );
    ( "incremental parsetree backend preserves state",
      test_incremental_parsetree_backend_preserves_state );
  ]

let () =
  List.iter
    (fun (name, run) ->
      try run ()
      with exn ->
        Printf.eprintf "FAILED: %s\n%s\n" name (Printexc.to_string exn);
        exit 1)
    tests
