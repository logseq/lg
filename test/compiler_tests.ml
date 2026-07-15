let expect_ok = function
  | Ok value -> value
  | Error (err : Lg.Compiler.compile_error) ->
      failwith ("expected successful compilation, got: " ^ err.message)

let expect_error expected = function
  | Ok value ->
      failwith ("expected compilation error, got OCaml output:\n" ^ value)
  | Error (err : Lg.Compiler.compile_error) ->
      if err.message <> expected then
        failwith
          (Printf.sprintf "expected error %S, got %S" expected err.message)

let expect_error_value expected = function
  | Ok _ -> failwith "expected compilation error, got successful result"
  | Error (err : Lg.Compiler.compile_error) ->
      if err.message <> expected then
        failwith
          (Printf.sprintf "expected error %S, got %S" expected err.message)

let string_contains_substring text expected =
  let expected_len = String.length expected in
  let rec loop index =
    index + expected_len <= String.length text
    && (String.sub text index expected_len = expected || loop (index + 1))
  in
  expected_len = 0 || loop 0

let count_generated_anonymous_record_types source =
  let prefix = "type nonrec t" in
  source |> String.split_on_char '\n'
  |> List.fold_left
       (fun count line ->
         if
           String.starts_with ~prefix line
           && String.length line > String.length prefix
           &&
           let suffix = line.[String.length prefix] in
           suffix >= '0' && suffix <= '9'
         then count + 1
         else count)
       0

let substring_index text expected =
  let expected_len = String.length expected in
  let rec loop index =
    if index + expected_len > String.length text then None
    else if String.sub text index expected_len = expected then Some index
    else loop (index + 1)
  in
  if expected_len = 0 then Some 0 else loop 0

let expect_substring_index text expected =
  match substring_index text expected with
  | Some index -> index
  | None -> failwith (Printf.sprintf "expected %S in source" expected)

let expect_error_contains expected = function
  | Ok _ -> failwith "expected compilation error, got successful result"
  | Error (err : Lg.Compiler.compile_error) ->
      if not (string_contains_substring err.message expected) then
        failwith
          (Printf.sprintf "expected error containing %S, got %S" expected
             err.message)

let typecheck_items source =
  match Lg.Lexer.tokenize source with
  | Error (err : Lg.Error.t) ->
      failwith ("expected successful lexing, got: " ^ err.message)
  | Ok tokens -> (
      match Lg.Parser.parse tokens with
      | Error (err : Lg.Error.t) ->
          failwith ("expected successful parsing, got: " ^ err.message)
      | Ok forms -> Lg.Typecheck.compile_forms forms |> expect_ok)

let typecheck_state source =
  match Lg.Lexer.tokenize source with
  | Error (err : Lg.Error.t) -> failwith err.message
  | Ok tokens -> (
      match Lg.Parser.parse tokens with
      | Error (err : Lg.Error.t) -> failwith err.message
      | Ok forms ->
          Lg.Typecheck.compile_forms_incremental Lg.Typecheck.empty_state forms
          |> expect_ok |> fst)

let expect_structured_value_expression source =
  let rec find_value_expression = function
    | [] -> None
    | Lg.Lowered.Value_binding { expression; _ } :: _ -> Some expression
    | Lg.Lowered.Group items :: rest -> (
        match find_value_expression items with
        | Some _ as expression -> expression
        | None -> find_value_expression rest)
    | _ :: rest -> find_value_expression rest
  in
  match typecheck_items source |> find_value_expression with
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
    if parent = dir then failwith "could not find repo root"
    else find_repo_root parent

let repo_root () = find_repo_root (Sys.getcwd ())

let rrbvec_build_dir () =
  Filename.concat (repo_root ()) "_build/default/vendor/rrbvec"

let rrbvec_cmi_dir () =
  Filename.concat (rrbvec_build_dir ()) ".rrbvec.objs/byte"

let lg_build_dir () = Filename.concat (repo_root ()) "_build/default/src"

let lg_runtime_build_dir () =
  Filename.concat (repo_root ()) "_build/default/runtime"

let lg_byte_cmi_dir () = Filename.concat (lg_build_dir ()) ".lg.objs/byte"
let lg_cma () = Filename.concat (lg_build_dir ()) "lg.cma"

let lg_runtime_byte_cmi_dir () =
  Filename.concat (lg_runtime_build_dir ()) ".lg_runtime.objs/byte"

let lg_runtime_cma () =
  Filename.concat (lg_runtime_build_dir ()) "lg_runtime.cma"

let rrbvec_cma () = Filename.concat (rrbvec_build_dir ()) "rrbvec.cma"

type compile_job = { name : string; ocaml_source : string }

type run_job = {
  name : string;
  expected_output : string;
  ocaml_source : string;
}

let pending_compile_jobs = ref []
let pending_run_jobs = ref []

let time_phase label fn =
  let started = Unix.gettimeofday () in
  let result = fn () in
  if Sys.getenv_opt "LG_TEST_TIMING" = Some "1" then
    Printf.eprintf "%s: %.3fs\n%!" label (Unix.gettimeofday () -. started);
  result

let test_directory =
  lazy
    (let name = "lg-tests-" ^ string_of_int (Unix.getpid ()) in
     let dir = Filename.concat (Filename.get_temp_dir_name ()) name in
     if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
     dir)

let test_dir () = Lazy.force test_directory

let compile_only_command dir ml_path =
  Printf.sprintf "cd %s && ocamlc -I %s -I %s -I %s -I %s -I %s -I %s -c %s"
    (Filename.quote dir)
    (Filename.quote (rrbvec_build_dir ()))
    (Filename.quote (rrbvec_cmi_dir ()))
    (Filename.quote (lg_build_dir ()))
    (Filename.quote (lg_byte_cmi_dir ()))
    (Filename.quote (lg_runtime_build_dir ()))
    (Filename.quote (lg_runtime_byte_cmi_dir ()))
    (Filename.quote (Filename.basename ml_path))

let compile_and_run_command dir ml_path exe_path output_path =
  let compile_cmd =
    Printf.sprintf
      "cd %s && ocamlfind ocamlc -package re -linkpkg -I %s -I %s -I %s -I %s \
       -I %s -I %s -o %s %s %s %s %s"
      (Filename.quote dir)
      (Filename.quote (rrbvec_build_dir ()))
      (Filename.quote (rrbvec_cmi_dir ()))
      (Filename.quote (lg_build_dir ()))
      (Filename.quote (lg_byte_cmi_dir ()))
      (Filename.quote (lg_runtime_build_dir ()))
      (Filename.quote (lg_runtime_byte_cmi_dir ()))
      (Filename.quote (Filename.basename exe_path))
      (Filename.quote (rrbvec_cma ()))
      (Filename.quote (lg_runtime_cma ()))
      (Filename.quote (lg_cma ()))
      (Filename.quote (Filename.basename ml_path))
  in
  let run_cmd =
    Printf.sprintf "%s > %s" (Filename.quote exe_path)
      (Filename.quote output_path)
  in
  (compile_cmd, run_cmd)

let compile_job_immediately (job : compile_job) =
  let dir = test_dir () in
  let ml_path = Filename.concat dir (job.name ^ ".ml") in
  write_file ml_path job.ocaml_source;
  match Sys.command (compile_only_command dir ml_path) with
  | 0 -> ()
  | code ->
      failwith
        (Printf.sprintf "generated OCaml did not compile, exit code %d:\n%s"
           code job.ocaml_source)

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let length = in_channel_length ic in
      really_input_string ic length)

let run_job_immediately (job : run_job) =
  let dir = test_dir () in
  let ml_path = Filename.concat dir (job.name ^ ".ml") in
  let exe_path = Filename.concat dir job.name in
  let output_path = Filename.concat dir (job.name ^ ".out") in
  write_file ml_path job.ocaml_source;
  let compile_cmd, run_cmd =
    compile_and_run_command dir ml_path exe_path output_path
  in
  match Sys.command compile_cmd with
  | code when code <> 0 ->
      failwith
        (Printf.sprintf "generated OCaml did not compile, exit code %d:\n%s"
           code job.ocaml_source)
  | _ -> (
      match Sys.command run_cmd with
      | code when code <> 0 ->
          failwith
            (Printf.sprintf "generated executable failed, exit code %d:\n%s"
               code job.ocaml_source)
      | _ ->
          let actual = read_file output_path in
          assert_equal_string job.expected_output actual)

let wrapped_module index source =
  Printf.sprintf "module Case_%04d = struct\n%s\nend\n" index source

let flush_compile_jobs (jobs : compile_job list) =
  match jobs with
  | [] -> ()
  | _ -> (
      let dir = test_dir () in
      let ml_path = Filename.concat dir "compile_batch.ml" in
      let source =
        jobs
        |> List.mapi (fun index (job : compile_job) ->
               wrapped_module index job.ocaml_source)
        |> String.concat "\n"
      in
      write_file ml_path source;
      match Sys.command (compile_only_command dir ml_path) with
      | 0 -> ()
      | code ->
          List.iter compile_job_immediately jobs;
          failwith
            (Printf.sprintf
               "batched generated OCaml failed with exit code %d, but isolated \
                cases passed"
               code))

let run_marker phase index = Printf.sprintf "__LG_TEST_%s_%04d__\n" phase index

let wrapped_run_module index (job : run_job) =
  Printf.sprintf "let () = print_string %S\n%slet () = print_string %S\n"
    (run_marker "BEGIN" index)
    (wrapped_module index job.ocaml_source)
    (run_marker "END" index)

let flush_run_jobs (jobs : run_job list) =
  match jobs with
  | [] -> ()
  | _ -> (
      let dir = test_dir () in
      let ml_path = Filename.concat dir "run_batch.ml" in
      let exe_path = Filename.concat dir "run_batch" in
      let output_path = Filename.concat dir "run_batch.out" in
      let source = jobs |> List.mapi wrapped_run_module |> String.concat "\n" in
      let expected =
        jobs
        |> List.mapi (fun index job ->
               run_marker "BEGIN" index ^ job.expected_output
               ^ run_marker "END" index)
        |> String.concat ""
      in
      write_file ml_path source;
      let compile_cmd, run_cmd =
        compile_and_run_command dir ml_path exe_path output_path
      in
      let fallback message =
        List.iter run_job_immediately jobs;
        failwith message
      in
      match Sys.command compile_cmd with
      | code when code <> 0 ->
          fallback
            (Printf.sprintf
               "batched generated OCaml failed with exit code %d, but isolated \
                cases passed"
               code)
      | _ -> (
          match Sys.command run_cmd with
          | code when code <> 0 ->
              fallback
                (Printf.sprintf
                   "batched generated executable failed with exit code %d, but \
                    isolated cases passed"
                   code)
          | _ ->
              let actual = read_file output_path in
              if actual <> expected then
                fallback
                  "batched generated output differed, but isolated cases passed"
          ))

let assert_ocaml_compiles name ocaml_source =
  pending_compile_jobs := { name; ocaml_source } :: !pending_compile_jobs

let assert_ocaml_runs name expected_output ocaml_source =
  pending_run_jobs :=
    { name; expected_output; ocaml_source } :: !pending_run_jobs

let flush_ocaml_jobs () =
  let compile_jobs = List.rev !pending_compile_jobs in
  let run_jobs = List.rev !pending_run_jobs in
  pending_compile_jobs := [];
  pending_run_jobs := [];
  time_phase "generated compile batch" (fun () ->
      flush_compile_jobs compile_jobs);
  time_phase "generated run batch" (fun () -> flush_run_jobs run_jobs)

let test_records_assoc_and_dissoc () =
  let source =
    {|
(def x {:name "Ada", :age 36})
(def y (assoc x :admin? true))
(def z (dissoc y :age))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_compiles "records_assoc_and_dissoc" ocaml_source

let test_assoc_rejects_type_changes () =
  let source =
    {|
(def x {:name "Ada", :age 36})
(def y (assoc x :age "old"))
|}
  in
  Lg.Compiler.compile_string source
  |> expect_error "cannot assoc :age as string because it is already int"

let test_dissoc_rejects_unknown_fields () =
  let source =
    {|
(def x {:name "Ada", :age 36})
(def y (dissoc x :admin?))
|}
  in
  Lg.Compiler.compile_string source
  |> expect_error "cannot dissoc unknown field :admin?"

let test_map_rejects_duplicate_fields () =
  let source = {|(def x {:name "Ada", :name "Grace"})|} in
  Lg.Compiler.compile_string source |> expect_error "duplicate field :name"

let test_hash_map_constructs_structural_maps () =
  let source =
    {|
(def user (hash-map :name "Ada" :age 36))
(println (str (:name user) ":" (:age user)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "hash_map_constructs_structural_maps" "Ada:36\n"
    ocaml_source

let test_hash_map_rejects_duplicate_fields () =
  Lg.Compiler.compile_string {|(def x (hash-map :name "Ada" :name "Grace"))|}
  |> expect_error "duplicate field :name"

let test_hash_map_rejects_odd_key_value_forms () =
  Lg.Compiler.compile_string {|(def x (hash-map :name "Ada" :age))|}
  |> expect_error "hash-map expects keyword/value pairs"

let test_map_literals_accept_computed_keys () =
  let source =
    {|
(defn nested [outer-key inner-key value]
  {outer-key {inner-key value}})
(def result (nested :outer :inner 42))
(println (get (get result :outer) :inner))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "map_literals_accept_computed_keys" "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_anonymous_maps_reuse_equal_shapes () =
  let source =
    {|
(def x {:a 1 :b "b"})
(def y {:a 2 :b "bbb"})
(def z (merge x y))
(println (count [x y z]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  if count_generated_anonymous_record_types ocaml_source <> 1 then
    failwith "equal anonymous map shapes must emit one OCaml record type";
  assert_ocaml_runs "anonymous_maps_reuse_equal_shapes" "3\n" ocaml_source

let test_anonymous_map_shape_ignores_field_order () =
  let source =
    {|
(def left {:a 1 :b "left"})
(def right {:b "right" :a 2})
(println (count [left right]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  if count_generated_anonymous_record_types ocaml_source <> 1 then
    failwith "anonymous map field order must not create a new type";
  assert_ocaml_runs "anonymous_map_shape_ignores_field_order" "2\n" ocaml_source

let test_anonymous_map_operations_reuse_result_shapes () =
  let source =
    {|
(def base {:a 1})
(def expanded (assoc base :b "expanded"))
(def literal {:b "literal" :a 2})
(def merged (merge base literal))
(def shrunk (dissoc literal :b))
(println (str (count [expanded literal merged]) ":" (count [base shrunk])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  if count_generated_anonymous_record_types ocaml_source <> 2 then
    failwith "map operations must reuse existing result shapes";
  assert_ocaml_runs "anonymous_map_operations_reuse_result_shapes" "3:2\n"
    ocaml_source

let test_module_local_anonymous_maps_reuse_equal_shapes () =
  let source =
    {|
(module Maps
  (def x {:a 1 :b "x"})
  (def y {:b "y" :a 2})
  (def values [x y])
  (defn size [] (count values)))
(println (Maps/size))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_local_anonymous_maps_reuse_equal_shapes" "2\n"
    ocaml_source

let test_incremental_anonymous_maps_reuse_equal_shapes () =
  let state, first =
    Lg.Compiler.compile_chunk Lg.Compiler.empty_state {|(def x {:a 1 :b "x"})|}
    |> expect_ok
  in
  let _state, second =
    Lg.Compiler.compile_chunk state
      {|
(def y {:b "y" :a 2})
(println (count [x y]))
|}
    |> expect_ok
  in
  assert_ocaml_runs "incremental_anonymous_maps_reuse_equal_shapes" "2\n"
    (first ^ "\n" ^ second)

let test_heterogeneous_record_vectors_use_dynamic_values () =
  let ocaml_source =
    Lg.Compiler.compile_string
    {|
(def integer-value {:value 1})
(def string-value {:value "one"})
(def values [integer-value string-value])
(println (count values))
|}
    |> expect_ok
  in
  assert_ocaml_runs "heterogeneous_record_vectors_use_dynamic_values" "2\n"
    ocaml_source

let test_declared_and_anonymous_records_share_dynamic_vectors () =
  let ocaml_source =
    Lg.Compiler.compile_string
    {|
(type-record user (name :string))
(def declared (record user (name "Ada")))
(def anonymous {:name "Ada"})
(def values [declared anonymous])
(println (count values))
|}
    |> expect_ok
  in
  assert_ocaml_runs "declared_and_anonymous_records_share_dynamic_vectors" "2\n"
    ocaml_source

let test_println_outputs_record_values () =
  let source =
    {|
(def x {:name "Ada", :age 36})
(def y (assoc x :admin? true))
(def z (dissoc y :age))
(println z)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "println_outputs_record_values"
    "{:name \"Ada\", :admin? true}\n" ocaml_source

let test_println_rejects_unknown_symbols () =
  Lg.Compiler.compile_string {|(println missing)|}
  |> expect_error "unknown symbol missing"

let test_print_and_println_match_clojure_output () =
  let source = {|
(print "a")
(print "b")
(println "c")
|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "print_and_println_match_clojure_output" "abc\n"
    ocaml_source

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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "core_api_if_and_vector_ops" "ok:1:3\n" ocaml_source

let test_boolean_core_api () =
  let source =
    {|(println (str (not false) ":" (true? true) ":" (false? false)))|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "boolean_core_api" "true:true:true\n" ocaml_source

let test_not_uses_static_clojure_truthiness () =
  let source =
    {|(println (str (not false) ":" (not 0) ":" (not "Ada") ":" (not [1])))|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "not_uses_static_clojure_truthiness"
    "true:false:false:false\n" ocaml_source

let test_nil_predicates_and_truthiness_use_options () =
  let source =
    {|
(def absent nil)
(def present (Some 7))
(defn missing? [value] (nil? value))
(defn present? [value] (some? value))
(println
  (str (nil? absent) ":" (some? absent) ":"
       (nil? present) ":" (some? present) ":"
       (nil? 1) ":" (some? 1) ":"
       (not absent) ":" (not present) ":"
       (missing? absent) ":" (missing? present) ":"
       (present? absent) ":" (present? present)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nil_predicates_and_truthiness_use_options"
    "true:false:false:true:false:true:true:false:true:false:false:true\n"
    ocaml_source

let test_if_some_and_when_some_bind_option_payloads () =
  let source =
    {|
(defn lookup [found?]
  (if found? (Some 7) nil))
(def found (if-some [value (lookup true)] (+ value 1) 0))
(def missing (if-some [value (lookup false)] (+ value 1) 0))
(when-some [value (lookup true)]
  (println (+ value 2)))
(when-some [value (lookup false)]
  (println (+ value 2)))
(println (str found ":" missing))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "if_some_and_when_some_bind_option_payloads" "9\n8:0\n"
    ocaml_source

let test_nil_predicates_evaluate_arguments_once () =
  let source =
    {|
(def calls (atom 0))
(println
  (nil?
    (do
      (reset! calls (+ (deref calls) 1))
      nil)))
(println (deref calls))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nil_predicates_evaluate_arguments_once" "true\n1\n"
    ocaml_source

let test_control_flow_lifts_nilable_branches () =
  let source =
    {|
(def present (if true 41 nil))
(def absent (if false 41 nil))
(def implicit-absent (if false 42))
(def when-present (when true 43))
(def when-absent (when false 43))
(println
  (str (if-some [value present] (+ value 1) 0) ":"
       (nil? absent) ":"
       (nil? implicit-absent) ":"
       (if-some [value when-present] value 0) ":"
       (nil? when-absent)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "control_flow_lifts_nilable_branches"
    "42:true:true:43:true\n" ocaml_source

let test_cond_uses_clojure_truthiness_and_implicit_nil () =
  let source =
    {|
(def selected (cond nil 1 "truthy" 2))
(def missing (cond false 1 nil 2))
(println (str selected ":" (nil? missing)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "cond_uses_clojure_truthiness_and_implicit_nil" "2:true\n"
    ocaml_source

let test_if_let_and_if_some_distinguish_false_from_nil () =
  let source =
    {|
(def value (if true false nil))
(println
  (str (if-let [bound value] :then :else) ":"
       (if-some [bound value] :then :else)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "if_let_and_if_some_distinguish_false_from_nil"
    ":else::then\n" ocaml_source

let test_nilable_vectors_lift_values_and_empty_vectors_are_polymorphic () =
  let source =
    {|
(def empty-values [])
(def numbers (conj empty-values 42))
(def maybe-numbers [1 nil])
(println
  (str (first numbers) ":" (count maybe-numbers) ":"
       (nil? (get maybe-numbers 1))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "nilable_vectors_lift_values_and_empty_vectors_are_polymorphic"
    "42:2:true\n" ocaml_source

let test_logical_forms_lift_nilable_operands () =
  let source =
    {|
(def fallback (or nil 7))
(def missing (and 7 nil))
(println (str (if-some [value fallback] value 0) ":" (nil? missing)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "logical_forms_lift_nilable_operands" "7:true\n"
    ocaml_source

let test_when_bindings_return_nullable_body_values () =
  let source =
    {|
(def from-let (when-let [value (if true 3 nil)] (+ value 1)))
(def from-some (when-some [value (if false 3 nil)] (+ value 1)))
(println
  (str (if-some [value from-let] value 0) ":" (nil? from-some)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "when_bindings_return_nullable_body_values" "4:true\n"
    ocaml_source

let test_nil_type_annotation_remains_explicitly_unsupported () =
  Lg.Compiler.compile_string {|(defn bad [^:nil x] x)|}
  |> expect_error "unknown parameter type ^:nil"

let test_type_predicates () =
  let source =
    {|
(println
  (str (int? 1) ":" (string? "Ada") ":" (keyword? :name) ":" (boolean? true) ":"
       (vector? [1]) ":" (list? (list 1)) ":" (set? (hash-set 1)) ":" (map? {:name "Ada"}) ":"
       (seq? (list 1)) ":" (seq? [1]) ":" (vector? (list 1)) ":" (map? [1])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "type_predicates"
    "true:true:true:true:true:true:true:true:true:false:false:false\n"
    ocaml_source

let test_type_predicates_reject_wrong_arity () =
  Lg.Compiler.compile_string {|(def x (vector? [1] [2]))|}
  |> expect_error "vector? expects 1 arguments"

let test_instance_predicate_supports_clojure_collection_interfaces () =
  let source =
    {|
(defn inspect [value]
  (str (instance? clojure.lang.Seqable value) ":"
       (instance? Iterable value) ":"
       (instance? java.util.Map value)))
(println (str (inspect [1]) ":" (inspect {:a 1}) ":" (inspect 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "instance_predicate_supports_clojure_collection_interfaces"
    "true:true:false:true:true:true:false:false:false\n" ocaml_source

let test_condp_selects_first_match_and_evaluates_target_once () =
  let source =
    {|
(def calls (atom 0))
(def result
  (condp = (do (swap! calls inc) 2)
    1 "one"
    2 "two"
    "other"))
(println (str result ":" (deref calls)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "condp_selects_first_match_and_evaluates_target_once"
    "two:1\n" ocaml_source

let test_subs_core_api () =
  let source =
    {|
(println (str (subs "clojure" 3) ":" (subs "clojure" 1 4)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "subs_core_api" "jure:loj\n" ocaml_source

let test_subs_rejects_non_string_sources () =
  Lg.Compiler.compile_string {|(def x (subs 123 1))|}
  |> expect_error "subs expects a string"

let test_subs_rejects_non_int_indexes () =
  Lg.Compiler.compile_string {|(def x (subs "abc" "1"))|}
  |> expect_error "subs indexes must be int"

let test_type_relations_are_explicit_and_strict () =
  if Lg.Types.equal Lg.Types.TUnknown Lg.Types.TInt then
    failwith "unknown must not be strictly equal to int";
  let name = Lg.Types.make_field ":name" Lg.Types.TString in
  let narrow = Lg.Types.TRecord [ name ] in
  let wide =
    Lg.Types.TRecord [ name; Lg.Types.make_field ":age" Lg.Types.TInt ]
  in
  if not (Lg.Types.row_compatible ~expected:narrow ~actual:wide) then
    failwith "wider structural records must remain row-compatible";
  let generated =
    Lg.Types.named_record ~type_name:"t1" ~set_module_name:"Set_t1"
      [ name; Lg.Types.make_field ":age" Lg.Types.TInt ]
  in
  if not (Lg.Types.row_compatible ~expected:narrow ~actual:generated) then
    failwith "generated records must remain row-compatible with structural rows";
  if
    not
      (Lg.Types.defer_to_ocaml ~expected:(Lg.Types.TOcaml "user_id")
         ~actual:Lg.Types.TInt)
  then failwith "opaque OCaml relationships must be explicitly deferred"

let test_dynamic_record_capabilities_resolve_unique_named_records () =
  let open Lg.Types in
  let max_eid = make_field ":max-eid" TInt in
  let db_ty =
    named_record ~type_name:"db" ~set_module_name:"Db_set"
      [ make_field ":root" (TRef TInt); max_eid ]
  in
  let env =
    Lg.Compiler_environment.add "__record//DB" (binding "db" db_ty)
      Lg.Compiler_environment.empty
  in
  let inferred =
    Lg.Function_elaborator.infer_named_record "" env
      (dynamic_constraint (TRecord [ max_eid ]))
  in
  match inferred with
  | TNamed_record record when record.type_name = "db" -> ()
  | _ ->
      failwith
        "a unique named record must be recovered from a dynamic row capability"

let test_assignability_reports_the_selected_semantic_rule () =
  let open Lg.Types in
  let name = make_field ":name" TString in
  let narrow = TRecord [ name ] in
  let wide = TRecord [ name; make_field ":age" TInt ] in
  let expect expected actual =
    if actual <> expected then
      failwith "unexpected assignability classification"
  in
  expect Equal (classify_assignability ~expected:TInt ~actual:TInt);
  expect Unknown (classify_assignability ~expected:TUnknown ~actual:TInt);
  expect Row_compatible (classify_assignability ~expected:narrow ~actual:wide);
  expect Deferred_to_ocaml
    (classify_assignability ~expected:(TOcaml "user_id") ~actual:TInt);
  expect Incompatible (classify_assignability ~expected:TString ~actual:TInt);
  if assignable ~policy:Nominal ~expected:narrow ~actual:wide then
    failwith "nominal assignment must not accept structural width";
  if not (assignable ~policy:Structural ~expected:narrow ~actual:wide) then
    failwith "structural assignment should accept wider records";
  if assignable ~policy:Structural ~expected:(TOcaml "user_id") ~actual:TInt
  then failwith "structural assignment must not defer to OCaml";
  if
    not
      (assignable ~policy:Host_boundary ~expected:(TOcaml "user_id")
         ~actual:TInt)
  then failwith "host boundary assignment should defer to OCaml";
  if assignable ~policy:Nominal ~expected:TInt ~actual:TUnknown then
    failwith "nominal assignment must not accept unresolved types";
  if assignable ~policy:Structural ~expected:TUnknown ~actual:TInt then
    failwith "structural assignment must not accept top-level unresolved types";
  if not (assignable ~policy:Host_boundary ~expected:TUnknown ~actual:TInt) then
    failwith "host boundaries may defer unresolved types to OCaml"

let test_named_records_use_nominal_type_identity () =
  let fields = [ Lg.Types.make_field ":name" Lg.Types.TString ] in
  let user_id = Lg.Type_id.create ~owner:[ "Domain" ] ~name:"user" in
  let project_id = Lg.Type_id.create ~owner:[ "Domain" ] ~name:"project" in
  let user =
    Lg.Types.named_record ~type_id:user_id ~type_name:"Domain.user"
      ~set_module_name:"Domain.User_set" fields
  in
  let project =
    Lg.Types.named_record ~type_id:project_id ~type_name:"Domain.project"
      ~set_module_name:"Domain.Project_set" fields
  in
  if Lg.Types.equal user project then
    failwith "same-shaped named records must remain nominally distinct"

let test_declared_type_ids_preserve_source_identity () =
  let state =
    typecheck_state
      {|
(module Domain
  (type-record user-profile (name :string)))
|}
  in
  let record =
    Lg.Resolver.lookup_record_type "" state.env "Domain.user-profile"
    |> expect_ok
  in
  if Lg.Type_id.to_string record.type_id <> "Domain/user-profile" then
    failwith "declared Type_id must preserve source ownership and spelling";
  match
    Lg.Type_registry.find_by_emitted_name "Domain.user_profile"
      (Lg.Compiler_environment.types state.env)
  with
  | Some declaration when Lg.Type_id.equal declaration.type_id record.type_id ->
      ()
  | _ -> failwith "module type declarations must survive in the typed registry"

let test_type_namespace_rejects_emitted_name_collisions () =
  Lg.Compiler.compile_string
    {|
(type-alias user-profile :int)
(type-record user_profile (name :string))
|}
  |> expect_error_contains "OCaml type name collision";
  Lg.Compiler.compile_string
    {|
(type-variant status Active)
(type-alias status :int)
|}
  |> expect_error "duplicate type status"

let test_compiler_identities_are_stable_and_distinct () =
  let symbol = Lg.Symbol_id.create ~owner:[ "Domain" ] ~name:"value" in
  let same_symbol = Lg.Symbol_id.create ~owner:[ "Domain" ] ~name:"value" in
  let protocol = Lg.Protocol_id.create ~owner:[ "Domain" ] ~name:"Labelled" in
  if not (Lg.Symbol_id.equal symbol same_symbol) then
    failwith "symbol identity must be stable for the same owner and name";
  if Lg.Symbol_id.to_string symbol <> "Domain/value" then
    failwith "symbol identity must preserve its qualified source name";
  if Lg.Protocol_id.to_string protocol <> "Domain/Labelled" then
    failwith "protocol identity must preserve its qualified source name"

let test_typed_protocol_and_module_registries () =
  let protocol = Lg.Protocol_id.create ~owner:[ "Domain" ] ~name:"Labelled" in
  let method_id =
    Lg.Method_id.create ~owner:[ "Domain"; "Labelled" ] ~name:"label"
  in
  let signature : Lg.Protocol_registry.method_signature =
    {
      method_id;
      param_tys = [ Lg.Types.TUnknown ];
      return_ty = Lg.Types.TString;
    }
  in
  let registry =
    Lg.Protocol_registry.declare protocol [ signature ]
      Lg.Protocol_registry.empty
    |> expect_ok
  in
  (match Lg.Protocol_registry.find_method protocol method_id registry with
  | Some found when found.return_ty = Lg.Types.TString -> ()
  | _ -> failwith "typed protocol method lookup failed");
  (match Lg.Protocol_registry.declare protocol [ signature ] registry with
  | Error _ -> ()
  | Ok _ -> failwith "duplicate protocol declarations must be rejected");
  let binding =
    Lg.Types.binding "label_int"
      (Lg.Types.TFn ([ Lg.Types.TInt ], Lg.Types.TString))
  in
  let registry =
    Lg.Protocol_registry.add_implementation protocol method_id
      Lg.Protocol_registry.Int_receiver binding registry
    |> expect_ok
  in
  (match
     Lg.Protocol_registry.find_implementation protocol method_id
       Lg.Protocol_registry.Int_receiver registry
   with
  | Some found when found.ocaml_name = "label_int" -> ()
  | _ -> failwith "typed protocol implementation lookup failed");
  let module_id = Lg.Module_id.create ~owner:[] ~name:"Users" in
  let signature_id = Lg.Signature_id.create ~owner:[] ~name:"Printable" in
  let functor_id = Lg.Functor_id.create ~owner:[] ~name:"Make" in
  let modules =
    Lg.Module_registry.empty
    |> Lg.Module_registry.declare_signature signature_id []
    |> expect_ok
    |> Lg.Module_registry.store_functor_result functor_id [ ("value", binding) ]
    |> Lg.Module_registry.add_alias module_id module_id
  in
  if Lg.Module_registry.find_signature signature_id modules <> Some [] then
    failwith "typed signature lookup failed";
  if
    Lg.Module_registry.find_functor_result functor_id modules
    <> Some [ ("value", binding) ]
  then failwith "typed functor result lookup failed"

let test_protocol_elaboration_populates_typed_registry () =
  let state =
    typecheck_state {|
(defprotocol Labelled (label [x] :string))
|}
  in
  let protocol = Lg.Protocol_id.create ~owner:[] ~name:"Labelled" in
  let method_id = Lg.Method_id.create ~owner:[ "Labelled" ] ~name:"label" in
  match
    Lg.Protocol_registry.find_method protocol method_id
      (Lg.Compiler_environment.protocols state.env)
  with
  | Some signature when signature.return_ty = Lg.Types.TString -> ()
  | _ -> failwith "defprotocol must populate the typed protocol registry"

let test_protocol_implementation_populates_typed_registry () =
  let state =
    typecheck_state
      {|
(defprotocol Labelled (label [x] :string))
(extend-type :int Labelled (label [x] (str x)))
|}
  in
  let protocol = Lg.Protocol_id.create ~owner:[] ~name:"Labelled" in
  let method_id = Lg.Method_id.create ~owner:[ "Labelled" ] ~name:"label" in
  match
    Lg.Protocol_registry.find_implementation protocol method_id
      Lg.Protocol_registry.Int_receiver
      (Lg.Compiler_environment.protocols state.env)
  with
  | Some binding when binding.ocaml_name <> "" -> ()
  | _ -> failwith "extend-type must populate the typed protocol registry"

let test_module_protocols_preserve_typed_registry_state () =
  let state =
    typecheck_state
      {|
(module Labels
  (defprotocol Labelled (label [x] :string))
  (extend-type :int Labelled (label [x] (str x))))
|}
  in
  let protocol = Lg.Protocol_id.create ~owner:[ "Labels" ] ~name:"Labelled" in
  let method_id =
    Lg.Method_id.create ~owner:[ "Labels"; "Labelled" ] ~name:"label"
  in
  let protocols = Lg.Compiler_environment.protocols state.env in
  if
    Lg.Protocol_registry.find_method protocol method_id protocols = None
    || Lg.Protocol_registry.find_implementation protocol method_id
      Lg.Protocol_registry.Int_receiver protocols
    = None
  then failwith "module compilation must preserve typed protocol registry state"

let test_module_elaboration_populates_typed_registry () =
  let state =
    typecheck_state
      {|
(module-signature MathSig (val answer :int))
(module Math (def answer 42))
(module-alias M Math)
(module-functor Make [Input MathSig] (def result Input/answer))
|}
  in
  let modules = Lg.Compiler_environment.modules state.env in
  let signature = Lg.Signature_id.create ~owner:[] ~name:"MathSig" in
  let alias = Lg.Module_id.create ~owner:[] ~name:"M" in
  let target = Lg.Module_id.create ~owner:[] ~name:"Math" in
  let functor_id = Lg.Functor_id.create ~owner:[] ~name:"Make" in
  if Lg.Module_registry.find_signature signature modules = None then
    failwith "module-signature must populate the typed module registry";
  if Lg.Module_registry.find_alias alias modules <> Some target then
    failwith "module-alias must populate the typed module registry";
  if Lg.Module_registry.find_functor_result functor_id modules = None then
    failwith "module-functor must populate the typed module registry"

let test_module_metadata_does_not_use_encoded_symbol_keys () =
  let state =
    typecheck_state
      {|
(module-signature MathSig (val answer :int))
(module-functor Make [Input MathSig] (def result Input/answer))
|}
  in
  let encoded =
    Lg.Compiler_environment.to_bindings state.env
    |> List.find_opt (fun (key, _) ->
           String.starts_with ~prefix:"__signature/" key
           || String.starts_with ~prefix:"__functor/" key
           || String.starts_with ~prefix:"__functor_record/" key)
  in
  if encoded <> None then
    failwith "module metadata must not be encoded as symbol-table keys"

let test_protocol_metadata_does_not_use_encoded_symbol_keys () =
  let state =
    typecheck_state
      {|
(defprotocol Labelled (label [x] :string))
(extend-type :int Labelled (label [x] (str x)))
|}
  in
  let encoded =
    Lg.Compiler_environment.to_bindings state.env
    |> List.find_opt (fun (key, _) ->
           String.ends_with ~suffix:"$protocol" key
           || String.starts_with ~prefix:"__protocol_impl/" key)
  in
  if encoded <> None then
    failwith "protocol metadata must not be encoded as symbol-table keys"

let test_emitted_ocaml_names_reject_source_collisions () =
  Lg.Compiler.compile_string {|
(def foo-bar 1)
(def foo_bar 2)
|}
  |> expect_error_contains
       "OCaml name collision: foo-bar and foo_bar both emit foo_bar";
  Lg.Compiler.compile_string
    {|
(module Values
  (def active? true)
  (def active_ false))
|}
  |> expect_error_contains
       "OCaml name collision: active? and active_ both emit active_"

let test_module_namespace_rejects_emitted_name_collisions () =
  Lg.Compiler.compile_string
    {|
(module foo-bar (def value 1))
(module foo_bar (def value 2))
|}
  |> expect_error_contains "OCaml module name collision";
  Lg.Compiler.compile_string
    {|
(module Target (def value 1))
(module Existing (def value 2))
(module-alias Existing Target)
|}
  |> expect_error "duplicate module Existing";
  Lg.Compiler.compile_string
    {|
(module-signature Input (val value :int))
(module Make (def value 1))
(module-functor Make [M Input] (def result M/value))
|}
  |> expect_error "duplicate module Make";
  Lg.Compiler.compile_string
    {|
(module-signature Input (val value :int))
(module Value Input (def value 1))
(module-functor Make [M Input] (def result M/value))
(module Existing (def result 0))
(module-apply Existing Make Value)
|}
  |> expect_error "duplicate module Existing"

let test_signature_namespace_rejects_emitted_name_collisions () =
  Lg.Compiler.compile_string
    {|
(module-signature value-sig (val value :int))
(module-signature value_sig (val value :int))
|}
  |> expect_error_contains "OCaml module type name collision";
  Lg.Compiler.compile_string
    {|
(module-signature ValueSig (val value :int))
(module-signature ValueSig (val other :int))
|}
  |> expect_error "duplicate module signature ValueSig"

let test_typed_environment_respects_lexical_shadowing () =
  let source =
    {|
(def value 1)
(defn shout [^:string value] (str value "!"))
(println (shout "Ada"))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_environment_respects_lexical_shadowing" "Ada!\n"
    ocaml_source

let test_typed_environment_replaces_top_level_bindings () =
  let source = {|
(def value 1)
(def value "Ada")
(println value)
|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_environment_replaces_top_level_bindings" "Ada\n"
    ocaml_source

let test_compiler_phases_have_explicit_boundaries () =
  let state = Lg.Compiler_state.empty in
  if Lg.Compiler_environment.to_bindings state.env <> [] then
    failwith "compiler state should start with an empty environment";
  let binding = Lg.Types.binding "value" Lg.Types.TInt in
  let env = Lg.Compiler_environment.add "value" binding state.env in
  let resolved = Lg.Resolver.lookup_binding "" env "value" |> expect_ok in
  if resolved.ty <> Lg.Types.TInt then
    failwith "resolver should return the typed binding";
  ignore (Lg.Lowering.structure_of_located_items []);
  let expression =
    Lg.Expression_elaborator.compile_expr "" Lg.Compiler_environment.empty
      (Lg.Ast.FInt 1)
      |> expect_ok
    in
    if expression.ty <> Lg.Types.TInt then
      failwith "expression elaboration should have one owner";
    match
    Lg.Top_level_elaborator.compile "" Lg.Compiler_environment.empty 1
      (Lg.Ast.FInt 1)
      |> expect_ok
    with
  | _, _, _, Lg.Lowered.Value_binding _ -> (
        if
          not
          (Lg.Expression_support.branch_types_compatible Lg.Types.TInt
             Lg.Types.TInt)
        then failwith "expression semantic helpers should have one owner";
        let parts : Lg.Expression_support.compiled_fn_parts =
          {
          param_bindings = [ ("x", Lg.Types.binding "x" Lg.Types.TInt) ];
            param_identities = [ None ];
            destructured_bindings = [];
          body = Lg.Types.typed_ir Lg.Types.TInt (Lg.Semantic_ir.Ident "x");
          }
        in
        let fn = Lg.Function_elaborator.fn_code parts in
        if fn.ty <> Lg.Types.TFn ([ Lg.Types.TInt ], Lg.Types.TInt) then
          failwith "function elaboration should have one owner";
        let conditional =
          let operations =
            Lg.Special_form_elaborator.create
              ~compile_expr:Lg.Expression_elaborator.compile_expr
          in
        operations.compile_if "" Lg.Compiler_environment.empty
          (Lg.Ast.FBool true) (Lg.Ast.FInt 1) (Lg.Ast.FInt 2)
          |> expect_ok
        in
        if conditional.ty <> Lg.Types.TInt then
          failwith "special-form elaboration should have one owner";
        let call =
          let operations =
            Lg.Call_elaborator.create
              ~compile_expr:Lg.Expression_elaborator.compile_expr
          in
        operations.compile_call "" Lg.Compiler_environment.empty "inc"
          [ Lg.Ast.FInt 1 ]
          |> expect_ok
        in
        if call.ty <> Lg.Types.TInt then
          failwith "call elaboration should have one owner";
        let list =
          let operations =
            Lg.Collection_operation_elaborator.create
              ~compile_expr:Lg.Expression_elaborator.compile_expr
            ~pack_dynamic_value:(fun _env _expected value ->
              Ok value.Lg.Types.semantic_expr)
            ~dynamic_unpack:(fun _env _expected expression -> Ok expression)
          in
        operations.compile_list "" Lg.Compiler_environment.empty
            [ Lg.Ast.FInt 1; Lg.Ast.FInt 2 ]
          |> expect_ok
        in
        if list.ty <> Lg.Types.TList Lg.Types.TInt then
          failwith "collection operation elaboration should have one owner";
        let identity =
          let operations =
            Lg.Function_combinator_elaborator.create
              ~compile_expr:Lg.Expression_elaborator.compile_expr
              ~dynamic_unpack:(fun _ _ expression -> Ok expression)
          in
        operations.compile_identity "" Lg.Compiler_environment.empty
          [ Lg.Ast.FInt 1 ]
          |> expect_ok
        in
      if identity.ty <> Lg.Types.TInt then
        failwith "core higher-order call elaboration should have one owner";
        let context =
          Lg.Elaboration_context.create
            ~compile_expr:Lg.Expression_elaborator.compile_expr
        in
        let special_forms = context.special_forms in
        if special_forms != context.special_forms then
          failwith "elaboration domains should be initialized once";
        let conditional =
          special_forms.compile_if "" Lg.Compiler_environment.empty
            (Lg.Ast.FBool true) (Lg.Ast.FInt 1) (Lg.Ast.FInt 2)
          |> expect_ok
        in
        if conditional.ty <> Lg.Types.TInt then
          failwith "typed elaboration context should route special forms";
        let calls = context.calls in
        if calls != context.calls then
          failwith "call elaboration should be initialized once";
        let result =
          calls.compile_call "" Lg.Compiler_environment.empty "inc"
            [ Lg.Ast.FInt 1 ]
          |> expect_ok
        in
        if result.ty <> Lg.Types.TInt then
          failwith "typed elaboration context should route calls";
        let scalar =
        Lg.Expression_elaborator.compile_expr "" Lg.Compiler_environment.empty
          (Lg.Ast.FInt 7)
          |> expect_ok
        in
        (match Lg.Semantic_ir.unlocated scalar.semantic_expr with
        | Lg.Semantic_ir.Int 7 -> ()
        | _ -> failwith "typed expressions should carry semantic AST nodes");
      match Lg.Lowering.expression scalar.semantic_expr with
        | Lg.Ocaml_ir.Int 7 -> ()
        | _ -> failwith "semantic lowering should produce backend IR")
    | _ -> failwith "top-level elaboration should have one owner"

let test_semantic_ast_preserves_nested_types () =
  let expression =
    Lg.Expression_elaborator.compile_expr "" Lg.Compiler_environment.empty
      (Lg.Ast.FList [ Lg.Ast.FSymbol "+"; Lg.Ast.FInt 1; Lg.Ast.FInt 2 ])
    |> expect_ok
  in
  let annotations = Lg.Semantic_ir.type_annotations expression.semantic_expr in
  if annotations <> [ Lg.Types.TInt; Lg.Types.TInt; Lg.Types.TInt ] then
    failwith "semantic AST must preserve parent and child expression types"

let test_source_node_identity_reaches_parsetree () =
  let source = "(def answer (+ 1 2))" in
  let structure =
    Lg.Compiler.compile_parsetree_with_filename ~filename:"identity.lgc" source
    |> expect_ok
  in
  let node_ids = ref [] in
  let iterator =
    {
      Ast_iterator.default_iterator with
      expr =
        (fun self expression ->
          List.iter
            (fun ({ Parsetree.attr_name = { txt; _ }; _ } : Parsetree.attribute)
               -> if txt = "lg.node_id" then node_ids := txt :: !node_ids)
            expression.pexp_attributes;
          Ast_iterator.default_iterator.expr self expression);
    }
  in
  iterator.structure iterator structure;
  match !node_ids with
  | [] -> failwith "expected source node identities on lowered expressions"
  | _ -> (
      let analysis =
        Lg.Toolchain.analyze ~filename:"identity.lgc" source |> expect_ok
      in
      let typed_node_ids = ref 0 in
      let iterator =
        {
          Tast_iterator.default_iterator with
          expr =
            (fun self expression ->
              List.iter
                (fun ({ Parsetree.attr_name = { txt; _ }; _ } :
                       Parsetree.attribute) ->
                  if txt = "lg.node_id" then incr typed_node_ids)
                expression.exp_attributes;
              Tast_iterator.default_iterator.expr self expression);
        }
      in
      iterator.structure iterator analysis.typed_structure;
      if !typed_node_ids = 0 then
        failwith "expected source node identities on typed expressions";
      let language_analysis =
        Lg.Language_service.analyze ~filename:"identity.lgc" source |> expect_ok
      in
      let offset = expect_substring_index source "1" in
      match Lg.Language_service.source_node_id_at language_analysis ~offset with
      | Some id when String.starts_with ~prefix:"identity.lgc:" id -> ()
      | Some id -> failwith ("unexpected source node identity " ^ id)
      | None -> failwith "expected LSP lookup to return a source node identity")

let test_source_node_identity_covers_value_bindings () =
  let source = "(def answer 42)" in
  let analysis =
    Lg.Language_service.analyze ~filename:"binding-identity.lgc" source
    |> expect_ok
  in
  let offset = expect_substring_index source "answer" in
  match Lg.Language_service.source_node_id_at analysis ~offset with
  | Some id when String.starts_with ~prefix:"binding-identity.lgc:" id -> ()
  | Some id -> failwith ("unexpected binding source node identity " ^ id)
  | None -> failwith "expected value binding to preserve source node identity"

let test_source_node_identity_covers_record_value_bindings () =
  let filename = "record-binding-identity.lgc" in
  let source = "(def user {:name \"Ada\"})" in
  let analysis = Lg.Language_service.analyze ~filename source |> expect_ok in
  let offset = expect_substring_index source "user" in
  match Lg.Language_service.source_node_id_at analysis ~offset with
  | Some id
    when Lg.Language_service.source_node_id_range id = Some (offset, offset + 4)
    ->
      ()
  | _ -> failwith "record value binding must preserve exact source identity"

let expect_source_id_at_text filename source analysis text =
  let offset = expect_substring_index source text in
  match Lg.Language_service.source_node_id_at analysis ~offset with
  | Some id
    when Lg.Language_service.source_node_id_range id
         = Some (offset, offset + String.length text)
         && String.starts_with ~prefix:(filename ^ ":") id ->
      ()
  | Some id -> failwith ("unexpected source node identity " ^ id)
  | None -> failwith ("expected source node identity at " ^ text)

let test_source_node_identity_covers_recursive_bindings () =
  let filename = "recursive-identity.lgc" in
  let source =
    "(defn countdown [^:int n] :int\n  (if (= n 0) 0 (countdown (dec n))))"
  in
  let analysis = Lg.Language_service.analyze ~filename source |> expect_ok in
  expect_source_id_at_text filename source analysis "countdown"

let test_source_node_identity_covers_function_parameters () =
  let filename = "parameter-identity.lgc" in
  let source = "(defn add-one [value] (+ value 1))" in
  let analysis = Lg.Language_service.analyze ~filename source |> expect_ok in
  expect_source_id_at_text filename source analysis "value"

let test_source_node_identity_covers_annotated_parameters () =
  let filename = "annotated-parameter-identity.lgc" in
  let source = "(defn increment [^:int value] (+ value 1))" in
  let analysis = Lg.Language_service.analyze ~filename source |> expect_ok in
  expect_source_id_at_text filename source analysis "value"

let test_source_node_identity_covers_destructuring_bindings () =
  let filename = "destructuring-identity.lgc" in
  let source =
    {|
(defn summarize [[first & rest :as all]]
  (str first ":" (count rest) ":" (count all)))
(defn label [{:keys [name] :as person}]
  (str name ":" (count person)))
|}
  in
  let analysis = Lg.Language_service.analyze ~filename source |> expect_ok in
  List.iter
    (expect_source_id_at_text filename source analysis)
    [ "first"; "rest"; "all"; "name"; "person" ]

let test_source_node_identity_covers_let_bindings () =
  let filename = "let-identity.lgc" in
  let source = "(def result (let [local 41] (+ local 1)))" in
  let analysis = Lg.Language_service.analyze ~filename source |> expect_ok in
  expect_source_id_at_text filename source analysis "local"

let test_source_node_identity_covers_let_destructuring () =
  let filename = "let-destructuring-identity.lgc" in
  let source =
    {|
(def user {:name "Ada"})
(def label
  (let [{:keys [name] :as person} user]
    (str name ":" (count person))))
|}
  in
  let analysis = Lg.Language_service.analyze ~filename source |> expect_ok in
  let name_search = "name] :as" in
  let name_offset = expect_substring_index source name_search in
  (match Lg.Language_service.source_node_id_at analysis ~offset:name_offset with
  | Some id
    when Lg.Language_service.source_node_id_range id
         = Some (name_offset, name_offset + 4) ->
      ()
  | _ -> failwith "expected exact identity for let-destructured name");
  expect_source_id_at_text filename source analysis "person"

let test_source_node_identity_covers_match_bindings () =
  let filename = "match-identity.lgc" in
  let source =
    {|
(type-variant message (Named :string))
(def label
  (match (Named "Ada")
    (as (Named value) whole) (str value ":" whole)))
|}
  in
  let analysis = Lg.Language_service.analyze ~filename source |> expect_ok in
  List.iter
    (expect_source_id_at_text filename source analysis)
    [ "value"; "whole" ]

let test_source_node_identity_covers_loop_bindings () =
  let filename = "loop-identity.lgc" in
  let source =
    "(def result (loop [counter 0] (if (= counter 2) counter (recur (inc \
     counter)))))"
  in
  let analysis = Lg.Language_service.analyze ~filename source |> expect_ok in
  expect_source_id_at_text filename source analysis "counter"

let test_source_node_identity_covers_catch_bindings () =
  let filename = "catch-identity.lgc" in
  let source =
    {|
(def result
  (try
    (raise (Failure "boom"))
    (catch (Failure message) (str "caught:" message))))
|}
  in
  let analysis = Lg.Language_service.analyze ~filename source |> expect_ok in
  expect_source_id_at_text filename source analysis "message"

let test_modules_resolve_qualified_symbols () =
  let source =
    {|
(module People
  (def user {:name "Ada"}))
(def label (str (get People/user :name) "!"))
(println label)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "modules_resolve_qualified_symbols" "Ada!\n" ocaml_source

let test_modules_prevent_unqualified_symbol_collisions () =
  let source =
    {|
(module First (def x 1))
(module Second (def x 2))
(println (str First/x ":" Second/x))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "modules_prevent_unqualified_symbol_collisions" "1:2\n"
    ocaml_source

let test_namespace_scopes_following_forms_without_ocaml_modules () =
  let source =
    {|
(ns app.math)
(def answer 40)
(defn add2 [x] (+ x 2))
(println (str answer ":" (add2 answer)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  if string_contains_substring ocaml_source "module App" then
    failwith "ns compatibility must not emit an OCaml module";
  assert_ocaml_runs "namespace_scopes_following_forms_without_ocaml_modules"
    "40:42\n" ocaml_source

let test_namespace_require_aliases_local_modules_across_files () =
  let state, math_ocaml =
    Lg.Compiler.compile_chunk Lg.Compiler.empty_state
      {|
(ns app.math)
(defn add [left right] (+ left right))
|}
    |> expect_ok
  in
  let _state, main_ocaml =
    Lg.Compiler.compile_chunk state
      {|
(ns app.main
  (:require [app.math :as math]))
(println (math/add 20 22))
|}
    |> expect_ok
  in
  assert_ocaml_runs "namespace_require_aliases_local_modules_across_files"
    "42\n"
    (math_ocaml ^ "\n" ^ main_ocaml)

let test_namespace_load_only_require_exposes_qualified_clojure_string () =
  let source =
    {|
(ns app.text
  (:require [clojure.string]))
(println (clojure.string/upper-case "ada"))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "namespace_load_only_require_exposes_qualified_clojure_string" "ADA\n"
    ocaml_source

let test_namespace_refer_clojure_exclude_allows_local_replacement () =
  let source =
    {|
(ns app.search
  (:refer-clojure :exclude [find]))
(defn find [value] (+ value 1))
(println (find 41))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "namespace_refer_clojure_exclude_allows_local_replacement"
    "42\n" ocaml_source

let test_namespace_refer_clojure_exclude_hides_core_binding () =
  Lg.Compiler.compile_string
    {|
(ns app.search
  (:refer-clojure :exclude [find]))
(def result (find (fn [value] true) [1]))
|}
  |> expect_error "unknown function find"

let test_namespace_accepts_host_import_clause () =
  let source = {|
(ns app.uuid
  (:import [java.util UUID]))
(println 42)
|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "namespace_accepts_host_import_clause" "42\n" ocaml_source

let test_namespace_accepts_qualified_host_import_symbol () =
  let source =
    {|
(ns app.uuid
  (:import java.util.UUID))
(def value (UUID/randomUUID))
(println (= value value))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "namespace_accepts_qualified_host_import_symbol" "true\n"
    ocaml_source

let test_namespace_drops_compile_time_only_host_import () =
  let source =
    {|
(ns app.macro-import
  (:import clojure.lang.IFn$OOL))
(defmacro passthrough [body]
  (let [_ (quote IFn$OOL)]
    body))
(println (passthrough 42))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "namespace_drops_compile_time_only_host_import" "42\n"
    ocaml_source

let test_namespace_rejects_runtime_unknown_host_import () =
  Lg.Compiler.compile_string
    {|
(ns app.runtime-import
  (:import missing.host.RuntimeClass))
(def value RuntimeClass/member)
|}
  |> expect_error "unsupported host import missing.host.RuntimeClass"

let test_namespace_ignores_clojure_compiler_directives () =
  let source =
    {|
(ns app.compiler-directives)
(set! *warn-on-reflection* true)
(println 42)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "namespace_ignores_clojure_compiler_directives" "42\n"
    ocaml_source

let test_host_import_type_hint_supports_instance_methods () =
  let source =
    {|
(ns app.uuid
  (:import [java.util UUID]))
(defn high [value]
  (.getMostSignificantBits ^UUID value))
(def value (UUID/randomUUID))
(println (= (high value) (high value)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "host_import_type_hint_supports_instance_methods" "true\n"
    ocaml_source

let test_system_current_time_millis_compiles_for_native () =
  let source =
    {|
(def now (System/currentTimeMillis))
(println (pos? now))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_compiles "system_current_time_millis_compiles_for_native"
    ocaml_source

let test_javascript_targets_compile_date_and_radix_interop () =
  let source =
    {|
(def text (.toString 255 16))
(def parsed (js/parseInt text 16))
(def now (.getTime (js/Date.)))
|}
  in
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source
    |> expect_ok)

let test_transient_collection_operations_preserve_values () =
  let source =
    {|
(defrecord Box [values])
(defrecord ReducerInput [values keys])
(def vector-values
  (persistent! (assoc! (conj! (transient [1]) 2) 0 3)))
(def set-values (persistent! (conj! (transient (hash-set 1)) 2)))
(def map-values
  (persistent!
    (dissoc! (transient (hash-map "a" 1 "b" 2)) "a")))
(defn remove-key [values]
  (persistent! (dissoc! (transient values) "a")))
(def inferred-map-values
  (remove-key (hash-map "a" 1 "b" 2)))
(def dynamic-map-values
  (persistent!
    (dissoc!
      (transient (:values (Box. (hash-map "a" 1 "b" 2))))
      "a")))
(defn remove-keys [input]
  (let [values (transient (:values input))
        remove-key (fn [values key] (dissoc! values key))
        values (reduce remove-key values (:keys input))]
    (persistent! values)))
(def reduced-map-values
  (remove-keys
    (ReducerInput. (hash-map "a" 1 "b" 2) ["a"])))
(println
  (str (= vector-values [3 2]) ":"
       (= set-values #{1 2}) ":"
       (= (get map-values "b") 2) ":"
       (= (count map-values) 1) ":"
       (= (count inferred-map-values) 1) ":"
       (= (get dynamic-map-values "b") 2) ":"
       (= (count dynamic-map-values) 1) ":"
       (= (count reduced-map-values) 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "transient_collection_operations_preserve_values"
    "true:true:true:true:true:true:true:true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_transient_operations_are_first_class_functions () =
  let source =
    {|
(def to-transient transient)
(def to-persistent persistent!)
(def values (to-persistent (conj! (to-transient [1]) 2)))
(println (= values [1 2]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "transient_operations_are_first_class_functions" "true\n"
    ocaml_source

let test_assert_accepts_optional_message () =
  let source =
    {|
(assert true)
(assert (= 1 1) "numbers differ")
(println 42)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "assert_accepts_optional_message" "42\n" ocaml_source

let test_into_cat_flattens_one_collection_level () =
  let source = {|
(println (= [1 2 3] (into [] cat [[1 2] [3]])))
|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "into_cat_flattens_one_collection_level" "true\n"
    ocaml_source

let test_mapv_vector_zips_multiple_collections () =
  let source =
    {|
(defn zip [a b & more]
  (apply mapv vector a b more))
(println
  (str (= [[1 4] [2 5]] (mapv vector [1 2 3] [4 5])) ":"
       (= [[1 4 7] [2 5 8]] (zip [1 2] [4 5] [7 8]))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "mapv_vector_zips_multiple_collections" "true:true\n"
    ocaml_source

let test_map_and_mapv_accept_multiple_collections () =
  let source =
    {|
(defn combine [left right]
  (mapv (fn [x y] (+ x y)) left right))
(println
  (str (pr-str (combine [1 2 3] [10 20])) ":"
       (pr-str (map (fn [x y] (- x y)) [10 20 30] [1 2]))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "map_and_mapv_accept_multiple_collections"
    "[11 22]:(9 18)\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_fnil_wraps_core_conj_with_default_collection () =
  let source =
    {|
(def conjv (fnil conj []))
(def conjs (fnil conj #{}))
(println (str (= [1] (conjv nil 1)) ":" (= #{1} (conjs nil 1))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "fnil_wraps_core_conj_with_default_collection" "true:true\n"
    ocaml_source

let test_top_level_definitions_accept_clojure_metadata () =
  let source =
    {|
(def ^:dynamic *answer* 41)
(defn ^:private increment [value] (+ value 1))
(println (increment *answer*))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "top_level_definitions_accept_clojure_metadata" "42\n"
    ocaml_source

let test_user_macros_expand_syntax_quote_and_unquote () =
  let source =
    {|
(defmacro choose [test then else]
  `(if ~test ~then ~else))
(println (choose true 42 0))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "user_macros_expand_syntax_quote_and_unquote" "42\n"
    ocaml_source

let test_user_macros_treat_host_classes_as_compile_time_values () =
  let source =
    {|
(defmacro host-class-equal? []
  (= java.lang.Boolean java.lang.Boolean))
(println (host-class-equal?))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "user_macros_treat_host_classes_as_compile_time_values"
    "true\n" ocaml_source

let test_user_macros_track_helpers_passed_as_values () =
  let source =
    {|
(defn emit [value]
  value)
(defmacro emit-first [value]
  (first (map emit [value])))
(println (emit-first 42))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "user_macros_track_helpers_passed_as_values" "42\n"
    ocaml_source

let test_user_macros_receive_portable_namespace_environment () =
  let source =
    {|
(defn cljs-env? [env]
  (boolean (:ns env)))
(defmacro portable [name]
  (if (cljs-env? &env)
    `(def ~(vary-meta name assoc :private true) 42)
    `(def ~name 0)))
(defmacro define-annotated [name]
  `(defn ~(vary-meta name identity) [] 7))
(portable ^:private answer)
(define-annotated ^number annotated)
(println (str answer ":" (annotated)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "user_macros_receive_portable_namespace_environment"
    "42:7\n" ocaml_source

let test_user_macros_support_collection_type_predicates () =
  let source =
    {|
(defmacro vector-form? [form]
  (vector? form))
(println (str (vector-form? [1]) ":" (vector-form? (1))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "user_macros_support_collection_type_predicates"
    "true:false\n" ocaml_source

let test_user_macros_can_emit_top_level_do_definitions () =
  let source =
    {|
(defmacro define-values []
  `(do
     (def first-value 20)
     (def second-value 22)))
(define-values)
(println (+ first-value second-value))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "user_macros_can_emit_top_level_do_definitions" "42\n"
    ocaml_source

let test_rand_int_uses_exclusive_positive_bound () =
  let source = {|(println (rand-int 1))|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "rand_int_uses_exclusive_positive_bound" "0\n" ocaml_source

let test_int_coerces_float_and_preserves_int () =
  let source =
    {|
(defn coerce [value] (int value))
(println (str (coerce 3.9) ":" (coerce 4)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "int_coerces_float_and_preserves_int" "3:4\n" ocaml_source

let test_namespace_rejects_malformed_and_repeated_forms () =
  Lg.Compiler.compile_string {|(ns)|}
  |> expect_error "ns expects a namespace symbol and optional clauses";
  Lg.Compiler.compile_string {|(ns :app)|}
  |> expect_error "ns expects a namespace symbol and optional clauses";
  Lg.Compiler.compile_string {|(ns app.one) (ns app.two)|}
  |> expect_error "ns may only appear once at the start of a file"

let test_datascript_schema_accepts_dynamic_keyword_or_string_values () =
  let source =
    {|
(defn system-keyword? [value]
  (and (or (keyword? value) (string? value))
       (if-let [ns (namespace (keyword value))]
         (= "db" ns)
         false)))
(println (str (system-keyword? :db/ident) ":"
              (system-keyword? "db/ident") ":"
              (system-keyword? :user/name)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "datascript_schema_accepts_dynamic_keyword_or_string_values"
    "true:true:false\n" ocaml_source

let test_datascript_schema_reads_regex_literals () =
  let source =
    {|
(require [clojure.string])
(println (first (clojure.string/split "db.install" #"\.")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "datascript_schema_reads_regex_literals" "db\n" ocaml_source

let test_re_matches_returns_clojure_match_values () =
  let source =
    {|
(println
  (str (pr-str (re-matches #"a+" "aaa")) ":"
       (pr-str (re-matches #"([a-z]+)-([0-9]+)" "abc-42")) ":"
       (pr-str (re-matches #"a+" "baaa"))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "re_matches_returns_clojure_match_values"
    "\"aaa\":[\"abc-42\" \"abc\" \"42\"]:nil\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_subs_accepts_guarded_dynamic_strings () =
  let source =
    {|
(let [[_ name] (re-matches #"([a-z]+)" "alpha")]
  (println (subs name 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "subs_accepts_guarded_dynamic_strings" "lpha\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_datascript_schema_reads_anonymous_functions_for_dynamic_contains () =
  let source =
    {|
(def schema-keys #{:db/ident :db/doc})
(defn schema-entity? [entity]
  (some #(contains? entity %) schema-keys))
(println (boolean (schema-entity? {:db/ident 1, :db/doc 2})))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "datascript_schema_reads_anonymous_functions_for_dynamic_contains" "true\n"
    ocaml_source

let test_top_level_require_imports_ocaml_modules () =
  let source =
    {|
(require [ocaml.String :as string])
(println (string/uppercase-ascii "ada"))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "top_level_require_imports_ocaml_modules" "ADA\n"
    ocaml_source

let test_top_level_require_rejects_unknown_lg_namespace () =
  Lg.Compiler.compile_string {|(require [people.core :as people])|}
  |> expect_error_contains "cannot require unknown namespace people.core"

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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_keyword_names_are_munged" "2:2:person:core\n"
    ocaml_source

let test_module_aliases_replace_legacy_import_aliases () =
  let source =
    {|
(module People (def user {:name "Ada"}))
(module-alias P People)
(println (get P/user :name))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_aliases_replace_legacy_import_aliases" "Ada\n"
    ocaml_source

let test_open_replaces_required_refer () =
  let source =
    {|
(module People
  (def user {:name "Ada"})
  (defn shout [^:string name] (str name "!")))
(open People)
(println (shout (:name user)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "open_replaces_required_refer" "Ada!\n" ocaml_source

let test_keyword_lookup_syntax () =
  let source =
    {|
(def user {:name "Ada", :age 36})
(println (str (:name user) ":" (:age user)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "keyword_lookup_syntax" "Ada:36\n" ocaml_source

let test_keyword_lookup_supports_typed_external_ocaml_records () =
  let ocaml_source =
    Lg.Compiler.compile_string
      {|
(defn incremented-file-size [^:Unix.stats value]
  (+ (:st-size value) 1))
|}
    |> expect_ok
  in
  if not (string_contains_substring ocaml_source ".st_size") then
    failwith "external OCaml record lookup should emit a native field access"

let test_keyword_lookup_delegates_unknown_external_fields_to_ocaml () =
  Lg.Compiler.compile_string
    {|
(defn bad-field [^:Unix.stats value]
  (:missing value))
|}
  |> expect_error_contains "no field missing"

let test_keyword_lookup_rejects_non_record_types () =
  Lg.Compiler.compile_string
    {|
(defn bad-field [^:int value]
  (:missing value))
|}
  |> expect_error_contains "get expects a map"

let test_typed_empty_vectors () =
  let source =
    {|
(def xs (vector-of :int))
(def ys (conj xs 42))
(println (str (empty? xs) ":" (count ys) ":" (first ys)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_empty_vectors" "true:1:42\n" ocaml_source

let test_vector_of_rejects_malformed_types () =
  Lg.Compiler.compile_string {|(def xs (vector-of :option<>))|}
  |> expect_error_contains "empty OCaml type"

let test_ocaml_module_require_aliases () =
  let source =
    {|
(require [ocaml.Stdlib :as std]
            [ocaml.String :as string])
(def label (str (string/uppercase-ascii "ada") ":" (std/string-of-int 42)))
(println label)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_module_require_aliases" "ADA:42\n" ocaml_source

let test_ocaml_module_require_refer () =
  let source =
    {|
(require [ocaml.Stdlib :refer [string-of-int]]
            [ocaml.String :refer [uppercase-ascii]])
(def label (str (uppercase-ascii "ada") ":" (string-of-int 42)))
(println label)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_function_parameters" "42:hi Ada::admin?!:3\n"
    ocaml_source

let test_unit_annotations_compile_through_source_backend () =
  let source =
    {|
(defn accept-unit [^:unit value]
  (do value (println "unit-ok")))
(accept-unit (run! (fn [^:int x] (println (str "item:" x))) [1]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "unit_annotations_compile_through_source_backend"
    "item:1\nunit-ok\n" ocaml_source

let test_host_owned_ocaml_type_annotations_compile () =
  let source = {|
(defn host-id [^:int x] x)
(def answer (host-id 42))
|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  if not (String.contains ocaml_source ':') then
    failwith "expected generated OCaml to contain a type constraint";
  assert_ocaml_runs "host_owned_ocaml_type_annotations_compile" "" ocaml_source

let test_generic_ocaml_calls_compile_through_source_backend () =
  let source =
    {|
(def answer (Stdlib.abs -42))
(def label (String.uppercase_ascii "ada"))
(println (str label ":" answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_ocaml_calls_compile_through_source_backend"
    "ADA:42\n" ocaml_source

let test_generic_ocaml_calls_accept_unit_return_type () =
  let source = {|
(def ignored (Stdlib.ignore 42))
(println "ignored")
|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_ocaml_calls_accept_unit_return_type" "ignored\n"
    ocaml_source

let test_generic_ocaml_calls_resolve_required_module_aliases () =
  let source =
    {|
(require [ocaml.Stdlib :as std]
            [ocaml.String :as string])
(def answer (std/abs -42))
(def label (string/uppercase_ascii "ada"))
(println (str label ":" answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_ocaml_calls_resolve_required_module_aliases"
    "ADA:42\n" ocaml_source

let test_generic_ocaml_calls_resolve_required_module_refers () =
  let source =
    {|
(require [ocaml.String :refer [uppercase_ascii]])
(def label (uppercase_ascii "ada"))
(println label)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_ocaml_calls_resolve_required_module_refers" "ADA\n"
    ocaml_source

let test_generic_ocaml_calls_resolve_required_module_refers_in_modules () =
  let source =
    {|
(require [ocaml.String :refer [uppercase_ascii]])
(module Greeter
  (def label (uppercase_ascii "ada")))
(println Greeter/label)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "generic_ocaml_calls_resolve_required_module_refers_in_modules" "ADA\n"
    ocaml_source

let test_generic_ocaml_calls_resolve_required_module_refers_in_functors () =
  let source =
    {|
(require [ocaml.String :refer [uppercase_ascii]])
(module-signature NameSig
  (val suffix :string))
(module Names NameSig
  (def suffix "!"))
(module-functor Make [M NameSig]
  (defn shout [name]
    (str (uppercase_ascii name) M/suffix)))
(module-apply App Make Names)
(println (App/shout "ada"))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "generic_ocaml_calls_resolve_required_module_refers_in_functors" "ADA!\n"
    ocaml_source

let test_typed_ocaml_refers_are_available_in_modules () =
  let source =
    {|
(require [ocaml.String :refer [uppercase-ascii]])
(module Greeter
  (def label (uppercase-ascii "ada")))
(println Greeter/label)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_ocaml_refers_are_available_in_modules" "ADA\n"
    ocaml_source

let test_typed_ocaml_refers_are_available_in_functors () =
  let source =
    {|
(require [ocaml.String :refer [uppercase-ascii]])
(module-signature NameSig
  (val suffix :string))
(module Names NameSig
  (def suffix "!"))
(module-functor Make [M NameSig]
  (defn shout [name]
    (str (uppercase-ascii name) M/suffix)))
(module-apply App Make Names)
(println (App/shout "ada"))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_ocaml_refers_are_available_in_functors" "ADA!\n"
    ocaml_source

let test_generic_ocaml_calls_resolve_opened_ocaml_modules () =
  let source =
    {|
(open String)
(def label (uppercase_ascii "ada"))
(println label)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_ocaml_calls_resolve_opened_ocaml_modules" "ADA\n"
    ocaml_source

let test_compile_string_runs_ocaml_typecheck_gate_for_host_calls () =
  Lg.Compiler.compile_string {|
(def answer (Stdlib.abs "bad"))
|}
  |> expect_error_contains "string"

let test_ocaml_errors_include_lg_source_locations () =
  Lg.Compiler.compile_string {|
(def ok 1)

(def answer (Stdlib.abs "bad"))
|}
  |> expect_error_contains "File \"<string>\", line 4"

let test_parsetree_items_preserve_top_level_source_locations () =
  let structure =
    Lg.Compiler.compile_parsetree {|
(def first 1)

(def second 2)
|}
    |> expect_ok
  in
  match structure with
  | [ first; second ] ->
      if
        first.pstr_loc.loc_start.pos_fname <> "<string>"
         || first.pstr_loc.loc_start.pos_lnum <> 2
      then failwith "expected first Parsetree item at <string>:2";
      if
        second.pstr_loc.loc_start.pos_fname <> "<string>"
         || second.pstr_loc.loc_start.pos_lnum <> 4
      then failwith "expected second Parsetree item at <string>:4"
  | _ -> failwith "expected two located Parsetree items"

let test_incremental_parsetree_preserves_chunk_source_locations () =
  let state, _ =
    Lg.Compiler.compile_chunk_parsetree Lg.Compiler.empty_state
      "\n(def first 1)"
    |> expect_ok
  in
  let _, structure =
    Lg.Compiler.compile_chunk_parsetree state "\n\n(def second 2)" |> expect_ok
  in
  match structure with
  | [ item ] ->
      if
        item.pstr_loc.loc_start.pos_fname <> "<string>"
         || item.pstr_loc.loc_start.pos_lnum <> 3
      then failwith "expected incremental Parsetree item at <string>:3"
  | _ -> failwith "expected one incremental Parsetree item"

let test_ocaml_errors_include_nested_expression_locations () =
  Lg.Compiler.compile_string
    "(def answer\n  (if true\n    (Stdlib.abs\n      \"bad\")\n    0))"
  |> expect_error_contains "line 4, characters 6-11"

let test_parsetree_expressions_preserve_nested_source_locations () =
  let structure =
    Lg.Compiler.compile_parsetree
      "(def answer\n\
      \  (if true\n\
      \    (String.uppercase_ascii\n\
      \      \"bad\")\n\
      \    \"ok\"))"
  in
  match structure with
  | Error err ->
      failwith ("expected valid nested expression, got: " ^ err.message)
  | Ok [ { pstr_desc = Pstr_value (_, [ binding ]); _ } ] -> (
      match binding.pvb_expr.pexp_desc with
      | Pexp_ifthenelse (_, then_expression, _) -> (
          match then_expression.pexp_desc with
          | Pexp_apply (_, [ (_, argument) ]) ->
              let start = argument.pexp_loc.loc_start in
              let finish = argument.pexp_loc.loc_end in
              if
                start.pos_lnum <> 4
                || start.pos_cnum - start.pos_bol <> 6
                 || finish.pos_cnum - finish.pos_bol <> 11
              then
                failwith
                  "expected nested string expression at line 4, characters 6-11"
          | _ -> failwith "expected nested OCaml application")
      | _ -> failwith "expected generated conditional expression")
  | Ok _ -> failwith "expected one generated value item"

let test_inferred_ocaml_calls_use_compiler_signatures () =
  let source =
    {|
(def answer (Stdlib.abs -42))
(def label (String.uppercase_ascii "ada"))
(println (str label ":" answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "inferred_ocaml_calls_use_compiler_signatures" "ADA:42\n"
    ocaml_source

let test_inferred_ocaml_calls_preserve_type_variable_identity () =
  let source =
    {|
(def values
  (List/init 3 (fn [index] (+ 1.0 (Float/of-int index)))))
(println (List/length values))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "inferred_ocaml_calls_preserve_type_variable_identity" "3\n"
    ocaml_source

let test_inferred_ocaml_calls_resolve_aliases_and_refers () =
  let source =
    {|
(require [ocaml.Stdlib :as std]
            [ocaml.String :refer [uppercase_ascii]])
(def answer (std/abs -42))
(def label (uppercase_ascii "ada"))
(println (str label ":" answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "inferred_ocaml_calls_resolve_aliases_and_refers" "ADA:42\n"
    ocaml_source

let test_inferred_ocaml_calls_reject_incompatible_arguments () =
  Lg.Compiler.compile_string {|
(def answer (Stdlib.abs "bad"))
|}
  |> expect_error_contains "string"

let test_inferred_ocaml_calls_reject_unknown_values () =
  Lg.Compiler.compile_string {|
(def answer (Stdlib.not_a_real_value 42))
|}
  |> expect_error_contains "Unbound value"

let test_inferred_ocaml_calls_support_required_labels () =
  let source =
    {|
(def starts (String.starts_with "ada" :prefix "ad"))
(println starts)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "inferred_ocaml_calls_support_required_labels" "true\n"
    ocaml_source

let test_inferred_ocaml_calls_support_optional_labels () =
  let source =
    {|
(def default-distance (String.edit_distance "abc" "adc"))
(def limited-distance
  (String.edit_distance "abc" "adc" :limit 2))
(println (+ default-distance limited-distance))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "inferred_ocaml_calls_support_optional_labels" "2\n"
    ocaml_source

let test_inferred_ocaml_calls_preserve_partial_labelled_functions () =
  let source =
    {|
(def starts-ad (String.starts_with :prefix "ad"))
(println (starts-ad "ada"))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "inferred_ocaml_calls_preserve_partial_labelled_functions"
    "true\n" ocaml_source

let test_inferred_ocaml_calls_support_labels_through_aliases () =
  let source =
    {|
(require [ocaml.String :as string])
(println (string/starts_with "ada" :prefix "ad"))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "inferred_ocaml_calls_support_labels_through_aliases"
    "true\n" ocaml_source

let test_inferred_ocaml_calls_reject_bad_labels () =
  Lg.Compiler.compile_string
    {|(def value (String.starts_with "ada" :unknown "ad"))|}
  |> expect_error_contains "unknown OCaml argument label :unknown";
  Lg.Compiler.compile_string
    {|
(def value
  (String.starts_with "ada" :prefix "ad" :prefix "a"))
|}
  |> expect_error_contains "duplicate OCaml argument label :prefix";
  Lg.Compiler.compile_string {|(def value (String.starts_with "ada" :prefix))|}
  |> expect_error_contains "OCaml argument label :prefix requires a value"

let test_inferred_labelled_calls_delegate_value_types_to_ocaml () =
  Lg.Compiler.compile_string
    {|(def value (String.starts_with "ada" :prefix 42))|}
  |> expect_error_contains "int"

let test_ocaml_package_requires_enable_inferred_calls () =
  Lg.Compiler.compile_string
    {|
(require [ocaml.package/core]
            [ocaml.Core.Int :as int])
(def answer (int/abs -42))
(println answer)
|}
  |> expect_ok |> ignore

let test_ocaml_package_requires_report_missing_packages () =
  Lg.Compiler.compile_string
    {|
(require [ocaml.package/lg-package-that-does-not-exist]
            [ocaml.Missing :as missing])
(def answer (missing/value 42))
|}
  |> expect_error_contains
       "OCaml package lg-package-that-does-not-exist was not found"

let test_ocaml_package_requires_reject_invalid_package_names () =
  Lg.Compiler.compile_string {|
(require [ocaml.package/bad;name])
|}
  |> expect_error_contains "invalid OCaml package name"

let test_direct_ocaml_calls_use_qualified_values () =
  let source =
    {|
(def answer (Stdlib.abs -42))
(def label (String.uppercase_ascii "ada"))
(println (str label ":" answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "direct_ocaml_calls_use_qualified_values" "ADA:42\n"
    ocaml_source

let test_direct_ocaml_calls_use_aliases_and_refers () =
  let source =
    {|
(require [ocaml.Stdlib :as std]
            [ocaml.String :refer [uppercase_ascii]])
(println (str (uppercase_ascii "ada") ":" (std/abs -42)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "direct_ocaml_calls_use_aliases_and_refers" "ADA:42\n"
    ocaml_source

let test_direct_ocaml_calls_support_labels_and_optional_arguments () =
  let source =
    {|
(require [ocaml.String :as string])
(def starts-ad (string/starts_with :prefix "ad"))
(def distance (string/edit_distance "abc" "adc" :limit 2))
(println (str (starts-ad "ada") ":" distance))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "direct_ocaml_calls_support_labels_and_optional_arguments"
    "true:1\n" ocaml_source

let test_direct_ocaml_calls_use_external_packages () =
  Lg.Compiler.compile_string
    {|
(require [ocaml.package/core]
            [ocaml.Core.Int :as int])
(println (int/abs -42))
|}
  |> expect_ok |> ignore

let test_direct_external_package_constructors_are_inferred () =
  Lg.Compiler.compile_string
    {|
(require [ocaml.package/unix]
            [ocaml.Unix :as unix])
(def address (unix/ADDR_UNIX "/tmp/lg.sock"))
(println "constructor-ok")
|}
  |> expect_ok |> ignore

let test_direct_external_package_constructors_reject_bad_arity () =
  Lg.Compiler.compile_string
    {|
(require [ocaml.package/unix]
            [ocaml.Unix :as unix])
(def address (unix/ADDR_UNIX))
|}
  |> expect_error "unix/ADDR_UNIX expects 1 arguments"

let test_direct_external_package_constructor_payloads_are_checked_by_ocaml () =
  Lg.Compiler.compile_string
    {|
(require [ocaml.package/unix]
            [ocaml.Unix :as unix])
(def address (unix/ADDR_UNIX 42))
|}
  |> expect_error_contains "string"

let test_direct_ocaml_calls_delegate_errors_to_ocaml () =
  Lg.Compiler.compile_string {|(def answer (Stdlib.abs "bad"))|}
  |> expect_error_contains "string";
  Lg.Compiler.compile_string
    {|(def answer (String.starts_with "ada" :unknown "a"))|}
  |> expect_error_contains "unknown OCaml argument label :unknown";
  Lg.Compiler.compile_string {|(def answer (Stdlib.not_a_real_value 42))|}
  |> expect_error_contains "Unbound value"

let test_generic_ocaml_calls_reject_bad_forms () =
  Lg.Compiler.compile_string {|(def answer (ocaml-call :int Stdlib.abs -42))|}
  |> expect_error_contains "unknown function ocaml-call";
  Lg.Compiler.compile_string {|(def answer (ocaml-field value name))|}
  |> expect_error_contains "unknown function ocaml-field";
  Lg.Compiler.compile_string {|(def value (ocaml-ref 1))|}
  |> expect_error_contains "unknown function ocaml-ref";
  Lg.Compiler.compile_string {|(def value (ocaml-construct Some 1))|}
  |> expect_error_contains "unknown function ocaml-construct";
  Lg.Compiler.compile_string {|(defn bad [^:ocaml/int value] value)|}
  |> expect_error_contains "the :ocaml/ type prefix is not supported";
  Lg.Compiler.compile_string {|(type-record box [value] (item :param/value))|}
  |> expect_error_contains "the :param/ type prefix is not supported"

let test_parsetree_typecheck_gate_rejects_invalid_required_module_alias_calls ()
    =
  Lg.Compiler.compile_parsetree
    {|
(require [ocaml.Stdlib :as std])
(def answer (std/abs "bad"))
|}
  |> expect_error_contains "string"

let test_parsetree_typecheck_gate_accepts_valid_host_calls () =
  Lg.Compiler.typecheck_parsetree
    {|
(def answer (Stdlib.abs -42))
(def label (String.uppercase_ascii "ada"))
|}
  |> expect_ok

let test_parsetree_typecheck_gate_rejects_invalid_host_calls () =
  Lg.Compiler.typecheck_parsetree {|
(def answer (Stdlib.abs "bad"))
|}
  |> expect_error_contains "string"

let test_parsetree_typecheck_gate_accepts_runtime_dependencies () =
  Lg.Compiler.typecheck_parsetree
    {|
(def user {:name "Ada", :age 36})
(def xs [1 2 3])
(def users (hash-set user))
(def answer (+ (count xs) (count users)))
|}
  |> expect_ok

let test_type_aliases_compile_through_source_backend () =
  let source =
    {|
(type-alias user-id :int)
(defn keep-user-id [^:user_id x] x)
(def answer (keep-user-id 42))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  if not (String.contains ocaml_source '=') then
    failwith "expected generated OCaml to contain a type alias";
  assert_ocaml_runs "type_aliases_compile_through_source_backend" ""
    ocaml_source

let test_parameterized_type_declarations_compile () =
  let source =
    {|
(type-alias maybe [a] :option<a>)
(type-record pair [a b]
  (left :a)
  (right :b))
(type-variant box [a]
  (Box :a))
(def pair-value (record pair (left 42) (right "Ada")))
(def box-value (Box 42))
(println "parameterized-ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "parameterized_type_declarations_compile"
    "parameterized-ok\n" ocaml_source

let test_parameterized_records_instantiate_field_types () =
  let source =
    {|
(type-record box [a]
  (value :a))
(type-record ordering [a]
  (compare-values :fn<a;a;int>))
(def int-box (record box (value 41)))
(def string-box (record box (value "Ada")))
(def int-ordering
  (record ordering (compare-values (fn [left right] (- left right)))))
(def int-value (+ (:value int-box) 1))
(def string-value (subs (:value string-box) 0 1))
(def compare-ints (:compare-values int-ordering))
(println (str int-value ":" string-value ":" (+ (compare-ints 4 2) 0)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "parameterized_records_instantiate_field_types" "42:A:2\n"
    ocaml_source

let test_recursive_record_array_fields_work_with_array_primitives () =
  let source =
    {|
(type-record tree [a]
  (keys :array<a>)
  (children :array<tree<a>>))
(def leaf
  (record tree (keys (array 1 2 3)) (children (Array.of_list (list)))))
(def root
  (record tree (keys (array 3)) (children (array leaf))))
(defn last-key [node]
  (let [children (:children node)]
    (if (= 0 (alength children))
      (let [keys (:keys node)]
        (aget keys (dec (alength keys))))
      (last-key
        (aget children (dec (alength children)))))))
(defn append-children [left right]
  (aconcat
    (:children left)
    (:children right)))
(println
  (str (+ (last-key root) 0) ":"
       (alength (append-children root root))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "recursive_record_array_fields_work_with_array_primitives"
    "3:2\n" ocaml_source

let test_parameterized_variants_instantiate_constructor_payloads () =
  let source =
    {|
(type-variant box [a]
  (Box :a))
(def int-box (Box 41))
(def string-box (Box "Ada"))
(def int-value
  (match int-box
    (Box value) (+ value 1)))
(def string-value
  (match string-box
    (Box value) (subs value 0 1)))
(println (str int-value ":" string-value))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "parameterized_variants_instantiate_constructor_payloads"
    "42:A\n" ocaml_source

let test_parameterized_types_compile_inside_modules () =
  let source =
    {|
(module Types
  (type-alias maybe [a] :option<a>)
  (type-record pair [a b]
    (left :a)
    (right :b))
  (type-variant box [a]
    (Box :a)))
(println "module-parameterized-ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "parameterized_types_compile_inside_modules"
    "module-parameterized-ok\n" ocaml_source

let test_parameterized_record_relationships_are_checked_by_ocaml () =
  Lg.Compiler.compile_string
    {|
(type-record same-pair [a]
  (left :a)
  (right :a))
(def bad (record same-pair (left 42) (right "Ada")))
|}
  |> expect_error_contains "string"

let test_parameterized_variant_relationships_are_checked_by_ocaml () =
  Lg.Compiler.compile_string
    {|
(type-variant same-pair [a]
  (Pair :a :a))
(def bad (Pair 42 "Ada"))
|}
  |> expect_error_contains "string"

let test_parameterized_type_declarations_reject_bad_parameters () =
  Lg.Compiler.compile_string {|(type-alias maybe [a a] :option<a>)|}
  |> expect_error_contains "duplicate type parameter a";
  Lg.Compiler.compile_string {|(type-record pair [a :bad] (value :a))|}
  |> expect_error_contains "type parameters must be symbols";
  Lg.Compiler.compile_string {|(type-variant box [a] (Box :missing))|}
  |> expect_error_contains "Unbound type constructor missing";
  Lg.Compiler.compile_string {|(type-alias maybe [] :option<int>)|}
  |> expect_error_contains "type parameter vector must not be empty";
  Lg.Compiler.compile_string {|(type-record bad [a] (callback :fn<int>))|}
  |> expect_error_contains "unknown record field type :fn<int>"

let test_ocaml_owned_branch_types_are_checked_by_ocaml () =
  let source =
    {|
(type-alias user-id :int)
(type-alias account-id :int)
(defn as-user [^:user_id x] x)
(defn as-account [^:account_id x] x)
(def if-id (if true (as-user 41) (as-account 42)))
(def cond-id (cond false (as-user 1) :else (as-account 2)))
(def match-id (match true true (as-user 3) false (as-account 4)))
(println "aliases-ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_owned_branch_types_are_checked_by_ocaml"
    "aliases-ok\n" ocaml_source

let test_ocaml_owned_branch_type_mismatch_is_delegated_to_ocaml () =
  Lg.Compiler.compile_string
    {|
(type-alias user-id :int)
(defn as-user [^:user_id x] x)
(def bad (if true (as-user 41) "bad"))
|}
  |> expect_error_contains "string"

let test_ocaml_option_and_result_constructors_compile_through_source_backend ()
    =
  let source =
    {|
(def present (Some 42))
(def absent None)
(def success (Ok "Ada"))
(def failure (Error "bad"))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "ocaml_option_and_result_constructors_compile_through_source_backend" ""
    ocaml_source

let test_ocaml_option_and_result_constructors_reject_bad_arity () =
  Lg.Compiler.compile_string {|(def value (Some ))|}
  |> expect_error "Some expects 1 arguments";
  Lg.Compiler.compile_string {|(def value (None 1))|}
  |> expect_error "None expects 0 arguments";
  Lg.Compiler.compile_string {|(def value (Ok ))|}
  |> expect_error "Ok expects 1 arguments";
  Lg.Compiler.compile_string {|(def value (Error ))|}
  |> expect_error "Error expects 1 arguments"

let test_direct_ocaml_option_and_result_constructors_compile () =
  let source =
    {|
(def present (Some 41))
(def absent None)
(def success (Ok "Ada"))
(def failure (Error "bad"))
(def present-score (match present (Some x) (+ x 1) None 0))
(def absent-score (match absent (Some x) (+ x 1) None 0))
(def success-label (match success (Ok x) x (Error x) x))
(def failure-label (match failure (Ok x) x (Error x) x))
(println (str present-score ":" absent-score ":" success-label ":" failure-label))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "direct_ocaml_option_and_result_constructors_compile"
    "42:0:Ada:bad\n" ocaml_source

let test_direct_declared_variant_constructors_compile () =
  let source =
    {|
(type-variant status Active (Named :string))
(def active Active)
(def named (Named "Ada"))
(def active-label (match active Active "active" (Named name) name))
(def named-label (match named Active "active" (Named name) name))
(println (str active-label ":" named-label))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "direct_declared_variant_constructors_compile"
    "active:Ada\n" ocaml_source

let test_direct_ocaml_constructors_reject_bad_arity () =
  Lg.Compiler.compile_string {|(def value (Some))|}
  |> expect_error "Some expects 1 arguments";
  Lg.Compiler.compile_string {|(def value (None 1))|}
  |> expect_error "None expects 0 arguments";
  Lg.Compiler.compile_string
    {|
(type-variant status Active (Named :string))
(def value (Named))
|}
  |> expect_error "Named expects 1 arguments"

let test_ocaml_option_and_result_patterns_compile_through_source_backend () =
  let source =
    {|
(def present (Some 41))
(def absent None)
(def success (Ok "Ada"))
(def failure (Error "bad"))
(def present-score
  (match present
    (Some x) (+ x 1)
    None 0))
(def absent-score
  (match absent
    (Some x) (+ x 1)
    None 0))
(def success-label
  (match success
    (Ok name) name
    (Error message) message))
(def failure-label
  (match failure
    (Ok name) name
    (Error message) message))
(println (str present-score ":" absent-score ":" success-label ":" failure-label))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "ocaml_option_and_result_patterns_compile_through_source_backend"
    "42:0:Ada:bad\n" ocaml_source

let test_ocaml_option_patterns_delegate_payload_typecheck_to_ocaml () =
  Lg.Compiler.compile_string
    {|
(def present (Some "bad"))
(def bad
  (match present
    (Some x) (+ x 1)
    None 0))
|}
  |> expect_error_contains "expected int arguments for +"

let test_ocaml_type_application_annotations_compile_through_source_backend () =
  let source =
    {|
(def present (Some 41))
(def absent None)
(def success (Ok "Ada"))
(def failure (Error "bad"))
(defn option-score [^:option<int> value]
  (match value
    (Some x) (+ x 1)
    None 0))
(defn result-label [^:result<string;string> value]
  (match value
    (Ok name) name
    (Error message) message))
(println (str (option-score present) ":" (option-score absent) ":"
              (result-label success) ":" (result-label failure)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "ocaml_type_application_annotations_compile_through_source_backend"
    "42:0:Ada:bad\n" ocaml_source

let test_ocaml_type_application_annotations_delegate_argument_mismatch_to_ocaml
    () =
  Lg.Compiler.compile_string
    {|
(def present (Some "bad"))
(defn option-score [^:option<int> value]
  (match value
    (Some x) (+ x 1)
    None 0))
(def bad (option-score present))
|}
  |> expect_error_contains "string"

let test_ocaml_type_application_annotations_reject_bad_forms () =
  Lg.Compiler.compile_string {|(defn bad [^:option<> value] value)|}
  |> expect_error "invalid type annotation ^:option<>";
  Lg.Compiler.compile_string {|(defn bad [^:result<int> value] value)|}
  |> expect_error "invalid type annotation ^:result<int>"

let test_concise_host_type_annotations_compile () =
  let source =
    {|
(defn option-score [^:option<int> value]
  (match value (Some x) (+ x 1) None 0))
(defn result-label [^:result<string;string> value]
  (match value (Ok x) x (Error x) x))
(defn tuple-label [^:tuple<int;string> value]
  (match value (tuple id name) (str name ":" id)))
(println
  (str (option-score (Some 41)) ":"
       (result-label (Ok "Ada")) ":"
       (tuple-label (tuple 7 "Grace"))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "concise_host_type_annotations_compile" "42:Ada:Grace:7\n"
    ocaml_source

let test_threading_and_option_binding_forms_compile () =
  let source =
    {|
(defn option-score [^:option<int> value]
  (if-let [x value] (+ x 1) 0))
(def threaded (-> 41 (+ 1) str))
(def threaded-last (->> 41 (str "value=")))
(defn maybe-thread [^:option<int> value]
  (some-> value (+ 1) str))
(defn maybe-thread-last [^:option<int> value]
  (some->> value (str "value=")))
(def some-threaded
  (if-some [value (maybe-thread (Some 41))] value "missing"))
(def some-missing
  (if-some [value (maybe-thread None)] value "missing"))
(def destructured-option
  (if-some [value (when-let [[left right] (Some [2 3])]
                    (+ left right))]
    value
    0))
(def combined
  (let-some [left (Some 2) right (Some 3)]
    (+ left right)
    0))
(def missing
  (let-some [left None right (Some 3)]
    (+ left right)
    9))
(def observed (atom 0))
(when-let [value (Some 7)]
  (reset! observed value))
(println
  (str (option-score (Some 41)) ":" (option-score None) ":"
       threaded ":" threaded-last ":" combined ":" missing ":"
       (deref observed) ":" some-threaded ":" some-missing ":"
       destructured-option ":" (maybe-thread-last (Some 41))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "threading_and_option_binding_forms_compile"
    "42:0:42:value=41:5:9:7:42:missing:5:value=41\n" ocaml_source;
  Lg.Compiler.compile_string {|(def bad (if-let [x] x 0))|}
  |> expect_error "if-let requires [name option], then, and else";
  Lg.Compiler.compile_string {|(def bad (-> 1 2))|}
  |> expect_error "threading steps must be symbols or call forms";
  Lg.Compiler.compile_string {|(def bad (some-> (Some 1) 2))|}
  |> expect_error "some-> steps must be symbols or call forms";
  Lg.Compiler.compile_string {|(def bad (let-some [x (Some 1) y] x 0))|}
  |> expect_error "let-some bindings require name/option pairs"

let test_combined_host_package_import_compiles () =
  let source =
    {|
(require [ocaml.core/Core.Int :as int])
(println (int/abs -42))
|}
  in
  let packages = Lg.Compiler.required_ocaml_packages source |> expect_ok in
  if packages <> [ "core" ] then
    failwith "combined host import should report its findlib package";
  Lg.Compiler.compile_string source |> expect_ok |> ignore

let test_ocaml_tuple_values_compile_through_source_backend () =
  let source =
    {|
(def pair (tuple 41 "Ada"))
(defn describe [^:tuple<int;string> value]
  (match value
    (tuple id name) (str name ":" (+ id 1))))
(println (describe pair))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_tuple_values_compile_through_source_backend"
    "Ada:42\n" ocaml_source

let test_ocaml_tuple_values_delegate_argument_mismatch_to_ocaml () =
  Lg.Compiler.compile_string
    {|
(def pair (tuple "bad" "Ada"))
(defn describe [^:tuple<int;string> value]
  (match value
    (tuple id name) (str name ":" (+ id 1))))
(def bad (describe pair))
|}
  |> expect_error_contains "string"

let test_ocaml_tuple_values_reject_bad_forms () =
  Lg.Compiler.compile_string {|(def value (tuple 1))|}
  |> expect_error "tuple expects at least 2 values";
  Lg.Compiler.compile_string {|(defn bad [^:tuple<int> value] value)|}
  |> expect_error "invalid type annotation ^:tuple<int>";
  Lg.Compiler.compile_string
    {|
(def pair (tuple 1 "Ada"))
(def bad (match pair
  (tuple id) id))
|}
  |> expect_error "tuple pattern arity mismatch"

let test_concise_tuple_values_and_patterns_compile () =
  let source =
    {|
(def pair (tuple 41 "Ada"))
(defn describe [^:tuple<int;string> value]
  (match value
    (tuple id name) (str name ":" (+ id 1))))
(println (describe pair))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "concise_tuple_values_and_patterns_compile" "Ada:42\n"
    ocaml_source;
  Lg.Compiler.compile_string {|(def bad (tuple 1))|}
  |> expect_error "tuple expects at least 2 values"

let test_ocaml_float_and_char_literals_compile () =
  let source =
    {|
(def sum (Float.add 1.5 2.25))
(def upper (Char.uppercase_ascii \a))
(println (str (Float.to_string sum) ":" (String.make 1 upper)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_float_and_char_literals_compile" "3.75:A\n"
    ocaml_source

let test_double_converts_ints_and_preserves_floats () =
  let source =
    {|
(println (str (Float.to_string (double 2)) ":"
              (Float.to_string (+ (double 2.5) 0.5))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "double_converts_ints_and_preserves_floats" "2.:3.\n"
    ocaml_source

let test_ocaml_arrays_support_construction_read_and_mutation () =
  let source =
    {|
(def values (array 1 2 3))
(aset values 1 42)
(unsafe-aset values 2 99)
(def empty-values (array-of :int))
(println
  (str (+ (aget values 1) 0) ":"
       (+ (unsafe-aget values 2) 0) ":"
       (Array.length empty-values)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_arrays_support_construction_read_and_mutation"
    "42:99:0\n" ocaml_source

let test_ocaml_array_primitives_support_polymorphic_helpers () =
  let source =
    {|
(defn copy-array [source]
  (Array.copy source))
(defn first-array [values]
  (aget values 0))
(defn seq-to-sorted-array [cmp values]
  (let [result (into-array values)]
    (asort! cmp result)
    result))
(def copied (copy-array (array 7 8 9)))
(def converted
  (seq-to-sorted-array (fn [left right] (- left right)) [5 4]))
(def converted-with-to-array (to-array [1 2 3]))
(println
  (str (alength copied) ":" (+ (first-array copied) 0) ":"
       (alength converted) ":" (+ (aget converted 1) 0) ":"
       (alength converted-with-to-array)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_array_primitives_support_polymorphic_helpers"
    "3:7:2:5:3\n" ocaml_source

let test_ocaml_refs_support_read_and_assignment () =
  let source =
    {|
(def cell (atom 40))
(reset! cell (+ (deref cell) 2))
(println (deref cell))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_refs_support_read_and_assignment" "42\n" ocaml_source

let test_concise_standard_type_annotations () =
  let source =
    {|
(type-record holder [value]
  (values :array<value>)
  (current :ref<option<value>>)
  (visit :fn<value;unit>))
(def holder-value
  (record holder (values (array 1 2)) (current (volatile! (Some 1))) (visit (fn [value] (Stdlib.ignore value)))))
(println (alength (:values holder-value)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "concise_standard_type_annotations" "2\n" ocaml_source

let test_volatile_nil_uses_contextual_option_reference_type () =
  let source =
    {|
(type-record holder (current :ref<option<int>>))
(def value (record holder (current (volatile! nil))))
(vreset! (:current value) (Some 7))
(println
  (match (deref (:current value))
    (Some number) number
    None 0))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "volatile_nil_uses_contextual_option_reference_type" "7\n"
    ocaml_source

let test_local_volatile_nil_infers_value_from_reset () =
  let source =
    {|
(let [slot (volatile! nil)]
  (vreset! slot 4)
  (println (+ (deref slot) 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "local_volatile_nil_infers_value_from_reset" "5\n"
    ocaml_source

let test_ocaml_arrays_reject_invalid_operations () =
  Lg.Compiler.compile_string {|(def values (array 1 "two"))|}
  |> expect_error_contains "OCaml array elements must have the same type";
  Lg.Compiler.compile_string {|(def value (aget 42 0))|}
  |> expect_error_contains "aget expects an OCaml array";
  Lg.Compiler.compile_string {|(def value (aget (array 1 2) "0"))|}
  |> expect_error_contains "OCaml array index must be int";
  Lg.Compiler.compile_string {|(aset (array 1 2) 0 "bad")|}
  |> expect_error_contains "OCaml array value must match element type";
  Lg.Compiler.compile_string {|(def values (array))|}
  |> expect_error_contains "empty OCaml array requires a type"

let test_ocaml_refs_reject_invalid_operations () =
  Lg.Compiler.compile_string {|(def value (deref 42))|}
  |> expect_error_contains "deref expects a reference";
  Lg.Compiler.compile_string {|(reset! 42 1)|}
  |> expect_error_contains "reset! expects a reference";
  Lg.Compiler.compile_string {|(reset! (atom 1) "bad")|}
  |> expect_error_contains "reset! value must match referenced type"

let test_float_arithmetic_rejects_mixed_numeric_types () =
  Lg.Compiler.compile_string {|(def bad (+ 1 2.5))|}
  |> expect_error "numeric arguments must all have the same type"

let test_float_arithmetic_uses_core_numeric_operators () =
  let source =
    {|
(println
  (str (+ 1.5 2.5) ":"
       (- 5.0 1.5) ":"
       (* 2.0 3.0) ":"
       (/ 7.5 2.5)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "float_arithmetic_uses_core_numeric_operators"
    "4.:3.5:6.:3.\n" ocaml_source

let test_float_arithmetic_uses_types_from_option_patterns () =
  let source =
    {|
(defn order-between [^:option<float> previous ^:option<float> next]
  (match (tuple previous next)
    (tuple None None) 1.0
    (tuple (Some previous) None) (+ previous 1.0)
    (tuple None (Some next)) (- next 1.0)
    (tuple (Some previous) (Some next)) (/ (+ previous next) 2.0)))
(println (order-between (Some 2.0) (Some 6.0)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "float_arithmetic_uses_types_from_option_patterns" "4.\n"
    ocaml_source

let test_float_numeric_core_is_coherent () =
  let source =
    {|
(defn midpoint [left right]
  (/ (+ left right) 2.0))
(println
  (str (midpoint 1.0 3.0) ":"
       (< 1.0 2.0 3.0) ":" (> 3.0 2.0 1.0) ":"
       (number? 1.5) ":" (float? 1.5) ":" (double? 1.5) ":"
       (rational? 1.5) ":"
       (zero? 0.0) ":" (pos? 1.5) ":" (neg? -1.5) ":"
       (max 1.5 3.0 2.0) ":" (min 1.5 3.0 2.0) ":"
       (compare 1.0 2.0) ":" (distinct? 1.0 2.0 1.0)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "float_numeric_core_is_coherent"
    "2.:true:true:true:true:true:false:true:true:true:3.:1.5:-1:false\n"
    ocaml_source

let test_float_sets_support_scalar_and_collection_elements () =
  let source =
    {|
(def values (hash-set 1.5 2.5 2.5))
(def lists (hash-set (list 1.5 2.5) (list 1.5 2.5)))
(def vectors (hash-set [1.5 2.5] [2.5 3.5]))
(println
  (str (count values) ":" (contains? values 2.5) ":"
       (count lists) ":" (contains? lists (list 1.5 2.5)) ":"
       (count vectors) ":" (contains? vectors [2.5 3.5])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "float_sets_support_scalar_and_collection_elements"
    "2:true:1:true:2:true\n" ocaml_source

let test_float_numeric_core_rejects_invalid_mixes () =
  Lg.Compiler.compile_string {|(def bad (< 1 2.0))|}
  |> expect_error_contains "same type";
  Lg.Compiler.compile_string {|(def bad (max 1 2.0))|}
  |> expect_error_contains "same type";
  Lg.Compiler.compile_string {|(def bad (even? 2.0))|}
  |> expect_error "expected int arguments for even?"

let test_ocaml_record_values_compile_through_source_backend () =
  let source =
    {|
(type-record user (name :string) (age :int))
(def ada (record user (name "Ada") (age 41)))
(println (str (:name ada) ":" (+ (:age ada) 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_record_values_compile_through_source_backend"
    "Ada:42\n" ocaml_source

let test_ocaml_record_values_support_qualified_module_types () =
  let source =
    {|
(module User
  (type-record user (name :string) (age :int)))
(def ada (record User.user (name "Ada") (age 41)))
(println (str (:name ada) ":" (+ (:age ada) 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_record_values_support_qualified_module_types"
    "Ada:42\n" ocaml_source

let test_ocaml_record_values_support_module_alias_types () =
  let source =
    {|
(module User
  (type-record user (name :string) (age :int)))
(module-alias U User)
(def ada (record U.user (name "Ada") (age 41)))
(println (str (:name ada) ":" (+ (:age ada) 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_record_values_support_module_alias_types" "Ada:42\n"
    ocaml_source

let test_ocaml_record_values_support_opened_module_types () =
  let source =
    {|
(module User
  (type-record user (name :string) (age :int)))
(open User)
(def ada (record user (name "Ada") (age 41)))
(println (str (:name ada) ":" (+ (:age ada) 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_record_values_support_opened_module_types" "Ada:42\n"
    ocaml_source

let test_ocaml_record_values_support_opened_module_types_in_module_body () =
  let source =
    {|
(module User
  (type-record user (name :string) (age :int)))
(module App
  (open User)
  (def ada (record user (name "Ada") (age 41)))
  (def label (str (:name ada) ":" (+ (:age ada) 1))))
(println App/label)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "ocaml_record_values_support_opened_module_types_in_module_body" "Ada:42\n"
    ocaml_source

let test_ocaml_record_values_support_included_module_types () =
  let source =
    {|
(module User
  (type-record user (name :string) (age :int)))
(module App
  (include User))
(def ada (record App.user (name "Ada") (age 41)))
(println (str (:name ada) ":" (+ (:age ada) 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_record_values_support_included_module_types"
    "Ada:42\n" ocaml_source

let test_ocaml_record_values_delegate_qualified_field_typecheck_to_ocaml () =
  Lg.Compiler.compile_string
    {|
(module User
  (type-record user (name :string) (age :int)))
(def bad (record User.user (name "Ada") (age "old")))
|}
  |> expect_error_contains "string"

let test_ocaml_record_values_delegate_field_typecheck_to_ocaml () =
  Lg.Compiler.compile_string
    {|
(type-record user (name :string) (age :int))
(def bad (record user (name "Ada") (age "old")))
|}
  |> expect_error_contains "string"

let test_ocaml_record_values_reject_bad_forms () =
  Lg.Compiler.compile_string {|(type-record user)|}
  |> expect_error "type-record expects at least one field";
  Lg.Compiler.compile_string {|(type-record user (name :unknown))|}
  |> expect_error_contains "Unbound type constructor unknown";
  Lg.Compiler.compile_string {|(def bad (record user))|}
  |> expect_error "unknown record type user";
  Lg.Compiler.compile_string
    {|
(type-record user (name :string))
(def bad (record user (name "Ada") (name "Grace")))
|}
  |> expect_error "duplicate record field name";
  Lg.Compiler.compile_string
    {|
(type-record user (name :string))
(def ada (record user (name "Ada")))
(def bad (:age ada))
|}
  |> expect_error "unknown record field age"

let test_ocaml_field_delegates_opaque_record_access_to_ocaml () =
  Lg.Compiler.compile_string
    {|
(defn attrs [^:External.record value]
  (:attrs value))
|}
  |> expect_error_contains "Unbound module External"

let test_ocaml_variants_compile_through_source_backend () =
  let source =
    {|
(type-variant status Active Inactive)
(def active Active)
(defn keep-status [^:status x] x)
(def saved (keep-status active))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_variants_compile_through_source_backend" ""
    ocaml_source

let test_ocaml_payload_variants_compile_through_source_backend () =
  let source =
    {|
(type-variant message Ping (Named :string) (Pair :int :string))
(def named (Named "Ada"))
(def pair (Pair 42 "Ada"))
(defn describe [^:message message]
  (match message
    (Named name) name
    (Pair id name) (str name ":" id)
    Ping "ping"))
(println (str (describe named) ":" (describe pair)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_payload_variants_compile_through_source_backend"
    "Ada:Ada:42\n" ocaml_source

let test_ocaml_payload_variants_delegate_payload_typecheck_to_ocaml () =
  Lg.Compiler.compile_string
    {|
(type-variant message (Named :string))
(def bad (Named 42))
|}
  |> expect_error_contains "int"

let test_ocaml_variant_constructors_reject_bad_arity () =
  Lg.Compiler.compile_string
    {|
(type-variant status Active (Named :string))
(def value (Active 1))
|}
  |> expect_error "Active expects 0 arguments";
  Lg.Compiler.compile_string
    {|
(type-variant status Active (Named :string))
(def value (Named))
|}
  |> expect_error "Named expects 1 arguments"

let test_ocaml_variants_reject_bad_declarations () =
  Lg.Compiler.compile_string {|(type-variant status)|}
  |> expect_error "type-variant expects at least one constructor";
  Lg.Compiler.compile_string {|(type-variant status Active Active)|}
  |> expect_error "duplicate variant constructor Active";
  Lg.Compiler.compile_string {|(type-variant status :Active)|}
  |> expect_error "type-variant constructors must be symbols"

let test_recursive_variants_support_nested_data_values () =
  let source =
    {|
(type-variant value
  Nil
  (IntValue :int)
  (ListValue :list<value>)
  (MapValue :list<tuple<value;value>>))
(def nil-value Nil)
(def nested (ListValue (list nil-value)))
(def entries (list (tuple nil-value nested)))
(def mapped (MapValue entries))
(println
  (str
    (match nested
      Nil 0
      (IntValue value) value
      (ListValue values) (count values)
      (MapValue values) (count values))
    ":"
    (match mapped
      Nil 0
      (IntValue value) value
      (ListValue values) (count values)
      (MapValue values) (count values))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "recursive_variants_support_nested_data_values" "1:1\n"
    ocaml_source

let test_module_recursive_variants_export_constructors () =
  let source =
    {|
(module Data
  (type-variant value End (Next :value)))
(def end-value Data/End)
(def next-value (Data/Next end-value))
(println
  (match next-value
    Data/End "end"
    (Data/Next value)
      (match value
        Data/End "next-end"
        (Data/Next _) "next-next")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_recursive_variants_export_constructors" "next-end\n"
    ocaml_source

let test_defonce_supports_top_level_and_module_values () =
  let source =
    {|
(defonce answer 42)
(module Config
  (defonce label "ready"))
(println (str answer ":" Config/label))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "defonce_supports_top_level_and_module_values" "42:ready\n"
    ocaml_source

let test_defonce_rejects_invalid_declarations () =
  Lg.Compiler.compile_string {|(defonce)|}
  |> expect_error "defonce expects a name and value";
  Lg.Compiler.compile_string {|(defonce value)|}
  |> expect_error "defonce expects a name and value";
  Lg.Compiler.compile_string {|(defonce value 1 2)|}
  |> expect_error "defonce expects a name and value"

let test_datascript_schema_constants_behavior () =
  let source =
    {|
(require [clojure.string :as string])

(module Datascript_schema
  (def schema-keys
    #{:db/ident :db/isComponent :db/noHistory :db/valueType :db/cardinality
      :db/unique :db/index :db.install/_attribute :db/doc :db/tupleType
      :db/tupleTypes :db/tupleAttrs})

  (defonce schema-attr?
    #{:db/id :db/ident :db/isComponent :db/valueType :db/cardinality
      :db/unique :db/index :db/doc :db/tupleAttrs :db/tupleType :db/tupleTypes})

  (def type?
    #{:db.type/number :db.type/instant :db.type/keyword :db.type/ref
      :db.type/string :db.type/uuid :db.type/tuple}))

(println
  (str (count Datascript_schema/schema-keys)
       ":" (contains? Datascript_schema/schema-attr? :db/id)
       ":" (some? (Datascript_schema/type? :db.type/ref))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "datascript_schema_constants_behavior" "12:true:true\n"
    ocaml_source

let test_typed_function_parameters_reject_bad_calls () =
  let source = {|
(defn inc1 [^:int x] (+ x 1))
(def bad (inc1 "Ada"))
|} in
  Lg.Compiler.compile_string source
  |> expect_error "inc1 called with incompatible arguments"

let test_unit_annotations_reject_non_unit_arguments () =
  let source =
    {|
(defn accept-unit [^:unit value] value)
(def bad (accept-unit 1))
|}
  in
  Lg.Compiler.compile_string source
  |> expect_error "accept-unit called with incompatible arguments"

let test_typed_function_parameters_reject_bad_bodies () =
  Lg.Compiler.compile_string {|(defn bad [^:string x] (+ x 1))|}
  |> expect_error "expected int arguments for +"

let test_typed_recursive_functions () =
  let source =
    {|
(defn factorial [^:int n] :int
  (if (= n 0) 1 (* n (factorial (- n 1)))))
(module Math
  (defn sum-to [^:int n] :int
    (if (= n 0) 0 (+ n (sum-to (- n 1))))))
(println (str (factorial 5) ":" (Math/sum-to 10)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_recursive_functions" "120:55\n" ocaml_source

let test_typed_recursive_functions_require_valid_signatures () =
  Lg.Compiler.compile_string {|
(defn bad [n] :int (bad n))
|}
  |> expect_error "recursive defn parameters require type annotations";
  Lg.Compiler.compile_string {|
(defn bad [^:int n] :string
  0)
|}
  |> expect_error "recursive defn bad must return string"

let test_multi_arity_defn_dispatches_fixed_arities () =
  let source =
    {|
(defn stamp
  ([] 10)
  ([^:int value] value))
(defn score
  ([^:int value] (+ value 1))
  ([^:int left ^:int right] (+ left right)))
(println (str (stamp) ":" (stamp 11) ":" (score 4) ":" (score 5 6)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "multi_arity_defn_dispatches_fixed_arities" "10:11:5:11\n"
    ocaml_source

let test_defn_accepts_docstring_before_arities () =
  let source =
    {|
(defn stamp
  "Returns the supplied value, or the default stamp."
  ([] 10)
  ([^:int value] value))
(println (str (stamp) ":" (stamp 11)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "defn_accepts_docstring_before_arities" "10:11\n"
    ocaml_source

let test_multi_arity_defn_dispatches_variadic_fallback () =
  let source =
    {|
(defn sum
  ([^:int value] value)
  ([^:int left ^:int right] (+ left right))
  ([^:int left ^:int right & more]
   (reduce + (+ left right) more)))
(defn all [& values] (reduce + 0 values))
(println (str (sum 1) ":" (sum 1 2) ":" (sum 1 2 3 4) ":" (all) ":" (all 5 6 7)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "multi_arity_defn_dispatches_variadic_fallback"
    "1:3:10:0:18\n" ocaml_source

let test_multi_arity_defn_supports_cross_arity_calls_and_recur () =
  let source =
    {|
(defn ascending?
  ([^:int x] true)
  ([^:int x ^:int y] (< x y))
  ([^:int x ^:int y & more]
   (if (ascending? x y)
     (if (next more)
       (recur y (first more) (next more))
       (ascending? y (first more)))
     false)))
(println (str (ascending? 1) ":" (ascending? 1 2) ":"
              (ascending? 1 2 3 4) ":" (ascending? 1 3 2 4)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "multi_arity_defn_supports_cross_arity_calls_and_recur"
    "true:true:true:false\n" ocaml_source

let test_multi_arity_defn_remains_callable_as_a_value () =
  let source =
    {|
(defn score
  ([^:int value] (+ value 1))
  ([^:int left ^:int right] (+ left right)))
(def selected score)
(println (str (selected 4) ":" (selected 5 6)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "multi_arity_defn_remains_callable_as_a_value" "5:11\n"
    ocaml_source

let test_modules_export_multi_arity_defn () =
  let source =
    {|
(module Math
  (defn score
    ([^:int value] (+ value 1))
    ([^:int left ^:int right] (+ left right))))
(println (str (Math/score 4) ":" (Math/score 5 6)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "modules_export_multi_arity_defn" "5:11\n" ocaml_source

let test_multi_arity_defn_rejects_invalid_declarations () =
  Lg.Compiler.compile_string
    {|
(defn bad
  ([value] value)
  ([other] other))
|}
  |> expect_error "defn bad has duplicate arity 1";
  Lg.Compiler.compile_string
    {|
(defn bad
  ([value & more] value)
  ([value] value))
|}
  |> expect_error "defn bad variadic arity must be last"

let test_multi_arity_defn_rejects_unsupported_calls () =
  Lg.Compiler.compile_string
    {|
(defn score
  ([^:int value] value)
  ([^:int left ^:int right] (+ left right)))
(def bad (score))
|}
  |> expect_error "score called with unsupported arity 0"

let test_private_defn_supports_single_and_typed_recursive_arities () =
  let source =
    {|
(defn- add-one [^:int value] (+ value 1))
(defn- factorial [^:int value] :int
  (if (= value 0) 1 (* value (factorial (dec value)))))
(println (str (add-one 41) ":" (factorial 5)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "private_defn_supports_single_and_typed_recursive_arities"
    "42:120\n" ocaml_source

let test_private_defn_supports_variadic_and_multi_arity_recur () =
  let source =
    {|
(defn- total [& values] (reduce + 0 values))
(defn- ascending?
  ([^:int x] true)
  ([^:int x ^:int y] (< x y))
  ([^:int x ^:int y & more]
   (if (ascending? x y)
     (if (next more)
       (recur y (first more) (next more))
       (ascending? y (first more)))
     false)))
(println (str (total 1 2 3 4) ":" (ascending? 1 2 3 4) ":"
              (ascending? 1 3 2 4)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "private_defn_supports_variadic_and_multi_arity_recur"
    "10:true:false\n" ocaml_source

let test_module_private_defn_is_internal_only () =
  let source =
    {|
(module Math
  (defn- hidden [^:int value] (+ value 1))
  (defn public [^:int value] (hidden value)))
(println (str (Math/public 41)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_private_defn_is_internal_only" "42\n" ocaml_source;
  Lg.Compiler.compile_string
    {|
(module Math
  (defn- hidden [^:int value] (+ value 1))
  (defn public [^:int value] (hidden value)))
(def leaked (Math/hidden 41))
|}
  |> expect_error_contains "Unbound module Math"

let test_private_defn_rejects_invalid_declarations () =
  Lg.Compiler.compile_string
    {|
(defn- bad
  ([value] value)
  ([other] other))
|}
  |> expect_error "defn bad has duplicate arity 1";
  Lg.Compiler.compile_string {|(defn- bad)|}
  |> expect_error "defn expects a name, parameter vector, and body"

let test_unannotated_function_parameters_infer_from_body () =
  let source =
    {|
(defn inc1 [x] (+ x 1))
(defn flip [flag] (not flag))
(println (str (inc1 41) ":" (flip false)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "unannotated_function_parameters_infer_from_body"
    "42:true\n" ocaml_source

let test_identity_function_is_polymorphic_at_call_sites () =
  let source =
    {|
(defn identity-value [x] x)
(println (str (identity-value 42) ":" (identity-value "Ada") ":" (identity-value true)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "identity_function_is_polymorphic_at_call_sites"
    "42:Ada:true\n" ocaml_source

let test_let_bound_identity_function_is_polymorphic_at_call_sites () =
  let source =
    {|
(println
  (let [identity-value (fn [x] x)]
    (str (identity-value 42) ":" (identity-value "Ada") ":" (identity-value true))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "let_bound_identity_function_is_polymorphic_at_call_sites"
    "42:Ada:true\n" ocaml_source

let test_conditional_function_is_polymorphic_at_call_sites () =
  let source =
    {|
(defn choose [flag left right]
  (if flag left right))
(println
  (str (+ (choose true 40 0) 2) ":"
       (choose false "Grace" "Ada")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "conditional_function_is_polymorphic_at_call_sites"
    "42:Ada\n" ocaml_source

let test_conditional_function_type_relationship_is_checked_by_ocaml () =
  Lg.Compiler.compile_string
    {|
(defn choose [flag left right]
  (if flag left right))
(def bad (choose true 42 "Ada"))
|}
  |> expect_error_contains "expected of type"

let test_unannotated_function_parameters_reject_bad_int_calls () =
  let source = {|
(defn inc1 [x] (+ x 1))
(def bad (inc1 "Ada"))
|} in
  Lg.Compiler.compile_string source
  |> expect_error "inc1 called with incompatible arguments"

let test_unannotated_function_parameters_use_clojure_truthiness () =
  let source =
    {|
(defn flip [flag] (not flag))
(println (str (flip 1) ":" (flip nil) ":" (flip false)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "unannotated_function_parameters_use_clojure_truthiness"
    "false:true:true\n" ocaml_source

let test_unannotated_function_parameters_infer_structural_map_fields () =
  let source =
    {|
(def user {:name "Ada", :age 36})
(defn next-age [person] (+ (:age person) 1))
(println (str (next-age user)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "unannotated_function_parameters_infer_structural_map_fields" "37\n"
    ocaml_source

let test_contextual_parameter_inference_preserves_nested_float_assoc_values () =
  let source =
    {|
(def score {:value 1.0})
(defn raise-score [score amount]
  (assoc score :value (+ amount 0.5)))
(println (str (:value (raise-score score 1.0))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "contextual_parameter_inference_preserves_nested_float_assoc_values" "1.5\n"
    ocaml_source

let test_top_level_defs_project_function_returned_structural_records_once () =
  let source =
    {|
(def score {:value 1.0})
(def calls (atom 0))
(defn raise-score [score amount]
  (do
    (reset! calls (+ (deref calls) 1))
    (assoc score :value (+ amount 0.5))))
(def updated (raise-score score 1.0))
(println (str (:value updated) ":" (deref calls)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "top_level_defs_project_function_returned_structural_records_once" "1.5:1\n"
    ocaml_source

let test_module_defs_project_function_returned_structural_records () =
  let source =
    {|
(module Scores
  (def score {:value 1.0})
  (defn raise-score [score amount]
    (assoc score :value (+ amount 0.5)))
  (def updated (raise-score score 1.0)))
(println (str (:value Scores/updated)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_defs_project_function_returned_structural_records"
    "1.5\n" ocaml_source

let test_unannotated_function_parameters_reject_missing_structural_map_fields ()
    =
  let source =
    {|
(def user {:name "Ada"})
(defn next-age [person] (+ (get person :age) 1))
(def bad (next-age user))
|}
  in
  Lg.Compiler.compile_string source
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "static_protocols_dispatch_by_receiver_type"
    "int:7:str:Ada\n" ocaml_source

let test_satisfies_question_checks_static_receivers () =
  let source =
    {|
(defprotocol Labelled (label [value] :string))
(extend-type :int Labelled (label [value] (str value)))
(println
  (str (satisfies? Labelled 7) ":"
       (satisfies? Labelled "seven")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "satisfies_question_checks_static_receivers" "true:false\n"
    ocaml_source

let test_satisfies_question_carries_a_generic_protocol_witness () =
  let source =
    {|
(defprotocol Labelled (label [value] :string))
(extend-type :int Labelled (label [value] (str value)))
(defn labelled? [value] (satisfies? Labelled value))
(println (str (labelled? 7) ":" (labelled? "seven")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "satisfies_question_carries_a_generic_protocol_witness"
    "true:false\n" ocaml_source

let test_satisfies_question_guards_generic_protocol_dispatch () =
  let source =
    {|
(defprotocol Labelled (label [value] :string))
(extend-type :int Labelled (label [value] (str "int:" value)))
(defn label-or-missing [value]
  (if (satisfies? Labelled value)
    (Labelled/label value)
    "missing"))
(println (str (label-or-missing 7) ":" (label-or-missing "seven")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "satisfies_question_guards_generic_protocol_dispatch"
    "int:7:missing\n" ocaml_source

let test_generic_protocol_witness_supports_multiple_methods () =
  let source =
    {|
(type-record pair-value (text :string) (number :int))
(defprotocol PairValue
  (pair-text [value] :string)
  (pair-number [value] :int))
(extend-type pair-value PairValue
  (pair-text [value] (:text value))
  (pair-number [value] (:number value)))
(defn summarize [value]
  (if (satisfies? PairValue value)
    (str (PairValue/pair-text value) ":" (PairValue/pair-number value))
    "missing"))
(def value (record pair-value (text "ready") (number 7)))
(println (str (summarize value) ":" (summarize 0)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_protocol_witness_supports_multiple_methods"
    "ready:7:missing\n" ocaml_source

let test_generic_protocol_witness_evaluates_receiver_once () =
  let source =
    {|
(type-record labelled-value (text :string))
(defprotocol Labelled (label [value] :string))
(extend-type labelled-value Labelled (label [value] (:text value)))
(def calls (atom 0))
(defn make-value []
  (do
    (swap! calls inc)
    (record labelled-value (text "ready"))))
(println (str (satisfies? Labelled (make-value)) ":" (deref calls)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_protocol_witness_evaluates_receiver_once"
    "true:1\n" ocaml_source

let test_generic_protocol_witness_flows_through_sequence_callbacks () =
  let source =
    {|
(type-record labelled-value (text :string))
(defprotocol Labelled (label [value] :string))
(extend-type labelled-value Labelled (label [value] (:text value)))
(defn label-or-missing [value]
  (if (satisfies? Labelled value)
    (Labelled/label value)
    "missing"))
(defn labels [values]
  (map label-or-missing values))
(def one (record labelled-value (text "one")))
(def two (record labelled-value (text "two")))
(println (pr-str (labels [one two])))
(println (pr-str (labels [1 2])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_protocol_witness_flows_through_sequence_callbacks"
    "(\"one\" \"two\")\n(\"missing\" \"missing\")\n" ocaml_source

let test_generic_protocol_witness_supports_parser_style_recursion () =
  let source =
    {|
(type-record leaf (text :string))
(defprotocol Traversable (walk-leaf [value] :string))
(extend-type leaf Traversable (walk-leaf [value] (:text value)))
(defn walk [value]
  (cond
    (satisfies? Traversable value) (Traversable/walk-leaf value)
    (sequential? value) (apply str (map walk value))
    :else "_"))
(def value (record leaf (text "leaf")))
(println (walk [value]))
(println (walk [1 2]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_protocol_witness_supports_parser_style_recursion"
    "leaf\n__\n" ocaml_source

let test_generic_protocol_witness_carries_callbacks_through_recursion () =
  let source =
    {|
(type-record leaf (text :string))
(defprotocol Traversable
  (walk-with [value f]))
(extend-type leaf Traversable
  (walk-with [value f] (f value)))
(defn walk [value f]
  (cond
    (satisfies? Traversable value) (Traversable/walk-with value f)
    (sequential? value) (walk (first value) f)
    :else (f value)))
(def value (record leaf (text "leaf")))
(def walked (walk [value] (fn [x] x)))
|}
  in
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dynamic_recursive_maps_support_assoc () =
  let source =
    {|
(type-record leaf (text :string))
(defprotocol Traversable (visit [value]))
(extend-type leaf Traversable (visit [value] value))
(defn rebuild [value]
  (cond
    (satisfies? Traversable value) (Traversable/visit value)
    (map? value) (assoc value :ready true)
    (seqable? value) (rebuild value)
    :else value))
(def rebuilt (rebuild {:answer 42}))
|}
  in
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_equality_infers_comparator_return_type () =
  let source =
    {|
(defn comparator-negative? [cmp values key]
  (neg? (cmp (unsafe-aget values 0) key)))
(defn matches [cmp values key]
  (let [_checked (comparator-negative? cmp values key)]
    (= 0 (cmp (unsafe-aget values 0) key))))
(println
  (str (matches (fn [left right] (- left right)) (array 4) 4) ":"
       (matches (fn [left right] (- left right)) (array 4) 3)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "equality_infers_comparator_return_type" "true:false\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dynamic_arrays_recover_generic_elements () =
  let source =
    {|
(defn first-plus-one [values]
  (+ (unsafe-aget values 0) 1))
(def int-box (assoc {} :value (array 1)))
(println (first-plus-one (get int-box :value)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_arrays_recover_generic_elements" "2\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_loop_nil_initial_value_can_become_optional () =
  let source =
    {|
(defn find-value []
  (loop [index 0
         result nil]
    (if (= index 1)
      result
      (recur (inc index) (Some 42)))))
(Stdlib.ignore (find-value))
(println "ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "loop_nil_initial_value_can_become_optional" "ok\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_loop_nil_initial_value_accepts_nullable_function_returns () =
  let source =
    {|
(defn maybe-values [value]
  (if (pos? value) (array value) nil))
(defn collect-values []
  (loop [index 0
         result nil]
    (if (= index 1)
      result
      (recur (inc index) (maybe-values 42)))))
(Stdlib.ignore (collect-values))
|}
  in
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_forward_declared_deftype_fields_keep_nominal_receiver () =
  let source =
    {|
(declare touch cleanup)
(deftype Cache [^clojure.lang.Associative key-value limit])
(defn touch [^Cache cache]
  (do
    (.valAt (.-key-value cache) :missing)
    (cleanup cache)))
(defn cleanup [^Cache cache]
  (if (> (count (.-key-value cache)) (.-limit cache))
    cache
    cache))
|}
  in
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_deftype_fields_accept_clojure_primitive_hints () =
  let source =
    {|
(deftype Metric [^int value ^boolean ready])
(defn usable? [^Metric metric]
  (and (pos? (.-value metric)) (.-ready metric)))
(println (usable? (Metric. 42 true)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "deftype_fields_accept_clojure_primitive_hints" "true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_deftype_unhinted_fields_preserve_dynamic_values () =
  let source =
    {|
(deftype Box [value])
(defn box-hash [^Box box]
  (hash (.-value box)))
(println (str (box-hash (Box. 42)) ":" (box-hash (Box. "abc"))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "deftype_unhinted_fields_preserve_dynamic_values"
    "1871679806:74834163\n" ocaml_source

let test_deftype_mutable_fields_support_set_bang () =
  let source =
    {|
(defprotocol MutableMetric
  (metric-value [metric] :int)
  (set-metric-value! [metric value] :int))
(deftype Metric [^:mutable ^int value]
  MutableMetric
  (metric-value [_] value)
  (set-metric-value! [_ next-value]
    (set! value next-value)))
(def metric (Metric. 1))
(set-metric-value! metric 42)
(println (.-value metric))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "deftype_mutable_fields_support_set_bang" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_deftype_methods_support_instance_call_syntax () =
  let source =
    {|
(deftype Box [^int value]
  Object
  (score [_] value)
  (doubleScore [this] (+ (.score this) (.score this))))
(println (.doubleScore (Box. 21)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "deftype_methods_support_instance_call_syntax" "42\n"
    ocaml_source

let test_defrecord_fields_infer_host_records_from_protocol_methods () =
  let source =
    {|
(type-record state (root :ref<option<int>>))
(defn root-value [value]
  (deref (:root value)))
(defprotocol Rooted
  (read-root [value]))
(do
  (defrecord Container [state])
  (extend-type Container
    Rooted
    (read-root [container]
      (root-value (.-state container)))))
(def container (Container. (record state (root (volatile! (Some 42))))))
(println (match (read-root container) (Some value) value None 0))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "defrecord_fields_infer_host_records_from_protocol_methods"
    "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_defrecord_fields_preserve_protocol_capabilities () =
  let source =
    {|
(defprotocol Searchable
  (search [value key]))
(type-record source (prefix :string))
(extend-type source
  Searchable
  (search [value key]
    (str (:prefix value) key)))
(do
  (defrecord Wrapper [source])
  (extend-type Wrapper
    Searchable
    (search [wrapper key]
      (search (.-source wrapper) key))))
(def wrapped (Wrapper. (record source (prefix "item-"))))
(println (search wrapped "42"))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "defrecord_fields_preserve_protocol_capabilities"
    "item-42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_defrecord_protocol_methods_support_forward_calls () =
  let source =
    {|
(defprotocol Measured
  (measure [value]))
(defprotocol Values
  (values [value]))
(do
  (defrecord Wrapper [items])
  (extend-type Wrapper
    Measured
    (measure [wrapper]
      (count (values wrapper)))
    Values
    (values [wrapper]
      (.-items wrapper))))
(def wrapped (Wrapper. [1 2 3]))
(println (measure wrapped))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "defrecord_protocol_methods_support_forward_calls" "3\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_defrecord_host_interfaces_support_overloaded_methods () =
  let source =
    {|
(do
  (defrecord Lookup [value])
  (extend-type Lookup
    clojure.lang.ILookup
    (valAt [lookup key]
      (if (= key :value) (.-value lookup) nil))
    (valAt [lookup key not-found]
      (if (= key :value) (.-value lookup) not-found))))
(def lookup (Lookup. 42))
(println (str (get lookup :value) ":" (get lookup :missing 7)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "defrecord_host_interfaces_support_overloaded_methods"
    "42:7\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_expression_type_hints_narrow_dynamic_records () =
  let source =
    {|
(defrecord Item [value])
(defn item-value [item]
  (.-value ^Item item))
(println (item-value (Item. 42)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "expression_type_hints_narrow_dynamic_records" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_opaque_records_project_safe_fields_without_casts () =
  let source =
    {|
(type-record state
  (root :ref<option<int>>)
  (label :string))
(defrecord Holder [value])
(defn held-state ^state [holder]
  (.-value ^Holder holder))
(def state-value
  (record state
    (root (volatile! (Some 1)))
    (label "ready")))
(println (.-label (held-state (Holder. state-value))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "opaque_records_project_safe_fields_without_casts" "ready\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_instance_question_recognizes_namespaced_protocol_interfaces () =
  let source =
    {|
(ns example.protocols)
(defprotocol Labelled
  (label [value]))
(type-record item (name :string))
(extend-type item Labelled
  (label [value] (:name value)))
(def value (record item (name "ready")))
(println (instance? example.protocols.Labelled value))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "instance_question_recognizes_namespaced_protocol_interfaces" "true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_generic_collection_returns_preserve_concrete_element_types () =
  let source =
    {|
(type-record address-store
  (addresses :array<option<int64>>)
  (delete :fn<array<int64>;unit>))
(defn address-value [candidate]
  (match candidate
    (Some address) address
    None (Stdlib.failwith "missing address")))
(defn address-option-equals? [candidate address]
  (match candidate
    (Some value) (= value address)
    None false))
(defn address-present? [candidate]
  (match candidate
    (Some address) (address-option-equals? candidate address)
    None false))
(defn removed-addresses [addresses]
  (into-array
    (map address-value
      (filter address-present? (array-to-seq addresses)))))
(defn delete-addresses [store]
  (let [delete (:delete store)]
    (delete (removed-addresses (:addresses store)))))
|}
  in
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_java_exception_constructors_map_to_runtime_exceptions () =
  let source =
    {|
(println
  (try
    (throw (UnsupportedOperationException. "not supported"))
    (catch _ "caught")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "java_exception_constructors_map_to_runtime_exceptions"
    "caught\n" ocaml_source

let test_java_exception_constructors_support_empty_messages () =
  let source =
    {|
(println
  (try
    (throw (IndexOutOfBoundsException.))
    (catch _ "caught")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "java_exception_constructors_support_empty_messages"
    "caught\n" ocaml_source

let test_print_method_defmethod_writes_custom_record_representations () =
  let source =
    {|
(deftype Person [name])
(defmethod print-method Person [^Person person ^java.io.Writer writer]
  (.write writer "#person ")
  (binding [*out* writer]
    (pr (.-name person))))
(println (pr-str (Person. "Ada")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "print_method_defmethod_writes_custom_record_representations"
    "#person \"Ada\"\n" ocaml_source

let test_java_writer_annotations_work_in_ordinary_functions () =
  let source =
    {|
(defn write-prefix [^java.io.Writer writer]
  (.write writer "#person "))
(defn write-values [^java.io.Writer writer values]
  (binding [*out* writer]
    (apply pr values)))
(deftype Person [name])
(defmethod print-method Person [^Person person ^java.io.Writer writer]
  (write-prefix writer)
  (write-values writer [[(.-name person)] [:ok]]))
(println (pr-str (Person. "Ada")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "java_writer_annotations_work_in_ordinary_functions"
    "#person [\"Ada\"] [:ok]\n" ocaml_source

let test_clojure_sorted_annotations_expose_dynamic_comparators () =
  let source =
    {|
(type-record sorted-box
  (comparator :fn<int;int;int>))
(defn comparator-for [sets key]
  (.comparator ^clojure.lang.Sorted (get sets key)))
(def cmp
  (comparator-for
    (hash-map :box (record sorted-box (comparator (fn [left right] (- left right)))))
    :box))
(println (cmp 7 2))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_sorted_annotations_expose_dynamic_comparators"
    "5\n" ocaml_source

let test_defn_accepts_attribute_maps_and_return_hints () =
  let source =
    {|
(defn add
  {:inline (fn [x y] (list '+ x y))}
  ^long [x y]
  (long (+ x y)))
(println (add 20 22))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "defn_accepts_attribute_maps_and_return_hints" "42\n"
    ocaml_source

let test_compare_supports_dynamic_scalar_values () =
  let source =
    {|
(defn cmp [x y]
  (if (nil? x) 0 (if (nil? y) 0 (compare x y))))
(println (str (cmp 1 2) ":" (cmp "b" "a") ":" (cmp :a :a)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "compare_supports_dynamic_scalar_values" "-1:1:0\n"
    ocaml_source

let test_class_and_identical_support_dynamic_values () =
  let source =
    {|
(defn same-class? [x y] (identical? (class x) (class y)))
(defn class-name [^Object x]
  (if (nil? x) x (.getName (. x (getClass)))))
(println (str (same-class? 1 2) ":" (same-class? 1 "1") ":"
              (class-name 1) ":" (type 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "class_and_identical_support_dynamic_values"
    "true:false:java.lang.Long:java.lang.Long\n" ocaml_source

let test_clojure_static_dot_calls_support_hasheq () =
  let source =
    {|
(println (= (hash :answer) (. clojure.lang.Util (hasheq :answer))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_static_dot_calls_support_hasheq" "true\n"
    ocaml_source

let test_clojure_number_and_comparable_interop () =
  let source =
    {|
(defn value-compare [x y]
  (cond
    (instance? Number x) (clojure.lang.Numbers/compare x y)
    (instance? Comparable x) (.compareTo x y)
    :else 0))
(println (str (value-compare 1 2) ":" (value-compare "b" "a")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_number_and_comparable_interop" "-1:1\n"
    ocaml_source

let test_clojure_equals_interop_supports_dynamic_values () =
  let source =
    {|
(defn string-equals [^Object value] (.equals "a" value))
(defn keyword-equals [^Object value] (.equals :a value))
(println (str (string-equals "a") ":" (string-equals 1) ":"
              (keyword-equals :a)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_equals_interop_supports_dynamic_values"
    "true:false:true\n" ocaml_source

let test_identical_and_equals_infer_heterogeneous_parameters () =
  let source =
    {|
(defn tx-id? [value]
  (or (identical? :db/current-tx value)
      (.equals ":db/current-tx" value)
      (.equals "datascript.tx" value)))
(println (str (tx-id? :db/current-tx) ":" (tx-id? "datascript.tx") ":"
              (tx-id? 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "identical_and_equals_infer_heterogeneous_parameters"
    "true:true:false\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_try_supports_clojure_exception_type_bindings () =
  let source =
    {|
(def diff
  (try
    1
    (catch ClassCastException _ :incomparable)))
(println
  (try
    (try
      (raise (Invalid_argument "bad"))
      (catch ClassCastException error (throw error)))
    (catch _ "caught")))
(println (str (= diff :incomparable) ":" (== diff 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "try_supports_clojure_exception_type_bindings"
    "caught\nfalse:true\n" ocaml_source

let test_macros_can_clear_form_metadata () =
  let source =
    {|
(defmacro without-meta [value]
  (with-meta value {}))
(println (without-meta ^long 42))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "macros_can_clear_form_metadata" "42\n" ocaml_source

let test_macros_can_apply_functions_to_argument_sequences () =
  let source =
    {|
(defmacro emit-call [function & arguments]
  (apply list function arguments))
(println (emit-call + 20 22))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "macros_can_apply_functions_to_argument_sequences" "42\n"
    ocaml_source

let test_cond_contextualizes_anonymous_function_branches () =
  let source =
    {|
(defn predicate-for [value]
  (cond
    (string? value) (fn [candidate] (string? candidate))
    (nil? value) (fn [candidate] (nil? candidate))
    :else (fn [candidate] (= value candidate))))
(println (str ((predicate-for "a") "b") ":"
              ((predicate-for nil) nil) ":"
              ((predicate-for 1) 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "cond_contextualizes_anonymous_function_branches"
    "true:true:true\n" ocaml_source

let test_callable_expressions_are_evaluated_once () =
  let source =
    {|
(def calls (volatile! 0))
(def result
  ((do (vswap! calls inc) (fn [value] (+ value 1))) 41))
(println (str result ":" (deref calls)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "callable_expressions_are_evaluated_once" "42:1\n"
    ocaml_source

let test_extend_type_supports_multiple_protocol_groups () =
  let source =
    {|
(defprotocol LeftValue (left-value [value]))
(defprotocol RightValue (right-value [value]))
(deftype PairValue [left right])
(extend-type PairValue
  LeftValue (left-value [value] (.-left value))
  RightValue (right-value [value] (.-right value)))
(def pair (PairValue. 20 22))
(println (+ (left-value pair) (right-value pair)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "extend_type_supports_multiple_protocol_groups" "42\n"
    ocaml_source

let test_clojure_map_entry_compiles_as_two_element_vector () =
  let source = {|(println (pr-str (clojure.lang.MapEntry :answer 42)))|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_map_entry_compiles_as_two_element_vector"
    "[:answer 42]\n" ocaml_source

let test_set_literals_are_callable_as_membership_lookup () =
  let source =
    {|
(println
  (str (#{:e :a :v} :a) ":"
       (nil? (#{:e :a :v} :missing))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "set_literals_are_callable_as_membership_lookup" ":a:true\n"
    ocaml_source

let test_static_sets_accept_dynamic_lookup_values () =
  let source =
    {|
(def schema-attr? #{:db/id :db/ident})
(defrecord Datom [^:dynamic a])
(defn schema-datom? [datom]
  (schema-attr? (.-a ^Datom datom)))
(println (some? (schema-datom? (Datom. :db/id))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "static_sets_accept_dynamic_lookup_values" "true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_generic_protocol_witness_compiles_for_javascript_targets () =
  let source =
    {|
(type-record labelled-value (text :string))
(defprotocol Labelled (label [value] :string))
(extend-type labelled-value Labelled (label [value] (:text value)))
(defn label-or-missing [value]
  (if (satisfies? Labelled value)
    (Labelled/label value)
    "missing"))
(def value (record labelled-value (text "ready")))
(println (label-or-missing value))
|}
  in
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source
    |> expect_ok)

let test_protocols_support_float_and_symbol_receivers () =
  let source =
    {|
(defprotocol Labelled (label [value] :string))
(extend-type :float Labelled
  (label [value] (str "float:" value)))
(extend-type :symbol Labelled
  (label [value] (str "symbol:" (name value))))
(println (str (label 2.5) ":" (label (symbol "ready"))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "protocols_support_float_and_symbol_receivers"
    "float:2.5:symbol:ready\n" ocaml_source

let test_protocols_support_generic_host_constructor_receivers () =
  let source =
    {|
(defprotocol Described (describe [value] :string))
(extend-type :option<int> Described
  (describe [value]
    (match value
      (Some number) (str "some:" number)
      None "none")))
(println (str (describe (Some 7)) ":" (describe None)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "protocols_support_generic_host_constructor_receivers"
    "some:7:none\n" ocaml_source

let test_protocols_support_external_ocaml_receivers () =
  let source =
    {|
(defprotocol Sized (byte-size [value] :int))
(extend-type :Unix/stats Sized
  (byte-size [value] (:st-size value)))
(defn file-size [^:Unix/stats stats]
  (byte-size stats))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  if not (string_contains_substring ocaml_source ".st_size") then
    failwith
      "external protocol implementation should compile native field access"

let test_protocols_reject_duplicate_host_constructor_implementations () =
  Lg.Compiler.compile_string
    {|
(defprotocol Described (describe [value] :string))
(extend-type :option<int> Described
  (describe [value] "first"))
(extend-type :option<string> Described
  (describe [value] "second"))
|}
  |> expect_error
       "duplicate implementation of Described/describe for option<string>"

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
  Lg.Compiler.compile_string source
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
  Lg.Compiler.compile_string source
  |> expect_error "protocol method label must return string"

let test_static_protocols_work_through_module_aliases () =
  let source =
    {|
(module Labels
  (defprotocol Labelled
    (label [x] :string))
  (extend-type :int
    Labelled
    (label [x] (str "int:" x))))
(module-alias L Labels)
(println (L/Labelled/label 9))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "static_protocols_work_through_module_aliases" "int:9\n"
    ocaml_source

let test_protocols_work_through_chained_module_aliases () =
  let source =
    {|
(module Labels
  (defprotocol Labelled (label [x] :string))
  (extend-type :int Labelled (label [x] (str "int:" x))))
(module-alias L Labels)
(module-alias LL L)
(println (LL/Labelled/label 9))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "protocols_work_through_chained_module_aliases" "int:9\n"
    ocaml_source

let test_protocols_work_through_module_local_aliases () =
  let source =
    {|
(module Labels
  (defprotocol Labelled (label [x] :string))
  (extend-type :int Labelled (label [x] (str "int:" x))))
(module App
  (module-alias L Labels)
  (def result (L/Labelled/label 9)))
(println App/result)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "protocols_work_through_module_local_aliases" "int:9\n"
    ocaml_source

let test_protocol_identity_disambiguates_same_named_methods () =
  let source =
    {|
(defprotocol Display
  (render [x] :string))
(defprotocol Debug
  (render [x] :string))
(extend-type :int
  Display
  (render [x] (str "display:" x)))
(extend-type :int
  Debug
  (render [x] (str "debug:" x)))
(println (str (Display/render 7) ":" (Debug/render 7)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "protocol_identity_disambiguates_same_named_methods"
    "display:7:debug:7\n" ocaml_source

let test_ambiguous_protocol_methods_require_explicit_identity () =
  let source =
    {|
(defprotocol Display (render [x] :string))
(defprotocol Debug (render [x] :string))
(extend-type :int Display (render [x] (str x)))
(extend-type :int Debug (render [x] (str x)))
(def value (render 7))
|}
  in
  Lg.Compiler.compile_string source
  |> expect_error_contains
       "ambiguous protocol method render; use Protocol/method"

let test_protocol_signatures_check_all_parameter_types () =
  let source =
    {|
(defprotocol Join
  (join [^:int value ^:string suffix] :string))
(extend-type :int
  Join
  (join [value ^:int suffix] (str value suffix)))
|}
  in
  Lg.Compiler.compile_string source
  |> expect_error_contains "protocol method join parameter 2 must be string"

let test_protocols_support_named_record_receivers () =
  let source =
    {|
(type-record user (name :string))
(defprotocol Labelled
  (label [value] :string))
(extend-type user
  Labelled
  (label [value] (:name value)))
(def ada (record user (name "Ada")))
(println (label ada))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "protocols_support_named_record_receivers" "Ada\n"
    ocaml_source

let test_named_record_updates_preserve_protocol_identity () =
  let source =
    {|
(type-record user (name :string) (age :int))
(defprotocol Labelled (label [value] :string))
(extend-type user Labelled
  (label [value] (str (:name value) ":" (:age value))))
(def ada (record user (name "Ada") (age 41)))
(def older (assoc ada :age 42))
(println (label older))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "named_record_updates_preserve_protocol_identity" "Ada:42\n"
    ocaml_source

let test_keyword_access_reads_nominal_record_fields () =
  let source =
    {|
(type-record user (name :string))
(type-record project (name :string))
(def ada (record user (name "Ada")))
(def lg (record project (name "lg")))
(println (str (:name ada) ":" (:name lg)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "keyword_access_reads_nominal_record_fields" "Ada:lg\n"
    ocaml_source;
  Lg.Compiler.compile_string
    {|
(type-record user (name :string))
(def ada (record user (name "Ada")))
(def missing (:missing ada))
|}
  |> expect_error "unknown record field missing"

let test_concise_external_type_paths_defer_to_ocaml () =
  Lg.Compiler.compile_string
    {|
(defn identity [^:External/value value] value)
|}
  |> expect_error_contains "Unbound module External"

let test_named_record_parameters_are_inferred_for_record_updates () =
  let source =
    {|
(type-record block
  (id :string)
  (indent :int)
  (parent-id :option<string>))
(defn move [block ^:int indent ^:option<string> parent-id]
  (assoc block :indent (max 0 indent) :parent-id parent-id))
(def original
  (record block (id "block-1") (indent 1) (parent-id None)))
(def moved (move original -2 (Some "parent")))
(println
  (str (:id moved) ":" (:indent moved) ":"
    (match (:parent-id moved)
      None "none"
      (Some parent-id) parent-id)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "named_record_parameters_are_inferred_for_record_updates"
    "block-1:0:parent\n" ocaml_source

let test_module_local_named_record_parameters_are_inferred () =
  let source =
    {|
(module Domain
  (type-record user (name :string))
  (defn rename [user ^:string name]
    (assoc user :name name))
  (def ada (record user (name "Ada"))))
(def renamed (Domain/rename Domain/ada "Grace"))
(println (:name renamed))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_local_named_record_parameters_are_inferred"
    "Grace\n" ocaml_source

let test_protocols_inside_modules_export_methods_and_record_impls () =
  let source =
    {|
(module Domain
  (type-record user (name :string))
  (defprotocol Labelled
    (label [value] :string))
  (extend-type user
    Labelled
    (label [value] (:name value)))
  (def ada (record user (name "Ada"))))
(println (Domain/Labelled/label Domain/ada))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "protocols_inside_modules_export_methods_and_record_impls"
    "Ada\n" ocaml_source

let test_protocols_reject_duplicate_method_declarations () =
  {|
(defprotocol Labelled
  (label [x] :string)
  (label [x] :string))
|}
  |> Lg.Compiler.compile_string
  |> expect_error "protocol Labelled declares duplicate method label"

let test_protocols_reject_duplicate_implementations () =
  {|
(defprotocol Labelled (label [x] :string))
(extend-type :int Labelled (label [x] (str x)))
(extend-type :int Labelled (label [x] (str x)))
|}
  |> Lg.Compiler.compile_string
  |> expect_error "duplicate implementation of Labelled/label for int"

let test_protocol_implementations_reject_emitted_name_collisions () =
  Lg.Compiler.compile_string
    {|
(defprotocol foo-bar (label [value] :string))
(defprotocol foo_bar (label [value] :string))
(extend-type :int foo-bar (label [value] (str value)))
(extend-type :int foo_bar (label [value] (str value)))
|}
  |> expect_error_contains "OCaml protocol implementation name collision"

let test_protocols_reject_duplicate_methods_in_one_extension () =
  {|
(defprotocol Labelled (label [x] :string))
(extend-type :int Labelled
  (label [x] (str x))
  (label [x] (str x)))
|}
  |> Lg.Compiler.compile_string
  |> expect_error "duplicate implementation of Labelled/label for int"

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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "do_and_multi_form_bodies"
    "inside-let\ninside-do\ninput:41\nresult:42\n" ocaml_source

let test_fn_empty_body_returns_nil () =
  let source =
    {|
(def f (fn [x]))
(defn g
  ([x])
  ([x y] y))
(println (str (nil? (f 1)) ":" (nil? (g 1)) ":" (= (g 1 42) 42)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "fn_empty_body_returns_nil" "true:true:true\n" ocaml_source

let test_vectors_support_mixed_element_types () =
  let source = {|(println (pr-str [1 "two"]))|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "vectors_support_mixed_element_types" "[1 \"two\"]\n"
    ocaml_source

let test_keyword_values_print_as_keywords () =
  let source = {|
(println (str :admin? ":" (pr-str :admin?)))
|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "keyword_values_print_as_keywords" ":admin?::admin?\n"
    ocaml_source

let test_keys_return_keyword_values () =
  let source =
    {|
(def user {:name "Ada", :age 36})
(println (pr-str (keys user)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "vals_return_homogeneous_values" "[1 2]:[1 2 3]:[10 20]\n"
    ocaml_source

let test_vals_accept_dynamic_maps () =
  let source =
    {|
(defrecord Box [values])
(def static-values
  (vals (persistent! (transient (hash-map "a" 1 "b" 2)))))
(def dynamic-values
  (vals (:values (Box. (hash-map "a" 1 "b" 2)))))
(println
  (str (= (count static-values) 2) ":"
       (= (count dynamic-values) 2)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "vals_accept_dynamic_maps" "true:true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_vals_rejects_heterogeneous_values () =
  Lg.Compiler.compile_string {|(def xs (vals {:name "Ada", :age 36}))|}
  |> expect_error "vals requires all map values to have the same type"

let test_vectors_support_mixed_keyword_and_string_elements () =
  let source = {|(println (pr-str [:name "name"]))|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "vectors_support_mixed_keyword_and_string_elements"
    "[:name \"name\"]\n" ocaml_source

let test_arithmetic_rejects_non_int_arguments () =
  Lg.Compiler.compile_string {|(def x (+ 1 "two"))|}
  |> expect_error "expected int arguments for +"

let test_arithmetic_core_arities () =
  let source =
    {|
(println (str (+) ":" (*) ":" (+ 1 2 3) ":" (- 5) ":" (- 10 3 2) ":" (/ 8 2 2)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "arithmetic_core_arities" "0:1:6:-5:5:2\n" ocaml_source

let test_integer_division_rejects_unsupported_arities () =
  Lg.Compiler.compile_string {|(def x (/ 10))|}
  |> expect_error "/ expects at least 2 arguments"

let test_chained_comparisons () =
  let source =
    {|
(println (str (< 1 2 3) ":" (< 1 3 2) ":" (= 1 1 1) ":" (= 1 1 2)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "chained_comparisons" "true:false:true:false\n" ocaml_source

let test_not_equal_core_api () =
  let source =
    {|
(println (str (not= 1 2) ":" (not= "Ada" "Ada") ":" (not= :name :age) ":"
              (not= true true false) ":" (not= 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "not_equal_core_api" "true:false:true:true:false\n"
    ocaml_source

let test_not_equal_rejects_mixed_types () =
  Lg.Compiler.compile_string {|(def x (not= 1 "1"))|}
  |> expect_error "not= arguments must have the same type: int, string"

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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "collection_equality_core_api"
    "true:false:true:true:true:true\n" ocaml_source

let test_get_rejects_unknown_map_fields () =
  let source = {|(def user {:name "Ada"})(def x (get user :age))|} in
  Lg.Compiler.compile_string source |> expect_error "unknown field :age"

let test_get_supports_default_values () =
  let source =
    {|
(def user {:name "Ada", :age 36})
(println (str (get user :age 0) ":" (get user :admin? false)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "get_supports_default_values" "36:false\n" ocaml_source

let test_get_rejects_default_type_mismatch_for_known_fields () =
  Lg.Compiler.compile_string {|(def x (get {:age 36} :age "unknown"))|}
  |> expect_error "get default for :age must be int"

let test_get_supports_vectors () =
  let source =
    {|
(def xs [10 20 30])
(println (str (get xs 1) ":" (get xs 9 99)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "get_supports_vectors" "20:99\n" ocaml_source

let test_get_rejects_vector_default_type_mismatch () =
  Lg.Compiler.compile_string {|(def x (get [1 2] 9 "missing"))|}
  |> expect_error "get default for vector must match element type"

let test_assoc_supports_multiple_pairs () =
  let source =
    {|
(def user {:name "Ada"})
(def updated (assoc user :age 36 :admin? true))
(println (str (:name updated) ":" (:age updated) ":" (:admin? updated)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "assoc_supports_multiple_pairs" "Ada:36:true\n" ocaml_source

let test_assoc_rejects_odd_key_value_pairs () =
  Lg.Compiler.compile_string {|(def bad (assoc {:name "Ada"} :age))|}
  |> expect_error "assoc expects map followed by keyword/value pairs"

let test_assoc_supports_vector_indexes () =
  let source =
    {|
(def xs [1 2 3])
(def ys (assoc xs 0 10 2 30))
(println (pr-str ys))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "assoc_supports_vector_indexes" "[10 2 30]\n" ocaml_source

let test_assoc_rejects_vector_value_type_mismatch () =
  Lg.Compiler.compile_string {|(def x (assoc [1 2] 0 "one"))|}
  |> expect_error "assoc vector value must match element type"

let test_assoc_rejects_vector_non_int_indexes () =
  Lg.Compiler.compile_string {|(def x (assoc [1 2] "0" 9))|}
  |> expect_error "assoc vector index must be int"

let test_dissoc_supports_multiple_keys () =
  let source =
    {|
(def user {:name "Ada", :age 36, :admin? true})
(def slim (dissoc user :age :admin?))
(println (str (:name slim) ":" (count slim)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "map_merge_update_and_select_keys" "Ada:true:38:2\n"
    ocaml_source

let test_merge_rejects_incompatible_overlapping_fields () =
  let source = {|(def bad (merge {:age 36} {:age "old"}))|} in
  Lg.Compiler.compile_string source
  |> expect_error "cannot merge :age as string because it is already int"

let test_update_rejects_type_changes () =
  let source =
    {|
(defn stringify-age [x] (str x))
(def bad (update {:age 36} :age stringify-age))
|}
  in
  Lg.Compiler.compile_string source
  |> expect_error "cannot update :age as string because it is already int"

let test_update_supports_extra_arguments () =
  let source =
    {|
(def user {:name "Ada", :age 36})
(def older (update user :age + 1))
(println (str (:name older) ":" (:age older)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "update_supports_extra_arguments" "Ada:37\n" ocaml_source

let test_keyword_let_bindings_preserve_static_map_access () =
  let source =
    {|
(def result
  (let [record {:age 40}
        key :age]
    (assoc record key (+ (get record key) 2))))
(println (:age result))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "keyword_let_bindings_preserve_static_map_access" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_assoc_packs_values_for_dynamic_record_fields () =
  let source =
    {|
(defrecord Holder [^:dynamic payload])
(def container (Holder. {:answer 1}))
(def result (assoc container :payload {:answer 42}))
(println (get (:payload result) :answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "assoc_packs_values_for_dynamic_record_fields" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_assoc_accepts_protocol_constrained_named_records () =
  let source =
    {|
(defprotocol HasValue
  (read-value [value] :int))
(defrecord state [value items]
  HasValue
  (read-value [state] (+ (.-value state) 0)))
(defrecord other-state [value]
  HasValue
  (read-value [state] (+ (.-value state) 0)))
(defn add-item [items item]
  (conj items item))
(defn replace-value [state]
  (if (read-value state)
    (-> state
      (update :items add-item 2)
      (assoc :value 42))
    state))
(def initial (state. 1 [1]))
(let [updated (replace-value initial)]
  (println [(:value updated) (count (:items updated))]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "assoc_accepts_protocol_constrained_named_records" "[42 2]\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_nested_named_records_resolve_protocol_receivers () =
  let source =
    {|
(defprotocol IDB
  (-attrs-by [db property]))
(defrecord DB []
  IDB
  (-attrs-by [_db property] [property]))
(defrecord TxReport [^DB db-after])
(defn has-tuples? [report]
  (not (empty? (-attrs-by (:db-after report) :db.type/tuple))))
(println (has-tuples? (TxReport. (DB.))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nested_named_records_resolve_protocol_receivers" "true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_update_preserves_named_records_with_opaque_fields () =
  let source =
    {|
(type-record tree [value]
  (item :value))
(type-record database
  (root :ref<option<tree<int>>>)
  (max-eid :int)
  (schema :dynamic))
(defprotocol IDB
  (valid-db? [db] :bool))
(extend-type database IDB
  (valid-db? [_db] true))
(defrecord TxReport [^database db-after])
(def tx0 100)
(defn new-eid? [db eid]
  (and (> eid (:max-eid db))
       (< eid tx0)))
(defn advance-max-eid [db eid]
  (cond-> db
    (new-eid? db eid)
    (assoc :max-eid eid)))
(defn update-rschema [db]
  (assoc db :max-eid 42))
(defn update-schema [db _datom]
  db)
(defn keep-schema [schema]
  schema)
(defn checked-db [db valid?]
  (if valid?
    (update-in db [:schema] keep-schema)
    (throw (ex-info "invalid db" {:error :invalid-db}))))
(defn with-datom [db datom]
  (satisfies? IDB db)
  (let [schema? true]
    (if true
      (cond-> db
        true (advance-max-eid datom)
        schema? (-> (update-schema datom)
                    (checked-db true)
                    update-rschema))
      db)))
(defn allocate-eid [report eid]
  (let [m report
        k :db-after]
    (assoc m k (advance-max-eid (get m k) eid))))
(def initial-db
  (record database
    (root (volatile! (Some (record tree (item 7)))))
    (max-eid 1)
    (schema {})))
(def initial-report (TxReport. initial-db))
(def updated
  (allocate-eid initial-report 42))
(def updated-db
  (with-datom (:db-after updated) 42))
(def db updated-db)
(println
  [(:max-eid db)
   (match (deref (:root db))
     (Some root) (:item root)
     None 0)])
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "update_preserves_named_records_with_opaque_fields"
    "[42 7]\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_defrecord_field_hints_reject_unknown_record_types () =
  Lg.Compiler.compile_string {|(defrecord Holder [^Missing value])|}
  |> expect_error_contains "unknown record type Missing"

let test_defrecord_preserves_extension_map_entries () =
  let source =
    {|
(defrecord Report [value])
(defn extend-report [report]
  (let [current (or (:extra report) 40)]
    (assoc report :extra (inc current))))
(def base (Report. 1))
(def extended (extend-report base))
(def updated (update extended :extra inc))
(def cleaned (dissoc updated :extra))
(println
  [(:value updated)
   (:extra updated)
   (contains? cleaned :extra)
   (count (keys updated))
   (count (keys cleaned))])
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "defrecord_preserves_extension_map_entries"
    "[1 42 false 2 1]\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_assoc_in_updates_nested_maps () =
  let source =
    {|
(defn set-score [db user-id score]
  (assoc-in db [:users user-id :score] score))
(def result (set-score {:users {1 {:score 1}}} 1 42))
(println (get-in result [:users 1 :score]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "assoc_in_updates_nested_maps" "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_assoc_in_preserves_named_records_with_references () =
  let source =
    {|
(defrecord State [^:dynamic schema root])
(def initial (State. {:old 1} (volatile! nil)))
(defn update-state [db key value]
  (let [schema (or (:schema db) {})]
    (if (schema key)
      (-> db (assoc-in [:schema key] value))
      (-> db (assoc-in [:schema key] value)))))
(def result (update-state initial :answer 42))
(println (get (:schema result) :answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "assoc_in_preserves_named_records_with_references" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_threaded_forms_accumulate_record_fields () =
  let source =
    {|
(defn add-fields [report]
  (-> report
    (assoc :first 20)
    (assoc :second 22)))
(def result (add-fields {:initial 0 :first 0 :second 0}))
(println (+ (:first result) (:second result)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "threaded_forms_accumulate_record_fields" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_update_infers_record_fields_from_updater_functions () =
  let source =
    {|
(defn advance [db eid]
  (assoc db :max-eid eid))
(defn allocate [report eid]
  (update report :db-after advance eid))
(println (:max-eid (:db-after (allocate {:db-after {:max-eid 1}} 42))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "update_infers_record_fields_from_updater_functions" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_update_works_as_a_nested_map_updater () =
  let source =
    {|
(def result (update {:inner {:count 40}} :inner update :count + 2))
(println (:count (:inner result)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "update_works_as_a_nested_map_updater" "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_update_works_as_a_nested_vector_updater () =
  let source =
    {|
(def result (update {:items [1 2 3]} :items update 1 + 40))
(println (nth (:items result) 1))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "update_works_as_a_nested_vector_updater" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nested_update_passes_all_extra_arguments () =
  let source =
    {|
(defn combine [current x y] (+ current (+ x y)))
(def result (update {:inner {:count 10}} :inner update :count combine 20 12))
(println (:count (:inner result)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nested_update_passes_all_extra_arguments" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_update_rejects_extra_argument_type_mismatch () =
  let source = {|(def bad (update {:age 36} :age + "one"))|} in
  Lg.Compiler.compile_string source
  |> expect_error
       "update function arguments do not match field and extra arguments"

let test_update_supports_vector_indexes () =
  let source =
    {|
(def xs [1 2 3])
(def ys (update xs 1 + 40))
(println (pr-str ys))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "update_supports_vector_indexes" "[1 42 3]\n" ocaml_source

let test_update_rejects_vector_index_type_mismatch () =
  Lg.Compiler.compile_string {|(def x (update [1 2] "0" inc))|}
  |> expect_error "update vector index must be int"

let test_select_keys_rejects_unknown_fields () =
  Lg.Compiler.compile_string {|(def bad (select-keys {:name "Ada"} [:age]))|}
  |> expect_error "cannot select unknown field :age"

let test_contains_supports_vector_indexes () =
  let source =
    {|
(def xs [1 2])
(println (str (contains? xs 0) ":" (contains? xs 2) ":" (contains? xs -1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "contains_supports_vector_indexes" "true:false:false\n"
    ocaml_source

let test_contains_rejects_vector_non_int_indexes () =
  Lg.Compiler.compile_string {|(def x (contains? [1 2] "0"))|}
  |> expect_error "contains? vector index must be int"

let test_if_supports_mixed_branch_types () =
  let source = {|(println (pr-str (if true 1 "one")))|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "if_supports_mixed_branch_types" "1\n" ocaml_source

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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "conditional_forms_work" "when-fired\nopen:ready\n"
    ocaml_source

let test_if_not_supports_mixed_branch_types () =
  let source = {|(println (pr-str (if-not true 1 "one")))|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "if_not_supports_mixed_branch_types" "\"one\"\n"
    ocaml_source

let test_cond_returns_nil_without_else () =
  let ocaml_source =
    Lg.Compiler.compile_string {|(println (nil? (cond false 1)))|} |> expect_ok
  in
  assert_ocaml_runs "cond_returns_nil_without_else" "true\n" ocaml_source

let test_cond_supports_mixed_branch_types () =
  let source = {|(println (pr-str (cond false 1 :else "one")))|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "cond_supports_mixed_branch_types" "\"one\"\n" ocaml_source

let test_cond_accepts_clojure_truthy_tests () =
  let ocaml_source =
    Lg.Compiler.compile_string {|(println (cond 1 "one" :else "fallback"))|}
    |> expect_ok
  in
  assert_ocaml_runs "cond_accepts_clojure_truthy_tests" "one\n" ocaml_source

let test_when_returns_nullable_value () =
  let ocaml_source =
    Lg.Compiler.compile_string
      {|(println (if-some [value (when true 1)] value 0))|}
    |> expect_ok
  in
  assert_ocaml_runs "when_returns_nullable_value" "1\n" ocaml_source

let test_when_not_negates_the_condition () =
  let source =
    {|
(def skipped (when-not true 1))
(def value (when-not false (+ 2 2)))
(println (str (if-some [x value] x 0) ":" (nil? skipped)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "when_not_negates_the_condition" "4:true\n" ocaml_source

let test_conditional_forms_accept_truthy_params () =
  let source =
    {|
(defn status [flag]
  (if-not flag "closed" "open"))
(println (str (status 1) ":" (status false)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "conditional_forms_accept_truthy_params" "open:closed\n"
    ocaml_source

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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "batched_core_functions_work"
    "true:true:true:true:true:true:false:5:-2:3:1:4:1:7:4:-1:8:4:true:false:true:false:true:false:true:false:true:false:true:false\n"
    ocaml_source

let test_batched_core_functions_reject_non_int_arguments () =
  Lg.Compiler.compile_string {|(def x (zero? "0"))|}
  |> expect_error "expected int arguments for zero?"

let test_batched_core_functions_reject_bad_arities () =
  Lg.Compiler.compile_string {|(def x (quot 1))|}
  |> expect_error "quot expects 2 arguments"

let test_batched_core_functions_infer_int_params () =
  let source =
    {|
(defn shifted [x] (bit-shift-left x 1))
(def bad (shifted "1"))
|}
  in
  Lg.Compiler.compile_string source
  |> expect_error "shifted called with incompatible arguments"

let test_batched_numeric_scalar_core_functions_work () =
  let source =
    {|
(println
  (str (integer? 1) ":" (integer? "1") ":"
       (nat-int? 0) ":" (nat-int? -1) ":" (nat-int? "0") ":"
       (pos-int? 1) ":" (pos-int? 0) ":"
       (neg-int? -1) ":" (neg-int? 0) ":"
       (boolean true) ":" (boolean false) ":" (boolean "x") ":"
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "batched_numeric_scalar_core_functions_work"
    "true:false:true:false:false:true:false:true:false:true:false:true:4:5:0:true:false:4611686018427387903:3:3:2:2:12:12:3:1:5:5:3:3:-4:-4:name:Ada::admin?::ready\n"
    ocaml_source

let test_batched_numeric_scalar_core_functions_reject_non_int_bit_args () =
  Lg.Compiler.compile_string {|(def x (bit-set 1 "2"))|}
  |> expect_error "expected int arguments for bit-set"

let test_hash_combine_matches_clojure_32_bit_overflow () =
  let source =
    {|
(println
  (str (hash-combine 0 0) ":"
       (clojure.lang.Util/hashCombine 1 2) ":"
       (hash-combine -1 42) ":"
       (hash-combine 2147483647 2147483647)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "hash_combine_matches_clojure_32_bit_overflow"
    "-1640531527:-1640531462:1640531549:1103660680\n" ocaml_source

let test_hash_matches_clojure_scalar_and_collection_values () =
  let source =
    {|
(println
  (str (hash nil) ":" (hash true) ":" (hash false) ":"
       (hash 1) ":" (hash -1) ":" (hash 42) ":"
       (hash 1.5) ":" (hash "abc") ":" (hash :db/ident) ":"
       (hash [1 2]) ":" (hash (list 1 2)) ":"
       (hash (hash-set 1 2)) ":" (hash {:a 1 :b 2}) ":"
       (= (hash-unordered-coll [1 2])
          (hash-unordered-coll [2 1]))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "hash_matches_clojure_scalar_and_collection_values"
    "0:1231:1237:1392991556:1651860712:1871679806:1073217536:74834163:-737096:156247261:156247261:460223544:161871944:true\n"
    ocaml_source

let test_numeric_double_equals_supports_mixed_numbers () =
  let source = {|(println (str (== 1 1) ":" (== 1 1.0) ":" (== 1 2)))|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "numeric_double_equals_supports_mixed_numbers"
    "true:true:false\n" ocaml_source

let test_case_supports_dynamic_keyword_and_string_targets () =
  let source =
    {|
(defn choose [^:dynamic value]
  (case value
    :answer 1
    "answer" 2
    0))
(println (str (choose :answer) ":" (choose "answer") ":" (choose :missing)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "case_supports_dynamic_keyword_and_string_targets" "1:2:0\n"
    ocaml_source

let test_batched_numeric_scalar_core_functions_reject_unchecked_arity () =
  Lg.Compiler.compile_string {|(def x (unchecked-add 1))|}
  |> expect_error "unchecked-add expects 2 arguments"

let test_batched_numeric_scalar_core_functions_reject_bad_name_arg () =
  Lg.Compiler.compile_string {|(def x (name 1))|}
  |> expect_error "name expects keyword, string, or symbol"

let test_batched_numeric_scalar_core_functions_infer_int_params () =
  let source =
    {|
(defn clear-second [x] (bit-clear x 1))
(def bad (clear-second "7"))
|}
  in
  Lg.Compiler.compile_string source
  |> expect_error "clear-second called with incompatible arguments"

let test_clojure_string_module_batch_works () =
  let source =
    {|
(require [clojure.string :as str])
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_string_module_batch_works"
    "ADA|ada|Ada|cba|hi|left|right|line|baNANA|baNAna|$1\n\
     true:true:true:true:2:4:[\"a\" \"b\" \"c\"]:[\"a\" \"b\"]\n"
    ocaml_source

let test_clojure_string_module_refer_works () =
  let source =
    {|
(require [clojure.string :refer [upper-case trim]])
(println (str (upper-case (trim " ada "))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_string_module_refer_works" "ADA\n" ocaml_source

let test_clojure_string_module_rejects_bad_args () =
  Lg.Compiler.compile_string
    {|
(require [clojure.string :as str])
(def x (str/upper-case 1))
|}
  |> expect_error "str/upper-case called with incompatible arguments"

let test_clojure_string_module_rejects_unknown_refer () =
  Lg.Compiler.compile_string {|
(require [clojure.string :refer [missing]])
|}
  |> expect_error "cannot refer unknown symbol clojure.string/missing"

let test_clojure_walk_preserves_collections_and_traversal_order () =
  let source =
    {|
(ns walk-example
  (:require [clojure.walk :as walk]))

(defn replace-two [^:dynamic value]
  (if (= value 2) 20 value))

(println
  (= {:items [1 {:value 20}]
      :values #{20}}
     (walk/postwalk replace-two
       {:items [1 {:value 2}]
        :values #{2}})))

(defn expand [^:dynamic value]
  (cond
    (= value 1) [2]
    (= value 2) 20
    :else value))

(println (= [[2]] (walk/postwalk expand [1])))
(println (= [[20]] (walk/prewalk expand [1])))
(println
  (= [:new 20]
     (walk/walk replace-two (fn [^:dynamic value] value) [:new 2])))
(println
  (= (assoc {} :new 1)
     (walk/postwalk
       (fn [^:dynamic value] (if (= value :old) :new value))
       {:old 1})))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_walk_preserves_collections_and_traversal_order"
    "true\ntrue\ntrue\ntrue\ntrue\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_clojure_data_diff_matches_recursive_collection_semantics () =
  let source =
    {|
(ns data-example
  (:require [clojure.data :as data]))

(println (pr-str (data/diff 1 1)))
(println (pr-str (data/diff 1 2)))
(println (pr-str (data/diff {:a 1 :b 2} {:a 1 :b 3 :c 4})))
(println (pr-str (data/diff [1 2] [1 3 4])))
(println (pr-str (data/diff #{1 2} #{2 3})))
(println (pr-str (data/diff [1] {:a 1})))

(deftype ComparableBox [value]
  clojure.data/EqualityPartition
  (equality-partition [_] :comparable-box)
  clojure.data/Diff
  (diff-similar [_ _]
    ["left" "right" "custom"]))

(println (pr-str (data/diff (ComparableBox. 1) (ComparableBox. 2))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_data_diff_matches_recursive_collection_semantics"
    "[nil nil 1]\n\
     [1 2 nil]\n\
     [{:b 2} {:b 3, :c 4} {:a 1}]\n\
     [[nil 2] [nil 3 4] [1]]\n\
     [#{1} #{3} #{2}]\n\
     [[1] {:a 1} nil]\n\
     [\"left\" \"right\" \"custom\"]\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_batched_predicate_collection_core_functions_work () =
  let source =
    {|
(def xs [1 2 3 4 5])
(def split (split-at 2 xs))
(def splitw (split-with (fn [x] (< x 4)) xs))
(def parts (partition-by (fn [x] (even? x)) [1 3 2 4 5]))
(println
  (str (any? 1) ":" (rational? 1) ":" (rational? "1") ":"
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
       (do (dorun xs) "done") ":" (pr-str (doall xs))))
(run! (fn [^:int x] (println (str "item:" x))) [1 2])
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "batched_predicate_collection_core_functions_work"
    "true:true:false:false:false:false:false:true:false:true:false:true:true:true:true:false:true:false:false:3:5:[1 \
     2 3 4]:[4 5]:[1 2 3]:[1 3 5]:2:[1 2]:[3 4 5]:[1 2 3]:[4 5]:3:2:2:done:[1 \
     2 3 4 5]\n\
     item:1\n\
     item:2\n"
    ocaml_source

let test_batched_predicate_collection_core_functions_reject_bad_counts () =
  Lg.Compiler.compile_string {|(def x (take-nth 0 [1 2]))|}
  |> expect_error "take-nth n must be positive"

let test_batched_predicate_collection_core_functions_reject_bad_predicates () =
  Lg.Compiler.compile_string
    {|(def x (split-with (fn [^:string s] true) [1 2]))|}
  |> expect_error "split-with expects a predicate matching collection elements"

let test_batched_predicate_collection_core_functions_reject_bad_run_function ()
    =
  Lg.Compiler.compile_string
    {|(def x (run! (fn [^:string s] (println s)) [1 2]))|}
  |> expect_error "run! function type does not match collection"

let test_doseq_infers_seqable_parameters () =
  let source =
    {|
(defn consume-entries [entries key]
  (doseq [entry entries]
    nil)
  (get entries key))
(println (consume-entries {:answer 42} :answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "doseq_infers_seqable_parameters" "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_for_supports_when_clauses () =
  let source =
    {|
(println (pr-str (for [x [1 2 3 4] :when (even? x)] x)))
(println
  (pr-str
    (for [x [1 2 3] :let [y (+ x 10)] :when (odd? x)] y)))
(println
  (pr-str
    (for [x [1 2]
          y [10 20]
          :when (= y 20)
          :when (odd? (+ x y))]
      (+ x y))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "for_supports_when_clauses" "(2 4)\n(11 13)\n(21)\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_merge_accepts_dynamic_map_parameters () =
  let source =
    {|
(defn combine [schema]
  (merge {:implicit true} schema))
(def combined (combine {:answer 42}))
(println (str (get combined :implicit) ":" (get combined :answer)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "merge_accepts_dynamic_map_parameters" "true:42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_record_arguments_fill_missing_optional_fields () =
  let source =
    {|
(defn option-value [opts]
  (if-some [storage (:storage opts)]
    1
    2))
(println (option-value {}))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "record_arguments_fill_missing_optional_fields" "2\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_reify_preserves_protocols_across_dynamic_fields () =
  let source =
    {|
(defprotocol Value
  (-value [this]))
(defn make-value []
  (reify Value
    (-value [_] 42)))
(defrecord Holder [value])
(def holder (Holder. (make-value)))
(println (-value (.-value holder)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "reify_preserves_protocols_across_dynamic_fields" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_parameters_preserve_multiple_protocol_constraints () =
  let source =
    {|
(defprotocol LeftValue
  (-left [this]))
(defprotocol RightValue
  (-right [this]))
(defrecord Pair []
  LeftValue
  (-left [_] 20)
  RightValue
  (-right [_] 22))
(defn total [value]
  (+ (-left value) (-right value)))
(println (total (Pair.)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "parameters_preserve_multiple_protocol_constraints" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_references_preserve_state_across_dynamic_fields () =
  let source =
    {|
(defrecord State [counter])
(def state (State. (atom 0)))
(println (deref (.-counter state)))
(println (zero? (deref (.-counter state))))
(reset! (.-counter state) 4)
(println (swap! (.-counter state) inc))
(println (deref (.-counter state)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "references_preserve_state_across_dynamic_fields"
    "0\ntrue\n5\n5\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_truthy_guards_preserve_dynamic_numeric_parameters () =
  let source =
    {|
(defn max-present [left right]
  (if (and right (> right left)) right left))
(println (max-present 1 nil))
(println (max-present 1 2))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "truthy_guards_preserve_dynamic_numeric_parameters" "1\n2\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_let_aliases_propagate_seqable_constraints () =
  let source =
    {|
(defn sum-values [record]
  (let [values (:values record)]
    (reduce + 0 values)))
(println (sum-values {:values [1 2 3]}))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "let_aliases_propagate_seqable_constraints" "6\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nested_drop_while_infers_seqable_parameters () =
  let source =
    {|
(defn first-valid [values]
  (first (drop-while (fn [value] (< value 2)) values)))
(println (first-valid [1 2 3]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nested_drop_while_infers_seqable_parameters" "2\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_loop_initializers_propagate_seqable_constraints () =
  let source =
    {|
(defn first-loop [values]
  (loop [remaining (seq values)]
    (first remaining)))
(println (+ (first-loop [42]) 0))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "loop_initializers_propagate_seqable_constraints" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_batched_predicate_collection_core_functions_accept_truthy_params () =
  let source =
    {|
(defn prefix [flag xs] (split-with (fn [x] flag) xs))
(println (str (count (first (prefix 1 [1 2]))) ":"
              (count (second (prefix 1 [1 2]))) ":"
              (count (first (prefix false [1 2]))) ":"
              (count (second (prefix false [1 2])))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "batched_predicate_collection_core_functions_accept_truthy_params"
    "2:0:0:2\n" ocaml_source

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
(defn namespace-or-empty [value]
  (if-let [ns (namespace value)] ns ""))
(println
  (str (name qualified) ":" (namespace-or-empty qualified) ":" (name kw) ":" (namespace-or-empty kw) ":"
       (name kw2) ":" (namespace-or-empty kw2) ":" (pr-str more-names) ":"
       (:name m1) ":" (:ready m2) ":" (pr-str s1) ":" (pr-str listed) ":"
       (symbol? simple) ":" (symbol? :ready) ":"
       (simple-symbol? simple) ":" (simple-symbol? qualified) ":"
       (qualified-symbol? qualified) ":" (qualified-symbol? simple) ":"
       (ident? simple) ":" (simple-ident? simple) ":" (qualified-ident? qualified)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "batched_identifier_and_constructor_core_functions_work"
    "name:user:name:user:id:user:[ready user/name]:Ada:true:#{1 2 3}:(1 2 3 \
     4):true:false:true:false:true:false:true:true:true\n"
    ocaml_source

let test_batched_identifier_and_constructor_core_functions_reject_bad_symbol_args
    () =
  Lg.Compiler.compile_string {|(def x (symbol 1))|}
  |> expect_error "symbol expects string, keyword, or symbol"

let test_batched_identifier_and_constructor_core_functions_reject_bad_keyword_args
    () =
  Lg.Compiler.compile_string {|(def x (keyword "user" 1))|}
  |> expect_error
       "keyword namespace and name must be string, keyword, or symbol"

let test_batched_identifier_and_constructor_core_functions_reject_bad_namespace_args
    () =
  Lg.Compiler.compile_string {|(def x (namespace 1))|}
  |> expect_error "namespace expects keyword or symbol"

let test_namespace_accepts_guarded_dynamic_identifiers () =
  let source =
    {|
(defn namespace-if-keyword [value]
  (if (keyword? value)
    (if-let [ns (namespace value)] (= ns "user") false)
    false))
(println
  (str (namespace-if-keyword :user/name) ":"
       (namespace-if-keyword "user/name")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "namespace_accepts_guarded_dynamic_identifiers"
    "true:false\n" ocaml_source

let test_batched_identifier_and_constructor_core_functions_reject_bad_list_star_tail
    () =
  Lg.Compiler.compile_string {|(def x (list* 1 2 3))|}
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
       (pr-str (cons 0 [1 2])) ":"
       (:missing {:x 1} 9) ":"
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
       (reduce-kv (fn [acc i x] (+ acc (+ i x))) 0 [10 20]) ":"
       (if-some [values (not-empty [1 2])] (count values) 0) ":"
       (nil? (not-empty []))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "batched_sequence_functions_work"
    "[1 3]:[1 2 3]:[3 4]:(1 2 3):(1 2 3):(1 2 3 4):(0 1 2):9:[1 2]:#{1 \
     2}:(\"x\" \"x\" \"x\"):(7 7 7):(1 0 2 0 3):(1 3 2 4):2:1:3:3:1:(0 1 3 \
     6):[1 2 1]:(10 21):[1 3]:[2 3]:31:2:true\n"
    ocaml_source

let test_sort_accepts_dynamic_collections () =
  let source =
    {|
(defrecord Box [values])
(def sorted (sort (:values (Box. [3 1 2]))))
(println (pr-str sorted))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sort_accepts_dynamic_collections" "(1 2 3)\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_metadata_map_values_constrain_function_parameters () =
  let source =
    {|
(defn attach-source [obj source]
  (with-meta obj {:source source}))
(println (pr-str (:source (meta (attach-source {:x 1} [1 2])))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "metadata_map_values_constrain_function_parameters"
    "[1 2]\n" ocaml_source

let test_logical_or_preserves_nullable_dynamic_results () =
  let source =
    {|
(type-record left-value (value :int))
(type-record right-value (value :string))
(defn parse-left [pick-left?]
  (when pick-left? (record left-value (value 1))))
(defn parse-right [pick-left?]
  (when (not pick-left?) (record right-value (value "right"))))
(defn parse-either [pick-left?]
  (or (parse-left pick-left?) (parse-right pick-left?)))
(println (some? (parse-either true)))
(println (some? (parse-either false)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "logical_or_preserves_nullable_dynamic_results"
    "true\ntrue\n" ocaml_source

let test_match_coerces_nullable_branches () =
  let source =
    {|
(type-record match-value (value :int))
(defn choose [key]
  (case key
    :value (record match-value (value 1))
    nil))
(println (some? (choose :value)))
(println (nil? (choose :missing)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "match_coerces_nullable_branches" "true\ntrue\n"
    ocaml_source

let test_lazy_map_defers_incrementally_and_memoizes_realized_values () =
  let source =
    {|
(def calls (atom 0))
(def mapped
  (map
    (fn [x]
      (do
        (reset! calls (+ (deref calls) 1))
        (+ x 1)))
    [1 2 3]))
(println (deref calls))
(println (first mapped))
(println (first mapped))
(println (deref calls))
(println (second mapped))
(println (deref calls))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "lazy_map_defers_incrementally_and_memoizes_realized_values"
    "0\n2\n2\n1\n3\n2\n" ocaml_source

let test_lazy_filter_realizes_only_enough_source_values () =
  let source =
    {|
(def calls (atom 0))
(def evens
  (filter
    (fn [x]
      (do
        (reset! calls (+ (deref calls) 1))
        (even? x)))
    [1 2 3 4]))
(println (deref calls))
(println (first evens))
(println (deref calls))
(println (first evens))
(println (deref calls))
(println (second evens))
(println (deref calls))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "lazy_filter_realizes_only_enough_source_values"
    "0\n2\n2\n2\n2\n4\n4\n" ocaml_source

let test_lazy_take_bounds_infinite_range_and_repeat () =
  let source =
    {|
(println (pr-str (take 5 (range))))
(println (pr-str (take 3 (repeat "x"))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "lazy_take_bounds_infinite_range_and_repeat"
    "(0 1 2 3 4)\n(\"x\" \"x\" \"x\")\n" ocaml_source

let test_lazy_map_accepts_all_builtin_seqable_types () =
  let source =
    {|
(def host-seq
  (List.to_seq (list 4 5)))
(println (pr-str (map inc (list 1 2))))
(println (pr-str (map inc [1 2])))
(println (pr-str (map inc (hash-set 2 1))))
(println (pr-str (map inc (array 1 2))))
(println (pr-str (map (fn [ch] (str ch)) "ab")))
(println (pr-str (map inc host-seq)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "lazy_map_accepts_all_builtin_seqable_types"
    "(2 3)\n(2 3)\n(2 3)\n(2 3)\n(\"a\" \"b\")\n(5 6)\n" ocaml_source

let test_ocaml_seq_unfold_builds_typed_lazy_sequences () =
  let source =
    {|
(def values
  (seq-unfold
    (fn [state]
      (if (< state 4)
        (Some (tuple state (inc state)))
        nil))
    1))
(println (reduce + 0 values))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_seq_unfold_builds_typed_lazy_sequences" "6\n"
    ocaml_source;
  Lg.Compiler.compile_string {|(seq-unfold (fn [x] (inc x)) 0)|}
  |> expect_error_contains
       "seq-unfold expects a state step function and initial state"

let test_ocaml_array_sequences_flat_map_lazily () =
  let source =
    {|
(def arrays
  (array
    (array 1 2)
    (array 3 4)))
(def values
  (seq-flat-map
    (fn [items] (array-to-seq items))
    arrays))
(println
  (str (reduce + 0 values) ":"
       (reduce
         (fn [acc value] (+ (* acc 10) value))
         0
         (array-to-rseq (array 1 2 3)))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_array_sequences_flat_map_lazily" "10:321\n"
    ocaml_source

let test_ocaml_uncurried_call_emits_melange_direct_application () =
  let source =
    {|
(defn add-two [left right] (+ left right))
(println (uncurried-call add-two 2 3))
|}
  in
  let ocaml_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok
  in
  if not (string_contains_substring ocaml_source "[@u") then
    failwith "expected an uncurried Melange application";
  Lg.Compiler.compile_string {|(uncurried-call (fn [x] x) 1 2)|}
  |> expect_error_contains
       "uncurried-call expects a binary function and two compatible arguments"

let test_reduce_accepts_all_builtin_seqable_types () =
  let source =
    {|
(def host-seq
  (List.to_seq (list 4 5)))
(println (reduce (fn [acc x] (+ acc x)) 0 (list 1 2)))
(println (reduce (fn [acc x] (+ acc x)) 0 [1 2]))
(println (reduce (fn [acc x] (+ acc x)) 0 (hash-set 2 1)))
(println (reduce (fn [acc x] (+ acc x)) 0 (array 1 2)))
(println (reduce (fn [acc ch] (str acc ch)) "" "ab"))
(println (reduce (fn [acc x] (+ acc x)) 0 host-seq))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "reduce_accepts_all_builtin_seqable_types"
    "3\n3\n3\n3\nab\n9\n" ocaml_source

let test_reduce_realizes_lazy_seq_once () =
  let source =
    {|
(def calls (atom 0))
(def values
  (map
    (fn [x]
      (do
        (reset! calls (+ (deref calls) 1))
        x))
    [1 2 3]))
(println (deref calls))
(println (reduce + 0 values))
(println (deref calls))
(println (reduce + 0 values))
(println (deref calls))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "reduce_realizes_lazy_seq_once" "0\n6\n3\n6\n3\n"
    ocaml_source

let test_reduced_values_support_predicates_and_unwrapping () =
  let source =
    {|
(def stopped (reduced 7))
(println
  (str (reduced? stopped) ":" (reduced? 7) ":"
       (unreduced stopped) ":" (unreduced 8)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "reduced_values_support_predicates_and_unwrapping"
    "true:false:7:8\n" ocaml_source

let test_reduce_stops_without_realizing_remaining_values () =
  let source =
    {|
(def calls (atom 0))
(def values
  (map
    (fn [x]
      (do
        (reset! calls (+ (deref calls) 1))
        x))
    [1 2 3 4 5]))
(def total
  (reduce
    (fn [acc x]
      (if (> x 3)
        (reduced acc)
        (+ acc x)))
    0
    values))
(println total)
(println (deref calls))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "reduce_stops_without_realizing_remaining_values" "6\n4\n"
    ocaml_source

let test_nil_initialized_reduce_returns_nullable_reduced_value () =
  let source =
    {|
(defn find [pred xs]
  (reduce
    (fn [_ x]
      (when (pred x)
        (reduced x)))
    nil
    xs))
(def found (find #(> % 2) [1 2 3 4]))
(println (str (= 3 found) ":" (nil? (find #(> % 9) [1 2 3]))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nil_initialized_reduce_returns_nullable_reduced_value"
    "true:true\n" ocaml_source

let test_reduce_short_circuits_builtin_and_custom_seqable_types () =
  let source =
    {|
(type-record cursor (values :list<int>))
(extend-type cursor Seqable
  (-seq [cursor]
    (map (fn [x] (+ x 0)) (:values cursor))))
(def custom (record cursor (values (list 1 2 3 4))))
(defn sum-before-three [values]
  (reduce
    (fn [acc x]
      (if (= x 3) (reduced acc) (+ acc x)))
    0
    values))
(def text
  (reduce
    (fn [acc ch]
      (if (= ch \c) (reduced acc) (str acc ch)))
    ""
    "abcd"))
(println
  (str (sum-before-three (list 1 2 3 100)) ":"
       (sum-before-three [1 2 3 100]) ":"
       (sum-before-three (array 1 2 3 100)) ":"
       (sum-before-three custom) ":" text ":"
       (reduce (fn [acc x] (reduced (+ acc x))) 10 (list-of :int))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "reduce_short_circuits_builtin_and_custom_seqable_types"
    "3:3:3:3:ab:10\n" ocaml_source

let test_custom_records_can_implement_core_seqable () =
  let source =
    {|
(type-record cursor (values :list<int>))
(extend-type cursor Seqable
  (-seq [cursor]
    (map (fn [x] x) (:values cursor))))
(def values (record cursor (values (list 1 2 3))))
(println (pr-str (map inc values)))
(println (reduce (fn [acc x] (+ acc x)) 0 values))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "custom_records_can_implement_core_seqable" "(2 3 4)\n6\n"
    ocaml_source

let test_modules_export_core_seqable_implementations () =
  let source =
    {|
(module Cursors
  (type-record cursor (values :list<int>))
  (extend-type cursor Seqable
    (-seq [cursor]
      (map (fn [x] x) (:values cursor))))
  (def values (record cursor (values (list 4 5)))))
(println (pr-str (map inc Cursors/values)))
(println (reduce (fn [acc x] (+ acc x)) 0 Cursors/values))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "modules_export_core_seqable_implementations" "(5 6)\n9\n"
    ocaml_source

let test_reduce_prefers_custom_reducible_over_seqable () =
  let source =
    {|
(def seq-calls (atom 0))
(type-record cursor (values :list<int>))
(extend-type cursor Seqable
  (-seq [cursor]
    (do
      (reset! seq-calls (+ (deref seq-calls) 1))
      (map (fn [x] x) (:values cursor)))))
(extend-type cursor Reducible
  (-reduce [cursor reducer init]
    (+ init 100)))
(def values (record cursor (values (list 1 2 3))))
(println (reduce (fn [acc x] (+ acc x)) 0 values))
(println (deref seq-calls))
(println (first (map inc values)))
(println (deref seq-calls))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "reduce_prefers_custom_reducible_over_seqable"
    "100\n0\n2\n1\n" ocaml_source

let test_reduce_specializes_builtin_reducible_types () =
  let source =
    {|
(def list-total (reduce (fn [acc x] (+ acc x)) 0 (list 1 2)))
(def vector-total (reduce (fn [acc x] (+ acc x)) 0 [1 2]))
(def array-total (reduce (fn [acc x] (+ acc x)) 0 (array 1 2)))
(def string-value (reduce (fn [acc ch] (str acc ch)) "" "ab"))
(def seq-total (reduce (fn [acc x] (+ acc x)) 0 (range 3)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  [
    "List.fold_left";
    "Rrbvec.fold_left";
    "Array.fold_left";
    "String.fold_left";
    "Seq.fold_left";
  ]
  |> List.iter (fun expected ->
         if not (string_contains_substring ocaml_source expected) then
           failwith ("missing specialized reducible call " ^ expected))

let test_reduce_infers_destructured_items_when_collection_is_generic () =
  let source =
    {|
(defn pair-total [values]
  (reduce
    (fn [total [left right]] (+ total left right))
    0
    values))
(println (pair-total [[1 2] [3 4]]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "reduce_infers_destructured_items_when_collection_is_generic" "10\n"
    ocaml_source

let test_count_prefers_custom_counted_over_seqable () =
  let source =
    {|
(def seq-calls (atom 0))
(type-record cursor (values :list<int>))
(extend-type cursor Seqable
  (-seq [cursor]
    (do
      (reset! seq-calls (+ (deref seq-calls) 1))
      (map (fn [x] x) (:values cursor)))))
(extend-type cursor Counted
  (-count [cursor] 3))
(def values (record cursor (values (list 1 2 3))))
(println (count values))
(println (deref seq-calls))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "count_prefers_custom_counted_over_seqable" "3\n0\n"
    ocaml_source

let test_first_and_last_accept_all_seqable_types () =
  let source =
    {|
(type-record cursor (values :list<int>))
(extend-type cursor Seqable
  (-seq [cursor]
    (map (fn [x] x) (:values cursor))))
(def values (record cursor (values (list 4 5 6))))
(def host-seq
  (List.to_seq (list 7 8)))
(println (str (+ (first values) 0) ":" (+ (last values) 0)))
(println (str (first (array 1 2)) ":" (last (array 1 2))))
(println (str (first "ab") ":" (last "ab")))
(println (str (+ (first host-seq) 0) ":" (+ (last host-seq) 0)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "first_and_last_accept_all_seqable_types"
    "4:6\n1:2\na:b\n7:8\n" ocaml_source

let test_custom_records_can_implement_core_indexed () =
  let source =
    {|
(type-record cursor (values :list<int>))
(extend-type cursor Indexed
  (-nth [cursor index]
    (+ (List.nth (:values cursor) index) 0)))
(def values (record cursor (values (list 4 5 6))))
(println (nth values 1))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "custom_records_can_implement_core_indexed" "5\n"
    ocaml_source

let test_nth_accepts_indexed_and_seqable_host_types () =
  let source =
    {|
(def host-seq
  (List.to_seq (list 7 8 9)))
(println (nth (array 1 2 3) 1))
(println (str (nth "abc" 1)))
(println (+ (nth host-seq 2) 0))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nth_accepts_indexed_and_seqable_host_types" "2\nb\n9\n"
    ocaml_source

let test_generic_sequence_functions_infer_seqable_dictionaries () =
  let source =
    {|
(type-record cursor (values :list<int>))
(extend-type cursor Seqable
  (-seq [cursor]
    (map (fn [x] x) (:values cursor))))
(defn total [values]
  (reduce + 0 values))
(defn increment-all [values]
  (map inc values))
(defn size [values]
  (count values))
(defn forwarded-total [values]
  (total values))
(def custom (record cursor (values (list 4 5))))
(def host-seq
  (List.to_seq (list 6 7)))
(println (str (total (list 1 2)) ":" (total [1 2]) ":"
              (total (array 1 2)) ":" (total custom) ":"
              (total host-seq)))
(println (pr-str (increment-all custom)))
(println (str (size [1 2 3]) ":" (size custom)))
(println (forwarded-total (array 8 9)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_sequence_functions_infer_seqable_dictionaries"
    "3:3:3:9:13\n(5 6)\n3:2\n17\n" ocaml_source

let test_generic_seqable_returns_instantiate_element_types () =
  let source =
    {|
(defn head [values] (first values))
(defn tail-value [values] (last values))
(println (+ (head [4 5]) 1))
(println (+ (tail-value (array 6 7)) 1))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_seqable_returns_instantiate_element_types" "5\n8\n"
    ocaml_source

let test_seqable_dictionary_arguments_evaluate_once () =
  let source =
    {|
(def calls (atom 0))
(defn total [values] (reduce + 0 values))
(println
  (total
    (do
      (reset! calls (+ (deref calls) 1))
      [1 2 3])))
(println (deref calls))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "seqable_dictionary_arguments_evaluate_once" "6\n1\n"
    ocaml_source

let test_modules_export_host_ocaml_seqable_implementations () =
  let source =
    {|
(module QueueSeq
  (extend-type :Queue.t<int> Seqable
    (-seq [queue]
      (Queue.to_seq queue))))
(def values
  (Queue.of_seq (List.to_seq (list 1 2 3))))
(println (pr-str (map inc values)))
(println (reduce (fn [acc x] (+ acc x)) 0 values))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "modules_export_host_ocaml_seqable_implementations"
    "(2 3 4)\n6\n" ocaml_source

let test_logseq_datascript_style_wrappers_use_collection_capabilities () =
  let source =
    {|
(module Datascript
  (type-record query-result (rows :list<int>))
  (extend-type query-result Seqable
    (-seq [result]
      (map (fn [row] row) (:rows result))))
  (extend-type query-result Counted
    (-count [result]
      (+ (List.length (:rows result)) 0))))
(module Logseq
  (type-record block-children (blocks :array<int>))
  (extend-type block-children Seqable
    (-seq [children]
      (map (fn [block] block) (:blocks children))))
  (extend-type block-children Counted
    (-count [children]
      (+ (Array.length (:blocks children)) 0))))
(defn summarize [values]
  (str (count values) ":" (reduce + 0 values) ":" (first values) ":" (last values)))
(def query
  (record Datascript.query-result (rows (list 1 2 3))))
(def children
  (record Logseq.block-children (blocks (array 4 5))))
(println (summarize query))
(println (summarize children))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "logseq_datascript_style_wrappers_use_collection_capabilities"
    "3:6:1:3\n2:9:4:5\n" ocaml_source

let test_sequence_navigation_accepts_all_seqable_types () =
  let source =
    {|
(type-record datom (fields :list<int>))
(extend-type datom Seqable
  (-seq [datom]
    (map (fn [field] (+ field 0)) (:fields datom))))
(def value (record datom (fields (list 1 2 3))))
(def host-seq
  (List.to_seq (list 7 8 9)))
(println (pr-str (seq value)))
(println (pr-str (rest value)))
(println (pr-str (next value)))
(println (second value))
(println (pr-str (nthnext value 2)))
(println (pr-str (nthrest value 3)))
(println (pr-str (rest (array 4 5 6))))
(println (str (second "ab")))
(println (+ (second host-seq) 0))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sequence_navigation_accepts_all_seqable_types"
    "(1 2 3)\n(2 3)\n(2 3)\n2\n(3)\n()\n(5 6)\nb\n8\n" ocaml_source

let test_generic_sequence_navigation_infers_seqable_dictionaries () =
  let source =
    {|
(type-record datom (fields :list<int>))
(extend-type datom Seqable
  (-seq [datom]
    (map (fn [field] (+ field 0)) (:fields datom))))
(defn tail [values] (rest values))
(defn next-tail [values] (next values))
(defn item-two [values] (second values))
(defn forwarded-tail [values] (tail values))
(defn no-values? [values] (empty? values))
(def value (record datom (fields (list 1 2 3))))
(println (pr-str (tail value)))
(println (pr-str (next-tail (array 4 5 6))))
(println (+ (item-two value) 0))
(println (pr-str (forwarded-tail (list 7 8 9))))
(println (str (no-values? value) ":" (no-values? (array-of :int))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_sequence_navigation_infers_seqable_dictionaries"
    "(2 3)\n(5 6)\n2\n(8 9)\nfalse:true\n" ocaml_source

let test_sequence_navigation_handles_empty_seqable_values () =
  let source =
    {|
(println (pr-str (seq (list-of :int))))
(println (pr-str (rest (vector-of :int))))
(println (pr-str (next (array-of :int))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sequence_navigation_handles_empty_seqable_values"
    "()\n()\nnil\n" ocaml_source

let test_generic_sequence_navigation_evaluates_arguments_once () =
  let source =
    {|
(def calls (atom 0))
(defn tail [values] (rest values))
(println
  (pr-str
    (tail
      (do
        (reset! calls (+ (deref calls) 1))
        [1 2 3]))))
(println (deref calls))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_sequence_navigation_evaluates_arguments_once"
    "(2 3)\n1\n" ocaml_source

let test_batched_sequence_functions_reject_type_mismatch () =
  Lg.Compiler.compile_string {|(def x (concat [1] ["two"]))|}
  |> expect_error "concat element types must match"

let test_batched_sequence_functions_reject_bad_functions () =
  Lg.Compiler.compile_string {|(def x (filterv (fn [^:string s] true) [1 2]))|}
  |> expect_error "filterv expects a predicate matching collection elements"

let test_batched_sequence_functions_reject_bad_counts () =
  Lg.Compiler.compile_string {|(def x (repeat "3" 1))|}
  |> expect_error "repeat count must be int"

let test_batched_sequence_functions_reject_bad_partition_size () =
  Lg.Compiler.compile_string {|(def x (partition 0 [1 2]))|}
  |> expect_error "partition size must be positive"

let test_batched_sequence_functions_reject_reduce_kv_non_collection () =
  Lg.Compiler.compile_string
    {|(def x (reduce-kv (fn [acc i x] (+ acc x)) 0 (list 1 2)))|}
  |> expect_error "reduce-kv expects a vector or map"

let test_interleave_accepts_multiple_collections () =
  let source =
    {|
(def xs (interleave [1 2 3] (list 10 20) (hash-set 100 200 300)))
(println (pr-str xs))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "interleave_accepts_multiple_collections"
    "(1 10 100 2 20 200)\n" ocaml_source

let test_interleave_rejects_later_type_mismatches () =
  Lg.Compiler.compile_string {|(def x (interleave [1] (list 2) ["three"]))|}
  |> expect_error "interleave element types must match"

let test_interleave_requires_two_collections () =
  Lg.Compiler.compile_string {|(def x (interleave [1 2]))|}
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
       (some? (some (fn [x] (> x 3)) xs)) ":"
       (nil? (some (fn [x] (> x 9)) xs)) ":"
       (pr-str (reductions + [1 2 3 4]))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "additional_sequence_helpers_work"
    "(2 3 4):(3 4):(4):1:[3 4]:(2):1:5:[4 3 2 1]:true:true:(1 3 6 10)\n"
    ocaml_source

let test_additional_sequence_helpers_reject_bad_counts () =
  Lg.Compiler.compile_string {|(def x (nthnext [1 2] "1"))|}
  |> expect_error "nthnext count must be int"

let test_some_returns_first_truthy_predicate_value () =
  let source =
    {|
(def found
  (some
    (fn [x]
      (if (> x 2)
        (Some (str "value-" x))
        None))
    [1 2 3 4]))
(def missing
  (some
    (fn [x]
      (if (> x 9)
        (Some (str "value-" x))
        None))
    [1 2 3 4]))
(println
  (str
    (match found (Some value) value None "missing") ":"
    (nil? missing)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "some_returns_first_truthy_predicate_value" "value-3:true\n"
    ocaml_source

let test_clojure_truthiness_in_conditions () =
  let source =
    {|
(println
  (str (if nil "bad" "nil-false") ":"
       (if false "bad" "false-false") ":"
       (if 0 "zero-true" "bad") ":"
       (if "" "empty-string-true" "bad") ":"
       (if (vector-of :int) "empty-vector-true" "bad")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_truthiness_in_conditions"
    "nil-false:false-false:zero-true:empty-string-true:empty-vector-true\n"
    ocaml_source

let test_and_or_return_values_and_short_circuit () =
  let source =
    {|
(def calls (atom 0))
(defn mark [value]
  (do
    (reset! calls (+ (deref calls) 1))
    value))
(def all-empty (and))
(def any-empty (or))
(def all-keyword (and :first :second))
(def any-option (or None (Some "ready")))
(def stopped-and (and false (mark true)))
(def stopped-or (or true (mark false)))
(println
  (str all-empty ":" (nil? any-empty) ":" (name all-keyword) ":"
       (match any-option (Some value) value None "missing") ":"
       stopped-and ":" stopped-or ":" (deref calls)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "and_or_return_values_and_short_circuit"
    "true:true:second:ready:false:true:0\n" ocaml_source

let test_and_or_single_values_are_unchanged () =
  let source =
    {|
(def all-value (and "value"))
(def any-value (or :ready))
(println (str all-value ":" (name any-value)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "and_or_single_values_are_unchanged" "value:ready\n"
    ocaml_source

let test_additional_sequence_helpers_reject_bad_reductions_arity () =
  Lg.Compiler.compile_string {|(def x (reductions +))|}
  |> expect_error "reductions expects function, optional init, and collection"

let test_let_defn_and_fn_values () =
  let source =
    {|
(defn inc1 [x] (+ x 1))
(def add2 (fn [x] (+ x 2)))
(def result (let [base 10
                  bumped (inc1 base)]
              (add2 bumped)))
(println result)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "let_defn_and_fn_values" "13\n" ocaml_source

let test_loop_and_recur_are_tail_recursive () =
  let source =
    {|
(def total
  (loop [n 5 acc 0]
    (if (= n 0)
      acc
      (recur (dec n) (+ acc n)))))
(println total)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "loop_and_recur_are_tail_recursive" "15\n" ocaml_source

let test_loop_recur_remains_tail_through_let_and_cond () =
  let source =
    {|
(def result
  (loop [value 0]
    (let [next (inc value)]
      (cond
        (= next 5) next
        :else (recur next)))))
(println result)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "loop_recur_remains_tail_through_let_and_cond" "5\n"
    ocaml_source

let test_loop_and_recur_delegate_ocaml_owned_alias_compatibility () =
  let source =
    {|
(type-alias user-id :int)
(type-alias account-id :int)
(defn as-user [^:user_id x] x)
(defn as-account [^:account_id x] x)
(def final-id
  (loop [id (as-user 0)
         n 1]
    (if (= n 0)
      id
      (recur (as-account 42) (dec n)))))
(println "loop-ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "loop_and_recur_delegate_ocaml_owned_alias_compatibility"
    "loop-ok\n" ocaml_source

let test_loop_and_recur_delegate_ocaml_owned_mismatch_to_ocaml () =
  Lg.Compiler.compile_string
    {|
(type-alias user-id :int)
(defn as-user [^:user_id x] x)
(def bad
  (loop [id (as-user 0)
         n 1]
    (if (= n 0)
      id
      (recur "bad" (dec n)))))
|}
  |> expect_error_contains "string"

let test_loop_and_recur_reject_invalid_calls () =
  Lg.Compiler.compile_string {|(recur 1)|}
  |> expect_error "recur is only valid in a loop tail position";
  Lg.Compiler.compile_string {|(loop [n 1] (recur n 0))|}
  |> expect_error "recur expects 1 arguments";
  Lg.Compiler.compile_string {|(loop [n 1] (recur "one"))|}
  |> expect_error "recur argument 1 must be int";
  Lg.Compiler.compile_string {|(loop [n 1] (+ 1 (recur (dec n))))|}
  |> expect_error "recur is only valid in a loop tail position"

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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "destructuring_supports_rest_and_defaults"
    "Ada:0:37:100:10:20:3:4:10:20:2:4\n" ocaml_source

let test_let_destructuring_accepts_generic_seqable_values () =
  let source =
    {|
(defn first-pair [values]
  (let [[left right] values]
    (str left ":" right)))
(println (first-pair ["a" "b"]))
(println (first-pair (list "c" "d")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "let_destructuring_accepts_generic_seqable_values"
    "a:b\nc:d\n" ocaml_source

let test_macro_slots_preserve_dynamic_seqable_values () =
  let source =
    {|
(defn first-through-slot [value]
  (let [slot (volatile! nil)]
    (vreset! slot value)
    (let [[first-value] (deref slot)]
      first-value)))
(println
  (str (first-through-slot ["vector"]) ":"
       (first-through-slot (list "list"))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "macro_slots_preserve_dynamic_seqable_values"
    "vector:list\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_destructuring_preserves_row_polymorphic_function_calls () =
  let source =
    {|
(def user {:name "Ada", :age 36, :admin? true})
(defn greeting [{:keys [name]}]
  (str "hi " name))
(println (greeting user))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "destructuring_preserves_row_polymorphic_function_calls"
    "hi Ada\n" ocaml_source

let test_map_destructuring_as_preserves_open_map_access () =
  let source =
    {|
(defn restore-value [{:keys [value] :as options}]
  (or (:restored options) value))
(println (restore-value {:value 42}))
(println (restore-value {:value 42 :restored 7}))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "map_destructuring_as_preserves_open_map_access" "42\n7\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "row_polymorphic_functions_accept_different_map_shapes"
    "hi Ada:hi Milo\n" ocaml_source

let test_row_types_bind_nested_capability_parameters () =
  let source =
    {|
(defn no-vars? [{:keys [vars]}]
  (empty? vars))
(println "ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "row_types_bind_nested_capability_parameters" "ok\n"
    ocaml_source

let test_destructuring_rejects_missing_map_fields () =
  let source =
    {|
(def user {:name "Ada"})
(defn next-age [{:keys [age]}] (+ age 1))
(def bad (next-age user))
|}
  in
  Lg.Compiler.compile_string source
  |> expect_error "next-age called with incompatible arguments"

let test_let_destructuring_supports_nested_sequences () =
  let source =
    {|
(let [[_ left [middle _ right]] [0 1 [2 0 3]]]
  (println (str left ":" middle ":" right)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "let_destructuring_supports_nested_sequences" "1:2:3\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_let_bindings_support_value_type_hints () =
  let source =
    {|
(defrecord Datom [value])
(let [datom ^Datom (Datom. 42)]
  (println (.-value datom)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "let_bindings_support_value_type_hints" "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_nested_record_fields_preserve_outer_record_inference () =
  let source =
    {|
(defrecord DB [max-tx])
(defrecord TxReport [^DB db-before ^DB db-after tx-data])
(defrecord Datom [value])
(defn transact-report [report datom]
  (let [before ^DB (:db-before report)
        db (:db-after report)
        value (:value datom)]
    report))
(defn transact-add [report]
  (let [db (:db-after report)
        report' (assoc report :extra true)
        new-datom (Datom. 2)]
    (transact-report report' new-datom)))
(def result (transact-add (TxReport. (DB. 1) (DB. 1) [])))
(println "ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nested_record_fields_preserve_outer_record_inference"
    "ok\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_threaded_keyword_access_preserves_nested_record_inference () =
  let source =
    {|
(defrecord DB [max-tx])
(defrecord TxReport [^DB db-before])
(defn current-tx [report]
  (-> report :db-before :max-tx long inc))
(def result (current-tx (TxReport. (DB. 1))))
(println result)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "threaded_keyword_access_preserves_nested_record_inference"
    "2\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_destructuring_rejects_unsupported_let_sources () =
  Lg.Compiler.compile_string {|(def x (let [{:keys [name]} [1 2]] name))|}
  |> expect_error "map destructuring expects a map"

let test_destructuring_rejects_bad_rest_binding () =
  Lg.Compiler.compile_string {|(def x (let [[head &] [1 2]] head))|}
  |> expect_error "sequential destructuring & must be followed by a symbol"

let test_destructuring_rejects_bad_or_defaults () =
  Lg.Compiler.compile_string
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sequence_core_api_on_vectors" "2:4:2:6:2:false\n"
    ocaml_source

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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "common_higher_order_helpers"
    "(1 2 2 3 3 4):(1 2 2 3):(3 2 1):true:false:true:false:false:true:[9 10 \
     11]:15:10:true:false:-1:1:4:1\n"
    ocaml_source

let test_common_higher_order_helpers_reject_bad_mapcat_result () =
  Lg.Compiler.compile_string {|(def x (mapcat (fn [x] (inc x)) [1 2]))|}
  |> expect_error "mapcat function must return a collection, got int"

let test_common_higher_order_helpers_reject_bad_predicates () =
  Lg.Compiler.compile_string
    {|(def f (every-pred (fn [x] (inc x)) (fn [x] true)))|}
  |> expect_error "every-pred expects predicates with the same argument type"

let test_common_higher_order_helpers_reject_mixed_juxt_returns () =
  Lg.Compiler.compile_string
    {|(def f (juxt (fn [x] (+ x 1)) (fn [x] (even? x))))|}
  |> expect_error "juxt functions must return the same type"

let test_common_higher_order_helpers_reject_compare_type_mismatch () =
  Lg.Compiler.compile_string {|(def x (compare 1 "1"))|}
  |> expect_error "compare arguments must have the same type: int and string"

let test_apply_rejects_bad_set_reducers () =
  Lg.Compiler.compile_string {|(def x (apply + (hash-set "a" "b")))|}
  |> expect_error "apply currently supports int binary reducers"

let test_apply_distinct_accepts_generic_seqable_values () =
  let source =
    {|
(defn all-distinct? [values]
  (apply clojure.core/distinct? values))
(println (all-distinct? [1 2 3]))
(println (all-distinct? (list 1 2 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "apply_distinct_accepts_generic_seqable_values"
    "true\nfalse\n" ocaml_source

let test_apply_calls_overloaded_functions_with_dynamic_arguments () =
  let source =
    {|
(defn make-value
  ([^int e a v] e)
  ([^int e a v tx] e))
(println (apply make-value [1 :name "Ada"]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "apply_calls_overloaded_functions_with_dynamic_arguments"
    "1\n" ocaml_source

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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "set_core_api" "true:false:4:#{1 2 3 4}:#{1 2 3 4}:#{1 3}\n"
    ocaml_source

let test_sets_support_named_records () =
  let source =
    {|
(def ada {:name "Ada", :age 36})
(def ada-copy {:name "Ada", :age 36})
(def users (hash-set ada ada-copy))
(def updated (conj users ada-copy))
(def matching (filter (fn [user] (= (:name user) "Ada")) updated))
(def all-ada? (every? (fn [user] (= (:name user) "Ada")) updated))
(def ages (map (fn [user] (:age user)) updated))
(def trimmed (disj updated ada-copy))
(def rebuilt (set [ada-copy]))
(println (str (count matching) ":" (= (first matching) ada) ":" all-ada? ":"
              (count ages) ":" (= (first ages) 36) ":" (count trimmed) ":"
              (contains? rebuilt ada)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sets_support_named_records" "1:true:true:1:true:0:true\n"
    ocaml_source

let test_sets_support_primitive_lists_and_vectors () =
  let source =
    {|
(def list-values (hash-set (list 1 2) (list 1 2)))
(def vector-values (hash-set [1 2] [1 2]))
(def more-vectors (conj vector-values [2 3]))
(println (str (count list-values) ":" (contains? list-values (list 1 2)) ":"
              (count more-vectors) ":" (contains? more-vectors [2 3]) ":"
              (pr-str list-values) ":" (pr-str more-vectors)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sets_support_primitive_lists_and_vectors"
    "1:true:2:true:#{(1 2)}:#{[1 2] [2 3]}\n" ocaml_source

let test_sets_support_nested_composite_elements () =
  let source =
    {|
(def paths (hash-set [[1 2] [3 4]] [[1 2] [3 4]]))
(def updated (conj paths [[5 6]]))
(println (str (count updated) ":" (contains? updated [[5 6]])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sets_support_nested_composite_elements" "2:true\n"
    ocaml_source

let test_set_positional_sequence_helpers () =
  let source =
    {|
(def xs (hash-set 3 1 2))
(def tail (rest xs))
(def empty-tail (rest (set-of :int)))
(println
  (str (first xs) ":" (second xs) ":" (last xs) ":"
       (count tail) ":" (first tail) ":" (empty? empty-tail)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "set_positional_sequence_helpers" "1:2:3:2:2:true\n"
    ocaml_source

let test_set_positional_sequence_helpers_reject_non_collections () =
  Lg.Compiler.compile_string {|(def x (first 1))|}
  |> expect_error "first expects a seqable value"

let test_conj_rejects_set_type_mismatch () =
  Lg.Compiler.compile_string {|(def xs (conj (hash-set 1) "two"))|}
  |> expect_error "conj value type must match set element type"

let test_disj_rejects_set_type_mismatch () =
  Lg.Compiler.compile_string {|(def xs (disj (hash-set 1) 1 "two"))|}
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "set_sequence_core_api" "true:true:true:6\n" ocaml_source

let test_set_sequence_predicates_reject_bad_predicates () =
  Lg.Compiler.compile_string
    {|(def x (every? (fn [x] (+ x 1)) (hash-set 1 2)))|}
  |> expect_error "every? expects a predicate matching set elements"

let test_reduce_rejects_bad_set_reducers () =
  Lg.Compiler.compile_string
    {|(def x (reduce (fn [acc x] (str acc x)) 0 (hash-set 1 2)))|}
  |> expect_error_contains "reduced value must match init"

let test_set_map_and_filter_core_api () =
  let source =
    {|
(def xs (hash-set 1 2 3))
(def mapped (map (fn [x] (+ x 1)) xs))
(def filtered (filter (fn [x] (> x 2)) mapped))
(println (str (pr-str mapped) ":" (pr-str filtered)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "set_map_and_filter_core_api" "(2 3 4):(3 4)\n" ocaml_source

let test_set_map_rejects_function_type_mismatch () =
  Lg.Compiler.compile_string
    {|(def xs (map (fn [^:string x] x) (hash-set 1 2)))|}
  |> expect_error "map function argument type does not match sequence"

let test_set_filter_rejects_non_bool_predicates () =
  Lg.Compiler.compile_string
    {|(def xs (filter (fn [x] (+ x 1)) (hash-set 1 2)))|}
  |> expect_error "filter expects a predicate matching sequence elements"

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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sequence_core_api_on_lists" "2:4:2:6\n" ocaml_source

let test_range_core_api () =
  let source =
    {|
(println (str (pr-str (range 4)) ":" (pr-str (range 2 6)) ":"
              (pr-str (range 2 10 3)) ":" (pr-str (range 5 0 -2))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "range_core_api" "(0 1 2 3):(2 3 4 5):(2 5 8):(5 3 1)\n"
    ocaml_source

let test_range_rejects_zero_step () =
  Lg.Compiler.compile_string {|(def xs (range 1 10 0))|}
  |> expect_error "range step cannot be 0"

let test_range_rejects_non_int_arguments () =
  Lg.Compiler.compile_string {|(def xs (range "4"))|}
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "take_and_drop_core_api" "(1 2):(3 4):(1 2 3 4):()\n"
    ocaml_source

let test_take_and_drop_reject_non_int_counts () =
  Lg.Compiler.compile_string {|(def x (take "2" [1 2]))|}
  |> expect_error "take count must be int"

let test_take_and_drop_support_sets () =
  let source = {|(println (pr-str (drop 1 (hash-set 1 2))))|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "take_and_drop_support_sets" "(2)\n" ocaml_source

let test_reverse_core_api () =
  let source =
    {|
(def xs [1 2 3])
(def ys (list 1 2 3))
(println (str (pr-str (reverse xs)) ":" (pr-str (reverse ys))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "reverse_core_api" "[3 2 1]:(3 2 1)\n" ocaml_source

let test_reverse_rejects_unsupported_collections () =
  Lg.Compiler.compile_string {|(def x (reverse (hash-set 1)))|}
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sequence_boolean_predicates" "true:true:true\n"
    ocaml_source

let test_sequence_boolean_predicates_reject_non_bool_predicates () =
  Lg.Compiler.compile_string {|(def x (every? (fn [x] (+ x 1)) [1 2]))|}
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "empty_core_api" "true:true:true:true\n" ocaml_source

let test_empty_rejects_unsupported_values () =
  Lg.Compiler.compile_string {|(def x (empty 1))|}
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "into_core_api" "[1 2 3]:(3 2 1):#{1 2 3}\n" ocaml_source

let test_into_accepts_inferred_seqable_parameters () =
  let source =
    {|
(defn append-all [values]
  (if (empty? values)
    []
    (into [] values)))
(def appended (append-all [1 2 3]))
(println (str (count appended) ":" (= 1 (first appended))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "into_accepts_inferred_seqable_parameters" "3:true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_eduction_applies_map_filter_and_cat_transducers () =
  let source =
    {|
(def mapped (->Eduction (map inc) [1 2 3]))
(def filtered (->Eduction (filter (fn [value] (> value 1))) [1 2 3]))
(def flattened
  (->Eduction (comp (map (fn [value] [value (inc value)])) cat) [1 3]))
(def transduced (transduce (map inc) + 0 [1 2 3]))
(println
  (str (pr-str (vec mapped)) ":"
       (pr-str (vec filtered)) ":"
       (pr-str (vec flattened)) ":" transduced))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "eduction_applies_map_filter_and_cat_transducers"
    "[2 3 4]:[2 3]:[1 2 3 4]:9\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_into_applies_composed_transducers () =
  let source =
    {|
(defn transform [values]
  (into #{}
    (comp
      (filter (fn [value] (> value 1)))
      (map inc))
    values))
(def transformed (transform [1 2 3]))
(println
  (str (count transformed) ":"
       (contains? transformed 3) ":"
       (contains? transformed 4)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "into_applies_composed_transducers" "2:true:true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_into_transducers_build_dynamic_sets () =
  let source =
    {|
(defrecord Datom [value])
(defn retract [datoms]
  (into #{}
    (comp
      (filter (fn [^Datom datom] (> (.-value datom) 1)))
      (map (fn [^Datom datom] [:retract (.-value datom)])))
    datoms))
(def result (retract [(Datom. 1) (Datom. 2) (Datom. 3)]))
(println (count result))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "into_transducers_build_dynamic_sets" "2\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_filter_accepts_dynamic_callable_record_fields () =
  let source =
    {|
(defrecord Filter [pred])
(def filter-value
  (Filter. (fn [value] (> value 1))))
(println (pr-str (vec (filter (.-pred filter-value) [1 2 3]))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "filter_accepts_dynamic_callable_record_fields" "[2 3]\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_sequence_operations_accept_host_optional_collections () =
  let source =
    {|
(def values (Some [1 2 3]))
(println (pr-str (vec (filter (fn [value] (> value 1)) values))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sequence_operations_accept_host_optional_collections"
    "[2 3]\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_reduce_infers_seqable_record_fields () =
  let source =
    {|
(defn sum-values [value]
  (reduce + 0 (:values value)))
(println (sum-values {:values [1 2 3]}))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "reduce_infers_seqable_record_fields" "6\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_thread_macros_accept_keyword_steps () =
  let source =
    {|
(defrecord Box [answer])
(println (-> (Box. 42) :answer))
(println (some-> (Box. 7) :answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "thread_macros_accept_keyword_steps" "42\n7\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_cond_thread_macros_apply_selected_steps () =
  let source =
    {|
(defn adjust [value increment? double?]
  (cond-> value
    increment? inc
    double? (* 2)))
(println (adjust 3 true false))
(println (adjust 3 true true))
(println (first (cond->> [1 2 3] true (map inc))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "cond_thread_macros_apply_selected_steps" "4\n8\n2\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_update_accepts_first_class_assoc_on_dynamic_maps () =
  let source =
    {|
(def result (update {} :nested assoc :answer 42))
(println (get (get result :nested) :answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "update_accepts_first_class_assoc_on_dynamic_maps" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_assoc_infers_dynamic_maps_for_variable_keys () =
  let source =
    {|
(defn put [m k v] (assoc m k v))
(def result (put {} :answer 42))
(println (get result :answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "assoc_infers_dynamic_maps_for_variable_keys" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_assoc_accepts_dynamic_collection_boundaries () =
  let source =
    {|
(defn put [^:dynamic m ^:dynamic k ^:dynamic v]
  (assoc m k v))
(def result (put {} :answer 42))
(println (get result :answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "assoc_accepts_dynamic_collection_boundaries" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_assoc_accepts_nullable_dynamic_maps () =
  let source =
    {|
(defn put [m k v]
  (if (nil? m)
    (assoc m k v)
    (assoc m k v)))
(def result (put {} :answer 42))
(println (get result :answer))
(defn first-value [value] (first [value 0]))
(def first-result (assoc (first-value {}) :second 7))
(println (get first-result :second))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "assoc_accepts_nullable_dynamic_maps" "42\n7\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_reduce_kv_accepts_dynamic_maps () =
  let source =
    {|
(defrecord Holder [value])
(defn copy-map [holder]
  (reduce-kv
    (fn [result key value]
      (assoc result key value))
    {}
    (.-value ^Holder holder)))
(def copied (copy-map (Holder. {:answer 42})))
(println (get copied :answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "reduce_kv_accepts_dynamic_maps" "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_map_value_parameters_support_guarded_sequence_use () =
  let source =
    {|
(defn sum-values [values]
  (reduce (fn [total value] (+ total value)) 0 values))
(defn multi-value? [value]
  (vector? value))
(defn resolve-like [entity]
  (reduce-kv
    (fn [[total seen] _attribute value]
      (if (multi-value? value)
        [(+ total (sum-values value)) (inc seen)]
        [total seen]))
    [0 0]
    entity))
(println (resolve-like {:values [1 2]}))
(println (resolve-like {:value 7}))
(println (resolve-like {}))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "map_value_parameters_support_guarded_sequence_use"
    "[3 1]\n[0 0]\n[0 0]\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_reduce_accepts_open_map_entries () =
  let source =
    {|
(defn sum-entity-values [entity]
  (let [eid (:db/id entity)]
    (reduce
      (fn [total [attribute value]]
        (if (= attribute :db/id) total (+ total value)))
      0
      entity)))
(println (sum-entity-values {:db/id 10 :a 1 :b 2}))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "reduce_accepts_open_map_entries" "3\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_update_refines_empty_nested_vector_elements () =
  let source =
    {|
(let [buckets [[] []]
      attribute :a
      value 1
      result (assoc buckets 0 (conj (nth buckets 0) [attribute value]))]
  (println (= [[[:a 1]] []] result)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "update_refines_empty_nested_vector_elements"
    "true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_apply_accepts_concat_as_a_core_function () =
  let source =
    {|
(println (pr-str (apply concat [0] [[1 2] [3 4]])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "apply_accepts_concat_as_a_core_function" "(0 1 2 3 4)\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_reduce_preserves_refined_vector_element_types () =
  let source =
    {|
(defn collect-entry-values [entity]
  (let [eid (:db/id entity)
        entries
        (apply concat
          (reduce
            (fn [buckets [attribute value]]
              (assoc buckets 0
                (conj (nth buckets 0) [attribute value])))
            [[] []]
            entity))]
    (mapv (fn [[_ value]] (+ value 0)) entries)))
(println (collect-entry-values {:db/id 10 :a 1 :b 2}))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "reduce_preserves_refined_vector_element_types"
    "[10 1 2]\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_set_literals_accept_dynamic_elements () =
  let source =
    {|
(defrecord Holder [value])
(defn singleton [holder]
  #{(.-value ^Holder holder)})
(def values (singleton (Holder. :answer)))
(println (contains? values :answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "set_literals_accept_dynamic_elements" "true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_contains_infers_generic_membership_for_variable_keys () =
  let source =
    {|
(defn member? [collection value]
  (contains? collection value))
(println (member? #{1 2} 2))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "contains_infers_generic_membership_for_variable_keys"
    "true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_into_rejects_element_type_mismatch () =
  Lg.Compiler.compile_string {|(def x (into [1] ["two"]))|}
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_empty_sets" "true:3:true:false:#{1 3}\n" ocaml_source

let test_sets_reject_nil_elements () =
  Lg.Compiler.compile_string {|(def values (set-of :nil))|}
  |> expect_error "unknown set element type :nil";
  Lg.Compiler.compile_string {|(def values (hash-set nil))|}
  |> expect_error "sets require a generated comparator for nil";
  Lg.Compiler.compile_string {|(def values (set [nil]))|}
  |> expect_error "sets require a generated comparator for nil"

let test_set_of_rejects_types_without_comparators () =
  Lg.Compiler.compile_string {|(def xs (set-of :record))|}
  |> expect_error "sets require a generated comparator for record"

let test_keyword_type_annotations_for_empty_collections () =
  let source =
    {|
(def xs (conj (vector-of :keyword) :name))
(def ys (conj (list-of :keyword) :age))
(def zs (into (set-of :keyword) [:name :name :age]))
(println (str (pr-str xs) ":" (pr-str ys) ":" (contains? zs :age) ":" (pr-str zs)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nth_supports_default_values" "99:88\n" ocaml_source

let test_nth_rejects_default_type_mismatch () =
  Lg.Compiler.compile_string {|(def x (nth [1 2] 5 "missing"))|}
  |> expect_error "nth default must match collection element type"

let test_typed_empty_lists () =
  let source =
    {|
(def xs (list-of :int))
(def ys (cons 42 xs))
(println (str (empty? xs) ":" (count ys) ":" (first ys)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_empty_lists" "true:1:42\n" ocaml_source

let test_empty_lists_infer_type_from_branch_context () =
  let source =
    {|
(defn values [^:bool enabled]
  (if enabled (list 42) (list)))
(defn matched-values [^:bool enabled]
  (match enabled
    true (list 42)
    false (list)))
(println
  (str (count (values true)) ":" (count (values false)) ":"
       (count (matched-values true)) ":" (count (matched-values false))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "empty_lists_infer_type_from_branch_context" "1:0:1:0\n"
    ocaml_source;
  Lg.Compiler.compile_string {|(def values (list))|}
  |> expect_error "empty list requires a contextual element type";
  Lg.Compiler.compile_string {|(defn values [] (list))|}
  |> expect_error "empty list requires a contextual element type"

let test_rest_is_empty_safe () =
  let source =
    {|
(def xs (rest (list-of :int)))
(def ys (rest (vector-of :int)))
(println (str (empty? xs) ":" (pr-str xs) ":" (empty? ys) ":" (pr-str ys)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "rest_is_empty_safe" "true:():true:()\n" ocaml_source

let test_lists_support_mixed_element_types () =
  let source = {|(println (pr-str (list 1 "two" :three)))|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "lists_support_mixed_element_types" "(1 \"two\" :three)\n"
    ocaml_source

let test_mixed_lists_pack_optional_values () =
  let source =
    {|
(defn context [value]
  (when (some? value) true)
  (list 'context value))
(println (str (pr-str (context nil)) ":" (pr-str (context 42))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "mixed_lists_pack_optional_values"
    "(context nil):(context 42)\n" ocaml_source

let test_conj_rejects_list_type_mismatch () =
  Lg.Compiler.compile_string {|(def xs (conj (list 1) "two"))|}
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "subvec_core_api" "[2 3 4]:[2 3]\n" ocaml_source

let test_subvec_rejects_non_vector_sources () =
  Lg.Compiler.compile_string {|(def x (subvec (list 1 2) 0))|}
  |> expect_error "subvec expects a vector"

let test_subvec_rejects_non_int_indexes () =
  Lg.Compiler.compile_string {|(def x (subvec [1 2] "0"))|}
  |> expect_error "subvec indexes must be int"

let test_peek_rejects_unsupported_collections () =
  Lg.Compiler.compile_string {|(def x (peek (hash-set 1)))|}
  |> expect_error "peek expects a list or vector"

let test_let_rejects_odd_binding_forms () =
  Lg.Compiler.compile_string {|(def x (let [a 1 b] a))|}
  |> expect_error "let bindings require an even number of forms"

let test_map_rejects_non_function_argument () =
  Lg.Compiler.compile_string {|(def xs (map 1 [1 2]))|}
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
(def case-keyword (case :keys :keys "keys" :syms "syms" "other"))
(def case-default (case :strs :keys "keys" :syms "syms" "other"))
(def case-grouped (case 2 (1 2) "small" "other"))
(println
  (str (describe 0) ":" (describe 2) ":"
       empty-list-score ":" one-list-score ":" two-list-score ":" many-list-score ":"
       empty-vector-score ":" one-vector-score ":" two-vector-score ":" many-vector-score ":"
       case-keyword ":" case-default ":" case-grouped))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "match_expression_works"
    "zero:n=2:0:7:7:99:0:7:7:99:keys:other:small\n" ocaml_source

let test_match_supports_mixed_branch_types () =
  let source = {|(println (pr-str (match 1 0 "zero" _ 1)))|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "match_supports_mixed_branch_types" "1\n" ocaml_source

let test_match_rejects_bad_clause_count () =
  Lg.Compiler.compile_string {|(def x (match 1 0 "zero" _))|}
  |> expect_error "match requires pattern/result pairs"

let test_match_rejects_pattern_type_mismatch () =
  Lg.Compiler.compile_string {|(def x (match 1 "1" 1 _ 0))|}
  |> expect_error "match pattern type must match target"

let test_match_infers_target_type_from_patterns () =
  Lg.Compiler.compile_string
    {|
(defn describe [x]
  (match x
    0 "zero"
    n (str "n=" n)))
(def bad (describe "x"))
|}
  |> expect_error "describe called with incompatible arguments"

let test_match_supports_ocaml_constructor_patterns () =
  let source =
    {|
(type-variant status Active Inactive)
(def active Active)
(def inactive Inactive)
(defn describe [^:status status]
  (match status
    Active "active"
    Inactive "inactive"))
(println (str (describe active) ":" (describe inactive)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "match_supports_ocaml_constructor_patterns"
    "active:inactive\n" ocaml_source

let test_compile_diagnostics_capture_ocaml_match_warnings () =
  let source =
    {|
(type-variant status Active Inactive)
(defn describe [^:status status]
  (match status
    Active "active"))
|}
  in
  let compilation =
    Lg.Compiler.compile_string_with_filename_and_diagnostics
      ~filename:"warning.lgc" source
    |> expect_ok
  in
  match compilation.diagnostics with
  | [ diagnostic ] ->
      if diagnostic.severity <> `Warning then
        failwith "expected an OCaml warning diagnostic";
      if not (string_contains_substring diagnostic.message "not exhaustive")
      then
        failwith
          ("expected non-exhaustive match warning, got: " ^ diagnostic.message);
      if not (string_contains_substring diagnostic.message "warning.lgc") then
        failwith ("expected warning filename, got: " ^ diagnostic.message)
  | diagnostics ->
      failwith
        (Printf.sprintf "expected one warning diagnostic, got %d"
           (List.length diagnostics))

let test_compile_diagnostics_are_empty_for_exhaustive_matches () =
  let source =
    {|
(type-variant status Active Inactive)
(defn describe [^:status status]
  (match status
    Active "active"
    Inactive "inactive"))
|}
  in
  let compilation =
    Lg.Compiler.compile_string_with_diagnostics source |> expect_ok
  in
  if compilation.diagnostics <> [] then
    failwith "expected exhaustive match compilation to have no diagnostics"

let test_parser_diagnostics_locate_unterminated_delimiters () =
  let source = "(def ok 1)\n(def broken [1 2" in
  match
    Lg.Compiler.compile_string_with_filename ~filename:"broken.lgc" source
  with
  | Ok _ -> failwith "expected an unterminated vector error"
  | Error error -> (
      if error.message <> "unterminated vector; expected ']'" then
        failwith ("unexpected parser error: " ^ error.message);
      match error.location with
      | Some location ->
          if location.loc_start.Lexing.pos_fname <> "broken.lgc" then
            failwith "parser error should preserve the source filename";
          if location.loc_start.Lexing.pos_lnum <> 2 then
            failwith "parser error should point to the opening delimiter line";
          if location.loc_start.Lexing.pos_cnum <> 23 then
            failwith "parser error should point to the opening delimiter"
      | None -> failwith "parser error should include a location")

let test_language_service_recovers_completed_prefix () =
  let source = "(def answer 41)\n(def broken (+ answer" in
  match
    Lg.Language_service.recover_completed_prefix ~filename:"editing.lgc" source
  with
  | None -> failwith "expected semantic analysis for the completed prefix"
  | Some analysis ->
      if
        not
          (List.exists
             (fun (symbol : Lg.Language_service.document_symbol) ->
               symbol.name = "answer")
             (Lg.Language_service.document_symbols analysis))
      then failwith "recovered analysis should preserve completed definitions"

let language_service_source =
  {|
(def answer 41)
(defn add-one [x] (+ x 1))
(def result (add-one answer))
|}

let analyze_language_service_source () =
  Lg.Language_service.analyze ~filename:"file:///tmp/service.lgc"
    language_service_source
  |> expect_ok

let test_language_service_hover_uses_ocaml_types () =
  let analysis = analyze_language_service_source () in
  let offset =
    expect_substring_index language_service_source "add-one answer"
  in
  match Lg.Language_service.hover analysis ~offset with
  | Some hover ->
      if not (string_contains_substring hover.contents "int -> int") then
        failwith
          ("expected inferred OCaml function type, got: " ^ hover.contents)
  | None -> failwith "expected hover information for add-one"

let test_language_service_definition_resolves_source_binding () =
  let analysis = analyze_language_service_source () in
  let usage = expect_substring_index language_service_source "answer))" in
  match Lg.Language_service.definition analysis ~offset:usage with
  | Some location ->
      if location.Location.loc_start.Lexing.pos_lnum <> 2 then
        failwith "expected answer definition on source line 2"
  | None -> failwith "expected definition for answer usage"

let test_language_service_completion_uses_source_names_and_types () =
  let analysis = analyze_language_service_source () in
  let items =
    Lg.Language_service.completions analysis
      ~offset:(String.length language_service_source)
  in
  let find label =
    List.find_opt
      (fun (item : Lg.Language_service.completion_item) -> item.label = label)
      items
  in
  (match find "add-one" with
  | Some item when string_contains_substring item.detail "int -> int" -> ()
  | Some item -> failwith ("expected add-one type detail, got: " ^ item.detail)
  | None -> failwith "expected source completion add-one");
  if find "answer" = None then failwith "expected source completion answer"

let test_language_service_queries_outside_symbols_are_empty () =
  let analysis = analyze_language_service_source () in
  if Lg.Language_service.hover analysis ~offset:0 <> None then
    failwith "expected no hover outside a symbol";
  if Lg.Language_service.definition analysis ~offset:0 <> None then
    failwith "expected no definition outside a symbol"

let test_language_service_signature_help_uses_typed_call_site () =
  let source =
    {|
(defn combine [left right] (+ left right))
(def total (combine 1 2))
(def nested (combine 1 (combine 2 3)))
|}
  in
  let analysis =
    Lg.Language_service.analyze ~filename:"file:///tmp/signature-help.lgc"
      source
    |> expect_ok
  in
  let assert_signature offset active_parameter =
    match Lg.Language_service.signature_help analysis ~offset with
    | Some signature
      when signature.label = "combine : int -> int -> int"
           && signature.parameters = [ "int"; "int" ]
           && signature.active_parameter = active_parameter ->
        ()
    | Some signature ->
        failwith
          (Printf.sprintf "unexpected signature help %s at parameter %d"
             signature.label signature.active_parameter)
    | None -> failwith "expected signature help"
  in
  assert_signature
    (expect_substring_index source "combine 1 2" + String.length "combine 1 ")
    1;
  assert_signature
    (expect_substring_index source "combine 2 3" + String.length "combine 2 ")
    1;
  if Lg.Language_service.signature_help analysis ~offset:0 <> None then
    failwith "signature help outside a call must be empty"

let span_text source (span : Lg.Ast.source_span) =
  String.sub source span.start_offset (span.end_offset - span.start_offset)

let test_language_service_references_use_typed_identity () =
  let source =
    {|
(def value 1)
(defn use [value] (+ value 1))
(def result (+ value (use 2)))
|}
  in
  let analysis =
    Lg.Language_service.analyze ~filename:"file:///tmp/references.lgc" source
    |> expect_ok
  in
  let top_level_usage = expect_substring_index source "value (use" in
  let references =
    Lg.Language_service.references analysis ~offset:top_level_usage
  in
  let referenced_text = List.map (span_text source) references in
  if referenced_text <> [ "value"; "value" ] then
    failwith
      ("expected only top-level value definition/use, got: "
      ^ String.concat "," referenced_text)

let test_language_service_rename_returns_exact_symbol_edits () =
  let analysis = analyze_language_service_source () in
  let usage = expect_substring_index language_service_source "answer))" in
  match Lg.Language_service.rename analysis ~offset:usage ~new_name:"total" with
  | Error err -> failwith ("expected rename edits, got: " ^ err.Lg.Error.message)
  | Ok edits -> (
      if List.length edits <> 2 then
        failwith "expected definition and usage edits";
      List.iter
        (fun (edit : Lg.Language_service.text_edit) ->
          if edit.new_text <> "total" then
            failwith "expected rename replacement total";
          if span_text language_service_source edit.range <> "answer" then
            failwith "expected rename to edit only source symbol spans")
        edits;
      match
         Lg.Language_service.rename analysis ~offset:usage ~new_name:"bad name"
       with
      | Error _ -> ()
      | Ok _ -> failwith "expected invalid rename target to be rejected")

let constructor_language_service_source =
  {|
(type-variant status Active (Named :string))
(def named (Named "Ada"))
(def label
  (match named
    (Named value) value
    Active "active"))
|}

let analyze_constructor_language_service_source () =
  Lg.Language_service.analyze ~filename:"file:///tmp/constructor-service.lgc"
    constructor_language_service_source
  |> expect_ok

let test_language_service_constructor_definition_uses_declaration_span () =
  let analysis = analyze_constructor_language_service_source () in
  let declaration =
    expect_substring_index constructor_language_service_source "Named :string"
  in
  let usage =
    expect_substring_index constructor_language_service_source "Named \"Ada\""
  in
  match Lg.Language_service.definition analysis ~offset:usage with
  | Some location ->
      if location.Location.loc_start.Lexing.pos_cnum <> declaration then
        failwith "expected constructor definition at its source declaration"
  | None -> failwith "expected constructor definition"

let test_language_service_constructor_references_and_rename_use_identity () =
  let analysis = analyze_constructor_language_service_source () in
  let usage =
    expect_substring_index constructor_language_service_source "Named \"Ada\""
  in
  let references = Lg.Language_service.references analysis ~offset:usage in
  let referenced_text =
    List.map (span_text constructor_language_service_source) references
  in
  if referenced_text <> [ "Named"; "Named"; "Named" ] then
    failwith
      ("expected constructor declaration/expression/pattern references, got: "
      ^ String.concat "," referenced_text);
  match
    Lg.Language_service.rename analysis ~offset:usage ~new_name:"Labelled"
  with
  | Error err -> failwith ("expected constructor rename, got: " ^ err.message)
  | Ok edits ->
      if List.length edits <> 3 then
        failwith "expected constructor declaration/expression/pattern edits";
      List.iter
        (fun (edit : Lg.Language_service.text_edit) ->
          if span_text constructor_language_service_source edit.range <> "Named"
          then failwith "expected constructor rename to edit exact spans")
        edits

let test_language_service_completion_includes_constructors () =
  let analysis = analyze_constructor_language_service_source () in
  let items =
    Lg.Language_service.completions analysis
      ~offset:(String.length constructor_language_service_source)
  in
  match
    List.find_opt
      (fun (item : Lg.Language_service.completion_item) -> item.label = "Named")
      items
  with
  | Some item when string_contains_substring item.detail "string" -> ()
  | Some item ->
      failwith ("expected constructor payload type detail, got: " ^ item.detail)
  | None -> failwith "expected constructor completion"

let test_workspace_constructor_definition_resolves_across_files () =
  let provider = "(type-variant status Active (Named :string))\n" in
  let consumer = "(def named (Named \"Ada\"))\n" in
  let provider_uri = "file:///tmp/status.lgc" in
  let consumer_uri = "file:///tmp/status-main.lgc" in
  let analyses =
    Lg.Language_service.analyze_workspace
      [ (consumer_uri, consumer); (provider_uri, provider) ]
    |> expect_ok
  in
  let consumer_analysis = List.assoc consumer_uri analyses in
  let usage = expect_substring_index consumer "Named" in
  match Lg.Language_service.definition consumer_analysis ~offset:usage with
  | Some location
    when location.Location.loc_start.Lexing.pos_fname = provider_uri
         && location.loc_start.pos_cnum
            = expect_substring_index provider "Named" ->
      ()
  | _ -> failwith "expected cross-file constructor definition"

let test_constructor_references_keep_module_identities_distinct () =
  let source =
    {|
(module Left
  (type-variant t (Named :string)))
(module Right
  (type-variant t (Named :string)))
(def left (Left/Named "L"))
(def right (Right/Named "R"))
|}
  in
  let analysis =
    Lg.Language_service.analyze ~filename:"file:///tmp/constructor-modules.lgc"
      source
    |> expect_ok
  in
  let usage =
    expect_substring_index source "Left/Named" + String.length "Left/"
  in
  let references = Lg.Language_service.references analysis ~offset:usage in
  let referenced_text = List.map (span_text source) references in
  if referenced_text <> [ "Named"; "Named" ] then
    failwith
      ("expected only Left.Named constructor references, got: "
      ^ String.concat "," referenced_text)

let test_language_service_constructor_capabilities () =
  let cases =
    [
      ( "definition",
        test_language_service_constructor_definition_uses_declaration_span );
      ( "references and rename",
        test_language_service_constructor_references_and_rename_use_identity );
      ( "completion", test_language_service_completion_includes_constructors );
      ( "cross-file definition",
        test_workspace_constructor_definition_resolves_across_files );
      ( "module identity",
        test_constructor_references_keep_module_identities_distinct );
    ]
  in
  let failures =
    List.filter_map
      (fun (name, test) ->
        try
          test ();
          None
        with Failure message -> Some (name ^ ": " ^ message))
      cases
  in
  if failures <> [] then
    failwith ("constructor tooling failures: " ^ String.concat " | " failures)

let type_language_service_source =
  {|
(type-alias user-id :int)
(type-record user (name :string))
(type-variant status Active Inactive)
(def ada (record user (name "Ada")))
(defn keep-id [^:user_id value] value)
(defn keep-status [^:status value] value)
|}

let analyze_type_language_service_source () =
  Lg.Language_service.analyze ~filename:"file:///tmp/type-service.lgc"
    type_language_service_source
  |> expect_ok

let test_language_service_type_definition_and_references_use_identity () =
  let analysis = analyze_type_language_service_source () in
  let declaration =
    expect_substring_index type_language_service_source "user (name"
  in
  let usage =
    expect_substring_index type_language_service_source "user (name \"Ada\""
  in
  (match Lg.Language_service.definition analysis ~offset:usage with
  | Some location when location.Location.loc_start.Lexing.pos_cnum = declaration
    ->
      ()
  | _ -> failwith "expected record type definition");
  let references = Lg.Language_service.references analysis ~offset:usage in
  let referenced_text =
    List.map (span_text type_language_service_source) references
  in
  if referenced_text <> [ "user"; "user" ] then
    failwith
      ("expected record type declaration/usage references, got: "
      ^ String.concat "," referenced_text)

let test_language_service_type_rename_edits_plain_type_spans () =
  let analysis = analyze_type_language_service_source () in
  let usage =
    expect_substring_index type_language_service_source "user (name \"Ada\""
  in
  match
    Lg.Language_service.rename analysis ~offset:usage ~new_name:"person"
  with
  | Error err -> failwith ("expected type rename, got: " ^ err.message)
  | Ok edits ->
      if List.length edits <> 2 then
        failwith "expected type declaration and construction edits";
      List.iter
        (fun (edit : Lg.Language_service.text_edit) ->
          if span_text type_language_service_source edit.range <> "user" then
            failwith "expected exact type source spans")
        edits

let test_language_service_alias_and_variant_annotations_resolve_types () =
  let analysis = analyze_type_language_service_source () in
  let check declaration_text usage_text =
    let declaration =
      expect_substring_index type_language_service_source declaration_text
    in
    let usage =
      expect_substring_index type_language_service_source usage_text
    in
    match Lg.Language_service.definition analysis ~offset:usage with
    | Some location
      when location.Location.loc_start.Lexing.pos_cnum = declaration ->
        ()
    | _ -> failwith ("expected type definition for " ^ usage_text)
  in
  check "user-id :int" "user_id value";
  check "status Active" "status value"

let test_language_service_completion_includes_source_type_names () =
  let analysis = analyze_type_language_service_source () in
  let items =
    Lg.Language_service.completions analysis
      ~offset:(String.length type_language_service_source)
  in
  let labels =
    List.map
      (fun (item : Lg.Language_service.completion_item) -> item.label)
      items
  in
  List.iter
    (fun name ->
      if not (List.mem name labels) then
        failwith ("expected type completion " ^ name))
    [ "user-id"; "user"; "status" ]

let test_workspace_type_definition_resolves_across_files () =
  let provider = "(type-record user (name :string))\n" in
  let consumer = "(def ada (record user (name \"Ada\")))\n" in
  let provider_uri = "file:///tmp/user-type.lgc" in
  let consumer_uri = "file:///tmp/user-main.lgc" in
  let analyses =
    Lg.Language_service.analyze_workspace
      [ (consumer_uri, consumer); (provider_uri, provider) ]
    |> expect_ok
  in
  let analysis = List.assoc consumer_uri analyses in
  let usage = expect_substring_index consumer "user" in
  match Lg.Language_service.definition analysis ~offset:usage with
  | Some location
    when location.Location.loc_start.Lexing.pos_fname = provider_uri
         && location.loc_start.pos_cnum = expect_substring_index provider "user"
    ->
      ()
  | _ -> failwith "expected cross-file type definition"

let test_type_references_keep_module_identities_distinct () =
  let source =
    {|
(module Left
  (type-record item (value :int)))
(module Right
  (type-record item (value :int)))
(def left (record Left.item (value 1)))
(def right (record Right.item (value 2)))
|}
  in
  let analysis =
    Lg.Language_service.analyze ~filename:"file:///tmp/type-modules.lgc" source
    |> expect_ok
  in
  let usage =
    expect_substring_index source "Left.item" + String.length "Left."
  in
  let references = Lg.Language_service.references analysis ~offset:usage in
  let referenced_text = List.map (span_text source) references in
  if referenced_text <> [ "item"; "item" ] then
    failwith
      ("expected only Left.item type references, got: "
      ^ String.concat "," referenced_text)

let test_language_service_type_capabilities () =
  let cases =
    [
      ( "definition and references",
        test_language_service_type_definition_and_references_use_identity );
      ( "rename", test_language_service_type_rename_edits_plain_type_spans );
      ( "alias and variant annotations",
        test_language_service_alias_and_variant_annotations_resolve_types );
      ( "completion", test_language_service_completion_includes_source_type_names );
      ( "cross-file definition",
        test_workspace_type_definition_resolves_across_files );
      ( "module identity", test_type_references_keep_module_identities_distinct );
    ]
  in
  let failures =
    List.filter_map
      (fun (name, test) ->
        try
          test ();
          None
        with Failure message -> Some (name ^ ": " ^ message))
      cases
  in
  if failures <> [] then
    failwith ("type tooling failures: " ^ String.concat " | " failures)

let module_language_service_source =
  {|
(module-signature ValueSig (val value :int))
(module First ValueSig (def value 1))
(module Second ValueSig (def value 2))
(def first-value First/value)
(def second-value Second/value)
|}

let analyze_module_language_service_source () =
  Lg.Language_service.analyze ~filename:"file:///tmp/module-service.lgc"
    module_language_service_source
  |> expect_ok

let test_language_service_module_definition_and_references_use_identity () =
  let analysis = analyze_module_language_service_source () in
  let declaration =
    expect_substring_index module_language_service_source "First ValueSig"
  in
  let usage =
    expect_substring_index module_language_service_source "First/value"
  in
  (match Lg.Language_service.definition analysis ~offset:usage with
  | Some location when location.Location.loc_start.Lexing.pos_cnum = declaration
    ->
      ()
  | _ -> failwith "expected module definition");
  let references = Lg.Language_service.references analysis ~offset:usage in
  let referenced_text =
    List.map (span_text module_language_service_source) references
  in
  if referenced_text <> [ "First"; "First" ] then
    failwith
      ("expected module declaration and qualified reference, got: "
      ^ String.concat "," referenced_text)

let test_language_service_module_rename_edits_only_module_segments () =
  let analysis = analyze_module_language_service_source () in
  let usage =
    expect_substring_index module_language_service_source "First/value"
  in
  match
    Lg.Language_service.rename analysis ~offset:usage ~new_name:"Primary"
  with
  | Error err -> failwith ("expected module rename, got: " ^ err.message)
  | Ok edits ->
      if List.length edits <> 2 then
        failwith "expected module declaration and qualified reference edits";
      List.iter
        (fun (edit : Lg.Language_service.text_edit) ->
          if span_text module_language_service_source edit.range <> "First" then
            failwith "module rename must not replace the qualified member")
        edits

let test_language_service_module_and_member_offsets_are_distinct () =
  let analysis = analyze_module_language_service_source () in
  let usage =
    expect_substring_index module_language_service_source "First/value"
  in
  let module_definition =
    Lg.Language_service.definition analysis ~offset:usage
  in
  let value_definition =
    Lg.Language_service.definition analysis
      ~offset:(usage + String.length "First/")
  in
  match (module_definition, value_definition) with
  | Some module_location, Some value_location
    when module_location.loc_start.pos_cnum
         = expect_substring_index module_language_service_source
             "First ValueSig"
         && value_location.loc_start.pos_cnum
            = expect_substring_index module_language_service_source "value :int"
    ->
      ()
  | Some module_location, Some value_location ->
      failwith
        (Printf.sprintf "expected module/member definitions at %d/%d, got %d/%d"
           (expect_substring_index module_language_service_source
              "First ValueSig")
           (expect_substring_index module_language_service_source "value :int")
           module_location.loc_start.pos_cnum value_location.loc_start.pos_cnum)
  | _ -> failwith "expected module and member definitions"

let test_module_references_keep_module_identities_distinct () =
  let analysis = analyze_module_language_service_source () in
  let usage =
    expect_substring_index module_language_service_source "First/value"
  in
  let references = Lg.Language_service.references analysis ~offset:usage in
  let referenced_text =
    List.map (span_text module_language_service_source) references
  in
  if referenced_text <> [ "First"; "First" ] then
    failwith
      ("expected only First module references, got: "
      ^ String.concat "," referenced_text)

let test_workspace_module_definition_resolves_across_files () =
  let provider = "(module Math (def answer 42))\n" in
  let consumer = "(def answer Math/answer)\n" in
  let provider_uri = "file:///tmp/math-module.lgc" in
  let consumer_uri = "file:///tmp/math-main.lgc" in
  let analyses =
    Lg.Language_service.analyze_workspace
      [ (consumer_uri, consumer); (provider_uri, provider) ]
    |> expect_ok
  in
  let analysis = List.assoc consumer_uri analyses in
  let usage = expect_substring_index consumer "Math/answer" in
  match Lg.Language_service.definition analysis ~offset:usage with
  | Some location
    when location.Location.loc_start.Lexing.pos_fname = provider_uri
         && location.loc_start.pos_cnum = expect_substring_index provider "Math"
    ->
      ()
  | _ -> failwith "expected cross-file module definition"

let test_language_service_module_signature_definition_and_references () =
  let analysis = analyze_module_language_service_source () in
  let declaration =
    expect_substring_index module_language_service_source "ValueSig (val"
  in
  let usage =
    expect_substring_index module_language_service_source "ValueSig (def"
  in
  (match Lg.Language_service.definition analysis ~offset:usage with
  | Some location when location.Location.loc_start.Lexing.pos_cnum = declaration
    ->
      ()
  | _ -> failwith "expected module signature definition");
  let references = Lg.Language_service.references analysis ~offset:usage in
  let referenced_text =
    List.map (span_text module_language_service_source) references
  in
  if referenced_text <> [ "ValueSig"; "ValueSig"; "ValueSig" ] then
    failwith
      ("expected signature declaration and module constraints, got: "
      ^ String.concat "," referenced_text)

let test_language_service_completion_includes_modules_and_signatures () =
  let analysis = analyze_module_language_service_source () in
  let items =
    Lg.Language_service.completions analysis
      ~offset:(String.length module_language_service_source)
  in
  let labels =
    List.map
      (fun (item : Lg.Language_service.completion_item) -> item.label)
      items
  in
  List.iter
    (fun name ->
      if not (List.mem name labels) then
        failwith ("expected module completion " ^ name))
    [ "First"; "Second"; "ValueSig" ]

let module_construct_language_service_source =
  {|
(module-signature ArgSig (val value :int))
(module Input ArgSig (def value 42))
(module-alias Alias Input)
(open Alias)
(include Alias)
(module-functor Make [Arg ArgSig]
  (def copied Arg/value))
(module-apply Output Make Input)
|}

let analyze_module_construct_language_service_source () =
  Lg.Language_service.analyze
    ~filename:"file:///tmp/module-construct-service.lgc"
    module_construct_language_service_source
  |> expect_ok

let assert_module_definition_offset analysis ~usage ~expected message =
  match Lg.Language_service.definition analysis ~offset:usage with
  | Some location when location.Location.loc_start.Lexing.pos_cnum = expected ->
      ()
  | Some location ->
      failwith
        (Printf.sprintf "%s: expected %d, got %d" message expected
           location.loc_start.pos_cnum)
  | None -> failwith (message ^ ": expected a definition")

let test_language_service_module_constructs_preserve_exact_locations () =
  let source = module_construct_language_service_source in
  let analysis = analyze_module_construct_language_service_source () in
  let arg_sig_declaration = expect_substring_index source "ArgSig (val" in
  let input_declaration = expect_substring_index source "Input ArgSig" in
  let alias_declaration = expect_substring_index source "Alias Input" in
  let functor_declaration = expect_substring_index source "Make [Arg" in
  let parameter_declaration = expect_substring_index source "Arg ArgSig" in
  let output_declaration = expect_substring_index source "Output Make" in
  assert_module_definition_offset analysis
    ~usage:(expect_substring_index source "Alias Input" + String.length "Alias ")
    ~expected:input_declaration "module alias target";
  assert_module_definition_offset analysis
    ~usage:(expect_substring_index source "open Alias" + String.length "open ")
    ~expected:alias_declaration "open module";
  assert_module_definition_offset analysis
    ~usage:
      (expect_substring_index source "include Alias" + String.length "include ")
    ~expected:alias_declaration "include module";
  assert_module_definition_offset analysis
    ~usage:(expect_substring_index source "Arg ArgSig" + String.length "Arg ")
    ~expected:arg_sig_declaration "functor parameter signature";
  assert_module_definition_offset analysis
    ~usage:(expect_substring_index source "Arg/value")
    ~expected:parameter_declaration "functor parameter";
  assert_module_definition_offset analysis
    ~usage:
      (expect_substring_index source "Output Make" + String.length "Output ")
    ~expected:functor_declaration "applied functor";
  assert_module_definition_offset analysis
    ~usage:(expect_substring_index source "Make Input" + String.length "Make ")
    ~expected:input_declaration "functor argument";
  assert_module_definition_offset analysis ~usage:output_declaration
    ~expected:output_declaration "module application result";
  let parameter_usage = expect_substring_index source "Arg/value" in
  let parameter_references =
    Lg.Language_service.references analysis ~offset:parameter_usage
  in
  let parameter_reference_offsets =
    List.map
      (fun (span : Lg.Ast.source_span) -> span.start_offset)
      parameter_references
  in
  if parameter_reference_offsets <> [ parameter_declaration; parameter_usage ]
  then
    failwith "functor parameter references must include its exact declaration";
  match
    Lg.Language_service.rename analysis ~offset:parameter_usage
      ~new_name:"Source"
  with
  | Error err -> failwith ("expected functor parameter rename: " ^ err.message)
  | Ok edits ->
      if List.length edits <> 2 then
        failwith "functor parameter rename must edit declaration and usage";
      List.iter
        (fun (edit : Lg.Language_service.text_edit) ->
          if span_text source edit.range <> "Arg" then
            failwith "functor parameter rename must use exact symbol spans")
        edits

let test_language_service_module_capabilities () =
  let cases =
    [
      ( "definition and references",
        test_language_service_module_definition_and_references_use_identity );
      ( "rename segments",
        test_language_service_module_rename_edits_only_module_segments );
      ( "module/member offsets",
        test_language_service_module_and_member_offsets_are_distinct );
      ( "module identity", test_module_references_keep_module_identities_distinct );
      ( "cross-file definition",
        test_workspace_module_definition_resolves_across_files );
      ( "signature identity",
        test_language_service_module_signature_definition_and_references );
      ( "module construct locations",
        test_language_service_module_constructs_preserve_exact_locations );
      ( "completion",
        test_language_service_completion_includes_modules_and_signatures );
    ]
  in
  let failures =
    List.filter_map
      (fun (name, test) ->
        try
          test ();
          None
        with Failure message -> Some (name ^ ": " ^ message))
      cases
  in
  if failures <> [] then
    failwith ("module tooling failures: " ^ String.concat " | " failures)

let protocol_language_service_source =
  {|
(defprotocol Labelled
  (label [value] :string))
(extend-type :int Labelled
  (label [value] (str value)))
(def result (Labelled/label 42))
|}

let analyze_protocol_language_service_source () =
  Lg.Language_service.analyze ~filename:"file:///tmp/protocol-service.lgc"
    protocol_language_service_source
  |> expect_ok

let test_language_service_protocol_definition_references_and_rename () =
  let analysis = analyze_protocol_language_service_source () in
  let declaration =
    expect_substring_index protocol_language_service_source "Labelled\n"
  in
  let usage =
    expect_substring_index protocol_language_service_source "Labelled/label"
  in
  (match Lg.Language_service.definition analysis ~offset:usage with
  | Some location when location.Location.loc_start.Lexing.pos_cnum = declaration
    ->
      ()
  | _ -> failwith "expected protocol definition");
  let references = Lg.Language_service.references analysis ~offset:usage in
  let referenced_text =
    List.map (span_text protocol_language_service_source) references
  in
  if referenced_text <> [ "Labelled"; "Labelled"; "Labelled" ] then
    failwith
      ("expected protocol declaration, extension, and call references, got: "
      ^ String.concat "," referenced_text);
  match Lg.Language_service.rename analysis ~offset:usage ~new_name:"Named" with
  | Error err -> failwith ("expected protocol rename, got: " ^ err.message)
  | Ok edits ->
      if List.length edits <> 3 then
        failwith "expected three protocol rename edits";
      List.iter
        (fun (edit : Lg.Language_service.text_edit) ->
          if span_text protocol_language_service_source edit.range <> "Labelled"
          then failwith "protocol rename must edit exact protocol segments")
        edits

let test_language_service_protocol_method_definition_references_and_rename () =
  let analysis = analyze_protocol_language_service_source () in
  let declaration =
    expect_substring_index protocol_language_service_source "label [value]"
  in
  let qualified =
    expect_substring_index protocol_language_service_source "Labelled/label"
  in
  let usage = qualified + String.length "Labelled/" in
  (match Lg.Language_service.definition analysis ~offset:usage with
  | Some location when location.Location.loc_start.Lexing.pos_cnum = declaration
    ->
      ()
  | _ -> failwith "expected protocol method definition");
  let references = Lg.Language_service.references analysis ~offset:usage in
  let referenced_text =
    List.map (span_text protocol_language_service_source) references
  in
  if referenced_text <> [ "label"; "label"; "label" ] then
    failwith
      ("expected method declaration, implementation, and call references, got: "
      ^ String.concat "," referenced_text);
  match
    Lg.Language_service.rename analysis ~offset:usage ~new_name:"name-of"
  with
  | Error err -> failwith ("expected method rename, got: " ^ err.message)
  | Ok edits ->
      if List.length edits <> 3 then
        failwith "expected three method rename edits";
      List.iter
        (fun (edit : Lg.Language_service.text_edit) ->
          if span_text protocol_language_service_source edit.range <> "label"
          then failwith "method rename must edit exact method segments")
        edits

let test_protocol_method_references_keep_protocol_identities_distinct () =
  let source =
    {|
(defprotocol Display (render [value] :string))
(defprotocol Debug (render [value] :string))
(extend-type :int Display (render [value] (str value)))
(extend-type :int Debug (render [value] (str value)))
(def display (Display/render 1))
(def debug (Debug/render 1))
|}
  in
  let analysis =
    Lg.Language_service.analyze ~filename:"file:///tmp/protocol-identities.lgc"
      source
    |> expect_ok
  in
  let qualified = expect_substring_index source "Display/render 1" in
  let usage = qualified + String.length "Display/" in
  let references = Lg.Language_service.references analysis ~offset:usage in
  let referenced_text = List.map (span_text source) references in
  if referenced_text <> [ "render"; "render"; "render" ] then
    failwith
      ("expected only Display/render references, got: "
      ^ String.concat "," referenced_text)

let test_workspace_protocol_definition_resolves_across_files () =
  let provider =
    "(defprotocol Labelled (label [value] :string))\n\
     (extend-type :int Labelled (label [value] (str value)))\n"
  in
  let consumer = "(def result (Labelled/label 42))\n" in
  let provider_uri = "file:///tmp/protocol-provider.lgc" in
  let consumer_uri = "file:///tmp/protocol-consumer.lgc" in
  let analyses =
    Lg.Language_service.analyze_workspace
      [ (consumer_uri, consumer); (provider_uri, provider) ]
    |> expect_ok
  in
  let analysis = List.assoc consumer_uri analyses in
  let usage = expect_substring_index consumer "Labelled/label" in
  match Lg.Language_service.definition analysis ~offset:usage with
  | Some location
    when location.Location.loc_start.Lexing.pos_fname = provider_uri
         && location.loc_start.pos_cnum
            = expect_substring_index provider "Labelled" ->
      ()
  | _ -> failwith "expected cross-file protocol definition"

let test_module_and_protocol_namespaces_remain_distinct () =
  let source =
    {|
(module Shared (def value 1))
(defprotocol Shared (label [value] :string))
(extend-type :int Shared (label [value] (str value)))
(def module-value Shared/value)
(def protocol-value (Shared/label 1))
|}
  in
  let analysis =
    Lg.Language_service.analyze
      ~filename:"file:///tmp/protocol-module-clash.lgc" source
    |> expect_ok
  in
  let module_usage = expect_substring_index source "Shared/value" in
  let protocol_usage = expect_substring_index source "Shared/label" in
  let module_declaration = expect_substring_index source "Shared (def value" in
  let protocol_declaration = expect_substring_index source "Shared (label" in
  match
    ( Lg.Language_service.definition analysis ~offset:module_usage,
      Lg.Language_service.definition analysis ~offset:protocol_usage )
  with
  | Some module_location, Some protocol_location
    when module_location.loc_start.pos_cnum = module_declaration
         && protocol_location.loc_start.pos_cnum = protocol_declaration ->
      let module_refs =
        Lg.Language_service.references analysis ~offset:module_usage
        |> List.map (span_text source)
      in
      let protocol_refs =
        Lg.Language_service.references analysis ~offset:protocol_usage
        |> List.map (span_text source)
      in
      if module_refs <> [ "Shared"; "Shared" ] then
        failwith "same-named module references must remain isolated";
      if protocol_refs <> [ "Shared"; "Shared"; "Shared" ] then
        failwith "same-named protocol references must remain isolated"
  | _ ->
      failwith
        "module and protocol qualifiers with the same name must stay distinct"

let test_language_service_completion_includes_protocols_and_methods () =
  let analysis = analyze_protocol_language_service_source () in
  let items =
    Lg.Language_service.completions analysis
      ~offset:(String.length protocol_language_service_source)
  in
  let labels =
    List.map
      (fun (item : Lg.Language_service.completion_item) -> item.label)
      items
  in
  List.iter
    (fun name ->
      if not (List.mem name labels) then
        failwith ("expected protocol completion " ^ name))
    [ "Labelled"; "label" ]

let test_language_service_protocol_capabilities () =
  let cases =
    [
      ( "protocol identity",
        test_language_service_protocol_definition_references_and_rename );
      ( "method identity",
        test_language_service_protocol_method_definition_references_and_rename
      );
      ( "same method names",
        test_protocol_method_references_keep_protocol_identities_distinct );
      ( "cross-file protocol",
        test_workspace_protocol_definition_resolves_across_files );
      ( "module/protocol collision",
        test_module_and_protocol_namespaces_remain_distinct );
      ( "completion",
        test_language_service_completion_includes_protocols_and_methods );
    ]
  in
  let failures =
    List.filter_map
      (fun (name, test) ->
        try
          test ();
          None
        with Failure message -> Some (name ^ ": " ^ message))
      cases
  in
  if failures <> [] then
    failwith ("protocol tooling failures: " ^ String.concat " | " failures)

let field_language_service_source =
  {|
(type-record user (name :string) (age :int))
(def ada (record user (name "Ada") (age 36)))
(def label (:name ada))
(def extracted (match ada (record (name value)) value))
|}

let analyze_field_language_service_source () =
  Lg.Language_service.analyze ~filename:"file:///tmp/field-service.lgc"
    field_language_service_source
  |> expect_ok

let test_language_service_field_definition_references_and_rename () =
  let analysis = analyze_field_language_service_source () in
  let declaration =
    expect_substring_index field_language_service_source "name :string"
  in
  let usage =
    expect_substring_index field_language_service_source ":name ada"
  in
  (match Lg.Language_service.definition analysis ~offset:usage with
  | Some location when location.Location.loc_start.Lexing.pos_cnum = declaration
    ->
      ()
  | _ -> failwith "expected record field definition");
  let references = Lg.Language_service.references analysis ~offset:usage in
  let referenced_text =
    List.map (span_text field_language_service_source) references
  in
  if referenced_text <> [ "name"; "name"; "name"; "name" ] then
    failwith
      ("expected field declaration, construction, access, and pattern \
        references, got: "
      ^ String.concat "," referenced_text);
  match
    Lg.Language_service.rename analysis ~offset:usage ~new_name:"display-name"
  with
  | Error err -> failwith ("expected field rename, got: " ^ err.message)
  | Ok edits ->
      if List.length edits <> 4 then failwith "expected four exact field edits";
      List.iter
        (fun (edit : Lg.Language_service.text_edit) ->
          if span_text field_language_service_source edit.range <> "name" then
            failwith "field rename must edit exact field symbols")
        edits

let test_field_references_keep_record_identities_distinct () =
  let source =
    {|
(type-record user (name :string))
(type-record project (name :string))
(def ada (record user (name "Ada")))
(def lg (record project (name "lg")))
(def user-name (:name ada))
(def project-name (:name lg))
|}
  in
  let analysis =
    Lg.Language_service.analyze ~filename:"file:///tmp/field-identities.lgc"
      source
    |> expect_ok
  in
  let usage = expect_substring_index source ":name ada" in
  let references = Lg.Language_service.references analysis ~offset:usage in
  let referenced_text = List.map (span_text source) references in
  if referenced_text <> [ "name"; "name"; "name" ] then
    failwith
      ("expected only user.name references, got: "
      ^ String.concat "," referenced_text)

let test_workspace_field_definition_resolves_across_files () =
  let provider = "(type-record user (name :string))\n" in
  let consumer =
    "(def ada (record user (name \"Ada\")))\n(def label (:name ada))\n"
  in
  let provider_uri = "file:///tmp/field-provider.lgc" in
  let consumer_uri = "file:///tmp/field-consumer.lgc" in
  let analyses =
    Lg.Language_service.analyze_workspace
      [ (consumer_uri, consumer); (provider_uri, provider) ]
    |> expect_ok
  in
  let analysis = List.assoc consumer_uri analyses in
  let usage = expect_substring_index consumer ":name ada" in
  match Lg.Language_service.definition analysis ~offset:usage with
  | Some location
    when location.Location.loc_start.Lexing.pos_fname = provider_uri
         && location.loc_start.pos_cnum = expect_substring_index provider "name"
    ->
      ()
  | _ -> failwith "expected cross-file field definition"

let test_language_service_completion_includes_record_fields () =
  let analysis = analyze_field_language_service_source () in
  let items =
    Lg.Language_service.completions analysis
      ~offset:(String.length field_language_service_source)
  in
  let labels =
    List.map
      (fun (item : Lg.Language_service.completion_item) -> item.label)
      items
  in
  List.iter
    (fun name ->
      if not (List.mem name labels) then
        failwith ("expected field completion " ^ name))
    [ "name"; "age" ]

let test_language_service_field_capabilities () =
  let cases =
    [
      ( "definition/references/rename",
        test_language_service_field_definition_references_and_rename );
      ( "record identity", test_field_references_keep_record_identities_distinct );
      ( "cross-file definition", test_workspace_field_definition_resolves_across_files );
      ( "completion", test_language_service_completion_includes_record_fields );
    ]
  in
  let failures =
    List.filter_map
      (fun (name, test) ->
        try
          test ();
          None
        with Failure message -> Some (name ^ ": " ^ message))
      cases
  in
  if failures <> [] then
    failwith ("field tooling failures: " ^ String.concat " | " failures)

let assert_semantic_hover analysis source ~offset ~symbol ~expected =
  match Lg.Language_service.hover analysis ~offset with
  | Some hover
    when string_contains_substring hover.contents expected
         && span_text source hover.range = symbol ->
      ()
  | Some hover ->
      failwith
        (Printf.sprintf "expected semantic hover containing %S, got %S" expected
           hover.contents)
  | None -> failwith ("expected semantic hover containing " ^ expected)

let test_language_service_semantic_hover_capabilities () =
  let constructor_analysis = analyze_constructor_language_service_source () in
  assert_semantic_hover constructor_analysis constructor_language_service_source
    ~offset:
      (expect_substring_index constructor_language_service_source
         "Named :string")
    ~symbol:"Named" ~expected:"constructor Named";
  let type_analysis = analyze_type_language_service_source () in
  assert_semantic_hover type_analysis type_language_service_source
    ~offset:(expect_substring_index type_language_service_source "user (name")
    ~symbol:"user" ~expected:"type user";
  let module_analysis = analyze_module_language_service_source () in
  assert_semantic_hover module_analysis module_language_service_source
    ~offset:
      (expect_substring_index module_language_service_source "First/value")
    ~symbol:"First" ~expected:"module First";
  assert_semantic_hover module_analysis module_language_service_source
    ~offset:
      (expect_substring_index module_language_service_source "ValueSig (val")
    ~symbol:"ValueSig" ~expected:"module type ValueSig";
  let protocol_analysis = analyze_protocol_language_service_source () in
  let protocol_usage =
    expect_substring_index protocol_language_service_source "Labelled/label"
  in
  assert_semantic_hover protocol_analysis protocol_language_service_source
    ~offset:protocol_usage ~symbol:"Labelled" ~expected:"protocol Labelled";
  assert_semantic_hover protocol_analysis protocol_language_service_source
    ~offset:(protocol_usage + String.length "Labelled/")
    ~symbol:"label" ~expected:"label :";
  let field_analysis = analyze_field_language_service_source () in
  assert_semantic_hover field_analysis field_language_service_source
    ~offset:
      (expect_substring_index field_language_service_source "name :string")
    ~symbol:"name" ~expected:"name : string"

let test_language_service_document_symbols_preserve_source_names () =
  let analysis = analyze_language_service_source () in
  let symbols = Lg.Language_service.document_symbols analysis in
  let find name =
    List.find_opt
      (fun (symbol : Lg.Language_service.document_symbol) -> symbol.name = name)
      symbols
  in
  if find "answer" = None then failwith "expected answer document symbol";
  (match find "add-one" with
  | Some { kind = `Function; _ } -> ()
  | Some _ -> failwith "expected add-one function symbol"
  | None -> failwith "expected add-one document symbol");
  if find "result" = None then failwith "expected result document symbol"

let test_language_service_recognizes_private_defn () =
  let source = "(defn- hidden [value] (+ value 1))\n" in
  let analysis =
    Lg.Language_service.analyze ~filename:"file:///tmp/private-defn.lgc" source
    |> expect_ok
  in
  match Lg.Language_service.document_symbols analysis with
  | [ { name = "hidden"; kind = `Function; _ } ] -> ()
  | _ -> failwith "expected defn- to produce a function document symbol"

let document_symbol_hierarchy_source =
  {|
(module-signature ValueSig (val value :int))
(module Domain
  (type-record user (name :string) (age :int))
  (type-variant status Active (Named :string))
  (defprotocol Display (render [value] :string))
  (defn identity [value] value)
  (module-signature Service
    (val get :int)
    (type item)
    (module Nested ValueSig)))
(def copied Domain/identity)
|}

let test_language_service_document_symbols_include_semantic_children () =
  let analysis =
    Lg.Language_service.analyze
      ~filename:"file:///tmp/document-symbol-hierarchy.lgc"
      document_symbol_hierarchy_source
    |> expect_ok
  in
  let symbols = Lg.Language_service.document_symbols analysis in
  let find name symbols =
    List.find_opt
      (fun (symbol : Lg.Language_service.document_symbol) -> symbol.name = name)
      symbols
    |> Option.get
  in
  let domain = find "Domain" symbols in
  let user = find "user" domain.children in
  let status = find "status" domain.children in
  let protocol = find "Display" domain.children in
  let service = find "Service" domain.children in
  let child_names (symbol : Lg.Language_service.document_symbol) =
    List.map
      (fun (child : Lg.Language_service.document_symbol) -> child.name)
      symbol.children
  in
  if child_names user <> [ "name"; "age" ] then
    failwith "record document symbol must include fields";
  if child_names status <> [ "Active"; "Named" ] then
    failwith "variant document symbol must include constructors";
  if child_names protocol <> [ "render" ] then
    failwith "protocol document symbol must include methods";
  if child_names service <> [ "get"; "item"; "Nested" ] then
    failwith "module signature symbol must include its declarations";
  let field = find "name" user.children in
  if span_text document_symbol_hierarchy_source field.selection_range <> "name"
  then failwith "document symbol selection range must be the declaration name";
  if span_text document_symbol_hierarchy_source field.range <> "(name :string)"
  then failwith "field document symbol range must cover its declaration"

let test_language_service_semantic_tokens_classify_symbols () =
  let analysis =
    Lg.Language_service.analyze ~filename:"file:///tmp/semantic-tokens.lgc"
      document_symbol_hierarchy_source
    |> expect_ok
  in
  let tokens = Lg.Language_service.semantic_tokens analysis in
  let has text kind =
    List.exists
      (fun (token : Lg.Language_service.semantic_token) ->
        span_text document_symbol_hierarchy_source token.range = text
        && token.kind = kind)
      tokens
  in
  List.iter
    (fun (text, kind) ->
      if not (has text kind) then
        failwith ("expected semantic token classification for " ^ text))
    [
      ("module", `Keyword);
      ("Domain", `Namespace);
      ("user", `Type);
      ("name", `Property);
      ("Active", `Enum_member);
      ("Display", `Interface);
      ("render", `Method);
      ("identity", `Function);
      ("value", `Parameter);
      (":string", `Keyword);
    ];
  let qualified =
    expect_substring_index document_symbol_hierarchy_source "Domain/identity"
  in
  let has_at offset text kind =
    List.exists
      (fun (token : Lg.Language_service.semantic_token) ->
        token.range.start_offset = offset
        && span_text document_symbol_hierarchy_source token.range = text
        && token.kind = kind)
      tokens
  in
  if not (has_at qualified "Domain" `Namespace) then
    failwith "qualified semantic token must split the module segment";
  if not (has_at (qualified + String.length "Domain/") "identity" `Function)
  then failwith "qualified semantic token must split the member segment"

let test_language_service_workspace_resolves_cross_file_identity () =
  let math = "(module Math (defn magnitude-plus-two [x] (+ x 2)))\n" in
  let main = "(def result (Math/magnitude-plus-two 40))\n" in
  let analyses =
    Lg.Language_service.analyze_workspace
      [ ("file:///tmp/main.lgc", main); ("file:///tmp/math.lgc", math) ]
    |> expect_ok
  in
  let main_analysis = List.assoc "file:///tmp/main.lgc" analyses in
  let usage = expect_substring_index main "Math/magnitude-plus-two 40" in
  if Lg.Language_service.semantic_uid_at main_analysis ~offset:usage = None then
    failwith "expected required workspace symbol to have a typed identity";
  match Lg.Language_service.definition main_analysis ~offset:usage with
  | Some location
    when location.Location.loc_start.Lexing.pos_fname = "file:///tmp/math.lgc"
    ->
      ()
  | _ -> failwith "expected required workspace symbol definition in math.lgc"

let test_workspace_index_reanalyzes_only_dependency_component () =
  let math_uri = "file:///tmp/math.lgc" in
  let main_uri = "file:///tmp/main.lgc" in
  let other_uri = "file:///tmp/other.lgc" in
  let index =
    Lg.Language_service.create_workspace_index
      [
        (math_uri, "(module Math (def answer 40))\n");
        (main_uri, "(def result (+ Math/answer 2))\n");
        (other_uri, "(module Other (def value 7))\n");
      ]
    |> expect_ok
  in
  let other_before =
    Lg.Language_service.workspace_analysis index other_uri |> Option.get
  in
  let index, reanalyzed =
    Lg.Language_service.update_workspace_index index ~filename:math_uri
      ~source:"(module Math (def answer 41))\n"
    |> expect_ok
  in
  if
    List.sort String.compare reanalyzed
    <> List.sort String.compare [ math_uri; main_uri ]
  then failwith "workspace invalidation must follow dependency edges only";
  let other_after =
    Lg.Language_service.workspace_analysis index other_uri |> Option.get
  in
  if other_before != other_after then
    failwith "unrelated workspace analyses must be reused";
  let index, reanalyzed =
    Lg.Language_service.update_workspace_index index ~filename:math_uri
      ~source:"(module Math (def answer 41))\n"
    |> expect_ok
  in
  ignore index;
  if reanalyzed <> [] then
    failwith "unchanged workspace documents must not be reanalyzed"

let test_workspace_index_tracks_top_level_symbol_dependencies () =
  let values_uri = "file:///tmp/values.lgc" in
  let consumer_uri = "file:///tmp/consumer.lgc" in
  let index =
    Lg.Language_service.create_workspace_index
      [
        (values_uri, "(def shared-answer 40)\n");
        (consumer_uri, "(def result (+ shared-answer 2))\n");
      ]
    |> expect_ok
  in
  let _index, reanalyzed =
    Lg.Language_service.update_workspace_index index ~filename:values_uri
      ~source:"(def shared-answer 41)\n"
    |> expect_ok
  in
  if List.sort String.compare reanalyzed <> [ consumer_uri; values_uri ] then
    failwith "workspace index must track top-level symbol dependencies"

let test_workspace_index_tracks_module_alias_dependencies () =
  let analyses =
    Lg.Language_service.create_workspace_index
      [
        ("file:///tmp/workspace-math.lgc", "(module Math (def value 42))\n");
        ("file:///tmp/workspace-alias.lgc", "(module-alias M Math)\n");
        ("file:///tmp/workspace-alias-user.lgc", "(def result M/value)\n");
      ]
    |> expect_ok
  in
  if
    Lg.Language_service.workspace_analysis analyses
      "file:///tmp/workspace-alias-user.lgc"
    = None
  then failwith "workspace index must connect module alias consumers"

let test_workspace_index_tracks_variant_constructor_dependencies () =
  let analyses =
    Lg.Language_service.create_workspace_index
      [
        ( "file:///tmp/workspace-status.lgc",
          "(type-variant status Active (Named :string))\n" );
        ( "file:///tmp/workspace-status-user.lgc",
          "(def current (Named \"Ada\"))\n" );
      ]
    |> expect_ok
  in
  if
    Lg.Language_service.workspace_analysis analyses
      "file:///tmp/workspace-status-user.lgc"
    = None
  then failwith "workspace index must connect variant constructor consumers"

let test_workspace_index_ignores_lexically_bound_names () =
  let provider_uri = "file:///tmp/workspace-global-value.lgc" in
  let local_uri = "file:///tmp/workspace-local-value.lgc" in
  let index =
    Lg.Language_service.create_workspace_index
      [
        (provider_uri, "(def value 1)\n(def nested 2)\n");
        (local_uri, "(defn identity [value] (let [nested value] nested))\n");
      ]
    |> expect_ok
  in
  let local_before =
    Lg.Language_service.workspace_analysis index local_uri |> Option.get
  in
  let index, reanalyzed =
    Lg.Language_service.update_workspace_index index ~filename:provider_uri
      ~source:"(def value 2)\n(def nested 3)\n"
    |> expect_ok
  in
  if reanalyzed <> [ provider_uri ] then
    failwith "lexically bound names must not create workspace dependency edges";
  let local_after =
    Lg.Language_service.workspace_analysis index local_uri |> Option.get
  in
  if local_before != local_after then
    failwith "local-only files must reuse their previous analysis"

let test_workspace_index_tracks_qualified_type_dependencies () =
  let provider_uri = "file:///tmp/workspace-domain-type.lgc" in
  let consumer_uri = "file:///tmp/workspace-domain-type-user.lgc" in
  let index =
    Lg.Language_service.create_workspace_index
      [
        (provider_uri, "(module Domain (type-record user (name :string)))\n");
        (consumer_uri, "(defn keep [^:Domain.user value] value)\n");
      ]
    |> expect_ok
  in
  if Lg.Language_service.workspace_analysis index consumer_uri = None then
    failwith
      "qualified OCaml type annotations must depend on their module provider"

let test_workspace_index_tracks_concise_type_dependencies () =
  let provider_uri = "file:///tmp/a-workspace-domain-concise.lgc" in
  let consumer_uri = "file:///tmp/z-workspace-domain-concise-user.lgc" in
  let index =
    Lg.Language_service.create_workspace_index
      [
        (provider_uri, "(module Domain (type-record user (name :string)))\n");
        (consumer_uri, "(defn keep [^:Domain/user value] value)\n");
      ]
    |> expect_ok
  in
  let _index, reanalyzed =
    Lg.Language_service.update_workspace_index index ~filename:provider_uri
      ~source:"(module Domain (type-record user (name :string) (age :int)))\n"
    |> expect_ok
  in
  if List.sort String.compare reanalyzed <> [ provider_uri; consumer_uri ] then
    failwith "concise type annotations must invalidate their module consumers"

let test_workspace_index_tracks_declaration_type_dependencies () =
  let provider_uri = "file:///tmp/a-workspace-domain-declaration.lgc" in
  let consumer_uri = "file:///tmp/z-workspace-domain-declaration-user.lgc" in
  let consumer =
    {|
(type-alias user-option :option<Domain.user>)
(type-record envelope (user :Domain.user))
(type-variant event (Created :Domain.user))
|}
  in
  let index =
    Lg.Language_service.create_workspace_index
      [
        (provider_uri, "(module Domain (type-record user (name :string)))\n");
        (consumer_uri, consumer);
      ]
    |> expect_ok
  in
  let _index, reanalyzed =
    Lg.Language_service.update_workspace_index index ~filename:provider_uri
      ~source:"(module Domain (type-record user (name :string) (age :int)))\n"
    |> expect_ok
  in
  if List.sort String.compare reanalyzed <> [ provider_uri; consumer_uri ] then
    failwith "type declarations must invalidate qualified type consumers"

let test_workspace_index_separates_module_and_protocol_providers () =
  let module_uri = "file:///tmp/workspace-shared-module.lgc" in
  let protocol_uri = "file:///tmp/workspace-shared-protocol.lgc" in
  let consumer_uri = "file:///tmp/workspace-shared-user.lgc" in
  let index =
    Lg.Language_service.create_workspace_index
      [
        (module_uri, "(module Shared (def value 1))\n");
        ( protocol_uri,
          "(defprotocol Shared (label [value] :string))\n\
           (extend-type :int Shared (label [value] (str value)))\n" );
        ( consumer_uri,
          "(def module-value Shared/value)\n\
           (def protocol-value (Shared/label 1))\n" );
      ]
    |> expect_ok
  in
  if Lg.Language_service.workspace_analysis index consumer_uri = None then
    failwith "module and protocol providers with the same name must coexist"

let test_workspace_index_handles_file_removal_readd_and_rename () =
  let provider_uri = "file:///tmp/lifecycle-math.lgc" in
  let renamed_uri = "file:///tmp/lifecycle-renamed-math.lgc" in
  let consumer_uri = "file:///tmp/lifecycle-main.lgc" in
  let other_uri = "file:///tmp/lifecycle-other.lgc" in
  let provider_source = "(module Math (def answer 42))\n" in
  let consumer_source = "(def result Math/answer)\n" in
  let index =
    Lg.Language_service.create_workspace_index
      [
        (provider_uri, provider_source);
        (consumer_uri, consumer_source);
        (other_uri, "(def stable 7)\n");
      ]
    |> expect_ok
  in
  let other_before =
    Lg.Language_service.workspace_analysis index other_uri |> Option.get
  in
  let index, affected =
    Lg.Language_service.remove_workspace_file index ~filename:provider_uri
    |> expect_ok
  in
  if List.sort String.compare affected <> [ consumer_uri; provider_uri ] then
    failwith
      "removing a provider must invalidate the deleted file and dependents";
  if Lg.Language_service.workspace_analysis index provider_uri <> None then
    failwith "removed files must not retain an analysis";
  if Lg.Language_service.workspace_error index consumer_uri = None then
    failwith "dependents of removed providers must retain an analysis error";
  let other_after =
    Lg.Language_service.workspace_analysis index other_uri |> Option.get
  in
  if other_before != other_after then
    failwith "removing a file must reuse unrelated analyses";
  let index, affected =
    Lg.Language_service.remove_workspace_file index ~filename:provider_uri
    |> expect_ok
  in
  if affected <> [] then failwith "removing an absent file must be a no-op";
  let index, affected =
    Lg.Language_service.update_workspace_index index ~filename:renamed_uri
      ~source:provider_source
    |> expect_ok
  in
  if List.sort String.compare affected <> [ consumer_uri; renamed_uri ] then
    failwith "adding a renamed provider must reanalyze its dependents";
  let consumer =
    Lg.Language_service.workspace_analysis index consumer_uri |> Option.get
  in
  let usage = expect_substring_index consumer_source "Math/answer" in
  match Lg.Language_service.definition consumer ~offset:usage with
  | Some location
    when location.Location.loc_start.Lexing.pos_fname = renamed_uri ->
      ()
  | _ -> failwith "definitions must move to the re-added provider URI"

let test_workspace_index_rejects_duplicate_providers () =
  match
    Lg.Language_service.create_workspace_index
      [
        ("file:///tmp/provider-one.lgc", "(def shared-value 1)\n");
        ("file:///tmp/provider-two.lgc", "(def shared-value 2)\n");
        ("file:///tmp/provider-user.lgc", "(def result shared-value)\n");
      ]
  with
  | Error error
    when string_contains_substring error.message
           "workspace symbol shared-value has multiple providers" ->
      ()
  | Error error ->
      failwith ("unexpected workspace provider error: " ^ error.message)
  | Ok _ -> failwith "workspace index must reject duplicate symbol providers"

let test_workspace_index_contains_component_errors () =
  let math_uri = "file:///tmp/error-math.lgc" in
  let main_uri = "file:///tmp/error-main.lgc" in
  let other_uri = "file:///tmp/error-other.lgc" in
  let index =
    Lg.Language_service.create_workspace_index
      [
        (math_uri, "(module Math (def answer 40))\n");
        (main_uri, "(def result Math/answer)\n");
        (other_uri, "(def stable 7)\n");
      ]
    |> expect_ok
  in
  let other_before =
    Lg.Language_service.workspace_analysis index other_uri |> Option.get
  in
  let index, reanalyzed =
    Lg.Language_service.update_workspace_index index ~filename:math_uri
      ~source:"(module Math"
    |> expect_ok
  in
  if
    List.sort String.compare reanalyzed
    <> List.sort String.compare [ math_uri; main_uri ]
  then failwith "invalid edits must remain scoped to their dependency component";
  if Lg.Language_service.workspace_error index math_uri = None then
    failwith "invalid workspace documents must retain their analysis error";
  let other_after =
    Lg.Language_service.workspace_analysis index other_uri |> Option.get
  in
  if other_before != other_after then
    failwith "component errors must not discard unrelated cached analyses"

let test_workspace_index_records_partial_component_errors () =
  let math_uri = "file:///tmp/partial-math.lgc" in
  let main_uri = "file:///tmp/partial-main.lgc" in
  let index =
    Lg.Language_service.create_workspace_index
      [
        (math_uri, "(module Math (def answer 40))\n");
        (main_uri, "(def result (Math/missing 2))\n");
      ]
    |> expect_ok
  in
  if Lg.Language_service.workspace_analysis index math_uri = None then
    failwith "valid provider must retain its workspace analysis";
  if Lg.Language_service.workspace_analysis index main_uri <> None then
    failwith "invalid consumer must not receive a partial workspace analysis";
  if Lg.Language_service.workspace_error index main_uri = None then
    failwith "omitted component files must retain their analysis error"

let test_workspace_diagnostics_belong_to_their_source_file () =
  let status_uri = "file:///tmp/diagnostic-status.lgc" in
  let main_uri = "file:///tmp/diagnostic-main.lgc" in
  let analyses =
    Lg.Language_service.analyze_workspace
      [
        ( status_uri,
          {|
(type-variant status Active Inactive)
(module Status
  (defn describe [^:status value]
    (match value Active "active")))
|}
        );
        (main_uri, "(def label (Status/describe Active))\n");
      ]
    |> expect_ok
  in
  let status = List.assoc status_uri analyses in
  let main = List.assoc main_uri analyses in
  if Lg.Language_service.diagnostics status = [] then
    failwith "warning source must retain its diagnostic";
  if Lg.Language_service.diagnostics main <> [] then
    failwith "workspace diagnostics must not leak to dependent files"

let test_formatter_normalizes_whitespace () =
  Lg.Formatter.format "(defn  add-one [ x ](+ x  1))"
  |> expect_ok
  |> assert_equal_string "(defn add-one [x] (+ x 1))\n"

let test_formatter_wraps_long_nested_forms () =
  let source =
    "(defn describe [person] (str (:name person) \":\" (:age person) \":\" \
     (:admin? person) \":\" (:role person)))"
  in
  let expected =
    {|(defn
  describe
  [person]
  (str (:name person) ":" (:age person) ":" (:admin? person) ":" (:role person)))
|}
  in
  Lg.Formatter.format source |> expect_ok |> assert_equal_string expected

let test_formatter_preserves_comments_strings_and_is_idempotent () =
  let source = "; before\n(def message \"[not ; syntax]\") ; after\n" in
  let expected = "; before\n(def message \"[not ; syntax]\")\n; after\n" in
  let formatted = Lg.Formatter.format source |> expect_ok in
  assert_equal_string expected formatted;
  Lg.Formatter.format formatted |> expect_ok |> assert_equal_string formatted

let test_formatter_rejects_unbalanced_delimiters () =
  Lg.Formatter.format "(def answer 42]"
  |> expect_error_value "mismatched closing delimiter ]"

let test_match_delegates_opaque_module_constructor_payload_patterns_to_ocaml ()
    =
  let source =
    {|
(module Msg
  (type-variant message Empty (Named :string)))
(def named (Msg/Named "Ada"))
(def empty Msg/Empty)
(defn describe [^:Msg.message message]
  (match message
    (Msg/Named name) name
    Msg/Empty "empty"))
(println (str (describe named) ":" (describe empty)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "match_delegates_opaque_module_constructor_payload_patterns_to_ocaml"
    "Ada:empty\n" ocaml_source

let test_match_delegates_unknown_opaque_constructor_errors_to_ocaml () =
  Lg.Compiler.compile_string
    {|
(module Msg
  (type-variant message Empty (Named :string)))
(def named (Msg/Named "Ada"))
(defn describe [^:Msg.message message]
  (match message
    (Msg/Missing name) name
    Msg/Empty "empty"))
(def result (describe named))
|}
  |> expect_error_contains "Unbound constructor"

let test_match_delegates_nested_opaque_constructor_patterns_to_ocaml () =
  Lg.Compiler.compile_string
    {|
(defn extract [^:External.outer value]
  (match value
    (External/Outer (External/Inner result)) result
    _ "missing"))
|}
  |> expect_error_contains "Unbound module External"

let test_match_delegates_generic_host_payload_patterns_to_ocaml () =
  Lg.Compiler.compile_string
    {|
(defn extract [^:External.record value]
  (match (List/assoc-opt "id" (:attrs value))
    (Some (External/Named result)) result
    _ "missing"))
|}
  |> expect_error_contains "Unbound module External"

let test_match_supports_record_alias_or_and_guard_patterns () =
  let source =
    {|
(type-record user (name :string) (age :int))
(def user-value (record user (name "Ada") (age 42)))
(def record-label
  (match user-value
    (record (name name) (age age)) (str name ":" age)))
(defn describe-option [^:option<int> value]
  (match value
    (when (Some x) (> x 0)) (str "positive:" x)
    (or None (Some 0)) "empty"
    (as (Some x) _whole) (str "other:" x)))
(println
  (str record-label ":"
       (describe-option (Some 3)) ":"
       (describe-option (Some 0)) ":"
       (describe-option None) ":"
       (describe-option (Some -2))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "match_supports_record_alias_or_and_guard_patterns"
    "Ada:42:positive:3:empty:empty:other:-2\n" ocaml_source

let test_record_patterns_reject_unknown_and_duplicate_fields () =
  Lg.Compiler.compile_string
    {|
(type-record user (name :string))
(def user-value (record user (name "Ada")))
(def value (match user-value (record (missing x)) x))
|}
  |> expect_error_contains "unknown record pattern field missing";
  Lg.Compiler.compile_string
    {|
(type-record user (name :string))
(def user-value (record user (name "Ada")))
(def value (match user-value (record (name x) (name y)) x))
|}
  |> expect_error_contains "duplicate record pattern field name"

let test_record_patterns_require_record_targets () =
  Lg.Compiler.compile_string {|(def value (match 42 (record (name x)) x))|}
  |> expect_error_contains "record pattern expects a record target"

let test_or_patterns_require_the_same_binders () =
  Lg.Compiler.compile_string
    {|
(def value
  (match (Some 42)
    (or (Some x) None) x))
|}
  |> expect_error_contains "or-pattern alternatives must bind the same names"

let test_match_guards_must_be_boolean () =
  Lg.Compiler.compile_string
    {|
(def value
  (match (Some 42)
    (when (Some x) x) x
    None 0))
|}
  |> expect_error "match guard must be bool"

let test_try_catches_ocaml_exceptions () =
  let source =
    {|
(def result
  (try
    (raise (Failure "boom"))
    (catch (Failure message) (str "caught:" message))
    (catch _ "other")))
(println result)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "try_catches_ocaml_exceptions" "caught:boom\n" ocaml_source

let test_try_supports_normal_results_multiple_body_forms_and_handlers () =
  let source =
    {|
(def normal
  (try
    (println "body")
    "ok"
    (catch (Invalid_argument message) (str "invalid:" message))
    (catch _ "other")))
(def invalid
  (try
    (raise (Invalid_argument "bad"))
    (catch (Failure message) (str "failure:" message))
    (catch (Invalid_argument message)
      (println "handled")
      (str "invalid:" message))))
(println (str normal ":" invalid))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "try_supports_normal_results_multiple_body_forms_and_handlers"
    "body\nhandled\nok:invalid:bad\n" ocaml_source

let test_try_and_raise_reject_malformed_forms () =
  Lg.Compiler.compile_string {|(def value (try 42))|}
  |> expect_error "try requires at least one catch clause";
  Lg.Compiler.compile_string {|(def value (try 42 (catch)))|}
  |> expect_error "catch requires a pattern and body";
  Lg.Compiler.compile_string {|(def value (try (catch _ 42)))|}
  |> expect_error "try requires a body";
  Lg.Compiler.compile_string {|(def value (raise))|}
  |> expect_error "raise expects 1 arguments";
  Lg.Compiler.compile_string {|(def value (raise 1 2))|}
  |> expect_error "raise expects 1 arguments"

let test_try_supports_mixed_branch_types () =
  let source =
    {|
(def normal (try 42 (catch _ "bad")))
(def caught (try (raise (Failure "boom")) (catch _ "bad")))
(println (str (pr-str normal) ":" (pr-str caught)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "try_supports_mixed_branch_types" "42:\"bad\"\n"
    ocaml_source

let test_raise_payload_is_checked_by_ocaml () =
  Lg.Compiler.compile_string {|(def value (raise 42))|}
  |> expect_error_contains "exn"

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
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_definitions_work" "44:Ada:hi Grace\n" ocaml_source

let test_module_definitions_reject_expressions () =
  Lg.Compiler.compile_string {|
(module Math
  (println "side effect"))
|}
  |> expect_error
       "module forms must be module-signature, type-alias, type-record, \
        type-variant, open, include, module-alias, defprotocol, extend-type, \
        def, defonce, defn, defn-, or module"

let test_module_definitions_support_type_aliases () =
  let source =
    {|
(module UserIds
  (type-alias user-id :int)
  (defn keep [^:user_id x] x)
  (def answer (keep 42)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_definitions_support_type_aliases" "" ocaml_source

let test_module_definitions_support_variants () =
  let source =
    {|
(module Status
  (type-variant status Active Inactive)
  (def active Active))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_definitions_support_variants" "" ocaml_source

let test_open_module_exposes_values () =
  let source =
    {|
(module Math
  (def answer 40)
  (defn add2 [x] (+ x 2)))
(open Math)
(println (add2 answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "open_module_exposes_values" "42\n" ocaml_source

let test_include_module_exposes_values () =
  let source =
    {|
(module Math
  (def answer 40)
  (defn add2 [x] (+ x 2)))
(include Math)
(println (add2 answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "include_module_exposes_values" "42\n" ocaml_source

let test_module_alias_exposes_values () =
  let source =
    {|
(module Math
  (def answer 40)
  (defn add2 [x] (+ x 2)))
(module-alias M Math)
(println (M/add2 M/answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_alias_exposes_values" "42\n" ocaml_source

let test_slash_qualification_covers_members_and_constructor_patterns () =
  let source =
    {|
(module Msg
  (type-variant message Empty (Named :string)))
(module-alias M Msg)
(def named (M/Named "Ada"))
(defn describe [^:Msg.message message]
  (match message
    (M/Named name) (String/uppercase-ascii name)
    M/Empty "empty"))
(println (describe named))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "slash_qualification_covers_members_and_constructor_patterns" "ADA\n"
    ocaml_source;
  Lg.Compiler.compile_string
    {|
(defn extract [^:External.outer value]
  (match value
    (External/Outer (External/Inner result)) result
    _ "missing"))
|}
  |> expect_error_contains "Unbound module External"

let test_lowercase_host_aliases_qualify_constructor_patterns () =
  let source =
    {|
(require [ocaml.Result :as result])
(def value (result/Ok "Ada"))
(def label
  (match value
    (result/Ok name) name
    (result/Error message) message))
(println label)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "lowercase_host_aliases_qualify_constructor_patterns"
    "Ada\n" ocaml_source

let test_module_alias_targets_nested_modules () =
  let source =
    {|
(module User
  (module Name
    (defn greet [name] (str "hi " name))))
(module-alias N User.Name)
(println (N/greet "Grace"))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_alias_targets_nested_modules" "hi Grace\n"
    ocaml_source

let test_module_alias_rejects_bad_forms () =
  Lg.Compiler.compile_string {|(module-alias M)|}
  |> expect_error "module-alias expects alias and target modules"

let test_include_module_rejects_bad_forms () =
  Lg.Compiler.compile_string {|(include)|}
  |> expect_error "include expects one module";
  Lg.Compiler.compile_string {|(module App (include))|}
  |> expect_error "include expects one module"

let test_module_signatures_constrain_modules () =
  let source =
    {|
(module-signature MathSig
  (val answer :int))
(module Math MathSig
  (def answer 42))
(println Math/answer)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_signatures_constrain_modules" "42\n" ocaml_source

let test_module_signature_ascription_is_checked_by_ocaml () =
  Lg.Compiler.compile_string
    {|
(module-signature MathSig
  (val answer :string))
(module Math MathSig
  (def answer 42))
|}
  |> expect_error_contains "string"

let test_module_signatures_support_type_items () =
  let source =
    {|
(module-signature UserSig
  (type user-id :int)
  (val answer :user_id))
(module User UserSig
  (type-alias user-id :int)
  (def answer 42))
(defn keep [^:User.user_id value] value)
(def saved (keep User/answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_signatures_support_type_items" "" ocaml_source

let test_module_signatures_support_parameterized_manifest_types () =
  let source =
    {|
(module-signature BoxSig
  (type box [a] :option<a>)
  (val value :box<int>))
(module Box BoxSig
  (type-alias box [a] :option<a>)
  (def value (Some 42)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_signatures_support_parameterized_manifest_types" ""
    ocaml_source

let test_module_signatures_support_parameterized_abstract_types () =
  let source =
    {|
(module-signature BoxSig
  (type box [a]))
(module Box BoxSig
  (type-alias box [a] :option<a>))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_signatures_support_parameterized_abstract_types" ""
    ocaml_source

let test_module_signatures_support_nested_modules () =
  let source =
    {|
(module-signature ValueSig
  (val value :int))
(module-signature OuterSig
  (module Inner ValueSig))
(module Outer OuterSig
  (module Inner ValueSig
    (def value 42)))
(println Outer.Inner/value)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_signatures_support_nested_modules" "42\n"
    ocaml_source

let test_functor_parameters_expose_nested_signature_modules () =
  let source =
    {|
(module-signature ValueSig
  (val value :int))
(module-signature OuterSig
  (module Inner ValueSig))
(module Outer OuterSig
  (module Inner ValueSig
    (def value 42)))
(module-functor Read [O OuterSig]
  (def result O.Inner/value))
(module-apply App Read Outer)
(println App/result)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "functor_parameters_expose_nested_signature_modules" "42\n"
    ocaml_source

let test_nested_module_signatures_are_checked_by_ocaml () =
  Lg.Compiler.compile_string
    {|
(module-signature ValueSig
  (val value :int))
(module-signature OuterSig
  (module Inner ValueSig))
(module Outer OuterSig
  (module Inner
    (def value "forty-two")))
|}
  |> expect_error_contains "not included"

let test_module_signatures_include_other_signatures () =
  let source =
    {|
(module-signature BaseSig
  (val base :int))
(module-signature ExtendedSig
  (include BaseSig)
  (val extra :int))
(module Values ExtendedSig
  (def base 19)
  (def extra 23))
(println (+ Values/base Values/extra))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_signatures_include_other_signatures" "42\n"
    ocaml_source

let test_module_signature_cycles_are_rejected () =
  Lg.Compiler.compile_string
    {|
(module-signature A (include B))
(module-signature B (include A))
(module-functor Make [M A] (def result 1))
|}
  |> expect_error_contains "cyclic module signature include"

let test_functor_parameters_expose_included_signature_values () =
  let source =
    {|
(module-signature BaseSig
  (val base :int))
(module-signature ExtendedSig
  (include BaseSig)
  (val extra :int))
(module Values ExtendedSig
  (def base 19)
  (def extra 23))
(module-functor Sum [V ExtendedSig]
  (def result (+ V/base V/extra)))
(module-apply App Sum Values)
(println App/result)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "functor_parameters_expose_included_signature_values" "42\n"
    ocaml_source

let test_included_module_signatures_are_checked_by_ocaml () =
  Lg.Compiler.compile_string
    {|
(module-signature BaseSig
  (val base :int))
(module-signature ExtendedSig
  (include BaseSig))
(module Values ExtendedSig
  (def other 42))
|}
  |> expect_error_contains "not included"

let test_unknown_signature_includes_are_checked_by_ocaml () =
  Lg.Compiler.compile_string
    {|
(module-signature ExtendedSig
  (include MissingSig))
|}
  |> expect_error_contains "Unbound module type"

let test_module_signature_type_items_are_checked_by_ocaml () =
  Lg.Compiler.compile_string
    {|
(module-signature UserSig
  (type user-id :string))
(module User UserSig
  (type-alias user-id :int))
|}
  |> expect_error_contains "user_id"

let test_module_signatures_support_abstract_type_items () =
  let source =
    {|
(module-signature UserSig
  (type user-id)
  (val answer :user_id))
(module User UserSig
  (type-alias user-id :int)
  (def answer 42))
(defn keep [^:User.user_id value] value)
(def saved (keep User/answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_signatures_support_abstract_type_items" ""
    ocaml_source

let test_module_signature_abstract_types_are_checked_by_ocaml () =
  Lg.Compiler.compile_string
    {|
(module-signature UserSig
  (type user-id)
  (val answer :user_id))
(module User UserSig
  (type-alias user-id :int)
  (def answer 42))
(def bad (+ User/answer 1))
|}
  |> expect_error_contains "User.user_id"

let test_module_signatures_reject_bad_forms () =
  Lg.Compiler.compile_string {|(module-signature MathSig)|}
  |> expect_error "module-signature expects at least one signature item";
  Lg.Compiler.compile_string {|(module-signature MathSig (value answer :int))|}
  |> expect_error
       "module-signature items must be val, type, module, or include \
        declarations";
  Lg.Compiler.compile_string {|(module-signature OuterSig (module Inner))|}
  |> expect_error
       "module-signature items must be val, type, module, or include \
        declarations";
  Lg.Compiler.compile_string {|(module-signature ExtendedSig (include))|}
  |> expect_error "module-signature include expects one module type";
  Lg.Compiler.compile_string
    {|(module-signature MathSig (val answer :unknown))|}
  |> expect_error_contains "Unbound type constructor unknown";
  Lg.Compiler.compile_string
    {|(module-signature MathSig (type user-id :unknown))|}
  |> expect_error_contains "Unbound type constructor unknown";
  Lg.Compiler.compile_string
    {|(module-signature BoxSig (type box [a] :option<b>))|}
  |> expect_error "unknown type parameter b";
  Lg.Compiler.compile_string {|(module-signature BoxSig (type box [a a]))|}
  |> expect_error "duplicate type parameter a";
  Lg.Compiler.compile_string {|(module-signature BoxSig (type box []))|}
  |> expect_error "type parameter vector must not be empty"

let test_module_functors_apply_modules () =
  let source =
    {|
(module-signature MathSig
  (val answer :int))
(module Math MathSig
  (def answer 40))
(module-functor Make [M MathSig]
  (def result (+ M/answer 2)))
(module-apply App Make Math)
(println App/result)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_functors_apply_modules" "42\n" ocaml_source

let test_module_functors_apply_multiple_modules () =
  let source =
    {|
(module-signature NumberSig
  (val value :int))
(module Left NumberSig
  (def value 19))
(module Right NumberSig
  (def value 23))
(module-functor Add [L NumberSig R NumberSig]
  (def result (+ L/value R/value)))
(module-apply App Add Left Right)
(println App/result)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_functors_apply_multiple_modules" "42\n" ocaml_source

let test_multi_parameter_functor_application_is_checked_by_ocaml () =
  Lg.Compiler.compile_string
    {|
(module-signature NumberSig
  (val value :int))
(module Good NumberSig
  (def value 19))
(module Bad
  (def value "twenty-three"))
(module-functor Add [L NumberSig R NumberSig]
  (def result (+ L/value R/value)))
(module-apply App Add Good Bad)
|}
  |> expect_error_contains "not compatible"

let test_module_functor_applications_expose_record_types () =
  let source =
    {|
(module-signature NameSig
  (val suffix :string))
(module Names NameSig
  (def suffix "!"))
(module-functor Make [M NameSig]
  (type-record user (name :string) (age :int)))
(module-apply App Make Names)
(def ada (record App.user (name "Ada") (age 41)))
(println (str (:name ada) ":" (+ (:age ada) 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_functor_applications_expose_record_types" "Ada:42\n"
    ocaml_source

let test_module_functor_applications_expose_protocols () =
  let source =
    {|
(module-signature NameSig
  (val suffix :string))
(module Names NameSig
  (def suffix "!"))
(module-functor Make [M NameSig]
  (defprotocol Labelled (label [x] :string))
  (extend-type :int Labelled (label [x] (str x M/suffix))))
(module-apply App Make Names)
(println (App/Labelled/label 9))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_functor_applications_expose_protocols" "9!\n"
    ocaml_source

let test_module_variants_export_constructors () =
  let source =
    {|
(module Status
  (type-variant status Active (Named :string)))
(def active Status/Active)
(def named (Status/Named "ready"))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_variants_export_constructors" "" ocaml_source

let test_module_functor_applications_expose_variant_constructors () =
  let source =
    {|
(module-signature EmptySig (val dummy :int))
(module Empty EmptySig (def dummy 0))
(module-functor Make [M EmptySig]
  (type-variant status Active (Named :string)))
(module-apply App Make Empty)
(def active App/Active)
(def named (App/Named "ready"))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_functor_applications_expose_variant_constructors" ""
    ocaml_source

let test_module_functor_applications_preserve_record_protocol_identity () =
  let source =
    {|
(module-signature EmptySig (val dummy :int))
(module Empty EmptySig (def dummy 0))
(module-functor Make [M EmptySig]
  (type-record user (name :string))
  (defprotocol Labelled (label [x] :string))
  (extend-type user Labelled (label [x] (:name x)))
  (def ada (record user (name "Ada"))))
(module-apply App Make Empty)
(println (App/Labelled/label App/ada))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "module_functor_applications_preserve_record_protocol_identity" "Ada\n"
    ocaml_source

let test_module_functor_applications_register_applied_types () =
  let state =
    typecheck_state
      {|
(module-signature EmptySig (val dummy :int))
(module Empty EmptySig (def dummy 0))
(module-functor Make [M EmptySig]
  (type-record user (name :string)))
(module-apply App Make Empty)
|}
  in
  match
    Lg.Type_registry.find_by_emitted_name "App.user"
      (Lg.Compiler_environment.types state.env)
  with
  | Some declaration
    when Lg.Type_id.equal declaration.type_id
           (Lg.Type_id.create ~owner:[ "App" ] ~name:"user") ->
      ()
  | _ -> failwith "applied functor types must have remapped stable identities"

let test_module_functor_applications_register_nested_modules () =
  let state =
    typecheck_state
      {|
(module-signature EmptySig (val dummy :int))
(module Empty EmptySig (def dummy 0))
(module-functor Make [M EmptySig]
  (module Inner (def value 42)))
(module-apply App Make Empty)
|}
  in
  let nested_id = Lg.Module_id.create ~owner:[ "App" ] ~name:"Inner" in
  if
    not
      (Lg.Module_registry.mem_module nested_id
         (Lg.Compiler_environment.modules state.env))
  then failwith "applied functors must register nested module identities"

let test_module_functor_applications_expose_nested_module_protocols () =
  let source =
    {|
(module-signature EmptySig (val dummy :int))
(module Empty EmptySig (def dummy 0))
(module-functor Make [M EmptySig]
  (module Inner
    (defprotocol Labelled (label [x] :string))
    (extend-type :int Labelled (label [x] (str "nested:" x)))))
(module-apply App Make Empty)
(println (App.Inner/Labelled/label 9))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_functor_applications_expose_nested_module_protocols"
    "nested:9\n" ocaml_source

let test_module_functor_applications_preserve_nested_module_aliases () =
  let source =
    {|
(module-signature EmptySig (val dummy :int))
(module Empty EmptySig (def dummy 0))
(module-functor Make [M EmptySig]
  (module Source
    (defprotocol Labelled (label [x] :string))
    (extend-type :int Labelled (label [x] (str "alias:" x))))
  (module-alias Alias Source))
(module-apply App Make Empty)
(println (App.Alias/Labelled/label 9))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_functor_applications_preserve_nested_module_aliases"
    "alias:9\n" ocaml_source

let test_module_functor_application_is_checked_by_ocaml () =
  Lg.Compiler.compile_string
    {|
(module-signature MathSig
  (val answer :int))
(module Bad
  (def answer "forty"))
(module-functor Make [M MathSig]
  (def result M/answer))
(module-apply App Make Bad)
|}
  |> expect_error_contains "not compatible"

let test_module_functors_reject_bad_forms () =
  Lg.Compiler.compile_string {|(module-functor Make M MathSig)|}
  |> expect_error
       "module-functor expects a name, [parameter signature ...], and body";
  Lg.Compiler.compile_string {|(module-functor Make [] (def answer 42))|}
  |> expect_error "module-functor parameter vector must not be empty";
  Lg.Compiler.compile_string
    {|(module-functor Make [M MathSig N] (def answer 42))|}
  |> expect_error "module-functor parameters must be name/signature pairs";
  Lg.Compiler.compile_string
    {|(module-functor Make [M :MathSig] (def answer 42))|}
  |> expect_error "module-functor parameters must be symbols";
  Lg.Compiler.compile_string {|(module-apply App Make)|}
  |> expect_error
       "module-apply expects result, functor, and one or more argument modules"

let test_module_definitions_support_open () =
  let source =
    {|
(module Math
  (def answer 40)
  (defn add2 [x] (+ x 2)))
(module App
  (open Math)
  (def result (add2 answer)))
(println App/result)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_definitions_support_open" "42\n" ocaml_source

let test_module_definitions_support_include () =
  let source =
    {|
(module Math
  (def answer 40)
  (defn add2 [x] (+ x 2)))
(module App
  (include Math)
  (def result (add2 answer)))
(println (str App/result ":" (App/add2 App/answer)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_definitions_support_include" "42:42\n" ocaml_source

let test_module_definitions_support_module_alias () =
  let source =
    {|
(module Math
  (def answer 40)
  (defn add2 [x] (+ x 2)))
(module App
  (module-alias M Math)
  (def result (M/add2 M/answer)))
(println (str App/result ":" (App.M/add2 App.M/answer)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_definitions_support_module_alias" "42:42\n"
    ocaml_source

let test_incremental_compilation_preserves_modules () =
  let state = Lg.Compiler.empty_state in
  let state, module_ocaml =
    Lg.Compiler.compile_chunk state
      {|
(module Math
  (defn add2 [x] (+ x 2)))
|}
    |> expect_ok
  in
  let _state, app_ocaml =
    Lg.Compiler.compile_chunk state {|
(println (Math/add2 40))
|} |> expect_ok
  in
  assert_ocaml_runs "incremental_compilation_preserves_modules" "42\n"
    (module_ocaml ^ "\n\n" ^ app_ocaml)

let test_incremental_compilation_preserves_opened_modules () =
  let state = Lg.Compiler.empty_state in
  let state, module_ocaml =
    Lg.Compiler.compile_chunk state
      {|
(module Math
  (def answer 40)
  (defn add2 [x] (+ x 2)))
|}
    |> expect_ok
  in
  let _state, app_ocaml =
    Lg.Compiler.compile_chunk state {|
(open Math)
(println (add2 answer))
|}
    |> expect_ok
  in
  assert_ocaml_runs "incremental_compilation_preserves_opened_modules" "42\n"
    (module_ocaml ^ "\n\n" ^ app_ocaml)

let test_incremental_compilation_preserves_module_aliases () =
  let state = Lg.Compiler.empty_state in
  let state, module_ocaml =
    Lg.Compiler.compile_chunk state
      {|
(module Math
  (def answer 40)
  (defn add2 [x] (+ x 2)))
|}
    |> expect_ok
  in
  let _state, app_ocaml =
    Lg.Compiler.compile_chunk state
      {|
(module-alias M Math)
(println (M/add2 M/answer))
|}
    |> expect_ok
  in
  assert_ocaml_runs "incremental_compilation_preserves_module_aliases" "42\n"
    (module_ocaml ^ "\n\n" ^ app_ocaml)

let test_incremental_compilation_preserves_state () =
  let state = Lg.Compiler.empty_state in
  let state, people_ocaml =
    Lg.Compiler.compile_chunk state
      {|(module People (def user {:name "Ada", :age 36}))|}
    |> expect_ok
  in
  let _state, app_ocaml =
    Lg.Compiler.compile_chunk state
      {|
(def updated (assoc People/user :admin? true))
(println (str (:name updated) ":" (:admin? updated) ":" (:age updated)))
|}
    |> expect_ok
  in
  assert_ocaml_runs "incremental_compilation_preserves_state" "Ada:true:36\n"
    (people_ocaml ^ "\n\n" ^ app_ocaml)

let test_incremental_compilation_preserves_record_sets () =
  let state = Lg.Compiler.empty_state in
  let state, people_ocaml =
    Lg.Compiler.compile_chunk state {|
(def ada {:name "Ada", :age 36})
|}
    |> expect_ok
  in
  let _state, app_ocaml =
    Lg.Compiler.compile_chunk state
      {|
(def users (hash-set ada))
(println (str (count users) ":" (contains? users ada)))
|}
    |> expect_ok
  in
  assert_ocaml_runs "incremental_compilation_preserves_record_sets" "1:true\n"
    (people_ocaml ^ "\n\n" ^ app_ocaml)

let test_incremental_compilation_preserves_composite_sets () =
  let state = Lg.Compiler.empty_state in
  let state, collections_ocaml =
    Lg.Compiler.compile_chunk state {|
(def values (hash-set [1 2]))
|}
    |> expect_ok
  in
  let _state, app_ocaml =
    Lg.Compiler.compile_chunk state
      {|
(def updated (conj values [2 3]))
(println (str (count updated) ":" (contains? updated [2 3])))
|}
    |> expect_ok
  in
  assert_ocaml_runs "incremental_compilation_preserves_composite_sets"
    "2:true\n"
    (collections_ocaml ^ "\n\n" ^ app_ocaml)

let test_module_definitions_support_record_sets () =
  let source =
    {|
(module People
  (def ada {:name "Ada", :age 36})
  (def users (hash-set ada)))
(println (str (count People/users) ":" (contains? People/users People/ada)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_definitions_support_record_sets" "1:true\n"
    ocaml_source

let test_module_definitions_support_composite_sets () =
  let source =
    {|
(module Groups
  (def values (hash-set (list 1 2))))
(println (str (count Groups/values) ":" (contains? Groups/values (list 1 2))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_definitions_support_composite_sets" "1:true\n"
    ocaml_source

let test_incremental_compilation_requires_prior_state () =
  Lg.Compiler.compile_chunk Lg.Compiler.empty_state
    {|(println (:name People/user))|}
  |> expect_error_value "unknown symbol People/user"

let test_incremental_compilation_preserves_protocols () =
  let state = Lg.Compiler.empty_state in
  let state, protocol_ocaml =
    Lg.Compiler.compile_chunk state
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
    Lg.Compiler.compile_chunk state {|
(println (label 42))
|} |> expect_ok
  in
  assert_ocaml_runs "incremental_compilation_preserves_protocols" "int:42\n"
    (protocol_ocaml ^ "\n\n" ^ call_ocaml)

let test_incremental_compile_chunk_runs_ocaml_typecheck_gate () =
  Lg.Compiler.compile_chunk Lg.Compiler.empty_state
    {|
(def answer (Stdlib.abs "bad"))
|}
  |> expect_error_contains "string"

let test_parsetree_backend_prints_runnable_ocaml () =
  let source =
    {|
(def user {:name "Ada", :age 36})
(println (str (:name user) ":" (:age user)))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_prints_runnable_ocaml" "Ada:36\n"
    ocaml_source

let test_parsetree_backend_supports_record_sets () =
  let source =
    {|
(def ada {:name "Ada", :age 36})
(def users (hash-set ada))
(println (str (count users) ":" (contains? users ada)))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_record_sets" "1:true\n"
    ocaml_source

let test_parsetree_backend_supports_composite_sets () =
  let source =
    {|
(def values (hash-set [1 2]))
(def updated (conj values [2 3]))
(println (str (count updated) ":" (contains? updated [2 3])))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_composite_sets" "2:true\n"
    ocaml_source

let test_parsetree_backend_supports_type_aliases () =
  let source =
    {|
(type-alias user-id :int)
(defn keep-user-id [^:user_id x] x)
(def answer (keep-user-id 42))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_type_aliases" "" ocaml_source

let test_parsetree_backend_supports_generic_ocaml_calls () =
  let source =
    {|
(def answer (Stdlib.abs -42))
(def label (String.uppercase_ascii "ada"))
(println (str label ":" answer))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_generic_ocaml_calls" "ADA:42\n"
    ocaml_source

let test_parsetree_backend_supports_ocaml_option_and_result_constructors () =
  let source =
    {|
(def present (Some 42))
(def absent None)
(def success (Ok "Ada"))
(def failure (Error "bad"))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs
    "parsetree_backend_supports_ocaml_option_and_result_constructors" ""
    ocaml_source

let test_parsetree_backend_supports_ocaml_option_and_result_patterns () =
  let source =
    {|
(def present (Some 41))
(def success (Ok "Ada"))
(def present-score
  (match present
    (Some x) (+ x 1)
    None 0))
(def success-label
  (match success
    (Ok name) name
    (Error message) message))
(println (str present-score ":" success-label))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs
    "parsetree_backend_supports_ocaml_option_and_result_patterns" "42:Ada\n"
    ocaml_source

let test_parsetree_backend_supports_ocaml_type_application_annotations () =
  let source =
    {|
(def present (Some 41))
(def success (Ok "Ada"))
(defn option-score [^:option<int> value]
  (match value
    (Some x) (+ x 1)
    None 0))
(defn result-label [^:result<string;string> value]
  (match value
    (Ok name) name
    (Error message) message))
(println (str (option-score present) ":" (result-label success)))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs
    "parsetree_backend_supports_ocaml_type_application_annotations" "42:Ada\n"
    ocaml_source

let test_parsetree_backend_supports_ocaml_tuple_values () =
  let source =
    {|
(def pair (tuple 41 "Ada"))
(defn describe [^:tuple<int;string> value]
  (match value
    (tuple id name) (str name ":" (+ id 1))))
(println (describe pair))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_ocaml_tuple_values" "Ada:42\n"
    ocaml_source

let test_parsetree_backend_supports_ocaml_record_values () =
  let source =
    {|
(type-record user (name :string) (age :int))
(def ada (record user (name "Ada") (age 41)))
(println (str (:name ada) ":" (+ (:age ada) 1)))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_ocaml_record_values" "Ada:42\n"
    ocaml_source

let test_parsetree_backend_supports_variants () =
  let source =
    {|
(type-variant status Active Inactive)
(def active Active)
(defn keep-status [^:status x] x)
(def saved (keep-status active))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_variants" "" ocaml_source

let test_parsetree_backend_supports_payload_variants () =
  let source =
    {|
(type-variant message Ping (Named :string) (Pair :int :string))
(def named (Named "Ada"))
(def pair (Pair 42 "Ada"))
(defn describe [^:message message]
  (match message
    (Named name) name
    (Pair id name) (str name ":" id)
    Ping "ping"))
(println (str (describe named) ":" (describe pair)))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_payload_variants" "Ada:Ada:42\n"
    ocaml_source

let test_parsetree_backend_supports_ocaml_constructor_patterns () =
  let source =
    {|
(type-variant status Active Inactive)
(def active Active)
(def inactive Inactive)
(defn describe [^:status status]
  (match status
    Active "active"
    Inactive "inactive"))
(println (str (describe active) ":" (describe inactive)))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_ocaml_constructor_patterns"
    "active:inactive\n" ocaml_source

let test_parsetree_backend_supports_open_module () =
  let source =
    {|
(module Math
  (def answer 40)
  (defn add2 [x] (+ x 2)))
(open Math)
(println (add2 answer))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_open_module" "42\n" ocaml_source

let test_parsetree_backend_supports_include_module () =
  let source =
    {|
(module Math
  (def answer 40)
  (defn add2 [x] (+ x 2)))
(include Math)
(println (add2 answer))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_include_module" "42\n"
    ocaml_source

let test_parsetree_backend_supports_module_alias () =
  let source =
    {|
(module Math
  (def answer 40)
  (defn add2 [x] (+ x 2)))
(module-alias M Math)
(println (M/add2 M/answer))
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_module_alias" "42\n"
    ocaml_source

let test_parsetree_backend_supports_module_signatures () =
  let source =
    {|
(module-signature MathSig
  (val answer :int))
(module Math MathSig
  (def answer 42))
(println Math/answer)
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_module_signatures" "42\n"
    ocaml_source

let test_parsetree_backend_supports_module_functors () =
  let source =
    {|
(module-signature MathSig
  (val answer :int))
(module Math MathSig
  (def answer 40))
(module-functor Make [M MathSig]
  (def result (+ M/answer 2)))
(module-apply App Make Math)
(println App/result)
|}
  in
  let structure = Lg.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Lg.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_module_functors" "42\n"
    ocaml_source

let test_parsetree_backend_preserves_static_errors () =
  Lg.Compiler.compile_parsetree {|(def x (+ 1 "two"))|}
  |> expect_error_value "expected int arguments for +"

let test_parsetree_backend_runs_ocaml_typecheck_gate () =
  Lg.Compiler.compile_parsetree {|
(def answer (Stdlib.abs "bad"))
|}
  |> expect_error_contains "string"

let test_parsetree_backend_builds_native_record_items () =
  let structure =
    Lg.Compiler.compile_parsetree {|(def user {:name "Ada", :age 36})|}
    |> expect_ok
  in
  match structure with
  | [ type_item; set_module_item; value_item ] -> (
      match
        (type_item.pstr_desc, set_module_item.pstr_desc, value_item.pstr_desc)
      with
      | Pstr_type _, Pstr_module _, Pstr_value _ -> ()
      | _ ->
          failwith
            "expected record type, comparator module, and value structure items"
      )
  | _ ->
      failwith
        "expected record type, comparator module, and value structure items"

let test_parsetree_backend_builds_native_value_items () =
  let structure =
    Lg.Compiler.compile_parsetree {|
(def answer 42)
(println answer)
|}
    |> expect_ok
  in
  match structure with
  | [ definition; effect_item ] -> (
      match (definition.pstr_desc, effect_item.pstr_desc) with
      | Pstr_value _, Pstr_value _ -> ()
      | _ -> failwith "expected definition and effect value structure items")
  | _ -> failwith "expected exactly two value structure items"

let test_parsetree_backend_builds_native_defn_items () =
  let structure =
    Lg.Compiler.compile_parsetree
      {|(defn next-age [person] (+ (:age person) 1))|}
    |> expect_ok
  in
  match structure with
  | [ type_item; value_item ] -> (
      match (type_item.pstr_desc, value_item.pstr_desc) with
      | Pstr_type _, Pstr_value _ -> ()
      | _ -> failwith "expected row type and function value structure items")
  | _ -> failwith "expected row type and function value structure items"

let test_parsetree_backend_builds_native_protocol_items () =
  let structure =
    Lg.Compiler.compile_parsetree
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
      | Pstr_value _ -> ()
      | _ -> failwith "expected protocol implementation value item")
  | _ -> failwith "expected one protocol implementation value item"

let test_parsetree_backend_builds_native_module_items () =
  let structure =
    Lg.Compiler.compile_parsetree
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
          | Pmod_structure [ _value_item; _nested_item ] -> ()
          | _ -> failwith "expected value and nested module body items")
      | _ -> failwith "expected module structure item")
  | _ -> failwith "expected one module structure item"

let test_parsetree_backend_builds_native_module_alias_items () =
  let structure =
    Lg.Compiler.compile_parsetree
      {|
(module Math
  (def answer 42))
(module-alias M Math)
|}
    |> expect_ok
  in
  match structure with
  | [ _module_item; alias_item ] -> (
      match alias_item.pstr_desc with
      | Pstr_module binding -> (
          match binding.pmb_expr.pmod_desc with
          | Pmod_ident _ -> ()
          | _ -> failwith "expected module alias expression")
      | _ -> failwith "expected module alias structure item")
  | _ -> failwith "expected module definition and alias items"

let test_parsetree_backend_builds_native_module_signature_items () =
  let structure =
    Lg.Compiler.compile_parsetree
      {|
(module-signature MathSig
  (val answer :int))
(module Math MathSig
  (def answer 42))
|}
    |> expect_ok
  in
  match structure with
  | [ signature_item; module_item ] -> (
      match (signature_item.pstr_desc, module_item.pstr_desc) with
      | Pstr_modtype _, Pstr_module binding -> (
          match binding.pmb_expr.pmod_desc with
          | Pmod_constraint _ -> ()
          | _ -> failwith "expected constrained module expression")
      | _ -> failwith "expected module type and constrained module items")
  | _ -> failwith "expected signature and constrained module items"

let test_parsetree_backend_builds_native_module_functor_items () =
  let structure =
    Lg.Compiler.compile_parsetree
      {|
(module-signature MathSig
  (val answer :int))
(module Math MathSig
  (def answer 40))
(module-functor Make [M MathSig]
  (def result (+ M/answer 2)))
(module-apply App Make Math)
|}
    |> expect_ok
  in
  match structure with
  | [ _signature_item; _module_item; functor_item; apply_item ] -> (
      match (functor_item.pstr_desc, apply_item.pstr_desc) with
      | Pstr_module functor_binding, Pstr_module apply_binding -> (
          match
            ( functor_binding.pmb_expr.pmod_desc,
              apply_binding.pmb_expr.pmod_desc )
          with
          | Pmod_functor _, Pmod_apply _ -> ()
          | _ -> failwith "expected functor and application module expressions")
      | _ -> failwith "expected functor and application module items")
  | _ -> failwith "expected signature, module, functor, and application items"

let test_parsetree_backend_builds_native_scalar_expressions () =
  List.iter expect_structured_value_expression
    [
      {|(def answer 42)|};
      {|(def result (boolean 1))|};
      {|(def result (integer? 1))|};
      {|(def result (bit-set 1 2))|};
      {|(def result (name :user/name))|};
      {|(def result (namespace :user/name))|};
      {|(def result (keyword "user" "name"))|};
      {|(def result (symbol :user :name))|};
    ]

let test_parsetree_backend_builds_native_collection_expressions () =
  let structure =
    Lg.Compiler.compile_parsetree {|
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
  | [ vector_item; list_item ] -> (
      let vector_expr = value_expression vector_item in
      let list_expr = value_expression list_item in
      match (vector_expr.pexp_desc, list_expr.pexp_desc) with
      | Pexp_apply _, Pexp_construct _ -> ()
      | _ ->
          failwith
            "expected vector application and list constructor expressions")
  | _ -> failwith "expected vector and list value bindings"

let test_parsetree_backend_builds_native_conditional_expressions () =
  let structure =
    Lg.Compiler.compile_parsetree
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
               match expression.pexp_desc with
               | Pexp_ifthenelse _ -> true
               | _ -> false)
             expressions)
      then
        failwith "expected native conditional expressions with ghost locations"
  | _ -> failwith "expected three conditional value bindings"

let test_parsetree_backend_builds_native_function_expressions () =
  expect_structured_value_expression {|(defn identity-value [x] x)|}

let test_parsetree_backend_builds_native_sequence_expressions () =
  expect_structured_value_expression {|(def result (do 1 2 3))|}

let test_parsetree_backend_builds_native_sequence_navigation_expressions () =
  List.iter expect_structured_value_expression
    [
      {|(def result (next (list 1 2)))|};
      {|(def result (next [1 2]))|};
      {|(def result (nthnext (list 1 2 3) 2))|};
      {|(def result (nthnext [1 2 3] 2))|};
      {|(def result (nthrest (list 1 2 3) 2))|};
      {|(def result (nthrest [1 2 3] 2))|};
      {|(def result (rseq [1 2]))|};
    ]

let test_parsetree_backend_builds_native_let_expressions () =
  expect_structured_value_expression
    {|(def result (let [x 1 y (+ x 1)] (+ y 1)))|}

let test_parsetree_backend_builds_native_match_expressions () =
  expect_structured_value_expression
    {|(def result (match [1 2] [x y] (+ x y)))|}

let test_parsetree_backend_builds_native_cond_expressions () =
  expect_structured_value_expression
    {|(def result (cond false 0 true 1 :else 2))|}

let test_parsetree_backend_builds_native_integer_expressions () =
  expect_structured_value_expression
    {|(def result (bit-or (+ 1 2) (bit-shift-left 1 2)))|}

let test_parsetree_backend_builds_native_comparison_expressions () =
  expect_structured_value_expression {|(def result (< 1 2 3))|}

let test_parsetree_backend_builds_native_record_field_expressions () =
  expect_structured_value_expression
    {|(def user {:name "Ada", :age 36})(def age (get user :age))|}

let test_parsetree_backend_builds_native_boolean_expressions () =
  expect_structured_value_expression {|(def result (not false))|}

let test_parsetree_backend_builds_native_string_expressions () =
  expect_structured_value_expression {|(def result (subs "lg" 1 4))|}

let test_parsetree_backend_builds_native_collection_core_expressions () =
  expect_structured_value_expression {|(def result (count [1 2 3]))|}

let test_parsetree_backend_builds_native_collection_match_expressions () =
  expect_structured_value_expression {|(def result (rest (list 1 2 3)))|}

let test_parsetree_backend_builds_native_function_combinator_expressions () =
  expect_structured_value_expression {|(def result (constantly 42))|}

let test_parsetree_backend_builds_native_partial_expressions () =
  expect_structured_value_expression {|(def add-ten (partial + 10))|}

let test_parsetree_backend_builds_native_empty_collection_expressions () =
  expect_structured_value_expression {|(def xs (vector-of :int))|}

let test_parsetree_backend_builds_native_collection_update_expressions () =
  expect_structured_value_expression {|(def xs (conj [1 2] 3))|}

let test_parsetree_backend_builds_native_collection_index_expressions () =
  expect_structured_value_expression {|(def x (nth [1 2 3] 1))|}

let test_parsetree_backend_builds_native_map_vector_expressions () =
  expect_structured_value_expression
    {|(def names (keys {:name "Ada", :age 36}))|}

let test_parsetree_backend_builds_native_contains_expressions () =
  expect_structured_value_expression
    {|(def present? (contains? {:name "Ada"} :name))|}

let test_parsetree_backend_builds_native_set_constructor_expressions () =
  expect_structured_value_expression {|(def ids (hash-set 3 1 2))|}

let test_parsetree_backend_builds_native_sequence_transform_expressions () =
  List.iter expect_structured_value_expression
    [
      {|(def result (sort [3 1 2]))|};
      {|(def result (concat [1 2] (list 3 4)))|};
      {|(def result (vec (list 1 2)))|};
      {|(def result (set [1 1 2]))|};
      {|(def result (repeat 3 :name))|};
      {|(def result (interpose 0 [1 2 3]))|};
      {|(def result (interleave [1 2] (list 3 4)))|};
      {|(def result (partition 2 [1 2 3]))|};
      {|(def result (partition-all 2 [1 2 3]))|};
      {|(def result (split-at 2 [1 2 3]))|};
      {|(def result (bounded-count 2 [1 2 3]))|};
      {|(def result (dorun [1 2 3]))|};
    ]

let test_incremental_parsetree_backend_preserves_state () =
  let state = Lg.Compiler.empty_state in
  let state, people_structure =
    Lg.Compiler.compile_chunk_parsetree state
      {|(module People (def user {:name "Ada", :age 36}))|}
    |> expect_ok
  in
  let _state, app_structure =
    Lg.Compiler.compile_chunk_parsetree state
      {|
(println (str (:name People/user) ":" (:age People/user)))
|}
    |> expect_ok
  in
  let people_ocaml = Lg.Compiler.print_parsetree people_structure in
  let app_ocaml = Lg.Compiler.print_parsetree app_structure in
  assert_ocaml_runs "incremental_parsetree_backend_preserves_state" "Ada:36\n"
    (people_ocaml ^ "\n\n" ^ app_ocaml)

let test_incremental_parsetree_backend_runs_ocaml_typecheck_gate () =
  Lg.Compiler.compile_chunk_parsetree Lg.Compiler.empty_state
    {|
(def answer (Stdlib.abs "bad"))
|}
  |> expect_error_contains "string"

let test_compile_string_prints_parsetree_backend_output () =
  let source =
    {|
(type-record user (name :string) (age :int))
(def user (record user (name "Ada") (age 36)))
(println (str (:name user) ":" (:age user)))
|}
  in
  let source_output = Lg.Compiler.compile_string source |> expect_ok in
  let parsetree_output =
    Lg.Compiler.compile_parsetree source
    |> expect_ok |> Lg.Compiler.print_parsetree
  in
  if source_output <> parsetree_output then
    failwith "compile_string should print the checked Parsetree backend output"

let test_infer_interface_prints_checked_signature () =
  let inferred =
    Lg.Compiler.infer_interface
      {|
(type-record user (name :string))
(defn user-name [^:user user] (:name user))
|}
    |> expect_ok
  in
  if not (string_contains_substring inferred "type nonrec user") then
    failwith "inferred interface should include the record type";
  if not (string_contains_substring inferred "val user_name : user -> string")
  then failwith "inferred interface should include the function signature"

let test_compile_chunk_prints_parsetree_backend_output () =
  let source =
    {|
(module Greeter
  (defn shout [name] (String.uppercase_ascii name)))
|}
  in
  let _, source_output =
    Lg.Compiler.compile_chunk Lg.Compiler.empty_state source |> expect_ok
  in
  let _, structure =
    Lg.Compiler.compile_chunk_parsetree Lg.Compiler.empty_state source
    |> expect_ok
  in
  let parsetree_output = Lg.Compiler.print_parsetree structure in
  if source_output <> parsetree_output then
    failwith "compile_chunk should print the checked Parsetree backend output"

let tests =
  [
    ( "records, assoc, and dissoc generate typed OCaml",
      test_records_assoc_and_dissoc );
    ( "assoc rejects changing an existing field type",
      test_assoc_rejects_type_changes );
    ("dissoc rejects unknown fields", test_dissoc_rejects_unknown_fields);
    ("map literals reject duplicate fields", test_map_rejects_duplicate_fields);
    ( "hash-map constructs structural maps",
      test_hash_map_constructs_structural_maps );
    ("hash-map rejects duplicate fields", test_hash_map_rejects_duplicate_fields);
    ( "hash-map rejects odd key value forms",
      test_hash_map_rejects_odd_key_value_forms );
    ("map literals accept computed keys", test_map_literals_accept_computed_keys);
    ("anonymous maps reuse equal shapes", test_anonymous_maps_reuse_equal_shapes);
    ( "anonymous map shape ignores field order",
      test_anonymous_map_shape_ignores_field_order );
    ( "anonymous map operations reuse result shapes",
      test_anonymous_map_operations_reuse_result_shapes );
    ( "module local anonymous maps reuse equal shapes",
      test_module_local_anonymous_maps_reuse_equal_shapes );
    ( "incremental anonymous maps reuse equal shapes",
      test_incremental_anonymous_maps_reuse_equal_shapes );
    ( "heterogeneous record vectors use dynamic values",
      test_heterogeneous_record_vectors_use_dynamic_values );
    ( "declared and anonymous records share dynamic vectors",
      test_declared_and_anonymous_records_share_dynamic_vectors );
    ("println outputs record values", test_println_outputs_record_values);
    ("println rejects unknown symbols", test_println_rejects_unknown_symbols);
    ( "print and println match Clojure output",
      test_print_and_println_match_clojure_output );
    ( "core api supports nested calls, maps, and vectors",
      test_core_api_nested_calls_maps_and_vectors );
    ("core api supports if and vector ops", test_core_api_if_and_vector_ops);
    ("boolean core api works", test_boolean_core_api);
    ( "not uses static Clojure truthiness",
      test_not_uses_static_clojure_truthiness );
    ( "nil predicates and truthiness use options",
      test_nil_predicates_and_truthiness_use_options );
    ( "if-some and when-some bind option payloads",
      test_if_some_and_when_some_bind_option_payloads );
    ( "nil predicates evaluate arguments once",
      test_nil_predicates_evaluate_arguments_once );
    ( "control flow lifts nilable branches",
      test_control_flow_lifts_nilable_branches );
    ( "cond uses Clojure truthiness and implicit nil",
      test_cond_uses_clojure_truthiness_and_implicit_nil );
    ( "if-let and if-some distinguish false from nil",
      test_if_let_and_if_some_distinguish_false_from_nil );
    ( "nilable vectors lift values and empty vectors are polymorphic",
      test_nilable_vectors_lift_values_and_empty_vectors_are_polymorphic );
    ( "logical forms lift nilable operands",
      test_logical_forms_lift_nilable_operands );
    ( "when bindings return nullable body values",
      test_when_bindings_return_nullable_body_values );
    ( "nil type annotation remains explicitly unsupported",
      test_nil_type_annotation_remains_explicitly_unsupported );
    ("type predicates work", test_type_predicates);
    ( "type predicates reject wrong arity",
      test_type_predicates_reject_wrong_arity );
    ( "instance? supports Clojure collection interfaces",
      test_instance_predicate_supports_clojure_collection_interfaces );
    ( "condp selects first match and evaluates target once",
      test_condp_selects_first_match_and_evaluates_target_once );
    ("subs core api works", test_subs_core_api);
    ("subs rejects non-string sources", test_subs_rejects_non_string_sources);
    ("subs rejects non-int indexes", test_subs_rejects_non_int_indexes);
    ( "type relations are explicit and strict",
      test_type_relations_are_explicit_and_strict );
    ( "dynamic record capabilities resolve unique named records",
      test_dynamic_record_capabilities_resolve_unique_named_records );
    ( "assignability reports the selected semantic rule",
      test_assignability_reports_the_selected_semantic_rule );
    ( "named records use nominal type identity",
      test_named_records_use_nominal_type_identity );
    ( "declared type ids preserve source identity",
      test_declared_type_ids_preserve_source_identity );
    ( "compiler identities are stable and distinct",
      test_compiler_identities_are_stable_and_distinct );
    ( "typed protocol and module registries",
      test_typed_protocol_and_module_registries );
    ( "protocol elaboration populates typed registry",
      test_protocol_elaboration_populates_typed_registry );
    ( "protocol implementation populates typed registry",
      test_protocol_implementation_populates_typed_registry );
    ( "module protocols preserve typed registry state",
      test_module_protocols_preserve_typed_registry_state );
    ( "module elaboration populates typed registry",
      test_module_elaboration_populates_typed_registry );
    ( "module metadata avoids encoded symbol keys",
      test_module_metadata_does_not_use_encoded_symbol_keys );
    ( "protocol metadata avoids encoded symbol keys",
      test_protocol_metadata_does_not_use_encoded_symbol_keys );
    ( "emitted OCaml names reject source collisions",
      test_emitted_ocaml_names_reject_source_collisions );
    ( "module namespace rejects emitted name collisions",
      test_module_namespace_rejects_emitted_name_collisions );
    ( "signature namespace rejects emitted name collisions",
      test_signature_namespace_rejects_emitted_name_collisions );
    ( "type namespace rejects emitted name collisions",
      test_type_namespace_rejects_emitted_name_collisions );
    ( "typed environment respects lexical shadowing",
      test_typed_environment_respects_lexical_shadowing );
    ( "typed environment replaces top-level bindings",
      test_typed_environment_replaces_top_level_bindings );
    ( "compiler phases have explicit boundaries",
      test_compiler_phases_have_explicit_boundaries );
    ( "semantic AST preserves nested types",
      test_semantic_ast_preserves_nested_types );
    ( "source node identity reaches parsetree",
      test_source_node_identity_reaches_parsetree );
    ( "source node identity covers value bindings",
      test_source_node_identity_covers_value_bindings );
    ( "source node identity covers record value bindings",
      test_source_node_identity_covers_record_value_bindings );
    ( "source node identity covers recursive bindings",
      test_source_node_identity_covers_recursive_bindings );
    ( "source node identity covers function parameters",
      test_source_node_identity_covers_function_parameters );
    ( "source node identity covers annotated parameters",
      test_source_node_identity_covers_annotated_parameters );
    ( "source node identity covers destructuring bindings",
      test_source_node_identity_covers_destructuring_bindings );
    ( "source node identity covers let bindings",
      test_source_node_identity_covers_let_bindings );
    ( "source node identity covers let destructuring",
      test_source_node_identity_covers_let_destructuring );
    ( "source node identity covers match bindings",
      test_source_node_identity_covers_match_bindings );
    ( "source node identity covers loop bindings",
      test_source_node_identity_covers_loop_bindings );
    ( "source node identity covers catch bindings",
      test_source_node_identity_covers_catch_bindings );
    ("modules resolve qualified symbols", test_modules_resolve_qualified_symbols);
    ( "modules prevent unqualified symbol collisions",
      test_modules_prevent_unqualified_symbol_collisions );
    ( "top-level require rejects unknown lg namespace",
      test_top_level_require_rejects_unknown_lg_namespace );
    ( "top-level require imports OCaml modules",
      test_top_level_require_imports_ocaml_modules );
    ( "namespace scopes following forms without OCaml modules",
      test_namespace_scopes_following_forms_without_ocaml_modules );
    ( "namespace require aliases local modules across files",
      test_namespace_require_aliases_local_modules_across_files );
    ( "namespace load-only require exposes qualified clojure.string",
      test_namespace_load_only_require_exposes_qualified_clojure_string );
    ( "namespace refer-clojure exclude allows local replacement",
      test_namespace_refer_clojure_exclude_allows_local_replacement );
    ( "namespace refer-clojure exclude hides core binding",
      test_namespace_refer_clojure_exclude_hides_core_binding );
    ( "namespace accepts host import clause",
      test_namespace_accepts_host_import_clause );
    ( "namespace accepts qualified host import symbol",
      test_namespace_accepts_qualified_host_import_symbol );
    ( "namespace drops compile-time-only host import",
      test_namespace_drops_compile_time_only_host_import );
    ( "namespace rejects runtime unknown host import",
      test_namespace_rejects_runtime_unknown_host_import );
    ( "namespace ignores Clojure compiler directives",
      test_namespace_ignores_clojure_compiler_directives );
    ( "host import type hint supports instance methods",
      test_host_import_type_hint_supports_instance_methods );
    ( "System currentTimeMillis compiles for native",
      test_system_current_time_millis_compiles_for_native );
    ( "JavaScript targets compile Date and radix interop",
      test_javascript_targets_compile_date_and_radix_interop );
    ( "transient collection operations preserve values",
      test_transient_collection_operations_preserve_values );
    ( "transient operations are first-class functions",
      test_transient_operations_are_first_class_functions );
    ("assert accepts optional message", test_assert_accepts_optional_message);
    ( "into cat flattens one collection level",
      test_into_cat_flattens_one_collection_level );
    ( "mapv vector zips multiple collections",
      test_mapv_vector_zips_multiple_collections );
    ( "map and mapv accept multiple collections",
      test_map_and_mapv_accept_multiple_collections );
    ( "fnil wraps core conj with default collection",
      test_fnil_wraps_core_conj_with_default_collection );
    ( "top-level definitions accept Clojure metadata",
      test_top_level_definitions_accept_clojure_metadata );
    ( "user macros expand syntax quote and unquote",
      test_user_macros_expand_syntax_quote_and_unquote );
    ( "user macros treat host classes as compile-time values",
      test_user_macros_treat_host_classes_as_compile_time_values );
    ( "user macros track helpers passed as values",
      test_user_macros_track_helpers_passed_as_values );
    ( "user macros receive portable namespace environment",
      test_user_macros_receive_portable_namespace_environment );
    ( "user macros support collection type predicates",
      test_user_macros_support_collection_type_predicates );
    ( "user macros can emit top-level do definitions",
      test_user_macros_can_emit_top_level_do_definitions );
    ( "rand-int uses an exclusive positive bound",
      test_rand_int_uses_exclusive_positive_bound );
    ( "int coerces float and preserves int",
      test_int_coerces_float_and_preserves_int );
    ( "namespace rejects malformed and repeated forms",
      test_namespace_rejects_malformed_and_repeated_forms );
    ( "datascript schema accepts dynamic keyword or string values",
      test_datascript_schema_accepts_dynamic_keyword_or_string_values );
    ( "datascript schema reads regex literals",
      test_datascript_schema_reads_regex_literals );
    ( "re-matches returns Clojure match values",
      test_re_matches_returns_clojure_match_values );
    ( "subs accepts guarded dynamic strings",
      test_subs_accepts_guarded_dynamic_strings );
    ( "datascript schema reads anonymous functions for dynamic contains",
      test_datascript_schema_reads_anonymous_functions_for_dynamic_contains );
    ("ocaml keyword names are munged", test_ocaml_keyword_names_are_munged);
    ( "module aliases replace legacy import aliases",
      test_module_aliases_replace_legacy_import_aliases );
    ("open replaces namespace refer", test_open_replaces_required_refer);
    ("keyword lookup syntax works", test_keyword_lookup_syntax);
    ( "keyword lookup supports typed external OCaml records",
      test_keyword_lookup_supports_typed_external_ocaml_records );
    ( "keyword lookup delegates unknown external fields to OCaml",
      test_keyword_lookup_delegates_unknown_external_fields_to_ocaml );
    ( "keyword lookup rejects non-record types",
      test_keyword_lookup_rejects_non_record_types );
    ("typed empty vectors work", test_typed_empty_vectors);
    ("vector-of rejects malformed types", test_vector_of_rejects_malformed_types);
    ("ocaml module require aliases work", test_ocaml_module_require_aliases);
    ("ocaml module require refer works", test_ocaml_module_require_refer);
    ("typed function parameters work", test_typed_function_parameters);
    ( "unit annotations compile through source backend",
      test_unit_annotations_compile_through_source_backend );
    ( "host-owned OCaml type annotations compile",
      test_host_owned_ocaml_type_annotations_compile );
    ( "generic OCaml calls compile through source backend",
      test_generic_ocaml_calls_compile_through_source_backend );
    ( "generic OCaml calls accept unit return type",
      test_generic_ocaml_calls_accept_unit_return_type );
    ( "generic OCaml calls resolve required module aliases",
      test_generic_ocaml_calls_resolve_required_module_aliases );
    ( "generic OCaml calls resolve required module refers",
      test_generic_ocaml_calls_resolve_required_module_refers );
    ( "generic OCaml calls resolve required module refers in modules",
      test_generic_ocaml_calls_resolve_required_module_refers_in_modules );
    ( "generic OCaml calls resolve required module refers in functors",
      test_generic_ocaml_calls_resolve_required_module_refers_in_functors );
    ( "typed OCaml refers are available in modules",
      test_typed_ocaml_refers_are_available_in_modules );
    ( "typed OCaml refers are available in functors",
      test_typed_ocaml_refers_are_available_in_functors );
    ( "generic OCaml calls resolve opened OCaml modules",
      test_generic_ocaml_calls_resolve_opened_ocaml_modules );
    ( "compile_string runs OCaml typecheck gate for host calls",
      test_compile_string_runs_ocaml_typecheck_gate_for_host_calls );
    ( "OCaml errors include lg source locations",
      test_ocaml_errors_include_lg_source_locations );
    ( "Parsetree items preserve top-level source locations",
      test_parsetree_items_preserve_top_level_source_locations );
    ( "incremental Parsetree preserves chunk source locations",
      test_incremental_parsetree_preserves_chunk_source_locations );
    ( "OCaml errors include nested expression locations",
      test_ocaml_errors_include_nested_expression_locations );
    ( "Parsetree expressions preserve nested source locations",
      test_parsetree_expressions_preserve_nested_source_locations );
    ( "inferred OCaml calls use compiler signatures",
      test_inferred_ocaml_calls_use_compiler_signatures );
    ( "inferred OCaml calls preserve type variable identity",
      test_inferred_ocaml_calls_preserve_type_variable_identity );
    ( "inferred OCaml calls resolve aliases and refers",
      test_inferred_ocaml_calls_resolve_aliases_and_refers );
    ( "inferred OCaml calls reject incompatible arguments",
      test_inferred_ocaml_calls_reject_incompatible_arguments );
    ( "inferred OCaml calls reject unknown values",
      test_inferred_ocaml_calls_reject_unknown_values );
    ( "inferred OCaml calls support required labels",
      test_inferred_ocaml_calls_support_required_labels );
    ( "inferred OCaml calls support optional labels",
      test_inferred_ocaml_calls_support_optional_labels );
    ( "inferred OCaml calls preserve partial labelled functions",
      test_inferred_ocaml_calls_preserve_partial_labelled_functions );
    ( "inferred OCaml calls support labels through aliases",
      test_inferred_ocaml_calls_support_labels_through_aliases );
    ( "inferred OCaml calls reject bad labels",
      test_inferred_ocaml_calls_reject_bad_labels );
    ( "inferred labelled calls delegate value types to OCaml",
      test_inferred_labelled_calls_delegate_value_types_to_ocaml );
    ( "OCaml package requires enable inferred calls",
      test_ocaml_package_requires_enable_inferred_calls );
    ( "OCaml package requires report missing packages",
      test_ocaml_package_requires_report_missing_packages );
    ( "OCaml package requires reject invalid package names",
      test_ocaml_package_requires_reject_invalid_package_names );
    ( "direct OCaml calls use qualified values",
      test_direct_ocaml_calls_use_qualified_values );
    ( "direct OCaml calls use aliases and refers",
      test_direct_ocaml_calls_use_aliases_and_refers );
    ( "direct OCaml calls support labels and optional arguments",
      test_direct_ocaml_calls_support_labels_and_optional_arguments );
    ( "direct OCaml calls use external packages",
      test_direct_ocaml_calls_use_external_packages );
    ( "direct external package constructors are inferred",
      test_direct_external_package_constructors_are_inferred );
    ( "direct external package constructors reject bad arity",
      test_direct_external_package_constructors_reject_bad_arity );
    ( "direct external package constructor payloads are checked by OCaml",
      test_direct_external_package_constructor_payloads_are_checked_by_ocaml );
    ( "direct OCaml calls delegate errors to OCaml",
      test_direct_ocaml_calls_delegate_errors_to_ocaml );
    ( "generic OCaml calls reject bad forms",
      test_generic_ocaml_calls_reject_bad_forms );
    ( "parsetree typecheck gate rejects invalid required module alias calls",
      test_parsetree_typecheck_gate_rejects_invalid_required_module_alias_calls
    );
    ( "parsetree typecheck gate accepts valid host calls",
      test_parsetree_typecheck_gate_accepts_valid_host_calls );
    ( "parsetree typecheck gate rejects invalid host calls",
      test_parsetree_typecheck_gate_rejects_invalid_host_calls );
    ( "parsetree typecheck gate accepts runtime dependencies",
      test_parsetree_typecheck_gate_accepts_runtime_dependencies );
    ( "type aliases compile through source backend",
      test_type_aliases_compile_through_source_backend );
    ( "parameterized type declarations compile",
      test_parameterized_type_declarations_compile );
    ( "parameterized variants instantiate constructor payloads",
      test_parameterized_variants_instantiate_constructor_payloads );
    ( "parameterized records instantiate field types",
      test_parameterized_records_instantiate_field_types );
    ( "recursive record array fields work with array primitives",
      test_recursive_record_array_fields_work_with_array_primitives );
    ( "parameterized types compile inside modules",
      test_parameterized_types_compile_inside_modules );
    ( "parameterized record relationships are checked by OCaml",
      test_parameterized_record_relationships_are_checked_by_ocaml );
    ( "parameterized variant relationships are checked by OCaml",
      test_parameterized_variant_relationships_are_checked_by_ocaml );
    ( "parameterized type declarations reject bad parameters",
      test_parameterized_type_declarations_reject_bad_parameters );
    ( "OCaml-owned branch types are checked by OCaml",
      test_ocaml_owned_branch_types_are_checked_by_ocaml );
    ( "OCaml-owned branch type mismatch is delegated to OCaml",
      test_ocaml_owned_branch_type_mismatch_is_delegated_to_ocaml );
    ( "OCaml option and result constructors compile through source backend",
      test_ocaml_option_and_result_constructors_compile_through_source_backend
    );
    ( "OCaml option and result constructors reject bad arity",
      test_ocaml_option_and_result_constructors_reject_bad_arity );
    ( "direct OCaml option and result constructors compile",
      test_direct_ocaml_option_and_result_constructors_compile );
    ( "direct declared variant constructors compile",
      test_direct_declared_variant_constructors_compile );
    ( "direct OCaml constructors reject bad arity",
      test_direct_ocaml_constructors_reject_bad_arity );
    ( "OCaml option and result patterns compile through source backend",
      test_ocaml_option_and_result_patterns_compile_through_source_backend );
    ( "OCaml option patterns delegate payload typecheck to OCaml",
      test_ocaml_option_patterns_delegate_payload_typecheck_to_ocaml );
    ( "OCaml type application annotations compile through source backend",
      test_ocaml_type_application_annotations_compile_through_source_backend );
    ( "OCaml type application annotations delegate argument mismatch to OCaml",
      test_ocaml_type_application_annotations_delegate_argument_mismatch_to_ocaml
    );
    ( "OCaml type application annotations reject bad forms",
      test_ocaml_type_application_annotations_reject_bad_forms );
    ( "syntax ergonomics: concise host type annotations compile",
      test_concise_host_type_annotations_compile );
    ( "syntax ergonomics: threading and option binding forms compile",
      test_threading_and_option_binding_forms_compile );
    ( "syntax ergonomics: combined host package import compiles",
      test_combined_host_package_import_compiles );
    ( "OCaml tuple values compile through source backend",
      test_ocaml_tuple_values_compile_through_source_backend );
    ( "OCaml tuple values delegate argument mismatch to OCaml",
      test_ocaml_tuple_values_delegate_argument_mismatch_to_ocaml );
    ( "OCaml tuple values reject bad forms",
      test_ocaml_tuple_values_reject_bad_forms );
    ( "syntax convergence: concise tuple values and patterns compile",
      test_concise_tuple_values_and_patterns_compile );
    ( "OCaml float and char literals compile",
      test_ocaml_float_and_char_literals_compile );
    ( "double converts ints and preserves floats",
      test_double_converts_ints_and_preserves_floats );
    ( "OCaml arrays support construction read and mutation",
      test_ocaml_arrays_support_construction_read_and_mutation );
    ( "OCaml array primitives support polymorphic helpers",
      test_ocaml_array_primitives_support_polymorphic_helpers );
    ( "OCaml refs support read and assignment",
      test_ocaml_refs_support_read_and_assignment );
    ("concise standard type annotations", test_concise_standard_type_annotations);
    ( "volatile nil uses contextual option reference type",
      test_volatile_nil_uses_contextual_option_reference_type );
    ( "local volatile nil infers value from reset",
      test_local_volatile_nil_infers_value_from_reset );
    ( "OCaml arrays reject invalid operations",
      test_ocaml_arrays_reject_invalid_operations );
    ( "OCaml refs reject invalid operations",
      test_ocaml_refs_reject_invalid_operations );
    ( "syntax convergence: float arithmetic rejects mixed numeric types",
      test_float_arithmetic_rejects_mixed_numeric_types );
    ( "syntax convergence: float arithmetic uses core numeric operators",
      test_float_arithmetic_uses_core_numeric_operators );
    ( "syntax convergence: float arithmetic uses types from option patterns",
      test_float_arithmetic_uses_types_from_option_patterns );
    ( "numeric coherence: float core operations agree",
      test_float_numeric_core_is_coherent );
    ( "numeric coherence: float sets support scalar and collection elements",
      test_float_sets_support_scalar_and_collection_elements );
    ( "numeric coherence: invalid float mixes are rejected",
      test_float_numeric_core_rejects_invalid_mixes );
    ( "OCaml record values compile through source backend",
      test_ocaml_record_values_compile_through_source_backend );
    ( "OCaml record values support qualified module types",
      test_ocaml_record_values_support_qualified_module_types );
    ( "OCaml record values support module alias types",
      test_ocaml_record_values_support_module_alias_types );
    ( "OCaml record values support opened module types",
      test_ocaml_record_values_support_opened_module_types );
    ( "OCaml record values support opened module types in module body",
      test_ocaml_record_values_support_opened_module_types_in_module_body );
    ( "OCaml record values support included module types",
      test_ocaml_record_values_support_included_module_types );
    ( "OCaml record values delegate qualified field typecheck to OCaml",
      test_ocaml_record_values_delegate_qualified_field_typecheck_to_ocaml );
    ( "OCaml record values delegate field typecheck to OCaml",
      test_ocaml_record_values_delegate_field_typecheck_to_ocaml );
    ( "OCaml record values reject bad forms",
      test_ocaml_record_values_reject_bad_forms );
    ( "OCaml field delegates opaque record access to OCaml",
      test_ocaml_field_delegates_opaque_record_access_to_ocaml );
    ( "OCaml variants compile through source backend",
      test_ocaml_variants_compile_through_source_backend );
    ( "OCaml payload variants compile through source backend",
      test_ocaml_payload_variants_compile_through_source_backend );
    ( "OCaml payload variants delegate payload typecheck to OCaml",
      test_ocaml_payload_variants_delegate_payload_typecheck_to_ocaml );
    ( "OCaml variant constructors reject bad arity",
      test_ocaml_variant_constructors_reject_bad_arity );
    ( "OCaml variants reject bad declarations",
      test_ocaml_variants_reject_bad_declarations );
    ( "recursive variants support nested data values",
      test_recursive_variants_support_nested_data_values );
    ( "module recursive variants export constructors",
      test_module_recursive_variants_export_constructors );
    ( "defonce supports top-level and module values",
      test_defonce_supports_top_level_and_module_values );
    ( "defonce rejects invalid declarations",
      test_defonce_rejects_invalid_declarations );
    ( "DataScript schema constants behavior",
      test_datascript_schema_constants_behavior );
    ( "typed function parameters reject bad calls",
      test_typed_function_parameters_reject_bad_calls );
    ( "unit annotations reject non-unit arguments",
      test_unit_annotations_reject_non_unit_arguments );
    ( "typed function parameters reject bad bodies",
      test_typed_function_parameters_reject_bad_bodies );
    ( "typed recursive functions", test_typed_recursive_functions );
    ( "typed recursive functions validate signatures",
      test_typed_recursive_functions_require_valid_signatures );
    ( "multi-arity defn dispatches fixed arities",
      test_multi_arity_defn_dispatches_fixed_arities );
    ( "defn accepts docstring before arities",
      test_defn_accepts_docstring_before_arities );
    ( "multi-arity defn dispatches variadic fallback",
      test_multi_arity_defn_dispatches_variadic_fallback );
    ( "multi-arity defn supports cross-arity calls and recur",
      test_multi_arity_defn_supports_cross_arity_calls_and_recur );
    ( "multi-arity defn remains callable as a value",
      test_multi_arity_defn_remains_callable_as_a_value );
    ("modules export multi-arity defn", test_modules_export_multi_arity_defn);
    ( "multi-arity defn rejects invalid declarations",
      test_multi_arity_defn_rejects_invalid_declarations );
    ( "multi-arity defn rejects unsupported calls",
      test_multi_arity_defn_rejects_unsupported_calls );
    ( "private defn supports single and typed recursive arities",
      test_private_defn_supports_single_and_typed_recursive_arities );
    ( "private defn supports variadic and multi-arity recur",
      test_private_defn_supports_variadic_and_multi_arity_recur );
    ( "module private defn is internal only",
      test_module_private_defn_is_internal_only );
    ( "private defn rejects invalid declarations",
      test_private_defn_rejects_invalid_declarations );
    ( "unannotated function parameters infer from body",
      test_unannotated_function_parameters_infer_from_body );
    ( "identity function is polymorphic at call sites",
      test_identity_function_is_polymorphic_at_call_sites );
    ( "let-bound identity function is polymorphic at call sites",
      test_let_bound_identity_function_is_polymorphic_at_call_sites );
    ( "conditional function is polymorphic at call sites",
      test_conditional_function_is_polymorphic_at_call_sites );
    ( "conditional function type relationship is checked by OCaml",
      test_conditional_function_type_relationship_is_checked_by_ocaml );
    ( "unannotated function parameters reject bad int calls",
      test_unannotated_function_parameters_reject_bad_int_calls );
    ( "unannotated function parameters use Clojure truthiness",
      test_unannotated_function_parameters_use_clojure_truthiness );
    ( "unannotated function parameters infer structural map fields",
      test_unannotated_function_parameters_infer_structural_map_fields );
    ( "contextual parameter inference preserves nested float assoc values",
      test_contextual_parameter_inference_preserves_nested_float_assoc_values );
    ( "top-level defs project function-returned structural records once",
      test_top_level_defs_project_function_returned_structural_records_once );
    ( "module defs project function-returned structural records",
      test_module_defs_project_function_returned_structural_records );
    ( "unannotated function parameters reject missing structural map fields",
      test_unannotated_function_parameters_reject_missing_structural_map_fields
    );
    ( "static protocols dispatch by receiver type",
      test_static_protocols_dispatch_by_receiver_type );
    ( "satisfies? checks static receivers",
      test_satisfies_question_checks_static_receivers );
    ( "satisfies? carries a generic protocol witness",
      test_satisfies_question_carries_a_generic_protocol_witness );
    ( "satisfies? guards generic protocol dispatch",
      test_satisfies_question_guards_generic_protocol_dispatch );
    ( "generic protocol witness supports multiple methods",
      test_generic_protocol_witness_supports_multiple_methods );
    ( "generic protocol witness evaluates receiver once",
      test_generic_protocol_witness_evaluates_receiver_once );
    ( "generic protocol witness flows through sequence callbacks",
      test_generic_protocol_witness_flows_through_sequence_callbacks );
    ( "generic protocol witness supports parser-style recursion",
      test_generic_protocol_witness_supports_parser_style_recursion );
    ( "generic protocol witness carries callbacks through recursion",
      test_generic_protocol_witness_carries_callbacks_through_recursion );
    ( "dynamic recursive maps support assoc",
      test_dynamic_recursive_maps_support_assoc );
    ( "equality infers comparator return type",
      test_equality_infers_comparator_return_type );
    ( "dynamic arrays recover generic elements",
      test_dynamic_arrays_recover_generic_elements );
    ( "loop nil initial value can become optional",
      test_loop_nil_initial_value_can_become_optional );
    ( "loop nil initial value accepts nullable function returns",
      test_loop_nil_initial_value_accepts_nullable_function_returns );
    ( "forward declared deftype fields keep nominal receiver",
      test_forward_declared_deftype_fields_keep_nominal_receiver );
    ( "deftype fields accept Clojure primitive hints",
      test_deftype_fields_accept_clojure_primitive_hints );
    ( "deftype unhinted fields preserve dynamic values",
      test_deftype_unhinted_fields_preserve_dynamic_values );
    ( "deftype mutable fields support set!",
      test_deftype_mutable_fields_support_set_bang );
    ( "deftype methods support instance call syntax",
      test_deftype_methods_support_instance_call_syntax );
    ( "defrecord fields infer host records from protocol methods",
      test_defrecord_fields_infer_host_records_from_protocol_methods );
    ( "defrecord fields preserve protocol capabilities",
      test_defrecord_fields_preserve_protocol_capabilities );
    ( "defrecord protocol methods support forward calls",
      test_defrecord_protocol_methods_support_forward_calls );
    ( "defrecord host interfaces support overloaded methods",
      test_defrecord_host_interfaces_support_overloaded_methods );
    ( "expression type hints narrow dynamic records",
      test_expression_type_hints_narrow_dynamic_records );
    ( "opaque records project safe fields without casts",
      test_opaque_records_project_safe_fields_without_casts );
    ( "instance? recognizes namespaced protocol interfaces",
      test_instance_question_recognizes_namespaced_protocol_interfaces );
    ( "generic collection returns preserve concrete element types",
      test_generic_collection_returns_preserve_concrete_element_types );
    ( "Java exception constructors map to runtime exceptions",
      test_java_exception_constructors_map_to_runtime_exceptions );
    ( "Java exception constructors support empty messages",
      test_java_exception_constructors_support_empty_messages );
    ( "print-method defmethod writes custom record representations",
      test_print_method_defmethod_writes_custom_record_representations );
    ( "Java Writer annotations work in ordinary functions",
      test_java_writer_annotations_work_in_ordinary_functions );
    ( "Clojure Sorted annotations expose dynamic comparators",
      test_clojure_sorted_annotations_expose_dynamic_comparators );
    ( "defn accepts attribute maps and return hints",
      test_defn_accepts_attribute_maps_and_return_hints );
    ( "compare supports dynamic scalar values",
      test_compare_supports_dynamic_scalar_values );
    ( "class and identical? support dynamic values",
      test_class_and_identical_support_dynamic_values );
    ( "Clojure static dot calls support hasheq",
      test_clojure_static_dot_calls_support_hasheq );
    ( "Clojure Number and Comparable interop",
      test_clojure_number_and_comparable_interop );
    ( "Clojure equals interop supports dynamic values",
      test_clojure_equals_interop_supports_dynamic_values );
    ( "identical? and .equals infer heterogeneous parameters",
      test_identical_and_equals_infer_heterogeneous_parameters );
    ( "try supports Clojure exception type bindings",
      test_try_supports_clojure_exception_type_bindings );
    ("macros can clear form metadata", test_macros_can_clear_form_metadata);
    ( "macros can apply functions to argument sequences",
      test_macros_can_apply_functions_to_argument_sequences );
    ( "cond contextualizes anonymous function branches",
      test_cond_contextualizes_anonymous_function_branches );
    ( "callable expressions are evaluated once",
      test_callable_expressions_are_evaluated_once );
    ( "extend-type supports multiple protocol groups",
      test_extend_type_supports_multiple_protocol_groups );
    ( "Clojure MapEntry compiles as a two-element vector",
      test_clojure_map_entry_compiles_as_two_element_vector );
    ( "set literals are callable as membership lookup",
      test_set_literals_are_callable_as_membership_lookup );
    ( "static sets accept dynamic lookup values",
      test_static_sets_accept_dynamic_lookup_values );
    ( "generic protocol witness compiles for JavaScript targets",
      test_generic_protocol_witness_compiles_for_javascript_targets );
    ( "protocols support float and symbol receivers",
      test_protocols_support_float_and_symbol_receivers );
    ( "protocols support generic host constructor receivers",
      test_protocols_support_generic_host_constructor_receivers );
    ( "protocols support external OCaml receivers",
      test_protocols_support_external_ocaml_receivers );
    ( "protocols reject duplicate host constructor implementations",
      test_protocols_reject_duplicate_host_constructor_implementations );
    ( "static protocols reject missing implementations",
      test_static_protocols_reject_missing_implementation );
    ( "static protocols reject return type mismatch",
      test_static_protocols_reject_return_type_mismatch );
    ( "static protocols work through module aliases",
      test_static_protocols_work_through_module_aliases );
    ( "protocols work through chained module aliases",
      test_protocols_work_through_chained_module_aliases );
    ( "protocols work through module-local aliases",
      test_protocols_work_through_module_local_aliases );
    ( "ambiguous protocol methods require explicit identity",
      test_ambiguous_protocol_methods_require_explicit_identity );
    ( "protocols inside modules export methods and record implementations",
      test_protocols_inside_modules_export_methods_and_record_impls );
    ( "protocols reject duplicate method declarations",
      test_protocols_reject_duplicate_method_declarations );
    ( "protocols reject duplicate implementations",
      test_protocols_reject_duplicate_implementations );
    ( "protocol implementations reject emitted name collisions",
      test_protocol_implementations_reject_emitted_name_collisions );
    ( "protocols reject duplicate methods in one extension",
      test_protocols_reject_duplicate_methods_in_one_extension );
    ( "protocols support named record receivers",
      test_protocols_support_named_record_receivers );
    ( "named record updates preserve protocol identity",
      test_named_record_updates_preserve_protocol_identity );
    ( "syntax convergence: keyword access reads nominal record fields",
      test_keyword_access_reads_nominal_record_fields );
    ( "syntax convergence: concise external type paths defer to OCaml",
      test_concise_external_type_paths_defer_to_ocaml );
    ( "named record parameters are inferred for record updates",
      test_named_record_parameters_are_inferred_for_record_updates );
    ( "module-local named record parameters are inferred",
      test_module_local_named_record_parameters_are_inferred );
    ( "protocol signatures check all parameter types",
      test_protocol_signatures_check_all_parameter_types );
    ( "protocol identity disambiguates same named methods",
      test_protocol_identity_disambiguates_same_named_methods );
    ("do and multi-form bodies work", test_do_and_multi_form_bodies);
    ("fn empty body returns nil", test_fn_empty_body_returns_nil);
    ( "vectors support mixed element types",
      test_vectors_support_mixed_element_types );
    ("keyword values print as keywords", test_keyword_values_print_as_keywords);
    ("keys return keyword values", test_keys_return_keyword_values);
    ("vals return homogeneous values", test_vals_return_homogeneous_values);
    ("vals accept dynamic maps", test_vals_accept_dynamic_maps);
    ("vals rejects heterogeneous values", test_vals_rejects_heterogeneous_values);
    ( "vectors support mixed keyword and string elements",
      test_vectors_support_mixed_keyword_and_string_elements );
    ( "arithmetic rejects non-int arguments",
      test_arithmetic_rejects_non_int_arguments );
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
    ( "assoc rejects vector non-int indexes",
      test_assoc_rejects_vector_non_int_indexes );
    ("dissoc supports multiple keys", test_dissoc_supports_multiple_keys);
    ( "map merge, update, and select-keys work",
      test_map_merge_update_and_select_keys );
    ( "merge rejects incompatible overlapping fields",
      test_merge_rejects_incompatible_overlapping_fields );
    ("update rejects type changes", test_update_rejects_type_changes);
    ("update supports extra arguments", test_update_supports_extra_arguments);
    ( "keyword let bindings preserve static map access",
      test_keyword_let_bindings_preserve_static_map_access );
    ( "assoc packs values for dynamic record fields",
      test_assoc_packs_values_for_dynamic_record_fields );
    ( "assoc accepts protocol constrained named records",
      test_assoc_accepts_protocol_constrained_named_records );
    ( "nested named records resolve protocol receivers",
      test_nested_named_records_resolve_protocol_receivers );
    ( "defrecord field hints reject unknown record types",
      test_defrecord_field_hints_reject_unknown_record_types );
    ( "defrecord preserves extension map entries",
      test_defrecord_preserves_extension_map_entries );
    ( "update preserves named records with opaque fields",
      test_update_preserves_named_records_with_opaque_fields );
    ("assoc-in updates nested maps", test_assoc_in_updates_nested_maps);
    ( "assoc-in preserves named records with references",
      test_assoc_in_preserves_named_records_with_references );
    ( "threaded forms accumulate record fields",
      test_threaded_forms_accumulate_record_fields );
    ( "update infers record fields from updater functions",
      test_update_infers_record_fields_from_updater_functions );
    ( "update works as a nested map updater",
      test_update_works_as_a_nested_map_updater );
    ( "update works as a nested vector updater",
      test_update_works_as_a_nested_vector_updater );
    ( "nested update passes all extra arguments",
      test_nested_update_passes_all_extra_arguments );
    ( "update rejects extra argument type mismatch",
      test_update_rejects_extra_argument_type_mismatch );
    ("update supports vector indexes", test_update_supports_vector_indexes);
    ( "update rejects vector index type mismatch",
      test_update_rejects_vector_index_type_mismatch );
    ( "select-keys rejects unknown fields",
      test_select_keys_rejects_unknown_fields );
    ("contains supports vector indexes", test_contains_supports_vector_indexes);
    ( "contains rejects vector non-int indexes",
      test_contains_rejects_vector_non_int_indexes );
    ("if supports mixed branch types", test_if_supports_mixed_branch_types);
    ("conditional forms work", test_conditional_forms_work);
    ( "if-not supports mixed branch types",
      test_if_not_supports_mixed_branch_types );
    ("cond returns nil without else", test_cond_returns_nil_without_else);
    ("cond supports mixed branch types", test_cond_supports_mixed_branch_types);
    ("cond accepts Clojure truthy tests", test_cond_accepts_clojure_truthy_tests);
    ("when returns nullable value", test_when_returns_nullable_value);
    ("when-not negates the condition", test_when_not_negates_the_condition);
    ( "conditional forms accept truthy params",
      test_conditional_forms_accept_truthy_params );
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
    ( "hash-combine matches Clojure 32-bit overflow",
      test_hash_combine_matches_clojure_32_bit_overflow );
    ( "hash matches Clojure scalar and collection values",
      test_hash_matches_clojure_scalar_and_collection_values );
    ( "numeric == supports mixed numbers",
      test_numeric_double_equals_supports_mixed_numbers );
    ( "case supports dynamic keyword and string targets",
      test_case_supports_dynamic_keyword_and_string_targets );
    ( "batched numeric/scalar core functions reject unchecked arity",
      test_batched_numeric_scalar_core_functions_reject_unchecked_arity );
    ( "batched numeric/scalar core functions reject bad name arg",
      test_batched_numeric_scalar_core_functions_reject_bad_name_arg );
    ( "batched numeric/scalar core functions infer int params",
      test_batched_numeric_scalar_core_functions_infer_int_params );
    ("clojure.string module batch works", test_clojure_string_module_batch_works);
    ("clojure.string module refer works", test_clojure_string_module_refer_works);
    ( "clojure.string module rejects bad args",
      test_clojure_string_module_rejects_bad_args );
    ( "clojure.string module rejects unknown refer",
      test_clojure_string_module_rejects_unknown_refer );
    ( "clojure.walk preserves collections and traversal order",
      test_clojure_walk_preserves_collections_and_traversal_order );
    ( "clojure.data diff matches recursive collection semantics",
      test_clojure_data_diff_matches_recursive_collection_semantics );
    ( "batched predicate/collection core functions work",
      test_batched_predicate_collection_core_functions_work );
    ( "batched predicate/collection core functions reject bad counts",
      test_batched_predicate_collection_core_functions_reject_bad_counts );
    ( "batched predicate/collection core functions reject bad predicates",
      test_batched_predicate_collection_core_functions_reject_bad_predicates );
    ( "batched predicate/collection core functions reject bad run function",
      test_batched_predicate_collection_core_functions_reject_bad_run_function
    );
    ("doseq infers seqable parameters", test_doseq_infers_seqable_parameters);
    ("for supports when clauses", test_for_supports_when_clauses);
    ( "merge accepts dynamic map parameters",
      test_merge_accepts_dynamic_map_parameters );
    ( "record arguments fill missing optional fields",
      test_record_arguments_fill_missing_optional_fields );
    ( "reify preserves protocols across dynamic fields",
      test_reify_preserves_protocols_across_dynamic_fields );
    ( "parameters preserve multiple protocol constraints",
      test_parameters_preserve_multiple_protocol_constraints );
    ( "references preserve state across dynamic fields",
      test_references_preserve_state_across_dynamic_fields );
    ( "truthy guards preserve dynamic numeric parameters",
      test_truthy_guards_preserve_dynamic_numeric_parameters );
    ( "let aliases propagate seqable constraints",
      test_let_aliases_propagate_seqable_constraints );
    ( "nested drop-while infers seqable parameters",
      test_nested_drop_while_infers_seqable_parameters );
    ( "loop initializers propagate seqable constraints",
      test_loop_initializers_propagate_seqable_constraints );
    ( "batched predicate/collection core functions accept truthy params",
      test_batched_predicate_collection_core_functions_accept_truthy_params );
    ( "batched identifier/constructor core functions work",
      test_batched_identifier_and_constructor_core_functions_work );
    ( "batched identifier/constructor core functions reject bad symbol args",
      test_batched_identifier_and_constructor_core_functions_reject_bad_symbol_args
    );
    ( "batched identifier/constructor core functions reject bad keyword args",
      test_batched_identifier_and_constructor_core_functions_reject_bad_keyword_args
    );
    ( "batched identifier/constructor core functions reject bad namespace args",
      test_batched_identifier_and_constructor_core_functions_reject_bad_namespace_args
    );
    ( "namespace accepts guarded dynamic identifiers",
      test_namespace_accepts_guarded_dynamic_identifiers );
    ( "batched identifier/constructor core functions reject bad list* tail",
      test_batched_identifier_and_constructor_core_functions_reject_bad_list_star_tail
    );
    ("batched sequence functions work", test_batched_sequence_functions_work);
    ("sort accepts dynamic collections", test_sort_accepts_dynamic_collections);
    ( "metadata map values constrain function parameters",
      test_metadata_map_values_constrain_function_parameters );
    ( "logical or preserves nullable dynamic results",
      test_logical_or_preserves_nullable_dynamic_results );
    ("match coerces nullable branches", test_match_coerces_nullable_branches);
    ( "lazy map defers incrementally and memoizes realized values",
      test_lazy_map_defers_incrementally_and_memoizes_realized_values );
    ( "lazy filter realizes only enough source values",
      test_lazy_filter_realizes_only_enough_source_values );
    ( "lazy take bounds infinite range and repeat",
      test_lazy_take_bounds_infinite_range_and_repeat );
    ( "lazy map accepts all builtin seqable types",
      test_lazy_map_accepts_all_builtin_seqable_types );
    ( "OCaml Seq unfold builds typed lazy sequences",
      test_ocaml_seq_unfold_builds_typed_lazy_sequences );
    ( "OCaml array sequences flat-map lazily",
      test_ocaml_array_sequences_flat_map_lazily );
    ( "OCaml uncurried call runs fixed-arity callbacks",
      test_ocaml_uncurried_call_emits_melange_direct_application );
    ( "reduce accepts all builtin seqable types",
      test_reduce_accepts_all_builtin_seqable_types );
    ("reduce realizes lazy seq once", test_reduce_realizes_lazy_seq_once);
    ( "reduced values support predicates and unwrapping",
      test_reduced_values_support_predicates_and_unwrapping );
    ( "reduce stops without realizing remaining values",
      test_reduce_stops_without_realizing_remaining_values );
    ( "nil initialized reduce returns nullable reduced value",
      test_nil_initialized_reduce_returns_nullable_reduced_value );
    ( "reduce short-circuits builtin and custom Seqable types",
      test_reduce_short_circuits_builtin_and_custom_seqable_types );
    ( "custom records can implement core Seqable",
      test_custom_records_can_implement_core_seqable );
    ( "modules export core Seqable implementations",
      test_modules_export_core_seqable_implementations );
    ( "reduce prefers custom Reducible over Seqable",
      test_reduce_prefers_custom_reducible_over_seqable );
    ( "reduce specializes builtin Reducible types",
      test_reduce_specializes_builtin_reducible_types );
    ( "reduce infers destructured items when collection is generic",
      test_reduce_infers_destructured_items_when_collection_is_generic );
    ( "count prefers custom Counted over Seqable",
      test_count_prefers_custom_counted_over_seqable );
    ( "first and last accept all Seqable types",
      test_first_and_last_accept_all_seqable_types );
    ( "custom records can implement core Indexed",
      test_custom_records_can_implement_core_indexed );
    ( "nth accepts Indexed and Seqable host types",
      test_nth_accepts_indexed_and_seqable_host_types );
    ( "generic sequence functions infer Seqable dictionaries",
      test_generic_sequence_functions_infer_seqable_dictionaries );
    ( "generic Seqable returns instantiate element types",
      test_generic_seqable_returns_instantiate_element_types );
    ( "Seqable dictionary arguments evaluate once",
      test_seqable_dictionary_arguments_evaluate_once );
    ( "modules export host OCaml Seqable implementations",
      test_modules_export_host_ocaml_seqable_implementations );
    ( "Logseq Datascript style wrappers use collection capabilities",
      test_logseq_datascript_style_wrappers_use_collection_capabilities );
    ( "sequence navigation accepts all Seqable types",
      test_sequence_navigation_accepts_all_seqable_types );
    ( "generic sequence navigation infers Seqable dictionaries",
      test_generic_sequence_navigation_infers_seqable_dictionaries );
    ( "sequence navigation handles empty Seqable values",
      test_sequence_navigation_handles_empty_seqable_values );
    ( "generic sequence navigation evaluates arguments once",
      test_generic_sequence_navigation_evaluates_arguments_once );
    ( "batched sequence functions reject type mismatch",
      test_batched_sequence_functions_reject_type_mismatch );
    ( "batched sequence functions reject bad functions",
      test_batched_sequence_functions_reject_bad_functions );
    ( "batched sequence functions reject bad counts",
      test_batched_sequence_functions_reject_bad_counts );
    ( "batched sequence functions reject bad partition size",
      test_batched_sequence_functions_reject_bad_partition_size );
    ( "batched sequence functions reject reduce-kv non-collection",
      test_batched_sequence_functions_reject_reduce_kv_non_collection );
    ( "interleave accepts multiple collections",
      test_interleave_accepts_multiple_collections );
    ( "interleave rejects later type mismatches",
      test_interleave_rejects_later_type_mismatches );
    ( "interleave requires two collections",
      test_interleave_requires_two_collections );
    ("additional sequence helpers work", test_additional_sequence_helpers_work);
    ( "additional sequence helpers reject bad counts",
      test_additional_sequence_helpers_reject_bad_counts );
    ( "some returns first truthy predicate value",
      test_some_returns_first_truthy_predicate_value );
    ( "Clojure truthiness works in conditions",
      test_clojure_truthiness_in_conditions );
    ( "and/or return values and short-circuit",
      test_and_or_return_values_and_short_circuit );
    ( "and/or single values are unchanged",
      test_and_or_single_values_are_unchanged );
    ( "additional sequence helpers reject bad reductions arity",
      test_additional_sequence_helpers_reject_bad_reductions_arity );
    ("let, defn, and fn values work", test_let_defn_and_fn_values);
    ("loop and recur are tail-recursive", test_loop_and_recur_are_tail_recursive);
    ( "loop/recur remains tail through let and cond",
      test_loop_recur_remains_tail_through_let_and_cond );
    ( "loop and recur delegate OCaml-owned alias compatibility",
      test_loop_and_recur_delegate_ocaml_owned_alias_compatibility );
    ( "loop and recur delegate OCaml-owned mismatch to OCaml",
      test_loop_and_recur_delegate_ocaml_owned_mismatch_to_ocaml );
    ( "loop and recur reject invalid calls",
      test_loop_and_recur_reject_invalid_calls );
    ( "destructuring works in let and functions",
      test_destructuring_in_let_and_functions );
    ( "destructuring supports direct keyword bindings",
      test_destructuring_supports_direct_keyword_bindings );
    ( "destructuring supports rest and defaults",
      test_destructuring_supports_rest_and_defaults );
    ( "let destructuring accepts generic seqable values",
      test_let_destructuring_accepts_generic_seqable_values );
    ( "macro slots preserve dynamic seqable values",
      test_macro_slots_preserve_dynamic_seqable_values );
    ( "destructuring preserves row polymorphic function calls",
      test_destructuring_preserves_row_polymorphic_function_calls );
    ( "map destructuring as preserves open map access",
      test_map_destructuring_as_preserves_open_map_access );
    ( "row polymorphic functions accept different map shapes",
      test_row_polymorphic_functions_accept_different_map_shapes );
    ( "row types bind nested capability parameters",
      test_row_types_bind_nested_capability_parameters );
    ( "destructuring rejects missing map fields",
      test_destructuring_rejects_missing_map_fields );
    ( "let destructuring supports nested sequences",
      test_let_destructuring_supports_nested_sequences );
    ( "let bindings support value type hints",
      test_let_bindings_support_value_type_hints );
    ( "nested record fields preserve outer record inference",
      test_nested_record_fields_preserve_outer_record_inference );
    ( "threaded keyword access preserves nested record inference",
      test_threaded_keyword_access_preserves_nested_record_inference );
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
    ( "apply distinct accepts generic seqable values",
      test_apply_distinct_accepts_generic_seqable_values );
    ( "apply calls overloaded functions with dynamic arguments",
      test_apply_calls_overloaded_functions_with_dynamic_arguments );
    ("apply rejects bad set reducers", test_apply_rejects_bad_set_reducers);
    ("set core api works", test_set_core_api);
    ("sets support named records", test_sets_support_named_records);
    ( "sets support primitive lists and vectors",
      test_sets_support_primitive_lists_and_vectors );
    ( "sets support nested composite elements",
      test_sets_support_nested_composite_elements );
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
    ( "take and drop reject non-int counts",
      test_take_and_drop_reject_non_int_counts );
    ("take and drop support sets", test_take_and_drop_support_sets);
    ("reverse core api works", test_reverse_core_api);
    ( "reverse rejects unsupported collections",
      test_reverse_rejects_unsupported_collections );
    ("sequence boolean predicates work", test_sequence_boolean_predicates);
    ( "sequence boolean predicates reject non-bool predicates",
      test_sequence_boolean_predicates_reject_non_bool_predicates );
    ("empty core api works", test_empty_core_api);
    ("empty rejects unsupported values", test_empty_rejects_unsupported_values);
    ("into core api works", test_into_core_api);
    ( "into accepts inferred seqable parameters",
      test_into_accepts_inferred_seqable_parameters );
    ( "Eduction applies map filter and cat transducers",
      test_eduction_applies_map_filter_and_cat_transducers );
    ( "into applies composed transducers",
      test_into_applies_composed_transducers );
    ( "into transducers build dynamic sets",
      test_into_transducers_build_dynamic_sets );
    ( "filter accepts dynamic callable record fields",
      test_filter_accepts_dynamic_callable_record_fields );
    ( "sequence operations accept host optional collections",
      test_sequence_operations_accept_host_optional_collections );
    ( "reduce infers seqable record fields",
      test_reduce_infers_seqable_record_fields );
    ( "thread macros accept keyword steps",
      test_thread_macros_accept_keyword_steps );
    ( "cond thread macros apply selected steps",
      test_cond_thread_macros_apply_selected_steps );
    ( "update accepts first-class assoc on dynamic maps",
      test_update_accepts_first_class_assoc_on_dynamic_maps );
    ( "assoc infers dynamic maps for variable keys",
      test_assoc_infers_dynamic_maps_for_variable_keys );
    ( "assoc accepts dynamic collection boundaries",
      test_assoc_accepts_dynamic_collection_boundaries );
    ( "assoc accepts nullable dynamic maps",
      test_assoc_accepts_nullable_dynamic_maps );
    ("reduce-kv accepts dynamic maps", test_reduce_kv_accepts_dynamic_maps);
    ( "map value parameters support guarded sequence use",
      test_map_value_parameters_support_guarded_sequence_use );
    ("reduce accepts open map entries", test_reduce_accepts_open_map_entries);
    ( "update refines empty nested vector elements",
      test_update_refines_empty_nested_vector_elements );
    ( "apply accepts concat as a core function",
      test_apply_accepts_concat_as_a_core_function );
    ( "reduce preserves refined vector element types",
      test_reduce_preserves_refined_vector_element_types );
    ( "set literals accept dynamic elements",
      test_set_literals_accept_dynamic_elements );
    ( "contains? infers generic membership for variable keys",
      test_contains_infers_generic_membership_for_variable_keys );
    ( "into rejects element type mismatch",
      test_into_rejects_element_type_mismatch );
    ("typed empty sets work", test_typed_empty_sets);
    ("sets reject nil elements", test_sets_reject_nil_elements);
    ( "set-of rejects types without comparators",
      test_set_of_rejects_types_without_comparators );
    ( "keyword type annotations for empty collections work",
      test_keyword_type_annotations_for_empty_collections );
    ("nth supports default values", test_nth_supports_default_values);
    ("nth rejects default type mismatch", test_nth_rejects_default_type_mismatch);
    ("typed empty lists work", test_typed_empty_lists);
    ( "syntax convergence: empty lists infer type from branch context",
      test_empty_lists_infer_type_from_branch_context );
    ("rest is empty-safe", test_rest_is_empty_safe);
    ("lists support mixed element types", test_lists_support_mixed_element_types);
    ("mixed lists pack optional values", test_mixed_lists_pack_optional_values);
    ("conj rejects list type mismatch", test_conj_rejects_list_type_mismatch);
    ("collection positional helpers work", test_collection_positional_helpers);
    ("subvec core api works", test_subvec_core_api);
    ("subvec rejects non-vector sources", test_subvec_rejects_non_vector_sources);
    ("subvec rejects non-int indexes", test_subvec_rejects_non_int_indexes);
    ( "peek rejects unsupported collections",
      test_peek_rejects_unsupported_collections );
    ("let rejects odd binding forms", test_let_rejects_odd_binding_forms);
    ("map rejects non-function argument", test_map_rejects_non_function_argument);
    ("match expression works", test_match_expression_works);
    ("match supports mixed branch types", test_match_supports_mixed_branch_types);
    ("match rejects bad clause count", test_match_rejects_bad_clause_count);
    ( "match rejects pattern type mismatch",
      test_match_rejects_pattern_type_mismatch );
    ( "match infers target type from patterns",
      test_match_infers_target_type_from_patterns );
    ( "match supports OCaml constructor patterns",
      test_match_supports_ocaml_constructor_patterns );
    ( "compile diagnostics capture OCaml match warnings",
      test_compile_diagnostics_capture_ocaml_match_warnings );
    ( "compile diagnostics are empty for exhaustive matches",
      test_compile_diagnostics_are_empty_for_exhaustive_matches );
    ( "parser diagnostics locate unterminated delimiters",
      test_parser_diagnostics_locate_unterminated_delimiters );
    ( "language service recovers completed prefix",
      test_language_service_recovers_completed_prefix );
    ( "language service hover uses OCaml types",
      test_language_service_hover_uses_ocaml_types );
    ( "language service definition resolves source binding",
      test_language_service_definition_resolves_source_binding );
    ( "language service completion uses source names and types",
      test_language_service_completion_uses_source_names_and_types );
    ( "language service queries outside symbols are empty",
      test_language_service_queries_outside_symbols_are_empty );
    ( "language service signature help uses typed call sites",
      test_language_service_signature_help_uses_typed_call_site );
    ( "language service references use typed identity",
      test_language_service_references_use_typed_identity );
    ( "language service rename returns exact symbol edits",
      test_language_service_rename_returns_exact_symbol_edits );
    ( "language service module capabilities",
      test_language_service_module_capabilities );
    ( "language service protocol capabilities",
      test_language_service_protocol_capabilities );
    ( "language service field capabilities",
      test_language_service_field_capabilities );
    ( "language service semantic hover capabilities",
      test_language_service_semantic_hover_capabilities );
    ( "language service constructor capabilities",
      test_language_service_constructor_capabilities );
    ( "language service type capabilities",
      test_language_service_type_capabilities );
    ( "language service document symbols preserve source names",
      test_language_service_document_symbols_preserve_source_names );
    ( "language service recognizes private defn",
      test_language_service_recognizes_private_defn );
    ( "language service document symbols include semantic children",
      test_language_service_document_symbols_include_semantic_children );
    ( "language service semantic tokens classify symbols",
      test_language_service_semantic_tokens_classify_symbols );
    ( "language service workspace resolves cross-file identity",
      test_language_service_workspace_resolves_cross_file_identity );
    ( "workspace index reanalyzes dependency components",
      test_workspace_index_reanalyzes_only_dependency_component );
    ( "workspace index tracks top-level dependencies",
      test_workspace_index_tracks_top_level_symbol_dependencies );
    ( "workspace index tracks module alias dependencies",
      test_workspace_index_tracks_module_alias_dependencies );
    ( "workspace index tracks variant constructor dependencies",
      test_workspace_index_tracks_variant_constructor_dependencies );
    ( "workspace index ignores lexically bound names",
      test_workspace_index_ignores_lexically_bound_names );
    ( "workspace index tracks qualified type dependencies",
      test_workspace_index_tracks_qualified_type_dependencies );
    ( "workspace index tracks concise type dependencies",
      test_workspace_index_tracks_concise_type_dependencies );
    ( "workspace index tracks declaration type dependencies",
      test_workspace_index_tracks_declaration_type_dependencies );
    ( "workspace index separates module and protocol providers",
      test_workspace_index_separates_module_and_protocol_providers );
    ( "workspace index handles file lifecycle",
      test_workspace_index_handles_file_removal_readd_and_rename );
    ( "workspace index rejects duplicate providers",
      test_workspace_index_rejects_duplicate_providers );
    ( "workspace index contains component errors",
      test_workspace_index_contains_component_errors );
    ( "workspace index records partial component errors",
      test_workspace_index_records_partial_component_errors );
    ( "workspace diagnostics belong to their source file",
      test_workspace_diagnostics_belong_to_their_source_file );
    ("formatter normalizes whitespace", test_formatter_normalizes_whitespace);
    ("formatter wraps long nested forms", test_formatter_wraps_long_nested_forms);
    ( "formatter preserves comments strings and is idempotent",
      test_formatter_preserves_comments_strings_and_is_idempotent );
    ( "formatter rejects unbalanced delimiters",
      test_formatter_rejects_unbalanced_delimiters );
    ( "match delegates opaque module constructor payload patterns to OCaml",
      test_match_delegates_opaque_module_constructor_payload_patterns_to_ocaml
    );
    ( "match delegates unknown opaque constructor errors to OCaml",
      test_match_delegates_unknown_opaque_constructor_errors_to_ocaml );
    ( "match supports record alias or and guard patterns",
      test_match_supports_record_alias_or_and_guard_patterns );
    ( "record patterns reject unknown and duplicate fields",
      test_record_patterns_reject_unknown_and_duplicate_fields );
    ( "record patterns require record targets",
      test_record_patterns_require_record_targets );
    ( "or patterns require the same binders",
      test_or_patterns_require_the_same_binders );
    ("match guards must be boolean", test_match_guards_must_be_boolean);
    ("try catches OCaml exceptions", test_try_catches_ocaml_exceptions);
    ( "try supports normal results multiple body forms and handlers",
      test_try_supports_normal_results_multiple_body_forms_and_handlers );
    ( "try and raise reject malformed forms",
      test_try_and_raise_reject_malformed_forms );
    ("try supports mixed branch types", test_try_supports_mixed_branch_types);
    ("raise payload is checked by OCaml", test_raise_payload_is_checked_by_ocaml);
    ("module definitions work", test_module_definitions_work);
    ( "module definitions reject expressions",
      test_module_definitions_reject_expressions );
    ( "module definitions support type aliases",
      test_module_definitions_support_type_aliases );
    ( "incremental compilation preserves modules",
      test_incremental_compilation_preserves_modules );
    ("open module exposes values", test_open_module_exposes_values);
    ("include module exposes values", test_include_module_exposes_values);
    ( "incremental compilation preserves opened modules",
      test_incremental_compilation_preserves_opened_modules );
    ( "incremental compilation preserves state",
      test_incremental_compilation_preserves_state );
    ( "incremental compilation preserves record sets",
      test_incremental_compilation_preserves_record_sets );
    ( "incremental compilation preserves composite sets",
      test_incremental_compilation_preserves_composite_sets );
    ( "module definitions support record sets",
      test_module_definitions_support_record_sets );
    ( "module definitions support composite sets",
      test_module_definitions_support_composite_sets );
    ( "module definitions support variants",
      test_module_definitions_support_variants );
    ( "incremental compilation requires prior state",
      test_incremental_compilation_requires_prior_state );
    ("module definitions support open", test_module_definitions_support_open);
    ( "module definitions support include",
      test_module_definitions_support_include );
    ( "module definitions support module alias",
      test_module_definitions_support_module_alias );
    ("module alias exposes values", test_module_alias_exposes_values);
    ( "syntax convergence: slash qualification covers members and constructor \
       patterns",
      test_slash_qualification_covers_members_and_constructor_patterns );
    ( "syntax convergence: lowercase host aliases qualify constructor patterns",
      test_lowercase_host_aliases_qualify_constructor_patterns );
    ( "module alias targets nested modules",
      test_module_alias_targets_nested_modules );
    ("module alias rejects bad forms", test_module_alias_rejects_bad_forms);
    ("include module rejects bad forms", test_include_module_rejects_bad_forms);
    ( "module signatures constrain modules",
      test_module_signatures_constrain_modules );
    ( "module signature ascription is checked by OCaml",
      test_module_signature_ascription_is_checked_by_ocaml );
    ( "module signatures support type items",
      test_module_signatures_support_type_items );
    ( "module signatures support parameterized manifest types",
      test_module_signatures_support_parameterized_manifest_types );
    ( "module signatures support parameterized abstract types",
      test_module_signatures_support_parameterized_abstract_types );
    ( "module signatures support nested modules",
      test_module_signatures_support_nested_modules );
    ( "functor parameters expose nested signature modules",
      test_functor_parameters_expose_nested_signature_modules );
    ( "nested module signatures are checked by OCaml",
      test_nested_module_signatures_are_checked_by_ocaml );
    ( "module signatures include other signatures",
      test_module_signatures_include_other_signatures );
    ( "module signature cycles are rejected",
      test_module_signature_cycles_are_rejected );
    ( "functor parameters expose included signature values",
      test_functor_parameters_expose_included_signature_values );
    ( "included module signatures are checked by OCaml",
      test_included_module_signatures_are_checked_by_ocaml );
    ( "unknown signature includes are checked by OCaml",
      test_unknown_signature_includes_are_checked_by_ocaml );
    ( "module signature type items are checked by OCaml",
      test_module_signature_type_items_are_checked_by_ocaml );
    ( "module signatures support abstract type items",
      test_module_signatures_support_abstract_type_items );
    ( "module signature abstract types are checked by OCaml",
      test_module_signature_abstract_types_are_checked_by_ocaml );
    ( "module signatures reject bad forms",
      test_module_signatures_reject_bad_forms );
    ("module functors apply modules", test_module_functors_apply_modules);
    ( "module functors apply multiple modules",
      test_module_functors_apply_multiple_modules );
    ( "module functor applications expose record types",
      test_module_functor_applications_expose_record_types );
    ( "module functor applications expose protocols",
      test_module_functor_applications_expose_protocols );
    ( "module variants export constructors",
      test_module_variants_export_constructors );
    ( "module functor applications expose variant constructors",
      test_module_functor_applications_expose_variant_constructors );
    ( "module functor applications preserve record protocol identity",
      test_module_functor_applications_preserve_record_protocol_identity );
    ( "module functor applications register applied types",
      test_module_functor_applications_register_applied_types );
    ( "module functor applications register nested modules",
      test_module_functor_applications_register_nested_modules );
    ( "module functor applications expose nested module protocols",
      test_module_functor_applications_expose_nested_module_protocols );
    ( "module functor applications preserve nested module aliases",
      test_module_functor_applications_preserve_nested_module_aliases );
    ( "module functor application is checked by OCaml",
      test_module_functor_application_is_checked_by_ocaml );
    ( "multi-parameter functor application is checked by OCaml",
      test_multi_parameter_functor_application_is_checked_by_ocaml );
    ("module functors reject bad forms", test_module_functors_reject_bad_forms);
    ( "incremental compilation preserves protocols",
      test_incremental_compilation_preserves_protocols );
    ( "incremental compile_chunk runs OCaml typecheck gate",
      test_incremental_compile_chunk_runs_ocaml_typecheck_gate );
    ( "incremental compilation preserves module aliases",
      test_incremental_compilation_preserves_module_aliases );
    ( "parsetree backend prints runnable ocaml",
      test_parsetree_backend_prints_runnable_ocaml );
    ( "parsetree backend supports record sets",
      test_parsetree_backend_supports_record_sets );
    ( "parsetree backend supports composite sets",
      test_parsetree_backend_supports_composite_sets );
    ( "parsetree backend supports type aliases",
      test_parsetree_backend_supports_type_aliases );
    ( "parsetree backend supports generic OCaml calls",
      test_parsetree_backend_supports_generic_ocaml_calls );
    ( "parsetree backend supports OCaml option and result constructors",
      test_parsetree_backend_supports_ocaml_option_and_result_constructors );
    ( "parsetree backend supports OCaml option and result patterns",
      test_parsetree_backend_supports_ocaml_option_and_result_patterns );
    ( "parsetree backend supports OCaml type application annotations",
      test_parsetree_backend_supports_ocaml_type_application_annotations );
    ( "parsetree backend supports OCaml tuple values",
      test_parsetree_backend_supports_ocaml_tuple_values );
    ( "parsetree backend supports OCaml record values",
      test_parsetree_backend_supports_ocaml_record_values );
    ( "parsetree backend supports variants",
      test_parsetree_backend_supports_variants );
    ( "parsetree backend supports payload variants",
      test_parsetree_backend_supports_payload_variants );
    ( "parsetree backend supports OCaml constructor patterns",
      test_parsetree_backend_supports_ocaml_constructor_patterns );
    ( "match delegates nested opaque constructor patterns to OCaml",
      test_match_delegates_nested_opaque_constructor_patterns_to_ocaml );
    ( "match delegates generic host payload patterns to OCaml",
      test_match_delegates_generic_host_payload_patterns_to_ocaml );
    ( "parsetree backend supports open module",
      test_parsetree_backend_supports_open_module );
    ( "parsetree backend supports include module",
      test_parsetree_backend_supports_include_module );
    ( "parsetree backend supports module alias",
      test_parsetree_backend_supports_module_alias );
    ( "parsetree backend supports module signatures",
      test_parsetree_backend_supports_module_signatures );
    ( "parsetree backend supports module functors",
      test_parsetree_backend_supports_module_functors );
    ( "parsetree backend preserves static errors",
      test_parsetree_backend_preserves_static_errors );
    ( "parsetree backend runs OCaml typecheck gate",
      test_parsetree_backend_runs_ocaml_typecheck_gate );
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
    ( "parsetree backend builds native module alias items",
      test_parsetree_backend_builds_native_module_alias_items );
    ( "parsetree backend builds native module signature items",
      test_parsetree_backend_builds_native_module_signature_items );
    ( "parsetree backend builds native module functor items",
      test_parsetree_backend_builds_native_module_functor_items );
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
    ( "parsetree backend builds native sequence navigation expressions",
      test_parsetree_backend_builds_native_sequence_navigation_expressions );
    ( "parsetree backend builds native match expressions",
      test_parsetree_backend_builds_native_match_expressions );
    ( "parsetree backend builds native let expressions",
      test_parsetree_backend_builds_native_let_expressions );
    ( "parsetree backend builds native cond expressions",
      test_parsetree_backend_builds_native_cond_expressions );
    ( "parsetree backend builds native integer expressions",
      test_parsetree_backend_builds_native_integer_expressions );
    ( "parsetree backend builds native comparison expressions",
      test_parsetree_backend_builds_native_comparison_expressions );
    ( "parsetree backend builds native record field expressions",
      test_parsetree_backend_builds_native_record_field_expressions );
    ( "parsetree backend builds native boolean expressions",
      test_parsetree_backend_builds_native_boolean_expressions );
    ( "parsetree backend builds native string expressions",
      test_parsetree_backend_builds_native_string_expressions );
    ( "parsetree backend builds native collection core expressions",
      test_parsetree_backend_builds_native_collection_core_expressions );
    ( "parsetree backend builds native collection match expressions",
      test_parsetree_backend_builds_native_collection_match_expressions );
    ( "parsetree backend builds native function combinator expressions",
      test_parsetree_backend_builds_native_function_combinator_expressions );
    ( "parsetree backend builds native partial expressions",
      test_parsetree_backend_builds_native_partial_expressions );
    ( "parsetree backend builds native empty collection expressions",
      test_parsetree_backend_builds_native_empty_collection_expressions );
    ( "parsetree backend builds native collection update expressions",
      test_parsetree_backend_builds_native_collection_update_expressions );
    ( "parsetree backend builds native collection index expressions",
      test_parsetree_backend_builds_native_collection_index_expressions );
    ( "parsetree backend builds native map vector expressions",
      test_parsetree_backend_builds_native_map_vector_expressions );
    ( "parsetree backend builds native contains expressions",
      test_parsetree_backend_builds_native_contains_expressions );
    ( "parsetree backend builds native set constructor expressions",
      test_parsetree_backend_builds_native_set_constructor_expressions );
    ( "parsetree backend builds native sequence transform expressions",
      test_parsetree_backend_builds_native_sequence_transform_expressions );
    ( "incremental parsetree backend preserves state",
      test_incremental_parsetree_backend_preserves_state );
    ( "incremental parsetree backend runs OCaml typecheck gate",
      test_incremental_parsetree_backend_runs_ocaml_typecheck_gate );
    ( "compile_string prints Parsetree backend output",
      test_compile_string_prints_parsetree_backend_output );
    ( "infer_interface prints checked signature",
      test_infer_interface_prints_checked_signature );
    ( "compile_chunk prints Parsetree backend output",
      test_compile_chunk_prints_parsetree_backend_output );
  ]

let () =
  let tests =
    match Sys.getenv_opt "LG_TEST_FILTER" with
    | None -> tests
    | Some filter ->
        List.filter
          (fun (name, _) -> string_contains_substring name filter)
          tests
  in
  time_phase "compiler tests" (fun () ->
      List.iter
        (fun (name, run) ->
          try run ()
          with exn ->
            Printf.eprintf "FAILED: %s\n%s\n" name (Printexc.to_string exn);
            exit 1)
        tests);
  flush_ocaml_jobs ()
