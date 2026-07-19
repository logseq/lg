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
  let anonymous_type_name line =
    let words =
      line |> String.split_on_char ' '
      |> List.filter (fun word -> word <> "")
    in
    let rec before_equals previous = function
      | "=" :: _ -> previous
      | word :: rest -> before_equals (Some word) rest
      | [] -> None
    in
    match words with
    | "type" :: "nonrec" :: rest -> before_equals None rest
    | _ -> None
  in
  source |> String.split_on_char '\n'
  |> List.fold_left
       (fun count line ->
         match anonymous_type_name line with
         | Some name
           when String.length name > 1 && name.[0] = 't'
                && name.[1] >= '0' && name.[1] <= '9' ->
             count + 1
         | Some _ | None -> count)
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

let test_directory = lazy (Filename.temp_dir "lg-tests-" "")

let test_dir () = Lazy.force test_directory

let test_test_directory_avoids_existing_pid_directory () =
  let legacy_dir =
    Filename.concat (Filename.get_temp_dir_name ())
      ("lg-tests-" ^ string_of_int (Unix.getpid ()))
  in
  let created = not (Sys.file_exists legacy_dir) in
  if created then Unix.mkdir legacy_dir 0o755;
  let sentinel = Filename.concat legacy_dir "stale-build-artifact" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists sentinel then Sys.remove sentinel;
      if created && Sys.file_exists legacy_dir then Unix.rmdir legacy_dir)
    (fun () ->
      write_file sentinel "stale";
      let actual = test_dir () in
      if actual = legacy_dir then
        failwith "compiler tests must not reuse an existing PID directory";
      if not (Sys.file_exists actual && Sys.is_directory actual) then
        failwith "compiler test directory must be created before use";
      if Sys.file_exists (Filename.concat actual "stale-build-artifact") then
        failwith "compiler test directory must not contain stale artifacts")

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
      "cd %s && ocamlfind ocamlc -package re,unix -linkpkg -I %s -I %s -I %s -I %s \
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

let test_dissoc_missing_fields_is_noop () =
  let source =
    {|
(def x {:name "Ada", :age 36})
(def y (dissoc x :admin?))
(println (str (:name y) ":" (count y)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dissoc_missing_fields_is_noop" "Ada:2\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

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
(defn debug-value [value]
  (prn "value" value))
(debug-value [1 2])
(prn)
|} in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "print_and_println_match_clojure_output"
    "abc\n\"value\" [1 2]\n\n"
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

let test_when_some_binding_constraints_reach_dynamic_calls () =
  let source =
    {|
(defn ^boolean check? [attr]
  (= (hash attr) 42))

(defn validate [c0]
  (when-some [attr c0]
    (when-not (check? attr)
      (str "Attribute " (pr-str attr) " should be marked"))))

(println (validate (identity "x")))
(println (validate nil))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "when_some_binding_constraints_reach_dynamic_calls"
    "Attribute \"x\" should be marked\nnil\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_qualified_type_predicates_materialize_dynamic_parameters () =
  let source =
    {|
(ns qualified-predicates
  (:require [clojure.edn :as edn]))

(defn parse-value [value]
  (let [value (if (clojure.core/string? value)
                (edn/read-string value)
                value)]
    value))

(println (= [1 2] (parse-value "[1 2]")))
(println (= [3 4] (parse-value [3 4])))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "qualified_type_predicates_materialize_dynamic_parameters"
    "true\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_doseq_map_entry_destructuring_preserves_map_values () =
  let source =
    {|
(ns doseq-map-entry
  (:require [clojure.edn :as edn]))

(defn print-fields [schema]
  (doseq [[attribute fields] schema]
    (println (str attribute ":" (:enabled fields false)))))

(print-fields (edn/read-string "{:name {:enabled true}}"))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "doseq_map_entry_destructuring_preserves_map_values"
    ":name:true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_compare_uses_dynamic_seqable_storage () =
  let source =
    {|
(defn compare-values [left right]
  (if (and (sequential? left) (sequential? right))
    0
    (compare left right)))

(defn less
  ([value] true)
  ([left right] (neg? (compare-values left right)))
  ([left right & more]
   (if (less left right)
     (if (next more)
       (recur right (first more) (next more))
       (less right (first more)))
     false)))

(println (compare-values "a" "b"))
(println (less "a" "b" "c"))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "compare_uses_dynamic_seqable_storage" "-1\ntrue\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_type_solver_preserves_shared_and_independent_variables () =
  let open Lg.Types in
  let template =
    TTuple
      [
        TVar "value";
        TFn ([ TVar "value"; TVar "other" ], TVar "other");
      ]
  in
  let actual = TTuple [ TInt; TFn ([ TInt; TString ], TString) ] in
  let substitutions =
    Lg.Type_solver.unify [] template actual
    |> Result.fold ~ok:Fun.id ~error:(fun _ ->
           failwith "compatible shared constraints must unify")
  in
  if Lg.Type_solver.apply substitutions (TVar "value") <> TInt then
    failwith "shared variables must resolve through every occurrence";
  if Lg.Type_solver.apply substitutions (TVar "other") <> TString then
    failwith "independent variables must retain independent solutions";
  let preserved =
    Lg.Type_solver.unify substitutions (TVar "value") TUnknown
    |> Result.fold ~ok:Fun.id ~error:(fun _ ->
           failwith "unknown evidence must not conflict")
  in
  if Lg.Type_solver.apply preserved (TVar "value") <> TInt then
    failwith "unknown evidence must not erase a concrete solution";
  match
    Lg.Type_solver.unify [] (TVar "recursive")
      (TVector (TVar "recursive"))
  with
  | Error _ -> ()
  | Ok _ -> failwith "recursive substitutions must fail the occurs check"

let test_empty_type_substitutions_preserve_type_identity () =
  let open Lg.Types in
  let ty =
    TFn ([ TRecord [ make_field ":name" TString ] ], TSeq (TVector TString))
  in
  if not (Lg.Type_solver.apply [] ty == ty) then
    failwith "empty substitutions must not copy an unchanged type tree"

let test_unrelated_type_substitutions_preserve_type_identity () =
  let open Lg.Types in
  let ty =
    TFn ([ TRecord [ make_field ":name" TString ] ], TSeq (TVector TString))
  in
  if not (Lg.Type_solver.apply [ ("other", TInt) ] ty == ty) then
    failwith "unrelated substitutions must not copy an unchanged type tree"

let test_type_solver_applies_deep_substitutions_linearly () =
  let open Lg.Types in
  let depth = 12_000 in
  let rec nest remaining ty =
    if remaining = 0 then ty else nest (remaining - 1) (TVector ty)
  in
  let rec leaf remaining = function
    | ty when remaining = 0 -> ty
    | TVector inner -> leaf (remaining - 1) inner
    | _ -> failwith "deep substitution changed the type shape"
  in
  let unchanged = TRecord [ make_field ":name" TString ] in
  let template = TTuple [ unchanged; nest depth (TVar "leaf") ] in
  let started_at = Sys.time () in
  let applied = Lg.Type_solver.apply [ ("leaf", TInt) ] template in
  let elapsed = Sys.time () -. started_at in
  (match applied with
  | TTuple [ actual_unchanged; nested ] ->
      if not (actual_unchanged == unchanged) then
        failwith "deep substitutions must preserve unchanged subtrees";
      if leaf depth nested <> TInt then
        failwith "deep substitutions must replace the target variable"
  | _ -> failwith "deep substitution changed the root type");
  if elapsed > 0.05 then
    failwith
      (Printf.sprintf
         "deep type substitution must be linear, but took %.3fs" elapsed)

let test_type_solver_preserves_shared_substitution_dags () =
  let open Lg.Types in
  let depth = 24 in
  let rec share remaining ty =
    if remaining = 0 then ty
    else
      let child = share (remaining - 1) ty in
      TTuple [ child; child ]
  in
  let rec leaf remaining = function
    | ty when remaining = 0 -> ty
    | TTuple [ left; right ] ->
        if not (left == right) then
          failwith "substitution must preserve shared type subtrees";
        leaf (remaining - 1) left
    | _ -> failwith "shared substitution changed the type shape"
  in
  let template = share depth (TVar "leaf") in
  let started_at = Sys.time () in
  let applied = Lg.Type_solver.apply [ ("leaf", TInt) ] template in
  let elapsed = Sys.time () -. started_at in
  if leaf depth applied <> TInt then
    failwith "shared substitutions must replace the target variable";
  if elapsed > 0.05 then
    failwith
      (Printf.sprintf
         "shared type substitution must be linear, but took %.3fs" elapsed)

let test_generic_record_calls_freshen_callee_type_variables () =
  let source =
    {|
(defn limit-rel [rel vars]
  (when-some [attrs' (not-empty (select-keys (:attrs rel) vars))]
    (assoc rel :attrs attrs')))

(defn limited-rel [context vars]
  (limit-rel (first (:rels context)) vars))

(def context {:rels [{:attrs {:a 1 :b 2}}]})
(when-some [limited (limited-rel context [:a])]
  (println (count (:attrs limited))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_record_calls_freshen_callee_type_variables" "1\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dynamic_sequences_adapt_to_nullable_callback_parameters () =
  let source =
    {|
(defn option-equals? [candidate value]
  (match candidate
    (Some candidate-value) (= candidate-value value)
    None false))
(defn option-present? [values value]
  (loop [index 0]
    (if (= index (Array.length values))
      false
      (if (option-equals? (aget values index) value)
        true
        (recur (inc index))))))
(defn removed-option? [candidate new-values]
  (match candidate
    (Some value) (not (option-present? new-values value))
    None false))
(defn option-value [candidate]
  (match candidate
    (Some value) value
    None (Stdlib.failwith "missing option value")))
(defn collect-removed-options [values new-values]
  (array-from
    (map option-value
      (filter
        (fn [candidate] (removed-option? candidate new-values))
        (array-to-seq values)))))
(def values (array (Some 1) (Some 2)))
(def new-values (array (Some 2)))
(def removed-values (collect-removed-options values new-values))
(println (str (Array.length removed-values) ":" (aget removed-values 0)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_sequences_adapt_to_nullable_callback_parameters"
    "1:1\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_keyword_lookup_constrains_first_of_protocol_sequences () =
  let source =
    {|
(defrecord Datom [v])
(defprotocol IndexAccess
  (-datoms [database component]))
(defn current-value [database]
  (:v (first (-datoms database nil))))
(defrecord Database [values]
  IndexAccess
  (-datoms [database _component] (.-values database)))
(println (current-value (Database. [(Datom. 42)])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "keyword_lookup_constrains_first_of_protocol_sequences"
    "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_protocol_result_context_does_not_constrain_arguments () =
  let source =
    {|
(defrecord Datom [v])
(defprotocol SchemaAccess
  (-schema [database]))
(defprotocol IndexAccess
  (-datoms [database index c0 c1 c2 c3]))
(defrecord Database [schema datoms]
  SchemaAccess
  (-schema [database] (.-schema database))
  IndexAccess
  (-datoms [database _index _c0 _c1 _c2 _c3] (.-datoms database)))
(defn same-datoms? [left right]
  (= (count left) (count right)))
(defn same-database? [database other]
  (and (instance? Database other)
       (= (-schema database) (-schema other))
       (same-datoms?
         (-datoms database :eavt nil nil nil nil)
         (-datoms other :eavt nil nil nil nil))))
(def database (Database. {:name "schema"} [(Datom. 42)]))
(println (same-database? database database))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "protocol_result_context_does_not_constrain_arguments"
    "true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_frontend_location_index_avoids_quadratic_scans () =
  let form_count = 5_000 in
  let source =
    List.init form_count (fun index ->
        Printf.sprintf "(def value%d %d)" index index)
    |> String.concat "\n"
  in
  let started_at = Sys.time () in
  let parsed =
    Lg.Toolchain.Lg_frontend.implementation ~filename:"large.cljc" source
    |> expect_ok
  in
  let elapsed = Sys.time () -. started_at in
  let last_location = List.nth parsed.locations (form_count - 1) in
  if last_location.loc_start.pos_lnum <> form_count then
    failwith "the indexed location must preserve the final source line";
  if elapsed > 1.0 then
    failwith
      (Printf.sprintf
         "frontend location construction took %.2fs; expected at most 1.00s"
         elapsed)

let test_refresh_named_record_realigns_forward_declared_records () =
  let open Lg.Types in
  let fields = [ make_field ":db-before" TUnknown ] in
  let fresh =
    named_record ~type_name:"txreport" ~set_module_name:"txreport_set" fields
  in
  let stale =
    named_record ~type_parameters:[ "value" ] ~type_name:"txreport"
      ~set_module_name:"txreport_set" fields
  in
  (match fresh with
  | TNamed_record fresh_record -> (
      let refreshed =
        refresh_named_record fresh_record (TFn ([ stale ], TNullable stale))
      in
      let expected = TFn ([ fresh ], TNullable fresh) in
      if refreshed <> expected then
        failwith
          ("forward-declared record arguments must realign to the emitted \
            declaration: " ^ source_name refreshed);
      let generic =
        named_record ~type_parameters:[ "value" ] ~type_name:"btset"
          ~set_module_name:"btset_set" fields
      in
      match generic with
      | TNamed_record generic_record ->
          let instantiated =
            TNamed_record { generic_record with type_arguments = [ TInt ] }
          in
          let refreshed = refresh_named_record generic_record instantiated in
          if refreshed <> instantiated then
            failwith
              "matching argument counts must keep the more specific \
               instantiation";
          let refreshed = refresh_named_record generic_record stale in
          if refreshed <> stale then
            failwith "unrelated record shapes must be left alone"
      | _ -> failwith "expected a named record")
  | _ -> failwith "expected a named record")

let test_freshen_deferred_dynamic_dispatch_stays_monomorphic () =
  let open Lg.Types in
  let dynamic = dynamic_constraint TUnknown in
  let protocol_id = Lg.Protocol_id.of_string "test/dynamic-dispatch" in
  let dynamic_method = TFn ([ dynamic; dynamic ], dynamic) in
  let dynamic_constraint_ty =
    protocol_constraint protocol_id [ dynamic_method ] TUnknown
  in
  (match
     Lg.Elaborator.freshen_deferred_type (TFn ([ dynamic_constraint_ty ], TInt))
   with
  | TFn ([ refreshed ], TInt) -> (
      match protocol_constraint_info refreshed with
      | Some (_, _, value_ty) ->
          if not (equal value_ty dynamic) then
            failwith
              ("dynamic dispatch must keep the container monomorphic dynamic, \
                got: " ^ source_name value_ty)
      | None -> failwith "expected a protocol constraint")
  | _ -> failwith "expected a deferred function type");
  let generic_method = TFn ([ TUnknown ], TUnknown) in
  let generic_constraint_ty =
    protocol_constraint protocol_id [ generic_method ] TUnknown
  in
  match
    Lg.Elaborator.freshen_deferred_type (TFn ([ generic_constraint_ty ], TInt))
  with
  | TFn ([ refreshed ], TInt) -> (
      match protocol_constraint_info refreshed with
      | Some (_, _, TVar _) -> ()
      | Some (_, _, value_ty) ->
          failwith
            ("generic receivers must keep freshened polymorphism, got: "
            ^ source_name value_ty)
      | None -> failwith "expected a protocol constraint")
  | _ -> failwith "expected a deferred function type"

let test_freshen_deferred_erased_seqable_values_stay_dynamic () =
  let open Lg.Types in
  let constraints =
    [
      seqable_constraint TUnknown;
      optional_seqable_constraint TUnknown TUnknown;
      optional_sequential_constraint TUnknown TUnknown;
    ]
  in
  List.iter
    (fun constraint_ty ->
      match Lg.Elaborator.freshen_deferred_type (TFn ([ constraint_ty ], TInt)) with
      | TFn ([ refreshed ], TInt) -> (
          match seqable_constraint_info refreshed with
          | Some (_, element_ty, value_ty)
            when is_dynamic element_ty && is_dynamic value_ty -> ()
          | Some (_, element_ty, value_ty) ->
              failwith
                ("deferred erased seqable items and values must stay dynamic, "
               ^ "got: " ^ source_name element_ty ^ " / "
                ^ source_name value_ty)
          | None -> failwith "expected a seqable constraint")
      | _ -> failwith "expected a deferred function type")
    constraints

let test_unresolved_type_scan_stops_at_nominal_records () =
  let open Lg.Types in
  let fields = [ make_field ":next" TUnknown ] in
  let nominal =
    named_record ~type_name:"node" ~set_module_name:"node_set" fields
  in
  if Lg.Call_elaborator.contains_unresolved_type nominal then
    failwith "nominal record fields must not be expanded during boundary scans";
  if
    not
      (Lg.Call_elaborator.contains_unresolved_type
         (TRecord fields))
  then failwith "structural record fields must retain unresolved type evidence"

let test_deferred_forward_calls_keep_nominal_receiver_evidence () =
  let source =
    {|
(declare indexed?)

(defprotocol IIndexAccess
  (-datoms [db index c0 c1 c2 c3]))

(defn validate-indexed [db index c0 c1 c2 c3]
  (when (= index :avet)
    (when-some [attr c0]
      (when-not (indexed? db attr)
        (str "Attribute " attr " should be indexed")))))

(defprotocol IDB
  (-schema [db])
  (-attrs-by [db property]))

(defrecord DB [attrs]
  IDB
  (-schema [db] (.-attrs db))
  (-attrs-by [db property] (.-attrs db))

  IIndexAccess
  (-datoms [db index c0 c1 c2 c3]
    (validate-indexed db index c0 c1 c2 c3)))

(defn concrete-indexed? [^DB db]
  (indexed? db :name))

(defn ^boolean indexed? [db attr]
  (contains? (-attrs-by db :db/index) attr))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_compiles "deferred_forward_calls_keep_nominal_receiver_evidence"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_deferred_named_record_fields_receive_body_constraints () =
  let source =
    read_file
      (Filename.concat (repo_root ()) "test/datascript/upstream/lru.cljc")
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_compiles
    "deferred_named_record_fields_receive_body_constraints" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_typed_ir_preserves_explicit_boundary_operations () =
  let open Lg.Types in
  let dynamic = dynamic_constraint TUnknown in
  let conversion =
    Lg.Semantic_ir.Apply
      ( Lg.Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.int",
        [ Lg.Semantic_ir.Int 42 ] )
  in
  let packed =
    Lg.Semantic_ir.PackDynamic
      { source_ty = TInt; target_ty = dynamic; conversion }
  in
  let unpacked =
    Lg.Semantic_ir.UnpackDynamic
      { source_ty = dynamic; target_ty = TInt; conversion = Lg.Semantic_ir.Int 42 }
  in
  let normalized =
    Lg.Semantic_ir.NullableToSeq
      {
        source_ty = TNullable (TVector TInt);
        element_ty = TInt;
        conversion = Lg.Semantic_ir.Ident "normalized";
      }
  in
  if
    not
      (Lg.Semantic_ir.exists_identifier
         (( = ) "Lg_runtime.Runtime_dynamic.int") packed)
  then failwith "boundary traversal must retain the conversion expression";
  let annotations = Lg.Semantic_ir.type_annotations packed in
  if not (List.mem TInt annotations && List.mem dynamic annotations) then
    failwith "pack boundary must retain source and target type evidence";
  (match Lg.Semantic_lowering.expression packed with
  | Lg.Ocaml_ir.Apply _ -> ()
  | _ -> failwith "pack boundary must lower to its conversion");
  (match Lg.Semantic_lowering.expression unpacked with
  | Lg.Ocaml_ir.Int 42 -> ()
  | _ -> failwith "unpack boundary must lower to its conversion");
  (match Lg.Semantic_lowering.expression normalized with
  | Lg.Ocaml_ir.Ident "normalized" -> ()
  | _ -> failwith "nullable sequence boundary must lower to its conversion");
  (match
     Lg.Expression_support.pack_plain_dynamic_value
       (typed_ir TInt (Lg.Semantic_ir.Int 42))
   with
  | Some expression -> (
      match expression with
      | Lg.Semantic_ir.PackDynamic _ -> ()
      | _ -> failwith "dynamic packing must emit an explicit boundary node")
  | None -> failwith "integer packing must be supported");
  match
    Lg.Collection_capability.to_seq_expr Lg.Compiler_environment.empty
      (typed_ir (TNullable (TVector TInt)) (Lg.Semantic_ir.Ident "values"))
  with
  | Ok (_, expression) -> (
      match expression with
      | Lg.Semantic_ir.NullableToSeq _ -> ()
      | _ -> failwith "nullable seq conversion must emit a boundary node")
  | Error _ -> failwith "nullable vectors must normalize to sequences"

let test_core_form_expansions_use_hygienic_identifiers () =
  let open Lg.Ast in
  let expansion =
    Lg.Core_form_expansion.update_in (FSymbol "target")
      [ FKeyword ":outer"; FKeyword ":inner" ]
      (FSymbol "inc") []
  in
  (match expansion with
  | FList
      (FCoreSymbol Core_update :: FSymbol "target" :: FKeyword ":outer"
      :: FCoreSymbol Core_update :: FKeyword ":inner" :: _) ->
      ()
  | _ ->
      failwith
        "compiler-generated update calls must use unforgeable core identifiers");
  (match
     Lg.Core_form_expansion.apply_transducer (FSymbol "values")
       (FList [ FSymbol "map"; FSymbol "inc" ])
   with
  | Ok (FList [ FCoreSymbol Core_map; FSymbol "inc"; FSymbol "values" ]) ->
      ()
  | _ -> failwith "transducer expansion must preserve a hygienic core map");
  let assoc_expansion =
    Lg.Core_form_expansion.assoc_in (FSymbol "target")
      [ FKeyword ":outer"; FKeyword ":inner" ]
      (FInt 42)
  in
  let rec contains_core expected = function
    | FCoreSymbol actual -> actual = expected
    | FList forms | FVector forms -> List.exists (contains_core expected) forms
    | FMap pairs ->
        List.exists
          (fun (key, value) ->
            contains_core expected key || contains_core expected value)
          pairs
    | FSymbol _ | FKeyword _ | FString _ | FRegex _ | FInt _ | FFloat _
    | FChar _ | FBool _ ->
        false
  in
  if
    not
      (contains_core Core_assoc assoc_expansion
      && contains_core Core_get assoc_expansion)
  then failwith "assoc-in must preserve hygienic assoc and get identifiers"

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

let test_dynamic_record_lookup_specializes_shared_generic_fields () =
  let open Lg.Types in
  let sorted value_ty = TOcaml_app ("sorted", [ value_ty ]) in
  let datom_ty = TOcaml "datom" in
  let db_ty =
    named_record ~type_parameters:[ "value" ] ~type_name:"db"
      ~set_module_name:"Db_set"
      [
        make_field ":eavt" (sorted datom_ty);
        make_field ":aevt" (sorted datom_ty);
        make_field ":avet" (sorted (TVar "value"));
      ]
  in
  let env =
    Lg.Compiler_environment.add "__record//DB" (binding "db" db_ty)
      Lg.Compiler_environment.empty
  in
  [ TVar "selected"; TUnknown ]
  |> List.iter (fun selected_ty ->
         match
           Lg.Expression_support.dynamic_key_record_type env
             (sorted selected_ty)
         with
         | Some (TNamed_record { type_arguments = [ value_ty ]; _ })
           when equal value_ty datom_ty ->
             ()
         | _ ->
             failwith
               "dynamic record lookup must retain the shared concrete field type")

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

let test_protocol_satisfaction_uses_stabilized_evidence () =
  let protocol = Lg.Protocol_id.create ~owner:[ "Domain" ] ~name:"Visible" in
  let method_id =
    Lg.Method_id.create ~owner:[ "Domain"; "Visible" ] ~name:"visible"
  in
  let signature : Lg.Protocol_registry.method_signature =
    {
      method_id;
      param_tys = [ Lg.Types.TUnknown ];
      return_ty = Lg.Types.TBool;
    }
  in
  let current =
    Lg.Protocol_registry.declare protocol [ signature ]
      Lg.Protocol_registry.empty
    |> expect_ok
  in
  let implementation =
    Lg.Types.binding "visible_int"
      (Lg.Types.TFn ([ Lg.Types.TInt ], Lg.Types.TBool))
  in
  let evidence =
    Lg.Protocol_registry.add_implementation protocol method_id
      Lg.Protocol_registry.Int_receiver implementation current
    |> expect_ok
  in
  let env =
    Lg.Compiler_environment.empty
    |> Lg.Compiler_environment.with_protocols current
    |> Lg.Compiler_environment.with_protocol_evidence (Some evidence)
  in
  if not (Lg.Protocol.type_satisfies env protocol Lg.Types.TInt) then
    failwith "protocol satisfaction must use stabilized implementation evidence"

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
              ~dynamic_unpack:Lg.Call_elaborator.dynamic_unpack
              ~pack_dynamic_value:Lg.Call_elaborator.pack_dynamic_value
              ~pack_constrained_value:Lg.Call_elaborator.pack_constrained_value
              ~argument_compatible:Lg.Call_elaborator.argument_compatible
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
              ~pack_dynamic_value:(fun _env _expected value ->
                Ok value.Lg.Types.semantic_expr)
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
    Lg.Compiler.compile_parsetree_with_filename ~filename:"identity.cljc" source
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
        Lg.Toolchain.analyze ~filename:"identity.cljc" source |> expect_ok
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
        Lg.Language_service.analyze ~filename:"identity.cljc" source |> expect_ok
      in
      let offset = expect_substring_index source "1" in
      match Lg.Language_service.source_node_id_at language_analysis ~offset with
      | Some id when String.starts_with ~prefix:"identity.cljc:" id -> ()
      | Some id -> failwith ("unexpected source node identity " ^ id)
      | None -> failwith "expected LSP lookup to return a source node identity")

let test_source_node_identity_covers_value_bindings () =
  let source = "(def answer 42)" in
  let analysis =
    Lg.Language_service.analyze ~filename:"binding-identity.cljc" source
    |> expect_ok
  in
  let offset = expect_substring_index source "answer" in
  match Lg.Language_service.source_node_id_at analysis ~offset with
  | Some id when String.starts_with ~prefix:"binding-identity.cljc:" id -> ()
  | Some id -> failwith ("unexpected binding source node identity " ^ id)
  | None -> failwith "expected value binding to preserve source node identity"

let test_source_node_identity_covers_record_value_bindings () =
  let filename = "record-binding-identity.cljc" in
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
  let filename = "recursive-identity.cljc" in
  let source =
    "(defn countdown [^:int n] :int\n  (if (= n 0) 0 (countdown (dec n))))"
  in
  let analysis = Lg.Language_service.analyze ~filename source |> expect_ok in
  expect_source_id_at_text filename source analysis "countdown"

let test_source_node_identity_covers_function_parameters () =
  let filename = "parameter-identity.cljc" in
  let source = "(defn add-one [value] (+ value 1))" in
  let analysis = Lg.Language_service.analyze ~filename source |> expect_ok in
  expect_source_id_at_text filename source analysis "value"

let test_source_node_identity_covers_annotated_parameters () =
  let filename = "annotated-parameter-identity.cljc" in
  let source = "(defn increment [^:int value] (+ value 1))" in
  let analysis = Lg.Language_service.analyze ~filename source |> expect_ok in
  expect_source_id_at_text filename source analysis "value"

let test_source_node_identity_covers_destructuring_bindings () =
  let filename = "destructuring-identity.cljc" in
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
  let filename = "let-identity.cljc" in
  let source = "(def result (let [local 41] (+ local 1)))" in
  let analysis = Lg.Language_service.analyze ~filename source |> expect_ok in
  expect_source_id_at_text filename source analysis "local"

let test_source_node_identity_covers_let_destructuring () =
  let filename = "let-destructuring-identity.cljc" in
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
  let filename = "match-identity.cljc" in
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
  let filename = "loop-identity.cljc" in
  let source =
    "(def result (loop [counter 0] (if (= counter 2) counter (recur (inc \
     counter)))))"
  in
  let analysis = Lg.Language_service.analyze ~filename source |> expect_ok in
  expect_source_id_at_text filename source analysis "counter"

let test_source_node_identity_covers_catch_bindings () =
  let filename = "catch-identity.cljc" in
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

let test_clojure_and_cljs_core_namespace_aliases_dispatch_to_core () =
  let source =
    {|
(ns app.core-alias
  (:require [#?(:native clojure.core :melange cljs.core) :as c]))
(def values (c/keys {:a 1 :b 2}))
(println (c/count values))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "clojure_and_cljs_core_namespace_aliases_dispatch_to_core" "2\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_keyword_lookup_ignores_refer_clojure_get_exclusion () =
  let source =
    {|
(ns app.keyword-lookup
  (:refer-clojure :exclude [get]))
(def value {:answer 42})
(println (:answer value))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "keyword_lookup_ignores_refer_clojure_get_exclusion" "42\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_namespace_rejects_import_clause () =
  Lg.Compiler.compile_string
    {|
(ns app.invalid
  (:import clojure.lang.IFn$OOL))
|}
  |> expect_error "lg namespaces do not support :import"

let test_namespace_ignores_reader_conditional_import_clause () =
  let source =
    {|
(ns app.portable
  #?(:clj (:import [java.lang Object])))
(println 42)
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "namespace_ignores_reader_conditional_import_clause" "42\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_reader_conditional_import_refers_lg_record_types () =
  let provider =
    {|
(ns app.parser-types)
(defrecord BindIgnore [])
|}
  in
  let consumer =
    {|
(ns app.query-types
  (:require
    [app.parser-types :as parser
     #?@(:cljs [:refer [BindIgnore]])])
  #?(:clj
     (:import
       [java.lang Object]
       [app.parser-types BindIgnore])))

(defprotocol BindingKind
  (binding-kind [binding]))
(extend-protocol BindingKind
  BindIgnore
  (binding-kind [_] :ignore))
|}
  in
  let compile target =
    let state, _ =
      Lg.Compiler.compile_chunk ~target Lg.Compiler.empty_state provider
      |> expect_ok
    in
    ignore (Lg.Compiler.compile_chunk ~target state consumer |> expect_ok)
  in
  compile Lg.Target.Native;
  compile Lg.Target.Melange

let test_dynamic_vars_bind_and_restore_portably () =
  let source =
    {|
(def ^:dynamic *selected* nil)
(defn selected? [value]
  (contains? *selected* value))

(println (selected? :a))
(println
  (binding [*selected* #{:a}]
    (selected? :a)))
(println
  (binding [*selected* #{:a}]
    (and
      (selected? :a)
      (binding [*selected* #{:b}]
        (and (selected? :b) (not (selected? :a))))
      (selected? :a))))
(try
  (binding [*selected* #{:a}]
    (throw (ex-info "stop" {})))
  (catch _ false))
(println (selected? :a))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_vars_bind_and_restore_portably"
    "false\ntrue\ntrue\nfalse\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_named_fn_is_locally_recursive () =
  let source =
    {|
(def countdown
  (fn countdown [value]
    (if (zero? value)
      0
      (countdown (dec value)))))
(println (= 0 (countdown 5)))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "named_fn_is_locally_recursive" "true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  Lg.Compiler.compile_string
    {|
(def value (fn private-name [x] x))
(def leaked private-name)
|}
  |> expect_error "unknown symbol private-name"

let test_if_joins_static_and_dynamic_function_parameters () =
  let source =
    {|
(defrecord Box [value])
(defn choose-reader [dynamic?]
  (if dynamic?
    (fn dynamic-reader [^:dynamic value] value)
    (fn box-reader [^Box value] (:value value))))
(def read-box (choose-reader false))
(def read-dynamic (choose-reader true))
(println (= 42 (read-box (Box. 42))))
(println (= :answer (read-dynamic :answer)))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "if_joins_static_and_dynamic_function_parameters"
    "true\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_jvm_lookup_hints_do_not_narrow_dynamic_values () =
  let source =
    {|
(ns app.lookup-hint
  (:require [#?(:cljs cljs.reader :clj clojure.edn) :as edn]))
(defn lookup-value [lookup key]
  (.valAt ^ILookup lookup key))
(println (= 42 (lookup-value (edn/read-string "{:answer 42}") :answer)))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "jvm_lookup_hints_do_not_narrow_dynamic_values" "true\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_object_marker_builds_dynamic_arrays_without_java () =
  let source =
    {|
(def values (into-array Object [1 2]))
(aset values 1 "two")
(println (= 1 (aget values 0)))
(println (= "two" (aget values 1)))
(def empty-values (make-array Object 2))
(println
  (and
    (nil? (aget empty-values 0))
    (nil? (aget empty-values 1))))
(defn clone-dynamic [^:dynamic source]
  (aclone source))
(def copied (clone-dynamic values))
(aset copied 0 2)
(println
  (and
    (= 1 (aget values 0))
    (= 2 (aget copied 0))))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "object_marker_builds_dynamic_arrays_without_java"
    "true\ntrue\ntrue\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_lazily_persistent_vector_create_owning_is_portable () =
  let source =
    {|
(def values (into-array Object [1 "two"]))
(def result (LazilyPersistentVector/createOwning values))
(println (= [1 "two"] result))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "lazily_persistent_vector_create_owning_is_portable"
    "true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_clojure_edn_read_string_behaves_on_native_and_melange () =
  let source =
    {|
(ns app.edn
  (:require [#?(:cljs cljs.reader :clj clojure.edn) :as edn]))

(println
  (= [nil true false -12 3.5 :kw 'symbol "text"]
     (edn/read-string "[nil true false -12 3.5 :kw symbol \"text\"]")))
(def nested
  (edn/read-string
    "{:rules [(rule [?e :name ?name])] :flags #{true nil}}"))
(println
  (and
    (= [(list 'rule ['?e :name '?name])] (:rules nested))
    (= 2 (count (:flags nested)))
    (contains? (:flags nested) true)
    (contains? (:flags nested) nil)))
(println
  (= {:a 1 :b [2 3]}
     (edn/read-string "{:a 1, ; ignored
                       :b [2 3]}")))
(println (= "hello world" (edn/read-string "\"hello world\"")))
(println (= "line\nnext" (edn/read-string "\"line\\nnext\"")))
(println (nil? (edn/read-string "")))
(println (= 1 (edn/read-string "1 2")))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_edn_read_string_behaves_on_native_and_melange"
    "true\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_clojure_edn_read_string_rejects_invalid_collections () =
  let source =
    {|
(ns app.invalid-edn
  (:require [#?(:cljs cljs.reader :clj clojure.edn) :as edn]))

(defn invalid-edn? [source]
  (try
    (edn/read-string source)
    false
    (catch _ true)))

(println (invalid-edn? "{:a}"))
(println (invalid-edn? "{:a 1 :a 2}"))
(println (invalid-edn? "#{1 1}"))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_edn_read_string_rejects_invalid_collections"
    "true\ntrue\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_referred_update_supports_threaded_nested_calls () =
  let provider_source =
    read_file
      (Filename.concat (repo_root ()) "test/datascript/upstream/inline.cljc")
  in
  let consumer_source =
    {|
(ns compat.app
  (:require [datascript.inline :refer [update]])
  (:refer-clojure :exclude [update]))
(deftype DB [max-eid])
(defn advance-max-eid [^DB db eid]
  (assoc db :max-eid eid))
(defn add-value [values value]
  (conj values value))
(defn allocate-eid
  ([report eid]
   (update report :db-after advance-max-eid eid))
  ([report e eid]
   (cond-> report
     true
     (->
       (update :tempids assoc e eid)
       (update :reverse-tempids update eid add-value e))

     true
     (update :db-after advance-max-eid eid))))
(def initial
  {:db-after (DB. 0)
   :tempids {}
   :reverse-tempids {}})
(def updated (allocate-eid initial "temp" 1))
(let [db-after ^DB (:db-after updated)]
  (println
    (str (.-max-eid db-after) ":"
         (get (:tempids updated) "temp") ":"
         (count (get (:reverse-tempids updated) 1)))))
|}
  in
  let compile target =
    let state, provider_ocaml =
      Lg.Compiler.compile_chunk ~target Lg.Compiler.empty_state provider_source
      |> expect_ok
    in
    let _, consumer_ocaml =
      Lg.Compiler.compile_chunk ~target state consumer_source |> expect_ok
    in
    (provider_ocaml ^ "\n" ^ consumer_ocaml, consumer_ocaml)
  in
  let ocaml_source, consumer_ocaml = compile Lg.Target.Native in
  if string_contains_substring consumer_ocaml "datascript_inline_update__" then
    failwith "inline update calls must not use the runtime wrapper";
  assert_ocaml_runs "referred_update_supports_threaded_nested_calls"
    "1:1:1\n" ocaml_source;
  ignore (compile Lg.Target.Melange)

let test_clj_reader_conditional_macros_survive_deferred_melange_bodies () =
  let provider_source =
    {|
(ns datascript.util)
#?(:clj
   (defmacro raise [& fragments]
     (let [msgs (butlast fragments)
           data (last fragments)]
       `(throw
          (ex-info
            (str ~@(map (fn [message]
                          (if (string? message)
                            message
                            (list 'pr-str message)))
                     msgs))
            ~data)))))
|}
  in
  let consumer_source =
    {|
(ns app.schema
  (:require [datascript.util :as util])
  #?(:cljs
     (:require-macros [app.schema :refer [validate-attribute]])))
#?(:clj
   (defmacro validate-attribute [attribute]
     `(when (= ~attribute :child)
        (util/raise "Bad component " ~attribute
          {:error :schema/validation :attribute ~attribute}))))
(defn validate-schema [attributes]
  (doseq [attribute attributes]
    (validate-attribute attribute)))
(defn rejected? []
  (try
    (validate-schema [:child])
    false
    (catch _ true)))
(println (rejected?))
|}
  in
  let compile target =
    let state, provider_ocaml =
      Lg.Compiler.compile_chunk ~target Lg.Compiler.empty_state provider_source
      |> expect_ok
    in
    let _, consumer_ocaml =
      Lg.Compiler.compile_chunk ~target state consumer_source |> expect_ok
    in
    provider_ocaml ^ "\n" ^ consumer_ocaml
  in
  let native_source = compile Lg.Target.Native in
  assert_ocaml_runs
    "clj_reader_conditional_macros_survive_deferred_melange_bodies" "true\n"
    native_source;
  ignore (compile Lg.Target.Melange)

let current_datascript_sources () =
    [
      "datascript/me/tonsky/persistent_sorted_set/arrays.cljc";
      "datascript/me/tonsky/persistent_sorted_set/protocol.cljc";
      "datascript/me/tonsky/persistent_sorted_set.cljc";
      "test/datascript/upstream/inline.cljc";
      "test/datascript/upstream/util.cljc";
      "test/datascript/upstream/lru.cljc";
      "test/datascript/upstream/schema.cljc";
      "test/datascript/upstream/db.cljc";
      "test/datascript/upstream/parser.cljc";
      "test/datascript/upstream/entity.cljc";
      "test/datascript/upstream/built_ins.cljc";
    ]
    |> List.map (fun path -> (path, read_file (Filename.concat (repo_root ()) path)))

let compile_current_datascript target extra_sources =
  let _, reversed_outputs =
    List.fold_left
      (fun (state, outputs) (filename, source) ->
        let state, output =
          match
            Lg.Compiler.compile_chunk_with_filename ~target ~filename state source
          with
          | Ok compiled -> compiled
          | Error (error : Lg.Compiler.compile_error) ->
              let location =
                match error.location with
                | None -> ""
                | Some location ->
                    Printf.sprintf " at %s:%d:%d"
                      location.Location.loc_start.Lexing.pos_fname
                      location.Location.loc_start.Lexing.pos_lnum
                      (location.Location.loc_start.Lexing.pos_cnum
                      - location.Location.loc_start.Lexing.pos_bol)
              in
              failwith
                (Printf.sprintf "failed to compile %s for %s%s: %s" filename
                   (Lg.Target.to_string target) location error.message)
        in
        (state, output :: outputs))
      (Lg.Compiler.empty_state, [])
      (current_datascript_sources () @ extra_sources)
  in
  String.concat "\n" (List.rev reversed_outputs)

let test_current_datascript_chain_compiles_for_native_and_melange () =
  ignore (compile_current_datascript Lg.Target.Native []);
  ignore (compile_current_datascript Lg.Target.Melange [])

let test_datascript_make_array_one_arity_behaves_on_native_and_melange () =
  let source =
    {|
(ns app.dynamic-array
  (:require [me.tonsky.persistent-sorted-set.arrays :as arrays]))

(def values (arrays/make-array 3))
(def empty-values (arrays/make-array 0))
(println
  (str
    (nil? (arrays/aget values 0)) ":"
    (= 0 (arrays/alength empty-values))))
(arrays/aset values 0 42)
(arrays/aset values 1 "answer")
(println
  (str
    (= 42 (arrays/aget values 0)) ":"
    (= "answer" (arrays/aget values 1)) ":"
    (nil? (arrays/aget values 2))))
|}
  in
  let sources = [ ("test/datascript/dynamic_array.cljc", source) ] in
  let native_source = compile_current_datascript Lg.Target.Native sources in
  assert_ocaml_runs
    "datascript_make_array_one_arity_behaves_on_native_and_melange"
    "true:true\ntrue:true:true\n" native_source;
  ignore (compile_current_datascript Lg.Target.Melange sources)

let test_current_datascript_entity_behaves_on_native () =
  let source =
    {|
(ns app.entity-behavior
  (:require [datascript.db :as db]
            [datascript.impl.entity :as entity]))
(def database
  (db/init-db
    [(db/datom 1 :name "Ivan")
     (db/datom 1 :age 19)]
    {}
    {}))
(when-some [person (entity/entity database 1)]
  (println
    (str (:db/id person) ":" (:name person) ":" (:age person) ":"
         (count person))))
|}
  in
  let native_source =
    compile_current_datascript Lg.Target.Native
      [ ("test/datascript/entity_behavior.cljc", source) ]
  in
  assert_ocaml_runs "current_datascript_entity_behaves_on_native"
    "1:Ivan:19:2\n" native_source;
  ignore
    (compile_current_datascript Lg.Target.Melange
       [ ("test/datascript/entity_behavior.cljc", source) ])

let test_current_datascript_pull_parser_compiles_for_native_and_melange () =
  let path = "test/datascript/upstream/pull_parser.cljc" in
  let source = read_file (Filename.concat (repo_root ()) path) in
  ignore (compile_current_datascript Lg.Target.Native [ (path, source) ]);
  ignore (compile_current_datascript Lg.Target.Melange [ (path, source) ])

let current_datascript_pull_sources () =
  [
    "test/datascript/upstream/pull_parser.cljc";
    "test/datascript/upstream/pull_api.cljc";
  ]
  |> List.map (fun path -> (path, read_file (Filename.concat (repo_root ()) path)))

let test_current_datascript_pull_api_compiles_for_native_and_melange () =
  let sources = current_datascript_pull_sources () in
  ignore (compile_current_datascript Lg.Target.Native sources);
  ignore (compile_current_datascript Lg.Target.Melange sources)

let test_current_datascript_pull_api_behaves_on_native () =
  let source =
    {|
(ns app.pull-api-behavior
  (:require [datascript.db :as db]
            [datascript.pull-api :as pull]))

(def database
  (db/init-db
    [(db/datom 1 :name "Ivan")
     (db/datom 1 :tags :a)
     (db/datom 1 :tags :b)
     (db/datom 1 :friend 2)
     (db/datom 2 :name "Oleg")
     (db/datom 3 :friend 2)]
    {:tags {:db/cardinality :db.cardinality/many}
     :friend {:db/valueType :db.type/ref
              :db/cardinality :db.cardinality/one}}
    {}))

(def direct (pull/pull database [:name :tags] 1))
(def defaulted (pull/pull database [[:missing :default "fallback"]] 1))
(def reverse-result (pull/pull database [{:_friend [:db/id]}] 2))
(def wildcard-result (pull/pull database [:*] 1))
(def many-result (pull/pull-many database [:name] [1 2 999]))

(println (= "Ivan" (:name direct)))
(println (= #{:a :b} (set (:tags direct))))
(println (= "fallback" (:missing defaulted)))
(println (= #{1 3} (set (map :db/id (:_friend reverse-result)))))
(println (and (= "Ivan" (:name wildcard-result))
              (= 2 (:db/id (:friend wildcard-result)))))
(println (and (= ["Ivan" "Oleg" nil] (mapv :name many-result))
              (nil? (pull/pull database [:name] 999))))
|}
  in
  let native_source =
    compile_current_datascript Lg.Target.Native
      (current_datascript_pull_sources ()
      @ [ ("test/datascript/pull_api_behavior.cljc", source) ])
  in
  assert_ocaml_runs "current_datascript_pull_api_behaves_on_native"
    "true\ntrue\ntrue\ntrue\ntrue\ntrue\n" native_source;
  ignore
    (compile_current_datascript Lg.Target.Melange
       (current_datascript_pull_sources ()
       @ [ ("test/datascript/pull_api_behavior.cljc", source) ]))

let current_datascript_query_sources () =
  let path = "test/datascript/upstream/query.cljc" in
  current_datascript_pull_sources ()
  @ [ (path, read_file (Filename.concat (repo_root ()) path)) ]

let test_current_datascript_query_compiles_for_native_and_melange () =
  let sources = current_datascript_query_sources () in
  ignore (compile_current_datascript Lg.Target.Native sources);
  ignore (compile_current_datascript Lg.Target.Melange sources)

let test_current_datascript_query_behaves_on_native () =
  let source =
    {|
(ns app.query-behavior
  (:require [datascript.db :as db]
            [datascript.query :as query]))

(def database
  (db/init-db
    [(db/datom 1 :name "Ivan")
     (db/datom 1 :age 19)
     (db/datom 2 :name "Oleg")]
    {}
    {}))

(println
  (= #{[1 "Ivan"] [2 "Oleg"]}
     (query/q
       '[:find ?e ?name
         :where [?e :name ?name]]
       database)))
|}
  in
  let sources =
    current_datascript_query_sources ()
    @ [ ("test/datascript/query_behavior.cljc", source) ]
  in
  let native_source = compile_current_datascript Lg.Target.Native sources in
  assert_ocaml_runs "current_datascript_query_behaves_on_native" "true\n"
    native_source;
  ignore (compile_current_datascript Lg.Target.Melange sources)

let current_datascript_serialize_sources () =
  let storage_source =
    {|
(ns datascript.storage)
(defn storage [_] nil)
|}
  in
  let path = "test/datascript/upstream/serialize.cljc" in
  [
    ("test/datascript/upstream/storage_stub.cljc", storage_source);
    (path, read_file (Filename.concat (repo_root ()) path));
  ]

let test_current_datascript_serialize_compiles_for_native_and_melange () =
  let sources = current_datascript_serialize_sources () in
  ignore (compile_current_datascript Lg.Target.Native sources);
  ignore (compile_current_datascript Lg.Target.Melange sources)

let test_current_datascript_serialize_roundtrips_on_native () =
  let source =
    {|
(ns app.serialize-behavior
  (:require [datascript.db :as db]
            [datascript.serialize :as serialize]))

(def database
  (db/init-db
    [(db/datom 1 :name "Ivan")
     (db/datom 1 :age 19)
     (db/datom 1 :active true)
     (db/datom 2 :name "Oleg")]
    {:name {:db/index true}}
    {}))

(def restored
  (-> database serialize/serializable serialize/from-serializable))

(println (= database restored))
(println (= 4 (count (:eavt restored))))
|}
  in
  let sources =
    current_datascript_serialize_sources ()
    @ [ ("test/datascript/serialize_behavior.cljc", source) ]
  in
  let native_source = compile_current_datascript Lg.Target.Native sources in
  assert_ocaml_runs "current_datascript_serialize_roundtrips_on_native"
    "true\ntrue\n" native_source;
  ignore (compile_current_datascript Lg.Target.Melange sources)

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

let test_javascript_targets_compile_error_classes () =
  let source =
    {|
#?(:cljs
   (do
     (def Exception js/Error)
     (def IllegalArgumentException js/Error)))
(defn fail-js []
  (throw (js/Error. "bad")))
(defn catches-js-error? []
  (try
    (fail-js)
    (catch js/Error _ true)))
|}
  in
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source
    |> expect_ok)

let test_cljs_writer_functions_compile () =
  let source =
    {|
(defn render-values [writer opts values]
  (pr-sequential-writer writer pr-writer "[" " " "]" opts values))
(deftype Item [^int value])
(defn render-items [writer opts values]
  (pr-sequential-writer
    writer
    (fn [item item-writer item-opts]
      (pr-writer (.-value item) item-writer item-opts))
    "[" " " "]" opts values))
(deftype Datom [^int e a v ^int tx])
(defprotocol IDatom
  (datom-tx [datom]))
(extend-type Datom
  IDatom
  (datom-tx [datom] (.-tx datom)))
(defn render-datoms [writer opts values]
  (pr-sequential-writer
    writer
    (fn [datom datom-writer datom-opts]
      (pr-sequential-writer
        datom-writer pr-writer "[" " " "]" datom-opts
        [(.-e datom) (.-a datom) (.-v datom) (datom-tx datom)]))
    "[" " " "]" opts values))
(defprotocol Items
  (-items [source]))
(deftype ItemSource [values]
  Items
  (-items [_] values))
(defn render-source [writer opts source]
  (pr-sequential-writer
    writer
    (fn [datom datom-writer datom-opts]
      (pr-writer (.-e datom) datom-writer datom-opts))
    "[" " " "]" opts (-items source)))
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

let test_get_supports_static_and_dynamic_transient_maps () =
  let source =
    {|
(ns app.transient-get
  (:require [#?(:cljs cljs.reader :clj clojure.edn) :as edn]))

(def static-values (assoc! (transient {}) "answer" 42))
(println
  (str (get static-values "answer") ":"
       (get static-values "missing" 9) ":"
       (nil? (get static-values "missing"))))

(def dynamic-values
  (assoc! (transient {}) (edn/read-string "[1 2]") "found"))
(println (get dynamic-values (edn/read-string "[1 2]") "missing"))

(defn group-by* [f init coll]
  (persistent!
    (reduce
      (fn [ret x]
        (let [k (f x)]
          (assoc! ret k (conj (get ret k init) x))))
      (transient {}) coll)))
(def groups
  (group-by* (fn [entry] entry) []
             [(edn/read-string "[1 2]")
              (edn/read-string "[1 2]")
              (edn/read-string "[3 4]")]))
(println
  (str (count (get groups (edn/read-string "[1 2]") [])) ":"
       (count (get groups (edn/read-string "[3 4]") []))))

(def expired (assoc! (transient {}) "answer" 42))
(persistent! expired)
(println
  (try
    (get expired "answer")
    "active"
    (catch (Invalid_argument _) "inactive")))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "get_supports_static_and_dynamic_transient_maps"
    "42:9:true\nfound\n2:1\ninactive\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_named_reducers_receive_contextual_accumulator_types () =
  let source =
    {|
(def kept-keys
  (persistent!
    (reduce-kv
      (fn keeper [acc key _]
        (conj! acc key))
      (transient [])
      (zipmap ["a" "b"] [1 2]))))
(def flattened
  (persistent!
    (reduce
      (fn outer [acc values]
        (reduce
          (fn inner [acc value]
            (conj! acc value))
          acc values))
      (transient [])
      [[1 2] [3]])))
(println (str (count kept-keys) ":" (pr-str flattened)))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "named_reducers_receive_contextual_accumulator_types"
    "2:[1 2 3]\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_count_supports_transient_collections () =
  let source =
    {|
(def vector-values (conj! (transient []) 1 2))
(def map-values (assoc! (transient {}) :a 1 :b 2))
(def set-values (conj! (transient (hash-set)) :a :b))
(println
  (str (count vector-values) ":"
       (count map-values) ":"
       (count set-values)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "count_supports_transient_collections" "2:2:2\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nth_supports_active_transient_vectors () =
  let source =
    {|
(def values (assoc! (conj! (transient [1]) 2 3) 0 9))
(println
  (str (nth values 0) ":"
       (nth values 1) ":"
       (nth values (dec (count values)))))
(persistent! values)
(println
  (try
    (nth values 0)
    "active"
    (catch (Invalid_argument _) "inactive")))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nth_supports_active_transient_vectors"
    "9:2:3\ninactive\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_volatile_transient_maps_specialize_from_vswap () =
  let source =
    {|
(defn build-index []
  (let [keywords (volatile! (transient []))
        index (volatile! (transient {}))
        write-keyword
          (fn [keyword]
            (or
              (get (deref index) keyword)
              (let [values (vswap! keywords conj! keyword)
                    position (dec (count values))]
                (vswap! index assoc! keyword position)
                position)))
        positions (mapv write-keyword [:a :b :a])]
    (= [0 1 0] positions)))
(println (build-index))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "volatile_transient_maps_specialize_from_vswap"
    "true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nth_narrows_dynamic_indexes_at_the_boundary () =
  let source =
    {|
(ns app.dynamic-nth
  (:require [#?(:cljs cljs.reader :clj clojure.edn) :as edn]))
(defn read-index [source] (edn/read-string source))
(defn generic-get [values index] (get values index))
(println (nth ["zero" "one"] (read-index "1")))
(println (nth ["zero" "one"] (generic-get [1] 0)))
(println
  (try
    (nth ["zero" "one"] (read-index "\"bad\""))
    "accepted"
    (catch (Invalid_argument _) "invalid")))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nth_narrows_dynamic_indexes_at_the_boundary"
    "one\none\ninvalid\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_var_quote_resolves_static_function_values () =
  let source =
    {|
(ns app.var-quote
  (:require [clojure.string :as str]))
(defn hidden [value] (+ value 1))
(def hidden-var #'hidden)
(println (hidden-var 41))
(println (#'str/upper-case "lg"))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "var_quote_resolves_static_function_values"
    "42\nLG\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_reader_conditional_accepts_metadata_branch_values () =
  let source =
    {|
(defn keep
  [#?(:cljs value
      :clj ^{:tag "[[Ljava.lang.Object;"} value)]
  value)
(println (keep 42))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "reader_conditional_accepts_metadata_branch_values" "42\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_metadata_map_prefixes_compile_without_java_types () =
  let source =
    {|
(def ^{:dynamic true :doc "Portable dynamic value"} *value* 41)
(defn add-hinted
  [value ^{:tag "[[Ljava.lang.Object;"} hinted]
  (+ value hinted))
(println (add-hinted *value* 1))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "metadata_map_prefixes_compile_without_java_types" "42\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_java_is_array_idiom_lowers_to_portable_array_predicate () =
  let source =
    {|
(defn host-array? [^:dynamic value]
  (.isArray (.getClass ^Object value)))
(println (host-array? (make-array 1)))
(println (host-array? [1]))
(println (host-array? "value"))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "java_is_array_idiom_lowers_to_portable_array_predicate"
    "true\nfalse\nfalse\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dotimes_evaluates_bounds_once_and_returns_nil () =
  let source =
    {|
(def evaluations (atom 0))
(def runs (atom 0))
(def values (make-array 3))
(defn limit []
  (swap! evaluations inc)
  3)
(def result
  (dotimes [index (limit)]
    (aset values index index)))
(dotimes [_ 0]
  (swap! runs inc))
(dotimes [_ -2]
  (swap! runs inc))
(println
  (str
    (= 0 (aget values 0)) ":"
    (= 1 (aget values 1)) ":"
    (= 2 (aget values 2)) ":"
    (= 1 @evaluations) ":"
    (= 0 @runs) ":"
    (nil? result)))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dotimes_evaluates_bounds_once_and_returns_nil"
    "true:true:true:true:true:true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_aget_supports_dynamic_arrays_with_inferred_indexes () =
  let source =
    {|
(ns app.dynamic-aget
  (:require [#?(:cljs cljs.reader :clj clojure.edn) :as edn]))

(defn pick [values indexes]
  (if (.isArray (.getClass ^Object values))
    (aget values (unsafe-aget indexes 0))
    nil))
(def values (make-array 2))
(def indexes (array-from [1]))
(aset values (first (edn/read-string "[0]")) "zero")
(aset values 1 "one")
(println (pick values indexes))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "aget_supports_dynamic_arrays_with_inferred_indexes" "one\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_persistent_transient_map_is_seqable () =
  let source =
    {|
(def empty-map (persistent! (transient {})))
(def present-map (persistent! (assoc! (transient {}) :answer 42)))
(println
  (str (nil? (not-empty empty-map)) ":"
       (some? (not-empty present-map))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "persistent_transient_map_is_seqable" "true:true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dynamic_transient_vector_accepts_static_values () =
  let source =
    {|
(defrecord Frame [^:transient-vector acc])
(defn add-value [^Frame frame]
  (Frame. (conj! (.-acc frame) 42)))
(def values
  (persistent! (.-acc (add-value (Frame. (transient []))))))
(println (pr-str values))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_transient_vector_accepts_static_values" "[42]\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dynamic_transient_map_accepts_static_entries () =
  let source =
    {|
(defrecord Frame [^:transient-map acc])
(defn add-entry [^Frame frame]
  (Frame. (assoc! (.-acc frame) :answer 42)))
(def values
  (persistent! (.-acc (add-entry (Frame. (transient {}))))))
(println (get values :answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_transient_map_accepts_static_entries" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_defrecord_preserves_transient_vector_shape () =
  let source =
    {|
(defprotocol Collector
  (-add [collector value]))
(defrecord CollectorFrame [^:transient-vector acc]
  Collector
  (-add [_ ^:dynamic value]
    (CollectorFrame. (conj! acc value))))
(def frame (CollectorFrame. (transient [])))
(def values (persistent! (.-acc (-add frame 1))))
(println (pr-str values))
  |}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "defrecord_preserves_transient_vector_shape" "[1]\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_defrecord_preserves_transient_map_shape () =
  let source =
    {|
(defprotocol Collector
  (-add [collector key value]))
(defrecord CollectorFrame [^:transient-map acc]
  Collector
  (-add [_ ^:dynamic key ^:dynamic value]
    (CollectorFrame. (assoc! acc key value))))
(def frame (CollectorFrame. (transient {})))
(def values (persistent! (.-acc (-add frame :answer 42))))
(println (get values :answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "defrecord_preserves_transient_map_shape" "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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
(defn combine-first-row [columns rows]
  (mapv (fn [column value] (+ column value)) columns (first rows)))
(defn add-index [values]
  (mapv (fn [value index] (+ value index)) values (range)))
(defn present-indexes [values]
  (remove nil? (map (fn [value index] (when value index)) values (range))))
(println
  (str (pr-str (combine [1 2 3] [10 20])) ":"
       (pr-str (map (fn [x y] (- x y)) [10 20 30] [1 2])) ":"
       (pr-str (add-index [10 20])) ":"
       (pr-str (map inc (present-indexes [true false true])))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "map_and_mapv_accept_multiple_collections"
    "[11 22]:(9 18):[10 21]:(1 3)\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_map_vector_preserves_heterogeneous_vectors () =
  let source =
    {|
(defn index-of [pred xs]
  (some (fn [[value index]] (when (pred value) index))
        (map vector xs (range))))
(if-some [index (index-of (fn [value] (= value :name)) [:other :name])]
  (println (+ index 1))
  (println -1))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "map_vector_preserves_heterogeneous_vectors" "2\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_forward_declared_functions_work_as_collection_callbacks () =
  let source =
    {|
(deftype Item [^int value])
(declare touch)
(defn touch-all [values]
  (map touch values))
(defn ^Item touch [^Item value]
  (do
    (count (touch-all []))
    value))
(def result (touch-all [(Item. 1) (Item. 2)]))
(println (str (count result) ":" (.-value ^Item (first result))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "forward_declared_functions_work_as_collection_callbacks"
    "2:1\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_conditional_records_pack_opaque_fields_at_dynamic_boundary () =
  let source =
    {|
(declare attrs-frame)
(defrecord ResultFrame [value])
(defrecord AttrsFrame [^:transient-map acc value])
(defn ref-frame [flag]
  (if flag
    (ResultFrame. 1)
    (attrs-frame 2)))
(defn attrs-frame [value]
  (AttrsFrame. (transient {}) value))
(println
  (str (instance? ResultFrame (ref-frame true)) ":"
       (instance? AttrsFrame (ref-frame false))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "conditional_records_pack_opaque_fields_at_dynamic_boundary"
    "true:true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dynamic_nominal_records_preserve_nested_nominal_fields () =
  let source =
    {|
(defrecord Inner [^:transient-map data])
(defrecord Outer [^Inner inner])
(defn nested-is-inner? [^:dynamic value]
  (instance? Inner (:inner value)))
(println (nested-is-inner? (Outer. (Inner. (transient {})))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_nominal_records_preserve_nested_nominal_fields"
    "true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dynamic_vector_literal_packs_anonymous_record_elements_directly () =
  let source =
    {|
(defn accept [^:dynamic value] (count value))
(println (accept [{:_friend [:db/id]}]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "dynamic_vector_literal_packs_anonymous_record_elements_directly" "1\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_equality_packs_vectors_with_nested_dynamic_elements () =
  let source =
    {|
(def actual (mapv :name [{:name "Ivan"} {}]))
(println (= ["Ivan" nil] actual))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "equality_packs_vectors_with_nested_dynamic_elements"
    "true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_destructured_row_parameter_stays_structural () =
  let source =
    {|
(defrecord Context [db visitor])
(defn read-visitor
  ([] (read-visitor nil))
  ([{:keys [visitor]}] visitor))
(defn pass-opts [opts] (read-visitor opts))
|}
  in
  let state = typecheck_state source in
  (match Lg.Compiler_environment.find_opt "pass-opts" state.env with
  | Some
      {
        ty =
          Lg.Types.TFn
            ([ Lg.Types.TNullable (Lg.Types.TRecord fields) ], _);
        _;
      }
    when Option.is_some (Lg.Types.find_field ":visitor" fields) ->
      ()
  | Some binding ->
      failwith
        ("destructured row became " ^ Lg.Types.source_name binding.ty)
  | None -> failwith "missing pass-opts binding");
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_forward_declared_multi_arity_functions_initialize_lazily () =
  let source =
    {|
(declare choose)
(defn ^int call-choose [^int value]
  (choose value))
(defn ^int call-choose-default [^int value ^int fallback]
  (choose value fallback))
(defn ^int choose
  ([^int value] value)
  ([^int value ^int fallback] (+ value fallback)))
(println "ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "forward_declared_multi_arity_functions_initialize_lazily"
    "ok\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_forward_declared_mutual_recursion_reuses_stabilized_signatures () =
  let source =
    {|
(declare dispatch step)
(defn dispatch
  ([value] (dispatch value 0))
  ([value total]
    (if (= value 0)
      total
      (step (dec value) (inc total)))))
(defn step [value total] :int
  (dispatch value total))
(println (dispatch 3))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "forward_declared_mutual_recursion_reuses_stabilized_signatures" "3\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_fnil_wraps_core_conj_with_default_collection () =
  let source =
    {|
(def conjv (fnil conj []))
(def conjs (fnil conj #{}))
(deftype Entry [^int value])
(defn choose [flag] (if flag (Entry. 1) :entry))
(def one (choose true))
(def existing (if true [one] nil))
(def entries (conjv existing (Entry. 2)))
(defn sum-entry [total ^Entry entry]
  (+ total (.-value entry)))
(println
  (str (= [1] (conjv nil 1)) ":" (= #{1} (conjs nil 1)) ":"
       (count entries) ":" (reduce sum-entry 0 [one])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "fnil_wraps_core_conj_with_default_collection"
    "true:true:2:1\n" ocaml_source

let test_dynamic_protocol_witnesses_unpack_common_returns () =
  let source =
    {|
(defprotocol IFlag
  (-flag [this]))
(deftype Flagged [^int id]
  IFlag
  (-flag [_] true))
(defn choose-flagged [pick]
  (if pick (Flagged. 0) :missing))
(def flagged-items [(choose-flagged true)])
(defn count-flags [total item]
  (if (-flag item) (+ total 1) total))
(def result (reduce count-flags 0 flagged-items))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  if
    not
      (string_contains_substring ocaml_source
         "Lg_runtime.Runtime_dynamic.as_bool")
  then
    failwith "dynamic protocol witnesses must unpack their common return type";
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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
  (aclone source))
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
    "3:7:2:5:3\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_to_array_is_first_class_for_dynamic_seqable_values () =
  let source =
    {|
(ns app.first-class-to-array
  (:require [#?(:cljs cljs.reader :clj clojure.edn) :as edn]))

(defn rows->arrays [rows]
  (let [data (filter (fn [row] (some? row)) rows)]
    (mapv to-array data)))
(def arrays (rows->arrays (edn/read-string "[[1 2] [3]]")))
(println (pr-str (mapv (fn [values] (alength values)) arrays)))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "to_array_is_first_class_for_dynamic_seqable_values"
    "[2 1]\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_array_arguments_adapt_nullable_elements () =
  let source =
    {|
(defn first-present [values]
  (when-some [value (aget values 0)]
    value))
(println (= 42 (first-present (to-array [42]))))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "array_arguments_adapt_nullable_elements" "true\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_optional_protocol_values_can_flow_to_seqable_else_branches () =
  let source =
    {|
(defprotocol Searchable
  (search-count [source]))
(deftype IndexedSource [value])
(extend-type IndexedSource
  Searchable
  (search-count [_] 7))

(defn seq-count [values]
  (count (filter (fn [_] true) values)))
(defn source-count [source]
  (if (satisfies? Searchable source)
    (search-count source)
    (seq-count source)))

(println (+ 0 (source-count (IndexedSource. 0))))
(println (+ 0 (source-count [1 2 3])))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "optional_protocol_values_can_flow_to_seqable_else_branches" "7\n3\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nested_protocol_witnesses_keep_concrete_receiver_storage () =
  let source =
    {|
(defprotocol Searchable
  (-search [db pattern]))
(defprotocol IndexAccess
  (-datoms [db index]))
(defprotocol Database
  (-attrs [db property]))

(type-record db (value :int))
(extend-type db
  Searchable
  (-search [db pattern] (+ (:value db) pattern))
  IndexAccess
  (-datoms [db index] (+ (:value db) index))
  Database
  (-attrs [db property] (+ (:value db) property)))

(defn resolve-value [db value]
  (+ (-search db value)
     (-datoms db value)
     (-attrs db value)))

(def value (record db (value 1)))
(println (resolve-value value 2))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "nested_protocol_witnesses_keep_concrete_receiver_storage" "9\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_weak_references_support_typed_cache_values () =
  let source =
    {|
(type-record cached-value (number :int))
(type-record cache (entry :weak<cached-value>))
(defn make-cache-entry [value] (weak-ref value))
(defn read-cache-entry [reference] (weak-deref reference))
(def value (record cached-value (number 42)))
(def cache-value (record cache (entry (make-cache-entry value))))
(println
  (match (read-cache-entry (:entry cache-value))
    (Some cached) (:number cached)
    None 0))
(weak-clear! (:entry cache-value))
(println (nil? (read-cache-entry (:entry cache-value))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "weak_references_support_typed_cache_values" "42\ntrue\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source
    |> expect_ok)

let test_weak_references_reject_invalid_calls () =
  Lg.Compiler.compile_string {|(weak-ref)|}
  |> expect_error_contains "weak-ref expects 1 argument";
  Lg.Compiler.compile_string {|(weak-ref 42)|}
  |> expect_error_contains "weak-ref expects a heap value";
  Lg.Compiler.compile_string {|(weak-deref 42)|}
  |> expect_error_contains "weak-deref expects a weak reference";
  Lg.Compiler.compile_string {|(weak-clear! 42)|}
  |> expect_error_contains "weak-clear! expects a weak reference"

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

let test_special_float_literals_are_portable () =
  let source =
    {|
(println
  (str (> ##Inf 1.0) ":"
       (< ##-Inf -1.0) ":"
       (not= ##NaN ##NaN) ":"
       (number? ##Inf)))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "special_float_literals_are_portable"
    "true:true:true:true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_numeric_equality_accepts_dynamic_ints_and_floats () =
  let source =
    {|
(defn infinite? [^:dynamic value]
  (== ##Inf value))

(println (infinite? 1))
(println (infinite? ##Inf))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "numeric_equality_accepts_dynamic_ints_and_floats"
    "false\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_multi_arity_defn_accepts_nil_for_destructured_options () =
  let source =
    {|
(defn parse-options
  ([^:int value] (parse-options value nil))
  ([^:int value {:keys [visitor]}]
   (if-some [visitor visitor]
     (visitor value)
     value)))
(println
  (str (parse-options 1) ":"
       (parse-options 2 {:visitor (fn [^:int value] (+ value 1))})))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "multi_arity_defn_accepts_nil_for_destructured_options"
    "1:3\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nullable_destructured_options_flow_through_forwarding_functions () =
  let source =
    {|
(defn parse-options
  ([^:int value] (parse-options value nil))
  ([^:int value {:keys [visitor]}]
   (if-some [visitor visitor] (visitor value) value)))
(defn forward-options [^:int value options]
  (parse-options value options))
(println (parse-options 1 nil))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "nullable_destructured_options_flow_through_forwarding_functions"
    "1\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_multi_arity_calls_project_structural_row_arguments () =
  let source =
    {|
(defn choose
  ([^:int value] value)
  ([^:int value opts]
   (if-some [replacement (:replacement opts)]
     (+ replacement 0)
     value)))
(defn forward [^:int value opts]
  (choose value opts))
(println (str (forward 1 {:replacement (Some 2)}) ":"
              (forward 3 {:replacement nil})))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "multi_arity_calls_project_structural_row_arguments"
    "2:3\n" ocaml_source

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

let test_satisfies_question_selects_each_generic_protocol_witness () =
  let source =
    {|
(defprotocol First (first-value [value] :int))
(defprotocol Second (second-value [value] :int))
(extend-type :int
  First (first-value [value] value)
  Second (second-value [value] value))
(defn both? [value]
  (and (satisfies? First value)
       (satisfies? Second value)))
(println (str (both? 7) ":" (both? "seven")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "satisfies_question_selects_each_generic_protocol_witness"
    "true:false\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_protocol_methods_use_their_static_receiver_witnesses () =
  let source =
    {|
(defprotocol Labelled (label [value] :string))
(defn labelled? [value] (satisfies? Labelled value))
(deftype Item [^String value]
  Labelled
  (label [item]
    (if (labelled? item) "ready" "missing")))
(println (label (Item. "ignored")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "protocol_methods_use_their_static_receiver_witnesses"
    "ready\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_extend_type_methods_use_their_static_receiver_witnesses () =
  let source =
    {|
(type-record item-value (value :string))
(defprotocol Labelled (label [value] :string))
(defn labelled? [value] (satisfies? Labelled value))
(extend-type item-value
  Labelled
  (label [item]
    (if (labelled? item) (:value item) "missing")))
(def item (record item-value (value "ready")))
(println (label item))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "extend_type_methods_use_their_static_receiver_witnesses"
    "ready\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_generic_protocol_witness_packs_seqable_arguments () =
  let source =
    {|
(defprotocol Search
  (-search [database pattern] :int))
(defn fsearch [database pattern]
  (-search database pattern))
(defrecord SearchDb [token]
  Search
  (-search [_database pattern]
    (count pattern)))
(def database (SearchDb. 0))
(defn search-size [database]
  (Search/-search database (mapv identity [1 "two"])))
(println (search-size database))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_protocol_witness_packs_seqable_arguments" "2\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_loop_bindings_accept_prefix_type_hints () =
  let source =
    {|
(deftype LoopBox [value])
(def result
  (loop [remaining 2
         ^LoopBox box (LoopBox. 0)]
    (if (zero? remaining)
      box
      (recur (dec remaining) (LoopBox. (+ (.-value box) 1))))))
(println (.-value result))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "loop_bindings_accept_prefix_type_hints" "2\n"
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

let test_forward_declared_functions_refresh_nominal_returns () =
  let source =
    {|
(deftype Datom [^int value])
(declare resolve-datom components->pattern)
(defn make-datom ^Datom []
  (Datom. 42))
(defn resolve-datom []
  (let [_ (list 'resolve-datom)]
    (make-datom)))
(defn components->pattern []
  (resolve-datom))
|}
  in
  let state = typecheck_state source in
  [ "resolve-datom"; "components->pattern" ]
  |> List.iter (fun name ->
         match Lg.Compiler_environment.find_opt name state.env with
         | Some { ty = Lg.Types.TFn (_, Lg.Types.TNamed_record record); _ }
           when record.type_name = "datom" ->
             ()
         | Some binding ->
             failwith
               (name ^ " retained " ^ Lg.Types.source_name binding.ty)
         | None -> failwith ("missing definition for " ^ name))

let test_quoted_symbols_do_not_create_recursive_dependencies () =
  let quoted_name =
    Lg.Ast.FList
      [ Lg.Ast.FSymbol "quote"; Lg.Ast.FSymbol "resolve-datom" ]
  in
  if
    Lg.Top_level_elaborator.function_is_recursive "" "resolve-datom"
      [ quoted_name ]
  then failwith "quoted symbols are data, not recursive calls"

let test_forward_declaration_detection_includes_overload_targets () =
  let binding =
    Lg.Types.binding ~forward_declared:true
      ~overload_targets:[ "item_at__arity_2_0" ] "item_at"
      (Lg.Types.TOverloaded_fn
         [
           {
             fixed_params = [ Lg.Types.TInt ];
             rest_param = None;
             return_ty = Lg.Types.TInt;
           };
         ])
  in
  let env =
    Lg.Compiler_environment.add "item-at" binding
      Lg.Compiler_environment.empty
  in
  let expression =
    Lg.Semantic_ir.Apply
      (Lg.Semantic_ir.Ident "item_at__arity_2_0", [ Lg.Semantic_ir.Int 0 ])
  in
  if
    not
      (Lg.Top_level_elaborator.expression_references_declaration env expression)
  then failwith "overload targets must retain forward declaration evidence"

let test_incremental_declarations_refresh_protocol_method_returns () =
  let source =
    {|
(deftype Item [^int value])
(declare make-item)
(defprotocol Items
  (-items [source]))
(deftype Source [^int unused]
  Items
  (-items [_]
    [(make-item)]))
(defn source-item [source]
  (first (-items source)))
(defn make-item ^Item []
  (let [_ (source-item (Source. 0))]
    (Item. 42)))
|}
  in
  let state, _ =
    Lg.Compiler.compile_chunk Lg.Compiler.empty_state source |> expect_ok
  in
  let env = state.typecheck_state.env in
  match Lg.Protocol.find_protocol_id "" env "Items" with
  | None -> failwith "missing Items protocol"
  | Some protocol_id -> (
      match Lg.Protocol.common_method_return env protocol_id "-items" with
      | Some (Lg.Types.TVector (Lg.Types.TNamed_record record))
        when record.type_name = "item" ->
          ()
      | Some return_ty ->
          failwith
            ("incremental protocol return remained "
           ^ Lg.Types.source_name return_ty)
      | None -> failwith "missing Items protocol implementation return")

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

let test_defrecord_inferred_generic_fields_preserve_value_types () =
  let source =
    {|
(type-record box [value]
  (item :value))
(defn box-item [box]
  (:item box))
(defprotocol Boxed
  (read-box [this]))
(defrecord Holder [box]
  Boxed
  (read-box [_]
    (box-item box)))
(defn holder-item [^Holder holder ^:keyword key]
  (:item (get holder key)))
(def int-holder (Holder. (record box (item 42))))
(def string-holder (Holder. (record box (item "answer"))))
(println
  (str (holder-item int-holder :box) ":"
       (holder-item string-holder :box)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "defrecord_inferred_generic_fields_preserve_value_types"
    "42:answer\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_defrecord_methods_infer_every_structural_generic_field () =
  let source =
    {|
(type-record box [value]
  (compare-values :fn<value;value;int>))
(defn compare-box [box left right]
  ((:compare-values box) left right))
(defprotocol Scored
  (score [this]))
(defrecord Triple [first second third]
  Scored
  (score [_]
    (+ (compare-box first 1 2)
       (compare-box second 2 3)
       (compare-box third 3 4))))
(defn int-box []
  (record box
    (compare-values (fn [left right] (compare left right)))))
(def triple (Triple. (int-box) (int-box) (int-box)))
(println (score triple))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "defrecord_methods_infer_every_structural_generic_field"
    "-3\n" ocaml_source;
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

let test_equality_dispatches_to_record_iequiv () =
  let source =
    {|
(defrecord EquivItem [^int id ^String cache]
  IEquiv
  (-equiv [left right]
    (= (.-id left) (.-id ^EquivItem right))))
(println (= (EquivItem. 1 "left") (EquivItem. 1 "right")))
(println (= (EquivItem. 1 "left") (EquivItem. 2 "left")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "equality_dispatches_to_record_iequiv" "true\nfalse\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_record_iequiv_refreshes_forward_protocol_dependencies () =
  let source =
    {|
(defprotocol Identified
  (-id [value] :int))
(declare equivalent-item?)
(defrecord EquivItem [^int id ^String cache]
  IEquiv
  (-equiv [left right]
    (equivalent-item? left right))
  Identified
  (-id [item]
    (.-id item)))
(defn equivalent-item? [left right]
  (= (-id left) (-id right)))
(println (= (EquivItem. 1 "left") (EquivItem. 1 "right")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "record_iequiv_refreshes_forward_protocol_dependencies"
    "true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_defrecord_host_methods_support_declared_helpers () =
  let source =
    {|
(defmacro defrecord-updatable [name fields & implementations]
  `(do
     (defrecord ~name ~fields)
     (extend-type ~name ~@implementations)))
(declare wrapper-count)
(defrecord-updatable Wrapper [items]
  ICounted
  (-count [wrapper]
    (wrapper-count wrapper)))
(defn wrapper-count [_wrapper]
  3)
|}
  in
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

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
(type-record address-node
  (addresses :array<option<int64>>))
(type-record address-bundle [value]
  (values :array<value>)
  (addresses :array<option<int64>>))
(defn pair-arrays [left right]
  (let [left-length (Array.length left)
        right-length (Array.length right)
        combined-at
        (fn [index]
          (if (< index left-length)
            (aget left index)
            (aget right (- index left-length))))]
    (if (= 0 (+ left-length right-length))
      (array left right)
      (array
        (Array.make left-length (combined-at 0))
        (Array.make right-length (combined-at left-length))))))
(defn pair-address-nodes [left right]
  (let [paired (pair-arrays (:addresses left) (:addresses right))]
    (array
      (record address-node (addresses (aget paired 0)))
      (record address-node (addresses (aget paired 1))))))
(defn pair-address-bundles [left right]
  (let [paired-values
        (pair-arrays (:values left) (:values right))
        paired-addresses
        (pair-arrays (:addresses left) (:addresses right))]
    (array
      (record address-bundle
        (values (aget paired-values 0))
        (addresses (aget paired-addresses 0)))
      (record address-bundle
        (values (aget paired-values 1))
        (addresses (aget paired-addresses 1))))))
(def present-node
  (record address-node
    (addresses (Array.make 1 (Some (Int64.of_int 7))))))
(def missing-node
  (record address-node
    (addresses (Array.make 1 None))))
(def empty-node
  (record address-node
    (addresses (Array.make 0 None))))
(def paired-values
  (pair-arrays (array 1) (array 2)))
(def paired-options
  (pair-arrays (array (Some "left")) (array None)))
(def paired-nodes (pair-address-nodes present-node missing-node))
(def paired-empty (pair-address-nodes empty-node empty-node))
(def paired-bundles
  (pair-address-bundles
    (record address-bundle
      (values (array 1))
      (addresses (:addresses present-node)))
    (record address-bundle
      (values (array 2))
      (addresses (:addresses missing-node)))))
(println
  (str (aget (aget paired-values 0) 0) ":"
       (some? (aget (aget paired-options 0) 0)) ":"
       (some? (aget (:addresses (aget paired-nodes 0)) 0)) ":"
       (nil? (aget (:addresses (aget paired-nodes 1)) 0)) ":"
       (some? (aget (:addresses (aget paired-bundles 0)) 0)) ":"
       (nil? (aget (:addresses (aget paired-bundles 1)) 0)) ":"
       (Array.length (:addresses (aget paired-empty 0)))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_collection_returns_preserve_concrete_element_types"
    "1:true:true:true:true:true:0\n" ocaml_source;
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

let test_apply_pr_accepts_lazy_sequences () =
  let source =
    {|
(defn write-values [^java.io.Writer writer values]
  (binding [*out* writer]
    (apply pr
      (map
        (fn [[e a v tx]] [e a v tx])
        values))))
(deftype Person [items])
(defmethod print-method Person [^Person person ^java.io.Writer writer]
  (.write writer "#person ")
  (write-values writer (.-items person)))
(println (pr-str (Person. [[1 :name "Ada" 2]])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "apply_pr_accepts_lazy_sequences"
    "#person [1 :name \"Ada\" 2]\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_apply_pr_accepts_refined_protocol_sequences () =
  let source =
    {|
(defprotocol Items
  (-items [source]))
(defprotocol Schema
  (-schema [source]))
(deftype Item [^int value])
(defn print-items [source, ^java.io.Writer writer]
  (binding [*out* writer]
    (pr (-schema source))
    (apply pr
      (map (fn [^Item item] [(.-value item)])
        (-items source)))))
(deftype DirectSource [^int unused]
  Items
  (-items [_] (Some (seq [(Item. 42)])))
  Schema
  (-schema [_] {}))
(deftype FilteredSource [source]
  Items
  (-items [_]
    (filter (fn [^Item item] (pos? (.-value item)))
      (-items source)))
  Schema
  (-schema [_] (-schema source)))
(defn print-direct [^DirectSource source, ^java.io.Writer writer]
  (print-items source writer))
(defn print-filtered [^FilteredSource source, ^java.io.Writer writer]
  (print-items source writer))
|}
  in
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_protocol_methods_merge_concrete_and_dynamic_sequence_returns () =
  let source =
    {|
(defprotocol Items
  (-items [this]))
(deftype Item [^int value])
(deftype StaticSource [^int unused])
(defrecord DynamicSource [^:dynamic items])
(extend-type StaticSource Items
  (-items [_]
    [(Item. 42)]))
(extend-type DynamicSource Items
  (-items [this]
    (filter (fn [_] true) (:items this))))
(defn collect-values [source]
  (map
    (fn [^Item item] (.-value item))
    (-items source)))
(def static-value (StaticSource. 0))
(def dynamic-value (DynamicSource. [(Item. 7)]))
(println (pr-str (collect-values static-value)))
(println (pr-str (collect-values dynamic-value)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "protocol_methods_merge_concrete_and_dynamic_sequence_returns"
    "(42)\n(7)\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_recursive_protocol_sequence_returns_remain_concrete () =
  let source =
    {|
(type-record sorted [value]
  (values :array<value>)
  (cmp :fn<value;value;int>))
(deftype Item [^int value])
(defn set-comparator [set]
  (:cmp set))
(defn set-slice-with [set from to cmp]
  (let [cmp (:cmp set)]
    (Some
      (filter
        (fn [item]
          (and (<= (cmp from item) 0)
               (<= (cmp item to) 0)))
        (array-seq (:values set))))))
(defn slice
  ([set from to]
   (slice set from to (set-comparator set)))
  ([set from to cmp]
   (set-slice-with set from to cmp)))
(defprotocol IIndex
  (-items [db]))
(defrecord DB [^sorted index]
  IIndex
  (-items [_]
    (slice index (Item. 0) (Item. 10))))
(defrecord FilteredDB [db pred]
  IIndex
  (-items [_]
    (filter pred (-items db))))
(defn validate-items [source]
  (not-empty (-items source)))
(defn touch-db [^DB db]
  (validate-items db)
  nil)
(def db
  (DB.
    (record sorted
      (values (array (Item. 1) (Item. 2)))
      (cmp (fn [left right]
             (compare (.-value left) (.-value right)))))))
(touch-db db)
(println "ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "recursive_protocol_sequence_returns_remain_concrete" "ok\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_recursive_protocol_vectors_keep_static_protocol_elements () =
  let source =
    {|
(defprotocol IFrame
  (-run [this]))
(defrecord ResultFrame [^int value]
  IFrame
  (-run [this]
    [(ResultFrame. value)]))
(defrecord PairFrame [^int value]
  IFrame
  (-run [this]
    [(ResultFrame. value) (PairFrame. value)]))
(defn run-frame [frame]
  (-run frame))
(println (count (run-frame (PairFrame. 7))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "recursive_protocol_vectors_keep_static_protocol_elements" "2\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_recursive_protocol_frame_stacks_preserve_dispatch_witnesses () =
  let source =
    {|
(defprotocol IFrame
  (-run [this]))
(defrecord ResultFrame [^int value]
  IFrame
  (-run [_] []))
(defrecord PairFrame [^int value]
  IFrame
  (-run [_] [(ResultFrame. value)]))
(defn first-seq [values] (first values))
(defn next-seq [values] (next values))
(defn conj-seq [values value]
  (if-some [values values]
    (conj values value)
    (list value)))
(defn run-stack []
  (loop [stack (list (PairFrame. 7))]
    (let [frame (first-seq stack)
          stack' (next-seq stack)]
      (if (not (instance? ResultFrame frame))
        (recur (reduce conj-seq stack' (-run frame)))
        (.-value ^ResultFrame frame)))))
(println (run-stack))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "recursive_protocol_frame_stacks_preserve_dispatch_witnesses" "7\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dynamic_values_preserve_partial_protocol_implementations () =
  let source =
    {|
(defprotocol IFrame
  (-merge [this result])
  (-run [this]))
(defrecord RunFrame []
  IFrame
  (-run [_] 42))
(defn run-dynamic [^:dynamic frame]
  (-run frame))
(println (run-dynamic (RunFrame.)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_values_preserve_partial_protocol_implementations"
    "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dynamic_protocol_results_preserve_dispatch_witnesses () =
  let source =
    {|
(defprotocol IFrame
  (-next [this] :dynamic)
  (-value [this]))
(defrecord Frame [^int value]
  IFrame
  (-next [_] (Frame. (inc value)))
  (-value [_] value))
(defn next-dynamic [^:dynamic frame]
  (-next frame))
(defn value-dynamic [^:dynamic frame]
  (-value frame))
(println (value-dynamic (next-dynamic (Frame. 41))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_protocol_results_preserve_dispatch_witnesses"
    "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dynamic_record_assoc_preserves_updated_nominal_value () =
  let source =
    {|
(defrecord PullAttr [name default])
(defn add-default [^:dynamic attr]
  (assoc attr :default "fallback"))
(defn read-default [^PullAttr attr]
  (:default attr))
(println (read-default (add-default (PullAttr. :missing nil))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_record_assoc_preserves_updated_nominal_value"
    "fallback\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dynamic_boundaries_preserve_next_nil_semantics () =
  let source =
    {|
(defn nil-dynamic? [^:dynamic value]
  (nil? value))
(defn tail [values]
  (next values))
(defn nullable-dynamic-loop [flag]
  (loop [current (if flag nil (if true (list 1) [1]))]
    (if flag
      (nil? current)
      (recur current))))
(println (nil-dynamic? (next [1])))
(println (nil-dynamic? (next [1 2])))
(println (nil-dynamic? (tail [1])))
(println (nil-dynamic? (tail (list 1))))
(println
  (loop [remaining (if true (list 1) [1])
         step 0]
    (if (= step 1)
      (nil? remaining)
      (recur (next remaining) 1))))
(println (nullable-dynamic-loop true))
(println
  (loop [remaining (if true (seq []) nil)
         step 0]
    (if (= step 1)
      (nil? remaining)
      (recur (tail remaining) 1))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_boundaries_preserve_next_nil_semantics"
    "true\nfalse\ntrue\ntrue\ntrue\ntrue\ntrue\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_typed_maps_compare_dynamic_vector_keys_structurally () =
  let source =
    {|
(deftype Cache [^clojure.lang.Associative entries])
(defn put-entry [^Cache cache key value]
  (Cache. (assoc (.-entries cache) key value)))
(defn get-entry [^Cache cache key]
  (get (.-entries cache) key))
(def first-key (if true [:name :tags] (list :name)))
(def second-key (if true [[:missing :default "fallback"]] (list :missing)))
(def cache
  (put-entry (put-entry (Cache. {}) first-key 1) second-key 2))
(println (get-entry cache first-key))
(println (get-entry cache second-key))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_maps_compare_dynamic_vector_keys_structurally"
    "1\n2\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_direct_val_at_uses_dynamic_map_comparator () =
  let source =
    {|
(deftype Cache [^clojure.lang.Associative entries]
  clojure.lang.ILookup
  (valAt [_ key] (.valAt entries key))
  (valAt [_ key not-found] (.valAt entries key not-found)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  if
    not
      (string_contains_substring ocaml_source
         "Lg_runtime.Runtime_map.get_option_dynamic")
  then failwith "direct valAt should use the dynamic map comparator";
  if
    not
      (string_contains_substring ocaml_source
         "Lg_runtime.Runtime_map.get_option_default_dynamic")
  then failwith "direct valAt with a default should use the dynamic map comparator";
  let melange_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok
  in
  if
    not
      (string_contains_substring melange_source
         "Lg_runtime.Runtime_map.get_option_dynamic")
  then failwith "Melange direct valAt should use the dynamic map comparator"

let test_recursive_protocol_vectors_materialize_optional_unknown_elements () =
  let source =
    {|
(declare next-frame)
(defprotocol IFrame
  (-run [this]))
(defrecord ResultFrame [^int value]
  IFrame
  (-run [_] []))
(defrecord PairFrame [^int value ^:transient-vector acc]
  IFrame
  (-run [this]
    [this (next-frame value)]))
(defn next-frame [value]
  (if true (ResultFrame. value) nil))
(println (count (-run (PairFrame. 7 (transient [])))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "recursive_protocol_vectors_materialize_optional_unknown_elements"
    "2\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_loop_protocol_vectors_widen_heterogeneous_elements_locally () =
  let source =
    {|
(declare next-frame)
(defprotocol IFrame
  (-run [this]))
(defrecord ResultFrame [^int value]
  IFrame
  (-run [_] []))
(defrecord LoopFrame [^int value]
  IFrame
  (-run [this]
    (loop [step value]
      (cond
        (= step 0) [(ResultFrame. value)]
        (= step 1) [this (next-frame value)]
        :else (recur 0)))))
(defn next-frame [value]
  (if true (ResultFrame. value) nil))
(println (count (-run (LoopFrame. 0))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "loop_protocol_vectors_widen_heterogeneous_elements_locally" "1\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_cond_protocol_vectors_widen_heterogeneous_elements_locally () =
  let source =
    {|
(declare next-frame)
(defprotocol IFrame
  (-run [this]))
(defrecord ResultFrame [^int value]
  IFrame
  (-run [_] []))
(defrecord CondFrame [^int value]
  IFrame
  (-run [this]
    (cond
      (= value 0) [(ResultFrame. value)]
      :else [this (next-frame value)])))
(defn next-frame [value]
  (if true (ResultFrame. value) nil))
(println (count (-run (CondFrame. 0))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "cond_protocol_vectors_widen_heterogeneous_elements_locally" "1\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_if_nullable_protocol_vectors_widen_elements_locally () =
  let source =
    {|
(declare next-frame)
(defprotocol IFrame
  (-run [this]))
(defrecord ResultFrame [^int value])
(defrecord IfFrame [^int value]
  IFrame
  (-run [this]
    (if (= value 0)
      [(ResultFrame. value)]
      (if (= value 1)
        [this (next-frame value)]
        nil))))
(defn next-frame [value]
  (if true (ResultFrame. value) nil))
(defn frame-count [frame]
  (if-some [frames (-run frame)]
    (count frames)
    0))
(println (frame-count (IfFrame. 0)))
(println (frame-count (IfFrame. 2)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "if_nullable_protocol_vectors_widen_elements_locally"
    "1\n0\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_cond_nullable_protocol_vectors_widen_elements_locally () =
  let source =
    {|
(declare next-frame)
(defprotocol IFrame
  (-run [this]))
(defrecord ResultFrame [^int value])
(defrecord CondFrame [^int value]
  IFrame
  (-run [this]
    (cond
      (= value 0) [(ResultFrame. value)]
      (= value 1) [this (next-frame value)]
      :else nil)))
(defn next-frame [value]
  (if true (ResultFrame. value) nil))
(defn frame-count [frame]
  (if-some [frames (-run frame)]
    (count frames)
    0))
(println (frame-count (CondFrame. 0)))
(println (frame-count (CondFrame. 2)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "cond_nullable_protocol_vectors_widen_elements_locally"
    "1\n0\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_if_some_packs_optional_record_elements_into_dynamic_vectors () =
  let source =
    {|
(defrecord Answer [^int value])
(defn choose [found]
  (if-some [value (if found (Some 1) None)]
    [1 "two"]
    [(Answer. 42) nil]))
(println (count (choose true)))
(println (count (choose false)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "if_some_packs_optional_record_elements_into_dynamic_vectors" "2\n2\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_conditional_vectors_store_seqable_capabilities () =
  let source =
    {|
(defn pair [form]
  (if (sequential? form)
    [(first form) (next form)]
    [nil form]))
(defrecord Variable [symbol])
(defrecord RuleVars [required free])
(defn parse-seq [parse-el form]
  (when (sequential? form)
    (reduce #(if-let [parsed (parse-el %2)]
               (conj %1 parsed)
               (reduced nil))
      [] form)))
(defn parse-variable [form]
  (when (symbol? form)
    (Variable. form)))
(defn parse-var-required [form]
  (or (parse-variable form)
      (throw (ex-info "invalid variable" {:form form}))))
(defn parse-rule [form]
  (when (sequential? form)
    (count form)))
(defn parse-rules [form]
  (parse-seq parse-rule form))
(defn split-rule-vars [form]
  (if (sequential? form)
    (let [[required rest] (if (sequential? (first form))
                            [(first form) (next form)]
                            [nil form])
          required* (parse-seq parse-var-required required)
          free* (parse-seq parse-var-required rest)]
      (RuleVars. required* free*))
    (RuleVars. nil nil)))
(defn rule-vars-arity [rule-vars]
  [(count (:required rule-vars)) (count (:free rule-vars))])
(println (count (pair [1 2])))
(println (count (pair 42)))
(println (pr-str (rule-vars-arity (split-rule-vars [['?a] '?b]))))
(println (pr-str (rule-vars-arity (split-rule-vars ['?a '?b]))))
(println (pr-str (rule-vars-arity (split-rule-vars []))))
(println (nil? (parse-seq parse-variable [1])))
(println (pr-str (parse-rules [[1] [2 3]])))
(println (pr-str (parse-rules [])))
(println (nil? (parse-rules [[1] 2])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "conditional_vectors_store_seqable_capabilities"
    "2\n2\n[1 1]\n[0 2]\n[0 0]\ntrue\n[1 2]\n[]\ntrue\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nullable_sequence_branches_do_not_gain_nested_options () =
  let source =
    {|
(defrecord Holder [value])
(defn choose [flag]
  (if flag
    (Some (list 1))
    (if :else (list 2) nil)))
(def holder (Holder. (choose true)))
(println (some? (.-value ^Holder holder)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nullable_sequence_branches_do_not_gain_nested_options"
    "true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nominal_sequence_branches_lift_into_nullable_results () =
  let source =
    {|
(deftype Datom [^int value])
(defrecord Holder [datoms])
(defn choose-datoms [mode]
  (cond
    (= mode 0)
    (filter (fn [^Datom _] true) [(Datom. 1)])

    (= mode 1)
    (Some (filter (fn [^Datom _] true) [(Datom. 2)]))

    (= mode 2)
    nil

    :else
    (take-while (fn [^Datom _] true)
      (filter (fn [^Datom _] true) [(Datom. 3)]))))
(defn chosen-count [mode]
  (let [holder (Holder. (choose-datoms mode))]
    (if-some [datoms (:datoms holder)]
      (count datoms)
      0)))
(defn first-is-datom? [mode]
  (instance? Datom (first (choose-datoms mode))))
(println
  (str (chosen-count 0) ":"
       (chosen-count 1) ":"
       (chosen-count 2) ":"
       (chosen-count 3) ":"
       (first-is-datom? 0) ":"
       (first-is-datom? 3)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nominal_sequence_branches_lift_into_nullable_results"
    "1:1:0:1:true:true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_cross_module_nominal_sequences_merge_with_dynamic_protocol_results () =
  let set_source =
    {|
(ns test.set)
(defn slice [values present?]
  (if present?
    (Some (filter (fn [_] true) values))
    nil))
(defn first-item [values]
  (first values))
|}
  in
  let db_source =
    {|
(ns test.db)
(defprotocol IDatom
  (datom-value [datom]))
(deftype Datom [^int value ^:mutable ^int idx]
  IDatom
  (datom-value [_] value)
  IEquiv
  (-equiv [_ other]
    (instance? Datom other)))
(defn ^Datom datom [value]
  (Datom. value 0))
|}
  in
  let app_source =
    {|
(ns test.app
  (:require [test.set :as set]
            [test.db :as db]))
(defprotocol Search
  (-search [db]))
(deftype DynamicDB [marker]
  Search
  (-search [_]
    (filter (fn [^test.db/Datom _] true) [(db/datom 2)])))
(defrecord Context [db])
(defrecord Holder [datoms])
(defn choose-datoms [^Context context mode]
  (let [db (:db context)]
    (cond
      (= mode 0)
      (set/slice [(db/datom 1)] true)

      (= mode 1)
      (-search db)

      (= mode 2)
      nil

      :else
      (take-while (fn [^test.db/Datom _] true) (-search db)))))
(def context (Context. (DynamicDB. true)))
(defn chosen-count [mode]
  (let [holder (Holder. (choose-datoms context mode))]
    (if-some [datoms (:datoms holder)]
      (count datoms)
      0)))
(defn first-is-datom? [mode]
  (instance? test.db/Datom (first (choose-datoms context mode))))
(defn first-value [mode]
  (let [holder (Holder. (choose-datoms context mode))
        ^test.db/Datom datom (set/first-item (.-datoms holder))]
    (.-value datom)))
(defn first-static-value []
  (let [^test.db/Datom datom
        (set/first-item
          (filter (fn [^test.db/Datom _] true) [(db/datom 3)]))]
    (.-value datom)))
(println
  (str (chosen-count 0) ":"
       (chosen-count 1) ":"
       (chosen-count 2) ":"
       (chosen-count 3) ":"
       (first-is-datom? 0) ":"
       (first-is-datom? 1) ":"
       (first-value 0) ":"
       (first-value 1) ":"
       (first-static-value)))
|}
  in
  let compile target =
    let state, set_ocaml =
      Lg.Compiler.compile_chunk ~target Lg.Compiler.empty_state set_source
      |> expect_ok
    in
    let state, db_ocaml =
      Lg.Compiler.compile_chunk ~target state db_source |> expect_ok
    in
    let _, app_ocaml =
      Lg.Compiler.compile_chunk ~target state app_source |> expect_ok
    in
    set_ocaml ^ "\n" ^ db_ocaml ^ "\n" ^ app_ocaml
  in
  let native_source = compile Lg.Target.Native in
  assert_ocaml_runs
    "cross_module_nominal_sequences_merge_with_dynamic_protocol_results"
    "1:1:0:1:true:true:1:2:3\n" native_source;
  ignore (compile Lg.Target.Melange)

let test_extend_protocol_keeps_parameter_positions_independent () =
  let source =
    {|
(defrecord FindRel [])
(defrecord FindColl [])
(defrecord FindScalar [])
(defrecord FindTuple [])
(defn map* [f xs]
  (reduce #(conj %1 (f %2)) (empty xs) xs))
(defn tuples->return-map [return-map tuples]
  (let [symbols (:symbols return-map)
        idxs (range 0 (count symbols))]
    (map*
      (fn [tuple]
        (reduce
          (fn [m i] (assoc m (nth symbols i) (nth tuple i)))
          {} idxs))
      tuples)))
(defprotocol PostProcess
  (-post-process [find return-map tuples]))
(extend-protocol PostProcess
  FindRel
  (-post-process [_ return-map tuples]
    (if (nil? return-map)
      tuples
      (tuples->return-map return-map tuples)))

  FindColl
  (-post-process [_ return-map tuples]
    (into [] (map first) tuples))

  FindScalar
  (-post-process [_ return-map tuples]
    (ffirst tuples)))
(println (= [1 2] (-post-process (FindColl.) nil [[1] [2]])))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "extend_protocol_keeps_parameter_positions_independent"
    "true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_cond_thread_preserves_guarded_seqable_aliases () =
  let source =
    {|
(defn query->map [query]
  (vec query))
(defn normalize [q & inputs]
  (count inputs)
  (cond-> q
    (sequential? q) query->map))
(defn normalize-alias [q]
  (let [alias q]
    (if (sequential? q)
      (query->map alias)
      alias)))
(println true)
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "cond_thread_preserves_guarded_seqable_aliases"
    "true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_vec_is_available_as_a_first_class_function () =
  let util_source =
    {|
(ns test.util)
(defn distinct-by [f coll]
  (reduce
    (fn [result value]
      (conj result (f value)))
    [] coll))
|}
  in
  let app_source =
    {|
(ns test.app
  (:require [test.util :as util]))
(def vectors (util/distinct-by vec [(list 1 2) (list 3)]))
(defn trim-tuples [^:dynamic tuples]
  (mapv #(vec (subvec % 0 1)) tuples))
(println
  (str (= [1 2] (first vectors)) ":"
       (= [3] (second vectors)) ":"
       (= 2 (count (trim-tuples [[1 2] [3 4]]))) ":"
       ((nth [symbol?] 0) 'ready)))
|}
  in
  let compile target =
    let state, util_ocaml =
      Lg.Compiler.compile_chunk ~target Lg.Compiler.empty_state util_source
      |> expect_ok
    in
    let _, app_ocaml =
      Lg.Compiler.compile_chunk ~target state app_source |> expect_ok
    in
    util_ocaml ^ "\n" ^ app_ocaml
  in
  let native_source = compile Lg.Target.Native in
  assert_ocaml_runs "vec_is_available_as_a_first_class_function"
    "true:true:true:true\n" native_source;
  ignore (compile Lg.Target.Melange)

let test_condp_preserves_function_recur_tail_positions () =
  let source =
    {|
(defn resolve-step [value]
  (condp = value
    0 (recur 1)
    1 42
    -1))
(println (resolve-step 0))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "condp_preserves_function_recur_tail_positions" "42\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_branch_local_record_hints_materialize_protocol_parameters () =
  let source =
    {|
(defprotocol Runner
  (-run [frame context]))
(defrecord Context [^int value])
(defrecord Frame []
  Runner
  (-run [_ context]
    (if true (.-value ^Context context) nil)))
(println (-run (Frame.) (Context. 42)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "branch_local_record_hints_materialize_protocol_parameters" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_protocol_witness_results_unpack_concrete_sequence_returns () =
  let open Lg.Types in
  let result =
    Lg.Call_elaborator.adapt_protocol_witness_result
      Lg.Compiler_environment.empty ~expected:(TSeq TInt)
      ~actual:(dynamic_constraint TUnknown)
      (Lg.Semantic_ir.Ident "raw_protocol_result")
    |> expect_ok
  in
  if
    not
      (Lg.Semantic_ir.exists_identifier
         (fun name -> name = "Lg_runtime.Runtime_dynamic.to_seq")
         result.semantic_expr)
  then failwith "concrete protocol sequence return was not unpacked"

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

let test_inline_attribute_expands_same_namespace_calls () =
  let source =
    {|
(defn answer
  {:inline (fn [] 42)}
  []
  0)
(println (answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "inline_attribute_expands_same_namespace_calls" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_if_merges_generic_array_function_branches () =
  let source =
    {|
(defn array-getter [check? index]
  (if check?
    (fn [values]
      (let [value (aget values index)]
        (if (int? value) value value)))
    (fn [values]
      (aget values index))))
(println ((array-getter true 0) (into-array [42])))
(println ((array-getter false 0) (into-array [7])))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "if_merges_generic_array_function_branches" "42\n7\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_if_some_preserves_nominal_array_elements () =
  let source =
    {|
(type-record tree [value]
  (key :value))
(defn make-tree [value]
  (record tree (key value)))
(defn rotate [node other]
  (if-some [other-node other]
    (array node other-node)
    (array (make-tree (:key node)))))
(println (:key (aget (rotate (make-tree 1) (Some (make-tree 2))) 1)))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "if_some_preserves_nominal_array_elements" "2\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_melange_array_dot_map_uses_static_array_map () =
  let source =
    {|
(ns app.array-dot-map
  (:require [#?(:cljs cljs.reader :clj clojure.edn) :as edn]))

(def values (into-array [1 2 3]))
(def mapped #?(:cljs (.map values inc) :clj (amap inc values)))
(println (= [2 3 4] (vec mapped)))
(def functions (into-array (edn/read-string "[0]")))
(aset functions 0 (fn [value] value))
(def invoked #?(:cljs (.map functions #(% 9)) :clj (amap #(% 9) functions)))
(println (count invoked))
(defn first-value [values]
  (aget values 0))
(defn invoke-one-or-all [one?]
  (if one?
    first-value
    (fn [value]
      (first (amap #(% value) functions)))))
(def combined
  ((invoke-one-or-all false) (into-array [9])))
(println (some? combined))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "melange_array_dot_map_uses_static_array_map"
    "true\n1\ntrue\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_contains_static_sets_handles_dynamic_candidates () =
  let source =
    {|
(def symbols #{'or 'and})
(def keywords #{:or :and})
(defn symbol-member? [^:dynamic value]
  (contains? symbols value))
(defn keyword-member? [^:dynamic value]
  (contains? keywords value))
(println
  (str (symbol-member? 'or) ":"
       (symbol-member? :or) ":"
       (keyword-member? :and) ":"
       (keyword-member? 'and)))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "contains_static_sets_handles_dynamic_candidates"
    "true:false:true:false\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_builtin_icomparable_supports_dynamic_dispatch () =
  let source =
    {|
(ns comparable-test)
(type-record ranked (value :int))
(extend-type ranked IComparable
  (-compare [left right]
    (compare (:value left) (:value right))))
(defn compare-if-supported [left right]
  (if (satisfies? IComparable left)
    (-compare left right)
    0))
(def low (record ranked (value 1)))
(def high (record ranked (value 2)))
(println (str (compare-if-supported low high) ":"
              (compare-if-supported 1 2)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "builtin_icomparable_supports_dynamic_dispatch" "-1:0\n"
    ocaml_source;
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

let test_keys_support_generic_and_dynamic_maps () =
  let source =
    {|
(ns app.keys
  (:require [#?(:cljs cljs.reader :clj clojure.edn) :as edn]))

(defn key-set [m]
  (set (keys m)))
(defn same-keys? [a b]
  (and
    (= (count a) (count b))
    (every? #(contains? b %) (keys a))
    (every? #(contains? a %) (keys b))))
(defn lookup-and-sum [attrs key]
  (+ (attrs key) (reduce + 0 (vals attrs))))
(defn pairwise-equal? [left right]
  (every?
    (fn [[left-value right-value]]
      (= left-value right-value))
    (map vector left right)))
(defn looks-like? [pattern form]
  (cond
    (= '_ pattern)
    true
    (= '[*] pattern)
    (sequential? form)
    (symbol? pattern)
    (= form pattern)
    (sequential? pattern)
    (if (= (last pattern) '*)
      (and
        (sequential? form)
        (every?
          (fn [[pattern-el form-el]]
            (looks-like? pattern-el form-el))
          (map vector (butlast pattern) form)))
      (and
        (sequential? form)
        (= (count form) (count pattern))
        (every?
          (fn [[pattern-el form-el]]
            (looks-like? pattern-el form-el))
          (map vector pattern form))))
    :else
    (pattern form)))

(def generic-keys
  (key-set (hash-map 'e 0 'v 1)))
(def dynamic-keys
  (keys (edn/read-string "{:a 1 :b 2}")))

(println
  (and
    (= 2 (count generic-keys))
    (every? #(or (= 'e %) (= 'v %)) generic-keys)))
(println
  (and
    (= 2 (count dynamic-keys))
    (= :a (first dynamic-keys))
    (= :b (second dynamic-keys))))
(println (empty? (keys (hash-map))))
(println
  (same-keys?
    (hash-map 'e 0 'v 1)
    (hash-map 'e 9 'v 2)))
(println
  (not
    (same-keys?
      (hash-map 'e 0 'v 1)
      (hash-map 'e 9))))
(println (= 4 (lookup-and-sum (hash-map 'a 1 'b 2) 'a)))
(println (pairwise-equal? [1 2] [1 2]))
(println (not (pairwise-equal? [1 2] [1 3])))
(println (looks-like? '[*] [1 2]))
(println (looks-like? '[a b] '[a b]))
(println (not (looks-like? '[a b] '[a c])))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "keys_support_generic_and_dynamic_maps"
    "true\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_get_returns_nil_for_unknown_map_fields () =
  let source =
    {|(def user {:name "Ada"})(println (nil? (get user :age)))|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "get_returns_nil_for_unknown_map_fields" "true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_get_dispatches_nullable_deftype_lookup_with_dynamic_keys () =
  let source =
    {|
(deftype LookupBox [value]
  ILookup
  (-lookup [_ key]
    (if (= key :value) value nil)))
(defn maybe-box [present]
  (when present (LookupBox. 42)))
(defn missing? [present key]
  (nil? (get (maybe-box present) key)))
(println
  (str (missing? true :value) ":"
       (missing? true :missing) ":"
       (missing? false :value) ":"
       (missing? true "value")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "get_dispatches_nullable_deftype_lookup_with_dynamic_keys"
    "false:true:true:true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_clojure_rt_assoc_matches_core_assoc () =
  let source =
    {|
(def updated-map (clojure.lang.RT/assoc {:a 1} :b 2))
(def updated-vector (clojure.lang.RT/assoc [10 20 30] 1 42))
(println (str (get updated-map :b) ":" (nth updated-vector 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_rt_assoc_matches_core_assoc" "2:42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_protocol_calls_pack_dynamic_non_receiver_arguments () =
  let source =
    {|
(defprotocol IDB
  (-attrs-by [db property]))
(defrecord DB [rschema]
  IDB
  (-attrs-by [db property]
    ((:rschema db) property)))
(def db (DB. {:db.cardinality/many #{:name}}))
(println (contains? (-attrs-by db :db.cardinality/many) :name))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "protocol_calls_pack_dynamic_non_receiver_arguments"
    "true\n" ocaml_source;
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

let test_named_record_calls_accept_structural_extension_fields () =
  let open Lg.Types in
  let value_field = make_field ":value" TInt in
  let extensible_report =
    named_record ~type_name:"report" ~set_module_name:"Report_set"
      [ value_field; make_record_extension_field () ]
  in
  let structural_report =
    TRecord [ value_field; make_field ":extra" TInt ]
  in
  if
    not
      (assignable ~policy:Host_boundary ~expected:extensible_report
         ~actual:structural_report)
  then
    failwith
      "an extensible named record must accept structural extension fields at a host boundary";
  if
    not
      (Lg.Call_elaborator.argument_compatible extensible_report
         structural_report)
  then
    failwith
      "function arguments must honor extensible named-record host boundaries";
  if
    not
      (assignable ~policy:Host_boundary ~expected:structural_report
         ~actual:extensible_report)
  then
    failwith
      "structural loop constraints must accept extensible named records";
  let closed_report =
    named_record ~type_name:"closed-report" ~set_module_name:"Closed_report_set"
      [ value_field ]
  in
  if
    assignable ~policy:Host_boundary ~expected:closed_report
      ~actual:structural_report
  then failwith "a closed named record must reject unknown structural fields";
  if
    assignable ~policy:Host_boundary ~expected:structural_report
      ~actual:closed_report
  then
    failwith
      "structural constraints with unknown fields must reject closed named records";
  let source =
    {|
(defprotocol HasValue
  (value-of [value]))
(defrecord Value [number]
  HasValue
  (value-of [value] (:number value)))
(defrecord Report [^Value value])
(defn check-report [report]
  (value-of (:value report)))
(defn process-report [initial-report]
  (loop [report initial-report
         remaining 1]
    (let [_current (value-of (:value report))]
      (if (zero? remaining)
        (check-report report)
        (recur (assoc report :extra 1) (dec remaining))))))
(println (process-report (Report. (Value. 42))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "named_record_calls_accept_structural_extension_fields"
    "42\n" ocaml_source;
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

let test_assoc_accepts_refined_dynamic_record_fields () =
  let source =
    {|
(defrecord State [^:dynamic schema])
(defn with-schema [db schema]
  {:pre [(or (nil? schema) (map? schema))]}
  (assoc db :schema schema))
(defn next-value [value]
  {:post [(> % value)]}
  (inc value))
(println (nil? (:schema (with-schema (State. {}) nil))))
(println (= 42 (:answer (:schema (with-schema (State. nil) {:answer 42})))))
(println (next-value 41))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "assoc_accepts_refined_dynamic_record_fields"
    "true\ntrue\n42\n"
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

let test_update_missing_structural_field_passes_nil_to_updater () =
  let source =
    {|
(defn value-from-old [old replacement]
  (if (nil? old) replacement 0))
(defmacro update-inline [m k f & more]
  `(let [m# ~m
         k# ~k]
     (assoc m# k# (~f (get m# k#) ~@more))))
(def base {:existing 1})
(def result (update-inline base :added value-from-old 42))
(println [(:existing result) (:added result)])
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "update_missing_structural_field_passes_nil_to_updater"
    "[1 42]\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_inline_update_infers_threaded_record_fields () =
  let source =
    {|
(deftype Datom [value])
(defmacro update-inline [m k f & more]
  `(let [m# ~m
         k# ~k]
     (assoc m# k# (~f (get m# k#) ~@more))))
(defn update-report [report ^Datom datom]
  (let [db (:db-after report)]
    (-> report
        (assoc :db-after db)
        (update-inline :tx-data conj datom))))
|}
  in
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_inline_update_refines_protocol_collection_elements () =
  let source =
    {|
(defprotocol IDatom
  (datom-added [datom] :bool))
(deftype Datom [value]
  IDatom
  (datom-added [_datom] true))
(defmacro update-inline [m k f & more]
  `(let [m# ~m
         k# ~k]
     (assoc m# k# (~f (get m# k#) ~@more))))
(defn update-report [report datom]
  (let [value (:value datom)
        updated (update-inline report :tx-data conj datom)]
    (datom-added datom)
    value
    updated))
|}
  in
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_inline_update_infers_transient_collection_boundaries () =
  let source =
    {|
(defmacro update-inline [m k f & more]
  `(let [m# ~m
         k# ~k]
     (assoc m# k# (~f (get m# k#) ~@more))))
(defn db-transient [db]
  (-> db
      (update-inline :eavt transient)
      (update-inline :aevt transient)))
(defn db-persistent [db]
  (-> db
      (update-inline :eavt persistent!)
      (update-inline :aevt persistent!)))
|}
  in
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_nested_update_infers_optional_map_value_collections () =
  let util_source =
    {|
(ns test.util)
(def conjs (fnil conj #{}))
(defn find-value [pred xs]
  (reduce
    (fn [_ value]
      (if (pred value) (reduced value) nil))
    nil xs))
|}
  in
  let app_source =
    {|
(ns test.app
  (:require [test.util :as util]))
(defn allocate [report ^:dynamic tempid eid]
  (update report :reverse-tempids update eid util/conjs tempid))
(defn has-tempid? [report eid]
  (let [tempids (get (:reverse-tempids report) eid)]
    (some? (util/find-value (fn [_] true) tempids))))
(defn uncalled-has-tempid? [report eid]
  (let [tempids (get (:reverse-tempids report) eid)
        tempid  (util/find-value (fn [_] true) tempids)]
    (some? tempid)))
(def report (allocate {:reverse-tempids {}} "temp" 1))
(println (has-tempid? report 1))
|}
  in
  let compile target =
    let state, util_ocaml =
      Lg.Compiler.compile_chunk ~target Lg.Compiler.empty_state util_source
      |> expect_ok
    in
    let _, app_ocaml =
      Lg.Compiler.compile_chunk ~target state app_source |> expect_ok
    in
    util_ocaml ^ "\n" ^ app_ocaml
  in
  let ocaml_source = compile Lg.Target.Native in
  assert_ocaml_runs "nested_update_infers_optional_map_value_collections"
    "true\n" ocaml_source;
  ignore (compile Lg.Target.Melange);
  ignore (compile Lg.Target.Js_of_ocaml)

let test_if_some_get_keeps_map_storage_non_nullable () =
  let open Lg.Types in
  let key_ty = dynamic_constraint TKeyword in
  let field =
    make_field ":values" (dynamic_map key_ty (TVar "map_value"))
  in
  let target =
    typed_ir (TRecord [ field ]) (Lg.Semantic_ir.Ident "target")
  in
  let replacement =
    typed_ir
      (dynamic_map key_ty (dynamic_constraint TUnknown))
      (Lg.Semantic_ir.Ident "replacement")
  in
  (match Lg.Structural_map.assoc target [ field ] ":values" replacement with
  | Ok _ -> ()
  | Error error ->
      failwith
        ("nested unresolved map values must refine at assoc: "
       ^ error.Lg.Error.message));
  let source =
    {|
(defn remember [report key ^int value]
  (if-some [existing (get (:values report) key)]
    (do (+ existing 0) report)
    (update report :values assoc key value)))
(def result (remember {:values {}} :answer 42))
(println (count (:values result)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "if_some_get_keeps_map_storage_non_nullable" "1\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_nullable_record_constraints_merge_across_branches () =
  let source =
    {|
(type-record callback-storage
  (accessed :fn<int;bool>)
  (restore :fn<int;int>))
(defn use-storage [storage]
  (if-some [value storage]
    (let [accessed (:accessed value)]
      (accessed 1))
    false)
  (match storage
    (Some value)
    (let [restore (:restore value)]
      (restore 41))
    None 0))
(def storage
  (Some
    (record callback-storage
      (accessed (fn [^int _address] true))
      (restore (fn [^int address] address)))))
(println (inc (use-storage storage)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nullable_record_constraints_merge_across_branches" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_named_record_inference_keeps_distinct_host_wrappers () =
  let source =
    {|
(type-record holder
  (storage :ref<option<int>>))
(def holder-value
  (record holder (storage (volatile! (Some 1)))))
(defn read-option [opts]
  (if-some [storage (:storage opts)]
    (some? storage)
    false))
(println (read-option {:storage (Some 42)}))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "named_record_inference_keeps_distinct_host_wrappers"
    "true\n" ocaml_source;
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

let test_cond_literal_true_is_exhaustive () =
  let source =
    {|
(defn compare-values [left right]
  (cond
    (<= left right) -1
    true 1))
(println (+ 1 (compare-values 2 1)))
(println (cond false 0 true 1 false "unreachable"))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "cond_literal_true_is_exhaustive" "2\n1\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_clojure_string_join_accepts_lazy_sequences () =
  let source =
    {|
(require [clojure.string :as str])
(println (str/join "-" (map str [1 2 3])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_string_join_accepts_lazy_sequences" "1-2-3\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_partition_by_keyword_infers_seqable_record_parameters () =
  let source =
    {|
(defrecord Datom [a])
(defn groups [datoms]
  (partition-by :a datoms))
(def result
  (groups [(Datom. :x) (Datom. :x) (Datom. :y)]))
(println (str (count result) ":" (count (first result))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "partition_by_keyword_infers_seqable_record_parameters"
    "2:2\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_doseq_prefers_reducible_over_seqable () =
  let source =
    {|
(type-record direct-values (items :array<int>))
(def seq-calls (atom 0))
(extend-type direct-values Seqable
  (-seq [values]
    (do
      (swap! seq-calls inc)
      (array-to-seq (:items values)))))
(extend-type direct-values Reducible
  (-reduce [_ f initial]
    (f (f (f initial 1) 2) 3)))
(def values (record direct-values (items (array 1 2 3))))
(def total (atom 0))
(doseq [value values]
  (swap! total + value))
(println (str (deref total) ":" (deref seq-calls)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "doseq_prefers_reducible_over_seqable" "6:0\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source
    |> expect_ok)

let test_vals_support_generic_dynamic_and_empty_maps () =
  let source =
    {|
(ns app.vals
  (:require [#?(:cljs cljs.reader :clj clojure.edn) :as edn]))

(defn max-value [values]
  (reduce max (vals values)))
(defn lookup-plus-max [values key]
  (+ (values key) (reduce max (vals values))))
(def dynamic-values
  (vals (edn/read-string "{:a 1 :b 2}")))

(println (= 3 (max-value (hash-map 'a 1 'b 3))))
(println (= 4 (lookup-plus-max (hash-map 'a 1 'b 3) 'a)))
(println
  (and
    (= 2 (count dynamic-values))
    (= 1 (first dynamic-values))
    (= 2 (second dynamic-values))))
(println (empty? (vals (hash-map))))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "vals_support_generic_dynamic_and_empty_maps"
    "true\ntrue\ntrue\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_zipmap_stops_at_shortest_and_preserves_dynamic_boundaries () =
  let source =
    {|
(ns app.zipmap
  (:require [#?(:cljs cljs.reader :clj clojure.edn) :as edn]))

(defn build-index [keys values]
  (zipmap keys values))

(println (= {:a 1 :b 2} (zipmap [:a :b :c] [1 2])))
(println (= {:a 1} (zipmap [:a] [1 2])))
(println (= {:a 2} (zipmap [:a :a] [1 2])))
(println (= {:a 0 :b 1} (zipmap [:a :b] (range))))
(println (empty? (zipmap [] [])))
(println (= {'x 10 'y 20} (build-index ['x 'y] [10 20])))
(def dynamic-result
  (zipmap
    (edn/read-string "[:a :b]")
    (edn/read-string "[1 \"two\"]")))
(println
  (and
    (= 1 (:a dynamic-result))
    (= "two" (:b dynamic-result))))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "zipmap_stops_at_shortest_and_preserves_dynamic_boundaries"
    "true\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_map_accepts_callable_map_values () =
  let source =
    {|
(ns app.callable-map
  (:require [#?(:cljs cljs.reader :clj clojure.edn) :as edn]))

(defrecord Relation [attrs])
(def relation (Relation. {:x 0 :y 1}))
(def evaluations (atom 0))
(defn relation-attrs []
  (do
    (swap! evaluations inc)
    (:attrs relation)))

(println (= [1 0] (vec (map (:attrs relation) [:y :x]))))
(println (= [0 1] (vec (map (relation-attrs) [:x :y]))))
(println (= 1 (deref evaluations)))
(def dynamic-attrs (edn/read-string "{:x 10 :y 20}"))
(println (= [20 10] (vec (map dynamic-attrs [:y :x]))))
(println (= 20 (dynamic-attrs :y)))
(def zipped-attrs (zipmap [:x :y] [30 40]))
(println (= 30 (zipped-attrs :x)))
(println (= :missing (zipped-attrs :z :missing)))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "map_accepts_callable_map_values"
    "true\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_ffirst_is_first_class_and_empty_safe () =
  let source =
    {|
(def rules
  [[['rule-a] :first]
   [['rule-b] :second]
   [['rule-a] :third]])
(def grouped (group-by ffirst rules))
(println (= 2 (count (get grouped 'rule-a))))
(println (= 1 (count (get grouped 'rule-b))))
(println
  (= [nil nil nil 1]
     (mapv ffirst [nil [] [[]] [[1 2]]])))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ffirst_is_first_class_and_empty_safe"
    "true\ntrue\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_group_by_infers_generic_seqable_collections () =
  let source =
    {|
(defn grouped-values [values]
  (group-by (fn [value] (odd? value)) values))
(def grouped (grouped-values [1 2 3]))
(println (= 2 (count (get grouped true))))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "group_by_infers_generic_seqable_collections" "true\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_group_by_unpacks_generic_seqable_items () =
  let source =
    {|
(ns app.group-by-capability
  (:require [#?(:cljs cljs.reader :clj clojure.edn) :as edn]))

(defn grouped-first [rows]
  (let [key-fn (fn [row] (first row))]
    (group-by key-fn rows)))
(def grouped
  (grouped-first (edn/read-string "[[1 2] [1 3] [2 4]]")))
(println (= 2 (count (get grouped (edn/read-string "1")))))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "group_by_unpacks_generic_seqable_items" "true\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_filterv_contextualizes_generic_seqable_items () =
  let source =
    {|
(ns app.filterv-capability
  (:require [#?(:cljs cljs.reader :clj clojure.edn) :as edn]))

(defn without-first [rows]
  (filterv (fn [row] (nil? (first row))) rows))
(println
  (= 1
     (count
       (without-first
         (edn/read-string "[[1 2] [nil 3] [4 5]]")))))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "filterv_contextualizes_generic_seqable_items" "true\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_vec_realizes_for_over_dynamic_map_entries () =
  let source =
    {|
(ns app.vec-for
  (:require [#?(:cljs cljs.reader :clj clojure.edn) :as edn]))

(defn remap-indexes [left right]
  (vec
    (for [[symbol right-index] right]
      [right-index (left symbol)])))
(defn realize-values [values]
  (vec values))
(defn transient-realize-values [values]
  (persistent! (transient (vec values))))
(println
  (= [[0 1] [1 0]]
     (remap-indexes
       (hash-map 'x 0 'y 1)
       (hash-map 'y 0 'x 1))))
(println (= "[1 2]" (pr-str (realize-values (edn/read-string "[1 2]")))))
(println (empty? (realize-values (edn/read-string "[]"))))
(println
  (= "[1 2]"
     (pr-str (transient-realize-values (edn/read-string "[1 2]")))))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "vec_realizes_for_over_dynamic_map_entries"
    "true\ntrue\ntrue\ntrue\n"
    native_source;
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

let test_forwarded_parameters_deduplicate_protocol_constraints () =
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
(defn forwarded-total [value]
  (total value))
(println (forwarded-total (Pair.)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "forwarded_parameters_deduplicate_protocol_constraints"
    "42\n" ocaml_source;
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

let test_and_truthy_guard_narrows_nullable_ints () =
  let source =
    {|
(defn positive-result [flag]
  (let [value (when flag 1)]
    (and value (pos? value))))
(println
  (str (if (positive-result true) "yes" "no") ":"
       (if (positive-result false) "yes" "no")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "and_truthy_guard_narrows_nullable_ints" "yes:no\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_or_nil_guard_narrows_nullable_records () =
  let source =
    {|
(defrecord Datom [a])
(defn maybe-datom [present?]
  (if present? (Datom. :name) nil))
(defn matching-or-missing? [datom]
  (or (nil? datom) (= (.-a datom) :name)))
(println
  (str (matching-or-missing? (maybe-datom false)) ":"
       (matching-or-missing? (maybe-datom true))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "or_nil_guard_narrows_nullable_records" "true:true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_some_guard_narrows_nullable_records () =
  let source =
    {|
(deftype Datom [a])
(defn maybe-datom [present?]
  (if present? (Datom. :name) nil))
(defn named? [datom]
  (and (some? datom) (= (.-a datom) :name)))
(println (str (named? (maybe-datom false)) ":"
              (named? (maybe-datom true))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "some_guard_narrows_nullable_records" "false:true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_or_nil_guard_narrows_hinted_dynamic_sequence_elements () =
  let source =
    {|
(deftype Datom [a])
(defprotocol IFrame (-run [this]))
(defn first-seq [values] (first values))
(defrecord Holder [datoms]
  IFrame
  (-run [_]
    (loop [remaining datoms]
      (let [^Datom datom (first-seq remaining)
            datom-ahead? (or (nil? datom) false)]
        (if datom-ahead?
          true
          (some? (.-a datom)))))))
(println (-run (Holder. (list (Datom. :name)))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "or_nil_guard_narrows_hinted_dynamic_sequence_elements"
    "true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_loop_parameters_widen_for_nullable_generic_recur_values () =
  let source =
    {|
(deftype Datom [a])
(defn first-seq [values] (first values))
(defn next-seq [values] (next values))
(defrecord Holder [datoms])
(defn last-datom-present? [^Holder holder]
  (loop [current (Datom. :initial)
         remaining (.-datoms holder)]
    (if (seq remaining)
      (recur (first-seq remaining) (next-seq remaining))
      (if (nil? current)
        false
        (some? (.-a current))))))
(println (last-datom-present? (Holder. (list (Datom. :name)))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "loop_parameters_widen_for_nullable_generic_recur_values"
    "true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_loop_recur_unpacks_dynamic_protocol_results_to_static_records () =
  let source =
    {|
(defprotocol ICache
  (-get [cache key default-fn]))
(defprotocol Runner
  (-run [frame cache]))
(defrecord Item [^int value])
(defrecord Frame [^Item initial]
  Runner
  (-run [_ cache]
    (loop [item initial
           attempt 0]
      (if (= attempt 0)
        (recur (-get cache :item #(Item. 7)) 1)
        (.-value ^Item item)))))
(defrecord Cache []
  ICache
  (-get [_ key default-fn]
    (do key (default-fn))))
(println (-run (Frame. (Item. 1)) (Cache.)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "loop_recur_unpacks_dynamic_protocol_results_to_static_records" "7\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_loop_recur_analysis_respects_nested_let_shadowing () =
  let source =
    {|
(defn remaining-count [values]
  (loop [remaining values
         result 0]
    (if (seq remaining)
      (let [next-values (next remaining)]
        (let [next-values next-values]
          (recur next-values (inc result))))
      result)))
(println (remaining-count [1 2 3]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "loop_recur_analysis_respects_nested_let_shadowing" "3\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_generic_loop_sequences_remain_specializable_at_static_call_sites () =
  let source =
    {|
(deftype Entry [value])
(defn same-items? [left right]
  (loop [xs (seq left)
         ys (seq right)]
    (cond
      (nil? xs) (nil? ys)
      (= (first xs) (first ys)) (recur (next xs) (next ys))
      :else false)))
(def entries (seq (list (Entry. 1))))
(println (same-items? entries entries))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "generic_loop_sequences_remain_specializable_at_static_call_sites" "true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_str_keeps_heterogeneous_record_fields_dynamic () =
  let source =
    {|
(defprotocol IRender
  (-render [this])
  (-produce [this]))
(defrecord Box [value]
  IRender
  (-render [_] (str value)))
(defrecord Producer []
  IRender
  (-render [_] "producer")
  (-produce [_] [(Box. (when true [1 2])) (Box. :ready)]))
(def boxes (-produce (Producer.)))
(println
  (str (-render (first boxes))
       ":"
       (-render (second boxes))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "str_keeps_heterogeneous_record_fields_dynamic"
    "[1 2]::ready\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_callable_set_parameters_remain_sets_for_conj () =
  let source =
    {|
(ns app.callable-set
  (:require [clojure.set :as set]))
(defn add-unseen [seen id]
  (if (seen id)
    seen
    (conj seen id)))
(defn remove-bound [bound values]
  (set (remove bound values)))
(defn remove-bound-and-known [bound values]
  (set/difference (set (remove bound values)) #{2}))
(def once (add-unseen #{} 1))
(println (str (count once) ":" (count (add-unseen once 1))))
(println (remove-bound #{1} [1 2]))
(println (remove-bound-and-known #{1} [1 2 3]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "callable_set_parameters_remain_sets_for_conj"
    "1:1\n#{2}\n#{3}\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_computed_sets_are_first_class_predicates () =
  let source =
    {|
(def blocked (set [2 4]))
(defn remove-dynamic [^:dynamic blocked]
  (remove blocked [1 2 3 4]))
(println (pr-str (vec (remove blocked [1 2 3 4]))))
(println (empty? (remove (set [1 2]) [1 2])))
(println (every? blocked [2 4]))
(println (pr-str (vec (remove-dynamic blocked))))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "computed_sets_are_first_class_predicates"
    "[1 3]\ntrue\ntrue\n[1 3]\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_generic_clojure_set_subset_constrains_parameters () =
  let source =
    {|
(ns app.set-subset
  (:require [clojure.set :as set]))
(defn subset-of? [left right]
  (set/subset? left right))
(defn values-set [values]
  (set values))
(defn values-subset-of? [values right]
  (set/subset? (values-set values) right))
(println (subset-of? #{1 2} #{1 2 3}))
(println (values-subset-of? [1 2] #{1 2 3}))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_clojure_set_subset_constrains_parameters"
    "true\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  let sequence_source =
    {|
(ns app.sequence-subset
  (:require [clojure.set :as set]))
(defn sequence-subset-of? [values right]
  (set/subset? values right))
(println (sequence-subset-of? [1 2] #{1 2 3}))
(defn sequence-missing [values right]
  (set/subset? values right)
  (set/difference (set values) right))
(println true)
|}
  in
  let native_source =
    Lg.Compiler.compile_string sequence_source |> expect_ok
  in
  assert_ocaml_runs "sequence_subset_accepts_seqable_values" "true\ntrue\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange sequence_source
    |> expect_ok)

let test_resolve_returns_nil_without_runtime_var_reflection () =
  let source =
    {|
(defn resolve-value [sym]
  (when-some [var (resolve sym)]
    (deref var)))
(println (nil? (resolve 'missing/value)))
(println (nil? (resolve-value 'missing/value)))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "resolve_returns_nil_without_runtime_var_reflection"
    "true\ntrue\n" native_source;
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
(def mixed-list (list* 'or-join [1 2] (list [3] [4])))
(defn namespace-or-empty [value]
  (if-let [ns (namespace value)] ns ""))
(println
  (str (name qualified) ":" (namespace-or-empty qualified) ":" (name kw) ":" (namespace-or-empty kw) ":"
       (name kw2) ":" (namespace-or-empty kw2) ":" (pr-str more-names) ":"
       (:name m1) ":" (:ready m2) ":" (pr-str s1) ":" (pr-str listed) ":"
       (pr-str mixed-list) ":"
       (symbol? simple) ":" (symbol? :ready) ":"
       (simple-symbol? simple) ":" (simple-symbol? qualified) ":"
       (qualified-symbol? qualified) ":" (qualified-symbol? simple) ":"
       (ident? simple) ":" (simple-ident? simple) ":" (qualified-ident? qualified)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "batched_identifier_and_constructor_core_functions_work"
    "name:user:name:user:id:user:[ready user/name]:Ada:true:#{1 2 3}:(1 2 3 \
     4):(or-join [1 2] [3] [4]):true:false:true:false:true:false:true:true:true\n"
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

let test_thread_last_inferred_functions_pass_collections_to_take_while () =
  let source =
    {|
(defmacro small-thread [values]
  `(clojure.core/->> ~values
     (take-while (fn [value] (< value 3)))))
(defn small-values [values]
  (small-thread values))
(println (pr-str (small-values [1 2 3 1])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "thread_last_inferred_functions_pass_collections_to_take_while"
    "(1 2)\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_two_arity_reduce_uses_first_or_zero_arity_identity () =
  let source =
    {|
(defn sum
  ([] 10)
  ([left right] (+ left right)))
(defn stop-sum
  ([] 0)
  ([acc value]
   (if (= value 3)
     (reduced acc)
     (+ acc value))))
(println (reduce max [1 3 2]))
(println (reduce sum [5]))
(println (reduce sum []))
(println (reduce stop-sum [0 1 2 3 100]))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "two_arity_reduce_uses_first_or_zero_arity_identity"
    "3\n5\n10\n3\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_reduce_refines_empty_set_accumulators_without_widening_static_sets () =
  let source =
    {|
(def numbers
  (reduce (fn [acc value] (conj acc value)) #{} [1 2 2]))
(deftype Entry [^int id])
(defn maybe-entry [present]
  (when present (Entry. 1)))
(def entries
  (reduce (fn [acc present] (conj acc (maybe-entry present)))
    #{} [true false]))
(defrecord Datom [value])
(def datoms [(Datom. 1) (Datom. "two")])
(def datom-values
  (reduce #(conj %1 (:value %2)) #{} datoms))
(println
  (str (count numbers) ":" (count entries) ":" (count datom-values)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  if
    not
      (string_contains_substring ocaml_source
         "Lg_runtime.Core_set.Int_set.add")
  then failwith "integer set accumulator must remain static";
  if
    not
      (string_contains_substring ocaml_source
         "Lg_runtime.Runtime_dynamic.conj")
  then failwith "nullable set accumulator must use the dynamic boundary";
  assert_ocaml_runs
    "reduce_refines_empty_set_accumulators_without_widening_static_sets"
    "2:2:2\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_concat_lifts_values_into_nullable_element_types () =
  let source =
    {|
(def optional-values
  [(first ["a"])
   (first (vector-of :string))])
(println
  (pr-str
    (map (fn [value]
           (if-some [present value] present "nil"))
         (concat optional-values ["b"]))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "concat_lifts_values_into_nullable_element_types"
    "(\"a\" \"nil\" \"b\")\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_interleave_accepts_inferred_seqable_parameters () =
  let source =
    {|
(defn weave [values]
  (interleave values (repeat :flush)))
(def woven (take 4 (weave [1 2])))
(println
  (str (= (first woven) 1) ":"
       (= (second woven) :flush) ":"
       (= (count woven) 4)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "interleave_accepts_inferred_seqable_parameters"
    "true:true:true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

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

let test_last_returns_nil_for_empty_collections () =
  let source =
    {|
(println (nil? (last (list))))
(println (nil? (last [])))
(println (nil? (last (seq []))))
(println (last (list 1 2 3)))
(println (last [4 5 6]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "last_returns_nil_for_empty_collections"
    "true\ntrue\ntrue\n3\n6\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_first_returns_nil_for_empty_collections () =
  let source =
    {|
(println (nil? (first (list))))
(println (nil? (first [])))
(println (nil? (first (seq []))))
(println (nil? (first (array-of :int))))
(println (nil? (first "")))
(println (+ (first (list 3 2 1)) 0))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "first_returns_nil_for_empty_collections"
    "true\ntrue\ntrue\ntrue\ntrue\n3\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_generic_first_can_seed_a_dynamic_reduce () =
  let source =
    {|
(defn aggregate-min [coll]
  (reduce
    (fn [acc x]
      (if (neg? (compare x acc)) x acc))
    (first coll)
    (next coll)))

(println (+ (aggregate-min [3 1 2]) 0))
(println (nil? (aggregate-min [])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_first_can_seed_a_dynamic_reduce" "1\ntrue\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_optional_record_dynamic_fields_use_runtime_nil () =
  let source =
    {|
(defrecord Entry [^:dynamic value])

(defn duplicate-first-value []
  (let [value (:value (first [(Entry. 7)]))
        values (transient [value])]
    (if (some? value)
      (persistent! (conj! values value))
      (persistent! values))))

(println (pr-str (duplicate-first-value)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "optional_record_dynamic_fields_use_runtime_nil"
    "[7 7]\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_rseq_dispatches_to_reversible_protocol () =
  let source =
    {|
(ns app.reversible)
(deftype ReversibleBox [values]
  IReversible
  (-rseq [_] [3 2 1]))
(println (pr-str (rseq (ReversibleBox. [1 2 3]))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "rseq_dispatches_to_reversible_protocol" "[3 2 1]\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_deftype_protocol_methods_support_multiple_arities () =
  let source =
    {|
(deftype LookupBox [value]
  ILookup
  (-lookup
    ([_ key] (if (= key :value) value nil))
    ([_ key not-found] (if (= key :value) value not-found))))
(def box (LookupBox. 42))
(println (str (get box :value) ":" (get box :missing 7)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "deftype_protocol_methods_support_multiple_arities" "42:7\n"
    ocaml_source

let test_macros_preserve_nested_parameter_type_hints () =
  let source =
    {|
(defprotocol Identified
  (-id [value]))
(deftype Item [value]
  Identified
  (-id [_] value))
(defn- choose-first-form [forms]
  (first (drop 0 forms)))
(defmacro first-form [forms]
  (choose-first-form forms))
(def item-value
  (first-form
    [(fn [^Item item]
       (+ (.-value item) (-id item)))]))
(println (item-value (Item. 21)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "macros_preserve_nested_parameter_type_hints" "42\n"
    ocaml_source

let test_protocol_calls_recover_structurally_inferred_named_records () =
  let source =
    {|
(defprotocol Identified
  (-id [value]))
(deftype Item [value]
  Identified
  (-id [_] value))
(defn item-score [item]
  (+ (:value item) (-id item)))
(println (item-score (Item. 21)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "protocol_calls_recover_structurally_inferred_named_records"
    "42\n" ocaml_source

let test_transducer_type_hints_infer_nominal_record_fields () =
  let source =
    {|
(defprotocol IDatom
  (datom-value [datom]))
(deftype Datom [e a v ^int tx]
  IDatom
  (datom-value [_] tx))
(defrecord Search [items]
  ICounted
  (-count [search]
    (count
      (->Eduction
        (filter (fn [^Datom datom]
                  (and (some? (.-v datom))
                       (pos? (datom-value datom)))))
        items))))
(defprotocol IProjection
  (-values [projection]))
(defrecord Projection [items]
  IProjection
  (-values [projection]
    (map (fn [^Datom datom]
           (datom-value datom))
      items)))
(def datoms [(Datom. 1 :a true 1)
             (Datom. 2 :a true -1)
             (Datom. 3 :a true 2)])
(println (count (Search. datoms)))
(println (pr-str (-values (Projection. datoms))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "transducer_type_hints_infer_nominal_record_fields"
    "2\n(1 -1 2)\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_deftype_methods_flush_after_their_declared_dependencies () =
  let source =
    {|
(declare item-score normalize-score)
(defprotocol Identified
  (-id [item]))
(deftype Item [^int value]
  Identified
  (-id [item] (item-score item)))
(defn item-score [^Item item]
  (normalize-score (.-value item)))
(defn normalize-score [score]
  score)

(declare unrelated)
(defrecord Items [values]
  ICounted
  (-count [items]
    (count
      (filter (fn [^Item item]
                (pos? (-id item)))
        values))))
(defn unrelated [] 42)

(println (count (Items. [(Item. 1) (Item. -1) (Item. 2)])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "deftype_methods_flush_after_their_declared_dependencies" "2\n"
    ocaml_source

let test_protocol_consumers_use_stable_later_implementation_returns () =
  let source =
    {|
(defprotocol Searchable
  (-search [data pattern]))
(defn first-match [data pattern]
  (first (-search data pattern)))
(deftype SearchData [values]
  Searchable
  (-search [data pattern]
    (if (seq values)
      (Some (seq values))
      None)))
(defn find-value [data]
  (first-match data []))
(println (find-value (SearchData. [42])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "protocol_consumers_use_stable_later_implementation_returns" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_deferred_initializers_run_before_first_ready_use () =
  let source =
    {|
(declare later-score)
(defprotocol Scored
  (-score [value]))
(deftype Score [^int value]
  Scored
  (-score [score]
    (later-score (.-value score))))
(def result (-score (Score. 41)))
(defn later-score [value]
  (+ value 1))
(println result)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "deferred_initializers_run_before_first_ready_use" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_declared_record_constructors_follow_record_dependencies () =
  let source =
    {|
(declare ->LaterFrame)
(defprotocol Frame
  (-run [frame]))
(defrecord EarlierFrame []
  Frame
  (-run [_]
    (->LaterFrame 42)))
(defrecord LaterFrame [value]
  Frame
  (-run [frame]
    frame))
(println (:value (-run (EarlierFrame.))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "declared_record_constructors_follow_record_dependencies"
    "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dependency_graph_orders_declared_protocol_dependencies () =
  let open Lg.Ast in
  let forms =
    [
      FList [ FSymbol "declare"; FSymbol "later-score" ];
      FList
        [
          FSymbol "defprotocol";
          FSymbol "Scored";
          FList [ FSymbol "-score"; FVector [ FSymbol "value" ] ];
        ];
      FList
        [
          FSymbol "deftype";
          FSymbol "Score";
          FVector [];
          FSymbol "Scored";
          FList
            [
              FSymbol "-score";
              FVector [ FSymbol "score" ];
              FList [ FSymbol "later-score"; FInt 41 ];
            ];
        ];
      FList
        [
          FSymbol "def";
          FSymbol "result";
          FList [ FSymbol "-score"; FList [ FSymbol "Score." ] ];
        ];
      FList
        [
          FSymbol "defn";
          FSymbol "later-score";
          FVector [ FSymbol "value" ];
          FSymbol "value";
        ];
    ]
  in
  let order = Lg.Dependency_graph.stable_order forms in
  let position index =
    List.find_index (( = ) index) order |> Option.value ~default:max_int
  in
  if not (position 4 < position 2 && position 2 < position 3) then
    failwith "declared helper, protocol implementation, and first use are misordered";
  let components =
    Lg.Dependency_graph.strongly_connected_components
      [
        { Lg.Dependency_graph.name = "left"; dependencies = [ "right" ] };
        { name = "right"; dependencies = [ "left" ] };
        { name = "after"; dependencies = [ "left" ] };
      ]
  in
  if not (List.exists (fun names -> List.sort String.compare names = [ "left"; "right" ]) components)
  then failwith "mutual recursion must form one strongly connected component"

let test_dependency_graph_orders_non_dash_protocol_methods_before_consumers () =
  let open Lg.Ast in
  let forms =
    [
      FList [ FSymbol "declare"; FSymbol "later" ];
      FList
        [
          FSymbol "defprotocol";
          FSymbol "IDatom";
          FList [ FSymbol "datom-tx"; FVector [ FSymbol "datom" ] ];
        ];
      FList [ FSymbol "deftype"; FSymbol "Datom"; FVector [ FSymbol "tx" ] ];
      FList
        [
          FSymbol "defn";
          FSymbol "compare-datoms";
          FVector [ FSymbol "left"; FSymbol "right" ];
          FList
            [
              FSymbol "compare";
              FList [ FSymbol "datom-tx"; FSymbol "left" ];
              FList [ FSymbol "datom-tx"; FSymbol "right" ];
            ];
        ];
      FList
        [
          FSymbol "deftype-methods";
          FSymbol "Datom";
          FSymbol "IDatom";
          FList
            [
              FSymbol "datom-tx";
              FVector [ FSymbol "datom" ];
              FSymbol "tx";
            ];
        ];
      FList
        [
          FSymbol "defn";
          FSymbol "later";
          FVector [ FSymbol "value" ];
          FSymbol "value";
        ];
    ]
  in
  let order = Lg.Dependency_graph.stable_order forms in
  let position index =
    List.find_index (( = ) index) order |> Option.value ~default:max_int
  in
  if not (position 1 < position 4 && position 4 < position 3) then
    failwith
      "ordinary protocol method implementations must precede their consumers"

let test_dependency_graph_keeps_declarations_before_macro_consumers () =
  let open Lg.Ast in
  let forms =
    [
      FList [ FSymbol "namespace-scope"; FSymbol "example.core" ];
      FList [ FSymbol "declare"; FSymbol "later"; FSymbol "build" ];
      FList [ FSymbol "deftrecord"; FSymbol "Variable"; FVector [] ];
      FList
        [
          FSymbol "defn";
          FSymbol "build";
          FVector [];
          FList
            [ FSymbol "later"; FList [ FSymbol "Variable." ] ];
        ];
      FList
        [
          FSymbol "defn";
          FSymbol "later";
          FVector [ FSymbol "value" ];
          FList [ FSymbol "build" ];
        ];
    ]
  in
  let order = Lg.Dependency_graph.stable_order forms in
  let position index =
    List.find_index (( = ) index) order |> Option.value ~default:max_int
  in
  if not (position 0 < position 1 && position 1 < position 2) then
    failwith
      "namespace and declare must be available before macro-generated consumers"

let test_dependency_graph_loads_requires_before_runtime_macro_consumers () =
  let open Lg.Ast in
  let forms =
    [
      FList [ FSymbol "namespace-scope"; FSymbol "datascript.db" ];
      FList
        [
          FSymbol "defmacro";
          FSymbol "validate-attr";
          FVector [ FSymbol "attribute" ];
          FList
            [
              FSymbol "syntax-quote";
              FList
                [
                  FSymbol "validate-schema";
                  FList [ FSymbol "util/raise"; FSymbol "attribute" ];
                ];
            ];
        ];
      FList
        [
          FSymbol "require";
          FVector
            [ FSymbol "datascript.util"; FKeyword ":as"; FSymbol "util" ];
          FVector
            [
              FSymbol "datascript.db";
              FKeyword ":refer";
              FVector [ FSymbol "validate-attr" ];
            ];
        ];
      FList [ FSymbol "declare"; FSymbol "later" ];
      FList
        [
          FSymbol "defn";
          FSymbol "validate-schema";
          FVector [ FSymbol "schema" ];
          FList [ FSymbol "util/raise"; FSymbol "schema" ];
        ];
      FList
        [
          FSymbol "defn";
          FSymbol "later";
          FVector [ FSymbol "value" ];
          FSymbol "value";
        ];
    ]
  in
  let order = Lg.Dependency_graph.stable_order forms in
  let position index =
    List.find_index (( = ) index) order |> Option.value ~default:max_int
  in
  if
    not
      (position 0 < position 1 && position 1 < position 2
     && position 2 < position 4 && position 3 < position 4)
  then
    failwith
      "compile-time macros and requires must load before runtime macro consumers"

let test_stabilization_ast_skips_mutual_function_bodies () =
  let open Lg.Ast in
  let forms =
    [
      FList [ FSymbol "declare"; FSymbol "left"; FSymbol "right" ];
      FList
        [
          FSymbol "defn";
          FSymbol "left";
          FVector [];
          FList [ FSymbol "right" ];
        ];
      FList
        [
          FSymbol "defn";
          FSymbol "right";
          FVector [];
          FList [ FSymbol "left" ];
        ];
      FList [ FSymbol "defn"; FSymbol "independent"; FVector []; FInt 42 ];
    ]
  in
  let evidence = Lg.Toolchain.stabilization_ast forms in
  match evidence with
  | [ _; FList (FSymbol "declare" :: names); FList [ FSymbol "declare" ]; last ] ->
      if names <> [ FSymbol "left"; FSymbol "right" ] then
        failwith "the evidence pass must retain both recursive declarations";
      if last != List.nth forms 3 then
        failwith "the evidence pass must preserve independent forms"
  | _ -> failwith "mutual function bodies must be omitted from evidence passes"

let test_declarations_do_not_merge_independent_functions () =
  let source =
    {|
(declare later)
(defprotocol Value
  (-value [item]))
(deftype Box [value]
  Value
  (-value [item] (.-value item)))
(defn independent [value]
  value)
(defn later [value]
  value)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  if string_contains_substring ocaml_source "and ((independent)[" then
    failwith
      "declare must not merge independent functions into one recursive group"

let test_typecheck_stabilizes_forward_declaration_abi () =
  let passes = ref 0 in
  let compile (state : Lg.Compiler_state.t) =
    incr passes;
    let row_arity = if !passes = 1 then 29 else 41 in
    let row_parameters =
      List.init row_arity (fun index -> Some ("'a" ^ string_of_int index))
    in
    let binding =
      Lg.Types.binding ~row_param_types:row_parameters "restore"
        (Lg.Types.TFn ([ Lg.Types.TInt ], Lg.Types.TInt))
    in
    let env = Lg.Compiler_environment.add "restore" binding state.env in
    Ok ({ state with env }, [])
  in
  let ast =
    [
      Lg.Ast.FList
        [
          Lg.Ast.FSymbol "declare";
          Lg.Ast.FSymbol "restore";
          Lg.Ast.FSymbol "helper";
        ];
      Lg.Ast.FList
        [
          Lg.Ast.FSymbol "def";
          Lg.Ast.FSymbol "restore";
          Lg.Ast.FList [ Lg.Ast.FSymbol "helper" ];
        ];
      Lg.Ast.FList
        [
          Lg.Ast.FSymbol "def";
          Lg.Ast.FSymbol "helper";
          Lg.Ast.FList [ Lg.Ast.FSymbol "restore" ];
        ];
    ]
  in
  let state, _ =
    Lg.Toolchain.stabilize_typecheck ~compile
      ~initial_state:Lg.Compiler_state.empty ast
    |> expect_ok
  in
  if !passes <> 3 then
    failwith
      ("forward declaration ABI should stabilize in three passes, got "
      ^ string_of_int !passes);
  let binding =
    Lg.Compiler_environment.find_opt "restore" state.env
    |> Option.get
  in
  if List.length binding.row_param_types <> 41 then
    failwith "the stabilized declaration ABI must be retained"

let test_typecheck_validates_full_compile_after_evidence_stabilizes () =
  let full_passes = ref 0 in
  let evidence_passes = ref 0 in
  let compile_with_row_arity passes row_arity (state : Lg.Compiler_state.t) =
    incr passes;
    let row_parameters =
      List.init row_arity (fun index -> Some ("'a" ^ string_of_int index))
    in
    let binding =
      Lg.Types.binding ~row_param_types:row_parameters "restore"
        (Lg.Types.TFn ([ Lg.Types.TInt ], Lg.Types.TInt))
    in
    let env = Lg.Compiler_environment.add "restore" binding state.env in
    Ok ({ state with env }, [])
  in
  let compile state =
    let row_arity = if !full_passes = 0 then 29 else 53 in
    compile_with_row_arity full_passes row_arity state
  in
  let compile_evidence state =
    compile_with_row_arity evidence_passes 41 state
  in
  let ast =
    [
      Lg.Ast.FList
        [ Lg.Ast.FSymbol "declare"; Lg.Ast.FSymbol "restore" ];
    ]
  in
  let state, _ =
    Lg.Toolchain.stabilize_typecheck ~compile_evidence ~compile
      ~initial_state:Lg.Compiler_state.empty ast
    |> expect_ok
  in
  if !full_passes <> 3 then
    failwith
      ("the final full compile should stabilize its own ABI, got "
      ^ string_of_int !full_passes
      ^ " full passes");
  let binding =
    Lg.Compiler_environment.find_opt "restore" state.env
    |> Option.get
  in
  if List.length binding.row_param_types <> 53 then
    failwith "the final full compile ABI must be retained"

let test_nested_simple_let_inference_visits_body_linearly () =
  let known_lookups = ref 0 in
  let lookup_function_ty name =
    if String.equal name "known" then (
      incr known_lookups;
      Ok (Lg.Types.TFn ([ Lg.Types.TInt ], Lg.Types.TInt))
    ) else Lg.Error.error ("unknown function " ^ name)
  in
  let rec nested_let depth value =
    if depth = 0 then
      Lg.Ast.FList [ Lg.Ast.FSymbol "known"; value ]
    else
      let local = "value" ^ string_of_int depth in
      Lg.Ast.FList
        [
          Lg.Ast.FSymbol "let";
          Lg.Ast.FVector [ Lg.Ast.FSymbol local; value ];
          nested_let (depth - 1) (Lg.Ast.FSymbol local);
        ]
  in
  ignore
    (Lg.Type_inference.infer_params ~lookup_function_ty
       ~lookup_protocol_constraint:(fun _ -> None)
       ~lookup_dynamic_key_record_type:(fun _ -> None)
       ~resolve_named_record:Fun.id
       [ ("input", Lg.Types.TInt) ]
       [ nested_let 6 (Lg.Ast.FSymbol "input") ]
    |> expect_ok);
  if !known_lookups > 16 then
    failwith
      ("nested simple lets should visit the body linearly, got "
      ^ string_of_int !known_lookups
      ^ " known-function lookups")

let test_global_function_alias_keeps_contextual_inference () =
  let lookup_function_ty name =
    if String.equal name "known" then
      Ok (Lg.Types.TFn ([ Lg.Types.TInt ], Lg.Types.TInt))
    else Lg.Error.error ("unknown function " ^ name)
  in
  let body =
    Lg.Ast.FList
      [
        Lg.Ast.FSymbol "let";
        Lg.Ast.FVector
          [ Lg.Ast.FSymbol "f"; Lg.Ast.FSymbol "known" ];
        Lg.Ast.FList
          [ Lg.Ast.FSymbol "f"; Lg.Ast.FSymbol "input" ];
      ]
  in
  let inferred =
    Lg.Type_inference.infer_params ~lookup_function_ty
      ~lookup_protocol_constraint:(fun _ -> None)
      ~lookup_dynamic_key_record_type:(fun _ -> None)
      ~resolve_named_record:Fun.id
      [ ("input", Lg.Types.TUnknown) ] [ body ]
    |> expect_ok
  in
  match List.assoc_opt "input" inferred with
  | Some Lg.Types.TInt -> ()
  | Some ty ->
      failwith
        ("global function alias should infer int, got "
        ^ Lg.Types.source_name ty)
  | None -> failwith "global function alias lost the input parameter"

let test_destructured_let_keeps_provisional_body_inference () =
  let source =
    {|
(defn first-plus-one [items]
  (let [[item] items]
    (+ item 1)))

(print (first-plus-one [41]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "destructured_let_keeps_provisional_body_inference" "42"
    ocaml_source

let test_recursive_collection_result_specializes_self_calls () =
  let source =
    {|
(defn expand-values [values]
  (map
    (fn [value]
      (if (sequential? value)
        (first (expand-values value))
        value))
    values))

(print "ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "recursive_collection_result_specializes_self_calls" "ok"
    ocaml_source

let test_grouped_records_preserve_constructor_type () =
  let source =
    {|
(defrecord Branch [vars clauses])
(defrecord Rule [rule-name branches])
(defrecord Parsed [source-name vars clauses])

(defn parse-branch [form]
  (Parsed. (first form) [] []))

(defn validate-branches [name branches]
  (:vars (first branches)))

(defn parse-rules [forms]
  (vec
    (for [[name branches] (group-by :source-name (map parse-branch forms))
          :let [branches (mapv #(Branch. (:vars %) (:clauses %)) branches)]]
      (do
        (validate-branches name branches)
        (Rule. name branches)))))

(print "ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "grouped_records_preserve_constructor_type" "ok"
    ocaml_source

let test_typecheck_skips_replay_without_new_stabilization_evidence () =
  let passes = ref 0 in
  let protocol_id =
    Lg.Protocol_id.create ~owner:[] ~name:"CompileOnce"
  in
  let compile (state : Lg.Compiler_state.t) =
    incr passes;
    let protocols =
      Lg.Protocol_registry.declare protocol_id []
        (Lg.Compiler_environment.protocols state.env)
      |> expect_ok
    in
    let env =
      Lg.Compiler_environment.with_protocols protocols state.env
    in
    Ok ({ state with env }, [])
  in
  let ast =
    [
      Lg.Ast.FList
        [ Lg.Ast.FSymbol "def"; Lg.Ast.FSymbol "answer"; Lg.Ast.FInt 42 ];
    ]
  in
  ignore
    (Lg.Toolchain.stabilize_typecheck ~compile
       ~initial_state:Lg.Compiler_state.empty ast
    |> expect_ok);
  if !passes <> 1 then
    failwith
      ("a form without forward declarations must compile once, got "
      ^ string_of_int !passes)

let test_recursive_declared_nullable_sequence_supports_not_empty () =
  let source =
    {|
(declare parse-node parse-nodes)

(defn parse-seq [parse-element forms]
  (when (sequential? forms)
    (reduce
      (fn [parsed form]
        (if-let [parsed-form (parse-element form)]
          (conj parsed parsed-form)
          (reduced nil)))
      [] forms)))

(defn parse-group [forms]
  (let [parsed (parse-nodes forms)]
    (if (not-empty parsed)
      (count parsed)
      0)))

(defn parse-node [form]
  (cond
    (nil? form) nil
    (sequential? form) (parse-group form)
    :else form))

(defn parse-nodes [forms]
  (parse-seq parse-node forms))

|}
  in
  let consumer =
    {|
(println
  (str (parse-group [1 2]) ":"
       (parse-group []) ":"
       (parse-group [nil])))
|}
  in
  let compile target =
    let state, provider =
      Lg.Compiler.compile_chunk ~target Lg.Compiler.empty_state source
      |> expect_ok
    in
    let _, consumer =
      Lg.Compiler.compile_chunk ~target state consumer |> expect_ok
    in
    provider ^ "\n" ^ consumer
  in
  let native_source =
    compile Lg.Target.Native
  in
  assert_ocaml_runs "recursive_declared_nullable_sequence_supports_not_empty"
    "2:0:0\n" native_source;
  ignore (compile Lg.Target.Melange)

let test_nested_keyword_lookup_preserves_nullable_map_evidence () =
  let source =
    {|
(defrecord ReturnMap [type symbols])
(defrecord Query [qreturn-map])

(defn validate-return-map [query]
  (when-some [return-map (:qreturn-map query)]
    (:type return-map))
  (when-some [return-symbols (:symbols (:qreturn-map query))]
    (count return-symbols))
  true)

(println
  (str (validate-return-map (Query. nil)) ":"
       (validate-return-map
         (Query. (ReturnMap. :keys [:name])))))
|}
  in
  let native_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Native source |> expect_ok
  in
  assert_ocaml_runs "nested_keyword_lookup_preserves_nullable_map_evidence"
    "true:true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_symbol_predicate_narrows_dynamic_value_in_then_branch () =
  let source =
    {|
(type-record plain-symbol
  (symbol :symbol))

(defn parse-plain-symbol [form]
  (when (and (symbol? form)
             (not (= form 'reserved)))
    (record plain-symbol (symbol form))))

(defn static-string-guard []
  (let [form "text"]
    (when (symbol? form)
      1)))

(def parsed (parse-plain-symbol 'name))
(println
  (str (match parsed (Some value) (:symbol value) None "missing") ":"
       (nil? (parse-plain-symbol 42)) ":"
       (nil? (static-string-guard))))
|}
  in
  let native_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Native source |> expect_ok
  in
  assert_ocaml_runs "symbol_predicate_narrows_dynamic_value_in_then_branch"
    "name:true:true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_protocol_record_reconstruction_keeps_field_type_open () =
  let source =
    {|
(defprotocol ITraversable
  (-postwalk [_ f]))

(defn postwalk [form f]
  (if (satisfies? ITraversable form)
    (f (-postwalk form f))
    (f form)))

(defrecord Placeholder []
  ITraversable
  (-postwalk [_ _f]
    (Placeholder.)))

(defrecord Variable [symbol]
  ITraversable
  (-postwalk [_ f]
    (Variable. (postwalk symbol f))))

(defn parse-placeholder [form]
  (when (= '_ form)
    (Placeholder.)))

(defn parse-variable [form]
  (when (symbol? form)
    (Variable. form)))

(def variable (parse-variable 'name))
(println
  (str (some? (parse-placeholder '_)) ":"
       (match variable (Some value) (:symbol value) None "missing") ":"
       (nil? (parse-variable 42))))
|}
  in
  let native_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Native source |> expect_ok
  in
  assert_ocaml_runs "protocol_record_reconstruction_keeps_field_type_open"
    "true:name:true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_parser_alternatives_preserve_open_argument_type () =
  let source =
    {|
(defrecord Placeholder [])
(defrecord Variable [symbol])
(defrecord Constant [value])

(defn parse-placeholder [form]
  (when (= '_ form)
    (Placeholder.)))

(defn parse-variable [form]
  (when (and (symbol? form)
             (= (first (name form)) \?))
    (Variable. form)))

(defn parse-constant [form]
  (when-not (and (symbol? form)
                 (= (first (name form)) \?))
    (Constant. form)))

(defn parse-pattern-element [form]
  (or (parse-placeholder form)
      (parse-variable form)
      (parse-constant form)))

(println
  (str (some? (parse-pattern-element '_)) ":"
       (some? (parse-pattern-element '?name)) ":"
       (some? (parse-pattern-element 42))))
|}
  in
  let native_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Native source |> expect_ok
  in
  assert_ocaml_runs "parser_alternatives_preserve_open_argument_type"
    "true:true:true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_parser_rule_map_allocates_anonymous_return_record () =
  let source =
    {|
(defrecord PlainSymbol [symbol])
(defrecord RuleVars [required free])
(defrecord Rule [name branches])

(defn parse-plain-symbol [form]
  (when (symbol? form)
    (PlainSymbol. form)))

(defn parse-rule-vars [forms]
  (RuleVars. [] []))

(defn ^:dynamic parse-clauses [_forms]
  [1])

(defn parse-seq [parse-element forms]
  (reduce
    (fn [parsed form]
      (if-let [value (parse-element form)]
        (conj parsed value)
        (reduced nil)))
    []
    forms))

(defn parse-rule [form]
  (let [name (first form)
        vars (second form)
        clauses (nth form 2)
        name* (or (parse-plain-symbol name)
                  (throw (ex-info "missing name" {})))
        vars* (parse-rule-vars vars)
        clauses* (parse-clauses clauses)]
    {:name name*
     :vars vars*
     :clauses clauses*}))

(println
  (:symbol
    (:name
      (parse-rule ['query ['?x] [['?x :name "Ada"]]]))))
|}
  in
  let native_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Native source |> expect_ok
  in
  let anonymous_types = count_generated_anonymous_record_types native_source in
  if anonymous_types <> 1 then
    failwith
      (Printf.sprintf
         "function return maps must allocate one anonymous record type, got %d"
         anonymous_types);
  assert_ocaml_runs
    "parser_rule_map_allocates_anonymous_return_record" "query\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_rule_vars_projection_preserves_nominal_argument_type () =
  let source =
    {|
(defrecord RuleVars [required free])
(defrecord RuleBranch [vars clauses])

(defn parse-rule []
  {:name :rule
   :vars (RuleVars. [1] [2 3])
   :clauses [1]})

(defn rule-vars-arity [rule-vars]
  [(count (:required rule-vars)) (count (:free rule-vars))])

(defn validate-arity [branches]
  (let [vars0 (:vars (first branches))
        vars1 (:vars (second branches))
        vars2 (:vars (last branches))
        arity0 (rule-vars-arity vars0)
        _arity1 (rule-vars-arity vars1)
        _arity2 (rule-vars-arity vars2)]
    arity0))

(defn parse-rules []
  (let [branches
        (mapv #(RuleBranch. (:vars %) (:clauses %))
          [(parse-rule) (parse-rule) (parse-rule)])]
    (validate-arity branches)))

(println
  (first (parse-rules)))
|}
  in
  let native_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Native source |> expect_ok
  in
  assert_ocaml_runs "rule_vars_projection_preserves_nominal_argument_type" "1\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_for_let_shadowing_replaces_nominal_collection_type () =
  let source =
    {|
(defrecord RuleVars [required free])
(defrecord RuleBranch [vars clauses])
(defrecord Rule [name branches])
(defrecord PlainSymbol [symbol])

(defn parse-rule []
  {:name (PlainSymbol. "rule")
   :vars (RuleVars. [1] [2 3])
   :clauses [1]})

(defn rule-vars-arity [rule-vars]
  [(count (:required rule-vars)) (count (:free rule-vars))])

(defn validate-arity [name branches]
  (let [vars0 (:vars (first branches))
        arity0 (rule-vars-arity vars0)]
    (doseq [branch (next branches)
            :let [vars (:vars branch)]]
      (when (not= arity0 (rule-vars-arity vars))
        (println (:symbol name))))))

(defn parse-rules []
  (vec
    (for [[name branches]
          (group-by :name
            [(parse-rule) (parse-rule) (parse-rule)])
          :let [branches
                (mapv #(RuleBranch. (:vars %) (:clauses %)) branches)]]
      (do
        (validate-arity name branches)
        (Rule. name branches)))))

(println (:symbol (:name (first (parse-rules)))))
|}
  in
  let native_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Native source |> expect_ok
  in
  assert_ocaml_runs "for_let_shadowing_replaces_nominal_collection_type"
    "rule\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_symbol_predicate_narrows_later_and_operands () =
  let source =
    {|
(defn marker? [value]
  (= '% value))

(defn plain-symbol? [value]
  (and (symbol? value)
       (not (marker? value))))

(println (str (plain-symbol? 'name) ":" (plain-symbol? 42)))
|}
  in
  let native_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Native source |> expect_ok
  in
  assert_ocaml_runs "symbol_predicate_narrows_later_and_operands"
    "true:false\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nested_sequential_branch_destructuring_preserves_dynamic_values () =
  let source =
    {|
(defn split-sequential [form]
  (if (sequential? form)
    (let [[required rest]
          (if (sequential? (first form))
            [(first form) (next form)]
            [nil form])]
      [required rest])
    [nil nil]))

(println (count (split-sequential [1 2])))
|}
  in
  let native_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Native source |> expect_ok
  in
  assert_ocaml_runs
    "nested_sequential_branch_destructuring_preserves_dynamic_values"
    "2\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_if_let_callback_accepts_optional_and_required_results () =
  let source =
    {|
(defn parse-seq [parse-element forms]
  (reduce
    (fn [parsed form]
      (if-let [value (parse-element form)]
        (conj parsed value)
        (reduced nil)))
    [] forms))

(defn parse-optional [value]
  (when (pos? value) value))

(defn parse-required [value]
  (inc value))

(defprotocol Tagged
  (tagged-value [value]))

(defrecord Parsed [value]
  Tagged
  (tagged-value [_] value))

(defn parse-record [value]
  (when (pos? value)
    (Parsed. value)))

(println
  (str (count (parse-seq parse-optional [1 2])) ":"
       (count (parse-seq parse-required [1 2])) ":"
       (nil? (parse-seq parse-optional [1 0])) ":"
       (count (parse-seq parse-record [1 2]))))
|}
  in
  let native_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Native source |> expect_ok
  in
  assert_ocaml_runs "if_let_callback_accepts_optional_and_required_results"
    "2:2:true:2\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_external_protocol_implementation_prevents_field_misspecialization () =
  let source =
    {|
(defprotocol IFindVars
  (-find-vars [this]))

(defprotocol ITraversable
  (-postwalk [this f]))

(defn postwalk [form f]
  (if (satisfies? ITraversable form)
    (f (-postwalk form f))
    (f form)))

(defrecord Variable [symbol]
  ITraversable
  (-postwalk [_ f]
    (Variable. (postwalk symbol f))))

(extend-protocol IFindVars
  Variable
  (-find-vars [this] [(:symbol this)]))

(defrecord Aggregate [args]
  ITraversable
  (-postwalk [_ f]
    (Aggregate. (postwalk args f)))
  IFindVars
  (-find-vars [_] (-find-vars (last args))))

(defrecord Pull [variable]
  ITraversable
  (-postwalk [_ f]
    (Pull. (postwalk variable f)))
  IFindVars
  (-find-vars [_] (-find-vars variable)))

(defn parse-variable [value]
  (when (instance? Variable value) value))

(def parsed (parse-variable (Variable. "name")))
(def pull
  (if (and parsed)
    (Pull. parsed)
    nil))
(println
  (if-some [value pull]
    (first (-find-vars value))
    "missing"))
|}
  in
  let native_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Native source |> expect_ok
  in
  assert_ocaml_runs
    "external_protocol_implementation_prevents_field_misspecialization"
    "name\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_logical_or_with_throw_preserves_peer_type () =
  let source =
    {|
(defn parse-values [present]
  (when present [1 2]))

(defn require-values [present]
  (or
    (parse-values present)
    (throw (ex-info "missing values" {}))))

(println (count (require-values true)))
|}
  in
  let native_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Native source |> expect_ok
  in
  assert_ocaml_runs "logical_or_with_throw_preserves_peer_type" "2\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_equality_parameter_widens_across_keyword_and_string () =
  let source =
    {|
(defn tx-id? [value]
  (or (= value :db/current-tx)
      (= value ":db/current-tx")
      (= value "datascript.tx")))
(println (str (tx-id? :db/current-tx) ":"
              (tx-id? "datascript.tx") ":"
              (tx-id? :other)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "equality_parameter_widens_across_keyword_and_string"
    "true:true:false\n" ocaml_source

let test_contextual_equality_callback_preserves_map_key_type () =
  let source =
    {|
(defn remove-map-entries [key-pred values]
  (persistent!
    (reduce-kv
      (fn [result key value]
        (if (key-pred key)
          result
          (assoc! result key value)))
      (transient (empty values))
      values)))

(println
  (= {:b 2}
     (remove-map-entries #(= % :a) {:a 1 :b 2})))
|}
  in
  let native_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Native source |> expect_ok
  in
  assert_ocaml_runs "contextual_equality_callback_preserves_map_key_type"
    "true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_recursive_deftype_helper_widens_fallback_to_dynamic () =
  let source =
    {|
(declare value-at)
(defprotocol Lookup
  (-lookup [value key])
  (-lookup-default [value key not-found]))
(deftype Value [payload ^boolean added]
  Lookup
  (-lookup [value key] (value-at value key nil))
  (-lookup-default [value key not-found] (value-at value key not-found)))
(defn value-at [^Value value key not-found]
  (case key
    :payload (.-payload value)
    :added (.-added value)
    not-found))
(def value (Value. 42 true))
(println (str (value-at value :payload nil) ":"
              (value-at value :added nil) ":"
              (value-at value :missing "missing")))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "recursive_deftype_helper_widens_fallback_to_dynamic"
    "42:true:missing\n" ocaml_source

let test_vector_preserves_nullable_collection_elements () =
  let source =
    {|
(defn nullable-parts []
  (loop [only-left []
         only-right []
         both []
         left (seq [1])
         right (seq [2])]
    (cond
      (empty? left)
      [(not-empty only-left)
       (not-empty (into only-right right))
       (not-empty both)]

      (empty? right)
      [(not-empty (into only-left left))
       (not-empty only-right)
       (not-empty both)]

      :else
      (cond
        (= (first left) 1)
        (recur (conj only-left (first left))
               only-right
               both
               (next left)
               right)))))
(println "ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "vector_preserves_nullable_collection_elements"
    "ok\n" ocaml_source

let test_loop_normalizes_seqable_parameters_to_sequences () =
  let source =
    {|
(defn count-values [values]
  (loop [values values
         total 0]
    (if (empty? values)
      total
      (recur (next values) (inc total)))))
(println (count-values [1 2 3]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "loop_normalizes_seqable_parameters_to_sequences" "3\n"
    ocaml_source

let test_dynamic_higher_order_parameters_adapt_nominal_callbacks () =
  let source =
    {|
(deftype Entry [^int value])
(defn compare-entries [^Entry left ^Entry right]
  (compare (.-value left) (.-value right)))
(defn compare-heads [left right comparator]
  (let [left-value (first left)
        right-value (first right)]
    (try
      (comparator left-value right-value)
      (catch ClassCastException _
        :incomparable))))
(println (compare-heads [(Entry. 1)] [(Entry. 2)] compare-entries))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_higher_order_parameters_adapt_nominal_callbacks"
    "-1\n" ocaml_source

let test_dynamic_maps_preserve_nominal_function_parameters () =
  let source =
    {|
(deftype Entry [^int value])
(defn compare-entries [^Entry left ^Entry right]
  (compare (.-value left) (.-value right)))
(def opts (assoc {} :cmp compare-entries))
(println ((get opts :cmp) (Entry. 1) (Entry. 2)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_maps_preserve_nominal_function_parameters" "-1\n"
    ocaml_source

let test_dynamic_maps_preserve_propagated_nominal_function_parameters () =
  let source =
    {|
(deftype Entry [^int value])
(defn compare-entries [^Entry left ^Entry right]
  (compare (.-value left) (.-value right)))
(defn with-comparator [opts comparator]
  (assoc opts :cmp comparator))
(def opts (with-comparator {} compare-entries))
(println ((get opts :cmp) (Entry. 2) (Entry. 2)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "dynamic_maps_preserve_propagated_nominal_function_parameters" "0\n"
    ocaml_source

let test_dynamic_map_parameters_preserve_nominal_function_values () =
  let source =
    {|
(deftype Entry [^int value])
(defn compare-entries [^Entry left ^Entry right]
  (compare (.-value left) (.-value right)))
(defn with-entry-comparator [opts]
  (assoc opts :cmp compare-entries))
(def opts (with-entry-comparator {}))
(println ((get opts :cmp) (Entry. 3) (Entry. 2)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_map_parameters_preserve_nominal_function_values"
    "1\n" ocaml_source

let test_dynamic_maps_instantiate_generic_record_function_fields () =
  let source =
    {|
(type-record ordering [value]
  (compare-values :fn<value;value;int>))
(deftype Entry [^int value])
(defn compare-entries [^Entry left ^Entry right]
  (compare (.-value left) (.-value right)))
(defn make-ordering [comparator]
  (record ordering (compare-values comparator)))
(def entry-ordering (make-ordering compare-entries))
(def opts (assoc {} :ordering entry-ordering))
(println "ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_maps_instantiate_generic_record_function_fields"
    "ok\n" ocaml_source

let test_dynamic_protocols_instantiate_generic_record_receivers () =
  let source =
    {|
(type-record ordering [value]
  (compare-values :fn<value;value;int>))
(defn compare-indirect [comparator left right]
  (comparator left right))
(defprotocol CompareValues
  (compare-values-with [this left right] :int))
(extend-type ordering CompareValues
  (compare-values-with [this left right]
    (compare-indirect (:compare-values this) left right)))
(deftype Entry [^int value])
(defn compare-entries [^Entry left ^Entry right]
  (compare (.-value left) (.-value right)))
(defn make-ordering [comparator]
  (record ordering (compare-values comparator)))
(def entry-ordering (make-ordering compare-entries))
(def opts (assoc {} :ordering entry-ordering))
(println "ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_protocols_instantiate_generic_record_receivers"
    "ok\n" ocaml_source

let test_dynamic_generic_nominal_arguments_stay_scoped_to_the_call () =
  let source =
    {|
(type-record bucket [value]
  (root :ref<option<value>>)
  (compare-values :fn<value;value;int>))
(defn bucket-comparator [bucket]
  (:compare-values bucket))
(defprotocol FindBucket
  (-find-bucket [catalog ^:keyword key]))
(defrecord Catalog [^bucket bucket]
  FindBucket
  (-find-bucket [catalog ^:keyword key]
    (let [_typed (bucket-comparator (.-bucket catalog))]
      (bucket-comparator (get catalog key)))))
(def int-bucket
  (record bucket
    (root (volatile! (Some 1)))
    (compare-values (fn [left right] (compare left right)))))
(def catalog (Catalog. int-bucket))
(println ((-find-bucket catalog :bucket) 2 1))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_generic_nominal_arguments_stay_scoped_to_the_call"
    "1\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dynamic_generic_nominals_are_consumed_inside_existential_scope () =
  let pss_sources =
    [
      "datascript/me/tonsky/persistent_sorted_set/arrays.cljc";
      "datascript/me/tonsky/persistent_sorted_set/protocol.cljc";
      "datascript/me/tonsky/persistent_sorted_set.cljc";
    ]
    |> List.map read_file
  in
  let consumer_source =
    {|
(ns test.existential-consumer
  (:require [me.tonsky.persistent-sorted-set :as set]))
(defn compare-through-map [database index]
  (let [sorted-set (get database index)
        comparator (set/comparator sorted-set)]
    (if comparator 1 0)))
(def int-set
  (set/from-sequential (fn [^int left ^int right] (compare left right)) [1 2]))
(def database (assoc {} :eavt int-set))
(def filtered (filter (fn [_] true) int-set))
(defrecord Search [items]
  IReversible
  (-rseq [search]
    (let [items (.-items search)
          comparator (set/comparator items)
          _ (comparator 1 2)]
      (rseq items))))
(defn hold-dynamic [^:dynamic value] value)
(def packed-search (hold-dynamic (Search. int-set)))
(defprotocol IndexedSearch
  (-search [this]))
(defrecord SearchIndex [items]
  IndexedSearch
  (-search [search]
    (let [items (.-items search)
          comparator (set/comparator items)
          _ (comparator 1 2)]
      (set/slice items 0 10))))
(defrecord FilteredIndex [index]
  IndexedSearch
  (-search [_]
    (filter (fn [_] true) (-search index))))
(def packed-index (hold-dynamic (SearchIndex. int-set)))
(def packed-filtered-index
  (hold-dynamic (FilteredIndex. (SearchIndex. int-set))))
(defn has-items [items]
  (if (empty? items) 0 1))
(deftype Item [^int value])
(defn compare-items [^Item left ^Item right]
  (compare (.-value left) (.-value right)))
(def item-set
  (set/from-sequential compare-items [(Item. 1)]))
(defrecord ItemIndex [items]
  IndexedSearch
  (-search [index]
    (set/slice (.-items index) (Item. 0) (Item. 2))))
(defrecord ItemContext [source])
(def item-context
  (ItemContext. (ItemIndex. item-set)))
(defn choose-items [mode]
  (let [index (:source item-context)]
    (cond
      (= mode 0) (set/slice item-set (Item. 0) (Item. 2))
      (= mode 1) (-search index)
      (= mode 2) nil
      :else (take-while (fn [^Item _] true)
              (-search index)))))
(defrecord OptionalItems [^:option<dynamic> items])
(def optional-items
  (OptionalItems. (choose-items 0)))
(def dynamic-items
  (OptionalItems. (choose-items 1)))
(println
  (str (compare-through-map database :eavt) ":" (+ (first filtered) 40) ":"
       (+ (first (rseq int-set)) 40) ":"
       (has-items int-set) ":"
       (count (.-items optional-items)) ":"
       (instance? Item (first (.-items optional-items))) ":"
       (count (.-items dynamic-items)) ":"
       (instance? Item (first (.-items dynamic-items)))))
|}
  in
  let compile target =
    let state, outputs =
      List.fold_left
        (fun (state, outputs) source ->
          let state, output =
            Lg.Compiler.compile_chunk ~target state source |> expect_ok
          in
          (state, output :: outputs))
        (Lg.Compiler.empty_state, []) pss_sources
    in
    let _, output =
      Lg.Compiler.compile_chunk ~target state consumer_source |> expect_ok
    in
    String.concat "\n" (List.rev (output :: outputs))
  in
  let ocaml_source = compile Lg.Target.Native in
  let node_conj_start =
    expect_substring_index ocaml_source
      "let rec ((me_tonsky_persistent_sorted_set_node_conj)"
  in
  let node_disj_start =
    expect_substring_index ocaml_source
      "let rec ((me_tonsky_persistent_sorted_set_node_disj)"
  in
  let node_conj_source =
    String.sub ocaml_source node_conj_start
      (node_disj_start - node_conj_start)
  in
  if
    string_contains_substring node_conj_source
      "Lg_runtime.Runtime_dynamic"
  then
    failwith
      "node-conj should not require dynamic recursive-call specialization";
  if
    string_contains_substring ocaml_source
      "index: Lg_runtime.Runtime_dynamic.t"
  then
    failwith "protocol-backed defrecord fields must retain nominal evidence";
  assert_ocaml_runs
    "dynamic_generic_nominals_are_consumed_inside_existential_scope"
    "1:41:42:1:1:true:1:true\n" ocaml_source;
  ignore (compile Lg.Target.Melange)

let test_overloaded_generic_bounds_specialize_dynamic_nominal_arguments () =
  let pss_sources =
    [
      "datascript/me/tonsky/persistent_sorted_set/arrays.cljc";
      "datascript/me/tonsky/persistent_sorted_set/protocol.cljc";
      "datascript/me/tonsky/persistent_sorted_set.cljc";
    ]
    |> List.map read_file
  in
  let consumer_source =
    {|
(ns test.dynamic-slice
  (:require [me.tonsky.persistent-sorted-set :as set]))
(defrecord Datom [^int e])
(defrecord Database [^set/btset avet])
(defn compare-datoms [^Datom left ^Datom right]
  (compare (.-e left) (.-e right)))
(defn hold-dynamic [^:dynamic value] value)
(defn slice-database [^:dynamic database]
  (set/slice
    (.-avet ^Database database)
    (Datom. 1)
    (Datom. 3)))
(def datoms
  (set/from-sequential
    compare-datoms
    [(Datom. 1) (Datom. 2) (Datom. 3)]))
(def sliced
  (set/slice
    (hold-dynamic datoms)
    (Datom. 1)
    (Datom. 3)))
(def database (hold-dynamic (Database. datoms)))
(def sliced-from-database
  (slice-database database))
(println (str (count sliced) ":" (count sliced-from-database)))
|}
  in
  let compile target =
    let state, outputs =
      List.fold_left
        (fun (state, outputs) source ->
          let state, output =
            Lg.Compiler.compile_chunk ~target state source |> expect_ok
          in
          (state, output :: outputs))
        (Lg.Compiler.empty_state, []) pss_sources
    in
    let _, output =
      Lg.Compiler.compile_chunk ~target state consumer_source |> expect_ok
    in
    String.concat "\n" (List.rev (output :: outputs))
  in
  let ocaml_source = compile Lg.Target.Native in
  assert_ocaml_runs
    "overloaded_generic_bounds_specialize_dynamic_nominal_arguments" "3:3\n"
    ocaml_source;
  ignore (compile Lg.Target.Melange)

let test_map_to_record_unpacks_dynamic_named_fields () =
  let source =
    {|
(deftype Entry [^int value])
(defrecord Holder [^Entry entry])
(def holder (map->Holder (assoc {} :entry (Entry. 7))))
(println (.-value (.-entry holder)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "map_to_record_unpacks_dynamic_named_fields" "7\n"
    ocaml_source

let test_map_to_record_preserves_generic_fields_from_map_literals () =
  let source =
    {|
(type-record ordering [value]
  (compare-values :fn<value;value;int>))
(type-record holder [value]
  (ordering :ordering<value>)
  (extra :option<int>))
(deftype Entry [^int value])
(defn compare-entries [^Entry left ^Entry right]
  (compare (.-value left) (.-value right)))
(def entry-ordering
  (record ordering (compare-values compare-entries)))
(def holder
  (map->holder {:ordering entry-ordering :extra nil}))
(println "ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "map_to_record_preserves_generic_fields_from_map_literals"
    "ok\n" ocaml_source

let test_cond_thread_arrays_preserve_nominal_elements_for_sorting () =
  let source =
    {|
(deftype Entry [^int value])
(type-record entry-array
  (values :array<entry>))
(defn compare-entries [^Entry left ^Entry right]
  (compare (.-value left) (.-value right)))
(defn sorted-entries [values]
  (first (drop-while (fn [^Entry _] false) values))
  (let [result (cond-> values
                 (not (array-value? values)) (array-from))]
    (asort! compare-entries result)
    result))
(def from-vector
  (record entry-array
    (values (sorted-entries [(Entry. 2) (Entry. 1)]))))
(def from-array
  (record entry-array
    (values (sorted-entries (array (Entry. 4) (Entry. 3))))))
(println "ok")
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "cond_thread_arrays_preserve_nominal_elements_for_sorting" "ok\n"
    ocaml_source

let test_cond_thread_recognizes_namespaced_array_normalization_macros () =
  let arrays_source =
    {|
(ns arrays)
(defmacro array? [value]
  `(array-value? ~value))
(defmacro into-array [values]
  `(array-from ~values))
(defmacro asort [values cmp]
  `(let [values# ~values]
     (do
       (asort! ~cmp values#)
       values#)))
|}
  in
  let app_source =
    {|
(ns app
  (:require [arrays :as arrays]))
(deftype Entry [^int value])
(defn compare-entries [^Entry left ^Entry right]
  (compare (.-value left) (.-value right)))
(defn normalize [values]
  (first values)
  (let [result (cond-> values
                 (not (arrays/array? values)) (arrays/into-array))]
    (arrays/asort result compare-entries)
    result))
(println "ok")
|}
  in
  let state, arrays_ocaml =
    Lg.Compiler.compile_chunk Lg.Compiler.empty_state arrays_source |> expect_ok
  in
  let _, app_ocaml = Lg.Compiler.compile_chunk state app_source |> expect_ok in
  assert_ocaml_runs
    "cond_thread_recognizes_namespaced_array_normalization_macros" "ok\n"
    (arrays_ocaml ^ "\n" ^ app_ocaml)

let test_occurrence_type_hints_only_refine_their_branch () =
  let source =
    {|
(defrecord Base [^int value])
(defrecord Wrapped [^Base base])
(defn unwrap ^Base [value]
  (if (instance? Wrapped value)
    (.-base ^Wrapped value)
    value))
(def base (Base. 7))
(println (.-value (unwrap base)))
(println (.-value (unwrap (Wrapped. base))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "occurrence_type_hints_only_refine_their_branch" "7\n7\n"
    ocaml_source

let test_update_reads_dynamic_reduce_accumulators_dynamically () =
  let source =
    {|
(defn reduce-indexed [f init xs]
  "Same as reduce, but f takes [acc el idx]"
  (first
    (reduce
      (fn [[acc idx] x]
        (let [res (f acc x idx)]
          (if (reduced? res)
            (reduced [res idx])
            [res (inc idx)])))
      [init 0]
      xs)))
(def result
  (reduce
    (fn [m key]
      (reduce-indexed
        (fn [m nested-key index]
          (update m key assoc nested-key index))
        m
        [:value]))
    {}
    [:item]))
(def empty-result
  (reduce-indexed
    (fn [m key index]
      (assoc m key index))
    {}
    []))
(def stopped
  (reduce-indexed
    (fn [m key index]
      (if (= index 1)
        (reduced (assoc m :stopped key))
        (assoc m key index)))
    {}
    [:first :second :third]))
(println
  (str (= 0 (get (get result :item) :value)) ":"
       (= {} empty-result) ":"
       (reduced? stopped) ":"
       (= :second (get (unreduced stopped) :stopped))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "update_reads_dynamic_reduce_accumulators_dynamically"
    "true:true:true:true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_reduce_updates_heterogeneous_vector_accumulator_slots () =
  let source =
    {|
(defn resolve-value [value]
  (if (= value 1)
    (Some 10)
    None))
(defn split-values [values]
  (reduce
    (fn [acc value]
      (if-some [resolved (resolve-value value)]
        (update acc 1 assoc value resolved)
        (update acc 0 conj value)))
    [[] {}]
    values))
(def mixed (split-values [1 2]))
(def only-insert (split-values [2]))
(def empty-result (split-values []))
(println
  (str (= [2] (nth mixed 0)) ":"
       (= {1 10} (nth mixed 1)) ":"
       (= [2] (nth only-insert 0)) ":"
       (= {} (nth only-insert 1)) ":"
       (= [] (nth empty-result 0)) ":"
       (= {} (nth empty-result 1))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "reduce_updates_heterogeneous_vector_accumulator_slots"
    "true:true:true:true:true:true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_conj_is_available_as_a_first_class_core_function () =
  let source =
    {|
(def append conj)
(println (pr-str (append [1] 2)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "conj_is_available_as_a_first_class_core_function"
    "[1 2]\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_variadic_equality_is_available_in_dynamic_function_maps () =
  let source =
    {|
(def query-fns {'= =, 'not= not=})
(def equals (get query-fns '=))
(def differs (get query-fns 'not=))
(println
  (str (equals 1 1 1) ":" (equals 1 2) ":"
       (differs 1 2) ":" (differs 1 1)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "variadic_equality_is_available_in_dynamic_function_maps"
    "true:false:true:false\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_numeric_core_functions_are_available_in_dynamic_function_maps () =
  let source =
    {|
(def query-fns
  {'quot quot, 'rem rem, 'mod mod, 'inc inc, 'dec dec, 'max max, 'min min,
   'zero? zero?, 'pos? pos?, 'neg? neg?, 'even? even?, 'odd? odd?,
   'compare compare})
(println ((get query-fns 'quot) 7 3))
(println ((get query-fns 'rem) 7 3))
(println ((get query-fns 'mod) -7 3))
(println ((get query-fns 'inc) 4))
(println ((get query-fns 'dec) 4))
(println ((get query-fns 'max) 2 5 3))
(println ((get query-fns 'min) 2 5 3))
(println ((get query-fns 'zero?) 0))
(println ((get query-fns 'pos?) 1))
(println ((get query-fns 'neg?) -1))
(println ((get query-fns 'even?) 4))
(println ((get query-fns 'odd?) 3))
(println ((get query-fns 'compare) 2 3))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "numeric_core_functions_are_available_in_dynamic_function_maps"
    "2\n1\n2\n5\n3\n5\n2\ntrue\ntrue\ntrue\ntrue\ntrue\n-1\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_random_core_functions_are_available_in_dynamic_function_maps () =
  let source =
    {|
(def query-fns {'rand rand, 'rand-int rand-int})
(def random-unit ((get query-fns 'rand)))
(def random-scaled ((get query-fns 'rand) 10.0))
(def random-int ((get query-fns 'rand-int) 10))
(println
  (str (and (<= 0.0 random-unit) (< random-unit 1.0)) ":"
       (and (<= 0.0 random-scaled) (< random-scaled 10.0)) ":"
       (and (<= 0 random-int) (< random-int 10))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "random_core_functions_are_available_in_dynamic_function_maps"
    "true:true:true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_logical_core_functions_are_available_in_dynamic_function_maps () =
  let source =
    {|
(def query-fns
  {'true? true?, 'false? false?, 'nil? nil?, 'some? some?, 'not not,
   'complement complement, 'identical? identical?, 'identity identity,
   'keyword keyword, 'meta meta, 'name name, 'namespace namespace, 'type type})
(def not-nil ((get query-fns 'complement) (get query-fns 'nil?)))
(def tagged (with-meta [1] {:tag 2}))
(println
  (str ((get query-fns 'true?) true) ":"
       ((get query-fns 'false?) false) ":"
       ((get query-fns 'nil?) nil) ":"
       ((get query-fns 'some?) 1) ":"
       ((get query-fns 'not) nil) ":"
       (not-nil 1) ":"
       ((get query-fns 'identical?) tagged tagged) ":"
       ((get query-fns 'identity) 7) ":"
       ((get query-fns 'keyword) "item") ":"
       ((get query-fns 'keyword) "ns" "item") ":"
       (= {:tag 2} ((get query-fns 'meta) tagged)) ":"
       ((get query-fns 'name) :ns/item) ":"
       ((get query-fns 'namespace) :ns/item) ":"
       (some? ((get query-fns 'type) tagged))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "logical_core_functions_are_available_in_dynamic_function_maps"
    "true:true:true:true:true:true:true:7::item::ns/item:true:item:ns:true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_collection_core_functions_are_available_in_dynamic_function_maps () =
  let source =
    {|
(def query-fns
  {'vector vector, 'list list, 'set set, 'hash-map hash-map, 'array-map array-map,
   'count count, 'range range, 'not-empty not-empty, 'empty? empty?,
   'contains? contains?, 'str str, 'subs subs, 'get get})
(println
  (str (= [1 2] ((get query-fns 'vector) 1 2)) ":"
       (= '(1 2) ((get query-fns 'list) 1 2)) ":"
       (= #{1 2} ((get query-fns 'set) [1 1 2])) ":"
       (= 1 ((get query-fns 'get) ((get query-fns 'hash-map) :a 1) :a)) ":"
       (= 2 ((get query-fns 'get) ((get query-fns 'array-map) :a 2) :a)) ":"
       (= 2 ((get query-fns 'count) [1 2])) ":"
       (= '(1 3) ((get query-fns 'range) 1 5 2)) ":"
       (nil? ((get query-fns 'not-empty) [])) ":"
       ((get query-fns 'empty?) []) ":"
       ((get query-fns 'contains?) {:a 1} :a) ":"
       (= "a1" ((get query-fns 'str) "a" 1)) ":"
       (= "bc" ((get query-fns 'subs) "abcd" 1 3)) ":"
       (= 9 ((get query-fns 'get) {} :missing 9))))
(println (= ["1" "2"] (mapv str [1 2])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "collection_core_functions_are_available_in_dynamic_function_maps"
    "true:true:true:true:true:true:true:true:true:true:true:true:true\ntrue\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_printing_and_regex_core_functions_are_available_in_dynamic_maps () =
  let source =
    {|
(def query-fns
  {'pr-str pr-str, 'print-str print-str, 'println-str println-str,
   'prn-str prn-str, 're-find re-find, 're-matches re-matches,
   're-seq re-seq, 're-pattern re-pattern})
(def pattern ((get query-fns 're-pattern) "a+"))
(println
  (str (= "[1 2]" ((get query-fns 'pr-str) [1 2])) ":"
       (= "a 1" ((get query-fns 'print-str) "a" 1)) ":"
       (= "a\n" ((get query-fns 'println-str) "a")) ":"
       (= ":a\n" ((get query-fns 'prn-str) :a)) ":"
       (= "aaa" ((get query-fns 're-matches) pattern "aaa")) ":"
       (= "aa" ((get query-fns 're-find) pattern "caa")) ":"
       (= 2 (count ((get query-fns 're-seq) pattern "a aa")))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "printing_and_regex_core_functions_are_available_in_dynamic_maps"
    "true:true:true:true:true:true:true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_clojure_string_escape_is_available_in_dynamic_maps () =
  let source =
    {|
(ns string-query (:require [clojure.string :as str]))
(def query-fns {'escape str/escape})
(println (= "a&lt;b" ((get query-fns 'escape) "a<b" {\< "&lt;"})))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_string_escape_is_available_in_dynamic_maps"
    "true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_type_predicates_are_available_in_dynamic_function_maps () =
  let source =
    {|
(def query-fns
  {'number? number?, 'integer? integer?, 'string? string?,
   'boolean? boolean?, 'keyword? keyword?})
(println
  (str ((get query-fns 'number?) 1.5) ":"
       ((get query-fns 'integer?) 1) ":"
       ((get query-fns 'string?) "a") ":"
       ((get query-fns 'boolean?) false) ":"
       ((get query-fns 'keyword?) :a)))
(println (= [true true] (mapv string? ["a" "b"])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "type_predicates_are_available_in_dynamic_function_maps"
    "true:true:true:true:true\ntrue\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_overloaded_functions_pack_at_dynamic_map_boundaries () =
  let source =
    {|
(defn choose
  ([x] x)
  ([x y] y)
  ([x y & more] (count more)))
(def query-fns {'choose choose})
(def choose* (get query-fns 'choose))
(println (str (choose* 1) ":" (choose* 1 2) ":" (choose* 1 2 3 4)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "overloaded_functions_pack_at_dynamic_map_boundaries"
    "1:2:2\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_sort_accepts_dynamic_seqable_function_parameters () =
  let source =
    {|
(defn ordered [coll] (sort coll))
(defn ordered-by [coll] (sort compare coll))
(def query-fns {'ordered ordered, 'ordered-by ordered-by})
(println
  (str (= '(1 2 3) ((get query-fns 'ordered) [3 1 2])) ":"
       (= '(1 2 3) ((get query-fns 'ordered-by) [3 1 2]))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sort_accepts_dynamic_seqable_function_parameters"
    "true:true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_random_collection_operations_preserve_element_types () =
  let source =
    {|
(def values [1 2 3])
(def selected (rand-nth values))
(def shuffled (shuffle values))
(println
  (str (contains? #{1 2 3} selected) ":"
       (= #{1 2 3} (set shuffled)) ":"
       (= 3 (count shuffled))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "random_collection_operations_preserve_element_types"
    "true:true:true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dynamic_reduce_branch_merges_with_nullable_vector_fallback () =
  let source =
    {|
(defn resolve-result [found entity]
  (if-some [idents (if found (Some #{:id}) None)]
    (reduce-kv
      (fn [[entity' upserts] key value]
        [(assoc entity' key value) upserts])
      [{} {}]
      entity)
    [entity nil]))
(def resolved (resolve-result true {:a 1}))
(def fallback (resolve-result false {:a 1}))
(def empty-resolved (resolve-result true {}))
(println
  (str (= {:a 1} (nth resolved 0)) ":"
       (= {} (nth resolved 1)) ":"
       (= {:a 1} (nth fallback 0)) ":"
       (nil? (nth fallback 1)) ":"
       (= {} (nth empty-resolved 0)) ":"
       (= {} (nth empty-resolved 1))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_reduce_branch_merges_with_nullable_vector_fallback"
    "true:true:true:true:true:true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nested_reducers_keep_entity_keyword_lookup_as_map_access () =
  let source =
    {|
(defn entity-id-after-upserts [entity upserts]
  (let [upsert-ids
        (reduce-kv
          (fn [result attribute values-to-entities]
            (reduce-kv
              (fn [result value entity-id]
                (assoc result entity-id [attribute value]))
              result
              values-to-entities))
          {}
          upserts)
        upsert-count (count upsert-ids)]
    (if (<= 2 upsert-count)
      nil
      (let [[upsert-id [attribute value]] (first upsert-ids)
            entity-id (:db/id entity)]
        (if entity-id true false)))))
(def present-result
  (entity-id-after-upserts {:db/id 7} {:name {"Ada" 1}}))
(def missing-result
  (entity-id-after-upserts {} {:name {"Ada" 1}}))
(def empty-result
  (entity-id-after-upserts {:db/id 9} {}))
(def conflict-result
  (entity-id-after-upserts {:db/id 9}
    {:name {"Ada" 1} :email {"ada@example.com" 2}}))
(println
  (str present-result ":" missing-result ":" empty-result ":"
       (nil? conflict-result)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nested_reducers_keep_entity_keyword_lookup_as_map_access"
    "true:false:true:true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_get_uses_dynamic_lookup_for_dynamic_targets () =
  let source =
    {|
(defn dynamic-get [value key]
  (get ^:dynamic value key))
(println (dynamic-get {:answer 42} :answer))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "get_uses_dynamic_lookup_for_dynamic_targets" "42\n"
    ocaml_source

let test_dynamic_record_keys_preserve_common_generic_field_types () =
  let source =
    {|
(type-record box [value]
  (item :value))
(type-record catalog [value]
  (left :box<value>)
  (right :box<value>)
  (count :int))
(defprotocol CatalogInfo
  (-catalog-count [this]))
(extend-type catalog
  CatalogInfo
  (-catalog-count [this] (:count this)))
(defn box-item [box]
  (+ (:item box) 0))
(defn validate-catalog [catalog]
  (-catalog-count catalog))
(defn choose-box [catalog ^:keyword key]
  (let [selected (get catalog key)]
    (+ (box-item selected) (- (validate-catalog catalog) 2))))
(def catalog-value
  (record catalog
    (left (record box (item 20)))
    (right (record box (item 22)))
    (count 2)))
(println
  (str (choose-box catalog-value :left) ":"
       (choose-box catalog-value :right) ":"
       (try
         (choose-box catalog-value :count)
         (catch (Invalid_argument _) -1))))
|}
  in
  let state = typecheck_state source in
  (match Lg.Compiler_environment.find_opt "choose-box" state.env with
  | Some { ty = Lg.Types.TFn (receiver_ty :: _, _); _ } ->
      if Lg.Types.is_dynamic receiver_ty then
        failwith
          ("a dynamic record key must not erase concrete record and protocol evidence: "
          ^ Lg.Types.source_name receiver_ty);
      (match Lg.Types.constraint_value_type receiver_ty with
      | Lg.Types.TNamed_record
          { type_name = "catalog"; type_arguments = [ Lg.Types.TInt ]; _ } ->
          ()
      | value_ty ->
          failwith
            ("expected catalog<int> evidence, got "
            ^ Lg.Types.source_name value_ty))
  | Some binding ->
      failwith
        ("expected choose-box to be a function, got "
        ^ Lg.Types.source_name binding.ty)
  | None -> failwith "missing choose-box binding");
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_record_keys_preserve_common_generic_field_types"
    "20:22:-1\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_deferred_generic_protocol_parameters_compile () =
  let source =
    {|
(type-record holder [value]
  (value :value))
(defprotocol HolderValue
  (-holder-value [this]))
(extend-type holder
  HolderValue
  (-holder-value [this] (:value this)))
(declare validate-holder)
(defn read-holder [holder]
  (validate-holder holder))
(defn validate-holder [holder]
  (-holder-value holder))
(def int-holder (record holder (value 42)))
(def string-holder (record holder (value "answer")))
(defn read-int-holder []
  (read-holder int-holder))
(defn read-string-holder []
  (read-holder string-holder))
(declare identity-later)
(defn forward-identity [value]
  (identity-later value))
(defn identity-later [value]
  value)
(defn read-int-identity []
  (+ (forward-identity 1) 0))
(defn read-string-identity []
  (str (forward-identity "a")))
|}
  in
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_expected_types_flow_into_conditional_function_parameters () =
  let source =
    {|
(deftype Entry [^int value])
(declare dynamic-number)
(defn make-entry [fallback]
  (Entry.
    (if true
      (dynamic-number)
      fallback)))
(defn ^:dynamic dynamic-number []
  1)
(defn build-entry []
  (make-entry 42))
|}
  in
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nullable_and_generic_sequence_branches_merge () =
  let source =
    {|
(type-record box [value]
  (item :value))
(deftype Entry [^int value])
(defn maybe-items [flag box]
  (if flag
    (seq [(Entry. 1)])
    (filter (fn [_] true) [(:item box)])))
(def entry-box (record box (item (Entry. 42))))
(println (.-value ^Entry (first (maybe-items false entry-box))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nullable_and_generic_sequence_branches_merge" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_annotated_predicates_resolve_record_types_inside_seqable_constraints () =
  let source =
    {|
(deftype Entry [^int value])
(defn keep-entries [values]
  (filter (fn [^Entry _entry] true) values))
(defn first-entry-value []
  (.-value ^Entry (first (keep-entries [(Entry. 42)]))))
|}
  in
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_map_accepts_nullable_sequences_returned_by_protocols () =
  let source =
    {|
(defprotocol Values
  (-values [source]))
(deftype Entry [^int value])
(deftype Source [^int value]
  Values
  (-values [_]
    (seq [(Entry. value)])))
(defn entry-values [source]
  (map (fn [^Entry entry] (.-value entry)) (-values source)))
(println (first (entry-values (Source. 42))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "map_accepts_nullable_sequences_returned_by_protocols"
    "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_map_normalizes_mixed_nullable_protocol_sequence_returns () =
  let source =
    {|
(defprotocol Values
  (-values [source]))
(deftype Entry [^int value])
(declare maybe-entry-seq)
(defrecord OptionalSource [marker ^Entry entry]
  Values
  (-values [_]
    (maybe-entry-seq entry)))
(deftype SequenceSource [^int value]
  Values
  (-values [_]
    (filter (fn [^Entry _entry] true) [(Entry. value)])))
(defn maybe-entry-seq [^Entry entry]
  (if true
    (Some (filter (fn [^Entry _entry] true) [entry]))
    None))
(defn entry-values [source]
  (map (fn [^Entry entry] (.-value entry)) (-values source)))
(defn optional-entry-values []
  (entry-values (OptionalSource. "marker" (Entry. 41))))
(defn sequence-entry-values []
  (entry-values (SequenceSource. 42)))
|}
  in
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nullable_values_pack_across_dynamic_logical_boundaries () =
  let source =
    {|
(defn ^number maybe-number [found value]
  (if found value nil))
(defn ^number strict-number [found value]
  (or
    (maybe-number found value)
    (throw (ex-info "missing" {}))))
(defn strict-default [found value]
  (or (maybe-number found value) 7))
(println
  (str (+ (strict-number true 42) 0) ":"
       (+ (strict-default false 42) 0)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nullable_values_pack_across_dynamic_logical_boundaries"
    "42:7\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nullable_cond_branches_pack_into_dynamic_results () =
  let source =
    {|
(defn dynamic-value [^:dynamic value]
  value)
(defn choose-value [dynamic? value present?]
  (cond
    dynamic? (dynamic-value value)
    :else (if present? 42 nil)))
(println
  (str (choose-value true 41 true) ":"
       (choose-value false 0 true) ":"
       (nil? (choose-value false 0 false))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nullable_cond_branches_pack_into_dynamic_results"
    "41:42:true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_update_in_uses_dynamic_callbacks_for_dynamic_record_fields () =
  let source =
    {|
(defmacro update [m k f & more]
  `(let [m# ~m
         k# ~k]
     (assoc m# k# (~f (get m# k#) ~@more))))
(defrecord Holder [value])
(defn remove-key [holder ^:dynamic key]
  (-> holder
      (assoc-in [:value :a] 1)
      (update-in [:value] #(dissoc % key))))
(defn remove-nested-key [holder ^:dynamic key]
  (update-in holder [:value :nested] #(dissoc % key)))
(defn remove-dynamic-entry [^:dynamic target ^:dynamic key]
  (clojure.core/update target key #(dissoc % :a)))
(def updated (remove-key (Holder. {:a 0 :b 2}) :b))
(def nested-updated
  (remove-nested-key
    (Holder.
      (hash-map
        (keyword "nested")
        (hash-map (keyword "a") 1 (keyword "b") 2)))
    :b))
(def direct-updated
  (remove-dynamic-entry
    (hash-map
      (keyword "nested")
      (hash-map (keyword "a") 1 (keyword "b") 2))
    (keyword "nested")))
(println
  (str (get (.-value updated) :a) ":"
       (nil? (get (.-value updated) :b)) ":"
       (nil? (get (get (.-value nested-updated) :nested) :b)) ":"
       (nil? (get (get direct-updated :nested) :a))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "update_in_uses_dynamic_callbacks_for_dynamic_record_fields"
    "1:true:true:true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dynamic_callable_type_variables_propagate_to_arguments () =
  let source =
    {|
(defn lookup-two [lookup first-key second-key]
  (lookup first-key)
  (lookup second-key))
(defn use-lookup [^:dynamic lookup]
  (lookup-two lookup 1 (keyword "answer")))
(def lookup-map
  (hash-map 1 "one" (keyword "answer") "ok"))
(def lookup-result (use-lookup lookup-map))
|}
  in
  ignore (Lg.Compiler.compile_string source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_generic_calls_unpack_dynamic_nominal_arguments () =
  let source =
    {|
(deftype Entry [^int value])
(type-record entry-consumer [value]
  (consume :fn<value;int>))
(type-record entry-set [value]
  (sample :value))
(defn consume-entry [consumer entry]
  ((:consume consumer) entry))
(defn forward-consume [consumer entry]
  (consume-entry consumer entry))
(def consumer
  (record entry-consumer
    (consume (fn [^Entry entry] (.-value entry)))))
(def entry-set-value
  (record entry-set (sample (Entry. 7))))
(defn compare-entries [^Entry left ^Entry right]
  (compare (.-value left) (.-value right)))
(defn remove-generic [set value comparator]
  (comparator (:sample set) value)
  set)
(defn find-entry [^:dynamic values]
  (first values))
(defn find-value [values]
  (if-some [entry (find-entry values)]
    (forward-consume consumer entry)
    0))
(defn remove-found [values]
  (if-some [entry (find-entry values)]
    (remove-generic entry-set-value entry compare-entries)
    entry-set-value))
(def removed (remove-found [(Entry. 42)]))
(println
  (+ (find-value [(Entry. 42)])
     (.-value ^Entry (:sample removed))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_calls_unpack_dynamic_nominal_arguments"
    "49\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_some_infers_generic_seqable_parameters () =
  let source =
    {|
(defn find-wanted [attrs]
  (some
    (fn [attr]
      (when (= attr :wanted) attr))
    attrs))
(println (str (find-wanted [:other :wanted]) ":"
              (nil? (find-wanted (list :other))) ":"
              (nil? (find-wanted []))))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "some_infers_generic_seqable_parameters"
    ":wanted:true:true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_map_key_evidence_preserves_generic_map_values () =
  let source =
    {|
(def schema-keys #{:db/ident :db/cardinality})
(defn schema-entity? [entity]
  (some (fn [key] (contains? entity key)) schema-keys))
(defn schema? [entity]
  (and (:db/ident entity) (:db/cardinality entity)))
(defn read-cardinality [entity]
  (when (and (schema-entity? entity)
             (contains? entity :db/ident)
             (schema? entity))
    (:db/cardinality entity)))
(defn read-value [entity]
  (when (contains? entity :value)
    (:value entity)))

(println
  (str (read-cardinality {:db/ident :name :db/cardinality :one}) ":"
       (nil? (read-cardinality {:other :value})) ":"
       (read-value {:value 42}) ":"
       (nil? (read-value {:other 42}))))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "map_key_evidence_preserves_generic_map_values"
    ":one:true:42:true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_filter_accepts_nullable_truthy_predicate_results () =
  let source =
    {|
(defn retain-allowed [values]
  (filter
    (fn [value]
      (some (fn [candidate] (= candidate value)) [2 3]))
    values))

(println (pr-str (vec (retain-allowed [1 2 3 4]))))
(println (empty? (retain-allowed [])))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "filter_accepts_nullable_truthy_predicate_results"
    "[2 3]\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_remove_specializes_nested_predicate_type_variables () =
  let source =
    {|
(defn remove-pairs [xs ys]
  (let [pairs (->> (map vector xs ys)
                   (remove (fn [[x y]] (= x y))))]
    [(map first pairs) (map second pairs)]))
(let [[left right] (remove-pairs [1 2 3] [0 2 4])]
  (println (str (reduce + 0 left) ":" (reduce + 0 right))))
(let [[left right] (remove-pairs [1 2] [1 2])]
  (println (str (empty? left) ":" (empty? right))))
(let [[left right] (remove-pairs [] [])]
  (println (str (count left) ":" (count right))))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "remove_specializes_nested_predicate_type_variables"
    "4:4\ntrue:true\n0:0\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nested_destructuring_materializes_erased_sequence_elements () =
  let source =
    {|
(def builtins {'known true})
(defn describe [clause]
  (let [[[f & args]] clause
        pred (get builtins f)]
    (if pred
      (str f ":" (count args))
      (str "Unknown predicate '" f "' in " clause))))

(println (describe [['known 1 2]]))
(println (describe [['missing]]))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nested_destructuring_materializes_erased_sequence_elements"
    "known:2\nUnknown predicate 'missing' in [[missing]]\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_swap_conj_refines_atom_collection_elements () =
  let source =
    {|
(defn collect-matching [value pred]
  (let [result (atom [])]
    (when (pred value)
      (swap! result conj value))
    @result))

(println (contains? (set (collect-matching 'item symbol?)) 'item))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "swap_conj_refines_atom_collection_elements" "true\n"
    native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_concat_specializes_unknown_elements_from_static_prefix () =
  let source =
    {|
(defn rule-guard [[_ & call-args] [_ & prev-args]]
  (concat ['-differ?] call-args prev-args))
(println
  (reduce
    (fn [result item] (str result ":" (name item)))
    ""
    (rule-guard ['ignored '?x] ['ignored '?y])))
(println (count (rule-guard ['ignored] ['ignored])))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "concat_specializes_unknown_elements_from_static_prefix"
    ":-differ?:?x:?y\n1\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_conditional_conj_specializes_empty_sets_from_guarded_values () =
  let source =
    {|
(defn selected-symbols [pattern]
  (let [[entity _ _ tx] pattern]
    (cond-> #{}
      (symbol? entity) (conj entity)
      (symbol? tx) (conj tx))))
(def both (selected-symbols ['?e :attr 1 '?tx]))
(def one (selected-symbols ['?e :attr 1 99]))
(def none (selected-symbols [10 :attr 1 99]))
(println
  (str (count both) ":" (contains? both '?e) ":" (contains? both '?tx)))
(println
  (str (count one) ":" (contains? one '?e) ":" (contains? one '?tx)))
(println (count none))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "conditional_conj_specializes_empty_sets_from_guarded_values"
    "2:true:true\n1:true:false\n0\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_conditional_conj_localizes_dynamic_sets_for_custom_guards () =
  let source =
    {|
(defn free-var? [value] (symbol? value))
(defn selected-vars [pattern]
  (let [[entity _ _ tx] pattern]
    (cond-> #{}
      (free-var? entity) (conj entity)
      (free-var? tx) (conj tx))))
(def both (selected-vars ['?e :attr 1 '?tx]))
(def one (selected-vars ['?e :attr 1 99]))
(def none (selected-vars [10 :attr 1 99]))
(println
  (str (count both) ":" (contains? both '?e) ":" (contains? both '?tx)))
(println
  (str (count one) ":" (contains? one '?e) ":" (contains? one '?tx)))
(println (count none))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "conditional_conj_localizes_dynamic_sets_for_custom_guards"
    "2:true:true\n1:true:false\n0\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_select_keys_accepts_runtime_seqable_key_collections () =
  let source =
    {|
(def attrs (zipmap ['?e '?a '?v] [1 2 3]))
(def selected (select-keys attrs #{'?v '?missing '?e}))
(def listed (select-keys attrs (list '?a '?missing)))
(def empty-selection (select-keys attrs #{}))
(def dynamic-selection (select-keys {'?e 1 '?a 2} (list '?a '?missing)))
(println
  (str (count selected) ":" (get selected '?e) ":" (get selected '?v)
       ":" (contains? selected '?a)))
(println (str (count listed) ":" (get listed '?a)))
(println (count empty-selection))
(println (str (count dynamic-selection) ":" (get dynamic-selection '?a)))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "select_keys_accepts_runtime_seqable_key_collections"
    "2:1:3:false\n1:2\n0\n1:2\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  Lg.Compiler.compile_string
    {|(def bad (select-keys (zipmap ['?e] [1]) [1]))|}
  |> expect_error_contains "select-keys"

let test_select_keys_infers_generic_runtime_map_record_fields () =
  let source =
    {|
(defn select-attrs [item keys]
  (select-keys (:attrs item) keys))
(defn limit-item [item keys]
  (when-some [selected (not-empty (select-keys (:attrs item) keys))]
    (assoc item :attrs selected)))
(def item {:attrs (zipmap ['?e '?a] [1 2])})
(def selected (select-attrs item #{'?a '?missing}))
(def empty-selection (select-attrs item #{}))
(def limited (limit-item item #{'?a}))
(def absent (limit-item item #{}))
(println
  (str (count selected) ":" (get selected '?a) ":"
       (contains? selected '?e)))
(println (count empty-selection))
(println
  (str (if-some [present limited] (get (:attrs present) '?a) 0)
       ":" (nil? absent)))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "select_keys_infers_generic_runtime_map_record_fields"
    "1:2:false\n0\n2:true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_select_keys_projects_open_row_extension_fields () =
  let source =
    {|
(defn settings [{:as opts}]
  (select-keys opts [:branching-factor :ref-type :missing]))
(def selected (settings {:branching-factor 32 :ref-type :int}))
(println
  (str (= 32 (get selected :branching-factor)) ":"
       (= :int (get selected :ref-type)) ":"
       (= 2 (count selected))))
(println (empty? (settings {})))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "select_keys_projects_open_row_extension_fields"
    "true:true:true\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_keep_drops_only_nil_across_generic_seqables () =
  let source =
    {|
(def calls (atom 0))
(defn keep-even [values]
  (keep
    (fn [value]
      (do
        (reset! calls (+ (deref calls) 1))
        (if (even? value) value nil)))
    values))
(def kept (keep-even [1 2 3 4]))
(println (pr-str (vec kept)))
(println (deref calls))
(println
  (pr-str
    (vec (keep (fn [value] (if (= value 1) false nil)) [1 2]))))
(println (pr-str (vec (keep inc [1 2]))))
(println (empty? (keep inc [])))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "keep_drops_only_nil_across_generic_seqables"
    "[2 4]\n4\n[false]\n[2 3]\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_map_indexed_infers_generic_seqable_parameters () =
  let source =
    {|
(defn indexed-sums [values]
  (vec (map-indexed (fn [index value] (+ index value)) values)))
(println (pr-str (indexed-sums [10 20 30])))
(println (pr-str (indexed-sums (list 4 5))))
(println (empty? (indexed-sums [])))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "map_indexed_infers_generic_seqable_parameters"
    "[10 21 32]\n[4 6]\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_mapv_infers_destructured_callback_parameters () =
  let source =
    {|
(defn transform-value [value] (inc value))
(defn dict-get [dictionary key] (get dictionary key))
(defn transform
  [container {:keys [read-fn transform-value]
              :or {read-fn identity transform-value transform-value}
              :as opts}]
  (let [_read (read-fn 1)]
    (if (contains? opts :skip)
      []
      (->> (dict-get container :values) (mapv transform-value)))))
(println (= [2 3 4] (transform {:values [1 2 3]} {})))
(println (= [4 8] (transform {:values (list 2 4)} {:transform-value (fn [value] (* value 2))})))
(println (empty? (transform {:values []} {})))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "mapv_infers_destructured_callback_parameters"
    "true\ntrue\ntrue\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_destructured_defaults_specialize_overloaded_function_values () =
  let source =
    {|
(defn render [value {:keys [freeze-fn] :or {freeze-fn pr-str}}]
  (freeze-fn value))
(def rendered (render 42 {}))
(println (string? rendered))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "destructured_defaults_specialize_overloaded_function_values"
    "true\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_nested_loops_keep_recur_return_types_scoped () =
  let source =
    {|
(def reader
  (loop [outer 0]
    (if (< outer 2)
      (recur (inc outer))
      (fn [value]
        (loop [inner 0]
          (if (< inner 2)
            (recur (inc inner))
            [value]))))))
(println (+ 0 (first (reader 42))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nested_loops_keep_recur_return_types_scoped" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_loop_recur_remains_tail_through_macros () =
  let source =
    {|
(defmacro tail-if [condition then else]
  `(if ~condition ~then ~else))
(def result
  (loop [value 0]
    (tail-if (= value 5)
      value
      (recur (inc value)))))
(println result)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "loop_recur_remains_tail_through_macros" "5\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

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
  |> expect_error "recur argument 1 must be int, got string";
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

let test_heterogeneous_destructuring_materializes_dynamic_elements () =
  let source =
    {|
(deftype Item [^int id value])
(defn build-item [pattern]
  (let [[id value] pattern]
    (Item. id value)))
(def item (build-item [42 :answer]))
(println (.-id item))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "heterogeneous_destructuring_materializes_dynamic_elements"
    "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_dynamic_predicates_do_not_erase_concrete_array_elements () =
  let source =
    {|
(defprotocol Combine
  (combine [this other]))
(deftype Item [^int id]
  Combine
  (combine [_ ^Item other] other))
(defn item? [value] (instance? Item value))
(defn compare-items [^Item left ^Item right]
  (compare (.-id left) (.-id right)))
(defn prepare [items]
  (drop-while item? items)
  (let [arr (array-from items)
        _   (asort! compare-items arr)]
    arr))
(def arr (prepare [(Item. 2) (Item. 1)]))
(println (.-id (unsafe-aget arr 0)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_predicates_do_not_erase_concrete_array_elements"
    "1\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_dynamic_arrays_preserve_array_identity_and_array_seq () =
  let source =
    {|
(defn classify [value]
  (cond
    (int? value) value
    (array? value) (first (array-seq value))
    (seq? value) -1
    :else -2))
(println (classify (array 42)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_arrays_preserve_array_identity_and_array_seq"
    "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source
    |> expect_ok)

let test_dynamic_recursive_array_seq_is_packed_at_the_self_call () =
  let source =
    {|
(defprotocol LookupStore
  (-lookup-value [db key])
  (-seek-value [db key lower upper]))
(defrecord DB [^int value]
  LookupStore
  (-lookup-value [db _] (.-value db))
  (-seek-value [db _ _ _] (.-value db)))
(defn classify [db value]
  (cond
    (int? value) (-lookup-value db :value)
    (array? value) (recur db (array-seq value))
    (sequential? value) (+ (count value) (-seek-value db :value nil nil))
    :else -1))
(println (classify (DB. 40) (array 40 41)))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_recursive_array_seq_is_packed_at_the_self_call"
    "42\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nested_callback_record_constraints_do_not_emit_fake_types () =
  let source =
    {|
(defn call-store [node storage]
  (let [value (:value node)
        store (:store storage)]
    (store node)
    value))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  if string_contains_substring ocaml_source " record" then
    failwith "structural callback constraints must not emit a fake record type";
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_cross_namespace_named_records_project_to_callback_rows () =
  let nodes_source =
    {|
(ns test.nodes)
(type-record tree [value]
  (children :array<value>))
(type-record storage [value]
  (store :fn<tree<value>;int;int>))
(defn make-tree [values]
  (record tree (children values)))
(defn make-storage [store]
  (record storage (store store)))
(defn node-store [node storage]
  (let [_     (:children node)
        store (:store storage)]
    (store node 0)))
|}
  in
  let set_source =
    {|
(ns test.set
  (:require [test.nodes :as nodes]))
(type-record holder [value]
  (backing :storage<value>))
(def root
  (nodes/make-tree (array 10 20)))
(def backing
  (nodes/make-storage (fn [_ count] count)))
(def container
  (record holder (backing backing)))
(def backing-value
  (:backing container))
(def result
  (nodes/node-store root backing-value))
|}
  in
  let compile target =
    let target_name =
      match target with
      | Lg.Target.Native -> "native"
      | Lg.Target.Melange -> "melange"
      | Lg.Target.Js_of_ocaml -> "js_of_ocaml"
    in
    let expect_target = function
      | Ok value -> value
      | Error (error : Lg.Compiler.compile_error) ->
          failwith (target_name ^ ": " ^ error.message)
    in
    let state, nodes_ocaml =
      Lg.Compiler.compile_chunk ~target Lg.Compiler.empty_state nodes_source
      |> expect_target
    in
    let _, set_ocaml =
      Lg.Compiler.compile_chunk ~target state set_source |> expect_target
    in
    nodes_ocaml ^ "\n" ^ set_ocaml
  in
  ignore (compile Lg.Target.Native);
  ignore (compile Lg.Target.Melange);
  ignore (compile Lg.Target.Js_of_ocaml)

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

let test_row_types_bind_named_record_parameters () =
  let source =
    {|
(type-record box [value]
  (item :value))
(defn boxed-item [{:keys [box]}]
  (.-item box))
(def value (record box (item 42)))
(println (boxed-item {:box value}))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "row_types_bind_named_record_parameters" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_named_record_constraints_keep_stronger_nested_evidence () =
  let source =
    {|
(type-record datom
  (e :int))
(type-record btset [value]
  (item :value))
(defrecord DB [^btset eavt])
(defrecord TxReport [^DB db-before])
(type-record datom-holder
  (eavt :btset<datom>))
(defn require-datom-set [set]
  (record datom-holder (eavt set)))
(defn inspect-db [^DB database]
  (:eavt database))
(defn transact [^TxReport report]
  (inspect-db (:db-before report))
  (require-datom-set (:eavt (:db-before report))))
(def datom-value (record datom (e 42)))
(def datom-set (record btset (item datom-value)))
(def database (DB. datom-set))
(def report (TxReport. database))
(def holder (transact report))
(println (:e (:item (:eavt holder))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "named_record_constraints_keep_stronger_nested_evidence"
    "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_dynamic_map_row_preserves_static_generic_field () =
  let source =
    {|
(type-record datom
  (e :int))
(type-record btset [value]
  (item :value)
  (slot :ref<value>))
(type-record datom-holder
  (eavt :btset<datom>))
(defrecord Database [^btset eavt])
(defn restore [{:keys [eavt marker]}]
  (record datom-holder (eavt eavt)))
(defn rebuild [^Database db]
  (restore {:eavt (:eavt db), :marker nil}))
(def datom-value (record datom (e 42)))
(def datom-set
  (record btset
    (item datom-value)
    (slot (volatile! datom-value))))
(def db (Database. datom-set))
(def holder (rebuild db))
(println (:e (:item (:eavt holder))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_map_row_preserves_static_generic_field" "42\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_value_type_hints_preserve_nullable_record_values () =
  let source =
    {|
(defrecord Datom [value])
(defn maybe-datom [pick]
  (if pick (Datom. 42) nil))
(let [^Datom datom (maybe-datom false)]
  (println (nil? datom)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "value_type_hints_preserve_nullable_record_values"
    "true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_defrecord_field_hints_preserve_inferred_nullability () =
  let source =
    {|
(defrecord Item [value])
(defrecord Cursor [^Item current remaining]
  Object
  (toString [_]
    (str (nil? current))))
(defn advance [^Cursor cursor]
  (Cursor. (first (.-remaining cursor))
           (next (.-remaining cursor))))
(def advanced (advance (Cursor. (Item. 1) (list))))
(println (nil? (.-current advanced)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "defrecord_field_hints_preserve_inferred_nullability"
    "true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_defrecord_self_constructors_preserve_hinted_field_nullability () =
  let source =
    {|
(defprotocol Step
  (-current-value [cursor])
  (-advance [cursor]))
(defrecord Item [value])
(defn item-value [^Item item]
  (.-value item))
(defn first-seq [xs]
  (first xs))
(defrecord Cursor [^Item current remaining]
  Step
  (-current-value [_]
    (item-value current))
  (-advance [_]
    (Cursor. (first-seq remaining) (next remaining))))
(def advanced (-advance (Cursor. (Item. 1) (list))))
(println (nil? (.-current advanced)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "defrecord_self_constructors_preserve_hinted_field_nullability"
    "true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_explicit_type_hints_narrow_nullable_field_receivers () =
  let source =
    {|
(deftype Datom [^int value])
(defn maybe-datom [pick]
  (if pick (Datom. 42) nil))
(println (.-value ^Datom (maybe-datom true)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "explicit_type_hints_narrow_nullable_field_receivers"
    "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nested_record_fields_preserve_outer_record_inference () =
  let source =
    {|
(defrecord DB [max-tx])
(defrecord TxReport [^DB db-before ^DB db-after tx-data])
(defrecord Datom [value])
(def conjv (fnil conj []))
(defn maybe-datom [present? value]
  (if present? (Datom. value) nil))
(defn transact-report [report datom]
  (let [before ^DB (:db-before report)
        db (:db-after report)
        value (:value datom)
        report' (update report :tx-data conj datom)]
    report'))
(defn transact-add [report old-datom value]
  (let [db (:db-after report)
        report' (assoc report :extra true)
        new-datom (Datom. value)]
    (cond
      (nil? old-datom)
      (transact-report report' new-datom)

      (= (:value old-datom) value)
      (update report' :tx-redundant conjv new-datom)

      :else
      (-> report'
          (transact-report old-datom)
          (transact-report new-datom)))))
(def initial (TxReport. (DB. 1) (DB. 1) []))
(def inserted (transact-add initial nil 2))
(def redundant (transact-add inserted (maybe-datom true 2) 2))
(def repeated (transact-add redundant (maybe-datom true 2) 2))
(def replaced (transact-add initial (maybe-datom true 1) 2))
(println
  (str (count (:tx-data initial)) ":"
       (count (:tx-data inserted)) ":"
       (count (:tx-data redundant)) ":"
       (count (:tx-redundant redundant)) ":"
       (count (:tx-redundant repeated)) ":"
       (count (:tx-data replaced)) ":"
       (instance? Datom (first (:tx-redundant repeated))) ":"
       (instance? Datom (last (:tx-data replaced)))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nested_record_fields_preserve_outer_record_inference"
    "0:1:1:1:2:2:true:true\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_cross_module_fnil_update_packs_nominal_vectors () =
  let util_source =
    {|
(ns test.util)
(def conjv (fnil conj []))
|}
  in
  let db_source =
    {|
(ns test.db
  (:require [test.util :as util]))
(defrecord DB [max-tx])
(defrecord TxReport [^DB db-before ^DB db-after tx-data])
(defprotocol IDatom
  (datom-value [this]))
(deftype Datom [value]
  IDatom
  (datom-value [_] value))
(defn ^Datom datom [value]
  (Datom. value))
(defn maybe-datom [present? value]
  (if present? (datom value) nil))
(defn transact-report [report datom]
  (let [_ (datom-value datom)]
    (update report :tx-data conj datom)))
(defn transact-add [report [_ present? old-value value :as entity]]
  (let [new-datom (datom value)
        old-datom ^Datom (maybe-datom present? old-value)]
    (cond
      (nil? old-datom)
      (transact-report report new-datom)

      (= (.-value ^Datom old-datom) value)
      (update report :tx-redundant util/conjv new-datom)

      :else
      (-> report
          (transact-report old-datom)
          (transact-report new-datom)))))
(def initial (TxReport. (DB. 1) (DB. 1) []))
(def inserted (transact-add initial [:add false 0 2]))
(def redundant (transact-add inserted [:add true 2 2]))
(def repeated (transact-add redundant [:add true 2 2]))
(def replaced (transact-add initial [:add true 1 2]))
(println
  (str (count (:tx-data initial)) ":"
       (count (:tx-data inserted)) ":"
       (count (:tx-data redundant)) ":"
       (count (:tx-redundant redundant)) ":"
       (count (:tx-redundant repeated)) ":"
       (count (:tx-data replaced)) ":"
       (instance? Datom (first (:tx-redundant repeated))) ":"
       (instance? Datom (last (:tx-data replaced)))))
|}
  in
  let compile target =
    let state, util_ocaml =
      Lg.Compiler.compile_chunk ~target Lg.Compiler.empty_state util_source
      |> expect_ok
    in
    let _, db_ocaml =
      Lg.Compiler.compile_chunk ~target state db_source |> expect_ok
    in
    util_ocaml ^ "\n" ^ db_ocaml
  in
  let native_source = compile Lg.Target.Native in
  assert_ocaml_runs "cross_module_fnil_update_packs_nominal_vectors"
    "0:1:1:1:2:2:true:true\n" native_source;
  ignore (compile Lg.Target.Melange)

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

let test_map_destructuring_supports_typed_direct_keyword_bindings () =
  let source =
    {|
(defrecord Context [value])
(defrecord Pattern [name])
(defn describe [{^Context context :context ^Pattern pattern :pattern}]
  (str (.-value context) ":" (.-name pattern)))
(println
  (describe {:context (Context. 42)
             :pattern (Pattern. "all")}))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "map_destructuring_supports_typed_direct_keyword_bindings"
    "42:all\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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
(def empty-tuples? (comp empty? :tuples))
(def always-ok (constantly "ok"))
(println (str (add10-after-double 4) ":" (identity 7) ":" (always-ok false) ":"
              (apply + [1 2 3]) ":" (apply + (hash-set 1 2 3)) ":"
              (empty-tuples? {:tuples []})))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "function_helpers" "18:7:ok:6:6:true\n" ocaml_source

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

let test_sort_by_adapts_static_key_function_to_dynamic_elements () =
  let source =
    {|
(defrecord SortAttr [name])
(defn sort-attrs [^:dynamic attrs]
  (sort-by (fn [^SortAttr attr] (:name attr)) attrs))
(def sorted
  (sort-attrs [(SortAttr. :b) (SortAttr. :a)]))
(println (:name (first sorted)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "sort_by_adapts_static_key_function_to_dynamic_elements" ":a\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_sort_by_preserves_named_record_lists () =
  let source =
    {|
(defrecord SortedAttr [name])
(defrecord SortedPattern [attrs first-attr last-attr reverse-attrs wildcard?])
(def default-attr (SortedAttr. :id))
(def default-pattern
  (map->SortedPattern {:attrs (list default-attr)}))
(defn finish-pattern [^SortedPattern result]
  (let [attrs (.-attrs result)
        key-fn (fn [^SortedAttr attr] (.-name attr))
        attrs (if (.-wildcard? result)
                (conj attrs default-attr)
                attrs)
        attrs (list* (sort-by key-fn attrs))
        datom-attrs (remove (fn [^SortedAttr attr] (= :other (.-name attr))) attrs)]
    (map->SortedPattern
      {:attrs attrs
       :first-attr (first datom-attrs)
       :last-attr (last datom-attrs)
       :reverse-attrs (list* (sort-by key-fn (.-reverse-attrs result)))
       :wildcard? (.-wildcard? result)})))
(def result
  (finish-pattern
    (map->SortedPattern
      {:attrs []
       :reverse-attrs []
       :wildcard? true})))
(println (.-name ^SortedAttr (first (.-attrs result))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sort_by_preserves_named_record_lists" ":id\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_remove_adapts_static_predicate_to_dynamic_elements () =
  let source =
    {|
(defrecord RemoveAttr [name])
(defn remove-a [^:dynamic attrs]
  (remove (fn [^RemoveAttr attr] (#{:a} (:name attr))) attrs))
(println
  (count
    (remove-a [(RemoveAttr. :a) (RemoveAttr. :b)])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "remove_adapts_static_predicate_to_dynamic_elements" "1\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_some_adapts_static_predicate_to_dynamic_elements () =
  let source =
    {|
(defrecord SomeAttr [name])
(defn find-a [^:dynamic attrs]
  (some (fn [^SomeAttr attr] (#{:a} (:name attr))) attrs))
(println
  (some? (find-a [(SomeAttr. :b) (SomeAttr. :a)])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "some_adapts_static_predicate_to_dynamic_elements" "true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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
(deftype Packed [a b c ^int n])
(defn make-dynamic-prefix [e a v ^int tx]
  (Packed. e a v tx))
(println (apply make-value [1 :name "Ada"]))
(println (.-n (apply make-dynamic-prefix [1 2 3 4])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "apply_calls_overloaded_functions_with_dynamic_arguments"
    "1\n4\n" ocaml_source

let test_apply_calls_dynamic_runtime_functions () =
  let source =
    {|
(defrecord Holder [function])
(defn call-runtime [holder initial arguments]
  (let [function (:function holder)]
    (apply function initial arguments)))
(def result
  (call-runtime
    (Holder. (fn [initial left right] (+ initial left right)))
    1
    [2 3]))
(println result)
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "apply_calls_dynamic_runtime_functions" "6\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_dynamic_named_records_preserve_mutable_field_identity () =
  let source =
    {|
(defprotocol Searchable
  (search-values [value]))
(type-record mutable
  (value :ref<int>))
(deftype Store [values]
  Searchable
  (search-values [_] values))
(defn ^mutable first-mutable [store]
  (first (search-values store)))
(defn replace-static! [^mutable mutable-value]
  (reset! (:value mutable-value) 42))
(defn replace-first! [values]
  (if-some [value (first-mutable (Store. values))]
    (replace-static! value)
    0))
(def mutable-value (record mutable (value (atom 1))))
(replace-first! [mutable-value])
(println (deref (:value mutable-value)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "dynamic_named_records_preserve_mutable_field_identity"
    "42\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

let test_nil_and_sequential_guards_preserve_seqability () =
  let source =
    {|
(defn guarded-count [values]
  (if (or (nil? values) (sequential? values))
    (count values)
    -1))
(println (guarded-count nil))
(println (guarded-count [1 2]))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "nil_and_sequential_guards_preserve_seqability" "0\n2\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok);
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Js_of_ocaml source |> expect_ok)

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
(defn clear [values]
  (empty values))
(println
  (and (= [] (clear [1 2]))
       (= (list) (clear (list 1 2)))
       (= #{} (clear (hash-set 1 2)))
       (= "" (clear "Ada"))))
(defn map-preserving [f values]
  (reduce (fn [result value] (conj result (f value)))
          (empty values)
          values))
(println
  (= [1 2]
     (map-preserving (fn [tuple] (nth tuple 0)) [[1] [2]])))
(println
  (and
    (= []
       (map-preserving (fn [tuple] (nth tuple 0)) []))
    (= (list 2 1)
       (map-preserving (fn [tuple] (nth tuple 0))
                       (list (list 1) (list 2))))
    (= ["a" "b"]
       (map-preserving (fn [tuple] (nth tuple 1))
                       [[0 "a"] [1 "b"]]))))
(defn project-tuples [return-map tuples]
  (let [symbols (:symbols return-map)]
    (map-preserving
      (fn [tuple] [(nth symbols 0) (nth tuple 0)])
      tuples)))
(println
  (and
    (= [[:value 1] [:value 2]]
       (project-tuples {:symbols [:value]} [[1] [2]]))
    (= [[:value 3]]
       (project-tuples {:symbols (list :value)} (list (list 3))))
    (= []
       (project-tuples {:symbols [:value]} []))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "empty_core_api"
    "true:true:true:true\ntrue\ntrue\ntrue\ntrue\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

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

let test_take_while_transducers_compile_and_truncate_sequences () =
  let source =
    {|
(def xf (take-while (fn [value] (< value 3))))
(def values
  (into []
    (take-while (fn [value] (< value 3)))
    [1 2 3 1]))
(println (str (boolean xf) ":" (pr-str values)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "take_while_transducers_compile_and_truncate_sequences"
    "true:[1 2]\n" ocaml_source;
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

let test_assoc_accepts_nullable_static_maps () =
  let source =
    {|
(defrecord Context [existing])
(defn maybe-map [present?]
  (if present? (Context. 1) nil))
(def present (assoc (maybe-map true) :existing 42))
(def absent (assoc (maybe-map false) :existing 7))
(println (get present :existing))
(println (get absent :existing))
|}
  in
  let native_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "assoc_accepts_nullable_static_maps" "42\n7\n" native_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_nullable_dynamic_arguments_unpack_to_nominal_parameters () =
  let provider =
    {|
(ns app.provider)
(defn maybe-first [^:dynamic value]
  (when true value))
|}
  in
  let consumer =
    {|
(ns app.consumer
  (:require [app.provider :refer [maybe-first]]))
(defrecord Relation [value])
(defn relation-value [^Relation relation]
  (.-value relation))
(println (relation-value (maybe-first (Relation. 42))))
|}
  in
  let compile target =
    let state, provider_source =
      Lg.Compiler.compile_chunk ~target Lg.Compiler.empty_state provider
      |> expect_ok
    in
    let _, consumer_source =
      Lg.Compiler.compile_chunk ~target state consumer |> expect_ok
    in
    provider_source ^ "\n" ^ consumer_source
  in
  let native_source = compile Lg.Target.Native in
  assert_ocaml_runs "nullable_dynamic_arguments_unpack_to_nominal_parameters"
    "42\n" native_source;
  ignore (compile Lg.Target.Melange)

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

let test_nominal_calls_unpack_capability_storage () =
  let source =
    {|
(deftype Item [^int value])
(defn touch [^Item item] (.-value item))
(defn consume [value many?]
  (if many?
    (reduce + 0 (map touch value))
    (touch value)))
(defn consume-dynamic [^:dynamic value]
  (consume value false))
(println (consume-dynamic (Item. 12)))
|}
  in
  let native_source =
    Lg.Compiler.compile_string ~target:Lg.Target.Native source |> expect_ok
  in
  assert_ocaml_runs
    "nominal_calls_unpack_capability_storage" "12\n"
    native_source;
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

let test_sets_support_vectors_with_dynamic_elements () =
  let source =
    {|
(defrecord Holder [value])
(defn operation [holder]
  [:db.fn/retractEntity (.-value ^Holder holder)])
(def holder (Holder. 42))
(def operations (hash-set (operation holder) (operation holder)))
(println
  (str (count operations) ":"
       (contains? operations [:db.fn/retractEntity (.-value ^Holder holder)])))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sets_support_vectors_with_dynamic_elements" "1:true\n"
    ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_cons_and_conj_pack_static_values_into_dynamic_vectors () =
  let source =
    {|
(def values [1 "two"])
(println (pr-str (cons :zero values)))
(println (pr-str (conj values :three)))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "cons_and_conj_pack_static_values_into_dynamic_vectors"
    "(:zero 1 \"two\")\n[1 \"two\" :three]\n" ocaml_source;
  ignore
    (Lg.Compiler.compile_string ~target:Lg.Target.Melange source |> expect_ok)

let test_concat_packs_nested_dynamic_vectors_at_element_boundary () =
  let source =
    {|
(defrecord Holder [entries])
(defn flush-values [holder]
  (reduce-kv
    (fn [entities key value]
      (conj entities [key value]))
    []
    (.-entries ^Holder holder)))
(def combined
  (concat (flush-values (Holder. {:answer 42})) [1 "two"]))
(println (count combined))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "concat_packs_nested_dynamic_vectors_at_element_boundary"
    "3\n" ocaml_source;
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
(defn literal-values [^:bool enabled]
  (if enabled (list 42) ()))
(println
  (str (count (values true)) ":" (count (values false)) ":"
       (count (matched-values true)) ":" (count (matched-values false)) ":"
       (count (literal-values true)) ":" (count (literal-values false))))
|}
  in
  let ocaml_source = Lg.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "empty_lists_infer_type_from_branch_context" "1:0:1:0:1:0\n"
    ocaml_source;
  Lg.Compiler.compile_string {|(def values (list))|}
  |> expect_error "empty list requires a contextual element type";
  Lg.Compiler.compile_string {|(defn values [] (list))|}
  |> expect_error "empty list requires a contextual element type";
  Lg.Compiler.compile_string {|(def values ())|}
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
      ~filename:"warning.cljc" source
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
      if not (string_contains_substring diagnostic.message "warning.cljc") then
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
    Lg.Compiler.compile_string_with_filename ~filename:"broken.cljc" source
  with
  | Ok _ -> failwith "expected an unterminated vector error"
  | Error error -> (
      if error.message <> "unterminated vector; expected ']'" then
        failwith ("unexpected parser error: " ^ error.message);
      match error.location with
      | Some location ->
          if location.loc_start.Lexing.pos_fname <> "broken.cljc" then
            failwith "parser error should preserve the source filename";
          if location.loc_start.Lexing.pos_lnum <> 2 then
            failwith "parser error should point to the opening delimiter line";
          if location.loc_start.Lexing.pos_cnum <> 23 then
            failwith "parser error should point to the opening delimiter"
      | None -> failwith "parser error should include a location")

let test_language_service_recovers_completed_prefix () =
  let source = "(def answer 41)\n(def broken (+ answer" in
  match
    Lg.Language_service.recover_completed_prefix ~filename:"editing.cljc" source
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
  Lg.Language_service.analyze ~filename:"file:///tmp/service.cljc"
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
    Lg.Language_service.analyze ~filename:"file:///tmp/signature-help.cljc"
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
    Lg.Language_service.analyze ~filename:"file:///tmp/references.cljc" source
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
  Lg.Language_service.analyze ~filename:"file:///tmp/constructor-service.cljc"
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
  let provider_uri = "file:///tmp/status.cljc" in
  let consumer_uri = "file:///tmp/status-main.cljc" in
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
    Lg.Language_service.analyze ~filename:"file:///tmp/constructor-modules.cljc"
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
  Lg.Language_service.analyze ~filename:"file:///tmp/type-service.cljc"
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
  let provider_uri = "file:///tmp/user-type.cljc" in
  let consumer_uri = "file:///tmp/user-main.cljc" in
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
    Lg.Language_service.analyze ~filename:"file:///tmp/type-modules.cljc" source
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
  Lg.Language_service.analyze ~filename:"file:///tmp/module-service.cljc"
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
  let provider_uri = "file:///tmp/math-module.cljc" in
  let consumer_uri = "file:///tmp/math-main.cljc" in
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
    ~filename:"file:///tmp/module-construct-service.cljc"
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
  Lg.Language_service.analyze ~filename:"file:///tmp/protocol-service.cljc"
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
    Lg.Language_service.analyze ~filename:"file:///tmp/protocol-identities.cljc"
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
  let provider_uri = "file:///tmp/protocol-provider.cljc" in
  let consumer_uri = "file:///tmp/protocol-consumer.cljc" in
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
      ~filename:"file:///tmp/protocol-module-clash.cljc" source
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
  Lg.Language_service.analyze ~filename:"file:///tmp/field-service.cljc"
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
    Lg.Language_service.analyze ~filename:"file:///tmp/field-identities.cljc"
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
  let provider_uri = "file:///tmp/field-provider.cljc" in
  let consumer_uri = "file:///tmp/field-consumer.cljc" in
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
    Lg.Language_service.analyze ~filename:"file:///tmp/private-defn.cljc" source
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
      ~filename:"file:///tmp/document-symbol-hierarchy.cljc"
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
    Lg.Language_service.analyze ~filename:"file:///tmp/semantic-tokens.cljc"
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
      [ ("file:///tmp/main.cljc", main); ("file:///tmp/math.cljc", math) ]
    |> expect_ok
  in
  let main_analysis = List.assoc "file:///tmp/main.cljc" analyses in
  let usage = expect_substring_index main "Math/magnitude-plus-two 40" in
  if Lg.Language_service.semantic_uid_at main_analysis ~offset:usage = None then
    failwith "expected required workspace symbol to have a typed identity";
  match Lg.Language_service.definition main_analysis ~offset:usage with
  | Some location
    when location.Location.loc_start.Lexing.pos_fname = "file:///tmp/math.cljc"
    ->
      ()
  | _ -> failwith "expected required workspace symbol definition in math.cljc"

let test_workspace_index_reanalyzes_only_dependency_component () =
  let math_uri = "file:///tmp/math.cljc" in
  let main_uri = "file:///tmp/main.cljc" in
  let other_uri = "file:///tmp/other.cljc" in
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
  let values_uri = "file:///tmp/values.cljc" in
  let consumer_uri = "file:///tmp/consumer.cljc" in
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
        ("file:///tmp/workspace-math.cljc", "(module Math (def value 42))\n");
        ("file:///tmp/workspace-alias.cljc", "(module-alias M Math)\n");
        ("file:///tmp/workspace-alias-user.cljc", "(def result M/value)\n");
      ]
    |> expect_ok
  in
  if
    Lg.Language_service.workspace_analysis analyses
      "file:///tmp/workspace-alias-user.cljc"
    = None
  then failwith "workspace index must connect module alias consumers"

let test_workspace_index_tracks_variant_constructor_dependencies () =
  let analyses =
    Lg.Language_service.create_workspace_index
      [
        ( "file:///tmp/workspace-status.cljc",
          "(type-variant status Active (Named :string))\n" );
        ( "file:///tmp/workspace-status-user.cljc",
          "(def current (Named \"Ada\"))\n" );
      ]
    |> expect_ok
  in
  if
    Lg.Language_service.workspace_analysis analyses
      "file:///tmp/workspace-status-user.cljc"
    = None
  then failwith "workspace index must connect variant constructor consumers"

let test_workspace_index_ignores_lexically_bound_names () =
  let provider_uri = "file:///tmp/workspace-global-value.cljc" in
  let local_uri = "file:///tmp/workspace-local-value.cljc" in
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
  let provider_uri = "file:///tmp/workspace-domain-type.cljc" in
  let consumer_uri = "file:///tmp/workspace-domain-type-user.cljc" in
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
  let provider_uri = "file:///tmp/a-workspace-domain-concise.cljc" in
  let consumer_uri = "file:///tmp/z-workspace-domain-concise-user.cljc" in
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
  let provider_uri = "file:///tmp/a-workspace-domain-declaration.cljc" in
  let consumer_uri = "file:///tmp/z-workspace-domain-declaration-user.cljc" in
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
  let module_uri = "file:///tmp/workspace-shared-module.cljc" in
  let protocol_uri = "file:///tmp/workspace-shared-protocol.cljc" in
  let consumer_uri = "file:///tmp/workspace-shared-user.cljc" in
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
  let provider_uri = "file:///tmp/lifecycle-math.cljc" in
  let renamed_uri = "file:///tmp/lifecycle-renamed-math.cljc" in
  let consumer_uri = "file:///tmp/lifecycle-main.cljc" in
  let other_uri = "file:///tmp/lifecycle-other.cljc" in
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
        ("file:///tmp/provider-one.cljc", "(def shared-value 1)\n");
        ("file:///tmp/provider-two.cljc", "(def shared-value 2)\n");
        ("file:///tmp/provider-user.cljc", "(def result shared-value)\n");
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
  let math_uri = "file:///tmp/error-math.cljc" in
  let main_uri = "file:///tmp/error-main.cljc" in
  let other_uri = "file:///tmp/error-other.cljc" in
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
  let math_uri = "file:///tmp/partial-math.cljc" in
  let main_uri = "file:///tmp/partial-main.cljc" in
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
  let status_uri = "file:///tmp/diagnostic-status.cljc" in
  let main_uri = "file:///tmp/diagnostic-main.cljc" in
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

let test_incremental_compile_chunk_typechecks_each_chunk_once () =
  let warning_source =
    {|
(type-variant status Active Inactive)
(defn describe [^:status status]
  (match status
    Active "active"))
|}
  in
  let state, first =
    Lg.Compiler.compile_chunk_with_filename_and_diagnostics
      ~filename:"first_chunk.cljc" Lg.Compiler.empty_state warning_source
    |> expect_ok
  in
  if List.length first.diagnostics <> 1 then
    failwith "the first chunk must report its non-exhaustive match warning";
  let _state, second =
    Lg.Compiler.compile_chunk_with_filename_and_diagnostics
      ~filename:"second_chunk.cljc" state "(def answer 42)"
    |> expect_ok
  in
  if second.diagnostics <> [] then
    failwith
      "an incremental compile must not typecheck and report earlier chunks again"

let test_incremental_compile_chunk_checks_new_code_against_prior_ocaml_env () =
  let state, _ =
    Lg.Compiler.compile_chunk Lg.Compiler.empty_state
      {|
(type-record user (name :string))
(def user (record user (name "Ada")))
|}
    |> expect_ok
  in
  Lg.Compiler.compile_chunk state
    {|
(def invalid-age (Stdlib.abs (:name user)))
|}
  |> expect_error_contains "string"

let test_portable_compiler_state_rebuilds_ocaml_environment () =
  let state, (first : Lg.Compiler.compilation) =
    Lg.Compiler.compile_chunk_with_filename_and_diagnostics
      ~filename:"first_chunk.cljc" Lg.Compiler.empty_state
      {|
(type-record user (name :string))
(def user (record user (name "Ada")))
|}
    |> expect_ok
  in
  let state = Lg.Compiler.cacheable_state state in
  let encoded = Marshal.to_string state [] in
  let restored : Lg.Compiler.state = Marshal.from_string encoded 0 in
  let restored =
    Lg.Compiler.restore_ocaml_environment ~packages:[] restored
      [ first.ocaml_source ]
    |> expect_ok
  in
  ignore
    (Lg.Compiler.compile_chunk restored {|(println (:name user))|}
    |> expect_ok)

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
  | [ type_item; set_module_item; value_item ] -> (
      match
        (type_item.pstr_desc, set_module_item.pstr_desc, value_item.pstr_desc)
      with
      | Pstr_type _, Pstr_module _, Pstr_value _ -> ()
      | _ ->
          failwith
            "expected row type, comparator module, and function value structure items"
      )
  | _ ->
      failwith
        "expected row type, comparator module, and function value structure items"

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
    ( "compiler test directory avoids existing PID directory",
      test_test_directory_avoids_existing_pid_directory );
    ( "records, assoc, and dissoc generate typed OCaml",
      test_records_assoc_and_dissoc );
    ( "assoc rejects changing an existing field type",
      test_assoc_rejects_type_changes );
    ("dissoc missing fields is a no-op", test_dissoc_missing_fields_is_noop);
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
    ( "when-some binding constraints reach dynamic calls",
      test_when_some_binding_constraints_reach_dynamic_calls );
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
    ( "qualified type predicates materialize dynamic parameters",
      test_qualified_type_predicates_materialize_dynamic_parameters );
    ( "doseq map entry destructuring preserves map values",
      test_doseq_map_entry_destructuring_preserves_map_values );
    ( "compare uses dynamic seqable storage",
      test_compare_uses_dynamic_seqable_storage );
    ( "instance? supports Clojure collection interfaces",
      test_instance_predicate_supports_clojure_collection_interfaces );
    ( "condp selects first match and evaluates target once",
      test_condp_selects_first_match_and_evaluates_target_once );
    ("subs core api works", test_subs_core_api);
    ("subs rejects non-string sources", test_subs_rejects_non_string_sources);
    ("subs rejects non-int indexes", test_subs_rejects_non_int_indexes);
    ( "type relations are explicit and strict",
      test_type_relations_are_explicit_and_strict );
    ( "type solver preserves shared and independent variables",
      test_type_solver_preserves_shared_and_independent_variables );
    ( "empty type substitutions preserve type identity",
      test_empty_type_substitutions_preserve_type_identity );
    ( "unrelated type substitutions preserve type identity",
      test_unrelated_type_substitutions_preserve_type_identity );
    ( "type solver applies deep substitutions linearly",
      test_type_solver_applies_deep_substitutions_linearly );
    ( "type solver preserves shared substitution DAGs",
      test_type_solver_preserves_shared_substitution_dags );
    ( "generic record calls freshen callee type variables",
      test_generic_record_calls_freshen_callee_type_variables );
    ( "dynamic sequences adapt to nullable callback parameters",
      test_dynamic_sequences_adapt_to_nullable_callback_parameters );
    ( "keyword lookup constrains first of protocol sequences",
      test_keyword_lookup_constrains_first_of_protocol_sequences );
    ( "protocol result context does not constrain arguments",
      test_protocol_result_context_does_not_constrain_arguments );
    ( "frontend location index avoids quadratic scans",
      test_frontend_location_index_avoids_quadratic_scans );
    ( "refresh named record realigns forward declared records",
      test_refresh_named_record_realigns_forward_declared_records );
    ( "freshen deferred dynamic dispatch stays monomorphic",
      test_freshen_deferred_dynamic_dispatch_stays_monomorphic );
    ( "freshen deferred erased seqable values stay dynamic",
      test_freshen_deferred_erased_seqable_values_stay_dynamic );
    ( "unresolved type scan stops at nominal records",
      test_unresolved_type_scan_stops_at_nominal_records );
    ( "deferred forward calls keep nominal receiver evidence",
      test_deferred_forward_calls_keep_nominal_receiver_evidence );
    ( "deferred named record fields receive body constraints",
      test_deferred_named_record_fields_receive_body_constraints );
    ( "typed IR preserves explicit boundary operations",
      test_typed_ir_preserves_explicit_boundary_operations );
    ( "core form expansions use hygienic identifiers",
      test_core_form_expansions_use_hygienic_identifiers );
    ( "dynamic record capabilities resolve unique named records",
      test_dynamic_record_capabilities_resolve_unique_named_records );
    ( "dynamic record lookup specializes shared generic fields",
      test_dynamic_record_lookup_specializes_shared_generic_fields );
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
    ( "protocol satisfaction uses stabilized evidence",
      test_protocol_satisfaction_uses_stabilized_evidence );
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
    ( "Clojure and CLJS core namespace aliases dispatch to core",
      test_clojure_and_cljs_core_namespace_aliases_dispatch_to_core );
    ( "keyword lookup ignores refer-clojure get exclusion",
      test_keyword_lookup_ignores_refer_clojure_get_exclusion );
    ( "namespace refer-clojure exclude allows local replacement",
      test_namespace_refer_clojure_exclude_allows_local_replacement );
    ( "namespace refer-clojure exclude hides core binding",
      test_namespace_refer_clojure_exclude_hides_core_binding );
    ( "namespace rejects import clauses",
      test_namespace_rejects_import_clause );
    ( "namespace ignores reader conditional import clauses",
      test_namespace_ignores_reader_conditional_import_clause );
    ( "reader conditional import refers LG record types",
      test_reader_conditional_import_refers_lg_record_types );
    ( "dynamic vars bind and restore portably",
      test_dynamic_vars_bind_and_restore_portably );
    ("named fn is locally recursive", test_named_fn_is_locally_recursive);
    ( "if joins static and dynamic function parameters",
      test_if_joins_static_and_dynamic_function_parameters );
    ( "JVM lookup hints do not narrow dynamic values",
      test_jvm_lookup_hints_do_not_narrow_dynamic_values );
    ( "Object marker builds dynamic arrays without Java",
      test_object_marker_builds_dynamic_arrays_without_java );
    ( "LazilyPersistentVector createOwning is portable",
      test_lazily_persistent_vector_create_owning_is_portable );
    ( "clojure.edn read-string behaves on Native and Melange",
      test_clojure_edn_read_string_behaves_on_native_and_melange );
    ( "clojure.edn read-string rejects invalid collections",
      test_clojure_edn_read_string_rejects_invalid_collections );
    ( "referred update supports threaded nested calls",
      test_referred_update_supports_threaded_nested_calls );
    ( "clj reader conditional macros survive deferred Melange bodies",
      test_clj_reader_conditional_macros_survive_deferred_melange_bodies );
    ( "reader conditional accepts metadata branch values",
      test_reader_conditional_accepts_metadata_branch_values );
    ( "metadata map prefixes compile without Java types",
      test_metadata_map_prefixes_compile_without_java_types );
    ( "Java isArray idiom lowers to portable array predicate",
      test_java_is_array_idiom_lowers_to_portable_array_predicate );
    ( "dotimes evaluates bounds once and returns nil",
      test_dotimes_evaluates_bounds_once_and_returns_nil );
    ( "aget supports dynamic arrays with inferred indexes",
      test_aget_supports_dynamic_arrays_with_inferred_indexes );
    ( "current DataScript chain compiles for Native and Melange",
      test_current_datascript_chain_compiles_for_native_and_melange );
    ( "DataScript make-array one arity behaves on Native and Melange",
      test_datascript_make_array_one_arity_behaves_on_native_and_melange );
    ( "current DataScript Entity behaves on Native",
      test_current_datascript_entity_behaves_on_native );
    ( "current DataScript pull parser compiles for Native and Melange",
      test_current_datascript_pull_parser_compiles_for_native_and_melange );
    ( "current DataScript pull API compiles for Native and Melange",
      test_current_datascript_pull_api_compiles_for_native_and_melange );
    ( "current DataScript pull API behaves on Native",
      test_current_datascript_pull_api_behaves_on_native );
    ( "current DataScript query compiles for Native and Melange",
      test_current_datascript_query_compiles_for_native_and_melange );
    ( "current DataScript query behaves on Native",
      test_current_datascript_query_behaves_on_native );
    ( "current DataScript serialize compiles for Native and Melange",
      test_current_datascript_serialize_compiles_for_native_and_melange );
    ( "current DataScript serialize roundtrips on Native",
      test_current_datascript_serialize_roundtrips_on_native );
    ( "namespace ignores Clojure compiler directives",
      test_namespace_ignores_clojure_compiler_directives );
    ( "System currentTimeMillis compiles for native",
      test_system_current_time_millis_compiles_for_native );
    ( "JavaScript targets compile Date and radix interop",
      test_javascript_targets_compile_date_and_radix_interop );
    ( "JavaScript targets compile error classes",
      test_javascript_targets_compile_error_classes );
    ( "CLJS writer functions compile", test_cljs_writer_functions_compile );
    ( "transient collection operations preserve values",
      test_transient_collection_operations_preserve_values );
    ( "get supports static and dynamic transient maps",
      test_get_supports_static_and_dynamic_transient_maps );
    ( "named reducers receive contextual accumulator types",
      test_named_reducers_receive_contextual_accumulator_types );
    ( "count supports transient collections",
      test_count_supports_transient_collections );
    ( "nth supports active transient vectors",
      test_nth_supports_active_transient_vectors );
    ( "volatile transient maps specialize from vswap",
      test_volatile_transient_maps_specialize_from_vswap );
    ( "nth narrows dynamic indexes at the boundary",
      test_nth_narrows_dynamic_indexes_at_the_boundary );
    ( "var quote resolves static function values",
      test_var_quote_resolves_static_function_values );
    ( "persistent transient map is seqable",
      test_persistent_transient_map_is_seqable );
    ( "dynamic transient vector accepts static values",
      test_dynamic_transient_vector_accepts_static_values );
    ( "dynamic transient map accepts static entries",
      test_dynamic_transient_map_accepts_static_entries );
    ( "defrecord preserves transient vector shape",
      test_defrecord_preserves_transient_vector_shape );
    ( "defrecord preserves transient map shape",
      test_defrecord_preserves_transient_map_shape );
    ( "transient operations are first-class functions",
      test_transient_operations_are_first_class_functions );
    ("assert accepts optional message", test_assert_accepts_optional_message);
    ( "into cat flattens one collection level",
      test_into_cat_flattens_one_collection_level );
    ( "mapv vector zips multiple collections",
      test_mapv_vector_zips_multiple_collections );
    ( "map and mapv accept multiple collections",
      test_map_and_mapv_accept_multiple_collections );
    ( "map vector preserves heterogeneous vectors",
      test_map_vector_preserves_heterogeneous_vectors );
    ( "forward-declared functions work as collection callbacks",
      test_forward_declared_functions_work_as_collection_callbacks );
    ( "conditional records pack opaque fields at dynamic boundary",
      test_conditional_records_pack_opaque_fields_at_dynamic_boundary );
    ( "dynamic nominal records preserve nested nominal fields",
      test_dynamic_nominal_records_preserve_nested_nominal_fields );
    ( "dynamic vector literals pack anonymous record elements directly",
      test_dynamic_vector_literal_packs_anonymous_record_elements_directly );
    ( "equality packs vectors with nested dynamic elements",
      test_equality_packs_vectors_with_nested_dynamic_elements );
    ( "destructured row parameters stay structural",
      test_destructured_row_parameter_stays_structural );
    ( "forward-declared multi-arity functions initialize lazily",
      test_forward_declared_multi_arity_functions_initialize_lazily );
    ( "forward-declared mutual recursion reuses stabilized signatures",
      test_forward_declared_mutual_recursion_reuses_stabilized_signatures );
    ( "fnil wraps core conj with default collection",
      test_fnil_wraps_core_conj_with_default_collection );
    ( "dynamic protocol witnesses unpack common returns",
      test_dynamic_protocol_witnesses_unpack_common_returns );
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
    ( "to-array is first-class for dynamic seqable values",
      test_to_array_is_first_class_for_dynamic_seqable_values );
    ( "array arguments adapt nullable elements",
      test_array_arguments_adapt_nullable_elements );
    ( "optional protocol values can flow to seqable else branches",
      test_optional_protocol_values_can_flow_to_seqable_else_branches );
    ( "nested protocol witnesses keep concrete receiver storage",
      test_nested_protocol_witnesses_keep_concrete_receiver_storage );
    ( "OCaml refs support read and assignment",
      test_ocaml_refs_support_read_and_assignment );
    ("concise standard type annotations", test_concise_standard_type_annotations);
    ( "weak references support typed cache values",
      test_weak_references_support_typed_cache_values );
    ( "weak references reject invalid calls",
      test_weak_references_reject_invalid_calls );
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
    ( "special float literals are portable",
      test_special_float_literals_are_portable );
    ( "numeric equality accepts dynamic ints and floats",
      test_numeric_equality_accepts_dynamic_ints_and_floats );
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
    ( "multi-arity defn accepts nil for destructured options",
      test_multi_arity_defn_accepts_nil_for_destructured_options );
    ( "nullable destructured options flow through forwarding functions",
      test_nullable_destructured_options_flow_through_forwarding_functions );
    ( "multi-arity defn remains callable as a value",
      test_multi_arity_defn_remains_callable_as_a_value );
    ( "multi-arity calls project structural row arguments",
      test_multi_arity_calls_project_structural_row_arguments );
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
    ( "satisfies? selects each generic protocol witness",
      test_satisfies_question_selects_each_generic_protocol_witness );
    ( "protocol methods use their static receiver witnesses",
      test_protocol_methods_use_their_static_receiver_witnesses );
    ( "extend-type methods use their static receiver witnesses",
      test_extend_type_methods_use_their_static_receiver_witnesses );
    ( "satisfies? guards generic protocol dispatch",
      test_satisfies_question_guards_generic_protocol_dispatch );
    ( "generic protocol witness supports multiple methods",
      test_generic_protocol_witness_supports_multiple_methods );
    ( "generic protocol witness evaluates receiver once",
      test_generic_protocol_witness_evaluates_receiver_once );
    ( "generic protocol witness packs seqable arguments",
      test_generic_protocol_witness_packs_seqable_arguments );
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
    ( "loop bindings accept prefix type hints",
      test_loop_bindings_accept_prefix_type_hints );
    ( "loop nil initial value accepts nullable function returns",
      test_loop_nil_initial_value_accepts_nullable_function_returns );
    ( "forward declared deftype fields keep nominal receiver",
      test_forward_declared_deftype_fields_keep_nominal_receiver );
    ( "forward declared functions refresh nominal returns",
      test_forward_declared_functions_refresh_nominal_returns );
    ( "quoted symbols do not create recursive dependencies",
      test_quoted_symbols_do_not_create_recursive_dependencies );
    ( "forward declaration detection includes overload targets",
      test_forward_declaration_detection_includes_overload_targets );
    ( "incremental declarations refresh protocol method returns",
      test_incremental_declarations_refresh_protocol_method_returns );
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
    ( "defrecord inferred generic fields preserve value types",
      test_defrecord_inferred_generic_fields_preserve_value_types );
    ( "defrecord methods infer every structural generic field",
      test_defrecord_methods_infer_every_structural_generic_field );
    ( "defrecord fields preserve protocol capabilities",
      test_defrecord_fields_preserve_protocol_capabilities );
    ( "defrecord protocol methods support forward calls",
      test_defrecord_protocol_methods_support_forward_calls );
    ( "equality dispatches to record IEquiv",
      test_equality_dispatches_to_record_iequiv );
    ( "record IEquiv refreshes forward protocol dependencies",
      test_record_iequiv_refreshes_forward_protocol_dependencies );
    ( "defrecord host methods support declared helpers",
      test_defrecord_host_methods_support_declared_helpers );
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
    ( "apply pr accepts lazy sequences",
      test_apply_pr_accepts_lazy_sequences );
    ( "apply pr accepts refined protocol sequences",
      test_apply_pr_accepts_refined_protocol_sequences );
    ( "protocol methods merge concrete and dynamic sequence returns",
      test_protocol_methods_merge_concrete_and_dynamic_sequence_returns );
    ( "recursive protocol sequence returns remain concrete",
      test_recursive_protocol_sequence_returns_remain_concrete );
    ( "recursive protocol vectors keep static protocol elements",
      test_recursive_protocol_vectors_keep_static_protocol_elements );
    ( "recursive protocol frame stacks preserve dispatch witnesses",
      test_recursive_protocol_frame_stacks_preserve_dispatch_witnesses );
    ( "dynamic values preserve partial protocol implementations",
      test_dynamic_values_preserve_partial_protocol_implementations );
    ( "dynamic protocol results preserve dispatch witnesses",
      test_dynamic_protocol_results_preserve_dispatch_witnesses );
    ( "dynamic record assoc preserves updated nominal value",
      test_dynamic_record_assoc_preserves_updated_nominal_value );
    ( "dynamic boundaries preserve next nil semantics",
      test_dynamic_boundaries_preserve_next_nil_semantics );
    ( "typed maps compare dynamic vector keys structurally",
      test_typed_maps_compare_dynamic_vector_keys_structurally );
    ( "direct valAt uses dynamic map comparator",
      test_direct_val_at_uses_dynamic_map_comparator );
    ( "recursive protocol vectors materialize optional unknown elements",
      test_recursive_protocol_vectors_materialize_optional_unknown_elements );
    ( "loop protocol vectors widen heterogeneous elements locally",
      test_loop_protocol_vectors_widen_heterogeneous_elements_locally );
    ( "cond protocol vectors widen heterogeneous elements locally",
      test_cond_protocol_vectors_widen_heterogeneous_elements_locally );
    ( "if nullable protocol vectors widen elements locally",
      test_if_nullable_protocol_vectors_widen_elements_locally );
    ( "cond nullable protocol vectors widen elements locally",
      test_cond_nullable_protocol_vectors_widen_elements_locally );
    ( "if-some packs optional record elements into dynamic vectors",
      test_if_some_packs_optional_record_elements_into_dynamic_vectors );
    ( "conditional vectors store seqable capabilities",
      test_conditional_vectors_store_seqable_capabilities );
    ( "nullable sequence branches do not gain nested options",
      test_nullable_sequence_branches_do_not_gain_nested_options );
    ( "nominal sequence branches lift into nullable results",
      test_nominal_sequence_branches_lift_into_nullable_results );
    ( "cross module nominal sequences merge with dynamic protocol results",
      test_cross_module_nominal_sequences_merge_with_dynamic_protocol_results );
    ( "extend protocol keeps parameter positions independent",
      test_extend_protocol_keeps_parameter_positions_independent );
    ( "cond thread preserves guarded seqable aliases",
      test_cond_thread_preserves_guarded_seqable_aliases );
    ( "vec is available as a first class function",
      test_vec_is_available_as_a_first_class_function );
    ( "condp preserves function recur tail positions",
      test_condp_preserves_function_recur_tail_positions );
    ( "branch-local record hints materialize protocol parameters",
      test_branch_local_record_hints_materialize_protocol_parameters );
    ( "protocol witness results unpack concrete sequence returns",
      test_protocol_witness_results_unpack_concrete_sequence_returns );
    ( "defn accepts attribute maps and return hints",
      test_defn_accepts_attribute_maps_and_return_hints );
    ( "inline attributes expand same namespace calls",
      test_inline_attribute_expands_same_namespace_calls );
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
    ( "if merges generic array function branches",
      test_if_merges_generic_array_function_branches );
    ( "if-some preserves nominal array elements",
      test_if_some_preserves_nominal_array_elements );
    ( "Melange array .map uses static array map",
      test_melange_array_dot_map_uses_static_array_map );
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
    ( "contains? static sets handles dynamic candidates",
      test_contains_static_sets_handles_dynamic_candidates );
    ( "generic protocol witness compiles for JavaScript targets",
      test_generic_protocol_witness_compiles_for_javascript_targets );
    ( "built-in IComparable supports dynamic dispatch",
      test_builtin_icomparable_supports_dynamic_dispatch );
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
    ( "keys support generic and dynamic maps",
      test_keys_support_generic_and_dynamic_maps );
    ("vals return homogeneous values", test_vals_return_homogeneous_values);
    ("vals accept dynamic maps", test_vals_accept_dynamic_maps);
    ( "vals support generic dynamic and empty maps",
      test_vals_support_generic_dynamic_and_empty_maps );
    ( "zipmap stops at shortest and preserves dynamic boundaries",
      test_zipmap_stops_at_shortest_and_preserves_dynamic_boundaries );
    ("map accepts callable map values", test_map_accepts_callable_map_values);
    ( "ffirst is first class and empty safe",
      test_ffirst_is_first_class_and_empty_safe );
    ( "group-by infers generic seqable collections",
      test_group_by_infers_generic_seqable_collections );
    ( "group-by unpacks generic seqable items",
      test_group_by_unpacks_generic_seqable_items );
    ( "filterv contextualizes generic seqable items",
      test_filterv_contextualizes_generic_seqable_items );
    ( "vec realizes for over dynamic map entries",
      test_vec_realizes_for_over_dynamic_map_entries );
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
    ("get returns nil for unknown map fields",
      test_get_returns_nil_for_unknown_map_fields );
    ("get supports default values", test_get_supports_default_values);
    ( "get rejects default type mismatch for known fields",
      test_get_rejects_default_type_mismatch_for_known_fields );
    ("get supports vectors", test_get_supports_vectors);
    ( "get dispatches nullable deftype lookup with dynamic keys",
      test_get_dispatches_nullable_deftype_lookup_with_dynamic_keys );
    ( "get rejects vector default type mismatch",
      test_get_rejects_vector_default_type_mismatch );
    ("assoc supports multiple pairs", test_assoc_supports_multiple_pairs);
    ("assoc rejects odd key value pairs", test_assoc_rejects_odd_key_value_pairs);
    ("assoc supports vector indexes", test_assoc_supports_vector_indexes);
    ( "clojure RT assoc matches core assoc",
      test_clojure_rt_assoc_matches_core_assoc );
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
    ( "protocol calls pack dynamic non-receiver arguments",
      test_protocol_calls_pack_dynamic_non_receiver_arguments );
    ( "defrecord field hints reject unknown record types",
      test_defrecord_field_hints_reject_unknown_record_types );
    ( "defrecord preserves extension map entries",
      test_defrecord_preserves_extension_map_entries );
    ( "named record calls accept structural extension fields",
      test_named_record_calls_accept_structural_extension_fields );
    ( "update preserves named records with opaque fields",
      test_update_preserves_named_records_with_opaque_fields );
    ("assoc-in updates nested maps", test_assoc_in_updates_nested_maps);
    ( "assoc-in preserves named records with references",
      test_assoc_in_preserves_named_records_with_references );
    ( "assoc accepts refined dynamic record fields",
      test_assoc_accepts_refined_dynamic_record_fields );
    ( "threaded forms accumulate record fields",
      test_threaded_forms_accumulate_record_fields );
    ( "update missing structural field passes nil to updater",
      test_update_missing_structural_field_passes_nil_to_updater );
    ( "inline update infers threaded record fields",
      test_inline_update_infers_threaded_record_fields );
    ( "inline update refines protocol collection elements",
      test_inline_update_refines_protocol_collection_elements );
    ( "inline update infers transient collection boundaries",
      test_inline_update_infers_transient_collection_boundaries );
    ( "nested update infers optional map value collections",
      test_nested_update_infers_optional_map_value_collections );
    ( "if-some get keeps map storage non-nullable",
      test_if_some_get_keeps_map_storage_non_nullable );
    ( "nullable record constraints merge across branches",
      test_nullable_record_constraints_merge_across_branches );
    ( "named record inference keeps distinct host wrappers",
      test_named_record_inference_keeps_distinct_host_wrappers );
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
    ("cond literal true is exhaustive", test_cond_literal_true_is_exhaustive);
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
    ( "clojure.string join accepts lazy sequences",
      test_clojure_string_join_accepts_lazy_sequences );
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
    ( "partition-by keyword infers seqable record parameters",
      test_partition_by_keyword_infers_seqable_record_parameters );
    ( "batched predicate/collection core functions reject bad counts",
      test_batched_predicate_collection_core_functions_reject_bad_counts );
    ( "batched predicate/collection core functions reject bad predicates",
      test_batched_predicate_collection_core_functions_reject_bad_predicates );
    ( "batched predicate/collection core functions reject bad run function",
      test_batched_predicate_collection_core_functions_reject_bad_run_function
    );
    ("doseq infers seqable parameters", test_doseq_infers_seqable_parameters);
    ( "doseq prefers reducible over seqable",
      test_doseq_prefers_reducible_over_seqable );
    ("for supports when clauses", test_for_supports_when_clauses);
    ( "merge accepts dynamic map parameters",
      test_merge_accepts_dynamic_map_parameters );
    ( "record arguments fill missing optional fields",
      test_record_arguments_fill_missing_optional_fields );
    ( "reify preserves protocols across dynamic fields",
      test_reify_preserves_protocols_across_dynamic_fields );
    ( "parameters preserve multiple protocol constraints",
      test_parameters_preserve_multiple_protocol_constraints );
    ( "forwarded parameters deduplicate protocol constraints",
      test_forwarded_parameters_deduplicate_protocol_constraints );
    ( "references preserve state across dynamic fields",
      test_references_preserve_state_across_dynamic_fields );
    ( "truthy guards preserve dynamic numeric parameters",
      test_truthy_guards_preserve_dynamic_numeric_parameters );
    ( "and truthy guard narrows nullable ints",
      test_and_truthy_guard_narrows_nullable_ints );
    ( "or nil guard narrows nullable records",
      test_or_nil_guard_narrows_nullable_records );
    ( "some guard narrows nullable records",
      test_some_guard_narrows_nullable_records );
    ( "or nil guard narrows hinted dynamic sequence elements",
      test_or_nil_guard_narrows_hinted_dynamic_sequence_elements );
    ( "loop parameters widen for nullable generic recur values",
      test_loop_parameters_widen_for_nullable_generic_recur_values );
    ( "loop recur unpacks dynamic protocol results to static records",
      test_loop_recur_unpacks_dynamic_protocol_results_to_static_records );
    ( "loop recur analysis respects nested let shadowing",
      test_loop_recur_analysis_respects_nested_let_shadowing );
    ( "generic loop sequences remain specializable at static call sites",
      test_generic_loop_sequences_remain_specializable_at_static_call_sites );
    ( "str keeps heterogeneous record fields dynamic",
      test_str_keeps_heterogeneous_record_fields_dynamic );
    ( "callable set parameters remain sets for conj",
      test_callable_set_parameters_remain_sets_for_conj );
    ( "computed sets are first-class predicates",
      test_computed_sets_are_first_class_predicates );
    ( "generic clojure.set subset constrains parameters",
      test_generic_clojure_set_subset_constrains_parameters );
    ( "resolve returns nil without runtime Var reflection",
      test_resolve_returns_nil_without_runtime_var_reflection );
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
    ( "thread-last inferred functions pass collections to take-while",
      test_thread_last_inferred_functions_pass_collections_to_take_while );
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
    ( "two-arity reduce uses first or zero-arity identity",
      test_two_arity_reduce_uses_first_or_zero_arity_identity );
    ( "reduce refines empty set accumulators without widening static sets",
      test_reduce_refines_empty_set_accumulators_without_widening_static_sets );
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
    ( "concat lifts values into nullable element types",
      test_concat_lifts_values_into_nullable_element_types );
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
    ( "interleave accepts inferred seqable parameters",
      test_interleave_accepts_inferred_seqable_parameters );
    ( "interleave rejects later type mismatches",
      test_interleave_rejects_later_type_mismatches );
    ( "interleave requires two collections",
      test_interleave_requires_two_collections );
    ("additional sequence helpers work", test_additional_sequence_helpers_work);
    ( "last returns nil for empty collections",
      test_last_returns_nil_for_empty_collections );
    ( "first returns nil for empty collections",
      test_first_returns_nil_for_empty_collections );
    ( "generic first can seed a dynamic reduce",
      test_generic_first_can_seed_a_dynamic_reduce );
    ( "optional record dynamic fields use runtime nil",
      test_optional_record_dynamic_fields_use_runtime_nil );
    ( "rseq dispatches to reversible protocol",
      test_rseq_dispatches_to_reversible_protocol );
    ( "deftype protocol methods support multiple arities",
      test_deftype_protocol_methods_support_multiple_arities );
    ( "macros preserve nested parameter type hints",
      test_macros_preserve_nested_parameter_type_hints );
    ( "protocol calls recover structurally inferred named records",
      test_protocol_calls_recover_structurally_inferred_named_records );
    ( "transducer type hints infer nominal record fields",
      test_transducer_type_hints_infer_nominal_record_fields );
    ( "deftype methods flush after their declared dependencies",
      test_deftype_methods_flush_after_their_declared_dependencies );
    ( "protocol consumers use stable later implementation returns",
      test_protocol_consumers_use_stable_later_implementation_returns );
    ( "deferred initializers run before first ready use",
      test_deferred_initializers_run_before_first_ready_use );
    ( "declared record constructors follow record dependencies",
      test_declared_record_constructors_follow_record_dependencies );
    ( "dependency graph orders declared protocol dependencies",
      test_dependency_graph_orders_declared_protocol_dependencies );
    ( "dependency graph orders non-dash protocol methods before consumers",
      test_dependency_graph_orders_non_dash_protocol_methods_before_consumers );
    ( "dependency graph keeps declarations before macro consumers",
      test_dependency_graph_keeps_declarations_before_macro_consumers );
    ( "dependency graph loads requires before runtime macro consumers",
      test_dependency_graph_loads_requires_before_runtime_macro_consumers );
    ( "stabilization ast skips mutual function bodies",
      test_stabilization_ast_skips_mutual_function_bodies );
    ( "declarations do not merge independent functions",
      test_declarations_do_not_merge_independent_functions );
    ( "typecheck stabilizes forward declaration ABI",
      test_typecheck_stabilizes_forward_declaration_abi );
    ( "typecheck validates full compile after evidence stabilizes",
      test_typecheck_validates_full_compile_after_evidence_stabilizes );
    ( "nested simple let inference visits body linearly",
      test_nested_simple_let_inference_visits_body_linearly );
    ( "global function alias keeps contextual inference",
      test_global_function_alias_keeps_contextual_inference );
    ( "destructured let keeps provisional body inference",
      test_destructured_let_keeps_provisional_body_inference );
    ( "recursive collection result specializes self calls",
      test_recursive_collection_result_specializes_self_calls );
    ( "grouped records preserve constructor type",
      test_grouped_records_preserve_constructor_type );
    ( "typecheck skips replay without new stabilization evidence",
      test_typecheck_skips_replay_without_new_stabilization_evidence );
    ( "recursive declared nullable sequence supports not-empty",
      test_recursive_declared_nullable_sequence_supports_not_empty );
    ( "nested keyword lookup preserves nullable map evidence",
      test_nested_keyword_lookup_preserves_nullable_map_evidence );
    ( "symbol predicate narrows dynamic value in then branch",
      test_symbol_predicate_narrows_dynamic_value_in_then_branch );
    ( "protocol record reconstruction keeps field type open",
      test_protocol_record_reconstruction_keeps_field_type_open );
    ( "parser alternatives preserve open argument type",
      test_parser_alternatives_preserve_open_argument_type );
    ( "parser rule map allocates anonymous return record",
      test_parser_rule_map_allocates_anonymous_return_record );
    ( "rule vars projection preserves nominal argument type",
      test_rule_vars_projection_preserves_nominal_argument_type );
    ( "for let shadowing replaces nominal collection type",
      test_for_let_shadowing_replaces_nominal_collection_type );
    ( "symbol predicate narrows later and operands",
      test_symbol_predicate_narrows_later_and_operands );
    ( "nested sequential branch destructuring preserves dynamic values",
      test_nested_sequential_branch_destructuring_preserves_dynamic_values );
    ( "if-let callback accepts optional and required results",
      test_if_let_callback_accepts_optional_and_required_results );
    ( "external protocol implementation prevents field misspecialization",
      test_external_protocol_implementation_prevents_field_misspecialization );
    ( "logical or with throw preserves peer type",
      test_logical_or_with_throw_preserves_peer_type );
    ( "equality parameter widens across keyword and string",
      test_equality_parameter_widens_across_keyword_and_string );
    ( "contextual equality callback preserves map key type",
      test_contextual_equality_callback_preserves_map_key_type );
    ( "recursive deftype helper widens fallback to dynamic",
      test_recursive_deftype_helper_widens_fallback_to_dynamic );
    ( "vector preserves nullable collection elements",
      test_vector_preserves_nullable_collection_elements );
    ( "loop normalizes seqable parameters to sequences",
      test_loop_normalizes_seqable_parameters_to_sequences );
    ( "dynamic higher-order parameters adapt nominal callbacks",
      test_dynamic_higher_order_parameters_adapt_nominal_callbacks );
    ( "dynamic maps preserve nominal function parameters",
      test_dynamic_maps_preserve_nominal_function_parameters );
    ( "dynamic maps preserve propagated nominal function parameters",
      test_dynamic_maps_preserve_propagated_nominal_function_parameters );
    ( "dynamic map parameters preserve nominal function values",
      test_dynamic_map_parameters_preserve_nominal_function_values );
    ( "dynamic maps instantiate generic record function fields",
      test_dynamic_maps_instantiate_generic_record_function_fields );
    ( "dynamic protocols instantiate generic record receivers",
      test_dynamic_protocols_instantiate_generic_record_receivers );
    ( "dynamic generic nominal arguments stay scoped to the call",
      test_dynamic_generic_nominal_arguments_stay_scoped_to_the_call );
    ( "dynamic generic nominals are consumed inside existential scope",
      test_dynamic_generic_nominals_are_consumed_inside_existential_scope );
    ( "overloaded generic bounds specialize dynamic nominal arguments",
      test_overloaded_generic_bounds_specialize_dynamic_nominal_arguments );
    ( "map->record unpacks dynamic named fields",
      test_map_to_record_unpacks_dynamic_named_fields );
    ( "map->record preserves generic fields from map literals",
      test_map_to_record_preserves_generic_fields_from_map_literals );
    ( "cond-> arrays preserve nominal elements for sorting",
      test_cond_thread_arrays_preserve_nominal_elements_for_sorting );
    ( "cond-> recognizes namespaced array normalization macros",
      test_cond_thread_recognizes_namespaced_array_normalization_macros );
    ( "occurrence type hints only refine their branch",
      test_occurrence_type_hints_only_refine_their_branch );
    ( "update reads dynamic reduce accumulators dynamically",
      test_update_reads_dynamic_reduce_accumulators_dynamically );
    ( "reduce updates heterogeneous vector accumulator slots",
      test_reduce_updates_heterogeneous_vector_accumulator_slots );
    ( "conj is available as a first-class core function",
      test_conj_is_available_as_a_first_class_core_function );
    ( "variadic equality is available in dynamic function maps",
      test_variadic_equality_is_available_in_dynamic_function_maps );
    ( "numeric core functions are available in dynamic function maps",
      test_numeric_core_functions_are_available_in_dynamic_function_maps );
    ( "random core functions are available in dynamic function maps",
      test_random_core_functions_are_available_in_dynamic_function_maps );
    ( "logical core functions are available in dynamic function maps",
      test_logical_core_functions_are_available_in_dynamic_function_maps );
    ( "collection core functions are available in dynamic function maps",
      test_collection_core_functions_are_available_in_dynamic_function_maps );
    ( "printing and regex core functions are available in dynamic maps",
      test_printing_and_regex_core_functions_are_available_in_dynamic_maps );
    ( "clojure.string escape is available in dynamic maps",
      test_clojure_string_escape_is_available_in_dynamic_maps );
    ( "type predicates are available in dynamic function maps",
      test_type_predicates_are_available_in_dynamic_function_maps );
    ( "overloaded functions pack at dynamic map boundaries",
      test_overloaded_functions_pack_at_dynamic_map_boundaries );
    ( "sort accepts dynamic seqable function parameters",
      test_sort_accepts_dynamic_seqable_function_parameters );
    ( "random collection operations preserve element types",
      test_random_collection_operations_preserve_element_types );
    ( "dynamic reduce branch merges with nullable vector fallback",
      test_dynamic_reduce_branch_merges_with_nullable_vector_fallback );
    ( "nested reducers keep entity keyword lookup as map access",
      test_nested_reducers_keep_entity_keyword_lookup_as_map_access );
    ( "get uses dynamic lookup for dynamic targets",
      test_get_uses_dynamic_lookup_for_dynamic_targets );
    ( "dynamic record keys preserve common generic field types",
      test_dynamic_record_keys_preserve_common_generic_field_types );
    ( "deferred generic protocol parameters compile",
      test_deferred_generic_protocol_parameters_compile );
    ( "expected types flow into conditional function parameters",
      test_expected_types_flow_into_conditional_function_parameters );
    ( "nullable and generic sequence branches merge",
      test_nullable_and_generic_sequence_branches_merge );
    ( "annotated predicates resolve record types inside seqable constraints",
      test_annotated_predicates_resolve_record_types_inside_seqable_constraints );
    ( "map accepts nullable sequences returned by protocols",
      test_map_accepts_nullable_sequences_returned_by_protocols );
    ( "map normalizes mixed nullable protocol sequence returns",
      test_map_normalizes_mixed_nullable_protocol_sequence_returns );
    ( "nullable values pack across dynamic logical boundaries",
      test_nullable_values_pack_across_dynamic_logical_boundaries );
    ( "nullable cond branches pack into dynamic results",
      test_nullable_cond_branches_pack_into_dynamic_results );
    ( "update-in uses dynamic callbacks for dynamic record fields",
      test_update_in_uses_dynamic_callbacks_for_dynamic_record_fields );
    ( "dynamic callable type variables propagate to arguments",
      test_dynamic_callable_type_variables_propagate_to_arguments );
    ( "generic calls unpack dynamic nominal arguments",
      test_generic_calls_unpack_dynamic_nominal_arguments );
    ( "additional sequence helpers reject bad counts",
      test_additional_sequence_helpers_reject_bad_counts );
    ( "some returns first truthy predicate value",
      test_some_returns_first_truthy_predicate_value );
    ( "some infers generic Seqable parameters",
      test_some_infers_generic_seqable_parameters );
    ( "map key evidence preserves generic map values",
      test_map_key_evidence_preserves_generic_map_values );
    ( "filter accepts nullable truthy predicate results",
      test_filter_accepts_nullable_truthy_predicate_results );
    ( "remove specializes nested predicate type variables",
      test_remove_specializes_nested_predicate_type_variables );
    ( "nested destructuring materializes erased sequence elements",
      test_nested_destructuring_materializes_erased_sequence_elements );
    ( "swap conj refines atom collection elements",
      test_swap_conj_refines_atom_collection_elements );
    ( "concat specializes unknown elements from static prefix",
      test_concat_specializes_unknown_elements_from_static_prefix );
    ( "conditional conj specializes empty sets from guarded values",
      test_conditional_conj_specializes_empty_sets_from_guarded_values );
    ( "conditional conj localizes dynamic sets for custom guards",
      test_conditional_conj_localizes_dynamic_sets_for_custom_guards );
    ( "select-keys accepts runtime Seqable key collections",
      test_select_keys_accepts_runtime_seqable_key_collections );
    ( "select-keys infers generic runtime map record fields",
      test_select_keys_infers_generic_runtime_map_record_fields );
    ( "select-keys projects open row extension fields",
      test_select_keys_projects_open_row_extension_fields );
    ( "keep drops only nil across generic Seqables",
      test_keep_drops_only_nil_across_generic_seqables );
    ( "map-indexed infers generic Seqable parameters",
      test_map_indexed_infers_generic_seqable_parameters );
    ( "mapv infers destructured callback parameters",
      test_mapv_infers_destructured_callback_parameters );
    ( "destructured defaults specialize overloaded function values",
      test_destructured_defaults_specialize_overloaded_function_values );
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
    ( "nested loops keep recur return types scoped",
      test_nested_loops_keep_recur_return_types_scoped );
    ( "loop/recur remains tail through let and cond",
      test_loop_recur_remains_tail_through_let_and_cond );
    ( "loop/recur remains tail through macros",
      test_loop_recur_remains_tail_through_macros );
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
    ( "heterogeneous destructuring materializes dynamic elements",
      test_heterogeneous_destructuring_materializes_dynamic_elements );
    ( "dynamic predicates do not erase concrete array elements",
      test_dynamic_predicates_do_not_erase_concrete_array_elements );
    ( "dynamic arrays preserve array identity and array-seq",
      test_dynamic_arrays_preserve_array_identity_and_array_seq );
    ( "dynamic recursive array-seq is packed at the self call",
      test_dynamic_recursive_array_seq_is_packed_at_the_self_call );
    ( "nested callback record constraints do not emit fake types",
      test_nested_callback_record_constraints_do_not_emit_fake_types );
    ( "cross namespace named records project to callback rows",
      test_cross_namespace_named_records_project_to_callback_rows );
    ( "macro slots preserve dynamic seqable values",
      test_macro_slots_preserve_dynamic_seqable_values );
    ( "destructuring preserves row polymorphic function calls",
      test_destructuring_preserves_row_polymorphic_function_calls );
    ( "map destructuring as preserves open map access",
      test_map_destructuring_as_preserves_open_map_access );
    ( "map destructuring supports typed direct keyword bindings",
      test_map_destructuring_supports_typed_direct_keyword_bindings );
    ( "row polymorphic functions accept different map shapes",
      test_row_polymorphic_functions_accept_different_map_shapes );
    ( "row types bind nested capability parameters",
      test_row_types_bind_nested_capability_parameters );
    ( "row types bind named record parameters",
      test_row_types_bind_named_record_parameters );
    ( "named record constraints keep stronger nested evidence",
      test_named_record_constraints_keep_stronger_nested_evidence );
    ( "dynamic map rows preserve static generic fields",
      test_dynamic_map_row_preserves_static_generic_field );
    ( "destructuring rejects missing map fields",
      test_destructuring_rejects_missing_map_fields );
    ( "let destructuring supports nested sequences",
      test_let_destructuring_supports_nested_sequences );
    ( "let bindings support value type hints",
      test_let_bindings_support_value_type_hints );
    ( "value type hints preserve nullable record values",
      test_value_type_hints_preserve_nullable_record_values );
    ( "defrecord field hints preserve inferred nullability",
      test_defrecord_field_hints_preserve_inferred_nullability );
    ( "defrecord self constructors preserve hinted field nullability",
      test_defrecord_self_constructors_preserve_hinted_field_nullability );
    ( "explicit type hints narrow nullable field receivers",
      test_explicit_type_hints_narrow_nullable_field_receivers );
    ( "nested record fields preserve outer record inference",
      test_nested_record_fields_preserve_outer_record_inference );
    ( "cross module fnil update packs nominal vectors",
      test_cross_module_fnil_update_packs_nominal_vectors );
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
    ( "sort-by adapts static key function to dynamic elements",
      test_sort_by_adapts_static_key_function_to_dynamic_elements );
    ( "sort-by preserves named record lists",
      test_sort_by_preserves_named_record_lists );
    ( "remove adapts static predicate to dynamic elements",
      test_remove_adapts_static_predicate_to_dynamic_elements );
    ( "some adapts static predicate to dynamic elements",
      test_some_adapts_static_predicate_to_dynamic_elements );
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
    ( "apply calls dynamic runtime functions",
      test_apply_calls_dynamic_runtime_functions );
    ( "dynamic named records preserve mutable field identity",
      test_dynamic_named_records_preserve_mutable_field_identity );
    ( "nil and sequential guards preserve seqability",
      test_nil_and_sequential_guards_preserve_seqability );
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
    ( "take-while transducers compile and truncate sequences",
      test_take_while_transducers_compile_and_truncate_sequences );
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
    ( "assoc accepts nullable static maps",
      test_assoc_accepts_nullable_static_maps );
    ( "nullable dynamic arguments unpack to nominal parameters",
      test_nullable_dynamic_arguments_unpack_to_nominal_parameters );
    ("reduce-kv accepts dynamic maps", test_reduce_kv_accepts_dynamic_maps);
    ( "nominal calls unpack capability storage",
      test_nominal_calls_unpack_capability_storage );
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
    ( "sets support vectors with dynamic elements",
      test_sets_support_vectors_with_dynamic_elements );
    ( "cons and conj pack static values into dynamic vectors",
      test_cons_and_conj_pack_static_values_into_dynamic_vectors );
    ( "concat packs nested dynamic vectors at element boundary",
      test_concat_packs_nested_dynamic_vectors_at_element_boundary );
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
    ( "incremental compile_chunk typechecks each chunk once",
      test_incremental_compile_chunk_typechecks_each_chunk_once );
    ( "incremental compile_chunk checks new code against prior OCaml env",
      test_incremental_compile_chunk_checks_new_code_against_prior_ocaml_env );
    ( "portable compiler state rebuilds OCaml environment",
      test_portable_compiler_state_rebuilds_ocaml_environment );
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
  Printexc.record_backtrace true;
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
            Printf.eprintf "FAILED: %s\n%s\n%s\n" name (Printexc.to_string exn)
              (Printexc.get_backtrace ());
            exit 1)
        tests);
  flush_ocaml_jobs ()
