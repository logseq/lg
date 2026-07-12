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

let string_contains_substring text expected =
  let expected_len = String.length expected in
  let rec loop index =
    index + expected_len <= String.length text
    && (String.sub text index expected_len = expected || loop (index + 1))
  in
  expected_len = 0 || loop 0

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
  | Error (err : Cljml.Compiler.compile_error) ->
      if not (string_contains_substring err.message expected) then
        failwith
          (Printf.sprintf "expected error containing %S, got %S" expected err.message)

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
    | Cljml.Lowered.Value_binding { expression; _ } :: _ -> Some expression
    | Cljml.Lowered.Group items :: rest -> (
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let source = {|(println (str (not false) ":" (true? true) ":" (false? false)))|} in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "boolean_core_api" "true:true:true\n" ocaml_source

let test_not_uses_static_clojure_truthiness () =
  let source =
    {|(println (str (not false) ":" (not 0) ":" (not "Ada") ":" (not [1])))|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "not_uses_static_clojure_truthiness" "true:false:false:false\n"
    ocaml_source

let test_nil_surface_is_not_supported () =
  Cljml.Compiler.compile_string {|(def x nil)|}
  |> expect_error "nil is not supported";
  Cljml.Compiler.compile_string {|(defn bad [^:nil x] x)|}
  |> expect_error "unknown parameter type ^:nil";
  Cljml.Compiler.compile_string {|(def x (nil? 1))|}
  |> expect_error "unknown function nil?";
  Cljml.Compiler.compile_string {|(def x (some? 1))|}
  |> expect_error "unknown function some?"

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

let test_type_relations_are_explicit_and_strict () =
  if Cljml.Types.equal Cljml.Types.TAny Cljml.Types.TInt then
    failwith "TAny must not be strictly equal to int";
  let name = Cljml.Types.make_field ":name" Cljml.Types.TString in
  let narrow = Cljml.Types.TRecord [ name ] in
  let wide =
    Cljml.Types.TRecord
      [ name; Cljml.Types.make_field ":age" Cljml.Types.TInt ]
  in
  if not (Cljml.Types.row_compatible ~expected:narrow ~actual:wide) then
    failwith "wider structural records must remain row-compatible";
  let generated =
    Cljml.Types.named_record ~type_name:"t1" ~set_module_name:"Set_t1"
      [ name; Cljml.Types.make_field ":age" Cljml.Types.TInt ]
  in
  if not (Cljml.Types.row_compatible ~expected:narrow ~actual:generated) then
    failwith "generated records must remain row-compatible with structural rows";
  if
    not
      (Cljml.Types.defer_to_ocaml
         ~expected:(Cljml.Types.TOcaml "user_id")
         ~actual:Cljml.Types.TInt)
  then failwith "opaque OCaml relationships must be explicitly deferred"

let test_named_records_use_nominal_type_identity () =
  let fields = [ Cljml.Types.make_field ":name" Cljml.Types.TString ] in
  let user_id = Cljml.Type_id.create ~owner:[ "Domain" ] ~name:"user" in
  let project_id = Cljml.Type_id.create ~owner:[ "Domain" ] ~name:"project" in
  let user =
    Cljml.Types.named_record ~type_id:user_id ~type_name:"Domain.user"
      ~set_module_name:"Domain.User_set" fields
  in
  let project =
    Cljml.Types.named_record ~type_id:project_id ~type_name:"Domain.project"
      ~set_module_name:"Domain.Project_set" fields
  in
  if Cljml.Types.equal user project then
    failwith "same-shaped named records must remain nominally distinct"

let test_compiler_identities_are_stable_and_distinct () =
  let symbol = Cljml.Symbol_id.create ~owner:[ "Domain" ] ~name:"value" in
  let same_symbol = Cljml.Symbol_id.create ~owner:[ "Domain" ] ~name:"value" in
  let protocol = Cljml.Protocol_id.create ~owner:[ "Domain" ] ~name:"Labelled" in
  if not (Cljml.Symbol_id.equal symbol same_symbol) then
    failwith "symbol identity must be stable for the same owner and name";
  if Cljml.Symbol_id.to_string symbol <> "Domain/value" then
    failwith "symbol identity must preserve its qualified source name";
  if Cljml.Protocol_id.to_string protocol <> "Domain/Labelled" then
    failwith "protocol identity must preserve its qualified source name"

let test_emitted_ocaml_names_reject_source_collisions () =
  Cljml.Compiler.compile_string
    {|
(def foo-bar 1)
(def foo_bar 2)
|}
  |> expect_error_contains
       "OCaml name collision: foo-bar and foo_bar both emit foo_bar";
  Cljml.Compiler.compile_string
    {|
(module Values
  (def active? true)
  (def active_ false))
|}
  |> expect_error_contains
       "OCaml name collision: active? and active_ both emit active_"

let test_modules_resolve_qualified_symbols () =
  let source =
    {|
(module People
  (def user {:name "Ada"}))
(def label (str (get People/user :name) "!"))
(println label)
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "modules_resolve_qualified_symbols" "Ada!\n"
    ocaml_source

let test_modules_prevent_unqualified_symbol_collisions () =
  let source =
    {|
(module First (def x 1))
(module Second (def x 2))
(println (str First/x ":" Second/x))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "modules_prevent_unqualified_symbol_collisions" "1:2\n"
    ocaml_source

let test_namespace_form_is_removed () =
  Cljml.Compiler.compile_string {|(ns legacy.core)|}
  |> expect_error "unknown function ns"

let test_top_level_require_imports_ocaml_modules () =
  let source =
    {|
(require [ocaml.String :as string])
(println (string/uppercase-ascii "ada"))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "top_level_require_imports_ocaml_modules" "ADA\n"
    ocaml_source

let test_top_level_require_rejects_cljml_namespace_imports () =
  Cljml.Compiler.compile_string {|(require [people.core :as people])|}
  |> expect_error_contains
       "require only accepts OCaml packages, OCaml modules, and clojure.string"

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

let test_module_aliases_replace_legacy_import_aliases () =
  let source =
    {|
(module People (def user {:name "Ada"}))
(module-alias P People)
(println (get P/user :name))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_aliases_replace_legacy_import_aliases" "Ada\n" ocaml_source

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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "open_replaces_required_refer" "Ada!\n" ocaml_source

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
(require [ocaml.Stdlib :as std]
            [ocaml.String :as string])
(def label (str (string/uppercase-ascii "ada") ":" (std/string-of-int 42)))
(println label)
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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

let test_unit_annotations_compile_through_source_backend () =
  let source =
    {|
(defn accept-unit [^:unit value]
  (do value (println "unit-ok")))
(accept-unit (run! (fn [^:int x] (println (str "item:" x))) [1]))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "unit_annotations_compile_through_source_backend"
    "item:1\nunit-ok\n" ocaml_source

let test_host_owned_ocaml_type_annotations_compile () =
  let source =
    {|
(defn host-id [^:ocaml/int x] x)
(def answer (host-id 42))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  if not (String.contains ocaml_source ':') then
    failwith "expected generated OCaml to contain a type constraint";
  assert_ocaml_runs "host_owned_ocaml_type_annotations_compile" "" ocaml_source

let test_generic_ocaml_calls_compile_through_source_backend () =
  let source =
    {|
(def answer (ocaml-call :int Stdlib.abs -42))
(def label (ocaml-call :string String.uppercase_ascii "ada"))
(println (str label ":" answer))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_ocaml_calls_compile_through_source_backend"
    "ADA:42\n" ocaml_source

let test_generic_ocaml_calls_accept_unit_return_type () =
  let source =
    {|
(def ignored (ocaml-call :unit Stdlib.ignore 42))
(println "ignored")
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_ocaml_calls_accept_unit_return_type"
    "ignored\n" ocaml_source

let test_generic_ocaml_calls_resolve_required_module_aliases () =
  let source =
    {|
(require [ocaml.Stdlib :as std]
            [ocaml.String :as string])
(def answer (ocaml-call :int std/abs -42))
(def label (ocaml-call :string string/uppercase_ascii "ada"))
(println (str label ":" answer))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_ocaml_calls_resolve_required_module_aliases"
    "ADA:42\n" ocaml_source

let test_generic_ocaml_calls_resolve_required_module_refers () =
  let source =
    {|
(require [ocaml.String :refer [uppercase_ascii]])
(def label (ocaml-call :string uppercase_ascii "ada"))
(println label)
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_ocaml_calls_resolve_required_module_refers"
    "ADA\n" ocaml_source

let test_generic_ocaml_calls_resolve_required_module_refers_in_modules () =
  let source =
    {|
(require [ocaml.String :refer [uppercase_ascii]])
(module Greeter
  (def label (ocaml-call :string uppercase_ascii "ada")))
(println Greeter/label)
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_ocaml_calls_resolve_required_module_refers_in_modules"
    "ADA\n" ocaml_source

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
    (str (ocaml-call :string uppercase_ascii name) M/suffix)))
(module-apply App Make Names)
(println (App/shout "ada"))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_ocaml_calls_resolve_required_module_refers_in_functors"
    "ADA!\n" ocaml_source

let test_typed_ocaml_refers_are_available_in_modules () =
  let source =
    {|
(require [ocaml.String :refer [uppercase-ascii]])
(module Greeter
  (def label (uppercase-ascii "ada")))
(println Greeter/label)
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "typed_ocaml_refers_are_available_in_functors" "ADA!\n"
    ocaml_source

let test_generic_ocaml_calls_resolve_opened_ocaml_modules () =
  let source =
    {|
(open String)
(def label (ocaml-call :string uppercase_ascii "ada"))
(println label)
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "generic_ocaml_calls_resolve_opened_ocaml_modules"
    "ADA\n" ocaml_source

let test_compile_string_runs_ocaml_typecheck_gate_for_host_calls () =
  Cljml.Compiler.compile_string
    {|
(def answer (ocaml-call :int Stdlib.abs "bad"))
|}
  |> expect_error_contains "string"

let test_ocaml_errors_include_cljml_source_locations () =
  Cljml.Compiler.compile_string
    {|
(def ok 1)

(def answer (Stdlib.abs "bad"))
|}
  |> expect_error_contains "File \"<string>\", line 4"

let test_parsetree_items_preserve_top_level_source_locations () =
  let structure =
    Cljml.Compiler.compile_parsetree
      {|
(def first 1)

(def second 2)
|}
    |> expect_ok
  in
  match structure with
  | [ first; second ] ->
      if first.pstr_loc.loc_start.pos_fname <> "<string>"
         || first.pstr_loc.loc_start.pos_lnum <> 2
      then failwith "expected first Parsetree item at <string>:2";
      if second.pstr_loc.loc_start.pos_fname <> "<string>"
         || second.pstr_loc.loc_start.pos_lnum <> 4
      then failwith "expected second Parsetree item at <string>:4"
  | _ -> failwith "expected two located Parsetree items"

let test_incremental_parsetree_preserves_chunk_source_locations () =
  let state, _ =
    Cljml.Compiler.compile_chunk_parsetree Cljml.Compiler.empty_state
      "\n(def first 1)"
    |> expect_ok
  in
  let _, structure =
    Cljml.Compiler.compile_chunk_parsetree state "\n\n(def second 2)"
    |> expect_ok
  in
  match structure with
  | [ item ] ->
      if item.pstr_loc.loc_start.pos_fname <> "<string>"
         || item.pstr_loc.loc_start.pos_lnum <> 3
      then failwith "expected incremental Parsetree item at <string>:3"
  | _ -> failwith "expected one incremental Parsetree item"

let test_ocaml_errors_include_nested_expression_locations () =
  Cljml.Compiler.compile_string
    "(def answer\n  (if true\n    (Stdlib.abs\n      \"bad\")\n    0))"
  |> expect_error_contains "line 4, characters 6-11"

let test_parsetree_expressions_preserve_nested_source_locations () =
  let structure =
    Cljml.Compiler.compile_parsetree
      "(def answer\n  (if true\n    (String.uppercase_ascii\n      \"bad\")\n    \"ok\"))"
  in
  match structure with
  | Error err ->
      failwith ("expected valid nested expression, got: " ^ err.message)
  | Ok
      [ { pstr_desc = Pstr_value (_, [ binding ]); _ } ] -> (
      match binding.pvb_expr.pexp_desc with
      | Pexp_ifthenelse (_, then_expression, _) -> (
          match then_expression.pexp_desc with
          | Pexp_apply (_, [ (_, argument) ]) ->
              let start = argument.pexp_loc.loc_start in
              let finish = argument.pexp_loc.loc_end in
              if start.pos_lnum <> 4 || start.pos_cnum - start.pos_bol <> 6
                 || finish.pos_cnum - finish.pos_bol <> 11
              then failwith "expected nested string expression at line 4, characters 6-11"
          | _ -> failwith "expected nested OCaml application")
      | _ -> failwith "expected generated conditional expression")
  | Ok _ -> failwith "expected one generated value item"

let test_inferred_ocaml_calls_use_compiler_signatures () =
  let source =
    {|
(def answer (ocaml-call Stdlib.abs -42))
(def label (ocaml-call String.uppercase_ascii "ada"))
(println (str label ":" answer))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "inferred_ocaml_calls_use_compiler_signatures"
    "ADA:42\n" ocaml_source

let test_inferred_ocaml_calls_resolve_aliases_and_refers () =
  let source =
    {|
(require [ocaml.Stdlib :as std]
            [ocaml.String :refer [uppercase_ascii]])
(def answer (ocaml-call std/abs -42))
(def label (ocaml-call uppercase_ascii "ada"))
(println (str label ":" answer))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "inferred_ocaml_calls_resolve_aliases_and_refers"
    "ADA:42\n" ocaml_source

let test_inferred_ocaml_calls_reject_incompatible_arguments () =
  Cljml.Compiler.compile_string
    {|
(def answer (ocaml-call Stdlib.abs "bad"))
|}
  |> expect_error_contains "string"

let test_inferred_ocaml_calls_reject_unknown_values () =
  Cljml.Compiler.compile_string
    {|
(def answer (ocaml-call Stdlib.not_a_real_value 42))
|}
  |> expect_error_contains "Unbound value"

let test_inferred_ocaml_calls_support_required_labels () =
  let source =
    {|
(def starts (ocaml-call String.starts_with "ada" :prefix "ad"))
(println starts)
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "inferred_ocaml_calls_support_required_labels"
    "true\n" ocaml_source

let test_inferred_ocaml_calls_support_optional_labels () =
  let source =
    {|
(def default-distance (ocaml-call String.edit_distance "abc" "adc"))
(def limited-distance
  (ocaml-call String.edit_distance "abc" "adc" :limit 2))
(println (+ default-distance limited-distance))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "inferred_ocaml_calls_support_optional_labels"
    "2\n" ocaml_source

let test_inferred_ocaml_calls_preserve_partial_labelled_functions () =
  let source =
    {|
(def starts-ad (ocaml-call String.starts_with :prefix "ad"))
(println (starts-ad "ada"))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "inferred_ocaml_calls_preserve_partial_labelled_functions"
    "true\n" ocaml_source

let test_inferred_ocaml_calls_support_labels_through_aliases () =
  let source =
    {|
(require [ocaml.String :as string])
(println (ocaml-call string/starts_with "ada" :prefix "ad"))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "inferred_ocaml_calls_support_labels_through_aliases"
    "true\n" ocaml_source

let test_inferred_ocaml_calls_reject_bad_labels () =
  Cljml.Compiler.compile_string
    {|(def value (ocaml-call String.starts_with "ada" :unknown "ad"))|}
  |> expect_error_contains "unknown OCaml argument label :unknown";
  Cljml.Compiler.compile_string
    {|
(def value
  (ocaml-call String.starts_with "ada" :prefix "ad" :prefix "a"))
|}
  |> expect_error_contains "duplicate OCaml argument label :prefix";
  Cljml.Compiler.compile_string
    {|(def value (ocaml-call String.starts_with "ada" :prefix))|}
  |> expect_error_contains "OCaml argument label :prefix requires a value"

let test_inferred_labelled_calls_delegate_value_types_to_ocaml () =
  Cljml.Compiler.compile_string
    {|(def value (ocaml-call String.starts_with "ada" :prefix 42))|}
  |> expect_error_contains "int"

let test_ocaml_package_requires_enable_inferred_calls () =
  Cljml.Compiler.compile_string
    {|
(require [ocaml.package/core]
            [ocaml.Core.Int :as int])
(def answer (ocaml-call int/abs -42))
(println answer)
|}
  |> expect_ok |> ignore

let test_ocaml_package_requires_report_missing_packages () =
  Cljml.Compiler.compile_string
    {|
(require [ocaml.package/cljml-package-that-does-not-exist]
            [ocaml.Missing :as missing])
(def answer (ocaml-call missing/value 42))
|}
  |> expect_error_contains "OCaml package cljml-package-that-does-not-exist was not found"

let test_ocaml_package_requires_reject_invalid_package_names () =
  Cljml.Compiler.compile_string
    {|
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "direct_ocaml_calls_support_labels_and_optional_arguments"
    "true:1\n" ocaml_source

let test_direct_ocaml_calls_use_external_packages () =
  Cljml.Compiler.compile_string
    {|
(require [ocaml.package/core]
            [ocaml.Core.Int :as int])
(println (int/abs -42))
|}
  |> expect_ok |> ignore

let test_direct_external_package_constructors_are_inferred () =
  Cljml.Compiler.compile_string
    {|
(require [ocaml.package/unix]
            [ocaml.Unix :as unix])
(def address (unix/ADDR_UNIX "/tmp/cljml.sock"))
(println "constructor-ok")
|}
  |> expect_ok |> ignore

let test_direct_external_package_constructors_reject_bad_arity () =
  Cljml.Compiler.compile_string
    {|
(require [ocaml.package/unix]
            [ocaml.Unix :as unix])
(def address (unix/ADDR_UNIX))
|}
  |> expect_error "unix/ADDR_UNIX expects 1 arguments"

let test_direct_external_package_constructor_payloads_are_checked_by_ocaml () =
  Cljml.Compiler.compile_string
    {|
(require [ocaml.package/unix]
            [ocaml.Unix :as unix])
(def address (unix/ADDR_UNIX 42))
|}
  |> expect_error_contains "string"

let test_direct_ocaml_calls_delegate_errors_to_ocaml () =
  Cljml.Compiler.compile_string {|(def answer (Stdlib.abs "bad"))|}
  |> expect_error_contains "string";
  Cljml.Compiler.compile_string
    {|(def answer (String.starts_with "ada" :unknown "a"))|}
  |> expect_error_contains "unknown OCaml argument label :unknown";
  Cljml.Compiler.compile_string {|(def answer (Stdlib.not_a_real_value 42))|}
  |> expect_error_contains "Unbound value"

let test_generic_ocaml_calls_reject_bad_forms () =
  Cljml.Compiler.compile_string {|(def answer (ocaml-call :unknown Stdlib.abs -42))|}
  |> expect_error "unknown ocaml-call return type :unknown";
  Cljml.Compiler.compile_string {|(def answer (ocaml-call :int :bad -42))|}
  |> expect_error "ocaml-call function must be a symbol"

let test_parsetree_typecheck_gate_rejects_invalid_required_module_alias_calls () =
  Cljml.Compiler.compile_parsetree
    {|
(require [ocaml.Stdlib :as std])
(def answer (ocaml-call :int std/abs "bad"))
|}
  |> expect_error_contains "string"

let test_parsetree_typecheck_gate_accepts_valid_host_calls () =
  Cljml.Compiler.typecheck_parsetree
    {|
(def answer (ocaml-call :int Stdlib.abs -42))
(def label (ocaml-call :string String.uppercase_ascii "ada"))
|}
  |> expect_ok

let test_parsetree_typecheck_gate_rejects_invalid_host_calls () =
  Cljml.Compiler.typecheck_parsetree
    {|
(def answer (ocaml-call :int Stdlib.abs "bad"))
|}
  |> expect_error_contains "string"

let test_parsetree_typecheck_gate_accepts_runtime_dependencies () =
  Cljml.Compiler.typecheck_parsetree
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
(type-alias user-id :ocaml/int)
(defn keep-user-id [^:ocaml/user_id x] x)
(def answer (keep-user-id 42))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  if not (String.contains ocaml_source '=') then
    failwith "expected generated OCaml to contain a type alias";
  assert_ocaml_runs "type_aliases_compile_through_source_backend" "" ocaml_source

let test_parameterized_type_declarations_compile () =
  let source =
    {|
(type-alias maybe [a] :ocaml/option<param/a>)
(type-record pair [a b]
  (left :param/a)
  (right :param/b))
(type-variant box [a]
  (Box :param/a))
(def pair-value (ocaml-record pair (left 42) (right "Ada")))
(def box-value (ocaml-construct Box 42))
(println "parameterized-ok")
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "parameterized_type_declarations_compile"
    "parameterized-ok\n" ocaml_source

let test_parameterized_types_compile_inside_modules () =
  let source =
    {|
(module Types
  (type-alias maybe [a] :ocaml/option<param/a>)
  (type-record pair [a b]
    (left :param/a)
    (right :param/b))
  (type-variant box [a]
    (Box :param/a)))
(println "module-parameterized-ok")
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "parameterized_types_compile_inside_modules"
    "module-parameterized-ok\n" ocaml_source

let test_parameterized_record_relationships_are_checked_by_ocaml () =
  Cljml.Compiler.compile_string
    {|
(type-record same-pair [a]
  (left :param/a)
  (right :param/a))
(def bad (ocaml-record same-pair (left 42) (right "Ada")))
|}
  |> expect_error_contains "string"

let test_parameterized_variant_relationships_are_checked_by_ocaml () =
  Cljml.Compiler.compile_string
    {|
(type-variant same-pair [a]
  (Pair :param/a :param/a))
(def bad (ocaml-construct Pair 42 "Ada"))
|}
  |> expect_error_contains "string"

let test_parameterized_type_declarations_reject_bad_parameters () =
  Cljml.Compiler.compile_string
    {|(type-alias maybe [a a] :ocaml/option<param/a>)|}
  |> expect_error_contains "duplicate type parameter a";
  Cljml.Compiler.compile_string
    {|(type-record pair [a :bad] (value :param/a))|}
  |> expect_error_contains "type parameters must be symbols";
  Cljml.Compiler.compile_string
    {|(type-variant box [a] (Box :param/missing))|}
  |> expect_error_contains "unknown type parameter missing";
  Cljml.Compiler.compile_string
    {|(type-alias maybe [] :ocaml/option<int>)|}
  |> expect_error_contains "type parameter vector must not be empty"

let test_ocaml_owned_branch_types_are_checked_by_ocaml () =
  let source =
    {|
(type-alias user-id :ocaml/int)
(type-alias account-id :ocaml/int)
(defn as-user [^:ocaml/user_id x] x)
(defn as-account [^:ocaml/account_id x] x)
(def if-id (if true (as-user 41) (as-account 42)))
(def cond-id (cond false (as-user 1) :else (as-account 2)))
(def match-id (match true true (as-user 3) false (as-account 4)))
(println "aliases-ok")
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_owned_branch_types_are_checked_by_ocaml"
    "aliases-ok\n" ocaml_source

let test_ocaml_owned_branch_type_mismatch_is_delegated_to_ocaml () =
  Cljml.Compiler.compile_string
    {|
(type-alias user-id :ocaml/int)
(defn as-user [^:ocaml/user_id x] x)
(def bad (if true (as-user 41) "bad"))
|}
  |> expect_error_contains "string"

let test_ocaml_option_and_result_constructors_compile_through_source_backend () =
  let source =
    {|
(def present (ocaml-some 42))
(def absent (ocaml-none))
(def success (ocaml-ok "Ada"))
(def failure (ocaml-error "bad"))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "ocaml_option_and_result_constructors_compile_through_source_backend"
    "" ocaml_source

let test_ocaml_option_and_result_constructors_reject_bad_arity () =
  Cljml.Compiler.compile_string {|(def value (ocaml-some))|}
  |> expect_error "ocaml-some expects 1 arguments";
  Cljml.Compiler.compile_string {|(def value (ocaml-none 1))|}
  |> expect_error "ocaml-none expects 0 arguments";
  Cljml.Compiler.compile_string {|(def value (ocaml-ok))|}
  |> expect_error "ocaml-ok expects 1 arguments";
  Cljml.Compiler.compile_string {|(def value (ocaml-error))|}
  |> expect_error "ocaml-error expects 1 arguments"

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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "direct_declared_variant_constructors_compile"
    "active:Ada\n" ocaml_source

let test_direct_ocaml_constructors_reject_bad_arity () =
  Cljml.Compiler.compile_string {|(def value (Some))|}
  |> expect_error "Some expects 1 arguments";
  Cljml.Compiler.compile_string {|(def value (None 1))|}
  |> expect_error "None expects 0 arguments";
  Cljml.Compiler.compile_string
    {|
(type-variant status Active (Named :string))
(def value (Named))
|}
  |> expect_error "Named expects 1 arguments"

let test_ocaml_option_and_result_patterns_compile_through_source_backend () =
  let source =
    {|
(def present (ocaml-some 41))
(def absent (ocaml-none))
(def success (ocaml-ok "Ada"))
(def failure (ocaml-error "bad"))
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "ocaml_option_and_result_patterns_compile_through_source_backend"
    "42:0:Ada:bad\n" ocaml_source

let test_ocaml_option_patterns_delegate_payload_typecheck_to_ocaml () =
  Cljml.Compiler.compile_string
    {|
(def present (ocaml-some "bad"))
(def bad
  (match present
    (Some x) (+ x 1)
    None 0))
|}
  |> expect_error_contains "string"

let test_ocaml_type_application_annotations_compile_through_source_backend () =
  let source =
    {|
(def present (ocaml-some 41))
(def absent (ocaml-none))
(def success (ocaml-ok "Ada"))
(def failure (ocaml-error "bad"))
(defn option-score [^:ocaml/option<int> value]
  (match value
    (Some x) (+ x 1)
    None 0))
(defn result-label [^:ocaml/result<string;string> value]
  (match value
    (Ok name) name
    (Error message) message))
(println (str (option-score present) ":" (option-score absent) ":"
              (result-label success) ":" (result-label failure)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "ocaml_type_application_annotations_compile_through_source_backend"
    "42:0:Ada:bad\n" ocaml_source

let test_ocaml_type_application_annotations_delegate_argument_mismatch_to_ocaml () =
  Cljml.Compiler.compile_string
    {|
(def present (ocaml-some "bad"))
(defn option-score [^:ocaml/option<int> value]
  (match value
    (Some x) (+ x 1)
    None 0))
(def bad (option-score present))
|}
  |> expect_error_contains "string"

let test_ocaml_type_application_annotations_reject_bad_forms () =
  Cljml.Compiler.compile_string
    {|(defn bad [^:ocaml/option<> value] value)|}
  |> expect_error "invalid OCaml type annotation ^:ocaml/option<>";
  Cljml.Compiler.compile_string
    {|(defn bad [^:ocaml/result<int> value] value)|}
  |> expect_error "invalid OCaml type annotation ^:ocaml/result<int>"

let test_ocaml_tuple_values_compile_through_source_backend () =
  let source =
    {|
(def pair (ocaml-tuple 41 "Ada"))
(defn describe [^:ocaml/tuple<int;string> value]
  (match value
    (ocaml-tuple id name) (str name ":" (+ id 1))))
(println (describe pair))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_tuple_values_compile_through_source_backend"
    "Ada:42\n" ocaml_source

let test_ocaml_tuple_values_delegate_argument_mismatch_to_ocaml () =
  Cljml.Compiler.compile_string
    {|
(def pair (ocaml-tuple "bad" "Ada"))
(defn describe [^:ocaml/tuple<int;string> value]
  (match value
    (ocaml-tuple id name) (str name ":" (+ id 1))))
(def bad (describe pair))
|}
  |> expect_error_contains "string"

let test_ocaml_tuple_values_reject_bad_forms () =
  Cljml.Compiler.compile_string {|(def value (ocaml-tuple 1))|}
  |> expect_error "ocaml-tuple expects at least 2 values";
  Cljml.Compiler.compile_string
    {|(defn bad [^:ocaml/tuple<int> value] value)|}
  |> expect_error "invalid OCaml type annotation ^:ocaml/tuple<int>";
  Cljml.Compiler.compile_string
    {|
(def pair (ocaml-tuple 1 "Ada"))
(def bad (match pair
  (ocaml-tuple id) id))
|}
  |> expect_error "tuple pattern arity mismatch"

let test_ocaml_float_and_char_literals_compile () =
  let source =
    {|
(def sum (Float.add 1.5 2.25))
(def upper (Char.uppercase_ascii \a))
(println (str (Float.to_string sum) ":" (String.make 1 upper)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_float_and_char_literals_compile" "3.75:A\n"
    ocaml_source

let test_ocaml_arrays_support_construction_read_and_mutation () =
  let source =
    {|
(def values (ocaml-array 1 2 3))
(ocaml-array-set! values 1 42)
(def empty-values (ocaml-array-of :int))
(println (str (+ (ocaml-array-get values 1) 0) ":" (Array.length empty-values)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_arrays_support_construction_read_and_mutation"
    "42:0\n" ocaml_source

let test_ocaml_refs_support_read_and_assignment () =
  let source =
    {|
(def cell (ocaml-ref 40))
(ocaml-reset! cell (+ (ocaml-deref cell) 2))
(println (ocaml-deref cell))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_refs_support_read_and_assignment" "42\n" ocaml_source

let test_ocaml_arrays_reject_invalid_operations () =
  Cljml.Compiler.compile_string {|(def values (ocaml-array 1 "two"))|}
  |> expect_error_contains "OCaml array elements must have the same type";
  Cljml.Compiler.compile_string {|(def value (ocaml-array-get 42 0))|}
  |> expect_error_contains "ocaml-array-get expects an OCaml array";
  Cljml.Compiler.compile_string
    {|(def value (ocaml-array-get (ocaml-array 1 2) "0"))|}
  |> expect_error_contains "OCaml array index must be int";
  Cljml.Compiler.compile_string
    {|(ocaml-array-set! (ocaml-array 1 2) 0 "bad")|}
  |> expect_error_contains "OCaml array value must match element type";
  Cljml.Compiler.compile_string {|(def values (ocaml-array))|}
  |> expect_error_contains "empty OCaml array requires a type"

let test_ocaml_refs_reject_invalid_operations () =
  Cljml.Compiler.compile_string {|(def value (ocaml-deref 42))|}
  |> expect_error_contains "ocaml-deref expects an OCaml ref";
  Cljml.Compiler.compile_string {|(ocaml-reset! 42 1)|}
  |> expect_error_contains "ocaml-reset! expects an OCaml ref";
  Cljml.Compiler.compile_string {|(ocaml-reset! (ocaml-ref 1) "bad")|}
  |> expect_error_contains "OCaml ref value must match referenced type"

let test_ocaml_float_literals_remain_outside_integer_arithmetic () =
  Cljml.Compiler.compile_string {|(def bad (+ 1.5 2.0))|}
  |> expect_error_contains "expected int arguments for +"

let test_ocaml_record_values_compile_through_source_backend () =
  let source =
    {|
(type-record user (name :string) (age :int))
(def ada (ocaml-record user (name "Ada") (age 41)))
(println (str (ocaml-field ada name) ":" (+ (ocaml-field ada age) 1)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_record_values_compile_through_source_backend"
    "Ada:42\n" ocaml_source

let test_ocaml_record_values_support_qualified_module_types () =
  let source =
    {|
(module User
  (type-record user (name :string) (age :int)))
(def ada (ocaml-record User.user (name "Ada") (age 41)))
(println (str (ocaml-field ada name) ":" (+ (ocaml-field ada age) 1)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_record_values_support_qualified_module_types"
    "Ada:42\n" ocaml_source

let test_ocaml_record_values_support_module_alias_types () =
  let source =
    {|
(module User
  (type-record user (name :string) (age :int)))
(module-alias U User)
(def ada (ocaml-record U.user (name "Ada") (age 41)))
(println (str (ocaml-field ada name) ":" (+ (ocaml-field ada age) 1)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_record_values_support_module_alias_types"
    "Ada:42\n" ocaml_source

let test_ocaml_record_values_support_opened_module_types () =
  let source =
    {|
(module User
  (type-record user (name :string) (age :int)))
(open User)
(def ada (ocaml-record user (name "Ada") (age 41)))
(println (str (ocaml-field ada name) ":" (+ (ocaml-field ada age) 1)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_record_values_support_opened_module_types"
    "Ada:42\n" ocaml_source

let test_ocaml_record_values_support_opened_module_types_in_module_body () =
  let source =
    {|
(module User
  (type-record user (name :string) (age :int)))
(module App
  (open User)
  (def ada (ocaml-record user (name "Ada") (age 41)))
  (def label (str (ocaml-field ada name) ":" (+ (ocaml-field ada age) 1))))
(println App/label)
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_record_values_support_opened_module_types_in_module_body"
    "Ada:42\n" ocaml_source

let test_ocaml_record_values_support_included_module_types () =
  let source =
    {|
(module User
  (type-record user (name :string) (age :int)))
(module App
  (include User))
(def ada (ocaml-record App.user (name "Ada") (age 41)))
(println (str (ocaml-field ada name) ":" (+ (ocaml-field ada age) 1)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_record_values_support_included_module_types"
    "Ada:42\n" ocaml_source

let test_ocaml_record_values_delegate_qualified_field_typecheck_to_ocaml () =
  Cljml.Compiler.compile_string
    {|
(module User
  (type-record user (name :string) (age :int)))
(def bad (ocaml-record User.user (name "Ada") (age "old")))
|}
  |> expect_error_contains "string"

let test_ocaml_record_values_delegate_field_typecheck_to_ocaml () =
  Cljml.Compiler.compile_string
    {|
(type-record user (name :string) (age :int))
(def bad (ocaml-record user (name "Ada") (age "old")))
|}
  |> expect_error_contains "string"

let test_ocaml_record_values_reject_bad_forms () =
  Cljml.Compiler.compile_string {|(type-record user)|}
  |> expect_error "type-record expects at least one field";
  Cljml.Compiler.compile_string {|(type-record user (name :unknown))|}
  |> expect_error "unknown record field type :unknown";
  Cljml.Compiler.compile_string {|(def bad (ocaml-record user))|}
  |> expect_error "unknown record type user";
  Cljml.Compiler.compile_string
    {|
(type-record user (name :string))
(def bad (ocaml-record user (name "Ada") (name "Grace")))
|}
  |> expect_error "duplicate record field name";
  Cljml.Compiler.compile_string
    {|
(type-record user (name :string))
(def ada (ocaml-record user (name "Ada")))
(def bad (ocaml-field ada age))
|}
  |> expect_error "unknown record field age"

let test_ocaml_variants_compile_through_source_backend () =
  let source =
    {|
(type-variant status Active Inactive)
(def active (ocaml-construct Active))
(defn keep-status [^:ocaml/status x] x)
(def saved (keep-status active))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_variants_compile_through_source_backend" "" ocaml_source

let test_ocaml_payload_variants_compile_through_source_backend () =
  let source =
    {|
(type-variant message Ping (Named :string) (Pair :int :string))
(def named (ocaml-construct Named "Ada"))
(def pair (ocaml-construct Pair 42 "Ada"))
(defn describe [^:ocaml/message message]
  (match message
    (Named name) name
    (Pair id name) (str name ":" id)
    Ping "ping"))
(println (str (describe named) ":" (describe pair)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "ocaml_payload_variants_compile_through_source_backend"
    "Ada:Ada:42\n" ocaml_source

let test_ocaml_payload_variants_delegate_payload_typecheck_to_ocaml () =
  Cljml.Compiler.compile_string
    {|
(type-variant message (Named :string))
(def bad (ocaml-construct Named 42))
|}
  |> expect_error_contains "int"

let test_ocaml_variant_constructors_reject_bad_arity () =
  Cljml.Compiler.compile_string {|(def value (ocaml-construct))|}
  |> expect_error "ocaml-construct expects a constructor name";
  Cljml.Compiler.compile_string {|(def value (ocaml-construct :Active 1))|}
  |> expect_error "ocaml-construct constructor must be a symbol"

let test_ocaml_variants_reject_bad_declarations () =
  Cljml.Compiler.compile_string {|(type-variant status)|}
  |> expect_error "type-variant expects at least one constructor";
  Cljml.Compiler.compile_string {|(type-variant status Active Active)|}
  |> expect_error "duplicate variant constructor Active";
  Cljml.Compiler.compile_string {|(type-variant status :Active)|}
  |> expect_error "type-variant constructors must be symbols"

let test_typed_function_parameters_reject_bad_calls () =
  let source =
    {|
(defn inc1 [^:int x] (+ x 1))
(def bad (inc1 "Ada"))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "inc1 called with incompatible arguments"

let test_unit_annotations_reject_non_unit_arguments () =
  let source =
    {|
(defn accept-unit [^:unit value] value)
(def bad (accept-unit 1))
|}
  in
  Cljml.Compiler.compile_string source
  |> expect_error "accept-unit called with incompatible arguments"

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

let test_identity_function_is_polymorphic_at_call_sites () =
  let source =
    {|
(defn identity-value [x] x)
(println (str (identity-value 42) ":" (identity-value "Ada") ":" (identity-value true)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "conditional_function_is_polymorphic_at_call_sites"
    "42:Ada\n" ocaml_source

let test_conditional_function_type_relationship_is_checked_by_ocaml () =
  Cljml.Compiler.compile_string
    {|
(defn choose [flag left right]
  (if flag left right))
(def bad (choose true 42 "Ada"))
|}
  |> expect_error_contains "expected of type"

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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "static_protocols_work_through_module_aliases" "int:9\n"
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  Cljml.Compiler.compile_string source
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
  Cljml.Compiler.compile_string source
  |> expect_error_contains "protocol method join parameter 2 must be string"

let test_protocols_support_named_record_receivers () =
  let source =
    {|
(type-record user (name :string))
(defprotocol Labelled
  (label [value] :string))
(extend-type user
  Labelled
  (label [value] (ocaml-field value name)))
(def ada (ocaml-record user (name "Ada")))
(println (label ada))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "protocols_support_named_record_receivers" "Ada\n" ocaml_source

let test_protocols_inside_modules_export_methods_and_record_impls () =
  let source =
    {|
(module Domain
  (type-record user (name :string))
  (defprotocol Labelled
    (label [value] :string))
  (extend-type user
    Labelled
    (label [value] (ocaml-field value name)))
  (def ada (ocaml-record user (name "Ada"))))
(println (Domain/Labelled/label Domain/ada))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "protocols_inside_modules_export_methods_and_record_impls"
    "Ada\n" ocaml_source

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
  |> expect_error "when body must be unit"

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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "batched_numeric_scalar_core_functions_work"
    "true:false:true:false:false:true:false:true:false:true:false:true:4:5:0:true:false:4611686018427387903:3:3:2:2:12:12:3:1:5:5:3:3:-4:-4:name:Ada::admin?::ready\n"
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_string_module_batch_works"
    "ADA|ada|Ada|cba|hi|left|right|line|baNANA|baNAna|$1\ntrue:true:true:true:2:4:[\"a\" \"b\" \"c\"]:[\"a\" \"b\"]\n"
    ocaml_source

let test_clojure_string_module_refer_works () =
  let source =
    {|
(require [clojure.string :refer [upper-case trim]])
(println (str (upper-case (trim " ada "))))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "clojure_string_module_refer_works" "ADA\n" ocaml_source

let test_clojure_string_module_rejects_bad_args () =
  Cljml.Compiler.compile_string
    {|
(require [clojure.string :as str])
(def x (str/upper-case 1))
|}
  |> expect_error "str/upper-case called with incompatible arguments"

let test_clojure_string_module_rejects_unknown_refer () =
  Cljml.Compiler.compile_string
    {|
(require [clojure.string :refer [missing]])
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "batched_predicate_collection_core_functions_work"
    "true:true:false:false:false:false:false:true:false:true:false:true:true:true:true:false:true:false:false:3:5:[1 2 3 4]:[4 5]:[1 2 3]:[1 3 5]:2:[1 2]:[3 4 5]:[1 2 3]:[4 5]:3:2:2:done:[1 2 3 4 5]\nitem:1\nitem:2\n"
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "loop_and_recur_are_tail_recursive" "15\n" ocaml_source

let test_loop_and_recur_delegate_ocaml_owned_alias_compatibility () =
  let source =
    {|
(type-alias user-id :ocaml/int)
(type-alias account-id :ocaml/int)
(defn as-user [^:ocaml/user_id x] x)
(defn as-account [^:ocaml/account_id x] x)
(def final-id
  (loop [id (as-user 0)
         n 1]
    (if (= n 0)
      id
      (recur (as-account 42) (dec n)))))
(println "loop-ok")
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "loop_and_recur_delegate_ocaml_owned_alias_compatibility"
    "loop-ok\n" ocaml_source

let test_loop_and_recur_delegate_ocaml_owned_mismatch_to_ocaml () =
  Cljml.Compiler.compile_string
    {|
(type-alias user-id :ocaml/int)
(defn as-user [^:ocaml/user_id x] x)
(def bad
  (loop [id (as-user 0)
         n 1]
    (if (= n 0)
      id
      (recur "bad" (dec n)))))
|}
  |> expect_error_contains "string"

let test_loop_and_recur_reject_invalid_calls () =
  Cljml.Compiler.compile_string {|(recur 1)|}
  |> expect_error "recur is only valid in a loop tail position";
  Cljml.Compiler.compile_string
    {|(loop [n 1] (recur n 0))|}
  |> expect_error "recur expects 1 arguments";
  Cljml.Compiler.compile_string
    {|(loop [n 1] (recur "one"))|}
  |> expect_error "recur argument 1 must be int";
  Cljml.Compiler.compile_string
    {|(loop [n 1] (+ 1 (recur (dec n))))|}
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
(println (str (count matching) ":" (contains? matching ada) ":" all-ada? ":"
              (count ages) ":" (contains? ages 36) ":" (count trimmed) ":"
              (contains? rebuilt ada)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "sets_support_primitive_lists_and_vectors"
    "1:true:2:true:#{(1 2)}:#{[1 2] [2 3]}\n"
    ocaml_source

let test_sets_support_nested_composite_elements () =
  let source =
    {|
(def paths (hash-set [[1 2] [3 4]] [[1 2] [3 4]]))
(def updated (conj paths [[5 6]]))
(println (str (count updated) ":" (contains? updated [[5 6]])))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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

let test_sets_reject_nil_elements () =
  Cljml.Compiler.compile_string {|(def values (set-of :nil))|}
  |> expect_error "unknown set element type :nil";
  Cljml.Compiler.compile_string {|(def values (hash-set nil))|}
  |> expect_error "nil is not supported";
  Cljml.Compiler.compile_string {|(def values (set [nil]))|}
  |> expect_error "nil is not supported"

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

let test_match_supports_ocaml_constructor_patterns () =
  let source =
    {|
(type-variant status Active Inactive)
(def active (ocaml-construct Active))
(def inactive (ocaml-construct Inactive))
(defn describe [^:ocaml/status status]
  (match status
    Active "active"
    Inactive "inactive"))
(println (str (describe active) ":" (describe inactive)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "match_supports_ocaml_constructor_patterns"
    "active:inactive\n" ocaml_source

let test_compile_diagnostics_capture_ocaml_match_warnings () =
  let source =
    {|
(type-variant status Active Inactive)
(defn describe [^:ocaml/status status]
  (match status
    Active "active"))
|}
  in
  let compilation =
    Cljml.Compiler.compile_string_with_filename_and_diagnostics
      ~filename:"warning.cljml" source
    |> expect_ok
  in
  match compilation.diagnostics with
  | [ diagnostic ] ->
      if diagnostic.severity <> `Warning then
        failwith "expected an OCaml warning diagnostic";
      if not (string_contains_substring diagnostic.message "not exhaustive") then
        failwith
          ("expected non-exhaustive match warning, got: " ^ diagnostic.message);
      if not (string_contains_substring diagnostic.message "warning.cljml") then
        failwith ("expected warning filename, got: " ^ diagnostic.message)
  | diagnostics ->
      failwith
        (Printf.sprintf "expected one warning diagnostic, got %d"
           (List.length diagnostics))

let test_compile_diagnostics_are_empty_for_exhaustive_matches () =
  let source =
    {|
(type-variant status Active Inactive)
(defn describe [^:ocaml/status status]
  (match status
    Active "active"
    Inactive "inactive"))
|}
  in
  let compilation =
    Cljml.Compiler.compile_string_with_diagnostics source |> expect_ok
  in
  if compilation.diagnostics <> [] then
    failwith "expected exhaustive match compilation to have no diagnostics"

let language_service_source =
  {|
(def answer 41)
(defn add-one [x] (+ x 1))
(def result (add-one answer))
|}

let analyze_language_service_source () =
  Cljml.Language_service.analyze ~filename:"file:///tmp/service.cljml"
    language_service_source
  |> expect_ok

let test_language_service_hover_uses_ocaml_types () =
  let analysis = analyze_language_service_source () in
  let offset = expect_substring_index language_service_source "add-one answer" in
  match Cljml.Language_service.hover analysis ~offset with
  | Some hover ->
      if not (string_contains_substring hover.contents "int -> int") then
        failwith ("expected inferred OCaml function type, got: " ^ hover.contents)
  | None -> failwith "expected hover information for add-one"

let test_language_service_definition_resolves_source_binding () =
  let analysis = analyze_language_service_source () in
  let usage = expect_substring_index language_service_source "answer))" in
  match Cljml.Language_service.definition analysis ~offset:usage with
  | Some location ->
      if location.Location.loc_start.Lexing.pos_lnum <> 2 then
        failwith "expected answer definition on source line 2"
  | None -> failwith "expected definition for answer usage"

let test_language_service_completion_uses_source_names_and_types () =
  let analysis = analyze_language_service_source () in
  let items =
    Cljml.Language_service.completions analysis
      ~offset:(String.length language_service_source)
  in
  let find label =
    List.find_opt
      (fun (item : Cljml.Language_service.completion_item) -> item.label = label)
      items
  in
  (match find "add-one" with
  | Some item when string_contains_substring item.detail "int -> int" -> ()
  | Some item -> failwith ("expected add-one type detail, got: " ^ item.detail)
  | None -> failwith "expected source completion add-one");
  if find "answer" = None then failwith "expected source completion answer"

let test_language_service_queries_outside_symbols_are_empty () =
  let analysis = analyze_language_service_source () in
  if Cljml.Language_service.hover analysis ~offset:0 <> None then
    failwith "expected no hover outside a symbol";
  if Cljml.Language_service.definition analysis ~offset:0 <> None then
    failwith "expected no definition outside a symbol"

let span_text source (span : Cljml.Ast.source_span) =
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
    Cljml.Language_service.analyze ~filename:"file:///tmp/references.cljml" source
    |> expect_ok
  in
  let top_level_usage = expect_substring_index source "value (use" in
  let references = Cljml.Language_service.references analysis ~offset:top_level_usage in
  let referenced_text = List.map (span_text source) references in
  if referenced_text <> [ "value"; "value" ] then
    failwith
      ("expected only top-level value definition/use, got: "
      ^ String.concat "," referenced_text)

let test_language_service_rename_returns_exact_symbol_edits () =
  let analysis = analyze_language_service_source () in
  let usage = expect_substring_index language_service_source "answer))" in
  match Cljml.Language_service.rename analysis ~offset:usage ~new_name:"total" with
  | Error err -> failwith ("expected rename edits, got: " ^ err.Cljml.Error.message)
  | Ok edits ->
      if List.length edits <> 2 then failwith "expected definition and usage edits";
      List.iter
        (fun (edit : Cljml.Language_service.text_edit) ->
          if edit.new_text <> "total" then failwith "expected rename replacement total";
          if span_text language_service_source edit.range <> "answer" then
            failwith "expected rename to edit only source symbol spans")
        edits;
      (match
         Cljml.Language_service.rename analysis ~offset:usage ~new_name:"bad name"
       with
      | Error _ -> ()
      | Ok _ -> failwith "expected invalid rename target to be rejected")

let test_language_service_document_symbols_preserve_source_names () =
  let analysis = analyze_language_service_source () in
  let symbols = Cljml.Language_service.document_symbols analysis in
  let find name =
    List.find_opt
      (fun (symbol : Cljml.Language_service.document_symbol) -> symbol.name = name)
      symbols
  in
  if find "answer" = None then failwith "expected answer document symbol";
  (match find "add-one" with
  | Some { kind = `Function; _ } -> ()
  | Some _ -> failwith "expected add-one function symbol"
  | None -> failwith "expected add-one document symbol");
  if find "result" = None then failwith "expected result document symbol"

let test_language_service_workspace_resolves_cross_file_identity () =
  let math =
    "(module Math (defn magnitude-plus-two [x] (+ x 2)))\n"
  in
  let main =
    "(def result (Math/magnitude-plus-two 40))\n"
  in
  let analyses =
    Cljml.Language_service.analyze_workspace
      [ ("file:///tmp/main.cljml", main); ("file:///tmp/math.cljml", math) ]
    |> expect_ok
  in
  let main_analysis = List.assoc "file:///tmp/main.cljml" analyses in
  let usage = expect_substring_index main "Math/magnitude-plus-two 40" in
  if Cljml.Language_service.value_uid_at main_analysis ~offset:usage = None then
    failwith "expected required workspace symbol to have a typed identity";
  match Cljml.Language_service.definition main_analysis ~offset:usage with
  | Some location
    when location.Location.loc_start.Lexing.pos_fname = "file:///tmp/math.cljml" ->
      ()
  | _ -> failwith "expected required workspace symbol definition in math.cljml"

let test_formatter_normalizes_whitespace () =
  Cljml.Formatter.format "(defn  add-one [ x ](+ x  1))"
  |> expect_ok
  |> assert_equal_string "(defn add-one [x] (+ x 1))\n"

let test_formatter_wraps_long_nested_forms () =
  let source =
    "(defn describe [person] (str (:name person) \":\" (:age person) \":\" (:admin? person) \":\" (:role person)))"
  in
  let expected =
    {|(defn
  describe
  [person]
  (str (:name person) ":" (:age person) ":" (:admin? person) ":" (:role person)))
|}
  in
  Cljml.Formatter.format source |> expect_ok |> assert_equal_string expected

let test_formatter_preserves_comments_strings_and_is_idempotent () =
  let source =
    "; before\n(def message \"[not ; syntax]\") ; after\n"
  in
  let expected =
    "; before\n(def message \"[not ; syntax]\")\n; after\n"
  in
  let formatted = Cljml.Formatter.format source |> expect_ok in
  assert_equal_string expected formatted;
  Cljml.Formatter.format formatted |> expect_ok |> assert_equal_string formatted

let test_formatter_rejects_unbalanced_delimiters () =
  Cljml.Formatter.format "(def answer 42]"
  |> expect_error_value "mismatched closing delimiter ]"

let test_match_delegates_opaque_module_constructor_payload_patterns_to_ocaml () =
  let source =
    {|
(module Msg
  (type-variant message Empty (Named :string)))
(def named (ocaml-construct Msg.Named "Ada"))
(def empty (ocaml-construct Msg.Empty))
(defn describe [^:ocaml/Msg.message message]
  (match message
    (Msg.Named name) name
    Msg.Empty "empty"))
(println (str (describe named) ":" (describe empty)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "match_delegates_opaque_module_constructor_payload_patterns_to_ocaml"
    "Ada:empty\n" ocaml_source

let test_match_delegates_unknown_opaque_constructor_errors_to_ocaml () =
  Cljml.Compiler.compile_string
    {|
(module Msg
  (type-variant message Empty (Named :string)))
(def named (ocaml-construct Msg.Named "Ada"))
(defn describe [^:ocaml/Msg.message message]
  (match message
    (Msg.Missing name) name
    Msg.Empty "empty"))
(def result (describe named))
|}
  |> expect_error_contains "Unbound constructor"

let test_match_supports_record_alias_or_and_guard_patterns () =
  let source =
    {|
(type-record user (name :string) (age :int))
(def user-value (ocaml-record user (name "Ada") (age 42)))
(def record-label
  (match user-value
    (record (name name) (age age)) (str name ":" age)))
(defn describe-option [^:ocaml/option<int> value]
  (match value
    (when (Some x) (> x 0)) (str "positive:" x)
    (or None (Some 0)) "empty"
    (as (Some x) _whole) (str "other:" x)))
(println
  (str record-label ":"
       (describe-option (ocaml-some 3)) ":"
       (describe-option (ocaml-some 0)) ":"
       (describe-option (ocaml-none)) ":"
       (describe-option (ocaml-some -2))))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "match_supports_record_alias_or_and_guard_patterns"
    "Ada:42:positive:3:empty:empty:other:-2\n" ocaml_source

let test_record_patterns_reject_unknown_and_duplicate_fields () =
  Cljml.Compiler.compile_string
    {|
(type-record user (name :string))
(def user-value (ocaml-record user (name "Ada")))
(def value (match user-value (record (missing x)) x))
|}
  |> expect_error_contains "unknown record pattern field missing";
  Cljml.Compiler.compile_string
    {|
(type-record user (name :string))
(def user-value (ocaml-record user (name "Ada")))
(def value (match user-value (record (name x) (name y)) x))
|}
  |> expect_error_contains "duplicate record pattern field name"

let test_record_patterns_require_record_targets () =
  Cljml.Compiler.compile_string
    {|(def value (match 42 (record (name x)) x))|}
  |> expect_error_contains "record pattern expects a record target"

let test_or_patterns_require_the_same_binders () =
  Cljml.Compiler.compile_string
    {|
(def value
  (match (ocaml-some 42)
    (or (Some x) None) x))
|}
  |> expect_error_contains "or-pattern alternatives must bind the same names"

let test_match_guards_must_be_boolean () =
  Cljml.Compiler.compile_string
    {|
(def value
  (match (ocaml-some 42)
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs
    "try_supports_normal_results_multiple_body_forms_and_handlers"
    "body\nhandled\nok:invalid:bad\n" ocaml_source

let test_try_and_raise_reject_malformed_forms () =
  Cljml.Compiler.compile_string {|(def value (try 42))|}
  |> expect_error "try requires at least one catch clause";
  Cljml.Compiler.compile_string {|(def value (try 42 (catch)))|}
  |> expect_error "catch requires a pattern and body";
  Cljml.Compiler.compile_string {|(def value (try (catch _ 42)))|}
  |> expect_error "try requires a body";
  Cljml.Compiler.compile_string {|(def value (raise))|}
  |> expect_error "raise expects 1 arguments";
  Cljml.Compiler.compile_string {|(def value (raise 1 2))|}
  |> expect_error "raise expects 1 arguments"

let test_try_rejects_branch_type_mismatch () =
  Cljml.Compiler.compile_string {|(def value (try 42 (catch _ "bad")))|}
  |> expect_error "try body and handlers must have the same type"

let test_raise_payload_is_checked_by_ocaml () =
  Cljml.Compiler.compile_string {|(def value (raise 42))|}
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_definitions_work" "44:Ada:hi Grace\n" ocaml_source

let test_module_definitions_reject_expressions () =
  Cljml.Compiler.compile_string
    {|
(module Math
  (println "side effect"))
|}
  |> expect_error
       "module forms must be module-signature, type-alias, type-record, type-variant, open, include, module-alias, defprotocol, extend-type, def, defn, or module"

let test_module_definitions_support_type_aliases () =
  let source =
    {|
(module UserIds
  (type-alias user-id :ocaml/int)
  (defn keep [^:ocaml/user_id x] x)
  (def answer (keep 42)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_definitions_support_type_aliases" "" ocaml_source

let test_module_definitions_support_variants () =
  let source =
    {|
(module Status
  (type-variant status Active Inactive)
  (def active (ocaml-construct Active)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_alias_exposes_values" "42\n" ocaml_source

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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_alias_targets_nested_modules" "hi Grace\n"
    ocaml_source

let test_module_alias_rejects_bad_forms () =
  Cljml.Compiler.compile_string {|(module-alias M)|}
  |> expect_error "module-alias expects alias and target modules"

let test_include_module_rejects_bad_forms () =
  Cljml.Compiler.compile_string {|(include)|}
  |> expect_error "include expects one module";
  Cljml.Compiler.compile_string {|(module App (include))|}
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_signatures_constrain_modules" "42\n" ocaml_source

let test_module_signature_ascription_is_checked_by_ocaml () =
  Cljml.Compiler.compile_string
    {|
(module-signature MathSig
  (val answer :ocaml/string))
(module Math MathSig
  (def answer 42))
|}
  |> expect_error_contains "string"

let test_module_signatures_support_type_items () =
  let source =
    {|
(module-signature UserSig
  (type user-id :ocaml/int)
  (val answer :ocaml/user_id))
(module User UserSig
  (type-alias user-id :ocaml/int)
  (def answer 42))
(defn keep [^:ocaml/User.user_id value] value)
(def saved (keep User/answer))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_signatures_support_type_items" "" ocaml_source

let test_module_signatures_support_parameterized_manifest_types () =
  let source =
    {|
(module-signature BoxSig
  (type box [a] :ocaml/option<param/a>)
  (val value :ocaml/box<int>))
(module Box BoxSig
  (type-alias box [a] :ocaml/option<param/a>)
  (def value (Some 42)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_signatures_support_parameterized_manifest_types" ""
    ocaml_source

let test_module_signatures_support_parameterized_abstract_types () =
  let source =
    {|
(module-signature BoxSig
  (type box [a]))
(module Box BoxSig
  (type-alias box [a] :ocaml/option<param/a>))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_signatures_support_nested_modules" "42\n" ocaml_source

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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "functor_parameters_expose_nested_signature_modules" "42\n"
    ocaml_source

let test_nested_module_signatures_are_checked_by_ocaml () =
  Cljml.Compiler.compile_string
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_signatures_include_other_signatures" "42\n"
    ocaml_source

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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "functor_parameters_expose_included_signature_values" "42\n"
    ocaml_source

let test_included_module_signatures_are_checked_by_ocaml () =
  Cljml.Compiler.compile_string
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
  Cljml.Compiler.compile_string
    {|
(module-signature ExtendedSig
  (include MissingSig))
|}
  |> expect_error_contains "Unbound module type"

let test_module_signature_type_items_are_checked_by_ocaml () =
  Cljml.Compiler.compile_string
    {|
(module-signature UserSig
  (type user-id :ocaml/string))
(module User UserSig
  (type-alias user-id :ocaml/int))
|}
  |> expect_error_contains "user_id"

let test_module_signatures_support_abstract_type_items () =
  let source =
    {|
(module-signature UserSig
  (type user-id)
  (val answer :ocaml/user_id))
(module User UserSig
  (type-alias user-id :ocaml/int)
  (def answer 42))
(defn keep [^:ocaml/User.user_id value] value)
(def saved (keep User/answer))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_signatures_support_abstract_type_items" ""
    ocaml_source

let test_module_signature_abstract_types_are_checked_by_ocaml () =
  Cljml.Compiler.compile_string
    {|
(module-signature UserSig
  (type user-id)
  (val answer :ocaml/user_id))
(module User UserSig
  (type-alias user-id :ocaml/int)
  (def answer 42))
(def bad (+ User/answer 1))
|}
  |> expect_error_contains "User.user_id"

let test_module_signatures_reject_bad_forms () =
  Cljml.Compiler.compile_string {|(module-signature MathSig)|}
  |> expect_error "module-signature expects at least one signature item";
  Cljml.Compiler.compile_string
    {|(module-signature MathSig (value answer :ocaml/int))|}
  |> expect_error
       "module-signature items must be val, type, module, or include declarations";
  Cljml.Compiler.compile_string
    {|(module-signature OuterSig (module Inner))|}
  |> expect_error
       "module-signature items must be val, type, module, or include declarations";
  Cljml.Compiler.compile_string
    {|(module-signature ExtendedSig (include))|}
  |> expect_error "module-signature include expects one module type";
  Cljml.Compiler.compile_string
    {|(module-signature MathSig (val answer :unknown))|}
  |> expect_error "unknown signature type :unknown";
  Cljml.Compiler.compile_string
    {|(module-signature MathSig (type user-id :unknown))|}
  |> expect_error "unknown signature type :unknown";
  Cljml.Compiler.compile_string
    {|(module-signature BoxSig (type box [a] :ocaml/option<param/b>))|}
  |> expect_error "unknown type parameter b";
  Cljml.Compiler.compile_string
    {|(module-signature BoxSig (type box [a a]))|}
  |> expect_error "duplicate type parameter a";
  Cljml.Compiler.compile_string
    {|(module-signature BoxSig (type box []))|}
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_functors_apply_multiple_modules" "42\n" ocaml_source

let test_multi_parameter_functor_application_is_checked_by_ocaml () =
  Cljml.Compiler.compile_string
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
(def ada (ocaml-record App.user (name "Ada") (age 41)))
(println (str (ocaml-field ada name) ":" (+ (ocaml-field ada age) 1)))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_functor_applications_expose_record_types"
    "Ada:42\n" ocaml_source

let test_module_functor_application_is_checked_by_ocaml () =
  Cljml.Compiler.compile_string
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
  Cljml.Compiler.compile_string {|(module-functor Make M MathSig)|}
  |> expect_error
       "module-functor expects a name, [parameter signature ...], and body";
  Cljml.Compiler.compile_string {|(module-functor Make [] (def answer 42))|}
  |> expect_error "module-functor parameter vector must not be empty";
  Cljml.Compiler.compile_string
    {|(module-functor Make [M MathSig N] (def answer 42))|}
  |> expect_error "module-functor parameters must be name/signature pairs";
  Cljml.Compiler.compile_string
    {|(module-functor Make [M :MathSig] (def answer 42))|}
  |> expect_error "module-functor parameters must be symbols";
  Cljml.Compiler.compile_string {|(module-apply App Make)|}
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_definitions_support_module_alias" "42:42\n"
    ocaml_source

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

let test_incremental_compilation_preserves_opened_modules () =
  let state = Cljml.Compiler.empty_state in
  let state, module_ocaml =
    Cljml.Compiler.compile_chunk state
      {|
(module Math
  (def answer 40)
  (defn add2 [x] (+ x 2)))
|}
    |> expect_ok
  in
  let _state, app_ocaml =
    Cljml.Compiler.compile_chunk state
      {|
(open Math)
(println (add2 answer))
|}
    |> expect_ok
  in
  assert_ocaml_runs "incremental_compilation_preserves_opened_modules" "42\n"
    (module_ocaml ^ "\n\n" ^ app_ocaml)

let test_incremental_compilation_preserves_module_aliases () =
  let state = Cljml.Compiler.empty_state in
  let state, module_ocaml =
    Cljml.Compiler.compile_chunk state
      {|
(module Math
  (def answer 40)
  (defn add2 [x] (+ x 2)))
|}
    |> expect_ok
  in
  let _state, app_ocaml =
    Cljml.Compiler.compile_chunk state
      {|
(module-alias M Math)
(println (M/add2 M/answer))
|}
    |> expect_ok
  in
  assert_ocaml_runs "incremental_compilation_preserves_module_aliases" "42\n"
    (module_ocaml ^ "\n\n" ^ app_ocaml)

let test_incremental_compilation_preserves_state () =
  let state = Cljml.Compiler.empty_state in
  let state, people_ocaml =
    Cljml.Compiler.compile_chunk state
      {|(module People (def user {:name "Ada", :age 36}))|}
    |> expect_ok
  in
  let _state, app_ocaml =
    Cljml.Compiler.compile_chunk state
      {|
(def updated (assoc People/user :admin? true))
(println (str (:name updated) ":" (:admin? updated) ":" (:age updated)))
|}
    |> expect_ok
  in
  assert_ocaml_runs "incremental_compilation_preserves_state"
    "Ada:true:36\n" (people_ocaml ^ "\n\n" ^ app_ocaml)

let test_incremental_compilation_preserves_record_sets () =
  let state = Cljml.Compiler.empty_state in
  let state, people_ocaml =
    Cljml.Compiler.compile_chunk state
      {|
(def ada {:name "Ada", :age 36})
|}
    |> expect_ok
  in
  let _state, app_ocaml =
    Cljml.Compiler.compile_chunk state
      {|
(def users (hash-set ada))
(println (str (count users) ":" (contains? users ada)))
|}
    |> expect_ok
  in
  assert_ocaml_runs "incremental_compilation_preserves_record_sets" "1:true\n"
    (people_ocaml ^ "\n\n" ^ app_ocaml)

let test_incremental_compilation_preserves_composite_sets () =
  let state = Cljml.Compiler.empty_state in
  let state, collections_ocaml =
    Cljml.Compiler.compile_chunk state
      {|
(def values (hash-set [1 2]))
|}
    |> expect_ok
  in
  let _state, app_ocaml =
    Cljml.Compiler.compile_chunk state
      {|
(def updated (conj values [2 3]))
(println (str (count updated) ":" (contains? updated [2 3])))
|}
    |> expect_ok
  in
  assert_ocaml_runs "incremental_compilation_preserves_composite_sets" "2:true\n"
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
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_definitions_support_record_sets" "1:true\n" ocaml_source

let test_module_definitions_support_composite_sets () =
  let source =
    {|
(module Groups
  (def values (hash-set (list 1 2))))
(println (str (count Groups/values) ":" (contains? Groups/values (list 1 2))))
|}
  in
  let ocaml_source = Cljml.Compiler.compile_string source |> expect_ok in
  assert_ocaml_runs "module_definitions_support_composite_sets" "1:true\n"
    ocaml_source

let test_incremental_compilation_requires_prior_state () =
  Cljml.Compiler.compile_chunk Cljml.Compiler.empty_state
    {|(println (:name People/user))|}
  |> expect_error_value "unknown symbol People/user"

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

let test_incremental_compile_chunk_runs_ocaml_typecheck_gate () =
  Cljml.Compiler.compile_chunk Cljml.Compiler.empty_state
    {|
(def answer (ocaml-call :int Stdlib.abs "bad"))
|}
  |> expect_error_contains "string"

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

let test_parsetree_backend_supports_record_sets () =
  let source =
    {|
(def ada {:name "Ada", :age 36})
(def users (hash-set ada))
(println (str (count users) ":" (contains? users ada)))
|}
  in
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_record_sets" "1:true\n" ocaml_source

let test_parsetree_backend_supports_composite_sets () =
  let source =
    {|
(def values (hash-set [1 2]))
(def updated (conj values [2 3]))
(println (str (count updated) ":" (contains? updated [2 3])))
|}
  in
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_composite_sets" "2:true\n"
    ocaml_source

let test_parsetree_backend_supports_type_aliases () =
  let source =
    {|
(type-alias user-id :ocaml/int)
(defn keep-user-id [^:ocaml/user_id x] x)
(def answer (keep-user-id 42))
|}
  in
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_type_aliases" "" ocaml_source

let test_parsetree_backend_supports_generic_ocaml_calls () =
  let source =
    {|
(def answer (ocaml-call :int Stdlib.abs -42))
(def label (ocaml-call :string String.uppercase_ascii "ada"))
(println (str label ":" answer))
|}
  in
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_generic_ocaml_calls"
    "ADA:42\n" ocaml_source

let test_parsetree_backend_supports_ocaml_option_and_result_constructors () =
  let source =
    {|
(def present (ocaml-some 42))
(def absent (ocaml-none))
(def success (ocaml-ok "Ada"))
(def failure (ocaml-error "bad"))
|}
  in
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_ocaml_option_and_result_constructors"
    "" ocaml_source

let test_parsetree_backend_supports_ocaml_option_and_result_patterns () =
  let source =
    {|
(def present (ocaml-some 41))
(def success (ocaml-ok "Ada"))
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
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
  assert_ocaml_runs
    "parsetree_backend_supports_ocaml_option_and_result_patterns" "42:Ada\n"
    ocaml_source

let test_parsetree_backend_supports_ocaml_type_application_annotations () =
  let source =
    {|
(def present (ocaml-some 41))
(def success (ocaml-ok "Ada"))
(defn option-score [^:ocaml/option<int> value]
  (match value
    (Some x) (+ x 1)
    None 0))
(defn result-label [^:ocaml/result<string;string> value]
  (match value
    (Ok name) name
    (Error message) message))
(println (str (option-score present) ":" (result-label success)))
|}
  in
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
  assert_ocaml_runs
    "parsetree_backend_supports_ocaml_type_application_annotations" "42:Ada\n"
    ocaml_source

let test_parsetree_backend_supports_ocaml_tuple_values () =
  let source =
    {|
(def pair (ocaml-tuple 41 "Ada"))
(defn describe [^:ocaml/tuple<int;string> value]
  (match value
    (ocaml-tuple id name) (str name ":" (+ id 1))))
(println (describe pair))
|}
  in
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_ocaml_tuple_values" "Ada:42\n"
    ocaml_source

let test_parsetree_backend_supports_ocaml_record_values () =
  let source =
    {|
(type-record user (name :string) (age :int))
(def ada (ocaml-record user (name "Ada") (age 41)))
(println (str (ocaml-field ada name) ":" (+ (ocaml-field ada age) 1)))
|}
  in
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_ocaml_record_values" "Ada:42\n"
    ocaml_source

let test_parsetree_backend_supports_variants () =
  let source =
    {|
(type-variant status Active Inactive)
(def active (ocaml-construct Active))
(defn keep-status [^:ocaml/status x] x)
(def saved (keep-status active))
|}
  in
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_variants" "" ocaml_source

let test_parsetree_backend_supports_payload_variants () =
  let source =
    {|
(type-variant message Ping (Named :string) (Pair :int :string))
(def named (ocaml-construct Named "Ada"))
(def pair (ocaml-construct Pair 42 "Ada"))
(defn describe [^:ocaml/message message]
  (match message
    (Named name) name
    (Pair id name) (str name ":" id)
    Ping "ping"))
(println (str (describe named) ":" (describe pair)))
|}
  in
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_payload_variants"
    "Ada:Ada:42\n" ocaml_source

let test_parsetree_backend_supports_ocaml_constructor_patterns () =
  let source =
    {|
(type-variant status Active Inactive)
(def active (ocaml-construct Active))
(def inactive (ocaml-construct Inactive))
(defn describe [^:ocaml/status status]
  (match status
    Active "active"
    Inactive "inactive"))
(println (str (describe active) ":" (describe inactive)))
|}
  in
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
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
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
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
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
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
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_module_alias" "42\n" ocaml_source

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
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
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
  let structure = Cljml.Compiler.compile_parsetree source |> expect_ok in
  let ocaml_source = Cljml.Compiler.print_parsetree structure in
  assert_ocaml_runs "parsetree_backend_supports_module_functors" "42\n"
    ocaml_source

let test_parsetree_backend_preserves_static_errors () =
  Cljml.Compiler.compile_parsetree {|(def x (+ 1 "two"))|}
  |> expect_error_value "expected int arguments for +"

let test_parsetree_backend_runs_ocaml_typecheck_gate () =
  Cljml.Compiler.compile_parsetree
    {|
(def answer (ocaml-call :int Stdlib.abs "bad"))
|}
  |> expect_error_contains "string"

let test_parsetree_backend_builds_native_record_items () =
  let structure =
    Cljml.Compiler.compile_parsetree {|(def user {:name "Ada", :age 36})|}
    |> expect_ok
  in
  match structure with
  | [ type_item; set_module_item; value_item ] -> (
      match (type_item.pstr_desc, set_module_item.pstr_desc, value_item.pstr_desc) with
      | Pstr_type _, Pstr_module _, Pstr_value _ -> ()
      | _ -> failwith "expected record type, comparator module, and value structure items")
  | _ -> failwith "expected record type, comparator module, and value structure items"

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
      | Pstr_value _, Pstr_value _ -> ()
      | _ -> failwith "expected definition and effect value structure items")
  | _ -> failwith "expected exactly two value structure items"

let test_parsetree_backend_builds_native_defn_items () =
  let structure =
    Cljml.Compiler.compile_parsetree
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
      | Pstr_value _ -> ()
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
          | Pmod_structure [ _value_item; _nested_item ] -> ()
          | _ -> failwith "expected value and nested module body items")
      | _ -> failwith "expected module structure item")
  | _ -> failwith "expected one module structure item"

let test_parsetree_backend_builds_native_module_alias_items () =
  let structure =
    Cljml.Compiler.compile_parsetree
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
    Cljml.Compiler.compile_parsetree
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
    Cljml.Compiler.compile_parsetree
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
            (functor_binding.pmb_expr.pmod_desc, apply_binding.pmb_expr.pmod_desc)
          with
          | Pmod_functor _, Pmod_apply _ -> ()
          | _ -> failwith "expected functor and application module expressions")
      | _ -> failwith "expected functor and application module items")
  | _ -> failwith "expected signature, module, functor, and application items"

let test_parsetree_backend_builds_native_scalar_expressions () =
  List.iter expect_structured_value_expression
    [ {|(def answer 42)|};
      {|(def result (boolean 1))|};
      {|(def result (integer? 1))|};
      {|(def result (bit-set 1 2))|};
      {|(def result (name :user/name))|};
      {|(def result (namespace :user/name))|};
      {|(def result (keyword "user" "name"))|};
      {|(def result (symbol :user :name))|} ]

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
      | Pexp_apply _, Pexp_construct _ -> ()
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
               match expression.pexp_desc with Pexp_ifthenelse _ -> true | _ -> false)
             expressions)
      then failwith "expected native conditional expressions with ghost locations"
  | _ -> failwith "expected three conditional value bindings"

let test_parsetree_backend_builds_native_function_expressions () =
  expect_structured_value_expression {|(defn identity-value [x] x)|}

let test_parsetree_backend_builds_native_sequence_expressions () =
  expect_structured_value_expression {|(def result (do 1 2 3))|}

let test_parsetree_backend_builds_native_sequence_navigation_expressions () =
  List.iter expect_structured_value_expression
    [ {|(def result (next (list 1 2)))|};
      {|(def result (next [1 2]))|};
      {|(def result (nthnext (list 1 2 3) 2))|};
      {|(def result (nthnext [1 2 3] 2))|};
      {|(def result (nthrest (list 1 2 3) 2))|};
      {|(def result (nthrest [1 2 3] 2))|};
      {|(def result (rseq [1 2]))|} ]

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
  expect_structured_value_expression
    {|(def result (< 1 2 3))|}

let test_parsetree_backend_builds_native_record_field_expressions () =
  expect_structured_value_expression
    {|(def user {:name "Ada", :age 36})(def age (get user :age))|}

let test_parsetree_backend_builds_native_boolean_expressions () =
  expect_structured_value_expression {|(def result (not false))|}

let test_parsetree_backend_builds_native_string_expressions () =
  expect_structured_value_expression {|(def result (subs "cljml" 1 4))|}

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
  expect_structured_value_expression {|(def names (keys {:name "Ada", :age 36}))|}

let test_parsetree_backend_builds_native_contains_expressions () =
  expect_structured_value_expression {|(def present? (contains? {:name "Ada"} :name))|}

let test_parsetree_backend_builds_native_set_constructor_expressions () =
  expect_structured_value_expression {|(def ids (hash-set 3 1 2))|}

let test_parsetree_backend_builds_native_sequence_transform_expressions () =
  List.iter expect_structured_value_expression
    [ {|(def result (sort [3 1 2]))|};
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
      {|(def result (dorun [1 2 3]))|} ]

let test_incremental_parsetree_backend_preserves_state () =
  let state = Cljml.Compiler.empty_state in
  let state, people_structure =
    Cljml.Compiler.compile_chunk_parsetree state
      {|(module People (def user {:name "Ada", :age 36}))|}
    |> expect_ok
  in
  let _state, app_structure =
    Cljml.Compiler.compile_chunk_parsetree state
      {|
(println (str (:name People/user) ":" (:age People/user)))
|}
    |> expect_ok
  in
  let people_ocaml = Cljml.Compiler.print_parsetree people_structure in
  let app_ocaml = Cljml.Compiler.print_parsetree app_structure in
  assert_ocaml_runs "incremental_parsetree_backend_preserves_state" "Ada:36\n"
    (people_ocaml ^ "\n\n" ^ app_ocaml)

let test_incremental_parsetree_backend_runs_ocaml_typecheck_gate () =
  Cljml.Compiler.compile_chunk_parsetree Cljml.Compiler.empty_state
    {|
(def answer (ocaml-call :int Stdlib.abs "bad"))
|}
  |> expect_error_contains "string"

let test_compile_string_prints_parsetree_backend_output () =
  let source =
    {|
(type-record user (name :string) (age :int))
(def user (ocaml-record user (name "Ada") (age 36)))
(println (str (ocaml-field user name) ":" (ocaml-field user age)))
|}
  in
  let source_output = Cljml.Compiler.compile_string source |> expect_ok in
  let parsetree_output =
    Cljml.Compiler.compile_parsetree source |> expect_ok
    |> Cljml.Compiler.print_parsetree
  in
  if source_output <> parsetree_output then
    failwith "compile_string should print the checked Parsetree backend output"

let test_compile_chunk_prints_parsetree_backend_output () =
  let source =
    {|
(module Greeter
  (defn shout [name] (ocaml-call :string String.uppercase_ascii name)))
|}
  in
  let _, source_output =
    Cljml.Compiler.compile_chunk Cljml.Compiler.empty_state source |> expect_ok
  in
  let _, structure =
    Cljml.Compiler.compile_chunk_parsetree Cljml.Compiler.empty_state source
    |> expect_ok
  in
  let parsetree_output = Cljml.Compiler.print_parsetree structure in
  if source_output <> parsetree_output then
    failwith "compile_chunk should print the checked Parsetree backend output"

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
    ("nil surface is not supported", test_nil_surface_is_not_supported);
    ("type predicates work", test_type_predicates);
    ("type predicates reject wrong arity", test_type_predicates_reject_wrong_arity);
    ("subs core api works", test_subs_core_api);
    ("subs rejects non-string sources", test_subs_rejects_non_string_sources);
    ("subs rejects non-int indexes", test_subs_rejects_non_int_indexes);
    ( "type relations are explicit and strict",
      test_type_relations_are_explicit_and_strict );
    ( "named records use nominal type identity",
      test_named_records_use_nominal_type_identity );
    ( "compiler identities are stable and distinct",
      test_compiler_identities_are_stable_and_distinct );
    ( "emitted OCaml names reject source collisions",
      test_emitted_ocaml_names_reject_source_collisions );
    ("modules resolve qualified symbols", test_modules_resolve_qualified_symbols);
    ( "modules prevent unqualified symbol collisions",
      test_modules_prevent_unqualified_symbol_collisions );
    ( "top-level require rejects cljml namespace imports",
      test_top_level_require_rejects_cljml_namespace_imports );
    ( "top-level require imports OCaml modules",
      test_top_level_require_imports_ocaml_modules );
    ("namespace form is removed", test_namespace_form_is_removed);
    ("ocaml keyword names are munged", test_ocaml_keyword_names_are_munged);
    ( "module aliases replace legacy import aliases",
      test_module_aliases_replace_legacy_import_aliases );
    ("open replaces namespace refer", test_open_replaces_required_refer);
    ("keyword lookup syntax works", test_keyword_lookup_syntax);
    ("typed empty vectors work", test_typed_empty_vectors);
    ("vector-of rejects unknown types", test_vector_of_rejects_unknown_types);
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
    ( "OCaml errors include cljml source locations",
      test_ocaml_errors_include_cljml_source_locations );
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
      test_parsetree_typecheck_gate_rejects_invalid_required_module_alias_calls );
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
      test_ocaml_option_and_result_constructors_compile_through_source_backend );
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
      test_ocaml_type_application_annotations_delegate_argument_mismatch_to_ocaml );
    ( "OCaml type application annotations reject bad forms",
      test_ocaml_type_application_annotations_reject_bad_forms );
    ( "OCaml tuple values compile through source backend",
      test_ocaml_tuple_values_compile_through_source_backend );
    ( "OCaml tuple values delegate argument mismatch to OCaml",
      test_ocaml_tuple_values_delegate_argument_mismatch_to_ocaml );
    ( "OCaml tuple values reject bad forms", test_ocaml_tuple_values_reject_bad_forms );
    ( "OCaml float and char literals compile",
      test_ocaml_float_and_char_literals_compile );
    ( "OCaml arrays support construction read and mutation",
      test_ocaml_arrays_support_construction_read_and_mutation );
    ( "OCaml refs support read and assignment",
      test_ocaml_refs_support_read_and_assignment );
    ( "OCaml arrays reject invalid operations",
      test_ocaml_arrays_reject_invalid_operations );
    ( "OCaml refs reject invalid operations",
      test_ocaml_refs_reject_invalid_operations );
    ( "OCaml float literals remain outside integer arithmetic",
      test_ocaml_float_literals_remain_outside_integer_arithmetic );
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
    ( "OCaml record values reject bad forms", test_ocaml_record_values_reject_bad_forms );
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
    ( "typed function parameters reject bad calls",
      test_typed_function_parameters_reject_bad_calls );
    ( "unit annotations reject non-unit arguments",
      test_unit_annotations_reject_non_unit_arguments );
    ( "typed function parameters reject bad bodies",
      test_typed_function_parameters_reject_bad_bodies );
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
    ( "static protocols work through module aliases",
      test_static_protocols_work_through_module_aliases );
    ( "ambiguous protocol methods require explicit identity",
      test_ambiguous_protocol_methods_require_explicit_identity );
    ( "protocols inside modules export methods and record implementations",
      test_protocols_inside_modules_export_methods_and_record_impls );
    ( "protocols support named record receivers",
      test_protocols_support_named_record_receivers );
    ( "protocol signatures check all parameter types",
      test_protocol_signatures_check_all_parameter_types );
    ( "protocol identity disambiguates same named methods",
      test_protocol_identity_disambiguates_same_named_methods );
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
    ( "clojure.string module batch works",
      test_clojure_string_module_batch_works );
    ( "clojure.string module refer works",
      test_clojure_string_module_refer_works );
    ( "clojure.string module rejects bad args",
      test_clojure_string_module_rejects_bad_args );
    ( "clojure.string module rejects unknown refer",
      test_clojure_string_module_rejects_unknown_refer );
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
    ("loop and recur are tail-recursive", test_loop_and_recur_are_tail_recursive);
    ( "loop and recur delegate OCaml-owned alias compatibility",
      test_loop_and_recur_delegate_ocaml_owned_alias_compatibility );
    ( "loop and recur delegate OCaml-owned mismatch to OCaml",
      test_loop_and_recur_delegate_ocaml_owned_mismatch_to_ocaml );
    ( "loop and recur reject invalid calls",
      test_loop_and_recur_reject_invalid_calls );
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
    ("sets reject nil elements", test_sets_reject_nil_elements);
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
    ("match supports OCaml constructor patterns", test_match_supports_ocaml_constructor_patterns);
    ( "compile diagnostics capture OCaml match warnings",
      test_compile_diagnostics_capture_ocaml_match_warnings );
    ( "compile diagnostics are empty for exhaustive matches",
      test_compile_diagnostics_are_empty_for_exhaustive_matches );
    ( "language service hover uses OCaml types",
      test_language_service_hover_uses_ocaml_types );
    ( "language service definition resolves source binding",
      test_language_service_definition_resolves_source_binding );
    ( "language service completion uses source names and types",
      test_language_service_completion_uses_source_names_and_types );
    ( "language service queries outside symbols are empty",
      test_language_service_queries_outside_symbols_are_empty );
    ( "language service references use typed identity",
      test_language_service_references_use_typed_identity );
    ( "language service rename returns exact symbol edits",
      test_language_service_rename_returns_exact_symbol_edits );
    ( "language service document symbols preserve source names",
      test_language_service_document_symbols_preserve_source_names );
    ( "language service workspace resolves cross-file identity",
      test_language_service_workspace_resolves_cross_file_identity );
    ( "formatter normalizes whitespace",
      test_formatter_normalizes_whitespace );
    ( "formatter wraps long nested forms",
      test_formatter_wraps_long_nested_forms );
    ( "formatter preserves comments strings and is idempotent",
      test_formatter_preserves_comments_strings_and_is_idempotent );
    ( "formatter rejects unbalanced delimiters",
      test_formatter_rejects_unbalanced_delimiters );
    ( "match delegates opaque module constructor payload patterns to OCaml",
      test_match_delegates_opaque_module_constructor_payload_patterns_to_ocaml );
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
    ( "match guards must be boolean",
      test_match_guards_must_be_boolean );
    ("try catches OCaml exceptions", test_try_catches_ocaml_exceptions);
    ( "try supports normal results multiple body forms and handlers",
      test_try_supports_normal_results_multiple_body_forms_and_handlers );
    ( "try and raise reject malformed forms",
      test_try_and_raise_reject_malformed_forms );
    ( "try rejects branch type mismatch",
      test_try_rejects_branch_type_mismatch );
    ( "raise payload is checked by OCaml",
      test_raise_payload_is_checked_by_ocaml );
    ("module definitions work", test_module_definitions_work);
    ("module definitions reject expressions", test_module_definitions_reject_expressions);
    ("module definitions support type aliases", test_module_definitions_support_type_aliases);
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
    ("module definitions support variants", test_module_definitions_support_variants);
    ( "incremental compilation requires prior state",
      test_incremental_compilation_requires_prior_state );
    ("module definitions support open", test_module_definitions_support_open);
    ("module definitions support include", test_module_definitions_support_include);
    ( "module definitions support module alias",
      test_module_definitions_support_module_alias );
    ("module alias exposes values", test_module_alias_exposes_values);
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
    ("parsetree backend supports variants", test_parsetree_backend_supports_variants);
    ( "parsetree backend supports payload variants",
      test_parsetree_backend_supports_payload_variants );
    ( "parsetree backend supports OCaml constructor patterns",
      test_parsetree_backend_supports_ocaml_constructor_patterns );
    ("parsetree backend supports open module", test_parsetree_backend_supports_open_module);
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
    ( "compile_chunk prints Parsetree backend output",
      test_compile_chunk_prints_parsetree_backend_output );
  ]

let () =
  List.iter
    (fun (name, run) ->
      try run ()
      with exn ->
        Printf.eprintf "FAILED: %s\n%s\n" name (Printexc.to_string exn);
        exit 1)
    tests
