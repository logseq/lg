(rule
 (targets
  datascript_conn_native_base.state
  datascript_conn_native_base_chunk.ml)
 (deps
  ../stdlib/lg_stdlib_native.state
  ../datascript/me/tonsky/persistent_sorted_set/arrays.cljc
  ../datascript/me/tonsky/persistent_sorted_set/protocol.cljc
  ../datascript/me/tonsky/persistent_sorted_set.mil
  ../datascript/me/tonsky/persistent_sorted_set.cljc
  datascript/lg/annotations.mil
  datascript/upstream/util.cljc
  datascript/upstream/lru.cljc
  datascript/upstream/schema.cljc
  datascript/upstream/db.cljc
  datascript/upstream/parser.cljc
  datascript/upstream/entity.cljc
  datascript/lg/built_ins.mil
  datascript/upstream/built_ins.cljc
  datascript/upstream/pull_parser.cljc
  datascript/upstream/pull_api.cljc
  datascript/lg/datafy.mil
  datascript/lg/datafy.cljc
  datascript/lg/query_types.mil
  datascript/lg/query_types.cljc
  datascript/lg/query.mil
  datascript/lg/query.cljc
  datascript/lg/query_v3.mil
  datascript/lg/query_v3.cljc
  datascript/upstream/storage.cljc
  datascript/upstream/storage_file.cljc
  datascript/upstream/serialize.cljc
  datascript/upstream/conn.cljc
  datascript/upstream/core.cljc
  datascript/lg/js.cljc
  ../test_runner/clojure/test.cljc
  ../test_runner/clojure/test_native.cljc
  datascript/upstream_test_support/datascript/test/core.cljc
  datascript/upstream_test_support/datascript/test/storage_support.cljc
  datascript/benchmark/datascript/bench/bench.cljc
  datascript/benchmark/datascript/bench/datascript.cljc
  %{lib:datascript.runtime:datascript_runtime.cmi}
  %{lib:datascript.runtime:datascript_runtime__Data_value.cmi}
  %{lib:datascript.runtime:datascript_runtime__Query_value.cmi}
  %{lib:datascript.runtime:datascript_runtime__Serialization_value.cmi}
  %{lib:datascript.runtime:datascript_runtime__Storage_backend.cmi}
  %{lib:datascript.runtime:datascript_runtime__Storage_value.cmi}
  %{lib:lg-test.runtime:lg_test_runtime.cmi}
  %{lib:lg-test.alcotest:lg_test_alcotest.cmi}
  ../bin/lg_cli.exe)
 (action
  (run
   %{exe:../bin/lg_cli.exe}
   --target
   native
   --compile-files-from-state
   %{dep:../stdlib/lg_stdlib_native.state}
   datascript_conn_native_base.state
   %{dep:../datascript/me/tonsky/persistent_sorted_set/arrays.cljc}
   %{dep:../datascript/me/tonsky/persistent_sorted_set/protocol.cljc}
   %{dep:../datascript/me/tonsky/persistent_sorted_set.mil}
   %{dep:../datascript/me/tonsky/persistent_sorted_set.cljc}
   %{dep:datascript/lg/annotations.mil}
   %{dep:datascript/upstream/util.cljc}
   %{dep:datascript/upstream/lru.cljc}
   %{dep:datascript/upstream/schema.cljc}
   %{dep:datascript/upstream/db.cljc}
   %{dep:datascript/upstream/parser.cljc}
   %{dep:datascript/upstream/entity.cljc}
   %{dep:datascript/lg/built_ins.mil}
   %{dep:datascript/upstream/built_ins.cljc}
   %{dep:datascript/upstream/pull_parser.cljc}
   %{dep:datascript/upstream/pull_api.cljc}
   %{dep:datascript/lg/datafy.mil}
   %{dep:datascript/lg/datafy.cljc}
   %{dep:datascript/lg/query_types.mil}
   %{dep:datascript/lg/query_types.cljc}
   %{dep:datascript/lg/query.mil}
   %{dep:datascript/lg/query.cljc}
   %{dep:datascript/lg/query_v3.mil}
   %{dep:datascript/lg/query_v3.cljc}
   %{dep:datascript/upstream/storage.cljc}
   %{dep:datascript/upstream/storage_file.cljc}
   %{dep:datascript/upstream/serialize.cljc}
   %{dep:datascript/upstream/conn.cljc}
   %{dep:datascript/upstream/core.cljc}
   %{dep:datascript/lg/js.cljc}
   %{dep:../test_runner/clojure/test.cljc}
   %{dep:../test_runner/clojure/test_native.cljc}
   %{dep:datascript/upstream_test_support/datascript/test/core.cljc}
   %{dep:datascript/upstream_test_support/datascript/test/storage_support.cljc}
   %{dep:datascript/benchmark/datascript/bench/bench.cljc}
   %{dep:datascript/benchmark/datascript/bench/datascript.cljc}
   -o
   datascript_conn_native_base_chunk.ml)))

(rule
 (target datascript_conn_native_base.ml)
 (deps
  ../stdlib/lg_stdlib_native.ml
  datascript_conn_native_base_chunk.ml)
 (action
  (with-stdout-to
   %{target}
   (progn
    (cat %{dep:../stdlib/lg_stdlib_native.ml})
    (cat %{dep:datascript_conn_native_base_chunk.ml})))))

(rule
 (targets
  datascript_conn_melange_base.state
  datascript_conn_melange_base_chunk.ml)
 (deps
  ../stdlib/lg_stdlib_melange.state
  ../datascript/me/tonsky/persistent_sorted_set/arrays.cljc
  ../datascript/me/tonsky/persistent_sorted_set/protocol.cljc
  ../datascript/me/tonsky/persistent_sorted_set.mil
  ../datascript/me/tonsky/persistent_sorted_set.cljc
  datascript/lg/annotations.mil
  datascript/upstream/util.cljc
  datascript/upstream/lru.cljc
  datascript/upstream/schema.cljc
  datascript/upstream/db.cljc
  datascript/upstream/parser.cljc
  datascript/upstream/entity.cljc
  datascript/lg/built_ins.mil
  datascript/upstream/built_ins.cljc
  datascript/upstream/pull_parser.cljc
  datascript/upstream/pull_api.cljc
  datascript/lg/datafy.mil
  datascript/lg/datafy.cljc
  datascript/lg/query_types.mil
  datascript/lg/query_types.cljc
  datascript/lg/query.mil
  datascript/lg/query.cljc
  datascript/lg/query_v3.mil
  datascript/lg/query_v3.cljc
  datascript/upstream/storage.cljc
  datascript/upstream/serialize.cljc
  datascript/upstream/conn.cljc
  datascript/upstream/core.cljc
  datascript/lg/js.cljc
  ../test_runner/clojure/test.cljc
  ../test_runner/clojure/test_melange.cljc
  datascript/upstream_test_support/datascript/test/cljs.cljc
  datascript/upstream_test_support/datascript/test/core.cljc
  datascript/upstream_test_support/datascript/test/storage_support.cljc
  datascript/benchmark/datascript/bench/bench.cljc
  datascript/benchmark/datascript/bench/datascript.cljc
  %{lib:datascript.runtime:datascript_runtime__Serialization_value.cmi}
  (package lg-test)
  ../bin/lg_cli.exe)
 (action
  (run
   %{exe:../bin/lg_cli.exe}
   --target
   melange
   --compile-files-from-state
   %{dep:../stdlib/lg_stdlib_melange.state}
   datascript_conn_melange_base.state
   %{dep:../datascript/me/tonsky/persistent_sorted_set/arrays.cljc}
   %{dep:../datascript/me/tonsky/persistent_sorted_set/protocol.cljc}
   %{dep:../datascript/me/tonsky/persistent_sorted_set.mil}
   %{dep:../datascript/me/tonsky/persistent_sorted_set.cljc}
   %{dep:datascript/lg/annotations.mil}
   %{dep:datascript/upstream/util.cljc}
   %{dep:datascript/upstream/lru.cljc}
   %{dep:datascript/upstream/schema.cljc}
   %{dep:datascript/upstream/db.cljc}
   %{dep:datascript/upstream/parser.cljc}
   %{dep:datascript/upstream/entity.cljc}
   %{dep:datascript/lg/built_ins.mil}
   %{dep:datascript/upstream/built_ins.cljc}
   %{dep:datascript/upstream/pull_parser.cljc}
   %{dep:datascript/upstream/pull_api.cljc}
   %{dep:datascript/lg/datafy.mil}
   %{dep:datascript/lg/datafy.cljc}
   %{dep:datascript/lg/query_types.mil}
   %{dep:datascript/lg/query_types.cljc}
   %{dep:datascript/lg/query.mil}
   %{dep:datascript/lg/query.cljc}
   %{dep:datascript/lg/query_v3.mil}
   %{dep:datascript/lg/query_v3.cljc}
   %{dep:datascript/upstream/storage.cljc}
   %{dep:datascript/upstream/serialize.cljc}
   %{dep:datascript/upstream/conn.cljc}
   %{dep:datascript/upstream/core.cljc}
   %{dep:datascript/lg/js.cljc}
   %{dep:../test_runner/clojure/test.cljc}
   %{dep:../test_runner/clojure/test_melange.cljc}
   %{dep:datascript/upstream_test_support/datascript/test/cljs.cljc}
   %{dep:datascript/upstream_test_support/datascript/test/core.cljc}
   %{dep:datascript/upstream_test_support/datascript/test/storage_support.cljc}
   %{dep:datascript/benchmark/datascript/bench/bench.cljc}
   %{dep:datascript/benchmark/datascript/bench/datascript.cljc}
   -o
   datascript_conn_melange_base_chunk.ml)))

(rule
 (target datascript_conn_melange_base.ml)
 (deps
  ../stdlib/lg_stdlib_melange.ml
  datascript_conn_melange_base_chunk.ml)
 (action
  (with-stdout-to
   %{target}
   (progn
    (cat %{dep:../stdlib/lg_stdlib_melange.ml})
    (cat %{dep:datascript_conn_melange_base_chunk.ml})))))

(rule
 (target datascript_conn_native_test_lru.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/lru.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_transact.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/transact.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_transact_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_transact.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_conn_native_test_transact_retract_static.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input datascript/upstream_tests/datascript/test/transact_retract_static.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_conn_native_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_transact_retract_static_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_transact_retract_static.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_conn_native_test_conn.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/conn.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_conn_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_conn.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_conn_native_test_listen.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/listen.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_listen_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_listen.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_conn_native_test_filter.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/filter_static.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_filter_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_filter.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_pull_api_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_pull_api.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_pull_parser_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_pull_parser.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_serialize_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_serialize.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_find_specs_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_query_find_specs.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_query.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_aggregates_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_query_aggregates.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_fns_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_query_fns.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_not_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_query_not.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_or_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_query_or.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_pull_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_query_pull.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_rules_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_query_rules.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_parser_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_parser.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_parser_rules_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_parser_rules.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_parser_where_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_parser_where.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_parser_find_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_parser_find.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_parser_query_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_parser_query.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_parser_return_map_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_parser_return_map.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_validation_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_validation.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_upsert_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_upsert.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_return_map_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_query_return_map.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_conn_native_test_ident.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/ident.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_components.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/components.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (targets datascript_conn_native_test_core.ml datascript_conn_native_test_core.state)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/core.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to datascript_conn_native_test_core.ml (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-state %{state} datascript_conn_native_test_core.state %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_db.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/db.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_entity.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/entity.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_explode.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/explode.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_index.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/index.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_issues.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/issues.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_lookup_refs.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/lookup_refs.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_parser.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/parser.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_parser_find.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/parser_find.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_parser_query.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/parser_query.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_parser_return_map.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/parser_return_map.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_parser_rules.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/parser_rules.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_parser_where.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/parser_where.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_pull_api.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/pull_api.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_pull_parser.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/pull_parser.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_query.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/query.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_query_aggregates.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/query_aggregates.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_query_find_specs.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/query_find_specs.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_query_fns.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/query_fns.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_query_not.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/query_not.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_query_or.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/query_or.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_query_pull.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/query_pull.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_query_return_map.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/query_return_map.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_query_rules.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/query_rules.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_serialize.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/serialize.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_storage.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/storage.cljs) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_tuples.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/tuples.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_upsert.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/upsert.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_validation.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/validation.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native.ml)
 (deps (:state datascript_conn_native_base.state) (:input ../test_runner/clojure/run.cljc) ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_conn_native_base\nlet () =\n  ignore Datascript_conn_native_test_lru.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_transact.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_conn.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_listen.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_filter.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_ident.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_components.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_core.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_db.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_entity.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_explode.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_index.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_issues.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_lookup_refs.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_parser.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_parser_find.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_parser_query.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_parser_return_map.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_parser_rules.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_parser_where.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_pull_api.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_pull_parser.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_query.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_query_aggregates.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_query_find_specs.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_query_fns.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_query_not.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_query_or.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_query_pull.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_query_return_map.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_query_rules.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_serialize.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_storage.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_tuples.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_upsert.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_validation.__lg_test_module_loaded\n")
    (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input})))))

(rule
 (target datascript_conn_melange_test_lru.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/lru.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_transact.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/transact.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_conn.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/conn.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_listen.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/listen.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_filter.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/filter_static.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_ident.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/ident.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_components.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/components.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (targets datascript_conn_melange_test_core.ml datascript_conn_melange_test_core.state)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/core.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to datascript_conn_melange_test_core.ml (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-state %{state} datascript_conn_melange_test_core.state %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_db.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/db.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_entity.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/entity.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_explode.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/explode.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_index.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/index.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_issues.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/issues.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_lookup_refs.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/lookup_refs.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_parser.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/parser.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_parser_find.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/parser_find.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_parser_query.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/parser_query.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_parser_return_map.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/parser_return_map.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_parser_rules.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/parser_rules.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_parser_where.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/parser_where.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_pull_api.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/pull_api.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_pull_parser.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/pull_parser.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_query.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/query.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_query_aggregates.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/query_aggregates.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_query_find_specs.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/query_find_specs.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_query_fns.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/query_fns.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_query_not.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/query_not.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_query_or.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/query_or.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_query_pull.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/query_pull.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_query_return_map.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/query_return_map.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_query_rules.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/query_rules.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_serialize.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/serialize.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_storage.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/storage.cljs) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_tuples.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/tuples.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_upsert.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/upsert.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_validation.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/validation.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange.ml)
 (deps (:state datascript_conn_melange_base.state) (:input ../test_runner/clojure/run.cljc) ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_conn_melange_base\nlet () =\n  ignore Datascript_conn_melange_test_lru.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_transact.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_conn.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_listen.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_filter.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_ident.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_components.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_core.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_db.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_entity.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_explode.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_index.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_issues.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_lookup_refs.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_parser.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_parser_find.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_parser_query.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_parser_return_map.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_parser_rules.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_parser_where.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_pull_api.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_pull_parser.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_query.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_query_aggregates.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_query_find_specs.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_query_fns.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_query_not.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_query_or.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_query_pull.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_query_return_map.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_query_rules.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_serialize.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_storage.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_tuples.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_upsert.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_validation.__lg_test_module_loaded\n")
      (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input})))))

(rule
 (target datascript_pull_melange_test_pull_api.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/pull_api.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_pull_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_pull_melange_test_pull_parser.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/pull_parser.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_pull_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_pull_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_pull_melange_base\nlet () =\n  ignore Datascript_pull_melange_test_pull_api.__lg_test_module_loaded;\n  ignore Datascript_pull_melange_test_pull_parser.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_serialize_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/serialize.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_serialize_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_serialize_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_serialize_melange_base\nlet () = ignore Datascript_serialize_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_find_specs_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/query_find_specs.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_query_find_specs_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_query_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/query.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_query_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_query_aggregates_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/query_aggregates.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_query_aggregates_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_query_aggregates_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_query_aggregates_melange_base\nlet () = ignore Datascript_query_aggregates_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_fns_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/query_fns.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_query_fns_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_query_fns_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_query_fns_melange_base\nlet () = ignore Datascript_query_fns_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_not_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/query_not.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_query_not_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_query_not_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_query_not_melange_base\nlet () = ignore Datascript_query_not_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_or_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/query_or.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_query_or_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_query_or_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_query_or_melange_base\nlet () = ignore Datascript_query_or_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_pull_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/query_pull.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_query_pull_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_query_pull_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_query_pull_melange_base\nlet () = ignore Datascript_query_pull_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_rules_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/query_rules.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_query_rules_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_query_rules_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_query_rules_melange_base\nlet () = ignore Datascript_query_rules_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_parser_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/parser.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_parser_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_parser_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_parser_melange_base\nlet () = ignore Datascript_parser_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_parser_rules_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/parser_rules.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_parser_rules_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_parser_rules_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_parser_rules_melange_base\nlet () = ignore Datascript_parser_rules_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_parser_where_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/parser_where.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_parser_where_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_parser_where_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_parser_where_melange_base\nlet () = ignore Datascript_parser_where_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_parser_find_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/parser_find.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_parser_find_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_parser_find_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_parser_find_melange_base\nlet () = ignore Datascript_parser_find_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_parser_query_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/parser_query.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_parser_query_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_parser_query_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_parser_query_melange_base\nlet () = ignore Datascript_parser_query_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_parser_return_map_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/parser_return_map.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_parser_return_map_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_parser_return_map_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_parser_return_map_melange_base\nlet () = ignore Datascript_parser_return_map_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_validation_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/validation.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_validation_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_validation_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_validation_melange_base\nlet () = ignore Datascript_validation_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_upsert_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/upsert.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_upsert_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_upsert_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_upsert_melange_base\nlet () = ignore Datascript_upsert_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_storage_native_test.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input datascript/upstream_tests/datascript/test/storage.clj)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_conn_native_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_storage_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_storage_native_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_storage_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/storage.cljs)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_storage_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_storage_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_storage_melange_base\nlet () = ignore Datascript_storage_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_tuples_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_tuples.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_tuples_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/tuples.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_tuples_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_tuples_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_tuples_melange_base\nlet () = ignore Datascript_tuples_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_query_melange_base\nlet () = ignore Datascript_query_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_find_specs_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_query_find_specs_melange_base\nlet () = ignore Datascript_query_find_specs_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_query_return_map_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/query_return_map.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_query_return_map_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_query_return_map_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_query_return_map_melange_base\nlet () = ignore Datascript_query_return_map_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (targets datascript_benchmark_native_support.ml datascript_benchmark_native_support.state)
 (deps
  (:state datascript_conn_native_base.state)
  ../bin/lg_cli.exe)
 (action
  (progn
   (copy %{state} datascript_benchmark_native_support.state)
   (with-stdout-to
    datascript_benchmark_native_support.ml
    (echo "let () = ()\n")))))

(rule
 (targets datascript_benchmark_native_impl.ml datascript_benchmark_native_impl.state)
 (deps
  (:state datascript_benchmark_native_support.state)
  ../bin/lg_cli.exe)
 (action
  (progn
   (copy %{state} datascript_benchmark_native_impl.state)
   (with-stdout-to
    datascript_benchmark_native_impl.ml
    (echo "include Datascript_conn_native_base\n")))))

(rule
 (target datascript_benchmark_native.ml)
 (deps
  (:state datascript_benchmark_native_impl.state)
  (:input datascript/benchmark/datascript/bench/runner.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_benchmark_native_impl\n")
    (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input})))))

(executable
 (name datascript_benchmark_native)
 (modes native)
 (modules
  datascript_benchmark_native_support
 datascript_benchmark_native_impl
 datascript_benchmark_native)
 (flags (:standard -w -5-11-26-27-32-37-39-69))
 (libraries datascript_conn_native_base_lib))

(rule
 (alias datascript-benchmark-scaling)
 (deps
  datascript_benchmark_scaling_test.sh
  datascript_benchmark_native.exe)
 (action
  (run
   bash
   %{dep:datascript_benchmark_scaling_test.sh}
   %{exe:datascript_benchmark_native.exe})))

(rule
 (alias datascript-benchmark-runtime-scaling)
 (deps
  datascript_benchmark_runtime_scaling_test.sh
  datascript_benchmark_native.exe
  (alias datascript-conn-melange)
  datascript-conn-melange/test/datascript_benchmark_melange.js)
 (action
  (run
   bash
   %{dep:datascript_benchmark_runtime_scaling_test.sh}
   %{exe:datascript_benchmark_native.exe}
   %{dep:datascript-conn-melange/test/datascript_benchmark_melange.js})))

(rule
 (alias datascript-serialization-full-scale-performance)
 (deps
  datascript_serialization_full_scale_performance_gate_test.sh
  datascript_benchmark_native.exe
  (alias datascript-conn-melange)
  datascript-conn-melange/test/datascript_benchmark_melange.js)
 (action
  (run
   bash
   %{dep:datascript_serialization_full_scale_performance_gate_test.sh}
   %{exe:datascript_benchmark_native.exe}
   %{dep:datascript-conn-melange/test/datascript_benchmark_melange.js})))

(rule
 (alias datascript-benchmark-surface)
 (deps
  ../script/check_datascript_benchmark_surface_test.sh
  datascript_benchmark_native.exe
  (alias datascript-conn-melange)
  datascript-conn-melange/test/datascript_benchmark_melange.js)
 (action
  (run
   bash
   %{dep:../script/check_datascript_benchmark_surface_test.sh})))

(rule
 (targets datascript_benchmark_melange_support.ml datascript_benchmark_melange_support.state)
 (deps
  (:state datascript_conn_melange_base.state)
  ../bin/lg_cli.exe)
 (action
  (progn
   (copy %{state} datascript_benchmark_melange_support.state)
   (with-stdout-to
    datascript_benchmark_melange_support.ml
    (echo "let () = ()\n")))))

(rule
 (targets datascript_benchmark_melange_impl.ml datascript_benchmark_melange_impl.state)
 (deps
  (:state datascript_benchmark_melange_support.state)
  ../bin/lg_cli.exe)
 (action
  (progn
   (copy %{state} datascript_benchmark_melange_impl.state)
   (with-stdout-to
    datascript_benchmark_melange_impl.ml
    (echo "include Datascript_conn_melange_base\n")))))

(rule
 (target datascript_benchmark_melange.ml)
 (deps
  (:state datascript_benchmark_melange_impl.state)
  (:input datascript/benchmark/datascript/bench/runner.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_benchmark_melange_impl\n")
    (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input})))))

(rule
 (target datascript_ident_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_ident.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_ident_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/ident.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_ident_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_ident_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_ident_melange_base\nlet () = ignore Datascript_ident_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_components_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_components.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_components_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/components.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_components_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_components_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_components_melange_base\nlet () = ignore Datascript_components_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_explode_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_explode.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_explode_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/explode.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_explode_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_explode_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_explode_melange_base\nlet () = ignore Datascript_explode_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_index_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_index.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_index_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/index.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_index_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_index_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_index_melange_base\nlet () = ignore Datascript_index_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_entity_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_entity.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_entity_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/entity.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_entity_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_entity_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_entity_melange_base\nlet () = ignore Datascript_entity_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_issues_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_issues.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_issues_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/issues.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_issues_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_issues_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_issues_melange_base\nlet () = ignore Datascript_issues_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_db_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_db.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_db_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/db.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_db_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_db_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_db_melange_base\nlet () = ignore Datascript_db_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_core_native_runner.ml)
 (deps
  (:state datascript_conn_native_test_core.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_core.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_core_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/core.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_core_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_core_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_core_melange_base\nlet () = ignore Datascript_core_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_lookup_refs_native_runner.ml)
 (deps
  (:state datascript_conn_native_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_conn_native_base\nlet () = ignore Datascript_conn_native_test_lookup_refs.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     native
     --compile-chunk-from
     %{state}
     %{input})))))

(rule
 (target datascript_lookup_refs_melange_test.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input datascript/upstream_tests/datascript/test/lookup_refs.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_lookup_refs_melange_base\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})
    (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_lookup_refs_melange_runner.ml)
 (deps
  (:state datascript_conn_melange_base.state)
  (:input ../test_runner/clojure/run.cljc)
  ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo
     "open Datascript_lookup_refs_melange_base\nlet () = ignore Datascript_lookup_refs_melange_test.__lg_test_module_loaded\n")
    (run
     %{exe:../bin/lg_cli.exe}
     --target
     melange
     --compile-chunk-from
     %{state}
     %{input})))))
