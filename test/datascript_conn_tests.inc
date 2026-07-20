(rule
 (targets datascript_conn_native_base.ml datascript_conn_native_base.state)
 (deps
  ../datascript/me/tonsky/persistent_sorted_set/arrays.cljc
  ../datascript/me/tonsky/persistent_sorted_set/protocol.cljc
  ../datascript/me/tonsky/persistent_sorted_set.cljc
  datascript/upstream/inline.cljc
  datascript/upstream/util.cljc
  datascript/upstream/lru.cljc
  datascript/upstream/schema.cljc
  datascript/upstream/db.cljc
  datascript/upstream/parser.cljc
  datascript/upstream/entity.cljc
  datascript/upstream/built_ins.cljc
  datascript/upstream/pull_parser.cljc
  datascript/upstream/pull_api.cljc
  datascript/upstream/query.cljc
  datascript/upstream/storage.cljc
  datascript/upstream/storage_file.cljc
  datascript/upstream/serialize.cljc
  datascript/upstream/conn.cljc
  datascript/upstream/core.cljc
  ../test_runner/clojure/test.cljc
  ../test_runner/clojure/test_native.cljc
  datascript/upstream_test_support/cognitect/transit.cljc
  datascript/upstream_test_support/datascript/test/core.cljc
  (package lg-test)
  ../test_runner/runtime/lg_test_runtime.cma
  ../test_runner/alcotest/lg_test_alcotest.cma
  ../bin/lg_cli.exe)
 (action
  (run
   %{exe:../bin/lg_cli.exe}
   --target
   native
   --compile-files-state
   datascript_conn_native_base.state
   %{dep:../datascript/me/tonsky/persistent_sorted_set/arrays.cljc}
   %{dep:../datascript/me/tonsky/persistent_sorted_set/protocol.cljc}
   %{dep:../datascript/me/tonsky/persistent_sorted_set.cljc}
   %{dep:datascript/upstream/inline.cljc}
   %{dep:datascript/upstream/util.cljc}
   %{dep:datascript/upstream/lru.cljc}
   %{dep:datascript/upstream/schema.cljc}
   %{dep:datascript/upstream/db.cljc}
   %{dep:datascript/upstream/parser.cljc}
   %{dep:datascript/upstream/entity.cljc}
   %{dep:datascript/upstream/built_ins.cljc}
   %{dep:datascript/upstream/pull_parser.cljc}
   %{dep:datascript/upstream/pull_api.cljc}
   %{dep:datascript/upstream/query.cljc}
   %{dep:datascript/upstream/storage.cljc}
   %{dep:datascript/upstream/storage_file.cljc}
   %{dep:datascript/upstream/serialize.cljc}
   %{dep:datascript/upstream/conn.cljc}
   %{dep:datascript/upstream/core.cljc}
   %{dep:../test_runner/clojure/test.cljc}
   %{dep:../test_runner/clojure/test_native.cljc}
   %{dep:datascript/upstream_test_support/cognitect/transit.cljc}
   %{dep:datascript/upstream_test_support/datascript/test/core.cljc}
   -o
   datascript_conn_native_base.ml)))

(rule
 (targets datascript_conn_melange_base.ml datascript_conn_melange_base.state)
 (deps
  ../datascript/me/tonsky/persistent_sorted_set/arrays.cljc
  ../datascript/me/tonsky/persistent_sorted_set/protocol.cljc
  ../datascript/me/tonsky/persistent_sorted_set.cljc
  datascript/upstream/inline.cljc
  datascript/upstream/util.cljc
  datascript/upstream/lru.cljc
  datascript/upstream/schema.cljc
  datascript/upstream/db.cljc
  datascript/upstream/parser.cljc
  datascript/upstream/entity.cljc
  datascript/upstream/built_ins.cljc
  datascript/upstream/pull_parser.cljc
  datascript/upstream/pull_api.cljc
  datascript/upstream/query.cljc
  datascript/upstream/storage.cljc
  datascript/upstream/serialize.cljc
  datascript/upstream/conn.cljc
  datascript/upstream/core.cljc
  ../test_runner/clojure/test.cljc
  ../test_runner/clojure/test_melange.cljc
  datascript/upstream_test_support/cognitect/transit.cljc
  datascript/upstream_test_support/datascript/test/cljs.cljc
  datascript/upstream_test_support/datascript/test/core.cljc
  (package lg-test)
  ../test_runner/runtime/lg_test_runtime.cma
  ../bin/lg_cli.exe)
 (action
  (run
   %{exe:../bin/lg_cli.exe}
   --target
   melange
   --compile-files-state
   datascript_conn_melange_base.state
   %{dep:../datascript/me/tonsky/persistent_sorted_set/arrays.cljc}
   %{dep:../datascript/me/tonsky/persistent_sorted_set/protocol.cljc}
   %{dep:../datascript/me/tonsky/persistent_sorted_set.cljc}
   %{dep:datascript/upstream/inline.cljc}
   %{dep:datascript/upstream/util.cljc}
   %{dep:datascript/upstream/lru.cljc}
   %{dep:datascript/upstream/schema.cljc}
   %{dep:datascript/upstream/db.cljc}
   %{dep:datascript/upstream/parser.cljc}
   %{dep:datascript/upstream/entity.cljc}
   %{dep:datascript/upstream/built_ins.cljc}
   %{dep:datascript/upstream/pull_parser.cljc}
   %{dep:datascript/upstream/pull_api.cljc}
   %{dep:datascript/upstream/query.cljc}
   %{dep:datascript/upstream/storage.cljc}
   %{dep:datascript/upstream/serialize.cljc}
   %{dep:datascript/upstream/conn.cljc}
   %{dep:datascript/upstream/core.cljc}
   %{dep:../test_runner/clojure/test.cljc}
   %{dep:../test_runner/clojure/test_melange.cljc}
   %{dep:datascript/upstream_test_support/cognitect/transit.cljc}
   %{dep:datascript/upstream_test_support/datascript/test/cljs.cljc}
   %{dep:datascript/upstream_test_support/datascript/test/core.cljc}
   -o
   datascript_conn_melange_base.ml)))

(rule
 (target datascript_conn_native_test_lru.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/lru.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_transact.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/transact.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_conn.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/conn.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_listen.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/listen.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_filter.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/filter.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

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
 (deps (:state datascript_conn_native_test_core.state) (:input datascript/upstream_tests/datascript/test/lookup_refs.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\nopen Datascript_conn_native_test_core\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native_test_parser.ml)
 (deps (:state datascript_conn_native_base.state) (:input datascript/upstream_tests/datascript/test/parser.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_native_base\n") (run %{exe:../bin/lg_cli.exe} --target native --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_native.ml)
 (deps (:state datascript_conn_native_base.state) (:input ../test_runner/clojure/run.cljc) ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_conn_native_base\nlet () =\n  ignore Datascript_conn_native_test_lru.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_transact.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_conn.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_listen.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_filter.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_ident.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_components.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_core.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_db.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_entity.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_explode.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_index.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_issues.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_lookup_refs.__lg_test_module_loaded;\n  ignore Datascript_conn_native_test_parser.__lg_test_module_loaded\n")
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
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/filter.cljc) ../bin/lg_cli.exe)
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
 (deps (:state datascript_conn_melange_test_core.state) (:input datascript/upstream_tests/datascript/test/lookup_refs.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\nopen Datascript_conn_melange_test_core\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange_test_parser.ml)
 (deps (:state datascript_conn_melange_base.state) (:input datascript/upstream_tests/datascript/test/parser.cljc) ../bin/lg_cli.exe)
 (action (with-stdout-to %{target} (progn (echo "open Datascript_conn_melange_base\n") (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input}) (echo "\nlet __lg_test_module_loaded = ()\n")))))

(rule
 (target datascript_conn_melange.ml)
 (deps (:state datascript_conn_melange_base.state) (:input ../test_runner/clojure/run.cljc) ../bin/lg_cli.exe)
 (action
  (with-stdout-to
   %{target}
   (progn
    (echo "open Datascript_conn_melange_base\nlet () =\n  ignore Datascript_conn_melange_test_lru.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_transact.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_conn.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_listen.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_filter.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_ident.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_components.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_core.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_db.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_entity.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_explode.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_index.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_issues.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_lookup_refs.__lg_test_module_loaded;\n  ignore Datascript_conn_melange_test_parser.__lg_test_module_loaded\n")
    (run %{exe:../bin/lg_cli.exe} --target melange --compile-chunk-from %{state} %{input})))))
