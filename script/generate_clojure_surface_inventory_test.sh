#!/bin/sh
set -eu

if test "$#" -ge 1; then
  root=$(CDPATH= cd -- "$1" && pwd)
else
  root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
fi
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

mkdir -p "$tmp/logseq/src"
cat >"$tmp/logseq/src/example.cljs" <<'EOF'
(ns example
  (:require [clojure.string :as string]
            [clojure.set :refer [union]]
            [clojure.walk :as walk]
            [cljs.reader :as reader]
            [cljs.pprint :as pprint]
            [cljs.test :as test]
            [cljs.spec.alpha :as spec]
            [cljs.core.async :as async]
            [cljs.core.async.impl.channels :as async-channels]
            [clojure.core.async :as jvm-async]
            [clojure.core.async.interop :as async-interop]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.tools.build.api :as build]
            [clojure.tools.deps :as deps]
            [clojure.tools.cli :as cli]
            [clojure.test.check.generators :as gen]
            [clojure.stacktrace :as stacktrace]
            [clojure.data.json :as json]
            [cljs.core.match :as core-match]
            [cljs.analyzer.api :as analyzer]
            [clojure.zip :as zip]))

(string/upper-case "logseq")
(union #{1} #{2})
(clojure.set/project #{} [])
(walk/postwalk identity {})
(reader/read-string "{:answer 42}")
(reader/parse-timestamp "2020-01-01T00:00:00.000Z")
(pprint/pprint "value")
(test/successful? {:fail 0 :error 0})
(test/empty-env)
(def current-test-env test/*current-env*)
(test/testing "inventory" true)
(test/deftest inventory-test (test/is true))
(test/are [value] (= value 1) 1)
(test/try-expr nil true)
(test/ns? 'example)
(test/run-test inventory-test)
(test/run-tests 'example)
(test/use-fixtures :each (fn [body] (body)))
(spec/valid? string? "value")
(zip/root nil)
(cljs.core/identity 1)
(cljs.core/chunk-buffer 4)
(cljs.core/array-chunk (cljs.core/array-values 1 2))
(io/resource "fixture.edn")
(shell/sh "true")
(build/create-basis {})
(deps/combine-aliases {} [])
(cli/parse-opts [] [])
(gen/generate gen/boolean)
(stacktrace/print-cause-trace nil)
(json/read-str "{}")
(core-match/match 1 1 :one)
(analyzer/all-ns)

(def documentation
  "See https://clojure.org/reference/reader for symbol syntax.")
;; https://clojure.org/reference/reader is documentation, not a qualified var.
EOF

cat >"$tmp/malformed.cljs" <<'EOF'
(ns malformed
EOF
if bb "$root/script/clojure_namespace_inventory.clj" \
    "$tmp/malformed.cljs" \
    >"$tmp/malformed.out" 2>"$tmp/malformed.err"; then
  echo "namespace scanner silently accepted an unreadable source file" >&2
  exit 1
fi
grep -F "$tmp/malformed.cljs" "$tmp/malformed.err" >/dev/null

mkdir -p "$tmp/clojurescript"
cat >"$tmp/clojurescript/core.cljs" <<'EOF'
(ns cljs.core)

(defn public-function [x] x)
(defn- private-function [x] x)
(defn ^:private metadata-private-function [x] x)
(defn attr-map-private-function
  {:private true}
  [x]
  x)
(defprotocol VisibleProtocol
  (visible-method [x])
  (attr-map-private-method
    {:private true}
    [x]))
#?(:cljs (defn conditional-function [x] x))
(if true
  (defn branch-defined-function [x] x)
  (defn branch-defined-function [x] x))
EOF
cat >"$tmp/clojurescript/core.cljc" <<'EOF'
(ns cljs.core)

(core/defmacro public-macro [form] form)
(core/defmacro ^:private private-macro [form] form)
(core/defmacro attr-map-private-macro
  {:private true}
  [form]
  form)
EOF

bb "$root/script/extract_clojurescript_public_vars.clj" cljs.core \
  "$tmp/clojurescript/core.cljs" "$tmp/clojurescript/core.cljc" \
  >"$tmp/upstream-vars.tsv"

awk -F '\t' '$1 == "cljs.core/public-function" && $2 == "function" {found=1} END {exit !found}' "$tmp/upstream-vars.tsv"
awk -F '\t' '$1 == "cljs.core/conditional-function" && $2 == "function" {found=1} END {exit !found}' "$tmp/upstream-vars.tsv"
awk -F '\t' '$1 == "cljs.core/branch-defined-function" && $2 == "function" {found=1} END {exit !found}' "$tmp/upstream-vars.tsv"
awk -F '\t' '$1 == "cljs.core/public-macro" && $2 == "macro" {found=1} END {exit !found}' "$tmp/upstream-vars.tsv"
awk -F '\t' '$1 == "cljs.core/visible-method" && $2 == "protocol-method" {found=1} END {exit !found}' "$tmp/upstream-vars.tsv"
awk -F '\t' '$1 ~ /private/ {found=1} END {exit found}' "$tmp/upstream-vars.tsv"

bb "$root/script/extract_stdlib_manifest_status.clj" \
  "$root/stdlib/upstream.edn" >"$tmp/manifest-status.tsv"
awk -F '\t' '
  BEGIN {
    split(".. await copy-arguments declare defmethod defmulti defn- defonce defprotocol defrecord deftype es6-iterable exists? extend-protocol extend-type gen-apply-to gen-apply-to-simple goog-define implements? import import-macros js-arguments js-comment js-debugger js-delete js-fn? js-in js-inline-comment js-mod js-str letfn load-file* macroexpand macroexpand-1 memfn ns-imports ns-interns ns-publics ns-unmap refer-clojure refer-global require require-global require-macros simple-benchmark specify specify! str_ this-as time unchecked-get unchecked-set undefined? use use-macros with-redefs", names, " ")
    for (i in names) required["clojure.core/" names[i]] = 1
  }
  $1 == "definition" && ($2 in required) &&
  $3 != "deferred" && $4 != "" {classified[$2] = 1}
  END {
    for (name in required) {
      if (!(name in classified)) {
        print "unclassified cljs.core macro: " name > "/dev/stderr"
        failed = 1
      }
    }
    exit failed
  }
' "$tmp/manifest-status.tsv"
awk -F '\t' '
  BEGIN {
    split("*exec-tap-fn* --destructure-map ExceptionInfo Throwable->map add-tap add-watch alter-meta! array-chunk array-iter array-list bases chunk chunk-append chunk-buffer chunk-cons chunk-first chunk-next chunk-rest chunked-seq chunked-seq? create-ns defmacro demunge destructure disj! dispatch-fn double-array dt->et eduction es6-entries-iterator es6-iterator es6-iterator-seq es6-set-entries-iterator eval find-macros-ns find-ns find-ns-obj get-method get-validator int-array is_proto_ iter iterable? iteration js-invoke js-iterable? js-keys key->js load-file long-array methods missing-protocol mk-bound-fn munge native-satisfies? nil-iter ns-interns* ns-name obj-map object-array persistent-array-map-seq pop! pr-seq-writer pr-str* pr-str-with-opts prefer-method prefers print-map print-meta? print-prefix-map print-str println-str prn-str prn-str-with-opts ranged-iterator remove-all-methods remove-method remove-tap remove-watch reset-meta! rsubseq seq-iter seq-to-map-for-destructuring set-print-err-fn! set-print-fn! set-validator! sorted-map-by sorted-set-by string-iter string-print subseq supers tap> test transformer-iterator type->str", names, " ")
    for (i in names) required["clojure.core/" names[i]] = 1
  }
  $1 == "definition" && ($2 in required) &&
  $3 != "deferred" && $4 != "" {classified[$2] = 1}
  END {
    for (name in required) {
      if (!(name in classified)) {
        print "unclassified cljs.core function: " name > "/dev/stderr"
        failed = 1
      }
    }
    exit failed
  }
' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.set/union" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/random-uuid" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/parse-uuid" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/system-time" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/aclone" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/array-seq" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/to-array" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/into-array" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/inc" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/completing" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.core/contains?" || $2 == "clojure.core/assoc" || $2 == "clojure.core/dissoc" || $2 == "clojure.core/keys") && $3 == "source" {found++} END {exit found != 4}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.core/subvec" || $2 == "clojure.core/array") && $3 == "source" {found++} END {exit found != 2}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/enable-console-print!" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.core/hash" || $2 == "clojure.core/compare" || $2 == "clojure.core/hash-ordered-coll" || $2 == "clojure.core/hash-unordered-coll") && $3 == "source" {found++} END {exit found != 4}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.core/make-array" || $2 == "clojure.core/aget" || $2 == "clojure.core/aset" || $2 == "clojure.core/atom" || $2 == "clojure.core/volatile!") && $3 == "source" {found++} END {exit found != 5}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.core/transient" || $2 == "clojure.core/persistent!" || $2 == "clojure.core/conj!" || $2 == "clojure.core/assoc!" || $2 == "clojure.core/dissoc!" || $2 == "clojure.core/pop!" || $2 == "clojure.core/disj!") && $3 == "source" {found++} END {exit found != 7}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.core/get" || $2 == "clojure.core/get-in" || $2 == "clojure.core/assoc-in" || $2 == "clojure.core/update" || $2 == "clojure.core/update-in" || $2 == "clojure.core/select-keys" || $2 == "clojure.core/merge" || $2 == "clojure.core/vals") && $3 == "source" {found++} END {exit found != 8}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.core/seq" || $2 == "clojure.core/first" || $2 == "clojure.core/rest" || $2 == "clojure.core/next" || $2 == "clojure.core/cons") && $3 == "source" {found++} END {exit found != 5}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "cljs.pprint/float?" || $2 == "cljs.pprint/char-code" || $2 == "cljs.pprint/getf" || $2 == "cljs.pprint/setf") && $3 == "source" {found++} END {exit found != 4}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/some" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/conj" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.core/namespace" || $2 == "clojure.core/unreduced") && $3 == "source" {found++} END {exit found != 2}' "$tmp/manifest-status.tsv"
awk -F '\t' '
  BEGIN {
    split("-as-transient -assoc -assoc! -assoc-n -assoc-n! -comparator -compare -compare-and-set! -conj -conj! -contains-key? -count -deref -disjoin -disjoin! -dissoc -dissoc! -drop -empty -entry-key -equiv -find -first -hash -key -kv-reduce -lookup -meta -next -nth -peek -persistent! -pop -pop! -realized? -reduce -reset! -rest -rseq -seq -sorted-seq -sorted-seq-from -swap! -val -vreset! -with-meta", names, " ")
    for (i in names) required["clojure.core/" names[i]] = 1
  }
  $1 == "definition" && ($2 in required) && $3 == "source" {found[$2] = 1}
  END {
    for (name in required) {
      if (!(name in found)) {
        print "core protocol method is not source-owned: " name > "/dev/stderr"
        failed = 1
      }
    }
    exit failed
  }
' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/name" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.core/keyword" || $2 == "clojure.core/symbol") && $3 == "source" {found++} END {exit found != 2}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/list*" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.core/unchecked-int" || $2 == "clojure.core/unchecked-long") && $3 == "source" {found++} END {exit found != 2}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.core/unchecked-max" || $2 == "clojure.core/unchecked-min") && $3 == "source" {found++} END {exit found != 2}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.core/mask" || $2 == "clojure.core/bitpos") && $3 == "source" {found++} END {exit found != 2}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/caching-hash" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/gensym" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/truth_" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/dec" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/bit-not" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/bit-and" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/bit-or" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/bit-xor" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/bit-shift-left" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/bit-shift-right" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/ratio?" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/decimal?" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/realized?" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/array-from" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/array-values" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/array-binary-search-left" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/array-binary-search-right" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/distinct?" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/not=" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/parse-long" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/parse-double" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/merge-with" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/keyword-identical?" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.core/key-test" || $2 == "clojure.core/reduceable?" || $2 == "clojure.core/vector-lite" || $2 == "clojure.core/hash-map-lite" || $2 == "clojure.core/set-lite") && $3 == "source" {found++} END {exit found != 5}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/flatten" && $3 == "blocked-static-typing" && $4 == "arbitrarily-nested-sequential-input-can-produce-heterogeneous-leaf-types-without-a-closed-recursive-value-domain" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/memoize" && $3 == "blocked-static-typing" && $4 == "returned-function-must-preserve-the-input-functions-complete-arity-shape-and-cache-heterogeneous-argument-tuples" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/NaN?" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/uuid?" && $3 == "source" && $4 == "source-public-function-matches-cljs-iuuid-predicate-with-a-first-class-nominal-uuid-signature-and-inline-static-specialization-that-distinguishes-ordinary-strings" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/delay?" && $3 == "source" && $4 == "source-public-function-matches-cljs-delay-instance-predicate-with-a-first-class-lazy-signature-and-inline-static-specialization-for-arbitrary-static-values" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/ensure-reduced" && $3 == "source" && $4 == "source-public-function-matches-cljs-conditional-reduced-wrapper-with-a-first-class-non-reduced-signature-and-inline-static-specialization-that-preserves-an-existing-parameterized-wrapper" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/force" && $3 == "source" && $4 == "source-public-function-matches-cljs-delay-force-with-a-first-class-lazy-signature-and-inline-static-specialization-that-preserves-non-delay-input-types" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/rand" && $3 == "source" && $4 == "source-public-overloads-preserve-cljs-zero-and-one-arity-floating-results-with-inline-static-int-or-float-bound-specialization" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/to-array-2d" && $3 == "source" && $4 == "source-port-preserves-ragged-nested-seqable-conversion-through-static-inner-and-outer-sequence-witnesses" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/keep-indexed" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/doseq" && $3 == "source" && $4 == "source-macro-preserves-clojurescript-binding-modifier-order-and-per-loop-while-termination-without-the-javascript-chunked-sequence-fast-path" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.core/chunk-buffer" || $2 == "clojure.core/array-chunk" || $2 == "clojure.core/chunk-append" || $2 == "clojure.core/chunk" || $2 == "clojure.core/-drop-first") && $3 == "source" {found++} END {exit found != 5}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/uuid" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.set/project" || $2 == "clojure.set/rename") && $3 == "source" {found++} END {exit found != 2}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.set/index" || $2 == "clojure.set/join") && $3 == "source" {found++} END {exit found != 2}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.walk/postwalk" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "cljs.reader/parse-and-validate-timestamp" && $3 == "source" && $4 == "source-port-preserves-upstream-validation-and-offset-control-flow-over-a-narrow-static-homogeneous-regex-capture-boundary" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "namespace" && $2 == "cljs.test" && $3 == "blocked-static-typing" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "namespace" && $2 == "cljs.spec.alpha" && $3 == "out-of-scope" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "namespace" && ($2 == "cljs.core.async" || $2 == "cljs.core.async.impl.channels" || $2 == "clojure.core.async" || $2 == "clojure.core.async.interop") && $3 == "out-of-scope" {found++} END {exit found != 4}' "$tmp/manifest-status.tsv"

"$root/script/generate_clojure_surface_inventory.sh" \
  "$root" "$tmp/logseq" >"$tmp/inventory.tsv"

awk -F '\t' '
  $1 == "namespace" &&
  ($2 == "clojure.core" || $2 == "cljs.core") &&
  $3 == "source-with-primitive-boundary" {found[$2] = 1}
  END {exit !(found["clojure.core"] && found["cljs.core"])}
' "$tmp/inventory.tsv"
awk -F '\t' '
  $1 == "namespace-bootstrap" &&
  ($2 == "clojure.core" || $2 == "cljs.core") &&
  $3 == "automatic-core-refer" {found[$2] = 1}
  END {exit !(found["clojure.core"] && found["cljs.core"])}
' "$tmp/inventory.tsv"
awk -F '\t' '
  $1 == "namespace" && $3 == "compiler-owned" {found = 1}
  END {exit found}
' "$tmp/inventory.tsv"
awk -F '\t' '
  $1 == "namespace" &&
  (($2 == "cljs.math" && $3 == "source-with-primitive-boundary") ||
   ($2 == "cljs.cache" && $3 == "source-with-primitive-boundary") ||
   ($2 == "clojure.core.protocols" && $3 == "source")) {found[$2] = 1}
  END {
    exit !(found["cljs.math"] && found["cljs.cache"] &&
           found["clojure.core.protocols"])
  }
' "$tmp/inventory.tsv"
manifest_namespace_count=$(awk -F '\t' '$1 == "namespace" {count++} END {print count + 0}' \
  "$tmp/manifest-status.tsv")
inventory_namespace_count=$(awk -F '\t' '$1 == "namespace" {count++} END {print count + 0}' \
  "$tmp/inventory.tsv")
if test "$inventory_namespace_count" -ne "$((manifest_namespace_count + 1))"; then
  echo "inventory must cover every manifest namespace plus the cljs.core alias" >&2
  exit 1
fi
awk -F '\t' '
  FNR == NR && $1 == "namespace" {required[$2] = 1; next}
  $1 == "namespace" {delete required[$2]}
  END {
    for (namespace in required) {
      print "manifest namespace missing from inventory: " namespace > "/dev/stderr"
      failed = 1
    }
    exit failed
  }
' "$tmp/manifest-status.tsv" "$tmp/inventory.tsv"

awk -F '\t' '
  ($1 == "logseq-namespace-status" ||
   $1 == "logseq-qualified-var-status") && $3 == "unsupported" {
    print "unclassified Logseq dependency: " $2 > "/dev/stderr"
    failed = 1
  }
  END {exit failed}
' "$tmp/inventory.tsv"
awk -F '\t' '
  ($1 == "logseq-namespace" || $1 == "logseq-qualified-var") &&
  $2 ~ /^clojure\.org(\/|$)/ {
    print "documentation URL was counted as Clojure code: " $2 > "/dev/stderr"
    failed = 1
  }
  END {exit failed}
' "$tmp/inventory.tsv"

awk -F '\t' '$1 == "compiler-call" && ($2 == "identity" || $3 == "source-shadowed") {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && ($2 == "keyword" || $2 == "symbol") {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && ($2 == "__lg_builtin-keyword" || $2 == "__lg_builtin-symbol") && $3 == "typed-primitive" {found++} END {exit found != 2}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && ($2 == "contains?" || $2 == "assoc" || $2 == "dissoc" || $2 == "find" || $2 == "keys") {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_find" && $3 == "typed-primitive" && $4 == "typed-map-entry-lookup-capability-primitive" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "definition" && ($2 == "clojure.core/max" || $2 == "clojure.core/min") && $3 == "source" {found++} END {exit found != 2}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "compiler-call" && ($2 == "__lg_max" || $2 == "__lg_min") && $3 == "typed-primitive" && $4 == "typed-numeric-extrema-specialization-primitive" {found++} END {exit found != 2}' "$tmp/inventory.tsv"
if awk -F '\t' '$1 == "compiler-call" && ($2 == "max" || $2 == "min") {found=1} END {exit !found}' "$tmp/inventory.tsv"; then
  echo "public max/min compiler dispatch must be removed" >&2
  exit 1
fi
awk -F '\t' '$1 == "compiler-call" && ($2 == "subvec" || $2 == "array") {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && ($2 == "__lg_subvec" || $2 == "__lg_array") && $3 == "typed-primitive" {found++} END {exit found != 2}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && ($2 == "weak-deref" || $2 == "weak-clear!") {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-form" && ($2 == "weak-deref" || $2 == "weak-clear!") {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && ($2 == "__lg_weak-deref" || $2 == "__lg_weak-clear!") && $3 == "typed-primitive" {found++} END {exit found != 2}' "$tmp/inventory.tsv"
rg -q '^\(defn weak-deref( |$)' "$root/stdlib/clojure/core.cljc"
rg -q '^\(defn weak-clear!( |$)' "$root/stdlib/clojure/core.cljc"
awk -F '\t' '$1 == "compiler-call" && ($2 == "future-call" || $2 == "enable-console-print!") {found=1} END {exit found}' "$tmp/inventory.tsv"
rg -q '^\(defn future-call ' "$root/stdlib/clojure/core.cljc"
rg -q '^\(defn enable-console-print! ' "$root/stdlib/clojure/core.cljc"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_map-predicate" {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_vector-predicate" {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_associative-predicate" {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_coll-predicate" {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_set-predicate" {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_reversible-predicate" {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && ($2 == "indexed?" || $2 == "__lg_sequential-predicate" || $2 == "__lg_sorted-predicate") {found=1} END {exit found}' "$tmp/inventory.tsv"
if awk -F '\t' '$1 == "compiler-call" && ($2 == "+" || $2 == "-" || $2 == "*" || $2 == "/" || $2 == "<" || $2 == "<=" || $2 == ">" || $2 == ">=" || $2 == "==") {found=1} END {exit !found}' "$tmp/inventory.tsv"; then
  echo "public numeric operator compiler dispatch must be removed" >&2
  exit 1
fi
awk -F '\t' '$1 == "compiler-call" && ($2 == "__lg_add" || $2 == "__lg_subtract" || $2 == "__lg_multiply" || $2 == "__lg_divide" || $2 == "__lg_less" || $2 == "__lg_less-equal" || $2 == "__lg_greater" || $2 == "__lg_greater-equal" || $2 == "__lg_numeric-equal") && $3 == "typed-primitive" {found++} END {exit found != 9}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && ($2 == "instance?" || $2 == "satisfies?") && $3 == "special-form" && $4 == "compiler-owned-static-type-or-protocol-witness-elaboration" {found++} END {exit found != 2}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "type" && $3 == "blocked-static-typing" && $4 == "runtime-class-inspection-conflicts-with-lg-closed-static-types" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "binding" && $3 == "special-form" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_ex-message" && $3 == "typed-primitive" && $4 == "static-exception-message-extraction-primitive" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_ex-cause" && $3 == "typed-primitive" && $4 == "static-optional-exception-cause-primitive" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_re-pattern" && $3 == "typed-primitive" && $4 == "validated-static-regex-construction-primitive" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_swap!" && $3 == "typed-primitive" && $4 == "typed-contextual-reference-swap-primitive" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_ensure-reduced" && $3 == "typed-primitive" && $4 == "typed-conditional-parameterized-reduced-wrapper-primitive" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_force" && $3 == "typed-primitive" && $4 == "typed-lazy-force-or-static-identity-primitive" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_rand" && $3 == "typed-primitive" && $4 == "typed-int-or-float-random-bound-specialization-primitive" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "re-find" && $3 == "blocked-static-typing" && $4 == "capture-count-dependent-optional-string-or-heterogeneous-capture-vector-result" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-form" && $2 == "doseq" {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-form" && $2 == "for" && $3 == "special-form" && $4 == "compiler-owned-binding-modifier-and-lazy-sequence-expansion" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-form" && ($2 == "case" || $2 == "condp") && $3 == "special-form" && $4 == "compiler-owned-source-control-flow-expansion" {found++} END {exit found != 2}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-form" && ($2 == "fn" || $2 == "let" || $2 == "loop") && $3 == "special-form" && $4 == "compiler-owned-syntax-or-control-flow" {found++} END {exit found != 3}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-form" && ($2 == "cat" || $2 == "or" || $2 == "when") {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '
  $1 == "compiler-call" && $3 == "blocked-static-typing" &&
  ($4 == "" || $4 == "requires-variadic-dependent-lazy-or-capability-type-support") {
    print "blocked compiler call lacks a concrete reason: " $2 > "/dev/stderr"
    failed=1
  }
  END {exit failed}
' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "Buffer.t" {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace" && $2 == "clojure.data" && $3 == "source-with-primitive-boundary" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace" && $2 == "clojure.string" && $3 == "source-with-primitive-boundary" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace" && $2 == "clojure.edn" && $3 == "source-with-primitive-boundary" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && $2 == "clojure.data/diff" && $3 == "source" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && $2 == "clojure.data/equality-partition" && $3 == "source" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && $2 == "clojure.data/diff-similar" && $3 == "source" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "runtime-primitive" && $2 == "Lg_runtime.Runtime_data.equality_partition_tag" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "runtime-primitive" && $2 == "Lg_runtime.Runtime_data.diff_similar" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && $2 == "clojure.string/escape" && $3 == "source" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "runtime-primitive" && $2 == "Lg_runtime.Runtime_string.split" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "runtime-primitive" && $2 == "Lg_runtime.Runtime_string.timestamp_captures" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "runtime-primitive" && $2 == "Lg_runtime.Runtime_uuid.valid_string" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "runtime-primitive" && $2 == "Lg_runtime.Runtime_array.copy" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "runtime-primitive" && $2 == "Lg_runtime.Runtime_array.of_seq" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "runtime-primitive" && $2 == "Lg_runtime.Runtime_future.realized" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace" && $2 == "clojure.string" && $3 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace" && $2 == "clojure.set" && $3 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var" && $2 == "clojure.string/upper-case" && $3 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "clojure.string" && $3 == "source-aggregate" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "clojure.set" && $3 == "source-aggregate" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "clojure.walk" && $3 == "source-aggregate" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "cljs.pprint" && $3 == "blocked-static-typing" && $4 == 1 && $5 == "logical-block-right-margin-and-custom-dispatch-layout-require-a-closed-static-pretty-writer-domain" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && ($2 == "cljs.test/compose-fixtures" || $2 == "cljs.test/join-fixtures" || $2 == "cljs.test/successful?") && $3 == "source" {found++} END {exit found != 3}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && ($2 == "cljs.test/run-block" || $2 == "cljs.test/test-var-block" || $2 == "cljs.test/test-var" || $2 == "cljs.test/test-vars-block" || $2 == "cljs.test/test-vars" || $2 == "cljs.test/testing-vars-str") && $3 == "source" {found++} END {exit found != 6}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && ($2 == "cljs.test/async" || $2 == "cljs.test/async?" || $2 == "cljs.test/block") && $3 == "source" {found++} END {exit found != 3}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && ($2 == "cljs.test/run-tests-block" || $2 == "cljs.test/test-all-vars-block" || $2 == "cljs.test/test-all-vars" || $2 == "cljs.test/test-ns-block" || $2 == "cljs.test/test-ns") && $3 == "source" {found++} END {exit found != 5}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && ($2 == "cljs.test/empty-env" || $2 == "cljs.test/get-current-env" || $2 == "cljs.test/set-env!" || $2 == "cljs.test/clear-env!" || $2 == "cljs.test/get-and-clear-env!" || $2 == "cljs.test/inc-report-counter!" || $2 == "cljs.test/testing-contexts-str" || $2 == "cljs.test/testing") && $3 == "source" {found++} END {exit found != 8}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && $2 == "cljs.test/*current-env*" && $3 == "source" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && ($2 == "cljs.test/is" || $2 == "cljs.test/are" || $2 == "cljs.test/try-expr" || $2 == "cljs.test/deftest" || $2 == "cljs.test/run-test" || $2 == "cljs.test/run-tests" || $2 == "cljs.test/ns?") && $3 == "source" {found++} END {exit found != 7}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && $2 == "cljs.test/use-fixtures" && $3 == "source" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && ($2 == "clojure.core/chunk-buffer" || $2 == "clojure.core/array-chunk" || $2 == "clojure.core/chunk-append" || $2 == "clojure.core/chunk") && $3 == "source" {found++} END {exit found != 4}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && ($2 == "clojure.core/-chunked-first" || $2 == "clojure.core/-chunked-rest" || $2 == "clojure.core/-chunked-next" || $2 == "clojure.core/chunk-cons" || $2 == "clojure.core/chunk-first" || $2 == "clojure.core/chunk-rest" || $2 == "clojure.core/chunk-next") && $3 == "source" {found++} END {exit found != 7}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && $2 == "clojure.core/uuid" && $3 == "source" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "cljs.test/successful?" && $3 == "source-aggregate" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && ($2 == "cljs.test/empty-env" || $2 == "cljs.test/testing") && $3 == "source-aggregate" && $4 == 1 {found++} END {exit found != 2}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "cljs.test/*current-env*" && $3 == "source-aggregate" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && ($2 == "cljs.test/is" || $2 == "cljs.test/run-tests") && $3 == "source-aggregate" && $4 == 1 {found++} END {exit found != 2}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "cljs.spec.alpha" && $3 == "out-of-scope" && $4 == 1 && $5 == "excluded-by-project-scope" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace" && $2 == "clojure.zip" && $3 == "source-with-primitive-boundary" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "clojure.zip" && $3 == "source-aggregate" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "clojure.walk/postwalk" && $3 == "source-aggregate" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "cljs.reader" && $3 == "source-aggregate" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "cljs.reader/read-string" && $3 == "source-aggregate" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "cljs.reader/parse-timestamp" && $3 == "host-boundary" && $4 == 1 && $5 == "returns-a-javascript-date-which-has-no-shared-native-source-representation" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "clojure.set/project" && $3 == "source-aggregate" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "clojure.zip/root" && $3 == "source-aggregate" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "cljs.core/identity" && $3 == "source-core-alias" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "cljs.spec.alpha/valid?" && $3 == "out-of-scope" && $4 == 1 && $5 == "excluded-by-project-scope" {found=1} END {exit !found}' "$tmp/inventory.tsv"
