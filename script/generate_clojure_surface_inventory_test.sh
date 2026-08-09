#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

mkdir -p "$tmp/logseq/src"
cat >"$tmp/logseq/src/example.cljs" <<'EOF'
(ns example
  (:require [clojure.string :as string]
            [clojure.set :refer [union]]
            [clojure.walk :as walk]
            [cljs.pprint :as pprint]
            [cljs.spec.alpha :as spec]
            [clojure.zip :as zip]))

(string/upper-case "logseq")
(union #{1} #{2})
(clojure.set/project #{} [])
(walk/postwalk identity {})
(pprint/pprint "value")
(spec/valid? string? "value")
(zip/root nil)
(cljs.core/identity 1)
EOF

mkdir -p "$tmp/clojurescript"
cat >"$tmp/clojurescript/core.cljs" <<'EOF'
(ns cljs.core)

(defn public-function [x] x)
(defn- private-function [x] x)
(defn ^:private metadata-private-function [x] x)
#?(:cljs (defn conditional-function [x] x))
EOF
cat >"$tmp/clojurescript/core.cljc" <<'EOF'
(ns cljs.core)

(core/defmacro public-macro [form] form)
(core/defmacro ^:private private-macro [form] form)
EOF

bb "$root/script/extract_clojurescript_public_vars.clj" cljs.core \
  "$tmp/clojurescript/core.cljs" "$tmp/clojurescript/core.cljc" \
  >"$tmp/upstream-vars.tsv"

awk -F '\t' '$1 == "cljs.core/public-function" && $2 == "function" {found=1} END {exit !found}' "$tmp/upstream-vars.tsv"
awk -F '\t' '$1 == "cljs.core/conditional-function" && $2 == "function" {found=1} END {exit !found}' "$tmp/upstream-vars.tsv"
awk -F '\t' '$1 == "cljs.core/public-macro" && $2 == "macro" {found=1} END {exit !found}' "$tmp/upstream-vars.tsv"
awk -F '\t' '$1 ~ /private/ {found=1} END {exit found}' "$tmp/upstream-vars.tsv"

bb "$root/script/extract_stdlib_manifest_status.clj" \
  "$root/stdlib/upstream.edn" >"$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.set/union" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/random-uuid" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/parse-uuid" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/system-time" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/aclone" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/array-seq" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/to-array" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/into-array" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/inc" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
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
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/rand" && $3 == "blocked-static-typing" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/distinct?" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/not=" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/parse-long" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/parse-double" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/merge-with" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/keyword-identical?" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/NaN?" && $3 == "source" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.core/uuid?" && $3 == "blocked-static-typing" && $4 == "native-uuid-is-nominal-but-melange-currently-erases-uuid-to-string-so-a-source-predicate-cannot-distinguish-ordinary-strings-without-a-shared-nominal-representation" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "definition" && $2 == "clojure.set/project" && $3 == "blocked-static-typing" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "namespace" && $2 == "cljs.test" && $3 == "blocked-static-typing" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"
awk -F '\t' '$1 == "namespace" && $2 == "cljs.spec.alpha" && $3 == "out-of-scope" {found=1} END {exit !found}' "$tmp/manifest-status.tsv"

"$root/script/generate_clojure_surface_inventory.sh" \
  "$root" "$tmp/logseq" >"$tmp/inventory.tsv"

awk -F '\t' '$1 == "compiler-call" && ($2 == "identity" || $3 == "source-shadowed") {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "+" && $3 == "typed-primitive" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "-" && $3 == "typed-primitive" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "binding" && $3 == "special-form" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_ex-message" && $3 == "typed-primitive" && $4 == "static-exception-message-extraction-primitive" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_ex-cause" && $3 == "typed-primitive" && $4 == "static-optional-exception-cause-primitive" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "__lg_re-pattern" && $3 == "typed-primitive" && $4 == "validated-static-regex-construction-primitive" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "re-find" && $3 == "blocked-static-typing" && $4 == "capture-count-dependent-optional-string-or-heterogeneous-capture-vector-result" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "rand" && $3 == "blocked-static-typing" && $4 == "same-arity-int-and-float-bound-overloads-cannot-share-one-source-function-type" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '
  $1 == "compiler-call" && $3 == "blocked-static-typing" &&
  ($4 == "" || $4 == "requires-variadic-dependent-lazy-or-capability-type-support") {
    print "blocked compiler call lacks a concrete reason: " $2 > "/dev/stderr"
    failed=1
  }
  END {exit failed}
' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "Buffer.t" {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace" && $2 == "clojure.data" && $3 == "compiler-owned" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace" && $2 == "clojure.string" && $3 == "source-with-primitive-boundary" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace" && $2 == "clojure.edn" && $3 == "source-with-primitive-boundary" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && $2 == "clojure.data/diff" && $3 == "host-boundary" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && $2 == "clojure.string/escape" && $3 == "source" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "runtime-primitive" && $2 == "Lg_runtime.Runtime_string.split" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "runtime-primitive" && $2 == "Lg_runtime.Runtime_uuid.valid_string" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "runtime-primitive" && $2 == "Lg_runtime.Runtime_array.copy" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "runtime-primitive" && $2 == "Lg_runtime.Runtime_array.of_seq" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "runtime-primitive" && $2 == "Lg_runtime.Runtime_future.realized" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace" && $2 == "clojure.string" && $3 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace" && $2 == "clojure.set" && $3 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var" && $2 == "clojure.string/upper-case" && $3 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "clojure.string" && $3 == "source-aggregate" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "clojure.set" && $3 == "source-aggregate" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "clojure.walk" && $3 == "blocked-static-typing" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "cljs.pprint" && $3 == "blocked-static-typing" && $4 == 1 && $5 == "readable-and-display-printing-require-distinct-static-printer-witnesses" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "cljs.spec.alpha" && $3 == "out-of-scope" && $4 == 1 && $5 == "excluded-by-project-scope" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "clojure.zip" && $3 == "blocked-static-typing" && $4 == 1 && $5 == "public-heterogeneous-location-vectors-and-metadata-held-generic-callbacks-require-a-closed-zipper-domain" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "clojure.walk/postwalk" && $3 == "blocked-static-typing" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "clojure.set/project" && $3 == "blocked-static-typing" && $4 == 1 && $5 == "dependent-relation-map-projection-is-not-yet-source-expressible" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "clojure.zip/root" && $3 == "blocked-static-typing" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "cljs.core/identity" && $3 == "source-core-alias" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "cljs.spec.alpha/valid?" && $3 == "out-of-scope" && $4 == 1 && $5 == "excluded-by-project-scope" {found=1} END {exit !found}' "$tmp/inventory.tsv"
