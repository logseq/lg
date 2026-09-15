#!/usr/bin/env bash
set -euo pipefail

lg_cli=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")

project=$(mktemp -d)
trap 'rm -rf "$project"' EXIT

mkdir -p "$project/lg/sample" "$project/lg-test/sample"
cat > "$project/lg/sample/core.cljc" <<'EOF'
(ns sample.core)

(defn add-two [value]
  (+ value 2))

(type-record block (uuid :string) (title :string))
(type-record tag (uuid :string) (value :string))

(defn remove-selected [^:set<string> ids ^:seq<block> blocks]
  (vec (remove #(contains? ids (:uuid %)) blocks)))

(defn make-block [uuid title]
  (record block (uuid uuid) (title title)))

(defn make-tag [uuid value]
  (record tag (uuid uuid) (value value)))
EOF

cat > "$project/lg-test/sample/core_test.cljc" <<'EOF'
(ns sample.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [sample.core :as core]))

(deftest addition-works
  (testing "integer addition"
    (is (= 4 (core/add-two 2)))))

(deftest remove-preserves-sequence-element-type
  (let [blocks [(core/make-block "a" "A")
                (core/make-block "b" "B")]]
    (is (= "b" (:uuid (first (core/remove-selected #{"a"} blocks)))))))
EOF

(
  cd "$project"
  output=$("$lg_cli" test 2>&1)
  printf '%s\n' "$output"
  if [[ "$output" != *"2 tests run"* ]]; then
    printf 'Expected the addition-works test to run.\n' >&2
    exit 1
  fi
)

context_project=$(mktemp -d)
trap 'rm -rf "$project" "$context_project"' EXIT

mkdir -p "$context_project/src/sample" "$context_project/tests/sample"
cat > "$context_project/dune-project" <<'EOF'
(lang dune 3.0)
(name sample_context)
EOF
cat > "$context_project/dune" <<EOF
(rule
 (deps
  (source_tree src))
 (action
  (run "$lg_cli" --compile-files-from %{lib:lg:stdlib/lg_stdlib_native.state} src -o sample.ml)))
EOF
cat > "$context_project/src/sample/context.cljc" <<'EOF'
(ns sample.context)

(defn from-discovered-source [value]
  (+ value 3))
EOF
cat > "$context_project/tests/sample/context_test.cljc" <<'EOF'
(ns sample.context-test
  (:require [clojure.test :refer [deftest is testing]]
            [sample.context :as context]))

(deftest discovered-source-tree-works
  (testing "dune-declared source tree"
    (is (= 8 (context/from-discovered-source 5)))))
EOF

(
  cd "$context_project"
  output=$("$lg_cli" test tests 2>&1)
  printf '%s\n' "$output"
  if [[ "$output" != *"1 test run"* ]]; then
    printf 'Expected lg test to discover the Dune-declared source tree.\n' >&2
    exit 1
  fi
)

mkdir -p "$project/interface-order"
cat > "$project/interface-order/core.lgi" <<'EOF'
(ns sample.interface-order)
(type-record scheduler (generation :int))
EOF
cat > "$project/interface-order/core.cljc" <<'EOF'
(ns sample.interface-order)
(defn scheduler [] (record scheduler (generation 0)))
EOF

"$lg_cli" --target native --compile-files "$project/interface-order" \
  -o "$project/interface-order-directory.ml"
"$lg_cli" --target native --compile-files \
  "$project/interface-order/core.cljc" "$project/interface-order/core.lgi" \
  -o "$project/interface-order-explicit.ml"
