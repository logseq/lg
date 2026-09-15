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
EOF

cat > "$project/lg-test/sample/core_test.cljc" <<'EOF'
(ns sample.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [sample.core :as core]))

(deftest addition-works
  (testing "integer addition"
    (is (= 4 (core/add-two 2)))))
EOF

(
  cd "$project"
  output=$("$lg_cli" test 2>&1)
  printf '%s\n' "$output"
  if [[ "$output" != *"1 test run"* ]]; then
    printf 'Expected the addition-works test to run.\n' >&2
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
