#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
checker="$repo_root/script/benchmark_persistent_sorted_set_gate.js"
error_file="$(mktemp)"
trap 'rm -f "$error_file"' EXIT

passing_output='runtime lg-native
conj-10K 1.0
reduce-300K 2.0
runtime lg-melange
conj-10K 1.1
reduce-300K 2.1
runtime upstream-cljs
conj-10K 3.0
reduce-300K 4.0'

printf '%s\n' "$passing_output" | node "$checker"

expect_failure() {
  local expected="$1"
  local output="$2"
  if printf '%s\n' "$output" | node "$checker" 2>"$error_file"; then
    echo "expected benchmark gate to fail: $expected" >&2
    exit 1
  fi
  if ! grep -Fq "$expected" "$error_file"; then
    cat "$error_file" >&2
    exit 1
  fi
}

expect_failure "missing lg-native benchmark results" 'runtime lg-melange
conj-10K 1.0
runtime upstream-cljs
conj-10K 3.0'

expect_failure "missing lg-melange reduce-300K" 'runtime lg-native
conj-10K 1.0
reduce-300K 2.0
runtime lg-melange
conj-10K 1.1
runtime upstream-cljs
conj-10K 3.0
reduce-300K 4.0'

expect_failure "lg-native conj-10K 3ms is not faster than upstream-cljs 3ms" 'runtime lg-native
conj-10K 3.0
runtime lg-melange
conj-10K 1.1
runtime upstream-cljs
conj-10K 3.0'

expect_failure "invalid upstream-cljs conj-10K result" 'runtime lg-native
conj-10K 1.0
runtime lg-melange
conj-10K 1.1
runtime upstream-cljs
conj-10K NaN'
