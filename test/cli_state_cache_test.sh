#!/bin/sh

set -eu

cli="$1"
source_file="$2"
continuation_file="$3"
stdlib_state="$4"
runtime_package_source="$5"
runtime_package_interface="$6"
cli_absolute=$(cd "$(dirname "$cli")" && pwd)/$(basename "$cli")
runtime_package_source_absolute=$(cd "$(dirname "$runtime_package_source")" && pwd)/$(basename "$runtime_package_source")
runtime_package_interface_absolute=$(cd "$(dirname "$runtime_package_interface")" && pwd)/$(basename "$runtime_package_interface")
test_dir=$(mktemp -d "${TMPDIR:-/tmp}/lg-state-cache-test.XXXXXX")

cleanup() {
  rm -rf "$test_dir"
}
trap cleanup EXIT HUP INT TERM

cat >"$test_dir/invalid-types.cljc" <<'SOURCE'
(ns contract-check)
(def functions (Array.make 1 (fn [x] x)))
(def number-result ((Array.get functions 0) 1))
(def string-result ((Array.get functions 0) "Ada"))
SOURCE

cat >"$test_dir/invalid-contract.cljc" <<'SOURCE'
(ns contract-check)
(signature contract-check/identity-value [a] :fn<a;a>)
(defn identity-value [x] 1)
SOURCE

expect_type_failure() {
  if "$@" >"$test_dir/type.stdout" 2>"$test_dir/type.stderr"; then
    echo "saved-state compilation accepted an invalid type contract" >&2
    exit 1
  fi
  if ! grep -Eq "expected of type|less general" "$test_dir/type.stderr"; then
    cat "$test_dir/type.stderr" >&2
    exit 1
  fi
}

for invalid_source in "$test_dir/invalid-types.cljc" "$test_dir/invalid-contract.cljc"; do
  expect_type_failure "$cli" --compile-chunk-from "$stdlib_state" \
    "$invalid_source" -o "$test_dir/invalid.ml"
  expect_type_failure "$cli" --compile-files-from "$stdlib_state" \
    "$invalid_source" -o "$test_dir/invalid.ml"
  expect_type_failure "$cli" --compile-files-from-state "$stdlib_state" \
    "$test_dir/invalid.state" "$invalid_source" -o "$test_dir/invalid.ml"
done
if [ -e "$test_dir/invalid.state" ] || [ -e "$test_dir/invalid.ml" ]; then
  echo "failed compilation published an artifact" >&2
  exit 1
fi

LG_CACHE_DIR="$test_dir/state-cache" \
  "$cli" --compile-files-from-state "$stdlib_state" "$test_dir/base.state" \
    "$source_file" -o "$test_dir/base.ml"

if grep -q 'clojure_core_trampoline' "$test_dir/base.ml"; then
  echo "state-producing compilation repeated the saved OCaml prefix" >&2
  exit 1
fi

(
  cd "$test_dir"
  LG_CACHE_DIR="$test_dir/state-cache" \
    "$cli_absolute" --compile-files-from-state "$test_dir/base.state" \
      "$test_dir/runtime-package.state" "$runtime_package_interface_absolute" \
      "$runtime_package_source_absolute" \
      -o "$test_dir/runtime-package.ml"
)

if grep -q 'math_magnitude_plus_two' "$test_dir/runtime-package.ml"; then
  echo "state-producing continuation repeated the saved OCaml prefix" >&2
  exit 1
fi

expect_state_failure() {
  state_path=$1
  expected_message=$2
  stderr_path=$3

  if "$cli" --compile-chunk-from "$state_path" "$continuation_file" \
      -o "$test_dir/invalid-state.ml" 2>"$stderr_path"; then
    echo "invalid compiler state unexpectedly compiled" >&2
    exit 1
  fi
  if ! grep -q "$expected_message" "$stderr_path"; then
    echo "compiler state failure did not report: $expected_message" >&2
    cat "$stderr_path" >&2
    exit 1
  fi
  if grep -q "Fatal error" "$stderr_path"; then
    echo "compiler state failure escaped as an uncaught exception" >&2
    cat "$stderr_path" >&2
    exit 1
  fi
}

printf 'not-an-lg-state\n' >"$test_dir/invalid.state"
expect_state_failure "$test_dir/invalid.state" \
  "invalid compiler state artifact" "$test_dir/invalid.stderr"

printf 'LG-COMPILER-STATE\n999\n' >"$test_dir/future.state"
expect_state_failure "$test_dir/future.state" \
  "unsupported compiler state version 999" "$test_dir/future.stderr"

printf 'LG-COMPILER-STATE\n1\n' >"$test_dir/legacy.state"
expect_state_failure "$test_dir/legacy.state" \
  "unsupported compiler state version 1" "$test_dir/legacy.stderr"

printf 'LG-COMPILER-STATE\n2\n' >"$test_dir/previous.state"
expect_state_failure "$test_dir/previous.state" \
  "unsupported compiler state version 2" "$test_dir/previous.stderr"

printf 'LG-COMPILER-STATE\n3\n' >"$test_dir/previous-3.state"
expect_state_failure "$test_dir/previous-3.state" \
  "unsupported compiler state version 3" "$test_dir/previous-3.stderr"

printf 'LG-COMPILER-STATE\n4\n' >"$test_dir/previous-4.state"
expect_state_failure "$test_dir/previous-4.state" \
  "unsupported compiler state version 4" "$test_dir/previous-4.stderr"

printf 'LG-COMPILER-STATE\n5\n' >"$test_dir/previous-5.state"
expect_state_failure "$test_dir/previous-5.state" \
  "unsupported compiler state version 5" "$test_dir/previous-5.stderr"

printf 'LG-COMPILER-STATE\n6\n' >"$test_dir/previous-6.state"
expect_state_failure "$test_dir/previous-6.state" \
  "unsupported compiler state version 6" "$test_dir/previous-6.stderr"

printf 'LG-COMPILER-STATE\n7\n' >"$test_dir/previous-7.state"
expect_state_failure "$test_dir/previous-7.state" \
  "unsupported compiler state version 7" "$test_dir/previous-7.stderr"

printf 'LG-COMPILER-STATE\n8\n' >"$test_dir/previous-8.state"
expect_state_failure "$test_dir/previous-8.state" \
  "unsupported compiler state version 8" "$test_dir/previous-8.stderr"

printf 'LG-COMPILER-STATE\n9\n' >"$test_dir/previous-9.state"
expect_state_failure "$test_dir/previous-9.state" \
  "unsupported compiler state version 9" "$test_dir/previous-9.stderr"

printf 'LG-COMPILER-STATE\n10\n' >"$test_dir/previous-10.state"
expect_state_failure "$test_dir/previous-10.state" \
  "unsupported compiler state version 10" "$test_dir/previous-10.stderr"

printf 'LG-COMPILER-STATE\n19\n' >"$test_dir/previous-19.state"
expect_state_failure "$test_dir/previous-19.state" \
  "unsupported compiler state version 19" "$test_dir/previous-19.stderr"

printf 'LG-COMPILER-STATE\n20\n' >"$test_dir/previous-20.state"
expect_state_failure "$test_dir/previous-20.state" \
  "unsupported compiler state version 20" "$test_dir/previous-20.stderr"

printf 'LG-COMPILER-STATE\n21\n' >"$test_dir/previous-21.state"
expect_state_failure "$test_dir/previous-21.state" \
  "unsupported compiler state version 21" "$test_dir/previous-21.stderr"

artifact_version=$(sed -n '2p' "$test_dir/base.state")
printf 'LG-COMPILER-STATE\n%s\nsaved-state\n536870913\n00000000000000000000000000000000\n' "$artifact_version" \
  >"$test_dir/oversized.state"
expect_state_failure "$test_dir/oversized.state" \
  "compiler state artifact exceeds maximum size" "$test_dir/oversized.stderr"

cp "$test_dir/base.state" "$test_dir/truncated.state"
truncate -s 32 "$test_dir/truncated.state"
expect_state_failure "$test_dir/truncated.state" \
  "truncated compiler state artifact" "$test_dir/truncated.stderr"

if [ -d "$test_dir/state-cache/compile-files" ]; then
  echo "state-producing compilation wrote a redundant prefix cache" >&2
  exit 1
fi

LG_CACHE_DIR="$test_dir/state-cache" \
  "$cli" --compile-files-from-state "$test_dir/base.state" \
    "$test_dir/continued.state" "$continuation_file" \
    -o "$test_dir/continued.ml"

if [ -d "$test_dir/state-cache/compile-files" ]; then
  echo "state-producing continuation wrote a redundant prefix cache" >&2
  exit 1
fi

LG_COMPILE_CACHE_MIN_SECONDS=0 \
LG_CACHE_DIR="$test_dir/output-cache" \
  "$cli" --compile-files-from "$stdlib_state" "$source_file" -o "$test_dir/output.ml"

if ! find "$test_dir/output-cache/compile-files" \
    -name '*.output.marshal' -type f | grep -q .; then
  echo "ordinary multi-file compilation did not retain its output cache" >&2
  exit 1
fi
if ! find "$test_dir/output-cache/compile-files" \
    -name '*.state.marshal' -type f | grep -q .; then
  echo "ordinary multi-file compilation did not retain its prefix state cache" >&2
  exit 1
fi

cached_output=$(find "$test_dir/output-cache/compile-files" \
  -name '*.output.marshal' -type f | head -1)
printf 'corrupt-prefix-output\n' >"$cached_output"
LG_COMPILE_CACHE_MIN_SECONDS=0 \
LG_COMPILE_CACHE_DEBUG=1 \
LG_CACHE_DIR="$test_dir/output-cache" \
  "$cli" --compile-files-from "$stdlib_state" "$source_file" \
    -o "$test_dir/rebuilt-output.ml" 2>"$test_dir/rebuilt-cache.stderr"
cmp "$test_dir/output.ml" "$test_dir/rebuilt-output.ml"
grep -q "compile cache ignored corrupt entry" \
  "$test_dir/rebuilt-cache.stderr"

LG_COMPILE_CACHE_MAX_BYTES=1 \
LG_COMPILE_CACHE_MIN_SECONDS=0 \
LG_CACHE_DIR="$test_dir/bounded-cache" \
  "$cli" --compile-files-from "$stdlib_state" "$source_file" \
    -o "$test_dir/bounded.ml"

# Count payload bytes, not directory blocks: du reports at least one block per
# directory on Linux even when every entry was pruned.
bounded_size=$(find "$test_dir/bounded-cache/compile-files" -type f \
  ! -name '.lock' -exec stat -c %s {} + 2>/dev/null \
  | awk '{total += $1} END {print total + 0}')
if [ "$bounded_size" -gt 1 ]; then
  echo "compile cache exceeded LG_COMPILE_CACHE_MAX_BYTES" >&2
  exit 1
fi

LG_COMPILE_CACHE_MIN_SECONDS=0 \
LG_CACHE_DIR="$test_dir/concurrent-cache" \
  "$cli" --compile-files-from "$stdlib_state" "$source_file" \
    -o "$test_dir/concurrent-first.ml" &
first_pid=$!
LG_COMPILE_CACHE_MIN_SECONDS=0 \
LG_CACHE_DIR="$test_dir/concurrent-cache" \
  "$cli" --compile-files-from "$stdlib_state" "$source_file" \
    -o "$test_dir/concurrent-second.ml" &
second_pid=$!
wait "$first_pid"
wait "$second_pid"
cmp "$test_dir/concurrent-first.ml" "$test_dir/concurrent-second.ml"

if [ ! -f "$test_dir/concurrent-cache/compile-files/.lock" ]; then
  echo "compile cache does not coordinate concurrent processes" >&2
  exit 1
fi
if find "$test_dir/concurrent-cache/compile-files" -name '*.tmp' -type f \
    | grep -q .; then
  echo "concurrent compile cache writes left temporary artifacts" >&2
  exit 1
fi
