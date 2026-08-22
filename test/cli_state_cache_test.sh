#!/bin/sh

set -eu

cli="$1"
source_file="$2"
continuation_file="$3"
stdlib_state="$4"
test_dir=$(mktemp -d "${TMPDIR:-/tmp}/lg-state-cache-test.XXXXXX")

cleanup() {
  rm -rf "$test_dir"
}
trap cleanup EXIT HUP INT TERM

LG_CACHE_DIR="$test_dir/state-cache" \
  "$cli" --compile-files-from-state "$stdlib_state" "$test_dir/base.state" \
    "$source_file" -o "$test_dir/base.ml"

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

printf 'LG-COMPILER-STATE\n2\nsaved-state\n536870913\n00000000000000000000000000000000\n' \
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

if find "$test_dir/output-cache/compile-files" \
    -name '*.state.marshal' -type f | grep -q .; then
  echo "ordinary multi-file compilation persisted cumulative prefix state" >&2
  exit 1
fi
if ! find "$test_dir/output-cache/compile-files" \
    -name '*.output.marshal' -type f | grep -q .; then
  echo "ordinary multi-file compilation did not retain its output cache" >&2
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

bounded_size=$(du -sk "$test_dir/bounded-cache/compile-files" \
  | awk '{print $1 * 1024}')
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
