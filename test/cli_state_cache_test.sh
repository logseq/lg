#!/bin/sh

set -eu

cli="$1"
source_file="$2"
continuation_file="$3"
test_dir=$(mktemp -d "${TMPDIR:-/tmp}/lg-state-cache-test.XXXXXX")

cleanup() {
  rm -rf "$test_dir"
}
trap cleanup EXIT HUP INT TERM

LG_CACHE_DIR="$test_dir/state-cache" \
  "$cli" --compile-files-state "$test_dir/base.state" \
    "$source_file" -o "$test_dir/base.ml"

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

LG_CACHE_DIR="$test_dir/output-cache" \
  "$cli" --compile-files "$source_file" -o "$test_dir/output.ml"

if ! find "$test_dir/output-cache/compile-files" \
    -name '*.state.marshal' -type f | grep -q .; then
  echo "ordinary multi-file compilation did not retain its prefix cache" >&2
  exit 1
fi
