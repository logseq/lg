#!/bin/sh
set -eu

compiler="$1"
stdlib_state="$2"
bad_source="$3"
expected="$4"
output="${TMPDIR:-/tmp}/lg-stdlib-negative-$$.ml"
errors="${TMPDIR:-/tmp}/lg-stdlib-negative-$$.err"
trap 'rm -f "$output" "$errors"' EXIT

if LG_DISABLE_COMPILE_CACHE=1 "$compiler" --target native \
  --compile-chunk-from "$stdlib_state" "$bad_source" \
  -o "$output" 2>"$errors"; then
  echo "expected compilation to fail for $bad_source" >&2
  exit 1
fi

if ! grep -F "$expected" "$errors" >/dev/null; then
  echo "expected error containing: $expected" >&2
  cat "$errors" >&2
  exit 1
fi
