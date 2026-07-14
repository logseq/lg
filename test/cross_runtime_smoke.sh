#!/bin/sh
set -eu

native_executable="$1"
melange_javascript="$2"
jsoo_javascript="$3"
expected="$4"
actual="$(mktemp "${TMPDIR:-/tmp}/lg-cross-runtime.XXXXXX")"
trap 'rm -f "$actual"' EXIT

case "$native_executable" in
  */*) ;;
  *) native_executable="./$native_executable" ;;
esac

{
  "$native_executable"
  node "$melange_javascript"
  node "$jsoo_javascript"
} > "$actual"

diff -u "$expected" "$actual"
