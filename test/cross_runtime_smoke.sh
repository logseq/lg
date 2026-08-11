#!/bin/sh
set -eu

native_executable="$1"
melange_javascript="$2"
if test "$#" -eq 3; then
  jsoo_javascript=
  expected="$3"
else
  jsoo_javascript="$3"
  expected="$4"
fi
actual="$(mktemp "${TMPDIR:-/tmp}/lg-cross-runtime.XXXXXX")"
trap 'rm -f "$actual"' EXIT

case "$native_executable" in
  */*) ;;
  *) native_executable="./$native_executable" ;;
esac

{
  "$native_executable"
  node "$melange_javascript"
  if test -n "$jsoo_javascript"; then
    node "$jsoo_javascript"
  fi
} > "$actual"

diff -u "$expected" "$actual"
