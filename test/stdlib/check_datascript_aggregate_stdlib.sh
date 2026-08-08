#!/bin/sh
set -eu

repo_root=$1
rules="$repo_root/test/datascript_conn_tests.inc"

if grep -Eq '\.\./stdlib/(clojure|cljs)/' "$rules"; then
  echo "DataScript base compilation must not enumerate stdlib source files" >&2
  exit 1
fi

grep -q '\.\./stdlib/lg_stdlib_native\.state' "$rules"
grep -q '\.\./stdlib/lg_stdlib_melange\.state' "$rules"

mode_count=$(grep -c -- '--compile-files-from-state' "$rules")
if [ "$mode_count" -ne 3 ]; then
  echo "DataScript compilation must restore both aggregate stdlib states and the native runtime state" >&2
  exit 1
fi

grep -q '%{dep:datascript_conn_native_runtime.state}' "$rules"
