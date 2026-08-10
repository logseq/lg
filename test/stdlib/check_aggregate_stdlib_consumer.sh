#!/bin/sh
set -eu

root=$1
consumer="$root/test/stdlib/dune"

if grep -E 'stdlib/(clojure|cljs)/.*\.mli|--compile-files' "$consumer" >/dev/null; then
  echo "stdlib consumer still enumerates individual namespace sources" >&2
  exit 1
fi

for artifact in lg_stdlib_native.ml lg_stdlib_native.state; do
  if ! grep -F "../../stdlib/$artifact" "$consumer" >/dev/null; then
    echo "stdlib consumer does not use aggregate artifact $artifact" >&2
    exit 1
  fi
done
