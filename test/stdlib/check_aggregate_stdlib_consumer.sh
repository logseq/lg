#!/bin/sh
set -eu

root=$1
consumer="$root/test/stdlib/dune"

if grep -E 'stdlib/(clojure|cljs)/.*\.lgi|--compile-files' "$consumer" >/dev/null; then
  echo "stdlib consumer still enumerates individual namespace sources" >&2
  exit 1
fi

if ! grep -F "../../stdlib/lg_stdlib_native.state" "$consumer" >/dev/null; then
  echo "stdlib consumer does not use the aggregate compiler state" >&2
  exit 1
fi

if ! grep -F "lg_compiled_stdlib_native" "$consumer" >/dev/null; then
  echo "stdlib consumer does not reuse the compiled stdlib library" >&2
  exit 1
fi

if grep -F '(cat %{dep:../../stdlib/lg_stdlib_native.ml})' "$consumer" >/dev/null; then
  echo "stdlib consumer still copies stdlib source into test modules" >&2
  exit 1
fi
