#!/bin/sh
set -eu

root=$1

if ! grep -E '^\(defn into([[:space:]]|$)' \
  "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/into is not defined in source" >&2
  exit 1
fi

for file in \
  src/call_elaborator.ml \
  src/core_sequence_transform.ml \
  src/type_inference.ml; do
  if grep -F '"into"' "$root/$file" >/dev/null; then
    echo "clojure.core/into is still compiler-dispatched in $file" >&2
    exit 1
  fi
done

if ! grep -F '"__lg_into"' "$root/src/call_elaborator.ml" >/dev/null; then
  echo "clojure.core/into has no internal typed primitive" >&2
  exit 1
fi
