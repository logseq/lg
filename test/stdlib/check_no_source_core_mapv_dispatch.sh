#!/bin/sh
set -eu

root=$1

if ! grep -E '^\(defn mapv([[:space:]]|$)' \
  "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/mapv is not defined in source" >&2
  exit 1
fi

for file in \
  src/call_elaborator.ml \
  src/type_inference.ml; do
  if grep -F '"mapv"' "$root/$file" >/dev/null; then
    echo "clojure.core/mapv is still compiler-dispatched in $file" >&2
    exit 1
  fi
done

if ! grep -F '"__lg_mapv"' "$root/src/call_elaborator.ml" >/dev/null; then
  echo "clojure.core/mapv has no internal typed primitive" >&2
  exit 1
fi
