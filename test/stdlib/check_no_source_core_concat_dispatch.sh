#!/bin/sh
set -eu

root=$1

if ! grep -E '^\(defn concat([[:space:]]|$)' \
  "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/concat is not defined in source" >&2
  exit 1
fi

for file in \
  src/call_elaborator.ml \
  src/core_sequence_transform.ml \
  src/function_combinator_elaborator.ml \
  src/type_inference.ml; do
  if grep -E '"(concat|(clojure|cljs)\.core/concat)"' \
    "$root/$file" >/dev/null; then
    echo "clojure.core/concat is still runtime compiler-dispatched in $file" >&2
    exit 1
  fi
done

if ! grep -F '"__lg_concat"' "$root/src/call_elaborator.ml" >/dev/null; then
  echo "clojure.core/concat has no internal mixed-storage primitive" >&2
  exit 1
fi
