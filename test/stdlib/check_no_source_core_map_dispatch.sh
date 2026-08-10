#!/bin/sh
set -eu

root=$1

if ! grep -E '^\(defn map([[:space:]]|$)' \
  "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/map is not defined in source" >&2
  exit 1
fi

for file in \
  src/call_elaborator.ml \
  src/function_combinator_elaborator.ml \
  src/type_inference.ml; do
  if grep -E 'FSymbol "(map|(clojure|cljs)\.core/map)"|\| "(map|(clojure|cljs)\.core/map)" ->' \
    "$root/$file" >/dev/null; then
    echo "clojure.core/map is still runtime compiler-dispatched in $file" >&2
    exit 1
  fi
done

if ! grep -F '"__lg_map"' "$root/src/call_elaborator.ml" >/dev/null; then
  echo "clojure.core/map has no internal mixed-storage primitive" >&2
  exit 1
fi
