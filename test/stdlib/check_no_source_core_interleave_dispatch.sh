#!/bin/sh
set -eu

root=$1

if ! grep -E '^\(defn interleave([[:space:]]|$)' \
  "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/interleave is not defined in source" >&2
  exit 1
fi

for file in \
  src/call_elaborator.ml \
  src/core_sequence_transform.ml \
  src/function_combinator_elaborator.ml \
  src/type_inference.ml; do
  if grep -E '"(interleave|(clojure|cljs)\.core/interleave)"' \
    "$root/$file" >/dev/null; then
    echo "clojure.core/interleave is still runtime compiler-dispatched in $file" >&2
    exit 1
  fi
done

if grep -E '^let interleave collections' \
  "$root/src/core_sequence_transform.ml" >/dev/null; then
  echo "clojure.core/interleave still has a compiler implementation" >&2
  exit 1
fi

if ! grep -F '"__lg_interleave"' "$root/src/call_elaborator.ml" >/dev/null; then
  echo "clojure.core/interleave has no internal mixed-storage primitive" >&2
  exit 1
fi
