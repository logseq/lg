#!/bin/sh
set -eu

root=$1

if ! grep -E '^\(defn comp([[:space:]]|$)' \
  "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/comp is not defined in source" >&2
  exit 1
fi

for file in \
  src/call_elaborator.ml \
  src/function_combinator_elaborator.ml \
  src/type_inference.ml; do
  if grep -E '"(comp|(clojure|cljs)\.core/comp)"' \
    "$root/$file" >/dev/null; then
    echo "clojure.core/comp is still compiler-dispatched by public name in $file" >&2
    exit 1
  fi
done

if ! grep -F '"__lg_comp"' "$root/src/call_elaborator.ml" >/dev/null; then
  echo "clojure.core/comp has no private static direct-call specialization" >&2
  exit 1
fi
