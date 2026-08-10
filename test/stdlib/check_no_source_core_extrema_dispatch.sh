#!/bin/sh
set -eu

root=$1

for name in max min; do
  if ! grep -E "^\\(defn (\\^number )?$name([[:space:]]|$)" \
    "$root/stdlib/clojure/core.cljc" >/dev/null; then
    echo "clojure.core/$name is not defined in source" >&2
    exit 1
  fi

  for file in \
    src/call_elaborator.ml \
    src/expression_support.ml \
    src/type_inference.ml; do
    if grep -F "\"$name\"" "$root/$file" >/dev/null; then
      echo "clojure.core/$name is still compiler-dispatched in $file" >&2
      exit 1
    fi
  done
done
