#!/bin/sh
set -eu

root=$1

for name in identity complement even? odd? not-any? not-every? split-at split-with; do
  for file in \
    src/call_elaborator.ml \
    src/expression_support.ml \
    src/function_combinator_elaborator.ml \
    src/sequence_call_elaborator.ml \
    src/type_inference.ml; do
    if grep -F "\"$name\"" "$root/$file" >/dev/null; then
      echo "clojure.core/$name is still compiler-dispatched in $file" >&2
      exit 1
    fi
  done
done
