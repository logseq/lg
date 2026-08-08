#!/bin/sh
set -eu

root=$1

for name in identity complement boolean quot rem mod unchecked-inc unchecked-inc-int unchecked-dec unchecked-dec-int unchecked-negate unchecked-negate-int even? odd? every? ffirst fnext nfirst nnext not-any? not-every? split-at split-with nthnext nthrest bounded-count butlast take-last drop-last reverse interpose dedupe distinct zipmap hash-combine; do
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
