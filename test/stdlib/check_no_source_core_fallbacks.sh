#!/bin/sh
set -eu

root=$1

for name in identity complement boolean reduced quot rem mod unchecked-add unchecked-add-int unchecked-subtract unchecked-subtract-int unchecked-multiply unchecked-multiply-int unchecked-divide-int unchecked-remainder-int unchecked-inc unchecked-inc-int unchecked-dec unchecked-dec-int unchecked-negate unchecked-negate-int rand-int rand-nth bit-shift-right-zero-fill second last even? odd? every? ffirst fnext nfirst nnext not-any? not-every? split-at split-with nthnext nthrest bounded-count butlast take-last drop-last reverse interpose dedupe distinct zipmap hash-combine; do
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

if grep -F 'FSymbol "not"' "$root/src/type_inference.ml" >/dev/null \
  || grep -F '"not" | "nil?"' "$root/src/call_elaborator.ml" >/dev/null \
  || grep -F '| "not" -> compile_not' "$root/src/core_boolean.ml" >/dev/null; then
  echo "clojure.core/not still has public-name compiler dispatch" >&2
  exit 1
fi
