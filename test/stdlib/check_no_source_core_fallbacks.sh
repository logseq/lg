#!/bin/sh
set -eu

root=$1

for name in identity complement boolean reduced ensure-reduced reset-vals! subs int-to-string-radix any? range shuffle inc dec bit-not ratio? decimal? realized? alength aclone acopy aslice aconcat array-to-seq array-to-rseq array-seq to-array into-array amap asort! quot rem mod unchecked-add unchecked-add-int unchecked-subtract unchecked-subtract-int unchecked-multiply unchecked-multiply-int unchecked-divide-int unchecked-remainder-int unchecked-inc unchecked-inc-int unchecked-dec unchecked-dec-int unchecked-negate unchecked-negate-int rand-int rand-nth bit-and bit-or bit-xor bit-shift-left bit-shift-right bit-shift-right-zero-fill bit-and-not unsigned-bit-shift-right bit-count second last even? odd? every? ffirst fnext nfirst nnext not-any? not-every? split-at split-with nthnext nthrest bounded-count butlast take-last drop-last reverse interpose dedupe distinct distinct? not= zipmap comparator frequencies update-vals update-keys hash-combine max-key min-key constantly vec random-uuid parse-uuid system-time parse-long parse-double merge-with NaN? infinite? keyword-identical? symbol-identical? hash-long special-symbol?; do
  for file in \
    src/call_elaborator.ml \
    src/expression_support.ml \
    src/function_combinator_elaborator.ml \
    src/sequence_call_elaborator.ml \
    src/type_inference.ml; do
    # The first-class diagnostic is a static boundary, not a vec implementation.
    if test "$name" = vec && test "$file" = src/expression_support.ml; then
      continue
    fi
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
