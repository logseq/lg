#!/bin/sh
set -eu

root=$1

for file in stdlib/clojure/core.mil stdlib/clojure/core.cljc; do
  if ! test -f "$root/$file"; then
    echo "$file is missing from the source standard library" >&2
    exit 1
  fi
done

for name in \
  identity complement boolean not reduced subs int-to-string-radix any? range shuffle \
  alength acopy aslice aconcat array-to-seq array-to-rseq quot rem mod \
  unchecked-add unchecked-add-int unchecked-subtract unchecked-subtract-int \
  unchecked-multiply unchecked-multiply-int unchecked-divide-int unchecked-remainder-int \
  unchecked-inc unchecked-inc-int unchecked-dec unchecked-dec-int unchecked-negate unchecked-negate-int \
  rand-int rand-nth bit-shift-right-zero-fill bit-and-not unsigned-bit-shift-right bit-count \
  second last \
  even? odd? every? ffirst fnext nfirst nnext not-any? not-every? \
  split-at split-with nthnext nthrest bounded-count butlast take-last drop-last reverse interpose dedupe distinct zipmap \
  comparator frequencies update-vals update-keys hash-combine max-key min-key constantly vec replicate key val parse-boolean splitv-at \
  booleans bytes chars shorts ints floats doubles longs random-uuid parse-uuid system-time parse-long parse-double merge-with \
  bit-clear bit-flip bit-set bit-test; do
  if ! grep -F "(defn $name" "$root/stdlib/clojure/core.cljc" >/dev/null; then
    echo "clojure.core/$name is not source-defined" >&2
    exit 1
  fi
done

if ! grep -F '(defmacro amap' "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/amap is not source-defined as the upstream macro" >&2
  exit 1
fi

if ! grep -F '(defn asort!' "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/asort! is not source-defined" >&2
  exit 1
fi

if ! grep -F '[clojure.core ' \
  "$root/stdlib/upstream.edn" >/dev/null; then
  echo "clojure.core is not first in aggregate stdlib order" >&2
  exit 1
fi
