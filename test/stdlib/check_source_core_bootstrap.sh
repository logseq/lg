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
  identity completing complement boolean not zero? pos? neg? abs byte float short unchecked-byte unchecked-char unchecked-short unchecked-float unchecked-double unchecked-int unchecked-long double int long reduced reset-vals! subs int-to-string-radix any? range shuffle inc dec bit-not ratio? decimal? realized? \
  alength aclone acopy aslice aconcat array-to-seq array-to-rseq array-seq to-array into-array array-from array-binary-search-left array-binary-search-right quot rem mod \
  unchecked-add unchecked-add-int unchecked-subtract unchecked-subtract-int \
  unchecked-multiply unchecked-multiply-int unchecked-divide-int unchecked-remainder-int \
  unchecked-inc unchecked-inc-int unchecked-dec unchecked-dec-int unchecked-negate unchecked-negate-int \
  rand-int rand-nth bit-and bit-or bit-xor bit-shift-left bit-shift-right bit-shift-right-zero-fill bit-and-not unsigned-bit-shift-right bit-count \
  second last \
  even? odd? every? ffirst fnext nfirst nnext not-any? not-every? \
  split-at split-with nthnext nthrest bounded-count butlast take-last drop-last reverse interpose dedupe distinct zipmap \
  comparator frequencies update-vals update-keys hash-combine max-key min-key constantly vec replicate key val parse-boolean splitv-at distinct? not= \
  booleans bytes chars shorts ints floats doubles longs random-uuid parse-uuid system-time parse-long parse-double merge-with \
  NaN? infinite? keyword-identical? symbol-identical? hash-long special-symbol? \
  bit-clear bit-flip bit-set bit-test; do
  if ! grep -F "(defn $name" "$root/stdlib/clojure/core.cljc" >/dev/null; then
    echo "clojure.core/$name is not source-defined" >&2
    exit 1
  fi
done

for name in 'zero?' 'pos?' 'neg?' 'abs' byte float short unchecked-byte unchecked-char unchecked-short unchecked-float unchecked-double double int long; do
  if ! sed -n "/^(defn $name$/,/^$/p" "$root/stdlib/clojure/core.cljc" \
    | grep -F '{:inline' >/dev/null; then
    echo "clojure.core/$name is missing its source inline macro" >&2
    exit 1
  fi
done

for name in bit-and bit-or bit-xor bit-shift-left bit-shift-right; do
  if ! grep -E "^\\(defn ${name}([[:space:]]|$)" \
    "$root/stdlib/clojure/core.cljc" >/dev/null; then
    echo "clojure.core/$name is not exactly source-defined" >&2
    exit 1
  fi
done

if ! grep -E '^\(defmacro array-values([[:space:]]|$)' \
  "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/array-values is not source-defined as a macro" >&2
  exit 1
fi

for name in comment doto when-first while if-not when when-not cond and or if-let when-let if-some when-some '->' '->>' 'as->' 'cond->' 'cond->>' 'some->' 'some->>'; do
  if ! grep -E "^\\(defmacro ${name}([[:space:]]|$)" \
    "$root/stdlib/clojure/core.cljc" >/dev/null; then
    echo "clojure.core/$name is not source-defined as a macro" >&2
    exit 1
  fi
done

for name in \
  'nil?' 'true?' 'false?' 'int?' 'number?' 'string?' 'keyword?' 'symbol?' \
  'vector?' 'list?' 'seq?' 'set?' 'map?' 'fn?' 'coll?' 'associative?' \
  'rational?' 'float?' 'double?' 'sequential?' 'reversible?' 'sorted?' \
  'char?' 'identical?' 'array?' 'array-value?' 'reduced?' \
  'some?' 'boolean?' 'integer?' 'pos-int?' 'neg-int?' 'nat-int?' \
  'ident?' 'simple-ident?' 'qualified-ident?' \
  'simple-symbol?' 'qualified-symbol?' \
  'simple-keyword?' 'qualified-keyword?' \
  'counted?' 'seqable?' 'empty?' 'not-empty'; do
  if ! grep -F "(defn $name" \
    "$root/stdlib/clojure/core.cljc" >/dev/null; then
    echo "clojure.core/$name is not source-defined as a function" >&2
    exit 1
  fi
  if ! sed -n "/^(defn $name$/,/^$/p" "$root/stdlib/clojure/core.cljc" \
    | grep -F '{:inline' >/dev/null; then
    echo "clojure.core/$name is missing its source inline specialization" >&2
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

if ! grep -F '(defn ex-message' "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/ex-message is not source-defined as a function" >&2
  exit 1
fi

if ! grep -F '(defn ex-cause' "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/ex-cause is not source-defined as a function" >&2
  exit 1
fi

if ! grep -F '(defn re-pattern' "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/re-pattern is not source-defined as a function" >&2
  exit 1
fi

if ! grep -F '[clojure.core ' \
  "$root/stdlib/upstream.edn" >/dev/null; then
  echo "clojure.core is not first in aggregate stdlib order" >&2
  exit 1
fi
