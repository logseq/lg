#!/bin/sh
set -eu

root=$1

for file in stdlib/clojure/core.lgi stdlib/clojure/core.cljc; do
  if ! test -f "$root/$file"; then
    echo "$file is missing from the source standard library" >&2
    exit 1
  fi
done

for protocol in IDrop IMapEntry INext IPending ISeq; do
  if ! grep -F "(defprotocol $protocol" \
    "$root/stdlib/clojure/core.cljc" >/dev/null; then
    echo "clojure.core/$protocol is not source-declared" >&2
    exit 1
  fi
done

for name in \
  identity completing complement every-pred some-fn boolean truth_ not zero? pos? neg? abs byte float short unchecked-byte unchecked-char unchecked-short unchecked-float unchecked-double unchecked-int unchecked-long double int long reduced reset-vals! subs int-to-string-radix any? range shuffle inc dec bit-not ratio? decimal? realized? int-rotate-left imul m3-mix-K1 m3-mix-H1 m3-fmix m3-hash-int m3-hash-unencoded-chars hash-string* mix-collection-hash \
  alength aclone acopy aslice aconcat array-to-seq array-to-rseq array-seq to-array into-array array-from array-binary-search-left array-binary-search-right object-array quot rem mod \
  unchecked-add unchecked-add-int unchecked-subtract unchecked-subtract-int \
  unchecked-multiply unchecked-multiply-int unchecked-divide-int unchecked-remainder-int \
  unchecked-inc unchecked-inc-int unchecked-dec unchecked-dec-int unchecked-negate unchecked-negate-int \
  rand-int rand-nth gensym bit-and unsafe-bit-and bit-or bit-xor bit-shift-left bit-shift-right bit-shift-right-zero-fill bit-and-not unsigned-bit-shift-right bit-count \
  second last \
  even? odd? every? ffirst fnext nfirst nnext not-any? not-every? \
  split-at split-with nthnext nthrest bounded-count butlast take-last drop-last reverse interpose dedupe distinct zipmap \
  comparator frequencies update-vals update-keys hash-combine max-key min-key constantly vec replicate key val key-test equiv-map reduceable? vector-lite hash-map-lite set-lite parse-boolean splitv-at distinct? not= \
  booleans bytes chars shorts ints floats doubles longs random-uuid parse-uuid system-time parse-long parse-double merge-with \
  NaN? infinite? keyword-identical? symbol-identical? hash-long special-symbol? \
  bit-clear bit-flip bit-set bit-test; do
  if ! grep -F "(defn $name" "$root/stdlib/clojure/core.cljc" >/dev/null; then
    echo "clojure.core/$name is not source-defined" >&2
    exit 1
  fi
done

for name in m3-seed m3-C1 m3-C2; do
  if ! grep -F "(def $name " "$root/stdlib/clojure/core.cljc" >/dev/null; then
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

for name in comment doto doseq when-first while if-not when when-not cond and or if-let when-let if-some when-some divide unchecked-max unchecked-min mask bitpos caching-hash '->' '->>' 'as->' 'cond->' 'cond->>' 'some->' 'some->>'; do
  if ! grep -E "^\\(defmacro ${name}([[:space:]]|$)" \
    "$root/stdlib/clojure/core.cljc" >/dev/null; then
    echo "clojure.core/$name is not source-defined as a macro" >&2
    exit 1
  fi
done

for name in truth_ \
  'nil?' 'true?' 'false?' 'int?' 'number?' 'string?' 'keyword?' 'symbol?' \
  'vector?' 'list?' 'seq?' 'set?' 'map?' 'fn?' 'coll?' 'associative?' \
  'rational?' 'float?' 'double?' 'sequential?' 'reversible?' 'sorted?' 'reduceable?' \
  'char?' 'identical?' 'array?' 'array-value?' 'reduced?' \
  'uuid?' 'delay?' \
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

for name in force ensure-reduced rand; do
  if ! grep -E "^\\(defn ${name}([[:space:]]|$)" \
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

if ! grep -E '^\(defn to-array-2d([[:space:]]|$)' \
  "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/to-array-2d is not source-defined as a function" >&2
  exit 1
fi

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

if ! grep -F '(defn ex-data' "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/ex-data is not source-defined as a function" >&2
  exit 1
fi

for name in '*exec-tap-fn*' add-tap remove-tap 'tap>'; do
  if ! grep -F "(defn $name" "$root/stdlib/clojure/core.cljc" >/dev/null; then
    echo "clojure.core/$name is not source-defined as a function" >&2
    exit 1
  fi
done

if ! grep -F '(defn re-pattern' "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/re-pattern is not source-defined as a function" >&2
  exit 1
fi

if ! grep -F '(defn re-find' "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/re-find is not source-defined as a function" >&2
  exit 1
fi

if ! grep -F '(defn re-matches' "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/re-matches is not source-defined as a function" >&2
  exit 1
fi

if ! grep -F '(defn re-seq' "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/re-seq is not source-defined as a function" >&2
  exit 1
fi

if ! grep -F '(defn add-watch' "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/add-watch is not source-defined as a function" >&2
  exit 1
fi

if ! grep -F '(defn remove-watch' "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/remove-watch is not source-defined as a function" >&2
  exit 1
fi

if ! grep -F '(defn get-validator' "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/get-validator is not source-defined as a function" >&2
  exit 1
fi

if ! grep -F '(defn set-validator!' "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/set-validator! is not source-defined as a function" >&2
  exit 1
fi

if ! grep -F '(signature clojure.core/get-validator [value]' \
  "$root/stdlib/clojure/core.lgi" >/dev/null \
  || ! grep -F ':fn<ref<value>;option<fn<value;bool>>>' \
    "$root/stdlib/clojure/core.lgi" >/dev/null; then
  echo "clojure.core/get-validator is not declared with a typed validator signature" >&2
  exit 1
fi

if ! grep -F '(signature clojure.core/set-validator! [value]' \
  "$root/stdlib/clojure/core.lgi" >/dev/null \
  || ! grep -F ':fn<ref<value>;option<fn<value;bool>>;nil>' \
    "$root/stdlib/clojure/core.lgi" >/dev/null; then
  echo "clojure.core/set-validator! is not declared with a typed validator signature" >&2
  exit 1
fi

if ! grep -F '(def ^:dynamic *flush-on-newline* true)' \
  "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/*flush-on-newline* is not source-defined as a dynamic var" >&2
  exit 1
fi

if ! grep -F '(def ^:dynamic *print-newline* true)' \
  "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/*print-newline* is not source-defined as a dynamic var" >&2
  exit 1
fi

if ! grep -F '(def ^:dynamic *print-readably* true)' \
  "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/*print-readably* is not source-defined as a dynamic var" >&2
  exit 1
fi

if ! grep -F '(def ^:dynamic *print-length* None)' \
  "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/*print-length* is not source-defined as a dynamic var" >&2
  exit 1
fi

if ! grep -F '(def ^:dynamic *print-level* None)' \
  "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/*print-level* is not source-defined as a dynamic var" >&2
  exit 1
fi

if ! grep -F '(signature clojure.core/*flush-on-newline*' \
  "$root/stdlib/clojure/core.lgi" >/dev/null \
  || ! grep -F ':bool' "$root/stdlib/clojure/core.lgi" >/dev/null; then
  echo "clojure.core/*flush-on-newline* is not declared with a bool signature" >&2
  exit 1
fi

if ! grep -F '(signature clojure.core/*print-newline*' \
  "$root/stdlib/clojure/core.lgi" >/dev/null \
  || ! grep -F ':bool' "$root/stdlib/clojure/core.lgi" >/dev/null; then
  echo "clojure.core/*print-newline* is not declared with a bool signature" >&2
  exit 1
fi

if ! grep -F '(signature clojure.core/*print-readably*' \
  "$root/stdlib/clojure/core.lgi" >/dev/null \
  || ! grep -F ':bool' "$root/stdlib/clojure/core.lgi" >/dev/null; then
  echo "clojure.core/*print-readably* is not declared with a bool signature" >&2
  exit 1
fi

if ! grep -F '(signature clojure.core/*print-length*' \
  "$root/stdlib/clojure/core.lgi" >/dev/null \
  || ! grep -F ':option<int>' "$root/stdlib/clojure/core.lgi" >/dev/null; then
  echo "clojure.core/*print-length* is not declared with an option<int> signature" >&2
  exit 1
fi

if ! grep -F '(signature clojure.core/*print-level*' \
  "$root/stdlib/clojure/core.lgi" >/dev/null \
  || ! grep -F ':option<int>' "$root/stdlib/clojure/core.lgi" >/dev/null; then
  echo "clojure.core/*print-level* is not declared with an option<int> signature" >&2
  exit 1
fi

if ! grep -F '[clojure.core ' \
  "$root/stdlib/upstream.edn" >/dev/null; then
  echo "clojure.core is not first in aggregate stdlib order" >&2
  exit 1
fi
