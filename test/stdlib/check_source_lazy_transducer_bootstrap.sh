#!/bin/sh
set -eu

root=$1

for name in filter remove take drop take-while drop-while map-indexed keep keep-indexed mapcat cat halt-when transduce sequence repeatedly take-nth random-sample partition partition-all partitionv-all partition-by repeat cycle filterv dorun run! group-by sort sort-by; do
  if ! grep -E "^\\(defn ${name}([[:space:]]|$)" \
    "$root/stdlib/clojure/core.cljc" >/dev/null; then
    echo "clojure.core/$name is not source-defined" >&2
    exit 1
  fi
done

for name in lazy-seq lazy-cat; do
  if ! grep -E "^\\(defmacro ${name}([[:space:]]|$)" \
    "$root/stdlib/clojure/core.cljc" >/dev/null; then
    echo "clojure.core/$name is not source-defined as a macro" >&2
    exit 1
  fi
done

if rg -n 'apply_transducer|\| "(filter|remove|take|drop|take-while|drop-while|map-indexed|keep|keep-indexed|mapcat|cat|halt-when|transduce|sequence|repeatedly|take-nth|random-sample|partition|partition-all|partitionv-all|partition-by|repeat|cycle|filterv|dorun|run!|group-by|sort|sort-by)" -> compile_' \
  "$root/src/call_elaborator.ml" \
  "$root/src/core_form_expansion.ml" >/dev/null; then
  echo "lazy/transducer functions are still publicly compiler-dispatched" >&2
  exit 1
fi

if rg -n 'Runtime_dynamic|Obj\.magic|__lg_dynamic|to_dynamic|of_dynamic' \
  "$root/stdlib/clojure/core.cljc" \
  "$root/stdlib/clojure/core.mil" \
  "$root/runtime/runtime_seq.ml" \
  "$root/runtime/runtime_seq_melange.ml" >/dev/null; then
  echo "lazy/transducer source layer introduced a dynamic escape hatch" >&2
  exit 1
fi
