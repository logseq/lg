#!/bin/sh
set -eu

root=$1

for name in fnil partial; do
  if ! grep -E "^\\(defn $name([[:space:]]|$)" \
    "$root/stdlib/clojure/core.cljc" >/dev/null; then
    echo "clojure.core/$name is not defined in source" >&2
    exit 1
  fi

  for file in \
    src/call_elaborator.ml \
    src/collection_operation_elaborator.ml \
    src/function_combinator_elaborator.ml \
    src/type_inference.ml; do
    if grep -E "\"($name|(clojure|cljs)\\.core/$name)\"" \
      "$root/$file" >/dev/null; then
      echo "clojure.core/$name is still compiler-dispatched by public name in $file" >&2
      exit 1
    fi
  done

  if ! grep -F "\"__lg_$name\"" "$root/src/call_elaborator.ml" >/dev/null; then
    echo "clojure.core/$name has no private static direct-call specialization" >&2
    exit 1
  fi
done
