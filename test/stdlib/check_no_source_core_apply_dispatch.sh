#!/bin/sh
set -eu

root=$1

if ! grep -E '^\(defn apply([[:space:]]|$)' \
  "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/apply is not defined in source" >&2
  exit 1
fi

if ! grep -F "(list '__lg_apply" \
  "$root/stdlib/clojure/core.cljc" >/dev/null; then
  echo "clojure.core/apply does not inline to its private typed primitive" >&2
  exit 1
fi

for file in \
  src/call_elaborator.ml \
  src/function_combinator_elaborator.ml \
  src/type_inference.ml; do
  if grep -E 'FSymbol "(apply|(clojure|cljs)\.core/apply)"|\| "apply" ->' \
    "$root/$file" >/dev/null; then
    echo "clojure.core/apply is still compiler-dispatched by public name in $file" >&2
    exit 1
  fi
done

if ! grep -F '"__lg_apply"' "$root/src/call_elaborator.ml" >/dev/null; then
  echo "clojure.core/apply has no private static direct-call specialization" >&2
  exit 1
fi

if ! grep -E '^    apply \{:status :static-adaptation$' \
  "$root/stdlib/upstream.edn" >/dev/null; then
  echo "clojure.core/apply has no audited static source adaptation" >&2
  exit 1
fi
