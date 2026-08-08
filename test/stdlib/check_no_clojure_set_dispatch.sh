#!/bin/sh
set -eu

root="$1"

if grep -F 'clojure_set_function' "$root/src/call_elaborator.ml" >/dev/null; then
  echo "clojure.set still has name-based call elaboration" >&2
  exit 1
fi

if grep -F 'Core_set.compile' "$root/src/call_elaborator.ml" >/dev/null; then
  echo "clojure.set still calls Core_set.compile" >&2
  exit 1
fi

if grep -F '"clojure.set"' "$root/src/core_namespaces.ml" >/dev/null; then
  echo "clojure.set is still classified as a compiler-owned namespace" >&2
  exit 1
fi

if test -e "$root/src/core_set.ml"; then
  echo "the compiler-owned clojure.set implementation still exists" >&2
  exit 1
fi

if grep -E 'has_source_name operation "(subset\?|union|intersection|difference)"' "$root/src/type_inference.ml" >/dev/null; then
  echo "clojure.set still has public-name type inference" >&2
  exit 1
fi

if ! grep -F '5c6ef531604662afbb33dc1b553d7602634d9656' "$root/stdlib/upstream.edn" >/dev/null; then
  echo "clojure.set is missing its pinned ClojureScript provenance" >&2
  exit 1
fi

for definition in union intersection difference 'subset?'; do
  if ! grep -F " $definition" "$root/stdlib/clojure/set.cljc" >/dev/null; then
    echo "clojure.set source is missing $definition" >&2
    exit 1
  fi
done
