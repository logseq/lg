#!/bin/sh
set -eu

root="$1"

for namespace in clojure.data clojure.edn cljs.reader clojure.string clojure.walk; do
  if grep -F "\"$namespace\"" "$root/src/core_namespaces.ml" >/dev/null; then
    echo "$namespace is still compiler-owned" >&2
    exit 1
  fi

  if ! grep -F "$namespace" "$root/stdlib/upstream.edn" >/dev/null; then
    echo "$namespace is missing from the stdlib upstream manifest" >&2
    exit 1
  fi
done

for module in core_data.ml core_edn.ml core_walk.ml; do
  if test -e "$root/src/$module"; then
    echo "$module still implements a public Clojure namespace in the compiler" >&2
    exit 1
  fi
done

if ! grep -F ':aggregate-namespaces' "$root/stdlib/upstream.edn" >/dev/null; then
  echo "the stdlib manifest does not declare its aggregate namespace order" >&2
  exit 1
fi
