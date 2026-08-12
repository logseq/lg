#!/bin/sh
set -eu

root="$1"

for namespace in clojure.data clojure.edn cljs.reader clojure.string clojure.walk clojure.zip; do
  if sed -n '/let is_core_namespace/,/let lookup_qualified_member/p' \
    "$root/src/core_namespaces.ml" | grep -F "\"$namespace\"" >/dev/null; then
    echo "$namespace is still compiler-owned" >&2
    exit 1
  fi

  if ! grep -F "$namespace" "$root/stdlib/upstream.edn" >/dev/null; then
    echo "$namespace is missing from the stdlib upstream manifest" >&2
    exit 1
  fi
done

for namespace in cljs.test; do
  if ! grep -F "  $namespace" "$root/stdlib/upstream.edn" >/dev/null; then
    echo "$namespace is missing a concrete Logseq migration classification" >&2
    exit 1
  fi
done


for namespace in clojure.test clojure.pprint; do
  if ! sed -n "/^  $namespace$/,/^  [a-z]/p" "$root/stdlib/upstream.edn" \
    | grep -F ':status :host-boundary' >/dev/null; then
    echo "$namespace is missing a concrete host-boundary status" >&2
    exit 1
  fi
done

for namespace in cljs.test; do
  if ! sed -n "/^  $namespace$/,/^  [a-z]/p" "$root/stdlib/upstream.edn" \
    | grep -F ':status :blocked' >/dev/null; then
    echo "$namespace is missing a concrete blocked status" >&2
    exit 1
  fi
done

for namespace in cljs.spec.alpha clojure.spec.alpha; do
  if ! sed -n "/^  $namespace$/,/^  [a-z]/p" "$root/stdlib/upstream.edn" \
    | grep -F ':status :out-of-scope' >/dev/null; then
    echo "$namespace is missing its excluded project scope status" >&2
    exit 1
  fi
  if ! sed -n "/^  $namespace$/,/^  [a-z]/p" "$root/stdlib/upstream.edn" \
    | grep -F ':reason :spec-is-explicitly-excluded-from-the-lg-stdlib-port' >/dev/null; then
    echo "$namespace is missing its explicit Spec exclusion reason" >&2
    exit 1
  fi
done

for namespace in cljs.core.async cljs.core.async.impl.channels clojure.core.async clojure.core.async.interop; do
  if ! sed -n "/^  $namespace$/,/^  [a-z]/p" "$root/stdlib/upstream.edn" \
    | grep -F ':status :out-of-scope' >/dev/null; then
    echo "$namespace is missing its excluded independent-library status" >&2
    exit 1
  fi
done

if ! grep -F ':aggregate-namespaces' "$root/stdlib/upstream.edn" >/dev/null; then
  echo "the stdlib manifest does not declare its aggregate namespace order" >&2
  exit 1
fi
