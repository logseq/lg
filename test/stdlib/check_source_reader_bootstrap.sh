#!/bin/sh
set -eu

root=$1

for file in \
  stdlib/clojure/edn.mil \
  stdlib/clojure/edn.cljc \
  stdlib/cljs/reader.mil \
  stdlib/cljs/reader.cljc; do
  if ! test -f "$root/$file"; then
    echo "$file is missing from the source standard library" >&2
    exit 1
  fi
done

if grep -F '"read-string"' "$root/src/core_edn.ml" >/dev/null; then
  echo "read-string is still exported by the compiler-owned EDN namespace" >&2
  exit 1
fi

for namespace in clojure.edn cljs.reader; do
  if sed -n '/let is_core_namespace/,/let lookup_qualified_member/p' \
    "$root/src/core_namespaces.ml" | grep -F "\"$namespace\"" >/dev/null; then
    echo "$namespace is still compiler-owned" >&2
    exit 1
  fi
done
