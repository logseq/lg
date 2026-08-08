#!/bin/sh
set -eu

root=$1

for file in stdlib/clojure/string.mil stdlib/clojure/string.cljc; do
  if ! test -f "$root/$file"; then
    echo "$file is missing from the source standard library" >&2
    exit 1
  fi
done

if ! grep -F '(defn escape' "$root/stdlib/clojure/string.cljc" >/dev/null; then
  echo "clojure.string/escape is not source-defined" >&2
  exit 1
fi

if grep -F 'clojure.string/escape' "$root/src/expression_support.ml" >/dev/null \
  || grep -F 'str/escape' "$root/src/expression_support.ml" >/dev/null; then
  echo "clojure.string/escape still has a compiler-owned first-class fallback" >&2
  exit 1
fi

if ! grep -F 'let escape source replacements' \
  "$root/runtime/runtime_string.ml" >/dev/null; then
  echo "clojure.string/escape is missing its typed host primitive" >&2
  exit 1
fi
