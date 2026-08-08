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
  identity complement boolean even? odd? every? ffirst fnext nfirst nnext not-any? not-every? \
  split-at split-with nthnext nthrest bounded-count butlast take-last drop-last reverse interpose dedupe distinct zipmap \
  bit-clear bit-flip bit-set bit-test; do
  if ! grep -F "(defn $name" "$root/stdlib/clojure/core.cljc" >/dev/null; then
    echo "clojure.core/$name is not source-defined" >&2
    exit 1
  fi
done

if ! grep -F '[clojure.core ' \
  "$root/stdlib/upstream.edn" >/dev/null; then
  echo "clojure.core is not first in aggregate stdlib order" >&2
  exit 1
fi
