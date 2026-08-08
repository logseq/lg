#!/bin/sh
set -eu

root=$1

for name in bit-clear bit-flip bit-set bit-test; do
  for file in \
    src/call_elaborator.ml \
    src/core_scalar.ml \
    src/type_inference.ml; do
    if grep -F "\"$name\"" "$root/$file" >/dev/null; then
      echo "clojure.core/$name is still compiler-dispatched in $file" >&2
      exit 1
    fi
  done
done
