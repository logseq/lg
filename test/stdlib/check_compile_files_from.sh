#!/bin/sh
set -eu

compiler=$1
state=$2
provider=$3
consumer=$4
output=$5

LG_DISABLE_COMPILE_CACHE=1 "$compiler" --target native \
  --compile-files-from "$state" "$provider" "$consumer" -o "$output"

LG_DISABLE_COMPILE_CACHE=1 "$compiler" --target native \
  --compile-files-from-state "$state" "$output.state" \
  "$provider" "$consumer" -o "$output.with-state.ml"

test -s "$output.state"
test -s "$output.with-state.ml"
