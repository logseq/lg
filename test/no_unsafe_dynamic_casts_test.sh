#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

if rg -n 'Obj\.(obj|magic)' "$root/runtime/runtime_dynamic.ml"; then
  echo "runtime_dynamic contains an unsafe cast" >&2
  exit 1
fi

if rg -n 'Runtime_dynamic\.polymorphic_(equal|hash|str|pr_str)' \
  "$root/src" "$root/runtime"; then
  echo "generic compiler/runtime code still probes universal dynamic values" >&2
  exit 1
fi
