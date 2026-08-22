#!/usr/bin/env bash
set -euo pipefail

workspace_root=${1:-$(cd "$(dirname "$0")/.." && pwd)}

for removed_path in \
  "$workspace_root/datascript" \
  "$workspace_root/datascript.opam" \
  "$workspace_root/test/datascript" \
  "$workspace_root/test/datascript_runtime" \
  "$workspace_root/benchmark/cljs/lg/benchmark/persistent_sorted_set.cljs"; do
  if [[ -e "$removed_path" ]]; then
    printf 'DataScript implementation/test content remains in LG: %s\n' "$removed_path" >&2
    exit 1
  fi
done

printf 'datascript-lg repository boundary passed\n'
