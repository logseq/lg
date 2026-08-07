#!/usr/bin/env bash
set -euo pipefail

workspace_root=$(cd "$(dirname "$0")/../.." && pwd)
source_file="$workspace_root/datascript/me/tonsky/persistent_sorted_set.cljc"
interface_file="$workspace_root/datascript/me/tonsky/persistent_sorted_set.mil"

if [[ ! -f "$interface_file" ]]; then
  echo "missing PSS interface: datascript/me/tonsky/persistent_sorted_set.mil" >&2
  exit 1
fi

if rg -n '^\s*\((type-alias|type-record|type-variant|signature)\b' "$source_file"; then
  echo "PSS implementation still contains type declarations or signatures" >&2
  exit 1
fi

mapfile -t compile_manifests < <(
  rg -l 'persistent_sorted_set\.cljc' \
    "$workspace_root/test/dune" \
    "$workspace_root/test/compiler_tests.ml" \
    "$workspace_root/test/datascript_conn_tests.inc"
)

for manifest in "${compile_manifests[@]}"; do
  source_count=$(rg -c 'persistent_sorted_set\.cljc' "$manifest")
  interface_count=$(rg -c 'persistent_sorted_set\.mil' "$manifest" || true)
  if [[ "$source_count" -ne "$interface_count" ]]; then
    echo "PSS interface/source count mismatch in $manifest: $interface_count/$source_count" >&2
    exit 1
  fi
done

echo "PSS interface audit passed"
