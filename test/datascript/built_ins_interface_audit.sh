#!/usr/bin/env bash
set -euo pipefail

workspace_root=$(cd "$(dirname "$0")/../.." && pwd)
source_file="$workspace_root/test/datascript/upstream/built_ins.cljc"
interface_file="$workspace_root/test/datascript/lg/built_ins.lgi"

if [[ ! -f "$interface_file" ]]; then
  echo "missing DataScript built-ins interface: test/datascript/lg/built_ins.lgi" >&2
  exit 1
fi

if rg -n '^\s*\((type-alias|type-record|type-variant|signature)\b' "$source_file"; then
  echo "built-ins implementation still contains movable type declarations" >&2
  exit 1
fi

for manifest in "$workspace_root/test/dune" "$workspace_root/test/datascript_conn_tests.inc"; do
  source_count=$(rg -c 'datascript/upstream/built_ins\.cljc' "$manifest")
  interface_count=$(rg -c 'datascript/lg/built_ins\.lgi' "$manifest" || true)
  if [[ "$source_count" -ne "$interface_count" ]]; then
    echo "built-ins interface/source count mismatch in $manifest: $interface_count/$source_count" >&2
    exit 1
  fi
done

echo "DataScript built-ins interface audit passed"
