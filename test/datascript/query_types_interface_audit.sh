#!/usr/bin/env bash
set -euo pipefail

workspace_root=$(cd "$(dirname "$0")/../.." && pwd)
source_file="$workspace_root/test/datascript/lg/query_types.cljc"
interface_file="$workspace_root/test/datascript/lg/query_types.mli"

if [[ ! -f "$interface_file" ]]; then
  echo "missing DataScript query-types interface: test/datascript/lg/query_types.mli" >&2
  exit 1
fi

if rg -n '^\s*\((type-alias|type-record|type-variant|signature)\b' "$source_file"; then
  echo "query-types implementation still contains movable type declarations" >&2
  exit 1
fi

for manifest in "$workspace_root/test/dune" "$workspace_root/test/datascript_conn_tests.inc" "$workspace_root/test/compiler_tests.ml"; do
  source_count=$(rg -c 'datascript/lg/query_types\.cljc' "$manifest" || true)
  interface_count=$(rg -c 'datascript/lg/query_types\.mli' "$manifest" || true)
  if [[ "$source_count" -ne "$interface_count" ]]; then
    echo "query-types interface/source count mismatch in $manifest: $interface_count/$source_count" >&2
    exit 1
  fi
done

echo "DataScript query-types interface audit passed"
