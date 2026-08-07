#!/usr/bin/env bash
set -euo pipefail

workspace_root=$(cd "$(dirname "$0")/../.." && pwd)
source_file="$workspace_root/test/datascript/lg/datafy.cljc"
interface_file="$workspace_root/test/datascript/lg/datafy.mil"

if [[ ! -f "$interface_file" ]]; then
  echo "missing DataScript datafy interface: test/datascript/lg/datafy.mil" >&2
  exit 1
fi

if rg -n '^\s*\((type-alias|type-record|type-variant|signature)\b' "$source_file"; then
  echo "datafy implementation still contains type declarations or signatures" >&2
  exit 1
fi

for manifest in "$workspace_root/test/dune" "$workspace_root/test/datascript_conn_tests.inc"; do
  source_count=$(rg -c 'datascript/lg/datafy\.cljc' "$manifest")
  interface_count=$(rg -c 'datascript/lg/datafy\.mil' "$manifest" || true)
  if [[ "$source_count" -ne "$interface_count" ]]; then
    echo "datafy interface/source count mismatch in $manifest: $interface_count/$source_count" >&2
    exit 1
  fi
done

echo "DataScript datafy interface audit passed"
