#!/usr/bin/env bash
set -euo pipefail

workspace_root=$(cd "$(dirname "$0")/../.." && pwd)

for stem in query query_v3; do
  source_file="$workspace_root/test/datascript/lg/$stem.cljc"
  interface_file="$workspace_root/test/datascript/lg/$stem.lgi"
  if [[ ! -f "$interface_file" ]]; then
    echo "missing DataScript query interface: test/datascript/lg/$stem.lgi" >&2
    exit 1
  fi
  if rg -n '^\s*\((type-alias|type-record|type-variant|signature)\b' "$source_file"; then
    echo "$stem implementation still contains movable type declarations" >&2
    exit 1
  fi
  for manifest in "$workspace_root/test/dune" "$workspace_root/test/datascript_conn_tests.inc" "$workspace_root/test/compiler_tests.ml"; do
    source_count=$(rg -c "datascript/lg/$stem\\.cljc" "$manifest" || true)
    interface_count=$(rg -c "datascript/lg/$stem\\.lgi" "$manifest" || true)
    if [[ "$source_count" -ne "$interface_count" ]]; then
      echo "$stem interface/source count mismatch in $manifest: $interface_count/$source_count" >&2
      exit 1
    fi
  done
done

echo "DataScript query interface audit passed"
