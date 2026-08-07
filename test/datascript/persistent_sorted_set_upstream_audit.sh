#!/usr/bin/env bash
set -euo pipefail

workspace_root=$(cd "$(dirname "$0")/../.." && pwd)
source_file="$workspace_root/datascript/me/tonsky/persistent_sorted_set.cljc"

require_definition() {
  local name=$1
  if ! PSS_NAME="$name" perl -0777 -ne \
    '$found = 1 if /\(defn-? \Q$ENV{PSS_NAME}\E(?:\s|\[)/; END {exit($found ? 0 : 1)}' \
    "$source_file"; then
    echo "missing upstream PSS helper: $name" >&2
    exit 1
  fi
}

for name in arr-map-inplace arr-partition-approx sorted-arr-distinct? sorted-arr-distinct \
  -rpath -next-path -prev-path '-seek*' '-rseek*' -slice; do
  require_definition "$name"
done

for name in alter-btset keys-for btset-iter iter riter sorted-set-by sorted-set; do
  require_definition "$name"
done

require_definition walk-addresses
require_definition restore

for name in -distance distance est-count; do
  require_definition "$name"
done

rg -Fq '(def avg-len ' "$source_file"
rg -Fq '(>= remaining (+ avg-len minimum))' "$source_file"

rg -q '\(arr-partition-approx min-len max-len values\)' "$source_file"
rg -q '\(sorted-arr-distinct sorted cmp\)' "$source_file"
rg -q '\(-next-path root current-path shift storage\)' "$source_file"
rg -q '\(-prev-path root current-path shift storage\)' "$source_file"
test "$(rg -F '(-seek*' "$source_file" | wc -l | tr -d ' ')" -ge 3
test "$(rg -F '(-rseek*' "$source_file" | wc -l | tr -d ' ')" -ge 3
test "$(rg -F '(alter-btset' "$source_file" | wc -l | tr -d ' ')" -ge 3
rg -q '\(iter set left right\)' "$source_file"
rg -q '\(riter set left right\)' "$source_file"

echo "PSS upstream helper audit passed"
