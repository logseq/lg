#!/usr/bin/env bash
set -euo pipefail

workspace_root=$(cd "$(dirname "$0")/../.." && pwd)
review_file="$workspace_root/test/datascript/source_review.tsv"

read -r upstream matched unreviewed < <(
  awk -F '\t' '
    $1 !~ /^#/ {
      upstream += $3
      matched += $4 + $5
      unreviewed += $6
    }
    END { print upstream, matched, unreviewed }
  ' "$review_file"
)

if [[ "$unreviewed" -ne 0 ]]; then
  echo "DataScript implementation review has $unreviewed unreviewed definitions" >&2
  exit 1
fi

percentage=$(awk -v matched="$matched" -v upstream="$upstream" \
  'BEGIN { printf "%.1f", 100 * matched / upstream }')

if awk -v matched="$matched" -v upstream="$upstream" \
  'BEGIN { exit !((100 * matched / upstream) < 95) }'; then
  echo "DataScript real implementation match is below 95%: $matched/$upstream ($percentage%)" >&2
  exit 1
fi

echo "DataScript real implementation audit passed: $matched/$upstream ($percentage%), unreviewed=$unreviewed"
