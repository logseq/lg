#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
catalog=${1:-"$repo_root/test/datascript/differential/cases.tsv"}
failures=0
cases=0

if [ ! -f "$catalog" ]; then
  echo "missing differential catalog: $catalog" >&2
  exit 1
fi

while IFS='	' read -r category case_id status upstream native melange evidence; do
  case "$category" in
    ""|\#*) continue ;;
  esac

  cases=$((cases + 1))
  missing_output=false
  for output in "$upstream" "$native" "$melange"; do
    if [ ! -f "$repo_root/$output" ]; then
      echo "$case_id: missing output fixture $output" >&2
      failures=$((failures + 1))
      missing_output=true
    fi
  done

  if [ "$missing_output" = true ]; then
    continue
  fi

  case "$status" in
    parity)
      if ! cmp -s "$repo_root/$upstream" "$repo_root/$native" ||
         ! cmp -s "$repo_root/$upstream" "$repo_root/$melange"; then
        echo "$case_id: parity output differs" >&2
        failures=$((failures + 1))
      fi
      ;;
    known-difference)
      if cmp -s "$repo_root/$upstream" "$repo_root/$native" &&
         cmp -s "$repo_root/$upstream" "$repo_root/$melange"; then
        echo "$case_id: stale known difference; all outputs now match" >&2
        failures=$((failures + 1))
      fi
      if [ -z "$evidence" ] || [ ! -f "$repo_root/$evidence" ]; then
        echo "$case_id: known difference lacks evidence" >&2
        failures=$((failures + 1))
      fi
      ;;
    *)
      echo "$case_id: invalid status $status" >&2
      failures=$((failures + 1))
      ;;
  esac
done < "$catalog"

if [ "$cases" -eq 0 ]; then
  echo "differential catalog is empty" >&2
  exit 1
fi

if [ "$failures" -ne 0 ]; then
  echo "DataScript differential baseline failures: $failures" >&2
  exit 1
fi

echo "DataScript differential baseline: cases=$cases"
