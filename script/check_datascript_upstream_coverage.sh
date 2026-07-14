#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
manifest="$repo_root/test/datascript/upstream_tests.tsv"
status_file="$repo_root/test/datascript/upstream_status.tsv"
require_complete=false
upstream=

while [ "$#" -gt 0 ]; do
  case "$1" in
    --require-complete)
      require_complete=true
      ;;
    --upstream)
      shift
      if [ "$#" -eq 0 ]; then
        echo "--upstream requires a path" >&2
        exit 2
      fi
      upstream=$1
      ;;
    *)
      echo "usage: $0 [--require-complete] [--upstream /path/to/datascript]" >&2
      exit 2
      ;;
  esac
  shift
done

for file in "$manifest" "$status_file"; do
  if [ ! -f "$file" ]; then
    echo "missing coverage file: $file" >&2
    exit 1
  fi
done

tmp_dir=${TMPDIR:-/tmp}/lg-datascript-coverage.$$
mkdir -p "$tmp_dir"
trap 'rm -rf "$tmp_dir"' EXIT HUP INT TERM

manifest_ids="$tmp_dir/manifest.ids"
status_ids="$tmp_dir/status.ids"

awk -F '\t' '
  NF != 2 || ($1 != "native" && $1 != "melange" && $1 != "both") || $2 == "" {
    print "invalid manifest row " NR > "/dev/stderr"
    failed = 1
  }
  { print $2 }
  END { if (failed) exit 1 }
' "$manifest" | LC_ALL=C sort > "$manifest_ids"

if [ "$(uniq -d "$manifest_ids" | wc -l | tr -d ' ')" -ne 0 ]; then
  echo "duplicate upstream test ids:" >&2
  uniq -d "$manifest_ids" >&2
  exit 1
fi

awk -F '\t' '
  NF != 4 || ($2 != "covered" && $2 != "missing" && $2 != "excluded") {
    print "invalid status row " NR > "/dev/stderr"
    failed = 1
  }
  $2 == "covered" && ($3 == "" || $3 == "-") {
    print "covered row lacks evidence at line " NR > "/dev/stderr"
    failed = 1
  }
  $2 == "excluded" && $4 == "" {
    print "excluded row lacks a reason at line " NR > "/dev/stderr"
    failed = 1
  }
  { print $1 }
  END { if (failed) exit 1 }
' "$status_file" | LC_ALL=C sort > "$status_ids"

awk -F '\t' '$2 == "covered" { print $3 }' "$status_file" |
while IFS= read -r evidence; do
  if [ ! -e "$repo_root/$evidence" ]; then
    echo "covered evidence does not exist: $evidence" >&2
    exit 1
  fi
done

if [ "$(uniq -d "$status_ids" | wc -l | tr -d ' ')" -ne 0 ]; then
  echo "duplicate status ids:" >&2
  uniq -d "$status_ids" >&2
  exit 1
fi

comm -23 "$manifest_ids" "$status_ids" > "$tmp_dir/unaccounted"
comm -13 "$manifest_ids" "$status_ids" > "$tmp_dir/stale"

if [ -s "$tmp_dir/unaccounted" ] || [ -s "$tmp_dir/stale" ]; then
  if [ -s "$tmp_dir/unaccounted" ]; then
    echo "unaccounted upstream tests:" >&2
    cat "$tmp_dir/unaccounted" >&2
  fi
  if [ -s "$tmp_dir/stale" ]; then
    echo "stale status entries:" >&2
    cat "$tmp_dir/stale" >&2
  fi
  exit 1
fi

if [ -n "$upstream" ]; then
  sh "$repo_root/script/list_datascript_upstream_tests.sh" "$upstream" \
    | LC_ALL=C sort > "$tmp_dir/live.tsv"
  LC_ALL=C sort "$manifest" > "$tmp_dir/snapshot.tsv"
  if ! cmp -s "$tmp_dir/live.tsv" "$tmp_dir/snapshot.tsv"; then
    echo "upstream test manifest is stale:" >&2
    diff -u "$tmp_dir/snapshot.tsv" "$tmp_dir/live.tsv" >&2 || true
    exit 1
  fi
fi

covered=$(awk -F '\t' '$2 == "covered" { count++ } END { print count + 0 }' "$status_file")
missing=$(awk -F '\t' '$2 == "missing" { count++ } END { print count + 0 }' "$status_file")
excluded=$(awk -F '\t' '$2 == "excluded" { count++ } END { print count + 0 }' "$status_file")
total=$(wc -l < "$manifest" | tr -d ' ')

echo "DataScript upstream coverage: total=$total covered=$covered missing=$missing excluded=$excluded"

if [ "$require_complete" = true ] && [ "$missing" -ne 0 ]; then
  echo "coverage is incomplete; $missing upstream tests remain" >&2
  exit 1
fi
