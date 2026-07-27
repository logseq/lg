#!/bin/sh
set -eu

if [ "$#" -ne 2 ]; then
  echo "usage: $0 UPSTREAM_MANIFEST LG_MANIFEST" >&2
  exit 2
fi

upstream=$1
lg=$2

for manifest in "$upstream" "$lg"; do
  if [ ! -f "$manifest" ]; then
    echo "missing API manifest: $manifest" >&2
    exit 1
  fi
done

tmp_dir=${TMPDIR:-/tmp}/lg-datascript-api.$$
mkdir -p "$tmp_dir"
trap 'rm -rf "$tmp_dir"' EXIT HUP INT TERM

LC_ALL=C sort -u "$upstream" > "$tmp_dir/upstream"
LC_ALL=C sort -u "$lg" > "$tmp_dir/lg"
comm -23 "$tmp_dir/upstream" "$tmp_dir/lg" > "$tmp_dir/missing"

if [ -s "$tmp_dir/missing" ]; then
  echo "LG is missing or narrows the following upstream API entries:" >&2
  cat "$tmp_dir/missing" >&2
  exit 1
fi

echo "DataScript API manifest contains every upstream entry"
