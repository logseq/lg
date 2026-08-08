#!/bin/sh

set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
limit_seconds=${LG_COLD_BUILD_LIMIT_SECONDS:-10}
timing_file=$(mktemp "${TMPDIR:-/tmp}/lg-cold-build-timing.XXXXXX")
build_log=$(mktemp "${TMPDIR:-/tmp}/lg-cold-build-log.XXXXXX")

cleanup() {
  rm -f "$timing_file" "$build_log"
}
trap cleanup EXIT HUP INT TERM

cd "$repo_root"
dune clean

if ! /usr/bin/time -p \
  env LG_DISABLE_COMPILE_CACHE=1 \
  dune build test/datascript_conn_native_runtime.ml \
  >"$build_log" 2>"$timing_file"; then
  cat "$build_log" >&2
  cat "$timing_file" >&2
  exit 1
fi

elapsed=$(awk '$1 == "real" { print $2 }' "$timing_file")
if [ -z "$elapsed" ]; then
  echo "cold build gate did not capture elapsed time" >&2
  cat "$timing_file" >&2
  exit 1
fi

awk -v elapsed="$elapsed" -v limit="$limit_seconds" 'BEGIN {
  if (elapsed > limit) {
    printf "cold build exceeded limit: %.2fs > %.2fs\n", elapsed, limit > "/dev/stderr"
    exit 1
  }
  printf "cold build: %.2fs (limit %.2fs)\n", elapsed, limit
}'
