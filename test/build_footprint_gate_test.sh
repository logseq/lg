#!/bin/sh

set -eu

build_dir=${1:-_build}
total_limit_kib=${LG_BUILD_FOOTPRINT_LIMIT_KIB:-430080}
test_limit_kib=${LG_TEST_BUILD_FOOTPRINT_LIMIT_KIB:-256000}
test_dir="$build_dir/default/test"

if [ ! -d "$test_dir" ]; then
  echo "build footprint gate requires a completed @runtest build: $test_dir" >&2
  exit 1
fi

measure_kib() {
  du -sk "$1" | awk '{ print $1 }'
}

assert_within_limit() {
  label=$1
  actual_kib=$2
  limit_kib=$3
  if [ "$actual_kib" -gt "$limit_kib" ]; then
    printf '%s footprint exceeded: %s KiB > %s KiB\n' \
      "$label" "$actual_kib" "$limit_kib" >&2
    exit 1
  fi
  printf '%s footprint: %s KiB (limit %s KiB)\n' \
    "$label" "$actual_kib" "$limit_kib"
}

assert_within_limit "build" "$(measure_kib "$build_dir")" "$total_limit_kib"
assert_within_limit "test build" "$(measure_kib "$test_dir")" "$test_limit_kib"
