#!/usr/bin/env bash
set -euo pipefail

native_executable=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
melange_javascript=$(cd "$(dirname "$2")" && pwd)/$(basename "$2")

run_native_freeze() {
  LG_BENCHMARK=freeze \
  LG_BENCH_SERIALIZE_PEOPLE=100000 \
  LG_BENCH_WARMUP_MS=0 \
  LG_BENCH_SAMPLE_MS=0 \
  LG_BENCH_BATCH=1 \
  LG_BENCH_SEED=42 \
    "$native_executable" |
    awk -F: '$1 == "freeze" { print $2 }'
}

run_melange_freeze() {
  LG_BENCHMARK=freeze \
  LG_BENCH_SERIALIZE_PEOPLE=100000 \
  LG_BENCH_WARMUP_MS=0 \
  LG_BENCH_SAMPLE_MS=0 \
  LG_BENCH_BATCH=1 \
  LG_BENCH_SEED=42 \
    node "$melange_javascript" |
    awk -F: '$1 == "freeze" { print $2 }'
}

median_of_five() {
  printf '%s\n' "$1" "$2" "$3" "$4" "$5" |
    sort -n |
    sed -n '3p'
}

assert_performance() {
  local runtime=$1
  local limit_ms=$2
  local runner=$3
  local first_ms
  local second_ms
  local third_ms
  local fourth_ms
  local fifth_ms
  local median_ms

  first_ms=$("$runner")
  second_ms=$("$runner")
  third_ms=$("$runner")
  fourth_ms=$("$runner")
  fifth_ms=$("$runner")

  if [[ -z "$first_ms" || -z "$second_ms" || -z "$third_ms" ||
        -z "$fourth_ms" || -z "$fifth_ms" ]]; then
    printf '%s freeze performance gate produced no sample\n' "$runtime"
    return 1
  fi

  median_ms=$(median_of_five \
    "$first_ms" "$second_ms" "$third_ms" "$fourth_ms" "$fifth_ms")
  awk -v runtime="$runtime" -v median_ms="$median_ms" -v limit_ms="$limit_ms" '
    BEGIN {
      if (median_ms > limit_ms) {
        printf "%s freeze remains above the focused limit: median=%sms limit=%sms\n",
          runtime, median_ms, limit_ms
        exit 1
      }
      printf "%s freeze performance gate: median=%sms limit=%sms\n",
        runtime, median_ms, limit_ms
    }
  '
}

failures=0

if ! assert_performance Native 390 run_native_freeze; then
  failures=1
fi

if ! assert_performance Melange 295 run_melange_freeze; then
  failures=1
fi

exit "$failures"
