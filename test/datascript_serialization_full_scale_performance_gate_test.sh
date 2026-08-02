#!/usr/bin/env bash
set -euo pipefail

native_executable=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
melange_javascript=$(cd "$(dirname "$2")" && pwd)/$(basename "$2")
selected_workload=${3:-all}

run_native() {
  local workload=$1

  LG_BENCHMARK="$workload" \
  LG_BENCH_SERIALIZE_PEOPLE=300000 \
  LG_BENCH_WARMUP_MS=0 \
  LG_BENCH_SAMPLE_MS=0 \
  LG_BENCH_BATCH=1 \
  LG_BENCH_SEED=42 \
    "$native_executable" |
    awk -F: -v workload="$workload" '$1 == workload { print $2 }'
}

run_melange() {
  local workload=$1

  LG_BENCHMARK="$workload" \
  LG_BENCH_SERIALIZE_PEOPLE=300000 \
  LG_BENCH_WARMUP_MS=0 \
  LG_BENCH_SAMPLE_MS=0 \
  LG_BENCH_BATCH=1 \
  LG_BENCH_SEED=42 \
    node --max-old-space-size=8192 "$melange_javascript" |
    awk -F: -v workload="$workload" '$1 == workload { print $2 }'
}

median_of_three() {
  printf '%s\n' "$1" "$2" "$3" |
    sort -n |
    sed -n '2p'
}

assert_performance() {
  local runtime=$1
  local workload=$2
  local limit_ms=$3
  local runner=$4
  local first_ms
  local second_ms
  local third_ms
  local median_ms

  first_ms=$("$runner" "$workload")
  second_ms=$("$runner" "$workload")
  third_ms=$("$runner" "$workload")

  if [[ -z "$first_ms" || -z "$second_ms" || -z "$third_ms" ]]; then
    printf '%s %s full-scale gate produced no sample\n' "$runtime" "$workload"
    return 1
  fi

  median_ms=$(median_of_three "$first_ms" "$second_ms" "$third_ms")
  awk -v runtime="$runtime" \
    -v workload="$workload" \
    -v median_ms="$median_ms" \
    -v limit_ms="$limit_ms" '
    BEGIN {
      if (median_ms > limit_ms) {
        printf "%s %s remains slower than pinned upstream: median=%sms upstream=%sms\n",
          runtime, workload, median_ms, limit_ms
        exit 1
      }
      printf "%s %s full-scale gate: median=%sms upstream=%sms\n",
        runtime, workload, median_ms, limit_ms
    }
  '
}

failures=0

if [[ "$selected_workload" == "all" || "$selected_workload" == "freeze" ]]; then
  if ! assert_performance Native freeze 680.8 run_native; then
    failures=1
  fi
  if ! assert_performance Melange freeze 680.8 run_melange; then
    failures=1
  fi
fi

if [[ "$selected_workload" == "all" || "$selected_workload" == "thaw" ]]; then
  if ! assert_performance Native thaw 1134.9 run_native; then
    failures=1
  fi
  if ! assert_performance Melange thaw 1134.9 run_melange; then
    failures=1
  fi
fi

if [[ "$selected_workload" != "all" && \
      "$selected_workload" != "freeze" && \
      "$selected_workload" != "thaw" ]]; then
  printf 'unknown serialization workload: %s\n' "$selected_workload" >&2
  exit 2
fi

exit "$failures"
