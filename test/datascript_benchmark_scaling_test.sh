#!/usr/bin/env bash
set -euo pipefail

benchmark_executable=$1
if [[ "$benchmark_executable" != */* ]]; then
  benchmark_executable="./$benchmark_executable"
fi

run_add_all() {
  local people_count=$1
  LG_BENCHMARK=add-all \
    LG_BENCH_PEOPLE="$people_count" \
    LG_BENCH_WARMUP_MS=0 \
    LG_BENCH_SAMPLE_MS=0 \
    LG_BENCH_BATCH=1 \
    LG_BENCH_SEED=42 \
    "$benchmark_executable" |
    awk -F: '/^add-all:/ { print $2 }'
}

run_once() {
  local benchmark_name=$1
  LG_BENCHMARK="$benchmark_name" \
    LG_BENCH_PEOPLE=1000 \
    LG_BENCH_WARMUP_MS=0 \
    LG_BENCH_SAMPLE_MS=0 \
    LG_BENCH_BATCH=1 \
    LG_BENCH_SEED=42 \
    "$benchmark_executable" |
    awk -F: -v benchmark_name="$benchmark_name" \
      '$1 == benchmark_name { print $2 }'
}

small_ms=$(run_add_all 1000)
large_ms=$(run_add_all 4000)

awk -v small_ms="$small_ms" -v large_ms="$large_ms" '
  BEGIN {
    ratio = large_ms / small_ms
    if (ratio >= 7.0) {
      printf "add-all scales superlinearly: 1000=%sms 4000=%sms ratio=%.2f\n",
        small_ms, large_ms, ratio
      exit 1
    }
    printf "add-all scaling: 1000=%sms 4000=%sms ratio=%.2f\n",
      small_ms, large_ms, ratio
  }
'

for benchmark_name in \
  q1 q2 q3 q4 q5-shortcircuit qpred1 qpred2 \
  pull-one pull-many pull-wildcard \
  freeze thaw; do
  benchmark_ms=$(run_once "$benchmark_name")
  awk -v benchmark_name="$benchmark_name" -v benchmark_ms="$benchmark_ms" '
    BEGIN {
      if (benchmark_ms < 0) {
        printf "missing upstream benchmark case: %s\n", benchmark_name
        exit 1
      }
      if (benchmark_name == "q5-shortcircuit" && benchmark_ms >= 1.0) {
        printf "query input failed to short-circuit: %s=%sms\n",
          benchmark_name, benchmark_ms
        exit 1
      }
      printf "benchmark case available: %s=%sms\n",
        benchmark_name, benchmark_ms
    }
  '
done
