#!/usr/bin/env bash
set -euo pipefail

melange_javascript=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")

run_benchmark() {
  local workload=$1

  LG_BENCHMARK="$workload" \
  LG_BENCH_PEOPLE=20000 \
  LG_BENCH_WARMUP_MS=2000 \
  LG_BENCH_SAMPLE_MS=1000 \
  LG_BENCH_BATCH=10 \
  LG_BENCH_SEED=42 \
    node "$melange_javascript" |
    awk -F: -v workload="$workload" '$1 == workload { print $2 }'
}

assert_upstream_performance() {
  local workload=$1
  local upstream_ms=$2
  local first_ms
  local second_ms
  local third_ms
  local median_ms

  first_ms=$(run_benchmark "$workload")
  second_ms=$(run_benchmark "$workload")
  third_ms=$(run_benchmark "$workload")
  median_ms=$(
    printf '%s\n' "$first_ms" "$second_ms" "$third_ms" |
      sort -n |
      sed -n '2p'
  )

  if [[ -z "$first_ms" || -z "$second_ms" || -z "$third_ms" ]]; then
    printf 'Melange %s performance gate produced no sample\n' "$workload"
    exit 1
  fi

  awk -v workload="$workload" \
    -v median_ms="$median_ms" \
    -v upstream_ms="$upstream_ms" '
    BEGIN {
      if (median_ms > upstream_ms) {
        printf "Melange %s remains slower than upstream: median=%sms upstream=%sms\n",
          workload, median_ms, upstream_ms
        exit 1
      }
      printf "Melange %s performance gate: median=%sms upstream=%sms\n",
        workload, median_ms, upstream_ms
    }
  '
}

failures=0

for workload_and_upstream in \
  "q1 1.5" \
  "q2 3.5" \
  "q3 5.2" \
  "q4 7.6" \
  "q5-shortcircuit 0.784" \
  "qpred1 7.4" \
  "qpred2 8.7"
do
  read -r workload upstream_ms <<< "$workload_and_upstream"
  if ! assert_upstream_performance "$workload" "$upstream_ms"; then
    failures=1
  fi
done

exit "$failures"
