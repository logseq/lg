#!/usr/bin/env bash
set -euo pipefail

melange_javascript=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")

run_qpred2() {
  LG_BENCHMARK=qpred2 \
  LG_BENCH_PEOPLE=20000 \
  LG_BENCH_WARMUP_MS=2000 \
  LG_BENCH_SAMPLE_MS=1000 \
  LG_BENCH_BATCH=10 \
  LG_BENCH_SEED=42 \
    node "$melange_javascript" |
    awk -F: '$1 == "qpred2" { print $2 }'
}

first_ms=$(run_qpred2)
second_ms=$(run_qpred2)
third_ms=$(run_qpred2)
median_ms=$(
  printf '%s\n' "$first_ms" "$second_ms" "$third_ms" |
    sort -n |
    sed -n '2p'
)

awk -v median_ms="$median_ms" '
  BEGIN {
    upstream_ms = 9.7
    if (median_ms > upstream_ms) {
      printf "Melange qpred2 remains slower than upstream: median=%sms upstream=%sms\n",
        median_ms, upstream_ms
      exit 1
    }
    printf "Melange qpred2 performance gate: median=%sms upstream=%sms\n",
      median_ms, upstream_ms
  }
'
