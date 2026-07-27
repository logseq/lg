#!/usr/bin/env bash
set -euo pipefail

native_executable=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
melange_javascript=$(cd "$(dirname "$2")" && pwd)/$(basename "$2")

run_native_once() {
  local benchmark_name=$1
  local people_count=${2:-20000}
  LG_BENCHMARK="$benchmark_name" \
    LG_BENCH_PEOPLE="$people_count" \
    LG_BENCH_WARMUP_MS=0 \
    LG_BENCH_SAMPLE_MS=0 \
    LG_BENCH_BATCH=1 \
    LG_BENCH_SEED=42 \
    "$native_executable" |
    awk -F: -v benchmark_name="$benchmark_name" \
      '$1 == benchmark_name { print $2 }'
}

run_melange_once() {
  local benchmark_name=$1
  local people_count=${2:-20000}
  LG_BENCHMARK="$benchmark_name" \
    LG_BENCH_PEOPLE="$people_count" \
    LG_BENCH_WARMUP_MS=0 \
    LG_BENCH_SAMPLE_MS=0 \
    LG_BENCH_BATCH=1 \
    LG_BENCH_SEED=42 \
    node "$melange_javascript" |
    awk -F: -v benchmark_name="$benchmark_name" \
      '$1 == benchmark_name { print $2 }'
}

run_native_serialization_once() {
  local benchmark_name=$1
  local people_count=$2
  LG_BENCHMARK="$benchmark_name" \
    LG_BENCH_SERIALIZE_PEOPLE="$people_count" \
    LG_BENCH_WARMUP_MS=0 \
    LG_BENCH_SAMPLE_MS=0 \
    LG_BENCH_BATCH=1 \
    LG_BENCH_SEED=42 \
    "$native_executable" |
    awk -F: -v benchmark_name="$benchmark_name" \
      '$1 == benchmark_name { print $2 }'
}

run_melange_serialization_once() {
  local benchmark_name=$1
  local people_count=$2
  LG_BENCHMARK="$benchmark_name" \
    LG_BENCH_SERIALIZE_PEOPLE="$people_count" \
    LG_BENCH_WARMUP_MS=0 \
    LG_BENCH_SAMPLE_MS=0 \
    LG_BENCH_BATCH=1 \
    LG_BENCH_SEED=42 \
    node "$melange_javascript" |
    awk -F: -v benchmark_name="$benchmark_name" \
      '$1 == benchmark_name { print $2 }'
}

native_init_ms=$(run_native_once init)
melange_init_ms=$(run_melange_once init)

failures=0

if ! awk -v native_ms="$native_init_ms" -v melange_ms="$melange_init_ms" '
  BEGIN {
    ratio = melange_ms / native_ms
    if (ratio >= 8.0) {
      printf "Melange init misses the native-array sort boundary: native=%sms melange=%sms ratio=%.2f\n",
        native_ms, melange_ms, ratio
      exit 1
    }
    printf "init cross-runtime scaling: native=%sms melange=%sms ratio=%.2f\n",
      native_ms, melange_ms, ratio
  }
'; then
  failures=1
fi

check_rule_scaling() {
  local runtime_name=$1
  local wide_5_ms=$2
  local wide_7_ms=$3
  awk \
    -v runtime_name="$runtime_name" \
    -v wide_5_ms="$wide_5_ms" \
    -v wide_7_ms="$wide_7_ms" '
    BEGIN {
      ratio = wide_7_ms / wide_5_ms
      if (ratio >= 25.0) {
        printf "%s rule expansion scales unlike upstream: wide-5x3=%sms wide-7x3=%sms ratio=%.2f\n",
          runtime_name, wide_5_ms, wide_7_ms, ratio
        exit 1
      }
      printf "%s rule expansion scaling: wide-5x3=%sms wide-7x3=%sms ratio=%.2f\n",
        runtime_name, wide_5_ms, wide_7_ms, ratio
    }
  '
}

native_wide_5_ms=$(run_native_once rules-wide-5x3)
native_wide_7_ms=$(run_native_once rules-wide-7x3)
melange_wide_5_ms=$(run_melange_once rules-wide-5x3)
melange_wide_7_ms=$(run_melange_once rules-wide-7x3)

if ! check_rule_scaling native "$native_wide_5_ms" "$native_wide_7_ms"; then
  failures=1
fi
if ! check_rule_scaling melange "$melange_wide_5_ms" "$melange_wide_7_ms"; then
  failures=1
fi

native_freeze_ms=$(run_native_serialization_once freeze 300000)
melange_freeze_ms=$(run_melange_serialization_once freeze 300000)

if ! awk -v native_ms="$native_freeze_ms" -v melange_ms="$melange_freeze_ms" '
  BEGIN {
    ratio = melange_ms / native_ms
    if (ratio >= 4.0) {
      printf "Melange freeze copies the closed serialization tree: native=%sms melange=%sms ratio=%.2f\n",
        native_ms, melange_ms, ratio
      exit 1
    }
    printf "freeze cross-runtime scaling: native=%sms melange=%sms ratio=%.2f\n",
      native_ms, melange_ms, ratio
  }
'; then
  failures=1
fi

exit "$failures"
