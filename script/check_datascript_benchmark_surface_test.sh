#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
native_runner="$repo_root/_build/default/test/datascript_benchmark_native.exe"
melange_runner="$repo_root/_build/default/test/datascript-conn-melange/test/datascript_benchmark_melange.js"
tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

expected_workloads=(
  add-1
  add-5
  add-all
  init
  find-datoms
  find-datom
  retract-5
  q1
  q2
  q3
  q4
  q5-shortcircuit
  qpred1
  qpred2
  pull-one-entities
  pull-one
  pull-many-entities
  pull-many
  pull-wildcard
  rules-wide-3x3
  rules-wide-5x3
  rules-wide-7x3
  rules-wide-4x6
  rules-long-10x3
  rules-long-30x3
  rules-long-30x5
  freeze
  thaw
)

benchmark_source="$repo_root/test/datascript/benchmark/datascript/bench/datascript.cljc"
thaw_restore_calls="$(
  awk '
    /^\(defn bench-thaw/ { in_thaw = 1; next }
    in_thaw && /^\(defn / { exit }
    in_thaw && /d\/from-serializable/ { calls++ }
    END { print calls + 0 }
  ' "$benchmark_source"
)"
if [ "$thaw_restore_calls" -ne 1 ]; then
  echo "bench-thaw must preserve upstream single-restore control flow: found $thaw_restore_calls calls" >&2
  exit 1
fi

check_runtime() {
  local runtime="$1"
  shift
  local output="$tmp_dir/$runtime.out"

  LG_BENCH_PEOPLE=100 \
  LG_BENCH_SERIALIZE_PEOPLE=100 \
  LG_BENCH_WARMUP_MS=0 \
  LG_BENCH_SAMPLE_MS=0 \
  LG_BENCH_BATCH=1 \
  LG_BENCH_SEED=42 \
    "$@" > "$output"

  for workload in "${expected_workloads[@]}"; do
    local value
    value="$(awk -F: -v workload="$workload" '$1 == workload { print $2 }' "$output")"
    if [ -z "$value" ]; then
      echo "$runtime benchmark output is missing $workload" >&2
      exit 1
    fi
    if ! awk -v value="$value" \
      'BEGIN { exit !(value ~ /^[0-9]+([.][0-9]*)?([eE][-+]?[0-9]+)?$/ && value >= 0) }'
    then
      echo "$runtime benchmark $workload returned invalid time: $value" >&2
      exit 1
    fi
  done

  local serialization_default
  serialization_default="$(
    LG_BENCH_WARMUP_MS=0 \
    LG_BENCH_SAMPLE_MS=0 \
    LG_BENCH_BATCH=1 \
    LG_BENCH_SEED=42 \
    LG_BENCHMARK=serialization-people-count \
      "$@"
  )"
  if ! awk -F: \
    '$1 == "serialization-people-count" && $2 == 300000 { found = 1 } END { exit !found }' \
    <<< "$serialization_default"
  then
    echo "$runtime benchmark uses the wrong default serialization population: $serialization_default" >&2
    exit 1
  fi

  local serialization_scaled
  serialization_scaled="$(
    LG_BENCH_SERIALIZE_PEOPLE=100 \
    LG_BENCH_WARMUP_MS=0 \
    LG_BENCH_SAMPLE_MS=0 \
    LG_BENCH_BATCH=1 \
    LG_BENCH_SEED=42 \
    LG_BENCHMARK=serialization-people-count \
      "$@"
  )"
  if ! awk -F: \
    '$1 == "serialization-people-count" && $2 == 100 { found = 1 } END { exit !found }' \
    <<< "$serialization_scaled"
  then
    echo "$runtime benchmark ignored the serialization population override: $serialization_scaled" >&2
    exit 1
  fi

  if LG_BENCH_SERIALIZE_PEOPLE=invalid \
     LG_BENCHMARK=serialization-people-count \
       "$@" > /dev/null 2>&1
  then
    echo "$runtime benchmark accepted an invalid serialization population" >&2
    exit 1
  fi

  local unknown_output
  if ! unknown_output="$(
    LG_BENCH_PEOPLE=10 \
    LG_BENCH_WARMUP_MS=0 \
    LG_BENCH_SAMPLE_MS=0 \
    LG_BENCH_BATCH=1 \
    LG_BENCH_SEED=42 \
    LG_BENCHMARK=not-a-benchmark \
      "$@"
  )"
  then
    echo "$runtime benchmark runner rejected an unknown workload" >&2
    exit 1
  fi
  if [ "$unknown_output" != "Unknown benchmark: not-a-benchmark" ]; then
    echo "$runtime benchmark runner did not match upstream unknown-workload output: $unknown_output" >&2
    exit 1
  fi
}

if [ ! -x "$native_runner" ]; then
  echo "Native DataScript benchmark runner is missing: $native_runner" >&2
  exit 2
fi
if [ ! -f "$melange_runner" ]; then
  echo "Melange DataScript benchmark runner is missing: $melange_runner" >&2
  exit 2
fi

check_runtime native "$native_runner"
check_runtime melange node "$melange_runner"

echo "DataScript benchmark surface matches the pinned upstream workload set"
