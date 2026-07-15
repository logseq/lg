#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
upstream_root="${UPSTREAM_DATASCRIPT_ROOT:-$(dirname "$repo_root")/datascript}"
checker="$repo_root/script/benchmark_persistent_sorted_set_gate.js"
work_dir="$(mktemp -d)"
trap 'rm -rf "$work_dir"' EXIT

if [ ! -f "$upstream_root/deps.edn" ]; then
  echo "DataScript upstream not found: $upstream_root" >&2
  exit 2
fi

if [ "${BENCH_SKIP_BUILD:-0}" != "1" ]; then
  dune build --profile release \
    test/persistent_sorted_set_benchmark_native.exe \
    @test/persistent-sorted-set-benchmark-melange \
    test/persistent_sorted_set_benchmark_jsoo.bc.js
fi

compile_options="{:target :nodejs
 :optimizations :advanced
 :output-to \"$work_dir/upstream.js\"
 :output-dir \"$work_dir/out\"
 :cache-analysis false}"

(
  cd "$upstream_root"
  clojure \
    -Sdeps "{:paths [\"$repo_root/benchmark/cljs\"]}" \
    -M:cljs -m cljs.main \
    -co "$compile_options" \
    -c lg.benchmark.persistent-sorted-set
)

run() {
  local runtime="$1"
  shift
  printf 'runtime %s\n' "$runtime"
  "$@"
}

output="$({
  run lg-native \
    "$repo_root/_build/default/test/persistent_sorted_set_benchmark_native.exe"
  run lg-melange node \
    "$repo_root/_build/default/test/persistent-sorted-set-benchmark-melange/test/persistent_sorted_set_benchmark_melange.js"
  run lg-js node \
    "$repo_root/_build/default/test/persistent_sorted_set_benchmark_jsoo.bc.js"
  run upstream-cljs node "$work_dir/upstream.js"
})"

printf '%s\n' "$output"
printf '%s\n' "$output" | node "$checker"
