#!/bin/sh

set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
limit_seconds=${LG_COLD_BUILD_LIMIT_SECONDS:-15}
timing_file=$(mktemp "${TMPDIR:-/tmp}/lg-cold-build-timing.XXXXXX")
build_log=$(mktemp "${TMPDIR:-/tmp}/lg-cold-build-log.XXXXXX")
build_root=$(mktemp -d "${TMPDIR:-/tmp}/lg-cold-build.XXXXXX")

cleanup() {
  rm -f "$timing_file" "$build_log"
  rm -rf "$build_root"
}
trap cleanup EXIT HUP INT TERM

cd "$repo_root"

# Wall-clock timings can be perturbed by unrelated host scheduling. Retry only
# failed samples so that the gate rejects sustained regressions, not one noisy run.
samples=""
for attempt in 1 2 3; do
  build_dir="$build_root/build-$attempt"
  if ! /usr/bin/time -p \
    env -u LG_CACHE_DIR -u LG_DISABLE_COMPILE_CACHE \
      dune build --build-dir "$build_dir" stdlib/lg_stdlib_native.state \
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

  samples="${samples}${samples:+, }${elapsed}s"
  if awk -v elapsed="$elapsed" -v limit="$limit_seconds" \
    'BEGIN { exit !(elapsed <= limit) }'; then
    printf 'cold build: %ss (limit %.2fs; samples: %s)\n' \
      "$elapsed" "$limit_seconds" "$samples"
    exit 0
  fi
done

printf 'cold build exceeded limit: all samples [%s] > %.2fs\n' \
  "$samples" "$limit_seconds" >&2
exit 1
