#!/bin/sh
set -eu
cli="$1"
stdlib_state="$2"
test_dir=$(mktemp -d "${TMPDIR:-/tmp}/lg-checkpoint-cache.XXXXXX")
trap 'rm -rf "$test_dir"' EXIT
for mode in plain saved; do
  cache="$test_dir/$mode"
  mkdir -p "$cache"
  a="$cache/a.cljc"
  b="$cache/b.cljc"
  c="$cache/c.cljc"
  printf '%s\n' '(def first-value 40)' > "$a"
  printf '%s\n' '(def second-value first-value)' > "$b"
  printf '%s\n' '(def third-value second-value)' > "$c"
  compile() {
    if [ "$mode" = saved ]; then
      "$cli" --compile-files-from "$stdlib_state" "$@"
    else
      "$cli" --compile-files "$@"
    fi
  }
  export LG_CACHE_DIR="$cache/cache" LG_COMPILE_CACHE_MIN_SECONDS=100000
  compile "$a" "$b" "$c" -o "$cache/cold.ml"
  states=$(find "$LG_CACHE_DIR" -name '*.state.marshal' | wc -l | tr -d ' ')
  if [ "$states" != 1 ]; then
    echo "small files must share one final checkpoint; found $states ($mode)" >&2
    exit 1
  fi
  LG_COMPILE_CACHE_DEBUG=1 compile "$a" "$b" "$c" -o "$cache/warm.ml" 2> "$cache/warm.err"
  cmp "$cache/cold.ml" "$cache/warm.ml"
  for source in "$a" "$b" "$c"; do
    grep -q "compile cache hit: $source" "$cache/warm.err"
  done
  # A changed suffix must replay the uncheckpointed files before compiling it.
  printf '%s\n' '(def third-value first-value)' > "$c"
  compile "$a" "$b" "$c" -o "$cache/changed.ml"
  LG_DISABLE_COMPILE_CACHE=1 compile "$a" "$b" "$c" -o "$cache/uncached.ml"
  cmp "$cache/changed.ml" "$cache/uncached.ml"
  # Appending a source resumes from the final checkpoint of the old invocation.
  d="$cache/d.cljc"
  printf '%s\n' '(def fourth-value third-value)' > "$d"
  LG_COMPILE_CACHE_DEBUG=1 compile "$a" "$b" "$c" "$d" -o "$cache/appended.ml" 2> "$cache/appended.err"
  grep -q "compile cache hit: $c" "$cache/appended.err"
  LG_DISABLE_COMPILE_CACHE=1 compile "$a" "$b" "$c" "$d" -o "$cache/uncached.ml"
  cmp "$cache/appended.ml" "$cache/uncached.ml"
  # A damaged checkpoint must fall back to an earlier valid state or rebuild.
  find "$LG_CACHE_DIR" -name '*.state.marshal' -exec sh -c 'printf corrupt > "$1"' sh {} \;
  LG_COMPILE_CACHE_DEBUG=1 compile "$a" "$b" "$c" "$d" -o "$cache/rebuilt.ml" 2> "$cache/rebuilt.err"
  cmp "$cache/rebuilt.ml" "$cache/uncached.ml"
  grep -q 'ignored corrupt entry' "$cache/rebuilt.err"
done
