#!/bin/sh
set -eu

root=$1
source_file="$root/stdlib/clojure/core.cljc"

for function in make-hierarchy isa? parents ancestors descendants derive underive; do
  if ! grep -F "(defn $function" "$source_file" >/dev/null; then
    echo "clojure.core/$function is not source-defined" >&2
    exit 1
  fi

  escaped_function=$(printf '%s' "$function" | sed 's/\?/\\?/g')
  if grep -E "^[[:space:]]*\\| \"(clojure.core/|cljs.core/)?${escaped_function}\"" \
    "$root/src/call_elaborator.ml" >/dev/null; then
    echo "clojure.core/$function is still compiler-dispatched" >&2
    exit 1
  fi
done

status_file=$(mktemp)
trap 'rm -f "$status_file"' EXIT HUP INT TERM
bb "$root/script/extract_stdlib_manifest_status.clj" \
  "$root/stdlib/upstream.edn" >"$status_file"

for function in make-hierarchy isa? parents ancestors descendants derive underive; do
  if ! awk -F '\t' -v name="clojure.core/$function" \
    '$1 == "definition" && $2 == name && $3 == "source" {found = 1} END {exit !found}' \
    "$status_file"; then
    echo "clojure.core/$function is not classified as source" >&2
    exit 1
  fi
done
