#!/bin/sh
set -eu

root=$1
source_file="$root/stdlib/clojure/zip.cljc"
interface_file="$root/stdlib/clojure/zip.mil"

for file in "$source_file" "$interface_file"; do
  if ! test -f "$file"; then
    echo "${file#"$root/"} is missing from the source standard library" >&2
    exit 1
  fi
done

for function in zipper seq-zip vector-zip xml-zip node branch? children make-node path lefts rights down up root right rightmost left leftmost insert-left insert-right replace edit insert-child append-child next prev end? remove; do
  if ! grep -F "(defn $function [" "$source_file" >/dev/null; then
    echo "clojure.zip/$function is not source-defined" >&2
    exit 1
  fi
done

if sed -n '/let is_core_namespace/,/let lookup_qualified_member/p' \
  "$root/src/core_namespaces.ml" | grep -F '"clojure.zip"' >/dev/null; then
  echo "clojure.zip is compiler-owned" >&2
  exit 1
fi

if grep -E '^[[:space:]]+\((current|path-value|context|left|right|changed)[[:space:]]' \
  "$interface_file" >/dev/null; then
  echo "clojure.zip internal fields can collide with consumer record inference" >&2
  exit 1
fi

status_file=$(mktemp)
trap 'rm -f "$status_file"' EXIT HUP INT TERM
bb "$root/script/extract_stdlib_manifest_status.clj" \
  "$root/stdlib/upstream.edn" >"$status_file"

for function in zipper node down up root right replace next end?; do
  if ! awk -F '\t' -v name="clojure.zip/$function" \
    '$1 == "definition" && $2 == name && $3 == "source" {found = 1} END {exit !found}' \
    "$status_file"; then
    echo "clojure.zip/$function is not classified as source" >&2
    exit 1
  fi
done
