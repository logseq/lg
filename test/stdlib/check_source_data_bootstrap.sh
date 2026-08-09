#!/bin/sh
set -eu

root=$1
source_file="$root/stdlib/clojure/data.cljc"
interface_file="$root/stdlib/clojure/data.mil"

for file in "$source_file" "$interface_file"; do
  if ! test -f "$file"; then
    echo "${file#"$root/"} is missing from the source standard library" >&2
    exit 1
  fi
done

if ! grep -F '(defn diff' "$source_file" >/dev/null; then
  echo "clojure.data/diff is not source-defined" >&2
  exit 1
fi

if grep -F '"clojure.data" -> Core_data.bindings' \
  "$root/src/core_namespaces.ml" >/dev/null; then
  echo "clojure.data is still compiler-owned" >&2
  exit 1
fi

if grep -F 'Runtime_dynamic' "$root/runtime/runtime_data.ml" >/dev/null; then
  echo "clojure.data still depends on Runtime_dynamic" >&2
  exit 1
fi

status_file=$(mktemp)
trap 'rm -f "$status_file"' EXIT HUP INT TERM
bb "$root/script/extract_stdlib_manifest_status.clj" \
  "$root/stdlib/upstream.edn" >"$status_file"

if ! awk -F '\t' \
  '$1 == "definition" && $2 == "clojure.data/diff" && $3 == "source" {found = 1} END {exit !found}' \
  "$status_file"; then
  echo "clojure.data/diff is not classified as source" >&2
  exit 1
fi
