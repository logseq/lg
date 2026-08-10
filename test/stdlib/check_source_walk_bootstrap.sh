#!/bin/sh
set -eu

root=$1
source_file="$root/stdlib/clojure/walk.cljc"
interface_file="$root/stdlib/clojure/walk.mli"

for file in "$source_file" "$interface_file"; do
  if ! test -f "$file"; then
    echo "${file#"$root/"} is missing from the source standard library" >&2
    exit 1
  fi
done

for definition in walk postwalk prewalk keywordize-keys stringify-keys prewalk-replace postwalk-replace; do
  if ! grep -F "(defn $definition" "$source_file" >/dev/null; then
    echo "clojure.walk/$definition is not source-defined" >&2
    exit 1
  fi
done

if grep -F '"clojure.walk" -> Core_walk.bindings' \
  "$root/src/core_namespaces.ml" >/dev/null; then
  echo "clojure.walk is still compiler-owned" >&2
  exit 1
fi

if grep -F 'Runtime_dynamic' "$root/runtime/runtime_walk.ml" >/dev/null; then
  echo "clojure.walk still depends on Runtime_dynamic" >&2
  exit 1
fi

status_file=$(mktemp)
trap 'rm -f "$status_file"' EXIT HUP INT TERM
bb "$root/script/extract_stdlib_manifest_status.clj" \
  "$root/stdlib/upstream.edn" >"$status_file"

for definition in walk postwalk prewalk keywordize-keys stringify-keys prewalk-replace postwalk-replace; do
  if ! awk -F '\t' -v name="clojure.walk/$definition" \
    '$1 == "definition" && $2 == name && $3 == "source" {found = 1} END {exit !found}' \
    "$status_file"; then
    echo "clojure.walk/$definition is not classified as source" >&2
    exit 1
  fi
done
