#!/bin/sh
set -eu

root=$1
source_file="$root/stdlib/clojure/data.cljc"
interface_file="$root/stdlib/clojure/data.mli"

for file in "$source_file" "$interface_file"; do
  if ! test -f "$file"; then
    echo "${file#"$root/"} is missing from the source standard library" >&2
    exit 1
  fi
done

for definition in \
  '(defprotocol EqualityPartition' \
  '(defprotocol Diff' \
  '(defn diff'; do
  if ! grep -F "$definition" "$source_file" >/dev/null; then
    echo "${definition#\(} is not source-defined in clojure.data" >&2
    exit 1
  fi
done

if ! grep -F '(extend-type :Lg_edn_backend.t' "$source_file" >/dev/null; then
  echo "clojure.data protocols are not implemented for the closed EDN domain" >&2
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

if grep -F 'declare_data_protocols' "$root/src/core_protocols.ml" >/dev/null \
  || grep -F 'EqualityPartition' "$root/src/core_protocols.ml" >/dev/null \
  || grep -F 'diff-similar' "$root/src/core_protocols.ml" >/dev/null; then
  echo "clojure.data protocols are still compiler-owned" >&2
  exit 1
fi

status_file=$(mktemp)
trap 'rm -f "$status_file"' EXIT HUP INT TERM
bb "$root/script/extract_stdlib_manifest_status.clj" \
  "$root/stdlib/upstream.edn" >"$status_file"

for definition in diff equality-partition diff-similar; do
  if ! awk -F '\t' -v expected="clojure.data/$definition" \
    '$1 == "definition" && $2 == expected && $3 == "source" {found = 1} END {exit !found}' \
    "$status_file"; then
    echo "clojure.data/$definition is not classified as source" >&2
    exit 1
  fi
done
