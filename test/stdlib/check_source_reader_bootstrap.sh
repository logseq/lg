#!/bin/sh
set -eu

root=$1

for file in \
  stdlib/clojure/edn.mli \
  stdlib/clojure/edn.cljc \
  stdlib/cljs/reader.mli \
  stdlib/cljs/reader.cljc; do
  if ! test -f "$root/$file"; then
    echo "$file is missing from the source standard library" >&2
    exit 1
  fi
done

if grep -F '"read-string"' "$root/src/core_edn.ml" >/dev/null; then
  echo "read-string is still exported by the compiler-owned EDN namespace" >&2
  exit 1
fi

for name in register-tag-parser! deregister-tag-parser! \
  register-default-tag-parser! deregister-default-tag-parser!; do
  if ! grep -F "(defn $name" "$root/stdlib/cljs/reader.cljc" >/dev/null; then
    echo "cljs.reader/$name is not source-defined" >&2
    exit 1
  fi
  if grep -F "\"$name\"" "$root/src/core_edn.ml" >/dev/null; then
    echo "cljs.reader/$name is still compiler-owned" >&2
    exit 1
  fi
done

for namespace in clojure.edn cljs.reader; do
  if grep -F "\"$namespace\"" "$root/src/core_namespaces.ml" >/dev/null; then
    echo "$namespace is still routed through Core_namespaces" >&2
    exit 1
  fi
  if sed -n '/let is_core_namespace/,/let lookup_qualified_member/p' \
    "$root/src/core_namespaces.ml" | grep -F "\"$namespace\"" >/dev/null; then
    echo "$namespace is still compiler-owned" >&2
    exit 1
  fi
done

manifest_status=$(mktemp "${TMPDIR:-/tmp}/lg-reader-manifest.XXXXXX")
trap 'rm -f "$manifest_status"' EXIT HUP INT TERM
bb "$root/script/extract_stdlib_manifest_status.clj" \
  "$root/stdlib/upstream.edn" >"$manifest_status"

failed=0
assert_status() {
  name=$1
  status=$2
  reason=$3
  if ! awk -F '\t' -v name="$name" -v status="$status" -v reason="$reason" \
      '$1 == "definition" && $2 == name && $3 == status && $4 == reason { found=1 }
       END { exit !found }' "$manifest_status"; then
    echo "$name is missing audited $status reason: $reason" >&2
    failed=1
  fi
}

assert_status cljs.reader/parse-and-validate-timestamp blocked-static-typing \
  regex-capture-dependent-optional-string-groups-are-not-exposed-by-the-static-regex-boundary
assert_status cljs.reader/parse-timestamp host-boundary \
  returns-a-javascript-date-which-has-no-shared-native-source-representation
assert_status cljs.reader/read blocked-static-typing \
  pushback-reader-overloads-and-heterogeneous-reader-default-eof-options-require-a-closed-reader-options-domain
assert_status clojure.edn/read blocked-static-typing \
  pushback-reader-overloads-and-heterogeneous-reader-default-eof-options-require-a-closed-reader-options-domain

exit "$failed"
