#!/bin/sh
set -eu

if test "$#" -ne 2; then
  echo "usage: $0 LG_ROOT LOGSEQ_ROOT" >&2
  exit 2
fi

lg_root=$(CDPATH= cd -- "$1" && pwd)
logseq_root=$(CDPATH= cd -- "$2" && pwd)
repository=$(sed -n 's/.*:logseq-repository "\([^"]*\)".*/\1/p' \
  "$lg_root/stdlib/upstream.edn" | head -1)
expected_commit=$(sed -n 's/.*:logseq-commit "\([0-9a-f][0-9a-f]*\)".*/\1/p' \
  "$lg_root/stdlib/upstream.edn" | head -1)
actual_commit=$(git -C "$logseq_root" rev-parse HEAD)

test -n "$repository" || {
  echo "stdlib/upstream.edn does not pin the Logseq repository" >&2
  exit 1
}
test -n "$expected_commit" || {
  echo "stdlib/upstream.edn does not pin the Logseq commit" >&2
  exit 1
}
test "$actual_commit" = "$expected_commit" || {
  echo "Logseq checkout does not match stdlib/upstream.edn: expected $expected_commit, found $actual_commit" >&2
  exit 1
}

printf 'meta\tlogseq-repository\t%s\n' "$repository"
"$lg_root/script/generate_clojure_surface_inventory.sh" \
  "$lg_root" "$logseq_root" \
  | awk -F '\t' '
      ($1 == "meta" && $2 == "logseq-commit") || $1 ~ /^logseq-/ {print}
    '

