#!/bin/sh
set -eu

if test "$#" -ge 1; then
  root=$(CDPATH= cd -- "$1" && pwd)
else
  root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
fi

report="$root/stdlib/logseq-dependencies.tsv"
generator="$root/script/generate_logseq_dependency_report.sh"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

test -x "$generator" || {
  echo "missing executable Logseq dependency report generator" >&2
  exit 1
}
test -f "$report" || {
  echo "missing checked Logseq dependency report" >&2
  exit 1
}

expected_repository=$(sed -n 's/.*:logseq-repository "\([^"]*\)".*/\1/p' \
  "$root/stdlib/upstream.edn" | head -1)
expected_commit=$(sed -n 's/.*:logseq-commit "\([0-9a-f][0-9a-f]*\)".*/\1/p' \
  "$root/stdlib/upstream.edn" | head -1)

test -n "$expected_repository" || {
  echo "stdlib/upstream.edn does not pin the Logseq repository" >&2
  exit 1
}
test -n "$expected_commit" || {
  echo "stdlib/upstream.edn does not pin the Logseq commit" >&2
  exit 1
}

awk -F '\t' -v repository="$expected_repository" '
  $1 == "meta" && $2 == "logseq-repository" && $3 == repository {found=1}
  END {exit !found}
' "$report"
awk -F '\t' -v commit="$expected_commit" '
  $1 == "meta" && $2 == "logseq-commit" && $3 == commit {found=1}
  END {exit !found}
' "$report"
awk -F '\t' '
  ($1 == "logseq-namespace-status" || $1 == "logseq-qualified-var-status") &&
  $3 == "unsupported" {
    print "unexplained unsupported Logseq dependency: " $2 > "/dev/stderr"
    failed=1
  }
  ($1 == "logseq-namespace-status" || $1 == "logseq-qualified-var-status") &&
  ($3 == "blocked-static-typing" || $3 == "host-boundary" ||
   $3 == "out-of-scope") && ($5 == "" || $5 == "manifest-source") {
    print "Logseq dependency lacks a concrete boundary reason: " $2 > "/dev/stderr"
    failed=1
  }
  END {exit failed}
' "$report"

git -C "$tmp" init -q
git -C "$tmp" config user.email inventory@example.invalid
git -C "$tmp" config user.name Inventory
git -C "$tmp" commit -q --allow-empty -m fixture
if "$generator" "$root" "$tmp" >"$tmp/output" 2>"$tmp/error"; then
  echo "Logseq report generator accepted an unpinned checkout" >&2
  exit 1
fi
grep -q "Logseq checkout does not match stdlib/upstream.edn" "$tmp/error"
