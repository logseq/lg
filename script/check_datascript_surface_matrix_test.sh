#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
matrix="$repo_root/test/datascript/differential/surface_matrix.tsv"
failures=0

if [ ! -f "$matrix" ]; then
  echo "FAIL: missing DataScript surface matrix" >&2
  exit 1
fi

expect_feature() {
  area=$1
  feature=$2
  if awk -F '\t' -v area="$area" -v feature="$feature" \
    '$1 == area && $2 == feature { found = 1 } END { exit !found }' "$matrix"
  then
    echo "PASS: $area/$feature"
  else
    echo "FAIL: missing $area/$feature" >&2
    failures=$((failures + 1))
  fi
}

for feature in \
  default-source-pattern explicit-source-pattern collection-source-pattern \
  predicate function rule recursion and not not-join or or-join
do
  expect_feature query-clause "$feature"
done

for feature in \
  relation collection tuple scalar aggregate pull with return-map uniqueness
do
  expect_feature find "$feature"
done

for feature in scalar collection tuple relation source rules; do
  expect_feature input "$feature"
done

for feature in \
  wildcard attribute alias default limit transform reverse recursion cycle \
  visitor missing-entity pull-many-parse-reuse
do
  expect_feature pull "$feature"
done

for feature in \
  entity-map add retract retract-attribute retract-entity cas raw-datom \
  transaction-function nested-map reverse-ref tuple-maintenance \
  unique-identity component-cascade tempid ordering invalid-input
do
  expect_feature transaction "$feature"
done

for feature in \
  default-payload custom-codec options schema datom-order index-reuse \
  branching reference-policy storage-rejection old-format
do
  expect_feature serialization "$feature"
done

if awk -F '\t' '$1 !~ /^#/ && $4 == "missing-behavior" { exit 1 }' "$matrix"
then
  echo "PASS: no cataloged behavior remains missing"
else
  echo "FAIL: surface matrix still contains missing behavior" >&2
  failures=$((failures + 1))
fi

if [ "$failures" -ne 0 ]; then
  echo "DataScript surface matrix failures: $failures" >&2
  exit 1
fi

echo "DataScript surface matrix is complete"
