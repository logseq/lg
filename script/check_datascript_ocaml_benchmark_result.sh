#!/bin/sh
set -eu

if [ "$#" -ne 1 ]; then
  echo "usage: $0 RESULT.tsv" >&2
  exit 2
fi

result=$1
if [ ! -f "$result" ]; then
  echo "datascript-ocaml benchmark result is missing: $result" >&2
  exit 1
fi

awk -F '\t' '
  BEGIN {
    expected["add-1"] = 1
    expected["add-5"] = 1
    expected["add-all"] = 1
    expected["datoms-name"] = 1
    expected["q1"] = 1
    expected["q2"] = 1
    expected["q3"] = 1
    expected["q4"] = 1
    expected["q5-shortcircuit"] = 1
    expected["qpred1"] = 1
    expected["qpred2"] = 1
    expected["q2pred"] = 1
    expected["pull-one"] = 1
    expected["storage-roundtrip"] = 1
  }

  $1 == "runtime" {
    runtime_count += 1
    runtime = $2
    next
  }

  $1 == "size" {
    size_count += 1
    size = $2
    next
  }

  $1 in expected {
    seen[$1] += 1
    if (NF != 2 || $2 !~ /^[0-9]+([.][0-9]+)?([eE][+-]?[0-9]+)?$/ || $2 <= 0) {
      printf "invalid duration for %s: %s\n", $1, $2 > "/dev/stderr"
      failed = 1
    }
    next
  }

  NF > 0 {
    printf "unexpected result row: %s\n", $0 > "/dev/stderr"
    failed = 1
  }

  END {
    if (runtime_count != 1 || runtime != "ocaml") {
      print "expected exactly one runtime=ocaml row" > "/dev/stderr"
      failed = 1
    }
    if (size_count != 1 || size != "20000") {
      print "expected exactly one size=20000 row" > "/dev/stderr"
      failed = 1
    }
    for (name in expected) {
      if (seen[name] != 1) {
        printf "expected exactly one %s row\n", name > "/dev/stderr"
        failed = 1
      }
    }
    exit failed
  }
' "$result"

echo "datascript-ocaml benchmark result is complete"
