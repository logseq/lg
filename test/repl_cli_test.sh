#!/usr/bin/env bash
set -euo pipefail

lg_cli=$1
repl_worker=$2
stdlib_state=$3

output=$(
  "$repl_worker" --state "$stdlib_state" <<'EOF'
(+ 40
 2)
:type [1 2]
(ns scripted.repl)
(def answer 7)
answer
:quit
EOF
)

case "$output" in
  *"42 : int"*"vector<int>"*"namespace scripted.repl"*"answer : int"*"7 : int"*)
    ;;
  *)
    printf 'unexpected REPL output:\n%s\n' "$output" >&2
    exit 1
    ;;
esac

printf ':quit\n' | "$lg_cli" repl --state "$stdlib_state"
