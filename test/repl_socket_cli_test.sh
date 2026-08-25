#!/usr/bin/env bash
set -euo pipefail

lg_cli=$1
repl_worker=$2
stdlib_state=$3

test_dir=$(mktemp -d)
server_pid=

cleanup() {
  if [[ -n "$server_pid" ]]; then
    kill "$server_pid" 2>/dev/null || true
    wait "$server_pid" 2>/dev/null || true
  fi
  rm -rf "$test_dir"
}
trap cleanup EXIT

port_file="$test_dir/port"
server_log="$test_dir/server.log"

"$repl_worker" --listen 127.0.0.1:0 --state "$stdlib_state" \
  --port-file "$port_file" >"$server_log" 2>&1 &
server_pid=$!

for _attempt in $(seq 1 100); do
  if [[ -s "$port_file" ]]; then
    break
  fi
  if ! kill -0 "$server_pid" 2>/dev/null; then
    printf 'Socket REPL server exited before announcing its port:\n' >&2
    sed -n '1,160p' "$server_log" >&2
    exit 1
  fi
  sleep 0.05
done

if [[ ! -s "$port_file" ]]; then
  printf 'Socket REPL server did not announce its port\n' >&2
  exit 1
fi

endpoint="127.0.0.1:$(tr -d '[:space:]' <"$port_file")"

output=$(
  "$lg_cli" repl --connect "$endpoint" 2>&1 <<'EOF'
(+ 40
 2)
:type [1 2]
(ns socket.demo)
(def answer 42)
(println "socket-output")
(throw (ex-info "socket-boom" {}))
answer
:quit
EOF
)

case "$output" in
  *"42 : int"*"vector<int>"*"namespace socket.demo"*"answer : int"*"socket-output"*"socket-boom"*"42 : int"*)
    ;;
  *)
    printf 'unexpected Socket REPL output:\n%s\n' "$output" >&2
    exit 1
    ;;
esac

printf '(def session-only 9)\n:quit\n' \
  | "$lg_cli" repl --connect "$endpoint" >/dev/null

isolated_output=$(
  printf 'session-only\n:quit\n' \
    | "$lg_cli" repl --connect "$endpoint" 2>&1
)

case "$isolated_output" in
  *"session-only"*"[LG"*)
    ;;
  *)
    printf 'Socket REPL connections did not have isolated sessions:\n%s\n' \
      "$isolated_output" >&2
    exit 1
    ;;
esac

if "$repl_worker" --listen 0.0.0.0:0 --state "$stdlib_state" \
  >"$test_dir/non-loopback.log" 2>&1; then
  printf 'Socket REPL unexpectedly accepted a non-loopback bind\n' >&2
  exit 1
fi

if ! grep -qi loopback "$test_dir/non-loopback.log"; then
  printf 'non-loopback rejection did not explain the security boundary:\n' >&2
  sed -n '1,160p' "$test_dir/non-loopback.log" >&2
  exit 1
fi
