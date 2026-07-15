set -eu

javascript="$1"
expected="$2"
actual="$(mktemp "${TMPDIR:-/tmp}/lg-jsoo.XXXXXX")"
trap 'rm -f "$actual"' EXIT

node "$javascript" > "$actual"
diff -u "$expected" "$actual"
