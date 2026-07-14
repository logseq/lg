set -eu

native_executable="$1"
melange_javascript="$2"
expected="$3"

case "$native_executable" in
  */*) ;;
  *) native_executable="./$native_executable" ;;
esac

actual="$(mktemp "${TMPDIR:-/tmp}/lg-datascript-value.XXXXXX")"
trap 'rm -f "$actual"' EXIT

"$native_executable" > "$actual"
node "$melange_javascript" >> "$actual"
diff -u "$expected" "$actual"
