set -eu

native_executable="$1"
expected="$2"

case "$native_executable" in
  */*) ;;
  *) native_executable="./$native_executable" ;;
esac

actual="$(mktemp "${TMPDIR:-/tmp}/lg-datascript-native.XXXXXX")"
trap 'rm -f "$actual"' EXIT

"$native_executable" > "$actual"
diff -u "$expected" "$actual"
