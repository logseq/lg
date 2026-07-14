#!/bin/sh
set -eu

if [ "$#" -ne 1 ]; then
  echo "usage: $0 /path/to/datascript" >&2
  exit 2
fi

upstream=$1
test_root="$upstream/test"

if [ ! -d "$test_root" ]; then
  echo "DataScript test directory not found: $test_root" >&2
  exit 2
fi

find "$test_root" -type f \( -name '*.clj' -o -name '*.cljc' -o -name '*.cljs' \) \
  | LC_ALL=C sort \
  | while IFS= read -r file; do
      relative=${file#"$test_root"/}
      case "$file" in
        *.clj) target=native ;;
        *.cljs) target=melange ;;
        *.cljc) target=both ;;
      esac
      awk -v path="$relative" -v target="$target" '
        function trim(value) {
          sub(/^[[:space:]]+/, "", value)
          sub(/[[:space:]]+$/, "", value)
          return value
        }

        function emit_name(rest, metadata_end, fields) {
          rest = trim(rest)
          while (substr(rest, 1, 1) == "^") {
            metadata_end = index(rest, "}")
            while (metadata_end == 0 && getline > 0) {
              rest = rest " " $0
              metadata_end = index(rest, "}")
            }
            if (metadata_end == 0) {
              print "unterminated deftest metadata in " path > "/dev/stderr"
              exit 2
            }
            rest = trim(substr(rest, metadata_end + 1))
          }
          while (rest == "" && getline > 0) {
            rest = trim($0)
          }
          split(rest, fields, /[[:space:]()\[\]{}]+/)
          if (fields[1] == "") {
            print "missing deftest name in " path > "/dev/stderr"
            exit 2
          }
          print target "\t" path ":" fields[1]
        }

        /^[[:space:]]*\(deftest[[:space:]]+/ {
          line = $0
          sub(/^[[:space:]]*\(deftest[[:space:]]+/, "", line)
          emit_name(line)
        }
      ' "$file"
    done
