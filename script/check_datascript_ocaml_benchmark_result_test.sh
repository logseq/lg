#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
checker="$repo_root/script/check_datascript_ocaml_benchmark_result.sh"
fixture_dir=$(mktemp -d "${TMPDIR:-/tmp}/lg-datascript-ocaml-result.XXXXXX")
trap 'rm -rf "$fixture_dir"' EXIT HUP INT TERM

write_valid_fixture() {
  output=$1
  cat >"$output" <<'EOF'
runtime	ocaml
size	20000
add-1	1.0
add-5	2.0
add-all	3.0
datoms-name	0.1
q1	0.2
q2	0.3
q3	0.4
q4	0.5
q5-shortcircuit	0.6
qpred1	0.7
qpred2	0.8
q2pred	0.9
pull-one	1.1
storage-roundtrip	1.2
EOF
}

expect_success() {
  description=$1
  shift
  if "$@" >/dev/null 2>&1; then
    echo "PASS: $description"
  else
    echo "FAIL: $description" >&2
    exit 1
  fi
}

expect_failure() {
  description=$1
  shift
  if "$@" >/dev/null 2>&1; then
    echo "FAIL: $description" >&2
    exit 1
  else
    echo "PASS: $description"
  fi
}

valid="$fixture_dir/valid.tsv"
write_valid_fixture "$valid"
expect_success "a complete pinned result is accepted" "$checker" "$valid"

missing="$fixture_dir/missing.tsv"
sed '/^qpred2	/d' "$valid" >"$missing"
expect_failure "a missing workload is rejected" "$checker" "$missing"

duplicate="$fixture_dir/duplicate.tsv"
cp "$valid" "$duplicate"
printf 'q1\t0.2\n' >>"$duplicate"
expect_failure "a duplicate workload is rejected" "$checker" "$duplicate"

invalid="$fixture_dir/invalid.tsv"
sed 's/^q2	0.3$/q2\tnot-a-number/' "$valid" >"$invalid"
expect_failure "a non-numeric duration is rejected" "$checker" "$invalid"

wrong_size="$fixture_dir/wrong-size.tsv"
sed 's/^size	20000$/size\t200/' "$valid" >"$wrong_size"
expect_failure "the wrong population is rejected" "$checker" "$wrong_size"
