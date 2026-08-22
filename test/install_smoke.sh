#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)

dune build --root "$root" @install

compiler="$root/_build/install/default/bin/lg"
package="$root/_build/install/default/lib/lg/dune-package"

test -x "$compiler"
test -f "$package"

output=$(mktemp "${TMPDIR:-/tmp}/lg-install-smoke.XXXXXX.ml")
interface=$(mktemp "${TMPDIR:-/tmp}/lg-install-smoke.XXXXXX.mli")
test_dir=$(mktemp -d "${TMPDIR:-/tmp}/lg-install-smoke.XXXXXX")
trap 'rm -f "$output" "$interface"; rm -rf "$test_dir"' EXIT

cd "$test_dir"
"$compiler" "$root/test/install_smoke.cljc" -o "$output"
grep -Fq 'let answer = 42' "$output"
"$compiler" --interface "$root/test/install_smoke.cljc" -o "$interface"
grep -Fq 'val answer : int' "$interface"
"$compiler" --run "$root/test/install_smoke.cljc"
