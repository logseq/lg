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
trap 'rm -f "$output" "$interface"' EXIT

"$compiler" "$root/examples/person.lgc" -o "$output"
grep -Fq 'let' "$output"
"$compiler" --interface "$root/examples/person.lgc" -o "$interface"
grep -Fq 'val label : string' "$interface"
