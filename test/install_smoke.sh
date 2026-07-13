#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)

dune build --root "$root" @install

compiler="$root/_build/install/default/bin/cljml"
package="$root/_build/install/default/lib/cljml/dune-package"

test -x "$compiler"
test -f "$package"

output=$(mktemp "${TMPDIR:-/tmp}/cljml-install-smoke.XXXXXX.ml")
interface=$(mktemp "${TMPDIR:-/tmp}/cljml-install-smoke.XXXXXX.mli")
trap 'rm -f "$output" "$interface"' EXIT

"$compiler" "$root/examples/person.cljml" -o "$output"
grep -Fq 'let' "$output"
"$compiler" --interface "$root/examples/person.cljml" -o "$interface"
grep -Fq 'val label : string' "$interface"
