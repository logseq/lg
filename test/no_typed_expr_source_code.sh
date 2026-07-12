#!/bin/sh
set -eu

if rg -n 'code : string;|code = Ocaml_ir\.to_source|code = "[^"]*";' src/types.ml src/destructure.ml src/typecheck.ml src/structural_map.ml src/core_compare.ml; then
  echo "typed_expr must not store legacy source-code strings" >&2
  exit 1
fi
