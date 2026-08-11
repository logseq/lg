#!/bin/sh
set -eu

matches=$(
  rg -n 'Ocaml_ir\.Raw|\bRaw\b|typed\s*\(' \
    src \
    -g '*.ml' -g '*.mli' \
    || true
)

if [ -n "$matches" ]; then
  printf '%s\n' "$matches"
  exit 1
fi
