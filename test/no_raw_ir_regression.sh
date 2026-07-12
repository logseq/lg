#!/bin/sh
set -eu

matches=$(
  rg -n 'Ocaml_ir\.Raw|\bRaw\b|\braw\b|typed\s*\(' \
    src README.md docs test \
    -g '*.ml' -g '*.mli' -g '*.md' -g '*.sh' \
    -g '!test/no_raw_ir_regression.sh' || true
)

if [ -n "$matches" ]; then
  printf '%s\n' "$matches"
  exit 1
fi
