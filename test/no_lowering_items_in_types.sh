#!/bin/sh
set -eu

matches=$(
  rg -n '\bcompiled_item\b|\bsignature_item\b|\bvalue_pattern\b|\bValue_binding\b|\bModule_def\b|\bRecord_def\b' \
    src/types.ml || true
)

if [ -n "$matches" ]; then
  printf '%s\n' "$matches"
  exit 1
fi
