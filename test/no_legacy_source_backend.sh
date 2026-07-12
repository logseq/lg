#!/bin/sh
set -eu

forbidden='let emit_program|let rec emit_item|let emit_record_def|let emit_module_signature|let emit_type_variant|let emit_type_alias|let emit_type '

if rg -n "$forbidden" src/codegen.ml; then
  echo "legacy compiled-item source backend emitters must not live in Codegen" >&2
  exit 1
fi
