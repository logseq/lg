#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

if rg -n 'Obj\.(obj|magic)' "$root/runtime/runtime_dynamic.ml"; then
  echo "runtime_dynamic contains an unsafe cast" >&2
  exit 1
fi

if rg -n 'Runtime_dynamic\.polymorphic_(equal|hash|str|pr_str)' \
  "$root/src" "$root/runtime"; then
  echo "generic compiler/runtime code still probes universal dynamic values" >&2
  exit 1
fi

if rg -n 'Runtime_dynamic' "$root/runtime/core_set.ml"; then
  echo "closed static set implementations depend on Runtime_dynamic" >&2
  exit 1
fi

if rg -n 'Core_scalar\.compile.*"' "$root/src"; then
  echo "migrated scalar builtins still dispatch through raw strings" >&2
  exit 1
fi

dynamic_files=$(rg -l 'Runtime_dynamic' "$root/src" "$root/runtime" \
  --glob '*.ml' --glob '*.mli' | sed "s|$root/||" | sort)
allowed_dynamic_files='runtime/runtime_map.ml
runtime/runtime_multimethod.ml
runtime/runtime_tap.ml
runtime/runtime_test_report.ml
runtime/runtime_transient.ml
src/call_elaborator.ml
src/codegen.ml
src/collection_capability.ml
src/collection_operation_elaborator.ml
src/core_boolean.ml
src/core_collection.ml
src/core_compare.ml
src/core_int.ml
src/core_predicate.ml
src/core_scalar.ml
src/core_sequence_transform.ml
src/destructure.ml
src/elaborator.ml
src/expression_elaborator.ml
src/expression_support.ml
src/function_combinator_elaborator.ml
src/ocaml_parsetree.ml
src/ocaml_signature.ml
src/require.ml
src/semantic_lowering.ml
src/sequence_call_elaborator.ml
src/special_form_elaborator.ml
src/structural_map.ml
src/tap_dynamic_boundary.ml
src/top_level_elaborator.ml
src/type_annotation.ml
src/types.ml'

if [ "$dynamic_files" != "$allowed_dynamic_files" ]; then
  echo "Runtime_dynamic boundary file inventory changed" >&2
  printf '%s\n' "$dynamic_files" >&2
  exit 1
fi

if rg -n 'parseInt.*mel\.scope' "$root/runtime/runtime_number_melange.ml"; then
  echo "parseInt is a global Melange binding and must not use a scoped primitive" >&2
  exit 1
fi

if rg -n 'assert false' \
  "$root/bin/lsp_server.ml" \
  "$root/src/language_service.ml" \
  "$root/src/parser.ml" \
  "$root/src/toolchain.ml"; then
  echo "production input boundaries must return errors instead of asserting" >&2
  exit 1
fi

if rg -n '__lg_.*constraint' "$root/src/types.ml" "$root/src/type_solver.ml"; then
  echo "compiler constraints must use closed variants, not reserved OCaml type names" >&2
  exit 1
fi

if rg -n 'record_extension_type|make_record_extension_field \?ty' \
  "$root/src"; then
  echo "record extensions must carry an explicit static source-row type" >&2
  exit 1
fi

if rg -U -n \
  'plan_and_emit_argument[[:space:][:print:]]*Error _ ->[[:space:]]*typed_row_argument' \
  "$root/src/call_elaborator.ml"; then
  echo "typed row adaptation planner errors must not fall back to legacy projection" >&2
  exit 1
fi

if rg --pcre2 -U -n \
  'plan_and_emit_argument(?s:.{0,400})\| Error _ ->\s*(project_constraint_row|adapt_value_to_type)' \
  "$root/src/call_elaborator.ml"; then
  echo "argument adaptation planner errors must not fall back to legacy adapters" >&2
  exit 1
fi

if rg --pcre2 -U -n \
  'typed_row_argument\s+env\s+\(Structural_map\.record_type_application record\)' \
  "$root/src/call_elaborator.ml"; then
  echo "named record arguments must use the typed adaptation planner" >&2
  exit 1
fi

if rg --pcre2 -U -n \
  'plan_and_emit_(callback|overload|collection|map)(?s:.{0,400})\| None ->\s*adapt_value_to_type' \
  "$root/src/call_elaborator.ml"; then
  echo "typed adaptation helper failures must not fall back to legacy adapters" >&2
  exit 1
fi

if rg --pcre2 -U -n \
  'Adaptation\.Function_overload(?s:.{0,300})adapt_value_to_type' \
  "$root/src/call_elaborator.ml"; then
  echo "function overload plans must not return to the legacy adapter" >&2
  exit 1
fi

if rg -n '__lg_generic_function_overload_adapter' \
  "$root/src/call_elaborator.ml"; then
  echo "legacy fixed-function overload emission must not remain reachable" >&2
  exit 1
fi

if rg -n 'Call_elaborator\.adapt_value_to_type' \
  "$root/src/top_level_elaborator.ml"; then
  echo "top-level declarations must use typed adaptation plans" >&2
  exit 1
fi

if rg -n 'Call_elaborator\.adapt_value_to_type' \
  "$root/src/expression_elaborator.ml"; then
  echo "function return and default expressions must use typed adaptation plans" >&2
  exit 1
fi

if rg -n 'adapt_value_to_type' \
  "$root/src/function_combinator_elaborator.ml"; then
  echo "function combinators must use typed adaptation plans" >&2
  exit 1
fi

if rg -n 'val adapt_value_to_type' "$root/src/call_elaborator.mli"; then
  echo "the legacy adapter must not remain part of the compiler module API" >&2
  exit 1
fi
