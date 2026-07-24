#!/usr/bin/env bash
set -euo pipefail

for generated_source in "$@"; do
  if grep -E \
    "Query_value.*(Runtime_dynamic|Lg_dyn|D\\.)|(Runtime_dynamic|Lg_dyn|D\\.).*Query_value" \
    "$generated_source" >/dev/null; then
    echo "Generated static query values cross a dynamic boundary: $generated_source" >&2
    exit 1
  fi

  if grep -E "hash:[[:space:]]*(D\\.t|Runtime_dynamic\\.t|Lg_dyn\\.t)" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript DB hash field is dynamic: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "(type_|symbols):[[:space:]]*(D\\.t|Runtime_dynamic\\.t|Lg_dyn\\.t)" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript query return-map fields are dynamic: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "(keys|syms|strs):[[:space:]]*(D\\.t|Runtime_dynamic\\.t|Lg_dyn\\.t)" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript legacy query-form fields are dynamic: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "(tempids|db_after):[[:space:]]*(D\\.t|Runtime_dynamic\\.t|Lg_dyn\\.t)" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript transaction fields are dynamic: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "datascript_db_datom__arity_5[^=]*added[[:space:]]*:[[:space:]]*(D\\.t|Runtime_dynamic\\.t|Lg_dyn\\.t)" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript datom added flag is dynamic: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "register_record_packer \"datascript\\.(db/(Datom|DB|TxReport)|conn/(Conn|conn-state)|parser/Variable)\"" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript core records register dynamic packers: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "seen:[[:space:]]*(D\\.t|Runtime_dynamic\\.t|Lg_dyn\\.t)" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript distinct state has a dynamic key set: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "(attribute|value):[[:space:]]*(D\\.t|Runtime_dynamic\\.t|Lg_dyn\\.t)" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript exception metadata has dynamic fields: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "(root_|shift)[^:]{0,4}:[[:space:]]*(D\\.t|Runtime_dynamic\\.t|Lg_dyn\\.t)" \
    "$generated_source" >/dev/null; then
    echo "Generated sorted-set traversal uses dynamic booleans or indexes: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "(D\\.|Runtime_dynamic\\.|Lg_dyn\\.)polymorphic_equal" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript uses universal polymorphic equality: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "let datascript_util_(distinct_by|find|single|concatv|zip|removem|conjv|conjs|reduce_indexed)" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript contains unused generic compatibility helpers: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "\\(D\\.keyword key\\)[[:space:]]+\\(D\\.host \"Datascript_runtime\\.Data_value\\.t\" value\\)" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript erases schema values while deriving properties: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "let datascript_db_(seqable_|datom_from_reader|db_from_reader)|let datascript_core_data_readers" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript contains dynamic EDN readers or dead sequence compatibility code: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "schema[[:space:]]*:[[:space:]]*(D\\.t|Runtime_dynamic\\.t|Lg_dyn\\.t)" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript erases a schema into a dynamic value: $generated_source" >&2
    exit 1
  fi

  if grep -E "D\\.merge" "$generated_source" >/dev/null; then
    echo "Generated DataScript uses universal map merge: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "db a \\(D\\.host \"Datascript_runtime\\.Data_value\\.t\" v\\)" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript erases a transaction value for cardinality checks: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "let datascript_schema_(schema_|is_system_keyword_|schema_entity_)|let datascript_db_check_schema_update" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript contains unused dynamic schema compatibility helpers: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "force_[[:space:]]*:[[:space:]]*(D\\.t|Runtime_dynamic\\.t|Lg_dyn\\.t)" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript storage force flag is dynamic: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "size[[:space:]]*:[[:space:]]*(D\\.t|Runtime_dynamic\\.t|Lg_dyn\\.t)" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript parser size is dynamic: $generated_source" >&2
    exit 1
  fi

  if grep -E "D\\.truthy" "$generated_source" >/dev/null; then
    echo "Generated DataScript relies on universal truthiness: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "D\\.host \"tx_entry\"" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript erases closed transaction entries: $generated_source" >&2
    exit 1
  fi

  if grep -E \
    "Runtime_dynamic|Lg_dyn|D\\.[A-Za-z_]+" \
    "$generated_source" >/dev/null; then
    echo "Generated DataScript references Runtime_dynamic: $generated_source" >&2
    exit 1
  fi
done
