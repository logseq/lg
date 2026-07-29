#!/bin/sh

set -eu

count_inline_hints() {
  {
    rg -o '\^[A-Za-z_:][A-Za-z0-9_./:<>,;?!-]*' "$@" \
      -g '*.clj' -g '*.cljc' -g '*.cljs' || true
  } | wc -l | tr -d ' '
}

query_hint_limit=413
query_hint_count=$(count_inline_hints test/datascript/lg)
if [ "$query_hint_count" -gt "$query_hint_limit" ]; then
  echo "DataScript query inline hints increased: $query_hint_count > $query_hint_limit" >&2
  exit 1
fi

query_v3_adapter_pattern='^\(defn- (\^[^[:space:]]+[[:space:]]+)?(concat-two|concatv-closed|zip-pair|zip-append|empty-collect-transforms|empty-collect-specimen)\b'
if rg -q "$query_v3_adapter_pattern" \
  test/datascript/lg/query_v3.cljc; then
  echo "DataScript query-v3 must not retain redundant collection adapter helpers" >&2
  exit 1
fi

query_adapter_pattern='^\(defn-[[:space:]]+(empty-collect-row|collect-copy-map|relation-has-collect-symbol\?|first-tuple-only)([[:space:]]|$)'
if rg -U -q "$query_adapter_pattern" test/datascript/lg/query.cljc; then
  echo "DataScript query must not retain redundant collection adapter helpers" >&2
  exit 1
fi

parser_hint_limit=77
parser_hint_count=$(count_inline_hints test/datascript/upstream/parser.cljc)
if [ "$parser_hint_count" -gt "$parser_hint_limit" ]; then
  echo "DataScript parser inline hints increased: $parser_hint_count > $parser_hint_limit" >&2
  exit 1
fi

pss_hint_count=$(count_inline_hints datascript)
if [ "$pss_hint_count" -ne 0 ]; then
  echo "Persistent sorted set must remain free of inline hints: $pss_hint_count" >&2
  exit 1
fi

echo "DataScript annotation audit passed: query=$query_hint_count, parser=$parser_hint_count, pss=$pss_hint_count"
