#!/bin/sh

set -eu

if [ -e test/datascript/lg/annotations.cljc ] || \
   [ ! -f test/datascript/lg/annotations.lgi ]; then
  echo "DataScript boundary signatures must live in test/datascript/lg/annotations.lgi" >&2
  exit 1
fi

count_inline_hints() {
  {
    rg -o --no-filename '\^[A-Za-z_:][A-Za-z0-9_./:<>,;?!-]*' "$@" \
      -g '*.clj' -g '*.cljc' -g '*.cljs' || true
  } | awk '
    $0 !~ /^\^:(private|const|mutable|dynamic|export|no-doc|ordering-fn)$/ {
      count++
    }
    END { print count + 0 }
  '
}

count_declaration_hints() {
  awk '
    function hint_count(line, count, hint) {
      count = 0
      while (match(line, /\^[A-Za-z_:][A-Za-z0-9_\.\/:<>,;?!-]*/)) {
        hint = substr(line, RSTART, RLENGTH)
        if (hint !~ /^\^:(private|const|mutable|dynamic|export|no-doc|ordering-fn)$/) {
          count++
        }
        line = substr(line, RSTART + RLENGTH)
      }
      return count
    }
    function paren_delta(line, opened, closed, copy) {
      copy = line
      opened = gsub(/\(/, "", copy)
      copy = line
      closed = gsub(/\)/, "", copy)
      return opened - closed
    }
    FNR == 1 {
      mode = ""
      depth = 0
    }
    mode == "fields" {
      total += hint_count($0)
      if (index($0, "]") > 0) mode = ""
      next
    }
    mode == "form" {
      total += hint_count($0)
      depth += paren_delta($0)
      if (depth <= 0) mode = ""
      next
    }
    /^\((deftype|defrecord|deftrecord)[[:space:]]/ {
      total += hint_count($0)
      if (index($0, "]") == 0) mode = "fields"
      next
    }
    /^[[:space:]]*\(defprotocol[[:space:]]/ {
      total += hint_count($0)
      depth = paren_delta($0)
      if (depth > 0) mode = "form"
      next
    }
    /^[[:space:]]+\(-[A-Za-z0-9_?!*+.<>=-]+[[:space:]]*\[/ {
      total += hint_count($0)
      if (index($0, "]") == 0) mode = "fields"
      next
    }
    /^\(def(once)?[[:space:]]+\^[A-Za-z_:]/ {
      total += hint_count($0)
      depth = paren_delta($0)
      if (depth > 0) mode = "form"
      next
    }
    END { print total + 0 }
  ' "$@"
}

query_hint_limit=225
query_hint_count=$(count_inline_hints test/datascript/lg)
if [ "$query_hint_count" -gt "$query_hint_limit" ]; then
  echo "DataScript query inline hints increased: $query_hint_count > $query_hint_limit" >&2
  exit 1
fi

datascript_hint_limit=92
datascript_hint_count=$(count_inline_hints \
  test/datascript/upstream \
  test/datascript/lg)
datascript_declaration_hint_count=$(count_declaration_hints \
  test/datascript/upstream/*.cljc \
  test/datascript/lg/*.cljc)
datascript_algorithm_hint_count=$((datascript_hint_count - datascript_declaration_hint_count))
if [ "$datascript_algorithm_hint_count" -gt "$datascript_hint_limit" ]; then
  echo "DataScript algorithm-local hints exceed the 95 percent removal target: $datascript_algorithm_hint_count > $datascript_hint_limit (total=$datascript_hint_count, declarations=$datascript_declaration_hint_count)" >&2
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

datom_receiver_hint_pattern='^\(defn (\^[^ ]+ )?(datom-attr|datom-print-string|equiv-datom) \[\^Datom'
if rg -q "$datom_receiver_hint_pattern" test/datascript/upstream/db.cljc; then
  echo "DataScript datom field helpers must infer their Datom receivers" >&2
  exit 1
fi

datom_comparator_hint_pattern='^\(defn cmp-datoms-[^ ]+ \^long \[\^Datom'
if rg -q "$datom_comparator_hint_pattern" test/datascript/upstream/db.cljc; then
  echo "DataScript datom comparators must infer record arguments and int results" >&2
  exit 1
fi

datom_callback_hint_pattern='\(fn \[[^]]*\^Datom'
if rg -q "$datom_callback_hint_pattern" test/datascript/upstream/db.cljc; then
  echo "DataScript collection callbacks must infer Datom parameters" >&2
  exit 1
fi

db_callback_hint_pattern='\(fn[[:space:]]*\[[^]]*\^[A-Za-z_:]'
if rg -U -q "$db_callback_hint_pattern" \
  test/datascript/upstream/db.cljc \
  test/datascript/upstream/storage.cljc \
  test/datascript/upstream/storage_file.cljc; then
  echo "DataScript DB and storage callbacks must infer ordinary parameters" >&2
  exit 1
fi

loop_binding_hint_pattern='\(loop \[[^]]*\^[A-Za-z_:]'
if rg -U -q "$loop_binding_hint_pattern" \
  test/datascript/upstream/db.cljc \
  test/datascript/upstream/storage.cljc; then
  echo "DataScript loop bindings must infer from initializers and recur values" >&2
  exit 1
fi

initializer_let_hint_pattern='\(let \[[^]]*\^[^[:space:]]+[[:space:]]+(left-datoms|right-datoms|attrs|ref-attrs|entity-refs|eids|step|entries)\b'
if rg -U -q "$initializer_let_hint_pattern" \
  test/datascript/upstream/db.cljc \
  test/datascript/upstream/storage.cljc; then
  echo "DataScript let bindings must infer concrete initializer result types" >&2
  exit 1
fi

datom_function_hint_pattern='^\(defn[^\n]*(\n[[:space:]]*)?\[[^]]*\^Datom'
if rg -U -q "$datom_function_hint_pattern" test/datascript/upstream/db.cljc; then
  echo "DataScript functions must infer ordinary Datom parameters" >&2
  exit 1
fi

pss_hint_count=$(count_inline_hints datascript)
if [ "$pss_hint_count" -ne 0 ]; then
  echo "Persistent sorted set must remain free of inline hints: $pss_hint_count" >&2
  exit 1
fi

built_ins_upstream_helpers='-differ? and-fn or-fn less greater less-equal greater-equal'
for helper in $built_ins_upstream_helpers; do
  if ! rg -F -q "(defn- $helper" \
    test/datascript/upstream/built_ins.cljc; then
    echo "DataScript built-ins must retain upstream helper implementation: $helper" >&2
    exit 1
  fi
done

datafy_upstream_helpers='datafy-entity-seq navize-pulled-entity navize-pulled-entity-seq'
for helper in $datafy_upstream_helpers; do
  if ! rg -F -q "(defn- $helper" test/datascript/lg/datafy.cljc; then
    echo "DataScript datafy must retain upstream helper implementation: $helper" >&2
    exit 1
  fi
done

pull_parser_upstream_helpers='check-limit index-of conj-attr'
for helper in $pull_parser_upstream_helpers; do
  if ! rg -F -q "(defn- $helper" test/datascript/upstream/pull_parser.cljc; then
    echo "DataScript pull parser must retain upstream helper implementation: $helper" >&2
    exit 1
  fi
done

entity_upstream_helpers='entid -lookup-backwards'
for helper in $entity_upstream_helpers; do
  if ! rg -F -q "(defn- $helper" test/datascript/upstream/entity.cljc; then
    echo "DataScript entity must retain upstream helper implementation: $helper" >&2
    exit 1
  fi
done

query_upstream_helpers='context-resolve-val rel-with-attr sum-rel*'
for helper in $query_upstream_helpers; do
  if ! rg -F -q "(defn- $helper" test/datascript/lg/query.cljc; then
    echo "DataScript query must retain upstream helper implementation: $helper" >&2
    exit 1
  fi
done

query_v3_upstream_helpers='rel->consts matches-pattern? resolve-in'
for helper in $query_v3_upstream_helpers; do
  if ! rg -F -q "(defn- $helper" test/datascript/lg/query_v3.cljc; then
    echo "DataScript query-v3 must retain upstream helper implementation: $helper" >&2
    exit 1
  fi
done

pull_api_upstream_helpers='assoc-some! attr-str conj-seq conj-some! first-seq next-seq'
for helper in $pull_api_upstream_helpers; do
  if ! rg -F -q "(defn- $helper" test/datascript/upstream/pull_api.cljc; then
    echo "DataScript pull API must retain upstream helper implementation: $helper" >&2
    exit 1
  fi
done

echo "DataScript annotation audit passed: total=$datascript_hint_count, declarations=$datascript_declaration_hint_count, algorithm=$datascript_algorithm_hint_count, query=$query_hint_count, parser=$parser_hint_count, pss=$pss_hint_count"
