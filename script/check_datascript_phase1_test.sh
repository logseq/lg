#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
failures=0

pass() {
  echo "PASS: $1"
}

fail() {
  echo "FAIL: $1" >&2
  failures=$((failures + 1))
}

expect_file() {
  description=$1
  path=$2
  if [ -f "$repo_root/$path" ]; then
    pass "$description"
  else
    fail "$description (missing $path)"
  fi
}

expect_text() {
  description=$1
  path=$2
  pattern=$3
  if [ -f "$repo_root/$path" ] &&
     LC_ALL=C grep -Eq "$pattern" "$repo_root/$path"; then
    pass "$description"
  else
    fail "$description"
  fi
}

expect_no_text() {
  description=$1
  path=$2
  pattern=$3
  if [ -f "$repo_root/$path" ] &&
     ! LC_ALL=C grep -Eq "$pattern" "$repo_root/$path"; then
    pass "$description"
  else
    fail "$description"
  fi
}

expect_no_algorithm_hints() {
  description=$1
  path=$2
  pattern=$3
  if [ -f "$repo_root/$path" ] &&
     ! awk '
         NR == 1 { next }
         /^[[:space:]]*\(deftype / { in_fields = 1; next }
         in_fields {
           if (index($0, "]") != 0) {
             in_fields = 0
           }
           next
         }
         { print }
       ' "$repo_root/$path" |
       LC_ALL=C grep -Eq "$pattern"; then
    pass "$description"
  else
    fail "$description"
  fi
}

expect_no_hints_between() {
  description=$1
  path=$2
  start_name=$3
  end_name=$4
  pattern=$5
  if [ -f "$repo_root/$path" ] &&
     ! awk -v start_name="$start_name" -v end_name="$end_name" '
         /^[[:space:]]*\(defn-?[[:space:]]/ {
           function_name = $0
           sub(/^[[:space:]]*\(defn-?[[:space:]]+/, "", function_name)
           if (function_name ~ /^\^/) {
             sub(/^[^[:space:]]+[[:space:]]+/, "", function_name)
           }
           sub(/[[:space:]].*$/, "", function_name)
           if (function_name == start_name) {
             in_range = 1
           }
           if (function_name == end_name) {
             in_range = 0
           }
         }
         in_range { print }
       ' "$repo_root/$path" |
       LC_ALL=C grep -Eq "$pattern"; then
    pass "$description"
  else
    fail "$description"
  fi
}

expect_success() {
  description=$1
  shift
  if "$@" >/dev/null 2>&1; then
    pass "$description"
  else
    fail "$description"
  fi
}

expect_failure() {
  description=$1
  shift
  if "$@" >/dev/null 2>&1; then
    fail "$description"
  else
    pass "$description"
  fi
}

upstream_doc=test/datascript/UPSTREAM.md
api_manifest=test/datascript/api_manifest/upstream.tsv
lg_api_manifest=test/datascript/api_manifest/lg.tsv
differential_cases=test/datascript/differential/cases.tsv
differential_runner=script/run_datascript_differential.sh
compare_script=script/check_datascript_api_manifest.sh
generate_script=script/generate_datascript_api_manifest.clj

expect_file "the pinned upstream is documented" "$upstream_doc"
expect_text "the upstream repository URL is exact" "$upstream_doc" \
  'https://github\.com/logseq/datascript(\.git)?'
expect_text "the full pinned commit is recorded" "$upstream_doc" \
  '3f141af97b70e1f14c65eaa119acd822ebece37e'
expect_no_text "built-in algorithms contain no inline type hints" \
  test/datascript/upstream/built_ins.cljc \
  '\^(:[[:alpha:]]|[[:upper:]])'
expect_no_text "PSS algorithms contain no inline type hints" \
  datascript/me/tonsky/persistent_sorted_set.cljc \
  '\^(:[[:alpha:]]|[[:upper:]])'
expect_no_algorithm_hints "entity algorithms contain no local type hints" \
  test/datascript/upstream/entity.cljc \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between \
  "pull option constructors contain no local type hints" \
  test/datascript/upstream/pull_api.cljc \
  pull-options pulled-to-data \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between "pull cursor algorithms contain no local type hints" \
  test/datascript/upstream/pull_api.cljc \
  pulled-to-data visit \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between \
  "pull visitor and forward cursor algorithms contain no local type hints" \
  test/datascript/upstream/pull_api.cljc \
  visit attrs-state \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between \
  "pull attrs and reference frame algorithms contain no local type hints" \
  test/datascript/upstream/pull_api.cljc \
  attrs-state ref-datom-id \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between \
  "pull multivalue reference frame algorithms contain no local type hints" \
  test/datascript/upstream/pull_api.cljc \
  ref-datom-id attr-at-index \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between \
  "pull result merging algorithms contain no local type hints" \
  test/datascript/upstream/pull_api.cljc \
  attr-at-index attrs-state-with \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between \
  "pull attribute state helpers contain no local type hints" \
  test/datascript/upstream/pull_api.cljc \
  attrs-state-with run-wildcard-attr \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between \
  "pull attribute runner contains no local type hints" \
  test/datascript/upstream/pull_api.cljc \
  run-wildcard-attr advance-reverse-state \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between \
  "pull reverse attribute runner contains no local type hints" \
  test/datascript/upstream/pull_api.cljc \
  advance-reverse-state run-frame \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between \
  "pull frame dispatch helpers contain no local type hints" \
  test/datascript/upstream/pull_api.cljc \
  run-frame first-frame \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between \
  "pull stack execution helpers contain no local type hints" \
  test/datascript/upstream/pull_api.cljc \
  first-frame parse-opts \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between \
  "pull parsed and source helpers contain no local type hints" \
  test/datascript/upstream/pull_api.cljc \
  parse-opts pull \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between \
  "pull public and pull-many helpers contain no local type hints" \
  test/datascript/upstream/pull_api.cljc \
  pull pull-many \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between \
  "pull-many public arities contain no local type hints" \
  test/datascript/upstream/pull_api.cljc \
  pull-many __end_of_file__ \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_text \
  "pull constructor and frame signatures remain inferred" \
  test/datascript/upstream/pull_api.cljc \
  '^\(signature datascript\.pull-api/(pull-options|expanding-ref-frame|run-multival-ref-frame)[[:space:]]*$'
expect_no_hints_between \
  "pull parser source constructors contain no local type hints" \
  test/datascript/upstream/pull_parser.cljc \
  attr-name-spec source-alias \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between \
  "pull parser source option constructors contain no local type hints" \
  test/datascript/upstream/pull_parser.cljc \
  source-alias attribute \
  '\^(:[[:alpha:]]|[[:alpha:]])'
expect_no_hints_between \
  "pull parser attribute accessors contain no local type hints" \
  test/datascript/upstream/pull_parser.cljc \
  attribute parse-attr-name \
  '\^(:[[:alpha:]]|[[:alpha:]])'

for mapping in \
  'src/datascript/query.cljc.*test/datascript/lg/query.cljc' \
  'src/datascript/db.cljc.*test/datascript/upstream/db.cljc' \
  'src/datascript/pull_api.cljc.*test/datascript/upstream/pull_api.cljc' \
  'src/datascript/impl/entity.cljc.*test/datascript/upstream/entity.cljc' \
  'src/datascript/conn.cljc.*test/datascript/upstream/conn.cljc' \
  'src/datascript/storage.clj'
do
  expect_text "file mapping is recorded: $mapping" "$upstream_doc" "$mapping"
done

expect_file "the upstream API manifest is checked in" "$api_manifest"
expect_file "the LG API manifest is checked in" "$lg_api_manifest"
expect_file "the upstream API manifest generator exists" "$generate_script"
if [ -f "$repo_root/$generate_script" ] &&
   [ -f "$repo_root/$lg_api_manifest" ]; then
  expect_success "the LG API manifest is reproducible from LG sources" \
    sh -c 'cd "$1" &&
      bb "$2" --lg . |
      cmp - "$3"' \
    sh "$repo_root" "$generate_script" "$lg_api_manifest"
fi
for kind in var arity protocol option source-form tagged-reader; do
  expect_text "the API manifest records $kind entries" "$api_manifest" \
    "^$kind	"
done
for arity in \
  'arity	datascript.core/transact!	2	fixed' \
  'arity	datascript.core/transact!	3	fixed' \
  'arity	datascript.core/with	2	fixed' \
  'arity	datascript.core/with	3	fixed'
do
  expect_text "the LG manifest records a multi-arity definition: $arity" \
    "$lg_api_manifest" "^$arity$"
done
expect_text "the LG manifest preserves the public pull visitor option" \
  "$lg_api_manifest" '^option	datascript\.pull-api/parse-opts	:visitor$'
expect_text "the LG manifest preserves the public data-readers registry" \
  "$lg_api_manifest" '^var	datascript\.core/data-readers$'
for query_helper_entry in \
  'var	datascript.query/map\*' \
  'arity	datascript.query/map\*	2	fixed' \
  'var	datascript.query/-group-by' \
  'arity	datascript.query/-group-by	3	fixed' \
  'var	datascript.query/hash-attrs' \
  'arity	datascript.query/hash-attrs	2	fixed' \
  'var	datascript.query/getter-fn' \
  'arity	datascript.query/getter-fn	2	fixed' \
  'var	datascript.query/tuple-key-fn' \
  'arity	datascript.query/tuple-key-fn	2	fixed' \
  'var	datascript.query/-resolve-clause' \
  'arity	datascript.query/-resolve-clause	2	fixed' \
  'arity	datascript.query/-resolve-clause	3	fixed' \
  'var	datascript.query/resolve-clause' \
  'arity	datascript.query/resolve-clause	2	fixed' \
  'var	datascript.query/-q' \
  'arity	datascript.query/-q	2	fixed' \
  'var	datascript.query/filter-by-pred' \
  'arity	datascript.query/filter-by-pred	2	fixed' \
  'var	datascript.query/bind-by-fn' \
  'arity	datascript.query/bind-by-fn	2	fixed' \
  'var	datascript.query/-call-fn' \
  'arity	datascript.query/-call-fn	4	fixed' \
  'var	datascript.query/solve-rule' \
  'arity	datascript.query/solve-rule	2	fixed' \
  'var	datascript.query/rule-seqid' \
  'var	datascript.query/expand-rule' \
  'arity	datascript.query/expand-rule	3	fixed'
do
  expect_text "the LG manifest preserves query helper: $query_helper_entry" \
    "$lg_api_manifest" "^$query_helper_entry$"
done
for query_v3_entry in \
  'var	datascript.query-v3/lru-cache-size' \
  'var	datascript.query-v3/mapa' \
  'arity	datascript.query-v3/mapa	2	fixed' \
  'var	datascript.query-v3/arange' \
  'arity	datascript.query-v3/arange	2	fixed' \
  'var	datascript.query-v3/subarr' \
  'arity	datascript.query-v3/subarr	3	fixed' \
  'var	datascript.query-v3/concatv' \
  'arity	datascript.query-v3/concatv	0	variadic' \
  'var	datascript.query-v3/zip' \
  'arity	datascript.query-v3/zip	2	fixed' \
  'arity	datascript.query-v3/zip	2	variadic' \
  'var	datascript.query-v3/has\?' \
  'arity	datascript.query-v3/has\?	2	fixed' \
  'var	datascript.query-v3/IRelation' \
  'protocol	datascript.query-v3/IRelation	-alter-coll' \
  'protocol	datascript.query-v3/IRelation	-arity' \
  'protocol	datascript.query-v3/IRelation	-copy-tuple' \
  'protocol	datascript.query-v3/IRelation	-fold' \
  'protocol	datascript.query-v3/IRelation	-getter' \
  'protocol	datascript.query-v3/IRelation	-indexes' \
  'protocol	datascript.query-v3/IRelation	-project' \
  'protocol	datascript.query-v3/IRelation	-size' \
  'protocol	datascript.query-v3/IRelation	-symbols' \
  'protocol	datascript.query-v3/IRelation	-union' \
  'arity	datascript.query-v3/IRelation/-alter-coll	2	fixed' \
  'arity	datascript.query-v3/IRelation/-arity	1	fixed' \
  'arity	datascript.query-v3/IRelation/-copy-tuple	5	fixed' \
  'arity	datascript.query-v3/IRelation/-fold	3	fixed' \
  'arity	datascript.query-v3/IRelation/-getter	2	fixed' \
  'arity	datascript.query-v3/IRelation/-indexes	2	fixed' \
  'arity	datascript.query-v3/IRelation/-project	2	fixed' \
  'arity	datascript.query-v3/IRelation/-size	1	fixed' \
  'arity	datascript.query-v3/IRelation/-symbols	1	fixed' \
  'arity	datascript.query-v3/IRelation/-union	2	fixed' \
  'var	datascript.query-v3/array-rel' \
  'arity	datascript.query-v3/array-rel	2	fixed' \
  'var	datascript.query-v3/coll-rel' \
  'arity	datascript.query-v3/coll-rel	2	fixed' \
  'var	datascript.query-v3/singleton-rel' \
  'var	datascript.query-v3/product' \
  'arity	datascript.query-v3/product	2	fixed' \
  'var	datascript.query-v3/product-all' \
  'arity	datascript.query-v3/product-all	1	fixed' \
  'var	datascript.query-v3/hash-map-rel' \
  'arity	datascript.query-v3/hash-map-rel	2	fixed' \
  'var	datascript.query-v3/hash-join' \
  'arity	datascript.query-v3/hash-join	4	fixed' \
  'var	datascript.query-v3/empty-context' \
  'var	datascript.query-v3/related-rels' \
  'arity	datascript.query-v3/related-rels	2	fixed' \
  'var	datascript.query-v3/extract-rels' \
  'arity	datascript.query-v3/extract-rels	2	fixed' \
  'var	datascript.query-v3/join-unrelated' \
  'arity	datascript.query-v3/join-unrelated	2	fixed' \
  'var	datascript.query-v3/hash-join-rel' \
  'arity	datascript.query-v3/hash-join-rel	2	fixed' \
  'var	datascript.query-v3/get-source' \
  'arity	datascript.query-v3/get-source	2	fixed' \
  'var	datascript.query-v3/resolve-pattern-db' \
  'arity	datascript.query-v3/resolve-pattern-db	2	fixed' \
  'var	datascript.query-v3/resolve-pattern-coll' \
  'arity	datascript.query-v3/resolve-pattern-coll	2	fixed' \
  'var	datascript.query-v3/resolve-pattern' \
  'arity	datascript.query-v3/resolve-pattern	2	fixed' \
  'var	datascript.query-v3/clause-syms' \
  'arity	datascript.query-v3/clause-syms	1	fixed' \
  'var	datascript.query-v3/substitute-constants' \
  'arity	datascript.query-v3/substitute-constants	2	fixed' \
  'var	datascript.query-v3/collect-args!' \
  'arity	datascript.query-v3/collect-args!	4	fixed' \
  'var	datascript.query-v3/get-f' \
  'arity	datascript.query-v3/get-f	3	fixed' \
  'var	datascript.query-v3/resolve-predicate' \
  'arity	datascript.query-v3/resolve-predicate	2	fixed' \
  'var	datascript.query-v3/resolve-function' \
  'arity	datascript.query-v3/resolve-function	2	fixed' \
  'var	datascript.query-v3/project-rel' \
  'arity	datascript.query-v3/project-rel	2	fixed' \
  'var	datascript.query-v3/project-context' \
  'arity	datascript.query-v3/project-context	2	fixed' \
  'var	datascript.query-v3/check-bound' \
  'arity	datascript.query-v3/check-bound	3	fixed' \
  'var	datascript.query-v3/upd-default-source' \
  'arity	datascript.query-v3/upd-default-source	2	fixed' \
  'var	datascript.query-v3/collect-opt' \
  'arity	datascript.query-v3/collect-opt	2	fixed' \
  'var	datascript.query-v3/subtract-from-rel' \
  'arity	datascript.query-v3/subtract-from-rel	3	fixed' \
  'var	datascript.query-v3/subtract-contexts' \
  'arity	datascript.query-v3/subtract-contexts	3	fixed' \
  'var	datascript.query-v3/resolve-not' \
  'arity	datascript.query-v3/resolve-not	2	fixed' \
  'var	datascript.query-v3/resolve-clauses' \
  'arity	datascript.query-v3/resolve-clauses	2	fixed' \
  'var	datascript.query-v3/resolve-or' \
  'arity	datascript.query-v3/resolve-or	2	fixed' \
  'var	datascript.query-v3/IClause' \
  'protocol	datascript.query-v3/IClause	-resolve-clause' \
  'arity	datascript.query-v3/IClause/-resolve-clause	2	fixed' \
  'var	datascript.query-v3/bind' \
  'arity	datascript.query-v3/bind	2	fixed' \
  'var	datascript.query-v3/resolve-ins' \
  'arity	datascript.query-v3/resolve-ins	3	fixed' \
  'var	datascript.query-v3/collect-consts' \
  'arity	datascript.query-v3/collect-consts	3	fixed' \
  'var	datascript.query-v3/collect-rel-xf' \
  'arity	datascript.query-v3/collect-rel-xf	2	fixed' \
  'var	datascript.query-v3/collect-to' \
  'arity	datascript.query-v3/collect-to	3	fixed' \
  'arity	datascript.query-v3/collect-to	4	fixed' \
  'arity	datascript.query-v3/collect-to	5	fixed' \
  'var	datascript.query-v3/q' \
  'arity	datascript.query-v3/q	1	variadic'
do
  expect_text "the LG manifest preserves query-v3 helper: $query_v3_entry" \
    "$lg_api_manifest" "^$query_v3_entry$"
done
for query_clause_option in \
  'option	datascript.query-v3/resolve-not	:clauses' \
  'option	datascript.query-v3/resolve-not	:source' \
  'option	datascript.query-v3/resolve-not	:vars' \
  'option	datascript.query-v3/resolve-or	:clauses' \
  'option	datascript.query-v3/resolve-or	:free' \
  'option	datascript.query-v3/resolve-or	:required' \
  'option	datascript.query-v3/resolve-or	:rule-vars' \
  'option	datascript.query-v3/resolve-or	:source'
do
  expect_text "the LG manifest preserves closed query clause option: $query_clause_option" \
    "$lg_api_manifest" "^$query_clause_option$"
done
expect_text "the LG manifest preserves query helper: *implicit-source*" \
  "$lg_api_manifest" '^var	datascript\.query/\*implicit-source\*$'
expect_text "the LG manifest preserves query helper: *lookup-attrs*" \
  "$lg_api_manifest" '^var	datascript\.query/\*lookup-attrs\*$'
expect_text "the LG manifest preserves query helper: *query-cache*" \
  "$lg_api_manifest" '^var	datascript\.query/\*query-cache\*$'
for js_adapter_entry in \
  'var	datascript.js/serializable' \
  'var	datascript.js/from_serializable' \
  'var	datascript.js/touch' \
  'var	datascript.js/entity_db' \
  'var	datascript.js/filter' \
  'var	datascript.js/is_filtered' \
  'var	datascript.js/conn_from_db' \
  'var	datascript.js/conn_from_datoms' \
  'arity	datascript.js/conn_from_datoms	1	fixed' \
  'arity	datascript.js/conn_from_datoms	2	fixed' \
  'var	datascript.js/create_conn' \
  'arity	datascript.js/create_conn	0	variadic' \
  'var	datascript.js/datoms' \
  'arity	datascript.js/datoms	2	variadic' \
  'var	datascript.js/db' \
  'arity	datascript.js/db	1	fixed' \
  'var	datascript.js/db_with' \
  'arity	datascript.js/db_with	2	fixed' \
  'var	datascript.js/empty_db' \
  'arity	datascript.js/empty_db	0	variadic' \
  'var	datascript.js/entity' \
  'arity	datascript.js/entity	2	fixed' \
  'var	datascript.js/init_db' \
  'arity	datascript.js/init_db	1	variadic' \
  'var	datascript.js/index_range' \
  'arity	datascript.js/index_range	4	fixed' \
  'var	datascript.js/js->Datom' \
  'arity	datascript.js/js->Datom	1	fixed' \
  'var	datascript.js/listen' \
  'var	datascript.js/unlisten' \
  'var	datascript.js/pull' \
  'arity	datascript.js/pull	3	fixed' \
  'var	datascript.js/pull_many' \
  'arity	datascript.js/pull_many	3	fixed' \
  'var	datascript.js/q' \
  'arity	datascript.js/q	1	variadic' \
  'var	datascript.js/resolve_tempid' \
  'arity	datascript.js/resolve_tempid	2	fixed' \
  'var	datascript.js/reset_conn' \
  'arity	datascript.js/reset_conn	2	variadic' \
  'var	datascript.js/squuid' \
  'arity	datascript.js/squuid	0	fixed' \
  'var	datascript.js/squuid_time_millis' \
  'arity	datascript.js/squuid_time_millis	1	fixed' \
  'var	datascript.js/seek_datoms' \
  'arity	datascript.js/seek_datoms	2	variadic' \
  'var	datascript.js/transact' \
  'arity	datascript.js/transact	2	variadic'
do
  expect_text "the LG manifest preserves typed JS adapter: $js_adapter_entry" \
    "$lg_api_manifest" "^$js_adapter_entry$"
done
for inline_entry in \
  'var	datascript.inline/assoc' \
  'arity	datascript.inline/assoc	3	fixed' \
  'arity	datascript.inline/assoc	3	variadic' \
  'arity	datascript.inline/update	3	fixed' \
  'arity	datascript.inline/update	4	fixed' \
  'arity	datascript.inline/update	5	fixed' \
  'arity	datascript.inline/update	6	fixed' \
  'arity	datascript.inline/update	6	variadic'
do
  expect_text "the LG manifest preserves inline API: $inline_entry" \
    "$lg_api_manifest" "^$inline_entry$"
done
for restore_option in \
  'option	datascript.db/db-from-reader	:datoms' \
  'option	datascript.db/db-from-reader	:schema' \
  'option	datascript.db/restore-db	:aevt' \
  'option	datascript.db/restore-db	:avet' \
  'option	datascript.db/restore-db	:eavt' \
  'option	datascript.db/restore-db	:max-eid' \
  'option	datascript.db/restore-db	:max-tx' \
  'option	datascript.db/restore-db	:schema' \
  'option	datascript.storage/restore-impl	:aevt' \
  'option	datascript.storage/restore-impl	:aevt-metadata' \
  'option	datascript.storage/restore-impl	:avet' \
  'option	datascript.storage/restore-impl	:avet-metadata' \
  'option	datascript.storage/restore-impl	:eavt' \
  'option	datascript.storage/restore-impl	:eavt-metadata' \
  'option	datascript.storage/restore-impl	:max-addr' \
  'option	datascript.storage/restore-impl	:max-eid' \
  'option	datascript.storage/restore-impl	:max-tx' \
  'option	datascript.storage/restore-impl	:schema'
do
  expect_text "the LG manifest preserves closed restore option: $restore_option" \
    "$lg_api_manifest" "^$restore_option$"
done

expect_file "the API manifest comparator exists" "$compare_script"
if [ -f "$repo_root/$compare_script" ]; then
  expect_success "an exact API manifest is accepted" \
    sh "$repo_root/$compare_script" \
    "$repo_root/test/datascript/api_manifest/test/exact-upstream.tsv" \
    "$repo_root/test/datascript/api_manifest/test/exact-lg.tsv"
  expect_failure "a missing public API is rejected" \
    sh "$repo_root/$compare_script" \
    "$repo_root/test/datascript/api_manifest/test/exact-upstream.tsv" \
    "$repo_root/test/datascript/api_manifest/test/missing-lg.tsv"
  expect_failure "a narrowed public arity is rejected" \
    sh "$repo_root/$compare_script" \
    "$repo_root/test/datascript/api_manifest/test/exact-upstream.tsv" \
    "$repo_root/test/datascript/api_manifest/test/narrowed-lg.tsv"
fi

expect_file "the differential behavior catalog exists" "$differential_cases"
expect_file "the differential runner exists" "$differential_runner"
for category in \
  query entity connection pull transaction pss serialization; do
  expect_text "the differential catalog covers $category" "$differential_cases" \
    "^$category	"
done
expect_text "the differential runner validates known differences" \
  "$differential_runner" 'known-difference\)'
expect_text "the known-difference lifecycle is documented" "$upstream_doc" \
  'known-difference.*row'

expect_text "the design contract pins the upstream commit" docs/design.md \
  '3f141af97b70e1f14c65eaa119acd822ebece37e'
expect_text "annotation difficulty cannot justify missing behavior" docs/design.md \
  '[Aa]nnotation difficulty.*(does not|cannot|must not).*missing'

if [ "$failures" -ne 0 ]; then
  echo "Phase 1 contract failures: $failures" >&2
  exit 1
fi

echo "Phase 1 contract checks passed"
