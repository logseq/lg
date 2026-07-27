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
  'arity	datascript.query/bind-by-fn	2	fixed'
do
  expect_text "the LG manifest preserves query helper: $query_helper_entry" \
    "$lg_api_manifest" "^$query_helper_entry$"
done
expect_text "the LG manifest preserves query helper: *implicit-source*" \
  "$lg_api_manifest" '^var	datascript\.query/\*implicit-source\*$'
expect_text "the LG manifest preserves query helper: *lookup-attrs*" \
  "$lg_api_manifest" '^var	datascript\.query/\*lookup-attrs\*$'
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
