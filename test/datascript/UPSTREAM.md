# DataScript upstream baseline

The compatibility baseline is the Logseq DataScript fork at
<https://github.com/logseq/datascript.git>, commit
`3f141af97b70e1f14c65eaa119acd822ebece37e` (`3f141af`, 2026-05-03).
All upstream comparisons and generated manifests must use this exact commit.

To reproduce the source checkout:

```sh
git clone https://github.com/logseq/datascript.git
git -C datascript checkout 3f141af97b70e1f14c65eaa119acd822ebece37e
bb script/generate_datascript_api_manifest.clj datascript \
  > test/datascript/api_manifest/upstream.tsv
bb script/generate_datascript_api_manifest.clj --lg . \
  > test/datascript/api_manifest/lg.tsv
sh script/check_datascript_api_manifest.sh \
  test/datascript/api_manifest/upstream.tsv \
  test/datascript/api_manifest/lg.tsv
```

## Source mapping

| Pinned upstream file | LG file |
|---|---|
| `src/datascript/query.cljc` | `test/datascript/lg/query.cljc` |
| `src/datascript/query_v3.cljc` | `test/datascript/lg/query_v3.cljc` |
| `src/datascript/db.cljc` | `test/datascript/upstream/db.cljc` |
| `src/datascript/pull_api.cljc` | `test/datascript/upstream/pull_api.cljc` |
| `src/datascript/impl/entity.cljc` | `test/datascript/upstream/entity.cljc` |
| `src/datascript/datafy.cljc` | `test/datascript/lg/datafy.cljc` |
| `src/datascript/conn.cljc` | `test/datascript/upstream/conn.cljc` |
| `src/datascript/storage.clj` | `test/datascript/upstream/storage.cljc`, `test/datascript/upstream/storage_file.cljc` |
| `src/datascript/storage.cljs` | `test/datascript/upstream/storage.cljc` |
| `src/datascript/core.cljc` | `test/datascript/upstream/core.cljc` |
| `src/datascript/serialize.cljc` | `test/datascript/upstream/serialize.cljc` |
| `src/datascript/pull_parser.cljc` | `test/datascript/upstream/pull_parser.cljc` |
| `src/datascript/built_ins.cljc` | `test/datascript/upstream/built_ins.cljc` |
| `src/datascript/js.cljs` | `test/datascript/lg/js.cljc` |

Persistent sorted set is maintained in the same repository under
`datascript/me/tonsky/`; it is a typed port of the corresponding upstream
dependency rather than a file in the Logseq DataScript fork.

`source_review.tsv` records the definition-level review of every mapped file.
Across the mapping, 280 of 376 upstream top-level definitions retain their
exact names (74.5%). The remaining 96 are reviewed closed-type replacements,
private-helper decompositions, or target-boundary splits; none remains
unreviewed. This exact-name percentage is a maintenance metric, not a behavior
score. Behavior remains governed by the upstream suites, differential catalog,
surface matrix, and public API manifest.

## Machine-checkable artifacts

- `api_manifest/upstream.tsv` records public vars, arities, protocol methods,
  public option keys, accepted source-form families, and tagged reader forms.
- `api_manifest/lg.tsv` is generated from the mapped LG implementation files;
  `check_datascript_phase1_test.sh` verifies that it is reproducible.
- `script/check_datascript_api_manifest.sh` rejects an LG manifest that omits
  or narrows an upstream entry.
- `differential/cases.tsv` names the initial observable-behavior fixtures.
  A `known-difference` row must differ from upstream and cite an existing
  implementation or test file. When behavior is restored, the row becomes
  `parity` and all three outputs must match.

The vendored files are ports, not an alternative authority. Local changes in
another DataScript checkout have no role in comparisons.

## Current parity matrix

| Area | Classification | Current evidence |
|---|---|---|
| Query | Behavior parity with closed representations | Upstream query splits pass on Native and Melange; the surface matrix covers every clause, find, and input family. |
| Database/transaction | Behavior parity with closed representations | Entity maps, operation vectors, raw datoms, transaction functions, upserts, tuple maintenance, component cascades, ordering, and invalid-input cases are covered. |
| Pull/entity | Behavior parity with closed representations | Attribute options, visitors, recursion, cycles, reverse references, missing entities, and parse reuse pass on both targets. |
| Connection/storage | Behavior parity with a target boundary | Connection and storage suites pass. Native and Melange use typed adapters and the documented closed `Strong | Weak` reference policy. |
| Serialization | Behavior parity with measured representation optimizations | Default and custom codecs, schema and option propagation, datom order, index reuse, branching, reference policy, attached-storage rejection, and old payloads are covered. |
| PSS | Behavior parity with measured representation optimizations | Ordering, slices, persistence, lazy traversal, storage callbacks, and reference policy pass; retained iterator differences are documented in `docs/design.md`. |
| Public API | Upstream-complete | The generated comparator reports every upstream var, arity, protocol method, option, source-form family, and tagged reader present in LG. |
| Benchmark | Complete | The final isolated 28-workload upstream/LG Native/LG Melange matrix passes on both LG targets; the latest pinned datascript-ocaml runner also completed all 14 supplemental workloads. |

As of 2026-08-05, the machine-checkable inventory reports 170/170 upstream
tests covered, the differential catalog reports 56/56 cases at parity, and the
surface matrix reports no cataloged behavior missing. `upstream_status.tsv`,
`differential/cases.tsv`, and the generated API manifests remain the
authoritative detailed evidence. A passing inventory is not permission to
remove upstream control flow or replace a closed representation with dynamic
typing; remaining source differences still require the review rules in
`docs/design.md`.

The final benchmark evidence is recorded in `benchmark/RESULTS.md`. Close or
initially failing comparisons were repeated in five isolated, order-reversed
pairs and compared by process medians. Native and Melange pass 28/28 tracked
upstream workloads. The supplemental datascript-ocaml result is stored in
`benchmark/datascript-ocaml-20260805.tsv` and is intentionally reported
separately because its generator, seed, schema, and single-process protocol
differ from the authoritative upstream runner.
