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
| `src/datascript/conn.cljc` | `test/datascript/upstream/conn.cljc` |
| `src/datascript/storage.clj` | `test/datascript/upstream/storage_file.cljc` |
| `src/datascript/storage.cljs` | `test/datascript/upstream/storage.cljc` |
| `src/datascript/core.cljc` | `test/datascript/upstream/core.cljc` |
| `src/datascript/serialize.cljc` | `test/datascript/upstream/serialize.cljc` |
| `src/datascript/pull_parser.cljc` | `test/datascript/upstream/pull_parser.cljc` |
| `src/datascript/built_ins.cljc` | `test/datascript/upstream/built_ins.cljc` |
| `src/datascript/js.cljs` | `test/datascript/lg/js.cljc` |

Persistent sorted set is maintained in the same repository under
`datascript/me/tonsky/`; it is a typed port of the corresponding upstream
dependency rather than a file in the Logseq DataScript fork.

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

## Initial parity matrix

| Area | Classification | Phase 1 evidence |
|---|---|---|
| Query | Missing behavior | Differential cases cover duplicate projection, inputs, rules, compound clauses, aggregation, pull, and return maps. |
| Database/transaction | Missing behavior | Upstream transaction tests are inventoried in `upstream_tests.tsv`; closed transaction forms remain incomplete. |
| Pull/entity | Missing behavior | Entity references and pull options/visitors are named differential cases. |
| Connection/storage | Target boundary plus missing behavior | Native and Melange use typed storage adapters and `Strong | Weak`; option propagation remains a compatibility task. |
| Serialization | Representation-only plus missing custom codec behavior | Closed payloads exist; custom callback arities remain in the API manifest. |
| PSS | Measured optimization | The permitted iterator, frame, and storage-policy differences are documented in `docs/design.md`. |

The full behavior inventory is in `differential/cases.tsv` and
`upstream_status.tsv`. No missing row is treated as target-host permission.
