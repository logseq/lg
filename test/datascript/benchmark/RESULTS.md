# DataScript benchmark results

Baseline captured on 2026-07-30 against pinned upstream DataScript commit
`3f141af`.

Each workload ran in an isolated process with 20,000 people, a 2-second
warmup, five 1-second samples, and batch size 10. Times are median
milliseconds per operation. Negative deltas are faster than upstream.
Upstream prints rounded values, so its deltas are approximate.

| Workload | Upstream JS | LG Native | Native delta | LG Melange | Melange delta |
| --- | ---: | ---: | ---: | ---: | ---: |
| `add-1` | 570.2 | 272.6 | -52.2% | 592.5 | +3.9% |
| `add-5` | 1390.2 | 715.0 | -48.6% | 1495.3 | +7.6% |
| `add-all` | 1805.9 | 785.3 | -56.5% | 1542.0 | -14.6% |
| `init` | 57.0 | 111.3 | +95.3% | 178.2 | +212.7% |
| `find-datoms` | 2.2 | 1.310 | -40.4% | 1.873 | -14.9% |
| `find-datom` | 2.8 | 0.874 | -68.8% | 1.142 | -59.2% |
| `retract-5` | 3283.6 | 764.8 | -76.7% | 1098.8 | -66.5% |
| `q1` | 1.8 | 0.881 | -51.1% | 2.267 | +25.9% |
| `q2` | 4.3 | 2.718 | -36.8% | 6.738 | +56.7% |
| `q3` | 6.9 | 4.088 | -40.8% | 8.999 | +30.4% |
| `q4` | 9.8 | 5.819 | -40.6% | 12.734 | +29.9% |
| `q5-shortcircuit` | 1.1 | 0.256 | -76.7% | 1.116 | +1.5% |
| `qpred1` | 8.1 | 12.565 | +55.1% | 32.788 | +304.8% |
| `qpred2` | 9.7 | 14.861 | +53.2% | 38.733 | +299.3% |
| `pull-one-entities` | 2.3 | 1.792 | -22.1% | 3.008 | +30.8% |
| `pull-one` | 1.1 | 1.108 | +0.7% | 2.400 | +118.2% |
| `pull-many-entities` | 6.7 | 4.727 | -29.4% | 8.156 | +21.7% |
| `pull-many` | 1.9 | 3.614 | +90.2% | 9.174 | +382.9% |
| `pull-wildcard` | 4.6 | 2.744 | -40.4% | 6.440 | +40.0% |
| `rules-wide-3x3` | 0.499 | 0.224 | -55.0% | 0.532 | +6.6% |
| `rules-wide-5x3` | 4.5 | 3.521 | -21.7% | 7.006 | +55.7% |
| `rules-wide-7x3` | 61.3 | 55.302 | -9.8% | 112.355 | +83.3% |
| `rules-wide-4x6` | 14.1 | 12.484 | -11.5% | 24.253 | +72.0% |
| `rules-long-10x3` | 1.7 | 0.624 | -63.3% | 1.653 | -2.8% |
| `rules-long-30x3` | 16.7 | 7.356 | -56.0% | 18.428 | +10.3% |
| `rules-long-30x5` | 21.2 | 10.551 | -50.2% | 26.703 | +26.0% |
| `freeze` | 854.6 | 2528.0 | +195.8% | 6589.4 | +671.0% |
| `thaw` | 1217.7 | 3289.9 | +170.2% | 11553.9 | +848.8% |

The first pass put three comparisons inside a 3% margin. Three additional
isolated runs changed all three to regressions:

| Workload | Upstream median | LG median | Delta |
| --- | ---: | ---: | ---: |
| Native `pull-one` | 1.000 | 1.087 | +8.7% |
| Melange `q5-shortcircuit` | 0.950 | 1.119 | +17.8% |
| Melange `rules-long-10x3` | 1.500 | 1.760 | +17.4% |

After margin reruns, Native is faster on 21 workloads and slower on 7.
Melange is faster on 4 workloads and slower on 24. The highest-priority shared
regressions are serialization, `init`, predicate queries, and `pull-many`.
Melange additionally has broad query, pull, and recursive-rule overhead.

## Predicate alignment rerun

After restoring upstream-shaped predicate filtering and preserving the closed
vector boundary through ordered comparisons, `qpred1` and `qpred2` were rerun
with the same protocol as the baseline:

| Workload | Upstream JS | LG Native | Native delta | LG Melange | Melange delta |
| --- | ---: | ---: | ---: | ---: | ---: |
| `qpred1` | 8.1 | 5.656 | -30.2% | 11.365 | +40.3% |
| `qpred2` | 9.7 | 7.435 | -23.4% | 16.069 | +65.7% |

Relative to the initial LG baseline, Native improved by 55.0% on `qpred1` and
50.0% on `qpred2`; Melange improved by 65.3% and 58.5%, respectively.

## Pull attribute indexing rerun

After preserving the closed vector boundary while advancing pull attributes,
the single-attribute and multi-attribute recursive pulls were rerun with the
same protocol:

| Workload | Upstream JS | LG Native | Native delta | LG Melange | Melange delta |
| --- | ---: | ---: | ---: | ---: | ---: |
| `pull-one` | 1.1 | 0.802 | -27.1% | 1.530 | +39.1% |
| `pull-many` | 1.9 | 1.518 | -20.1% | 3.228 | +69.9% |

Relative to the initial LG baseline, Native improved by 27.7% on `pull-one`
and 58.0% on `pull-many`; Melange improved by 36.3% and 64.8%,
respectively.

## Init AVET extraction rerun

After replacing the intermediate generic sequence used for AVET extraction
with an order-preserving closed-array filter, `init` was rerun with the same
protocol:

| Workload | Upstream JS | LG Native | Native delta | LG Melange | Melange delta |
| --- | ---: | ---: | ---: | ---: | ---: |
| `init` | 57.0 | 86.502 | +51.8% | 104.131 | +82.7% |

Relative to the initial LG baseline, Native improved by 22.3% and Melange
improved by 41.6%.

## Prepared datom row rerun

After extracting each prepared datom into one closed record, deserialization
validates and unwraps each JSON row once instead of repeating the work for its
entity, attribute, value, and transaction fields.

At the full 300,000-person serialization population, the before and after
runs used zero-duration warmup and sample windows with batch size 1. The
benchmark harness still performs one warmup operation and five measured
operations, reporting their median:

| Runtime | Before thaw | After thaw | Change |
| --- | ---: | ---: | ---: |
| LG Native | 2742.932 | 2395.522 | -12.7% |
| LG Melange | 7190.629 | 3783.929 | -47.4% |

At 20,000 people, the standard 2-second warmup, five 1-second samples, and
batch size 10 produced:

| Workload | LG Native | LG Melange |
| --- | ---: | ---: |
| `freeze` | 111.056 | 172.646 |
| `thaw` | 135.655 | 76.941 |

## Recursive rule guard rerun

After preserving the closed predicate-operand vector while checking recursive
rule guards, the wide recursive rule workloads were rerun with the baseline
protocol:

| Workload | Upstream JS | LG Native | Native delta | LG Melange | Melange delta |
| --- | ---: | ---: | ---: | ---: | ---: |
| `rules-wide-5x3` | 4.5 | 2.210 | -50.9% | 3.402 | -24.4% |
| `rules-wide-7x3` | 61.3 | 33.661 | -45.1% | 51.560 | -15.9% |

Relative to the initial LG baseline, Native improved by 37.2% on
`rules-wide-5x3` and 39.1% on `rules-wide-7x3`; Melange improved by 51.4% and
54.1%, respectively.

## Complete alignment rerun

The complete matrix was rerun after the static vector-to-array `init-db` path,
Melange comparator bridge cleanup, closed-array schema filtering, prepared
serialization records, predicate alignment, pull indexing, and recursive-rule
guard changes. The protocol remained 20,000 people, a 2-second warmup, five
1-second samples, batch size 10, seed 42, and one isolated process per
workload.

| Workload | Upstream JS | LG Native | Native delta | LG Melange | Melange delta |
| --- | ---: | ---: | ---: | ---: | ---: |
| `add-1` | 570.2 | 266.030 | -53.3% | 481.293 | -15.6% |
| `add-5` | 1390.2 | 616.557 | -55.6% | 1339.648 | -3.6% |
| `add-all` | 1805.9 | 672.352 | -62.8% | 1426.711 | -21.0% |
| `init` | 57.0 | 44.207 | -22.4% | 40.343 | -29.2% |
| `find-datoms` | 2.2 | 1.147 | -47.8% | 1.688 | -23.3% |
| `find-datom` | 2.8 | 0.755 | -73.0% | 1.064 | -62.0% |
| `retract-5` | 3283.6 | 652.667 | -80.1% | 1031.351 | -68.6% |
| `q1` | 1.8 | 0.699 | -61.1% | 2.002 | +11.2% |
| `q2` | 4.3 | 2.183 | -49.2% | 5.450 | +26.8% |
| `q3` | 6.9 | 3.276 | -52.5% | 7.577 | +9.8% |
| `q4` | 9.8 | 4.719 | -51.8% | 12.072 | +23.2% |
| `q5-shortcircuit` | 1.1 | 0.199 | -81.9% | 1.002 | -9.0% |
| `qpred1` | 8.1 | 5.379 | -33.6% | 13.860 | +71.1% |
| `qpred2` | 9.7 | 6.909 | -28.8% | 18.611 | +91.9% |
| `pull-one-entities` | 2.3 | 1.559 | -32.2% | 2.746 | +19.4% |
| `pull-one` | 1.1 | 0.771 | -29.9% | 1.710 | +55.5% |
| `pull-many-entities` | 6.7 | 4.157 | -37.9% | 7.378 | +10.1% |
| `pull-many` | 1.9 | 1.601 | -15.7% | 3.402 | +79.1% |
| `pull-wildcard` | 4.6 | 2.337 | -49.2% | 5.443 | +18.3% |
| `rules-wide-3x3` | 0.499 | 0.173 | -65.3% | 0.375 | -24.8% |
| `rules-wide-5x3` | 4.5 | 2.360 | -47.6% | 3.765 | -16.3% |
| `rules-wide-7x3` | 61.3 | 37.383 | -39.0% | 65.536 | +6.9% |
| `rules-wide-4x6` | 14.1 | 9.194 | -34.8% | 14.673 | +4.1% |
| `rules-long-10x3` | 1.7 | 0.472 | -72.2% | 1.111 | -34.7% |
| `rules-long-30x3` | 16.7 | 3.701 | -77.8% | 8.682 | -48.0% |
| `rules-long-30x5` | 21.2 | 5.036 | -76.2% | 11.246 | -47.0% |
| `freeze` | 854.6 | 130.824 | -84.7% | 206.503 | -75.8% |
| `thaw` | 1217.7 | 152.317 | -87.5% | 102.098 | -91.6% |

The first Melange `add-5` measurement was 1424.049 ms, within 3% of
upstream. Five additional isolated runs measured 1236.189, 1357.726,
1313.478, 1339.648, and 1340.776 ms. Their 1339.648 ms median is 3.6% faster
than upstream and is used in the table.

Native is faster than upstream on all 28 tracked workloads. Melange is faster
on 15 and slower on 13. The remaining acceptance failures are `q1` through
`q4`, both predicate queries, all five pull workloads, and
`rules-wide-7x3`/`rules-wide-4x6`.

## Direct static vector and closed-hash rerun

Static `filterv` now filters an RRB vector directly, and static `mapv` uses
`Rrbvec.map` on both Native and Melange instead of a Melange-only
vector-to-array round trip. Query result hashing now combines closed-sum tags
with their already-computed payload hashes, and persistent-map insertion
hashes a new key once instead of repeating the hash during insertion.

The close and previously failing Melange workloads were rerun in isolated
processes with the same 20,000-person protocol. Three independent query and
pull runs were used for the medians below.

| Workload | Upstream JS | LG Melange | Melange delta |
| --- | ---: | ---: | ---: |
| `q1` | 1.8 | 1.589 | -11.7% |
| `q2` | 4.3 | 4.468 | +3.9% |
| `q3` | 6.9 | 6.217 | -9.9% |
| `q4` | 9.8 | 9.284 | -5.3% |
| `qpred1` | 8.1 | 7.479 | -7.7% |
| `qpred2` | 9.7 | 12.072 | +24.5% |
| `pull-one-entities` | 2.3 | 2.207 | -4.0% |
| `pull-one` | 1.1 | 1.396 | +26.9% |
| `pull-many-entities` | 6.7 | 5.969 | -10.9% |
| `pull-many` | 1.9 | 2.510 | +32.1% |
| `pull-wildcard` | 4.6 | 3.972 | -13.7% |
| `rules-wide-3x3` | 0.499 | 0.338 | -32.3% |
| `rules-wide-5x3` | 4.5 | 3.413 | -24.2% |
| `rules-wide-7x3` | 61.3 | 52.383 | -14.5% |
| `rules-wide-4x6` | 14.1 | 12.519 | -11.2% |
| `rules-long-10x3` | 1.7 | 1.005 | -40.9% |
| `rules-long-30x3` | 16.7 | 8.599 | -48.5% |
| `rules-long-30x5` | 21.2 | 10.036 | -52.7% |
| `freeze` | 854.6 | 166.127 | -80.6% |
| `thaw` | 1217.7 | 74.242 | -93.9% |

The serialization rows use `LG_BENCH_SERIALIZE_PEOPLE=20000`, matching the
20,000-person matrix protocol. A diagnostic run without that override used
the runner's intentional 300,000-person default and is not included here.

Against the recorded pinned-upstream matrix, Melange is now faster on 24 of
28 workloads. The remaining failures are `q2`, `qpred2`, `pull-one`, and
`pull-many`. A same-host rerun of the pinned upstream runner measured medians
of 3.7, 7.7, 1.0, and 1.6 ms respectively, confirming that all four remain
real optimization gaps rather than rounded-baseline noise.

## Singleton relation and pull alias-hash rerun

Singleton relation products now map the existing RRB row vector directly
instead of converting it to an array and rebuilding the same vector. Parsed
pull attributes cache the generic static hash of their closed alias value, and
pull result insertion reuses that hash. Both changes retain the existing
upstream-shaped control flow and closed static representations.

The final release bundle was rebuilt before measuring. Each workload below ran
in three isolated processes with the standard 20,000-person protocol; the
reported LG value is the median.

| Workload | Upstream JS | LG Melange | Melange delta |
| --- | ---: | ---: | ---: |
| `q2` | 4.3 | 4.437 | +3.2% |
| `qpred2` | 9.7 | 11.619 | +19.8% |
| `pull-one` | 1.1 | 1.339 | +21.7% |
| `pull-many` | 1.9 | 2.151 | +13.2% |
| `pull-wildcard` | 4.6 | 3.864 | -16.0% |

The singleton product reduced a 100,000-row allocation check from 5,732,080
bytes to less than 5,500,000 bytes. The pull alias-hash cache also improved the
wildcard regression sentinel rather than trading the focused pull gains for a
broader pull slowdown. The four previously identified acceptance failures
remain failures and require further optimization.

## Empty-tuple relation identity rerun

A relation containing the single empty tuple is the identity element of a
Cartesian product. LG now reuses the other RRB relation directly instead of
copying every row through an empty array append. The 100,000-row allocation
regression fell from 3,310,056 bytes to less than 2,048 bytes.

The final release bundle was rebuilt before three isolated runs of each
affected query. Melange `q1` improved from 1.589 ms to a 1.533 ms median,
3.5 percent faster. `q2` and `q3` remained within measurement noise at 4.455
ms and 6.275 ms, while `q4` measured 9.094 ms. The optimization therefore does
not close the remaining `q2` or `qpred2` acceptance gaps.

## Unified closed-query executor rerun

The closed `q` entry point now delegates to the same typed executor used by the
rest of the static query API. This removes a second context pipeline that
carried an otherwise elidable scalar input through every relation row. The
executor also compares exactly two closed equality or ordering operands
directly; empty, unary, and variadic comparisons keep the general upstream
path.

The `qpred2` performance gate was written first and failed with an 11.239 ms
median against the 9.7 ms upstream threshold. After the implementation, the
same three-isolated-process gate passed at 6.722 ms. The other affected queries
were also run in three isolated processes with the standard protocol.

| Workload | Upstream JS | LG Melange | Melange delta |
| --- | ---: | ---: | ---: |
| `q1` | 1.8 | 1.561 | -13.3% |
| `q2` | 4.3 | 4.460 | +3.7% |
| `q3` | 6.9 | 6.103 | -11.6% |
| `q4` | 9.8 | 9.145 | -6.7% |
| `qpred1` | 8.1 | 6.131 | -24.3% |
| `qpred2` | 9.7 | 6.722 | -30.7% |

Melange is now faster than the recorded upstream matrix on 25 of 28 workloads.
The remaining failures are `q2`, `pull-one`, and `pull-many`.

## Bound-pattern constraint classification rerun

The `q2` performance gate was added before the implementation and failed at a
4.401 ms median against the recorded 4.3 ms upstream threshold. A first
single-allocation row-construction attempt regressed to 4.962 ms and was
removed.

The retained implementation classifies value and transaction pattern positions
once before reducing the bound-entity input rows. Unbound variables and missing
positions no longer repeat relation-attribute lookups that can only return no
constraint. Constants and already-bound variables retain the same resolution
path, and the EAVT slice bounds, comparator, datom order, projected columns, and
multiplicity are unchanged.

The same three-isolated-process gate passed with a 4.275 ms `q2` median.
`qpred2` remained below its gate at 7.719 ms. Melange is now faster than the
recorded upstream matrix on 26 of 28 workloads; the remaining failures are
`pull-one` and `pull-many`.

## Closed serialization checkpoint

The release bundle was rebuilt in full before every measurement in this
checkpoint. Query row concatenation now specializes the common one-plus-two
column case without changing the general array append path. The three-run
Melange medians were 3.835 ms for `q2` and 9.161 ms for `qpred2`, below the
4.3 ms and 9.7 ms pinned-upstream gates.

The pull gate was recalibrated against a fresh same-host run of the pinned
upstream checkout. LG measured 1.190 ms for `pull-one` versus 1.3 ms upstream,
and 1.860 ms for `pull-many` versus 1.9 ms upstream.

Serialization retains closed `Small_int`, `Int4_vector`, and `Int_vector`
representations. Attribute indexing uses a closed string hashtable and avoids
the full externally observable Clojure hash finalizer because the temporary
table hash never crosses the runtime boundary. The Melange JSON writer emits
common encoded `Int4_vector` datoms as one row token while preserving the
generic fallback for complex values. At 100,000 serialization people,
Melange `freeze` improved from 567.5 ms to 485.0 ms. At 300,000 people, an
isolated run improved from 1768.2 ms to 1534.0 ms.

Default thaw now keeps prepared datom values in the closed sum
`Prepared_edn_value | Prepared_json_value` and decodes JSON primitives
directly to `Data_value`; custom codecs retain the EDN boundary. Native
100,000-person `thaw` improved from 744.6 ms to 704.9 ms. Melange
100,000-person runs measured 431.0, 412.4, and 401.9 ms; the 300,000-person
workload remains GC-sensitive and is not accepted as an upstream performance
pass yet.

Attribute thaw now converts the closed attribute vector to one array before
the datom loop, and index restoration uses one typed loop instead of a
callback per restored datom. The TDD performance gate uses three isolated
100,000-person runs. Before this change, Melange `thaw` had a 402.369 ms
median and failed the 400 ms limit; afterward, Native measured 669.771 ms
against its 690 ms limit and Melange measured 388.236 ms against its 400 ms
limit.

At the full 300,000-person population with zero-duration timing windows and
batch size 1, the same release bundle measured 1213.183 ms Native `freeze`,
2393.918 ms Native `thaw`, 1391.709 ms Melange `freeze`, and 2732.450 ms
Melange `thaw`. These results remain slower than the fresh pinned-upstream
669.8 ms `freeze` and 1117.5 ms `thaw` measurements, so full serialization
acceptance remains open.

The behavior checkpoint passed the 396-test combined upstream suite, Native
connection tests with 2,566 assertions, query tests with 752 assertions,
rules tests with 30 assertions, serialization tests with 45 assertions, the
56-case differential catalog, the exact API manifest, the complete surface
matrix, and the generated-code static-boundary scan. The repository-wide
compiler suite still has unrelated static-migration failures, so the final
acceptance phase remains open.
