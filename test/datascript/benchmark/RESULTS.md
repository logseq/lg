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

## Attribute serialization lookup checkpoint

A V8 profile of the 100,000-person freeze workload attributed 356 of 379
samples in the datom encoding subtree to `find_attribute_index`. The temporary
attribute index used Clojure string hashing and an OCaml hashtable for every
datom even though the benchmark schema has only a few attributes.

The retained implementation uses one closed dispatch:

- up to eight attributes use a reverse string-array scan;
- larger schemas use a typed string hashtable;
- both branches preserve upstream's last-index result for duplicate
  attributes.

The three-process 100,000-person freeze gate was RED at a 475.431 ms Melange
median. After the change, release medians were 365.102 ms Native and
340.956 ms Melange. The corresponding 300,000-person isolated measurements
were 1177.524 ms Native and 1059.947 ms Melange, versus 1213.183 ms and
1391.709 ms at the preceding checkpoint. Thaw remained inside its focused
gate at 674.613 ms Native and 370.289 ms Melange.

This removes the dominant common-schema lookup cost without making large
schemas quadratic. Full 300,000-person freeze is still slower than the fresh
669.8 ms pinned-upstream measurement, so serialization acceptance remains
open.

## Compact Int4 vector checkpoint

The Melange JSON writer already emits a compact `Int4_vector` row as one
token, but the enclosing vector still emitted every separator as a second
token. The retained path combines the separator with a compact Int4 row while
leaving mixed vectors and rows with non-compact values on the generic writer
path.

The focused 100,000-person gate was RED at a 335.930 ms Melange median.
Afterward, the release median was 319.336 ms against the 330 ms gate; Native
remained green at 362.204 ms. The three 300,000-person Melange samples were
1196.435, 1014.501, and 1023.118 ms, for a 1023.118 ms median. The full
500,000-value JSON writer stress test also passed with a 112 MB Node heap.
Native and Melange thaw stayed inside their existing gates at 675.912 ms and
368.342 ms.

This optimization preserves vector order and the exact row JSON representation
while halving token pushes for the compact EAVT case. Full serialization
acceptance remains open because the 300,000-person freeze median is still
slower than pinned upstream.

## Uncurried JSON array iteration checkpoint

The repeated-freeze CPU profile showed that Melange's generic
`Array.iteri` callback dispatch remained one of the largest non-GC costs in
the JSON writer. The retained implementation uses direct indexed loops for
the closed `Vector` and `Int_vector` cases. Element order, separator placement,
compact `Int4_vector` handling, and the generic fallback branch are unchanged.

The five-process 100,000-person gate was RED at a 322.519 ms Melange median
against the 310 ms limit. The final GREEN rerun measured 301.075 ms, an
approximately 6.6 percent improvement. Native remained green at 361.512 ms.
At 300,000 people, five isolated old-path samples had a 1259.821 ms median;
the direct-loop path measured 1121.379 ms, an approximately 11.0 percent
improvement. The 500,000-value writer stress test passed with a 112 MB Node
heap, and Melange thaw remained green at 370.335 ms.

Exact JSON coverage includes an empty integer vector and a vector that mixes
compact datoms, generic tagged-value datoms, and scalar values. The combined
upstream suite passed 396 tests, and the Native connection suite passed 394
tests containing 2,566 assertions. Full serialization acceptance remains open
because the 300,000-person freeze median is still slower than pinned upstream.

## Compact integer-vector separator checkpoint

The two serialized secondary indexes are closed `Int_vector` values. Their
writer path previously pushed a comma token and an integer token separately
for every element after the first. The retained path prefixes each integer
token with its separator, preserving the exact JSON text while halving token
array growth for these indexes.

The direct-loop path was RED at a 297.196 ms five-process median against the
295 ms Melange gate. The final compact-separator path measured 285.908 ms, an
approximately 3.8 percent improvement. Exact tests cover empty, odd-length,
even-length, zero, and negative integer vectors. The 500,000-value writer
stress test passed with a 112 MB Node heap, and Melange thaw remained green at
370.532 ms.

At 300,000 people, the prior direct-loop median was 1121.379 ms and the compact
separator median was 1114.146 ms. This is within measurement noise, so it is
recorded only as no observed scale regression rather than as a full-scale
performance win. A chunked `slice`/`join` experiment improved freeze further
but made the subsequent thaw workload retain enough allocation pressure to
miss its gate, and a paired-integer string experiment also regressed freeze;
both were removed. Full serialization acceptance remains open.

The behavior checkpoint passed the 396-test combined upstream suite, Native
connection tests with 2,566 assertions, query tests with 752 assertions,
rules tests with 30 assertions, serialization tests with 45 assertions, the
56-case differential catalog, the exact API manifest, the complete surface
matrix, and the generated-code static-boundary scan. The repository-wide
compiler suite still has unrelated static-migration failures, so the final
acceptance phase remains open.

## Fresh same-host release matrix (2026-08-01)

After restoring the public storage path and rebuilding both LG targets with
`--profile release`, all non-serialization workloads were rerun in isolated
processes on the same host. Each process used 20,000 people, a 2-second warmup,
five 1-second samples, batch size 10, and seed 42. The pinned upstream runner
uses the same timing windows and batch size; its output is rounded.

| Workload | Upstream JS | LG Native | Native delta | LG Melange | Melange delta |
| --- | ---: | ---: | ---: | ---: | ---: |
| `add-1` | 493.4 | 270.145 | -45.2% | 467.257 | -5.3% |
| `add-5` | 1197.9 | 612.040 | -48.9% | 1349.085 | +12.6% |
| `add-all` | 1665.5 | 668.136 | -59.9% | 1466.373 | -12.0% |
| `init` | 54.5 | 53.313 | -2.2% | 36.161 | -33.7% |
| `find-datoms` | 1.4 | 0.725 | -48.2% | 1.194 | -14.7% |
| `find-datom` | 1.7 | 0.786 | -53.8% | 0.870 | -48.8% |
| `retract-5` | 2902.3 | 835.361 | -71.2% | 1048.936 | -63.9% |
| `q1` | 1.5 | 0.774 | -48.4% | 1.943 | +29.6% |
| `q2` | 3.5 | 1.976 | -43.5% | 4.051 | +15.7% |
| `q3` | 5.2 | 2.790 | -46.4% | 6.352 | +22.1% |
| `q4` | 7.6 | 4.034 | -46.9% | 8.843 | +16.4% |
| `q5-shortcircuit` | 0.784 | 0.217 | -72.4% | 1.024 | +30.7% |
| `qpred1` | 7.4 | 4.850 | -34.5% | 7.764 | +4.9% |
| `qpred2` | 8.7 | 6.468 | -25.7% | 11.559 | +32.9% |
| `pull-one-entities` | 1.8 | 1.307 | -27.4% | 2.208 | +22.7% |
| `pull-one` | 1.1 | 0.678 | -38.4% | 1.296 | +17.8% |
| `pull-many-entities` | 4.9 | 3.267 | -33.3% | 5.898 | +20.4% |
| `pull-many` | 2.1 | 1.311 | -37.6% | 2.068 | -1.5% |
| `pull-wildcard` | 4.5 | 1.990 | -55.8% | 3.871 | -14.0% |
| `rules-wide-3x3` | 0.484 | 0.136 | -72.0% | 0.304 | -37.1% |
| `rules-wide-5x3` | 4.7 | 1.704 | -63.7% | 2.801 | -40.4% |
| `rules-wide-7x3` | 62.9 | 25.654 | -59.2% | 43.933 | -30.2% |
| `rules-wide-4x6` | 14.3 | 6.231 | -56.4% | 9.449 | -33.9% |
| `rules-long-10x3` | 1.7 | 0.391 | -77.0% | 0.989 | -41.8% |
| `rules-long-30x3` | 16.1 | 3.026 | -81.2% | 7.551 | -53.1% |
| `rules-long-30x5` | 21.6 | 3.978 | -81.6% | 9.246 | -57.2% |

Serialization was compared separately at the pinned runner's fixed 300,000
people, with identical timing windows and batch size:

| Workload | Upstream JS | LG Native | Native delta | LG Melange | Melange delta |
| --- | ---: | ---: | ---: | ---: | ---: |
| `freeze` | 680.8 | 480.609 | -29.4% | 961.423 | +41.2% |
| `thaw` | 1134.9 | 1171.422 | +3.2% | 1254.921 | +10.6% |

The LG-only 20,000-person serialization rows were 32.344 ms Native and 63.036
ms Melange for `freeze`, and 73.459 ms Native and 50.590 ms Melange for
`thaw`. They are not compared with upstream because changing the pinned
runner's fixed 300,000-person serialization population would alter the
authoritative benchmark harness.

## Post-annotation-cleanup release matrix (2026-08-02)

After reducing algorithm-local DataScript hints to 40, both LG release
runners were rebuilt and every workload was run in an isolated process. The
non-serialization workloads used 20,000 people, a 2-second warmup, five
1-second samples, batch size 10, and seed 42. Serialization used the pinned
runner's authoritative 300,000-person population with the same timing
parameters. The upstream column is the same-host pinned `3f141af` baseline
recorded on 2026-08-01.

| Workload | Upstream JS | LG Native | Native delta | LG Melange | Melange delta |
| --- | ---: | ---: | ---: | ---: | ---: |
| `add-1` | 493.400 | 273.134 | -44.6% | 473.373 | -4.1% |
| `add-5` | 1197.900 | 622.445 | -48.0% | 1122.661 | -6.3% |
| `add-all` | 1665.500 | 676.718 | -59.4% | 1230.350 | -26.1% |
| `init` | 54.500 | 53.457 | -1.9% | 32.520 | -40.3% |
| `find-datoms` | 1.400 | 0.694 | -50.4% | 0.953 | -31.9% |
| `find-datom` | 1.700 | 0.535 | -68.5% | 0.703 | -58.6% |
| `retract-5` | 2902.300 | 664.793 | -77.1% | 891.260 | -69.3% |
| `q1` | 1.500 | 0.632 | -57.9% | 1.008 | -32.8% |
| `q2` | 3.500 | 1.584 | -54.8% | 2.524 | -27.9% |
| `q3` | 5.200 | 2.213 | -57.4% | 4.066 | -21.8% |
| `q4` | 7.600 | 3.131 | -58.8% | 5.480 | -27.9% |
| `q5-shortcircuit` | 0.784 | 0.171 | -78.2% | 0.417 | -46.8% |
| `qpred1` | 7.400 | 3.979 | -46.2% | 5.482 | -25.9% |
| `qpred2` | 8.700 | 5.228 | -39.9% | 7.990 | -8.2% |
| `pull-one-entities` | 1.800 | 1.192 | -33.8% | 1.580 | -12.2% |
| `pull-one` | 1.100 | 0.481 | -56.2% | 0.837 | -23.9% |
| `pull-many-entities` | 4.900 | 3.067 | -37.4% | 4.677 | -4.5% |
| `pull-many` | 2.100 | 1.115 | -46.9% | 1.648 | -21.5% |
| `pull-wildcard` | 4.500 | 1.859 | -58.7% | 3.546 | -21.2% |
| `rules-wide-3x3` | 0.484 | 0.134 | -72.2% | 0.280 | -42.1% |
| `rules-wide-5x3` | 4.700 | 1.587 | -66.2% | 2.556 | -45.6% |
| `rules-wide-7x3` | 62.900 | 23.467 | -62.7% | 37.442 | -40.5% |
| `rules-wide-4x6` | 14.300 | 5.862 | -59.0% | 8.512 | -40.5% |
| `rules-long-10x3` | 1.700 | 0.397 | -76.7% | 0.901 | -47.0% |
| `rules-long-30x3` | 16.100 | 3.110 | -80.7% | 6.786 | -57.9% |
| `rules-long-30x5` | 21.600 | 3.977 | -81.6% | 8.445 | -60.9% |
| `freeze` | 680.800 | 468.616 | -31.2% | 772.063 | +13.4% |
| `thaw` | 1134.900 | 1049.509 | -7.5% | 981.915 | -13.5% |

All 26 non-serialization workloads remain faster than pinned upstream on both
LG targets. Native is also faster for both serialization workloads. Melange
`thaw` is faster, while Melange `freeze` remains the only tracked regression
at +13.4%.

Native is faster on all 26 non-serialization workloads and on full-scale
freeze; full-scale thaw is within 3.2 percent but remains a failure. Melange is
faster on 15 of 26 non-serialization workloads. Its confirmed release-matrix
failures are `add-5`, `q1` through `q5-shortcircuit`, `qpred1`, `qpred2`,
`pull-one-entities`, `pull-one`, `pull-many-entities`, full-scale `freeze`, and
full-scale `thaw`. Close results require independent reruns before an
implementation change is accepted.

## Prepared datom cursor checkpoint (2026-08-02)

The full-scale thaw gate previously matched the same closed prepared datom
once for entity, attribute, value, and transaction, then matched the value
again for default decoding. The retained implementation reads each row once
into one reusable typed cursor. It does not allocate a row record, introduce a
dynamic value, or change restoration order. Custom codecs still receive the
same closed EDN value.

The Native 300,000-person median improved from 1177.197 ms to 1045.487 ms.
Melange measured 1124.227 ms. Both pass the pinned 1134.9 ms upstream gate;
the Melange margin is approximately 0.9 percent and therefore remains subject
to final acceptance reruns. Native and Melange serialization behavior tests
also passed (9 tests on each target, 45 Melange assertions).

Full serialization acceptance remains open because Melange `freeze` is still
slower than upstream. The last rebuilt full-scale median was approximately
760 ms against 680.8 ms upstream. Larger integer-vector chunks, a string-rope
writer, native-map string interning, and alternate small-schema attribute
indexing either regressed `freeze`, regressed `thaw`, or added complexity
without a measured improvement and were removed.

## Full serialization acceptance checkpoint (2026-08-03)

The remaining Melange `freeze` regression was split into closed-value
construction and JSON writing before changing the implementation. At 300,000
people, isolated samples attributed roughly 330 ms to `serializable` and 439
ms to JSON writing. The retained implementation makes four local changes:

- immutable `Small_int` values from 0 through 127 are shared by the
  serialization encoder;
- the Melange JSON writer shares the corresponding integer text tokens;
- compact `Int4_array` rows are joined through one reusable 4,096-row buffer;
- repeated entity prefixes and the common `,attribute,` and `,tx]` fragments
  are reused while preserving the generic mixed-value fallback.

The source representation remains the closed `Int4_array` sum case. No
`Runtime_dynamic.t`, unsafe cast, source conversion escape hatch, or compiler
special case was added. Exact JSON tests cover empty arrays, cached and
uncached integers, repeated entities, mixed compact and generic values, and
unequal column lengths.

The final release-profile full-scale gate used three isolated processes per
runtime and workload, 300,000 people, zero-duration timing windows, batch size
1, and seed 42:

| Workload | Upstream JS | LG Native | Native delta | LG Melange | Melange delta |
| --- | ---: | ---: | ---: | ---: | ---: |
| `freeze` | 680.800 | 514.552 | -24.4% | 612.537 | -10.0% |
| `thaw` | 1134.900 | 1041.185 | -8.3% | 997.071 | -12.1% |

All four full-scale serialization comparisons now pass. A chunk size of
8,192 regressed the focused JSON measurement, and constructing temporary JS
JSON row arrays nearly doubled it; both experiments were removed before the
final gate.

## Final typed-boundary verification (2026-08-03)

After reducing the reviewed inventory to 4 algorithm-local annotations and
91 closed declaration annotations, the complete DataScript upstream alias,
the cross-runtime scaling gates, and the full-scale serialization gate were
rebuilt serially. No dynamic boundary or compiler special case was added.

The full-scale serialization gate used the pinned 300,000-person workload and
the recorded upstream baseline:

| Workload | Upstream JS | LG Native | Native delta | LG Melange | Melange delta |
| --- | ---: | ---: | ---: | ---: | ---: |
| `freeze` | 680.800 | 528.994 | -22.3% | 588.983 | -13.5% |
| `thaw` | 1134.900 | 1071.185 | -5.6% | 964.377 | -15.0% |

All four comparisons pass. The scaling checks also passed: `add-all` grew by
4.59 times when the population grew from 1,000 to 4,000; the Melange/Native
`init` ratio was 0.56; the Melange/Native `freeze` ratio was 1.37; and the
wide recursive-rule 5x3-to-7x3 ratios were 14.75 Native and 13.97 Melange.
The dedicated recursive-rule performance test passed separately.

## Singleton slice boundary checkpoint (2026-08-05)

Profiling the remaining Melange `q3` regression identified repeated small EAVT
slices in the bound-entity query path. The retained PSS change recognizes the
case where the lower-bound key is within the requested range and the next key
in the same leaf is already above the upper bound. It then uses that next key
as the exclusive right path instead of repeating an upper-bound binary search.
All other ranges retain the existing `rseek-path` or `binary-search-r` control
flow. The optimization does not change cursor order, inclusivity, or storage
restoration.

Three stable, order-reversed adjacent A/B pairs compared commit `e34cc95`
against the modified worktree with the standard 20,000-person `q3` protocol.
The modified runner was faster by 4.1%, 7.3%, and 4.0%, for a median improvement
of 4.1%. Absolute measurements outside this window were discarded because
WindowServer, Chrome, and other UI processes consumed substantial CPU and made
the same binary vary by more than 2x. They are not evidence for the final
upstream gate.

Native and Melange singleton-slice and reversed-range tests pass. The complete
query suite (164 tests and 752 assertions per target), transaction suite,
pull suite, and index suite also pass with the optimized boundary search. PSS
still has zero inline type hints, and generated DataScript remains free of
dynamic boundaries.
