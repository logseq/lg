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
