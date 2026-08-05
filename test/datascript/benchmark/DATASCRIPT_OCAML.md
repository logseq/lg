# datascript-ocaml benchmark baseline

The Native comparison baseline is
<https://github.com/logseq/datascript-ocaml.git>, commit
`42160006c6fa7af9c9b50cc08e933d0a01715abc`
(`4216000`, 2026-08-04). Benchmark comparisons must use this exact commit.
The same revision is available to benchmark tooling in
`test/datascript/benchmark/datascript-ocaml.rev`.

To reproduce the comparison checkout and build its benchmark runner:

```sh
git clone https://github.com/logseq/datascript-ocaml.git
git -C datascript-ocaml checkout 42160006c6fa7af9c9b50cc08e933d0a01715abc
dune build --root datascript-ocaml -j 1 bench/bench_ocaml.exe
```

This pin is a performance comparison baseline only. The Logseq DataScript fork
pinned in `test/datascript/UPSTREAM.md` remains authoritative for behavior and
public API compatibility.

## Comparison protocol

The pinned runner contains 14 workloads. Exactly 10 operation-comparable
workloads have LG counterparts:
`add-1`, `add-5`, `add-all`, `q1`, `q2`, `q3`, `q4`, `q5-shortcircuit`,
`qpred1`, and `qpred2`. They use the same broad operation shape, but the random
generator, seed, schema details, and generated rows are not identical. Their
results therefore form a supplemental Native comparison rather than the
upstream compatibility performance gate.

The remaining workloads are reported without an LG ratio:

- `datoms-name` has no equivalent LG index-scan workload;
- `q2pred` has no equivalent LG query workload;
- `pull-one` uses a different database graph and pull pattern;
- `storage-roundtrip` has no equivalent LG benchmark workload.

The datascript-ocaml runner executes all 14 workloads in one process and does
not expose single-workload selection or a batch-size option. Each workload
does perform its own warmup, full major collection, and median sampling. The
final supplemental run uses the closest common timing and population settings:

```sh
_build/default/bench/bench_ocaml.exe \
  --size 20000 --warmup-ms 2000 --sample-ms 1000 --samples 5
```

LG and upstream DataScript remain isolated one workload per process with their
documented batch size. The final report must keep the datascript-ocaml table
separate and describe these protocol and data-set differences.

The complete 2026-08-05 run is checked in as
`datascript-ocaml-20260805.tsv`. Validate it with:

```sh
sh script/check_datascript_ocaml_benchmark_result.sh \
  test/datascript/benchmark/datascript-ocaml-20260805.tsv
```
