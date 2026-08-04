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
