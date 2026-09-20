# clojure-test-suite integration

This directory contains LG's incremental runner for selected tests from
`jank-lang/clojure-test-suite`.

Pinned upstream clone used for the first batch:

```text
https://github.com/jank-lang/clojure-test-suite
6299706516f55d2ccb2d5abb5662489971584dea
```

The local audit clone lives at `vendor/clojure-test-suite` when present. The
Dune target uses fixed copies under `test/clojure_suite/upstream` so the smoke
test remains reproducible without committing a nested Git repository.

Run the current native + Melange smoke with:

```bash
dune build @test/clojure_suite/clojure-test-suite-smoke
```

Known candidate failures from the local upstream clone are tracked in
`failures.edn`. Promote one namespace at a time from `failures.edn` into the
smoke runner after adding the required LG support.

Run a full compile scan of the local upstream clone with:

```bash
python3 test/clojure_suite/scan_clojure_suite.py \
  --report test/clojure_suite/scan_report.json
```

Summarize the ignored raw report with:

```bash
python3 test/clojure_suite/summarize_clojure_suite.py \
  --upstream-commit 6299706516f55d2ccb2d5abb5662489971584dea \
  --compiled-both-output test/clojure_suite/compiled_both_namespaces.txt
```

The current failure classification is tracked in `failure_classes.md`.
`compiled_both_namespaces.txt` is the committed 185-namespace scan surface,
and `promotion_exclusions.tsv` records a concrete static-error or host-boundary
reason for every target that is not executed by the smoke manifest. Verify the
target-level closure with:

```bash
dune build \
  @test/clojure_suite/clojure-test-suite-promotion-audit
```

The aggregate smoke depends on this audit, so CI rejects any compiled Native or
Melange target that is neither promoted nor explicitly classified.
