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
rtk dune build @test/clojure_suite/clojure-test-suite-smoke
```

Known candidate failures from the local upstream clone are tracked in
`failures.edn`. Promote one namespace at a time from `failures.edn` into the
smoke runner after adding the required LG support.
