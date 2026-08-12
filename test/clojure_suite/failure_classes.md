# clojure-test-suite failure classes

This classification is from the completed compile scan over the local upstream
clone:

- upstream: `https://github.com/jank-lang/clojure-test-suite`
- commit: `6299706516f55d2ccb2d5abb5662489971584dea`
- raw local report: `test/clojure_suite/scan_report.json`

The raw report is ignored because it contains local paths. Regenerate the
auditable summary with:

```bash
python3 test/clojure_suite/summarize_clojure_suite.py \
  --upstream-commit 6299706516f55d2ccb2d5abb5662489971584dea
```

## Compile coverage

| metric | count |
| --- | ---: |
| compile attempts | 476 |
| compiled | 51 |
| compile failed | 425 |
| namespaces scanned | 238 |
| namespaces compiled on both native and Melange | 24 |
| namespaces failed on both native and Melange | 211 |
| native-only compiled namespaces | 0 |
| Melange-only compiled namespaces | 3 |

Target split:

| target | compiled | compile failed |
| --- | ---: | ---: |
| native | 24 | 214 |
| Melange | 27 | 211 |

Namespaces currently compiling on both targets:

- `clojure.core-test.aclone`
- `clojure.core-test.and`
- `clojure.core-test.any-qmark`
- `clojure.core-test.associative-qmark`
- `clojure.core-test.comment`
- `clojure.core-test.fn-qmark`
- `clojure.core-test.keyword`
- `clojure.core-test.make-hierarchy`
- `clojure.core-test.name`
- `clojure.core-test.nan-qmark`
- `clojure.core-test.nil-qmark`
- `clojure.core-test.not`
- `clojure.core-test.or`
- `clojure.core-test.pr-str`
- `clojure.core-test.print-str`
- `clojure.core-test.println-str`
- `clojure.core-test.prn-str`
- `clojure.core-test.rand-int`
- `clojure.core-test.sequential-qmark`
- `clojure.core-test.some-qmark`
- `clojure.core-test.symbol`
- `clojure.core-test.when`
- `clojure.core-test.when-not`
- `clojure.core-test.with-out-str`

## Failure classes

| class | failures | handling |
| --- | ---: | --- |
| `static-typing-or-closed-domain-boundary` | 232 | Split into intentional LG static errors, typed capability gaps, and places where a narrow runtime boundary is justified. Do not weaken all calls to dynamic. The count increased after namespace/parser harness blockers were cleared because those tests now reach real LG static boundaries. |
| `host-boundary-or-platform-specific` | 81 | Keep JVM/JS class identity, `cljs.js`, Java interop, and native output module gaps as host-boundary unless LG has a deliberate static representation. Direct `js/undefined` is now a narrow Melange host constant that lowers to static nil; Native `System/getProperty` is limited to the `"line.separator"` literal; Native `(Object.)` is only a truthy suite sentinel and does not implement JVM object identity. Other JS/JVM globals remain host-boundary. The large count comes from `number_range.cljc` reaching `Long/MAX_VALUE` / `js/Number.MAX_SAFE_INTEGER`. |
| `reader-or-numeric-literal` | 79 | Decide numeric tower and reader literal scope before implementation. Bigint (`N`), bigdecimal (`M`), ratios, UUID tags, and non-ASCII char literals are visible blockers. |
| `missing-core-api-macro-or-var` | 24 | Audit each missing public var/macro/special behavior. Examples include `bound-fn`, `bound-fn*`, `format`, `eval`, `intern`, `numerator`, `denominator`, `promise`, `definterface`, `def`, and defmulti dispatch coverage. |
| `unsupported-form-or-arity` | 7 | Known examples: `atom` option arity, `fnil` default positions, native `re-find` arity, and test macro `are` argument shape. These are targeted compatibility tasks. |
| `unsupported-namespace-form` | 2 | The suite uses `:import`; LG namespaces currently reject it. Treat as namespace parser/support-surface work, not stdlib source migration. |

## Platform skew

- `clojure.core-test.format`: Melange compiles; native fails on missing `format`.
- `clojure.core-test.num`: Melange compiles; native fails on `definterface`.
- `clojure.core-test.remove-watch`: Melange compiles; native fails on `def`.

## Current interpretation

The 425 compile failures are not 425 independent core defects. The current
highest leverage blockers are:

1. Static typing versus upstream negative runtime tests: many upstream tests
   intentionally call functions with bad argument types and expect `thrown?`.
   In LG, many of these should remain compile-time errors and need a separate
   static-error lane.
2. Host and numeric boundaries exposed by `number_range`: `Long/MAX_VALUE`,
   `js/Number.MAX_SAFE_INTEGER`, bigint, bigdecimal, ratio, UUID, and char
   literal support must be scoped explicitly because they affect reader, type
   system, equality, ordering, arithmetic, printing, and EDN.
3. Missing API/macro forms: these should be triaged public-var by public-var.
   Some are source-portable functions; others are compiler/host behavior.
4. Host boundaries: JVM class identity and JS globals are not portable stdlib
   source and should remain classified unless an LG-native representation is
   designed.

## Promotion/typecheck failures

The compile scan only verifies that LG can generate target OCaml/Melange source.
Promotion into `@test/clojure_suite/clojure-test-suite-smoke` adds OCaml
typechecking and runtime execution. A namespace can therefore be `compiled-both`
in `scan_report.json` but still be blocked from smoke promotion.

- `clojure.core-test.aclone`: LG generation succeeds for Native and Melange,
  but smoke promotion typechecks the generated OCaml and fails on the upstream
  helper that calls `(clone-test (int-array 3) ...)` and then
  `(clone-test (object-array 3) ...)`. The helper's `aset` writes infer the
  parameter as `int array`; size-created `object-array` remains
  `option array` to preserve nil slots. Fixing this without dynamic requires a
  static array read/write capability or call-site specialization.

## Common API blockers that are not missing APIs

- `clojure.core/add-watch` and `clojure.core/remove-watch` are already
  source-defined and covered by focused Native/Melange tests for typed refs and
  `IWatchable`. The upstream `add-watch` namespace currently fails earlier in
  the test body because it accumulates heterogeneous watcher event maps and
  `ex-data` payloads in one collection. That should be handled as a closed
  watch-event/ex-data domain or a documented narrow ex-data dynamic boundary,
  not by weakening ordinary record storage to dynamic.

## Suggested repair order

1. Re-run the compile scan and promote newly compiling namespaces to Dune smoke.
2. Add a static-error test lane for upstream `thrown?` cases that LG rejects at
   compile time by design.
3. Repair small source-portable API gaps that do not require broad type-system
   changes.
4. Address regex, watches/ex-data, hierarchy/multimethod dynamic boundaries with
   narrow documented runtime types where static closed domains are insufficient.
5. Decide numeric tower scope before implementing bigint/ratio/bigdecimal
   behavior.
