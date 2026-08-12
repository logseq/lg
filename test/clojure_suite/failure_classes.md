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
| compiled | 43 |
| compile failed | 433 |
| namespaces scanned | 238 |
| namespaces compiled on both native and Melange | 19 |
| namespaces failed on both native and Melange | 214 |
| native-only compiled namespaces | 1 |
| Melange-only compiled namespaces | 4 |

Target split:

| target | compiled | compile failed |
| --- | ---: | ---: |
| native | 20 | 218 |
| Melange | 23 | 215 |

Namespaces currently compiling on both targets:

- `clojure.core-test.aclone`
- `clojure.core-test.and`
- `clojure.core-test.any-qmark`
- `clojure.core-test.associative-qmark`
- `clojure.core-test.comment`
- `clojure.core-test.keyword`
- `clojure.core-test.make-hierarchy`
- `clojure.core-test.name`
- `clojure.core-test.nan-qmark`
- `clojure.core-test.or`
- `clojure.core-test.pr-str`
- `clojure.core-test.print-str`
- `clojure.core-test.println-str`
- `clojure.core-test.prn-str`
- `clojure.core-test.rand-int`
- `clojure.core-test.sequential-qmark`
- `clojure.core-test.symbol`
- `clojure.core-test.when`
- `clojure.core-test.when-not`

## Failure classes

| class | failures | handling |
| --- | ---: | --- |
| `static-typing-or-closed-domain-boundary` | 192 | Split into intentional LG static errors, typed capability gaps, and places where a narrow runtime boundary is justified. Do not weaken all calls to dynamic. |
| `missing-suite-support-namespace-or-helper` | 85 | Port test support namespaces/helpers first, especially `clojure.core-test.number-range`, `clojure.core-test.eq`, `clojure.core-test.every-qmark`, and missing `portability` helpers. |
| `reader-or-numeric-literal` | 79 | Decide numeric tower and reader literal scope before implementation. Bigint (`N`), bigdecimal (`M`), ratios, UUID tags, and non-ASCII char literals are visible blockers. |
| `suite-require-form-not-accepted` | 28 | Extend scan ingestion or namespace parsing for the suite's require form shape before treating these as stdlib failures. |
| `missing-core-api-macro-or-var` | 24 | Audit each missing public var/macro/special behavior. Examples include `bound-fn`, `bound-fn*`, `format`, `eval`, `intern`, `numerator`, `denominator`, `promise`, `definterface`, `def`, and defmulti dispatch coverage. |
| `host-boundary-or-platform-specific` | 15 | Keep JVM/JS class identity, `cljs.js`, `js/*`, Java interop, and native output module gaps as host-boundary unless LG has a deliberate static representation. |
| `reader-conditional-support` | 5 | Fix reader conditional edge cases separately from core var behavior. |
| `unsupported-form-or-arity` | 3 | Known examples: `fnil` default positions and `re-find` arity. These are small, targeted compatibility tasks. |
| `unsupported-namespace-form` | 2 | The suite uses `:import`; LG namespaces currently reject it. Treat as namespace parser/support-surface work, not stdlib source migration. |

## Platform skew

- `clojure.core-test.format`: Melange compiles; native fails on missing `format`.
- `clojure.core-test.nil-qmark`: native compiles; Melange fails on `js/undefined`.
- `clojure.core-test.num`: Melange compiles; native fails on `definterface`.
- `clojure.core-test.remove-watch`: Melange compiles; native fails on `def`.
- `clojure.core-test.with-out-str`: Melange compiles; native fails on `Unbound module System`.

## Current interpretation

The 433 compile failures are not 433 independent core defects. The highest
leverage blockers are:

1. Suite ingestion and support namespaces: this blocks numeric, equality, lazy,
   watch, and predicate test families before the actual public var behavior is
   reached.
2. Static typing versus upstream negative runtime tests: many upstream tests
   intentionally call functions with bad argument types and expect `thrown?`.
   In LG, many of these should remain compile-time errors and need a separate
   static-error lane.
3. Reader/numeric coverage: bigint, bigdecimal, ratio, UUID, and char literal
   support must be scoped explicitly because they affect reader, type system,
   equality, ordering, arithmetic, printing, and EDN.
4. Missing API/macro forms: these should be triaged public-var by public-var.
   Some are source-portable functions; others are compiler/host behavior.
5. Host boundaries: JVM class identity and JS globals are not portable stdlib
   source and should remain classified unless an LG-native representation is
   designed.

## Suggested repair order

1. Fix suite ingestion/support first:
   - load required `clojure.core-test.*` support namespaces;
   - extend `lg_portability.cljc` only for test harness helpers;
   - handle accepted require form variants used by the suite.
2. Re-run the compile scan and promote newly compiling namespaces to Dune smoke.
3. Add a static-error test lane for upstream `thrown?` cases that LG rejects at
   compile time by design.
4. Repair small source-portable API gaps that do not require broad type-system
   changes.
5. Address regex, watches/ex-data, hierarchy/multimethod dynamic boundaries with
   narrow documented runtime types where static closed domains are insufficient.
6. Decide numeric tower scope before implementing bigint/ratio/bigdecimal
   behavior.
