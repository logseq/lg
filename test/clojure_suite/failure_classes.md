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
  --upstream-commit 6299706516f55d2ccb2d5abb5662489971584dea \
  --static-error-report test/clojure_suite/static_error_lane.json \
  --repair-lane-report test/clojure_suite/repair_lanes.json
```

## Compile coverage

| metric | count |
| --- | ---: |
| compile attempts | 476 |
| compiled | 381 |
| compile failed | 95 |
| namespaces scanned | 238 |
| namespaces compiled on both native and Melange | 185 |
| namespaces failed on both native and Melange | 42 |
| native-only compiled namespaces | 8 |
| Melange-only compiled namespaces | 3 |

Target split:

| target | compiled | compile failed |
| --- | ---: | ---: |
| native | 193 | 45 |
| Melange | 188 | 50 |

The summarizer emits the authoritative current list of all 173 namespaces.
The list below records the earlier 46-namespace milestone and is retained only
as migration history:

- `clojure.core-test.aclone`
- `clojure.core-test.and`
- `clojure.core-test.any-qmark`
- `clojure.core-test.associative-qmark`
- `clojure.core-test.bound-fn`
- `clojure.core-test.bound-fn-star`
- `clojure.core-test.comment`
- `clojure.core-test.denominator`
- `clojure.core-test.every-qmark`
- `clojure.core-test.fn-qmark`
- `clojure.core-test.format`
- `clojure.core-test.group-by`
- `clojure.core-test.hash-set`
- `clojure.core-test.ifn-qmark`
- `clojure.core-test.intern`
- `clojure.core-test.keyword`
- `clojure.core-test.make-hierarchy`
- `clojure.core-test.name`
- `clojure.core-test.nan-qmark`
- `clojure.core-test.nil-qmark`
- `clojure.core-test.not`
- `clojure.core-test.not-every-qmark`
- `clojure.core-test.number-range`
- `clojure.core-test.numerator`
- `clojure.core-test.or`
- `clojure.core-test.partial`
- `clojure.core-test.persistent-bang`
- `clojure.core-test.plus-squote`
- `clojure.core-test.pop-bang`
- `clojure.core-test.pr-str`
- `clojure.core-test.print-str`
- `clojure.core-test.println-str`
- `clojure.core-test.prn-str`
- `clojure.core-test.rand-int`
- `clojure.core-test.random-sample`
- `clojure.core-test.rationalize`
- `clojure.core-test.sequential-qmark`
- `clojure.core-test.some`
- `clojure.core-test.some-qmark`
- `clojure.core-test.star-squote`
- `clojure.core-test.symbol`
- `clojure.core-test.uuid-qmark`
- `clojure.core-test.var-qmark`
- `clojure.core-test.when`
- `clojure.core-test.when-not`
- `clojure.core-test.with-out-str`

## Failure classes

| class | failures | handling |
| --- | ---: | --- |
| `static-typing-or-closed-domain-boundary` | 82 | The remaining entries are intentional LG static errors. Do not weaken ordinary values or collections to dynamic. |
| `reader-or-numeric-literal` | 4 | Remaining blockers are tagged `#inst` literals and real `with-precision` BigDecimal semantics. Arbitrary precision remains a separate numeric-tower design. |
| `host-boundary-or-platform-specific` | 9 | Keep JVM/JS class identity, Java interop, target globals, Var mutation, and true asynchronous `future` behavior gated unless LG introduces deliberate portable static representations. |
| `missing-suite-support-namespace-or-helper` | 0 | The current scan has no remaining failures in this class. Suite helpers remain compatibility scaffolding and should not be counted as stdlib API support. |
| `missing-core-api-macro-or-var` | 0 | The current scan has no remaining failures in this class. New entries should be inspected before adding compiler-owned public-name dispatch. |
| `unsupported-form-or-arity` | 0 | The `every_qmark.cljc` `letfn` blocker has been cleared. The previous `are`/`#uuid` false arity blocker in `parse_uuid.cljc` has also been cleared. |
| `unsupported-namespace-form` | 0 | The current scan has no remaining failures in this class. JVM `:import` is classified as a host/platform boundary for native/Melange rather than stdlib source migration work. |
| `other-compiler-error` | 0 | The current scan has no unclassified compiler errors; new entries in this class should be inspected before changing compiler behavior. |

Static typing subclasses:

| subclass | failures | handling |
| --- | ---: | --- |
| `negative-runtime-test-is-static-error` | 67 | Upstream intentionally calls functions with wrong runtime argument types and expects thrown exceptions. LG keeps these as compile-time errors. |
| `suite-polymorphic-fixture-is-static-error` | 15 | Whole-suite fixtures reuse incompatible concrete domains, including numeric predicate `are` fixtures, the `find.cljc` mixed int/keyword/string key map, Native `nth.cljc` sharing one helper across collections and regex matchers, and `eq`/`not-eq` sharing one helper across function, sequential, and map domains. Supported monomorphic/direct calls have focused coverage. |
| `heterogeneous-collection-needs-closed-domain` | 0 | No remaining compile failure is assigned to this implementation lane; known heterogeneous suite fixtures are audited as intentional static errors. |
| `first-class-polymorphic-or-hof` | 0 | No remaining compile failure is assigned to the implementation lane. The `juxt.cljc` whole-file fixture intentionally combines functions with incompatible static argument domains and is audited separately; supported monomorphic `juxt` arities remain covered on Native and Melange. |
| `transient-collection-boundary` | 0 | No remaining compile failure is assigned to this subclass. The `transient.cljc` whole-file `are` fixture reuses one inferred local function across vector, map, and set domains and is audited as a static error; focused typed transient operations remain covered separately. |
| `dynamic-boundary-needs-closed-domain` | 2 | Failures such as watch events cross a narrow dynamic boundary today. Model common Logseq-facing domains explicitly or document the smallest allowed dynamic boundary before expanding support. |
| `form-or-declaration-static-gap` | 0 | Current repair lanes have no remaining failures in this subclass. New entries should be inspected before adding compiler-owned public-name dispatch. |
| `typed-protocol-or-capability-gap` | 0 | The structural-record `find` and closed tuple/list/nested-vector set comparator gaps are fixed. New entries must receive focused TDD coverage before entering this lane. |

Repair lanes are also emitted to
`test/clojure_suite/repair_lanes.json`, one normalized entry per failing
namespace/target:

| lane | failures | interpretation |
| --- | ---: | --- |
| `design-reader-and-numeric-tower` | 4 | Tagged instant literals and BigDecimal precision/rounding require deliberate source and runtime types. |
| `audit-as-static-error` | 82 | Upstream negative runtime tests and whole-suite heterogeneous/polymorphic fixtures that LG intentionally rejects at compile time. |
| `design-closed-domain-or-narrow-runtime-boundary` | 0 | The current scan has no remaining positive closed-domain implementation failures. |
| `implement-static-language-capability` | 0 | The current scan has no remaining positive implementation failures. Closed tuple/list/nested-vector element types receive deterministic generated `Set.Make` modules without dynamic storage. |
| `document-or-gate-host-boundary` | 9 | JVM/JS identity, Java interop, target-only globals, and futures remain documented/gated. |
| `implement-form-or-reader-support` | 0 | Current repair lanes have no remaining failures in this lane. New compiler/analyzer form gaps must be implemented as static forms rather than source-portable function dispatch. |

## Platform skew

- `clojure.core-test.eval`: Native compiles by skipping unsupported `eval`;
  Melange still fails on `cljs.js`.
- `clojure.core-test.num`: Melange compiles; native fails on `definterface`.
- `clojure.core-test.remove-watch`: Melange compiles; native fails on `def`.

## Current interpretation

The 95 compile failures are not 95 independent core defects. The current
highest leverage blockers are:

1. Static typing versus upstream negative runtime tests: many upstream tests
   intentionally call functions with bad argument types and expect `thrown?`.
   In LG, many of these should remain compile-time errors and need a separate
   static-error lane.
2. Numeric boundaries now exposed past `number_range`: bigint, bigdecimal,
   ratio, and char literal support must be scoped explicitly because they
   affect reader, type system, equality, ordering, arithmetic, printing, and
   EDN. UUID string reader literals are supported, but Java/CLJS UUID class and
   record identity remain separate host/representation boundaries.
3. Host boundaries: JVM class identity, JVM `:import`, and JS globals are not portable stdlib
   source and should remain classified unless an LG-native representation is
   designed.

`clojure.core-test.find` now passes its structural-record lookup cases on both
targets, including missing keyword fields and single evaluation. Its remaining
whole-file failure is `{0 1 :0 2 "0" 3}`, whose key domain is intentionally
heterogeneous; LG keeps that fixture in the static-error audit rather than
erasing the keys into dynamic storage.

`clojure.core-test.list`, `clojure.core-test.vector`,
`clojure.core-test.pop`, and `clojure.core-test.conj` now compile on Native and
Melange. Heterogeneous values
that are recursively representable as Clojure data use the closed
`Lg_edn_backend.t` sum, including structural maps and provably empty lists,
vectors, sets, and maps. Functions and host values remain static errors unless
the program declares an explicit closed sum.

`clojure.core-test.identical-qmark` also compiles on both targets. Physical
identity remains statically typed; an empty map's fully open key/value
parameters may instantiate against another map at the comparison site without
introducing EDN or dynamic conversion.

The heterogeneous three-argument `nth` behavior now returns a closed EDN value
for EDN-compatible element/default alternatives, including provably empty
collections. The namespace compiles on Melange. Native reaches the suite's
`re-matcher` branch, where one `are` helper combines matchers with unrelated
collection types; direct matcher and collection calls are covered separately,
so that remaining whole-fixture failure is audited as a static error.

`clojure.core-test.reverse` and `clojure.core-test.rseq` now compile on both
targets. Melange treats its ClojureScript char representation as a singleton
seqable value for `reverse`, while the source-owned `persistent-tree-map`
implements `IReversible/-rseq` through its existing descending tree traversal.
The targeted scanner result was verified after rebuilding both aggregate
stdlib state artifacts.

`clojure.core-test.hash-map` now compiles on both targets. When heterogeneous
keys and values are recursively Clojure data, `hash-map` constructs a concrete
`Runtime_map<Lg_edn_backend.t,Lg_edn_backend.t>` and collection operations pack
their statically typed keys at the call boundary. No `Runtime_dynamic` storage
or conversion is involved.

`clojure.core-test.interleave` now compiles on both targets. Inputs with
different EDN-compatible element types are each mapped into the closed EDN sum
before the existing lazy interleave algorithm runs; non-data elements remain a
static error.

`clojure.core-test.seq` now compiles on both targets. `into` instantiates an
open runtime-map target from statically typed source entries, and source-defined
nominal collections such as `persistent-tree-map` use their `ICollection/-conj`
implementation through a typed fold. Built-in collection mismatches retain
their existing compile-time errors; no dynamic storage or collection type-name
dispatch was added.

`clojure.core-test.eq` and `clojure.core-test.not-eq` now advance past sorted
collection construction. `sorted-map-by` and `sorted-set-by` preserve the
pinned ClojureScript `fn->comparator` call order through a private static
return-type-directed primitive: integer comparators remain unchanged and
boolean predicates use the upstream forward-then-reverse three-way result.
Sorted map and set equality is based on their typed collection contents rather
than comparator or tree representation, including equality with hash maps and
hash sets. The remaining whole-namespace failure is the suite's single `eq`
parameter being reused across incompatible function, sequential, and map
domains; it is recorded in the intentional static-error lane.

`clojure.core-test.when-let` now compiles on Native and Melange. Truthy
bindings whose static types exclude both nil and false keep the body result
type directly instead of manufacturing an impossible nullable branch. Named
recursive functions also reuse the typed recursive-function preparation path;
homogeneous `lazy-seq`/`cons` self recursion infers its sequence element and
return types without `any` or dynamic storage.

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
- `clojure.core/get-in` now compiles the upstream namespace on Native and
  Melange. Literal `nil`, quoted and constructed empty paths return the target
  without entering `Runtime_dynamic`; static vector paths preserve left-to-right
  argument evaluation, short-circuit non-associative intermediate values, and
  traverse recursively closed EDN map/vector data through a typed closed-sum
  lookup boundary.
- `clojure.core/ex-info` data literal maps now accept local statically typed
  scalar values through the documented exception-only `exception-data<T>`
  capability. First-class `every?` now adapts to helper-accumulated overloads,
  including upstream empty-collection behavior for non-callable predicates and
  nil collections. `(complement not-every?)` normalizes to the same static path,
  so `every_qmark.cljc` and `not_every_qmark.cljc` compile on both Native and
  Melange.
- Direct `clojure.core/atom` calls now accept static option pairs for `nil nil`,
  `:meta`, `:validator`, and combined metadata/validator options in either
  order. `cljs.core/IAtom` protocol aliases now resolve to the core static
  protocol and ref atoms implement the IAtom marker through the typed
  `compare-and-set!` primitive. The upstream `atom.cljc` namespace still does
  not promote because both Native and Melange are blocked by naked `(atom nil)`
  forms that need an explicit `ref<option<T>>` payload type. This should be
  fixed with typed option refs, not by widening refs to dynamic.
- `clojure.core/re-matcher` and Native `(re-find matcher)` now exist as a
  narrow typed regex matcher boundary. Matcher `nth` can read the current
  capture group and matcher `nth` with a heterogeneous default uses a regex-only
  dynamic result. The upstream `nth.cljc` namespace now fails later on the
  general `nth default must match collection element type` static rule, not on
  `re-find` arity.
- `#uuid` tagged reader literals now parse as one literal form and elaborate to
  `Lg_runtime.Runtime_uuid.t`. This clears the previous `are`/reader shape
  blocker and promotes `clojure.core-test.uuid-qmark` on both Native and
  Melange. The upstream `parse_uuid.cljc` namespace now fails later: Native
  uses `java.util.UUID`, while Melange references the `cljs.core.UUID` record
  type.
- `clojure.core/some` now supports first-class numeric predicates over
  `double-array`/`float-array` element types and optional map-predicate lookups
  such as `(some {2 "two"} [nil 3 2])` without dynamic packing. The upstream
  `some.cljc` namespace compiles on both Native and Melange.
- `clojure.core/fnil` now supports variadic function arities whose default
  positions fall in the wrapped function's rest parameter. The upstream
  `fnil.cljc` namespace now fails later because the test stores int defaults and
  the symbol `'not-nil` in the same result vector; that is a heterogeneous
  collection typing issue, not an unsupported `fnil` arity.
- Runtime source-level syntax-quoted constants now compile as quoted data,
  clearing the false `syntax-quote` missing-core blocker in
  `constantly.cljc`. That namespace now fails later on real reader/numeric
  boundaries: Native reaches the ratio literal `111/7`, while Melange reaches
  the named character literal `\return`.
- `clojure.test/async` is available to the suite runner as a compile-time
  compatibility macro that binds the completion callback and expands the body.
  It is not core.async support and does not implement a general async scheduler.
  This clears the previous `taps.cljc` missing refer blocker; `taps.cljc` now
  exposes real target blockers instead: Native Java interop
  `clojure.lang.IPending`, and Melange's typed `swap!` receiver requirement.
- `clojure.core-test.portability/when-var-exists` now has LG-specific
  compile-time gating for explicitly unsupported vars. This prevents suites for
  unsupported APIs from failing merely because their skipped body contains
  missing forms. The newly compiling namespaces `bound-fn`, `bound-fn-star`,
  `denominator`, `intern`, `numerator`, and `rationalize` therefore indicate
  correct suite gating, not implementation of those public APIs.
- `clojure.core/=` and `clojure.core/not=` now support direct comparisons of
  disjoint static source types without dynamic packing. The upstream
  `eq.cljc` helper still fails because it passes equality as a first-class
  parameter and reuses that parameter at unrelated argument types. Supporting
  that shape requires a typed equality capability or overload representation,
  not a universal dynamic function.
- `clojure.core/format` is present only on Native and currently implements the
  unary string pass-through case covered by the upstream default suite. LG does
  not implement Java `Formatter`; adding formatted arguments needs a separate
  scoped design.
- `defmulti` now supports the source dispatch form `first` in addition to the
  existing keyword, `identity`, and inline `fn` dispatch forms. The
  `ifn_qmark.cljc` suite body that defines `my-multi` inside a `deftest` is now
  skipped by the suite portability gate because it requires Clojure
  expression-position top-level Var definition semantics. This is suite gating,
  not support for local runtime Var interning.
- Native `num.cljc` `definterface` and Native `remove_watch.cljc`
  expression-position `def` are classified as host-boundary rather than
  source-portable missing core APIs. `definterface` is JVM interface syntax,
  and the `remove-watch` branch exercises Clojure Var-object watch semantics.
- Empty-field `deftype` now compiles as a static nominal record with a hidden
  identity field, so repeated zero-argument constructors preserve distinct
  instance identity without changing source constructor arity. Source-defined
  `deftype`/`defrecord` names now also compile as portable EDN symbol hierarchy
  tags such as `clojure.core-test.parents/TestParentsRecord`, without modeling
  JVM or JS class objects. `parents.cljc` and `descendants.cljc` now fail later
  on mixed EDN named-value vectors such as `[TestParentsRecord ::record]`,
  which need a closed EDN literal/domain promotion rather than a type-name
  symbol fix.
- `clojure.core/var?` now compiles as a source inline predicate for direct
  `#'x` and `(var x)` syntax and is declared in `core.lgi`, so it works through
  explicit refer and automatic core refer. This promotes `var_qmark.cljc` on
  both Native and Melange. LG still does not model a full first-class JVM Var
  object: current `#'x` compilation resolves to the referenced static value for
  call/deref compatibility, so bound Var-object identity remains outside this
  narrow predicate surface.
- `clojure.core-test.portability/big-int?` is available as a suite helper and
  `+'`/`*'` bodies are gated as unsupported numeric-tower tests. This removes
  the remaining suite-helper failure class and promotes `plus_squote.cljc` and
  `star_squote.cljc` on both Native and Melange. Ordinary `plus.cljc` and
  `star.cljc` now fail later on bigint reader literals such as `1N`.

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
