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
| compiled | 375 |
| compile failed | 101 |
| namespaces scanned | 238 |
| namespaces compiled on both native and Melange | 183 |
| namespaces failed on both native and Melange | 46 |
| native-only compiled namespaces | 5 |
| Melange-only compiled namespaces | 4 |

Target split:

| target | compiled | compile failed |
| --- | ---: | ---: |
| native | 188 | 50 |
| Melange | 187 | 51 |

The summarizer emits the authoritative current list of all 183 namespaces.
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
| `static-typing-or-closed-domain-boundary` | 92 | The remaining entries are intentional LG static errors. Do not weaken ordinary values or collections to dynamic. |
| `reader-or-numeric-literal` | 0 | `#inst`, decimal literals, arbitrary-precision decimal arithmetic, ordering, and `with-precision` are implemented for Native and Melange. |
| `host-boundary-or-platform-specific` | 9 | Keep JVM/JS class identity, Java interop, target globals, Var mutation, and true asynchronous `future` behavior gated unless LG introduces deliberate portable static representations. |
| `missing-suite-support-namespace-or-helper` | 0 | The current scan has no remaining failures in this class. Suite helpers remain compatibility scaffolding and should not be counted as stdlib API support. |
| `missing-core-api-macro-or-var` | 0 | The current scan has no remaining failures in this class. New entries should be inspected before adding compiler-owned public-name dispatch. |
| `unsupported-form-or-arity` | 0 | The `every_qmark.cljc` `letfn` blocker has been cleared. The previous `are`/`#uuid` false arity blocker in `parse_uuid.cljc` has also been cleared. |
| `unsupported-namespace-form` | 0 | The current scan has no remaining failures in this class. JVM `:import` is classified as a host/platform boundary for native/Melange rather than stdlib source migration work. |
| `other-compiler-error` | 0 | The current scan has no unclassified compiler errors; new entries in this class should be inspected before changing compiler behavior. |

Static typing subclasses:

| subclass | failures | handling |
| --- | ---: | --- |
| `negative-runtime-test-is-static-error` | 59 | Upstream intentionally calls functions with wrong runtime argument types and expects thrown exceptions. LG keeps these as compile-time errors. |
| `suite-polymorphic-fixture-is-static-error` | 20 | Whole-suite fixtures reuse incompatible concrete domains, including numeric predicate `are` fixtures, collection fixtures such as `disj.cljc`, `sort.cljc`, and `zipmap.cljc`, `seq.cljc` mixing decimal and int set elements, Native `nth.cljc` sharing one helper across collections and regex matchers, and `eq`/`not-eq` sharing one helper across function, sequential, and map domains. Supported monomorphic/direct calls have focused coverage. |
| `heterogeneous-collection-needs-closed-domain` | 7 | Heterogeneous list/vector and nullable element fixtures remain audited static errors; they require explicit closed sums rather than dynamic storage. |
| `first-class-polymorphic-or-hof` | 2 | The remaining entries are whole-file fixtures that combine incompatible static higher-order domains. Supported monomorphic and call-site-specialized higher-order behavior remains covered on Native and Melange. |
| `transient-collection-boundary` | 0 | No remaining compile failure is assigned to this subclass. The `transient.cljc` whole-file `are` fixture reuses one inferred local function across vector, map, and set domains and is audited as a static error; focused typed transient operations remain covered separately. |
| `dynamic-boundary-needs-closed-domain` | 0 | No remaining suite compile failure requires expanding a dynamic boundary. |
| `form-or-declaration-static-gap` | 4 | These whole-suite declaration/pattern fixtures are retained as audited static errors under the user-selected policy; direct supported forms retain focused coverage. |
| `typed-protocol-or-capability-gap` | 0 | The structural-record `find` and closed tuple/list/nested-vector set comparator gaps are fixed. New entries must receive focused TDD coverage before entering this lane. |

The summary groups failing namespace/target entries into these repair lanes:

| lane | failures | interpretation |
| --- | ---: | --- |
| `design-reader-and-numeric-tower` | 0 | Tagged instant and decimal reader/numeric blockers are cleared. |
| `audit-as-static-error` | 92 | All failures caused by the deliberate LG static type contract are audited here and are not repair work. |
| `design-closed-domain-or-narrow-runtime-boundary` | 0 | The current scan has no remaining positive closed-domain implementation failures. |
| `implement-static-language-capability` | 0 | The current scan has no remaining positive implementation failures. Closed tuple/list/nested-vector element types receive deterministic generated `Set.Make` modules without dynamic storage. |
| `document-or-gate-host-boundary` | 9 | JVM/JS identity, Java interop, target-only globals, and futures remain documented/gated. |
| `implement-form-or-reader-support` | 0 | Current repair lanes have no remaining failures in this lane. New compiler/analyzer form gaps must be implemented as static forms rather than source-portable function dispatch. |

## Platform skew

Nine namespaces compile on only one target: `double`, `long`, `not-empty`,
`nth`, `nthrest`, `num`, `parse-uuid`, `remove-watch`, and `subs`. Their failed
target entries are classified as either an audited static error or a documented
host/platform boundary in the generated reports.

## Current interpretation

The 101 compile failures are not unresolved core defects under the selected LG
policy. They consist only of 92 audited static errors and 9 documented
host/platform boundaries:

1. Static typing versus upstream negative runtime tests: many upstream tests
   intentionally call functions with bad argument types and expect `thrown?`.
   In LG, many of these should remain compile-time errors and need a separate
   static-error lane.
2. Tagged instants and arbitrary-precision decimal literals now have closed
   static representations. Decimal equality, ordering, arithmetic, printing,
   and `with-precision` rounding compile on Native and Melange without dynamic
   storage. Ratios and JVM numeric class identity remain outside this batch.
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

The promotion runner is manifest-driven by
`test/clojure_suite/promoted_namespaces.txt`; adding a runtime-ready namespace
does not require editing Dune or a generated runner. A line contains either a
namespace, or a namespace followed by the explicit `native` or `melange`
target qualifier. The current manifest contains 155 namespaces (155/183,
84.7%). Native runs the 148 applicable namespaces, while Melange runs 154
namespaces with 2,834 assertions. Both targets pass with zero failures and zero
errors. The six
Melange-only namespaces (`double-qmark`, `float-qmark`, `int-qmark`,
`integer-qmark`, `neg-int-qmark`, and `pos-int-qmark`) assert
ClojureScript/JVM numeric identity distinctions that Native's current float,
int, and bigint source domains do not expose.

The promoted predicate/collection batch also verifies option-valued map-entry
keys, map entries as vectors, sequence protocol predicates, decimal versus
float identity, Melange safe-integer predicates, and target-correct 32-bit hash
mixing. Source-returned generic sets are converted at their call boundary to
the concrete static set module, and map equality accepts a typed value-equality
callback for map-of-set results; neither path stores dynamic values.

The `apply`, `assoc`, `concat`, and `cons` runtime batch is promoted on both
targets. A statically literal empty spread now invokes the function's actual
zero/fixed arity without generating impossible element branches. Applying over
the result of `conj` on a literal vector preserves the exact appended argument
forms, so alternating key/value arguments retain their independent static
types. This covers ClojureScript's odd-`assoc` nil behavior without making the
spread collection or public API dynamic.

The `constantly`, `second`, `last`, and `empty` namespaces are also promoted on
both targets. `constantly` uses a private typed constant-function capability so
ignored argument types instantiate independently at every direct, `apply`, or
higher-order call; its captured result remains statically typed and evaluated
once. The `rest`, `next`, `nnext`, and `fnext` namespaces now promote on both
targets. Unresolved vector context no longer erases concrete EDN storage, and
sequential equality converts closed tuple map entries and nested vectors to a
shared EDN comparison representation. Empty `next` sequences retain compact
`Seq.t` storage while observing Clojure nil semantics for equality, `nil?`, and
EDN conversion; `rest` remains a non-nil empty sequence.

The next collection/sequence promotion adds `butlast`, `conj`, `dissoc`,
`distinct`, `drop`, `find`, `partial`, `peek`, `rand-nth`, `reverse`, `rseq`,
`seq?`, and `take-last` on both targets. `run!` remains outside the manifest
because an open callback's `Reduced<T>` capability is not yet propagated into
the source function parameter. `pop` remains outside because eagerly packing a
discarded infinite sequence inside a heterogeneous vector does not terminate.
`group-by` remains outside because metadata-bearing vector keys and grouped
item metadata order still disagree with upstream. The `disj`, `sort`,
`take-while`, and `zipmap` whole-file fixtures reuse incompatible concrete
domains and remain audited static errors rather than dynamicized tests.

The following higher-order batch promotes `set`, `some`, and `when-first` on
both targets. Unary polymorphic callback instantiation preserves `identity`'s
element/result relationship, closed EDN truthiness preserves nil/false
semantics, and cross-representation set equality compares tuple map entries
with vector entries through a closed EDN conversion. `get-in` remains outside
because its upstream fixture embeds an unselected infinite `(range)` inside a
heterogeneous nested map; eager closed EDN packing does not terminate. `vec`
remains outside because the ClojureScript fixture requires a vector to alias a
mutable JavaScript array, while LG's Native/Melange RRB vector representation
copies array contents.

The following lazy iteration and map construction batch promotes `cycle`,
`doseq`, `hash-map`, and `interleave` on both targets. The upstream fixtures
exercise bounded consumption of infinite sequences, nested binding modifiers,
variadic `apply`, duplicate map keys, heterogeneous entries, and nested maps.
Empty structural maps are compatible with runtime maps regardless of the
runtime map's static key type, and nested generic map compatibility is checked
recursively. This fixes independently instantiated empty-map equality without
dynamic storage or a name-based special case.

The upstream `fnil` fixture remains an audited static error. It reuses one
closure created by `(fnil test-fn 100)` first with `nil`/integer input and then
with a symbol input. The closure's nullable argument has one rigid static
payload type in LG, so the fixture cannot require both `int` and `string`
without an explicit closed sum. The promoted copy is intentionally omitted.

The following numeric extrema batch promotes `max` and `min` on both targets.
Native exact mixed integer/ratio calls use the closed ratio domain and explicit
cross-domain equality. Melange nil/numeric calls use zero only for comparison
and retain the selected original value as `option<int>` or `option<float>`, so
nil identity is not silently replaced by numeric zero. The fixtures cover NaN,
infinities, variadic order, and single-argument non-number identity without
dynamic values.

The following numeric ordering batch promotes `<`, `<=`, `>`, and `>=` on both
targets. Native ratio/integer comparisons remain exact in the closed ratio
domain, ratio/float comparisons widen explicitly, and decimal ordering keeps
its exact decimal representation. Melange preserves target-specific nil-as-zero
comparison. Pairwise short-circuiting, NaN, infinities, mixed numeric types,
unary identity, and variadic `apply` are covered without dynamic dispatch.

The scalar coercion batch promotes `byte`, `char`, `float`, and `parse-boolean`
on both targets, plus `neg-int?` on Melange. `byte` and `float` retain the
ClojureScript identity-cast behavior on Native as well as Melange; mixed
numeric assertions are partitioned by static domain rather than stored in a
universal number value. Melange integer-guard narrowing now projects a
runtime-confirmed safe float or decimal into its existing JavaScript integer
representation before evaluating the guarded branch. `bigint` remains outside
promotion because the upstream namespace requires a distinct `1N` reader and
source domain; Native currently collapses that suffix into `int`, which would
make the fixture's `neg-int? -1N` and `neg-int? -1` expectations contradictory.
No dynamic boundary was added for that missing domain.

The following predicate batch promotes `uuid?` on both targets and `pos-int?`
on Melange. `uuid?` now recognizes the present branch of the statically typed
`option<uuid>` returned by `parse-uuid`, while preserving false for a failed
parse and for every non-UUID static type. `pos-int?` reuses the safe numeric
guard narrowing fixed by the preceding batch; Native remains excluded because
its current source reader does not distinguish `1N` from `1`.

The numeric predicate batch promotes `zero?`, `pos?`, and `neg?` on both
targets. Direct calls preserve int and float behavior and now compare closed
decimal and ratio values against their exact domain zero. Melange additionally
preserves ClojureScript nil and boolean coercion: all three predicates are
false for nil, `pos?` follows the boolean value, and `zero?`/`neg?` remain
false. Native bad-type runtime-exception assertions are omitted because LG
rejects those calls statically; the positive-bigint identity assertion is also
omitted on Native until bigint has a distinct reader/source domain. No numeric
value is packed into `Runtime_dynamic`.

The nested sequence batch promotes `ffirst` and `nfirst` on both targets.
Runtime map entries now expose their two-field tuple as a static seqable value
when key and value share a type, and expected nested-seqable constraints flow
into map literal key/value elaboration, including empty maps. This preserves
map, set, vector, list, range, and string traversal without name-based
dispatch. The upstream bad-inner-type runtime-exception assertions remain
static errors, and `(nfirst "")` is omitted because proving its `option<char>`
is None requires a value-level empty-string type unavailable in the current
type system.

The universal-predicate batch promotes `every?` and `not-every?` on both
targets through the upstream first-class helper. Helper-accumulated static
overloads preserve callable sets and maps, empty and nil collections, truthy
identity values across concrete types, early termination, and infinite
sequences. Non-empty all-nil vectors retain `vector<nil>` inference rather than
collapsing into an empty `vector<any>` overload, and `true?`/`false?` propagate
their declared boolean input to otherwise open call results. The upstream
bad-shape runtime-exception assertions remain static errors and are omitted
under the selected static-error policy. No ordinary value or callback is
stored in `Runtime_dynamic`.

The numeric-parser batch promotes `parse-long` and `parse-double` on both
targets. Native decimal integers are validated against the actual static OCaml
integer range before conversion, so upstream 18-digit values no longer inherit
Melange's JavaScript safe-integer ceiling. Melange keeps the ClojureScript
safe-integer rule. Decimal floats preserve malformed-input rejection,
scientific notation, and positive/negative Infinity. Non-string exception
assertions are omitted because LG rejects those calls statically.

The scalar/UUID/string-lookup batch promotes `short`, `random-uuid`, and a
portable static subset of `get` on both targets. `short` follows the
ClojureScript identity-cast branch. UUID generation retains the upstream
version-four format check through `str`, `clojure.string/split`, and nested
`get-in`. String lookup now performs explicit bounds checks, returns
`option<char>` for two arguments, and accepts a same-typed `char` default for
three arguments. The upstream `get` rows that deliberately mix unrelated
collection/result/default domains remain compile-time static errors and are
not weakened into a universal value.

The lazy-prefix batch promotes `take`, `take-nth`, `take-while`, `drop-last`,
and `drop-while` on both targets. The fixtures retain upstream laziness,
finite/infinite prefixes, empty inputs, boundary counts, negative drop counts,
transducer early termination, and per-reduction state reset. The source
`drop-while` transducer preserves predicate short-circuit order with nested
conditionals so truthy callback capabilities are not collapsed to `bool`.
Native `take-nth` now handles the ClojureScript zero-step transducer result
explicitly instead of raising OCaml `Division_by_zero`; negative steps keep the
same index/remainder order.

The arithmetic batch promotes `+`, `-`, `*`, and `/` on both targets. Direct
and first-class calls cover each supported arity, `apply`, mixed int/float
coercion, decimals, Infinity, and NaN. Native integer `/` now uses the closed
exact-ratio domain for reciprocal, binary, variadic, and applied calls instead
of OCaml truncating division. Melange keeps its separate typed floating path,
including ClojureScript Infinity and NaN results for division by zero. Neither
target introduces dynamic numeric storage.

The hierarchy batch promotes `parents`, `ancestors`, `descendants`, `derive`,
and `underive` on both targets. Its closed EDN fixtures cover direct and
transitive relationships, chains and diamonds, immutable local updates,
global updates with cleanup, symbol tags, idempotent direct edges, and
transitive-closure rebuilding after `underive`. The original `ancestors`
fixture is still gated by `when-var-exists` because it embeds JVM/JavaScript
class objects; the curated fixture bypasses that host-class-only gate and
executes the portable hierarchy behavior directly.

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
- Runtime source-level syntax-quoted constants compile as quoted data, and the
  target reader branches exclude JVM-only ratios while preserving named
  characters. Together with the typed constant-function capability this now
  promotes `constantly.cljc` on Native and Melange.
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
  JVM or JS class objects. The portable keyword/symbol surfaces from
  `parents.cljc`, `ancestors.cljc`, and `descendants.cljc` are promoted through
  curated fixtures. Their remaining mixed EDN named-value vectors such as
  `[TestParentsRecord ::record]` need a closed EDN literal/domain promotion
  rather than a type-name symbol fix.
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
2. Keep upstream `thrown?` cases that LG rejects at compile time in the
   generated audit only; they are not runtime repair work or Dune gates.
3. Repair small source-portable API gaps that do not require broad type-system
   changes.
4. Address regex, watches/ex-data, and multimethod dynamic boundaries with
   narrow documented runtime types where static closed domains are insufficient.
5. Keep ratio and JVM numeric-class behavior explicitly gated unless a closed
   portable representation is added.
