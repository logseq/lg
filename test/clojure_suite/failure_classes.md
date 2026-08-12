# clojure-test-suite failure classes

This is the current classification from:

- the local upstream clone at `vendor/clojure-test-suite`
- upstream commit `6299706516f55d2ccb2d5abb5662489971584dea`
- a local sample scan over the first 5 upstream namespaces
- the interrupted full scan output up to `clojure.core-test.drop-while`
- source-wide pattern counts over `vendor/clojure-test-suite/test/clojure/core_test/*.cljc`

The full compile scan was intentionally paused before completion. Do not treat
the counts below as pass/fail totals; use them to prioritize repair batches.

## 1. Test harness/support namespaces

Observed examples:

- `clojure.core-test.abs` fails because `clojure.core-test.number-range` is not loaded.
- `clojure.core-test.add-watch` fails because the LG portability shim does not expose `sleep`.

Source-wide indicators:

- `clojure.core-test.number-range` is required by 35 core test namespaces.
- `clojure.core-test.portability` helpers beyond `when-var-exists` are needed by watch, lazy, and exception tests.

Likely handling:

- Port or shim suite support namespaces first.
- Keep these under `test/clojure_suite`, not in the production stdlib unless a helper is actually public Clojure API.

## 2. Host/JVM/JS boundary tests

Observed examples:

- `clojure.core-test.ancestors` fails on native with `unknown symbol Object`.
- The same namespace fails on Melange with `unknown symbol js/Object`.

Source-wide indicators:

- JS host forms appear in 10 namespaces.
- JVM/class forms such as `Object`, `Boolean`, and `clojure.lang.*` appear in 16 namespaces.
- Var identity forms (`#'`) appear in 6 namespaces.
- Object/array interop forms (`object-array`, `into-array`, `make-array`) appear in 12 namespaces.

Likely handling:

- Classify as host-boundary unless LG has an explicit portable static representation.
- Do not emulate JVM class identity in the compiler.
- Where a test has a CLJS branch that avoids JVM behavior, prefer running that branch for both native and Melange only if LG's intended semantics are CLJS-aligned for that var.

## 3. Numeric tower and literal support

Observed examples:

- `clojure.core-test.str` fails on `unknown symbol 0N`.
- `bigint`, `bigdec`, arithmetic, predicate, and comparison tests contain bigint, ratio, and bigdecimal literals.

Source-wide indicators:

- Bigint literals appear in 78 namespaces.
- Bigdecimal literals appear in 71 namespaces.
- Ratio literals appear in 70 namespaces.
- `clojure.core-test.number-range` is also numeric-heavy and blocks 35 namespaces.

Likely handling:

- Decide explicit LG numeric tower scope before fixing these.
- If only int/float remain supported, these tests need an unsupported-feature classification rather than dynamic fallback.
- If bigint/ratio/decimal are in scope, add reader, type, runtime, print, equality, ordering, and arithmetic tests before implementation.

## 4. Static compile error vs runtime `thrown?`

Observed examples:

- `clojure.core-test.count` expects `(p/thrown? (count 1))`; LG rejects it statically: `count expects a counted or seqable value, got int`.
- Many numeric and collection tests assert runtime exceptions for bad argument types.

Source-wide indicators:

- `p/thrown?` / `thrown?` appears in 139 namespaces.

Likely handling:

- Split these into two lanes:
  - LG static-error compatibility tests for cases intentionally rejected at compile time.
  - Runtime exception tests only for calls whose argument types are statically valid.
- Do not weaken static typing to make negative runtime tests compile.

## 5. Open IFn behavior and seq coercion

Observed examples:

- `clojure.core-test.apply` fails at `(apply + "")`: Clojure treats an empty string as an empty seq at runtime, while LG types strings as `seq<char>` and `+` requires ints.
- The same test also covers maps, keywords, vectors, and sets used as functions.

Likely handling:

- Model supported IFn cases as typed capabilities or closed dispatch forms.
- Keep invalid/open calls as static errors unless the compatibility boundary is explicitly documented.
- Avoid introducing a universal dynamic function application path.

## 6. Watches, validators, dynamic vars, futures

Observed examples:

- `add-watch` currently first fails on missing `sleep`, but the source also covers atom watches, var watches, `ex-info`/`ex-data`, validators, and `alter-var-root`.
- `binding`, `bound-fn`, and future tests cover thread/dynamic binding behavior.

Source-wide indicators:

- Watch/validator APIs appear in 3 namespaces.
- Binding/future forms appear in 7 namespaces.

Likely handling:

- Atoms watches are plausible stdlib/runtime work.
- Var watches, `alter-var-root`, and thread/future propagation are host/compiler boundary decisions.

## 7. Hierarchy and multimethod related behavior

Observed examples:

- `ancestors` reaches host type inheritance immediately.

Source-wide indicators:

- Hierarchy APIs appear in 6 namespaces: `ancestors`, `derive`, `descendants`, `make-hierarchy`, `parents`, `underive`.

Likely handling:

- Keyword/symbol hierarchy relationships are source-portable.
- Class/type inheritance parts are host-boundary.
- Multimethod/hierarchy dynamic payloads may justify a documented narrow dynamic boundary, but not source-level dynamic escape hatches.

## 8. Transients

Source-wide indicators:

- Transient APIs appear in 9 namespaces.

Likely handling:

- LG already has typed transient constraints; upstream tests likely need filtering between supported typed transient cases and unsupported open/heterogeneous cases.

## 9. Regex

Source-wide indicators:

- Regex forms/functions appear in 6 namespaces.

Likely handling:

- Regex is a good candidate for a narrow host-boundary runtime type.
- Keep captures and return shapes statically modeled where possible.

## 10. Print dialect mismatch

Observed examples:

- `clojure.core-test.println-str` expects `17.0` on the default/JVM branch and `17` on the CLJS branch.
- LG currently renders whole floats as `17.` through OCaml formatting.

Likely handling:

- Decide whether native LG should follow Clojure JVM or CLJS formatting for each printed type.
- Then make `str`, `print-str`, `println-str`, `pr-str`, and DataScript/EDN print expectations consistent across native and Melange.

## Suggested next order

1. Finish support namespace ingestion: `number-range`, full LG `portability` shim.
2. Complete the scanner so paused runs keep partial JSON and add a summarizer that groups errors by normalized message.
3. Promote only namespaces that compile on both native and Melange into Dune smoke.
4. For failures, repair in this order:
   - source-portable support shims;
   - static-error test lane for negative runtime cases;
   - print formatting;
   - regex;
   - watches/ex-data;
   - hierarchy/multimethods;
   - numeric tower only after scope is decided.
