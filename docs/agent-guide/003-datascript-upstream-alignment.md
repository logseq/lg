# DataScript Upstream Alignment and Inference Cleanup Implementation Plan

Goal: Restore the complete Logseq DataScript public API and upstream-shaped implementations while making the compiler infer ordinary static types without pervasive source annotations or any dynamic fallback.

Architecture: Separate solver metavariables, rigid declared variables, and generalized inferred variables, then infer each top-level dependency component with bidirectional constraints before lowering semantic IR.
Closed variants, records, options, and explicit host boundaries remain authoritative, while algorithm bodies recover their types from constructors, field access, calls, collection operations, pattern matching, and loop recurrence.
Once the compiler can express the required types without local hints, port upstream DataScript control flow directly and retain only measured representation optimizations.

Tech Stack: OCaml 5, Dune, Melange, LG static Clojure, RRB vectors, persistent-sorted-set, DataScript CLJC, Node.js, and ClojureScript.

Related: Builds on `docs/agent-guide/001-clojure-core-static-compiler.md` and supersedes the source-annotation-heavy parts of `docs/agent-guide/002-datascript-performance-remediation.md`.

## Problem statement

The current DataScript port contains approximately 1,852 inline type hints and 149 explicit `signature` forms.
The persistent-sorted-set implementation contains approximately 107 inline type hints and 13 explicit signatures.
Most of these annotations repeat information that already exists in closed record fields, variant constructors, function calls, collection operations, or loop initializers.

This annotation density makes the port harder to compare with upstream and encourages a second implementation instead of a static representation of the upstream algorithm.
It also hides compiler inference gaps because a new annotation is usually easier than fixing the missing constraint propagation.

The current compiler uses `TUnknown` and `TVar` for several different purposes.
Solver-local unknowns, declared rigid type parameters, deferred placeholders, and inferred polymorphic variables are therefore refined by a large set of special cases.
`src/type_inference.ml` and `src/function_elaborator.ml` contain name-based and shape-based recovery paths that can infer individual examples but do not form a clean module-level inference model.

DataScript behavior has also diverged from upstream in places that static typing does not require.
The query implementation supports only default-source patterns and a hard-coded two-argument `>` predicate.
Query relation outputs do not apply upstream set semantics or `:with` processing.
Entity references are returned as raw `Data_value.Ref` values instead of lazy entities.
Connection constructors lose supplied database options.
Pull omits the options and visitor arities.
Transaction input omits transaction functions and raw datoms.

The solution is not to restore dynamic values.
The solution is to give the compiler enough static inference to keep the upstream implementation readable, then model every known heterogeneous boundary with a closed sum.

## Compatibility contract

The authoritative baseline is the Logseq DataScript fork at commit `3f141af`.
The implementation must track the CLJS-visible public API of that fork.

Every public namespace, var, function name, arity, accepted source form, option, callback position, output shape, ordering rule, uniqueness rule, and error condition must remain compatible.
Static typing may replace an internal map, vector, protocol dispatch, or universal value with a closed record or sum, but it must not remove or rename a public API.
Compile-time elaboration may turn literal query, pull, transaction, schema, or option forms into closed values as long as the caller keeps the same source syntax and observable behavior.
Programmatically constructed runtime forms must remain supported through a closed EDN or domain-specific representation rather than `Runtime_dynamic.t`.

Target-specific host facilities may use different internal implementations and concrete host types, but every DataScript API available in the pinned Logseq fork must exist on both LG Native and LG Melange.
An API that accepts or returns a JavaScript host object must retain the same public operation through a target-specific typed adapter on Native; host representation differences are not permission to omit or narrow the DataScript API.

## Baseline and acceptance criteria

| Area | Current baseline | Required outcome |
|---|---:|---|
| DataScript inline type hints | Approximately 1,852 | At least 95 percent removed, with every remaining hint reviewed and no hints on ordinary callback, `let`, `loop`, or reducer bindings |
| DataScript explicit signatures | Approximately 149 | Retained only for host boundaries, intentionally constrained public APIs, or genuinely ambiguous recursive interfaces |
| PSS inline type hints | Approximately 107 | No algorithm-local hints, except a single target-specific path representation boundary if inference cannot remove it |
| PSS explicit signatures | 13 | Retained only where storage callbacks or polymorphic recursion require an interface |
| Generated dynamic use | None in DataScript hot paths | Remains none |
| Unsafe casts | None | Remains none |
| Query behavior | Benchmark subset | Upstream patterns, functions, predicates, rules, negation, disjunction, aggregation, pull expressions, inputs, `:with`, return maps, and result uniqueness |
| Public API | Several narrowed arities and preconstructed inputs | Same names, arities, accepted source forms, output shapes, and errors as the pinned Logseq fork |
| Entity behavior | Raw ref values | Upstream lazy entity navigation with statically typed entity results |
| Connection options | Some options discarded | Full typed option propagation |
| Pull behavior | Typed pattern, three-argument API | Upstream arities, visitor behavior, cycle handling, and parse reuse through static representations |
| Transaction behavior | Closed subset | Upstream transaction forms represented by a closed `tx-entry` sum |
| Performance | Native and Melange faster on 11 workloads | No tracked workload slower than upstream and no material regression from the current LG baseline |

An upstream algorithm file passes the readability criterion when removing a type hint does not require moving the same hint to an adjacent helper.
Type declarations and external package signatures do not count as annotation noise.
Local callback parameters, reducer accumulators, loop variables, direct record receivers, and constructor-determined return values must be inferred.

## Testing Plan

Testing will compare observable behavior rather than internal type structures.
Implementation work uses a test-first red-green-refactor loop.
Each implementation batch starts with a focused failing behavior or performance regression, then runs focused compiler checks, differential DataScript execution, generated-code inspection, and relevant performance measurements after the minimal implementation change.

Create a differential runner under `test/datascript/differential/` that executes the same behavior cases against pinned upstream ClojureScript, LG Native, and LG Melange.
Assert that LG relation outputs contain no duplicate tuples before normalizing upstream set order for comparison.
Preserve ordering for index sequences, pull vectors, transaction reports, and other order-sensitive APIs.

Cover query patterns, lookup refs, multiple sources, scalar and collection inputs, relation inputs, predicates, function clauses, rules, recursion, `not`, `not-join`, `or`, `or-join`, `and`, aggregates, pull expressions, `:with`, return maps, duplicate projection, empty results, and all four find shapes.

Cover entity forward references, reverse references, cardinality-many references, component recursion, caching, `touch`, identity, equality, and database-version behavior.

Cover connection option propagation, storage attachment, reference policy, listeners, reset, transaction metadata, and stored transaction tails.

Cover pull wildcard, explicit attributes, aliases, defaults, limits, transforms, reverse references, recursion limits, cycles, visitors, missing entities, and `pull-many` parse reuse.

Cover transaction maps, add, retract, retract attribute, retract entity, CAS, raw datoms, transaction functions, nested maps, reverse refs, tuple maintenance, unique identity retries, component cascades, tempids, ordering, and error cases.

Cover PSS lookup, insertion, deletion, split, rotation, forward and reverse slices, iterator repeatability, weak restoration, storage addresses, and sorted-array construction.

Generate an API manifest from the pinned fork and compare it with LG.
The manifest must include public vars, function arities, protocol methods, option keys, and reader-visible tagged forms.
The comparison must fail if LG silently omits or narrows an upstream API.

Run Dune commands serially.
Use these commands as the final validation sequence:

```sh
rtk dune exec test/compiler_tests.exe
rtk dune exec test/runtime_dynamic_tests.exe
rtk dune exec test/runtime_transient_tests.exe
rtk dune exec test/datascript_runtime_tests.exe
rtk dune build @datascript-api-smoke
rtk dune build @datascript-storage-smoke
rtk dune build @datascript-conn
rtk dune build @datascript-differential
```

The expected result is zero failures.
The current stale compiler tests that expect automatic dynamic widening must be updated to expect static errors or closed-sum behavior.
The current virtual-library and generated-module failures in the DataScript smoke aliases must be fixed as build configuration problems rather than bypassed.

Run the complete benchmark matrix with 20,000 people, a 2-second warmup, five 1-second samples, batch size 10, and isolated processes.
Run upstream JavaScript, LG Native, and LG Melange separately.
Repeat any workload whose winning margin is below 3 percent at least five times and compare medians.

Scan generated Native and Melange sources for forbidden boundaries:

```sh
rtk rg -n "Obj\\.magic|Runtime_dynamic|__lg_dynamic|__lg_dynamic-narrow|\\bto_dynamic\\b|\\bof_dynamic\\b" _build/default/test
```

The expected result is no hit in project-authored DataScript, PSS, or generated application code.

## Architecture

The target compiler flow is:

```text
source forms
    |
    v
dependency graph and top-level SCCs
    |
    v
predeclare function arities with fresh solver metavariables
    |
    v
bidirectional constraint collection
    |  constructors, calls, records, patterns, collections, loop/recur
    v
solve one substitution set per SCC
    |
    v
generalize eligible inferred functions
    |
    v
typed semantic IR
    |
    v
Native or Melange lowering
```

The compiler must distinguish these type categories:

| Category | Purpose | May escape an SCC |
|---|---|---|
| Solver metavariable | Unknown being solved during inference | No |
| Rigid declared variable | User-declared type parameter in a signature or type declaration | Yes, unchanged |
| Generalized inferred variable | Polymorphic variable quantified after solving a function definition | Yes, through a type scheme |
| Concrete type | Record, variant, option, collection, scalar, function, or host type | Yes |
| Dynamic implementation boundary | Documented compiler/runtime compatibility boundary only | Never as a source type |

Do not encode these categories through `TVar` string prefixes.
Do not use `TUnknown` as both an inference hole and a request to generate a dynamic representation.
Do not materialize an unsolved metavariable as dynamic.
An unsolved variable at an exported or executed boundary is a compile error with the source location and the conflicting or missing evidence.

This is intentionally smaller than the OCaml type checker.
LG inference covers source-level functions, let-polymorphism, closed algebraic data, records, options, collections, protocols, and the representation choices needed before OCaml code exists.
OCaml remains responsible for modules, functors, host packages, exhaustiveness, and final host-language validation.
Do not duplicate OCaml features that can be expressed and checked after semantic lowering.

Use `@ocaml-development` while changing compiler, Dune, Native, or Melange behavior.
Use `@code-simplify` when removing superseded inference branches and annotation scaffolding.

## Annotation policy

Annotations are permitted at these boundaries:

- Closed record, variant, and alias declarations.
- Declared public interfaces that intentionally constrain a more general implementation.
- OCaml package and external host function signatures.
- Target-specific representation boundaries such as the Native/Melange PSS path type.
- Polymorphic recursion that cannot be inferred without an explicit interface.
- Ambiguous empty collections that receive no expected type from any caller, field, return position, or recurrence.

Annotations are not permitted merely to help the current compiler with:

- A callback whose parameter types are fixed by `map`, `reduce`, `filter`, `sort`, or another higher-order function.
- A reducer accumulator fixed by its initial value and result.
- A `loop` variable fixed by its initializer and every `recur`.
- A direct field access on a known record.
- A match branch fixed by a closed constructor.
- A function return fixed by all branches.
- A collection element fixed by `conj`, lookup, iteration, or a caller.
- An option payload fixed by `Some`, `None`, `if-some`, or `match`.
- A value passed directly to a function with a known type scheme.

Add an annotation audit to CI.
The audit must count algorithm-local hints separately from type declarations and external signatures.
It must fail if DataScript or PSS adds a new local hint without an allowlisted design explanation.

### Reviewed remaining annotation inventory

The reviewed DataScript inventory contains 95 inline annotations: 91 declaration
annotations and 4 algorithm-local annotations. Persistent sorted set contains no
inline annotations. The 91 declaration annotations are part of the closed static
model rather than inference scaffolding:

| Boundary | Count | Justification |
| --- | ---: | --- |
| Nominal record and deftype fields | 70 | Preserve the closed shapes of datoms, databases, filtered databases, transaction reports, entities, connections, and parser forms. |
| Typed global state | 2 | Constrain the data reader registry and weak stored-database registry. The query cache is constrained by its explicit signature without a duplicate inline annotation. |
| Protocol method parameters | 19 | Define closed index, equality, ordering, relation, and storage dispatch contracts. |

The four algorithm-local annotations were each removed and independently
recompiled against the complete Native DataScript source set. Each removal has a
specific static failure and is therefore retained:

| Location | Boundary | Failure without the annotation |
| --- | --- | --- |
| `datascript.db/entid-strict` | Nominal `DB` receiver | The named database record reaches a dynamic boundary as an anonymous structural record. |
| `datascript.lg.query-types/resolve-not` | Recursive relation result | The recursive SCC emits an unbound OCaml value without an explicit result interface. |
| `datascript.storage/serialize-node` | `Datom array` returned by the PSS node accessor | A datom reaches a dynamic boundary before indexed serialization. |
| `datascript.core/filter` | Nominal `FilteredDB` result | Downstream hashing sees an anonymous `__lg_record` instead of the nominal filtered database type. |

These four annotations may be removed only after a general inference improvement
eliminates the corresponding failure. Do not replace them with dynamic values,
source conversion helpers, name-based inference, or DataScript-specific compiler
branches.

## Phase 1: Pin upstream and record the parity matrix

Files:

- `test/datascript/UPSTREAM.md`
- `test/datascript/upstream/`
- `test/datascript/upstream_tests/`
- `test/datascript/differential/`
- `docs/design.md`

Tasks:

1. Record the authoritative Logseq DataScript repository URL, commit `3f141af`, and file mapping in `test/datascript/UPSTREAM.md`.
2. Treat commit `3f141af` as the pinned compatibility baseline.
3. Record which LG files correspond to upstream `query.cljc`, `db.cljc`, `pull_api.cljc`, `impl/entity.cljc`, `conn.cljc`, and storage implementations.
4. Generate a manifest of every upstream public API name, arity, protocol method, option key, accepted source form, and tagged reader form.
5. Add an automated comparison that rejects missing or narrowed LG APIs.
6. Build a table of every query clause, find element, input binding, pull option, transaction form, and serialization option.
7. Mark each difference as representation-only, measured optimization, missing behavior, or a genuine target-host boundary.
8. Add output fixtures for the behavior categories in the testing plan.
9. Fix the Dune aliases so each existing DataScript smoke target resolves the correct Native or Melange virtual-library implementation.
10. Confirm the baseline differential runner reports known differences without modifying implementation behavior.
11. Update `docs/design.md` with the pinned upstream definition and the rule that missing behavior cannot be justified by annotation difficulty.

Expected result:

- The repository has one reproducible upstream baseline.
- The complete public API surface is machine-checkable.
- Every behavior restoration task has a named differential case.
- Existing local modifications in a separate upstream checkout are irrelevant to the comparison.

## Phase 2: Separate inference variables from source type variables

Files:

- `src/semantic_type.ml`
- `src/types.ml`
- `src/type_solver.ml`
- `src/compiler_environment.ml`
- `src/signature_elaborator.ml`
- `src/type_parameters.ml`
- `src/semantic_ir.ml`
- `test/compiler_tests.ml`

Tasks:

1. Introduce an explicit solver metavariable representation with stable numeric identity.
2. Keep declared type parameters rigid and distinguish them from solver metavariables.
3. Introduce a type-scheme representation containing quantified inferred variables and a monotype body.
4. Store type schemes, rather than bare polymorphic `ty` values, for generalized symbol bindings.
5. Keep monomorphic bindings for refs, mutable values, transients, effectful initializers, and explicitly rigid declarations.
6. Move occurs checks, substitution application, free-variable collection, and generalization into `src/type_solver.ml`.
7. Instantiate generalized variables freshly at every call.
8. Never instantiate rigid declared variables as dynamic values.
9. Remove reliance on names such as `let_fn_`, `sequence_element_`, and `lg_deferred_` to determine type-variable semantics.
10. Preserve source locations on metavariables and constraints so unresolved-variable errors point to the expression that lacked evidence.
11. Update language-service rendering to display generalized variables consistently and hide solver-local identities.
12. Update compiler behavior checks after the new solver representation is complete.

Expected result:

- A source type parameter and an inference hole can no longer be confused.
- A polymorphic inferred function can be called at multiple static types.
- Mutable values remain monomorphic.
- No solver variable lowers to `Runtime_dynamic`.

## Phase 3: Infer top-level dependency components

Files:

- `src/dependency_graph.ml`
- `src/elaborator.ml`
- `src/top_level_elaborator.ml`
- `src/function_elaborator.ml`
- `src/module_metadata.ml`
- `src/compiler_state.ml`
- `test/compiler_tests.ml`

Tasks:

1. Reuse `Dependency_graph.strongly_connected_components` for all top-level function definitions, not only groups that already contain `declare` or `defn-signature`.
2. Predeclare every function arity in an SCC with fresh parameter and return metavariables.
3. Predeclare explicitly annotated positions with their declared concrete or rigid types.
4. Elaborate every body in the SCC against the shared predeclared environment.
5. Collect self-call and mutual-call constraints in the same substitution set.
6. Solve the complete SCC before emitting semantic IR.
7. Apply solved types to every parameter, recursive binding, return expression, and environment entry.
8. Generalize eligible non-recursive functions after solving.
9. Generalize recursive functions only when the inferred monotype is sound under the value restriction.
10. Require an explicit interface for true polymorphic recursion.
11. Remove `declare` and `defn-signature` forms whose only purpose was ordering or inference.
12. Preserve declarations that are actual module interfaces or cross-chunk contracts.
13. Emit one diagnostic for an unsolved SCC instead of cascading errors for every call site.
14. Verify incremental compilation serializes and restores inferred schemes without converting them to dynamic types.

Expected result:

- Forward and mutually recursive DataScript helpers infer without local signatures.
- Function order can remain close to upstream.
- Sidecar signature files stop acting as a manual dependency solver.

## Phase 4: Complete bidirectional local inference

Files:

- `src/type_inference.ml`
- `src/type_solver.ml`
- `src/expression_elaborator.ml`
- `src/function_elaborator.ml`
- `src/special_form_elaborator.ml`
- `src/collection_operation_elaborator.ml`
- `src/sequence_call_elaborator.ml`
- `src/destructure.ml`
- `src/protocol_elaborator.ml`
- `src/structural_map.ml`
- `test/compiler_tests.ml`

Tasks:

1. Pass expected types from function return positions into branch expressions.
2. Pass expected parameter types into function literals used as call arguments.
3. Infer `map`, `mapv`, `filter`, `keep`, `group-by`, `sort`, and sequence callback parameters from collection element types.
4. Infer `reduce` callback accumulator and element parameters from the initializer and collection.
5. Unify the callback return with the reducer accumulator.
6. Infer `loop` binding types from initializers.
7. Unify every `recur` argument with the corresponding loop binding.
8. Infer option payloads from `Some`, `None`, `if-some`, `when-some`, and constructor matches.
9. Infer closed-variant payloads from constructors and propagate them into match patterns.
10. Infer nominal records from constructors, declared fields, direct field access, and function expectations.
11. Resolve structural record fields without widening the record to a dynamic map.
12. Infer collection element types from literals, `conj`, lookup, iteration, reducers, and expected return types.
13. Keep empty collections as unsolved metavariables until contextual inference finishes.
14. Reject a truly ambiguous empty collection with a concise request for one boundary annotation.
15. Infer array element types through `aget`, `aset`, `Array.map`, `Array.fold`, and array construction.
16. Infer comparator argument types from sorted-set operations and comparator fields.
17. Preserve concrete collection storage while satisfying `seqable<T>` constraints.
18. Infer overloaded function arities independently and combine them only after every arity is solved.
19. Remove refinement branches that create `Types.dynamic_constraint` when the only issue is an unsolved static variable.
20. Replace name-based inference heuristics with explicit constraints.
21. Keep host-boundary assignability strict after expected-type propagation.
22. Update behavior checks after each completed inference category.

Expected result:

- Ordinary upstream-shaped callbacks and loops compile without hints.
- Empty collections require annotations only when the program genuinely provides no type evidence.
- Heterogeneous collections still fail and request a closed sum.

## Phase 5: Use PSS as the inference acceptance corpus

Files:

- `datascript/me/tonsky/persistent_sorted_set.cljc`
- `datascript/me/tonsky/persistent_sorted_set/arrays.cljc`
- `datascript/me/tonsky/persistent_sorted_set/protocol.cljc`
- `test/datascript/persistent_sorted_set/`
- `docs/design.md`

Tasks:

1. Define one target-selected `path` representation alias for Native integer paths and Melange floating-point paths.
2. Define one closed comparator function type at the public sorted-set boundary.
3. Keep `tree`, `storage`, `btset`, iterator, and settings record fields concrete.
4. Remove parameter hints from path helpers once calls and arithmetic determine the alias.
5. Remove comparator hints from binary search and sorted-set helpers once the public comparator boundary propagates.
6. Remove address hints once storage records and `option<int>` fields determine them.
7. Remove callback hints from slice reducers and iterators.
8. Remove loop-variable hints once initializer and recurrence inference is complete.
9. Remove redundant PSS signatures after SCC inference resolves forward helpers.
10. Keep a signature only if it represents a real public polymorphic interface or storage callback boundary.
11. Compare generated Native and Melange PSS representations before and after annotation removal.
12. Run all PSS behavior suites.
13. Run PSS storage tests with both `Strong` and `Weak`.
14. Run PSS benchmarks and reject any material regression.
15. Add the annotation audit with PSS as the first enforced source tree.

Expected result:

- The 1,724-line PSS algorithm reads like the upstream algorithm.
- Algorithm-local type hints are eliminated.
- Native and Melange retain their intended path representations.

## Phase 6: Restore DataScript connection and public API parity

Files:

- `test/datascript/upstream/conn.cljc`
- `test/datascript/upstream/core.cljc`
- `test/datascript/upstream/storage.cljc`
- `test/datascript/upstream/storage_file.cljc`
- `test/datascript/api_smoke.cljc`
- `test/datascript/storage_smoke.cljc`

Tasks:

1. Pass the supplied typed options through `conn-from-datoms`.
2. Pass the supplied typed options through `create-conn`.
3. Preserve storage and reference policy together instead of applying storage after constructing a default database.
4. Keep `Conn` as a concrete typed record with one authoritative state reference.
5. Preserve upstream listener replacement and callback order.
6. Preserve transaction metadata and `:skip-store?` behavior with a closed metadata representation.
7. Restore every public API arity from the pinned Logseq fork.
8. Preserve public option source syntax while elaborating options into a closed record.
9. Keep docstrings synchronized with the compatible API and target-host behavior.
10. Keep Native/Melange storage selection explicit and free of Java aliases.
11. Verify restored and newly created connections use the same typed option path.

Expected result:

- Connection construction matches upstream observable behavior.
- No connection helper requires local parameter hints.

## Phase 7: Replace the benchmark-only query executor with an upstream-shaped closed executor

Files:

- `test/datascript/upstream/parser.cljc`
- `test/datascript/upstream/built_ins.cljc`
- `test/datascript/lg/query.cljc`
- `test/datascript/lg/query_types.cljc`
- `test/datascript_runtime/query_value.ml`
- `test/datascript_runtime/query_value.mli`, if introduced for an explicit runtime boundary
- `test/datascript/upstream_tests/datascript/test/query*.cljc`
- `test/datascript/upstream_tests/datascript/test/parser*.cljc`

Tasks:

1. Restore the pinned fork's public `q` call shape, including its variadic input API.
2. Preserve literal query source syntax by elaborating literals into the closed query representation at compile time.
3. Support programmatically constructed query values by validating a closed EDN representation into the same query representation at runtime.
4. Keep parser forms as closed records and variants.
5. Restore the upstream query context fields for relations, constants, sources, rules, and default source.
6. Represent query clause dispatch with one closed clause match that mirrors upstream branch order.
7. Restore default and explicit source patterns.
8. Restore collection-source patterns.
9. Restore arbitrary static built-in predicates by dispatching the existing closed `query-function` sum.
10. Restore typed input callable predicates without introducing a universal callable value.
11. Restore function clauses with a closed callable result boundary.
12. Restore `and`.
13. Restore `not` and `not-join` with upstream binding checks and subtraction semantics.
14. Restore `or` and `or-join` with upstream free-variable validation and union semantics.
15. Restore rules, required variables, recursive expansion, and repeated-use guards.
16. Restore all input binding shapes and multiple sources.
17. Restore relation uniqueness before find-shape post-processing.
18. Restore `:with` by including its variables during uniqueness and projecting them away afterward.
19. Restore aggregates using the existing closed aggregate-function sum.
20. Restore pull find expressions through typed pull options.
21. Restore return maps.
22. Preserve all four find shapes through the closed `output` sum.
23. Preserve lookup-ref database association for entity-valued variables.
24. Retain the measured bound-entity EAVT specialization only as a fast path inside the upstream pattern branch.
25. Retain constant substitution only for a single-row input relation when multiplicity cannot change.
26. Remove the hard-coded `>` executor after generic closed built-in dispatch is active.
27. Remove unused built-in constructors or implement their upstream behavior.
28. Remove query-local signatures and callback hints made redundant by compiler inference.
29. Compare each clause category against upstream differential fixtures.
30. Run the complete upstream query and parser cases on Native and Melange.
31. Re-run all query benchmarks after every restored clause family.

Expected result:

- Query is a static representation of upstream control flow rather than a benchmark-specific second implementation.
- Duplicate projection, `:with`, rules, aggregates, and compound clauses match upstream.
- Query source remains readable without pervasive hints.

## Phase 8: Restore entity and pull behavior

Files:

- `test/datascript/upstream/entity.cljc`
- `test/datascript/upstream/pull_parser.cljc`
- `test/datascript/upstream/pull_api.cljc`
- `test/datascript/upstream/core.cljc`
- `test/datascript/upstream_tests/datascript/test/entity.cljc`
- `test/datascript/upstream_tests/datascript/test/pull_api.cljc`
- `test/datascript/upstream_tests/datascript/test/pull_parser.cljc`

Tasks:

1. Define a closed entity attribute result that distinguishes scalar data, one entity reference, and many entity references.
2. Return lazy typed `Entity` values for forward reference attributes.
3. Return typed entity collections for cardinality-many references.
4. Return typed entities for reverse references.
5. Restore recursive component touching.
6. Keep entity cache fields concrete and monomorphic.
7. Preserve entity equality by database identity and entity ID.
8. Preserve entity hash consistency without Java identity APIs.
9. Keep pull parser output as closed `PullPattern` and `pull-attr` types.
10. Restore source pull syntax through compile-time elaboration into `PullPattern`.
11. Parse programmatically assembled pull forms through a closed runtime pull-form representation.
12. Restore the options arity with a typed pull-options record.
13. Restore visitor callbacks with a closed visit-event sum or a concrete typed callback.
14. Parse or elaborate a pattern once for `pull-many`.
15. Preserve upstream list-stack frame order with one closed frame sum.
16. Retain cached cursor nodes and immediate child conversion only where documented.
17. Preserve wildcard, reverse, recursion, cycle, default, limit, transform, alias, and missing-value behavior.
18. Remove state and callback annotations after inference fixes determine them.
19. Run entity and pull differential cases.
20. Re-run all three pull benchmarks on Native and Melange.

Expected result:

- Entity navigation and pull API behavior match upstream.
- Pull remains statically typed and retains its measured performance.

## Phase 9: Complete the closed transaction model without changing upstream order

Files:

- `test/datascript/upstream/db.cljc`
- `test/datascript/upstream/conn.cljc`
- `test/datascript/upstream/core.cljc`
- `test/datascript/upstream_tests/datascript/test/transact.cljc`
- `test/datascript/upstream_tests/datascript/test/upsert.cljc`
- `test/datascript/upstream_tests/datascript/test/validation.cljc`
- `test/datascript_runtime/data_value.ml`

Tasks:

1. Map every upstream transaction input form to one `tx-entry` constructor.
2. Add a closed raw-datom transaction constructor.
3. Add a concrete typed transaction-function constructor.
4. Define the transaction-function input and output types without dynamic values.
5. Preserve installed transaction-function lookup through a closed registry keyed by ident.
6. Preserve upstream dispatch order for maps, operation vectors, raw datoms, and invalid inputs.
7. Preserve add, retract, retract attribute, retract entity, and CAS branch order.
8. Preserve nested entity expansion and reverse-reference expansion.
9. Preserve component cascade order and incoming-reference cleanup.
10. Preserve tuple queueing and flushing order.
11. Preserve tempid allocation and value-only tempid validation.
12. Preserve unique-identity retry behavior from the initial report.
13. Keep `TxSetTuple` and tuple flush markers internal.
14. Compare the explicit continuation worklist to upstream branch by branch.
15. Remove any continuation or state field that exists only because of an earlier simplified algorithm.
16. Retain the worklist only if it is a semantics-preserving representation of the same upstream expansion order.
17. Infer helper, reducer, and loop types from the closed `tx-entry` and `TxReport` types.
18. Remove transaction-local hints after compiler inference is complete.
19. Run all transaction, schema, tuple, component, and upsert differential cases.
20. Re-run add and bulk-transaction scaling benchmarks.

Expected result:

- Static transaction data covers the upstream domain.
- Transaction ordering and retry behavior are upstream-compatible.
- No raw transaction vector requires dynamic representation.

## Phase 10: Align serialization boundaries and documentation

Files:

- `test/datascript/upstream/serialize.cljc`
- `test/datascript/upstream/storage.cljc`
- `test/datascript/upstream/core.cljc`
- `test/datascript_runtime/serialization_value.ml`
- `test/datascript_runtime/storage_value.ml`
- `docs/design.md`

Tasks:

1. Keep the default serialized payload as a closed sum.
2. Define a concrete typed codec record for upstream custom serialization callbacks.
3. Restore every public serialization arity through that typed codec instead of a universal function value.
4. Preserve the upstream option source syntax through compile-time elaboration and closed runtime option forms.
5. Preserve schema, datom ordering, index reuse, branching settings, and `Strong | Weak` policy.
6. Preserve old-format compatibility only through explicit closed version constructors.
7. Infer serialization helpers from the closed payload and codec fields.
8. Remove redundant serialization hints.
9. Run round-trip fixtures across Native and Melange.
10. Verify weak storage can release and restore nodes after deserialization.

Expected result:

- Serialization documentation matches the static API.
- Custom behavior, if retained, is typed at the smallest boundary.

## Phase 11: Remove inference workarounds and annotation scaffolding

Files:

- `src/type_inference.ml`
- `src/function_elaborator.ml`
- `src/special_form_elaborator.ml`
- `src/expression_elaborator.ml`
- `src/signature_overlay.ml`
- `test/datascript/lg/annotations.cljc`
- `test/datascript/upstream/*.cljc`
- `test/datascript/lg/*.cljc`
- `datascript/me/tonsky/persistent_sorted_set.cljc`

Tasks:

1. Delete sidecar signatures now inferred from source.
2. Keep external OCaml package signatures in one boundary-focused file.
3. Remove inline hints from callback parameters.
4. Remove inline hints from reducer accumulators.
5. Remove inline hints from loop variables.
6. Remove inline hints from direct record receivers.
7. Remove return hints determined by constructors or all branches.
8. Remove empty-collection hints when caller or field context determines the type.
9. Delete name-prefix inference behavior replaced by explicit metavariables and schemes.
10. Delete dynamic refinement paths that are unreachable under the static design.
11. Consolidate duplicate constraint logic into `src/type_solver.ml`.
12. Keep elaborators responsible for language semantics, not ad hoc whole-program type recovery.
13. Run `ocamlformat` on modified OCaml files.
14. Run the annotation audit and review every remaining allowlisted hint.
15. Update `docs/design.md` with the final annotation policy and inference model.

Expected result:

- DataScript and PSS algorithm files are visually comparable with upstream.
- Remaining annotations describe real boundaries rather than compiler weaknesses.
- Compiler complexity decreases after inference is centralized.

## Phase 12: Final upstream review and performance gate

Files:

- `test/datascript/UPSTREAM.md`
- `test/datascript/differential/`
- `test/datascript/benchmark/`
- `test/datascript/upstream_bench/`
- `docs/design.md`
- `docs/agent-guide/003-datascript-upstream-alignment.md`

Tasks:

1. Regenerate the file-by-file upstream parity matrix.
2. Regenerate the public API manifest and require an exact match on both Native and Melange.
3. Review every remaining algorithmic diff.
4. Revert any diff that is neither a static representation requirement nor a documented measured optimization.
5. Confirm every retained optimization preserves branch order, cursor movement, result multiplicity, and termination.
6. Run all compiler and runtime suites.
7. Run all DataScript Native and Melange suites.
8. Run the differential suite.
9. Run the generated-code forbidden-boundary scan.
10. Record final annotation counts.
11. Record the remaining annotations and their boundary justifications.
12. Run the complete upstream, Native, and Melange benchmark matrix.
13. Repeat close results and record medians.
14. Run bulk-transaction and PSS scaling gates.
15. Update `docs/design.md` with final accepted representation differences and benchmark results.
16. Commit in reviewable phases, with compiler inference changes separate from DataScript behavior restoration.

Expected result:

- Native and Melange pass the same ported upstream behavior set.
- Both remain faster than upstream across the tracked benchmark matrix.
- The implementation is static, readable, and close enough to upstream for future diffs to be reviewed directly.

## Edge cases

- Mutually recursive functions with different arities must share one SCC without merging unrelated arity types.
- Polymorphic recursion must require an explicit interface rather than being guessed.
- Mutable refs and transients must obey a value restriction and remain monomorphic.
- Empty maps, vectors, sets, lists, queues, and arrays must wait for contextual evidence and fail if still ambiguous.
- Heterogeneous literals must continue to request a closed sum.
- `nil` must not become an inference wildcard and must remain absence represented by `option` where absence is part of the type.
- A record field name shared by several nominal types must use caller, constructor, or receiver evidence and report ambiguity if none exists.
- Pattern-match branches must unify their return type without boxing incompatible branches.
- Native integer PSS paths and Melange floating-point paths must not force numeric annotations through the whole algorithm.
- Generic comparators must instantiate per set without erasing the element type.
- Query relation uniqueness and `:with` semantics must be validated with duplicate source datoms.
- Rule recursion must retain upstream cycle guards and termination.
- Pull recursion must distinguish cycle detection from explicit recursion limits.
- Transaction retry must restart from the initial report and retain forced tempid resolutions.
- Weak storage tests must not rely on deterministic garbage collection timing.
- Incremental compilation must restore inferred schemes without changing rigid variables.
- Language-service hover and signature help must show stable inferred types rather than solver IDs.

## Testing Details

Behavior tests compare normalized public outputs from the pinned upstream implementation, Native LG, and Melange LG.
Compiler checks exercise inferred functions through real calls, records, collections, recursion, options, and higher-order operations rather than inspecting internal solver data structures.
Performance tests use identical workloads, data sizes, warmup, samples, and batches.
Generated-code scans verify that successful inference did not reintroduce dynamic storage or unsafe casts.

## Implementation Details

- Separate solver metavariables, rigid variables, and generalized variables.
- Infer and solve complete top-level SCCs before lowering.
- Propagate expected types bidirectionally into callbacks, branches, records, collections, and loops.
- Apply a value restriction to refs, transients, mutable state, and effectful definitions.
- Use PSS as the first annotation-free acceptance corpus.
- Preserve upstream DataScript branch order and public behavior.
- Use closed sums for query clauses, results, entity values, pull frames, and transaction entries.
- Keep dynamic types out of DataScript and PSS source and generated code.
- Retain only measured, documented representation optimizations.
- Enforce readability through a structural annotation audit.

## Question

There are no open product-scope questions.
The baseline is the Logseq DataScript fork at commit `3f141af`.
All public APIs, including runtime query forms, transaction functions, pull options, visitors, serialization codecs, and APIs backed by host-specific representations, are required compatibility surface on both Native and Melange.
Implementation details may use closed static representations but may not narrow the source API.

---
