# DataScript Performance Remediation Implementation Plan

Goal: Make the LG DataScript port faster than upstream ClojureScript on Native and Melange across the complete ported benchmark suite without diverging from upstream behavior or relying on dynamic types where static representations are available.

Architecture: Preserve upstream DataScript algorithms and control flow while
representing each known heterogeneous domain with closed sums and records.
Move repeated work out of hot paths through static query rows, typed cursors,
closed transaction states, allocation-conscious projection, and measured
target-specific runtime improvements.

Tech Stack: Clojure-compatible LG source, OCaml 5, Dune, Melange, Alcotest, Node.js, upstream ClojureScript benchmarks, and the local datascript-ocaml comparison project.

Related: `docs/agent-guide/001-clojure-core-static-compiler.md`,
`docs/design.md`, `test/datascript/benchmark`,
`test/datascript/upstream_bench`, and `src/elaborator.ml`.

## Problem statement

The current DataScript port is functionally close to upstream but its query and transaction hot paths allocate and dispatch substantially more than upstream ClojureScript and datascript-ocaml.

The current 20,000-person q1 workload is faster than upstream ClojureScript on
both Native and Melange. Deeper Melange joins and pull traversal remain the
measured gaps.

The original generated code represented query relations and contexts with broad
`Dynamic.t` fields and explicitly narrowed tuple values dynamically. Those
representations are no longer acceptable. Current work uses closed DataScript
values, query results, sources, inputs, transaction entries, parser forms, and
pull frames; remaining performance work must retain those static domains.

Persistent sorted set traversal itself is lazy and is not the primary
bottleneck, so this work must keep PSS laziness and repeatable, memoized
sequence semantics intact and target the surrounding compiler and runtime
representation costs.

## Current benchmark snapshot

The July 24, 2026 reference run used a 20,000-person query database and the
upstream benchmark policy: 2 seconds of warmup, five 1-second samples, median,
and batch size 10. Lower is better. Upstream prints rounded values; LG values
below retain additional precision only to make regressions easier to spot.

| Case | Upstream CLJS | LG Native | LG Melange |
| --- | ---: | ---: | ---: |
| add-all | 1349.2 ms | 556.756 ms | 832.232 ms |
| q1 | 1.4 ms | 0.581 ms | 1.417 ms |
| q2 | 3.2 ms | 2.202 ms | 4.182 ms |
| q3 | 4.6 ms | 3.883 ms | 7.958 ms |
| q4 | 6.6 ms | 5.420 ms | 11.409 ms |
| q5-shortcircuit | 0.735 ms | 0.266 ms | 0.968 ms |
| qpred1 | 11.5 ms | 6.795 ms | 10.202 ms |
| qpred2 | 12.3 ms | 10.130 ms | 15.769 ms |
| pull-one | 1.0 ms | 0.726 ms | 1.131 ms |
| pull-many | 1.8 ms | 1.720 ms | 2.517 ms |
| pull-wildcard | 3.9 ms | 2.274 ms | 4.209 ms |

Native is faster in every measured case. The pull-many improvement removes two
static representation costs without changing upstream pull control flow:
recursive vector conversion no longer builds an intermediate RRB vector, and
identity map adaptation at an OCaml boundary no longer rebuilds the map through
`to_list` and `of_list`. Deferred self-recursion also calls a local direct
recursive implementation while the holder remains available for genuine
forward calls, matching upstream's direct loop without changing its branches.
Melange is faster for add-all and qpred1, while q1 is close to upstream and the
deeper joins, scalar-input predicate query, and pull traversal still trail.
These remaining Melange gaps are the next profiling targets. They do not
justify a second query or pull algorithm or a return to dynamic representation.

Each row is measured in a separate process. Running all benchmarks in one
process mutates shared benchmark state and leaves JIT and GC history from the
transaction workloads, so those sequential timings are not comparable. Native
q4 showed occasional 6.5–7.4 ms outliers; the latest three isolated repeats had
a 5.42 ms median.

The LG run uses a fixed seed. The clean upstream harness does not expose a seed,
so this table is a directional performance snapshot rather than a bit-for-bit
identical data-set comparison. Performance gates inside this repository use
fixed inputs and semantic result assertions.

## Testing Plan

Testing must establish behavioral and performance contracts before implementation so every optimization remains semantics-preserving on Native and Melange.

| Test layer | Contract | Native | Melange |
| --- | --- | --- | --- |
| Compiler behavior | Typed query tuple access compiles and returns the same values without explicit dynamic narrowing | Required | Required |
| Generated and runtime code | Project-authored source and DataScript-generated OCaml contain no `Obj.magic` | Required | Required |
| Compiler behavior | An immutable quoted query value created in a function is shared across repeated calls while retaining Clojure equality and metadata behavior | Required | Required |
| Runtime behavior | Closed DataScript records and sums preserve lookup, printing, equality, hashing, and protocol dispatch without dynamic conversion | Required | Required |
| Runtime behavior | Keyword and symbol comparison preserves namespace and name ordering for qualified, unqualified, empty, and Unicode identifiers | Required | Required |
| DataScript regression | Upstream query, transaction, pull, entity, serialization, and persistent sorted set behavior and case coverage pass; static constructor adaptations are documented | Required | Required |
| Performance | Warm and full benchmark runs report per-case time, allocation or GC evidence where available, and ratios against upstream ClojureScript | Required | Required |
| Build performance | Incremental test build remains under 1 second when only one test module changes | Required | Required |
| Build performance | Clean build remains under 10 seconds on the reference machine | Required | Required |

All new compiler and runtime regression cases will be added before implementation and run once to confirm that they fail for the intended missing behavior or performance guard.

Performance gates will use multiple warm samples, report the median, isolate compilation from execution, and reject regressions rather than trusting one noisy wall-clock observation.

The final benchmark matrix will cover every ported upstream benchmark rather than optimizing only q1, and it will separately report startup, database construction, query execution, and transaction execution where the harness permits.

NOTE: I will write *all* tests before I add any implementation behavior.

## Design

The optimization boundary is the compiler and runtime support layer, not a rewritten DataScript fork.

Compiler implementation source, runtime implementation source, and generated application code must all remain free of `Obj.magic`.

Static typing changes must preserve upstream DataScript behavior.
`Runtime_dynamic.t` is not an implementation strategy for DataScript data,
protocols, serialization, queries, pull, or transactions because these are
known closed domains.

```text
upstream-compatible DataScript CLJC
                |
                v
     typed LG semantic IR
       |        |        |
       v        v        v
 static tuple  typed    closed records
 operations    cursors  and sums
       \        |        /
        \       |       /
         v      v      v
       smaller OCaml hot paths
          |           |
          v           v
       Native       Melange JS
          \           /
           benchmark matrix
```

Static query tuple annotations must express the actual array element type so
tuple access, equality, hashing, and relation joins compile directly without a
universal runtime wrapper.

Every `Obj.magic` site must be traced to a missing type relation or representation conversion and removed at that source, with explicit typed adapters used only where a real runtime boundary remains.

Quoted immutable collections must be materialized once at the narrowest safe static scope and reused by calls, while expressions containing runtime evaluation, mutable transients, or call-specific metadata remain per-call.

Keyword and symbol comparison must avoid repeated prefix stripping and substring allocation by retaining or lazily caching parsed namespace and name components without changing equality, hashing, printing, or interning behavior.

Melange-specific work must follow evidence from generated JavaScript and allocation profiles after the shared fixes, and must not introduce a second DataScript implementation.

## Milestones

### Milestone 1: Lock contracts in tests

Add behavior tests for typed arrays, shared immutable quoted values, immutable record conversion, and complete identifier ordering edge cases.

Add source and generated-code regression tests that fail if project-authored runtime, compiler, or DataScript application output contains `Obj.magic`.

Add a reproducible benchmark gate that validates its own parsing and threshold logic with fixture input before it executes expensive benchmarks.

Run the focused suite and retain the expected RED evidence for each missing contract.

### Milestone 2: Remove query-path dynamic operations

Give DataScript query tuples and relation values static types that match their upstream runtime shape.

Use direct typed tuple access after compiler behavior tests prove it on both
targets.

Inspect generated OCaml and JavaScript to verify that the hot path no longer emits equivalent `Dynamic.t`, `Obj.magic`, or pack-and-unpack work under another name.

### Milestone 3: Share immutable quoted constants

Extend semantic analysis and emission so safe quoted immutable literals are initialized once and referenced from repeated function calls.

Keep mutable literal construction local when sharing would be observably
incorrect. A literal that needs heterogeneous known values uses a closed sum.

Verify generated code size does not increase beyond the existing four-times-source constraint.

### Milestone 4: Keep DataScript records closed and concrete

Remove record-to-dynamic conversion from DataScript-facing generated code.
Keep database fields, transaction reports, parser nodes, pull frames, storage
payloads, and serialization values concrete.

Verify record lookup, protocol dispatch, printing, equality, hashing, and
record update behavior directly on their static representations.

### Milestone 5: Remove identifier comparison allocation

Store or cache keyword and symbol namespace and name components so comparison does not repeatedly allocate substrings.

Verify the full comparison matrix and profile transaction construction again.

### Milestone 6: Address remaining Melange-specific overhead

Profile the updated query and transaction cases to quantify remaining sequence forcing, RRB conversion, hashing, equality, module, and garbage collection costs.

Apply only changes that improve a measured hot path and preserve the shared Native and Melange semantics.

### Milestone 7: Full validation and report

Run compiler, runtime, upstream DataScript, Native, and Melange test suites from a clean state.

Run every ported benchmark against upstream ClojureScript and datascript-ocaml with identical data sizes and warmup policy.

Record generated-code size, clean build time, incremental build time, full test
time, benchmark ratios, and the scan proving that DataScript-facing generated
code contains no dynamic types.

## Edge cases and constraints

Quoted values with metadata, nested records, functions, regex values, or mutable/transient descendants must not be shared until their semantics are proven safe.

Typed caches and storage references must not retain unbounded dead database
values, particularly in long-lived Melange processes.

Identifier parsing must preserve keywords with no namespace, symbols with multiple slash characters, the special slash symbol, Unicode text, and existing invalid-input behavior.

Benchmark comparisons must use equivalent optimized production configurations and must not mix cold compilation time into runtime measurements.

The benchmark gate must tolerate normal machine noise through warmup and median sampling but still fail a repeatable regression.

Upstream test module names and contents remain one-to-one unless a platform compatibility shim is strictly necessary and documented.

No js_of_ocaml support or import support is included in this work.

Pretty-printer size is outside the optimization scope unless profiling proves it executes in a benchmark hot path.

## Acceptance criteria

All existing compiler and DataScript tests pass on Native and Melange with no new warnings.

Project-authored OCaml and DataScript-generated application OCaml contain zero `Obj.magic` occurrences, with vendored third-party code measured separately.

Every ported LG benchmark is faster than its equivalent upstream ClojureScript benchmark on the reference machine under the documented sampling method.

LG Native also meets or beats the local datascript-ocaml comparison for equivalent operations, or the final report identifies a measured representation difference that requires an explicit follow-up decision.

Incremental test builds remain below 1 second, clean builds remain below 10 seconds, and the full test run remains below 10 seconds on the reference machine.

Generated OCaml attributable to each CLJC module remains no more than four times the source code size under the agreed measurement that excludes pretty-printer support.

DataScript-facing generated code contains no dynamic representation. Any
remaining compiler/runtime dynamic implementation is outside DataScript and
must have a separately documented genuinely open boundary.

The compiler and runtime contain no `Obj.magic`, and no compiler lowering or backend recreates equivalent unchecked casts through another primitive.

## Testing Details

Tests will live in the existing compiler and runtime Alcotest suites, the one-to-one upstream DataScript test tree, and a benchmark gate script with self-tests for result parsing and comparison.

Focused RED and GREEN commands will build one target at a time, and final validation will run Native and Melange sequentially to avoid competing Dune processes.

Benchmark artifacts will record commit, target, toolchain versions, data size, warmup count, sample count, median time, and comparison ratio.

## Implementation Details

- Add typed query tuple and relation annotations without changing query semantics.
- Remove query-local dynamic narrowing after direct typed operations pass on both targets.
- Eliminate generated `Obj.magic` by repairing the responsible type inference and lowering boundaries.
- Hoist safe immutable quoted literals through semantic IR rather than source-specific rewrites.
- Keep immutable DataScript records in their concrete static representation.
- Cache parsed keyword and symbol components and compare them without substring allocation.
- Reprofile before making any Melange-only runtime change.
- Keep PSS traversal lazy and verify DataScript still calls the lazy PSS path.
- Inspect generated OCaml and JavaScript after every compiler representation change.
- Preserve upstream test files and avoid DataScript-specific compiler branches.
- Document benchmark evidence and the zero-dynamic generated-code scan.

## Question

No blocking product decision is required to start because the existing requirement defines upstream ClojureScript as the mandatory benchmark target and datascript-ocaml as an additional Native comparison.

If a benchmark is dominated by a capability that upstream does not implement equivalently, the final report will isolate it instead of silently changing the workload.

---
