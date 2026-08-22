# LG Current Architecture

> Snapshot date: 2026-07-27  
> Repository commit: `a29abbf898d16e275d7c0b26ded60f7623f8de75`  
> Scope: the current working tree, including local uncommitted changes  
> Authority: `docs/design.md` remains the language and compatibility contract

## Executive summary

LG is a statically typed Clojure-family frontend for OCaml. It parses located
Clojure forms, performs LG-owned inference and semantic elaboration, emits a
typed semantic IR, lowers that IR into an OCaml-oriented IR and Parsetree, and
then asks OCaml compiler-libs to typecheck the result.

The most complex and least maintainable part of the current architecture is:

> **Call elaboration and type adaptation, centered on
> `src/call_elaborator.ml`.**

This is not merely the largest compiler file. It currently combines several
different architectural responsibilities:

- dispatch for hundreds of core function names;
- argument and result compatibility;
- dynamic boundary packing and unpacking;
- row projection and named-record adaptation;
- seqable, printable, truthy, contains, and protocol witnesses;
- callback and overloaded-function adapters;
- OCaml host calls;
- protocol calls;
- metadata, printing, regex, transient, collection, and scalar behavior;
- the composition root for several other elaborators.

The largest downstream domain subsystem is the standalone DataScript port.
Its complexity is mostly inherent: it must preserve a large upstream dynamic
API and control flow while using closed static representations on Native and
Melange. It is difficult, but it has a clearer package and repository boundary
than call elaboration.

The largest maintenance multiplier is the test/build matrix:

- `test/compiler_tests.ml` is about 34,000 lines;
- `test/dune` is about 4,200 lines;
- the current test Dune file contains roughly 128 rules, 49 Melange emits, and
  60 executables;
- a compiler change often requires updating a large monolithic test registry
  and several target-specific build rules.

## Architectural invariants

The following rules come from `docs/design.md` and constrain every layer:

1. Static typing is the default and invariant.
2. An incompatible or unresolved type must not silently become
   `Runtime_dynamic.t`.
3. Known heterogeneous domains use closed variants and records.
4. Absence uses `option`.
5. Declared type variables remain rigid static types.
6. Records, tuples, callbacks, collections, and host values keep concrete
   representations.
7. Java and JVM compatibility are rejected rather than emulated.
8. OCaml interop is explicit and preserves declared host types.
9. DataScript behavior follows the pinned upstream implementation even when
   its representation is replaced with closed static types.
10. `Obj.magic` and public dynamic conversion escape hatches are forbidden.

These invariants explain why LG cannot use a universal runtime value as a
general solution to compiler complexity.

## Top-level pipeline

```text
.cljc source
    |
    v
Lexer + Parser
    |
    v
Located Ast.form tree
    |
    +--> metadata and reader-conditional normalization
    +--> dependency analysis and stable top-level ordering
    +--> macro expansion
    |
    v
LG type inference and semantic elaboration
    |
    +--> Compiler_environment
    +--> Type_solver substitutions
    +--> declaration/protocol evidence stabilization
    |
    v
Lowered.compiled_item + typed Semantic_ir
    |
    v
Semantic_lowering
    |
    v
Ocaml_ir
    |
    v
OCaml Parsetree
    |
    v
compiler-libs typecheck
    |
    +--> Typedtree and warnings for language services
    |
    v
Printed .ml / Native / Melange / js_of_ocaml
```

LG has two typechecking authorities, but they own different questions:

- LG owns source semantics, collection shapes, static protocol constraints,
  structural rows, nullable behavior, and source-oriented rejection.
- OCaml owns host module types, package values, constructor payloads, module
  inclusion, pattern coverage, and final generated-code validity.

## Layer map

| Layer | Main modules | Responsibility |
|---|---|---|
| Source model | `ast.ml`, `lexer.ml`, `parser.ml`, `source_context.ml` | Tokens, forms, spans, reader conditionals, source identity |
| Frontend normalization | `toolchain.ml`, `macro_expander.ml`, `core_form_expansion.ml` | Metadata normalization, namespace scope forms, macro expansion |
| Dependency ordering | `dependency_graph.ml` | Providers, references, SCCs, stable compilation order, recursive groups |
| Type model | `semantic_type.ml`, `types.ml`, `type_solver.ml`, `type_inference.ml` | Type vocabulary, constraints, substitutions, generalization, inference |
| Compiler state | `compiler_environment.ml`, `compiler_state.ml`, registries | Symbols, modules, types, protocols, signatures, macros, target, expected type |
| Expression elaboration | `expression_elaborator.ml`, `special_form_elaborator.ml`, `destructure.ml` | Literals, special forms, functions, control flow, destructuring |
| Call elaboration | `call_elaborator.ml` and specialized call modules | Core calls, adaptation, witnesses, protocol and host calls |
| Top-level elaboration | `top_level_elaborator.ml`, module/type/protocol elaborators | Definitions, modules, signatures, variants, records, protocols |
| Typed IR | `semantic_ir.ml`, `lowered.ml` | Typed expressions and top-level semantic items |
| Backend lowering | `semantic_lowering.ml`, `ocaml_ir.ml`, `ocaml_parsetree.ml` | Explicit type-erasure boundary and structured OCaml generation |
| Final checking | `toolchain.ml` | compiler-libs environment, Parsetree checking, Typedtree, diagnostics |
| Runtime | `runtime/`, EDN backends | Typed collections, seqs, refs, weak storage, target-specific primitives |
| CLI and cache | `bin/lg_cli.ml` | Compile/run modes, incremental states, package linking, Marshal caches |
| LSP | `language_service.ml`, `bin/lsp_server.ml` | Typedtree-based hover, definition, references, rename, completion, indexing |
| DataScript integration | external `datascript-lg` package | Repository-boundary validation only; the typed port, tests, parity artifacts, and benchmarks live in the standalone project |

## Frontend

### Source representation

`Ast.form` is the source-level syntax tree. The lexer and parser retain spans,
and `Source_context` maps forms to OCaml `Location.t` values and stable source
node identities.

Source locations flow through:

```text
Ast.form
  -> typed Semantic_ir.Located
  -> Ocaml_ir.Located
  -> Parsetree locations and attributes
  -> Typedtree
  -> CLI and LSP diagnostics
```

This is a strong part of the architecture. Generated nodes without a direct
source form inherit an owning location instead of losing diagnostics.

### Normalization

`Toolchain.Lg_frontend` currently performs more than parsing. It also
normalizes:

- metadata type hints;
- `^:dynamic` definitions;
- reader-conditionals and target selection;
- namespace-scope forms;
- source locations;
- host type hint spelling.

This is practical, but the frontend boundary should remain explicit:
normalization may reshape syntax, while type-directed behavior belongs in
elaboration.

### Macros

The current working tree contains macro definitions, registries, inline
macros, macro functions, macro values, gensym generation, and macro expansion.
Expansion happens before ordinary typed elaboration, and the expanded forms
must pass the same typechecking path as authored forms.

The macro implementation is part of current architecture, but the static
design contract still applies: expansion must not introduce a general runtime
dynamic escape hatch.

## Dependency ordering and recursive definitions

`dependency_graph.ml` computes symbol providers and references, strongly
connected components, stable ordering, and recursive definition groups.

Before elaboration, `toolchain.ml` rewrites recursive groups into declarations
and normalized recursive-definition forms. This separates source order from
the order required to infer mutually dependent declarations.

The same dependency model is reused by the workspace language service to
reanalyze only affected dependency components.

This reuse is valuable, but it couples batch compilation and editor behavior
to the accuracy of source-level dependency extraction. Any new source form
that defines or references names must update dependency analysis as well as
elaboration.

## Type system

### Type vocabulary

`semantic_type.ml` defines the central type sum:

- scalars: int, float, char, string, regex, symbol, keyword, bool, unit;
- absence: `TNil`, `TNullable`;
- variables: `TUnknown`, `TMeta`, `TVar`;
- host types: `TOcaml`, `TOcaml_app`;
- tuples, arrays, refs, lists, vectors, sets, seqs;
- functions and overloaded functions;
- structural and named records.

`types.ml` includes this vocabulary and adds compiler-facing concepts:

- symbol bindings and schemes;
- host references;
- row parameter metadata;
- constraint encodings;
- assignability and representation checks;
- source and OCaml type naming;
- set module selection;
- protocol and capability witness layouts.

### Constraint representation

Several LG-only constraints are encoded as special `TOcaml_app` names:

- seqable;
- optional seqable/sequential;
- contains;
- truthy;
- printable;
- symbol predicate;
- protocol witness;
- open/dynamic boundary.

These are not ordinary source-visible OCaml applications. They are compiler
constraints whose value representation is later converted to witness tuples,
options, or runtime boundary values.

This encoding keeps the public `ty` sum smaller, but it makes maintenance
harder because many modules must recognize reserved string names and preserve
their payload conventions.

### Solver and inference

`type_solver.ml` owns:

- fresh metavariables;
- substitutions;
- occurs checks;
- unification;
- generalization and instantiation.

`type_inference.ml` adds source semantics and domain-specific refinement:

- branch and collection inference;
- protocol constraints;
- callback and function types;
- record fields;
- open constraint refinement;
- definition and body inference.

The separation is directionally correct: the solver is generic, while
inference understands LG forms. In practice, compatibility decisions also
exist in `types.ml`, `call_elaborator.ml`, collection capability modules, and
specialized elaborators. The hard maintenance problem is therefore not the
solver itself but keeping all compatibility layers consistent.

### Evidence stabilization

The compiler does not always infer final declarations in one pass.
`toolchain.ml`:

1. builds dependency-stabilized forms;
2. compiles an evidence form when recursive declarations require it;
3. collects declaration ABI and protocol evidence;
4. seeds another compiler state;
5. recompiles until declaration ABI stabilizes;
6. fails after 16 passes.

Evidence passes do not emit final code. When a chunk contains forward or
recursive declarations, the evidence AST therefore retains only the ordinary
definitions needed by those declarations and their transitive source
dependencies. Unrelated function bodies are replaced by empty declaration
placeholders. A definition already covered by a previously compiled sidecar
signature contributes a declaration instead of recompiling its body. Inline
signatures still retain their bodies because the same chunk may need their
inferred capability and row evidence. The final full pass always compiles and
validates every source definition, and any ABI change continues through the
same fixed-point loop.

The ABI comparison covers:

- canonical type scheme;
- row parameter types;
- overload row types;
- overload targets;
- return-parameter relationships.

This fixed-point process is one of the most semantically complex parts of LG.
It is necessary for current recursive inference, but it makes failures and
performance sensitive to changes in declarations, protocol evidence, and
dependency normalization.

## Compiler environment and state

`Compiler_environment.t` contains:

- target;
- symbol bindings;
- protocol registry and optional protocol evidence;
- module registry;
- type registry;
- signature overlays;
- anonymous records;
- namespace aliases and core exclusions;
- macros, inline macros, macro functions, and macro values;
- contextual expected type.

Symbol bindings and the local-name constructor index use a persistent
bitmap-indexed hash trie ported from ClojureScript's `PersistentHashMap` and
`BitmapIndexedNode` control flow. Exact lookup and path-copying updates avoid
materializing the complete environment, while the secondary local-name index
avoids scanning every namespace for constructor candidates. Both indexes share
the immutable binding and type payloads.

`Compiler_state.t` adds:

- current scope;
- next type identifier;
- accumulated `Lowered.compiled_item` values;
- shared-value tracking.

`Toolchain.state` adds another outer state:

- typecheck state;
- located items accumulated across chunks;
- optional OCaml compiler environment.

The three state layers serve real purposes, but ownership must remain clear:

- `Compiler_environment` is semantic name/type context;
- `Compiler_state` is one LG compilation state;
- `Toolchain.state` is incremental multi-chunk plus compiler-libs state.

New caches or registries should be placed according to that ownership instead
of being added to whichever state is easiest to reach.

## Expression and call elaboration

### Expression elaboration

`expression_elaborator.ml` handles literals and Clojure-shaped syntax:

- symbol lookup;
- vectors and maps;
- `if`, `cond`, `case`, logical forms;
- `let`, `loop`, `recur`;
- functions;
- threading forms;
- `match`, `try`;
- comprehensions;
- keyword and set invocation;
- ordinary calls.

It delegates ordinary calls to `Call_elaborator` and shares recursive
callbacks through `Elaboration_context`.

### Call elaboration

`call_elaborator.ml` is the current architectural hotspot.

Measured in the current working tree:

| Metric | Value |
|---|---:|
| Lines | about 11,947 |
| Referenced modules reported by `ocamldep` | 49 |
| Changes in the last 80 commits | 48 commits |
| `Runtime_dynamic` textual references | about 75 |

The file begins with reusable compatibility and adaptation functions, then
`create` constructs a large mutually recursive elaborator. It wires:

- `Special_form_elaborator`;
- `Collection_operation_elaborator`;
- `Sequence_call_elaborator`;
- `Function_combinator_elaborator`;
- `Comparison_set_elaborator`.

It then adds its own large dispatch and implementation set.

The difficulty is not just line count. Four kinds of change intersect here:

```text
new core API behavior
    x static type compatibility
    x runtime representation
    x host/protocol/callback adaptation
```

A change to one dimension can silently invalidate the other three.

Examples:

- changing seqable constraints affects function arguments, collection
  transforms, witness layout, generated OCaml, and callbacks;
- changing record compatibility affects row projection, equality, protocol
  receivers, set modules, and named-record adaptation;
- changing dynamic boundary rules affects argument packing, callback packing,
  metadata, printing, transients, and protocol paths;
- changing overloaded functions affects arity selection, storage tuples,
  projections, wrappers, and inferred call results.

This is the first subsystem that should be decomposed for maintainability.

### Specialized elaborators

The specialized modules already provide useful domain boundaries:

- collection operations;
- sequence transforms;
- function combinators;
- comparisons and sets;
- special forms;
- protocols;
- modules and type definitions.

However, `Call_elaborator` remains both their composition root and the owner of
shared conversion policy. Callback injection avoids direct OCaml module
cycles, but it also makes ownership hard to follow.

## Typed IR and backend

### Semantic IR

`Semantic_ir.t` is the typed semantic expression tree. Each expression may be
wrapped in:

- `Typed`;
- `Located`;
- explicit conversion nodes such as `PackDynamic`, `UnpackDynamic`, and
  `NullableToSeq`.

`Lowered.compiled_item` is the corresponding top-level IR for:

- value and recursive bindings;
- deferred polymorphic bindings;
- records, variants, aliases;
- modules, signatures, functors, includes, and aliases;
- protocols and other emitted definitions.

The architecture deliberately separates expression semantics from top-level
structure.

### Explicit lowering boundary

`semantic_lowering.ml` erases `Semantic_ir.Typed` and converts expressions into
`Ocaml_ir`.

`ocaml_parsetree.ml` converts top-level `Lowered.compiled_item` and lowered
expressions into structured Parsetree nodes. It also:

- maps LG types to OCaml core types;
- creates record and variant declarations;
- emits set modules;
- relocates generated structures;
- tracks requested set definitions;
- removes unused anonymous types;
- supports incremental item emission.

The Parsetree backend is large, but its responsibility is substantially
clearer than call elaboration. It is not currently the primary maintenance
problem.

### Final OCaml check

`Toolchain.Ocaml_typechecker` initializes compiler-libs, manages include
directories, typechecks the generated structure, captures warnings, and
returns:

- `Typedtree.structure`;
- a summarized compiler environment;
- source-located diagnostics.

All public compile paths pass through this final gate.

## Runtime architecture

`lg.runtime` builds in Native, bytecode, and Melange modes. It provides:

- RRB vectors;
- typed and polymorphic sets;
- maps and transients;
- lazy/memoized sequences;
- refs, weak references, slots, and reduced values;
- static comparison, hashing, printing, strings, UUID, time, and random;
- EDN support through a target-selected backend;
- narrowly scoped dynamic runtime support.

Target-specific modules use explicit suffixes or backend libraries, for
example:

- `runtime_seq.ml` / `runtime_seq_melange.ml`;
- `runtime_weak_stdlib.ml` / `runtime_weak_melange.ml`;
- `runtime_int.ml` / `runtime_int_melange.ml`;
- Native and Melange EDN backend implementations.

This is preferable to hiding target differences behind a universal value.
The maintenance risk is semantic drift between paired modules.

## Targets

The supported target model is:

| Target | Compiler | Generated program/runtime |
|---|---|---|
| Native | Native compiler tool | OCaml native |
| Melange | Native compiler tool | Melange-compatible OCaml/JavaScript |
| js_of_ocaml | Native compiler tool | OCaml bytecode compiled to JavaScript |

Reader conditionals select target-specific forms. Unselected forms are parsed
but not elaborated or OCaml-typechecked.

Target selection flows through:

- parser features;
- compiler environment;
- runtime helper selection;
- required OCaml packages;
- final build/link commands;
- test matrix.

A target-specific behavior change therefore needs both compiler and runtime
tests.

## Incremental compilation and cache

`Toolchain.state` preserves semantic state, located emitted items, and the
compiler-libs environment across chunks.

`bin/lg_cli.ml` can save compilation state and prefix caches with OCaml
`Marshal`. Cache identity currently incorporates:

- OCaml version;
- compiler artifacts or executable digest;
- target;
- the content digest of a restored saved state, when compilation resumes from
  one;
- previous prefix key;
- input path and source.

Both fresh and saved-state compilation cache every source prefix. A fully warm
saved-state build reads the cached generated source and final cacheable state
without reconstructing the compiler-libs environment. Saved-state chunks already
defer OCaml checking to the generated compilation unit, so a partial cache miss
continues from the cached semantic state after registering the required package
include directories; it does not parse the preceding generated sources. Fresh
compilation restores its compiler-libs environment lazily at the first cache
miss, using the preceding generated sources.

The compiler-libs environment itself is removed from the cacheable state and
reconstructed by parsing and typechecking cached OCaml sources.

This is a reasonable safety boundary, but cache compatibility remains tied to
OCaml Marshal representation and compiler state shape. Cache reads therefore
must continue to fail closed and rebuild instead of attempting partial
recovery.

## Language service

`language_service.ml` uses both LG semantic state and OCaml Typedtree.

It provides:

- diagnostics;
- hover types;
- go-to-definition;
- references and rename;
- signature help;
- semantic tokens;
- completion;
- document and workspace symbols;
- dependency-component workspace updates.

The LSP's semantic identity can come from:

- OCaml `Uid`;
- LG type, module, protocol, or method identities;
- source spans and source node IDs.

This dual identity system is powerful but coupled to the entire compiler
pipeline. A change to generated names, source attributes, registry identity,
or Typedtree shape can affect editor behavior even if compiled programs still
run correctly.

## DataScript subsystem

DataScript is the largest downstream domain subsystem, but it is not part of
this repository. The standalone `datascript-lg` project owns its implementation
and tests. Its persistent index implementation is supplied by the separate
`persistent-sorted-set-lg` project.

The selected `.cljc` source and test trees contain roughly 43,000 lines. Major
files include:

- upstream-shaped database implementation;
- query parser;
- typed query implementation and query type definitions;
- pull API and parser;
- connection, storage, serialization, and transaction behavior;
- upstream tests and differential fixtures.

The architecture is:

```text
Pinned upstream behavior
    |
    v
standalone datascript-lg
    +--> upstream-shaped LG source
    +--> closed DataScript runtime types
    +--> API manifest and upstream test inventory
    +--> differential cases
    +--> Native and Melange builds
    +--> performance/scaling benchmarks
    |
    +--> installed lg + lg-test packages
    +--> installed persistent-sorted-set-lg package
```

Known heterogeneous domains are modeled explicitly:

- data values;
- query inputs, sources, rows, and results;
- parser return forms;
- transaction entries and state transitions;
- schemas;
- serialization and storage payloads.

DataScript is difficult to maintain because every change must satisfy three
constraints:

1. upstream observable behavior;
2. LG static representation rules;
3. Native and Melange performance.

Unlike call elaboration, this complexity has a documented upstream boundary
and domain-specific types. It should remain isolated rather than being solved
with general compiler dynamic behavior.

## Tests and build matrix

The LG test architecture includes:

- one large compiler test executable;
- focused runtime tests;
- Native and Melange generated programs;
- native and JavaScript benchmarks;
- a large Dune rule matrix.

Upstream DataScript test ports, differential scripts, manifests, and DataScript
benchmarks are part of `datascript-lg`. Persistent sorted set tests and
benchmarks are part of `persistent-sorted-set-lg`.

Current maintenance hotspots:

| File | Approximate size |
|---|---:|
| `test/compiler_tests.ml` | 34,387 lines |
| `test/dune` | 4,212 lines |
| compiler test functions | 1,409 |
| `test/dune` rules | 128 |
| `test/dune` Melange emits | 49 |
| `test/dune` executables | 60 |

The test suite has strong coverage, but its structure makes ownership and
focused execution harder than necessary. The build file also repeats target
matrices that should be derived from a smaller manifest.

## Complexity and maintainability ranking

### 1. Call elaboration and type adaptation

Severity: **highest**

Why:

- largest compiler module;
- broadest dependency fan-out;
- highest recent change frequency after the monolithic test file;
- owns both policy and implementation;
- mixes static, host, protocol, runtime, and API concerns;
- uses a very large mutually recursive closure;
- shares behavior with multiple specialized elaborators through callbacks.

Failure mode:

A local API fix changes representation or compatibility behavior elsewhere.

### 2. Type inference and evidence stabilization

Severity: **high**

Why:

- `type_inference.ml` is about 5,700 lines;
- compatibility logic is distributed across multiple modules;
- recursive definitions use a multi-pass fixed point;
- protocol evidence and declaration ABI feed back into recompilation;
- reserved constraint encodings must be recognized consistently.

Failure mode:

A type appears to compile in one context but fails, widens, or requires a
different witness in another context.

### 3. DataScript

Severity: **high, mostly inherent**

Why:

- largest domain and compatibility surface;
- upstream behavior, static representation, and performance all constrain it;
- Native and Melange must remain aligned;
- transaction, query, pull, storage, and serialization are independent complex
  state machines.

Failure mode:

A representation simplification changes upstream ordering, laziness,
multiplicity, retry, or storage semantics.

### 4. Test and Dune matrix

Severity: **high maintenance cost**

Why:

- monolithic test source;
- monolithic test registration list;
- repeated target rules;
- difficult subsystem ownership;
- small changes produce large review diffs.

Failure mode:

Tests exist but are hard to locate, run, or update consistently across targets.

### 5. Toolchain, compiler-libs, LSP, and cache state

Severity: **medium-high**

Why:

- combines LG state with OCaml compiler environments;
- Typedtree shapes and UIDs affect tooling;
- incremental and batch compilation must agree;
- cache restoration replays generated OCaml;
- global compiler-libs state and include directories require care.

Failure mode:

Batch compilation succeeds while incremental compilation, restored cache, or
editor analysis diverges.

### 6. Dynamic compatibility boundary

Severity: **medium architectural debt**

Why:

- `Runtime_dynamic` still appears in compiler and runtime internals;
- call elaboration contains most compiler-side references;
- some core behavior still has both static and dynamic branches;
- the design contract permits only narrow documented internal boundaries.

Failure mode:

A compatibility branch grows into a general conversion path or leaks into
public types and collections.

## Recommended maintenance boundaries

These are architectural directions, not authorization to change behavior.

### Decompose call elaboration by policy

Keep one thin call dispatcher, but move policy into named modules:

```text
Call_dispatch
  -> Argument_adaptation
  -> Callback_adaptation
  -> Row_adaptation
  -> Protocol_call
  -> Host_call
  -> Runtime_boundary
  -> Core API family elaborators
```

The first extraction should be argument/result adaptation because it is reused
by core calls, host calls, callbacks, and protocols.

Avoid creating another compatibility overlay. Extract existing behavior and
tests without changing branch order or generated representation.

### Make constraint kinds explicit

Reserved `TOcaml_app` strings should gradually become one closed compiler
constraint representation or be accessed exclusively through one module.

At minimum:

- construction and matching stay in one module;
- payload order is not repeated by callers;
- lowering owns the mapping to witness tuples;
- source-visible host applications cannot collide with compiler constraints.

### Separate inference from adaptation

Inference should answer the static type relationship. Adaptation should build
the required witness, projection, wrapper, or rejection.

Today these decisions are sometimes interleaved in `type_inference.ml`,
`types.ml`, and `call_elaborator.ml`. A single compatibility classification
should drive code generation:

```text
Exact
Row_projection
Nullable_adaptation
Seqable_witness
Protocol_witness
Host_deferred
Incompatible
```

Any open/dynamic internal boundary should be a separately named case with a
documented justification, not the default classification.

### Split the compiler tests by subsystem

Recommended ownership:

- frontend and reader;
- types and solver;
- functions and calls;
- collections and sequences;
- records, variants, protocols;
- modules and host interop;
- backend and diagnostics;
- incremental compilation and LSP;
- rejection/static-safety tests.

A small shared test support library can keep helpers without retaining one
34,000-line registry.

### Generate the test target matrix from a manifest

Keep test cases and target expectations in a compact data file, then generate
repetitive Dune stanzas deterministically.

The generator should:

- be simple and repository-local;
- emit stable ordering;
- have a stale-output check;
- make Native/Melange parity visible;
- avoid hiding unique test behavior in generation templates.

### Inventory dynamic boundaries

Maintain a machine-readable list of permitted dynamic boundaries:

- file and function;
- concrete upstream or runtime behavior;
- why a closed type is insufficient;
- accepted input and validated static output;
- test proving the boundary stays narrow.

CI can reject new `Runtime_dynamic` references outside the inventory.

### Preserve DataScript isolation

Do not move DataScript-specific heterogeneity into generic compiler logic.

Prefer:

- closed domain variants;
- named state records;
- typed accessors;
- one dispatch sum per domain;
- upstream-shaped control flow;
- differential tests and measured local optimizations.

## Change routing guide

| Change | Primary modules | Also verify |
|---|---|---|
| New syntax or reader form | lexer, parser, frontend normalization | formatter, locations, LSP tokens |
| New special form | expression/special-form elaborator | dependency graph, inference, locations |
| New core function | matching specialized call module | dispatcher, type inference, Native/Melange runtime |
| Type relationship | `types.ml`, `type_solver.ml`, inference | call adaptation, Parsetree type mapping |
| New collection capability | collection capability and sequence modules | witnesses, callbacks, equality, first-class rejection |
| Record/row change | structural map, types, call adaptation | sets, protocols, Parsetree, LSP |
| Protocol change | protocol modules and registries | evidence stabilization, call adaptation, modules |
| OCaml interop change | require, host interop, signatures | package loading, final OCaml check, cache restore |
| Backend node | semantic IR/lowering, OCaml IR/Parsetree | source locations, incremental emission |
| Runtime behavior | typed runtime module | target pair, compiler lowering, parity tests |
| DataScript behavior | mapped upstream/domain file | API manifest, differential test, both targets, benchmark |
| LSP behavior | language service/server | source IDs, Typedtree identities, incremental workspace |

## Validation layers

Changes should be verified at the narrowest relevant layer and then through the
full boundary they affect:

```text
unit/type rejection
  -> generated Semantic_ir or OCaml
  -> compiler-libs typecheck
  -> Native execution
  -> Melange execution when portable
  -> incremental compilation when stateful
  -> LSP behavior when identities/locations change
  -> DataScript differential and benchmark when applicable
```

Native and Melange Dune commands must be serialized because they share the
same Dune build lock.

## Current strengths

Despite the hotspots, the architecture has several strong foundations:

- one explicit static design contract;
- a closed central type vocabulary;
- a separate typed semantic IR;
- structured Parsetree output without source-string fallback;
- compiler-libs as a final correctness gate;
- preserved source locations into Typedtree;
- typed registries with nominal identities;
- incremental semantic and OCaml compiler state;
- dependency-component workspace analysis;
- explicit target runtime modules;
- pinned DataScript upstream behavior and differential artifacts;
- measured, documented performance deviations.

The maintenance goal should be to preserve these boundaries while reducing the
number of responsibilities concentrated in call elaboration, inference
stabilization, and the test matrix.
