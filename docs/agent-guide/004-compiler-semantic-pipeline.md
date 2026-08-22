# ADR 004: Introduce an Explicit Compiler Semantic Pipeline

- Status: Proposed
- Date: 2026-08-13
- Decision owners: LG compiler maintainers
- Related: `docs/design.md`, `docs/architecture.md`, `docs/stdlib.md`, and
  `datascript-lg/docs/agent-guide/003-datascript-upstream-alignment.md`

## Context

LG parses Clojure syntax, infers and checks LG types, elaborates typed semantic
IR, lowers that IR to OCaml Parsetree, and asks the OCaml compiler to perform
the final host-language checks. This overall direction remains correct and is
the architecture required by `docs/design.md`.

The internal boundaries between those stages are less explicit than the
top-level pipeline suggests. Several compiler modules independently inspect raw
`Ast.form` values and recognize special forms or builtins by string. Type
inference, dependency analysis, macro expansion, expression elaboration, and
call elaboration therefore duplicate knowledge about the same source forms.

The central `Semantic_type.ty` sum also represents several different domains:

- LG source types;
- inference metavariables and rigid variables;
- OCaml host types;
- compiler-only capabilities and constraints;
- protocol witness storage; and
- some runtime representation choices.

Compiler-only constraints such as seqable, truthy, printable, contains, and
protocol constraints are encoded as specially named `TOcaml_app` values. This
makes every recursive type operation responsible for recognizing reserved
strings and preserving their payload conventions. `TNullable value` and the
host representation `TOcaml_app ("option", [value])` are also handled together
throughout inference and elaboration, which mixes source semantics with host
representation.

Call elaboration is the point where these concerns converge. It currently
performs name dispatch, arity and overload selection, type compatibility,
structural row projection, protocol and sequence witness construction,
callback adaptation, nullable adaptation, runtime representation conversion,
dynamic-boundary validation, and Semantic IR emission. Splitting this code into
more files without changing the intermediate representations would preserve the
same coupling across module boundaries.

This structure has the following costs:

- adding a source form requires synchronized changes in several modules;
- compatibility checks and emitted conversions can disagree;
- compiler-only constraints are not exhaustive OCaml variants;
- source semantics and target representation decisions are interleaved;
- focused unit tests cannot exercise call adaptation without running much of
  elaboration; and
- the compiler is difficult to migrate incrementally toward self-hosted LG
  source because the semantic interfaces are not small and stable.

The solution must not weaken LG's static guarantees. It must preserve closed
heterogeneous domains, precise collection storage types, upstream Clojure and
DataScript behavior, and OCaml compiler-libs as the final host-language check.

## Decision

LG will adopt an explicit semantic pipeline between macro expansion and OCaml
lowering:

```text
source text
    |
    v
located reader AST
    |
    v
macro expansion and surface normalization
    |
    v
name resolution and core-form decoding
    |
    v
resolved core AST
    |
    v
constraint collection and solving
    |
    v
typed core AST
    |
    v
call selection and typed adaptation planning
    |
    v
Semantic_ir
    |
    v
Ocaml_ir and checked Parsetree
```

The transition will be incremental. Existing source behavior and public
compiler APIs remain stable while individual form families move to the new
pipeline.

### 1. Decode core forms once

After macro expansion and surface normalization, the compiler will convert raw
list shapes into a closed resolved core AST. Core syntax will no longer be
rediscovered independently by inference, dependency analysis, and elaboration.

The resolved AST will contain explicit constructors for semantic forms such as
`let`, `fn`, `if`, `loop`, `recur`, `match`, definitions, modules, and ordinary
calls. It will retain a generic application node for ordinary callable values.

Each resolved node will carry a stable source node identity and location. Nodes
introduced by expansion or normalization will retain the source node that owns
them.

Names that require compiler-owned handling will resolve to a closed identifier:

```ocaml
type builtin_id =
  | Conj
  | Assoc
  | Seq
  | Reduce
  | Hash
  | Print
  | Atom
  | Host_primitive of Host_primitive_id.t
```

The exact constructor inventory will be derived from the remaining private
compiler ABI. A public standard-library function that can be expressed in LG
source will continue to move out of compiler dispatch according to
`docs/stdlib.md`.

String names remain valid at parsing, macro, namespace, and external interop
boundaries. They will not be the internal identity of a resolved core form or
compiler builtin.

### 2. Represent compiler constraints explicitly

Compiler-only constraints will move out of string-named `TOcaml_app` values
into a closed constraint representation. At minimum, it will distinguish:

```ocaml
type constraint_ =
  | Seqable of {
      requirement : [ `Required | `Optional | `Optional_sequential ];
      element : ty;
      storage : ty;
    }
  | Contains of { key : ty; storage : ty }
  | Truthy of ty
  | Printable of ty
  | Hashable of ty
  | Comparable of ty
  | Protocol of protocol_constraint
  | Open_boundary of open_boundary_constraint
```

This representation is compiler-internal. It does not add a source-visible
dynamic type, conversion API, or implicit fallback.

`TOcaml` and `TOcaml_app` will represent actual OCaml host types only. Source
nullability will remain an LG type until representation lowering chooses OCaml
`option`. Code that intentionally accepts an OCaml `option` through interop may
retain a host type, but source-nullability operations will not need to pattern
match both encodings throughout the compiler.

The following relations will remain distinct and will have named APIs:

- inference unification;
- source-type equality;
- structural row compatibility;
- call-boundary assignability;
- runtime representation compatibility; and
- host-boundary deferral to the OCaml typechecker.

No generic `compatible` operation will silently combine these relations.

### 3. Plan adaptations before emitting IR

A call will be resolved and typed before any representation conversion is
emitted. The compiler will produce a closed adaptation plan for every argument
and result that does not use the identity representation:

```ocaml
type adaptation =
  | Identity
  | Nullable of adaptation
  | Row_projection of row_projection
  | Protocol_witness of protocol_witness_plan
  | Sequence_witness of sequence_witness_plan
  | Callback of callback_plan
  | Overload of overload_plan
  | Collection_representation of collection_plan
  | Compose of adaptation list
```

An adaptation planner may either return a valid plan or a structured type
error. Semantic IR emission consumes the plan and does not repeat type
compatibility decisions. Dynamic-boundary adapters remain separate, named by
their documented boundary, and may only be selected by an explicit boundary
plan.

The steady-state call pipeline will be:

```text
resolve callable
    -> select arity and signature
    -> solve argument and result types
    -> plan typed adaptations
    -> emit Semantic_ir
```

### 4. Preserve the OCaml backend boundary

This decision does not replace OCaml Parsetree, Typedtree, or compiler-libs.
OCaml remains responsible for modules, functors, host package signatures,
constructor payload checks, exhaustiveness, and final host-language validation.

Direct compiler-libs usage should remain concentrated in the existing OCaml IR,
Parsetree, toolchain, and language-service boundaries. The resolved and typed
core ASTs will not contain Parsetree nodes.

## Invariants

Every migration step must preserve these invariants:

- no `Obj.magic` or equivalent unsafe cast;
- no implicit conversion to or from `Runtime_dynamic.t`;
- no source-visible dynamic constraint or pack/unpack escape hatch;
- known heterogeneous domains remain closed variants and records;
- collection element, key, value, and storage types remain concrete;
- protocol and sequence witnesses remain statically typed;
- source `nil` remains typed absence and lowers to `option`;
- public source forms, arities, diagnostics, evaluation order, and target
  behavior remain compatible;
- upstream DataScript control flow and observable behavior remain authoritative;
  and
- generated OCaml continues to pass the compiler-libs typecheck gate.

## Consequences

### Positive

- Core syntax is decoded once and handled exhaustively.
- Dependency analysis, inference, elaboration, and LSP tooling share the same
  semantic node identities.
- Adding a builtin produces compiler exhaustiveness failures at the remaining
  integration points instead of relying on string searches.
- Constraint payloads and adaptation plans can be tested without compiling an
  entire source namespace.
- Type compatibility and conversion emission have one contract.
- `call_elaborator.ml` can shrink by responsibility instead of being split
  mechanically.
- The semantic frontend becomes a smaller candidate for future self-hosting,
  while the OCaml compiler-libs bridge remains native.

### Negative

- The compiler will temporarily contain both legacy raw-form paths and resolved
  paths during migration.
- Some forms that currently combine inference and emission will require new
  intermediate data structures.
- Changing the internal type and constraint representation will touch many
  recursive type traversals.
- Exact generated OCaml names or formatting may change unless deterministic
  name allocation is addressed alongside the affected migration batch.
- A premature generic framework could obscure Clojure-specific semantics, so
  each abstraction must be introduced from at least two concrete existing use
  cases.

### Neutral

- Generated programs continue to depend on the typed LG runtime.
- The compiler itself remains a native OCaml tool while it depends on
  compiler-libs.
- Source standard-library bootstrap remains separate from compiler
  self-hosting.

## Alternatives considered

### Keep the current architecture and split large files

Rejected. Moving functions into more modules would reduce file length but keep
raw-form string dispatch, string-encoded constraints, and repeated adaptation
decisions. It would increase navigation cost without establishing semantic
boundaries.

### Move more behavior into runtime dynamic dispatch

Rejected. This conflicts with `docs/design.md`, weakens static collection and
protocol guarantees, and would reintroduce runtime representation costs in
DataScript hot paths.

### Use OCaml Parsetree as the only typed IR

Rejected. LG owns semantics that OCaml types do not directly represent,
including typed lazy sequences, structural source rows, nullable joins,
protocol witnesses, Clojure truthiness, and closed compatibility boundaries.
Parsetree remains a backend rather than LG's semantic type system.

### Rewrite the compiler in one pass

Rejected. A flag-day rewrite would make compatibility regressions difficult to
localize and would compete with current DataScript and standard-library work.
The existing compiler is the executable specification used to migrate one
semantic family at a time.

### Implement full self-hosting first

Rejected. Self-hosting before stable semantic interfaces would reproduce the
same coupling in LG source. This ADR reduces the future self-hosting boundary
without making self-hosting a prerequisite.

## Migration strategy

### Phase 1: Establish resolved identities

1. Inventory compiler-owned forms and private builtins.
2. Introduce `Builtin_id` and resolve a small scalar builtin family to it.
3. Introduce located resolved call nodes while leaving unrelated forms on the
   legacy path.
4. Make dependency analysis and call elaboration consume the resolved identity
   for the migrated family.
5. Add a static audit preventing new string dispatch for migrated builtins.

### Phase 2: Establish the adaptation planner

1. Introduce `Adaptation.Identity` and one concrete row-projection plan.
2. Move the existing compatibility check and IR emission for that projection
   behind the planner.
3. Add nullable, protocol witness, sequence witness, callback, and overload
   plans one at a time.
4. Delete a legacy compatibility/emission branch after its replacement passes
   focused and full compiler tests.

### Phase 3: Replace string-encoded constraints

1. Introduce the closed constraint representation beside existing type values.
2. Migrate seqable constraints first because their element/storage relationship
   crosses several higher-order collection operations.
3. Migrate protocol, truthy, printable, contains, hashing, and comparison
   constraints.
4. Reject new compiler constraint names encoded as `TOcaml_app`.
5. Remove the legacy constraint encodings only after serialized bootstrap state
   has an explicit compatibility or regeneration boundary.

### Phase 4: Complete the resolved core AST

1. Move binding and control-flow forms: `let`, `fn`, `if`, `loop`, and `recur`.
2. Move pattern forms and type definitions.
3. Move module, protocol, macro-result, and host-interoperability forms.
4. Make dependency analysis, type inference, semantic elaboration, formatting,
   and language-service indexing consume the resolved nodes relevant to them.
5. Remove superseded raw-form semantic matching.

### Phase 5: Consolidate the compiler session

After the semantic phases are explicit, adopt a session-owned fresh-name supply
and make batch, incremental, standard-library, and LSP entry points compose the
same phase APIs. Versioned compiler-state serialization is a separate decision
and may be recorded in a follow-up ADR.

## Validation

Every migration batch will be test-driven and behavior-preserving. Validation
will include:

- focused parser/resolution, inference, adaptation, and elaboration tests;
- rejection tests for heterogeneous values and forbidden dynamic boundaries;
- generated Semantic IR and OCaml output checks where representation matters;
- compiler-libs validation of generated Parsetree;
- Native, Melange, and js_of_ocaml coverage for affected portable forms;
- stdlib bootstrap and restore tests when compiler state changes;
- DataScript differential tests when a migrated adaptation is used by
  DataScript; and
- cold-build and benchmark gates for changes on compilation or runtime hot
  paths.

Dune commands must run serially. The exact focused command depends on the
migrated family; a completed phase must pass the repository's full `dune test`
suite and the applicable stdlib/DataScript aliases before legacy code is
deleted.

## Acceptance criteria

This ADR is implemented when:

1. all compiler-owned core forms and builtins have closed internal identities;
2. dependency analysis, inference, and elaboration no longer rediscover those
   forms by raw symbol string;
3. compiler-only constraints are closed variants rather than reserved
   `TOcaml_app` names;
4. source nullability and OCaml `option` are separated until representation
   lowering;
5. every non-identity call conversion is represented by a typed adaptation
   plan before Semantic IR emission;
6. compatibility decisions are not repeated during adaptation emission;
7. the OCaml backend and final compiler-libs validation remain intact;
8. no static-safety or upstream-compatibility invariant has been weakened; and
9. legacy raw-form, constraint-string, and duplicated adaptation paths have
   been removed rather than retained as compatibility overlays.

## Follow-up decisions

Record separate ADRs if the project chooses to change:

- compilation-session ownership of fresh identifiers and caches;
- the persistent/versioned compiler-state artifact format;
- the batch, incremental, and LSP session API;
- structured diagnostic kinds and related locations; or
- the scope and staging policy for compiler self-hosting.
