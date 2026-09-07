# ADR: Typed DataScript literals and application APIs

- Status: Accepted (implemented)
- Date: 2026-09-07
- Scope: LG, the DataScript package used by ledraw, and ledraw integration

## Context

ledraw currently uses `datascript_ocaml`, not the standalone `datascript-lg`
implementation. Its `ledraw.edn-form` module recursively converts the LG EDN
backend into DataScript query forms. `store.cljc` manually builds query and
transaction syntax, repeatedly parses queries containing application values,
and decodes result rows with defaults that conflate absence and invalid data.
Schema attribute names and options are also maintained in parallel arrays.

Closed static representations are required, but spelling their constructors at
every application call site is not. Previous work added GADTs, polymorphic
fields, and module features; these do not by themselves solve literal syntax
or boundary validation.

## Decision

### Ownership and extensibility

LG owns reusable literal elaboration facilities and compiler diagnostics.
DataScript owns schema, transaction, query, pull, attribute codecs, and result
projection behavior. ledraw owns application attribute declarations and business
logic. No DataScript implementation or reverse dependency enters LG.

Use existing LG macros and ordinary typed functions when they can implement
literal expansion correctly. Extend the compiler only for demonstrated missing
generic capabilities. Do not create a DataScript-specific compiler branch or a
second query/transaction engine.

### Literal elaboration

A library-selected literal builder expands scalar, keyword, symbol, vector,
list, map, and set syntax directly into the library's closed constructors.
Ordinary LG collection inference and quote retain their existing meanings.
The builder is explicit, so heterogeneous DSL syntax does not silently change
the type of an ordinary collection. Embedded expressions are statically checked,
evaluated once in source order, and never converted through a dynamic value.
Unsupported forms and ambiguous conversions produce source errors.

DataScript exposes concise schema, query, and transaction entry points using
this mechanism. Fixed queries are parsed once and receive changing values via
`:in`; a query with a document ID must not be rebuilt for each document.
Where direct transaction construction is possible, avoid an intermediate EDN
backend tree. Dynamic external EDN remains a separately validated library
boundary; preserve its supported semantics and reject lossy conversions.

The earlier discussion's `d/query`, `d/transact!`, and `d/defattr` spellings are
illustrative. Final exported syntax must be documented and exercised by ledraw.

### Attribute and result typing

A typed attribute declaration associates an attribute name with a concrete
codec and cardinality/options. It provides typed writes, entity reads, and
reusable query/pull result projections. Optional absence is an option; malformed
values are explicit errors. Defaults are chosen by application code only for
absence or documented legacy compatibility.

A result can be promised as `vector<string>` only when its projection validates
that claim or a checked attribute/query contract establishes it. Attribute names
alone do not establish types. Preserve upstream query, schema, and transaction
validation even when a source declaration supplies additional static evidence.
Do not silently impose new database schema restrictions on existing ledraw
storage; test persisted legacy values before adding write-time restrictions.

### Runtime and compatibility boundaries

Use closed sums, records, and typed codecs. No `Obj.magic`, universal dynamic
values, inferred heterogeneous fallback, or silent numeric truncation. Keep
DataScript query execution, transaction ordering, tempids, lookup refs, and
storage behavior in the existing implementation. Preserve Native/Melange
observable behavior and existing persistence/sync formats.

## Alternatives

- String-only APIs reduce boilerplate but retain parsing and lose source-level
  checking. They remain useful for external input, not the primary literal API.
- Application-local wrappers leave each client maintaining a codec and an AST
  builder. Put reusable behavior in a package instead.
- An all-purpose dynamic map would weaken the design contract and is rejected.
- A new compiler plugin framework is unnecessary unless existing macro and type
  facilities demonstrably cannot support the required behavior.

## Implementation and acceptance

For each bounded phase, write positive, negative, boundary, and evaluation-order
regressions first; observe the expected RED failures; implement minimally; run
GREEN checks; refactor and rerun the relevant checks.

1. Establish reusable explicit literal expansion with a non-DataScript example.
   Test nested mixed forms, empty containers, symbols/keywords, embedded values,
   unsupported values, lexical bindings, single evaluation, and order.
2. Add the DataScript library facade and checked EDN conversion. Test real
   schema/transaction/query behavior, lookup refs, invalid inputs, integer
   boundaries, and parameterized query reuse on Native and Melange.
3. Add typed attribute and result/projection APIs. Test absent versus malformed
   values, wrong writes/results, multi-column results, and persisted ledraw data.
4. Migrate ledraw's schema construction, repeated ID queries, transaction
   construction, and shared EDN conversion to the reusable APIs. Remove obsolete
   AST construction and result decoder helpers where replaced. Preserve unrelated
   working-tree changes and application defaults that are intentional.
5. Run LG full tests, the affected DataScript tests, ledraw storage/sync tests,
   and a Melange integration build/runtime check. Verify representation and
   query reuse without claiming unmeasured performance gains. Record exact
   commands, results, changed repositories, and any remaining limits here.

The goal is complete only after implementation and ledraw integration have
passed acceptance, not merely after publishing this ADR or exposing wrappers.

## Execution record

- Generic `lg.literal/build` is implemented as a library macro. Independent
  closed-tree, nested literal, empty-container, namespace, lexical binding,
  single-evaluation/order, and rejection tests passed.
- A missing generic host capability was exposed by codec constants. Imported
  OCaml constants now retain their checked static types as values; Native and
  Melange compiler regressions passed.
- The optional `datascript-ocaml-lg` package implements checked external EDN,
  typed codecs/attributes/projections, query reuse, pull, and direct typed entity
  transactions. OCaml and LG Native tests passed; the same LG fixture compiled
  and executed with Melange.
- ledraw now uses literal schema data, reusable parameterized ID queries,
  literal snapshot/retraction construction, and the library EDN boundary.
  Its integer-overflow regression was observed RED before migration and GREEN
  afterward. The full Native application suite passed (301 tests).
- DataScript's full suite passed after the allocation-free read helper
  refactor, including the Native/Melange source fixtures and static rejection
  tests. ledraw's final storage/sync selection passed (72 tests).
- The web integration exposed a generic record normalization gap: emitted
  OCaml application names were not resolved across namespaces during recursive
  inference. Recovery is confined to recursive constraint normalization and
  requires a unique matching record identity; both
  successful resolution and ambiguous-name rejection have regression coverage.
  An obsolete dynamic-boundary inventory was also corrected (removed files
  only; no new dynamic boundary).
- General record unification and ordinary callback inference retain their
  existing behavior. The compiler fix is confined to normalizing recursive
  constraints; no toolbar parameter annotations are required.
- The web application's empty global app reference now states its concrete
  type. Native desktop and mobile link configurations include the optional
  facade; build configuration checks pass.
  Full platform packaging/device launches are outside this acceptance run.
- ledraw's full web build and LG's full compiler/runtime suite passed with
  the final compiler and application sources. The final isolated storage/sync
  run passed all 72 tests, including the serialization timing threshold. A prior
  run concurrent with full compiler tests exceeded that threshold; no threshold
  was changed. Existing unrelated worktree changes remain intact.
- Acceptance is complete across LG, DataScript OCaml, and ledraw. All three
  repositories pass `git diff --check`.

### Reproduction commands

All commands use the `5.5.0` opam switch. Run Dune commands sequentially.
Run timing-sensitive application tests after compiler builds have finished.
Paths below are relative to each named repository.

| Repository | Command | Coverage |
| --- | --- | --- |
| LG | `opam exec --switch=5.5.0 -- dune runtest` | Full compiler/runtime suite |
| DataScript OCaml | `opam exec --switch=5.5.0 -- dune runtest` | Existing engine suite, typed API, Native/Melange execution, static rejections |
| ledraw | `bash scripts/test-lg.sh` | Full Native suite, 301 tests |
| ledraw | `bash scripts/test-lg.sh test 'ledraw[.](store\|sync)-test'` | Final storage/sync selection, 72 tests |
| ledraw | `env LUI_ROOT="$PWD/_build/lui-source" opam exec --switch=5.5.0 -- dune build --build-dir /tmp/ledraw-typed-api-build @web` | Full Melange application and worker build |
| ledraw | `bash scripts/test-build-config.sh` | Platform dependency/link configuration |

The standalone integer-boundary test is also available through ledraw's
`dune runtest test` using an isolated build directory. Its failure before the
migration confirmed that the former converter silently truncated `Int64`.

### Remaining limits

The initial implementation required explicit result projections. The amendment
below adds inference from typed attributes and explicit codec evidence; it does
not add a general Datalog schema/type inference engine. `d/entity` is the concise cardinality-one
path; cardinality-many writes use the typed `Attribute.entries` API. Nested or
non-scalar pull/query results remain available through the upstream closed API.
Native platform dependency lists are updated, but device launches and full
mobile/desktop packaging were not part of this verification. No measured
runtime speedup is claimed.

## Amendment: inferred queries and direct EDN literals (2026-09-07)

`d/q` accepts a quoted or unquoted query vector directly, followed by `db` and
inputs. `d/defquery` is optional and prepares a reusable query once. Both share
the same macro elaboration and inferred projection behavior.

A symbolic attribute binding in a datom pattern supplies its static codec.
Keyword EDN can retain its original syntax through an explicit
`{:attributes {:item/id item-id}}` association. An explicit `:types` map supplies
codec evidence for otherwise unknown result variables. Attribute associations
validate their keyword names. No mutable global schema registry, runtime
reflection, or universal value type is introduced.

Relation results infer a scalar row for one find variable and a flat tuple for
multiple variables. All occurrences of a variable must have compatible codec
types, including explicit hints. Codecs are evaluated once during preparation;
actual query results still pass through checked decoders. Entity, attribute,
and transaction positions provide their known static types. Predicate clauses
are retained, and function outputs require explicit evidence. Aggregates,
non-relation find shapes, logical/rule clauses, and nested pull outputs retain
the explicit projection API.

ledraw's reusable ID/history/cursor queries now use `d/defquery` and typed
attributes, removing manual column indices and projection construction.
Validation for this amendment covers both target runtimes, static rejections,
cross-namespace attributes, parameterized reuse, direct keyword queries,
malformed DB values, and single evaluation. DataScript's full `dune runtest`
suite passed, including Native/Melange runtime fixtures and both-target static
rejections. ledraw's full `@web` build and all 72 isolated storage/sync tests
passed after migration. No LG compiler or query-engine algorithm changes were
needed for this amendment.

## Amendment: open attributes without caller type annotations (2026-09-07)

Ordinary `d/q` and `d/defquery` relation queries no longer require `:attributes`,
`:types`, or static schema declarations. Query result positions with no narrower
static evidence retain the existing closed `Datascript.value` type. This is an
explicit library result contract, not a compiler-wide heterogeneous fallback.
Entity/attribute/transaction positions and optional typed declarations retain
their established projections. The application may add properties at runtime,
store heterogeneous values, and reuse one query across differently shaped DBs.
The result type never depends on the first row or a runtime schema lookup.

Scalar query inputs accept ordinary LG strings, integers, floats, booleans,
and keywords. A statically dispatched protocol also accepts the upstream closed
value and query-argument types, keeping existing explicit inputs compatible.
Query, DB, and input expressions evaluate once in source order.

Native regression tests exposed an existing engine defect: the optimized
single-binding relation path substituted input variables in datom patterns and
comparison predicates but omitted equality predicates. The same failure occurred
with the old explicit codec API. Equality now uses the same bound-term
substitution, preserving the engine's execution path and equality semantics.
No static schema carrier, global registry, unsafe cast, or universal dynamic
representation is introduced.

Application integration also exposed a generic compiler gap: qualified OCaml
constructors were looked up only as OCaml values during parameter inference.
Constructor payload signatures now participate in the same lookup, preventing
protocol witnesses from being passed into a concrete string parameter. Native
and Melange regression coverage combines a generic protocol call with a concrete
host-constructor helper, without parameter annotations at the application site.

Sequence argument planning also now recognizes a protocol-constrained element
by its concrete payload type. The existing lazy sequence adapter attaches the
static protocol witness per element. This allows unannotated query helpers to be
used through `map` over ordinary string collections, while mismatched integer
collections remain rejected. It neither changes collection storage types nor
adds dynamic conversions.

Already-carried protocol witnesses keep their protocol identity: the new
sequence conversion applies only to plain payload elements. A distinct protocol
with the same method shape is not interchangeable.

The storage/sync integration found a second inference defect: branch results did
not propagate their shared type equations back to parameters, and merging an
unresolved exception branch with a type variable lost the known type variable.
This caused an inline string result to receive an opaque printer and changed
entity keys to `shape/id:<value>`. Branch inference now retains those equations
and preserves the known branch type, with a regression that executes both paths
without annotations. No application-side bindings or type hints hide this defect.

Nested calls now also use the known return signatures of their argument
expressions when specializing parameter types. This preserves concrete string
evidence through inline calls inside loops, as well as through let-bound values.
The regression includes constructing and looking up an entity-key map in a loop.

Concrete printable parameters also constrain both generated printer functions
to their known payload type. Otherwise an unused string display function could
retain a weak type variable in a rebindable function root, preventing module
export. The regression checks standalone module compilation as well as runtime
output; runtime-only local-module execution does not enforce this export rule.

### Verification of open-attribute queries

- LG `opam exec --switch=5.5.0 -- dune runtest` passed, including branch
  inference, module-export, qualified-constructor, and protocol-sequence
  regressions. The conditional-parameter rejection now occurs during LG
  inference rather than only during downstream OCaml checking.
- ledraw's isolated storage/sync selection passed all 72 tests in 25.614 seconds.
  Its query input now passes `doc-id` directly; no application type annotations
  or sync-code workarounds were added.
- ledraw's complete Melange `@web` build passed using the final installed
  compiler and the existing LUI source build.
- DataScript `opam exec --switch=5.5.0 -- dune runtest` passed with the final
  compiler, including Native/Melange query execution and static rejection
  fixtures. A separate fresh build directory also verified that the LG fixture
  rules explicitly build their required DataScript interfaces before compilation.
