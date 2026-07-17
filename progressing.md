# Progressing Notes

This file records concrete problems encountered while porting Persistent Sorted
Set and DataScript to LG. Add an entry when a problem is first observed, then
update the same entry with its root cause, fix, and verification evidence.

## Porting principles

- Prefer static evidence. Record types, protocol constraints, sequence element
  types, and other concrete capabilities must compose instead of being erased
  to `dynamic` by a temporary inference fallback.
- Use `dynamic` only for genuinely open runtime boundaries or explicit source
  annotations. Once a concrete type becomes known, adapt at that boundary and
  keep the rest of the program statically typed.
- Never use `Obj.magic` to bridge a missing type relationship. Extend the type
  evidence or the boundary adapter instead.

## 2026-07-16: Dune formatting alias has no project configuration

- Status: Recorded; no source-format mutation performed
- Symptom: `dune fmt` reports that OCamlformat is disabled because the repository
  has no `.ocamlformat` and outside-project formatting is not enabled.
- Impact: Formatting did not run; it did not alter build or runtime behavior.
- Direction: Add an explicit project-wide OCamlformat configuration in a
  separate style change before applying bulk formatting. Avoid silently using
  machine defaults that could rewrite unrelated user code.

## 2026-07-16: Dynamic and nullable boundaries disappear into ordinary IR

- Status: Fixed
- Symptom: After elaboration, dynamic packing, dynamic unpacking, and nullable
  sequence normalization are indistinguishable from ordinary runtime calls and
  pattern matches. Later compiler phases cannot validate or preserve boundary
  intent, and LG type metadata can drift from the generated conversion.
- Root cause: `Semantic_ir.t` has only generic `Apply` and `Match` nodes for
  these operations. Conversion helpers immediately lower their intent into
  runtime implementation details.
- Fix: Added typed `PackDynamic`, `UnpackDynamic`, and `NullableToSeq`
  operations carrying source/target type evidence and a conversion subtree.
  Every semantic traversal and the lowering path handles them, and all central
  conversion helpers now emit the operations. The established conversion
  algorithms remain unchanged in this stage to limit semantic risk.
- Verification: A focused IR regression was RED first with an unbound boundary
  constructor, then with plain dynamic packing that emitted only `Apply`. It is
  now green and checks traversal, lowering, central dynamic packing, and
  nullable sequence normalization. The complete compiler suite, `dune build`,
  and full compiler tests pass. Initial full DB verification was incorrectly
  reported complete before the underlying PTY process returned its exit code;
  see the follow-up issue below.

## 2026-07-16: Boundary wrappers changed representation inspection semantics

- Status: Fixed
- Symptom: Full DB typechecking fails in `transact-report`: a normalized
  `Seq.empty` protocol return is placed where `Runtime_dynamic.t` is expected.
- Root cause: Several elaborators intentionally call `Semantic_ir.unlocated` to
  inspect the underlying representation (`Ident`, `Match`, and so on). After
  adding explicit boundary nodes, `unlocated` stopped at `PackDynamic`,
  `UnpackDynamic`, or `NullableToSeq`. That changed adapter selection while the
  actual lowered conversion remained the same, recreating metadata/expression
  drift.
- Fix: Treat boundary operations as typed/location wrappers for representation
  inspection: they remain present in the Semantic IR and its general traversal,
  but `unlocated` recursively returns their conversion subtree.
- Verification: The explicit boundary IR regression and complete compiler suite
  pass. Full DB compilation proceeds through the boundary representation issue
  and now exposes the independent protocol-signature timing problem below.

## 2026-07-16: PTY completion was mistaken for Dune process completion

- Status: Root cause identified; verification procedure corrected
- Symptom: The outer wait helper reported completion together with a nested PTY
  session id. Starting another Dune command then produced `Another Dune instance
  is currently running`; more importantly, the nested command's final failure
  could be missed.
- Root cause: A returned `SESSION_ID` means the nested command still requires
  polling even when the outer wait cell says it completed.
- Fix: Poll the nested PTY session until it returns an explicit `exit_code` and
  check that code before reporting verification success or launching another
  Dune command.

## 2026-07-16: Internal core expansions still rely on forgeable symbol strings

- Status: Fixed
- Symptom: `update-in` had to emit the string `clojure.core/update` to avoid a
  user macro named `update`. This works for that spelling but does not provide a
  general guarantee for other compiler-generated core calls or values.
- Root cause: `Ast.form` represented both parsed user symbols and internal core
  references as `FSymbol string`, so macro expansion and scope lookup could not
  distinguish their origin.
- Direction: Add a closed `FCoreSymbol` AST variant that the reader cannot
  produce. Macro evaluation treats it as opaque, type inference recognizes its
  fixed core semantics, and final expression elaboration dispatches directly to
  a qualified core binding without macro or local-name lookup.
- Verification: The focused structural regression was RED because
  `FCoreSymbol` did not exist. After the first implementation, the existing
  nested update regression exposed that an internal core identifier must also
  work as a higher-order value, not only as a call head. The final regressions
  cover `update-in`, `assoc-in`, `get-in`, transducer expansion, user macro
  shadowing, Native runtime, and Melange compilation; the complete compiler
  suite passes.

## 2026-07-16: Protocol signature changes after an earlier consumer is compiled

- Status: Fixed
- Symptom: Full DB compilation reaches `transact-report`, where an `ISearch`
  witness normalizes the concrete implementation's nullable `datom Seq.t` to
  `Seq.t`, while the already-generated `fsearch` body expects the method result
  as `Runtime_dynamic.t`.
- Root cause: `fsearch` is compiled while the protocol method return is still
  unresolved and therefore emits a dynamic `to_seq` boundary. Later protocol
  implementation evidence refines `-search` to a nullable typed sequence, and
  downstream witness construction uses that newer signature. Definition order
  has produced two incompatible views of the same protocol method.
- Fix: The first elaboration pass now supplies read-only implementation evidence
  to the second pass, while the live protocol registry is rebuilt normally.
  Explicitly annotated method returns retain their static ABI. An unannotated
  method whose later implementations provide concrete evidence is fixed to the
  dynamic ABI instead of changing an already-compiled consumer from dynamic to
  a typed representation; implementations pack at the witness boundary.
- Verification: The focused late-implementation regression runs as `42` on
  Native and compiles on Melange. Full DB compilation passes the `ISearch`
  witness mismatch and reaches `resolve-upserts`.

## 2026-07-16: Declared dependency order is not modeled across expanded forms

- Status: Fixed
- Symptom: A protocol implementation that references a declared helper cannot
  be called by a top-level value before the helper's later source definition;
  the implementation is either unavailable during elaboration or initialized
  after its first use.
- Root cause: Reader preprocessing splits type declarations, method bodies,
  `defn-signature`, `declare`, and recursive definition groups. Source order and
  an end-of-module deferred queue cannot express the resulting dependencies.
- Fix: Added a dependency graph over top-level providers and references,
  including macro-like `def*` forms, protocol methods, declarations,
  constructors, `deftype-methods`, and recursive groups. Tarjan SCCs preserve
  mutual recursion; the stable condensation order schedules dependencies before
  consumers while leaving chunks without declarations unchanged. Locations are
  reordered with their forms.
- Verification: Focused SCC and first-ready-use regressions pass on Native and
  Melange. The mixed nullable protocol regression and complete compiler suite
  remain green.

## 2026-07-16: `if-some` rejects a statically non-nil reference value

- Status: Fixed
- Symptom: Once protocol signatures stabilize, DataScript's explicitly hinted
  `fsearch` has static `Datom` type. `if-some` rejects it as “got map” even
  though Clojure permits `if-some` for any expression and a non-null static
  reference makes the some branch unconditional.
- Root cause: Option lowering supported runtime dynamic values and explicit
  OCaml option storage only; it treated every other static type as invalid.
- Fix: For a static non-option value, compile both branches for validation and
  type merging, bind the value once, and select the some branch directly.
  `if-let` additionally preserves its truthiness guard.
- Verification: Full DB compilation passes the former `fsearch` option-binding
  failure and reaches `resolve-upserts`; the complete compiler suite passes.

## 2026-07-16: Typed vector-of-optional-maps reaches a dynamic reduce boundary

- Status: Fixed
- Symptom: `resolve-upserts` produces
  `vector<option<map<dynamic,dynamic>>>`, but a downstream reduce boundary
  expects `Runtime_dynamic.t`.
- Root cause: The heterogeneous accumulator `[[] {}]` is correctly packed as a
  dynamic vector. However, `Runtime_dynamic.get` did not support integer vector
  indexes and always returned nil. The nil branch then evaluated `(conj nil v)`,
  correctly producing a list, and `Runtime_dynamic.assoc` could not replace a
  vector slot at all. A separate missing parity feature prevented `conj` from
  being passed as a first-class updater in focused source.
- Fix: Added first-class `conj` through the existing dynamic function ABI.
  Dynamic vectors now implement integer `get`, `get-default`, and `assoc` while
  preserving vector representation. `PackDynamic` now also converts typed
  `Runtime_map.t` entries recursively, which lets nullable maps nested inside a
  vector cross a dynamic branch boundary. No reduce-specific or
  DataScript-specific path was added.
- Verification: The focused regression covers resolved, unresolved, mixed, and
  empty inputs; it runs as six true assertions on Native and compiles on
  Melange. A second exact regression covers the dynamic reduce result merged
  with `[entity nil]`; it runs as six true assertions on Native and compiles on
  Melange. The complete compiler suite passes. Full Native DataScript
  compilation passes `resolve-upserts` and reaches `validate-upserts`.

## 2026-07-16: `validate-upserts` emits an unconstrained `db_id` field access

- Status: Fixed
- Symptom: Full Native compilation reaches `db.cljc` lines 1284-1313 and OCaml
  rejects an access to the unbound record field `db_id`.
- Root cause: Mixed `let` bindings whose first value was destructured stopped
  fallback inference at that first binding. Later bindings therefore could not
  constrain outer parameters. In addition, constraints learned from the body
  about a local binding were not propagated back through that binding's value.
- Fix: Mixed bindings now infer every value, then propagate each inferred local
  pattern type back into its corresponding value and outer parameter. This is
  bidirectional `let` inference rather than a record-field special case.
- Verification: The focused nested `reduce-kv` regression was RED first with
  an unbound `db_id` field. It now distinguishes present and missing `:db/id`,
  handles empty/conflicting upserts, runs on Native, and compiles on Melange.
  The complete compiler suite passes. Full Native DataScript compilation no
  longer fails in `validate-upserts` and reaches the independent existential
  `btset` escape below.

## 2026-07-16: Empty dynamic sequence operations throw instead of yielding nil

- Status: Fixed
- Symptom: After mixed-binding inference was repaired, the exact
  `validate-upserts` reduction threw `first of empty sequence` for an empty
  upsert map. Dynamic positional destructuring had the same failure mode for a
  missing item.
- Root cause: Generic dynamic `first` used the throwing sequence primitive, and
  generic dynamic destructuring used the throwing indexed primitive. Clojure
  semantics require both missing positions to yield nil.
- Fix: Dynamic `first` and dynamic positional destructuring use optional runtime
  sequence access and translate `None` to `Runtime_dynamic.nil`. Statically
  non-nil collection paths retain their existing representation-specific code.
- Verification: The same Native/Melange regression covers the empty path and
  the complete compiler suite passes.

## 2026-07-16: Existential `btset` type escapes from `numeric-eid-exists?`

- Status: Fixed
- Symptom: Full Native compilation reaches `db.cljc` lines 992-997 and OCaml
  reports that `$0 btset` cannot be used as `'a btset` because the existential
  type constructor would escape its scope.
- Root cause: Nominal `UnpackDynamic` lowered as an ordinary expression. A
  generic payload therefore escaped its GADT match branch when passed to a
  function or stored in a `let` binding. The reported source location belonged
  to the recursive group; the first bad expression was actually the dynamic
  `btset` consumed in `find-datom`.
- Fix: Semantic lowering now treats generic nominal unpack as a scoped
  elimination. Function application and a following `let` consumer are moved
  inside the unpack branch, so the hidden type never escapes. Concrete generic
  arguments are tracked separately from declaration parameter names, while a
  runtime nominal match deliberately retains an existential wildcard; no cast
  or `Obj.magic` is used.
- Verification: A PSS-backed Native/Melange regression was RED first with the
  same `$0 btset` escape and now consumes a comparator within the existential
  scope. Full Native DataScript compilation passes the escape and the later
  `Datom`/`$0` mismatch after the defrecord inference fix below.

## 2026-07-16: Defrecord accessor inference shadows its own field evidence

- Status: Fixed
- Symptom: The generated `DB` record typed only `avet` as a persistent sorted
  set; `eavt` and `aevt` remained dynamic. Consequently `find-datom` recovered
  an existential set and could not prove its comparator accepts `Datom`.
- Root cause: Defrecord field inference rewrote `(.-eavt db)` to `eavt` inside
  the existing binding `let [eavt (.-eavt db)]`, producing the self-binding
  `let [eavt eavt]`. The same happened for `aevt`, cutting the constraint path.
  Structural generic fields were also discarded before nominal resolution, and
  outer record declarations reused declaration parameter names instead of the
  instantiated generic arguments.
- Fix: Accessor evidence uses distinct internal field variables and is unified
  with direct lexical field evidence after method inference. Structural fields
  are resolved to nominal records before the concrete-field gate. Named records
  now carry separate concrete `type_arguments`, and the unified solver,
  OCaml-type lowering, occurrence check, and variable collection all preserve
  those arguments.
- Verification: A RED Native/Melange regression with three structurally
  inferred generic fields now runs as `-3`. Full DB tracing confirms `eavt`,
  `aevt`, and `avet` are all inferred as the PSS record, and compilation passes
  the `find-datom` `Datom` mismatch.

## 2026-07-16: `DB` search implementation returns `Seq<Datom>` to a dynamic ABI

- Status: Reproduced; next DataScript blocker
- Symptom: Full Native compilation now reaches the `DB` definition at
  `db.cljc` lines 450-543. Its search implementation returns `datom Seq.t`, but
  the protocol witness currently expects `Runtime_dynamic.t Seq.t`.
- Root cause: Pending focused reduction. Record-field inference now exposes the
  concrete sequence element, revealing that protocol return stabilization kept
  a dynamic element ABI for a method whose concrete implementation is typed.
- Direction: Add the exact mixed-implementation Native/Melange regression and
  make the protocol witness boundary perform the explicit element conversion
  selected by the stable method ABI.

## 2026-07-16: Systemic compiler design gaps exposed by the DataScript port

- Status: Architectural diagnosis
- Symptom: Fixing one deep DataScript type error repeatedly reveals another
  nearby boundary error, and locally broad fixes can regress previously valid
  generic or reducer code.
- Root causes:
  - Type relationships are represented mostly as `TUnknown`, `TVar`, and local
    heuristics instead of a single constraint/unification model. Relations such
    as `container<value>`, `value`, and `value -> value -> int` are easily lost
    across function, record, protocol, and deferred-definition boundaries.
  - Type inference, call specialization, capability packing, and OCaml lowering
    each refine types independently. The LG type metadata can therefore disagree
    with the OCaml expression already generated for the same value.
  - Dynamic/static conversion is implemented at many individual call sites.
    Missing one adapter produces raw `option`, record, integer, or sequence
    values at `Runtime_dynamic.t` boundaries; adding a broad adapter can erase
    useful static evidence elsewhere.
  - Core-form expansion is not fully hygienic. Internal forms can be intercepted
    by user macros or namespace aliases, as happened with DataScript's imported
    `update` macro.
  - Deferred definitions solve OCaml ordering with polymorphic holders, but
    dependency order, initialization order, and inferred polymorphism are not
    modeled as one graph.
- Consequence: The current compiler can pass many isolated tests while still
  lacking compositional guarantees. DataScript is acting as a whole-program
  stress test rather than merely a source port.
- Direction: Stop adding broad one-off type heuristics. Consolidate generic
  unification, make boundary coercion explicit and centralized, preserve one
  typed IR as the source of truth through lowering, make core expansion
  hygienic, and model deferred dependencies explicitly. Continue using focused
  regressions to protect behavior while replacing each heuristic cluster.

## 2026-07-16: Unknown callable arguments do not create shared constraints

- Status: Fixed
- Symptom: A generic function taking `container<value>`, `value`, and
  `value -> value -> int` compiles its dynamically discovered value as
  `Runtime_dynamic.t`, even when the concrete container and comparator prove
  that `value` is a nominal `Entry`.
- Root cause: Function-body inference represented each unknown argument to a
  callable parameter as `TUnknown`. Applying the callable therefore recorded no
  equation between a container field, the sibling value parameter, and the
  comparator parameters. Later call-site heuristics could only guess locally.
- Direction: Introduce one unification solver with an occurs check and
  substitution propagation across the full parameter environment. Unknown
  callable arguments receive fresh variables so source-level relationships are
  retained until concrete call-site evidence resolves them.
- Fix: Added a unified solver with occurs checking, structural constraint
  traversal, and substitution propagation across the complete parameter
  environment. Callable arguments with no evidence receive fresh variables;
  named record, sibling value, and comparator shapes retain those variables
  until call-site evidence resolves them.
- Verification: The strengthened regression runs as `49` on Native and compiles
  on Melange. Solver tests cover shared variables, independent variables,
  unknown evidence, and recursive-type rejection. The complete compiler suite,
  `dune build`, and full Native and Melange PSS + DataScript `db.cljc`
  compilation pass.

## 2026-07-16: Conflicting nested-vector evidence was previously erased

- Status: Fixed
- Symptom: After replacing substitution heuristics with unification, the full
  build reports that `vector<vector<int>>` and
  `vector<vector<vector<int>>>` reach the same equality comparison in
  `datascript/util_test.cljc`.
- Root cause: The former substitution table replaced a type variable with
  `TUnknown` whenever later evidence conflicted. That masked a missing relation
  in variadic `(apply mapv vector ...)`: fixed collections were inferred as
  unrelated unknowns and the rest collection only as a sequence of unknowns.
  An overloaded-call heuristic then used an entire `vector<int>` as the output
  leaf type, producing `vector<vector<vector<int>>>` metadata for a runtime
  value whose real shape was `vector<vector<int>>`.
- Fix: Infer one fresh zip element variable, constrain every fixed input as a
  vector of that element, and constrain the variadic rest value as a sequence
  of those vectors. This matches the specialized runtime implementation, gives
  the solver the relationship present in the source, and avoids weakening
  conflict handling.
- Verification: The existing `mapv vector zips multiple collections` regression
  runs as `true:true` on Native. The complete compiler suite and DataScript util
  targets pass through `dune build`.

## 2026-07-16: Dynamic callable conflict incorrectly keeps its first key type

- Status: Fixed
- Symptom: The existing dynamic lookup regression accepts an integer key first
  and a keyword key second, but generated OCaml requires both keys to be `int`.
- Root cause: Unified constraints correctly relate the two keys through the
  callable parameter. At the call site, concrete `int` and `keyword` evidence
  conflicts. Retaining the first solution is wrong because the callable actual
  is explicitly dynamic and therefore accepts heterogeneous runtime keys.
- Fix: When and only when a dynamic callable is present, resolve variables
  participating in a concrete call-site conflict to the dynamic boundary type.
  Static generic conflicts remain visible and are not weakened globally.
- Verification: The focused Native and Melange dynamic-callable regression is
  green, as are the generic nominal-boundary regression and complete compiler
  suite.

## 2026-07-16: Nullable protocol sequence is not normalized at a sequence boundary

- Status: Fixed
- Symptom: Compiling the upstream `db.cljc` for Native reaches `pr-db`, where the
  generated OCaml passes a `datom Seq.t option` returned by `-datoms` directly to
  `Runtime_seq.map`, which expects `datom Seq.t`.
- Scope: The Persistent Sorted Set `slice` path returns an optional lazy
  sequence, while another `IIndexAccess/-datoms` implementation returns a lazy
  sequence directly. The protocol call must preserve Clojure's `nil`-as-empty
  sequence behavior at the `map` boundary.
- Root cause: Protocol return merging correctly chose `Seq.t`, but
  `adapt_witness_method` only normalized sequence returns when the expected
  element was dynamic. A concrete `datom Seq.t option` therefore passed through
  unchanged to a consumer requiring `datom Seq.t`.
- Investigation: A minimal protocol with one nullable-sequence implementation
  and one sequence implementation compiles and runs on both targets. Therefore,
  mixed return shapes alone do not reproduce the bug; another part of the
  `IIndexAccess` receiver or call structure is required.
- Fix: Normalize every expected lazy-sequence protocol return through
  `Collection_capability.to_seq_expr`. This preserves laziness and maps
  `None`/`nil` to `Seq.empty`; equal element types avoid an unnecessary map.
- Verification: The exact deferred nullable-return regression is green on
  Native and Melange. Full Native PSS + DataScript compilation now passes the
  former `pr-db` failure and reaches `db.cljc` lines 992-997.

## 2026-07-16: Generic protocol witness loses its record element annotation

- Status: Fixed
- Symptom: A protocol implemented by generic `optional-source<value>` and
  concrete `SequenceSource` rejects the valid value `entry optional_source`,
  claiming that `sequencesource optional_source` is required.
- Root cause: The protocol witness adapter converts concrete record elements to
  dynamic elements with `Runtime_seq.map`, but its mapper parameter had no OCaml
  type constraint. When multiple record types exposed the same field name,
  OCaml selected the most recently declared record and changed the adapter's
  receiver type.
- Fix: Reuse a `typed_item_pattern` helper to constrain named-record mapper
  parameters in both ordinary dynamic collection packing and protocol witness
  return adaptation.
- Verification: The focused regression compiles and runs as `41:42` on Native
  and compiles on Melange after refactoring and removing diagnostic output.

## 2026-07-16: Deferred protocol implementation initializes after top-level use

- Status: Reproduced; deferred because it is independent of the current DB
  typecheck blocker
- Symptom: A protocol method that references a declared function is stored in a
  deferred holder. Calling the protocol through a top-level value in the same
  module raises `Invalid_argument("option is None")`.
- Root cause: The generated `let ()` assignment that fills the holder is emitted
  after all ordinary top-level forms, including calls that read the holder.
- Planned fix: Preserve dependency safety while placing deferred initializers
  before the first top-level use, or reject unsafe initialization cycles with a
  clear compile error.
- Verification: Pending a dedicated runtime regression after the current
  DataScript `db.cljc` type boundary is green.

## 2026-07-16: Constructor reaches a dynamic return position in `entid`

- Status: Fixed
- Symptom: Full Native compilation now fails at `db.cljc` lines 992-997 with
  “This expression should not be a constructor, the expected type is
  `Runtime_dynamic.t`.”
- Root cause: `entid` merges dynamic branches with a nullable integer branch,
  so the overall result is `Runtime_dynamic.t`. `pack_plain_dynamic_value` has
  no nullable case; coercion therefore leaves `Some` and `None` constructors in
  a position where OCaml requires a dynamic value.
- Fix: `pack_plain_dynamic_value` now matches nullable values once, packs
  `None` as `Runtime_dynamic.nil`, and recursively packs the `Some payload`.
- Verification: The exact nested-cond regression runs as `41:42:true` on Native
  and compiles on Melange. All compiler tests filtered by `nullable` pass. Full
  Native PSS + DataScript compilation now passes `entid-strict` and reaches
  `db.cljc` lines 1129-1150.

## 2026-07-16: Non-final `or` returns an internal option wrapper

- Status: Fixed
- Symptom: A truthy nullable value passed through `or` as `Some payload`, so a
  number printed as `<value>` rather than `42`.
- Root cause: Logical lowering used the nullable storage expression as the
  source return value instead of its truthy payload.
- Fix: Infer non-final nullable `or` operands from their payload type and emit
  `Option.get` after the truthiness guard. Explicit `nil` still participates in
  nullable branch merging for compatibility with existing `if-some` lowering.
- Verification: Focused true/fallback paths, existing logical tests, and
  existing and/or short-circuit tests pass; Native runtime is `42:7`, and the
  focused source compiles on Melange.

## 2026-07-16: `some?` over-constrains an otherwise dynamic parameter to option

- Status: Reproduced; deferred as a separate inference issue
- Symptom: `(defn maybe-number [value] (if (some? value) value nil))` generates
  an `_ option` parameter, so a normal Clojure call with integer `42` fails the
  OCaml typecheck instead of accepting the value.
- Root cause: Predicate inference models `some?` by changing the input's storage
  type to option rather than accepting an arbitrary value and refining only the
  guarded branch.
- Planned fix: Keep the input capability/dynamic type stable and apply an
  occurrence refinement inside the true branch.
- Verification: Pending a dedicated Native and Melange runtime regression.

## 2026-07-16: Dynamic `schema` field is used as an association list

- Status: Fixed
- Symptom: Full Native compilation fails at `db.cljc` lines 1129-1150 because
  `g__325.schema` has type `Runtime_dynamic.t`, while the generated consumer
  requires `(Runtime_dynamic.t * 'a) list`.
- Root cause: `update` compiled an anonymous updater before inspecting the
  target field. The callback therefore inferred `%` from `dissoc` as a static
  Runtime_map parameter. Later changing only its type metadata to dynamic left
  the already-generated static function body unchanged. In DataScript this was
  compounded by `Core_form_expansion.update_in` emitting an unqualified
  `update`: the imported `datascript.inline/update` macro intercepted it and
  bypassed the contextual core update compiler entirely.
- Fix: Internal update-in expansion now emits `clojure.core/update`, including
  nested updates, so namespace shadowing cannot change compiler semantics.
  Core update also compiles the target first and feeds its field type into
  `Function_elaborator.prepare` as the anonymous updater's first parameter
  override. Both keyword-field and dynamic-key update paths use this context;
  dynamic nested values therefore select `Runtime_dynamic.dissoc` in the body.
- Verification: The focused Native test runs as `1:true`, and the same source
  compiles on Melange even when a user macro named `update` is in scope. The
  regression covers one-level update-in, two-level update-in, and direct core
  update with a dynamic target/key. Full Native PSS + DataScript compilation now
  passes `remove-schema` and reaches `db.cljc` line 1172.

## 2026-07-16: Nested updater constraints do not reach captured parameters

- Status: Reproduced; deferred as a separate inference issue
- Symptom: After compiling a dynamic `dissoc` callback correctly, its captured
  outer `key` parameter remains untyped. Calling the outer function with `:b`
  passes a raw string where `Runtime_dynamic.t` is required.
- Root cause: Type inference for nested function bodies does not propagate
  constraints on free variables back to the enclosing function parameters.
- Planned fix: Merge inferred free-variable constraints from nested callbacks
  into the enclosing inference environment without leaking local bindings.
- Verification: Pending a dedicated Native and Melange regression. The current
  update-in test uses an explicit dynamic key, matching DataScript's dynamic
  `v-ident`, so this independent issue does not obscure the DB blocker.

## 2026-07-16: Integer is not packed for a dynamic schema update

- Status: Fixed
- Symptom: Full Native compilation now fails at `db.cljc` line 1172 because an
  `int` expression is passed where `Runtime_dynamic.t` is expected.
- Root cause: `get-e-schema` calls its local `schema` callable with both `e` and
  `db-ident`, but inference represented the callable argument and those sibling
  parameters as unrelated unknowns. At the call site a dynamic schema value
  forced the generated callable wrapper to accept `Runtime_dynamic.t`, while
  the integer `e` remained unboxed.
- Fix: At a normal function call, when an actual argument is dynamic while its
  expected shape is a function, unresolved types in that call signature are
  materialized as dynamic. This uses concrete call-site evidence and keeps
  ordinary local/generic callable inference unchanged, while ensuring sibling
  key arguments are packed for the dynamic callable boundary.
- Verification: The focused source reproducing a dynamic callable with integer
  and keyword keys compiles on Native and Melange. Full Native PSS + DataScript
  compilation now passes line 1172 and reaches `db.cljc` line 1201.

## 2026-07-16: Dynamic callable result metadata disagrees with generated value

- Status: Reproduced; deferred as a separate result-specialization issue
- Symptom: After the callable arguments compile correctly, consuming the result
  with `println`, `str`, or `=` can select a static string path even though the
  generated expression has type `Runtime_dynamic.t`.
- Root cause: Call return metadata is specialized from the static map value
  candidate while the dynamic callable wrapper still returns a dynamic value.
- Planned fix: Keep return metadata and generated wrapper representation in
  lockstep when a callable parameter is supplied dynamically.
- Verification: Pending a dedicated runtime regression. The current key-packing
  regression verifies Native/Melange compilation without consuming the result.

## 2026-07-16: Dynamic callable TVar specialization is too broad

- Status: Fixed
- Symptom: The first full DB verification after the callable-key fix regresses
  to `db.cljc` line 614: a dynamic pattern is generated where OCaml requires an
  integer pattern.
- Root cause: Giving every TVar nested under any dynamic actual parameter
  unconditional dynamic precedence also changes unrelated generic callback
  variables that already have concrete evidence elsewhere in the call.
- Fix: Removed shared-TVar propagation from local callable inference. Dynamic
  materialization now occurs only at a call site where the actual argument is
  dynamic and the corresponding expected argument is a function shape.
- Verification: The existing `update reads dynamic reduce accumulators
  dynamically` regression is green again, while the new dynamic callable/key
  compilation regression remains green on Native and Melange. Full DB now
  progresses beyond line 1172.

## 2026-07-16: Dynamic value reaches a `Datom` update boundary

- Status: Fixed
- Symptom: Full Native compilation fails at `db.cljc` line 1201 because a
  `Runtime_dynamic.t` value is passed where a concrete `Datom` is required.
- Root cause: A generic consumer inferred structurally as
  `{consume: α -> int}` was recognized as nominal `entry-consumer<value>`, but
  `infer_named_record` returned the uninstantiated formal record. The sibling
  value parameter kept α, so their relationship disappeared and a later dynamic
  argument was not unpacked to the concrete nominal type.
- Fix: When structural fields uniquely select a named record, instantiate that
  record from the matched template/actual field types. Shared TVars now survive
  the structural-to-nominal conversion; dynamic actuals are ignored as generic
  type evidence, allowing concrete evidence from the container to drive
  unpacking.
- Verification: The focused generic consumer runs on Native and compiles on
  Melange. The strengthened container/value/comparator regression runs as `49`.
  Full Native and Melange PSS + DataScript `db.cljc` compilation passes line
  1201 and completes successfully.

## 2026-07-16: Named-record inference erases a formal generic with `TUnknown`

- Status: Fixed
- Symptom: The first named-record instantiation fix regressed the existing
  int/string Holder test: the int result was emitted in a string concatenation
  position.
- Root cause: A matched structural field with exact type `TUnknown` was used as
  type evidence, replacing the named record's formal `value` parameter with
  unknown and breaking the return relationship needed for per-call
  polymorphism.
- Fix: Instantiate a uniquely matched named record only from fields whose actual
  type is not exact `TUnknown`. TVars and structured types containing TVars still
  carry relationships; absent evidence preserves the formal parameter.
- Verification: Both `defrecord inferred generic fields preserve value types`
  and `generic calls unpack dynamic nominal arguments` are green.

## 2026-07-16: Generic protocol receivers lose their concrete sequence element

- Status: Fixed
- Symptom: A `btset<Datom>` `Seqable` implementation returns `Seq<Datom>`, but
  the erased function boundary expects `Seq<dynamic>`.
- Root cause: Generic receiver method types were instantiated only through some
  protocol lookup paths. Core collection capability lookup kept the declaration
  type variable, and Seqable adapters and stored dynamic values selected
  different element representations.
- Fix: Centralized receiver method instantiation and applied it to core protocol
  lookup. Seqable adapters now own the element conversion, and erased storage
  reuses the adapted sequence.
- Verification: The focused PSS generic nominal test passes on Native and
  Melange. Full Native `db.cljc` compilation passes the DB `diff-sorted` call.

## 2026-07-16: Dynamic sequence reducers require explicit item adaptation

- Status: Fixed
- Symptom: `reduce` passes `Runtime_dynamic.t` sequence elements directly to a
  reducer whose item parameter is a concrete record or protocol constraint.
- Root cause: Unary sequence functions had a dynamic/static adapter, but reducer
  functions did not have the corresponding two-argument boundary adapter.
- Fix: Added reducer item adaptation while preserving the accumulator type.
  Dynamic protocol witness methods also unpack invocation results using their
  refined common return type.
- Verification: Focused record reducer and dynamic protocol return regressions
  pass. Full Native `db.cljc` advances through `check-value-tempids`.

## 2026-07-16: Deferred return type applies `TxReport` as a generic constructor

- Status: Fixed
- Symptom: Full Native compilation now reaches `retry-with-tempid` and emits
  `datom txreport option`, although `txreport` has no type parameters.
- Root cause: Forward-declared record bindings retained the stale generic shape
  inferred before the nominal type declaration was available.
- Fix: Refresh forward-declared bindings when their named record declaration is
  emitted and preserve the refreshed type through deferred binding emission.
- Verification: Full Native compilation no longer emits `datom txreport
  option` and advances through the later DB declarations.

## 2026-07-17: Declared functions form an oversized recursive group

- Status: Fixed
- Symptom: `validate-indexed` has a polymorphic deferred holder, but its
  implementation becomes monomorphic to `DB`; after removing that blocker,
  `assoc-lru` similarly exposes a holder that is more general than its dynamic
  map implementation.
- Root cause: A `declare` followed by split `deftype` methods grouped every
  intervening function into one OCaml `let rec`. `deftype-methods` was also
  misclassified as providing the record type, creating false dependency cycles.
  Separately, explicit nominal hints were resolved only after parameter
  inference, named-record field constraints were ignored, map `assoc` erased its
  key/value shape, and all accumulated substitutions were discarded after one
  unrelated field conflict.
- Fix: Let the dependency graph order individual definitions, classify
  `deftype-methods` only as protocol method providers, resolve explicit record
  hints before inference, propagate map key/value constraints through nominal
  fields, preserve inferred nominal type arguments, and accumulate independent
  substitutions incrementally.
- Verification: Focused declaration ordering, deferred protocol receiver,
  deferred initializer, type solver, and upstream `lru.cljc` regressions pass.
  Full Native PSS + DataScript compilation advances past both LRU and
  `validate-indexed`.

## 2026-07-17: `restore-db` emits an unbound record type variable

- Status: Fixed
- Symptom: Full Native compilation now fails at `db.cljc` lines 769-781 with
  `The type variable 'value is unbound in this type declaration`.
- Root cause: The generated `restore-db` row declared fresh parameters for its
  structural fields, but named-record parameterization changed only the record's
  declaration parameter names. Its concrete type arguments and fields retained
  the original `value`, producing `avet : 'value btset` without binding
  `'value` in the row declaration.
- Fix: Row parameterization now uses one variable map recursively across a
  named record's declaration parameters, concrete arguments, and fields.
- Verification: A focused generic named-record row regression reproduced the
  same OCaml error, then passed on Native and Melange. Full Native PSS +
  DataScript compilation passes `restore-db` and reaches `transact-report`.

## 2026-07-17: Erased sequence storage reuses protocol callback elements

- Status: Fixed
- Symptom: A sequence of nominal values adapted for a generic protocol callback
  is passed to `Runtime_dynamic.seq` as `(witness, value)` tuples instead of
  dynamic values.
- Root cause: The callback adapter and erased dynamic storage reused the same
  mapped sequence even though they have different element ABIs.
- Fix: Keep the witness-bearing callback adapter separate. Build erased storage
  from the original sequence and pack each underlying element to dynamic.
- Verification: `generic protocol witness flows through sequence callbacks`
  runs successfully again.

## 2026-07-17: Forward generic record patterns bind declaration variables

- Status: Fixed
- Symptom: A forward-declared generic record used as a function parameter
  emitted a pattern annotation containing declaration variables such as
  `'value`, which were not bound by the surrounding function type.
- Root cause: Function pattern constraints replaced top-level TVars with `_`
  but did not recurse through named-record fields and concrete type arguments.
- Fix: Pattern constraint normalization now recursively replaces unresolved
  variables throughout named records while retaining their nominal identity.
- Verification: The focused forward-declared record regression passes, the
  complete compiler suite passes, and no cast or `Obj.magic` is used.

## 2026-07-17: Occurrence type hints escape conditional branches

- Status: Fixed
- Symptom: Inferring `^Wrapped value` inside one `if` branch changed the whole
  function parameter to `Wrapped`, rejecting the valid fallback call with the
  base value. Treating every unknown hinted value as dynamic fixed that case
  but broke unconditional hints and their call ABI.
- Root cause: Type hints supplied useful nominal evidence to fixed-point
  parameter inference, but the inference loop did not distinguish an
  unconditional hint from branch-local occurrence evidence. Branch hint state
  also had to be reset on every stabilization pass.
- Fix: Unconditional hints still resolve nominal parameters. Conditional
  inference records only symbols refined by occurrence hints and restores
  their pre-branch types afterward; all unrelated `if` inference remains
  unchanged.
- Verification: The occurrence-hint, unconditional-record-hint, dynamic-set,
  and nullable-compare regressions pass. The complete compiler suite and
  `dune build` pass.

## 2026-07-17: `transact-report` crosses a dynamic sequence ABI

- Status: Fixed
- Symptom: Full Native compilation now reaches `db.cljc` lines 1222-1235 and
  passes `datom Seq.t` where the selected boundary requires
  `Runtime_dynamic.t Seq.t`.
- Root cause: Incremental compilation's second pass seeded only
  `defn-signature` declarations, not ordinary `declare` forms. Even after the
  final declared bindings were seeded, protocol evidence still retained the
  first pass's unresolved method implementations, so their common return type
  remained dynamic instead of the concrete `Datom Seq`.
- Fix: Share declaration collection between full and incremental typechecking,
  seed ordinary declarations with their final first-pass bindings, and replace
  matching protocol evidence as second-pass implementations are recompiled.
  Dynamic record lookup now also specializes shared generic fields from their
  concrete field evidence instead of erasing them. Quoted symbols are excluded
  from recursive dependency analysis.
- Verification: Focused declaration, protocol return, record specialization,
  and quoted-symbol regressions pass. The complete compiler suite and
  `dune build` pass. Full Native PSS + DataScript compilation no longer fails
  in `transact-report` and advances to the independent `nth-datom` binding
  issue below. No per-Datom dynamic packing or `Obj.magic` was introduced.

## 2026-07-17: `nth-datom` is emitted after an earlier call site

- Status: Fixed
- Symptom: Full Native compilation reaches `db.cljc` lines 83-115, where the
  generated code references `datascript_db_nth_datom__arity_2_0` before that
  value is bound.
- Root cause: Calls to a declared multi-arity function lower directly to an
  arity target such as `nth_datom__arity_2_0`, but deferred expression detection
  compared identifiers only with the binding's dispatcher name. The method was
  therefore emitted before the target existed.
- Fix: Treat every overload target on a forward-declared binding as declared
  evidence alongside its dispatcher.
- Verification: The focused overload-target regression fails before the fix
  and passes afterward. The full parser dependency chain no longer reports the
  unbound `nth-datom` target and advances to the sequence issue below.

## 2026-07-17: `entid` loses sequence evidence

- Status: Fixed
- Symptom: The full `datascript.parser` dependency build reaches `db.cljc`
  lines 951-987 and passes `Runtime_dynamic.t` where OCaml expects
  `unit -> 'a Seq.node`.
- Root cause: A protocol-constrained receiver retained a dynamic method-return
  ABI, while the call node was labeled with the later concrete common return
  without unpacking the witness result.
- Fix: Read the actual return type from the receiver's witness method and adapt
  that result to the concrete expected return at the call boundary.
- Verification: A focused dynamic-witness-to-concrete-sequence regression
  passes, and the full Native parser dependency chain advances beyond `entid`.

## 2026-07-17: Witness construction rewrites a deferred dynamic ABI

- Status: Fixed
- Symptom: After `entid` was repaired, calls to `pr-db` passed a witness whose
  `-datoms` method returned `Seq.t`, while the previously emitted `pr-db`
  parameter required `Runtime_dynamic.t`.
- Root cause: Witness construction replaced unresolved method returns with the
  registry's later common return. This changed the caller ABI without changing
  the already-emitted function. Leaving the type as `TUnknown` was also wrong,
  because deferred emission had already materialized that unknown as dynamic.
- Fix: Preserve the binding's witness ABI and materialize only its unresolved
  positions as dynamic before adapting concrete implementations.
- Verification: The focused function-before-implementations `apply pr`
  regression fails with `Seq.empty` versus dynamic before the fix and passes on
  Native and Melange afterward. Full Native compilation advances beyond
  `pr-db` to `check-value-tempids`.

## 2026-07-17: A dynamic key erases generic record and protocol evidence

- Status: Fixed
- Symptom: `choose-box` inferred its `catalog<int>` parameter as
  `dynamic<optional-protocol<CatalogInfo;any>>`. Dynamic packing then projected
  only the concrete `count` field and lost generic `left` and `right` fields,
  causing `Runtime_dynamic.as_int` to fail at runtime.
- Root cause: General `get` inference installed a provisional dynamic type
  before contextual inference ran. Later record evidence stayed trapped inside
  that wrapper. In addition, the field type `box<value>` remained an unresolved
  type application, so structural `box<int>` evidence could not specialize it.
- Fix: Track explicit dynamic parameters separately from inference fallbacks.
  Contextual record evidence can replace only the inferred wrapper while
  retaining protocol constraints. Resolve named record applications through the
  type registry before matching shared fields, and discard identity type
  substitutions such as `value -> value` to avoid recursive substitution.
- Verification: The regression now asserts that `choose-box` retains a
  protocol-constrained `catalog<int>` parameter and runs correctly on Native
  and Melange. Explicit dynamic `get`, dynamic protocol witnesses, the complete
  compiler suite, and `dune build` all pass.

## 2026-07-17: `check-value-tempids` passes bool to a dynamic boundary

- Status: Open; next DataScript blocker
- Symptom: Full Native parser dependency compilation reaches `db.cljc` lines
  1435-1448 and reports `bool` where `Runtime_dynamic.t` is expected.
- Current evidence: The failing form reduces transient tempid maps using a
  callback that branches on `datom-added`. It is independent of the repaired
  protocol sequence witness ABI and still needs a focused reduction.

## 2026-07-17: `util/raise` is not expanded in part of `db.cljc`

- Status: Open
- Symptom: The parser dependency build also reports `unknown function
  util/raise` for `validate-schema` at `db.cljc` lines 655-700.
- Current evidence: `raise` is an upstream macro from `datascript.util`, and the
  parser behavior tests do not exercise this path. The compiler must preserve
  its cross-namespace macro alias instead of falling back to a dynamic call.

## 2026-07-17: Prefer static evidence over `dynamic`

- Status: Project invariant
- Rule: Preserve or infer concrete types whenever the source provides evidence.
  Use `dynamic` only for genuinely open values or explicit dynamic annotations;
  never use it to hide namespace resolution, macro expansion, or type inference
  defects.
- Practical gate: Focused regressions must exercise Native and Melange, and
  compiler changes must not route a statically expandable call through a
  dynamic runtime wrapper.

## 2026-07-17: DataScript inline `assoc` and `update`

- Status: Fixed
- Symptom: The exact upstream `datascript.inline` module was missing. After it
  was restored one-to-one, `clojure.lang.RT/assoc` was unknown and referred
  `update` calls degraded to its dynamic runtime fallback. `allocate-eid` then
  failed with incompatible arguments or a dynamic callback ABI.
- Root cause: `defn` attribute maps were accepted but `:inline` metadata was
  discarded. Namespace aliases therefore preserved only the runtime binding,
  not the compile-time inline definition.
- Fix: Keep inline definitions separately from ordinary macros, propagate them
  through namespace aliases and `:refer`, and expand calls in both the defining
  and consuming namespaces. Compile an inline function's own runtime fallback
  with inherited inline definitions temporarily disabled, then restore them;
  this prevents definition bootstrapping from changing the fallback ABI.
  Treat `clojure.lang.RT/assoc` as a compile-time compatibility spelling of core
  `assoc`; it emits no Java import or `clojure.lang` runtime dependency.
- Verification: The exact upstream `inline.clj` bytes are mirrored as
  `test/datascript/upstream/inline.cljc`. Focused tests prove same-namespace
  expansion, cross-namespace `:refer`, nested `cond->`/`->` updates, passing
  `update` as an updater, Native execution, Melange generation, and absence of
  calls to the dynamic update wrapper. The complete compiler suite passes.

## 2026-07-17: `restore-db` row type arity diverges

- Status: Fixed
- Symptom: Full Native PSS + DataScript parser compilation now reaches Storage
  restore and emits `datascript_db_restore_db_row0` with 41 declared type
  parameters but applies it with 29 arguments.
- Root cause: Typechecking always ran exactly two passes. The first pass inferred
  a 29-parameter forward declaration without final protocol evidence. The
  second pass compiled earlier `restore-db` call sites against that stale ABI,
  then inferred the definition's final 41-parameter row type.
- Fix: Iterate typechecking until forward-declaration ABI shapes stabilize.
  Compare the actual boundary evidence—source type shape, row parameters,
  overload rows and targets, and return parameter index—rather than nominal
  identities inside the protocol registry. Stable modules still take two
  passes; `db.cljc` takes one additional pass for 29 -> 41 -> 41.
- Verification: A focused test simulates that exact transition and proves the
  third pass retains the 41-parameter ABI. The real Native parser dependency
  chain no longer reports 41/29 and advances to `transact-tx-data`.

## 2026-07-17: `transact-tx-data` loses `datom btset` evidence

- Status: Open; next DataScript blocker
- Symptom: Full Native parser dependency compilation reaches `db.cljc` lines
  1700-1707 and passes an existential `$0 btset` where `datom btset` is
  required.
- Current evidence: This occurs after `assoc-auto-tempids` feeds
  `transact-tx-data-impl`. The storage/PSS value type is still nominally a
  `btset`, but its element parameter is hidden. The fix must preserve the
  concrete `datom` argument; widening either side to `dynamic` is not allowed.
