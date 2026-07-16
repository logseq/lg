# Progressing Notes

This file records concrete problems encountered while porting Persistent Sorted
Set and DataScript to LG. Add an entry when a problem is first observed, then
update the same entry with its root cause, fix, and verification evidence.

## 2026-07-16: Dune formatting alias has no project configuration

- Status: Recorded; no source-format mutation performed
- Symptom: `dune fmt` reports that OCamlformat is disabled because the repository
  has no `.ocamlformat` and outside-project formatting is not enabled.
- Impact: Formatting did not run; it did not alter build or runtime behavior.
- Direction: Add an explicit project-wide OCamlformat configuration in a
  separate style change before applying bulk formatting. Avoid silently using
  machine defaults that could rewrite unrelated user code.

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
