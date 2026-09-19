# Hint Reduction Inference Plan

Goal: reduce the number of local type hints needed by the chat OCaml-to-LG
migration without weakening LG's static typing contract.

Status: implemented for the migration slices that blocked chat live-sync,
outliner, mobile graph, graph runtime, RPC, graph-store seed compilation,
entity-sync runtime behavior, and stdlib native-state rebuilding. The formal
chat `@core/runtest` gate passed after the return-payload fix. The broader
compiler suite still has unrelated remaining failures from the active inference
refactor, so this document should be treated as the current implementation plan
plus guardrails, not as a claim that every compiler regression is resolved.

Related design contract: `docs/design.md`.

## Problem

Recent chat migration commits repeatedly hit the same compiler gaps:

- callback parameters needed hints even when calls, branches, or collection
  operations already carried enough context;
- result and nullable branches lost useful payload context;
- record updates through `assoc`, `update`, `swap!`, and return-param methods
  projected only the changed row and then forgot the original nominal record;
- structural records and runtime maps sometimes chose storage too early;
- equality could push an overly narrow structural expected type into the next
  operand.

The desired compiler behavior is static and local:

- infer from the nearest trustworthy context;
- preserve nominal records at call sites when a helper returns an updated row;
- keep rigid `TVar` values rigid;
- convert open or structural results only at the explicit boundary that needs
  them;
- never fall back to `Runtime_dynamic.t` to make migration code compile.

## Implemented Slice

The current implementation keeps the refactor deliberately inside existing
compiler modules instead of introducing a new abstraction layer while the chat
port is still moving.

### Type Inference

- Freshen inferred function types before use so a migration helper call does not
  leak one call site's constraints into another.
- Seed callback and branch result constraints from surrounding result, option,
  `try`, and collection contexts.
- Keep simple generic `let` initializer calls from polluting later uses with a
  concrete return instance.
- Infer record-field value context for migration-shaped `assoc`, `update`, and
  `swap!` forms.
- Merge compatible structural and non-nominal named record rows without erasing
  concrete field types.
- Keep weak anonymous record observations fresh instead of reusing a global
  nominal record merely because the field layout matches. Layout-compatible rows
  such as `{uuid}` or `{uuid,title}` can arise in unrelated domains, and sharing
  their generated OCaml record type leaks constraints across callbacks,
  variants, and node-route projections.
- Merge two fresh compiler-generated anonymous records at branch joins only when
  their rows are bidirectionally compatible. This keeps allocation isolated for
  unrelated domains while allowing local `if`/`try` paths, such as live-sync
  cursor updates, to agree on a single static result type without extra hints.
- Reuse concrete structural record materialization within one function body.
  This is intentionally narrower than global layout reuse: it only prevents a
  single branch join from emitting `t4`/`t5` for the same concrete row while
  leaving unrelated functions and owners isolated.
- Infer the element row of `group-by` buckets from the consumer body when a
  let-bound grouped value is immediately destructured and traversed. This keeps
  chat's `duplicated-labels` path static without requiring a call-site hint.
- Treat `apply list` as producing a list-compatible seqable when the expected
  argument type is a seqable constraint, not only when it is exactly `list`.
  This lets imported helpers such as `store/append-rows` push their inferred
  row/seqable parameter context back into `(apply list rows)`.
- Preserve the actual `Ok` payload when adapting result-return-param calls.
  When the storage result carries a seqable adapter or an anonymous update row,
  the adapter must use the matched success payload as the updated value. It may
  use the original return parameter only to fill unchanged record fields.
- Preserve `result<param; error>` return types for deferred forward-declared
  functions whose success payload returns one parameter. Deferred binding
  freshening must not collapse these functions to `param -> param`, because the
  body still constructs `Ok`.
- Rebuild generalized binding schemes after SCC substitutions refine a
  predeclared function type. The binding's public type and scheme must move
  together; otherwise a later call can instantiate a stale scheme and turn a
  refined `seq<Item>` result back into `seq<inference-variable>`.
- Preserve guarded protocol evidence when body-derived parameter expectations
  refine a fallback capability such as `seqable<any>`. A function guarded with
  `satisfies?` must keep accepting either a protocol receiver or the statically
  checked fallback storage, instead of letting the fallback overwrite the
  optional protocol wrapper.
- Keep source-owned sorted collection protocols statically typed. `ISorted`
  method sidecars preserve the relationship between entries, keys, and storage,
  while `persistent-tree-set` uses typed helpers for comparator, equality,
  count, and empty operations so the internal map value type `bool` does not
  leak into the public set element type.
- Treat `ICollection/-conj` as a self-returning protocol method. Collection
  implementations that preserve receiver storage, including sorted sets, must
  return the original collection type instead of inferring a fresh type from an
  internal helper payload.
- Infer block and conditional optional results through `do`, `when`, and
  all-optional `or` forms. A fallback resolver such as
  `(or (when known? value) (resolve key))` should infer `resolve` as returning
  the same optional payload without local hints.
- Keep keyword-condition evidence out of generic map storage. A keyword lookup
  used only as a condition now constrains map-like and `contains?`-guarded
  receivers through the field payload instead of materializing
  `truthy<option<T>>` as the map value type. This fixes the previous
  `map key evidence preserves generic map values` nontermination/type-error
  path and keeps schema-style generic maps callable without local hints.
- Treat `and` as guard operands followed by one returned value. In ordinary
  value inference, operands before the final operand receive truthiness
  evidence, while the final operand is inferred as the returned value. If an
  outer condition observes the whole `and`, that outer condition may require
  truthiness for the final value. This keeps schema predicates from turning
  returned map fields into truthiness-witness storage.
- Infer `for`/`doseq` generator `:let` bindings sequentially inside the
  generator environment. A later `:let` name shadows the collection element for
  the body only; it must not rewrite the sequence element type used by the loop
  cursor or by earlier binding expressions.
- Infer simple `for`, single-collection `map`/`mapv`, and `group-by` return
  relationships when the surrounding source already exposes the callback body,
  grouping key, and collection element type. This lets parser/rule-shaped code
  preserve bucket value types without a local hint on every intermediate vector.
- Infer multi-collection `map`/`mapv` callback results when the callback and
  collection element types are statically visible. Variadic constructors such
  as `vector` therefore produce `vector<vector<T>>` for zipped inputs instead
  of needing a result hint at the equality or call site.
- Specialize multi-collection `mapcat` directly through typed zipped sequences
  and `flat_map`. Inline callbacks that return vectors now receive concrete
  element parameters before their result vector is checked as seqable, so
  2/3/variadic arities do not need return hints.
- Infer uncontextualized `filter`/`filterv`/`remove` predicate parameters from
  the collection element type inside helper function bodies. This preserves the
  static relationship between a predicate parameter and a later `items`
  argument, even when the helper's return context is only discovered through
  `first`/`if-some`.
- Use concrete collection element evidence from non-callback arguments before
  compiling callback arguments in ordinary named calls, not only OCaml calls.
  When a callback parameter is still open or structural and the same call has a
  single record-shaped collection element candidate, the callback receives that
  record context without requiring a local parameter hint.

### Elaboration

- Preserve result-match payload context through branch elaboration.
- Preserve structural record projections through `Structural_map` helpers so
  generated storage and generated access agree.
- Contextualize record update field values with the known field type.
- Keep record receivers on the static record-update path when the keyword is a
  declared field.
- Avoid propagating structural record rows as expected types through equality
  operands.
- Infer expected function types for higher-order parameters when the parameter
  itself is called inside the function body and the argument type can be read
  from a local `match`/`tuple`/`Some` branch context.
- Only unwrap truthy nullable bindings when the unwrapped value is actually used
  by the branch body. This avoids generating warning-26 bindings such as
  `let known_ = Option.get known_ in` for conditions that are only truthiness
  guards.
- Keep all-optional `or` lowering to a single option layer by merging operand
  payloads before wrapping the result. This prevents generated equality code
  from seeing `option<option<T>>` when the source expression returns
  `option<T>`.
- Preserve homogeneous record maps as HAMT values across branch adaptation.
  Structural projection is needed for OCaml records, but homogeneous map rows
  already share one runtime representation; adapting them by rebuilding
  `M.of_list` loses update-storage identity and obscures `assoc`/`merge`/`dissoc`
  behavior.
- Share function-local anonymous record materialization between nested records
  and return records. Nested update helpers such as `assoc-in` should not create
  one generated record type for the function result and another for the same row
  inside map values.
- Adapt structural updater results before writing them back to known record
  fields. A named updater may return its own anonymous row, but `update` must
  project that result to the field row before constructing the enclosing record.
- Retain enough nominal context for local record bindings and loop cursors when
  a structural row exactly matches one declared record layout. This is scoped to
  the individual binding/cursor boundary so field access such as `branch.vars`
  does not become ambiguous between same-field records, while unrelated
  structural rows remain structural.
- Clear an outer expected result type while compiling the input collections and
  synthetic callback body for multi-collection `map`/`mapv`. A result context
  such as `vector<vector<int>>` constrains the map result, not each input vector
  literal or each scalar argument passed to a variadic `vector` callback.
- Clear the same outer expected result type for `mapcat` input collections.
  The flattening result constrains the `mapcat` output; it must not be pushed
  into the source collections being zipped.
- Expand visible OCaml host records inside the adaptation planner before row
  projection. A callback that only needs structural fields may consume a host
  record directly when the declared host record supplies those fields, avoiding
  an anonymous record reconstruction that would lose OCaml field-label scope.
- Keep callback wrappers when function types differ even if each individual
  argument/result adaptation is identity. The wrapper carries the incoming host
  parameter type and lets inline LG lambdas resolve host record fields without
  hints.

### Return-Param Calls

Return-param methods are common in the migration because they model operations
that return an updated version of one argument. The implemented rule is:

- let the helper return the minimal static storage row it actually updates;
- reconstruct the expected or original nominal record at the call site by taking
  updated fields from the row and unchanged fields from the original argument;
- do not re-adapt an already reconstructed value as if it were still the row;
- avoid refining a non-runtime record result into a runtime-map-shaped row.

This lets migration code omit local hints around helpers that update one field
but still need the full record immediately afterward.

## Guardrails

- No `Obj.magic`.
- No `Runtime_dynamic.t` fallback for unresolved migration code.
- No `__lg_dynamic`, `to_dynamic`, `of_dynamic`, or dynamic-narrow escape hatch.
- Rigid declared type variables stay rigid, including in collection and callback
  contextualization.
- Public sidecar and OCaml host-boundary contracts remain authoritative.
- The compiler may infer local context, but it must not invent a weaker public
  API type.

## Regression Coverage

The implemented tests cover these chat-shaped cases:

- independent nominal payloads sampled from different signals;
- nested record constraints through refs and accessors;
- later record updates preserving prior fields;
- `swap!` record updates preserving the record target;
- nullable projection equality preserving both option layers;
- nullable callback results surviving `when-some` binding;
- recursive unit returns seeded from host calls;
- destructured tuple lookup preserving independent positions;
- callback `assoc` preserving nominal record collection fields;
- computed record fields receiving field context;
- `if-some` plus `assoc` reusing a structural row without generating map access;
- membership key inference from literal sets;
- returned record parameters preserving nominal call-site fields.
- named `filterv` predicates preserving the collection element type when shared
  `:state` fields exist in other records;
- weak anonymous record observations from helpers such as `same-status?` not
  polluting later callbacks or same-layout records, including both one-field and
  two-field rows;
- fresh anonymous records with compatible rows merging across branch results
  without falling back to a global layout cache;
- anonymous record branch materialization reusing one generated OCaml record
  type across `if`, `if-some`, and `try` paths;
- reference accessors preserving declared ref payloads when another record has
  the same field names.
- payload constructors preserving their namespace and result type identity when
  another namespace exposes a same-named constructor;
- constructor definition/reference/rename/completion language-service spans
  surviving generated OCaml result constraints.
- `group-by` bucket consumers preserving fields discovered in the function body;
- imported function calls propagating seqable expectations into
  `(apply list rows)`;
- truthy nullable guards avoiding unused unwrap bindings.
- result return-parameter wrappers preserving recursive list payloads instead
  of rebuilding success from the initial empty accumulator.
- forward-declared result helpers keeping success payloads inferred from typed
  accumulators, including Datascript payloads.
- forward-declared collection callbacks preserving their refined result
  payloads through generalized SCC bindings, so `(map touch values)` keeps
  `seq<Item>` after `touch` is compiled.
- optional resolver fallbacks inferring from `or`/`when` without annotating the
  resolver callback.
- homogeneous map operations staying on HAMT storage after branch/vector
  adaptation.
- `assoc-in` nested map updates preserving one anonymous record identity for
  repeated nested rows.
- `update` inferring record fields from updater functions without annotating
  the updater result.
- parser rule map helpers allocating one anonymous return record for the
  function result instead of duplicating the same row.
- `for` generator `:let` shadowing preserving the original collection element
  type through grouped parser/rule code.
- `doseq` loop cursors preserving seqable iteration and nominal projection
  context without annotating the branch element.
- multi-collection `mapv vector` and `apply map vector` preserving variadic
  collection adapters under an outer equality/result context.
- `mapcat` matching multiple collection arities without forcing a heterogeneous
  vector of callback parameter variables.
- host callback vectors preserving OCaml record identity through `atom`,
  `swap!`, `filterv`, `mapv`, helper predicates, selection helpers, and mutable
  host fields without callback parameter hints.
- hash-map `update` preserving ClojureScript missing-key semantics for named
  updaters that test the old value with `nil?`, while keeping extra updater
  arguments statically adapted instead of requiring call-site hints.

Each migration-facing positive case should compile for Native and Melange. Cases
that guard runtime behavior also execute the Native output.

## Verified Commands

Focused checks run for this slice:

```sh
dune exec test/compiler_tests.exe -- --filter "if-some assoc branches reuse structural row"
dune exec test/compiler_tests.exe -- --filter "return-param record update preserves call-site nominal fields"
dune exec test/compiler_tests.exe -- --filter "single field anonymous records do not cross pollute callbacks"
dune exec test/compiler_tests.exe -- --filter "filterv named predicate preserves shared state record"
dune exec test/compiler_tests.exe -- --filter "shared single field variant state"
dune exec test/compiler_tests.exe -- --filter "fresh anonymous records merge across branches"
dune exec test/compiler_tests.exe -- --filter "fresh anonymous record branches emit shared record type"
dune exec test/compiler_tests.exe -- --filter "reference accessor keeps declared payload"
dune exec test/compiler_tests.exe -- --filter "payload constructor namespace identity"
dune exec test/compiler_tests.exe -- --filter "language service constructor capabilities"
dune exec test/compiler_tests.exe -- --filter "group-by function body infers bucket value fields without call site"
dune exec test/compiler_tests.exe -- --filter "truthy nullable condition does not unwrap unused payload"
dune exec test/compiler_tests.exe -- --filter "apply list constrains function parameter as seqable"
dune exec test/compiler_tests.exe -- --filter "apply list uses imported seqable parameter context"
dune exec test/compiler_tests.exe -- --filter "result return-param preserves recursive list payload"
dune exec test/compiler_tests.exe -- --filter "return-param record update preserves call-site nominal fields"
dune exec test/compiler_tests.exe -- --filter "result preserves returned record parameter"
dune exec test/compiler_tests.exe -- --filter "recursive result helpers infer from typed accumulator"
dune exec test/compiler_tests.exe -- --filter "forward result payload infers from typed accumulator"
dune exec test/compiler_tests.exe -- --filter "forward Datascript result payload infers from typed accumulator"
dune exec test/compiler_tests.exe -- --filter "forward-declared functions work as collection callbacks"
dune exec test/compiler_tests.exe -- --filter "optional protocol values can flow to seqable else branches"
dune exec test/compiler_tests.exe -- --filter "sorted range queries match ClojureScript"
dune exec test/compiler_tests.exe -- --filter "sorted range queries reject mismatched keys"
dune exec test/compiler_tests.exe -- --filter "sorted-set preserves ClojureScript order and persistence"
dune exec test/compiler_tests.exe -- --filter "or infers optional resolver return"
dune exec test/compiler_tests.exe -- --filter "nullable equality evaluates each operand once"
dune exec test/compiler_tests.exe -- --filter "homogeneous map operations stay on HAMT storage"
dune exec test/compiler_tests.exe -- --filter "homogeneous maps"
dune exec test/compiler_tests.exe -- --filter "homogeneous map shape ignores field order"
dune exec test/compiler_tests.exe -- --filter "anonymous maps reuse equal shapes"
dune exec test/compiler_tests.exe -- --filter "assoc-in updates nested maps"
dune exec test/compiler_tests.exe -- --filter "assoc-in preserves named records with references"
dune exec test/compiler_tests.exe -- --filter "update infers record fields from updater functions"
dune exec test/compiler_tests.exe -- --filter "update works as a nested map updater"
dune exec test/compiler_tests.exe -- --filter "parser rule map allocates anonymous return record"
dune exec test/compiler_tests.exe -- --filter "for let shadowing replaces nominal collection type"
dune exec test/compiler_tests.exe -- --filter "doseq infers seqable parameters" --filter "doseq uses upstream seqable iteration" --filter "rule vars projection preserves nominal argument type" --filter "for let shadowing replaces nominal collection type"
dune exec test/compiler_tests.exe -- --filter "mapv vector zips multiple collections"
dune exec test/compiler_tests.exe -- --filter "map vector apply preserves variadic collection adapters"
dune exec test/compiler_tests.exe -- --filter "mapcat matches multiple collection arities"
dune exec test/compiler_tests.exe -- --filter "mapcat specializes identity collection return"
dune exec test/compiler_tests.exe -- --filter "mapcat preserves empty nil and transducer results"
dune exec test/compiler_tests.exe -- --filter "core mapcat preserves record callback result type"
dune exec test/compiler_tests.exe -- --filter "external record atom vector preserves nominal identity"
dune exec test/compiler_tests.exe -- --filter "inferred OCaml calls implicitly apply trailing unit after optional labels"
dune exec test/compiler_tests.exe -- --filter "host callback atom vector preserves nominal identity"
dune exec test/compiler_tests.exe -- --filter "hash map update preserves present and missing value semantics"
dune exec test/compiler_tests.exe -- --filter "keyword conditions do not infer optional storage"
dune exec test/compiler_tests.exe -- --filter "map key evidence preserves generic map values"
dune exec test/compiler_tests.exe -- --filter "nil predicates"
dune exec test/compiler_tests.exe -- --filter "optional protocol values can flow to seqable else branches"
dune exec test/compiler_tests.exe -- --filter "homogeneous maps"
dune exec test/compiler_tests.exe -- --filter "assoc-in updates nested maps"
dune exec test/compiler_tests.exe -- --filter "protocol sequence returns support ordinary core calls"
dune exec test/compiler_tests.exe -- --filter "protocol methods merge concrete static sequence returns"
dune exec test/compiler_tests.exe -- --filter "recursive protocol sequence returns remain concrete"
dune exec test/compiler_tests.exe -- --filter "protocol constraint patterns annotate only the stored value"
dune exec test/compiler_tests.exe -- --filter "map normalizes mixed nullable protocol sequence returns"
dune build stdlib/lg_stdlib_native.state
dune build bin/lg_cli.exe
CHAT_LG_BINARY=/Users/tiensonqin/Codes/projects/lg/_build/default/bin/lg_cli.exe \
bb /tmp/check-chat-seed.clj live_sync_test
CHAT_LG_BINARY=/Users/tiensonqin/Codes/projects/lg/_build/default/bin/lg_cli.exe \
bb /tmp/check-chat-seed.clj outliner_test
CHAT_LG_BINARY=/Users/tiensonqin/Codes/projects/lg/_build/default/bin/lg_cli.exe \
bb /tmp/check-chat-seed.clj rpc_test
CHAT_LG_BINARY=/Users/tiensonqin/Codes/projects/lg/_build/default/bin/lg_cli.exe \
bb /tmp/check-chat-seed.clj e2e_seed_data_test
CHAT_LG_BINARY=/Users/tiensonqin/Codes/projects/lg/_build/default/bin/lg_cli.exe \
bb /tmp/check-chat-seed.clj graph_store_test
dune build @core/runtest
```

All focused compiler checks above passed for the current anonymous-record,
seqable, generator-shadowing, multi-map, and result-return-param slices. The
formal chat `@core/runtest` gate passed with `705/705` tests after the earlier
return-payload fix.

## Remaining Migration Work

The other active chat migration thread has moved mobile graph, graph runtime,
mobile session, native crypto linkage, LG entry cleanup, entity-sync behavior,
and persisted seed coverage forward. The latest known chat compiler blockers
from this plan are fixed locally and covered by focused tests plus
`graph_store_test` and `@core/runtest`.

Remaining work is now compiler-suite level:

- rerun the official chat coverage gate from a clean state and keep the green
  `@core/runtest` result reproducible from committed compiler changes;
- triage the remaining canonical `compiler_tests.exe` failures from the broad
  inference refactor by design area, not by patching individual expected
  outputs. The keyword-condition/map-key conflict is fixed locally: both
  `keyword conditions do not infer optional storage` and
  `map key evidence preserves generic map values` pass as focused checks. The
  broad suite still contains independent protocol-witness, dynamic-boundary,
  multi-arity rendering, collection-family, destructuring, and expected-error
  wording failures. Treat those as separate design audits before changing code;
- keep reducing newly discovered hints only when the surrounding static context
  is already present and trustworthy;
- add a small representative chat fixture that can run next to the synthetic
  compiler tests so future inference changes do not depend only on ad-hoc seed
  scripts.

If the fullgate exposes a new blocker, diagnose it as a new narrow inference
hole first. Do not widen anonymous-record sharing or introduce dynamic escape
hatches to make the gate pass.
