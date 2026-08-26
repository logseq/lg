# ADR 009: Provide a Complete UI Hot Reload Experience

- Status: Accepted; Phases 0 and 1 complete
- Date: 2026-08-26
- Decision owners: LG compiler and LUI maintainers
- Related: `docs/design.md`, `docs/architecture.md`, and
  `docs/agent-guide/008-static-repl-sessions.md`

## Context

Fast feedback is a core requirement for productive UI development. LG
applications currently need to rebuild or restart after a view change. That
interrupts the edit-observe loop, discards application state, and forces the
developer to navigate back to the UI being edited.

The goal is broader than replacing one renderer. Saving a relevant source or
asset file should update the running application with no manual command while
preserving as much useful context as safely possible. Compilation errors should
remain visible without replacing the last working UI.

Existing systems demonstrate useful parts of the solution:

- [Vercel Native](https://github.com/vercel-labs/native) keeps the application
  model and message loop alive, interprets changed view input in development,
  validates a candidate tree, and adopts it only after a successful load.
- [GPUI](https://github.com/zed-industries/zed/tree/main/crates/gpui) retains
  state in typed entities and renders from that state. It does not provide a
  first-party mechanism for replacing compiled Rust application code in place;
  external style and layout reload is the common safe boundary.
- [Hotcaml](https://github.com/let-def/hotcaml) demonstrates native OCaml code
  replacement, including the additional indirection required for existing
  callers to observe new definitions.

LG must provide a polished experience without weakening its static type
contract. Hot reload cannot introduce a universal source value, `Obj.magic`,
hidden casts, or implicit conversion through `Runtime_dynamic.t`.

## Product outcome

During normal UI work, the developer should be able to:

1. edit a view, style, asset, event handler, or same-signature UI helper;
2. save the file once;
3. see the running UI update automatically;
4. remain on the same screen with the same model, focus, selection, scroll
   position, window geometry, and retained widget state where identities still
   match; and
5. keep using the last known-good UI when the edit is invalid.

The development client must explain what happened. It reports whether a change
was hot-applied, required state migration, caused an automatic restart, or was
rejected. A developer should not need to infer reload status from a frozen or
partially updated window.

The initial performance objectives for a warm local session are:

- acknowledge a relevant file change within 100 ms after the editor write
  burst settles;
- show a compile diagnostic or commit a small single-file UI change within
  500 ms at p95; and
- render the first committed frame without a visible blank window.

These are acceptance targets, not claims about the Phase 0 prototype. The
benchmark fixture and measurement point must be recorded before declaring a
phase complete.

## Decision drivers

The design must:

- optimize the edit-save-observe loop rather than expose a low-level reload
  command;
- preserve the live model and UI context whenever the static contract permits;
- cover view structure, properties, styles, assets, event handlers, and
  same-signature UI logic;
- automatically fall back to migration or restart when an edit cannot be
  applied safely in place;
- reject incomplete candidates before they become visible;
- retain the last known-good UI after compilation or validation failure;
- prevent stale compilation work from overwriting a newer edit;
- preserve retained widget identity through normal LUI reconciliation;
- provide precise editor and in-application diagnostics; and
- add no watcher, compiler service, interpreter, or reload transport to release
  builds.

## Decision

LG will provide a development-only **UI Hot Reload session**. It combines
incremental compilation, typed replacement slots, retained-tree
reconciliation, resource invalidation, state preservation, diagnostics, and
automatic fallback into one workflow.

"Complete hot reload" describes the developer experience, not permission to
replace arbitrary code unsafely. Every change is classified by its static
impact and handled by the least disruptive valid strategy.

### Reload capability levels

| Change | Default action | State behavior |
| --- | --- | --- |
| Style, theme token, image, font, shader, or other UI asset | Invalidate the affected resource and redraw | Preserve model and retained UI state |
| View structure, properties, or child composition with the same root contract | Compile, validate, reconcile, and atomically hot-apply | Preserve model; retain matching widget state |
| Event handler, view-local helper, formatter, or update logic with the same exact static signature | Replace the generated typed slot and redraw | Preserve model; new invocations use new code |
| Subscription definition with compatible identity and message type | Diff subscriptions after commit | Preserve compatible subscriptions; restart changed resources |
| Model or message representation with an accepted migration | Soft restart through the typed migration boundary | Preserve migrated model and restorable UI context |
| Model/message type without migration, package graph, FFI, runtime, compiler option, or host ABI | Rebuild and automatically restart the development target | Restore navigation context when possible; otherwise start cleanly |
| Invalid or incomplete candidate | Reject and show diagnostics | Keep the last known-good application unchanged |

This matrix is part of the contract. Unsupported edits must not appear to
succeed while leaving old closures or partially updated widgets alive.

### Stable, statically typed application roots

Each reloadable application generates one closed record containing its exact
reloadable roots. Conceptually:

```ocaml
type roots = {
  view : model -> view;
  update : message -> model -> model * effect list;
  subscriptions : model -> subscription list;
}

type generation = {
  id : int;
  roots : roots;
  source_hash : string;
}
```

The actual fields may differ, but they remain concrete and application
specific. There is no heterogeneous global function registry. Reloading a root
with a different type fingerprint is rejected from in-place publication and
routed to migration or restart.

Generated call sites that must observe replacement dispatch through the stable
root record or a concrete typed slot. Ordinary non-reloadable code keeps direct
static calls. This indirection exists only in development builds and only at
declared reload boundaries.

Reloadable candidate evaluation may define roots and pure construction data;
it must not run arbitrary top-level application effects before commit. Effects,
subscriptions, resource acquisition, and lifecycle changes are activated by
the coordinator after publication through explicit typed operations.

### One transactional reload pipeline

Every input source uses the same generation pipeline:

```text
watch import and asset closure
          |
          v
coalesce edits and assign generation
          |
          v
parse -> infer/check -> lower -> build candidate
          |
          v
validate root types, resources, and tree invariants
          |
          v
classify: hot apply | migrate | restart | reject
          |
          v
commit on the UI thread -> reconcile -> one visible frame
```

The watcher hashes the complete transitive import and asset closure. Atomic
editor renames and multi-write save bursts are coalesced. An unchanged content
hash is a no-op. Each request and artifact carries a monotonically increasing
generation; a result older than the newest requested generation is discarded.

Compilation happens away from the UI thread. Publication and retained-tree
mutation happen on the UI thread. The committed root record, source/dependency
hash, resource generation, and `PatchBatch` become visible as one transaction.
No candidate may publish a partial patch sequence.

If post-commit activation fails, the coordinator restores the previous roots
and resource generation when rollback is safe. If external effects make
rollback unsafe, it automatically restarts the target and reports the reason.

### Preserve the developer's place

LUI reconciles a candidate tree against the committed tree by widget type,
explicit key, and structural position. Matching identities retain local state
and resources. Incompatible identities are replaced through normal patch
operations.

The development runtime also captures restorable UI context before commit or
restart:

- focused widget identity and text selection;
- scroll offsets;
- window size, position, and active window;
- navigation route and selected tabs where the application exposes typed
  restoration hooks; and
- inspector selection and debug overlay state.

Restoration is best effort and identity-based. The runtime must not attach old
state to a new widget merely because it occupies the same memory address.
Password fields, secure input, and application-declared sensitive state are
never included in restart snapshots.

### State migration is explicit and typed

Same-fingerprint model types keep the live value directly. A changed model
type can cross a soft restart only through a developer-supplied typed migration
whose old and new types are known at compilation time. A versioned closed
snapshot format may be generated for records and variants whose fields have
supported codecs.

Migration failure does not fall back to a dynamic map. It produces a diagnostic
and performs a clean automatic restart after developer confirmation when data
loss would be surprising. In unattended test sessions, the configured policy
decides whether to restart or fail the run.

Queued messages and effects are generation tagged. Messages whose concrete
type fingerprint does not match the committed generation are drained before
migration or discarded with a diagnostic. They are never cast into the new
message type.

### Styles and assets are first-class reload inputs

Style and asset changes should not require recompiling unrelated LG code. The
dependency graph records which views consume each style token, image, font, or
shader. Reload invalidates only affected caches and schedules a redraw.

A replacement resource is decoded and validated before the old resource is
released. Decode or GPU upload failure keeps the previous resource active and
reports the source path and underlying error. Resource disposal occurs after
the committed frame can no longer reference the old generation.

### Diagnostics are part of hot reload

The development session exposes one closed diagnostic event protocol for the
terminal, editor, and in-application overlay. Events include:

- change detected and compilation started;
- reload committed with generation, elapsed time, and affected roots;
- reload rejected with structured source diagnostics;
- migration required or failed;
- automatic restart started and completed; and
- target disconnected or protocol version mismatched.

Compile errors keep the last known-good UI interactive. The overlay is
non-modal, can be collapsed, and disappears after the next successful
generation. Editor diagnostics retain file, range, phase, and related-location
information. Repeated events for the same diagnostic are deduplicated.

The session exposes reload timing and invalidation traces to developer tools so
slow feedback can be diagnosed rather than accepted as unexplained latency.

The initial transport is local-only and uses a per-session capability token.
Both peers negotiate an explicit protocol and compiler version before accepting
source, artifacts, diagnostics, or control messages. Remote development must
use an authenticated encrypted tunnel; the target never exposes an
unauthenticated code-loading endpoint.

### Development and release paths remain separate

The watcher, incremental compiler service, bytecode or candidate loader,
diagnostic transport, reload coordinator, snapshot hooks, and overlay exist
only in development builds. Release builds use direct statically compiled
roots and do not ship a reload socket or code loader.

The development and release paths must share the typed semantic IR and
rendering fixtures. Hot-reload interpretation or bytecode execution is not a
second UI language. Differential tests compare the same view through a clean
build and a hot-reload generation.

The initial Native development target is a persistent OCaml bytecode process
that hosts both LUI and the application runtime. The stable root types are
loaded when that process starts. Candidate LG code is compiled and evaluated
by a single serialized compiler-libs/Toploop coordinator in the same address
space, so the resulting typed closures can be placed directly into candidate
root slots. The coordinator hands only a validated generation to the UI thread,
which performs the atomic commit and redraw.

This reuses the persistent bytecode foundation established by ADR 008 without
attempting to transfer function values between processes. The application must
remain responsive between commits; implementation benchmarks will determine
whether parsing and other immutable front-end work should run in a helper
process. Native packaged releases remain ahead-of-time compiled. Native
dynamic-library replacement is not the default reload mechanism.

## Implementation status

The implementation now has three layers of evidence.

The isolated LG prototype proves typed same-address replacement semantics in a
persistent process. The production LUI runtime now provides:

- a schema-generated internal `Root` node that is not part of the authoring
  element surface;
- exactly-one-child, top-level-only, and no-properties validation for that
  root on LG, SwiftUI, Flutter, and Web;
- a layout-transparent host implementation (`display: contents` on Web and
  direct child projection on SwiftUI and Flutter);
- one atomic retained batch for replacing the root child;
- application-specific typed view slots with source and contract hashes;
- monotonically increasing requested and committed generations;
- stale, unchanged, rejected, applied, and restart-required outcomes;
- a closed runtime checkpoint that restores node ownership, properties,
  children, handlers, dynamic segments, identifiers, and pending operations
  after candidate failure;
- reloadable reducer applications whose typed model survives view replacement
  and whose new event handlers replace the old generation;
- repeated-reload tests that verify old nodes and event handlers do not grow;
- transactional descendant reconciliation by node kind and structural position,
  preserving compatible native node identities while replacing only
  incompatible subtrees;
- a closed `RemoveProp` patch across LG, SwiftUI, Flutter, and Web so a
  disappearing property does not force node replacement;
- stable component state scopes, separate from generation-owned handler and
  subscription scopes, including nested component paths; and
- retained-handler rebinding through typed node aliases, so compatible native
  controls use the new generation's closures;
- stable typed roots for ordinary and recursive same-signature functions, so
  existing callers observe a replacement without a dynamic registry;
- transactional root observers that let LUI validate a tentative candidate and
  restore the last-known-good function when reconciliation rejects it;
- an attached Native bytecode session that can bootstrap from an
  application-specific saved state and open its linked implementation module;
- a content-hashed watch session with monotonic generations, write-burst
  settling, changed-path compilation, structured rejection, and last-known-good
  behavior;
- an LG-to-bytecode-to-LUI end-to-end test that creates a live reducer app,
  reloads a saved View automatically, preserves its model, rejects invalid
  source, and commits only the newest write in a burst;
- explicit string reload keys whose identities survive sibling insertion and
  reordering; and
- active component-path tracking that retains compatible local state, prunes
  removed state scopes, and restores the registry after candidate failure.

Phase 1 is the complete View loop, not the complete ADR. The watcher accepts the
transitive source closure from the development target and hashes every supplied
path; automatic workspace closure discovery belongs in the developer tooling
integration. Swift and Flutter retained-backend suites cover keyed moves,
retained focus objects, draft/IME state, and atomic root replacement. A Web
browser acceptance fixture for focus, selection, scroll, and window context is
still part of Phase 4 tooling. The resource dependency graph, typed migrations,
automatic target restart, and in-application diagnostic overlay remain in later
phases.

## Prototype evidence

`prototype/hot_reload` is the Phase 0 typed replacement prototype. Its view
source accepts literal text and one typed `{count}` binding. That syntax is a
test fixture, not proposed LG or LUI syntax.

The prototype proves that:

- an existing caller observes a successfully replaced renderer;
- the live model survives replacement;
- a failed candidate retains the last known-good renderer and source;
- failed and unchanged candidates do not advance the generation;
- an unknown model binding produces a structured diagnostic; and
- an oversized candidate is rejected before publication.

Run the evidence with:

```sh
dune runtest prototype/hot_reload
```

Phase 0 by itself does not prove those behaviors. Phase 1 now proves real LG
compilation, attached bytecode evaluation, LUI reconciliation, file watching,
same-signature View and helper replacement, and retained model/widget state.
Asset replacement, migration, and restart fallback remain later-phase work.

## Delivery phases

### Phase 0: Replacement semantics

- Complete: typed renderer replacement, generation tracking, transactional
  publication, and last-known-good behavior in the isolated prototype.

### Phase 1: End-to-end View loop

- Complete: generate and validate a real internal LUI root across all retained
  backends.
- Complete: publish a typed application-specific view slot into a reloadable
  reducer application while preserving its model and replacing handlers.
- Complete: replace the root child through one atomic `PatchBatch`, roll back
  invalid candidates, and reject stale or incompatible generations before view
  construction.
- Complete: reconcile matching descendants by type and structural position,
  remove stale properties, replace only incompatible subtrees, rebind event
  handlers, and preserve nested component-local state scopes.
- Complete: hash and watch the View dependency closure supplied by the
  development target, settle editor write bursts, and compile changed paths in
  the attached persistent bytecode process.
- Complete: route tentative typed root replacement through LUI validation and
  roll the root back when the candidate is rejected.
- Complete: reconcile explicit keys and prune state scopes for component paths
  that are no longer mounted.
- Complete: prove the real save-to-visible-View loop, invalid-source LKG, and
  newest-generation-only behavior in one attached end-to-end test.
- Complete: retain host objects through atomic root replacement and keyed moves;
  the Swift and Flutter suites cover focus-bearing controls and native draft/IME
  state. Cross-host browser context restoration remains a tooling acceptance
  item in Phase 4.

### Phase 2: UI code and resources

- Add typed slots for event handlers, UI helpers, compatible update logic, and
  subscriptions.
- Reload styles, themes, images, fonts, and shaders with dependency-based cache
  invalidation.
- Add cancellation, stale-generation tests, rollback tests, and long-running
  resource-leak tests.

### Phase 3: Migration and seamless fallback

- Add typed versioned model migration and safe snapshot codecs.
- Add automatic rebuild/restart with navigation-context restoration.
- Explain every fallback decision in the client and overlay.
- Exercise rapid edits, syntax errors, type changes, target disconnects, and
  compiler crashes in end-to-end tests.

### Phase 4: Tooling quality

- Integrate reload status, timing, component invalidation, and retained-state
  inspection with editor and UI developer tools.
- Maintain p50/p95 latency dashboards and representative application fixtures.
- Gate releases on clean-build/hot-reload rendering parity.

Stages that touch the compiler, runtime, LUI types, or host interop require an
implementation review against `docs/design.md`.

## Acceptance scenarios

A production-ready UI hot reload session must demonstrate all of the following:

- editing nested view structure updates the visible window without losing the
  application model;
- keyed children preserve their local state across insertion and reordering;
- focus, text selection, scroll, and window geometry survive compatible edits;
- a changed event handler is used on the next interaction, including by a
  retained widget;
- style and image edits update only their dependent views and caches;
- syntax, type, asset decode, and candidate validation failures keep the last
  good UI usable;
- rapid consecutive saves cannot publish generations out of order;
- a compatible model migration preserves state;
- an incompatible type or ABI change triggers an explained automatic restart;
- repeated reloads do not grow retained widgets, callbacks, subscriptions, GPU
  resources, or compiler artifacts without bound; and
- a clean build and a hot-reloaded build render equivalent trees for the same
  fixture.

## Alternatives considered

### Limit the feature to View replacement

Rejected as the final product boundary. It proves the core transaction but
leaves styles, assets, handlers, compatible logic, diagnostics, and fallback
outside the workflow. The Phase 0 prototype remains useful as the smallest
mechanism test.

### Reload arbitrary native OCaml modules or dynamic libraries

Rejected as the default mechanism. Native code replacement widens the unsafe
surface to ABI compatibility, closures, module initialization, callbacks, and
host resources. It also does not solve widget identity, diagnostics, migration,
or stale-generation handling.

### Restart after every edit and serialize the model

Rejected as the primary experience. It loses non-serializable resources and
makes ordinary view edits pay migration and startup costs. Automatic restart is
the transparent fallback for changes outside a safe hot-apply boundary.

### Use a universal dynamic application representation

Rejected. It conflicts with LG's static type contract and moves failures from
candidate compilation into the running application. Known roots, views,
properties, messages, migrations, diagnostic events, and patches use concrete
records and closed variants.

### Reload only external styles or layout data

Rejected as incomplete. It is a fast path within the chosen design but cannot
represent conditional structure, child composition, typed bindings, or event
behavior.

## Consequences

The system is more than a file watcher: it introduces development-session
coordination, generated typed slots, dependency tracking, retained-state
restoration, resource generations, diagnostics, and automatic fallback. That
complexity is justified only if measured end-to-end feedback and state
preservation improve materially.

The static boundary remains explicit. Many UI edits apply without losing state,
while incompatible model, dependency, runtime, and ABI changes rebuild and
restart automatically. The developer receives one coherent workflow even
though the implementation uses several safe strategies.

The last known-good generation remains the central reliability invariant. A
reload is successful only when the candidate is completely validated, committed
on the UI thread, and able to produce a visible frame.
