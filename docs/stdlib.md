# Source standard library

LG should implement Clojure namespaces as LG source whenever their behavior can
be expressed with the language's static types. The compiler owns syntax,
elaboration, and the smallest host-runtime primitives needed by source code; it
does not own public library functions merely because they are widely used.

This boundary is important for migrating Logseq. A source namespace can be
ported, reviewed against upstream, compiled once, and then consumed by Logseq
code through ordinary `:require`, `:as`, and `:refer` forms. Adding or changing
such a namespace must not require rebuilding compiler dispatch code.

## Upstream and compatibility tracking

[`stdlib/upstream.edn`](../stdlib/upstream.edn) pins the ClojureScript commit
used by each port and records the status of individual definitions. Statuses
have the following meanings:

- `:ported`: the LG source follows the upstream source algorithm directly.
- `:static-adaptation`: control flow is preserved, but a small typed helper or
  signature is needed to express an upstream dynamic relationship.
- `:host-primitive`: the definition must remain at a documented runtime or
  compiler boundary.
- `:deferred`: the definition depends on language or library support that has
  not been ported yet; the manifest records the reason.

The first port is `clojure.set`, based on ClojureScript
`src/main/cljs/clojure/set.cljs` at commit
`5c6ef531604662afbb33dc1b553d7602634d9656`. Its size-based binary algorithms
and branch order follow upstream. The variadic definitions reduce through
typed binary helpers because LG cannot yet express the upstream `max-key`
dependency and variadic rest relationship in one source signature.

## Bootstrap artifacts

The `stdlib` Dune directory compiles source namespaces before application
chunks:

```sh
dune build @stdlib/stdlib-native
dune build @stdlib/stdlib-melange
```

Each alias produces checked OCaml source plus a target-specific serialized LG
compiler state. A consumer restores that state with `--compile-chunk-from`, so
namespace aliases, referred vars, signatures, and inferred bindings resolve
through the same ordinary incremental namespace machinery as application
code. The state is target-specific and must be regenerated with the compiler;
it is a build artifact, not a checked-in compatibility database.

The aggregate artifacts are `_build/default/stdlib/lg_stdlib_native.ml` plus
`lg_stdlib_native.state`, and the corresponding `lg_stdlib_melange.*` pair.
A consumer depends on the pair, compiles only its application chunk from the
state, and concatenates the aggregate implementation with the emitted chunk:

```sh
dune exec bin/lg_cli.exe -- \
  --target native \
  --compile-chunk-from _build/default/stdlib/lg_stdlib_native.state \
  app.cljc -o app_chunk.ml
```

Consumers must not enumerate individual stdlib `.mil` or `.cljc` files.
`test/stdlib` exercises this contract, including negative type tests restored
from the same aggregate state.

Generic `set<element>` source functions use LG's statically typed generic set
representation internally. Calls from concrete persistent set modules convert
through typed `elements` and `of_list` operations at that source-function
boundary, and results convert back to the statically selected concrete module.
Element types remain unified across all inputs and outputs. This boundary does
not use `Runtime_dynamic.t`, `Obj.magic`, or a source-visible conversion API.

## Porting another namespace

1. Inventory the namespace and transitive namespace dependencies in the Logseq
   code being migrated.
2. Pin the relevant ClojureScript source commit in `stdlib/upstream.edn`.
3. Copy the public algorithm into `stdlib/<namespace>.cljc`, keeping upstream
   control flow and observable arities. Add the narrowest `.mil` signatures
   needed to state relationships that inference cannot yet recover.
4. Classify every upstream definition in the manifest. Document every static
   adaptation or deferred definition instead of silently replacing behavior.
5. Add a bootstrap/restore integration test for Native and Melange, behavioral
   tests for every arity and important branch, static rejection tests, and an
   audit that rejects public-name compiler dispatch.
6. Add the namespace to the target's aggregate stdlib build only after its
   dependency namespaces are available.

Priorities should be driven by an actual Logseq dependency inventory. Likely
early layers include remaining source-definable `clojure.core` functions,
`clojure.string`, `clojure.set`, `clojure.walk`, and reader/EDN namespaces,
followed by the library namespaces that appear most often in the selected
Logseq migration slice. Compiler changes should add general language support
needed by several ports, not a dispatch branch for one public var name.

## Auditable surface inventory

Run the inventory from the repository root:

```sh
script/generate_clojure_surface_inventory.sh . ../logseq ../clojurescript
```

The tab-separated output records the pinned ClojureScript commit, every string
alternative in the compiler's main call dispatcher, compiler-owned namespace
vars, referenced runtime primitive boundaries, and Logseq standard-library
namespace/qualified-var usage. The Logseq reader resolves aliases from each
file's `ns` form and respects `.gitignore`; qualified-var counts are lexical
occurrences after alias resolution, so they are a prioritization signal rather
than a reachability analysis.

When a pinned ClojureScript checkout is supplied, the report also contains an
`upstream-var` row for every public function and macro read from the reviewed
core and namespace sources. The extractor evaluates both Clojure and
ClojureScript reader-conditional branches, handles tagged JavaScript literals,
and excludes private definitions. The pinned surface currently contains 855
function/macro entries, including 666 entries in `cljs.core`. Each row is
classified independently as source, typed primitive, special form, host
boundary, static-typing blocker, or deferred. A namespace's aggregate support
does not make a missing public var appear supported. The current baseline is
89 source entries, 175 typed primitives, 5 special forms, 12 host boundaries,
206 static-typing blockers, and 368 deferred entries. The deferred set is the
explicit queue for further source-port and compiler/macro-boundary review.

When the optional ClojureScript checkout is supplied, its `HEAD` must match the
commit in `stdlib/upstream.edn`. The inventory also records the Logseq checkout
commit. `logseq-namespace-status` and `logseq-qualified-var-status` rows classify
each observed dependency as `source-aggregate`, `source-core-alias`,
`blocked-static-typing`, or `unsupported`; the final field is a machine-readable
reason. This makes unsupported namespaces visible without confusing test and
build-time libraries with source namespaces that LG already provides.

The generator pins the reviewed compiler dispatch count and fails when that
surface changes. Entries are classified as `source-shadowed`,
`blocked-static-typing`, `special-form`, `typed-primitive`, or `host-boundary`.
Every `blocked-static-typing` entry must have a concrete, machine-checked
reason; the inventory test rejects the former catch-all blocker description.
Completion requires reducing `source-shadowed` to zero by removing its legacy
name-based compiler fallback, while resolving each static-typing blocker as the
language gains the required capability, variadic, or higher-order relation.
The current 276-name compiler dispatch inventory has zero `source-shadowed`
entries: the source definitions of `identity`, `complement`, `boolean`, `even?`, `odd?`, `every?`, `ffirst`, `fnext`, `nfirst`, `nnext`,
`not`, `reduced`, `not-any?`, `not-every?`, `split-at`, `split-with`, `nthnext`, `nthrest`, `bounded-count`, `butlast`, `take-last`, `drop-last`, `reverse`, `second`, `last`, `interpose`, `dedupe`, `distinct`, `zipmap`, `hash-combine`, `quot`, `rem`, `mod`, the `unchecked-*` integer arithmetic helpers, `rand-int`, `rand-nth`, `bit-shift-right-zero-fill`, `clojure.string/escape`,
`subs`, `int-to-string-radix`, `any?`, `range`, `shuffle`, `alength`, `acopy`,
`aslice`, `aconcat`, `array-to-seq`, `array-to-rseq`,
the upstream four-argument `amap` macro, the typed `asort!` extension,
and the derived bit functions have no legacy compiler fallback. At the current checkpoint, the Logseq tree requires
`clojure.string` 391 times,
`clojure.set` 74 times, `clojure.walk` 30 times, `clojure.edn` 27 times,
`cljs.reader` 27 times, and `clojure.data` 6 times. This makes the remaining
reader/walk/data boundaries visible instead of treating `clojure.set` as the
scope of the standard-library migration. The aggregate `clojure.set` source
namespace now provides `union`, `intersection`, `difference`, `subset?`,
`superset?`, `select`, `map-invert`, and `rename-keys`; its 74 namespace
references and all 221 observed qualified-var references resolve through the
source artifact. The inventory records `project`, `rename`, `index`, and
`join` as var-level static-typing blockers, so an unimplemented var cannot
inherit the supported status of its namespace. The same checkout also reports
`cljs.test` occurs 229 times and is explicitly blocked on analyzer-backed
macros, dynamic test environments, and a closed report-event domain.
`clojure.test` occurs 51 times and is classified as a JVM-only host boundary.
`cljs.pprint` occurs 15 times and is explicitly blocked because
readable and display printing need distinct static printer witnesses;
`clojure.pprint` occurs 14 times and is a JVM-only host boundary. `clojure.zip`
occurs 3 times and is blocked on its public heterogeneous location vectors and
metadata-held generic callbacks. `cljs.spec.alpha` is blocked on analyzer macro
integration and a closed recursive spec/explain-data domain, while the
`clojure.spec.alpha` references are JVM-only. `clojure.walk` and `clojure.data` remain explicitly blocked because
their upstream algorithms traverse heterogeneous Clojure trees; a valid port
must use a closed value domain rather than the existing `Runtime_dynamic.t`
boundary.

The architecture tests in `test/stdlib` enforce that `clojure.set` is no
longer classified as compiler-owned and that source-owned core functions have
no name-based call elaboration or inference path.
