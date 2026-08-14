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

The first namespace port is `clojure.set`, based on ClojureScript
`src/main/cljs/clojure/set.cljs` at commit
`5c6ef531604662afbb33dc1b553d7602634d9656`. Its size-based binary algorithms
and branch order follow upstream. Its variadic definitions reduce through
typed binary helpers while preserving the same size-based selection behavior.

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

The CLI can also compile and execute a consumer against the same artifact pair:

```sh
dune exec bin/lg_cli.exe -- \
  --run-from _build/default/stdlib/lg_stdlib_native.state \
  _build/default/stdlib/lg_stdlib_native.ml app.cljc
```

Use `--run-files-from` with the same first two arguments for a multi-file
consumer. These commands compile only consumer chunks; they do not rebuild or
enumerate stdlib sources.

Consumers must not enumerate individual stdlib `.lgi` or `.cljc` files.
`test/stdlib` exercises this contract, including negative type tests restored
from the same aggregate state.

In-process tooling can pass the restored compiler state to
`Language_service.analyze_from_state` or
`Language_service.analyze_workspace_from_state`. This keeps completion, hover,
definitions, and workspace analysis on the same source registry as ordinary
compilation. A consumer that intentionally analyzes a source fragment without
an `ns` form can first call `Compiler.with_source_scope ""`; this selects the
root source scope and installs automatic core refers without reintroducing
name-based compiler dispatch. Ordinary source files should continue to declare
their namespace.

The compiler regression runner follows the same contract. Its ordinary
`Lg.Compiler.compile_string` test facade compiles application chunks from one
cached aggregate state per target, while `Raw_lg.Compiler` is reserved for
tests that intentionally exercise an empty compiler state. This prevents
source-owned core names from being reintroduced as public compiler dispatch
merely to keep legacy compiler-only test fixtures working.

LG signature sidecars use the `.lgi` extension consistently. These files
contain LG `signature` forms and are compiled by `lg_cli` before their matching
`.cljc` source. The `.mli` extension is reserved for ordinary OCaml interfaces
parsed by the OCaml compiler. The legacy `.mil` extension and LG signature forms
stored in `.mli` files are rejected by repository architecture tests.

Multi-file compilation prepares each source once. The parsed forms provide
both required OCaml packages and the subsequent incremental compilation input;
the CLI does not parse the same file again after restoring a compiler state.
State-producing compilation does not read or write prefix caches: its explicit
state artifact is already the reusable checkpoint. The cold-build gate covers
the full aggregate-stdlib plus DataScript path and enforces a fixed 10-second
limit. The limit cannot be relaxed through configuration, and the gate removes
cache-related environment settings before building from a clean tree.

The cold path builds the native compiler with classic inlining and disables
cross-function expansion. Recursive DataScript and persistent-sorted-set SCCs
carry rigid static signatures, so they compile once instead of repeatedly
stabilizing inferred declaration ABIs. The gate measures both compiler
construction and source generation; neither optimization depends on a warm
Dune or LG compile cache.

The lightweight type-refinement operations live in a separate compilation unit
from full parameter inference. This lets OCaml compile the large call elaborator
and the full inference engine in parallel without duplicating either algorithm.

Generic `set<element>` source functions use LG's statically typed generic set
representation internally. Calls from concrete persistent set modules convert
through typed `elements` and `of_list` operations at that source-function
boundary, and results convert back to the statically selected concrete module.
For overloaded functions, the compiler retains each declared arity's original
return type as the implementation storage type before specializing the public
result. This keeps one-, two-, and variadic `clojure.set` calls on the same
generic boundary instead of mistaking a `Runtime_poly_set.t` result for a
concrete `Int_set.t` or another element-specific module.
Element types remain unified across all inputs and outputs. This boundary does
not use `Runtime_dynamic.t`, `Obj.magic`, or a source-visible conversion API.

Persistent maps carry metadata as a closed `Lg_edn_backend.t` value. The public
`meta` and `with-meta` vars are source functions backed by the ClojureScript
`IMeta` and `IWithMeta` protocols; they work through automatic core refer,
qualified aliases, and first-class bindings. A private `__lg_with-meta` typed
primitive converts statically representable EDN metadata literals at the call
boundary. Persistent `assoc`/`dissoc` operations and source ports such as
`update-keys` and `update-vals` preserve metadata on Native and Melange without
converting the map or metadata through `Runtime_dynamic.t`.
`vary-meta` is likewise a public source var with all six pinned ClojureScript
arities. Its first-class map signature consumes and returns the closed metadata
domain; direct calls bind the receiver once and contextually specialize
`IMeta`/`IWithMeta` protocol calls, including variadic
callbacks and custom metadata-capable receivers.

`sorted-map` is a distinct source-defined collection, not an alias for the
default hash map and not a vector-backed compatibility layer. Its closed
`persistent-tree-map-node<key,value>` sum ports ClojureScript's red/black node
states, insertion balancing, append, and deletion balancing. `assoc` and
`dissoc` retain logarithmic path-copying behavior and preserve metadata;
missing-key deletion returns the original value. `ISeqable` emits entries by
tree traversal, and `ISorted` supplies ordered sequences, bounded traversal,
entry keys, and the stored comparator. `sorted-map-by` shares the same tree and
evaluates its comparator once. LG currently requires that comparator to have
the static type `key -> key -> int`; ClojureScript's additional normalization
of boolean predicate comparators remains a documented static adaptation.

`sorted-set` and `sorted-set-by` wrap that same red-black tree in a typed
`persistent-tree-set<value>` record. They preserve comparator order,
persistent `conj`/`disj`, `ISorted`, and metadata without substituting a hash
set or vector representation.

`mk-bound-fn`, `subseq`, and `rsubseq` are source-defined from the pinned
ClojureScript algorithms. Their `sorted<entry;key;storage>` sidecar capability
keeps map entries distinct from map keys while using the same `ISorted`
protocol witness for tree sets. LG sequences represent empty ranges directly
instead of using ClojureScript's nullable sequence sentinel. Because LG
comparison operators are compiled as closures rather than stable JavaScript
function objects, the direction branch probes the supplied ordering predicate;
the bounded traversal, endpoint inclusion, cursor movement, and termination
order remain the upstream ones.

Sidecars describe a homogeneous variadic arity with
`variadic-fn<fixed...;rest;result>`. The final two arguments are the rest
element and result types; preceding arguments are fixed parameters. A
`variadic-fn` can appear inside `overload<...>`, allowing source definitions to
share key, value, callback, and rest-element variables without erasing the rest
sequence.

## Porting another namespace

1. Inventory the namespace and transitive namespace dependencies in the Logseq
   code being migrated.
2. Pin the relevant ClojureScript source commit in `stdlib/upstream.edn`.
3. Copy the public algorithm into `stdlib/<namespace>.cljc`, keeping upstream
   control flow and observable arities. Add the narrowest `.lgi` signatures
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
vars, runtime primitive boundaries referenced directly or through stdlib
namespace aliases, and Logseq standard-library namespace, qualified-var, and
automatic core-refer usage. The Logseq reader resolves aliases from each file's
`ns` form, respects `.gitignore`, and records unqualified symbols that match a
known `clojure.core` manifest entry as `logseq-core-var` rows. Qualified-var and
core-var counts are lexical occurrences after alias or core resolution, so they
are a prioritization signal rather than a reachability analysis.

When a pinned ClojureScript checkout is supplied, the report also contains an
`upstream-var` row for every public function, macro, var, multimethod, and
protocol method read from the reviewed core and namespace sources. The extractor evaluates both Clojure and
ClojureScript reader-conditional branches, handles tagged JavaScript literals,
recurses through top-level `if` branches, and excludes private definitions. The
pinned surface currently contains 1,058 public entries, including 790 entries
in `cljs.core`. Public methods declared by
`defprotocol` are inventoried independently instead of being hidden behind the
protocol var. Each row is
classified independently as source, typed primitive, special form, host
boundary, static-typing blocker, out of scope, or deferred. A namespace's
aggregate support does not make a missing public var appear supported. Manifest entries for
`clojure.core` also classify the corresponding `cljs.core` function and inline
macro surfaces. The current baseline is 786 source entries, 3 typed primitives,
59 special forms, 123 host boundaries, 29 deferred entries, and 58 explicitly
out-of-scope Spec entries. With Spec excluded by project scope, source coverage
is 786/1,000 (78.6%); 971/1,000 entries are either source-owned or have a
documented primitive, special-form, or host boundary. All 29 deferred entries
belong to `cljs.pprint`. `cljs.core` itself has 616 source entries, 3 typed
primitives, 56 special forms, 115 host boundaries, and no deferred or
unclassified entries. Every public function,
macro, protocol method, multimethod, and public value discovered in the pinned
surface therefore has explicit ownership and evidence.
Source coverage only counts
real precompiled LG definitions; classifying a boundary does not inflate the
percentage.

Runtime validation against the pinned `jank-lang/clojure-test-suite` checkout
is separate from compile-scan coverage. The manifest-driven promoted smoke
currently contains 174 namespaces (174/185, 94.1%): Native executes 167
applicable namespaces as 185 tests, and Melange executes 173 namespaces as 191
tests with 2,998 assertions. Both are green. The
manifest accepts an optional `native` or `melange` qualifier, so target-specific
numeric identity tests remain explicit while ordinary additions require no
Dune or generated-runner edits. Static-error fixtures remain in the generated
audit and are not treated as runtime failures or weakened into dynamic values.

The promoted dynamic Var batch preserves `^:dynamic` metadata on both `defn`
and `defn-`, stores a dynamic function as a typed `ref<fn<...>>`, and
dereferences it through the ordinary runtime-value path when it appears in call
position. `binding` remains a compiler-owned scope form because it installs and
restores typed Var roots. Public `bound-fn*` is source-defined with an inline
call to the private `__lg_bound-fn` typed primitive, while `bound-fn` is a
source macro over it. The primitive enumerates statically known dynamic roots at
the call site and captures each root in its own concrete option type; it uses
neither `Runtime_dynamic.t`, a heterogeneous registry, nor universal
pack/unpack. Unbound roots remain uncaptured and observe later call-site
bindings, while active roots are restored around every call, including nested
capture and exception restoration.

The promoted `rest`, `next`, `nnext`, and `fnext` batch preserves map-entry
tuple storage and nested vector storage while implementing Clojure sequential
equality across those representations. The private `__lg_next_seq` marker uses
an empty `Seq.t` as its compact OCaml representation, but equality, `nil?`, and
EDN conversion observe it as Clojure nil; ordinary `rest` continues to return
an empty non-nil sequence.

The promoted universal-predicate batch adds `every?` and `not-every?` through
the upstream first-class helper shared by both namespaces. Static overloaded
callback parameters preserve vector, set, list, nil, callable set/map, truthy
identity, early-termination, and infinite-sequence cases. Non-empty all-nil
vectors retain a `vector<nil>` inference instead of merging with empty
`vector<any>` overloads, and literal `true?`/`false?` calls constrain open call
results to their declared boolean input without introducing dynamic storage.
The upstream bad-shape runtime-exception assertions remain static errors and
are intentionally omitted under the static-error policy.

The promoted numeric-parser batch adds `parse-long` and `parse-double` on both
targets. Decimal grammar, signs, scientific notation, Infinity, malformed
strings, and nullable results follow the upstream target branches. Native
`parse-long` validates with `int_of_string_opt`, accepting values within the
host static integer range, while Melange retains the ClojureScript
`Number.isSafeInteger` boundary. Non-string runtime-exception assertions remain
compile-time type errors and are omitted under the static-error policy.

The scalar/UUID/string-lookup batch promotes `short`, `random-uuid`, and the
statically expressible string cases from `get` on both targets. `short` keeps
the ClojureScript identity-cast behavior in the shared source library.
`random-uuid` preserves the upstream version-four and variant bits, and its
format assertion now traverses the split UUID through ordinary `get-in`.
Two-argument string `get` returns `option<char>` after explicit lower/upper
bounds checks; the three-argument form accepts a statically matching `char`
default. This also lets nested `get-in` continue through strings without a
dynamic lookup or a name-specific UUID workaround.

The lazy-prefix batch promotes `take`, `take-nth`, `take-while`, `drop-last`,
and `drop-while` on both targets. It covers finite and infinite inputs, empty
and nil inputs, count boundaries, negative `drop-last`, transducer early
termination, and fresh state when one transducer value is reused. `drop-while`
keeps the upstream predicate call order while spelling its first-item state as
nested source conditionals so a general truthy predicate remains statically
represented. `take-nth` explicitly skips every input for a zero-step
transducer, matching ClojureScript on Native without evaluating integer
remainder by zero. Negative steps retain their upstream transducer behavior.

The arithmetic batch promotes `+`, `-`, `*`, and `/` on both targets with
zero, unary, binary, variadic, and `apply` coverage where each operator allows
those arities. Native integer division now returns exact ratios for direct and
first-class calls, including non-integral and reciprocal results, instead of
silently truncating through OCaml integer division. Melange uses a separate
typed floating primitive and retains ClojureScript Infinity and NaN behavior.
Decimal, mixed int/float, and left-to-right reduction remain statically typed.

The checked-arithmetic batch promotes `+'` and `*'` on both targets. Their
source definitions preserve the upstream zero, unary, binary, variadic,
`apply`, and first-class call shapes over LG's static integer, float, and
decimal numeric domains. Apostrophes are encoded as `_prime` in generated
OCaml bindings, so an ordinary source name and its apostrophe-suffixed variant
remain distinct instead of colliding after name sanitization. The same batch
promotes direct `var?` syntax recognition and the static callable domains of
`ifn?`, including functions, maps, sets, vectors, keywords, and symbols. LG
does not expose a first-class JavaScript Var wrapper; resolved values therefore
remain non-Vars outside literal `var`/reader-var-quote syntax.

The higher-order defaulting batch promotes `fnil` on both targets. Its
first-class sidecar now retains independent rigid types for the first, second,
third, and variadic-rest argument positions instead of forcing every default
and wrapped parameter into one type. Direct, aliased, qualified, and
materialized calls preserve two- and three-argument substitution, false versus
nil behavior, and one-time evaluation of default expressions without dynamic
packing.

The hierarchy batch promotes `parents`, `ancestors`, `descendants`, `derive`,
and `underive` on both targets. Curated upstream fixtures cover direct and
transitive queries, chains, diamonds, multiple parents, immutable local
hierarchies, global mutation and cleanup, symbol tags, idempotent derivation,
and closure rebuilding after edge removal. The upstream host-class and custom
record cases remain outside the portable fixture because LG intentionally uses
the closed EDN named-value hierarchy domain rather than JVM or JavaScript class
objects.

The transient collection batch promotes `assoc!`, `conj!`, `disj!`, `dissoc!`,
`persistent!`, and `pop!` on both targets. Map, vector, and set fixtures are
partitioned by static collection domain while retaining upstream zero, unary,
and variadic behavior, repeated and absent key removal, vector append/update,
map-entry insertion, set insertion/removal, and persistent conversion. Invalid
persistent collection calls remain compile-time static errors and are not
weakened into a universal transient representation.

The comparison/eager traversal batch promotes `compare`, `shuffle`, `update`,
`group-by`, and `run!` on both targets. Homogeneous vectors now compare in
Clojure lexicographic order through recursive static element comparators rather
than the RRB tree's OCaml representation. `run!` accepts a closed typed
ordinary-or-reduced callback capability, preserves left-to-right effects,
returns nil, and stops immediately on a reduced result. The fixtures also cover
shuffle permutation invariants, all supported `update` extra-argument arities,
and stable item order within `group-by` buckets. Metadata-bearing `group-by`
rows remain outside this portable fixture until metadata-preserving vector
storage is available.

The promoted collection/sequence batch adds `butlast`, `conj`, `dissoc`,
`distinct`, `drop`, `find`, `partial`, `peek`, `rand-nth`, `reverse`, `rseq`,
`seq?`, and `take-last`. Empty stack peeks and empty reverse sequences now use
typed nil-aware results, three-argument `into` preserves its target collection
type through transducer inference, `conj` infers appended values independently
from the result collection expectation, and structural-map `find` coerces a
closed lookup key to the map's declared key representation before lookup.

The next higher-order and collection conversion batch promotes `set`, `some`,
and `when-first`. Polymorphic unary callbacks now instantiate their return type
from the collection element type, so `some identity` retains a closed EDN
result. Closed EDN truthiness and boolean predicates distinguish only nil and
false as falsey, and set equality converts otherwise incompatible but
EDN-representable element shapes before comparison. This covers map and set
predicates, nil-key maps, tuple map entries versus vector entries, and mixed
closed EDN sequences without dynamic storage. The upstream `get-in` fixture
remains outside promotion because an unselected `(range)` inside a heterogeneous
nested map is eagerly realized while packing the map into closed EDN. The
upstream `vec` fixture remains outside because ClojureScript aliases the source
JavaScript array, while LG vectors use one persistent RRB representation on
Native and Melange and therefore copy array contents.

The promoted lazy iteration and map construction batch adds `cycle`, `doseq`,
`hash-map`, and `interleave`. It covers finite prefixes of infinite sequences,
destructuring and modifier clauses in `doseq`, variadic interleaving through
`apply`, heterogeneous map construction, duplicate keys, and nested maps. Map
equality now treats an empty structural map as compatible with a runtime map
of any static key type and recursively compares generic nested map shapes;
independently instantiated empty maps therefore no longer compile to constant
false, and no dynamic representation is introduced.

The promoted numeric extrema batch adds `max` and `min`. Native mixed
integer/ratio calls promote integers into the closed `Runtime_ratio.t` domain,
compare exact ratios, and use explicit cross-domain numeric equality. Melange
nil/numeric calls compare nil as JavaScript zero while returning an optional
original argument, preserving whether nil or the number won the comparison.
NaN propagation, infinities, single-argument identity, later-tie selection,
and variadic left-to-right reduction remain covered without dynamic values.

The promoted numeric ordering batch adds `<`, `<=`, `>`, and `>=`. Native
ratio/integer pairs compare exactly through `Runtime_ratio.compare`, while
ratio/float pairs use an explicit float projection. Decimal comparisons retain
their exact decimal path; Melange nil/numeric pairs retain ClojureScript's
target-specific zero coercion. Unary identity, pairwise short-circuit order,
NaN, infinities, mixed numeric calls, and variadic `apply` are covered without
dynamic numeric dispatch.

The scalar coercion batch adds `byte`, `char`, `float`, and `parse-boolean` on
both targets, with `neg-int?` additionally promoted on Melange. The cast
functions preserve their ClojureScript identity behavior, while the fixture's
mixed ratio, decimal, integer, and float cases remain separate static domains.
When a Melange `int?` guard succeeds for a safe float or decimal, guarded code
now receives the corresponding JavaScript integer representation instead of an
unreachable placeholder. Native `neg-int?` and `bigint` promotion still require
a distinct arbitrary-precision integer reader/source domain; that gap is kept
explicit rather than represented dynamically.

The next predicate batch adds `uuid?` on both targets and `pos-int?` on
Melange. A successful `parse-uuid` remains `option<uuid>` at its public static
boundary; the UUID predicate tests that option directly instead of rejecting
the wrapper or unpacking it dynamically. Melange positive-integer checks reuse
the safe float/decimal-to-JavaScript-integer guard projection. Native promotion
continues to wait for a distinct bigint reader and source domain.

The numeric predicate batch adds `zero?`, `pos?`, and `neg?` on both targets.
Their source-owned inline paths now preserve the argument's closed int, float,
decimal, or ratio domain and compare against a same-domain zero. Melange also
implements the upstream nil/boolean coercion explicitly. First-class use keeps
the existing static integer contract, and direct polymorphic calls do not add
dynamic numeric storage.

The nested sequence batch adds `ffirst` and `nfirst` on both targets. Map-entry
tuples participate in the general static seqable capability when their fields
share an element type, and nested expected types refine map literals before
code generation. Empty maps therefore preserve nil behavior while non-empty
maps return the key or value tail through the ordinary source composition.
No function-name dispatch or dynamic map-entry representation is involved.

The corrected Logseq audit exposes high-frequency automatic core blockers that
were invisible in qualified-var-only reports. At the pinned Logseq commit the
largest former blocker rows were `type` (499), `with-redefs` (427),
`flatten` (15), and `memoize` (10). `type` is a documented host boundary,
`with-redefs` is now source-aggregate supported through typed var-root
rebinding for ordinary Logseq test replacement shapes, and `flatten` is now
source-aggregate supported for statically homogeneous sequential layers
through a private typed primitive. `defmethod` appears 162
times, `defmulti` 8 times, `dispatch-fn` 7 times, `methods` 5 times, and
`get-method` once; those rows are now source-aggregate supported through the
documented runtime multifn dynamic boundary.
`add-watch` appears 36 times and `remove-watch` appears 14 times; both are now
source-aggregate owned through `IWatchable/-add-watch` and
`IWatchable/-remove-watch`. `get-validator` and `set-validator!` are also
source-aggregate owned; the reference runtime stores an optional
`value -> bool` validator tied to the same reference value type. The underlying
protocol methods, including `-notify-watches`, dispatch to a
compiler-registered `Runtime_reference.t` implementation that keeps the
watched value type parameterized and stores only keyword-keyed callbacks for
that reference value type. `reset-meta!` and `alter-meta!` are now
source-aggregate owned over the same typed reference cells. The runtime stores
reference metadata as closed `Lg_edn_backend.t`; the only compiler ABI is the
private `__lg_reset-meta!` primitive that packs statically representable EDN
metadata and mutates the reference metadata field without `Runtime_dynamic.t`.
`alter-meta!` preserves the upstream update arities when the updater returns a
closed EDN metadata value. `re-find` appears 290 times,
`ex-data` appears 210 times, `re-matches` appears 87 times, and `re-seq`
appears 23 times; all four are
now source-aggregate owned, with regex match shapes and exception data recorded
as documented narrow dynamic boundaries rather than blockers. These rows are
the priority order for removing the remaining static blockers unless a
lower-count item unlocks several higher-count ones.

The printing entry-point cluster is source-owned. `str`, `pr-str`, `pr-str*`,
`print-str`, `println-str`, `prn-str`, `pr`, `print`, `println`, and `prn` retain
their zero, one, and variadic public shapes. Direct calls expand through source
inline definitions so heterogeneous arguments keep independent display and
readable printer witnesses; first-class calls use the homogeneous static
variadic signature. Only display rendering, readable rendering, and static
string output remain private typed ABI operations. Automatic core references
and qualified/referred calls use the same inline definitions; the compiler no
longer dispatches on those public names.

The writer printing cluster is source-owned as well. `IPrintWithWriter`,
`-pr-writer`, `pr-writer`, `pr-sequential-writer`, `write-all`, `string-print`,
and both `newline` arities follow the pinned ClojureScript control flow. Built-in
values and consumer extensions dispatch through static protocol witnesses, and
writer callbacks remain first-class. The only compiler boundary is the private
typed rendering operation used by built-in implementations. LG currently
models `*flush-on-newline*`, `*print-newline*`, `*print-readably*`,
`*print-meta*`, `*print-dup*`, and `*print-namespace-maps*` as source dynamic
vars with type `bool`, and models `*print-length*` and `*print-level*` as
source dynamic vars with type `option<int>`.
`pr-str`, `pr-str*`, `pr`, `prn`, and `prn-str` select readable or display
printer witnesses through `*print-readably*`, while `println` and `prn` use
`*print-newline*` only for the trailing newline switch and `*flush-on-newline*`
only for the newline-triggered stdout flush. `binding` and
`pr-str-with-opts`/`prn-str-with-opts` support the typed `{:print-length n}`
options map without widening printer options to `Runtime_dynamic.t`.
`*print-level*` preserves the upstream recursive collection depth limit through
the same static readable printer witnesses. The custom more marker remains an
explicit static adaptation instead of silently widening the options value to a
dynamic map.
The `*print-meta*`, `*print-dup*`, and `*print-namespace-maps*` switches are
ordinary static Vars and no longer compiler-injected. `print-meta?`,
`print-map`, and `print-prefix-map` are source public vars backed by private
typed printer primitives: `print-meta?` checks the typed `{:meta bool}` option
and closed EDN metadata without `Runtime_dynamic.t`, while map rendering uses
independent static key/value printer witnesses and honors
`*print-namespace-maps*` for literal namespaced keyword maps. The current
static adaptation evaluates the upstream `print-one` callback argument but does
not delegate rendering to arbitrary custom callbacks; that restriction is
recorded in `stdlib/upstream.edn`.

The numeric operator cluster is source-owned. `+`, `-`, `*`, `/`, `<`, `<=`,
`>`, `>=`, and `==` preserve the pinned zero, unary, binary, and variadic
shapes that apply to each operator, including unary reciprocal division and
pairwise monotonic comparison. Their source inline definitions route direct
integer, floating, and mixed numeric calls to private typed primitives; the
ordinary source functions remain statically first-class for homogeneous integer
calls. Native `/` returns the closed exact-ratio domain for integer direct and
first-class calls; Melange `/` returns its statically typed floating numeric
result and preserves ClojureScript zero-division behavior. Generated OCaml uses
readable names such as `clojure_core_add` and
`clojure_core_less_equal`. Metadata normalization recognizes a generic type
hint only when its `<...>` form is complete, so ordinary `<` and `<=` symbols
remain valid source definition names.

Generic `=` is source-owned with the pinned one-, two-, and variadic
short-circuiting control flow. Its inline definition delegates each direct call
to the private `__lg_equal` static capability boundary, while the ordinary
source function supports homogeneous statically typed first-class use. The
remaining previously generic “typed primitive” classifications were audited:
`instance?` and `satisfies?` are compiler-owned static type/protocol witness
elaboration because their first position is an analyzer symbol rather than a
runtime source value, and `type` is rejected because runtime class inspection
conflicts with LG's closed static type model. The pinned upstream inventory now
contains no public surface classified merely as a typed primitive.

`munge` and `demunge` preserve ClojureScript's string-or-symbol result identity
through private protocols with a static `:self` return. Their source functions
remain first-class; the runtime boundary is limited to typed string
transformation using the complete ClojureScript character, demunge, and
JavaScript-reserved-word tables.

The denominator now includes the complete pinned `cljs.math` public surface.
Its first source batches provide trigonometric and hyperbolic functions,
logarithms, square root, exponential functions, stable hypotenuse,
ceiling/floor, degree/radian conversion, and the `E`/`PI` constants through
static OCaml float operations on both Native and Melange. Cube root, power, and
random use small explicitly typed cross-target runtime boundaries rather than
dynamic values. The private upstream `IEEE-fmod` helper is retained as a
private source definition for the future public `IEEE-remainder` port and is
not counted as public surface.
The private absolute-value helper and public sign-bit copying function use the
same narrow IEEE float boundary; `rint` preserves the upstream 2^52
ties-to-even algorithm in source, and `signum` composes the source operations
without dynamic numeric dispatch.
The next IEEE batch adds public `round`, `get-exponent`, `next-after`,
`next-up`, `next-down`, `ulp`, `scalb`, and `IEEE-remainder`. Bit-sensitive
operations share a small statically typed `Int64` implementation on Native and
Melange; rounding and IEEE remainder control flow remains readable LG source.
The public-surface extractor recognizes both metadata attached to a name and
the declaration attribute-map form used throughout `cljs.math`, so private
upstream helpers cannot inflate the denominator or the deferred queue.
The six `*-exact` arithmetic functions use binary64 source arithmetic and the
same positive and negative safe-integer limits as ClojureScript. This keeps
their behavior identical across Native and Melange instead of inheriting each
target's OCaml `int` width. Overflow construction crosses only the existing
open `ExceptionInfo` data field through a typed `string -> exn` runtime helper;
source maps are not packed into dynamic values.
`floor-div` and `floor-mod` complete the pinned public `cljs.math` surface: the
namespace now has zero deferred public vars. Their source preserves upstream
safe-integer validation, truncation, sign correction, divisor-signed remainder,
and JavaScript zero-divisor number behavior in the shared binary64 domain.
`prim-seq` and `set-from-indexed-seq` are source-defined generic collection
adapters. They reuse the existing typed array sequence and default HAMT set,
including both `prim-seq` arities, without introducing the JavaScript-only
`IndexedSeq` representation or the upstream transient array fast path.
`munge-str` is a source-visible static string function. Its complete upstream
character table lives in a typed runtime helper because the current LG lexer
cannot express delimiter characters such as braces and brackets as char
literals; the boundary is `string -> string` and does not use dynamic values.
The collection access, update, and transient families are source-visible,
including `contains?`, `get`, `get-in`, `assoc`, `assoc-in`, `dissoc`, `update`,
`update-in`, `select-keys`, `merge`, `keys`, `vals`, and the seven public
transient operations. Their source arities preserve ClojureScript control flow
while receiver/key/value-dependent elaboration is restricted to private
`__lg_*` primitives.
`seq`, `first`, `rest`, `next`, and `cons` are also precompiled source
functions over explicit polymorphic seqable signatures. Public call elaboration
has been removed. The type inference pass still recognizes the four upstream
sequence names before source expansion; these are static constraints, not
runtime implementations. `next` deliberately remains a non-inline source call
so its optional-sequence result cannot destabilize generic loop inference.
`some` now preserves the upstream source loop and first-truthy short circuit.
Direct calls inline to a private typed primitive only for the callback-result
option-flattening boundary, while first-class calls use the source binding and
its inferred static truthiness contract.
`doall` is source-owned with both upstream arities. It realizes through the
source `dorun` implementation and returns the original collection rather than
substituting a vector or another eager representation. Its explicit
`seqable<value; storage> -> storage` relationship preserves vector, list, and
other statically known collection storage without a dynamic value boundary.
`char` is source-owned through a private static coercion protocol implemented
for integers, one-character strings, and LG chars. Invalid strings and
unsupported static inputs retain the upstream runtime error behavior. Integer
conversion is restricted to LG's OCaml-backed eight-bit char domain rather
than silently pretending to support JavaScript UTF-16 code units.
`name` is source-owned through a private static coercion protocol as well.
Strings retain the upstream identity case, while keywords and symbols delegate
to their `INamed` implementations. The macro-helper extractor tracks lexical
bindings, so a local such as `when-first`'s destructured `name` can no longer
steal the public runtime definition into the compile-time-only helper set.
`conj` is source-owned with the complete zero, one, two, and variadic upstream
arity family. Lists, vectors, sets, sequences, user `ICollection`
implementations, and statically typed map-entry tuples preserve their result
category. Direct calls inline to the private collection ABI, and map entries
expand through private association so record shape changes remain visible to
type checking. The tuple is a documented adaptation for ClojureScript's
heterogeneous two-element map-entry vector.
`unreduced` is source-owned and uses only a private parameterized reduced-value
extraction boundary. `namespace` is source-owned through consumer-state
`INamed/-namespace` expansion; the `cljs.core` protocol alias resolves to the
canonical `clojure.core` protocol so user implementations retain static
witnesses. `keyword` and `symbol` are source-owned one/two-arity functions over
private static coercion protocols. Their namespace arity accepts `nil`, strings,
keywords, and symbols without a dynamic union; only the final typed string
construction remains primitive. ClojureScript `Var` to symbol conversion is not
available because LG exposes no source `Var` value. LG's current string-backed
identifier representation also cannot retain ClojureScript's separate cached
namespace/name fields for malformed multiple-slash constructor strings; this
deviation is recorded in the manifest rather than hidden by the source port.
`list*` is source-owned as a macro over the private typed list-splice primitive.
All direct upstream arities preserve final-sequence expansion, left-to-right
single evaluation, alias/refer behavior, and LG's existing eager typed-list
result for one static element type. The complete first-class heterogeneous
variadic function shape remains unavailable without dynamic typing; this
static adaptation is explicit in the manifest. The compiler no longer
dispatches on the public `list*` name or packs heterogeneous prefixes.
All pinned public `cljs.core` macros now have explicit ownership and zero remain
deferred. Compiler/analyzer declarations and namespace-environment operations
are special forms; JavaScript syntax and host-object macros are host boundaries;
multimethod, per-object protocol extension, and dynamic root-rebinding macros
carry concrete static blockers. These classifications do not count as source
coverage.
All remaining public `cljs.core` function and value surfaces are also explicitly
classified. The source-owned `not-native` value preserves the upstream `nil`
sentinel. The non-source families are JavaScript iterators and prototype
inspection, chunked-sequence internals, multimethods, validators,
heterogeneous printing, sorted collections, bootstrap namespace objects, and
analyzer helpers. Each individual var
has its concrete reason in `stdlib/upstream.edn`; there is no unreviewed
function queue hidden behind a generic deferred reason. Further source coverage
therefore proceeds by implementing one of these missing static capabilities as
a coherent family rather than by copying isolated wrappers.
The `clojure.core.protocols` aggregate namespace defines `Datafiable/datafy`
and `Navigable/nav` in source. Static protocol dispatch now supports an
upstream-compatible `:default` implementation, while concrete receiver
extensions still take precedence. Default methods are emitted as ordinary
polymorphic OCaml functions and survive precompiled-state chunk loading.
`cljs.core/INamed` and `cljs.core/IWriter` are also source protocols.
Keywords, symbols, user records, and typed OCaml buffers dispatch through
ordinary static protocol extensions. The compiler's own buffer writes use the
private `__lg_write` ABI, so the public `-write` name is not intercepted by the
call elaborator.

The statically supported ClojureScript collection, lookup, metadata,
comparison, reference, sorted-collection, and transient protocol declarations
also live in `clojure/core.cljc`. Their 45 public methods are source-owned; the
compiler registry retains only the receiver-specific typed implementations for
built-in lists, vectors, hash maps, sets, references, and host-backed
collections. A source declaration is checked against that implementation
surface and must preserve every supported method and fixed arity. Qualified
`cljs.core` aliases resolve these declarations to the same root protocol IDs,
so consumers do not get a parallel compatibility protocol. The statically
adapted `IReduce` surface exposes the explicit-initial-value arity, and `ISwap`
uses a unary typed updater after the public wrapper captures extra arguments;
both differences are recorded in `stdlib/upstream.edn`.

`ISeq` and `INext` are source-declared with typed `Seq.t` receiver witnesses.
`-first` has a static optional-element result, while `-rest` and `-next` retain
the sequence representation; the empty-aware `-next` adaptation is documented
in `stdlib/upstream.edn`. Native and Melange dispatch directly to the typed
sequence runtime, and vectors participate only after `seq`, matching the
ClojureScript protocol boundary. Every source `defprotocol` now exports its
protocol name as a compile-time namespace marker. Consequently protocol names
work through ordinary `:refer` across precompiled chunks without creating a
runtime protocol object or a parallel protocol identity; using such a marker as
an ordinary runtime value remains rejected.

`IDrop` uses an associated method-result witness, so vectors, sequences, and
user-defined receivers can return their statically known element sequence
without a universal collection value. Its nil-at-end result uses the same
empty-aware static sequence adaptation as `INext`. `IMapEntry` carries separate
`-key` and `-val` result types in one witness; the built-in map-entry tuple and
user-defined records therefore preserve different key and value types through
first-class `key` and `val` aliases. `IPending` has closed witnesses for LG
delays (`Lazy.t`), eager futures, and user implementations. LG's erased `Seq.t`
representation does not expose lazy realization state, so that unsupported
receiver difference remains explicit in `stdlib/upstream.edn`.

The complete pinned protocol-method surface now has no deferred entries: 53
methods are source-defined, 19 have concrete static-typing blockers, 7 are
explicit host boundaries, and 7 are out of scope. The compiler registry still
provides the receiver-specific typed implementations behind source-declared
protocol methods; those implementation witnesses are not separate public vars
and therefore do not inflate source coverage.
`ensure-reduced` is source-owned with a first-class `a -> Reduced<a>` signature
for ordinary values and a direct-call static specialization that preserves an
existing `Reduced<a>` wrapper without nesting it. `Reduced<a>` implements the
pinned `IDeref` contract through typed payload extraction.
`delay?` is source-owned through a first-class `Lazy.t<a> -> bool` signature
and direct-call static specialization for arbitrary known input types. `force`
is source-owned with a first-class `Lazy.t<a> -> a` signature and direct-call
static specialization that returns non-delay inputs unchanged. Both operations
evaluate their argument exactly once and remain fully static. The exact Logseq
scan finds one `force` call and no `ensure-reduced` calls.
`keep-indexed`, `take-nth`, `random-sample`, `partition-all`, and
`partitionv-all` now share the source lazy-sequence and reducing-function
foundation, so their collection and stateful transducer arities are both
source-defined.
`doseq` is a precompiled source macro and resolves through automatic core
refer, `cljs.core` alias/refer, and qualified `clojure.core` calls. It expands
only to private `__lg_doseq` macro elaboration. The elaborator preserves the
pinned modifier order and propagates recur ownership so `:while` terminates
only its current binding loop, including after `:let`; the JavaScript chunked
sequence fast path is omitted. Every binding starts from the receiver's static
Seqable implementation as upstream does; a separate Reducible implementation
does not bypass observable sequence construction. The Logseq scan finds 69
exact `doseq` calls in 40 source files.
`to-array-2d` is source-owned over nested static Seqable witnesses. It eagerly
maps each inner seqable to an array and realizes the outer array, preserving
ragged lengths, order, and single evaluation without the upstream JavaScript
preallocation loop or dynamic packing.
`rand` is source-owned with the pinned zero- and one-argument floating result.
Its first-class overload accepts a float bound, while direct calls inline to a
private specialization that also preserves the existing integer-bound API.
The exact Logseq scan finds one direct `rand` call and no `to-array-2d` calls.
`char` is blocked for the same first-class overload limitation: the upstream
one-argument function accepts either an integer code unit or a string. A
single-domain port would silently narrow ClojureScript compatibility.
`hash`, `compare`, `hash-ordered-coll`, and `hash-unordered-coll` are now
source-owned. `hashable<T>` carries a closed `T -> int` witness assembled from
LG's static scalar, collection, record, and `IHash` implementations;
`comparable<T>` carries a `T -> T -> int` witness and keeps both operands in one
concrete domain. The two collection functions preserve the pinned
ClojureScript accumulation loops and final Murmur3 mix. Only the minimal
private `__lg_hash` and `__lg_compare` elaboration ABI remains in the compiler,
and neither capability uses dynamic packing.
`make-array`, `aget`, `aset`, `atom`, and `volatile!` are also source-owned.
`array-index<T>` carries only a closed `T -> int` witness, preserving integer
and floating array indexes without an open numeric value. Array allocation,
reads, writes, and reference allocation remain private typed primitives.
`int-array`, `long-array`, `double-array`, and `object-array` are source-owned
with their pinned one- and two-argument control flow. Private source protocols
distinguish size inputs from homogeneous list, vector, sequence, and array
inputs, and distinguish scalar initial values from sequence initializers without
dynamic dispatch. The small generic runtime helper only performs bounded array
filling. Because OCaml arrays cannot contain JavaScript's uninitialized holes,
size-only numeric arrays use the corresponding Clojure zero value; shorter
numeric sequence initializers leave the remaining cells at that same zero value.
`object-array` uses nil for size-only allocation and for sequence padding, while
scalar two-argument initialization fills the array with the scalar value.
Sequence inputs retain their homogeneous static element type instead of being
coerced, matching the pinned ClojureScript behavior.
`ICloneable`, `-clone`, and `clone` are source-owned. The protocol's `:self`
result keeps each receiver and clone in one static type. LG supplies fresh,
equal list, vector, sequence, and hash-map implementations; the map clone shares
immutable HAMT nodes and insertion-order storage while allocating a new typed
map record, and a nonempty vector clone allocates only a new wrapper around the
immutable RRB trie. OCaml's empty list and empty vector are singleton values, so
their clone retains physical identity; this observable static adaptation is
recorded in the manifest. `cloneable?` is also source-owned: both direct and
first-class calls use an optional static `ICloneable` witness and return false
for non-implementing values without dynamic inspection.
Generated OCaml names encode a trailing bang as `_bang`, so source definitions
such as `volatile!` remain distinct from predicates such as `volatile?` and the
generated bindings stay readable. The manifest records the upstream arities
that still require dependent nested-array types or a closed validator-bearing
Atom domain.

The complete ClojureScript transient family is source-owned: `transient`,
`persistent!`, `conj!`, `assoc!`, `dissoc!`, `pop!`, and `disj!`. The public
algorithms dispatch through the upstream editable and transient protocols.
Static inline expansion preserves heterogeneous variadic map pairs without
introducing dynamic storage; the runtime keeps precise mutable vector, map,
and set representations, persists maps into the default HAMT, and rejects use
after persistence.

The map access and update family is also source-owned: `get`, `get-in`,
`assoc-in`, `update`, `update-in`, `select-keys`, `merge`, and `vals`. Their
public definitions follow the pinned ClojureScript lookup, recursive path,
left-to-right merge, and projection algorithms. Inline expansion uses private
static primitives where result types depend on a record field, collection
element, callback, or path. Keyword access and nested update expansion use the
same private forms, so precompiled namespaces retain precise record and map
types without introducing `Runtime_dynamic` values.

The Logseq/DataScript compatibility helpers `weak-deref` and `weak-clear!` are
ordinary precompiled `clojure.core` source functions with explicit
`weak<value>` signatures. They call the shared runtime weak-reference API;
only `weak-ref` remains compiler-owned because its public contract must reject
immediate values that cannot be held by the target weak-reference mechanism.
`future-call` is likewise a precompiled source wrapper over the typed future
runtime and preserves its zero-argument callback result type. The source
`enable-console-print!` compatibility function remains a no-op because LG
printing is already selected by the Native or Melange runtime.

When the optional ClojureScript checkout is supplied, its `HEAD` must match the
commit in `stdlib/upstream.edn`. The Logseq repository and commit are pinned in
the same manifest. `stdlib/logseq-dependencies.tsv` is the checked machine
report for that exact tree; regenerate it with:

```sh
script/generate_logseq_dependency_report.sh . /path/to/logseq \
  > /tmp/logseq-dependencies.tsv
diff -u stdlib/logseq-dependencies.tsv /tmp/logseq-dependencies.tsv
```

The generator rejects a checkout at any other commit. CI verifies that report
metadata matches the manifest, that no observed dependency is unexplained, and
that every blocked, host, or out-of-scope row has a concrete reason.
`logseq-namespace-status` and `logseq-qualified-var-status` rows classify
each observed dependency as `source-aggregate`, `source-core-alias`,
`blocked-static-typing`, `out-of-scope`, or `unsupported`; the final field is a
machine-readable reason. The scanner reads Clojure forms instead of matching
raw text, so comments, docstrings, and URLs cannot become false qualified-var
dependencies. The pinned Logseq checkout has no unexplained `unsupported`
rows. This keeps test and build-time libraries distinct from source namespaces
that LG already provides.

The generator pins the reviewed compiler dispatch count and fails when that
surface changes. Entries are classified as `source-shadowed`,
`blocked-static-typing`, `special-form`, `typed-primitive`, or `host-boundary`.
The call elaborator contributes 243 reviewed routes. A separate OCaml-AST
extractor now audits 155 form-head pattern routes in expression elaboration and
type inference; this closes the former gap where `case`, `condp`, `dotimes`,
`fn`, `for`, `let`, and `loop` were compiler-owned but appeared as unclassified
deferred upstream vars. The private `__lg_doseq` route is classified separately
as macro control-flow elaboration. Both counts are pinned, so adding a new
name-based form path requires an explicit inventory review.
The form inventory also joins against the manifest's source-owned core vars.
It rejects any migrated public var that reappears in expression elaboration or
type inference. The legacy `first`, `next`, `rest`, `seq`, `some`, and `juxt`
routes have been removed; source inline expansion and aggregate `.lgi`
signatures now reach only their private typed primitives. Every retained call
and form route has an item-specific boundary reason, and private `__lg_*`
operations cannot be classified as generic host interop.
Every `blocked-static-typing` entry must have a concrete, machine-checked
reason; the inventory test rejects the former catch-all blocker description.
Completion requires reducing `source-shadowed` to zero by removing its legacy
name-based compiler fallback, while resolving each static-typing blocker as the
language gains the required capability, variadic, or higher-order relation.
The current 243-name compiler dispatch inventory has zero `source-shadowed`
entries: the source definitions of `identity`, `complement`, `boolean`, `truth_`,
`int-rotate-left`, `imul`, `m3-mix-K1`, `m3-mix-H1`, `m3-fmix`,
`m3-hash-int`, `m3-hash-unencoded-chars`, `hash-string*`,
`mix-collection-hash`, `even?`, `odd?`, `every?`, `ffirst`,
`fnext`, `nfirst`, `nnext`,
`not`, the call-site-specialized `nil?`, `true?`, `false?`, `int?`, `number?`,
`string?`, `keyword?`, `symbol?`, `vector?`, `list?`, `seq?`, `set?`, `map?`,
`fn?`, `coll?`, `associative?`, `rational?`, `float?`, `double?`,
`sequential?`, `reversible?`, `sorted?`, `uuid?`, `delay?`, `force`, and
`ensure-reduced` source functions,
plus `some?`,
`boolean?`, `empty?`, `not-empty`, `integer?`,
`pos-int?`, `neg-int?`, `nat-int?`, `ident?`, `simple-ident?`,
`qualified-ident?`, `simple-symbol?`, `qualified-symbol?`, `simple-keyword?`,
`qualified-keyword?`, `counted?`, `seqable?`, and `doseq` source macros,
`reduced`, `reset-vals!`, `vary-meta`,
`inc`, `dec`, `max`, `min`, `bit-not`, `bit-and`, `unsafe-bit-and`, `bit-or`,
`bit-xor`, `bit-shift-left`, `bit-shift-right`, `not-any?`, `not-every?`, `split-at`, `split-with`, `nthnext`, `nthrest`, `bounded-count`, `butlast`, `take-last`, `drop-last`, `reverse`, `second`, `last`, `interpose`, `dedupe`, `distinct`, `zipmap`, `hash-combine`, `quot`, `rem`, `mod`, the `unchecked-*` integer arithmetic helpers, `rand`, `rand-int`, `rand-nth`, `to-array-2d`, `bit-shift-right-zero-fill`, `clojure.string/escape`,
`subs`, `int-to-string-radix`, `any?`, `ratio?`, `decimal?`, `realized?`, `range`, `shuffle`, `alength`, `aclone`, `acopy`,
`aslice`, `aconcat`, `array-to-seq`, `array-to-rseq`, `array-seq`, `to-array`,
`rseq`, `find`, `deref`, `reset!`, `swap!`, `swap-vals!`, `vreset!`, `vswap!`, `compare-and-set!`,
`empty`, `peek`, `pop`, `disj`, `list`, `vector`, `hash-map`, `array-map`,
`hash-set`, `set`,
`into-array`, the `array-values` source macro, `array-from`, `array-binary-search-left`, and
`array-binary-search-right`,
the upstream four-argument `amap` macro, the typed `asort!` extension,
`bit-and-not`, `unsigned-bit-shift-right`, the HAMT `mask`, `bitpos`, and
`caching-hash` source macros, `bit-count`, `comparator`,
`constantly`, `vec`, `max-key`, `min-key`, `frequencies`, `update-vals`,
`update-keys`, `replicate`, `key`, `val`, `parse-boolean`, `random-uuid`,
`parse-uuid`, `system-time`, `parse-long`, `parse-double`, `merge-with`, `NaN?`,
`gensym`, `infinite?`, `keyword-identical?`, `symbol-identical?`, `hash-long`,
`hash-double`, `hash-keyword`, `hash-string`, `add-to-string-hash-cache`,
`flush`, `array-index-of`,
`special-symbol?`, `distinct?`, `not=`, and the
derived bit functions, plus `splitv-at`, `iterate`, `tree-seq`, `partitionv`,
`areduce`, `locking`,
and the ClojureScript array-hint identity functions
`booleans`, `bytes`, `chars`, `shorts`, `ints`, `floats`, `doubles`, and
`longs`, have no legacy compiler fallback.

The source sequence batch preserves `iterate`'s memoized lazy successor
generation, `tree-seq`'s root-before-children depth-first order and guarded
child callback, and all three `partitionv` arities including overlap, omitted
short tails, and padding. Their typed unfold state avoids a public `lazy-seq`
compiler route. Overloaded calls now also unify repeated `seqable<T>` element
variables across arguments, so a padding collection cannot silently use a
different element type from the input collection.

### Tagged instants, ratios, and arbitrary-precision decimals

`#inst` literals are validated during compilation and lower to the closed
`Runtime_instant.t` representation. Display output is normalized to UTC and
readable output retains the `#inst` EDN tag. Invalid dates, offsets, or tagged
payloads fail before generated OCaml is emitted.

Native ratio literals lower to the closed `Runtime_ratio.t` representation
instead of being rounded to machine floats by the reader. The normalized
numerator/denominator pair preserves exact display, equality, `abs`, `inc`,
`dec`, and mixed ratio/integer arithmetic without dynamic storage. Melange
continues to follow ClojureScript's JavaScript numeric literal surface, where
the upstream reader does not expose JVM ratio literals.

Decimal `M` literals lower to the closed `Runtime_decimal.t` representation.
The runtime stores an arbitrary-length coefficient and decimal scale rather
than a machine float. Static decimal addition, subtraction, multiplication,
terminating division, equality, ordering, absolute value, and mixed int/float
adaptation are available on Native and Melange without `Runtime_dynamic`.
`with-precision` is a precompiled `clojure.core` source macro. It expands to a
private typed math-context thunk, supports `UP`, `DOWN`, `CEILING`, `FLOOR`,
`HALF_UP`, `HALF_DOWN`, `HALF_EVEN`, and `UNNECESSARY`, and restores the prior
context after normal return or exception. The compiler inventory records only
the private typed primitive and the `#inst` reader form; the public
`with-precision` name remains source-owned.

`eduction` is source-owned as a public macro over the existing typed
`->Eduction` constructor. It preserves the upstream `xform*` then final
collection call shape and composes transducers left-to-right before constructing
the typed transformer sequence. A first-class variadic `eduction` function value
is still not representable by LG's current static function type syntax, so the
adaptation is recorded explicitly in `stdlib/upstream.edn`.

The source array and host-stub batch adds the pinned `areduce`, `locking`,
`flush`, and `add-to-string-hash-cache` definitions. `areduce` evaluates its
array expression once and retains the upstream indexed accumulator loop.
`locking` intentionally does not evaluate its lock expression on JavaScript;
an explicit leading `nil` also preserves the empty-body result under LG's
non-empty `do` validation. `flush` remains the upstream zero-argument `nil`
stub, and `add-to-string-hash-cache` returns the same string hash while omitting
only the unobservable mutable JavaScript cache.

The collection constructor family now follows the same boundary. Public
`list`, `vector`, `hash-map`, `array-map`, `hash-set`, and `set` bindings live
in `clojure.core` source and remain usable through aliases, refers, qualified
calls, and first-class values. Their inline definitions lower direct calls to
internal `__lg_*` primitives so static element, key, and value types are
retained without `Runtime_dynamic.t`. Map constructors preserve
ClojureScript's odd-keyval rejection and last-value-wins behavior. This moves
11 independently inventoried function and inline-macro surfaces from typed
primitive to source ownership.

The `cljs.reader` source namespace owns the complete mutable tag-parser
registry surface: `register-tag-parser!`, `deregister-tag-parser!`,
`register-default-tag-parser!`, and `deregister-default-tag-parser!`. The
source wrappers preserve previous-parser return values and use closed
`Lg_edn_backend.t` callbacks. Explicit adapters convert public tag symbols to
runtime string keys and restore symbols before default callbacks; host
assignability remains strict. `read-string` applies specific parsers before
the default parser and preserves an unknown tagged value when neither exists.
Neither `cljs.reader` nor `clojure.edn` remains routed through
`Core_namespaces`; only the private typed EDN parsing primitive stays in the
compiler/runtime boundary.
The current `read-string` source wrapper supports the ordinary one-argument
entry point. Its upstream options-map arity and the stream-oriented `read`
overloads remain explicit blockers until reader/default/eof options have a
closed static source domain. `parse-and-validate-timestamp` now preserves the
pinned ClojureScript defaults, leap-year rules, fractional-millisecond
normalization, range errors, and offset calculation in source. A narrow typed
runtime matcher exposes only `option<list<option<string>>>` captures for the
fixed upstream timestamp regex; no dynamic capture vector crosses the
boundary. `parse-timestamp` remains a JavaScript `Date` boundary.

At the current checkpoint, the pinned Logseq tree requires
`clojure.string` 416 times,
`clojure.set` 75 times, `clojure.walk` 33 times, `clojure.edn` 27 times,
`cljs.reader` 28 times, and `clojure.data` 6 times. It also requires
`cljs.test` in 287 files and `cljs.pprint` in 15. Observed `cljs.test/report`
uses in custom `defmethod` reporters now resolve through the aggregate source
namespace and a documented report-event multimethod boundary. This keeps the
remaining
reader boundaries visible instead of treating `clojure.set` as the
scope of the standard-library migration. The same scan finds 677 `some?` and
44 `boolean?` occurrences. It also finds 537 `empty?`, 77 `integer?`, one
`counted?`, and one `seqable?` occurrence. These now resolve through source
macros for automatic
core refer, `cljs.core` alias/refer, and qualified `clojure.core` calls while
preserving single evaluation and nullable flow narrowing. The source
`seqable?` also restores the upstream `nil`, map, and static-array cases;
`counted?` uses the existing static `ICounted` registry and recognizes the
persistent hash-map protocol receiver.
The same scan finds 115 `not-empty` occurrences. Its source macro preserves the
upstream `seq` guard while specializing the nullable result to the concrete
input collection type, including metadata-bearing persistent maps, so this is
no longer classified as a dependent-result blocker.
The Logseq scan finds 18 uses of the integer sign predicates and 55 uses of the
simple/qualified identifier-family predicates. These now follow the upstream
guarded source control flow. LG narrows `int?`, `keyword?`, and `symbol?`
branches through internal typed helpers so the guarded branch remains readable
source without restoring public-name dispatch; these two newly needed helper
routes explain why removing nine public routes reduces the raw dispatch count
by seven. The identifier family is also available as source functions with
static symbol or keyword signatures. The heterogeneous `ident?` variants use a
keyword runtime instance and retain keyword-or-symbol behavior in their inline
direct-call definitions, avoiding an implicit union or dynamic adapter.
`counted?`, `seqable?`, `empty?`, and `not-empty` provide polymorphic vector
function instances and inline direct calls across the broader supported
collection domains. Direct `not-empty` retains its input collection type while
the first-class vector instance returns an optional vector.
`ex-message` is a source function over a minimal internal
`exn -> option<string>` primitive and works as a normal static function value.
`ex-cause` is likewise a source function over `exn -> option<exn>`.
`Exception_info` stores that closed optional cause directly, and `ex-info`
supports the pinned upstream two- and three-argument arities without widening
its existing exception-only data boundary.
`ex-data` is source-owned and returns the documented exception-only open data
payload. Non-empty EDN-like literal maps passed to `ex-info` are built directly
inside that payload boundary. Local static scalar values used in those literal
maps cross through an internal `exception-data<T>` capability carried only by
the exception-data path, so ordinary records and collections still do not gain
a global dynamic conversion path.
`*exec-tap-fn*`, `add-tap`, `remove-tap`, and `tap>` are source-owned core
functions backed by a documented tap runtime boundary. The boundary stores tap
callbacks as `Runtime_dynamic.t -> unit` and converts each direct `tap>` call
from its static source type at the call site; this is intentionally narrow and
does not expose a source-level dynamic type or permit dynamic collections.
Native and Melange execute the supplied tap thunk synchronously and return
`true`, rather than using ClojureScript's default JavaScript `setTimeout`
scheduler. `remove-tap` matches ordinary symbol/qualified-symbol callbacks by a
stable source identity; anonymous callbacks can be added, but, as upstream
requires, callers must retain the same callback identity to remove them.
`defmulti` and `defmethod` are source-visible core macros backed by a documented
runtime multifn boundary. The boundary stores dispatch values, method-table
keys, and call arguments as `Runtime_dynamic.t` because ClojureScript
multimethod dispatch is intentionally open; method bodies themselves are still
compiled as ordinary LG static functions. `methods`, `get-method`, and
`dispatch-fn` are source-visible call-site macros for symbol-based
introspection. `methods` returns the dynamic method table map, while
`get-method` and `dispatch-fn` return nil-or-handle dynamic values; these
handles are introspection results, not a general dynamic function-call escape
hatch. This boundary is separate from DataScript and ordinary collection
representations.
`js-obj` and `js->clj` are explicit JavaScript host boundaries: neither has a
portable Native representation, and treating them as ordinary maps would lose
JavaScript identity and interop semantics.
`re-pattern` is a first-class source `string -> regex` function with inline
specialization that preserves the upstream identity case for a regex argument.
The internal constructor validates the pattern once and retains the pinned
ClojureScript `(?flags)` prefix behavior. Melange passes those flags to
JavaScript `RegExp`; Native maps `i`, `m`, and `s` to the OCaml Re backend,
ignores the unobservable match-indices flag `d`, and uses the backend's string
semantics for `u`. The unsupported JavaScript `x` flag remains an error.
`re-find`, `re-matches`, and `re-seq` are source-owned and can be required, referred,
aliased, or used via automatic core refer. Their first-class sidecar type
supports the common no-capture `regex -> string -> option<string>` case.
Direct/open match results use the existing narrow
`Runtime_dynamic.regex_match` boundary because ClojureScript returns either a
string, a capture vector with optional elements, or nil depending on the regex
shape. The internal `__lg_re-find` and `__lg_re-matches` primitives record that
boundary explicitly and also provide static optional string/vector
specializations when the expected type is known. `re-seq` uses
`__lg_re-seq` plus `Runtime_dynamic.regex_match_sequence` for the same open
match shape across successive, non-overlapping matches, including nil for no
match and zero-width progress. Generated ML delegates matching and flag handling to
named `Runtime_string.regex_*` helpers instead of emitting target-specific
regex state machines at each call site.
`completing` is a pure source higher-order function with no runtime primitive.
Its sidecar signature retains the reducing callback's zero- and two-argument
relations and the returned function's zero-, one-, and two-argument result
types. The one-argument public clause emits the same identity completion
directly instead of recursively invoking the two-argument clause, because LG's
rigid overload parameters cannot re-instantiate `identity` across that internal
cross-arity call. Observable callback order and results remain the pinned
ClojureScript behavior.
The broader public static predicate family is defined as ClojureScript-style
source functions with source inline definitions. The runtime function bodies
have concrete static signatures, including polymorphic collection element
types where applicable, so compatible higher-order uses remain ordinary vars.
Most direct calls expand to non-public `__lg_*-predicate` primitives for
call-site type tests and guard narrowing. `map?`, `vector?`, `set?`, `coll?`,
`associative?`, and `reversible?` have moved fully to the ClojureScript protocol
model: their source functions and inline specializations use `satisfies? IMap`,
`satisfies? IVector`, `satisfies? ISet`, `satisfies? ICollection`,
`satisfies? IAssociative`, and `satisfies? IReversible`, with no
predicate-specific name dispatch. Predicate
arguments are evaluated exactly once even when the result is statically known.
`ifn?` uses the same source-wrapper boundary with a private static callable
predicate. It recognizes functions, `IFn/-invoke` nominal deftypes, keywords,
symbols, vectors, maps, and sets without inspecting a runtime type tag or
packing the value dynamically. Its materialized fallback has the same strict
function signature as `fn?`; direct calls and ordinary aliases retain the
broader inline callable test.
Persistent HAMTs and `defrecord` values participate in `map?` through `IMap`;
RRB vectors and custom implementations participate in `vector?` through
`IVector/-assoc-n`. Persistent HAMTs, RRB vectors, structural maps, source
`defrecord` values, and custom implementations participate in `associative?`.
Lists, sequences, RRB vectors, sets, persistent HAMTs, structural maps, source
`defrecord` values, and custom implementations participate in `coll?`.
Static sets and custom implementations participate in `set?` through `ISet`;
`ISet/-disjoin` selects the generated set module for the element type.
RRB vectors and custom implementations participate in `reversible?` through
`IReversible`; strings and lists no longer receive the former LG-only result.
The Logseq scan finds 166 direct `coll?` occurrences and 47 direct `sorted?`
occurrences.
`zero?`, `pos?`, and `neg?` now pair source functions for static first-class
integer use with source inline macros for direct int/float specialization.
`char?`, `identical?`, `array?`, the LG `array-value?` extension, and `reduced?`
are source functions with concrete or polymorphic static signatures and source
inline specialization. The same function/inline boundary now covers `some?`,
`boolean?`, `integer?`, `pos-int?`, `neg-int?`, and `nat-int?`, preserving
compatible higher-order use without dynamic adapters. `abs` likewise pairs a
source integer function with a typed direct-call macro so float
negative zero, NaN, and infinity retain the pinned ClojureScript `Math.abs`
behavior. These additions brought the raw dispatch inventory to 237: the public
routes are replaced by internal routes, while `char?` and `abs` add two minimal
static primitives for previously deferred source vars. LG's distinct `char`
type is an explicit adaptation from ClojureScript's one-character JavaScript
string representation. The Logseq scan finds 163 direct `zero?`, 160 direct
`pos?`, 42 direct `neg?`, 42 direct `identical?`, 23 direct `array?`, and 6
direct `abs` calls.
`ex-message` and `ex-cause` retain minimal typed `exn -> option<string>` and
`exn -> option<exn>` routes behind their public source functions. `re-pattern`
retains a validated static regex constructor while the public var remains
source-defined. After removing the `map?`, `vector?`, `set?`, `coll?`,
`associative?`, `reversible?`, `indexed?`, `sequential?`, and `sorted?` name
routes, plus the public `rseq`, `find`, `deref`, `reset!`,
`swap!`, `compare-and-set!`, `vreset!`, `vswap!`, `empty`, `peek`, `pop`, and
`disj` routes, plus the formerly compiler-owned `every-pred` and `some-fn`
routes, the raw compiler-call inventory contains 197 names. The five additional
routes are private `__lg_uuid-predicate`, `__lg_delay-predicate`, `__lg_force`,
`__lg_ensure-reduced`, and `__lg_rand` static specialization primitives; none
of their public source functions is compiler-dispatched.
`uuid?` follows the pinned ClojureScript `IUUID` predicate through a nominal
`Lg_runtime.Runtime_uuid.t` shared by Native and Melange, so an ordinary string
does not become a UUID on the JavaScript target. `delay?` follows the upstream
`Delay` instance predicate through `Lazy.t<a>`. Their public source functions
remain first-class at those concrete static types, while direct inline calls
accept any known static type, evaluate it once, and return false for a
nonmatching type. The exact Logseq scan finds 190 `uuid?` calls in 70 source
files and 2 `delay?` calls in one source file.
The reference functions delegate through the pinned ClojureScript `IDeref` and
`IReset` protocol shape; static implementations cover refs, lazy values,
futures, and slots without dynamic packing. `get-validator` and
`set-validator!` are source functions over a typed reference-validator
primitive. The stored callback is `value -> bool`, clearing uses `nil`, and
`reset!`/`swap!` validate before mutating or notifying watches.
References also implement `IMeta/-meta`; `reset-meta!` writes closed
`Lg_edn_backend.t` metadata and returns the stored metadata, while
`alter-meta!` reads the current metadata once, invokes the update function with
the pinned ClojureScript arity shape, and stores the returned closed metadata.
Plain heterogeneous source maps are not silently erased into metadata through a
dynamic conversion; callers that compute metadata in helpers should return the
closed EDN metadata value explicitly.
`compare-and-set!` preserves the upstream deref/equality/reset control flow and
evaluates its arguments once.
`swap!` preserves all pinned ClojureScript arities as a first-class source
function through `ISwap`. Direct calls use a private contextual specialization
so overloaded update functions retain their static arity information; the
reference expression is still evaluated exactly once. Applying a known
variadic callback now lowers directly instead of emitting a redundant wildcard
match, keeping the generated ML readable.
`swap-vals!` reuses the same source/protocol structure for all four upstream
arities and returns a homogeneous old/new vector. Its direct-call inline form
binds the reference once before dereferencing and invoking the private static
swap specialization, including for custom `IDeref`/`ISwap` receivers. The
first-class signature remains parameterized over ordinary LG references; custom
receivers use direct protocol-specialized calls.
The volatile reference family delegates through `IVolatile`: `vreset!` is an
ordinary source function and `vswap!` retains the upstream variadic source
macro expansion through `IVolatile/-vreset!` and `IDeref/-deref`.
`rseq` and `find` inline their pinned ClojureScript protocol calls while
retaining ordinary source definitions;
generic source functions infer the corresponding protocol witness without
dynamic packing.
The collection lifecycle family now follows the same boundary. Public source
functions and inline forms delegate to ClojureScript's
`IEmptyableCollection`, `IStack`, and `ISet`. Static implementations cover
lists, vectors, strings, sets, metadata-preserving hash maps, and explicit
user protocol implementations. The `disj` inline form nests protocol calls in
left-to-right argument order and binds the collection once. LG's static API
continues to reject nil collection receivers, and `peek` retains the existing
nonempty collection result type because a value-dependent nullable result is
not yet expressible as one generic source signature.
`count` and `nth` now follow ClojureScript's `ICounted` and `IIndexed`
protocols as public source functions. Their inline definitions retain direct
static specialization for nil, lists, vectors, sets, maps, strings, arrays,
and indexed defaults. Named protocol functions used as collection callbacks
are contextualized into concrete closures, so higher-order uses such as
`(map count)` carry the correct static witness without dynamic storage.
The first chunking batch ports `IChunk/-drop-first`, `array-chunk`,
`chunk-buffer`, `chunk-append`, and `chunk`. `ArrayChunk` is a parameterized
array-backed source record implementing `ICounted`, `IIndexed`, `IChunk`, and
`IReduce`; the mutable builder uses a minimal parameterized OCaml buffer with
capacity checks, one-time freeze semantics, and no dynamic storage. Custom
`IReduce` receivers can now be
reduced directly without also pretending to be seqable. Small typed helpers
keep arithmetic outside protocol method declarations, avoiding false
dependency edges in the current aggregate source scheduler.
The numeric coercions `int`, `long`, `double`, `unchecked-int`, and
`unchecked-long` are source functions with inline source specialization over
internal typed primitives. The unchecked pair deliberately shares the
`__lg_long` truncation boundary: pinned ClojureScript implements both with
`fix`, rather than the `int` function's `bit-or` source operation. This keeps
the distinction explicit within LG's current static integer domain. `byte` and
`float` preserve the pinned ClojureScript identity function and inline macro.
This keeps higher-order calls and namespace exports in the aggregate stdlib
while removing the public conversion names from compiler dispatch. The Logseq
scan finds 21 direct `int`, 9 direct `long`, and 6 direct `double` calls.
The public `unchecked-max` and `unchecked-min` macros are also pinned source:
their two-argument clauses bind both inputs exactly once before comparing, and
their variadic clauses preserve upstream nesting through the existing static
`max` and `min` operations.
Public `max` and `min` are now pinned source functions with one-, two-, and
variadic arities. A private static `INumericExtrema` protocol keeps homogeneous
int and float values first-class, including higher-order reduction. Direct
calls inline to `__lg_max` or `__lg_min` only for mixed int/float widening; the
primitive binds every argument in source order before comparison, and the
shared float helper preserves ClojureScript NaN propagation and later-value
tie selection. Generated Native and Melange ML therefore remains statically
typed and does not depend on OCaml's evaluation order or `Stdlib.max`/`min`
primitive lowering.
The pinned internal-public `mask` and `bitpos` macros are source adaptations of
the ClojureScript HAMT bit arithmetic. They preserve the unsigned right shift,
five-bit index mask, and bit-position composition through LG's existing typed
32-bit operations, with each macro argument evaluated once.
The pinned `caching-hash` macro also remains source-identical: it validates the
mutable cache field symbol, returns a present cached integer without invoking
the hash function, and computes and stores a missing value once. Deftype method
bodies now expand source macros before mutable-field assignment rewriting, so a
macro-produced `set!` follows the same typed field path as a handwritten one.
Runtime `gensym` is now an overloaded source function with the pinned default
and explicit-prefix behavior and a typed integer atom counter. LG initializes
that internal counter eagerly because a global cannot change statically from
`nil` to `atom<int>`; macro expansion retains its separate compiler-time gensym
primitive, which is macro/compiler behavior rather than runtime public dispatch.
The public function and inline-macro surfaces of `truth_` now resolve from one
source definition. Its inline form delegates to `boolean`, substituting the
argument once while preserving ClojureScript's exact nil-and-false-only falsey
rule for static values; the macro evaluator keeps a separate compile-time
primitive for evaluating upstream macro bodies.
The Murmur3 integer chain used by ClojureScript's persistent HashMap is now
ordinary source: `int-rotate-left`, the portable 16-bit `imul` fallback,
`m3-mix-K1`, `m3-mix-H1`, `m3-fmix`, `m3-hash-int`, and
`mix-collection-hash`. Their operation order and signed 32-bit results match
the pinned source. Three small `Runtime_int` operations provide only the
representation boundary that LG's wider Native integer domain cannot express:
signed 32-bit coercion, masked signed left shift, and unsigned 32-bit right
shift. The public helpers remain first-class source vars and introduce no
dynamic values. The inventory extractor now sees `imul` inside its upstream
top-level feature-selection `if`, correcting the audited surface from 855 to
856 entries.
`m3-hash-unencoded-chars` and `hash-string*` continue that source chain with
the pinned UTF-16 code-unit algorithms. Native and Melange both convert LG's
UTF-8 byte-string representation to a code-unit array in linear time. This
explicit representation boundary keeps both source loops identical, handles
BMP and surrogate-pair characters, and avoids repeatedly scanning UTF-8 for
each code-unit index. The `hash-string*` recurrence also retains CLJS's
untruncated final JavaScript Number addition on Melange through the existing
typed int/float identity boundary; the next `imul` still performs the pinned
signed-32-bit coercion.
The pinned identity function/inline-macro definitions for `short`,
`unchecked-byte`, `unchecked-char`, `unchecked-short`, `unchecked-float`, and
`unchecked-double` are also pure source definitions. They need no compiler or
runtime ABI and preserve polymorphic first-class identity behavior.
The `comment`, `doto`, `when-first`, and `while` macros are now precompiled
source macros. `doto` preserves single evaluation and effect order;
`when-first` preserves one `seq` evaluation; and `while` uses an explicit
`if`/`do` tail solely to make LG's static `recur` validation see the same loop
edge as the upstream `when` expansion.
The imported Clojure control macros `if-not`, `when`, `when-not`, and `cond`
are source macros as well. Their public-name expression, inference,
loop-tail, and macro-evaluator cases have been removed. `cond` retains test
order and short-circuiting, validates even forms at expansion time, and stops
expanding after literal `true` or `:else` so unreachable heterogeneous tails
do not weaken static branch types. The Logseq scan finds 275 direct `if-not`,
5,130 direct `when`, 1,143 direct `when-not`, and 31 direct `cond` forms.
The public `and` and `or` vars are source macros backed by internal typed
logical primitives. This retains the pinned ClojureScript left-to-right
short-circuit and value-return behavior without exposing the public names to
expression elaboration, inference, or compile-time evaluation. Literal
truthy/falsey prefixes and unreachable tails are pruned before static branch
convergence, so `(and true value)` and `(or nil value)` retain `value`'s exact
type. The Logseq scan finds 3,778 direct `and` and 3,277 direct `or` forms.
The binding controls `if-let`, `when-let`, `if-some`, and `when-some` are
source macros over internal typed option-binding primitives. They preserve
single evaluation, destructuring, truthy-versus-non-nil selection, public
arities, and empty-body evaluation without leaving public-name compiler or
macro-evaluator cases. The Logseq scan finds 591 direct `if-let`, 1,706 direct
`when-let`, 9 direct `if-some`, and 6 direct `when-some` forms.
The foundational `->` and `->>` macros now use the pinned ClojureScript source
loop as well. Symbol, keyword, and call-form steps preserve order, and
step-form type-hint metadata round-trips through the compile-time `meta` and
`with-meta` primitives. Their former expression elaboration, inference, and
macro-evaluator branches have been removed. The Logseq scan finds 1,978 direct
`->` and 1,552 direct `->>` forms.
The `as->`, `cond->`, `cond->>`, `some->`, and `some->>` macros are also
precompiled source macros rather than public-name compiler cases. Their source
bodies preserve the pinned ClojureScript binding order, single evaluation, and
nil short-circuiting. LG uses finite `mapcat` binding construction in place of
the upstream `repeat`/`interleave` construction so macro expansion does not
materialize an infinite compile-time sequence. The Logseq scan finds 5 direct
`as->`, 584 direct `cond->`, 38 direct `cond->>`, 1,082 direct `some->`, and
148 direct `some->>` calls.
`indexed?` now uses a distinct public `IIndexed` protocol, so LG's internal
linear-list `Indexed` capability does not incorrectly claim the upstream
constant-time contract. `ISequential` is represented as a real zero-method
marker protocol with explicit per-type evidence, and `ISorted` preserves all
four upstream method arities for user-defined implementations. The source
predicates `ifind?`, `map-entry?`, `regexp?`, and `volatile?` now follow the
pinned ClojureScript capability checks. `IMapEntry` is a marker protocol backed
by LG's statically typed map-entry tuple receiver; regex testing uses one
private static type primitive. Because LG atoms and volatiles that support
`vreset!` share the same `ref<value>` representation, `volatile?` reports that
`IVolatile` capability rather than a distinct JavaScript constructor identity.
`true?` and
`false?` retain the minimal
internal typed identity primitive used by the pinned ClojureScript
implementation, but their public vars now come from the source standard
library. A repository-wide Logseq symbol scan
also finds 824 `random-uuid`, 46 `parse-uuid`, and 14 `system-time`
occurrences; those three core functions now resolve from the aggregate source
artifact, including `cljs.core` aliases and refers. The same scan finds 30
`parse-long` and 7 `parse-double` occurrences; both parsers now preserve the
upstream grammar and safe-number bounds through the same source artifact. The
same tree contains 14 direct `merge-with` references plus `apply merge-with`
call sites. Its source port preserves the upstream entry fold and left-to-right
combiner order. Because LG has no value-dependent return types, supplying only
`nil` map arguments yields an empty typed map rather than upstream nil; the
zero-map `(merge-with f)` arity still returns nil, and this static adaptation is
recorded in `stdlib/upstream.edn`. Logseq has 12 direct
`keyword-identical?` calls; the source implementation uses LG's statically
typed keyword equality, matching ClojureScript's fallback comparison of fully
qualified names without an open runtime type test. The matching symbol
function and the upstream float predicates, long hash combiner, and
special-symbol membership function are source-backed in the same batch.
`uuid?` is now source-owned over the shared nominal UUID representation, so
Native and Melange both distinguish UUID values from ordinary strings. The
public `uuid` constructor is likewise a first-class source function over the
closed UUID type. Its public name no longer remains in compiler call dispatch
and resolves through automatic core refer, aliases, and explicit refers.
`current-time-millis` remains a documented target host boundary: moving its
Unix-epoch implementation into the aggregate would force every Native
consumer to link Unix even when unused, while Melange cannot share that
dependency. `system-time` is not a substitute because it has monotonic/process
time semantics rather than Unix-epoch semantics.
The aggregate `clojure.set` source
namespace now provides `union`, `intersection`, `difference`, `subset?`,
`superset?`, `select`, `project`, `map-invert`, `rename-keys`, `rename`,
`index`, and both arities of `join`;
its 74 namespace references and all 221 observed qualified-var references
resolve through the source artifact. Projected, renamed, indexed, and joined
relations use a
typed map-set backend
whose persistent hash buckets use order-independent Clojure map hashes and
whose equality ignores map insertion order and metadata. This matches Clojure
map equality without dynamic packing or quadratic whole-relation scans.
`Runtime_map`-valued outer keys statically select the same hash/equality pair
for lookup, association, membership, removal, entry lookup, and outer-map
equality. The source `index` preserves ClojureScript's projection grouping and
set deduplication. `join` preserves its nonempty guards, smaller-relation index
selection, natural-key intersection, key-map inversion, scan order, and nested
merge reduction through small typed helpers. Generated fold binders use
reserved hygienic names, so a nested reduction cannot capture the outer row.
The same checkout also reports
`cljs.test` occurs 229 times and now participates in the aggregate source
library. Its source implementation includes `empty-env`, the current
environment lifecycle, report-counter updates, context rendering, `testing`,
`is`, `are`, `try-expr`, `deftest`, `run-test`, `run-tests`, `ns?`,
`use-fixtures`, `compose-fixtures`, `join-fixtures`, `successful?`, and
`report` and `do-report`. The
environment is a closed record backed by a statically typed dynamic binding;
`*current-env*` itself is inventoried as a source var rather than inheriting the
namespace's remaining async/report blocker.
counters use LG's default hashmap, and `testing` preserves upstream
push/body/finally/pop order.
This required general `try`/`finally` support, including finally-only forms and
exception propagation, and generates readable `Fun.protect` code without
`Runtime_dynamic`. The open reporter event payload is restricted to the
`cljs.test/report` primitive boundary, and `do-report` reuses that boundary as
a source var. Shared Native/Melange `do-report` does not synthesize the
JavaScript stack file/line enrichment performed by upstream `cljs.test`; that
host-only normalization is recorded in `stdlib/upstream.edn`. Reporter state,
counters, and current environment remain statically typed. The open polymorphic formatter field is
still recorded as an `empty-env` adaptation in `stdlib/upstream.edn`. The remaining runner and
assertion batch uses a homogeneous static synchronous-test registry in
definition order. Boolean assertions retain single evaluation and pass, fail,
and unexpected-error counting; `are` retains template order and validates its
argument cardinality. The current static batch intentionally rejects
non-boolean `is` forms. Synchronous `:once` and `:each` fixtures preserve
definition order for both wrapping functions and `:before`/`:after` maps.
They use a closed fixture variant and namespace registry, reject mixed fixture
representations, and run map cleanup through readable `finally` control flow.
The synchronous block batch also ports `run-block`, `test-var-block`,
`test-var`, `test-vars-block`, `test-vars`, and `testing-vars-str`. Static test
thunks remain homogeneous, execute left to right, and reuse the registered-test
counter path. A closed test-location record replaces the heterogeneous report
event map for file, line, and optional-column rendering. The next batch ports
`async`, `async?`, and `block` through a recursive closed `test-action` sum with
synchronous thunk, asynchronous CPS, and injected-block cases. `run-block`
preserves left-to-right execution, resumes at `done`, prepends injected blocks,
and warns without rerunning the continuation when `done` is called twice.
Direct outer `async` forms in `deftest` participate in the static registry and
delay subsequent tests until their continuation runs. Nested `testing` forms
use a deferred action so their context stack remains active until asynchronous
completion, including exception cleanup. Async tests combined with fixtures,
namespace hooks, analyzer-wide test discovery, and special assertion methods
remain explicit blockers rather than silently falling back to dynamic values.
`test-all-vars-block`, `test-all-vars`, `test-ns-block`,
`test-ns`, and `run-tests-block` now validate quoted namespace forms and compose
the same closed runner actions in upstream order. The static definition-order
namespace registry replaces analyzer Var metadata discovery; environment setup
and cleanup remain explicit actions so asynchronous continuation order is not
changed. Analyzer-discovered `test-ns-hook`, summary report events, and
`run-all-tests` namespace enumeration remain recorded blockers.
The chunked sequence surface now also declares `IChunkedSeq` and
`IChunkedNext` in source and provides `chunk-cons`, `chunk-first`, `chunk-rest`,
and `chunk-next`. A parameterized record stores an optional `ArrayChunk`, the
typed remaining sequence, and metadata. This closed representation preserves
the upstream empty-chunk content branch without a value-dependent return type,
while nonempty chunks retain first/rest/next protocol behavior. The
vector-trie-specific `chunked-seq` constructors remain blocked. The
`chunked-seq?` predicate is source-owned and uses an optional static
`IChunkedSeq` witness for first-class true and false calls. The source-owned
`implements?` macro expands to the same static `satisfies?` test, preserving
single evaluation without exposing ClojureScript's JavaScript protocol-mask
layout in the LG runtime.
Inventory reporting keeps that namespace-level blocker
while allowing each source-owned var to resolve as `source-aggregate`; a
blocked sibling does not downgrade a ported qualified var.
`clojure.test` occurs 51 times and is classified as a JVM-only host boundary.
`cljs.pprint` occurs 15 times. Its independent `float?` and `char-code` helpers
are now precompiled source definitions, using private static protocols and a
typed UTF-16 code-unit boundary. The upstream `getf` and `setf` state macros are
also source-owned and preserve their double-dereference lookup and
`swap!`/`assoc` update behavior. The formatter, writer, and dispatch-table
surface remains explicitly blocked because full logical-block layout,
right-margin state, and custom dispatch need a closed static pretty-writer
domain. `pprint` itself is source-owned for both
upstream arities: direct output and `Buffer.t` writer output use a private
typed readable-printer primitive and append the upstream newline. This removes
the public compiler dispatch and resolves Logseq's qualified `pprint` calls;
right-margin, logical-block, and custom-dispatch formatting remain recorded as
the namespace-level adaptation instead of being silently claimed. The upstream
`deftype` macro remains a
specific static blocker: it generates generic unannotated record fields plus a
public nominal constructor and predicate, which cannot be preserved by
introducing dynamic fields or changing that record API.
`clojure.pprint` occurs 14 times and is a JVM-only host boundary. All 28 public
`clojure.zip` vars are now precompiled source definitions. Its parameterized
closed location, path, and callback-context records replace upstream's
heterogeneous metadata vector without dynamic packing. Navigation, changed
propagation, rebuilding, depth-first traversal, and removal retain the pinned
ClojureScript control flow; sibling collections are normalized to typed vectors.
The unary `edit` arity used by Logseq is supported, while additional variadic
callback arguments remain explicitly recorded as a dependent-`apply` blocker.
All 35 observed Logseq qualified zipper calls now resolve through the aggregate
source artifact. `cljs.spec.alpha` and `clojure.spec.alpha`
are explicitly excluded from the LG stdlib port; their Logseq references remain visible in the
inventory but do not count against migration completion. The independent
`core.async` library is excluded as well, including the observed
`cljs.core.async`, `cljs.core.async.impl.channels`, `clojure.core.async`, and
`clojure.core.async.interop` namespaces. They remain visible as out-of-scope
Logseq dependencies and are not candidates for the LG source stdlib port.
Evidence-backed out-of-scope entries also cover JVM-only `clojure.java.io`,
`clojure.java.shell`, and `clojure.stacktrace`; JVM build tooling under
`clojure.tools.*`; compiler APIs under `cljs.analyzer*`; and the independent
`cljs.core.match`, `clojure.data.json`, and
`clojure.test.check.generators` libraries. These require separate pinned
upstreams or host integrations rather than being silently treated as missing
stdlib source. The aggregate now contains all seven public `clojure.walk`
functions over the closed `Lg_edn_backend.t` tree domain. Its source definitions preserve upstream
pre-order, post-order, map-entry, key-conversion, and replacement order while a
small typed runtime primitive rebuilds one collection level. The former
`Runtime_dynamic.t` implementation and compiler namespace route are gone.
`clojure.data/diff` now uses the same explicit closed domain: the source public
function delegates to a typed runtime port that preserves atom, map, set, and
sequential partitions, recursive nil placement, key membership, and upstream
result order. Its upstream `EqualityPartition/equality-partition` and
`Diff/diff-similar` protocols are source-owned and statically extensible.
Their default source implementations target `Lg_edn_backend.t`; a small typed
integer tag crosses the OCaml boundary for partition classification, while the
generated implementation returns real LG keyword literals and contains no
dynamic conversion. The closed-domain `diff` entry point intentionally does
not accept arbitrary source records. `clojure.string/split` is also
source-owned with both public
arities, regex captures, empty-regex handling, and positive, zero, and negative
limits. Consequently, `Core_namespaces` no longer routes any non-core public
namespace, and the old `Core_data`, `Core_string`, and dynamic data-diff
implementations have been removed.

The seven public hierarchy functions are source-owned with their pinned
explicit/global arities. A minimal closed `Lg_edn_backend.t` runtime primitive
stores the three parent/ancestor/descendant relations, maintains transitive
closure in upstream update order, rebuilds closure on `underive`, and rejects
self or cyclic derivations. Recursive vector `isa?` and global hierarchy
mutation are covered on Native and Melange. JavaScript constructor inheritance
is retained as an explicit host-only boundary rather than being simulated with
dynamic values. Closed EDN equality now uses Clojure set/map semantics instead
of OCaml representation order, which is required by hierarchy result sets.

The source core also includes ClojureScript's `key-test`, `equiv-map`, `reduceable?`,
`vector-lite`, `hash-map-lite`, and `set-lite`. `IReduce` is a source-facing
alias of LG's existing static `Reducible` protocol, including a typed map
reducer, so explicit implementations work through both names. Lite collections
use LG's default persistent vector, hash-map, and set representations because
their upstream runtime classes are explicitly internal; observable collection
behavior is retained. `hash-map-lite` rejects odd key/value input instead of
inserting an implicit `nil`, because that value would violate a homogeneous
static map value type. `equiv-map` preserves the upstream count guard and
first-mismatch short-circuit. Its typed `IFind` option replaces ClojureScript's
private `NeverEquiv` sentinel, while static record types remain outside the map
signature.

The ClojureScript `divide` macro is source-defined with its unary, binary, and
variadic arities. Ordered `let` bindings retain JavaScript's left-to-right
operand evaluation when the expansion is emitted as OCaml, while division
remains left-associated and delegates to LG's typed `/` primitive.

`cljs.cache` is now part of the Native and Melange aggregate source library,
using the `cljs-cache` 0.1.4 source used by Logseq as its behavioral reference.
`CacheProtocol`, `through`, `BasicCache`, `TTLCache`, `LRUCache`, and their
factories remain source-owned. The cache implementations are `deftype` values,
as generated by the upstream `defcache` macro, rather than records exposing
their implementation fields as map entries. Protocol methods are exported as
ordinary namespace Vars, so `:refer` and qualified calls behave like
ClojureScript while dispatch remains static. Functions such as `through`
retain the relationship between their cache parameter and result across
separately compiled namespaces.

The LRU implementation follows `tailrecursion/cljs-priority-map` rather than
substituting a vector. `Runtime_priority_map` keeps the same two indexes: an
ordered `priority -> item-set` map and a persistent hash `item -> priority` map.
Integer priorities use OCaml's ordered map and item buckets use LG's default
persistent hash map. This keeps minimum-priority lookup and priority updates
readable and logarithmic without dynamic values. New runtime boundaries use
ordinary OCaml `.mli` interfaces; `cljs.cache` itself needs no LG signature
sidecar.
TTL expiry and cleanup follow the upstream timestamp-table algorithm. The
cross-target source `system-time` function supplies the current timestamp to a
typed runtime state, so cache keys and values remain statically homogeneous.
The cache deftypes implement upstream `defcache` behavior for `ILookup`,
`IAssociative`, `IMap`, `ICounted`, `ICollection`, `IEquiv`,
`IEmptyableCollection`, and `ISeqable`. LG's statically typed map-entry tuple is
used at the `ICollection` boundary. The JavaScript-only `IIterable` method is a
documented host boundary because it has no shared Native/Melange iterator type.

The inventory explicitly classifies the audited clone, dependent-result,
transducer, tagged-literal, and metadata-transform surfaces. `clone` cannot be
replaced by identity because `identical?` observes the fresh collection objects
created by ClojureScript. `record?` is now source-owned through an `IRecord`
marker that only non-nominal `defrecord` types implicitly satisfy; ordinary
maps, nominal type records, and unrelated values return false through an
optional static witness. Tagged literals now use a parameterized nominal source
type, so each payload remains statically typed while direct `:tag` and `:form`
access, static `get`, structural equality, upstream hash composition, and
first-class `tagged-literal?` need no universal value. `replace` preserves its
transducer arity and representation-dependent
lazy or vector result through inline protocol dispatch; vector metadata is
not yet representable by LG's current static vector runtime and is recorded as
the remaining adaptation.
`spread` and `trampoline` each require a heterogeneous or dependent function
relationship that the current static source type system cannot express without
narrowing an upstream arity. `vec-lite` is source-owned as a one-argument
wrapper over the existing static `vec` realization boundary.

JavaScript loose equality and falsiness macros, Closure `Uri`, JavaScript
symbols and object constructors, CLJS `Var` and `Inst` values, analyzer-only
casts, and the host writer/newline functions are recorded as host boundaries.
Native implementations do not substitute Clojure truthiness or unrelated
nominal types for those target-specific behaviors.

`flatten` is source-owned and available through automatic core refer, explicit
`:refer`, `cljs.core` aliases, and qualified `clojure.core` calls. Direct calls
inline to the private `__lg_flatten` primitive, which preserves ClojureScript's
`sequential?` boundary instead of treating every `seqable?` value as nested:
vectors, lists, and seqs are flattened one homogeneous static layer, while
strings remain scalar values. Arbitrarily nested inputs whose leaves require a
heterogeneous recursive value domain still need an explicit closed source
variant before they can be represented statically.

`memoize` is source-owned for direct calls through a private typed ABI covering
zero through three fixed-arity functions. Unary calls use the argument as the
cache key; binary and ternary calls use typed OCaml tuple keys; zero-arity calls
store a single optional cached result. This keeps Logseq's ordinary
`(memoize (fn ...))` shape available through `require`, `alias`, and `refer`
without `Runtime_dynamic.t`. A faithful first-class variadic `memoize` function
value still cannot be represented without erasing heterogeneous argument tuples,
so that upstream gap is recorded as a static adaptation in
`stdlib/upstream.edn`.

`with-redefs` is source-owned as a macro that expands to the private
`__lg_with_redefs` typed var-root primitive. Replacement values are checked
against the root's static value type, the original root is restored after
normal completion or exceptions, and ordinary calls through the root read the
current typed function value. The current source adaptation rootizes ordinary
monomorphic source `defn` functions outside the compiler-owned stdlib and
DataScript implementation namespaces. This covers the common Logseq test
pattern of temporarily replacing functions through aliases without widening
those functions, arguments, or return values to `Runtime_dynamic.t`.

`apply` is source-owned and available through automatic core refer, explicit
`:refer`, `cljs.core` aliases, and qualified `clojure.core` calls. Its four
statically expressible fixed source arities are precompiled in the aggregate
stdlib. Direct calls of every upstream arity inline to the private
`__lg_apply` primitive, which preserves the dependent relationship between
fixed arguments, the final seqable argument, overloaded function arities, and
the result type. Core source helpers use that same private boundary during
bootstrap so their overloaded callbacks are not monomorphized through the
narrower first-class source signature. The pinned Logseq scan contains eight
qualified `clojure.core/apply` calls; they resolve through the aggregate source
namespace. The upstream five-fixed-plus-final-sequence first-class arity remains
documented as unrepresentable because its rest arguments are heterogeneous by
construction.

`mapcat` is source-owned with its transducer, single-collection, two-collection,
three-collection, and variadic collection arities. Multi-collection calls stop
at the shortest input and preserve callback argument order. The implementation
uses the same static `map`, `map-many-seq`, `concat`, and `apply` capabilities as
ordinary source consumers; callback inputs, seqable callback results, and
transducer accumulator types remain statically related. The upstream
`mapcat.cljc` namespace now reaches only deliberate negative tests whose
callbacks return non-seqable values, which the suite audit records as expected
compile-time errors on Native and Melange.

The architecture tests in `test/stdlib` enforce that `clojure.set` is no
longer classified as compiler-owned and that source-owned core functions have
no name-based call elaboration or inference path.

The inventory reports `clojure.core` and `cljs.core` as
`source-with-primitive-boundary`, matching the aggregate source artifact that
owns their public vars. It records their compiler bootstrap role separately as
`namespace-bootstrap ... automatic-core-refer`. `Core_namespaces` therefore
identifies the two automatic-refer namespace names, but supplies no public
bindings or qualified-member implementations. This distinction prevents the
automatic core environment rule from being mistaken for compiler ownership of
the standard-library namespace.

Namespace ownership rows are generated from `stdlib/upstream.edn`, not from a
second namespace list in the audit script. Every manifest namespace therefore
appears in the inventory, and `cljs.core` is added as the documented automatic
alias of the `clojure.core` implementation. The manifest's explicit
`:primitive-boundary` marker distinguishes pure aggregate source namespaces
from source namespaces that call a typed or host primitive. CI compares the
manifest and inventory namespace counts, so adding `cljs.math`, `cljs.cache`,
`clojure.core.protocols`, or a future aggregate namespace cannot silently omit
it from the audit.

Ownership and support completeness are separate inventory dimensions.
`namespace` rows say whether an implementation is precompiled source or only
represented in the manifest. `namespace-status` rows carry the manifest's
support classification and concrete reason. Thus partially implemented
`cljs.test` and `cljs.pprint` retain source ownership while exposing their
remaining static blockers, and every `manifest-only` namespace must have a CI
checked `blocked-static-typing`, `host-boundary`, or `out-of-scope` reason.
