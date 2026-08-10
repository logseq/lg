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

Consumers must not enumerate individual stdlib `.mil` or `.cljc` files.
`test/stdlib` exercises this contract, including negative type tests restored
from the same aggregate state.

Multi-file compilation prepares each source once. The parsed forms provide
both required OCaml packages and the subsequent incremental compilation input;
the CLI does not parse the same file again after restoring a compiler state.
State-producing compilation does not read or write prefix caches: its explicit
state artifact is already the reusable checkpoint. The cold-build gate covers
the full aggregate-stdlib plus DataScript path and enforces a fixed 10-second
limit. The limit cannot be relaxed through configuration, and the gate removes
cache-related environment settings before building from a clean tree.

Generic `set<element>` source functions use LG's statically typed generic set
representation internally. Calls from concrete persistent set modules convert
through typed `elements` and `of_list` operations at that source-function
boundary, and results convert back to the statically selected concrete module.
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
vars, runtime primitive boundaries referenced directly or through stdlib
namespace aliases, and Logseq standard-library
namespace/qualified-var usage. The Logseq reader resolves aliases from each
file's `ns` form and respects `.gitignore`; qualified-var counts are lexical
occurrences after alias resolution, so they are a prioritization signal rather
than a reachability analysis.

When a pinned ClojureScript checkout is supplied, the report also contains an
`upstream-var` row for every public function, macro, and protocol method read from the reviewed
core and namespace sources. The extractor evaluates both Clojure and
ClojureScript reader-conditional branches, handles tagged JavaScript literals,
recurses through top-level `if` branches, and excludes private definitions. The
pinned surface currently contains 985 function, macro, and protocol-method
entries, including 753 entries in `cljs.core`. Public methods declared by
`defprotocol` are inventoried independently instead of being hidden behind the
protocol var. Each row is
classified independently as source, typed primitive, special form, host
boundary, static-typing blocker, out of scope, or deferred. A namespace's
aggregate support does not make a missing public var appear supported. Manifest entries for
`clojure.core` also classify the corresponding `cljs.core` function and inline
macro surfaces. The current baseline is 565 source entries (57.36%), 28 typed
primitives, 43 special forms, 97 host boundaries, 201 static-typing blockers,
51 out-of-scope entries, and zero deferred entries. Source coverage only counts
real precompiled LG definitions; classifying a boundary does not inflate the
percentage.
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
`list*` remains explicitly blocked where its complete variadic first-class
upstream type cannot be expressed without dynamic typing; direct compiler
support is not counted as a source port.
All pinned public `cljs.core` macros now have explicit ownership and zero remain
deferred. Compiler/analyzer declarations and namespace-environment operations
are special forms; JavaScript syntax and host-object macros are host boundaries;
multimethod, per-object protocol extension, and dynamic root-rebinding macros
carry concrete static blockers. These classifications do not count as source
coverage.
All remaining public `cljs.core` function surfaces are also explicitly
classified. The non-source families are JavaScript iterators and prototype
inspection, chunked-sequence internals, multimethods, reference watches and
validators, heterogeneous printing, sorted collections, bootstrap namespace
objects, and analyzer helpers. Each individual var
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
also live in `clojure/core.cljc`. Their 38 public methods are source-owned; the
compiler registry retains only the receiver-specific typed implementations for
built-in lists, vectors, hash maps, sets, references, and host-backed
collections. A source declaration is checked against that implementation
surface and must preserve every supported method and fixed arity. Qualified
`cljs.core` aliases resolve these declarations to the same root protocol IDs,
so consumers do not get a parallel compatibility protocol. The statically
adapted `IReduce` surface exposes the explicit-initial-value arity, and `ISwap`
uses a unary typed updater after the public wrapper captures extra arguments;
both differences are recorded in `stdlib/upstream.edn`.

The complete pinned protocol-method surface now has no deferred entries: 6
methods are source-defined, 34 are typed primitives, 32 have concrete static
typing blockers, 7 are explicit host boundaries, and 7 are out of scope.
The typed primitives are the compiler-registered static protocol ABI used by
built-in collection, reference, metadata, comparison, and transient receiver
implementations. Future source migration can replace those entries protocol
family by protocol family.
`ensure-reduced` is explicitly blocked because its same-arity return type is
dependent on whether the input is already `Reduced<T>`; representing that
contract as a normal generic function would incorrectly nest the wrapper.
`delay?` and `force` remain blocked rather than using direct-call-only inline
type tests. Their pinned ClojureScript functions are first-class: `delay?`
accepts every value type, while `force` returns either a delay's payload or the
unchanged non-delay input. LG cannot yet express those relationships through
one static source function without a typed instance/forceable capability.
`keep-indexed`, `take-nth`, `random-sample`, `partition-all`, and
`partitionv-all` now share the source lazy-sequence and reducing-function
foundation, so their collection and stateful transducer arities are both
source-defined.
The existing compiler-owned `doseq` expansion remains a visible blocker rather
than counting as supported: it rejects the upstream `:while` binding modifier
and cannot yet preserve termination of the current nested loop when `:while`
follows a `:let` modifier.
`to-array-2d` is blocked as a whole rather than restricted to vectors. Its
upstream input is a seqable whose elements are themselves seqable; LG currently
loses each inner value's static sequence witness when that nested capability is
passed through the array-conversion callback. A valid source port must retain
that witness without dynamic packing.
`rand` remains compiler-owned because its one-argument public API accepts both
int and float bounds, while source signatures cannot yet express same-arity
overloads without rejecting one of those existing cases.
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
commit in `stdlib/upstream.edn`. The inventory also records the Logseq checkout
commit. `logseq-namespace-status` and `logseq-qualified-var-status` rows classify
each observed dependency as `source-aggregate`, `source-core-alias`,
`blocked-static-typing`, `out-of-scope`, or `unsupported`; the final field is a
machine-readable reason. This makes unsupported namespaces visible without confusing test and
build-time libraries with source namespaces that LG already provides.

The generator pins the reviewed compiler dispatch count and fails when that
surface changes. Entries are classified as `source-shadowed`,
`blocked-static-typing`, `special-form`, `typed-primitive`, or `host-boundary`.
The call elaborator contributes 197 reviewed routes. A separate OCaml-AST
extractor now audits 130 form-head pattern routes in expression elaboration and
type inference; this closes the former gap where `case`, `condp`, `doseq`,
`dotimes`, `fn`, `for`, `let`, and `loop` were compiler-owned but appeared as
unclassified deferred upstream vars. Both counts are pinned, so adding a new
name-based form path requires an explicit inventory review.
Every `blocked-static-typing` entry must have a concrete, machine-checked
reason; the inventory test rejects the former catch-all blocker description.
Completion requires reducing `source-shadowed` to zero by removing its legacy
name-based compiler fallback, while resolving each static-typing blocker as the
language gains the required capability, variadic, or higher-order relation.
The current 197-name compiler dispatch inventory has zero `source-shadowed`
entries: the source definitions of `identity`, `complement`, `boolean`, `truth_`,
`int-rotate-left`, `imul`, `m3-mix-K1`, `m3-mix-H1`, `m3-fmix`,
`m3-hash-int`, `m3-hash-unencoded-chars`, `hash-string*`,
`mix-collection-hash`, `even?`, `odd?`, `every?`, `ffirst`,
`fnext`, `nfirst`, `nnext`,
`not`, the call-site-specialized `nil?`, `true?`, `false?`, `int?`, `number?`,
`string?`, `keyword?`, `symbol?`, `vector?`, `list?`, `seq?`, `set?`, `map?`,
`fn?`, `coll?`, `associative?`, `rational?`, `float?`, `double?`,
`sequential?`, `reversible?`, and `sorted?` source functions, plus `some?`,
`boolean?`, `empty?`, `not-empty`, `integer?`,
`pos-int?`, `neg-int?`, `nat-int?`, `ident?`, `simple-ident?`,
`qualified-ident?`, `simple-symbol?`, `qualified-symbol?`, `simple-keyword?`,
`qualified-keyword?`, `counted?`, and `seqable?` source macros, `reduced`, `reset-vals!`,
`inc`, `dec`, `bit-not`, `bit-and`, `bit-or`,
`bit-xor`, `bit-shift-left`, `bit-shift-right`, `not-any?`, `not-every?`, `split-at`, `split-with`, `nthnext`, `nthrest`, `bounded-count`, `butlast`, `take-last`, `drop-last`, `reverse`, `second`, `last`, `interpose`, `dedupe`, `distinct`, `zipmap`, `hash-combine`, `quot`, `rem`, `mod`, the `unchecked-*` integer arithmetic helpers, `rand-int`, `rand-nth`, `bit-shift-right-zero-fill`, `clojure.string/escape`,
`subs`, `int-to-string-radix`, `any?`, `ratio?`, `decimal?`, `realized?`, `range`, `shuffle`, `alength`, `aclone`, `acopy`,
`aslice`, `aconcat`, `array-to-seq`, `array-to-rseq`, `array-seq`, `to-array`,
`rseq`, `find`, `deref`, `reset!`, `vreset!`, `vswap!`, `compare-and-set!`,
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
closed static source domain. Timestamp validation is blocked on typed regex
capture groups, while `parse-timestamp` remains a JavaScript `Date` boundary.

At the current checkpoint, the Logseq tree requires
`clojure.string` 391 times,
`clojure.set` 74 times, `clojure.walk` 30 times, `clojure.edn` 27 times,
`cljs.reader` 27 times, and `clojure.data` 6 times. This makes the remaining
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
`ex-data` remains blocked because its upstream result is an open heterogeneous
map currently stored in the documented exception-only dynamic payload.
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
`re-find`, `re-matches`, and `re-seq` remain explicitly blocked from source
porting because their capture-count-dependent results need a closed match-value
domain; the first two retain their existing narrow runtime result boundary.
Their generated ML now delegates matching and flag handling to named
`Runtime_string.regex_*_groups` functions instead of emitting target-specific
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
`compare-and-set!`, `vreset!`, `vswap!`, `empty`, `peek`, `pop`, and `disj`
routes, the raw compiler-call inventory contains 197 names.
The reference functions delegate through the pinned ClojureScript `IDeref` and
`IReset` protocol shape; static implementations cover refs, lazy values,
futures, and slots without dynamic packing. `compare-and-set!` preserves the
upstream deref/equality/reset control flow and evaluates its arguments once.
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
`uuid?` remains a concrete static blocker: Native UUID values are nominal,
while Melange currently represents UUID values as strings, so a source
predicate cannot distinguish them from ordinary strings until both targets
share a nominal representation. The aggregate `clojure.set` source
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
`clojure.pprint` occurs 14 times and is a JVM-only host boundary. All 28 public
`clojure.zip` vars are now precompiled source definitions. Its parameterized
closed location, path, and callback-context records replace upstream's
heterogeneous metadata vector without dynamic packing. Navigation, changed
propagation, rebuilding, depth-first traversal, and removal retain the pinned
ClojureScript control flow; sibling collections are normalized to typed vectors.
The unary `edit` arity used by Logseq is supported, while additional variadic
callback arguments remain explicitly recorded as a dependent-`apply` blocker.
All 36 observed Logseq qualified zipper calls now resolve through the aggregate
source artifact. `cljs.spec.alpha` and `clojure.spec.alpha`
are explicitly out of scope; their Logseq references remain visible in the
inventory but do not count against migration completion. The independent
`core.async` library is excluded as well, including the observed
`cljs.core.async`, `cljs.core.async.impl.channels`, `clojure.core.async`, and
`clojure.core.async.interop` namespaces. They remain visible as out-of-scope
Logseq dependencies and are not candidates for the LG source stdlib port. The
aggregate now contains all seven public `clojure.walk` functions over the closed
`Lg_edn_backend.t` tree domain. Its source definitions preserve upstream
pre-order, post-order, map-entry, key-conversion, and replacement order while a
small typed runtime primitive rebuilds one collection level. The former
`Runtime_dynamic.t` implementation and compiler namespace route are gone.
`clojure.data/diff` now uses the same explicit closed domain: the source public
function delegates to a typed runtime port that preserves atom, map, set, and
sequential partitions, recursive nil placement, key membership, and upstream
result order. `clojure.string/split` is also source-owned with both public
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
ordinary `.mli` interfaces; no `.mil` sidecar was added for `cljs.cache`.
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
created by ClojureScript, while `cloneable?`, `record?`, and `tagged-literal?`
are first-class predicates over arbitrary values. `replace` still combines a
transducer arity with representation-dependent lazy or vector results.
`spread`, `trampoline`, `swap-vals!`, `vary-meta`, and
`vec-lite` each require a heterogeneous or dependent function relationship
that the current static source type system cannot express without narrowing an
upstream arity.

JavaScript loose equality and falsiness macros, Closure `Uri`, JavaScript
symbols and object constructors, CLJS `Var` and `Inst` values, analyzer-only
casts, and the host writer/newline functions are recorded as host boundaries.
Native implementations do not substitute Clojure truthiness or unrelated
nominal types for those target-specific behaviors.

`flatten` and `memoize` are explicit blockers rather than partial Logseq-facing
ports. Arbitrarily nested `flatten` input can yield heterogeneous leaf types and
therefore needs a closed recursive value domain. A faithful first-class
`memoize` must preserve every arity of its input function while using complete,
potentially heterogeneous argument tuples as cache keys; LG cannot yet express
that returned-function relationship statically.

The architecture tests in `test/stdlib` enforce that `clojure.set` is no
longer classified as compiler-owned and that source-owned core functions have
no name-based call elaboration or inference path.
