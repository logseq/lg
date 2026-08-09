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
state artifact is already the reusable checkpoint. The default cold-build gate
covers the full aggregate-stdlib plus DataScript path and enforces a 10-second
limit without a cache-related environment setting or persisted cache entry.

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
`upstream-var` row for every public function and macro read from the reviewed
core and namespace sources. The extractor evaluates both Clojure and
ClojureScript reader-conditional branches, handles tagged JavaScript literals,
recurses through top-level `if` branches, and excludes private definitions. The
pinned surface currently contains 856 function/macro entries, including 667
entries in `cljs.core`. Each row is
classified independently as source, typed primitive, special form, host
boundary, static-typing blocker, out of scope, or deferred. A namespace's
aggregate support does not make a missing public var appear supported. Manifest entries for
`clojure.core` also classify the corresponding `cljs.core` function and inline
macro surfaces. The current baseline is 341 source entries (39.84%), 62 typed
primitives, 12 special forms, 15 host boundaries, 173 static-typing blockers,
44 out-of-scope Spec entries, and 209 deferred entries. The deferred set is the
explicit queue for further source-port and compiler/macro-boundary review.
`ensure-reduced` is explicitly blocked because its same-arity return type is
dependent on whether the input is already `Reduced<T>`; representing that
contract as a normal generic function would incorrectly nest the wrapper.
`delay?` and `force` remain blocked rather than using direct-call-only inline
type tests. Their pinned ClojureScript functions are first-class: `delay?`
accepts every value type, while `force` returns either a delay's payload or the
unchanged non-delay input. LG cannot yet express those relationships through
one static source function without a typed instance/forceable capability.
`keep-indexed` is also blocked as a whole: porting only its two-argument lazy
sequence clause would silently omit the one-argument stateful transducer.
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
`hash-ordered-coll` and `hash-unordered-coll` remain blocked from source
ownership because their generic elements need an `IHash` capability witness
inside the function body. `hash-unordered-coll` therefore remains a typed
compiler boundary rather than substituting OCaml polymorphic hashing for the
ClojureScript hash contract.

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
The call elaborator contributes 233 reviewed routes. A separate OCaml-AST
extractor now audits 157 form-head pattern routes in expression elaboration and
type inference; this closes the former gap where `case`, `condp`, `doseq`,
`dotimes`, `fn`, `for`, `let`, and `loop` were compiler-owned but appeared as
unclassified deferred upstream vars. Both counts are pinned, so adding a new
name-based form path requires an explicit inventory review.
Every `blocked-static-typing` entry must have a concrete, machine-checked
reason; the inventory test rejects the former catch-all blocker description.
Completion requires reducing `source-shadowed` to zero by removing its legacy
name-based compiler fallback, while resolving each static-typing blocker as the
language gains the required capability, variadic, or higher-order relation.
The current 220-name compiler dispatch inventory has zero `source-shadowed`
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

At the current checkpoint, the Logseq tree requires
`clojure.string` 391 times,
`clojure.set` 74 times, `clojure.walk` 30 times, `clojure.edn` 27 times,
`cljs.reader` 27 times, and `clojure.data` 6 times. This makes the remaining
reader/walk/data boundaries visible instead of treating `clojure.set` as the
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
routes, the raw compiler-call inventory contains 220 names.
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
`clojure.pprint` occurs 14 times and is a JVM-only host boundary. `clojure.zip`
occurs 3 times and is blocked on its public heterogeneous location vectors and
metadata-held generic callbacks. `cljs.spec.alpha` and `clojure.spec.alpha`
are explicitly out of scope; their Logseq references remain visible in the
inventory but do not count against migration completion. `clojure.walk` and `clojure.data` remain explicitly blocked because
their upstream algorithms traverse heterogeneous Clojure trees; a valid port
must use a closed value domain rather than the existing `Runtime_dynamic.t`
boundary.

The source core also includes ClojureScript's `key-test`, `reduceable?`,
`vector-lite`, `hash-map-lite`, and `set-lite`. `IReduce` is a source-facing
alias of LG's existing static `Reducible` protocol, including a typed map
reducer, so explicit implementations work through both names. Lite collections
use LG's default persistent vector, hash-map, and set representations because
their upstream runtime classes are explicitly internal; observable collection
behavior is retained. `hash-map-lite` rejects odd key/value input instead of
inserting an implicit `nil`, because that value would violate a homogeneous
static map value type.

`flatten` and `memoize` are explicit blockers rather than partial Logseq-facing
ports. Arbitrarily nested `flatten` input can yield heterogeneous leaf types and
therefore needs a closed recursive value domain. A faithful first-class
`memoize` must preserve every arity of its input function while using complete,
potentially heterogeneous argument tuples as cache keys; LG cannot yet express
that returned-function relationship statically.

The architecture tests in `test/stdlib` enforce that `clojure.set` is no
longer classified as compiler-owned and that source-owned core functions have
no name-based call elaboration or inference path.
