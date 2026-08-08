# LG Design

## Purpose

LG is a statically typed Clojure dialect that targets OCaml. Its design goal is
to preserve useful Clojure syntax and control flow without reproducing the JVM
object model or silently erasing known types into a universal runtime value.

Static typing is the default and the invariant. Compatibility is accepted only
when it can be represented explicitly and does not weaken the rest of the
program.

## Static typing rules

### No implicit dynamic fallback

The compiler must not turn an unresolved or incompatible type into
`Runtime_dynamic.t` merely to make a program compile.

When inference cannot find one static type, compilation must fail with an error
that:

1. identifies the incompatible types;
2. identifies the collection, branch, field, or function boundary;
3. suggests defining a closed sum type when all alternatives are known.

Unknown types are inference variables, not permission to generate dynamic
storage.

Declared type parameters are rigid static types. A `TVar` in a sidecar
signature, record application, callback, collection, option, equality
operation, or zero-argument function result must remain the same type variable;
the compiler must not materialize it as `dynamic<any>`. Sidecar signatures are
authoritative contracts, not hints that body inference may widen.

`fn<result>` denotes a zero-argument function returning `result`. The compiler
must not invent a `unit` source parameter to encode this arity.

`variadic-fn<fixed...;rest;result>` denotes one function arity with zero or
more fixed parameter types, one homogeneous variadic rest element type, and a
result type. It may appear directly or as an item of `overload<...>`. The
declared rest relationship remains static; it is not an erased sequence of
dynamic values.

The host-boundary assignability policy does not make a dynamic constraint
assignable to a static type, or a static type assignable to a dynamic
constraint. OCaml ownership is not evidence that an unsafe conversion is
valid.

An unannotated `defrecord` field is valid only when protocol methods provide
enough evidence to infer one concrete static type. Otherwise compilation fails
with the field name and asks for a static type annotation. Record construction
at a later top-level form does not retroactively justify an implicit dynamic
field.

### Heterogeneous values use closed sums

Known heterogeneous domains must use explicit tagged unions. This rule applies
equally to:

- vectors;
- maps, including mixed key or value types;
- sets;
- lists;
- queues;
- arrays;
- function branches and return values;
- records containing one of several known payloads.

For example, a collection containing integers and strings is invalid unless its
element type is an explicit sum such as:

```clojure
(type-variant scalar
  (IntValue :int)
  (StringValue :string))

[(IntValue 1) (StringValue "one")]
```

The compiler must not use `Runtime_dynamic.t` as an anonymous sum type.

A keyword-keyed map literal is not an escape from this rule. If its values have
different types, compilation fails even when the compiler could represent the
literal as an anonymous OCaml record. Use `type-record`/`defrecord` when the
keys are fields with distinct fixed types. Use a closed sum when the value
positions belong to one heterogeneous map domain.

Collection updates obey the same rule as literals. `conj`, `cons`, `assoc`,
`merge`, and related operations must retain the existing element, key, and
value types. They must not widen a collection to `dynamic` or silently replace
`map<K,V1>` with `map<K,V2>`.

Collection equality also stays static. Compatible sequential containers may be
compared through their statically typed sequences; they must not be recursively
boxed into universal values merely to reuse dynamic equality.

Polymorphic collection operations are not exposed as untyped first-class
dynamic functions. A bare value such as `(def append conj)` or passing bare
`assoc` as an updater is rejected when no concrete function type is available.
Define a statically typed wrapper whose parameters name the collection, key,
and value types. Direct calls remain statically specialized.

The same rule applies to predicates, scalar conversions, printing, regex
operations, array conversion, and transient conversion when their bare
first-class type would require a universal argument or result. For example,
bare `number?`, `identity`, `str`, `re-find`, `to-array`, `transient`, and
`pr-writer` values are rejected. A direct call may remain supported when the
compiler can specialize it statically; otherwise the author must define a
wrapper with concrete parameter and result types.

Functions are not automatically packed into `Runtime_dynamic.t`. The compiler
does not generate arity adapters that unpack dynamic arguments and repack
results. A heterogeneous function map, callback field, or query dispatch table
must use a closed sum whose variants contain the supported concrete function
types.

Tuples and records cannot be erased at a dynamic boundary. A tuple is not
converted into a dynamic vector, and a record is not converted into a dynamic
map, opaque object, or field-projection table. When several tuple shapes or
record types must share one position, define a closed sum whose constructors
contain those concrete tuples or records.

The generic compiler boundary does not box any statically known value into
`Runtime_dynamic.t`. This includes scalars, options, references, homogeneous
collections, sequence constraints, protocol witnesses, host values, and
`reify` implementations. A dynamic boundary cannot be used as an implicit
conversion API. Code that knows the supported alternatives must keep their
static types or define a closed sum.

### Absence uses `option`

`nil` represents absence and must lower to an option-like static type. A value
that is either absent or a `T` is `option<T>`, not `dynamic`.

Tests and internal code should use `None` and `Some` when they are already at an
explicit OCaml boundary. Ordinary LG source may use `nil`, `if-some`,
`when-some`, and related forms, but inference must retain the concrete payload
type.

Do not mechanically replace meaningful domain variants named `Nil`. For
example, `Data_value.Nil` is a real constructor in the closed DataScript value
domain, while an absent tree child or missing lookup result is an `option`.

### Dynamically bindable Vars remain statically typed

`(def ^:dynamic *var* value)` means that the Var supports dynamic rebinding. It
does not mean that the Var's value has a dynamic type.

Each dynamically bindable Var has one inferred or annotated static value type.
Every `binding` value must match it.

The following are not supported:

- `[^:dynamic value]` as a parameter type;
- `:dynamic` in signatures or sidecars;
- `vector<dynamic>`, `map<...,dynamic>`, or similar collection types;
- source-level dynamic packing or narrowing escape forms;
- a dynamic marker field added to a record to make it packable.

There is no public conversion pair equivalent to `to_dynamic` and
`of_dynamic`.

Generated identifiers must not expose `__lg_dynamic`,
`__lg_dynamic-narrow`, or equivalent source escape-hatch names. An internal
erased adapter, where one still exists for a documented runtime boundary, must
be named for that exact boundary and must not imply a general conversion API.

### Open boundaries are exceptional

A truly open extension point may need an internal compatibility adapter, but it
must satisfy all of these conditions:

- the accepted upstream behavior is concretely documented;
- a closed sum would lose required behavior, not merely require more modeling;
- the dynamic value is restricted to the smallest field or call boundary;
- the value is validated and converted to a static representation immediately;
- no enclosing collection, record, or subsystem becomes dynamic;
- the boundary is reviewed as a design change.

This exception applies only to compiler/runtime implementation internals. It
does not authorize a source type annotation, sidecar type, collection element
type, host-boundary coercion, or general-purpose pack/unpack API.

An arbitrary function value or runtime Var lookup is not such a boundary.
`resolve` and `requiring-resolve` are rejected because their result type is
open; supported Vars must be represented by an explicit closed sum or a
statically typed registry. The compiler must not enable whole-program runtime
Var reflection or register every definition as a dynamic value.

LG source syntax does not expose a universal dynamic type or an escape hatch for
creating one. Source code also cannot require
`ocaml.Lg_runtime.Runtime_dynamic`; that module is a compiler implementation
detail, not an interop boundary. There is no `Lg_dyn` compatibility alias.

Untyped transient annotations are rejected for the same reason.
`^:transient-vector` and `^:transient-map` do not silently create collections
whose elements, keys, or values are dynamic. A transient collection must retain
the concrete types of the persistent collection from which it was created.

`nil` does not supply a missing element type. Mutable cells initialized with
`nil`, such as `(atom nil)` or `(volatile! nil)`, require an explicit
`ref<option<T>>` context. The compiler must not infer universal dynamic storage
from an otherwise untyped `nil`.

## Interop rules

### No Java or JVM object model

LG does not support Java interop. The compiler must reject, rather than emulate
or silently translate:

- `java.*`, `javax.*`, and `clojure.lang.*` classes;
- `:import` clauses for JVM classes;
- JVM type hints such as `^Object` and `^java.io.Writer`;
- Java constructors and static methods;
- reflection and class inspection such as `class`, `type`, `.getClass`, and
  `.getName`;
- Java comparison, hashing, exception, stream, and collection aliases;
- fake JVM class names returned by runtime values;
- JVM protocol aliases used as substitutes for LG protocols.

Required behavior must be expressed through an LG function, an LG protocol, a
closed sum, or an explicit OCaml package binding. For example, time, radix
formatting, buffers, and file access use named LG/OCaml primitives instead of
`System`, `String`, or `java.io` compatibility shims.

### OCaml interop is explicit

OCaml interop is allowed through declared package modules, signatures, concrete
host types, and explicit constructors. It must preserve the declared OCaml type
and must not pass through a universal boxed value.

## DataScript model

DataScript is a known domain and therefore is not an open dynamic boundary.
Follow the `datascript-ocaml` approach while retaining reusable upstream
algorithms.

### Upstream behavior is authoritative

The authoritative compatibility baseline is the Logseq DataScript fork at
commit `3f141af97b70e1f14c65eaa119acd822ebece37e`. The repository URL, source
mapping, generated public API manifest, and differential fixture inventory are
recorded in `test/datascript/UPSTREAM.md`.

Static typing may change the representation of an upstream value, but it must
not remove an upstream operation, arity, predicate composition rule, index
ordering, transaction ordering rule, or observable database behavior.

Annotation difficulty cannot justify missing upstream behavior. A compiler
inference gap must be fixed or represented by a permitted explicit boundary;
it must not narrow or remove the DataScript API.

Ports keep upstream control flow and algorithms unless a measured,
semantics-preserving optimization is documented. Replacing an upstream
protocol or heterogeneous record with a closed sum changes dispatch
representation, not frame order, cursor movement, branch order, or termination
behavior.

Transaction data is a closed `tx-entry` sum, but the sum is only a static
representation of upstream transaction forms. It must preserve unique-identity
upserts, explicit-ID conflict checks, nested entity maps, component cascades,
incoming-reference cleanup, tuple maintenance, and cardinality-many
compare-and-set behavior. Static typing is not permission to replace the
upstream transaction state machine with a reduced subset.

The currently accepted measured representation optimizations are narrow:

- Native forward sorted-set slices use a typed iterator cursor instead of the
  recursive sequence wrapper. Bounds, ordering, laziness, and termination are
  unchanged; restoring the wrapper made the pull benchmarks about 20% slower.
- A sequence exposed by a persistent sorted-set iterator is memoized on both
  Native and Melange. Re-reading it after `count`, `first`, or another consumer
  must produce the same values, matching upstream Clojure lazy-sequence
  semantics. An unmemoized iterator sequence is not an acceptable performance
  optimization.
- Pull uses the upstream list-shaped frame stack and caches the current datom
  plus its lazy tail. This preserves upstream cursor movement while avoiding
  repeated evaluation of an unmemoized sequence node.
- Pull entity accumulators remain persistent typed maps. LG's current
  transient hash builder has a higher fixed cost for the small maps produced
  per entity; restoring it made all three pull benchmarks slower. This changes
  only the accumulator representation, not the upstream frame transitions.
- Completed child pull frames are converted immediately to the closed
  `Data_value` representation before merging into their parent. The root frame
  still returns the same typed map and frame order is unchanged. This reduced
  Native pull-one/pull-many/pull-wildcard from about 0.89/2.76/3.85 ms to
  0.75/2.03/2.89 ms by avoiding a second recursive result-tree lifetime.
- `Datom.idx` and its cached hash remain mutable `int` fields, as upstream
  intends. Wrapping either field in `ref<int>` adds one heap object per mutable
  field per datom and changes representation without adding safety.
- Static query clauses retain upstream's one-lookup-plus-hash-join fallback.
  When the entity position is already bound, LG specializes the same logical
  join to repeated EAVT index slices and appends only unbound columns. This
  avoids eagerly projecting every datom for the right relation. On the
  20,000-person benchmark it reduced Native q2/q3/q4 from about
  8.9/12.6/19.9 ms to 2.1/3.6/5.1 ms while preserving result-count checks.
- Query output projection reuses relation rows when the requested find columns
  already are the complete row in order. It does not allocate an identical
  array per result. This reduced Native q1 from about 1.55 ms to 0.55 ms.
- Single-key hash joins use a scalar key and append the right-only columns in
  one array allocation. Multi-key joins retain the general closed-array key
  path.
- A statically typed single-vector `mapv` maps the RRB vector directly.
  Native and Melange both retain the RRB structure. Other seqable inputs and
  multi-collection `mapv` keep their existing sequence semantics.
- A relation product with a singleton side maps the other RRB row vector
  directly instead of converting it to an array and rebuilding the same RRB
  vector. Cartesian-product row order and row concatenation are unchanged.
  The 100,000-row allocation regression dropped below 5.5 MB from 5.73 MB,
  and Melange `qpred2` improved from 12.072 ms to 11.619 ms.
- The single empty tuple is the identity element of a relation product. The
  product reuses the other RRB relation directly instead of copying every row
  through an empty array append. Empty-relation behavior, row order, and row
  contents are unchanged. The 100,000-row allocation check dropped below
  2 KB from 3.31 MB, and Melange `q1` improved from 1.589 ms to 1.533 ms.
- Bound-entity query clauses reduce the same upstream EAVT slice directly into
  result rows when no transaction constraint is present. The slice bounds,
  comparator, datom order, added filtering, and projected columns are
  unchanged; this only avoids allocating an intermediate datom vector.
- When that bound attribute is cardinality-one, the reducer uses one typed
  EAVT lower-bound seek and validates the returned datom. Cardinality-many
  attributes retain the complete slice traversal and result order.
- Unindexed AEVT value scans compare closed `String` and `Keyword` payloads
  with static string equality. Every other DataScript value retains the full
  closed-domain equality operation.
- The persistent-sorted-set slice reducer invokes its typed binary callback
  directly on Melange, matching the ordinary node reducer. Native control flow
  is unchanged.
- Bound-entity query clauses classify value and transaction pattern positions
  once before reducing input rows. An unbound variable or missing position
  cannot constrain a slice, so it does not repeat relation-attribute lookups for
  every row. Constants and already-bound variables retain the same lookup and
  reference-resolution path. This reduced the Melange `q2` median from
  4.401 ms to 4.275 ms against the 4.3 ms upstream gate.
- A single-row scalar query input that is not part of the requested result may
  be substituted into patterns and predicates. Multi-row relations are never
  elided, even when all rows happen to contain equal values, because their
  multiplicity can affect query semantics.
- The closed `q` entry point delegates to the same typed executor used by the
  rest of the static query API. It does not retain a second context pipeline
  that carries an elidable scalar input through every relation row. Exactly
  two ordered or equality operands are compared directly through their closed
  values; empty, unary, and variadic calls retain the general upstream path.
  On Melange this reduced `qpred1` from 7.371 ms to 6.131 ms and `qpred2` from
  11.619 ms to 6.722 ms.
- Recursive rule cycle guards retain upstream generation, activation, row,
  operand-pair, and branch order. The closed `-differ?` predicate compares its
  already compiled operand pairs directly instead of constructing temporary
  result and value vectors for the general callable path. On the wide-7x3
  benchmark this reduced Native from 45.03 ms to 34.79 ms and Melange from
  about 85 ms to 52.41 ms while the recursive-cycle and false-argument
  upstream tests remained unchanged.
- Rule expansion uses the parser's closed typed branch expander. Calls must
  match the declared rule arity and report `Rule arity mismatch` instead of
  silently dropping extra arguments.
- Parsed pull attributes cache the generic static hash of their closed alias
  value. Pull result insertion reuses that validated hash while retaining the
  same persistent map, key equality, duplicate replacement, and frame
  transitions. On Melange this reduced pull-one from 1.396 ms to 1.339 ms and
  pull-many from 2.510 ms to 2.151 ms.
- The default static map is a ClojureScript-shaped persistent hash map. Its
  trie stores key/value leaves directly in bitmap-indexed nodes, expands a
  node with 16 occupied branches into a 32-way array node, and keeps equal-hash
  keys in a collision node. Updates copy only the nodes on the edited path;
  ordinary lookup no longer performs a second vector access after the trie
  lookup. A structurally shared sequence spine preserves the insertion-order
  contract required by DataScript relation bindings without participating in
  lookup. Metadata is stored once on the map root so `assoc`, `dissoc`, and
  `empty` preserve the ClojureScript metadata contract. The runtime map has
  static registry implementations for ClojureScript's `ILookup`,
  `ICollection`, `IAssociative`, `IFind`, `IMap`, `IKVReduce`, `IMeta`, and
  `IWithMeta`, in addition to its seq, count, and empty capabilities. Protocol
  method signatures retain all fixed arities under one method identity, and
  each implementation records one statically typed target per arity. Map
  `-lookup` therefore dispatches through the ordinary protocol registry for
  both its nullable two-argument result and its concrete three-argument
  default result. A protocol witness stores an overload bundle in that method's
  single witness slot, so generic protocol-constrained functions select the
  same fixed arity without dynamic packing. `IEquiv` and `IHash` remain
  compiler-elaborated so their key and value operations stay statically typed.
- DataScript identifier comparison checks namespace and name slices in place
  instead of allocating substrings. Separator handling and lexical ordering
  remain identical.
- Persistent sorted set iteration retains upstream leaf-sized chunk boundaries.
  Each leaf has one memoized sequence boundary, while values inside the leaf
  are read directly from its existing key array. The next leaf remains lazy,
  so storage callbacks occur in the same order as upstream. On the tracked
  300,000-element `next` workload this reduced Native and Melange to
  2.90 ms and 4.20 ms respectively, versus 6.05 ms for upstream ClojureScript.
- JSON database thaw keeps the input text in a closed `Json_source` constructor
  until `from-serializable` consumes it. The target JSON parser then converts
  database fields directly into named prepared records: entity, attribute, and
  transaction fields remain `int`, while only the encoded datom value stays in
  the closed EDN domain required by custom codecs. This preserves the JSON
  format and upstream restoration order without retaining a second generic EDN
  tree. It allows Melange thaw to complete under the default Node heap, but the
  full 300,000-person benchmark still shows substantial serialization overhead.
  Attribute lookup now converts the closed attribute vector once before the
  datom loop, and index restoration uses a typed array loop without callback
  allocation. Prepared datoms are read once per row into a reusable closed
  cursor whose entity, attribute, encoded value, and transaction fields retain
  their concrete types. Default JSON thaw therefore performs one closed datom
  dispatch instead of repeatedly matching the same row for each field; custom
  codecs still receive the same closed EDN value. The 300,000-person median
  improved from 1177.197 ms to 1045.487 ms on Native. Melange measured
  1124.227 ms. Both are below the pinned 1134.9 ms upstream gate, although the
  Melange margin remains narrow and must be rechecked in final acceptance.
- Serialization attribute indexing uses one closed size dispatch. Schemas with
  at most eight attributes use a reverse string-array scan; larger schemas use
  a typed string hashtable. Both retain the upstream last-index result for
  duplicate attributes. This removes repeated Clojure string hashing from the
  common freeze path while preserving bounded lookup for large schemas.
- The Melange JSON writer combines an enclosing vector separator with a
  compact `Int4_vector` row. Mixed vectors and rows whose value requires the
  generic writer retain the original recursive path. This preserves exact JSON
  order and values while reducing token pressure on serialized EAVT arrays.

The complete 2026-07-30 benchmark baseline uses 20,000 people, a 2-second
warmup, five 1-second samples, batch size 10, and an isolated process for each
workload. Native is faster than pinned upstream JavaScript on 21 of 28
workloads and slower on 7. Melange is faster on 4 and slower on 24. The
remaining regressions are real acceptance failures, not accepted representation
differences. Serialization, `init`, predicate queries, and `pull-many` are the
highest-priority shared failures. Full results and margin reruns are recorded
in `test/datascript/benchmark/RESULTS.md`.

When an upstream API accepts several known shapes, LG represents those shapes
with a closed sum, record, option, or static protocol. It does not make the API
dynamic, and it does not delete the API to avoid modeling the shapes.

Static typing must also preserve readability. Prefer named state records,
small typed accessors, and one closed dispatch sum over long positional
constructors, duplicated branches, compatibility overlays, or a second
implementation of the upstream algorithm.

In particular:

- repeated database filters compose in the same order as upstream and each
  new predicate receives the original unfiltered database;
- `init-db` retains its schema and options behavior through typed options;
- query, pull, rules, storage, and serialization features remain present when
  their raw EDN representation is replaced by typed constructors;
- the benchmark suite keeps every upstream case whose implementation has been
  ported and never reports success by silently dropping a slow or untyped case.

Target-specific narrowing is explicit. OCaml Native and Melange support only
`Strong | Weak` storage references because neither target has a soft-reference
primitive. The default is `Weak`, which preserves the upstream default's
releasable-cache intent. Serialized LG databases likewise store only these two
closed constructors; `Soft` is not an accepted source or payload value.

The retention policy applies to the entire persisted tree, not only its root.
`Strong` keeps restored roots and children strongly reachable. `Weak` permits
both roots and children to be reclaimed and restored from their addresses.
Persistent updates such as `conj` and `disj` retain the source set's policy.

### Data values

DataScript values use a closed recursive `Data_value.t` sum. It contains the
supported scalar and collection constructors and does not contain a `Dynamic`
constructor.

Data values must not provide generic `to_dynamic` or `of_dynamic` conversions.
Comparison, equality, hashing, serialization, and printing operate directly on
the closed constructors.

### Concrete domain types

Keep these types concrete:

- entity, transaction, and storage addresses: `int`;
- attributes and stored values: `Data_value.t` or a narrower domain type;
- `added`: `bool`;
- optional tree children and restored nodes: `option`;
- tuple rows: statically typed arrays;
- callbacks: concrete `fn<...>` types;
- schemas, transaction entities, query forms, parser forms, and query results:
  dedicated records and closed sums.

An address is an in-process/storage index for LG DataScript and uses `int`.
Converting every address through `int64` is unnecessary. A file-format boundary
may convert locally when a library formatter requires it.

### Query tuples

Tuple access goes through a statically typed `tuple-get`. Call sites must not
pack a tuple into a universal runtime value before indexing it. If query rows
have multiple known shapes, define a row sum type and match it explicitly.

Query execution uses a closed result domain such as
`Datascript_runtime.Query_value.result`: entity IDs, attributes, DataScript
values, database handles, and pull results are separate constructors of one
sum. A runtime query source is likewise a closed
`Database_source | Relation_source` sum. `Context.sources`, relation rows, and
query input decoding must converge on these types; they must not store raw
universal values and recover their shape with `satisfies?`, `sequential?`, or
dynamic tuple access.

Query inputs are also closed. The outer input sum distinguishes a source,
parsed rules, and a binding value. Binding values use a recursive
`Scalar result | Collection binding-value` sum, so tuple and collection inputs
can contain known heterogeneous result constructors without erasing them into
`dynamic`. The native `q` API accepts this explicit input vector; an arbitrary
variadic sequence of untyped host values is not a supported compatibility
boundary.

Parser return maps are a closed sum, not a record with dynamic `type` and
`symbols` fields. The branches retain their element types:
`ReturnKeys vector<keyword>`, `ReturnSyms vector<symbol>`, and
`ReturnStrs vector<string>`. Raw heterogeneous query-form maps are not an
alternate parser API; callers construct a typed `Query`.

Database pattern matches are normalized immediately:

- a Datom becomes an `Entity | Attr | Value` result array;
- free-variable positions are projected with an `int array`;
- Relation attributes are always `map<string;int>`;
- lookup-ref databases are stored per variable on the Relation;
- sum, product, and hash join operate directly on closed result arrays.

Hash join normalizes `Attr attr` and `Value (Keyword attr)` to the same closed
key and resolves entity lookup refs through the Relation's explicit database
map. Query resolution must use this data instead of dynamically bound
`*lookup-attrs*` or `*implicit-source*` Vars.

### Transactions and schemas

Transaction data is not an arbitrary heterogeneous vector/map graph. Define
closed transaction entity and operation types. Schema records and validation
errors likewise use concrete fields or explicit sums.

Transaction APIs accept `vector<tx-entry>`. Lazy or arbitrary seqables are not
an open transaction boundary: callers realize them into a typed vector first.
Expansion, tuple flushing, and the pending transaction queue retain that vector
type throughout execution.

Transaction expansion must not repeatedly copy the unprocessed tail. A static
worklist of `tx-entry` batches preserves upstream left-to-right expansion and
tuple-flush ordering while keeping bulk transactions subquadratic.

Unique-identity conflicts discovered after a tempid has already been allocated
must restart from the initial transaction report, as upstream does. The typed
state machine represents this with closed `Continue | Restart` and
`Finished | Retry` results. It must not keep the partially applied database,
silently move only the identity datom, or use a dynamic exception payload.
Repeated retries accumulate their forced tempid resolutions, so transaction
order and earlier reference assertions are preserved.

A vector-form unique-identity upsert must bind an as-yet-unallocated tempid
directly to the existing entity. Writing the datom to the existing entity while
leaving `:tempids` pointed at a newly allocated phantom eid is not compatible
with upstream. For a ref-valued identity attribute, lookup uses transaction
tempid resolutions as well as the current database; otherwise an already
resolved reference tempid can miss the existing identity and create the same
phantom mapping.

Schema transaction validation remains part of the transaction state machine.
Incomplete schema entities and identifiers in the reserved `db` namespace are
rejected before entity expansion. Static schema maps do not justify skipping
these upstream checks.

Derived tuple attributes cannot be modified directly. User `TxAdd` and
`TxRetract` entries may only repeat a tuple that already exactly matches its
source attributes; tuple maintenance itself uses a distinct internal
`TxSetTuple` constructor. This preserves upstream's internal-operation marker
without metadata or dynamic transaction vectors.

Reference tempids are tracked in concrete `TxReport` maps until the resolved
entity ID is also used as a transaction subject. Finishing with a tempid that
appeared only as a value is an error, matching upstream. The bookkeeping must
not turn `TxReport`, its tempid map, or reference values into dynamic data.
Tempids are rejected in retract, retract-attribute, retract-entity, and CAS
entries; only add and entity-map expansion may allocate them.

Legacy tagged EDN readers for `datascript/Datom` and `datascript/DB` are not a
dynamic compatibility boundary. Their heterogeneous vectors and maps must not
be unpacked through a universal value or variadic `apply`. Static callers use
typed constructors and closed storage/serialization payloads.

Error reporting must not retain a heterogeneous universal ex-data map merely
for compatibility. A static exception with a useful message is preferred. If
machine-readable error data is required, define a closed error sum.

Connections use explicit static functions such as `current-db : Conn -> DB`.
They do not register dynamic record packers solely to support Clojure's generic
`IDeref` object protocol. Core `DB`, `TxReport`, `Conn`, and connection-state
records must remain outside the dynamic record registry.

The same rule applies to ordinary `extend-type`: a statically known receiver
uses its compiler-known protocol witness and implementation. Defining a static
extension must not automatically register a dynamic protocol adapter or record
packer. DataScript `Datom` operations such as index mutation, equality, and
hashing are explicit typed functions; they do not require JVM-style `IHash`,
`IEquiv`, or a universal protocol registry.

Static records and their static protocol implementations do not emit dynamic
lookup registrations, record packers, or `Runtime_dynamic.nominal_tag`
extensions. The compiler does not scan generated IR for record-to-dynamic
conversions, rewrite record definitions to add packers, or track packer state
across incremental compilation. The runtime does not provide a generic named
record payload or process-wide protocol, lookup, printer, or record-packer
registry.

Database and filtered-database equality, hashing, and counting compare the
upstream-observable schema and EAVT datoms, not internal IDs, cache state, or
index counts. Datom equality and hashes ignore transaction IDs as upstream
does. A comparison that intentionally accepts both `DB` and `FilteredDB` must
declare a closed database-view sum; it must not make `IEquiv`'s second argument
dynamic merely to recover cross-record dispatch.

`reify` stores only its statically typed method function or method tuple. It
does not build a second dynamic protocol table, and the runtime does not
provide dynamic protocol invocation or arity adapters.

### Storage and serialization

Storage is generic in its payload type. The storage record, codec callbacks, and
tree nodes must share that type parameter. Serialization payloads use a closed
serialization type or an explicitly supplied typed codec.

A configurable codec does not justify making the entire storage backend,
options map, or tree dynamic.

File storage reads and writes the closed storage payload. Its default codec may
use OCaml `Marshal`; custom callbacks must still have concrete
`Storage_value.t` input and output types. Generic EDN `freeze`/`thaw` callbacks
that erase the payload type are not supported.

## Compiler architecture

Prefer direct, typed elaboration:

```text
parse -> infer/check -> typed semantic IR -> OCaml
```

Avoid compatibility overlays that rewrite invalid programs into dynamic ones.
In particular:

- do not add implicit pack/unpack nodes after type checking fails;
- do not merge incompatible branches by boxing both branches;
- do not widen a heterogeneous collection to a universal element type;
- do not infer an unannotated record field as dynamic;
- do not add record caches, protocol adapters, or reflection tables solely for
  dynamic packing;
- do not hide unsafe behavior behind generated helpers.

Compiler errors are part of the language design. A concise error that asks the
author to define a sum type is preferable to a complex runtime compatibility
path.

Generic `seqable<T>` signatures retain both the element type and the concrete
collection storage type at each call. The compiler passes a static sequence
adapter beside the original collection; it must not erase a `vector<T>`,
`list<T>`, `array<T>`, or another statically supported collection to dynamic
storage while instantiating the signature.

Anonymous `fn` forms support the same fixed and final variadic clause layout as
multi-arity `defn`. Each clause lowers to a readable local OCaml function and
the value carries one static overload bundle. A variadic clause has one
homogeneous rest element type; ignored arguments do not justify a dynamic rest
sequence.

Static scalar operations must also stay in static runtime modules. For example,
integer `mod` uses `Runtime_int`, not `Runtime_dynamic`, even when preserving
Clojure's signed-modulus semantics.

## Performance

The native DataScript implementation should operate on unboxed or predictably
boxed static OCaml values in hot paths:

- datom comparison;
- sorted-set traversal;
- query joins and tuple indexing;
- schema lookup;
- transaction processing;
- storage address traversal.

Benchmarks must compare native LG DataScript with upstream JavaScript and guard
against regressions. Improving performance by caching universal wrappers is not
an acceptable substitute for removing the wrappers.

Benchmark workloads, data size, warmup, sampling, and feature coverage must be
comparable. A scaling gate complements time-per-operation comparisons so an
apparently acceptable small benchmark cannot hide quadratic bulk-transaction
behavior.

## Change checklist

Before merging a compiler, runtime, or DataScript change, verify:

- no new source-level dynamic type or escape hatch was introduced;
- known heterogeneous values use a closed sum;
- absence uses `option`;
- record and callback fields remain concrete;
- no Java/JVM compatibility alias was added;
- generated code does not use `Obj.magic`;
- generated DataScript hot paths contain no `Runtime_dynamic` references;
- focused rejection tests cover invalid heterogeneous and interop cases;
- DataScript runtime tests and the relevant native benchmark pass.
