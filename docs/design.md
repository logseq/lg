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

`try` inference preserves static constraints from its body and handlers. An
unresolved branch does not force other branches into dynamic storage; elaboration
merges the resolved branch types and rejects incompatible alternatives.

Unannotated accessors that dereference record fields retain a fresh payload
variable, shared with the reference field. Calling the accessor specializes
that variable to the stored type without copying or erasing the mutable cell.

Truthiness checks preserve a callback's known return type. In `or`, a static
fallback constrains an otherwise unknown callback result as optional instead
of incorrectly forcing that callback to return a boolean.

A keyword field used as a condition requires truthiness, not optional storage.
The field can remain a boolean, option, or another statically typed value;
using it in a condition must not prevent later record constraints from resolving
its actual type.

An `and` used as a condition preserves boolean storage: false already provides
the falsey case. Guard inference waits for the other operands' constraints
before considering nullable non-boolean values, preserving optional numeric
comparisons without imposing option storage on unresolved or boolean values.
Host `option<T>` and LG nullable values use the same payload truthiness,
including false boolean payloads.

In an `and` expression, operands before the final operand are guards and the
final operand is the returned value. Ordinary value inference must therefore
apply truthiness evidence to the guards but infer the final operand as a value.
Only an outer condition that observes the whole `and` may require truthiness for
that final value. This keeps schema-style predicates from turning returned map
fields into witness storage.

Variant payload adaptation destructures capability evidence at the payload
boundary, just as ordinary parameter binding does. Forwarding a variant to a
typed callback must expose its original collection value without its sequence
adapter, while preserving concrete element types and single evaluation.

Contextual collection results retain record fields required by their callbacks.
Structural callback parameters stay structural even when their fields also
match a named record.

The last operand of `or` is a fallback value, not a truthiness test. Its expected
result type flows into callback calls without inferring optionality from an
earlier operand. A callback returning `nil` remains absent when adapted to an
optional result; it must not become `Some None`.

Repeated names in sequential `let` bindings introduce independent lexical
bindings. An initializer and earlier closures still refer to the preceding
binding; type constraints from a later binding must not change that earlier
value's type, including when the later binding destructures a tuple.

Conditional binding locals shadow source macros and inline macros only in the
present branch. Initializers and absent branches use the enclosing environment.
Macro expansion retains the complete `let` scope rather than expanding its
bindings and body independently; destructured names obey the same rules.

Sequential destructuring preserves a known tuple source even when every
position has the same inferred type. Equal element types alone do not turn
tuple storage into vector storage; actual vectors retain their sequence shape.

Record updates propagate a known field type into the assigned value. Contextual
updates of unresolved expression receivers defer their storage choice instead
of assuming a homogeneous map from the first assigned keyword. Callback result
requirements can therefore identify a nested named record before elaboration;
known map receivers retain map update semantics.

Contextual `apply` results constrain the rest element through the function's variadic
signature, including fixed arguments before the final collection. `concat`
propagates a concrete expected element type to unresolved inputs, but does not
replace known elements or spread partial callback rows into other collections.
Applying a sequence consumer to a dereferenced value does not infer a sequence
adapter as the mutable cell's payload. Initialization and mutation determine
the stored collection type; the consumer observes a sequence view of it.

Keyword access on a function result propagates the required field back into
the function's result type. Multiple accesses constrain the same underlying
value, so unrelated records sharing one field name cannot choose its identity.
Known map results retain map lookup semantics rather than acquiring record
constraints.

Calls through expressions propagate their function parameter types into the
arguments, just like calls through local names. Keyword lookup on a function
result preserves a known callback signature; named record arguments stay static
and the callee expression is evaluated once.

Record inference considers declared nested fields when choosing between
compatible record candidates. A generic field is not evidence for the nested
fields of a concrete reference payload. Reference payload records must satisfy
their required fields; inference never converts the mutable cell to a projected
record reference. These requirements recurse through optional payloads and
nested references, preserving cell identity across reads and writes. Accessors
without such nested requirements stay generic.

Structural and named record requirements on the same field merge into the named
type when it satisfies the structural requirements. This works in either order
and through nested fields, without dropping the named record's remaining fields
or accepting incompatible field types.

An open-row extension field keeps its extension role even when its payload has
truthiness, sequence, protocol, or other capability evidence. Adapting a missing
extension field uses the complete static source row as the extension payload,
then constructs the required capability witness at that boundary.

Membership tests on record fields use the same static collection constraints as
membership on local values. Resolving a field to a set or map constrains its key
type, while a vector constrains the key to an integer index. Keyword field access
does not force the collection into a dynamic map.

Sequence observations on string-valued record fields retain string storage and
character elements. This applies in either inference order and inside nested
records, without accepting incompatible numeric element constraints.

Repeated nil predicates preserve an existing nil-check constraint rather than
adding an option around it. Matching and conditional binding inspect the known
option stored beneath outer capability evidence, preserving any evidence inside
the payload. Passing or returning such a value retains the complete option,
including `Some false`, and evaluates the source expression once.

Result payload constraints flow from match branches back into inferred
callbacks and polymorphic calls. An absent constructor payload must not erase
the shared type variable carried by the other branch. A resolved host record
and its external type name unify with the same type arguments, consistently
with host-boundary assignability.

Recursive constraints normalize visible OCaml record and polymorphic variant
aliases before unification. Recursive payload references retain their host
identity; normalization does not expand them indefinitely or erase payload types.
When nested constraints expose different expansion depths of the same alias,
unification resolves the visible definition only at the conflicting boundary.
Repeated alias/type pairs on the current unification path close the recursive
comparison. This does not skip sibling constraints or subsequent equations;
incompatible payloads remain errors.

Nested `Ok` and `Error` patterns infer payload structure recursively, including
tuples and options. A helper matching an error tuple therefore does not need a
parameter hint; unconstrained alternatives retain independent type variables.

Result constructors describe the stored payload, not the capability evidence
used to inspect it. Branch adaptation converts both Result alternatives when
their payload representations differ, including nested tuples. Unqualified
nullary constructors retain their nominal result type in emitted expressions
so distinct namespaces may use the same OCaml constructor spelling.
Function recursion and value dependency analysis use value references only:
quoted symbols, record type names, and record field names are data or type
positions, not calls to same-named functions.

Core sequence-operation inference is gated by the resolved callable shape.
Source bindings and `:refer-clojure :exclude` shadow core helpers such as
`remove`; special filter/remove callback inference must not rewrite those local
functions or their parameters.

Anonymous recursive rows imported from OCaml retain an inference variable at
each recursive back-edge instead of inventing an opaque source type. Generated
callbacks are still checked against the original OCaml interface, so recursive
payloads cannot be used at incompatible types. Named recursive aliases retain
their canonical host identity.

Destructured local bindings retain independent inference variables until their
initializers and uses constrain them. In a mixed `let`, later initializer
expressions constrain earlier destructured values as well as the final body.
An open callback result may infer a tuple without merging independent positions
into one collection element type; known sequence inputs keep sequence semantics.

Declared type parameters are rigid static types. A `TVar` in a sidecar
signature, record application, callback, collection, option, equality
operation, or zero-argument function result must remain the same type variable;
the compiler must not materialize it as `dynamic<any>`. Sidecar signatures are
authoritative contracts, not hints that body inference may widen.

An explicit value signature is retained separately from inferred expression
annotations through lowering. The generated binding checks the original
signature with universally quantified type parameters, for recursive and
nonrecursive definitions alike. A body cannot specialize `fn<a;a>` to
`fn<int;int>` or `fn<a;int>`. Calls instantiate declared type parameters afresh.
Omitted types remain inference variables; their generalization follows the
OCaml value restriction instead of adding an implicit universal promise.
Runtime storage wrappers, such as the reference behind a dynamically bindable
Var, must preserve the declared payload type.

Normal compilation and compilation resumed from a saved state perform the same
OCaml contract and value-restriction checks. A saved state restores the checked
OCaml prefix before accepting new code; an invalid continuation must not publish
an output or an updated state. Cache replay may skip rechecking an unchanged,
previously validated prefix, but a subsequent cache miss restores that prefix
before checking new code.

`fn<result>` denotes a zero-argument function returning `result`. The compiler
must not invent a `unit` source parameter to encode this arity.

`variadic-fn<fixed...;rest;result>` denotes one function arity with zero or
more fixed parameter types, one homogeneous variadic rest element type, and a
result type. It may appear directly or as an item of `overload<...>`. The
declared rest relationship remains static; it is not an erased sequence of
dynamic values.

`reducing-callback-result<T>` is a sidecar-only callback result type. It accepts
either `T` or `reduced<T>` from a reducing callback while preserving `T` as the
accumulator type. It must not add capability evidence to `T` or expose the
internal reduced wrapper as the enclosing function's result type. At the
callback boundary both alternatives use the closed `Runtime_reduced.t` type:
ordinary results are wrapped as continuing values, and already reduced results
retain their tag. The receiving function handles the tag and returns its
statically declared result; callback adaptation must not change that result
through an exception-based propagation convention.

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

When an expression has an explicit expected closed-sum type, the compiler may
insert a unary variant constructor only if exactly one constructor payload is
statically assignable from the expression type. This contextual injection is
part of closed-sum elaboration: it emits the constructor directly and never
boxes through `Runtime_dynamic`. Zero matching constructors is an error, as is
more than one matching constructor. Unknown, dynamic, and type-variable
expressions are never candidates for implicit injection. This rule lets a
typed port preserve upstream branch order and control flow without duplicating
the algorithm solely to spell constructor wrappers.

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

Vector literal elements evaluate once in source order. Lowering sequences
effectful element expressions before constructing the backing OCaml list;
an exception stops evaluation before any later element runs.

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

### Sequence transducer consumers

`sequence` and `eduction` collect emitted values through a reducer with a `unit`
accumulator. Their transducer parameter is instantiated at `unit`; it does not
promise support for an arbitrary caller-selected accumulator. `transduce`
continues to preserve the accumulator type supplied by the caller's reducer.

### Tap values

The tap registry stores the closed EDN value domain, `Lg_edn_backend.t`.
Supported scalar and collection payloads are converted directly to that domain;
callbacks receive a closed value or an explicitly checked scalar projection.
An arbitrary rigid type parameter is not evidence that its values can be
converted to EDN. Tap registration and delivery must not erase callbacks or
payloads into `Runtime_dynamic.t`.

### Sequential result bindings

`let*` binds successful `Result` payloads in source order:

```clojure
(let* [fields (as-map input)
       version (required "format-version" as-int fields)]
  (Ok version))
```

Each binding expression is evaluated once. `Ok` unwraps into the binding
pattern; `Error` returns immediately from the `let*` expression without
evaluating subsequent bindings or the body. Binding patterns use ordinary
`let` destructuring. The body returns a result explicitly; it is not implicitly
wrapped. An empty binding vector evaluates the body directly.

Expansion uses statically checked `match` and `let` forms, retaining the result
payload and error types. All error branches must have compatible types.
Ordinary `let` retains its existing semantics. LG's `let*` is intentionally
different from Clojure's internal ordinary binding form of the same name.

### Collection match patterns

Collection patterns in `match` are exact-length unless they end with
`& binding`. `[head & tail]` requires at least one element and binds the
remaining elements as a statically typed list; `[_ _ & _]` requires at least
two elements and discards the rest. This also applies inside constructor
payload patterns. A rest binding must be the final item, and malformed rest
patterns are rejected instead of treating `&` as a variable.

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

An external closed value domain may register an `exception-data-adapter` whose
exact type is `T -> Lg_edn_backend.t`. The compiler uses that adapter only while
constructing the documented `ex-info` payload and composes it recursively
through statically typed collections. Registration does not make `T` dynamic,
does not create a public pack/unpack API, and does not affect ordinary values or
collections.

An external closed value domain may likewise register a `truthiness-adapter`
whose exact type is `T -> bool`. Conditional lowering calls that adapter instead
of assuming every value of `T` is truthy. This preserves Clojure `nil` and
`false` semantics for a statically modeled external sum without exposing a
dynamic representation or changing the stored type.

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

Inference and call elaboration use the same OCaml argument matching rules.
Labels select their declared parameters independently of source order;
unlabelled arguments skip optional parameters, and explicitly supplied optional
values use the parameter's payload type. Label tokens are not positional
arguments. These constraints apply to ordinary functions, local functions,
callbacks, and record fields without requiring redundant source type hints.

A host callback's unresolved result does not discard its known parameter types.
Mutable reference updates infer the updater's ordinary application, including
source inline expansion, and propagate its result back to the same cell.
This applies equally to local cells and keyword-accessed record fields.
Dereferencing a record field propagates payload constraints in both directions;
record constructors supply their declared element type to collection updates.
Captured vectors therefore retain host record identity across callback writes
and later field reads without requiring parameter hints.
Collection callbacks may refresh record metadata only for the same type
identity. A matching short name must never substitute a different source or
host record, even when their fields are identical.
Read-only field constraints do not prefer a nominal record merely because its
total field count matches the observed fields. If several record types satisfy
the observations, the parameter stays structural unless other type evidence
selects one. Mutation additionally requires every written field to be mutable.
When a callback consumes an inferred structural row, argument adaptation reads
the incoming host record's declared fields before planning the static field
projection. Passing the callback by name must not require an explicit hint.
An atom initializer receives the expected cell payload type, not the enclosing
reference type, so collection-producing expressions keep their element types.
Anonymous reference updaters propagate their parameter constraints back to the
cell and are checked with the same payload type as their input and result.
Read-only predicates may project fields without narrowing the stored record.
Generic collection results follow their declared type-variable relationships;
an unresolved vector element must not be replaced with an arbitrary callback's
return type. Concrete argument evidence refines callback context before an
expected result shape supplies constraints, preserving the input record type.
Mutable host record fields use the same checked assignment syntax as source
records. Host metadata must confirm mutability and the stored field type.

A direct OCaml call can accept a positional LG function where a host callback
has labelled parameters. The host signature supplies the labels and parameter
order; optional callback parameters reach the LG function as `option` values.
Elaboration evaluates the callback expression once, then emits a statically
typed labelled lambda that invokes it. Both argument and result types are
checked without dynamic boxing. This boundary adaptation does not introduce
source-level labelled function syntax.

### Foreign declarations are typed

`(ffi name [argument-types ...] result-type options)` declares an ordinary
statically typed function backed by an explicit foreign ABI. The options select
one backend: `:native` names a C function, `:js` names a JavaScript binding,
and `:ocaml` names a linked C primitive using the OCaml runtime value ABI.
Target-specific declarations use reader conditionals.
The full decision and acceptance criteria are in
[ADR 011](agent-guide/011-foreign-function-interface.md).

Native bindings lower to ctypes descriptors. `:int` means C int, `:float` C
double, `:bool` C bool, `:string` a temporary NUL-terminated input string, and
`:unit` a void result. Empty argument vectors represent zero-argument calls.
Explicit unit parameters and unsupported representations are errors. Returned
strings require an ownership adapter; they cannot silently lose a required
deallocation. Library paths and foreign symbols are literal data.

`:ocaml` declarations lower directly to typed OCaml `external` bindings and
are Native-only. They require a C identifier, closed static argument/result
types, and at most five parameters; `[]` supplies the OCaml unit argument.
They accept no ctypes/JavaScript options or compiler-internal `%` primitives.
The linked stub must implement the declared OCaml value layouts, root values
across allocations, and manage any opaque custom-block lifetime. Strings retain
their length and embedded NUL bytes. This ABI does not convert OCaml values to
ordinary C scalars and must never be substituted for `:native` implicitly.

Direct Native callback parameters require `:callbacks :call`. Their typed
function-pointer descriptors preserve scalar argument/result types and source
arity. C may invoke them only during the enclosing call, on the calling thread
with the runtime lock held. Retained callbacks require an explicit managed
handle and are not implied by a direct function parameter.

Retained callbacks use `callback<fn<...>>` handles created by `:native :callback`
and released by `:native :release`. `Foreign.dynamic_funptr` roots each closure
until release. The runtime's typed static-function-pointer view uses the exact
same C signature; it does not convert through a numeric address or erased value.
The caller must unregister the callback from C before release. Released handles
are rejected before C entry, and release clears the OCaml root. This API does
not enable foreign-thread calls or runtime-lock release.

Native `pointer<T>` annotations map to typed ctypes pointers. Nullable pointer
parameters and results use `option<pointer<T>>` and the `ptr_opt` descriptor.
Pointer results require an explicit borrowed-ownership contract; that contract
does not allocate, free, or extend the lifetime of foreign storage. A plain
pointer result declares non-NULL storage. Pointer pointees remain concrete
scalar storage or recursively typed pointers; strings, functions, and LG
collections are not inferred C storage layouts.

Owned pointers use the abstract `Lg_ffi.Owned_pointer.t` handle and the source
annotation `owned-pointer<T>`. A closed Live/Released state retains the exact
pointee. The allocation declaration specifies `:ownership :owned` and a literal
`:release` symbol resolved in the same library. Passing a handle into C checks
that it is live. The `:native :release` operation releases once and invalidates
the handle; repeated release is harmless. NULL cannot become a live owned
handle. Explicit release and scoped cleanup determine lifetime, not GC timing.

Every foreign parameter and result retains its declared static type. An FFI
declaration is not permission to erase vectors, maps, records, options,
callbacks, pointers, or type variables into a universal runtime value.
Unknown options and incompatible targets must fail before host code generation.

JavaScript bindings target Melange and lower to typed external declarations.
`:module` imports a module and `:scope` traverses literal property names before
calling the selected function. They retain fixed source arities and ordinary
first-class function behavior. `(extern-type name)` declares a distinct opaque
host identity without exposing its representation. Constructor, method, property,
and indexed operations preserve that identity. Ordinary LG records and variants
are not interchangeable with opaque foreign objects.

JavaScript FFI accepts recursively typed homogeneous arrays and direct callback
parameters. Callback arities are uncurried only at the foreign boundary; a
zero-argument source callback stays zero-argument in JavaScript. Explicit return
adapters map null/undefined to a statically typed option. A final array may be
spread with `:variadic true`. Indexed array reads and writes retain the array's
element type; an explicit nullable read may return `option<Element>`.

JavaScript object builders use `:js :object`, one declared record argument,
and an opaque result. Field projections feed a typed `mel.obj` external;
the record is never cast to a host object. `:rename` selects property names,
and `:optional` explicitly unwraps option fields and omits absent properties.
Unknown fields, duplicate property names, and the special literal key
`__proto__` are rejected. Each invocation creates a fresh object.

The compiler keeps Native and Melange package search paths separate and resets
OCaml interface caches when changing target. A previous Melange compilation
must not make Native ctypes load JavaScript standard-library interfaces.

### Timing and output

`time` is a source macro that evaluates its expression once and returns that
same static value after printing elapsed milliseconds through `prn`. Failed
evaluation propagates the exception and does not print a success timing line.
The output respects lexical capture through `with-out-str`.

`system-time` measures monotonic elapsed milliseconds, including time spent
waiting; process CPU time is not a valid implementation. Native uses a typed
POSIX clock primitive, Melange uses `performance.now`, and js_of_ocaml provides
the matching JavaScript clock primitive. Runtime-specific elapsed formatting
lives in an ordinary source function because macro reading uses the host
dialect: Native follows Clojure float display, JavaScript follows the six
fractional digits in ClojureScript's `time` macro.

Formatting arguments use a closed scalar sum with concrete payloads. Decimal
formatting rounds decimal digits half-up, preserving Clojure results at values
such as 1.005, rather than delegating rounding to the target's binary printf.
Fixed, scientific, and significant-digit formatting share one digits/point
representation. Width and precision on strings count UTF-16 units while
preserving LG's byte-string representation.

A literal format string can constrain unresolved numeric arguments: `d`, `o`,
and `x` conversions require integers, while `e`, `f`, and `g` require floats.
Inference and rendering share the format parser, including positional indices
and previous-argument reuse. Dynamic format strings do not provide this
evidence; conflicting conversion requirements do not choose an arbitrary type.
Already concrete arguments retain their existing runtime validation, and
declared polymorphic signatures remain rigid.

`print`, `println`, `pr`, `prn`, `newline`, and `flush` return `nil`.
Output primitives may return OCaml unit internally; source functions must keep
the Clojure return contract. String-producing variants return strings, including
their zero-argument empty-string or newline cases. `flush` flushes standard
output and is a no-op for a captured string buffer. Custom rendering consumes
the writer's contents and ignores the printer callback's return value.

Bytecode workers use Dune's `byte_complete` mode so the monotonic clock primitive
is linked into their runtime. Dynamic compilation runners explicitly reference
the clock module, ensuring newly loaded code can call the primitive without
relying on a separately installed shared library.

### Truthiness remains static

Clojure truthiness treats only `nil` and `false` as false. Conditions and
`not` must preserve that behavior without erasing their operands into a
universal runtime value. A concrete boolean is negated directly; concrete
always-truthy values and `nil` may be reduced after preserving operand
evaluation; options, sequences, closed EDN values, and documented external
boundaries use their precise static truthiness operation. An unresolved
function parameter acquires a truthiness capability during inference, and the
witness remains limited to that generic function boundary. Ordinary inlined
`not` calls must not allocate a witness pair or callback.

Direct source `compare` calls follow the same boundary rule. When both operands
have one concrete comparable type, the source wrapper expands to the static
comparator intrinsic and must not allocate a comparable witness pair. A
first-class or genuinely generic `compare` value retains its typed capability;
inlining does not erase that polymorphic boundary.

Direct source `name` calls also specialize at the call site. Concrete keyword,
symbol, and string operands use their static coercion without a witness;
unresolved generic operands retain `INameCoercion`, and unsupported concrete
operands preserve the source function's runtime `Invalid_argument` behavior.

## DataScript model

DataScript is a known domain and therefore is not an open dynamic boundary.
Follow the `datascript-ocaml` approach while retaining reusable upstream
algorithms.

### Repository ownership

The LG repository owns the compiler, runtime, and standard library only. It
must not contain DataScript or persistent-sorted-set implementation sources or
their test suites.

The standalone `datascript-lg` project owns the DataScript LG sources, closed
OCaml runtime types, upstream compatibility tests, differential fixtures,
audits, and benchmarks. It consumes LG through the installed `lg` and
`lg-test` packages. The standalone `persistent-sorted-set-lg` project owns the
persistent sorted set implementation, tests, audits, and benchmarks;
`datascript-lg` consumes it through the installed
`persistent-sorted-set-lg` package.

LG keeps a repository-boundary test that verifies no copied implementation
remains here without introducing a reverse dependency on downstream packages.
DataScript behavior and performance changes are developed and tested in the
owning standalone project.

### Upstream behavior is authoritative

The authoritative compatibility baseline is the Logseq DataScript fork at
commit `3f141af97b70e1f14c65eaa119acd822ebece37e`. The repository URL, source
mapping, generated public API manifest, and differential fixture inventory are
recorded in `datascript-lg/test/datascript/UPSTREAM.md`.

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
- Pull frame dispatch returns the same one- or two-frame sequence as a typed
  list and prepends it with `List.rev_append`. Frame order and transitions are
  unchanged; the port does not build a temporary RRB vector merely to fold it
  back into the list-shaped stack.
- Direct `lookup-entity` calls inline its typed implementation instead of
  crossing the forward-declaration cell required by `deftype` protocol methods.
  Recursive entity benchmark traversal consumes the closed
  `EntityReferenceSet.items` vector in its existing order, avoiding a generic
  seq-to-vector round trip without changing the traversed tree.
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
in `datascript-lg/test/datascript/benchmark/RESULTS.md`.

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

For source-declared protocols, the nonempty `__lg_next_seq<T>` representation
is the same static `:seq` receiver domain as `seq<T>`. Protocol satisfaction,
witness construction, and direct implementation lookup must normalize both to
the source `:seq` receiver. Compiler-owned protocols keep their explicit host
receiver identities and are not changed by this source-protocol normalization.

An open protocol method return is an interface constraint, not permission to
erase a concrete implementation return. Direct record/type implementations
infer their body return first and register that concrete function type unless
the declared return actually needs contextual construction, such as a function
return or a closed variant value.

A protocol method whose result preserves its receiver type declares `:self`
as its return annotation. The compiler substitutes the statically witnessed
receiver type at each call; it must not merge concrete implementations into a
universal return value. This is the protocol form for representation-preserving
operations such as string-or-symbol name transformations.

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

### Core name conflicts

Top-level value, function, and macro definitions that reuse a core name must
explicitly declare `(:refer-clojure :exclude [name])` in their namespace. The
compiler reports the collision at the definition instead of silently removing
a core binding or macro. Namespace exclusions are processed before definitions
are dependency-ordered. Lexical parameter and local-binding shadowing remains
unchanged; qualified core calls remain available after exclusion.

Sequence call specialization checks the resolved binding against the core
binding, not the unqualified spelling of the callee. Qualified user functions
and lexical aliases named `map`, `mapv`, `seq`, `reduce`, or `filter` retain
their own behavior.

### Recursive functions and inferred interfaces

`letfn` accepts a nonempty vector of fixed-arity local function definitions.
Every name is visible in every definition and in the enclosing body. Dependency
components are inferred before their users: a mutually recursive component is
monomorphic internally and generalized after checking, while independent
components retain independent polymorphic uses. Lexically bound parameter and
local names are not dependencies on names they shadow. Duplicate function
names and incompatible recursive applications are errors.

Recursive inference retains equations between call arguments, parameters, and
branch results. Capability evidence constrains the underlying value; it must
not create a recursive storage equation between a value and its witness pair.
Concrete result information is preserved when a result follows a parameter,
including a typed collection passed an initially empty collection.

Module-local recursive `defn` and `defn-` support inferred parameters and return
types. An explicit return annotation remains a checked contract and does not
require otherwise inferable parameter annotations.

Inferred OCaml interfaces omit private `defn-` values, including their generated
arity helpers, inside nested modules and functor results. The implementation
retains those values for internal calls. Public signatures continue to come
from OCaml's checked types, preserving polymorphism and abstract type identity.

A signature include can constrain a type declared by the included signature:

```clojure
(module-signature ValueSig (type item) (val value :item))
(module-signature IntSig (include ValueSig (with-type item :int)))
(module-signature IntValue (include ValueSig (substitute-type item :int)))
```

`with-type` preserves the type declaration and makes its equality explicit.
`substitute-type` replaces its uses and removes the declaration from the result.
Parameterized declarations use the same optional parameter vector as type
definitions, for example `(substitute-type box [a] :option<a>)`. These constraints
lower to OCaml signature constraints; OCaml checks compatibility with the
original declaration. Frontend metadata applies the same substitutions so
functor calls retain the concrete parameter and result relationships. Unknown
type names and mismatched parameter arities are errors. Nested module paths are
supported, for example `(with-type Inner.item :int)` and
`(substitute-type Inner.item :int)`. Functor applications substitute actual
argument module paths into exported values and type aliases. Private values
remain hidden through module aliases, applications, and explicit ascriptions;
a named signature is expanded in the inferred interface when filtering requires
it.

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

When the consumer requires a different static element representation, that
sequence adapter performs the checked element conversion lazily. This includes
polymorphic-variant payload adaptations, not only record projections.
The original collection storage remains unchanged.

Source interfaces may name that storage relationship as
`seqable<Element; Storage>`. This is required for higher-order collection
functions whose sequence witness crosses another source function boundary;
the stored value is `Storage`, never a nested or copied capability pair.

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

Before merging a compiler or runtime change, verify:

- no new source-level dynamic type or escape hatch was introduced;
- known heterogeneous values use a closed sum;
- absence uses `option`;
- record and callback fields remain concrete;
- no Java/JVM compatibility alias was added;
- generated code does not use `Obj.magic`;
- generated DataScript hot paths contain no `Runtime_dynamic` references;
- focused rejection tests cover invalid heterogeneous and interop cases;
- when the change affects DataScript, the standalone `datascript-lg` runtime,
  parity, differential, and relevant benchmark gates pass;
- when the change affects persistent sorted sets, the standalone
  `persistent-sorted-set-lg` Native, Melange, js_of_ocaml, audit, and benchmark
  gates pass.

### First-class module values

`(pack-module Implementation Signature)` creates a statically typed OCaml
module package. Its annotation is `:module<Signature>`. Packages can be returned
from functions, selected by ordinary control flow, and stored in homogeneous
collections. `(let-module [M package] body...)` unpacks a package into a lexical
module binding. Signature members retain their declared types, including abstract
members qualified by the local module. The OCaml checker enforces signature
inclusion and prevents these local abstract types from escaping. This is the
traditional first-class module mechanism shared by Native and Melange; it does
not use OCaml 5.5 module-dependent functions.

### Explicitly polymorphic record fields

A field declaration `(run (forall [a] :fn<a;a>))` quantifies `a` within that
field. One record value can expose the function at multiple argument types.
Construction must supply an implementation that is general enough for every
quantified type; OCaml checks this against an explicitly polymorphic record
label. Field quantifiers are excluded from the containing record's free type
variables, and substitution preserves their scope and avoids variable capture.
A field quantifier must have a different name from the record's own type
parameters.

### Named variant constructor identity

Calls to local named variant constructors retain their inferred result type in
generated OCaml, including payload and zero-argument calls. A constructor from
another namespace with the same emitted name must not change the selected
variant inside a conditional, option, or collection. This constraint comes from
the resolved binding; callers do not need a source type hint.

### GADT constructor indices and existential payloads

A `type-variant` constructor can end with `(returns :family<index>)` to specify
its result index. For example, `(IntExpression :int (returns :expression<int>))`
constructs only an `expression<int>`. An optional constructor-local parameter
vector, such as `(Pack [a] :a :fn<a;string> (returns :packed))`, scopes payload
type variables to that constructor. Variables absent from the result are
existential when the constructor is matched.

A generic eliminator uses an explicit signature, for example
`(signature evaluate [a] :fn<expression<a>;a>)`. GADT patterns introduce
branch-local type equations; they do not specialize the enclosing recursive
function. Generated explicitly polymorphic bindings that use these equations
introduce OCaml locally abstract types. OCaml checks index consistency and
rejects existential types that escape their pattern scope. Constructors and
payloads retain ordinary static OCaml representations on Native and Melange.

### Polymorphic variants and row types

`(tag Ready)` constructs a constant tag; `(tag Value 42)` constructs a tag with
one statically typed payload. Patterns use the same forms. Matching can infer an
upper row and its payload types from the branches, without a parameter annotation.
An exhaustive match limits admissible tags; a catch-all permits additional tags.
The generated representation is an OCaml polymorphic variant, not a runtime
map or universal value.

Annotations use `:variant<Ready;Value:int>` for a closed row,
`:variant-open<Value:int>` for a lower bound that permits additional tags, and
`:variant-upper<Ready;Value:int>` for an upper bound. Payload types can contain
declared type parameters. Functions can accept and return open rows, and OCaml
checks the final row constraints and shared return relationships. Compiler type
metadata preserves ordinary variant rows, including required tags inside an
upper bound read from an OCaml interface.

Known tags print as `(tag Name)` or `(tag Name payload)`. A tag outside the known
part of an open row uses the opaque `<variant>` display; printing does not inspect
or erase an unknown payload.

### Library-selected literals

`lg.literal/build` accepts a literal map from kinds to constructor symbols and a
source form. It expands that explicit form into ordinary statically checked
constructor calls. Scalar kinds are `:nil`, `:bool`, `:int`, `:float`, `:string`,
`:keyword`, `:symbol`, and `:char`. Collection kinds are `:vector`, `:list`,
`:set`, and `:map`; collection constructors take an OCaml list, and map entries
are tuples. Keyword payloads retain their namespace without the leading colon.
A library supplies only the kinds its closed representation supports.

`(unquote expression)` embeds an already constructed value. `(unquote :kind
expression)` passes a typed expression to the selected scalar constructor.
Generated lexical bindings evaluate expressions once in source order, including
nested map keys and values. Unsupported kinds/forms are expansion errors;
constructor argument compatibility is checked normally. Ordinary collection and
quote semantics do not change. The compiler contains no DataScript-specific
literal handling.
