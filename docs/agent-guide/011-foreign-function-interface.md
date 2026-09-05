# ADR 011: Typed Foreign Function Declarations

Status: Implemented; final validation recorded below

Date: 2026-09-05

## Context

LG should make foreign bindings as convenient to write and use as Melange,
including native C bindings through ctypes. Existing OCaml package interop is
useful but requires a second source language for common bindings. The
[research report](011-foreign-function-interface_report.md) compares upstream
semantics and identifies the compiler integration points.

The goal is a typed binding authoring surface, normal LG calls, clear compiler
errors, and executable examples for JavaScript and native C. It does not mean
that C and JS have interchangeable representations or that every Melange
attribute becomes public LG syntax.

## Decision

### One declaration, ordinary calls

```clojure
(ffi floor [:float] :float {:js "floor" :scope ["Math"]})
(ffi basename [:string] :string
  {:js "basename" :module "node:path"})
(ffi c-abs [:int] :int {:native "abs"})

(floor 3.8)
(basename "/tmp/example.txt")
(c-abs -42)
```

The grammar is `(ffi name [argument-types ...] result-type options)`.
Every argument and result is explicit. Empty arguments mean a zero-argument
source function. The compiler supplies an OCaml unit argument internally.
Bindings are ordinary first-class functions, with normal namespace exports,
aliases, higher-order use, call checking, and incremental compilation.

Use exactly one `:js` or `:native` selector. Named functions use string selectors;
indexed JS operations use the keyword selectors described below. Options are compile-time
data, validated into a closed descriptor. Unknown keys, duplicate keys, malformed
values, incompatible operations, and wrong target use are errors. Strings are
literal foreign names, never source fragments. Existing reader conditionals
choose target-specific declarations in shared source.

### JavaScript operations

Default operation is a function call. `:module` selects an imported module;
`:scope` is a vector of property names. `:kind` selects an explicit operation:

| Kind | Meaning | Signature constraint |
| --- | --- | --- |
| `:call` | Function call | Any fixed arity |
| `:new` | Constructor | Returns a declared host type |
| `:send` | Method call with `this` | Receiver is the first argument |
| `:get` | Named property read | One receiver argument |
| `:set` | Named property write | Receiver and value, returns unit |
| `:get-index` | Indexed property read | Receiver and key |
| `:set-index` | Indexed property write | Receiver, key, value; returns unit |

Receiver operations cannot also select a module/global scope. A property getter
is a function so reading a mutable property happens at the call, not when the
binding module initializes. Named globals can be exposed with a zero-argument
getter using a scoped property operation in a later extension; do not confuse
calling a global function with reading a global value.

Indexed operations have no property name. Their compact selectors are
`{:js :get-index}` and `{:js :set-index}`, with no separate `:kind` option.
All other operations retain the string selector shown above. Index keys are
statically `:int` or `:string`; writes return `:unit`. This avoids inventing a
dummy foreign name that would be silently ignored.

`(extern-type element)` declares an opaque host identity. Distinct declarations
are not interchangeable. Existing aliases to concrete OCaml host types remain
available. No generic `js/any` or universal object type is introduced.

```clojure
(extern-type element)
(ffi query-selector [:element :string] :option<element>
  {:js "querySelector" :kind :send :return :nullable})
(ffi set-text! [:element :string] :unit
  {:js "textContent" :kind :set})
```

Scalar, homogeneous array, and opaque-host representations are checked
recursively. RRB vectors, maps, lists, arbitrary records, and variants are not
implicitly JS values. Nullable result adaptation requires an option result
and `:return :nullable`, `:null`, or `:undefined`. Nested options need explicit
adapters. Callback parameters retain their function type and are uncurried at
the foreign boundary; zero-argument callbacks preserve their source arity.
`:variadic true` spreads a final homogeneous array, preserving fixed argument
order. It does not make the LG function itself variadic.

Object construction uses `(ffi make [:input-record] :host-type {:js :object})`.
The single input is a declared typed record; the result is an opaque extern-type.
Every record field becomes a property, using its source name by default.
`:rename {:source-field "foreignName"}` overrides individual property names.
`:optional [:field]` requires an option field and omits the property for `nil`;
present values are unwrapped. Unmarked option fields are rejected. Unknown
fields, duplicate property names, `__proto__` (special literal semantics),
and conflicting call adapters are errors.
The builder creates a fresh object on each call and preserves the input record.
It lowers to a typed `mel.obj` external and a record-projection wrapper, with
optional labelled arguments and explicit `mel.as` property names. A prototype
compiled with installed Melange 7.0.1-55 verified opaque results, renamed
properties, and optional-property omission before implementation. No record
representation cast or heterogeneous map conversion is involved.

### Native C operations

`:native` names a C function and is supported on Native. `:library` optionally
names a shared library; otherwise symbol lookup uses the process namespace.
The initial backend emits typed ctypes descriptors and `Foreign.foreign`.
It binds once per declaration, not once per call. Required ctypes packages
participate in normal dependency discovery and build/link configuration.

The primitive ABI mappings are explicit: `:int` is C int, `:float` is C double,
`:bool` is C bool, `:string` is a temporary NUL-terminated string, and a result
of `:unit` is C void. A zero-argument C function gets a void descriptor, not
one fictitious C argument. Unit is rejected as an explicit parameter. Width-
specific integer types and C float need distinct descriptors; they must not
silently reuse int/double because their LG values look similar.

Native pointer bindings must preserve the pointee type, nullability, and
ownership contract. Temporary strings cannot be retained or modified by C.
`:pointer<T>` names a typed C pointer; `:option<pointer<T>>` explicitly maps
NULL to nil. Direct pointer results require `:ownership :borrowed`, declaring
that the C owner keeps the memory alive and LG does not free it. Plain pointer
results declare a non-NULL contract; nullable C results must use the option
form. Pointees initially use scalar storage types (int, double, bool, char),
void, or recursively typed pointers. Strings and function types are not storage
pointees. Pointers retain their concrete ctypes type at every host boundary.
Owned allocations use a separate managed lifetime as described below.
Managed owned pointers use an abstract typed handle with `Live pointer | Released`
state and one typed release function. Explicit release is idempotent; extracting
the address after release fails before entering C. A scoped helper releases on
both normal return and exceptions. Explicit release is the primary contract;
this stage does not make release timing depend on GC finalizers. The handle
preserves the pointee type and never stores an untyped numeric address.
An owned allocation declares `:owned-pointer<T>` as its result and
`:ownership :owned :release "deallocator"` as options. The deallocator is
resolved from the same library with signature `T* -> void`. Parameters declared
as `:owned-pointer<T>` borrow a live handle for the call; they cannot implicitly
become raw pointers. `(ffi close [:owned-pointer<T>] :unit {:native :release})`
releases the handle without looking up another C symbol. Allocation returning
NULL raises an error rather than creating a live NULL handle.
Owned buffers require allocation/release operations with an explicit lifetime.
Returned C strings cannot silently discard an ownership obligation. Structs
and unions use verified ctypes layout descriptions or explicit OCaml adapters,
not inferred LG record memory layouts.

Callbacks passed for the duration of one call and callbacks retained by C are
different APIs. The latter require a rooted handle with explicit release.
Call-duration callbacks use direct `:fn<...>` parameter types and require
`:callbacks :call` on the native declaration. Their signature lowers to a
typed `Foreign.funptr`; C must finish calling them before the foreign call
returns. The runtime lock remains held and callbacks run on the calling thread.
Callback parameters/results use the same scalar ABI rules, with no nested
callbacks or returned function pointers. Zero-argument callbacks retain their
source arity. Retention requires the separate managed-handle API; it is not
enabled by a function signature alone.
Retained callbacks use `callback<fn<...>>`, an abstract typed handle.
`(ffi retain [:fn<...>] :callback<fn<...>> {:native :callback})` creates the
handle; native registration functions accept that handle type directly.
The `:native :release` operation also releases callback handles. The caller
must unregister the callback from C before release. A released handle cannot
be passed into another C call. The runtime uses `Foreign.dynamic_funptr` to
root the closure and preserve its exact signature. Its typed `static_funptr`
view describes the same C function-pointer ABI, with no universal value or
numeric address conversion. Foreign threads and runtime-lock release remain
unsupported; C invokes these callbacks on the calling OCaml thread.
Runtime-lock release and foreign-thread callbacks are not inferred from a
signature. Until these policies have implemented tests, unsupported options
must fail instead of pretending to honor them. C variadic functions likewise
need ABI-aware promotion and are not the JS array-spread operation.

### Compiler architecture and diagnostics

Parse/check declarations into a closed FFI descriptor. Register a precise
`TFn` in the existing environment. Emit a dedicated lowered declaration and
construct the OCaml Parsetree structurally. Do not interpolate arbitrary OCaml,
add dynamic call dispatch, use unsafe casts, or weaken host assignability.

Dependency ordering, source locations, module visibility, interface inference,
and state artifacts must understand the declaration. Report errors against
the declaration/option and retain normal argument mismatch diagnostics at
calls. A target mismatch must explain which target the binding requires and
point to reader conditionals for shared files. Native bindings must never
cause ctypes to be loaded for a JS-only program.

### Target policy

JS declarations initially target Melange. Native declarations target Native.
js_of_ocaml gets an explicit unsupported-target diagnostic until a separately
validated backend exists. This is an ABI boundary, not an attempt to emulate
JS on Native or call C through a browser. Existing OCaml interop remains usable
on all targets with their existing package availability rules.

## Implementation sequence

### Additional requirement: Clojure timing and output

The user also requires `time` and the print family. Existing source stdlib
printing must remain available alongside FFI. `time` is a source macro:
evaluate one expression once, measure elapsed milliseconds, print the readable
`Elapsed time: ... msecs` string with `prn`, and return the original static
value. If evaluation throws, propagate the exception without a success timing
line. Printing must follow the existing output binding, including `with-out-str`.

Native `system-time` currently measures process CPU time via `Sys.time`; replace
it with a monotonic clock that includes waits. Melange uses `performance.now`.
Native code uses a small typed clock primitive, not JVM aliases or an FFI
dynamic value. js_of_ocaml must have a corresponding JavaScript primitive.
The time origin is unspecified; only differences are meaningful.

Sources: [Clojure core time/printf implementations](https://github.com/clojure/clojure/blob/master/src/clj/clojure/core.clj),
[POSIX monotonic clock](https://pubs.opengroup.org/onlinepubs/000095399/functions/clock_getres.html).

The implementation comparison uses local sibling checkouts: Clojure
`53b73bcc8cff905f4002ff8593db783b11060fa7`,
`src/clj/clojure/core.clj` (`time`, print family, `format`, `printf`), and
ClojureScript `5c6ef531604662afbb33dc1b553d7602634d9656`,
`src/main/clojure/cljs/core.cljc` (`time`) and
`src/main/cljs/cljs/core.cljs` (`system-time`, print family).
ClojureScript timing renders six fractional digits; Native keeps Clojure's
ordinary floating-point rendering. Both use `prn`, so normal readable output
quotes the timing string.

The output acceptance surface is `print`, `println`, `pr`, `prn`, `printf`,
`print-str`, `println-str`, `pr-str`, `prn-str`, `newline`, `flush`, and
`with-out-str`. Preserve spacing, readable escaping, line termination, return
semantics, evaluation order, and output capture. `format`/`printf` need static
format checking or a closed typed argument domain; Java Formatter and universal
argument boxing are not permitted. The existing print functions now return nil;
`newline` participates in lexical output capture and `flush` delivers buffered
standard output. Formatting support is implemented through the closed argument domain below.

Formatting uses a closed internal argument sum (text, integer, decimal,
boolean, character, nil), populated by static elaboration. It is not a source
type or a general boxing API. Format strings may be runtime strings. The
portable scalar surface is `%s`, `%b`, `%c`, `%d`, `%o`, `%x`, `%e`, `%f`, `%g`,
their uppercase forms, `%%`, and `%n`, with explicit/relative argument indices,
width, precision, and applicable flags. JVM object hashing, date objects,
locale objects, and `Formattable` dispatch are outside the static language's
object model. Invalid combinations and argument kinds fail rather than coercing
arbitrary data. `printf` writes the completed format through ordinary `print`
and returns nil, including under `with-out-str`.

The monotonic native clock is packaged as a standalone Dune foreign archive:
Dune 3.24.2 crashes when `foreign_stubs` is attached directly to this library's
combined Native/byte/Melange modes. Bytecode workers use the supported
`byte_complete` executable mode and explicitly link the clock module for later
dynamic loading. This also removes dependence on an installation-time DLL
search path when running the local REPL or generated test programs.

Timing/output validation includes actual Native, Melange, and js_of_ocaml
execution; a wait-inclusive monotonic clock test; nil-return and zero-argument
printer cases; readable output; nested output capture; failed/nested timing;
and a pipe-based test proving that `flush` sends buffered bytes before exit.

1. Research and write this ADR before implementation.
2. Add declaration parsing, primitive native calls, dependency discovery,
   real C execution, and rejection tests.
3. Add opaque identities and Melange calls/imports/scopes, constructors,
   receiver and index operations, real Node execution, and rejection tests.
4. Add JS callback/nullable/array-spread/object-builder adapters; amend the
   builder decision using prototype evidence.
5. Add typed native pointers, library loading, lifetime guidance, and callback
   support with actual C fixtures; verify ABI-specific scalar mappings.
6. Finish user documentation, examples, namespace/interface/cache coverage,
   and regression validation.

Within each implementation stage, write behavioral tests first, establish the
expected failure, implement, run tests, simplify, and rerun relevant tests.
The whole feature remains incomplete until the acceptance matrix is met;
finishing primitive native calls alone does not fulfill the objective.

### Current implementation evidence

The public form is `ffi`, as requested during implementation; there is no
`defextern` or `defexternal` alias. Native scalar declarations and explicit
library selection are implemented. `test/ffi_tests.ml` compiles a real shared
C library and executes generated OCaml against it, checking argument order,
double and boolean ABI behavior, empty/nonempty strings, zero-argument calls,
void results with observable mutation, and a first-class alias.

The same test executable verifies root/nested-module dependency discovery,
inactive reader branches, module export, interface inference, incremental
compilation, REPL definition classification, and invalid declarations/calls.
Compiler state format 19 invalidates artifacts predating opaque types and adapters.
The JS tests also verify interface inference and a saved-state round trip followed
by incremental compilation. The remaining evidence is recorded below.
See [the user guide](../ffi.md) for the currently supported
surface and explicit ABI boundaries.

JavaScript scalar calls lower to Melange external declarations, preserving
module import, nested scope, fixed arity, and first-class aliases. The real Node
fixture verifies left-to-right argument evaluation, zero-argument calls, scalar
results, void mutation, namespace exports, global calls, and `node:path` imports.
Wrong targets and representations are rejected before code generation.

Research for this stage used the installed Melange 7.0.1-55 source, particularly
`jscomp/test/bs_qualified.ml`, and a compiling `melc` prototype. This version
does not accept the legacy `mel.val` attribute; an ordinary external is sufficient
for a global call. Named module and scope attributes are built structurally.

Running the JS and Native fixtures in one process exposed stale target state in
OCaml interface lookup. Package paths are now retained separately per target;
switching target resets the host interface cache and search path. A nested FFI
declaration's name is also excluded from value dependencies, so it cannot
incorrectly depend on an unrelated outer definition of the same name.

Validated on the existing `5.5.0` opam switch:

- `dune exec test/ffi_tests.exe`: ten Native/JS execution, validation,
  integration, and artifact groups pass.
- `dune exec test/compiler_gate_tests.exe`: target, concurrency, and fuzz
  regressions pass. The stale reader-conditional diagnostic fixture now matches
  the parser's existing diagnostic.
- `dune exec test/compiler_support_performance_tests.exe`: passes.
- `dune runtest`: the full repository test suite passes, including compiler,
  prototype/production REPL, stdlib, inventory, and cross-runtime checks.
- `dune exec lg -- --run examples/ffi_native.cljc`: prints `42`, confirming
  automatic package discovery through the actual CLI execution path.
- `dune build lg.opam bin/lg_cli.exe test/ffi_tests.exe`: succeeds.

The Node fixture additionally exercises opaque constructor identities, method
`this`, mutable properties, indexed properties, module-local opaque declarations,
type aliases, zero/two-argument callbacks, a retained closure, all three nullable
return adapters, empty/nonempty array spreading, and type-preserving indexed
array writes. An explicit undefined-to-option indexed read handles out-of-bounds
access. Opaque declarations also survive REPL classification and artifact restore.

These adapters use Melange's typed `mel.uncurry`, `mel.return`, and
`mel.variadic` attributes. The test emits Melange's actual option and spread
runtime modules from their installed compiled artifacts; it does not substitute
test implementations of either conversion.

## Acceptance matrix

| Requirement | Authoritative evidence |
| --- | --- |
| Clean binding authoring and normal use | Documented LG examples compile and run without handwritten glue for common JS/C calls |
| Static errors | Wrong arity/type, open/dynamic payload, invalid options and target mismatch fail at LG source locations |
| Namespaces and state | Bindings survive require aliases, module exports, interface inference, and incremental/cache round trips |
| Native primitive ABI | Real C fixture checks zero/multiple arguments, int/double/bool/string input and void output |
| Native library selection | Fixture loaded by explicit path; missing library and symbol produce useful failures |
| Native pointer ownership | Allocation/use/release and nullable pointer examples; no pointer type erasure |
| Native callbacks | Actual C invocation verifies argument order and result; retained callback stays rooted until release |
| JS operations | Node fixture verifies imports, scopes, constructors, receiver identity, named and indexed mutation |
| JS adapters | Tests cover zero/multiple callback arguments, null/undefined/non-null results, empty/nonempty spread arrays, optional object fields |
| Build integration | Required packages detected; generated Native executable and Melange output build with documented commands |
| Design invariants | No unsafe casts, public dynamic escape hatch, or implicit collection/record erasure |
| Regressions | Focused interop tests and repository compiler suite pass; affected target builds pass |

## Consequences

Binding authors still assert the external library's signature: LG cannot prove
that a JS package or C shared object actually implements that contract. LG can
check internal use and supported representations, then test known fixtures.
The explicit descriptor keeps target-specific behavior visible while call
sites remain ordinary LG. Advanced ABI cases can continue using typed OCaml
adapter packages; they are not justification for unsafe source escape hatches.

Object-builder acceptance now includes actual Melange/Node execution for required
fields, renamed properties, omitted and present optional fields, first-class
aliases, and fresh object identity. Rejections cover unknown and duplicate fields,
invalid input/result types, unsupported adapters, and the special `__proto__`
property. Module builders survive compiler-state serialization and restoration.
The module test exposed an unqualified record constructor type for slash-qualified
source names; exported module record metadata now qualifies the type, and
exported FFI signatures qualify their record parameter types. Namespace records
keep their existing flattened representation.

Native direct callback acceptance now includes actual shared-library execution
for zero/two arguments, captured values, void side effects, and double results.
Missing lifetime policy, retained-policy misuse, nested callback signatures,
explicit unit arguments, returned strings, and returned function pointers are
rejected. The compiler descriptor is a closed recursive native ABI type.

Owned-pointer execution now covers real C allocation, borrowing through a typed
handle, matching deallocation, idempotent release, and rejection before C entry
after release. The independent lifecycle test additionally verifies cleanup on
normal/exceptional scope exit, escaped-handle invalidation, and NULL rejection.
The native-only `lg.ffi` library keeps this lifecycle implementation separate
from the portable runtime. Native dependency discovery includes the library;
FFI tests depend on the local package installation artifacts.

Retained callback acceptance includes real C registration and invocation,
post-GC closure survival, weak-reference evidence of capture collection after
release, idempotent release, and refusal to pass a released handle into C.
Source FFI tests additionally cover callback-handle construction, matching
registration signatures, invocation, unregistering, and release.

Formatting Unicode width and precision now use UTF-16 code units while retaining
LG's UTF-8 byte-string representation on all three generated targets. Tests
exercise BMP text and a supplementary character under width/precision, plus
mixed scalar formatting and captured printf, in Native, Melange, and
js_of_ocaml. The focused cross-runtime alias is independent of runtest; runtest
depends on it in one direction. Byte-complete REPL test executables now have
explicit `.bc.exe` test actions. The surface inventory classifies the new
`__lg_format` and `__lg_flush_output` internal operations. The final full-suite rerun includes the formatting conformance work.

## Final validation

All commands use opam switch `5.5.0`, on `main`, with no separate worktree.
`dune runtest` passes. The ten-group FFI executable additionally verifies missing
library/symbol diagnostics and Native/Melange namespace require aliases.
`ffi_lifetime_tests` proves owned-pointer cleanup and retained-callback GC
lifetimes. The three-target execution fixture verifies timing, print return
values/capture, decimal rounding, Unicode, and option formatting. Native and JS
CLI examples were recompiled and executed with their documented outputs.
The prototype REPL now shares the production artifact reader rather than
hardcoding a state version. No DataScript algorithms were modified.
