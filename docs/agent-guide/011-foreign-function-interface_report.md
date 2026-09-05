# FFI research: Melange and ctypes

Date: 2026-09-05

## Repository findings

The starting revision is `c9e61d8` on `main`. LG has explicit OCaml package
imports, type aliases, external records, signatures, and host calls. It has no
source declaration for a JavaScript external or a C ABI function.

Relevant integration points:

- `src/top_level_elaborator.ml` registers typed declarations in the ordinary
  compiler environment.
- `src/dependency_graph.ml` orders definitions and named type dependencies.
- `src/lowered.ml` models declarations; `src/ocaml_parsetree.ml` emits actual
  OCaml syntax trees. This is preferable to interpolating generated code.
- `src/toolchain.ml` discovers package dependencies before checking generated
  OCaml. Native FFI dependencies must participate here.
- `src/target.ml` already supports reader conditionals for Native, Melange,
  and js_of_ocaml. These targets do not share one foreign ABI.
- `docs/design.md` requires static signatures, explicit host boundaries, and
  closed heterogeneous domains. FFI cannot introduce a universal JS value.

The shell's default opam switch `5.5` lacks the build dependencies. The existing
`5.5.0` switch contains Dune 3.24.2, Melange, and `ctypes-foreign`; use
`opam exec --switch=5.5.0 -- ...` for validation. No dependency installation is
needed to begin experiments. Installed package metadata identifies Melange
`7.0.1-55` and ctypes-foreign `0.24.0`.

## Melange

Melange expresses foreign declarations with OCaml `external` plus attributes.
Its attribute vocabulary covers module imports, scoped globals, constructors,
methods, property access, index access, object construction, array spreading,
and nullable returns. Callback arguments can request automatic uncurrying.
This is a useful semantic model for LG; copying the OCaml attribute syntax
into Clojure metadata would not improve authoring.

Source: [Melange attributes](https://melange.re/v5.0.0/attributes-and-extension-nodes.html).

Object methods need receiver-aware calls. Property reads and writes are
operations, not aliases for detached functions. Object-builder labels determine
property names, and optional labels omit properties. Nullable return conversion
can accept either null, undefined, or both; LG should default to both when a
binding explicitly requests nullable conversion, retaining a precise option
payload.

Source: [JavaScript objects and values](https://melange.re/v5.0.0/working-with-js-objects-and-values.html).

Melange arrays have a JS array representation, but LG vectors use RRB storage.
Lists, options, variants, and 64-bit integers also have representation details
that must not be mistaken for arbitrary JavaScript values. A closed source
type is necessary but not sufficient to prove ABI compatibility. FFI must
validate supported representations recursively, with explicit adapters for
the others. Ordinary LG integers are suitable for integer APIs; JS number APIs
such as time and fractional arithmetic should use floats.

Source: [Runtime representations](https://melange.re/v5.0.0/data-types-and-runtime-rep.html).

The public documentation consulted is version 5.0.0; this repository requires
Melange >= 6.0. Actual generated declarations must also be compiled and run
with the installed compiler. Documentation alone is not an acceptance test.

### Object-builder prototype follow-up

The installed Melange compiler accepts an opaque result for `mel.obj`, despite
the documentation's common wildcard-result examples. A compiled prototype used
`field0:(string [@mel.as "display-name"])` and an optional integer label;
the output contained the renamed property and omitted the absent optional
property. This supports a record-projection wrapper without exposing `Js.t` or
casting the record. The implementation uses local external declarations so
builder helpers do not become public source bindings.

Source: [Melange object builders](https://melange.re/unstable/working-with-js-objects-and-values.html).

## Native C via ctypes

ctypes builds typed C descriptions and binds functions without handwritten C
stubs. Descriptions cover numeric types, pointers, structs, unions, arrays,
and functions. This makes ctypes a suitable backend for a declaration-oriented
LG API rather than a reason to expose untyped foreign calls.

Source: [ocaml-ctypes](https://github.com/yallop/ocaml-ctypes).

`Foreign.foreign` accepts a typed function description and an optional loaded
library. Lookup failures are observable. Runtime-lock release changes which
arguments and callbacks are safe. Error handling cannot assume that any change
to errno means failure. Callback function pointers have lifetime and thread
requirements; a retained callback needs a retained root, not merely a correct
function type.

Source: [Foreign API](https://yallop.github.io/ocaml-ctypes/Foreign.html).

Pointers preserve the pointee type. Managed allocation and borrowed pointers
have different lifetimes, and a numeric address does not keep storage alive.
Struct layout must come from C declarations or verified ctypes descriptors;
an LG record's memory layout is not a C struct layout.

Source: [Ctypes API](https://yallop.github.io/ocaml-ctypes/Ctypes.html).

Dune can compile foreign stubs and integrate external build systems. Dynamic
ctypes binding is the smaller first backend; generated stubs are a future
build strategy for static linking, cross compilation, and restricted platforms,
not a second source syntax.

Sources: [Dune foreign libraries](https://dune.readthedocs.io/en/stable/foreign-code.html),
[foreign stubs](https://dune.readthedocs.io/en/stable/reference/foreign-stubs.html).

## Alternatives

| Approach | Assessment |
| --- | --- |
| Handwritten OCaml binding modules only | Already possible, but does not provide the requested LG authoring experience. Retain for advanced adapters. |
| Raw JS/C snippets in LG | Makes signatures and source diagnostics harder to enforce; not needed for the requested declarative API. |
| Copy every Melange attribute into metadata | Flexible but makes invalid combinations and target differences hard to discover. |
| One typed declaration with a closed target descriptor | Reuses normal LG calls, supports focused validation, and maps naturally to both backends. Selected. |
| Universal JS object or native address value | Violates the design contract and hides representation/lifetime mistakes. Rejected. |

## Result

Adopt the declaration and acceptance contract in
[ADR 011](011-foreign-function-interface.md). Verify compiler rejection cases,
generated declarations, and actual JS/C execution separately. Generated text
alone cannot prove receiver behavior, callback ABI, or native linking.

### Native callback implementation evidence

Installed ctypes-foreign 0.24.0 `foreign.mli` documents that `Foreign.funptr`
ties the C function-pointer lifetime to its OCaml closure. Its runtime-lock
option defaults to false; when entering OCaml from C, that option means acquiring
the lock. LG keeps the enclosing foreign call's lock and leaves the callback
option false, supporting synchronous callbacks on the calling thread. The
explicit `:callbacks :call` declaration records the no-retention contract.
The real C fixture verifies zero-argument and two-argument callbacks, captured
values, void side effects, and double results. Retained callbacks remain a
separate `dynamic_funptr` handle/lifetime implementation task.

### Borrowed pointer evidence

The installed ctypes interface provides `ptr` with a precise pointee type and
`ptr_opt` with the same pointee plus OCaml option. LG maps `pointer<T>` and
`option<pointer<T>>` to those descriptors. The C fixture reads and writes a
borrowed static counter, round-trips NULL/nil and a present pointer, and reads
through an `int **`. An int-pointer argument supplied to a float-pointer
parameter is rejected by static OCaml checking. These tests prove borrowed
interop, not managed allocation or release.

### Retained callback evidence

The runtime uses ctypes-foreign `dynamic_funptr`, whose interface explicitly
requires `free` after C stops using the callback. Its abstract handle and the
`static_funptr` descriptor both describe the same exact function signature;
`Ctypes.coerce` converts between these two representations of that ABI without
a universal OCaml value or numeric address. The runtime clears its live state
before freeing, so repeated release is harmless and the closure root is removed.
The real C registration fixture remains callable after major GC. A weak
reference proves captured data is retained while registered and collected after
unregister/release. Passing the released handle back to C is rejected. The
compiler's source fixture also verifies retain/register/invoke/unregister/release.

### Formatting prototype and initial execution

Local Clojure `core.clj` implements `printf` by applying `format` and passing
its result to `print`. The installed Clojure CLI produced pinned scalar
examples for nil, boolean, width, grouping, radix, and floating conversions.
The initial portable runtime uses a closed argument sum and parses argument
indices, flags, width, and precision. OCaml `%g` drops trailing zeros even
with its alternate flag, so the implementation selects fixed/scientific output
from the rounded exponent and requested significant digits. The compiler
elaborates arguments into the closed sum with ordered bindings. Further
cross-runtime and edge-case conformance checks remain required before acceptance.

### Decimal formatting conformance

Local Clojure probes exposed binary printf rounding differences for 1.005,
2.675, and 1.25, plus trailing-zero and alternate-point differences. Formatting
now obtains a decimal representation that round-trips to the same float and
performs decimal half-up rounding over named digits/point state. Fixed,
scientific, and general output share that representation. Tests cover carry
across the radix point and scientific threshold, subnormal values, large
exponents, negative zero, alternate decimal points, and grouping. `%s` uses
Clojure-style shortest float display rather than OCaml's default reduced
precision. Additional cross-runtime source assertions pin the observed results.
