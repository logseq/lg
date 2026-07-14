# Differences From Clojure

cljml is a Clojure-syntax language that targets OCaml.

The goal is API familiarity, not JVM Clojure runtime identity.

## Static typing

The elaboration layer keeps three type relationships separate. `equal` is
strict and nominal, `row_compatible` permits explicit structural-record
projection, and `defer_to_ocaml` marks host-owned relationships that only the
OCaml typechecker should decide. `TUnknown` represents unresolved source
metadata and is only accepted at explicitly selected boundaries; it is not
equal to every type. Explicit `type-record` declarations carry stable
`Type_id` identities, while compiler-generated records for structural maps are
marked structural and may participate in row projection.

Source identities use dedicated `Symbol_id`, `Type_id`, `Method_id`,
`Protocol_id`, `Module_id`, `Signature_id`, and `Functor_id` types. Typed
registries own type, protocol, implementation, module, signature, functor, and
alias metadata instead of encoding it in symbol-table strings. Emitted OCaml
value, type, and module names are checked per scope, so source names that munge
to the same OCaml identifier are rejected before lowering.

cljml type checks programs before emitting OCaml.

Vectors are homogeneous.

Maps are structural records when created from map literals.

`hash-map` creates structural records from keyword/value pairs.

`assoc` can add one or more fields to a structural map, but it cannot change the type of an existing field.

Updating an existing field of a declared record preserves its nominal
`Type_id`, including protocol receiver identity. Adding a field produces a new
structural map because ordinary OCaml records have a closed field set.

`assoc` can update one or more persistent vector indexes when the replacement values match the element type.

`dissoc` can remove one or more known fields from a structural map.

`get` on a structural map requires a literal keyword that exists in the map type.

Three-argument `get` can return a default for an absent literal key; when the key is present, the default must match the field type.

`update` can pass extra arguments after the update function, but the function must return the existing field or vector element type.

Keyword call syntax such as `(:name user)` is supported for structural maps.

`if` branches must have the same type for cljml-owned core types. When branch
results are OCaml-owned types such as aliases, option/result applications, or
tuples, cljml lowers the branch expressions and lets the OCaml typechecker
decide compatibility.

Function parameter types are inferred from body constraints where possible; annotations such as `^:int`, `^:string`, `^:keyword`, and `^:unit` are optional explicit hints for cljml core types. `:unit` is also valid as an explicit `ocaml-call` return type for side-effecting host calls. Opaque host-owned annotations such as `^:ocaml/int` lower to OCaml parameter constraints and are delegated to the OCaml typechecker instead of being interpreted as full cljml types. Host-owned type applications use angle brackets, for example `^:ocaml/option<int>`, `^:ocaml/result<string;string>`, and `^:ocaml/tuple<int;string>`; `;` separates multiple type arguments because commas are reader whitespace.

The built-in host applications also have concise spellings:
`^:option<int>`, `^:result<string;string>`, and `^:tuple<int;string>`.
External type paths use forms such as `^:Datascript/entity`, which lowers to
`Datascript.entity`. The `^:ocaml/...` forms remain compatibility escape
hatches.

Type aliases such as `(type-alias user-id :ocaml/int)` emit OCaml aliases in
both source and Parsetree backends. The alias is OCaml-owned metadata; cljml can
lower references such as `^:ocaml/user_id` but does not implement alias
expansion as part of its own type system.

Aliases, records, and variants can declare OCaml type parameters with a vector
after the type name. Parameter references use `:param/name`, including inside
host type applications:

```clojure
(type-alias maybe [a] :ocaml/option<param/a>)
(type-record pair [a b] (left :param/a) (right :param/b))
(type-variant box [a] (Box :param/a))
```

These lower to OCaml type parameters and variables. cljml validates parameter
scope and declaration shape; OCaml enforces relationships such as multiple
fields or constructor payloads sharing the same parameter.

OCaml records such as `(type-record user (name :string) (age :int))` emit
ordinary OCaml record declarations in both source and Parsetree backends.
Values can be constructed with `(record user (name "Ada") (age 41))`, and
fields can be accessed with `(:name user-value)`. `ocaml-record` and
`ocaml-field` remain available for compatibility. cljml checks record
shape and field names; field value compatibility remains owned by OCaml.
Unannotated function parameters infer record rows from field reads and `assoc`
updates. When exactly one declared named record matches the inferred row,
cljml preserves that nominal record identity through the function result.
Records declared inside modules can be constructed from outside with qualified
type names such as `(record User.user ...)`, including through module aliases
such as `(record U.user ...)`. Opened modules expose record type names in the
current scope, so `(open User)` allows `(record user ...)`.

OCaml variants such as `(type-variant status Active Inactive)` emit ordinary
OCaml variant declarations in both source and Parsetree backends. Payload
constructors can be declared with forms such as `(Named :string)` or
`(Pair :int :string)`, and can be constructed directly with `Active` or
`(Named "Ada")`; `ocaml-construct` remains a compatibility spelling. cljml
validates the surface shape, duplicate
constructors, and constructor arity for known constructors, but payload type
compatibility and exhaustiveness remain owned by OCaml. For opaque host-owned
targets, module-qualified constructor patterns such as `(Msg.Named name)` lower
directly to OCaml; constructor existence, arity, and payload typing are checked
by OCaml.

OCaml option/result constructors use ordinary constructor syntax:
`(Some value)`, `None`, `(Ok value)`, and `(Error value)`. The corresponding
`ocaml-*` spellings remain compatible. cljml checks only surface arity and
lowers to the OCaml constructors. Match forms can destructure them with OCaml
constructor patterns
such as `(Some x)`, `None`, `(Ok value)`, and `(Error err)`. Payload and
polymorphic option/result typing stay owned by OCaml.

OCaml tuples use `(tuple a b ...)` and can be destructured with patterns such
as `(tuple id name)`. cljml checks tuple arity and lowers tuple annotations such
as `^:ocaml/tuple<int;string>` to OCaml tuple constraints; element compatibility
remains owned by OCaml. `ocaml-tuple` remains available for compatibility.

The core arithmetic
operators `+`, `-`, `*`, and `/` select OCaml integer or floating-point
operators from their statically known operand types; one call cannot mix
integer and float operands. An empty `(list)` receives its element type from
`if`, `if-not`, or `match` branch context. Without such a context, use
`list-of` with an explicit type.

Keyword lookup in typed contexts can infer structural map field requirements for unannotated function parameters.

Function calls can pass wider structural maps when the callee only requires a known subset of fields.

`let`, `fn`, and `defn` support a static destructuring subset inspired by
Clojure destructuring. Associative destructuring works on structural maps with
`:keys`, direct `{local :keyword}` bindings, scalar literal `:or` defaults, and
`:as`. Sequential destructuring works on typed vectors and lists with fixed
positional bindings, `& rest`, and `:as`. Keyword argument destructuring,
non-literal default expressions, and nil-padding are not supported yet. cljml
does not have a surface `nil` value or nilable collection element type.

Row polymorphism is represented in cljml's static type compatibility: a
function parameter inferred as a structural map with fields `:name` and `:age`
can be called with a wider map that also has other fields. For generated OCaml,
cljml emits a narrow record type for row-shaped function parameters and
projects wider map records into that narrow row at the call site. This keeps
the runtime representation as OCaml records while allowing one function to
accept different structural map shapes that share the required fields.

Protocols are a static subset of Clojure protocols. `defprotocol` records the
types of every annotated parameter and the return value, and `extend-type`
emits ordinary OCaml functions for primitive and named-record receivers. Calls
dispatch at compile time from the first argument type, so there is no runtime
protocol table, dynamic extension, metadata dispatch, or reflection. Protocol
identity includes its owning module and protocol name; when two protocols expose
the same method name, `Protocol/method` selects one explicitly while the
traditional unqualified method spelling remains available when unambiguous.
`defprotocol` and `extend-type` are also valid inside `module`; exported calls
use `Module/Protocol/method` and retain qualified record receiver identity.

`do`, `fn`, `defn`, and `let` bodies evaluate forms in order and return the final form's type.
An explicitly typed recursive definition uses
`(defn name [^:type argument ...] :return-type body...)` and lowers to native
OCaml `let rec`. Recursive parameters and the return value are checked against
the declared source signature.
Unconstrained identity-style functions such as `(defn id [x] x)` and
`(let [id (fn [x] x)] ...)` preserve OCaml-owned call-site polymorphism. cljml
tracks only the fact that the return value is the same parameter so its core API
can continue elaborating the result; OCaml still owns the actual polymorphic
typechecking.

`loop` lowers to a local OCaml tail-recursive function. `recur` is valid only
in the tail position of the nearest loop and must preserve the number and
static types of the loop bindings. For OCaml-owned loop binding types, recur
argument compatibility is delegated to the OCaml typechecker.

`if-not`, `when`, and `cond` are compiler-recognized forms rather than macros.
`when` currently supports unit bodies, and `cond` requires an `:else` branch
because cljml does not add implicit nil results. OCaml-owned branch result
compatibility is delegated to the OCaml typechecker.

`if-let` and `when-let` bind the payload of an OCaml option. `let-some` accepts
multiple sequential name/option pairs and evaluates one fallback when any
binding is `None`. `->` and `->>` provide first- and last-argument threading
without requiring a general macro system.

`match` is a compiler-recognized static pattern form rather than a macro. The
current subset supports scalar literal patterns, `_`, symbol binders, and
fixed-length list/vector patterns written with vector pattern syntax. Record
patterns use `(record (field pattern) ...)`, aliases use `(as pattern name)`,
or-patterns use `(or left right)`, and guarded clauses use
`(when pattern guard)` in the pattern position. Alternatives of an or-pattern
must bind the same names. Pattern literals can constrain unannotated function
parameters. cljml does not implement a parallel exhaustiveness checker; record
labels, constructor typing, guard compatibility, exhaustiveness, and redundant
cases are delegated to OCaml. OCaml-owned match branch result compatibility is
also delegated to OCaml.

`try` is a compiler-recognized OCaml exception form rather than a macro. It
uses one or more body forms followed by `(catch pattern body...)` handlers, and
`raise` lowers to OCaml's ordinary `raise` call. Catch patterns use the same
pattern compiler as `match`, so exception constructor existence, payload types,
and coverage stay delegated to OCaml.

Top-level expression forms are supported and emit `let _ = ...`; top-level map literals still need a `def` because structural maps require generated record definitions.

`print` and `println` follow Clojure's newline behavior.

`not` follows the values represented by cljml: `false` is falsey, while
integers, strings, collections, keywords, symbols, and records are truthy. The
argument is still evaluated before the boolean result is produced. cljml does
not support a surface `nil` value, `nil?`, or `some?`.

Type predicates such as `int?`, `integer?`, `number?`, `nat-int?`, `pos-int?`,
`neg-int?`, `string?`, `keyword?`, `boolean?`, `vector?`, `list?`, `seq?`,
`set?`, `map?`, `fn?`, `coll?`, `associative?`, `indexed?`, `seqable?`, and
`counted?` are resolved from static cljml types or direct OCaml integer checks.
`any?`, `rational?`, `ratio?`, `float?`, `double?`, `decimal?`,
`simple-keyword?`, `qualified-keyword?`, `symbol?`, `simple-symbol?`,
`qualified-symbol?`, `ident?`, `simple-ident?`, `qualified-ident?`,
`sequential?`, `reversible?`, and `sorted?` are also static predicates in the
current subset. `number?` recognizes both cljml integers and OCaml floats;
`float?` and `double?` recognize the OCaml float representation. Ratios and
decimals are not represented, so their predicates return false.

`subs` supports two- and three-argument typed string slicing.

The arithmetic operators `+`, `-`, `*`, and `/`, ordered comparisons,
`zero?`, `pos?`, `neg?`, `max`, and `min` support statically uniform integer
or float operands. One call cannot mix the two representations. Integer `+`
and `*` retain their Clojure identity arities; `/` requires at least two
operands and does not introduce ratios. Same-typed `=` and `not=` are
supported, and same-shaped structural maps compare field by field. Integer-only
helpers include `even?`, `odd?`, `quot`, `rem`, `mod`, `bit-and`, `bit-or`,
`bit-xor`, `bit-not`, `bit-set`, `bit-clear`, `bit-flip`, `bit-test`,
`bit-shift-left`, `bit-shift-right`, and `bit-shift-right-zero-fill`.
Unchecked integer functions map directly to OCaml integer operators, so their
exact overflow behavior follows the OCaml target. Sets support floats and
float lists or vectors through generated OCaml comparators.

`boolean`, `name`, `namespace`, `keyword`, and `symbol` are supported for the
current scalar subset. `name` works on strings, keywords, and symbols.
`namespace` works on keywords and symbols, but returns `""` for unqualified
identifiers because cljml does not yet have a typed optional string or nilable
string. `keyword` and `symbol` work on strings, keywords, and symbols.

## Macros

Macros are not supported.

Reader macro support is intentionally minimal while the typed core is being built.

## Modules and imports

cljml has no namespace declaration or namespace import mechanism. `module`
creates the same ownership boundary as an OCaml module; `module-alias`, `open`,
and `include` provide module reuse. Top-level `require` is restricted to OCaml
packages, OCaml modules, and the typed `clojure.string` compatibility module.
Qualified source references consistently use `/`, including module values,
functions, constructors, and constructor patterns. Value and function member
names use cljml kebab-case and lower to OCaml snake_case; constructors preserve
their OCaml capitalization.
Package and module imports can be combined as
`[ocaml.core/Core.Int :as int]`; this adds findlib package `core` and aliases
module `Core.Int` in one entry. Separate `ocaml.package/...` entries remain
compatible.
Incremental compilation preserves modules, aliases, types, protocols, the type
counter, and value bindings across source chunks.

The LSP workspace index builds module and top-level symbol dependency
components. A changed document reanalyzes its dependency/reverse-dependency
component only, reuses unrelated Typedtree analyses, and contains parse or type
errors without discarding unaffected components.

The CLI exposes the same state across files with
`--compile-files ... -o output.ml` and `--run-files ...`. Input order defines
compilation order. Each file keeps its own diagnostic filename and line map,
while module/type state and the union of findlib package dependencies
flow forward. A Dune rule can list `.cljml` files as dependencies, generate one
`.ml` target with `--compile-files`, and compile it through an ordinary library
or executable stanza; see `examples/multi_file/dune`.

Protocol signatures and implementations are preserved in the same incremental
compiler state. Module-owned methods use `Module/Protocol/method`, and module
aliases preserve that protocol identity.

`module` emits an OCaml module and registers bindings for qualified calls such
as `Math/add2`. The current static subset allows `module-signature`,
`type-alias`, `type-variant`, `open`, `include`, `module-alias`, `def`, `defn`,
and nested `module` forms inside a module. `open` emits an OCaml open item and
re-exports already-known module value/function bindings as unqualified cljml
symbols, including OCaml record type metadata. `include` emits an OCaml include
item, exposes already-known module bindings as unqualified cljml symbols, and
re-exports direct included bindings and OCaml record type metadata when used
inside another module. `module-alias` emits an OCaml module alias and
re-exports already-known target module value/function bindings and OCaml record
type metadata under the alias.
`module-signature` emits an OCaml module type from `val` declarations, abstract
type declarations such as `(type user-id)`, and manifest type declarations such
as `(type user-id :ocaml/int)`. Signature types accept parameter vectors:
`(type box [a])` is abstract and `(type box [a] :ocaml/option<param/a>)` is
manifest. cljml validates parameter scope, while OCaml checks signature
matching. Nested module items use `(module Inner InnerSig)`. Their known value
metadata is exposed through functor parameter paths such as `M.Inner/value`,
and OCaml checks that implementations include the declared nested module.
`(include BaseSig)` emits an OCaml signature include and exposes the included
signature's known values through concrete modules and functor parameters.
`(module Name Signature ...)` emits an ascribed module whose signature match is
checked by OCaml.
`module-functor` emits an OCaml functor. Its parameter vector contains one or
more name/signature pairs; multiple pairs lower to curried OCaml functor
parameters. `module-apply` accepts the corresponding module arguments in order
and exposes the applied module's already-known result bindings and OCaml record
type metadata. Functor parameter and application signature matching are checked
by OCaml.

Clojure symbols and keywords that would emit OCaml reserved words are munged as
legal OCaml identifiers while preserving source-level names.

## Collections

Vectors compile to `Rrbvec.t` persistent vectors.

Lists compile to OCaml lists and support `list`, `list*`, `list-of`, `cons`, `conj`, `first`, `second`, `last`, `peek`, `pop`, `rest`, `next`, `nthnext`, `nthrest`, `ffirst`, `fnext`, `nfirst`, `nnext`, `rseq`, `nth`, `count`, `map`, `filter`, `remove`, `some`, `take-while`, `drop-while`, `distinct`, `dedupe`, `sort`, `sort-by`, `concat`, `mapcat`, `vec`, `set`, `repeat`, `repeatedly`, `interpose`, `interleave`, `partition`, `partition-all`, `reductions`, `map-indexed`, `filterv`, `mapv`, `reduce`, `reduce-kv`, `butlast`, `take-last`, `drop-last`, `take-nth`, `split-at`, `split-with`, `partition-by`, `bounded-count`, `dorun`, `doall`, and `run!` where the static element types line up.

Vectors support `first`, `second`, `last`, `peek`, `pop`, `rest`, `next`, `nthnext`, `nthrest`, `ffirst`, `fnext`, `nfirst`, `nnext`, `rseq`, `nth`, `get`, `assoc`, `update`, `contains?`, `subvec`, and the current eager sequence operations.

`map`, `filter`, `take`, and `drop` return typed memoized lazy seqs. Already
realized nodes are cached, so repeated traversal does not rerun producer side
effects. Lists, vectors, sets, arrays, strings, and host `Seq.t`, list, and array
values are built-in seqable inputs. `rest` and `next` still preserve their
legacy concrete collection representation pending migration to the same seq
abstraction. `nil` remains unsupported.

`first`, `second`, and `last` accept typed lists, vectors, and sets. Set
iteration follows the canonical order from the underlying OCaml `Set.Make`
instance.

Three-argument `nth` returns a typed default for out-of-range list and vector indexes.

`subvec` returns an `Rrbvec.t` persistent vector slice.

`empty` returns a same-typed empty list, vector, set, or string.

`into` transfers elements between typed list, vector, and set collections when
the element types match.

`take` and `drop` return lazy seqs and accept every built-in seqable type.

`reduce` is eager and accepts every built-in seqable type: lists, vectors,
sets, arrays, strings, typed lazy seqs, and host OCaml `Seq.t`, list, and array
values. Reducing a memoized lazy seq realizes each source node at most once.

`Seqable` is reserved as a compiler-owned protocol. Custom named records and
host wrapper types may use `(extend-type T Seqable (-seq [value] ...))`; `-seq`
must return a typed lazy seq. Core `map` and `reduce` resolve this capability
statically, including implementations exported from modules.

`reverse` returns a same-typed reversed list or vector.

`every?`, `not-any?`, `not-every?`, and the current static subset of `some`
return typed booleans for list, vector, and set predicates.

The sequence migration is incremental. `map`, `filter`, `take`, `drop`,
`range`, and `repeat` are lazy. `mapv` and `filterv` remain explicit eager
persistent-vector materializers. `concat`, `sort`,
`interpose`, `interleave`, `partition`, `partition-all`, `reductions`, and
`map-indexed` return OCaml lists, `split-at` and `split-with` return persistent
vectors of the input collection representation, and same-shape operations such
as `remove`, `take-while`, `drop-while`, `distinct`, `dedupe`, `butlast`,
`take-last`, `drop-last`, `take-nth`, `nthnext`, `nthrest`, and `rseq` currently
preserve the input collection representation where practical. `dorun` and
`doall` are explicit realization boundaries.

`range` returns a typed memoized lazy integer seq. `(range)` is unbounded;
bounded one-, two-, and three-argument forms remain lazy.

`interleave` accepts two or more typed list, vector, or set inputs with the same
element type. It eagerly returns an OCaml list and stops at the shortest input.

Empty vector literals still require explicit element typing.

Use `(vector-of :int)`, `(vector-of :string)`, `(vector-of :symbol)`, `(vector-of :keyword)`, or `(vector-of :bool)` for typed empty vectors.

Use `(list-of :int)`, `(list-of :string)`, `(list-of :symbol)`, `(list-of :keyword)`, or `(list-of :bool)` for typed empty lists.

Use `(set-of :int)`, `(set-of :string)`, `(set-of :symbol)`, `(set-of :keyword)`, or `(set-of :bool)` for typed empty sets.

Sets compile to persistent OCaml `Set.Make` instances and support `hash-set`,
`sorted-set`, `set-of`, `conj`, `disj`, `contains?`, `every?`, `not-any?`, `not-every?`,
`map`, `filter`, and `reduce`. Built-in comparators cover scalar values,
scalar lists and vectors, nested integer vectors, and named structural map
records. `nil` is not a supported surface value or set element.

`conj` accepts one or more same-typed values after a list, vector, or set.

`disj` accepts zero or more same-typed values after the set.

Function helpers include `apply`, `comp`, `partial`, `identity`, `constantly`,
`complement`, `every-pred`, `some-fn`, and `juxt` for the current unary or
integer-reducer subset. `apply` currently supports integer binary reducers over
typed lists, vectors, and sets, with optional fixed leading integer arguments.
`some-fn` returns a typed boolean in this subset rather than an arbitrary truthy
value.

Comparison helpers include `distinct?`, `compare`, `max-key`, and `min-key` for
same-typed comparable scalar values.

Keywords and symbols are statically distinct from strings, although the current
runtime representation is still an OCaml string.

Maps currently compile to OCaml records when their keys are known statically.
`array-map` and `sorted-map` currently share the same structural map
representation as `hash-map`.

Structural map helpers include `merge`, `update`, `select-keys`, `keys`, and `vals`. Overlapping fields in `merge` and updated fields in `update` must keep their existing static type, and `vals` requires all selected map values to have the same type.

Lazy sequence APIs cache realized nodes; explicit collection constructors and
`mapv`/`filterv` materialize results.

## Host interop

Direct OCaml package interop has two paths.

External findlib packages are declared independently from their modules. For
example, `(require [ocaml.package/core] [ocaml.Core.Int :as int])` adds the
`core` package's recursive compiler include directories and aliases the
`Core.Int` module. `(int/abs -42)` can then infer its signature from
the package CMI. CLI `--run` links every declared package through
`ocamlfind ocamlopt -linkpkg`. Missing and invalid package names fail before
cljml elaboration. This mirrors the OCaml/Reason boundary where project
dependencies establish the compiler environment and source imports name
modules within that environment.

Constructor signatures are discovered from the same package compiler
environment. A file requiring `[ocaml.package/unix]` and aliasing
`[ocaml.Unix :as unix]` can construct
`(unix/ADDR_UNIX "/tmp/app.sock")` directly. cljml uses compiler-libs metadata
for constructor arity and result-type elaboration, preserves constructor
capitalization, and delegates payload compatibility to the OCaml typechecker.

Currently supported typed examples include `ocaml.Stdlib` aliases or refers for
`string-of-int` and `int-of-string`, and `ocaml.String` aliases or refers for
`uppercase-ascii` and `length`.

Ordinary OCaml module functions are called directly. Calls such as
`(Stdlib.abs -42)` read the value signature from the OCaml compiler environment.
`ocaml-call` remains a compatibility escape hatch; explicit-return calls such as
`(ocaml-call :int Stdlib.abs -42)` remain available for opaque host boundaries.
Direct calls also resolve
required OCaml aliases and refers, for example
`(std/abs -42)` after `[ocaml.Stdlib :as std]`, or
`(uppercase_ascii "ada")` after
`[ocaml.String :refer [uppercase_ascii]]`. OCaml value refers remain available
inside module and functor bodies. cljml uses
the inferred or declared return type to continue source elaboration. Function
existence, argument arity, and argument compatibility are checked by the OCaml
typechecker.

OCaml floats and characters use native literals such as `1.5` and `\a`.
OCaml arrays use `(ocaml-array 1 2 3)`, `(ocaml-array-of :int)`,
`ocaml-array-get`, and `ocaml-array-set!`. OCaml references use `ocaml-ref`,
`ocaml-deref`, and `ocaml-reset!`. These forms preserve their element types and
mutation semantics in the generated Parsetree. Core cljml arithmetic remains
statically typed: uniform integer operands select integer operators, while
uniform float operands select OCaml float operators. Direct OCaml calls such as
`(Float.add 1.5 2.25)` remain available for host interop.

Labelled and optional OCaml arguments are written as keyword/value pairs, such
as `(String.starts_with "ada" :prefix "ad")`. cljml validates label
names and duplicates from the compiler signature, emits labelled Parsetree
application arguments, and lets the OCaml typechecker validate argument values.
Optional labels can be omitted. Supplying a label while leaving positional
arguments unapplied preserves an ordinary partially applied function when the
remaining parameters are positional.
`Cljml.Compiler.typecheck_parsetree` exposes this OCaml compiler-libs check as
an explicit Parsetree validation gate. The gate adds local Dune build CMI
directories for cljml and `Rrbvec` when available, and accepts extra include
directories through `CLJML_OCAML_INCLUDE_PATH`.
`compile_string_with_diagnostics` and its filename-aware variant return enabled
OCaml warnings alongside generated source. CLI compilation prints them to
stderr, and the LSP publishes them with warning severity. Exhaustiveness and
redundancy therefore remain OCaml-owned checks without disappearing at the
cljml tooling boundary.

The stable backend still emits OCaml source from cljml's typed IR. The
Parsetree backend no longer reparses the whole generated program: it lowers
compiled items independently, constructs structural record definitions
directly as `Pstr_type` and `Pstr_value`, and directly constructs ordinary
top-level value/effect bindings. Require and protocol declaration comments do
not produce AST nodes. Row type definitions, OCaml-owned type aliases, nullary
variant declarations, `defn`, and protocol implementation bindings are also
structured items. `open` lowers directly to `Pstr_open`. Nested modules are
represented recursively and lower directly to `Pstr_module` and
`Pmod_structure`; module aliases lower directly to `Pstr_module` and
`Pmod_ident`; module signatures lower directly to `Pstr_modtype`, and ascribed
modules lower with `Pmod_constraint`; module functors and applications lower
directly to `Pmod_functor` and `Pmod_apply`. There is no remaining whole-program
or structure-item OCaml parser path.
The generated structure can be passed to `Cljml.Compiler.typecheck_parsetree`
to run the OCaml compiler-libs typechecker as the final host-language check,
including generated code that depends on the cljml runtime and `Rrbvec` CMIs.
`Cljml.Compiler.compile_chunk_parsetree` follows the same incremental state
model as `compile_chunk`, returns the current chunk's structure, and typechecks
the accumulated Parsetree state before returning. The public
`Cljml.Compiler.compile_parsetree` API also runs this OCaml typecheck gate by
default. Public source compilation APIs and CLI compile/run paths print source
from the checked Parsetree output before returning or executing generated OCaml
source. Typed expression payloads lower from structured `Ocaml_ir` nodes; there
is no expression-level source fallback or
`Parse.expression` path. Scalar literals, identifiers, list/vector literals,
`if`, `if-not`, `when`, ordinary function roots, typed parameter constraints,
OCaml option/result/variant constructors, multi-form bodies, simple local `let`
bindings, destructuring bindings, and static `match` clauses all lower
directly. The test suite also guards this boundary with a static regression
check, so the legacy unstructured expression path cannot be reintroduced
silently. Parsetree is a backend
representation here, not cljml's full type system.

Lexer tokens and the complete parsed form tree retain byte spans. Every
elaborated expression carries its exact form location into generated Parsetree;
synthetic nodes without a direct form use their owning top-level span. Locations
survive incremental compilation. OCaml compiler-libs errors therefore report
precise nested lines and columns through ordinary library APIs;
`compile_string_with_filename`, the CLI, and the LSP retain actual input paths.

The scalar and integer cores lower `boolean`, `name`, `namespace`, `keyword`,
`symbol`, arithmetic, division/remainder, `min`/`max`, bitwise operators,
shifts, `inc`/`dec`, and integer predicates directly through the shared
expression IR.

Equality and ordered integer comparisons also lower directly. Recursive record
equality remains an explicit migration boundary.

The structured expression subset now also covers string conversion and
printing, string slicing, function composition/partial application, direct
record fields and record values, typed empty collections, collection
prepend/update operations, indexing, `rest`, recursive sequence helpers, eager
sequence transforms, and complex destructuring.

`hash-set`, `sorted-set`, `set-of`, `conj`, `disj`, `contains?`, set equality,
set sequence conversion, and set printing now use persistent OCaml `Set.Make`
instances. Primitive static element types use built-in runtime comparators.
Lists and persistent vectors of the supported scalar element types also use
dedicated persistent `Set.Make` instances.

Each named structural map record emits a sibling `Set.Make` comparator module,
and same-shaped records are explicitly projected at set mutation and membership
boundaries. This preserves static record types while retaining Clojure-style
structural map compatibility.

## ReasonML architecture alignment

ReasonML is the architectural reference, not a syntax or standard-library
compatibility target. The current compiler boundary is aligned as follows:

| Requirement | Current evidence |
| --- | --- |
| Alternate syntax frontend | The cljml reader produces a located Lisp AST without parsing generated OCaml source. |
| Semantic elaboration boundary | cljml owns Clojure surface rules, core API compatibility, collection representations, and source-oriented errors. Every elaborated semantic expression retains its `Semantic_type.ty`; `Semantic_lowering` is the explicit boundary that erases those annotations into backend `Ocaml_ir`. |
| Native OCaml backend | Every supported expression and structure item lowers through structured `Ocaml_ir` and OCaml `Parsetree`; regression guards reject unstructured source-backed fallback nodes and legacy item emitters. |
| OCaml type system as final truth | Public source, Parsetree, incremental, CLI, and LSP paths run the compiler-libs typechecker. Host calls, polymorphic relationships, module inclusion, constructor payloads, pattern exhaustiveness, and warnings are checked by OCaml. |
| Function polymorphism | Top-level and let-bound functions can be instantiated at different call-site types. Non-trivial relationships such as both branches of a polymorphic chooser are accepted or rejected by OCaml. |
| Host type surface | Aliases, parameterized types, records, variants, option/result, tuples, arrays, references, constructors, patterns, labelled arguments, and package values lower to native OCaml nodes. |
| Module system | Modules, aliases, open/include, parameterized signatures, nested signature modules, signature includes, multi-parameter functors, applications, and typed recursive functions lower to native OCaml AST. Typed registries reject source and emitted-name collisions before lowering. |
| Diagnostics and tooling boundary | Source node identity and locations survive into Typedtree. Compiler errors and warnings reach the library API and CLI. The dependency-aware LSP workspace index reuses unaffected component analyses for diagnostics, hover types, cross-file definitions, type-detailed completion, identity-aware references, rename, highlights, and symbols, and provides deterministic comment-preserving formatting. |

This does not make cljml a Reason syntax clone. `.re`/`.rei` parsing, `refmt`,
JSX, and full `ocaml-lsp` feature parity are not cljml language requirements.
Likewise, the documented absence of macros, nil, laziness, the JVM numeric
tower, and dynamic Clojure runtime features is an intentional static Clojure
dialect boundary rather than an OCaml backend gap.

The typed standard library also includes a `clojure.string` namespace that can
be required with `:as` or `:refer`. Its current subset includes `blank?`,
`capitalize`, `ends-with?`, `includes?`, `index-of`, `join`, `last-index-of`,
`lower-case`, `re-quote-replacement`, `replace`, `replace-first`, `reverse`,
`split`, `split-lines`, `starts-with?`, `trim`, `trim-newline`, `triml`,
`trimr`, and `upper-case`. `replace` and `split` currently use literal string
matches, not regex patterns.

Host package aliases are explicit in top-level `require`; they do not introduce
a cljml namespace layer.
