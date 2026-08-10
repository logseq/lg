# Clojure Core Static Compiler Implementation Plan

Goal: Build lg into a statically typed Clojure-syntax language that compiles to OCaml and exposes a core API that stays close to Clojure where static typing permits.

Architecture: lg should follow the same broad shape as ReasonML: parse a source syntax into an AST, elaborate that AST into OCaml Parsetree, then let the OCaml compiler own the full host-language type system.
The frontend keeps Clojure surface syntax, while the backend emits OCaml data structures and functions so generated code can use the OCaml compiler, OCaml typechecker, and OCaml packages.
The implementation should mirror ReasonML's toolchain boundary: a frontend parses source into an AST, an elaboration phase enforces lg-specific semantics and builds typed OCaml items, and a backend emits OCaml source or Parsetree.
lg should also support incremental compilation: callers must be able to parse, typecheck, and emit one source chunk while preserving modules, aliases, generated type counters, and previously compiled bindings for later chunks.

Tech Stack: OCaml 5.4.1, Dune, generated OCaml backend, `RCmerci/rrbvec` for persistent vectors, integration tests that compile and run emitted OCaml.

Related: Builds on the initial lg prototype in `/Users/tiensonqin/Documents/lg`.

## Problem statement

The current compiler is a prototype with hard-coded top-level `def`, `assoc`, `dissoc`, and `print` cases.

The requested language is broader.
It should accept familiar Clojure syntax, reject type errors before OCaml compilation, provide built-in data structures, and expose core APIs with names and behavior close to Clojure.

Macros are explicitly out of scope.
Reader macros, syntax quote, metadata, vars as runtime objects, multimethods, laziness, dynamic vars, and JVM interop are out of scope for the first complete static language.
Explicit OCaml modules and qualified module symbol resolution are in scope;
Clojure namespace declarations are not part of the language.

The reference model is ReasonML in architecture rather than syntax.
ReasonML preserves an alternate syntax over the OCaml toolchain.
lg should preserve Clojure-like syntax over an OCaml-typed backend.
The long-term owner of complete function, module, pattern, and host-package typing should be OCaml's typechecker, not a parallel lg reimplementation of OCaml's type system.
lg's type/lowering layer should remain as a semantic elaboration layer for Clojure-like surface rules, predictable collection representations, better source-level diagnostics, and compatibility checks that OCaml cannot infer from the source syntax alone.
For unconstrained identity-style functions, lg should preserve only enough
metadata to keep the return-parameter relationship visible to source-level core
forms; OCaml should still own the actual call-site polymorphism.

ClojureDart is the reference model for Clojure dialect ergonomics over a non-JVM host.
Its docs emphasize explicit host differences, symbol munging, and host package interop as first-class compiler concerns.
lg should follow that posture for OCaml packages and should keep a documented differences surface instead of silently diverging from Clojure.

API compatibility, runtime representation, and performance must be balanced explicitly.
The source-level API should stay close to Clojure, but the elaborated OCaml representation should prefer predictable OCaml data structures over runtime dynamism.
When exact Clojure behavior would require slow dynamic dispatch or weak static types, lg should document the difference, choose a typed representation, and add tests that lock in that behavior.

## Testing Plan

I will add integration tests that compile lg source strings to OCaml, compile the emitted OCaml with `ocamlc`, run the result, and assert stdout.

I will test Clojure-style forms using vector literals, map literals, ordinary prefix calls, and nested expressions.

I will test static type errors for heterogeneous vectors, invalid arithmetic arguments, `get` on unknown map fields, `assoc` changing an existing field type, and `if` branch type mismatch.

I will test command-line behavior through `dune exec lg -- --run examples/person.cljc`.

I will add extensive tests across these layers:

- Reader/parser behavior for Clojure syntax accepted by lg.
- Typechecker success and failure cases for every supported core API.
- Generated OCaml compilation and execution for user-visible behavior.
- Module, alias, open, and include resolution across source chunks.
- Incremental compilation state across source chunks.
- Runtime representation checks for persistent vectors and structural maps where behavior depends on representation.
- CLI smoke tests for file compilation and `--run`.

NOTE: I will write *all* tests before I add any implementation behavior.

## Scope

The compiler should support this syntax without macros.

| Syntax | Example | Phase |
| --- | --- | --- |
| Scalar literals | `1`, `"Ada"`, `true`, `false` | 1 |
| Symbols | `x`, `user-name` | 1 |
| Keywords | `:name`, `:admin?` | 1 |
| Lists as calls | `(+ 1 2)` | 1 |
| Lists as data | `(list 1 2 3)` | 2 |
| Vectors | `[1 2 3]` | 1 |
| Maps | `{:name "Ada" :age 36}` | 1 |
| `def` | `(def x 1)` | 1 |
| `if` | `(if admin? "yes" "no")` | 1 |
| Conditional forms | `(if-not ready? "wait" "go")`, `(when ready? (println "go"))`, `(cond ready? "go" :else "wait")` | 3 |
| Static match | `(match x 0 "zero" _ "other")` | 3 |
| `let` | `(let [x 1] (+ x 2))` | 2 |
| `fn` | `(fn [x] (+ x 1))` | 2 |
| `defn` | `(defn inc1 [x] (+ x 1))` | 2 |
| `loop` / `recur` | `(loop [n 3 acc 0] (if (= n 0) acc (recur (dec n) (+ acc n))))` | 2 |
| Static destructuring | `(let [{:keys [name]} user] name)` | 3 |
| Static protocols | `(defprotocol Labelled (label [x] :string))` | 3 |
| Module form | `(module App (def value 1))` | 1 |
| Module form | `(module Math (defn add2 [x] (+ x 2)))` | 3 |

## Core API Roadmap

| Category | Functions | Phase |
| --- | --- | --- |
| Printing | `print`, `println`, `pr-str` | 1 |
| Boolean and predicates | `not`, `true?`, `false?`, `boolean`, `int?`, `integer?`, `number?`, `nat-int?`, `pos-int?`, `neg-int?`, `string?`, `keyword?`, `symbol?`, `boolean?`, `vector?`, `list?`, `seq?`, `set?`, `map?`, `fn?`, `coll?`, `associative?`, `indexed?`, `seqable?`, `counted?`, `any?`, `rational?`, `ratio?`, `float?`, `double?`, `decimal?`, `simple-keyword?`, `qualified-keyword?`, `simple-symbol?`, `qualified-symbol?`, `ident?`, `simple-ident?`, `qualified-ident?`, `sequential?`, `reversible?`, `sorted?` | 1 |
| Arithmetic | `+`, `-`, `*`, `/`, `inc`, `dec`, `<`, `<=`, `>`, `>=`, `=`, `not=`, `zero?`, `pos?`, `neg?`, `even?`, `odd?`, `max`, `min`, `quot`, `rem`, `mod`, `bit-and`, `bit-or`, `bit-xor`, `bit-not`, `bit-set`, `bit-clear`, `bit-flip`, `bit-test`, `bit-shift-left`, `bit-shift-right`, `bit-shift-right-zero-fill`, `unchecked-add`, `unchecked-add-int`, `unchecked-subtract`, `unchecked-subtract-int`, `unchecked-multiply`, `unchecked-multiply-int`, `unchecked-divide-int`, `unchecked-remainder-int`, `unchecked-inc`, `unchecked-inc-int`, `unchecked-dec`, `unchecked-dec-int`, `unchecked-negate`, `unchecked-negate-int` | 1 |
| Strings | `str`, `subs`, `clojure.string/blank?`, `clojure.string/capitalize`, `clojure.string/ends-with?`, `clojure.string/includes?`, `clojure.string/index-of`, `clojure.string/join`, `clojure.string/last-index-of`, `clojure.string/lower-case`, `clojure.string/re-quote-replacement`, `clojure.string/replace`, `clojure.string/replace-first`, `clojure.string/reverse`, `clojure.string/split`, `clojure.string/split-lines`, `clojure.string/starts-with?`, `clojure.string/trim`, `clojure.string/trim-newline`, `clojure.string/triml`, `clojure.string/trimr`, `clojure.string/upper-case` | 1 |
| Scalars | `name`, `namespace`, `keyword`, `symbol` | 1 |
| Maps | `hash-map`, `array-map`, `sorted-map`, `get`, `assoc`, `dissoc`, `merge`, `update`, `select-keys`, `contains?`, `keys`, `vals` | 1 |
| Vectors | `vector`, `conj`, `count`, `nth`, `get`, `assoc`, `update`, `contains?`, `subvec`, `first`, `second`, `last`, `peek`, `pop`, `rest` | 1 |
| Lists | `list`, `list*`, `list-of`, `cons`, `conj`, `count`, `nth`, `first`, `second`, `last`, `peek`, `pop`, `rest` | 2 |
| Sequences | `seq`, `empty`, `empty?`, `into`, `take`, `drop`, `reverse`, `range`, `every?`, `not-any?`, `not-every?`, `map`, `filter`, `remove`, `take-while`, `drop-while`, `distinct`, `dedupe`, `sort`, `sort-by`, `concat`, `mapcat`, `vec`, `set`, `repeat`, `repeatedly`, `interpose`, `interleave`, `partition`, `partition-all`, `reductions`, `map-indexed`, `filterv`, `mapv`, `reduce`, `reduce-kv`, `butlast`, `take-last`, `drop-last`, `take-nth`, `split-at`, `split-with`, `partition-by`, `bounded-count`, `dorun`, `doall` | 2 |
| Functions | `apply`, `comp`, `partial`, `identity`, `constantly`, `complement`, `every-pred`, `some-fn`, `juxt`, `run!` | 2 |
| Comparison helpers | `distinct?`, `compare`, `max-key`, `min-key` | 2 |
| Sets | `hash-set`, `sorted-set`, `set-of`, `conj`, `contains?`, `disj` | 3 |
| Protocols | `defprotocol`, `extend-type`, static method dispatch by receiver type | 3 |

## Type System

The first type system should be explicit and structural.
It should infer ordinary function parameter types from source-level constraints where possible, while allowing optional annotations for ambiguous cases.

Scalar types are `int`, `string`, `symbol`, `keyword`, `bool`, and `unit`.
lg represents surface `nil` separately from explicit OCaml option values.
Control-flow joins lower a Clojure nilable type to an OCaml option. `:unit`
remains available for host side-effecting values, including `^:unit`
parameters and direct host calls.
Arithmetic starts as integer-only; ratio-producing Clojure arities such as unary `/` are documented differences until numeric tower support exists.

Type predicates are resolved from static lg types. This includes scalar
predicates and collection capability predicates such as `coll?`,
`associative?`, `indexed?`, `seqable?`, `counted?`, `sequential?`,
`reversible?`, and `sorted?`. Numeric tower predicates currently reflect the
integer-only runtime.

Scalar helpers such as `boolean`, `name`, `namespace`, `keyword`, and `symbol`
are implemented for the types currently represented in lg. Unchecked integer
operations lower directly to OCaml integer operators.

`if-not`, `when`, and `cond` are compiler-recognized forms in the static core.
Missing branches produce Clojure `nil`; joining `nil` with a concrete value
produces a nullable result. lg checks branch compatibility for lg-owned core types, while
OCaml-owned branch result compatibility is delegated to the OCaml typechecker.

`match` is a compiler-recognized static pattern form. The current subset
supports scalar literal patterns, `_`, symbol binders, and fixed-length
list/vector patterns. Literal patterns participate in parameter type inference;
uppercase symbols lower as OCaml constructor patterns for opaque host-owned
targets. Module-qualified payload constructor patterns such as `(Msg.Named x)`
also lower directly to OCaml for opaque host-owned targets, so constructor
existence, arity, and payload typing can remain owned by OCaml.
OCaml-owned match branch result compatibility is also delegated to OCaml.
Exhaustiveness checking is out of scope for the current common subset
and remains owned by OCaml for host types.

Top-level expression forms are emitted as `let _ = ...`; top-level structural
map literals still need `def` so the compiler can emit record definitions.

Protocols are represented as compile-time method signatures plus generated
implementation functions. The current subset supports scalar receiver type
keywords such as `:int` and `:string`, checks implementation return types
against the protocol signature, and dispatches method calls by the first
argument's static type.

Modules compile to OCaml modules. The current static subset supports
`module-signature`, `type-alias`, `type-variant`, `open`, `include`,
`module-alias`, `def`, `defn`, and nested `module` forms in module bodies.
Qualified calls such as `Math/add2` resolve through the type environment.
`(open Math)` emits an OCaml `open` item and exposes already-known module
value/function bindings as unqualified lg symbols, including OCaml record
type metadata. `(include Math)` emits an OCaml `include` item, exposes
already-known module bindings as unqualified symbols, and re-exports direct
included bindings when used inside another module, including OCaml record type
metadata. `(module-alias M Math)` emits an
OCaml module alias and exposes already-known `Math/...` bindings as `M/...`.
`(module-signature MathSig (val answer :int))` emits an OCaml module type.
Signatures also support abstract type items such as `(type user-id)` and
manifest type items such as `(type user-id :int)`, which lower to OCaml
`type user_id` and `type user_id = int`. Nested signature modules use
`(module Inner InnerSig)` and lower to native OCaml signature module items;
their known values remain addressable through functor parameters. Signature
`(include BaseSig)` items lower to native OCaml signature includes and propagate
the included signature's known value metadata.
`(module Math MathSig ...)` emits an ascribed module whose signature match is
checked by OCaml. `(module-functor Make [M MathSig] ...)` emits an OCaml
functor. Additional name/signature pairs in the parameter vector lower to
curried functor parameters, and `module-apply` accepts their module arguments in
order while exposing the applied module's already-known result bindings and
OCaml record type metadata. Functor parameter and application signature
matching remain owned by OCaml.

`subs` supports two- and three-argument typed string slicing.

Vector types are homogeneous as `vector<T>` and compile to `Rrbvec.t`.

Vectors are associative collections: integer indexes work with `get`, `assoc`,
`update`, and `contains?`.

`subvec` returns a persistent vector slice.

List types are homogeneous as `list<T>` and compile to OCaml lists.

`first`, `second`, and `last` work on typed lists, vectors, and sets.

`rest` preserves the concrete list, vector, or set type and returns a
same-typed empty collection at the end.

`nth` supports a typed default value for out-of-range list and vector indexes.

`empty` returns a same-typed empty list, vector, set, or string.

`into` transfers elements between typed list, vector, and set collections when
the element types match.

`take` and `drop` return same-typed list or vector slices.

`reverse` returns a same-typed reversed list or vector.

`range` returns an eager typed integer list.

`every?`, `not-any?`, and `not-every?` return typed booleans for list, vector,
and set predicates.

Sequence APIs are eager in the current runtime. The compiler prefers concrete
typed lists, vectors, and sets over lazy seq objects until the type system has a
dedicated sequence abstraction.

`map`, `filter`, `mapcat`, `sort-by`, `reduce`, `butlast`, `take-last`,
`drop-last`, `take-nth`, `split-at`, `split-with`, `partition-by`,
`bounded-count`, `dorun`, `doall`, and `run!` operate eagerly over concrete
typed collections.

`interleave` accepts two or more same-element-type collections, returns an
eager typed list, and stops when the shortest input is exhausted.

`apply` supports integer binary reducers over typed lists, vectors, and sets,
including fixed leading integer arguments before the final collection.

Function helpers such as `comp`, `partial`, `identity`, `constantly`,
`complement`, `every-pred`, `some-fn`, and `juxt` remain statically typed.
`every-pred` and `some-fn` are source-owned and expose the pinned ClojureScript
constructor arities plus returned zero-, one-, two-, three-, and variadic
arities. Their predicates share one static argument domain; `some-fn` preserves
the first truthy result without a dynamic collection boundary.

Comparison helpers `distinct?`, `compare`, `max-key`, and `min-key` are
supported for same-typed comparable scalar values.

`conj` accepts one or more same-typed values after a list, vector, or set.

`disj` accepts zero or more same-typed values after the set.

Map literal types are structural records keyed by Clojure keywords.

`hash-map` creates structural records from keyword/value pairs.

`array-map` retains the ordinary static map representation. `sorted-map` and
`sorted-map-by` use the source standard library's typed persistent red-black
tree and preserve comparator order across `assoc`, `dissoc`, and sequence
operations.

`mk-bound-fn`, `subseq`, and `rsubseq` use the source `ISorted` protocol through
the closed `sorted<entry;key;storage>` capability. This keeps tree-map entries
as `tuple<key,value>` and tree-set entries as values without dynamic packing or
same-arity collection-specific overloads.

Keyword lookup in a typed context can infer structural map field requirements
for unannotated function parameters.

Function calls can pass wider structural maps when all fields required by the
callee are present with compatible types.

Static destructuring is supported for `let`, `fn`, and `defn`. Associative
destructuring works on structural maps with `:keys`, direct `{local :keyword}`
bindings, scalar literal `:or` defaults, and `:as`; sequential destructuring
works on typed vectors and lists with fixed positional bindings, `& rest`, and
`:as`. Destructured function parameters infer row-shaped structural map
requirements, so callers may pass wider maps when the required fields are
present.

Generated OCaml preserves row-polymorphic calls by emitting a narrow record type
for each row-shaped function parameter and projecting wider structural records
to that narrow type at call sites. This avoids replacing maps with dynamic
dictionaries while still allowing shared-field function reuse across different
map shapes.

`keys` returns a homogeneous `vector<keyword>`.

`assoc` returns a new structural record type when adding one or more fields.

`assoc` rejects changing an existing field to a different type.

`dissoc` returns a new structural record type when removing one or more fields.

`merge` and `update` preserve existing field types for overlapping keys.

`update` passes the existing field value followed by any extra arguments to the
update function.

`get` on a structural map and literal keyword returns that field type.

Three-argument `get` returns a typed default when the literal key is absent.

Same-shaped structural maps compare field by field with `=` and `not=`.

`if` requires a bool condition and same-type branches.

`loop` introduces simple local bindings and lowers to an OCaml tail-recursive
function. `recur` is valid only in the tail position of the nearest `loop`,
must receive exactly one argument for every loop binding, and must preserve the
static type of each binding. For OCaml-owned loop binding types, recur argument
compatibility is delegated to the OCaml typechecker.

## Architecture Tasks

1. Replace the hard-coded parser with a general reader AST.

2. Treat OCaml Parsetree plus the OCaml typechecker as the long-term typed backend boundary.
   lg should not grow a complete duplicate of OCaml's type system.
   The lg semantic layer should only keep source-level forms, core API compatibility rules, collection representation metadata, and diagnostics that cannot be delegated to OCaml.

3. Add reader tokens for brackets, comments, and nested expressions.

4. Parse source into `form` values for symbols, keywords, literals, lists, vectors, and maps.

5. Lower top-level forms into typed statements.

6. Add a typed expression representation that carries scalar, vector, record, and unit types.

7. Add a static environment for top-level bindings and local bindings.

8. Implement built-in core functions in the type checker.

9. Emit OCaml expressions for all supported typed expressions.

10. Emit OCaml record types for structural map shapes.

11. Keep top-level bindings unqualified and use explicit modules as ownership
    boundaries.

12. Resolve qualified symbols such as `People/user` through module metadata.

13. Generate OCaml module and binding names deterministically and detect
    collisions.

15. Keep generated OCaml deterministic so tests can compare output or behavior.

16. Add a CLI `--run` path that compiles generated OCaml and executes it.

17. Add a ClojureDart-style compatibility document that records lg differences from JVM Clojure.

18. Treat symbol munging as a dedicated compiler module, not local string replacement.
    `Names` owns source-to-OCaml identifier munging, including OCaml reserved
    words and digit-leading generated names.

19. Support top-level `require` only for OCaml packages/modules and the typed
    `clojure.string` compatibility module.

20. Keep ClojureDart as a reference for non-JVM dialect ergonomics and compatibility documentation.

21. Add an incremental compiler state that persists the top-level environment,
    modules, generated record type counter, and emitted items.

22. Expose a public incremental API that can compile a source chunk into emitted OCaml while returning the next compiler state.

23. Keep incremental output deterministic and compatible with whole-file compilation.

24. Preserve static protocol signatures and implementations in the same
    incremental compiler state used for ordinary module and value bindings.

25. Make Parsetree lowering the primary correctness path.
    Public source APIs and CLI compile/run paths now print checked
    `Parsetree.structure` output. The old source backend can remain useful for
    debugging internals, but supported expression categories should lower from
    structured `Ocaml_ir` nodes rather than source-backed fallback expressions.

26. Add OCaml-native type surface incrementally after the Parsetree boundary is solid: variants, tuples, option/result, type aliases, module aliases, module signatures, functors, `open`, `include`, and ordinary OCaml package references.
    The first supported host-owned type boundary is opaque parameter
    annotations such as `^:int`, which lower to OCaml constraints and are
    delegated to the OCaml typechecker instead of being fully interpreted by
    lg. Host-owned type application annotations such as
    `^:option<int>`, `^:result<string;string>`, and
    `^:tuple<int;string>` lower to OCaml type constructors or tuple
    constraints; `;` separates multiple type arguments because commas are reader
    whitespace. Type aliases such as `(type-alias user-id :int)`
    now lower to OCaml type declarations in both source and Parsetree backends.
    OCaml record declarations such as
    `(type-record user (name :string) (age :int))` lower to OCaml record types,
    values can be constructed with `(record user (name "Ada") (age 41))`,
    module records can be constructed with qualified type names such as
    `(record User.user ...)` including through opened or included modules,
    and fields can be accessed with
    `(:name value)`.
    OCaml option/result values can be constructed with `(Some value)`,
    `None`, `(Ok value)`, and `(Error value)`. Variant
    declarations such as `(type-variant status Active Inactive)` and payload
    constructor declarations such as `(type-variant message (Named :string))`
    lower directly to OCaml variants. Constructor values such as
    `Active` and `(Named "Ada")` lower
    directly to OCaml. Tuple values such as `(tuple 1 "Ada")` lower to
    OCaml tuples. `match` can lower uppercase symbols as OCaml constructor
    patterns for opaque host-owned targets. `(open Math)` lowers to a structured
    OCaml open item and updates the lg environment for already-known module
    bindings and OCaml record type metadata.
    `(include Math)` lowers to a structured OCaml include item, exposes
    already-known module bindings as unqualified symbols, and re-exports direct
    included bindings from module bodies.
    `(module-alias M Math)` lowers to an OCaml module alias in both source and
    Parsetree backends and updates the lg environment for already-known
    aliased module bindings. `(module-signature MathSig (val answer :int))`
    lowers to an OCaml module type; abstract signature type items such as
    `(type user-id)` and manifest items such as `(type user-id :int)`
    lower to OCaml `type user_id` and `type user_id = int`.
    `(module Math MathSig ...)` lowers to an ascribed module whose signature
    match remains owned by OCaml.
    `(module-functor Make [M MathSig] ...)` and
    `(module-apply App Make Math)` lower to OCaml functor and application module
    expressions in both source and Parsetree backends. Applied modules expose
    already-known result bindings and OCaml record type metadata. Generic host
    calls such as `(Stdlib.abs -42)` lower to ordinary OCaml module
    references with an explicit return type. They also resolve required OCaml
    aliases and refers, for example `(std/abs -42)` after
    `[ocaml.Stdlib :as std]` and
    `(uppercase_ascii "ada")` after
    `[ocaml.String :refer [uppercase_ascii]]`, without adding those unknown
    host functions to lg's typed core table. OCaml value refers remain
    available inside module and functor bodies compiled from the current
    namespace. Function existence, argument arity, and argument compatibility
    remain owned by the OCaml compiler.
    `Lg.Compiler.typecheck_parsetree` runs the generated Parsetree through
    the OCaml compiler-libs typechecker as the current explicit final-truth
    gate. It adds local Dune build CMI directories for lg and `Rrbvec` when
    available, and accepts extra include directories through
    `LG_OCAML_INCLUDE_PATH`. Public `compile_parsetree` and
    `compile_chunk_parsetree` now run this gate before returning. Public source
    APIs and CLI compile/run paths print the checked Parsetree output before
    returning or executing generated OCaml source. Diagnostic-aware source APIs
    capture enabled OCaml warnings; the CLI writes them to stderr and the LSP
    publishes them with warning severity, so exhaustiveness and redundancy
    diagnostics cross the lg tooling boundary without a parallel checker.

## Phase 1 Task List

1. Write failing integration tests for nested calls, vector literals, map field access, `if`, arithmetic, `str`, and collection printing.

2. Write failing integration tests for type errors.

3. Run `rtk dune test --root .` and confirm failures are from missing language behavior.

4. Implement the reader AST.

5. Vendor `RCmerci/rrbvec` under `vendor/rrbvec` until it is available through opam or another package source.

6. Implement type inference for literals, defs, calls, vectors, maps, namespaces, and if.

7. Implement core API typing and code generation for Phase 1 APIs.

8. Update `examples/person.cljc` to exercise top-level `require` and explicit
   modules without a namespace declaration.

9. Run `rtk dune test --root .`.

10. Run `rtk dune exec --root . lg -- --run examples/person.cljc`.

11. Refactor duplicated printing and type comparison code.

12. Re-run all verification commands.

## Edge Cases

Empty vectors need a type annotation eventually.
For Phase 1 they should be rejected with a clear error.

Heterogeneous vectors should be rejected.

Duplicate map literal keys should be rejected.

Unknown symbols should be rejected.

Unqualified symbols resolve in lexical or top-level scope.

Qualified symbols resolve to exactly the requested module.

Unknown map fields should be rejected when accessed with a literal keyword.

Branch type mismatch should be rejected.

Arithmetic should reject strings, bools, vectors, maps, and unit.

## Questions

Should lg eventually use a persistent map library in addition to `Rrbvec` for vectors.

Empty collections can use explicit helper annotations such as `(vector-of :int)`,
`(list-of :keyword)`, and `(set-of :keyword)`.

Function parameters can use lg annotations such as `^:unit` for
side-effecting values, and can also use opaque host-owned annotations such as
`^:int` when the body only needs to pass the value through or call host
code that OCaml will typecheck. Host-owned type applications use
angle brackets, for example `^:option<int>` and
`^:result<string;string>`. OCaml tuple annotations use
`^:tuple<int;string>`.

OCaml-owned type aliases can be emitted with `(type-alias user-id :int)`
and referenced from annotations such as `^:user_id`.

OCaml-owned records can be emitted with
`(type-record user (name :string) (age :int))`, constructed with
`(record user (name "Ada") (age 41))`, constructed across module
boundaries with qualified type names such as `(record User.user ...)`,
constructed through module aliases such as `(record U.user ...)`, exposed
through opened modules such as `(open User)` plus `(record user ...)`,
and accessed with `(:name value)`.

OCaml-owned variants can be emitted with `(type-variant status Active Inactive)`
or payload constructors such as `(type-variant message (Named :string))`, and
constructor values can be emitted with `Active` or
`(Named "Ada")`. For opaque host-owned targets, constructor
patterns such as `(Msg.Named name)` lower directly to OCaml without requiring a
lg constructor binding.

OCaml option/result constructors are available as explicit host forms:
`(Some value)`, `None`, `(Ok value)`, and
`(Error value)`. Match forms can destructure them with OCaml constructor
patterns such as `(Some x)`, `None`, `(Ok value)`, and `(Error err)`, while
payload and polymorphic option/result typing remain owned by OCaml.

OCaml tuples are available as explicit host forms with `(tuple a b ...)`.
Match forms can destructure them with tuple patterns such as
`(tuple id name)`, while element compatibility remains owned by OCaml.

Keyword lookup syntax like `(:name user)` is supported in addition to `(get user :name)`.

Should Clojure sequence APIs be eager by default or backed by OCaml `Seq.t`.

lg uses top-level `require` for a small typed OCaml host interop table, for
example `[ocaml.String :as string]` or
`[ocaml.String :refer [length]]`. For ordinary OCaml module functions outside
that table, `(Module.function args...)` is the explicit
interop escape hatch, and it can use required aliases/refers for OCaml modules
without adding those functions to lg's typed core table.

## Testing Details

The tests exercise source-level behavior by compiling lg source to OCaml, compiling the generated OCaml with `ocamlc`, running the executable, and asserting stdout or compiler error messages.
They do not test internal AST shapes directly.

## Implementation Details

- Keep implementation separated into `Ast`, `Lexer`, `Parser`, `Toolchain`, `Typecheck`, `Codegen`, and support modules.
- Keep common collection and eager sequence lowering in focused core modules
  rather than growing `Typecheck` with runtime code templates.
- Use structural record types for maps.
- Use `Rrbvec.t` for Phase 1 vectors.
- Use generated helper functions only when necessary.
- Preserve Clojure function names at the source layer.
- Emit readable OCaml for debugging.
- Reject unsupported syntax explicitly.
- Keep macros out of the reader and evaluator.
- Keep tests behavior-oriented.
- Keep CLI behavior compatible with the existing prototype.
- Keep whole-file and incremental compilation paths sharing the same frontend, typechecker, and backend modules.
- Keep lg type analysis focused on source elaboration and Clojure-like core
  semantics. Do not implement OCaml's full type system in lg when the OCaml
  compiler can own the check after Parsetree lowering.
- Keep `Types` focused on lg type shapes and per-binding lowering metadata.
  Top-level/module lowering items belong in `Lowered`, and the test suite
  guards against moving those item definitions back into `Types`.
- Use `Lg.Compiler.typecheck_parsetree` when a test or caller needs the
  host-language truth check without shelling out to `ocamlc`; it should cover
  generated code that depends on lg runtime modules and `Rrbvec` CMIs.
- Keep public source and Parsetree compilation APIs gated by the OCaml
  typechecker. For incremental compilation, typecheck the accumulated Parsetree
  state while still returning only the current chunk's source or structure.
- Lower compiled items independently in the Parsetree backend. Structural
  record definitions and ordinary top-level bindings use direct Parsetree
  builders. Row type definitions, functions, and protocol implementations are
  grouped structured items. Modules recursively contain compiled items and
  lower directly to Parsetree; module aliases lower directly to module
  bindings with identifier module expressions; module signatures lower directly
  to module type declarations; ascribed modules use constrained module
  expressions; functors and applications lower directly to functor and apply
  module expressions. Typed expressions carry shared `Ocaml_ir` nodes; scalar
  literals, identifiers, list/vector construction, applications, conditionals,
  ordinary functions, typed parameter patterns, multi-form sequencing, simple
  local bindings, static matches, the integer/boolean/string cores, scalar
  helpers, common function combinators, direct record fields/values, common
  collection operations, recursive sequence helpers, eager sequence transforms,
  and destructuring helpers lower from structured expression nodes. Keep the
  static regression check in the test suite green so the legacy unstructured
  expression path cannot return silently.
- Preserve enough compiler state to support editor/server workflows without reparsing and rechecking unrelated chunks.
- Use extensive integration tests before expanding each core API category.
- The current `RCmerci/rrbvec` repository has an opam file but no package in the active opam index, so the prototype vendors its library source.

## Question

The main open design choice is how aggressively to expose OCaml-native type
features through Clojure-like syntax now that the Parsetree backend no longer
uses expression-level source fallbacks.

---
