# Clojure Core Static Compiler Implementation Plan

Goal: Build cljml into a statically typed Clojure-syntax language that compiles to OCaml and exposes a core API that stays close to Clojure where static typing permits.

Architecture: cljml should follow the same broad shape as ReasonML: parse a source syntax into an AST, type check that AST against a typed core language, then emit normal OCaml.
The frontend keeps Clojure surface syntax, while the backend emits OCaml data structures and functions so generated code can use the OCaml compiler and OCaml packages.
The implementation should mirror ReasonML's toolchain boundary: a frontend parses source into an AST, a type/lowering phase builds typed items, and a backend prints OCaml.
cljml should also support incremental compilation: callers must be able to parse, typecheck, and emit one source chunk while preserving namespace state, aliases, generated type counters, and previously compiled bindings for later chunks.

Tech Stack: OCaml 5.4.1, Dune, generated OCaml backend, `RCmerci/rrbvec` for persistent vectors, integration tests that compile and run emitted OCaml.

Related: Builds on the initial cljml prototype in `/Users/tiensonqin/Documents/cljml`.

## Problem statement

The current compiler is a prototype with hard-coded top-level `def`, `assoc`, `dissoc`, and `print` cases.

The requested language is broader.
It should accept familiar Clojure syntax, reject type errors before OCaml compilation, provide built-in data structures, and expose core APIs with names and behavior close to Clojure.

Macros are explicitly out of scope.
Reader macros, syntax quote, metadata, vars as runtime objects, multimethods, laziness, dynamic vars, and JVM interop are out of scope for the first complete static language.
Namespace declarations and qualified symbol resolution are in scope.

The reference model is ReasonML in architecture rather than syntax.
ReasonML preserves an alternate syntax over the OCaml toolchain.
cljml should preserve Clojure-like syntax over a typed core and OCaml backend.

ClojureDart is the reference model for Clojure dialect ergonomics over a non-JVM host.
Its docs emphasize explicit host differences, namespace import behavior, symbol munging, and host package interop as first-class compiler concerns.
cljml should follow that posture for OCaml packages and should keep a documented differences surface instead of silently diverging from Clojure.

API compatibility, runtime representation, and performance must be balanced explicitly.
The source-level API should stay close to Clojure, but the typed core should prefer predictable OCaml representations over runtime dynamism.
When exact Clojure behavior would require slow dynamic dispatch or weak static types, cljml should document the difference, choose a typed representation, and add tests that lock in that behavior.

## Testing Plan

I will add integration tests that compile cljml source strings to OCaml, compile the emitted OCaml with `ocamlc`, run the result, and assert stdout.

I will test Clojure-style forms using vector literals, map literals, ordinary prefix calls, and nested expressions.

I will test static type errors for heterogeneous vectors, invalid arithmetic arguments, `get` on unknown map fields, `assoc` changing an existing field type, and `if` branch type mismatch.

I will test command-line behavior through `dune exec bin/cljml_cli.exe -- --run examples/person.cljml`.

I will add extensive tests across these layers:

- Reader/parser behavior for Clojure syntax accepted by cljml.
- Typechecker success and failure cases for every supported core API.
- Generated OCaml compilation and execution for user-visible behavior.
- Namespace and alias resolution across multiple namespaces.
- Incremental compilation state across source chunks.
- Runtime representation checks for persistent vectors and structural maps where behavior depends on representation.
- CLI smoke tests for file compilation and `--run`.

NOTE: I will write *all* tests before I add any implementation behavior.

## Scope

The compiler should support this syntax without macros.

| Syntax | Example | Phase |
| --- | --- | --- |
| Scalar literals | `1`, `"Ada"`, `true`, `false`, `nil` | 1 |
| Symbols | `x`, `user-name` | 1 |
| Keywords | `:name`, `:admin?` | 1 |
| Lists as calls | `(+ 1 2)` | 1 |
| Lists as data | `(list 1 2 3)` | 2 |
| Vectors | `[1 2 3]` | 1 |
| Maps | `{:name "Ada" :age 36}` | 1 |
| `def` | `(def x 1)` | 1 |
| `if` | `(if admin? "yes" "no")` | 1 |
| Conditional forms | `(if-not ready? "wait" "go")`, `(when ready? (println "go"))`, `(cond ready? "go" :else "wait")` | 3 |
| `let` | `(let [x 1] (+ x 2))` | 2 |
| `fn` | `(fn [x] (+ x 1))` | 2 |
| `defn` | `(defn inc1 [x] (+ x 1))` | 2 |
| Static protocols | `(defprotocol Labelled (label [x] :string))` | 3 |
| Namespace form | `(ns app.main)` | 1 |

## Core API Roadmap

| Category | Functions | Phase |
| --- | --- | --- |
| Printing | `print`, `println`, `pr-str` | 1 |
| Boolean and predicates | `not`, `true?`, `false?`, `nil?`, `some?`, `boolean`, `int?`, `integer?`, `number?`, `nat-int?`, `pos-int?`, `neg-int?`, `string?`, `keyword?`, `boolean?`, `vector?`, `list?`, `seq?`, `set?`, `map?`, `fn?`, `coll?`, `associative?`, `indexed?`, `seqable?`, `counted?`, `any?`, `rational?`, `ratio?`, `float?`, `double?`, `decimal?`, `simple-keyword?`, `qualified-keyword?`, `ident?`, `simple-ident?`, `qualified-ident?`, `sequential?`, `reversible?`, `sorted?` | 1 |
| Arithmetic | `+`, `-`, `*`, `/`, `inc`, `dec`, `<`, `<=`, `>`, `>=`, `=`, `not=`, `zero?`, `pos?`, `neg?`, `even?`, `odd?`, `max`, `min`, `quot`, `rem`, `mod`, `bit-and`, `bit-or`, `bit-xor`, `bit-not`, `bit-set`, `bit-clear`, `bit-flip`, `bit-test`, `bit-shift-left`, `bit-shift-right`, `bit-shift-right-zero-fill`, `unchecked-add`, `unchecked-add-int`, `unchecked-subtract`, `unchecked-subtract-int`, `unchecked-multiply`, `unchecked-multiply-int`, `unchecked-divide-int`, `unchecked-remainder-int`, `unchecked-inc`, `unchecked-inc-int`, `unchecked-dec`, `unchecked-dec-int`, `unchecked-negate`, `unchecked-negate-int` | 1 |
| Strings | `str`, `subs`, `clojure.string/blank?`, `clojure.string/capitalize`, `clojure.string/ends-with?`, `clojure.string/includes?`, `clojure.string/index-of`, `clojure.string/join`, `clojure.string/last-index-of`, `clojure.string/lower-case`, `clojure.string/re-quote-replacement`, `clojure.string/replace`, `clojure.string/replace-first`, `clojure.string/reverse`, `clojure.string/split`, `clojure.string/split-lines`, `clojure.string/starts-with?`, `clojure.string/trim`, `clojure.string/trim-newline`, `clojure.string/triml`, `clojure.string/trimr`, `clojure.string/upper-case` | 1 |
| Scalars | `name`, `keyword` | 1 |
| Maps | `hash-map`, `get`, `assoc`, `dissoc`, `merge`, `update`, `select-keys`, `contains?`, `keys`, `vals` | 1 |
| Vectors | `vector`, `conj`, `count`, `nth`, `get`, `assoc`, `update`, `contains?`, `subvec`, `first`, `second`, `last`, `peek`, `pop`, `rest` | 1 |
| Lists | `list`, `list-of`, `cons`, `conj`, `count`, `nth`, `first`, `second`, `last`, `peek`, `pop`, `rest` | 2 |
| Sequences | `seq`, `empty`, `empty?`, `into`, `take`, `drop`, `reverse`, `range`, `every?`, `not-any?`, `not-every?`, `map`, `filter`, `remove`, `take-while`, `drop-while`, `distinct`, `dedupe`, `sort`, `concat`, `vec`, `set`, `repeat`, `repeatedly`, `interpose`, `interleave`, `partition`, `partition-all`, `reductions`, `map-indexed`, `filterv`, `mapv`, `reduce`, `reduce-kv`, `butlast`, `take-last`, `drop-last`, `take-nth`, `split-at`, `split-with`, `partition-by`, `bounded-count`, `dorun`, `doall` | 2 |
| Functions | `apply`, `comp`, `partial`, `identity`, `constantly`, `run!` | 2 |
| Sets | `hash-set`, `set-of`, `conj`, `contains?`, `disj` | 3 |
| Protocols | `defprotocol`, `extend-type`, static method dispatch by receiver type | 3 |

## Type System

The first type system should be explicit and structural.
It should infer ordinary function parameter types from source-level constraints where possible, while allowing optional annotations for ambiguous cases.

Scalar types are `int`, `string`, `keyword`, `bool`, `nil`, and `unit`.
Arithmetic starts as integer-only; ratio-producing Clojure arities such as unary `/` are documented differences until numeric tower support exists.

Type predicates are resolved from static cljml types. This includes scalar
predicates and collection capability predicates such as `coll?`,
`associative?`, `indexed?`, `seqable?`, `counted?`, `sequential?`,
`reversible?`, and `sorted?`. Numeric tower predicates currently reflect the
integer-only runtime.

Scalar helpers such as `boolean`, `name`, and `keyword` are implemented for the
types currently represented in cljml. Unchecked integer operations lower
directly to OCaml integer operators.

`if-not`, `when`, and `cond` are compiler-recognized forms in the static core.
`cond` requires an `:else` branch until cljml has a union type for implicit nil.

Top-level expression forms are emitted as `let _ = ...`; top-level structural
map literals still need `def` so the compiler can emit record definitions.

Protocols are represented as compile-time method signatures plus generated
implementation functions. The current subset supports scalar receiver type
keywords such as `:int` and `:string`, checks implementation return types
against the protocol signature, and dispatches method calls by the first
argument's static type.

`subs` supports two- and three-argument typed string slicing.

Vector types are homogeneous as `vector<T>` and compile to `Rrbvec.t`.

Vectors are associative collections: integer indexes work with `get`, `assoc`,
`update`, and `contains?`.

`subvec` returns a persistent vector slice.

List types are homogeneous as `list<T>` and compile to OCaml lists.

`rest` returns a same-typed empty list or vector when called on an empty list
or vector.

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

`map`, `filter`, `reduce`, `butlast`, `take-last`, `drop-last`, `take-nth`,
`split-at`, `split-with`, `partition-by`, `bounded-count`, `dorun`, `doall`,
and `run!` operate eagerly over concrete typed collections.

`apply` supports integer binary reducers over typed lists, vectors, and sets.

`disj` accepts zero or more same-typed values after the set.

Map literal types are structural records keyed by Clojure keywords.

`hash-map` creates structural records from keyword/value pairs.

Keyword lookup in a typed context can infer structural map field requirements
for unannotated function parameters.

Function calls can pass wider structural maps when all fields required by the
callee are present with compatible types.

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

## Architecture Tasks

1. Replace the hard-coded parser with a general reader AST.

2. Add reader tokens for brackets, nil, comments, and nested expressions.

3. Parse source into `form` values for symbols, keywords, literals, lists, vectors, and maps.

4. Lower top-level forms into typed statements.

5. Add a typed expression representation that carries scalar, vector, record, unit, and nil types.

6. Add a static environment for top-level bindings and local bindings.

7. Implement built-in core functions in the type checker.

8. Emit OCaml expressions for all supported typed expressions.

9. Emit OCaml record types for structural map shapes.

10. Maintain namespace state for top-level forms.

11. Resolve unqualified symbols in the current namespace.

12. Resolve qualified symbols like `people.core/user` across namespaces.

13. Generate OCaml names with namespace prefixes to avoid collisions.

14. Keep generated OCaml deterministic so tests can compare output or behavior.

15. Add a CLI `--run` path that compiles generated OCaml and executes it.

16. Add a ClojureDart-style compatibility document that records cljml differences from JVM Clojure.

17. Treat symbol munging as a dedicated compiler module, not local string replacement.
    `Names` owns source-to-OCaml identifier munging, including OCaml reserved
    words and digit-leading generated names.

18. Extend `ns` parsing to handle `:require` aliases and refers for cljml namespaces and OCaml package/module interop.

19. Keep ClojureDart as a reference for non-JVM dialect ergonomics and compatibility documentation.

20. Add an incremental compiler state that persists current namespace, top-level environment, generated record type counter, and emitted items.

21. Expose a public incremental API that can compile a source chunk into emitted OCaml while returning the next compiler state.

22. Keep incremental output deterministic and compatible with whole-file compilation.

23. Preserve static protocol signatures and implementations in the same
    incremental compiler state used for ordinary namespace bindings.

## Phase 1 Task List

1. Write failing integration tests for nested calls, vector literals, map field access, `if`, arithmetic, `str`, and collection printing.

2. Write failing integration tests for type errors.

3. Run `rtk dune test --root .` and confirm failures are from missing language behavior.

4. Implement the reader AST.

5. Vendor `RCmerci/rrbvec` under `vendor/rrbvec` until it is available through opam or another package source.

6. Implement type inference for literals, defs, calls, vectors, maps, namespaces, and if.

7. Implement core API typing and code generation for Phase 1 APIs.

8. Update `examples/person.cljml` to use `(ns examples.person)`.

9. Run `rtk dune test --root .`.

10. Run `rtk dune exec --root . bin/cljml_cli.exe -- --run examples/person.cljml`.

11. Refactor duplicated printing and type comparison code.

12. Re-run all verification commands.

## Edge Cases

Empty vectors need a type annotation eventually.
For Phase 1 they should be rejected with a clear error.

Heterogeneous vectors should be rejected.

Duplicate map literal keys should be rejected.

Unknown symbols should be rejected.

Unqualified symbols should resolve only in the current namespace.

Qualified symbols should resolve to exactly the requested namespace.

Unknown map fields should be rejected when accessed with a literal keyword.

Branch type mismatch should be rejected.

Arithmetic should reject strings, bools, vectors, maps, nil, and unit.

## Questions

Should cljml eventually use a persistent map library in addition to `Rrbvec` for vectors.

Empty collections can use explicit helper annotations such as `(vector-of :int)`,
`(list-of :keyword)`, and `(set-of :keyword)`.

Keyword lookup syntax like `(:name user)` is supported in addition to `(get user :name)`.

Should Clojure sequence APIs be eager by default or backed by OCaml `Seq.t`.

cljml mirrors ClojureDart's `ns` `:require` shape for a small typed OCaml host interop table, for example `[ocaml.String :as string]` or `[ocaml.String :refer [length]]`.

## Testing Details

The tests exercise source-level behavior by compiling cljml source to OCaml, compiling the generated OCaml with `ocamlc`, running the executable, and asserting stdout or compiler error messages.
They do not test internal AST shapes directly.

## Implementation Details

- Keep implementation separated into `Ast`, `Lexer`, `Parser`, `Toolchain`, `Typecheck`, `Codegen`, and support modules.
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
- Preserve enough compiler state to support editor/server workflows without reparsing and rechecking unrelated chunks.
- Use extensive integration tests before expanding each core API category.
- The current `RCmerci/rrbvec` repository has an opam file but no package in the active opam index, so the prototype vendors its library source.

## Question

The main open design choice is whether Phase 2 should prioritize functions and `defn`, or persistent maps and sets.

---
