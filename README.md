# cljml

`cljml` is a small prototype for a statically typed Lisp in the Clojure family.
The current backend emits OCaml, so generated programs can be checked by the
OCaml compiler and can later interoperate with OCaml packages.

The compiler pipeline is intentionally split into cljml syntax and typing
first, then OCaml lowering:

- cljml has its own Lisp AST and typed IR for static Clojure-like semantics.
  The long-term architecture follows ReasonML more than a standalone compiler:
  cljml should elaborate Clojure-like syntax into OCaml Parsetree and leave the
  complete host-language type system to the OCaml compiler.
- Typed expressions form a `Semantic_ir` tree whose elaborated nodes retain
  `Semantic_type.ty` annotations. `Semantic_lowering` explicitly erases those
  annotations into `Ocaml_ir`, which is shared by the source and Parsetree
  backends. Scalar literals, identifiers, list/vector construction,
  applications, conditionals, ordinary functions, parameter constraints,
  multi-form sequencing, simple `let` bindings, and static `match` expressions
  are structured. Integer arithmetic, predicates, bitwise operators,
  comparisons, scalar helpers, string slicing, function combinators, record
  field access and values, plus common collection constructors, transforms,
  updates, accessors, and `rest` also lower directly. The expression IR no
  longer has a source-backed fallback node, and the test suite includes a
  static regression guard against reintroducing that legacy unstructured path.
- The checked Parsetree backend is now the stable output path. Public source
  compilation prints the OCaml source from the checked `Parsetree.structure`
  rather than from the legacy source backend.
- `Cljml.Compiler.compile_parsetree` lowers compiled items independently into
  `Parsetree.structure`. Structural records, ordinary top-level values,
  effects, row type definitions, functions, protocol implementations, and
  nested modules, module aliases, and module signatures are constructed
  directly. Typed expression payloads lower from shared `Ocaml_ir` nodes
  instead of reparsed OCaml snippets. The public `compile_parsetree` API now
  runs the generated structure through the OCaml typechecker before returning.
- `Cljml.Compiler.typecheck_parsetree` runs the generated Parsetree through the
  OCaml compiler-libs typechecker. This is the current explicit final-truth
  gate for host-owned checks such as ordinary OCaml module function calls. It
  adds the local Dune build CMI directories for cljml and `Rrbvec` when they are
  available, and accepts extra directories through `CLJML_OCAML_INCLUDE_PATH`.
- `Cljml.Compiler.compile_chunk_parsetree` uses the same incremental compiler
  state as `compile_chunk`, but returns an OCaml structure for the current
  chunk after typechecking the accumulated Parsetree state.
- `Cljml.Compiler.compile_string`, `Cljml.Compiler.compile_chunk`, and the CLI
  compile/run paths use the checked Parsetree printer as their source output.
  Generated host-language errors are caught before source is returned or
  executed. `compile_string_with_diagnostics` and
  `compile_string_with_filename_and_diagnostics` additionally return enabled
  OCaml warnings with the generated source. The CLI prints these warnings to
  stderr without turning successful compilation into failure.
- Lexer tokens and the complete parsed form tree retain source spans. Each
  elaborated expression carries its own source location into Parsetree, while
  generated nodes without a direct form inherit their owning top-level span.
  OCaml diagnostics therefore report precise lines and columns through the
  library API, CLI, and LSP. Incremental compilation preserves locations for
  accumulated chunks. Use
  `compile_string_with_filename` when embedding the compiler with a real path.
- Core cljml type shapes remain in `Types`; lowered top-level/module items live
  in `Lowered`, so backend item construction is kept separate from source type
  metadata.
- Parsetree is not used as cljml's full type system. cljml has a typed,
  memoized lazy-seq representation, while `nil` remains outside the supported
  surface.

Sets use persistent OCaml `Set.Make` modules rather than list-backed values.
The runtime provides comparators for `int`, `string` (including keywords and
symbols), and `bool`, plus typed `list` and `Rrbvec` vector values containing
those scalar types. Every top-level structural map is emitted as a named OCaml
record with a sibling `Set.Make` module using `Stdlib.compare`, so records can
be set elements without sacrificing static types. Same-shaped records are
projected to the set element record type at `hash-set`, `conj`, `contains?`, and
`disj` boundaries.

Supported prototype forms:

```clojure
(require [ocaml.String :as string])
(def x {:name "Ada", :age 36})
(def y (assoc x :admin? true))
(def z (dissoc y :age))
(def ages (conj (vector-of :int) 36))
(def label (str (string/uppercase-ascii (:name z)) ":" (:admin? z) ":" (count ages)))
(println label)
```

The compiler infers record-like map shapes automatically:

- `{:name "Ada", :age 36}` becomes an OCaml record with `string` and `int`
  fields.
- `[36 37 38]` becomes an `Rrbvec.t` persistent vector.
- `(list 1 2 3)` becomes a typed OCaml list.
- `:admin?` is a distinct `keyword` value in the static type system, and
  `(symbol "user" "name")` creates a distinct `symbol` value.
- `(hash-map :name "Ada" :age 36)` creates the same structural map shape as a
  map literal.
- `(assoc x :age 36 :admin? true)` produces a new record shape with added or
  updated fields.
- `(assoc [1 2 3] 1 42)` updates a persistent vector index.
- `(subvec [1 2 3] 1 3)` returns a persistent vector slice.
- `(dissoc y :age :admin?)` produces a new record shape with those fields
  removed.
- `(get x :missing default)` returns a typed default when the field is absent.
- `merge`, `update`, and `select-keys` work on structural maps when field
  keys are known statically.
- `(update x :age + 1)` passes the current field value plus extra arguments to
  the update function.
- `(update xs 0 inc)` updates a persistent vector index.
- `(module Math (defn add2 [x] (+ x 2)))` emits an OCaml module, and
  `Math/add2` resolves to that module binding. The current subset supports
  `module-signature`, `type-alias`, `type-variant`, `open`, `include`,
  `module-alias`, `def`, `defn`, and nested `module` forms inside a module.
  `(open Math)` emits OCaml `open Math` and exposes already-known module
  value/function bindings as unqualified symbols in the current cljml
  environment, including OCaml record type metadata. `(include Math)` emits
  OCaml `include Math`, exposes already-known module bindings as unqualified
  symbols, and re-exports direct included bindings and OCaml record type
  metadata when used inside another module.
  `(module-alias M Math)` emits an
  OCaml module alias and exposes already-known `Math/...` bindings plus OCaml
  record type metadata as `M/...`.
  `(module-signature MathSig (val answer :ocaml/int))` emits an OCaml
  module type. Signatures also support abstract type items such as
  `(type user-id)` and manifest type items such as `(type user-id :ocaml/int)`,
  which lower to OCaml `type user_id` and `type user_id = int`. Signature type
  items accept parameter vectors too: `(type box [a])` declares an abstract
  `'a box`, while `(type box [a] :ocaml/option<param/a>)` declares a manifest
  `'a box = 'a option`. `(module Inner InnerSig)` adds a nested module item to
  a signature. Known nested values remain available through functor parameters,
  for example `M.Inner/value`. `(include BaseSig)` includes another module type
  and propagates its known values into functor parameter scope. Module and
  signature inclusion, plus parameter relationships, are checked by OCaml.
  `(module Math MathSig ...)` emits an ascribed module whose signature match is
  checked by OCaml. `(module-functor Make [M MathSig] ...)`
  emits an OCaml functor. Multiple name/signature pairs declare a curried
  multi-parameter functor, for example `[L MathSig R MathSig]`, and
  `(module-apply App Make Left Right)` applies its arguments in order. Applied
  modules expose the functor's already-known result bindings and OCaml record
  type metadata; parameter and application signature checks remain owned by
  OCaml.
- Clojure symbols and keywords that collide with OCaml reserved words are
  munged when emitted as OCaml identifiers.
- `(defn inc1 [x] (+ x 1))`, `(fn [x] ...)`, and `(let [...] ...)` are
  supported for typed function workflows.
- `let`, `fn`, and `defn` support a static destructuring subset. Map
  destructuring supports `{:keys [...]}`, `{local :keyword}`, scalar literal
  `:or` defaults, and `:as` for structural maps; vector/list destructuring
  supports fixed positions, `& rest`, and `:as` for typed vectors and lists.
- Function parameter types are inferred from body constraints where possible.
  Keyword lookup constraints such as `(:age person)` can infer required
  structural map fields when the field value type is known from context.
  Unconstrained identity-style functions keep their OCaml-owned polymorphism at
  call sites, including `let`-bound `(fn [x] x)` values; cljml only preserves
  the return-parameter relationship needed by core forms such as `str`, and the
  OCaml typechecker remains the final check.
  Row-shaped structural map parameters can accept wider maps with the required
  fields; the compiler projects wide records to generated narrow row records
  before calling the OCaml function.
  Optional `^:int`, `^:string`, `^:symbol`, `^:keyword`, `^:bool`, or `^:unit`
  annotations can make a cljml core parameter type explicit. `nil` is not a
  supported surface value or parameter type. Opaque host-owned
  annotations such as `^:ocaml/int` lower to OCaml parameter constraints and
  are left for the OCaml typechecker; cljml core APIs do not treat them as
  known `:int` or `:string` values. Host-owned type applications use angle
  brackets, for example `^:ocaml/option<int>` and
  `^:ocaml/result<string;string>`. OCaml tuple annotations use the same syntax,
  for example `^:ocaml/tuple<int;string>`; `;` separates multiple type
  arguments because commas are reader whitespace.
- `(type-alias user-id :ocaml/int)` emits an OCaml type alias such as
  `type user_id = int`. The alias can be referenced from host-owned
  annotations like `^:ocaml/user_id`; cljml does not expand or infer through
  the alias itself.
- Aliases, records, and variants accept explicit type parameter vectors.
  `(type-alias maybe [a] :ocaml/option<param/a>)`,
  `(type-record pair [a b] (left :param/a) (right :param/b))`, and
  `(type-variant box [a] (Box :param/a))` lower to ordinary parameterized OCaml
  declarations. Parameter references are scoped to the declaration, and OCaml
  checks relationships between instantiated values.
- `(type-record user (name :string) (age :int))` emits an OCaml record type.
  `(ocaml-record user (name "Ada") (age 41))` constructs a value of that type,
  and `(ocaml-field user-value name)` lowers to an OCaml record field access.
  Module records can be constructed with qualified type names such as
  `(ocaml-record User.user ...)` or through module aliases such as
  `(ocaml-record U.user ...)`. Opened modules expose record type names in the
  current scope, so `(open User)` allows `(ocaml-record user ...)`. cljml checks
  record shape and field names; field value compatibility remains owned by
  OCaml.
  Function parameters do not need named-record annotations. Field reads and
  `assoc` updates infer row constraints, and cljml preserves nominal identity
  when exactly one declared record matches those constraints.
- `(type-variant status Active Inactive)` emits a nullary OCaml variant type
  such as `type status = Active | Inactive`. Payload constructors can be
  declared with forms such as `(Named :string)` or `(Pair :int :string)`.
  `Active` and `(Named "Ada")` lower directly to OCaml constructor values;
  `ocaml-construct` remains as a compatibility spelling. Constructor payload
  typing and exhaustiveness remain owned by OCaml. For opaque host-owned
  targets, module-qualified constructor
  patterns such as `(Msg.Named name)` lower directly to OCaml; constructor
  existence, arity, and payload typing are checked by OCaml.
- `(Some value)`, `None`, `(Ok value)`, and `(Error value)` lower directly to
  their OCaml constructors. The older `ocaml-some`, `ocaml-none`, `ocaml-ok`,
  and `ocaml-error` forms remain compatible. Match forms can destructure them
  with OCaml constructor
  patterns such as `(Some x)`, `None`, `(Ok value)`, and `(Error err)`. cljml
  validates constructor arity and type-application annotation shape, but leaves
  option/result payload compatibility to OCaml.
- `(ocaml-tuple a b ...)` lowers to an OCaml tuple value. Match forms can
  destructure tuple values with `(ocaml-tuple x y ...)`, and tuple annotations
  lower to OCaml tuple constraints. Tuple arity is checked by cljml; element
  compatibility remains owned by OCaml.
- `do`, `fn`, `defn`, and `let` bodies can contain multiple forms; earlier
  forms are evaluated for effects and the final form supplies the value.
- `loop` introduces typed local bindings and `recur` performs a tail call to
  the nearest loop. Recur arguments must preserve the binding count and types,
  and recur is rejected outside a loop tail position. For OCaml-owned loop
  binding types, recur argument compatibility is delegated to the OCaml
  typechecker.
- `if-not`, `when`, and `cond` are compiler-recognized conditional forms.
  `cond` requires an `:else` branch in the current static subset. Core cljml
  branch types are checked by cljml; OCaml-owned branch result compatibility is
  delegated to the OCaml typechecker.
- `match` is a compiler-recognized static pattern form. It supports scalar
  literal patterns, `_`, symbol binders, and fixed-length list/vector patterns
  such as `[]`, `[x]`, and `[x y]`. Structured OCaml patterns include
  `(record (field pattern) ...)`, `(as pattern name)`, and
  `(or left-pattern right-pattern)`. A guarded clause writes its pattern as
  `(when pattern guard)`. These lower directly to OCaml record, alias, or, and
  guarded case nodes. For opaque host-owned targets, uppercase symbols lower as
  OCaml constructor patterns, so constructor typing, guards, and exhaustiveness
  remain owned by the OCaml typechecker.
- `try` lowers to OCaml exception handling. It takes one or more body forms
  followed by `(catch pattern body...)` clauses, and `raise` lowers to OCaml's
  ordinary `raise` call. Catch patterns reuse the same pattern syntax as
  `match`, including constructor payload patterns such as `(Failure message)`;
  exception constructor existence, payload typing, and handler coverage remain
  owned by OCaml.
- Top-level expression forms are evaluated with `let _ = ...`, so side-effect
  forms such as `(when flag (println "ready"))` can appear at file scope.
- `print` writes without a trailing newline; `println` writes with a trailing
  newline.
- `not` follows the statically represented values: only `false` is falsey;
  other values are truthy. cljml does not support a surface `nil` value.
- `int?`, `integer?`, `number?`, `nat-int?`, `pos-int?`, `neg-int?`,
  `string?`, `keyword?`, `boolean?`, `vector?`, `list?`, `seq?`, `set?`,
  `map?`, `fn?`, `coll?`, `associative?`, `indexed?`, `seqable?`, and
  `counted?` are supported as static type predicates. `any?`, `rational?`,
  `ratio?`, `float?`, `double?`, `decimal?`, `simple-keyword?`,
  `qualified-keyword?`, `symbol?`, `simple-symbol?`, `qualified-symbol?`,
  `ident?`, `simple-ident?`, `qualified-ident?`, `sequential?`, `reversible?`,
  and `sorted?` are also supported for the current static type subset.
- `boolean`, `name`, `namespace`, `keyword`, and `symbol` are supported for the
  current scalar subset.
- `subs` supports two- and three-argument typed string slicing.
- `+`, `*`, `-`, `/`, ordered comparisons, `=`, and `not=` follow
  Clojure-style arities where the current type system can represent them.
- `zero?`, `pos?`, `neg?`, `even?`, `odd?`, `max`, `min`, `quot`, `rem`,
  `mod`, `bit-and`, `bit-or`, `bit-xor`, `bit-not`, `bit-shift-left`, and
  `bit-shift-right` are supported for integers.
- `bit-set`, `bit-clear`, `bit-flip`, `bit-test`, `bit-shift-right-zero-fill`,
  and unchecked integer aliases such as `unchecked-add`, `unchecked-add-int`,
  `unchecked-inc`, and `unchecked-negate-int` compile to OCaml integer
  operations.
- `=` and `not=` compare same-shaped structural maps field by field.
- `range` produces a typed memoized lazy seq; the zero-argument form is
  unbounded.
- `list`, `list*`, `list-of`, `cons`, `second`, `last`, `peek`, `pop`, `map`,
  `filter`, `remove`, `take-while`, `drop-while`, `distinct`, `dedupe`,
  `sort`, `concat`, `vec`, `set`, `repeat`, `repeatedly`, `interpose`,
  `interleave`, `partition`, `partition-all`, `reductions`, `map-indexed`,
  `filterv`, `mapv`, `mapcat`, `sort-by`, `reduce`, `reduce-kv`, `apply`,
  `comp`, `partial`, `identity`, `constantly`, `complement`, `every-pred`,
  `some-fn`, `juxt`, `distinct?`, `compare`, `max-key`, `min-key`, `butlast`,
  `take-last`, `drop-last`, `take-nth`, `split-at`, `split-with`,
  `partition-by`, `bounded-count`, `dorun`, `doall`, and `run!` are supported
  for the current typed collection/function subset.
- `interleave` accepts two or more same-element-type collections and stops when
  the shortest input is exhausted.
- `reduce` accepts typed lists, vectors, sets, OCaml arrays, strings, lazy seqs,
  and host OCaml `Seq.t`, list, and array values. It eagerly consumes its input;
  memoized lazy seq nodes are not recomputed on later reductions.
- `Seqable` is a compiler-owned protocol. Named records and host wrapper types
  can implement `-seq`, returning a typed lazy seq, and then work directly with
  `map` and `reduce`; implementations declared in modules are exported with the
  module.
- `Reducible` is a compiler-owned optimization protocol. `reduce` uses a
  matching `-reduce` implementation before falling back to `Seqable`; built-in
  collections specialize directly to their native OCaml folds.
- `apply` supports integer binary reducers over typed lists, vectors, and sets,
  including fixed leading integer arguments before the final collection.
- `get` supports vector indexes, and `nth` supports typed default values for
  lists and vectors.
- `first`, `second`, and `last` work on typed lists, vectors, and sets.
- `rest` preserves the concrete list, vector, or set type and is empty-safe.
- `empty` returns a same-typed empty list, vector, set, or string.
- `into` transfers elements between typed list, vector, and set collections.
- `take` and `drop` return typed memoized lazy seqs for every built-in seqable
  input.
- `reverse` returns a same-typed reversed list or vector.
- `every?`, `not-any?`, and `not-every?` work on typed lists, vectors, and
  sets.
- `array-map` and `sorted-map` create structural maps like `hash-map` in the
  current static subset.
- `hash-set`, `sorted-set`, `set-of`, `conj`, `disj`, and `contains?` are
  supported for homogeneous sets, and common eager sequence helpers work over
  sets where their result has a statically representable type.
- `conj` accepts one or more same-typed values after a list, vector, or set.
- `disj` accepts zero or more same-typed values after the set.
- `(module-alias M Math)` aliases a previously compiled cljml/OCaml module;
  `(open Math)` exposes its known bindings without introducing namespaces.
- `(require [ocaml.String :as string])` aliases an OCaml module. Ordinary
  functions are called directly, for example `(string/uppercase_ascii "ada")`.
- `(require [ocaml.String :refer [uppercase_ascii]])` can refer an ordinary
  OCaml function and call it directly as `(uppercase_ascii "ada")` without
  adding it to cljml's typed core table. These OCaml value refers are also
  available inside module and functor bodies.
- `(require [ocaml.core/Core.Int :as int])` combines the findlib package
  dependency and OCaml module alias. The separate
  `[ocaml.package/core] [ocaml.Core.Int :as int]` form remains compatible.
  Package metadata adds
  recursive `.cmi` directories before elaboration, so calls such as
  `(int/abs -42)` infer their signature. CLI `--run` passes the same
  package list to `ocamlfind ocamlopt -linkpkg` for native linking.
- Required packages also expose constructor metadata through compiler-libs.
  For example, `[ocaml.package/unix] [ocaml.Unix :as unix]` allows
  `(unix/ADDR_UNIX "/tmp/app.sock")` without declaring its payload or result
  type. Constructor names retain OCaml capitalization; payload compatibility
  remains checked by the OCaml typechecker.
- `(Stdlib.abs -42)` reads the ordinary OCaml value signature from the compiler
  environment so cljml can continue elaborating the inferred result. Module
  aliases and referred OCaml values use the same direct form. `ocaml-call`
  remains as a compatibility escape hatch for an explicit host return type.
  OCaml checks function existence and argument compatibility.
- OCaml-native scalar and mutable values stay explicit: float and character
  literals use `1.5` and `\a`; `(ocaml-array ...)`, `(ocaml-array-of :int)`,
  `ocaml-array-get`, and `ocaml-array-set!` lower to OCaml arrays; `ocaml-ref`,
  `ocaml-deref`, and `ocaml-reset!` lower to OCaml references. Core arithmetic
  operators select integer or float OCaml operators from static operand types.
- OCaml labelled and optional arguments use keyword/value pairs, for example
  `(String.starts_with "ada" :prefix "ad")` and
  `(String.edit_distance "abc" "adc" :limit 2)`. Labels are read
  from the compiler signature, optional labels may be omitted, and partially
  applied functions preserve their remaining positional function type.
- `(require [clojure.string :as str])` can alias a typed `clojure.string`
  subset: `blank?`, `capitalize`, `ends-with?`, `includes?`, `index-of`,
  `join`, `last-index-of`, `lower-case`, `re-quote-replacement`, `replace`,
  `replace-first`, `reverse`, `split`, `split-lines`, `starts-with?`, `trim`,
  `trim-newline`, `triml`, `trimr`, and `upper-case`.
- `defprotocol` and `extend-type` support static compile-time dispatch for
  primitive and named OCaml record receivers. Protocol identity distinguishes
  same-named methods through `Protocol/method`; annotated parameter types are
  checked; module-owned protocols are exported as `Module/Protocol/method`;
  and module aliases preserve protocol identity.
- `(:name user)` works as keyword lookup syntax for structural maps.
- `(keys user)` returns a persistent vector of keyword values, and `(vals user)`
  returns a persistent vector when all map values have the same type.
- `(vector-of :int)`, `(list-of :int)`, and `(set-of :int)` create explicitly
  typed empty collections; `:symbol` and `:keyword` are supported alongside
  the other scalar type keywords.
- Updating an existing field with a different type is rejected.
- `Cljml.Compiler.compile_chunk` supports incremental compilation by returning
  the next compiler state plus the OCaml emitted for the current source chunk
  after typechecking the accumulated Parsetree state.

See [docs/differences.md](docs/differences.md) for current differences from
JVM Clojure.

Run the tests:

```sh
dune test
```

Compile the example to OCaml:

```sh
dune exec bin/cljml_cli.exe -- examples/person.cljml -o /tmp/person.ml
ocamlopt -I _build/default/vendor/rrbvec \
  -I _build/default/vendor/rrbvec/.rrbvec.objs/byte \
  -I _build/default/src \
  -I _build/default/src/.cljml.objs/byte \
  -I _build/default/src/.cljml.objs/native \
  -o /tmp/person \
  _build/default/vendor/rrbvec/rrbvec.cmxa \
  _build/default/src/cljml.cmxa \
  /tmp/person.ml
```

Run the example:

```sh
dune exec bin/cljml_cli.exe -- --run examples/person.cljml
```

Compile or run several cljml files in one incremental compiler state:

```sh
dune exec bin/cljml_cli.exe -- \
  --compile-files src/math.cljml src/main.cljml -o app.ml
dune exec bin/cljml_cli.exe -- \
  --run-files src/math.cljml src/main.cljml
```

Files are processed in the supplied order. Modules, types,
protocols, inferred OCaml signatures, and package dependencies remain available
to later files. Package dependencies are unioned for native linking, while
errors retain the path and line of the owning input file.

[`examples/multi_file/dune`](examples/multi_file/dune) is an executable Dune
integration: a rule treats `.cljml` files as dependencies, generates `app.ml`,
and compiles it with an ordinary executable stanza.

Start compiler-backed editor diagnostics through the standard Language Server
Protocol:

```sh
dune exec bin/cljml_cli.exe -- --lsp
```

The server supports full document synchronization; publishes errors plus OCaml
warnings such as non-exhaustive and redundant matches; and provides
Typedtree-backed hover types, jump-to-definition, type-detailed completion, and
identity-aware references, rename, highlights, document/workspace symbols, and
comment-preserving document formatting. Workspace indexing includes unopened
`.cljml` files and supports cross-file definitions, references, and rename. It
tracks module and top-level symbol dependency components, reanalyzes only the
component affected by an edit, reuses unrelated Typedtree analyses, and keeps
unaffected files available while another component contains an error.
See
[docs/editor-tooling.md](docs/editor-tooling.md) for Neovim and Emacs setup.
