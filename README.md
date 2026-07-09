# cljml

`cljml` is a small prototype for a statically typed Lisp in the Clojure family.
The current backend emits OCaml, so generated programs can be checked by the
OCaml compiler and can later interoperate with OCaml packages.

The compiler pipeline is intentionally split into cljml syntax and typing
first, then OCaml lowering:

- cljml has its own Lisp AST and typed IR for static Clojure-like semantics.
- Typed expressions carry an `Ocaml_ir` node shared by the source and
  Parsetree backends. Scalar literals, identifiers, list/vector construction,
  applications, and conditionals are structured; `Raw` marks expression
  categories that still need migration.
- The OCaml source backend remains the stable output path.
- `Cljml.Compiler.compile_parsetree` lowers compiled items independently into
  `Parsetree.structure`. Structural records, ordinary top-level values,
  effects, row type definitions, functions, protocol implementations, and
  nested modules are constructed directly. Typed expression payloads are the
  remaining source-backed backend boundary.
- `Cljml.Compiler.compile_chunk_parsetree` uses the same incremental compiler
  state as `compile_chunk`, but returns an OCaml structure for the current
  chunk.
- Parsetree is not used as cljml's full type system, and this direction does
  not introduce nilable sequences or lazy seqs.

Supported prototype forms:

```clojure
(ns examples.person
  (:require [ocaml.String :as string]))
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
- `(ns examples.person)` scopes unqualified symbols and avoids generated OCaml
  name collisions.
- `(module Math (defn add2 [x] (+ x 2)))` emits an OCaml module, and
  `Math/add2` resolves to that module binding. The current subset supports
  `def`, `defn`, and nested `module` forms inside a module.
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
  Row-shaped structural map parameters can accept wider maps with the required
  fields; the compiler projects wide records to generated narrow row records
  before calling the OCaml function.
  Optional `^:int`, `^:string`, `^:symbol`, `^:keyword`, `^:bool`, or `^:nil`
  annotations can make a parameter type explicit.
- `do`, `fn`, `defn`, and `let` bodies can contain multiple forms; earlier
  forms are evaluated for effects and the final form supplies the value.
- `if-not`, `when`, and `cond` are compiler-recognized conditional forms.
  `cond` requires an `:else` branch in the current static subset.
- `match` is a compiler-recognized static pattern form. It supports scalar
  literal patterns, `_`, symbol binders, and fixed-length list/vector patterns
  such as `[]`, `[x]`, and `[x y]`.
- Top-level expression forms are evaluated with `let _ = ...`, so side-effect
  forms such as `(when flag (println "ready"))` can appear at file scope.
- `print` writes without a trailing newline; `println` writes with a trailing
  newline.
- `not` follows Clojure truthiness for the statically represented values:
  only `false` and `nil` are falsey; other values are truthy.
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
- `range` produces an eager typed integer list.
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
- `apply` supports integer binary reducers over typed lists, vectors, and sets,
  including fixed leading integer arguments before the final collection.
- `get` supports vector indexes, and `nth` supports typed default values for
  lists and vectors.
- `first`, `second`, and `last` work on typed lists, vectors, and sets.
- `rest` preserves the concrete list, vector, or set type and is empty-safe.
- `empty` returns a same-typed empty list, vector, set, or string.
- `into` transfers elements between typed list, vector, and set collections.
- `take` and `drop` return same-typed list or vector slices.
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
- `(:require [some.ns :as alias])` can alias previously compiled namespaces.
- `(:require [some.ns :refer [user]])` can refer previously compiled namespace
  bindings into the current namespace.
- `(:require [ocaml.String :as string])` can alias a small typed table of OCaml
  host functions.
- `(:require [clojure.string :as str])` can alias a typed `clojure.string`
  subset: `blank?`, `capitalize`, `ends-with?`, `includes?`, `index-of`,
  `join`, `last-index-of`, `lower-case`, `re-quote-replacement`, `replace`,
  `replace-first`, `reverse`, `split`, `split-lines`, `starts-with?`, `trim`,
  `trim-newline`, `triml`, `trimr`, and `upper-case`.
- `defprotocol` and `extend-type` support a first static protocol subset.
  Dispatch is resolved at compile time from the first argument type, and
  namespace aliases such as `labels/label` work when the protocol namespace has
  been compiled and required.
- `(:name user)` works as keyword lookup syntax for structural maps.
- `(keys user)` returns a persistent vector of keyword values, and `(vals user)`
  returns a persistent vector when all map values have the same type.
- `(vector-of :int)`, `(list-of :int)`, and `(set-of :int)` create explicitly
  typed empty collections; `:symbol` and `:keyword` are supported alongside
  the other scalar type keywords.
- Updating an existing field with a different type is rejected.
- `Cljml.Compiler.compile_chunk` supports incremental compilation by returning
  the next compiler state plus the OCaml emitted for the current source chunk.

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
