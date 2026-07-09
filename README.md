# cljml

`cljml` is a small prototype for a statically typed Lisp in the Clojure family.
The current backend emits OCaml, so generated programs can be checked by the
OCaml compiler and can later interoperate with OCaml packages.

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
- `:admin?` is a distinct `keyword` value in the static type system.
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
- Clojure symbols and keywords that collide with OCaml reserved words are
  munged when emitted as OCaml identifiers.
- `(defn inc1 [x] (+ x 1))`, `(fn [x] ...)`, and `(let [...] ...)` are
  supported for typed function workflows.
- Function parameter types are inferred from body constraints where possible.
  Keyword lookup constraints such as `(:age person)` can infer required
  structural map fields when the field value type is known from context.
  Optional `^:int`, `^:string`, `^:keyword`, `^:bool`, or `^:nil` annotations
  can make a parameter type explicit.
- `do`, `fn`, `defn`, and `let` bodies can contain multiple forms; earlier
  forms are evaluated for effects and the final form supplies the value.
- `if-not`, `when`, and `cond` are compiler-recognized conditional forms.
  `cond` requires an `:else` branch in the current static subset.
- Top-level expression forms are evaluated with `let _ = ...`, so side-effect
  forms such as `(when flag (println "ready"))` can appear at file scope.
- `print` writes without a trailing newline; `println` writes with a trailing
  newline.
- `int?`, `integer?`, `number?`, `nat-int?`, `pos-int?`, `neg-int?`,
  `string?`, `keyword?`, `boolean?`, `vector?`, `list?`, `seq?`, `set?`,
  `map?`, `fn?`, `coll?`, `associative?`, `indexed?`, `seqable?`, and
  `counted?` are supported as static type predicates.
- `boolean`, `name`, and `keyword` are supported for the current scalar subset.
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
- `list`, `list-of`, `cons`, `second`, `last`, `peek`, `pop`, `map`,
  `filter`, `remove`, `take-while`, `drop-while`, `distinct`, `dedupe`,
  `sort`, `concat`, `vec`, `set`, `repeat`, `repeatedly`, `interpose`,
  `interleave`, `partition`, `partition-all`, `reductions`, `map-indexed`,
  `filterv`, `mapv`, `reduce`, `reduce-kv`, `apply`, `comp`, `partial`,
  `identity`, and `constantly` are supported for the current typed
  collection/function subset.
- `apply` supports integer binary reducers over typed lists, vectors, and sets.
- `get` supports vector indexes, and `nth` supports typed default values for
  lists and vectors.
- `rest` returns a same-typed empty list or vector when called on an empty
  list or vector.
- `empty` returns a same-typed empty list, vector, set, or string.
- `into` transfers elements between typed list, vector, and set collections.
- `take` and `drop` return same-typed list or vector slices.
- `reverse` returns a same-typed reversed list or vector.
- `every?`, `not-any?`, and `not-every?` work on typed lists, vectors, and
  sets.
- `hash-set`, `set-of`, `conj`, `disj`, and `contains?` are supported for
  homogeneous sets, and `map`, `filter`, and `reduce` work over sets.
- `disj` accepts zero or more same-typed values after the set.
- `(:require [some.ns :as alias])` can alias previously compiled namespaces.
- `(:require [some.ns :refer [user]])` can refer previously compiled namespace
  bindings into the current namespace.
- `(:require [ocaml.String :as string])` can alias a small typed table of OCaml
  host functions.
- `defprotocol` and `extend-type` support a first static protocol subset.
  Dispatch is resolved at compile time from the first argument type, and
  namespace aliases such as `labels/label` work when the protocol namespace has
  been compiled and required.
- `(:name user)` works as keyword lookup syntax for structural maps.
- `(keys user)` returns a persistent vector of keyword values, and `(vals user)`
  returns a persistent vector when all map values have the same type.
- `(vector-of :int)`, `(list-of :int)`, and `(set-of :int)` create explicitly
  typed empty collections; `:keyword` is supported alongside the other scalar
  type keywords.
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
  -o /tmp/person _build/default/vendor/rrbvec/rrbvec.cmxa /tmp/person.ml
```

Run the example:

```sh
dune exec bin/cljml_cli.exe -- --run examples/person.cljml
```
