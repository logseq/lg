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
- `(assoc x :admin? true)` produces a new record shape with an added `bool`
  field.
- `(dissoc y :age)` produces a new record shape with that field removed.
- `merge`, `update`, and `select-keys` work on structural maps when field
  keys are known statically.
- `(ns examples.person)` scopes unqualified symbols and avoids generated OCaml
  name collisions.
- `(defn inc1 [x] (+ x 1))`, `(fn [x] ...)`, and `(let [...] ...)` are
  supported for typed function workflows.
- Function parameter types are inferred from body constraints where possible.
  Keyword lookup constraints such as `(:age person)` can infer required
  structural map fields when the field value type is known from context.
  Optional `^:int`, `^:string`, `^:bool`, or `^:nil` annotations can make a
  parameter type explicit.
- `do`, `fn`, `defn`, and `let` bodies can contain multiple forms; earlier
  forms are evaluated for effects and the final form supplies the value.
- `print` writes without a trailing newline; `println` writes with a trailing
  newline.
- `+`, `*`, `-`, `/`, and chained comparisons follow Clojure-style arities
  where the current integer-only type system can represent them.
- `list`, `list-of`, `cons`, `second`, `last`, `peek`, `pop`, `map`,
  `filter`, `reduce`, `apply`, `comp`, `partial`, `identity`, and
  `constantly` are supported for the current typed collection/function subset.
- `hash-set`, `disj`, and `contains?` are supported for homogeneous sets.
- `(:require [some.ns :as alias])` can alias previously compiled namespaces.
- `(:require [some.ns :refer [user]])` can refer previously compiled namespace
  bindings into the current namespace.
- `(:require [ocaml.String :as string])` can alias a small typed table of OCaml
  host functions.
- `(:name user)` works as keyword lookup syntax for structural maps.
- `(keys user)` returns a persistent vector of keyword values.
- `(vector-of :int)` creates an explicitly typed empty persistent vector.
- `(list-of :int)` creates an explicitly typed empty list.
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
