# Differences From Clojure

cljml is a Clojure-syntax language that targets OCaml.

The goal is API familiarity, not JVM Clojure runtime identity.

## Static typing

cljml type checks programs before emitting OCaml.

Vectors are homogeneous.

Maps are structural records when created from map literals.

`assoc` can add a field to a structural map, but it cannot change the type of an existing field.

`dissoc` can remove a known field from a structural map.

`get` on a structural map requires a literal keyword that exists in the map type.

Keyword call syntax such as `(:name user)` is supported for structural maps.

`if` branches must have the same type.

Function parameter types are inferred from body constraints where possible; annotations such as `^:int` and `^:string` are optional explicit hints.

`do`, `fn`, `defn`, and `let` bodies evaluate forms in order and return the final form's type.

`print` and `println` follow Clojure's newline behavior.

Arithmetic is currently integer-only. `+` and `*` support Clojure identity arities, comparisons can be chained, and `/` requires at least two integer arguments because cljml does not yet have ratios.

## Macros

Macros are not supported.

Reader macro support is intentionally minimal while the typed core is being built.

## Namespaces

`ns` switches the current namespace.

Unqualified symbols resolve in the current namespace.

Qualified symbols can reference previously compiled namespaces.

`(:require [some.ns :as alias])` aliases previously compiled namespace bindings.

Incremental compilation preserves namespace, alias, type counter, and binding state across source chunks.

## Collections

Vectors compile to `Rrbvec.t` persistent vectors.

Lists compile to OCaml lists and support `list`, `list-of`, `cons`, `conj`, `first`, `second`, `last`, `peek`, `pop`, `rest`, `nth`, `count`, `map`, `filter`, and `reduce`.

Vectors support `first`, `second`, `last`, `peek`, `pop`, `rest`, `nth`, and the current eager sequence operations.

Empty vector literals still require explicit element typing.

Use `(vector-of :int)`, `(vector-of :string)`, `(vector-of :bool)`, or `(vector-of :nil)` for typed empty vectors.

Use `(list-of :int)`, `(list-of :string)`, `(list-of :bool)`, or `(list-of :nil)` for typed empty lists.

Sets currently compile to sorted unique OCaml lists.

Maps currently compile to OCaml records when their keys are known statically.

Sequence APIs are eager.

## Host interop

Direct OCaml package interop is limited to an explicit typed host table.

Currently supported examples include `ocaml.Stdlib` aliases for `string-of-int` and `int-of-string`, and `ocaml.String` aliases for `uppercase-ascii` and `length`.

The planned direction follows ClojureDart's approach of making host package aliases explicit in `ns`.
