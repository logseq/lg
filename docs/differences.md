# Differences From Clojure

cljml is a Clojure-syntax language that targets OCaml.

The goal is API familiarity, not JVM Clojure runtime identity.

## Static typing

cljml type checks programs before emitting OCaml.

Vectors are homogeneous.

Maps are structural records when created from map literals.

`hash-map` creates structural records from keyword/value pairs.

`assoc` can add one or more fields to a structural map, but it cannot change the type of an existing field.

`assoc` can update one or more persistent vector indexes when the replacement values match the element type.

`dissoc` can remove one or more known fields from a structural map.

`get` on a structural map requires a literal keyword that exists in the map type.

Three-argument `get` can return a default for an absent literal key; when the key is present, the default must match the field type.

`update` can pass extra arguments after the update function, but the function must return the existing field or vector element type.

Keyword call syntax such as `(:name user)` is supported for structural maps.

`if` branches must have the same type.

Function parameter types are inferred from body constraints where possible; annotations such as `^:int` and `^:string` are optional explicit hints.

Keyword lookup in typed contexts can infer structural map field requirements for unannotated function parameters.

Function calls can pass wider structural maps when the callee only requires a known subset of fields.

`do`, `fn`, `defn`, and `let` bodies evaluate forms in order and return the final form's type.

`print` and `println` follow Clojure's newline behavior.

Type predicates such as `int?`, `string?`, `keyword?`, `boolean?`, `vector?`,
`list?`, `set?`, and `map?` are resolved from static cljml types.

Arithmetic is currently integer-only. `+` and `*` support Clojure identity arities, comparisons can be chained, and `/` requires at least two integer arguments because cljml does not yet have ratios.

## Macros

Macros are not supported.

Reader macro support is intentionally minimal while the typed core is being built.

## Namespaces

`ns` switches the current namespace.

Unqualified symbols resolve in the current namespace.

Qualified symbols can reference previously compiled namespaces.

`(:require [some.ns :as alias])` aliases previously compiled namespace bindings.

`(:require [some.ns :refer [name]])` refers previously compiled namespace bindings into the current namespace.

Incremental compilation preserves namespace, alias, refer, type counter, and binding state across source chunks.

## Collections

Vectors compile to `Rrbvec.t` persistent vectors.

Lists compile to OCaml lists and support `list`, `list-of`, `cons`, `conj`, `first`, `second`, `last`, `peek`, `pop`, `rest`, `nth`, `count`, `map`, `filter`, and `reduce`.

Vectors support `first`, `second`, `last`, `peek`, `pop`, `rest`, `nth`, `get`, `assoc`, `update`, `contains?`, `subvec`, and the current eager sequence operations.

Three-argument `nth` returns a typed default for out-of-range list and vector indexes.

`subvec` returns an `Rrbvec.t` persistent vector slice.

`empty` returns a same-typed empty list, vector, set, or string.

`into` transfers elements between typed list, vector, and set collections when
the element types match.

Empty vector literals still require explicit element typing.

Use `(vector-of :int)`, `(vector-of :string)`, `(vector-of :bool)`, or `(vector-of :nil)` for typed empty vectors.

Use `(list-of :int)`, `(list-of :string)`, `(list-of :bool)`, or `(list-of :nil)` for typed empty lists.

Sets currently compile to sorted unique OCaml lists.

Keywords are statically distinct from strings, although the current runtime
representation is still an OCaml string.

Maps currently compile to OCaml records when their keys are known statically.

Structural map helpers include `merge`, `update`, and `select-keys`. Overlapping fields in `merge` and updated fields in `update` must keep their existing static type.

Sequence APIs are eager.

## Host interop

Direct OCaml package interop is limited to an explicit typed host table.

Currently supported examples include `ocaml.Stdlib` aliases or refers for `string-of-int` and `int-of-string`, and `ocaml.String` aliases or refers for `uppercase-ascii` and `length`.

The planned direction follows ClojureDart's approach of making host package aliases explicit in `ns`.
