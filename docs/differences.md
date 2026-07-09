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

`if` branches must have the same type.

## Macros

Macros are not supported.

Reader macro support is intentionally minimal while the typed core is being built.

## Namespaces

`ns` switches the current namespace.

Unqualified symbols resolve in the current namespace.

Qualified symbols can reference previously compiled namespaces.

`(:require [some.ns :as alias])` aliases previously compiled namespace bindings.

## Collections

Vectors compile to `Rrbvec.t` persistent vectors.

Sets currently compile to sorted unique OCaml lists.

Maps currently compile to OCaml records when their keys are known statically.

Sequence APIs are eager.

## Host interop

Direct OCaml package interop is not implemented yet.

The planned direction follows ClojureDart's approach of making host package aliases explicit in `ns`.
