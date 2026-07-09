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

Function parameter types are inferred from body constraints where possible; annotations such as `^:int`, `^:string`, and `^:keyword` are optional explicit hints.

Keyword lookup in typed contexts can infer structural map field requirements for unannotated function parameters.

Function calls can pass wider structural maps when the callee only requires a known subset of fields.

`let`, `fn`, and `defn` support a static destructuring subset inspired by
Clojure destructuring. Associative destructuring works on structural maps with
`:keys`, direct `{local :keyword}` bindings, scalar literal `:or` defaults, and
`:as`. Sequential destructuring works on typed vectors and lists with fixed
positional bindings, `& rest`, and `:as`. Keyword argument destructuring,
non-literal default expressions, and nil-padding are not supported yet because
the current OCaml runtime representation has no nilable collection element
type.

Row polymorphism is represented in cljml's static type compatibility: a
function parameter inferred as a structural map with fields `:name` and `:age`
can be called with a wider map that also has other fields. For generated OCaml,
cljml emits a narrow record type for row-shaped function parameters and
projects wider map records into that narrow row at the call site. This keeps
the runtime representation as OCaml records while allowing one function to
accept different structural map shapes that share the required fields.

Protocols are a static subset of Clojure protocols. `defprotocol` records typed method signatures, and `extend-type` emits ordinary OCaml functions for supported receiver types. Calls dispatch at compile time from the first argument type, so there is no runtime protocol table, dynamic extension, metadata dispatch, or reflection.

`do`, `fn`, `defn`, and `let` bodies evaluate forms in order and return the final form's type.

`if-not`, `when`, and `cond` are compiler-recognized forms rather than macros. `when` currently supports unit or nil bodies, and `cond` requires an `:else` branch because cljml does not yet have a union type for implicit nil results.

`match` is a compiler-recognized static pattern form rather than a macro. The
current subset supports scalar literal patterns, `_`, symbol binders, and
fixed-length list/vector patterns written with vector pattern syntax. Pattern
literals can constrain unannotated function parameters. There is no
exhaustiveness checker yet.

Top-level expression forms are supported and emit `let _ = ...`; top-level map literals still need a `def` because structural maps require generated record definitions.

`print` and `println` follow Clojure's newline behavior.

Type predicates such as `int?`, `integer?`, `number?`, `nat-int?`, `pos-int?`,
`neg-int?`, `string?`, `keyword?`, `boolean?`, `vector?`, `list?`, `seq?`,
`set?`, `map?`, `fn?`, `coll?`, `associative?`, `indexed?`, `seqable?`, and
`counted?` are resolved from static cljml types or direct OCaml integer checks.
`any?`, `rational?`, `ratio?`, `float?`, `double?`, `decimal?`,
`simple-keyword?`, `qualified-keyword?`, `symbol?`, `simple-symbol?`,
`qualified-symbol?`, `ident?`, `simple-ident?`, `qualified-ident?`,
`sequential?`, `reversible?`, and `sorted?` are also static predicates in the
current subset. Numeric tower predicates reflect the integer-only runtime, so
ratio and floating predicates currently return false.

`subs` supports two- and three-argument typed string slicing.

Arithmetic is currently integer-only. `+` and `*` support Clojure identity arities, ordered comparisons can be chained, same-typed `=` and `not=` are supported, same-shaped structural maps compare field by field, and `/` requires at least two integer arguments because cljml does not yet have ratios. Integer helpers include `zero?`, `pos?`, `neg?`, `even?`, `odd?`, `max`, `min`, `quot`, `rem`, `mod`, `bit-and`, `bit-or`, `bit-xor`, `bit-not`, `bit-set`, `bit-clear`, `bit-flip`, `bit-test`, `bit-shift-left`, `bit-shift-right`, and `bit-shift-right-zero-fill`. Unchecked integer functions map directly to OCaml integer operators, so their exact overflow behavior follows the OCaml target.

`boolean`, `name`, `namespace`, `keyword`, and `symbol` are supported for the
current scalar subset. `name` works on strings, keywords, and symbols.
`namespace` works on keywords and symbols, but returns `""` for unqualified
identifiers because cljml does not yet have a typed optional string or nilable
string. `keyword` and `symbol` work on strings, keywords, and symbols.

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

Protocol signatures and implementations are preserved in the same incremental compiler state. Namespace aliases can qualify protocol method calls, for example `labels/label`, after the protocol namespace has been compiled and required.

`module` emits an OCaml module and registers bindings for qualified calls such
as `Math/add2`. The current static subset allows `def`, `defn`, and nested
`module` forms inside a module. Module signatures, functors, `open`, and module
aliases are not supported yet.

Clojure symbols and keywords that would emit OCaml reserved words are munged as
legal OCaml identifiers while preserving source-level names.

## Collections

Vectors compile to `Rrbvec.t` persistent vectors.

Lists compile to OCaml lists and support `list`, `list*`, `list-of`, `cons`, `conj`, `first`, `second`, `last`, `peek`, `pop`, `rest`, `next`, `nthnext`, `nthrest`, `ffirst`, `fnext`, `nfirst`, `nnext`, `rseq`, `nth`, `count`, `map`, `filter`, `remove`, `some`, `take-while`, `drop-while`, `distinct`, `dedupe`, `sort`, `sort-by`, `concat`, `mapcat`, `vec`, `set`, `repeat`, `repeatedly`, `interpose`, `interleave`, `partition`, `partition-all`, `reductions`, `map-indexed`, `filterv`, `mapv`, `reduce`, `reduce-kv`, `butlast`, `take-last`, `drop-last`, `take-nth`, `split-at`, `split-with`, `partition-by`, `bounded-count`, `dorun`, `doall`, and `run!` where the static element types line up.

Vectors support `first`, `second`, `last`, `peek`, `pop`, `rest`, `next`, `nthnext`, `nthrest`, `ffirst`, `fnext`, `nfirst`, `nnext`, `rseq`, `nth`, `get`, `assoc`, `update`, `contains?`, `subvec`, and the current eager sequence operations.

`rest` and `next` return a same-typed empty list or vector when called on an
empty list or vector because cljml does not yet have a nilable sequence result
type.

Three-argument `nth` returns a typed default for out-of-range list and vector indexes.

`subvec` returns an `Rrbvec.t` persistent vector slice.

`empty` returns a same-typed empty list, vector, set, or string.

`into` transfers elements between typed list, vector, and set collections when
the element types match.

`take` and `drop` return same-typed list or vector slices.

`reverse` returns a same-typed reversed list or vector.

`every?`, `not-any?`, `not-every?`, and the current static subset of `some`
return typed booleans for list, vector, and set predicates.

Sequence operations are eager and return concrete typed collections. For example
`mapv` and `filterv` return persistent vectors, `concat`, `sort`,
`interpose`, `interleave`, `partition`, `partition-all`, `reductions`, and
`map-indexed` return OCaml lists, `split-at` and `split-with` return persistent
vectors of the input collection representation, and same-shape operations such
as `remove`, `take-while`, `drop-while`, `distinct`, `dedupe`, `butlast`,
`take-last`, `drop-last`, `take-nth`, `nthnext`, `nthrest`, and `rseq` preserve
the input collection representation where practical. `dorun` and `doall` are
eager because cljml does not yet have lazy seqs.

`range` returns an eager typed integer list.

Empty vector literals still require explicit element typing.

Use `(vector-of :int)`, `(vector-of :string)`, `(vector-of :symbol)`, `(vector-of :keyword)`, `(vector-of :bool)`, or `(vector-of :nil)` for typed empty vectors.

Use `(list-of :int)`, `(list-of :string)`, `(list-of :symbol)`, `(list-of :keyword)`, `(list-of :bool)`, or `(list-of :nil)` for typed empty lists.

Use `(set-of :int)`, `(set-of :string)`, `(set-of :symbol)`, `(set-of :keyword)`, `(set-of :bool)`, or `(set-of :nil)` for typed empty sets.

Sets currently compile to sorted unique OCaml lists and support `hash-set`,
`sorted-set`, `set-of`, `conj`, `disj`, `contains?`, `every?`, `not-any?`, `not-every?`,
`map`, `filter`, and `reduce`.

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

Sequence APIs are eager.

## Host interop

Direct OCaml package interop is limited to an explicit typed host table.

Currently supported examples include `ocaml.Stdlib` aliases or refers for `string-of-int` and `int-of-string`, and `ocaml.String` aliases or refers for `uppercase-ascii` and `length`.

The typed standard library also includes a `clojure.string` namespace that can
be required with `:as` or `:refer`. Its current subset includes `blank?`,
`capitalize`, `ends-with?`, `includes?`, `index-of`, `join`, `last-index-of`,
`lower-case`, `re-quote-replacement`, `replace`, `replace-first`, `reverse`,
`split`, `split-lines`, `starts-with?`, `trim`, `trim-newline`, `triml`,
`trimr`, and `upper-case`. `replace` and `split` currently use literal string
matches, not regex patterns.

The planned direction follows ClojureDart's approach of making host package aliases explicit in `ns`.
