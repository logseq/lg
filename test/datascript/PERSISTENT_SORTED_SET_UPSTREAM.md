# Persistent sorted set upstream alignment

The comparison baseline is the local `logseq/fork` checkout at
`/Users/tiensonqin/Codes/projects/persistent-sorted-set`, revision
`efc3add9af192abc64711bb58c3d1d2352097129`. Its source and test files are
clean; the checkout only has a local `project.clj` version change and tool
caches.

Run the reproducible name-discovery report with:

```sh
bb script/check_persistent_sorted_set_upstream.clj \
  /Users/tiensonqin/Codes/projects/persistent-sorted-set \
  /Users/tiensonqin/Codes/projects/lg
```

Definition-name alignment is a discovery aid, not the implementation score.
The reviewed implementation score counts a differently named LG definition
only when it retains the upstream responsibility with a closed static type.

| Target | Exact definitions | Reviewed typed equivalents | Missing behavior | Real implementation match |
| --- | ---: | ---: | ---: | ---: |
| Native | 28 | 6 | 1 | 34/35 (97.1%) |
| Melange | 80 | 24 | 1 | 104/105 (99.0%) |

The remaining behavior on both targets is `sorted-set*`, including its open
metadata/options map. The default `restore` constructor now retains lazy root
loading and derives unknown tree height/count only when those values are first
needed. LG also keeps the explicit, statically typed `restore-by`,
`sorted-set-with-comparator`, `with-ref-type`, and `with-branching-factor`
boundaries.

Native typed equivalents:

| Upstream definition | LG implementation evidence |
| --- | --- |
| `array-from-indexed` | `partition-indexed-array` copies indexed values into typed arrays. |
| `array-type` | OCaml arrays retain their element type without a runtime array-class token. |
| `if-cljs` | Reader features select Native/Melange code at compile time. |
| `map->settings` | The closed `set-settings` record validates branching factor and `Strong | Weak`. |
| `settings->map` | `settings` returns the same statically typed settings domain. |
| `split` | `arr-partition-approx` preserves the upstream min/average/max split control flow. |

Melange typed equivalents:

| Upstream definitions | LG implementation evidence |
| --- | --- |
| `BTSet`, `Leaf`, `Node` | Closed `btset` and `tree` records model the same state without runtime class dispatch. |
| `Iter`, `ReverseIter`, `Chunk`, `IIter`, `ISeek` | The typed `iterator` record and runtime chunked sequence preserve forward/reverse iteration and seek behavior. |
| `INode`, `IRoot`, `IStore` | Closed record operations replace JS protocols while retaining node, root, and storage responsibilities. |
| `array-type`, `if-cljs` | Static OCaml arrays and reader features replace JS runtime dispatch. |
| `child` | `node-child` performs the same lazy child lookup; the shorter global name conflicts with LG lexical locals. |
| `ensure-addresses!`, `node-addresses->array` | `children-addresses`, constructors, and typed address arrays keep addresses initialized. |
| `indexed` | Explicit indexed loops preserve order without allocating `[index value]` pairs. |
| `make-reference`, `read-reference` | `weak-ref` and `weak-deref` are the documented host boundary. |
| `return-array` | `rotate` constructs closed `tree array` results directly instead of filtering nullable JS arguments. |
| `set-address!`, `set-child!` | Typed array updates in storage and mutation paths preserve the same writes. |
| `uninitialized-address`, `uninitialized-hash` | Closed `option` fields represent uninitialized state. |

Upstream test-name alignment is 15/15 (100%) on Native and 11/11 (100%) on
Melange. The named compatibility tests execute real set, slice, reverse slice,
seek, reduction, overflow, storage deletion, stable-address, walk, and lazy
restore behavior in `leaf_test.cljc` and `storage_test.cljc`; they are not empty
name stubs. The Native `iter-over-transient` case tests iterator stability
across a persistent update because LG does not expose JVM mutable transient
identity.

The PSS types and signatures live in
`datascript/me/tonsky/persistent_sorted_set.mil`. The implementation audit
rejects type declarations or signatures left in the `.cljc` file and verifies
that every compile manifest loads the interface before the implementation.
