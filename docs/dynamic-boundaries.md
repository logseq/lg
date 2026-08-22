# Runtime Dynamic Boundaries

LG does not have a source-visible dynamic type. `Runtime_dynamic.t` is limited
to compatibility boundaries whose upstream domain is intentionally open. The
test suite freezes the current file inventory so a new use requires an explicit
design review.

## Accepted runtime boundaries

- `Runtime_tap` carries arbitrary values to process-wide tap callbacks. The
  compiler validates and converts each supported static value at
  `Tap_dynamic_boundary`.
- `Runtime_multimethod` stores open dispatch values because Clojure multimethod
  hierarchies can dispatch on unrelated scalar and composite values.
- `Runtime_test_report` transports arbitrary `is` expression values to the test
  reporting compatibility API.
- `Runtime_map` and the dynamic-key functions in `Runtime_transient` implement
  explicitly heterogeneous map-key operations. Homogeneous maps retain their
  concrete key and value types and do not use this configuration.

These are boundary fields or functions, not permission to make an enclosing
collection, compiler state, or public source type dynamic.

## Compiler references

Compiler references fall into three roles:

- recognizing the closed internal constraint that marks one of the accepted
  open boundaries;
- emitting a validated conversion at that individual boundary; or
- lowering the already selected boundary conversion to OCaml.

They do not define `to_dynamic`, `of_dynamic`, `__lg_dynamic`, or implicit
host-boundary assignability. New compiler files may not reference
`Runtime_dynamic` until the concrete upstream behavior and the insufficiency of
a closed variant are documented here.

## Removed boundary

The old `Core_set.Dynamic_vector_set` and
`Core_set.Dynamic_vector_vector_set` implementations were not valid open
boundaries. They allowed an impossible source dynamic element type to select a
runtime comparator. LG now rejects that set element type during compilation
instead of materializing a dynamic collection.
