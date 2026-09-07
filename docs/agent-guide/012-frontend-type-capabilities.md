# Frontend type capabilities

## Goal

Expose the established OCaml/Melange static language capabilities identified in
the frontend review without universal runtime values or unsafe casts. Preserve
existing Clojure evaluation and control flow. `docs/design.md` remains the
authoritative contract.

## Delivery order and acceptance

1. **Inference and recursive groups** (implemented): support mutually recursive
   local functions, dependency-ordered generalization, unannotated module
   recursion, and constraint propagation without silently accepting a partial
   result after a fixed number of inference passes. Verify captures, shadowing,
   forward references, polymorphic reuse outside recursive groups, invalid
   recursive applications, and source diagnostics.
2. **Module interfaces** (implemented): retain inferred polymorphism and abstract
   type relationships across modules; exclude private values from exported
   interfaces; support signature type equalities and destructive substitution.
   Verify generated interfaces independently and reject abstraction leaks.
3. **First-class modules** (implemented): typed package values, packing, unpacking,
   and locally scoped abstract types. Verify runtime implementation selection
   and rejection of escaping abstract types. OCaml 5.5 module-dependent
   functions are outside this goal.
4. **Polymorphic fields** (implemented): explicit quantified record field types,
   rigid checking on construction, fresh instantiation on projection, and
   rejection of specialized implementations or escaping quantifiers.
5. **GADTs** (implemented): constructor result indices, locally abstract parameters,
   branch-local type refinement, and existential scope checks. Verify typed
   expression evaluation and rejection of inconsistent result indices.
6. **Polymorphic variants** (implemented): explicit static row types, construction,
   pattern matching, and row constraints. Verify composition and reject absent
   tags and incompatible payloads. They must not introduce implicit widening of
   existing closed heterogeneous domains.

For each bounded implementation step, write positive and negative regression
tests first, observe the expected failures, implement the behavior, verify it,
and refactor with the tests remaining green. Run the applicable Native and
Melange checks, then the full repository suite before final acceptance. Record
remaining limitations explicitly; partial phases do not complete the goal.

Native-only effects and Domains are outside this goal. New language syntax and
semantics will be documented in `docs/design.md` alongside each implementation.

## Verification

Focused regression tests cover all six phases on applicable Native and Melange
paths. The cross-runtime fixture exercises local recursion, module recursion,
packages, polymorphic fields, GADTs, and variant rows. Positive cases include
recursive indexed expression evaluation and existential payload/callback pairs;
negative cases reject index mismatches, specialized polymorphic fields, escaping
abstract types, and incompatible variant rows.

Integration regressions additionally cover:

- Long inference chains and dependency-ordered local recursive groups.
- Private values behind explicit module ascriptions, aliases, and functor results.
- Nested signature equalities and destructive substitutions.
- Functor argument type equalities retained in exported values and record fields.
- Printer aliases that preserve user modules across incremental compilation.
- Generated collection dependencies that appear only in GADT result indices.
- Serialized compiler-state version checks after metadata schema changes.

Final `opam exec --switch=5.5.0 -- dune runtest` passed on 2026-09-07,
including the subsequent module integration, printer fixes, collection result
indices, artifact version 22, and Native/Melange cross-runtime regressions.
`git diff --check` also passed. All six delivery phases are accepted within the
supported scope below.

## Supported scope

This delivers the frontend capabilities above; it does not claim complete OCaml
syntax or feature parity. Generic GADT elimination requires an explicit signature.
Record-field quantifiers cannot shadow the record's type parameters. Variant
syntax supports a constant tag or one payload (use a tuple for several values),
and exact, lower, and upper row annotations. Direct tag patterns infer rows;
more complex pattern combinations may require a parameter annotation. There is
no source syntax for naming a row tail or expressing OCaml payload intersections.
Unknown tags in an open row print as `<variant>` without inspecting their payload.
First-class module packages use named signatures; package-specific `with type`
syntax is not exposed (define a constrained signature first).

Effects, Domains, and OCaml 5.5 module-dependent functions remain excluded.
