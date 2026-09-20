# OCaml Interface Sidecars Implementation Plan

Goal: Let LG programs declare their static types and value contracts in ordinary OCaml `.mli` files instead of LG `.lgi` files.

Architecture: Parse interfaces with OCaml compiler-libs and associate each interface with the LG source having the same path stem.
Translate supported declarations into the existing static declaration and signature pipeline before checking the implementation.
Place interface-owned manifest types in an ordinary OCaml module per source unit so common type names cannot shadow another interface's types.
Preserve interface declarations in compiler state so incremental compilation and saved-state compilation enforce the same contracts.

Tech Stack: OCaml, compiler-libs, LG typed elaboration, Dune, shell integration tests.

Related: Builds on 004-compiler-semantic-pipeline.md and 005-compiler-state-artifacts.md.

## Problem statement

LG currently accepts `.lgi` contracts but parses every input as Clojure syntax.
An OCaml interface must supply constraints before LG infers function bodies, and its type variables must remain universally quantified.
Renaming files or checking only the final output would not provide this behavior.

## Testing Plan

Exercise the compiler API with a `.mli` chunk followed by its matching LG implementation.
Compile and execute scalar functions, polymorphic functions, callbacks, options, collections, records, aliases, and closed variants.
Check zero-argument functions, variadic functions, multiple arities, munged names, and namespaces.
Reject incompatible implementations, polymorphic specialization, missing interface values, unsupported interface constructs, ambiguous names, and duplicate contracts.
Check reversed workspace input order and multiple namespaces.
Exercise CLI directory discovery, explicitly selected files, saved-state restoration, and an invalid continuation that must not publish output.
Retain existing `.lgi` tests and run the compiler regression suite plus relevant CLI and architecture gates.

NOTE: I will write *all* tests before I add any implementation behavior.

## Tasks

1. Add compiler regression cases in `test/compiler_tests.ml` and CLI integration coverage in `test/mli_sidecar_cli_test.sh`, registered in `test/dune`.
2. Run `dune exec test/compiler_tests.exe -- --filter 'mli sidecar'` and verify failures identify missing OCaml interface support.
3. Add `src/ocaml_interface.ml` to parse OCaml signatures, resolve source names, and translate static declarations with original interface locations.
4. Update `src/toolchain.ml` to retain pending interfaces, apply the matching interface before source inference, and order workspace interfaces before implementations.
5. Update `bin/lg_cli.ml` to discover paired interfaces and include them in namespace dependency ordering without treating unrelated OCaml interfaces as LG sources.
6. Verify saved-state handling retains pending interfaces and invalid implementations fail before output is written.
   Update the artifact format version and its malformed-artifact tests when adding serializable interface metadata.
   Include adjacent-interface discovery in single-file compilation and interface inference.
   Link paired files in the language service dependency graph and verify editing an interface invalidates its implementation.
7. Update `docs/design.md`, `docs/stdlib.md`, and `README.md` with supported syntax, ownership, naming, and invocation examples.
8. Run focused regressions, CLI coverage, and broader compiler checks; record results here.

## Verification

Implementation and focused verification completed on 2026-09-20.

| Check | Result |
| --- | --- |
| `dune exec test/compiler_tests.exe -- --filter 'mli sidecar'` | Passed all 19 test groups, including Native execution and Melange compilation. |
| `dune exec test/compiler_tests.exe` | Passed the complete compiler regression suite. |
| `dune build @test/mli-sidecar` | Passed directory, explicit-file, single-file, inferred-interface, and saved-state CLI checks. |
| `test/cli_state_cache_test.sh` with current compiler and native stdlib artifacts | Passed, including oversized-state validation using the actual artifact version. |
| `test/language_service_navigation_tests.exe` with the native stdlib state | Passed. |
| `test/no_unsafe_dynamic_casts_test.sh` and `test/no_raw_ir_regression.sh` | Passed. |
| `git diff --check` | Passed. |
| LG `dune runtest` | Passed the full suite with the final compiler. |
| signal-lg standalone `dune runtest --root .` | Passed Native and Melange: 14 tests each; Melange reports 80 assertions. |
| LUI standalone `dune runtest --root .` | Passed Native and Melange: 201 tests each; Melange reports 1910 assertions. Hot reload p95: 239 ms against the 500 ms gate. |
| Chat `dune build @runtest core/logseq_chat_mobile_entry.ml` | Passed; 707 Chat tests and the mobile entry compile. |

The initial failures were reproduced against an isolated archive of HEAD `3995895c`.
They were subsequently fixed: public core spellings no longer bypass source expansion, swap inference expands the actual updater, the syntax inventory reflects existing forms, and compatibility assertions use supported exception patterns.
The compiler-state oversized-artifact check reads the actual artifact version.

Standalone consumer checks used the current LG executable and libraries from its Dune install tree; LUI also used the current signal-lg install tree.
The hot reload performance gate was rerun after concurrent compiler jobs completed.

The Chat migration replaces all four `.lgi` sidecars with `.mli`, removes five obsolete declarations, and removes 104 redundant implementation annotations.
Integration regressions cover macro-generated definitions, imported types, qualified interface types, record names and fields, `assoc`/`update`, and generic callbacks over differently instantiated nominal records.
Callback argument inference uses nominal type arguments without unifying unrelated declaration-local field variables; final argument checking retains the complete static contract.
Saved Melange state restoration explicitly resolves its runtime packages for installed consumers.

The implementation also fixes opened constructor spelling, rejects field annotations that conflict with sidecar contracts, and preserves nested interface declaration locations in OCaml diagnostics.

## Testing Details

Runtime assertions prove the declarations influence usable compiled programs.
Negative compilation cases prove contracts constrain implementations rather than merely being parsed or ignored.
CLI tests prove discovery, dependency ordering, and persisted contracts work through user-facing commands.

## Implementation Details

- `.mli` remains ordinary OCaml syntax; `.lgi` remains supported.
- Pair interfaces by source path stem rather than guessing namespaces from directories.
- Resolve OCaml identifiers against LG source names using the compiler's existing name sanitization and reject ambiguous matches.
- Define manifest aliases, records, and variants in the interface so authors can keep type definitions outside LG implementation files.
- Abstract declarations require a corresponding concrete LG type definition.
- Interpret function storage signatures using the implementation's declared arities, including unit arguments and variadic sequence storage.
- Reuse rigid LG signature enforcement and OCaml's final type check.
- Reject unsupported declarations and forbidden dynamic types with interface locations instead of weakening their meaning.
- Preserve pending interface declarations in serializable compiler state.
- Keep new metadata closed and statically typed.

## Question

An interface is a declaration source and a set of checked contracts, matching existing `.lgi` semantics; it does not hide undeclared LG namespace members.
OCaml features with no faithful LG declaration representation must produce an explicit diagnostic.
Supporting full OCaml module sealing would be a separate namespace/export design change.

---
