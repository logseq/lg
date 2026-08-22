# ADR 007: Stable Compiler Diagnostic Identity

Status: Accepted

## Context

Compiler errors and OCaml warnings crossed the library, CLI, and LSP boundaries
with a message and optional location. Integrations had to parse human-readable
English text to classify failures, and LSP clients could not retain a stable
identity across wording improvements.

## Decision

Every public compiler error has:

- a stable string code;
- a closed compiler phase;
- a human-readable message; and
- an optional source location.

The phases are lexing, parsing, semantic analysis, lowering, OCaml checking,
and infrastructure. Existing `message` and `location` fields remain available.

The initial code families are:

- `LG1001` for lexical errors;
- `LG1002` for parse errors;
- `LG2000` for semantic errors not yet assigned a narrower code;
- `LG4000` for OCaml typecheck failures;
- `LG9000` for compiler artifact and infrastructure failures; and
- `OCAML-WARNING` for warnings reported by compiler-libs.

New diagnostics must use a stable code based on the semantic failure, not the
exact message. Narrower codes may replace `LG2000` without changing the phase.

The LSP publishes the code in the standard diagnostic `code` field and the
phase in `data.phase`. The CLI preserves its existing readable prefix and adds
the code as a suffix. The `Compiler` module has an explicit interface so new
implementation helpers do not silently become part of that facade.

## Validation

Library tests assert lexical/parsing and semantic identities. Warning tests
assert the OCaml warning identity. CLI/LSP smoke tests assert both error and
warning codes and phases on the wire.
