# ADR 010: Elm-Level Compiler Diagnostics and Trustworthy Source Mapping

Status: Accepted

## Context

LG diagnostics currently have stable codes and phases, but most failures are
still constructed as one English string plus one optional `Location.t`. This
makes several common errors expensive to diagnose:

- a delimiter error identifies an opening delimiter but not the conflicting
  closing delimiter or the parser expectation;
- a conditional type error lists two types without identifying the two source
  branches that produced them;
- errors involving generated or transformed forms can fall back to the whole
  enclosing form;
- the CLI cannot explain an error with an inline source snippet;
- the LSP derives quick fixes by matching human-readable error text.

The target quality bar is Elm: a diagnostic should identify the source
construct, explain the semantic relationship that failed, show the relevant
source, compare actual and expected values, and offer a specific next action
when one is known.

This is not primarily a terminal-color or prose project. Elm keeps source
regions on syntax, records contextual type expectations while generating
constraints, computes structured type differences, builds a report containing
a title, region, suggestions, and document, and only then renders that report
for a particular consumer. Relevant upstream references are:

- [`Reporting.Annotation`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Reporting/Annotation.hs)
  for regions attached to compiler values;
- [`Type.Error`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Type/Error.hs)
  for structural type comparison and classified differences;
- [`Reporting.Error.Type`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Reporting/Error/Type.hs)
  for context-specific explanations such as a particular call argument or
  conditional branch;
- [`Reporting.Report`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Reporting/Report.hs)
  and [`Reporting.Render.Code`](https://github.com/elm/compiler/blob/1bd5b36915a38335195ca7792fe3995f53d84d5e/compiler/src/Reporting/Render/Code.hs)
  for the separation between diagnostic data and source rendering.

The comparison was made against Elm compiler commit
`1bd5b36915a38335195ca7792fe3995f53d84d5e` and Elm CLI 0.19.2. Real CLI
output was also checked for a record-level type mismatch and two distinct
unfinished-parenthesis states.

## Decision

LG diagnostics are structured compiler output. Human-readable text is one
projection of that output and must not be used as an internal protocol.

### Diagnostic model

Every compile error has:

- a stable code and compiler phase, as established by ADR 007;
- a short, stable title suitable for a CLI header;
- an explanatory message;
- an optional primary source location;
- zero or more related source locations with labels; and
- zero or more actionable hints;
- zero or more closed, machine-applicable text edits; and
- optional structured type-mismatch facts: context, expected type, actual
  type, and the smallest structural difference.

The primary location answers “where did compilation become impossible?” A
related location answers “which earlier source construct established the
expectation or opened the construct?” For example, an incompatible `if` branch
points primarily at the conflicting branch and labels the previous branch as
the source of the expected type.

Machine-applicable fixes use a closed edit type. The LSP does not recognize
fixes by comparing `message` strings. Diagnostic codes and structured fix data
are the protocol boundary.

### Source-map contract

Source mapping is a correctness property. A diagnostic is not complete if its
source location cannot be trusted.

All source positions use byte offsets internally and carry the original source
identity. Conversion to line/column coordinates happens at the presentation
boundary from the original source text. CLI columns are Unicode-aware; LSP
positions continue to use the protocol-required UTF-16 units.

The following invariants apply:

1. Parsed leaves and collections cover their exact original source spans.
2. A transformation that preserves a source form preserves its origin.
3. A synthesized form records the source form that caused synthesis. Macro
   expansion must retain an origin chain from generated form to macro call and,
   when available, to the macro definition template.
4. Semantic IR and generated OCaml Parsetree nodes retain the most specific LG
   origin available. Generated helper nodes may be ghost locations, but their
   nearest enclosing user expression must not be ghosted.
5. Primary and related locations pass through the same filename and
   line-coordinate normalization.
6. Incremental compilation and cached compiler state must not change a source
   identity or silently reuse a span from a different source revision.
7. Renderers never infer a source span from generated OCaml text when an LG
   origin exists.
8. Provenance lookup remains expected O(1) in the successful compilation path.
   Source identity must never require a structural AST scan per lookup.

The existing physical-form lookup in `Source_context` remains an implementation
detail and an O(1) transient index, not the source-map identity. Stable source
node IDs include the source revision and exact span. Generated IDs add a
deterministic expansion path and explicit origin relation carried through the
semantic pipeline. Reconstructed macro forms are registered once per expansion;
lookups never fall back to structural tree searches.

### Compilation-speed contract

Diagnostic quality must not make normal compilation materially slower.
Successful compilation performs constant-time provenance lookup and does not
run a diagnostic-only whole-tree pass. Work proportional to a macro expansion
is allowed once while registering that expansion. Expensive recovery, origin
search, and structural type-difference rendering are deferred until an error is
being constructed.

Performance validation includes the existing large-frontend linearity test and
cold-build gate, plus an A/B comparison against the unchanged revision when a
machine cannot meet the absolute wall-clock gate. A change is rejected if the
median or repeated samples show a material regression even when correctness
tests pass.

### Parser diagnostics

The parser reports the expectation it was satisfying, not only the token that
failed. Delimiter diagnostics distinguish at least:

- an opener followed by end-of-input;
- an incorrect closing delimiter;
- an opener with no following expression;
- a malformed map key/value pair; and
- reader-conditional feature/form structure errors.

An incorrect closer is the primary location; its opener is a related location.
An unfinished collection retains the opener as the primary location until the
lexer/parser expose an exact EOF cursor, after which EOF becomes primary and
the opener becomes related.

### Type diagnostics

Type inference and elaboration retain contextual expectations instead of
flattening a failure into `type A and type B are incompatible`. Contexts include
function argument index, return annotation, conditional branch, collection
element, record field, reactive property, protocol method, and host boundary.

Type presentation has two layers:

- a structural type difference that finds the smallest meaningful mismatch;
- context-specific prose that explains why those two types were compared.

Known LG-specific remedies remain explicit. In particular, heterogeneous
branches suggest a closed sum type; diagnostics must never suggest a dynamic
escape hatch forbidden by `docs/design.md`.

Reactive-property diagnostics identify the property declaration, the
expression that produced the actual type, the expected and actual types, and
the enclosing component or record when available. Nested conditionals retain
an expectation chain so the innermost conflicting branch is primary and outer
branches are related context rather than one large enclosing span.

### Presentation boundaries

The CLI renders a compiler error with:

1. title, code, and original filename;
2. explanation;
3. source snippet and caret for the primary span;
4. labeled related snippets when the matching source is available, otherwise
   an explicit filename and source coordinate rather than unrelated text; and
5. hints.

The LSP publishes the same primary range, code, phase, hints, and related
locations. Terminal layout and LSP JSON are renderers of one diagnostic; they
do not maintain separate error taxonomies.

### Validation

Diagnostic behavior uses golden-style assertions at public integration
boundaries. Tests cover:

- exact filename and byte offsets;
- line and display-column conversion after non-ASCII source;
- primary and related spans for mismatched delimiters and branch errors;
- macro expansion, reader conditionals, metadata normalization, namespace
  lowering, incremental chunks, and cached state;
- Native and Melange source origins where lowering differs;
- CLI text without ANSI color and LSP JSON; and
- stable codes independent of wording changes; and
- large-file and cold-build time/allocation budgets.

Tests should assert the complete rendered shape for a small representative set
and structured fields for the larger diagnostic matrix. They should not make
all prose immutable when only code, title category, spans, and semantic facts
need stability.

## Implementation status

Implemented in August 2026.

- `Source_node_id` identifies a source revision, exact span, and deterministic
  generated path. Macro definitions retain a provenance table; generated forms
  carry call-site identity plus template origin chains through cached state,
  semantic IR, and OCaml Parsetree attributes. Unquoted arguments keep their
  original identity.
- Lexer and parser diagnostics distinguish exact EOF, incorrect closers,
  missing reader forms, incomplete maps, and incomplete reader conditionals.
  Delimiter fixes are structured insertions or replacements.
- `Error.type_mismatch` carries a closed type tree, contextual expectation, and
  smallest structural difference. Migrated contexts cover nested conditional
  branches, typed record/property values (the reusable boundary for reactive
  properties), source calls, protocol methods, annotations, and OCaml host
  calls.
- CLI rendering has an exact golden shape and Unicode-aware carets. Cross-file
  related locations never render against the primary file's text. LSP ranges
  use the originating open document, workspace snapshot, or disk source for
  UTF-16 conversion; when that source is unavailable, LSP reports the exact
  line without inventing a character range.
- LSP diagnostics expose structured type facts and related information. Code
  actions consume `Error.fix`; no English-message matching remains.
- Regression coverage includes source revisions, repeated equal forms, macro
  call/template/argument origins, Marshal-restored incremental macros, exact
  EOF and Unicode positions, Native/Melange origin equality, CLI output,
  workspace incremental publication, and cross-file LSP related ranges.
- The existing 15-second isolated cold-build gate passed at 14.48 seconds on
  the implementation host. Diagnostic-only Parsetree origin search remains on
  the OCaml exception path, and structural type differences are built only
  while constructing an error.

Errors outside the migrated semantic families retain stable phase defaults and
can adopt richer prose incrementally without changing this data model.

## Alternatives considered

### Improve existing error strings only

Rejected. It cannot support multiple spans, reliable editor integrations,
machine-applicable fixes, or presentation-specific rendering.

### Render OCaml compiler errors directly

Rejected. Generated identifiers and generated-source locations expose an
implementation detail and cannot reliably identify the LG expression that
caused the error.

### Store source text inside every error

Rejected. It duplicates potentially large inputs and makes cached/incremental
errors harder to reason about. Diagnostics carry source identity and spans;
the caller supplies the matching source snapshot to a renderer.

### Adopt a universal dynamic value to simplify type explanations

Rejected by the language design. Diagnostic quality must expose static type
relationships, not weaken them.

## Consequences

Compiler phases must spend more effort preserving provenance and expectation
context. Error constructors become slightly more explicit, and public record
changes require synchronized library, CLI, REPL, and LSP updates.

In return, errors become independently renderable, editor integrations stop
depending on English text, source-map regressions become testable, and LG can
improve wording without sacrificing stable semantic identity.
