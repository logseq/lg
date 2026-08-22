# ADR 006: Serialize Compiler-Libs Sessions

Status: Accepted

## Context

LG's semantic state is immutable and returned explicitly between incremental
compilations. OCaml compiler-libs is different: target flags, load paths,
environment caches, lazy initialization, and the warning reporter are
process-global mutable state.

Concurrent Native and Melange calls could interleave that state. The observed
failure was `CamlinternalLazy.Undefined` during concurrent initialization, but
target leakage and warning-reporter restoration were also possible even when a
run happened to complete.

## Decision

All supported compiler and language-service entry points execute their
compiler-libs portion through one process-wide coordinator. The coordinator
holds a mutex for the complete operation and releases it with exception-safe
cleanup.

The unit of isolation is a compiler API call. Semantic `Compiler.state` values
remain caller-owned and can be prepared independently, but operations that
consult or update compiler-libs are serialized. This makes concurrent callers
safe without pretending that compiler-libs itself supports parallel sessions.

Pure accessors and output formatting do not take the coordinator. Internal
`Toolchain` modules are not a supported concurrency boundary and must not be
exposed as production API.

## Consequences

- Native, Melange, LSP, and CLI callers cannot race compiler-libs globals.
- Independent processes still compile in parallel.
- A single process does not perform parallel OCaml typechecking. Semantic
  parallelism can be introduced later only after its compiler-libs boundary is
  separated explicitly.
- Slow operations hold the coordinator for correctness. Metrics should make
  queueing visible before considering a more complex worker-process design.

## Validation

A stress regression starts Native compilation, Melange compilation, and
language-service analysis in multiple domains behind one start barrier. Every
result must retain its target-specific branch and the run must not raise from
compiler-libs initialization.
