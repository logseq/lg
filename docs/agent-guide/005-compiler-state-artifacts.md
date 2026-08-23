# ADR 005: Version and Bound Compiler State Artifacts

Status: Accepted

## Context

LG carries semantic and OCaml compiler state across source chunks. The CLI
persists that state for standard-library bootstrapping and explicit
continuation. Ordinary multi-file compilation also caches prefix outputs.

The original format wrote `Lg.Compiler.state` directly with OCaml `Marshal`.
That representation had no magic header, format version, artifact kind, payload
length, or integrity check. Invalid input escaped as an uncaught exception, and
a corrupt internal prefix state could turn a cache hit into a failed build
instead of a cache miss.

Every cached prefix also retained the complete reachable compiler state. A disk
size limit bounds retained files but does not prevent cumulative serialization
work or repeated state payloads.

## Decision

All persisted compiler state uses one internal artifact envelope containing:

- a fixed LG compiler-state magic value;
- a format version;
- an artifact kind;
- the exact payload length;
- a payload digest; and
- one opaque Marshal payload.

The envelope is validated before unmarshalling. Unknown versions and kinds are
rejected explicitly. Truncated, corrupt, or trailing payloads fail closed.
Payloads larger than 512 MiB are rejected before hashing or unmarshalling.
Writes marshal into a temporary payload, write a temporary complete artifact,
and atomically rename the artifact into place.

User-supplied saved states report structured compiler errors. Internal cache
artifacts are disposable: an invalid output is removed and compilation
continues as a cache miss.

Marshal remains an implementation payload, not the persistent format contract.
Version 2 records compiler states whose semantic constraints use closed OCaml
variants; readers reject version 1 rather than unmarshalling its incompatible
type layout. Readers must never attempt compatibility recovery across an
unknown envelope version.

Version 7 adds registered exception-data adapters to the compiler environment.
Readers reject version 6 before unmarshalling because OCaml record layout
changes are not Marshal-compatible and can otherwise crash the process.
Version 8 distinguishes direct adapters from record-field adapters; readers
likewise reject version 7 before unmarshalling the changed adapter payload.

## Checkpoint ownership

Explicit saved states are versioned full checkpoints. They are requested by the
caller and provide the base state for a later continuation, such as the
precompiled standard library.

Ordinary prefix caching does not persist compiler state. It stores only the
content-addressed compilation output for each source. On a cache hit, LG replays
semantic compilation with OCaml checking disabled to reconstruct the typed
state, while returning the cached output and diagnostics. If a later source is
a cache miss, LG restores the OCaml compiler environment once from the cached
OCaml sources before checking the new source.

This avoids cumulative serialized state at every prefix while preserving
partial-prefix reuse, deterministic output, source locations, package
discovery, and batch/incremental equivalence. A cache capacity limit remains a
retention policy rather than the mechanism that bounds checkpoint growth.

## Concurrency

Artifact files are immutable after publication. Writers publish with atomic
rename. Cache reads, writes, access-time updates, generation removal, and
capacity pruning are coordinated by an advisory process lock stored at the
cache root. Directory creation tolerates concurrent creators. The lock file is
not part of a compiler generation and is never pruned.

## Validation

Integration tests cover:

- valid state continuation;
- invalid artifact magic;
- unsupported versions;
- oversized payload declarations;
- truncated payloads;
- payload integrity;
- corrupt prefix-cache recovery;
- target mismatch;
- cache size pruning; and
- concurrent cache writers and cleanup of temporary artifacts.

The applicable CLI smoke, full LG suite, and downstream compiler consumers must
pass whenever the artifact payload or restoration path changes.
