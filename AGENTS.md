@/Users/tiensonqin/.codex/RTK.md

# Project constraints

- Read and follow `docs/design.md` before changing the language, compiler,
  runtime, DataScript port, type system, or interop behavior. It is the
  authoritative design contract for static typing and compatibility boundaries.
- Code and comments must be written in English.
- Solve root causes instead of adding workarounds.
- Prefer simple designs over complex ones.

## OCaml type safety

- Never use `Obj.magic`. Do not hide it behind an alias, wrapper, generated helper, or equivalent unsafe cast.
- Heterogeneous Clojure data does not by itself justify using `Runtime_dynamic.t` or another universal dynamic type.
- Model known heterogeneous domains with closed, statically typed variants and records. This includes DataScript values, datoms, schemas, transactions, query forms, query inputs, query results, parser forms, EDN values, serialization payloads, and mixed-type map keys whenever their supported domain can be represented explicitly.
- Prefer the datascript-ocaml approach: use recursive tagged unions such as a closed `value` type instead of erasing values into a universal runtime representation.
- Treat upstream DataScript algorithms and observable behavior as authoritative. Static typing may replace raw representations, but it must not remove API arities, repeated-filter composition, query/pull/rules cases, transaction ordering, or benchmark workloads.
- Preserve upstream control flow when replacing dynamic dispatch with closed sums. Do not create a second simplified algorithm; keep branch order, cursor movement, frame order, and termination behavior unless a measured semantics-preserving optimization is documented.
- Keep static ports readable with named state records, small typed accessors, and one closed dispatch sum. Avoid long positional constructors, duplicated branches, and compatibility overlays.
- Keep concrete fields concrete. For example, entity IDs, attributes, transaction IDs, `added`, schema attributes, callbacks, and storage payloads must retain their precise types.
- Type variables in declared signatures are rigid static types. Do not materialize them as dynamic values for equality, callbacks, zero-argument functions, options, collections, or host calls.
- Host-boundary assignability must not permit implicit conversion between static values and dynamic constraints.
- `Runtime_dynamic.t` is allowed only inside a documented compiler/runtime implementation boundary where upstream behavior cannot be represented by a closed type without losing required compatibility. It is never a source type, sidecar type, collection type, or public conversion API.
- When a dynamic boundary is unavoidable, restrict it to the smallest individual field or function boundary. Never make an enclosing record, collection, or subsystem dynamic merely because one contained value is open.
- Convert from a dynamic boundary into a validated static representation as early as possible, and return to dynamic form only where the external compatibility boundary requires it.
- Before introducing or expanding dynamic typing, document the concrete upstream behavior that requires an open type and explain why a closed variant is insufficient.
- Do not generate or accept `__lg_dynamic`, `__lg_dynamic-narrow`, `to_dynamic`, or `of_dynamic` escape hatches.
- Storage reference types are the closed sum `Strong | Weak` on both Native and Melange. Do not add a fake `Soft` constructor; use `Weak` for releasable caches.
