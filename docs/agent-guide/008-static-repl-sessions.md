# ADR 008: Add Static REPL Sessions over the OCaml Bytecode Toplevel

- Status: Accepted; Phase 1 implemented
- Date: 2026-08-26
- Decision owners: LG compiler maintainers
- Related: `docs/design.md`, `docs/architecture.md`,
  `docs/agent-guide/005-compiler-state-artifacts.md`,
  `docs/agent-guide/006-compiler-session-concurrency.md`, and
  `docs/agent-guide/007-structured-compiler-diagnostics.md`

## Context

Before this decision, LG supported incremental compilation but not interactive
evaluation. The public
compiler API can parse, typecheck, and emit one source chunk while returning a
new `Compiler.state`. That state preserves namespaces, aliases, macros, types,
protocols, and prior bindings. The CLI can also restore the precompiled
standard-library state and compile or run another chunk.

The existing run path is not a REPL. `lg --run` and `lg --run-from` generate a
temporary OCaml source file, invoke `ocamlopt`, run a fresh executable, and
exit. Repeating that operation would preserve compiler state only if it were
saved explicitly, but it would lose atoms, mutable references, closures, open
files, registered callbacks, and every other runtime value after each form.

Clojure's REPL keeps a current namespace and runtime process, evaluates forms
sequentially, prints each result, preserves definitions and side effects, and
records recent results and exceptions in `*1`, `*2`, `*3`, and `*e`. Clojure
also provides raw Socket REPLs through `clojure.core.server` and the structured
prepl event model. nREPL is a separate ecosystem protocol, not the definition
of a Clojure REPL.

ClojureDart is a useful typed-host comparison. Its beta socket REPL does not
interpret Dart values in the compiler process. It compiles each form, triggers
Dart/Flutter hot reload, and dispatches a generated thunk into the still-running
target process. The target owns `*1`, `*2`, `*3`, `*e`, output, and Flutter
state. This preserves a dynamic ClojureDart runtime, but its arbitrary result
slots and runtime namespace/Var behavior cannot be copied into LG without
weakening LG's static type contract.

LG needs the same interactive development loop without adding a source-level
universal value, runtime `eval`, whole-program Var registry, or implicit
`Runtime_dynamic.t` conversion.

## Clojure REPL surface comparison

Clojure's REPL support is a stack of related facilities rather than one
protocol. LG separates the same concerns so the local terminal, a future socket
client, and an attached application can reuse one compiler session model.

| Facility | Clojure behavior | LG decision |
| --- | --- | --- |
| Terminal REPL | `clojure.main/repl` reads forms, evaluates them in a persistent runtime, tracks `*ns*`, and prints results. | Phase 1 implements `lg repl` over one persistent OCaml bytecode toplevel. The prompt follows the committed LG namespace. |
| Form reader | Reads one complete Clojure form and continues multiline input. | Phase 1 uses the LG lexer and parser and distinguishes empty, complete, incomplete, and invalid input. It can consume several forms from pasted input. |
| Namespace | `in-ns`, `ns`, and `*ns*` change the session's current namespace. | `(ns ...)` updates the incremental compiler state. Later forms resolve in that namespace, and the prompt changes with it. There is no runtime namespace registry. |
| Type inspection | Clojure values carry runtime classes; tooling can inspect metadata and Java classes. | Every evaluated LG expression prints its compiler-known static type. `:type form` typechecks without executing the form. |
| Recent results | `*1`, `*2`, `*3`, and `*e` hold arbitrary runtime objects. | Source-visible heterogeneous result Vars are excluded. A protocol client may retain rendered values, static type names, and diagnostics. |
| Raw Socket REPL | `clojure.core.server` can expose a `clojure.main/repl` accept function over a socket; text input/output can be used with telnet or netcat. | Phase 2 adds a loopback-only Socket REPL using LG's structured protocol and bundled client. A raw text adapter may be added later but is not the protocol contract. |
| prepl | `prepl`/`io-prepl` emits maps tagged `:ret`, `:out`, `:err`, or `:tap`; a form has one return event and may have multiple output events. | LG adopts the event-stream shape but uses a closed request/response sum. A value event contains only rendered text and its static type, never an erased LG value. |
| Session isolation | A Clojure socket server accepts clients on separate threads in one JVM; dynamic bindings such as `*in*`, `*out*`, `*err*`, `*ns*`, and `*session*` distinguish clients while application state can still be shared. | Each LG stateful session owns a worker process because `Toploop` and compiler-libs have process-global mutable state. Evaluation inside a worker is serialized. |
| Interrupt and failure | Frontends can interrupt evaluation; a REPL normally survives form errors. | Phase 1 survives compile errors. Phase 2 assigns request IDs and interrupt tokens; a wedged worker can be terminated without corrupting other sessions. |
| Dependencies | Clojure CLI aliases establish the classpath before a REPL; libraries can also build tooling for runtime dependency changes. | The package/load-path set is fixed when an LG worker starts. Interactive dependency mutation is a separate design problem. |
| Network security | A Socket REPL is arbitrary code execution; Clojure's examples and defaults use loopback addresses. | LG binds to loopback by default. Initial Socket REPL support rejects non-loopback binding; remote access must be provided by an authenticated, encrypted tunnel or gateway. |
| nREPL | nREPL defines a bencode message protocol, operations, sessions, and middleware used by editor tooling. | Deliberately unsupported. LG does not implement nREPL transport, operations, bencode framing, or middleware compatibility. |

The comparison is behavioral, not representational. Clojure can retain
arbitrary objects in Vars and protocol maps because it is dynamic. LG keeps
compiler state, concrete runtime values, and transport metadata separate.

## Delivery status

Phase 1 is the local Native bytecode REPL implemented by this ADR. Later phases
are accepted architectural directions, not claims about current commands.

| Capability | Status |
| --- | --- |
| Persistent bytecode evaluation and runtime heap | Implemented |
| Multiline and pasted-form reader | Implemented |
| Precompiled Native stdlib bootstrap | Implemented |
| Namespace switching and namespace prompt | Implemented |
| Static type beside evaluated values | Implemented |
| Non-executing `:type` query | Implemented |
| Candidate compiler-state rollback on compile/evaluation failure | Implemented |
| Typed same-signature root replacement for existing callers | Planned |
| Evaluation IDs, captured stdout/stderr, and interrupt | Planned |
| Socket REPL and bundled socket client | Planned, Phase 2 |
| nREPL compatibility | Excluded |

## Decision drivers

The design must:

- preserve compiler and runtime state across forms;
- use the existing incremental compiler and precompiled stdlib state;
- run generated code after the final OCaml typecheck;
- retain precise static types for definitions, references, functions, records,
  and collections;
- print values without returning heterogeneous OCaml values through a universal
  host container;
- keep compile failures from advancing the session compiler state;
- support Clojure-like namespace and macro workflows where they are statically
  representable;
- make redefinition behavior explicit where OCaml lexical bindings differ from
  Clojure Vars; and
- leave a clean transport boundary for editor clients and a future attached
  target REPL.

## Prototype evidence

The prototype in `prototype/repl` validates the proposed execution foundation.
It:

1. reads the existing versioned Native stdlib state artifact;
2. restores the OCaml checking environment from the artifact's aggregate
   source;
3. initializes a bytecode toplevel with `Toploop.prepare`;
4. adds the compiled stdlib interface directory to the toplevel load path;
5. opens the already-linked `Lg_stdlib_native` compilation unit;
6. compiles every LG form with `Compiler.compile_chunk_parsetree`;
7. executes the resulting structure as `Parsetree.Ptop_def`; and
8. commits the candidate `Compiler.state` only after successful execution.

The integration test proves that:

- a definition is visible to a later form;
- a function compiled in one form runs in a later form;
- a bare expression produces a result;
- a failed compilation does not publish its candidate compiler state;
- new forms see a later same-name definition;
- an already compiled function retains the old direct binding under the
  current static lowering;
- precompiled stdlib functions are callable;
- a macro defined in one form expands in a later form; and
- an atom mutated in one form retains its runtime value in a later form.

The prototype initially used `Toploop.initialize_toplevel_env` alone. That was
insufficient for a custom toplevel because linked project compilation units
were not imported into the complete toplevel setup. `Toploop.prepare`, plus the
compiled stdlib CMI directory, is the required initialization boundary.

The prototype uses the OCaml toplevel printer only to prove execution. The
Phase 1 implementation replaces that path and does not expose OCaml value
rendering as LG result rendering.

## Decision

LG adds a persistent, statically typed REPL session whose first execution
backend is the OCaml bytecode toplevel. Phase 1 implements the terminal worker;
Phase 2 will add the socket transport defined below.

This is a development tool and compiler service. It does not add a runtime
`eval` function to the LG language.

### 1. One worker process owns one runtime session

The first user-facing entry point is `lg repl`. It starts one bytecode
worker process and keeps it alive until the user exits. The worker owns:

- one target-specific `Compiler.state`;
- one OCaml toplevel environment and runtime heap;
- the current source namespace;
- the fixed package/load-path set selected at startup; and
- the result sink for the current evaluation.

Forms are compiled and executed sequentially. `Toploop` and compiler-libs use
process-global mutable state, so multiple independent runtime sessions will not
share one process. A future network server will allocate one worker process per
persistent session and communicate with it through the REPL message protocol.
This extends ADR 006 instead of weakening its serialization boundary.

The initial REPL executes LG's Native target semantics as OCaml bytecode. It is
not a Melange REPL and does not claim to execute inside a running application.

### 2. Bootstrap from the precompiled stdlib artifacts

The worker executable links the Native runtime and
`lg_compiled_stdlib_native` bytecode library. Startup:

1. locate and validate `lg_stdlib_native.state` through the shared compiler
   artifact reader;
2. verify that its target is Native and that its compiler/artifact version
   matches the running compiler;
3. restore the compiler-libs checking environment from the aggregate stdlib
   source recorded in the artifact;
4. run `Toploop.prepare`;
5. add the stdlib and package CMI directories to the toplevel load path;
6. execute `open Lg_stdlib_native` once; and
7. select the `user` source scope with automatic core refers.

The artifact envelope reader is shared by the CLI and REPL through
`Compiler_artifact`; the production worker does not copy or bypass the artifact
contract.

The package set is fixed at session startup in the first version. Interactive
dependency addition requires coordinated CMI discovery, bytecode loading, and
compiler-state updates and will be a separate decision. An unsupported package
request must fail explicitly rather than compile against a package that is not
available to the runtime toplevel.

### 3. Read complete LG forms, not terminal lines

The terminal frontend accumulates input until the reader reports one
complete form. The reader API will distinguish:

```ocaml
type repl_read =
  | Empty
  | Complete of { source : string; remaining : string }
  | Incomplete
  | Invalid of Compiler.compile_error
```

Unterminated lists, vectors, maps, strings, regexes, reader conditionals, and
anonymous function forms produce `Incomplete` only when more input can
complete them. Other lexical or parse failures produce the existing structured
diagnostic immediately.

Phase 2 adds request IDs and source-location metadata so editor diagnostics and
stack traces can retain the originating file, line, and column.

### 4. Compile and execute transactionally

The compiler exposes a REPL-specific form operation over the same semantic
pipeline as `compile_chunk_parsetree`:

```ocaml
val compile_repl_form :
  ?target:Target.t -> state -> string ->
  (state * repl_compilation, compile_error) result
```

`repl_compilation` contains the checked Parsetree and a closed result-display
plan for value, definition, namespace, or compiler-known summary. It does not
contain an `Obj.t`,
`Runtime_dynamic.t`, or universal LG value.

Evaluation follows this state transition:

```text
committed compiler state
        |
        | compile one form
        v
candidate compiler state + checked Parsetree
        |
        | execute in the persistent bytecode toplevel
        v
commit candidate state on success
```

A reader, macro, semantic, lowering, or OCaml-checking error leaves both the
committed compiler state and the toplevel unchanged. If generated code raises
at runtime, the candidate compiler state is not committed. Runtime side effects
that occurred before the exception are not rolled back; this matches ordinary
interactive evaluation and avoids pretending that arbitrary OCaml effects are
transactional.

Only the current form is submitted to `Toploop`. Prior forms are not regenerated
or replayed.

### 5. Render results through a statically selected LG printer

Production result rendering does not use `Toploop`'s OCaml printer. The compiler
evaluates a top-level expression exactly once and emits the existing,
statically selected LG print operation for its concrete type. The runtime sends
only the rendered string and static type name to the session sink.

Conceptually, an expression form becomes:

```ocaml
let () = Repl_runtime.publish_value "<static type>"
  (<typed LG printer> <typed expression>)
```

This preserves evaluation order and side effects while keeping the concrete
value out of a heterogeneous host container. Records, variants, options,
collections, and external closed domains continue to use their existing static
printing evidence. A value with no valid static printer is a compile-time REPL
error with its concrete type.

Definition forms return a closed definition summary rather than a fake
runtime Var. Namespace, type, protocol, macro, and module forms likewise return
their compiler-known summary. User stdout and stderr are separate from the
form's value response once Phase 2 output capture is implemented.

### 6. Preserve namespaces in compiler state

The prompt reflects the committed compiler state's source scope, initially
`user`. An `(ns ...)` form uses the existing incremental namespace machinery;
subsequent forms compile in that namespace. Aliases, refers, macro refers,
protocols, and types remain compiler state, not a runtime reflection registry.

`Compiler.state` has a read-only current-scope accessor so terminal and
protocol frontends do not inspect `Toolchain.state` internals. Namespace
enumeration and completion will reuse language-service indexes. LG will not add
Clojure's runtime `all-ns`, `intern`, `resolve`, or arbitrary Var lookup solely
for the REPL.

The terminal prints the static type beside every value and definition. Its
`:type form` command uses the same typechecker against the committed session
state but does not execute the form, so type inspection cannot trigger the
form's side effects.

### 7. Make redefinition static and explicit

The prototype confirms the baseline OCaml behavior:

- a later same-name definition shadows the earlier binding for newly compiled
  forms; and
- already compiled functions retain direct references to the earlier binding.

That baseline is safe but insufficient for the common REPL workflow of editing
a function and invoking an already compiled caller. LG will therefore support
typed root replacement when the old and new definition have the same static
function type.

LG already emits typed function roots for ordinary monomorphic application
functions used by `with-redefs`. REPL compilation will reuse the existing root
when a same-name `defn` has the same static type. Existing callers continue to
deref that concrete function root and observe the new implementation. No
dynamic function adapter or heterogeneous Var table is introduced.

When a redefinition changes type, LG cannot update the old typed root. It will
create a new static binding generation for newly compiled forms and report an
interactive warning that existing compiled callers retain the old generation.
The same generational rule applies to nominal record and variant type
redefinitions.

Macros are compile-time definitions. Redefining a macro affects later macro
expansions but cannot rewrite code that was already expanded and executed.

Rootizing arbitrary `def` values is deferred. The initial implementation will
not silently turn every top-level value into a mutable cell merely to imitate
Clojure Var identity.

### 8. Do not expose heterogeneous recent-result Vars

Clojure's `*1`, `*2`, `*3`, and `*e` can hold unrelated runtime object types
over time. Implementing them as ordinary LG Vars would violate the invariant
that a dynamically bindable Var has one static value type.

Phase 1 prints the current rendered result and error. Phase 2 may retain recent
rendered results and structured errors in the host session protocol. Terminal
commands and editor clients may display or copy that history, but LG source
cannot consume it as an arbitrary value.

A future typed history feature may expose a compiler-resolved alias to one
specific generated result binding, preserving that result's concrete type. It
must be designed separately and must not become a universal mutable result
slot.

### 9. Add an LG Socket REPL with a closed protocol

Phase 2 adds a Socket REPL for editor clients and terminal attachment. It is an
LG protocol, not an nREPL implementation. The server supervisor creates one
bytecode worker process per stateful session and serializes evaluations within
that worker. Separate workers isolate compiler-libs globals, runtime heaps,
interrupts, crashes, and code accumulation.

The first transport is length-prefixed JSON over TCP. Framing is independent of
newlines in source or output, and the payload is constrained by a closed schema.
Requests are:

```ocaml
type request =
  | Evaluate of evaluation_request
  | Type_of of type_request
  | Interrupt of request_id
  | Describe
  | Close
```

An evaluation request includes a request ID, source text, optional source
location, and optional expected namespace. Responses form a stream of closed
events:

```ocaml
type response =
  | Stdout of text_event
  | Stderr of text_event
  | Value of { rendered : string; type_name : string }
  | Definition of { name : string; type_name : string }
  | Namespace of string
  | Diagnostic of Compiler.compile_error
  | Status of final_status
```

This borrows prepl's useful property that output can stream before exactly one
final status, while preserving LG's static boundary. JSON never carries an
erased LG runtime value. The protocol is versioned, `Describe` reports the
worker target and capabilities, and an expected-namespace mismatch rejects the
request instead of silently compiling in the wrong context.

The server binds to a loopback address by default and accepts `port 0` so the OS
can choose a free port that the launcher reports to its client. Initial support
does not allow a non-loopback bind. A remote workflow must place the loopback
listener behind an authenticated, encrypted tunnel or gateway because the
service provides arbitrary code execution. Resource limits, maximum frame
size, idle timeout, and worker termination are server responsibilities.

`lg repl --connect HOST:PORT` is the bundled interactive client. Closing a
client sends `Close`; the terminal's local `:quit` remains a frontend command.
A raw netcat-compatible text adapter may be considered later, but structured
framing is the interoperability contract.

### 10. Keep application hot reload out of scope

This ADR does not design or implement attached application REPLs or hot reload
for Melange, Native desktop, iOS, or Android. ClojureDart remains useful prior
art, but LG will make a separate decision if application attachment becomes a
priority. The bytecode REPL must not claim target-app execution semantics.

### 11. Do not implement nREPL

nREPL is useful prior art for editor workflows, sessions, streaming output, and
interrupt semantics, but LG does not support it. This ADR does not authorize an
nREPL port, bencode codec, nREPL operation names, middleware model, discovery
files, or compatibility claim. Supporting it later would require a separate
decision and must still preserve LG's closed static value boundary.

## Alternatives considered

### Recompile and run a fresh executable for every form

Rejected. It can reuse compiler state but loses runtime identity and replays or
discards side effects. It is a command runner, not a Clojure-like REPL.

### Interpret LG forms in the compiler

Rejected. A complete interpreter would duplicate runtime algorithms and host
interop, diverge from generated OCaml behavior, and pressure the design toward
a universal value representation. Macro evaluation remains a deliberately
smaller compile-time domain and is not an application evaluator.

### Use Native `Dynlink` plugins for every form

Deferred. `Dynlink` safely loads `.cmxs` plugins, but each plugin is a new OCaml
compilation unit, loaded units cannot be unloaded, and values are not directly
available to the host without a registration API. Incrementally extending one
toplevel namespace, result printing, package compilation, and same-name
redefinition are more complex than with the bytecode toplevel. Native plugins
may become a later execution backend if bytecode performance is insufficient.

### Store every result and Var as `Runtime_dynamic.t`

Rejected by `docs/design.md`. Interactive convenience is not an open external
boundary and does not justify erasing statically known values, functions,
records, collections, or result history.

### Copy ClojureDart's hot-reload design for the first version

Deferred. It is appropriate for an attached Flutter or browser target but
requires a target reload mechanism and runtime transport that LG does not yet
have. The bytecode toplevel proves the core session semantics with substantially
less machinery.

### Implement nREPL directly

Rejected for this scope. nREPL's extensible operation and middleware maps are a
good fit for a dynamic ecosystem, but adopting its transport does not solve
LG's target execution or static result-boundary problems. The smaller closed LG
protocol can state exactly which requests and result domains are supported.

## Consequences

- LG gains a practical read-compile-execute-print loop without a runtime
  interpreter.
- Compiler and runtime state live together for the duration of a worker.
- The stdlib checkpoint remains the single semantic bootstrap source.
- Result rendering stays statically typed and Clojure-shaped.
- Same-type function redefinition through typed roots remains planned;
  Phase 1 follows OCaml's lexical generation behavior.
- `*1`-style arbitrary source values, runtime `eval`, and Var reflection remain
  unsupported.
- The first implementation is bytecode-only and may run slower than a Native
  executable.
- OCaml toplevel definitions accumulate and cannot be unloaded. Long sessions
  consume increasing code and type-environment memory; the worker process is
  the reclamation boundary.
- A worker crash loses runtime state. Persisted `Compiler.state` alone cannot
  resume live closures, atoms, resources, or callbacks.
- Multiple editor sessions require multiple worker processes, which also gives
  clean interruption and failure isolation.
- Socket access is deliberately a separate, loopback-only phase because it is
  an arbitrary-code-execution boundary.
- nREPL clients and middleware do not work with LG.

## Implementation sequence

### Phase 1: local terminal worker

1. Keep the isolated prototype and integration test as feasibility evidence.
   Done.
2. Share compiler artifact reading between the CLI and REPL. Done.
3. Add complete/incomplete/invalid form reading and multiline terminal input.
   Done.
4. Add the current-scope, REPL compilation, and non-executing type-query APIs
   without duplicating the semantic pipeline. Done.
5. Bootstrap a persistent bytecode worker from the Native stdlib state. Done.
6. Publish rendered LG values and static types through a closed runtime sink.
   Done.
7. Expose the worker as `lg repl`, including namespace prompts and clean exit.
   Done.

### Phase 1 follow-up

1. Capture stdout and stderr independently from the result event.
2. Add request IDs, synthetic source locations, error recovery tests, and local
   interruption.
3. Add same-type `defn` root replacement and type-change warnings.
4. Add protocol, record, variant, runtime-exception, and long-session coverage.

### Phase 2: Socket REPL

1. Define and version the closed length-prefixed JSON schema.
2. Add worker supervision, request IDs, streaming output, interrupt, frame
   limits, idle timeout, and loopback-only TCP binding.
3. Add `lg repl --connect`, session lifecycle tests, concurrent-worker tests,
   and security-negative tests.

Attached application hot reload and nREPL are not implementation phases of this
ADR.

## Validation

Phase 1 validation covers:

- multiline reader completeness and invalid-form recovery;
- stdlib state/version/target validation;
- stdlib functions, macros, aliases, refers, and protocols in the REPL;
- definitions, functions, records, variants, and namespaces across forms;
- mutable atom/reference state and closure identity across forms;
- compile failures without candidate-state publication;
- Clojure-shaped printing for every supported statically printable domain;
- macro redefinition affecting only later expansion;
- rejection of unsupported dynamic result/Var operations;
- absence of new `Runtime_dynamic`, `Obj.magic`, `__lg_dynamic`, `to_dynamic`,
  or `of_dynamic` paths; and
- sequential compiler-libs/Toploop access under ADR 006.

The follow-up phases additionally test runtime exceptions without session
termination; separate stdout, stderr, value, warning, diagnostic, and status
events; interruption; same-type root replacement; type-changing generation
isolation; worker cleanup and restart; socket framing and limits; concurrent
session isolation; and loopback enforcement.

The focused prototype and production tests are:

```sh
opam exec -- dune runtest prototype/repl
opam exec -- dune exec ./test/repl_session_tests.bc -- \
  ./_build/default/stdlib/lg_stdlib_native.state
opam exec -- dune exec -- bash test/repl_cli_test.sh \
  _build/default/bin/lg_cli.exe \
  _build/default/bin/lg_repl_worker.bc \
  _build/default/stdlib/lg_stdlib_native.state
```

Dune commands remain serialized with every other Dune operation in the
repository.

## References

- [Clojure REPL and main entry points](https://clojure.org/reference/repl_and_main)
- [Clojure CLI Socket REPL guide](https://clojure.org/guides/deps_and_cli#_using_a_repl)
- [Clojure `core.server` implementation](https://github.com/clojure/clojure/blob/master/src/clj/clojure/core/server.clj)
- [Clojure prepl API](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core.server/prepl)
- [Clojure namespaces](https://clojure.org/reference/namespaces)
- [nREPL sessions, comparison reference only](https://nrepl.org/nrepl/building_clients.html#_sessions)
- [nREPL protocol, comparison reference only](https://spec.nrepl.org/)
- [ClojureDart REPL overview](https://github.com/Tensegritics/ClojureDart#repl-beta)
- [ClojureDart host compilation and reload loop](https://github.com/Tensegritics/ClojureDart/blob/dfa7a0c83e0902aae23df992ded3fb7f8b577bc6/clj/src/cljd/build.clj#L261-L317)
- [ClojureDart per-form recompilation](https://github.com/Tensegritics/ClojureDart/blob/dfa7a0c83e0902aae23df992ded3fb7f8b577bc6/clj/src/cljd/compiler.cljc#L5110-L5159)
- [ClojureDart target-side REPL state](https://github.com/Tensegritics/ClojureDart/blob/dfa7a0c83e0902aae23df992ded3fb7f8b577bc6/clj/src/cljd/flutter.cljd#L1216-L1275)
- [OCaml toplevel system](https://ocaml.org/manual/5.1/toplevel.html)
- [OCaml Dynlink library](https://ocaml.org/manual/5.1/libdynlink.html)
