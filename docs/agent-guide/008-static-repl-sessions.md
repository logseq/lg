# ADR 008: Add Static REPL Sessions over the OCaml Bytecode Toplevel

- Status: Accepted; local, Socket, and core nREPL tooling phases implemented
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

ClojureDart is a useful typed-host comparison. Its `flutter-repl` branch does not
interpret Dart values in the compiler process. It compiles each form, triggers
Dart/Flutter hot reload, and dispatches a generated thunk into the still-running
target process. The target stores `*1` and `*e`, sends prefixed stdout/value/ack
messages through the Flutter process stream, and preserves Flutter state. This
preserves a dynamic ClojureDart runtime, but its arbitrary result slots and
runtime namespace/Var behavior cannot be copied into LG without weakening LG's
static type contract. Its constant stream tag also assumes one active REPL and
is not an appropriate multi-client protocol for LG.

LG needs the same interactive development loop without adding a source-level
universal value, runtime `eval`, whole-program Var registry, or implicit
`Runtime_dynamic.t` conversion.

## Clojure REPL surface comparison

Clojure's REPL support is a stack of related facilities rather than one
protocol. LG separates the same concerns so the local terminal and socket
client can reuse one compiler session model without implying an attached
application REPL.

| Facility | Clojure behavior | LG decision |
| --- | --- | --- |
| Terminal REPL | `clojure.main/repl` reads forms, evaluates them in a persistent runtime, tracks `*ns*`, and prints results. | Phase 1 implements `lg repl` over one persistent OCaml bytecode toplevel. The prompt follows the committed LG namespace. |
| Form reader | Reads one complete Clojure form and continues multiline input. | Phase 1 uses the LG lexer and parser and distinguishes empty, complete, incomplete, and invalid input. It can consume several forms from pasted input. |
| Namespace | `in-ns`, `ns`, and `*ns*` change the session's current namespace. | `(ns ...)` updates the incremental compiler state. Later forms resolve in that namespace, and the prompt changes with it. There is no runtime namespace registry. |
| Type inspection | Clojure values carry runtime classes; tooling can inspect metadata and Java classes. | Every evaluated LG expression prints its compiler-known static type. `:type form` typechecks without executing the form. |
| Recent results | `*1`, `*2`, `*3`, and `*e` hold arbitrary runtime objects. | Source-visible heterogeneous result Vars are excluded. A protocol client may retain rendered values, static type names, and diagnostics. |
| Raw Socket REPL | `clojure.core.server` can expose a `clojure.main/repl` accept function over a socket; text input/output can be used with telnet or netcat. | Phase 2 implements a loopback-only Socket REPL using LG's structured protocol and bundled client. It is not a raw text socket. |
| prepl | `prepl`/`io-prepl` emits maps tagged `:ret`, `:out`, `:err`, or `:tap`; a form has one return event and may have multiple output events. | LG adopts the event-stream shape but uses a closed request/response sum. A value event contains only rendered text and its static type, never an erased LG value. |
| Session isolation | A Clojure socket server accepts clients on separate threads in one JVM; dynamic bindings such as `*in*`, `*out*`, `*err*`, `*ns*`, and `*session*` distinguish clients while application state can still be shared. | Each LG stateful session owns a worker process because `Toploop` and compiler-libs have process-global mutable state. Evaluation inside a worker is serialized. |
| Interrupt and failure | A REPL normally survives form errors. Raw `clojure.core.server` REPL and prepl do not define a request-ID interrupt operation; editor interrupt behavior normally comes from a richer protocol such as nREPL. | LG survives compile and runtime errors and assigns request IDs. Protocol version 1 reports `interrupt_supported = false`; process termination remains the hard stop boundary. |
| Dependencies | Clojure CLI aliases establish the classpath before a REPL; libraries can also build tooling for runtime dependency changes. | The package/load-path set is fixed when an LG worker starts. Interactive dependency mutation is a separate design problem. |
| Network security | A Socket REPL is arbitrary code execution; Clojure's examples and defaults use loopback addresses. | LG binds to loopback by default. Initial Socket REPL support rejects non-loopback binding; remote access must be provided by an authenticated, encrypted tunnel or gateway. |
| nREPL | nREPL defines a bencode message protocol, request IDs, persistent sessions, operations, and an extensible capability directory used by editor tooling. | Phase 3 implements a separate standards-based bencode endpoint over the same LG session engine. The existing JSON Socket REPL remains an LG protocol and is not relabeled as nREPL. |
| Editor clients | CIDER connects to nREPL and expects many Clojure-specific features from `cider-nrepl` middleware. `neat` is a language-agnostic nREPL client with a reusable integration suite. | `neat` is the verified Phase 3 client. CIDER is unverified and, if used, can rely only on operations LG advertises; LG does not claim `cider-nrepl` middleware compatibility. Neither client can connect to the JSON Socket REPL port. |

The comparison is behavioral, not representational. Clojure can retain
arbitrary objects in Vars and protocol maps because it is dynamic. LG keeps
compiler state, concrete runtime values, and transport metadata separate.

## Delivery status

Phase 1 is the local Native bytecode REPL. Phase 2 is the loopback Socket REPL
and bundled client. Later follow-ups are accepted directions, not claims about
current commands.

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
| Evaluation IDs and buffered stdout/stderr events | Implemented |
| Socket REPL and bundled socket client | Implemented, Phase 2 |
| Protocol interrupt operation | Excluded from protocol version 1 |
| Baseline nREPL endpoint | Implemented, Phase 3 |
| nREPL `load-file`, `lookup`, and `completions` | Implemented, Phase 3 |
| `neat` compatibility validation | Implemented, Phase 3 |
| `cider-nrepl` middleware compatibility | Excluded |

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
Phase 2 implements the socket transport defined below.

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
process-global mutable state, so multiple independent runtime sessions do not
share one process. The Socket REPL supervisor allocates one worker process per
TCP connection and communicates with it through the REPL message protocol.
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

Phase 2 adds request IDs. Protocol source-location metadata remains a future
extension; current diagnostics use the locations produced for the submitted
form.

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
their compiler-known summary. Phase 2 captures user stdout and stderr and sends
them as events separate from the form's value response.

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

Phase 2 implements a Socket REPL for editor clients and terminal attachment. It
is an LG protocol, not an nREPL implementation. The server supervisor creates
one bytecode worker process per TCP connection and serializes evaluations
within that worker. Separate workers isolate compiler-libs globals, runtime
heaps, crashes, and code accumulation.

The transport is JSON over TCP with a four-byte unsigned big-endian payload
length. Framing is independent of newlines in source or output. Frames larger
than 8 MiB are rejected before payload allocation. Every message carries
protocol version 1 and is decoded immediately into a closed OCaml sum. Requests
are:

```ocaml
type request =
  | Evaluate of source_request
  | Type_of of source_request
  | Lookup of lookup_request
  | Completions of completions_request
  | Describe of { id : string }
  | Close of { id : string }
```

A source request contains a request ID, source text, an optional expected
namespace, and an optional source filename. The expected namespace lets a
client reject stale state instead of silently evaluating in a different
namespace. The filename lets `load-file` preserve compiler locations without
changing evaluation semantics. Lookup and completion requests are closed query
records used by the nREPL adapter; they do not execute source. Responses form a
stream of closed events:

```ocaml
type response =
  | Description of description
  | Stdout of { id : string; text : string }
  | Stderr of { id : string; text : string }
  | Value of value_event
  | Definition of definition_event
  | Summary of summary_event
  | Namespace of namespace_event
  | Type_result of type_event
  | Lookup_result of lookup_event
  | Completions_result of completions_event
  | Diagnostic of diagnostic_event
  | Status of status_event
```

Every accepted request ends with one `Status` while the connection remains
usable after compile or runtime failure. JSON never carries an erased LG
runtime value. `Describe` reports the Native bytecode target, current namespace,
protocol version, and `interrupt_supported = false`.

The current worker buffers stdout and stderr for one evaluation, then emits a
stdout event, a stderr event, the result or diagnostic event, and the final
status. It does not yet preserve live interleaving between stdout and stderr and
does not claim prepl-style real-time streaming. The separation is still
important: printed application text can never corrupt protocol framing or be
confused with a rendered return value.

Protocol version 1 deliberately has no `Interrupt` request. Neither Clojure's
raw `clojure.core.server` REPL nor prepl defines a request-ID interrupt
operation, and the researched ClojureDart loop synchronizes one form with an
acknowledgement rather than implementing independent interruption. LG reports
the absence as a capability instead of exposing an operation that cannot be
implemented correctly. Closing an idle connection ends its worker; an external
supervisor may terminate a stuck worker process.

The server binds to a loopback address by default and accepts `port 0` so the OS
can choose a free port. `--port-file` makes the chosen port available to a
launcher. Non-loopback binds are rejected. A remote workflow must place the
loopback listener behind an authenticated, encrypted tunnel or gateway because
the service provides arbitrary code execution. Idle timeout and additional
resource controls remain future supervisor work.

The server and client entry points are:

```sh
lg repl --listen 127.0.0.1:0 [--state PATH] [--port-file PATH]
lg repl --connect HOST:PORT
```

`lg repl --connect HOST:PORT` is the bundled interactive client. Closing a
client sends `Close`; the terminal's local `:quit` remains a frontend command.
The client performs `Describe` before reading forms, tracks the server-reported
namespace, supports multiline input and `:type`, and renders the same typed
results as the local terminal. A raw netcat-compatible text adapter may be
considered later, but structured framing is the interoperability contract.

### 10. Keep application hot reload out of scope

This ADR does not design or implement attached application REPLs or hot reload
for Melange, Native desktop, iOS, or Android. ClojureDart remains useful prior
art, but LG will make a separate decision if application attachment becomes a
priority. The bytecode REPL must not claim target-app execution semantics.

### 11. Add a separate, standards-based nREPL endpoint

Phase 3 implements nREPL without changing the JSON Socket REPL wire format. A
dedicated listener makes protocol selection explicit; it does not sniff the
first bytes of a connection and does not expose JSON messages under an nREPL
port. The entry point is:

```sh
lg repl --nrepl-listen 127.0.0.1:0 \
  [--state PATH] [--port-file .nrepl-port]
```

The default nREPL transport is bencode over TCP. LG implements bencode through
a closed recursive transport value with byte string, integer, list, and
dictionary constructors. Decoding into an open dynamic map or
`Runtime_dynamic.t` is forbidden. At the operation boundary, each supported
message is validated into a closed request record before it reaches compiler or
runtime code.

The first supported operation set is:

- `describe`, with an exact directory of the operations and LG extensions that
  are actually available;
- `clone`, allocating one isolated bytecode worker and returning its persistent
  nREPL session ID;
- `eval`, reading and evaluating every complete LG form in the request in order
  and returning separate `out`, `err`, and `value` responses followed by
  `status = ["done"]`;
- `load-file`, evaluating every form through the same session path while
  preserving `file-path` or `file-name` and source line information in compiler
  locations;
- `lookup`, resolving a symbol in the named session and returning its source
  name, namespace, static type, and available definition location;
- `completions`, returning prefix-matched candidates and their static types from
  the current incremental compiler environment;
- `close`, terminating the named worker and releasing its runtime heap; and
- `stdin`, returning a defined unsupported-input response until LG exposes a
  real evaluation input boundary.

An unknown operation returns `status = ["unknown-op", "done"]`. Evaluation
responses echo the opaque request `id` and session ID. Successful value
responses include the standard rendered `value` and `ns` fields plus an
optional namespaced `lg/type` field. Completion entries use the standard
`candidate` and `type` fields. Lookup returns an `info` dictionary with
`name`, `ns`, `type`, and any known `file`, `line`, and `column`. Diagnostics use
standard `err`, `ex`, and terminal status fields; transport payloads never
contain an erased LG value.

`lookup` and `completions` reuse LG's compiler-backed Language Service and the
same `Compiler.state` that evaluation advances. They do not maintain a second
index, scan runtime values, enumerate arbitrary Vars, or execute the queried
form. Their optional `ns` selects the static namespace to query without
mutating the session's current namespace. `load-file` is not textual replay
through a terminal frontend: each form uses the ordinary
compile/typecheck/execute/commit transaction, and failed forms do not publish
their candidate compiler state.

nREPL sessions and TCP connections are distinct concepts. Each connection
handler owns a registry from session IDs to worker processes and serializes
messages received on that connection. Different TCP connections progress in
independent handler and worker processes. Closing a connection terminates its
remaining sessions. An evaluation without a session uses an ephemeral worker.
`clone` without a source session creates a fresh worker. Cloning a live LG
session's OCaml heap cannot be implemented faithfully, so a `clone` request
naming an existing session fails explicitly rather than pretending to copy
closures, atoms, or resources.

The endpoint does not advertise `interrupt` or middleware operations. In
particular, the server must not advertise `interrupt` until the supervisor can
terminate an evaluation and leave the named session in a defined state.

Compatibility is validated with the language-agnostic `nrepl/neat` client and
direct protocol integration tests. The acceptance path covers
connect, `describe`, session creation, multiline and multi-form evaluation,
stdout/stderr, namespace tracking, file loading with source locations, typed
lookup and completion, errors followed by another successful eval, unknown
operations, and close. CIDER was not installed in the validation environment.
Its debugging, test, and other advanced features depend on Clojure-specific
`cider-nrepl` middleware and are not an LG compatibility promise.

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

### Replace the LG Socket protocol with nREPL

Rejected. nREPL is valuable as an editor interoperability endpoint, but it does
not replace the smaller closed protocol used by LG's bundled terminal client
and worker boundary. Keeping separate listeners avoids transport sniffing,
keeps the internal static contract small, and lets the nREPL adapter expose only
the operations it implements faithfully.

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
- Socket access is loopback-only because it is an arbitrary-code-execution
  boundary.
- The Phase 2 JSON listener is not nREPL. Phase 3 implements a distinct bencode
  listener and baseline operation set.
- Language-agnostic nREPL compatibility does not imply Clojure runtime or
  `cider-nrepl` middleware compatibility.

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

1. Capture stdout and stderr independently from the result event. Done in Phase
   2 with per-evaluation buffering.
2. Add request IDs and error recovery tests. Done in Phase 2.
3. Add synthetic source locations and local interruption.
4. Add same-type `defn` root replacement and type-change warnings.
5. Add record, variant, and long-session coverage.

### Phase 2: Socket REPL

1. Define and version the closed length-prefixed JSON schema. Done.
2. Add per-connection worker supervision, request IDs, buffered output events,
   an 8 MiB frame limit, port-zero discovery, and loopback-only TCP binding.
   Done.
3. Add `lg repl --connect`, namespace/type support, session isolation, runtime
   error recovery, framing tests, and a non-loopback security test. Done.
4. Add live interleaved output, idle timeout, resource limits, and a truthful
   interruption capability. Deferred and not advertised by protocol version 1.

### Phase 3: nREPL interoperability

1. Implement and fuzz a bounded, incremental bencode codec using a closed
   recursive transport type. The bounded codec and malformed-input tests are
   implemented; generative fuzzing remains a follow-up.
2. Add the separate loopback nREPL listener, session registry, and one bytecode
   worker per persistent session. Done.
3. Implement `describe`, `clone`, `eval`, `close`, defined `stdin` behavior,
   unknown-op responses, ephemeral evaluation, request correlation, and LG's
   namespaced static-type response field. Done.
4. Implement `load-file`, compiler-backed `lookup`, and compiler-backed
   `completions`, including source file and line preservation. Done.
5. Validate the endpoint with protocol tests, direct wire tests, and
   the real `nrepl/neat` client. Done. CIDER-specific behavior remains
   unverified because CIDER was not installed.

Attached application hot reload remains outside this ADR. `cider-nrepl`
middleware emulation is not an implementation phase.

## Validation

Phase 1 and Phase 2 validation cover:

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

Current Socket REPL tests additionally cover runtime exceptions without session
termination; separate stdout, value, diagnostic, and status events; worker
cleanup on close; socket framing and limits; connection isolation; and loopback
enforcement. Future tests cover stderr-producing forms, live interleaving,
interruption, same-type root replacement, type-changing generation isolation,
concurrent active clients, idle cleanup, and restart after worker failure.

Phase 3 validation adds canonical and malformed bencode inputs, bounded decode,
all advertised operation shapes, session/connection lifetime separation,
multi-form eval, `load-file` source attribution, typed lookup and completion,
unknown-op handling, and the language-agnostic `neat` client smoke test. No
CIDER compatibility claim is made beyond behavior that is verified and listed
explicitly.

The focused prototype and production tests are:

```sh
opam exec -- dune runtest prototype/repl
opam exec -- dune exec ./test/repl_session_tests.bc -- \
  ./_build/default/stdlib/lg_stdlib_native.state
opam exec -- dune exec -- bash test/repl_cli_test.sh \
  _build/default/bin/lg_cli.exe \
  _build/default/bin/lg_repl_worker.bc \
  _build/default/stdlib/lg_stdlib_native.state
opam exec -- dune exec ./test/repl_protocol_tests.bc
opam exec -- dune exec -- bash test/repl_socket_cli_test.sh \
  _build/default/bin/lg_cli.exe \
  _build/default/bin/lg_repl_worker.bc \
  _build/default/stdlib/lg_stdlib_native.state
opam exec -- dune exec ./test/repl_nrepl_protocol_tests.bc
opam exec -- dune exec ./test/repl_nrepl_cli_tests.bc -- \
  _build/default/bin/lg_repl_worker.bc \
  _build/default/stdlib/lg_stdlib_native.state
```

`test/repl_neat_client_test.el` is the real-client smoke test. With the
`nrepl/neat` checkout on Emacs' load path and an LG nREPL listener running, set
`LG_NREPL_TEST_PORT` and load that file in batch Emacs.

Dune commands remain serialized with every other Dune operation in the
repository.

## References

- [Clojure REPL and main entry points](https://clojure.org/reference/repl_and_main)
- [Clojure CLI Socket REPL guide](https://clojure.org/guides/deps_and_cli#_using_a_repl)
- [Clojure `core.server` implementation](https://github.com/clojure/clojure/blob/master/src/clj/clojure/core/server.clj)
- [Clojure prepl API](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core.server/prepl)
- [Clojure namespaces](https://clojure.org/reference/namespaces)
- [nREPL protocol specification](https://spec.nrepl.org/)
- [nREPL client and session guidance](https://nrepl.org/nrepl/building_clients.html)
- [nREPL bencode transport](https://nrepl.org/nrepl/design/transports.html)
- [`neat`, the language-agnostic Emacs nREPL client](https://github.com/nrepl/neat)
- [CIDER connection requirements](https://docs.cider.mx/cider/basics/up_and_running.html)
- [ClojureDart host compilation and reload loop](https://github.com/Tensegritics/ClojureDart/blob/2d895b01cd66e8e0ec6750c78d062b09140c94ca/clj/src/cljd/build.clj)
- [ClojureDart per-form recompilation](https://github.com/Tensegritics/ClojureDart/blob/2d895b01cd66e8e0ec6750c78d062b09140c94ca/clj/src/cljd/compiler.cljc)
- [ClojureDart target-side REPL state](https://github.com/Tensegritics/ClojureDart/blob/2d895b01cd66e8e0ec6750c78d062b09140c94ca/clj/src/cljd/flutter.cljd)
- [OCaml toplevel system](https://ocaml.org/manual/5.1/toplevel.html)
- [OCaml Dynlink library](https://ocaml.org/manual/5.1/libdynlink.html)
