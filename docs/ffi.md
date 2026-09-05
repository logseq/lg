# Foreign function bindings

Declare a foreign function with `ffi`, then call it like an ordinary LG function:

```clojure
(ffi c-abs [:int] :int {:native "abs"})
(c-abs -42)
```

The declaration is `(ffi name [argument-types ...] result-type options)`.
All types are explicit. Use `[]` for a zero-argument function. Bindings can be
aliased, exported from a module, and passed as typed function values.

## Native C

The current implementation supports Native through ctypes. The function name
is a literal C symbol. Add `:library "/path/to/library.so"` (or the platform's
library filename) to select a shared library. Without it, ctypes searches the
current process. The library and symbol are resolved when the binding is
initialized.

| LG type | C ABI type | Supported positions |
| --- | --- | --- |
| `:int` | `int` | Argument and result |
| `:float` | `double` | Argument and result |
| `:bool` | `_Bool` | Argument and result |
| `:string` | NUL-terminated string | Input only; C must not retain or modify it |
| `:unit` | `void` | Result only |

LG does not infer a C signature from the symbol name. The declaration must
match the library's actual ABI. In particular, C `long`, `size_t`, and `float`
are not aliases for the `int` and `double` mappings above. Pointers and callbacks
use the explicit lifetime forms below. Returned-string copying, struct layout,
and C variadic calls require a typed OCaml adapter.

The compiler discovers `ctypes-foreign` and `lg.ffi` as required packages.
The CLI run path uses discovered packages; when compiling generated OCaml in
your own Dune executable, include both in `libraries` alongside the normal LG
runtime dependencies. In this repository, `dune build @install` prepares the
local package artifacts.

```sh
dune exec lg -- --run examples/ffi_native.cljc
```

Expected output: `42`.

## Target selection and implementation status

Melange supports global function calls, imported module functions, and nested
scopes with scalar argument/result types:

```clojure
(ffi floor [:float] :float {:js "floor" :scope ["Math"]})
(ffi basename [:string] :string {:js "basename" :module "node:path"})
(ffi posix-basename [:string] :string
  {:js "basename" :module "node:path" :scope ["posix"]})
```

JavaScript supports `:int`, `:float`, `:bool`, and `:string` arguments and
results, plus `:unit` results. These declarations do not load ctypes. Generated
OCaml must be preprocessed with `melange.ppx` when compiling with Melange:

```sh
dune exec lg -- --target melange examples/ffi_js.cljc -o /tmp/ffi_js.ml
melc --ppx "$(ocamlfind query melange.ppx)/ppx.exe --as-ppx" \
  --mel-module-type commonjs -o /tmp/ffi_js.js /tmp/ffi_js.ml
node /tmp/ffi_js.js
```

Expected output: `example.txt`, then `3` on the next line. In a Dune
`melange.emit` stanza, use `(preprocess (pps melange.ppx))`.

Declare opaque objects and their operations explicitly:

```clojure
(extern-type number-map)
(ffi make-map [] :number-map {:js "Map" :kind :new})
(ffi put! [:number-map :string :int] :number-map {:js "set" :kind :send})
(ffi size [:number-map] :int {:js "size" :kind :get})
(ffi lookup [:number-map :string] :option<int>
  {:js "get" :kind :send :return :undefined})

(def entries (make-map))
(put! entries "answer" 42)
(def answer (lookup entries "answer"))
```

Different `extern-type` declarations are distinct static types. `:send` passes
the first argument as `this`. `:get` reads a named property at call time;
`:set` accepts receiver/value and returns unit. Receiver operations cannot use
`:module` or `:scope`.

Indexed reads use `{:js :get-index}` and writes use `{:js :set-index}`. Opaque
objects accept integer or string indices. Arrays use integer indices and retain
their element type. For an out-of-bounds array read, declare an option result
and add `:return :undefined`.

`:return :null`, `:return :undefined`, and `:return :nullable` convert their
respective absence values to an option. The last handles both null and undefined.
An option result requires an explicit adapter; nested options require a separate
host adapter.

Direct callback parameters use `:fn<...>` signatures and are uncurried at the
boundary, including `:fn<int>` for a zero-argument callback returning an integer.
Arrays use `:array<T>`. `:variadic true` spreads the final array for a call,
constructor, or method; it does not change the source function's fixed arity.
Use Dune `melange.emit` for applications using adapters, so the required Melange
runtime JavaScript modules are emitted automatically.

Native bindings are rejected on Melange and js_of_ocaml. Use a reader
conditional when including Native-only declarations in a shared file:

```clojure
#?(:native (ffi c-abs [:int] :int {:native "abs"}))
```

Build JavaScript objects from explicitly typed records:

```clojure
(type-record request-options
  (timeout :int)
  (label :option<string>))
(extern-type js-options)
(ffi make-options [:request-options] :js-options
  {:js :object :rename {:timeout "timeoutMs"} :optional [:label]})

(def options (make-options (request-options. 500 nil)))
```

This produces a fresh object with `timeoutMs: 500` and no `label` property.
Present optional values become ordinary property values. All other record
fields are required and use their source names unless renamed. The input record
is preserved. Unknown fields, duplicate property names, and the special literal
key `__proto__` are rejected. Field values support scalars, homogeneous arrays,
and opaque host types; records are not recursively converted to JS objects.

[ADR 011](agent-guide/011-foreign-function-interface.md) records the ABI and
lifetime decisions behind the following native forms.

Native functions that invoke callbacks synchronously can declare the lifetime
explicitly:

```clojure
(ffi visit [:fn<int;unit>] :unit
  {:native "visit" :library "./libexample.so" :callbacks :call})
```

The C function must finish using the callback before it returns. Callback
signatures support scalar arguments and scalar or void results, including
zero-argument functions such as `:fn<int>`. Calls keep the runtime lock and
callbacks run on the calling thread. Nested callbacks, returned function
pointers, and retained callbacks are not accepted by this declaration.

Use a typed handle when C retains a callback after registration returns:

```clojure
(ffi retain [:fn<int;int>] :callback<fn<int;int>> {:native :callback})
(ffi register [:callback<fn<int;int>>] :unit {:native "register_callback"})
(ffi unregister [] :unit {:native "unregister_callback"})
(ffi close-callback [:callback<fn<int;int>>] :unit {:native :release})

(def callback (retain (fn [x] (+ x 1))))
(register callback)
;; Later, after C has stopped using the callback:
(unregister)
(close-callback callback)
```

The handle keeps the closure alive across GC. Release it only after C has
unregistered it and any calls have finished. Releasing twice is harmless;
passing a released handle into C raises an error. Callbacks still run on the
calling OCaml thread; foreign-thread invocation is not enabled by this API.

Borrowed pointers preserve their storage type:

```clojure
(ffi current-counter [] :pointer<int>
  {:native "current_counter" :ownership :borrowed})
(ffi read-counter [:pointer<int>] :int {:native "read_counter"})
(ffi find-counter [:int] :option<pointer<int>>
  {:native "find_counter" :ownership :borrowed})
```

Use the option form when C can return NULL; it converts NULL to nil and accepts
nil for nullable pointer arguments. A plain pointer result declares that C
returns a non-NULL pointer. `:ownership :borrowed` means the C owner keeps the
storage alive; LG does not free it. Do not use it for an allocation requiring
release. Pointer types can nest and their pointees may be int, float (C double),
bool, char, or unit (C void). String, collection, and function pointees require
a dedicated adapter.

Owned pointers use a distinct handle type and name the matching C deallocator:

```clojure
(ffi allocate [:int] :owned-pointer<int>
  {:native "allocate_counter" :library "./libcounter.so"
   :ownership :owned :release "free_counter"})
(ffi read-owned [:owned-pointer<int>] :int
  {:native "read_counter" :library "./libcounter.so"})
(ffi close [:owned-pointer<int>] :unit {:native :release})

(def counter (allocate 42))
(try (read-owned counter) (finally (close counter)))
```

The deallocator must have signature `T* -> void` and come from the same library
as the allocation function. NULL allocation results raise an error. Explicit
release is idempotent, and using a released handle fails before entering C.
Use `try`/`finally` for exception-safe release; handles do not rely on a GC
finalizer. C must not retain the borrowed address beyond the handle's lifetime.
Generated native bindings depend on the `lg.ffi` runtime and ctypes-foreign.

## Timing and printing

Programs compiled with the source standard library can use Clojure timing and
printing alongside foreign calls:

```clojure
(def answer (time (+ 20 22)))
(println "Answer:" answer)
(prn "readable output")
(def captured (with-out-str (print "hello") (newline)))
```

`time` evaluates once, prints elapsed milliseconds, and returns the expression's
value. Native measures monotonic elapsed time, including waits. Both JavaScript
targets use a monotonic performance clock and ClojureScript's six fractional
digits. An exception propagates without printing a timing line.

`print`, `println`, `pr`, `prn`, `newline`, and `flush` return nil. The corresponding
`print-str`, `println-str`, `pr-str`, and `prn-str` functions return strings.
`with-out-str` captures direct printing and timing output. `format` and `printf`
now accept runtime format strings and statically typed arguments:

```clojure
(format "%s:%04d:%.2f" "item" 7 2.5)
(printf "%s%n" "done")
```

They expand at compile time to preserve heterogeneous argument types; define a
function with concrete parameter types when a first-class formatter is needed.
`printf` returns nil and respects `with-out-str`. Scalar conversion, decimal
rounding, Unicode width/precision, optional values, and output capture are tested
on Native, Melange, and js_of_ocaml. Supported conversions are `s`, `b`, `c`,
`d`, `o`, `x`, `e`, `f`, `g`, their applicable uppercase forms, `%`, and `n`.
JVM date objects, object hashes, and Formattable dispatch are not available.

Run the timing example with the precompiled standard library:

```sh
dune exec lg -- --run-from _build/default/stdlib/lg_stdlib_native.state \
  _build/default/stdlib/lg_stdlib_native.ml examples/time_print.cljc
```
