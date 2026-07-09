# ClojureDart Reference Notes

ClojureDart is a Clojure dialect targeting Dart and Flutter.

The relevant lesson for cljml is not Dart interop itself.
The lesson is that a Clojure dialect over a typed host needs explicit compiler support for host differences, namespace aliases, symbol munging, and compatibility documentation.

Useful alignment points.

| ClojureDart concern | cljml implication |
| --- | --- |
| Source files stay Clojure-like while targeting Dart. | cljml source should stay Clojure-like while targeting OCaml. |
| `ns` supports host package requires with aliases. | cljml should extend `ns` to support OCaml module/package aliases. |
| Differences from JVM Clojure are documented. | cljml needs a `docs/differences.md` before broad API claims. |
| Symbol munging is documented as an internal compiler concern. | cljml keeps `Names` as the single place for OCaml identifier munging. |
| Reader and compiler are separate subsystems. | cljml keeps `Lexer`, `Parser`, `Toolchain`, `Typecheck`, and `Codegen` separated. |

References used.

- https://github.com/Tensegritics/ClojureDart
- `/tmp/ClojureDart/doc/README.md`
- `/tmp/ClojureDart/doc/differences.md`
- `/tmp/ClojureDart/doc/internals/MUNGING.md`
- `/tmp/ClojureDart/java/src/cljd/lang/LispReader.java`
- `/tmp/ClojureDart/clj/src/cljd/compiler.cljc`
