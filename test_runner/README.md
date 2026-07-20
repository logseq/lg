# lg-test

`lg-test` runs CljML tests written with the common `clojure.test` API.
Native executables use Alcotest, while Melange executables use the same test
registry, assertions, testing contexts, and fixtures with a JavaScript runner.

Supported forms:

- `deftest`
- `is`, including `thrown?` and `thrown-with-msg?`
- `are`
- `testing`
- `use-fixtures` with `:once` and `:each`
- `run-tests`

Compile these sources before the test namespaces:

- `clojure/test.cljc`
- `clojure/test_native.cljc` for Native, or `clojure/test_melange.cljc` for Melange

Compile `clojure/run.cljc` after all test namespaces. Link Native executables
with `lg-test.alcotest` and Melange executables with `lg-test.melange`.

The source files are installed under the `lg-test` package share directory.
