(lang dune 3.17)
(using melange 0.1)

(name lg)

(generate_opam_files true)

(package
 (name lg)
 (maintainers "Tienson Qin <tiensonqin@gmail.com>")
 (authors "Tienson Qin")
 (synopsis "Clojure syntax for OCaml")
 (description
  "A statically typed Clojure-syntax frontend that emits checked OCaml Parsetree.")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.17))
  (melange (>= 6.0))
  (js_of_ocaml (>= 6.0))
  re
  melange-edn-core
  melange-edn-native
  melange-edn-melange
  melange-transit-native
  melange-transit-melange
  yojson))

(package
 (name lg-test)
 (maintainers "Tienson Qin <tiensonqin@gmail.com>")
 (authors "Tienson Qin")
 (synopsis "Clojure-compatible test runner for CljML")
 (description
  "A Clojure.test-compatible test runner with Alcotest and Melange backends.")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.17))
  (melange (>= 6.0))
  alcotest
  lg))

(package
 (name datascript)
 (maintainers "Tienson Qin <tiensonqin@gmail.com>")
 (authors "Tienson Qin")
 (synopsis "DataScript runtime support for CljML")
 (description
  "Closed DataScript domain types and Native/Melange boundary adapters for CljML.")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.17))
  (melange (>= 6.0))
  lg))
