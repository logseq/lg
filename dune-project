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
  yojson))
