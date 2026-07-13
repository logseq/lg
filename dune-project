(lang dune 3.0)

(name cljml)

(generate_opam_files true)

(package
 (name cljml)
 (maintainers "Tienson Qin <tiensonqin@gmail.com>")
 (authors "Tienson Qin")
 (synopsis "Clojure syntax for OCaml")
 (description
  "A statically typed Clojure-syntax frontend that emits checked OCaml Parsetree.")
 (depends
  (ocaml (>= 5.4))
  (dune (>= 3.0))
  yojson))
