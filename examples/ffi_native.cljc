(require [ocaml.Stdlib :as io])

(ffi c-abs [:int] :int {:native "abs"})

(io/print-endline (io/string-of-int (c-abs -42)))
