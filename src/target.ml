type t = Native | Melange | Js_of_ocaml

let default = Native

let to_string = function
  | Native -> "native"
  | Melange -> "melange"
  | Js_of_ocaml -> "js"

let feature target = ":" ^ to_string target

let reader_features target =
  match target with
  | Native -> [ feature target; ":clj" ]
  | Melange -> [ feature target; ":cljs" ]
  | Js_of_ocaml -> [ feature target; ":js-of-ocaml"; ":cljs" ]

let reader_dialect_feature = function
  | Native -> ":clj"
  | Melange | Js_of_ocaml -> ":cljs"

let of_string = function
  | "native" -> Ok Native
  | "melange" -> Ok Melange
  | "js" | "js-of-ocaml" -> Ok Js_of_ocaml
  | value ->
      Error
        (Printf.sprintf
           "unknown target %s; expected native, melange, or js" value)
