type t = Native | Melange | Js_of_ocaml

let default = Native

let to_string = function
  | Native -> "native"
  | Melange -> "melange"
  | Js_of_ocaml -> "js-of-ocaml"

let feature target = ":" ^ to_string target

let of_string = function
  | "native" -> Ok Native
  | "melange" -> Ok Melange
  | "js-of-ocaml" -> Ok Js_of_ocaml
  | value ->
      Error
        (Printf.sprintf
           "unknown target %s; expected native, melange, or js-of-ocaml" value)
