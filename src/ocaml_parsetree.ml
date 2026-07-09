let parse_implementation source =
  let lexbuf = Lexing.from_string source in
  try Ok (Parse.implementation lexbuf)
  with exn ->
    Error.error ("generated OCaml did not parse: " ^ Printexc.to_string exn)

let print_implementation structure =
  Format.asprintf "%a@." Pprintast.structure structure
