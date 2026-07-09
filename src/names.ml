let sanitize_name name =
  let buffer = Buffer.create (String.length name) in
  String.iter
    (function
      | 'a' .. 'z' as ch -> Buffer.add_char buffer ch
      | 'A' .. 'Z' as ch -> Buffer.add_char buffer (Char.lowercase_ascii ch)
      | '0' .. '9' as ch -> Buffer.add_char buffer ch
      | '_' -> Buffer.add_char buffer '_'
      | '-' | '?' | '!' | '/' | '.' -> Buffer.add_char buffer '_'
      | _ -> Buffer.add_char buffer '_')
    name;
  let candidate = Buffer.contents buffer in
  if candidate = "" then "value_" else candidate

let keyword_to_ocaml_name keyword =
  let raw =
    if String.length keyword > 0 && keyword.[0] = ':' then
      String.sub keyword 1 (String.length keyword - 1)
    else keyword
  in
  sanitize_name raw

let has_namespace name = String.contains name '/'

let namespaced_key current_ns name =
  if has_namespace name || current_ns = "" then name else current_ns ^ "/" ^ name

let ocaml_binding_name current_ns name =
  if current_ns = "" then sanitize_name name
  else sanitize_name (current_ns ^ "_" ^ name)
