let ocaml_keywords =
  [
    "and";
    "as";
    "assert";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "lazy";
    "let";
    "match";
    "method";
    "mod";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
  ]

let legalize_ocaml_identifier candidate =
  let candidate =
    if candidate = "_" then "__lg_wildcard_value"
    else if candidate = "" then "value_"
    else
      match candidate.[0] with
      | '0' .. '9' -> "value_" ^ candidate
      | _ -> candidate
  in
  if List.mem candidate ocaml_keywords then candidate ^ "_" else candidate

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
  legalize_ocaml_identifier candidate

let keyword_source_name keyword =
  if String.length keyword > 0 && keyword.[0] = ':' then
    String.sub keyword 1 (String.length keyword - 1)
  else keyword

let keyword_to_ocaml_name keyword =
  sanitize_name (keyword_source_name keyword)

let is_qualified name = String.contains name '/'

let scoped_key scope name =
  if is_qualified name || scope = "" then name else scope ^ "/" ^ name

let ocaml_binding_name scope name =
  if scope = "" then sanitize_name name
  else sanitize_name (scope ^ "_" ^ name)

let module_segment_to_ocaml name =
  let sanitized = sanitize_name name in
  if sanitized = "" then "Module_"
  else
    String.make 1 (Char.uppercase_ascii sanitized.[0])
    ^ String.sub sanitized 1 (String.length sanitized - 1)

let module_path_to_ocaml path =
  path |> String.split_on_char '.' |> List.map module_segment_to_ocaml
  |> String.concat "."
