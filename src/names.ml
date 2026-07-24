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

let compact_digest name =
  let digest = Digest.string name in
  let value = ref 0L in
  for index = 0 to 3 do
    value :=
      Int64.logor (Int64.shift_left !value 8)
        (Int64.of_int (Char.code digest.[index]))
  done;
  let alphabet =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  in
  let encoded = Bytes.make 6 'a' in
  for index = 5 downto 0 do
    let digit = Int64.to_int (Int64.rem !value 62L) in
    Bytes.set encoded index alphabet.[digit];
    value := Int64.div !value 62L
  done;
  Bytes.unsafe_to_string encoded

let compact_source_binding name =
  if String.length name > 24 then
    "g" ^ compact_digest name
  else name

let ocaml_binding_name scope name =
  let candidate =
    if scope = "" then sanitize_name name
    else sanitize_name (scope ^ "_" ^ name)
  in
  if
    scope <> ""
    &&
    match scope.[0] with 'a' .. 'z' -> true | _ -> false
  then compact_source_binding candidate
  else candidate

let module_segment_to_ocaml name =
  let sanitized = sanitize_name name in
  if sanitized = "" then "Module_"
  else
    String.make 1 (Char.uppercase_ascii sanitized.[0])
    ^ String.sub sanitized 1 (String.length sanitized - 1)

let module_path_to_ocaml path =
  path |> String.split_on_char '.' |> List.map module_segment_to_ocaml
  |> String.concat "."

let compact_runtime_aliases =
  [
    ("Lg_runtime.Runtime_seq", "Lg_runtime.Lg_seq");
    ("Lg_runtime.Runtime_map", "Lg_runtime.Lg_map");
    ("Lg_runtime.Runtime_exception", "Lg_runtime.Lg_exn");
    ("Lg_runtime.Core_set", "Lg_runtime.Lg_set");
  ]

let compact_runtime_path name =
  let rec compact = function
    | [] -> name
    | (prefix, alias) :: rest ->
        if String.equal name prefix then alias
        else
          let dotted_prefix = prefix ^ "." in
          if String.starts_with ~prefix:dotted_prefix name then
            alias
            ^ String.sub name (String.length prefix)
                (String.length name - String.length prefix)
          else compact rest
  in
  compact compact_runtime_aliases

let replace_all source pattern replacement =
  let pattern_length = String.length pattern in
  let source_length = String.length source in
  let buffer = Buffer.create source_length in
  let rec loop index =
    if index >= source_length then Buffer.contents buffer
    else if
      index + pattern_length <= source_length
      && String.sub source index pattern_length = pattern
    then (
      Buffer.add_string buffer replacement;
      loop (index + pattern_length))
    else (
      Buffer.add_char buffer source.[index];
      loop (index + 1))
  in
  loop 0

let compact_runtime_source source =
  List.fold_left
    (fun source (prefix, alias) -> replace_all source prefix alias)
    source compact_runtime_aliases

let compact_generated_name name =
  if String.starts_with ~prefix:"__lg_" name then
    "l" ^ compact_digest name
  else name

let compact_generated_source source =
  let source_length = String.length source in
  let is_identifier_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let buffer = Buffer.create source_length in
  let rec loop index =
    if index >= source_length then Buffer.contents buffer
    else if
      index + 5 <= source_length && String.sub source index 5 = "__lg_"
    then
      let rec identifier_end cursor =
        if cursor < source_length && is_identifier_char source.[cursor] then
          identifier_end (cursor + 1)
        else cursor
      in
      let end_index = identifier_end (index + 5) in
      let name = String.sub source index (end_index - index) in
      Buffer.add_string buffer (compact_generated_name name);
      loop end_index
    else (
      Buffer.add_char buffer source.[index];
      loop (index + 1))
  in
  loop 0
