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

module String_set = Set.Make (String)

let ocaml_keyword_set = String_set.of_list ocaml_keywords

let legalize_ocaml_identifier candidate =
  let candidate =
    if candidate = "_" then "__lg_wildcard_value"
    else if candidate = "" then "value_"
    else
      match candidate.[0] with
      | '0' .. '9' -> "value_" ^ candidate
      | _ -> candidate
  in
  if String_set.mem candidate ocaml_keyword_set then candidate ^ "_"
  else candidate

let sanitized_names = Hashtbl.create 4096

let sanitize_name name =
  match Hashtbl.find_opt sanitized_names name with
  | Some sanitized -> sanitized
  | None ->
      let buffer = Buffer.create (String.length name) in
      String.iter
        (function
          | 'a' .. 'z' as ch -> Buffer.add_char buffer ch
          | 'A' .. 'Z' as ch -> Buffer.add_char buffer (Char.lowercase_ascii ch)
          | '0' .. '9' as ch -> Buffer.add_char buffer ch
          | '_' -> Buffer.add_char buffer '_'
          | '!' -> Buffer.add_string buffer "_bang"
          | '\'' -> Buffer.add_string buffer "_prime"
          | '-' | '?' | '/' | '.' -> Buffer.add_char buffer '_'
          | _ -> Buffer.add_char buffer '_')
        name;
      let sanitized = Buffer.contents buffer |> legalize_ocaml_identifier in
      Hashtbl.add sanitized_names name sanitized;
      sanitized

let is_ocaml_operator_name name =
  let is_operator_character = function
    | '=' | '<' | '>' | '@' | '^' | '|' | '&' | '+' | '-' | '*' | '/'
    | '$' | '%' | '#' | '!' | '?' | '~' | ':' | '.' -> true
    | _ -> false
  in
  String.length name > 0 && String.for_all is_operator_character name

let ocaml_member_name name =
  if is_ocaml_operator_name name then name else sanitize_name name

let keyword_source_name keyword =
  if String.length keyword > 0 && keyword.[0] = ':' then
    String.sub keyword 1 (String.length keyword - 1)
  else keyword

let keyword_to_ocaml_name keyword =
  sanitize_name (keyword_source_name keyword)

let is_qualified name = String.contains name '/'

let scoped_keys = Hashtbl.create 4096

let scoped_key scope name =
  if is_qualified name || scope = "" then name
  else
    let key = (scope, name) in
    match Hashtbl.find_opt scoped_keys key with
    | Some scoped -> scoped
    | None ->
        let scoped = scope ^ "/" ^ name in
        Hashtbl.add scoped_keys key scoped;
        scoped

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
  if String.length name <= 96 then name
  else String.sub name 0 80 ^ "_" ^ compact_digest name

let ocaml_binding_names = Hashtbl.create 4096

let operator_binding_name = function
  | "+" -> Some "add"
  | "+'" -> Some "add_prime"
  | "-" -> Some "subtract"
  | "*" -> Some "multiply"
  | "*'" -> Some "multiply_prime"
  | "/" -> Some "divide"
  | "<" -> Some "less"
  | "<=" -> Some "less_equal"
  | ">" -> Some "greater"
  | ">=" -> Some "greater_equal"
  | "=" -> Some "equal"
  | "==" -> Some "numeric_equal"
  | _ -> None

let ocaml_binding_name scope name =
  let key = (scope, name) in
  match Hashtbl.find_opt ocaml_binding_names key with
  | Some binding -> binding
  | None ->
      let candidate =
        let readable_name =
          operator_binding_name name |> Option.value ~default:name
        in
        if scope = "" then sanitize_name readable_name
        else sanitize_name (scope ^ "_" ^ readable_name)
      in
      let binding =
        if
          scope <> ""
          &&
          match scope.[0] with 'a' .. 'z' -> true | _ -> false
        then compact_source_binding candidate
        else candidate
      in
      Hashtbl.add ocaml_binding_names key binding;
      binding

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

let replace_runtime_path_prefix aliases name =
  let rec replace = function
    | [] -> name
    | (prefix, alias) :: rest ->
        if String.equal name prefix then alias
        else
          let dotted_prefix = prefix ^ "." in
          if String.starts_with ~prefix:dotted_prefix name then
            alias
            ^ String.sub name (String.length prefix)
                (String.length name - String.length prefix)
          else replace rest
  in
  replace aliases

let compact_runtime_path name =
  replace_runtime_path_prefix compact_runtime_aliases name

let canonical_runtime_path name =
  compact_runtime_aliases
  |> List.map (fun (canonical, compact) -> (compact, canonical))
  |> fun aliases -> replace_runtime_path_prefix aliases name

let replace_runtime_path source pattern replacement =
  let pattern_length = String.length pattern in
  let source_length = String.length source in
  let buffer = Buffer.create source_length in
  let identifier_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false
  in
  let rec loop index =
    if index >= source_length then Buffer.contents buffer
    else if
      index + pattern_length <= source_length
      && String.sub source index pattern_length = pattern
      &&
      let following = index + pattern_length in
      following = source_length
      || source.[following] = '.'
      || not (identifier_char source.[following])
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
    (fun source (prefix, alias) -> replace_runtime_path source prefix alias)
    source compact_runtime_aliases

let compact_generated_name name =
  name

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
