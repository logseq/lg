let is_whitespace = function
  | ' ' | '\n' | '\r' | '\t' | '\012' -> true
  | _ -> false

let blank source = String.trim source = ""
let trim source = String.trim source
let lower_case source = String.lowercase_ascii source
let upper_case source = String.uppercase_ascii source
let ends_with source suffix = String.ends_with ~suffix source

let index_of_from source needle start =
  let needle_len = String.length needle in
  let source_len = String.length source in
  let start = max 0 (min source_len start) in
  if needle_len = 0 then start
  else
    let rec search index =
      if index + needle_len > source_len then -1
      else if String.sub source index needle_len = needle then index
      else search (index + 1)
    in
    search start

let index_of source needle = index_of_from source needle 0

let includes source needle = index_of source needle >= 0
let index_of_int source needle = index_of source needle
let join separator (to_seq, values) =
  String.concat separator (List.of_seq (to_seq values))

let join_seq separator values =
  String.concat separator (List.of_seq values)

let last_index_of_from source needle start =
  let needle_len = String.length needle in
  let source_len = String.length source in
  let start = min source_len start in
  if start < 0 then -1
  else if needle_len = 0 then start
  else
    let rec search index =
      if index < 0 then -1
      else if String.sub source index needle_len = needle then index
      else search (index - 1)
    in
    search (min start (source_len - needle_len))

let last_index_of source needle =
  last_index_of_from source needle (String.length source)

let last_index_of_int source needle = last_index_of source needle

let length source = String.length source

let char_of_int code =
  if code < 0 || code > 255 then
    invalid_arg "Argument to char must be an 8-bit character code"
  else Char.chr code

let char_of_string source =
  if String.length source = 1 then source.[0]
  else invalid_arg "Argument to char must be a character or number"

let utf8_scalar_at source index =
  let source_length = String.length source in
  if index < 0 || index >= source_length then
    invalid_arg "UTF-8 byte index is out of bounds";
  let byte offset = Char.code source.[index + offset] in
  let continuation offset =
    if index + offset >= source_length then invalid_arg "truncated UTF-8 string";
    let value = byte offset in
    if value land 0xc0 <> 0x80 then invalid_arg "invalid UTF-8 continuation";
    value land 0x3f
  in
  let leading = byte 0 in
  if leading land 0x80 = 0 then (leading, index + 1)
  else if leading land 0xe0 = 0xc0 then
    (((leading land 0x1f) lsl 6) lor continuation 1, index + 2)
  else if leading land 0xf0 = 0xe0 then
    ( ((leading land 0x0f) lsl 12)
      lor (continuation 1 lsl 6)
      lor continuation 2,
      index + 3 )
  else if leading land 0xf8 = 0xf0 then
    ( ((leading land 0x07) lsl 18)
      lor (continuation 1 lsl 12)
      lor (continuation 2 lsl 6)
      lor continuation 3,
      index + 4 )
  else invalid_arg "invalid UTF-8 leading byte"

let utf16_length source =
  let rec loop byte_index length =
    if byte_index = String.length source then length
    else
      let scalar, next_byte = utf8_scalar_at source byte_index in
      loop next_byte (length + if scalar <= 0xffff then 1 else 2)
  in
  loop 0 0

let utf16_code_units source =
  let units = Array.make (utf16_length source) 0 in
  let rec loop byte_index unit_index =
    if byte_index = String.length source then units
    else
      let scalar, next_byte = utf8_scalar_at source byte_index in
      if scalar <= 0xffff then (
        units.(unit_index) <- scalar;
        loop next_byte (unit_index + 1))
      else
        let supplementary = scalar - 0x10000 in
        units.(unit_index) <- 0xd800 lor (supplementary lsr 10);
        units.(unit_index + 1) <- 0xdc00 lor (supplementary land 0x3ff);
        loop next_byte (unit_index + 2)
  in
  loop 0 0

let char_code_of_char value = Char.code value

let char_code_of_string source =
  let units = utf16_code_units source in
  if Array.length units = 1 then units.(0)
  else invalid_arg "Argument to char-code must be a single UTF-16 code unit"

let substring_from source start =
  String.sub source start (String.length source - start)

let substring_range source start stop =
  String.sub source start (stop - start)

let replace source match_value replacement =
  let match_len = String.length match_value in
  if match_len = 0 then source
  else
    let buffer = Buffer.create (String.length source) in
    let rec loop index =
      if index >= String.length source then ()
      else if
        index + match_len <= String.length source
        && String.sub source index match_len = match_value
      then (
        Buffer.add_string buffer replacement;
        loop (index + match_len))
      else (
        Buffer.add_char buffer source.[index];
        loop (index + 1))
    in
    loop 0;
    Buffer.contents buffer

let replace_first source match_value replacement =
  let match_len = String.length match_value in
  if match_len = 0 then source
  else
    let rec search index =
      if index + match_len > String.length source then source
      else if String.sub source index match_len = match_value then
        String.sub source 0 index ^ replacement
        ^ String.sub source (index + match_len)
            (String.length source - index - match_len)
      else search (index + 1)
    in
    search 0

let regex_prefix = "\000lg-regex:"

let regex_source expression =
  if String.starts_with ~prefix:regex_prefix expression then
    String.sub expression
      (String.length regex_prefix)
      (String.length expression - String.length regex_prefix)
  else invalid_arg "expected an LG regular expression"

let regex_parts expression =
  let source = regex_source expression in
  if String.starts_with ~prefix:"(?" source then
    match String.index_from_opt source 2 ')' with
    | Some close ->
        let flags = String.sub source 2 (close - 2) in
        if
          String.for_all
            (function 'i' | 'd' | 'm' | 's' | 'u' | 'x' -> true | _ -> false)
            flags
        then
          ( String.sub source (close + 1) (String.length source - close - 1),
            flags )
        else (source, "")
    | None -> (source, "")
  else (source, "")

let regex_pattern expression = fst (regex_parts expression)

let regex expression =
  let tagged = regex_prefix ^ expression in
  let pattern, flags = regex_parts tagged in
  let _ = Lg_edn_backend.regex_valid_with_flags ~pattern ~flags in
  tagged

let regex_captures match_ =
  Array.to_list match_.Lg_edn_backend.captures

let regex_find_groups expression source =
  let pattern, flags = regex_parts expression in
  Lg_edn_backend.regex_find_groups_with_flags ~pattern ~flags source
  |> Option.map regex_captures

let regex_matches_groups expression source =
  let pattern, flags = regex_parts expression in
  Lg_edn_backend.regex_matches_groups_with_flags ~pattern ~flags source
  |> Option.map regex_captures

let regex_first_match groups =
  match groups with match_value :: _ -> match_value | [] -> None

let regex_find_match expression source =
  Option.bind (regex_find_groups expression source) regex_first_match

let regex_matches_match expression source =
  Option.bind (regex_matches_groups expression source) regex_first_match

let regex_find_group_vector expression source =
  regex_find_groups expression source |> Option.map Rrbvec.of_list

let regex_matches_group_vector expression source =
  regex_matches_groups expression source |> Option.map Rrbvec.of_list

let timestamp_pattern =
  regex
    "(\\d\\d\\d\\d)(?:-(\\d\\d)(?:-(\\d\\d)(?:[T](\\d\\d)(?::(\\d\\d)(?::(\\d\\d)(?:[.](\\d+))?)?)?)?)?)?(?:[Z]|([-+])(\\d\\d):(\\d\\d))?"

let timestamp_captures source = regex_matches_groups timestamp_pattern source

let escape_regex_literal source =
  let buffer = Buffer.create (String.length source) in
  String.iter
    (fun ch ->
      (match ch with
      | '\\' | '.' | '*' | '+' | '?' | '[' | ']' | '(' | ')' | '{' | '}'
      | '^' | '$' | '|' ->
          Buffer.add_char buffer '\\'
      | _ -> ());
      Buffer.add_char buffer ch)
    source;
  Buffer.contents buffer

let split_with_limit source separator limit =
  let pattern, flags =
    if String.starts_with ~prefix:regex_prefix separator then
      regex_parts separator
    else (escape_regex_literal separator, "")
  in
  Lg_edn_backend.regex_split_with_flags ~pattern ~flags ~limit:(Some limit)
    source
  |> Array.to_list
  |> List.map (Option.value ~default:"")
  |> Rrbvec.of_list

let split source separator = split_with_limit source separator 0

let split_lines source =
  let length = String.length source in
  let rec loop acc start index =
    if index >= length then
      let acc =
        if start < length || length = 0 then
          String.sub source start (length - start) :: acc
        else acc
      in
      List.rev acc
    else if source.[index] = '\n' then
      let stop =
        if index > start && source.[index - 1] = '\r' then index - 1 else index
      in
      loop
        (String.sub source start (stop - start) :: acc)
        (index + 1) (index + 1)
    else loop acc start (index + 1)
  in
  Rrbvec.of_list (loop [] 0 0)

let triml source =
  let rec first index =
    if index >= String.length source then String.length source
    else if is_whitespace source.[index] then first (index + 1)
    else index
  in
  let start = first 0 in
  String.sub source start (String.length source - start)

let trimr source =
  let rec last index =
    if index < 0 then -1
    else if is_whitespace source.[index] then last (index - 1)
    else index
  in
  let stop = last (String.length source - 1) in
  if stop < 0 then "" else String.sub source 0 (stop + 1)

let trim_newline source =
  let rec stop index =
    if index < 0 then -1
    else
      match source.[index] with '\n' | '\r' -> stop (index - 1) | _ -> index
  in
  let last = stop (String.length source - 1) in
  if last < 0 then "" else String.sub source 0 (last + 1)

let capitalize source =
  if source = "" then ""
  else
    String.uppercase_ascii (String.sub source 0 1)
    ^ String.lowercase_ascii (String.sub source 1 (String.length source - 1))

let reverse source =
  String.of_seq (List.to_seq (List.rev (List.of_seq (String.to_seq source))))

let escape source replacements =
  let buffer = Buffer.create (String.length source) in
  String.iter
    (fun character ->
      match Runtime_map.get_option replacements character with
      | Some replacement -> Buffer.add_string buffer replacement
      | None -> Buffer.add_char buffer character)
    source;
  Buffer.contents buffer

let munge_str source =
  let replacement = function
    | '-' -> Some "_"
    | ':' -> Some "_COLON_"
    | '+' -> Some "_PLUS_"
    | '>' -> Some "_GT_"
    | '<' -> Some "_LT_"
    | '=' -> Some "_EQ_"
    | '~' -> Some "_TILDE_"
    | '!' -> Some "_BANG_"
    | '@' -> Some "_CIRCA_"
    | '#' -> Some "_SHARP_"
    | '\'' -> Some "_SINGLEQUOTE_"
    | '"' -> Some "_DOUBLEQUOTE_"
    | '%' -> Some "_PERCENT_"
    | '^' -> Some "_CARET_"
    | '&' -> Some "_AMPERSAND_"
    | '*' -> Some "_STAR_"
    | '|' -> Some "_BAR_"
    | '{' -> Some "_LBRACE_"
    | '}' -> Some "_RBRACE_"
    | '[' -> Some "_LBRACK_"
    | ']' -> Some "_RBRACK_"
    | '/' -> Some "_SLASH_"
    | '\\' -> Some "_BSLASH_"
    | '?' -> Some "_QMARK_"
    | _ -> None
  in
  let buffer = Buffer.create (String.length source) in
  String.iter
    (fun character ->
      match replacement character with
      | Some encoded -> Buffer.add_string buffer encoded
      | None -> Buffer.add_char buffer character)
    source;
  Buffer.contents buffer

let javascript_reserved_words =
  [
    "arguments";
    "abstract";
    "await";
    "boolean";
    "break";
    "byte";
    "case";
    "catch";
    "char";
    "class";
    "const";
    "continue";
    "debugger";
    "default";
    "delete";
    "do";
    "double";
    "else";
    "enum";
    "export";
    "extends";
    "final";
    "finally";
    "float";
    "for";
    "function";
    "goto";
    "if";
    "implements";
    "import";
    "in";
    "instanceof";
    "int";
    "interface";
    "let";
    "long";
    "native";
    "new";
    "package";
    "private";
    "protected";
    "public";
    "return";
    "short";
    "static";
    "super";
    "switch";
    "synchronized";
    "this";
    "throw";
    "throws";
    "transient";
    "try";
    "typeof";
    "var";
    "void";
    "volatile";
    "while";
    "with";
    "yield";
    "methods";
    "null";
    "constructor";
  ]

let munge source =
  let munged = munge_str source in
  if munged = ".." then "_DOT__DOT_"
  else if List.mem munged javascript_reserved_words then munged ^ "$"
  else munged

let demunge_replacements =
  [
    ("_SINGLEQUOTE_", "'");
    ("_DOUBLEQUOTE_", "\"");
    ("_AMPERSAND_", "&");
    ("_PERCENT_", "%");
    ("_LBRACE_", "{");
    ("_RBRACE_", "}");
    ("_LBRACK_", "[");
    ("_RBRACK_", "]");
    ("_BSLASH_", "\\");
    ("_COLON_", ":");
    ("_TILDE_", "~");
    ("_CIRCA_", "@");
    ("_SHARP_", "#");
    ("_CARET_", "^");
    ("_QMARK_", "?");
    ("_SLASH_", "/");
    ("_PLUS_", "+");
    ("_BANG_", "!");
    ("_STAR_", "*");
    ("_BAR_", "|");
    ("_GT_", ">");
    ("_LT_", "<");
    ("_EQ_", "=");
    ("_", "-");
    ("$", "/");
  ]

let demunge source =
  if source = "_DOT__DOT_" then ".."
  else
    let source =
      if String.ends_with ~suffix:"$" source then
        String.sub source 0 (String.length source - 1)
      else source
    in
    let length = String.length source in
    let buffer = Buffer.create length in
    let rec copy index =
      if index < length then
        match
          List.find_opt
            (fun (encoded, _) ->
              let encoded_length = String.length encoded in
              index + encoded_length <= length
              && String.sub source index encoded_length = encoded)
            demunge_replacements
        with
        | Some (encoded, decoded) ->
            Buffer.add_string buffer decoded;
            copy (index + String.length encoded)
        | None ->
            Buffer.add_char buffer source.[index];
            copy (index + 1)
    in
    copy 0;
    Buffer.contents buffer

let starts_with source prefix = String.starts_with ~prefix source
let identity source = source

let digit_value = function
  | '0' .. '9' as digit -> Char.code digit - Char.code '0'
  | 'a' .. 'z' as digit -> Char.code digit - Char.code 'a' + 10
  | 'A' .. 'Z' as digit -> Char.code digit - Char.code 'A' + 10
  | _ -> -1

let parse_int_radix source radix =
  if radix < 2 || radix > 36 then
    invalid_arg "radix must be between 2 and 36";
  let source = String.trim source in
  if source = "" then invalid_arg "cannot parse an empty integer";
  let negative, start =
    match source.[0] with
    | '-' -> (true, 1)
    | '+' -> (false, 1)
    | _ -> (false, 0)
  in
  if start = String.length source then invalid_arg "integer requires digits";
  let rec loop result index =
    if index = String.length source then result
    else
      let digit = digit_value source.[index] in
      if digit < 0 || digit >= radix then invalid_arg "invalid digit for radix"
      else loop ((result * radix) + digit) (index + 1)
  in
  let result = loop 0 start in
  if negative then -result else result

let parse_float_radix source radix =
  if radix < 2 || radix > 36 then
    invalid_arg "radix must be between 2 and 36";
  let source = String.trim source in
  if source = "" then invalid_arg "cannot parse an empty number";
  let negative, start =
    match source.[0] with
    | '-' -> (true, 1)
    | '+' -> (false, 1)
    | _ -> (false, 0)
  in
  if start = String.length source then invalid_arg "number requires digits";
  let radix = Float.of_int radix in
  let rec loop result index =
    if index = String.length source then result
    else
      let digit = digit_value source.[index] in
      if digit < 0 || Float.of_int digit >= radix then
        invalid_arg "invalid digit for radix"
      else loop ((result *. radix) +. Float.of_int digit) (index + 1)
  in
  let result = loop 0.0 start in
  if negative then -.result else result

let decimal_integer_string source =
  let length = String.length source in
  let start =
    if length > 0 && (source.[0] = '-' || source.[0] = '+') then 1 else 0
  in
  if start = length then false
  else
    let rec valid index =
      index = length
      ||
      match source.[index] with
      | '0' .. '9' -> valid (index + 1)
      | _ -> false
    in
    valid start

let safe_decimal_integer_string source =
  if not (decimal_integer_string source) then false
  else
    let length = String.length source in
    let start =
      if source.[0] = '-' || source.[0] = '+' then 1 else 0
    in
    let rec skip_zeroes index =
      if index < length && source.[index] = '0' then skip_zeroes (index + 1)
      else index
    in
    let significant_start = skip_zeroes start in
    let significant_length = length - significant_start in
    significant_length < 16
    ||
    (significant_length = 16
    && String.sub source significant_start significant_length
       <= "9007199254740991")

let ascii_whitespace character = Char.code character <= 0x20

let ascii_trim_bounds source =
  let length = String.length source in
  let rec find_start index =
    if index < length && ascii_whitespace source.[index] then
      find_start (index + 1)
    else index
  in
  let rec find_end index =
    if index > 0 && ascii_whitespace source.[index - 1] then
      find_end (index - 1)
    else index
  in
  (find_start 0, find_end length)

let signed_start source start finish =
  if start < finish && (source.[start] = '-' || source.[start] = '+') then
    start + 1
  else start

let double_nan_string source =
  let start, finish = ascii_trim_bounds source in
  let start = signed_start source start finish in
  finish - start = 3 && String.sub source start 3 = "NaN"

let double_number_string source =
  let start, finish = ascii_trim_bounds source in
  let unsigned_start = signed_start source start finish in
  let exact literal =
    finish - unsigned_start = String.length literal
    && String.sub source unsigned_start (String.length literal) = literal
  in
  if exact "Infinity" then true
  else
    let rec digits index =
      if index < finish then
        match source.[index] with
        | '0' .. '9' -> digits (index + 1)
        | _ -> index
      else index
    in
    let integer_end = digits unsigned_start in
    let has_integer = integer_end > unsigned_start in
    let mantissa_end =
      if integer_end < finish && source.[integer_end] = '.' then
        let fraction_end = digits (integer_end + 1) in
        if has_integer || fraction_end > integer_end + 1 then
          Some fraction_end
        else None
      else if has_integer then Some integer_end
      else None
    in
    match mantissa_end with
    | None -> false
    | Some mantissa_end ->
        let exponent_end =
          if
            mantissa_end < finish
            && (source.[mantissa_end] = 'e' || source.[mantissa_end] = 'E')
          then
            let exponent_start =
              signed_start source (mantissa_end + 1) finish
            in
            let exponent_end = digits exponent_start in
            if exponent_end = exponent_start then None else Some exponent_end
          else Some mantissa_end
        in
        (match exponent_end with
        | None -> false
        | Some exponent_end ->
            let suffix_end =
              if
                exponent_end < finish
                &&
                match source.[exponent_end] with
                | 'd' | 'D' | 'f' | 'F' -> true
                | _ -> false
              then exponent_end + 1
              else exponent_end
            in
            suffix_end = finish)

let parse_decimal_float source =
  let start, finish = ascii_trim_bounds source in
  let finish =
    if finish > start then
      match source.[finish - 1] with
      | 'd' | 'D' | 'f' | 'F' -> finish - 1
      | _ -> finish
    else finish
  in
  let normalized = String.sub source start (finish - start) in
  match normalized with
  | "Infinity" | "+Infinity" -> Float.infinity
  | "-Infinity" -> Float.neg_infinity
  | _ -> float_of_string normalized

let int_to_string_radix value radix =
  if radix < 2 || radix > 36 then
    invalid_arg "radix must be between 2 and 36";
  if value = 0 then "0"
  else
    let digits = "0123456789abcdefghijklmnopqrstuvwxyz" in
    let rec loop value acc =
      if value = 0 then acc
      else
        let digit = abs (value mod radix) in
        loop (value / radix) (digits.[digit] :: acc)
    in
    let encoded = loop value [] |> List.to_seq |> String.of_seq in
    if value < 0 then "-" ^ encoded else encoded
