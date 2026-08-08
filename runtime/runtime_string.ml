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

let regex_pattern expression =
  if String.starts_with ~prefix:regex_prefix expression then
    String.sub expression
      (String.length regex_prefix)
      (String.length expression - String.length regex_prefix)
  else invalid_arg "expected an LG regular expression"

let split source separator =
  let literal_regex pattern =
    let buffer = Buffer.create (String.length pattern) in
    let rec loop index =
      if index >= String.length pattern then Buffer.contents buffer
      else
        match pattern.[index] with
        | '\\' when index + 1 < String.length pattern ->
            Buffer.add_char buffer pattern.[index + 1];
            loop (index + 2)
        | ('.' | '*' | '+' | '?' | '[' | ']' | '(' | ')' | '{' | '}' | '^'
          | '$' | '|') as ch ->
            invalid_arg
              (Printf.sprintf
                 "clojure.string/split does not yet support regex operator %c" ch)
        | ch ->
            Buffer.add_char buffer ch;
            loop (index + 1)
    in
    loop 0
  in
  let separator =
    if String.starts_with ~prefix:regex_prefix separator then
      literal_regex (regex_pattern separator)
    else separator
  in
  let separator_len = String.length separator in
  if separator_len = 0 then Rrbvec.of_list [ source ]
  else
    let rec loop acc start index =
      if index + separator_len > String.length source then
        List.rev (String.sub source start (String.length source - start) :: acc)
      else if String.sub source index separator_len = separator then
        loop
          (String.sub source start (index - start) :: acc)
          (index + separator_len) (index + separator_len)
      else loop acc start (index + 1)
    in
    Rrbvec.of_list (loop [] 0 0)

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
