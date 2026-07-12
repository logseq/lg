let is_whitespace = function
  | ' ' | '\n' | '\r' | '\t' | '\012' -> true
  | _ -> false

let blank source = String.trim source = ""

let ends_with source suffix = String.ends_with ~suffix source

let index_of source needle =
  let needle_len = String.length needle in
  let source_len = String.length source in
  if needle_len = 0 then 0
  else
    let rec search index =
      if index + needle_len > source_len then -1
      else if String.sub source index needle_len = needle then index
      else search (index + 1)
    in
    search 0

let includes source needle = index_of source needle >= 0

let join separator values = String.concat separator (Rrbvec.to_list values)

let last_index_of source needle =
  let needle_len = String.length needle in
  let source_len = String.length source in
  if needle_len = 0 then source_len
  else
    let rec search index =
      if index < 0 then -1
      else if String.sub source index needle_len = needle then index
      else search (index - 1)
    in
    search (source_len - needle_len)

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
        String.sub source 0 index
        ^ replacement
        ^ String.sub source (index + match_len)
            (String.length source - index - match_len)
      else search (index + 1)
    in
    search 0

let split source separator =
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
  let rec loop acc start index =
    if index >= String.length source then
      List.rev (String.sub source start (String.length source - start) :: acc)
    else if source.[index] = '\n' then
      let stop = if index > start && source.[index - 1] = '\r' then index - 1 else index in
      loop (String.sub source start (stop - start) :: acc) (index + 1) (index + 1)
    else loop acc start (index + 1)
  in
  Rrbvec.of_list (if source = "" then [] else loop [] 0 0)

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
    else match source.[index] with '\n' | '\r' -> stop (index - 1) | _ -> index
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

let starts_with source prefix = String.starts_with ~prefix source

let identity source = source
