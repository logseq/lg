open Types

let whitespace_predicate =
  "(fun ch -> ch = ' ' || ch = '\\n' || ch = '\\r' || ch = '\\t' || ch = '\\012')"

let index_of_code =
  "(fun source needle -> let needle_len = String.length needle in let source_len = String.length source in if needle_len = 0 then 0 else let rec search index = if index + needle_len > source_len then -1 else if String.sub source index needle_len = needle then index else search (index + 1) in search 0)"

let last_index_of_code =
  "(fun source needle -> let needle_len = String.length needle in let source_len = String.length source in if needle_len = 0 then source_len else let rec search index = if index < 0 then -1 else if String.sub source index needle_len = needle then index else search (index - 1) in search (source_len - needle_len))"

let replace_code =
  "(fun source match_value replacement -> let match_len = String.length match_value in if match_len = 0 then source else let buffer = Buffer.create (String.length source) in let rec loop index = if index >= String.length source then () else if index + match_len <= String.length source && String.sub source index match_len = match_value then (Buffer.add_string buffer replacement; loop (index + match_len)) else (Buffer.add_char buffer source.[index]; loop (index + 1)) in loop 0; Buffer.contents buffer)"

let replace_first_code =
  "(fun source match_value replacement -> let match_len = String.length match_value in if match_len = 0 then source else let rec search index = if index + match_len > String.length source then source else if String.sub source index match_len = match_value then String.sub source 0 index ^ replacement ^ String.sub source (index + match_len) (String.length source - index - match_len) else search (index + 1) in search 0)"

let split_code =
  "(fun source separator -> let separator_len = String.length separator in if separator_len = 0 then Rrbvec.of_list [source] else let rec loop acc start index = if index + separator_len > String.length source then List.rev (String.sub source start (String.length source - start) :: acc) else if String.sub source index separator_len = separator then loop (String.sub source start (index - start) :: acc) (index + separator_len) (index + separator_len) else loop acc start (index + 1) in Rrbvec.of_list (loop [] 0 0))"

let split_lines_code =
  "(fun source -> let rec loop acc start index = if index >= String.length source then List.rev (String.sub source start (String.length source - start) :: acc) else if source.[index] = '\\n' then let stop = if index > start && source.[index - 1] = '\\r' then index - 1 else index in loop (String.sub source start (stop - start) :: acc) (index + 1) (index + 1) else loop acc start (index + 1) in Rrbvec.of_list (if source = \"\" then [] else loop [] 0 0))"

let trim_left_code =
  "(fun source -> let is_ws = " ^ whitespace_predicate ^ " in let rec first index = if index >= String.length source then String.length source else if is_ws source.[index] then first (index + 1) else index in let start = first 0 in String.sub source start (String.length source - start))"

let trim_right_code =
  "(fun source -> let is_ws = " ^ whitespace_predicate ^ " in let rec last index = if index < 0 then -1 else if is_ws source.[index] then last (index - 1) else index in let stop = last (String.length source - 1) in if stop < 0 then \"\" else String.sub source 0 (stop + 1))"

let trim_newline_code =
  "(fun source -> let rec stop index = if index < 0 then -1 else match source.[index] with '\\n' | '\\r' -> stop (index - 1) | _ -> index in let last = stop (String.length source - 1) in if last < 0 then \"\" else String.sub source 0 (last + 1))"

let capitalize_code =
  "(fun source -> if source = \"\" then \"\" else String.uppercase_ascii (String.sub source 0 1) ^ String.lowercase_ascii (String.sub source 1 (String.length source - 1)))"

let bindings =
  [
    ("blank?", { ocaml_name = "(fun source -> String.trim source = \"\")"; ty = TFn ([ TString ], TBool) });
    ("capitalize", { ocaml_name = capitalize_code; ty = TFn ([ TString ], TString) });
    ("ends-with?", { ocaml_name = "(fun source suffix -> String.ends_with ~suffix source)"; ty = TFn ([ TString; TString ], TBool) });
    ("includes?", { ocaml_name = "(fun source needle -> (" ^ index_of_code ^ ") source needle >= 0)"; ty = TFn ([ TString; TString ], TBool) });
    ("index-of", { ocaml_name = index_of_code; ty = TFn ([ TString; TString ], TInt) });
    ("join", { ocaml_name = "(fun separator values -> String.concat separator (Rrbvec.to_list values))"; ty = TFn ([ TString; TVector TString ], TString) });
    ("last-index-of", { ocaml_name = last_index_of_code; ty = TFn ([ TString; TString ], TInt) });
    ("lower-case", { ocaml_name = "String.lowercase_ascii"; ty = TFn ([ TString ], TString) });
    ("re-quote-replacement", { ocaml_name = "(fun source -> source)"; ty = TFn ([ TString ], TString) });
    ("replace", { ocaml_name = replace_code; ty = TFn ([ TString; TString; TString ], TString) });
    ("replace-first", { ocaml_name = replace_first_code; ty = TFn ([ TString; TString; TString ], TString) });
    ("reverse", { ocaml_name = "(fun source -> String.of_seq (List.to_seq (List.rev (List.of_seq (String.to_seq source)))))"; ty = TFn ([ TString ], TString) });
    ("split", { ocaml_name = split_code; ty = TFn ([ TString; TString ], TVector TString) });
    ("split-lines", { ocaml_name = split_lines_code; ty = TFn ([ TString ], TVector TString) });
    ("starts-with?", { ocaml_name = "(fun source prefix -> String.starts_with ~prefix source)"; ty = TFn ([ TString; TString ], TBool) });
    ("trim", { ocaml_name = "String.trim"; ty = TFn ([ TString ], TString) });
    ("trim-newline", { ocaml_name = trim_newline_code; ty = TFn ([ TString ], TString) });
    ("triml", { ocaml_name = trim_left_code; ty = TFn ([ TString ], TString) });
    ("trimr", { ocaml_name = trim_right_code; ty = TFn ([ TString ], TString) });
    ("upper-case", { ocaml_name = "String.uppercase_ascii"; ty = TFn ([ TString ], TString) });
  ]
