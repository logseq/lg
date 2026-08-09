open Parsetree

let constructor_name (constructor : Longident.t Location.loc) =
  match constructor with
  | { txt = Longident.Lident name; _ } -> Some name
  | _ -> None

let rec string_patterns pattern =
  match pattern.ppat_desc with
  | Ppat_constant { pconst_desc = Pconst_string (value, _, _); _ } ->
      [ value ]
  | Ppat_alias (nested, _) -> string_patterns nested
  | Ppat_or (left, right) -> string_patterns left @ string_patterns right
  | _ -> []

let symbol_pattern pattern =
  match pattern.ppat_desc with
  | Ppat_construct (constructor, Some (_, argument))
    when constructor_name constructor = Some "FSymbol" ->
      string_patterns argument
  | _ -> []

let list_head_symbols pattern =
  match pattern.ppat_desc with
  | Ppat_construct (constructor, Some (_, argument))
    when constructor_name constructor = Some "::" -> (
      match argument.ppat_desc with
      | Ppat_tuple ([ (_, head); (_, _tail) ], _) -> symbol_pattern head
      | _ -> [])
  | _ -> []

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "usage: extract_ocaml_form_symbol_patterns.ml FILE...";
    exit 2);
  let symbols = ref [] in
  let inspect_file file =
    let channel = open_in file in
    let lexbuf = Lexing.from_channel channel in
    Location.init lexbuf file;
    let structure = Parse.implementation lexbuf in
    close_in channel;
    let iterator =
      {
        Ast_iterator.default_iterator with
        pat =
          (fun self pattern ->
            (match pattern.ppat_desc with
            | Ppat_construct (constructor, Some (_, forms))
              when constructor_name constructor = Some "FList" ->
                symbols := list_head_symbols forms @ !symbols
            | _ -> ());
            Ast_iterator.default_iterator.pat self pattern);
      }
    in
    iterator.structure iterator structure
  in
  Array.to_list Sys.argv |> List.tl |> List.iter inspect_file;
  List.sort_uniq String.compare !symbols |> List.iter print_endline
