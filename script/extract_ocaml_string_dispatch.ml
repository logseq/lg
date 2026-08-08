open Asttypes
open Parsetree

let string_patterns pattern =
  let rec collect acc pattern =
    match pattern.ppat_desc with
    | Ppat_constant { pconst_desc = Pconst_string (value, _, _); _ } ->
        value :: acc
    | Ppat_alias (nested, _) -> collect acc nested
    | Ppat_or (left, right) -> collect (collect acc left) right
    | _ -> acc
  in
  collect [] pattern

let is_name_expression expression =
  match expression.pexp_desc with
  | Pexp_ident { txt = Longident.Lident "name"; _ } -> true
  | _ -> false

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "usage: extract_ocaml_string_dispatch.ml FILE";
    exit 2);
  let channel = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel channel in
  Location.init lexbuf Sys.argv.(1);
  let structure = Parse.implementation lexbuf in
  close_in channel;
  let largest = ref [] in
  let iterator =
    {
      Ast_iterator.default_iterator with
      expr =
        (fun self expression ->
          (match expression.pexp_desc with
          | Pexp_match (subject, cases) when is_name_expression subject ->
              let names =
                List.concat_map
                  (fun case -> string_patterns case.pc_lhs)
                  cases
              in
              if List.length names > List.length !largest then largest := names
          | _ -> ());
          Ast_iterator.default_iterator.expr self expression);
    }
  in
  iterator.structure iterator structure;
  List.sort_uniq String.compare !largest |> List.iter print_endline
