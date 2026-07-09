open Ast
open Types

let add_namespace_alias_bindings env namespace alias =
  let prefix = namespace ^ "/" in
  let prefix_len = String.length prefix in
  let alias_entries =
    env
    |> List.filter_map (fun (key, binding) ->
           if String.length key > prefix_len && String.sub key 0 prefix_len = prefix then
             let local =
               String.sub key prefix_len (String.length key - prefix_len)
             in
             Some (alias ^ "/" ^ local, binding)
           else None)
  in
  env @ alias_entries

let add_ocaml_alias_bindings env module_name alias =
  let host_functions =
    match module_name with
    | "ocaml.Stdlib" ->
        [
          ("string-of-int", { ocaml_name = "string_of_int"; ty = TFn ([ TInt ], TString) });
          ("int-of-string", { ocaml_name = "int_of_string"; ty = TFn ([ TString ], TInt) });
        ]
    | "ocaml.String" ->
        [
          ( "uppercase-ascii",
            { ocaml_name = "String.uppercase_ascii"; ty = TFn ([ TString ], TString) } );
          ("length", { ocaml_name = "String.length"; ty = TFn ([ TString ], TInt) });
        ]
    | _ -> []
  in
  env @ List.map (fun (name, binding) -> (alias ^ "/" ^ name, binding)) host_functions

let parse_aliases clauses =
  let parse_require_entry = function
    | FVector [ FSymbol namespace; FKeyword ":as"; FSymbol alias ] -> Ok (namespace, alias)
    | _ -> Error.error "ns :require entries must be [namespace :as alias]"
  in
  let rec parse_clause acc = function
    | [] -> Ok (List.rev acc)
    | FList (FKeyword ":require" :: entries) :: rest ->
        let rec parse_entries acc = function
          | [] -> Ok acc
          | entry :: entries -> (
              match parse_require_entry entry with
              | Error _ as err -> err
              | Ok alias -> parse_entries (alias :: acc) entries)
        in
        (match parse_entries acc entries with
        | Error _ as err -> err
        | Ok acc -> parse_clause acc rest)
    | _ -> Error.error "unsupported ns clause"
  in
  parse_clause [] clauses
