open Ast
open Types

type require_spec =
  | Alias of {
      namespace : string;
      alias : string;
    }
  | Refer of {
      namespace : string;
      names : string list;
    }

let ocaml_host_functions = function
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

let add_namespace_refer_bindings env current_ns namespace names =
  let rec loop acc = function
    | [] -> Ok acc
    | name :: rest -> (
        let source_key = Names.namespaced_key namespace name in
        match List.assoc_opt source_key env with
        | Some binding ->
            let target_key = Names.namespaced_key current_ns name in
            loop (acc @ [ (target_key, binding) ]) rest
        | None -> Error.error ("cannot refer unknown symbol " ^ source_key))
  in
  loop env names

let add_ocaml_alias_bindings env module_name alias =
  env
  @ (ocaml_host_functions module_name
    |> List.map (fun (name, binding) -> (alias ^ "/" ^ name, binding)))

let add_ocaml_refer_bindings env current_ns module_name names =
  let host_functions = ocaml_host_functions module_name in
  let rec loop acc = function
    | [] -> Ok acc
    | name :: rest -> (
        match List.assoc_opt name host_functions with
        | Some binding ->
            let target_key = Names.namespaced_key current_ns name in
            loop (acc @ [ (target_key, binding) ]) rest
        | None -> Error.error ("cannot refer unknown symbol " ^ module_name ^ "/" ^ name))
  in
  loop env names

let parse_requires clauses =
  let parse_refer_names = function
    | FVector names ->
        let rec loop acc = function
          | [] -> Ok (List.rev acc)
          | FSymbol name :: rest -> loop (name :: acc) rest
          | _ -> Error.error "ns :refer expects a vector of symbols"
        in
        loop [] names
    | _ -> Error.error "ns :refer expects a vector of symbols"
  in
  let parse_require_entry = function
    | FVector (FSymbol namespace :: options) ->
        let rec parse_options acc = function
          | [] -> if acc = [] then Error.error "ns :require entry requires :as or :refer" else Ok acc
          | FKeyword ":as" :: FSymbol alias :: rest ->
              parse_options (Alias { namespace; alias } :: acc) rest
          | FKeyword ":refer" :: names :: rest -> (
              match parse_refer_names names with
              | Error _ as err -> err
              | Ok names -> parse_options (Refer { namespace; names } :: acc) rest)
          | _ -> Error.error "ns :require entries must use :as alias or :refer [symbols]"
        in
        parse_options [] options |> Result.map List.rev
    | _ -> Error.error "ns :require entries must start with a namespace symbol"
  in
  let rec parse_clause acc = function
    | [] -> Ok (List.rev acc)
    | FList (FKeyword ":require" :: entries) :: rest ->
        let rec parse_entries acc = function
          | [] -> Ok acc
          | entry :: entries -> (
              match parse_require_entry entry with
              | Error _ as err -> err
              | Ok specs -> parse_entries (List.rev specs @ acc) entries)
        in
        (match parse_entries acc entries with
        | Error _ as err -> err
        | Ok acc -> parse_clause acc rest)
    | _ -> Error.error "unsupported ns clause"
  in
  parse_clause [] clauses
