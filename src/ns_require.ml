open Ast
open Types

type require_spec =
  | Package of string
  | Alias of {
      namespace : string;
      alias : string;
    }
  | Refer of {
      namespace : string;
      names : string list;
    }

let ocaml_value name ty = Types.binding ~host_reference:(Ocaml_value name) name ty

let ocaml_host_functions = function
  | "ocaml.Stdlib" ->
      [
        ("string-of-int", ocaml_value "string_of_int" (TFn ([ TInt ], TString)));
        ("int-of-string", ocaml_value "int_of_string" (TFn ([ TString ], TInt)));
      ]
  | "ocaml.String" ->
      [
        ( "uppercase-ascii",
          ocaml_value "String.uppercase_ascii" (TFn ([ TString ], TString)) );
        ("length", ocaml_value "String.length" (TFn ([ TString ], TInt)));
      ]
  | _ -> []

let ocaml_module_path module_name =
  let prefix = "ocaml." in
  if String.starts_with ~prefix module_name then
    String.sub module_name (String.length prefix)
      (String.length module_name - String.length prefix)
  else module_name

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

let add_clojure_string_alias_bindings env alias =
  env
  @ (Core_string.bindings
    |> List.map (fun (name, binding) -> (alias ^ "/" ^ name, binding)))

let add_clojure_string_refer_bindings env current_ns names =
  let rec loop acc = function
    | [] -> Ok acc
    | name :: rest -> (
        match List.assoc_opt name Core_string.bindings with
        | Some binding ->
            let target_key = Names.namespaced_key current_ns name in
            loop (acc @ [ (target_key, binding) ]) rest
        | None -> Error.error ("cannot refer unknown symbol clojure.string/" ^ name))
  in
  loop env names

let add_ocaml_alias_bindings env module_name alias =
  let module_path = ocaml_module_path module_name in
  env
  @ [
      ( alias,
        Types.binding ~host_reference:(Ocaml_module module_path) module_path
          (TOcaml "__module") );
    ]
  @ (ocaml_host_functions module_name
    |> List.map (fun (name, binding) -> (alias ^ "/" ^ name, binding)))

let add_ocaml_refer_bindings env current_ns module_name names =
  let host_functions = ocaml_host_functions module_name in
  let module_path = ocaml_module_path module_name in
  let rec loop acc = function
    | [] -> Ok acc
    | name :: rest -> (
        match List.assoc_opt name host_functions with
        | Some binding ->
            let target_key = Names.namespaced_key current_ns name in
            loop (acc @ [ (target_key, binding) ]) rest
        | None ->
            let target_key = Names.namespaced_key current_ns name in
            let ocaml_name = module_path ^ "." ^ Names.sanitize_name name in
            let binding =
              Types.binding ~host_reference:(Ocaml_value ocaml_name) ocaml_name
                (TOcaml "__value")
            in
            loop (acc @ [ (target_key, binding) ]) rest)
  in
  loop env names

let parse_requires clauses =
  let package_prefix = "ocaml.package/" in
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
    | FVector [ FSymbol namespace ]
      when String.starts_with ~prefix:package_prefix namespace ->
        let package =
          String.sub namespace (String.length package_prefix)
            (String.length namespace - String.length package_prefix)
        in
        if Ocaml_package.valid_name package then Ok [ Package package ]
        else Error.error ("invalid OCaml package name " ^ package)
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

let package_names specs =
  specs
  |> List.filter_map (function
       | Package package -> Some package
       | Alias _ | Refer _ -> None)
