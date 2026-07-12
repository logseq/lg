open Ast
open Types

type require_spec =
  | Package of string
  | Alias of {
      module_name : string;
      alias : string;
    }
  | Refer of {
      module_name : string;
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

let add_clojure_string_alias_bindings env alias =
  env
  @ (Core_string.bindings
    |> List.map (fun (name, binding) -> (alias ^ "/" ^ name, binding)))

let add_clojure_string_refer_bindings env scope names =
  let rec loop acc = function
    | [] -> Ok acc
    | name :: rest -> (
        match List.assoc_opt name Core_string.bindings with
        | Some binding ->
            let target_key = Names.scoped_key scope name in
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

let add_ocaml_refer_bindings env scope module_name names =
  let host_functions = ocaml_host_functions module_name in
  let module_path = ocaml_module_path module_name in
  let rec loop acc = function
    | [] -> Ok acc
    | name :: rest -> (
        match List.assoc_opt name host_functions with
        | Some binding ->
            let target_key = Names.scoped_key scope name in
            loop (acc @ [ (target_key, binding) ]) rest
        | None ->
            let target_key = Names.scoped_key scope name in
            let ocaml_name = module_path ^ "." ^ Names.sanitize_name name in
            let binding =
              Types.binding ~host_reference:(Ocaml_value ocaml_name) ocaml_name
                (TOcaml "__value")
            in
            loop (acc @ [ (target_key, binding) ]) rest)
  in
  loop env names

let parse_entries entries =
  let package_prefix = "ocaml.package/" in
  let parse_refer_names = function
    | FVector names ->
        let rec loop acc = function
          | [] -> Ok (List.rev acc)
          | FSymbol name :: rest -> loop (name :: acc) rest
          | _ -> Error.error "require :refer expects a vector of symbols"
        in
        loop [] names
    | _ -> Error.error "require :refer expects a vector of symbols"
  in
  let parse_require_entry = function
    | FVector [ FSymbol module_name ]
      when String.starts_with ~prefix:package_prefix module_name ->
        let package =
          String.sub module_name (String.length package_prefix)
            (String.length module_name - String.length package_prefix)
        in
        if Ocaml_package.valid_name package then Ok [ Package package ]
        else Error.error ("invalid OCaml package name " ^ package)
    | FVector (FSymbol module_name :: options) ->
        let rec parse_options acc = function
          | [] -> if acc = [] then Error.error "require entry requires :as or :refer" else Ok acc
          | FKeyword ":as" :: FSymbol alias :: rest ->
              parse_options (Alias { module_name; alias } :: acc) rest
          | FKeyword ":refer" :: names :: rest -> (
              match parse_refer_names names with
              | Error _ as err -> err
              | Ok names -> parse_options (Refer { module_name; names } :: acc) rest)
          | _ -> Error.error "require entries must use :as alias or :refer [symbols]"
        in
        parse_options [] options |> Result.map List.rev
    | _ -> Error.error "require entries must start with a module symbol"
  in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | entry :: rest -> (
        match parse_require_entry entry with
        | Error _ as err -> err
        | Ok specs -> loop (List.rev_append specs acc) rest)
  in
  loop [] entries

let package_names specs =
  specs
  |> List.filter_map (function
       | Package package -> Some package
       | Alias _ | Refer _ -> None)
