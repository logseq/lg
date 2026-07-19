open Types

let rec find_project_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then Some dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_project_root parent

let existing_dirs dirs = List.filter Sys.file_exists dirs

let env_include_dirs () =
  match Sys.getenv_opt "LG_OCAML_INCLUDE_PATH" with
  | None -> []
  | Some value ->
      value |> String.split_on_char ':' |> List.filter (fun dir -> dir <> "")

let project_include_dirs () =
  match find_project_root (Sys.getcwd ()) with
  | None -> []
  | Some root ->
      existing_dirs
        [
          Filename.concat root "_build/default/src";
          Filename.concat root "_build/default/src/.lg.objs/byte";
          Filename.concat root "_build/default/runtime";
          Filename.concat root "_build/default/runtime/.lg_runtime.objs/byte";
          Filename.concat root "_build/default/vendor/rrbvec";
          Filename.concat root "_build/default/vendor/rrbvec/.rrbvec.objs/byte";
        ]

let include_dirs () = env_include_dirs () @ project_include_dirs ()
let package_include_dirs = ref []
let initialized_include_dirs = ref None
let active_include_dirs () = include_dirs () @ !package_include_dirs

let ensure_initialized () =
  let dirs = active_include_dirs () in
  if !initialized_include_dirs <> Some dirs then (
    Lg_compiler_support.Ocaml_value.init dirs;
    initialized_include_dirs := Some dirs)

let add_include_dirs dirs =
  let updated = List.sort_uniq String.compare (dirs @ !package_include_dirs) in
  if updated <> !package_include_dirs then package_include_dirs := updated;
  ensure_initialized ()

let init () = ensure_initialized ()

type parameter_label = Positional | Labelled of string | Optional of string
type parameter = { label : parameter_label; ty : Types.ty }
type value_signature = { parameters : parameter list; return_type : Types.ty }

type constructor_signature = {
  payload_types : Types.ty list;
  result_type : Types.ty;
}

module Lookup_key = struct
  type t = string list * string

  let equal = ( = )
  let hash = Hashtbl.hash
end

module Lookup_cache = Hashtbl.Make (Lookup_key)

let value_signature_cache =
  Domain.DLS.new_key (fun () -> Lookup_cache.create 64)

let constructor_signature_cache =
  Domain.DLS.new_key (fun () -> Lookup_cache.create 32)

let rec of_compiler_type =
  let open Lg_compiler_support.Ocaml_value in
  function
  | Variable id -> TVar ("ocaml_" ^ string_of_int id)
  | Arrow (Unlabelled, argument, result) ->
      let arguments, result = function_parts result in
      TFn (of_compiler_type argument :: arguments, result)
  | Arrow ((Labelled _ | Optional _), _, _) -> TOcaml "labelled_function"
  | Tuple elements -> TTuple (List.map of_compiler_type elements)
  | Constructor (name, arguments) -> (
      let name =
        match String.split_on_char '.' name with
        | "Stdlib" :: rest -> String.concat "." rest
        | _ -> name
      in
      let arguments = List.map of_compiler_type arguments in
      match (name, arguments) with
      | "int", [] -> TInt
      | "float", [] -> TFloat
      | "char", [] -> TChar
      | "string", [] -> TString
      | "bool", [] -> TBool
      | "unit", [] -> TUnit
      | "list", [ inner ] -> TList inner
      | "array", [ inner ] -> TArray inner
      | "ref", [ inner ] -> TRef inner
      | name, [] -> TOcaml name
      | name, arguments -> TOcaml_app (name, arguments))
  | Opaque -> TOcaml "value"

and function_parts compiler_type =
  match of_compiler_type compiler_type with
  | TFn (arguments, result) -> (arguments, result)
  | result -> ([], result)

let rec signature_of_compiler_type =
  let open Lg_compiler_support.Ocaml_value in
  function
  | Arrow (label, argument, result) ->
      let signature = signature_of_compiler_type result in
      let label =
        match label with
        | Unlabelled -> Positional
        | Labelled name -> Labelled name
        | Optional name -> Optional name
      in
      {
        signature with
        parameters =
          { label; ty = of_compiler_type argument } :: signature.parameters;
      }
  | compiler_type ->
      { parameters = []; return_type = of_compiler_type compiler_type }

let value_signature name =
  let include_dirs = include_dirs () in
  let cache = Domain.DLS.get value_signature_cache in
  let key = (include_dirs, name) in
  match Lookup_cache.find_opt cache key with
  | Some signature -> signature
  | None ->
      let signature =
        match Lg_compiler_support.Ocaml_value.lookup ~include_dirs name with
        | Error message -> Error.error message
        | Ok compiler_type -> Ok (signature_of_compiler_type compiler_type)
      in
      Lookup_cache.add cache key signature;
      signature

let constructor_signature name =
  let include_dirs = include_dirs () in
  let cache = Domain.DLS.get constructor_signature_cache in
  let key = (include_dirs, name) in
  match Lookup_cache.find_opt cache key with
  | Some signature -> signature
  | None ->
      let signature =
        match
          Lg_compiler_support.Ocaml_value.lookup_constructor ~include_dirs name
        with
        | Error message -> Error.error message
        | Ok constructor ->
            Ok
              {
                payload_types =
                  List.map of_compiler_type constructor.arguments;
                result_type = of_compiler_type constructor.result;
              }
      in
      Lookup_cache.add cache key signature;
      signature

let parameter_label_name = function
  | Positional -> None
  | Labelled name | Optional name -> Some name

let result_after_application signature arguments =
  let argument_labels = List.map fst arguments in
  let named_labels = List.filter_map Fun.id argument_labels in
  let rec reject_duplicate seen = function
    | [] -> Ok ()
    | label :: rest ->
        if List.mem label seen then
          Error.error ("duplicate OCaml argument label :" ^ label)
        else reject_duplicate (label :: seen) rest
  in
  let known_label label =
    List.exists
      (fun parameter -> parameter_label_name parameter.label = Some label)
      signature.parameters
  in
  match reject_duplicate [] named_labels with
  | Error _ as err -> err
  | Ok () -> (
      match
        List.find_opt (fun label -> not (known_label label)) named_labels
      with
      | Some label -> Error.error ("unknown OCaml argument label :" ^ label)
      | None -> (
          let consumed_named =
            arguments
            |> List.filter_map (function
              | None, _ -> None
              | Some label, actual_ty ->
                  signature.parameters
                  |> List.find_opt (fun parameter ->
                      parameter_label_name parameter.label = Some label)
                  |> Option.map (fun parameter -> (parameter.ty, actual_ty)))
          in
          let remaining =
            List.filter
              (fun parameter ->
                match parameter_label_name parameter.label with
                | Some label -> not (List.mem label named_labels)
                | None -> true)
              signature.parameters
          in
          let rec consume_positionals consumed remaining = function
            | [] -> Ok (consumed, remaining)
            | (Some _, _) :: rest -> consume_positionals consumed remaining rest
            | (None, actual_ty) :: rest ->
                let rec consume prefix = function
                  | [] -> Error.error "too many positional OCaml arguments"
                  | { label = Optional _; _ } :: parameters ->
                      consume prefix parameters
                  | ({ label = Labelled _; _ } as parameter) :: parameters ->
                      consume (parameter :: prefix) parameters
                  | { label = Positional; ty } :: parameters ->
                      consume_positionals
                        ((ty, actual_ty) :: consumed)
                        (List.rev_append prefix parameters)
                        rest
                in
                consume [] remaining
          in
          match consume_positionals consumed_named remaining arguments with
          | Error _ as err -> err
          | Ok (consumed, parameters) ->
              let result_ty =
                match parameters with
                | [] -> signature.return_type
                | parameters
                  when List.for_all
                         (fun parameter -> parameter.label = Positional)
                         parameters ->
                    TFn
                      ( List.map (fun parameter -> parameter.ty) parameters,
                        signature.return_type )
                | _ -> TOcaml "labelled_function"
              in
              let templates, actuals = List.split consumed in
              Ok (Types.instantiate_type ~templates ~actuals result_ty)))
