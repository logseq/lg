open Types

let rec find_project_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then Some dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_project_root parent

let existing_dirs dirs = List.filter Sys.file_exists dirs

let env_include_dirs () =
  match Sys.getenv_opt "CLJML_OCAML_INCLUDE_PATH" with
  | None -> []
  | Some value ->
      value |> String.split_on_char ':' |> List.filter (fun dir -> dir <> "")

let project_include_dirs () =
  match find_project_root (Sys.getcwd ()) with
  | None -> []
  | Some root ->
      existing_dirs
        [ Filename.concat root "_build/default/src";
          Filename.concat root "_build/default/src/.cljml.objs/byte";
          Filename.concat root "_build/default/vendor/rrbvec";
          Filename.concat root "_build/default/vendor/rrbvec/.rrbvec.objs/byte" ]

let include_dirs () = env_include_dirs () @ project_include_dirs ()

let package_include_dirs = ref []

let add_include_dirs dirs =
  package_include_dirs :=
    List.sort_uniq String.compare (dirs @ !package_include_dirs);
  Cljml_compiler_support.Ocaml_value.init dirs

let init () =
  Cljml_compiler_support.Ocaml_value.init
    (include_dirs () @ !package_include_dirs)

type parameter_label =
  | Positional
  | Labelled of string
  | Optional of string

type parameter = {
  label : parameter_label;
  ty : Types.ty;
}

type value_signature = {
  parameters : parameter list;
  return_type : Types.ty;
}

type constructor_signature = {
  payload_types : Types.ty list;
  result_type : Types.ty;
}

let rec of_compiler_type =
  let open Cljml_compiler_support.Ocaml_value in
  function
  | Variable -> TAny
  | Arrow (Unlabelled, argument, result) ->
      let arguments, result = function_parts result in
      TFn (of_compiler_type argument :: arguments, result)
  | Arrow ((Labelled _ | Optional _), _, _) -> TOcaml "labelled_function"
  | Tuple elements -> TTuple (List.map of_compiler_type elements)
  | Constructor (name, arguments) ->
      let arguments = List.map of_compiler_type arguments in
      (match (name, arguments) with
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
  let open Cljml_compiler_support.Ocaml_value in
  function
  | Arrow (label, argument, result) ->
      let signature = signature_of_compiler_type result in
      let label =
        match label with
        | Unlabelled -> Positional
        | Labelled name -> Labelled name
        | Optional name -> Optional name
      in
      { signature with
        parameters = { label; ty = of_compiler_type argument } :: signature.parameters;
      }
  | compiler_type -> { parameters = []; return_type = of_compiler_type compiler_type }

let value_signature name =
  match Cljml_compiler_support.Ocaml_value.lookup ~include_dirs:(include_dirs ()) name with
  | Error message -> Error.error message
  | Ok compiler_type -> Ok (signature_of_compiler_type compiler_type)

let constructor_signature name =
  match
    Cljml_compiler_support.Ocaml_value.lookup_constructor
      ~include_dirs:(include_dirs ()) name
  with
  | Error message -> Error.error message
  | Ok constructor ->
      Ok
        { payload_types = List.map of_compiler_type constructor.arguments;
          result_type = of_compiler_type constructor.result }

let parameter_label_name = function
  | Positional -> None
  | Labelled name | Optional name -> Some name

let result_after_application signature argument_labels =
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
      match List.find_opt (fun label -> not (known_label label)) named_labels with
      | Some label -> Error.error ("unknown OCaml argument label :" ^ label)
      | None ->
          let remaining =
            List.filter
              (fun parameter ->
                match parameter_label_name parameter.label with
                | Some label -> not (List.mem label named_labels)
                | None -> true)
              signature.parameters
          in
          let rec consume_positionals remaining = function
            | [] -> Ok remaining
            | Some _ :: rest -> consume_positionals remaining rest
            | None :: rest ->
                let rec consume prefix = function
                  | [] -> Error.error "too many positional OCaml arguments"
                  | { label = Optional _; _ } :: parameters -> consume prefix parameters
                  | ({ label = Labelled _; _ } as parameter) :: parameters ->
                      consume (parameter :: prefix) parameters
                  | { label = Positional; _ } :: parameters ->
                      consume_positionals (List.rev_append prefix parameters) rest
                in
                consume [] remaining
          in
          match consume_positionals remaining argument_labels with
          | Error _ as err -> err
          | Ok [] -> Ok signature.return_type
          | Ok parameters ->
              if List.for_all (fun parameter -> parameter.label = Positional) parameters then
                Ok (TFn (List.map (fun parameter -> parameter.ty) parameters, signature.return_type))
              else Ok (TOcaml "labelled_function"))
