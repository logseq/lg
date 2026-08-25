type value = {
  rendered : string;
  type_name : string;
}

type definition = {
  name : string;
  type_name : string;
}

type outcome =
  | Value of value
  | Definition of definition
  | Namespace of string
  | Summary of string

type evaluation = {
  outcome : outcome;
  namespace : string;
}

type t = { mutable compiler_state : Lg.Compiler.state }

type saved_compilation_state = {
  target : Lg.Target.t;
  state : Lg.Compiler.state;
  packages : string list;
  ocaml_source : string;
}

let _runtime_anchor = Lg_runtime.Runtime_reference.of_value ()
let _rrbvec_anchor = Rrbvec.empty
let _stdlib_anchor = Lg_stdlib_native.clojure_core_inc 0

let infrastructure_error message =
  Error
    {
      Lg.Compiler.code = "LG9000";
      phase = `Infrastructure;
      message;
      location = None;
    }

let prepare_toplevel () =
  let output = Buffer.create 128 in
  let formatter = Format.formatter_of_buffer output in
  let succeeded = Toploop.prepare formatter () in
  Format.pp_print_flush formatter ();
  if succeeded then Ok ()
  else
    let message = Buffer.contents output in
    infrastructure_error
      (if String.equal message "" then "failed to prepare the OCaml toplevel"
       else message)

let execute structure =
  let output = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer output in
  let succeeded =
    Toploop.execute_phrase false formatter (Parsetree.Ptop_def structure)
  in
  Format.pp_print_flush formatter ();
  if succeeded then Ok ()
  else
    let message = Buffer.contents output in
    infrastructure_error
      (if String.equal message "" then "OCaml toplevel evaluation failed"
       else message)

let open_precompiled_stdlib () =
  let lexbuf = Lexing.from_string "open Lg_stdlib_native;;" in
  Location.init lexbuf "<repl-bootstrap>";
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  match phrase with
  | exception exn ->
      infrastructure_error
        ("failed to parse the REPL bootstrap: " ^ Printexc.to_string exn)
  | phrase -> (
      match phrase with
      | Parsetree.Ptop_def structure -> execute structure
      | Parsetree.Ptop_dir _ ->
          infrastructure_error "invalid REPL bootstrap phrase")

let installed_lg_root state_path =
  let state_directory = Filename.dirname state_path in
  if String.equal (Filename.basename state_directory) "stdlib" then
    let root = Filename.dirname state_directory in
    if Sys.file_exists (Filename.concat root "META") then Some root else None
  else None

let package_include_directories packages =
  packages
  |> List.concat_map (fun package ->
         match Lg.Ocaml_package.query package with
         | Ok directories -> directories
         | Error _ -> [])

let rec has_path_component component path =
  let basename = Filename.basename path in
  if String.equal basename component then true
  else
    let parent = Filename.dirname path in
    not (String.equal parent path) && has_path_component component parent

let is_native_toplevel_directory path =
  not (has_path_component "melange" path)

let configure_toplevel_load_path ~state_path ~packages =
  let state_directory = Filename.dirname state_path in
  let package_directories =
    package_include_directories
      ("lg.stdlib.native" :: "lg.runtime" :: "lg.edn-backend.native"
     :: "rrbvec" :: packages)
  in
  let installed_directories =
    installed_lg_root state_path
    |> Option.map Lg.Ocaml_package.expand_include_directory
    |> Option.value ~default:[]
  in
  let artifact_directories =
    [
      Filename.concat state_directory "native";
      Filename.concat state_directory
        ".lg_compiled_stdlib_native.objs/byte";
    ]
  in
  package_directories @ installed_directories @ artifact_directories
  |> List.filter (fun path ->
         is_native_toplevel_directory path
         && Sys.file_exists path && Sys.is_directory path)
  |> List.sort_uniq String.compare |> List.iter Topdirs.dir_directory

let create_from_stdlib ~state_path =
  match Lg.Compiler_artifact.read ~kind:"saved-state" ~path:state_path with
  | Error message -> infrastructure_error message
  | Ok saved ->
      let saved = (saved : saved_compilation_state) in
      if saved.target <> Lg.Target.Native then
        infrastructure_error "REPL requires a Native stdlib state"
      else
        match
          Lg.Compiler.restore_ocaml_environment ~target:Lg.Target.Native
            ~packages:saved.packages saved.state [ saved.ocaml_source ]
        with
        | Error _ as error -> error
        | Ok compiler_state -> (
            match prepare_toplevel () with
            | Error _ as error -> error
            | Ok () ->
                configure_toplevel_load_path ~state_path
                  ~packages:saved.packages;
                Result.map
                  (fun () ->
                    {
                      compiler_state =
                        Lg.Compiler.with_source_scope "user" compiler_state;
                    })
                  (open_precompiled_stdlib ()))

let namespace session = Lg.Compiler.source_scope session.compiler_state
let prompt session = namespace session ^ "=> "

let eval session source =
  match Lg.Compiler.compile_repl_form session.compiler_state source with
  | Error _ as error -> error
  | Ok (candidate_state, compilation) ->
      Lg_runtime.Runtime_repl.clear ();
      (match execute compilation.structure with
      | Error _ as error -> error
      | Ok () ->
          let outcome =
            match compilation.kind with
            | Lg.Compiler.Repl_value -> (
                match Lg_runtime.Runtime_repl.take () with
                | Some value ->
                    Ok
                      (Value
                         {
                           rendered = value.rendered;
                           type_name = value.type_name;
                         })
                | None -> infrastructure_error "REPL value was not published")
            | Lg.Compiler.Repl_definition definition ->
                Ok
                  (Definition
                     {
                       name = definition.name;
                       type_name = definition.type_name;
                     })
            | Lg.Compiler.Repl_namespace namespace -> Ok (Namespace namespace)
            | Lg.Compiler.Repl_summary summary -> Ok (Summary summary)
          in
          Result.map
            (fun outcome ->
              session.compiler_state <- candidate_state;
              { outcome; namespace = namespace session })
            outcome)

let type_of session source =
  Lg.Compiler.infer_repl_type session.compiler_state source
