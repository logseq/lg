let usage () =
  prerr_endline
    "Usage: lg <input.lgc> [-o output.ml] | --interface <input.lgc> [-o \
     output.mli] | --run <input.lgc> | --compile-files <input.lgc>... -o \
     output.ml | --run-files <input.lgc>... | --lsp";
  exit 2

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let length = in_channel_length ic in
      really_input_string ic length)

let write_output output_path contents =
  match output_path with
  | None -> print_string contents
  | Some path ->
      let oc = open_out path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () -> output_string oc contents)

type mode =
  | Compile of { input_path : string; output_path : string option }
  | Interface of { input_path : string; output_path : string option }
  | Run of { input_path : string }
  | Compile_files of { input_paths : string list; output_path : string }
  | Run_files of { input_paths : string list }
  | Lsp

let extract_target args =
  let rec loop target reversed = function
    | [] -> (target, List.rev reversed)
    | "--target" :: value :: rest -> (
        match Lg.Target.of_string value with
        | Ok target -> loop target reversed rest
        | Error message ->
            prerr_endline ("lg: " ^ message);
            exit 2)
    | [ "--target" ] -> usage ()
    | argument :: rest -> loop target (argument :: reversed) rest
  in
  loop Lg.Target.default [] args

let parse_args argv =
  let target, args = extract_target (Array.to_list argv) in
  let mode =
    match args with
    | [ _program; "--lsp" ] -> Lsp
    | [ _program; "--interface"; input ] ->
        Interface { input_path = input; output_path = None }
    | [ _program; "--interface"; input; "-o"; output ] ->
        Interface { input_path = input; output_path = Some output }
    | [ _program; input ] -> Compile { input_path = input; output_path = None }
    | [ _program; input; "-o"; output ] ->
        Compile { input_path = input; output_path = Some output }
    | [ _program; "--run"; input ] -> Run { input_path = input }
    | _program :: "--compile-files" :: args -> (
        match List.rev args with
        | output_path :: "-o" :: reversed_inputs when reversed_inputs <> [] ->
            Compile_files
              { input_paths = List.rev reversed_inputs; output_path }
        | _ -> usage ())
    | _program :: "--run-files" :: input_paths when input_paths <> [] ->
        Run_files { input_paths }
    | _ -> usage ()
  in
  (target, mode)

let rec find_repo_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then failwith "could not find repo root"
    else find_repo_root parent

let rrbvec_build_dir () =
  Filename.concat
    (find_repo_root (Sys.getcwd ()))
    "_build/default/vendor/rrbvec"

let rrbvec_cmi_dir () =
  Filename.concat (rrbvec_build_dir ()) ".rrbvec.objs/byte"

let lg_build_dir () =
  Filename.concat (find_repo_root (Sys.getcwd ())) "_build/default/src"

let lg_runtime_build_dir () =
  Filename.concat (find_repo_root (Sys.getcwd ())) "_build/default/runtime"

let lg_byte_cmi_dir () = Filename.concat (lg_build_dir ()) ".lg.objs/byte"
let lg_native_cmi_dir () = Filename.concat (lg_build_dir ()) ".lg.objs/native"

let lg_runtime_byte_cmi_dir () =
  Filename.concat (lg_runtime_build_dir ()) ".lg_runtime.objs/byte"

let lg_runtime_native_cmi_dir () =
  Filename.concat (lg_runtime_build_dir ()) ".lg_runtime.objs/native"

let rrbvec_cmxa () = Filename.concat (rrbvec_build_dir ()) "rrbvec.cmxa"

let lg_runtime_cmxa () =
  Filename.concat (lg_runtime_build_dir ()) "lg_runtime.cmxa"

let lg_cmxa () = Filename.concat (lg_build_dir ()) "lg.cmxa"

let run_ocaml_source packages ocaml_source =
  let ml_path = Filename.temp_file "lg" ".ml" in
  let exe_path = Filename.temp_file "lg" ".exe" in
  write_output (Some ml_path) ocaml_source;
  let package_options =
    match packages with
    | [] -> ""
    | packages ->
        "-package " ^ Filename.quote (String.concat "," packages) ^ " -linkpkg "
  in
  let compile_cmd =
    Printf.sprintf
      "ocamlfind ocamlopt %s-I %s -I %s -I %s -I %s -I %s -I %s -I %s -I %s -o \
       %s %s %s %s %s"
      package_options
      (Filename.quote (rrbvec_build_dir ()))
      (Filename.quote (rrbvec_cmi_dir ()))
      (Filename.quote (lg_build_dir ()))
      (Filename.quote (lg_byte_cmi_dir ()))
      (Filename.quote (lg_native_cmi_dir ()))
      (Filename.quote (lg_runtime_build_dir ()))
      (Filename.quote (lg_runtime_byte_cmi_dir ()))
      (Filename.quote (lg_runtime_native_cmi_dir ()))
      (Filename.quote exe_path)
      (Filename.quote (rrbvec_cmxa ()))
      (Filename.quote (lg_runtime_cmxa ()))
      (Filename.quote (lg_cmxa ()))
      (Filename.quote ml_path)
  in
  match Sys.command compile_cmd with
  | 0 ->
      let exit_code = Sys.command (Filename.quote exe_path) in
      Sys.remove ml_path;
      Sys.remove exe_path;
      exit exit_code
  | code ->
      Sys.remove ml_path;
      Sys.remove exe_path;
      exit code

let compile_files target input_paths =
  let rec loop state packages outputs diagnostics = function
    | [] ->
        Ok
          ( List.sort_uniq String.compare packages,
            String.concat "\n" (List.rev outputs),
            List.concat (List.rev diagnostics) )
    | input_path :: rest -> (
        let source = read_file input_path in
        match Lg.Compiler.required_ocaml_packages ~target source with
        | Error _ as err -> err
        | Ok source_packages -> (
            match
              Lg.Compiler.compile_chunk_with_filename_and_diagnostics ~target
                ~filename:input_path state source
            with
            | Error _ as err -> err
            | Ok (state, compilation) ->
                loop state
                  (List.rev_append source_packages packages)
                  (compilation.ocaml_source :: outputs)
                  (compilation.diagnostics :: diagnostics)
                  rest))
  in
  loop Lg.Compiler.empty_state [] [] [] input_paths

let compile_file target input_path =
  let source = read_file input_path in
  match Lg.Compiler.required_ocaml_packages ~target source with
  | Error _ as err -> err
  | Ok packages -> (
      match
        Lg.Compiler.compile_string_with_filename_and_diagnostics ~target
          ~filename:input_path source
      with
      | Error _ as err -> err
      | Ok compilation -> Ok (packages, compilation))

let infer_interface target input_path =
  let source = read_file input_path in
  Lg.Compiler.infer_interface_with_filename ~target ~filename:input_path source

let report_diagnostics diagnostics =
  List.iter
    (fun (diagnostic : Lg.Compiler.diagnostic) ->
      prerr_endline diagnostic.message)
    diagnostics

let report_error (err : Lg.Compiler.compile_error) =
  let location =
    match err.Lg.Compiler.location with
    | None -> ""
    | Some location -> Format.asprintf "%a: " Location.print_loc location
  in
  prerr_endline (location ^ "lg: " ^ err.Lg.Compiler.message);
  exit 1

let () =
  let target, mode = parse_args Sys.argv in
  match mode with
  | Compile { input_path; output_path } -> (
      match compile_file target input_path with
      | Error err -> report_error err
      | Ok (_packages, compilation) ->
          report_diagnostics compilation.diagnostics;
          write_output output_path compilation.ocaml_source)
  | Interface { input_path; output_path } -> (
      match infer_interface target input_path with
      | Error err -> report_error err
      | Ok interface -> write_output output_path interface)
  | Run { input_path } -> (
      match compile_file target input_path with
      | Error err -> report_error err
      | Ok (packages, compilation) ->
          report_diagnostics compilation.diagnostics;
          run_ocaml_source packages compilation.ocaml_source)
  | Compile_files { input_paths; output_path } -> (
      match compile_files target input_paths with
      | Error err -> report_error err
      | Ok (_packages, ocaml_source, diagnostics) ->
          report_diagnostics diagnostics;
          write_output (Some output_path) ocaml_source)
  | Run_files { input_paths } -> (
      match compile_files target input_paths with
      | Error err -> report_error err
      | Ok (packages, ocaml_source, diagnostics) ->
          report_diagnostics diagnostics;
          run_ocaml_source packages ocaml_source)
  | Lsp -> Lsp_server.run ()
