let usage () =
  prerr_endline
    "Usage: cljml_cli <input.cljml> [-o output.ml] | \
     --interface <input.cljml> [-o output.mli] | --run <input.cljml> | \
     --compile-files <input.cljml>... -o output.ml | --run-files <input.cljml>... | \
     --lsp";
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

let parse_args argv =
  match Array.to_list argv with
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

let rec find_repo_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then failwith "could not find repo root" else find_repo_root parent

let rrbvec_build_dir () =
  Filename.concat (find_repo_root (Sys.getcwd ())) "_build/default/vendor/rrbvec"

let rrbvec_cmi_dir () = Filename.concat (rrbvec_build_dir ()) ".rrbvec.objs/byte"

let cljml_build_dir () =
  Filename.concat (find_repo_root (Sys.getcwd ())) "_build/default/src"

let cljml_byte_cmi_dir () = Filename.concat (cljml_build_dir ()) ".cljml.objs/byte"

let cljml_native_cmi_dir () = Filename.concat (cljml_build_dir ()) ".cljml.objs/native"

let rrbvec_cmxa () = Filename.concat (rrbvec_build_dir ()) "rrbvec.cmxa"

let cljml_cmxa () = Filename.concat (cljml_build_dir ()) "cljml.cmxa"

let run_ocaml_source packages ocaml_source =
  let ml_path = Filename.temp_file "cljml" ".ml" in
  let exe_path = Filename.temp_file "cljml" ".exe" in
  write_output (Some ml_path) ocaml_source;
  let package_options =
    match packages with
    | [] -> ""
    | packages ->
        "-package " ^ Filename.quote (String.concat "," packages) ^ " -linkpkg "
  in
  let compile_cmd =
    Printf.sprintf
      "ocamlfind ocamlopt %s-I %s -I %s -I %s -I %s -I %s -o %s %s %s %s"
      package_options
      (Filename.quote (rrbvec_build_dir ()))
      (Filename.quote (rrbvec_cmi_dir ()))
      (Filename.quote (cljml_build_dir ()))
      (Filename.quote (cljml_byte_cmi_dir ()))
      (Filename.quote (cljml_native_cmi_dir ()))
      (Filename.quote exe_path)
      (Filename.quote (rrbvec_cmxa ()))
      (Filename.quote (cljml_cmxa ()))
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

let compile_files input_paths =
  let rec loop state packages outputs diagnostics = function
    | [] ->
        Ok
          ( List.sort_uniq String.compare packages,
            String.concat "\n" (List.rev outputs),
            List.concat (List.rev diagnostics) )
    | input_path :: rest ->
        let source = read_file input_path in
        (match Cljml.Compiler.required_ocaml_packages source with
        | Error _ as err -> err
        | Ok source_packages -> (
            match
              Cljml.Compiler.compile_chunk_with_filename_and_diagnostics
                ~filename:input_path state source
            with
            | Error _ as err -> err
            | Ok (state, compilation) ->
                loop state (List.rev_append source_packages packages)
                  (compilation.ocaml_source :: outputs)
                  (compilation.diagnostics :: diagnostics) rest))
  in
  loop Cljml.Compiler.empty_state [] [] [] input_paths

let compile_file input_path =
  let source = read_file input_path in
  match Cljml.Compiler.required_ocaml_packages source with
  | Error _ as err -> err
  | Ok packages -> (
      match
        Cljml.Compiler.compile_string_with_filename_and_diagnostics
          ~filename:input_path source
      with
      | Error _ as err -> err
      | Ok compilation -> Ok (packages, compilation))

let infer_interface input_path =
  let source = read_file input_path in
  Cljml.Compiler.infer_interface_with_filename ~filename:input_path source

let report_diagnostics diagnostics =
  List.iter
    (fun (diagnostic : Cljml.Compiler.diagnostic) ->
      prerr_endline diagnostic.message)
    diagnostics

let report_error (err : Cljml.Compiler.compile_error) =
  prerr_endline ("cljml: " ^ err.Cljml.Compiler.message);
  exit 1

let () =
  let mode = parse_args Sys.argv in
  match mode with
  | Compile { input_path; output_path } -> (
      match compile_file input_path with
      | Error err -> report_error err
      | Ok (_packages, compilation) ->
          report_diagnostics compilation.diagnostics;
          write_output output_path compilation.ocaml_source)
  | Interface { input_path; output_path } -> (
      match infer_interface input_path with
      | Error err -> report_error err
      | Ok interface -> write_output output_path interface)
  | Run { input_path } -> (
      match compile_file input_path with
      | Error err -> report_error err
      | Ok (packages, compilation) ->
          report_diagnostics compilation.diagnostics;
          run_ocaml_source packages compilation.ocaml_source)
  | Compile_files { input_paths; output_path } -> (
      match compile_files input_paths with
      | Error err -> report_error err
      | Ok (_packages, ocaml_source, diagnostics) ->
          report_diagnostics diagnostics;
          write_output (Some output_path) ocaml_source)
  | Run_files { input_paths } -> (
      match compile_files input_paths with
      | Error err -> report_error err
      | Ok (packages, ocaml_source, diagnostics) ->
          report_diagnostics diagnostics;
          run_ocaml_source packages ocaml_source)
  | Lsp -> Lsp_server.run ()
