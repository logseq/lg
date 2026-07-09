let usage () =
  prerr_endline "Usage: cljml_cli <input.cljml> [-o output.ml] | --run <input.cljml>";
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
  | Run of { input_path : string }

let parse_args argv =
  match Array.to_list argv with
  | [ _program; input ] -> Compile { input_path = input; output_path = None }
  | [ _program; input; "-o"; output ] ->
      Compile { input_path = input; output_path = Some output }
  | [ _program; "--run"; input ] -> Run { input_path = input }
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

let run_ocaml_source ocaml_source =
  let ml_path = Filename.temp_file "cljml" ".ml" in
  let exe_path = Filename.temp_file "cljml" ".exe" in
  write_output (Some ml_path) ocaml_source;
  let compile_cmd =
    Printf.sprintf "ocamlopt -I %s -I %s -I %s -I %s -I %s -o %s %s %s %s"
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

let () =
  let mode = parse_args Sys.argv in
  let input_path =
    match mode with
    | Compile { input_path; _ } | Run { input_path } -> input_path
  in
  let source = read_file input_path in
  match Cljml.Compiler.compile_string source with
  | Ok ocaml_source -> (
      match mode with
      | Compile { output_path; _ } -> write_output output_path ocaml_source
      | Run _ -> run_ocaml_source ocaml_source)
  | Error err ->
      prerr_endline ("cljml: " ^ err.message);
      exit 1
