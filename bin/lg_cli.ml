let usage () =
  prerr_endline
    "Usage: lg <input.cljc> [-o output.ml] | --interface <input.cljc> [-o \
     output.mli] | --run <input.cljc> | --compile-files <input.cljc>... -o \
     output.ml | --compile-files-state <state> <input.cljc>... -o output.ml | \
     --compile-files-from <state> <input.cljc>... -o output.ml | \
     --compile-files-from-state <input-state> <output-state> <input.cljc>... -o \
     output.ml | \
     --compile-chunk-from <state> <input.cljc> [-o output.ml] | \
     --compile-chunk-state <input-state> <output-state> <input.cljc> [-o \
     output.ml] | \
     --run-files <input.cljc>... | --lsp";
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

type cached_prefix_state = { state : Lg.Compiler.state }

type cached_prefix_output = {
  source_packages : string list;
  compilation : Lg.Compiler.compilation;
}

type compiler_state = Live of Lg.Compiler.state | Cached of string

type saved_compilation_state = {
  target : Lg.Target.t;
  state : Lg.Compiler.state;
  packages : string list;
}

let write_saved_compilation_state path saved =
  let output = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () -> Marshal.to_channel output saved [])

let read_saved_compilation_state path =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () -> (Marshal.from_channel input : saved_compilation_state))

let compile_cache_enabled () =
  Sys.getenv_opt "LG_DISABLE_COMPILE_CACHE" <> Some "1"

let compile_cache_min_seconds () =
  match Sys.getenv_opt "LG_COMPILE_CACHE_MIN_SECONDS" with
  | Some value -> Option.value (float_of_string_opt value) ~default:0.
  | None -> 0.

let rec find_repo_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then failwith "could not find repo root"
    else find_repo_root parent

let rec ensure_directory path =
  if Sys.file_exists path then ()
  else (
    ensure_directory (Filename.dirname path);
    Unix.mkdir path 0o755)

let compile_cache_directory () =
  match Sys.getenv_opt "LG_CACHE_DIR" with
  | Some path -> Filename.concat path "compile-files"
  | None ->
      Filename.concat
        (find_repo_root (Sys.getcwd ()))
        ".lg-cache/compile-files"

let compiler_cache_identity () =
  let repo_root = find_repo_root (Sys.getcwd ()) in
  let adjacent_compiler_directory =
    Filename.concat (Filename.dirname Sys.executable_name) "../src"
  in
  let workspace_compiler_directory =
    Filename.concat repo_root "_build/default/src"
  in
  let compiler_artifacts directory =
    [ "lg.cmxa"; "lg.a"; "lg.cma" ]
    |> List.map (Filename.concat directory)
    |> List.filter Sys.file_exists
  in
  let artifacts =
    match compiler_artifacts adjacent_compiler_directory with
    | _ :: _ as artifacts -> artifacts
    | [] -> (
        match compiler_artifacts workspace_compiler_directory with
        | _ :: _ as artifacts -> artifacts
        | [] -> [ Sys.executable_name ])
  in
  let artifact_identity path =
    Filename.basename path ^ "\000" ^ Digest.to_hex (Digest.file path)
  in
  String.concat "\000"
    (Sys.ocaml_version :: List.map artifact_identity artifacts)
  |> Digest.string |> Digest.to_hex

let next_prefix_key ~target previous_key input_path source =
  Digest.string
    (String.concat "\000"
       [ previous_key; Lg.Target.to_string target; input_path; source ])
  |> Digest.to_hex

let cache_path key suffix =
  Filename.concat (compile_cache_directory ()) (key ^ suffix ^ ".marshal")

let read_marshaled path =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () -> Marshal.from_channel input)

let read_cached_prefix_output key =
  if not (compile_cache_enabled ()) then None
  else
    let state_path = cache_path key ".state" in
    let output_path = cache_path key ".output" in
    if not (Sys.file_exists state_path && Sys.file_exists output_path) then None
    else
      try
        Some (read_marshaled output_path : cached_prefix_output)
      with _ -> None

let read_cached_prefix_state key =
  let started_at = Sys.time () in
  try
    let path = cache_path key ".state" in
    let cached = (read_marshaled path : cached_prefix_state) in
    if Sys.getenv_opt "LG_COMPILE_TIMINGS" = Some "1" then
      Printf.eprintf "lg: read cached state: %.3fs\n%!"
        (Sys.time () -. started_at);
    Ok cached.state
  with exn ->
    Error
      {
        Lg.Compiler.message =
          "failed to read cached compiler state: " ^ Printexc.to_string exn;
        location = None;
      }

let write_marshaled path value =
  let temporary =
    Filename.temp_file ~temp_dir:(Filename.dirname path) "prefix-" ".tmp"
  in
  let output = open_out_bin temporary in
  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () -> Marshal.to_channel output value []);
  Sys.rename temporary path

let write_cached_prefix key state output =
  if compile_cache_enabled () then
    try
      let started_at = Sys.time () in
      let directory = compile_cache_directory () in
      ensure_directory directory;
      write_marshaled (cache_path key ".state")
        { state = Lg.Compiler.cacheable_state state };
      write_marshaled (cache_path key ".output") output;
      if Sys.getenv_opt "LG_COMPILE_TIMINGS" = Some "1" then
        Printf.eprintf "lg: wrote cached prefix: %.3fs\n%!"
          (Sys.time () -. started_at)
    with _ -> ()

let report_cache_hit input_path =
  if Sys.getenv_opt "LG_COMPILE_CACHE_DEBUG" = Some "1" then
    Printf.eprintf "lg: compile cache hit: %s\n%!" input_path

type mode =
  | Compile of { input_path : string; output_path : string option }
  | Interface of { input_path : string; output_path : string option }
  | Run of { input_path : string }
  | Compile_files of { input_paths : string list; output_path : string }
  | Compile_files_state of {
      state_path : string;
      input_paths : string list;
      output_path : string;
    }
  | Compile_files_from of {
      state_path : string;
      input_paths : string list;
      output_path : string;
    }
  | Compile_files_from_state of {
      state_path : string;
      output_state_path : string;
      input_paths : string list;
      output_path : string;
    }
  | Compile_chunk_from of {
      state_path : string;
      input_path : string;
      output_path : string option;
    }
  | Compile_chunk_state of {
      state_path : string;
      output_state_path : string;
      input_path : string;
      output_path : string option;
    }
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
    | _program :: "--compile-files-state" :: state_path :: args -> (
        match List.rev args with
        | output_path :: "-o" :: reversed_inputs when reversed_inputs <> [] ->
            Compile_files_state
              {
                state_path;
                input_paths = List.rev reversed_inputs;
                output_path;
              }
        | _ -> usage ())
    | _program :: "--compile-files-from" :: state_path :: args -> (
        match List.rev args with
        | output_path :: "-o" :: reversed_inputs when reversed_inputs <> [] ->
            Compile_files_from
              {
                state_path;
                input_paths = List.rev reversed_inputs;
                output_path;
              }
        | _ -> usage ())
    | _program :: "--compile-files-from-state" :: state_path
      :: output_state_path :: args -> (
        match List.rev args with
        | output_path :: "-o" :: reversed_inputs when reversed_inputs <> [] ->
            Compile_files_from_state
              {
                state_path;
                output_state_path;
                input_paths = List.rev reversed_inputs;
                output_path;
              }
        | _ -> usage ())
    | [
     _program;
     "--compile-chunk-from";
     state_path;
     input_path;
     "-o";
     output_path;
    ] ->
        Compile_chunk_from
          { state_path; input_path; output_path = Some output_path }
    | [ _program; "--compile-chunk-from"; state_path; input_path ] ->
        Compile_chunk_from { state_path; input_path; output_path = None }
    | [
     _program;
     "--compile-chunk-state";
     state_path;
     output_state_path;
     input_path;
     "-o";
     output_path;
    ] ->
        Compile_chunk_state
          {
            state_path;
            output_state_path;
            input_path;
            output_path = Some output_path;
          }
    | [
     _program;
     "--compile-chunk-state";
     state_path;
     output_state_path;
     input_path;
    ] ->
        Compile_chunk_state
          { state_path; output_state_path; input_path; output_path = None }
    | _program :: "--run-files" :: input_paths when input_paths <> [] ->
        Run_files { input_paths }
    | _ -> usage ()
  in
  (target, mode)

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
  let packages = List.sort_uniq String.compare ("unix" :: packages) in
  let package_options =
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

let concatenate_compilation_outputs outputs =
  let runtime_open = "open Lg_runtime\n" in
  let runtime_open_length = String.length runtime_open in
  let _, outputs =
    List.fold_left
      (fun (seen_runtime_open, outputs) output ->
        let starts_with_runtime_open =
          String.starts_with ~prefix:runtime_open output
        in
        let output =
          if seen_runtime_open && starts_with_runtime_open then
            String.sub output runtime_open_length
              (String.length output - runtime_open_length)
          else output
        in
        (seen_runtime_open || starts_with_runtime_open, output :: outputs))
      (false, []) outputs
  in
  outputs |> List.rev |> String.concat "\n"

let read_compiler_state = function
  | Live state -> Ok state
  | Cached key -> read_cached_prefix_state key

let resume_compiler_state ~target ~packages ~sources = function
  | Live state -> Ok state
  | Cached key ->
      Result.bind (read_cached_prefix_state key) (fun state ->
          Lg.Compiler.restore_ocaml_environment ~target ~packages state sources)

let compile_files target input_paths =
  let rec loop prefix_key compiler_state packages outputs diagnostics =
    function
    | [] ->
        let packages = List.sort_uniq String.compare packages in
        let outputs = List.rev outputs in
        Result.map
          (fun state ->
            let ocaml_source = concatenate_compilation_outputs outputs in
            ( state,
              packages,
              ocaml_source,
              List.concat (List.rev diagnostics) ))
          (read_compiler_state compiler_state)
    | input_path :: rest -> (
        let source = read_file input_path in
        let prefix_key =
          next_prefix_key ~target prefix_key input_path source
        in
        match Lg.Compiler.required_ocaml_packages ~target source with
        | Error _ as err -> err
        | Ok source_packages -> (
            match read_cached_prefix_output prefix_key with
            | Some cached ->
                report_cache_hit input_path;
                loop prefix_key (Cached prefix_key)
                  (List.rev_append cached.source_packages packages)
                  (cached.compilation.ocaml_source :: outputs)
                  (cached.compilation.diagnostics :: diagnostics)
                  rest
            | None ->
                Result.bind
                  (resume_compiler_state ~target ~packages
                     ~sources:(List.rev outputs) compiler_state)
                  (fun state ->
                    if Sys.getenv_opt "LG_COMPILE_TIMINGS" = Some "1" then
                      Printf.eprintf "lg: compiling %s\n%!" input_path;
                    let started_at = Sys.time () in
                    match
                      Lg.Compiler.compile_chunk_with_filename_and_diagnostics
                        ~target ~filename:input_path state source
                    with
                    | Error _ as err -> err
                    | Ok (state, compilation) ->
                        if
                          Sys.time () -. started_at
                          >= compile_cache_min_seconds ()
                        then
                          write_cached_prefix prefix_key state
                            { source_packages; compilation };
                        loop prefix_key (Live state)
                          (List.rev_append source_packages packages)
                          (compilation.ocaml_source :: outputs)
                          (compilation.diagnostics :: diagnostics)
                          rest)))
  in
  loop (compiler_cache_identity ()) (Live Lg.Compiler.empty_state) [] [] []
    input_paths

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

let compile_chunk_from_saved_state target state_path input_path =
  let saved = read_saved_compilation_state state_path in
  if saved.target <> target then
    Error
      {
        Lg.Compiler.message =
          "saved compiler state target does not match --target";
        location = None;
      }
  else
    let source = read_file input_path in
    Result.bind
      (Lg.Compiler.required_ocaml_packages ~target source)
      (fun source_packages ->
        let packages =
          List.sort_uniq String.compare (source_packages @ saved.packages)
        in
        Result.bind
          (Lg.Compiler.restore_ocaml_environment ~target ~packages saved.state [])
          (fun state ->
            Lg.Compiler.compile_chunk_with_filename_and_diagnostics ~target
              ~filename:input_path ~check_ocaml:false state source
            |> Result.map (fun (state, compilation) ->
                   (state, packages, compilation))))

let compile_files_from_saved_state target state_path input_paths =
  let saved = read_saved_compilation_state state_path in
  if saved.target <> target then
    Error
      {
        Lg.Compiler.message =
          "saved compiler state target does not match --target";
        location = None;
      }
  else
    let rec read_sources sources packages = function
      | [] -> Ok (List.rev sources, List.sort_uniq String.compare packages)
      | input_path :: rest ->
          let source = read_file input_path in
          Result.bind
            (Lg.Compiler.required_ocaml_packages ~target source)
            (fun source_packages ->
              read_sources ((input_path, source) :: sources)
                (List.rev_append source_packages packages)
                rest)
    in
    Result.bind
      (read_sources [] saved.packages input_paths)
      (fun (sources, packages) ->
        Result.bind
          (Lg.Compiler.restore_ocaml_environment ~target ~packages saved.state
             [])
          (fun state ->
            let rec compile state outputs diagnostics = function
              | [] ->
                  Ok
                    ( state,
                      packages,
                      concatenate_compilation_outputs (List.rev outputs),
                      List.concat (List.rev diagnostics) )
              | (input_path, source) :: rest ->
                  Result.bind
                    (Lg.Compiler.compile_chunk_with_filename_and_diagnostics
                       ~target ~filename:input_path ~check_ocaml:false state source)
                    (fun (state, compilation) ->
                      compile state (compilation.ocaml_source :: outputs)
                        (compilation.diagnostics :: diagnostics)
                        rest)
            in
            compile state [] [] sources))

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
      | Ok (_state, _packages, ocaml_source, diagnostics) ->
          report_diagnostics diagnostics;
          write_output (Some output_path) ocaml_source)
  | Compile_files_state { state_path; input_paths; output_path } -> (
      match compile_files target input_paths with
      | Error err -> report_error err
      | Ok (state, packages, ocaml_source, diagnostics) ->
          report_diagnostics diagnostics;
          write_output (Some output_path) ocaml_source;
          write_saved_compilation_state state_path
            {
              target;
              state = Lg.Compiler.cacheable_state state;
              packages;
            })
  | Compile_files_from { state_path; input_paths; output_path } -> (
      match compile_files_from_saved_state target state_path input_paths with
      | Error err -> report_error err
      | Ok (_state, _packages, ocaml_source, diagnostics) ->
          report_diagnostics diagnostics;
          write_output (Some output_path) ocaml_source)
  | Compile_files_from_state
      { state_path; output_state_path; input_paths; output_path } -> (
      match compile_files_from_saved_state target state_path input_paths with
      | Error err -> report_error err
      | Ok (state, packages, ocaml_source, diagnostics) ->
          report_diagnostics diagnostics;
          write_output (Some output_path) ocaml_source;
          write_saved_compilation_state output_state_path
            {
              target;
              state = Lg.Compiler.cacheable_state state;
              packages;
            })
  | Compile_chunk_from { state_path; input_path; output_path } -> (
      match compile_chunk_from_saved_state target state_path input_path with
      | Error err -> report_error err
      | Ok (_state, _packages, compilation) ->
          report_diagnostics compilation.diagnostics;
          write_output output_path compilation.ocaml_source)
  | Compile_chunk_state
      { state_path; output_state_path; input_path; output_path } -> (
      match compile_chunk_from_saved_state target state_path input_path with
      | Error err -> report_error err
      | Ok (state, packages, compilation) ->
          report_diagnostics compilation.diagnostics;
          write_output output_path compilation.ocaml_source;
          write_saved_compilation_state output_state_path
            {
              target;
              state = Lg.Compiler.cacheable_state state;
              packages;
            })
  | Run_files { input_paths } -> (
      match compile_files target input_paths with
      | Error err -> report_error err
      | Ok (_state, packages, ocaml_source, diagnostics) ->
          report_diagnostics diagnostics;
          run_ocaml_source packages ocaml_source)
  | Lsp -> Lsp_server.run ()
