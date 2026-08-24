let usage () =
  prerr_endline
    "Usage: lg <input.cljc> [-o output.ml] | --interface <input.cljc> [-o \
     output.mli] | --run <input.cljc> | --compile-files <input.cljc>... -o \
     output.ml | --compile-files-state <state> <input.cljc>... -o output.ml | \
     --compile-files-from <state> <input.cljc>... -o output.ml | \
     --compile-files-chunk-from <state> <input.cljc>... -o output.ml | \
     --compile-files-from-state <input-state> <output-state> <input.cljc>... -o \
     output.ml | \
     --compile-chunk-from <state> <input.cljc> [-o output.ml] | \
     --compile-chunk-state <input-state> <output-state> <input.cljc> [-o \
     output.ml] | \
     --run-from <state> <implementation.ml> <input.cljc> | \
     --run-files <input.cljc>... | \
     --run-files-from <state> <implementation.ml> <input.cljc>... | --lsp. \
     Batch commands default to all .clj, .cljc, .cljs, and .lgi files in the \
     current directory.";
  exit 2

let tune_compiler_gc () =
  let control = Gc.get () in
  let minor_heap_size = 16 * 1024 * 1024 in
  if control.minor_heap_size < minor_heap_size || control.space_overhead < 200
  then
    Gc.set
      {
        control with
        minor_heap_size = max control.minor_heap_size minor_heap_size;
        space_overhead = max control.space_overhead 200;
      }

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let length = in_channel_length ic in
      really_input_string ic length)

let source_extensions = [ ".lgi"; ".clj"; ".cljc"; ".cljs" ]

let has_source_extension path =
  List.exists (Filename.check_suffix path) source_extensions

let expand_input_path path =
  if Sys.file_exists path && Sys.is_directory path then
    Sys.readdir path |> Array.to_list |> List.sort String.compare
    |> List.filter has_source_extension
    |> List.map (Filename.concat path)
  else if Sys.file_exists path then [ path ]
  else
    let sources =
      source_extensions
      |> List.map (fun extension -> path ^ extension)
      |> List.filter Sys.file_exists
    in
    if sources = [] then [ path ] else sources

let expand_input_paths paths =
  let paths = if paths = [] then [ "." ] else paths in
  List.concat_map expand_input_path paths

let write_output output_path contents =
  match output_path with
  | None -> print_string contents
  | Some path ->
      let oc = open_out path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () -> output_string oc contents)

type cached_prefix_output = {
  source_packages : string list;
  compilation : Lg.Compiler.compilation;
}

type compiler_state =
  | Live of Lg.Compiler.state
  | Replayed of Lg.Compiler.state

type saved_compilation_state = {
  target : Lg.Target.t;
  state : Lg.Compiler.state;
  packages : string list;
  ocaml_source : string;
}

let compiler_error message =
  Error
    { Lg.Compiler.code = "LG9000";
      phase = `Infrastructure;
      message;
      location = None }

let write_saved_compilation_state path saved =
  Compiler_artifact.write ~kind:"saved-state" ~path saved

let read_saved_compilation_state path =
  match Compiler_artifact.read ~kind:"saved-state" ~path with
  | Ok saved -> Ok (saved : saved_compilation_state)
  | Error message -> compiler_error message

let compile_cache_enabled () =
  Sys.getenv_opt "LG_DISABLE_COMPILE_CACHE" <> Some "1"

let compile_cache_min_seconds () =
  match Sys.getenv_opt "LG_COMPILE_CACHE_MIN_SECONDS" with
  | Some value -> Option.value (float_of_string_opt value) ~default:0.1
  | None -> 0.1

let default_compile_cache_max_bytes =
  Int64.mul 256L (Int64.mul 1024L 1024L)

let compile_cache_max_bytes () =
  match Sys.getenv_opt "LG_COMPILE_CACHE_MAX_BYTES" with
  | Some value ->
      Option.value (Int64.of_string_opt value)
        ~default:default_compile_cache_max_bytes
  | None -> default_compile_cache_max_bytes

let rec find_repo_root_opt dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then Some dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_repo_root_opt parent

let rec ensure_directory path =
  if Sys.file_exists path then ()
  else (
    ensure_directory (Filename.dirname path);
    try Unix.mkdir path 0o755 with
    | Unix.Unix_error (Unix.EEXIST, _, _) when Sys.is_directory path -> ())

let compile_cache_directory () =
  match Sys.getenv_opt "LG_CACHE_DIR" with
  | Some path -> Filename.concat path "compile-files"
  | None ->
      let base_directory =
        Option.value (find_repo_root_opt (Sys.getcwd ())) ~default:(Sys.getcwd ())
      in
      Filename.concat
        base_directory ".lg-cache/compile-files"

let compile_cache_lock_name = ".lock"

let with_compile_cache_lock action =
  let directory = compile_cache_directory () in
  ensure_directory directory;
  let lock_path = Filename.concat directory compile_cache_lock_name in
  let descriptor =
    Unix.openfile lock_path [ Unix.O_CREAT; Unix.O_RDWR ] 0o600
  in
  Fun.protect
    ~finally:(fun () -> Unix.close descriptor)
    (fun () ->
      Unix.lockf descriptor Unix.F_LOCK 0;
      Fun.protect
        ~finally:(fun () -> Unix.lockf descriptor Unix.F_ULOCK 0)
        action)

let compute_compiler_cache_identity () =
  let adjacent_compiler_directory =
    Filename.concat (Filename.dirname Sys.executable_name) "../src"
  in
  let compiler_artifacts directory =
    [ "lg.cmxa"; "lg.cma" ]
    |> List.map (Filename.concat directory)
    |> List.filter Sys.file_exists
  in
  let artifacts =
    match compiler_artifacts adjacent_compiler_directory with
    | _ :: _ as artifacts -> artifacts
    | [] ->
      find_repo_root_opt (Sys.getcwd ())
      |> Option.map (fun repo_root ->
             Filename.concat repo_root "_build/default/src")
      |> Option.map compiler_artifacts
      |> Option.value ~default:[]
      |> (function
           | _ :: _ as artifacts -> artifacts
           | [] -> [ Sys.executable_name ])
  in
  let artifact_identity path =
    Filename.basename path ^ "\000" ^ Digest.to_hex (Digest.file path)
  in
  String.concat "\000"
    (Sys.ocaml_version :: List.map artifact_identity artifacts)
  |> Digest.string |> Digest.to_hex

let compiler_cache_identity =
  let identity = lazy (compute_compiler_cache_identity ()) in
  fun () -> Lazy.force identity

let reader_target_cache_key = function
  | None -> "default"
  | Some target -> Lg.Target.to_string target

let next_prefix_key ~target ?reader_target previous_key input_path source =
  Digest.string
    (String.concat "\000"
       [
         previous_key;
         Lg.Target.to_string target;
         reader_target_cache_key reader_target;
         input_path;
         source;
       ])
  |> Digest.to_hex

let saved_state_prefix_key ~target ?reader_target state_path =
  Digest.string
    (String.concat "\000"
       [
         compiler_cache_identity ();
         "saved-state";
         Lg.Target.to_string target;
         reader_target_cache_key reader_target;
         Digest.to_hex (Digest.file state_path);
       ])
  |> Digest.to_hex

let compile_cache_generation_directory () =
  Filename.concat (compile_cache_directory ()) (compiler_cache_identity ())

let cache_path key suffix =
  Filename.concat (compile_cache_generation_directory ())
    (key ^ suffix ^ ".marshal")

type cache_entry_files = {
  paths : string list;
  size : int64;
  modified_at : float;
}

let cache_entry_key filename =
  let suffixes = [ ".output.marshal" ] in
  suffixes
  |> List.find_map (fun suffix ->
         if Filename.check_suffix filename suffix then
           Some
             (String.sub filename 0
                (String.length filename - String.length suffix))
         else None)

let rec remove_cache_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name -> remove_cache_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Sys.remove path

let prune_obsolete_cache_generations () =
  let root = compile_cache_directory () in
  if Sys.file_exists root then
    let current = compiler_cache_identity () in
    Sys.readdir root
    |> Array.iter (fun name ->
           if
             (not (String.equal name current))
             && not (String.equal name compile_cache_lock_name)
           then
             let path = Filename.concat root name in
             try remove_cache_tree path with
             | Sys_error _ | Unix.Unix_error _ -> ())

let prune_compile_cache_unlocked () =
  prune_obsolete_cache_generations ();
  let directory = compile_cache_generation_directory () in
  if Sys.file_exists directory then
    let entries = Hashtbl.create 128 in
    Sys.readdir directory
    |> Array.iter (fun filename ->
           match cache_entry_key filename with
           | None -> ()
           | Some key ->
               let path = Filename.concat directory filename in
               let stats = Unix.stat path in
               let existing =
                 Hashtbl.find_opt entries key
                 |> Option.value
                      ~default:{ paths = []; size = 0L; modified_at = 0. }
               in
               Hashtbl.replace entries key
                 {
                   paths = path :: existing.paths;
                   size = Int64.add existing.size (Int64.of_int stats.st_size);
                   modified_at = max existing.modified_at stats.st_mtime;
                 });
    let entries = Hashtbl.to_seq_values entries |> List.of_seq in
    let total =
      List.fold_left
        (fun total entry -> Int64.add total entry.size)
        0L entries
    in
    let maximum = max 0L (compile_cache_max_bytes ()) in
    if Int64.compare total maximum > 0 then
      let oldest_first =
        List.sort
          (fun left right -> Float.compare left.modified_at right.modified_at)
          entries
      in
      ignore
        (List.fold_left
           (fun remaining entry ->
             if Int64.compare remaining maximum <= 0 then remaining
             else (
               List.iter
                 (fun path -> if Sys.file_exists path then Sys.remove path)
                 entry.paths;
               Int64.sub remaining entry.size))
           total oldest_first)

let prune_compile_cache () =
  with_compile_cache_lock prune_compile_cache_unlocked

let touch_cache_entry key =
  let now = Unix.gettimeofday () in
  let path = cache_path key ".output" in
  if Sys.file_exists path then Unix.utimes path now now

let report_corrupt_cache_entry key messages =
  if Sys.getenv_opt "LG_COMPILE_CACHE_DEBUG" = Some "1" then
    Printf.eprintf "lg: compile cache ignored corrupt entry %s: %s\n%!" key
      (String.concat "; " messages)

let read_cached_prefix_output key =
  if not (compile_cache_enabled ()) then None
  else
    with_compile_cache_lock (fun () ->
        let output_path = cache_path key ".output" in
        if not (Sys.file_exists output_path) then None
        else
          match
            Compiler_artifact.read ~kind:"prefix-output" ~path:output_path
          with
          | Ok cached_output ->
            touch_cache_entry key;
            Some (cached_output : cached_prefix_output)
          | Error message ->
            report_corrupt_cache_entry key [ message ];
            Compiler_artifact.remove_if_present output_path;
            None)

let write_cached_prefix key output =
  if compile_cache_enabled () then
    try
      let started_at = Sys.time () in
      with_compile_cache_lock (fun () ->
          let directory = compile_cache_generation_directory () in
          ensure_directory directory;
          Compiler_artifact.write ~kind:"prefix-output"
            ~path:(cache_path key ".output") output);
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
  | Run_from of {
      state_path : string;
      implementation_path : string;
      input_path : string;
    }
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
      include_prefix : bool;
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
  | Run_files_from of {
      state_path : string;
      implementation_path : string;
      input_paths : string list;
    }
  | Lsp

let extract_compilation_options args =
  let reader_target_of_string = function
    | "clj" -> Ok Lg.Target.Native
    | "cljs" -> Ok Lg.Target.Melange
    | value -> Error ("unknown reader dialect " ^ value ^ "; expected clj or cljs")
  in
  let rec loop target reader_target reversed = function
    | [] -> (target, reader_target, List.rev reversed)
    | "--target" :: value :: rest -> (
        match Lg.Target.of_string value with
        | Ok target -> loop target reader_target reversed rest
        | Error message ->
            prerr_endline ("lg: " ^ message);
            exit 2)
    | "--reader-dialect" :: value :: rest -> (
        match reader_target_of_string value with
        | Ok reader_target -> loop target (Some reader_target) reversed rest
        | Error message ->
            prerr_endline ("lg: " ^ message);
            exit 2)
    | [ "--target" ] -> usage ()
    | [ "--reader-dialect" ] -> usage ()
    | argument :: rest -> loop target reader_target (argument :: reversed) rest
  in
  loop Lg.Target.default None [] args

let parse_args argv =
  let target, reader_target, args =
    extract_compilation_options (Array.to_list argv)
  in
  let mode =
    match args with
    | [ _program; "--lsp" ] -> Lsp
    | [ _program; "--interface"; input ] ->
        Interface { input_path = input; output_path = None }
    | [ _program; "--interface"; input; "-o"; output ] ->
        Interface { input_path = input; output_path = Some output }
    | [ _program; input ] when not (String.starts_with ~prefix:"--" input) ->
        Compile { input_path = input; output_path = None }
    | [ _program; input; "-o"; output ]
      when not (String.starts_with ~prefix:"--" input) ->
        Compile { input_path = input; output_path = Some output }
    | [ _program; "--run"; input ] -> Run { input_path = input }
    | [ _program; "--run-from"; state_path; implementation_path; input_path ] ->
        Run_from { state_path; implementation_path; input_path }
    | _program :: "--compile-files" :: args -> (
        match List.rev args with
        | output_path :: "-o" :: reversed_inputs ->
            Compile_files
              { input_paths = List.rev reversed_inputs; output_path }
        | _ -> usage ())
    | _program :: "--compile-files-state" :: state_path :: args -> (
        match List.rev args with
        | output_path :: "-o" :: reversed_inputs ->
            Compile_files_state
              {
                state_path;
                input_paths = List.rev reversed_inputs;
                output_path;
              }
        | _ -> usage ())
    | _program :: "--compile-files-from" :: state_path :: args -> (
        match List.rev args with
        | output_path :: "-o" :: reversed_inputs ->
            Compile_files_from
              {
                state_path;
                input_paths = List.rev reversed_inputs;
                output_path;
                include_prefix = true;
              }
        | _ -> usage ())
    | _program :: "--compile-files-chunk-from" :: state_path :: args -> (
        match List.rev args with
        | output_path :: "-o" :: reversed_inputs ->
            Compile_files_from
              {
                state_path;
                input_paths = List.rev reversed_inputs;
                output_path;
                include_prefix = false;
              }
        | _ -> usage ())
    | _program :: "--compile-files-from-state" :: state_path
      :: output_state_path :: args -> (
        match List.rev args with
        | output_path :: "-o" :: reversed_inputs ->
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
    | _program :: "--run-files" :: input_paths ->
        Run_files { input_paths }
    | _program :: "--run-files-from" :: state_path :: implementation_path
      :: input_paths ->
        Run_files_from { state_path; implementation_path; input_paths }
    | _ -> usage ()
  in
  (target, reader_target, mode)

type native_link_layout = {
  include_directories : string list;
  archives : string list;
}

let development_link_layout executable_directory =
  let build_directory = Filename.dirname executable_directory in
  let rrbvec_directory = Filename.concat build_directory "vendor/rrbvec" in
  let compiler_directory = Filename.concat build_directory "src" in
  let runtime_directory = Filename.concat build_directory "runtime" in
  let backend_directory =
    Filename.concat build_directory "runtime_edn_backend_native"
  in
  let compiler_archive = Filename.concat compiler_directory "lg.cmxa" in
  if not (Sys.file_exists compiler_archive) then None
  else
    Some
      {
        include_directories =
          [
            rrbvec_directory;
            Filename.concat rrbvec_directory ".rrbvec.objs/byte";
            compiler_directory;
            Filename.concat compiler_directory ".lg.objs/byte";
            Filename.concat compiler_directory ".lg.objs/native";
            runtime_directory;
            Filename.concat runtime_directory ".lg_runtime.objs/byte";
            Filename.concat runtime_directory ".lg_runtime.objs/native";
            Filename.concat backend_directory
              ".lg_edn_backend_native.objs/byte";
          ];
        archives =
          [
            Filename.concat rrbvec_directory "rrbvec.cmxa";
            Filename.concat backend_directory "lg_edn_backend_native.cmxa";
            Filename.concat runtime_directory "lg_runtime.cmxa";
            compiler_archive;
          ];
      }

let installed_link_layout executable_directory =
  let prefix = Filename.dirname executable_directory in
  let library_directory = Filename.concat prefix "lib/lg" in
  let rrbvec_directory = Filename.concat library_directory "rrbvec" in
  let runtime_directory = Filename.concat library_directory "runtime" in
  let backend_directory = Filename.concat library_directory "edn-backend/native" in
  let compiler_archive = Filename.concat library_directory "lg.cmxa" in
  if not (Sys.file_exists compiler_archive) then None
  else
    Some
      {
        include_directories =
          [
            library_directory;
            rrbvec_directory;
            runtime_directory;
            backend_directory;
          ];
        archives =
          [
            Filename.concat rrbvec_directory "rrbvec.cmxa";
            Filename.concat backend_directory "lg_edn_backend_native.cmxa";
            Filename.concat runtime_directory "lg_runtime.cmxa";
            compiler_archive;
          ];
      }

let native_link_layout () =
  let executable_path =
    if Filename.is_relative Sys.executable_name then
      Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name
  in
  let executable_directory = Filename.dirname executable_path in
  match development_link_layout executable_directory with
  | Some _ as layout -> layout
  | None -> installed_link_layout executable_directory

let run_ocaml_source packages ocaml_source =
  let ml_path = Filename.temp_file "lg" ".ml" in
  let exe_path = Filename.temp_file "lg" ".exe" in
  write_output (Some ml_path) ocaml_source;
  let packages =
    List.sort_uniq String.compare
      ("melange-edn-native" :: "re" :: "unix" :: packages)
    |> List.filter (fun package ->
           not
             (List.mem package
                [ "lg";
                  "lg.runtime";
                  "lg.rrbvec";
                  "lg.edn-backend";
                  "lg.edn-backend.native" ]))
  in
  let package_options =
    "-package " ^ Filename.quote (String.concat "," packages) ^ " -linkpkg "
  in
  let layout =
    match native_link_layout () with
    | Some layout -> layout
    | None ->
      prerr_endline "lg: could not locate installed native runtime artifacts";
      exit 2
  in
  let include_options =
    layout.include_directories
    |> List.map (fun directory -> "-I " ^ Filename.quote directory)
    |> String.concat " "
  in
  let archives =
    layout.archives |> List.map Filename.quote |> String.concat " "
  in
  let compile_cmd =
    Printf.sprintf "ocamlfind ocamlopt %s%s -o %s %s %s" package_options
      include_options (Filename.quote exe_path) archives (Filename.quote ml_path)
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
  | Live state | Replayed state -> Ok state

let resume_compiler_state ~target ~packages ~sources = function
  | Live state -> Ok state
  | Replayed state ->
      Lg.Compiler.restore_ocaml_environment ~target ~packages state sources

let resume_saved_compiler_state ~target ~packages = function
  | Live state -> Ok state
  | Replayed state ->
      Lg.Compiler.restore_ocaml_environment ~target ~packages state []

let replay_cached_prefix compiler_state prepared =
  Result.bind (read_compiler_state compiler_state) (fun state ->
      Lg.Compiler.compile_prepared_chunk_with_diagnostics ~check_ocaml:false
        state prepared
      |> Result.map fst)

let order_prepared_sources ?reader_target target compiler_state sources =
  let source_texts =
    List.map (fun (path, source, _prepared) -> (path, source)) sources
  in
  match
    Lg.Toolchain.order_workspace_from_state ~target ?reader_target compiler_state
      source_texts
  with
  | Error _ as error -> error
  | Ok paths ->
      let rec reorder ordered = function
        | [] -> Ok (List.rev ordered)
        | path :: rest -> (
            match
              List.find_opt
                (fun (source_path, _source, _prepared) -> source_path = path)
                sources
            with
            | Some source -> reorder (source :: ordered) rest
            | None -> compiler_error ("missing analyzed source " ^ path))
      in
      reorder [] paths

let order_input_paths ?reader_target target compiler_state input_paths =
  let sources = List.map (fun path -> (path, read_file path)) input_paths in
  Lg.Toolchain.order_workspace_from_state ~target ?reader_target compiler_state
    sources

let compile_files ?(use_cache = true) ?reader_target target input_paths =
  let input_paths = expand_input_paths input_paths in
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
          next_prefix_key ~target ?reader_target prefix_key input_path source
        in
        match
          Lg.Compiler.prepare_source ~target ?reader_target
            ~filename:input_path source
        with
        | Error _ as err -> err
        | Ok prepared -> (
            let source_packages =
              Lg.Compiler.prepared_source_required_packages prepared
            in
            match
              if use_cache then read_cached_prefix_output prefix_key else None
            with
            | Some cached ->
                report_cache_hit input_path;
                Result.bind
                  (replay_cached_prefix compiler_state prepared)
                  (fun state ->
                    loop prefix_key (Replayed state)
                      (List.rev_append cached.source_packages packages)
                      (cached.compilation.ocaml_source :: outputs)
                      (cached.compilation.diagnostics :: diagnostics)
                      rest)
            | None ->
                Result.bind
                  (resume_compiler_state ~target ~packages
                     ~sources:(List.rev outputs) compiler_state)
                  (fun state ->
                    if Sys.getenv_opt "LG_COMPILE_TIMINGS" = Some "1" then
                      Printf.eprintf "lg: compiling %s\n%!" input_path;
                    let started_at = Sys.time () in
                    match
                      Lg.Compiler.compile_prepared_chunk_with_diagnostics state
                        prepared
                    with
                    | Error _ as err -> err
                    | Ok (state, compilation) ->
                        if
                          use_cache
                          &&
                          Sys.time () -. started_at
                          >= compile_cache_min_seconds ()
                        then
                          write_cached_prefix prefix_key
                            { source_packages; compilation };
                        loop prefix_key (Live state)
                          (List.rev_append source_packages packages)
                          (compilation.ocaml_source :: outputs)
                          (compilation.diagnostics :: diagnostics)
                          rest)))
  in
  let initial_prefix_key =
    if use_cache then compiler_cache_identity () else "cache-disabled"
  in
  let result =
    Result.bind
      (order_input_paths ?reader_target target Lg.Compiler.empty_state input_paths)
      (fun input_paths ->
        loop initial_prefix_key (Live Lg.Compiler.empty_state) [] [] []
          input_paths)
  in
  if use_cache then prune_compile_cache ();
  result

let compile_file ?reader_target target input_path =
  let source = read_file input_path in
  match
    Lg.Compiler.prepare_source ~target ?reader_target ~filename:input_path source
  with
  | Error _ as err -> err
  | Ok prepared ->
      let packages = Lg.Compiler.prepared_source_required_packages prepared in
      Lg.Compiler.compile_prepared_chunk_with_diagnostics Lg.Compiler.empty_state
        prepared
      |> Result.map (fun (_state, compilation) -> (packages, compilation))

let compile_chunk_from_saved_state ?reader_target target state_path input_path =
  Result.bind (read_saved_compilation_state state_path) (fun saved ->
      if saved.target <> target then
        compiler_error "saved compiler state target does not match --target"
      else
        let source = read_file input_path in
        Result.bind
          (Lg.Compiler.prepare_source ~target ?reader_target
             ~filename:input_path source)
          (fun prepared ->
            let source_packages =
              Lg.Compiler.prepared_source_required_packages prepared
            in
            let packages =
              List.sort_uniq String.compare (source_packages @ saved.packages)
            in
            Result.bind
              (Lg.Compiler.restore_ocaml_environment ~target ~packages saved.state
                 [])
              (fun state ->
                Lg.Compiler.compile_prepared_chunk_with_diagnostics
                  ~check_ocaml:false state prepared
                |> Result.map (fun (state, compilation) ->
                       (state, packages, compilation)))))

let compile_files_from_saved_state ?(use_cache = true) ?reader_target target
    state_path input_paths =
  let input_paths = expand_input_paths input_paths in
  match read_saved_compilation_state state_path with
  | Error _ as error -> error
  | Ok saved ->
    if saved.target <> target then
      compiler_error "saved compiler state target does not match --target"
    else
    Result.bind
      (order_input_paths ?reader_target target saved.state input_paths)
      (fun input_paths ->
    let result =
    let rec read_sources sources packages = function
      | [] -> Ok (List.rev sources, List.sort_uniq String.compare packages)
      | input_path :: rest ->
          let source = read_file input_path in
          Result.bind
            (Lg.Compiler.prepare_source ~target ?reader_target
               ~filename:input_path source)
            (fun prepared ->
              let source_packages =
                Lg.Compiler.prepared_source_required_packages prepared
              in
              read_sources ((input_path, source, prepared) :: sources)
                (List.rev_append source_packages packages)
                rest)
    in
    Result.bind
      (read_sources [] saved.packages input_paths)
      (fun (sources, packages) ->
        Result.bind
          (Lg.Compiler.restore_ocaml_environment ~target ~packages saved.state [])
          (fun initial_state ->
            Result.bind
              (order_prepared_sources ?reader_target target initial_state sources)
              (fun sources ->
        let rec compile prefix_key compiler_state outputs diagnostics =
          function
          | [] ->
              Result.map
                (fun state ->
                  ( state,
                    packages,
                    concatenate_compilation_outputs (List.rev outputs),
                    List.concat (List.rev diagnostics) ))
                (read_compiler_state compiler_state)
          | (input_path, source, prepared) :: rest -> (
              let prefix_key =
                next_prefix_key ~target ?reader_target prefix_key input_path source
              in
              match
                if use_cache then read_cached_prefix_output prefix_key else None
              with
              | Some cached ->
                  report_cache_hit input_path;
                  Result.bind
                    (replay_cached_prefix compiler_state prepared)
                    (fun state ->
                      compile prefix_key (Replayed state)
                        (cached.compilation.ocaml_source :: outputs)
                        (cached.compilation.diagnostics :: diagnostics)
                        rest)
              | None ->
                  Result.bind
                    (resume_saved_compiler_state ~target ~packages compiler_state)
                    (fun state ->
                      if Sys.getenv_opt "LG_COMPILE_TIMINGS" = Some "1" then
                        Printf.eprintf "lg: compiling %s\n%!" input_path;
                      let started_at = Sys.time () in
                      match
                        Lg.Compiler.compile_prepared_chunk_with_diagnostics
                          ~check_ocaml:false state prepared
                      with
                      | Error _ as err -> err
                      | Ok (state, compilation) ->
                          if
                            use_cache
                            &&
                            Sys.time () -. started_at
                            >= compile_cache_min_seconds ()
                          then
                            write_cached_prefix prefix_key
                              {
                                source_packages = [];
                                compilation;
                              };
                          compile prefix_key (Live state)
                            (compilation.ocaml_source :: outputs)
                            (compilation.diagnostics :: diagnostics)
                            rest))
        in
        let initial_prefix_key =
          if use_cache then
            saved_state_prefix_key ~target ?reader_target state_path
          else "cache-disabled"
        in
        compile initial_prefix_key (Live initial_state) [] [] sources)))
    in
    if use_cache then prune_compile_cache ();
    result)

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
  prerr_endline
    (location ^ "lg: " ^ err.Lg.Compiler.message ^ " ["
   ^ err.Lg.Compiler.code ^ "]");
  exit 1

let run_lsp () =
  let executable_directory = Filename.dirname Sys.executable_name in
  let adjacent_executables =
    [
      Filename.concat executable_directory "lg_lsp.exe";
      Filename.concat executable_directory "lg-lsp";
    ]
  in
  match List.find_opt Sys.file_exists adjacent_executables with
  | Some executable -> Unix.execv executable [| executable |]
  | None -> Unix.execvp "lg-lsp" [| "lg-lsp" |]

let () =
  let target, reader_target, mode = parse_args Sys.argv in
  (match mode with Lsp -> () | _ -> tune_compiler_gc ());
  match mode with
  | Compile { input_path; output_path } -> (
      match compile_file ?reader_target target input_path with
      | Error err -> report_error err
      | Ok (_packages, compilation) ->
          report_diagnostics compilation.diagnostics;
          write_output output_path compilation.ocaml_source)
  | Interface { input_path; output_path } -> (
      match infer_interface target input_path with
      | Error err -> report_error err
      | Ok interface -> write_output output_path interface)
  | Run { input_path } -> (
      match compile_file ?reader_target target input_path with
      | Error err -> report_error err
      | Ok (packages, compilation) ->
          report_diagnostics compilation.diagnostics;
          run_ocaml_source packages compilation.ocaml_source)
  | Run_from { state_path; implementation_path; input_path } -> (
      match
        compile_chunk_from_saved_state ?reader_target target state_path input_path
      with
      | Error err -> report_error err
      | Ok (_state, packages, compilation) ->
          report_diagnostics compilation.diagnostics;
          run_ocaml_source packages
            (concatenate_compilation_outputs
               [ read_file implementation_path; compilation.ocaml_source ]))
  | Compile_files { input_paths; output_path } -> (
      match compile_files ?reader_target target input_paths with
      | Error err -> report_error err
      | Ok (_state, _packages, ocaml_source, diagnostics) ->
          report_diagnostics diagnostics;
          write_output (Some output_path) ocaml_source)
  | Compile_files_state { state_path; input_paths; output_path } -> (
      match
        compile_files ~use_cache:false ?reader_target target input_paths
      with
      | Error err -> report_error err
      | Ok (state, packages, ocaml_source, diagnostics) ->
          report_diagnostics diagnostics;
          write_output (Some output_path) ocaml_source;
          write_saved_compilation_state state_path
            {
              target;
              state = Lg.Compiler.cacheable_state state;
              packages;
              ocaml_source;
            })
  | Compile_files_from
      { state_path; input_paths; output_path; include_prefix } -> (
      match
        compile_files_from_saved_state ?reader_target target state_path input_paths
      with
      | Error err -> report_error err
      | Ok (_state, _packages, ocaml_source, diagnostics) ->
          report_diagnostics diagnostics;
          let ocaml_source =
            if include_prefix then
              let saved =
                match read_saved_compilation_state state_path with
                | Ok saved -> saved
                | Error err -> report_error err
              in
              concatenate_compilation_outputs
                [ saved.ocaml_source; ocaml_source ]
            else ocaml_source
          in
          write_output (Some output_path) ocaml_source)
  | Compile_files_from_state
      { state_path; output_state_path; input_paths; output_path } -> (
      match
        compile_files_from_saved_state ~use_cache:false ?reader_target target
          state_path input_paths
      with
      | Error err -> report_error err
      | Ok (state, packages, ocaml_source, diagnostics) ->
          report_diagnostics diagnostics;
          let saved =
            match read_saved_compilation_state state_path with
            | Ok saved -> saved
            | Error err -> report_error err
          in
          let ocaml_source =
            concatenate_compilation_outputs
              [ saved.ocaml_source; ocaml_source ]
          in
          write_output (Some output_path) ocaml_source;
          write_saved_compilation_state output_state_path
            {
              target;
              state = Lg.Compiler.cacheable_state state;
              packages;
              ocaml_source;
            })
  | Compile_chunk_from { state_path; input_path; output_path } -> (
      match
        compile_chunk_from_saved_state ?reader_target target state_path input_path
      with
      | Error err -> report_error err
      | Ok (_state, _packages, compilation) ->
          report_diagnostics compilation.diagnostics;
          write_output output_path compilation.ocaml_source)
  | Compile_chunk_state
      { state_path; output_state_path; input_path; output_path } -> (
      match
        compile_chunk_from_saved_state ?reader_target target state_path input_path
      with
      | Error err -> report_error err
      | Ok (state, packages, compilation) ->
          report_diagnostics compilation.diagnostics;
          write_output output_path compilation.ocaml_source;
          let saved =
            match read_saved_compilation_state state_path with
            | Ok saved -> saved
            | Error err -> report_error err
          in
          write_saved_compilation_state output_state_path
            {
              target;
              state = Lg.Compiler.cacheable_state state;
              packages;
              ocaml_source =
                concatenate_compilation_outputs
                  [ saved.ocaml_source; compilation.ocaml_source ];
            })
  | Run_files { input_paths } -> (
      match compile_files ?reader_target target input_paths with
      | Error err -> report_error err
      | Ok (_state, packages, ocaml_source, diagnostics) ->
          report_diagnostics diagnostics;
          run_ocaml_source packages ocaml_source)
  | Run_files_from
      { state_path; implementation_path; input_paths } -> (
      match
        compile_files_from_saved_state ?reader_target target state_path input_paths
      with
      | Error err -> report_error err
      | Ok (_state, packages, ocaml_source, diagnostics) ->
          report_diagnostics diagnostics;
          run_ocaml_source packages
            (concatenate_compilation_outputs
               [ read_file implementation_path; ocaml_source ]))
  | Lsp -> run_lsp ()
