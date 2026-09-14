let valid_name name =
  name <> ""
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' -> true
         | _ -> false)
       name

let read_lines channel =
  let rec loop acc =
    match input_line channel with
    | line -> loop (if line = "" then acc else line :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop []

let contains_compiled_interface directory =
  Sys.file_exists directory && Sys.is_directory directory
  && Sys.readdir directory
     |> Array.exists (String.ends_with ~suffix:".cmi")

let expand_include_directory directory =
  let rec compiled_directories directory =
    if Sys.file_exists directory && Sys.is_directory directory then
      let children =
        Sys.readdir directory |> Array.to_list
        |> List.map (Filename.concat directory)
        |> List.filter (fun path ->
               Sys.file_exists path && Sys.is_directory path)
        |> List.concat_map compiled_directories
      in
      if contains_compiled_interface directory then directory :: children
      else children
    else []
  in
  directory :: compiled_directories directory

let unique_directories directories =
  List.fold_left
    (fun unique directory ->
      if List.mem directory unique then unique else unique @ [ directory ])
    [] directories

let direct_ocamlpath_directories package =
  let separator = if Sys.win32 then ';' else ':' in
  let package_components = String.split_on_char '.' package in
  [ Sys.getenv_opt "OCAMLPATH"; Sys.getenv_opt "LG_OCAML_INCLUDE_PATH" ]
  |> List.filter_map Fun.id
  |> String.concat (String.make 1 separator)
  |> String.split_on_char separator
  |> List.filter_map (fun root ->
      if root = "" then None
      else if contains_compiled_interface root then Some root
      else
        let directory =
          List.fold_left Filename.concat root package_components
        in
        if contains_compiled_interface directory then Some directory else None)
  |> List.concat_map expand_include_directory
  |> unique_directories

let query_cache = Hashtbl.create 8

let authoritative_include_path () =
  match Sys.getenv_opt "LG_OCAML_INCLUDE_PATH_AUTHORITATIVE" with
  | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
  | _ -> false

let query package =
  if not (valid_name package) then
    Error.error ("invalid OCaml package name " ^ package)
  else
    let cache_key =
      package ^ "\000"
      ^ (Sys.getenv_opt "OCAMLPATH" |> Option.value ~default:"")
      ^ "\000"
      ^ (Sys.getenv_opt "LG_OCAML_INCLUDE_PATH" |> Option.value ~default:"")
    in
    match Hashtbl.find_opt query_cache cache_key with
    | Some result -> result
    | None ->
        let direct_dirs = direct_ocamlpath_directories package in
        let argv = [| "ocamlfind"; "query"; "-r"; "-format"; "%d"; package |] in
        let stdout, stdin, stderr =
          Unix.open_process_args_full "ocamlfind" argv (Unix.environment ())
        in
        let directories =
          read_lines stdout |> List.concat_map expand_include_directory
          |> List.sort_uniq String.compare
        in
        let _diagnostic = read_lines stderr in
        let result =
          match Unix.close_process_full (stdout, stdin, stderr) with
          | WEXITED 0 ->
              let directories =
                if authoritative_include_path ()
                then
                  match direct_dirs with
                  | _ :: _ -> direct_dirs
                  | [] -> directories
                else direct_dirs @ directories
              in
              Ok (unique_directories directories)
          | WEXITED _ | WSIGNALED _ | WSTOPPED _ -> (
              match direct_dirs with
              | _ :: _ as directories -> Ok directories
              | [] ->
                  Error.error ("OCaml package " ^ package ^ " was not found"))
        in
        Hashtbl.replace query_cache cache_key result;
        result

let include_dirs packages =
  let rec loop directories = function
    | [] -> Ok (unique_directories directories)
    | package :: rest -> (
        match query package with
        | Error _ as err -> err
        | Ok package_dirs -> loop (directories @ package_dirs) rest)
  in
  loop [] packages
