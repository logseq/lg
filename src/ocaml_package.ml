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
  let nested =
    if Sys.file_exists directory && Sys.is_directory directory then
      Sys.readdir directory |> Array.to_list
      |> List.map (Filename.concat directory)
      |> List.filter contains_compiled_interface
    else []
  in
  directory :: nested

let query_cache = Hashtbl.create 8

let query package =
  if not (valid_name package) then
    Error.error ("invalid OCaml package name " ^ package)
  else
    match Hashtbl.find_opt query_cache package with
    | Some result -> result
    | None ->
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
          | WEXITED 0 -> Ok directories
          | WEXITED _ | WSIGNALED _ | WSTOPPED _ ->
              Error.error ("OCaml package " ^ package ^ " was not found")
        in
        Hashtbl.replace query_cache package result;
        result

let include_dirs packages =
  let rec loop directories = function
    | [] -> Ok (List.sort_uniq String.compare directories)
    | package :: rest -> (
        match query package with
        | Error _ as err -> err
        | Ok package_dirs -> loop (List.rev_append package_dirs directories) rest)
  in
  loop [] packages
