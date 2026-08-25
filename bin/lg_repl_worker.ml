module Reader = Lg_repl.Reader
module Session = Lg_repl.Session

let usage () =
  prerr_endline "Usage: lg repl [--state <lg_stdlib_native.state>]";
  exit 2

let rec find_repo_root_opt directory =
  if Sys.file_exists (Filename.concat directory "dune-project") then
    Some directory
  else
    let parent = Filename.dirname directory in
    if String.equal parent directory then None else find_repo_root_opt parent

let default_state_path () =
  let executable_directory = Filename.dirname Sys.executable_name in
  let repository_state =
    find_repo_root_opt (Sys.getcwd ())
    |> Option.map (fun root ->
           Filename.concat root "stdlib/lg_stdlib_native.state")
  in
  let candidates =
    Option.to_list (Sys.getenv_opt "LG_STDLIB_STATE")
    @ [
        Filename.concat executable_directory "../stdlib/lg_stdlib_native.state";
        Filename.concat executable_directory
          "../lib/lg/stdlib/lg_stdlib_native.state";
        Filename.concat executable_directory "lg_stdlib_native.state";
      ]
    @ Option.to_list repository_state
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None ->
      prerr_endline
        "lg: unable to find lg_stdlib_native.state; pass --state or set \
         LG_STDLIB_STATE";
      exit 2

let state_path argv =
  match Array.to_list argv with
  | [ _program ] -> default_state_path ()
  | [ _program; "--state"; path ] -> path
  | _ -> usage ()

let report_error (error : Lg.Compiler.compile_error) =
  let location =
    match error.location with
    | None -> ""
    | Some location -> Format.asprintf "%a: " Location.print_loc location
  in
  Printf.eprintf "%slg: %s [%s]\n%!" location error.message error.code

let print_evaluation (evaluation : Session.evaluation) =
  match evaluation.outcome with
  | Session.Value value ->
      Printf.printf "%s : %s\n%!" value.rendered value.type_name
  | Session.Definition definition ->
      Printf.printf "%s : %s\n%!" definition.name definition.type_name
  | Session.Namespace namespace -> Printf.printf "namespace %s\n%!" namespace
  | Session.Summary summary -> Printf.printf "%s\n%!" summary

type processing = Done | Need_more of string | Quit

let rec process_forms session source =
  match Reader.read source with
  | Reader.Empty -> Done
  | Reader.Incomplete -> Need_more source
  | Reader.Invalid error ->
      report_error error;
      Done
  | Reader.Complete complete ->
      (match Session.eval session complete.source with
      | Ok evaluation -> print_evaluation evaluation
      | Error error -> report_error error);
      process_forms session complete.remaining

let process_type_query session source =
  match Reader.read source with
  | Reader.Empty | Reader.Incomplete -> Need_more (":type " ^ source)
  | Reader.Invalid error ->
      report_error error;
      Done
  | Reader.Complete { source; remaining } ->
      if not (String.equal (String.trim remaining) "") then (
        prerr_endline "lg: :type expects exactly one form";
        Done)
      else (
        match Session.type_of session source with
        | Ok type_name ->
            print_endline type_name;
            Done
        | Error error ->
            report_error error;
            Done)

let process session source =
  let input = String.trim source in
  if String.equal input ":quit" || String.equal input ":q" then Quit
  else if
    String.equal input ":type"
    || String.starts_with ~prefix:":type " input
    || String.starts_with ~prefix:":type\n" input
  then
    let expression =
      String.sub input 5 (String.length input - 5) |> String.trim
    in
    process_type_query session expression
  else process_forms session source

let run session =
  let interactive = Unix.isatty Unix.stdin in
  let rec loop pending =
    if interactive then (
      print_string
        (if String.equal pending "" then Session.prompt session else "... ");
      flush stdout);
    match input_line stdin with
    | line ->
        let source =
          if String.equal pending "" then line else pending ^ "\n" ^ line
        in
        (match process session source with
        | Done -> loop ""
        | Need_more remaining -> loop remaining
        | Quit -> ())
    | exception End_of_file ->
        if not (String.equal (String.trim pending) "") then
          prerr_endline "lg: incomplete form at end of input"
  in
  loop ""

let () =
  let state_path = state_path Sys.argv in
  match Session.create_from_stdlib ~state_path with
  | Error error ->
      report_error error;
      exit 1
  | Ok session -> run session
