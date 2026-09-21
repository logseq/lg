module Protocol = Lg_repl.Protocol
module Reader = Lg_repl.Reader
module Session = Lg_repl.Session
module Nrepl_server = Lg_repl.Nrepl_server
module Socket_session = Lg_repl.Socket_session

let usage status =
  let output = if status = 0 then stdout else stderr in
  output_string output
    "Usage:\n\
    \  lg repl [--state <lg_stdlib_native.state>]\n\
    \  lg repl --listen [HOST:]PORT [--state <path>] [--port-file <path>]\n\
    \  lg repl --nrepl-listen [HOST:]PORT [--state <path>] [--port-file \
     <path>]\n\
    \  lg repl --connect HOST:PORT\n";
  flush output;
  exit status

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

type command_line = {
  state_path : string option;
  listen : string option;
  nrepl_listen : string option;
  connect : string option;
  port_file : string option;
  socket_session : bool;
  nrepl_connection : bool;
}

let empty_command_line =
  {
    state_path = None;
    listen = None;
    nrepl_listen = None;
    connect = None;
    port_file = None;
    socket_session = false;
    nrepl_connection = false;
  }

let set_once current value update options =
  match current with None -> update value options | Some _ -> usage 2

let parse_command_line argv =
  let rec loop index options =
    if index = Array.length argv then options
    else
      match argv.(index) with
      | "--help" | "-h" -> usage 0
      | "--state" when index + 1 < Array.length argv ->
          let path = argv.(index + 1) in
          let options =
            set_once options.state_path path
              (fun state_path options -> { options with state_path = Some state_path })
              options
          in
          loop (index + 2) options
      | "--listen" when index + 1 < Array.length argv ->
          let endpoint = argv.(index + 1) in
          let options =
            set_once options.listen endpoint
              (fun listen options -> { options with listen = Some listen })
              options
          in
          loop (index + 2) options
      | "--nrepl-listen" when index + 1 < Array.length argv ->
          let endpoint = argv.(index + 1) in
          let options =
            set_once options.nrepl_listen endpoint
              (fun nrepl_listen options ->
                { options with nrepl_listen = Some nrepl_listen })
              options
          in
          loop (index + 2) options
      | "--connect" when index + 1 < Array.length argv ->
          let endpoint = argv.(index + 1) in
          let options =
            set_once options.connect endpoint
              (fun connect options -> { options with connect = Some connect })
              options
          in
          loop (index + 2) options
      | "--port-file" when index + 1 < Array.length argv ->
          let path = argv.(index + 1) in
          let options =
            set_once options.port_file path
              (fun port_file options -> { options with port_file = Some port_file })
              options
          in
          loop (index + 2) options
      | "--socket-session" when not options.socket_session ->
          loop (index + 1) { options with socket_session = true }
      | "--nrepl-connection" when not options.nrepl_connection ->
          loop (index + 1) { options with nrepl_connection = true }
      | _ -> usage 2
  in
  loop 1 empty_command_line

type endpoint = {
  host : string;
  address : Unix.inet_addr;
  port : int;
}

let resolve_ipv4 host =
  try Unix.inet_addr_of_string host
  with Failure _ ->
    try
      let entry = Unix.gethostbyname host in
      if Array.length entry.h_addr_list = 0 then
        failwith (Printf.sprintf "host %s has no IPv4 address" host)
      else entry.h_addr_list.(0)
    with Not_found -> failwith (Printf.sprintf "unknown host %s" host)

let parse_port text =
  match int_of_string_opt text with
  | Some port when port >= 0 && port <= 65535 -> port
  | _ -> failwith (Printf.sprintf "invalid TCP port %s" text)

let parse_endpoint ~default_host text =
  let host, port =
    match String.split_on_char ':' text with
    | [ port ] -> (default_host, parse_port port)
    | [ host; port ] when not (String.equal host "") -> (host, parse_port port)
    | _ -> failwith (Printf.sprintf "invalid endpoint %s" text)
  in
  { host; address = resolve_ipv4 host; port }

let is_loopback address =
  let text = Unix.string_of_inet_addr address in
  String.starts_with ~prefix:"127." text

let absolute_path path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path

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

type frontend = {
  prompt : unit -> string;
  evaluate : string -> unit;
  type_of : string -> unit;
  close : unit -> unit;
}

type processing = Done | Need_more of string | Quit

let rec process_forms frontend source =
  match Reader.read source with
  | Reader.Empty -> Done
  | Reader.Incomplete -> Need_more source
  | Reader.Invalid error ->
      report_error error;
      Done
  | Reader.Complete complete ->
      frontend.evaluate complete.source;
      process_forms frontend complete.remaining

let process_type_query frontend source =
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
        frontend.type_of source;
        Done)

let process frontend source =
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
    process_type_query frontend expression
  else process_forms frontend source

let run frontend =
  let interactive = Unix.isatty Unix.stdin in
  let rec loop pending =
    if interactive then (
      print_string (if String.equal pending "" then frontend.prompt () else "... ");
      flush stdout);
    match input_line stdin with
    | line ->
        let source =
          if String.equal pending "" then line else pending ^ "\n" ^ line
        in
        (match process frontend source with
        | Done -> loop ""
        | Need_more remaining -> loop remaining
        | Quit -> ())
    | exception End_of_file ->
        if not (String.equal (String.trim pending) "") then
          prerr_endline "lg: incomplete form at end of input"
  in
  Fun.protect ~finally:frontend.close (fun () -> loop "")

let local_frontend session =
  {
    prompt = (fun () -> Session.prompt session);
    evaluate =
      (fun source ->
        match Session.eval session source with
        | Ok evaluation -> print_evaluation evaluation
        | Error error -> report_error error);
    type_of =
      (fun source ->
        match Session.type_of session source with
        | Ok type_name -> print_endline type_name
        | Error error -> report_error error);
    close = (fun () -> ());
  }

exception Socket_repl_error of string

type remote = {
  input : in_channel;
  output : out_channel;
  mutable namespace : string;
  mutable next_id : int;
  mutable closed : bool;
}

let response_id = function
  | Protocol.Description { id; _ }
  | Protocol.Stdout { id; _ }
  | Protocol.Stderr { id; _ }
  | Protocol.Value { id; _ }
  | Protocol.Definition { id; _ }
  | Protocol.Summary { id; _ }
  | Protocol.Namespace { id; _ }
  | Protocol.Type_result { id; _ }
  | Protocol.Lookup_result { id; _ }
  | Protocol.Completions_result { id; _ }
  | Protocol.Diagnostic { id; _ }
  | Protocol.Status { id; _ } -> id

let render_response remote = function
  | Protocol.Description
      { protocol_version; target = Protocol.Native_bytecode; namespace; _ } ->
      if protocol_version <> Protocol.protocol_version then
        raise
          (Socket_repl_error
             (Printf.sprintf "unsupported protocol version %d" protocol_version));
      remote.namespace <- namespace
  | Protocol.Stdout { text; _ } ->
      output_string stdout text;
      flush stdout
  | Protocol.Stderr { text; _ } ->
      output_string stderr text;
      flush stderr
  | Protocol.Value { rendered; type_name; _ } ->
      Printf.printf "%s : %s\n%!" rendered type_name
  | Protocol.Definition { name; type_name; _ } ->
      Printf.printf "%s : %s\n%!" name type_name
  | Protocol.Summary { text; _ } -> Printf.printf "%s\n%!" text
  | Protocol.Namespace { namespace; _ } ->
      Printf.printf "namespace %s\n%!" namespace
  | Protocol.Type_result { type_name; _ } -> Printf.printf "%s\n%!" type_name
  | Protocol.Lookup_result _ | Protocol.Completions_result _ -> ()
  | Protocol.Diagnostic { code; message; location; _ } ->
      let prefix = Option.fold ~none:"" ~some:(fun value -> value ^ ": ") location in
      Printf.eprintf "%slg: %s [%s]\n%!" prefix message code
  | Protocol.Status _ -> ()

let next_request_id remote =
  let id = string_of_int remote.next_id in
  remote.next_id <- remote.next_id + 1;
  id

let exchange remote request =
  let expected_id =
    match request with
    | Protocol.Evaluate request | Protocol.Load_file request
    | Protocol.Type_of request ->
        request.id
    | Protocol.Lookup request -> request.id
    | Protocol.Completions request -> request.id
    | Protocol.Describe { id } | Protocol.Close { id } -> id
  in
  (match Protocol.write_request remote.output request with
  | Ok () -> ()
  | Error message -> raise (Socket_repl_error message));
  let rec read_until_status () =
    match Protocol.read_response remote.input with
    | Error message -> raise (Socket_repl_error message)
    | Ok None -> raise (Socket_repl_error "connection closed before final status")
    | Ok (Some response) ->
        let actual_id = response_id response in
        if not (String.equal expected_id actual_id) then
          raise
            (Socket_repl_error
               (Printf.sprintf "expected response %s but received %s" expected_id
                  actual_id));
        render_response remote response;
        (match response with
        | Protocol.Status { namespace; status; _ } ->
            remote.namespace <- namespace;
            status
        | _ -> read_until_status ())
  in
  read_until_status ()

let remote_frontend remote =
  let source_request source =
    Protocol.
      {
        id = next_request_id remote;
        source;
        expected_namespace = Some remote.namespace;
        filename = None;
      }
  in
  {
    prompt = (fun () -> remote.namespace ^ "=> ");
    evaluate =
      (fun source ->
        ignore (exchange remote (Protocol.Evaluate (source_request source))));
    type_of =
      (fun source ->
        ignore (exchange remote (Protocol.Type_of (source_request source))));
    close =
      (fun () ->
        if not remote.closed then (
          remote.closed <- true;
          ignore
            (exchange remote (Protocol.Close { id = next_request_id remote }))));
  }

let connect_to_server endpoint =
  if endpoint.port = 0 then
    raise (Socket_repl_error "cannot connect to TCP port 0");
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  try
    Unix.connect socket (Unix.ADDR_INET (endpoint.address, endpoint.port));
    let input = Unix.in_channel_of_descr (Unix.dup socket) in
    let output = Unix.out_channel_of_descr socket in
    set_binary_mode_in input true;
    set_binary_mode_out output true;
    (input, output)
  with exn ->
    Unix.close socket;
    raise exn

let run_remote endpoint =
  let input, output = connect_to_server endpoint in
  let remote =
    { input; output; namespace = "user"; next_id = 1; closed = false }
  in
  Fun.protect
    ~finally:(fun () ->
      close_in_noerr input;
      close_out_noerr output)
    (fun () ->
      match exchange remote (Protocol.Describe { id = next_request_id remote }) with
      | Protocol.Done -> run (remote_frontend remote)
      | Protocol.Failed | Protocol.Unsupported ->
          raise (Socket_repl_error "server rejected the Describe request"))

let write_port_file path port =
  let output = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () -> Printf.fprintf output "%d\n" port)

type listener_kind = Socket_repl | Nrepl

let serve_connections kind endpoint ~state_path ~port_file =
  if not (is_loopback endpoint.address) then
    failwith "Socket REPL servers may only bind to an IPv4 loopback address";
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close socket)
    (fun () ->
      Unix.setsockopt socket Unix.SO_REUSEADDR true;
      Unix.set_close_on_exec socket;
      Unix.bind socket (Unix.ADDR_INET (endpoint.address, endpoint.port));
      Unix.listen socket 16;
      let port =
        match Unix.getsockname socket with
        | Unix.ADDR_INET (_, port) -> port
        | Unix.ADDR_UNIX _ -> assert false
      in
      Option.iter (fun path -> write_port_file path port) port_file;
      let name = match kind with Socket_repl -> "Socket REPL" | Nrepl -> "nREPL" in
      Printf.printf "LG %s listening on %s:%d\n%!" name endpoint.host port;
      Sys.set_signal Sys.sigchld Sys.Signal_ignore;
      Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
      let worker = Sys.executable_name in
      let child_mode =
        match kind with
        | Socket_repl -> "--socket-session"
        | Nrepl -> "--nrepl-connection"
      in
      let worker_argv =
        [| worker; child_mode; "--state"; absolute_path state_path |]
      in
      let rec accept () =
        match Unix.accept socket with
        | client, _ ->
            Unix.set_close_on_exec client;
            (try
               ignore
                 (Unix.create_process worker worker_argv client client Unix.stderr)
             with exn ->
               Printf.eprintf "lg: unable to start Socket REPL worker: %s\n%!"
                 (Printexc.to_string exn));
            Unix.close client;
            accept ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> accept ()
      in
      accept ())

let diagnostic_phase = function
  | `Lexing -> Protocol.Lexing
  | `Parsing -> Protocol.Parsing
  | `Semantic -> Protocol.Semantic
  | `Lowering -> Protocol.Lowering
  | `Ocaml -> Protocol.Ocaml
  | `Infrastructure -> Protocol.Infrastructure

let report_socket_startup_error output ~id
    (error : Lg.Compiler.compile_error) =
  let location =
    Option.map (fun location -> Format.asprintf "%a" Location.print_loc location)
      error.location
  in
  ignore
    (Protocol.write_response output
       (Protocol.Diagnostic
          {
            id;
            code = error.code;
            phase = diagnostic_phase error.phase;
            message = error.message;
            location;
          }));
  ignore
    (Protocol.write_response output
       (Protocol.Status { id; namespace = "user"; status = Protocol.Failed }))

let restore_sigchld () =
  (* The listener ignores SIGCHLD to auto-reap session children, and ignored
     dispositions survive execve. Child workers spawn subprocesses (dune via
     open_process_args_full) whose waitpid fails with ECHILD unless the
     disposition is reset. *)
  Sys.set_signal Sys.sigchld Sys.Signal_default

let run_socket_session state_path =
  restore_sigchld ();
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  match Session.create_from_stdlib ~state_path with
  | Error error -> (
      match Protocol.read_request stdin with
      | Ok None -> ()
      | Error _ -> report_socket_startup_error stdout ~id:"protocol" error
      | Ok (Some request) ->
          let id =
            match request with
            | Protocol.Evaluate request | Protocol.Load_file request
            | Protocol.Type_of request ->
                request.id
            | Protocol.Lookup request -> request.id
            | Protocol.Completions request -> request.id
            | Protocol.Describe { id } | Protocol.Close { id } -> id
          in
          report_socket_startup_error stdout ~id error)
  | Ok session -> Socket_session.serve session ~input:stdin ~output:stdout

let run_nrepl_connection state_path =
  restore_sigchld ();
  Nrepl_server.serve_connection ~worker_path:Sys.executable_name ~state_path
    ~input:stdin ~output:stdout

let run_local state_path =
  match Session.create_from_stdlib ~state_path with
  | Error error ->
      report_error error;
      exit 1
  | Ok session -> run (local_frontend session)

let selected_state_path = function
  | Some state_path -> state_path
  | None -> default_state_path ()

let main () =
  let options = parse_command_line Sys.argv in
  match
    ( options.socket_session,
      options.nrepl_connection,
      options.listen,
      options.nrepl_listen,
      options.connect )
  with
  | true, false, None, None, None when Option.is_none options.port_file ->
      let state_path = Option.value options.state_path ~default:"" in
      if String.equal state_path "" then usage 2 else run_socket_session state_path
  | false, true, None, None, None when Option.is_none options.port_file ->
      let state_path = Option.value options.state_path ~default:"" in
      if String.equal state_path "" then usage 2
      else run_nrepl_connection state_path
  | false, false, Some endpoint, None, None ->
      let state_path = selected_state_path options.state_path in
      let endpoint = parse_endpoint ~default_host:"127.0.0.1" endpoint in
      serve_connections Socket_repl endpoint ~state_path
        ~port_file:options.port_file
  | false, false, None, Some endpoint, None ->
      let state_path = selected_state_path options.state_path in
      let endpoint = parse_endpoint ~default_host:"127.0.0.1" endpoint in
      serve_connections Nrepl endpoint ~state_path ~port_file:options.port_file
  | false, false, None, None, Some endpoint
    when Option.is_none options.state_path && Option.is_none options.port_file ->
      let endpoint = parse_endpoint ~default_host:"127.0.0.1" endpoint in
      run_remote endpoint
  | false, false, None, None, None when Option.is_none options.port_file ->
      let state_path = selected_state_path options.state_path in
      run_local state_path
  | _ -> usage 2

let () =
  try main () with
  | Socket_repl_error message ->
      Printf.eprintf "lg: Socket REPL protocol error: %s\n%!" message;
      exit 1
  | Failure message ->
      Printf.eprintf "lg: %s\n%!" message;
      exit 1
  | Unix.Unix_error (error, operation, argument) ->
      let argument = if String.equal argument "" then "" else " " ^ argument in
      Printf.eprintf "lg: %s%s: %s\n%!" operation argument
        (Unix.error_message error);
      exit 1
