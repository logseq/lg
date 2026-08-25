module Bencode = Nrepl_bencode
module Internal = Protocol
module Reader = Reader
module Session_protocol = Nrepl_protocol

exception Connection_closed

type worker_session = {
  id : string;
  process : int;
  input : in_channel;
  output : out_channel;
  mutable namespace : string;
  mutable closed : bool;
}

let request_counter = ref 0

let next_id prefix =
  incr request_counter;
  Printf.sprintf "%s-%d-%d" prefix (Unix.getpid ()) !request_counter

let response_id = function
  | Internal.Description { id; _ }
  | Internal.Stdout { id; _ }
  | Internal.Stderr { id; _ }
  | Internal.Value { id; _ }
  | Internal.Definition { id; _ }
  | Internal.Summary { id; _ }
  | Internal.Namespace { id; _ }
  | Internal.Type_result { id; _ }
  | Internal.Lookup_result { id; _ }
  | Internal.Completions_result { id; _ }
  | Internal.Diagnostic { id; _ }
  | Internal.Status { id; _ } -> id

let exchange session request =
  let id =
    match request with
    | Internal.Evaluate request | Internal.Type_of request -> request.id
    | Internal.Lookup request -> request.id
    | Internal.Completions request -> request.id
    | Internal.Describe { id } | Internal.Close { id } -> id
  in
  match Internal.write_request session.output request with
  | Error message -> Error message
  | Ok () ->
      let rec loop responses =
        match Internal.read_response session.input with
        | Error message -> Error message
        | Ok None -> Error "LG REPL worker closed before final status"
        | Ok (Some response) ->
            if not (String.equal id (response_id response)) then
              Error "LG REPL worker returned an unexpected request ID"
            else
              let responses = response :: responses in
              match response with
              | Internal.Status { namespace; status; _ } ->
                  session.namespace <- namespace;
                  Ok (List.rev responses, status)
              | _ -> loop responses
      in
      loop []

let release_worker session =
  if not session.closed then (
    session.closed <- true;
    close_in_noerr session.input;
    close_out_noerr session.output)

let terminate_worker session =
  if not session.closed then (
    (try Unix.kill session.process Sys.sigterm with Unix.Unix_error _ -> ());
    release_worker session)

let close_worker session =
  if not session.closed then (
    let id = next_id "close" in
    ignore (exchange session (Internal.Close { id }));
    release_worker session)

let startup_error responses fallback =
  responses
  |> List.find_map (function
       | Internal.Diagnostic { message; _ } -> Some message
       | _ -> None)
  |> Option.value ~default:fallback

let spawn_worker ~worker_path ~state_path session_id =
  let parent_socket, child_socket =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  Unix.set_close_on_exec parent_socket;
  Unix.set_close_on_exec child_socket;
  let arguments =
    [|
      worker_path;
      "--socket-session";
      "--state";
      state_path;
    |]
  in
  match
    try
      Ok
        (Unix.create_process worker_path arguments child_socket child_socket
           Unix.stderr)
    with exn -> Error (Printexc.to_string exn)
  with
  | Error message ->
      Unix.close parent_socket;
      Unix.close child_socket;
      Error ("unable to start LG REPL worker: " ^ message)
  | Ok process ->
      Unix.close child_socket;
      let input = Unix.in_channel_of_descr (Unix.dup parent_socket) in
      let output = Unix.out_channel_of_descr parent_socket in
      set_binary_mode_in input true;
      set_binary_mode_out output true;
      let session =
        {
          id = session_id;
          process;
          input;
          output;
          namespace = "user";
          closed = false;
        }
      in
      let id = next_id "describe" in
      (match exchange session (Internal.Describe { id }) with
      | Ok (_, Internal.Done) -> Ok session
      | Ok (responses, (Internal.Failed | Internal.Unsupported)) ->
          let message = startup_error responses "LG REPL worker startup failed" in
          terminate_worker session;
          Error message
      | Error message ->
          terminate_worker session;
          Error message)

let send output response =
  let encoded = Session_protocol.response_to_bencode response in
  match Bencode.write output encoded with
  | Ok () -> ()
  | Error _ -> raise Connection_closed

let send_error output ~id ~session ~error_type message statuses =
  send output
    (Session_protocol.Error { id; session; error_type; message });
  send output (Session_protocol.Status { id; session; statuses })

let compiler_error_message (error : Lg.Compiler.compile_error) =
  let location =
    Option.fold ~none:"" ~some:(fun location ->
        Format.asprintf "%a: " Location.print_loc location)
      error.location
  in
  location ^ error.message

let send_internal_responses output ~id ~external_session session responses =
  List.iter
    (function
      | Internal.Stdout { text; _ } ->
          send output
            (Session_protocol.Output
               {
                 id;
                 session = external_session;
                 channel = Session_protocol.Stdout;
                 text;
               })
      | Internal.Stderr { text; _ } ->
          send output
            (Session_protocol.Output
               {
                 id;
                 session = external_session;
                 channel = Session_protocol.Stderr;
                 text;
               })
      | Internal.Value { rendered; type_name; _ } ->
          send output
            (Session_protocol.Value
               {
                 id;
                 session = external_session;
                 value = rendered;
                 namespace = session.namespace;
                 type_name = Some type_name;
               })
      | Internal.Definition { name; type_name; _ } ->
          send output
            (Session_protocol.Value
               {
                 id;
                 session = external_session;
                 value = name;
                 namespace = session.namespace;
                 type_name = Some type_name;
               })
      | Internal.Summary { text; _ } ->
          send output
            (Session_protocol.Value
               {
                 id;
                 session = external_session;
                 value = text;
                 namespace = session.namespace;
                 type_name = None;
               })
      | Internal.Namespace { namespace; _ } ->
          send output
            (Session_protocol.Value
               {
                 id;
                 session = external_session;
                 value = "namespace " ^ namespace;
                 namespace;
                 type_name = None;
               })
      | Internal.Diagnostic { code; message; location; _ } ->
          let message =
            Option.fold ~none:message ~some:(fun location ->
                location ^ ": " ^ message)
              location
          in
          send output
            (Session_protocol.Error
               {
                 id;
                 session = external_session;
                 error_type = code;
                 message;
               })
      | Internal.Description _ | Internal.Type_result _
      | Internal.Lookup_result _ | Internal.Completions_result _
      | Internal.Status _ -> ())
    responses

let namespace_matches output ~id ~external_session session expected =
  match expected with
  | None -> true
  | Some expected when String.equal expected session.namespace -> true
  | Some expected ->
      send_error output ~id ~session:external_session
        ~error_type:"namespace-mismatch"
        (Printf.sprintf "nREPL request expected namespace %s but session is %s"
           expected session.namespace)
        [ Session_protocol.Eval_error; Session_protocol.Done ];
      false

let newline_count text length =
  let count = ref 0 in
  for index = 0 to length - 1 do
    if text.[index] = '\n' then incr count
  done;
  !count

let evaluate output (request : Session_protocol.eval_request) external_session
    session =
  if
    namespace_matches output ~id:request.Session_protocol.id ~external_session
      session request.namespace
  then
      let initial_line = Option.value request.line ~default:1 in
      let rec forms line source =
        match Reader.read source with
        | Reader.Empty ->
            send output
              (Session_protocol.Status
                 {
                   id = request.id;
                   session = external_session;
                   statuses = [ Session_protocol.Done ];
                 })
        | Reader.Incomplete ->
            send_error output ~id:request.id ~session:external_session
              ~error_type:"LG5001" "incomplete LG form"
              [ Session_protocol.Eval_error; Session_protocol.Done ]
        | Reader.Invalid error ->
            send_error output ~id:request.id ~session:external_session
              ~error_type:error.code (compiler_error_message error)
              [ Session_protocol.Eval_error; Session_protocol.Done ]
        | Reader.Complete complete ->
            let internal_id = next_id "eval" in
            let compiled_source =
              String.make (max 0 (line - 1)) '\n' ^ complete.source
            in
            let internal_request =
              Internal.Evaluate
                {
                  id = internal_id;
                  source = compiled_source;
                  expected_namespace = Some session.namespace;
                  filename = request.file;
                }
            in
            (match exchange session internal_request with
            | Error message ->
                send_error output ~id:request.id ~session:external_session
                  ~error_type:"server-error" message
                  [ Session_protocol.Server_error; Session_protocol.Done ]
            | Ok (responses, Internal.Done) ->
                send_internal_responses output ~id:request.id ~external_session
                  session responses;
                let consumed =
                  String.length source - String.length complete.remaining
                in
                forms (line + newline_count source consumed) complete.remaining
            | Ok (responses, (Internal.Failed | Internal.Unsupported)) ->
                send_internal_responses output ~id:request.id ~external_session
                  session responses;
                send output
                  (Session_protocol.Status
                     {
                       id = request.id;
                       session = external_session;
                       statuses =
                         [ Session_protocol.Eval_error; Session_protocol.Done ];
                     }))
      in
      forms initial_line request.code

let lookup output (request : Session_protocol.lookup_request) external_session
    session =
  let symbol =
    match request.namespace with
    | Some namespace
      when not (String.equal namespace session.namespace)
           && not (String.contains request.symbol '/') ->
        namespace ^ "/" ^ request.symbol
    | Some _ | None -> request.symbol
  in
  let internal_id = next_id "lookup" in
  match
    exchange session
      (Internal.Lookup
         {
           id = internal_id;
           symbol;
           expected_namespace = Some session.namespace;
         })
  with
    | Error message ->
        send_error output ~id:request.id ~session:external_session
          ~error_type:"server-error" message
          [ Session_protocol.Server_error; Session_protocol.Done ]
    | Ok (responses, Internal.Done) ->
        responses
        |> List.find_map (function
             | Internal.Lookup_result { result; _ } -> result
             | _ -> None)
        |> Option.iter (fun (result : Internal.lookup) ->
               send output
                 (Session_protocol.Lookup_result
                    {
                      id = request.id;
                      session = external_session;
                      info =
                        {
                          name = result.name;
                          namespace = Some result.namespace;
                          type_name = result.type_name;
                          file = result.file;
                          line = result.line;
                          column = result.column;
                        };
                    }));
        send output
          (Session_protocol.Status
             {
               id = request.id;
               session = external_session;
               statuses = [ Session_protocol.Done ];
             })
    | Ok (responses, (Internal.Failed | Internal.Unsupported)) ->
        send_internal_responses output ~id:request.id ~external_session session
          responses;
        send output
          (Session_protocol.Status
             {
               id = request.id;
               session = external_session;
               statuses = [ Session_protocol.Eval_error; Session_protocol.Done ];
             })

let completions output (request : Session_protocol.completions_request)
    external_session session =
  let internal_prefix, returned_prefix =
    match request.namespace with
    | Some namespace when not (String.equal namespace session.namespace) ->
        let namespace_prefix = namespace ^ "/" in
        (namespace_prefix ^ request.prefix, Some namespace_prefix)
    | Some _ | None -> (request.prefix, None)
  in
  let internal_id = next_id "completions" in
  match
    exchange session
      (Internal.Completions
         {
           id = internal_id;
           prefix = internal_prefix;
           expected_namespace = Some session.namespace;
         })
  with
    | Error message ->
        send_error output ~id:request.id ~session:external_session
          ~error_type:"server-error" message
          [ Session_protocol.Server_error; Session_protocol.Done ]
    | Ok (responses, Internal.Done) ->
        let candidates =
          responses
          |> List.find_map (function
               | Internal.Completions_result { candidates; _ } -> Some candidates
               | _ -> None)
          |> Option.value ~default:[]
          |> List.map (fun (candidate : Internal.completion) ->
                 let returned_candidate =
                   match returned_prefix with
                   | Some prefix
                     when String.starts_with ~prefix candidate.candidate ->
                       String.sub candidate.candidate (String.length prefix)
                         (String.length candidate.candidate
                         - String.length prefix)
                   | Some _ | None -> candidate.candidate
                 in
                 {
                   Session_protocol.candidate = returned_candidate;
                   type_name = candidate.type_name;
                 })
        in
        send output
          (Session_protocol.Completions_result
             { id = request.id; session = external_session; candidates });
        send output
          (Session_protocol.Status
             {
               id = request.id;
               session = external_session;
               statuses = [ Session_protocol.Done ];
             })
    | Ok (responses, (Internal.Failed | Internal.Unsupported)) ->
        send_internal_responses output ~id:request.id ~external_session session
          responses;
        send output
          (Session_protocol.Status
             {
               id = request.id;
               session = external_session;
               statuses = [ Session_protocol.Eval_error; Session_protocol.Done ];
             })

let serve_connection ~worker_path ~state_path ~input ~output =
  set_binary_mode_in input true;
  set_binary_mode_out output true;
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let sessions = Hashtbl.create 8 in
  let cleanup () =
    Hashtbl.iter (fun _ session -> terminate_worker session) sessions;
    Hashtbl.clear sessions
  in
  let session_id () = next_id "session" in
  let create_session () =
    let id = session_id () in
    Result.map
      (fun session ->
        Hashtbl.add sessions id session;
        session)
      (spawn_worker ~worker_path ~state_path id)
  in
  let with_worker ~id requested_session action =
    match requested_session with
    | Some requested_session -> (
        match Hashtbl.find_opt sessions requested_session with
        | Some session -> action (Some requested_session) session
        | None ->
            send_error output ~id ~session:(Some requested_session)
              ~error_type:"session-not-found" "nREPL session does not exist"
              [ Session_protocol.Session_not_found; Session_protocol.Done ])
    | None -> (
        match spawn_worker ~worker_path ~state_path (session_id ()) with
        | Error message ->
            send_error output ~id ~session:None ~error_type:"server-error"
              message [ Session_protocol.Server_error; Session_protocol.Done ]
        | Ok session ->
            Fun.protect
              ~finally:(fun () -> close_worker session)
              (fun () -> action None session))
  in
  let rec loop () =
    match Bencode.read input with
    | Ok None -> ()
    | Error message ->
        send_error output ~id:None ~session:None ~error_type:"server-error"
          message [ Session_protocol.Server_error; Session_protocol.Done ]
    | Ok (Some value) -> (
        match Session_protocol.request_of_bencode value with
        | Error message ->
            send_error output ~id:None ~session:None ~error_type:"server-error"
              message [ Session_protocol.Server_error; Session_protocol.Done ];
            loop ()
        | Ok (Session_protocol.Describe { id }) ->
            send output
              (Session_protocol.Description
                 {
                   id;
                   ops =
                     [
                       "clone";
                       "close";
                       "completions";
                       "describe";
                       "eval";
                       "load-file";
                       "lookup";
                       "stdin";
                     ];
                   lg_version = "dev";
                 });
            loop ()
        | Ok (Session_protocol.Clone { id; source_session = Some source }) ->
            send_error output ~id ~session:(Some source) ~error_type:"clone-error"
              "LG cannot clone a live bytecode session"
              [ Session_protocol.Clone_error; Session_protocol.Done ];
            loop ()
        | Ok (Session_protocol.Clone { id; source_session = None }) ->
            (match create_session () with
            | Ok session ->
                send output
                  (Session_protocol.New_session { id; session = session.id })
            | Error message ->
                send_error output ~id ~session:None ~error_type:"clone-error"
                  message [ Session_protocol.Clone_error; Session_protocol.Done ]);
            loop ()
        | Ok (Session_protocol.Close { id; session }) ->
            (match Hashtbl.find_opt sessions session with
            | None ->
                send output
                  (Session_protocol.Status
                     {
                       id;
                       session = Some session;
                       statuses =
                         [
                           Session_protocol.Session_not_found;
                           Session_protocol.Done;
                         ];
                     })
            | Some worker ->
                Hashtbl.remove sessions session;
                close_worker worker;
                send output
                  (Session_protocol.Status
                     {
                       id;
                       session = Some session;
                       statuses = [ Session_protocol.Done ];
                     }));
            loop ()
        | Ok (Session_protocol.Stdin { id; session; _ }) ->
            send output
              (Session_protocol.Status
                 {
                   id;
                   session;
                   statuses =
                     [ Session_protocol.Stdin_unsupported; Session_protocol.Done ];
                 });
            loop ()
        | Ok (Session_protocol.Unknown { id; session; _ }) ->
            send output
              (Session_protocol.Status
                 {
                   id;
                   session;
                   statuses =
                     [ Session_protocol.Unknown_op; Session_protocol.Done ];
                 });
            loop ()
        | Ok (Session_protocol.Eval request) ->
            with_worker ~id:request.id request.session
              (fun external_session session ->
                evaluate output request external_session session);
            loop ()
        | Ok (Session_protocol.Load_file request) ->
            let file =
              match request.file_path with
              | Some _ as path -> path
              | None -> request.file_name
            in
            let eval_request : Session_protocol.eval_request =
              {
                id = request.id;
                session = request.session;
                code = request.contents;
                namespace = None;
                file;
                line = Some 1;
                column = None;
              }
            in
            with_worker ~id:request.id request.session
              (fun external_session session ->
                evaluate output eval_request external_session session);
            loop ()
        | Ok (Session_protocol.Lookup request) ->
            with_worker ~id:request.id request.session
              (fun external_session session ->
                lookup output request external_session session);
            loop ()
        | Ok (Session_protocol.Completions request) ->
            with_worker ~id:request.id request.session
              (fun external_session session ->
                completions output request external_session session);
            loop ())
  in
  Fun.protect ~finally:cleanup (fun () ->
      try loop () with Connection_closed -> ())
