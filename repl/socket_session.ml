module Protocol = Protocol

let phase_of_compiler_phase = function
  | `Lexing -> Protocol.Lexing
  | `Parsing -> Protocol.Parsing
  | `Semantic -> Protocol.Semantic
  | `Lowering -> Protocol.Lowering
  | `Ocaml -> Protocol.Ocaml
  | `Infrastructure -> Protocol.Infrastructure

let protocol_diagnostic ~id ~code message =
  Protocol.Diagnostic
    {
      id;
      code;
      phase = Protocol.Protocol;
      message;
      location = None;
    }

let compiler_diagnostic ~id (error : Lg.Compiler.compile_error) =
  let location =
    Option.map (fun location -> Format.asprintf "%a" Location.print_loc location)
      error.location
  in
  Protocol.Diagnostic
    {
      id;
      code = error.code;
      phase = phase_of_compiler_phase error.phase;
      message = error.message;
      location;
    }

let read_file path =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let remove_if_present path =
  try Sys.remove path with Sys_error _ -> ()

let capture_output action =
  let stdout_path, captured_stdout =
    Filename.open_temp_file "lg-repl-stdout-" ".log"
  in
  let stderr_path, captured_stderr =
    Filename.open_temp_file "lg-repl-stderr-" ".log"
  in
  let saved_stdout = Unix.dup Unix.stdout in
  let saved_stderr = Unix.dup Unix.stderr in
  let restored = ref false in
  let restore () =
    if not !restored then (
      restored := true;
      flush stdout;
      flush stderr;
      Unix.dup2 saved_stdout Unix.stdout;
      Unix.dup2 saved_stderr Unix.stderr;
      Unix.close saved_stdout;
      Unix.close saved_stderr;
      close_out_noerr captured_stdout;
      close_out_noerr captured_stderr)
  in
  Fun.protect
    ~finally:(fun () ->
      restore ();
      remove_if_present stdout_path;
      remove_if_present stderr_path)
    (fun () ->
      flush stdout;
      flush stderr;
      Unix.dup2 (Unix.descr_of_out_channel captured_stdout) Unix.stdout;
      Unix.dup2 (Unix.descr_of_out_channel captured_stderr) Unix.stderr;
      let outcome =
        try Ok (Fun.protect ~finally:restore action)
        with exn -> Error exn
      in
      let stdout_text = read_file stdout_path in
      let stderr_text = read_file stderr_path in
      (outcome, stdout_text, stderr_text))

let send output response =
  match Protocol.write_response output response with
  | Ok () -> true
  | Error _ -> false

let send_status output session ~id status =
  send output
    (Protocol.Status
       { id; namespace = Session.namespace session; status })

let send_output output ~id stdout_text stderr_text =
  let stdout_sent =
    String.equal stdout_text ""
    || send output (Protocol.Stdout { id; text = stdout_text })
  in
  let stderr_sent =
    String.equal stderr_text ""
    || send output (Protocol.Stderr { id; text = stderr_text })
  in
  stdout_sent && stderr_sent

let expected_namespace_matches output session ~id expected_namespace =
  match expected_namespace with
  | None -> true
  | Some expected ->
      let actual = Session.namespace session in
      if String.equal expected actual then true
      else
        let message =
          Printf.sprintf
            "REPL namespace changed: request expected %s but session is %s"
            expected actual
        in
        ignore (send output (protocol_diagnostic ~id ~code:"LG5002" message));
        ignore (send_status output session ~id Protocol.Failed);
        false

let response_of_evaluation ~id (evaluation : Session.evaluation) =
  match evaluation.outcome with
  | Session.Value value ->
      Protocol.Value
        { id; rendered = value.rendered; type_name = value.type_name }
  | Session.Definition definition ->
      Protocol.Definition
        { id; name = definition.name; type_name = definition.type_name }
  | Session.Namespace namespace -> Protocol.Namespace { id; namespace }
  | Session.Summary text -> Protocol.Summary { id; text }

let evaluate output session (request : Protocol.source_request) =
  if
    expected_namespace_matches output session ~id:request.Protocol.id
      request.expected_namespace
  then
    let result, stdout_text, stderr_text =
      capture_output (fun () ->
          Session.eval ?filename:request.Protocol.filename session request.source)
    in
    if send_output output ~id:request.id stdout_text stderr_text then
      match result with
      | Error exn ->
          ignore
            (send output
               (protocol_diagnostic ~id:request.id ~code:"LG9000"
                  ("REPL worker failed: " ^ Printexc.to_string exn)));
          ignore (send_status output session ~id:request.id Protocol.Failed)
      | Ok (Error error) ->
          ignore (send output (compiler_diagnostic ~id:request.id error));
          ignore (send_status output session ~id:request.id Protocol.Failed)
      | Ok (Ok evaluation) ->
          ignore (send output (response_of_evaluation ~id:request.id evaluation));
          ignore (send_status output session ~id:request.id Protocol.Done)

let type_of output session (request : Protocol.source_request) =
  if
    expected_namespace_matches output session ~id:request.Protocol.id
      request.expected_namespace
  then
    match Session.type_of session request.Protocol.source with
    | Error error ->
        ignore (send output (compiler_diagnostic ~id:request.id error));
        ignore (send_status output session ~id:request.id Protocol.Failed)
    | Ok type_name ->
        ignore
          (send output (Protocol.Type_result { id = request.id; type_name }));
        ignore (send_status output session ~id:request.id Protocol.Done)

let lookup output session (request : Protocol.lookup_request) =
  if
    expected_namespace_matches output session ~id:request.Protocol.id
      request.expected_namespace
  then
    match Session.lookup session request.symbol with
    | Error error ->
        ignore (send output (compiler_diagnostic ~id:request.id error));
        ignore (send_status output session ~id:request.id Protocol.Failed)
    | Ok result ->
        let result =
          Option.map
            (fun (lookup : Session.lookup) ->
              {
                Protocol.name = lookup.name;
                namespace = lookup.namespace;
                type_name = lookup.type_name;
                file = lookup.file;
                line = lookup.line;
                column = lookup.column;
              })
            result
        in
        ignore (send output (Protocol.Lookup_result { id = request.id; result }));
        ignore (send_status output session ~id:request.id Protocol.Done)

let completions output session (request : Protocol.completions_request) =
  if
    expected_namespace_matches output session ~id:request.Protocol.id
      request.expected_namespace
  then
    match Session.completions session request.prefix with
    | Error error ->
        ignore (send output (compiler_diagnostic ~id:request.id error));
        ignore (send_status output session ~id:request.id Protocol.Failed)
    | Ok candidates ->
        let candidates =
          List.map
            (fun (completion : Session.completion) ->
              {
                Protocol.candidate = completion.candidate;
                type_name = completion.type_name;
              })
            candidates
        in
        ignore
          (send output
             (Protocol.Completions_result { id = request.id; candidates }));
        ignore (send_status output session ~id:request.id Protocol.Done)

let describe output session id =
  ignore
    (send output
       (Protocol.Description
          {
            id;
            protocol_version = Protocol.protocol_version;
            target = Protocol.Native_bytecode;
            namespace = Session.namespace session;
            interrupt_supported = false;
          }));
  ignore (send_status output session ~id Protocol.Done)

let serve session ~input ~output =
  set_binary_mode_in input true;
  set_binary_mode_out output true;
  let rec loop () =
    match Protocol.read_request input with
    | Ok None -> ()
    | Error message ->
        ignore
          (send output
             (protocol_diagnostic ~id:"protocol" ~code:"LG5003" message));
        ignore (send_status output session ~id:"protocol" Protocol.Failed)
    | Ok (Some (Protocol.Describe { id })) ->
        describe output session id;
        loop ()
    | Ok (Some (Protocol.Evaluate request)) ->
        evaluate output session request;
        loop ()
    | Ok (Some (Protocol.Type_of request)) ->
        type_of output session request;
        loop ()
    | Ok (Some (Protocol.Lookup request)) ->
        lookup output session request;
        loop ()
    | Ok (Some (Protocol.Completions request)) ->
        completions output session request;
        loop ()
    | Ok (Some (Protocol.Close { id })) ->
        ignore (send_status output session ~id Protocol.Done)
  in
  loop ()
