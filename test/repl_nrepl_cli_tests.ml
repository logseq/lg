module Bencode = Lg_repl.Nrepl_bencode

let fail format = Printf.ksprintf failwith format

let expect_ok = function
  | Ok value -> value
  | Error message -> fail "unexpected nREPL transport error: %s" message

let bytes value = Bencode.Byte_string value

let request fields = Bencode.Dictionary fields

let fields = function
  | Bencode.Dictionary fields -> fields
  | _ -> fail "nREPL response was not a dictionary"

let find name response = List.assoc_opt name (fields response)

let find_string name response =
  match find name response with
  | Some (Bencode.Byte_string value) -> Some value
  | None -> None
  | Some _ -> fail "nREPL response field %s was not a byte string" name

let find_strings name response =
  match find name response with
  | Some (Bencode.List values) ->
      List.map
        (function
          | Bencode.Byte_string value -> value
          | _ -> fail "nREPL response field %s contained a non-string" name)
        values
  | None -> []
  | Some _ -> fail "nREPL response field %s was not a list" name

let find_dictionary name response =
  match find name response with
  | Some (Bencode.Dictionary fields) -> Some fields
  | None -> None
  | Some _ -> fail "nREPL response field %s was not a dictionary" name

let find_list name response =
  match find name response with
  | Some (Bencode.List values) -> Some values
  | None -> None
  | Some _ -> fail "nREPL response field %s was not a list" name

let dictionary_string name fields =
  match List.assoc_opt name fields with
  | Some (Bencode.Byte_string value) -> Some value
  | None -> None
  | Some _ -> fail "nREPL dictionary field %s was not a string" name

let dictionary_int name fields =
  match List.assoc_opt name fields with
  | Some (Bencode.Integer value) -> Some (Int64.to_int value)
  | None -> None
  | Some _ -> fail "nREPL dictionary field %s was not an integer" name

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search index =
    if index + fragment_length > text_length then false
    else if String.sub text index fragment_length = fragment then true
    else search (index + 1)
  in
  fragment_length = 0 || search 0

let read_file path =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let write_file path source =
  let output = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output source)

let rec mkdir_p path =
  if Sys.file_exists path then ()
  else (
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o700)

let wait_for_port process port_path log_path =
  let rec loop attempts =
    if Sys.file_exists port_path && (Unix.stat port_path).st_size > 0 then
      read_file port_path |> String.trim |> int_of_string
    else
      match Unix.waitpid [ Unix.WNOHANG ] process with
      | 0, _ when attempts > 0 ->
          Unix.sleepf 0.05;
          loop (attempts - 1)
      | 0, _ -> fail "nREPL server did not announce a port"
      | _ -> fail "nREPL server exited during startup:\n%s" (read_file log_path)
  in
  loop 100

let send output fields = Bencode.write output (request fields) |> expect_ok

let read_until_done input expected_id =
  let started_at = Unix.gettimeofday () in
  let rec loop responses =
    match Bencode.read input |> expect_ok with
    | None -> fail "nREPL connection closed before status done"
    | Some response ->
        (match find_string "id" response with
        | Some id when String.equal id expected_id -> ()
        | Some id -> fail "expected nREPL response id %s, got %s" expected_id id
        | None -> fail "nREPL response did not include an id");
        let responses = response :: responses in
        if List.mem "done" (find_strings "status" response) then
          List.rev responses
        else loop responses
  in
  let responses = loop [] in
  if Sys.getenv_opt "LG_NREPL_TEST_TIMING" = Some "1" then
    Printf.eprintf "nREPL %s %.3fs\n%!" expected_id
      (Unix.gettimeofday () -. started_at);
  responses

let one_field name responses =
  match List.find_map (find_string name) responses with
  | Some value -> value
  | None -> fail "nREPL responses did not include %s" name

let statuses responses = List.concat_map (find_strings "status") responses

let errors responses =
  responses |> List.filter_map (find_string "err") |> String.concat ""

let outputs responses =
  responses |> List.filter_map (find_string "out") |> String.concat ""

let test_session input output =
  send output [ ("op", bytes "describe"); ("id", bytes "describe") ];
  let described = read_until_done input "describe" in
  let advertised_ops =
    described
    |> List.find_map (fun response ->
           match find "ops" response with
           | Some (Bencode.Dictionary ops) -> Some ops
           | Some _ | None -> None)
    |> Option.value ~default:[]
  in
  List.iter
    (fun op ->
      if not (List.mem_assoc op advertised_ops) then
        fail "nREPL describe response did not advertise %s" op)
    [ "eval"; "load-file"; "lookup"; "completions" ];

  send output [ ("op", bytes "clone"); ("id", bytes "clone-1") ];
  let session = read_until_done input "clone-1" |> one_field "new-session" in

  send output
    [
      ("op", bytes "eval");
      ("id", bytes "eval-1");
      ("session", bytes session);
      ( "code",
        bytes
          "(ns nrepl.demo)\n(def answer 42)\n(println \"nrepl-output\")\nanswer" );
    ];
  let evaluated = read_until_done input "eval-1" in
  let printed =
    outputs evaluated
  in
  if not (contains printed "nrepl-output") then
    fail "nREPL eval did not return stdout: out=%S err=%S status=%s"
      printed (errors evaluated) (String.concat "," (statuses evaluated));
  let values = List.filter_map (find_string "value") evaluated in
  if not (List.mem "42" values) then
    fail "nREPL multi-form eval did not return 42";
  let typed_value =
    match
      List.find_opt
        (fun response -> find_string "value" response = Some "42")
        evaluated
    with
    | Some response -> response
    | None -> fail "missing final nREPL value"
  in
  if find_string "lg/type" typed_value <> Some "int" then
    fail "nREPL value did not include its LG static type";
  if find_string "ns" typed_value <> Some "nrepl.demo" then
    fail "nREPL value did not report the committed namespace";

  send output
    [
      ("op", bytes "completions");
      ("id", bytes "completions");
      ("session", bytes session);
      ("prefix", bytes "ans");
      ("ns", bytes "nrepl.demo");
    ];
  let completed = read_until_done input "completions" in
  let candidates =
    completed |> List.find_map (find_list "completions")
    |> Option.value ~default:[]
  in
  let answer_completion =
    candidates
    |> List.find_map (function
         | Bencode.Dictionary fields
           when dictionary_string "candidate" fields = Some "answer" ->
             Some fields
         | Bencode.Dictionary _ -> None
         | _ -> fail "completion candidate was not a dictionary")
  in
  (match answer_completion with
  | Some fields when dictionary_string "type" fields = Some "int" -> ()
  | Some _ -> fail "answer completion did not include its static type"
  | None ->
      let rendered =
        candidates
        |> List.filter_map (function
             | Bencode.Dictionary fields -> dictionary_string "candidate" fields
             | _ -> None)
        |> String.concat ", "
      in
      fail "completions did not include answer; received: %s; status: %s"
        rendered (String.concat ", " (statuses completed)));

  send output
    [
      ("op", bytes "completions");
      ("id", bytes "completions-empty");
      ("session", bytes session);
      ("prefix", bytes "");
      ("ns", bytes "nrepl.demo");
    ];
  let empty_completed = read_until_done input "completions-empty" in
  let empty_candidates =
    empty_completed |> List.find_map (find_list "completions")
    |> Option.value ~default:[]
  in
  if List.length empty_candidates > 256 then
    fail "empty-prefix completions returned %d candidates"
      (List.length empty_candidates);

  send output
    [
      ("op", bytes "lookup");
      ("id", bytes "lookup-answer");
      ("session", bytes session);
      ("sym", bytes "answer");
      ("ns", bytes "nrepl.demo");
    ];
  let answer_info =
    read_until_done input "lookup-answer"
    |> List.find_map (find_dictionary "info")
  in
  (match answer_info with
  | Some fields
    when dictionary_string "name" fields = Some "answer"
         && dictionary_string "ns" fields = Some "nrepl.demo"
         && dictionary_string "type" fields = Some "int" ->
      ()
  | Some fields ->
      let encoded =
        Bencode.to_string (Bencode.Dictionary fields) |> expect_ok
      in
      fail "lookup returned incomplete answer metadata: %s" encoded
  | None -> fail "lookup did not return answer metadata");

  send output
    [
      ("op", bytes "load-file");
      ("id", bytes "load-file");
      ("session", bytes session);
      ("file-path", bytes "/tmp/lg-nrepl-loaded.cljc");
      ("file-name", bytes "lg-nrepl-loaded.cljc");
      ("file", bytes "(ns loaded.demo)\n(def loaded-value 9)\n");
    ];
  let loaded = read_until_done input "load-file" in
  if List.mem "eval-error" (statuses loaded) then
    fail "nREPL load-file returned eval-error";
  send output
    [
      ("op", bytes "lookup");
      ("id", bytes "lookup-loaded");
      ("session", bytes session);
      ("sym", bytes "loaded-value");
      ("ns", bytes "loaded.demo");
    ];
  let loaded_info =
    read_until_done input "lookup-loaded"
    |> List.find_map (find_dictionary "info")
  in
  (match loaded_info with
  | Some fields
    when dictionary_string "name" fields = Some "loaded-value"
         && dictionary_string "type" fields = Some "int"
         && dictionary_string "file" fields
            = Some "/tmp/lg-nrepl-loaded.cljc"
         && dictionary_int "line" fields = Some 2 ->
      ()
  | Some _ -> fail "lookup did not preserve load-file source metadata"
  | None -> fail "lookup did not find the loaded definition");

  let project_root = Filename.temp_file "lg-nrepl-project-" "" in
  Sys.remove project_root;
  Unix.mkdir project_root 0o700;
  write_file (Filename.concat project_root "dune-project")
    "(lang dune 3.20)\n(name lg_nrepl_project)\n";
  let source_directory =
    Filename.concat project_root "lg/logseq_chat/core"
  in
  mkdir_p source_directory;
  let protocol_path = Filename.concat source_directory "sync_protocol.cljc" in
  write_file protocol_path
    "(ns logseq-chat.sync-protocol)\n(defn marker [] 41)\n";
  let entity_path = Filename.concat source_directory "entity_sync.cljc" in
  let entity_source =
    "(ns logseq-chat.entity-sync\n\
    \  (:require [logseq-chat.sync-protocol :as protocol]))\n\
     (defn synced [] (+ (protocol/marker) 1))\n"
  in
  write_file entity_path entity_source;
  send output
    [
      ("op", bytes "load-file");
      ("id", bytes "load-file-workspace");
      ("session", bytes session);
      ("file-path", bytes entity_path);
      ("file-name", bytes "entity_sync.cljc");
      ("file", bytes entity_source);
    ];
  let workspace_loaded = read_until_done input "load-file-workspace" in
  if List.mem "eval-error" (statuses workspace_loaded) then
    fail "nREPL load-file did not load workspace namespace dependencies: %s"
      (errors workspace_loaded);
  send output
    [
      ("op", bytes "eval");
      ("id", bytes "workspace-eval");
      ("session", bytes session);
      ("code", bytes "(synced)");
  ];
  let workspace_evaluated = read_until_done input "workspace-eval" in
  if List.mem "eval-error" (statuses workspace_evaluated) then
    fail "nREPL workspace dependency eval failed: out=%S err=%S status=%s"
      (outputs workspace_evaluated) (errors workspace_evaluated)
      (String.concat "," (statuses workspace_evaluated));
  let workspace_values = List.filter_map (find_string "value") workspace_evaluated in
  if not (List.mem "42" workspace_values) then
    fail "nREPL workspace dependency was not available after load-file";

  send output
    [
      ("op", bytes "load-file");
      ("id", bytes "load-file-forward");
      ("session", bytes session);
      ("file-path", bytes "/tmp/lg-nrepl-forward.cljc");
      ("file-name", bytes "lg-nrepl-forward.cljc");
      ( "file",
        bytes
          "(ns loaded.forward)\n\
           (defn first-value [] (second-value))\n\
           (defn second-value [] 42)\n" );
    ];
  let forward_loaded = read_until_done input "load-file-forward" in
  if List.mem "eval-error" (statuses forward_loaded) then
    fail "nREPL load-file split a forward reference across evals: %s"
      (errors forward_loaded);
  send output
    [
      ("op", bytes "eval");
      ("id", bytes "forward-eval");
      ("session", bytes session);
      ("code", bytes "(ns loaded.forward)\n(first-value)");
    ];
  let forward_evaluated = read_until_done input "forward-eval" in
  if List.mem "eval-error" (statuses forward_evaluated) then
    fail "nREPL forward-reference eval failed: out=%S err=%S status=%s"
      (outputs forward_evaluated) (errors forward_evaluated)
      (String.concat "," (statuses forward_evaluated));
  let forward_values = List.filter_map (find_string "value") forward_evaluated in
  if not (List.mem "42" forward_values) then
    fail "nREPL load-file did not commit the forward-reference file";

  send output
    [
      ("op", bytes "lookup");
      ("id", bytes "lookup-other-ns");
      ("session", bytes session);
      ("sym", bytes "answer");
      ("ns", bytes "nrepl.demo");
    ];
  let other_namespace_info =
    read_until_done input "lookup-other-ns"
    |> List.find_map (find_dictionary "info")
  in
  (match other_namespace_info with
  | Some fields
    when dictionary_string "name" fields = Some "answer"
         && dictionary_string "ns" fields = Some "nrepl.demo"
         && dictionary_string "type" fields = Some "int" ->
      ()
  | Some _ -> fail "lookup did not honor the requested namespace"
  | None -> fail "lookup did not find a symbol in another namespace");

  send output
    [
      ("op", bytes "completions");
      ("id", bytes "completions-other-ns");
      ("session", bytes session);
      ("prefix", bytes "ans");
      ("ns", bytes "nrepl.demo");
    ];
  let other_namespace_candidates =
    read_until_done input "completions-other-ns"
    |> List.find_map (find_list "completions")
    |> Option.value ~default:[]
  in
  if
    not
      (List.exists
         (function
           | Bencode.Dictionary fields ->
               dictionary_string "candidate" fields = Some "answer"
               && dictionary_string "type" fields = Some "int"
           | _ -> false)
         other_namespace_candidates)
  then fail "completions did not honor the requested namespace";

  send output
    [
      ("op", bytes "eval");
      ("id", bytes "eval-error");
      ("session", bytes session);
      ("code", bytes "(throw (ex-info \"nrepl-boom\" {}))");
    ];
  let failed = read_until_done input "eval-error" in
  if not (List.mem "eval-error" (statuses failed)) then
    fail "nREPL runtime error did not report eval-error";

  send output
    [
      ("op", bytes "eval");
      ("id", bytes "eval-after-error");
      ("session", bytes session);
      ("code", bytes "(ns loaded.demo)\nloaded-value");
  ];
  let recovered = read_until_done input "eval-after-error" in
  let recovered_values = List.filter_map (find_string "value") recovered in
  if not (List.mem "9" recovered_values) then
    fail "nREPL session did not recover after an evaluation error";

  send output [ ("op", bytes "clone"); ("id", bytes "clone-2") ];
  let isolated_session =
    read_until_done input "clone-2" |> one_field "new-session"
  in
  send output
    [
      ("op", bytes "eval");
      ("id", bytes "isolated");
      ("session", bytes isolated_session);
      ("code", bytes "answer");
    ];
  let isolated = read_until_done input "isolated" in
  if not (List.mem "eval-error" (statuses isolated)) then
    fail "separate nREPL sessions shared compiler state";

  send output
    [
      ("op", bytes "future-op");
      ("id", bytes "unknown");
      ("session", bytes session);
    ];
  let unknown = read_until_done input "unknown" in
  if not (List.mem "unknown-op" (statuses unknown)) then
    fail "unknown nREPL operation did not report unknown-op";

  send output
    [
      ("op", bytes "stdin");
      ("id", bytes "stdin");
      ("session", bytes session);
      ("stdin", bytes "ignored\n");
    ];
  let stdin_response = read_until_done input "stdin" in
  if not (List.mem "stdin-unsupported" (statuses stdin_response)) then
    fail "nREPL stdin did not report its defined unsupported status";

  send output
    [
      ("op", bytes "close");
      ("id", bytes "close");
      ("session", bytes session);
    ];
  ignore (read_until_done input "close")

let () =
  if Array.length Sys.argv <> 3 then
    fail "expected the REPL worker and stdlib state paths";
  let worker = Sys.argv.(1) in
  let state_path = Sys.argv.(2) in
  let port_path = Filename.temp_file "lg-nrepl-port-" ".txt" in
  let log_path = Filename.temp_file "lg-nrepl-server-" ".log" in
  Sys.remove port_path;
  let null_input = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let log_output =
    Unix.openfile log_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
  in
  let process =
    Unix.create_process worker
      [|
        worker;
        "--nrepl-listen";
        "127.0.0.1:0";
        "--state";
        state_path;
        "--port-file";
        port_path;
      |]
      null_input log_output log_output
  in
  Unix.close null_input;
  Unix.close log_output;
  Fun.protect
    ~finally:(fun () ->
      (try Unix.kill process Sys.sigterm with Unix.Unix_error _ -> ());
      (try ignore (Unix.waitpid [] process) with Unix.Unix_error _ -> ());
      (try Sys.remove port_path with Sys_error _ -> ());
      (try Sys.remove log_path with Sys_error _ -> ()))
    (fun () ->
      let port = wait_for_port process port_path log_path in
      let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.connect socket
        (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
      let input = Unix.in_channel_of_descr (Unix.dup socket) in
      let output = Unix.out_channel_of_descr socket in
      set_binary_mode_in input true;
      set_binary_mode_out output true;
      Fun.protect
        ~finally:(fun () ->
          close_in_noerr input;
          close_out_noerr output)
        (fun () -> test_session input output))
