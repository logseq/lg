module Protocol = Lg_repl.Protocol

let fail format = Printf.ksprintf failwith format

let expect_ok = function
  | Ok value -> value
  | Error message -> fail "unexpected protocol error: %s" message

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search index =
    if index + fragment_length > text_length then false
    else if String.sub text index fragment_length = fragment then true
    else search (index + 1)
  in
  fragment_length = 0 || search 0

let expect_error_containing fragment = function
  | Ok _ -> fail "expected protocol error containing %S" fragment
  | Error message ->
      if not (contains message fragment) then
        fail "expected protocol error containing %S, got %S" fragment message

let expect_equal label expected actual =
  if expected <> actual then fail "%s did not round-trip" label

let with_pipe action =
  let input_fd, output_fd = Unix.pipe () in
  let input = Unix.in_channel_of_descr input_fd in
  let output = Unix.out_channel_of_descr output_fd in
  Fun.protect
    ~finally:(fun () ->
      close_in_noerr input;
      close_out_noerr output)
    (fun () -> action input output)

let request =
  Protocol.Evaluate
    {
      id = "request-7";
      source = "(+ 40\n 2)";
      expected_namespace = Some "user";
      filename = Some "src/demo.cljc";
    }

let response =
  Protocol.Value
    { id = "request-7"; rendered = "42"; type_name = "int" }

let test_closed_json_round_trips () =
  Protocol.request_to_json request |> Protocol.request_of_json |> expect_ok
  |> expect_equal "request JSON" request;
  Protocol.response_to_json response |> Protocol.response_of_json |> expect_ok
  |> expect_equal "response JSON" response;
  Protocol.request_of_json
    (`Assoc
      [
        ("version", `Int Protocol.protocol_version);
        ("op", `String "unknown");
        ("id", `String "1");
      ])
  |> expect_error_containing "unknown";
  Protocol.request_of_json
    (`Assoc
      [
        ("version", `Int Protocol.protocol_version);
        ("op", `String "evaluate");
        ("id", `String "1");
      ])
  |> expect_error_containing "source";
  Protocol.request_of_json
    (`Assoc [ ("op", `String "describe"); ("id", `String "1") ])
  |> expect_error_containing "version"

let test_request_and_response_framing () =
  with_pipe (fun input output ->
      Protocol.write_request output request |> expect_ok;
      Protocol.read_request input |> expect_ok
      |> expect_equal "framed request" (Some request));
  with_pipe (fun input output ->
      Protocol.write_response output response |> expect_ok;
      Protocol.read_response input |> expect_ok
      |> expect_equal "framed response" (Some response))

let test_all_request_domains_are_closed () =
  let requests =
    [
      Protocol.Type_of
        {
          id = "type";
          source = "answer";
          expected_namespace = Some "demo";
          filename = None;
        };
      Protocol.Lookup
        { id = "lookup"; symbol = "answer"; expected_namespace = Some "demo" };
      Protocol.Completions
        { id = "complete"; prefix = "ans"; expected_namespace = Some "demo" };
      Protocol.Describe { id = "describe" };
      Protocol.Close { id = "close" };
    ]
  in
  List.iter
    (fun expected ->
      Protocol.request_to_json expected |> Protocol.request_of_json |> expect_ok
      |> expect_equal "closed request" expected)
    requests

let write_u32 output value =
  output_byte output ((value lsr 24) land 0xff);
  output_byte output ((value lsr 16) land 0xff);
  output_byte output ((value lsr 8) land 0xff);
  output_byte output (value land 0xff)

let test_frame_boundaries () =
  with_pipe (fun input output ->
      close_out output;
      Protocol.read_request input |> expect_ok
      |> expect_equal "clean frame EOF" None);
  with_pipe (fun input output ->
      write_u32 output (Protocol.max_frame_bytes + 1);
      flush output;
      Protocol.read_request input |> expect_error_containing "maximum");
  with_pipe (fun input output ->
      write_u32 output 4;
      output_string output "{";
      close_out output;
      Protocol.read_request input |> expect_error_containing "truncated")

let test_all_response_domains_are_closed () =
  let responses =
    [
      Protocol.Description
        {
          id = "describe";
          protocol_version = 1;
          target = Protocol.Native_bytecode;
          namespace = "user";
          interrupt_supported = false;
        };
      Protocol.Stdout { id = "1"; text = "out" };
      Protocol.Stderr { id = "1"; text = "err" };
      Protocol.Definition { id = "1"; name = "answer"; type_name = "int" };
      Protocol.Summary { id = "1"; text = "macro preserve" };
      Protocol.Namespace { id = "1"; namespace = "demo" };
      Protocol.Type_result { id = "1"; type_name = "vector<int>" };
      Protocol.Lookup_result
        {
          id = "1";
          result =
            Some
              {
                name = "answer";
                namespace = "demo";
                type_name = Some "int";
                file = Some "src/demo.cljc";
                line = Some 2;
                column = Some 1;
              };
        };
      Protocol.Completions_result
        {
          id = "1";
          candidates =
            [ { Protocol.candidate = "answer"; type_name = Some "int" } ];
        };
      Protocol.Diagnostic
        {
          id = "1";
          code = "LG5001";
          phase = Protocol.Semantic;
          message = "bad form";
          location = None;
        };
      Protocol.Status
        { id = "1"; namespace = "demo"; status = Protocol.Done };
      Protocol.Status
        { id = "2"; namespace = "demo"; status = Protocol.Failed };
      Protocol.Status
        { id = "3"; namespace = "demo"; status = Protocol.Unsupported };
    ]
  in
  List.iter
    (fun expected ->
      Protocol.response_to_json expected |> Protocol.response_of_json
      |> expect_ok |> expect_equal "closed response" expected)
    responses

let () =
  test_closed_json_round_trips ();
  test_request_and_response_framing ();
  test_all_request_domains_are_closed ();
  test_frame_boundaries ();
  test_all_response_domains_are_closed ()
