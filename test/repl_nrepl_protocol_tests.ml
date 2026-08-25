module Bencode = Lg_repl.Nrepl_bencode
module Protocol = Lg_repl.Nrepl_protocol

let fail format = Printf.ksprintf failwith format

let expect_ok = function
  | Ok value -> value
  | Error message -> fail "unexpected nREPL protocol error: %s" message

let expect_equal label expected actual =
  if expected <> actual then fail "%s did not match" label

let expect_error label = function
  | Ok _ -> fail "%s should fail" label
  | Error _ -> ()

let bytes value = Bencode.Byte_string value

let dictionary entries = Bencode.Dictionary entries

let with_pipe action =
  let input_fd, output_fd = Unix.pipe () in
  let input = Unix.in_channel_of_descr input_fd in
  let output = Unix.out_channel_of_descr output_fd in
  Fun.protect
    ~finally:(fun () ->
      close_in_noerr input;
      close_out_noerr output)
    (fun () -> action input output)

let test_bencode_round_trip () =
  let value =
    dictionary
      [
        ("b", Bencode.List [ bytes "one"; bytes "two" ]);
        ("a", Bencode.Integer 1L);
      ]
  in
  Bencode.to_string value |> expect_ok
  |> expect_equal "canonical bencode" "d1:ai1e1:bl3:one3:twoee";
  Bencode.of_string "d1:ai1e1:bl3:one3:twoee" |> expect_ok
  |> expect_equal "bencode value"
       (dictionary
          [
            ("a", Bencode.Integer 1L);
            ("b", Bencode.List [ bytes "one"; bytes "two" ]);
          ]);
  Bencode.of_string "i-0e" |> expect_error "negative zero";
  Bencode.of_string "3:ab" |> expect_error "truncated byte string";
  Bencode.of_string "i1ejunk" |> expect_error "trailing payload"

let test_incremental_bencode_stream () =
  with_pipe (fun input output ->
      Bencode.write output (bytes "first") |> expect_ok;
      Bencode.write output (Bencode.Integer 2L) |> expect_ok;
      Bencode.read input |> expect_ok
      |> expect_equal "first stream value" (Some (bytes "first"));
      Bencode.read input |> expect_ok
      |> expect_equal "second stream value" (Some (Bencode.Integer 2L));
      close_out output;
      Bencode.read input |> expect_ok |> expect_equal "clean stream EOF" None)

let test_bencode_limits () =
  let oversized_header = string_of_int (Bencode.max_byte_string_bytes + 1) ^ ":" in
  Bencode.of_string oversized_header |> expect_error "oversized byte string";
  let rec nested depth value =
    if depth = 0 then value else nested (depth - 1) (Bencode.List [ value ])
  in
  nested (Bencode.max_nesting_depth + 1) (bytes "x") |> Bencode.to_string
  |> expect_error "nested output"

let test_closed_request_decoding () =
  Protocol.request_of_bencode
    (dictionary [ ("op", bytes "describe"); ("id", bytes "d1") ])
  |> expect_ok
  |> expect_equal "describe request" (Protocol.Describe { id = Some "d1" });
  Protocol.request_of_bencode
    (dictionary [ ("op", bytes "clone"); ("id", bytes "c1") ])
  |> expect_ok
  |> expect_equal "clone request"
       (Protocol.Clone { id = Some "c1"; source_session = None });
  Protocol.request_of_bencode
    (dictionary
       [
         ("op", bytes "load-file");
         ("id", bytes "l1");
         ("session", bytes "s1");
         ("file", bytes "(def loaded 9)");
         ("file-path", bytes "/tmp/loaded.cljc");
         ("file-name", bytes "loaded.cljc");
       ])
  |> expect_ok
  |> expect_equal "load-file request"
       (Protocol.Load_file
          {
            id = Some "l1";
            session = Some "s1";
            contents = "(def loaded 9)";
            file_path = Some "/tmp/loaded.cljc";
            file_name = Some "loaded.cljc";
          });
  Protocol.request_of_bencode
    (dictionary
       [
         ("op", bytes "lookup");
         ("id", bytes "k1");
         ("session", bytes "s1");
         ("sym", bytes "loaded");
         ("ns", bytes "loaded.demo");
       ])
  |> expect_ok
  |> expect_equal "lookup request"
       (Protocol.Lookup
          {
            id = Some "k1";
            session = Some "s1";
            symbol = "loaded";
            namespace = Some "loaded.demo";
          });
  Protocol.request_of_bencode
    (dictionary
       [
         ("op", bytes "completions");
         ("id", bytes "m1");
         ("session", bytes "s1");
         ("prefix", bytes "loa");
         ("ns", bytes "loaded.demo");
       ])
  |> expect_ok
  |> expect_equal "completions request"
       (Protocol.Completions
          {
            id = Some "m1";
            session = Some "s1";
            prefix = "loa";
            namespace = Some "loaded.demo";
          });
  Protocol.request_of_bencode
    (dictionary
       [
         ("op", bytes "eval");
         ("id", bytes "e1");
         ("session", bytes "s1");
         ("code", bytes "(+ 1 2)");
         ("ns", bytes "user");
         ("file", bytes "src/demo.cljc");
         ("line", Bencode.Integer 3L);
         ("column", Bencode.Integer 4L);
       ])
  |> expect_ok
  |> expect_equal "eval request"
       (Protocol.Eval
          {
            id = Some "e1";
            session = Some "s1";
            code = "(+ 1 2)";
            namespace = Some "user";
            file = Some "src/demo.cljc";
            line = Some 3;
            column = Some 4;
          });
  Protocol.request_of_bencode
    (dictionary
       [
         ("op", bytes "stdin");
         ("id", bytes "i1");
         ("session", bytes "s1");
         ("stdin", bytes "answer\n");
       ])
  |> expect_ok
  |> expect_equal "stdin request"
       (Protocol.Stdin
          { id = Some "i1"; session = Some "s1"; input = "answer\n" });
  Protocol.request_of_bencode
    (dictionary
       [
         ("op", bytes "future-op");
         ("id", bytes "u1");
         ("session", bytes "s1");
       ])
  |> expect_ok
  |> expect_equal "unknown request"
       (Protocol.Unknown
          { id = Some "u1"; session = Some "s1"; op = "future-op" });
  Protocol.request_of_bencode (dictionary [ ("op", bytes "eval") ])
  |> expect_error "eval without code";
  Protocol.request_of_bencode (Bencode.List [])
  |> expect_error "non-dictionary request"

let test_closed_response_encoding () =
  Protocol.response_to_bencode
    (Protocol.Description
       {
         id = Some "d1";
         ops =
           [
             "describe";
             "clone";
             "eval";
             "load-file";
             "lookup";
             "completions";
             "close";
             "stdin";
           ];
         lg_version = "dev";
       })
  |> expect_equal "description response"
       (dictionary
          [
            ("id", bytes "d1");
            ( "ops",
              dictionary
                [
                  ("describe", dictionary []);
                  ("clone", dictionary []);
                  ("eval", dictionary []);
                  ("load-file", dictionary []);
                  ("lookup", dictionary []);
                  ("completions", dictionary []);
                  ("close", dictionary []);
                  ("stdin", dictionary []);
                ] );
            ("versions", dictionary [ ("lg", bytes "dev") ]);
            ("status", Bencode.List [ bytes "done" ]);
          ]);
  Protocol.response_to_bencode
    (Protocol.Value
       {
         id = Some "e1";
         session = Some "s1";
         value = "3";
         namespace = "user";
         type_name = Some "int";
       })
  |> expect_equal "typed value response"
       (dictionary
          [
            ("id", bytes "e1");
            ("session", bytes "s1");
            ("value", bytes "3");
            ("ns", bytes "user");
            ("lg/type", bytes "int");
          ]);
  Protocol.response_to_bencode
    (Protocol.Lookup_result
       {
         id = Some "k1";
         session = Some "s1";
         info =
           {
             name = "loaded";
             namespace = Some "loaded.demo";
             type_name = Some "int";
             file = Some "/tmp/loaded.cljc";
             line = Some 2;
             column = Some 1;
           };
       })
  |> expect_equal "lookup response"
       (dictionary
          [
            ("id", bytes "k1");
            ("session", bytes "s1");
            ( "info",
              dictionary
                [
                  ("name", bytes "loaded");
                  ("ns", bytes "loaded.demo");
                  ("type", bytes "int");
                  ("file", bytes "/tmp/loaded.cljc");
                  ("line", Bencode.Integer 2L);
                  ("column", Bencode.Integer 1L);
                ] );
          ]);
  Protocol.response_to_bencode
    (Protocol.Completions_result
       {
         id = Some "m1";
         session = Some "s1";
         candidates =
           [
             { Protocol.candidate = "loaded"; type_name = Some "int" };
           ];
       })
  |> expect_equal "completions response"
       (dictionary
          [
            ("id", bytes "m1");
            ("session", bytes "s1");
            ( "completions",
              Bencode.List
                [
                  dictionary
                    [ ("candidate", bytes "loaded"); ("type", bytes "int") ];
                ] );
          ]);
  Protocol.response_to_bencode
    (Protocol.Status
       {
         id = Some "u1";
         session = Some "s1";
         statuses = [ Protocol.Unknown_op; Protocol.Done ];
       })
  |> expect_equal "unknown op response"
       (dictionary
          [
            ("id", bytes "u1");
            ("session", bytes "s1");
            ("status", Bencode.List [ bytes "unknown-op"; bytes "done" ]);
          ])

let () =
  test_bencode_round_trip ();
  test_incremental_bencode_stream ();
  test_bencode_limits ();
  test_closed_request_decoding ();
  test_closed_response_encoding ()
