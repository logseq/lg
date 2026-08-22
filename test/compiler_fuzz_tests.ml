let failf format = Printf.ksprintf failwith format

let alphabet =
  "()[]{}#_'`~@^:;\\\" abcdefghijklmnopqrstuvwxyz0123456789\n\t"

let generated_source state length =
  String.init length (fun _ -> alphabet.[Random.State.int state (String.length alphabet)])

let assert_compile_is_total ~case source =
  try
    match Lg.Compiler.compile_string source with
    | Ok _ -> ()
    | Error { code; message; _ } ->
        if code = "" then failf "case %d returned an empty diagnostic code" case;
        if message = "" then failf "case %d returned an empty diagnostic message" case
  with exn ->
    failf "case %d raised %s for input %S" case (Printexc.to_string exn) source

let test_generated_inputs_do_not_escape_the_compiler_result () =
  let state = Random.State.make [| 0x4c47; 0x2026; 0x0822 |] in
  let fixed_inputs =
    [ "\000"; "\255"; "("; "[}"; "{:a}"; "#?"; "#_("; "\"unterminated" ]
  in
  List.iteri (fun case source -> assert_compile_is_total ~case source) fixed_inputs;
  for case = List.length fixed_inputs to 255 do
    let length = Random.State.int state 96 in
    assert_compile_is_total ~case (generated_source state length)
  done

let run () = test_generated_inputs_do_not_escape_the_compiler_result ()
