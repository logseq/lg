let fail message = raise (Failure message)

let contains source expected =
  let source_length = String.length source in
  let expected_length = String.length expected in
  let rec search offset =
    offset + expected_length <= source_length
    && (String.sub source offset expected_length = expected
       || search (offset + 1))
  in
  expected_length = 0 || search 0

let source =
  {|
(def platform
  #?(:native "native-session"
     :melange "melange-session"))
(def string-length (String.length platform))
|}

let compile_and_check target expected unexpected =
  match Lg.Compiler.compile_string ~target source with
  | Error error -> fail ("concurrent compile failed: " ^ error.message)
  | Ok generated ->
      if not (contains generated expected) then
        fail ("concurrent compile omitted " ^ expected);
      if contains generated unexpected then
        fail ("concurrent compile leaked " ^ unexpected)

let run_worker start target expected unexpected =
  while not (Atomic.get start) do
    Domain.cpu_relax ()
  done;
  for _ = 1 to 20 do
    compile_and_check target expected unexpected
  done

let analyze_and_check () =
  match Lg.Language_service.analyze ~filename:"concurrent.cljc" source with
  | Error error -> fail ("concurrent analysis failed: " ^ error.message)
  | Ok analysis ->
      if Lg.Language_service.diagnostics analysis <> [] then
        fail "concurrent analysis returned unexpected diagnostics"

let run_analysis_worker start =
  while not (Atomic.get start) do
    Domain.cpu_relax ()
  done;
  for _ = 1 to 20 do
    analyze_and_check ()
  done

let run () =
  let start = Atomic.make false in
  let workers =
    [
      (Lg.Target.Native, "native-session", "melange-session");
      (Lg.Target.Melange, "melange-session", "native-session");
      (Lg.Target.Native, "native-session", "melange-session");
      (Lg.Target.Melange, "melange-session", "native-session");
    ]
    |> List.map (fun (target, expected, unexpected) ->
           Domain.spawn (fun () ->
               run_worker start target expected unexpected))
  in
  let analysis_worker = Domain.spawn (fun () -> run_analysis_worker start) in
  Atomic.set start true;
  List.iter Domain.join workers;
  Domain.join analysis_worker
