let _ensure_edn_backend_linked = Lg_edn_backend.of_edn_string

(* Dynamically loaded programs may use this primitive in the complete runtime. *)
let _ensure_clock_linked = Lg_runtime.Runtime_time.now

let build_root () =
  Sys.executable_name |> Filename.dirname |> Filename.dirname

let object_directories () =
  let root = build_root () in
  [
    Filename.concat root "runtime/.lg_runtime.objs/byte";
    Filename.concat root "vendor/rrbvec/.rrbvec.objs/byte";
    Filename.concat root "src/.lg.objs/byte";
  ]

let object_path unit_name =
  let filename = String.uncapitalize_ascii unit_name ^ ".cmo" in
  object_directories ()
  |> List.find_map (fun directory ->
         let path = Filename.concat directory filename in
         if Sys.file_exists path then Some path else None)

let rec load path =
  try Dynlink.loadfile path with
  | (Dynlink.Error (Dynlink.Unavailable_unit unit_name) as error) -> (
      match object_path unit_name with
      | Some dependency ->
          load dependency;
          load path
      | None -> raise error)

let () =
  if Array.length Sys.argv < 2 then
    invalid_arg "compiler_test_runner expects bytecode objects";
  ignore (Re.compile (Re.str ""));
  ignore (Re.Perl.compile_pat "");
  ignore (Weak.create 0);
  Dynlink.allow_unsafe_modules true;
  for index = 1 to Array.length Sys.argv - 1 do
    load Sys.argv.(index)
  done
