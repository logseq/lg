type t = { mutable compiler_state : Lg.Compiler.state }

type saved_compilation_state = {
  target : Lg.Target.t;
  state : Lg.Compiler.state;
  packages : string list;
  ocaml_source : string;
}

(* The bytecode toplevel can only execute units that are linked into its host
   process. These anchors keep the runtime units used by the prototype visible. *)
let _runtime_anchor = Lg_runtime.Runtime_reference.of_value ()
let _rrbvec_anchor = Rrbvec.empty
let _stdlib_anchor = Lg_stdlib_native.clojure_core_inc 0

let infrastructure_error message =
  Error
    ({
       Lg.Compiler.code = "LG9000";
       phase = `Infrastructure;
       title = "INFRASTRUCTURE ERROR";
       message;
       location = None;
       related = [];
       hints = [];
       fixes = [];
       type_mismatch = None;
     }
      : Lg.Compiler.compile_error)

let prepare_toplevel () =
  let output = Buffer.create 128 in
  let formatter = Format.formatter_of_buffer output in
  let succeeded = Toploop.prepare formatter () in
  Format.pp_print_flush formatter ();
  if succeeded then Ok ()
  else
    infrastructure_error
      (let message = Buffer.contents output in
       if String.equal message "" then "failed to prepare the OCaml toplevel"
       else message)

let read_saved_state path =
  let truncated () = Error "truncated compiler state artifact" in
  try
    let input = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr input)
      (fun () ->
        let line () = try Some (input_line input) with End_of_file -> None in
        match (line (), line (), line (), line (), line ()) with
        | ( Some "LG-COMPILER-STATE",
            Some "11",
            Some "saved-state",
            Some length_text,
            Some expected_digest ) -> (
            match int_of_string_opt length_text with
            | None -> Error "invalid compiler state artifact length"
            | Some length when length < 0 ->
                Error "invalid compiler state artifact length"
            | Some length ->
                let payload_start = pos_in input in
                let remaining = in_channel_length input - payload_start in
                if remaining <> length then
                  Error "invalid compiler state artifact length"
                else
                  let digest = Digest.channel input length |> Digest.to_hex in
                  if not (String.equal digest expected_digest) then
                    Error "compiler state artifact checksum mismatch"
                  else (
                    seek_in input payload_start;
                    try Ok (Marshal.from_channel input : saved_compilation_state)
                    with _ -> Error "invalid compiler state artifact payload"))
        | None, _, _, _, _ | _, None, _, _, _ | _, _, None, _, _
        | _, _, _, None, _ | _, _, _, _, None ->
            truncated ()
        | _ -> Error "unsupported compiler state artifact")
  with Sys_error message -> Error message

let open_precompiled_stdlib () =
  let lexbuf = Lexing.from_string "open Lg_stdlib_native;;" in
  Location.init lexbuf "<repl-bootstrap>";
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  let output = Buffer.create 128 in
  let formatter = Format.formatter_of_buffer output in
  let succeeded = Toploop.execute_phrase false formatter phrase in
  Format.pp_print_flush formatter ();
  if succeeded then Ok ()
  else
    infrastructure_error
      (let message = Buffer.contents output in
       if String.equal message "" then "failed to open the precompiled stdlib"
       else message)

let create_from_stdlib ~state_path =
  match read_saved_state state_path with
  | Error message -> infrastructure_error message
  | Ok saved when saved.target <> Lg.Target.Native ->
      infrastructure_error "REPL prototype requires a Native stdlib state"
  | Ok saved -> (
      match
        Lg.Compiler.restore_ocaml_environment ~target:Lg.Target.Native
          ~packages:saved.packages saved.state [ saved.ocaml_source ]
      with
      | Error _ as error -> error
      | Ok compiler_state -> (
          match prepare_toplevel () with
          | Error _ as error -> error
          | Ok () ->
              let stdlib_cmi_directory =
                Filename.concat (Filename.dirname state_path)
                  ".lg_compiled_stdlib_native.objs/byte"
              in
              Topdirs.dir_directory stdlib_cmi_directory;
              Result.map
                (fun () ->
                  {
                    compiler_state =
                      Lg.Compiler.with_source_scope "user" compiler_state;
                  })
                (open_precompiled_stdlib ())))

let execute structure =
  let output = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer output in
  let succeeded =
    Toploop.execute_phrase true formatter (Parsetree.Ptop_def structure)
  in
  Format.pp_print_flush formatter ();
  (succeeded, Buffer.contents output)

let eval session source =
  match Lg.Compiler.compile_chunk_parsetree session.compiler_state source with
  | Error _ as error -> error
  | Ok (next_state, structure) ->
      let succeeded, output = execute structure in
      if succeeded then (
        session.compiler_state <- next_state;
        Ok output)
      else
        infrastructure_error
          (if String.equal output "" then "OCaml toplevel evaluation failed"
           else output)
