type parser_result = {
  ast : Ast.form list;
  locations : Location.t list;
  form_locations : Source_context.entry list;
  parsed_as : [ `Cljml ];
}

type typed_result = {
  ast : Ast.form list;
  items : Lowered.compiled_item list;
  locations : Location.t list;
  typecheck_state : Typecheck.state;
}

type parsetree_result = {
  ast : Ast.form list;
  items : Lowered.compiled_item list;
  structure : Parsetree.structure;
}

type diagnostic_severity = [ `Warning ]

type diagnostic = {
  message : string;
  severity : diagnostic_severity;
}

type compilation = {
  ocaml_source : string;
  diagnostics : diagnostic list;
}

type language_analysis = {
  typed_structure : Typedtree.structure;
  compiler_env : Env.t;
  typecheck_state : Typecheck.state;
  diagnostics : diagnostic list;
}

type state = {
  typecheck_state : Typecheck.state;
  located_items : (Location.t * Lowered.compiled_item) list;
}

module type FRONTEND = sig
  val implementation : ?filename:string -> string -> (parser_result, Error.t) result
end

module Cljml_frontend : FRONTEND = struct
  let position filename source offset =
    let rec loop index line line_start =
      if index >= offset then
        { Lexing.pos_fname = filename;
          pos_lnum = line;
          pos_bol = line_start;
          pos_cnum = offset;
        }
      else if source.[index] = '\n' then loop (index + 1) (line + 1) (index + 1)
      else loop (index + 1) line line_start
    in
    loop 0 1 0

  let location filename source (span : Ast.source_span) =
    { Location.loc_start = position filename source span.start_offset;
      loc_end = position filename source span.end_offset;
      loc_ghost = false;
    }

  let implementation ?(filename = "<string>") source =
    match Lexer.tokenize source with
    | Error _ as err -> err
    | Ok tokens -> (
        match Parser.parse_located tokens with
        | Error _ as err -> err
        | Ok located_ast ->
            let rec form_locations acc located =
              let location = location filename source located.Ast.span in
              List.fold_left form_locations
                ((located.Ast.form, location) :: acc)
                located.Ast.children
            in
            Ok
              { ast = List.map (fun located -> located.Ast.form) located_ast;
                locations =
                  List.map
                    (fun located -> location filename source located.Ast.span)
                    located_ast;
                form_locations =
                  List.fold_left form_locations [] located_ast;
                parsed_as = `Cljml;
              })
end

module Ocaml_parsetree_backend = struct
  let implementation (typed : typed_result) =
    match
      Ocaml_parsetree.structure_of_located_items
        (List.combine typed.locations typed.items)
    with
    | Error _ as err -> err
    | Ok structure -> Ok { ast = typed.ast; items = typed.items; structure }

  let print = Ocaml_parsetree.print_implementation
end

module Ocaml_typechecker = struct
  type analysis = {
    typed_structure : Typedtree.structure;
    compiler_env : Env.t;
    diagnostics : diagnostic list;
  }

  let exception_message exn =
    Format.asprintf "%a" Location.report_exception exn |> String.trim

  let analyze structure =
    let diagnostics = ref [] in
    let previous_warning_reporter = !Location.warning_reporter in
    let capture_warning location warning =
      match previous_warning_reporter location warning with
      | None -> None
      | Some report ->
          let message =
            Format.asprintf "%a" Location.print_report report |> String.trim
          in
          diagnostics := { message; severity = `Warning } :: !diagnostics;
          None
    in
    try
      let typed_structure, compiler_env =
        Fun.protect
          ~finally:(fun () ->
            Location.warning_reporter := previous_warning_reporter)
          (fun () ->
            Location.warning_reporter := capture_warning;
            Ocaml_signature.init ();
            let env = Compmisc.initial_env () in
            let typed_structure, _signature, _signature_names, _shape, env =
              Typemod.type_structure env structure
            in
            (typed_structure, env))
      in
      Ok
        {
          typed_structure;
          compiler_env;
          diagnostics = List.rev !diagnostics;
        }
    with exn ->
      Error.error ("OCaml typecheck failed: " ^ exception_message exn)

  let structure structure =
    match analyze structure with
    | Error _ as err -> err
    | Ok analysis -> Ok analysis.diagnostics
end

let empty_state =
  { typecheck_state = Typecheck.empty_state; located_items = [] }

let required_packages_from_ast ast =
  let rec loop packages = function
    | [] -> Ok (List.sort_uniq String.compare packages)
    | Ast.FList (Ast.FSymbol "require" :: entries) :: rest -> (
        match Require.parse_entries entries with
        | Error _ as err -> err
        | Ok specs -> loop (Require.package_names specs @ packages) rest)
    | _ :: rest -> loop packages rest
  in
  loop [] ast

let prepare_packages ast =
  match required_packages_from_ast ast with
  | Error _ as err -> err
  | Ok packages -> (
      match Ocaml_package.include_dirs packages with
      | Error _ as err -> err
      | Ok include_dirs ->
          Ocaml_signature.add_include_dirs include_dirs;
          Ok packages)

let checked_parsetree (typed : typed_result) =
  match Ocaml_parsetree_backend.implementation typed with
  | Error _ as err -> err
  | Ok result -> (
      match Ocaml_typechecker.structure result.structure with
      | Error _ as err -> err
      | Ok diagnostics -> Ok (result, diagnostics))

let typecheck (parsed : parser_result) =
  match prepare_packages parsed.ast with
  | Error _ as err -> err
  | Ok _ -> (
      match
        Source_context.with_locations parsed.form_locations (fun () ->
            Typecheck.compile_forms_incremental Typecheck.empty_state parsed.ast)
      with
      | Error _ as err -> err
      | Ok (typecheck_state, items) ->
          Ok
            {
              ast = parsed.ast;
              items;
              locations = parsed.locations;
              typecheck_state;
            })

let typecheck_incremental state (parsed : parser_result) =
  match prepare_packages parsed.ast with
  | Error _ as err -> err
  | Ok _ -> (
      match
        Source_context.with_locations parsed.form_locations (fun () ->
            Typecheck.compile_forms_incremental state.typecheck_state parsed.ast)
      with
      | Error _ as err -> err
      | Ok (typecheck_state, items) ->
          let located_items =
            state.located_items @ List.combine parsed.locations items
          in
          let state = { typecheck_state; located_items } in
          Ok
            ( state,
              {
                ast = parsed.ast;
                items;
                locations = parsed.locations;
                typecheck_state;
              } ))

let required_ocaml_packages source =
  match Cljml_frontend.implementation source with
  | Error _ as err -> err
  | Ok parsed -> required_packages_from_ast parsed.ast

let analyze ?(filename = "<string>") source =
  match Cljml_frontend.implementation ~filename source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck parsed with
      | Error _ as err -> err
      | Ok typed -> (
          match Ocaml_parsetree_backend.implementation typed with
          | Error _ as err -> err
          | Ok parsetree -> (
              match Ocaml_typechecker.analyze parsetree.structure with
              | Error _ as err -> err
              | Ok analysis ->
                  Ok
                    {
                      typed_structure = analysis.typed_structure;
                      compiler_env = analysis.compiler_env;
                      typecheck_state = typed.typecheck_state;
                      diagnostics = analysis.diagnostics;
                    })))

let analyze_workspace sources =
  let ocaml_valid state =
    match Ocaml_parsetree.structure_of_located_items state.located_items with
    | Error _ -> false
    | Ok structure -> (
        match Ocaml_typechecker.analyze structure with
        | Ok _ -> true
        | Error _ -> false)
  in
  let rec parse acc = function
    | [] -> Ok (List.rev acc)
    | (filename, source) :: rest -> (
        match Cljml_frontend.implementation ~filename source with
        | Error _ -> parse acc rest
        | Ok parsed -> parse ((filename, parsed) :: acc) rest)
  in
  let rec compile state compiled pending =
    match pending with
    | [] -> Ok (state, List.rev compiled)
    | _ ->
        let rec try_pending deferred = function
          | [] -> Ok (state, List.rev compiled)
          | (filename, parsed) :: rest -> (
              match typecheck_incremental state parsed with
              | Ok (next_state, _typed) when ocaml_valid next_state ->
                  compile next_state (filename :: compiled)
                    (List.rev_append deferred rest)
              | Ok _ | Error _ ->
                  try_pending ((filename, parsed) :: deferred) rest)
        in
        try_pending [] pending
  in
  match parse [] sources with
  | Error _ as err -> err
  | Ok parsed -> (
      match compile empty_state [] parsed with
      | Error _ as err -> err
      | Ok (_state, filenames) when filenames = [] ->
          Error.error "workspace contains no analyzable cljml files"
      | Ok (state, filenames) -> (
          match Ocaml_parsetree.structure_of_located_items state.located_items with
          | Error _ as err -> err
          | Ok structure -> (
              match Ocaml_typechecker.analyze structure with
              | Error _ as err -> err
              | Ok analysis ->
                  let result =
                    {
                      typed_structure = analysis.typed_structure;
                      compiler_env = analysis.compiler_env;
                      typecheck_state = state.typecheck_state;
                      diagnostics = analysis.diagnostics;
                    }
                  in
                  Ok (List.map (fun filename -> (filename, result)) filenames))))

let implementation_with_diagnostics ?(filename = "<string>") source =
  match Cljml_frontend.implementation ~filename source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck parsed with
      | Error _ as err -> err
      | Ok typed -> (
          match checked_parsetree typed with
          | Error _ as err -> err
          | Ok (result, diagnostics) ->
              Ok
                { ocaml_source = Ocaml_parsetree_backend.print result.structure;
                  diagnostics;
                }))

let implementation ?(filename = "<string>") source =
  match implementation_with_diagnostics ~filename source with
  | Error _ as err -> err
  | Ok compilation -> Ok compilation.ocaml_source

let implementation_parsetree ?(filename = "<string>") source =
  match Cljml_frontend.implementation ~filename source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck parsed with
      | Error _ as err -> err
      | Ok typed -> (
          match checked_parsetree typed with
          | Error _ as err -> err
          | Ok (result, _diagnostics) -> Ok result.structure))

let typecheck_parsetree ?(filename = "<string>") source =
  match Cljml_frontend.implementation ~filename source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck parsed with
      | Error _ as err -> err
      | Ok typed -> (
          match checked_parsetree typed with
          | Error _ as err -> err
          | Ok _ -> Ok ()))

let print_parsetree = Ocaml_parsetree_backend.print

let compile_chunk_with_diagnostics ?(filename = "<string>") state source =
  match Cljml_frontend.implementation ~filename source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck_incremental state parsed with
      | Error _ as err -> err
      | Ok (state, typed) -> (
          match Ocaml_parsetree_backend.implementation typed with
          | Error _ as err -> err
          | Ok result -> (
              match Ocaml_parsetree.structure_of_located_items state.located_items with
              | Error _ as err -> err
              | Ok accumulated_structure -> (
                  match Ocaml_typechecker.structure accumulated_structure with
                  | Error _ as err -> err
                  | Ok diagnostics ->
                      Ok
                        ( state,
                          {
                            ocaml_source =
                              Ocaml_parsetree_backend.print result.structure;
                            diagnostics;
                          } )))))

let compile_chunk ?(filename = "<string>") state source =
  match compile_chunk_with_diagnostics ~filename state source with
  | Error _ as err -> err
  | Ok (state, compilation) -> Ok (state, compilation.ocaml_source)

let compile_chunk_parsetree ?(filename = "<string>") state source =
  match Cljml_frontend.implementation ~filename source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck_incremental state parsed with
      | Error _ as err -> err
      | Ok (state, typed) -> (
          match Ocaml_parsetree_backend.implementation typed with
          | Error _ as err -> err
          | Ok result -> (
              match Ocaml_parsetree.structure_of_located_items state.located_items with
              | Error _ as err -> err
              | Ok accumulated_structure -> (
                  match Ocaml_typechecker.structure accumulated_structure with
                  | Error _ as err -> err
                  | Ok _diagnostics -> Ok (state, result.structure)))))
