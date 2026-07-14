type parser_result = {
  target : Target.t;
  ast : Ast.form list;
  locations : Location.t list;
  form_locations : Source_context.entry list;
  parsed_as : [ `Lg ];
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
  location : Location.t option;
}

type compilation = { ocaml_source : string; diagnostics : diagnostic list }

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

module String_set = Set.Make (String)

module type FRONTEND = sig
  val implementation :
    ?target:Target.t ->
    ?filename:string ->
    string ->
    (parser_result, Error.t) result
end

module Lg_frontend : FRONTEND = struct
  let position filename source offset =
    let rec loop index line line_start =
      if index >= offset then
        {
          Lexing.pos_fname = filename;
          pos_lnum = line;
          pos_bol = line_start;
          pos_cnum = offset;
        }
      else if source.[index] = '\n' then loop (index + 1) (line + 1) (index + 1)
      else loop (index + 1) line line_start
    in
    loop 0 1 0

  let location filename source (span : Ast.source_span) =
    {
      Location.loc_start = position filename source span.start_offset;
      loc_end = position filename source span.end_offset;
      loc_ghost = false;
    }

  let normalize_error_location filename source (error : Error.t) =
    match error.location with
    | None -> error
    | Some location ->
        {
          error with
          location =
            Some
              {
                location with
                loc_start =
                  position filename source location.loc_start.Lexing.pos_cnum;
                loc_end =
                  position filename source location.loc_end.Lexing.pos_cnum;
              };
        }

  let namespace_scope_form span namespace_name =
    {
      Ast.form =
        Ast.FList [ Ast.FSymbol "namespace-scope"; Ast.FSymbol namespace_name ];
      span;
      children = [];
    }

  let metadata_symbol name = String.starts_with ~prefix:"^" name

  let host_type_hint name =
    metadata_symbol name && not (String.starts_with ~prefix:"^:" name)

  let supported_type_hint name =
    if not (host_type_hint name) then false
    else
      let type_name = String.sub name 1 (String.length name - 1) in
      Option.is_some (Host_interop.type_annotation type_name)
      || type_name = "clojure.lang.Associative"
      || (not (String.contains type_name '.'))
         && not (String.contains type_name '/')

  let rec drop_definition_metadata = function
    | Ast.FSymbol metadata :: rest when metadata_symbol metadata ->
        drop_definition_metadata rest
    | forms -> forms

  let rec normalize_metadata = function
    | Ast.FList
        (Ast.FSymbol (("def" | "defonce" | "defn" | "defn-") as head)
        :: forms) ->
        Ast.FList
          (Ast.FSymbol head
          :: normalize_metadata_sequence (drop_definition_metadata forms))
    | Ast.FList forms -> Ast.FList (normalize_metadata_sequence forms)
    | Ast.FVector forms ->
        Ast.FVector (normalize_vector_metadata_sequence forms)
    | Ast.FMap entries ->
        Ast.FMap
          (List.map
             (fun (key, value) ->
               (normalize_metadata key, normalize_metadata value))
             entries)
    | form -> form

  and normalize_metadata_sequence = function
    | Ast.FSymbol metadata :: form :: rest
      when supported_type_hint metadata ->
        Ast.FList
          [ Ast.FSymbol "__type-hint";
            Ast.FSymbol metadata;
            normalize_metadata form ]
        :: normalize_metadata_sequence rest
    | Ast.FSymbol metadata :: rest when host_type_hint metadata ->
        normalize_metadata_sequence rest
    | Ast.FSymbol metadata :: rest when metadata_symbol metadata ->
        normalize_metadata_sequence rest
    | form :: rest ->
        normalize_metadata form :: normalize_metadata_sequence rest
    | [] -> []

  and normalize_vector_metadata_sequence = function
    | Ast.FSymbol metadata :: rest when supported_type_hint metadata ->
        Ast.FSymbol metadata :: normalize_vector_metadata_sequence rest
    | Ast.FSymbol metadata :: rest when host_type_hint metadata ->
        normalize_vector_metadata_sequence rest
    | form :: rest ->
        normalize_metadata form :: normalize_vector_metadata_sequence rest
    | [] -> []

  let normalize_located_metadata located =
    { located with Ast.form = normalize_metadata located.Ast.form }

  let extract_compile_time_helpers located_ast =
    let rec quoted_refs refs = function
      | Ast.FList
          [ Ast.FSymbol ("unquote" | "unquote-splicing"); expression ] ->
          form_refs refs expression
      | Ast.FList forms | Ast.FVector forms ->
          List.fold_left quoted_refs refs forms
      | Ast.FMap entries ->
          List.fold_left
            (fun refs (key, value) ->
              quoted_refs (quoted_refs refs key) value)
            refs entries
      | _ -> refs
    and form_refs refs = function
      | Ast.FList [ Ast.FSymbol "syntax-quote"; quoted ] ->
          quoted_refs refs quoted
      | Ast.FList (Ast.FSymbol name :: forms) ->
          List.fold_left form_refs (String_set.add name refs) forms
      | Ast.FList forms | Ast.FVector forms ->
          List.fold_left form_refs refs forms
      | Ast.FMap entries ->
          List.fold_left
            (fun refs (key, value) -> form_refs (form_refs refs key) value)
            refs entries
      | Ast.FSymbol name -> String_set.add name refs
      | _ -> refs
    in
    let definitions =
      located_ast
      |> List.filter_map (fun located ->
             match located.Ast.form with
             | Ast.FList
                 (Ast.FSymbol ("def" | "defonce" | "defn" | "defn-")
                 :: Ast.FSymbol name :: forms) ->
                 Some (name, forms)
             | _ -> None)
    in
    let initial_refs =
      List.fold_left
        (fun refs located ->
          match located.Ast.form with
          | Ast.FList (Ast.FSymbol "defmacro" :: _name :: forms) ->
              List.fold_left form_refs refs forms
          | _ -> refs)
        String_set.empty located_ast
    in
    let rec close refs =
      let expanded =
        List.fold_left
          (fun refs (name, forms) ->
            if String_set.mem name refs then
              List.fold_left form_refs refs forms
            else refs)
          refs definitions
      in
      if String_set.equal refs expanded then refs else close expanded
    in
    let helper_names = close initial_refs in
    List.map
      (fun located ->
        match located.Ast.form with
        | Ast.FList
            (Ast.FSymbol ("defn" | "defn-") :: Ast.FSymbol name :: forms)
          when String_set.mem name helper_names ->
            {
              located with
              Ast.form =
                Ast.FList
                  (Ast.FSymbol "macro-helper-defn" :: Ast.FSymbol name :: forms);
            }
        | Ast.FList
            (Ast.FSymbol ("def" | "defonce") :: Ast.FSymbol name :: forms)
          when String_set.mem name helper_names ->
            {
              located with
              Ast.form =
                Ast.FList
                  (Ast.FSymbol "macro-helper-def" :: Ast.FSymbol name :: forms);
            }
        | _ -> located)
      located_ast

  let lower_namespace located_ast =
    let is_namespace = function
      | { Ast.form = Ast.FList (Ast.FSymbol "ns" :: _); _ } -> true
      | _ -> false
    in
    match located_ast with
    | [] -> Ok []
    | { Ast.form = Ast.FList (Ast.FSymbol "ns" :: forms); span; _ } :: body -> (
        if List.exists is_namespace body then
          Error.error "ns may only appear once at the start of a file"
        else
          let rec drop_namespace_metadata = function
            | Ast.FSymbol metadata :: rest
              when String.starts_with ~prefix:"^" metadata ->
                drop_namespace_metadata rest
            | forms -> forms
          in
          match drop_namespace_metadata forms with
          | Ast.FSymbol namespace_name :: clauses ->
              let segments = String.split_on_char '.' namespace_name in
              if List.exists (fun segment -> segment = "") segments then
                Error.error "ns expects a namespace symbol and optional clauses"
              else
                let normalize_import = function
                  | Ast.FSymbol qualified_name -> (
                      match String.rindex_opt qualified_name '.' with
                      | Some separator ->
                          let package = String.sub qualified_name 0 separator in
                          let class_name =
                            String.sub qualified_name (separator + 1)
                              (String.length qualified_name - separator - 1)
                          in
                          Ast.FVector
                            [ Ast.FSymbol package; Ast.FSymbol class_name ]
                      | None -> Ast.FSymbol qualified_name)
                  | Ast.FList entries -> Ast.FVector entries
                  | entry -> entry
                in
                let rec parse_clauses require_entries exclusions imports = function
                  | [] ->
                      Ok
                        ( List.rev require_entries |> List.concat,
                          List.rev exclusions |> List.concat,
                          List.rev imports |> List.concat )
                  | Ast.FList
                      (Ast.FKeyword (":require" | ":require-macros") :: entries)
                    :: rest ->
                      parse_clauses (entries :: require_entries) exclusions imports
                        rest
                  | Ast.FList
                      [ Ast.FKeyword ":refer-clojure";
                        Ast.FKeyword ":exclude";
                        Ast.FVector names ]
                    :: rest ->
                      parse_clauses require_entries (names :: exclusions) imports
                        rest
                  | Ast.FList (Ast.FKeyword ":import" :: entries) :: rest ->
                      parse_clauses require_entries exclusions
                        (List.map normalize_import entries :: imports) rest
                  | _ ->
                      Error.error
                        "ns supports :require, :require-macros, :refer-clojure :exclude, and :import clauses"
                in
                Result.map
                  (fun (require_entries, exclusions, imports) ->
                    let namespace_form = namespace_scope_form span namespace_name in
                    let synthetic_form head entries =
                      {
                        Ast.form =
                          Ast.FList (Ast.FSymbol head :: entries);
                        span;
                        children = [];
                      }
                    in
                    let clauses =
                      []
                      |> (fun forms ->
                           if require_entries = [] then forms
                           else synthetic_form "require" require_entries :: forms)
                      |> (fun forms ->
                           if exclusions = [] then forms
                           else
                             synthetic_form "refer-clojure-exclude" exclusions
                             :: forms)
                      |> (fun forms ->
                           if imports = [] then forms
                           else synthetic_form "host-import" imports :: forms)
                      |> List.rev
                    in
                    namespace_form :: clauses @ body)
                  (parse_clauses [] [] [] clauses)
          | _ -> Error.error "ns expects a namespace symbol and optional clauses")
    | first :: rest ->
        if List.exists is_namespace rest then
          Error.error "ns may only appear once at the start of a file"
        else Ok (first :: rest)

  let defer_deftype_methods located_ast =
    let declared_names =
      located_ast
      |> List.concat_map (fun located ->
             match located.Ast.form with
             | Ast.FList (Ast.FSymbol "declare" :: names) ->
                 List.filter_map
                   (function Ast.FSymbol name -> Some name | _ -> None)
                   names
             | _ -> [])
    in
    let definition_name located =
      match located.Ast.form with
      | Ast.FList
          (Ast.FSymbol ("def" | "defonce" | "defn" | "defn-")
          :: Ast.FSymbol name :: _) ->
          Some name
      | _ -> None
    in
    let flush output_rev deferred_rev = deferred_rev @ output_rev in
    let rec loop output_rev deferred_rev unresolved = function
      | [] -> List.rev (flush output_rev deferred_rev)
      | ({ Ast.form =
             Ast.FList
               (Ast.FSymbol "deftype" :: name :: fields :: (_ :: _ as methods));
           _ } as located)
        :: rest ->
          let type_form =
            { located with
              Ast.form = Ast.FList [ Ast.FSymbol "deftype"; name; fields ];
            }
          in
          let methods_form =
            { located with
              Ast.form =
                Ast.FList
                  (Ast.FSymbol "deftype-methods" :: name :: methods);
            }
          in
          let output_rev = type_form :: output_rev in
          let deferred_rev = methods_form :: deferred_rev in
          if unresolved = [] then
            loop (flush output_rev deferred_rev) [] unresolved rest
          else loop output_rev deferred_rev unresolved rest
      | form :: rest ->
          let unresolved =
            match definition_name form with
            | None -> unresolved
            | Some name -> List.filter (fun declared -> declared <> name) unresolved
          in
          let output_rev = form :: output_rev in
          if unresolved = [] && deferred_rev <> [] then
            loop (flush output_rev deferred_rev) [] unresolved rest
          else loop output_rev deferred_rev unresolved rest
    in
    loop [] [] declared_names located_ast

  let group_declared_functions located_ast =
    let declared_names =
      located_ast
      |> List.concat_map (fun located ->
             match located.Ast.form with
             | Ast.FList (Ast.FSymbol "declare" :: names) ->
                 List.filter_map
                   (function Ast.FSymbol name -> Some name | _ -> None)
                   names
             | _ -> [])
    in
    let rec loop output_rev definitions_rev unresolved = function
      | [] ->
          if definitions_rev = [] then List.rev output_rev
          else List.rev output_rev @ List.rev definitions_rev
      | ({ Ast.form =
             Ast.FList
               (Ast.FSymbol ("defn" | "defn-") :: Ast.FSymbol name :: _);
           _ } as definition)
        :: rest
        when definitions_rev <> [] || List.mem name declared_names ->
          let signature =
            { definition with
              Ast.form =
                Ast.FList
                  [ Ast.FSymbol "defn-signature"; definition.Ast.form ];
            }
          in
          let definitions_rev = definition :: definitions_rev in
          let unresolved =
            if List.mem name declared_names then
              List.filter (fun candidate -> candidate <> name) unresolved
            else unresolved
          in
          if unresolved = [] then
            let definitions = List.rev definitions_rev in
            let group =
              { definition with
                Ast.form =
                  Ast.FList
                    (Ast.FSymbol "defn-group"
                    :: List.map (fun item -> item.Ast.form) definitions);
              }
            in
            loop (group :: signature :: output_rev) [] unresolved rest
          else loop (signature :: output_rev) definitions_rev unresolved rest
      | form :: rest -> loop (form :: output_rev) definitions_rev unresolved rest
    in
    loop [] [] declared_names located_ast

  let implementation ?(target = Target.default) ?(filename = "<string>") source
      =
    let is_compile_time_form located =
      match located.Ast.form with
      | Ast.FList
          (Ast.FSymbol ("defmacro" | "macro-helper-defn" | "macro-helper-def")
          :: _) ->
          true
      | _ -> false
    in
    let drop_compile_time_only_host_imports located_ast =
      let runtime_forms =
        List.filter
          (fun located ->
            (not (is_compile_time_form located))
            &&
            match located.Ast.form with
            | Ast.FList (Ast.FSymbol "host-import" :: _) -> false
            | _ -> true)
          located_ast
      in
      let symbol_uses_class class_name symbol =
        symbol = class_name
        || symbol = "^" ^ class_name
        || String.starts_with ~prefix:(class_name ^ "/") symbol
        || String.starts_with ~prefix:(class_name ^ ".") symbol
      in
      let rec form_uses_class class_name = function
        | Ast.FSymbol symbol -> symbol_uses_class class_name symbol
        | Ast.FList forms | Ast.FVector forms ->
            List.exists (form_uses_class class_name) forms
        | Ast.FMap entries ->
            List.exists
              (fun (key, value) ->
                form_uses_class class_name key
                || form_uses_class class_name value)
              entries
        | _ -> false
      in
      let class_is_used class_name =
        List.exists
          (fun located -> form_uses_class class_name located.Ast.form)
          runtime_forms
      in
      List.filter_map
        (fun located ->
          match located.Ast.form with
          | Ast.FList (Ast.FSymbol "host-import" :: entries) ->
              let entries =
                List.filter_map
                  (function
                    | Ast.FVector (package :: classes) ->
                        let classes =
                          List.filter
                            (function
                              | Ast.FSymbol class_name -> class_is_used class_name
                              | _ -> true)
                            classes
                        in
                        if classes = [] then None
                        else Some (Ast.FVector (package :: classes))
                    | entry -> Some entry)
                  entries
              in
              if entries = [] then None
              else
                Some
                  {
                    located with
                    Ast.form =
                      Ast.FList (Ast.FSymbol "host-import" :: entries);
                  }
          | _ -> Some located)
        located_ast
    in
    let drop_clojure_compiler_directives located_ast =
      List.filter
        (fun located ->
          match located.Ast.form with
          | Ast.FList
              [ Ast.FSymbol "set!";
                Ast.FSymbol
                  ("*warn-on-reflection*" | "*unchecked-math*");
                _ ] ->
              false
          | _ -> true)
        located_ast
    in
    let same_span left right =
      left.Ast.span.start_offset = right.Ast.span.start_offset
      && left.Ast.span.end_offset = right.Ast.span.end_offset
    in
    let add_clj_compile_time_forms tokens original_located_ast located_ast =
      match target with
      | Target.Native -> Ok located_ast
      | Target.Melange | Target.Js_of_ocaml -> (
          match Parser.parse_located ~target:Target.Native tokens with
          | Error _ as error -> error
          | Ok native_original -> (
              match lower_namespace native_original with
              | Error _ as error -> error
              | Ok native_located ->
                  let compile_time_forms =
                    native_located
                    |> List.map normalize_located_metadata
                    |> extract_compile_time_helpers
                    |> List.filter is_compile_time_form
                    |> List.filter (fun candidate ->
                           not
                             (List.exists
                                (same_span candidate)
                                original_located_ast))
                  in
                  (match located_ast with
                  | ({ Ast.form =
                         Ast.FList [ Ast.FSymbol "namespace-scope"; _ ];
                       _ } as namespace_scope)
                    :: rest ->
                      Ok (namespace_scope :: compile_time_forms @ rest)
                  | _ -> Ok (compile_time_forms @ located_ast))))
    in
    match Lexer.tokenize source with
    | Error _ as err -> err
    | Ok tokens -> (
        match Parser.parse_located ~target tokens with
        | Error error -> Error (normalize_error_location filename source error)
        | Ok original_located_ast -> (
            match lower_namespace original_located_ast with
            | Error error -> Error (normalize_error_location filename source error)
            | Ok target_located_ast -> (
                match
                  add_clj_compile_time_forms tokens original_located_ast
                    target_located_ast
                with
                | Error error ->
                    Error (normalize_error_location filename source error)
                | Ok located_ast ->
                let located_ast =
                  located_ast
                  |> List.map normalize_located_metadata
                  |> extract_compile_time_helpers
                  |> drop_clojure_compiler_directives
                  |> drop_compile_time_only_host_imports
                  |> defer_deftype_methods |> group_declared_functions
                in
                let rec form_locations acc located =
                  let location = location filename source located.Ast.span in
                  List.fold_left form_locations
                    ((located.Ast.form, location) :: acc)
                    located.Ast.children
                in
                Ok
                  {
                    target;
                    ast = List.map (fun located -> located.Ast.form) located_ast;
                    locations =
                      List.map
                        (fun located -> location filename source located.Ast.span)
                        located_ast;
                    form_locations =
                      List.fold_left form_locations
                        (List.fold_left form_locations [] original_located_ast)
                        located_ast;
                    parsed_as = `Lg;
                  })))
end

module Ocaml_parsetree_backend = struct
  let implementation (typed : typed_result) =
    match
      Lowering.structure_of_located_items
        (List.combine typed.locations typed.items)
    with
    | Error _ as err -> err
    | Ok structure -> Ok { ast = typed.ast; items = typed.items; structure }

  let print = Lowering.print_implementation
end

module Ocaml_typechecker = struct
  type analysis = {
    typed_structure : Typedtree.structure;
    compiler_env : Env.t;
    diagnostics : diagnostic list;
  }

  let exception_message exn =
    Format.asprintf "%a" Location.report_exception exn |> String.trim

  let exception_location exn =
    match Location.error_of_exn exn with
    | Some (`Ok report) when not report.Location.main.loc.loc_ghost ->
        Some report.main.loc
    | Some (`Ok _) | Some `Already_displayed | None -> None

  let initial_env_cache = ref None

  let initial_env () =
    let include_dirs = Ocaml_signature.active_include_dirs () in
    match !initial_env_cache with
    | Some (cached_dirs, env) when cached_dirs = include_dirs -> env
    | _ ->
        Ocaml_signature.init ();
        [ "unix"; "str" ]
        |> List.map (Filename.concat Config.standard_library)
        |> List.filter Sys.file_exists
        |> List.iter (Load_path.add_dir ~hidden:false);
        let env = Compmisc.initial_env () in
        initial_env_cache := Some (include_dirs, env);
        env

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
          diagnostics :=
            { message; severity = `Warning; location = Some location }
            :: !diagnostics;
          None
    in
    try
      let typed_structure, compiler_env =
        Fun.protect
          ~finally:(fun () ->
            Location.warning_reporter := previous_warning_reporter)
          (fun () ->
            Location.warning_reporter := capture_warning;
            let env = initial_env () in
            let typed_structure, _signature, _signature_names, _shape, env =
              Typemod.type_structure env structure
            in
            (typed_structure, env))
      in
      Ok { typed_structure; compiler_env; diagnostics = List.rev !diagnostics }
    with exn ->
      Error.error ?location:(exception_location exn)
        ("OCaml typecheck failed: " ^ exception_message exn)

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

let prepare_packages target ast =
  match required_packages_from_ast ast with
  | Error _ as err -> err
  | Ok packages ->
      let packages =
        match target with
        | Target.Melange -> "melange" :: packages
        | Target.Js_of_ocaml -> "js_of_ocaml" :: packages
        | Target.Native -> packages
      in
      (
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
  match prepare_packages parsed.target parsed.ast with
  | Error _ as err -> err
  | Ok _ -> (
      let initial_state =
        Compiler_state.with_target parsed.target Typecheck.empty_state
      in
      let compile state =
        Source_context.with_locations parsed.form_locations (fun () ->
            Typecheck.compile_forms_incremental state parsed.ast)
      in
      match compile initial_state
      with
      | Error _ as err -> err
      | Ok (first_state, _) ->
          let rec declared_names declared = function
            | [] -> List.rev declared
            | Ast.FList (Ast.FSymbol "declare" :: form_names) :: rest ->
                let declared =
                  List.fold_left
                    (fun declared -> function
                      | Ast.FSymbol name -> name :: declared
                      | _ -> declared)
                    declared form_names
                in
                declared_names declared rest
            | Ast.FList
                [ Ast.FSymbol "defn-signature";
                  Ast.FList
                    (Ast.FSymbol ("defn" | "defn-")
                    :: Ast.FSymbol name :: _);
                ]
              :: rest ->
                declared_names (name :: declared) rest
            | _ :: rest -> declared_names declared rest
          in
          let declarations =
            let final_bindings =
              Compiler_environment.to_bindings first_state.env
            in
            declared_names [] parsed.ast
            |> List.concat_map (fun name ->
                   let suffix = "/" ^ name in
                   final_bindings
                   |> List.filter (fun (key, _) ->
                          key = name || String.ends_with ~suffix key))
            |> List.map (fun (key, (binding : Types.binding)) ->
                   (key, { binding with forward_declared = true }))
            |> List.sort_uniq (fun (left, _) (right, _) ->
                   String.compare left right)
          in
          let seeded_state =
            { initial_state with
              env =
                Compiler_environment.add_bindings declarations
                  initial_state.env }
          in
          (match compile seeded_state with
          | Error _ as err -> err
          | Ok (typecheck_state, items) ->
          Ok
            {
              ast = parsed.ast;
              items;
              locations = parsed.locations;
              typecheck_state;
            }) )

let typecheck_incremental state (parsed : parser_result) =
  match prepare_packages parsed.target parsed.ast with
  | Error _ as err -> err
  | Ok _ -> (
      let initial_state =
        if state.located_items = [] then
          Compiler_state.with_target parsed.target state.typecheck_state
        else state.typecheck_state
      in
      let compile typecheck_state =
        Source_context.with_locations parsed.form_locations (fun () ->
            Typecheck.compile_forms_incremental typecheck_state parsed.ast)
      in
      match compile initial_state
      with
      | Error _ as err -> err
      | Ok (first_state, _) ->
          let rec signature_names names = function
            | [] -> List.rev names
            | Ast.FList
                [ Ast.FSymbol "defn-signature";
                  Ast.FList
                    (Ast.FSymbol ("defn" | "defn-")
                    :: Ast.FSymbol name :: _);
                ]
              :: rest ->
                signature_names (name :: names) rest
            | _ :: rest -> signature_names names rest
          in
          let final_bindings =
            Compiler_environment.to_bindings first_state.env
          in
          let declarations =
            signature_names [] parsed.ast
            |> List.concat_map (fun name ->
                   let suffix = "/" ^ name in
                   final_bindings
                   |> List.filter (fun (key, _) ->
                          key = name || String.ends_with ~suffix key))
            |> List.map (fun (key, (binding : Types.binding)) ->
                   (key, { binding with forward_declared = true }))
            |> List.sort_uniq (fun (left, _) (right, _) ->
                   String.compare left right)
          in
          let seeded_state =
            { initial_state with
              env =
                Compiler_environment.add_bindings declarations
                  initial_state.env }
          in
          (match compile seeded_state with
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
              } )))

let required_ocaml_packages ?(target = Target.default) source =
  match Lg_frontend.implementation ~target source with
  | Error _ as err -> err
  | Ok parsed -> required_packages_from_ast parsed.ast

let analyze ?(target = Target.default) ?(filename = "<string>") source =
  match Lg_frontend.implementation ~target ~filename source with
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

let interface ?(target = Target.default) ?(filename = "<string>") source =
  match analyze ~target ~filename source with
  | Error _ as err -> err
  | Ok analysis ->
      Ok
        (Printtyp.wrap_printing_env ~error:false analysis.compiler_env
           (fun () ->
             Format.asprintf "%a@." Printtyp.signature
               analysis.typed_structure.str_type))

let analyze_workspace_with_errors ?(target = Target.default) sources =
  let validate_ocaml state =
    match Lowering.structure_of_located_items state.located_items with
    | Error _ as err -> err
    | Ok structure -> Ocaml_typechecker.analyze structure |> Result.map ignore
  in
  let rec parse parsed errors = function
    | [] -> Ok (List.rev parsed, List.rev errors)
    | (filename, source) :: rest -> (
        match Lg_frontend.implementation ~target ~filename source with
        | Error error -> parse parsed ((filename, error) :: errors) rest
        | Ok result -> parse ((filename, result) :: parsed) errors rest)
  in
  let rec compile state compiled pending =
    match pending with
    | [] -> Ok (state, List.rev compiled, [])
    | _ ->
        let rec try_pending deferred errors = function
          | [] -> Ok (state, List.rev compiled, List.rev errors)
          | (filename, parsed) :: rest -> (
              match typecheck_incremental state parsed with
              | Error error ->
                  try_pending
                    ((filename, parsed) :: deferred)
                    ((filename, error) :: errors)
                    rest
              | Ok (next_state, _typed) -> (
                  match validate_ocaml next_state with
                  | Ok () ->
                      compile next_state (filename :: compiled)
                        (List.rev_append deferred rest)
                  | Error error ->
                      try_pending
                        ((filename, parsed) :: deferred)
                        ((filename, error) :: errors)
                        rest))
        in
        try_pending [] [] pending
  in
  match parse [] [] sources with
  | Error _ as err -> err
  | Ok (parsed, parse_errors) -> (
      match compile empty_state [] parsed with
      | Error _ as err -> err
      | Ok (_state, [], compile_errors) -> Ok ([], parse_errors @ compile_errors)
      | Ok (state, filenames, compile_errors) -> (
          match Lowering.structure_of_located_items state.located_items with
          | Error _ as err -> err
          | Ok structure -> (
              match Ocaml_typechecker.analyze structure with
              | Error _ as err -> err
              | Ok analysis ->
                  let result filename =
                    {
                      typed_structure = analysis.typed_structure;
                      compiler_env = analysis.compiler_env;
                      typecheck_state = state.typecheck_state;
                      diagnostics =
                        List.filter
                          (fun diagnostic ->
                            match diagnostic.location with
                            | Some location ->
                                location.Location.loc_start.Lexing.pos_fname
                                = filename
                            | None -> false)
                          analysis.diagnostics;
                    }
                  in
                  Ok
                    ( List.map
                        (fun filename -> (filename, result filename))
                        filenames,
                      parse_errors @ compile_errors ))))

let analyze_workspace ?(target = Target.default) sources =
  match analyze_workspace_with_errors ~target sources with
  | Error _ as err -> err
  | Ok ([], (_, error) :: _) -> Error error
  | Ok ([], []) -> Error.error "workspace contains no analyzable lg files"
  | Ok (analyses, []) -> Ok analyses
  | Ok (analyses, _errors) -> Ok analyses

let implementation_with_diagnostics ?(target = Target.default)
    ?(filename = "<string>") source =
  match Lg_frontend.implementation ~target ~filename source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck parsed with
      | Error _ as err -> err
      | Ok typed -> (
          match checked_parsetree typed with
          | Error _ as err -> err
          | Ok (result, diagnostics) ->
              Ok
                {
                  ocaml_source = Ocaml_parsetree_backend.print result.structure;
                  diagnostics;
                }))

let implementation ?(target = Target.default) ?(filename = "<string>") source =
  match implementation_with_diagnostics ~target ~filename source with
  | Error _ as err -> err
  | Ok compilation -> Ok compilation.ocaml_source

let implementation_parsetree ?(target = Target.default) ?(filename = "<string>")
    source =
  match Lg_frontend.implementation ~target ~filename source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck parsed with
      | Error _ as err -> err
      | Ok typed -> (
          match checked_parsetree typed with
          | Error _ as err -> err
          | Ok (result, _diagnostics) -> Ok result.structure))

let typecheck_parsetree ?(target = Target.default) ?(filename = "<string>")
    source =
  match Lg_frontend.implementation ~target ~filename source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck parsed with
      | Error _ as err -> err
      | Ok typed -> (
          match checked_parsetree typed with
          | Error _ as err -> err
          | Ok _ -> Ok ()))

let print_parsetree = Ocaml_parsetree_backend.print

let compile_chunk_with_diagnostics ?(target = Target.default)
    ?(filename = "<string>") state source =
  match Lg_frontend.implementation ~target ~filename source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck_incremental state parsed with
      | Error _ as err -> err
      | Ok (state, typed) -> (
          match Ocaml_parsetree_backend.implementation typed with
          | Error _ as err -> err
          | Ok result -> (
              match Lowering.structure_of_located_items state.located_items with
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

let compile_chunk ?(target = Target.default) ?(filename = "<string>") state
    source =
  match compile_chunk_with_diagnostics ~target ~filename state source with
  | Error _ as err -> err
  | Ok (state, compilation) -> Ok (state, compilation.ocaml_source)

let compile_chunk_parsetree ?(target = Target.default) ?(filename = "<string>")
    state source =
  match Lg_frontend.implementation ~target ~filename source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck_incremental state parsed with
      | Error _ as err -> err
      | Ok (state, typed) -> (
          match Ocaml_parsetree_backend.implementation typed with
          | Error _ as err -> err
          | Ok result -> (
              match Lowering.structure_of_located_items state.located_items with
              | Error _ as err -> err
              | Ok accumulated_structure -> (
                  match Ocaml_typechecker.structure accumulated_structure with
                  | Error _ as err -> err
                  | Ok _diagnostics -> Ok (state, result.structure)))))
