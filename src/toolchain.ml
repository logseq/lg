type parser_result = {
  target : Target.t;
  source_unit : string;
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
  requested_set_modules : string list;
  ocaml_env : Env.t option;
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
  let line_starts source =
    let starts = ref [ 0 ] in
    String.iteri
      (fun index char ->
        if char = '\n' then starts := (index + 1) :: !starts)
      source;
    Array.of_list (List.rev !starts)

  let position filename line_starts offset =
    let rec search low high =
      if low > high then high
      else
        let middle = low + ((high - low) / 2) in
        if line_starts.(middle) <= offset then search (middle + 1) high
        else search low (middle - 1)
    in
    let line_index = search 0 (Array.length line_starts - 1) in
    {
      Lexing.pos_fname = filename;
      pos_lnum = line_index + 1;
      pos_bol = line_starts.(line_index);
      pos_cnum = offset;
    }

  let location filename line_starts (span : Ast.source_span) =
    {
      Location.loc_start = position filename line_starts span.start_offset;
      loc_end = position filename line_starts span.end_offset;
      loc_ghost = false;
    }

  let normalize_error_location filename line_starts (error : Error.t) =
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
                  position filename line_starts
                    location.loc_start.Lexing.pos_cnum;
                loc_end =
                  position filename line_starts
                    location.loc_end.Lexing.pos_cnum;
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

  let definition_type_hint name =
    host_type_hint name
    || String.contains name '<'
    || List.mem name
         [
           "^:int";
           "^:ordering";
           "^:float";
           "^:char";
           "^:string";
           "^:symbol";
           "^:keyword";
           "^:bool";
           "^:unit";
           "^:buffer";
         ]

  let supported_type_hint name =
    if not (host_type_hint name) then false
    else
      let type_name = String.sub name 1 (String.length name - 1) in
      let qualified_record_hint =
        match String.rindex_opt type_name '/' with
        | Some separator when separator < String.length type_name - 1 ->
            let local_name =
              String.sub type_name (separator + 1)
                (String.length type_name - separator - 1)
            in
            Char.uppercase_ascii local_name.[0] = local_name.[0]
        | Some _ | None -> false
      in
      Option.is_some (Host_interop.type_annotation type_name)
      || qualified_record_hint
      || (not (String.contains type_name '.'))
         && not (String.contains type_name '/')

  let metadata_map_annotations entries =
    entries
    |> List.filter_map (function
         | Ast.FKeyword ":tag", (Ast.FString tag | Ast.FSymbol tag) ->
             Some (Ast.FSymbol ("^" ^ tag))
         | Ast.FKeyword ":tag", Ast.FKeyword tag ->
             Some (Ast.FSymbol ("^" ^ tag))
         | Ast.FKeyword ":dynamic", Ast.FBool true ->
             Some (Ast.FSymbol "^:dynamic")
         | _ -> None)

  let keyword_metadata_value metadata form =
    if
      String.starts_with ~prefix:"^:" metadata
      && match form with Ast.FVector _ | Ast.FMap _ -> true | _ -> false
    then
      let keyword = String.sub metadata 1 (String.length metadata - 1) in
      Some
        (Ast.FList
           [
             Ast.FSymbol "with-meta";
             form;
             Ast.FMap [ (Ast.FKeyword keyword, Ast.FBool true) ];
           ])
    else None

  let rec drop_definition_metadata = function
    | Ast.FSymbol "^" :: Ast.FMap _ :: rest ->
        drop_definition_metadata rest
    | Ast.FSymbol metadata :: rest when metadata_symbol metadata ->
        drop_definition_metadata rest
    | forms -> forms

  let dynamic_definition_forms forms =
    match forms with
    | Ast.FSymbol "^" :: Ast.FMap entries :: rest
      when List.mem (Ast.FKeyword ":dynamic", Ast.FBool true) entries ->
        Ast.FSymbol "^:dynamic" :: drop_definition_metadata rest
    | Ast.FSymbol "^:dynamic" :: rest ->
        Ast.FSymbol "^:dynamic" :: drop_definition_metadata rest
    | forms -> drop_definition_metadata forms

  let rec normalize_metadata = function
    | Ast.FList
        (Ast.FSymbol (("def" | "defonce") as head) :: forms) ->
        (match forms with
        | Ast.FSymbol "^:dynamic"
          :: Ast.FSymbol annotation
          :: name :: [ value ]
          when definition_type_hint annotation ->
            Ast.FList
              [
                Ast.FSymbol head;
                Ast.FSymbol "^:dynamic";
                normalize_metadata name;
                Ast.FList
                  [
                    Ast.FSymbol "__type-hint";
                    Ast.FSymbol annotation;
                    normalize_metadata value;
                  ];
              ]
        | Ast.FSymbol metadata
          :: Ast.FSymbol annotation
          :: name :: [ value ]
          when metadata_symbol metadata && definition_type_hint annotation ->
            Ast.FList
              [
                Ast.FSymbol head;
                normalize_metadata name;
                Ast.FList
                  [
                    Ast.FSymbol "__type-hint";
                    Ast.FSymbol annotation;
                    normalize_metadata value;
                  ];
              ]
        | Ast.FSymbol annotation :: name :: [ value ]
          when definition_type_hint annotation ->
            Ast.FList
              [
                Ast.FSymbol head;
                normalize_metadata name;
                Ast.FList
                  [
                    Ast.FSymbol "__type-hint";
                    Ast.FSymbol annotation;
                    normalize_metadata value;
                  ];
              ]
        | forms ->
            let forms = dynamic_definition_forms forms in
            let forms =
              match forms with
              | Ast.FSymbol "^:dynamic" :: rest ->
                  Ast.FSymbol "^:dynamic" :: normalize_metadata_sequence rest
              | forms -> normalize_metadata_sequence forms
            in
            Ast.FList (Ast.FSymbol head :: forms))
    | Ast.FList
        (Ast.FSymbol (("defn" | "defn-") as head) :: forms)
      ->
        (match forms with
        | Ast.FSymbol annotation :: name :: rest
          when definition_type_hint annotation ->
            Ast.FList
              (Ast.FSymbol head :: normalize_metadata name
              :: Ast.FSymbol annotation
              :: normalize_metadata_sequence rest)
        | forms ->
            Ast.FList
              (Ast.FSymbol head
              :: normalize_metadata_sequence
                   (drop_definition_metadata forms)))
    | Ast.FList forms -> Ast.FList (normalize_metadata_sequence forms)
    | Ast.FVector forms ->
        Ast.FVector (normalize_vector_metadata_sequence forms)
    | Ast.FMap entries ->
        let forms =
          entries
          |> List.concat_map (fun (key, value) -> [ key; value ])
          |> normalize_metadata_sequence
        in
        let rec pairs acc = function
          | key :: value :: rest -> pairs ((key, value) :: acc) rest
          | [] -> List.rev acc
          | [ _ ] -> assert false
        in
        Ast.FMap (pairs [] forms)
    | form -> form

  and normalize_metadata_sequence = function
    | Ast.FSymbol "^" :: Ast.FMap entries :: form :: rest ->
        normalize_metadata_sequence
          (metadata_map_annotations entries @ (form :: rest))
    | Ast.FSymbol metadata :: form :: rest -> (
        match keyword_metadata_value metadata form with
        | Some annotated ->
            normalize_metadata annotated :: normalize_metadata_sequence rest
        | None when supported_type_hint metadata ->
            Ast.FList
              [
                Ast.FSymbol "__type-hint";
                Ast.FSymbol metadata;
                normalize_metadata form;
              ]
            :: normalize_metadata_sequence rest
        | None when host_type_hint metadata || metadata_symbol metadata ->
            normalize_metadata_sequence (form :: rest)
        | None ->
            normalize_metadata (Ast.FSymbol metadata)
            :: normalize_metadata_sequence (form :: rest))
    | form :: rest ->
        normalize_metadata form :: normalize_metadata_sequence rest
    | [] -> []

  and normalize_vector_metadata_sequence = function
    | Ast.FSymbol "^" :: Ast.FMap entries :: form :: rest ->
        normalize_vector_metadata_sequence
          (metadata_map_annotations entries @ (form :: rest))
    | Ast.FSymbol metadata :: form :: rest -> (
        match keyword_metadata_value metadata form with
        | Some annotated ->
            normalize_metadata annotated
            :: normalize_vector_metadata_sequence rest
        | None when supported_type_hint metadata ->
            Ast.FSymbol metadata
            :: normalize_vector_metadata_sequence (form :: rest)
        | None when host_type_hint metadata ->
            normalize_vector_metadata_sequence (form :: rest)
        | None ->
            normalize_metadata (Ast.FSymbol metadata)
            :: normalize_vector_metadata_sequence (form :: rest))
    | form :: rest ->
        normalize_metadata form :: normalize_vector_metadata_sequence rest
    | [] -> []

  let normalize_located_metadata located =
    { located with Ast.form = normalize_metadata located.Ast.form }

  let extract_compile_time_helpers located_ast =
    let rec quoted_refs refs = function
      | Ast.FList [ Ast.FSymbol ("unquote" | "unquote-splicing"); expression ]
        ->
          form_refs refs expression
      | Ast.FList forms | Ast.FVector forms ->
          List.fold_left quoted_refs refs forms
      | Ast.FMap entries ->
          List.fold_left
            (fun refs (key, value) -> quoted_refs (quoted_refs refs key) value)
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
              :: Ast.FSymbol name
              :: forms) ->
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
            if String_set.mem name refs then List.fold_left form_refs refs forms
            else refs)
          refs definitions
      in
      if String_set.equal refs expanded then refs else close expanded
    in
    let helper_names = close initial_refs in
    List.map
      (fun located ->
        match located.Ast.form with
        | Ast.FList (Ast.FSymbol ("defn" | "defn-") :: Ast.FSymbol name :: forms)
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
                let rec parse_clauses require_entries exclusions =
                  function
                  | [] ->
                      Ok
                        ( List.rev require_entries |> List.concat,
                          List.rev exclusions |> List.concat )
                  | Ast.FList
                      (Ast.FKeyword (":require" | ":require-macros") :: entries)
                    :: rest ->
                      parse_clauses
                        (entries :: require_entries)
                        exclusions rest
                  | Ast.FList
                      [
                        Ast.FKeyword ":refer-clojure";
                        Ast.FKeyword ":exclude";
                        Ast.FVector names;
                      ]
                    :: rest ->
                      parse_clauses require_entries (names :: exclusions) rest
                  | Ast.FList (Ast.FKeyword ":import" :: _) :: _ ->
                      Error.error "lg namespaces do not support :import"
                  | _ ->
                      Error.error
                          "ns supports :require, :require-macros, :refer-clojure \
                         :exclude clauses"
                in
                Result.map
                  (fun (require_entries, exclusions) ->
                    let namespace_form =
                      namespace_scope_form span namespace_name
                    in
                    let synthetic_form head entries =
                      {
                        Ast.form = Ast.FList (Ast.FSymbol head :: entries);
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
                      |> List.rev
                    in
                    (namespace_form :: clauses) @ body)
                  (parse_clauses [] [] clauses)
          | _ ->
              Error.error "ns expects a namespace symbol and optional clauses")
    | first :: rest ->
        if List.exists is_namespace rest then
          Error.error "ns may only appear once at the start of a file"
        else Ok (first :: rest)

  let split_deftype_methods located_ast =
    located_ast
    |> List.concat_map (fun located ->
           match located.Ast.form with
           | Ast.FList
               (Ast.FSymbol (("deftype" | "defrecord") as definition)
               :: name :: fields :: (_ :: _ as methods))
             when
               definition = "deftype"
               || List.exists
                    (function
                      | Ast.FSymbol "ILookup" -> true
                      | _ -> false)
                    methods
             ->
               [
                 {
                   located with
                   Ast.form =
                     Ast.FList [ Ast.FSymbol definition; name; fields ];
                 };
                 {
                   located with
                   Ast.form =
                     Ast.FList
                       (Ast.FSymbol "deftype-methods" :: name :: methods);
                 };
               ]
           | _ -> [ located ])

  let implementation_uncached ?(target = Target.default)
      ?(filename = "<string>") source =
    let line_starts = line_starts source in
    let is_compile_time_form located =
      match located.Ast.form with
      | Ast.FList
          (Ast.FSymbol ("defmacro" | "macro-helper-defn" | "macro-helper-def")
          :: _) ->
          true
      | _ -> false
    in
    let drop_clojure_compiler_directives located_ast =
      List.filter
        (fun located ->
          match located.Ast.form with
          | Ast.FList
              [
                Ast.FSymbol "set!";
                Ast.FSymbol ("*warn-on-reflection*" | "*unchecked-math*");
                _;
              ] ->
              false
          | _ -> true)
        located_ast
    in
    let same_span left right =
      left.Ast.span.start_offset = right.Ast.span.start_offset
      && left.Ast.span.end_offset = right.Ast.span.end_offset
    in
    let add_clj_compile_time_forms tokens located_ast =
      match target with
      | Target.Native -> Ok located_ast
      | Target.Melange | Target.Js_of_ocaml -> (
          match Parser.parse_located ~target:Target.Native tokens with
          | Error _ as error -> error
          | Ok native_original -> (
              match lower_namespace native_original with
              | Error _ as error -> error
              | Ok native_located -> (
                  let compile_time_forms =
                    native_located
                    |> List.map normalize_located_metadata
                    |> extract_compile_time_helpers
                    |> List.filter is_compile_time_form
                  in
                  let located_ast =
                    List.filter
                      (fun located ->
                        not
                          (List.exists
                             (fun compile_time_form ->
                               same_span compile_time_form located)
                             compile_time_forms))
                      located_ast
                  in
                  match located_ast with
                  | ({
                       Ast.form = Ast.FList [ Ast.FSymbol "namespace-scope"; _ ];
                       _;
                     } as namespace_scope)
                    :: rest ->
                      Ok ((namespace_scope :: compile_time_forms) @ rest)
                  | _ -> Ok (compile_time_forms @ located_ast))))
    in
    match Lexer.tokenize source with
    | Error _ as err -> err
    | Ok tokens -> (
        match Parser.parse_located ~target tokens with
        | Error error ->
            Error (normalize_error_location filename line_starts error)
        | Ok original_located_ast -> (
            match lower_namespace original_located_ast with
            | Error (error : Error.t) ->
                Error (normalize_error_location filename line_starts error)
            | Ok target_located_ast -> (
                match
                  add_clj_compile_time_forms tokens target_located_ast
                with
                | Error error ->
                    Error (normalize_error_location filename line_starts error)
                | Ok located_ast ->
                let located_ast =
                  located_ast
                  |> List.map normalize_located_metadata
                  |> extract_compile_time_helpers
                  |> drop_clojure_compiler_directives
                  |> split_deftype_methods
                in
                let rec form_locations acc located =
                  let location =
                    location filename line_starts located.Ast.span
                  in
                  List.fold_left form_locations
                    ((located.Ast.form, location) :: acc)
                    located.Ast.children
                in
                let ast =
                  List.map (fun located -> located.Ast.form) located_ast
                in
                let form_locations =
                  List.fold_left form_locations
                    (List.fold_left form_locations [] original_located_ast)
                    located_ast
                in
                let normalized_form_locations =
                  let candidates = Hashtbl.create 128 in
                  List.iter
                    (fun (form, location) ->
                      let locations =
                        Hashtbl.find_opt candidates form
                        |> Option.value ~default:[]
                      in
                      Hashtbl.replace candidates form (location :: locations))
                    form_locations;
                  let compare_location left right =
                    let by_start =
                      Int.compare left.Location.loc_start.Lexing.pos_cnum
                        right.Location.loc_start.Lexing.pos_cnum
                    in
                    if by_start <> 0 then by_start
                    else
                      Int.compare left.Location.loc_end.Lexing.pos_cnum
                        right.Location.loc_end.Lexing.pos_cnum
                  in
                  Hashtbl.filter_map_inplace
                    (fun _ locations ->
                      Some (List.sort_uniq compare_location locations))
                    candidates;
                  let rec bind_form bindings form =
                    let bindings =
                      match Hashtbl.find_opt candidates form with
                      | Some (location :: remaining) ->
                          Hashtbl.replace candidates form remaining;
                          (form, location) :: bindings
                      | Some [] | None -> bindings
                    in
                    match form with
                    | Ast.FList forms | Ast.FVector forms ->
                        List.fold_left bind_form bindings forms
                    | Ast.FMap entries ->
                        List.fold_left
                          (fun bindings (key, value) ->
                            bind_form (bind_form bindings key) value)
                          bindings entries
                    | Ast.FSymbol _ | Ast.FCoreSymbol _ | Ast.FKeyword _
                    | Ast.FString _ | Ast.FRegex _ | Ast.FInt _ | Ast.FFloat _
                    | Ast.FChar _ | Ast.FBool _ ->
                        bindings
                  in
                  List.fold_left bind_form [] ast
                in
                Ok
                  {
                    target;
                    source_unit =
                      "source_" ^ Digest.to_hex (Digest.string source);
                    ast;
                    locations =
                      List.map
                        (fun located ->
                          location filename line_starts located.Ast.span)
                        located_ast;
                    form_locations =
                      normalized_form_locations @ form_locations;
                    parsed_as = `Lg;
                  })))

  let parsed_sources = Hashtbl.create 64

  let implementation ?(target = Target.default) ?(filename = "<string>") source =
    let key =
      String.concat "\000"
        [ Target.to_string target; filename; Digest.to_hex (Digest.string source) ]
    in
    match Hashtbl.find_opt parsed_sources key with
    | Some parsed -> Ok parsed
    | None -> (
        match implementation_uncached ~target ~filename source with
        | Error _ as error -> error
        | Ok parsed as result ->
            Hashtbl.add parsed_sources key parsed;
            result)
end

module Ocaml_parsetree_backend = struct
  let implementation ?(previous_items = []) ?(previous_set_modules = [])
      (typed : typed_result) =
    match
      let items = List.combine typed.locations typed.items in
      let previous_set_modules =
        List.sort_uniq String.compare
          (previous_set_modules
          @ Lowering.requested_set_modules_from_located_items previous_items)
      in
      if previous_set_modules = [] then Lowering.structure_of_located_items items
      else
        Lowering.structure_of_incremental_located_items_with_modules
          ~previous_set_modules items
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

  let analyze ?compiler_env structure =
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
            let env = Option.value compiler_env ~default:(initial_env ()) in
            let typed_structure, _signature, _signature_names, _shape, env =
              Typemod.type_structure env structure
            in
            Envaux.reset_cache ();
            let env = env |> Env.keep_only_summary |> Envaux.env_of_only_summary in
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
  {
    typecheck_state = Typecheck.empty_state;
    located_items = [];
    requested_set_modules = [];
    ocaml_env = None;
  }

let cacheable_state state = { state with ocaml_env = None }

let restore_ocaml_environment ?(target = Target.default) ~packages state
    sources =
  let packages =
    match target with
    | Target.Melange -> "melange" :: packages
    | Target.Js_of_ocaml -> "re" :: "js_of_ocaml" :: packages
    | Target.Native -> "re" :: packages
  in
  match Ocaml_package.include_dirs packages with
  | Error _ as error -> error
  | Ok include_dirs ->
      Ocaml_signature.add_include_dirs include_dirs;
      let rec restore compiler_env index = function
        | [] -> Ok { state with ocaml_env = compiler_env }
        | source :: rest -> (
            try
              let lexbuf = Lexing.from_string source in
              Location.init lexbuf (Printf.sprintf "<cached:%d>" index);
              let structure = Parse.implementation lexbuf in
              match Ocaml_typechecker.analyze ?compiler_env structure with
              | Error _ as error -> error
              | Ok analysis ->
                  restore (Some analysis.compiler_env) (index + 1) rest
            with exn ->
              Error.error
                ("failed to restore cached OCaml environment: "
               ^ Ocaml_typechecker.exception_message exn))
      in
      restore None 0 sources

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
  | Ok packages -> (
      let packages =
        match target with
        | Target.Melange -> "melange" :: packages
        | Target.Js_of_ocaml -> "re" :: "js_of_ocaml" :: packages
        | Target.Native -> "re" :: packages
      in
      match Ocaml_package.include_dirs packages with
      | Error _ as err -> err
      | Ok include_dirs ->
          Ocaml_signature.add_include_dirs include_dirs;
          Ok packages)

let checked_parsetree (typed : typed_result) =
  match Ocaml_parsetree_backend.implementation typed with
  | Error _ as err -> err
  | Ok result -> (
      if Sys.getenv_opt "LG_DUMP_ML" = Some "1" then
        Printf.eprintf "%s\n%!"
          (Ocaml_parsetree.print_implementation result.structure);
      match Ocaml_typechecker.structure result.structure with
      | Error _ as err -> err
      | Ok diagnostics -> Ok (result, diagnostics))

let stabilize_dependencies (parsed : parser_result) =
  let order = Dependency_graph.stable_order parsed.ast in
  {
    parsed with
    ast = List.map (List.nth parsed.ast) order;
    locations = List.map (List.nth parsed.locations) order;
  }

let declaration_bindings ast env =
  let module Declared_names = Set.Make (String) in
  let rec declared_names declared = function
    | [] -> List.rev declared
    | Ast.FList (Ast.FSymbol "declare" :: form_names) :: rest ->
        let declared =
          List.fold_left
            (fun declared -> function
              | Ast.FSymbol name -> name :: declared | _ -> declared)
            declared form_names
        in
        declared_names declared rest
    | Ast.FList
        (Ast.FSymbol "declare+" :: Ast.FSymbol name :: _signature)
      :: rest ->
        declared_names (name :: declared) rest
    | Ast.FList
        [
          Ast.FSymbol "defn-signature";
          Ast.FList
            (Ast.FSymbol ("defn" | "defn-") :: Ast.FSymbol name :: _);
        ]
      :: rest ->
        declared_names (name :: declared) rest
    | _ :: rest -> declared_names declared rest
  in
  let declared = declared_names [] ast |> Declared_names.of_list in
  let rec is_declared_key key offset =
    let candidate =
      if offset = 0 then key
      else String.sub key offset (String.length key - offset)
    in
    Declared_names.mem candidate declared
    ||
    match String.index_from_opt key offset '/' with
    | None -> false
    | Some separator -> is_declared_key key (separator + 1)
  in
  Compiler_environment.filter_map
    (fun key binding ->
      if is_declared_key key 0 then Some (key, binding) else None)
    env
  |> List.map (fun (key, (binding : Types.binding)) ->
         if binding.forward_declared then (key, binding)
         else (key, { binding with forward_declared = true }))
  |> List.sort_uniq (fun (left, _) (right, _) -> String.compare left right)

let stabilization_ast ?(signed_names = []) ast =
  let module Signed_names = Set.Make (String) in
  let module Required_names = Set.Make (String) in
  let signed_names = Signed_names.of_list signed_names in
  let scope =
    ast
    |> List.find_map (function
         | Ast.FList
             [ Ast.FSymbol "namespace-scope"; Ast.FSymbol namespace ] ->
             Some namespace
         | _ -> None)
    |> Option.value ~default:""
  in
  let explicitly_signed_definition = function
    | Ast.FList
        (Ast.FSymbol ("def" | "defonce" | "defn" | "defn-")
        :: Ast.FSymbol name :: _) ->
        Signed_names.mem name signed_names
        || (scope <> ""
           && Signed_names.mem (Names.scoped_key scope name) signed_names)
    | _ -> false
  in
  let ordinary_definition = function
    | Ast.FList
        (Ast.FSymbol ("def" | "defonce" | "defn" | "defn-")
        :: Ast.FSymbol _ :: _) ->
        true
    | _ -> false
  in
  let add_name_variants names name =
    let names = Required_names.add name names in
    match String.rindex_opt name '/' with
    | None -> names
    | Some separator ->
        Required_names.add
          (String.sub name (separator + 1)
             (String.length name - separator - 1))
          names
  in
  let intersects names candidates =
    List.exists
      (fun candidate ->
        let variants = add_name_variants Required_names.empty candidate in
        not
          (Required_names.is_empty (Required_names.inter names variants)))
      candidates
  in
  let recursive_groups = Dependency_graph.recursive_groups ast in
  let recursive_indices = List.concat recursive_groups in
  let recursive_group_by_index = Array.make (List.length ast) None in
  List.iter
    (fun group ->
      List.iter
        (fun index -> recursive_group_by_index.(index) <- Some group)
        group)
    recursive_groups;
  let declared_names =
    let explicit =
      ast
      |> List.concat_map (function
           | Ast.FList (Ast.FSymbol ("declare" | "declare+") :: _)
           | Ast.FList [ Ast.FSymbol "defn-signature"; _ ] as form ->
               Dependency_graph.provided_names form
           | _ -> [])
    in
    let recursive =
      recursive_indices
      |> List.concat_map (fun index ->
             Dependency_graph.provided_names (List.nth ast index))
    in
    List.sort_uniq String.compare (explicit @ recursive)
  in
  if declared_names = [] then ast
  else
    let indexed = List.mapi (fun index form -> (index, form)) ast in
    let selected = Array.make (List.length ast) false in
    let rec close required_names =
      let required_names, changed =
        List.fold_left
          (fun (required_names, changed) (index, form) ->
            let provided = Dependency_graph.provided_names form in
            if
              ordinary_definition form
              && not selected.(index)
              && intersects required_names provided
            then
              let () = selected.(index) <- true in
              let required_names =
                if
                  explicitly_signed_definition form
                  || Option.is_some recursive_group_by_index.(index)
                then required_names
                else
                  List.fold_left add_name_variants required_names
                    (Dependency_graph.dependency_symbols form)
              in
              (required_names, true)
            else (required_names, changed))
          (required_names, false) indexed
      in
      if changed then close required_names else ()
    in
    let required_names =
      List.fold_left add_name_variants Required_names.empty declared_names
    in
    close required_names;
    List.mapi
      (fun index form ->
        match recursive_group_by_index.(index) with
        | Some (first :: _ as indices) when index = first ->
            let names =
              indices
              |> List.concat_map (fun member ->
                     List.nth ast member |> Dependency_graph.provided_names)
              |> List.sort_uniq String.compare
              |> List.map (fun name -> Ast.FSymbol name)
            in
            Ast.FList (Ast.FSymbol "declare" :: names)
        | Some (_ :: _) -> Ast.FList [ Ast.FSymbol "declare" ]
        | Some [] -> assert false
        | None when ordinary_definition form ->
            if not selected.(index) then
              Ast.FList [ Ast.FSymbol "declare" ]
            else if explicitly_signed_definition form then
              match Dependency_graph.provided_names form with
              | name :: _ ->
                  Ast.FList [ Ast.FSymbol "declare"; Ast.FSymbol name ]
              | [] -> form
            else form
        | None -> form)
      ast

let recursive_definition_ast ast =
  let definition_forms = function
    | Ast.FList
        (Ast.FSymbol ("defn" | "defn-") :: Ast.FSymbol _
        :: Ast.FString _docstring :: forms) ->
        Some forms
    | Ast.FList
        (Ast.FSymbol ("defn" | "defn-") :: Ast.FSymbol _ :: forms) ->
        Some forms
    | _ -> None
  in
  let plain_definition form =
    match definition_forms form with
    | Some (Ast.FVector _ :: _) -> true
    | Some (Ast.FList (Ast.FVector _ :: _) :: clauses) ->
        List.for_all
          (function Ast.FList (Ast.FVector _ :: _) -> true | _ -> false)
          clauses
    | Some _ | None -> false
  in
  let multi_arity_definition = function
    | form -> (
        match definition_forms form with
        | Some (Ast.FList (Ast.FVector _ :: _) :: _) -> true
        | Some _ | None -> false)
  in
  let recursive_groups =
    Dependency_graph.recursive_groups ast
    |> List.filter_map (fun indices ->
           if
             List.length indices >= 2
             && List.for_all
                  (fun index -> plain_definition (List.nth ast index))
                  indices
           then
             Some
               (List.stable_sort
                  (fun left right ->
                    Bool.compare
                      (multi_arity_definition (List.nth ast left))
                      (multi_arity_definition (List.nth ast right)))
                  indices)
           else None)
  in
  let normalize_definition = function
    | Ast.FList
        (Ast.FSymbol (("defn" | "defn-") as definition)
        :: (Ast.FSymbol _ as name) :: Ast.FString _docstring :: forms) ->
        Ast.FList (Ast.FSymbol definition :: name :: forms)
    | form -> form
  in
  List.mapi
    (fun index form ->
      match List.find_opt (List.exists (( = ) index)) recursive_groups with
      | None -> form
      | Some (first :: _ as indices) when index = first ->
          Ast.FList
            (Ast.FSymbol "recursive-definition-group"
            :: List.map
                 (fun member -> normalize_definition (List.nth ast member))
                 indices)
      | Some (_ :: _) -> Ast.FList [ Ast.FSymbol "declare" ]
      | Some [] -> assert false)
    ast

let affected_stabilization_forms ast evidence_ast changed_names =
  let module Names = Set.Make (String) in
  let add_name_variants names name =
    let names = Names.add name names in
    match String.rindex_opt name '/' with
    | None -> names
    | Some separator ->
        Names.add
          (String.sub name (separator + 1)
             (String.length name - separator - 1))
          names
  in
  let intersects names candidates =
    List.exists
      (fun candidate ->
        not (Names.is_empty (Names.inter names (add_name_variants Names.empty candidate))))
      candidates
  in
  let affected_names =
    List.fold_left add_name_variants Names.empty changed_names
  in
  let indexed =
    List.map2
      (fun original evidence ->
        ( original,
          evidence,
          List.sort_uniq String.compare
            (Dependency_graph.provided_names original
            @ Dependency_graph.provided_names evidence),
          List.sort_uniq String.compare
            (Dependency_graph.dependency_symbols original
            @ Dependency_graph.dependency_symbols evidence) ))
      ast evidence_ast
  in
  let can_recompile = function
    | Ast.FList
        (Ast.FSymbol
          ( "def" | "defonce" | "defn" | "defn-" | "defmacro"
          | "recursive-definition-group" )
        :: _) ->
        true
    | _ -> false
  in
  indexed
  |> List.filter_map (fun (original, evidence, provided, dependencies) ->
         if
           provided <> [] && can_recompile original
           && (intersects affected_names provided
              || intersects affected_names dependencies)
         then Some evidence
         else None)

let stabilize_typecheck ?compile_evidence ?compile_evidence_subset ~compile
    ~(initial_state : Compiler_state.t) ast =
  let report_timings = Sys.getenv_opt "LG_COMPILE_TIMINGS" = Some "1" in
  let compile_pass compiler pass state =
    let started_at = if report_timings then Sys.time () else 0.0 in
    let result = compiler state in
    if report_timings then
      Printf.eprintf "lg: typecheck stabilization pass %d: %.3fs\n%!" pass
        (Sys.time () -. started_at);
    result
  in
  let binding_abi_equal (left : Types.binding) (right : Types.binding) =
    (match (left.scheme, right.scheme) with
    | Some left, Some right ->
        Types.source_name (Type_solver.canonical_scheme_body left)
        = Types.source_name (Type_solver.canonical_scheme_body right)
    | None, None -> Types.source_name left.ty = Types.source_name right.ty
    | Some _, None | None, Some _ -> false)
    && left.row_param_types = right.row_param_types
    && left.overload_row_param_types = right.overload_row_param_types
    && left.overload_targets = right.overload_targets
    && left.return_param_index = right.return_param_index
  in
  let declarations_abi_equal left right =
    List.length left = List.length right
    &&
    List.for_all2
      (fun (left_name, left_binding) (right_name, right_binding) ->
        left_name = right_name
        && binding_abi_equal left_binding right_binding)
      left right
  in
  let changed_declaration_names previous next =
    next
    |> List.filter_map (fun (name, binding) ->
           match List.assoc_opt name previous with
           | Some previous_binding
             when binding_abi_equal previous_binding binding ->
               None
           | Some _ | None -> Some name)
  in
  let report_changed_declarations previous next =
    if report_timings then
      let previous_by_name name =
        List.find_opt (fun (candidate, _) -> String.equal name candidate) previous
        |> Option.map snd
      in
      next
      |> List.filter_map (fun (name, binding) ->
             match previous_by_name name with
             | Some previous_binding
               when binding_abi_equal previous_binding binding ->
                 None
             | None -> Some name
             | Some previous_binding ->
                 let changes =
                   []
                   |> (fun changes ->
                        if
                          String.equal (Types.source_name previous_binding.ty)
                            (Types.source_name binding.ty)
                        then changes
                        else "type" :: changes)
                   |> (fun changes ->
                        if previous_binding.row_param_types = binding.row_param_types
                        then changes
                        else "rows" :: changes)
                   |> (fun changes ->
                        if
                          previous_binding.overload_row_param_types
                          = binding.overload_row_param_types
                        then changes
                        else "overload-rows" :: changes)
                   |> (fun changes ->
                        if previous_binding.overload_targets = binding.overload_targets
                        then changes
                        else "overloads" :: changes)
                   |> (fun changes ->
                        if
                          previous_binding.return_param_index
                          = binding.return_param_index
                        then changes
                        else "return-param" :: changes)
                 in
                 Some (name ^ "[" ^ String.concat "," (List.rev changes) ^ "]"))
      |> function
      | [] -> ()
      | names ->
          Printf.eprintf "lg: changed declaration ABI: %s\n%!"
            (String.concat ", " names)
  in
  let evidence_compile = Option.value compile_evidence ~default:compile in
  let initial_compile state =
    match compile_evidence with
    | None -> compile state
    | Some compile_evidence -> (
        match compile_evidence state with
        | Ok _ as result -> result
        | Error _ -> compile state)
  in
  let seeded_state declarations protocol_evidence =
    {
      initial_state with
      env =
        initial_state.env
        |> Compiler_environment.add_bindings declarations
        |> Compiler_environment.with_protocol_evidence
             (Some protocol_evidence);
    }
  in
  let rec continue_full remaining pass declarations protocol_evidence =
    if remaining = 0 then
      Error.error "type evidence did not stabilize after 16 passes"
    else
      match
        compile_pass compile pass
          (seeded_state declarations protocol_evidence)
      with
      | Error _ as error -> error
      | Ok ((next_state : Compiler_state.t), items) ->
          let next_declarations = declaration_bindings ast next_state.env in
          let next_protocols =
            Compiler_environment.protocols next_state.env
          in
          if declarations_abi_equal next_declarations declarations then
            Ok (next_state, items)
          else (
            report_changed_declarations declarations next_declarations;
            continue_full (remaining - 1) (pass + 1) next_declarations
              next_protocols
          )
  and finish remaining pass declarations protocol_evidence evidence_result =
    match compile_evidence with
    | None -> Ok evidence_result
    | Some _ ->
        if remaining = 0 then
          Error.error "type evidence did not stabilize after 16 passes"
        else
          match
            compile_pass compile pass
              (seeded_state declarations protocol_evidence)
          with
          | Error _ as error -> error
          | Ok ((next_state : Compiler_state.t), items) ->
              let next_declarations =
                declaration_bindings ast next_state.env
              in
              let next_protocols =
                Compiler_environment.protocols next_state.env
              in
              if declarations_abi_equal next_declarations declarations then
                Ok (next_state, items)
              else (
                report_changed_declarations declarations next_declarations;
                continue_full (remaining - 1) (pass + 1)
                  next_declarations next_protocols
              )
  in
  let rec continue remaining pass declarations protocol_evidence
      (evidence_base : Compiler_state.t) changed_names =
    if remaining = 0 then
      Error.error "type evidence did not stabilize after 16 passes"
    else
      let full_state = seeded_state declarations protocol_evidence in
      let subset_state =
        {
          evidence_base with
          items = initial_state.items;
          env =
            evidence_base.env
            |> Compiler_environment.add_bindings declarations
            |> Compiler_environment.with_protocol_evidence
                 (Some protocol_evidence);
        }
      in
      let compiler _state =
        match compile_evidence_subset with
        | None -> evidence_compile full_state
        | Some compile_subset -> (
            match compile_subset changed_names subset_state with
            | Ok _ as result -> result
            | Error error ->
                if report_timings then
                  Printf.eprintf "lg: stabilization subset fallback: %s\n%!"
                    error.Error.message;
                evidence_compile full_state)
      in
      match
        compile_pass compiler pass full_state
      with
      | Error _ as error -> error
      | Ok ((next_state : Compiler_state.t), items) ->
          let next_declarations = declaration_bindings ast next_state.env in
          let next_protocols =
            Compiler_environment.protocols next_state.env
          in
          if declarations_abi_equal next_declarations declarations then
            finish (remaining - 1) (pass + 1) next_declarations
              next_protocols (next_state, items)
          else (
            report_changed_declarations declarations next_declarations;
            let changed_names =
              changed_declaration_names declarations next_declarations
            in
            continue (remaining - 1) (pass + 1) next_declarations
              next_protocols evidence_base changed_names
          )
  in
  match compile_pass initial_compile 1 initial_state with
  | Error _ as error -> error
  | Ok (((first_state : Compiler_state.t), _) as first_result) ->
      let initial_declarations =
        declaration_bindings ast initial_state.env
      in
      let first_declarations = declaration_bindings ast first_state.env in
      let first_protocols = Compiler_environment.protocols first_state.env in
      if declarations_abi_equal first_declarations initial_declarations then
        finish 15 2 first_declarations first_protocols first_result
      else (
        report_changed_declarations initial_declarations first_declarations;
        continue 15 2 first_declarations first_protocols first_state
          (changed_declaration_names initial_declarations first_declarations))

let typecheck (parsed : parser_result) =
  let parsed = stabilize_dependencies parsed in
  match prepare_packages parsed.target parsed.ast with
  | Error _ as err -> err
  | Ok _ -> (
      let initial_state =
        Compiler_state.with_target parsed.target Typecheck.empty_state
      in
      let compilation_ast = recursive_definition_ast parsed.ast in
      let compile state =
        Source_context.with_source_unit parsed.source_unit (fun () ->
            Source_context.with_locations parsed.form_locations (fun () ->
                Typecheck.compile_forms_incremental state compilation_ast))
      in
      let signed_names =
        initial_state.env |> Compiler_environment.signatures
        |> Signature_overlay.value_names
      in
      let evidence_ast = stabilization_ast ~signed_names parsed.ast in
      let compile_evidence =
        if List.for_all2 ( == ) evidence_ast parsed.ast then None
        else
          Some
            (fun state ->
              Source_context.with_source_unit parsed.source_unit (fun () ->
                  Source_context.with_locations parsed.form_locations (fun () ->
                  Typecheck.compile_forms_incremental state evidence_ast)))
      in
      let compile_evidence_subset =
        Option.map
          (fun _ changed_names state ->
            let forms =
              affected_stabilization_forms parsed.ast evidence_ast
                changed_names
            in
            if Sys.getenv_opt "LG_COMPILE_TIMINGS" = Some "1" then
              Printf.eprintf "lg: stabilization subset: %d forms\n%!"
                (List.length forms);
            Source_context.with_source_unit parsed.source_unit (fun () ->
                Source_context.with_locations parsed.form_locations (fun () ->
                    Typecheck.compile_forms_incremental state forms)))
          compile_evidence
      in
      match
        stabilize_typecheck ?compile_evidence ?compile_evidence_subset ~compile
          ~initial_state parsed.ast
      with
      | Error _ as err -> err
      | Ok (typecheck_state, items) ->
          Ok
            {
              ast = parsed.ast;
              items;
              locations = parsed.locations;
              typecheck_state;
            } )

let typecheck_incremental state (parsed : parser_result) =
  let parsed = stabilize_dependencies parsed in
  match prepare_packages parsed.target parsed.ast with
  | Error _ as err -> err
  | Ok _ -> (
      let initial_state =
        if state.located_items = [] then
          Compiler_state.with_target parsed.target state.typecheck_state
        else state.typecheck_state
      in
      let initial_state =
        {
          initial_state with
          env =
            Compiler_environment.with_protocol_evidence None initial_state.env;
        }
      in
      let compilation_ast = recursive_definition_ast parsed.ast in
      let compile typecheck_state =
        Source_context.with_source_unit parsed.source_unit (fun () ->
            Source_context.with_locations parsed.form_locations (fun () ->
                Typecheck.compile_forms_incremental typecheck_state
                  compilation_ast))
      in
      let signed_names =
        initial_state.env |> Compiler_environment.signatures
        |> Signature_overlay.value_names
      in
      let evidence_ast = stabilization_ast ~signed_names parsed.ast in
      let compile_evidence =
        if List.for_all2 ( == ) evidence_ast parsed.ast then None
        else
          Some
            (fun typecheck_state ->
              Source_context.with_source_unit parsed.source_unit (fun () ->
                  Source_context.with_locations parsed.form_locations (fun () ->
                      Typecheck.compile_forms_incremental typecheck_state
                        evidence_ast)))
      in
      let compile_evidence_subset =
        Option.map
          (fun _ changed_names typecheck_state ->
            let forms =
              affected_stabilization_forms parsed.ast evidence_ast
                changed_names
            in
            if Sys.getenv_opt "LG_COMPILE_TIMINGS" = Some "1" then
              Printf.eprintf "lg: stabilization subset: %d forms\n%!"
                (List.length forms);
            Source_context.with_source_unit parsed.source_unit (fun () ->
                Source_context.with_locations parsed.form_locations (fun () ->
                    Typecheck.compile_forms_incremental typecheck_state forms)))
          compile_evidence
      in
      match
        stabilize_typecheck ?compile_evidence ?compile_evidence_subset ~compile
          ~initial_state parsed.ast
      with
      | Error _ as err -> err
      | Ok (typecheck_state, items) ->
          let located_items =
            state.located_items @ List.combine parsed.locations items
          in
          let state =
            { state with typecheck_state; located_items }
          in
          Ok
            ( state,
              {
                ast = parsed.ast;
                items;
                locations = parsed.locations;
                typecheck_state;
              } ))

let required_ocaml_packages ?(target = Target.default) ?(filename = "<string>")
    source =
  match Lg_frontend.implementation ~target ~filename source with
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
              if Sys.getenv_opt "LG_DUMP_ML" = Some "1" then
                Printf.eprintf "%s\n%!"
                  (Ocaml_parsetree.print_implementation parsetree.structure);
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
              if Sys.getenv_opt "LG_DUMP_ML" = Some "1" then
                Printf.eprintf "%s\n%!"
                  (Ocaml_parsetree.print_implementation structure);
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
    ?(filename = "<string>") ?(check_ocaml = true) state source =
  let previous_set_modules = state.requested_set_modules in
  match Lg_frontend.implementation ~target ~filename source with
  | Error _ as err -> err
  | Ok parsed ->
      (match typecheck_incremental state parsed with
      | Error _ as err -> err
      | Ok (state, typed) ->
          (match
             Ocaml_parsetree_backend.implementation ~previous_set_modules typed
           with
          | Error _ as err -> err
          | Ok result ->
              let current_items = List.combine typed.locations typed.items in
              let requested_set_modules =
                List.sort_uniq String.compare
                  (previous_set_modules
                  @ Lowering.requested_set_modules_from_located_items
                      current_items)
              in
              let state =
                { state with located_items = []; requested_set_modules }
              in
              let ocaml_source =
                Ocaml_parsetree_backend.print result.structure
              in
              if Sys.getenv_opt "LG_DUMP_ML" = Some "1" then
                Printf.eprintf "%s\n%!" ocaml_source;
              if not check_ocaml then
                Ok (state, { ocaml_source; diagnostics = [] })
              else
                match
                  Ocaml_typechecker.analyze ?compiler_env:state.ocaml_env
                    result.structure
                with
                | Error _ as err -> err
                | Ok analysis ->
                    let state =
                      { state with ocaml_env = Some analysis.compiler_env }
                    in
                    Ok
                      ( state,
                        {
                          ocaml_source;
                          diagnostics = analysis.diagnostics;
                        } )))

let compile_chunk ?(target = Target.default) ?(filename = "<string>") state
    source =
  match compile_chunk_with_diagnostics ~target ~filename state source with
  | Error _ as err -> err
  | Ok (state, compilation) -> Ok (state, compilation.ocaml_source)

let compile_chunk_parsetree ?(target = Target.default) ?(filename = "<string>")
    state source =
  let previous_items = state.located_items in
  match Lg_frontend.implementation ~target ~filename source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck_incremental state parsed with
      | Error _ as err -> err
      | Ok (state, typed) -> (
          match
            Ocaml_parsetree_backend.implementation ~previous_items typed
          with
          | Error _ as err -> err
          | Ok result -> (
              if Sys.getenv_opt "LG_DUMP_ML" = Some "1" then
                Printf.eprintf "%s\n%!"
                  (Ocaml_parsetree_backend.print result.structure);
              match
                Ocaml_typechecker.analyze ?compiler_env:state.ocaml_env
                  result.structure
              with
              | Error _ as err -> err
              | Ok analysis ->
                  let state =
                    { state with ocaml_env = Some analysis.compiler_env }
                  in
                  Ok (state, result.structure))))
