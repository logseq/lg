type hover = {
  contents : string;
  range : Ast.source_span;
}

type completion_item = {
  label : string;
  detail : string;
}

type text_edit = {
  range : Ast.source_span;
  new_text : string;
}

type symbol_kind = [ `Module | `Function | `Variable | `Type | `Interface ]

type document_symbol = {
  name : string;
  detail : string option;
  kind : symbol_kind;
  range : Ast.source_span;
  selection_range : Ast.source_span;
  children : document_symbol list;
}

type t = {
  source : string;
  tokens : Ast.token list;
  forms : Ast.located_form list;
  compiler : Toolchain.language_analysis;
}

let analyze ~filename source =
  match Lexer.tokenize source with
  | Error _ as err -> err
  | Ok tokens -> (
      match Parser.parse_located tokens with
      | Error _ as err -> err
      | Ok forms -> (
          match Toolchain.analyze ~filename source with
          | Error _ as err -> err
          | Ok compiler -> Ok { source; tokens; forms; compiler }))

let analyze_workspace sources =
  let rec parse acc = function
    | [] -> Ok (List.rev acc)
    | (filename, source) :: rest -> (
        match Lexer.tokenize source with
        | Error _ -> parse acc rest
        | Ok tokens -> (
            match Parser.parse_located tokens with
            | Error _ -> parse acc rest
            | Ok forms -> parse ((filename, source, tokens, forms) :: acc) rest))
  in
  match parse [] sources with
  | Error _ as err -> err
  | Ok parsed -> (
      match Toolchain.analyze_workspace sources with
      | Error _ as err -> err
      | Ok analyses ->
          let compiler filename = List.assoc_opt filename analyses in
          Ok
            (List.filter_map
               (fun (filename, source, tokens, forms) ->
                 compiler filename
                 |> Option.map (fun compiler ->
                        (filename, { source; tokens; forms; compiler })))
               parsed))

let diagnostics analysis = analysis.compiler.diagnostics

let token_at analysis offset =
  List.find_opt
    (fun (token : Ast.token) ->
      token.span.start_offset <= offset && offset < token.span.end_offset)
    analysis.tokens

let symbol_span_at analysis offset =
  match token_at analysis offset with
  | Some { desc = Symbol _; span } -> Some span
  | _ -> None

let location_contains_offset location offset =
  (not location.Location.loc_ghost)
  && location.loc_start.Lexing.pos_cnum <= offset
  && offset < location.loc_end.Lexing.pos_cnum

let location_size location =
  location.Location.loc_end.Lexing.pos_cnum
  - location.loc_start.Lexing.pos_cnum

let smallest_expression typed_structure offset predicate =
  let best = ref None in
  let consider expression =
    if location_contains_offset expression.Typedtree.exp_loc offset && predicate expression
    then
      match !best with
      | None -> best := Some expression
      | Some current
        when location_size expression.exp_loc < location_size current.exp_loc ->
          best := Some expression
      | Some _ -> ()
  in
  let base = Tast_iterator.default_iterator in
  let iterator =
    {
      base with
      expr =
        (fun self expression ->
          consider expression;
          base.expr self expression);
    }
  in
  iterator.structure iterator typed_structure;
  !best

let source_node_id_of_attributes attributes =
  attributes
  |> List.find_map
       (fun ({ Parsetree.attr_name = { txt; _ }; attr_payload; _ } : Parsetree.attribute) ->
         if txt <> "cljml.node_id" then None
         else
           match attr_payload with
           | PStr
               [ { pstr_desc =
                     Pstr_eval
                       ( { pexp_desc =
                             Pexp_constant
                               { pconst_desc = Pconst_string (id, _, _); _ };
                           _ },
                         _ );
                   _ } ] ->
               Some id
           | _ -> None)

let source_node_id_at analysis ~offset =
  match
    smallest_expression analysis.compiler.typed_structure offset (fun expression ->
        Option.is_some (source_node_id_of_attributes expression.exp_attributes))
  with
  | None -> None
  | Some expression -> source_node_id_of_attributes expression.exp_attributes

let print_type env ty =
  Printtyp.wrap_printing_env ~error:false env (fun () ->
      Format.asprintf "%a" Printtyp.type_scheme ty)

let source_symbol_basename name =
  match String.rindex_opt name '/' with
  | None -> name
  | Some index -> String.sub name (index + 1) (String.length name - index - 1)

let identifier_name_matches source_name path =
  let expected = source_symbol_basename source_name |> Names.sanitize_name in
  let actual = Path.name path |> Names.sanitize_name in
  actual = expected || String.ends_with ~suffix:("_" ^ expected) actual

let hover analysis ~offset =
  match symbol_span_at analysis offset with
  | None -> None
  | Some range ->
      let identifier expression =
        match expression.Typedtree.exp_desc with
        | Typedtree.Texp_ident _ -> true
        | _ -> false
      in
      let expression =
        match
          smallest_expression analysis.compiler.typed_structure offset identifier
        with
        | Some _ as expression -> expression
        | None ->
            smallest_expression analysis.compiler.typed_structure offset (fun _ -> true)
      in
      expression
      |> Option.map (fun (expression : Typedtree.expression) ->
             {
               contents = print_type expression.exp_env expression.exp_type;
               range;
             })

let definition analysis ~offset =
  match token_at analysis offset with
  | None -> None
  | Some { desc = Symbol source_name; _ } -> (
      match
        smallest_expression analysis.compiler.typed_structure offset (fun expression ->
            match expression.exp_desc with
            | Typedtree.Texp_ident (path, _, _) ->
                identifier_name_matches source_name path
            | _ -> false)
      with
      | Some { exp_desc = Typedtree.Texp_ident (_, _, description); _ }
        when not description.val_loc.Location.loc_ghost ->
          Some description.val_loc
      | _ -> None)
  | Some _ -> None

type value_identity = {
  uid : Typedtree.Uid.t;
  definition_location : Location.t;
}

let identifier_identity_at analysis offset source_name =
  smallest_expression analysis.compiler.typed_structure offset (fun expression ->
      match expression.Typedtree.exp_desc with
      | Typedtree.Texp_ident (path, _, _) ->
          identifier_name_matches source_name path
      | _ -> false)
  |> Option.map (fun (expression : Typedtree.expression) ->
         match expression.exp_desc with
         | Typedtree.Texp_ident (_, _, description) ->
             {
               uid = description.val_uid;
               definition_location = description.val_loc;
             }
         | _ -> assert false)

let binding_identity_at analysis offset source_name =
  let best = ref None in
  let consider location name uid =
    if
      location_contains_offset location offset
      && Names.sanitize_name source_name = Names.sanitize_name name
    then
      let size = location_size location in
      match !best with
      | None -> best := Some (size, uid, location)
      | Some (current_size, _, _) when size < current_size ->
          best := Some (size, uid, location)
      | Some _ -> ()
  in
  let base = Tast_iterator.default_iterator in
  let iterator =
    {
      base with
      pat =
        (fun (type kind) self
             (pattern : kind Typedtree.general_pattern) ->
          (match pattern.pat_desc with
          | Typedtree.Tpat_var (_, name, uid) ->
              consider pattern.pat_loc name.txt uid
          | Typedtree.Tpat_alias (_, _, name, uid, _) ->
              consider pattern.pat_loc name.txt uid
          | _ -> ());
          base.pat self pattern);
    }
  in
  iterator.structure iterator analysis.compiler.typed_structure;
  match !best with
  | Some (_, uid, definition_location) -> Some { uid; definition_location }
  | None -> None

let value_identity_at analysis offset =
  match token_at analysis offset with
  | Some { desc = Symbol source_name; _ } -> (
      match identifier_identity_at analysis offset source_name with
      | Some _ as identity -> identity
      | None -> binding_identity_at analysis offset source_name)
  | _ -> None

let compare_span (left : Ast.source_span) (right : Ast.source_span) =
  Int.compare left.start_offset right.start_offset

let references analysis ~offset =
  match value_identity_at analysis offset with
  | None -> []
  | Some target ->
      analysis.tokens
      |> List.filter_map (fun (token : Ast.token) ->
             match token.desc with
             | Symbol _ -> (
                 match value_identity_at analysis token.span.start_offset with
                 | Some identity when Typedtree.Uid.equal identity.uid target.uid ->
                     Some token.span
                 | _ -> None)
             | _ -> None)
      |> List.sort_uniq compare_span

let value_uid_at analysis ~offset =
  value_identity_at analysis offset |> Option.map (fun identity -> identity.uid)

let references_to_uid analysis uid =
  analysis.tokens
  |> List.filter_map (fun (token : Ast.token) ->
         match token.desc with
         | Symbol _ -> (
             match value_identity_at analysis token.span.start_offset with
             | Some identity when Typedtree.Uid.equal identity.uid uid ->
                 Some token.span
             | _ -> None)
         | _ -> None)
  |> List.sort_uniq compare_span

let valid_rename_name name =
  match Lexer.tokenize name with
  | Ok [ { desc = Symbol parsed; span } ] ->
      parsed = name && span.start_offset = 0 && span.end_offset = String.length name
  | _ -> false

let rename analysis ~offset ~new_name =
  if not (valid_rename_name new_name) then Error.error "invalid rename target"
  else
    match references analysis ~offset with
    | [] -> Error.error "symbol cannot be renamed"
    | ranges -> Ok (List.map (fun range -> { range; new_text = new_name }) ranges)

let prepare_rename analysis ~offset =
  match (symbol_span_at analysis offset, value_identity_at analysis offset) with
  | Some range, Some _ -> Some range
  | _ -> None

let symbol_kind = function
  | "defn" -> Some `Function
  | "def" -> Some `Variable
  | "module" | "module-alias" | "module-apply" | "module-functor" ->
      Some `Module
  | "module-signature" -> Some `Interface
  | "type-alias" | "type-record" | "type-variant" | "defprotocol" ->
      Some `Type
  | _ -> None

let rec symbols_of_form (located : Ast.located_form) =
  match located.children with
  | { form = FSymbol head; _ } :: ({ form = FSymbol name; span = selection_range; _ } as _name)
    :: rest -> (
      match symbol_kind head with
      | None -> []
      | Some kind ->
          let children =
            if head = "module" then List.concat_map symbols_of_form rest else []
          in
          [
            {
              name;
              detail = None;
              kind;
              range = located.span;
              selection_range;
              children;
            };
          ])
  | _ -> []

let document_symbols analysis = List.concat_map symbols_of_form analysis.forms

let completion_source_names analysis =
  analysis.compiler.typecheck_state.env
  |> Compiler_environment.filter_map (fun key (binding : Types.binding) ->
         if String.starts_with ~prefix:"__" key then None
         else Some (binding.ocaml_name, key))

let completions analysis ~offset =
  let env =
    match
      smallest_expression analysis.compiler.typed_structure offset (fun _ -> true)
    with
    | Some expression -> expression.exp_env
    | None -> analysis.compiler.compiler_env
  in
  let source_names = completion_source_names analysis in
  let source_label name =
    source_names |> List.assoc_opt name |> Option.value ~default:name
  in
  Env.fold_values
    (fun name _path description items ->
      if String.starts_with ~prefix:"__" name then items
      else
        {
          label = source_label name;
          detail = print_type env description.val_type;
        }
        :: items)
    None env []
  |> List.sort_uniq (fun left right -> String.compare left.label right.label)

module String_map = Map.Make (String)
module String_set = Set.Make (String)

type workspace_index = {
  sources : string String_map.t;
  analyses : t String_map.t;
  errors : Error.t String_map.t;
  components : String_set.t list;
}

let rec form_symbols acc form =
  let open Ast in
  match form with
  | FSymbol name -> String_set.add name acc
  | FList forms | FVector forms ->
      List.fold_left form_symbols acc forms
  | FMap entries ->
      List.fold_left
        (fun acc (key, value) -> form_symbols (form_symbols acc key) value)
        acc entries
  | FBool _ | FInt _ | FFloat _ | FChar _ | FString _ | FKeyword _ -> acc

let provided_names source =
  let open Ast in
  match Lexer.tokenize source with
  | Error _ -> String_set.empty
  | Ok tokens -> (
      match Parser.parse tokens with
      | Error _ -> String_set.empty
      | Ok forms ->
          List.fold_left
            (fun names -> function
              | FList
                  (FSymbol ("module-alias" | "module-apply")
                  :: FSymbol name :: _) ->
                  String_set.add name names
              | FList
                  (FSymbol "type-variant" :: FSymbol name :: constructors) ->
                  List.fold_left
                    (fun names -> function
                      | FSymbol constructor
                      | FList (FSymbol constructor :: _) ->
                          String_set.add constructor names
                      | _ -> names)
                    (String_set.add name names) constructors
              | FList
                  (FSymbol
                    ( "module" | "module-signature" | "module-functor" )
                  :: FSymbol name :: _) ->
                  String_set.add name names
              | FList
                  (FSymbol
                    ( "def" | "defn" | "type-alias" | "type-record" )
                  :: FSymbol name :: _) ->
                  String_set.add name names
              | FList (FSymbol "defprotocol" :: FSymbol protocol_name :: methods) ->
                  List.fold_left
                    (fun names -> function
                      | FList (FSymbol method_name :: _) ->
                          String_set.add method_name names
                      | _ -> names)
                    (String_set.add protocol_name names) methods
              | _ -> names)
            String_set.empty forms)

let referenced_names source =
  match Lexer.tokenize source with
  | Error _ -> String_set.empty
  | Ok tokens -> (
      match Parser.parse tokens with
      | Error _ -> String_set.empty
      | Ok forms -> List.fold_left form_symbols String_set.empty forms)

let root_name symbol =
  match String.index_opt symbol '/' with
  | None -> symbol
  | Some index -> String.sub symbol 0 index

let workspace_providers sources =
  String_map.fold
    (fun filename source providers ->
      match providers with
      | Error _ as err -> err
      | Ok providers ->
          String_set.fold
            (fun name providers ->
              match providers with
              | Error _ as err -> err
              | Ok providers -> (
                  match String_map.find_opt name providers with
                  | Some existing when existing <> filename ->
                      Error.error
                        ("workspace symbol " ^ name
                       ^ " has multiple providers: " ^ existing ^ " and "
                       ^ filename)
                  | _ -> Ok (String_map.add name filename providers)))
            (provided_names source) (Ok providers))
    sources (Ok String_map.empty)

let workspace_components sources =
  match workspace_providers sources with
  | Error _ as err -> err
  | Ok providers ->
  let dependencies =
    String_map.mapi
      (fun filename source ->
        String_set.fold
             (fun symbol dependencies ->
               match String_map.find_opt (root_name symbol) providers with
               | Some provider when provider <> filename ->
                   String_set.add provider dependencies
               | _ -> dependencies)
             (referenced_names source)
             String_set.empty)
      sources
  in
  let adjacent filename =
    let direct =
      String_map.find_opt filename dependencies
      |> Option.value ~default:String_set.empty
    in
    String_map.fold
      (fun candidate candidate_dependencies adjacent ->
        if String_set.mem filename candidate_dependencies then
          String_set.add candidate adjacent
        else adjacent)
      dependencies direct
  in
  let rec component pending visited =
    match String_set.choose_opt pending with
    | None -> visited
    | Some filename ->
        let pending = String_set.remove filename pending in
        if String_set.mem filename visited then component pending visited
        else
          component
            (String_set.union pending (adjacent filename))
            (String_set.add filename visited)
  in
  let rec collect remaining components =
    match String_set.choose_opt remaining with
    | None -> List.rev components
    | Some filename ->
        let members = component (String_set.singleton filename) String_set.empty in
        collect (String_set.diff remaining members) (members :: components)
  in
  Ok
    (collect
       (String_map.to_seq sources |> Seq.map fst |> String_set.of_seq)
       [])

let analyze_component sources filenames =
  let component_sources =
    String_set.to_seq filenames
    |> Seq.map (fun filename -> (filename, String_map.find filename sources))
    |> List.of_seq
  in
  analyze_workspace component_sources
  |> Result.map (fun analyses ->
         List.fold_left
           (fun result (filename, analysis) ->
             String_map.add filename analysis result)
           String_map.empty analyses)

let create_workspace_index source_list =
  let sources =
    List.fold_left
      (fun sources (filename, source) -> String_map.add filename source sources)
      String_map.empty source_list
  in
  match workspace_components sources with
  | Error _ as err -> err
  | Ok components ->
  let analyze_individually component analyses errors =
    String_set.fold
      (fun filename (analyses, errors) ->
        match analyze ~filename (String_map.find filename sources) with
        | Ok analysis -> (String_map.add filename analysis analyses, errors)
        | Error error -> (analyses, String_map.add filename error errors))
      component (analyses, errors)
  in
  let rec analyze_all analyses errors = function
    | [] -> Ok { sources; analyses; errors; components }
    | component :: rest -> (
        match analyze_component sources component with
        | Error _ ->
            let analyses, errors =
              analyze_individually component analyses errors
            in
            analyze_all analyses errors rest
        | Ok component_analyses ->
            analyze_all
              (String_map.union (fun _ _ updated -> Some updated) analyses
                 component_analyses)
              errors rest)
  in
  analyze_all String_map.empty String_map.empty components

let workspace_analysis index filename =
  String_map.find_opt filename index.analyses

let workspace_error index filename = String_map.find_opt filename index.errors

let component_containing filename components =
  List.find_opt (String_set.mem filename) components
  |> Option.value ~default:(String_set.singleton filename)

let update_workspace_index index ~filename ~source =
  match String_map.find_opt filename index.sources with
  | Some previous when previous = source -> Ok (index, [])
  | _ ->
      let old_affected = component_containing filename index.components in
      let sources = String_map.add filename source index.sources in
      (match workspace_components sources with
      | Error _ as err -> err
      | Ok components ->
      let affected_components =
        List.filter
          (fun component ->
            not (String_set.is_empty (String_set.inter component old_affected)))
          components
      in
      let reanalyzed =
        List.fold_left String_set.union String_set.empty affected_components
      in
      let analyses =
        String_set.fold String_map.remove reanalyzed index.analyses
      in
      let errors = String_set.fold String_map.remove reanalyzed index.errors in
      let analyze_individually component analyses errors =
        String_set.fold
          (fun filename (analyses, errors) ->
            match analyze ~filename (String_map.find filename sources) with
            | Ok analysis -> (String_map.add filename analysis analyses, errors)
            | Error error -> (analyses, String_map.add filename error errors))
          component (analyses, errors)
      in
      let rec rebuild analyses errors = function
        | [] ->
            Ok
              ( { sources; analyses; errors; components },
                String_set.elements reanalyzed )
        | component :: rest -> (
            match analyze_component sources component with
            | Error _ ->
                let analyses, errors =
                  analyze_individually component analyses errors
                in
                rebuild analyses errors rest
            | Ok component_analyses ->
                rebuild
                  (String_map.union (fun _ _ updated -> Some updated) analyses
                     component_analyses)
                  errors rest)
      in
      rebuild analyses errors affected_components)
