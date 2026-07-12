type hover = {
  contents : string;
  range : Ast.source_span;
}

type completion_item = {
  label : string;
  detail : string;
}

type t = {
  source : string;
  tokens : Ast.token list;
  compiler : Toolchain.language_analysis;
}

let analyze ~filename source =
  match Lexer.tokenize source with
  | Error _ as err -> err
  | Ok tokens -> (
      match Toolchain.analyze ~filename source with
      | Error _ as err -> err
      | Ok compiler -> Ok { source; tokens; compiler })

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

let print_type env ty =
  Printtyp.wrap_printing_env ~error:false env (fun () ->
      Format.asprintf "%a" Printtyp.type_scheme ty)

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
  match symbol_span_at analysis offset with
  | None -> None
  | Some _ -> (
      match
        smallest_expression analysis.compiler.typed_structure offset (fun expression ->
            match expression.exp_desc with
            | Typedtree.Texp_ident _ -> true
            | _ -> false)
      with
      | Some { exp_desc = Typedtree.Texp_ident (_, _, description); _ }
        when not description.val_loc.Location.loc_ghost ->
          Some description.val_loc
      | _ -> None)

let completion_source_names analysis =
  let current_ns = analysis.compiler.typecheck_state.Typecheck.current_ns in
  let namespace_prefix = if current_ns = "" then "" else current_ns ^ "/" in
  let prefix_length = String.length namespace_prefix in
  analysis.compiler.typecheck_state.env
  |> List.filter_map (fun (key, (binding : Types.binding)) ->
         if String.starts_with ~prefix:"__" key then None
         else
           let label =
             if
               prefix_length > 0
               && String.length key > prefix_length
               && String.sub key 0 prefix_length = namespace_prefix
             then String.sub key prefix_length (String.length key - prefix_length)
             else key
           in
           Some (binding.ocaml_name, label))

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
