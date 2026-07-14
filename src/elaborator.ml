let compile_expr = Expression_elaborator.compile_expr
let compile_top_level = Top_level_elaborator.compile

type state = Compiler_state.t

let empty_state = Compiler_state.empty

let expand_deferred_binding name value_type expression =
  let implementation_name = name ^ "__implementation" in
  let reference_type = Types.TRef (Types.TNullable value_type) in
  let reference =
    Lowered.Value_binding
      { pattern = Lowered.Named implementation_name;
        expression =
          Semantic_ir.annotate reference_type
            (Semantic_ir.Apply
               ( Semantic_ir.Ident "ref",
                 [ Semantic_ir.Constructor ("None", None) ] ));
      }
  in
  let wrapper =
    match value_type with
    | Types.TFn (parameter_types, _) ->
        let names =
          List.mapi
            (fun index _ -> "__lg_deferred_argument_" ^ string_of_int index)
            parameter_types
        in
        Semantic_ir.Fun
          ( List.map (fun name -> Semantic_ir.PVar name) names,
            Semantic_ir.Apply
              ( Semantic_ir.Apply
                  ( Semantic_ir.Ident "Option.get",
                    [ Semantic_ir.Prefix
                        ("!", Semantic_ir.Ident implementation_name) ] ),
                List.map (fun name -> Semantic_ir.Ident name) names ) )
    | _ ->
        Semantic_ir.Apply
          ( Semantic_ir.Ident "Option.get",
            [ Semantic_ir.Prefix
                ("!", Semantic_ir.Ident implementation_name) ] )
  in
  let wrapper =
    Lowered.Value_binding
      { pattern = Lowered.Named name;
        expression = Semantic_ir.annotate value_type wrapper;
      }
  in
  let initialize =
    Lowered.Value_binding
      { pattern = Lowered.Unit_pattern;
        expression =
          Semantic_ir.Infix
            ( ":=",
              Semantic_ir.Ident implementation_name,
              Semantic_ir.Constructor ("Some", Some expression) );
      }
  in
  ([ reference; wrapper ], [ initialize ])

let rec order_deferred_item = function
  | Lowered.Deferred_value_binding { name; value_type; expression } ->
      let immediate, deferred =
        expand_deferred_binding name value_type expression
      in
      (Lowered.Group immediate, deferred)
  | Lowered.Group items ->
      let immediate, deferred = order_deferred_items items in
      (Lowered.Group immediate, deferred)
  | item -> (item, [])

and order_deferred_items items =
  List.fold_left
    (fun (immediate, deferred) item ->
      let item_immediate, item_deferred = order_deferred_item item in
      (item_immediate :: immediate, deferred @ item_deferred))
    ([], []) items
  |> fun (immediate, deferred) -> (List.rev immediate, deferred)

let append_deferred items deferred =
  match (List.rev items, deferred) with
  | _, [] -> items
  | [], _ -> [ Lowered.Group deferred ]
  | Lowered.Group items :: rest, deferred ->
      List.rev (Lowered.Group (items @ deferred) :: rest)
  | item :: rest, deferred ->
      List.rev (Lowered.Group (item :: deferred) :: rest)

let compile_forms_incremental (state : Compiler_state.t) forms =
  let rec loop scope env next_type items = function
    | [] -> Ok (scope, env, next_type, List.rev items)
    | form :: rest -> (
        match compile_top_level scope env next_type form with
        | Error error ->
            Error
              (Error.with_location_if_missing (Source_context.find form) error)
        | Ok (scope, env, next_type, item) ->
            loop scope env next_type (item :: items) rest)
  in
  match loop state.scope state.env state.next_type [] forms with
  | Error _ as err -> err
  | Ok (scope, env, next_type, new_items) ->
      let immediate, deferred = order_deferred_items new_items in
      let new_items = append_deferred immediate deferred in
      let next_state =
        {
          Compiler_state.scope;
          Compiler_state.env;
          next_type;
          items = state.items @ new_items;
        }
      in
      Ok (next_state, new_items)

let compile_forms forms =
  match compile_forms_incremental empty_state forms with
  | Error _ as err -> err
  | Ok (_state, items) -> Ok items
