let compile_expr = Expression_elaborator.compile_expr
let compile_top_level = Top_level_elaborator.compile

type state = Compiler_state.t

let empty_state = Compiler_state.empty

let rec contains_inferred_type = function
  | Types.TUnknown | Types.TVar _ -> true
  | Types.TNullable ty | Types.TArray ty | Types.TRef ty | Types.TList ty
  | Types.TVector ty | Types.TSet ty | Types.TSeq ty ->
      contains_inferred_type ty
  | Types.TOcaml_app (_, arguments) | Types.TTuple arguments ->
      List.exists contains_inferred_type arguments
  | Types.TFn (parameters, return_ty) ->
      List.exists contains_inferred_type (return_ty :: parameters)
  | Types.TOverloaded_fn arities ->
      List.exists
        (fun (arity : Types.fn_arity) ->
          List.exists contains_inferred_type arity.fixed_params
          || (match arity.rest_param with
             | Some ty -> contains_inferred_type ty
             | None -> false)
          || contains_inferred_type arity.return_ty)
        arities
  | Types.TRecord fields | Types.TNamed_record { fields; _ } ->
      List.exists
        (fun (field : Types.field) -> contains_inferred_type field.ty)
        fields
  | Types.TInt | Types.TFloat | Types.TChar | Types.TString | Types.TRegex
  | Types.TMap_keys | Types.TSymbol | Types.TKeyword | Types.TBool | Types.TUnit
  | Types.TNil | Types.TOcaml _ ->
      false

let deferred_type_variables ty =
  let rec collect variables = function
    | Types.TUnknown -> "a" :: variables
    | Types.TVar name -> name :: variables
    | Types.TNullable ty | Types.TArray ty | Types.TRef ty | Types.TList ty
    | Types.TVector ty | Types.TSet ty | Types.TSeq ty ->
        collect variables ty
    | Types.TOcaml_app (_, arguments) | Types.TTuple arguments ->
        List.fold_left collect variables arguments
    | Types.TFn (parameters, return_ty) ->
        List.fold_left collect (collect variables return_ty) parameters
    | Types.TOverloaded_fn arities ->
        List.fold_left
          (fun variables (arity : Types.fn_arity) ->
            let variables = collect variables arity.return_ty in
            let variables = List.fold_left collect variables arity.fixed_params in
            Option.fold ~none:variables ~some:(collect variables)
              arity.rest_param)
          variables arities
    | Types.TRecord fields ->
        List.fold_left
          (fun variables (field : Types.field) -> collect variables field.ty)
          variables fields
    | Types.TNamed_record record ->
        List.rev_append record.type_parameters variables
    | Types.TInt | Types.TFloat | Types.TChar | Types.TString | Types.TRegex
    | Types.TMap_keys | Types.TSymbol | Types.TKeyword | Types.TBool
    | Types.TUnit | Types.TNil | Types.TOcaml _ ->
        variables
  in
  collect [] ty |> List.sort_uniq String.compare

let freshen_deferred_type ?return_param_index ty =
  let next = ref 0 in
  let fresh_variable () =
    let name = "lg_deferred_" ^ string_of_int !next in
    incr next;
    Types.TVar name
  in
  let rec freshen = function
    | Types.TUnknown -> fresh_variable ()
    | Types.TVar _ as ty -> ty
    | Types.TNullable ty -> Types.TNullable (freshen ty)
    | Types.TArray ty -> Types.TArray (freshen ty)
    | Types.TRef ty -> Types.TRef (freshen ty)
    | Types.TList ty -> Types.TList (freshen ty)
    | Types.TVector ty -> Types.TVector (freshen ty)
    | Types.TSet ty -> Types.TSet (freshen ty)
    | Types.TSeq ty -> Types.TSeq (freshen ty)
    | Types.TOcaml_app (name, [ element_ty; value_ty ])
      when name = Types.seqable_constraint_name
           || name = Types.optional_seqable_constraint_name
           || name = Types.optional_sequential_constraint_name ->
        let element_ty =
          if Types.equal element_ty Types.TUnknown then
            Types.dynamic_constraint Types.TUnknown
          else freshen element_ty
        in
        let value_ty =
          if Types.equal value_ty Types.TUnknown then
            Types.dynamic_constraint Types.TUnknown
          else freshen value_ty
        in
        Types.TOcaml_app (name, [ element_ty; value_ty ])
    | Types.TOcaml_app (_, _) as constraint_ty
      when Option.is_some (Types.protocol_constraint_info constraint_ty) ->
        freshen_protocol_constraint constraint_ty
    | Types.TOcaml_app (name, arguments) ->
        Types.TOcaml_app (name, List.map freshen arguments)
    | Types.TTuple items -> Types.TTuple (List.map freshen items)
    | Types.TFn (parameters, return_ty) ->
        Types.TFn (List.map freshen parameters, freshen return_ty)
    | Types.TOverloaded_fn arities ->
        Types.TOverloaded_fn
          (List.map
             (fun (arity : Types.fn_arity) ->
               ({ fixed_params = List.map freshen arity.fixed_params;
                  rest_param = Option.map freshen arity.rest_param;
                  return_ty = freshen arity.return_ty;
                }
                 : Types.fn_arity))
             arities)
    | Types.TRecord fields ->
        Types.TRecord
          (List.map
             (fun (field : Types.field) ->
               { field with ty = freshen field.ty })
             fields)
    | Types.TNamed_record _ as ty -> ty
    | (Types.TInt | Types.TFloat | Types.TChar | Types.TString | Types.TRegex
      | Types.TMap_keys | Types.TSymbol | Types.TKeyword | Types.TBool
      | Types.TUnit | Types.TNil | Types.TOcaml _) as ty ->
        ty
  and freshen_protocol_constraint constraint_ty =
    let rec methods = function
      | Types.TUnit -> Some []
      | Types.TTuple [ method_ty; rest ] ->
          Option.map (fun rest -> method_ty :: rest) (methods rest)
      | _ -> None
    in
    let rec dynamic_unknowns = function
      | Types.TUnknown -> Types.dynamic_constraint Types.TUnknown
      | Types.TVar _ as ty -> ty
      | Types.TNullable ty -> Types.TNullable (dynamic_unknowns ty)
      | Types.TArray ty -> Types.TArray (dynamic_unknowns ty)
      | Types.TRef ty -> Types.TRef (dynamic_unknowns ty)
      | Types.TList ty -> Types.TList (dynamic_unknowns ty)
      | Types.TVector ty -> Types.TVector (dynamic_unknowns ty)
      | Types.TSet ty -> Types.TSet (dynamic_unknowns ty)
      | Types.TSeq ty -> Types.TSeq (dynamic_unknowns ty)
      | Types.TOcaml_app (name, arguments) ->
          Types.TOcaml_app (name, List.map dynamic_unknowns arguments)
      | Types.TTuple items -> Types.TTuple (List.map dynamic_unknowns items)
      | Types.TFn (parameters, return_ty) ->
          Types.TFn
            (List.map dynamic_unknowns parameters, dynamic_unknowns return_ty)
      | Types.TOverloaded_fn arities ->
          Types.TOverloaded_fn
            (List.map
               (fun (arity : Types.fn_arity) ->
                 ({ fixed_params = List.map dynamic_unknowns arity.fixed_params;
                    rest_param = Option.map dynamic_unknowns arity.rest_param;
                    return_ty = dynamic_unknowns arity.return_ty;
                  }
                   : Types.fn_arity))
               arities)
      | Types.TRecord fields ->
          Types.TRecord
            (List.map
               (fun (field : Types.field) ->
                 { field with ty = dynamic_unknowns field.ty })
               fields)
      | (Types.TNamed_record _ | Types.TInt | Types.TFloat | Types.TChar
      | Types.TString | Types.TRegex | Types.TMap_keys | Types.TSymbol
      | Types.TKeyword | Types.TBool | Types.TUnit | Types.TNil | Types.TOcaml _
        ) as ty ->
          ty
    in
    match Types.protocol_constraint_info constraint_ty with
    | None -> freshen constraint_ty
    | Some (protocol_id, witness_ty, value_ty) ->
        (* A witness whose receivers are already dynamic dispatches
           monomorphically in the implementation; freshening its container to a
           rigid variable would claim a polymorphism the generated code does
           not have. *)
        let dynamic_dispatch =
          match methods witness_ty with
          | Some method_tys ->
              List.exists
                (function
                  | Types.TFn (receiver :: _, _) -> Types.is_dynamic receiver
                  | _ -> false)
                method_tys
          | None -> false
        in
        let value_ty =
          if dynamic_dispatch then dynamic_unknowns value_ty
          else freshen value_ty
        in
        (match methods witness_ty with
        | None ->
            Types.protocol_constraint protocol_id [] value_ty
        | Some method_tys ->
            let method_tys =
              List.map
                (function
                  | Types.TFn (_receiver :: parameters, return_ty) ->
                      (* Untyped protocol method positions dispatch through
                         the dynamic witness ABI in the implementation;
                         rigid variables would claim polymorphism the
                         generated code does not have. *)
                      Types.TFn
                        ( Types.constraint_value_type value_ty
                          :: List.map dynamic_unknowns parameters,
                          dynamic_unknowns return_ty )
                  | method_ty -> freshen method_ty)
                method_tys
            in
            Types.protocol_constraint protocol_id method_tys value_ty)
  in
  match ty with
  | Types.TFn (parameters, return_ty) ->
      let parameters = List.map freshen parameters in
      let return_ty =
        match (return_param_index, return_ty, parameters) with
        | None, Types.TUnknown, [ parameter ] ->
            Types.constraint_value_type parameter
        | Some index, _, _ -> (
            match List.nth_opt parameters index with
            | Some parameter -> Types.constraint_value_type parameter
            | None -> freshen return_ty)
        | None, _, _ -> freshen return_ty
      in
      Types.TFn (parameters, return_ty)
  | ty -> freshen ty

let expand_deferred_binding name value_type return_param_index expression =
  let value_type = Types.align_deferred_param_types value_type expression in
  let value_type = freshen_deferred_type ?return_param_index value_type in
  let implementation_name = name ^ "__implementation" in
  let holder_type_name = implementation_name ^ "_holder" in
  let holder_field_name = "value" in
  let holder_type =
    Lowered.Polymorphic_holder_type
      { type_name = holder_type_name;
        field_name = holder_field_name;
        value_type;
        type_variables = deferred_type_variables value_type;
      }
  in
  let reference_type = Types.TRef (Types.TOcaml holder_type_name) in
  let reference =
    Lowered.Value_binding
      { pattern = Lowered.Named implementation_name;
        expression =
          Semantic_ir.annotate reference_type
            (Semantic_ir.Apply
               ( Semantic_ir.Ident "ref",
                 [ Semantic_ir.Record
                     ( [ ( holder_field_name,
                           Semantic_ir.Constructor ("None", None) ) ],
                       Some holder_type_name );
                 ] ));
      }
  in
  let implementation () =
    Semantic_ir.Apply
      ( Semantic_ir.Ident "Option.get",
        [ Semantic_ir.Field
            ( Semantic_ir.Prefix ("!", Semantic_ir.Ident implementation_name),
              holder_field_name );
        ] )
  in
  let parameter_patterns names parameter_types =
    List.map2
      (fun name ty ->
        if contains_inferred_type ty then Semantic_ir.PVar name
        else
          Semantic_ir.PConstraint
            (Semantic_ir.PVar name, Types.ocaml_name ty))
      names parameter_types
  in
  let wrapper =
    match value_type with
    | Types.TFn (parameter_types, _) ->
        let names =
          List.mapi
            (fun index _ -> "__lg_deferred_argument_" ^ string_of_int index)
            parameter_types
        in
        let patterns = parameter_patterns names parameter_types in
        Semantic_ir.Fun
          ( patterns,
            Semantic_ir.Apply
                ( implementation (),
                List.map (fun name -> Semantic_ir.Ident name) names ) )
    | Types.TOverloaded_fn arities ->
        let projection index =
          let rec descend expression remaining =
            if remaining = 0 then
              Semantic_ir.Apply (Semantic_ir.Ident "fst", [ expression ])
            else
              descend
                (Semantic_ir.Apply (Semantic_ir.Ident "snd", [ expression ]))
                (remaining - 1)
          in
          descend (implementation ()) index
        in
        let arity_wrapper index (arity : Types.fn_arity) =
          let parameter_types =
            arity.fixed_params
            @ Option.fold ~none:[] ~some:(fun rest_ty -> [ Types.TSeq rest_ty ])
                arity.rest_param
          in
          let names =
            List.mapi
              (fun parameter_index _ ->
                "__lg_deferred_argument_" ^ string_of_int index ^ "_"
                ^ string_of_int parameter_index)
              parameter_types
          in
          Semantic_ir.Fun
            ( parameter_patterns names parameter_types,
              Semantic_ir.Apply
                ( projection index,
                  List.map (fun name -> Semantic_ir.Ident name) names ) )
        in
        let rec storage index = function
          | [] -> Semantic_ir.Unit
          | arity :: rest ->
              Semantic_ir.Tuple
                [ arity_wrapper index arity; storage (index + 1) rest ]
        in
        storage 0 arities
    | _ ->
        implementation ()
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
              Semantic_ir.Record
                ( [ ( holder_field_name,
                      Semantic_ir.Constructor ("Some", Some expression) ) ],
                  Some holder_type_name ) );
      }
  in
  ([ holder_type; reference; wrapper ], [ initialize ])

let rec value_pattern_name = function
  | Lowered.Named name -> Some name
  | Lowered.Located_value (_, _, pattern) -> value_pattern_name pattern
  | Lowered.Unit_pattern | Lowered.Ignore_pattern -> None

let rec provided_value_names = function
  | Lowered.Value_binding { pattern; _ } ->
      Option.fold ~none:[] ~some:(fun name -> [ name ])
        (value_pattern_name pattern)
  | Lowered.Recursive_value_binding { name; _ }
  | Lowered.Deferred_value_binding { name; _ } ->
      [ name ]
  | Lowered.Recursive_value_bindings bindings ->
      List.map (fun (binding : Lowered.recursive_value) -> binding.name) bindings
  | Lowered.Record_def { var_name; _ }
  | Lowered.Projected_record_def { var_name; _ } ->
      [ var_name ]
  | Lowered.Group items -> List.concat_map provided_value_names items
  | Lowered.Polymorphic_holder_type _ | Lowered.Comment _
  | Lowered.Type_def _ | Lowered.Type_alias _ | Lowered.Type_variant _
  | Lowered.Module_def _ | Lowered.Module_alias _ | Lowered.Module_functor _
  | Lowered.Module_apply _ | Lowered.Module_signature _
  | Lowered.Open_module _ | Lowered.Include_module _ ->
      []

let rec expand_deferred_item = function
  | Lowered.Deferred_value_binding
      { name; value_type; return_param_index; expression } ->
      let immediate, deferred =
        expand_deferred_binding name value_type return_param_index expression
      in
      ( Lowered.Group immediate,
        List.map (fun initialize -> (expression, initialize)) deferred )
  | Lowered.Group items ->
      let immediate, deferred = expand_deferred_items items in
      (Lowered.Group immediate, deferred)
  | item -> (item, [])

and expand_deferred_items items =
  List.fold_left
    (fun (immediate, deferred) item ->
      let item_immediate, item_deferred = expand_deferred_item item in
      (item_immediate :: immediate, deferred @ item_deferred))
    ([], []) items
  |> fun (immediate, deferred) -> (List.rev immediate, deferred)

let order_deferred_items items =
  let providers = List.map provided_value_names items in
  let scheduled = Array.make (List.length items) [] in
  let immediate =
    List.mapi
      (fun index item ->
        let immediate, deferred = expand_deferred_item item in
        List.iter
          (fun (expression, initialize) ->
            let target =
              providers
              |> List.mapi (fun provider_index names ->
                     if
                       List.exists
                         (fun name ->
                           Semantic_ir.exists_identifier
                             (String.equal name)
                             expression)
                         names
                     then provider_index
                     else index)
              |> List.fold_left max index
            in
            scheduled.(target) <- scheduled.(target) @ [ initialize ])
          deferred;
        immediate)
      items
  in
  immediate
  |> List.mapi (fun index item ->
         match scheduled.(index) with
         | [] -> item
         | initializers -> (
             match item with
             | Lowered.Group items -> Lowered.Group (items @ initializers)
             | item -> Lowered.Group (item :: initializers)))

let rec form_references_unresolved_declaration scope env = function
  | Ast.FSymbol name -> (
      match Resolver.lookup_binding scope env name with
      | Ok (binding : Types.binding) ->
          Types.equal binding.ty (Types.TOcaml "__declared_fn")
          || (binding.forward_declared && contains_inferred_type binding.ty)
      | Error _ -> false)
  | Ast.FList (Ast.FSymbol ("quote" | "clojure.core/quote") :: _) -> false
  | Ast.FList forms | Ast.FVector forms ->
      List.exists (form_references_unresolved_declaration scope env) forms
  | Ast.FMap pairs ->
      List.exists
        (fun (key, value) ->
          form_references_unresolved_declaration scope env key
          || form_references_unresolved_declaration scope env value)
        pairs
  | Ast.FCoreSymbol _ | Ast.FKeyword _ | Ast.FString _ | Ast.FRegex _
  | Ast.FInt _ | Ast.FFloat _ | Ast.FChar _ | Ast.FBool _ ->
      false

let rec form_references_names names = function
  | Ast.FSymbol name ->
      List.exists
        (fun provider ->
          name = provider || String.ends_with ~suffix:("/" ^ provider) name)
        names
  | Ast.FList (Ast.FSymbol ("quote" | "clojure.core/quote") :: _) -> false
  | Ast.FList forms | Ast.FVector forms ->
      List.exists (form_references_names names) forms
  | Ast.FMap pairs ->
      List.exists
        (fun (key, value) ->
          form_references_names names key || form_references_names names value)
        pairs
  | Ast.FCoreSymbol _ | Ast.FKeyword _ | Ast.FString _ | Ast.FRegex _
  | Ast.FInt _ | Ast.FFloat _ | Ast.FChar _ | Ast.FBool _ ->
      false

let add_unresolved_names names form =
  List.fold_left
    (fun names name ->
      if List.mem name names then names else name :: names)
    names (Dependency_graph.provided_names form)

let remove_resolved_names names form =
  let resolved = Dependency_graph.provided_names form in
  List.filter (fun name -> not (List.mem name resolved)) names

let compile_forms_incremental (state : Compiler_state.t) forms =
  let report_timings = Sys.getenv_opt "LG_COMPILE_TIMINGS" = Some "1" in
  let finish scope env next_type items =
    let items =
      items
      |> List.sort (fun (left, _) (right, _) -> Int.compare left right)
      |> List.map snd
    in
    Ok (scope, env, next_type, items)
  in
  let rec compile_pending scope env next_type items unresolved_names pending =
    let rec loop scope env next_type items unresolved_names deferred first_error
        made_progress = function
      | [] ->
          if deferred = [] then finish scope env next_type items
          else if made_progress then
            compile_pending scope env next_type items unresolved_names
              (List.rev deferred)
          else (
            match first_error with
            | Some error -> Error error
            | None -> Error.error "declared forms made no compilation progress")
      | (index, form) :: rest -> (
        let started_at = if report_timings then Sys.time () else 0.0 in
        let compiled = compile_top_level scope env next_type form in
        let elapsed = if report_timings then Sys.time () -. started_at else 0.0 in
        if report_timings && elapsed >= 0.01 then (
          let names = Dependency_graph.provided_names form in
          Printf.eprintf "lg: form %d%s: %.3fs\n%!" index
            (match names with
            | [] -> ""
            | _ -> " (" ^ String.concat ", " names ^ ")")
            elapsed);
        match compiled with
        | Error error ->
            let error =
              Error.with_location_if_missing (Source_context.find form) error
            in
            let can_defer =
              form_references_unresolved_declaration scope env form
              || form_references_names unresolved_names form
            in
            if can_defer then
              let first_error =
                match first_error with
                | Some _ -> first_error
                | None -> Some error
              in
              let unresolved_names =
                add_unresolved_names unresolved_names form
              in
              loop scope env next_type items unresolved_names
                ((index, form) :: deferred) first_error made_progress rest
            else Error error
        | Ok (scope, env, next_type, item) ->
            let unresolved_names =
              remove_resolved_names unresolved_names form
            in
            loop scope env next_type ((index, item) :: items) unresolved_names
              deferred first_error true rest)
    in
    loop scope env next_type items unresolved_names [] None false pending
  in
  let indexed_forms = List.mapi (fun index form -> (index, form)) forms in
  match
    compile_pending state.scope state.env state.next_type [] [] indexed_forms
  with
  | Error _ as err -> err
  | Ok (scope, env, next_type, new_items) ->
      let new_items = order_deferred_items new_items in
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
