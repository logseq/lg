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

let rec add_dynamic_record_request requests record =
  let already_requested =
    List.exists
      (fun existing ->
        Type_id.equal existing.Types.type_id record.Types.type_id)
      !requests
  in
  if record.Types.type_parameters = [] && not already_requested then (
    requests := record :: !requests;
    List.iter
      (fun (field : Types.field) ->
        add_dynamic_record_requests_from_type requests field.ty)
      record.fields)

and add_dynamic_record_requests_from_type requests = function
  | Types.TNamed_record record -> add_dynamic_record_request requests record
  | Types.TNullable ty | Types.TArray ty | Types.TRef ty | Types.TList ty
  | Types.TVector ty | Types.TSet ty | Types.TSeq ty ->
      add_dynamic_record_requests_from_type requests ty
  | Types.TOcaml_app (_, arguments) | Types.TTuple arguments ->
      List.iter (add_dynamic_record_requests_from_type requests) arguments
  | Types.TFn (arguments, result) ->
      List.iter (add_dynamic_record_requests_from_type requests) arguments;
      add_dynamic_record_requests_from_type requests result
  | Types.TOverloaded_fn arities ->
      List.iter
        (fun (arity : Types.fn_arity) ->
          List.iter
            (add_dynamic_record_requests_from_type requests)
            arity.fixed_params;
          Option.iter
            (add_dynamic_record_requests_from_type requests)
            arity.rest_param;
          add_dynamic_record_requests_from_type requests arity.return_ty)
        arities
  | Types.TRecord fields ->
      List.iter
        (fun (field : Types.field) ->
          add_dynamic_record_requests_from_type requests field.ty)
        fields
  | Types.TInt | Types.TFloat | Types.TChar | Types.TString | Types.TRegex
  | Types.TMap_keys | Types.TSymbol | Types.TKeyword | Types.TBool
  | Types.TUnit | Types.TNil | Types.TUnknown | Types.TVar _ | Types.TOcaml _ ->
      ()

let collect_dynamic_record_requests items =
  let requests = ref [] in
  let collect_expression expression =
    ignore
      (Semantic_ir.rewrite
         (fun expression ->
           (match expression with
           | Semantic_ir.PackDynamic
               { source_ty = Types.TNamed_record record; _ } ->
               add_dynamic_record_request requests record
           | _ -> ());
           expression)
         expression)
  in
  let rec collect_item = function
    | Lowered.Value_binding { expression; _ }
    | Lowered.Recursive_value_binding { expression; _ }
    | Lowered.Deferred_value_binding { expression; _ } ->
        collect_expression expression
    | Lowered.Recursive_value_bindings bindings ->
        List.iter
          (fun (binding : Lowered.recursive_value) ->
            collect_expression binding.expression)
          bindings
    | Lowered.Record_def { values; _ } ->
        List.iter (fun (_, value) -> collect_expression value) values
    | Lowered.Projected_record_def { source; _ } -> collect_expression source
    | Lowered.Group items
    | Lowered.Module_def { items; _ }
    | Lowered.Module_functor { items; _ } ->
        List.iter collect_item items
    | Lowered.Polymorphic_holder_type _ | Lowered.Comment _
    | Lowered.Type_def _ | Lowered.Type_alias _ | Lowered.Type_variant _
    | Lowered.Module_alias _ | Lowered.Module_apply _
    | Lowered.Module_signature _ | Lowered.Open_module _
    | Lowered.Include_module _ ->
        ()
  in
  List.iter collect_item items;
  List.rev !requests

let local_record_type_name (record : Types.named_record) =
  match String.rindex_opt record.type_name '.' with
  | None -> record.type_name
  | Some separator ->
      String.sub record.type_name (separator + 1)
        (String.length record.type_name - separator - 1)

let requested_dynamic_record requests type_id type_name =
  List.exists
    (fun record ->
      Type_id.equal record.Types.type_id type_id
      || String.equal (local_record_type_name record) type_name)
    requests

let append_basic_dynamic_record_packer env item record =
  Result.map
    (function
      | None -> item
      | Some expression ->
          Lowered.Group
            [
              item;
              Lowered.Value_binding
                { pattern = Lowered.Unit_pattern; expression };
            ])
    (Call_elaborator.compile_dynamic_record_packer_registration ~basic:true
       ~protocol_ids:[] env record)

let rec add_initial_dynamic_record_packer env requests = function
  | Lowered.Type_def
      ({
         type_id;
         type_name;
         type_parameters;
         fields;
         nominal;
         dynamic_packer;
         location;
       } as definition) ->
      if
        dynamic_packer
        || not (requested_dynamic_record requests type_id type_name)
      then
        Ok (Lowered.Type_def definition)
      else
        let type_item =
          Lowered.Type_def
            {
              type_id;
              type_name;
              type_parameters;
              fields;
              nominal;
              dynamic_packer = true;
              location;
            }
        in
        let record =
          match
            Types.named_record ~type_id ~nominal ~type_name ~type_parameters
              ~set_module_name:("Set_" ^ type_name) fields
          with
          | Types.TNamed_record record -> record
          | _ -> assert false
        in
        append_basic_dynamic_record_packer env type_item record
  | Lowered.Group items ->
      Result.map (fun items -> Lowered.Group items)
        (add_initial_dynamic_record_packers env requests items)
  | Lowered.Module_def definition ->
      Result.map
        (fun items -> Lowered.Module_def { definition with items })
        (add_initial_dynamic_record_packers env requests definition.items)
  | Lowered.Module_functor definition ->
      Result.map
        (fun items -> Lowered.Module_functor { definition with items })
        (add_initial_dynamic_record_packers env requests definition.items)
  | Lowered.Record_def
      ({ type_id; type_name; set_module_name; fields; dynamic_packer; _ } as definition)
      as item ->
      if
        dynamic_packer
        || not (requested_dynamic_record requests type_id type_name)
      then Ok item
      else
        let record =
          match
            Types.named_record ~type_id ~type_name ~set_module_name fields
          with
          | Types.TNamed_record record -> record
          | _ -> assert false
        in
        append_basic_dynamic_record_packer env
          (Lowered.Record_def { definition with dynamic_packer = true }) record
  | Lowered.Projected_record_def
      ({ type_id; type_name; set_module_name; fields; dynamic_packer; _ } as definition)
      as item ->
      if
        dynamic_packer
        || not (requested_dynamic_record requests type_id type_name)
      then Ok item
      else
        let record =
          match
            Types.named_record ~type_id ~type_name ~set_module_name fields
          with
          | Types.TNamed_record record -> record
          | _ -> assert false
        in
        append_basic_dynamic_record_packer env
          (Lowered.Projected_record_def
             { definition with dynamic_packer = true })
          record
  | item -> Ok item

and add_initial_dynamic_record_packers env requests items =
  let rec loop compiled = function
    | [] -> Ok (List.rev compiled)
    | item :: rest ->
        Result.bind
          (add_initial_dynamic_record_packer env requests item)
          (fun item -> loop (item :: compiled) rest)
  in
  loop [] items

let dynamic_record_is_defined items record =
  let rec defined = function
    | Lowered.Type_def { type_id = candidate; type_name; _ } ->
        Type_id.equal candidate record.Types.type_id
        || String.equal type_name (local_record_type_name record)
    | Lowered.Record_def { type_name; _ }
    | Lowered.Projected_record_def { type_name; _ } ->
        Type_id.equal (Types.type_id_of_name type_name) record.Types.type_id
        || String.equal type_name (local_record_type_name record)
    | Lowered.Group items
    | Lowered.Module_def { items; _ }
    | Lowered.Module_functor { items; _ } ->
        List.exists defined items
    | _ -> false
  in
  List.exists defined items

let compile_existing_dynamic_record_packers env items requests =
  let rec loop registrations = function
    | [] -> Ok (List.rev registrations)
    | record :: rest when dynamic_record_is_defined items record ->
        loop registrations rest
    | record :: rest ->
        Result.bind
          (Call_elaborator.compile_dynamic_record_packer_registration env record)
          (fun registration ->
            let registrations =
              match registration with
              | None -> registrations
              | Some expression ->
                  Lowered.Value_binding
                    { pattern = Lowered.Unit_pattern; expression }
                  :: registrations
            in
            loop registrations rest)
  in
  loop [] requests

let share_expression ?(forbidden = []) shared_values expression =
  let shared_values = ref shared_values in
  let definitions = ref [] in
  let expression =
    Semantic_ir.rewrite
      (function
        | Semantic_ir.SharedValue (name, value) ->
            if
              Semantic_ir.exists_identifier
                (fun identifier -> List.mem identifier forbidden)
                value
            then value
            else if List.mem name !shared_values then Semantic_ir.Ident name
            else (
              shared_values := name :: !shared_values;
              definitions :=
                Lowered.Value_binding
                  { pattern = Lowered.Named name; expression = value }
                :: !definitions;
              Semantic_ir.Ident name)
        | expression -> expression)
      expression
  in
  (List.rev !definitions, expression, !shared_values)

let share_expressions shared_values expressions =
  let rec loop shared_values definitions compiled = function
    | [] -> (List.rev definitions, List.rev compiled, shared_values)
    | expression :: rest ->
        let new_definitions, expression, shared_values =
          share_expression shared_values expression
        in
        loop shared_values
          (List.rev_append new_definitions definitions)
          (expression :: compiled) rest
  in
  loop shared_values [] [] expressions

let rec share_item_expressions shared_values item =
  let grouped definitions item =
    match definitions with
    | [] -> [ item ]
    | _ -> [ Lowered.Group (definitions @ [ item ]) ]
  in
  let share_single ?(forbidden = []) rebuild expression =
    let definitions, expression, shared_values =
      share_expression ~forbidden shared_values expression
    in
    (grouped definitions (rebuild expression), shared_values)
  in
  match item with
  | Lowered.Value_binding binding ->
      let forbidden =
        match binding.pattern with Lowered.Named name -> [ name ] | _ -> []
      in
      share_single ~forbidden
        (fun expression -> Lowered.Value_binding { binding with expression })
        binding.expression
  | Lowered.Recursive_value_binding binding ->
      share_single ~forbidden:[ binding.name ]
        (fun expression ->
          Lowered.Recursive_value_binding { binding with expression })
        binding.expression
  | Lowered.Recursive_value_bindings bindings ->
      let forbidden =
        List.map (fun (binding : Lowered.recursive_value) -> binding.name) bindings
      in
      let definitions, expressions, shared_values =
        let rec loop shared_values definitions compiled = function
          | [] -> (List.rev definitions, List.rev compiled, shared_values)
          | (binding : Lowered.recursive_value) :: rest ->
              let new_definitions, expression, shared_values =
                share_expression ~forbidden shared_values binding.expression
              in
              loop shared_values
                (List.rev_append new_definitions definitions)
                (expression :: compiled) rest
        in
        loop shared_values [] [] bindings
      in
      let bindings =
        List.map2
          (fun (binding : Lowered.recursive_value) expression ->
            { binding with expression })
          bindings expressions
      in
      ( grouped definitions (Lowered.Recursive_value_bindings bindings),
        shared_values )
  | Lowered.Deferred_value_binding binding ->
      share_single ~forbidden:[ binding.name ]
        (fun expression ->
          Lowered.Deferred_value_binding { binding with expression })
        binding.expression
  | Lowered.Record_def definition ->
      let definitions, expressions, shared_values =
        share_expressions shared_values (List.map snd definition.values)
      in
      let values =
        List.map2
          (fun (field, _) expression -> (field, expression))
          definition.values expressions
      in
      ( grouped definitions (Lowered.Record_def { definition with values }),
        shared_values )
  | Lowered.Projected_record_def definition ->
      share_single ~forbidden:[ definition.var_name ]
        (fun source ->
          Lowered.Projected_record_def { definition with source })
        definition.source
  | Lowered.Group items ->
      let items, shared_values = share_item_list shared_values items in
      ([ Lowered.Group items ], shared_values)
  | Lowered.Module_def definition ->
      let items, _ = share_item_list [] definition.items in
      ([ Lowered.Module_def { definition with items } ], shared_values)
  | Lowered.Module_functor definition ->
      let items, _ = share_item_list [] definition.items in
      ([ Lowered.Module_functor { definition with items } ], shared_values)
  | item -> ([ item ], shared_values)

and share_item_list shared_values items =
  let rec loop shared_values compiled = function
    | [] -> (List.rev compiled, shared_values)
    | item :: rest ->
        let items, shared_values =
          share_item_expressions shared_values item
        in
        loop shared_values (List.rev_append items compiled) rest
  in
  loop shared_values [] items

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

let resolve_anonymous_record_patterns env items =
  let rec resolve_pattern = function
    | Semantic_ir.PLocated (node_id, location, pattern) ->
        Semantic_ir.PLocated
          (node_id, location, resolve_pattern pattern)
    | Semantic_ir.PTyped (pattern, Types.TRecord fields) -> (
        match
          Compiler_environment.find_anonymous_record
            ~owner:(Source_context.anonymous_record_owner "") fields env
        with
        | Some record ->
            Semantic_ir.PConstraint
              ( resolve_pattern pattern,
                Structural_map.record_type_application record )
        | None ->
            Semantic_ir.PTyped
              (resolve_pattern pattern, Types.TRecord fields))
    | Semantic_ir.PTyped (pattern, ty) ->
        Semantic_ir.PTyped (resolve_pattern pattern, ty)
    | Semantic_ir.PConstructor (name, payload) ->
        Semantic_ir.PConstructor (name, Option.map resolve_pattern payload)
    | Semantic_ir.PTuple patterns ->
        Semantic_ir.PTuple (List.map resolve_pattern patterns)
    | Semantic_ir.PList patterns ->
        Semantic_ir.PList (List.map resolve_pattern patterns)
    | Semantic_ir.PCons (head, tail) ->
        Semantic_ir.PCons (resolve_pattern head, resolve_pattern tail)
    | Semantic_ir.PRecord fields ->
        Semantic_ir.PRecord
          (List.map
             (fun (name, pattern) -> (name, resolve_pattern pattern))
             fields)
    | Semantic_ir.PAlias (pattern, name) ->
        Semantic_ir.PAlias (resolve_pattern pattern, name)
    | Semantic_ir.POr (left, right) ->
        Semantic_ir.POr (resolve_pattern left, resolve_pattern right)
    | Semantic_ir.PConstraint (pattern, type_name) ->
        Semantic_ir.PConstraint (resolve_pattern pattern, type_name)
    | (Semantic_ir.PVar _ | Semantic_ir.PAny | Semantic_ir.PUnit
      | Semantic_ir.PInt _ | Semantic_ir.PInt64 _ | Semantic_ir.PString _
      | Semantic_ir.PBool _) as pattern ->
        pattern
  in
  let resolve_expression expression =
    Semantic_ir.rewrite
      (function
        | Semantic_ir.Fun (patterns, body) ->
            Semantic_ir.Fun (List.map resolve_pattern patterns, body)
        | Semantic_ir.Let (bindings, body) ->
            Semantic_ir.Let
              (List.map
                 (fun (pattern, value) ->
                   (resolve_pattern pattern, value))
                 bindings,
               body)
        | Semantic_ir.LetRec (name, patterns, body, arguments) ->
            Semantic_ir.LetRec
              (name, List.map resolve_pattern patterns, body, arguments)
        | Semantic_ir.LetRecIn (name, patterns, body, next) ->
            Semantic_ir.LetRecIn
              (name, List.map resolve_pattern patterns, body, next)
        | Semantic_ir.Match (target, cases) ->
            Semantic_ir.Match
              (target,
               List.map
                 (fun (pattern, body) ->
                   (resolve_pattern pattern, body))
                 cases)
        | Semantic_ir.Match_guarded (target, cases) ->
            Semantic_ir.Match_guarded
              (target,
               List.map
                 (fun (pattern, guard, body) ->
                   (resolve_pattern pattern, guard, body))
                 cases)
        | Semantic_ir.Try (body, cases) ->
            Semantic_ir.Try
              (body,
               List.map
                 (fun (pattern, guard, result) ->
                   (resolve_pattern pattern, guard, result))
                 cases)
        | expression -> expression)
      expression
  in
  let rec resolve_item = function
    | Lowered.Value_binding binding ->
        Lowered.Value_binding
          { binding with expression = resolve_expression binding.expression }
    | Lowered.Recursive_value_binding binding ->
        Lowered.Recursive_value_binding
          { binding with expression = resolve_expression binding.expression }
    | Lowered.Recursive_value_bindings bindings ->
        Lowered.Recursive_value_bindings
          (List.map
             (fun (binding : Lowered.recursive_value) ->
               { binding with
                 expression = resolve_expression binding.expression;
               })
             bindings)
    | Lowered.Deferred_value_binding binding ->
        Lowered.Deferred_value_binding
          { binding with expression = resolve_expression binding.expression }
    | Lowered.Record_def definition ->
        Lowered.Record_def
          { definition with
            values =
              List.map
                (fun (field, value) ->
                  (field, resolve_expression value))
                definition.values;
          }
    | Lowered.Projected_record_def definition ->
        Lowered.Projected_record_def
          { definition with source = resolve_expression definition.source }
    | Lowered.Group items -> Lowered.Group (List.map resolve_item items)
    | Lowered.Module_def definition ->
        Lowered.Module_def
          { definition with items = List.map resolve_item definition.items }
    | Lowered.Module_functor definition ->
        Lowered.Module_functor
          { definition with items = List.map resolve_item definition.items }
    | item -> item
  in
  List.map resolve_item items

let rec form_uses_runtime_var_reflection = function
  | Ast.FList (Ast.FSymbol ("resolve" | "requiring-resolve") :: _) -> true
  | Ast.FList (Ast.FSymbol ("quote" | "clojure.core/quote") :: _) -> false
  | Ast.FList forms | Ast.FVector forms ->
      List.exists form_uses_runtime_var_reflection forms
  | Ast.FMap pairs ->
      List.exists
        (fun (key, value) ->
          form_uses_runtime_var_reflection key
          || form_uses_runtime_var_reflection value)
        pairs
  | Ast.FCoreSymbol _ | Ast.FSymbol _ | Ast.FKeyword _ | Ast.FString _
  | Ast.FRegex _ | Ast.FInt _ | Ast.FFloat _ | Ast.FChar _ | Ast.FBool _ ->
      false

let rec runtime_definition_names scope = function
  | Ast.FList [ Ast.FSymbol "defn-signature"; definition ] ->
      runtime_definition_names scope definition
  | Ast.FList (Ast.FSymbol "recursive-definition-group" :: definitions) ->
      List.concat_map (runtime_definition_names scope) definitions
  | Ast.FList
      (Ast.FSymbol ("def" | "defonce" | "defn" | "defn-")
      :: Ast.FSymbol name :: _) ->
      [ Names.scoped_key scope name ]
  | _ -> []

let rec qualified_symbols = function
  | Ast.FSymbol name when Names.is_qualified name -> [ name ]
  | Ast.FList forms | Ast.FVector forms ->
      List.concat_map qualified_symbols forms
  | Ast.FMap pairs ->
      List.concat_map
        (fun (key, value) -> qualified_symbols key @ qualified_symbols value)
        pairs
  | Ast.FCoreSymbol _ | Ast.FSymbol _ | Ast.FKeyword _ | Ast.FString _
  | Ast.FRegex _ | Ast.FInt _ | Ast.FFloat _ | Ast.FChar _ | Ast.FBool _ ->
      []

let rec quoted_qualified_symbols = function
  | Ast.FList
      [ Ast.FSymbol ("quote" | "clojure.core/quote"); quoted ] ->
      qualified_symbols quoted
  | Ast.FList forms | Ast.FVector forms ->
      List.concat_map quoted_qualified_symbols forms
  | Ast.FMap pairs ->
      List.concat_map
        (fun (key, value) ->
          quoted_qualified_symbols key @ quoted_qualified_symbols value)
        pairs
  | Ast.FCoreSymbol _ | Ast.FSymbol _ | Ast.FKeyword _ | Ast.FString _
  | Ast.FRegex _ | Ast.FInt _ | Ast.FFloat _ | Ast.FChar _ | Ast.FBool _ ->
      []

let compile_runtime_var_registrations definitions requests registered env
    reflection_requested =
  if not reflection_requested then ([], registered)
  else
    List.fold_left
      (fun (items, registered) source_name ->
        if
          List.mem source_name registered
          || not (List.mem source_name requests)
        then (items, registered)
        else
          match Compiler_environment.find_opt source_name env with
          | None -> (items, registered)
          | Some (binding : Types.binding) when binding.forward_declared ->
              (items, registered)
          | Some binding ->
              let dynamic = Types.dynamic_constraint Types.TUnknown in
              let value =
                Types.typed_ir binding.ty (Semantic_ir.Ident binding.ocaml_name)
              in
              (match Call_elaborator.pack_dynamic_value env dynamic value with
              | Error _ -> (items, registered)
              | Ok packed ->
                  let registration =
                    Lowered.Value_binding
                      {
                        pattern = Lowered.Unit_pattern;
                        expression =
                          Semantic_ir.Apply
                            ( Semantic_ir.Ident
                                "Lg_runtime.Runtime_dynamic.register_var",
                              [ Semantic_ir.String source_name; packed ] );
                      }
                  in
                  (registration :: items, source_name :: registered)))
      ([], registered) definitions

let surround_item before item after =
  match (before, after) with
  | [], [] -> item
  | _ ->
      Lowered.Group (List.rev before @ [ item ] @ List.rev after)

let compile_forms_incremental (state : Compiler_state.t) forms =
  let report_timings = Sys.getenv_opt "LG_COMPILE_TIMINGS" = Some "1" in
  let runtime_var_reflection = ref state.runtime_var_reflection in
  let runtime_definitions = ref state.runtime_definitions in
  let runtime_var_requests = ref state.runtime_var_requests in
  let runtime_vars = ref state.runtime_vars in
  let finish scope env next_type items =
    let items =
      items
      |> List.sort (fun (left, _) (right, _) -> Int.compare left right)
      |> List.map snd
    in
    Ok
      ( scope,
        env,
        next_type,
        items,
        !runtime_var_reflection,
        !runtime_definitions,
        !runtime_var_requests,
        !runtime_vars )
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
        let previous_env = env in
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
            runtime_var_requests :=
              List.rev_append
                (quoted_qualified_symbols form)
                !runtime_var_requests;
            runtime_var_reflection :=
              !runtime_var_reflection || form_uses_runtime_var_reflection form;
            let before_registrations, registered =
              compile_runtime_var_registrations !runtime_definitions
                !runtime_var_requests !runtime_vars previous_env
                !runtime_var_reflection
            in
            runtime_definitions :=
              List.rev_append
                (runtime_definition_names scope form)
                !runtime_definitions;
            let after_registrations, registered =
              compile_runtime_var_registrations !runtime_definitions
                !runtime_var_requests registered env
                !runtime_var_reflection
            in
            runtime_vars := registered;
            let item =
              surround_item before_registrations item after_registrations
            in
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
  | Ok
      ( scope,
        env,
        next_type,
        new_items,
        runtime_var_reflection,
        runtime_definitions,
        runtime_var_requests,
        runtime_vars ) ->
      let new_items =
        new_items |> order_deferred_items
        |> resolve_anonymous_record_patterns env
      in
      let requested =
        collect_dynamic_record_requests new_items
        |> List.filter (fun record ->
               not
                 (List.exists
                    (fun type_id ->
                      Type_id.equal type_id record.Types.type_id)
                    state.dynamic_packers))
      in
      Result.bind
        (add_initial_dynamic_record_packers env requested new_items)
        (fun new_items ->
          Result.map
            (fun registrations ->
              let new_items =
                match (registrations, new_items) with
                | [], items -> items
                | registrations, Lowered.Group items :: rest ->
                    Lowered.Group (registrations @ items) :: rest
                | registrations, first :: rest ->
                    Lowered.Group (registrations @ [ first ]) :: rest
                | _ :: _, [] -> assert false
              in
              let dynamic_packers =
                List.fold_left
                  (fun type_ids record -> record.Types.type_id :: type_ids)
                  state.dynamic_packers requested
              in
              let new_items, shared_values =
                share_item_list state.shared_values new_items
              in
              let next_state =
                {
                  Compiler_state.scope;
                  Compiler_state.env;
                  next_type;
                  items = state.items @ new_items;
                  dynamic_packers;
                  shared_values;
                  runtime_var_reflection;
                  runtime_definitions;
                  runtime_var_requests;
                  runtime_vars;
                }
              in
              (next_state, new_items))
            (compile_existing_dynamic_record_packers env new_items requested))

let compile_forms forms =
  match compile_forms_incremental empty_state forms with
  | Error _ as err -> err
  | Ok (_state, items) -> Ok items
