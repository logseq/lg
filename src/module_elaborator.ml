open Ast
open Types
open Lowered

module Env = Compiler_environment

let compile_expr = Expression_elaborator.compile_expr
let prepare_fn = Expression_elaborator.prepare_fn
let prepare_recursive_fn = Expression_elaborator.prepare_recursive_fn
let fn_code = Expression_elaborator.fn_code
let binding_of_expr = Expression_support.binding_of_expr
let row_param_type_names = Expression_support.row_param_type_names
let row_type_items = Expression_support.row_type_items
let check_emitted_name_collision = Resolver.check_emitted_name_collision
let inherit_scope_ocaml_value_refers =
  Expression_support.inherit_scope_ocaml_value_refers
let compile_defprotocol = Protocol_elaborator.compile_defprotocol
let compile_extend_type = Protocol_elaborator.compile_extend_type

let module_id_of_path module_path =
  match String.rindex_opt module_path '.' with
  | None -> Module_id.create ~owner:[] ~name:module_path
  | Some separator ->
      let owner = String.sub module_path 0 separator in
      let name =
        String.sub module_path (separator + 1)
          (String.length module_path - separator - 1)
      in
      Module_id.create ~owner:[ owner ] ~name

let module_binding_key = Module_environment.binding_key
let module_binding_ocaml_name = Module_environment.binding_ocaml_name
let changed_bindings = Module_environment.changed_bindings
let open_module_bindings = Module_environment.open_bindings
let include_module_public_bindings = Module_environment.include_public_bindings
let alias_module_bindings = Module_environment.alias_bindings

let resolve_module_target_path scope env target_name =
  if scope = "" || String.contains target_name '.' then target_name
  else
    let local_id = Module_id.create ~owner:[ scope ] ~name:target_name in
    if Module_registry.mem_module local_id (Env.modules env) then
      scope ^ "." ^ target_name
    else target_name

let compile_module_alias ?semantic_target scope env next_type alias_name target_name =
  let target_path =
    Option.value semantic_target
      ~default:(resolve_module_target_path scope env target_name)
  in
  let alias_bindings = alias_module_bindings env alias_name target_path in
  let owner = if scope = "" then [] else [ scope ] in
  let alias_id = Module_id.create ~owner ~name:alias_name in
  let target_id = Module_id.of_string target_path in
  match Module_registry.declare_alias alias_id target_id (Env.modules env) with
  | Error _ as err -> err
  | Ok modules ->
      Ok
        ( scope,
          env |> Env.with_modules modules |> Env.add_bindings alias_bindings,
          next_type,
          Module_alias
            {
              alias_name = Names.module_segment_to_ocaml alias_name;
              target_name = Names.module_path_to_ocaml target_name;
            } )

let parse_type_parameters = Type_parameters.parse

let compile_module_signature scope env next_type signature_name item_forms =
  Module_signature_elaborator.compile scope env next_type signature_name item_forms

let compile_type_alias = Type_definition_elaborator.compile_type_alias
let compile_type_record = Type_definition_elaborator.compile_type_record
let record_type_public_binding =
  Type_definition_elaborator.record_type_public_binding
let compile_type_variant = Type_definition_elaborator.compile_type_variant

let variant_public_bindings module_path previous updated =
  let module_name = Names.module_path_to_ocaml module_path in
  changed_bindings previous updated
  |> List.map (fun (key, (binding : binding)) ->
         let constructor_name =
           match String.rindex_opt key '/' with
           | None -> key
           | Some index ->
               String.sub key (index + 1) (String.length key - index - 1)
         in
         ( key,
           { binding with
             ocaml_name = module_name ^ "." ^ constructor_name;
             ty = Types.qualify_module_type module_name binding.ty;
           } ))

let compile_module_apply scope env next_type module_name functor_name
    argument_names =
  let applied_bindings =
    Module_metadata.apply_functor_result_bindings env module_name functor_name
  in
  let module_id =
    Module_id.create ~owner:(if scope = "" then [] else [ scope ])
      ~name:module_name
  in
  match
    Module_registry.declare_module module_id Applied (Env.modules env)
  with
  | Error _ as err -> err
  | Ok modules ->
      (match
         Module_registry.apply_functor_aliases ~module_name ~functor_name modules
       with
      | Error _ as err -> err
      | Ok modules ->
          (match Module_metadata.apply_functor_types env module_name functor_name with
          | Error _ as err -> err
          | Ok types ->
              let protocols =
                Module_metadata.apply_functor_protocols env module_name functor_name
              in
              Ok
                ( scope,
                  env |> Env.with_modules modules |> Env.with_protocols protocols
                  |> Env.with_types types |> Env.add_bindings applied_bindings,
                  next_type,
                  Module_apply
                    {
                      module_name = Names.module_segment_to_ocaml module_name;
                      functor_name = Names.module_path_to_ocaml functor_name;
                      argument_names = List.map Names.module_path_to_ocaml argument_names;
                    } )))

let rec compile_module ?signature_name ?(register_module = true) scope env next_type module_path
    module_segment forms =
  let env = inherit_scope_ocaml_value_refers scope module_path env in
  let rec compile_module_form env public_bindings next_type items = function
    | FList (FSymbol "module-signature" :: FSymbol signature_name :: item_forms) -> (
        match compile_module_signature module_path env next_type signature_name item_forms with
        | Error _ as err -> err
        | Ok (_scope, env, next_type, item) ->
            Ok (env, public_bindings, next_type, item :: items))
    | FList (FSymbol "module-signature" :: _) ->
        Error.error "module-signature expects a name and signature items"
    | FList [ FSymbol "type-alias"; FSymbol name; FVector parameter_forms; manifest_form ] -> (
        match parse_type_parameters (FVector parameter_forms) with
        | Error _ as err -> err
        | Ok type_parameters -> (
            match
              compile_type_alias module_path env next_type name type_parameters
                manifest_form
            with
            | Error _ as err -> err
            | Ok (_scope, env, next_type, item) ->
                Ok (env, public_bindings, next_type, item :: items)))
    | FList [ FSymbol "type-alias"; FSymbol name; manifest_form ] -> (
        match compile_type_alias module_path env next_type name [] manifest_form with
        | Error _ as err -> err
        | Ok (_scope, env, next_type, item) ->
            Ok (env, public_bindings, next_type, item :: items))
    | FList
        (FSymbol "type-record" :: FSymbol name :: FVector parameter_forms
        :: field_forms) -> (
        match parse_type_parameters (FVector parameter_forms) with
        | Error _ as err -> err
        | Ok type_parameters -> (
            match
              compile_type_record module_path env next_type name type_parameters
                field_forms
            with
            | Error _ as err -> err
            | Ok (_scope, env, next_type, item) -> (
                match record_type_public_binding module_path name env with
                | Error _ as err -> err
                | Ok public_binding ->
                    Ok
                      ( env,
                        public_bindings @ [ public_binding ],
                        next_type,
                        item :: items ))))
    | FList (FSymbol "type-record" :: FSymbol name :: field_forms) -> (
        match compile_type_record module_path env next_type name [] field_forms with
        | Error _ as err -> err
        | Ok (_scope, env, next_type, item) -> (
            match record_type_public_binding module_path name env with
            | Error _ as err -> err
            | Ok public_binding ->
                Ok
                  ( env,
                    public_bindings @ [ public_binding ],
                    next_type,
                    item :: items )))
    | FList (FSymbol "type-record" :: _) ->
        Error.error "type-record expects a name and fields"
    | FList
        (FSymbol "type-variant" :: FSymbol name :: FVector parameter_forms
        :: constructor_forms) -> (
        match parse_type_parameters (FVector parameter_forms) with
        | Error _ as err -> err
        | Ok type_parameters -> (
            match
              compile_type_variant module_path env next_type name type_parameters
                constructor_forms
            with
            | Error _ as err -> err
            | Ok (_scope, updated_env, next_type, item) ->
                let exported = variant_public_bindings module_path env updated_env in
                Ok
                  ( updated_env,
                    public_bindings @ exported,
                    next_type,
                    item :: items )))
    | FList (FSymbol "type-variant" :: FSymbol name :: constructor_forms) -> (
        match compile_type_variant module_path env next_type name [] constructor_forms with
        | Error _ as err -> err
        | Ok (_scope, updated_env, next_type, item) ->
            let exported = variant_public_bindings module_path env updated_env in
            Ok
              ( updated_env,
                public_bindings @ exported,
                next_type,
                item :: items ))
    | FList [ FSymbol "open"; FSymbol opened_module ] ->
        let env = open_module_bindings module_path env opened_module in
        Ok
          ( env,
            public_bindings,
            next_type,
            Open_module (Names.module_path_to_ocaml opened_module) :: items )
    | FList [ FSymbol "include"; FSymbol included_module ] ->
        let included_public_bindings =
          include_module_public_bindings module_path env included_module
        in
        let env = open_module_bindings module_path env included_module in
        Ok
          ( env,
            public_bindings @ included_public_bindings,
            next_type,
            Include_module (Names.module_path_to_ocaml included_module) :: items )
    | FList (FSymbol "include" :: _) ->
        Error.error "include expects one module"
    | FList [ FSymbol "module-alias"; FSymbol alias_name; FSymbol target_name ] ->
        let target_path = resolve_module_target_path module_path env target_name in
        let public_alias_path = module_path ^ "." ^ alias_name in
        let public_alias_bindings =
          alias_module_bindings env public_alias_path target_path
        in
        (match
           compile_module_alias ~semantic_target:target_path module_path env next_type
             alias_name target_name
         with
        | Error _ as err -> err
        | Ok (_scope, env, next_type, item) ->
            Ok
              ( env,
                public_bindings @ public_alias_bindings,
                next_type,
                item :: items ))
    | FList (FSymbol "module-alias" :: _) ->
        Error.error "module-alias expects alias and target modules"
    | FList (FSymbol "defprotocol" :: FSymbol protocol_name :: method_forms) -> (
        match compile_defprotocol module_path env next_type protocol_name method_forms with
        | Error _ as err -> err
        | Ok (_scope, updated_env, next_type, item) ->
            let exported = changed_bindings env updated_env in
            Ok
              ( updated_env,
                public_bindings @ exported,
                next_type,
                item :: items ))
    | FList
        (FSymbol "extend-type" :: receiver_form :: FSymbol protocol_name
        :: method_forms) -> (
        match
          compile_extend_type module_path env next_type receiver_form protocol_name
            method_forms
        with
        | Error _ as err -> err
        | Ok (_scope, updated_env, next_type, item) ->
            let exported =
              changed_bindings env updated_env
              |> List.map (fun (key, (binding : binding)) ->
                     let qualified_ty =
                       Types.qualify_module_type
                         (Names.module_path_to_ocaml module_path)
                         binding.ty
                     in
                     let key =
                       match (String.rindex_opt key '/', qualified_ty) with
                       | Some separator, TFn (TNamed_record record :: _, _) ->
                           String.sub key 0 (separator + 1) ^ record.type_name
                       | _ -> key
                     in
                     ( key,
                       {
                         binding with
                         ocaml_name =
                           Names.module_path_to_ocaml module_path ^ "."
                           ^ binding.ocaml_name;
                         ty = qualified_ty;
                       } ))
            in
            Ok
              ( updated_env,
                public_bindings @ exported,
                next_type,
                item :: items ))
    | FList [ FSymbol "def"; FSymbol name; expr_form ] -> (
        match compile_expr module_path env expr_form with
        | Error _ as err -> err
        | Ok expr ->
            let local_name = Names.sanitize_name name in
            let key = module_binding_key module_path name in
            let local_binding = binding_of_expr local_name expr in
            let public_binding =
              Types.binding
                ?return_param_index:(expr.return_param_index)
                (module_binding_ocaml_name module_path name)
                (Types.qualify_module_type
                   (Names.module_path_to_ocaml module_path)
                   expr.ty)
            in
            (match check_emitted_name_collision env ~source_key:key ~ocaml_name:local_name with
            | Error _ as err -> err
            | Ok () -> (match expr.ty with
            | TRecord fields -> (
                match expr.record_values with
                | None -> Error.error "internal error: record expression missing values"
                | Some values ->
                    let type_name = "t" ^ string_of_int next_type in
                    let set_module_name = "Set_" ^ type_name in
                    let local_record_ty =
                      Types.named_record ~type_name ~set_module_name fields
                    in
                    let public_record_ty =
                      Types.named_record
                        ~type_name:(Names.module_path_to_ocaml module_path ^ "." ^ type_name)
                        ~set_module_name:
                          (Names.module_path_to_ocaml module_path ^ "." ^ set_module_name)
                        fields
                    in
                    let local_binding = Types.binding local_name local_record_ty in
                    let public_binding =
                      Types.binding (module_binding_ocaml_name module_path name)
                        public_record_ty
                    in
                    let item =
                      Record_def
                        { var_name = local_name;
                          type_name;
                          set_module_name;
                          fields;
                          values }
                    in
                    Ok
                      ( Env.add key local_binding env,
                        public_bindings @ [ (key, public_binding) ],
                        next_type + 1,
                        item :: items ))
            | _ ->
                let item =
                  Value_binding
                    { pattern = Named local_name; expression = expr.semantic_expr }
                in
                Ok
                  ( Env.add key local_binding env,
                    public_bindings @ [ (key, public_binding) ],
                    next_type,
                    item :: items ))))
    | FList
        (FSymbol "defn" :: ((FSymbol name) as name_form) :: params
        :: FKeyword return_keyword
        :: body_forms) -> (
        match Type_annotation.of_keyword return_keyword with
        | Error _ as err -> err
        | Ok return_ty ->
            let local_name = Names.sanitize_name name in
            (match
               prepare_recursive_fn ~ocaml_name:local_name module_path env name
                 return_ty params body_forms
             with
            | Error _ as err -> err
            | Ok parts ->
                let public_name = module_binding_ocaml_name module_path name in
                let param_tys =
                  parts.param_bindings
                  |> List.map (fun (_key, (binding : binding)) -> binding.ty)
                in
                let local_row_types = row_param_type_names local_name param_tys in
                let public_row_types = row_param_type_names public_name param_tys in
                let expr = fn_code ~row_param_type_names:local_row_types parts in
                let key = module_binding_key module_path name in
                (match
                   check_emitted_name_collision env ~source_key:key
                     ~ocaml_name:local_name
                 with
                | Error _ as err -> err
                | Ok () ->
                    let local_binding =
                      binding_of_expr ~row_param_types:local_row_types local_name
                        expr
                    in
                    let public_binding =
                      Types.binding ~row_param_types:public_row_types public_name
                        (Types.qualify_module_type
                           (Names.module_path_to_ocaml module_path)
                           expr.ty)
                    in
                    let type_items = row_type_items local_row_types param_tys in
                    let value_item =
                      Recursive_value_binding
                        { name = local_name;
                          identity =
                            Source_context.find name_form
                            |> Option.map (fun location ->
                                   (Source_node_id.of_location location, location));
                          expression = expr.semantic_expr;
                        }
                    in
                    Ok
                      ( Env.add key local_binding env,
                        public_bindings @ [ (key, public_binding) ],
                        next_type,
                        Group (type_items @ [ value_item ]) :: items ))))
    | FList (FSymbol "defn" :: FSymbol name :: params :: body_forms) -> (
        match prepare_fn module_path env params body_forms with
        | Error _ as err -> err
        | Ok parts -> (
            let local_name = Names.sanitize_name name in
            let public_name = module_binding_ocaml_name module_path name in
            let param_tys =
              parts.param_bindings
              |> List.map (fun (_key, (binding : binding)) -> binding.ty)
            in
            let local_row_types = row_param_type_names local_name param_tys in
            let public_row_types = row_param_type_names public_name param_tys in
            let expr = fn_code ~row_param_type_names:local_row_types parts in
            let key = module_binding_key module_path name in
            match
              check_emitted_name_collision env ~source_key:key ~ocaml_name:local_name
            with
            | Error _ as err -> err
            | Ok () -> (match expr.ty with
            | TFn _ ->
                let local_binding =
                  binding_of_expr ~row_param_types:local_row_types local_name expr
                in
                let public_binding =
                  Types.binding ~row_param_types:public_row_types
                    ?return_param_index:(expr.return_param_index) public_name
                    (Types.qualify_module_type
                       (Names.module_path_to_ocaml module_path)
                       expr.ty)
                in
                let type_items = row_type_items local_row_types param_tys in
                let value_item =
                  Value_binding
                    { pattern = Named local_name; expression = expr.semantic_expr }
                in
                Ok
                  ( Env.add key local_binding env,
                    public_bindings @ [ (key, public_binding) ],
                    next_type,
                    Group (type_items @ [ value_item ]) :: items )
            | _ -> Error.error "defn body did not compile to a function")))
    | FList
        (FSymbol "module" :: FSymbol nested_segment :: FSymbol nested_signature_name
        :: nested_forms) -> (
        let nested_path = module_path ^ "." ^ nested_segment in
        match
          compile_module ~signature_name:nested_signature_name scope env next_type
            nested_path nested_segment nested_forms
        with
        | Error _ as err -> err
        | Ok
            ( _scope,
              nested_env,
              nested_public_bindings,
              next_type,
              nested_item ) ->
            Ok
              ( Env.add_bindings nested_public_bindings nested_env,
                public_bindings @ nested_public_bindings,
                next_type,
                nested_item :: items ))
    | FList (FSymbol "module" :: FSymbol nested_segment :: nested_forms) -> (
        let nested_path = module_path ^ "." ^ nested_segment in
        match compile_module scope env next_type nested_path nested_segment nested_forms with
        | Error _ as err -> err
        | Ok
            ( _scope,
              nested_env,
              nested_public_bindings,
              next_type,
              nested_item ) ->
            Ok
              ( Env.add_bindings nested_public_bindings nested_env,
                public_bindings @ nested_public_bindings,
                next_type,
                nested_item :: items ))
    | _ ->
        Error.error
          "module forms must be module-signature, type-alias, type-record, type-variant, open, include, module-alias, defprotocol, extend-type, def, defn, or module"
  and loop env public_bindings next_type items = function
    | [] ->
        let module_name = Names.module_segment_to_ocaml module_segment in
        let protocols =
          Protocol_registry.qualify_implementations ~owner:[ module_path ]
            ~module_name:(Names.module_path_to_ocaml module_path)
            (Env.protocols env)
        in
        let env = Env.with_protocols protocols env in
        let modules =
          if register_module then
            Module_registry.declare_module (module_id_of_path module_path)
              Concrete (Env.modules env)
          else Ok (Env.modules env)
        in
        (match modules with
        | Error _ as err -> err
        | Ok modules ->
            Ok
              ( scope,
                Env.with_modules modules env,
                public_bindings,
                next_type,
                Module_def
                  {
                    module_name;
                    signature_name =
                      Option.map Names.module_path_to_ocaml signature_name;
                    items = List.rev items;
                  } ))
    | form :: rest -> (
        match compile_module_form env public_bindings next_type items form with
        | Error _ as err -> err
        | Ok (env, public_bindings, next_type, items) ->
            loop env public_bindings next_type items rest)
  in
  loop env [] next_type [] forms

let compile_module_functor scope env next_type functor_name parameter_form
    body_forms =
  let rec parse_parameters acc = function
    | [] -> Ok (List.rev acc)
    | FSymbol parameter_name :: FSymbol parameter_signature :: rest ->
        parse_parameters
          ((parameter_name, parameter_signature) :: acc)
          rest
    | [ _ ] ->
        Error.error "module-functor parameters must be name/signature pairs"
    | _ -> Error.error "module-functor parameters must be symbols"
  in
  match parameter_form with
  | FVector [] -> Error.error "module-functor parameter vector must not be empty"
  | FVector parameter_forms -> (
      match parse_parameters [] parameter_forms with
      | Error _ as err -> err
      | Ok parameters ->
          let rec collect_parameter_bindings bindings = function
            | [] -> Ok (List.rev bindings |> List.concat)
            | (parameter_name, parameter_signature) :: rest -> (
                match
                  Module_metadata.signature_parameter_bindings env parameter_name
                    ~scope parameter_signature
                with
                | Error _ as err -> err
                | Ok parameter_bindings ->
                    collect_parameter_bindings
                      (parameter_bindings :: bindings) rest)
          in
          (match collect_parameter_bindings [] parameters with
          | Error _ as err -> err
          | Ok parameter_bindings ->
          let functor_env = Env.add_bindings parameter_bindings env in
          (match
             compile_module ~register_module:false scope functor_env next_type functor_name
               functor_name body_forms
           with
          | Error _ as err -> err
          | Ok (_scope, module_env, public_bindings, next_type, module_item) -> (
              match module_item with
              | Module_def { items; _ } ->
                  let functor_id =
                    Functor_id.create
                      ~owner:(if scope = "" then [] else [ scope ])
                      ~name:functor_name
                  in
                  let modules =
                    Module_registry.declare_module
                      (Module_id.create
                         ~owner:(if scope = "" then [] else [ scope ])
                         ~name:functor_name)
                      Functor (Env.modules env)
                  in
                  (match modules with
                  | Error _ as err -> err
                  | Ok modules ->
                      let modules =
                        Module_registry.store_functor_result functor_id
                          public_bindings modules
                      in
                      let modules =
                        Module_registry.store_functor_protocols functor_id
                          (Env.protocols module_env) modules
                      in
                      let modules =
                        Module_registry.store_functor_types functor_id
                          (Env.types module_env) modules
                      in
                      let modules =
                        Module_registry.store_functor_aliases functor_id
                          (Env.modules module_env) modules
                      in
                      Ok
                        ( scope,
                          Env.with_modules modules env,
                          next_type,
                          Module_functor
                            {
                              functor_name =
                                Names.module_segment_to_ocaml functor_name;
                              parameters =
                                List.map
                                  (fun (name, signature) ->
                                    ( Names.module_segment_to_ocaml name,
                                      Names.module_path_to_ocaml signature ))
                                  parameters;
                              items;
                            } ))
              | _ ->
                  Error.error
                    "internal error: module functor body did not compile"))))
  | _ ->
      Error.error
        "module-functor expects a name, [parameter signature ...], and body"
