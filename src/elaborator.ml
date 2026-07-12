open Ast
open Types
open Lowered

module Env = Compiler_environment

let compile_expr = Expression_elaborator.compile_expr
let prepare_fn = Expression_elaborator.prepare_fn
let fn_code = Expression_elaborator.fn_code
let compile_fn = Expression_elaborator.compile_fn
let compile_args_for = Expression_elaborator.compile_args_for
let compile_call = Expression_elaborator.compile_call
let binding_of_expr = Expression_elaborator.binding_of_expr
let row_param_type_names = Expression_elaborator.row_param_type_names
let row_type_items = Expression_elaborator.row_type_items
let check_emitted_name_collision = Resolver.check_emitted_name_collision
let lookup_record_type = Resolver.lookup_record_type
let record_type_key = Resolver.record_type_key
let inherit_scope_ocaml_value_refers =
  Expression_elaborator.inherit_scope_ocaml_value_refers

let compile_defprotocol scope env next_type protocol_name method_forms =
  match Protocol_elaborator.define scope env protocol_name method_forms with
  | Error _ as err -> err
  | Ok (env, item) -> Ok (scope, env, next_type, item)

let protocol_receiver_type scope env = function
  | FKeyword receiver_keyword -> Type_annotation.of_keyword receiver_keyword
  | FSymbol type_name ->
      lookup_record_type scope env type_name
      |> Result.map (fun record -> TNamed_record record)
  | _ -> Error.error "extend-type receiver must be a type keyword or record type"

let compile_extend_type scope env next_type receiver_form protocol_name method_forms =
  match protocol_receiver_type scope env receiver_form with
  | Error _ as err -> err
  | Ok receiver_ty ->
      let compile_method env = function
        | FList (FSymbol method_name :: params :: body_forms) -> (
            match Protocol_elaborator.marker scope env protocol_name method_name with
            | Error _ as err -> err
            | Ok marker -> (
                match Protocol.annotate_receiver receiver_ty params with
                | Error _ as err -> err
                | Ok params -> (
                    let param_type_overrides =
                      match receiver_ty with
                      | TNamed_record _ -> [ Some receiver_ty ]
                      | _ -> []
                    in
                    match
                      compile_fn ~param_type_overrides scope env params body_forms
                    with
                    | Error _ as err -> err
                    | Ok expr -> (
                        match (marker.ty, expr.ty) with
                        | TFn (expected_params, _), TFn (actual_params, _)
                          when List.length expected_params <> List.length actual_params ->
                            Error.error (method_name ^ " called with incompatible arguments")
                        | TFn (expected_params, expected_ret),
                          TFn (actual_params, actual_ret)
                          -> (
                            match actual_params with
                            | [] ->
                                Error.error
                                  "protocol methods must have a receiver parameter"
                            | actual_receiver :: _ ->
                                if not (Types.equal receiver_ty actual_receiver) then
                                  Error.error
                                    ("protocol implementation receiver must be "
                                   ^ source_name receiver_ty)
                                else
                                  let mismatch =
                                    List.combine expected_params actual_params
                                    |> List.mapi (fun index (expected, actual) ->
                                           (index, expected, actual))
                                    |> List.find_opt
                                         (fun (_index, expected, actual) ->
                                           not
                                             (Types.assignable ~expected
                                                ~actual))
                                  in
                                  (match mismatch with
                                  | Some (index, expected, _actual) ->
                                      Error.error
                                        ("protocol method " ^ method_name ^ " parameter "
                                       ^ string_of_int (index + 1) ^ " must be "
                                       ^ source_name expected)
                                  | None
                                    when not
                                           (Types.assignable ~expected:expected_ret
                                              ~actual:actual_ret) ->
                                  Error.error
                                    ("protocol method " ^ method_name ^ " must return "
                                   ^ source_name expected_ret)
                                  | None -> (
                                  let ocaml_name =
                                    Protocol.impl_ocaml_name scope protocol_name
                                      method_name receiver_ty
                                  in
                                  let binding = binding_of_expr ocaml_name expr in
                                  (match
                                     Protocol_elaborator.add_implementation scope env
                                       protocol_name method_name receiver_ty marker binding
                                   with
                                  | Error _ as err -> err
                                  | Ok env ->
                                      Ok
                                        ( env,
                                          Value_binding
                                            {
                                              pattern = Named ocaml_name;
                                              expression = expr.ocaml_expr;
                                            } )))))
                        | _ -> Error.error "protocol method did not compile to a function"))))
        | _ -> Error.error "extend-type methods must be (method-name [params] body)"
      in
      let rec loop env items = function
        | [] ->
            Ok
              ( scope,
                env,
                next_type,
                Group (List.rev items) )
        | method_form :: rest -> (
            match compile_method env method_form with
            | Error _ as err -> err
            | Ok (env, item) -> loop env (item :: items) rest)
      in
      loop env [] method_forms

let module_binding_key = Module_environment.binding_key
let module_binding_ocaml_name = Module_environment.binding_ocaml_name
let changed_bindings = Module_environment.changed_bindings
let open_module_bindings = Module_environment.open_bindings
let include_module_public_bindings = Module_environment.include_public_bindings
let alias_module_bindings = Module_environment.alias_bindings

let compile_module_alias scope env next_type alias_name target_name =
  let alias_bindings = alias_module_bindings env alias_name target_name in
  Ok
    ( scope,
      Env.add_bindings alias_bindings env,
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

let compile_module_apply scope env next_type module_name functor_name
    argument_names =
  let applied_bindings =
    Module_metadata.apply_functor_result_bindings env module_name functor_name
  in
  Ok
    ( scope,
      Env.add_bindings applied_bindings env,
      next_type,
      Module_apply
        {
          module_name = Names.module_segment_to_ocaml module_name;
          functor_name = Names.module_path_to_ocaml functor_name;
          argument_names = List.map Names.module_path_to_ocaml argument_names;
        } )

let rec compile_module ?signature_name scope env next_type module_path
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
            | Ok (_scope, _env, next_type, item) ->
                Ok (env, public_bindings, next_type, item :: items)))
    | FList [ FSymbol "type-alias"; FSymbol name; manifest_form ] -> (
        match compile_type_alias module_path env next_type name [] manifest_form with
        | Error _ as err -> err
        | Ok (_scope, _env, next_type, item) ->
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
            | Ok (_scope, _env, next_type, item) ->
                Ok (env, public_bindings, next_type, item :: items)))
    | FList (FSymbol "type-variant" :: FSymbol name :: constructor_forms) -> (
        match compile_type_variant module_path env next_type name [] constructor_forms with
        | Error _ as err -> err
        | Ok (_scope, _env, next_type, item) ->
            Ok (env, public_bindings, next_type, item :: items))
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
        let local_alias_bindings =
          alias_module_bindings env alias_name target_name
        in
        let public_alias_path = module_path ^ "." ^ alias_name in
        let public_alias_bindings =
          alias_module_bindings env public_alias_path target_name
        in
        Ok
          ( Env.add_bindings local_alias_bindings env,
            public_bindings @ public_alias_bindings,
            next_type,
            Module_alias
              {
                alias_name = Names.module_segment_to_ocaml alias_name;
                target_name = Names.module_path_to_ocaml target_name;
              }
            :: items )
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
                    { pattern = Named local_name; expression = expr.ocaml_expr }
                in
                Ok
                  ( Env.add key local_binding env,
                    public_bindings @ [ (key, public_binding) ],
                    next_type,
                    item :: items ))))
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
                    { pattern = Named local_name; expression = expr.ocaml_expr }
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
        | Ok (_scope, nested_public_bindings, next_type, nested_item) ->
            Ok
              ( Env.add_bindings nested_public_bindings env,
                public_bindings @ nested_public_bindings,
                next_type,
                nested_item :: items ))
    | FList (FSymbol "module" :: FSymbol nested_segment :: nested_forms) -> (
        let nested_path = module_path ^ "." ^ nested_segment in
        match compile_module scope env next_type nested_path nested_segment nested_forms with
        | Error _ as err -> err
        | Ok (_scope, nested_public_bindings, next_type, nested_item) ->
            Ok
              ( Env.add_bindings nested_public_bindings env,
                public_bindings @ nested_public_bindings,
                next_type,
                nested_item :: items ))
    | _ ->
        Error.error
          "module forms must be module-signature, type-alias, type-record, type-variant, open, include, module-alias, defprotocol, extend-type, def, defn, or module"
  and loop env public_bindings next_type items = function
    | [] ->
        let module_name = Names.module_segment_to_ocaml module_segment in
        Ok
          ( scope,
            public_bindings,
            next_type,
            Module_def
              {
                module_name;
                signature_name =
                  Option.map Names.module_path_to_ocaml signature_name;
                items = List.rev items;
              } )
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
          (( parameter_name,
             Names.module_path_to_ocaml parameter_signature )
          :: acc)
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
          let parameter_bindings =
            parameters
            |> List.concat_map (fun (parameter_name, parameter_signature) ->
                   Module_metadata.signature_parameter_bindings env parameter_name
                     parameter_signature)
          in
          let functor_env = Env.add_bindings parameter_bindings env in
          (match
             compile_module scope functor_env next_type functor_name
               functor_name body_forms
           with
          | Error _ as err -> err
          | Ok (_scope, public_bindings, next_type, module_item) -> (
              match module_item with
              | Module_def { items; _ } ->
                  let functor_bindings =
                    Module_metadata.store_functor_result_bindings functor_name
                      public_bindings
                  in
                  Ok
                    ( scope,
                      Env.add_bindings functor_bindings env,
                      next_type,
                      Module_functor
                        {
                          functor_name =
                            Names.module_segment_to_ocaml functor_name;
                          parameters =
                            List.map
                              (fun (name, signature) ->
                                ( Names.module_segment_to_ocaml name,
                                  signature ))
                              parameters;
                          items;
                        } )
              | _ ->
                  Error.error
                    "internal error: module functor body did not compile")))
  | _ ->
      Error.error
        "module-functor expects a name, [parameter signature ...], and body"

let compile_top_level scope env next_type = function
  | FList (FSymbol "module-signature" :: FSymbol signature_name :: item_forms) ->
      compile_module_signature scope env next_type signature_name item_forms
  | FList (FSymbol "module-signature" :: _) ->
      Error.error "module-signature expects a name and signature items"
  | FList [ FSymbol "type-alias"; FSymbol name; manifest_form ] ->
      compile_type_alias scope env next_type name [] manifest_form
  | FList [ FSymbol "type-alias"; FSymbol name; FVector parameter_forms; manifest_form ] -> (
      match parse_type_parameters (FVector parameter_forms) with
      | Error _ as err -> err
      | Ok type_parameters ->
          compile_type_alias scope env next_type name type_parameters manifest_form)
  | FList
      (FSymbol "type-record" :: FSymbol name :: FVector parameter_forms
      :: field_forms) -> (
      match parse_type_parameters (FVector parameter_forms) with
      | Error _ as err -> err
      | Ok type_parameters ->
          compile_type_record scope env next_type name type_parameters field_forms)
  | FList (FSymbol "type-record" :: FSymbol name :: field_forms) ->
      compile_type_record scope env next_type name [] field_forms
  | FList (FSymbol "type-record" :: _) ->
      Error.error "type-record expects a name and fields"
  | FList
      (FSymbol "type-variant" :: FSymbol name :: FVector parameter_forms
      :: constructor_forms) -> (
      match parse_type_parameters (FVector parameter_forms) with
      | Error _ as err -> err
      | Ok type_parameters ->
          compile_type_variant scope env next_type name type_parameters constructor_forms)
  | FList (FSymbol "type-variant" :: FSymbol name :: constructor_forms) ->
      compile_type_variant scope env next_type name [] constructor_forms
  | FList [ FSymbol "open"; FSymbol module_path ] ->
      let env = open_module_bindings scope env module_path in
      Ok
        ( scope,
          env,
          next_type,
          Open_module (Names.module_path_to_ocaml module_path) )
  | FList [ FSymbol "include"; FSymbol module_path ] ->
      let env = open_module_bindings scope env module_path in
      Ok
        ( scope,
          env,
          next_type,
          Include_module (Names.module_path_to_ocaml module_path) )
  | FList (FSymbol "include" :: _) ->
      Error.error "include expects one module"
  | FList [ FSymbol "module-alias"; FSymbol alias_name; FSymbol target_name ] ->
      compile_module_alias scope env next_type alias_name target_name
  | FList (FSymbol "module-alias" :: _) ->
      Error.error "module-alias expects alias and target modules"
  | FList
      (FSymbol "module-functor" :: FSymbol functor_name :: parameter_form
      :: body_forms) ->
      compile_module_functor scope env next_type functor_name parameter_form
        body_forms
  | FList (FSymbol "module-functor" :: _) ->
      Error.error
        "module-functor expects a name, [parameter signature ...], and body"
  | FList
      (FSymbol "module-apply" :: FSymbol module_name :: FSymbol functor_name
      :: (_ :: _ as argument_forms)) ->
      let rec parse_arguments acc = function
        | [] -> Ok (List.rev acc)
        | FSymbol name :: rest -> parse_arguments (name :: acc) rest
        | _ ->
            Error.error
              "module-apply expects result, functor, and one or more argument modules"
      in
      (match parse_arguments [] argument_forms with
      | Error _ as err -> err
      | Ok argument_names ->
          compile_module_apply scope env next_type module_name functor_name
            argument_names)
  | FList (FSymbol "module-apply" :: _) ->
      Error.error
        "module-apply expects result, functor, and one or more argument modules"
  | FList [ FSymbol "def"; FSymbol name; expr_form ] -> (
      match compile_expr scope env expr_form with
      | Error _ as err -> err
      | Ok expr ->
          let ocaml_name = Names.ocaml_binding_name scope name in
          let env_key = Names.scoped_key scope name in
          (match check_emitted_name_collision env ~source_key:env_key ~ocaml_name with
          | Error _ as err -> err
          | Ok () -> (match expr.ty with
          | TRecord fields -> (
              match expr.record_values with
              | None -> Error.error "internal error: record expression missing values"
              | Some values ->
                  let type_name = "t" ^ string_of_int next_type in
                  let set_module_name = "Set_" ^ type_name in
                  let binding =
                    Types.binding ocaml_name
                      (Types.named_record ~type_name ~set_module_name fields)
                  in
                  Ok
                    ( scope,
                      Env.add env_key binding env,
                      next_type + 1,
                      Record_def
                        { var_name = ocaml_name;
                          type_name;
                          set_module_name;
                          fields;
                          values } ))
          | _ ->
              let binding = binding_of_expr ocaml_name expr in
              Ok
                ( scope,
                  Env.add env_key binding env,
                  next_type,
                  Value_binding
                    { pattern = Named ocaml_name; expression = expr.ocaml_expr } ))))
  | FList (FSymbol "defn" :: FSymbol name :: params :: body_forms) -> (
      match prepare_fn scope env params body_forms with
      | Error _ as err -> err
      | Ok parts -> (
          let ocaml_name = Names.ocaml_binding_name scope name in
          let param_tys =
            parts.param_bindings
            |> List.map (fun (_key, (binding : binding)) -> binding.ty)
          in
          let row_param_types = row_param_type_names ocaml_name param_tys in
          let expr = fn_code ~row_param_type_names:row_param_types parts in
          let env_key = Names.scoped_key scope name in
          match check_emitted_name_collision env ~source_key:env_key ~ocaml_name with
          | Error _ as err -> err
          | Ok () -> (match expr.ty with
          | TFn _ ->
              let binding = binding_of_expr ~row_param_types ocaml_name expr in
              let type_items = row_type_items row_param_types param_tys in
              let value_item =
                Value_binding
                  { pattern = Named ocaml_name; expression = expr.ocaml_expr }
              in
              Ok
                ( scope,
                  Env.add env_key binding env,
                  next_type,
                  Group (type_items @ [ value_item ]) )
          | _ -> Error.error "defn body did not compile to a function")))
  | FList (FSymbol "defprotocol" :: FSymbol protocol_name :: method_forms) ->
      compile_defprotocol scope env next_type protocol_name method_forms
  | FList
      (FSymbol "extend-type" :: receiver_form :: FSymbol protocol_name
      :: method_forms) ->
      compile_extend_type scope env next_type receiver_form protocol_name
        method_forms
  | FList (FSymbol "module" :: FSymbol module_name :: FSymbol signature_name :: forms) -> (
      match
        compile_module ~signature_name scope env next_type module_name module_name
          forms
      with
      | Error _ as err -> err
      | Ok (scope, module_bindings, next_type, item) ->
          Ok (scope, Env.add_bindings module_bindings env, next_type, item))
  | FList (FSymbol "module" :: FSymbol module_name :: forms) -> (
      match compile_module scope env next_type module_name module_name forms with
      | Error _ as err -> err
      | Ok (scope, module_bindings, next_type, item) ->
          Ok (scope, Env.add_bindings module_bindings env, next_type, item))
  | FList (FSymbol (("print" | "println") as name) :: args) -> (
      match compile_call scope env name args with
      | Error _ as err -> err
      | Ok expr ->
          Ok
            ( scope,
              env,
              next_type,
              Value_binding
                { pattern = Unit_pattern; expression = expr.ocaml_expr } ))
  | FList (FSymbol "require" :: entries) -> (
      match Require.parse_entries entries with
      | Error _ as err -> err
      | Ok specs ->
          let rec apply_specs env = function
            | [] -> Ok env
            | Require.Package _ :: rest -> apply_specs env rest
            | Require.Alias { module_name; alias } :: rest ->
                if String.starts_with ~prefix:"ocaml." module_name then
                  apply_specs
                    (Require.add_ocaml_alias_bindings env module_name alias)
                    rest
                else if module_name = "clojure.string" then
                  apply_specs
                    (Require.add_clojure_string_alias_bindings env alias)
                    rest
                else
                  Error.error
                    "require only accepts OCaml packages, OCaml modules, and clojure.string"
            | Require.Refer { module_name; names } :: rest ->
                let result =
                  if String.starts_with ~prefix:"ocaml." module_name then
                    Require.add_ocaml_refer_bindings env scope module_name names
                  else if module_name = "clojure.string" then
                    Require.add_clojure_string_refer_bindings env scope names
                  else
                    Error.error
                      "require only accepts OCaml packages, OCaml modules, and clojure.string"
                in
                (match result with
                | Error _ as err -> err
                | Ok env -> apply_specs env rest)
          in
          (match apply_specs env specs with
          | Error _ as err -> err
          | Ok env -> Ok (scope, env, next_type, Comment "require")))
  | (FList (FSymbol "loop" :: _) as form) -> (
      match compile_expr scope env form with
      | Error _ as err -> err
      | Ok expr ->
          Ok
            ( scope,
              env,
              next_type,
              Value_binding
                { pattern = Ignore_pattern; expression = expr.ocaml_expr } ))
  | FList (FSymbol "recur" :: _) ->
      Error.error "recur is only valid in a loop tail position"
  | form -> (
      match compile_expr scope env form with
      | Error _ as err -> err
      | Ok expr -> (
          match expr.record_values with
          | Some _ -> Error.error "top-level map literals must be bound with def"
          | None ->
              Ok
                ( scope,
                  env,
                  next_type,
                  Value_binding
                    { pattern = Ignore_pattern; expression = expr.ocaml_expr } )))

type state = Compiler_state.t

let empty_state = Compiler_state.empty

let compile_forms_incremental (state : Compiler_state.t) forms =
  let rec loop env next_type items = function
    | [] -> Ok (env, next_type, List.rev items)
    | form :: rest -> (
        match compile_top_level "" env next_type form with
        | Error _ as err -> err
        | Ok (_scope, env, next_type, item) ->
            loop env next_type (item :: items) rest)
  in
  match loop state.env state.next_type [] forms with
  | Error _ as err -> err
  | Ok (env, next_type, new_items) ->
      let next_state =
        {
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
