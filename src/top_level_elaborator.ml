open Ast
open Types
open Lowered

module Env = Compiler_environment

let compile_expr = Expression_elaborator.compile_expr
let prepare_fn = Expression_elaborator.prepare_fn
let prepare_recursive_fn = Expression_elaborator.prepare_recursive_fn
let prepare_inferred_recursive_fn =
  Expression_elaborator.prepare_inferred_recursive_fn
let fn_code = Expression_elaborator.fn_code
let compile_fn = Expression_elaborator.compile_fn
let compile_args_for = Expression_elaborator.compile_args_for
let compile_call = Expression_elaborator.compile_call
let binding_of_expr = Expression_support.binding_of_expr
let allocate_anonymous_record = Expression_support.allocate_anonymous_record
let row_param_type_names = Expression_support.row_param_type_names
let row_type_items = Expression_support.row_type_items
let check_emitted_name_collision = Resolver.check_emitted_name_collision
let unresolved_contextual_type = Expression_support.unresolved_contextual_type
let lookup_record_type = Resolver.lookup_record_type
let record_type_key = Resolver.record_type_key
let inherit_scope_ocaml_value_refers =
  Expression_support.inherit_scope_ocaml_value_refers

let rec form_mentions_symbol name = function
  | FSymbol candidate -> candidate = name
  | FList forms | FVector forms ->
      List.exists (form_mentions_symbol name) forms
  | FMap pairs ->
      List.exists
        (fun (key, value) ->
          form_mentions_symbol name key || form_mentions_symbol name value)
        pairs
  | FInt _ | FFloat _ | FChar _ | FString _ | FRegex _ | FBool _
  | FKeyword _ ->
      false

let compile_defprotocol = Protocol_elaborator.compile_defprotocol
let compile_extend_type = Protocol_elaborator.compile_extend_type

let located_value_pattern form pattern =
  match Source_context.find form with
  | None -> pattern
  | Some location ->
      Located_value (Source_node_id.of_location location, location, pattern)

let compile_module_alias = Module_elaborator.compile_module_alias
let compile_module_signature = Module_signature_elaborator.compile
let compile_module_apply = Module_elaborator.compile_module_apply
let compile_module = Module_elaborator.compile_module
let compile_module_functor = Module_elaborator.compile_module_functor
let open_module_bindings = Module_environment.open_bindings
let parse_type_parameters = Type_parameters.parse
let compile_type_alias = Type_definition_elaborator.compile_type_alias
let compile_type_record = Type_definition_elaborator.compile_type_record
let compile_type_variant = Type_definition_elaborator.compile_type_variant

let rec compile scope env next_type = function
  | FList
      (FSymbol "deftype" :: ((FSymbol name) as name_form) :: FVector raw_fields
      :: _interface_forms) ->
      let rec field_names acc = function
        | [] -> Ok (List.rev acc)
        | FSymbol metadata :: rest
          when String.starts_with ~prefix:"^" metadata ->
            field_names acc rest
        | FSymbol field_name :: rest -> field_names (field_name :: acc) rest
        | _ -> Error.error "deftype fields must be symbols"
      in
      Result.bind (field_names [] raw_fields) (fun fields ->
          if fields = [] then Error.error "deftype expects at least one field"
          else
            let type_parameters =
              List.mapi (fun index _ -> "field" ^ string_of_int index) fields
            in
            let field_forms =
              List.map2
                (fun field_name parameter ->
                  FList
                    [ FSymbol field_name;
                      FKeyword (":" ^ parameter);
                    ])
                fields type_parameters
            in
            compile_type_record ?location:(Source_context.find name_form) scope env
              next_type name type_parameters field_forms)
  | FList
      (FSymbol "deftype-methods" :: FSymbol type_name :: interface_forms) -> (
      match Resolver.lookup_record_type scope env type_name with
      | Error _ as err -> err
      | Ok record ->
          let receiver_ty = TNamed_record record in
          let rec compile_methods env items current_interface = function
            | [] -> Ok (scope, env, next_type, Group (List.rev items))
            | FSymbol interface_name :: rest ->
                compile_methods env items (Some interface_name) rest
            | FList
                (FSymbol method_name :: (FVector params as params_form)
                :: body_forms)
              :: rest ->
                if current_interface = Some "IPrintWithWriter" then
                  compile_methods env items current_interface rest
                else
                let arity = List.length params in
                let source_name =
                  Expression_support.deftype_method_name record method_name arity
                in
                let ocaml_name = Names.sanitize_name source_name in
                let receiver_name, params_form =
                  match params with
                  | FSymbol "_" :: remaining ->
                      let receiver_name = "__lg_deftype_this" in
                      ( receiver_name,
                        FVector (FSymbol receiver_name :: remaining) )
                  | FSymbol receiver_name :: _ -> (receiver_name, params_form)
                  | _ -> ("__lg_deftype_this", params_form)
                in
                let rec form_mentions name = function
                  | FSymbol candidate -> candidate = name
                  | FList forms | FVector forms ->
                      List.exists (form_mentions name) forms
                  | FMap pairs ->
                      List.exists
                        (fun (key, value) ->
                          form_mentions name key || form_mentions name value)
                        pairs
                  | FInt _ | FFloat _ | FChar _ | FString _ | FRegex _
                  | FBool _ | FKeyword _ ->
                      false
                in
                let field_bindings =
                  record.fields
                  |> List.filter (fun (field : field) ->
                         let source_name =
                           Names.keyword_source_name field.keyword
                         in
                         List.exists (form_mentions source_name) body_forms)
                  |> List.concat_map (fun (field : field) ->
                         let source_name =
                           Names.keyword_source_name field.keyword
                         in
                         [ FSymbol source_name;
                           FList
                             [ FSymbol (".-" ^ source_name);
                               FSymbol receiver_name;
                             ];
                         ])
                in
                let body_forms =
                  [ FList
                      (FSymbol "let" :: FVector field_bindings :: body_forms)
                  ]
                in
                (match
                   Expression_elaborator.compile_fn
                     ~param_type_overrides:[ Some receiver_ty ] scope env
                     params_form body_forms
                 with
                | Error _ as err -> err
                | Ok implementation ->
                    let binding = binding_of_expr ocaml_name implementation in
                    let env =
                      Env.add (Names.scoped_key scope source_name) binding env
                    in
                    let item =
                      Value_binding
                        { pattern = Named ocaml_name;
                          expression = implementation.semantic_expr;
                        }
                    in
                    compile_methods env (item :: items) current_interface rest)
            | _ :: _ ->
                Error.error
                  "deftype methods must be (method-name [params] body...)"
          in
          compile_methods env [] None interface_forms)
  | FList (FSymbol "defn-group" :: definitions) ->
      let rec compile_definitions env row_items bindings = function
        | [] ->
            Ok
              ( scope,
                env,
                next_type,
                Group
                  (List.rev row_items
                  @ [ Recursive_value_bindings (List.rev bindings) ]) )
        | FList
            (FSymbol ("defn" | "defn-") :: ((FSymbol name) as name_form)
            :: params :: body_forms)
          :: rest -> (
            match prepare_fn scope env params body_forms with
            | Error _ as err -> err
            | Ok parts ->
                let ocaml_name = Names.ocaml_binding_name scope name in
                let param_tys =
                  parts.param_bindings
                  |> List.map (fun (_key, (binding : binding)) -> binding.ty)
                in
                let row_param_types =
                  row_param_type_names ocaml_name param_tys
                in
                let expr =
                  fn_code ~row_param_type_names:row_param_types parts
                in
                let binding =
                  binding_of_expr ~row_param_types ocaml_name expr
                in
                let env =
                  Env.add (Names.scoped_key scope name) binding env
                in
                let rows = row_type_items row_param_types param_tys in
                let recursive_binding =
                  { name = ocaml_name;
                    identity =
                      Source_context.find name_form
                      |> Option.map (fun location ->
                             (Source_node_id.of_location location, location));
                    expression = expr.semantic_expr;
                  }
                in
                compile_definitions env
                  (List.rev_append rows row_items)
                  (recursive_binding :: bindings) rest)
        | _ :: _ ->
            Error.error "defn-group only supports function definitions"
      in
      compile_definitions env [] [] definitions
  | FList
      (FSymbol "module-signature" :: ((FSymbol signature_name) as name_form)
      :: item_forms) ->
      compile_module_signature ?location:(Source_context.find name_form) scope env
        next_type signature_name item_forms
  | FList (FSymbol "module-signature" :: _) ->
      Error.error "module-signature expects a name and signature items"
  | FList [ FSymbol "type-alias"; ((FSymbol name) as name_form); manifest_form ] ->
      compile_type_alias ?location:(Source_context.find name_form) scope env next_type
        name [] manifest_form
  | FList
      [ FSymbol "type-alias";
        ((FSymbol name) as name_form);
        FVector parameter_forms;
        manifest_form ] -> (
      match parse_type_parameters (FVector parameter_forms) with
      | Error _ as err -> err
      | Ok type_parameters ->
          compile_type_alias ?location:(Source_context.find name_form) scope env
            next_type name type_parameters manifest_form)
  | FList
      (FSymbol "type-record" :: ((FSymbol name) as name_form)
      :: FVector parameter_forms
      :: field_forms) -> (
      match parse_type_parameters (FVector parameter_forms) with
      | Error _ as err -> err
      | Ok type_parameters ->
          compile_type_record ?location:(Source_context.find name_form) scope env
            next_type name type_parameters field_forms)
  | FList
      (FSymbol "type-record" :: ((FSymbol name) as name_form) :: field_forms) ->
      compile_type_record ?location:(Source_context.find name_form) scope env next_type
        name [] field_forms
  | FList (FSymbol "type-record" :: _) ->
      Error.error "type-record expects a name and fields"
  | FList
      (FSymbol "type-variant" :: ((FSymbol name) as name_form)
      :: FVector parameter_forms
      :: constructor_forms) -> (
      match parse_type_parameters (FVector parameter_forms) with
      | Error _ as err -> err
      | Ok type_parameters ->
          compile_type_variant ?location:(Source_context.find name_form) scope env
            next_type name type_parameters constructor_forms)
  | FList
      (FSymbol "type-variant" :: ((FSymbol name) as name_form)
      :: constructor_forms) ->
      compile_type_variant ?location:(Source_context.find name_form) scope env
        next_type name [] constructor_forms
  | FList [ FSymbol "open"; ((FSymbol module_path) as module_form) ] ->
      let env = open_module_bindings scope env module_path in
      Ok
        ( scope,
          env,
          next_type,
          Open_module
            { module_name = Names.module_path_to_ocaml module_path;
              location = Source_context.find module_form } )
  | FList [ FSymbol "include"; ((FSymbol module_path) as module_form) ] ->
      let env = open_module_bindings scope env module_path in
      Ok
        ( scope,
          env,
          next_type,
          Include_module
            { module_name = Names.module_path_to_ocaml module_path;
              location = Source_context.find module_form } )
  | FList (FSymbol "include" :: _) ->
      Error.error "include expects one module"
  | FList
      [ FSymbol "module-alias";
        ((FSymbol alias_name) as alias_form);
        ((FSymbol target_name) as target_form) ] ->
      compile_module_alias ?location:(Source_context.find alias_form)
        ?target_location:(Source_context.find target_form) scope env next_type
        alias_name target_name
  | FList (FSymbol "module-alias" :: _) ->
      Error.error "module-alias expects alias and target modules"
  | FList
      (FSymbol "module-functor" :: ((FSymbol functor_name) as name_form)
      :: parameter_form
      :: body_forms) ->
      compile_module_functor ?location:(Source_context.find name_form) scope env
        next_type functor_name parameter_form body_forms
  | FList (FSymbol "module-functor" :: _) ->
      Error.error
        "module-functor expects a name, [parameter signature ...], and body"
  | FList
      (FSymbol "module-apply" :: ((FSymbol module_name) as name_form)
      :: ((FSymbol functor_name) as functor_form)
      :: (_ :: _ as argument_forms)) ->
      let rec parse_arguments acc = function
        | [] -> Ok (List.rev acc)
        | ((FSymbol name) as form) :: rest ->
            parse_arguments
              ({ module_name = name; location = Source_context.find form } :: acc)
              rest
        | _ ->
            Error.error
              "module-apply expects result, functor, and one or more argument modules"
      in
      (match parse_arguments [] argument_forms with
      | Error _ as err -> err
      | Ok argument_names ->
          compile_module_apply ?location:(Source_context.find name_form)
            ?functor_location:(Source_context.find functor_form) scope env next_type
            module_name functor_name argument_names)
  | FList (FSymbol "module-apply" :: _) ->
      Error.error
        "module-apply expects result, functor, and one or more argument modules"
  | FList
      [ FSymbol ("def" | "defonce"); ((FSymbol name) as name_form); expr_form ] -> (
      match compile_expr scope env expr_form with
      | Error _ as err -> err
      | Ok expr when unresolved_contextual_type expr.ty ->
          Error.error "empty list requires a contextual element type"
      | Ok expr ->
          let ocaml_name = Names.ocaml_binding_name scope name in
          let env_key = Names.scoped_key scope name in
          (match check_emitted_name_collision env ~source_key:env_key ~ocaml_name with
          | Error _ as err -> err
          | Ok () -> (match expr.ty with
          | TRecord fields ->
              let identity =
                Source_context.find name_form
                |> Option.map (fun location ->
                       (Source_node_id.of_location location, location))
              in
              let allocation =
                allocate_anonymous_record ~owner:"" env next_type fields
              in
              let record_ty = TNamed_record allocation.record in
              let binding = Types.binding ocaml_name record_ty in
              let env = Env.add env_key binding allocation.env in
              if allocation.fresh then
                let item =
                  match expr.record_values with
                  | Some values ->
                      Record_def
                        { var_name = ocaml_name;
                          identity;
                          type_name = allocation.record.type_name;
                          set_module_name = allocation.record.set_module_name;
                          fields;
                          values }
                  | None ->
                      Projected_record_def
                        { var_name = ocaml_name;
                          identity;
                          type_name = allocation.record.type_name;
                          set_module_name = allocation.record.set_module_name;
                          fields;
                          source = expr.semantic_expr }
                in
                Ok (scope, env, allocation.next_type, item)
              else
                let expr = Structural_map.as_named_record allocation.record expr in
                Ok
                  ( scope,
                    env,
                    allocation.next_type,
                    Value_binding
                      { pattern = located_value_pattern name_form (Named ocaml_name);
                        expression = expr.semantic_expr } )
          | _ ->
              let binding = binding_of_expr ocaml_name expr in
              Ok
                ( scope,
                  Env.add env_key binding env,
                  next_type,
                  Value_binding
                    {
                      pattern = located_value_pattern name_form (Named ocaml_name);
                      expression = expr.semantic_expr;
                    } ))))
  | FList
      (FSymbol (("defn" | "defn-") as definition)
      :: ((FSymbol _) as name_form) :: FString _docstring :: forms) ->
      compile scope env next_type
        (FList (FSymbol definition :: name_form :: forms))
  | FList
      (FSymbol ("defn" | "defn-") :: ((FSymbol name) as name_form)
      :: ((FList _) as first_clause) :: remaining_clauses) ->
      let ocaml_name = Names.ocaml_binding_name scope name in
      let env_key = Names.scoped_key scope name in
      (match check_emitted_name_collision env ~source_key:env_key ~ocaml_name with
      | Error _ as err -> err
      | Ok () -> (
          match
            Expression_elaborator.prepare_multi_arity_fn ~ocaml_name scope env name
              (first_clause :: remaining_clauses)
          with
          | Error _ as err -> err
          | Ok prepared ->
              let targets, row_items, recursive_bindings =
                Expression_elaborator.lower_prepared_multi_arity prepared
              in
              let binding =
                Types.binding ~overload_targets:targets ocaml_name prepared.expr.ty
              in
              let value_item =
                Value_binding
                  { pattern = located_value_pattern name_form (Named ocaml_name);
                    expression = prepared.expr.semantic_expr }
              in
              Ok
                ( scope,
                  Env.add env_key binding env,
                  next_type,
                  Group
                    (row_items
                    @ [ Recursive_value_bindings recursive_bindings; value_item ])
                )))
  | FList
      (FSymbol (("defn" | "defn-") as definition)
      :: ((FSymbol _name) as name_form)
      :: ((FVector params) as params_form) :: body_forms)
    when List.exists (function FSymbol "&" -> true | _ -> false) params ->
      compile scope env next_type
        (FList
           [ FSymbol definition;
             name_form;
             FList (params_form :: body_forms) ])
  | FList
      (FSymbol ("defn" | "defn-") :: ((FSymbol name) as name_form) :: params
      :: FKeyword return_keyword
      :: body_forms) -> (
      match Type_annotation.of_keyword return_keyword with
      | Error _ as err -> err
      | Ok return_ty ->
          let ocaml_name = Names.ocaml_binding_name scope name in
          (match
             prepare_recursive_fn ~ocaml_name scope env name return_ty params
               body_forms
           with
          | Error _ as err -> err
          | Ok parts ->
              let param_tys =
                parts.param_bindings
                |> List.map (fun (_key, (binding : binding)) -> binding.ty)
              in
              let row_param_types = row_param_type_names ocaml_name param_tys in
              let expr = fn_code ~row_param_type_names:row_param_types parts in
              let env_key = Names.scoped_key scope name in
              (match
                 check_emitted_name_collision env ~source_key:env_key ~ocaml_name
               with
              | Error _ as err -> err
              | Ok () ->
                  let binding =
                    binding_of_expr ~row_param_types ocaml_name expr
                  in
                  let type_items = row_type_items row_param_types param_tys in
                  let value_item =
                    Recursive_value_binding
                      { name = ocaml_name;
                        identity =
                          Source_context.find name_form
                          |> Option.map (fun location ->
                                 (Source_node_id.of_location location, location));
                        expression = expr.semantic_expr;
                      }
                  in
                  Ok
                    ( scope,
                      Env.add env_key binding env,
                      next_type,
                      Group (type_items @ [ value_item ]) ))))
  | FList
      (FSymbol ("defn" | "defn-") :: ((FSymbol name) as name_form) :: params
      :: body_forms)
    when List.exists (form_mentions_symbol name) body_forms -> (
      let ocaml_name = Names.ocaml_binding_name scope name in
      match
        prepare_inferred_recursive_fn ~ocaml_name scope env name params body_forms
      with
      | Error _ as err -> err
      | Ok parts ->
          let param_tys =
            parts.param_bindings
            |> List.map (fun (_key, (binding : binding)) -> binding.ty)
          in
          let row_param_types = row_param_type_names ocaml_name param_tys in
          let expr = fn_code ~row_param_type_names:row_param_types parts in
          let env_key = Names.scoped_key scope name in
          (match check_emitted_name_collision env ~source_key:env_key ~ocaml_name with
          | Error _ as err -> err
          | Ok () ->
              let binding = binding_of_expr ~row_param_types ocaml_name expr in
              let type_items = row_type_items row_param_types param_tys in
              let value_item =
                Recursive_value_binding
                  { name = ocaml_name;
                    identity =
                      Source_context.find name_form
                      |> Option.map (fun location ->
                             (Source_node_id.of_location location, location));
                    expression = expr.semantic_expr;
                  }
              in
              Ok
                ( scope,
                  Env.add env_key binding env,
                  next_type,
                  Group (type_items @ [ value_item ]) )))
  | FList
      (FSymbol ("defn" | "defn-") :: ((FSymbol name) as name_form) :: params
      :: body_forms) -> (
      match prepare_fn scope env params body_forms with
      | Error _ as err -> err
      | Ok parts when unresolved_contextual_type parts.body.ty ->
          Error.error "empty list requires a contextual element type"
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
                  {
                    pattern = located_value_pattern name_form (Named ocaml_name);
                    expression = expr.semantic_expr;
                  }
              in
              Ok
                ( scope,
                  Env.add env_key binding env,
                  next_type,
                  Group (type_items @ [ value_item ]) )
          | _ -> Error.error "defn body did not compile to a function")))
  | FList
      (FSymbol "defprotocol" :: ((FSymbol protocol_name) as name_form)
      :: method_forms) ->
      compile_defprotocol ?location:(Source_context.find name_form) scope env next_type
        protocol_name method_forms
  | FList
      (FSymbol "extend-type" :: receiver_form :: FSymbol protocol_name
      :: method_forms) ->
      compile_extend_type scope env next_type receiver_form protocol_name
        method_forms
  | FList
      (FSymbol "module" :: ((FSymbol module_name) as name_form)
      :: ((FSymbol signature_name) as signature_form)
      :: forms) -> (
      match
        compile_module ?location:(Source_context.find name_form) ~signature_name
          ?signature_location:(Source_context.find signature_form) scope env next_type
          module_name module_name forms
      with
      | Error _ as err -> err
      | Ok (scope, module_env, module_bindings, next_type, item) ->
          let env =
            env
            |> Env.with_protocols (Env.protocols module_env)
            |> Env.with_modules (Env.modules module_env)
            |> Env.with_types (Env.types module_env)
            |> Env.add_bindings module_bindings
          in
          Ok (scope, env, next_type, item))
  | FList
      (FSymbol "module" :: ((FSymbol module_name) as name_form) :: forms) -> (
      match
        compile_module ?location:(Source_context.find name_form) scope env next_type
          module_name module_name forms
      with
      | Error _ as err -> err
      | Ok (scope, module_env, module_bindings, next_type, item) ->
          let env =
            env
            |> Env.with_protocols (Env.protocols module_env)
            |> Env.with_modules (Env.modules module_env)
            |> Env.with_types (Env.types module_env)
            |> Env.add_bindings module_bindings
          in
          Ok (scope, env, next_type, item))
  | FList (FSymbol (("print" | "println") as name) :: args) -> (
      match compile_call scope env name args with
      | Error _ as err -> err
      | Ok expr ->
          Ok
            ( scope,
              env,
              next_type,
              Value_binding
                { pattern = Unit_pattern; expression = expr.semantic_expr } ))
  | FList [ FSymbol "namespace-scope"; FSymbol namespace_name ] ->
      Ok (namespace_name, env, next_type, Comment ("namespace " ^ namespace_name))
  | FList (FSymbol "refer-clojure-exclude" :: names) ->
      let rec parse_names acc = function
        | [] -> Ok (List.rev acc)
        | FSymbol name :: rest -> parse_names (name :: acc) rest
        | _ -> Error.error ":refer-clojure :exclude expects a vector of symbols"
      in
      Result.map
        (fun names ->
          let env = Env.add_core_exclusions ~scope names env in
          (scope, env, next_type, Comment "refer-clojure exclude"))
        (parse_names [] names)
  | FList (FSymbol "host-import" :: entries) ->
      let rec add_classes env package = function
        | [] -> Ok env
        | FSymbol class_name :: rest -> (
            match Require.add_host_import env package class_name with
            | Error _ as err -> err
            | Ok env -> add_classes env package rest)
        | _ -> Error.error ":import class names must be symbols"
      in
      let rec add_entries env = function
        | [] -> Ok env
        | FVector (FSymbol package :: classes) :: rest -> (
            match add_classes env package classes with
            | Error _ as err -> err
            | Ok env -> add_entries env rest)
        | _ -> Error.error ":import expects vectors containing a package and classes"
      in
      Result.map
        (fun env -> (scope, env, next_type, Comment "host import"))
        (add_entries env entries)
  | FList (FSymbol "defmacro" :: FSymbol name :: forms) ->
      Result.map
        (fun definition ->
          let env = Env.add_macro ~scope ~name definition env in
          (scope, env, next_type, Comment ("macro " ^ name)))
        (Macro_definition.create ~namespace:scope ~name forms)
  | FList (FSymbol "macro-helper-defn" :: FSymbol name :: forms) ->
      Result.map
        (fun definition ->
          let env = Env.add_macro_function ~scope ~name definition env in
          (scope, env, next_type, Comment ("macro helper " ^ name)))
        (Macro_definition.create ~namespace:scope ~name forms)
  | FList (FSymbol "macro-helper-def" :: FSymbol name :: forms) ->
      let value = match forms with [] -> FSymbol "nil" | value :: _ -> value in
      let env = Env.add_macro_value ~scope ~name value env in
      Ok (scope, env, next_type, Comment ("macro value " ^ name))
  | FList [ FSymbol ("def" as kind); FSymbol name ] ->
      compile scope env next_type
        (FList [ FSymbol kind; FSymbol name; FSymbol "nil" ])
  | FList (FSymbol "declare" :: names) ->
      let rec add_declarations env = function
        | [] -> Ok env
        | FSymbol name :: rest ->
            let key = Names.scoped_key scope name in
            let ocaml_name = Names.ocaml_binding_name scope name in
            let binding = Types.binding ocaml_name (TOcaml "__declared_fn") in
            add_declarations (Env.add key binding env) rest
        | _ -> Error.error "declare expects symbols"
      in
      Result.map
        (fun env -> (scope, env, next_type, Comment "declare"))
        (add_declarations env names)
  | FList (FSymbol "require" :: entries) -> (
      match Require.parse_entries entries with
      | Error _ as err -> err
      | Ok specs ->
          let rec apply_specs env = function
            | [] -> Ok env
            | Require.Package _ :: rest -> apply_specs env rest
            | Require.Load { module_name } :: rest ->
                let result =
                  if module_name = "clojure.set" then
                    Ok env
                  else if module_name = "clojure.string" then
                    Ok
                      (Require.add_clojure_string_alias_bindings env module_name)
                  else if String.starts_with ~prefix:"ocaml." module_name then
                    Ok (Require.add_ocaml_alias_bindings env module_name module_name)
                  else Require.ensure_namespace env module_name
                in
                (match result with
                | Error _ as err -> err
                | Ok env -> apply_specs env rest)
            | Require.Alias { module_name; alias } :: rest ->
                if String.starts_with ~prefix:"ocaml." module_name then
                  apply_specs
                    (Require.add_ocaml_alias_bindings env module_name alias)
                    rest
                else if Require.core_namespace module_name then
                  let env =
                    if module_name = "clojure.string" then
                      Require.add_clojure_string_alias_bindings env alias
                    else env
                  in
                  apply_specs
                    (Env.add_namespace_alias ~scope ~alias ~target:module_name env)
                    rest
                else (
                  match Require.add_lg_alias_bindings env module_name alias with
                  | Error _ as err -> err
                  | Ok env ->
                      let env =
                        Env.add_namespace_alias ~scope ~alias
                          ~target:module_name env
                      in
                      apply_specs env rest)
            | Require.Refer { module_name; names } :: rest ->
                let result =
                  if String.starts_with ~prefix:"ocaml." module_name then
                    Require.add_ocaml_refer_bindings env scope module_name names
                  else if module_name = "clojure.string" then
                    Require.add_clojure_string_refer_bindings env scope names
                  else Require.add_lg_refer_bindings env scope module_name names
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
                { pattern = Ignore_pattern; expression = expr.semantic_expr } ))
  | FList (FSymbol "recur" :: _) ->
      Error.error "recur is only valid in a loop tail position"
  | FList (FSymbol ("defn" | "defn-") :: _) ->
      Error.error "defn expects a name, parameter vector, and body"
  | FList (FSymbol "defonce" :: _) ->
      Error.error "defonce expects a name and value"
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
                    { pattern = Ignore_pattern; expression = expr.semantic_expr } )))
