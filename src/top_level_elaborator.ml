open Ast
open Types
open Lowered

module Env = Compiler_environment

let compile_expr = Expression_elaborator.compile_expr
let prepare_fn = Expression_elaborator.prepare_fn
let prepare_recursive_fn = Expression_elaborator.prepare_recursive_fn
let fn_code = Expression_elaborator.fn_code
let compile_fn = Expression_elaborator.compile_fn
let compile_args_for = Expression_elaborator.compile_args_for
let compile_call = Expression_elaborator.compile_call
let binding_of_expr = Expression_support.binding_of_expr
let row_param_type_names = Expression_support.row_param_type_names
let row_type_items = Expression_support.row_type_items
let check_emitted_name_collision = Resolver.check_emitted_name_collision
let lookup_record_type = Resolver.lookup_record_type
let record_type_key = Resolver.record_type_key
let inherit_scope_ocaml_value_refers =
  Expression_support.inherit_scope_ocaml_value_refers

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

let compile scope env next_type = function
  | FList (FSymbol "module-signature" :: FSymbol signature_name :: item_forms) ->
      compile_module_signature scope env next_type signature_name item_forms
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
  | FList [ FSymbol "def"; ((FSymbol name) as name_form); expr_form ] -> (
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
                          identity =
                            Source_context.find name_form
                            |> Option.map (fun location ->
                                   (Source_node_id.of_location location, location));
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
                    {
                      pattern = located_value_pattern name_form (Named ocaml_name);
                      expression = expr.semantic_expr;
                    } ))))
  | FList
      (FSymbol "defn" :: ((FSymbol name) as name_form) :: params
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
      (FSymbol "defn" :: ((FSymbol name) as name_form) :: params :: body_forms) -> (
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
      | Ok (scope, module_env, module_bindings, next_type, item) ->
          let env =
            env
            |> Env.with_protocols (Env.protocols module_env)
            |> Env.with_modules (Env.modules module_env)
            |> Env.with_types (Env.types module_env)
            |> Env.add_bindings module_bindings
          in
          Ok (scope, env, next_type, item))
  | FList (FSymbol "module" :: FSymbol module_name :: forms) -> (
      match compile_module scope env next_type module_name module_name forms with
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
                { pattern = Ignore_pattern; expression = expr.semantic_expr } ))
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
                    { pattern = Ignore_pattern; expression = expr.semantic_expr } )))
