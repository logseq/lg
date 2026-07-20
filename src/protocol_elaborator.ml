open Ast
open Types
open Lowered

module Env = Compiler_environment

let define ?location scope env protocol_name method_forms =
  match Protocol.defprotocol scope protocol_name method_forms with
  | Error _ as err -> err
  | Ok (protocol_id, signatures) ->
      let method_locations =
        List.filter_map
          (function
            | FList (((FSymbol method_name) as name_form) :: _) ->
                Source_context.find name_form
                |> Option.map (fun location ->
                       (Protocol.method_id protocol_id method_name, location))
            | _ -> None)
          method_forms
      in
      (match
         Protocol_registry.declare ?location ~method_locations protocol_id signatures
           (Env.protocols env)
       with
      | Error _ as err -> err
      | Ok protocols ->
          let env = Env.with_protocols protocols env in
          Ok (env, Comment ("protocol " ^ protocol_name)))

let marker scope env protocol_name method_name =
  match Protocol.lookup_protocol_marker scope env protocol_name method_name with
  | None ->
      Error.error
        ("protocol " ^ protocol_name ^ " does not define method " ^ method_name)
  | Some marker
    when not
           (match Protocol.find_protocol_id scope env protocol_name with
           | Some protocol_id ->
               Protocol.marker_has_protocol_id marker protocol_id
           | None -> false) ->
      Error.error
        ("protocol " ^ protocol_name ^ " does not define method " ^ method_name)
  | Some marker -> Ok marker

let add_implementation ?location env method_name receiver_ty marker binding =
  match
    (marker.protocol_id, Protocol.registry_receiver_id receiver_ty)
  with
  | None, _ | _, None ->
      Error.error
        ("protocol implementations do not support receiver type "
       ^ source_name receiver_ty)
  | Some protocol_id, Some receiver_id ->
      let method_id = Protocol.method_id protocol_id method_name in
      let existing =
        Protocol_registry.find_implementation protocol_id method_id receiver_id
          (Env.protocols env)
      in
      let protocols =
        match existing with
        | Some existing
          when existing.forward_declared
               && existing.ocaml_name = binding.ocaml_name ->
            Ok
              (Protocol_registry.replace_implementation protocol_id method_id
                 receiver_id binding (Env.protocols env))
        | Some _ ->
            Error.error
              ("duplicate implementation of " ^ Protocol_id.name protocol_id
             ^ "/" ^ method_name ^ " for " ^ source_name receiver_ty)
        | None ->
            Protocol_registry.add_implementation ?location protocol_id
              method_id receiver_id binding (Env.protocols env)
      in
      (match protocols with
      | Error _ as err -> err
      | Ok protocols ->
          let env = Env.with_protocols protocols env in
          let protocol_evidence =
            Env.protocol_evidence env
            |> Option.map
                 (Protocol_registry.replace_implementation protocol_id
                    method_id receiver_id binding)
          in
          Ok (Env.with_protocol_evidence protocol_evidence env))

let compile_defprotocol ?location scope env next_type protocol_name method_forms =
  match define ?location scope env protocol_name method_forms with
  | Error _ as err -> err
  | Ok (env, item) -> Ok (scope, env, next_type, item)

let protocol_receiver_type scope env = function
  | FKeyword receiver_keyword -> Type_annotation.of_keyword receiver_keyword
  | FSymbol type_name ->
      Resolver.lookup_record_type scope env type_name
      |> Result.map (fun record -> TNamed_record record)
  | _ -> Error.error "extend-type receiver must be a type keyword or record type"

let protocol_parameter_overrides receiver_ty = function
  | TFn (parameter_tys, _) ->
      let infer_generic_parameters =
        match receiver_ty with
        | TNamed_record { type_parameters = _ :: _; fields; _ } ->
            Option.is_none (Types.find_record_extension_field fields)
        | _ -> false
      in
      List.mapi
        (fun index ty ->
          if index = 0 then Some receiver_ty
          else if infer_generic_parameters then None
          else
            Some
              (match ty with
              | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
              | ty -> ty))
        parameter_tys
  | _ -> [ Some receiver_ty ]

let predeclare_implementations_from_evidence scope env receiver_form
    protocol_name method_forms =
  match
    ( Protocol.find_protocol_id scope env protocol_name,
      Env.protocol_evidence env )
  with
  | None, _ | _, None -> Ok env
  | Some _, Some evidence ->
      Result.bind (protocol_receiver_type scope env receiver_form)
        (fun receiver_ty ->
          let rec predeclare env = function
            | [] -> Ok env
            | FList (FSymbol method_name :: _params :: _) :: rest ->
                Result.bind (marker scope env protocol_name method_name)
                  (fun marker ->
                    match
                      ( marker.protocol_id,
                        Protocol.registry_receiver_id receiver_ty )
                    with
                    | Some protocol_id, Some receiver_id ->
                        let method_id =
                          Protocol.method_id protocol_id method_name
                        in
                        (match
                           Protocol_registry.find_implementation protocol_id
                             method_id receiver_id evidence
                         with
                        | None -> predeclare env rest
                        | Some binding ->
                            Result.bind
                              (add_implementation env method_name receiver_ty
                                 marker
                                 { binding with forward_declared = true })
                              (fun env -> predeclare env rest))
                    | None, _ | _, None -> predeclare env rest)
            | _ :: rest -> predeclare env rest
          in
          predeclare env method_forms)

let compile_extend_type scope env next_type receiver_form protocol_name method_forms =
  match protocol_receiver_type scope env receiver_form with
  | Error _ as err -> err
  | Ok receiver_ty ->
      let rec form_mentions name = function
        | FSymbol candidate -> candidate = name
        | FList forms | FVector forms ->
            List.exists (form_mentions name) forms
        | FMap pairs ->
            List.exists
              (fun (key, value) ->
                form_mentions name key || form_mentions name value)
              pairs
        | FInt _ | FFloat _ | FChar _ | FString _ | FRegex _ | FBool _
        | FKeyword _ | FCoreSymbol _ ->
            false
      in
      let bind_record_fields params body_forms =
        match (receiver_ty, params) with
        | TNamed_record record, FVector (FSymbol receiver_name :: _) ->
            let bindings =
              record.fields
              |> List.filter (fun (field : field) ->
                     let name = Names.keyword_source_name field.keyword in
                     List.exists (form_mentions name) body_forms)
              |> List.concat_map (fun (field : field) ->
                     let name = Names.keyword_source_name field.keyword in
                     [ FSymbol name;
                       FList
                         [ FSymbol (".-" ^ name);
                           FSymbol receiver_name;
                         ];
                     ])
            in
            if bindings = [] then body_forms
            else [ FList (FSymbol "let" :: FVector bindings :: body_forms) ]
        | _ -> body_forms
      in
      let compile_method implementation_names env = function
        | FList (((FSymbol method_name) as name_form) :: params :: body_forms) -> (
            match marker scope env protocol_name method_name with
            | Error _ as err -> err
            | Ok marker -> (
                match Protocol.annotate_receiver receiver_ty params with
                | Error _ as err -> err
                | Ok params -> (
                    let body_forms = bind_record_fields params body_forms in
                    let param_type_overrides =
                      protocol_parameter_overrides receiver_ty marker.ty
                    in
                    match
                      Expression_elaborator.compile_fn ~param_type_overrides scope env params body_forms
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
                                             (Types.assignable ~policy:Host_boundary ~expected
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
                                           (Types.assignable ~policy:Host_boundary
                                              ~expected:expected_ret
                                              ~actual:actual_ret) ->
                                  Error.error
                                    ("protocol method " ^ method_name ^ " must return "
                                   ^ source_name expected_ret)
                                  | None -> (
                                  let ocaml_name =
                                    Protocol.impl_ocaml_name scope protocol_name
                                      method_name receiver_ty
                                  in
                                  let binding = Expression_support.binding_of_expr ocaml_name expr in
                                  (match
                                     add_implementation
                                       ?location:(Source_context.find name_form) env
                                       method_name receiver_ty marker binding
                                   with
                                  | Error _ as err -> err
                                  | Ok env ->
                                      let declared_names =
                                        Env.filter_map
                                          (fun _ (binding : binding) ->
                                            match binding.ty with
                                            | TOcaml "__declared_fn" ->
                                                Some binding.ocaml_name
                                            | _ when binding.forward_declared ->
                                                Some binding.ocaml_name
                                            | _ -> None)
                                          env
                                      in
                                      let item =
                                        if
                                          Semantic_ir.exists_identifier
                                            (fun name ->
                                              List.mem name declared_names)
                                            expr.semantic_expr
                                        then
                                          Deferred_value_binding
                                            { name = ocaml_name;
                                              value_type =
                                                Protocol.refine_deferred_type
                                                  env expr.ty;
                                              return_param_index =
                                                expr.return_param_index;
                                              expression = expr.semantic_expr;
                                            }
                                        else if
                                          Semantic_ir.exists_identifier
                                            (fun name ->
                                              List.mem name implementation_names)
                                            expr.semantic_expr
                                        then
                                          Recursive_value_binding
                                            { name = ocaml_name;
                                              identity = None;
                                              expression = expr.semantic_expr;
                                            }
                                        else
                                          Value_binding
                                            { pattern = Named ocaml_name;
                                              expression = expr.semantic_expr;
                                            }
                                      in
                                      Ok
                                        ( env,
                                          item )))))
                        | _ -> Error.error "protocol method did not compile to a function"))))
        | _ -> Error.error "extend-type methods must be (method-name [params] body)"
      in
      let rec loop implementation_names env items = function
        | [] ->
            let ordinary, recursive =
              List.rev items
              |> List.fold_left
                   (fun (ordinary, recursive) -> function
                     | Recursive_value_binding
                         { name; identity; expression } ->
                         ( ordinary,
                           ({ name; identity; expression } : recursive_value)
                           :: recursive )
                     | item -> (item :: ordinary, recursive))
                   ([], [])
            in
            let items =
              List.rev ordinary
              @
              match List.rev recursive with
              | [] -> []
              | bindings -> [ Recursive_value_bindings bindings ]
            in
            Ok
              ( scope,
                env,
                next_type,
                Group items )
        | method_form :: rest -> (
            match compile_method implementation_names env method_form with
            | Error _ as err -> err
            | Ok (env, item) ->
                loop implementation_names env (item :: items) rest)
      in
      let rec predeclare_exact env evidence_env names = function
        | [] -> Ok (env, List.sort_uniq String.compare names)
        | FList (FSymbol method_name :: _params :: _) :: rest -> (
            match marker scope env protocol_name method_name with
            | Error _ as error -> error
            | Ok marker -> (
                match
                  ( marker.protocol_id,
                    Protocol.registry_receiver_id receiver_ty )
                with
                | Some protocol_id, Some receiver_id ->
                    let method_id =
                      Protocol.method_id protocol_id method_name
                    in
                    (match
                       Protocol_registry.find_implementation protocol_id
                         method_id receiver_id (Env.protocols evidence_env)
                     with
                    | None ->
                        Error.error
                          ("missing inferred protocol implementation for "
                         ^ method_name)
                    | Some binding ->
                        let binding =
                          { binding with forward_declared = true }
                        in
                        (match
                           add_implementation env method_name receiver_ty marker
                             binding
                         with
                        | Error _ as error -> error
                        | Ok env ->
                            predeclare_exact env evidence_env
                              (binding.ocaml_name :: names) rest))
                | None, _ | _, None ->
                    Error.error
                      ("protocol implementations do not support receiver type "
                     ^ source_name receiver_ty)))
        | _ :: _ ->
            Error.error "extend-type methods must be (method-name [params] body)"
      in
      Result.bind (loop [] env [] method_forms)
        (fun (_, evidence_env, _, _) ->
          Result.bind
            (predeclare_exact env evidence_env [] method_forms)
            (fun (env, implementation_names) ->
              loop implementation_names env [] method_forms))
