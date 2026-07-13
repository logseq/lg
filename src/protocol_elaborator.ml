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
           (Protocol.marker_has_protocol_id marker
              (Protocol.protocol_id scope protocol_name)) ->
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
      if
        Option.is_some
          (Protocol_registry.find_implementation protocol_id method_id
             receiver_id (Env.protocols env))
      then
        Error.error
          ("duplicate implementation of " ^ Protocol_id.name protocol_id ^ "/"
         ^ method_name ^ " for " ^ source_name receiver_ty)
      else
        (match
         Protocol_registry.add_implementation ?location protocol_id method_id
           receiver_id binding (Env.protocols env)
       with
      | Error _ as err -> err
      | Ok protocols -> Ok (Env.with_protocols protocols env))

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

let compile_extend_type scope env next_type receiver_form protocol_name method_forms =
  match protocol_receiver_type scope env receiver_form with
  | Error _ as err -> err
  | Ok receiver_ty ->
      let compile_method env = function
        | FList (((FSymbol method_name) as name_form) :: params :: body_forms) -> (
            match marker scope env protocol_name method_name with
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
                                      Ok
                                        ( env,
                                          Value_binding
                                            {
                                              pattern = Named ocaml_name;
                                              expression = expr.semantic_expr;
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
