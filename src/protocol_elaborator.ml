open Types
open Lowered

module Env = Compiler_environment

let define scope env protocol_name method_forms =
  match Protocol.defprotocol_bindings scope protocol_name method_forms with
  | Error _ as err -> err
  | Ok bindings ->
      let env =
        List.fold_left
          (fun env (key, binding) ->
            if Protocol.is_legacy_marker key binding && Env.mem key env then
              Env.add key (Protocol.ambiguous_marker_binding ()) env
            else Env.add key binding env)
          env bindings
      in
      Ok (env, Comment ("protocol " ^ protocol_name))

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

let add_implementation scope env protocol_name method_name receiver_ty marker binding =
  match Protocol.marker_impl_name marker method_name receiver_ty with
  | None ->
      Error.error
        ("protocol implementations do not support receiver type "
       ^ source_name receiver_ty)
  | Some impl_key_name ->
      let env_key = Names.scoped_key scope impl_key_name in
      if Env.mem env_key env then
        Error.error
          ("duplicate implementation of " ^ protocol_name ^ "/" ^ method_name
         ^ " for " ^ source_name receiver_ty)
      else Ok (Env.add env_key binding env)

