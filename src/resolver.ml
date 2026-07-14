open Types

module Env = Compiler_environment

let record_type_key scope type_name = "__record/" ^ scope ^ "/" ^ type_name

let split_qualified_type_name type_name =
  match String.rindex_opt type_name '.' with
  | None -> None
  | Some index ->
      let module_path = String.sub type_name 0 index in
      let local_name =
        String.sub type_name (index + 1) (String.length type_name - index - 1)
      in
      Some (module_path, local_name)

let qualify_record_type module_path record =
  let type_name = Names.module_path_to_ocaml module_path ^ "." ^ record.type_name in
  {
    record with
    type_name;
    set_module_name =
      Names.module_path_to_ocaml module_path ^ "." ^ record.set_module_name;
  }

let lookup_record_type scope env type_name =
  let lookup owner local_name = Env.find_opt (record_type_key owner local_name) env in
  let local_lookup owner local_name =
    match lookup owner local_name with
    | Some ({ ty = TNamed_record record; _ } : binding) -> Ok record
    | Some _ -> Error.error ("invalid record type metadata for " ^ type_name)
    | None -> Error.error ("unknown record type " ^ type_name)
  in
  match split_qualified_type_name type_name with
  | Some (module_path, local_name) -> (
      match local_lookup (Names.module_path_to_ocaml module_path) local_name with
      | Ok record -> Ok (qualify_record_type module_path record)
      | Error _ as err -> err)
  | None -> local_lookup scope type_name

let lookup_binding scope env name =
  match Env.find_opt (Names.scoped_key scope name) env with
  | Some (binding : binding) -> Ok binding
  | None -> Error.error ("unknown function " ^ name)

let binding_owner key =
  match String.rindex_opt key '/' with
  | None -> ""
  | Some index -> String.sub key 0 index

let check_emitted_name_collision env ~source_key ~ocaml_name =
  let owner = binding_owner source_key in
  match
    List.find_opt
      (fun (key, (binding : binding)) ->
        (not (String.starts_with ~prefix:"__" key))
        && key <> source_key && binding_owner key = owner
        && binding.ocaml_name = ocaml_name)
      (Env.to_bindings env)
  with
  | None -> Ok ()
  | Some (existing_key, _) ->
      let source_name = Protocol.method_basename source_key in
      let existing_name = Protocol.method_basename existing_key in
      Error.error
        ("OCaml name collision: " ^ existing_name ^ " and " ^ source_name
       ^ " both emit " ^ ocaml_name)

let lookup_host_reference scope env name =
  match Env.find_opt (Names.scoped_key scope name) env with
  | Some _ as binding -> binding
  | None -> Env.find_opt name env

let starts_with_uppercase name =
  String.length name > 0
  && Char.uppercase_ascii name.[0] = name.[0]

let ocaml_call_target scope env function_name =
  match lookup_host_reference scope env function_name with
  | Some { host_reference = Some (Ocaml_value ocaml_name); _ } -> Some ocaml_name
  | _ -> (
      match String.split_on_char '/' function_name with
      | [ alias; member_name ] -> (
          match lookup_host_reference scope env alias with
          | Some { host_reference = Some (Ocaml_module module_path); _ } ->
              Some (module_path ^ "." ^ Names.sanitize_name member_name)
          | None -> (
              match Host_interop.implicit_module alias with
              | Some module_path ->
                  Some (module_path ^ "." ^ Names.sanitize_name member_name)
              | None when String.length alias > 0 && starts_with_uppercase alias ->
                  Some (alias ^ "." ^ Names.sanitize_name member_name)
              | None -> None)
          | _ when String.length alias > 0 && starts_with_uppercase alias ->
              Some (alias ^ "." ^ Names.sanitize_name member_name)
          | _ -> None)
      | _ ->
          let first_segment =
            match String.split_on_char '.' function_name with
            | first :: _ -> first
            | [] -> ""
          in
          if String.contains function_name '.' && first_segment <> ""
             && Char.uppercase_ascii first_segment.[0] = first_segment.[0]
          then Some function_name
          else None)

let resolve_ocaml_call_target scope env function_name =
  match ocaml_call_target scope env function_name with
  | Some target -> target
  | None -> function_name

let resolve_ocaml_constructor_target scope env constructor_name =
  match String.split_on_char '/' constructor_name with
  | [ alias; member_name ] -> (
      match lookup_host_reference scope env alias with
      | Some { host_reference = Some (Ocaml_module module_path); _ } ->
          module_path ^ "." ^ member_name
      | _ when starts_with_uppercase alias -> alias ^ "." ^ member_name
      | _ -> constructor_name)
  | _ -> constructor_name
