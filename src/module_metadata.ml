open Types
open Lowered

module Env = Compiler_environment

let signature_binding_key signature_name value_name =
  "__signature/" ^ signature_name ^ "/" ^ value_name

let functor_result_key functor_name value_name =
  "__functor/" ^ functor_name ^ "/" ^ value_name

let functor_result_record_key functor_name type_name =
  "__functor_record/" ^ functor_name ^ "/" ^ type_name

let signature_bindings env signature_name items =
  let nested_value_path module_name value_path =
    match String.rindex_opt value_path '/' with
    | None -> module_name ^ "/" ^ value_path
    | Some separator ->
        let nested_path = String.sub value_path 0 separator in
        let value_name =
          String.sub value_path (separator + 1)
            (String.length value_path - separator - 1)
        in
        module_name ^ "." ^ nested_path ^ "/" ^ value_name
  in
  let item_bindings = function
    | Signature_value { source_name; value_name; value_type } ->
        [
          ( signature_binding_key signature_name source_name,
            Types.binding value_name value_type );
        ]
    | Signature_type _ -> []
    | Signature_module { source_name; module_signature; _ } ->
        let nested_prefix = "__signature/" ^ module_signature ^ "/" in
        let nested_prefix_len = String.length nested_prefix in
        env
        |> Env.filter_map (fun key binding ->
               if
                 String.length key > nested_prefix_len
                 && String.sub key 0 nested_prefix_len = nested_prefix
               then
                 let nested_name =
                   String.sub key nested_prefix_len
                     (String.length key - nested_prefix_len)
                 in
                 Some
                   ( signature_binding_key signature_name
                       (nested_value_path source_name nested_name),
                     binding )
               else None)
    | Signature_include { module_signature } ->
        let included_prefix = "__signature/" ^ module_signature ^ "/" in
        let included_prefix_len = String.length included_prefix in
        env
        |> Env.filter_map (fun key binding ->
               if
                 String.length key > included_prefix_len
                 && String.sub key 0 included_prefix_len = included_prefix
               then
                 let value_path =
                   String.sub key included_prefix_len
                     (String.length key - included_prefix_len)
                 in
                 Some (signature_binding_key signature_name value_path, binding)
               else None)
  in
  List.concat_map item_bindings items

let parameter_value_binding parameter_name value_path (binding : binding) =
  match String.rindex_opt value_path '/' with
  | None ->
      ( Module_environment.binding_key parameter_name value_path,
        {
          binding with
          ocaml_name =
            Names.module_segment_to_ocaml parameter_name ^ "." ^ binding.ocaml_name;
        } )
  | Some separator ->
      let nested_path = String.sub value_path 0 separator in
      let value_name =
        String.sub value_path (separator + 1)
          (String.length value_path - separator - 1)
      in
      let parameter_path = parameter_name ^ "." ^ nested_path in
      ( Module_environment.binding_key parameter_path value_name,
        {
          binding with
          ocaml_name =
            Names.module_path_to_ocaml parameter_path ^ "." ^ binding.ocaml_name;
        } )

let rec typed_signature_bindings modules signature_id =
  let qualify prefix (path, binding) = (prefix ^ "/" ^ path, binding) in
  match
    Module_registry.find_signature_named ~owner:(Signature_id.owner signature_id)
      (Signature_id.name signature_id) modules
  with
  | None -> None
  | Some (resolved_id, items) ->
      let owner = Signature_id.owner resolved_id in
      let bindings =
        items
        |> List.concat_map (function
             | Signature_value { source_name; value_name; value_type } ->
                 [ (source_name, Types.binding value_name value_type) ]
             | Signature_type _ -> []
             | Signature_module { source_name; module_signature; _ } ->
                 let nested_id =
                   Signature_id.create ~owner ~name:module_signature
                 in
                 Option.value
                   (typed_signature_bindings modules nested_id
                   |> Option.map (List.map (qualify source_name)))
                   ~default:[]
             | Signature_include { module_signature } ->
                 let included_id =
                   Signature_id.create ~owner ~name:module_signature
                 in
                 Option.value
                   (typed_signature_bindings modules included_id)
                   ~default:[])
      in
      Some bindings

let signature_parameter_bindings ~scope env parameter_name signature_name =
  let signature_id =
    Signature_id.create
      ~owner:(if scope = "" then [] else [ scope ])
      ~name:signature_name
  in
  match typed_signature_bindings (Env.modules env) signature_id with
  | Some bindings ->
      List.map
        (fun (value_path, binding) ->
          parameter_value_binding parameter_name value_path binding)
        bindings
  | None ->
  let prefix = "__signature/" ^ signature_name ^ "/" in
  let prefix_len = String.length prefix in
  env
  |> Env.filter_map (fun key (binding : binding) ->
         if String.length key > prefix_len && String.sub key 0 prefix_len = prefix then
           let value_name =
             String.sub key prefix_len (String.length key - prefix_len)
           in
           Some (parameter_value_binding parameter_name value_name binding)
         else None)

let store_functor_result_bindings functor_name public_bindings =
  let prefix = functor_name ^ "/" in
  let prefix_len = String.length prefix in
  let record_prefix = "__record/" ^ functor_name ^ "/" in
  let record_prefix_len = String.length record_prefix in
  public_bindings
  |> List.filter_map (fun (key, (binding : binding)) ->
         if String.length key > prefix_len && String.sub key 0 prefix_len = prefix then
           let value_name =
             String.sub key prefix_len (String.length key - prefix_len)
           in
           Some (functor_result_key functor_name value_name, binding)
         else if
           String.length key > record_prefix_len
           && String.sub key 0 record_prefix_len = record_prefix
         then
           let type_name =
             String.sub key record_prefix_len
               (String.length key - record_prefix_len)
           in
           Some (functor_result_record_key functor_name type_name, binding)
         else None)

let apply_stored_functor_result module_name functor_name public_bindings =
  let prefix = functor_name ^ "/" in
  let prefix_len = String.length prefix in
  let record_prefix = "__record/" ^ functor_name ^ "/" in
  let record_prefix_len = String.length record_prefix in
  public_bindings
  |> List.filter_map (fun (key, (binding : binding)) ->
         if String.length key > prefix_len && String.sub key 0 prefix_len = prefix then
           let value_name = String.sub key prefix_len (String.length key - prefix_len) in
           Some
             ( Module_environment.binding_key module_name value_name,
               { binding with ocaml_name = Module_environment.binding_ocaml_name module_name value_name } )
         else if String.length key > record_prefix_len
                 && String.sub key 0 record_prefix_len = record_prefix then
           let type_name = String.sub key record_prefix_len (String.length key - record_prefix_len) in
           Some (Resolver.record_type_key module_name type_name, binding)
         else None)

let apply_functor_result_bindings env module_name functor_name =
  let functor_id = Functor_id.of_string functor_name in
  match Module_registry.find_functor_result functor_id (Env.modules env) with
  | Some bindings -> apply_stored_functor_result module_name functor_name bindings
  | None ->
  let prefix = "__functor/" ^ functor_name ^ "/" in
  let prefix_len = String.length prefix in
  let record_prefix = "__functor_record/" ^ functor_name ^ "/" in
  let record_prefix_len = String.length record_prefix in
  env
  |> Env.filter_map (fun key (binding : binding) ->
         if String.length key > prefix_len && String.sub key 0 prefix_len = prefix then
           let value_name =
             String.sub key prefix_len (String.length key - prefix_len)
           in
           Some
             ( Module_environment.binding_key module_name value_name,
               {
                 binding with
                 ocaml_name = Module_environment.binding_ocaml_name module_name value_name;
               } )
         else if
           String.length key > record_prefix_len
           && String.sub key 0 record_prefix_len = record_prefix
         then
           let type_name =
             String.sub key record_prefix_len
               (String.length key - record_prefix_len)
           in
           Some (Resolver.record_type_key module_name type_name, binding)
         else None)
