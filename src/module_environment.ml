open Types

module Env = Compiler_environment

let binding_key module_path name = module_path ^ "/" ^ name

let binding_ocaml_name module_path name =
  Names.module_path_to_ocaml module_path ^ "." ^ Names.sanitize_name name

let changed_bindings previous updated =
  Env.to_bindings updated
  |> List.filter (fun (key, binding) ->
         match Env.find_opt key previous with
         | None -> true
         | Some previous_binding -> previous_binding <> binding)

let open_bindings scope env module_path =
  let prefix = module_path ^ "/" in
  let prefix_len = String.length prefix in
  let record_prefix = "__record/" ^ module_path ^ "/" in
  let record_prefix_len = String.length record_prefix in
  let opened =
    env
    |> Env.filter_map (fun key (binding : binding) ->
           if String.length key > prefix_len && String.sub key 0 prefix_len = prefix then
             let local = String.sub key prefix_len (String.length key - prefix_len) in
             let opened_binding =
               { binding with ocaml_name = Names.sanitize_name local }
             in
             Some (Names.scoped_key scope local, opened_binding)
           else if
             String.length key > record_prefix_len
             && String.sub key 0 record_prefix_len = record_prefix
           then
             let local =
               String.sub key record_prefix_len
                 (String.length key - record_prefix_len)
             in
             Some (Resolver.record_type_key scope local, binding)
           else None)
  in
  let env = Env.add_bindings opened env in
  let opened_module = Names.module_path_to_ocaml module_path in
  Env.add
    ("__opened/" ^ scope ^ "/" ^ opened_module)
    (Types.binding ~host_reference:(Ocaml_module opened_module) opened_module
       (TOcaml "__module"))
    env

let include_public_bindings module_path env included_module_path =
  let prefix = included_module_path ^ "/" in
  let prefix_len = String.length prefix in
  let record_prefix = "__record/" ^ included_module_path ^ "/" in
  let record_prefix_len = String.length record_prefix in
  env
  |> Env.filter_map (fun key (binding : binding) ->
         if String.length key > prefix_len && String.sub key 0 prefix_len = prefix then
           let name = String.sub key prefix_len (String.length key - prefix_len) in
           Some
             ( binding_key module_path name,
               { binding with ocaml_name = binding_ocaml_name module_path name } )
         else if
           String.length key > record_prefix_len
           && String.sub key 0 record_prefix_len = record_prefix
         then
           let name =
             String.sub key record_prefix_len
               (String.length key - record_prefix_len)
           in
           Some (Resolver.record_type_key module_path name, binding)
         else None)

let alias_bindings env alias_path target_path =
  let direct_prefix = target_path ^ "/" in
  let direct_prefix_len = String.length direct_prefix in
  let nested_prefix = target_path ^ "." in
  let nested_prefix_len = String.length nested_prefix in
  let direct_record_prefix = "__record/" ^ target_path ^ "/" in
  let direct_record_prefix_len = String.length direct_record_prefix in
  let nested_record_prefix = "__record/" ^ target_path ^ "." in
  let nested_record_prefix_len = String.length nested_record_prefix in
  env
  |> Env.filter_map (fun key (binding : binding) ->
         if
           String.length key > direct_prefix_len
           && String.sub key 0 direct_prefix_len = direct_prefix
         then
           let name =
             String.sub key direct_prefix_len
               (String.length key - direct_prefix_len)
           in
           let alias_key = binding_key alias_path name in
           let alias_binding =
             { binding with ocaml_name = binding_ocaml_name alias_path name }
           in
           Some (alias_key, alias_binding)
         else if
           String.length key > nested_prefix_len
           && String.sub key 0 nested_prefix_len = nested_prefix
         then
           let suffix =
             String.sub key nested_prefix_len
               (String.length key - nested_prefix_len)
           in
           (match String.split_on_char '/' suffix with
           | [ nested_path; name ] ->
               let nested_alias_path = alias_path ^ "." ^ nested_path in
               Some
                 ( binding_key nested_alias_path name,
                   {
                     binding with
                     ocaml_name = binding_ocaml_name nested_alias_path name;
                   } )
           | _ -> None)
         else if
           String.length key > direct_record_prefix_len
           && String.sub key 0 direct_record_prefix_len = direct_record_prefix
         then
           let name =
             String.sub key direct_record_prefix_len
               (String.length key - direct_record_prefix_len)
           in
           Some (Resolver.record_type_key alias_path name, binding)
         else if
           String.length key > nested_record_prefix_len
           && String.sub key 0 nested_record_prefix_len = nested_record_prefix
         then
           let suffix =
             String.sub key nested_record_prefix_len
               (String.length key - nested_record_prefix_len)
           in
           (match String.split_on_char '/' suffix with
           | [ nested_path; name ] ->
               Some
                 ( Resolver.record_type_key (alias_path ^ "." ^ nested_path) name,
                   binding )
           | _ -> None)
         else None)
