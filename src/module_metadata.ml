open Types
open Lowered

module Env = Compiler_environment
module Signature_set = Set.Make (Signature_id)

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

let typed_signature_bindings modules signature_id =
  let qualify prefix (path, binding) = (prefix ^ "/" ^ path, binding) in
  let rec expand visiting signature_id =
    match
      Module_registry.find_signature_named
        ~owner:(Signature_id.owner signature_id)
        (Signature_id.name signature_id) modules
    with
    | None -> Ok []
    | Some (resolved_id, items) ->
        if Signature_set.mem resolved_id visiting then
          Error.error
            ("cyclic module signature include "
           ^ Signature_id.to_string resolved_id)
        else
          let visiting = Signature_set.add resolved_id visiting in
          let owner = Signature_id.owner resolved_id in
          let rec collect bindings = function
            | [] -> Ok (List.rev bindings |> List.concat)
            | Signature_value { source_name; value_name; value_type } :: rest ->
                collect
                  ([ (source_name, Types.binding value_name value_type) ]
                  :: bindings)
                  rest
            | Signature_type _ :: rest -> collect ([] :: bindings) rest
            | Signature_module { source_name; module_signature; _ } :: rest ->
                let nested_id =
                  Signature_id.create ~owner ~name:module_signature
                in
                (match expand visiting nested_id with
                | Error _ as err -> err
                | Ok nested ->
                    collect (List.map (qualify source_name) nested :: bindings)
                      rest)
            | Signature_include { module_signature } :: rest ->
                let included_id =
                  Signature_id.create ~owner ~name:module_signature
                in
                (match expand visiting included_id with
                | Error _ as err -> err
                | Ok included -> collect (included :: bindings) rest)
          in
          collect [] items
  in
  expand Signature_set.empty signature_id

let signature_parameter_bindings ~scope env parameter_name signature_name =
  let signature_id =
    Signature_id.create
      ~owner:(if scope = "" then [] else [ scope ])
      ~name:signature_name
  in
  typed_signature_bindings (Env.modules env) signature_id
  |> Result.map
       (List.map (fun (value_path, binding) ->
            parameter_value_binding parameter_name value_path binding))

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
  | None -> []
