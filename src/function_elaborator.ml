open Types
open Expression_support

module Env = Compiler_environment

let prepare ?(param_type_overrides = []) ~lookup_function_ty ~compile_body scope env
    params body_forms =
  match Destructure.parse_param_specs params with
  | Error _ as err -> err
  | Ok specs ->
      let inference_params =
        specs
        |> List.fold_left
             (fun acc (spec : Destructure.param_spec) ->
               let param_ty = Option.value spec.explicit_ty ~default:TUnknown in
               let acc = (spec.source_name, param_ty) :: acc in
               if spec.destructured then
                 Destructure.pattern_names spec.pattern
                 |> List.fold_left (fun acc name -> (name, TUnknown) :: acc) acc
               else acc)
             []
        |> List.rev
      in
      match Type_inference.infer_params ~lookup_function_ty inference_params body_forms with
      | Error _ as err -> err
      | Ok inferred ->
          let lookup_inferred name =
            inferred |> List.assoc_opt name |> Option.value ~default:TUnknown
          in
          let infer_spec_ty (spec : Destructure.param_spec) =
            if spec.destructured then
              Destructure.infer_pattern_type spec.pattern lookup_inferred
            else Ok (lookup_inferred spec.source_name)
          in
          let rec build acc = function
            | [] -> Ok (List.rev acc)
            | spec :: rest -> (
                match infer_spec_ty spec with
                | Error _ as err -> err
                | Ok ty -> build ((spec, ty) :: acc) rest)
          in
          match build [] specs with
          | Error _ as err -> err
          | Ok typed_specs ->
              let typed_specs =
                typed_specs
                |> List.mapi (fun index (spec, inferred_ty) ->
                       match List.nth_opt param_type_overrides index with
                       | Some (Some ty) -> (spec, ty)
                       | _ -> (spec, inferred_ty))
              in
              let param_bindings =
                typed_specs
                |> List.map (fun ((spec : Destructure.param_spec), ty) ->
                       ( Names.scoped_key scope spec.source_name,
                         Types.binding spec.ocaml_name ty ))
              in
              let param_identities =
                typed_specs
                |> List.map (fun ((spec : Destructure.param_spec), _) ->
                       spec.identity)
              in
              let param_targets =
                typed_specs
                |> List.map (fun ((spec : Destructure.param_spec), ty) ->
                       (spec, typed_ir ty (Semantic_ir.Ident spec.ocaml_name)))
              in
              let destructured_bindings =
                let rec loop acc = function
                  | [] -> Ok (List.rev acc)
                  | (spec, target) :: rest ->
                      if not spec.Destructure.destructured then loop acc rest
                      else (
                        match Destructure.bind_pattern target spec.pattern with
                        | Error _ as err -> err
                        | Ok bindings -> loop (List.rev_append bindings acc) rest)
                in
                loop [] param_targets
              in
              (match destructured_bindings with
              | Error _ as err -> err
              | Ok destructured_bindings ->
                  let local_bindings =
                    destructured_bindings
                    |> List.map (fun (binding : Destructure.local_binding) ->
                           ( Names.scoped_key scope binding.source_name,
                             Types.binding binding.ocaml_name binding.ty ))
                  in
                  let env =
                    env |> Env.add_bindings param_bindings
                    |> Env.add_bindings local_bindings
                  in
                  match
                    compile_body scope env "function body requires at least one form"
                      body_forms
                  with
                  | Error _ as err -> err
                  | Ok body ->
                      Ok
                        { param_bindings;
                          param_identities;
                          destructured_bindings;
                          body;
                        })

let fn_code ?(row_param_type_names = []) parts =
  let param_names =
    parts.param_bindings |> List.map (fun (_key, binding) -> binding.ocaml_name)
  in
  let param_tys =
    parts.param_bindings |> List.map (fun (_key, (binding : binding)) -> binding.ty)
  in
  let param_patterns =
    List.map2 (fun name ty -> (name, ty)) param_names param_tys
    |> List.mapi (fun index (name, ty) ->
           let pattern =
             match List.nth_opt row_param_type_names index with
             | Some (Some type_name) ->
                 Semantic_ir.PConstraint (Semantic_ir.PVar name, type_name)
             | _ -> (
                 match param_constraint_name ty with
                 | Some type_name ->
                     Semantic_ir.PConstraint (Semantic_ir.PVar name, type_name)
                 | None -> Semantic_ir.PVar name)
           in
           match List.nth_opt parts.param_identities index |> Option.join with
           | None -> pattern
           | Some (node_id, location) ->
               Semantic_ir.PLocated (node_id, location, pattern))
  in
  let body_expr =
    match parts.destructured_bindings with
    | [] -> parts.body.semantic_expr
    | bindings ->
        Semantic_ir.Let
          ( List.map
              (fun (binding : Destructure.local_binding) ->
                let pattern = Semantic_ir.PVar binding.ocaml_name in
                let pattern =
                  match binding.identity with
                  | None -> pattern
                  | Some (node_id, location) ->
                      Semantic_ir.PLocated (node_id, location, pattern)
                in
                (pattern, binding.semantic_expr))
              bindings,
            parts.body.semantic_expr )
  in
  let return_param_index =
    match
      (parts.destructured_bindings, Semantic_ir.unlocated parts.body.semantic_expr)
    with
    | [], Semantic_ir.Ident returned_name ->
        param_names
        |> List.mapi (fun index name -> (index, name))
        |> List.find_opt (fun (_index, name) -> name = returned_name)
        |> Option.map fst
    | _ -> None
  in
  {
    (typed_ir (TFn (param_tys, parts.body.ty))
       (Semantic_ir.Fun (param_patterns, body_expr))) with
    return_param_index;
  }
