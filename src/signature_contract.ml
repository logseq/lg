module Env = Compiler_environment

let declared_type module_path env name =
  let emitted_name =
    if module_path = "" then name else module_path ^ "." ^ name
  in
  let local_bindings =
    if module_path = "" then []
    else
      Env.bindings_emitted_as name env
      |> List.filter (fun (source_name, _) ->
             match String.rindex_opt source_name '/' with
             | None -> false
             | Some index ->
                 String.sub source_name 0 index
                 |> Names.module_path_to_ocaml
                 |> String.equal module_path)
  in
  (Env.bindings_emitted_as emitted_name env @ local_bindings)
  |> List.find_map (fun (source_name, (binding : Types.binding)) ->
         Signature_overlay.find_value source_name (Env.signatures env)
         |> Option.map (fun ty ->
                let scope =
                  match String.rindex_opt source_name '/' with
                  | None -> ""
                  | Some index -> String.sub source_name 0 index
                in
                let ty =
                  Function_elaborator.infer_named_record scope env ty
                  |> Type_solver.freshen_unknowns
                in
                if binding.dynamically_bindable then Types.TRef ty else ty))

let rec declared_pattern module_path env = function
  | Lowered.Named name as pattern -> (
      match declared_type module_path env name with
      | None -> pattern
      | Some ty -> Lowered.Declared_value (pattern, ty))
  | Lowered.Located_value (identity, location, pattern) ->
      Lowered.Located_value (identity, location, declared_pattern module_path env pattern)
  | (Lowered.Declared_value _ | Lowered.Unit_pattern | Lowered.Ignore_pattern)
    as pattern -> pattern

(* Keep the source declaration independent of inferred expression annotations.
   The backend checks its universally quantified variables on the binding,
   including when elaboration needed a contextual result type. *)
let rec annotate ?(module_path = "") env = function
  | Lowered.Value_binding binding ->
      Lowered.Value_binding
        { binding with pattern = declared_pattern module_path env binding.pattern }
  | Lowered.Recursive_value_binding binding ->
      let type_annotation =
        match declared_type module_path env binding.name with
        | Some _ as declaration -> declaration
        | None -> binding.type_annotation
      in
      Lowered.Recursive_value_binding { binding with type_annotation }
  | Lowered.Recursive_value_bindings bindings ->
      Lowered.Recursive_value_bindings
        (List.map
           (fun (binding : Lowered.recursive_value) ->
             match declared_type module_path env binding.name with
             | None -> binding
             | Some ty -> { binding with type_annotation = Some ty })
           bindings)
  | Lowered.Group items -> Lowered.Group (List.map (annotate ~module_path env) items)
  (* Modules attach contracts before exporting and qualifying their local types. *)
  | item -> item
