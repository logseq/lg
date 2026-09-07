module Env = Compiler_environment

let mark_private ?(module_path = "") env item =
  let hide env name =
    let name = if module_path = "" then name else module_path ^ "." ^ name in
    Env.hide_export name env
  in
  let rec pattern env = function
    | Lowered.Named name -> hide env name
    | Lowered.Declared_value (inner, _) | Lowered.Located_value (_, _, inner) ->
        pattern env inner
    | Lowered.Unit_pattern | Lowered.Ignore_pattern -> env
  in
  let rec mark env = function
    | Lowered.Group items -> List.fold_left mark env items
    | Lowered.Value_binding { pattern = binding; _ } -> pattern env binding
    | Lowered.Recursive_value_binding { name; _ }
    | Lowered.Deferred_value_binding { name; _ } ->
        hide env name
    | Lowered.Recursive_value_bindings bindings ->
        List.fold_left
          (fun env (binding : Lowered.recursive_value) -> hide env binding.name)
          env bindings
    | _ -> env
  in
  mark env item
