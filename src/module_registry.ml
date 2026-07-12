module Signature_map = Map.Make (Signature_id)
module Functor_map = Map.Make (Functor_id)
module Module_map = Map.Make (Module_id)
module Module_set = Set.Make (Module_id)
module Emitted_module_map = Map.Make (String)
module Emitted_signature_map = Map.Make (String)

type module_kind = Concrete | Alias | Functor | Applied

type module_declaration = {
  module_id : Module_id.t;
  kind : module_kind;
}

type t = {
  signatures : Lowered.signature_item list Signature_map.t;
  emitted_signatures : Signature_id.t Emitted_signature_map.t;
  functor_results : (string * Types.binding) list Functor_map.t;
  functor_protocols : Protocol_registry.t Functor_map.t;
  aliases : Module_id.t Module_map.t;
  module_declarations : module_declaration Emitted_module_map.t;
}

let empty =
  {
    signatures = Signature_map.empty;
    emitted_signatures = Emitted_signature_map.empty;
    functor_results = Functor_map.empty;
    functor_protocols = Functor_map.empty;
    aliases = Module_map.empty;
    module_declarations = Emitted_module_map.empty;
  }

let declare_signature signature_id items registry =
  if Signature_map.mem signature_id registry.signatures then
    Error.error
      ("duplicate module signature " ^ Signature_id.to_string signature_id)
  else
    let emitted_name =
      String.concat "."
        (Signature_id.owner signature_id @ [ Signature_id.name signature_id ])
      |> Names.module_path_to_ocaml
    in
    match Emitted_signature_map.find_opt emitted_name registry.emitted_signatures with
    | Some existing ->
        Error.error
          ("OCaml module type name collision: " ^ Signature_id.to_string existing
         ^ " and " ^ Signature_id.to_string signature_id ^ " both emit "
         ^ emitted_name)
    | None ->
        Ok
          {
            registry with
            signatures = Signature_map.add signature_id items registry.signatures;
            emitted_signatures =
              Emitted_signature_map.add emitted_name signature_id
                registry.emitted_signatures;
          }

let find_signature signature_id registry =
  Signature_map.find_opt signature_id registry.signatures

let find_signature_named ~owner name registry =
  Signature_map.to_seq registry.signatures
  |> Seq.find_map (fun (signature_id, items) ->
         if
           Signature_id.owner signature_id = owner
           && (Signature_id.name signature_id = name
              || Names.module_path_to_ocaml (Signature_id.name signature_id) = name)
         then Some (signature_id, items)
         else None)

let store_functor_result functor_id bindings registry =
  {
    registry with
    functor_results = Functor_map.add functor_id bindings registry.functor_results;
  }

let find_functor_result functor_id registry =
  Functor_map.find_opt functor_id registry.functor_results

let store_functor_protocols functor_id protocols registry =
  {
    registry with
    functor_protocols = Functor_map.add functor_id protocols registry.functor_protocols;
  }

let find_functor_protocols functor_id registry =
  Functor_map.find_opt functor_id registry.functor_protocols

let emitted_module_name module_id =
  String.concat "." (Module_id.owner module_id @ [ Module_id.name module_id ])
  |> Names.module_path_to_ocaml

let declare_module module_id kind registry =
  let emitted_name = emitted_module_name module_id in
  match Emitted_module_map.find_opt emitted_name registry.module_declarations with
  | Some existing when Module_id.equal existing.module_id module_id ->
      Error.error ("duplicate module " ^ Module_id.to_string module_id)
  | Some existing ->
      Error.error
        ("OCaml module name collision: " ^ Module_id.to_string existing.module_id
       ^ " and " ^ Module_id.to_string module_id ^ " both emit " ^ emitted_name)
  | None ->
      Ok
        {
          registry with
          module_declarations =
            Emitted_module_map.add emitted_name { module_id; kind }
              registry.module_declarations;
        }

let add_alias alias target registry =
  { registry with aliases = Module_map.add alias target registry.aliases }

let declare_alias alias target registry =
  declare_module alias Alias registry
  |> Result.map (add_alias alias target)

let find_alias alias registry = Module_map.find_opt alias registry.aliases

let resolve_alias ~scope module_path registry =
  let top = Module_id.create ~owner:[] ~name:module_path in
  let scoped =
    if scope = "" then top
    else Module_id.create ~owner:[ scope ] ~name:module_path
  in
  let initial =
    if Module_map.mem scoped registry.aliases then Some scoped
    else if Module_map.mem top registry.aliases then Some top
    else None
  in
  let rec resolve visited module_id =
    if Module_set.mem module_id visited then None
    else
      match Module_map.find_opt module_id registry.aliases with
      | None -> Some module_id
      | Some target ->
          let target =
            if scope <> "" && Module_id.owner target = [] then
              let local =
                Module_id.create ~owner:[ scope ] ~name:(Module_id.name target)
              in
              if Module_map.mem local registry.aliases then local else target
            else target
          in
          resolve (Module_set.add module_id visited) target
  in
  Option.bind initial (resolve Module_set.empty)
