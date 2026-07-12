module Signature_map = Map.Make (Signature_id)
module Functor_map = Map.Make (Functor_id)
module Module_map = Map.Make (Module_id)

type t = {
  signatures : Lowered.signature_item list Signature_map.t;
  functor_results : (string * Types.binding) list Functor_map.t;
  aliases : Module_id.t Module_map.t;
}

let empty =
  {
    signatures = Signature_map.empty;
    functor_results = Functor_map.empty;
    aliases = Module_map.empty;
  }

let declare_signature signature_id items registry =
  if Signature_map.mem signature_id registry.signatures then
    Error.error
      ("duplicate module signature " ^ Signature_id.to_string signature_id)
  else
    Ok
      {
        registry with
        signatures = Signature_map.add signature_id items registry.signatures;
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

let add_alias alias target registry =
  { registry with aliases = Module_map.add alias target registry.aliases }

let find_alias alias registry = Module_map.find_opt alias registry.aliases
