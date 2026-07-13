module Emitted_map = Map.Make (String)

type kind = Alias | Record | Variant

type declaration = {
  type_id : Type_id.t;
  kind : kind;
}

type t = declaration Emitted_map.t

let empty = Emitted_map.empty

let emitted_name ~scope source_name =
  let name = Names.sanitize_name source_name in
  if scope = "" then name
  else Names.module_path_to_ocaml scope ^ "." ^ name

let declare ~scope source_name kind registry =
  let type_id =
    Type_id.create ~owner:(if scope = "" then [] else [ scope ])
      ~name:source_name
  in
  let emitted_name = emitted_name ~scope source_name in
  match Emitted_map.find_opt emitted_name registry with
  | Some existing when Type_id.equal existing.type_id type_id ->
      Error.error ("duplicate type " ^ Type_id.to_string type_id)
  | Some existing ->
      Error.error
        ("OCaml type name collision: " ^ Type_id.to_string existing.type_id
       ^ " and " ^ Type_id.to_string type_id ^ " both emit " ^ emitted_name)
  | None ->
      Ok
        ( type_id,
          Emitted_map.add emitted_name { type_id; kind } registry )

let find_by_emitted_name emitted_name registry =
  Emitted_map.find_opt emitted_name registry

let export_scope ~from_scope ~to_scope source target =
  let remap_scope owner =
    match owner with
    | [ path ] when path = from_scope -> Some to_scope
    | [ path ] when String.starts_with ~prefix:(from_scope ^ ".") path ->
        Some
          (to_scope
          ^ String.sub path (String.length from_scope)
              (String.length path - String.length from_scope))
    | _ -> None
  in
  Emitted_map.fold
    (fun _ declaration result ->
      match (result, remap_scope (Type_id.owner declaration.type_id)) with
      | (Error _ as err), _ -> err
      | Ok registry, None -> Ok registry
      | Ok registry, Some scope ->
          declare ~scope (Type_id.name declaration.type_id) declaration.kind registry
          |> Result.map snd)
    source (Ok target)
