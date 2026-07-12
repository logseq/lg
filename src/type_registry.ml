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
