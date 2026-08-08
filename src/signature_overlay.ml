module String_map = Map.Make (String)

type entry = Record of Types.field list | Value of Types.ty
type t = entry String_map.t

let empty = String_map.empty

let add name entry overlays =
  if String_map.mem name overlays then
    Error.error ("duplicate sidecar signature " ^ name)
  else Ok (String_map.add name entry overlays)

let find name overlays = String_map.find_opt name overlays

let find_record name overlays =
  match find name overlays with Some (Record fields) -> Some fields | _ -> None

let find_value name overlays =
  match find name overlays with Some (Value ty) -> Some ty | _ -> None

let value_names overlays =
  String_map.fold
    (fun name entry names ->
      match entry with Value _ -> name :: names | Record _ -> names)
    overlays []
