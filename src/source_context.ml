type entry = Ast.form * Location.t

module Form_table = Hashtbl.Make (struct
  type t = Ast.form

  let equal left right = left == right

  let leaf_hash = function
    | Ast.FSymbol value -> Hashtbl.hash (0, value)
    | Ast.FCoreSymbol value -> Hashtbl.hash (1, value)
    | Ast.FKeyword value -> Hashtbl.hash (2, value)
    | Ast.FString value -> Hashtbl.hash (3, value)
    | Ast.FRegex value -> Hashtbl.hash (4, value)
    | Ast.FInt value -> Hashtbl.hash (5, value)
    | Ast.FFloat value -> Hashtbl.hash (6, value)
    | Ast.FDecimal value -> Hashtbl.hash (7, value)
    | Ast.FChar value -> Hashtbl.hash (8, value)
    | Ast.FBool value -> Hashtbl.hash (9, value)
    | Ast.FList _ -> 10
    | Ast.FVector _ -> 11
    | Ast.FMap _ -> 12

  let hash = function
    | (Ast.FSymbol _ | Ast.FCoreSymbol _ | Ast.FKeyword _ | Ast.FString _
      | Ast.FRegex _ | Ast.FInt _ | Ast.FFloat _ | Ast.FDecimal _ | Ast.FChar _
      | Ast.FBool _)
      as form ->
        leaf_hash form
    | Ast.FList forms -> (
        match forms with
        | head :: _ -> Hashtbl.hash (9, List.length forms, leaf_hash head)
        | [] -> 9)
    | Ast.FVector forms -> Hashtbl.hash (10, List.length forms)
    | Ast.FMap fields -> (
        match fields with
        | (key, _) :: _ ->
            Hashtbl.hash (11, List.length fields, leaf_hash key)
        | [] -> 12)
end)

let locations : Location.t Form_table.t Domain.DLS.key =
  Domain.DLS.new_key (fun () -> Form_table.create 0)

let source_unit : string option Domain.DLS.key =
  Domain.DLS.new_key (fun () -> None)

let find form = Form_table.find_opt (Domain.DLS.get locations) form

let anonymous_record_owner owner =
  match Domain.DLS.get source_unit with
  | None -> owner
  | Some source_unit when owner = "" -> source_unit
  | Some source_unit -> source_unit ^ "." ^ owner

let with_source_unit unit_id f =
  let previous = Domain.DLS.get source_unit in
  Domain.DLS.set source_unit (Some unit_id);
  Fun.protect ~finally:(fun () -> Domain.DLS.set source_unit previous) f

let with_locations entries f =
  let previous = Domain.DLS.get locations in
  let current = Form_table.create (List.length entries) in
  List.iter (fun (form, location) -> Form_table.replace current form location) entries;
  Domain.DLS.set locations current;
  Fun.protect ~finally:(fun () -> Domain.DLS.set locations previous) f
