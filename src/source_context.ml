type entry = Ast.form * Location.t

module Form_table = Hashtbl.Make (struct
  type t = Ast.form

  let equal left right = left == right
  let hash = Hashtbl.hash
end)

let locations : Location.t Form_table.t Domain.DLS.key =
  Domain.DLS.new_key (fun () -> Form_table.create 0)

let find form = Form_table.find_opt (Domain.DLS.get locations) form

let with_locations entries f =
  let previous = Domain.DLS.get locations in
  let current = Form_table.create (List.length entries) in
  List.iter (fun (form, location) -> Form_table.replace current form location) entries;
  Domain.DLS.set locations current;
  Fun.protect ~finally:(fun () -> Domain.DLS.set locations previous) f
