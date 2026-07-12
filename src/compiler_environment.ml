module Symbol_map = Map.Make (Symbol_id)

type t = Types.binding Symbol_map.t

let empty = Symbol_map.empty
let find_opt name env = Symbol_map.find_opt (Symbol_id.of_string name) env
let mem name env = Symbol_map.mem (Symbol_id.of_string name) env
let remove name env = Symbol_map.remove (Symbol_id.of_string name) env

let add name binding env =
  Symbol_map.add (Symbol_id.of_string name) binding env

let add_bindings bindings env =
  List.fold_left (fun env (name, binding) -> add name binding env) env bindings

let of_bindings bindings = add_bindings bindings empty

let to_bindings env =
  Symbol_map.bindings env
  |> List.map (fun (id, binding) -> (Symbol_id.to_string id, binding))

let fold f env initial =
  Symbol_map.fold
    (fun id binding state -> f (Symbol_id.to_string id) binding state)
    env initial

let filter_map f env =
  fold
    (fun name binding result ->
      match f name binding with None -> result | Some value -> value :: result)
    env []
  |> List.rev
