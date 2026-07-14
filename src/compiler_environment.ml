module Symbol_map = Map.Make (Symbol_id)

type t = {
  symbols : Types.binding Symbol_map.t;
  protocols : Protocol_registry.t;
  modules : Module_registry.t;
  types : Type_registry.t;
}

let empty =
  {
    symbols = Symbol_map.empty;
    protocols = Core_protocols.initial_registry;
    modules = Module_registry.empty;
    types = Type_registry.empty;
  }

let find_opt name env =
  Symbol_map.find_opt (Symbol_id.of_string name) env.symbols

let mem name env = Symbol_map.mem (Symbol_id.of_string name) env.symbols

let remove name env =
  { env with symbols = Symbol_map.remove (Symbol_id.of_string name) env.symbols }

let add name binding env =
  {
    env with
    symbols = Symbol_map.add (Symbol_id.of_string name) binding env.symbols;
  }

let add_bindings bindings env =
  List.fold_left (fun env (name, binding) -> add name binding env) env bindings

let of_bindings bindings = add_bindings bindings empty

let to_bindings env =
  Symbol_map.bindings env.symbols
  |> List.map (fun (id, binding) -> (Symbol_id.to_string id, binding))

let fold f env initial =
  Symbol_map.fold
    (fun id binding state -> f (Symbol_id.to_string id) binding state)
    env.symbols initial

let filter_map f env =
  fold
    (fun name binding result ->
      match f name binding with None -> result | Some value -> value :: result)
    env []
  |> List.rev

let protocols env = env.protocols
let with_protocols protocols env = { env with protocols }
let modules env = env.modules
let with_modules modules env = { env with modules }
let types env = env.types
let with_types types env = { env with types }
