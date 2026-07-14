module Symbol_map = Map.Make (Symbol_id)

type t = {
  symbols : Types.binding Symbol_map.t;
  protocols : Protocol_registry.t;
  modules : Module_registry.t;
  types : Type_registry.t;
  anonymous_records : (string * Semantic_type.named_record) list;
}

let empty =
  {
    symbols = Symbol_map.empty;
    protocols = Core_protocols.initial_registry;
    modules = Module_registry.empty;
    types = Type_registry.empty;
    anonymous_records = [];
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

let rec anonymous_type_equal left right =
  match (left, right) with
  | Types.TRecord left, Types.TRecord right -> anonymous_fields_equal left right
  | _ -> Types.equal left right

and anonymous_fields_equal left right =
  List.length left = List.length right
  &&
  List.for_all
    (fun (left_field : Types.field) ->
      match
        List.find_opt
          (fun (right_field : Types.field) ->
            right_field.keyword = left_field.keyword)
          right
      with
      | Some right_field -> anonymous_type_equal left_field.ty right_field.ty
      | None -> false)
    left

let find_anonymous_record ~owner fields env =
  env.anonymous_records
  |> List.find_map (fun (record_owner, (record : Semantic_type.named_record)) ->
         if
           record_owner = owner
           && anonymous_fields_equal fields record.fields
         then Some record
         else None)

let add_anonymous_record ~owner record env =
  { env with anonymous_records = (owner, record) :: env.anonymous_records }
