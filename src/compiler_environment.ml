module Symbol_map = Map.Make (Symbol_id)

type t = {
  target : Target.t;
  symbols : Types.binding Symbol_map.t;
  protocols : Protocol_registry.t;
  protocol_evidence : Protocol_registry.t option;
  modules : Module_registry.t;
  types : Type_registry.t;
  anonymous_records : (string * Semantic_type.named_record) list;
  namespace_aliases : (string * string) list;
  core_exclusions : (string * string) list;
  macros : (string * Macro_definition.t) list;
  macro_functions : (string * Macro_definition.t) list;
  macro_values : (string * Ast.form) list;
  expected_type : Types.ty option;
}

let empty =
  {
    target = Target.default;
    symbols = Symbol_map.empty;
    protocols = Core_protocols.initial_registry;
    protocol_evidence = None;
    modules = Module_registry.empty;
    types = Type_registry.empty;
    anonymous_records = [];
    namespace_aliases = [];
    core_exclusions = [];
    macros = [];
    macro_functions = [];
    macro_values = [];
    expected_type = None;
  }

let target env = env.target
let with_target target env = { env with target }
let expected_type env = env.expected_type
let with_expected_type expected_type env = { env with expected_type }

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
let protocol_evidence env = env.protocol_evidence
let with_protocol_evidence protocol_evidence env =
  { env with protocol_evidence }
let modules env = env.modules
let with_modules modules env = { env with modules }
let types env = env.types
let with_types types env = { env with types }

let add_namespace_alias ~scope ~alias ~target env =
  let key = Names.scoped_key scope alias in
  { env with namespace_aliases = (key, target) :: env.namespace_aliases }

let resolve_namespace_alias ~scope alias env =
  match List.assoc_opt (Names.scoped_key scope alias) env.namespace_aliases with
  | Some _ as target -> target
  | None -> List.assoc_opt alias env.namespace_aliases

let add_core_exclusions ~scope names env =
  let exclusions =
    List.map (fun name -> (Names.scoped_key scope name, name)) names
  in
  { env with core_exclusions = exclusions @ env.core_exclusions }

let core_excluded ~scope name env =
  List.mem_assoc (Names.scoped_key scope name) env.core_exclusions

let add_macro ~scope ~name definition env =
  let key = Names.scoped_key scope name in
  { env with macros = (key, definition) :: List.remove_assoc key env.macros }

let add_macro_alias ~alias definition env =
  { env with macros = (alias, definition) :: List.remove_assoc alias env.macros }

let find_macro ~scope name env =
  match List.assoc_opt (Names.scoped_key scope name) env.macros with
  | Some _ as definition -> definition
  | None -> List.assoc_opt name env.macros

let namespace_macros namespace env =
  let prefix = namespace ^ "/" in
  env.macros
  |> List.filter_map (fun (key, definition) ->
         if String.starts_with ~prefix key then
           let name =
             String.sub key (String.length prefix)
               (String.length key - String.length prefix)
           in
           Some (name, definition)
         else None)

let add_macro_function ~scope ~name definition env =
  let key = Names.scoped_key scope name in
  {
    env with
    macro_functions =
      (key, definition) :: List.remove_assoc key env.macro_functions;
  }

let find_macro_function ~scope name env =
  match List.assoc_opt (Names.scoped_key scope name) env.macro_functions with
  | Some _ as definition -> definition
  | None -> List.assoc_opt name env.macro_functions

let add_macro_value ~scope ~name value env =
  let key = Names.scoped_key scope name in
  { env with macro_values = (key, value) :: List.remove_assoc key env.macro_values }

let find_macro_value ~scope name env =
  match List.assoc_opt (Names.scoped_key scope name) env.macro_values with
  | Some _ as value -> value
  | None -> List.assoc_opt name env.macro_values

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
