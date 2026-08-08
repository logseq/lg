module Symbol_map = Persistent_hash_map.Make (struct
  type t = Symbol_id.t

  let equal = Symbol_id.equal
  let hash = Hashtbl.hash
end)
module String_map = Persistent_hash_map.Make (struct
  type t = string

  let equal = String.equal
  let hash = Hashtbl.hash
end)

type t = {
  target : Target.t;
  symbols : Types.binding Symbol_map.t;
  record_symbols : Types.binding Symbol_map.t;
  record_bindings_by_lookup :
    (Symbol_id.t * Types.binding) list String_map.t;
  bindings_by_name : (Symbol_id.t * Types.binding) list String_map.t;
  bindings_by_emitted_name :
    (Symbol_id.t * Types.binding) list String_map.t;
  opened_bindings_by_scope :
    (Symbol_id.t * Types.binding) list String_map.t;
  protocols : Protocol_registry.t;
  protocol_evidence : Protocol_registry.t option;
  modules : Module_registry.t;
  types : Type_registry.t;
  signatures : Signature_overlay.t;
  anonymous_records : (string * Semantic_type.named_record) list;
  namespace_aliases : (string * string) list;
  core_exclusions : (string * string) list;
  macros : (string * Macro_definition.t) list;
  inline_macros : (string * Macro_definition.t) list;
  macro_functions : (string * Macro_definition.t) list;
  macro_values : (string * Ast.form) list;
  expected_type : Types.ty option;
}

let empty =
  {
    target = Target.default;
    symbols = Symbol_map.empty;
    record_symbols = Symbol_map.empty;
    record_bindings_by_lookup = String_map.empty;
    bindings_by_name = String_map.empty;
    bindings_by_emitted_name = String_map.empty;
    opened_bindings_by_scope = String_map.empty;
    protocols = Core_protocols.initial_registry;
    protocol_evidence = None;
    modules = Module_registry.empty;
    types = Type_registry.empty;
    signatures = Signature_overlay.empty;
    anonymous_records = [];
    namespace_aliases = [];
    core_exclusions = [];
    macros = [];
    inline_macros = [];
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

let remove_indexed_binding key id index =
  String_map.update key
    (function
      | None -> None
      | Some bindings -> (
          match
            List.filter (fun (candidate, _) -> not (Symbol_id.equal candidate id))
              bindings
          with
          | [] -> None
          | bindings -> Some bindings))
    index

let internal_scope ~prefix name =
  let scope_start = String.length prefix in
  if not (String.starts_with ~prefix name) then None
  else
    match String.rindex_opt name '/' with
    | Some separator when separator >= scope_start ->
        Some (String.sub name scope_start (separator - scope_start))
    | Some _ | None -> None

let record_lookup_index_key name (binding : Types.binding) =
  match (internal_scope ~prefix:"__record/" name, binding.ty) with
  | Some scope, Types.TNamed_record record ->
      Some (scope ^ "\000" ^ record.type_name)
  | Some _, _ | None, _ -> None

let opened_scope name =
  internal_scope ~prefix:"__opened/" name

let remove name env =
  let id = Symbol_id.of_string name in
  let previous = Symbol_map.find_opt id env.symbols in
  let bindings_by_emitted_name =
    match previous with
    | None -> env.bindings_by_emitted_name
    | Some binding ->
        remove_indexed_binding binding.Types.ocaml_name id
          env.bindings_by_emitted_name
  in
  let record_bindings_by_lookup =
    match Option.bind previous (record_lookup_index_key name) with
    | None -> env.record_bindings_by_lookup
    | Some key ->
        remove_indexed_binding key id env.record_bindings_by_lookup
  in
  let opened_bindings_by_scope =
    match opened_scope name with
    | None -> env.opened_bindings_by_scope
    | Some scope ->
        remove_indexed_binding scope id env.opened_bindings_by_scope
  in
  {
    env with
    symbols = Symbol_map.remove id env.symbols;
    record_symbols = Symbol_map.remove id env.record_symbols;
    record_bindings_by_lookup;
    bindings_by_name =
      remove_indexed_binding (Symbol_id.name id) id env.bindings_by_name;
    bindings_by_emitted_name;
    opened_bindings_by_scope;
  }

let add name binding env =
  let id = Symbol_id.of_string name in
  let previous = Symbol_map.find_opt id env.symbols in
  let bindings_by_name =
    remove_indexed_binding (Symbol_id.name id) id env.bindings_by_name
  in
  let bindings_by_name =
    String_map.update (Symbol_id.name id)
      (function
        | None -> Some [ (id, binding) ]
        | Some bindings -> Some ((id, binding) :: bindings))
      bindings_by_name
  in
  let bindings_by_emitted_name =
    match previous with
    | None -> env.bindings_by_emitted_name
    | Some previous ->
        remove_indexed_binding previous.Types.ocaml_name id
          env.bindings_by_emitted_name
  in
  let bindings_by_emitted_name =
    String_map.update binding.Types.ocaml_name
      (function
        | None -> Some [ (id, binding) ]
        | Some bindings -> Some ((id, binding) :: bindings))
      bindings_by_emitted_name
  in
  let record_bindings_by_lookup =
    match Option.bind previous (record_lookup_index_key name) with
    | None -> env.record_bindings_by_lookup
    | Some key ->
        remove_indexed_binding key id env.record_bindings_by_lookup
  in
  let record_bindings_by_lookup =
    match record_lookup_index_key name binding with
    | None -> record_bindings_by_lookup
    | Some key ->
        String_map.update key
          (function
            | None -> Some [ (id, binding) ]
            | Some bindings -> Some ((id, binding) :: bindings))
          record_bindings_by_lookup
  in
  let opened_bindings_by_scope =
    match opened_scope name with
    | None -> env.opened_bindings_by_scope
    | Some scope ->
        let index =
          remove_indexed_binding scope id env.opened_bindings_by_scope
        in
        String_map.update scope
          (function
            | None -> Some [ (id, binding) ]
            | Some bindings -> Some ((id, binding) :: bindings))
          index
  in
  {
    env with
    symbols = Symbol_map.add id binding env.symbols;
    record_symbols =
      (if String.starts_with ~prefix:"__record/" name then
         Symbol_map.add id binding env.record_symbols
       else Symbol_map.remove id env.record_symbols);
    record_bindings_by_lookup;
    bindings_by_name;
    bindings_by_emitted_name;
    opened_bindings_by_scope;
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

let find_map f env =
  Symbol_map.find_map
    (fun id binding -> f (Symbol_id.to_string id) binding)
    env.symbols

let filter_record_bindings f env =
  Symbol_map.fold
    (fun id binding result ->
      match f (Symbol_id.to_string id) binding with
      | None -> result
      | Some value -> value :: result)
    env.record_symbols []
  |> List.rev

let find_record_binding f env =
  Symbol_map.find_map
    (fun id binding -> f (Symbol_id.to_string id) binding)
    env.record_symbols

let bindings_named name env =
  String_map.find_opt name env.bindings_by_name
  |> Option.value ~default:[] |> List.map snd

let bindings_emitted_as name env =
  String_map.find_opt name env.bindings_by_emitted_name
  |> Option.value ~default:[]
  |> List.map (fun (id, binding) -> (Symbol_id.to_string id, binding))

let record_bindings_named ~scope ~type_name env =
  String_map.find_opt (scope ^ "\000" ^ type_name)
    env.record_bindings_by_lookup
  |> Option.value ~default:[] |> List.map snd

let opened_bindings scope env =
  String_map.find_opt scope env.opened_bindings_by_scope
  |> Option.value ~default:[] |> List.map snd

let protocols env = env.protocols
let with_protocols protocols env = { env with protocols }
let protocol_evidence env = env.protocol_evidence
let with_protocol_evidence protocol_evidence env =
  { env with protocol_evidence }
let modules env = env.modules
let with_modules modules env = { env with modules }
let types env = env.types
let with_types types env = { env with types }
let signatures env = env.signatures
let with_signatures signatures env = { env with signatures }

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

let remove_macro_alias ~alias definition env =
  match List.assoc_opt alias env.macros with
  | Some current when current = definition ->
      { env with macros = List.remove_assoc alias env.macros }
  | Some _ | None -> env

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

let add_inline_macro ~scope ~name definition env =
  let key = Names.scoped_key scope name in
  {
    env with
    inline_macros =
      (key, definition) :: List.remove_assoc key env.inline_macros;
  }

let add_inline_macro_alias ~alias definition env =
  {
    env with
    inline_macros =
      (alias, definition) :: List.remove_assoc alias env.inline_macros;
  }

let remove_inline_macro_alias ~alias definition env =
  match List.assoc_opt alias env.inline_macros with
  | Some current when current = definition ->
      { env with inline_macros = List.remove_assoc alias env.inline_macros }
  | Some _ | None -> env

let find_inline_macro ~scope name env =
  match List.assoc_opt (Names.scoped_key scope name) env.inline_macros with
  | Some _ as definition -> definition
  | None -> List.assoc_opt name env.inline_macros

let namespace_inline_macros namespace env =
  let prefix = namespace ^ "/" in
  env.inline_macros
  |> List.filter_map (fun (key, definition) ->
         if String.starts_with ~prefix key then
           let name =
             String.sub key (String.length prefix)
               (String.length key - String.length prefix)
           in
           Some (name, definition)
         else None)

let inline_macros env = env.inline_macros
let with_inline_macros inline_macros env = { env with inline_macros }

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
  | Types.TRecord left, Types.TNamed_record { nominal = false; fields = right; _ }
  | Types.TNamed_record { nominal = false; fields = left; _ }, Types.TRecord right ->
      anonymous_fields_equal left right
  | Types.TNullable left, Types.TNullable right
  | Types.TArray left, Types.TArray right
  | Types.TRef left, Types.TRef right
  | Types.TList left, Types.TList right
  | Types.TVector left, Types.TVector right
  | Types.TSet left, Types.TSet right
  | Types.TSeq left, Types.TSeq right ->
      anonymous_type_equal left right
  | Types.TOcaml_app (left_name, left_args),
    Types.TOcaml_app (right_name, right_args)
    when left_name = right_name ->
      List.length left_args = List.length right_args
      && List.for_all2 anonymous_type_equal left_args right_args
  | Types.TTuple left, Types.TTuple right ->
      List.length left = List.length right
      && List.for_all2 anonymous_type_equal left right
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

let rec anonymous_type_layout_compatible left right =
  match (left, right) with
  | (Types.TUnknown | Types.TMeta _ | Types.TVar _), _
  | _, (Types.TUnknown | Types.TMeta _ | Types.TVar _) ->
      true
  | Types.TRecord left, Types.TRecord right ->
      anonymous_fields_layout_compatible left right
  | Types.TRecord _, _ | _, Types.TRecord _ -> false
  | Types.TNullable left, Types.TNullable right
  | Types.TArray left, Types.TArray right
  | Types.TRef left, Types.TRef right
  | Types.TList left, Types.TList right
  | Types.TVector left, Types.TVector right
  | Types.TSet left, Types.TSet right
  | Types.TSeq left, Types.TSeq right ->
      anonymous_type_layout_compatible left right
  | Types.TOcaml_app (left_name, left_args),
    Types.TOcaml_app (right_name, right_args)
    when left_name = right_name ->
      List.length left_args = List.length right_args
      && List.for_all2 anonymous_type_layout_compatible left_args right_args
  | Types.TTuple left, Types.TTuple right ->
      List.length left = List.length right
      && List.for_all2 anonymous_type_layout_compatible left right
  | _ -> Types.ocaml_name left = Types.ocaml_name right

and anonymous_fields_layout_compatible left right =
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
      | Some right_field ->
          anonymous_type_layout_compatible left_field.ty right_field.ty
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

let find_anonymous_record_by_layout ~owner fields env =
  env.anonymous_records
  |> List.find_map (fun (record_owner, (record : Semantic_type.named_record)) ->
         if
           record_owner = owner
           && anonymous_fields_layout_compatible fields record.fields
         then Some record
         else None)

let add_anonymous_record ~owner record env =
  { env with anonymous_records = (owner, record) :: env.anonymous_records }
