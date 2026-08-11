module Symbol_map = Persistent_hash_map.Make (struct
  type t = Symbol_id.t

  let equal = Symbol_id.equal
  let hash = Symbol_id.hash
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
  record_bindings_by_lookup : Symbol_id.t list String_map.t;
  bindings_by_name : Symbol_id.t list String_map.t;
  bindings_by_emitted_name : Symbol_id.t list String_map.t;
  bindings_by_namespace : Symbol_id.t list String_map.t;
  opened_bindings_by_scope : Symbol_id.t list String_map.t;
  unresolved_declaration_names : int String_map.t;
  unresolved_declaration_binding_names : int String_map.t;
  explicit_declaration_names : unit String_map.t;
  protocols : Protocol_registry.t;
  protocol_evidence : Protocol_registry.t option;
  modules : Module_registry.t;
  types : Type_registry.t;
  signatures : Signature_overlay.t;
  anonymous_records : (string * Semantic_type.named_record) list;
  namespace_aliases : string String_map.t;
  core_exclusions : unit String_map.t;
  macros : Macro_definition.t String_map.t;
  inline_macros : Macro_definition.t String_map.t;
  macro_functions : Macro_definition.t String_map.t;
  macro_values : Ast.form String_map.t;
  expected_type : Types.ty option;
  source_macros_expanded : bool;
}

let empty =
  {
    target = Target.default;
    symbols = Symbol_map.empty;
    record_symbols = Symbol_map.empty;
    record_bindings_by_lookup = String_map.empty;
    bindings_by_name = String_map.empty;
    bindings_by_emitted_name = String_map.empty;
    bindings_by_namespace = String_map.empty;
    opened_bindings_by_scope = String_map.empty;
    unresolved_declaration_names = String_map.empty;
    unresolved_declaration_binding_names = String_map.empty;
    explicit_declaration_names = String_map.empty;
    protocols = Core_protocols.initial_registry;
    protocol_evidence = None;
    modules = Module_registry.empty;
    types = Type_registry.empty;
    signatures = Signature_overlay.empty;
    anonymous_records = [];
    namespace_aliases = String_map.empty;
    core_exclusions = String_map.empty;
    macros = String_map.empty;
    inline_macros = String_map.empty;
    macro_functions = String_map.empty;
    macro_values = String_map.empty;
    expected_type = None;
    source_macros_expanded = false;
  }

let target env = env.target
let with_target target env = { env with target }
let expected_type env = env.expected_type
let with_expected_type expected_type env = { env with expected_type }
let source_macros_expanded env = env.source_macros_expanded

let with_source_macros_expanded source_macros_expanded env =
  { env with source_macros_expanded }

let find_opt name env =
  Symbol_map.find_opt (Symbol_id.of_string name) env.symbols

let mem name env = Symbol_map.mem (Symbol_id.of_string name) env.symbols

let remove_indexed_binding key id index =
  let rec remove = function
    | [] as bindings -> bindings
    | candidate :: rest when Symbol_id.equal candidate id -> rest
    | binding :: rest as bindings ->
        let updated_rest = remove rest in
        if updated_rest == rest then bindings else binding :: updated_rest
  in
  match String_map.find_opt key index with
  | None -> index
  | Some bindings ->
      let updated = remove bindings in
      if updated == bindings then index
      else if updated = [] then String_map.remove key index
      else String_map.add key updated index

let add_indexed_binding key id index =
  String_map.update key
    (function
      | None -> Some [ id ]
      | Some bindings -> Some (id :: bindings))
    index

let update_name_count delta name counts =
  String_map.update name
    (function
      | None when delta > 0 -> Some delta
      | None -> None
      | Some count ->
          let count = count + delta in
          if count = 0 then None else Some count)
    counts

let update_name_counts delta names counts =
  List.fold_left (fun counts name -> update_name_count delta name counts) counts
    names

let binding_unresolved_declaration_names (binding : Types.binding) =
  match binding.ty with
  | Types.TOcaml "__declared_fn" -> [ binding.ocaml_name ]
  | _ when binding.forward_declared -> [ binding.ocaml_name ]
  | _ -> []

let binding_unresolved_declaration_reference_names (binding : Types.binding) =
  match binding.ty with
  | Types.TOcaml "__declared_fn" -> [ binding.ocaml_name ]
  | _ when binding.forward_declared ->
      List.sort_uniq String.compare
        (binding.ocaml_name :: binding.overload_targets)
  | _ -> []

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

let namespace_prefixes name =
  let record_prefix = "__record/" in
  let name =
    if String.starts_with ~prefix:record_prefix name then
      String.sub name (String.length record_prefix)
        (String.length name - String.length record_prefix)
    else name
  in
  let rec dotted_prefixes namespace prefixes =
    match String.rindex_opt namespace '.' with
    | None -> prefixes
    | Some separator ->
        let namespace = String.sub namespace 0 separator in
        dotted_prefixes namespace (namespace :: prefixes)
  in
  let rec collect offset prefixes =
    match String.index_from_opt name offset '/' with
    | None -> List.sort_uniq String.compare prefixes
    | Some separator ->
        let prefix = String.sub name 0 separator in
        collect (separator + 1) (dotted_prefixes prefix (prefix :: prefixes))
  in
  collect 0 []

let update_namespace_index update id name index =
  List.fold_left (fun index namespace -> update namespace id index) index
    (namespace_prefixes name)

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
  let bindings_by_namespace =
    match previous with
    | None -> env.bindings_by_namespace
    | Some _ ->
        update_namespace_index remove_indexed_binding id name
          env.bindings_by_namespace
  in
  let unresolved_declaration_names =
    match previous with
    | None -> env.unresolved_declaration_names
    | Some binding ->
        update_name_counts (-1)
          (binding_unresolved_declaration_reference_names binding)
          env.unresolved_declaration_names
  in
  let unresolved_declaration_binding_names =
    match previous with
    | None -> env.unresolved_declaration_binding_names
    | Some binding ->
        update_name_counts (-1)
          (binding_unresolved_declaration_names binding)
          env.unresolved_declaration_binding_names
  in
  {
    env with
    symbols = Symbol_map.remove id env.symbols;
    record_symbols = Symbol_map.remove id env.record_symbols;
    record_bindings_by_lookup;
    bindings_by_name =
      remove_indexed_binding (Symbol_id.name id) id env.bindings_by_name;
    bindings_by_emitted_name;
    bindings_by_namespace;
    opened_bindings_by_scope;
    unresolved_declaration_names;
    unresolved_declaration_binding_names;
  }

let add name binding env =
  let id = Symbol_id.of_string name in
  let previous = Symbol_map.find_opt id env.symbols in
  let bindings_by_name =
    match previous with
    | None ->
        add_indexed_binding (Symbol_id.name id) id env.bindings_by_name
    | Some _ -> env.bindings_by_name
  in
  let bindings_by_emitted_name =
    match previous with
    | Some previous
      when String.equal previous.Types.ocaml_name binding.Types.ocaml_name ->
        env.bindings_by_emitted_name
    | None -> env.bindings_by_emitted_name
    | Some previous ->
        remove_indexed_binding previous.Types.ocaml_name id
          env.bindings_by_emitted_name
  in
  let bindings_by_emitted_name =
    match previous with
    | Some previous
      when String.equal previous.Types.ocaml_name binding.Types.ocaml_name ->
        bindings_by_emitted_name
    | None | Some _ ->
        add_indexed_binding binding.Types.ocaml_name id bindings_by_emitted_name
  in
  let previous_record_key = Option.bind previous (record_lookup_index_key name) in
  let next_record_key = record_lookup_index_key name binding in
  let record_bindings_by_lookup =
    match (previous_record_key, next_record_key) with
    | Some previous_key, Some next_key when String.equal previous_key next_key ->
        env.record_bindings_by_lookup
    | None, _ -> env.record_bindings_by_lookup
    | Some key, _ ->
        remove_indexed_binding key id env.record_bindings_by_lookup
  in
  let record_bindings_by_lookup =
    match (previous_record_key, next_record_key) with
    | Some previous_key, Some next_key when String.equal previous_key next_key ->
        record_bindings_by_lookup
    | _, None -> record_bindings_by_lookup
    | _, Some key ->
        add_indexed_binding key id record_bindings_by_lookup
  in
  let opened_bindings_by_scope =
    match (previous, opened_scope name) with
    | Some _, Some _ -> env.opened_bindings_by_scope
    | None, Some scope ->
        add_indexed_binding scope id env.opened_bindings_by_scope
    | _, None -> env.opened_bindings_by_scope
  in
  let bindings_by_namespace =
    match previous with
    | Some _ -> env.bindings_by_namespace
    | None ->
        update_namespace_index add_indexed_binding id name
          env.bindings_by_namespace
  in
  let unresolved_declaration_names =
    match previous with
    | None -> env.unresolved_declaration_names
    | Some previous ->
        update_name_counts (-1)
          (binding_unresolved_declaration_reference_names previous)
          env.unresolved_declaration_names
  in
  let unresolved_declaration_names =
    update_name_counts 1 (binding_unresolved_declaration_reference_names binding)
      unresolved_declaration_names
  in
  let unresolved_declaration_binding_names =
    match previous with
    | None -> env.unresolved_declaration_binding_names
    | Some previous ->
        update_name_counts (-1)
          (binding_unresolved_declaration_names previous)
          env.unresolved_declaration_binding_names
  in
  let unresolved_declaration_binding_names =
    update_name_counts 1
      (binding_unresolved_declaration_names binding)
      unresolved_declaration_binding_names
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
    bindings_by_namespace;
    opened_bindings_by_scope;
    unresolved_declaration_names;
    unresolved_declaration_binding_names;
  }

let add_bindings bindings env =
  List.fold_left
    (fun env (name, binding) ->
      match find_opt name env with
      | Some previous when previous == binding -> env
      | Some _ | None -> add name binding env)
    env bindings

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

let binding_entries_named name env =
  String_map.find_opt name env.bindings_by_name
  |> Option.value ~default:[]
  |> List.filter_map (fun id ->
         Symbol_map.find_opt id env.symbols
         |> Option.map (fun binding -> (Symbol_id.to_string id, binding)))

let bindings_named name env =
  binding_entries_named name env |> List.map snd

let namespace_binding_entries namespace env =
  String_map.find_opt namespace env.bindings_by_namespace
  |> Option.value ~default:[]
  |> List.filter_map (fun id ->
         Symbol_map.find_opt id env.symbols
         |> Option.map (fun binding -> (Symbol_id.to_string id, binding)))

let bindings_emitted_as name env =
  String_map.find_opt name env.bindings_by_emitted_name
  |> Option.value ~default:[]
  |> List.filter_map (fun id ->
         Symbol_map.find_opt id env.symbols
         |> Option.map (fun binding -> (Symbol_id.to_string id, binding)))

let record_bindings_named ~scope ~type_name env =
  String_map.find_opt (scope ^ "\000" ^ type_name)
    env.record_bindings_by_lookup
  |> Option.value ~default:[]
  |> List.filter_map (fun id -> Symbol_map.find_opt id env.symbols)

let opened_bindings scope env =
  String_map.find_opt scope env.opened_bindings_by_scope
  |> Option.value ~default:[]
  |> List.filter_map (fun id -> Symbol_map.find_opt id env.symbols)

let unresolved_declaration name env =
  String_map.mem name env.unresolved_declaration_names

let unresolved_declaration_binding name env =
  String_map.mem name env.unresolved_declaration_binding_names

let add_explicit_declaration name env =
  {
    env with
    explicit_declaration_names =
      String_map.add name () env.explicit_declaration_names;
  }

let explicitly_declared name env =
  String_map.mem name env.explicit_declaration_names

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
  { env with namespace_aliases = String_map.add key target env.namespace_aliases }

let resolve_namespace_alias ~scope alias env =
  match String_map.find_opt (Names.scoped_key scope alias) env.namespace_aliases with
  | Some _ as target -> target
  | None -> String_map.find_opt alias env.namespace_aliases

let add_core_exclusions ~scope names env =
  let exclusions =
    List.fold_left
      (fun exclusions name ->
        String_map.add (Names.scoped_key scope name) () exclusions)
      env.core_exclusions names
  in
  { env with core_exclusions = exclusions }

let core_excluded ~scope name env =
  String_map.mem (Names.scoped_key scope name) env.core_exclusions

let add_macro ~scope ~name definition env =
  let key = Names.scoped_key scope name in
  { env with macros = String_map.add key definition env.macros }

let add_macro_alias ~alias definition env =
  { env with macros = String_map.add alias definition env.macros }

let remove_macro_alias ~alias definition env =
  match String_map.find_opt alias env.macros with
  | Some current when current = definition ->
      { env with macros = String_map.remove alias env.macros }
  | Some _ | None -> env

let find_macro ~scope name env =
  match String_map.find_opt (Names.scoped_key scope name) env.macros with
  | Some _ as definition -> definition
  | None -> String_map.find_opt name env.macros

let namespace_macros namespace env =
  let prefix = namespace ^ "/" in
  String_map.fold
    (fun key definition macros ->
      if not (String.starts_with ~prefix key) then macros
      else
        let name =
          String.sub key (String.length prefix)
            (String.length key - String.length prefix)
        in
        (name, definition) :: macros)
    env.macros []

let add_inline_macro ~scope ~name definition env =
  let key = Names.scoped_key scope name in
  {
    env with
    inline_macros = String_map.add key definition env.inline_macros;
  }

let add_inline_macro_alias ~alias definition env =
  {
    env with
    inline_macros = String_map.add alias definition env.inline_macros;
  }

let remove_inline_macro_alias ~alias definition env =
  match String_map.find_opt alias env.inline_macros with
  | Some current when current = definition ->
      { env with inline_macros = String_map.remove alias env.inline_macros }
  | Some _ | None -> env

let find_inline_macro ~scope name env =
  match String_map.find_opt (Names.scoped_key scope name) env.inline_macros with
  | Some _ as definition -> definition
  | None -> String_map.find_opt name env.inline_macros

let namespace_inline_macros namespace env =
  let prefix = namespace ^ "/" in
  String_map.fold
    (fun key definition macros ->
      if not (String.starts_with ~prefix key) then macros
      else
        let name =
          String.sub key (String.length prefix)
            (String.length key - String.length prefix)
        in
        (name, definition) :: macros)
    env.inline_macros []

let inline_macros env = env.inline_macros
let with_inline_macros inline_macros env = { env with inline_macros }
let clear_inline_macros env = { env with inline_macros = String_map.empty }

let without_source_callable ~scope name env =
  if Names.is_qualified name then env
  else
    let keys = List.sort_uniq String.compare [ name; Names.scoped_key scope name ] in
    {
      env with
      macros =
        List.fold_left (fun macros key -> String_map.remove key macros) env.macros
          keys;
      inline_macros =
        List.fold_left
          (fun inline_macros key -> String_map.remove key inline_macros)
          env.inline_macros keys;
    }

let source_callable_shadowed ~scope name env =
  core_excluded ~scope name env

let add_macro_function ~scope ~name definition env =
  let key = Names.scoped_key scope name in
  {
    env with
    macro_functions = String_map.add key definition env.macro_functions;
  }

let find_macro_function ~scope name env =
  match String_map.find_opt (Names.scoped_key scope name) env.macro_functions with
  | Some _ as definition -> definition
  | None -> String_map.find_opt name env.macro_functions

let add_macro_value ~scope ~name value env =
  let key = Names.scoped_key scope name in
  { env with macro_values = String_map.add key value env.macro_values }

let find_macro_value ~scope name env =
  match String_map.find_opt (Names.scoped_key scope name) env.macro_values with
  | Some _ as value -> value
  | None -> String_map.find_opt name env.macro_values

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
