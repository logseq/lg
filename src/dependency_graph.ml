open Ast

module String_map = Map.Make (String)
module String_set = Set.Make (String)
module Int_set = Set.Make (Int)

type node = {
  name : string;
  dependencies : string list;
}

let strongly_connected_components nodes =
  let graph =
    List.fold_left
      (fun graph node -> String_map.add node.name node.dependencies graph)
      String_map.empty nodes
  in
  let next_index = ref 0 in
  let indices = Hashtbl.create (List.length nodes) in
  let lowlinks = Hashtbl.create (List.length nodes) in
  let on_stack = Hashtbl.create (List.length nodes) in
  let stack = Stack.create () in
  let components = ref [] in
  let rec visit name =
    let index = !next_index in
    incr next_index;
    Hashtbl.add indices name index;
    Hashtbl.add lowlinks name index;
    Stack.push name stack;
    Hashtbl.replace on_stack name true;
    let dependencies =
      String_map.find_opt name graph |> Option.value ~default:[]
    in
    List.iter
      (fun dependency ->
        if String_map.mem dependency graph then
          match Hashtbl.find_opt indices dependency with
          | None ->
              visit dependency;
              Hashtbl.replace lowlinks name
                (min (Hashtbl.find lowlinks name)
                   (Hashtbl.find lowlinks dependency))
          | Some dependency_index ->
              if Hashtbl.find_opt on_stack dependency = Some true then
                Hashtbl.replace lowlinks name
                  (min (Hashtbl.find lowlinks name) dependency_index))
      dependencies;
    if Hashtbl.find lowlinks name = Hashtbl.find indices name then
      let rec pop component =
        let member = Stack.pop stack in
        Hashtbl.replace on_stack member false;
        let component = member :: component in
        if member = name then component else pop component
      in
      components := pop [] :: !components
  in
  List.iter
    (fun node ->
      if not (Hashtbl.mem indices node.name) then visit node.name)
    nodes;
  List.rev !components

let rec symbols form =
  match form with
  | FSymbol name -> [ name ]
  | FCoreSymbol core -> [ core_symbol_qualified_name core ]
  | FList forms | FVector forms -> List.concat_map symbols forms
  | FMap pairs ->
      List.concat_map
        (fun (key, value) -> symbols key @ symbols value)
        pairs
  | FKeyword _ | FString _ | FRegex _ | FInt _ | FFloat _ | FChar _ | FBool _ ->
      []

let dependency_symbols = function
  | FList (FSymbol "declare" :: names) ->
      List.filter_map
        (function
          | FSymbol name
            when not (String.starts_with ~prefix:"->" name)
                 && not (String.starts_with ~prefix:"map->" name) ->
              Some name
          | _ -> None)
        names
  | FList (FSymbol "defprotocol" :: _name :: method_forms) ->
      let rec type_hints = function
        | FSymbol name when String.starts_with ~prefix:"^" name -> [ name ]
        | FList forms | FVector forms -> List.concat_map type_hints forms
        | FMap pairs ->
            List.concat_map
              (fun (key, value) -> type_hints key @ type_hints value)
              pairs
        | FSymbol _ | FCoreSymbol _ | FKeyword _ | FString _ | FRegex _
        | FInt _ | FFloat _ | FChar _ | FBool _ ->
            []
      in
      List.concat_map type_hints method_forms
  | FList
      (FSymbol ("deftype" | "defrecord") :: _name :: fields
      :: implementations) ->
      let implementation_symbols = function
        | FList (_method_name :: body) -> List.concat_map symbols body
        | form -> symbols form
      in
      symbols fields @ List.concat_map implementation_symbols implementations
  | form -> symbols form

let direct_method_names forms =
  List.filter_map
    (function FList (FSymbol name :: _) -> Some name | _ -> None)
    forms

let method_names form =
  let rec collect names = function
    | FList (FSymbol name :: rest)
      when String.starts_with ~prefix:"-" name
           && not (String.starts_with ~prefix:"->" name) ->
        List.fold_left collect (name :: names) rest
    | FList forms | FVector forms -> List.fold_left collect names forms
    | FMap pairs ->
        List.fold_left
          (fun names (key, value) -> collect (collect names key) value)
          names pairs
    | FSymbol _ | FCoreSymbol _ | FKeyword _ | FString _ | FRegex _ | FInt _
    | FFloat _ | FChar _ | FBool _ ->
        names
  in
  collect [] form

let rec provided_names = function
  | FList [ FSymbol "defn-signature"; definition ] -> provided_names definition
  | FList (FSymbol "declare" :: names) ->
      List.filter_map (function FSymbol name -> Some name | _ -> None) names
  | FList (FSymbol "recursive-definition-group" :: definitions) ->
      List.concat_map provided_names definitions
  | FList (FSymbol "defprotocol" :: FSymbol name :: method_forms) ->
      name :: direct_method_names method_forms
  | FList
      (FSymbol ("deftype" | "defrecord") :: FSymbol name :: _ as forms) ->
      name :: ("->" ^ name) :: ("map->" ^ name) :: method_names (FList forms)
  | FList
      (FSymbol ("def" | "defonce" | "defn" | "defn-") :: FSymbol name :: _)
    ->
      [ name ]
  | FList
      (FSymbol ("extend-type" | "deftype-methods") :: _ as forms) ->
      method_names (FList forms)
  | FList (FSymbol "extend-protocol" :: _ as forms) ->
      method_names (FList forms)
  | FList (FSymbol definition :: FSymbol name :: _ as forms)
    when String.starts_with ~prefix:"def" definition ->
      name :: method_names (FList forms)
  | _ -> []

let has_declarations forms =
  List.exists
    (function
      | FList (FSymbol "declare" :: _)
      | FList [ FSymbol "defn-signature"; _ ] ->
          true
      | _ -> false)
    forms

let indexed_forms forms = List.mapi (fun index form -> (index, form)) forms

let provider_indices indexed =
  let protocol_methods =
    List.fold_left
      (fun methods (_, form) ->
        match form with
        | FList (FSymbol "defprotocol" :: _name :: method_forms) ->
            direct_method_names method_forms
            |> List.fold_left
                 (fun methods name -> String_set.add name methods)
                 methods
        | _ -> methods)
      String_set.empty indexed
  in
  let implementation_method_names = function
    | FList
        (FSymbol ("deftype" | "defrecord") :: _ :: _
        :: implementations)
    | FList
        (FSymbol ("extend-type" | "deftype-methods") :: _
        :: implementations)
    | FList (FSymbol "extend-protocol" :: _ :: implementations) ->
        direct_method_names implementations
        |> List.filter (fun name -> String_set.mem name protocol_methods)
    | _ -> []
  in
  List.fold_left
    (fun providers (index, form) ->
      List.fold_left
        (fun providers name ->
          let existing =
            String_map.find_opt name providers |> Option.value ~default:[]
          in
          String_map.add name (index :: existing) providers)
        providers
        (provided_names form @ implementation_method_names form))
    String_map.empty indexed

let declaration_provider_indices indexed =
  List.fold_left
    (fun providers (index, form) ->
      match form with
      | FList (FSymbol "declare" :: names) ->
          List.fold_left
            (fun providers -> function
              | FSymbol name ->
                  let existing =
                    String_map.find_opt name providers
                    |> Option.value ~default:[]
                  in
                  String_map.add name (index :: existing) providers
              | _ -> providers)
            providers names
      | _ -> providers)
    String_map.empty indexed

let form_dependencies ?(ignore_declarations = false) providers
    declaration_providers index form =
  let prefer_declarations =
    match form with
    | FList (FSymbol ("deftype" | "defrecord") :: _) -> true
    | _ -> false
  in
  match form with
  | FList (FSymbol "declare" :: _) when ignore_declarations -> []
  | FList
      (FSymbol ("defmacro" | "macro-helper-defn" | "macro-helper-def") :: _)
    ->
      []
  | form ->
      dependency_symbols form
      |> List.concat_map (fun name ->
             let candidates =
               let candidates = [ name ] in
               let candidates =
                 if String.ends_with ~suffix:"." name then
                   String.sub name 0 (String.length name - 1) :: candidates
                 else candidates
               in
               if String.starts_with ~prefix:"^" name then
                 String.sub name 1 (String.length name - 1) :: candidates
               else candidates
             in
              List.concat_map
                (fun candidate ->
                  let all =
                    String_map.find_opt candidate providers
                    |> Option.value ~default:[]
                  in
                  if
                    prefer_declarations
                    && not (String.starts_with ~prefix:"->" candidate)
                    && not (String.starts_with ~prefix:"map->" candidate)
                    && not (String.ends_with ~suffix:"." candidate)
                  then
                    match String_map.find_opt candidate declaration_providers with
                    | Some declarations -> declarations
                    | None -> all
                  else all)
               candidates)
      |> List.filter (fun dependency -> dependency <> index)
      |> List.sort_uniq Int.compare

let dependency_components ?(ignore_declarations = false) forms =
  let indexed = indexed_forms forms in
  let providers = provider_indices indexed in
  let declaration_providers = declaration_provider_indices indexed in
  let components =
    indexed
    |> List.map (fun (index, form) ->
           {
             name = string_of_int index;
             dependencies =
               form_dependencies ~ignore_declarations providers
                 declaration_providers index form
               |> List.map string_of_int;
           })
    |> strongly_connected_components
  in
  (providers, components)

let recursive_group_supported forms indices =
  List.for_all
    (fun index ->
      match List.nth forms index with
      | FList (FSymbol ("defn" | "defn-") :: _) -> true
      | _ -> false)
    indices

let recursive_groups forms =
  if not (has_declarations forms) then []
  else
    let _, components =
      dependency_components ~ignore_declarations:true forms
    in
    components
    |> List.map (List.map int_of_string)
    |> List.filter (function
         | _ :: _ :: _ as indices -> recursive_group_supported forms indices
         | _ -> false)
    |> List.map (List.sort Int.compare)

let stable_order forms =
  if not (has_declarations forms) then List.mapi (fun index _ -> index) forms
  else
    let providers, components = dependency_components forms in
    let declaration_providers =
      declaration_provider_indices (indexed_forms forms)
    in
    let dependencies index form =
      form_dependencies providers declaration_providers index form
    in
    let component_of = Hashtbl.create (List.length forms) in
    List.iteri
      (fun component members ->
        List.iter
          (fun member -> Hashtbl.add component_of (int_of_string member) component)
          members)
      components;
    let component_dependencies =
      List.mapi
        (fun component members ->
          members
          |> List.concat_map (fun member ->
                 let index = int_of_string member in
                 let form = List.nth forms index in
                 dependencies index form)
          |> List.fold_left
               (fun dependencies dependency ->
                 let dependency_component = Hashtbl.find component_of dependency in
                 if dependency_component = component then dependencies
                 else Int_set.add dependency_component dependencies)
               Int_set.empty)
        components
    in
    let component_minimum component =
      List.nth components component
      |> List.map int_of_string |> List.fold_left min max_int
    in
    let rec release emitted remaining ordered =
      if Int_set.is_empty remaining then List.rev ordered
      else
        let ready =
          Int_set.elements remaining
          |> List.filter (fun component ->
                 Int_set.subset
                   (List.nth component_dependencies component)
                   emitted)
          |> List.sort (fun left right ->
                 Int.compare (component_minimum left) (component_minimum right))
        in
        match ready with
        | [] -> failwith "dependency graph condensation must be acyclic"
        | component :: _ ->
            let members =
              List.nth components component
              |> List.map int_of_string |> List.sort Int.compare
            in
            release (Int_set.add component emitted)
              (Int_set.remove component remaining)
              (List.rev_append members ordered)
    in
    let order =
      release Int_set.empty
        (List.init (List.length components) Fun.id |> Int_set.of_list)
        []
    in
    let is_namespace index =
      match List.nth forms index with
      | FList (FSymbol ("ns" | "namespace-scope") :: _) -> true
      | _ -> false
    in
    let is_declaration index =
      match List.nth forms index with
      | FList (FSymbol "declare" :: _) -> true
      | _ -> false
    in
    let namespaces, order = List.partition is_namespace order in
    let declarations, order = List.partition is_declaration order in
    namespaces @ declarations @ order
