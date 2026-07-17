open Ast

module String_map = Map.Make (String)
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

let method_names form =
  let rec collect names = function
    | FList (FSymbol name :: rest) when String.starts_with ~prefix:"-" name ->
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
  | FList
      (FSymbol ("deftype" | "defrecord" | "defprotocol")
      :: FSymbol name :: _ as forms) ->
      name :: method_names (FList forms)
  | FList
      (FSymbol ("def" | "defonce" | "defn" | "defn-") :: FSymbol name :: _)
    ->
      [ name ]
  | FList
      (FSymbol ("extend-type" | "deftype-methods") :: _ as forms) ->
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

let stable_order forms =
  if not (has_declarations forms) then List.mapi (fun index _ -> index) forms
  else
    let indexed = List.mapi (fun index form -> (index, form)) forms in
    let providers =
      List.fold_left
        (fun providers (index, form) ->
          List.fold_left
            (fun providers name ->
              let existing =
                String_map.find_opt name providers |> Option.value ~default:[]
              in
              String_map.add name (index :: existing) providers)
            providers (provided_names form))
        String_map.empty indexed
    in
    let dependencies index form =
      symbols form
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
                 String_map.find_opt candidate providers
                 |> Option.value ~default:[])
               candidates)
      |> List.filter (fun dependency -> dependency <> index)
      |> List.sort_uniq Int.compare
    in
    let graph =
      List.map
        (fun (index, form) ->
          {
            name = string_of_int index;
            dependencies =
              List.map string_of_int (dependencies index form);
          })
        indexed
    in
    let components = strongly_connected_components graph in
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
    release Int_set.empty
      (List.init (List.length components) Fun.id |> Int_set.of_list)
      []
