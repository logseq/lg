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
  | FKeyword _ | FString _ | FRegex _ | FInt _ | FFloat _ | FDecimal _
  | FChar _ | FBool _ ->
      []

let rec value_symbols form =
  match form with
  | FList (FSymbol "ffi" :: _) -> []
  | FList (FSymbol ("quote" | "clojure.core/quote") :: _) -> []
  | FList (FSymbol "record" :: FSymbol _ :: fields) ->
      fields
      |> List.concat_map (function
           | FList (_field_name :: values) -> List.concat_map value_symbols values
           | field -> value_symbols field)
  | FSymbol name -> [ name ]
  | FCoreSymbol core -> [ core_symbol_qualified_name core ]
  | FList forms | FVector forms -> List.concat_map value_symbols forms
  | FMap pairs ->
      List.concat_map
        (fun (key, value) -> value_symbols key @ value_symbols value)
        pairs
  | FKeyword _ | FString _ | FRegex _ | FInt _ | FFloat _ | FDecimal _
  | FChar _ | FBool _ ->
      []

let rec record_type_symbols form =
  match form with
  | FList (FSymbol "record" :: FSymbol name :: fields) ->
      name
      :: List.concat_map
           (function
             | FList (_field_name :: values) ->
                 List.concat_map record_type_symbols values
             | field -> record_type_symbols field)
           fields
  | FList forms | FVector forms -> List.concat_map record_type_symbols forms
  | FMap pairs ->
      List.concat_map
        (fun (key, value) ->
          record_type_symbols key @ record_type_symbols value)
        pairs
  | FSymbol _ | FCoreSymbol _ | FKeyword _ | FString _ | FRegex _ | FInt _
  | FFloat _ | FDecimal _ | FChar _ | FBool _ ->
      []

let type_annotation_builtins =
  String_set.of_list
    [
      "array";
      "bool";
      "bytes";
      "char";
      "comparable";
      "array-index";
      "dynamic";
      "float";
      "fn";
      "hashable";
      "int";
      "int64";
      "long";
      "number";
      "keyword";
      "list";
      "map";
      "option";
      "ordering";
      "overload";
      "ref";
      "result";
      "seq";
      "seqable";
      "set";
      "string";
      "symbol";
      "tuple";
      "unit";
      "variadic-fn";
      "vector";
      "weak";
    ]

let type_annotation_symbols = function
  | FKeyword annotation ->
      let is_type_character = function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' | '/'
        | '?' | '!' ->
            true
        | _ -> false
      in
      let add_token token tokens =
        if token = "" || String_set.mem token type_annotation_builtins then
          tokens
        else
          let local =
            match String.rindex_opt token '/' with
            | Some index ->
                String.sub token (index + 1)
                  (String.length token - index - 1)
            | None -> token
          in
          if local = token then token :: tokens else local :: token :: tokens
      in
      let rec scan start index tokens =
        if index < String.length annotation then
          if is_type_character annotation.[index] then
            scan start (index + 1) tokens
          else
            let token = String.sub annotation start (index - start) in
            scan (index + 1) (index + 1) (add_token token tokens)
        else
          let token = String.sub annotation start (index - start) in
          List.rev (add_token token tokens)
      in
      scan 0 0 []
  | form -> symbols form

let inline_type_hint_symbols forms =
  let metadata_flags =
    String_set.of_list
      [
        ":const";
        ":dynamic";
        ":inline";
        ":macro";
        ":mutable";
        ":no-doc";
        ":once";
        ":private";
        ":unsynchronized-mutable";
        ":volatile-mutable";
      ]
  in
  forms
  |> List.concat_map (function
       | FSymbol annotation when String.starts_with ~prefix:"^" annotation ->
           let annotation =
             String.sub annotation 1 (String.length annotation - 1)
           in
           if String_set.mem annotation metadata_flags then []
           else type_annotation_symbols (FKeyword annotation)
       | _ -> [])

let require_referred_symbols entries =
  let rec options = function
    | FKeyword (":refer" | ":refer-macros") :: FVector names :: rest ->
        List.filter_map
          (function FSymbol name -> Some name | _ -> None)
          names
        @ options rest
    | _ :: rest -> options rest
    | [] -> []
  in
  entries
  |> List.concat_map (function
       | FVector (_module_name :: spec_options) -> options spec_options
       | _ -> [])

let dependency_symbols = function
  | FList [ FSymbol "ffi"; _; FVector parameters; result; _ ] ->
      List.concat_map type_annotation_symbols (result :: parameters)
  | FList (FSymbol "require" :: entries) -> require_referred_symbols entries
  | FList
      (FSymbol ("optional-sequential-adapter" | "optional-map-adapter") :: _)
    ->
      []
  | FList (FSymbol "exception-data-adapter" :: _) -> []
  | FList (FSymbol "nil-value-adapter" :: _) -> []
  | FList (FSymbol "truthiness-adapter" :: _) -> []
  | FList (FSymbol "empty-map-default" :: _) -> []
  | FList (FSymbol "closed-sum-constructors" :: _) -> []
  | FList (FSymbol "contextual-closed-sum-constructors" :: _) -> []
  | FList
      (FSymbol "external-record" :: _name :: FVector parameters :: field_forms)
    ->
      let parameters =
        parameters
        |> List.filter_map (function FSymbol name -> Some name | _ -> None)
        |> String_set.of_list
      in
      field_forms
      |> List.concat_map (function
           | FList (_field_name :: annotations) ->
               List.concat_map type_annotation_symbols annotations
           | form -> type_annotation_symbols form)
      |> List.filter (fun name -> not (String_set.mem name parameters))
  | FList (FSymbol "external-record" :: _name :: field_forms) ->
      field_forms
      |> List.concat_map (function
           | FList (_field_name :: annotations) ->
               List.concat_map type_annotation_symbols annotations
           | form -> type_annotation_symbols form)
  | FList [ FSymbol "signature"; _name; _annotation ] ->
      (* Signatures are forward declarations. Named record references remain
         placeholders until the corresponding record definition is available. *)
      []
  | FList
      [
        FSymbol "signature";
        _name;
        FVector parameters;
        _annotation;
      ] ->
      ignore parameters;
      []
  | FList
      (FSymbol "type-record" :: _name :: FVector parameters :: field_forms) ->
      let parameters =
        parameters
        |> List.filter_map (function FSymbol name -> Some name | _ -> None)
        |> String_set.of_list
      in
      field_forms
      |> List.concat_map (function
           | FList (_field_name :: annotations) ->
               List.concat_map type_annotation_symbols annotations
           | form -> type_annotation_symbols form)
      |> List.filter (fun name -> not (String_set.mem name parameters))
  | FList (FSymbol "type-record" :: _name :: field_forms) ->
      field_forms
      |> List.concat_map (function
           | FList (_field_name :: annotations) ->
               List.concat_map type_annotation_symbols annotations
           | form -> type_annotation_symbols form)
  | FList
      (FSymbol "type-variant" :: _name :: FVector parameters
      :: constructor_forms) ->
      let parameters =
        parameters
        |> List.filter_map (function FSymbol name -> Some name | _ -> None)
        |> String_set.of_list
      in
      constructor_forms
      |> List.concat_map (function
           | FList (_constructor_name :: annotations) ->
               List.concat_map type_annotation_symbols annotations
           | _ -> [])
      |> List.filter (fun name -> not (String_set.mem name parameters))
  | FList (FSymbol "type-variant" :: _name :: constructor_forms) ->
      constructor_forms
      |> List.concat_map (function
           | FList (_constructor_name :: annotations) ->
               List.concat_map type_annotation_symbols annotations
           | _ -> [])
  | FList
      [
        FSymbol "type-alias";
        _name;
        FVector parameters;
        manifest;
      ] ->
      let parameters =
        parameters
        |> List.filter_map (function FSymbol name -> Some name | _ -> None)
        |> String_set.of_list
      in
      type_annotation_symbols manifest
      |> List.filter (fun name -> not (String_set.mem name parameters))
  | FList [ FSymbol "type-alias"; _name; manifest ] ->
      type_annotation_symbols manifest
  | FList (FSymbol "declare+" :: FSymbol name :: _) -> [ "declare+"; name ]
  | FList
      (FSymbol "declare+"
      :: FList [ FSymbol "__type-hint"; _; FSymbol name ]
      :: _) ->
      [ "declare+"; name ]
  | FList (FSymbol "declare" :: names) ->
      List.filter_map
        (function
          | FSymbol name
            when not (String.starts_with ~prefix:"->" name)
                 && not (String.starts_with ~prefix:"map->" name) ->
              Some name
          | _ -> None)
        names
  | (FList
      (FSymbol ("def" | "defonce") :: forms) as form) ->
      value_symbols form @ inline_type_hint_symbols forms
  | FList (FSymbol "defprotocol" :: _name :: method_forms) ->
      let rec type_dependencies = function
        | FKeyword _ as annotation -> type_annotation_symbols annotation
        | FSymbol name when String.starts_with ~prefix:"^" name -> [ name ]
        | FList forms | FVector forms ->
            List.concat_map type_dependencies forms
        | FMap pairs ->
            List.concat_map
              (fun (key, value) ->
                type_dependencies key @ type_dependencies value)
              pairs
        | FSymbol _ | FCoreSymbol _ | FString _ | FRegex _ | FInt _
        | FFloat _ | FDecimal _ | FChar _ | FBool _ ->
            []
      in
      List.concat_map type_dependencies method_forms
  | FList
      (FSymbol ("deftype" | "defrecord") :: _name :: fields
      :: implementations) ->
      let implementation_symbols = function
        | FList (_method_name :: body) -> List.concat_map symbols body
        | form -> symbols form
      in
      symbols fields @ List.concat_map implementation_symbols implementations
  | FList
      (FSymbol "deftype-methods" :: type_name :: protocol
      :: implementations) ->
      let implementation_symbols = function
        | FList (_method_name :: body) -> List.concat_map symbols body
        | form -> symbols form
      in
      symbols type_name @ symbols protocol
      @ List.concat_map implementation_symbols implementations
  | form -> value_symbols form

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
    | FFloat _ | FDecimal _ | FChar _ | FBool _ ->
        names
  in
  collect [] form

let rec provided_names = function
  | FList [ FSymbol "defn-signature"; definition ] -> provided_names definition
  | FList (FSymbol "do" :: forms) -> List.concat_map provided_names forms
  | FList (FSymbol "signature" :: FSymbol name :: _) -> (
      match String.rindex_opt name '/' with
      | Some index ->
          [
            name;
            String.sub name (index + 1) (String.length name - index - 1);
          ]
      | None -> [ name ])
  | FList (FSymbol "declare+" :: FSymbol name :: _) -> [ name ]
  | FList
      (FSymbol "declare+"
      :: FList [ FSymbol "__type-hint"; _; FSymbol name ]
      :: _) ->
      [ name ]
  | FList (FSymbol "declare" :: names) ->
      List.filter_map (function FSymbol name -> Some name | _ -> None) names
  | FList (FSymbol "recursive-definition-group" :: definitions) ->
      List.concat_map provided_names definitions
  | FList (FSymbol "defprotocol" :: FSymbol name :: method_forms) ->
      name :: direct_method_names method_forms
  | FList
      (FSymbol ("deftype" | "defrecord") :: FSymbol name :: _ as forms) ->
      name :: ("->" ^ name) :: ("map->" ^ name) :: method_names (FList forms)
  | FList (FSymbol "type-variant" :: FSymbol name :: declarations) ->
      let constructors =
        declarations
        |> List.filter_map (function
             | FSymbol constructor -> Some constructor
             | FList (FSymbol constructor :: _) -> Some constructor
             | FVector _ | FList _ | FMap _ | FCoreSymbol _ | FKeyword _
             | FString _ | FRegex _ | FInt _ | FFloat _ | FDecimal _ | FChar _
             | FBool _ ->
                 None)
      in
      name :: constructors
  | FList
      (FSymbol ("extern-type" | "type-alias" | "type-record" | "external-record")
      :: FSymbol name :: _) ->
      [ name ]
  | FList
      (FSymbol ("def" | "defonce") :: FSymbol "^:dynamic" :: FSymbol name
      :: _) ->
      [ name ]
  | FList
      (FSymbol ("defn" | "defn-") :: FSymbol "^:dynamic" :: FSymbol name
      :: _) ->
      [ name ]
  | FList
      (FSymbol ("def" | "defonce" | "defn" | "defn-" | "ffi") :: FSymbol name :: _)
    ->
      [ name ]
  | FList
      (FSymbol definition
      :: FList [ FSymbol "__type-hint"; _; FSymbol name ] :: _)
    when String.starts_with ~prefix:"def" definition ->
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
      | FList (FSymbol "declare+" :: FSymbol _ :: _)
      | FList
          (FSymbol "declare+"
          :: FList [ FSymbol "__type-hint"; _; FSymbol _ ]
          :: _)
      | FList [ FSymbol "defn-signature"; _ ] ->
          true
      | _ -> false)
    forms

let indexed_forms forms = List.mapi (fun index form -> (index, form)) forms

let provider_indices ?(ignore_declarations = false) indexed =
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
      match form with
      | FList (FSymbol ("declare" | "declare+") :: _)
        when ignore_declarations ->
          providers
      | _ ->
          List.fold_left
            (fun providers name ->
              let existing =
                String_map.find_opt name providers |> Option.value ~default:[]
              in
              String_map.add name (index :: existing) providers)
            providers
            (provided_names form @ implementation_method_names form))
    String_map.empty indexed

let type_provider_indices indexed =
  List.fold_left
    (fun providers (index, form) ->
      let names =
        match form with
        | FList
            (FSymbol
              ( "extern-type" | "type-variant" | "type-alias" | "type-record"
              | "external-record" | "deftype" | "defrecord" | "defprotocol" )
            :: FSymbol name :: _) ->
            [ name ]
        | _ -> []
      in
      List.fold_left
        (fun providers name ->
          let existing =
            String_map.find_opt name providers |> Option.value ~default:[]
          in
          String_map.add name (index :: existing) providers)
        providers names)
    String_map.empty indexed

let declaration_provider_indices indexed =
  List.fold_left
    (fun providers (index, form) ->
      match form with
      | FList (FSymbol "declare+" :: FSymbol name :: _) ->
          let existing =
            String_map.find_opt name providers |> Option.value ~default:[]
          in
          String_map.add name (index :: existing) providers
      | FList
          (FSymbol "declare+"
          :: FList [ FSymbol "__type-hint"; _; FSymbol name ]
          :: _) ->
          let existing =
            String_map.find_opt name providers |> Option.value ~default:[]
          in
          String_map.add name (index :: existing) providers
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
      | FList (FSymbol "signature" :: FSymbol _ :: _) ->
          List.fold_left
            (fun providers name ->
              let existing =
                String_map.find_opt name providers |> Option.value ~default:[]
              in
              String_map.add name (index :: existing) providers)
            providers (provided_names form)
      | _ -> providers)
    String_map.empty indexed

let graph_scope forms =
  forms
  |> List.find_map (function
       | FList [ FSymbol "namespace-scope"; FSymbol scope ] -> Some scope
       | _ -> None)
  |> Option.value ~default:""

let signature_type_dependencies scope forms =
  let dependencies = function
    | FList [ FSymbol "signature"; FSymbol name; annotation ] ->
        Some (name, type_annotation_symbols annotation)
    | FList
        [ FSymbol "signature";
          FSymbol name;
          FVector parameters;
          annotation;
        ] ->
        let parameters =
          parameters
          |> List.filter_map (function FSymbol name -> Some name | _ -> None)
          |> String_set.of_list
        in
        Some
          ( name,
            type_annotation_symbols annotation
            |> List.filter (fun name ->
                   not (String_set.mem name parameters)) )
    | _ -> None
  in
  List.fold_left
    (fun signatures form ->
      match dependencies form with
      | None -> signatures
      | Some (name, dependencies) ->
          let name =
            if String.contains name '/' then name
            else Names.scoped_key scope name
          in
          String_map.add name dependencies signatures)
    String_map.empty forms

let form_dependencies ?(ignore_declarations = false) ~scope
    ~signature_dependencies providers type_providers declaration_providers index
    form =
  let prefer_declarations =
    match form with
    | FList (FSymbol ("deftype" | "defrecord") :: _) -> true
    | _ -> false
  in
  let type_definition_form = function
    | FList
        (FSymbol
          ( "extern-type" | "type-variant" | "type-alias" | "type-record"
          | "external-record" )
        :: _) ->
        true
    | _ -> false
  in
  match form with
  | FList (FSymbol "declare+" :: _) when ignore_declarations -> []
  | FList (FSymbol "declare" :: _) when ignore_declarations -> []
  | FList
      (FSymbol ("defmacro" | "macro-helper-defn" | "macro-helper-def") :: _)
    ->
      []
  | form ->
      let signed_dependencies =
        match form with
        | FList
            (FSymbol ("def" | "defonce" | "defn" | "defn-")
            :: FSymbol name :: _) ->
            String_map.find_opt (Names.scoped_key scope name)
              signature_dependencies
            |> Option.value ~default:[]
        | _ -> []
      in
      let resolve_dependencies ~prefer_declarations provider_map dependencies =
        dependencies
        |> List.concat_map (fun name ->
             let candidates =
               let candidates = [ name ] in
               let candidates =
                 if String.ends_with ~suffix:"." name then
                   String.sub name 0 (String.length name - 1) :: candidates
                 else candidates
               in
               if String.starts_with ~prefix:"^" name then
                 let hinted_name =
                   String.sub name 1 (String.length name - 1)
                 in
                 let local_name =
                   let separator =
                     match String.rindex_opt hinted_name '/' with
                     | Some index -> Some index
                     | None -> String.rindex_opt hinted_name '.'
                   in
                   Option.map
                     (fun index ->
                       String.sub hinted_name (index + 1)
                         (String.length hinted_name - index - 1))
                     separator
                 in
                 Option.to_list local_name @ (hinted_name :: candidates)
               else candidates
             in
              List.concat_map
                (fun candidate ->
                  let all =
                    String_map.find_opt candidate provider_map
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
      in
      let value_dependencies, type_dependencies =
        if type_definition_form form then
          ([], dependency_symbols form @ record_type_symbols form @ signed_dependencies)
        else
          (dependency_symbols form, record_type_symbols form @ signed_dependencies)
      in
      (resolve_dependencies ~prefer_declarations providers value_dependencies
      @ resolve_dependencies ~prefer_declarations:false type_providers
          type_dependencies)
      |> List.filter (fun dependency -> dependency <> index)
      |> List.sort_uniq Int.compare

let dependency_components ?(ignore_declarations = false)
    ?(external_signature_dependencies = []) forms =
  let indexed = indexed_forms forms in
  let providers = provider_indices ~ignore_declarations indexed in
  let type_providers = type_provider_indices indexed in
  let declaration_providers = declaration_provider_indices indexed in
  let scope = graph_scope forms in
  let signature_dependencies =
    List.fold_left
      (fun dependencies (name, type_dependencies) ->
        String_map.add name type_dependencies dependencies)
      (signature_type_dependencies scope forms)
      external_signature_dependencies
  in
  let components =
    indexed
    |> List.map (fun (index, form) ->
           {
             name = string_of_int index;
             dependencies =
           form_dependencies ~ignore_declarations ~scope ~signature_dependencies
             providers type_providers declaration_providers index form
               |> List.map string_of_int;
           })
    |> strongly_connected_components
  in
  (providers, type_providers, components)

let recursive_group_supported forms indices =
  List.for_all
    (fun index ->
      match List.nth forms index with
      | FList (FSymbol ("defn" | "defn-") :: _) -> true
      | _ -> false)
    indices

let recursive_groups forms =
  let _, _, components =
    dependency_components ~ignore_declarations:true forms
  in
  components
  |> List.map (List.map int_of_string)
  |> List.filter (function
       | _ :: _ :: _ as indices -> recursive_group_supported forms indices
       | _ -> false)
  |> List.map (List.sort Int.compare)

let stable_order ?(external_signature_dependencies = []) forms =
  let providers, type_providers, component_lists =
    dependency_components ~external_signature_dependencies forms
  in
  let declaration_providers =
    declaration_provider_indices (indexed_forms forms)
  in
  let scope = graph_scope forms in
  let signature_dependencies =
    List.fold_left
      (fun dependencies (name, type_dependencies) ->
        String_map.add name type_dependencies dependencies)
      (signature_type_dependencies scope forms)
      external_signature_dependencies
  in
  let forms = Array.of_list forms in
  let components =
    component_lists
    |> List.map (fun members ->
           members |> List.map int_of_string |> List.sort Int.compare)
    |> Array.of_list
  in
  let component_count = Array.length components in
  let component_of = Array.make (Array.length forms) (-1) in
  Array.iteri
    (fun component members ->
      List.iter (fun member -> component_of.(member) <- component) members)
    components;
  let component_dependencies =
    Array.mapi
      (fun component members ->
        List.fold_left
          (fun dependencies index ->
            form_dependencies ~scope ~signature_dependencies providers
              type_providers declaration_providers index forms.(index)
            |> List.fold_left
                 (fun dependencies dependency ->
                   let dependency_component = component_of.(dependency) in
                   if dependency_component = component then dependencies
                   else Int_set.add dependency_component dependencies)
                 dependencies)
          Int_set.empty members)
      components
  in
  let dependents = Array.make component_count [] in
  Array.iteri
    (fun component dependencies ->
      Int_set.iter
        (fun dependency ->
          dependents.(dependency) <- component :: dependents.(dependency))
        dependencies)
    component_dependencies;
  let remaining_dependencies =
    Array.map Int_set.cardinal component_dependencies
  in
  let component_minimum =
    Array.map
      (function first :: _ -> first | [] -> assert false)
      components
  in
  let module Ready = Set.Make (struct
    type t = int * int

    let compare (left_minimum, left) (right_minimum, right) =
      match Int.compare left_minimum right_minimum with
      | 0 -> Int.compare left right
      | comparison -> comparison
  end) in
  let ready =
    Array.fold_left
      (fun ready component ->
        if remaining_dependencies.(component) = 0 then
          Ready.add (component_minimum.(component), component) ready
        else ready)
      Ready.empty (Array.init component_count Fun.id)
  in
  let rec release emitted_count ready ordered =
    if Ready.is_empty ready then
      if emitted_count = component_count then List.rev ordered
      else failwith "dependency graph condensation must be acyclic"
    else
      let ((_, component) as entry) = Ready.min_elt ready in
      let ready = Ready.remove entry ready in
      let ready =
        List.fold_left
          (fun ready dependent ->
            remaining_dependencies.(dependent) <-
              remaining_dependencies.(dependent) - 1;
            if remaining_dependencies.(dependent) = 0 then
              Ready.add (component_minimum.(dependent), dependent) ready
            else ready)
          ready dependents.(component)
      in
      release (emitted_count + 1) ready
        (List.rev_append components.(component) ordered)
  in
  let order = release 0 ready [] in
  let is_namespace index =
    match forms.(index) with
    | FList (FSymbol ("ns" | "namespace-scope" | "refer-clojure-exclude") :: _) -> true
    | _ -> false
  in
  let is_declaration index =
    match forms.(index) with
    | FList (FSymbol "declare" :: _) -> true
    | _ -> false
  in
  let namespaces, order = List.partition is_namespace order in
  let declarations, order = List.partition is_declaration order in
  namespaces @ declarations @ order
