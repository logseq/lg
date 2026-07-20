open Ast
open Types

module Env = Compiler_environment

type method_signature = {
  method_id : Method_id.t;
  method_name : string;
  param_tys : ty list;
  return_ty : ty;
}

let method_id protocol_id method_name =
  Method_id.create
    ~owner:(Protocol_id.owner protocol_id @ [ Protocol_id.name protocol_id ])
    ~name:method_name

let protocol_id scope protocol_name =
  if String.contains protocol_name '/' then Protocol_id.of_string protocol_name
  else
    Protocol_id.create
      ~owner:(if scope = "" then [] else [ scope ])
      ~name:protocol_name

let method_basename name =
  match String.rindex_opt name '/' with
  | None -> name
  | Some index -> String.sub name (index + 1) (String.length name - index - 1)

let marker_binding protocol_id signature =
  Types.binding ~protocol_id (Protocol_id.to_string protocol_id)
    (TFn (signature.param_tys, signature.return_ty))

let resolve_protocol_id ~scope env protocol_id =
  let registry = Env.protocols env in
  if Option.is_some (Protocol_registry.find_protocol protocol_id registry) then
    protocol_id
  else
    match Protocol_id.owner protocol_id with
    | [ module_path ] ->
        (match Env.resolve_namespace_alias ~scope module_path env with
        | Some target ->
            Protocol_id.create ~owner:[ target ]
              ~name:(Protocol_id.name protocol_id)
        | None -> (
            match
              Module_registry.resolve_alias ~scope module_path (Env.modules env)
            with
            | None -> protocol_id
            | Some target ->
                Protocol_id.create ~owner:[ Module_id.to_string target ]
                  ~name:(Protocol_id.name protocol_id)))
    | _ -> protocol_id

let find_protocol_id scope env protocol_name =
  let protocol_name =
    match method_basename protocol_name with
    | "ICounted" -> "Counted"
    | _ -> protocol_name
  in
  let registry = Env.protocols env in
  let scoped_id =
    protocol_id scope protocol_name |> resolve_protocol_id ~scope env
  in
  let root_id = Protocol_id.create ~owner:[] ~name:protocol_name in
  if Option.is_some (Protocol_registry.find_protocol scoped_id registry) then
    Some scoped_id
  else if Option.is_some (Protocol_registry.find_protocol root_id registry) then
    Some root_id
  else None

let method_is_ambiguous scope env method_name =
  if String.contains method_name '/' then false
  else
    let owner = if scope = "" then [] else [ scope ] in
    let protocols =
      Protocol_registry.protocols_for_method ~owner ~method_name
        (Env.protocols env)
    in
    let protocols =
      if protocols = [] && owner <> [] then
        Protocol_registry.protocols_for_method ~owner:[] ~method_name
          (Env.protocols env)
      else protocols
    in
    (match protocols with
    | _ :: _ :: _ -> true
    | [] | [ _ ] -> false)

let receiver_id = function
  | TInt -> Some "int"
  | TFloat -> Some "float"
  | TChar -> Some "char"
  | TString -> Some "string"
  | TSymbol -> Some "symbol"
  | TKeyword -> Some "keyword"
  | TBool -> Some "bool"
  | TUnit -> Some "unit"
  | TList _ -> Some "list"
  | TVector _ -> Some "vector"
  | TSet _ -> Some "set"
  | TSeq _ -> Some "seq"
  | TArray _ -> Some "array"
  | TRef _ -> Some "ref"
  | TTuple _ -> Some "tuple"
  | TOcaml name | TOcaml_app (name, _) -> Some name
  | TNamed_record record -> Some record.type_name
  | _ -> None

let registry_receiver_id = Receiver_id.of_type

let type_satisfies env protocol_id receiver_ty =
  let satisfies registry =
    match
      ( Protocol_registry.find_protocol protocol_id registry,
        registry_receiver_id receiver_ty )
    with
    | Some declaration, Some receiver_id ->
        Protocol_registry.Method_map.for_all
          (fun method_id _ ->
            Option.is_some
              (Protocol_registry.find_implementation protocol_id method_id
                 receiver_id registry))
          declaration.methods
    | None, _ | _, None -> false
  in
  satisfies (Env.protocols env)
  ||
  match Env.protocol_evidence env with
  | Some evidence -> satisfies evidence
  | None -> false

let satisfied_protocols env receiver_ty =
  Protocol_registry.declarations (Env.protocols env)
  |> List.filter_map (fun (protocol_id, _) ->
         let compiler_protocol =
           List.mem (Protocol_id.name protocol_id)
             [ "Seqable"; "Reducible"; "Counted"; "Indexed"; "Emptyable" ]
         in
         if (not compiler_protocol) && type_satisfies env protocol_id receiver_ty
         then Some protocol_id
         else None)

let constraint_type scope env protocol_name =
  match find_protocol_id scope env protocol_name with
  | None -> None
  | Some protocol_id ->
      Protocol_registry.find_protocol protocol_id (Env.protocols env)
      |> Option.map (fun (declaration : Protocol_registry.declaration) ->
             let method_types =
               declaration.methods
               |> Protocol_registry.Method_map.bindings
               |> List.map (fun (_, (signature : Protocol_registry.method_signature)) ->
                      TFn (signature.param_tys, signature.return_ty))
             in
             Types.protocol_constraint protocol_id method_types TUnknown)

let instantiate_receiver_binding receiver_ty (implementation : binding) =
  {
    implementation with
    ty =
      Types.instantiate_receiver_method_type receiver_ty implementation.ty;
  }

let apply_method_signature
    (signature : Protocol_registry.method_signature)
    (implementation : binding) =
  match implementation.ty with
  | TFn (parameters, (TUnknown | TVar _))
    when not (Types.equal signature.return_ty TUnknown) ->
      { implementation with ty = TFn (parameters, signature.return_ty) }
  | _ -> implementation

let witness_implementations env protocol_id receiver_ty =
  match
    ( Protocol_registry.find_protocol protocol_id (Env.protocols env),
      registry_receiver_id receiver_ty )
  with
  | Some (declaration : Protocol_registry.declaration), Some receiver_id ->
      let implementations =
        declaration.methods
        |> Protocol_registry.Method_map.bindings
        |> List.map (fun (method_id, signature) ->
               Protocol_registry.find_implementation protocol_id method_id
                 receiver_id (Env.protocols env)
               |> Option.map (fun implementation ->
                      implementation
                      |> instantiate_receiver_binding receiver_ty
                      |> apply_method_signature signature))
      in
      if List.for_all Option.is_some implementations then
        Some
          (List.map
             (fun implementation ->
               instantiate_receiver_binding receiver_ty
                 (Option.get implementation))
             implementations)
      else None
  | None, _ | _, None -> None

let witness_methods env protocol_id receiver_ty =
  match
    ( Protocol_registry.find_protocol protocol_id (Env.protocols env),
      registry_receiver_id receiver_ty )
  with
  | Some (declaration : Protocol_registry.declaration), Some receiver_id ->
      let rec collect methods = function
        | [] -> Some (List.rev methods)
        | (method_id, signature) :: rest -> (
            match
              Protocol_registry.find_implementation protocol_id method_id
                receiver_id (Env.protocols env)
            with
            | None -> None
            | Some implementation ->
                collect
                  ( ( Method_id.name method_id,
                      implementation
                      |> instantiate_receiver_binding receiver_ty
                      |> apply_method_signature signature )
                  :: methods )
                  rest)
      in
      collect []
        (Protocol_registry.Method_map.bindings declaration.methods)
  | None, _ | _, None -> None

let witness_implemented_methods env protocol_id receiver_ty =
  match
    ( Protocol_registry.find_protocol protocol_id (Env.protocols env),
      registry_receiver_id receiver_ty )
  with
  | Some (declaration : Protocol_registry.declaration), Some receiver_id ->
      let methods =
        declaration.methods
        |> Protocol_registry.Method_map.bindings
        |> List.filter_map (fun (method_id, signature) ->
               Protocol_registry.find_implementation protocol_id method_id
                 receiver_id (Env.protocols env)
               |> Option.map (fun implementation ->
                      ( Method_id.name method_id,
                        implementation
                        |> instantiate_receiver_binding receiver_ty
                        |> apply_method_signature signature )))
      in
      if methods = [] then None else Some methods
  | None, _ | _, None -> None

let implemented_protocols env receiver_ty =
  Protocol_registry.declarations (Env.protocols env)
  |> List.filter_map (fun (protocol_id, _) ->
         let compiler_protocol =
           List.mem (Protocol_id.name protocol_id)
             [ "Seqable"; "Reducible"; "Counted"; "Indexed"; "Emptyable" ]
         in
         if
           (not compiler_protocol)
           && Option.is_some
                (witness_implemented_methods env protocol_id receiver_ty)
         then Some protocol_id
         else None)

let lookup_marker scope env method_name =
  let registry = Env.protocols env in
  let marker_for protocol_id method_name =
    let method_id = method_id protocol_id method_name in
    Protocol_registry.find_method protocol_id method_id registry
    |> Option.map (fun (signature : Protocol_registry.method_signature) ->
           marker_binding protocol_id
             { method_id;
               method_name;
               param_tys = signature.param_tys;
               return_ty = signature.return_ty;
             })
  in
  match List.rev (String.split_on_char '/' method_name) with
    | method_name :: protocol_name :: reversed_owner ->
        let protocol_name =
          String.concat "/" (List.rev (protocol_name :: reversed_owner))
        in
        let namespace_owner =
          match Env.resolve_namespace_alias ~scope protocol_name env with
          | Some target -> Some target
          | None ->
              let protocols =
                Protocol_registry.protocols_for_method
                  ~owner:[ protocol_name ] ~method_name registry
              in
              if protocols = [] then None else Some protocol_name
        in
        (match namespace_owner with
        | Some owner -> (
            match
              Protocol_registry.protocols_for_method ~owner:[ owner ]
                ~method_name registry
            with
            | [ protocol_id ] -> marker_for protocol_id method_name
            | [] | _ :: _ :: _ -> None)
        | None ->
            let scoped_id =
              protocol_id scope protocol_name |> resolve_protocol_id ~scope env
            in
            (match marker_for scoped_id method_name with
            | Some _ as marker -> marker
            | None ->
                marker_for
                  (Protocol_id.create ~owner:[] ~name:protocol_name)
                  method_name))
    | [ method_name ] ->
        let owner = if scope = "" then [] else [ scope ] in
        let protocols =
          Protocol_registry.protocols_for_method ~owner ~method_name registry
        in
        let protocols =
          if protocols = [] && owner <> [] then
            Protocol_registry.protocols_for_method ~owner:[] ~method_name
              registry
          else protocols
        in
        (match protocols with
        | [ protocol_id ] ->
            marker_for protocol_id method_name
        | [] | _ :: _ :: _ -> None)
    | [] -> None

let lookup_protocol_marker scope env protocol_name method_name =
  let registry = Env.protocols env in
  match find_protocol_id scope env protocol_name with
  | None -> None
  | Some id ->
      let method_id = method_id id method_name in
      (match Protocol_registry.find_method id method_id registry with
      | Some (signature : Protocol_registry.method_signature) ->
          Some
            (marker_binding id
               {
                 method_id;
                 method_name;
                 param_tys = signature.param_tys;
                 return_ty = signature.return_ty;
               })
      | None -> None)

let lookup_impl env protocol_id method_name receiver_ty =
  match registry_receiver_id receiver_ty with
  | Some receiver_id ->
      let method_id = method_id protocol_id method_name in
      let registry = Env.protocols env in
      Protocol_registry.find_implementation protocol_id method_id receiver_id
        registry
      |> Option.map (fun implementation ->
             let implementation =
               instantiate_receiver_binding receiver_ty implementation
             in
             match Protocol_registry.find_method protocol_id method_id registry with
             | None -> implementation
             | Some signature ->
                 apply_method_signature signature implementation)
  | None -> None

let lookup_marker_impl env (marker : binding) method_name receiver_ty =
  match marker.protocol_id with
  | None -> None
  | Some protocol_id -> lookup_impl env protocol_id method_name receiver_ty

let rec merge_method_return_types env left right =
  match (left, right) with
    | Types.TUnknown, _ | _, Types.TUnknown | Types.TVar _, _
    | _, Types.TVar _ ->
        Some (Types.dynamic_constraint Types.TUnknown)
    | left, right when Types.equal left right -> Some left
    | left, right when Types.is_dynamic left || Types.is_dynamic right ->
        Some (Types.dynamic_constraint Types.TUnknown)
    | Types.TSeq left, Types.TSeq right ->
        Option.map
          (fun element -> Types.TSeq element)
          (merge_method_return_types env left right)
    | Types.TList left, Types.TList right ->
        Option.map
          (fun element -> Types.TList element)
          (merge_method_return_types env left right)
    | Types.TVector left, Types.TVector right ->
        Option.map
          (fun element -> Types.TVector element)
          (merge_method_return_types env left right)
    | Types.TArray left, Types.TArray right ->
        Option.map
          (fun element -> Types.TArray element)
          (merge_method_return_types env left right)
    | _ -> (
        match
          ( Collection_capability.element_type_of_ty env left,
            Collection_capability.element_type_of_ty env right )
        with
        | Some left, Some right ->
            Option.map
              (fun element -> Types.TSeq element)
              (merge_method_return_types env left right)
        | None, _ | _, None -> None)

let common_method_return env protocol_id method_name =
  let method_id = method_id protocol_id method_name in
  let registry =
    Compiler_environment.protocol_evidence env
    |> Option.value ~default:(Env.protocols env)
  in
  let return_types =
    Protocol_registry.implementations_for_method protocol_id method_id registry
    |> List.filter_map (fun (implementation : binding) ->
           match implementation.ty with
           | TFn (_, return_ty) when not (Types.equal return_ty TUnknown) ->
               Some return_ty
           | _ -> None)
  in
  match return_types with
  | [] -> None
  | first :: rest ->
      List.fold_left
        (fun merged return_ty ->
          Option.bind merged (fun merged ->
              merge_method_return_types env merged return_ty))
        (Some first) rest

let common_method_returns env protocol_id =
  match
    Protocol_registry.find_protocol protocol_id (Env.protocols env)
  with
  | None -> []
  | Some declaration ->
      declaration.methods
      |> Protocol_registry.Method_map.bindings
      |> List.map (fun (method_id, _) ->
             common_method_return env protocol_id (Method_id.name method_id))

let refine_constraint_method_returns env ty =
  let rec method_types = function
    | TUnit -> Some []
    | TTuple [ method_ty; rest ] ->
        Option.map (fun rest -> method_ty :: rest) (method_types rest)
    | _ -> None
  in
  match Types.protocol_constraint_info ty with
  | None -> ty
  | Some (protocol_id, witness_ty, value_ty) -> (
      match method_types witness_ty with
      | None -> ty
      | Some methods ->
          let returns = common_method_returns env protocol_id in
          if List.length methods <> List.length returns then ty
          else
            let methods =
              List.map2
                (fun method_ty return_ty ->
                  match (method_ty, return_ty) with
                  | TFn (params, (TUnknown | TVar _)), Some _ ->
                      TFn (params, Types.dynamic_constraint TUnknown)
                  | _ -> method_ty)
                methods returns
            in
            Types.protocol_constraint protocol_id methods value_ty)

let refine_deferred_type env = function
  | TFn (parameters, return_ty) ->
      TFn
        ( List.map (refine_constraint_method_returns env) parameters,
          return_ty )
  | ty -> ty

let common_method_return_param_index env protocol_id method_name =
  let method_id = method_id protocol_id method_name in
  let registry =
    Compiler_environment.protocol_evidence env
    |> Option.value ~default:(Env.protocols env)
  in
  let indices =
    Protocol_registry.implementations_for_method protocol_id method_id
      registry
    |> List.filter_map (fun (implementation : binding) ->
           implementation.return_param_index)
  in
  match indices with
  | first :: rest when List.for_all (( = ) first) rest -> Some first
  | [] | _ -> None

let method_position env (marker : binding) method_name =
  match marker.protocol_id with
  | None -> None
  | Some protocol_id -> (
      match Protocol_registry.find_protocol protocol_id (Env.protocols env) with
      | None -> None
      | Some declaration ->
          declaration.methods
          |> Protocol_registry.Method_map.bindings
          |> List.mapi (fun index (method_id, _) -> (index, Method_id.name method_id))
          |> List.find_map (fun (index, name) ->
                 if name = method_name then Some index else None))

let method_count env (marker : binding) =
  match marker.protocol_id with
  | None -> 0
  | Some protocol_id -> (
      match Protocol_registry.find_protocol protocol_id (Env.protocols env) with
      | None -> 0
      | Some declaration -> Protocol_registry.Method_map.cardinal declaration.methods)

let marker_has_protocol_id (marker : binding) protocol_id =
  Option.fold ~none:false
    ~some:(fun marker_id -> Protocol_id.equal marker_id protocol_id)
    marker.protocol_id

let parse_method_signature = function
  | FList [ FSymbol method_name; params ]
  | FList [ FSymbol method_name; params; FKeyword _ ] as method_form -> (
      let return_ty =
        match method_form with
        | FList [ _; _; FKeyword return_keyword ] ->
            Type_annotation.of_keyword return_keyword
        | _ -> Ok TUnknown
      in
      match (Type_annotation.parse_params params, return_ty) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok params, Ok return_ty ->
          if params = [] then Error.error "protocol methods must have a receiver parameter"
          else
            Ok
              {
                method_id = Method_id.create ~owner:[] ~name:method_name;
                method_name;
                param_tys = List.map snd params;
                return_ty;
              })
  | _ ->
      Error.error
        "defprotocol methods must be (method-name [params]) or (method-name [params] :return-type)"

let defprotocol scope protocol_name method_forms =
  let id = protocol_id scope protocol_name in
  let rec loop seen signatures = function
    | [] -> Ok (id, List.rev signatures)
    | method_form :: rest -> (
        match parse_method_signature method_form with
        | Error _ as err -> err
        | Ok signature ->
            if List.mem signature.method_name seen then
              Error.error
                ("protocol " ^ protocol_name ^ " declares duplicate method "
               ^ signature.method_name)
            else
            let signature =
              { signature with method_id = method_id id signature.method_name }
            in
            let registry_signature : Protocol_registry.method_signature =
              {
                method_id = signature.method_id;
                param_tys = signature.param_tys;
                return_ty = signature.return_ty;
              }
            in
            loop (signature.method_name :: seen)
              (registry_signature :: signatures)
              rest)
  in
  loop [] [] method_forms

let annotate_receiver receiver_ty = function
  | FVector (FSymbol annotation :: FSymbol _name :: _rest as params)
    when String.starts_with ~prefix:"^:" annotation -> (
      match Type_annotation.of_param_annotation annotation with
      | Error _ as err -> err
      | Ok ty ->
          if Types.equal ty receiver_ty then Ok (FVector params)
          else
            Error.error
              ("protocol implementation receiver must be " ^ source_name receiver_ty))
  | FVector (FSymbol name :: rest) -> (
      match registry_receiver_id receiver_ty with
      | Some _ -> Ok (FVector (FSymbol name :: rest))
      | None ->
          Error.error
            ("protocol implementations do not support receiver type "
           ^ source_name receiver_ty))
  | FVector _ -> Error.error "protocol methods must have a receiver parameter"
  | _ -> Error.error "protocol method parameters must be a vector"

let impl_ocaml_name scope protocol_name method_name receiver_ty =
  Names.ocaml_binding_name scope
    ("protocol_" ^ protocol_name ^ "_" ^ method_name ^ "_"
   ^ Option.value (receiver_id receiver_ty) ~default:(source_name receiver_ty))
