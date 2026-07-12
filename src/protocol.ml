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
        (match
           Module_registry.resolve_alias ~scope module_path (Env.modules env)
         with
        | None -> protocol_id
        | Some target ->
            Protocol_id.create ~owner:[ Module_id.to_string target ]
              ~name:(Protocol_id.name protocol_id))
    | _ -> protocol_id

let method_is_ambiguous scope env method_name =
  if String.contains method_name '/' then false
  else
    let owner = if scope = "" then [] else [ scope ] in
    (match
      Protocol_registry.protocols_for_method ~owner ~method_name
        (Env.protocols env)
    with
    | _ :: _ :: _ -> true
    | [] | [ _ ] -> false)

let receiver_id = function
  | TInt -> Some "int"
  | TString -> Some "string"
  | TKeyword -> Some "keyword"
  | TBool -> Some "bool"
  | TNamed_record record -> Some record.type_name
  | _ -> None

let registry_receiver_id = function
  | TInt -> Some Protocol_registry.Int_receiver
  | TString -> Some String_receiver
  | TKeyword -> Some Keyword_receiver
  | TBool -> Some Bool_receiver
  | TNamed_record record -> Some (Record_receiver record.type_id)
  | _ -> None

let receiver_annotation receiver_ty =
  let keyword =
    match receiver_ty with
    | TInt -> Some ":int"
    | TString -> Some ":string"
    | TKeyword -> Some ":keyword"
    | TBool -> Some ":bool"
    | _ -> None
  in
  Option.map (fun keyword -> "^" ^ keyword) keyword

let lookup_marker scope env method_name =
  let registry = Env.protocols env in
  match List.rev (String.split_on_char '/' method_name) with
    | method_name :: protocol_name :: reversed_owner ->
        let protocol_name =
          String.concat "/" (List.rev (protocol_name :: reversed_owner))
        in
        let protocol_id =
          protocol_id scope protocol_name |> resolve_protocol_id ~scope env
        in
        let method_id = method_id protocol_id method_name in
        Protocol_registry.find_method protocol_id method_id registry
        |> Option.map (fun (signature : Protocol_registry.method_signature) ->
               marker_binding protocol_id
                 {
                   method_id;
                   method_name;
                   param_tys = signature.param_tys;
                   return_ty = signature.return_ty;
                 })
    | [ method_name ] ->
        let owner = if scope = "" then [] else [ scope ] in
        (match
           Protocol_registry.protocols_for_method ~owner ~method_name registry
         with
        | [ protocol_id ] ->
            let method_id = method_id protocol_id method_name in
            Protocol_registry.find_method protocol_id method_id registry
            |> Option.map (fun (signature : Protocol_registry.method_signature) ->
                   marker_binding protocol_id
                     {
                       method_id;
                       method_name;
                       param_tys = signature.param_tys;
                       return_ty = signature.return_ty;
                     })
        | [] | _ :: _ :: _ -> None)
    | [] -> None

let lookup_protocol_marker scope env protocol_name method_name =
  let id = protocol_id scope protocol_name in
  let id = resolve_protocol_id ~scope env id in
  let method_id = method_id id method_name in
  match Protocol_registry.find_method id method_id (Env.protocols env) with
  | Some (signature : Protocol_registry.method_signature) ->
      Some
        (marker_binding id
           {
             method_id;
             method_name;
             param_tys = signature.param_tys;
             return_ty = signature.return_ty;
           })
  | None -> None

let lookup_impl env protocol_id method_name receiver_ty =
  match registry_receiver_id receiver_ty with
  | Some receiver_id ->
      let method_id = method_id protocol_id method_name in
      Protocol_registry.find_implementation protocol_id method_id receiver_id
        (Env.protocols env)
  | None -> None

let lookup_marker_impl env (marker : binding) method_name receiver_ty =
  match marker.protocol_id with
  | None -> None
  | Some protocol_id -> lookup_impl env protocol_id method_name receiver_ty

let marker_has_protocol_id (marker : binding) protocol_id =
  Option.fold ~none:false
    ~some:(fun marker_id -> Protocol_id.equal marker_id protocol_id)
    marker.protocol_id

let parse_method_signature = function
  | FList [ FSymbol method_name; params; FKeyword return_keyword ] -> (
      match (Type_annotation.parse_params params, Type_annotation.of_keyword return_keyword) with
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
        "defprotocol methods must be (method-name [params] :return-type)"

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
      match receiver_ty with
      | TNamed_record _ -> Ok (FVector (FSymbol name :: rest))
      | _ -> (match receiver_annotation receiver_ty with
      | None ->
          Error.error
            ("protocol implementations do not support receiver type "
           ^ source_name receiver_ty)
      | Some annotation -> Ok (FVector (FSymbol annotation :: FSymbol name :: rest))))
  | FVector _ -> Error.error "protocol methods must have a receiver parameter"
  | _ -> Error.error "protocol method parameters must be a vector"

let impl_ocaml_name scope protocol_name method_name receiver_ty =
  Names.ocaml_binding_name scope
    ("protocol_" ^ protocol_name ^ "_" ^ method_name ^ "_"
   ^ Option.value (receiver_id receiver_ty) ~default:(source_name receiver_ty))
