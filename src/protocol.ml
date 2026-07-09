open Ast
open Types

type method_signature = {
  method_name : string;
  arity : int;
  return_ty : ty;
}

let marker_name method_name = method_name ^ "$protocol"

let impl_name method_name receiver_ty =
  let suffix =
    match receiver_ty with
    | TInt -> Some "int"
    | TString -> Some "string"
    | TKeyword -> Some "keyword"
    | TBool -> Some "bool"
    | TNil -> Some "nil"
    | _ -> None
  in
  Option.map (fun suffix -> method_name ^ "$" ^ suffix) suffix

let receiver_annotation receiver_ty =
  let keyword =
    match receiver_ty with
    | TInt -> Some ":int"
    | TString -> Some ":string"
    | TKeyword -> Some ":keyword"
    | TBool -> Some ":bool"
    | TNil -> Some ":nil"
    | _ -> None
  in
  Option.map (fun keyword -> "^" ^ keyword) keyword

let lookup_marker current_ns env method_name =
  List.assoc_opt (Names.namespaced_key current_ns (marker_name method_name)) env

let lookup_impl current_ns env method_name receiver_ty =
  match impl_name method_name receiver_ty with
  | None -> None
  | Some impl_name -> List.assoc_opt (Names.namespaced_key current_ns impl_name) env

let parse_method_signature = function
  | FList [ FSymbol method_name; params; FKeyword return_keyword ] -> (
      match (Type_annotation.parse_params params, Type_annotation.of_keyword return_keyword) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok params, Ok return_ty ->
          if params = [] then Error.error "protocol methods must have a receiver parameter"
          else Ok { method_name; arity = List.length params; return_ty })
  | _ ->
      Error.error
        "defprotocol methods must be (method-name [params] :return-type)"

let marker_binding protocol_name signature =
  let params = List.init signature.arity (fun _ -> TAny) in
  Types.binding protocol_name (TFn (params, signature.return_ty))

let defprotocol_bindings current_ns protocol_name method_forms =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | method_form :: rest -> (
        match parse_method_signature method_form with
        | Error _ as err -> err
        | Ok signature ->
            let key =
              Names.namespaced_key current_ns (marker_name signature.method_name)
            in
            let binding = marker_binding protocol_name signature in
            loop ((key, binding) :: acc) rest)
  in
  loop [] method_forms

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
      match receiver_annotation receiver_ty with
      | None ->
          Error.error
            ("protocol implementations do not support receiver type "
           ^ source_name receiver_ty)
      | Some annotation -> Ok (FVector (FSymbol annotation :: FSymbol name :: rest)))
  | FVector _ -> Error.error "protocol methods must have a receiver parameter"
  | _ -> Error.error "protocol method parameters must be a vector"

let impl_ocaml_name current_ns protocol_name method_name receiver_ty =
  Names.ocaml_binding_name current_ns
    ("protocol_" ^ protocol_name ^ "_" ^ method_name ^ "_" ^ source_name receiver_ty)
