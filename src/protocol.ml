open Ast
open Types

type method_signature = {
  method_name : string;
  param_tys : ty list;
  return_ty : ty;
}

let protocol_id scope protocol_name =
  if String.contains protocol_name '/' then protocol_name
  else Names.scoped_key scope protocol_name

let marker_name protocol_name method_name =
  protocol_name ^ "/" ^ method_name ^ "$protocol"

let legacy_marker_name method_name = method_name ^ "$protocol"
let ambiguous_protocol_id = "__ambiguous_protocol__"

let method_basename name =
  match String.rindex_opt name '/' with
  | None -> name
  | Some index -> String.sub name (index + 1) (String.length name - index - 1)

let is_legacy_marker key (binding : binding) =
  let suffix = "$protocol" in
  let basename = method_basename key in
  if not (String.ends_with ~suffix basename) then false
  else
    let method_name =
      String.sub basename 0 (String.length basename - String.length suffix)
    in
    key <> marker_name binding.ocaml_name method_name

let ambiguous_marker_binding () = Types.binding ambiguous_protocol_id TAny

let method_is_ambiguous scope env method_name =
  if String.contains method_name '/' then false
  else
    match
      List.assoc_opt
        (Names.scoped_key scope (legacy_marker_name method_name))
        env
    with
    | Some binding -> binding.ocaml_name = ambiguous_protocol_id
    | None -> false

let receiver_id = function
  | TInt -> Some "int"
  | TString -> Some "string"
  | TKeyword -> Some "keyword"
  | TBool -> Some "bool"
  | TNamed_record record -> Some record.type_name
  | _ -> None

let impl_name protocol_id method_name receiver_ty =
  receiver_id receiver_ty
  |> Option.map (fun receiver ->
         "__protocol_impl/" ^ protocol_id ^ "/" ^ method_name ^ "/" ^ receiver)

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
  let names =
    if String.contains method_name '/' then
      [ scope ^ "/" ^ method_name ^ "$protocol"; method_name ^ "$protocol" ]
    else [ Names.scoped_key scope (legacy_marker_name method_name) ]
  in
  List.find_map (fun name -> List.assoc_opt name env) names

let lookup_protocol_marker scope env protocol_name method_name =
  let id = protocol_id scope protocol_name in
  List.assoc_opt (marker_name id method_name) env

let lookup_impl env protocol_id method_name receiver_ty =
  match impl_name protocol_id method_name receiver_ty with
  | None -> None
  | Some impl_name -> List.assoc_opt impl_name env

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
                method_name;
                param_tys = List.map snd params;
                return_ty;
              })
  | _ ->
      Error.error
        "defprotocol methods must be (method-name [params] :return-type)"

let marker_binding protocol_name signature =
  Types.binding protocol_name (TFn (signature.param_tys, signature.return_ty))

let defprotocol_bindings scope protocol_name method_forms =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | method_form :: rest -> (
        match parse_method_signature method_form with
        | Error _ as err -> err
        | Ok signature ->
            let id = protocol_id scope protocol_name in
            let binding = marker_binding id signature in
            let canonical = marker_name id signature.method_name in
            let legacy =
              Names.scoped_key scope
                (legacy_marker_name signature.method_name)
            in
            loop ((canonical, binding) :: (legacy, binding) :: acc) rest)
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
