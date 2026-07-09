open Ast
open Types

type param_spec = {
  pattern : form;
  source_name : string;
  ocaml_name : string;
  explicit_ty : ty option;
  destructured : bool;
}

type local_binding = {
  source_name : string;
  ocaml_name : string;
  ty : ty;
  code : string;
}

let is_type_annotation name = String.starts_with ~prefix:"^:" name

let keyword_for_local name = ":" ^ name

let ignore_name name = name = "_"

let local_binding source_name ty code =
  { source_name; ocaml_name = Names.sanitize_name source_name; ty; code }

let rec pattern_names = function
  | FSymbol name -> if ignore_name name then [] else [ name ]
  | FVector forms -> sequence_pattern_names forms
  | FMap pairs -> map_pattern_names pairs
  | _ -> []

and sequence_pattern_names forms =
  let rec loop acc = function
    | [] -> List.rev acc
    | FKeyword ":as" :: FSymbol name :: [] ->
        if ignore_name name then List.rev acc else List.rev (name :: acc)
    | FKeyword ":as" :: _ -> List.rev acc
    | FSymbol name :: rest when not (ignore_name name) -> loop (name :: acc) rest
    | _ :: rest -> loop acc rest
  in
  loop [] forms

and map_pattern_names pairs =
  let add_name acc name = if ignore_name name then acc else name :: acc in
  let add_keys acc = function
    | FVector keys ->
        List.fold_left
          (fun acc -> function FSymbol name -> add_name acc name | _ -> acc)
          acc keys
    | _ -> acc
  in
  pairs
  |> List.fold_left
       (fun acc -> function
         | FKeyword ":keys", value -> add_keys acc value
         | FKeyword ":as", FSymbol name -> add_name acc name
         | FSymbol name, FKeyword _ -> add_name acc name
         | _ -> acc)
       []
  |> List.rev

let parse_param_specs = function
  | FVector params ->
      let rec loop index acc = function
        | [] -> Ok (List.rev acc)
        | FSymbol annotation :: FSymbol name :: rest when is_type_annotation annotation -> (
            match Type_annotation.of_param_annotation annotation with
            | Error _ as err -> err
            | Ok ty ->
                loop (index + 1)
                  ({ pattern = FSymbol name;
                     source_name = name;
                     ocaml_name = Names.sanitize_name name;
                     explicit_ty = Some ty;
                     destructured = false }
                  :: acc)
                  rest)
        | FSymbol name :: rest ->
            loop (index + 1)
              ({ pattern = FSymbol name;
                 source_name = name;
                 ocaml_name = Names.sanitize_name name;
                 explicit_ty = None;
                 destructured = false }
              :: acc)
              rest
        | (FVector _ | FMap _) as pattern :: rest ->
            let source_name = "__destructure" ^ string_of_int index in
            loop (index + 1)
              ({ pattern;
                 source_name;
                 ocaml_name = Names.sanitize_name source_name;
                 explicit_ty = None;
                 destructured = true }
              :: acc)
              rest
        | _ -> Error.error "function parameters must be symbols or destructuring patterns"
      in
      loop 0 [] params
  | _ -> Error.error "function parameters must be a vector"

type map_binding = { local_name : string; keyword : string }

type map_pattern = {
  field_bindings : map_binding list;
  as_name : string option;
}

let parse_map_pattern pairs =
  let parse_keys = function
    | FVector keys ->
        keys
        |> List.fold_left
             (fun acc -> function
               | FSymbol name when not (ignore_name name) ->
                   Result.map
                     (fun bindings ->
                       { local_name = name; keyword = keyword_for_local name } :: bindings)
                     acc
               | FSymbol _ -> acc
               | _ -> Error.error "map destructuring :keys expects symbols")
             (Ok [])
    | _ -> Error.error "map destructuring :keys expects a vector"
  in
  let rec loop fields as_name = function
    | [] -> Ok { field_bindings = List.rev fields; as_name }
    | (FKeyword ":keys", value) :: rest -> (
        match parse_keys value with
        | Error _ as err -> err
        | Ok key_fields -> loop (List.rev_append key_fields fields) as_name rest)
    | (FKeyword ":as", FSymbol name) :: rest ->
        loop fields (if ignore_name name then as_name else Some name) rest
    | (FSymbol local_name, FKeyword keyword) :: rest ->
        if ignore_name local_name then loop fields as_name rest
        else loop ({ local_name; keyword } :: fields) as_name rest
    | _ :: _ -> Error.error "unsupported map destructuring form"
  in
  loop [] None pairs

let field_type fields keyword =
  match find_field keyword fields with
  | Some field -> Ok field
  | None -> Error.error ("cannot destructure missing field " ^ keyword)

let infer_map_type pattern lookup_local_ty =
  parse_map_pattern pattern
  |> Result.map (fun parsed ->
         let fields =
           parsed.field_bindings
           |> List.map (fun { local_name; keyword } ->
                  make_field keyword (lookup_local_ty local_name))
         in
         TRecord fields)

let infer_sequence_type forms lookup_local_ty =
  let names =
    forms
    |> List.filter_map (function
         | FSymbol name when not (ignore_name name) -> Some name
         | _ -> None)
  in
  let element_ty =
    names
    |> List.fold_left
         (fun acc name ->
           let ty = lookup_local_ty name in
           match acc with
           | None -> Some ty
           | Some existing when Types.equal existing ty -> Some existing
           | Some _ -> Some TAny)
         None
    |> Option.value ~default:TAny
  in
  Ok (TVector element_ty)

let infer_pattern_type pattern lookup_local_ty =
  match pattern with
  | FSymbol name -> Ok (lookup_local_ty name)
  | FMap pairs -> infer_map_type pairs lookup_local_ty
  | FVector forms -> infer_sequence_type forms lookup_local_ty
  | _ -> Error.error "unsupported destructuring pattern"

let bind_map (target : typed_expr) pairs =
  match target.ty with
  | TRecord fields -> (
      match parse_map_pattern pairs with
      | Error _ as err -> err
      | Ok parsed ->
          let bind_field { local_name; keyword } =
            match field_type fields keyword with
            | Error _ as err -> err
            | Ok field ->
                Ok
                  (local_binding local_name field.ty
                     (Structural_map.field_code target field))
          in
          let rec bind_fields acc = function
            | [] ->
                let acc =
                  match parsed.as_name with
                  | None -> acc
                  | Some name -> local_binding name target.ty target.code :: acc
                in
                Ok (List.rev acc)
            | binding :: rest -> (
                match bind_field binding with
                | Error _ as err -> err
                | Ok binding -> bind_fields (binding :: acc) rest)
          in
          bind_fields [] parsed.field_bindings)
  | _ -> Error.error "map destructuring expects a map"

let bind_sequence (target : typed_expr) forms =
  let rec split_items acc = function
    | [] -> Ok (List.rev acc, None)
    | FKeyword ":as" :: FSymbol name :: [] ->
        Ok (List.rev acc, if ignore_name name then None else Some name)
    | FKeyword ":as" :: _ -> Error.error "sequential destructuring :as must be last"
    | FSymbol name :: rest ->
        split_items (if ignore_name name then acc else name :: acc) rest
    | _ :: _ -> Error.error "unsupported sequential destructuring form"
  in
  let bind_at inner index name =
    let code =
      match target.ty with
      | TList _ -> "List.nth (" ^ target.code ^ ") " ^ string_of_int index
      | TVector _ -> "Rrbvec.nth (" ^ target.code ^ ") " ^ string_of_int index
      | _ -> target.code
    in
    local_binding name inner code
  in
  match target.ty with
  | TList inner | TVector inner -> (
      match split_items [] forms with
      | Error _ as err -> err
      | Ok (names, as_name) ->
          let bindings = names |> List.mapi (bind_at inner) in
          let bindings =
            match as_name with
            | None -> bindings
            | Some name -> bindings @ [ local_binding name target.ty target.code ]
          in
          Ok bindings)
  | _ -> Error.error "sequential destructuring expects a list or vector"

let bind_pattern (target : typed_expr) pattern =
  match pattern with
  | FSymbol name ->
      if ignore_name name then Ok []
      else Ok [ local_binding name target.ty target.code ]
  | FMap pairs -> bind_map target pairs
  | FVector forms -> bind_sequence target forms
  | _ -> Error.error "unsupported destructuring pattern"

let let_code binding = "let " ^ binding.ocaml_name ^ " = " ^ binding.code
