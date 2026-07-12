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
  semantic_expr : Semantic_ir.t;
}

let is_type_annotation name = String.starts_with ~prefix:"^:" name

let keyword_for_local name = ":" ^ name

let ignore_name name = name = "_"

let local_binding source_name ty semantic_expr =
  { source_name;
    ocaml_name = Names.sanitize_name source_name;
    ty;
    semantic_expr = Semantic_ir.annotate ty semantic_expr }

type map_binding = {
  local_name : string;
  keyword : string;
  default_form : form option;
}

type map_pattern = {
  field_bindings : map_binding list;
  as_name : string option;
}

type sequence_pattern = {
  item_names : string list;
  rest_name : string option;
  sequence_as_name : string option;
}

let parse_sequence_pattern forms =
  let rec loop items rest_name as_name = function
    | [] -> Ok { item_names = List.rev items; rest_name; sequence_as_name = as_name }
    | FKeyword ":as" :: FSymbol name :: [] ->
        Ok
          { item_names = List.rev items;
            rest_name;
            sequence_as_name = if ignore_name name then as_name else Some name }
    | FKeyword ":as" :: _ -> Error.error "sequential destructuring :as must be last"
    | FSymbol "&" :: FSymbol name :: rest ->
        if Option.is_some rest_name then
          Error.error "sequential destructuring & can appear only once"
        else loop items (if ignore_name name then rest_name else Some name) as_name rest
    | FSymbol "&" :: _ ->
        Error.error "sequential destructuring & must be followed by a symbol"
    | FSymbol name :: rest when Option.is_none rest_name ->
        loop (if ignore_name name then items else name :: items) rest_name as_name rest
    | _ :: _ when Option.is_some rest_name ->
        Error.error "sequential destructuring only supports :as after & rest"
    | _ :: _ -> Error.error "unsupported sequential destructuring form"
  in
  loop [] None None forms

let rec pattern_names = function
  | FSymbol name -> if ignore_name name then [] else [ name ]
  | FVector forms -> sequence_pattern_names forms
  | FMap pairs -> map_pattern_names pairs
  | _ -> []

and sequence_pattern_names forms =
  match parse_sequence_pattern forms with
  | Error _ -> []
  | Ok { item_names; rest_name; sequence_as_name } ->
      item_names
      @ (rest_name |> Option.to_list)
      @ (sequence_as_name |> Option.to_list)

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

let parse_map_pattern pairs =
  let default_for defaults name = List.assoc_opt name defaults in
  let parse_keys = function
    | FVector keys ->
        keys
        |> List.fold_left
             (fun acc -> function
               | FSymbol name when not (ignore_name name) ->
                   Result.map
                     (fun bindings ->
                       { local_name = name;
                         keyword = keyword_for_local name;
                         default_form = None }
                       :: bindings)
                     acc
               | FSymbol _ -> acc
               | _ -> Error.error "map destructuring :keys expects symbols")
             (Ok [])
    | _ -> Error.error "map destructuring :keys expects a vector"
  in
  let parse_defaults = function
    | FMap pairs ->
        pairs
        |> List.fold_left
             (fun acc -> function
               | FSymbol name, value when not (ignore_name name) ->
                   Result.map (fun defaults -> (name, value) :: defaults) acc
               | FSymbol _, _ -> acc
               | _ -> Error.error "map destructuring :or defaults must use symbols")
             (Ok [])
    | _ -> Error.error "map destructuring :or expects a map"
  in
  let apply_defaults defaults fields =
    fields
    |> List.map (fun field ->
           { field with default_form = default_for defaults field.local_name })
  in
  let rec loop fields as_name defaults = function
    | [] -> Ok { field_bindings = apply_defaults defaults (List.rev fields); as_name }
    | (FKeyword ":keys", value) :: rest -> (
        match parse_keys value with
        | Error _ as err -> err
        | Ok key_fields -> loop (List.rev_append key_fields fields) as_name defaults rest)
    | (FKeyword ":as", FSymbol name) :: rest ->
        loop fields (if ignore_name name then as_name else Some name) defaults rest
    | (FKeyword ":or", defaults_form) :: rest -> (
        match parse_defaults defaults_form with
        | Error _ as err -> err
        | Ok parsed_defaults -> loop fields as_name (parsed_defaults @ defaults) rest)
    | (FSymbol local_name, FKeyword keyword) :: rest ->
        if ignore_name local_name then loop fields as_name defaults rest
        else loop ({ local_name; keyword; default_form = None } :: fields) as_name defaults rest
    | _ :: _ -> Error.error "unsupported map destructuring form"
  in
  loop [] None [] pairs

let field_type fields keyword =
  match find_field keyword fields with
  | Some field -> Ok field
  | None -> Error.error ("cannot destructure missing field " ^ keyword)

let literal_default = function
  | FInt value -> Ok (typed_ir TInt (Semantic_ir.Int value))
  | FString value -> Ok (typed_ir TString (Semantic_ir.String value))
  | FBool value -> Ok (typed_ir TBool (Semantic_ir.Bool value))
  | FKeyword keyword -> Ok (typed_ir TKeyword (Semantic_ir.String keyword))
  | _ -> Error.error "map destructuring :or defaults must be scalar literals"

let infer_map_type pattern lookup_local_ty =
  parse_map_pattern pattern
  |> Result.map (fun parsed ->
         let fields =
           parsed.field_bindings
           |> List.map (fun { local_name; keyword; _ } ->
                  make_field keyword (lookup_local_ty local_name))
         in
         TRecord fields)

let infer_sequence_type forms lookup_local_ty =
  match parse_sequence_pattern forms with
  | Error _ as err -> err
  | Ok pattern ->
      let element_ty =
        pattern.item_names
        |> List.fold_left
             (fun acc name ->
               let ty = lookup_local_ty name in
               match acc with
               | None -> Some ty
               | Some existing when Types.equal existing ty -> Some existing
               | Some _ -> Some TUnknown)
             None
        |> Option.value ~default:TUnknown
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
  | TRecord fields | TNamed_record { fields; _ } -> (
      match parse_map_pattern pairs with
      | Error _ as err -> err
      | Ok parsed ->
          let bind_field { local_name; keyword; default_form } =
            match field_type fields keyword with
            | Error _ -> (
                match default_form with
                | None -> Error.error ("cannot destructure missing field " ^ keyword)
                | Some form -> (
                    match literal_default form with
                    | Error _ as err -> err
                    | Ok value -> Ok (local_binding local_name value.ty value.semantic_expr)))
            | Ok field ->
                Ok
                  (local_binding local_name field.ty
                     (Structural_map.field_expr target field))
          in
          let rec bind_fields acc = function
            | [] ->
                let acc =
                  match parsed.as_name with
                  | None -> acc
                  | Some name -> local_binding name target.ty target.semantic_expr :: acc
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
  let bind_at inner index name =
    let semantic_expr =
      match target.ty with
      | TList _ -> Semantic_ir.Apply (Semantic_ir.Ident "List.nth", [ target.semantic_expr; Semantic_ir.Int index ])
      | TVector _ -> Semantic_ir.Apply (Semantic_ir.Ident "Rrbvec.nth", [ target.semantic_expr; Semantic_ir.Int index ])
      | _ -> target.semantic_expr
    in
    local_binding name inner semantic_expr
  in
  let bind_rest count name =
    let semantic_expr =
      match target.ty with
      | TList _ ->
          Core_sequence_transform.drop_list_expr (Semantic_ir.Int count) target.semantic_expr
      | TVector _ ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Rrbvec.of_list",
              [ Core_sequence_transform.drop_list_expr (Semantic_ir.Int count)
                  (Semantic_ir.Apply (Semantic_ir.Ident "Rrbvec.to_list", [ target.semantic_expr ])) ] )
      | _ -> target.semantic_expr
    in
    local_binding name target.ty semantic_expr
  in
  match target.ty with
  | TList inner | TVector inner -> (
      match parse_sequence_pattern forms with
      | Error _ as err -> err
      | Ok pattern ->
          let item_count = List.length pattern.item_names in
          let bindings = pattern.item_names |> List.mapi (bind_at inner) in
          let bindings =
            match pattern.rest_name with
            | None -> bindings
            | Some name -> bindings @ [ bind_rest item_count name ]
          in
          let bindings =
            match pattern.sequence_as_name with
            | None -> bindings
            | Some name -> bindings @ [ local_binding name target.ty target.semantic_expr ]
          in
          Ok bindings)
  | _ -> Error.error "sequential destructuring expects a list or vector"

let bind_pattern (target : typed_expr) pattern =
  match pattern with
  | FSymbol name ->
      if ignore_name name then Ok []
      else Ok [ local_binding name target.ty target.semantic_expr ]
  | FMap pairs -> bind_map target pairs
  | FVector forms -> bind_sequence target forms
  | _ -> Error.error "unsupported destructuring pattern"
