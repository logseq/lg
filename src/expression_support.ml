open Types
open Lowered

module Env = Compiler_environment

let rec truthiness_expression ty expression =
  match ty with
  | TBool -> expression
  | TOcaml_app ("option", [ payload_ty ]) ->
      Semantic_ir.Match
        ( expression,
          [ (Semantic_ir.PConstructor ("None", None), Semantic_ir.Bool false);
            ( Semantic_ir.PConstructor
                ("Some", Some (Semantic_ir.PVar "truthy_value")),
              truthiness_expression payload_ty
                (Semantic_ir.Ident "truthy_value") );
          ] )
  | TOcaml "option" ->
      Semantic_ir.Match
        ( expression,
          [ (Semantic_ir.PConstructor ("None", None), Semantic_ir.Bool false);
            ( Semantic_ir.PConstructor ("Some", Some Semantic_ir.PAny),
              Semantic_ir.Bool true );
          ] )
  | TSeq _ ->
      Semantic_ir.Apply
        ( Semantic_ir.Ident "not",
          [ Semantic_ir.Apply
              ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.is_empty",
                [ expression ] ) ] )
  | _ -> Semantic_ir.Sequence [ expression; Semantic_ir.Bool true ]

let condition_expression expr =
  Ok (truthiness_expression expr.ty expr.semantic_expr)

let apply name args = Semantic_ir.Apply (Semantic_ir.Ident name, args)

let rec drop n xs =
  if n <= 0 then xs
  else match xs with [] -> [] | _ :: rest -> drop (n - 1) rest

let option_for_all predicate = function None -> true | Some value -> predicate value

let is_ocaml_owned_type = function
  | TFloat | TChar | TArray _ | TRef _ | TOcaml _ | TOcaml_app _ | TTuple _ -> true
  | _ -> false

let is_ocaml_constructor_pattern_target target_ty name =
  is_ocaml_owned_type target_ty
  || (match target_ty with
     | TUnknown | TVar _ -> String.contains name '.' || String.contains name '/'
     | _ -> false)

let branch_types_compatible left right =
  Types.equal left right
  || left = TUnknown || right = TUnknown
  || (match (left, right) with
     | TList TUnknown, TList _ | TList _, TList TUnknown -> true
     | _ -> false)
  || Types.defer_to_ocaml ~expected:left ~actual:right

let merge_branch_types left right =
  if Types.equal left right then Some left
  else
    match (left, right) with
    | TList TUnknown, TList inner | TList inner, TList TUnknown ->
        Some (TList inner)
    | TUnknown, ty | ty, TUnknown -> Some ty
    | _ when Types.defer_to_ocaml ~expected:left ~actual:right -> Some left
    | _ -> None

let merge_branch_expressions left right =
  let continue expression =
    Semantic_ir.Apply
      (Semantic_ir.Ident "Lg_runtime.Runtime_reduced.continue", [ expression ])
  in
  match (Types.reduced_element left.ty, Types.reduced_element right.ty) with
  | Some left_inner, Some right_inner when Types.equal left_inner right_inner ->
      Some (left.ty, left.semantic_expr, right.semantic_expr)
  | Some inner, None when Types.equal inner right.ty ->
      Some (left.ty, left.semantic_expr, continue right.semantic_expr)
  | None, Some inner when Types.equal left.ty inner ->
      Some (right.ty, continue left.semantic_expr, right.semantic_expr)
  | _ ->
      merge_branch_types left.ty right.ty
      |> Option.map (fun ty -> (ty, left.semantic_expr, right.semantic_expr))

let unresolved_contextual_type = function
  | TList TUnknown -> true
  | _ -> false

let lg_metadata_type_for_ocaml_payload = function
  | TOcaml "int" -> TInt
  | TOcaml "float" -> TFloat
  | TOcaml "char" -> TChar
  | TOcaml "string" -> TString
  | TOcaml "bool" -> TBool
  | TOcaml "unit" -> TUnit
  | ty -> ty

let rec lg_metadata_type_for_ocaml_type = function
  | TOcaml "int" -> TInt
  | TOcaml "float" -> TFloat
  | TOcaml "char" -> TChar
  | TOcaml "string" -> TString
  | TOcaml "bool" -> TBool
  | TOcaml "unit" -> TUnit
  | TTuple args -> TTuple (List.map lg_metadata_type_for_ocaml_type args)
  | ty -> ty

let ocaml_builtin_constructor_payloads target_ty constructor_name =
  match (target_ty, constructor_name) with
  | TOcaml "option", "Some" -> Some [ TUnknown ]
  | TOcaml "option", "None" -> Some []
  | TOcaml_app ("option", [ payload_ty ]), "Some" ->
      Some [ lg_metadata_type_for_ocaml_payload payload_ty ]
  | TOcaml_app ("option", [ _ ]), "None" -> Some []
  | TOcaml "result", "Ok" -> Some [ TUnknown ]
  | TOcaml "result", "Error" -> Some [ TUnknown ]
  | TOcaml_app ("result", [ ok_ty; _ ]), "Ok" ->
      Some [ lg_metadata_type_for_ocaml_payload ok_ty ]
  | TOcaml_app ("result", [ _; error_ty ]), "Error" ->
      Some [ lg_metadata_type_for_ocaml_payload error_ty ]
  | _ -> None

let record_type_key = Resolver.record_type_key

let record_type_application type_name parameters =
  match parameters with
  | [] -> type_name
  | [ _ ] -> "_ " ^ type_name
  | parameters ->
      "(" ^ String.concat ", " (List.map (fun _ -> "_") parameters) ^ ") "
      ^ type_name

let lookup_record_type = Resolver.lookup_record_type

let starts_with_uppercase name =
  String.length name > 0
  &&
  let first = name.[0] in
  first >= 'A' && first <= 'Z'

let is_constructor_name name =
  let segments =
    name |> String.split_on_char '/' |> List.concat_map (String.split_on_char '.')
  in
  match List.rev segments with
  | segment :: _ -> starts_with_uppercase segment
  | [] -> false

let lookup_binding = Resolver.lookup_binding

let binding_of_expr ?(row_param_types = []) ocaml_name expr =
  Types.binding ~row_param_types ?return_param_index:expr.return_param_index
    ocaml_name expr.ty

let check_emitted_name_collision = Resolver.check_emitted_name_collision

let lookup_function scope env name =
  match lookup_binding scope env name with
  | Ok binding -> Ok (typed_ir binding.ty (Semantic_ir.Ident binding.ocaml_name))
  | Error _ -> (
      match name with
      | "+" ->
          Ok
            (typed_ir (TFn ([ TInt; TInt ], TInt))
               (Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "a"; Semantic_ir.PVar "b" ],
                    Semantic_ir.Infix ("+", Semantic_ir.Ident "a", Semantic_ir.Ident "b") )))
      | "-" ->
          Ok
            (typed_ir (TFn ([ TInt; TInt ], TInt))
               (Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "a"; Semantic_ir.PVar "b" ],
                    Semantic_ir.Infix ("-", Semantic_ir.Ident "a", Semantic_ir.Ident "b") )))
      | "*" ->
          Ok
            (typed_ir (TFn ([ TInt; TInt ], TInt))
               (Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "a"; Semantic_ir.PVar "b" ],
                    Semantic_ir.Infix ("*", Semantic_ir.Ident "a", Semantic_ir.Ident "b") )))
      | "/" ->
          Ok
            (typed_ir (TFn ([ TInt; TInt ], TInt))
               (Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "a"; Semantic_ir.PVar "b" ],
                    Semantic_ir.Infix ("/", Semantic_ir.Ident "a", Semantic_ir.Ident "b") )))
      | "inc" ->
          Ok
            (typed_ir (TFn ([ TInt ], TInt))
               (Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "x" ],
                    Semantic_ir.Infix ("+", Semantic_ir.Ident "x", Semantic_ir.Int 1) )))
      | "dec" ->
          Ok
            (typed_ir (TFn ([ TInt ], TInt))
               (Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "x" ],
                    Semantic_ir.Infix ("-", Semantic_ir.Ident "x", Semantic_ir.Int 1) )))
      | "not" ->
          Ok
            (typed_ir (TFn ([ TBool ], TBool))
               (Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "x" ],
                    Semantic_ir.Prefix ("not", Semantic_ir.Ident "x") )))
      | _ -> Error.error ("unknown function " ^ name))

let ocaml_call_target = Resolver.ocaml_call_target
let resolve_ocaml_call_target = Resolver.resolve_ocaml_call_target
let resolve_ocaml_constructor_target = Resolver.resolve_ocaml_constructor_target

let inherit_scope_ocaml_value_refers scope module_path env =
  let prefix = scope ^ "/" in
  let prefix_len = String.length prefix in
  let inherited =
    Env.filter_map (fun key (binding : binding) ->
           match binding.host_reference with
           | Some (Ocaml_value _) when
               String.length key > prefix_len
               && String.sub key 0 prefix_len = prefix ->
               let name =
                 String.sub key prefix_len (String.length key - prefix_len)
               in
               Some (Names.scoped_key module_path name, binding)
           | _ -> None) env
  in
  Env.add_bindings inherited env

type compiled_fn_parts = {
  param_bindings : (string * binding) list;
  param_identities : (Source_node_id.t * Location.t) option list;
  destructured_bindings : Destructure.local_binding list;
  body : typed_expr;
}

let row_param_type_names prefix param_tys =
  param_tys
  |> List.mapi (fun index -> function
       | TRecord _ -> Some (prefix ^ "_row" ^ string_of_int index)
       | _ -> None)

let row_type_items row_type_names param_tys =
  List.map2
    (fun row_type_name param_ty ->
      match (row_type_name, param_ty) with
      | Some type_name, TRecord fields ->
          Some
            (Type_def
               { type_name; type_parameters = []; fields; location = None })
      | _ -> None)
    row_type_names param_tys
  |> List.filter_map Fun.id

let row_project_expr type_name fields arg =
  let source = "__row_source" in
  Semantic_ir.Let
    ( [ (Semantic_ir.PVar source, arg.semantic_expr) ],
      Semantic_ir.Record
        ( List.map
            (fun (field : field) ->
              (field.ocaml_name, Semantic_ir.Field (Semantic_ir.Ident source, field.ocaml_name)))
            fields,
          Some type_name ) )

let row_arg_expr row_type_name expected_ty arg =
  match (row_type_name, expected_ty, arg.ty) with
  | Some type_name, TRecord fields, (TRecord _ | TNamed_record _) ->
      row_project_expr type_name fields arg
  | _ -> arg.semantic_expr

let coerce_set_element element_ty value =
  match element_ty with
  | TNamed_record expected -> (
      match value.ty with
      | TNamed_record actual when actual.type_name = expected.type_name ->
          Ok value.semantic_expr
      | (TRecord actual_fields | TNamed_record { fields = actual_fields; _ })
        when Types.assignable ~policy:Structural ~expected:element_ty
               ~actual:value.ty ->
          let rec project_fields acc = function
            | [] -> Ok (List.rev acc)
            | (field : field) :: rest -> (
                match find_field field.keyword actual_fields with
                | None -> Error.error "set record coercion is missing a field"
                | Some actual_field ->
                    project_fields
                      ((field.ocaml_name, Structural_map.field_expr value actual_field) :: acc)
                      rest)
          in
          project_fields [] expected.fields
          |> Result.map (fun fields -> Semantic_ir.Record (fields, Some expected.type_name))
      | _ -> Error.error "set value type must match record element type")
  | _ ->
      if Types.equal element_ty value.ty then Ok value.semantic_expr
      else Error.error "set value type must match element type"

let constrain_record_function_argument_expr fn element_ty =
  let rec constrain_pattern type_name = function
    | Semantic_ir.PVar name ->
        Some (Semantic_ir.PConstraint (Semantic_ir.PVar name, type_name))
    | Semantic_ir.PLocated (node_id, location, pattern) ->
        constrain_pattern type_name pattern
        |> Option.map (fun pattern ->
               Semantic_ir.PLocated (node_id, location, pattern))
    | _ -> None
  in
  match (Semantic_ir.unlocated fn.semantic_expr, element_ty) with
  | Semantic_ir.Fun ([ pattern ], body), TNamed_record record -> (
      match constrain_pattern record.type_name pattern with
      | Some pattern -> Semantic_ir.Fun ([ pattern ], body)
      | None -> fn.semantic_expr)
  | _ -> fn.semantic_expr

let param_constraint_name = function
  | TOcaml_app (name, [ _; _ ]) when name = Types.seqable_constraint_name -> None
  | (TInt | TFloat | TChar | TString | TSymbol | TKeyword | TBool | TUnit
    | TArray _ | TRef _ | TOcaml _ | TOcaml_app _ | TTuple _ | TNamed_record _) as ty ->
      Some (Types.ocaml_name ty)
  | _ -> None
