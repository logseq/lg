open Types
open Lowered

module Env = Compiler_environment

let ensure_bool expr =
  if Types.compatible ~expected:TBool ~actual:expr.ty then Ok ()
  else Error.error "if condition must be bool"

let apply name args = Ocaml_ir.Apply (Ocaml_ir.Ident name, args)

let rec drop n xs =
  if n <= 0 then xs
  else match xs with [] -> [] | _ :: rest -> drop (n - 1) rest

let option_for_all predicate = function None -> true | Some value -> predicate value

let is_ocaml_owned_type = function
  | TFloat | TChar | TArray _ | TRef _ | TOcaml _ | TOcaml_app _ | TTuple _ -> true
  | _ -> false

let branch_types_compatible left right =
  Types.equal left right
  || left = TAny || right = TAny
  || Types.defer_to_ocaml ~expected:left ~actual:right

let cljml_metadata_type_for_ocaml_payload = function
  | TOcaml "int" -> TInt
  | TOcaml "string" -> TString
  | TOcaml "bool" -> TBool
  | TOcaml "unit" -> TUnit
  | ty -> ty

let rec cljml_metadata_type_for_ocaml_type = function
  | TOcaml "int" -> TInt
  | TOcaml "string" -> TString
  | TOcaml "bool" -> TBool
  | TOcaml "unit" -> TUnit
  | TTuple args -> TTuple (List.map cljml_metadata_type_for_ocaml_type args)
  | ty -> ty

let ocaml_builtin_constructor_payloads target_ty constructor_name =
  match (target_ty, constructor_name) with
  | TOcaml "option", "Some" -> Some [ TAny ]
  | TOcaml "option", "None" -> Some []
  | TOcaml_app ("option", [ payload_ty ]), "Some" ->
      Some [ cljml_metadata_type_for_ocaml_payload payload_ty ]
  | TOcaml_app ("option", [ _ ]), "None" -> Some []
  | TOcaml "result", "Ok" -> Some [ TAny ]
  | TOcaml "result", "Error" -> Some [ TAny ]
  | TOcaml_app ("result", [ ok_ty; _ ]), "Ok" ->
      Some [ cljml_metadata_type_for_ocaml_payload ok_ty ]
  | TOcaml_app ("result", [ _; error_ty ]), "Error" ->
      Some [ cljml_metadata_type_for_ocaml_payload error_ty ]
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
  | Ok binding -> Ok (typed_ir binding.ty (Ocaml_ir.Ident binding.ocaml_name))
  | Error _ -> (
      match name with
      | "+" ->
          Ok
            (typed_ir (TFn ([ TInt; TInt ], TInt))
               (Ocaml_ir.Fun
                  ( [ Ocaml_ir.PVar "a"; Ocaml_ir.PVar "b" ],
                    Ocaml_ir.Infix ("+", Ocaml_ir.Ident "a", Ocaml_ir.Ident "b") )))
      | "-" ->
          Ok
            (typed_ir (TFn ([ TInt; TInt ], TInt))
               (Ocaml_ir.Fun
                  ( [ Ocaml_ir.PVar "a"; Ocaml_ir.PVar "b" ],
                    Ocaml_ir.Infix ("-", Ocaml_ir.Ident "a", Ocaml_ir.Ident "b") )))
      | "*" ->
          Ok
            (typed_ir (TFn ([ TInt; TInt ], TInt))
               (Ocaml_ir.Fun
                  ( [ Ocaml_ir.PVar "a"; Ocaml_ir.PVar "b" ],
                    Ocaml_ir.Infix ("*", Ocaml_ir.Ident "a", Ocaml_ir.Ident "b") )))
      | "/" ->
          Ok
            (typed_ir (TFn ([ TInt; TInt ], TInt))
               (Ocaml_ir.Fun
                  ( [ Ocaml_ir.PVar "a"; Ocaml_ir.PVar "b" ],
                    Ocaml_ir.Infix ("/", Ocaml_ir.Ident "a", Ocaml_ir.Ident "b") )))
      | "inc" ->
          Ok
            (typed_ir (TFn ([ TInt ], TInt))
               (Ocaml_ir.Fun
                  ( [ Ocaml_ir.PVar "x" ],
                    Ocaml_ir.Infix ("+", Ocaml_ir.Ident "x", Ocaml_ir.Int 1) )))
      | "dec" ->
          Ok
            (typed_ir (TFn ([ TInt ], TInt))
               (Ocaml_ir.Fun
                  ( [ Ocaml_ir.PVar "x" ],
                    Ocaml_ir.Infix ("-", Ocaml_ir.Ident "x", Ocaml_ir.Int 1) )))
      | "not" ->
          Ok
            (typed_ir (TFn ([ TBool ], TBool))
               (Ocaml_ir.Fun
                  ( [ Ocaml_ir.PVar "x" ],
                    Ocaml_ir.Prefix ("not", Ocaml_ir.Ident "x") )))
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
          Some (Type_def { type_name; type_parameters = []; fields })
      | _ -> None)
    row_type_names param_tys
  |> List.filter_map Fun.id

let row_project_expr type_name fields arg =
  let source = "__row_source" in
  Ocaml_ir.Let
    ( [ (Ocaml_ir.PVar source, arg.ocaml_expr) ],
      Ocaml_ir.Record
        ( List.map
            (fun (field : field) ->
              (field.ocaml_name, Ocaml_ir.Field (Ocaml_ir.Ident source, field.ocaml_name)))
            fields,
          Some type_name ) )

let row_arg_expr row_type_name expected_ty arg =
  match (row_type_name, expected_ty, arg.ty) with
  | Some type_name, TRecord fields, (TRecord _ | TNamed_record _) ->
      row_project_expr type_name fields arg
  | _ -> arg.ocaml_expr

let coerce_set_element element_ty value =
  match element_ty with
  | TNamed_record expected -> (
      match value.ty with
      | TNamed_record actual when actual.type_name = expected.type_name ->
          Ok value.ocaml_expr
      | (TRecord actual_fields | TNamed_record { fields = actual_fields; _ })
        when Types.compatible ~expected:element_ty ~actual:value.ty ->
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
          |> Result.map (fun fields -> Ocaml_ir.Record (fields, Some expected.type_name))
      | _ -> Error.error "set value type must match record element type")
  | _ ->
      if Types.equal element_ty value.ty then Ok value.ocaml_expr
      else Error.error "set value type must match element type"

let constrain_record_function_argument_expr fn element_ty =
  match (Ocaml_ir.unlocated fn.ocaml_expr, element_ty) with
  | Ocaml_ir.Fun ([ Ocaml_ir.PVar name ], body), TNamed_record record ->
      Ocaml_ir.Fun ([ Ocaml_ir.PConstraint (Ocaml_ir.PVar name, record.type_name) ], body)
  | _ -> fn.ocaml_expr

let param_constraint_name = function
  | (TInt | TFloat | TChar | TString | TSymbol | TKeyword | TBool | TUnit
    | TArray _ | TRef _ | TOcaml _ | TOcaml_app _ | TTuple _ | TNamed_record _) as ty ->
      Some (Types.ocaml_name ty)
  | _ -> None
