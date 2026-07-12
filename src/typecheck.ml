open Ast
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

let record_type_key scope type_name =
  "__record/" ^ scope ^ "/" ^ type_name

let record_type_application type_name parameters =
  match parameters with
  | [] -> type_name
  | [ _ ] -> "_ " ^ type_name
  | parameters ->
      "(" ^ String.concat ", " (List.map (fun _ -> "_") parameters) ^ ") "
      ^ type_name

let split_qualified_type_name type_name =
  match String.rindex_opt type_name '.' with
  | None -> None
  | Some index ->
      let module_path = String.sub type_name 0 index in
      let local_name =
        String.sub type_name (index + 1) (String.length type_name - index - 1)
      in
      Some (module_path, local_name)

let qualify_record_type module_path record =
  let type_name = Names.module_path_to_ocaml module_path ^ "." ^ record.type_name in
  {
    record with
    type_id = Types.type_id_of_name type_name;
    type_name;
    set_module_name =
      Names.module_path_to_ocaml module_path ^ "." ^ record.set_module_name;
  }

let lookup_record_type scope env type_name =
  let lookup owner local_name =
    Env.find_opt (record_type_key owner local_name) env
  in
  let local_lookup owner local_name =
    match lookup owner local_name with
    | Some ({ ty = TNamed_record record; _ } : binding) -> Ok record
    | Some _ -> Error.error ("invalid record type metadata for " ^ type_name)
    | None -> Error.error ("unknown record type " ^ type_name)
  in
  match split_qualified_type_name type_name with
  | Some (module_path, local_name) -> (
      match local_lookup (Names.module_path_to_ocaml module_path) local_name with
      | Ok record -> Ok (qualify_record_type module_path record)
      | Error _ as err -> err)
  | None -> local_lookup scope type_name

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

let lookup_binding scope env name =
  match Env.find_opt (Names.scoped_key scope name) env with
  | Some (binding : binding) -> Ok binding
  | None -> Error.error ("unknown function " ^ name)

let binding_of_expr ?(row_param_types = []) ocaml_name expr =
  Types.binding ~row_param_types ?return_param_index:expr.return_param_index
    ocaml_name expr.ty

let binding_owner key =
  match String.rindex_opt key '/' with
  | None -> ""
  | Some index -> String.sub key 0 index

let check_emitted_name_collision env ~source_key ~ocaml_name =
  let owner = binding_owner source_key in
  match
    List.find_opt
      (fun (key, (binding : binding)) ->
        (not (String.starts_with ~prefix:"__" key))
        && key <> source_key && binding_owner key = owner
        && binding.ocaml_name = ocaml_name)
      (Env.to_bindings env)
  with
  | None -> Ok ()
  | Some (existing_key, _) ->
      let source_name = Protocol.method_basename source_key in
      let existing_name = Protocol.method_basename existing_key in
      Error.error
        ("OCaml name collision: " ^ existing_name ^ " and " ^ source_name
       ^ " both emit " ^ ocaml_name)

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

let ocaml_call_target scope env function_name =
  let lookup name =
    match Env.find_opt (Names.scoped_key scope name) env with
    | Some _ as binding -> binding
    | None -> Env.find_opt name env
  in
  match lookup function_name with
  | Some { host_reference = Some (Ocaml_value ocaml_name); _ } -> Some ocaml_name
  | _ -> (
      match String.split_on_char '/' function_name with
      | [ alias; member_name ] -> (
          match lookup alias with
          | Some { host_reference = Some (Ocaml_module module_path); _ } ->
              Some (module_path ^ "." ^ Names.sanitize_name member_name)
          | _ -> None)
      | _ ->
          let first_segment =
            match String.split_on_char '.' function_name with
            | first :: _ -> first
            | [] -> ""
          in
          if String.contains function_name '.' && first_segment <> ""
             && Char.uppercase_ascii first_segment.[0] = first_segment.[0]
          then Some function_name
          else None)

let resolve_ocaml_call_target scope env function_name =
  match ocaml_call_target scope env function_name with
  | Some target -> target
  | None -> function_name

let resolve_ocaml_constructor_target scope env constructor_name =
  let lookup name =
    match Env.find_opt (Names.scoped_key scope name) env with
    | Some _ as binding -> binding
    | None -> Env.find_opt name env
  in
  match String.split_on_char '/' constructor_name with
  | [ alias; member_name ] -> (
      match lookup alias with
      | Some { host_reference = Some (Ocaml_module module_path); _ } ->
          module_path ^ "." ^ member_name
      | _ -> constructor_name)
  | _ -> constructor_name

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

let rec compile_expr scope (env : Env.t) form =
  match compile_expr_unlocated scope env form with
  | Error _ as err -> err
  | Ok expression -> (
      match Source_context.find form with
      | None -> Ok expression
      | Some location ->
          Ok
            { expression with
              ocaml_expr = Ocaml_ir.Located (location, expression.ocaml_expr);
            })

and compile_expr_unlocated scope (env : Env.t) = function
  | FInt value -> Ok (typed_ir TInt (Ocaml_ir.Int value))
  | FFloat value -> Ok (typed_ir TFloat (Ocaml_ir.Float value))
  | FChar value -> Ok (typed_ir TChar (Ocaml_ir.Char value))
  | FString value -> Ok (typed_ir TString (Ocaml_ir.String value))
  | FBool value -> Ok (typed_ir TBool (Ocaml_ir.Bool value))
  | FKeyword keyword -> Ok (typed_ir TKeyword (Ocaml_ir.String keyword))
  | FSymbol name -> (
      match Env.find_opt (Names.scoped_key scope name) env with
      | Some { ty = TFn ([], return_ty); _ } when is_constructor_name name ->
          Ok (typed_ir return_ty (Ocaml_ir.Constructor (name, None)))
      | Some binding -> Ok (typed_ir binding.ty (Ocaml_ir.Ident binding.ocaml_name))
      | None when name = "None" ->
          Ok (typed_ir (TOcaml_app ("option", [ TAny ])) (Ocaml_ir.Constructor (name, None)))
      | None -> Error.error ("unknown symbol " ^ name))
  | FVector forms -> compile_vector scope env forms
  | FMap pairs -> compile_map scope env pairs
  | FList (FSymbol "loop" :: bindings :: body_forms) ->
      compile_loop scope env bindings body_forms
  | FList (FSymbol "recur" :: _) ->
      Error.error "recur is only valid in a loop tail position"
  | FList (FSymbol "let" :: bindings :: body_forms) ->
      compile_let scope env bindings body_forms
  | FList (FSymbol "fn" :: params :: body_forms) ->
      compile_fn scope env params body_forms
  | FList (FSymbol "do" :: body_forms) ->
      compile_body scope env "do requires at least one form" body_forms
  | FList [ FKeyword keyword; target ] ->
      compile_get scope env [ target; FKeyword keyword ]
  | FList (FKeyword _ :: _) -> Error.error "keyword lookup expects one argument"
  | FList (FSymbol "if" :: condition :: then_form :: else_form :: []) ->
      compile_if scope env condition then_form else_form
  | FList (FSymbol "if-not" :: condition :: then_form :: else_form :: []) ->
      compile_if_not scope env condition then_form else_form
  | FList (FSymbol "when" :: condition :: body_forms) ->
      compile_when scope env condition body_forms
  | FList (FSymbol "cond" :: clauses) -> compile_cond scope env clauses
  | FList (FSymbol "match" :: target :: clauses) ->
      compile_match scope env target clauses
  | FList (FSymbol "try" :: forms) -> compile_try scope env forms
  | FList (FSymbol name :: args) -> compile_call scope env name args
  | FList [] -> Error.error "empty list is not callable"
  | FList _ -> Error.error "call head must be a symbol"

and compile_vector scope env forms =
  match forms with
  | [] -> Error.error "empty vector requires a type annotation"
  | first :: rest -> (
      match compile_expr scope env first with
      | Error _ as err -> err
      | Ok first_expr ->
          let rec loop acc = function
            | [] ->
                let values =
                  List.rev acc |> List.map (fun expr -> expr.ocaml_expr)
                in
                Ok
                  (typed_ir (TVector first_expr.ty)
                     (Ocaml_ir.Apply
                        (Ocaml_ir.Ident "Rrbvec.of_list", [ Ocaml_ir.List values ])))
            | form :: rest -> (
                match compile_expr scope env form with
                | Error _ as err -> err
                | Ok expr ->
                    if Types.equal first_expr.ty expr.ty then loop (expr :: acc) rest
                    else Error.error "vector elements must all have the same type")
          in
          loop [ first_expr ] rest)

and compile_map scope env pairs =
  let compile_pair = function
    | FKeyword keyword, value_form -> (
        match compile_expr scope env value_form with
        | Ok value -> Ok (keyword, value)
        | Error _ as err -> err)
    | _ -> Error.error "map keys must be keywords"
  in
  let rec loop acc = function
    | [] ->
        let pairs = List.rev acc in
        let keyword_pairs = List.map (fun (keyword, value) -> (keyword, value)) pairs in
        Structural_map.validate_unique_keywords keyword_pairs
        |> Result.map (fun () ->
               let fields =
                 pairs |> List.map (fun (keyword, value) -> make_field keyword value.ty)
               in
               let values =
                 List.map2
                   (fun field (_keyword, value) -> (field, value.ocaml_expr))
                   fields pairs
               in
               {
                 ty = TRecord fields;
                 ocaml_expr =
                   Ocaml_ir.Record
                     (List.map
                        (fun ((field : field), value) -> (field.ocaml_name, value))
                        values, None);
                 record_values = Some values;
                 return_param_index = None;
               })
    | pair :: rest -> (
        match compile_pair pair with
        | Ok pair -> loop (pair :: acc) rest
        | Error _ as err -> err)
  in
  loop [] pairs

and compile_if scope env condition then_form else_form =
  match
    ( compile_expr scope env condition,
      compile_expr scope env then_form,
      compile_expr scope env else_form )
  with
  | (Error _ as err), _, _ -> err
  | _, (Error _ as err), _ -> err
  | _, _, (Error _ as err) -> err
  | Ok condition, Ok then_expr, Ok else_expr -> (
      match ensure_bool condition with
      | Error _ as err -> err
      | Ok () ->
          if branch_types_compatible then_expr.ty else_expr.ty then
            Ok
              (typed_ir then_expr.ty
                 (Ocaml_ir.If
                    ( condition.ocaml_expr,
                      then_expr.ocaml_expr,
                      else_expr.ocaml_expr )))
          else Error.error "if branches must have same type")

and compile_if_not scope env condition then_form else_form =
  match
    ( compile_expr scope env condition,
      compile_expr scope env then_form,
      compile_expr scope env else_form )
  with
  | (Error _ as err), _, _ -> err
  | _, (Error _ as err), _ -> err
  | _, _, (Error _ as err) -> err
  | Ok condition, Ok then_expr, Ok else_expr -> (
      match ensure_bool condition with
      | Error _ as err -> err
      | Ok () ->
          if branch_types_compatible then_expr.ty else_expr.ty then
            Ok
              (typed_ir then_expr.ty
                 (Ocaml_ir.If
                    ( Ocaml_ir.Apply
                        (Ocaml_ir.Ident "not", [ condition.ocaml_expr ]),
                      then_expr.ocaml_expr,
                      else_expr.ocaml_expr )))
          else Error.error "if-not branches must have same type")

and compile_when scope env condition body_forms =
  match
    ( compile_expr scope env condition,
      compile_body scope env "when body requires at least one form" body_forms )
  with
  | (Error _ as err), _ -> err
  | _, (Error _ as err) -> err
  | Ok condition, Ok body -> (
      match ensure_bool condition with
      | Error _ as err -> err
      | Ok () ->
          if Types.equal body.ty TUnit then
            Ok
              (typed_ir body.ty
                 (Ocaml_ir.If
                    (condition.ocaml_expr, body.ocaml_expr, Ocaml_ir.Unit)))
          else Error.error "when body must be unit")

and compile_cond scope env clauses =
  let parse_pairs clauses =
    let rec loop acc = function
      | [] -> Error.error "cond requires an :else branch"
      | [ _ ] -> Error.error "cond requires test/expression pairs"
      | FKeyword ":else" :: else_form :: [] -> Ok (List.rev acc, else_form)
      | FKeyword ":else" :: _ -> Error.error "cond :else must be last"
      | test_form :: value_form :: rest -> loop ((test_form, value_form) :: acc) rest
    in
    loop [] clauses
  in
  let compile_test form =
    match compile_expr scope env form with
    | Error _ as err -> err
    | Ok test ->
        if Types.equal test.ty TBool then Ok test else Error.error "cond tests must be bool"
  in
  let rec compile_pairs acc = function
    | [] -> Ok (List.rev acc)
    | (test_form, value_form) :: rest -> (
        match (compile_test test_form, compile_expr scope env value_form) with
        | (Error _ as err), _ -> err
        | _, (Error _ as err) -> err
        | Ok test, Ok value -> compile_pairs ((test, value) :: acc) rest)
  in
  match parse_pairs clauses with
  | Error _ as err -> err
  | Ok (pairs, else_form) -> (
      match (compile_pairs [] pairs, compile_expr scope env else_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok pairs, Ok else_expr ->
          if
            List.for_all
              (fun (_test, value) -> branch_types_compatible value.ty else_expr.ty)
              pairs
          then
            let expression =
              List.fold_right
                (fun (test, value) acc ->
                  Ocaml_ir.If (test.ocaml_expr, value.ocaml_expr, acc))
                pairs else_expr.ocaml_expr
            in
            Ok (typed_ir else_expr.ty expression)
          else Error.error "cond branches must have same type")

and compile_match scope env target_form clauses =
  let rec parse_pairs acc = function
    | [] -> Ok (List.rev acc)
    | [ _ ] -> Error.error "match requires pattern/result pairs"
    | pattern :: result :: rest -> parse_pairs ((pattern, result) :: acc) rest
  in
  let literal_pattern expected_ty form =
    match compile_expr scope env form with
    | Error _ as err -> err
    | Ok pattern ->
        if Types.equal expected_ty pattern.ty then
          (match form with
          | FInt value -> Ok (Ocaml_ir.PInt value)
          | FString value | FKeyword value -> Ok (Ocaml_ir.PString value)
          | FBool value -> Ok (Ocaml_ir.PBool value)
          | _ -> Error.error "unsupported match pattern")
        else Error.error "match pattern type must match target"
  in
  let rec compile_pattern target_ty pattern =
    match (target_ty, pattern) with
    | _, FSymbol "_" -> Ok (Ocaml_ir.PAny, [])
    | target_ty, FList [ FSymbol "as"; inner_pattern; FSymbol alias ] -> (
        match compile_pattern target_ty inner_pattern with
        | Error _ as err -> err
        | Ok (inner_pattern, bindings) ->
            let ocaml_name = Names.sanitize_name alias in
            let binding =
              ( Names.scoped_key scope alias,
                Types.binding ocaml_name target_ty )
            in
            Ok (Ocaml_ir.PAlias (inner_pattern, ocaml_name), bindings @ [ binding ]))
    | target_ty, FList [ FSymbol "or"; left_form; right_form ] -> (
        match
          (compile_pattern target_ty left_form, compile_pattern target_ty right_form)
        with
        | (Error _ as err), _ -> err
        | _, (Error _ as err) -> err
        | Ok (left, left_bindings), Ok (right, right_bindings) ->
            let binding_names bindings =
              bindings |> List.map fst |> List.sort_uniq String.compare
            in
            if binding_names left_bindings <> binding_names right_bindings then
              Error.error "or-pattern alternatives must bind the same names"
            else Ok (Ocaml_ir.POr (left, right), left_bindings))
    | (TRecord fields | TNamed_record { fields; _ }),
      FList (FSymbol "record" :: field_patterns) ->
        let rec compile_fields compiled bindings seen = function
          | [] -> Ok (Ocaml_ir.PRecord (List.rev compiled), bindings)
          | FList [ FSymbol field_name; field_pattern ] :: rest ->
              let ocaml_name = Names.sanitize_name field_name in
              if List.mem ocaml_name seen then
                Error.error ("duplicate record pattern field " ^ field_name)
              else (
                match
                  List.find_opt
                    (fun (field : field) -> field.ocaml_name = ocaml_name)
                    fields
                with
                | None -> Error.error ("unknown record pattern field " ^ field_name)
                | Some field -> (
                    match compile_pattern field.ty field_pattern with
                    | Error _ as err -> err
                    | Ok (pattern, field_bindings) ->
                        compile_fields
                          ((field.ocaml_name, pattern) :: compiled)
                          (bindings @ field_bindings) (ocaml_name :: seen) rest))
          | _ -> Error.error "record pattern fields must be (name pattern)"
        in
        compile_fields [] [] [] field_patterns
    | _, FList (FSymbol "record" :: _) ->
        Error.error "record pattern expects a record target"
    | TTuple payload_tys, FList (FSymbol "ocaml-tuple" :: payload_patterns) ->
        let rec compile_payloads patterns bindings = function
          | [], [] -> Ok (List.rev patterns, bindings)
          | payload_ty :: payload_tys, pattern :: payload_patterns -> (
              match
                compile_pattern
                  (cljml_metadata_type_for_ocaml_type payload_ty)
                  pattern
              with
              | Error _ as err -> err
              | Ok (pattern, pattern_bindings) ->
                  compile_payloads (pattern :: patterns)
                    (bindings @ pattern_bindings)
                    (payload_tys, payload_patterns))
          | _ -> Error.error "tuple pattern arity mismatch"
        in
        compile_payloads [] [] (payload_tys, payload_patterns)
        |> Result.map (fun (patterns, bindings) -> (Ocaml_ir.PTuple patterns, bindings))
    | target_ty, FSymbol name
      when is_ocaml_owned_type target_ty && starts_with_uppercase name ->
        Ok (Ocaml_ir.PConstructor (name, None), [])
    | target_ty, FList (FSymbol name :: payload_patterns)
      when is_ocaml_owned_type target_ty && starts_with_uppercase name -> (
        let builtin_constructor_payloads =
          ocaml_builtin_constructor_payloads target_ty name
        in
        let compile_constructor_payloads payload_tys =
          let rec compile_payloads patterns bindings = function
            | [], [] -> Ok (List.rev patterns, bindings)
            | payload_ty :: payload_tys, pattern :: payload_patterns -> (
                match compile_pattern payload_ty pattern with
                | Error _ as err -> err
                | Ok (pattern, pattern_bindings) ->
                    compile_payloads (pattern :: patterns)
                      (bindings @ pattern_bindings)
                      (payload_tys, payload_patterns))
            | _ -> Error.error "constructor pattern arity mismatch"
          in
          compile_payloads [] [] (payload_tys, payload_patterns)
          |> Result.map (fun (patterns, bindings) ->
                 let payload_pattern =
                   match patterns with
                   | [] -> None
                   | [ pattern ] -> Some pattern
                   | _ -> Some (Ocaml_ir.PTuple patterns)
                 in
                 (Ocaml_ir.PConstructor (name, payload_pattern), bindings))
        in
        match builtin_constructor_payloads with
        | Some payload_tys -> compile_constructor_payloads payload_tys
        | None -> (
            match lookup_binding scope env name with
            | Error _ ->
                let opaque_payload_tys =
                  List.map (fun _ -> TAny) payload_patterns
                in
                compile_constructor_payloads opaque_payload_tys
            | Ok constructor -> (
                match constructor.ty with
                | TFn (payload_tys, _)
                  when List.length payload_tys = List.length payload_patterns ->
                    compile_constructor_payloads payload_tys
                | TFn _ -> Error.error "constructor pattern arity mismatch"
                | _ -> Error.error (name ^ " is not a constructor"))))
    | _, FSymbol name ->
        let ocaml_name = Names.sanitize_name name in
        Ok
          ( Ocaml_ir.PVar ocaml_name,
            [ (Names.scoped_key scope name, Types.binding ocaml_name target_ty) ] )
    | TInt, FInt value -> Ok (Ocaml_ir.PInt value, [])
    | TString, FString value -> Ok (Ocaml_ir.PString value, [])
    | TKeyword, FKeyword keyword -> Ok (Ocaml_ir.PString keyword, [])
    | TBool, FBool value -> Ok (Ocaml_ir.PBool value, [])
    | TList inner, FVector patterns ->
        compile_list_like_pattern inner patterns
    | TVector inner, FVector patterns ->
        compile_list_like_pattern inner patterns
    | _ -> (
        match pattern with
        | FInt _ | FString _ | FKeyword _ | FBool _ ->
            literal_pattern target_ty pattern |> Result.map (fun code -> (code, []))
        | FVector _ -> Error.error "match collection pattern must match target collection"
        | _ -> Error.error "unsupported match pattern")
  and compile_list_like_pattern inner patterns =
    let rec loop compiled_patterns bindings = function
      | [] -> Ok (List.rev compiled_patterns, bindings)
      | pattern :: rest -> (
          match compile_pattern inner pattern with
          | Error _ as err -> err
          | Ok (compiled_pattern, pattern_bindings) ->
              loop (compiled_pattern :: compiled_patterns)
                (bindings @ pattern_bindings) rest)
    in
    loop [] [] patterns
    |> Result.map (fun (patterns, bindings) -> (Ocaml_ir.PList patterns, bindings))
  in
  let compile_clause target_ty (pattern_form, result_form) =
    let pattern_form, guard_form =
      match pattern_form with
      | FList [ FSymbol "when"; pattern_form; guard_form ] ->
          (pattern_form, Some guard_form)
      | pattern_form -> (pattern_form, None)
    in
    match compile_pattern target_ty pattern_form with
    | Error _ as err -> err
    | Ok (pattern_code, bindings) -> (
        let clause_env = Env.add_bindings bindings env in
        let guard =
          match guard_form with
          | None -> Ok None
          | Some guard_form -> (
              match compile_expr scope clause_env guard_form with
              | Error _ as err -> err
              | Ok guard when Types.equal guard.ty TBool -> Ok (Some guard.ocaml_expr)
              | Ok _ -> Error.error "match guard must be bool")
        in
        match (guard, compile_expr scope clause_env result_form) with
        | (Error _ as err), _ -> err
        | _, (Error _ as err) -> err
        | Ok guard, Ok result -> Ok (pattern_code, guard, result))
  in
  match (compile_expr scope env target_form, parse_pairs [] clauses) with
  | (Error _ as err), _ -> err
  | _, (Error _ as err) -> err
  | Ok target, Ok pairs -> (
      let target_expr =
        match target.ty with
        | TVector _ ->
            Ocaml_ir.Apply (Ocaml_ir.Ident "Rrbvec.to_list", [ target.ocaml_expr ])
        | _ -> target.ocaml_expr
      in
      let rec compile_clauses acc = function
        | [] -> Ok (List.rev acc)
        | pair :: rest -> (
            match compile_clause target.ty pair with
            | Error _ as err -> err
            | Ok clause -> compile_clauses (clause :: acc) rest)
      in
      match compile_clauses [] pairs with
      | Error _ as err -> err
      | Ok [] -> Error.error "match requires pattern/result pairs"
      | Ok ((_, _, first_result) :: _ as clauses) ->
          if
            List.for_all
              (fun (_, _, result) ->
                branch_types_compatible first_result.ty result.ty)
              clauses
          then
            Ok
              (typed_ir first_result.ty
                 (Ocaml_ir.Match_guarded
                    ( target_expr,
                      clauses
                      |> List.map (fun (pattern, guard, result) ->
                             (pattern, guard, result.ocaml_expr)) )))
          else Error.error "match branches must have same type")

and compile_body scope env empty_error forms =
  match forms with
  | [] -> Error.error empty_error
  | [ form ] -> compile_expr scope env form
  | form :: rest -> (
      match (compile_expr scope env form, compile_body scope env empty_error rest) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok expr, Ok body ->
          Ok
            (typed_ir body.ty
               (Ocaml_ir.Sequence [ expr.ocaml_expr; body.ocaml_expr ])))

and compile_try scope env forms =
  let is_catch_clause = function
    | FList (FSymbol "catch" :: _) -> true
    | _ -> false
  in
  let rec split_body acc = function
    | [] -> Error.error "try requires at least one catch clause"
    | form :: rest when is_catch_clause form -> Ok (List.rev acc, form :: rest)
    | form :: rest -> split_body (form :: acc) rest
  in
  let parse_catch = function
    | FList (FSymbol "catch" :: pattern :: body_forms) -> (
        match body_forms with
        | [] -> Error.error "catch requires a pattern and body"
        | [ body ] -> Ok (pattern, body)
        | body_forms -> Ok (pattern, FList (FSymbol "do" :: body_forms)))
    | FList [ FSymbol "catch" ] -> Error.error "catch requires a pattern and body"
    | _ -> Error.error "try handlers must be catch clauses"
  in
  let rec parse_catches acc = function
    | [] -> Ok (List.rev acc)
    | form :: rest -> (
        match parse_catch form with
        | Error _ as err -> err
        | Ok clause -> parse_catches (clause :: acc) rest)
  in
  let compatible_try_type body_ty handlers_ty =
    match (body_ty, handlers_ty) with
    | TAny, ty | ty, TAny -> Ok ty
    | _ when branch_types_compatible body_ty handlers_ty -> Ok body_ty
    | _ -> Error.error "try body and handlers must have the same type"
  in
  match split_body [] forms with
  | Error _ as err -> err
  | Ok ([], _) -> Error.error "try requires a body"
  | Ok (body_forms, catch_forms) -> (
      match (compile_body scope env "try requires a body" body_forms, parse_catches [] catch_forms) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok body, Ok catches -> (
          let exception_name = "__cljml_caught_exception" in
          let exception_binding =
            ( Names.scoped_key scope exception_name,
              Types.binding exception_name (TOcaml "exn") )
          in
          match
            compile_match scope
              (Env.add (fst exception_binding) (snd exception_binding) env)
              (FSymbol exception_name)
              (List.concat_map (fun (pattern, handler) -> [ pattern; handler ]) catches)
          with
          | Error _ as err -> err
          | Ok handlers -> (
              match (handlers.ocaml_expr, compatible_try_type body.ty handlers.ty) with
              | _, (Error _ as err) -> err
              | Ocaml_ir.Match_guarded (_, cases), Ok ty ->
                  Ok (typed_ir ty (Ocaml_ir.Try (body.ocaml_expr, cases)))
              | _, Ok _ -> Error.error "internal error: malformed try handlers")))

and loop_branch_type left right =
  match (left, right) with
  | TAny, ty | ty, TAny -> Ok ty
  | left, right when branch_types_compatible left right -> Ok left
  | _ -> Error.error "loop branches must have same type"

and compile_recur scope env loop_name param_tys arg_forms =
  if List.length arg_forms <> List.length param_tys then
    Error.error
      ("recur expects " ^ string_of_int (List.length param_tys) ^ " arguments")
  else
    match compile_args_for scope env arg_forms with
    | Error _ as err -> err
    | Ok args ->
        let rec validate index expected actual =
          match (expected, actual) with
          | [], [] -> Ok ()
          | expected_ty :: expected, arg :: actual ->
              if branch_types_compatible expected_ty arg.ty then
                validate (index + 1) expected actual
              else
                Error.error
                  ("recur argument " ^ string_of_int index ^ " must be "
                 ^ Types.source_name expected_ty)
          | _ -> Error.error "internal error: recur argument validation"
        in
        validate 1 param_tys args
        |> Result.map (fun () ->
               typed_ir TAny
                 (Ocaml_ir.Apply
                    (Ocaml_ir.Ident loop_name, List.map (fun arg -> arg.ocaml_expr) args)))

and compile_loop_tail scope env loop_name param_tys = function
  | FList (FSymbol "recur" :: arg_forms) ->
      compile_recur scope env loop_name param_tys arg_forms
  | FList [ FSymbol "if"; condition_form; then_form; else_form ] -> (
      match
        ( compile_expr scope env condition_form,
          compile_loop_tail scope env loop_name param_tys then_form,
          compile_loop_tail scope env loop_name param_tys else_form )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok condition, Ok then_expr, Ok else_expr -> (
          match (ensure_bool condition, loop_branch_type then_expr.ty else_expr.ty) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok (), Ok result_ty ->
              Ok
                (typed_ir result_ty
                   (Ocaml_ir.If
                      ( condition.ocaml_expr,
                        then_expr.ocaml_expr,
                        else_expr.ocaml_expr )))))
  | FList [ FSymbol "if-not"; condition_form; then_form; else_form ] ->
      compile_loop_tail scope env loop_name param_tys
        (FList
           [ FSymbol "if";
             FList [ FSymbol "not"; condition_form ];
             then_form;
             else_form ])
  | FList (FSymbol "do" :: body_forms) ->
      compile_loop_tail_body scope env loop_name param_tys body_forms
  | form -> compile_expr scope env form

and compile_loop_tail_body scope env loop_name param_tys forms =
  match forms with
  | [] -> Error.error "loop body requires at least one form"
  | [ form ] -> compile_loop_tail scope env loop_name param_tys form
  | form :: rest -> (
      match
        ( compile_expr scope env form,
          compile_loop_tail_body scope env loop_name param_tys rest )
      with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok expression, Ok body ->
          Ok
            (typed_ir body.ty
               (Ocaml_ir.Sequence [ expression.ocaml_expr; body.ocaml_expr ])))

and compile_loop scope env bindings body_forms =
  match bindings with
  | FVector forms ->
      if List.length forms mod 2 <> 0 then
        Error.error "loop bindings require an even number of forms"
      else
        let rec compile_bindings names values tys = function
          | [] -> Ok (List.rev names, List.rev values, List.rev tys)
          | FSymbol name :: value_form :: rest ->
              if name = "_" || List.mem name names then
                Error.error "loop binding names must be unique symbols"
              else (
                match compile_expr scope env value_form with
                | Error _ as err -> err
                | Ok value ->
                    compile_bindings (name :: names) (value :: values)
                      (value.ty :: tys) rest)
          | _ -> Error.error "loop binding names must be symbols"
        in
        (match compile_bindings [] [] [] forms with
        | Error _ as err -> err
        | Ok (names, values, param_tys) ->
            let loop_name = "loop__" in
            let loop_env =
              List.fold_left2
                (fun env name ty ->
                  Env.add (Names.scoped_key scope name)
                    (Types.binding (Names.sanitize_name name) ty)
                    env)
                env names param_tys
            in
            (match
               compile_loop_tail_body scope loop_env loop_name param_tys
                 body_forms
             with
            | Error _ as err -> err
            | Ok body ->
                let params =
                  List.map
                    (fun name -> Ocaml_ir.PVar (Names.sanitize_name name))
                    names
                in
                Ok
                  (typed_ir body.ty
                     (Ocaml_ir.LetRec
                        ( loop_name,
                          params,
                          body.ocaml_expr,
                          List.map (fun value -> value.ocaml_expr) values )))))
  | _ -> Error.error "loop bindings must be a vector"

and compile_let scope env bindings body_forms =
  match bindings with
  | FVector forms ->
      if List.length forms mod 2 <> 0 then
        Error.error "let bindings require an even number of forms"
      else
        let rec bind env ir_bindings = function
          | [] -> (
              match
                compile_body scope env "let body requires at least one form"
                  body_forms
              with
              | Error _ as err -> err
              | Ok body ->
                  Ok
                    {
                      (typed_ir body.ty
                         (Ocaml_ir.Let (List.rev ir_bindings, body.ocaml_expr)))
                      with
                      return_param_index = body.return_param_index;
                    })
          | pattern :: value_form :: rest -> (
              match compile_expr scope env value_form with
              | Error _ as err -> err
              | Ok value -> (
                  match Destructure.bind_pattern value pattern with
                  | Error _ as err -> err
                  | Ok bindings ->
                      let env_bindings =
                        match (pattern, bindings) with
                        | FSymbol name, [ binding ] ->
                            [
                              ( Names.scoped_key scope name,
                                Types.binding
                                  ?return_param_index:(value.return_param_index)
                                  binding.ocaml_name binding.ty );
                            ]
                        | _ ->
                            bindings
                            |> List.map (fun (binding : Destructure.local_binding) ->
                                   ( Names.scoped_key scope binding.source_name,
                                     Types.binding binding.ocaml_name binding.ty ))
                      in
                      let ir_bindings =
                        match pattern with
                        | FSymbol "_" -> (Ocaml_ir.PAny, value.ocaml_expr) :: ir_bindings
                        | _ ->
                            bindings
                            |> List.fold_left
                                 (fun acc (binding : Destructure.local_binding) ->
                                   (Ocaml_ir.PVar binding.ocaml_name, binding.ocaml_expr)
                                   :: acc)
                                 ir_bindings
                      in
                      bind (Env.add_bindings env_bindings env) ir_bindings rest))
          | [ _ ] -> Error.error "let bindings require an even number of forms"
        in
        bind env [] forms
  | _ -> Error.error "let bindings must be a vector"

and prepare_fn ?(param_type_overrides = []) scope env params body_forms =
  match Destructure.parse_param_specs params with
  | Error _ as err -> err
  | Ok specs ->
      let lookup_function_ty name =
        match lookup_function scope env name with
        | Ok fn -> Ok fn.ty
        | Error _ as err -> err
      in
      let inference_params =
        specs
        |> List.fold_left
             (fun acc (spec : Destructure.param_spec) ->
               let param_ty = Option.value spec.explicit_ty ~default:TAny in
               let acc = (spec.source_name, param_ty) :: acc in
               if spec.destructured then
                 Destructure.pattern_names spec.pattern
                 |> List.fold_left (fun acc name -> (name, TAny) :: acc) acc
               else acc)
             []
        |> List.rev
      in
      match Type_inference.infer_params ~lookup_function_ty inference_params body_forms with
      | Error _ as err -> err
      | Ok inferred ->
          let lookup_inferred name =
            inferred |> List.assoc_opt name |> Option.value ~default:TAny
          in
          let infer_spec_ty (spec : Destructure.param_spec) =
            if spec.destructured then
              Destructure.infer_pattern_type spec.pattern lookup_inferred
            else Ok (lookup_inferred spec.source_name)
          in
          let rec build acc = function
            | [] -> Ok (List.rev acc)
            | spec :: rest -> (
                match infer_spec_ty spec with
                | Error _ as err -> err
                | Ok ty -> build ((spec, ty) :: acc) rest)
          in
          match build [] specs with
          | Error _ as err -> err
          | Ok typed_specs ->
              let typed_specs =
                typed_specs
                |> List.mapi (fun index (spec, inferred_ty) ->
                       match List.nth_opt param_type_overrides index with
                       | Some (Some ty) -> (spec, ty)
                       | _ -> (spec, inferred_ty))
              in
              let param_bindings =
                typed_specs
                |> List.map (fun ((spec : Destructure.param_spec), ty) ->
                       ( Names.scoped_key scope spec.source_name,
                         Types.binding spec.ocaml_name ty ))
              in
              let param_targets =
                typed_specs
                |> List.map (fun ((spec : Destructure.param_spec), ty) ->
                       (spec, typed_ir ty (Ocaml_ir.Ident spec.ocaml_name)))
              in
              let destructured_bindings =
                let rec loop acc = function
                  | [] -> Ok (List.rev acc)
                  | (spec, target) :: rest ->
                      if not spec.Destructure.destructured then loop acc rest
                      else (
                        match Destructure.bind_pattern target spec.pattern with
                        | Error _ as err -> err
                        | Ok bindings -> loop (List.rev_append bindings acc) rest)
                in
                loop [] param_targets
              in
              (match destructured_bindings with
              | Error _ as err -> err
              | Ok destructured_bindings ->
                  let local_bindings =
                    destructured_bindings
                    |> List.map (fun (binding : Destructure.local_binding) ->
                           ( Names.scoped_key scope binding.source_name,
                             Types.binding binding.ocaml_name binding.ty ))
                  in
                  let env =
                    env |> Env.add_bindings param_bindings
                    |> Env.add_bindings local_bindings
                  in
                  match
                    compile_body scope env "function body requires at least one form"
                      body_forms
                  with
                  | Error _ as err -> err
                  | Ok body -> Ok { param_bindings; destructured_bindings; body })

and fn_code ?(row_param_type_names = []) parts =
  let param_names =
    parts.param_bindings |> List.map (fun (_key, binding) -> binding.ocaml_name)
  in
  let param_tys =
    parts.param_bindings |> List.map (fun (_key, (binding : binding)) -> binding.ty)
  in
  let param_patterns =
    List.map2
      (fun name ty -> (name, ty))
      param_names param_tys
    |> List.mapi (fun index (name, ty) ->
           match List.nth_opt row_param_type_names index with
           | Some (Some type_name) ->
               Ocaml_ir.PConstraint (Ocaml_ir.PVar name, type_name)
           | _ -> (
               match param_constraint_name ty with
               | Some type_name -> Ocaml_ir.PConstraint (Ocaml_ir.PVar name, type_name)
               | None -> Ocaml_ir.PVar name))
  in
  let body_expr =
    match parts.destructured_bindings with
    | [] -> parts.body.ocaml_expr
    | bindings ->
        Ocaml_ir.Let
          ( List.map
              (fun (binding : Destructure.local_binding) ->
                (Ocaml_ir.PVar binding.ocaml_name, binding.ocaml_expr))
              bindings,
            parts.body.ocaml_expr )
  in
  let return_param_index =
    match
      (parts.destructured_bindings, Ocaml_ir.unlocated parts.body.ocaml_expr)
    with
    | [], Ocaml_ir.Ident returned_name ->
        param_names
        |> List.mapi (fun index name -> (index, name))
        |> List.find_opt (fun (_index, name) -> name = returned_name)
        |> Option.map fst
    | _ -> None
  in
  { (typed_ir (TFn (param_tys, parts.body.ty)) (Ocaml_ir.Fun (param_patterns, body_expr))) with
    return_param_index }

and compile_fn ?(param_type_overrides = []) scope env params body_forms =
  match prepare_fn ~param_type_overrides scope env params body_forms with
  | Error _ as err -> err
  | Ok parts -> Ok (fn_code parts)

and compile_ocaml_arguments scope env forms =
  let rec parse acc = function
    | [] -> Ok (List.rev acc)
    | FKeyword label :: [] ->
        Error.error ("OCaml argument label " ^ label ^ " requires a value")
    | FKeyword label :: value_form :: rest ->
        let label = String.sub label 1 (String.length label - 1) in
        parse ((Some label, value_form) :: acc) rest
    | value_form :: rest -> parse ((None, value_form) :: acc) rest
  in
  let rec compile acc = function
    | [] -> Ok (List.rev acc)
    | (label, form) :: rest -> (
        match compile_expr scope env form with
        | Error _ as err -> err
        | Ok argument -> compile ((label, argument) :: acc) rest)
  in
  match parse [] forms with
  | Error _ as err -> err
  | Ok arguments -> compile [] arguments

and ocaml_apply function_name arguments =
  if List.exists (fun (label, _) -> Option.is_some label) arguments then
    Ocaml_ir.Labelled_apply
      ( Ocaml_ir.Ident function_name,
        List.map
          (fun (label, argument) -> (label, argument.ocaml_expr))
          arguments )
  else
    Ocaml_ir.Apply
      ( Ocaml_ir.Ident function_name,
        List.map (fun (_, argument) -> argument.ocaml_expr) arguments )

and compile_call scope env name arg_forms =
  let compile_args () = compile_args_for scope env arg_forms in
  let constructor ?(display_name = name) ?(constructor_name = name) return_ty
      expected_arity =
    match compile_args () with
    | Error _ as err -> err
    | Ok args when List.length args <> expected_arity ->
        Error.error
          (display_name ^ " expects " ^ string_of_int expected_arity ^ " arguments")
    | Ok args ->
        let payload =
          match args with
          | [] -> None
          | [ value ] -> Some value.ocaml_expr
          | values -> Some (Ocaml_ir.Tuple (List.map (fun value -> value.ocaml_expr) values))
        in
        Ok
          (typed_ir (return_ty args)
             (Ocaml_ir.Constructor (constructor_name, payload)))
  in
  match name with
  | "raise" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ arg ] ->
          Ok
            (typed_ir TAny
               (Ocaml_ir.Apply (Ocaml_ir.Ident "raise", [ arg.ocaml_expr ])))
      | Ok _ -> Error.error "raise expects 1 arguments")
  | "Some" ->
      constructor
        (function [ value ] -> TOcaml_app ("option", [ value.ty ]) | _ -> TAny)
        1
  | "None" -> constructor (fun _ -> TOcaml_app ("option", [ TAny ])) 0
  | "Ok" ->
      constructor
        (function
          | [ value ] -> TOcaml_app ("result", [ value.ty; TAny ])
          | _ -> TAny)
        1
  | "Error" ->
      constructor
        (function
          | [ value ] -> TOcaml_app ("result", [ TAny; value.ty ])
          | _ -> TAny)
        1
  | "ocaml-array" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [] -> Error.error "empty OCaml array requires a type"
      | Ok (first :: rest as values) ->
          if List.for_all (fun value -> Types.equal first.ty value.ty) rest then
            Ok
              (typed_ir (TArray first.ty)
                 (Ocaml_ir.Array (List.map (fun value -> value.ocaml_expr) values)))
          else Error.error "OCaml array elements must have the same type")
  | "ocaml-array-of" -> (
      match arg_forms with
      | [ FKeyword keyword ] -> (
          match Type_annotation.of_keyword keyword with
          | Error _ as err -> err
          | Ok element_ty -> Ok (typed_ir (TArray element_ty) (Ocaml_ir.Array [])))
      | _ -> Error.error "ocaml-array-of expects one type")
  | "ocaml-array-get" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ array; index ] -> (
          match array.ty with
          | TArray element_ty ->
              if Types.equal index.ty TInt then
                Ok
                  (typed_ir element_ty
                     (apply "Array.get" [ array.ocaml_expr; index.ocaml_expr ]))
              else Error.error "OCaml array index must be int"
          | _ -> Error.error "ocaml-array-get expects an OCaml array")
      | Ok _ -> Error.error "ocaml-array-get expects 2 arguments")
  | "ocaml-array-set!" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ array; index; value ] -> (
          match array.ty with
          | TArray element_ty ->
              if not (Types.equal index.ty TInt) then
                Error.error "OCaml array index must be int"
              else if not (Types.equal element_ty value.ty) then
                Error.error "OCaml array value must match element type"
              else
                Ok
                  (typed_ir TUnit
                     (apply "Array.set"
                        [ array.ocaml_expr; index.ocaml_expr; value.ocaml_expr ]))
          | _ -> Error.error "ocaml-array-set! expects an OCaml array")
      | Ok _ -> Error.error "ocaml-array-set! expects 3 arguments")
  | "ocaml-ref" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ value ] -> Ok (typed_ir (TRef value.ty) (apply "ref" [ value.ocaml_expr ]))
      | Ok _ -> Error.error "ocaml-ref expects 1 argument")
  | "ocaml-deref" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ value ] -> (
          match value.ty with
          | TRef referenced_ty ->
              Ok (typed_ir referenced_ty (Ocaml_ir.Prefix ("!", value.ocaml_expr)))
          | _ -> Error.error "ocaml-deref expects an OCaml ref")
      | Ok _ -> Error.error "ocaml-deref expects 1 argument")
  | "ocaml-reset!" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ reference; value ] -> (
          match reference.ty with
          | TRef referenced_ty ->
              if Types.equal referenced_ty value.ty then
                Ok
                  (typed_ir TUnit
                     (Ocaml_ir.Infix (":=", reference.ocaml_expr, value.ocaml_expr)))
              else Error.error "OCaml ref value must match referenced type"
          | _ -> Error.error "ocaml-reset! expects an OCaml ref")
      | Ok _ -> Error.error "ocaml-reset! expects 2 arguments")
  | "ocaml-call" -> (
      match arg_forms with
      | FKeyword return_keyword :: FSymbol function_name :: value_forms -> (
          match Type_annotation.of_keyword return_keyword with
          | Error _ -> Error.error ("unknown ocaml-call return type " ^ return_keyword)
          | Ok return_ty -> (
              match compile_ocaml_arguments scope env value_forms with
              | Error _ as err -> err
              | Ok args ->
                  let function_name =
                    resolve_ocaml_call_target scope env function_name
                  in
                  Ok (typed_ir return_ty (ocaml_apply function_name args))))
      | FSymbol function_name :: value_forms -> (
          compile_inferred_ocaml_call scope env function_name value_forms)
      | FKeyword _ :: _ ->
          Error.error "ocaml-call function must be a symbol"
      | _ -> Error.error "ocaml-call expects return type, function, and arguments")
  | "ocaml-some" ->
      constructor ~display_name:"ocaml-some" ~constructor_name:"Some"
        (fun _ -> TOcaml "option") 1
  | "ocaml-none" ->
      constructor ~display_name:"ocaml-none" ~constructor_name:"None"
        (fun _ -> TOcaml "option") 0
  | "ocaml-ok" ->
      constructor ~display_name:"ocaml-ok" ~constructor_name:"Ok"
        (fun _ -> TOcaml "result") 1
  | "ocaml-error" ->
      constructor ~display_name:"ocaml-error" ~constructor_name:"Error"
        (fun _ -> TOcaml "result") 1
  | "ocaml-tuple" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok ([] | [ _ ]) -> Error.error "ocaml-tuple expects at least 2 values"
      | Ok values ->
          Ok
            (typed_ir
               (TTuple (List.map (fun value -> value.ty) values))
               (Ocaml_ir.Tuple (List.map (fun value -> value.ocaml_expr) values))))
  | "ocaml-record" -> (
      let field_value record field_form =
        match field_form with
        | FList [ FSymbol field_name; value_form ] -> (
            let ocaml_name = Names.sanitize_name field_name in
            match
              List.find_opt
                (fun (field : field) -> field.ocaml_name = ocaml_name)
                record.fields
            with
            | None -> Error.error ("unknown record field " ^ field_name)
            | Some field -> (
                match compile_expr scope env value_form with
                | Error _ as err -> err
                | Ok value -> Ok (field, value)))
        | _ -> Error.error "ocaml-record fields must be (name value)"
      in
      let rec compile_fields record acc seen = function
        | [] -> Ok (List.rev acc)
        | field_form :: rest -> (
            match field_value record field_form with
            | Error _ as err -> err
            | Ok ((field, _value) as pair) ->
                if List.mem field.ocaml_name seen then
                  Error.error "duplicate record field name"
                else compile_fields record (pair :: acc) (field.ocaml_name :: seen) rest)
      in
      match arg_forms with
      | FSymbol type_name :: field_forms -> (
          match lookup_record_type scope env type_name with
          | Error _ as err -> err
          | Ok (record : named_record) -> (
              match compile_fields record [] [] field_forms with
              | Error _ as err -> err
              | Ok values ->
                  let missing =
                    record.fields
                    |> List.filter (fun (field : field) ->
                           not
                             (List.exists
                                (fun ((actual : field), _) ->
                                  actual.ocaml_name = field.ocaml_name)
                                values))
                  in
                  if missing <> [] then Error.error "record value is missing fields"
                  else
                    Ok
                      {
                        (typed_ir
                           (TNamed_record record)
                           (Ocaml_ir.Record
                              ( List.map
                                  (fun ((field : field), value) ->
                                    (field.ocaml_name, value.ocaml_expr))
                                  values,
                                Some
                                  (record_type_application record.type_name
                                     record.type_parameters) )))
                        with
                        record_values =
                          Some
                            (List.map
                               (fun ((field : field), value) -> (field, value.ocaml_expr))
                               values);
                      }))
      | _ -> Error.error "ocaml-record expects a record type and fields")
  | "ocaml-field" -> (
      match arg_forms with
      | [ target_form; FSymbol field_name ] -> (
          match compile_expr scope env target_form with
          | Error _ as err -> err
          | Ok target -> (
              let fields =
                match target.ty with
                | TRecord fields | TNamed_record { fields; _ } -> Ok fields
                | _ -> Error.error "ocaml-field expects a record value"
              in
              match fields with
              | Error _ as err -> err
              | Ok fields -> (
                  let ocaml_name = Names.sanitize_name field_name in
                  match
                    List.find_opt
                      (fun (field : field) -> field.ocaml_name = ocaml_name)
                      fields
                  with
                  | None -> Error.error ("unknown record field " ^ field_name)
                  | Some field ->
                      Ok
                        (typed_ir field.ty
                           (Ocaml_ir.Field (target.ocaml_expr, field.ocaml_name))))))
      | _ -> Error.error "ocaml-field expects record value and field name")
  | "ocaml-construct" -> (
      match arg_forms with
      | FSymbol constructor_name :: payload_forms -> (
          match compile_args_for scope env payload_forms with
          | Error _ as err -> err
          | Ok payloads -> (
              let constructor_ty =
                match lookup_binding scope env constructor_name with
                | Ok { ty = TFn (payload_tys, ret); _ }
                  when List.length payload_tys = List.length payloads ->
                    Ok ret
                | Ok { ty = TFn _; _ } ->
                    Error.error "ocaml-construct payload arity mismatch"
                | Ok _ -> Error.error (constructor_name ^ " is not a constructor")
                | Error _ -> Ok (TOcaml "variant")
              in
              match constructor_ty with
              | Error _ as err -> err
              | Ok constructor_ty ->
                  let payload_expr =
                    match payloads with
                    | [] -> None
                    | [ payload ] -> Some payload.ocaml_expr
                    | _ ->
                        Some
                          (Ocaml_ir.Tuple
                             (List.map (fun payload -> payload.ocaml_expr) payloads))
                  in
                  Ok
                    (typed_ir constructor_ty
                       (Ocaml_ir.Constructor (constructor_name, payload_expr)))))
      | FKeyword _ :: _ -> Error.error "ocaml-construct constructor must be a symbol"
      | _ -> Error.error "ocaml-construct expects a constructor name")
  | "+" | "-" | "*" | "/" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok args -> (
          match Core_int.expect_int_args name args with
          | Error _ as err -> err
          | Ok () -> Core_int.compile_operator name args))
  | "inc" ->
      compile_int_unary_call scope env name
        (fun expression -> Ocaml_ir.Infix ("+", expression, Ocaml_ir.Int 1))
        arg_forms
  | "dec" ->
      compile_int_unary_call scope env name
        (fun expression -> Ocaml_ir.Infix ("-", expression, Ocaml_ir.Int 1))
        arg_forms
  | "=" | "not=" | "<" | "<=" | ">" | ">=" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok args -> Core_compare.compile name args)
  | "not" | "true?" | "false?" | "int?" | "number?"
  | "string?" | "keyword?" | "boolean?" | "vector?" | "list?" | "seq?" | "set?"
  | "map?" | "fn?" | "coll?" | "associative?" | "indexed?" | "seqable?" | "counted?"
    -> compile_boolean_call scope env name arg_forms
  | "integer?" | "nat-int?" | "pos-int?" | "neg-int?" | "boolean" | "bit-set"
  | "bit-clear" | "bit-flip" | "bit-test" | "bit-shift-right-zero-fill"
  | "unchecked-add" | "unchecked-add-int" | "unchecked-subtract"
  | "unchecked-subtract-int" | "unchecked-multiply" | "unchecked-multiply-int"
  | "unchecked-divide-int" | "unchecked-remainder-int" | "unchecked-inc"
  | "unchecked-inc-int" | "unchecked-dec" | "unchecked-dec-int"
  | "unchecked-negate" | "unchecked-negate-int" | "name" | "namespace" | "keyword"
  | "symbol" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok args -> Core_scalar.compile name args)
  | "any?" | "rational?" | "ratio?" | "float?" | "double?" | "decimal?"
  | "symbol?" | "simple-symbol?" | "qualified-symbol?" | "simple-keyword?"
  | "qualified-keyword?" | "ident?" | "simple-ident?" | "qualified-ident?"
  | "sequential?" | "reversible?" | "sorted?" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok args -> Core_predicate.compile name args)
  | "zero?" ->
      compile_int_unary_call scope env name
        (fun expression -> Ocaml_ir.Infix ("=", expression, Ocaml_ir.Int 0))
        arg_forms
      |> Result.map (fun expr -> { expr with ty = TBool })
  | "pos?" ->
      compile_int_unary_call scope env name
        (fun expression -> Ocaml_ir.Infix (">", expression, Ocaml_ir.Int 0))
        arg_forms
      |> Result.map (fun expr -> { expr with ty = TBool })
  | "neg?" ->
      compile_int_unary_call scope env name
        (fun expression -> Ocaml_ir.Infix ("<", expression, Ocaml_ir.Int 0))
        arg_forms
      |> Result.map (fun expr -> { expr with ty = TBool })
  | "even?" ->
      compile_int_unary_call scope env name
        (fun expression ->
          Ocaml_ir.Infix
            ( "=",
              Ocaml_ir.Infix ("mod", expression, Ocaml_ir.Int 2),
              Ocaml_ir.Int 0 ))
        arg_forms
      |> Result.map (fun expr -> { expr with ty = TBool })
  | "odd?" ->
      compile_int_unary_call scope env name
        (fun expression ->
          Ocaml_ir.Infix
            ( "<>",
              Ocaml_ir.Infix ("mod", expression, Ocaml_ir.Int 2),
              Ocaml_ir.Int 0 ))
        arg_forms
      |> Result.map (fun expr -> { expr with ty = TBool })
  | "str" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok args ->
          let expr =
            match args with
            | [] -> Ocaml_ir.String ""
            | _ -> args |> List.map (Codegen.stringify_expr_ir ~pr:false) |> Codegen.concat_expr
          in
          Ok (typed_ir TString expr))
  | "subs" -> compile_subs scope env arg_forms
  | "max" | "min" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok args -> Core_int.compile_min_max name args)
  | "quot" | "rem" | "mod" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok args -> Core_int.compile_binary name args)
  | "bit-and" | "bit-or" | "bit-xor" ->
      (match compile_args () with
      | Error _ as err -> err
      | Ok args -> Core_int.compile_variadic_bitwise name args)
  | "bit-not" ->
      compile_int_unary_call scope env name
        (fun expression -> Ocaml_ir.Prefix ("lnot", expression))
        arg_forms
  | "bit-shift-left" | "bit-shift-right" ->
      (match compile_args () with
      | Error _ as err -> err
      | Ok args -> Core_int.compile_binary name args)
  | "pr-str" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ arg ] -> Ok (typed_ir TString (Codegen.stringify_expr_ir ~pr:true arg))
      | Ok _ -> Error.error "pr-str expects 1 arguments")
  | "print" | "println" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ arg ] ->
          let printer = if name = "print" then "print_string" else "print_endline" in
          Ok
            (typed_ir TUnit
               (Ocaml_ir.Apply (Ocaml_ir.Ident printer, [ Codegen.print_expr_ir arg ])))
      | Ok _ -> Error.error (name ^ " expects 1 arguments"))
  | "list" -> compile_list scope env arg_forms
  | "list*" -> compile_list_star scope env arg_forms
  | "range" -> compile_range scope env arg_forms
  | "list-of" -> compile_list_of arg_forms
  | "cons" -> compile_cons scope env arg_forms
  | "vector" -> compile_vector scope env arg_forms
  | "vector-of" -> compile_vector_of arg_forms
  | "count" -> compile_collection_call scope env name arg_forms
  | "conj" -> compile_conj scope env arg_forms
  | "first" | "second" | "last" | "peek" | "pop" ->
      compile_collection_call scope env name arg_forms
  | "subvec" -> compile_subvec scope env arg_forms
  | "nth" -> compile_nth scope env arg_forms
  | "get" -> compile_get scope env arg_forms
  | "assoc" -> compile_assoc scope env arg_forms
  | "dissoc" -> compile_dissoc scope env arg_forms
  | "merge" -> compile_merge scope env arg_forms
  | "update" -> compile_update scope env arg_forms
  | "select-keys" -> compile_select_keys scope env arg_forms
  | "contains?" -> compile_contains scope env arg_forms
  | "keys" -> compile_keys scope env arg_forms
  | "vals" -> compile_vals scope env arg_forms
  | "hash-map" | "array-map" | "sorted-map" -> compile_hash_map scope env arg_forms
  | "rest" | "seq" | "empty?" -> compile_collection_call scope env name arg_forms
  | "into" -> compile_sequence_transform_call scope env name arg_forms
  | "take" | "drop" -> compile_collection_call scope env name arg_forms
  | "butlast" | "take-last" | "drop-last" | "take-nth" ->
      compile_sequence_transform_call scope env name arg_forms
  | "next" | "nthnext" | "nthrest" | "ffirst" | "fnext" | "nfirst" | "nnext"
  | "rseq" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok args -> Core_sequence.compile name args)
  | "some" -> compile_some scope env arg_forms
  | "split-at" -> compile_sequence_transform_call scope env name arg_forms
  | "split-with" -> compile_split_with scope env arg_forms
  | "partition-by" -> compile_partition_by scope env arg_forms
  | "bounded-count" | "dorun" | "doall" ->
      compile_sequence_transform_call scope env name arg_forms
  | "run!" -> compile_run_bang scope env arg_forms
  | "reverse" -> compile_collection_call scope env name arg_forms
  | "every?" | "not-any?" | "not-every?" ->
      compile_sequence_bool_predicate scope env name arg_forms
  | "map" -> compile_map_call scope env arg_forms
  | "filter" -> compile_filter scope env arg_forms
  | "remove" | "take-while" | "drop-while" | "distinct" | "dedupe" | "sort" ->
      compile_sequence_transform_call scope env name arg_forms
  | "sort-by" -> compile_sort_by scope env arg_forms
  | "concat" -> compile_sequence_transform_call scope env name arg_forms
  | "mapcat" -> compile_mapcat scope env arg_forms
  | "vec" | "set" | "repeat" ->
      compile_sequence_transform_call scope env name arg_forms
  | "repeatedly" -> compile_repeatedly scope env arg_forms
  | "interpose" | "interleave" | "partition" | "partition-all" ->
      compile_sequence_transform_call scope env name arg_forms
  | "reductions" -> compile_reductions scope env arg_forms
  | "map-indexed" -> compile_map_indexed scope env arg_forms
  | "filterv" -> compile_filterv scope env arg_forms
  | "mapv" -> compile_mapv scope env arg_forms
  | "reduce-kv" -> compile_reduce_kv scope env arg_forms
  | "reduce" -> compile_reduce scope env arg_forms
  | "apply" -> compile_apply scope env arg_forms
  | "comp" -> compile_comp scope env arg_forms
  | "partial" -> compile_partial scope env arg_forms
  | "identity" -> compile_identity scope env arg_forms
  | "constantly" -> compile_constantly scope env arg_forms
  | "complement" -> compile_complement scope env arg_forms
  | "every-pred" -> compile_predicate_combinator scope env "every-pred" arg_forms
  | "some-fn" -> compile_predicate_combinator scope env "some-fn" arg_forms
  | "juxt" -> compile_juxt scope env arg_forms
  | "distinct?" -> compile_distinct_question scope env arg_forms
  | "compare" -> compile_compare scope env arg_forms
  | "max-key" | "min-key" -> compile_key_extreme scope env name arg_forms
  | "hash-set" | "sorted-set" -> compile_hash_set scope env arg_forms
  | "set-of" -> compile_set_of arg_forms
  | "disj" -> compile_disj scope env arg_forms
  | "empty" -> compile_collection_call scope env name arg_forms
  | _ when is_constructor_name name -> (
      match lookup_binding scope env name with
      | Ok { ty = TFn (payload_tys, return_ty); _ } ->
          constructor (fun _ -> return_ty) (List.length payload_tys)
      | _ ->
          let constructor_name =
            resolve_ocaml_constructor_target scope env name
          in
          (match Ocaml_signature.constructor_signature constructor_name with
          | Error _ as err -> err
          | Ok signature ->
              constructor ~constructor_name
                (fun _ -> signature.result_type)
                (List.length signature.payload_types)))
  | _ -> compile_named_function_call scope env name arg_forms

and compile_inferred_ocaml_call scope env function_name value_forms =
  match compile_ocaml_arguments scope env value_forms with
  | Error _ as err -> err
  | Ok arguments ->
          let function_name =
            resolve_ocaml_call_target scope env function_name
          in
          match Ocaml_signature.value_signature function_name with
          | Error _ as err -> err
          | Ok signature -> (
              let labels = List.map fst arguments in
              match Ocaml_signature.result_after_application signature labels with
              | Error _ as err -> err
              | Ok return_ty ->
                  Ok (typed_ir return_ty (ocaml_apply function_name arguments)))

and compile_int_unary_call scope env name build_code arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok args -> Core_int.compile_unary name args build_code

and compile_boolean_call scope env name arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok args -> Core_boolean.compile name args

and compile_collection_call scope env name arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok args -> Core_collection.compile name args

and compile_sequence_transform_call scope env name arg_forms =
  match (name, arg_forms) with
  | ("partition" | "partition-all"), FInt size :: _ when size <= 0 ->
      Error.error (name ^ " size must be positive")
  | "take-nth", FInt count :: _ when count <= 0 ->
      Error.error "take-nth n must be positive"
  | ("remove" | "take-while" | "drop-while"), [ fn_form; collection_form ] -> (
      match
        ( compile_function_arg scope env fn_form,
          compile_expr scope env collection_form )
      with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> Core_sequence_transform.compile name [ fn; collection ])
  | _ -> (
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok args -> Core_sequence_transform.compile name args)

and compile_subs scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ source; start ] -> (
      match (source.ty, start.ty) with
      | TString, TInt ->
          Ok
            (typed_ir TString
               (Ocaml_ir.Apply
                  ( Ocaml_ir.Ident "String.sub",
                    [ source.ocaml_expr;
                      start.ocaml_expr;
                      Ocaml_ir.Infix
                        ( "-",
                          Ocaml_ir.Apply
                            (Ocaml_ir.Ident "String.length", [ source.ocaml_expr ]),
                          start.ocaml_expr ) ] )))
      | TString, _ -> Error.error "subs indexes must be int"
      | _ -> Error.error "subs expects a string")
  | Ok [ source; start; stop ] -> (
      match (source.ty, start.ty, stop.ty) with
      | TString, TInt, TInt ->
          Ok
            (typed_ir TString
               (Ocaml_ir.Apply
                  ( Ocaml_ir.Ident "String.sub",
                    [ source.ocaml_expr;
                      start.ocaml_expr;
                      Ocaml_ir.Infix ("-", stop.ocaml_expr, start.ocaml_expr) ] )))
      | TString, _, _ -> Error.error "subs indexes must be int"
      | _ -> Error.error "subs expects a string")
  | Ok _ -> Error.error "subs expects string, start, and optional end"

and compile_list scope env forms =
  match forms with
  | [] -> Error.error "empty list requires a type annotation"
  | first :: rest -> (
      match compile_expr scope env first with
      | Error _ as err -> err
      | Ok first_expr ->
          let rec loop acc = function
            | [] ->
                let values =
                  List.rev acc |> List.map (fun expr -> expr.ocaml_expr)
                in
                Ok (typed_ir (TList first_expr.ty) (Ocaml_ir.List values))
            | form :: rest -> (
                match compile_expr scope env form with
                | Error _ as err -> err
                | Ok expr ->
                    if Types.equal first_expr.ty expr.ty then loop (expr :: acc) rest
                    else Error.error "list elements must all have the same type")
          in
          loop [ first_expr ] rest)

and compile_list_star scope env arg_forms =
  match List.rev arg_forms with
  | [] -> Error.error "list* expects values and final collection"
  | final_form :: prefix_forms_rev -> (
      match compile_expr scope env final_form with
      | Error _ as err -> err
      | Ok final -> (
          match Core_sequence_transform.collection_to_list_expr final with
          | Error _ -> Error.error "list* final argument must be a collection"
          | Ok (inner, final_list_expr) -> (
              let prefix_forms = List.rev prefix_forms_rev in
              match compile_args_for scope env prefix_forms with
              | Error _ as err -> err
              | Ok prefix_args ->
                  if List.for_all (fun arg -> Types.equal inner arg.ty) prefix_args then
                    let list_expr =
                      match prefix_args with
                      | [] -> final_list_expr
                      | _ ->
                          Ocaml_ir.Infix
                            ( "@",
                              Ocaml_ir.List
                                (List.map (fun arg -> arg.ocaml_expr) prefix_args),
                              final_list_expr )
                    in
                    Ok (typed_ir (TList inner) list_expr)
                  else Error.error "list* value type must match final collection element type")))

and compile_range scope env arg_forms =
  let literal_zero = function FInt 0 -> true | _ -> false in
  let range_expr start stop step =
    let current = Ocaml_ir.Ident "current" in
    let stop_ident = Ocaml_ir.Ident "stop" in
    let step_ident = Ocaml_ir.Ident "step" in
    let done_expr =
      Ocaml_ir.If
        ( Ocaml_ir.Infix (">", step_ident, Ocaml_ir.Int 0),
          Ocaml_ir.Infix (">=", current, stop_ident),
          Ocaml_ir.Infix ("<=", current, stop_ident) )
    in
    let body =
      Ocaml_ir.If
        ( Ocaml_ir.Infix ("=", step_ident, Ocaml_ir.Int 0),
          apply "invalid_arg" [ Ocaml_ir.String "range step cannot be 0" ],
          Ocaml_ir.If
            ( done_expr,
              apply "List.rev" [ Ocaml_ir.Ident "acc" ],
              apply "range"
                [ Ocaml_ir.Cons (current, Ocaml_ir.Ident "acc");
                  Ocaml_ir.Infix ("+", current, step_ident);
                  stop_ident;
                  step_ident ] ) )
    in
    Ocaml_ir.LetRec
      ( "range",
        [ Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "current"; Ocaml_ir.PVar "stop"; Ocaml_ir.PVar "step" ],
        body,
        [ Ocaml_ir.List []; start; stop; step ] )
  in
  match arg_forms with
  | [ end_form ] -> (
      match compile_expr scope env end_form with
      | Error _ as err -> err
      | Ok end_expr ->
          if Types.equal end_expr.ty TInt then
            Ok (typed_ir (TList TInt) (range_expr (Ocaml_ir.Int 0) end_expr.ocaml_expr (Ocaml_ir.Int 1)))
          else Error.error "range arguments must be int")
  | [ start_form; end_form ] -> (
      match (compile_expr scope env start_form, compile_expr scope env end_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok start_expr, Ok end_expr ->
          if Types.equal start_expr.ty TInt && Types.equal end_expr.ty TInt then
            Ok (typed_ir (TList TInt) (range_expr start_expr.ocaml_expr end_expr.ocaml_expr (Ocaml_ir.Int 1)))
          else Error.error "range arguments must be int")
  | [ start_form; end_form; step_form ] ->
      if literal_zero step_form then Error.error "range step cannot be 0"
      else (
        match
          ( compile_expr scope env start_form,
            compile_expr scope env end_form,
            compile_expr scope env step_form )
        with
        | (Error _ as err), _, _ -> err
        | _, (Error _ as err), _ -> err
        | _, _, (Error _ as err) -> err
        | Ok start_expr, Ok end_expr, Ok step_expr ->
            if
              Types.equal start_expr.ty TInt && Types.equal end_expr.ty TInt
              && Types.equal step_expr.ty TInt
            then
              Ok
                (typed_ir (TList TInt)
                   (range_expr start_expr.ocaml_expr end_expr.ocaml_expr step_expr.ocaml_expr))
            else Error.error "range arguments must be int")
  | _ -> Error.error "range expects end, start/end, or start/end/step"

and compile_list_of arg_forms =
  match arg_forms with
  | [ FKeyword keyword ] -> (
      match Type_annotation.of_keyword keyword with
      | Error _ as err -> err
      | Ok element_ty -> Ok (typed_ir (TList element_ty) (Ocaml_ir.List [])))
  | _ -> Error.error "list-of expects one type keyword"

and compile_vector_of arg_forms =
  match arg_forms with
  | [ FKeyword keyword ] -> (
      match Type_annotation.of_keyword keyword with
      | Error _ as err -> err
      | Ok element_ty ->
          Ok (typed_ir (TVector element_ty) (Ocaml_ir.Ident "Rrbvec.empty")))
  | _ -> Error.error "vector-of expects one type keyword"

and compile_conj scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok (collection :: values) when values <> [] ->
      let add_value collection value =
        match collection.ty with
        | TList inner when Types.equal inner value.ty ->
            Ok
              (typed_ir collection.ty
                 (Ocaml_ir.Cons (value.ocaml_expr, collection.ocaml_expr)))
        | TList _ -> Error.error "conj value type must match list element type"
        | TVector inner when Types.equal inner value.ty ->
            Ok
              (typed_ir collection.ty
                 (Ocaml_ir.Apply
                    ( Ocaml_ir.Ident "Rrbvec.push_back",
                      [ collection.ocaml_expr; value.ocaml_expr ] )))
        | TVector _ -> Error.error "conj value type must match vector element type"
        | TSet inner when Types.same_shape inner value.ty ->
            Result.bind (Types.set_module_name inner) (fun set_module ->
                   coerce_set_element inner value
                   |> Result.map (fun value ->
                          typed_ir collection.ty
                            (Ocaml_ir.Apply
                               ( Ocaml_ir.Ident (set_module ^ ".add"),
                                 [ value; collection.ocaml_expr ] ))))
        | TSet _ -> Error.error "conj value type must match set element type"
        | _ -> Error.error "conj expects a list, vector, or set"
      in
      values
      |> List.fold_left
           (fun acc value ->
             match acc with
             | Error _ as err -> err
             | Ok collection -> add_value collection value)
           (Ok collection)
  | Ok _ -> Error.error "conj expects collection and values"

and compile_cons scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ value; collection ] -> (
      match collection.ty with
      | TList inner when Types.equal inner value.ty ->
          Ok
            (typed_ir collection.ty
               (Ocaml_ir.Cons (value.ocaml_expr, collection.ocaml_expr)))
      | TList _ -> Error.error "cons value type must match list element type"
      | _ -> Error.error "cons expects a value and list")
  | Ok _ -> Error.error "cons expects value and list"

and compile_subvec scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ vector; start ] -> (
      match (vector.ty, start.ty) with
      | TVector _, TInt ->
          Ok
            (typed_ir vector.ty
               (Ocaml_ir.Apply
                  ( Ocaml_ir.Ident "Option.get",
                    [ Ocaml_ir.Apply
                        ( Ocaml_ir.Ident "Rrbvec.subvec",
                          [ vector.ocaml_expr;
                            start.ocaml_expr;
                            Ocaml_ir.Apply
                              (Ocaml_ir.Ident "Rrbvec.length", [ vector.ocaml_expr ]) ] ) ] )))
      | TVector _, _ -> Error.error "subvec indexes must be int"
      | _ -> Error.error "subvec expects a vector")
  | Ok [ vector; start; stop ] -> (
      match (vector.ty, start.ty, stop.ty) with
      | TVector _, TInt, TInt ->
          Ok
            (typed_ir vector.ty
               (Ocaml_ir.Apply
                  ( Ocaml_ir.Ident "Option.get",
                    [ Ocaml_ir.Apply
                        ( Ocaml_ir.Ident "Rrbvec.subvec",
                          [ vector.ocaml_expr; start.ocaml_expr; stop.ocaml_expr ] ) ] )))
      | TVector _, _, _ -> Error.error "subvec indexes must be int"
      | _ -> Error.error "subvec expects a vector")
  | Ok _ -> Error.error "subvec expects vector, start, and optional stop"

and compile_nth scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ collection; index ] -> (
      match (collection.ty, index.ty) with
      | TList inner, TInt ->
          Ok
            (typed_ir inner
               (Ocaml_ir.Apply
                  (Ocaml_ir.Ident "List.nth", [ collection.ocaml_expr; index.ocaml_expr ])))
      | TList _, _ -> Error.error "nth index must be int"
      | TVector inner, TInt ->
          Ok
            (typed_ir inner
               (Ocaml_ir.Apply
                  (Ocaml_ir.Ident "Rrbvec.nth", [ collection.ocaml_expr; index.ocaml_expr ])))
      | TVector _, _ -> Error.error "nth index must be int"
      | _ -> Error.error "nth expects a list or vector")
  | Ok [ collection; index; default ] -> (
      match (collection.ty, index.ty) with
      | TList inner, TInt when Types.equal inner default.ty ->
          Ok
            (typed_ir inner
               (Ocaml_ir.If
                  ( Ocaml_ir.Infix ("<", index.ocaml_expr, Ocaml_ir.Int 0),
                    default.ocaml_expr,
                    Ocaml_ir.Match
                      ( apply "List.nth_opt" [ collection.ocaml_expr; index.ocaml_expr ],
                        [ ( Ocaml_ir.PConstructor ("Some", Some (Ocaml_ir.PVar "value")),
                            Ocaml_ir.Ident "value" );
                          (Ocaml_ir.PConstructor ("None", None), default.ocaml_expr) ] ) )))
      | TList _, TInt -> Error.error "nth default must match collection element type"
      | TList _, _ -> Error.error "nth index must be int"
      | TVector inner, TInt when Types.equal inner default.ty ->
          Ok
            (typed_ir inner
               (Ocaml_ir.Match
                  ( apply "Rrbvec.nth_opt" [ collection.ocaml_expr; index.ocaml_expr ],
                    [ ( Ocaml_ir.PConstructor ("Some", Some (Ocaml_ir.PVar "value")),
                        Ocaml_ir.Ident "value" );
                      (Ocaml_ir.PConstructor ("None", None), default.ocaml_expr) ] )))
      | TVector _, TInt -> Error.error "nth default must match collection element type"
      | TVector _, _ -> Error.error "nth index must be int"
      | _ -> Error.error "nth expects a list or vector")
  | Ok _ -> Error.error "nth expects 2 or 3 arguments"

and compile_get scope env arg_forms =
  match arg_forms with
  | [ target_form; FKeyword keyword ] -> (
      match compile_expr scope env target_form with
      | Error _ as err -> err
      | Ok target -> (
          match target.ty with
          | TRecord fields | TNamed_record { fields; _ } -> (
              match find_field keyword fields with
              | Some field ->
                  Ok
                    (typed_ir field.ty
                       (Structural_map.field_expr target field))
              | None -> Error.error ("unknown field " ^ keyword))
          | _ -> Error.error "get expects a map"))
  | [ target_form; index_form ] -> (
      match (compile_expr scope env target_form, compile_expr scope env index_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok target, Ok index -> (
          match (target.ty, index.ty) with
          | TVector inner, TInt ->
              Ok
                (typed_ir inner
                   (apply "Rrbvec.nth" [ target.ocaml_expr; index.ocaml_expr ]))
          | TVector _, _ -> Error.error "get vector index must be int"
          | _ -> Error.error "get key must be a keyword"))
  | [ target_form; FKeyword keyword; default_form ] -> (
      match
        (compile_expr scope env target_form, compile_expr scope env default_form)
      with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok target, Ok default -> (
          match target.ty with
          | TRecord fields | TNamed_record { fields; _ } -> (
              match find_field keyword fields with
              | Some field when Types.equal field.ty default.ty ->
                  Ok
                    (typed_ir field.ty
                       (Structural_map.field_expr target field))
              | Some field ->
                  Error.error
                    ("get default for " ^ keyword ^ " must be " ^ source_name field.ty)
              | None -> Ok default)
          | _ -> Error.error "get expects a map"))
  | [ target_form; index_form; default_form ] -> (
      match
        ( compile_expr scope env target_form,
          compile_expr scope env index_form,
          compile_expr scope env default_form )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok target, Ok index, Ok default -> (
          match (target.ty, index.ty) with
          | TVector inner, TInt when Types.equal inner default.ty ->
              Ok
                (typed_ir inner
                   (Ocaml_ir.Match
                      ( apply "Rrbvec.nth_opt" [ target.ocaml_expr; index.ocaml_expr ],
                        [ ( Ocaml_ir.PConstructor ("Some", Some (Ocaml_ir.PVar "value")),
                            Ocaml_ir.Ident "value" );
                          (Ocaml_ir.PConstructor ("None", None), default.ocaml_expr) ] )))
          | TVector _, TInt -> Error.error "get default for vector must match element type"
          | TVector _, _ -> Error.error "get vector index must be int"
          | _ -> Error.error "get key must be a keyword"))
  | _ -> Error.error "get expects 2 or 3 arguments"

and compile_assoc scope env arg_forms =
  match arg_forms with
  | target_form :: pair_forms ->
      let rec compile_record_pairs acc = function
        | [] -> Ok (List.rev acc)
        | FKeyword keyword :: value_form :: rest -> (
            match compile_expr scope env value_form with
            | Error _ as err -> err
            | Ok value -> compile_record_pairs ((keyword, value) :: acc) rest)
        | _ -> Error.error "assoc expects map followed by keyword/value pairs"
      in
      let rec compile_vector_pairs acc = function
        | [] -> Ok (List.rev acc)
        | index_form :: value_form :: rest -> (
            match
              ( compile_expr scope env index_form,
                compile_expr scope env value_form )
            with
            | (Error _ as err), _ -> err
            | _, (Error _ as err) -> err
            | Ok index, Ok value -> compile_vector_pairs ((index, value) :: acc) rest)
        | _ -> Error.error "assoc expects collection followed by key/value pairs"
      in
      (match compile_expr scope env target_form with
      | Error _ as err -> err
      | Ok target -> (
          if pair_forms = [] || List.length pair_forms mod 2 <> 0 then
            match target.ty with
            | TRecord _ | TNamed_record _ ->
                Error.error "assoc expects map followed by keyword/value pairs"
            | TVector _ -> Error.error "assoc expects vector followed by index/value pairs"
            | _ -> Error.error "assoc expects collection followed by key/value pairs"
          else
            match target.ty with
            | TRecord _ | TNamed_record _ -> (
                match compile_record_pairs [] pair_forms with
                | Error _ as err -> err
                | Ok pairs -> Structural_map.assoc_many target pairs)
            | TVector inner -> (
                match compile_vector_pairs [] pair_forms with
                | Error _ as err -> err
                | Ok pairs ->
                    let rec apply_pairs expr = function
                      | [] -> Ok expr
                      | (index, value) :: rest ->
                          if not (Types.equal index.ty TInt) then
                            Error.error "assoc vector index must be int"
                          else if not (Types.equal value.ty inner) then
                            Error.error "assoc vector value must match element type"
                          else
                            apply_pairs
                              (apply "Rrbvec.set"
                                 [ expr; index.ocaml_expr; value.ocaml_expr ])
                              rest
                    in
                    (match apply_pairs target.ocaml_expr pairs with
                    | Error _ as err -> err
                    | Ok expr -> Ok (typed_ir target.ty expr)))
            | _ -> Error.error "assoc expects a map or vector"))
  | _ -> Error.error "assoc expects collection followed by key/value pairs"

and compile_dissoc scope env arg_forms =
  match arg_forms with
  | target_form :: key_forms -> (
      let rec parse_keys acc = function
        | [] -> Ok (List.rev acc)
        | FKeyword keyword :: rest -> parse_keys (keyword :: acc) rest
        | _ -> Error.error "dissoc expects map followed by keywords"
      in
      match compile_expr scope env target_form with
      | Error _ as err -> err
      | Ok target -> (
          match parse_keys [] key_forms with
          | Error _ as err -> err
          | Ok keywords -> Structural_map.dissoc_many target keywords))
  | _ -> Error.error "dissoc expects map followed by keywords"

and compile_merge scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok maps -> Structural_map.merge maps

and compile_hash_map scope env arg_forms =
  let rec parse_pairs acc = function
    | [] -> Ok (List.rev acc)
    | FKeyword keyword :: value_form :: rest ->
        parse_pairs ((FKeyword keyword, value_form) :: acc) rest
    | _ -> Error.error "hash-map expects keyword/value pairs"
  in
  if arg_forms = [] || List.length arg_forms mod 2 <> 0 then
    Error.error "hash-map expects keyword/value pairs"
  else
    match parse_pairs [] arg_forms with
    | Error _ as err -> err
    | Ok pairs -> compile_map scope env pairs

and compile_update scope env arg_forms =
  match arg_forms with
  | target_form :: FKeyword keyword :: fn_form :: extra_forms -> (
      match
        ( compile_expr scope env target_form,
          compile_function_arg scope env fn_form,
          compile_args_for scope env extra_forms )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok target, Ok fn, Ok extra_args -> (
          match target.ty with
          | TRecord fields | TNamed_record { fields; _ } -> (
              match find_field keyword fields with
              | None -> Error.error ("cannot update unknown field " ^ keyword)
              | Some field -> (
                  match fn.ty with
                  | TFn (param_tys, ret)
                    when List.length param_tys = List.length extra_args + 1
                         && Types.compatible ~expected:(List.hd param_tys)
                              ~actual:field.ty
                         && List.for_all2
                              (fun expected arg -> Types.compatible ~expected ~actual:arg.ty)
                              (drop 1 param_tys) extra_args
                         && Types.equal ret field.ty ->
                      let value_expr =
                        Ocaml_ir.Apply
                          ( fn.ocaml_expr,
                            Structural_map.field_expr target field
                            :: List.map (fun arg -> arg.ocaml_expr) extra_args )
                      in
                      Structural_map.update_value target fields keyword ret value_expr
                  | TFn (_param_tys, ret) when not (Types.equal ret field.ty) ->
                      Error.error
                        (Printf.sprintf "cannot update %s as %s because it is already %s"
                           keyword (source_name ret) (source_name field.ty))
                  | TFn _ ->
                      Error.error
                        "update function arguments do not match field and extra arguments"
                  | _ -> Error.error "update expects a function"))
          | _ -> Error.error "update expects a map"))
  | target_form :: index_form :: fn_form :: extra_forms -> (
      match
        ( compile_expr scope env target_form,
          compile_expr scope env index_form,
          compile_function_arg scope env fn_form,
          compile_args_for scope env extra_forms )
      with
      | (Error _ as err), _, _, _ -> err
      | _, (Error _ as err), _, _ -> err
      | _, _, (Error _ as err), _ -> err
      | _, _, _, (Error _ as err) -> err
      | Ok target, Ok index, Ok fn, Ok extra_args -> (
          match (target.ty, index.ty) with
          | TVector inner, TInt -> (
              match fn.ty with
              | TFn (param_tys, ret)
                when List.length param_tys = List.length extra_args + 1
                     && Types.compatible ~expected:(List.hd param_tys) ~actual:inner
                     && List.for_all2
                          (fun expected arg -> Types.compatible ~expected ~actual:arg.ty)
                          (drop 1 param_tys) extra_args
                     && Types.equal ret inner ->
                  let old_expr =
                    apply "Rrbvec.nth" [ target.ocaml_expr; index.ocaml_expr ]
                  in
                  let value_expr =
                    Ocaml_ir.Apply
                      (fn.ocaml_expr, old_expr :: List.map (fun arg -> arg.ocaml_expr) extra_args)
                  in
                  Ok
                    (typed_ir target.ty
                       (apply "Rrbvec.set"
                          [ target.ocaml_expr; index.ocaml_expr; value_expr ]))
              | TFn (_param_tys, ret) when not (Types.equal ret inner) ->
                  Error.error
                    ("cannot update vector element as " ^ source_name ret
                   ^ " because it is already " ^ source_name inner)
              | TFn _ ->
                  Error.error
                    "update function arguments do not match vector element and extra arguments"
              | _ -> Error.error "update expects a function")
          | TVector _, _ -> Error.error "update vector index must be int"
          | _ -> Error.error "update expects a map or vector"))
  | _ -> Error.error "update expects collection, key/index, function, and optional arguments"

and compile_select_keys scope env arg_forms =
  match arg_forms with
  | [ target_form; FVector key_forms ] -> (
      let rec parse_keywords acc = function
        | [] -> Ok (List.rev acc)
        | FKeyword keyword :: rest -> parse_keywords (keyword :: acc) rest
        | _ -> Error.error "select-keys expects a vector of keywords"
      in
      match (compile_expr scope env target_form, parse_keywords [] key_forms) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok target, Ok keywords -> (
          match target.ty with
          | TRecord fields | TNamed_record { fields; _ } ->
              Structural_map.select_keys target fields keywords
          | _ -> Error.error "select-keys expects a map"))
  | [ _; _ ] -> Error.error "select-keys expects a vector of keywords"
  | _ -> Error.error "select-keys expects map and key vector"

and compile_contains scope env arg_forms =
  let compile_collection_contains target value =
    match (target.ty, value.ty) with
    | TSet inner, _ when Types.same_shape inner value.ty ->
        Result.bind (Types.set_module_name inner) (fun set_module ->
               coerce_set_element inner value
               |> Result.map (fun value ->
                      typed_ir TBool
                        (Ocaml_ir.Apply
                           (Ocaml_ir.Ident (set_module ^ ".mem"),
                            [ value; target.ocaml_expr ]))))
    | TSet _, _ -> Error.error "contains? value type must match set element type"
    | TVector _, TInt ->
        Ok
          (typed_ir TBool
             (Ocaml_ir.Infix
                ( "&&",
                  Ocaml_ir.Infix (">=", value.ocaml_expr, Ocaml_ir.Int 0),
                  Ocaml_ir.Infix
                    ( "<",
                      value.ocaml_expr,
                      Ocaml_ir.Apply
                        (Ocaml_ir.Ident "Rrbvec.length", [ target.ocaml_expr ]) ) )))
    | TVector _, _ -> Error.error "contains? vector index must be int"
    | _ -> Error.error "contains? expects a map, set, or vector"
  in
  match arg_forms with
  | target_form :: FKeyword keyword :: [] -> (
      match compile_expr scope env target_form with
      | Error _ as err -> err
      | Ok target -> (
          match target.ty with
          | TRecord fields | TNamed_record { fields; _ } ->
              Ok
                (typed_ir TBool
                   (Ocaml_ir.Bool (Option.is_some (find_field keyword fields))))
          | _ ->
              compile_collection_contains target
                (typed_ir TKeyword (Ocaml_ir.String keyword))))
  | target_form :: value_form :: [] -> (
      match (compile_expr scope env target_form, compile_expr scope env value_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok target, Ok value -> compile_collection_contains target value)
  | _ -> Error.error "contains? expects collection and key"

and compile_keys scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ target ] -> (
      match target.ty with
      | TRecord fields | TNamed_record { fields; _ } ->
          Ok
            (typed_ir (TVector TKeyword)
               (Ocaml_ir.Apply
                  ( Ocaml_ir.Ident "Rrbvec.of_list",
                    [ Ocaml_ir.List
                        (fields
                        |> List.map (fun (field : field) ->
                               Ocaml_ir.String field.keyword)) ] )))
      | _ -> Error.error "keys expects a map")
  | Ok _ -> Error.error "keys expects 1 arguments"

and compile_vals scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ target ] -> (
      match target.ty with
      | TRecord [] | TNamed_record { fields = []; _ } ->
          Error.error "vals requires a non-empty map"
      | TRecord (first :: rest) | TNamed_record { fields = first :: rest; _ } ->
          if List.for_all (fun (field : field) -> Types.equal first.ty field.ty) rest then
            Ok
              (typed_ir (TVector first.ty)
                 (Ocaml_ir.Apply
                    ( Ocaml_ir.Ident "Rrbvec.of_list",
                      [ Ocaml_ir.List
                          ((first :: rest)
                          |> List.map (fun (field : field) ->
                                 Structural_map.field_expr target field)) ] )))
          else Error.error "vals requires all map values to have the same type"
      | _ -> Error.error "vals expects a map")
  | Ok _ -> Error.error "vals expects 1 arguments"

and compile_function_arg scope env = function
  | FSymbol name -> lookup_function scope env name
  | form -> compile_expr scope env form

and compile_function_arg_for_collection scope env element_ty = function
  | FList (FSymbol "fn" :: FVector [ FSymbol name ] :: body_forms) ->
      let binding = Types.binding (Names.sanitize_name name) element_ty in
      let function_env =
        Env.add (Names.scoped_key scope name) binding env
      in
      compile_body scope function_env "function body requires at least one form"
        body_forms
      |> Result.map (fun body ->
             let pattern =
               match element_ty with
               | TNamed_record record ->
                   Ocaml_ir.PConstraint
                     (Ocaml_ir.PVar binding.ocaml_name, record.type_name)
               | _ -> Ocaml_ir.PVar binding.ocaml_name
             in
             typed_ir (TFn ([ element_ty ], body.ty))
               (Ocaml_ir.Fun ([ pattern ], body.ocaml_expr)))
  | form -> compile_function_arg scope env form

and compile_named_function_call scope env name arg_forms =
  match ocaml_call_target scope env name with
  | Some _ -> compile_inferred_ocaml_call scope env name arg_forms
  | None -> (
      match lookup_binding scope env name with
  | Error _ -> compile_protocol_call scope env name arg_forms
  | Ok fn -> (
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok args -> (
          match fn.ty with
          | TFn (param_tys, ret)
            when List.length param_tys = List.length args
                 && List.for_all2
                      (fun expected arg -> Types.compatible ~expected ~actual:arg.ty)
                      param_tys args ->
              let arg_exprs =
                args
                |> List.mapi (fun index arg ->
                       let row_type_name = List.nth_opt fn.row_param_types index |> Option.join in
                      let expected_ty = List.nth param_tys index in
                      row_arg_expr row_type_name expected_ty arg)
              in
              let ret =
                match (fn.return_param_index, ret) with
                | Some index, TAny -> (
                    match List.nth_opt args index with
                    | Some arg -> arg.ty
                    | None -> ret)
                | _ -> ret
              in
              Ok (typed_ir ret (Ocaml_ir.Apply (Ocaml_ir.Ident fn.ocaml_name, arg_exprs)))
          | TFn _ -> Error.error (name ^ " called with incompatible arguments")
          | _ -> Error.error (name ^ " is not callable"))))

and compile_protocol_call scope env name arg_forms =
  if Protocol.method_is_ambiguous scope env name then
    Error.error
      ("ambiguous protocol method " ^ name ^ "; use Protocol/method")
  else match Protocol.lookup_marker scope env name with
  | None -> Error.error ("unknown function " ^ name)
  | Some marker -> (
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok args -> (
          match marker.ty with
          | TFn (param_tys, _ret) when List.length param_tys <> List.length args ->
              Error.error (name ^ " called with incompatible arguments")
          | TFn (_, _) -> (
              match args with
              | [] -> Error.error (name ^ " called with incompatible arguments")
              | receiver :: _ -> (
                  let method_name = Protocol.method_basename name in
                  match
                    Protocol.lookup_impl env marker.ocaml_name method_name receiver.ty
                  with
                  | None ->
                      Error.error
                        ("no protocol implementation for " ^ name ^ " and "
                       ^ source_name receiver.ty)
                  | Some impl -> (
                      match impl.ty with
                      | TFn (param_tys, ret)
                        when List.length param_tys = List.length args
                             && List.for_all2
                                  (fun expected arg ->
                                    Types.compatible ~expected ~actual:arg.ty)
                                  param_tys args ->
                          Ok
                            (typed_ir ret
                               (Ocaml_ir.Apply
                                  ( Ocaml_ir.Ident impl.ocaml_name,
                                    List.map (fun arg -> arg.ocaml_expr) args )))
                      | TFn _ -> Error.error (name ^ " called with incompatible arguments")
                      | _ -> Error.error (name ^ " is not callable"))))
          | _ -> Error.error (name ^ " is not callable")))

and collection_to_list_expr collection =
  Core_sequence_transform.collection_to_list_expr collection

and collection_from_list_expr collection_ty list_expr =
  Core_sequence_transform.collection_from_list_expr collection_ty list_expr

and comparable_type = function
  | TInt | TString | TSymbol | TKeyword | TBool | TAny -> true
  | _ -> false

and compile_sort_by scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], key_ty), Ok (inner, list_expr)
            when Types.equal param_ty inner && comparable_type key_ty ->
              Ok
                (typed_ir (TList inner)
                   (apply "List.sort"
                      [ Ocaml_ir.Fun
                          ( [ Ocaml_ir.PVar "left"; Ocaml_ir.PVar "right" ],
                            apply "Stdlib.compare"
                              [ Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "left" ]);
                                Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "right" ]) ] );
                        list_expr ]))
          | TFn ([ param_ty ], _), Ok (inner, _) when not (Types.equal param_ty inner) ->
              Error.error "sort-by key function must match collection elements"
          | TFn _, Ok _ -> Error.error "sort-by key function must return a comparable value"
          | _, Ok _ -> Error.error "sort-by expects a function"
          | _, Error _ -> Error.error "sort-by expects a collection"))
  | _ -> Error.error "sort-by expects function and collection"

and compile_mapcat scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], TList ret_inner), Ok (inner, list_expr)
            when Types.equal param_ty inner ->
              Ok
                (typed_ir (TList ret_inner)
                   (apply "List.concat"
                      [ apply "List.map" [ fn.ocaml_expr; list_expr ] ]))
          | TFn ([ param_ty ], TVector ret_inner), Ok (inner, list_expr)
            when Types.equal param_ty inner ->
              Ok
                (typed_ir (TList ret_inner)
                   (apply "List.concat"
                      [ apply "List.map"
                          [ Ocaml_ir.Fun
                              ( [ Ocaml_ir.PVar "item" ],
                                apply "Rrbvec.to_list"
                                  [ Ocaml_ir.Apply
                                      (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]) ] );
                            list_expr ] ]))
          | TFn ([ param_ty ], TSet ret_inner), Ok (inner, list_expr)
            when Types.equal param_ty inner -> (
              match Types.set_module_name ret_inner with
              | Error _ as err -> err
              | Ok set_module ->
                  Ok
                    (typed_ir (TList ret_inner)
                       (apply "List.concat"
                          [ apply "List.map"
                              [ Ocaml_ir.Fun
                                  ( [ Ocaml_ir.PVar "item" ],
                                    apply (set_module ^ ".elements")
                                      [ Ocaml_ir.Apply
                                          (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]) ] );
                                list_expr ] ])))
          | TFn ([ param_ty ], _), Ok (inner, _) when not (Types.equal param_ty inner) ->
              Error.error "mapcat function argument type does not match collection"
          | TFn _, Ok _ -> Error.error "mapcat function must return a collection"
          | _, Ok _ -> Error.error "mapcat expects a function"
          | _, Error _ -> Error.error "mapcat expects a collection"))
  | _ -> Error.error "mapcat expects function and collection"

and compile_repeatedly scope env arg_forms =
  match arg_forms with
  | count_form :: fn_form :: [] -> (
      match (compile_expr scope env count_form, compile_function_arg scope env fn_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok count, Ok fn -> (
          if not (Types.equal count.ty TInt) then Error.error "repeatedly count must be int"
          else
            match fn.ty with
            | TFn ([], ret) ->
                let body =
                  Ocaml_ir.If
                    ( Ocaml_ir.Infix ("<=", Ocaml_ir.Ident "n", Ocaml_ir.Int 0),
                      Ocaml_ir.Ident "acc",
                      apply "repeatedly"
                        [ Ocaml_ir.Cons
                            ( Ocaml_ir.Apply (fn.ocaml_expr, []),
                              Ocaml_ir.Ident "acc" );
                          Ocaml_ir.Infix ("-", Ocaml_ir.Ident "n", Ocaml_ir.Int 1) ] )
                in
                Ok
                  (typed_ir (TList ret)
                     (Ocaml_ir.LetRec
                        ( "repeatedly",
                          [ Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "n" ],
                          body,
                          [ Ocaml_ir.List []; count.ocaml_expr ] )))
            | TFn _ -> Error.error "repeatedly expects a zero-argument function"
            | _ -> Error.error "repeatedly expects a function"))
  | _ -> Error.error "repeatedly expects count and function"

and compile_reductions scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ acc_ty; item_ty ], ret), Ok (inner, list_expr)
            when Types.equal acc_ty inner && Types.equal item_ty inner && Types.equal ret inner ->
              let reductions_body =
                Ocaml_ir.Match
                  ( Ocaml_ir.Ident "xs",
                    [ (Ocaml_ir.PList [], apply "List.rev" [ Ocaml_ir.Ident "acc" ]);
                      ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "tail"),
                        Ocaml_ir.Let
                          ( [ ( Ocaml_ir.PVar "next",
                                Ocaml_ir.Apply
                                  ( fn.ocaml_expr,
                                    [ Ocaml_ir.Ident "current"; Ocaml_ir.Ident "item" ] ) ) ],
                            apply "reductions"
                              [ Ocaml_ir.Ident "next";
                                Ocaml_ir.Cons
                                  (Ocaml_ir.Ident "next", Ocaml_ir.Ident "acc");
                                Ocaml_ir.Ident "tail" ] ) ) ] )
              in
              Ok
                (typed_ir (TList inner)
                   (Ocaml_ir.Match
                      ( list_expr,
                        [ (Ocaml_ir.PList [], Ocaml_ir.List []);
                          ( Ocaml_ir.PCons (Ocaml_ir.PVar "first", Ocaml_ir.PVar "rest"),
                            Ocaml_ir.LetRec
                              ( "reductions",
                                [ Ocaml_ir.PVar "current";
                                  Ocaml_ir.PVar "acc";
                                  Ocaml_ir.PVar "xs" ],
                                reductions_body,
                                [ Ocaml_ir.Ident "first";
                                  Ocaml_ir.List [ Ocaml_ir.Ident "first" ];
                                  Ocaml_ir.Ident "rest" ] ) ) ] )))
          | TFn _, Ok _ -> Error.error "reductions function type does not match collection"
          | _, Ok _ -> Error.error "reductions expects a function"
          | _, Error _ -> Error.error "reductions expects a collection"))
  | fn_form :: init_form :: collection_form :: [] -> (
      match
        ( compile_function_arg scope env fn_form,
          compile_expr scope env init_form,
          compile_expr scope env collection_form )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok fn, Ok init, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ acc_ty; item_ty ], ret), Ok (inner, list_expr)
            when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
              let reductions_body =
                Ocaml_ir.Match
                  ( Ocaml_ir.Ident "xs",
                    [ (Ocaml_ir.PList [], apply "List.rev" [ Ocaml_ir.Ident "acc" ]);
                      ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "rest"),
                        Ocaml_ir.Let
                          ( [ ( Ocaml_ir.PVar "next",
                                Ocaml_ir.Apply
                                  ( fn.ocaml_expr,
                                    [ Ocaml_ir.Ident "current"; Ocaml_ir.Ident "item" ] ) ) ],
                            apply "reductions"
                              [ Ocaml_ir.Ident "next";
                                Ocaml_ir.Cons
                                  (Ocaml_ir.Ident "next", Ocaml_ir.Ident "acc");
                                Ocaml_ir.Ident "rest" ] ) ) ] )
              in
              Ok
                (typed_ir (TList init.ty)
                   (Ocaml_ir.LetRec
                      ( "reductions",
                        [ Ocaml_ir.PVar "current";
                          Ocaml_ir.PVar "acc";
                          Ocaml_ir.PVar "xs" ],
                        reductions_body,
                        [ init.ocaml_expr;
                          Ocaml_ir.List [ init.ocaml_expr ];
                          list_expr ] )))
          | TFn _, Ok _ -> Error.error "reductions function type does not match init and collection"
          | _, Ok _ -> Error.error "reductions expects a function"
          | _, Error _ -> Error.error "reductions expects a collection"))
  | _ -> Error.error "reductions expects function, optional init, and collection"

and compile_split_with scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], TBool), Ok (inner, list_expr) when Types.equal param_ty inner ->
              let split_body =
                Ocaml_ir.Match
                  ( Ocaml_ir.Ident "rest",
                    [ ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "tail"),
                        Ocaml_ir.If
                          ( Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]),
                            apply "split"
                              [ Ocaml_ir.Cons
                                  (Ocaml_ir.Ident "item", Ocaml_ir.Ident "prefix");
                                Ocaml_ir.Ident "tail" ],
                            Ocaml_ir.Tuple
                              [ apply "List.rev" [ Ocaml_ir.Ident "prefix" ];
                                Ocaml_ir.Ident "rest" ] ) );
                      ( Ocaml_ir.PAny,
                        Ocaml_ir.Tuple
                          [ apply "List.rev" [ Ocaml_ir.Ident "prefix" ];
                            Ocaml_ir.Ident "rest" ] ) ] )
              in
              let pair_expr =
                Ocaml_ir.LetRec
                  ( "split",
                    [ Ocaml_ir.PVar "prefix"; Ocaml_ir.PVar "rest" ],
                    split_body,
                    [ Ocaml_ir.List []; list_expr ] )
              in
              Ok
                (typed_ir (TVector collection.ty)
                   (Ocaml_ir.Let
                      ( [ (Ocaml_ir.PVar "pair", pair_expr) ],
                        apply "Rrbvec.of_list"
                          [ Ocaml_ir.List
                              [ collection_from_list_expr collection.ty
                                  (apply "fst" [ Ocaml_ir.Ident "pair" ]);
                                collection_from_list_expr collection.ty
                                  (apply "snd" [ Ocaml_ir.Ident "pair" ]) ] ] )))
          | TFn _, Ok _ -> Error.error "split-with expects a predicate matching collection elements"
          | _, Ok _ -> Error.error "split-with expects a function"
          | _, Error _ -> Error.error "split-with expects a collection"))
  | _ -> Error.error "split-with expects function and collection"

and compile_partition_by scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], key_ty), Ok (inner, list_expr) when Types.equal param_ty inner ->
              ignore key_ty;
              let finish_call =
                apply "finish" [ Ocaml_ir.Ident "groups"; Ocaml_ir.Ident "current" ]
              in
              let start_new_group =
                Ocaml_ir.Let
                  ( [ ( Ocaml_ir.PVar "groups",
                        Ocaml_ir.Match
                          ( Ocaml_ir.Ident "current",
                            [ (Ocaml_ir.PList [], Ocaml_ir.Ident "groups");
                              ( Ocaml_ir.PAny,
                                Ocaml_ir.Cons
                                  ( apply "List.rev" [ Ocaml_ir.Ident "current" ],
                                    Ocaml_ir.Ident "groups" ) ) ] ) ) ],
                    apply "partition"
                      [ Ocaml_ir.Ident "groups";
                        Ocaml_ir.List [ Ocaml_ir.Ident "item" ];
                        Ocaml_ir.Constructor ("Some", Some (Ocaml_ir.Ident "key"));
                        Ocaml_ir.Ident "rest" ] )
              in
              let partition_body =
                Ocaml_ir.Match
                  ( Ocaml_ir.Ident "xs",
                    [ (Ocaml_ir.PList [], finish_call);
                      ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "rest"),
                        Ocaml_ir.Let
                          ( [ ( Ocaml_ir.PVar "key",
                                Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]) ) ],
                            Ocaml_ir.Match
                              ( Ocaml_ir.Ident "current_key",
                                [ ( Ocaml_ir.PConstructor ("Some", Some (Ocaml_ir.PVar "previous")),
                                    Ocaml_ir.If
                                      ( Ocaml_ir.Infix
                                          ( "=", Ocaml_ir.Ident "previous",
                                            Ocaml_ir.Ident "key" ),
                                        apply "partition"
                                          [ Ocaml_ir.Ident "groups";
                                            Ocaml_ir.Cons
                                              ( Ocaml_ir.Ident "item",
                                                Ocaml_ir.Ident "current" );
                                            Ocaml_ir.Ident "current_key";
                                            Ocaml_ir.Ident "rest" ],
                                        start_new_group ) );
                                  (Ocaml_ir.PAny, start_new_group) ] ) ) ) ] )
              in
              let finish_body =
                Ocaml_ir.Match
                  ( Ocaml_ir.Ident "current",
                    [ (Ocaml_ir.PList [], apply "List.rev" [ Ocaml_ir.Ident "groups" ]);
                      ( Ocaml_ir.PAny,
                        apply "List.rev"
                          [ Ocaml_ir.Cons
                              ( apply "List.rev" [ Ocaml_ir.Ident "current" ],
                                Ocaml_ir.Ident "groups" ) ] ) ] )
              in
              Ok
                (typed_ir (TList (TList inner))
                   (Ocaml_ir.LetRecIn
                      ( "finish",
                        [ Ocaml_ir.PVar "groups"; Ocaml_ir.PVar "current" ],
                        finish_body,
                        Ocaml_ir.LetRec
                          ( "partition",
                            [ Ocaml_ir.PVar "groups";
                              Ocaml_ir.PVar "current";
                              Ocaml_ir.PVar "current_key";
                              Ocaml_ir.PVar "xs" ],
                            partition_body,
                            [ Ocaml_ir.List [];
                              Ocaml_ir.List [];
                              Ocaml_ir.Constructor ("None", None);
                              list_expr ] ) )))
          | TFn _, Ok _ -> Error.error "partition-by function type does not match collection"
          | _, Ok _ -> Error.error "partition-by expects a function"
          | _, Error _ -> Error.error "partition-by expects a collection"))
  | _ -> Error.error "partition-by expects function and collection"

and compile_run_bang scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], _ret), Ok (inner, list_expr) when Types.equal param_ty inner ->
              Ok
                (typed_ir TUnit
                   (Ocaml_ir.Let
                      ( [ ( Ocaml_ir.PUnit,
                            apply "List.iter"
                              [ Ocaml_ir.Fun
                                  ( [ Ocaml_ir.PVar "item" ],
                                    apply "ignore"
                                      [ Ocaml_ir.Apply
                                          (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]) ] );
                                list_expr ] ) ],
                        Ocaml_ir.Unit )))
          | TFn _, Ok _ -> Error.error "run! function type does not match collection"
          | _, Ok _ -> Error.error "run! expects a function"
          | _, Error _ -> Error.error "run! expects a collection"))
  | _ -> Error.error "run! expects function and collection"

and compile_map_indexed scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ TInt; item_ty ], ret), Ok (inner, list_expr) when Types.equal item_ty inner ->
              Ok
                (typed_ir (TList ret)
                   (apply "List.mapi"
                      [ Ocaml_ir.Fun
                          ( [ Ocaml_ir.PVar "index"; Ocaml_ir.PVar "item" ],
                            Ocaml_ir.Apply
                              ( fn.ocaml_expr,
                                [ Ocaml_ir.Ident "index"; Ocaml_ir.Ident "item" ] ) );
                        list_expr ]))
          | TFn _, Ok _ -> Error.error "map-indexed function type does not match collection"
          | _, Ok _ -> Error.error "map-indexed expects a function"
          | _, Error _ -> Error.error "map-indexed expects a collection"))
  | _ -> Error.error "map-indexed expects function and collection"

and compile_filterv scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], TBool), Ok (inner, list_expr) when Types.equal param_ty inner ->
              Ok
                (typed_ir (TVector inner)
                   (apply "Rrbvec.of_list"
                      [ apply "List.filter" [ fn.ocaml_expr; list_expr ] ]))
          | TFn _, Ok _ ->
              Error.error "filterv expects a predicate matching collection elements"
          | _, Ok _ -> Error.error "filterv expects a function"
          | _, Error _ -> Error.error "filterv expects a collection"))
  | _ -> Error.error "filterv expects function and collection"

and compile_mapv scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], ret), Ok (inner, list_expr) when Types.equal param_ty inner ->
              Ok
                (typed_ir (TVector ret)
                   (apply "Rrbvec.of_list"
                      [ apply "List.map" [ fn.ocaml_expr; list_expr ] ]))
          | TFn _, Ok _ -> Error.error "mapv function type does not match collection"
          | _, Ok _ -> Error.error "mapv expects a function"
          | _, Error _ -> Error.error "mapv expects a collection"))
  | _ -> Error.error "mapv expects function and collection"

and compile_reduce_kv scope env arg_forms =
  match arg_forms with
  | fn_form :: init_form :: collection_form :: [] -> (
      match
        ( compile_function_arg scope env fn_form,
          compile_expr scope env init_form,
          compile_expr scope env collection_form )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok fn, Ok init, Ok collection -> (
          match (fn.ty, collection.ty) with
          | TFn ([ acc_ty; TInt; item_ty ], ret), TVector inner
            when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
              Ok
                (typed_ir init.ty
                   (apply "List.fold_left"
                      [ Ocaml_ir.Fun
                          ( [ Ocaml_ir.PVar "acc";
                              Ocaml_ir.PTuple
                                [ Ocaml_ir.PVar "index"; Ocaml_ir.PVar "item" ] ],
                            Ocaml_ir.Apply
                              ( fn.ocaml_expr,
                                [ Ocaml_ir.Ident "acc";
                                  Ocaml_ir.Ident "index";
                                  Ocaml_ir.Ident "item" ] ) );
                        init.ocaml_expr;
                        apply "List.mapi"
                          [ Ocaml_ir.Fun
                              ( [ Ocaml_ir.PVar "index"; Ocaml_ir.PVar "item" ],
                                Ocaml_ir.Tuple
                                  [ Ocaml_ir.Ident "index"; Ocaml_ir.Ident "item" ] );
                            apply "Rrbvec.to_list" [ collection.ocaml_expr ] ] ]))
          | TFn _, TVector _ -> Error.error "reduce-kv function type does not match vector"
          | _, TVector _ -> Error.error "reduce-kv expects a function"
          | _ -> Error.error "reduce-kv expects a vector"))
  | _ -> Error.error "reduce-kv expects function, init, and vector"

and compile_some scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], TBool), Ok (inner, list_expr) when Types.equal param_ty inner ->
              Ok (typed_ir TBool (apply "List.exists" [ fn.ocaml_expr; list_expr ]))
          | TFn _, Ok _ -> Error.error "some expects a predicate matching collection elements"
          | _, Ok _ -> Error.error "some expects a function"
          | _, Error _ -> Error.error "some expects a collection"))
  | _ -> Error.error "some expects function and collection"

and compile_sequence_bool_predicate scope env name arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          let build all_expr =
            match name with
            | "every?" -> all_expr
            | "not-any?" -> all_expr
            | "not-every?" -> Ocaml_ir.Prefix ("not", all_expr)
            | _ -> all_expr
          in
          let predicate_expr =
            match name with
            | "not-any?" ->
                Ocaml_ir.Fun
                  ( [ Ocaml_ir.PVar "item" ],
                    Ocaml_ir.Prefix
                      ("not", Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ])) )
            | _ -> fn.ocaml_expr
          in
          match (fn.ty, collection.ty) with
          | TFn ([ param_ty ], TBool), TList inner when Types.equal param_ty inner ->
              let all_expr = apply "List.for_all" [ predicate_expr; collection.ocaml_expr ] in
              Ok (typed_ir TBool (build all_expr))
          | TFn _, TList _ -> Error.error (name ^ " expects a predicate matching list elements")
          | _, TList _ -> Error.error (name ^ " expects a function")
          | TFn ([ param_ty ], TBool), TVector inner when Types.equal param_ty inner ->
              let all_expr = apply "Rrbvec.for_all" [ predicate_expr; collection.ocaml_expr ] in
              Ok (typed_ir TBool (build all_expr))
          | TFn _, TVector _ ->
              Error.error (name ^ " expects a predicate matching vector elements")
          | _, TVector _ -> Error.error (name ^ " expects a function")
          | TFn ([ param_ty ], TBool), TSet inner
            when Types.compatible ~expected:param_ty ~actual:inner ->
              Types.set_module_name inner
              |> Result.map (fun set_module ->
                     let fn_expr = constrain_record_function_argument_expr fn inner in
                     let predicate_expr =
                       match name with
                       | "not-any?" ->
                           Ocaml_ir.Fun
                             ( [ Ocaml_ir.PVar "item" ],
                               Ocaml_ir.Prefix
                                 ( "not",
                                   Ocaml_ir.Apply (fn_expr, [ Ocaml_ir.Ident "item" ]) ) )
                       | _ -> fn_expr
                     in
                     let all_expr =
                       apply "List.for_all"
                         [ predicate_expr;
                           apply (set_module ^ ".elements") [ collection.ocaml_expr ] ]
                     in
                     typed_ir TBool (build all_expr))
          | TFn _, TSet _ -> Error.error (name ^ " expects a predicate matching set elements")
          | _, TSet _ -> Error.error (name ^ " expects a function")
          | _ -> Error.error (name ^ " expects a list, vector, or set")))
  | _ -> Error.error (name ^ " expects function and collection")

and compile_map_call scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match compile_expr scope env collection_form with
      | Error _ as err -> err
      | Ok collection ->
          let fn =
            match collection.ty with
            | TList inner | TVector inner | TSet inner ->
                compile_function_arg_for_collection scope env inner fn_form
            | _ -> compile_function_arg scope env fn_form
          in
          (match fn with
          | Error _ as err -> err
          | Ok fn -> (
          match (fn.ty, collection.ty) with
          | TFn ([ param_ty ], ret), TList inner when Types.equal param_ty inner ->
              Ok
                (typed_ir (TList ret)
                   (apply "List.map" [ fn.ocaml_expr; collection.ocaml_expr ]))
          | TFn _, TList _ -> Error.error "map function argument type does not match list"
          | _, TList _ -> Error.error "map expects a function"
          | TFn ([ param_ty ], ret), TVector inner when Types.equal param_ty inner ->
              Ok
                (typed_ir (TVector ret)
                   (apply "Rrbvec.map" [ fn.ocaml_expr; collection.ocaml_expr ]))
          | TFn _, TVector _ -> Error.error "map function argument type does not match vector"
          | _, TVector _ -> Error.error "map expects a function"
          | TFn ([ param_ty ], ret), TSet inner
            when Types.compatible ~expected:param_ty ~actual:inner ->
              Result.bind (Types.set_module_name ret) (fun result_module ->
                  Types.set_module_name inner
                  |> Result.map (fun source_module ->
                         let fn_expr = constrain_record_function_argument_expr fn inner in
                         typed_ir (TSet ret)
                           (apply (result_module ^ ".of_list")
                              [ apply "List.map"
                                  [ fn_expr;
                                    apply (source_module ^ ".elements")
                                      [ collection.ocaml_expr ] ] ])))
          | TFn _, TSet _ -> Error.error "map function argument type does not match set"
          | _, TSet _ -> Error.error "map expects a function"
          | _ -> Error.error "map expects a list, vector, or set")))
  | _ -> Error.error "map expects function and collection"

and compile_filter scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection.ty) with
          | TFn ([ param_ty ], TBool), TList inner when Types.equal param_ty inner ->
              Ok
                (typed_ir collection.ty
                   (apply "List.filter" [ fn.ocaml_expr; collection.ocaml_expr ]))
          | TFn _, TList _ -> Error.error "filter expects a predicate matching list elements"
          | _, TList _ -> Error.error "filter expects a function"
          | TFn ([ param_ty ], TBool), TVector inner when Types.equal param_ty inner ->
              Ok
                (typed_ir collection.ty
                   (apply "Rrbvec.filter" [ fn.ocaml_expr; collection.ocaml_expr ]))
          | TFn _, TVector _ -> Error.error "filter expects a predicate matching vector elements"
          | _, TVector _ -> Error.error "filter expects a function"
          | TFn ([ param_ty ], TBool), TSet inner
            when Types.compatible ~expected:param_ty ~actual:inner ->
              Types.set_module_name inner
              |> Result.map (fun set_module ->
                     let fn_expr = constrain_record_function_argument_expr fn inner in
                     typed_ir collection.ty
                       (apply (set_module ^ ".of_list")
                          [ apply "List.filter"
                              [ fn_expr;
                                apply (set_module ^ ".elements")
                                  [ collection.ocaml_expr ] ] ]))
          | TFn _, TSet _ -> Error.error "filter expects a predicate matching set elements"
          | _, TSet _ -> Error.error "filter expects a function"
          | _ -> Error.error "filter expects a list, vector, or set"))
  | _ -> Error.error "filter expects function and collection"

and compile_reduce scope env arg_forms =
  match arg_forms with
  | fn_form :: init_form :: collection_form :: [] -> (
      match
        ( compile_function_arg scope env fn_form,
          compile_expr scope env init_form,
          compile_expr scope env collection_form )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok fn, Ok init, Ok collection -> (
          match (fn.ty, collection.ty) with
          | TFn ([ acc_ty; item_ty ], ret), TList inner
            when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
              Ok
                (typed_ir init.ty
                   (apply "List.fold_left"
                      [ fn.ocaml_expr; init.ocaml_expr; collection.ocaml_expr ]))
          | TFn _, TList _ -> Error.error "reduce function type does not match init and list"
          | _, TList _ -> Error.error "reduce expects a function"
          | TFn ([ acc_ty; item_ty ], ret), TVector inner
            when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
             Ok
                (typed_ir init.ty
                   (apply "Rrbvec.fold_left"
                      [ fn.ocaml_expr; init.ocaml_expr; collection.ocaml_expr ]))
          | TFn _, TVector _ -> Error.error "reduce function type does not match init and vector"
          | _, TVector _ -> Error.error "reduce expects a function"
          | TFn ([ acc_ty; item_ty ], ret), TSet inner
            when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
              Types.set_module_name inner
              |> Result.map (fun set_module ->
                     typed_ir init.ty
                       (apply "List.fold_left"
                          [ fn.ocaml_expr;
                            init.ocaml_expr;
                            apply (set_module ^ ".elements")
                              [ collection.ocaml_expr ] ]))
          | TFn _, TSet _ -> Error.error "reduce function type does not match init and set"
          | _, TSet _ -> Error.error "reduce expects a function"
          | _ -> Error.error "reduce expects a list, vector, or set"))
  | _ -> Error.error "reduce expects function, init, and collection"

and compile_apply scope env arg_forms =
  let rec split_last acc = function
    | [] -> None
    | [ last ] -> Some (List.rev acc, last)
    | item :: rest -> split_last (item :: acc) rest
  in
  match arg_forms with
  | fn_form :: rest -> (
      match split_last [] rest with
      | None -> Error.error "apply expects function and collection"
      | Some (fixed_forms, collection_form) -> (
          match
            ( compile_function_arg scope env fn_form,
              compile_args_for scope env fixed_forms,
              compile_expr scope env collection_form )
          with
          | (Error _ as err), _, _ -> err
          | _, (Error _ as err), _ -> err
          | _, _, (Error _ as err) -> err
          | Ok fn, Ok fixed_args, Ok collection -> (
              match collection_to_list_expr collection with
              | Error _ -> Error.error "apply expects a list, vector, or set"
              | Ok (inner, list_expr) -> (
                  match fn.ty with
                  | TFn ([ TInt; TInt ], TInt)
                    when Types.equal inner TInt
                         && List.for_all (fun arg -> Types.equal arg.ty TInt) fixed_args ->
                      let values_expr =
                        match fixed_args with
                        | [] -> list_expr
                        | _ ->
                            Ocaml_ir.Infix
                              ( "@",
                                Ocaml_ir.List
                                  (List.map (fun arg -> arg.ocaml_expr) fixed_args),
                                list_expr )
                      in
                      Ok
                        (typed_ir TInt
                           (apply "List.fold_left"
                              [ fn.ocaml_expr; Ocaml_ir.Int 0; values_expr ]))
                  | TFn ([ TInt; TInt ], TInt) ->
                      Error.error "apply currently supports int binary reducers"
                  | TFn _ -> Error.error "apply currently supports int binary reducers"
                  | _ -> Error.error "apply expects a function"))))
  | _ -> Error.error "apply expects function and collection"

and compile_comp scope env arg_forms =
  match arg_forms with
  | [] -> Error.error "comp expects at least 1 function"
  | _ -> (
      let compiled =
        arg_forms
        |> List.fold_left
             (fun acc form ->
               match acc with
               | Error _ as err -> err
               | Ok fns -> (
                   match compile_function_arg scope env form with
                   | Error _ as err -> err
                   | Ok fn -> Ok (fn :: fns)))
             (Ok [])
        |> Result.map List.rev
      in
      match compiled with
      | Error _ as err -> err
      | Ok fns -> (
          let rec check_chain = function
            | [] -> Error.error "comp expects at least 1 function"
            | [ fn ] -> (
                match fn.ty with
                | TFn ([ arg ], ret) -> Ok (arg, ret)
                | TFn _ -> Error.error "comp expects unary functions"
                | _ -> Error.error "comp expects functions")
            | left :: (right :: _ as rest) -> (
                match (left.ty, right.ty) with
                | TFn ([ left_arg ], _left_ret), TFn ([ _right_arg ], right_ret)
                  when Types.equal left_arg right_ret ->
                    check_chain rest |> Result.map (fun (arg, _ret) ->
                        match List.hd fns with
                        | { ty = TFn ([ _ ], final_ret); _ } -> (arg, final_ret)
                        | _ -> (arg, right_ret))
                | TFn _, TFn _ -> Error.error "comp function types do not line up"
                | _ -> Error.error "comp expects functions")
          in
          match check_chain fns with
          | Error _ as err -> err
          | Ok (arg_ty, ret_ty) ->
              let inner =
                List.rev fns
                |> List.fold_left
                     (fun expression fn ->
                       Ocaml_ir.Apply (fn.ocaml_expr, [ expression ]))
                     (Ocaml_ir.Ident "x")
              in
              Ok
                (typed_ir (TFn ([ arg_ty ], ret_ty))
                   (Ocaml_ir.Fun ([ Ocaml_ir.PVar "x" ], inner)))))

and compile_partial scope env arg_forms =
  match arg_forms with
  | fn_form :: fixed_forms -> (
      match (compile_function_arg scope env fn_form, compile_args_for scope env fixed_forms) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok fixed_args -> (
          match fn.ty with
          | TFn (param_tys, ret) when List.length fixed_args < List.length param_tys ->
              let fixed_tys = List.map (fun arg -> arg.ty) fixed_args in
              let expected_fixed_tys = param_tys |> List.filteri (fun index _ -> index < List.length fixed_tys) in
              if List.for_all2 Types.equal fixed_tys expected_fixed_tys then
                let remaining_tys = drop (List.length fixed_args) param_tys in
                let remaining_names =
                  remaining_tys |> List.mapi (fun index _ -> "arg" ^ string_of_int index)
                in
                let remaining_exprs =
                  remaining_names |> List.map (fun name -> Ocaml_ir.Ident name)
                in
                Ok
                  (typed_ir (TFn (remaining_tys, ret))
                     (Ocaml_ir.Fun
                        ( List.map (fun name -> Ocaml_ir.PVar name) remaining_names,
                          Ocaml_ir.Apply
                            ( fn.ocaml_expr,
                              List.map (fun arg -> arg.ocaml_expr) fixed_args
                              @ remaining_exprs ))))
              else Error.error "partial fixed arguments do not match function"
          | TFn _ -> Error.error "partial requires fewer arguments than function arity"
          | _ -> Error.error "partial expects a function"))
  | _ -> Error.error "partial expects a function"

and compile_identity scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ arg ] -> Ok arg
  | Ok _ -> Error.error "identity expects 1 arguments"

and compile_constantly scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ value ] ->
      Ok
        (typed_ir (TFn ([ TAny ], value.ty))
           (Ocaml_ir.Fun ([ Ocaml_ir.PAny ], value.ocaml_expr)))
  | Ok _ -> Error.error "constantly expects 1 arguments"

and compile_complement scope env arg_forms =
  match arg_forms with
  | [ fn_form ] -> (
      match compile_function_arg scope env fn_form with
      | Error _ as err -> err
      | Ok fn -> (
          match fn.ty with
          | TFn ([ arg_ty ], TBool) ->
              Ok
                (typed_ir (TFn ([ arg_ty ], TBool))
                   (Ocaml_ir.Fun
                      ( [ Ocaml_ir.PVar "x" ],
                        Ocaml_ir.Prefix
                          ("not", Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "x" ])) )))
          | TFn _ -> Error.error "complement expects a predicate"
          | _ -> Error.error "complement expects a function"))
  | _ -> Error.error "complement expects 1 function"

and compile_predicate_combinator scope env name arg_forms =
  let compile_fns =
    arg_forms
    |> List.fold_left
         (fun acc form ->
           match acc with
           | Error _ as err -> err
           | Ok fns -> (
               match compile_function_arg scope env form with
               | Error _ as err -> err
               | Ok fn -> Ok (fn :: fns)))
         (Ok [])
    |> Result.map List.rev
  in
  match compile_fns with
  | Error _ as err -> err
  | Ok [] -> Error.error (name ^ " expects at least 1 predicate")
  | Ok fns -> (
      let rec collect arg_ty exprs = function
        | [] -> Ok (arg_ty, List.rev exprs)
        | fn :: rest -> (
            match fn.ty with
            | TFn ([ current_arg ], TBool)
              when option_for_all (fun arg_ty -> Types.equal arg_ty current_arg) arg_ty ->
                collect (Some current_arg)
                  (Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "x" ]) :: exprs)
                  rest
            | TFn _ ->
                Error.error (name ^ " expects predicates with the same argument type")
            | _ -> Error.error (name ^ " expects predicates"))
      in
      match collect None [] fns with
      | Error _ as err -> err
      | Ok (None, _) -> Error.error (name ^ " expects at least 1 predicate")
      | Ok (Some arg_ty, exprs) ->
          let op = if name = "every-pred" then "&&" else "||" in
          let body =
            match exprs with
            | [] -> Ocaml_ir.Bool (name = "every-pred")
            | first :: rest ->
                List.fold_left
                  (fun acc expr -> Ocaml_ir.Infix (op, acc, expr))
                  first rest
          in
          Ok
            (typed_ir (TFn ([ arg_ty ], TBool))
               (Ocaml_ir.Fun ([ Ocaml_ir.PVar "x" ], body))))

and compile_juxt scope env arg_forms =
  let compile_fns =
    arg_forms
    |> List.fold_left
         (fun acc form ->
           match acc with
           | Error _ as err -> err
           | Ok fns -> (
               match compile_function_arg scope env form with
               | Error _ as err -> err
               | Ok fn -> Ok (fn :: fns)))
         (Ok [])
    |> Result.map List.rev
  in
  match compile_fns with
  | Error _ as err -> err
  | Ok [] -> Error.error "juxt expects at least 1 function"
  | Ok fns -> (
      let rec collect arg_ty ret_ty exprs = function
        | [] -> Ok (arg_ty, ret_ty, List.rev exprs)
        | fn :: rest -> (
            match fn.ty with
            | TFn ([ current_arg ], current_ret)
              when option_for_all
                     (fun arg_ty ->
                       Types.compatible ~expected:arg_ty ~actual:current_arg)
                     arg_ty
                   && option_for_all
                        (fun ret_ty ->
                          Types.compatible ~expected:ret_ty ~actual:current_ret)
                        ret_ty ->
                collect (Some current_arg) (Some current_ret)
                  (Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "x" ]) :: exprs)
                  rest
            | TFn ([ current_arg ], _)
              when option_for_all
                     (fun arg_ty ->
                       Types.compatible ~expected:arg_ty ~actual:current_arg)
                     arg_ty ->
                Error.error "juxt functions must return the same type"
            | TFn _ -> Error.error "juxt functions must accept the same argument type"
            | _ -> Error.error "juxt expects functions")
      in
      match collect None None [] fns with
      | Error _ as err -> err
      | Ok (Some arg_ty, Some ret_ty, exprs) ->
          Ok
            (typed_ir (TFn ([ arg_ty ], TVector ret_ty))
               (Ocaml_ir.Fun
                  ( [ Ocaml_ir.PVar "x" ],
                    apply "Rrbvec.of_list" [ Ocaml_ir.List exprs ] )))
      | Ok _ -> Error.error "juxt expects at least 1 function")

and compile_distinct_question scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok ([] | [ _ ]) -> Ok (typed_ir TBool (Ocaml_ir.Bool true))
  | Ok (first :: _ as args) ->
      if List.for_all (fun arg -> Types.equal first.ty arg.ty) args then
        Ok
          (typed_ir TBool
             (Ocaml_ir.Infix
                ( "=",
                  apply "List.length"
                    [ apply "List.sort_uniq"
                        [ Ocaml_ir.Ident "compare";
                          Ocaml_ir.List (List.map (fun arg -> arg.ocaml_expr) args) ] ],
                  Ocaml_ir.Int (List.length args) )))
      else Error.error "distinct? arguments must have the same type"

and compile_compare scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ left; right ] ->
      if not (Types.equal left.ty right.ty) then
        Error.error "compare arguments must have the same type"
      else if not (comparable_type left.ty) then
        Error.error "compare expects comparable arguments"
      else
        Ok
          (typed_ir TInt
             (apply "Stdlib.compare" [ left.ocaml_expr; right.ocaml_expr ]))
  | Ok _ -> Error.error "compare expects 2 arguments"

and compile_key_extreme scope env name arg_forms =
  match arg_forms with
  | fn_form :: value_forms when value_forms <> [] -> (
      match (compile_function_arg scope env fn_form, compile_args_for scope env value_forms) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok values -> (
          let first = List.hd values in
          if not (List.for_all (fun value -> Types.equal first.ty value.ty) values) then
            Error.error (name ^ " values must have the same type")
          else
            match fn.ty with
            | TFn ([ arg_ty ], key_ty)
              when Types.compatible ~expected:arg_ty ~actual:first.ty
                   && comparable_type key_ty ->
                let rest = List.tl values in
                let compare_op = if name = "max-key" then ">" else "<" in
                let expr =
                  match rest with
                  | [] -> first.ocaml_expr
                  | _ ->
                      Ocaml_ir.Let
                        ( [ (Ocaml_ir.PVar "key_fn", fn.ocaml_expr);
                            ( Ocaml_ir.PVar "choose",
                              Ocaml_ir.Fun
                                ( [ Ocaml_ir.PVar "best"; Ocaml_ir.PVar "item" ],
                                  Ocaml_ir.If
                                    ( Ocaml_ir.Infix
                                        ( compare_op,
                                          apply "Stdlib.compare"
                                            [ Ocaml_ir.Apply
                                                ( Ocaml_ir.Ident "key_fn",
                                                  [ Ocaml_ir.Ident "item" ] );
                                              Ocaml_ir.Apply
                                                ( Ocaml_ir.Ident "key_fn",
                                                  [ Ocaml_ir.Ident "best" ] ) ],
                                          Ocaml_ir.Int 0 ),
                                      Ocaml_ir.Ident "item",
                                      Ocaml_ir.Ident "best" ) ) ) ],
                          apply "List.fold_left"
                            [ Ocaml_ir.Ident "choose";
                              first.ocaml_expr;
                              Ocaml_ir.List (List.map (fun value -> value.ocaml_expr) rest) ] )
                in
                Ok (typed_ir first.ty expr)
            | TFn _ -> Error.error (name ^ " expects a key function matching values")
            | _ -> Error.error (name ^ " expects a function")))
  | _ -> Error.error (name ^ " expects function and values")

and compile_hash_set scope env arg_forms =
  match arg_forms with
  | [] -> Error.error "empty hash-set requires a type annotation"
  | first :: rest -> (
      match compile_expr scope env first with
      | Error _ as err -> err
      | Ok first_expr ->
          let rec loop values = function
            | [] ->
                Result.bind (Types.set_module_name first_expr.ty) (fun set_module ->
                       let rec coerce_values acc = function
                         | [] -> Ok (List.rev acc)
                         | value :: rest ->
                             Result.bind (coerce_set_element first_expr.ty value)
                               (fun value -> coerce_values (value :: acc) rest)
                       in
                       coerce_values [] (List.rev values)
                       |> Result.map (fun values ->
                              typed_ir (TSet first_expr.ty)
                                (Ocaml_ir.Apply
                                   ( Ocaml_ir.Ident (set_module ^ ".of_list"),
                                     [ Ocaml_ir.List values ] ))))
            | form :: rest -> (
                match compile_expr scope env form with
                | Error _ as err -> err
                | Ok expr ->
                    if Types.same_shape first_expr.ty expr.ty then
                      loop (expr :: values) rest
                    else Error.error "hash-set elements must all have the same type")
          in
          loop [ first_expr ] rest)

and compile_set_of arg_forms =
  match arg_forms with
  | [ FKeyword keyword ] -> (
      match Type_annotation.of_keyword keyword with
      | Error _ -> Error.error ("unknown set element type " ^ keyword)
      | Ok element_ty ->
          Types.set_module_name element_ty
          |> Result.map (fun set_module ->
                 typed_ir (TSet element_ty) (Ocaml_ir.Ident (set_module ^ ".empty"))))
  | _ -> Error.error "set-of expects one type keyword"

and compile_disj scope env arg_forms =
  match arg_forms with
  | collection_form :: value_forms -> (
      match compile_expr scope env collection_form with
      | Error _ as err -> err
      | Ok collection -> (
          match collection.ty with
          | TSet inner ->
              let rec remove_values expression = function
                | [] -> Ok (typed_ir collection.ty expression)
                | value_form :: rest -> (
                    match compile_expr scope env value_form with
                    | Error _ as err -> err
                    | Ok value ->
                        if Types.same_shape inner value.ty then
                          Result.bind (Types.set_module_name inner)
                            (fun set_module ->
                              Result.bind (coerce_set_element inner value) (fun value ->
                                     remove_values
                                       (Ocaml_ir.Apply
                                          ( Ocaml_ir.Ident (set_module ^ ".remove"),
                                            [ value; expression ] ))
                                       rest))
                        else Error.error "disj value type must match set element type")
              in
              remove_values collection.ocaml_expr value_forms
          | _ -> Error.error "disj expects a set"))
  | [] -> Error.error "disj expects a set"

and compile_args_for scope env arg_forms =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | form :: rest -> (
        match compile_expr scope env form with
        | Ok expr -> loop (expr :: acc) rest
        | Error _ as err -> err)
  in
  loop [] arg_forms

let compile_defprotocol scope env next_type protocol_name method_forms =
  match Protocol.defprotocol_bindings scope protocol_name method_forms with
  | Error _ as err -> err
  | Ok bindings ->
      let env =
        List.fold_left
          (fun env (key, binding) ->
            if Protocol.is_legacy_marker key binding && Env.mem key env then
              Env.add key (Protocol.ambiguous_marker_binding ()) env
            else Env.add key binding env)
          env bindings
      in
      Ok
        ( scope,
          env,
          next_type,
          Comment ("protocol " ^ protocol_name) )

let protocol_receiver_type scope env = function
  | FKeyword receiver_keyword -> Type_annotation.of_keyword receiver_keyword
  | FSymbol type_name ->
      lookup_record_type scope env type_name
      |> Result.map (fun record -> TNamed_record record)
  | _ -> Error.error "extend-type receiver must be a type keyword or record type"

let compile_extend_type scope env next_type receiver_form protocol_name method_forms =
  match protocol_receiver_type scope env receiver_form with
  | Error _ as err -> err
  | Ok receiver_ty ->
      let compile_method env = function
        | FList (FSymbol method_name :: params :: body_forms) -> (
            match
              Protocol.lookup_protocol_marker scope env protocol_name method_name
            with
            | None ->
                Error.error
                  ("protocol " ^ protocol_name ^ " does not define method " ^ method_name)
            | Some marker
              when marker.ocaml_name <> Protocol.protocol_id scope protocol_name ->
                Error.error
                  ("protocol " ^ protocol_name ^ " does not define method " ^ method_name)
            | Some marker -> (
                match Protocol.annotate_receiver receiver_ty params with
                | Error _ as err -> err
                | Ok params -> (
                    let param_type_overrides =
                      match receiver_ty with
                      | TNamed_record _ -> [ Some receiver_ty ]
                      | _ -> []
                    in
                    match
                      compile_fn ~param_type_overrides scope env params body_forms
                    with
                    | Error _ as err -> err
                    | Ok expr -> (
                        match (marker.ty, expr.ty) with
                        | TFn (expected_params, _), TFn (actual_params, _)
                          when List.length expected_params <> List.length actual_params ->
                            Error.error (method_name ^ " called with incompatible arguments")
                        | TFn (expected_params, expected_ret),
                          TFn (actual_params, actual_ret)
                          -> (
                            match actual_params with
                            | [] ->
                                Error.error
                                  "protocol methods must have a receiver parameter"
                            | actual_receiver :: _ ->
                                if not (Types.equal receiver_ty actual_receiver) then
                                  Error.error
                                    ("protocol implementation receiver must be "
                                   ^ source_name receiver_ty)
                                else
                                  let mismatch =
                                    List.combine expected_params actual_params
                                    |> List.mapi (fun index (expected, actual) ->
                                           (index, expected, actual))
                                    |> List.find_opt
                                         (fun (_index, expected, actual) ->
                                           not
                                             (Types.compatible ~expected
                                                ~actual))
                                  in
                                  (match mismatch with
                                  | Some (index, expected, _actual) ->
                                      Error.error
                                        ("protocol method " ^ method_name ^ " parameter "
                                       ^ string_of_int (index + 1) ^ " must be "
                                       ^ source_name expected)
                                  | None
                                    when not
                                           (Types.compatible ~expected:expected_ret
                                              ~actual:actual_ret) ->
                                  Error.error
                                    ("protocol method " ^ method_name ^ " must return "
                                   ^ source_name expected_ret)
                                  | None -> (
                                  match
                                    Protocol.impl_name marker.ocaml_name method_name
                                      receiver_ty
                                  with
                                  | None ->
                                      Error.error
                                        ("protocol implementations do not support receiver type "
                                       ^ source_name receiver_ty)
                                  | Some impl_key_name ->
                                      let ocaml_name =
                                        Protocol.impl_ocaml_name scope
                                          protocol_name method_name receiver_ty
                                      in
                                      let env_key =
                                        Names.scoped_key scope impl_key_name
                                      in
                                      let binding = binding_of_expr ocaml_name expr in
                                      Ok
                                        ( Env.add env_key binding env,
                                          Value_binding
                                            {
                                              pattern = Named ocaml_name;
                                              expression = expr.ocaml_expr;
                                            } ))))
                        | _ -> Error.error "protocol method did not compile to a function"))))
        | _ -> Error.error "extend-type methods must be (method-name [params] body)"
      in
      let rec loop env items = function
        | [] ->
            Ok
              ( scope,
                env,
                next_type,
                Group (List.rev items) )
        | method_form :: rest -> (
            match compile_method env method_form with
            | Error _ as err -> err
            | Ok (env, item) -> loop env (item :: items) rest)
      in
      loop env [] method_forms

let module_binding_key module_path name = module_path ^ "/" ^ name

let module_binding_ocaml_name module_path name =
  Names.module_path_to_ocaml module_path ^ "." ^ Names.sanitize_name name

let protocol_marker_key key = String.ends_with ~suffix:"$protocol" key

let changed_bindings previous updated =
  Env.to_bindings updated
  |> List.filter
    (fun (key, binding) ->
      match Env.find_opt key previous with
      | None -> true
      | Some previous_binding -> previous_binding <> binding)

let open_module_bindings scope env module_path =
  let prefix = module_path ^ "/" in
  let prefix_len = String.length prefix in
  let record_prefix = "__record/" ^ module_path ^ "/" in
  let record_prefix_len = String.length record_prefix in
  let opened =
    env
    |> Env.filter_map (fun key (binding : binding) ->
           if String.length key > prefix_len && String.sub key 0 prefix_len = prefix then
             let local = String.sub key prefix_len (String.length key - prefix_len) in
             let opened_binding =
               if protocol_marker_key key then binding
               else { binding with ocaml_name = Names.sanitize_name local }
             in
             Some (Names.scoped_key scope local, opened_binding)
           else if
             String.length key > record_prefix_len
             && String.sub key 0 record_prefix_len = record_prefix
           then
             let local =
               String.sub key record_prefix_len
                 (String.length key - record_prefix_len)
             in
             Some (record_type_key scope local, binding)
           else None)
  in
  Env.add_bindings opened env

let include_module_public_bindings module_path env included_module_path =
  let prefix = included_module_path ^ "/" in
  let prefix_len = String.length prefix in
  let record_prefix = "__record/" ^ included_module_path ^ "/" in
  let record_prefix_len = String.length record_prefix in
  env
  |> Env.filter_map (fun key (binding : binding) ->
         if String.length key > prefix_len && String.sub key 0 prefix_len = prefix then
           let name = String.sub key prefix_len (String.length key - prefix_len) in
           Some
             ( module_binding_key module_path name,
               if protocol_marker_key key then binding
               else
                 {
                   binding with
                   ocaml_name = module_binding_ocaml_name module_path name;
                 }
             )
         else if
           String.length key > record_prefix_len
           && String.sub key 0 record_prefix_len = record_prefix
         then
           let name =
             String.sub key record_prefix_len
               (String.length key - record_prefix_len)
           in
           Some (record_type_key module_path name, binding)
         else None)

let alias_module_bindings env alias_path target_path =
  let direct_prefix = target_path ^ "/" in
  let direct_prefix_len = String.length direct_prefix in
  let nested_prefix = target_path ^ "." in
  let nested_prefix_len = String.length nested_prefix in
  let direct_record_prefix = "__record/" ^ target_path ^ "/" in
  let direct_record_prefix_len = String.length direct_record_prefix in
  let nested_record_prefix = "__record/" ^ target_path ^ "." in
  let nested_record_prefix_len = String.length nested_record_prefix in
  env
  |> Env.filter_map (fun key (binding : binding) ->
         if
           String.length key > direct_prefix_len
           && String.sub key 0 direct_prefix_len = direct_prefix
         then
           let name =
             String.sub key direct_prefix_len
               (String.length key - direct_prefix_len)
           in
           let alias_key = module_binding_key alias_path name in
           let alias_binding =
             if protocol_marker_key key then binding
             else { binding with ocaml_name = module_binding_ocaml_name alias_path name }
           in
           Some (alias_key, alias_binding)
        else if
          String.length key > nested_prefix_len
          && String.sub key 0 nested_prefix_len = nested_prefix
         then
           let suffix =
             String.sub key nested_prefix_len
               (String.length key - nested_prefix_len)
           in
           match String.split_on_char '/' suffix with
           | [ nested_path; name ] ->
               let nested_alias_path = alias_path ^ "." ^ nested_path in
               let alias_key = module_binding_key nested_alias_path name in
               let alias_binding =
                 {
                   binding with
                   ocaml_name = module_binding_ocaml_name nested_alias_path name;
                 }
               in
               Some (alias_key, alias_binding)
           | _ -> None
        else if
          String.length key > direct_record_prefix_len
          && String.sub key 0 direct_record_prefix_len = direct_record_prefix
         then
           let name =
             String.sub key direct_record_prefix_len
               (String.length key - direct_record_prefix_len)
           in
           Some (record_type_key alias_path name, binding)
        else if
          String.length key > nested_record_prefix_len
          && String.sub key 0 nested_record_prefix_len = nested_record_prefix
         then
           let suffix =
             String.sub key nested_record_prefix_len
               (String.length key - nested_record_prefix_len)
           in
           match String.split_on_char '/' suffix with
           | [ nested_path; name ] ->
               Some (record_type_key (alias_path ^ "." ^ nested_path) name, binding)
           | _ -> None
         else None)

let signature_binding_key signature_name value_name =
  "__signature/" ^ signature_name ^ "/" ^ value_name

let functor_result_key functor_name value_name =
  "__functor/" ^ functor_name ^ "/" ^ value_name

let functor_result_record_key functor_name type_name =
  "__functor_record/" ^ functor_name ^ "/" ^ type_name

let signature_metadata_bindings env signature_name items =
  let nested_value_path module_name value_path =
    match String.rindex_opt value_path '/' with
    | None -> module_name ^ "/" ^ value_path
    | Some separator ->
        let nested_path = String.sub value_path 0 separator in
        let value_name =
          String.sub value_path (separator + 1)
            (String.length value_path - separator - 1)
        in
        module_name ^ "." ^ nested_path ^ "/" ^ value_name
  in
  let item_bindings = function
    | Signature_value { source_name; value_name; value_type } ->
        [
          ( signature_binding_key signature_name source_name,
            Types.binding value_name value_type );
        ]
    | Signature_type _ -> []
    | Signature_module { source_name; module_signature; _ } ->
        let nested_prefix = "__signature/" ^ module_signature ^ "/" in
        let nested_prefix_len = String.length nested_prefix in
        env
        |> Env.filter_map (fun key binding ->
               if
                 String.length key > nested_prefix_len
                 && String.sub key 0 nested_prefix_len = nested_prefix
               then
                 let nested_name =
                   String.sub key nested_prefix_len
                     (String.length key - nested_prefix_len)
                 in
                 Some
                   ( signature_binding_key signature_name
                       (nested_value_path source_name nested_name),
                     binding )
               else None)
    | Signature_include { module_signature } ->
        let included_prefix = "__signature/" ^ module_signature ^ "/" in
        let included_prefix_len = String.length included_prefix in
        env
        |> Env.filter_map (fun key binding ->
               if
                 String.length key > included_prefix_len
                 && String.sub key 0 included_prefix_len = included_prefix
               then
                 let value_path =
                   String.sub key included_prefix_len
                     (String.length key - included_prefix_len)
                 in
                 Some (signature_binding_key signature_name value_path, binding)
               else None)
  in
  List.concat_map item_bindings items

let parameter_value_binding parameter_name value_path (binding : binding) =
  match String.rindex_opt value_path '/' with
  | None ->
      ( module_binding_key parameter_name value_path,
        {
          binding with
          ocaml_name =
            Names.module_segment_to_ocaml parameter_name ^ "." ^ binding.ocaml_name;
        } )
  | Some separator ->
      let nested_path = String.sub value_path 0 separator in
      let value_name =
        String.sub value_path (separator + 1)
          (String.length value_path - separator - 1)
      in
      let parameter_path = parameter_name ^ "." ^ nested_path in
      ( module_binding_key parameter_path value_name,
        {
          binding with
          ocaml_name =
            Names.module_path_to_ocaml parameter_path ^ "." ^ binding.ocaml_name;
        } )

let signature_parameter_bindings env parameter_name signature_name =
  let prefix = "__signature/" ^ signature_name ^ "/" in
  let prefix_len = String.length prefix in
  env
  |> Env.filter_map (fun key (binding : binding) ->
         if String.length key > prefix_len && String.sub key 0 prefix_len = prefix then
           let value_name =
             String.sub key prefix_len (String.length key - prefix_len)
           in
           Some (parameter_value_binding parameter_name value_name binding)
         else None)

let store_functor_result_bindings functor_name public_bindings =
  let prefix = functor_name ^ "/" in
  let prefix_len = String.length prefix in
  let record_prefix = "__record/" ^ functor_name ^ "/" in
  let record_prefix_len = String.length record_prefix in
  public_bindings
  |> List.filter_map (fun (key, (binding : binding)) ->
         if String.length key > prefix_len && String.sub key 0 prefix_len = prefix then
           let value_name =
             String.sub key prefix_len (String.length key - prefix_len)
           in
           Some (functor_result_key functor_name value_name, binding)
         else if
           String.length key > record_prefix_len
           && String.sub key 0 record_prefix_len = record_prefix
         then
           let type_name =
             String.sub key record_prefix_len
               (String.length key - record_prefix_len)
           in
           Some (functor_result_record_key functor_name type_name, binding)
         else None)

let apply_functor_result_bindings env module_name functor_name =
  let prefix = "__functor/" ^ functor_name ^ "/" in
  let prefix_len = String.length prefix in
  let record_prefix = "__functor_record/" ^ functor_name ^ "/" in
  let record_prefix_len = String.length record_prefix in
  env
  |> Env.filter_map (fun key (binding : binding) ->
         if String.length key > prefix_len && String.sub key 0 prefix_len = prefix then
           let value_name =
             String.sub key prefix_len (String.length key - prefix_len)
           in
           Some
             ( module_binding_key module_name value_name,
               {
                 binding with
                 ocaml_name = module_binding_ocaml_name module_name value_name;
               } )
         else if
           String.length key > record_prefix_len
           && String.sub key 0 record_prefix_len = record_prefix
         then
           let type_name =
             String.sub key record_prefix_len
               (String.length key - record_prefix_len)
           in
           Some (record_type_key module_name type_name, binding)
         else None)

let compile_module_alias scope env next_type alias_name target_name =
  let alias_bindings = alias_module_bindings env alias_name target_name in
  Ok
    ( scope,
      Env.add_bindings alias_bindings env,
      next_type,
      Module_alias
        {
          alias_name = Names.module_segment_to_ocaml alias_name;
          target_name = Names.module_path_to_ocaml target_name;
        } )

let parse_type_parameters = function
  | FVector [] -> Error.error "type parameter vector must not be empty"
  | FVector forms ->
      let rec loop parameters = function
        | [] -> Ok (List.rev parameters)
        | FSymbol parameter :: rest ->
            let parameter = Names.sanitize_name parameter in
            if List.mem parameter parameters then
              Error.error ("duplicate type parameter " ^ parameter)
            else loop (parameter :: parameters) rest
        | _ -> Error.error "type parameters must be symbols"
      in
      loop [] forms
  | _ -> Error.error "type parameters must be a vector"

let compile_module_signature scope env next_type signature_name item_forms =
  let rec parse items = function
    | [] -> Ok (List.rev items)
    | FList [ FSymbol "val"; FSymbol value_name; FKeyword keyword ] :: rest -> (
        match Type_annotation.of_keyword keyword with
        | Error _ -> Error.error ("unknown signature type " ^ keyword)
        | Ok value_type ->
            parse
              (Signature_value
                 {
                   source_name = value_name;
                   value_name = Names.sanitize_name value_name;
                   value_type;
                 }
              :: items)
              rest)
    | FList [ FSymbol "type"; FSymbol type_name; FKeyword keyword ] :: rest -> (
        match Type_annotation.of_keyword keyword with
        | Error _ -> Error.error ("unknown signature type " ^ keyword)
        | Ok manifest ->
            parse
              (Signature_type
                 {
                   type_name = Names.sanitize_name type_name;
                   type_parameters = [];
                   manifest = Some manifest;
                 }
              :: items)
              rest)
    | FList [ FSymbol "type"; FSymbol type_name ] :: rest ->
        parse
          (Signature_type
             {
               type_name = Names.sanitize_name type_name;
               type_parameters = [];
               manifest = None;
             }
          :: items)
          rest
    | FList
        [ FSymbol "module"; FSymbol module_name; FSymbol module_signature ]
      :: rest ->
        parse
          (Signature_module
             {
               source_name = module_name;
               module_name = Names.module_segment_to_ocaml module_name;
               module_signature = Names.module_path_to_ocaml module_signature;
             }
          :: items)
          rest
    | FList [ FSymbol "include"; FSymbol module_signature ] :: rest ->
        parse
          (Signature_include
             { module_signature = Names.module_path_to_ocaml module_signature }
          :: items)
          rest
    | FList (FSymbol "include" :: _) :: _ ->
        Error.error "module-signature include expects one module type"
    | FList
        [ FSymbol "type"; FSymbol type_name; (FVector _ as parameter_form);
          FKeyword keyword ]
      :: rest -> (
        match parse_type_parameters parameter_form with
        | Error _ as err -> err
        | Ok type_parameters -> (
            match
              Type_annotation.of_keyword_with_parameters type_parameters keyword
            with
            | Error (err : Error.t)
              when String.starts_with ~prefix:"unknown type parameter " err.message ->
                Error err
            | Error _ -> Error.error ("unknown signature type " ^ keyword)
            | Ok manifest ->
                parse
                  (Signature_type
                     {
                       type_name = Names.sanitize_name type_name;
                       type_parameters;
                       manifest = Some manifest;
                     }
                  :: items)
                  rest))
    | FList
        [ FSymbol "type"; FSymbol type_name; (FVector _ as parameter_form) ]
      :: rest -> (
        match parse_type_parameters parameter_form with
        | Error _ as err -> err
        | Ok type_parameters ->
            parse
              (Signature_type
                 {
                   type_name = Names.sanitize_name type_name;
                   type_parameters;
                   manifest = None;
                 }
              :: items)
          rest
        )
    | _ ->
        Error.error
          "module-signature items must be val, type, module, or include declarations"
  in
  match parse [] item_forms with
  | Error _ as err -> err
  | Ok [] -> Error.error "module-signature expects at least one signature item"
  | Ok items ->
      let signature_name = Names.module_segment_to_ocaml signature_name in
      let env = Env.add_bindings (signature_metadata_bindings env signature_name items) env in
      Ok
        ( scope,
          env,
          next_type,
          Module_signature
            { signature_name; items } )

let compile_type_alias scope env next_type name type_parameters manifest_form =
  match manifest_form with
  | FKeyword keyword -> (
      match Type_annotation.of_keyword_with_parameters type_parameters keyword with
      | Error _ as err when String.starts_with ~prefix:":param/" keyword -> err
      | Error _ -> Error.error ("unknown type alias target " ^ keyword)
      | Ok manifest ->
          let type_name = Names.sanitize_name name in
          Ok
            ( scope,
              env,
              next_type,
              Type_alias { type_name; type_parameters; manifest } ))
  | _ -> Error.error "type-alias expects a type keyword target"

let compile_type_record scope env next_type name type_parameters field_forms =
  let field_spec = function
    | FList [ FSymbol field_name; FKeyword keyword ] -> (
        match Type_annotation.of_keyword_with_parameters type_parameters keyword with
        | Error _ as err when String.starts_with ~prefix:":param/" keyword -> err
        | Error _ -> Error.error ("unknown record field type " ^ keyword)
        | Ok ty ->
            Ok
              {
                keyword = ":" ^ field_name;
                ocaml_name = Names.sanitize_name field_name;
                ty;
              })
    | _ -> Error.error "type-record fields must be (name :type)"
  in
  let rec parse (fields : field list) = function
    | [] -> Ok (List.rev fields)
    | field_form :: rest -> (
        match field_spec field_form with
        | Error _ as err -> err
        | Ok field ->
            if
              List.exists
                (fun (existing : field) -> existing.ocaml_name = field.ocaml_name)
                fields
            then Error.error "duplicate record field name"
            else parse (field :: fields) rest)
  in
  match parse [] field_forms with
  | Error _ as err -> err
  | Ok [] -> Error.error "type-record expects at least one field"
  | Ok fields ->
      let type_name = Names.sanitize_name name in
      let record_ty =
        Types.named_record ~nominal:true ~type_name ~type_parameters
          ~set_module_name:(type_name ^ "_set") fields
      in
      let env =
        Env.add (record_type_key scope name) (Types.binding type_name record_ty) env
      in
      Ok
        ( scope,
          env,
          next_type,
          Type_def { type_name; type_parameters; fields } )

let record_type_public_binding module_path name env =
  let key = record_type_key module_path name in
  match Env.find_opt key env with
  | Some binding -> Ok (key, binding)
  | None -> Error.error ("internal error: missing record metadata for " ^ name)

let compile_type_variant scope env next_type name type_parameters constructor_forms =
  let constructor_name = function
    | FSymbol constructor -> Ok constructor
    | _ -> Error.error "type-variant constructors must be symbols"
  in
  let payload_type = function
    | FKeyword keyword -> (
        match Type_annotation.of_keyword_with_parameters type_parameters keyword with
        | Ok ty -> Ok ty
        | Error _ as err when String.starts_with ~prefix:":param/" keyword -> err
        | Error _ -> Error.error ("unknown variant payload type " ^ keyword))
    | _ -> Error.error "type-variant payload types must be keywords"
  in
  let constructor_spec = function
    | FSymbol constructor ->
        Ok { constructor_name = constructor; payload_types = [] }
    | FList (constructor_form :: payload_forms) -> (
        match constructor_name constructor_form with
        | Error _ as err -> err
        | Ok constructor_name ->
            let rec parse_payloads acc = function
              | [] -> Ok (List.rev acc)
              | payload_form :: rest -> (
                  match payload_type payload_form with
                  | Error _ as err -> err
                  | Ok payload_ty -> parse_payloads (payload_ty :: acc) rest)
            in
            parse_payloads [] payload_forms
            |> Result.map (fun payload_types -> { constructor_name; payload_types }))
    | _ -> Error.error "type-variant constructors must be symbols"
  in
  let rec parse constructors = function
    | [] -> Ok (List.rev constructors)
    | constructor_form :: rest -> (
        match constructor_spec constructor_form with
        | Error _ as err -> err
        | Ok constructor ->
            if
              List.exists
                (fun existing ->
                  existing.constructor_name = constructor.constructor_name)
                constructors
            then Error.error ("duplicate variant constructor " ^ constructor.constructor_name)
            else parse (constructor :: constructors) rest)
  in
  match parse [] constructor_forms with
  | Error _ as err -> err
  | Ok [] -> Error.error "type-variant expects at least one constructor"
  | Ok constructors ->
      let type_name = Names.sanitize_name name in
      let constructor_bindings =
        let result_type =
          match type_parameters with
          | [] -> TOcaml type_name
          | parameters -> TOcaml_app (type_name, List.map (fun name -> TVar name) parameters)
        in
        constructors
        |> List.map (fun constructor ->
               ( Names.scoped_key scope constructor.constructor_name,
                 Types.binding constructor.constructor_name
                   (TFn (constructor.payload_types, result_type)) ))
      in
      Ok
        ( scope,
          Env.add_bindings constructor_bindings env,
          next_type,
          Type_variant { type_name; type_parameters; constructors } )

let compile_module_apply scope env next_type module_name functor_name
    argument_names =
  let applied_bindings =
    apply_functor_result_bindings env module_name functor_name
  in
  Ok
    ( scope,
      Env.add_bindings applied_bindings env,
      next_type,
      Module_apply
        {
          module_name = Names.module_segment_to_ocaml module_name;
          functor_name = Names.module_path_to_ocaml functor_name;
          argument_names = List.map Names.module_path_to_ocaml argument_names;
        } )

let rec compile_module ?signature_name scope env next_type module_path
    module_segment forms =
  let env = inherit_scope_ocaml_value_refers scope module_path env in
  let rec compile_module_form env public_bindings next_type items = function
    | FList (FSymbol "module-signature" :: FSymbol signature_name :: item_forms) -> (
        match compile_module_signature module_path env next_type signature_name item_forms with
        | Error _ as err -> err
        | Ok (_scope, env, next_type, item) ->
            Ok (env, public_bindings, next_type, item :: items))
    | FList (FSymbol "module-signature" :: _) ->
        Error.error "module-signature expects a name and signature items"
    | FList [ FSymbol "type-alias"; FSymbol name; FVector parameter_forms; manifest_form ] -> (
        match parse_type_parameters (FVector parameter_forms) with
        | Error _ as err -> err
        | Ok type_parameters -> (
            match
              compile_type_alias module_path env next_type name type_parameters
                manifest_form
            with
            | Error _ as err -> err
            | Ok (_scope, _env, next_type, item) ->
                Ok (env, public_bindings, next_type, item :: items)))
    | FList [ FSymbol "type-alias"; FSymbol name; manifest_form ] -> (
        match compile_type_alias module_path env next_type name [] manifest_form with
        | Error _ as err -> err
        | Ok (_scope, _env, next_type, item) ->
            Ok (env, public_bindings, next_type, item :: items))
    | FList
        (FSymbol "type-record" :: FSymbol name :: FVector parameter_forms
        :: field_forms) -> (
        match parse_type_parameters (FVector parameter_forms) with
        | Error _ as err -> err
        | Ok type_parameters -> (
            match
              compile_type_record module_path env next_type name type_parameters
                field_forms
            with
            | Error _ as err -> err
            | Ok (_scope, env, next_type, item) -> (
                match record_type_public_binding module_path name env with
                | Error _ as err -> err
                | Ok public_binding ->
                    Ok
                      ( env,
                        public_bindings @ [ public_binding ],
                        next_type,
                        item :: items ))))
    | FList (FSymbol "type-record" :: FSymbol name :: field_forms) -> (
        match compile_type_record module_path env next_type name [] field_forms with
        | Error _ as err -> err
        | Ok (_scope, env, next_type, item) -> (
            match record_type_public_binding module_path name env with
            | Error _ as err -> err
            | Ok public_binding ->
                Ok
                  ( env,
                    public_bindings @ [ public_binding ],
                    next_type,
                    item :: items )))
    | FList (FSymbol "type-record" :: _) ->
        Error.error "type-record expects a name and fields"
    | FList
        (FSymbol "type-variant" :: FSymbol name :: FVector parameter_forms
        :: constructor_forms) -> (
        match parse_type_parameters (FVector parameter_forms) with
        | Error _ as err -> err
        | Ok type_parameters -> (
            match
              compile_type_variant module_path env next_type name type_parameters
                constructor_forms
            with
            | Error _ as err -> err
            | Ok (_scope, _env, next_type, item) ->
                Ok (env, public_bindings, next_type, item :: items)))
    | FList (FSymbol "type-variant" :: FSymbol name :: constructor_forms) -> (
        match compile_type_variant module_path env next_type name [] constructor_forms with
        | Error _ as err -> err
        | Ok (_scope, _env, next_type, item) ->
            Ok (env, public_bindings, next_type, item :: items))
    | FList [ FSymbol "open"; FSymbol opened_module ] ->
        let env = open_module_bindings module_path env opened_module in
        Ok
          ( env,
            public_bindings,
            next_type,
            Open_module (Names.module_path_to_ocaml opened_module) :: items )
    | FList [ FSymbol "include"; FSymbol included_module ] ->
        let included_public_bindings =
          include_module_public_bindings module_path env included_module
        in
        let env = open_module_bindings module_path env included_module in
        Ok
          ( env,
            public_bindings @ included_public_bindings,
            next_type,
            Include_module (Names.module_path_to_ocaml included_module) :: items )
    | FList (FSymbol "include" :: _) ->
        Error.error "include expects one module"
    | FList [ FSymbol "module-alias"; FSymbol alias_name; FSymbol target_name ] ->
        let local_alias_bindings =
          alias_module_bindings env alias_name target_name
        in
        let public_alias_path = module_path ^ "." ^ alias_name in
        let public_alias_bindings =
          alias_module_bindings env public_alias_path target_name
        in
        Ok
          ( Env.add_bindings local_alias_bindings env,
            public_bindings @ public_alias_bindings,
            next_type,
            Module_alias
              {
                alias_name = Names.module_segment_to_ocaml alias_name;
                target_name = Names.module_path_to_ocaml target_name;
              }
            :: items )
    | FList (FSymbol "module-alias" :: _) ->
        Error.error "module-alias expects alias and target modules"
    | FList (FSymbol "defprotocol" :: FSymbol protocol_name :: method_forms) -> (
        match compile_defprotocol module_path env next_type protocol_name method_forms with
        | Error _ as err -> err
        | Ok (_scope, updated_env, next_type, item) ->
            let exported = changed_bindings env updated_env in
            Ok
              ( updated_env,
                public_bindings @ exported,
                next_type,
                item :: items ))
    | FList
        (FSymbol "extend-type" :: receiver_form :: FSymbol protocol_name
        :: method_forms) -> (
        match
          compile_extend_type module_path env next_type receiver_form protocol_name
            method_forms
        with
        | Error _ as err -> err
        | Ok (_scope, updated_env, next_type, item) ->
            let exported =
              changed_bindings env updated_env
              |> List.map (fun (key, (binding : binding)) ->
                     let qualified_ty =
                       Types.qualify_module_type
                         (Names.module_path_to_ocaml module_path)
                         binding.ty
                     in
                     let key =
                       match (String.rindex_opt key '/', qualified_ty) with
                       | Some separator, TFn (TNamed_record record :: _, _) ->
                           String.sub key 0 (separator + 1) ^ record.type_name
                       | _ -> key
                     in
                     ( key,
                       {
                         binding with
                         ocaml_name =
                           Names.module_path_to_ocaml module_path ^ "."
                           ^ binding.ocaml_name;
                         ty = qualified_ty;
                       } ))
            in
            Ok
              ( updated_env,
                public_bindings @ exported,
                next_type,
                item :: items ))
    | FList [ FSymbol "def"; FSymbol name; expr_form ] -> (
        match compile_expr module_path env expr_form with
        | Error _ as err -> err
        | Ok expr ->
            let local_name = Names.sanitize_name name in
            let key = module_binding_key module_path name in
            let local_binding = binding_of_expr local_name expr in
            let public_binding =
              Types.binding
                ?return_param_index:(expr.return_param_index)
                (module_binding_ocaml_name module_path name)
                (Types.qualify_module_type
                   (Names.module_path_to_ocaml module_path)
                   expr.ty)
            in
            (match check_emitted_name_collision env ~source_key:key ~ocaml_name:local_name with
            | Error _ as err -> err
            | Ok () -> (match expr.ty with
            | TRecord fields -> (
                match expr.record_values with
                | None -> Error.error "internal error: record expression missing values"
                | Some values ->
                    let type_name = "t" ^ string_of_int next_type in
                    let set_module_name = "Set_" ^ type_name in
                    let local_record_ty =
                      Types.named_record ~type_name ~set_module_name fields
                    in
                    let public_record_ty =
                      Types.named_record
                        ~type_name:(Names.module_path_to_ocaml module_path ^ "." ^ type_name)
                        ~set_module_name:
                          (Names.module_path_to_ocaml module_path ^ "." ^ set_module_name)
                        fields
                    in
                    let local_binding = Types.binding local_name local_record_ty in
                    let public_binding =
                      Types.binding (module_binding_ocaml_name module_path name)
                        public_record_ty
                    in
                    let item =
                      Record_def
                        { var_name = local_name;
                          type_name;
                          set_module_name;
                          fields;
                          values }
                    in
                    Ok
                      ( Env.add key local_binding env,
                        public_bindings @ [ (key, public_binding) ],
                        next_type + 1,
                        item :: items ))
            | _ ->
                let item =
                  Value_binding
                    { pattern = Named local_name; expression = expr.ocaml_expr }
                in
                Ok
                  ( Env.add key local_binding env,
                    public_bindings @ [ (key, public_binding) ],
                    next_type,
                    item :: items ))))
    | FList (FSymbol "defn" :: FSymbol name :: params :: body_forms) -> (
        match prepare_fn module_path env params body_forms with
        | Error _ as err -> err
        | Ok parts -> (
            let local_name = Names.sanitize_name name in
            let public_name = module_binding_ocaml_name module_path name in
            let param_tys =
              parts.param_bindings
              |> List.map (fun (_key, (binding : binding)) -> binding.ty)
            in
            let local_row_types = row_param_type_names local_name param_tys in
            let public_row_types = row_param_type_names public_name param_tys in
            let expr = fn_code ~row_param_type_names:local_row_types parts in
            let key = module_binding_key module_path name in
            match
              check_emitted_name_collision env ~source_key:key ~ocaml_name:local_name
            with
            | Error _ as err -> err
            | Ok () -> (match expr.ty with
            | TFn _ ->
                let local_binding =
                  binding_of_expr ~row_param_types:local_row_types local_name expr
                in
                let public_binding =
                  Types.binding ~row_param_types:public_row_types
                    ?return_param_index:(expr.return_param_index) public_name
                    (Types.qualify_module_type
                       (Names.module_path_to_ocaml module_path)
                       expr.ty)
                in
                let type_items = row_type_items local_row_types param_tys in
                let value_item =
                  Value_binding
                    { pattern = Named local_name; expression = expr.ocaml_expr }
                in
                Ok
                  ( Env.add key local_binding env,
                    public_bindings @ [ (key, public_binding) ],
                    next_type,
                    Group (type_items @ [ value_item ]) :: items )
            | _ -> Error.error "defn body did not compile to a function")))
    | FList
        (FSymbol "module" :: FSymbol nested_segment :: FSymbol nested_signature_name
        :: nested_forms) -> (
        let nested_path = module_path ^ "." ^ nested_segment in
        match
          compile_module ~signature_name:nested_signature_name scope env next_type
            nested_path nested_segment nested_forms
        with
        | Error _ as err -> err
        | Ok (_scope, nested_public_bindings, next_type, nested_item) ->
            Ok
              ( Env.add_bindings nested_public_bindings env,
                public_bindings @ nested_public_bindings,
                next_type,
                nested_item :: items ))
    | FList (FSymbol "module" :: FSymbol nested_segment :: nested_forms) -> (
        let nested_path = module_path ^ "." ^ nested_segment in
        match compile_module scope env next_type nested_path nested_segment nested_forms with
        | Error _ as err -> err
        | Ok (_scope, nested_public_bindings, next_type, nested_item) ->
            Ok
              ( Env.add_bindings nested_public_bindings env,
                public_bindings @ nested_public_bindings,
                next_type,
                nested_item :: items ))
    | _ ->
        Error.error
          "module forms must be module-signature, type-alias, type-record, type-variant, open, include, module-alias, defprotocol, extend-type, def, defn, or module"
  and loop env public_bindings next_type items = function
    | [] ->
        let module_name = Names.module_segment_to_ocaml module_segment in
        Ok
          ( scope,
            public_bindings,
            next_type,
            Module_def
              {
                module_name;
                signature_name =
                  Option.map Names.module_path_to_ocaml signature_name;
                items = List.rev items;
              } )
    | form :: rest -> (
        match compile_module_form env public_bindings next_type items form with
        | Error _ as err -> err
        | Ok (env, public_bindings, next_type, items) ->
            loop env public_bindings next_type items rest)
  in
  loop env [] next_type [] forms

let compile_module_functor scope env next_type functor_name parameter_form
    body_forms =
  let rec parse_parameters acc = function
    | [] -> Ok (List.rev acc)
    | FSymbol parameter_name :: FSymbol parameter_signature :: rest ->
        parse_parameters
          (( parameter_name,
             Names.module_path_to_ocaml parameter_signature )
          :: acc)
          rest
    | [ _ ] ->
        Error.error "module-functor parameters must be name/signature pairs"
    | _ -> Error.error "module-functor parameters must be symbols"
  in
  match parameter_form with
  | FVector [] -> Error.error "module-functor parameter vector must not be empty"
  | FVector parameter_forms -> (
      match parse_parameters [] parameter_forms with
      | Error _ as err -> err
      | Ok parameters ->
          let parameter_bindings =
            parameters
            |> List.concat_map (fun (parameter_name, parameter_signature) ->
                   signature_parameter_bindings env parameter_name
                     parameter_signature)
          in
          let functor_env = Env.add_bindings parameter_bindings env in
          (match
             compile_module scope functor_env next_type functor_name
               functor_name body_forms
           with
          | Error _ as err -> err
          | Ok (_scope, public_bindings, next_type, module_item) -> (
              match module_item with
              | Module_def { items; _ } ->
                  let functor_bindings =
                    store_functor_result_bindings functor_name public_bindings
                  in
                  Ok
                    ( scope,
                      Env.add_bindings functor_bindings env,
                      next_type,
                      Module_functor
                        {
                          functor_name =
                            Names.module_segment_to_ocaml functor_name;
                          parameters =
                            List.map
                              (fun (name, signature) ->
                                ( Names.module_segment_to_ocaml name,
                                  signature ))
                              parameters;
                          items;
                        } )
              | _ ->
                  Error.error
                    "internal error: module functor body did not compile")))
  | _ ->
      Error.error
        "module-functor expects a name, [parameter signature ...], and body"

let compile_top_level scope env next_type = function
  | FList (FSymbol "module-signature" :: FSymbol signature_name :: item_forms) ->
      compile_module_signature scope env next_type signature_name item_forms
  | FList (FSymbol "module-signature" :: _) ->
      Error.error "module-signature expects a name and signature items"
  | FList [ FSymbol "type-alias"; FSymbol name; manifest_form ] ->
      compile_type_alias scope env next_type name [] manifest_form
  | FList [ FSymbol "type-alias"; FSymbol name; FVector parameter_forms; manifest_form ] -> (
      match parse_type_parameters (FVector parameter_forms) with
      | Error _ as err -> err
      | Ok type_parameters ->
          compile_type_alias scope env next_type name type_parameters manifest_form)
  | FList
      (FSymbol "type-record" :: FSymbol name :: FVector parameter_forms
      :: field_forms) -> (
      match parse_type_parameters (FVector parameter_forms) with
      | Error _ as err -> err
      | Ok type_parameters ->
          compile_type_record scope env next_type name type_parameters field_forms)
  | FList (FSymbol "type-record" :: FSymbol name :: field_forms) ->
      compile_type_record scope env next_type name [] field_forms
  | FList (FSymbol "type-record" :: _) ->
      Error.error "type-record expects a name and fields"
  | FList
      (FSymbol "type-variant" :: FSymbol name :: FVector parameter_forms
      :: constructor_forms) -> (
      match parse_type_parameters (FVector parameter_forms) with
      | Error _ as err -> err
      | Ok type_parameters ->
          compile_type_variant scope env next_type name type_parameters constructor_forms)
  | FList (FSymbol "type-variant" :: FSymbol name :: constructor_forms) ->
      compile_type_variant scope env next_type name [] constructor_forms
  | FList [ FSymbol "open"; FSymbol module_path ] ->
      let env = open_module_bindings scope env module_path in
      Ok
        ( scope,
          env,
          next_type,
          Open_module (Names.module_path_to_ocaml module_path) )
  | FList [ FSymbol "include"; FSymbol module_path ] ->
      let env = open_module_bindings scope env module_path in
      Ok
        ( scope,
          env,
          next_type,
          Include_module (Names.module_path_to_ocaml module_path) )
  | FList (FSymbol "include" :: _) ->
      Error.error "include expects one module"
  | FList [ FSymbol "module-alias"; FSymbol alias_name; FSymbol target_name ] ->
      compile_module_alias scope env next_type alias_name target_name
  | FList (FSymbol "module-alias" :: _) ->
      Error.error "module-alias expects alias and target modules"
  | FList
      (FSymbol "module-functor" :: FSymbol functor_name :: parameter_form
      :: body_forms) ->
      compile_module_functor scope env next_type functor_name parameter_form
        body_forms
  | FList (FSymbol "module-functor" :: _) ->
      Error.error
        "module-functor expects a name, [parameter signature ...], and body"
  | FList
      (FSymbol "module-apply" :: FSymbol module_name :: FSymbol functor_name
      :: (_ :: _ as argument_forms)) ->
      let rec parse_arguments acc = function
        | [] -> Ok (List.rev acc)
        | FSymbol name :: rest -> parse_arguments (name :: acc) rest
        | _ ->
            Error.error
              "module-apply expects result, functor, and one or more argument modules"
      in
      (match parse_arguments [] argument_forms with
      | Error _ as err -> err
      | Ok argument_names ->
          compile_module_apply scope env next_type module_name functor_name
            argument_names)
  | FList (FSymbol "module-apply" :: _) ->
      Error.error
        "module-apply expects result, functor, and one or more argument modules"
  | FList [ FSymbol "def"; FSymbol name; expr_form ] -> (
      match compile_expr scope env expr_form with
      | Error _ as err -> err
      | Ok expr ->
          let ocaml_name = Names.ocaml_binding_name scope name in
          let env_key = Names.scoped_key scope name in
          (match check_emitted_name_collision env ~source_key:env_key ~ocaml_name with
          | Error _ as err -> err
          | Ok () -> (match expr.ty with
          | TRecord fields -> (
              match expr.record_values with
              | None -> Error.error "internal error: record expression missing values"
              | Some values ->
                  let type_name = "t" ^ string_of_int next_type in
                  let set_module_name = "Set_" ^ type_name in
                  let binding =
                    Types.binding ocaml_name
                      (Types.named_record ~type_name ~set_module_name fields)
                  in
                  Ok
                    ( scope,
                      Env.add env_key binding env,
                      next_type + 1,
                      Record_def
                        { var_name = ocaml_name;
                          type_name;
                          set_module_name;
                          fields;
                          values } ))
          | _ ->
              let binding = binding_of_expr ocaml_name expr in
              Ok
                ( scope,
                  Env.add env_key binding env,
                  next_type,
                  Value_binding
                    { pattern = Named ocaml_name; expression = expr.ocaml_expr } ))))
  | FList (FSymbol "defn" :: FSymbol name :: params :: body_forms) -> (
      match prepare_fn scope env params body_forms with
      | Error _ as err -> err
      | Ok parts -> (
          let ocaml_name = Names.ocaml_binding_name scope name in
          let param_tys =
            parts.param_bindings
            |> List.map (fun (_key, (binding : binding)) -> binding.ty)
          in
          let row_param_types = row_param_type_names ocaml_name param_tys in
          let expr = fn_code ~row_param_type_names:row_param_types parts in
          let env_key = Names.scoped_key scope name in
          match check_emitted_name_collision env ~source_key:env_key ~ocaml_name with
          | Error _ as err -> err
          | Ok () -> (match expr.ty with
          | TFn _ ->
              let binding = binding_of_expr ~row_param_types ocaml_name expr in
              let type_items = row_type_items row_param_types param_tys in
              let value_item =
                Value_binding
                  { pattern = Named ocaml_name; expression = expr.ocaml_expr }
              in
              Ok
                ( scope,
                  Env.add env_key binding env,
                  next_type,
                  Group (type_items @ [ value_item ]) )
          | _ -> Error.error "defn body did not compile to a function")))
  | FList (FSymbol "defprotocol" :: FSymbol protocol_name :: method_forms) ->
      compile_defprotocol scope env next_type protocol_name method_forms
  | FList
      (FSymbol "extend-type" :: receiver_form :: FSymbol protocol_name
      :: method_forms) ->
      compile_extend_type scope env next_type receiver_form protocol_name
        method_forms
  | FList (FSymbol "module" :: FSymbol module_name :: FSymbol signature_name :: forms) -> (
      match
        compile_module ~signature_name scope env next_type module_name module_name
          forms
      with
      | Error _ as err -> err
      | Ok (scope, module_bindings, next_type, item) ->
          Ok (scope, Env.add_bindings module_bindings env, next_type, item))
  | FList (FSymbol "module" :: FSymbol module_name :: forms) -> (
      match compile_module scope env next_type module_name module_name forms with
      | Error _ as err -> err
      | Ok (scope, module_bindings, next_type, item) ->
          Ok (scope, Env.add_bindings module_bindings env, next_type, item))
  | FList (FSymbol (("print" | "println") as name) :: args) -> (
      match compile_call scope env name args with
      | Error _ as err -> err
      | Ok expr ->
          Ok
            ( scope,
              env,
              next_type,
              Value_binding
                { pattern = Unit_pattern; expression = expr.ocaml_expr } ))
  | FList (FSymbol "require" :: entries) -> (
      match Require.parse_entries entries with
      | Error _ as err -> err
      | Ok specs ->
          let rec apply_specs env = function
            | [] -> Ok env
            | Require.Package _ :: rest -> apply_specs env rest
            | Require.Alias { module_name; alias } :: rest ->
                if String.starts_with ~prefix:"ocaml." module_name then
                  apply_specs
                    (Require.add_ocaml_alias_bindings env module_name alias)
                    rest
                else if module_name = "clojure.string" then
                  apply_specs
                    (Require.add_clojure_string_alias_bindings env alias)
                    rest
                else
                  Error.error
                    "require only accepts OCaml packages, OCaml modules, and clojure.string"
            | Require.Refer { module_name; names } :: rest ->
                let result =
                  if String.starts_with ~prefix:"ocaml." module_name then
                    Require.add_ocaml_refer_bindings env scope module_name names
                  else if module_name = "clojure.string" then
                    Require.add_clojure_string_refer_bindings env scope names
                  else
                    Error.error
                      "require only accepts OCaml packages, OCaml modules, and clojure.string"
                in
                (match result with
                | Error _ as err -> err
                | Ok env -> apply_specs env rest)
          in
          (match apply_specs env specs with
          | Error _ as err -> err
          | Ok env -> Ok (scope, env, next_type, Comment "require")))
  | (FList (FSymbol "loop" :: _) as form) -> (
      match compile_expr scope env form with
      | Error _ as err -> err
      | Ok expr ->
          Ok
            ( scope,
              env,
              next_type,
              Value_binding
                { pattern = Ignore_pattern; expression = expr.ocaml_expr } ))
  | FList (FSymbol "recur" :: _) ->
      Error.error "recur is only valid in a loop tail position"
  | form -> (
      match compile_expr scope env form with
      | Error _ as err -> err
      | Ok expr -> (
          match expr.record_values with
          | Some _ -> Error.error "top-level map literals must be bound with def"
          | None ->
              Ok
                ( scope,
                  env,
                  next_type,
                  Value_binding
                    { pattern = Ignore_pattern; expression = expr.ocaml_expr } )))

type state = {
  env : Env.t;
  next_type : int;
  items : compiled_item list;
}

let empty_state = { env = Env.empty; next_type = 1; items = [] }

let compile_forms_incremental state forms =
  let rec loop env next_type items = function
    | [] -> Ok (env, next_type, List.rev items)
    | form :: rest -> (
        match compile_top_level "" env next_type form with
        | Error _ as err -> err
        | Ok (_scope, env, next_type, item) ->
            loop env next_type (item :: items) rest)
  in
  match loop state.env state.next_type [] forms with
  | Error _ as err -> err
  | Ok (env, next_type, new_items) ->
      let next_state =
        { env; next_type; items = state.items @ new_items }
      in
      Ok (next_state, new_items)

let compile_forms forms =
  match compile_forms_incremental empty_state forms with
  | Error _ as err -> err
  | Ok (_state, items) -> Ok items
