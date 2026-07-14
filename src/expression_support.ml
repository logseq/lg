open Types
open Lowered

module Env = Compiler_environment

let rec truthiness_expression ty expression =
  match ty with
  | ty when Types.is_dynamic ty ->
      Semantic_ir.Apply
        ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.truthy",
          [ expression ] )
  | TBool -> expression
  | TNil -> Semantic_ir.Sequence [ expression; Semantic_ir.Bool false ]
  | TNullable payload_ty ->
      Semantic_ir.Match
        ( expression,
          [ (Semantic_ir.PConstructor ("None", None), Semantic_ir.Bool false);
            ( Semantic_ir.PConstructor
                ("Some", Some (Semantic_ir.PVar "truthy_value")),
              truthiness_expression payload_ty
                (Semantic_ir.Ident "truthy_value") );
          ] )
  | TOcaml_app ("option", [ _ ]) | TOcaml "option" ->
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
  | TOcaml_app (name, [ _ ]) when name = Types.next_seq_type_name ->
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
     | TNullable _ -> List.mem name [ "Some"; "None" ]
     | TUnknown | TVar _ ->
         List.mem name [ "Some"; "None"; "Ok"; "Error" ]
         || String.contains name '.' || String.contains name '/'
     | _ -> false)

let plain_dynamic_compatible_type = function
  | TInt | TFloat | TChar | TString | TSymbol | TKeyword | TBool | TNil
  | TNamed_record _ ->
      true
  | ty -> Types.is_dynamic ty

let rec merge_branch_types left right =
  if Types.equal left right then Some left
  else
    match (left, right) with
    | left, right when Types.is_dynamic left || Types.is_dynamic right ->
        Some (Types.dynamic_constraint TUnknown)
    | TNil, (TOcaml_app ("option", _) as option_ty)
    | (TOcaml_app ("option", _) as option_ty), TNil
    | TNil, (TOcaml "option" as option_ty)
    | (TOcaml "option" as option_ty), TNil ->
        Some option_ty
    | TNil, TNullable inner | TNullable inner, TNil -> Some (TNullable inner)
    | TNil, ty | ty, TNil -> Some (TNullable ty)
    | TNullable left, TNullable right -> (
        match merge_branch_types left right with
        | Some inner -> Some (TNullable inner)
        | None
          when plain_dynamic_compatible_type left
               && plain_dynamic_compatible_type right ->
            Some (TNullable (Types.dynamic_constraint TUnknown))
        | None -> None)
    | TNullable inner, ty | ty, TNullable inner ->
        Option.map (fun merged -> TNullable merged)
          (merge_branch_types inner ty)
    | TList TUnknown, TList inner | TList inner, TList TUnknown ->
        Some (TList inner)
    | TSeq TUnknown, TSeq inner | TSeq inner, TSeq TUnknown ->
        Some (TSeq inner)
    | TVector (TUnknown | TVar _), TVector inner
    | TVector inner, TVector (TUnknown | TVar _) ->
        Some (TVector inner)
    | TVar _, TVar _ -> Some left
    | TVar _, ty | ty, TVar _ -> Some ty
    | TUnknown, ty | ty, TUnknown -> Some ty
    | _ when Types.defer_to_ocaml ~expected:left ~actual:right -> Some left
    | _ -> None

let branch_types_compatible left right =
  Option.is_some (merge_branch_types left right)

let capability_storage_expression ty expression =
  let rec build name = function
    | ty when Types.is_dynamic ty -> Semantic_ir.Ident name
    | ty -> (
        match Types.protocol_constraint_info ty with
        | Some (protocol_id, _, value_ty) ->
            Semantic_ir.Tuple
              [ Semantic_ir.Ident
                  (Types.protocol_witness_name name protocol_id);
                build name value_ty;
              ]
        | None -> (
            match ty with
            | TOcaml_app (constraint_name, [ _element_ty; value_ty ])
              when constraint_name = Types.seqable_constraint_name
                   || constraint_name = Types.optional_seqable_constraint_name
                   || constraint_name =
                      Types.optional_sequential_constraint_name ->
                Semantic_ir.Tuple
                  [ Semantic_ir.Ident
                      (if constraint_name = Types.seqable_constraint_name then
                         name ^ "__seq"
                       else name ^ "__seq_optional");
                    build name value_ty;
                  ]
            | _ -> Semantic_ir.Ident name))
  in
  match Semantic_ir.unlocated expression with
  | Semantic_ir.Ident name -> build name ty
  | _ -> expression

let rec pack_plain_dynamic_value value =
  let runtime name arguments =
    Semantic_ir.Apply
      (Semantic_ir.Ident ("Lg_runtime.Runtime_dynamic." ^ name), arguments)
  in
  match value.ty with
  | ty when Types.is_dynamic ty -> Some value.semantic_expr
  | TInt -> Some (runtime "int" [ value.semantic_expr ])
  | TFloat -> Some (runtime "float" [ value.semantic_expr ])
  | TChar -> Some (runtime "char" [ value.semantic_expr ])
  | TString -> Some (runtime "string" [ value.semantic_expr ])
  | TSymbol -> Some (runtime "symbol" [ value.semantic_expr ])
  | TKeyword -> Some (runtime "keyword" [ value.semantic_expr ])
  | TBool -> Some (runtime "bool" [ value.semantic_expr ])
  | TNil -> Some (Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.nil")
  | TVector element_ty | TList element_ty | TSeq element_ty ->
      let item_name = "__lg_plain_dynamic_item" in
      let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
      Option.map
        (fun packed_item ->
          let mapper =
            Semantic_ir.Fun ([ Semantic_ir.PVar item_name ], packed_item)
          in
          match value.ty with
          | TVector _ ->
              runtime "vector"
                [ Semantic_ir.Apply
                    ( Semantic_ir.Ident "Rrbvec.map",
                      [ mapper; value.semantic_expr ] ) ]
          | TList _ ->
              runtime "list"
                [ Semantic_ir.Apply
                    ( Semantic_ir.Ident "List.map",
                      [ mapper; value.semantic_expr ] ) ]
          | _ ->
              runtime "seq"
                [ Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                      [ mapper; value.semantic_expr ] ) ])
        (pack_plain_dynamic_value item)
  | TNamed_record record ->
      let rec fields packed = function
        | [] -> Some (List.rev packed)
        | (field : field) :: rest ->
            let field_value =
              typed_ir field.ty (Structural_map.field_expr value field)
            in
            (match pack_plain_dynamic_value field_value with
            | None -> None
            | Some field_value ->
                let key = runtime "keyword" [ Semantic_ir.String field.keyword ] in
                fields (Semantic_ir.Tuple [ key; field_value ] :: packed) rest)
      in
      Option.map
        (fun fields ->
          runtime "record"
            [ Semantic_ir.String record.type_name; Semantic_ir.List fields ])
        (fields [] record.fields)
  | _ -> None

let coerce_expression_to_type ?(stored = false) target_ty source_ty expression =
  match (target_ty, source_ty) with
  | TVector element_ty, source_ty when Types.is_dynamic source_ty ->
      let dynamic name arguments =
        Semantic_ir.Apply
          ( Semantic_ir.Ident ("Lg_runtime.Runtime_dynamic." ^ name),
            arguments )
      in
      let item_name = "__lg_dynamic_vector_item" in
      let item = Semantic_ir.Ident item_name in
      let unpacked_item =
        match element_ty with
        | ty when Types.is_dynamic ty -> item
        | TInt -> dynamic "as_int" [ item ]
        | TFloat -> dynamic "as_float" [ item ]
        | TChar -> dynamic "as_char" [ item ]
        | TString -> dynamic "as_string" [ item ]
        | TSymbol -> dynamic "as_symbol" [ item ]
        | TKeyword -> dynamic "as_keyword" [ item ]
        | TBool -> dynamic "as_bool" [ item ]
        | _ -> item
      in
      Semantic_ir.Apply
        ( Semantic_ir.Ident "Rrbvec.of_list",
          [ Semantic_ir.Apply
              ( Semantic_ir.Ident "List.of_seq",
                [ Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                      [ Semantic_ir.Fun
                          ([ Semantic_ir.PVar item_name ], unpacked_item);
                        dynamic "to_seq" [ expression ];
                      ] );
                ] );
          ] )
  | target_ty, source_ty
    when Types.is_dynamic target_ty && not (Types.is_dynamic source_ty) ->
      pack_plain_dynamic_value (typed_ir source_ty expression)
      |> Option.value ~default:expression
  | TNullable target, TNullable source
    when Types.is_dynamic target && not (Types.is_dynamic source) ->
      let value_name = "__lg_nullable_dynamic_value" in
      let value = typed_ir source (Semantic_ir.Ident value_name) in
      let packed =
        pack_plain_dynamic_value value
        |> Option.value ~default:value.semantic_expr
      in
      Semantic_ir.Match
        ( expression,
          [ ( Semantic_ir.PConstructor ("None", None),
              Semantic_ir.Constructor ("None", None) );
            ( Semantic_ir.PConstructor
                ("Some", Some (Semantic_ir.PVar value_name)),
              Semantic_ir.Constructor ("Some", Some packed) );
          ] )
  | target_ty, TOcaml_app (constraint_name, [ _element_ty; _value_ty ])
    when (match target_ty with
         | TSeq _ -> true
         | TOcaml_app (name, [ _ ]) -> name = Types.next_seq_type_name
         | _ -> false)
         && (constraint_name = Types.seqable_constraint_name
            || constraint_name = Types.optional_seqable_constraint_name
            || constraint_name = Types.optional_sequential_constraint_name) ->
      let adapter_name = "__lg_coerce_seq_adapter" in
      let value_name = "__lg_coerce_seq_value" in
      let adapter = Semantic_ir.Ident adapter_name in
      let adapter =
        if constraint_name = Types.seqable_constraint_name then adapter
        else
          Semantic_ir.Match
            ( adapter,
              [ ( Semantic_ir.PConstructor ("None", None),
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident "invalid_arg",
                      [ Semantic_ir.String "value is not sequential" ] ) );
                ( Semantic_ir.PConstructor
                    ("Some", Some (Semantic_ir.PVar adapter_name)),
                  Semantic_ir.Ident adapter_name );
              ] )
      in
      Semantic_ir.Match
        ( (if stored then expression
           else capability_storage_expression source_ty expression),
          [ ( Semantic_ir.PTuple
                [ Semantic_ir.PVar adapter_name;
                  Semantic_ir.PVar value_name;
                ],
              Semantic_ir.Apply
                (adapter, [ Semantic_ir.Ident value_name ]) );
          ] )
  | TNullable _, TNil -> expression
  | TNullable _, TNullable _ -> expression
  | TNullable _, _ -> Semantic_ir.Constructor ("Some", Some expression)
  | _ -> expression

let merge_branch_expressions left right =
  let merge_tuple_items left_types left_items right_types right_items =
    let rec merge types left_values right_values =
      match (types, left_values, right_values) with
      | [], [], [] -> Some ([], [], [])
      | (left_ty, right_ty) :: types, left_value :: left_values,
        right_value :: right_values ->
          let merged_ty =
            match merge_branch_types left_ty right_ty with
            | Some ty -> Some ty
            | None
              when plain_dynamic_compatible_type left_ty
                   && plain_dynamic_compatible_type right_ty ->
                Some (Types.dynamic_constraint TUnknown)
            | None -> None
          in
          Option.bind merged_ty (fun merged_ty ->
              Option.map
                (fun (merged_types, merged_left, merged_right) ->
                  ( merged_ty :: merged_types,
                    coerce_expression_to_type ~stored:true merged_ty left_ty
                      left_value
                    :: merged_left,
                    coerce_expression_to_type ~stored:true merged_ty right_ty
                      right_value
                    :: merged_right ))
                (merge types left_values right_values))
      | _ -> None
    in
    merge (List.combine left_types right_types) left_items right_items
  in
  match
    (left.ty, right.ty)
  with
  | TTuple left_types, TTuple right_types
    when List.length left_types = List.length right_types ->
      let left_names =
        List.mapi (fun index _ -> "__lg_left_tuple_" ^ string_of_int index)
          left_types
      in
      let right_names =
        List.mapi (fun index _ -> "__lg_right_tuple_" ^ string_of_int index)
          right_types
      in
      Option.map
        (fun (types, left_items, right_items) ->
          let rebuild expression names items =
            Semantic_ir.Match
              ( expression,
                [ ( Semantic_ir.PTuple
                      (List.map (fun name -> Semantic_ir.PVar name) names),
                    Semantic_ir.Tuple items );
                ] )
          in
          ( TTuple types,
            rebuild left.semantic_expr left_names left_items,
            rebuild right.semantic_expr right_names right_items ))
        (merge_tuple_items left_types
           (List.map (fun name -> Semantic_ir.Ident name) left_names)
           right_types
           (List.map (fun name -> Semantic_ir.Ident name) right_names))
  | _ ->
  let continue expression =
    Semantic_ir.Apply
      (Semantic_ir.Ident "Lg_runtime.Runtime_reduced.continue", [ expression ])
  in
  match (Types.reduced_element left.ty, Types.reduced_element right.ty) with
  | Some left_inner, Some right_inner when Types.equal left_inner right_inner ->
      Some (left.ty, left.semantic_expr, right.semantic_expr)
  | Some TNil, None ->
      let nullable = TNullable right.ty in
      Some
        ( Types.reduced nullable,
          left.semantic_expr,
          continue (Semantic_ir.Constructor ("Some", Some right.semantic_expr)) )
  | None, Some TNil ->
      let nullable = TNullable left.ty in
      Some
        ( Types.reduced nullable,
          continue (Semantic_ir.Constructor ("Some", Some left.semantic_expr)),
          right.semantic_expr )
  | Some inner, None when Types.equal inner right.ty ->
      Some (left.ty, left.semantic_expr, continue right.semantic_expr)
  | None, Some inner when Types.equal left.ty inner ->
      Some (right.ty, continue left.semantic_expr, right.semantic_expr)
  | _ -> (
      match merge_branch_types left.ty right.ty with
      | None -> None
      | Some result_ty ->
          Some
            ( result_ty,
              coerce_expression_to_type result_ty left.ty left.semantic_expr,
              coerce_expression_to_type result_ty right.ty right.semantic_expr ))

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
  | TNullable payload_ty, "Some" -> Some [ payload_ty ]
  | TNullable _, "None" -> Some []
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

let deftype_method_name (record : named_record) method_name arity =
  "__deftype/" ^ Type_id.to_string record.type_id ^ "/" ^ method_name ^ "/"
  ^ string_of_int arity

let lookup_deftype_method scope env record method_name arity =
  lookup_binding scope env (deftype_method_name record method_name arity)

let binding_of_expr ?(row_param_types = []) ocaml_name expr =
  Types.binding ~row_param_types ?return_param_index:expr.return_param_index
    ocaml_name expr.ty

type anonymous_record_allocation = {
  record : named_record;
  env : Env.t;
  next_type : int;
  fresh : bool;
}

let allocate_anonymous_record ~owner env next_type fields =
  match Env.find_anonymous_record ~owner fields env with
  | Some record -> { record; env; next_type; fresh = false }
  | None ->
      let type_name = "t" ^ string_of_int next_type in
      let set_module_name = "Set_" ^ type_name in
      let record =
        match Types.named_record ~type_name ~set_module_name fields with
        | TNamed_record record -> record
        | _ -> assert false
      in
      { record;
        env = Env.add_anonymous_record ~owner record env;
        next_type = next_type + 1;
        fresh = true }

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

let parameterize_row_fields fields =
  let next_parameter = ref 0 in
  let named_parameters = ref [] in
  let parameters = ref [] in
  let fresh_parameter () =
    let parameter = "a" ^ string_of_int !next_parameter in
    incr next_parameter;
    parameters := parameter :: !parameters;
    parameter
  in
  let named_parameter name =
    match List.assoc_opt name !named_parameters with
    | Some parameter -> parameter
    | None ->
        let parameter = fresh_parameter () in
        named_parameters := (name, parameter) :: !named_parameters;
        parameter
  in
  let rec parameterize = function
    | TUnknown -> TVar (fresh_parameter ())
    | TVar name -> TVar (named_parameter name)
    | TNullable ty -> TNullable (parameterize ty)
    | TOcaml_app (name, arguments) ->
        TOcaml_app (name, List.map parameterize arguments)
    | TTuple items -> TTuple (List.map parameterize items)
    | TArray ty -> TArray (parameterize ty)
    | TRef ty -> TRef (parameterize ty)
    | TList ty -> TList (parameterize ty)
    | TVector ty -> TVector (parameterize ty)
    | TSet ty -> TSet (parameterize ty)
    | TSeq ty -> TSeq (parameterize ty)
    | TFn (parameters, return_type) ->
        TFn (List.map parameterize parameters, parameterize return_type)
    | TOverloaded_fn arities ->
        TOverloaded_fn
          (List.map
             (fun (arity : fn_arity) ->
               { fixed_params = List.map parameterize arity.fixed_params;
                 rest_param = Option.map parameterize arity.rest_param;
                 return_ty = parameterize arity.return_ty })
             arities)
    | TRecord fields -> TRecord (List.map parameterize_field fields)
    | (TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol
      | TKeyword | TBool | TUnit | TNil | TOcaml _ | TNamed_record _) as ty ->
        ty
  and parameterize_field (field : field) =
    { field with ty = parameterize field.ty }
  in
  let fields = List.map parameterize_field fields in
  (fields, List.rev !parameters)

let row_param_type_names prefix param_tys =
  param_tys
  |> List.mapi (fun index -> function
       | TRecord fields ->
           let type_name = prefix ^ "_row" ^ string_of_int index in
           let _, parameters = parameterize_row_fields fields in
           let parameters = List.map (fun name -> "'" ^ name) parameters in
           let applied_name =
             match parameters with
             | [] -> type_name
             | [ parameter ] -> parameter ^ " " ^ type_name
             | parameters ->
                 "(" ^ String.concat ", " parameters ^ ") " ^ type_name
           in
           Some applied_name
       | _ -> None)

let row_type_items row_type_names param_tys =
  List.map2
    (fun row_type_name param_ty ->
      match (row_type_name, param_ty) with
      | Some applied_name, TRecord fields ->
          let type_name =
            match String.rindex_opt applied_name ' ' with
            | None -> applied_name
            | Some index ->
                String.sub applied_name (index + 1)
                  (String.length applied_name - index - 1)
          in
          let fields, type_parameters = parameterize_row_fields fields in
          Some
            (Type_def
               { type_name; type_parameters; fields; location = None })
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
