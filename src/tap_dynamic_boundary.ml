open Ast
open Types

let dynamic_ty = Types.dynamic_constraint TUnknown

let apply name args = Semantic_ir.Apply (Semantic_ir.Ident name, args)
let dynamic_runtime name = "Lg_runtime.Runtime_dynamic." ^ name
let typed_dynamic expression = typed_ir dynamic_ty expression

let rec convert_typed_value value =
  let mapper_for_type ty =
    let value_name = "__lg_tap_dynamic_value" in
    Result.map
      (fun body -> Semantic_ir.Fun ([ Semantic_ir.PVar value_name ], body))
      (convert_type ty (Semantic_ir.Ident value_name))
  in
  if Types.is_dynamic value.ty then Ok value.semantic_expr
  else
    match Types.constraint_value_type value.ty with
    | TUnknown | TMeta _ | TVar _ -> Ok value.semantic_expr
    | TNil -> Ok (Semantic_ir.Ident (dynamic_runtime "nil"))
    | TInt | TOcaml "int" -> Ok (apply (dynamic_runtime "int") [ value.semantic_expr ])
    | TFloat -> Ok (apply (dynamic_runtime "float") [ value.semantic_expr ])
    | TChar -> Ok (apply (dynamic_runtime "char") [ value.semantic_expr ])
    | TString -> Ok (apply (dynamic_runtime "string") [ value.semantic_expr ])
    | TKeyword -> Ok (apply (dynamic_runtime "keyword") [ value.semantic_expr ])
    | TSymbol -> Ok (apply (dynamic_runtime "symbol") [ value.semantic_expr ])
    | TBool -> Ok (apply (dynamic_runtime "bool") [ value.semantic_expr ])
    | TRegex -> Ok (apply (dynamic_runtime "regex") [ value.semantic_expr ])
    | TRecord _ -> (
        match value.record_values with
        | Some fields ->
            let rec compile_fields acc = function
              | [] -> Ok (List.rev acc)
              | ((field : Types.field), expression) :: rest -> (
                  match convert_type field.ty expression with
                  | Error _ as error -> error
                  | Ok value ->
                      let key =
                        apply (dynamic_runtime "keyword")
                          [ Semantic_ir.String field.keyword ]
                      in
                      compile_fields
                        (Semantic_ir.Tuple [ key; value ] :: acc)
                        rest)
            in
            Result.map
              (fun entries -> apply (dynamic_runtime "map") [ Semantic_ir.List entries ])
              (compile_fields [] fields)
        | None ->
            Error.error "tap dynamic boundary requires record field evidence")
    | TVector element_ty -> (
        match mapper_for_type element_ty with
        | Error _ as error -> error
        | Ok mapper ->
            Ok
              (apply (dynamic_runtime "vector")
                 [
                   apply "Rrbvec.of_list"
                     [
                       apply "List.map"
                         [ mapper; apply "Rrbvec.to_list" [ value.semantic_expr ] ];
                     ];
                 ]))
    | TList element_ty -> (
        match mapper_for_type element_ty with
        | Error _ as error -> error
        | Ok mapper ->
            Ok
              (apply (dynamic_runtime "list")
                 [ apply "List.map" [ mapper; value.semantic_expr ] ]))
    | map_ty when Option.is_some (Types.dynamic_map_types map_ty) -> (
        let key_ty, value_ty = Option.get (Types.dynamic_map_types map_ty) in
        match (mapper_for_type key_ty, mapper_for_type value_ty) with
        | (Error _ as error), _ | _, (Error _ as error) -> error
        | Ok key_mapper, Ok value_mapper ->
            Ok
              (apply (dynamic_runtime "map")
                 [
                   apply "List.of_seq"
                     [
                       apply "Seq.map"
                         [
                           Semantic_ir.Fun
                             ( [ Semantic_ir.PVar "__lg_tap_map_entry" ],
                               Semantic_ir.Match
                                 ( Semantic_ir.Ident "__lg_tap_map_entry",
                                   [
                                     ( Semantic_ir.PTuple
                                         [
                                           Semantic_ir.PVar "__lg_tap_map_key";
                                           Semantic_ir.PVar "__lg_tap_map_value";
                                         ],
                                       Semantic_ir.Tuple
                                         [
                                           apply_expression key_mapper
                                             [ Semantic_ir.Ident "__lg_tap_map_key" ];
                                           apply_expression value_mapper
                                             [
                                               Semantic_ir.Ident
                                                 "__lg_tap_map_value";
                                             ];
                                         ] );
                                   ] ) );
                           apply "Lg_runtime.Runtime_map.to_seq"
                             [ value.semantic_expr ];
                         ];
                     ];
                 ]))
    | ty ->
        Error.error
          ("tap dynamic boundary does not support " ^ Types.source_name ty)

and convert_type ty expression =
  convert_typed_value (typed_ir ty expression)

and apply_expression fn args = Semantic_ir.Apply (fn, args)

let rec compile_form ~compile_expr scope env form =
  match form with
  | FKeyword keyword ->
      Ok
        (typed_dynamic
           (apply (dynamic_runtime "keyword") [ Semantic_ir.String keyword ]))
  | FString value ->
      Ok
        (typed_dynamic
           (apply (dynamic_runtime "string") [ Semantic_ir.String value ]))
  | FInt value ->
      Ok (typed_dynamic (apply (dynamic_runtime "int") [ Semantic_ir.Int value ]))
  | FFloat value ->
      Ok
        (typed_dynamic (apply (dynamic_runtime "float") [ Semantic_ir.Float value ]))
  | FDecimal _ ->
      Error.error "static decimal values cannot cross the tap dynamic boundary"
  | FChar value ->
      Ok (typed_dynamic (apply (dynamic_runtime "char") [ Semantic_ir.Char value ]))
  | FBool value ->
      Ok (typed_dynamic (apply (dynamic_runtime "bool") [ Semantic_ir.Bool value ]))
  | FRegex value ->
      Ok
        (typed_dynamic (apply (dynamic_runtime "regex") [ Semantic_ir.String value ]))
  | FSymbol "nil" -> Ok (typed_dynamic (Semantic_ir.Ident (dynamic_runtime "nil")))
  | FVector values ->
      compile_sequence ~compile_expr scope env values (fun values ->
          apply (dynamic_runtime "vector")
            [ apply "Rrbvec.of_list" [ Semantic_ir.List values ] ])
  | FMap entries ->
      let rec compile_entries acc = function
        | [] -> Ok (List.rev acc)
        | (key, value) :: rest -> (
            match
              ( compile_form ~compile_expr scope env key,
                compile_form ~compile_expr scope env value )
            with
            | Ok key, Ok value ->
                compile_entries
                  (Semantic_ir.Tuple [ key.semantic_expr; value.semantic_expr ]
                  :: acc)
                  rest
            | (Error _ as error), _ | _, (Error _ as error) -> error)
      in
      Result.map
        (fun entries ->
          typed_dynamic (apply (dynamic_runtime "map") [ Semantic_ir.List entries ]))
        (compile_entries [] entries)
  | FList _ | FSymbol _ | FCoreSymbol _ -> (
      match compile_expr scope env form with
      | Error _ as error -> error
      | Ok value -> Result.map typed_dynamic (convert_typed_value value))

and compile_sequence ~compile_expr scope env values constructor =
  let rec compile_values acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest -> (
        match compile_form ~compile_expr scope env value with
        | Error _ as error -> error
        | Ok value -> compile_values (value.semantic_expr :: acc) rest)
  in
  Result.map
    (fun values -> typed_dynamic (constructor values))
    (compile_values [] values)

let printable_dynamic_argument dynamic_expression =
  Semantic_ir.Tuple
    [
      Semantic_ir.Tuple
        [
          Semantic_ir.Ident (dynamic_runtime "str");
          Semantic_ir.Ident (dynamic_runtime "pr_str");
        ];
      dynamic_expression;
    ]

let hashable_dynamic_argument dynamic_expression =
  Semantic_ir.Tuple
    [ Semantic_ir.Ident (dynamic_runtime "hash"); dynamic_expression ]

let comparable_dynamic_argument dynamic_expression =
  Semantic_ir.Tuple
    [ Semantic_ir.Ident (dynamic_runtime "compare"); dynamic_expression ]

let truthy_dynamic_argument dynamic_expression =
  Semantic_ir.Tuple
    [ Semantic_ir.Ident (dynamic_runtime "truthy"); dynamic_expression ]

let nil_predicate_dynamic_argument dynamic_expression =
  Semantic_ir.Tuple
    [ Semantic_ir.Ident (dynamic_runtime "is_nil"); dynamic_expression ]

let callback_argument_for_type ty dynamic_expression =
  if Types.is_dynamic ty then Ok dynamic_expression
  else
    match ty with
    | ty when Option.is_some (Types.printable_constraint_info ty) ->
        Ok (printable_dynamic_argument dynamic_expression)
    | ty when Option.is_some (Types.hashable_constraint_info ty) ->
        Ok (hashable_dynamic_argument dynamic_expression)
    | ty when Option.is_some (Types.comparable_constraint_info ty) ->
        Ok (comparable_dynamic_argument dynamic_expression)
    | ty when Option.is_some (Types.truthy_constraint_info ty) ->
        Ok (truthy_dynamic_argument dynamic_expression)
    | ty when Option.is_some (Types.nil_predicate_constraint_info ty) ->
        Ok (nil_predicate_dynamic_argument dynamic_expression)
    | TInt | TOcaml "int" ->
        Ok (apply (dynamic_runtime "as_int") [ dynamic_expression ])
    | TFloat -> Ok (apply (dynamic_runtime "as_float") [ dynamic_expression ])
    | TChar -> Ok (apply (dynamic_runtime "as_char") [ dynamic_expression ])
    | TString -> Ok (apply (dynamic_runtime "as_string") [ dynamic_expression ])
    | TKeyword -> Ok (apply (dynamic_runtime "as_keyword") [ dynamic_expression ])
    | TSymbol -> Ok (apply (dynamic_runtime "as_symbol") [ dynamic_expression ])
    | TBool -> Ok (apply (dynamic_runtime "as_bool") [ dynamic_expression ])
    | TUnknown | TMeta _ | TVar _ -> Ok dynamic_expression
    | ty ->
        Error.error
          ("tap callback dynamic boundary does not support "
          ^ Types.source_name ty)

let compile_callback ~compile_expr scope env form =
  match compile_expr scope env form with
  | Error _ as error -> error
  | Ok { ty = TFn ([ parameter_ty ], _return_ty); semantic_expr; _ } ->
      let parameter_name = "__lg_tap_value" in
      Result.map
        (fun argument ->
          typed_ir (TFn ([ dynamic_ty ], TUnit))
            (Semantic_ir.Fun
               ( [ Semantic_ir.PVar parameter_name ],
                 Semantic_ir.Sequence
                   [
                     Semantic_ir.Apply
                       (semantic_expr, [ argument ]);
                     Semantic_ir.Unit;
                   ] )))
        (callback_argument_for_type parameter_ty (Semantic_ir.Ident parameter_name))
  | Ok { ty = TFn (parameters, _); _ } ->
      Error.error
        ("tap callbacks expect one argument, got "
        ^ string_of_int (List.length parameters))
  | Ok callback ->
      Error.error
        ("tap expects a one-argument function, got "
        ^ Types.source_name callback.ty)
