open Types

let one_arg name args =
  match args with
  | [ arg ] -> Ok arg
  | _ -> Error.error (name ^ " expects 1 arguments")

let evaluated_argument arg =
  Semantic_ir.evaluate_for_effect arg.semantic_expr

let type_predicate name predicate args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg ->
      Ok
        (typed_ir TBool
           (Semantic_ir.Sequence
              [ evaluated_argument arg; Semantic_ir.Bool (predicate arg.ty) ]))

let compile_predicate name args expected_ty =
  type_predicate name (fun actual_ty -> Types.equal actual_ty expected_ty) args

let compile_bool_literal_predicate name args expected =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg ->
      let expression =
        if Types.is_dynamic arg.ty then
          Semantic_ir.Apply
            ( Semantic_ir.Ident
                (if expected then "Lg_runtime.Runtime_dynamic.is_true"
                 else "Lg_runtime.Runtime_dynamic.is_false"),
              [ arg.semantic_expr ] )
        else if Types.equal arg.ty TBool then
          Semantic_ir.Infix ("=", arg.semantic_expr, Semantic_ir.Bool expected)
        else
          Semantic_ir.Sequence [ evaluated_argument arg; Semantic_ir.Bool false ]
      in
      Ok (typed_ir TBool expression)

let compile_type_predicate name predicate args = type_predicate name predicate args

let compile_runtime_type_predicate name runtime_function predicate args =
  match one_arg name args with
  | Error _ as error -> error
  | Ok arg when Types.is_dynamic arg.ty ->
      Ok
        (typed_ir TBool
           (Semantic_ir.Apply
              (Semantic_ir.Ident runtime_function, [ arg.semantic_expr ])))
  | Ok arg ->
      Ok
        (typed_ir TBool
           (Semantic_ir.Sequence
              [ evaluated_argument arg; Semantic_ir.Bool (predicate arg.ty) ]))

let compile_string_family_predicate name ~keyword args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg -> (
      match arg.ty with
      | ty when Types.is_dynamic ty ->
          let function_name =
            if keyword then "Lg_runtime.Runtime_dynamic.is_keyword"
            else "Lg_runtime.Runtime_dynamic.is_string"
          in
          Ok
            (typed_ir TBool
               (Semantic_ir.Apply
                  (Semantic_ir.Ident function_name, [ arg.semantic_expr ])))
      | TUnknown ->
          let starts_with_colon =
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Lg_runtime.Runtime_string.starts_with",
                [ arg.semantic_expr; Semantic_ir.String ":" ] )
          in
          let expression =
            if keyword then starts_with_colon
            else Semantic_ir.Prefix ("not", starts_with_colon)
          in
          Ok (typed_ir TBool expression)
      | actual ->
          let matches = if keyword then Types.equal actual TKeyword else Types.equal actual TString in
          Ok
            (typed_ir TBool
               (Semantic_ir.Sequence
                  [ evaluated_argument arg; Semantic_ir.Bool matches ])))

let compile_nil_predicate name args expected_nil =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg ->
      let is_nil =
        Expression_support.nil_predicate_expression arg.ty arg.semantic_expr
      in
      let expression =
        if expected_nil then is_nil else Semantic_ir.Prefix ("not", is_nil)
      in
      Ok (typed_ir TBool expression)

let compile name args =
  match name with
  | "__lg_nil-predicate" -> compile_nil_predicate name args true
  | "__lg_true-predicate" -> compile_bool_literal_predicate name args true
  | "__lg_false-predicate" -> compile_bool_literal_predicate name args false
  | "__lg_int-predicate" ->
      compile_runtime_type_predicate name "Lg_runtime.Runtime_dynamic.is_int"
        (function TInt -> true | _ -> false)
        args
  | "__lg_number-predicate" ->
      compile_runtime_type_predicate name "Lg_runtime.Runtime_dynamic.is_number"
        Types.is_numeric args
  | "__lg_string-predicate" ->
      compile_string_family_predicate name ~keyword:false args
  | "__lg_keyword-predicate" ->
      compile_string_family_predicate name ~keyword:true args
  | "__lg_list-predicate" ->
      compile_runtime_type_predicate name "Lg_runtime.Runtime_dynamic.is_list"
        (function TList _ -> true | _ -> false)
        args
  | "__lg_seq-predicate" ->
      compile_runtime_type_predicate name "Lg_runtime.Runtime_dynamic.is_seq"
        (function TList _ | TSeq _ -> true | _ -> false)
        args
  | "__lg_fn-predicate" ->
      compile_type_predicate name (function TFn _ -> true | _ -> false) args
  | "__lg_uuid-predicate" ->
      compile_type_predicate name
        (function TOcaml "Lg_runtime.Runtime_uuid.t" -> true | _ -> false)
        args
  | "__lg_delay-predicate" ->
      compile_type_predicate name
        (function TOcaml_app ("Lazy.t", [ _ ]) -> true | _ -> false)
        args
  | _ -> Error.error ("unknown function " ^ name)
