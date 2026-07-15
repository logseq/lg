open Ast
open Types
open Expression_support
module Env = Compiler_environment

type expression_result = (typed_expr, Error.t) result
type type_result = (ty, Error.t) result

type t = {
  compile_vector : string -> Env.t -> Ast.form list -> expression_result;
  compile_map :
    string -> Env.t -> (Ast.form * Ast.form) list -> expression_result;
  compile_if :
    string -> Env.t -> Ast.form -> Ast.form -> Ast.form -> expression_result;
  compile_if_not :
    string -> Env.t -> Ast.form -> Ast.form -> Ast.form -> expression_result;
  compile_if_let :
    string -> Env.t -> Ast.form -> Ast.form -> Ast.form -> expression_result;
  compile_if_some :
    string -> Env.t -> Ast.form -> Ast.form -> Ast.form -> expression_result;
  compile_when_let :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_when_some :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_let_some :
    string -> Env.t -> Ast.form -> Ast.form -> Ast.form -> expression_result;
  compile_when :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_cond : string -> Env.t -> Ast.form list -> expression_result;
  compile_logical :
    string -> Env.t -> [ `And | `Or ] -> Ast.form list -> expression_result;
  compile_match :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_body :
    string -> Env.t -> string -> Ast.form list -> expression_result;
  compile_try : string -> Env.t -> Ast.form list -> expression_result;
  loop_branch_type : ty -> ty -> type_result;
  compile_recur :
    string -> Env.t -> string -> ty list -> Ast.form list -> expression_result;
  compile_loop_tail :
    string -> Env.t -> string -> ty list -> Ast.form -> expression_result;
  compile_loop_tail_body :
    string -> Env.t -> string -> ty list -> Ast.form list -> expression_result;
  compile_loop :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_let :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
}

let compile_args_for compile_expr scope env arg_forms =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | form :: rest -> (
        match compile_expr scope env form with
        | Ok expr -> loop (expr :: acc) rest
        | Error _ as err -> err)
  in
  loop [] arg_forms

let located_pattern identity pattern =
  match identity with
  | None -> pattern
  | Some (node_id, location) -> Semantic_ir.PLocated (node_id, location, pattern)

let located_form_pattern form pattern =
  located_pattern (Destructure.source_identity form) pattern

let create ~compile_expr =
  let compile_args_for = compile_args_for compile_expr in
  let rec compile_vector scope env forms =
    match forms with
    | [] ->
        Ok
          (typed_ir (TVector (TVar "vector_element"))
             (Semantic_ir.Ident "Rrbvec.empty"))
    | first :: rest -> (
        match compile_expr scope env first with
        | Error _ as err -> err
        | Ok first_expr ->
            let rec loop acc = function
              | [] -> (
                  let expressions = List.rev acc in
                  let element_ty =
                    List.fold_left
                      (fun merged expr ->
                        Option.bind merged (fun ty ->
                            merge_branch_types ty expr.ty))
                      (Some first_expr.ty) expressions
                  in
                  let rec compile_dynamic values = function
                    | [] ->
                        Ok
                          (typed_ir
                             (Types.dynamic_constraint TUnknown)
                             (Semantic_ir.Apply
                                ( Semantic_ir.Ident
                                    "Lg_runtime.Runtime_dynamic.vector",
                                  [
                                    Semantic_ir.Apply
                                      ( Semantic_ir.Ident "Rrbvec.of_list",
                                        [ Semantic_ir.List (List.rev values) ]
                                      );
                                  ] )))
                    | form :: forms -> (
                        match
                          compile_expr scope env
                            (FList [ FSymbol "__lg_dynamic"; form ])
                        with
                        | Error _ as error -> error
                        | Ok value ->
                            compile_dynamic
                              (value.semantic_expr :: values)
                              forms)
                  in
                  match element_ty with
                  | None -> compile_dynamic [] forms
                  | Some element_ty when Types.is_dynamic element_ty ->
                      compile_dynamic [] forms
                  | Some element_ty
                    when not
                           (List.for_all
                              (fun expression ->
                                Types.equal element_ty expression.ty
                                ||
                                match (element_ty, expression.ty) with
                                | TNullable _, TNil -> true
                                | TNullable inner, TNullable actual
                                | TNullable inner, actual ->
                                    Types.assignable ~policy:Host_boundary
                                      ~expected:inner ~actual
                                | _ -> false)
                              expressions) ->
                      Ok
                        (typed_ir
                           (TTuple
                              (List.map
                                 (fun expression -> expression.ty)
                                 expressions))
                           (Semantic_ir.Tuple
                              (List.map
                                 (fun expression ->
                                   capability_storage_expression expression.ty
                                     expression.semantic_expr)
                                 expressions)))
                  | Some element_ty ->
                  let values =
                    expressions
                    |> List.map (fun expr ->
                           coerce_expression_to_type element_ty expr.ty
                             expr.semantic_expr)
                  in
                  Ok
                    (typed_ir (TVector element_ty)
                       (Semantic_ir.Apply
                              ( Semantic_ir.Ident "Rrbvec.of_list",
                                [ Semantic_ir.List values ] ))))
              | form :: rest -> (
                  match compile_expr scope env form with
                  | Error _ as err -> err
                  | Ok expr -> loop (expr :: acc) rest)
            in
            loop [ first_expr ] rest)
  and compile_map scope env pairs =
    if
      List.exists
        (fun (key, _value) -> match key with FKeyword _ -> false | _ -> true)
        pairs
    then
      let arguments =
        List.concat_map (fun (key, value) -> [ key; value ]) pairs
      in
      compile_expr scope env (FList (FSymbol "hash-map" :: arguments))
    else
    let compile_pair = function
      | FKeyword keyword, value_form -> (
          match compile_expr scope env value_form with
          | Ok value -> Ok (keyword, value_form, value)
          | Error _ as err -> err)
      | _ -> Error.error "map keys must be keywords"
    in
    let rec loop acc = function
      | [] ->
          let pairs = List.rev acc in
          if pairs = [] then
            Ok
                (typed_ir
                   (Types.dynamic_constraint TUnknown)
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.map",
                      [ Semantic_ir.List [] ] )))
          else
          let keyword_pairs =
            List.map (fun (keyword, _form, value) -> (keyword, value)) pairs
          in
          Result.bind
            (Structural_map.validate_unique_keywords keyword_pairs)
            (fun () ->
                 let dynamic_value = function
                   | ty when Types.is_dynamic ty -> true
                   | TNil | TNullable _ | TOcaml_app ("option", _) -> true
                   | _ -> false
                 in
                 if
                   List.exists
                     (fun (_keyword, _form, value) -> dynamic_value value.ty)
                     pairs
                 then
                   let rec compile_dynamic_pairs acc = function
                     | [] -> Ok (List.rev acc)
                     | (keyword, value_form, _value) :: rest -> (
                         match
                           compile_expr scope env
                             (FList [ FSymbol "__lg_dynamic"; value_form ])
                         with
                         | Error _ as error -> error
                         | Ok value ->
                             let key =
                               Semantic_ir.Apply
                                 ( Semantic_ir.Ident
                                     "Lg_runtime.Runtime_dynamic.keyword",
                                   [ Semantic_ir.String keyword ] )
                             in
                             compile_dynamic_pairs
                               (Semantic_ir.Tuple [ key; value.semantic_expr ]
                               :: acc)
                               rest)
                   in
                   compile_dynamic_pairs [] pairs
                   |> Result.map (fun entries ->
                        typed_ir
                          (Types.dynamic_constraint TUnknown)
                            (Semantic_ir.Apply
                             ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.map",
                                 [ Semantic_ir.List entries ] )))
                 else
                   let fields =
                     pairs
                     |> List.map (fun (keyword, _form, value) ->
                            make_field keyword value.ty)
                   in
                   let values =
                     List.map2
                       (fun field (_keyword, _form, value) ->
                         (field, value.semantic_expr))
                       fields pairs
                   in
                   Ok
                     {
                       (typed_ir (TRecord fields)
                          (Semantic_ir.Record
                             ( List.map
                                 (fun ((field : field), value) ->
                                   (field.ocaml_name, value))
                                 values,
                                None )))
                        with
                       record_values = Some values;
                     })
      | pair :: rest -> (
          match compile_pair pair with
          | Ok pair -> loop (pair :: acc) rest
          | Error _ as err -> err)
    in
    loop [] pairs
  and option_payload_type = function
    | TNullable payload_ty -> Ok payload_ty
    | TNil -> Ok TUnknown
    | TOcaml_app ("option", [ payload_ty ]) ->
        Ok (lg_metadata_type_for_ocaml_payload payload_ty)
    | TOcaml "option" -> Ok TUnknown
    | TUnknown | TVar _ -> Ok TUnknown
    | ty ->
        Error.error
          ("option binding requires an option value, got "
         ^ Types.source_name ty)
  and parse_option_binding form error_message =
    match form with
    | FVector [ ((FSymbol _ | FVector _ | FMap _) as pattern); option_form ] ->
        Ok (pattern, option_form)
    | _ -> Error.error error_message
  and compile_option_match ?(require_truthy = false) scope env pattern
      option_form compile_some compile_none branch_error =
    let payload_name = "__lg_option_value" in
    let compile_some_branch payload_ty =
      let payload = typed_ir payload_ty (Semantic_ir.Ident payload_name) in
      match Destructure.bind_pattern ~env payload pattern with
      | Error _ as error -> error
      | Ok bindings -> (
          let env_bindings =
            bindings
            |> List.map (fun (binding : Destructure.local_binding) ->
                   ( Names.scoped_key scope binding.source_name,
                     Types.binding binding.ocaml_name binding.ty ))
          in
          let some_env = Env.add_bindings env_bindings env in
          match compile_some some_env with
          | Error _ as error -> error
          | Ok expression ->
              let ir_bindings =
                bindings
                |> List.map (fun (binding : Destructure.local_binding) ->
                       ( located_pattern binding.identity
                           (Semantic_ir.PVar binding.ocaml_name),
                         binding.semantic_expr ))
              in
              if ir_bindings = [] then Ok expression
              else
                Ok
                  {
                    expression with
                    semantic_expr =
                      Semantic_ir.Let (ir_bindings, expression.semantic_expr);
                  })
    in
    let option_expression =
      match option_form with
      | FList [ FSymbol "first"; collection_form ] -> (
          match compile_expr scope env collection_form with
          | Error _ as error -> error
          | Ok collection -> (
              match Collection_capability.to_seq_expr env collection with
              | Error _ -> Error.error "first expects a seqable value"
              | Ok (inner, sequence) ->
                  Ok
                    (typed_ir (TNullable inner)
                       (Semantic_ir.Apply
                          ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.first_opt",
                            [ sequence ] )))))
      | _ -> compile_expr scope env option_form
    in
    match option_expression with
    | Error _ as err -> err
    | Ok option_expr when Types.is_dynamic option_expr.ty -> (
        match (compile_some_branch option_expr.ty, compile_none ()) with
        | (Error _ as error), _ -> error
        | _, (Error _ as error) -> error
        | Ok some_expr, Ok none_expr -> (
            match merge_branch_expressions some_expr none_expr with
            | None -> Error.error branch_error
            | Some (result_ty, some_code, none_code) ->
                Ok
                  (typed_ir result_ty
                     (Semantic_ir.Let
                        ( [
                            ( Semantic_ir.PVar payload_name,
                              option_expr.semantic_expr );
                          ],
                          Semantic_ir.If
                            ( (if require_truthy then
                                 truthiness_expression option_expr.ty
                                   (Semantic_ir.Ident payload_name)
                               else
                                 Semantic_ir.Apply
                                   ( Semantic_ir.Ident "not",
                                     [
                                       Semantic_ir.Apply
                                         ( Semantic_ir.Ident
                                             "Lg_runtime.Runtime_dynamic.is_nil",
                                           [ Semantic_ir.Ident payload_name ] );
                                     ] )),
                              some_code,
                              none_code ) )))))
    | Ok option_expr -> (
        match option_payload_type option_expr.ty with
        | Error _ as err -> err
        | Ok payload_ty -> (
            match (compile_some_branch payload_ty, compile_none ()) with
            | (Error _ as err), _ -> err
            | _, (Error _ as err) -> err
            | Ok some_expr, Ok none_expr -> (
                match merge_branch_expressions some_expr none_expr with
                | None -> Error.error branch_error
                | Some (result_ty, some_code, none_code) ->
                    let some_code =
                      if require_truthy then
                        Semantic_ir.If
                          ( truthiness_expression payload_ty
                              (Semantic_ir.Ident payload_name),
                            some_code,
                            none_code )
                      else some_code
                    in
                    Ok
                      (typed_ir result_ty
                         (Semantic_ir.Match
                            ( option_expr.semantic_expr,
                              [
                                ( Semantic_ir.PConstructor
                                    ( "Some",
                                      Some (Semantic_ir.PVar payload_name) ),
                                  some_code );
                                ( Semantic_ir.PConstructor ("None", None),
                                  none_code );
                              ] ))))))
  and compile_if_let scope env binding_form then_form else_form =
    match
      parse_option_binding binding_form
        "if-let requires [name option], then, and else"
    with
    | Error _ as err -> err
    | Ok (name, option_form) ->
        compile_option_match ~require_truthy:true scope env name option_form
          (fun some_env -> compile_expr scope some_env then_form)
          (fun () -> compile_expr scope env else_form)
          "if-let branches must have same type"
  and compile_if_some scope env binding_form then_form else_form =
    match
      parse_option_binding binding_form
        "if-some requires [name option], then, and else"
    with
    | Error _ as err -> err
    | Ok (name, option_form) ->
        compile_option_match scope env name option_form
          (fun some_env -> compile_expr scope some_env then_form)
          (fun () -> compile_expr scope env else_form)
          "if-some branches must have same type"
  and compile_when_binding ~require_truthy scope env binding_form body_forms
      error_prefix =
    match
      parse_option_binding binding_form
        (error_prefix ^ " requires [name option] and a body")
    with
    | Error _ as err -> err
    | Ok (name, option_form) ->
        compile_option_match ~require_truthy scope env name option_form
          (fun some_env ->
            compile_body scope some_env
              (error_prefix ^ " requires a body")
              body_forms)
          (fun () ->
            Ok (typed_ir TNil (Semantic_ir.Constructor ("None", None))))
          (error_prefix ^ " body cannot be made nullable")
  and compile_when_let scope env binding_form body_forms =
    compile_when_binding ~require_truthy:true scope env binding_form body_forms
      "when-let"
  and compile_when_some scope env binding_form body_forms =
    compile_when_binding ~require_truthy:false scope env binding_form body_forms
      "when-some"
  and compile_let_some scope env bindings_form then_form else_form =
    let rec parse_bindings acc = function
      | [] -> Ok (List.rev acc)
      | (FSymbol _ as pattern) :: option_form :: rest ->
          parse_bindings ((pattern, option_form) :: acc) rest
      | _ -> Error.error "let-some bindings require name/option pairs"
    in
    match bindings_form with
    | FVector forms -> (
        match parse_bindings [] forms with
        | Error _ as err -> err
        | Ok [] -> Error.error "let-some requires at least one binding"
        | Ok bindings -> (
            match compile_expr scope env else_form with
            | Error _ as err -> err
            | Ok else_expr ->
                let rec compile_bindings current_env = function
                  | [] -> compile_expr scope current_env then_form
                  | (pattern, option_form) :: rest ->
                      compile_option_match scope current_env pattern option_form
                        (fun some_env -> compile_bindings some_env rest)
                        (fun () -> Ok else_expr)
                        "let-some branches must have same type"
                in
                compile_bindings env bindings))
    | _ -> Error.error "let-some bindings must be a vector"
  and compile_if scope env condition then_form else_form =
    let compile_tuple_branch expected_types = function
      | FVector forms when List.length expected_types = List.length forms ->
          let rec compile values expected_types forms =
            match (expected_types, forms) with
            | [], [] ->
                Ok
                  (typed_ir (TTuple expected_types)
                     (Semantic_ir.Tuple (List.rev values)))
            | expected :: expected_rest, form :: form_rest -> (
                match compile_expr scope env form with
                | Error _ as error -> error
                | Ok value -> (
                    let expression =
                      if Types.is_dynamic expected then
                        pack_plain_dynamic_value value
                      else
                        match Types.next_seq_element expected with
                        | Some expected_inner -> (
                            match
                              Collection_capability.to_seq_expr env value
                            with
                            | Ok (actual_inner, sequence)
                              when Types.assignable ~policy:Host_boundary
                                     ~expected:expected_inner
                                     ~actual:actual_inner ->
                                Some sequence
                            | _ -> None)
                        | None -> (
                            match expected with
                            | TSeq expected_inner -> (
                                match
                                  Collection_capability.to_seq_expr env value
                                with
                                | Ok (actual_inner, sequence)
                                  when Types.assignable ~policy:Host_boundary
                                         ~expected:expected_inner
                                         ~actual:actual_inner ->
                                    Some sequence
                                | _ -> None)
                            | _
                              when Types.assignable ~policy:Host_boundary
                                     ~expected ~actual:value.ty ->
                                Some
                                  (coerce_expression_to_type expected value.ty
                                     value.semantic_expr)
                            | _ -> None)
                    in
                    match expression with
                    | None ->
                        Error.error "if tuple branches must have same type"
                    | Some expression ->
                        compile (expression :: values) expected_rest form_rest))
            | _ -> Error.error "if tuple branches must have same arity"
          in
          let result = compile [] expected_types forms in
          Result.map
            (fun expression -> { expression with ty = TTuple expected_types })
            result
      | _ -> Error.error "if tuple branch must be a vector"
    in
    match
      ( compile_expr scope env condition,
        compile_expr scope env then_form,
        compile_expr scope env else_form )
    with
    | (Error _ as err), _, _ -> err
    | _, (Error _ as err), _ -> err
    | _, _, (Error _ as err) -> err
    | Ok condition, Ok then_expr, Ok else_expr -> (
        let aligned =
          match (then_expr.ty, else_expr.ty) with
          | TTuple then_types, TTuple else_types
            when List.length then_types = List.length else_types
                 && (not (Types.equal then_expr.ty else_expr.ty))
                 &&
                 match (then_form, else_form) with
                    | FVector _, FVector _ -> true
                 | _ -> false ->
              let merged_types =
                List.map2
                  (fun left right ->
                    match merge_branch_types left right with
                    | Some ty -> Some ty
                    | None
                      when plain_dynamic_compatible_type left
                           && plain_dynamic_compatible_type right ->
                        Some (Types.dynamic_constraint TUnknown)
                    | None -> None)
                  then_types else_types
              in
              if List.for_all Option.is_some merged_types then
                let merged_types = List.map Option.get merged_types in
                match
                   ( compile_tuple_branch merged_types then_form,
                     compile_tuple_branch merged_types else_form )
                 with
                | (Error _ as error), _ -> error
                | _, (Error _ as error) -> error
                | Ok then_expr, Ok else_expr -> Ok (then_expr, else_expr)
              else Ok (then_expr, else_expr)
          | TTuple expected, ty
            when (not (Types.equal (TTuple expected) ty))
                 && match else_form with FVector _ -> true | _ -> false ->
              Result.map
                (fun else_expr -> (then_expr, else_expr))
                (compile_tuple_branch expected else_form)
          | ty, TTuple expected
            when (not (Types.equal ty (TTuple expected)))
                 && match then_form with FVector _ -> true | _ -> false ->
              Result.map
                (fun then_expr -> (then_expr, else_expr))
                (compile_tuple_branch expected then_form)
          | _ -> Ok (then_expr, else_expr)
        in
        match aligned with
        | Error _ as error -> error
        | Ok (then_expr, else_expr) -> (
        match condition_expression condition with
        | Error _ as err -> err
            | Ok condition_code -> (
            match merge_branch_expressions then_expr else_expr with
            | Some (result_ty, then_code, else_code) ->
              Ok
                (typed_ir result_ty
                   (Semantic_ir.If
                            (condition_code, then_code, else_code)))
            | None -> (
                match
                  ( Collection_capability.to_seq_expr env then_expr,
                    Collection_capability.to_seq_expr env else_expr )
                with
                    | ( Ok (then_inner, then_sequence),
                        Ok (else_inner, else_sequence) )
                  when Types.equal then_inner else_inner
                       || Types.equal then_inner TUnknown
                       || Types.equal else_inner TUnknown
                       || Types.is_dynamic then_inner
                       || Types.is_dynamic else_inner ->
                    let inner =
                      if Types.equal then_inner TUnknown then else_inner
                      else then_inner
                    in
                    Ok
                      (typed_ir (TSeq inner)
                         (Semantic_ir.If
                            (condition_code, then_sequence, else_sequence)))
                    | _ -> Error.error "if branches must have same type")))
        )
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
        match condition_expression condition with
        | Error _ as err -> err
        | Ok condition_code -> (
            match merge_branch_expressions then_expr else_expr with
            | Some (result_ty, then_code, else_code) ->
              Ok
                (typed_ir result_ty
                   (Semantic_ir.If
                      ( Semantic_ir.Apply
                          (Semantic_ir.Ident "not", [ condition_code ]),
                        then_code,
                        else_code )))
            | None -> Error.error "if-not branches must have same type"))
  and compile_when scope env condition body_forms =
    match
      ( compile_expr scope env condition,
        compile_body scope env "when body requires at least one form" body_forms
      )
    with
    | (Error _ as err), _ -> err
    | _, (Error _ as err) -> err
    | Ok condition, Ok body -> (
        match condition_expression condition with
        | Error _ as err -> err
        | Ok condition_code -> (
            let nil = typed_ir TNil (Semantic_ir.Constructor ("None", None)) in
            match merge_branch_expressions body nil with
            | Some (result_ty, body_code, nil_code) ->
                Ok
                  (typed_ir result_ty
                     (Semantic_ir.If (condition_code, body_code, nil_code)))
            | None -> Error.error "when body cannot be made nullable"))
  and compile_cond scope env clauses =
    let anonymous_fn = function
      | FList (FSymbol "fn" :: FVector _ :: _) -> true
      | _ -> false
    in
    let contextualize_fn = function
      | FList (FSymbol "fn" :: FVector params :: body) ->
          let params =
            List.map
              (function
                | FSymbol name ->
                    FList
                      [ FSymbol "__type-hint"; FSymbol "^Object"; FSymbol name ]
                | param -> param)
              params
          in
          FList (FSymbol "fn" :: FVector params :: body)
      | form -> form
    in
    let parse_pairs clauses =
      let rec loop acc = function
        | [] -> Ok (List.rev acc, FSymbol "nil")
        | [ _ ] -> Error.error "cond requires test/expression pairs"
        | [ FKeyword ":else"; else_form ] -> Ok (List.rev acc, else_form)
        | FKeyword ":else" :: _ -> Error.error "cond :else must be last"
        | test_form :: value_form :: rest ->
            loop ((test_form, value_form) :: acc) rest
      in
      loop [] clauses
    in
    let compile_test form =
      match compile_expr scope env form with
      | Error _ as err -> err
      | Ok test -> Ok test
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
        let pairs, else_form =
          let values = else_form :: List.map snd pairs in
          if List.for_all anonymous_fn values then
            ( List.map
                (fun (test, value) -> (test, contextualize_fn value))
                pairs,
              contextualize_fn else_form )
          else (pairs, else_form)
        in
        match (compile_pairs [] pairs, compile_expr scope env else_form) with
        | (Error _ as err), _ -> err
        | _, (Error _ as err) -> err
        | Ok pairs, Ok else_expr -> (
            let branch_types =
              else_expr.ty :: List.map (fun (_, value) -> value.ty) pairs
            in
            let static_result_ty =
              branch_types |> List.filter (fun ty -> not (Types.is_dynamic ty))
              |> function
              | [] -> None
              | first :: rest ->
                  List.fold_left
                    (fun merged ty ->
                      Option.bind merged (fun current ->
                          merge_branch_types current ty))
                    (Some first) rest
            in
            let merged_result_ty =
              List.fold_left
                (fun merged (_test, value) ->
                  Option.bind merged (fun ty -> merge_branch_types ty value.ty))
                (Some else_expr.ty) pairs
            in
            let result_ty =
              match static_result_ty with
              | Some (TVector _ as vector_ty) -> Some vector_ty
              | _ -> merged_result_ty
            in
            match result_ty with
            | Some result_ty ->
              let expression =
                List.fold_right
                  (fun (test, value) acc ->
                    Semantic_ir.If
                      ( truthiness_expression test.ty test.semantic_expr,
                        coerce_expression_to_type result_ty value.ty
                          value.semantic_expr,
                        acc ))
                  pairs
                  (coerce_expression_to_type result_ty else_expr.ty
                     else_expr.semantic_expr)
              in
              Ok (typed_ir result_ty expression)
            | None ->
                Error.error
                  ("cond branches must have same type: "
                  ^ String.concat ", " (List.map Types.source_name branch_types)
                  )))
  and compile_logical scope env operator forms =
    match forms with
    | [] -> (
        match operator with
        | `And -> Ok (typed_ir TBool (Semantic_ir.Bool true))
        | `Or ->
            Ok
              (typed_ir
                 (TOcaml_app ("option", [ TUnknown ]))
                 (Semantic_ir.Constructor ("None", None))))
    | _ -> (
        match compile_args_for scope env forms with
        | Error _ as err -> err
        | Ok expressions -> (
            let lower result_ty expressions =
              let rec lower_expressions = function
                | [] -> assert false
                | [ expression ] ->
                    coerce_expression_to_type result_ty expression.ty
                      expression.semantic_expr
                | expression :: rest ->
                    let value_name = "logical_value" in
                    let raw_value = Semantic_ir.Ident value_name in
                    let value =
                      coerce_expression_to_type result_ty expression.ty
                        raw_value
                    in
                    let condition =
                      truthiness_expression expression.ty raw_value
                    in
                    let next = lower_expressions rest in
                    let result =
                      match operator with
                      | `And -> Semantic_ir.If (condition, next, value)
                      | `Or -> Semantic_ir.If (condition, value, next)
                    in
                    Semantic_ir.Let
                      ( [
                          (Semantic_ir.PVar value_name, expression.semantic_expr);
                        ],
                        result )
              in
              Ok (typed_ir result_ty (lower_expressions expressions))
            in
            let result_ty =
              match expressions with
              | [] -> None
              | first :: rest ->
                  List.fold_left
                    (fun merged expression ->
                      Option.bind merged (fun ty ->
                          merge_branch_types ty expression.ty))
                    (Some first.ty) rest
            in
            let rec contains_dynamic = function
              | ty when Types.is_dynamic ty -> true
              | TNullable inner | TOcaml_app ("option", [ inner ]) ->
                  contains_dynamic inner
              | _ -> false
            in
            let lower_dynamic () =
              let dynamic_forms =
                List.map
                  (fun form -> FList [ FSymbol "__lg_dynamic"; form ])
                  forms
              in
              match compile_args_for scope env dynamic_forms with
              | Error _ as error -> error
              | Ok dynamic_expressions ->
                  lower (Types.dynamic_constraint TUnknown) dynamic_expressions
            in
            match result_ty with
            | Some result_ty when not (contains_dynamic result_ty) ->
                lower result_ty expressions
            | Some _ | None -> lower_dynamic ()))
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
            match form with
            | FInt value -> Ok (Semantic_ir.PInt value)
            | FString value | FKeyword value -> Ok (Semantic_ir.PString value)
            | FBool value -> Ok (Semantic_ir.PBool value)
            | _ -> Error.error "unsupported match pattern"
          else Error.error "match pattern type must match target"
    in
    let rec compile_pattern target_ty pattern =
      let result =
        match (target_ty, pattern) with
      | _, FSymbol "_" -> Ok (Semantic_ir.PAny, [])
        | ( target_ty,
            FList [ FSymbol "as"; inner_pattern; (FSymbol alias as alias_form) ]
          ) -> (
          match compile_pattern target_ty inner_pattern with
          | Error _ as err -> err
          | Ok (inner_pattern, bindings) ->
              let ocaml_name = Names.sanitize_name alias in
              let binding =
                ( Names.scoped_key scope alias,
                  Types.binding ocaml_name target_ty )
              in
              Ok
                ( located_form_pattern alias_form
                    (Semantic_ir.PAlias (inner_pattern, ocaml_name)),
                  bindings @ [ binding ] ))
      | target_ty, FList [ FSymbol "or"; left_form; right_form ] -> (
          match
              ( compile_pattern target_ty left_form,
                compile_pattern target_ty right_form )
          with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok (left, left_bindings), Ok (right, right_bindings) ->
              let binding_names bindings =
                bindings |> List.map fst |> List.sort_uniq String.compare
              in
                if binding_names left_bindings <> binding_names right_bindings
                then
                Error.error "or-pattern alternatives must bind the same names"
              else Ok (Semantic_ir.POr (left, right), left_bindings))
        | ( (TRecord fields | TNamed_record { fields; _ }),
            FList (FSymbol "record" :: field_patterns) ) ->
          let rec compile_fields compiled bindings seen = function
            | [] -> Ok (Semantic_ir.PRecord (List.rev compiled), bindings)
              | FList [ FSymbol field_name; field_pattern ] :: rest -> (
                let ocaml_name = Names.sanitize_name field_name in
                if List.mem ocaml_name seen then
                  Error.error ("duplicate record pattern field " ^ field_name)
                  else
                  match
                    List.find_opt
                      (fun (field : field) -> field.ocaml_name = ocaml_name)
                      fields
                  with
                    | None ->
                        Error.error
                          ("unknown record pattern field " ^ field_name)
                  | Some field -> (
                      match compile_pattern field.ty field_pattern with
                      | Error _ as err -> err
                      | Ok (pattern, field_bindings) ->
                          compile_fields
                            ((field.ocaml_name, pattern) :: compiled)
                              (bindings @ field_bindings)
                              (ocaml_name :: seen) rest))
            | _ -> Error.error "record pattern fields must be (name pattern)"
          in
          compile_fields [] [] [] field_patterns
      | _, FList (FSymbol "record" :: _) ->
          Error.error "record pattern expects a record target"
        | TTuple payload_tys, FList (FSymbol "tuple" :: payload_patterns) ->
          let rec compile_payloads patterns bindings = function
            | [], [] -> Ok (List.rev patterns, bindings)
            | payload_ty :: payload_tys, pattern :: payload_patterns -> (
                match
                  compile_pattern
                    (lg_metadata_type_for_ocaml_type payload_ty)
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
            |> Result.map (fun (patterns, bindings) ->
                (Semantic_ir.PTuple patterns, bindings))
      | target_ty, FSymbol name
        when is_ocaml_constructor_pattern_target target_ty name
             && is_constructor_name name ->
          Ok
            ( Semantic_ir.PConstructor
                (resolve_ocaml_constructor_target scope env name, None),
              [] )
      | target_ty, FList (FSymbol name :: payload_patterns)
        when is_ocaml_constructor_pattern_target target_ty name
             && is_constructor_name name -> (
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
                     | _ -> Some (Semantic_ir.PTuple patterns)
                   in
                   ( Semantic_ir.PConstructor
                       ( resolve_ocaml_constructor_target scope env name,
                         payload_pattern ),
                     bindings ))
          in
          match builtin_constructor_payloads with
          | Some payload_tys -> compile_constructor_payloads payload_tys
          | None -> (
              match lookup_binding scope env name with
              | Error _ ->
                  let opaque_payload_tys =
                    List.map (fun _ -> TUnknown) payload_patterns
                  in
                  compile_constructor_payloads opaque_payload_tys
              | Ok constructor -> (
                  match constructor.ty with
                  | TFn (payload_tys, return_ty)
                      when List.length payload_tys
                           = List.length payload_patterns -> (
                      let instantiated =
                        Types.instantiate_type ~templates:[ return_ty ]
                          ~actuals:[ target_ty ] constructor.ty
                      in
                        match instantiated with
                      | TFn (payload_tys, _) ->
                          compile_constructor_payloads payload_tys
                      | _ -> Error.error (name ^ " is not a constructor"))
                  | TFn _ -> Error.error "constructor pattern arity mismatch"
                  | _ -> Error.error (name ^ " is not a constructor"))))
      | _, FSymbol name ->
          let ocaml_name = Names.sanitize_name name in
          Ok
            ( Semantic_ir.PVar ocaml_name,
                [
                  ( Names.scoped_key scope name,
                    Types.binding ocaml_name target_ty );
                ] )
      | TInt, FInt value -> Ok (Semantic_ir.PInt value, [])
      | TString, FString value -> Ok (Semantic_ir.PString value, [])
      | TKeyword, FKeyword keyword -> Ok (Semantic_ir.PString keyword, [])
      | TBool, FBool value -> Ok (Semantic_ir.PBool value, [])
      | TList inner, FVector patterns ->
          compile_list_like_pattern inner patterns
      | TVector inner, FVector patterns ->
          compile_list_like_pattern inner patterns
      | _ -> (
          match pattern with
          | FInt _ | FString _ | FKeyword _ | FBool _ ->
                literal_pattern target_ty pattern
                |> Result.map (fun code -> (code, []))
            | FVector _ ->
                Error.error
                  "match collection pattern must match target collection"
          | _ -> Error.error "unsupported match pattern")
      in
      Result.map
        (fun (compiled, bindings) ->
          (located_form_pattern pattern compiled, bindings))
        result
    and compile_list_like_pattern inner patterns =
      let rec loop compiled_patterns bindings = function
        | [] -> Ok (List.rev compiled_patterns, bindings)
        | pattern :: rest -> (
            match compile_pattern inner pattern with
            | Error _ as err -> err
            | Ok (compiled_pattern, pattern_bindings) ->
                loop
                  (compiled_pattern :: compiled_patterns)
                  (bindings @ pattern_bindings)
                  rest)
      in
      loop [] [] patterns
      |> Result.map (fun (patterns, bindings) ->
          (Semantic_ir.PList patterns, bindings))
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
                | Ok guard when Types.equal guard.ty TBool ->
                    Ok (Some guard.semantic_expr)
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
              Semantic_ir.Apply
                (Semantic_ir.Ident "Rrbvec.to_list", [ target.semantic_expr ])
          | _ -> target.semantic_expr
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
        | Ok ((_, _, first_result) :: rest as clauses) -> (
            let result_ty =
              List.fold_left
                (fun merged (_, _, result) ->
                  Option.bind merged (fun ty -> merge_branch_types ty result.ty))
                (Some first_result.ty) rest
            in
            match result_ty with
            | Some result_ty ->
              Ok
                (typed_ir result_ty
                   (Semantic_ir.Match_guarded
                      ( target_expr,
                        clauses
                        |> List.map (fun (pattern, guard, result) ->
                               ( pattern,
                                 guard,
                                 coerce_expression_to_type result_ty result.ty
                                   result.semantic_expr )) )))
            | None -> Error.error "match branches must have same type"))
  and compile_body scope env empty_error forms =
    match forms with
    | [] -> Error.error empty_error
    | [ form ] -> compile_expr scope env form
    | form :: rest -> (
        match
          (compile_expr scope env form, compile_body scope env empty_error rest)
        with
        | (Error _ as err), _ -> err
        | _, (Error _ as err) -> err
        | Ok expr, Ok body ->
            Ok
              (typed_ir body.ty
                 (Semantic_ir.Sequence
                    [ expr.semantic_expr; body.semantic_expr ])))
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
      | FList
          (FSymbol "catch"
          :: FSymbol exception_type
          :: FSymbol binding
          :: body_forms) -> (
          match body_forms with
          | [] -> Error.error "catch requires a type, binding, and body"
          | _ ->
              let constructor =
                match exception_type with
                | "ClassCastException" -> "Invalid_argument"
                | _ -> exception_type
              in
              let pattern =
                FList
                  [
                    FSymbol "as";
                    FList [ FSymbol constructor; FSymbol "_" ];
                    FSymbol binding;
                  ]
              in
              let body =
                match body_forms with
                | [ body ] -> body
                | body_forms -> FList (FSymbol "do" :: body_forms)
              in
              Ok (pattern, body))
      | FList (FSymbol "catch" :: pattern :: body_forms) -> (
          match body_forms with
          | [] -> Error.error "catch requires a pattern and body"
          | [ body ] -> Ok (pattern, body)
          | body_forms -> Ok (pattern, FList (FSymbol "do" :: body_forms)))
      | FList [ FSymbol "catch" ] ->
          Error.error "catch requires a pattern and body"
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
      | TUnknown, ty when plain_dynamic_compatible_type ty ->
          Ok (Types.dynamic_constraint TUnknown)
      | ty, TUnknown when plain_dynamic_compatible_type ty ->
          Ok (Types.dynamic_constraint TUnknown)
      | _ -> (
      match merge_branch_types body_ty handlers_ty with
      | Some ty -> Ok ty
      | None -> Error.error "try body and handlers must have the same type")
    in
    match split_body [] forms with
    | Error _ as err -> err
    | Ok ([], _) -> Error.error "try requires a body"
    | Ok (body_forms, catch_forms) -> (
        match
          ( compile_body scope env "try requires a body" body_forms,
            parse_catches [] catch_forms )
        with
        | (Error _ as err), _ -> err
        | _, (Error _ as err) -> err
        | Ok body, Ok catches -> (
            let exception_name = "__lg_caught_exception" in
            let exception_binding =
              ( Names.scoped_key scope exception_name,
                Types.binding exception_name (TOcaml "exn") )
            in
            match
              compile_match scope
                (Env.add (fst exception_binding) (snd exception_binding) env)
                (FSymbol exception_name)
                (List.concat_map
                   (fun (pattern, handler) -> [ pattern; handler ])
                   catches)
            with
            | Error _ as err -> err
            | Ok handlers -> (
                match
                  ( Semantic_ir.unlocated handlers.semantic_expr,
                    compatible_try_type body.ty handlers.ty )
                with
                | _, (Error _ as err) -> err
                | Semantic_ir.Match_guarded (_, cases), Ok ty ->
                    let body_expression =
                      coerce_expression_to_type ty body.ty body.semantic_expr
                    in
                    let cases =
                      List.map
                        (fun (pattern, guard, expression) ->
                          ( pattern,
                            guard,
                            coerce_expression_to_type ty handlers.ty expression
                          ))
                        cases
                    in
                    Ok (typed_ir ty (Semantic_ir.Try (body_expression, cases)))
                | _, Ok _ ->
                    Error.error "internal error: malformed try handlers")))
  and loop_branch_type left right =
    match merge_branch_types left right with
    | Some ty -> Ok ty
    | None ->
        Error.error
          ("loop branches must have same type: " ^ Types.source_name left
         ^ " and " ^ Types.source_name right)
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
                if
                  Types.assignable ~policy:Host_boundary ~expected:expected_ty
                       ~actual:arg.ty
                  || Types.defer_to_ocaml ~expected:expected_ty ~actual:arg.ty
                then validate (index + 1) expected actual
                else
                  Error.error
                    ("recur argument " ^ string_of_int index ^ " must be "
                   ^ Types.source_name expected_ty)
            | _ -> Error.error "internal error: recur argument validation"
          in
          validate 1 param_tys args
          |> Result.map (fun () ->
                 typed_ir TUnknown
                   (Semantic_ir.Apply
                      ( Semantic_ir.Ident loop_name,
                        List.map2
                          (fun expected_ty arg ->
                            coerce_expression_to_type expected_ty arg.ty
                              arg.semantic_expr)
                          param_tys args )))
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
            match
              ( condition_expression condition,
                loop_branch_type then_expr.ty else_expr.ty )
            with
            | (Error _ as err), _ -> err
            | _, (Error _ as err) -> err
            | Ok condition_code, Ok result_ty ->
                Ok
                  (typed_ir result_ty
                     (Semantic_ir.If
                        ( condition_code,
                          then_expr.semantic_expr,
                          else_expr.semantic_expr )))))
    | FList [ FSymbol "if-not"; condition_form; then_form; else_form ] ->
        compile_loop_tail scope env loop_name param_tys
          (FList
             [
               FSymbol "if";
               FList [ FSymbol "not"; condition_form ];
               then_form;
               else_form;
             ])
    | FList
        [
          FSymbol ("if-let" as binding_form_name);
          binding_form;
          then_form;
          else_form;
        ]
    | FList
        [
          FSymbol ("if-some" as binding_form_name);
          binding_form;
          then_form;
          else_form;
        ] -> (
        let error_prefix = binding_form_name in
        match
          parse_option_binding binding_form
            (error_prefix ^ " requires [name option], then, and else")
        with
        | Error _ as error -> error
        | Ok (pattern, option_form) ->
            compile_option_match
              ~require_truthy:(binding_form_name = "if-let")
              scope env pattern option_form
              (fun some_env ->
                compile_loop_tail scope some_env loop_name param_tys then_form)
              (fun () ->
                compile_loop_tail scope env loop_name param_tys else_form)
              (error_prefix ^ " branches must have same type"))
    | FList (FSymbol "do" :: body_forms) ->
        compile_loop_tail_body scope env loop_name param_tys body_forms
    | FList (FSymbol "let" :: bindings :: body_forms) ->
        compile_let_tail scope env loop_name param_tys bindings body_forms
    | FList (FSymbol "cond" :: clauses) ->
        let rec expand = function
          | [] -> Ok (FSymbol "nil")
          | [ _ ] -> Error.error "cond requires test/expression pairs"
          | [ FKeyword ":else"; else_form ] -> Ok else_form
          | FKeyword ":else" :: _ -> Error.error "cond :else must be last"
          | test_form :: value_form :: rest ->
              Result.map
                (fun else_form ->
                  FList [ FSymbol "if"; test_form; value_form; else_form ])
                (expand rest)
        in
        Result.bind (expand clauses) (fun form ->
            compile_loop_tail scope env loop_name param_tys form)
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
                 (Semantic_ir.Sequence
                    [ expression.semantic_expr; body.semantic_expr ])))
  and compile_loop scope env bindings body_forms =
    match bindings with
    | FVector forms -> (
        if List.length forms mod 2 <> 0 then
          Error.error "loop bindings require an even number of forms"
        else
          let rec compile_bindings names identities values tys = function
            | [] ->
                Ok
                  ( List.rev names,
                    List.rev identities,
                    List.rev values,
                    List.rev tys )
            | (FSymbol name as name_form) :: value_form :: rest -> (
                if name = "_" || List.mem name names then
                  Error.error "loop binding names must be unique symbols"
                else
                  match compile_expr scope env value_form with
                  | Error _ as err -> err
                  | Ok value ->
                      let value_ty =
                        if Types.equal value.ty TNil then TNullable TUnknown
                        else value.ty
                      in
                      let value =
                        match Types.seqable_constraint_info value_ty with
                        | None -> Ok value
                        | Some (_, element_ty, _) -> (
                            match
                              Collection_capability.to_seq_expr env value
                            with
                            | Error _ as error -> error
                            | Ok (_, sequence) ->
                                let element_ty =
                                  match element_ty with
                                  | TUnknown | TVar _ ->
                                      Types.dynamic_constraint TUnknown
                                  | ty -> ty
                                in
                                Ok (typed_ir (TSeq element_ty) sequence))
                      in
                      Result.bind value (fun value ->
                          compile_bindings (name :: names)
                            (Destructure.source_identity name_form :: identities)
                            (value :: values) (value_ty :: tys) rest))
            | _ -> Error.error "loop binding names must be symbols"
          in
          match compile_bindings [] [] [] [] forms with
          | Error _ as err -> err
          | Ok (names, identities, values, param_tys) -> (
              let loop_name = "loop__" in
              let loop_env =
                List.fold_left2
                  (fun env name ty ->
                    Env.add
                      (Names.scoped_key scope name)
                      (Types.binding (Names.sanitize_name name) ty)
                      env)
                  env names param_tys
              in
              match
                 compile_loop_tail_body scope loop_env loop_name param_tys
                   body_forms
               with
              | Error _ as err -> err
              | Ok body ->
                  let params =
                    List.map2
                      (fun name identity ->
                        located_pattern identity
                          (Semantic_ir.PVar (Names.sanitize_name name)))
                      names identities
                  in
                  Ok
                    (typed_ir body.ty
                       (Semantic_ir.LetRec
                          ( loop_name,
                            params,
                            body.semantic_expr,
                            List.map (fun value -> value.semantic_expr) values
                          )))))
    | _ -> Error.error "loop bindings must be a vector"
  and compile_let_tail scope env loop_name param_tys bindings body_forms =
    compile_let_with_body
      (fun scope env forms ->
        compile_loop_tail_body scope env loop_name param_tys forms)
      scope env bindings body_forms
  and compile_let scope env bindings body_forms =
    compile_let_with_body
      (fun scope env forms ->
        compile_body scope env "let body requires at least one form" forms)
      scope env bindings body_forms
  and compile_let_with_body compile_let_body scope env bindings body_forms =
    let rec capability_pattern name ty =
      match Types.protocol_constraint_info ty with
      | Some (protocol_id, _, value_ty) ->
          Semantic_ir.PTuple
            [
              Semantic_ir.PVar (Types.protocol_witness_name name protocol_id);
              capability_pattern name value_ty;
            ]
      | None -> (
          match ty with
          | TOcaml_app (constraint_name, [ _element_ty; value_ty ])
            when constraint_name = Types.seqable_constraint_name
                 || constraint_name = Types.optional_seqable_constraint_name
                 || constraint_name = Types.optional_sequential_constraint_name
            ->
              Semantic_ir.PTuple
                [
                  Semantic_ir.PVar
                    (if constraint_name = Types.seqable_constraint_name then
                       name ^ "__seq"
                     else name ^ "__seq_optional");
                  capability_pattern name value_ty;
                ]
          | _ -> Semantic_ir.PVar name)
    in
    match bindings with
    | FVector forms ->
        if List.length forms mod 2 <> 0 then
          Error.error "let bindings require an even number of forms"
        else
          let rec bind env ir_bindings = function
            | [] -> (
                match compile_let_body scope env body_forms with
                | Error _ as err -> err
                | Ok body ->
                    Ok
                      {
                        (typed_ir body.ty
                           (Semantic_ir.Let
                              (List.rev ir_bindings, body.semantic_expr)))
                        with
                        return_param_index = body.return_param_index;
                      })
            | pattern :: value_form :: rest -> (
                let value =
                  match (pattern, value_form) with
                  | FSymbol _, FList [ FSymbol "volatile!"; FSymbol "nil" ] ->
                      Ok
                        (typed_ir
                           (TOcaml_app
                              ( "Lg_runtime.Runtime_slot.t",
                                [ Types.dynamic_constraint TUnknown ] ))
                           (Semantic_ir.Apply
                              ( Semantic_ir.Ident "Lg_runtime.Runtime_slot.empty",
                                [ Semantic_ir.Unit ] )))
                  | _ -> compile_expr scope env value_form
                in
                match value with
                | Error _ as err -> err
                | Ok value -> (
                    match Destructure.bind_pattern ~env value pattern with
                    | Error _ as err -> err
                    | Ok bindings ->
                        let env_bindings =
                          match (pattern, bindings) with
                          | FSymbol name, [ binding ] ->
                              let constant_keyword =
                                match value_form with
                                | FKeyword keyword -> Some keyword
                                | _ -> None
                              in
                              [
                                ( Names.scoped_key scope name,
                                  Types.binding
                                    ?return_param_index:value.return_param_index
                                    ?constant_keyword
                                    binding.ocaml_name binding.ty );
                              ]
                          | _ ->
                              bindings
                              |> List.map
                                   (fun (binding : Destructure.local_binding) ->
                                     ( Names.scoped_key scope binding.source_name,
                                       Types.binding binding.ocaml_name
                                         binding.ty ))
                        in
                        let ir_bindings =
                          match (pattern, bindings) with
                          | FSymbol _, [ binding ]
                            when Option.is_some
                                   (Types.protocol_constraint_info binding.ty)
                                 || Option.is_some
                                      (Types.seqable_constraint_element
                                         binding.ty) ->
                              ( located_pattern binding.identity
                                  (capability_pattern binding.ocaml_name
                                     binding.ty),
                                value.semantic_expr )
                              :: ir_bindings
                          | FSymbol "_", _ ->
                              ( located_form_pattern pattern Semantic_ir.PAny,
                                value.semantic_expr )
                              :: ir_bindings
                          | _ ->
                              bindings
                              |> List.fold_left
                                   (fun acc
                                        (binding : Destructure.local_binding) ->
                                     ( located_pattern binding.identity
                                         (Semantic_ir.PVar binding.ocaml_name),
                                       binding.semantic_expr )
                                     :: acc)
                                   ir_bindings
                        in
                        bind
                          (Env.add_bindings env_bindings env)
                          ir_bindings rest))
            | [ _ ] ->
                Error.error "let bindings require an even number of forms"
          in
          bind env [] forms
    | _ -> Error.error "let bindings must be a vector"
  in
  {
    compile_vector;
    compile_map;
    compile_if;
    compile_if_not;
    compile_if_let;
    compile_if_some;
    compile_when_let;
    compile_when_some;
    compile_let_some;
    compile_when;
    compile_cond;
    compile_match;
    compile_logical;
    compile_body;
    compile_try;
    loop_branch_type;
    compile_recur;
    compile_loop_tail;
    compile_loop_tail_body;
    compile_loop;
    compile_let;
  }
