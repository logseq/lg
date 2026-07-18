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

let symbol_predicate_narrowings condition =
  let has_source_name name expected =
    name = expected || String.ends_with ~suffix:("/" ^ expected) name
  in
  let rec collect narrowed = function
    | FList (FSymbol name :: conditions) when has_source_name name "and" ->
        List.fold_left collect narrowed conditions
    | FList [ FSymbol predicate; FSymbol name ]
      when has_source_name predicate "symbol?" ->
        if List.mem name narrowed then narrowed else name :: narrowed
    | _ -> narrowed
  in
  collect [] condition |> List.rev

let narrow_symbol_predicates scope env condition body =
  symbol_predicate_narrowings condition
  |> fun names ->
  let names =
    List.filter
      (fun name ->
        match Resolver.lookup_binding scope env name with
        | Ok (binding : Types.binding) ->
            Types.constraint_value_type binding.ty |> Types.is_dynamic
        | Error _ -> false)
      names
  in
  let body =
    List.fold_right
      (fun name body ->
        let narrowed =
          FList
            [
              FSymbol "__lg_dynamic-narrow";
              FList [ FSymbol "quote"; FSymbol "__lg_symbol_type" ];
              FSymbol name;
            ]
        in
        FList
          [ FSymbol "let"; FVector [ FSymbol name; narrowed ]; body ])
      names body
  in
  let rec truthy_symbols = function
    | FSymbol name -> [ name ]
    | FList [ FSymbol predicate; FSymbol name ]
      when predicate = "some?"
           || String.ends_with ~suffix:"/some?" predicate ->
        [ name ]
    | FList (FSymbol name :: forms)
      when name = "and" || String.ends_with ~suffix:"/and" name ->
        List.concat_map truthy_symbols forms
    | _ -> []
  in
  let nullable_names =
    truthy_symbols condition
    |> List.sort_uniq String.compare
    |> List.filter (fun name ->
           match Resolver.lookup_binding scope env name with
           | Ok (binding : Types.binding) -> (
               match Types.constraint_value_type binding.ty with
               | TNullable _ | TOcaml_app ("option", [ _ ]) -> true
               | _ -> false)
           | Error _ -> false)
  in
  List.fold_right
    (fun name body ->
      FList
        [
          FSymbol "let";
          FVector
            [
              FSymbol name;
              FList [ FSymbol "__lg_nullable-value"; FSymbol name ];
            ];
          body;
        ])
    nullable_names body

let rec false_nil_predicate_names = function
  | FList [ FSymbol predicate; FSymbol name ]
    when predicate = "nil?"
         || String.ends_with ~suffix:"/nil?" predicate ->
      [ name ]
  | FList (FSymbol name :: conditions)
    when name = "or" || String.ends_with ~suffix:"/or" name ->
      List.concat_map false_nil_predicate_names conditions
  | _ -> []

let narrow_non_nil_name scope env name body =
  match Resolver.lookup_binding scope env name with
  | Ok (binding : Types.binding) -> (
      match Types.constraint_value_type binding.ty with
      | TNullable _ | TOcaml_app ("option", [ _ ]) ->
          FList
            [
              FSymbol "let";
              FVector
                [
                  FSymbol name;
                  FList [ FSymbol "__lg_nullable-value"; FSymbol name ];
                ];
              body;
            ]
      | _ -> body)
  | Error _ -> body

let narrow_false_nil_predicates scope env condition body =
  let names =
    match condition with
    | FSymbol alias -> (
        match Resolver.lookup_binding scope env alias with
        | Ok (binding : Types.binding) -> binding.false_non_nil_names
        | Error _ -> [])
    | condition -> false_nil_predicate_names condition
  in
  names
  |> List.sort_uniq String.compare
  |> List.fold_left
       (fun body name -> narrow_non_nil_name scope env name body)
       body

let create ~compile_expr ~dynamic_unpack ~pack_dynamic_value =
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
                        let values =
                          List.map
                            (fun (keyword, _form, value) ->
                              (make_field keyword value.ty, value.semantic_expr))
                            pairs
                        in
                        {
                          (typed_ir
                             (Types.dynamic_constraint TUnknown)
                             (Semantic_ir.Apply
                                ( Semantic_ir.Ident
                                    "Lg_runtime.Runtime_dynamic.map",
                                  [ Semantic_ir.List entries ] )))
                          with
                          record_values = Some values;
                        })
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
    | Ok option_expr
      when (match option_expr.ty with
           | TNullable _ | TNil | TOcaml "option"
           | TOcaml_app ("option", [ _ ]) | TUnknown | TVar _ ->
               false
           | _ -> true) -> (
        match (compile_some_branch option_expr.ty, compile_none ()) with
        | (Error _ as error), _ -> error
        | _, (Error _ as error) -> error
        | Ok some_expr, Ok none_expr -> (
            match merge_branch_expressions some_expr none_expr with
            | None -> Error.error branch_error
            | Some (result_ty, some_code, none_code) ->
                let body =
                  if require_truthy then
                    Semantic_ir.If
                      ( truthiness_expression option_expr.ty
                          (Semantic_ir.Ident payload_name),
                        some_code,
                        none_code )
                  else some_code
                in
                Ok
                  (typed_ir result_ty
                     (Semantic_ir.Let
                        ( [
                            ( Semantic_ir.PVar payload_name,
                              option_expr.semantic_expr );
                          ],
                          body )))))
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
    let then_form = narrow_symbol_predicates scope env condition then_form in
    let else_form =
      narrow_false_nil_predicates scope env condition else_form
    in
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
                    | _ ->
                        let describe_type = function
                          | TRecord fields ->
                              "map {"
                              ^ String.concat ", "
                                  (List.map
                                     (fun (field : field) -> field.keyword)
                                     fields)
                              ^ "}"
                          | TNamed_record record -> record.type_name
                          | ty -> Types.source_name ty
                        in
                        Error.error
                          ("if branches must have same type: "
                          ^ describe_type then_expr.ty ^ " and "
                          ^ describe_type else_expr.ty))))
        )
  and compile_if_not scope env condition then_form else_form =
    let else_form = narrow_symbol_predicates scope env condition else_form in
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
    let body_forms =
      match body_forms with
      | [] -> []
      | [ body ] -> [ narrow_symbol_predicates scope env condition body ]
      | forms ->
          [
            narrow_symbol_predicates scope env condition
              (FList (FSymbol "do" :: forms));
          ]
    in
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
    let forms =
      match operator with
      | `Or ->
          let rec narrow_later conditions = function
            | [] -> []
            | form :: rest ->
                let narrowed =
                  List.fold_left
                    (fun body condition ->
                      narrow_false_nil_predicates scope env condition body)
                    form conditions
                in
                narrowed :: narrow_later (form :: conditions) rest
          in
          narrow_later [] forms
      | `And ->
          let rec narrow_later conditions = function
            | [] -> []
            | form :: rest ->
                let narrowed =
                  match conditions with
                  | [] -> form
                  | _ ->
                      narrow_symbol_predicates scope env
                        (FList
                           (FSymbol "and" :: List.rev conditions))
                        form
                in
                narrowed :: narrow_later (form :: conditions) rest
          in
          narrow_later [] forms
    in
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
                    let condition =
                      truthiness_expression expression.ty raw_value
                    in
                    let next = lower_expressions rest in
                    let value =
                      match (operator, expression.ty) with
                      | `Or, TNil -> next
                      | `Or, (TNullable payload_ty
                             | TOcaml_app ("option", [ payload_ty ])) ->
                          coerce_expression_to_type result_ty payload_ty
                            (Semantic_ir.Apply
                               ( Semantic_ir.Ident "Option.get",
                                 [ raw_value ] ))
                      | _ ->
                          coerce_expression_to_type result_ty expression.ty
                            raw_value
                    in
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
              let last_index = List.length expressions - 1 in
              expressions
              |> List.mapi (fun index expression ->
                     let last = index = last_index in
                     match (operator, last, expression.ty) with
                     | ( `Or,
                         false,
                         (TNullable payload_ty
                         | TOcaml_app ("option", [ payload_ty ])) ) ->
                         Some payload_ty
                     | _ -> Some expression.ty)
              |> List.filter_map Fun.id
              |> function
              | [] -> None
              | first :: rest ->
                  List.fold_left
                    (fun merged ty ->
                      Option.bind merged (fun merged ->
                          merge_branch_types merged ty))
                    (Some first) rest
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
                if exception_type = "js/Error" then
                  FList [ FSymbol "as"; FSymbol "_"; FSymbol binding ]
                else
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
                   ^ Types.source_name expected_ty ^ ", got "
                   ^ Types.source_name arg.ty)
            | _ -> Error.error "internal error: recur argument validation"
          in
          let adapt_argument expected_ty (arg : typed_expr) =
            match (expected_ty, arg.ty) with
            | ( (TNullable expected_inner
                | TOcaml_app ("option", [ expected_inner ])),
                (TNullable actual_inner
                | TOcaml_app ("option", [ actual_inner ])) )
              when Types.is_dynamic actual_inner
                   && not (Types.is_dynamic expected_inner) ->
                let payload_name = "__lg_recur_optional_payload" in
                Result.map
                  (fun payload ->
                    Semantic_ir.Match
                      ( arg.semantic_expr,
                        [
                          ( Semantic_ir.PConstructor ("None", None),
                            Semantic_ir.Constructor ("None", None) );
                          ( Semantic_ir.PConstructor
                              ("Some", Some (Semantic_ir.PVar payload_name)),
                            Semantic_ir.Constructor ("Some", Some payload) );
                        ] ))
                  (dynamic_unpack env expected_inner
                     (Semantic_ir.Ident payload_name))
            | _ ->
                Ok
                  (coerce_expression_to_type expected_ty arg.ty
                     arg.semantic_expr)
          in
          Result.bind (validate 1 param_tys args) (fun () ->
                 let rec adapt adapted expected args =
                   match (expected, args) with
                   | [], [] -> Ok (List.rev adapted)
                   | expected_ty :: expected, arg :: args ->
                       Result.bind (adapt_argument expected_ty arg)
                         (fun expression ->
                           adapt
                             (capability_storage_expression expected_ty
                                expression
                             :: adapted)
                             expected args)
                   | _ -> Error.error "internal error: recur adaptation"
                 in
                 adapt [] param_tys args)
          |> Result.map (fun arguments ->
                 let return_ty =
                   match Env.find_opt loop_name env with
                   | Some { ty = TFn (_, return_ty); _ } -> return_ty
                   | _ -> TUnknown
                 in
                 typed_ir return_ty
                   (Semantic_ir.Apply
                      ( Semantic_ir.Ident loop_name,
                        arguments )))
  and compile_loop_tail scope env loop_name param_tys = function
    | FList (FSymbol "recur" :: arg_forms) ->
        compile_recur scope env loop_name param_tys arg_forms
    | FList [ FSymbol "if"; condition_form; then_form; else_form ] -> (
        let then_form =
          narrow_symbol_predicates scope env condition_form then_form
        in
        let else_form =
          narrow_false_nil_predicates scope env condition_form else_form
        in
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
                let coerce_branch branch =
                  if Types.equal branch.ty TUnknown then branch.semantic_expr
                  else
                    coerce_expression_to_type result_ty branch.ty
                      branch.semantic_expr
                in
                Ok
                  (typed_ir result_ty
                     (Semantic_ir.If
                        ( condition_code,
                          coerce_branch then_expr,
                          coerce_branch else_expr )))))
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
    | (FList (FSymbol name :: args) as form) -> (
        match Env.find_macro ~scope name env with
        | None -> compile_expr scope env form
        | Some definition ->
            Result.bind
              (Macro_expander.expand ~compiler_env:env definition args)
              (fun expanded ->
                compile_loop_tail scope env loop_name param_tys expanded))
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
        let forms = Destructure.normalize_binding_type_hints forms in
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
                      let binding_ty =
                        if Types.equal value.ty TNil then TNullable TUnknown
                        else value.ty
                      in
                      let value =
                        match Types.seqable_constraint_info binding_ty with
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
                          let binding_ty =
                            match Types.seqable_constraint_info binding_ty with
                            | Some _ -> value.ty
                            | None -> binding_ty
                          in
                          compile_bindings (name :: names)
                            (Destructure.source_identity name_form :: identities)
                            (value :: values) (binding_ty :: tys) rest))
            | _ -> Error.error "loop binding names must be symbols"
          in
          match compile_bindings [] [] [] [] forms with
          | Error _ as err -> err
          | Ok (names, identities, values, param_tys) -> (
              let inferred_param_tys =
                let local_tys = List.combine names param_tys in
                let rec collect_aliases aliases = function
                  | FList
                      (FSymbol ("let" | "let*") :: FVector bindings
                      :: body_forms) ->
                      let rec binding_aliases aliases = function
                        | FSymbol name :: value :: rest ->
                            binding_aliases ((name, value) :: aliases) rest
                        | _ :: _ :: rest -> binding_aliases aliases rest
                        | _ -> aliases
                      in
                      List.fold_left collect_aliases
                        (binding_aliases aliases bindings)
                        body_forms
                  | FList forms | FVector forms ->
                      List.fold_left collect_aliases aliases forms
                  | FMap pairs ->
                      List.fold_left
                        (fun aliases (key, value) ->
                          collect_aliases
                            (collect_aliases aliases key)
                            value)
                        aliases pairs
                  | _ -> aliases
                in
                let aliases =
                  List.fold_left collect_aliases [] body_forms
                in
                let rec form_type = function
                  | FSymbol name -> (
                      match List.assoc_opt name local_tys with
                      | Some ty -> ty
                      | None -> (
                          match List.assoc_opt name aliases with
                          | Some value -> form_type value
                          | None -> (
                              match Resolver.lookup_binding scope env name with
                              | Ok (binding : Types.binding) -> binding.ty
                              | Error _ -> TUnknown)))
                  | FList (FSymbol name :: _reducer :: init :: _)
                    when name = "reduce"
                         || String.ends_with ~suffix:"/reduce" name ->
                      form_type init
                  | FList (FSymbol name :: arguments) -> (
                      match Resolver.lookup_binding scope env name with
                      | Ok { ty = TFn (parameter_tys, return_ty); _ }
                        when List.length parameter_tys
                             = List.length arguments ->
                          Types.instantiate_type ~templates:parameter_tys
                            ~actuals:(List.map form_type arguments)
                            return_ty
                      | Ok { ty = TOverloaded_fn arities; _ } -> (
                          match
                            List.find_opt
                              (fun arity ->
                                List.length arity.fixed_params
                                = List.length arguments)
                              arities
                          with
                          | Some arity ->
                              Types.instantiate_type
                                ~templates:arity.fixed_params
                                ~actuals:(List.map form_type arguments)
                                arity.return_ty
                          | None -> TUnknown)
                      | Ok _ | Error _ -> TUnknown)
                  | FList _ | FVector _ | FMap _ | FCoreSymbol _
                  | FKeyword _ | FString _ | FRegex _ | FInt _ | FFloat _
                  | FChar _ | FBool _ ->
                      TUnknown
                in
                let rec recur_arguments = function
                  | FList (FSymbol "recur" :: arguments) -> [ arguments ]
                  | FList (FSymbol ("loop" | "fn") :: _) -> []
                  | FList forms | FVector forms ->
                      List.concat_map recur_arguments forms
                  | FMap pairs ->
                      List.concat_map
                        (fun (key, value) ->
                          recur_arguments key @ recur_arguments value)
                        pairs
                  | _ -> []
                in
                let recurs = List.concat_map recur_arguments body_forms in
                List.mapi
                  (fun index fallback ->
                    let recur_types =
                      List.filter_map
                        (fun arguments ->
                          Option.map form_type
                            (List.nth_opt arguments index))
                        recurs
                    in
                    let sequence_element = function
                      | TList inner | TVector inner | TSeq inner -> Some inner
                      | TOcaml_app (name, [ inner ])
                        when name = Types.next_seq_type_name ->
                          Some inner
                      | _ -> None
                    in
                    let merge_sequence_inner current_inner actual_inner =
                      match actual_inner with
                      | TUnknown | TVar _ ->
                          Types.dynamic_constraint current_inner
                      | _ -> (
                          match
                            merge_branch_types current_inner actual_inner
                          with
                          | Some inner -> inner
                          | None -> Types.dynamic_constraint TUnknown)
                    in
                    let widen_container current actual =
                      match (current, actual) with
                      | ( (TList current_inner | TVector current_inner),
                          TSeq actual_inner )
                      | ( TSeq current_inner,
                          (TList actual_inner | TVector actual_inner) ) ->
                          let inner =
                            merge_sequence_inner current_inner actual_inner
                          in
                          TSeq inner
                      | ( (TList current_inner | TVector current_inner),
                          TOcaml_app (name, [ actual_inner ]) )
                        when name = Types.next_seq_type_name ->
                          let inner =
                            merge_sequence_inner current_inner actual_inner
                          in
                          TSeq inner
                      | TSeq current_inner, TSeq actual_inner ->
                          TSeq
                            (merge_sequence_inner current_inner actual_inner)
                      | current, actual
                        when Option.is_some (sequence_element current)
                             && (match actual with
                                | TUnknown | TVar _ -> true
                                | _ -> false) ->
                          current
                      | current, _ -> current
                    in
                    let fallback =
                      List.fold_left widen_container fallback recur_types
                    in
                    let becomes_nullable =
                      List.exists
                        (function
                          | TNullable _ | TOcaml_app ("option", [ _ ]) -> true
                          | _ -> false)
                        recur_types
                    in
                    if
                      becomes_nullable
                      && not
                           (match fallback with
                           | TNullable _ | TOcaml_app ("option", [ _ ]) -> true
                           | _ -> false)
                    then TNullable fallback
                    else fallback)
                  param_tys
              in
              let param_tys =
                let rec contains_protocol ty =
                  Option.is_some (Types.protocol_constraint_info ty)
                  ||
                  match Types.dynamic_constraint_info ty with
                  | Some capability -> contains_protocol capability
                  | None -> (
                      match ty with
                      | TNullable inner | TArray inner | TRef inner
                      | TList inner | TVector inner | TSet inner | TSeq inner
                      | TOcaml_app (_, [ inner ]) ->
                          contains_protocol inner
                      | TOcaml_app (_, arguments) | TTuple arguments ->
                          List.exists contains_protocol arguments
                      | TFn (parameters, return_ty) ->
                          List.exists contains_protocol
                            (return_ty :: parameters)
                      | TOverloaded_fn arities ->
                          List.exists
                            (fun arity ->
                              List.exists contains_protocol
                                (arity.return_ty :: arity.fixed_params))
                            arities
                      | TRecord fields | TNamed_record { fields; _ } ->
                          List.exists
                            (fun (field : field) ->
                              contains_protocol field.ty)
                            fields
                      | TInt | TFloat | TChar | TString | TRegex | TMap_keys
                      | TSymbol | TKeyword | TBool | TUnit | TNil | TUnknown
                      | TVar _ | TOcaml _ ->
                          false)
                in
                let lookup_function_ty =
                  Expression_support.lookup_function_ty scope env
                in
                let lookup_protocol_constraint =
                  Protocol.constraint_type scope env
                in
                let lookup_dynamic_key_record_type =
                  Expression_support.dynamic_key_record_type env
                in
                let resolve_named_record =
                  Function_elaborator.infer_named_record scope env
                in
                match
                  Type_inference.infer_params ~lookup_function_ty
                    ~lookup_protocol_constraint
                    ~lookup_dynamic_key_record_type ~resolve_named_record
                    (List.combine names inferred_param_tys)
                    body_forms
                with
                | Error _ -> inferred_param_tys
                | Ok inferred ->
                    List.map2
                      (fun name fallback ->
                        match List.assoc_opt name inferred with
                        | Some ty when contains_protocol ty -> ty
                        | Some _ | None -> fallback)
                      names inferred_param_tys
              in
              let param_tys =
                List.map
                  (function
                    | TSeq (TUnknown | TVar _) ->
                        TSeq (Types.dynamic_constraint TUnknown)
                    | ty -> ty)
                  param_tys
              in
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
                  let adapt_initial param_ty (value : typed_expr) =
                    match (param_ty, value.ty) with
                    | ( TSeq target_inner,
                        (TList source_inner | TVector source_inner) )
                      when Types.is_dynamic target_inner
                           && not (Types.is_dynamic source_inner) ->
                        let item_name = "__lg_loop_initial_item" in
                        let item =
                          typed_ir source_inner
                            (Semantic_ir.Ident item_name)
                        in
                        Result.map
                          (fun packed ->
                            let sequence =
                              match value.ty with
                              | TList _ ->
                                  Semantic_ir.Apply
                                    ( Semantic_ir.Ident
                                        "Lg_runtime.Runtime_seq.of_list",
                                      [ value.semantic_expr ] )
                              | TVector _ ->
                                  Semantic_ir.Apply
                                    ( Semantic_ir.Ident
                                        "Lg_runtime.Runtime_seq.of_vector",
                                      [ value.semantic_expr ] )
                              | _ -> assert false
                            in
                            Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_seq.map",
                                [
                                  Semantic_ir.Fun
                                    ([ Semantic_ir.PVar item_name ], packed);
                                  sequence;
                                ] ))
                          (pack_dynamic_value env target_inner item)
                    | _ ->
                        Ok
                          (coerce_expression_to_type param_ty value.ty
                             value.semantic_expr
                          |> capability_storage_expression param_ty)
                  in
                  let rec adapt_initials adapted param_tys values =
                    match (param_tys, values) with
                    | [], [] -> Ok (List.rev adapted)
                    | param_ty :: param_tys, value :: values ->
                        Result.bind (adapt_initial param_ty value)
                          (fun value ->
                            adapt_initials (value :: adapted) param_tys values)
                    | _ -> Error.error "internal error: loop initial values"
                  in
                  Result.bind (adapt_initials [] param_tys values)
                    (fun initial_values ->
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
                            initial_values
                          ))))))
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
    let rec remaining_binding_names names = function
      | pattern :: _value :: rest ->
          remaining_binding_names
            (List.rev_append (Destructure.pattern_names pattern) names)
            rest
      | _ -> List.rev names
    in
    let rec remaining_value_forms values = function
      | _pattern :: value :: rest ->
          remaining_value_forms (value :: values) rest
      | _ -> List.rev values
    in
    let inferred_binding_type env name rest =
      let names = name :: remaining_binding_names [] rest in
      let params =
        names
        |> List.sort_uniq String.compare
        |> List.map (fun name -> (name, TUnknown))
      in
      let forms = remaining_value_forms [] rest @ body_forms in
      let lookup_function_ty = Expression_support.lookup_function_ty scope env in
      let lookup_protocol_constraint = Protocol.constraint_type scope env in
      let lookup_dynamic_key_record_type =
        Expression_support.dynamic_key_record_type env
      in
      let resolve_named_record =
        Function_elaborator.infer_named_record scope env
      in
      match
        Type_inference.infer_params ~lookup_function_ty
          ~lookup_protocol_constraint ~lookup_dynamic_key_record_type
          ~resolve_named_record params forms
      with
      | Ok inferred ->
          List.assoc_opt name inferred |> Option.value ~default:TUnknown
      | Error _ -> TUnknown
    in
    let expected_value_env env pattern _value_form rest =
      match pattern with
      | FSymbol name -> (
          let inferred = inferred_binding_type env name rest in
          match inferred with
          | TUnknown | TVar _ -> env
          | ty -> Env.with_expected_type (Some ty) env)
      | _ -> env
    in
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
        let forms = Destructure.normalize_binding_type_hints forms in
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
                let value_env =
                  expected_value_env env pattern value_form rest
                in
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
                  | _ -> compile_expr scope value_env value_form
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
                              let false_non_nil_names =
                                false_nil_predicate_names value_form
                              in
                              [
                                ( Names.scoped_key scope name,
                                  Types.binding
                                    ?return_param_index:value.return_param_index
                                    ?constant_keyword ~false_non_nil_names
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
                                capability_storage_expression binding.ty
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
