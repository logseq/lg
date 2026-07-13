open Ast
open Types
open Expression_support

module Env = Compiler_environment

type expression_result = (typed_expr, Error.t) result
type type_result = (ty, Error.t) result

type t = {
  compile_vector : string -> Env.t -> Ast.form list -> expression_result;
  compile_map : string -> Env.t -> (Ast.form * Ast.form) list -> expression_result;
  compile_if :
    string -> Env.t -> Ast.form -> Ast.form -> Ast.form -> expression_result;
  compile_if_not :
    string -> Env.t -> Ast.form -> Ast.form -> Ast.form -> expression_result;
  compile_when : string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_cond : string -> Env.t -> Ast.form list -> expression_result;
  compile_match : string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_body : string -> Env.t -> string -> Ast.form list -> expression_result;
  compile_try : string -> Env.t -> Ast.form list -> expression_result;
  loop_branch_type : ty -> ty -> type_result;
  compile_recur :
    string -> Env.t -> string -> ty list -> Ast.form list -> expression_result;
  compile_loop_tail :
    string -> Env.t -> string -> ty list -> Ast.form -> expression_result;
  compile_loop_tail_body :
    string -> Env.t -> string -> ty list -> Ast.form list -> expression_result;
  compile_loop : string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_let : string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
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
  | Some (node_id, location) ->
      Semantic_ir.PLocated (node_id, location, pattern)

let located_form_pattern form pattern =
  located_pattern (Destructure.source_identity form) pattern

let create ~compile_expr =
  let compile_args_for = compile_args_for compile_expr in
  let rec compile_vector scope env forms =
    match forms with
    | [] -> Error.error "empty vector requires a type annotation"
    | first :: rest -> (
        match compile_expr scope env first with
        | Error _ as err -> err
        | Ok first_expr ->
            let rec loop acc = function
              | [] ->
                  let values =
                    List.rev acc |> List.map (fun expr -> expr.semantic_expr)
                  in
                  Ok
                    (typed_ir (TVector first_expr.ty)
                       (Semantic_ir.Apply
                          (Semantic_ir.Ident "Rrbvec.of_list", [ Semantic_ir.List values ])))
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
                     (fun field (_keyword, value) -> (field, value.semantic_expr))
                     fields pairs
                 in
                 {
                   (typed_ir (TRecord fields)
                      (Semantic_ir.Record
                         (List.map
                            (fun ((field : field), value) ->
                              (field.ocaml_name, value))
                            values, None))) with
                   record_values = Some values;
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
                   (Semantic_ir.If
                      ( condition.semantic_expr,
                        then_expr.semantic_expr,
                        else_expr.semantic_expr )))
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
                   (Semantic_ir.If
                      ( Semantic_ir.Apply
                          (Semantic_ir.Ident "not", [ condition.semantic_expr ]),
                        then_expr.semantic_expr,
                        else_expr.semantic_expr )))
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
                   (Semantic_ir.If
                      (condition.semantic_expr, body.semantic_expr, Semantic_ir.Unit)))
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
                    Semantic_ir.If (test.semantic_expr, value.semantic_expr, acc))
                  pairs else_expr.semantic_expr
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
            | FInt value -> Ok (Semantic_ir.PInt value)
            | FString value | FKeyword value -> Ok (Semantic_ir.PString value)
            | FBool value -> Ok (Semantic_ir.PBool value)
            | _ -> Error.error "unsupported match pattern")
          else Error.error "match pattern type must match target"
    in
    let rec compile_pattern target_ty pattern =
      let result =
        match (target_ty, pattern) with
      | _, FSymbol "_" -> Ok (Semantic_ir.PAny, [])
      | target_ty,
        FList
          [ FSymbol "as"; inner_pattern; ((FSymbol alias) as alias_form) ] -> (
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
              else Ok (Semantic_ir.POr (left, right), left_bindings))
      | (TRecord fields | TNamed_record { fields; _ }),
        FList (FSymbol "record" :: field_patterns) ->
          let rec compile_fields compiled bindings seen = function
            | [] -> Ok (Semantic_ir.PRecord (List.rev compiled), bindings)
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
          |> Result.map (fun (patterns, bindings) -> (Semantic_ir.PTuple patterns, bindings))
      | target_ty, FSymbol name
        when is_ocaml_owned_type target_ty && starts_with_uppercase name ->
          Ok (Semantic_ir.PConstructor (name, None), [])
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
                     | _ -> Some (Semantic_ir.PTuple patterns)
                   in
                   (Semantic_ir.PConstructor (name, payload_pattern), bindings))
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
                  | TFn (payload_tys, _)
                    when List.length payload_tys = List.length payload_patterns ->
                      compile_constructor_payloads payload_tys
                  | TFn _ -> Error.error "constructor pattern arity mismatch"
                  | _ -> Error.error (name ^ " is not a constructor"))))
      | _, FSymbol name ->
          let ocaml_name = Names.sanitize_name name in
          Ok
            ( Semantic_ir.PVar ocaml_name,
              [ (Names.scoped_key scope name, Types.binding ocaml_name target_ty) ] )
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
              literal_pattern target_ty pattern |> Result.map (fun code -> (code, []))
          | FVector _ -> Error.error "match collection pattern must match target collection"
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
                loop (compiled_pattern :: compiled_patterns)
                  (bindings @ pattern_bindings) rest)
      in
      loop [] [] patterns
      |> Result.map (fun (patterns, bindings) -> (Semantic_ir.PList patterns, bindings))
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
                | Ok guard when Types.equal guard.ty TBool -> Ok (Some guard.semantic_expr)
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
              Semantic_ir.Apply (Semantic_ir.Ident "Rrbvec.to_list", [ target.semantic_expr ])
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
        | Ok ((_, _, first_result) :: _ as clauses) ->
            if
              List.for_all
                (fun (_, _, result) ->
                  branch_types_compatible first_result.ty result.ty)
                clauses
            then
              Ok
                (typed_ir first_result.ty
                   (Semantic_ir.Match_guarded
                      ( target_expr,
                        clauses
                        |> List.map (fun (pattern, guard, result) ->
                               (pattern, guard, result.semantic_expr)) )))
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
                 (Semantic_ir.Sequence [ expr.semantic_expr; body.semantic_expr ])))
  
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
      | TUnknown, ty | ty, TUnknown -> Ok ty
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
                match
                  ( Semantic_ir.unlocated handlers.semantic_expr,
                    compatible_try_type body.ty handlers.ty )
                with
                | _, (Error _ as err) -> err
                | Semantic_ir.Match_guarded (_, cases), Ok ty ->
                    Ok (typed_ir ty (Semantic_ir.Try (body.semantic_expr, cases)))
                | _, Ok _ -> Error.error "internal error: malformed try handlers")))
  
  and loop_branch_type left right =
    match (left, right) with
    | TUnknown, ty | ty, TUnknown -> Ok ty
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
                 typed_ir TUnknown
                   (Semantic_ir.Apply
                      (Semantic_ir.Ident loop_name, List.map (fun arg -> arg.semantic_expr) args)))
  
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
                     (Semantic_ir.If
                        ( condition.semantic_expr,
                          then_expr.semantic_expr,
                          else_expr.semantic_expr )))))
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
                 (Semantic_ir.Sequence [ expression.semantic_expr; body.semantic_expr ])))
  
  and compile_loop scope env bindings body_forms =
    match bindings with
    | FVector forms ->
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
            | ((FSymbol name) as name_form) :: value_form :: rest ->
                if name = "_" || List.mem name names then
                  Error.error "loop binding names must be unique symbols"
                else (
                  match compile_expr scope env value_form with
                  | Error _ as err -> err
                  | Ok value ->
                      compile_bindings (name :: names)
                        (Destructure.source_identity name_form :: identities)
                        (value :: values) (value.ty :: tys) rest)
            | _ -> Error.error "loop binding names must be symbols"
          in
          (match compile_bindings [] [] [] [] forms with
          | Error _ as err -> err
          | Ok (names, identities, values, param_tys) ->
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
                            List.map (fun value -> value.semantic_expr) values )))))
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
                           (Semantic_ir.Let (List.rev ir_bindings, body.semantic_expr)))
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
                          | FSymbol "_" ->
                              ( located_form_pattern pattern Semantic_ir.PAny,
                                value.semantic_expr )
                              :: ir_bindings
                          | _ ->
                              bindings
                              |> List.fold_left
                                   (fun acc (binding : Destructure.local_binding) ->
                                     ( located_pattern binding.identity
                                         (Semantic_ir.PVar binding.ocaml_name),
                                       binding.semantic_expr )
                                     :: acc)
                                   ir_bindings
                        in
                        bind (Env.add_bindings env_bindings env) ir_bindings rest))
            | [ _ ] -> Error.error "let bindings require an even number of forms"
          in
          bind env [] forms
    | _ -> Error.error "let bindings must be a vector"
  
  in
  { compile_vector; compile_map; compile_if; compile_if_not; compile_when; compile_cond; compile_match; compile_body; compile_try; loop_branch_type; compile_recur; compile_loop_tail; compile_loop_tail_body; compile_loop; compile_let }
