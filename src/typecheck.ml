open Ast
open Types

let ensure_bool expr =
  if Types.equal expr.ty TBool then Ok () else Error.error "if condition must be bool"

let parenthesize code = "(" ^ code ^ ")"

let apply_code fn_code arg_codes =
  match arg_codes with
  | [] -> parenthesize (fn_code ^ " ()")
  | _ -> parenthesize (fn_code ^ " " ^ (arg_codes |> List.map parenthesize |> String.concat " "))

let rec drop n xs =
  if n <= 0 then xs
  else match xs with [] -> [] | _ :: rest -> drop (n - 1) rest

let option_for_all predicate = function None -> true | Some value -> predicate value

let lookup_binding current_ns env name =
  match List.assoc_opt (Names.namespaced_key current_ns name) env with
  | Some (binding : binding) -> Ok binding
  | None -> Error.error ("unknown function " ^ name)

let lookup_function current_ns env name =
  match lookup_binding current_ns env name with
  | Ok binding -> Ok (typed binding.ty binding.ocaml_name)
  | Error _ -> (
      match name with
      | "+" -> Ok (typed (TFn ([ TInt; TInt ], TInt)) "(fun a b -> a + b)")
      | "-" -> Ok (typed (TFn ([ TInt; TInt ], TInt)) "(fun a b -> a - b)")
      | "*" -> Ok (typed (TFn ([ TInt; TInt ], TInt)) "(fun a b -> a * b)")
      | "/" -> Ok (typed (TFn ([ TInt; TInt ], TInt)) "(fun a b -> a / b)")
      | "inc" -> Ok (typed (TFn ([ TInt ], TInt)) "(fun x -> x + 1)")
      | "dec" -> Ok (typed (TFn ([ TInt ], TInt)) "(fun x -> x - 1)")
      | "not" -> Ok (typed (TFn ([ TBool ], TBool)) "not")
      | _ -> Error.error ("unknown function " ^ name))

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
      | Some type_name, TRecord fields -> Some (Type_def { type_name; fields })
      | _ -> None)
    row_type_names param_tys
  |> List.filter_map Fun.id

let row_project_code type_name fields arg =
  let source = "__row_source" in
  let values =
    fields
    |> List.map (fun (field : field) ->
           field.ocaml_name ^ " = " ^ source ^ "." ^ field.ocaml_name)
    |> String.concat "; "
  in
  "(let " ^ source ^ " = " ^ arg.code ^ " in ({" ^ values ^ "} : " ^ type_name ^ "))"

let row_arg_code row_type_name expected_ty arg =
  match (row_type_name, expected_ty, arg.ty) with
  | Some type_name, TRecord fields, TRecord _ -> row_project_code type_name fields arg
  | _ -> arg.code

let rec compile_expr current_ns (env : (string * binding) list) = function
  | FInt value -> Ok (typed_ir TInt (Ocaml_ir.Int value))
  | FString value -> Ok (typed_ir TString (Ocaml_ir.String value))
  | FBool value -> Ok (typed_ir TBool (Ocaml_ir.Bool value))
  | FNil -> Ok (typed_ir TNil Ocaml_ir.Unit)
  | FKeyword keyword -> Ok (typed_ir TKeyword (Ocaml_ir.String keyword))
  | FSymbol name -> (
      match List.assoc_opt (Names.namespaced_key current_ns name) env with
      | Some binding -> Ok (typed_ir binding.ty (Ocaml_ir.Ident binding.ocaml_name))
      | None -> Error.error ("unknown symbol " ^ name))
  | FVector forms -> compile_vector current_ns env forms
  | FMap pairs -> compile_map current_ns env pairs
  | FList (FSymbol "let" :: bindings :: body_forms) ->
      compile_let current_ns env bindings body_forms
  | FList (FSymbol "fn" :: params :: body_forms) ->
      compile_fn current_ns env params body_forms
  | FList (FSymbol "do" :: body_forms) ->
      compile_body current_ns env "do requires at least one form" body_forms
  | FList [ FKeyword keyword; target ] ->
      compile_get current_ns env [ target; FKeyword keyword ]
  | FList (FKeyword _ :: _) -> Error.error "keyword lookup expects one argument"
  | FList (FSymbol "if" :: condition :: then_form :: else_form :: []) ->
      compile_if current_ns env condition then_form else_form
  | FList (FSymbol "if-not" :: condition :: then_form :: else_form :: []) ->
      compile_if_not current_ns env condition then_form else_form
  | FList (FSymbol "when" :: condition :: body_forms) ->
      compile_when current_ns env condition body_forms
  | FList (FSymbol "cond" :: clauses) -> compile_cond current_ns env clauses
  | FList (FSymbol "match" :: target :: clauses) ->
      compile_match current_ns env target clauses
  | FList (FSymbol name :: args) -> compile_call current_ns env name args
  | FList [] -> Error.error "empty list is not callable"
  | FList _ -> Error.error "call head must be a symbol"

and compile_vector current_ns env forms =
  match forms with
  | [] -> Error.error "empty vector requires a type annotation"
  | first :: rest -> (
      match compile_expr current_ns env first with
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
                match compile_expr current_ns env form with
                | Error _ as err -> err
                | Ok expr ->
                    if Types.equal first_expr.ty expr.ty then loop (expr :: acc) rest
                    else Error.error "vector elements must all have the same type")
          in
          loop [ first_expr ] rest)

and compile_map current_ns env pairs =
  let compile_pair = function
    | FKeyword keyword, value_form -> (
        match compile_expr current_ns env value_form with
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
                 code = "<record>";
                 ocaml_expr = Ocaml_ir.Raw "<record>";
                 record_values = Some values;
               })
    | pair :: rest -> (
        match compile_pair pair with
        | Ok pair -> loop (pair :: acc) rest
        | Error _ as err -> err)
  in
  loop [] pairs

and compile_if current_ns env condition then_form else_form =
  match
    ( compile_expr current_ns env condition,
      compile_expr current_ns env then_form,
      compile_expr current_ns env else_form )
  with
  | (Error _ as err), _, _ -> err
  | _, (Error _ as err), _ -> err
  | _, _, (Error _ as err) -> err
  | Ok condition, Ok then_expr, Ok else_expr -> (
      match ensure_bool condition with
      | Error _ as err -> err
      | Ok () ->
          if Types.equal then_expr.ty else_expr.ty then
            Ok
              (typed_ir then_expr.ty
                 (Ocaml_ir.If
                    ( condition.ocaml_expr,
                      then_expr.ocaml_expr,
                      else_expr.ocaml_expr )))
          else Error.error "if branches must have same type")

and compile_if_not current_ns env condition then_form else_form =
  match
    ( compile_expr current_ns env condition,
      compile_expr current_ns env then_form,
      compile_expr current_ns env else_form )
  with
  | (Error _ as err), _, _ -> err
  | _, (Error _ as err), _ -> err
  | _, _, (Error _ as err) -> err
  | Ok condition, Ok then_expr, Ok else_expr -> (
      match ensure_bool condition with
      | Error _ as err -> err
      | Ok () ->
          if Types.equal then_expr.ty else_expr.ty then
            Ok
              (typed_ir then_expr.ty
                 (Ocaml_ir.If
                    ( Ocaml_ir.Apply
                        (Ocaml_ir.Ident "not", [ condition.ocaml_expr ]),
                      then_expr.ocaml_expr,
                      else_expr.ocaml_expr )))
          else Error.error "if-not branches must have same type")

and compile_when current_ns env condition body_forms =
  match
    ( compile_expr current_ns env condition,
      compile_body current_ns env "when body requires at least one form" body_forms )
  with
  | (Error _ as err), _ -> err
  | _, (Error _ as err) -> err
  | Ok condition, Ok body -> (
      match ensure_bool condition with
      | Error _ as err -> err
      | Ok () ->
          if Types.equal body.ty TUnit || Types.equal body.ty TNil then
            Ok
              (typed_ir body.ty
                 (Ocaml_ir.If
                    (condition.ocaml_expr, body.ocaml_expr, Ocaml_ir.Unit)))
          else Error.error "when body must be unit or nil")

and compile_cond current_ns env clauses =
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
    match compile_expr current_ns env form with
    | Error _ as err -> err
    | Ok test ->
        if Types.equal test.ty TBool then Ok test else Error.error "cond tests must be bool"
  in
  let rec compile_pairs acc = function
    | [] -> Ok (List.rev acc)
    | (test_form, value_form) :: rest -> (
        match (compile_test test_form, compile_expr current_ns env value_form) with
        | (Error _ as err), _ -> err
        | _, (Error _ as err) -> err
        | Ok test, Ok value -> compile_pairs ((test, value) :: acc) rest)
  in
  match parse_pairs clauses with
  | Error _ as err -> err
  | Ok (pairs, else_form) -> (
      match (compile_pairs [] pairs, compile_expr current_ns env else_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok pairs, Ok else_expr ->
          if
            List.for_all
              (fun (_test, value) -> Types.equal value.ty else_expr.ty)
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

and compile_match current_ns env target_form clauses =
  let rec parse_pairs acc = function
    | [] -> Ok (List.rev acc)
    | [ _ ] -> Error.error "match requires pattern/result pairs"
    | pattern :: result :: rest -> parse_pairs ((pattern, result) :: acc) rest
  in
  let literal_pattern expected_ty form =
    match compile_expr current_ns env form with
    | Error _ as err -> err
    | Ok pattern ->
        if Types.equal expected_ty pattern.ty then
          (match form with
          | FInt value -> Ok (Ocaml_ir.PInt value)
          | FString value | FKeyword value -> Ok (Ocaml_ir.PString value)
          | FBool value -> Ok (Ocaml_ir.PBool value)
          | FNil -> Ok Ocaml_ir.PUnit
          | _ -> Error.error "unsupported match pattern")
        else Error.error "match pattern type must match target"
  in
  let rec compile_pattern target_ty pattern =
    match (target_ty, pattern) with
    | _, FSymbol "_" -> Ok (Ocaml_ir.PAny, [])
    | _, FSymbol name ->
        let ocaml_name = Names.sanitize_name name in
        Ok
          ( Ocaml_ir.PVar ocaml_name,
            [ (Names.namespaced_key current_ns name, Types.binding ocaml_name target_ty) ] )
    | TInt, FInt value -> Ok (Ocaml_ir.PInt value, [])
    | TString, FString value -> Ok (Ocaml_ir.PString value, [])
    | TKeyword, FKeyword keyword -> Ok (Ocaml_ir.PString keyword, [])
    | TBool, FBool value -> Ok (Ocaml_ir.PBool value, [])
    | TNil, FNil -> Ok (Ocaml_ir.PUnit, [])
    | TList inner, FVector patterns ->
        compile_list_like_pattern inner patterns
    | TVector inner, FVector patterns ->
        compile_list_like_pattern inner patterns
    | _ -> (
        match pattern with
        | FInt _ | FString _ | FKeyword _ | FBool _ | FNil ->
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
    match compile_pattern target_ty pattern_form with
    | Error _ as err -> err
    | Ok (pattern_code, bindings) -> (
        match compile_expr current_ns (env @ bindings) result_form with
        | Error _ as err -> err
        | Ok result -> Ok (pattern_code, result))
  in
  match (compile_expr current_ns env target_form, parse_pairs [] clauses) with
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
      | Ok ((_, first_result) :: _ as clauses) ->
          if List.for_all (fun (_, result) -> Types.equal first_result.ty result.ty) clauses then
            Ok
              (typed_ir first_result.ty
                 (Ocaml_ir.Match
                    ( target_expr,
                      clauses
                      |> List.map (fun (pattern, result) ->
                             (pattern, result.ocaml_expr)) )))
          else Error.error "match branches must have same type")

and compile_body current_ns env empty_error forms =
  match forms with
  | [] -> Error.error empty_error
  | [ form ] -> compile_expr current_ns env form
  | form :: rest -> (
      match (compile_expr current_ns env form, compile_body current_ns env empty_error rest) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok expr, Ok body ->
          Ok
            (typed_ir body.ty
               (Ocaml_ir.Sequence [ expr.ocaml_expr; body.ocaml_expr ])))

and compile_let current_ns env bindings body_forms =
  match bindings with
  | FVector forms ->
      if List.length forms mod 2 <> 0 then
        Error.error "let bindings require an even number of forms"
      else
        let rec bind env code_parts ir_bindings = function
          | [] -> (
              match
                compile_body current_ns env "let body requires at least one form"
                  body_forms
              with
              | Error _ as err -> err
              | Ok body ->
                  (match ir_bindings with
                  | Some bindings ->
                      Ok
                        (typed_ir body.ty
                           (Ocaml_ir.Let (List.rev bindings, body.ocaml_expr)))
                  | None ->
                      let code =
                        code_parts
                        |> List.fold_left
                             (fun acc binding_code -> binding_code ^ " in " ^ acc)
                             body.code
                      in
                      Ok (typed body.ty ("(" ^ code ^ ")"))))
          | pattern :: value_form :: rest -> (
              match compile_expr current_ns env value_form with
              | Error _ as err -> err
              | Ok value -> (
                  match Destructure.bind_pattern value pattern with
                  | Error _ as err -> err
                  | Ok bindings ->
                      let env_bindings =
                        bindings
                        |> List.map (fun (binding : Destructure.local_binding) ->
                                ( Names.namespaced_key current_ns binding.source_name,
                                 Types.binding binding.ocaml_name binding.ty ))
                      in
                      let code_bindings = bindings |> List.map Destructure.let_code in
                      let ir_bindings =
                        match (ir_bindings, pattern) with
                        | Some bindings, FSymbol "_" ->
                            Some ((Ocaml_ir.PAny, value.ocaml_expr) :: bindings)
                        | Some bindings, FSymbol name ->
                            Some
                              ( (Ocaml_ir.PVar (Names.sanitize_name name), value.ocaml_expr)
                              :: bindings )
                        | _ -> None
                      in
                      bind (env @ env_bindings)
                        (List.rev_append code_bindings code_parts)
                        ir_bindings rest))
          | [ _ ] -> Error.error "let bindings require an even number of forms"
        in
        bind env [] (Some []) forms
  | _ -> Error.error "let bindings must be a vector"

and prepare_fn current_ns env params body_forms =
  match Destructure.parse_param_specs params with
  | Error _ as err -> err
  | Ok specs ->
      let lookup_function_ty name =
        match lookup_function current_ns env name with
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
              let param_bindings =
                typed_specs
                |> List.map (fun ((spec : Destructure.param_spec), ty) ->
                       ( Names.namespaced_key current_ns spec.source_name,
                         Types.binding spec.ocaml_name ty ))
              in
              let param_targets =
                typed_specs
                |> List.map (fun ((spec : Destructure.param_spec), ty) ->
                       (spec, typed ty spec.ocaml_name))
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
                           ( Names.namespaced_key current_ns binding.source_name,
                             Types.binding binding.ocaml_name binding.ty ))
                  in
                  let env = env @ param_bindings @ local_bindings in
                  match
                    compile_body current_ns env "function body requires at least one form"
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
    param_names
    |> List.mapi (fun index name ->
           match List.nth_opt row_param_type_names index with
           | Some (Some type_name) ->
               Ocaml_ir.PConstraint (Ocaml_ir.PVar name, type_name)
           | _ -> Ocaml_ir.PVar name)
  in
  let body_expr =
    match parts.destructured_bindings with
    | [] -> parts.body.ocaml_expr
    | bindings ->
        let body_code =
          bindings
          |> List.map Destructure.let_code
          |> List.fold_left
               (fun acc binding_code -> binding_code ^ " in " ^ acc)
               parts.body.code
        in
        Ocaml_ir.Raw body_code
  in
  typed_ir (TFn (param_tys, parts.body.ty)) (Ocaml_ir.Fun (param_patterns, body_expr))

and compile_fn current_ns env params body_forms =
  match prepare_fn current_ns env params body_forms with
  | Error _ as err -> err
  | Ok parts -> Ok (fn_code parts)

and compile_call current_ns env name arg_forms =
  let compile_args () = compile_args_for current_ns env arg_forms in
  match name with
  | "+" | "-" | "*" | "/" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok args -> (
          match Core_int.expect_int_args name args with
          | Error _ as err -> err
          | Ok () -> Core_int.compile_operator name args))
  | "inc" ->
      compile_int_unary_call current_ns env name
        (fun expression -> Ocaml_ir.Infix ("+", expression, Ocaml_ir.Int 1))
        arg_forms
  | "dec" ->
      compile_int_unary_call current_ns env name
        (fun expression -> Ocaml_ir.Infix ("-", expression, Ocaml_ir.Int 1))
        arg_forms
  | "=" | "not=" | "<" | "<=" | ">" | ">=" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok args -> Core_compare.compile name args)
  | "not" | "nil?" | "some?" | "true?" | "false?" | "int?" | "number?"
  | "string?" | "keyword?" | "boolean?" | "vector?" | "list?" | "seq?" | "set?"
  | "map?" | "fn?" | "coll?" | "associative?" | "indexed?" | "seqable?" | "counted?"
    -> compile_boolean_call current_ns env name arg_forms
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
      compile_int_unary_call current_ns env name
        (fun expression -> Ocaml_ir.Infix ("=", expression, Ocaml_ir.Int 0))
        arg_forms
      |> Result.map (fun expr -> { expr with ty = TBool })
  | "pos?" ->
      compile_int_unary_call current_ns env name
        (fun expression -> Ocaml_ir.Infix (">", expression, Ocaml_ir.Int 0))
        arg_forms
      |> Result.map (fun expr -> { expr with ty = TBool })
  | "neg?" ->
      compile_int_unary_call current_ns env name
        (fun expression -> Ocaml_ir.Infix ("<", expression, Ocaml_ir.Int 0))
        arg_forms
      |> Result.map (fun expr -> { expr with ty = TBool })
  | "even?" ->
      compile_int_unary_call current_ns env name
        (fun expression ->
          Ocaml_ir.Infix
            ( "=",
              Ocaml_ir.Infix ("mod", expression, Ocaml_ir.Int 2),
              Ocaml_ir.Int 0 ))
        arg_forms
      |> Result.map (fun expr -> { expr with ty = TBool })
  | "odd?" ->
      compile_int_unary_call current_ns env name
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
          let code =
            match args with
            | [] -> {|""|}
            | _ -> args |> List.map (Codegen.stringify_expr ~pr:false) |> String.concat " ^ "
          in
          Ok (typed TString code))
  | "subs" -> compile_subs current_ns env arg_forms
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
      compile_int_unary_call current_ns env name
        (fun expression -> Ocaml_ir.Prefix ("lnot", expression))
        arg_forms
  | "bit-shift-left" | "bit-shift-right" ->
      (match compile_args () with
      | Error _ as err -> err
      | Ok args -> Core_int.compile_binary name args)
  | "pr-str" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ arg ] -> Ok (typed TString (Codegen.stringify_expr ~pr:true arg))
      | Ok _ -> Error.error "pr-str expects 1 arguments")
  | "print" | "println" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ arg ] ->
          let printer = if name = "print" then "print_string" else "print_endline" in
          Ok (typed TUnit (printer ^ " (" ^ Codegen.print_expr arg ^ ")"))
      | Ok _ -> Error.error (name ^ " expects 1 arguments"))
  | "list" -> compile_list current_ns env arg_forms
  | "list*" -> compile_list_star current_ns env arg_forms
  | "range" -> compile_range current_ns env arg_forms
  | "list-of" -> compile_list_of arg_forms
  | "cons" -> compile_cons current_ns env arg_forms
  | "vector" -> compile_vector current_ns env arg_forms
  | "vector-of" -> compile_vector_of arg_forms
  | "count" -> compile_collection_call current_ns env name arg_forms
  | "conj" -> compile_conj current_ns env arg_forms
  | "first" | "second" | "last" | "peek" | "pop" ->
      compile_collection_call current_ns env name arg_forms
  | "subvec" -> compile_subvec current_ns env arg_forms
  | "nth" -> compile_nth current_ns env arg_forms
  | "get" -> compile_get current_ns env arg_forms
  | "assoc" -> compile_assoc current_ns env arg_forms
  | "dissoc" -> compile_dissoc current_ns env arg_forms
  | "merge" -> compile_merge current_ns env arg_forms
  | "update" -> compile_update current_ns env arg_forms
  | "select-keys" -> compile_select_keys current_ns env arg_forms
  | "contains?" -> compile_contains current_ns env arg_forms
  | "keys" -> compile_keys current_ns env arg_forms
  | "vals" -> compile_vals current_ns env arg_forms
  | "hash-map" | "array-map" | "sorted-map" -> compile_hash_map current_ns env arg_forms
  | "rest" | "seq" | "empty?" -> compile_collection_call current_ns env name arg_forms
  | "into" -> compile_sequence_transform_call current_ns env name arg_forms
  | "take" | "drop" -> compile_collection_call current_ns env name arg_forms
  | "butlast" | "take-last" | "drop-last" | "take-nth" ->
      compile_sequence_transform_call current_ns env name arg_forms
  | "next" | "nthnext" | "nthrest" | "ffirst" | "fnext" | "nfirst" | "nnext"
  | "rseq" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok args -> Core_sequence.compile name args)
  | "some" -> compile_some current_ns env arg_forms
  | "split-at" -> compile_sequence_transform_call current_ns env name arg_forms
  | "split-with" -> compile_split_with current_ns env arg_forms
  | "partition-by" -> compile_partition_by current_ns env arg_forms
  | "bounded-count" | "dorun" | "doall" ->
      compile_sequence_transform_call current_ns env name arg_forms
  | "run!" -> compile_run_bang current_ns env arg_forms
  | "reverse" -> compile_collection_call current_ns env name arg_forms
  | "every?" | "not-any?" | "not-every?" ->
      compile_sequence_bool_predicate current_ns env name arg_forms
  | "map" -> compile_map_call current_ns env arg_forms
  | "filter" -> compile_filter current_ns env arg_forms
  | "remove" | "take-while" | "drop-while" | "distinct" | "dedupe" | "sort" ->
      compile_sequence_transform_call current_ns env name arg_forms
  | "sort-by" -> compile_sort_by current_ns env arg_forms
  | "concat" -> compile_sequence_transform_call current_ns env name arg_forms
  | "mapcat" -> compile_mapcat current_ns env arg_forms
  | "vec" | "set" | "repeat" ->
      compile_sequence_transform_call current_ns env name arg_forms
  | "repeatedly" -> compile_repeatedly current_ns env arg_forms
  | "interpose" | "interleave" | "partition" | "partition-all" ->
      compile_sequence_transform_call current_ns env name arg_forms
  | "reductions" -> compile_reductions current_ns env arg_forms
  | "map-indexed" -> compile_map_indexed current_ns env arg_forms
  | "filterv" -> compile_filterv current_ns env arg_forms
  | "mapv" -> compile_mapv current_ns env arg_forms
  | "reduce-kv" -> compile_reduce_kv current_ns env arg_forms
  | "reduce" -> compile_reduce current_ns env arg_forms
  | "apply" -> compile_apply current_ns env arg_forms
  | "comp" -> compile_comp current_ns env arg_forms
  | "partial" -> compile_partial current_ns env arg_forms
  | "identity" -> compile_identity current_ns env arg_forms
  | "constantly" -> compile_constantly current_ns env arg_forms
  | "complement" -> compile_complement current_ns env arg_forms
  | "every-pred" -> compile_predicate_combinator current_ns env "every-pred" arg_forms
  | "some-fn" -> compile_predicate_combinator current_ns env "some-fn" arg_forms
  | "juxt" -> compile_juxt current_ns env arg_forms
  | "distinct?" -> compile_distinct_question current_ns env arg_forms
  | "compare" -> compile_compare current_ns env arg_forms
  | "max-key" | "min-key" -> compile_key_extreme current_ns env name arg_forms
  | "hash-set" | "sorted-set" -> compile_hash_set current_ns env arg_forms
  | "set-of" -> compile_set_of arg_forms
  | "disj" -> compile_disj current_ns env arg_forms
  | "empty" -> compile_collection_call current_ns env name arg_forms
  | _ -> compile_named_function_call current_ns env name arg_forms

and compile_int_unary_call current_ns env name build_code arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok args -> Core_int.compile_unary name args build_code

and compile_boolean_call current_ns env name arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok args -> Core_boolean.compile name args

and compile_collection_call current_ns env name arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok args -> Core_collection.compile name args

and compile_sequence_transform_call current_ns env name arg_forms =
  match (name, arg_forms) with
  | ("partition" | "partition-all"), FInt size :: _ when size <= 0 ->
      Error.error (name ^ " size must be positive")
  | "take-nth", FInt count :: _ when count <= 0 ->
      Error.error "take-nth n must be positive"
  | ("remove" | "take-while" | "drop-while"), [ fn_form; collection_form ] -> (
      match
        ( compile_function_arg current_ns env fn_form,
          compile_expr current_ns env collection_form )
      with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> Core_sequence_transform.compile name [ fn; collection ])
  | _ -> (
      match compile_args_for current_ns env arg_forms with
      | Error _ as err -> err
      | Ok args -> Core_sequence_transform.compile name args)

and compile_subs current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
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

and compile_list current_ns env forms =
  match forms with
  | [] -> Error.error "empty list requires a type annotation"
  | first :: rest -> (
      match compile_expr current_ns env first with
      | Error _ as err -> err
      | Ok first_expr ->
          let rec loop acc = function
            | [] ->
                let values =
                  List.rev acc |> List.map (fun expr -> expr.ocaml_expr)
                in
                Ok (typed_ir (TList first_expr.ty) (Ocaml_ir.List values))
            | form :: rest -> (
                match compile_expr current_ns env form with
                | Error _ as err -> err
                | Ok expr ->
                    if Types.equal first_expr.ty expr.ty then loop (expr :: acc) rest
                    else Error.error "list elements must all have the same type")
          in
          loop [ first_expr ] rest)

and compile_list_star current_ns env arg_forms =
  match List.rev arg_forms with
  | [] -> Error.error "list* expects values and final collection"
  | final_form :: prefix_forms_rev -> (
      match compile_expr current_ns env final_form with
      | Error _ as err -> err
      | Ok final -> (
          match collection_to_list_code final with
          | Error _ -> Error.error "list* final argument must be a collection"
          | Ok (inner, final_list_code) -> (
              let prefix_forms = List.rev prefix_forms_rev in
              match compile_args_for current_ns env prefix_forms with
              | Error _ as err -> err
              | Ok prefix_args ->
                  if List.for_all (fun arg -> Types.equal inner arg.ty) prefix_args then
                    let prefix_code =
                      prefix_args |> List.map (fun arg -> arg.code) |> String.concat "; "
                    in
                    let list_code =
                      match prefix_args with
                      | [] -> final_list_code
                      | _ -> "[" ^ prefix_code ^ "] @ (" ^ final_list_code ^ ")"
                    in
                    Ok (typed (TList inner) list_code)
                  else Error.error "list* value type must match final collection element type")))

and compile_range current_ns env arg_forms =
  let literal_zero = function FInt 0 -> true | _ -> false in
  match arg_forms with
  | [ end_form ] -> (
      match compile_expr current_ns env end_form with
      | Error _ as err -> err
      | Ok end_expr ->
          if Types.equal end_expr.ty TInt then
            Ok
              (typed (TList TInt)
                 ("(let rec range acc current stop step = if current >= stop then List.rev acc else range (current :: acc) (current + step) stop step in range [] 0 ("
                ^ end_expr.code ^ ") 1)"))
          else Error.error "range arguments must be int")
  | [ start_form; end_form ] -> (
      match (compile_expr current_ns env start_form, compile_expr current_ns env end_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok start_expr, Ok end_expr ->
          if Types.equal start_expr.ty TInt && Types.equal end_expr.ty TInt then
            Ok
              (typed (TList TInt)
                 ("(let rec range acc current stop step = if current >= stop then List.rev acc else range (current :: acc) (current + step) stop step in range [] ("
                ^ start_expr.code ^ ") (" ^ end_expr.code ^ ") 1)"))
          else Error.error "range arguments must be int")
  | [ start_form; end_form; step_form ] ->
      if literal_zero step_form then Error.error "range step cannot be 0"
      else (
        match
          ( compile_expr current_ns env start_form,
            compile_expr current_ns env end_form,
            compile_expr current_ns env step_form )
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
                (typed (TList TInt)
                   ("(let rec range acc current stop step = if step = 0 then invalid_arg \"range step cannot be 0\" else if (step > 0 && current >= stop) || (step < 0 && current <= stop) then List.rev acc else range (current :: acc) (current + step) stop step in range [] ("
                  ^ start_expr.code ^ ") (" ^ end_expr.code ^ ") (" ^ step_expr.code ^ "))"))
            else Error.error "range arguments must be int")
  | _ -> Error.error "range expects end, start/end, or start/end/step"

and compile_list_of arg_forms =
  match arg_forms with
  | [ FKeyword keyword ] -> (
      match Type_annotation.of_keyword keyword with
      | Error _ as err -> err
      | Ok element_ty -> Ok (typed (TList element_ty) "[]"))
  | _ -> Error.error "list-of expects one type keyword"

and compile_vector_of arg_forms =
  match arg_forms with
  | [ FKeyword keyword ] -> (
      match Type_annotation.of_keyword keyword with
      | Error _ as err -> err
      | Ok element_ty -> Ok (typed (TVector element_ty) "Rrbvec.empty"))
  | _ -> Error.error "vector-of expects one type keyword"

and compile_conj current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok (collection :: values) when values <> [] ->
      let add_value collection value =
        match collection.ty with
        | TList inner when Types.equal inner value.ty ->
            Ok (typed collection.ty ("(" ^ value.code ^ " :: (" ^ collection.code ^ "))"))
        | TList _ -> Error.error "conj value type must match list element type"
        | TVector inner when Types.equal inner value.ty ->
            Ok
              (typed collection.ty
                 ("Rrbvec.push_back (" ^ collection.code ^ ") (" ^ value.code ^ ")"))
        | TVector _ -> Error.error "conj value type must match vector element type"
        | TSet inner when Types.equal inner value.ty ->
            Ok
              (typed collection.ty
                 ("List.sort_uniq compare (" ^ value.code ^ " :: (" ^ collection.code ^ "))"))
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

and compile_cons current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ value; collection ] -> (
      match collection.ty with
      | TList inner when Types.equal inner value.ty ->
          Ok (typed collection.ty ("(" ^ value.code ^ " :: (" ^ collection.code ^ "))"))
      | TList _ -> Error.error "cons value type must match list element type"
      | _ -> Error.error "cons expects a value and list")
  | Ok _ -> Error.error "cons expects value and list"

and compile_subvec current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ vector; start ] -> (
      match (vector.ty, start.ty) with
      | TVector _, TInt ->
          Ok
            (typed vector.ty
               ("Option.get (Rrbvec.subvec (" ^ vector.code ^ ") (" ^ start.code
              ^ ") (Rrbvec.length (" ^ vector.code ^ ")))"))
      | TVector _, _ -> Error.error "subvec indexes must be int"
      | _ -> Error.error "subvec expects a vector")
  | Ok [ vector; start; stop ] -> (
      match (vector.ty, start.ty, stop.ty) with
      | TVector _, TInt, TInt ->
          Ok
            (typed vector.ty
               ("Option.get (Rrbvec.subvec (" ^ vector.code ^ ") (" ^ start.code
              ^ ") (" ^ stop.code ^ "))"))
      | TVector _, _, _ -> Error.error "subvec indexes must be int"
      | _ -> Error.error "subvec expects a vector")
  | Ok _ -> Error.error "subvec expects vector, start, and optional stop"

and compile_nth current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ collection; index ] -> (
      match (collection.ty, index.ty) with
      | TList inner, TInt ->
          Ok (typed inner ("List.nth (" ^ collection.code ^ ") (" ^ index.code ^ ")"))
      | TList _, _ -> Error.error "nth index must be int"
      | TVector inner, TInt ->
          Ok (typed inner ("Rrbvec.nth (" ^ collection.code ^ ") (" ^ index.code ^ ")"))
      | TVector _, _ -> Error.error "nth index must be int"
      | _ -> Error.error "nth expects a list or vector")
  | Ok [ collection; index; default ] -> (
      match (collection.ty, index.ty) with
      | TList inner, TInt when Types.equal inner default.ty ->
          Ok
            (typed inner
               ("(if (" ^ index.code ^ ") < 0 then " ^ default.code
              ^ " else try List.nth (" ^ collection.code ^ ") (" ^ index.code
              ^ ") with Failure _ -> " ^ default.code ^ ")"))
      | TList _, TInt -> Error.error "nth default must match collection element type"
      | TList _, _ -> Error.error "nth index must be int"
      | TVector inner, TInt when Types.equal inner default.ty ->
          Ok
            (typed inner
               ("(match Rrbvec.nth_opt (" ^ collection.code ^ ") (" ^ index.code
              ^ ") with Some value -> value | None -> " ^ default.code ^ ")"))
      | TVector _, TInt -> Error.error "nth default must match collection element type"
      | TVector _, _ -> Error.error "nth index must be int"
      | _ -> Error.error "nth expects a list or vector")
  | Ok _ -> Error.error "nth expects 2 or 3 arguments"

and compile_get current_ns env arg_forms =
  match arg_forms with
  | [ target_form; FKeyword keyword ] -> (
      match compile_expr current_ns env target_form with
      | Error _ as err -> err
      | Ok target -> (
          match target.ty with
          | TRecord fields -> (
              match find_field keyword fields with
              | Some field ->
                  Ok
                    (typed_ir field.ty
                       (Structural_map.field_expr target field))
              | None -> Error.error ("unknown field " ^ keyword))
          | _ -> Error.error "get expects a map"))
  | [ target_form; index_form ] -> (
      match (compile_expr current_ns env target_form, compile_expr current_ns env index_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok target, Ok index -> (
          match (target.ty, index.ty) with
          | TVector inner, TInt ->
              Ok (typed inner ("Rrbvec.nth (" ^ target.code ^ ") (" ^ index.code ^ ")"))
          | TVector _, _ -> Error.error "get vector index must be int"
          | _ -> Error.error "get key must be a keyword"))
  | [ target_form; FKeyword keyword; default_form ] -> (
      match
        (compile_expr current_ns env target_form, compile_expr current_ns env default_form)
      with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok target, Ok default -> (
          match target.ty with
          | TRecord fields -> (
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
        ( compile_expr current_ns env target_form,
          compile_expr current_ns env index_form,
          compile_expr current_ns env default_form )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok target, Ok index, Ok default -> (
          match (target.ty, index.ty) with
          | TVector inner, TInt when Types.equal inner default.ty ->
              Ok
                (typed inner
                   ("(match Rrbvec.nth_opt (" ^ target.code ^ ") (" ^ index.code
                  ^ ") with Some value -> value | None -> " ^ default.code ^ ")"))
          | TVector _, TInt -> Error.error "get default for vector must match element type"
          | TVector _, _ -> Error.error "get vector index must be int"
          | _ -> Error.error "get key must be a keyword"))
  | _ -> Error.error "get expects 2 or 3 arguments"

and compile_assoc current_ns env arg_forms =
  match arg_forms with
  | target_form :: pair_forms ->
      let rec compile_record_pairs acc = function
        | [] -> Ok (List.rev acc)
        | FKeyword keyword :: value_form :: rest -> (
            match compile_expr current_ns env value_form with
            | Error _ as err -> err
            | Ok value -> compile_record_pairs ((keyword, value) :: acc) rest)
        | _ -> Error.error "assoc expects map followed by keyword/value pairs"
      in
      let rec compile_vector_pairs acc = function
        | [] -> Ok (List.rev acc)
        | index_form :: value_form :: rest -> (
            match
              ( compile_expr current_ns env index_form,
                compile_expr current_ns env value_form )
            with
            | (Error _ as err), _ -> err
            | _, (Error _ as err) -> err
            | Ok index, Ok value -> compile_vector_pairs ((index, value) :: acc) rest)
        | _ -> Error.error "assoc expects collection followed by key/value pairs"
      in
      (match compile_expr current_ns env target_form with
      | Error _ as err -> err
      | Ok target -> (
          if pair_forms = [] || List.length pair_forms mod 2 <> 0 then
            match target.ty with
            | TRecord _ -> Error.error "assoc expects map followed by keyword/value pairs"
            | TVector _ -> Error.error "assoc expects vector followed by index/value pairs"
            | _ -> Error.error "assoc expects collection followed by key/value pairs"
          else
            match target.ty with
            | TRecord _ -> (
                match compile_record_pairs [] pair_forms with
                | Error _ as err -> err
                | Ok pairs -> Structural_map.assoc_many target pairs)
            | TVector inner -> (
                match compile_vector_pairs [] pair_forms with
                | Error _ as err -> err
                | Ok pairs ->
                    let rec apply_pairs code = function
                      | [] -> Ok code
                      | (index, value) :: rest ->
                          if not (Types.equal index.ty TInt) then
                            Error.error "assoc vector index must be int"
                          else if not (Types.equal value.ty inner) then
                            Error.error "assoc vector value must match element type"
                          else
                            apply_pairs
                              ("Rrbvec.set (" ^ code ^ ") (" ^ index.code ^ ") ("
                             ^ value.code ^ ")")
                              rest
                    in
                    (match apply_pairs target.code pairs with
                    | Error _ as err -> err
                    | Ok code -> Ok (typed target.ty code)))
            | _ -> Error.error "assoc expects a map or vector"))
  | _ -> Error.error "assoc expects collection followed by key/value pairs"

and compile_dissoc current_ns env arg_forms =
  match arg_forms with
  | target_form :: key_forms -> (
      let rec parse_keys acc = function
        | [] -> Ok (List.rev acc)
        | FKeyword keyword :: rest -> parse_keys (keyword :: acc) rest
        | _ -> Error.error "dissoc expects map followed by keywords"
      in
      match compile_expr current_ns env target_form with
      | Error _ as err -> err
      | Ok target -> (
          match parse_keys [] key_forms with
          | Error _ as err -> err
          | Ok keywords -> Structural_map.dissoc_many target keywords))
  | _ -> Error.error "dissoc expects map followed by keywords"

and compile_merge current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok maps -> Structural_map.merge maps

and compile_hash_map current_ns env arg_forms =
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
    | Ok pairs -> compile_map current_ns env pairs

and compile_update current_ns env arg_forms =
  match arg_forms with
  | target_form :: FKeyword keyword :: fn_form :: extra_forms -> (
      match
        ( compile_expr current_ns env target_form,
          compile_function_arg current_ns env fn_form,
          compile_args_for current_ns env extra_forms )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok target, Ok fn, Ok extra_args -> (
          match target.ty with
          | TRecord fields -> (
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
        ( compile_expr current_ns env target_form,
          compile_expr current_ns env index_form,
          compile_function_arg current_ns env fn_form,
          compile_args_for current_ns env extra_forms )
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
                  let old_code = "Rrbvec.nth (" ^ target.code ^ ") (" ^ index.code ^ ")" in
                  let value_code =
                    apply_code fn.code
                      (old_code :: List.map (fun arg -> arg.code) extra_args)
                  in
                  Ok
                    (typed target.ty
                       ("Rrbvec.set (" ^ target.code ^ ") (" ^ index.code ^ ") ("
                      ^ value_code ^ ")"))
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

and compile_select_keys current_ns env arg_forms =
  match arg_forms with
  | [ target_form; FVector key_forms ] -> (
      let rec parse_keywords acc = function
        | [] -> Ok (List.rev acc)
        | FKeyword keyword :: rest -> parse_keywords (keyword :: acc) rest
        | _ -> Error.error "select-keys expects a vector of keywords"
      in
      match (compile_expr current_ns env target_form, parse_keywords [] key_forms) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok target, Ok keywords -> (
          match target.ty with
          | TRecord fields -> Structural_map.select_keys target fields keywords
          | _ -> Error.error "select-keys expects a map"))
  | [ _; _ ] -> Error.error "select-keys expects a vector of keywords"
  | _ -> Error.error "select-keys expects map and key vector"

and compile_contains current_ns env arg_forms =
  let compile_collection_contains target value =
    match (target.ty, value.ty) with
    | TSet inner, _ when Types.equal inner value.ty ->
        Ok (typed TBool ("List.mem (" ^ value.code ^ ") (" ^ target.code ^ ")"))
    | TSet _, _ -> Error.error "contains? value type must match set element type"
    | TVector _, TInt ->
        Ok
          (typed TBool
             ("((" ^ value.code ^ ") >= 0 && (" ^ value.code ^ ") < Rrbvec.length ("
            ^ target.code ^ "))"))
    | TVector _, _ -> Error.error "contains? vector index must be int"
    | _ -> Error.error "contains? expects a map, set, or vector"
  in
  match arg_forms with
  | target_form :: FKeyword keyword :: [] -> (
      match compile_expr current_ns env target_form with
      | Error _ as err -> err
      | Ok target -> (
          match target.ty with
          | TRecord fields ->
              Ok (typed TBool (string_of_bool (Option.is_some (find_field keyword fields))))
          | _ ->
              compile_collection_contains target
                (typed TKeyword (Codegen.ocaml_string_literal keyword))))
  | target_form :: value_form :: [] -> (
      match (compile_expr current_ns env target_form, compile_expr current_ns env value_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok target, Ok value -> compile_collection_contains target value)
  | _ -> Error.error "contains? expects collection and key"

and compile_keys current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ target ] -> (
      match target.ty with
      | TRecord fields ->
          let values =
            fields
            |> List.map (fun (field : field) -> Codegen.ocaml_string_literal field.keyword)
            |> String.concat "; "
          in
          Ok (typed (TVector TKeyword) ("Rrbvec.of_list [" ^ values ^ "]"))
      | _ -> Error.error "keys expects a map")
  | Ok _ -> Error.error "keys expects 1 arguments"

and compile_vals current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ target ] -> (
      match target.ty with
      | TRecord [] -> Error.error "vals requires a non-empty map"
      | TRecord (first :: rest) ->
          if List.for_all (fun (field : field) -> Types.equal first.ty field.ty) rest then
            let values =
              (first :: rest)
              |> List.map (fun (field : field) -> Structural_map.field_code target field)
              |> String.concat "; "
            in
            Ok (typed (TVector first.ty) ("Rrbvec.of_list [" ^ values ^ "]"))
          else Error.error "vals requires all map values to have the same type"
      | _ -> Error.error "vals expects a map")
  | Ok _ -> Error.error "vals expects 1 arguments"

and compile_function_arg current_ns env = function
  | FSymbol name -> lookup_function current_ns env name
  | form -> compile_expr current_ns env form

and compile_named_function_call current_ns env name arg_forms =
  match lookup_binding current_ns env name with
  | Error _ -> compile_protocol_call current_ns env name arg_forms
  | Ok fn -> (
      match compile_args_for current_ns env arg_forms with
      | Error _ as err -> err
      | Ok args -> (
          match fn.ty with
          | TFn (param_tys, ret)
            when List.length param_tys = List.length args
                 && List.for_all2
                      (fun expected arg -> Types.compatible ~expected ~actual:arg.ty)
                      param_tys args ->
              let arg_codes =
                args
                |> List.mapi (fun index arg ->
                       let row_type_name = List.nth_opt fn.row_param_types index |> Option.join in
                       let expected_ty = List.nth param_tys index in
                       row_arg_code row_type_name expected_ty arg)
              in
              Ok (typed ret (apply_code fn.ocaml_name arg_codes))
          | TFn _ -> Error.error (name ^ " called with incompatible arguments")
          | _ -> Error.error (name ^ " is not callable")))

and compile_protocol_call current_ns env name arg_forms =
  match Protocol.lookup_marker current_ns env name with
  | None -> Error.error ("unknown function " ^ name)
  | Some marker -> (
      match compile_args_for current_ns env arg_forms with
      | Error _ as err -> err
      | Ok args -> (
          match marker.ty with
          | TFn (param_tys, _ret) when List.length param_tys <> List.length args ->
              Error.error (name ^ " called with incompatible arguments")
          | TFn (_, _) -> (
              match args with
              | [] -> Error.error (name ^ " called with incompatible arguments")
              | receiver :: _ -> (
                  match Protocol.lookup_impl current_ns env name receiver.ty with
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
                            (typed ret
                               (apply_code impl.ocaml_name
                                  (List.map (fun arg -> arg.code) args)))
                      | TFn _ -> Error.error (name ^ " called with incompatible arguments")
                      | _ -> Error.error (name ^ " is not callable"))))
          | _ -> Error.error (name ^ " is not callable")))

and collection_to_list_code collection =
  Core_sequence_transform.collection_to_list_code collection

and collection_from_list_code collection_ty list_code =
  Core_sequence_transform.collection_from_list_code collection_ty list_code

and comparable_type = function
  | TInt | TString | TSymbol | TKeyword | TBool | TAny -> true
  | _ -> false

and compile_sort_by current_ns env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg current_ns env fn_form, compile_expr current_ns env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_code collection) with
          | TFn ([ param_ty ], key_ty), Ok (inner, list_code)
            when Types.equal param_ty inner && comparable_type key_ty ->
              Ok
                (typed (TList inner)
                   ("List.sort (fun left right -> Stdlib.compare "
                  ^ apply_code fn.code [ "left" ]
                  ^ " " ^ apply_code fn.code [ "right" ] ^ ") (" ^ list_code ^ ")"))
          | TFn ([ param_ty ], _), Ok (inner, _) when not (Types.equal param_ty inner) ->
              Error.error "sort-by key function must match collection elements"
          | TFn _, Ok _ -> Error.error "sort-by key function must return a comparable value"
          | _, Ok _ -> Error.error "sort-by expects a function"
          | _, Error _ -> Error.error "sort-by expects a collection"))
  | _ -> Error.error "sort-by expects function and collection"

and compile_mapcat current_ns env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg current_ns env fn_form, compile_expr current_ns env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_code collection) with
          | TFn ([ param_ty ], TList ret_inner), Ok (inner, list_code)
            when Types.equal param_ty inner ->
              Ok (typed (TList ret_inner) ("List.concat (List.map " ^ fn.code ^ " (" ^ list_code ^ "))"))
          | TFn ([ param_ty ], TVector ret_inner), Ok (inner, list_code)
            when Types.equal param_ty inner ->
              Ok
                (typed (TList ret_inner)
                   ("List.concat (List.map (fun item -> Rrbvec.to_list "
                  ^ apply_code fn.code [ "item" ] ^ ") (" ^ list_code ^ "))"))
          | TFn ([ param_ty ], TSet ret_inner), Ok (inner, list_code)
            when Types.equal param_ty inner ->
              Ok
                (typed (TList ret_inner)
                   ("List.concat (List.map " ^ fn.code ^ " (" ^ list_code ^ "))"))
          | TFn ([ param_ty ], _), Ok (inner, _) when not (Types.equal param_ty inner) ->
              Error.error "mapcat function argument type does not match collection"
          | TFn _, Ok _ -> Error.error "mapcat function must return a collection"
          | _, Ok _ -> Error.error "mapcat expects a function"
          | _, Error _ -> Error.error "mapcat expects a collection"))
  | _ -> Error.error "mapcat expects function and collection"

and compile_repeatedly current_ns env arg_forms =
  match arg_forms with
  | count_form :: fn_form :: [] -> (
      match (compile_expr current_ns env count_form, compile_function_arg current_ns env fn_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok count, Ok fn -> (
          if not (Types.equal count.ty TInt) then Error.error "repeatedly count must be int"
          else
            match fn.ty with
            | TFn ([], ret) ->
                Ok
                  (typed (TList ret)
                     ("(let rec repeatedly acc n = if n <= 0 then acc else repeatedly ("
                    ^ apply_code fn.code [] ^ " :: acc) (n - 1) in repeatedly [] ("
                    ^ count.code ^ "))"))
            | TFn _ -> Error.error "repeatedly expects a zero-argument function"
            | _ -> Error.error "repeatedly expects a function"))
  | _ -> Error.error "repeatedly expects count and function"

and compile_reductions current_ns env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg current_ns env fn_form, compile_expr current_ns env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_code collection) with
          | TFn ([ acc_ty; item_ty ], ret), Ok (inner, list_code)
            when Types.equal acc_ty inner && Types.equal item_ty inner && Types.equal ret inner ->
              Ok
                (typed (TList inner)
                   ("(match " ^ list_code
                  ^ " with [] -> [] | first :: rest -> let rec reductions current acc xs = match xs with [] -> List.rev acc | item :: tail -> let next = "
                  ^ apply_code fn.code [ "current"; "item" ]
                  ^ " in reductions next (next :: acc) tail in reductions first [first] rest)"))
          | TFn _, Ok _ -> Error.error "reductions function type does not match collection"
          | _, Ok _ -> Error.error "reductions expects a function"
          | _, Error _ -> Error.error "reductions expects a collection"))
  | fn_form :: init_form :: collection_form :: [] -> (
      match
        ( compile_function_arg current_ns env fn_form,
          compile_expr current_ns env init_form,
          compile_expr current_ns env collection_form )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok fn, Ok init, Ok collection -> (
          match (fn.ty, collection_to_list_code collection) with
          | TFn ([ acc_ty; item_ty ], ret), Ok (inner, list_code)
            when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
              Ok
                (typed (TList init.ty)
                   ("(let rec reductions current acc xs = match xs with [] -> List.rev acc | item :: rest -> let next = "
                  ^ apply_code fn.code [ "current"; "item" ]
                  ^ " in reductions next (next :: acc) rest in reductions (" ^ init.code
                  ^ ") [" ^ init.code ^ "] (" ^ list_code ^ "))"))
          | TFn _, Ok _ -> Error.error "reductions function type does not match init and collection"
          | _, Ok _ -> Error.error "reductions expects a function"
          | _, Error _ -> Error.error "reductions expects a collection"))
  | _ -> Error.error "reductions expects function, optional init, and collection"

and compile_split_with current_ns env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg current_ns env fn_form, compile_expr current_ns env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_code collection) with
          | TFn ([ param_ty ], TBool), Ok (inner, list_code) when Types.equal param_ty inner ->
              let pair_code =
                "(let rec split prefix rest = match rest with item :: tail when "
                ^ apply_code fn.code [ "item" ]
                ^ " -> split (item :: prefix) tail | _ -> (List.rev prefix, rest) in split [] ("
                ^ list_code ^ "))"
              in
              let left =
                collection_from_list_code collection.ty ("(fst " ^ pair_code ^ ")")
              in
              let right =
                collection_from_list_code collection.ty ("(snd " ^ pair_code ^ ")")
              in
              Ok (typed (TVector collection.ty) ("Rrbvec.of_list [" ^ left ^ "; " ^ right ^ "]"))
          | TFn _, Ok _ -> Error.error "split-with expects a predicate matching collection elements"
          | _, Ok _ -> Error.error "split-with expects a function"
          | _, Error _ -> Error.error "split-with expects a collection"))
  | _ -> Error.error "split-with expects function and collection"

and compile_partition_by current_ns env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg current_ns env fn_form, compile_expr current_ns env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_code collection) with
          | TFn ([ param_ty ], key_ty), Ok (inner, list_code) when Types.equal param_ty inner ->
              let code =
                "(let rec finish groups current = match current with [] -> List.rev groups | _ -> List.rev (List.rev current :: groups) in let rec partition groups current current_key xs = match xs with [] -> finish groups current | item :: rest -> let key = "
                ^ apply_code fn.code [ "item" ]
                ^ " in match current_key with Some previous when previous = key -> partition groups (item :: current) current_key rest | _ -> let groups = match current with [] -> groups | _ -> List.rev current :: groups in partition groups [item] (Some key) rest in partition [] [] None ("
                ^ list_code ^ "))"
              in
              ignore key_ty;
              Ok (typed (TList (TList inner)) code)
          | TFn _, Ok _ -> Error.error "partition-by function type does not match collection"
          | _, Ok _ -> Error.error "partition-by expects a function"
          | _, Error _ -> Error.error "partition-by expects a collection"))
  | _ -> Error.error "partition-by expects function and collection"

and compile_run_bang current_ns env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg current_ns env fn_form, compile_expr current_ns env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_code collection) with
          | TFn ([ param_ty ], _ret), Ok (inner, list_code) when Types.equal param_ty inner ->
              Ok
                (typed TNil
                   ("(let () = List.iter (fun item -> ignore ("
                  ^ apply_code fn.code [ "item" ]
                  ^ ")) (" ^ list_code ^ ") in ())"))
          | TFn _, Ok _ -> Error.error "run! function type does not match collection"
          | _, Ok _ -> Error.error "run! expects a function"
          | _, Error _ -> Error.error "run! expects a collection"))
  | _ -> Error.error "run! expects function and collection"

and compile_map_indexed current_ns env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg current_ns env fn_form, compile_expr current_ns env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_code collection) with
          | TFn ([ TInt; item_ty ], ret), Ok (inner, list_code) when Types.equal item_ty inner ->
              Ok
                (typed (TList ret)
                   ("List.mapi (fun index item -> "
                  ^ apply_code fn.code [ "index"; "item" ]
                  ^ ") (" ^ list_code ^ ")"))
          | TFn _, Ok _ -> Error.error "map-indexed function type does not match collection"
          | _, Ok _ -> Error.error "map-indexed expects a function"
          | _, Error _ -> Error.error "map-indexed expects a collection"))
  | _ -> Error.error "map-indexed expects function and collection"

and compile_filterv current_ns env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg current_ns env fn_form, compile_expr current_ns env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_code collection) with
          | TFn ([ param_ty ], TBool), Ok (inner, list_code) when Types.equal param_ty inner ->
              Ok
                (typed (TVector inner)
                   ("Rrbvec.of_list (List.filter " ^ fn.code ^ " (" ^ list_code ^ "))"))
          | TFn _, Ok _ ->
              Error.error "filterv expects a predicate matching collection elements"
          | _, Ok _ -> Error.error "filterv expects a function"
          | _, Error _ -> Error.error "filterv expects a collection"))
  | _ -> Error.error "filterv expects function and collection"

and compile_mapv current_ns env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg current_ns env fn_form, compile_expr current_ns env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_code collection) with
          | TFn ([ param_ty ], ret), Ok (inner, list_code) when Types.equal param_ty inner ->
              Ok (typed (TVector ret) ("Rrbvec.of_list (List.map " ^ fn.code ^ " (" ^ list_code ^ "))"))
          | TFn _, Ok _ -> Error.error "mapv function type does not match collection"
          | _, Ok _ -> Error.error "mapv expects a function"
          | _, Error _ -> Error.error "mapv expects a collection"))
  | _ -> Error.error "mapv expects function and collection"

and compile_reduce_kv current_ns env arg_forms =
  match arg_forms with
  | fn_form :: init_form :: collection_form :: [] -> (
      match
        ( compile_function_arg current_ns env fn_form,
          compile_expr current_ns env init_form,
          compile_expr current_ns env collection_form )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok fn, Ok init, Ok collection -> (
          match (fn.ty, collection.ty) with
          | TFn ([ acc_ty; TInt; item_ty ], ret), TVector inner
            when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
              Ok
                (typed init.ty
                   ("List.fold_left (fun acc (index, item) -> "
                  ^ apply_code fn.code [ "acc"; "index"; "item" ]
                  ^ ") (" ^ init.code
                  ^ ") (List.mapi (fun index item -> (index, item)) (Rrbvec.to_list ("
                  ^ collection.code ^ ")))"))
          | TFn _, TVector _ -> Error.error "reduce-kv function type does not match vector"
          | _, TVector _ -> Error.error "reduce-kv expects a function"
          | _ -> Error.error "reduce-kv expects a vector"))
  | _ -> Error.error "reduce-kv expects function, init, and vector"

and compile_some current_ns env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg current_ns env fn_form, compile_expr current_ns env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_code collection) with
          | TFn ([ param_ty ], TBool), Ok (inner, list_code) when Types.equal param_ty inner ->
              Ok (typed TBool ("List.exists " ^ fn.code ^ " (" ^ list_code ^ ")"))
          | TFn _, Ok _ -> Error.error "some expects a predicate matching collection elements"
          | _, Ok _ -> Error.error "some expects a function"
          | _, Error _ -> Error.error "some expects a collection"))
  | _ -> Error.error "some expects function and collection"

and compile_sequence_bool_predicate current_ns env name arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg current_ns env fn_form, compile_expr current_ns env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          let build all_code =
            match name with
            | "every?" -> all_code
            | "not-any?" -> all_code
            | "not-every?" -> "not (" ^ all_code ^ ")"
            | _ -> all_code
          in
          let predicate_code =
            match name with
            | "not-any?" -> "(fun item -> not (" ^ apply_code fn.code [ "item" ] ^ "))"
            | _ -> fn.code
          in
          match (fn.ty, collection.ty) with
          | TFn ([ param_ty ], TBool), TList inner when Types.equal param_ty inner ->
              let all_code = "List.for_all " ^ predicate_code ^ " (" ^ collection.code ^ ")" in
              Ok (typed TBool (build all_code))
          | TFn _, TList _ -> Error.error (name ^ " expects a predicate matching list elements")
          | _, TList _ -> Error.error (name ^ " expects a function")
          | TFn ([ param_ty ], TBool), TVector inner when Types.equal param_ty inner ->
              let all_code = "Rrbvec.for_all " ^ predicate_code ^ " (" ^ collection.code ^ ")" in
              Ok (typed TBool (build all_code))
          | TFn _, TVector _ ->
              Error.error (name ^ " expects a predicate matching vector elements")
          | _, TVector _ -> Error.error (name ^ " expects a function")
          | TFn ([ param_ty ], TBool), TSet inner when Types.equal param_ty inner ->
              let all_code = "List.for_all " ^ predicate_code ^ " (" ^ collection.code ^ ")" in
              Ok (typed TBool (build all_code))
          | TFn _, TSet _ -> Error.error (name ^ " expects a predicate matching set elements")
          | _, TSet _ -> Error.error (name ^ " expects a function")
          | _ -> Error.error (name ^ " expects a list, vector, or set")))
  | _ -> Error.error (name ^ " expects function and collection")

and compile_map_call current_ns env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg current_ns env fn_form, compile_expr current_ns env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection.ty) with
          | TFn ([ param_ty ], ret), TList inner when Types.equal param_ty inner ->
              Ok
                (typed (TList ret)
                   ("List.map " ^ fn.code ^ " (" ^ collection.code ^ ")"))
          | TFn _, TList _ -> Error.error "map function argument type does not match list"
          | _, TList _ -> Error.error "map expects a function"
          | TFn ([ param_ty ], ret), TVector inner when Types.equal param_ty inner ->
              Ok
                (typed (TVector ret)
                   ("Rrbvec.map " ^ fn.code ^ " (" ^ collection.code ^ ")"))
          | TFn _, TVector _ -> Error.error "map function argument type does not match vector"
          | _, TVector _ -> Error.error "map expects a function"
          | TFn ([ param_ty ], ret), TSet inner when Types.equal param_ty inner ->
              Ok
                (typed (TSet ret)
                   ("List.sort_uniq compare (List.map " ^ fn.code ^ " ("
                  ^ collection.code ^ "))"))
          | TFn _, TSet _ -> Error.error "map function argument type does not match set"
          | _, TSet _ -> Error.error "map expects a function"
          | _ -> Error.error "map expects a list, vector, or set"))
  | _ -> Error.error "map expects function and collection"

and compile_filter current_ns env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg current_ns env fn_form, compile_expr current_ns env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection.ty) with
          | TFn ([ param_ty ], TBool), TList inner when Types.equal param_ty inner ->
              Ok
                (typed collection.ty
                   ("List.filter " ^ fn.code ^ " (" ^ collection.code ^ ")"))
          | TFn _, TList _ -> Error.error "filter expects a predicate matching list elements"
          | _, TList _ -> Error.error "filter expects a function"
          | TFn ([ param_ty ], TBool), TVector inner when Types.equal param_ty inner ->
              Ok
                (typed collection.ty
                   ("Rrbvec.filter " ^ fn.code ^ " (" ^ collection.code ^ ")"))
          | TFn _, TVector _ -> Error.error "filter expects a predicate matching vector elements"
          | _, TVector _ -> Error.error "filter expects a function"
          | TFn ([ param_ty ], TBool), TSet inner when Types.equal param_ty inner ->
              Ok
                (typed collection.ty
                   ("List.filter " ^ fn.code ^ " (" ^ collection.code ^ ")"))
          | TFn _, TSet _ -> Error.error "filter expects a predicate matching set elements"
          | _, TSet _ -> Error.error "filter expects a function"
          | _ -> Error.error "filter expects a list, vector, or set"))
  | _ -> Error.error "filter expects function and collection"

and compile_reduce current_ns env arg_forms =
  match arg_forms with
  | fn_form :: init_form :: collection_form :: [] -> (
      match
        ( compile_function_arg current_ns env fn_form,
          compile_expr current_ns env init_form,
          compile_expr current_ns env collection_form )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok fn, Ok init, Ok collection -> (
          match (fn.ty, collection.ty) with
          | TFn ([ acc_ty; item_ty ], ret), TList inner
            when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
              Ok
                (typed init.ty
                   ("List.fold_left " ^ fn.code ^ " (" ^ init.code ^ ") ("
                  ^ collection.code ^ ")"))
          | TFn _, TList _ -> Error.error "reduce function type does not match init and list"
          | _, TList _ -> Error.error "reduce expects a function"
          | TFn ([ acc_ty; item_ty ], ret), TVector inner
            when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
             Ok
                (typed init.ty
                   ("Rrbvec.fold_left " ^ fn.code ^ " (" ^ init.code ^ ") ("
                  ^ collection.code ^ ")"))
          | TFn _, TVector _ -> Error.error "reduce function type does not match init and vector"
          | _, TVector _ -> Error.error "reduce expects a function"
          | TFn ([ acc_ty; item_ty ], ret), TSet inner
            when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
              Ok
                (typed init.ty
                   ("List.fold_left " ^ fn.code ^ " (" ^ init.code ^ ") ("
                  ^ collection.code ^ ")"))
          | TFn _, TSet _ -> Error.error "reduce function type does not match init and set"
          | _, TSet _ -> Error.error "reduce expects a function"
          | _ -> Error.error "reduce expects a list, vector, or set"))
  | _ -> Error.error "reduce expects function, init, and collection"

and compile_apply current_ns env arg_forms =
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
            ( compile_function_arg current_ns env fn_form,
              compile_args_for current_ns env fixed_forms,
              compile_expr current_ns env collection_form )
          with
          | (Error _ as err), _, _ -> err
          | _, (Error _ as err), _ -> err
          | _, _, (Error _ as err) -> err
          | Ok fn, Ok fixed_args, Ok collection -> (
              match collection_to_list_code collection with
              | Error _ -> Error.error "apply expects a list, vector, or set"
              | Ok (inner, list_code) -> (
                  match fn.ty with
                  | TFn ([ TInt; TInt ], TInt)
                    when Types.equal inner TInt
                         && List.for_all (fun arg -> Types.equal arg.ty TInt) fixed_args ->
                      let fixed_code =
                        match fixed_args with
                        | [] -> ""
                        | _ ->
                            "["
                            ^ (fixed_args
                              |> List.map (fun arg -> arg.code)
                              |> String.concat "; ")
                            ^ "] @ "
                      in
                      Ok
                        (typed TInt
                           ("List.fold_left " ^ fn.code ^ " 0 (" ^ fixed_code ^ "("
                          ^ list_code ^ "))"))
                  | TFn ([ TInt; TInt ], TInt) ->
                      Error.error "apply currently supports int binary reducers"
                  | TFn _ -> Error.error "apply currently supports int binary reducers"
                  | _ -> Error.error "apply expects a function"))))
  | _ -> Error.error "apply expects function and collection"

and compile_comp current_ns env arg_forms =
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
                   match compile_function_arg current_ns env form with
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

and compile_partial current_ns env arg_forms =
  match arg_forms with
  | fn_form :: fixed_forms -> (
      match (compile_function_arg current_ns env fn_form, compile_args_for current_ns env fixed_forms) with
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

and compile_identity current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ arg ] -> Ok arg
  | Ok _ -> Error.error "identity expects 1 arguments"

and compile_constantly current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ value ] ->
      Ok
        (typed_ir (TFn ([ TAny ], value.ty))
           (Ocaml_ir.Fun ([ Ocaml_ir.PAny ], value.ocaml_expr)))
  | Ok _ -> Error.error "constantly expects 1 arguments"

and compile_complement current_ns env arg_forms =
  match arg_forms with
  | [ fn_form ] -> (
      match compile_function_arg current_ns env fn_form with
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

and compile_predicate_combinator current_ns env name arg_forms =
  let compile_fns =
    arg_forms
    |> List.fold_left
         (fun acc form ->
           match acc with
           | Error _ as err -> err
           | Ok fns -> (
               match compile_function_arg current_ns env form with
               | Error _ as err -> err
               | Ok fn -> Ok (fn :: fns)))
         (Ok [])
    |> Result.map List.rev
  in
  match compile_fns with
  | Error _ as err -> err
  | Ok [] -> Error.error (name ^ " expects at least 1 predicate")
  | Ok fns -> (
      let rec collect arg_ty codes = function
        | [] -> Ok (arg_ty, List.rev codes)
        | fn :: rest -> (
            match fn.ty with
            | TFn ([ current_arg ], TBool)
              when option_for_all (fun arg_ty -> Types.equal arg_ty current_arg) arg_ty ->
                collect (Some current_arg) (apply_code fn.code [ "x" ] :: codes) rest
            | TFn _ ->
                Error.error (name ^ " expects predicates with the same argument type")
            | _ -> Error.error (name ^ " expects predicates"))
      in
      match collect None [] fns with
      | Error _ as err -> err
      | Ok (None, _) -> Error.error (name ^ " expects at least 1 predicate")
      | Ok (Some arg_ty, codes) ->
          let op = if name = "every-pred" then " && " else " || " in
          Ok
            (typed (TFn ([ arg_ty ], TBool))
               ("(fun x -> " ^ String.concat op codes ^ ")")))

and compile_juxt current_ns env arg_forms =
  let compile_fns =
    arg_forms
    |> List.fold_left
         (fun acc form ->
           match acc with
           | Error _ as err -> err
           | Ok fns -> (
               match compile_function_arg current_ns env form with
               | Error _ as err -> err
               | Ok fn -> Ok (fn :: fns)))
         (Ok [])
    |> Result.map List.rev
  in
  match compile_fns with
  | Error _ as err -> err
  | Ok [] -> Error.error "juxt expects at least 1 function"
  | Ok fns -> (
      let rec collect arg_ty ret_ty codes = function
        | [] -> Ok (arg_ty, ret_ty, List.rev codes)
        | fn :: rest -> (
            match fn.ty with
            | TFn ([ current_arg ], current_ret)
              when option_for_all (fun arg_ty -> Types.equal arg_ty current_arg) arg_ty
                   && option_for_all (fun ret_ty -> Types.equal ret_ty current_ret) ret_ty ->
                collect (Some current_arg) (Some current_ret)
                  (apply_code fn.code [ "x" ] :: codes)
                  rest
            | TFn ([ current_arg ], _)
              when option_for_all (fun arg_ty -> Types.equal arg_ty current_arg) arg_ty ->
                Error.error "juxt functions must return the same type"
            | TFn _ -> Error.error "juxt functions must accept the same argument type"
            | _ -> Error.error "juxt expects functions")
      in
      match collect None None [] fns with
      | Error _ as err -> err
      | Ok (Some arg_ty, Some ret_ty, codes) ->
          Ok
            (typed (TFn ([ arg_ty ], TVector ret_ty))
               ("(fun x -> Rrbvec.of_list [" ^ String.concat "; " codes ^ "])"))
      | Ok _ -> Error.error "juxt expects at least 1 function")

and compile_distinct_question current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok ([] | [ _ ]) -> Ok (typed TBool "true")
  | Ok (first :: _ as args) ->
      if List.for_all (fun arg -> Types.equal first.ty arg.ty) args then
        let values = args |> List.map (fun arg -> arg.code) |> String.concat "; " in
        let len = string_of_int (List.length args) in
        Ok
          (typed TBool
             ("(List.length (List.sort_uniq compare [" ^ values ^ "]) = " ^ len ^ ")"))
      else Error.error "distinct? arguments must have the same type"

and compile_compare current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ left; right ] ->
      if not (Types.equal left.ty right.ty) then
        Error.error "compare arguments must have the same type"
      else if not (comparable_type left.ty) then
        Error.error "compare expects comparable arguments"
      else Ok (typed TInt ("Stdlib.compare (" ^ left.code ^ ") (" ^ right.code ^ ")"))
  | Ok _ -> Error.error "compare expects 2 arguments"

and compile_key_extreme current_ns env name arg_forms =
  match arg_forms with
  | fn_form :: value_forms when value_forms <> [] -> (
      match (compile_function_arg current_ns env fn_form, compile_args_for current_ns env value_forms) with
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
                let compare_op = if name = "max-key" then "> 0" else "< 0" in
                let code =
                  match rest with
                  | [] -> first.code
                  | _ ->
                      "(let key_fn = " ^ fn.code
                      ^ " in let choose best item = if Stdlib.compare (key_fn item) (key_fn best) "
                      ^ compare_op ^ " then item else best in List.fold_left choose ("
                      ^ first.code ^ ") ["
                      ^ (rest |> List.map (fun value -> value.code) |> String.concat "; ")
                      ^ "])"
                in
                Ok (typed first.ty code)
            | TFn _ -> Error.error (name ^ " expects a key function matching values")
            | _ -> Error.error (name ^ " expects a function")))
  | _ -> Error.error (name ^ " expects function and values")

and compile_hash_set current_ns env arg_forms =
  match arg_forms with
  | [] -> Error.error "empty hash-set requires a type annotation"
  | first :: rest -> (
      match compile_expr current_ns env first with
      | Error _ as err -> err
      | Ok first_expr ->
          let rec loop values = function
            | [] ->
                let values = List.rev values |> String.concat "; " in
                Ok
                  (typed (TSet first_expr.ty)
                     ("List.sort_uniq compare [" ^ values ^ "]"))
            | form :: rest -> (
                match compile_expr current_ns env form with
                | Error _ as err -> err
                | Ok expr ->
                    if Types.equal first_expr.ty expr.ty then loop (expr.code :: values) rest
                    else Error.error "hash-set elements must all have the same type")
          in
          loop [ first_expr.code ] rest)

and compile_set_of arg_forms =
  match arg_forms with
  | [ FKeyword keyword ] -> (
      match Type_annotation.of_keyword keyword with
      | Error _ -> Error.error ("unknown set element type " ^ keyword)
      | Ok element_ty -> Ok (typed (TSet element_ty) "[]"))
  | _ -> Error.error "set-of expects one type keyword"

and compile_disj current_ns env arg_forms =
  match arg_forms with
  | collection_form :: value_forms -> (
      match compile_expr current_ns env collection_form with
      | Error _ as err -> err
      | Ok collection -> (
          match collection.ty with
          | TSet inner ->
              let rec remove_values code = function
                | [] -> Ok (typed collection.ty code)
                | value_form :: rest -> (
                    match compile_expr current_ns env value_form with
                    | Error _ as err -> err
                    | Ok value ->
                        if Types.equal inner value.ty then
                          remove_values
                            ("List.filter (fun item -> item <> " ^ value.code ^ ") (" ^ code
                           ^ ")")
                            rest
                        else Error.error "disj value type must match set element type")
              in
              remove_values collection.code value_forms
          | _ -> Error.error "disj expects a set"))
  | [] -> Error.error "disj expects a set"

and compile_args_for current_ns env arg_forms =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | form :: rest -> (
        match compile_expr current_ns env form with
        | Ok expr -> loop (expr :: acc) rest
        | Error _ as err -> err)
  in
  loop [] arg_forms

let compile_defprotocol current_ns env next_type protocol_name method_forms =
  match Protocol.defprotocol_bindings current_ns protocol_name method_forms with
  | Error _ as err -> err
  | Ok bindings ->
      Ok
        ( current_ns,
          env @ bindings,
          next_type,
          Comment ("protocol " ^ protocol_name) )

let compile_extend_type current_ns env next_type receiver_keyword protocol_name method_forms =
  match Type_annotation.of_keyword receiver_keyword with
  | Error _ as err -> err
  | Ok receiver_ty ->
      let compile_method env = function
        | FList (FSymbol method_name :: params :: body_forms) -> (
            match Protocol.lookup_marker current_ns env method_name with
            | None ->
                Error.error
                  ("protocol " ^ protocol_name ^ " does not define method " ^ method_name)
            | Some marker when marker.ocaml_name <> protocol_name ->
                Error.error
                  ("protocol " ^ protocol_name ^ " does not define method " ^ method_name)
            | Some marker -> (
                match Protocol.annotate_receiver receiver_ty params with
                | Error _ as err -> err
                | Ok params -> (
                    match compile_fn current_ns env params body_forms with
                    | Error _ as err -> err
                    | Ok expr -> (
                        match (marker.ty, expr.ty) with
                        | TFn (expected_params, _), TFn (actual_params, _)
                          when List.length expected_params <> List.length actual_params ->
                            Error.error (method_name ^ " called with incompatible arguments")
                        | TFn (_expected_params, expected_ret), TFn (actual_params, actual_ret)
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
                                else if not (Types.equal expected_ret actual_ret) then
                                  Error.error
                                    ("protocol method " ^ method_name ^ " must return "
                                   ^ source_name expected_ret)
                                else (
                                  match Protocol.impl_name method_name receiver_ty with
                                  | None ->
                                      Error.error
                                        ("protocol implementations do not support receiver type "
                                       ^ source_name receiver_ty)
                                  | Some impl_key_name ->
                                      let ocaml_name =
                                        Protocol.impl_ocaml_name current_ns
                                          protocol_name method_name receiver_ty
                                      in
                                      let env_key =
                                        Names.namespaced_key current_ns impl_key_name
                                      in
                                      let binding = Types.binding ocaml_name expr.ty in
                                      Ok
                                        ( env @ [ (env_key, binding) ],
                                          Value_binding
                                            {
                                              pattern = Named ocaml_name;
                                              expression = expr.ocaml_expr;
                                            } )))
                        | _ -> Error.error "protocol method did not compile to a function"))))
        | _ -> Error.error "extend-type methods must be (method-name [params] body)"
      in
      let rec loop env items = function
        | [] ->
            Ok
              ( current_ns,
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

let rec compile_module current_ns env next_type module_path module_segment forms =
  let rec compile_module_form env public_bindings next_type items = function
    | FList [ FSymbol "def"; FSymbol name; expr_form ] -> (
        match compile_expr module_path env expr_form with
        | Error _ as err -> err
        | Ok expr ->
            let local_name = Names.sanitize_name name in
            let key = module_binding_key module_path name in
            let local_binding = Types.binding local_name expr.ty in
            let public_binding =
              Types.binding (module_binding_ocaml_name module_path name) expr.ty
            in
            (match expr.ty with
            | TRecord fields -> (
                match expr.record_values with
                | None -> Error.error "internal error: record expression missing values"
                | Some values ->
                    let type_name = "t" ^ string_of_int next_type in
                    let item =
                      Record_def
                        { var_name = local_name; type_name; fields; values }
                    in
                    Ok
                      ( env @ [ (key, local_binding) ],
                        public_bindings @ [ (key, public_binding) ],
                        next_type + 1,
                        item :: items ))
            | _ ->
                let item =
                  Value_binding
                    { pattern = Named local_name; expression = expr.ocaml_expr }
                in
                Ok
                  ( env @ [ (key, local_binding) ],
                    public_bindings @ [ (key, public_binding) ],
                    next_type,
                    item :: items )))
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
            match expr.ty with
            | TFn _ ->
                let key = module_binding_key module_path name in
                let local_binding =
                  Types.binding ~row_param_types:local_row_types local_name expr.ty
                in
                let public_binding =
                  Types.binding ~row_param_types:public_row_types public_name expr.ty
                in
                let type_items = row_type_items local_row_types param_tys in
                let value_item =
                  Value_binding
                    { pattern = Named local_name; expression = expr.ocaml_expr }
                in
                Ok
                  ( env @ [ (key, local_binding) ],
                    public_bindings @ [ (key, public_binding) ],
                    next_type,
                    Group (type_items @ [ value_item ]) :: items )
            | _ -> Error.error "defn body did not compile to a function"))
    | FList (FSymbol "module" :: FSymbol nested_segment :: nested_forms) -> (
        let nested_path = module_path ^ "." ^ nested_segment in
        match compile_module current_ns env next_type nested_path nested_segment nested_forms with
        | Error _ as err -> err
        | Ok (_current_ns, nested_public_bindings, next_type, nested_item) ->
            Ok
              ( env @ nested_public_bindings,
                public_bindings @ nested_public_bindings,
                next_type,
                nested_item :: items ))
    | _ -> Error.error "module forms must be def, defn, or module"
  and loop env public_bindings next_type items = function
    | [] ->
        let module_name = Names.module_segment_to_ocaml module_segment in
        Ok
          ( current_ns,
            public_bindings,
            next_type,
            Module_def { module_name; items = List.rev items } )
    | form :: rest -> (
        match compile_module_form env public_bindings next_type items form with
        | Error _ as err -> err
        | Ok (env, public_bindings, next_type, items) ->
            loop env public_bindings next_type items rest)
  in
  loop env [] next_type [] forms

let compile_top_level current_ns env next_type = function
  | FList [ FSymbol "def"; FSymbol name; expr_form ] -> (
      match compile_expr current_ns env expr_form with
      | Error _ as err -> err
      | Ok expr ->
          let ocaml_name = Names.ocaml_binding_name current_ns name in
          let env_key = Names.namespaced_key current_ns name in
          (match expr.ty with
          | TRecord fields -> (
              match expr.record_values with
              | None -> Error.error "internal error: record expression missing values"
              | Some values ->
                  let type_name = "t" ^ string_of_int next_type in
                  let binding = Types.binding ocaml_name (TRecord fields) in
                  Ok
                    ( current_ns,
                      env @ [ (env_key, binding) ],
                      next_type + 1,
                      Record_def { var_name = ocaml_name; type_name; fields; values } ))
          | _ ->
              let binding = Types.binding ocaml_name expr.ty in
              Ok
                ( current_ns,
                  env @ [ (env_key, binding) ],
                  next_type,
                  Value_binding
                    { pattern = Named ocaml_name; expression = expr.ocaml_expr } )))
  | FList (FSymbol "defn" :: FSymbol name :: params :: body_forms) -> (
      match prepare_fn current_ns env params body_forms with
      | Error _ as err -> err
      | Ok parts -> (
          let ocaml_name = Names.ocaml_binding_name current_ns name in
          let param_tys =
            parts.param_bindings
            |> List.map (fun (_key, (binding : binding)) -> binding.ty)
          in
          let row_param_types = row_param_type_names ocaml_name param_tys in
          let expr = fn_code ~row_param_type_names:row_param_types parts in
          match expr.ty with
          | TFn _ ->
              let env_key = Names.namespaced_key current_ns name in
              let binding = Types.binding ~row_param_types ocaml_name expr.ty in
              let type_items = row_type_items row_param_types param_tys in
              let value_item =
                Value_binding
                  { pattern = Named ocaml_name; expression = expr.ocaml_expr }
              in
              Ok
                ( current_ns,
                  env @ [ (env_key, binding) ],
                  next_type,
                  Group (type_items @ [ value_item ]) )
          | _ -> Error.error "defn body did not compile to a function"))
  | FList (FSymbol "defprotocol" :: FSymbol protocol_name :: method_forms) ->
      compile_defprotocol current_ns env next_type protocol_name method_forms
  | FList
      (FSymbol "extend-type" :: FKeyword receiver_keyword :: FSymbol protocol_name
      :: method_forms) ->
      compile_extend_type current_ns env next_type receiver_keyword protocol_name
        method_forms
  | FList (FSymbol "module" :: FSymbol module_name :: forms) -> (
      match compile_module current_ns env next_type module_name module_name forms with
      | Error _ as err -> err
      | Ok (current_ns, module_bindings, next_type, item) ->
          Ok (current_ns, env @ module_bindings, next_type, item))
  | FList (FSymbol (("print" | "println") as name) :: args) -> (
      match compile_call current_ns env name args with
      | Error _ as err -> err
      | Ok expr ->
          Ok
            ( current_ns,
              env,
              next_type,
              Value_binding
                { pattern = Unit_pattern; expression = expr.ocaml_expr } ))
  | FList (FSymbol "ns" :: FSymbol namespace :: clauses) -> (
      match Ns_require.parse_requires clauses with
      | Error _ as err -> err
      | Ok specs ->
          let rec apply_specs env = function
            | [] -> Ok env
            | Ns_require.Alias { namespace = required_ns; alias } :: rest ->
                let env =
                  if String.starts_with ~prefix:"ocaml." required_ns then
                    Ns_require.add_ocaml_alias_bindings env required_ns alias
                  else if required_ns = "clojure.string" then
                    Ns_require.add_clojure_string_alias_bindings env alias
                  else Ns_require.add_namespace_alias_bindings env required_ns alias
                in
                apply_specs env rest
            | Ns_require.Refer { namespace = required_ns; names } :: rest ->
                let result =
                  if String.starts_with ~prefix:"ocaml." required_ns then
                    Ns_require.add_ocaml_refer_bindings env namespace required_ns names
                  else if required_ns = "clojure.string" then
                    Ns_require.add_clojure_string_refer_bindings env namespace names
                  else Ns_require.add_namespace_refer_bindings env namespace required_ns names
                in
                (match result with
                | Error _ as err -> err
                | Ok env -> apply_specs env rest)
          in
          (match apply_specs env specs with
          | Error _ as err -> err
          | Ok env -> Ok (namespace, env, next_type, Comment ("ns " ^ namespace))))
  | form -> (
      match compile_expr current_ns env form with
      | Error _ ->
          Error.error
            "expected top-level def, defn, defprotocol, extend-type, print, println, ns, or expression form"
      | Ok expr -> (
          match expr.record_values with
          | Some _ -> Error.error "top-level map literals must be bound with def"
          | None ->
              Ok
                ( current_ns,
                  env,
                  next_type,
                  Value_binding
                    { pattern = Ignore_pattern; expression = expr.ocaml_expr } )))

type state = {
  current_ns : string;
  env : (string * binding) list;
  next_type : int;
  items : compiled_item list;
}

let empty_state = { current_ns = ""; env = []; next_type = 1; items = [] }

let compile_forms_incremental state forms =
  let rec loop current_ns env next_type items = function
    | [] -> Ok (current_ns, env, next_type, List.rev items)
    | form :: rest -> (
        match compile_top_level current_ns env next_type form with
        | Error _ as err -> err
        | Ok (current_ns, env, next_type, item) ->
            loop current_ns env next_type (item :: items) rest)
  in
  match loop state.current_ns state.env state.next_type [] forms with
  | Error _ as err -> err
  | Ok (current_ns, env, next_type, new_items) ->
      let next_state =
        { current_ns; env; next_type; items = state.items @ new_items }
      in
      Ok (next_state, new_items)

let compile_forms forms =
  match compile_forms_incremental empty_state forms with
  | Error _ as err -> err
  | Ok (_state, items) -> Ok items
