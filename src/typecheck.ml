open Ast
open Types

let ensure_int_args name args =
  if List.for_all (fun arg -> Types.equal arg.ty TInt) args then Ok ()
  else Error.error ("expected int arguments for " ^ name)

let ensure_bool expr =
  if Types.equal expr.ty TBool then Ok () else Error.error "if condition must be bool"

let parenthesize code = "(" ^ code ^ ")"

let apply_code fn_code arg_codes =
  match arg_codes with
  | [] -> fn_code
  | _ -> parenthesize (fn_code ^ " " ^ (arg_codes |> List.map parenthesize |> String.concat " "))

let rec drop n xs =
  if n <= 0 then xs
  else match xs with [] -> [] | _ :: rest -> drop (n - 1) rest

let lookup_function current_ns env name =
  match List.assoc_opt (Names.namespaced_key current_ns name) env with
  | Some (binding : binding) -> Ok (typed binding.ty binding.ocaml_name)
  | None -> (
      match name with
      | "+" -> Ok (typed (TFn ([ TInt; TInt ], TInt)) "(fun a b -> a + b)")
      | "-" -> Ok (typed (TFn ([ TInt; TInt ], TInt)) "(fun a b -> a - b)")
      | "*" -> Ok (typed (TFn ([ TInt; TInt ], TInt)) "(fun a b -> a * b)")
      | "/" -> Ok (typed (TFn ([ TInt; TInt ], TInt)) "(fun a b -> a / b)")
      | "inc" -> Ok (typed (TFn ([ TInt ], TInt)) "(fun x -> x + 1)")
      | "dec" -> Ok (typed (TFn ([ TInt ], TInt)) "(fun x -> x - 1)")
      | "not" -> Ok (typed (TFn ([ TBool ], TBool)) "not")
      | _ -> Error.error ("unknown function " ^ name))

let rec compile_expr current_ns (env : (string * binding) list) = function
  | FInt value -> Ok (typed TInt (string_of_int value))
  | FString value -> Ok (typed TString (Codegen.ocaml_string_literal value))
  | FBool true -> Ok (typed TBool "true")
  | FBool false -> Ok (typed TBool "false")
  | FNil -> Ok (typed TNil "()")
  | FKeyword keyword -> Ok (typed TKeyword (Codegen.ocaml_string_literal keyword))
  | FSymbol name -> (
      match List.assoc_opt (Names.namespaced_key current_ns name) env with
      | Some binding -> Ok (typed binding.ty binding.ocaml_name)
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
                let values = List.rev acc |> String.concat "; " in
                Ok
                  (typed (TVector first_expr.ty)
                     ("Rrbvec.of_list [" ^ values ^ "]"))
            | form :: rest -> (
                match compile_expr current_ns env form with
                | Error _ as err -> err
                | Ok expr ->
                    if Types.equal first_expr.ty expr.ty then loop (expr.code :: acc) rest
                    else Error.error "vector elements must all have the same type")
          in
          loop [ first_expr.code ] rest)

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
                   (fun field (_keyword, value) -> (field, value.code))
                   fields pairs
               in
               { ty = TRecord fields; code = "<record>"; record_values = Some values })
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
              (typed then_expr.ty
                 ("(if " ^ condition.code ^ " then " ^ then_expr.code ^ " else "
                ^ else_expr.code ^ ")"))
          else Error.error "if branches must have same type")

and compile_body current_ns env empty_error forms =
  match forms with
  | [] -> Error.error empty_error
  | [ form ] -> compile_expr current_ns env form
  | form :: rest -> (
      match (compile_expr current_ns env form, compile_body current_ns env empty_error rest) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok expr, Ok body ->
          Ok (typed body.ty ("(let _ = " ^ expr.code ^ " in " ^ body.code ^ ")")))

and compile_let current_ns env bindings body_forms =
  match bindings with
  | FVector forms ->
      if List.length forms mod 2 <> 0 then
        Error.error "let bindings require an even number of forms"
      else
        let rec bind env code_parts = function
          | [] -> (
              match
                compile_body current_ns env "let body requires at least one form"
                  body_forms
              with
              | Error _ as err -> err
              | Ok body ->
                  let code =
                    code_parts
                    |> List.fold_left
                         (fun acc (name, value_code) ->
                           "let " ^ name ^ " = " ^ value_code ^ " in " ^ acc)
                         body.code
                  in
                  Ok (typed body.ty ("(" ^ code ^ ")")))
          | FSymbol name :: value_form :: rest -> (
              match compile_expr current_ns env value_form with
              | Error _ as err -> err
              | Ok value ->
                  let ocaml_name = Names.sanitize_name name in
                  let env_key = Names.namespaced_key current_ns name in
                  let binding = { ocaml_name; ty = value.ty } in
                  bind (env @ [ (env_key, binding) ])
                    ((ocaml_name, value.code) :: code_parts)
                    rest)
          | _ -> Error.error "let binding names must be symbols"
        in
        bind env [] forms
  | _ -> Error.error "let bindings must be a vector"

and compile_fn current_ns env params body_forms =
  match Type_annotation.parse_params params with
  | Error _ as err -> err
  | Ok params ->
      let lookup_function_ty name =
        match lookup_function current_ns env name with
        | Ok fn -> Ok fn.ty
        | Error _ as err -> err
      in
      match Type_inference.infer_params ~lookup_function_ty params body_forms with
      | Error _ as err -> err
      | Ok params ->
          let param_bindings =
            params
            |> List.map (fun (name, ty) ->
                   let ocaml_name = Names.sanitize_name name in
                   let env_key = Names.namespaced_key current_ns name in
                   (env_key, { ocaml_name; ty }))
          in
          let env = env @ param_bindings in
          match
            compile_body current_ns env "function body requires at least one form"
              body_forms
          with
          | Error _ as err -> err
          | Ok body ->
              let params =
                param_bindings |> List.map (fun (_key, binding) -> binding.ocaml_name)
              in
              let param_tys =
                param_bindings
                |> List.map (fun (_key, (binding : binding)) -> binding.ty)
              in
              Ok
                (typed (TFn (param_tys, body.ty))
                   ("(fun " ^ String.concat " " params ^ " -> " ^ body.code ^ ")"))

and compile_call current_ns env name arg_forms =
  let compile_args () = compile_args_for current_ns env arg_forms in
  match name with
  | "+" | "-" | "*" | "/" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok args -> (
          match ensure_int_args name args with
          | Error _ as err -> err
          | Ok () -> compile_int_operator name args))
  | "inc" ->
      compile_unary_int current_ns env name (fun code -> "(" ^ code ^ " + 1)") arg_forms
  | "dec" ->
      compile_unary_int current_ns env name (fun code -> "(" ^ code ^ " - 1)") arg_forms
  | "=" | "<" | "<=" | ">" | ">=" -> compile_comparison current_ns env name arg_forms
  | "not" -> compile_not current_ns env arg_forms
  | "nil?" -> compile_predicate current_ns env name arg_forms TNil
  | "some?" -> compile_some_predicate current_ns env arg_forms
  | "true?" -> compile_bool_literal_predicate current_ns env name arg_forms true
  | "false?" -> compile_bool_literal_predicate current_ns env name arg_forms false
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
  | "list-of" -> compile_list_of arg_forms
  | "cons" -> compile_cons current_ns env arg_forms
  | "vector" -> compile_vector current_ns env arg_forms
  | "vector-of" -> compile_vector_of arg_forms
  | "count" -> compile_count current_ns env arg_forms
  | "conj" -> compile_conj current_ns env arg_forms
  | "first" -> compile_first current_ns env arg_forms
  | "second" -> compile_second current_ns env arg_forms
  | "last" -> compile_last current_ns env arg_forms
  | "peek" -> compile_peek current_ns env arg_forms
  | "pop" -> compile_pop current_ns env arg_forms
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
  | "hash-map" -> compile_hash_map current_ns env arg_forms
  | "rest" -> compile_rest current_ns env arg_forms
  | "seq" -> compile_seq current_ns env arg_forms
  | "empty?" -> compile_empty current_ns env arg_forms
  | "map" -> compile_map_call current_ns env arg_forms
  | "filter" -> compile_filter current_ns env arg_forms
  | "reduce" -> compile_reduce current_ns env arg_forms
  | "apply" -> compile_apply current_ns env arg_forms
  | "comp" -> compile_comp current_ns env arg_forms
  | "partial" -> compile_partial current_ns env arg_forms
  | "identity" -> compile_identity current_ns env arg_forms
  | "constantly" -> compile_constantly current_ns env arg_forms
  | "hash-set" -> compile_hash_set current_ns env arg_forms
  | "disj" -> compile_disj current_ns env arg_forms
  | _ -> compile_named_function_call current_ns env name arg_forms

and compile_int_operator name args =
  match (name, args) with
  | "+", [] -> Ok (typed TInt "0")
  | "*", [] -> Ok (typed TInt "1")
  | "/", ([] | [ _ ]) -> Error.error "/ expects at least 2 arguments"
  | _, [] -> Error.error (name ^ " expects at least 1 arguments")
  | _, [ arg ] when name = "-" -> Ok (typed TInt ("(-" ^ arg.code ^ ")"))
  | _, [ arg ] -> Ok (typed TInt arg.code)
  | _, first :: rest ->
      let op =
        match name with
        | "+" -> " + "
        | "-" -> " - "
        | "*" -> " * "
        | "/" -> " / "
        | _ -> " "
      in
      let code =
        rest |> List.fold_left (fun acc arg -> "(" ^ acc ^ op ^ arg.code ^ ")") first.code
      in
      Ok (typed TInt code)

and compile_unary_int current_ns env name build_code arg_forms =
  match arg_forms with
  | [ form ] -> (
      match compile_expr current_ns env form with
      | Error _ as err -> err
      | Ok arg ->
          if Types.equal arg.ty TInt then Ok (typed TInt (build_code arg.code))
          else Error.error ("expected int arguments for " ^ name))
  | _ -> Error.error (name ^ " expects 1 arguments")

and compile_comparison current_ns env name arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok ([] | [ _ ]) -> Ok (typed TBool "true")
  | Ok args ->
      let pairwise_codes op args =
        let rec loop acc = function
          | left :: ((right :: _) as rest) ->
              loop (("(" ^ left.code ^ " " ^ op ^ " " ^ right.code ^ ")") :: acc) rest
          | _ -> List.rev acc
        in
        loop [] args
      in
      if name = "=" then
        let first = List.hd args in
        if List.for_all (fun arg -> Types.equal first.ty arg.ty) args then
          Ok (typed TBool (String.concat " && " (pairwise_codes "=" args)))
        else Error.error "= arguments must have the same type"
      else if List.for_all (fun arg -> Types.equal arg.ty TInt) args then
        Ok (typed TBool (String.concat " && " (pairwise_codes name args)))
      else Error.error ("expected int arguments for " ^ name)

and compile_not current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ arg ] ->
      if Types.equal arg.ty TBool then Ok (typed TBool ("not " ^ arg.code))
      else Error.error "not expects bool"
  | Ok _ -> Error.error "not expects 1 arguments"

and compile_predicate current_ns env name arg_forms expected_ty =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ arg ] -> Ok (typed TBool (string_of_bool (Types.equal arg.ty expected_ty)))
  | Ok _ -> Error.error (name ^ " expects 1 arguments")

and compile_some_predicate current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ arg ] -> Ok (typed TBool (string_of_bool (not (Types.equal arg.ty TNil))))
  | Ok _ -> Error.error "some? expects 1 arguments"

and compile_bool_literal_predicate current_ns env name arg_forms expected =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ arg ] ->
      if Types.equal arg.ty TBool then
        Ok (typed TBool ("(" ^ arg.code ^ " = " ^ string_of_bool expected ^ ")"))
      else Ok (typed TBool "false")
  | Ok _ -> Error.error (name ^ " expects 1 arguments")

and compile_count current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ arg ] -> (
      match arg.ty with
      | TList _ -> Ok (typed TInt ("List.length (" ^ arg.code ^ ")"))
      | TVector _ -> Ok (typed TInt ("Rrbvec.length " ^ arg.code))
      | TSet _ -> Ok (typed TInt ("List.length " ^ arg.code))
      | TRecord fields -> Ok (typed TInt (string_of_int (List.length fields)))
      | TString -> Ok (typed TInt ("String.length " ^ arg.code))
      | _ -> Error.error "count expects a collection or string")
  | Ok _ -> Error.error "count expects 1 arguments"

and compile_list current_ns env forms =
  match forms with
  | [] -> Error.error "empty list requires a type annotation"
  | first :: rest -> (
      match compile_expr current_ns env first with
      | Error _ as err -> err
      | Ok first_expr ->
          let rec loop acc = function
            | [] ->
                let values = List.rev acc |> String.concat "; " in
                Ok (typed (TList first_expr.ty) ("[" ^ values ^ "]"))
            | form :: rest -> (
                match compile_expr current_ns env form with
                | Error _ as err -> err
                | Ok expr ->
                    if Types.equal first_expr.ty expr.ty then loop (expr.code :: acc) rest
                    else Error.error "list elements must all have the same type")
          in
          loop [ first_expr.code ] rest)

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
  | Ok [ collection; value ] -> (
      match collection.ty with
      | TList inner when Types.equal inner value.ty ->
          Ok (typed collection.ty ("(" ^ value.code ^ " :: (" ^ collection.code ^ "))"))
      | TList _ -> Error.error "conj value type must match list element type"
      | TVector inner when Types.equal inner value.ty ->
          Ok
            (typed collection.ty
               ("Rrbvec.push_back (" ^ collection.code ^ ") (" ^ value.code ^ ")"))
      | TVector _ -> Error.error "conj value type must match vector element type"
      | _ -> Error.error "conj expects a list or vector")
  | Ok _ -> Error.error "conj expects 2 arguments"

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

and compile_first current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ collection ] -> (
      match collection.ty with
      | TList inner -> Ok (typed inner ("List.hd (" ^ collection.code ^ ")"))
      | TVector inner -> Ok (typed inner ("Rrbvec.nth (" ^ collection.code ^ ") 0"))
      | _ -> Error.error "first expects a list or vector")
  | Ok _ -> Error.error "first expects 1 arguments"

and compile_second current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ collection ] -> (
      match collection.ty with
      | TList inner -> Ok (typed inner ("List.nth (" ^ collection.code ^ ") 1"))
      | TVector inner -> Ok (typed inner ("Rrbvec.nth (" ^ collection.code ^ ") 1"))
      | _ -> Error.error "second expects a list or vector")
  | Ok _ -> Error.error "second expects 1 arguments"

and compile_last current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ collection ] -> (
      match collection.ty with
      | TList inner -> Ok (typed inner ("List.hd (List.rev (" ^ collection.code ^ "))"))
      | TVector inner ->
          Ok (typed inner ("Option.get (Rrbvec.peek_back (" ^ collection.code ^ "))"))
      | _ -> Error.error "last expects a list or vector")
  | Ok _ -> Error.error "last expects 1 arguments"

and compile_peek current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ collection ] -> (
      match collection.ty with
      | TList inner -> Ok (typed inner ("List.hd (" ^ collection.code ^ ")"))
      | TVector inner ->
          Ok (typed inner ("Option.get (Rrbvec.peek_back (" ^ collection.code ^ "))"))
      | _ -> Error.error "peek expects a list or vector")
  | Ok _ -> Error.error "peek expects 1 arguments"

and compile_pop current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ collection ] -> (
      match collection.ty with
      | TList _ -> Ok (typed collection.ty ("List.tl (" ^ collection.code ^ ")"))
      | TVector _ ->
          Ok
            (typed collection.ty
               ("snd (Option.get (Rrbvec.pop_back (" ^ collection.code ^ ")))"))
      | _ -> Error.error "pop expects a list or vector")
  | Ok _ -> Error.error "pop expects 1 arguments"

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
  | Ok _ -> Error.error "nth expects 2 arguments"

and compile_get current_ns env arg_forms =
  match arg_forms with
  | [ target_form; FKeyword keyword ] -> (
      match compile_expr current_ns env target_form with
      | Error _ as err -> err
      | Ok target -> (
          match target.ty with
          | TRecord fields -> (
              match find_field keyword fields with
              | Some field -> Ok (typed field.ty (Structural_map.field_code target field))
              | None -> Error.error ("unknown field " ^ keyword))
          | _ -> Error.error "get expects a map"))
  | [ _; _ ] -> Error.error "get key must be a keyword"
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
                  Ok (typed field.ty (Structural_map.field_code target field))
              | Some field ->
                  Error.error
                    ("get default for " ^ keyword ^ " must be " ^ source_name field.ty)
              | None -> Ok default)
          | _ -> Error.error "get expects a map"))
  | [ _; _; _ ] -> Error.error "get key must be a keyword"
  | _ -> Error.error "get expects 2 or 3 arguments"

and compile_assoc current_ns env arg_forms =
  match arg_forms with
  | target_form :: pair_forms ->
      let rec compile_pairs acc = function
        | [] -> Ok (List.rev acc)
        | FKeyword keyword :: value_form :: rest -> (
            match compile_expr current_ns env value_form with
            | Error _ as err -> err
            | Ok value -> compile_pairs ((keyword, value) :: acc) rest)
        | _ -> Error.error "assoc expects map followed by keyword/value pairs"
      in
      if pair_forms = [] || List.length pair_forms mod 2 <> 0 then
        Error.error "assoc expects map followed by keyword/value pairs"
      else (
        match (compile_expr current_ns env target_form, compile_pairs [] pair_forms) with
        | (Error _ as err), _ -> err
        | _, (Error _ as err) -> err
        | Ok target, Ok pairs -> Structural_map.assoc_many target pairs)
  | _ -> Error.error "assoc expects map followed by keyword/value pairs"

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
                      let old_code = Structural_map.field_code target field in
                      let value_code =
                        apply_code fn.code
                          (old_code :: List.map (fun arg -> arg.code) extra_args)
                      in
                      Structural_map.update_value target fields keyword ret value_code
                  | TFn (_param_tys, ret) when not (Types.equal ret field.ty) ->
                      Error.error
                        (Printf.sprintf "cannot update %s as %s because it is already %s"
                           keyword (source_name ret) (source_name field.ty))
                  | TFn _ ->
                      Error.error
                        "update function arguments do not match field and extra arguments"
                  | _ -> Error.error "update expects a function"))
          | _ -> Error.error "update expects a map"))
  | _ -> Error.error "update expects map, keyword, function, and optional arguments"

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
  match arg_forms with
  | target_form :: FKeyword keyword :: [] -> (
      match compile_expr current_ns env target_form with
      | Error _ as err -> err
      | Ok target -> (
          match target.ty with
          | TRecord fields ->
              Ok (typed TBool (string_of_bool (Option.is_some (find_field keyword fields))))
            | _ -> Error.error "contains? expects a map"))
  | target_form :: value_form :: [] -> (
      match (compile_expr current_ns env target_form, compile_expr current_ns env value_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok target, Ok value -> (
          match target.ty with
          | TSet inner when Types.equal inner value.ty ->
              Ok (typed TBool ("List.mem (" ^ value.code ^ ") (" ^ target.code ^ ")"))
          | TSet _ -> Error.error "contains? value type must match set element type"
          | _ -> Error.error "contains? expects a map or set"))
  | _ -> Error.error "contains? expects map and keyword"

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
              |> List.map (fun (field : field) -> target.code ^ "." ^ field.ocaml_name)
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
  match lookup_function current_ns env name with
  | Error _ as err -> err
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
              Ok (typed ret (apply_code fn.code (List.map (fun arg -> arg.code) args)))
          | TFn _ -> Error.error (name ^ " called with incompatible arguments")
          | _ -> Error.error (name ^ " is not callable")))

and compile_rest current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ collection ] -> (
      match collection.ty with
      | TList _ -> Ok (typed collection.ty ("List.tl (" ^ collection.code ^ ")"))
      | TVector _ ->
          Ok (typed collection.ty ("Rrbvec.of_list (List.tl (Rrbvec.to_list " ^ collection.code ^ "))"))
      | _ -> Error.error "rest expects a list or vector")
  | Ok _ -> Error.error "rest expects 1 arguments"

and compile_seq current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ collection ] -> (
      match collection.ty with
      | TList _ | TVector _ | TSet _ -> Ok collection
      | _ -> Error.error "seq expects a collection")
  | Ok _ -> Error.error "seq expects 1 arguments"

and compile_empty current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ collection ] -> (
      match collection.ty with
      | TList _ -> Ok (typed TBool ("((" ^ collection.code ^ ") = [])"))
      | TVector _ -> Ok (typed TBool ("Rrbvec.is_empty " ^ collection.code))
      | TSet _ -> Ok (typed TBool ("(" ^ collection.code ^ " = [])"))
      | TString -> Ok (typed TBool ("(" ^ collection.code ^ " = \"\")"))
      | _ -> Error.error "empty? expects a collection or string")
  | Ok _ -> Error.error "empty? expects 1 arguments"

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
          | _ -> Error.error "map expects a vector"))
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
          | _ -> Error.error "filter expects a vector"))
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
          | _ -> Error.error "reduce expects a vector"))
  | _ -> Error.error "reduce expects function, init, and collection"

and compile_apply current_ns env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg current_ns env fn_form, compile_expr current_ns env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection.ty) with
          | TFn ([ TInt; TInt ], TInt), TList TInt ->
              Ok (typed TInt ("List.fold_left " ^ fn.code ^ " 0 (" ^ collection.code ^ ")"))
          | TFn _, TList _ -> Error.error "apply currently supports int binary reducers"
          | _, TList _ -> Error.error "apply expects a function"
          | TFn ([ TInt; TInt ], TInt), TVector TInt ->
              Ok (typed TInt ("Rrbvec.fold_left " ^ fn.code ^ " 0 (" ^ collection.code ^ ")"))
          | TFn _, TVector _ -> Error.error "apply currently supports int binary reducers"
          | _, TVector _ -> Error.error "apply expects a function"
          | _ -> Error.error "apply expects a vector"))
  | _ -> Error.error "apply expects function and collection"

and compile_comp current_ns env arg_forms =
  match arg_forms with
  | [ left_form; right_form ] -> (
      match (compile_function_arg current_ns env left_form, compile_function_arg current_ns env right_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok left, Ok right -> (
          match (left.ty, right.ty) with
          | TFn ([ left_arg ], left_ret), TFn ([ right_arg ], right_ret)
            when Types.equal left_arg right_ret ->
              Ok
                (typed (TFn ([ right_arg ], left_ret))
                   ("(fun x -> " ^ apply_code left.code [ apply_code right.code [ "x" ] ] ^ ")"))
          | TFn _, TFn _ -> Error.error "comp function types do not line up"
          | _ -> Error.error "comp expects functions"))
  | _ -> Error.error "comp expects 2 functions"

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
                let code =
                  "(fun " ^ String.concat " " remaining_names ^ " -> "
                  ^ apply_code fn.code
                      (List.map (fun arg -> arg.code) fixed_args @ remaining_names)
                  ^ ")"
                in
                Ok (typed (TFn (remaining_tys, ret)) code)
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
  | Ok [ value ] -> Ok (typed (TFn ([ TAny ], value.ty)) ("(fun _ -> " ^ value.code ^ ")"))
  | Ok _ -> Error.error "constantly expects 1 arguments"

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

and compile_disj current_ns env arg_forms =
  match compile_args_for current_ns env arg_forms with
  | Error _ as err -> err
  | Ok [ collection; value ] -> (
      match collection.ty with
      | TSet inner when Types.equal inner value.ty ->
          Ok
            (typed collection.ty
               ("List.filter (fun item -> item <> " ^ value.code ^ ") " ^ collection.code))
      | TSet _ -> Error.error "disj value type must match set element type"
      | _ -> Error.error "disj expects a set")
  | Ok _ -> Error.error "disj expects set and value"

and compile_args_for current_ns env arg_forms =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | form :: rest -> (
        match compile_expr current_ns env form with
        | Ok expr -> loop (expr :: acc) rest
        | Error _ as err -> err)
  in
  loop [] arg_forms

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
                  let binding = { ocaml_name; ty = TRecord fields } in
                  Ok
                    ( current_ns,
                      env @ [ (env_key, binding) ],
                      next_type + 1,
                      Record_def { var_name = ocaml_name; type_name; fields; values } ))
          | _ ->
              let binding = { ocaml_name; ty = expr.ty } in
              Ok
                ( current_ns,
                  env @ [ (env_key, binding) ],
                  next_type,
                  Emit ("let " ^ ocaml_name ^ " = " ^ expr.code) )))
  | FList (FSymbol "defn" :: FSymbol name :: params :: body_forms) -> (
      match compile_fn current_ns env params body_forms with
      | Error _ as err -> err
      | Ok expr -> (
          match expr.ty with
          | TFn _ ->
              let ocaml_name = Names.ocaml_binding_name current_ns name in
              let env_key = Names.namespaced_key current_ns name in
              let binding = { ocaml_name; ty = expr.ty } in
              Ok
                ( current_ns,
                  env @ [ (env_key, binding) ],
                  next_type,
                  Emit ("let " ^ ocaml_name ^ " = " ^ expr.code) )
          | _ -> Error.error "defn body did not compile to a function"))
  | FList (FSymbol (("print" | "println") as name) :: args) -> (
      match compile_call current_ns env name args with
      | Error _ as err -> err
      | Ok expr -> Ok (current_ns, env, next_type, Emit ("let () = " ^ expr.code)))
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
                  else Ns_require.add_namespace_alias_bindings env required_ns alias
                in
                apply_specs env rest
            | Ns_require.Refer { namespace = required_ns; names } :: rest ->
                let result =
                  if String.starts_with ~prefix:"ocaml." required_ns then
                    Ns_require.add_ocaml_refer_bindings env namespace required_ns names
                  else Ns_require.add_namespace_refer_bindings env namespace required_ns names
                in
                (match result with
                | Error _ as err -> err
                | Ok env -> apply_specs env rest)
          in
          (match apply_specs env specs with
          | Error _ as err -> err
          | Ok env -> Ok (namespace, env, next_type, Emit ("(* ns " ^ namespace ^ " *)"))))
  | _ -> Error.error "expected top-level def, print, println, or ns form"

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
