open Ast
open Types

let replace_param name ty params =
  params
  |> List.map (fun (param_name, param_ty) ->
         if param_name = name then (param_name, ty) else (param_name, param_ty))

let constrain_symbol expected_ty params name =
  match List.assoc_opt name params with
  | None -> Ok params
  | Some TAny -> Ok (replace_param name expected_ty params)
  | Some _existing_ty -> Ok params

let add_record_field_constraint name keyword field_ty params =
  let merge_fields fields =
    match find_field keyword fields with
    | None -> Ok (make_field keyword field_ty :: fields)
    | Some field when Types.equal field.ty field_ty -> Ok fields
    | Some field ->
        Error.error
          ("cannot infer " ^ keyword ^ " as " ^ Types.source_name field_ty
         ^ " because it is already " ^ Types.source_name field.ty)
  in
  match List.assoc_opt name params with
  | None -> Ok params
  | Some TAny -> Ok (replace_param name (TRecord [ make_field keyword field_ty ]) params)
  | Some (TRecord fields) -> (
      match merge_fields fields with
      | Error _ as err -> err
      | Ok fields -> Ok (replace_param name (TRecord fields) params))
  | Some _existing_ty -> Ok params

let infer_params ~lookup_function_ty params body_forms =
  let rec infer_expected expected_ty params = function
    | FSymbol name -> constrain_symbol expected_ty params name
    | FList [ FKeyword keyword; FSymbol name ] ->
        add_record_field_constraint name keyword expected_ty params
    | FList [ FSymbol "get"; FSymbol name; FKeyword keyword ] ->
        add_record_field_constraint name keyword expected_ty params
    | form -> infer_form params form
  and infer_all params forms =
    let rec loop params = function
      | [] -> Ok params
      | form :: rest -> (
          match infer_form params form with
          | Error _ as err -> err
          | Ok params -> loop params rest)
    in
    loop params forms
  and infer_expected_all expected_ty params forms =
    let rec loop params = function
      | [] -> Ok params
      | form :: rest -> (
          match infer_expected expected_ty params form with
          | Error _ as err -> err
          | Ok params -> loop params rest)
    in
    loop params forms
  and infer_known_call name params args =
    match lookup_function_ty name with
    | Ok (TFn (param_tys, _ret)) when List.length param_tys = List.length args ->
        List.fold_left2
          (fun acc expected_ty arg ->
            match acc with
            | Error _ as err -> err
            | Ok params -> infer_expected expected_ty params arg)
          (Ok params) param_tys args
    | _ -> infer_all params args
  and infer_let params bindings body_forms =
    match bindings with
    | FVector forms ->
        let rec infer_values params = function
          | [] -> Ok params
          | FSymbol _name :: value_form :: rest -> (
              match infer_form params value_form with
              | Error _ as err -> err
              | Ok params -> infer_values params rest)
          | _ -> Ok params
        in
        (match infer_values params forms with
        | Error _ as err -> err
        | Ok params -> infer_all params body_forms)
    | _ -> infer_all params body_forms
  and infer_form params = function
    | FList
        (FSymbol
          ( "+"
          | "-"
          | "*"
          | "/"
          | "max"
          | "min"
          | "bit-and"
          | "bit-or"
          | "bit-xor" )
        :: args) ->
        infer_expected_all TInt params args
    | FList [ FSymbol ("inc" | "dec" | "zero?" | "pos?" | "neg?" | "even?" | "odd?" | "bit-not"); arg ] ->
        infer_expected TInt params arg
    | FList [ FSymbol ("quot" | "rem" | "mod" | "bit-shift-left" | "bit-shift-right"); left; right ] -> (
        match infer_expected TInt params left with
        | Error _ as err -> err
        | Ok params -> infer_expected TInt params right)
    | FList (FSymbol ("<" | "<=" | ">" | ">=") :: args) ->
        infer_expected_all TInt params args
    | FList [ FSymbol "not"; arg ] -> infer_expected TBool params arg
    | FList [ FSymbol "if"; condition; then_form; else_form ] -> (
        match infer_expected TBool params condition with
        | Error _ as err -> err
        | Ok params -> (
            match infer_form params then_form with
            | Error _ as err -> err
            | Ok params -> infer_form params else_form))
    | FList [ FSymbol "if-not"; condition; then_form; else_form ] -> (
        match infer_expected TBool params condition with
        | Error _ as err -> err
        | Ok params -> (
            match infer_form params then_form with
            | Error _ as err -> err
            | Ok params -> infer_form params else_form))
    | FList (FSymbol "when" :: condition :: body_forms) -> (
        match infer_expected TBool params condition with
        | Error _ as err -> err
        | Ok params -> infer_all params body_forms)
    | FList (FSymbol "cond" :: clauses) ->
        let rec infer_clauses params = function
          | [] -> Ok params
          | [ form ] -> infer_form params form
          | FKeyword ":else" :: value_form :: rest -> (
              match infer_form params value_form with
              | Error _ as err -> err
              | Ok params -> infer_clauses params rest)
          | test_form :: value_form :: rest -> (
              match infer_expected TBool params test_form with
              | Error _ as err -> err
              | Ok params -> (
                  match infer_form params value_form with
                  | Error _ as err -> err
                  | Ok params -> infer_clauses params rest))
        in
        infer_clauses params clauses
    | FList (FSymbol "do" :: body_forms) -> infer_all params body_forms
    | FList (FSymbol "let" :: bindings :: body_forms) ->
        infer_let params bindings body_forms
    | FList (FSymbol "fn" :: _params :: _body_forms) -> Ok params
    | FList (FSymbol name :: args) -> infer_known_call name params args
    | FVector forms -> infer_all params forms
    | FMap pairs ->
        pairs
        |> List.fold_left
             (fun acc (_key, value) ->
               match acc with
               | Error _ as err -> err
               | Ok params -> infer_form params value)
             (Ok params)
    | FList forms -> infer_all params forms
    | FInt _ | FString _ | FBool _ | FNil | FKeyword _ | FSymbol _ -> Ok params
  in
  infer_all params body_forms
