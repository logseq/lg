open Ast
open Types

let replace_param name ty params =
  params
  |> List.map (fun (param_name, param_ty) ->
         if param_name = name then (param_name, ty) else (param_name, param_ty))

let constrain_symbol expected_ty params name =
  match List.assoc_opt name params with
  | None -> Ok params
  | Some TUnknown -> Ok (replace_param name expected_ty params)
  | Some _existing_ty -> Ok params

let constrain_seqable element_ty params name =
  match List.assoc_opt name params with
  | None -> Ok params
  | Some TUnknown ->
      Ok (replace_param name (Types.seqable_constraint element_ty) params)
  | Some existing -> (
      match Types.seqable_constraint_element existing with
      | Some TUnknown when element_ty <> TUnknown ->
          Ok (replace_param name (Types.seqable_constraint element_ty) params)
      | Some _ -> Ok params
      | None -> Ok params)

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
  | Some TUnknown -> Ok (replace_param name (TRecord [ make_field keyword field_ty ]) params)
  | Some (TRecord fields) -> (
      match merge_fields fields with
      | Error _ as err -> err
      | Ok fields -> Ok (replace_param name (TRecord fields) params))
  | Some _existing_ty -> Ok params

let rec numeric_form_type params = function
  | FInt _ -> TInt
  | FFloat _ -> TFloat
  | FSymbol name ->
      List.assoc_opt name params |> Option.value ~default:TUnknown
  | FList (FSymbol ("+" | "-" | "*" | "/" | "max" | "min") :: args) ->
      let types = List.map (numeric_form_type params) args in
      if List.exists (Types.equal TFloat) types then TFloat
      else if List.exists (Types.equal TInt) types then TInt
      else TUnknown
  | _ -> TUnknown

let inferred_form_type params = function
  | FInt _ -> TInt
  | FFloat _ -> TFloat
  | FChar _ -> TChar
  | FString _ -> TString
  | FBool _ -> TBool
  | FKeyword _ -> TKeyword
  | FSymbol name ->
      List.assoc_opt name params |> Option.value ~default:TUnknown
  | (FList (FSymbol ("+" | "-" | "*" | "/" | "max" | "min") :: _) as form) ->
      numeric_form_type params form
  | FList [ FSymbol "not"; _ ] -> TBool
  | _ -> TUnknown

let select_fn_arity arities argument_count =
  match
    List.find_opt
      (fun (arity : fn_arity) ->
        Option.is_none arity.rest_param
        && List.length arity.fixed_params = argument_count)
      arities
  with
  | Some arity -> Some arity
  | None ->
      List.find_opt
        (fun (arity : fn_arity) ->
          Option.is_some arity.rest_param
          && argument_count >= List.length arity.fixed_params)
        arities

let infer_params ~lookup_function_ty params body_forms =
  let rec infer_expected expected_ty params = function
    | FSymbol name -> constrain_symbol expected_ty params name
    | FList (FSymbol ("+" | "-" | "*" | "/" | "max" | "min") :: args)
      when Types.equal expected_ty TInt || Types.equal expected_ty TFloat ->
        infer_expected_all expected_ty params args
    | FList [ FKeyword keyword; FSymbol name ] ->
        add_record_field_constraint name keyword expected_ty params
    | FList [ FSymbol "get"; FSymbol name; FKeyword keyword ] ->
        add_record_field_constraint name keyword expected_ty params
    | FList [ FSymbol "ocaml-field"; FSymbol name; FSymbol field_name ] ->
        add_record_field_constraint name (":" ^ field_name) expected_ty params
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
  and infer_collection params = function
    | FSymbol name -> constrain_symbol (TVector TUnknown) params name
    | form -> infer_form params form
  and infer_known_call name params args =
    match lookup_function_ty name with
    | Ok (TFn (param_tys, _ret)) when List.length param_tys = List.length args ->
        List.fold_left2
          (fun acc expected_ty arg ->
            match acc with
            | Error _ as err -> err
            | Ok params -> infer_expected expected_ty params arg)
          (Ok params) param_tys args
    | Ok (TOverloaded_fn arities) -> (
        match select_fn_arity arities (List.length args) with
        | None -> infer_all params args
        | Some arity ->
            let fixed_count = List.length arity.fixed_params in
            let expected_tys =
              arity.fixed_params
              @
              match arity.rest_param with
              | None -> []
              | Some rest_ty ->
                  List.init (List.length args - fixed_count) (fun _ -> rest_ty)
            in
            List.fold_left2
              (fun acc expected_ty arg ->
                match acc with
                | Error _ as err -> err
                | Ok params -> infer_expected expected_ty params arg)
              (Ok params) expected_tys args)
    | _ -> infer_all params args
  and inferred_unary_function_param _params = function
    | FSymbol name -> (
        match lookup_function_ty name with
        | Ok (TFn ([ param_ty ], _)) -> param_ty
        | _ -> TUnknown)
    | FList (FSymbol "fn" :: FVector [ FSymbol name ] :: body_forms) -> (
        match infer_all [ (name, TUnknown) ] body_forms with
        | Ok inferred ->
            List.assoc_opt name inferred |> Option.value ~default:TUnknown
        | Error _ -> TUnknown)
    | _ -> TUnknown
  and inferred_reducer_item params init = function
    | FSymbol name -> (
        match lookup_function_ty name with
        | Ok (TFn ([ _; item_ty ], _)) -> item_ty
        | _ -> TUnknown)
    | FList
        (FSymbol "fn" :: FVector [ FSymbol accumulator; FSymbol item ]
        :: body_forms) ->
        let accumulator_ty = inferred_form_type params init in
        (match
           infer_all [ (accumulator, accumulator_ty); (item, TUnknown) ] body_forms
         with
        | Ok inferred ->
            List.assoc_opt item inferred |> Option.value ~default:TUnknown
        | Error _ -> TUnknown)
    | _ -> TUnknown
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
  and infer_assoc params target pairs =
    let rec infer_pairs params = function
      | [] -> Ok params
      | FKeyword keyword :: value_form :: rest -> (
          match infer_form params value_form with
          | Error _ as err -> err
          | Ok params -> (
              match target with
              | FSymbol name ->
                  let field_ty = inferred_form_type params value_form in
                  (match add_record_field_constraint name keyword field_ty params with
                  | Error _ as err -> err
                  | Ok params -> infer_pairs params rest)
              | _ -> infer_pairs params rest))
      | forms -> infer_all params forms
    in
    match infer_form params target with
    | Error _ as err -> err
    | Ok params -> infer_pairs params pairs
  and infer_match params target clauses =
    let pattern_type = function
      | FInt _ -> Some TInt
      | FString _ -> Some TString
      | FKeyword _ -> Some TKeyword
      | FBool _ -> Some TBool
      | _ -> None
    in
    let rec infer_clauses params = function
      | [] -> Ok params
      | [ form ] -> infer_form params form
      | pattern :: result :: rest ->
          let params =
            match pattern_type pattern with
            | Some expected_ty -> infer_expected expected_ty params target
            | None -> infer_form params target
          in
          (match params with
          | Error _ as err -> err
          | Ok params -> (
              match infer_form params result with
              | Error _ as err -> err
              | Ok params -> infer_clauses params rest))
    in
    infer_clauses params clauses
  and infer_form params = function
    | FList [ FSymbol ("nil?" | "some?"); FSymbol value ] ->
        constrain_symbol (TOcaml_app ("option", [ TUnknown ])) params value
    | FList [ FSymbol "count"; FSymbol collection ] ->
        constrain_seqable TUnknown params collection
    | FList [ FSymbol "ocaml-array-from"; FSymbol collection ] ->
        constrain_seqable TUnknown params collection
    | FList [ FSymbol "ocaml-array-sort!"; FSymbol comparator; _ ] ->
        constrain_symbol
          (TFn ([ TUnknown; TUnknown ], TInt))
          params comparator
    | FList
        [ FSymbol ("ocaml-uncurried-call" | "ocaml-uncurried-compare" as name);
          FSymbol fn;
          left;
          right ] ->
        let return_ty =
          if name = "ocaml-uncurried-compare" then TInt else TUnknown
        in
        constrain_symbol
          (TFn
             ( [ inferred_form_type params left; inferred_form_type params right ],
               return_ty ))
          params fn
    | FList
        [ FSymbol
            ("first" | "second" | "last" | "seq" | "rest" | "next" | "empty?");
          FSymbol collection ] ->
        constrain_seqable TUnknown params collection
    | FList
        [ FSymbol ("nthnext" | "nthrest"); FSymbol collection; count ] -> (
        match infer_expected TInt params count with
        | Error _ as err -> err
        | Ok params -> constrain_seqable TUnknown params collection)
    | FList [ FSymbol "map"; fn; FSymbol collection ] ->
        let element_ty = inferred_unary_function_param params fn in
        constrain_seqable element_ty params collection
    | FList [ FSymbol "reduce"; reducer; init; FSymbol collection ] ->
        let element_ty = inferred_reducer_item params init reducer in
        constrain_seqable element_ty params collection
    | FList (FSymbol ("+" | "-" | "*" | "/" | "max" | "min") :: args) ->
        let expected_ty =
          if List.exists (fun arg -> Types.equal (numeric_form_type params arg) TFloat) args
          then TFloat
          else TInt
        in
        infer_expected_all expected_ty params args
    | FList
        (FSymbol
          ( "bit-and"
          | "bit-or"
          | "bit-xor"
          | "unchecked-add"
          | "unchecked-add-int"
          | "unchecked-subtract"
          | "unchecked-subtract-int"
          | "unchecked-multiply"
          | "unchecked-multiply-int" )
        :: args) ->
        infer_expected_all TInt params args
    | FList
        [
          FSymbol
            ( "inc"
            | "dec"
            | "zero?"
            | "pos?"
            | "neg?"
            | "even?"
            | "odd?"
            | "nat-int?"
            | "pos-int?"
            | "neg-int?"
            | "bit-not"
            | "unchecked-inc"
            | "unchecked-inc-int"
            | "unchecked-dec"
            | "unchecked-dec-int"
            | "unchecked-negate"
            | "unchecked-negate-int" );
          arg;
        ] ->
        infer_expected TInt params arg
    | FList
        [
          FSymbol
            ( "quot"
            | "rem"
            | "mod"
            | "bit-shift-left"
            | "bit-shift-right"
            | "bit-set"
            | "bit-clear"
            | "bit-flip"
            | "bit-test"
            | "bit-shift-right-zero-fill"
            | "unchecked-divide-int"
            | "unchecked-remainder-int" );
          left;
          right;
        ] -> (
        match infer_expected TInt params left with
        | Error _ as err -> err
        | Ok params -> infer_expected TInt params right)
    | FList (FSymbol ("<" | "<=" | ">" | ">=") :: args) ->
        let expected_ty =
          if List.exists (fun arg -> Types.equal (numeric_form_type params arg) TFloat) args
          then TFloat
          else TInt
        in
        infer_expected_all expected_ty params args
    | FList [ FSymbol "not"; arg ] -> infer_expected TBool params arg
    | FList [ FKeyword keyword; FSymbol name ] ->
        add_record_field_constraint name keyword TUnknown params
    | FList [ FSymbol "contains?"; FSymbol name; key ] -> (
        match infer_expected TMap_keys params (FSymbol name) with
        | Error _ as err -> err
        | Ok params -> infer_expected TKeyword params key)
    | FList [ FSymbol "ocaml-field"; FSymbol name; FSymbol field_name ] ->
        add_record_field_constraint name (":" ^ field_name) TUnknown params
    | FList (FSymbol "assoc" :: target :: pairs) ->
        infer_assoc params target pairs
    | FList (FSymbol "str" :: args) ->
        infer_expected_all TString params args
    | FList [ FSymbol "if"; condition; then_form; else_form ] -> (
        match infer_expected TBool params condition with
        | Error _ as err -> err
        | Ok params -> (
            match infer_form params then_form with
            | Error _ as err -> err
            | Ok params -> infer_form params else_form))
    | FList [ FSymbol "if"; condition; then_form ] -> (
        match infer_expected TBool params condition with
        | Error _ as err -> err
        | Ok params -> infer_form params then_form)
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
    | FList (FSymbol "match" :: target :: clauses) ->
        infer_match params target clauses
    | FList
        [
          FSymbol "split-with";
          FList (FSymbol "fn" :: _fn_params :: [ body_form ]);
          collection;
        ] -> (
        match infer_expected TBool params body_form with
        | Error _ as err -> err
        | Ok params -> infer_collection params collection)
    | FList
        [
          FSymbol "partition-by";
          FList (FSymbol "fn" :: _fn_params :: body_forms);
          collection;
        ] -> (
        match infer_all params body_forms with
        | Error _ as err -> err
        | Ok params -> infer_collection params collection)
    | FList [ FSymbol ("butlast" | "dorun" | "doall"); collection ] ->
        infer_collection params collection
    | FList [ FSymbol ("take-last" | "drop-last" | "take-nth" | "split-at" | "bounded-count"); count; collection ] -> (
        match infer_expected TInt params count with
        | Error _ as err -> err
        | Ok params -> infer_collection params collection)
    | FList [ FSymbol "run!"; FList (FSymbol "fn" :: _fn_params :: body_forms); collection ] -> (
        match infer_all params body_forms with
        | Error _ as err -> err
        | Ok params -> infer_collection params collection)
    | FList (FSymbol "do" :: body_forms) -> infer_all params body_forms
    | FList (FSymbol "let" :: bindings :: body_forms) ->
        infer_let params bindings body_forms
    | FList (FSymbol "fn" :: _params :: body_forms) -> infer_all params body_forms
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
    | FInt _ | FFloat _ | FChar _ | FString _ | FRegex _ | FBool _ | FKeyword _
    | FSymbol _ ->
        Ok params
  in
  infer_all params body_forms
