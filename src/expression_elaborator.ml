open Ast
open Types
open Expression_support

module Env = Compiler_environment

let rec compile_expr scope (env : Env.t) form =
  match compile_expr_unlocated scope env form with
  | Error _ as err -> err
  | Ok expression -> (
      match Source_context.find form with
      | None -> Ok expression
      | Some location ->
          let node_id = Source_node_id.of_location location in
          Ok
            { expression with
              ocaml_expr = Ocaml_ir.Located (node_id, location, expression.ocaml_expr);
            })

and compile_expr_unlocated scope (env : Env.t) = function
  | FInt value -> Ok (typed_ir TInt (Ocaml_ir.Int value))
  | FFloat value -> Ok (typed_ir TFloat (Ocaml_ir.Float value))
  | FChar value -> Ok (typed_ir TChar (Ocaml_ir.Char value))
  | FString value -> Ok (typed_ir TString (Ocaml_ir.String value))
  | FBool value -> Ok (typed_ir TBool (Ocaml_ir.Bool value))
  | FKeyword keyword -> Ok (typed_ir TKeyword (Ocaml_ir.String keyword))
  | FSymbol name -> (
      match Env.find_opt (Names.scoped_key scope name) env with
      | Some { ty = TFn ([], return_ty); _ } when is_constructor_name name ->
          Ok (typed_ir return_ty (Ocaml_ir.Constructor (name, None)))
      | Some binding -> Ok (typed_ir binding.ty (Ocaml_ir.Ident binding.ocaml_name))
      | None when name = "None" ->
          Ok (typed_ir (TOcaml_app ("option", [ TAny ])) (Ocaml_ir.Constructor (name, None)))
      | None -> Error.error ("unknown symbol " ^ name))
  | FVector forms -> compile_vector scope env forms
  | FMap pairs -> compile_map scope env pairs
  | FList (FSymbol "loop" :: bindings :: body_forms) ->
      compile_loop scope env bindings body_forms
  | FList (FSymbol "recur" :: _) ->
      Error.error "recur is only valid in a loop tail position"
  | FList (FSymbol "let" :: bindings :: body_forms) ->
      compile_let scope env bindings body_forms
  | FList (FSymbol "fn" :: params :: body_forms) ->
      compile_fn scope env params body_forms
  | FList (FSymbol "do" :: body_forms) ->
      compile_body scope env "do requires at least one form" body_forms
  | FList [ FKeyword keyword; target ] ->
      compile_get scope env [ target; FKeyword keyword ]
  | FList (FKeyword _ :: _) -> Error.error "keyword lookup expects one argument"
  | FList (FSymbol "if" :: condition :: then_form :: else_form :: []) ->
      compile_if scope env condition then_form else_form
  | FList (FSymbol "if-not" :: condition :: then_form :: else_form :: []) ->
      compile_if_not scope env condition then_form else_form
  | FList (FSymbol "when" :: condition :: body_forms) ->
      compile_when scope env condition body_forms
  | FList (FSymbol "cond" :: clauses) -> compile_cond scope env clauses
  | FList (FSymbol "match" :: target :: clauses) ->
      compile_match scope env target clauses
  | FList (FSymbol "try" :: forms) -> compile_try scope env forms
  | FList (FSymbol name :: args) -> compile_call scope env name args
  | FList [] -> Error.error "empty list is not callable"
  | FList _ -> Error.error "call head must be a symbol"

and compile_vector scope env forms =
  Special_form_elaborator.compile_vector ~compile_expr scope env forms

and compile_map scope env pairs =
  Special_form_elaborator.compile_map ~compile_expr scope env pairs

and compile_if scope env condition then_form else_form =
  Special_form_elaborator.compile_if ~compile_expr scope env condition then_form else_form

and compile_if_not scope env condition then_form else_form =
  Special_form_elaborator.compile_if_not ~compile_expr scope env condition then_form else_form

and compile_when scope env condition body_forms =
  Special_form_elaborator.compile_when ~compile_expr scope env condition body_forms

and compile_cond scope env clauses =
  Special_form_elaborator.compile_cond ~compile_expr scope env clauses

and compile_match scope env target_form clauses =
  Special_form_elaborator.compile_match ~compile_expr scope env target_form clauses

and compile_body scope env empty_error forms =
  Special_form_elaborator.compile_body ~compile_expr scope env empty_error forms

and compile_try scope env forms =
  Special_form_elaborator.compile_try ~compile_expr scope env forms

and loop_branch_type left right =
  Special_form_elaborator.loop_branch_type ~compile_expr left right

and compile_recur scope env loop_name param_tys arg_forms =
  Special_form_elaborator.compile_recur ~compile_expr scope env loop_name param_tys arg_forms

and compile_loop_tail scope env loop_name param_tys form =
  Special_form_elaborator.compile_loop_tail ~compile_expr scope env loop_name param_tys form

and compile_loop_tail_body scope env loop_name param_tys forms =
  Special_form_elaborator.compile_loop_tail_body ~compile_expr scope env loop_name param_tys forms

and compile_loop scope env bindings body_forms =
  Special_form_elaborator.compile_loop ~compile_expr scope env bindings body_forms

and compile_let scope env bindings body_forms =
  Special_form_elaborator.compile_let ~compile_expr scope env bindings body_forms
and prepare_fn ?(param_type_overrides = []) scope env params body_forms =
  let lookup_function_ty name =
    match lookup_function scope env name with
    | Ok fn -> Ok fn.ty
    | Error _ as err -> err
  in
  Function_elaborator.prepare ~param_type_overrides ~lookup_function_ty
    ~compile_body scope env params body_forms

and fn_code ?(row_param_type_names = []) parts =
  Function_elaborator.fn_code ~row_param_type_names parts

and compile_fn ?(param_type_overrides = []) scope env params body_forms =
  match prepare_fn ~param_type_overrides scope env params body_forms with
  | Error _ as err -> err
  | Ok parts -> Ok (fn_code parts)

and compile_ocaml_arguments scope env forms =
  let rec parse acc = function
    | [] -> Ok (List.rev acc)
    | FKeyword label :: [] ->
        Error.error ("OCaml argument label " ^ label ^ " requires a value")
    | FKeyword label :: value_form :: rest ->
        let label = String.sub label 1 (String.length label - 1) in
        parse ((Some label, value_form) :: acc) rest
    | value_form :: rest -> parse ((None, value_form) :: acc) rest
  in
  let rec compile acc = function
    | [] -> Ok (List.rev acc)
    | (label, form) :: rest -> (
        match compile_expr scope env form with
        | Error _ as err -> err
        | Ok argument -> compile ((label, argument) :: acc) rest)
  in
  match parse [] forms with
  | Error _ as err -> err
  | Ok arguments -> compile [] arguments

and ocaml_apply function_name arguments =
  if List.exists (fun (label, _) -> Option.is_some label) arguments then
    Ocaml_ir.Labelled_apply
      ( Ocaml_ir.Ident function_name,
        List.map
          (fun (label, argument) -> (label, argument.ocaml_expr))
          arguments )
  else
    Ocaml_ir.Apply
      ( Ocaml_ir.Ident function_name,
        List.map (fun (_, argument) -> argument.ocaml_expr) arguments )

and compile_call scope env name arg_forms =
  let compile_args () = compile_args_for scope env arg_forms in
  let constructor ?(display_name = name) ?(constructor_name = name) return_ty
      expected_arity =
    match compile_args () with
    | Error _ as err -> err
    | Ok args when List.length args <> expected_arity ->
        Error.error
          (display_name ^ " expects " ^ string_of_int expected_arity ^ " arguments")
    | Ok args ->
        let payload =
          match args with
          | [] -> None
          | [ value ] -> Some value.ocaml_expr
          | values -> Some (Ocaml_ir.Tuple (List.map (fun value -> value.ocaml_expr) values))
        in
        Ok
          (typed_ir (return_ty args)
             (Ocaml_ir.Constructor (constructor_name, payload)))
  in
  match name with
  | "raise" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ arg ] ->
          Ok
            (typed_ir TAny
               (Ocaml_ir.Apply (Ocaml_ir.Ident "raise", [ arg.ocaml_expr ])))
      | Ok _ -> Error.error "raise expects 1 arguments")
  | "Some" ->
      constructor
        (function [ value ] -> TOcaml_app ("option", [ value.ty ]) | _ -> TAny)
        1
  | "None" -> constructor (fun _ -> TOcaml_app ("option", [ TAny ])) 0
  | "Ok" ->
      constructor
        (function
          | [ value ] -> TOcaml_app ("result", [ value.ty; TAny ])
          | _ -> TAny)
        1
  | "Error" ->
      constructor
        (function
          | [ value ] -> TOcaml_app ("result", [ TAny; value.ty ])
          | _ -> TAny)
        1
  | "ocaml-array" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [] -> Error.error "empty OCaml array requires a type"
      | Ok (first :: rest as values) ->
          if List.for_all (fun value -> Types.equal first.ty value.ty) rest then
            Ok
              (typed_ir (TArray first.ty)
                 (Ocaml_ir.Array (List.map (fun value -> value.ocaml_expr) values)))
          else Error.error "OCaml array elements must have the same type")
  | "ocaml-array-of" -> (
      match arg_forms with
      | [ FKeyword keyword ] -> (
          match Type_annotation.of_keyword keyword with
          | Error _ as err -> err
          | Ok element_ty -> Ok (typed_ir (TArray element_ty) (Ocaml_ir.Array [])))
      | _ -> Error.error "ocaml-array-of expects one type")
  | "ocaml-array-get" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ array; index ] -> (
          match array.ty with
          | TArray element_ty ->
              if Types.equal index.ty TInt then
                Ok
                  (typed_ir element_ty
                     (apply "Array.get" [ array.ocaml_expr; index.ocaml_expr ]))
              else Error.error "OCaml array index must be int"
          | _ -> Error.error "ocaml-array-get expects an OCaml array")
      | Ok _ -> Error.error "ocaml-array-get expects 2 arguments")
  | "ocaml-array-set!" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ array; index; value ] -> (
          match array.ty with
          | TArray element_ty ->
              if not (Types.equal index.ty TInt) then
                Error.error "OCaml array index must be int"
              else if not (Types.equal element_ty value.ty) then
                Error.error "OCaml array value must match element type"
              else
                Ok
                  (typed_ir TUnit
                     (apply "Array.set"
                        [ array.ocaml_expr; index.ocaml_expr; value.ocaml_expr ]))
          | _ -> Error.error "ocaml-array-set! expects an OCaml array")
      | Ok _ -> Error.error "ocaml-array-set! expects 3 arguments")
  | "ocaml-ref" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ value ] -> Ok (typed_ir (TRef value.ty) (apply "ref" [ value.ocaml_expr ]))
      | Ok _ -> Error.error "ocaml-ref expects 1 argument")
  | "ocaml-deref" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ value ] -> (
          match value.ty with
          | TRef referenced_ty ->
              Ok (typed_ir referenced_ty (Ocaml_ir.Prefix ("!", value.ocaml_expr)))
          | _ -> Error.error "ocaml-deref expects an OCaml ref")
      | Ok _ -> Error.error "ocaml-deref expects 1 argument")
  | "ocaml-reset!" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ reference; value ] -> (
          match reference.ty with
          | TRef referenced_ty ->
              if Types.equal referenced_ty value.ty then
                Ok
                  (typed_ir TUnit
                     (Ocaml_ir.Infix (":=", reference.ocaml_expr, value.ocaml_expr)))
              else Error.error "OCaml ref value must match referenced type"
          | _ -> Error.error "ocaml-reset! expects an OCaml ref")
      | Ok _ -> Error.error "ocaml-reset! expects 2 arguments")
  | "ocaml-call" -> (
      match arg_forms with
      | FKeyword return_keyword :: FSymbol function_name :: value_forms -> (
          match Type_annotation.of_keyword return_keyword with
          | Error _ -> Error.error ("unknown ocaml-call return type " ^ return_keyword)
          | Ok return_ty -> (
              match compile_ocaml_arguments scope env value_forms with
              | Error _ as err -> err
              | Ok args ->
                  let function_name =
                    resolve_ocaml_call_target scope env function_name
                  in
                  Ok (typed_ir return_ty (ocaml_apply function_name args))))
      | FSymbol function_name :: value_forms -> (
          compile_inferred_ocaml_call scope env function_name value_forms)
      | FKeyword _ :: _ ->
          Error.error "ocaml-call function must be a symbol"
      | _ -> Error.error "ocaml-call expects return type, function, and arguments")
  | "ocaml-some" ->
      constructor ~display_name:"ocaml-some" ~constructor_name:"Some"
        (fun _ -> TOcaml "option") 1
  | "ocaml-none" ->
      constructor ~display_name:"ocaml-none" ~constructor_name:"None"
        (fun _ -> TOcaml "option") 0
  | "ocaml-ok" ->
      constructor ~display_name:"ocaml-ok" ~constructor_name:"Ok"
        (fun _ -> TOcaml "result") 1
  | "ocaml-error" ->
      constructor ~display_name:"ocaml-error" ~constructor_name:"Error"
        (fun _ -> TOcaml "result") 1
  | "ocaml-tuple" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok ([] | [ _ ]) -> Error.error "ocaml-tuple expects at least 2 values"
      | Ok values ->
          Ok
            (typed_ir
               (TTuple (List.map (fun value -> value.ty) values))
               (Ocaml_ir.Tuple (List.map (fun value -> value.ocaml_expr) values))))
  | "ocaml-record" -> (
      let field_value record field_form =
        match field_form with
        | FList [ FSymbol field_name; value_form ] -> (
            let ocaml_name = Names.sanitize_name field_name in
            match
              List.find_opt
                (fun (field : field) -> field.ocaml_name = ocaml_name)
                record.fields
            with
            | None -> Error.error ("unknown record field " ^ field_name)
            | Some field -> (
                match compile_expr scope env value_form with
                | Error _ as err -> err
                | Ok value -> Ok (field, value)))
        | _ -> Error.error "ocaml-record fields must be (name value)"
      in
      let rec compile_fields record acc seen = function
        | [] -> Ok (List.rev acc)
        | field_form :: rest -> (
            match field_value record field_form with
            | Error _ as err -> err
            | Ok ((field, _value) as pair) ->
                if List.mem field.ocaml_name seen then
                  Error.error "duplicate record field name"
                else compile_fields record (pair :: acc) (field.ocaml_name :: seen) rest)
      in
      match arg_forms with
      | FSymbol type_name :: field_forms -> (
          match lookup_record_type scope env type_name with
          | Error _ as err -> err
          | Ok (record : named_record) -> (
              match compile_fields record [] [] field_forms with
              | Error _ as err -> err
              | Ok values ->
                  let missing =
                    record.fields
                    |> List.filter (fun (field : field) ->
                           not
                             (List.exists
                                (fun ((actual : field), _) ->
                                  actual.ocaml_name = field.ocaml_name)
                                values))
                  in
                  if missing <> [] then Error.error "record value is missing fields"
                  else
                    Ok
                      {
                        (typed_ir
                           (TNamed_record record)
                           (Ocaml_ir.Record
                              ( List.map
                                  (fun ((field : field), value) ->
                                    (field.ocaml_name, value.ocaml_expr))
                                  values,
                                Some
                                  (record_type_application record.type_name
                                     record.type_parameters) )))
                        with
                        record_values =
                          Some
                            (List.map
                               (fun ((field : field), value) -> (field, value.ocaml_expr))
                               values);
                      }))
      | _ -> Error.error "ocaml-record expects a record type and fields")
  | "ocaml-field" -> (
      match arg_forms with
      | [ target_form; FSymbol field_name ] -> (
          match compile_expr scope env target_form with
          | Error _ as err -> err
          | Ok target -> (
              let fields =
                match target.ty with
                | TRecord fields | TNamed_record { fields; _ } -> Ok fields
                | _ -> Error.error "ocaml-field expects a record value"
              in
              match fields with
              | Error _ as err -> err
              | Ok fields -> (
                  let ocaml_name = Names.sanitize_name field_name in
                  match
                    List.find_opt
                      (fun (field : field) -> field.ocaml_name = ocaml_name)
                      fields
                  with
                  | None -> Error.error ("unknown record field " ^ field_name)
                  | Some field ->
                      Ok
                        (typed_ir field.ty
                           (Ocaml_ir.Field (target.ocaml_expr, field.ocaml_name))))))
      | _ -> Error.error "ocaml-field expects record value and field name")
  | "ocaml-construct" -> (
      match arg_forms with
      | FSymbol constructor_name :: payload_forms -> (
          match compile_args_for scope env payload_forms with
          | Error _ as err -> err
          | Ok payloads -> (
              let constructor_ty =
                match lookup_binding scope env constructor_name with
                | Ok { ty = TFn (payload_tys, ret); _ }
                  when List.length payload_tys = List.length payloads ->
                    Ok ret
                | Ok { ty = TFn _; _ } ->
                    Error.error "ocaml-construct payload arity mismatch"
                | Ok _ -> Error.error (constructor_name ^ " is not a constructor")
                | Error _ -> Ok (TOcaml "variant")
              in
              match constructor_ty with
              | Error _ as err -> err
              | Ok constructor_ty ->
                  let payload_expr =
                    match payloads with
                    | [] -> None
                    | [ payload ] -> Some payload.ocaml_expr
                    | _ ->
                        Some
                          (Ocaml_ir.Tuple
                             (List.map (fun payload -> payload.ocaml_expr) payloads))
                  in
                  Ok
                    (typed_ir constructor_ty
                       (Ocaml_ir.Constructor (constructor_name, payload_expr)))))
      | FKeyword _ :: _ -> Error.error "ocaml-construct constructor must be a symbol"
      | _ -> Error.error "ocaml-construct expects a constructor name")
  | "+" | "-" | "*" | "/" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok args -> (
          match Core_int.expect_int_args name args with
          | Error _ as err -> err
          | Ok () -> Core_int.compile_operator name args))
  | "inc" ->
      compile_int_unary_call scope env name
        (fun expression -> Ocaml_ir.Infix ("+", expression, Ocaml_ir.Int 1))
        arg_forms
  | "dec" ->
      compile_int_unary_call scope env name
        (fun expression -> Ocaml_ir.Infix ("-", expression, Ocaml_ir.Int 1))
        arg_forms
  | "=" | "not=" | "<" | "<=" | ">" | ">=" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok args -> Core_compare.compile name args)
  | "not" | "true?" | "false?" | "int?" | "number?"
  | "string?" | "keyword?" | "boolean?" | "vector?" | "list?" | "seq?" | "set?"
  | "map?" | "fn?" | "coll?" | "associative?" | "indexed?" | "seqable?" | "counted?"
    -> compile_boolean_call scope env name arg_forms
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
      compile_int_unary_call scope env name
        (fun expression -> Ocaml_ir.Infix ("=", expression, Ocaml_ir.Int 0))
        arg_forms
      |> Result.map (fun expr -> { expr with ty = TBool })
  | "pos?" ->
      compile_int_unary_call scope env name
        (fun expression -> Ocaml_ir.Infix (">", expression, Ocaml_ir.Int 0))
        arg_forms
      |> Result.map (fun expr -> { expr with ty = TBool })
  | "neg?" ->
      compile_int_unary_call scope env name
        (fun expression -> Ocaml_ir.Infix ("<", expression, Ocaml_ir.Int 0))
        arg_forms
      |> Result.map (fun expr -> { expr with ty = TBool })
  | "even?" ->
      compile_int_unary_call scope env name
        (fun expression ->
          Ocaml_ir.Infix
            ( "=",
              Ocaml_ir.Infix ("mod", expression, Ocaml_ir.Int 2),
              Ocaml_ir.Int 0 ))
        arg_forms
      |> Result.map (fun expr -> { expr with ty = TBool })
  | "odd?" ->
      compile_int_unary_call scope env name
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
          let expr =
            match args with
            | [] -> Ocaml_ir.String ""
            | _ -> args |> List.map (Codegen.stringify_expr_ir ~pr:false) |> Codegen.concat_expr
          in
          Ok (typed_ir TString expr))
  | "subs" -> compile_subs scope env arg_forms
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
      compile_int_unary_call scope env name
        (fun expression -> Ocaml_ir.Prefix ("lnot", expression))
        arg_forms
  | "bit-shift-left" | "bit-shift-right" ->
      (match compile_args () with
      | Error _ as err -> err
      | Ok args -> Core_int.compile_binary name args)
  | "pr-str" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ arg ] -> Ok (typed_ir TString (Codegen.stringify_expr_ir ~pr:true arg))
      | Ok _ -> Error.error "pr-str expects 1 arguments")
  | "print" | "println" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok [ arg ] ->
          let printer = if name = "print" then "print_string" else "print_endline" in
          Ok
            (typed_ir TUnit
               (Ocaml_ir.Apply (Ocaml_ir.Ident printer, [ Codegen.print_expr_ir arg ])))
      | Ok _ -> Error.error (name ^ " expects 1 arguments"))
  | "list" -> compile_list scope env arg_forms
  | "list*" -> compile_list_star scope env arg_forms
  | "range" -> compile_range scope env arg_forms
  | "list-of" -> compile_list_of arg_forms
  | "cons" -> compile_cons scope env arg_forms
  | "vector" -> compile_vector scope env arg_forms
  | "vector-of" -> compile_vector_of arg_forms
  | "count" -> compile_collection_call scope env name arg_forms
  | "conj" -> compile_conj scope env arg_forms
  | "first" | "second" | "last" | "peek" | "pop" ->
      compile_collection_call scope env name arg_forms
  | "subvec" -> compile_subvec scope env arg_forms
  | "nth" -> compile_nth scope env arg_forms
  | "get" -> compile_get scope env arg_forms
  | "assoc" -> compile_assoc scope env arg_forms
  | "dissoc" -> compile_dissoc scope env arg_forms
  | "merge" -> compile_merge scope env arg_forms
  | "update" -> compile_update scope env arg_forms
  | "select-keys" -> compile_select_keys scope env arg_forms
  | "contains?" -> compile_contains scope env arg_forms
  | "keys" -> compile_keys scope env arg_forms
  | "vals" -> compile_vals scope env arg_forms
  | "hash-map" | "array-map" | "sorted-map" -> compile_hash_map scope env arg_forms
  | "rest" | "seq" | "empty?" -> compile_collection_call scope env name arg_forms
  | "into" -> compile_sequence_transform_call scope env name arg_forms
  | "take" | "drop" -> compile_collection_call scope env name arg_forms
  | "butlast" | "take-last" | "drop-last" | "take-nth" ->
      compile_sequence_transform_call scope env name arg_forms
  | "next" | "nthnext" | "nthrest" | "ffirst" | "fnext" | "nfirst" | "nnext"
  | "rseq" -> (
      match compile_args () with
      | Error _ as err -> err
      | Ok args -> Core_sequence.compile name args)
  | "some" -> compile_some scope env arg_forms
  | "split-at" -> compile_sequence_transform_call scope env name arg_forms
  | "split-with" -> compile_split_with scope env arg_forms
  | "partition-by" -> compile_partition_by scope env arg_forms
  | "bounded-count" | "dorun" | "doall" ->
      compile_sequence_transform_call scope env name arg_forms
  | "run!" -> compile_run_bang scope env arg_forms
  | "reverse" -> compile_collection_call scope env name arg_forms
  | "every?" | "not-any?" | "not-every?" ->
      compile_sequence_bool_predicate scope env name arg_forms
  | "map" -> compile_map_call scope env arg_forms
  | "filter" -> compile_filter scope env arg_forms
  | "remove" | "take-while" | "drop-while" | "distinct" | "dedupe" | "sort" ->
      compile_sequence_transform_call scope env name arg_forms
  | "sort-by" -> compile_sort_by scope env arg_forms
  | "concat" -> compile_sequence_transform_call scope env name arg_forms
  | "mapcat" -> compile_mapcat scope env arg_forms
  | "vec" | "set" | "repeat" ->
      compile_sequence_transform_call scope env name arg_forms
  | "repeatedly" -> compile_repeatedly scope env arg_forms
  | "interpose" | "interleave" | "partition" | "partition-all" ->
      compile_sequence_transform_call scope env name arg_forms
  | "reductions" -> compile_reductions scope env arg_forms
  | "map-indexed" -> compile_map_indexed scope env arg_forms
  | "filterv" -> compile_filterv scope env arg_forms
  | "mapv" -> compile_mapv scope env arg_forms
  | "reduce-kv" -> compile_reduce_kv scope env arg_forms
  | "reduce" -> compile_reduce scope env arg_forms
  | "apply" -> compile_apply scope env arg_forms
  | "comp" -> compile_comp scope env arg_forms
  | "partial" -> compile_partial scope env arg_forms
  | "identity" -> compile_identity scope env arg_forms
  | "constantly" -> compile_constantly scope env arg_forms
  | "complement" -> compile_complement scope env arg_forms
  | "every-pred" -> compile_predicate_combinator scope env "every-pred" arg_forms
  | "some-fn" -> compile_predicate_combinator scope env "some-fn" arg_forms
  | "juxt" -> compile_juxt scope env arg_forms
  | "distinct?" -> compile_distinct_question scope env arg_forms
  | "compare" -> compile_compare scope env arg_forms
  | "max-key" | "min-key" -> compile_key_extreme scope env name arg_forms
  | "hash-set" | "sorted-set" -> compile_hash_set scope env arg_forms
  | "set-of" -> compile_set_of arg_forms
  | "disj" -> compile_disj scope env arg_forms
  | "empty" -> compile_collection_call scope env name arg_forms
  | _ when is_constructor_name name -> (
      match lookup_binding scope env name with
      | Ok { ty = TFn (payload_tys, return_ty); _ } ->
          constructor (fun _ -> return_ty) (List.length payload_tys)
      | _ ->
          let constructor_name =
            resolve_ocaml_constructor_target scope env name
          in
          (match Ocaml_signature.constructor_signature constructor_name with
          | Error _ as err -> err
          | Ok signature ->
              constructor ~constructor_name
                (fun _ -> signature.result_type)
                (List.length signature.payload_types)))
  | _ -> compile_named_function_call scope env name arg_forms

and compile_inferred_ocaml_call scope env function_name value_forms =
  match compile_ocaml_arguments scope env value_forms with
  | Error _ as err -> err
  | Ok arguments ->
          let function_name =
            resolve_ocaml_call_target scope env function_name
          in
          match Ocaml_signature.value_signature function_name with
          | Error _ as err -> err
          | Ok signature -> (
              let labels = List.map fst arguments in
              match Ocaml_signature.result_after_application signature labels with
              | Error _ as err -> err
              | Ok return_ty ->
                  Ok (typed_ir return_ty (ocaml_apply function_name arguments)))

and compile_int_unary_call scope env name build_code arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok args -> Core_int.compile_unary name args build_code

and compile_boolean_call scope env name arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok args -> Core_boolean.compile name args

and compile_collection_call scope env name arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok args -> Core_collection.compile name args

and compile_sequence_transform_call scope env name arg_forms =
  match (name, arg_forms) with
  | ("partition" | "partition-all"), FInt size :: _ when size <= 0 ->
      Error.error (name ^ " size must be positive")
  | "take-nth", FInt count :: _ when count <= 0 ->
      Error.error "take-nth n must be positive"
  | ("remove" | "take-while" | "drop-while"), [ fn_form; collection_form ] -> (
      match
        ( compile_function_arg scope env fn_form,
          compile_expr scope env collection_form )
      with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> Core_sequence_transform.compile name [ fn; collection ])
  | _ -> (
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok args -> Core_sequence_transform.compile name args)

and compile_subs scope env arg_forms =
  match compile_args_for scope env arg_forms with
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

and compile_list scope env forms =
  match forms with
  | [] -> Error.error "empty list requires a type annotation"
  | first :: rest -> (
      match compile_expr scope env first with
      | Error _ as err -> err
      | Ok first_expr ->
          let rec loop acc = function
            | [] ->
                let values =
                  List.rev acc |> List.map (fun expr -> expr.ocaml_expr)
                in
                Ok (typed_ir (TList first_expr.ty) (Ocaml_ir.List values))
            | form :: rest -> (
                match compile_expr scope env form with
                | Error _ as err -> err
                | Ok expr ->
                    if Types.equal first_expr.ty expr.ty then loop (expr :: acc) rest
                    else Error.error "list elements must all have the same type")
          in
          loop [ first_expr ] rest)

and compile_list_star scope env arg_forms =
  match List.rev arg_forms with
  | [] -> Error.error "list* expects values and final collection"
  | final_form :: prefix_forms_rev -> (
      match compile_expr scope env final_form with
      | Error _ as err -> err
      | Ok final -> (
          match Core_sequence_transform.collection_to_list_expr final with
          | Error _ -> Error.error "list* final argument must be a collection"
          | Ok (inner, final_list_expr) -> (
              let prefix_forms = List.rev prefix_forms_rev in
              match compile_args_for scope env prefix_forms with
              | Error _ as err -> err
              | Ok prefix_args ->
                  if List.for_all (fun arg -> Types.equal inner arg.ty) prefix_args then
                    let list_expr =
                      match prefix_args with
                      | [] -> final_list_expr
                      | _ ->
                          Ocaml_ir.Infix
                            ( "@",
                              Ocaml_ir.List
                                (List.map (fun arg -> arg.ocaml_expr) prefix_args),
                              final_list_expr )
                    in
                    Ok (typed_ir (TList inner) list_expr)
                  else Error.error "list* value type must match final collection element type")))

and compile_range scope env arg_forms =
  let literal_zero = function FInt 0 -> true | _ -> false in
  let range_expr start stop step =
    let current = Ocaml_ir.Ident "current" in
    let stop_ident = Ocaml_ir.Ident "stop" in
    let step_ident = Ocaml_ir.Ident "step" in
    let done_expr =
      Ocaml_ir.If
        ( Ocaml_ir.Infix (">", step_ident, Ocaml_ir.Int 0),
          Ocaml_ir.Infix (">=", current, stop_ident),
          Ocaml_ir.Infix ("<=", current, stop_ident) )
    in
    let body =
      Ocaml_ir.If
        ( Ocaml_ir.Infix ("=", step_ident, Ocaml_ir.Int 0),
          apply "invalid_arg" [ Ocaml_ir.String "range step cannot be 0" ],
          Ocaml_ir.If
            ( done_expr,
              apply "List.rev" [ Ocaml_ir.Ident "acc" ],
              apply "range"
                [ Ocaml_ir.Cons (current, Ocaml_ir.Ident "acc");
                  Ocaml_ir.Infix ("+", current, step_ident);
                  stop_ident;
                  step_ident ] ) )
    in
    Ocaml_ir.LetRec
      ( "range",
        [ Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "current"; Ocaml_ir.PVar "stop"; Ocaml_ir.PVar "step" ],
        body,
        [ Ocaml_ir.List []; start; stop; step ] )
  in
  match arg_forms with
  | [ end_form ] -> (
      match compile_expr scope env end_form with
      | Error _ as err -> err
      | Ok end_expr ->
          if Types.equal end_expr.ty TInt then
            Ok (typed_ir (TList TInt) (range_expr (Ocaml_ir.Int 0) end_expr.ocaml_expr (Ocaml_ir.Int 1)))
          else Error.error "range arguments must be int")
  | [ start_form; end_form ] -> (
      match (compile_expr scope env start_form, compile_expr scope env end_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok start_expr, Ok end_expr ->
          if Types.equal start_expr.ty TInt && Types.equal end_expr.ty TInt then
            Ok (typed_ir (TList TInt) (range_expr start_expr.ocaml_expr end_expr.ocaml_expr (Ocaml_ir.Int 1)))
          else Error.error "range arguments must be int")
  | [ start_form; end_form; step_form ] ->
      if literal_zero step_form then Error.error "range step cannot be 0"
      else (
        match
          ( compile_expr scope env start_form,
            compile_expr scope env end_form,
            compile_expr scope env step_form )
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
                (typed_ir (TList TInt)
                   (range_expr start_expr.ocaml_expr end_expr.ocaml_expr step_expr.ocaml_expr))
            else Error.error "range arguments must be int")
  | _ -> Error.error "range expects end, start/end, or start/end/step"

and compile_list_of arg_forms =
  match arg_forms with
  | [ FKeyword keyword ] -> (
      match Type_annotation.of_keyword keyword with
      | Error _ as err -> err
      | Ok element_ty -> Ok (typed_ir (TList element_ty) (Ocaml_ir.List [])))
  | _ -> Error.error "list-of expects one type keyword"

and compile_vector_of arg_forms =
  match arg_forms with
  | [ FKeyword keyword ] -> (
      match Type_annotation.of_keyword keyword with
      | Error _ as err -> err
      | Ok element_ty ->
          Ok (typed_ir (TVector element_ty) (Ocaml_ir.Ident "Rrbvec.empty")))
  | _ -> Error.error "vector-of expects one type keyword"

and compile_conj scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok (collection :: values) when values <> [] ->
      let add_value collection value =
        match collection.ty with
        | TList inner when Types.equal inner value.ty ->
            Ok
              (typed_ir collection.ty
                 (Ocaml_ir.Cons (value.ocaml_expr, collection.ocaml_expr)))
        | TList _ -> Error.error "conj value type must match list element type"
        | TVector inner when Types.equal inner value.ty ->
            Ok
              (typed_ir collection.ty
                 (Ocaml_ir.Apply
                    ( Ocaml_ir.Ident "Rrbvec.push_back",
                      [ collection.ocaml_expr; value.ocaml_expr ] )))
        | TVector _ -> Error.error "conj value type must match vector element type"
        | TSet inner when Types.same_shape inner value.ty ->
            Result.bind (Types.set_module_name inner) (fun set_module ->
                   coerce_set_element inner value
                   |> Result.map (fun value ->
                          typed_ir collection.ty
                            (Ocaml_ir.Apply
                               ( Ocaml_ir.Ident (set_module ^ ".add"),
                                 [ value; collection.ocaml_expr ] ))))
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

and compile_cons scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ value; collection ] -> (
      match collection.ty with
      | TList inner when Types.equal inner value.ty ->
          Ok
            (typed_ir collection.ty
               (Ocaml_ir.Cons (value.ocaml_expr, collection.ocaml_expr)))
      | TList _ -> Error.error "cons value type must match list element type"
      | _ -> Error.error "cons expects a value and list")
  | Ok _ -> Error.error "cons expects value and list"

and compile_subvec scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ vector; start ] -> (
      match (vector.ty, start.ty) with
      | TVector _, TInt ->
          Ok
            (typed_ir vector.ty
               (Ocaml_ir.Apply
                  ( Ocaml_ir.Ident "Option.get",
                    [ Ocaml_ir.Apply
                        ( Ocaml_ir.Ident "Rrbvec.subvec",
                          [ vector.ocaml_expr;
                            start.ocaml_expr;
                            Ocaml_ir.Apply
                              (Ocaml_ir.Ident "Rrbvec.length", [ vector.ocaml_expr ]) ] ) ] )))
      | TVector _, _ -> Error.error "subvec indexes must be int"
      | _ -> Error.error "subvec expects a vector")
  | Ok [ vector; start; stop ] -> (
      match (vector.ty, start.ty, stop.ty) with
      | TVector _, TInt, TInt ->
          Ok
            (typed_ir vector.ty
               (Ocaml_ir.Apply
                  ( Ocaml_ir.Ident "Option.get",
                    [ Ocaml_ir.Apply
                        ( Ocaml_ir.Ident "Rrbvec.subvec",
                          [ vector.ocaml_expr; start.ocaml_expr; stop.ocaml_expr ] ) ] )))
      | TVector _, _, _ -> Error.error "subvec indexes must be int"
      | _ -> Error.error "subvec expects a vector")
  | Ok _ -> Error.error "subvec expects vector, start, and optional stop"

and compile_nth scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ collection; index ] -> (
      match (collection.ty, index.ty) with
      | TList inner, TInt ->
          Ok
            (typed_ir inner
               (Ocaml_ir.Apply
                  (Ocaml_ir.Ident "List.nth", [ collection.ocaml_expr; index.ocaml_expr ])))
      | TList _, _ -> Error.error "nth index must be int"
      | TVector inner, TInt ->
          Ok
            (typed_ir inner
               (Ocaml_ir.Apply
                  (Ocaml_ir.Ident "Rrbvec.nth", [ collection.ocaml_expr; index.ocaml_expr ])))
      | TVector _, _ -> Error.error "nth index must be int"
      | _ -> Error.error "nth expects a list or vector")
  | Ok [ collection; index; default ] -> (
      match (collection.ty, index.ty) with
      | TList inner, TInt when Types.equal inner default.ty ->
          Ok
            (typed_ir inner
               (Ocaml_ir.If
                  ( Ocaml_ir.Infix ("<", index.ocaml_expr, Ocaml_ir.Int 0),
                    default.ocaml_expr,
                    Ocaml_ir.Match
                      ( apply "List.nth_opt" [ collection.ocaml_expr; index.ocaml_expr ],
                        [ ( Ocaml_ir.PConstructor ("Some", Some (Ocaml_ir.PVar "value")),
                            Ocaml_ir.Ident "value" );
                          (Ocaml_ir.PConstructor ("None", None), default.ocaml_expr) ] ) )))
      | TList _, TInt -> Error.error "nth default must match collection element type"
      | TList _, _ -> Error.error "nth index must be int"
      | TVector inner, TInt when Types.equal inner default.ty ->
          Ok
            (typed_ir inner
               (Ocaml_ir.Match
                  ( apply "Rrbvec.nth_opt" [ collection.ocaml_expr; index.ocaml_expr ],
                    [ ( Ocaml_ir.PConstructor ("Some", Some (Ocaml_ir.PVar "value")),
                        Ocaml_ir.Ident "value" );
                      (Ocaml_ir.PConstructor ("None", None), default.ocaml_expr) ] )))
      | TVector _, TInt -> Error.error "nth default must match collection element type"
      | TVector _, _ -> Error.error "nth index must be int"
      | _ -> Error.error "nth expects a list or vector")
  | Ok _ -> Error.error "nth expects 2 or 3 arguments"

and compile_get scope env arg_forms =
  match arg_forms with
  | [ target_form; FKeyword keyword ] -> (
      match compile_expr scope env target_form with
      | Error _ as err -> err
      | Ok target -> (
          match target.ty with
          | TRecord fields | TNamed_record { fields; _ } -> (
              match find_field keyword fields with
              | Some field ->
                  Ok
                    (typed_ir field.ty
                       (Structural_map.field_expr target field))
              | None -> Error.error ("unknown field " ^ keyword))
          | _ -> Error.error "get expects a map"))
  | [ target_form; index_form ] -> (
      match (compile_expr scope env target_form, compile_expr scope env index_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok target, Ok index -> (
          match (target.ty, index.ty) with
          | TVector inner, TInt ->
              Ok
                (typed_ir inner
                   (apply "Rrbvec.nth" [ target.ocaml_expr; index.ocaml_expr ]))
          | TVector _, _ -> Error.error "get vector index must be int"
          | _ -> Error.error "get key must be a keyword"))
  | [ target_form; FKeyword keyword; default_form ] -> (
      match
        (compile_expr scope env target_form, compile_expr scope env default_form)
      with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok target, Ok default -> (
          match target.ty with
          | TRecord fields | TNamed_record { fields; _ } -> (
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
        ( compile_expr scope env target_form,
          compile_expr scope env index_form,
          compile_expr scope env default_form )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok target, Ok index, Ok default -> (
          match (target.ty, index.ty) with
          | TVector inner, TInt when Types.equal inner default.ty ->
              Ok
                (typed_ir inner
                   (Ocaml_ir.Match
                      ( apply "Rrbvec.nth_opt" [ target.ocaml_expr; index.ocaml_expr ],
                        [ ( Ocaml_ir.PConstructor ("Some", Some (Ocaml_ir.PVar "value")),
                            Ocaml_ir.Ident "value" );
                          (Ocaml_ir.PConstructor ("None", None), default.ocaml_expr) ] )))
          | TVector _, TInt -> Error.error "get default for vector must match element type"
          | TVector _, _ -> Error.error "get vector index must be int"
          | _ -> Error.error "get key must be a keyword"))
  | _ -> Error.error "get expects 2 or 3 arguments"

and compile_assoc scope env arg_forms =
  match arg_forms with
  | target_form :: pair_forms ->
      let rec compile_record_pairs acc = function
        | [] -> Ok (List.rev acc)
        | FKeyword keyword :: value_form :: rest -> (
            match compile_expr scope env value_form with
            | Error _ as err -> err
            | Ok value -> compile_record_pairs ((keyword, value) :: acc) rest)
        | _ -> Error.error "assoc expects map followed by keyword/value pairs"
      in
      let rec compile_vector_pairs acc = function
        | [] -> Ok (List.rev acc)
        | index_form :: value_form :: rest -> (
            match
              ( compile_expr scope env index_form,
                compile_expr scope env value_form )
            with
            | (Error _ as err), _ -> err
            | _, (Error _ as err) -> err
            | Ok index, Ok value -> compile_vector_pairs ((index, value) :: acc) rest)
        | _ -> Error.error "assoc expects collection followed by key/value pairs"
      in
      (match compile_expr scope env target_form with
      | Error _ as err -> err
      | Ok target -> (
          if pair_forms = [] || List.length pair_forms mod 2 <> 0 then
            match target.ty with
            | TRecord _ | TNamed_record _ ->
                Error.error "assoc expects map followed by keyword/value pairs"
            | TVector _ -> Error.error "assoc expects vector followed by index/value pairs"
            | _ -> Error.error "assoc expects collection followed by key/value pairs"
          else
            match target.ty with
            | TRecord _ | TNamed_record _ -> (
                match compile_record_pairs [] pair_forms with
                | Error _ as err -> err
                | Ok pairs -> Structural_map.assoc_many target pairs)
            | TVector inner -> (
                match compile_vector_pairs [] pair_forms with
                | Error _ as err -> err
                | Ok pairs ->
                    let rec apply_pairs expr = function
                      | [] -> Ok expr
                      | (index, value) :: rest ->
                          if not (Types.equal index.ty TInt) then
                            Error.error "assoc vector index must be int"
                          else if not (Types.equal value.ty inner) then
                            Error.error "assoc vector value must match element type"
                          else
                            apply_pairs
                              (apply "Rrbvec.set"
                                 [ expr; index.ocaml_expr; value.ocaml_expr ])
                              rest
                    in
                    (match apply_pairs target.ocaml_expr pairs with
                    | Error _ as err -> err
                    | Ok expr -> Ok (typed_ir target.ty expr)))
            | _ -> Error.error "assoc expects a map or vector"))
  | _ -> Error.error "assoc expects collection followed by key/value pairs"

and compile_dissoc scope env arg_forms =
  match arg_forms with
  | target_form :: key_forms -> (
      let rec parse_keys acc = function
        | [] -> Ok (List.rev acc)
        | FKeyword keyword :: rest -> parse_keys (keyword :: acc) rest
        | _ -> Error.error "dissoc expects map followed by keywords"
      in
      match compile_expr scope env target_form with
      | Error _ as err -> err
      | Ok target -> (
          match parse_keys [] key_forms with
          | Error _ as err -> err
          | Ok keywords -> Structural_map.dissoc_many target keywords))
  | _ -> Error.error "dissoc expects map followed by keywords"

and compile_merge scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok maps -> Structural_map.merge maps

and compile_hash_map scope env arg_forms =
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
    | Ok pairs -> compile_map scope env pairs

and compile_update scope env arg_forms =
  match arg_forms with
  | target_form :: FKeyword keyword :: fn_form :: extra_forms -> (
      match
        ( compile_expr scope env target_form,
          compile_function_arg scope env fn_form,
          compile_args_for scope env extra_forms )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok target, Ok fn, Ok extra_args -> (
          match target.ty with
          | TRecord fields | TNamed_record { fields; _ } -> (
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
        ( compile_expr scope env target_form,
          compile_expr scope env index_form,
          compile_function_arg scope env fn_form,
          compile_args_for scope env extra_forms )
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
                  let old_expr =
                    apply "Rrbvec.nth" [ target.ocaml_expr; index.ocaml_expr ]
                  in
                  let value_expr =
                    Ocaml_ir.Apply
                      (fn.ocaml_expr, old_expr :: List.map (fun arg -> arg.ocaml_expr) extra_args)
                  in
                  Ok
                    (typed_ir target.ty
                       (apply "Rrbvec.set"
                          [ target.ocaml_expr; index.ocaml_expr; value_expr ]))
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

and compile_select_keys scope env arg_forms =
  match arg_forms with
  | [ target_form; FVector key_forms ] -> (
      let rec parse_keywords acc = function
        | [] -> Ok (List.rev acc)
        | FKeyword keyword :: rest -> parse_keywords (keyword :: acc) rest
        | _ -> Error.error "select-keys expects a vector of keywords"
      in
      match (compile_expr scope env target_form, parse_keywords [] key_forms) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok target, Ok keywords -> (
          match target.ty with
          | TRecord fields | TNamed_record { fields; _ } ->
              Structural_map.select_keys target fields keywords
          | _ -> Error.error "select-keys expects a map"))
  | [ _; _ ] -> Error.error "select-keys expects a vector of keywords"
  | _ -> Error.error "select-keys expects map and key vector"

and compile_contains scope env arg_forms =
  let compile_collection_contains target value =
    match (target.ty, value.ty) with
    | TSet inner, _ when Types.same_shape inner value.ty ->
        Result.bind (Types.set_module_name inner) (fun set_module ->
               coerce_set_element inner value
               |> Result.map (fun value ->
                      typed_ir TBool
                        (Ocaml_ir.Apply
                           (Ocaml_ir.Ident (set_module ^ ".mem"),
                            [ value; target.ocaml_expr ]))))
    | TSet _, _ -> Error.error "contains? value type must match set element type"
    | TVector _, TInt ->
        Ok
          (typed_ir TBool
             (Ocaml_ir.Infix
                ( "&&",
                  Ocaml_ir.Infix (">=", value.ocaml_expr, Ocaml_ir.Int 0),
                  Ocaml_ir.Infix
                    ( "<",
                      value.ocaml_expr,
                      Ocaml_ir.Apply
                        (Ocaml_ir.Ident "Rrbvec.length", [ target.ocaml_expr ]) ) )))
    | TVector _, _ -> Error.error "contains? vector index must be int"
    | _ -> Error.error "contains? expects a map, set, or vector"
  in
  match arg_forms with
  | target_form :: FKeyword keyword :: [] -> (
      match compile_expr scope env target_form with
      | Error _ as err -> err
      | Ok target -> (
          match target.ty with
          | TRecord fields | TNamed_record { fields; _ } ->
              Ok
                (typed_ir TBool
                   (Ocaml_ir.Bool (Option.is_some (find_field keyword fields))))
          | _ ->
              compile_collection_contains target
                (typed_ir TKeyword (Ocaml_ir.String keyword))))
  | target_form :: value_form :: [] -> (
      match (compile_expr scope env target_form, compile_expr scope env value_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok target, Ok value -> compile_collection_contains target value)
  | _ -> Error.error "contains? expects collection and key"

and compile_keys scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ target ] -> (
      match target.ty with
      | TRecord fields | TNamed_record { fields; _ } ->
          Ok
            (typed_ir (TVector TKeyword)
               (Ocaml_ir.Apply
                  ( Ocaml_ir.Ident "Rrbvec.of_list",
                    [ Ocaml_ir.List
                        (fields
                        |> List.map (fun (field : field) ->
                               Ocaml_ir.String field.keyword)) ] )))
      | _ -> Error.error "keys expects a map")
  | Ok _ -> Error.error "keys expects 1 arguments"

and compile_vals scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ target ] -> (
      match target.ty with
      | TRecord [] | TNamed_record { fields = []; _ } ->
          Error.error "vals requires a non-empty map"
      | TRecord (first :: rest) | TNamed_record { fields = first :: rest; _ } ->
          if List.for_all (fun (field : field) -> Types.equal first.ty field.ty) rest then
            Ok
              (typed_ir (TVector first.ty)
                 (Ocaml_ir.Apply
                    ( Ocaml_ir.Ident "Rrbvec.of_list",
                      [ Ocaml_ir.List
                          ((first :: rest)
                          |> List.map (fun (field : field) ->
                                 Structural_map.field_expr target field)) ] )))
          else Error.error "vals requires all map values to have the same type"
      | _ -> Error.error "vals expects a map")
  | Ok _ -> Error.error "vals expects 1 arguments"

and compile_function_arg scope env = function
  | FSymbol name -> lookup_function scope env name
  | form -> compile_expr scope env form

and compile_function_arg_for_collection scope env element_ty = function
  | FList (FSymbol "fn" :: FVector [ FSymbol name ] :: body_forms) ->
      let binding = Types.binding (Names.sanitize_name name) element_ty in
      let function_env =
        Env.add (Names.scoped_key scope name) binding env
      in
      compile_body scope function_env "function body requires at least one form"
        body_forms
      |> Result.map (fun body ->
             let pattern =
               match element_ty with
               | TNamed_record record ->
                   Ocaml_ir.PConstraint
                     (Ocaml_ir.PVar binding.ocaml_name, record.type_name)
               | _ -> Ocaml_ir.PVar binding.ocaml_name
             in
             typed_ir (TFn ([ element_ty ], body.ty))
               (Ocaml_ir.Fun ([ pattern ], body.ocaml_expr)))
  | form -> compile_function_arg scope env form

and compile_named_function_call scope env name arg_forms =
  match ocaml_call_target scope env name with
  | Some _ -> compile_inferred_ocaml_call scope env name arg_forms
  | None -> (
      match lookup_binding scope env name with
  | Error _ -> compile_protocol_call scope env name arg_forms
  | Ok fn -> (
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok args -> (
          match fn.ty with
          | TFn (param_tys, ret)
            when List.length param_tys = List.length args
                 && List.for_all2
                      (fun expected arg -> Types.compatible ~expected ~actual:arg.ty)
                      param_tys args ->
              let arg_exprs =
                args
                |> List.mapi (fun index arg ->
                       let row_type_name = List.nth_opt fn.row_param_types index |> Option.join in
                      let expected_ty = List.nth param_tys index in
                      row_arg_expr row_type_name expected_ty arg)
              in
              let ret =
                match (fn.return_param_index, ret) with
                | Some index, TAny -> (
                    match List.nth_opt args index with
                    | Some arg -> arg.ty
                    | None -> ret)
                | _ -> ret
              in
              Ok (typed_ir ret (Ocaml_ir.Apply (Ocaml_ir.Ident fn.ocaml_name, arg_exprs)))
          | TFn _ -> Error.error (name ^ " called with incompatible arguments")
          | _ -> Error.error (name ^ " is not callable"))))

and compile_protocol_call scope env name arg_forms =
  if Protocol.method_is_ambiguous scope env name then
    Error.error
      ("ambiguous protocol method " ^ name ^ "; use Protocol/method")
  else match Protocol.lookup_marker scope env name with
  | None -> Error.error ("unknown function " ^ name)
  | Some marker -> (
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok args -> (
          match marker.ty with
          | TFn (param_tys, _ret) when List.length param_tys <> List.length args ->
              Error.error (name ^ " called with incompatible arguments")
          | TFn (_, _) -> (
              match args with
              | [] -> Error.error (name ^ " called with incompatible arguments")
              | receiver :: _ -> (
                  let method_name = Protocol.method_basename name in
                  match
                    Protocol.lookup_marker_impl env marker method_name receiver.ty
                  with
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
                                    Types.assignable ~expected ~actual:arg.ty)
                                  param_tys args ->
                          Ok
                            (typed_ir ret
                               (Ocaml_ir.Apply
                                  ( Ocaml_ir.Ident impl.ocaml_name,
                                    List.map (fun arg -> arg.ocaml_expr) args )))
                      | TFn _ -> Error.error (name ^ " called with incompatible arguments")
                      | _ -> Error.error (name ^ " is not callable"))))
          | _ -> Error.error (name ^ " is not callable")))

and collection_to_list_expr collection =
  Core_sequence_transform.collection_to_list_expr collection

and collection_from_list_expr collection_ty list_expr =
  Core_sequence_transform.collection_from_list_expr collection_ty list_expr

and comparable_type = function
  | TInt | TString | TSymbol | TKeyword | TBool | TAny -> true
  | _ -> false

and compile_sort_by scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], key_ty), Ok (inner, list_expr)
            when Types.equal param_ty inner && comparable_type key_ty ->
              Ok
                (typed_ir (TList inner)
                   (apply "List.sort"
                      [ Ocaml_ir.Fun
                          ( [ Ocaml_ir.PVar "left"; Ocaml_ir.PVar "right" ],
                            apply "Stdlib.compare"
                              [ Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "left" ]);
                                Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "right" ]) ] );
                        list_expr ]))
          | TFn ([ param_ty ], _), Ok (inner, _) when not (Types.equal param_ty inner) ->
              Error.error "sort-by key function must match collection elements"
          | TFn _, Ok _ -> Error.error "sort-by key function must return a comparable value"
          | _, Ok _ -> Error.error "sort-by expects a function"
          | _, Error _ -> Error.error "sort-by expects a collection"))
  | _ -> Error.error "sort-by expects function and collection"

and compile_mapcat scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], TList ret_inner), Ok (inner, list_expr)
            when Types.equal param_ty inner ->
              Ok
                (typed_ir (TList ret_inner)
                   (apply "List.concat"
                      [ apply "List.map" [ fn.ocaml_expr; list_expr ] ]))
          | TFn ([ param_ty ], TVector ret_inner), Ok (inner, list_expr)
            when Types.equal param_ty inner ->
              Ok
                (typed_ir (TList ret_inner)
                   (apply "List.concat"
                      [ apply "List.map"
                          [ Ocaml_ir.Fun
                              ( [ Ocaml_ir.PVar "item" ],
                                apply "Rrbvec.to_list"
                                  [ Ocaml_ir.Apply
                                      (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]) ] );
                            list_expr ] ]))
          | TFn ([ param_ty ], TSet ret_inner), Ok (inner, list_expr)
            when Types.equal param_ty inner -> (
              match Types.set_module_name ret_inner with
              | Error _ as err -> err
              | Ok set_module ->
                  Ok
                    (typed_ir (TList ret_inner)
                       (apply "List.concat"
                          [ apply "List.map"
                              [ Ocaml_ir.Fun
                                  ( [ Ocaml_ir.PVar "item" ],
                                    apply (set_module ^ ".elements")
                                      [ Ocaml_ir.Apply
                                          (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]) ] );
                                list_expr ] ])))
          | TFn ([ param_ty ], _), Ok (inner, _) when not (Types.equal param_ty inner) ->
              Error.error "mapcat function argument type does not match collection"
          | TFn _, Ok _ -> Error.error "mapcat function must return a collection"
          | _, Ok _ -> Error.error "mapcat expects a function"
          | _, Error _ -> Error.error "mapcat expects a collection"))
  | _ -> Error.error "mapcat expects function and collection"

and compile_repeatedly scope env arg_forms =
  match arg_forms with
  | count_form :: fn_form :: [] -> (
      match (compile_expr scope env count_form, compile_function_arg scope env fn_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok count, Ok fn -> (
          if not (Types.equal count.ty TInt) then Error.error "repeatedly count must be int"
          else
            match fn.ty with
            | TFn ([], ret) ->
                let body =
                  Ocaml_ir.If
                    ( Ocaml_ir.Infix ("<=", Ocaml_ir.Ident "n", Ocaml_ir.Int 0),
                      Ocaml_ir.Ident "acc",
                      apply "repeatedly"
                        [ Ocaml_ir.Cons
                            ( Ocaml_ir.Apply (fn.ocaml_expr, []),
                              Ocaml_ir.Ident "acc" );
                          Ocaml_ir.Infix ("-", Ocaml_ir.Ident "n", Ocaml_ir.Int 1) ] )
                in
                Ok
                  (typed_ir (TList ret)
                     (Ocaml_ir.LetRec
                        ( "repeatedly",
                          [ Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "n" ],
                          body,
                          [ Ocaml_ir.List []; count.ocaml_expr ] )))
            | TFn _ -> Error.error "repeatedly expects a zero-argument function"
            | _ -> Error.error "repeatedly expects a function"))
  | _ -> Error.error "repeatedly expects count and function"

and compile_reductions scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ acc_ty; item_ty ], ret), Ok (inner, list_expr)
            when Types.equal acc_ty inner && Types.equal item_ty inner && Types.equal ret inner ->
              let reductions_body =
                Ocaml_ir.Match
                  ( Ocaml_ir.Ident "xs",
                    [ (Ocaml_ir.PList [], apply "List.rev" [ Ocaml_ir.Ident "acc" ]);
                      ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "tail"),
                        Ocaml_ir.Let
                          ( [ ( Ocaml_ir.PVar "next",
                                Ocaml_ir.Apply
                                  ( fn.ocaml_expr,
                                    [ Ocaml_ir.Ident "current"; Ocaml_ir.Ident "item" ] ) ) ],
                            apply "reductions"
                              [ Ocaml_ir.Ident "next";
                                Ocaml_ir.Cons
                                  (Ocaml_ir.Ident "next", Ocaml_ir.Ident "acc");
                                Ocaml_ir.Ident "tail" ] ) ) ] )
              in
              Ok
                (typed_ir (TList inner)
                   (Ocaml_ir.Match
                      ( list_expr,
                        [ (Ocaml_ir.PList [], Ocaml_ir.List []);
                          ( Ocaml_ir.PCons (Ocaml_ir.PVar "first", Ocaml_ir.PVar "rest"),
                            Ocaml_ir.LetRec
                              ( "reductions",
                                [ Ocaml_ir.PVar "current";
                                  Ocaml_ir.PVar "acc";
                                  Ocaml_ir.PVar "xs" ],
                                reductions_body,
                                [ Ocaml_ir.Ident "first";
                                  Ocaml_ir.List [ Ocaml_ir.Ident "first" ];
                                  Ocaml_ir.Ident "rest" ] ) ) ] )))
          | TFn _, Ok _ -> Error.error "reductions function type does not match collection"
          | _, Ok _ -> Error.error "reductions expects a function"
          | _, Error _ -> Error.error "reductions expects a collection"))
  | fn_form :: init_form :: collection_form :: [] -> (
      match
        ( compile_function_arg scope env fn_form,
          compile_expr scope env init_form,
          compile_expr scope env collection_form )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok fn, Ok init, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ acc_ty; item_ty ], ret), Ok (inner, list_expr)
            when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
              let reductions_body =
                Ocaml_ir.Match
                  ( Ocaml_ir.Ident "xs",
                    [ (Ocaml_ir.PList [], apply "List.rev" [ Ocaml_ir.Ident "acc" ]);
                      ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "rest"),
                        Ocaml_ir.Let
                          ( [ ( Ocaml_ir.PVar "next",
                                Ocaml_ir.Apply
                                  ( fn.ocaml_expr,
                                    [ Ocaml_ir.Ident "current"; Ocaml_ir.Ident "item" ] ) ) ],
                            apply "reductions"
                              [ Ocaml_ir.Ident "next";
                                Ocaml_ir.Cons
                                  (Ocaml_ir.Ident "next", Ocaml_ir.Ident "acc");
                                Ocaml_ir.Ident "rest" ] ) ) ] )
              in
              Ok
                (typed_ir (TList init.ty)
                   (Ocaml_ir.LetRec
                      ( "reductions",
                        [ Ocaml_ir.PVar "current";
                          Ocaml_ir.PVar "acc";
                          Ocaml_ir.PVar "xs" ],
                        reductions_body,
                        [ init.ocaml_expr;
                          Ocaml_ir.List [ init.ocaml_expr ];
                          list_expr ] )))
          | TFn _, Ok _ -> Error.error "reductions function type does not match init and collection"
          | _, Ok _ -> Error.error "reductions expects a function"
          | _, Error _ -> Error.error "reductions expects a collection"))
  | _ -> Error.error "reductions expects function, optional init, and collection"

and compile_split_with scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], TBool), Ok (inner, list_expr) when Types.equal param_ty inner ->
              let split_body =
                Ocaml_ir.Match
                  ( Ocaml_ir.Ident "rest",
                    [ ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "tail"),
                        Ocaml_ir.If
                          ( Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]),
                            apply "split"
                              [ Ocaml_ir.Cons
                                  (Ocaml_ir.Ident "item", Ocaml_ir.Ident "prefix");
                                Ocaml_ir.Ident "tail" ],
                            Ocaml_ir.Tuple
                              [ apply "List.rev" [ Ocaml_ir.Ident "prefix" ];
                                Ocaml_ir.Ident "rest" ] ) );
                      ( Ocaml_ir.PAny,
                        Ocaml_ir.Tuple
                          [ apply "List.rev" [ Ocaml_ir.Ident "prefix" ];
                            Ocaml_ir.Ident "rest" ] ) ] )
              in
              let pair_expr =
                Ocaml_ir.LetRec
                  ( "split",
                    [ Ocaml_ir.PVar "prefix"; Ocaml_ir.PVar "rest" ],
                    split_body,
                    [ Ocaml_ir.List []; list_expr ] )
              in
              Ok
                (typed_ir (TVector collection.ty)
                   (Ocaml_ir.Let
                      ( [ (Ocaml_ir.PVar "pair", pair_expr) ],
                        apply "Rrbvec.of_list"
                          [ Ocaml_ir.List
                              [ collection_from_list_expr collection.ty
                                  (apply "fst" [ Ocaml_ir.Ident "pair" ]);
                                collection_from_list_expr collection.ty
                                  (apply "snd" [ Ocaml_ir.Ident "pair" ]) ] ] )))
          | TFn _, Ok _ -> Error.error "split-with expects a predicate matching collection elements"
          | _, Ok _ -> Error.error "split-with expects a function"
          | _, Error _ -> Error.error "split-with expects a collection"))
  | _ -> Error.error "split-with expects function and collection"

and compile_partition_by scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], key_ty), Ok (inner, list_expr) when Types.equal param_ty inner ->
              ignore key_ty;
              let finish_call =
                apply "finish" [ Ocaml_ir.Ident "groups"; Ocaml_ir.Ident "current" ]
              in
              let start_new_group =
                Ocaml_ir.Let
                  ( [ ( Ocaml_ir.PVar "groups",
                        Ocaml_ir.Match
                          ( Ocaml_ir.Ident "current",
                            [ (Ocaml_ir.PList [], Ocaml_ir.Ident "groups");
                              ( Ocaml_ir.PAny,
                                Ocaml_ir.Cons
                                  ( apply "List.rev" [ Ocaml_ir.Ident "current" ],
                                    Ocaml_ir.Ident "groups" ) ) ] ) ) ],
                    apply "partition"
                      [ Ocaml_ir.Ident "groups";
                        Ocaml_ir.List [ Ocaml_ir.Ident "item" ];
                        Ocaml_ir.Constructor ("Some", Some (Ocaml_ir.Ident "key"));
                        Ocaml_ir.Ident "rest" ] )
              in
              let partition_body =
                Ocaml_ir.Match
                  ( Ocaml_ir.Ident "xs",
                    [ (Ocaml_ir.PList [], finish_call);
                      ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "rest"),
                        Ocaml_ir.Let
                          ( [ ( Ocaml_ir.PVar "key",
                                Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]) ) ],
                            Ocaml_ir.Match
                              ( Ocaml_ir.Ident "current_key",
                                [ ( Ocaml_ir.PConstructor ("Some", Some (Ocaml_ir.PVar "previous")),
                                    Ocaml_ir.If
                                      ( Ocaml_ir.Infix
                                          ( "=", Ocaml_ir.Ident "previous",
                                            Ocaml_ir.Ident "key" ),
                                        apply "partition"
                                          [ Ocaml_ir.Ident "groups";
                                            Ocaml_ir.Cons
                                              ( Ocaml_ir.Ident "item",
                                                Ocaml_ir.Ident "current" );
                                            Ocaml_ir.Ident "current_key";
                                            Ocaml_ir.Ident "rest" ],
                                        start_new_group ) );
                                  (Ocaml_ir.PAny, start_new_group) ] ) ) ) ] )
              in
              let finish_body =
                Ocaml_ir.Match
                  ( Ocaml_ir.Ident "current",
                    [ (Ocaml_ir.PList [], apply "List.rev" [ Ocaml_ir.Ident "groups" ]);
                      ( Ocaml_ir.PAny,
                        apply "List.rev"
                          [ Ocaml_ir.Cons
                              ( apply "List.rev" [ Ocaml_ir.Ident "current" ],
                                Ocaml_ir.Ident "groups" ) ] ) ] )
              in
              Ok
                (typed_ir (TList (TList inner))
                   (Ocaml_ir.LetRecIn
                      ( "finish",
                        [ Ocaml_ir.PVar "groups"; Ocaml_ir.PVar "current" ],
                        finish_body,
                        Ocaml_ir.LetRec
                          ( "partition",
                            [ Ocaml_ir.PVar "groups";
                              Ocaml_ir.PVar "current";
                              Ocaml_ir.PVar "current_key";
                              Ocaml_ir.PVar "xs" ],
                            partition_body,
                            [ Ocaml_ir.List [];
                              Ocaml_ir.List [];
                              Ocaml_ir.Constructor ("None", None);
                              list_expr ] ) )))
          | TFn _, Ok _ -> Error.error "partition-by function type does not match collection"
          | _, Ok _ -> Error.error "partition-by expects a function"
          | _, Error _ -> Error.error "partition-by expects a collection"))
  | _ -> Error.error "partition-by expects function and collection"

and compile_run_bang scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], _ret), Ok (inner, list_expr) when Types.equal param_ty inner ->
              Ok
                (typed_ir TUnit
                   (Ocaml_ir.Let
                      ( [ ( Ocaml_ir.PUnit,
                            apply "List.iter"
                              [ Ocaml_ir.Fun
                                  ( [ Ocaml_ir.PVar "item" ],
                                    apply "ignore"
                                      [ Ocaml_ir.Apply
                                          (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]) ] );
                                list_expr ] ) ],
                        Ocaml_ir.Unit )))
          | TFn _, Ok _ -> Error.error "run! function type does not match collection"
          | _, Ok _ -> Error.error "run! expects a function"
          | _, Error _ -> Error.error "run! expects a collection"))
  | _ -> Error.error "run! expects function and collection"

and compile_map_indexed scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ TInt; item_ty ], ret), Ok (inner, list_expr) when Types.equal item_ty inner ->
              Ok
                (typed_ir (TList ret)
                   (apply "List.mapi"
                      [ Ocaml_ir.Fun
                          ( [ Ocaml_ir.PVar "index"; Ocaml_ir.PVar "item" ],
                            Ocaml_ir.Apply
                              ( fn.ocaml_expr,
                                [ Ocaml_ir.Ident "index"; Ocaml_ir.Ident "item" ] ) );
                        list_expr ]))
          | TFn _, Ok _ -> Error.error "map-indexed function type does not match collection"
          | _, Ok _ -> Error.error "map-indexed expects a function"
          | _, Error _ -> Error.error "map-indexed expects a collection"))
  | _ -> Error.error "map-indexed expects function and collection"

and compile_filterv scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], TBool), Ok (inner, list_expr) when Types.equal param_ty inner ->
              Ok
                (typed_ir (TVector inner)
                   (apply "Rrbvec.of_list"
                      [ apply "List.filter" [ fn.ocaml_expr; list_expr ] ]))
          | TFn _, Ok _ ->
              Error.error "filterv expects a predicate matching collection elements"
          | _, Ok _ -> Error.error "filterv expects a function"
          | _, Error _ -> Error.error "filterv expects a collection"))
  | _ -> Error.error "filterv expects function and collection"

and compile_mapv scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], ret), Ok (inner, list_expr) when Types.equal param_ty inner ->
              Ok
                (typed_ir (TVector ret)
                   (apply "Rrbvec.of_list"
                      [ apply "List.map" [ fn.ocaml_expr; list_expr ] ]))
          | TFn _, Ok _ -> Error.error "mapv function type does not match collection"
          | _, Ok _ -> Error.error "mapv expects a function"
          | _, Error _ -> Error.error "mapv expects a collection"))
  | _ -> Error.error "mapv expects function and collection"

and compile_reduce_kv scope env arg_forms =
  match arg_forms with
  | fn_form :: init_form :: collection_form :: [] -> (
      match
        ( compile_function_arg scope env fn_form,
          compile_expr scope env init_form,
          compile_expr scope env collection_form )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok fn, Ok init, Ok collection -> (
          match (fn.ty, collection.ty) with
          | TFn ([ acc_ty; TInt; item_ty ], ret), TVector inner
            when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
              Ok
                (typed_ir init.ty
                   (apply "List.fold_left"
                      [ Ocaml_ir.Fun
                          ( [ Ocaml_ir.PVar "acc";
                              Ocaml_ir.PTuple
                                [ Ocaml_ir.PVar "index"; Ocaml_ir.PVar "item" ] ],
                            Ocaml_ir.Apply
                              ( fn.ocaml_expr,
                                [ Ocaml_ir.Ident "acc";
                                  Ocaml_ir.Ident "index";
                                  Ocaml_ir.Ident "item" ] ) );
                        init.ocaml_expr;
                        apply "List.mapi"
                          [ Ocaml_ir.Fun
                              ( [ Ocaml_ir.PVar "index"; Ocaml_ir.PVar "item" ],
                                Ocaml_ir.Tuple
                                  [ Ocaml_ir.Ident "index"; Ocaml_ir.Ident "item" ] );
                            apply "Rrbvec.to_list" [ collection.ocaml_expr ] ] ]))
          | TFn _, TVector _ -> Error.error "reduce-kv function type does not match vector"
          | _, TVector _ -> Error.error "reduce-kv expects a function"
          | _ -> Error.error "reduce-kv expects a vector"))
  | _ -> Error.error "reduce-kv expects function, init, and vector"

and compile_some scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection_to_list_expr collection) with
          | TFn ([ param_ty ], TBool), Ok (inner, list_expr) when Types.equal param_ty inner ->
              Ok (typed_ir TBool (apply "List.exists" [ fn.ocaml_expr; list_expr ]))
          | TFn _, Ok _ -> Error.error "some expects a predicate matching collection elements"
          | _, Ok _ -> Error.error "some expects a function"
          | _, Error _ -> Error.error "some expects a collection"))
  | _ -> Error.error "some expects function and collection"

and compile_sequence_bool_predicate scope env name arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          let build all_expr =
            match name with
            | "every?" -> all_expr
            | "not-any?" -> all_expr
            | "not-every?" -> Ocaml_ir.Prefix ("not", all_expr)
            | _ -> all_expr
          in
          let predicate_expr =
            match name with
            | "not-any?" ->
                Ocaml_ir.Fun
                  ( [ Ocaml_ir.PVar "item" ],
                    Ocaml_ir.Prefix
                      ("not", Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ])) )
            | _ -> fn.ocaml_expr
          in
          match (fn.ty, collection.ty) with
          | TFn ([ param_ty ], TBool), TList inner when Types.equal param_ty inner ->
              let all_expr = apply "List.for_all" [ predicate_expr; collection.ocaml_expr ] in
              Ok (typed_ir TBool (build all_expr))
          | TFn _, TList _ -> Error.error (name ^ " expects a predicate matching list elements")
          | _, TList _ -> Error.error (name ^ " expects a function")
          | TFn ([ param_ty ], TBool), TVector inner when Types.equal param_ty inner ->
              let all_expr = apply "Rrbvec.for_all" [ predicate_expr; collection.ocaml_expr ] in
              Ok (typed_ir TBool (build all_expr))
          | TFn _, TVector _ ->
              Error.error (name ^ " expects a predicate matching vector elements")
          | _, TVector _ -> Error.error (name ^ " expects a function")
          | TFn ([ param_ty ], TBool), TSet inner
            when Types.compatible ~expected:param_ty ~actual:inner ->
              Types.set_module_name inner
              |> Result.map (fun set_module ->
                     let fn_expr = constrain_record_function_argument_expr fn inner in
                     let predicate_expr =
                       match name with
                       | "not-any?" ->
                           Ocaml_ir.Fun
                             ( [ Ocaml_ir.PVar "item" ],
                               Ocaml_ir.Prefix
                                 ( "not",
                                   Ocaml_ir.Apply (fn_expr, [ Ocaml_ir.Ident "item" ]) ) )
                       | _ -> fn_expr
                     in
                     let all_expr =
                       apply "List.for_all"
                         [ predicate_expr;
                           apply (set_module ^ ".elements") [ collection.ocaml_expr ] ]
                     in
                     typed_ir TBool (build all_expr))
          | TFn _, TSet _ -> Error.error (name ^ " expects a predicate matching set elements")
          | _, TSet _ -> Error.error (name ^ " expects a function")
          | _ -> Error.error (name ^ " expects a list, vector, or set")))
  | _ -> Error.error (name ^ " expects function and collection")

and compile_map_call scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match compile_expr scope env collection_form with
      | Error _ as err -> err
      | Ok collection ->
          let fn =
            match collection.ty with
            | TList inner | TVector inner | TSet inner ->
                compile_function_arg_for_collection scope env inner fn_form
            | _ -> compile_function_arg scope env fn_form
          in
          (match fn with
          | Error _ as err -> err
          | Ok fn -> (
          match (fn.ty, collection.ty) with
          | TFn ([ param_ty ], ret), TList inner when Types.equal param_ty inner ->
              Ok
                (typed_ir (TList ret)
                   (apply "List.map" [ fn.ocaml_expr; collection.ocaml_expr ]))
          | TFn _, TList _ -> Error.error "map function argument type does not match list"
          | _, TList _ -> Error.error "map expects a function"
          | TFn ([ param_ty ], ret), TVector inner when Types.equal param_ty inner ->
              Ok
                (typed_ir (TVector ret)
                   (apply "Rrbvec.map" [ fn.ocaml_expr; collection.ocaml_expr ]))
          | TFn _, TVector _ -> Error.error "map function argument type does not match vector"
          | _, TVector _ -> Error.error "map expects a function"
          | TFn ([ param_ty ], ret), TSet inner
            when Types.compatible ~expected:param_ty ~actual:inner ->
              Result.bind (Types.set_module_name ret) (fun result_module ->
                  Types.set_module_name inner
                  |> Result.map (fun source_module ->
                         let fn_expr = constrain_record_function_argument_expr fn inner in
                         typed_ir (TSet ret)
                           (apply (result_module ^ ".of_list")
                              [ apply "List.map"
                                  [ fn_expr;
                                    apply (source_module ^ ".elements")
                                      [ collection.ocaml_expr ] ] ])))
          | TFn _, TSet _ -> Error.error "map function argument type does not match set"
          | _, TSet _ -> Error.error "map expects a function"
          | _ -> Error.error "map expects a list, vector, or set")))
  | _ -> Error.error "map expects function and collection"

and compile_filter scope env arg_forms =
  match arg_forms with
  | fn_form :: collection_form :: [] -> (
      match (compile_function_arg scope env fn_form, compile_expr scope env collection_form) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok collection -> (
          match (fn.ty, collection.ty) with
          | TFn ([ param_ty ], TBool), TList inner when Types.equal param_ty inner ->
              Ok
                (typed_ir collection.ty
                   (apply "List.filter" [ fn.ocaml_expr; collection.ocaml_expr ]))
          | TFn _, TList _ -> Error.error "filter expects a predicate matching list elements"
          | _, TList _ -> Error.error "filter expects a function"
          | TFn ([ param_ty ], TBool), TVector inner when Types.equal param_ty inner ->
              Ok
                (typed_ir collection.ty
                   (apply "Rrbvec.filter" [ fn.ocaml_expr; collection.ocaml_expr ]))
          | TFn _, TVector _ -> Error.error "filter expects a predicate matching vector elements"
          | _, TVector _ -> Error.error "filter expects a function"
          | TFn ([ param_ty ], TBool), TSet inner
            when Types.compatible ~expected:param_ty ~actual:inner ->
              Types.set_module_name inner
              |> Result.map (fun set_module ->
                     let fn_expr = constrain_record_function_argument_expr fn inner in
                     typed_ir collection.ty
                       (apply (set_module ^ ".of_list")
                          [ apply "List.filter"
                              [ fn_expr;
                                apply (set_module ^ ".elements")
                                  [ collection.ocaml_expr ] ] ]))
          | TFn _, TSet _ -> Error.error "filter expects a predicate matching set elements"
          | _, TSet _ -> Error.error "filter expects a function"
          | _ -> Error.error "filter expects a list, vector, or set"))
  | _ -> Error.error "filter expects function and collection"

and compile_reduce scope env arg_forms =
  match arg_forms with
  | fn_form :: init_form :: collection_form :: [] -> (
      match
        ( compile_function_arg scope env fn_form,
          compile_expr scope env init_form,
          compile_expr scope env collection_form )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok fn, Ok init, Ok collection -> (
          match (fn.ty, collection.ty) with
          | TFn ([ acc_ty; item_ty ], ret), TList inner
            when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
              Ok
                (typed_ir init.ty
                   (apply "List.fold_left"
                      [ fn.ocaml_expr; init.ocaml_expr; collection.ocaml_expr ]))
          | TFn _, TList _ -> Error.error "reduce function type does not match init and list"
          | _, TList _ -> Error.error "reduce expects a function"
          | TFn ([ acc_ty; item_ty ], ret), TVector inner
            when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
             Ok
                (typed_ir init.ty
                   (apply "Rrbvec.fold_left"
                      [ fn.ocaml_expr; init.ocaml_expr; collection.ocaml_expr ]))
          | TFn _, TVector _ -> Error.error "reduce function type does not match init and vector"
          | _, TVector _ -> Error.error "reduce expects a function"
          | TFn ([ acc_ty; item_ty ], ret), TSet inner
            when Types.equal acc_ty init.ty && Types.equal item_ty inner && Types.equal ret init.ty ->
              Types.set_module_name inner
              |> Result.map (fun set_module ->
                     typed_ir init.ty
                       (apply "List.fold_left"
                          [ fn.ocaml_expr;
                            init.ocaml_expr;
                            apply (set_module ^ ".elements")
                              [ collection.ocaml_expr ] ]))
          | TFn _, TSet _ -> Error.error "reduce function type does not match init and set"
          | _, TSet _ -> Error.error "reduce expects a function"
          | _ -> Error.error "reduce expects a list, vector, or set"))
  | _ -> Error.error "reduce expects function, init, and collection"

and compile_apply scope env arg_forms =
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
            ( compile_function_arg scope env fn_form,
              compile_args_for scope env fixed_forms,
              compile_expr scope env collection_form )
          with
          | (Error _ as err), _, _ -> err
          | _, (Error _ as err), _ -> err
          | _, _, (Error _ as err) -> err
          | Ok fn, Ok fixed_args, Ok collection -> (
              match collection_to_list_expr collection with
              | Error _ -> Error.error "apply expects a list, vector, or set"
              | Ok (inner, list_expr) -> (
                  match fn.ty with
                  | TFn ([ TInt; TInt ], TInt)
                    when Types.equal inner TInt
                         && List.for_all (fun arg -> Types.equal arg.ty TInt) fixed_args ->
                      let values_expr =
                        match fixed_args with
                        | [] -> list_expr
                        | _ ->
                            Ocaml_ir.Infix
                              ( "@",
                                Ocaml_ir.List
                                  (List.map (fun arg -> arg.ocaml_expr) fixed_args),
                                list_expr )
                      in
                      Ok
                        (typed_ir TInt
                           (apply "List.fold_left"
                              [ fn.ocaml_expr; Ocaml_ir.Int 0; values_expr ]))
                  | TFn ([ TInt; TInt ], TInt) ->
                      Error.error "apply currently supports int binary reducers"
                  | TFn _ -> Error.error "apply currently supports int binary reducers"
                  | _ -> Error.error "apply expects a function"))))
  | _ -> Error.error "apply expects function and collection"

and compile_comp scope env arg_forms =
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
                   match compile_function_arg scope env form with
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

and compile_partial scope env arg_forms =
  match arg_forms with
  | fn_form :: fixed_forms -> (
      match (compile_function_arg scope env fn_form, compile_args_for scope env fixed_forms) with
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

and compile_identity scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ arg ] -> Ok arg
  | Ok _ -> Error.error "identity expects 1 arguments"

and compile_constantly scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ value ] ->
      Ok
        (typed_ir (TFn ([ TAny ], value.ty))
           (Ocaml_ir.Fun ([ Ocaml_ir.PAny ], value.ocaml_expr)))
  | Ok _ -> Error.error "constantly expects 1 arguments"

and compile_complement scope env arg_forms =
  match arg_forms with
  | [ fn_form ] -> (
      match compile_function_arg scope env fn_form with
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

and compile_predicate_combinator scope env name arg_forms =
  let compile_fns =
    arg_forms
    |> List.fold_left
         (fun acc form ->
           match acc with
           | Error _ as err -> err
           | Ok fns -> (
               match compile_function_arg scope env form with
               | Error _ as err -> err
               | Ok fn -> Ok (fn :: fns)))
         (Ok [])
    |> Result.map List.rev
  in
  match compile_fns with
  | Error _ as err -> err
  | Ok [] -> Error.error (name ^ " expects at least 1 predicate")
  | Ok fns -> (
      let rec collect arg_ty exprs = function
        | [] -> Ok (arg_ty, List.rev exprs)
        | fn :: rest -> (
            match fn.ty with
            | TFn ([ current_arg ], TBool)
              when option_for_all (fun arg_ty -> Types.equal arg_ty current_arg) arg_ty ->
                collect (Some current_arg)
                  (Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "x" ]) :: exprs)
                  rest
            | TFn _ ->
                Error.error (name ^ " expects predicates with the same argument type")
            | _ -> Error.error (name ^ " expects predicates"))
      in
      match collect None [] fns with
      | Error _ as err -> err
      | Ok (None, _) -> Error.error (name ^ " expects at least 1 predicate")
      | Ok (Some arg_ty, exprs) ->
          let op = if name = "every-pred" then "&&" else "||" in
          let body =
            match exprs with
            | [] -> Ocaml_ir.Bool (name = "every-pred")
            | first :: rest ->
                List.fold_left
                  (fun acc expr -> Ocaml_ir.Infix (op, acc, expr))
                  first rest
          in
          Ok
            (typed_ir (TFn ([ arg_ty ], TBool))
               (Ocaml_ir.Fun ([ Ocaml_ir.PVar "x" ], body))))

and compile_juxt scope env arg_forms =
  let compile_fns =
    arg_forms
    |> List.fold_left
         (fun acc form ->
           match acc with
           | Error _ as err -> err
           | Ok fns -> (
               match compile_function_arg scope env form with
               | Error _ as err -> err
               | Ok fn -> Ok (fn :: fns)))
         (Ok [])
    |> Result.map List.rev
  in
  match compile_fns with
  | Error _ as err -> err
  | Ok [] -> Error.error "juxt expects at least 1 function"
  | Ok fns -> (
      let rec collect arg_ty ret_ty exprs = function
        | [] -> Ok (arg_ty, ret_ty, List.rev exprs)
        | fn :: rest -> (
            match fn.ty with
            | TFn ([ current_arg ], current_ret)
              when option_for_all
                     (fun arg_ty ->
                       Types.compatible ~expected:arg_ty ~actual:current_arg)
                     arg_ty
                   && option_for_all
                        (fun ret_ty ->
                          Types.compatible ~expected:ret_ty ~actual:current_ret)
                        ret_ty ->
                collect (Some current_arg) (Some current_ret)
                  (Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "x" ]) :: exprs)
                  rest
            | TFn ([ current_arg ], _)
              when option_for_all
                     (fun arg_ty ->
                       Types.compatible ~expected:arg_ty ~actual:current_arg)
                     arg_ty ->
                Error.error "juxt functions must return the same type"
            | TFn _ -> Error.error "juxt functions must accept the same argument type"
            | _ -> Error.error "juxt expects functions")
      in
      match collect None None [] fns with
      | Error _ as err -> err
      | Ok (Some arg_ty, Some ret_ty, exprs) ->
          Ok
            (typed_ir (TFn ([ arg_ty ], TVector ret_ty))
               (Ocaml_ir.Fun
                  ( [ Ocaml_ir.PVar "x" ],
                    apply "Rrbvec.of_list" [ Ocaml_ir.List exprs ] )))
      | Ok _ -> Error.error "juxt expects at least 1 function")

and compile_distinct_question scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok ([] | [ _ ]) -> Ok (typed_ir TBool (Ocaml_ir.Bool true))
  | Ok (first :: _ as args) ->
      if List.for_all (fun arg -> Types.equal first.ty arg.ty) args then
        Ok
          (typed_ir TBool
             (Ocaml_ir.Infix
                ( "=",
                  apply "List.length"
                    [ apply "List.sort_uniq"
                        [ Ocaml_ir.Ident "compare";
                          Ocaml_ir.List (List.map (fun arg -> arg.ocaml_expr) args) ] ],
                  Ocaml_ir.Int (List.length args) )))
      else Error.error "distinct? arguments must have the same type"

and compile_compare scope env arg_forms =
  match compile_args_for scope env arg_forms with
  | Error _ as err -> err
  | Ok [ left; right ] ->
      if not (Types.equal left.ty right.ty) then
        Error.error "compare arguments must have the same type"
      else if not (comparable_type left.ty) then
        Error.error "compare expects comparable arguments"
      else
        Ok
          (typed_ir TInt
             (apply "Stdlib.compare" [ left.ocaml_expr; right.ocaml_expr ]))
  | Ok _ -> Error.error "compare expects 2 arguments"

and compile_key_extreme scope env name arg_forms =
  match arg_forms with
  | fn_form :: value_forms when value_forms <> [] -> (
      match (compile_function_arg scope env fn_form, compile_args_for scope env value_forms) with
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
                let compare_op = if name = "max-key" then ">" else "<" in
                let expr =
                  match rest with
                  | [] -> first.ocaml_expr
                  | _ ->
                      Ocaml_ir.Let
                        ( [ (Ocaml_ir.PVar "key_fn", fn.ocaml_expr);
                            ( Ocaml_ir.PVar "choose",
                              Ocaml_ir.Fun
                                ( [ Ocaml_ir.PVar "best"; Ocaml_ir.PVar "item" ],
                                  Ocaml_ir.If
                                    ( Ocaml_ir.Infix
                                        ( compare_op,
                                          apply "Stdlib.compare"
                                            [ Ocaml_ir.Apply
                                                ( Ocaml_ir.Ident "key_fn",
                                                  [ Ocaml_ir.Ident "item" ] );
                                              Ocaml_ir.Apply
                                                ( Ocaml_ir.Ident "key_fn",
                                                  [ Ocaml_ir.Ident "best" ] ) ],
                                          Ocaml_ir.Int 0 ),
                                      Ocaml_ir.Ident "item",
                                      Ocaml_ir.Ident "best" ) ) ) ],
                          apply "List.fold_left"
                            [ Ocaml_ir.Ident "choose";
                              first.ocaml_expr;
                              Ocaml_ir.List (List.map (fun value -> value.ocaml_expr) rest) ] )
                in
                Ok (typed_ir first.ty expr)
            | TFn _ -> Error.error (name ^ " expects a key function matching values")
            | _ -> Error.error (name ^ " expects a function")))
  | _ -> Error.error (name ^ " expects function and values")

and compile_hash_set scope env arg_forms =
  match arg_forms with
  | [] -> Error.error "empty hash-set requires a type annotation"
  | first :: rest -> (
      match compile_expr scope env first with
      | Error _ as err -> err
      | Ok first_expr ->
          let rec loop values = function
            | [] ->
                Result.bind (Types.set_module_name first_expr.ty) (fun set_module ->
                       let rec coerce_values acc = function
                         | [] -> Ok (List.rev acc)
                         | value :: rest ->
                             Result.bind (coerce_set_element first_expr.ty value)
                               (fun value -> coerce_values (value :: acc) rest)
                       in
                       coerce_values [] (List.rev values)
                       |> Result.map (fun values ->
                              typed_ir (TSet first_expr.ty)
                                (Ocaml_ir.Apply
                                   ( Ocaml_ir.Ident (set_module ^ ".of_list"),
                                     [ Ocaml_ir.List values ] ))))
            | form :: rest -> (
                match compile_expr scope env form with
                | Error _ as err -> err
                | Ok expr ->
                    if Types.same_shape first_expr.ty expr.ty then
                      loop (expr :: values) rest
                    else Error.error "hash-set elements must all have the same type")
          in
          loop [ first_expr ] rest)

and compile_set_of arg_forms =
  match arg_forms with
  | [ FKeyword keyword ] -> (
      match Type_annotation.of_keyword keyword with
      | Error _ -> Error.error ("unknown set element type " ^ keyword)
      | Ok element_ty ->
          Types.set_module_name element_ty
          |> Result.map (fun set_module ->
                 typed_ir (TSet element_ty) (Ocaml_ir.Ident (set_module ^ ".empty"))))
  | _ -> Error.error "set-of expects one type keyword"

and compile_disj scope env arg_forms =
  match arg_forms with
  | collection_form :: value_forms -> (
      match compile_expr scope env collection_form with
      | Error _ as err -> err
      | Ok collection -> (
          match collection.ty with
          | TSet inner ->
              let rec remove_values expression = function
                | [] -> Ok (typed_ir collection.ty expression)
                | value_form :: rest -> (
                    match compile_expr scope env value_form with
                    | Error _ as err -> err
                    | Ok value ->
                        if Types.same_shape inner value.ty then
                          Result.bind (Types.set_module_name inner)
                            (fun set_module ->
                              Result.bind (coerce_set_element inner value) (fun value ->
                                     remove_values
                                       (Ocaml_ir.Apply
                                          ( Ocaml_ir.Ident (set_module ^ ".remove"),
                                            [ value; expression ] ))
                                       rest))
                        else Error.error "disj value type must match set element type")
              in
              remove_values collection.ocaml_expr value_forms
          | _ -> Error.error "disj expects a set"))
  | [] -> Error.error "disj expects a set"

and compile_args_for scope env arg_forms =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | form :: rest -> (
        match compile_expr scope env form with
        | Ok expr -> loop (expr :: acc) rest
        | Error _ as err -> err)
  in
  loop [] arg_forms
