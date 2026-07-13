open Ast
open Types
open Expression_support

module Env = Compiler_environment

type expression_result = (typed_expr, Error.t) result

type t = {
  compile_call : string -> Env.t -> string -> Ast.form list -> expression_result;
  compile_args_for :
    string -> Env.t -> Ast.form list -> (typed_expr list, Error.t) result;
}

let create ~compile_expr =
  let special_forms : Special_form_elaborator.t =
    Special_form_elaborator.create ~compile_expr
  in
  let collection : Collection_operation_elaborator.t =
    Collection_operation_elaborator.create ~compile_expr
  in
  let sequence : Sequence_call_elaborator.t =
    Sequence_call_elaborator.create ~compile_expr
  in
  let functions : Function_combinator_elaborator.t =
    Function_combinator_elaborator.create ~compile_expr
  in
  let comparisons : Comparison_set_elaborator.t =
    Comparison_set_elaborator.create ~compile_expr
  in
  let compile_vector = special_forms.compile_vector in
  let compile_list = collection.compile_list in
  let compile_list_star = collection.compile_list_star in
  let compile_range = collection.compile_range in
  let compile_list_of = collection.compile_list_of in
  let compile_vector_of = collection.compile_vector_of in
  let compile_conj = collection.compile_conj in
  let compile_cons = collection.compile_cons in
  let compile_subvec = collection.compile_subvec in
  let compile_nth = collection.compile_nth in
  let compile_get = collection.compile_get in
  let compile_assoc = collection.compile_assoc in
  let compile_dissoc = collection.compile_dissoc in
  let compile_merge = collection.compile_merge in
  let compile_hash_map = collection.compile_hash_map in
  let compile_update = collection.compile_update in
  let compile_select_keys = collection.compile_select_keys in
  let compile_contains = collection.compile_contains in
  let compile_keys = collection.compile_keys in
  let compile_vals = collection.compile_vals in
  let compile_sort_by = sequence.compile_sort_by in
  let compile_mapcat = sequence.compile_mapcat in
  let compile_repeatedly = sequence.compile_repeatedly in
  let compile_reductions = sequence.compile_reductions in
  let compile_split_with = sequence.compile_split_with in
  let compile_partition_by = sequence.compile_partition_by in
  let compile_run_bang = sequence.compile_run_bang in
  let compile_map_indexed = sequence.compile_map_indexed in
  let compile_filterv = sequence.compile_filterv in
  let compile_mapv = sequence.compile_mapv in
  let compile_reduce_kv = sequence.compile_reduce_kv in
  let compile_some = sequence.compile_some in
  let compile_sequence_bool_predicate = sequence.compile_sequence_bool_predicate in
  let compile_map_call = sequence.compile_map_call in
  let compile_filter = sequence.compile_filter in
  let compile_reduce = sequence.compile_reduce in
  let compile_apply = functions.compile_apply in
  let compile_comp = functions.compile_comp in
  let compile_partial = functions.compile_partial in
  let compile_identity = functions.compile_identity in
  let compile_constantly = functions.compile_constantly in
  let compile_complement = functions.compile_complement in
  let compile_predicate_combinator = functions.compile_predicate_combinator in
  let compile_juxt = functions.compile_juxt in
  let compile_distinct_question = comparisons.compile_distinct_question in
  let compile_compare = comparisons.compile_compare in
  let compile_key_extreme = comparisons.compile_key_extreme in
  let compile_hash_set = comparisons.compile_hash_set in
  let compile_set_of = comparisons.compile_set_of in
  let compile_disj = comparisons.compile_disj in
  let rec compile_ocaml_arguments scope env forms =
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
      Semantic_ir.Labelled_apply
        ( Semantic_ir.Ident function_name,
          List.map
            (fun (label, argument) -> (label, argument.semantic_expr))
            arguments )
    else
      Semantic_ir.Apply
        ( Semantic_ir.Ident function_name,
          List.map (fun (_, argument) -> argument.semantic_expr) arguments )
  
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
            | [ value ] -> Some value.semantic_expr
            | values -> Some (Semantic_ir.Tuple (List.map (fun value -> value.semantic_expr) values))
          in
          Ok
            (typed_ir (return_ty args)
               (Semantic_ir.Constructor (constructor_name, payload)))
    in
    match name with
    | "raise" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ arg ] ->
            Ok
              (typed_ir TUnknown
                 (Semantic_ir.Apply (Semantic_ir.Ident "raise", [ arg.semantic_expr ])))
        | Ok _ -> Error.error "raise expects 1 arguments")
    | "Some" ->
        constructor
          (function [ value ] -> TOcaml_app ("option", [ value.ty ]) | _ -> TUnknown)
          1
    | "None" -> constructor (fun _ -> TOcaml_app ("option", [ TUnknown ])) 0
    | "Ok" ->
        constructor
          (function
            | [ value ] -> TOcaml_app ("result", [ value.ty; TUnknown ])
            | _ -> TUnknown)
          1
    | "Error" ->
        constructor
          (function
            | [ value ] -> TOcaml_app ("result", [ TUnknown; value.ty ])
            | _ -> TUnknown)
          1
    | "ocaml-array" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [] -> Error.error "empty OCaml array requires a type"
        | Ok (first :: rest as values) ->
            if List.for_all (fun value -> Types.equal first.ty value.ty) rest then
              Ok
                (typed_ir (TArray first.ty)
                   (Semantic_ir.Array (List.map (fun value -> value.semantic_expr) values)))
            else Error.error "OCaml array elements must have the same type")
    | "ocaml-array-of" -> (
        match arg_forms with
        | [ FKeyword keyword ] -> (
            match Type_annotation.of_keyword keyword with
            | Error _ as err -> err
            | Ok element_ty -> Ok (typed_ir (TArray element_ty) (Semantic_ir.Array [])))
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
                       (apply "Array.get" [ array.semantic_expr; index.semantic_expr ]))
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
                          [ array.semantic_expr; index.semantic_expr; value.semantic_expr ]))
            | _ -> Error.error "ocaml-array-set! expects an OCaml array")
        | Ok _ -> Error.error "ocaml-array-set! expects 3 arguments")
    | "ocaml-ref" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ] -> Ok (typed_ir (TRef value.ty) (apply "ref" [ value.semantic_expr ]))
        | Ok _ -> Error.error "ocaml-ref expects 1 argument")
    | "ocaml-deref" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ] -> (
            match value.ty with
            | TRef referenced_ty ->
                Ok (typed_ir referenced_ty (Semantic_ir.Prefix ("!", value.semantic_expr)))
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
                       (Semantic_ir.Infix (":=", reference.semantic_expr, value.semantic_expr)))
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
                 (Semantic_ir.Tuple (List.map (fun value -> value.semantic_expr) values))))
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
                      let instantiated_record =
                        match
                          Types.instantiate_type
                            ~templates:
                              (List.map
                                 (fun ((field : field), _) -> field.ty)
                                 values)
                            ~actuals:
                              (List.map (fun (_, value) -> value.ty) values)
                            (TNamed_record record)
                        with
                        | TNamed_record record -> record
                        | _ -> record
                      in
                      Ok
                        {
                          (typed_ir
                             (TNamed_record instantiated_record)
                             (Semantic_ir.Record
                                ( List.map
                                    (fun ((field : field), value) ->
                                      (field.ocaml_name, value.semantic_expr))
                                    values,
                                  Some
                                    (record_type_application record.type_name
                                       record.type_parameters) )))
                          with
                          record_values =
                            Some
                              (List.map
                                 (fun ((field : field), value) -> (field, value.semantic_expr))
                                 values);
                        }))
        | _ -> Error.error "ocaml-record expects a record type and fields")
    | "ocaml-field" -> (
        match arg_forms with
        | [ target_form; FSymbol field_name ] -> (
            match compile_expr scope env target_form with
            | Error _ as err -> err
            | Ok target -> (
                match target.ty with
                | ty when is_ocaml_owned_type ty ->
                    Ok
                      (typed_ir TUnknown
                         (Semantic_ir.Field
                            (target.semantic_expr, Names.sanitize_name field_name)))
                | TRecord fields | TNamed_record { fields; _ } -> (
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
                             (Semantic_ir.Field (target.semantic_expr, field.ocaml_name))))
                | _ -> Error.error "ocaml-field expects a record value"))
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
                      Ok
                        (Types.instantiate_type ~templates:payload_tys
                           ~actuals:(List.map (fun payload -> payload.ty) payloads)
                           ret)
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
                      | [ payload ] -> Some payload.semantic_expr
                      | _ ->
                          Some
                            (Semantic_ir.Tuple
                               (List.map (fun payload -> payload.semantic_expr) payloads))
                    in
                    Ok
                      (typed_ir constructor_ty
                         (Semantic_ir.Constructor (constructor_name, payload_expr)))))
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
          (fun expression -> Semantic_ir.Infix ("+", expression, Semantic_ir.Int 1))
          arg_forms
    | "dec" ->
        compile_int_unary_call scope env name
          (fun expression -> Semantic_ir.Infix ("-", expression, Semantic_ir.Int 1))
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
          (fun expression -> Semantic_ir.Infix ("=", expression, Semantic_ir.Int 0))
          arg_forms
        |> Result.map (fun expr -> { expr with ty = TBool })
    | "pos?" ->
        compile_int_unary_call scope env name
          (fun expression -> Semantic_ir.Infix (">", expression, Semantic_ir.Int 0))
          arg_forms
        |> Result.map (fun expr -> { expr with ty = TBool })
    | "neg?" ->
        compile_int_unary_call scope env name
          (fun expression -> Semantic_ir.Infix ("<", expression, Semantic_ir.Int 0))
          arg_forms
        |> Result.map (fun expr -> { expr with ty = TBool })
    | "even?" ->
        compile_int_unary_call scope env name
          (fun expression ->
            Semantic_ir.Infix
              ( "=",
                Semantic_ir.Infix ("mod", expression, Semantic_ir.Int 2),
                Semantic_ir.Int 0 ))
          arg_forms
        |> Result.map (fun expr -> { expr with ty = TBool })
    | "odd?" ->
        compile_int_unary_call scope env name
          (fun expression ->
            Semantic_ir.Infix
              ( "<>",
                Semantic_ir.Infix ("mod", expression, Semantic_ir.Int 2),
                Semantic_ir.Int 0 ))
          arg_forms
        |> Result.map (fun expr -> { expr with ty = TBool })
    | "str" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok args ->
            let expr =
              match args with
              | [] -> Semantic_ir.String ""
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
          (fun expression -> Semantic_ir.Prefix ("lnot", expression))
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
                 (Semantic_ir.Apply (Semantic_ir.Ident printer, [ Codegen.print_expr_ir arg ])))
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
        | Ok { ty = TFn (payload_tys, return_ty); ocaml_name; _ } ->
            constructor ~constructor_name:ocaml_name
              (fun args ->
                Types.instantiate_type ~templates:payload_tys
                  ~actuals:(List.map (fun arg -> arg.ty) args)
                  return_ty)
              (List.length payload_tys)
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
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "String.sub",
                      [ source.semantic_expr;
                        start.semantic_expr;
                        Semantic_ir.Infix
                          ( "-",
                            Semantic_ir.Apply
                              (Semantic_ir.Ident "String.length", [ source.semantic_expr ]),
                            start.semantic_expr ) ] )))
        | TString, _ -> Error.error "subs indexes must be int"
        | _ -> Error.error "subs expects a string")
    | Ok [ source; start; stop ] -> (
        match (source.ty, start.ty, stop.ty) with
        | TString, TInt, TInt ->
            Ok
              (typed_ir TString
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "String.sub",
                      [ source.semantic_expr;
                        start.semantic_expr;
                        Semantic_ir.Infix ("-", stop.semantic_expr, start.semantic_expr) ] )))
        | TString, _, _ -> Error.error "subs indexes must be int"
        | _ -> Error.error "subs expects a string")
    | Ok _ -> Error.error "subs expects string, start, and optional end"
  
  and compile_function_arg scope env = function
    | FSymbol name -> lookup_function scope env name
    | form -> compile_expr scope env form
  
  and compile_named_function_call scope env name arg_forms =
    match lookup_binding scope env name with
    | Error _ -> (
        match Protocol.lookup_marker scope env name with
        | Some _ -> compile_protocol_call scope env name arg_forms
        | None -> (
            match ocaml_call_target scope env name with
            | Some _ -> compile_inferred_ocaml_call scope env name arg_forms
            | None -> compile_protocol_call scope env name arg_forms))
    | Ok { host_reference = Some (Ocaml_value _); _ } ->
        compile_inferred_ocaml_call scope env name arg_forms
    | Ok fn -> (
        match compile_args_for scope env arg_forms with
        | Error _ as err -> err
        | Ok args -> (
            match fn.ty with
            | TFn (param_tys, ret)
              when List.length param_tys = List.length args
                   && List.for_all2
                        (fun expected arg ->
                          Types.assignable ~policy:Host_boundary ~expected
                            ~actual:arg.ty)
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
                  | Some index, TUnknown -> (
                      match List.nth_opt args index with
                      | Some arg -> arg.ty
                      | None -> ret)
                  | _ -> ret
                in
                Ok (typed_ir ret (Semantic_ir.Apply (Semantic_ir.Ident fn.ocaml_name, arg_exprs)))
            | TFn _ -> Error.error (name ^ " called with incompatible arguments")
            | _ -> Error.error (name ^ " is not callable")))
  
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
                                      Types.assignable ~policy:Host_boundary ~expected
                                        ~actual:arg.ty)
                                    param_tys args ->
                            Ok
                              (typed_ir ret
                                 (Semantic_ir.Apply
                                    ( Semantic_ir.Ident impl.ocaml_name,
                                      List.map (fun arg -> arg.semantic_expr) args )))
                        | TFn _ -> Error.error (name ^ " called with incompatible arguments")
                        | _ -> Error.error (name ^ " is not callable"))))
            | _ -> Error.error (name ^ " is not callable")))
  
  and compile_args_for scope env arg_forms =
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | form :: rest -> (
          match compile_expr scope env form with
          | Ok expr -> loop (expr :: acc) rest
          | Error _ as err -> err)
    in
    loop [] arg_forms
  in
  { compile_call; compile_args_for }
