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

let array_element_type = function
  | TArray element_ty -> Some element_ty
  | TOcaml "array" -> Some TUnknown
  | TOcaml_app ("array", [ element_ty ]) ->
      Some (lg_metadata_type_for_ocaml_type element_ty)
  | TUnknown | TVar _ -> Some TUnknown
  | _ -> None

let compatible_array_types left right =
  match (array_element_type left, array_element_type right) with
  | Some left, Some right ->
      Types.equal left TUnknown
      || Types.equal right TUnknown
      || Types.assignable ~policy:Host_boundary ~expected:left ~actual:right
      || Types.assignable ~policy:Host_boundary ~expected:right ~actual:left
  | _ -> false

let int_parameter_type = function
  | TInt | TUnknown | TVar _ -> true
  | _ -> false

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
  let compile_find = collection.compile_find in
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
    match lookup_binding scope env name with
    | Ok _ when not (Resolver.starts_with_uppercase name) ->
        compile_named_function_call scope env name arg_forms
    | Error _ when Env.core_excluded ~scope name env ->
        Error.error ("unknown function " ^ name)
    | Ok _ | Error _ ->
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
    | constructor_name
      when String.ends_with ~suffix:"." constructor_name -> (
        let type_name =
          String.sub constructor_name 0 (String.length constructor_name - 1)
        in
        match Env.find_opt type_name env with
        | Some { host_reference = Some (Ocaml_module module_path); _ } ->
            compile_inferred_ocaml_call scope env (module_path ^ ".create")
              arg_forms
        | _ -> (match Resolver.lookup_record_type scope env type_name with
        | Error _ as err -> err
        | Ok record -> (
            match compile_args () with
            | Error _ as err -> err
            | Ok args when List.length args <> List.length record.fields ->
                Error.error
                  (constructor_name ^ " expects "
                 ^ string_of_int (List.length record.fields)
                 ^ " arguments")
            | Ok args ->
                let instantiated =
                  Types.instantiate_type
                    ~templates:
                      (List.map (fun parameter -> TVar parameter)
                         record.type_parameters)
                    ~actuals:(List.map (fun arg -> arg.ty) args)
                    (TNamed_record record)
                in
                let record =
                  match instantiated with
                  | TNamed_record record -> record
                  | _ -> record
                in
                let values = List.combine record.fields args in
                Ok
                  {
                    (typed_ir (TNamed_record record)
                       (Semantic_ir.Record
                          ( List.map
                              (fun ((field : field), arg) ->
                                (field.ocaml_name, arg.semantic_expr))
                              values,
                            Some
                              (record_type_application record.type_name
                                 record.type_parameters) ))) with
                    record_values =
                      Some
                        (List.map
                           (fun (field, arg) -> (field, arg.semantic_expr))
                           values);
                  })))
    | method_name when String.starts_with ~prefix:"." method_name -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok ({ ty = TOcaml receiver_type; _ } :: _) -> (
            match Host_interop.instance_method ~receiver_type ~method_name with
            | None -> Error.error ("unsupported host method " ^ method_name)
            | Some function_name ->
                compile_inferred_ocaml_call scope env function_name arg_forms)
        | Ok ({ ty = TOcaml_app (receiver_type, []); _ } :: _) -> (
            match Host_interop.instance_method ~receiver_type ~method_name with
            | None -> Error.error ("unsupported host method " ^ method_name)
            | Some function_name ->
                compile_inferred_ocaml_call scope env function_name arg_forms)
        | Ok _ -> Error.error (method_name ^ " expects a host receiver"))
    | field_access when String.starts_with ~prefix:".-" field_access -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ target ] -> (
            let keyword =
              ":"
              ^ String.sub field_access 2 (String.length field_access - 2)
            in
            match target.ty with
            | TRecord fields | TNamed_record { fields; _ } -> (
                match find_field keyword fields with
                | None -> Error.error ("unknown field " ^ keyword)
                | Some field ->
                    Ok
                      (typed_ir field.ty
                         (Structural_map.field_expr target field)))
            | _ -> Error.error (field_access ^ " expects a deftype value"))
        | Ok _ -> Error.error (field_access ^ " expects 1 argument"))
    | ".valAt" | "-lookup" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ target; key ] ->
            Ok
              (typed_ir (TNullable TUnknown)
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_map.get_option",
                      [ target.semantic_expr; key.semantic_expr ] )))
        | Ok [ target; key; default ] ->
            Ok
              (typed_ir (TNullable TUnknown)
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident
                        "Lg_runtime.Runtime_map.get_option_default",
                      [ target.semantic_expr;
                        key.semantic_expr;
                        default.semantic_expr;
                      ] )))
        | Ok _ -> Error.error ".valAt expects 2 or 3 arguments")
    | ".containsKey" -> compile_contains scope env arg_forms
    | ".entryAt" -> compile_find scope env arg_forms
    | "-contains-key?" -> compile_contains scope env arg_forms
    | "reduced" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ] ->
            Ok
              (typed_ir (Types.reduced value.ty)
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_reduced.reduced",
                      [ value.semantic_expr ] )))
        | Ok _ -> Error.error "reduced expects 1 arguments")
    | "reduced?" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ] -> (
            match Types.reduced_element value.ty with
            | Some _ ->
                Ok
                  (typed_ir TBool
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_reduced.is_reduced",
                          [ value.semantic_expr ] )))
            | None ->
                Ok
                  (typed_ir TBool
                     (Semantic_ir.Sequence
                        [ value.semantic_expr; Semantic_ir.Bool false ])))
        | Ok _ -> Error.error "reduced? expects 1 arguments")
    | "unreduced" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ] -> (
            match Types.reduced_element value.ty with
            | Some inner ->
                Ok
                  (typed_ir inner
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_reduced.unreduced",
                          [ value.semantic_expr ] )))
            | None -> Ok value)
        | Ok _ -> Error.error "unreduced expects 1 arguments")
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
    | "ocaml-seq-unfold" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok
            [ { ty =
                  TFn
                    ( [ parameter_ty ],
                      TOcaml_app
                        ("option", [ TTuple [ element_ty; next_ty ] ]) );
                semantic_expr = step;
                _ };
              initial ]
          when Types.assignable ~policy:Host_boundary ~expected:parameter_ty
                 ~actual:initial.ty
               && Types.assignable ~policy:Host_boundary ~expected:parameter_ty
                    ~actual:next_ty ->
            Ok
              (typed_ir (TOcaml_app ("Seq.t", [ element_ty ]))
                 (apply "Lg_runtime.Runtime_seq.memoize"
                    [ apply "Seq.unfold" [ step; initial.semantic_expr ] ]))
        | Ok [ _; _ ] ->
            Error.error
              "ocaml-seq-unfold expects a state step function and initial state"
        | Ok _ -> Error.error "ocaml-seq-unfold expects 2 arguments")
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
    | "ocaml-array-make" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ { ty = TInt; semantic_expr; _ } ] ->
            let empty_value =
              apply "Obj.magic" [ Semantic_ir.Constructor ("None", None) ]
            in
            Ok
              (typed_ir (TArray TUnknown)
                 (apply "Array.make" [ semantic_expr; empty_value ]))
        | Ok [ _ ] -> Error.error "ocaml-array-make size must be int"
        | Ok _ -> Error.error "ocaml-array-make expects 1 argument")
    | "ocaml-array-from" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ ({ ty = TArray _; semantic_expr; _ } as array) ] ->
            Ok { array with semantic_expr = apply "Array.copy" [ semantic_expr ] }
        | Ok [ collection ] -> (
            match Core_sequence_transform.collection_to_seq_expr collection with
            | Error _ -> Error.error "ocaml-array-from expects a seqable value"
            | Ok (element_ty, sequence) ->
                Ok
                  (typed_ir (TArray element_ty)
                     (apply "Array.of_seq" [ sequence ])))
        | Ok _ -> Error.error "ocaml-array-from expects 1 argument")
    | "ocaml-array-get" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ array; index ] -> (
            match array_element_type array.ty with
            | Some element_ty ->
                if int_parameter_type index.ty then
                  Ok
                    (typed_ir element_ty
                       (apply "Array.get" [ array.semantic_expr; index.semantic_expr ]))
                else Error.error "OCaml array index must be int"
            | None -> Error.error "ocaml-array-get expects an OCaml array")
        | Ok _ -> Error.error "ocaml-array-get expects 2 arguments")
    | "ocaml-array-set!" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ array; index; value ] -> (
            match array_element_type array.ty with
            | Some element_ty ->
                if not (int_parameter_type index.ty) then
                  Error.error "OCaml array index must be int"
                else if
                  not
                    (Types.assignable ~policy:Host_boundary ~expected:element_ty
                       ~actual:value.ty)
                then
                  Error.error "OCaml array value must match element type"
                else
                  Ok
                    (typed_ir TUnit
                       (apply "Array.set"
                          [ array.semantic_expr; index.semantic_expr; value.semantic_expr ]))
            | None -> Error.error "ocaml-array-set! expects an OCaml array")
        | Ok _ -> Error.error "ocaml-array-set! expects 3 arguments")
    | "ocaml-array-length" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ] -> (
            match array_element_type value.ty with
            | Some _ ->
                Ok
                  (typed_ir TInt
                     (apply "Array.length" [ value.semantic_expr ]))
            | None ->
                Error.error
                  ("ocaml-array-length expects an OCaml array, got "
                 ^ Types.source_name value.ty))
        | Ok _ -> Error.error "ocaml-array-length expects 1 argument")
    | "ocaml-array-copy!" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok
            [ { ty = source_type; semantic_expr = source; _ };
              { ty = TInt; semantic_expr = source_start; _ };
              { ty = TInt; semantic_expr = source_end; _ };
              { ty = target_type; semantic_expr = target; _ };
              { ty = TInt; semantic_expr = target_start; _ } ]
          when compatible_array_types source_type target_type ->
            let length = Semantic_ir.Infix ("-", source_end, source_start) in
            Ok
              (typed_ir TUnit
                 (apply "Array.blit"
                    [ source; source_start; target; target_start; length ]))
        | Ok [ _; _; _; _; _ ] ->
            Error.error "ocaml-array-copy! expects compatible arrays and int indexes"
        | Ok _ -> Error.error "ocaml-array-copy! expects 5 arguments")
    | "ocaml-array-copy" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ ({ ty = TArray _; semantic_expr; _ } as array) ] ->
            Ok { array with semantic_expr = apply "Array.copy" [ semantic_expr ] }
        | Ok [ { ty = TUnknown; semantic_expr; _ } ] ->
            Ok
              (typed_ir (TArray TUnknown)
                 (apply "Array.copy" [ semantic_expr ]))
        | Ok [ _ ] -> Error.error "ocaml-array-copy expects an OCaml array"
        | Ok _ -> Error.error "ocaml-array-copy expects 1 argument")
    | "ocaml-array-slice" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok
            [ { ty = array_type; semantic_expr = array; _ };
              { ty = TInt; semantic_expr = from; _ };
              { ty = TInt; semantic_expr = to_; _ } ] -> (
            match array_element_type array_type with
            | Some element_ty ->
                let length = Semantic_ir.Infix ("-", to_, from) in
                Ok
                  (typed_ir (TArray element_ty)
                     (apply "Array.sub" [ array; from; length ]))
            | None -> Error.error "ocaml-array-slice expects an OCaml array")
        | Ok _ -> Error.error "ocaml-array-slice expects an array and two int indexes")
    | "ocaml-array-append" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok
            [ { ty = left_type; semantic_expr = left; _ };
              { ty = right_type; semantic_expr = right; _ } ]
          when compatible_array_types left_type right_type -> (
            match (array_element_type left_type, array_element_type right_type) with
            | Some left_ty, Some right_ty ->
                let element_ty =
                  if Types.equal left_ty TUnknown then right_ty else left_ty
                in
                Ok
                  (typed_ir (TArray element_ty)
                     (apply "Array.append" [ left; right ]))
            | _ -> Error.error "ocaml-array-append expects compatible arrays")
        | Ok [ _; _ ] -> Error.error "ocaml-array-append expects compatible arrays"
        | Ok _ -> Error.error "ocaml-array-append expects 2 arguments")
    | "ocaml-array-map" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok
            [ { ty = TFn ([ parameter_ty ], return_ty); semantic_expr = fn; _ };
              { ty = array_type; semantic_expr = array; _ } ] -> (
            match array_element_type array_type with
            | Some element_ty
              when Types.assignable ~policy:Host_boundary ~expected:parameter_ty
                     ~actual:element_ty ->
                Ok
                  (typed_ir (TArray return_ty)
                     (apply "Array.map" [ fn; array ]))
            | Some _ | None ->
                Error.error
                  "ocaml-array-map expects a unary function and compatible array")
        | Ok [ _; _ ] ->
            Error.error "ocaml-array-map expects a unary function and compatible array"
        | Ok _ -> Error.error "ocaml-array-map expects 2 arguments")
    | "ocaml-array-sort!" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok
            [ { ty = TFn ([ left_ty; right_ty ], TInt); semantic_expr = cmp; _ };
              { ty = TArray element_ty; semantic_expr = array; _ } ]
          when Types.assignable ~policy:Host_boundary ~expected:left_ty
                 ~actual:element_ty
               && Types.assignable ~policy:Host_boundary ~expected:right_ty
                    ~actual:element_ty ->
            Ok (typed_ir TUnit (apply "Array.sort" [ cmp; array ]))
        | Ok [ _; _ ] ->
            Error.error "ocaml-array-sort! expects a comparator and compatible array"
        | Ok _ -> Error.error "ocaml-array-sort! expects 2 arguments")
    | "ocaml-array?" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ] ->
            Ok
              (typed_ir TBool
                 (Semantic_ir.Sequence
                    [ value.semantic_expr;
                      Semantic_ir.Bool
                        (match value.ty with TArray _ -> true | _ -> false);
                    ]))
        | Ok _ -> Error.error "ocaml-array? expects 1 argument")
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
    | ("tuple" | "ocaml-tuple") as tuple_name -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok ([] | [ _ ]) ->
            Error.error (tuple_name ^ " expects at least 2 values")
        | Ok values ->
            Ok
              (typed_ir
                 (TTuple (List.map (fun value -> value.ty) values))
                 (Semantic_ir.Tuple (List.map (fun value -> value.semantic_expr) values))))
    | "record" | "ocaml-record" -> (
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
        | Ok args ->
            if Result.is_ok (Core_int.expect_int_args name args) then
              Core_int.compile_operator name args
            else if Core_float.expect_float_args args then
              Core_float.compile_operator name args
            else if
              List.for_all
                (fun arg ->
                  Core_int.accepts_int arg.ty || Core_float.accepts_float arg.ty)
                args
            then Error.error "numeric arguments must all have the same type"
            else
              match Core_int.expect_int_args name args with
              | Error _ as err -> err
              | Ok () -> assert false)
    | "inc" ->
        compile_int_unary_call scope env name
          (fun expression -> Semantic_ir.Infix ("+", expression, Semantic_ir.Int 1))
          arg_forms
    | "dec" ->
        compile_int_unary_call scope env name
          (fun expression -> Semantic_ir.Infix ("-", expression, Semantic_ir.Int 1))
          arg_forms
    | "rand-int" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ { ty = TInt; semantic_expr; _ } ] ->
            Ok
              (typed_ir TInt
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_random.rand_int",
                      [ semantic_expr ] )))
        | Ok [ _ ] -> Error.error "rand-int expects an int"
        | Ok _ -> Error.error "rand-int expects 1 argument")
    | "int" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ ({ ty = TInt; _ } as value) ] -> Ok value
        | Ok [ { ty = TFloat; semantic_expr; _ } ] ->
            Ok
              (typed_ir TInt
                 (Semantic_ir.Apply
                    (Semantic_ir.Ident "int_of_float", [ semantic_expr ])))
        | Ok [ { ty = TOcaml "int64"; semantic_expr; _ } ] ->
            Ok
              (typed_ir TInt
                 (Semantic_ir.Apply
                    (Semantic_ir.Ident "Int64.to_int", [ semantic_expr ])))
        | Ok [ _ ] -> Error.error "int expects a numeric value"
        | Ok _ -> Error.error "int expects 1 argument")
    | "double" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ { ty = TInt; semantic_expr; _ } ] ->
            Ok
              (typed_ir TFloat
                 (Semantic_ir.Apply
                    (Semantic_ir.Ident "float_of_int", [ semantic_expr ])))
        | Ok [ ({ ty = TFloat; _ } as value) ] -> Ok value
        | Ok [ _ ] -> Error.error "double expects a numeric value"
        | Ok _ -> Error.error "double expects 1 argument")
    | "reify" -> (
        match arg_forms with
        | FSymbol protocol_name :: method_forms ->
            let compile_method = function
              | FList (FSymbol method_name :: params :: body_forms) -> (
                  match
                    Protocol.lookup_protocol_marker scope env protocol_name method_name
                  with
                  | None ->
                      Error.error
                        ("protocol " ^ protocol_name ^ " does not define method "
                       ^ method_name)
                  | Some marker -> (
                      match
                        ( Protocol.method_position env marker method_name,
                          compile_expr scope env
                            (FList
                               (FSymbol "fn"
                               :: (match params with
                                  | FVector (FSymbol "_" :: remaining) ->
                                      FVector remaining
                                  | _ -> params)
                               :: body_forms)) )
                      with
                      | None, _ -> Error.error ("unknown protocol method " ^ method_name)
                      | _, (Error _ as err) -> err
                      | Some position, Ok implementation ->
                          Ok (position, marker, implementation)))
              | _ -> Error.error "reify methods must be (method-name [params] body...)"
            in
            let rec compile_methods acc = function
              | [] -> Ok (List.sort (fun (left, _, _) (right, _, _) -> compare left right) acc)
              | method_form :: rest -> (
                  match compile_method method_form with
                  | Error _ as err -> err
                  | Ok method_impl -> compile_methods (method_impl :: acc) rest)
            in
            (match compile_methods [] method_forms with
            | Error _ as err -> err
            | Ok [] -> Error.error "reify expects at least one method"
            | Ok ((_, marker, _) :: _ as implementations) ->
                if List.length implementations <> Protocol.method_count env marker then
                  Error.error
                    ("reify must implement every method of protocol " ^ protocol_name)
                else
                  let methods =
                    List.map (fun (_, _, implementation) -> implementation) implementations
                  in
                  let payload_ty, payload_expr =
                    match methods with
                    | [ method_impl ] -> (method_impl.ty, method_impl.semantic_expr)
                    | _ ->
                        ( TTuple (List.map (fun method_impl -> method_impl.ty) methods),
                          Semantic_ir.Tuple
                            (List.map (fun method_impl -> method_impl.semantic_expr) methods) )
                  in
                  Ok
                    (typed_ir
                       (TOcaml_app ("Lg_runtime.Runtime_reify.t", [ payload_ty ]))
                       payload_expr))
        | _ -> Error.error "reify expects a protocol and method implementations")
    | "volatile!" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ initial ] ->
            Ok
              (typed_ir (TRef initial.ty)
                 (Semantic_ir.Apply
                    (Semantic_ir.Ident "ref", [ initial.semantic_expr ])))
        | Ok _ -> Error.error "volatile! expects 1 argument")
    | "deref" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ reference ] -> (
            match reference.ty with
            | TRef value_ty ->
                Ok
                  (typed_ir value_ty
                     (Semantic_ir.Prefix ("!", reference.semantic_expr)))
            | _ -> Error.error "deref expects a reference")
        | Ok _ -> Error.error "deref expects 1 argument")
    | "vswap!" -> (
        match arg_forms with
        | reference_form :: function_form :: extra_forms -> (
            match compile_expr scope env reference_form with
            | Error _ as err -> err
            | Ok reference -> (
                match reference.ty with
                | TRef value_ty ->
                    let value_name = "__lg_vswap_value" in
                    let updater_env =
                      Env.add (Names.scoped_key scope value_name)
                        (Types.binding value_name value_ty)
                        env
                    in
                    let updater_body =
                      FList
                        (function_form :: FSymbol value_name :: extra_forms)
                    in
                    (match compile_expr scope updater_env updater_body with
                    | Error _ as err -> err
                    | Ok updater_body ->
                        let updater =
                          typed_ir (TFn ([ value_ty ], updater_body.ty))
                            (Semantic_ir.Fun
                               ([ Semantic_ir.PVar value_name ],
                                updater_body.semantic_expr))
                        in
                        let updated_name = "__lg_vswap_updated" in
                        let updated_expr =
                          Semantic_ir.Apply
                            ( updater.semantic_expr,
                              [ Semantic_ir.Prefix
                                  ("!", reference.semantic_expr)
                              ] )
                        in
                        Ok
                          (typed_ir value_ty
                             (Semantic_ir.Let
                                ( [ (Semantic_ir.PVar updated_name, updated_expr) ],
                                  Semantic_ir.Sequence
                                    [ Semantic_ir.Infix
                                        ( ":=",
                                          reference.semantic_expr,
                                          Semantic_ir.Ident updated_name );
                                      Semantic_ir.Ident updated_name
                                    ] ))))
                | _ -> Error.error "vswap! expects a reference as its first argument"))
        | _ -> Error.error "vswap! expects a reference, function, and optional arguments")
    | "=" | "not=" | "<" | "<=" | ">" | ">=" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok args -> Core_compare.compile name args)
    | "not" | "nil?" | "some?" | "true?" | "false?" | "int?" | "number?"
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
    | ("zero?" | "pos?" | "neg?") as predicate -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ arg ] ->
            let operator =
              match predicate with
              | "zero?" -> "="
              | "pos?" -> ">"
              | "neg?" -> "<"
              | _ -> assert false
            in
            let zero =
              match arg.ty with
              | TInt | TUnknown -> Ok (Semantic_ir.Int 0)
              | TFloat -> Ok (Semantic_ir.Float "0.0")
              | _ -> Error.error ("expected int arguments for " ^ predicate)
            in
            Result.map
              (fun zero ->
                typed_ir TBool
                  (Semantic_ir.Infix (operator, arg.semantic_expr, zero)))
              zero
        | Ok _ -> Error.error (predicate ^ " expects 1 arguments"))
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
        | Ok (first :: _ as args)
          when Types.equal first.ty TFloat
               && List.for_all (fun arg -> Types.equal arg.ty TFloat) args ->
            Core_float.compile_min_max name args
        | Ok args
          when List.exists (fun arg -> Types.equal arg.ty TFloat) args
               && List.for_all
                    (fun arg -> Types.is_numeric arg.ty)
                    args ->
            Error.error (name ^ " numeric arguments must all have the same type")
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
    | "find" -> compile_find scope env arg_forms
    | "assoc" | "-assoc" -> compile_assoc scope env arg_forms
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
        | Ok args -> Core_sequence.compile env name args)
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
            let constructor_name =
              if String.contains name '/' then
                resolve_ocaml_constructor_target scope env name
              else ocaml_name
            in
            constructor ~constructor_name
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
                let arguments =
                  match (arguments, signature.parameters) with
                  | [],
                    [
                      {
                        Ocaml_signature.label = Ocaml_signature.Positional;
                        ty = TUnit;
                      };
                    ] ->
                      [ (None, typed_ir TUnit Semantic_ir.Unit) ]
                  | _ -> arguments
                in
                let argument_types =
                  List.map (fun (label, argument) -> (label, argument.ty)) arguments
                in
                match
                  Ocaml_signature.result_after_application signature argument_types
                with
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
    | Ok args -> Core_collection.compile env name args

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

  and overloaded_projection expression index =
    let rec descend expression remaining =
      if remaining = 0 then
        Semantic_ir.Apply (Semantic_ir.Ident "fst", [ expression ])
      else
        descend
          (Semantic_ir.Apply (Semantic_ir.Ident "snd", [ expression ]))
          (remaining - 1)
    in
    descend expression index

  and select_overloaded_arity arities argument_count =
    let indexed = List.mapi (fun index arity -> (index, arity)) arities in
    match
      List.find_opt
        (fun (_, (arity : fn_arity)) ->
          Option.is_none arity.rest_param
          && List.length arity.fixed_params = argument_count)
        indexed
    with
    | Some selected -> Some selected
    | None ->
        List.find_opt
          (fun (_, (arity : fn_arity)) ->
            Option.is_some arity.rest_param
            && argument_count >= List.length arity.fixed_params)
          indexed

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
            | (TUnknown | TVar _)
              when (match arg_forms with
                   | [ _key; FSymbol "nil" ] -> true
                   | _ -> false) -> (
                match args with
                | [ key; _default ] ->
                    Ok
                      (typed_ir (TNullable TUnknown)
                         (Semantic_ir.Apply
                            ( Semantic_ir.Ident
                                "Lg_runtime.Runtime_map.get_option",
                              [ Semantic_ir.Ident fn.ocaml_name;
                                key.semantic_expr;
                              ] )))
                | _ -> assert false)
            | TOcaml "__declared_fn" | TUnknown | TVar _ ->
                Ok
                  (typed_ir TUnknown
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident fn.ocaml_name,
                          List.map (fun arg -> arg.semantic_expr) args )))
            | TOverloaded_fn arities -> (
                match select_overloaded_arity arities (List.length args) with
                | None ->
                    Error.error
                      (name ^ " called with unsupported arity "
                     ^ string_of_int (List.length args))
                | Some (arity_index, arity) ->
                    let fixed_count = List.length arity.fixed_params in
                    let rec split_at count acc values =
                      if count = 0 then (List.rev acc, values)
                      else
                        match values with
                        | [] -> (List.rev acc, [])
                        | value :: rest ->
                            split_at (count - 1) (value :: acc) rest
                    in
                    let fixed_args, extra_args = split_at fixed_count [] args
                    in
                    let fixed_compatible =
                      List.for_all2
                        (fun expected arg ->
                          Types.assignable ~policy:Host_boundary ~expected
                            ~actual:arg.ty)
                        arity.fixed_params fixed_args
                    in
                    let rest_compatible =
                      match arity.rest_param with
                      | None -> extra_args = []
                      | Some expected ->
                          List.for_all
                            (fun arg ->
                              Types.assignable ~policy:Host_boundary ~expected
                                ~actual:arg.ty)
                            extra_args
                    in
                    if not (fixed_compatible && rest_compatible) then
                      Error.error (name ^ " called with incompatible arguments")
                    else
                      let arguments =
                        List.map (fun arg -> arg.semantic_expr) fixed_args
                        @
                        match arity.rest_param with
                        | None -> []
                        | Some _ ->
                            [ Semantic_ir.Apply
                                ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.of_list",
                                  [ Semantic_ir.List
                                      (List.map
                                         (fun arg -> arg.semantic_expr)
                                         extra_args) ] ) ]
                      in
                      let target =
                        match List.nth_opt fn.overload_targets arity_index with
                        | Some target -> Semantic_ir.Ident target
                        | None ->
                            overloaded_projection
                              (Semantic_ir.Ident fn.ocaml_name)
                              arity_index
                      in
                      Ok
                        (typed_ir arity.return_ty
                           (Semantic_ir.Apply (target, arguments))))
            | TFn (param_tys, ret)
              when List.length param_tys = List.length args
                   && List.for_all2
                        (fun expected arg ->
                          match Types.seqable_constraint_element expected with
                          | Some _ -> Collection_capability.accepts_seqable env arg.ty
                          | None ->
                              Types.assignable ~policy:Host_boundary ~expected
                                ~actual:arg.ty)
                        param_tys args ->
                let rec compile_arg_exprs index acc = function
                  | [] -> Ok (List.rev acc)
                  | arg :: rest ->
                      let expected_ty = List.nth param_tys index in
                      match Types.seqable_constraint_element expected_ty with
                      | Some _ -> (
                          match Collection_capability.pack_seqable_argument env arg with
                          | Error _ as err -> err
                          | Ok expression ->
                              compile_arg_exprs (index + 1) (expression :: acc) rest)
                      | None ->
                          let row_type_name =
                            List.nth_opt fn.row_param_types index |> Option.join
                          in
                          let expression =
                            match (expected_ty, arg.record_values) with
                            | TMap_keys, Some values ->
                                Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Core_set.String_set.of_list",
                                    [ Semantic_ir.List
                                        (List.map
                                           (fun ((field : field), _) ->
                                             Semantic_ir.String field.keyword)
                                           values) ] )
                            | _ -> row_arg_expr row_type_name expected_ty arg
                          in
                          compile_arg_exprs (index + 1) (expression :: acc) rest
                in
                (match compile_arg_exprs 0 [] args with
                | Error _ as err -> err
                | Ok arg_exprs ->
                let ret =
                  match (fn.return_param_index, ret) with
                  | Some index, TUnknown -> (
                      match List.nth_opt args index with
                      | Some arg -> arg.ty
                      | None -> ret)
                  | _ -> ret
                in
                let ret =
                  match ret with
                  | TSeq TUnknown ->
                      param_tys
                      |> List.mapi (fun index param_ty -> (index, param_ty))
                      |> List.find_map (fun (index, param_ty) ->
                             match Types.seqable_constraint_element param_ty with
                             | None -> None
                             | Some _ -> (
                                 match List.nth_opt args index with
                                 | None -> None
                                 | Some arg ->
                                     Collection_capability.element_type env arg))
                      |> Option.map (fun element_ty -> TSeq element_ty)
                      |> Option.value ~default:ret
                  | _ -> ret
                in
                Ok
                  (typed_ir ret
                     (Semantic_ir.Apply
                        (Semantic_ir.Ident fn.ocaml_name, arg_exprs))))
            | TSet element_ty -> (
                match args with
                | [ arg ] when Types.same_shape element_ty arg.ty ->
                    Result.map
                      (fun set_module ->
                        let present =
                          Semantic_ir.Apply
                            ( Semantic_ir.Ident (set_module ^ ".mem"),
                              [ arg.semantic_expr; Semantic_ir.Ident fn.ocaml_name ] )
                        in
                        typed_ir (TOcaml_app ("option", [ element_ty ]))
                          (Semantic_ir.If
                             ( present,
                               Semantic_ir.Constructor
                                 ("Some", Some arg.semantic_expr),
                               Semantic_ir.Constructor ("None", None) )))
                      (Types.set_module_name element_ty)
                | [ _ ] ->
                    Error.error (name ^ " called with incompatible arguments")
                | _ -> Error.error (name ^ " expects 1 arguments"))
            | TFn _ -> Error.error (name ^ " called with incompatible arguments")
            | _ -> Error.error (name ^ " is not callable")))

  and compile_protocol_call scope env name arg_forms =
    if Protocol.method_is_ambiguous scope env name then
      Error.error
        ("ambiguous protocol method " ^ name ^ "; use Protocol/method")
    else
      match Protocol.lookup_marker scope env name with
      | None -> Error.error ("unknown function " ^ name)
      | Some marker -> (
          match compile_args_for scope env arg_forms with
          | Error _ as err -> err
          | Ok args -> (
              match marker.ty with
              | TFn (param_tys, _ret)
                when List.length param_tys <> List.length args ->
                  Error.error (name ^ " called with incompatible arguments")
              | TFn (_, _) -> (
                  match args with
                  | [] -> Error.error (name ^ " called with incompatible arguments")
                  | receiver :: _ ->
                      let method_name = Protocol.method_basename name in
                      (match receiver.ty with
                      | TOcaml_app ("Lg_runtime.Runtime_reify.t", [ payload_ty ]) -> (
                          match Protocol.method_position env marker method_name with
                          | None -> Error.error ("unknown protocol method " ^ method_name)
                          | Some position ->
                              let method_expr =
                                match payload_ty with
                                | TTuple method_tys ->
                                    let binding_name = "__lg_reify_method" in
                                    let patterns =
                                      List.mapi
                                        (fun index _ ->
                                          if index = position then
                                            Semantic_ir.PVar binding_name
                                          else Semantic_ir.PAny)
                                        method_tys
                                    in
                                    Semantic_ir.Match
                                      ( receiver.semantic_expr,
                                        [ ( Semantic_ir.PTuple patterns,
                                            Semantic_ir.Ident binding_name ) ] )
                                | _ -> receiver.semantic_expr
                              in
                              let method_ty =
                                match payload_ty with
                                | TTuple method_tys ->
                                    List.nth_opt method_tys position
                                    |> Option.value ~default:TUnknown
                                | method_ty -> method_ty
                              in
                              let call_args =
                                match method_ty with
                                | TFn (params, _)
                                  when List.length params = List.length args - 1 ->
                                    List.tl args
                                | _ -> args
                              in
                              let return_ty =
                                match marker.ty with
                                | TFn (_, return_ty) -> return_ty
                                | _ -> TUnknown
                              in
                              Ok
                                (typed_ir return_ty
                                   (Semantic_ir.Apply
                                      ( method_expr,
                                        List.map (fun arg -> arg.semantic_expr) call_args ))))
                      | _ -> (
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
                                            Types.assignable ~policy:Host_boundary
                                              ~expected ~actual:arg.ty)
                                          param_tys args ->
                                  Ok
                                    (typed_ir ret
                                       (Semantic_ir.Apply
                                          ( Semantic_ir.Ident impl.ocaml_name,
                                            List.map
                                              (fun arg -> arg.semantic_expr)
                                              args )))
                              | TFn _ ->
                                  Error.error
                                    (name ^ " called with incompatible arguments")
                              | _ -> Error.error (name ^ " is not callable")))))
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
