open Ast
open Types
open Expression_support
module Env = Compiler_environment

type expression_result = (typed_expr, Error.t) result

type t = {
  compile_call :
    string -> Env.t -> string -> Ast.form list -> expression_result;
  compile_args_for :
    string -> Env.t -> Ast.form list -> (typed_expr list, Error.t) result;
}

let constrained_argument_counter = ref 0
let function_adapter_counter = ref 0
let row_argument_counter = ref 0
let array_literal_counter = ref 0

let array_element_needs_binding expression =
  match Semantic_ir.unlocated expression with
  | Int _ | Int64 _ | Float _ | String _ | Char _ | Bool _ | Unit | Ident _ ->
      false
  | _ -> true

let ordered_array_expression expressions =
  incr array_literal_counter;
  let array_index = string_of_int !array_literal_counter in
  let bindings, elements =
    expressions
    |> List.mapi (fun element_index expression ->
           if array_element_needs_binding expression then
             let name =
               "__lg_array_element_" ^ array_index ^ "_"
               ^ string_of_int element_index
             in
             (Some (Semantic_ir.PVar name, expression), Semantic_ir.Ident name)
           else (None, expression))
    |> List.split
  in
  match List.filter_map Fun.id bindings with
  | [] -> Semantic_ir.Array elements
  | bindings -> Semantic_ir.Let (bindings, Semantic_ir.Array elements)

let select_binding_arity (binding : binding) argument_count =
  match binding.ty with
  | TOverloaded_fn arities ->
      arities
      |> List.mapi (fun index arity -> (index, arity))
      |> List.find_opt (fun (_, arity) ->
             Option.is_none arity.rest_param
             && List.length arity.fixed_params = argument_count)
      |> Option.map (fun (index, arity) ->
             {
               binding with
               ocaml_name =
                 List.nth_opt binding.overload_targets index
                 |> Option.value ~default:binding.ocaml_name;
               ty = TFn (arity.fixed_params, arity.return_ty);
             })
  | _ -> Some binding

let is_identity_conversion name expression =
  match Semantic_ir.unlocated expression with
  | Semantic_ir.Ident candidate -> String.equal candidate name
  | _ -> false

let is_var_quote_marker name =
  String.equal name "__lg-var-quote"
  || String.ends_with ~suffix:"/__lg-var-quote" name

let is_java_namespace name =
  String.starts_with ~prefix:"java." name
  || String.starts_with ~prefix:"javax." name
  || String.starts_with ~prefix:"clojure.lang." name

let is_java_type_name name =
  is_java_namespace name
  || List.mem name
       [ "ClassCastException"; "Comparable"; "Iterable"; "Number"; "Object" ]

let java_interop_error name =
  Error.error
    ("Java interop is not supported; use static LG types and functions ("
   ^ name ^ ")")

let unreachable_narrowed_value ty kind =
  typed_ir ty
    (Semantic_ir.Apply
       ( Semantic_ir.Ident "invalid_arg",
         [ Semantic_ir.String ("unreachable " ^ kind ^ " branch") ] ))

let array_element_type = function
  | TArray element_ty -> Some element_ty
  | TOcaml "array" -> Some TUnknown
  | TOcaml_app ("array", [ element_ty ]) ->
      Some (lg_metadata_type_for_ocaml_type element_ty)
  | TUnknown | TMeta _ | TVar _ -> Some TUnknown
  | _ -> None

let typed_item_pattern name = function
  | TNamed_record record ->
      Semantic_ir.PConstraint
        ( Semantic_ir.PVar name,
          record_type_application record.type_name record.type_arguments )
  | _ -> Semantic_ir.PVar name

let typed_dynamic_item_pattern env name = function
  | TRecord fields -> (
      match
        Env.find_anonymous_record
          ~owner:(Source_context.anonymous_record_owner "") fields env
      with
      | Some record ->
          Semantic_ir.PConstraint
            ( Semantic_ir.PVar name,
              Structural_map.record_type_application record )
      | None -> Semantic_ir.PTyped (Semantic_ir.PVar name, TRecord fields))
  | ty -> typed_item_pattern name ty

let weak_referenceable_type = function
  | TRecord _ | TNamed_record _ | TArray _ | TRef _ | TVector _ | TSet _
  | TFn _ | TUnknown | TMeta _ | TVar _ ->
      true
  | TOcaml_app
      (name, _)
    when name <> "option" && name <> "list" && name <> "Seq.t"
         && name <> "Seq" ->
      true
  | ty when Types.is_dynamic ty -> true
  | _ -> false

let callback_parameters_compatible expected actual =
  List.length expected = List.length actual
  && List.for_all2
       (fun expected actual ->
         Types.assignable ~policy:Host_boundary ~expected ~actual
         || Result.is_ok
              (Type_solver.unify Type_solver.empty expected actual))
       expected actual

let unresolved_record_placeholder = function
  | TOcaml name -> String.starts_with ~prefix:"__lg_record:" name
  | _ -> false

let callback_record_compatible env expected actual =
  let expected = Collection_capability.resolve_callback_record env expected in
  (unresolved_record_placeholder expected
  && Option.is_some (Types.record_fields actual))
  ||
  (Option.is_some (Types.record_fields expected)
  && Option.is_some (Types.record_fields actual)
  && Types.row_compatible ~expected ~actual)

let expects_dynamic_value = Types.is_dynamic

let expects_optional_dynamic_value = function
  | TNullable inner | TOcaml_app ("option", [ inner ]) -> Types.is_dynamic inner
  | _ -> false

let contains_dynamic_type = Types.contains_dynamic

let runtime_map_lookup_operation target_ty actual_key_ty operation =
  let declared_key_ty =
    match Types.dynamic_map_types target_ty with
    | Some (key_ty, _) -> key_ty
    | None -> actual_key_ty
  in
  let dynamic_key =
    expects_dynamic_value declared_key_ty
    || expects_dynamic_value actual_key_ty
  in
  "Lg_runtime.Runtime_map." ^ operation
  ^ if dynamic_key then "_dynamic" else ""

let rec materialize_protocol_unknown = function
  | TUnknown | TMeta _ | TVar _ -> Types.dynamic_constraint TUnknown
  | TNullable ty -> TNullable (materialize_protocol_unknown ty)
  | TFn (parameters, return_ty) ->
      TFn
        ( List.map materialize_protocol_unknown parameters,
          materialize_protocol_unknown return_ty )
  | ty -> ty

let rec concrete_nominal_type_argument = function
  | TUnknown | TMeta _ | TVar _ -> false
  | ty when Types.is_dynamic ty -> false
  | TNullable ty | TArray ty | TRef ty | TList ty | TVector ty | TSet ty
  | TSeq ty ->
      concrete_nominal_type_argument ty
  | TOcaml_app (_, arguments) | TTuple arguments ->
      List.for_all concrete_nominal_type_argument arguments
  | TFn (parameters, return_ty) ->
      List.for_all concrete_nominal_type_argument (return_ty :: parameters)
  | TOverloaded_fn arities ->
      List.for_all
        (fun (arity : fn_arity) ->
          List.for_all concrete_nominal_type_argument
            (arity.return_ty :: arity.fixed_params)
          && Option.fold ~none:true ~some:concrete_nominal_type_argument
               arity.rest_param)
        arities
  | TNamed_record record ->
      List.for_all concrete_nominal_type_argument record.type_arguments
  | TRecord _ -> false
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TOcaml _ ->
      true

let concrete_nominal_record (record : named_record) =
  List.for_all concrete_nominal_type_argument record.type_arguments

let specialize_dynamic_nominal_unpack expected expression =
  match (expected, expression) with
  | ( TNamed_record expected_record,
      Semantic_ir.UnpackDynamic
        ({ target_ty = TNamed_record actual_record; conversion; _ } as unpack) )
    when concrete_nominal_record expected_record
         && Type_id.equal expected_record.type_id actual_record.type_id ->
      Some
        (Semantic_ir.UnpackDynamic
           {
             unpack with
             target_ty = expected;
             conversion;
           })
  | _ -> None

let rec contains_unresolved_type = function
  | ty when Types.is_dynamic ty -> false
  | TUnknown | TMeta _ | TVar _ -> true
  | TNullable ty | TArray ty | TRef ty | TList ty | TVector ty | TSet ty
  | TSeq ty ->
      contains_unresolved_type ty
  | TOcaml_app (_, arguments) | TTuple arguments ->
      List.exists contains_unresolved_type arguments
  | TFn (parameters, return_ty) ->
      List.exists contains_unresolved_type (return_ty :: parameters)
  | TOverloaded_fn arities ->
      List.exists
        (fun (arity : fn_arity) ->
          List.exists contains_unresolved_type
            (arity.return_ty :: arity.fixed_params)
          || Option.fold ~none:false ~some:contains_unresolved_type
               arity.rest_param)
        arities
  | TNamed_record _ -> false
  | TRecord fields ->
      List.exists
        (fun (field : field) -> contains_unresolved_type field.ty)
        fields
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TOcaml _ ->
      false

let maybe_reduced_callback_payload expected actual =
  match (expected, actual) with
  | TFn (expected_params, expected_return), TFn (actual_params, actual_return)
    when callback_parameters_compatible expected_params actual_params -> (
      match
        ( Types.maybe_reduced_callback_element expected_return,
          Types.reduced_element actual_return )
      with
      | Some expected_inner, Some actual_inner
        when Types.assignable ~policy:Host_boundary ~expected:expected_inner
               ~actual:actual_inner ->
          Some actual_inner
      | _ -> None)
  | _ -> None

let is_optional_type = function
  | TNullable _ | TOcaml_app ("option", [ _ ]) -> true
  | _ -> false

let optional_payload = function
  | TNullable ty | TOcaml_app ("option", [ ty ]) -> Some ty
  | _ -> None

let align_optional_inference template actual =
  match (optional_payload template, optional_payload actual) with
  | Some template, Some actual -> (template, actual)
  | Some template, None when not (Types.equal actual TNil) ->
      (template, actual)
  | None, Some actual -> (template, actual)
  | Some _, None | None, None -> (template, actual)

let same_set_storage_representation left right =
  Types.set_module_name left = Types.set_module_name right

let same_static_set_representation left right =
  match (Types.set_module_name left, Types.set_module_name right) with
  | Ok left_module, Ok right_module ->
      left_module <> "Lg_runtime.Runtime_poly_set"
      && String.equal left_module right_module
  | Error _, _ | _, Error _ -> false

let overloaded_arity_parameters (arity : fn_arity) argument_count =
  let fixed_count = List.length arity.fixed_params in
  if argument_count < fixed_count then None
  else
    match arity.rest_param with
    | None ->
        if argument_count = fixed_count then Some arity.fixed_params else None
    | Some rest_ty ->
        Some
          (arity.fixed_params
      @ List.init (argument_count - fixed_count) (fun _ -> rest_ty))

let is_edn_value_type = Edn_value_elaborator.is_value_type

let rec argument_compatible expected actual =
  if Types.is_dynamic expected then true
  else if is_edn_value_type expected then is_edn_value_type actual
  else if Option.is_some (Types.protocol_constraint_info expected) then true
  else if Option.is_some (Types.truthy_constraint_info expected) then true
  else if Option.is_some (Types.nil_predicate_constraint_info expected) then
    true
  else if Option.is_some (Types.printable_constraint_info expected) then true
  else if Option.is_some (Types.hashable_constraint_info expected) then true
  else if Option.is_some (Types.comparable_constraint_info expected) then true
  else if Option.is_some (Types.array_index_constraint_info expected) then true
  else if Option.is_some (Types.symbol_predicate_constraint_info expected) then
    true
  else if Option.is_some (Types.contains_constraint_info expected) then
    Collection_capability.accepts_contains actual
  else if Option.is_some (Types.seqable_constraint_info expected) then
    match (Types.seqable_constraint_info expected, actual) with
    | Some ((`Optional | `Optional_sequential), _, _), TNil -> true
    | _, (TNullable actual | TOcaml_app ("option", [ actual ])) ->
        argument_compatible expected actual
    | _, (TList _ | TVector _ | TSet _ | TSeq _ | TArray _ | TString) ->
        true
    | _, actual when is_edn_value_type actual -> true
    | _, ty when Types.is_dynamic ty -> true
    | _, _ -> Option.is_some (Types.seqable_constraint_info actual)
  else
    match (expected, actual) with
    | (TNullable _ | TOcaml_app ("option", [ _ ])), TNil -> true
    | ( (TNullable expected | TOcaml_app ("option", [ expected ])),
        (TNullable actual | TOcaml_app ("option", [ actual ])) ) ->
        argument_compatible expected actual
    | (TNullable expected | TOcaml_app ("option", [ expected ])), actual ->
        argument_compatible expected actual
    | TOcaml "int", TInt | TInt, TOcaml "int" -> true
    | TFn ([ TUnit ], expected_return), TFn ([], actual_return)
    | TFn ([], expected_return), TFn ([ TUnit ], actual_return) ->
        argument_compatible expected_return actual_return
    | expected, (TNullable actual | TOcaml_app ("option", [ actual ]))
      when (match expected with
           | TRecord _ | TNamed_record _ | TArray _
           | TOcaml_app ("array", [ _ ]) ->
               true
           | _ -> false)
           && argument_compatible expected actual ->
        true
    | TNamed_record _, TRecord _
      when Types.assignable ~policy:Host_boundary ~expected ~actual ->
        true
    | TNamed_record expected_record, TNamed_record actual_record
      when expected_record.type_name = actual_record.type_name ->
        Types.assignable ~policy:Host_boundary ~expected ~actual
    | ( (TRecord expected_fields | TNamed_record { fields = expected_fields; _ }),
        (TRecord actual_fields | TNamed_record { fields = actual_fields; _ }) )
      ->
        List.for_all
          (fun (expected : field) ->
            match find_field expected.keyword actual_fields with
            | Some actual -> argument_compatible expected.ty actual.ty
            | None ->
                Types.is_record_extension_field expected
                || is_optional_type expected.ty)
          expected_fields
    | expected_map,
      (TRecord actual_fields | TNamed_record { fields = actual_fields; _ })
      when Option.is_some (Types.dynamic_map_types expected_map)
           && Types.is_homogeneous_record actual_fields ->
        let expected_key, expected_value =
          Option.get (Types.dynamic_map_types expected_map)
        in
        let actual_value =
          Types.homogeneous_record_value_type actual_fields |> Option.get
        in
        argument_compatible expected_key TKeyword
        && argument_compatible expected_value actual_value
    | expected_map, actual
      when Option.is_some (Types.dynamic_map_types expected_map)
           && not (Types.equal actual (Types.constraint_value_type actual)) ->
        argument_compatible expected_map (Types.constraint_value_type actual)
    | expected_map, actual_map
      when Option.is_some (Types.dynamic_map_types expected_map) -> (
        match actual_map with
        | TUnknown | TMeta _ | TVar _ -> true
        | actual_map -> (
            match Types.dynamic_map_types actual_map with
            | Some (actual_key, actual_value) ->
                let expected_key, expected_value =
                  Option.get (Types.dynamic_map_types expected_map)
                in
                argument_compatible expected_key actual_key
                && argument_compatible expected_value actual_value
            | None -> false))
    | _ when Types.assignable ~policy:Host_boundary ~expected ~actual -> true
    | TFn (expected_params, expected_return), TFn (actual_params, actual_return)
      when callback_parameters_compatible expected_params actual_params -> (
        if
          Types.equal expected_return TBool
          && expects_dynamic_value actual_return
        then true
        else if argument_compatible expected_return actual_return then true
        else
        match Types.maybe_reduced_callback_element expected_return with
        | Some expected_inner -> (
            match Types.reduced_element actual_return with
            | Some actual_inner ->
                  Types.assignable ~policy:Host_boundary
                    ~expected:expected_inner ~actual:actual_inner
            | None ->
                  Types.assignable ~policy:Host_boundary
                    ~expected:expected_inner ~actual:actual_return)
        | None -> false)
    | TFn (expected_params, _) as expected_fn, TOverloaded_fn arities ->
        List.exists
          (fun arity ->
            match
              overloaded_arity_parameters arity (List.length expected_params)
            with
            | None -> false
            | Some actual_params ->
                argument_compatible expected_fn
                  (TFn (actual_params, arity.return_ty)))
          arities
    | TOverloaded_fn expected_arities, TOverloaded_fn actual_arities ->
        let function_type (arity : fn_arity) =
          let parameters =
            match arity.rest_param with
            | None -> arity.fixed_params
            | Some rest_ty -> arity.fixed_params @ [ TSeq rest_ty ]
          in
          TFn (parameters, arity.return_ty)
        in
        List.for_all
          (fun (expected : fn_arity) ->
            actual_arities
            |> List.find_opt (fun (actual : fn_arity) ->
                   List.length actual.fixed_params
                   = List.length expected.fixed_params
                   && Option.is_some actual.rest_param
                      = Option.is_some expected.rest_param)
            |> Option.fold ~none:false ~some:(fun actual ->
                   argument_compatible (function_type expected)
                     (function_type actual)))
          expected_arities
    | _ -> false

let named_argument_compatible expected actual =
  argument_compatible expected actual
  ||
  ((match expected with
   | TInt | TFloat | TChar | TString | TBool | TKeyword | TSymbol -> true
   | _ -> false)
  && Option.is_none (optional_payload expected)
  && Option.fold ~none:false ~some:(argument_compatible expected)
       (optional_payload actual))

let rec witness_storage = function
  | [] -> Semantic_ir.Unit
  | method_expr :: rest ->
      Semantic_ir.Tuple [ method_expr; witness_storage rest ]

let rec witness_method expression position =
  if position = 0 then
    Semantic_ir.Apply (Semantic_ir.Ident "fst", [ expression ])
  else
    witness_method
      (Semantic_ir.Apply (Semantic_ir.Ident "snd", [ expression ]))
      (position - 1)

let is_generated_callback_argument name =
  String.starts_with ~prefix:"__lg_erased_callback_arg_" name
  || String.starts_with ~prefix:"__lg_callback_argument_" name
  || String.starts_with ~prefix:"__lg_nullable_callback_arg_" name
  || String.starts_with ~prefix:"__lg_static_argument_" name
  || String.starts_with ~prefix:"__lg_erased_protocol_arg_" name
  || String.starts_with ~prefix:"__lg_erased_optional_value" name
  || String.starts_with ~prefix:"__lg_branch_optional_item" name
  || String.starts_with ~prefix:"__lg_branch_optional_payload" name
  || String.starts_with ~prefix:"__lg_erased_seqable_item" name
  || String.starts_with ~prefix:"__lg_constrained_argument" name
  || String.starts_with ~prefix:"__lg_adapt_collection_item" name
  || String.starts_with ~prefix:"__lg_protocol_witness_argument_" name

let protocol_witness_expression protocol_id receiver =
  match Semantic_ir.unlocated receiver.semantic_expr with
  | Semantic_ir.Ident name when is_generated_callback_argument name ->
      Some
        (Semantic_ir.Apply (Semantic_ir.Ident "fst", [ receiver.semantic_expr ]))
  | Semantic_ir.Ident name ->
      Some (Semantic_ir.Ident (Types.protocol_witness_name name protocol_id))
  | _ -> None

let rec has_protocol_constraint protocol_id ty =
  match Types.protocol_constraint_info ty with
  | Some (candidate, _, value_ty) ->
      Protocol_id.equal candidate protocol_id
      || has_protocol_constraint protocol_id value_ty
  | None -> false

let rec protocol_constraint_witness protocol_id ty =
  match Types.protocol_constraint_info ty with
  | Some (candidate, witness_ty, value_ty) ->
      if Protocol_id.equal candidate protocol_id then Some witness_ty
      else protocol_constraint_witness protocol_id value_ty
  | None -> None

let has_capability_constraint ty =
  Types.is_dynamic ty
  || Option.is_some (Types.protocol_constraint_info ty)
  || Option.is_some (Types.truthy_constraint_info ty)
  || Option.is_some (Types.nil_predicate_constraint_info ty)
  || Option.is_some (Types.printable_constraint_info ty)
  || Option.is_some (Types.hashable_constraint_info ty)
  || Option.is_some (Types.comparable_constraint_info ty)
  || Option.is_some (Types.array_index_constraint_info ty)
  || Option.is_some (Types.symbol_predicate_constraint_info ty)
  || Option.is_some (Types.contains_constraint_info ty)
  ||
  match ty with
  | TOcaml_app (name, [ _; _ ]) ->
      name = Types.seqable_constraint_name
      || name = Types.optional_seqable_constraint_name
      || name = Types.optional_sequential_constraint_name
  | _ -> false

let named_record_has_capability_fields (record : named_record) =
  List.exists
    (fun (field : field) -> has_capability_constraint field.ty)
    record.fields

let uses_dynamic_value_storage ty =
  let value_ty = Types.constraint_value_type ty in
  Types.is_dynamic value_ty
  ||
  match value_ty with
  | TUnknown | TMeta _ | TVar _ -> has_capability_constraint ty
  | _ -> false

let constrained_identifier_expression name ty =
  let rec build = function
    | ty when Types.is_dynamic ty -> Semantic_ir.Ident name
    | ty -> (
        match Types.protocol_constraint_info ty with
        | Some (protocol_id, _, value_ty) ->
            Semantic_ir.Tuple
              [
                Semantic_ir.Ident (Types.protocol_witness_name name protocol_id);
                build value_ty;
              ]
        | None -> (
            match Types.truthy_constraint_info ty with
            | Some value_ty ->
                Semantic_ir.Tuple
                  [
                    Semantic_ir.Ident (name ^ "__truthy");
                    build value_ty;
                  ]
            | None -> (
                match Types.nil_predicate_constraint_info ty with
                | Some value_ty ->
                    Semantic_ir.Tuple
                      [
                        Semantic_ir.Ident (name ^ "__nil");
                        build value_ty;
                      ]
                | None -> (
                match Types.printable_constraint_info ty with
                | Some value_ty ->
                    Semantic_ir.Tuple
                      [
                        Semantic_ir.Tuple
                          [ Semantic_ir.Ident (name ^ "__print");
                            Semantic_ir.Ident (name ^ "__pr");
                          ];
                        build value_ty;
                    ]
                | None -> (
                    match Types.hashable_constraint_info ty with
                    | Some value_ty ->
                        Semantic_ir.Tuple
                          [
                            Semantic_ir.Ident (name ^ "__hash");
                            build value_ty;
                          ]
                    | None -> (
                        match Types.comparable_constraint_info ty with
                        | Some value_ty ->
                            Semantic_ir.Tuple
                              [
                                Semantic_ir.Ident (name ^ "__compare");
                                build value_ty;
                              ]
                        | None -> (
                            match Types.array_index_constraint_info ty with
                            | Some value_ty ->
                                Semantic_ir.Tuple
                                  [
                                    Semantic_ir.Ident (name ^ "__index");
                                    build value_ty;
                                  ]
                            | None -> (
                    match Types.symbol_predicate_constraint_info ty with
                    | Some value_ty ->
                        Semantic_ir.Tuple
                          [
                            Semantic_ir.Ident (name ^ "__symbol");
                            build value_ty;
                          ]
                    | None -> (
            match Types.contains_constraint_info ty with
            | Some (_, value_ty) ->
                Semantic_ir.Tuple
                  [
                    Semantic_ir.Ident (name ^ "__contains");
                    build value_ty;
                  ]
            | None -> (
            match ty with
            | TOcaml_app (constraint_name, [ _element_ty; value_ty ])
              when constraint_name = Types.seqable_constraint_name
                   || constraint_name = Types.optional_seqable_constraint_name
                   || constraint_name
                      = Types.optional_sequential_constraint_name ->
                Semantic_ir.Tuple
                  [
                    Semantic_ir.Ident
                      (if constraint_name = Types.seqable_constraint_name then
                         name ^ "__seq"
                       else name ^ "__seq_optional");
                    build value_ty;
                  ]
            | _ -> Semantic_ir.Ident name))))))))))
  in
  build ty

let constrained_identifier_pattern name ty =
  let rec build = function
    | ty when Types.is_dynamic ty -> Semantic_ir.PVar name
    | ty -> (
        match Types.protocol_constraint_info ty with
        | Some (protocol_id, _, value_ty) ->
            Semantic_ir.PTuple
              [
                Semantic_ir.PVar
                  (Types.protocol_witness_name name protocol_id);
                build value_ty;
              ]
        | None -> (
            match Types.truthy_constraint_info ty with
            | Some value_ty ->
                Semantic_ir.PTuple
                  [
                    Semantic_ir.PVar (name ^ "__truthy");
                    build value_ty;
                  ]
            | None -> (
                match Types.nil_predicate_constraint_info ty with
                | Some value_ty ->
                    Semantic_ir.PTuple
                      [
                        Semantic_ir.PVar (name ^ "__nil");
                        build value_ty;
                      ]
                | None -> (
                match Types.printable_constraint_info ty with
                | Some value_ty ->
                    Semantic_ir.PTuple
                      [
                        Semantic_ir.PTuple
                          [ Semantic_ir.PVar (name ^ "__print");
                            Semantic_ir.PVar (name ^ "__pr");
                          ];
                        build value_ty;
                    ]
                | None -> (
                    match Types.hashable_constraint_info ty with
                    | Some value_ty ->
                        Semantic_ir.PTuple
                          [
                            Semantic_ir.PVar (name ^ "__hash");
                            build value_ty;
                          ]
                    | None -> (
                        match Types.comparable_constraint_info ty with
                        | Some value_ty ->
                            Semantic_ir.PTuple
                              [
                                Semantic_ir.PVar (name ^ "__compare");
                                build value_ty;
                              ]
                        | None -> (
                            match Types.array_index_constraint_info ty with
                            | Some value_ty ->
                                Semantic_ir.PTuple
                                  [
                                    Semantic_ir.PVar (name ^ "__index");
                                    build value_ty;
                                  ]
                            | None -> (
                    match Types.symbol_predicate_constraint_info ty with
                    | Some value_ty ->
                        Semantic_ir.PTuple
                          [
                            Semantic_ir.PVar (name ^ "__symbol");
                            build value_ty;
                          ]
                    | None -> (
            match Types.contains_constraint_info ty with
            | Some (_, value_ty) ->
                Semantic_ir.PTuple
                  [
                    Semantic_ir.PVar (name ^ "__contains");
                    build value_ty;
                  ]
            | None -> (
            match ty with
            | TOcaml_app (constraint_name, [ _element_ty; value_ty ])
              when constraint_name = Types.seqable_constraint_name
                   || constraint_name = Types.optional_seqable_constraint_name
                   || constraint_name
                      = Types.optional_sequential_constraint_name ->
                Semantic_ir.PTuple
                  [
                    Semantic_ir.PVar
                      (if constraint_name = Types.seqable_constraint_name then
                         name ^ "__seq"
                       else name ^ "__seq_optional");
                    build value_ty;
                  ]
            | _ -> Semantic_ir.PVar name))))))))))
  in
  build ty

let constrained_argument_expression argument =
  match Semantic_ir.unlocated argument.semantic_expr with
  | Semantic_ir.Ident name when is_generated_callback_argument name ->
      argument.semantic_expr
  | Semantic_ir.Ident name -> constrained_identifier_expression name argument.ty
  | _ -> argument.semantic_expr

let constrained_value_projection expression =
  match Semantic_ir.unlocated expression with
  | Semantic_ir.Tuple [ _witness; value ] -> value
  | _ -> Semantic_ir.Apply (Semantic_ir.Ident "snd", [ expression ])

let constrained_witness_projection expression =
  match Semantic_ir.unlocated expression with
  | Semantic_ir.Tuple [ witness; _value ] -> witness
  | _ -> Semantic_ir.Apply (Semantic_ir.Ident "fst", [ expression ])

let project_protocol_constraint protocol_id argument =
  let rec project ty expression =
    match Types.protocol_constraint_info ty with
    | Some (candidate_id, _, value_ty) ->
        let witness = constrained_witness_projection expression in
        let value = constrained_value_projection expression in
        if Protocol_id.equal candidate_id protocol_id then
          Some (witness, typed_ir value_ty value)
        else project value_ty value
    | None -> None
  in
  project argument.ty (constrained_argument_expression argument)

let rec constrained_value_expression ty expression =
  match Types.protocol_constraint_info ty with
  | Some (_, _, value_ty) ->
      constrained_value_expression value_ty
        (constrained_value_projection expression)
  | None -> (
      match Types.truthy_constraint_info ty with
      | Some value_ty ->
          constrained_value_expression value_ty
            (constrained_value_projection expression)
      | None -> (
          match Types.nil_predicate_constraint_info ty with
          | Some value_ty ->
              constrained_value_expression value_ty
                (constrained_value_projection expression)
          | None -> (
          match Types.printable_constraint_info ty with
          | Some value_ty ->
              constrained_value_expression value_ty
                (constrained_value_projection expression)
          | None -> (
              match Types.hashable_constraint_info ty with
              | Some value_ty ->
                  constrained_value_expression value_ty
                    (constrained_value_projection expression)
              | None -> (
                  match Types.comparable_constraint_info ty with
                  | Some value_ty ->
                      constrained_value_expression value_ty
                        (constrained_value_projection expression)
                  | None -> (
                      match Types.array_index_constraint_info ty with
                      | Some value_ty ->
                          constrained_value_expression value_ty
                            (constrained_value_projection expression)
                      | None -> (
              match Types.symbol_predicate_constraint_info ty with
              | Some value_ty ->
                  constrained_value_expression value_ty
                    (constrained_value_projection expression)
              | None -> (
      match Types.contains_constraint_info ty with
      | Some (_, value_ty) ->
          constrained_value_expression value_ty
            (constrained_value_projection expression)
      | None -> (
      match ty with
      | TOcaml_app (constraint_name, [ _element_ty; value_ty ])
        when constraint_name = Types.seqable_constraint_name
             || constraint_name = Types.optional_seqable_constraint_name
             || constraint_name = Types.optional_sequential_constraint_name ->
          constrained_value_expression value_ty
            (constrained_value_projection expression)
      | _ -> expression)))))))))

let constrained_argument_value argument =
  match Semantic_ir.unlocated argument.semantic_expr with
  | Semantic_ir.Ident name when is_generated_callback_argument name ->
      constrained_value_expression argument.ty argument.semantic_expr
  | Semantic_ir.Ident _ -> argument.semantic_expr
  | Semantic_ir.Sequence expressions -> (
      match List.rev expressions with
      | final_expression :: _ -> (
          match Semantic_ir.unlocated final_expression with
          | Semantic_ir.Ident _ -> argument.semantic_expr
          | _ -> constrained_value_expression argument.ty argument.semantic_expr)
      | [] -> constrained_value_expression argument.ty argument.semantic_expr)
  | _ -> constrained_value_expression argument.ty argument.semantic_expr

let is_sequential_type = function
  | TList _ | TVector _ | TSeq _ -> true
  | ty -> (
      match Types.seqable_constraint_info ty with
      | Some (`Optional_sequential, _, _) -> true
      | Some ((`Required | `Optional), _, _) | None ->
          Option.is_some (Types.next_seq_element ty))

let resolve_named_record_application env = function
  | TOcaml_app (name, arguments) as ty ->
      let records =
        Env.filter_record_bindings
          (fun key (binding : binding) ->
            if String.starts_with ~prefix:"__record/" key then
              match binding.ty with
              | TNamed_record record
                when (record.type_name = name
                     || Type_id.name record.type_id = name)
                     && List.length record.type_parameters
                        = List.length arguments ->
                  Some record
              | _ -> None
            else None)
          env
        |> List.fold_left
             (fun records record ->
               if
                 List.exists
                   (fun existing ->
                     Type_id.equal existing.type_id record.type_id)
                   records
               then records
               else record :: records)
             []
      in
      (match records with
      | [ record ] ->
          let substitutions =
            List.combine record.type_parameters arguments
            |> List.map (fun (parameter, argument) ->
                   (Type_solver.Declared parameter, argument))
          in
          Type_solver.apply (Type_solver.of_list substitutions)
            (TNamed_record record)
      | [] | _ :: _ :: _ -> ty)
  | ty -> ty

let compile_registered_hash env value =
  let value_ty =
    Types.constraint_value_type value.ty |> resolve_named_record_application env
  in
  match Core_protocols.find_hash value_ty (Env.protocols env) with
  | Some { ty = TFn ([ _ ], return_ty); ocaml_name; _ }
    when Types.equal return_ty TInt
         || match return_ty with
            | TUnknown | TMeta _ | TVar _ -> true
            | _ -> false ->
      Some
        (Semantic_ir.Apply
           (Semantic_ir.Ident ocaml_name, [ constrained_argument_value value ]))
  | Some _ -> None
  | None -> None

let rec compile_static_hash_capability env value =
  let apply name arguments =
    Semantic_ir.Apply (Semantic_ir.Ident name, arguments)
  in
  match Types.hashable_constraint_info value.ty with
  | Some _ ->
      let expression =
        match Semantic_ir.unlocated value.semantic_expr with
        | Semantic_ir.Ident name when not (is_generated_callback_argument name) ->
            apply (name ^ "__hash") [ value.semantic_expr ]
        | _ ->
            Semantic_ir.Apply
              ( apply "fst" [ value.semantic_expr ],
                [ apply "snd" [ value.semantic_expr ] ] )
      in
      Ok expression
  | None -> (
  match compile_registered_hash env value with
  | Some expression -> Ok expression
  | None -> (
      let value_ty = Types.constraint_value_type value.ty in
      let expression = constrained_argument_value value in
      let value = typed_ir value_ty expression in
      match value_ty with
      | TInt -> Ok (apply "Lg_runtime.Runtime_hash.hash_int" [ expression ])
      | TFloat -> Ok (apply "Lg_runtime.Runtime_hash.hash_float" [ expression ])
      | TChar -> Ok (apply "Char.code" [ expression ])
      | TString | TRegex ->
          Ok (apply "Lg_runtime.Runtime_hash.hash_string" [ expression ])
      | TSymbol ->
          Ok (apply "Lg_runtime.Runtime_hash.hash_symbol" [ expression ])
      | TKeyword ->
          Ok (apply "Lg_runtime.Runtime_hash.hash_keyword" [ expression ])
      | TBool ->
          Ok
            (Semantic_ir.If
               (expression, Semantic_ir.Int 1231, Semantic_ir.Int 1237))
      | TNil | TUnit ->
          Ok (Semantic_ir.Sequence [ expression; Semantic_ir.Int 0 ])
      | TNullable inner | TOcaml_app ("option", [ inner ]) ->
          let item_name = "__lg_constrained_argument_optional_hash_value" in
          let item = typed_ir inner (Semantic_ir.Ident item_name) in
          Result.map
            (fun item_hash ->
              Semantic_ir.Match
                ( expression,
                  [
                    ( Semantic_ir.PConstructor ("None", None),
                      Semantic_ir.Int 0 );
                    ( Semantic_ir.PConstructor
                        ("Some", Some (Semantic_ir.PVar item_name)),
                      item_hash );
                  ] ))
            (compile_static_hash_capability env item)
      | TTuple [ left_ty; right_ty ] ->
          let left = typed_ir left_ty (apply "fst" [ expression ]) in
          let right = typed_ir right_ty (apply "snd" [ expression ]) in
          Result.bind (compile_static_hash_capability env left) (fun left_hash ->
              Result.map
                (fun right_hash ->
                  apply "Lg_runtime.Runtime_hash.hash_ordered"
                    [
                      apply "List.to_seq"
                        [ Semantic_ir.List [ left_hash; right_hash ] ];
                    ])
                (compile_static_hash_capability env right))
      | TRecord fields | TNamed_record { nominal = false; fields; _ } ->
          let fields =
            List.filter
              (fun field -> not (Types.is_record_extension_field field))
              fields
          in
          let rec entry_hashes hashes = function
            | [] -> Ok (List.rev hashes)
            | (field : field) :: rest ->
                let field_value =
                  typed_ir field.ty (Structural_map.field_expr value field)
                in
                Result.bind
                  (compile_static_hash_capability env field_value)
                  (fun value_hash ->
                    let key_hash =
                      apply "Lg_runtime.Runtime_hash.hash_keyword"
                        [ Semantic_ir.String field.keyword ]
                    in
                    let entry_hash =
                      apply "Lg_runtime.Runtime_hash.hash_ordered"
                        [
                          apply "List.to_seq"
                            [ Semantic_ir.List [ key_hash; value_hash ] ];
                        ]
                    in
                    entry_hashes (entry_hash :: hashes) rest)
          in
          Result.map
            (fun hashes ->
              apply "Lg_runtime.Runtime_hash.hash_unordered"
                [ apply "List.to_seq" [ Semantic_ir.List hashes ] ])
            (entry_hashes [] fields)
      | TArray _ | TList _ | TVector _ | TSeq _ ->
          compile_static_collection_hash_capability env
            "Lg_runtime.Runtime_hash.hash_ordered" value
      | TSet _ | TOcaml_app ("Lg_runtime.Runtime_map.t", [ _; _ ]) ->
          compile_static_collection_hash_capability env
            "Lg_runtime.Runtime_hash.hash_unordered" value
      | _ ->
          Error.error
            ("hash requires a statically supported type, got "
           ^ Types.source_name value_ty)))

and compile_static_collection_hash_capability env hash_name value =
  Result.bind (Collection_capability.to_seq_expr env value)
    (fun (element_ty, sequence) ->
      match element_ty with
      | TUnknown | TMeta _ | TVar _ ->
          Error.error "hash requires a closed collection element type"
      | element_ty ->
          let item_name = "__lg_hash_item" in
          let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
          Result.map
            (fun item_hash ->
              Semantic_ir.Apply
                ( Semantic_ir.Ident hash_name,
                  [
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                        [
                          Semantic_ir.Fun
                            ([ Semantic_ir.PVar item_name ], item_hash);
                          sequence;
                        ] );
                  ] ))
            (compile_static_hash_capability env item))

let compile_static_compare_capability env left right =
  let value_ty = Types.constraint_value_type left.ty in
  if not (Types.equal value_ty (Types.constraint_value_type right.ty)) then
    Error.error
      ("compare arguments must have the same type: "
     ^ Types.source_name value_ty ^ " and "
     ^ Types.source_name (Types.constraint_value_type right.ty))
  else
    match Core_protocols.find_comparable value_ty (Env.protocols env) with
    | Some { ty = TFn ([ _; _ ], return_ty); ocaml_name; _ }
      when Types.equal return_ty TInt
           || Types.equal return_ty (TOcaml "int")
           || match return_ty with
              | TUnknown | TMeta _ | TVar _ -> true
              | _ -> false ->
        Ok
          (Semantic_ir.Apply
             ( Semantic_ir.Ident ocaml_name,
               [ constrained_argument_value left; constrained_argument_value right ] ))
    | Some _ -> Error.error "IComparable/-compare has an invalid signature"
    | None ->
        let rec comparable_type = function
          | TInt | TFloat | TString | TSymbol | TKeyword | TBool -> true
          | TNullable inner | TOcaml_app ("option", [ inner ]) ->
              comparable_type inner
          | _ -> false
        in
        if Types.equal value_ty TKeyword || Types.equal value_ty TSymbol then
          Ok
            (Semantic_ir.Apply
                ( Semantic_ir.Ident
                   "Lg_runtime.Runtime_keyword.compare_identifier",
                 [
                   constrained_argument_value left;
                   constrained_argument_value right;
                 ] ))
        else if comparable_type value_ty then
          Ok
            (Semantic_ir.Apply
               ( Semantic_ir.Ident "Stdlib.compare",
                 [ constrained_argument_value left; constrained_argument_value right ] ))
        else
          Error.error
            "compare expects one concrete comparable type; define a closed sum \
             type and match its cases explicitly for a heterogeneous domain"

let compile_static_array_index value =
  match Types.array_index_constraint_info value.ty with
  | Some _ -> (
      match Semantic_ir.unlocated value.semantic_expr with
      | Semantic_ir.Ident name ->
          Ok
            (Semantic_ir.Apply
               (Semantic_ir.Ident (name ^ "__index"), [ value.semantic_expr ]))
      | _ ->
          Ok
            (Semantic_ir.Apply
               ( Semantic_ir.Apply
                   (Semantic_ir.Ident "fst", [ value.semantic_expr ]),
                 [
                   Semantic_ir.Apply
                     (Semantic_ir.Ident "snd", [ value.semantic_expr ]);
                 ] )))
  | None ->
      let value_ty = Types.constraint_value_type value.ty in
      let expression = constrained_argument_value value in
      if Types.equal value_ty TInt then Ok expression
      else if Types.equal value_ty TFloat then
        Ok
          (Semantic_ir.Apply
             (Semantic_ir.Ident "int_of_float", [ expression ]))
      else
        Error.error
          ("array index requires int or float, got " ^ Types.source_name value_ty)

let dynamic_boundary_error_message direction ty =
  let cannot_cross subject guidance =
    subject ^ " cannot cross a dynamic boundary; " ^ guidance
  in
  match ty with
  | TUnknown | TMeta _ | TVar _ -> None
  | ty when Types.is_dynamic ty -> None
  | (TInt | TFloat | TChar | TString | TRegex | TSymbol | TKeyword | TBool)
  | TOcaml
      ( "int" | "int64" | "float" | "char" | "string" | "bool"
      | "Lg_runtime.Runtime_uuid.t" ) ->
      Some
        (cannot_cross "static values"
           "keep the value statically typed or define a sum type: use a closed \
            sum type containing every alternative")
  | TUnit -> Some (cannot_cross "static values" "keep unit statically typed")
  | TNil ->
      Some
        (cannot_cross "nil" "give it a concrete option payload type")
  | TNullable _ | TOcaml_app ("option", [ _ ]) ->
      Some
        (cannot_cross "optional values"
           "keep absence as a statically typed option")
  | TRef _ ->
      Some
        (cannot_cross "references" "keep mutable state statically typed")
  | TFn _ | TOverloaded_fn _ ->
      Some
        (cannot_cross "functions"
           "define a closed sum type containing the supported functions")
  | TOcaml_app ("Lg_runtime.Runtime_reify.t", [ _ ]) ->
      Some
        (cannot_cross "reified protocol implementations"
           "keep the protocol receiver statically typed")
  | ty when Option.is_some (Types.protocol_constraint_info ty) ->
      Some
        (cannot_cross "protocol-constrained values"
           "keep the protocol witness statically typed")
  | TOcaml_app (constraint_name, [ _; _ ])
    when constraint_name = Types.seqable_constraint_name
         || constraint_name = Types.optional_seqable_constraint_name
         || constraint_name = Types.optional_sequential_constraint_name ->
      Some
        (cannot_cross "seqable constraints"
           "keep the collection and its sequence adapter statically typed")
  | TTuple _ ->
      Some
        (cannot_cross "tuples"
           "define a closed sum type containing the supported tuple shapes")
  | TNamed_record _ ->
      Some
        (cannot_cross "named records"
           "define a closed sum type containing the supported records")
  | TRecord _ ->
      Some
        (cannot_cross "records"
           "define a closed sum type containing the supported records")
  | (TVector _ | TList _ | TSeq _ | TSet _ | TArray _)
  | TOcaml_app (("list" | "List.t" | "Seq.t" | "Seq" | "array"), [ _ ]) ->
      Some
        (cannot_cross "collections"
           "keep the collection statically typed and use a closed sum for \
            heterogeneous elements")
  | TOcaml_app (name, [ _ ]) when name = Types.next_seq_type_name ->
      Some
        (cannot_cross "collections"
           "keep the collection statically typed and use a closed sum for \
            heterogeneous elements")
  | map_ty when Option.is_some (Types.dynamic_map_types map_ty) ->
      Some
        (cannot_cross "collections"
           "keep the map statically typed and use a closed sum for \
            heterogeneous keys or values")
  | TOcaml _ ->
      Some
        (cannot_cross "host values"
           "keep the declared OCaml type or define a closed sum type")
  | _ ->
      let action =
        match direction with
        | `Pack -> "pass " ^ Types.source_name ty ^ " through"
        | `Unpack -> "recover " ^ Types.source_name ty ^ " from"
      in
      Some ("cannot " ^ action ^ " a dynamic function boundary")

let dynamic_unpack env ty expression =
  let ty = resolve_named_record_application env ty in
  match dynamic_boundary_error_message `Unpack ty with
  | Some message -> Error.error (message ^ "; got " ^ Types.source_name ty)
  | None ->
      Ok
        (Semantic_ir.UnpackDynamic
           {
             source_ty = Types.dynamic_constraint TUnknown;
             target_ty = ty;
             conversion = expression;
           })

let rec semantic_expression_location = function
  | Semantic_ir.Located (_, location, _) -> Some location
  | Semantic_ir.Typed (_, expression) -> semantic_expression_location expression
  | _ -> None

let pack_dynamic_value _env expected_dynamic argument =
  match dynamic_boundary_error_message `Pack argument.ty with
  | Some message ->
      Error.error
        ?location:(semantic_expression_location argument.semantic_expr)
        (message ^ "; got " ^ Types.source_name argument.ty)
  | None ->
      Ok
        (Semantic_ir.PackDynamic
           {
             source_ty = argument.ty;
             target_ty = expected_dynamic;
             conversion = argument.semantic_expr;
           })

let rec pack_metadata_expression ty expression =
  let convert name =
    Ok
      (Semantic_ir.Apply
         (Semantic_ir.Ident ("Lg_runtime.Runtime_metadata." ^ name), [ expression ]))
  in
  match Types.constraint_value_type ty with
  | TNil ->
      Ok
        (Semantic_ir.Sequence
           [ expression; Semantic_ir.Ident "Lg_runtime.Runtime_metadata.nil" ])
  | TBool -> convert "of_bool"
  | TInt | TOcaml "int" -> convert "of_int"
  | TFloat -> convert "of_float"
  | TChar -> convert "of_char"
  | TString -> convert "of_string"
  | TRegex -> convert "of_regex"
  | TSymbol -> convert "of_symbol"
  | TKeyword -> convert "of_keyword"
  | TOcaml "Lg_edn_backend.t" -> Ok expression
  | TList element_ty ->
      Result.map
        (fun mapper ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Lg_runtime.Runtime_metadata.of_list",
              [ mapper; expression ] ))
        (metadata_mapper element_ty)
  | TSeq element_ty ->
      Result.map
        (fun mapper ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Lg_runtime.Runtime_metadata.of_seq",
              [ mapper; expression ] ))
        (metadata_mapper element_ty)
  | TVector element_ty ->
      Result.map
        (fun mapper ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Lg_runtime.Runtime_metadata.of_vector",
              [ mapper; expression ] ))
        (metadata_mapper element_ty)
  | TArray element_ty | TOcaml_app ("array", [ element_ty ]) ->
      Result.map
        (fun mapper ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Lg_runtime.Runtime_metadata.of_array",
              [ mapper; expression ] ))
        (metadata_mapper element_ty)
  | TSet element_ty -> (
      match (metadata_mapper element_ty, Types.set_module_name element_ty) with
      | (Error _ as error), _ | _, (Error _ as error) -> error
      | Ok mapper, Ok set_module ->
          Ok
            (Semantic_ir.Apply
               ( Semantic_ir.Ident "Lg_runtime.Runtime_metadata.of_set",
                 [
                   mapper;
                   Semantic_ir.Apply
                     ( Semantic_ir.Ident (set_module ^ ".elements"),
                       [ expression ] );
                 ] )))
  | TNullable element_ty | TOcaml_app ("option", [ element_ty ]) ->
      let value_name = "__lg_metadata_optional_value" in
      Result.map
        (fun packed ->
          Semantic_ir.Match
            ( expression,
              [
                ( Semantic_ir.PConstructor ("None", None),
                  Semantic_ir.Ident "Lg_runtime.Runtime_metadata.nil" );
                ( Semantic_ir.PConstructor
                    ("Some", Some (Semantic_ir.PVar value_name)),
                  packed );
              ] ))
        (pack_metadata_expression element_ty (Semantic_ir.Ident value_name))
  | map_ty -> (
      match Types.dynamic_map_types map_ty with
      | Some (key_ty, value_ty) -> (
          match (metadata_mapper key_ty, metadata_mapper value_ty) with
          | (Error _ as error), _ | _, (Error _ as error) -> error
          | Ok key_mapper, Ok value_mapper ->
              Ok
                (Semantic_ir.Apply
                   ( Semantic_ir.Ident "Lg_runtime.Runtime_metadata.of_map",
                     [ key_mapper; value_mapper; expression ] )))
      | None ->
          Error.error
            ("metadata requires a closed EDN-compatible static value; got "
            ^ Types.source_name ty))

and metadata_mapper ty =
  let value_name = "__lg_metadata_value" in
  Result.map
    (fun body ->
      Semantic_ir.Fun ([ Semantic_ir.PVar value_name ], body))
    (pack_metadata_expression ty (Semantic_ir.Ident value_name))

let rec unpack_metadata_expression ty expression =
  let convert name =
    Ok
      (Semantic_ir.Apply
         (Semantic_ir.Ident ("Lg_runtime.Runtime_metadata." ^ name), [ expression ]))
  in
  match Types.constraint_value_type ty with
  | TBool -> convert "bool_value"
  | TInt | TOcaml "int" -> convert "int_value"
  | TFloat -> convert "float_value"
  | TString -> convert "string_value"
  | TChar -> convert "char_value"
  | TSymbol -> convert "symbol_value"
  | TKeyword -> convert "keyword_value"
  | TRegex -> convert "regex_value"
  | TOcaml "Lg_edn_backend.t" -> Ok expression
  | TList element_ty ->
      Result.map
        (fun decoder ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Lg_runtime.Runtime_metadata.list_value",
              [ decoder; expression ] ))
        (metadata_decoder element_ty)
  | TSeq element_ty ->
      Result.map
        (fun decoder ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Lg_runtime.Runtime_metadata.seq_value",
              [ decoder; expression ] ))
        (metadata_decoder element_ty)
  | TVector element_ty ->
      Result.map
        (fun decoder ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Lg_runtime.Runtime_metadata.vector_value",
              [ decoder; expression ] ))
        (metadata_decoder element_ty)
  | TArray element_ty | TOcaml_app ("array", [ element_ty ]) ->
      Result.map
        (fun decoder ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Lg_runtime.Runtime_metadata.array_value",
              [ decoder; expression ] ))
        (metadata_decoder element_ty)
  | TSet element_ty -> (
      match (metadata_decoder element_ty, Types.set_module_name element_ty) with
      | (Error _ as error), _ | _, (Error _ as error) -> error
      | Ok decoder, Ok set_module ->
          Ok
            (Semantic_ir.Apply
               ( Semantic_ir.Ident (set_module ^ ".of_list"),
                 [
                   Semantic_ir.Apply
                     ( Semantic_ir.Ident
                         "Lg_runtime.Runtime_metadata.set_values",
                       [ decoder; expression ] );
                 ] )))
  | TNullable element_ty | TOcaml_app ("option", [ element_ty ]) ->
      Result.map
        (fun decoder ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Lg_runtime.Runtime_metadata.option_value",
              [ decoder; expression ] ))
        (metadata_decoder element_ty)
  | map_ty -> (
      match Types.dynamic_map_types map_ty with
      | Some (key_ty, value_ty) -> (
          match (metadata_decoder key_ty, metadata_decoder value_ty) with
          | (Error _ as error), _ | _, (Error _ as error) -> error
          | Ok key_decoder, Ok value_decoder ->
              Ok
                (Semantic_ir.Apply
                   ( Semantic_ir.Ident "Lg_runtime.Runtime_metadata.map_value",
                     [ key_decoder; value_decoder; expression ] )))
      | None ->
          Error.error
            ("metadata lookup cannot decode " ^ Types.source_name ty))

and metadata_decoder ty =
  let value_name = "__lg_metadata_decoded_value" in
  Result.map
    (fun body -> Semantic_ir.Fun ([ Semantic_ir.PVar value_name ], body))
    (unpack_metadata_expression ty (Semantic_ir.Ident value_name))

let rec constrained_storage_type expected actual =
  match Types.dynamic_constraint_info expected with
  | Some _ -> expected
  | None -> (
      match Types.protocol_constraint_info expected with
      | Some (_, _, value_ty) -> constrained_storage_type value_ty actual
      | None -> (
          match Types.contains_constraint_info expected with
          | Some (_, value_ty) -> (
              match value_ty with
              | TUnknown | TMeta _ | TVar _ ->
                  Types.constraint_value_type actual
              | _ -> constrained_storage_type value_ty actual)
          | None -> (
          match expected with
          | TOcaml_app (name, [ _element_ty; value_ty ])
            when name = Types.seqable_constraint_name
                 || name = Types.optional_seqable_constraint_name
                 || name = Types.optional_sequential_constraint_name -> (
              match value_ty with
              | TUnknown | TMeta _ | TVar _ ->
                  Types.constraint_value_type actual
              | _ -> constrained_storage_type value_ty actual)
          | TUnknown | TMeta _ | TVar _ -> Types.constraint_value_type actual
          | _ -> Types.constraint_value_type actual)))

let rec pack_constrained_value ?row_type_name env expected argument =
  let expected = Types.deduplicate_protocol_constraints expected in
  let requires_binding =
    match
      (argument.record_values, Semantic_ir.unlocated argument.semantic_expr)
    with
    | Some _, _ -> false
    | None, Semantic_ir.Ident _ -> false
    | ( None,
        Semantic_ir.Apply
          (Semantic_ir.Ident "Rrbvec.of_list", [ Semantic_ir.List _ ]) ) ->
        false
    | ( None,
        ( Semantic_ir.Int _ | Semantic_ir.Int64 _ | Semantic_ir.Float _
        | Semantic_ir.String _ | Semantic_ir.Char _ | Semantic_ir.Bool _
        | Semantic_ir.Unit | Semantic_ir.Constructor (_, None) ) ) ->
        false
    | _ -> true
  in
  if requires_binding then
      let () = incr constrained_argument_counter in
      let argument_name =
        "__lg_constrained_argument_"
        ^ string_of_int !constrained_argument_counter
      in
      let bound_expression = argument.semantic_expr in
      let bound_argument =
      {
        argument with
          semantic_expr =
          Semantic_ir.annotate argument.ty (Semantic_ir.Ident argument_name);
        }
      in
      pack_constrained_value ?row_type_name env expected bound_argument
      |> Result.map (fun packed ->
             let occurrences = ref 0 in
             let packed =
               Semantic_ir.rewrite
                 (function
                   | Semantic_ir.Ident name as expression
                     when String.equal name argument_name ->
                       incr occurrences;
                       expression
                   | expression -> expression)
                 packed
             in
             if !occurrences = 1 then
               Semantic_ir.rewrite
                 (function
                   | Semantic_ir.Ident name
                     when String.equal name argument_name ->
                       bound_expression
                   | expression -> expression)
                 packed
             else
               Semantic_ir.Let
                 ([ (Semantic_ir.PVar argument_name, bound_expression) ], packed))
  else
  match (expected, argument.ty) with
  | (TNullable _ | TOcaml_app ("option", [ _ ])), TNil ->
      Ok (Semantic_ir.Constructor ("None", None))
  | ( (TNullable expected_inner | TOcaml_app ("option", [ expected_inner ])),
      (TNullable actual_inner | TOcaml_app ("option", [ actual_inner ])) )
    when has_capability_constraint expected_inner ->
      let value_name = "__lg_optional_constrained_value" in
      let value = typed_ir actual_inner (Semantic_ir.Ident value_name) in
      Result.map
        (fun packed ->
          Semantic_ir.Match
            ( argument.semantic_expr,
              [
                ( Semantic_ir.PConstructor ("None", None),
                  Semantic_ir.Constructor ("None", None) );
                ( Semantic_ir.PConstructor
                    ( "Some",
                      Some
                        (constrained_identifier_pattern value_name actual_inner)
                    ),
                  Semantic_ir.Constructor ("Some", Some packed) );
              ] ))
        (pack_constrained_value env expected_inner value)
  | (TNullable inner | TOcaml_app ("option", [ inner ])), actual
    when not
           (match actual with
           | TNullable _ | TOcaml_app ("option", [ _ ]) -> true
           | _ -> false) ->
      let inner =
        match inner with
        | TUnknown | TMeta _ | TVar _ -> actual
        | inner -> inner
      in
      Result.map
        (fun value -> Semantic_ir.Constructor ("Some", Some value))
        (pack_constrained_value env inner argument)
  | ( expected,
      (TNullable actual_ty | TOcaml_app ("option", [ actual_ty ])) )
    when Option.is_some (Types.protocol_constraint_info expected)
         &&
         let actual_value_ty = Types.constraint_value_type actual_ty in
         argument_compatible expected actual_value_ty ->
      let actual_value_ty = Types.constraint_value_type actual_ty in
      pack_constrained_value ?row_type_name env expected
        (typed_ir actual_value_ty
           (Semantic_ir.Apply
              (Semantic_ir.Ident "Option.get", [ argument.semantic_expr ])))
  | ( expected,
      (TNullable actual_ty | TOcaml_app ("option", [ actual_ty ])) )
    when (match Types.seqable_constraint_info expected with
         | Some ((`Optional | `Optional_sequential), _, _) ->
             argument_compatible expected actual_ty
         | Some (`Required, _, _) | None -> false) ->
      let value_name = "__lg_optional_capability_value" in
      let value = typed_ir actual_ty (Semantic_ir.Ident value_name) in
      Result.bind
        (pack_constrained_value ?row_type_name env expected
           (typed_ir TNil (Semantic_ir.Constructor ("None", None))))
        (fun absent ->
          Result.map
            (fun present ->
              Semantic_ir.Match
                ( argument.semantic_expr,
                  [
                    (Semantic_ir.PConstructor ("None", None), absent);
                    ( Semantic_ir.PConstructor
                        ( "Some",
                          Some
                            (constrained_identifier_pattern value_name
                               actual_ty) ),
                      present );
                  ] ))
            (pack_constrained_value ?row_type_name env expected value))
  | ( TOcaml_app (expected_name, [ expected_element; expected_value ]),
      TOcaml_app (actual_name, [ actual_element; actual_value ]) )
    when expected_name = actual_name
         && (expected_name = Types.seqable_constraint_name
            || expected_name = Types.optional_seqable_constraint_name
            || expected_name = Types.optional_sequential_constraint_name)
         && Types.equal expected_value actual_value
         &&
         (Types.equal expected_element actual_element
         || ((not (has_capability_constraint expected_element))
            && Bool.equal
                 (Types.is_dynamic actual_element)
                 (Types.is_dynamic expected_element)
            && Types.assignable ~policy:Host_boundary
                 ~expected:expected_element ~actual:actual_element)) ->
      Ok (constrained_argument_expression argument)
  | expected, actual
    when (match
            ( Types.protocol_constraint_info expected,
              Types.protocol_constraint_info actual )
          with
         | Some (expected_id, _, _), Some (actual_id, _, _) ->
             Protocol_id.equal expected_id actual_id
             && Types.equal expected actual
         | _ -> false) ->
      Ok (constrained_argument_expression argument)
  | expected, actual
    when Option.is_some (Types.truthy_constraint_info expected)
         && Option.is_some (Types.truthy_constraint_info actual) ->
      Ok (constrained_argument_expression argument)
  | expected, _
    when Option.is_some (Types.truthy_constraint_info expected) ->
      let value_ty = Types.truthy_constraint_info expected |> Option.get in
      let witness_value_ty =
        match value_ty with
        | TUnknown | TMeta _ | TVar _ ->
            Types.constraint_value_type argument.ty
        | ty -> ty
      in
      let value_name = "__lg_truthy_value" in
      let value = Semantic_ir.Ident value_name in
      let pattern =
        if Expression_support.truthiness_needs_value witness_value_ty then
          Semantic_ir.PVar value_name
        else Semantic_ir.PAny
      in
      let witness =
        Semantic_ir.Fun
          ( [ pattern ],
            Expression_support.truthiness_expression witness_value_ty value )
      in
      Result.map
        (fun packed -> Semantic_ir.Tuple [ witness; packed ])
        (pack_constrained_value env value_ty argument)
  | expected, actual
    when Option.is_some (Types.nil_predicate_constraint_info expected)
         && Option.is_some (Types.nil_predicate_constraint_info actual) ->
      Ok (constrained_argument_expression argument)
  | expected, _
    when Option.is_some (Types.nil_predicate_constraint_info expected) ->
      let value_ty =
        Types.nil_predicate_constraint_info expected |> Option.get
      in
      let witness_value_ty =
        match value_ty with
        | TUnknown | TMeta _ | TVar _ ->
            Types.constraint_value_type argument.ty
        | ty -> ty
      in
      let value_name = "__lg_nil_predicate_value" in
      let value = Semantic_ir.Ident value_name in
      let witness =
        Semantic_ir.Fun
          ( [ Semantic_ir.PVar value_name ],
            Expression_support.nil_predicate_expression witness_value_ty value
          )
      in
      Result.map
        (fun packed -> Semantic_ir.Tuple [ witness; packed ])
        (pack_constrained_value env value_ty argument)
  | expected, actual
    when Option.is_some (Types.printable_constraint_info expected)
         && Option.is_some (Types.printable_constraint_info actual) ->
      Ok (constrained_argument_expression argument)
  | expected, _
    when Option.is_some (Types.printable_constraint_info expected) ->
      let value_ty = Types.printable_constraint_info expected |> Option.get in
      let witness_value_ty =
        match value_ty with
        | TUnknown | TMeta _ | TVar _ ->
            Types.constraint_value_type argument.ty
        | ty -> ty
      in
      let value_name = "__lg_printable_value" in
      let value =
        typed_ir witness_value_ty (Semantic_ir.Ident value_name)
      in
      let display_witness =
        Semantic_ir.Fun
          ( [ Semantic_ir.PVar value_name ],
            Codegen.stringify_expr_ir ~pr:false value )
      in
      let readable_witness =
        Semantic_ir.Fun
          ( [ Semantic_ir.PVar value_name ],
            Codegen.stringify_expr_ir ~pr:true value )
      in
      Result.map
        (fun packed ->
          Semantic_ir.Tuple
            [ Semantic_ir.Tuple [ display_witness; readable_witness ]; packed ])
        (pack_constrained_value env value_ty argument)
  | expected, actual
    when Option.is_some (Types.hashable_constraint_info expected)
         && Option.is_some (Types.hashable_constraint_info actual) ->
      Ok (constrained_argument_expression argument)
  | expected, _ when Option.is_some (Types.hashable_constraint_info expected) ->
      let value_ty = Types.hashable_constraint_info expected |> Option.get in
      let witness_value_ty =
        match value_ty with
        | TUnknown | TMeta _ | TVar _ -> Types.constraint_value_type argument.ty
        | ty -> ty
      in
      let value_name = "__lg_hashable_value" in
      let value = typed_ir witness_value_ty (Semantic_ir.Ident value_name) in
      Result.bind (compile_static_hash_capability env value) (fun hash ->
          let witness =
            Semantic_ir.Fun ([ Semantic_ir.PVar value_name ], hash)
          in
          Result.map
            (fun packed -> Semantic_ir.Tuple [ witness; packed ])
            (pack_constrained_value env value_ty argument))
  | expected, actual
    when Option.is_some (Types.comparable_constraint_info expected)
         && Option.is_some (Types.comparable_constraint_info actual) ->
      Ok (constrained_argument_expression argument)
  | expected, _ when Option.is_some (Types.comparable_constraint_info expected) ->
      let value_ty = Types.comparable_constraint_info expected |> Option.get in
      let witness_value_ty =
        match value_ty with
        | TUnknown | TMeta _ | TVar _ -> Types.constraint_value_type argument.ty
        | ty -> ty
      in
      let left_name = "__lg_comparable_left" in
      let right_name = "__lg_comparable_right" in
      let left = typed_ir witness_value_ty (Semantic_ir.Ident left_name) in
      let right = typed_ir witness_value_ty (Semantic_ir.Ident right_name) in
      Result.bind
        (compile_static_compare_capability env left right)
        (fun compared ->
          let witness =
            Semantic_ir.Fun
              ( [ Semantic_ir.PVar left_name; Semantic_ir.PVar right_name ],
                compared )
          in
          Result.map
            (fun packed -> Semantic_ir.Tuple [ witness; packed ])
            (pack_constrained_value env value_ty argument))
  | expected, actual
    when Option.is_some (Types.array_index_constraint_info expected)
         && Option.is_some (Types.array_index_constraint_info actual) ->
      Ok (constrained_argument_expression argument)
  | expected, _
    when Option.is_some (Types.array_index_constraint_info expected) ->
      let value_ty = Types.array_index_constraint_info expected |> Option.get in
      let witness_value_ty =
        match value_ty with
        | TUnknown | TMeta _ | TVar _ -> Types.constraint_value_type argument.ty
        | ty -> ty
      in
      let value_name = "__lg_array_index_value" in
      let value = typed_ir witness_value_ty (Semantic_ir.Ident value_name) in
      let converted =
        if Types.equal witness_value_ty TInt then Ok value.semantic_expr
        else if Types.equal witness_value_ty TFloat then
          Ok
            (Semantic_ir.Apply
               (Semantic_ir.Ident "int_of_float", [ value.semantic_expr ]))
        else
          Error.error
            ("array index requires int or float, got "
           ^ Types.source_name witness_value_ty)
      in
      Result.bind converted (fun converted ->
          let witness =
            Semantic_ir.Fun ([ Semantic_ir.PVar value_name ], converted)
          in
          Result.map
            (fun packed -> Semantic_ir.Tuple [ witness; packed ])
            (pack_constrained_value env value_ty argument))
  | expected, actual
    when Option.is_some (Types.symbol_predicate_constraint_info expected)
         && Option.is_some
              (Types.symbol_predicate_constraint_info actual) ->
      Ok (constrained_argument_expression argument)
  | expected, _
    when Option.is_some (Types.symbol_predicate_constraint_info expected) ->
      let value_ty =
        Types.symbol_predicate_constraint_info expected |> Option.get
      in
      let witness_value_ty =
        match value_ty with
        | TUnknown | TMeta _ | TVar _ ->
            Types.constraint_value_type argument.ty
        | ty -> ty
      in
      let value_name = "__lg_symbol_predicate_value" in
      let value = Semantic_ir.Ident value_name in
      let witness_body =
        if Types.equal witness_value_ty TSymbol then
          Semantic_ir.Constructor ("Some", Some value)
        else if Types.is_dynamic witness_value_ty then
          Semantic_ir.If
            ( Semantic_ir.Apply
                ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.is_symbol",
                  [ value ] ),
              Semantic_ir.Constructor
                ( "Some",
                  Some
                    (Semantic_ir.Apply
                       ( Semantic_ir.Ident
                           "Lg_runtime.Runtime_dynamic.as_symbol",
                         [ value ] )) ),
              Semantic_ir.Constructor ("None", None) )
        else Semantic_ir.Constructor ("None", None)
      in
      let witness =
        Semantic_ir.Fun ([ Semantic_ir.PVar value_name ], witness_body)
      in
      Result.map
        (fun packed -> Semantic_ir.Tuple [ witness; packed ])
        (pack_constrained_value env value_ty argument)
  | expected, actual
    when Option.is_some (Types.contains_constraint_info expected)
         && Option.is_some (Types.contains_constraint_info actual) ->
      Ok (constrained_argument_expression argument)
  | expected, _
    when Option.is_some (Types.contains_constraint_info expected) ->
      let _, value_ty =
        Types.contains_constraint_info expected |> Option.get
      in
      Result.bind
        (Collection_capability.contains_adapter argument)
        (fun witness ->
          let packed =
            match (value_ty, row_type_name) with
            | TNamed_record record, _ ->
                project_constraint_row ~named_record:record env
                  record.type_name record.fields argument
            | TRecord fields, Some type_name ->
                project_constraint_row env type_name fields argument
            | (TUnknown | TMeta _ | TVar _), _ -> Ok Semantic_ir.Unit
            | _, _ ->
                pack_constrained_value ?row_type_name env value_ty argument
          in
          Result.map
            (fun packed -> Semantic_ir.Tuple [ witness; packed ])
            packed)
  | _ -> (
  match Types.dynamic_constraint_info expected with
  | Some _ -> pack_dynamic_value env expected argument
        | None -> (
  match Types.protocol_constraint_info expected with
  | Some _ when Types.is_dynamic argument.ty ->
      dynamic_unpack env expected argument.semantic_expr
  | Some (protocol_id, witness_ty, value_ty) ->
      let projected_protocol =
        project_protocol_constraint protocol_id argument
      in
      let rec witness_method_types = function
        | TUnit -> Ok []
        | TTuple [ method_ty; rest ] ->
            Result.map
              (fun methods -> method_ty :: methods)
              (witness_method_types rest)
        | _ -> Error.error "invalid protocol witness type"
      in
      let rec adapt_witness_method expected_ty (implementation : binding) =
        match (expected_ty, implementation.ty) with
        | TOverloaded_fn expected_arities, TOverloaded_fn actual_arities ->
            let stored_fn_type (arity : fn_arity) =
              let parameters =
                match arity.rest_param with
                | None -> arity.fixed_params
                | Some rest_ty -> arity.fixed_params @ [ TSeq rest_ty ]
              in
              TFn (parameters, arity.return_ty)
            in
            let find_actual (expected : fn_arity) =
              actual_arities
              |> List.mapi (fun index actual -> (index, actual))
              |> List.find_opt (fun (_, (actual : fn_arity)) ->
                     List.length actual.fixed_params
                     = List.length expected.fixed_params
                     && Option.is_some actual.rest_param
                        = Option.is_some expected.rest_param)
            in
            let rec adapt_arities adapted = function
              | [] -> Ok (witness_storage (List.rev adapted))
              | expected :: rest -> (
                  match find_actual expected with
                  | None ->
                      Error.error
                        "protocol witness implementation is missing an arity"
                  | Some (index, actual) -> (
                      match List.nth_opt implementation.overload_targets index with
                      | None ->
                          Error.error
                            "protocol witness implementation is missing an overload target"
                      | Some ocaml_name ->
                          let selected =
                            {
                              implementation with
                              ocaml_name;
                              ty = stored_fn_type actual;
                            }
                          in
                          Result.bind
                            (adapt_witness_method (stored_fn_type expected) selected)
                            (fun adapted_method ->
                              adapt_arities (adapted_method :: adapted) rest)))
            in
            adapt_arities [] expected_arities
        | ( TFn (expected_params, expected_return),
            TFn (actual_params, actual_return) )
          when List.length expected_params = List.length actual_params ->
            let stored_receiver_ty =
              constrained_storage_type value_ty argument.ty
            in
            let expected_params =
              match (expected_params, actual_params) with
              | _expected_receiver :: expected_rest,
                actual_receiver :: _actual_rest ->
                  let receiver_ty =
                    match stored_receiver_ty with
                    | TUnknown | TMeta _ | TVar _ -> actual_receiver
                    | ty -> ty
                  in
                  receiver_ty :: expected_rest
              | [], [] -> []
              | _ -> expected_params
            in
            let parameter_names =
              List.mapi
                (fun index _ ->
                  "__lg_protocol_witness_argument_" ^ string_of_int index)
                expected_params
            in
            let rec adapt_parameters adapted expected actual names =
              match (expected, actual, names) with
              | [], [], [] -> Ok (List.rev adapted)
              | expected_ty :: expected, actual_ty :: actual, name :: names ->
                  let expected_ty =
                    match expected_ty with
                    | TUnknown | TMeta _ ->
                        Types.constraint_value_type actual_ty
                    | ty -> ty
                  in
                  let value =
                    typed_ir expected_ty (Semantic_ir.Ident name)
                  in
                  let adapted_value =
                    if
                      Types.equal expected_ty actual_ty
                      &&
                      match (expected_ty, actual_ty) with
                      | TNamed_record expected, TNamed_record actual ->
                          List.length expected.type_arguments
                          = List.length actual.type_arguments
                          && List.for_all2 Types.equal
                               expected.type_arguments actual.type_arguments
                      | _ -> true
                    then
                      Ok value.semantic_expr
                    else if
                      expects_dynamic_value expected_ty
                      && not (expects_dynamic_value actual_ty)
                    then dynamic_unpack env actual_ty value.semantic_expr
                    else if
                      has_capability_constraint actual_ty
                      ||
                      match (actual_ty, expected_ty) with
                      | TNamed_record actual, TNamed_record expected ->
                          Type_id.equal actual.type_id expected.type_id
                      | _ -> false
                    then
                      pack_constrained_value env actual_ty value
                    else
                      Ok
                        (coerce_expression_to_type actual_ty expected_ty
                           value.semantic_expr)
                  in
                  Result.bind adapted_value (fun adapted_value ->
                      adapt_parameters (adapted_value :: adapted) expected
                        actual names)
              | _ -> Error.error "protocol witness method arity mismatch"
            in
            Result.bind
              (adapt_parameters [] expected_params actual_params
                 parameter_names)
              (fun arguments ->
                let expected_return =
                  match expected_return with
                  | TUnknown | TMeta _ | TVar _ -> actual_return
                  | ty -> ty
                in
                let result =
                  typed_ir actual_return
                    (Semantic_ir.Apply
                       ( Semantic_ir.Ident implementation.ocaml_name,
                         arguments ))
                in
                let adapt_sequence expected_element actual_element sequence =
                  if Types.equal expected_element actual_element then
                    Ok sequence
                  else
                    let item_name = "__lg_protocol_return_item" in
                    let item =
                      typed_ir actual_element (Semantic_ir.Ident item_name)
                    in
                    Result.map
                      (fun item ->
                        let item_pattern =
                          typed_item_pattern item_name actual_element
                        in
                        Semantic_ir.Apply
                          ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                            [
                              Semantic_ir.Fun ([ item_pattern ], item);
                              sequence;
                            ] ))
                      (if Types.is_dynamic expected_element then
                         pack_dynamic_value env expected_element item
                       else if has_capability_constraint expected_element then
                         pack_constrained_value env expected_element item
                       else if Types.is_dynamic actual_element then
                         dynamic_unpack env expected_element item.semantic_expr
                       else
                         Ok
                           (coerce_expression_to_type expected_element
                              actual_element item.semantic_expr))
                in
                let adapted_result =
                  if Types.equal expected_return actual_return then
                    Ok result.semantic_expr
                  else
                    match (expected_return, actual_return) with
                    | ( (TNullable expected_inner
                        | TOcaml_app ("option", [ expected_inner ])),
                        (TNullable actual_inner
                        | TOcaml_app ("option", [ actual_inner ])) ) -> (
                        match
                          ( Types.next_seq_element expected_inner,
                            Types.next_seq_element actual_inner )
                        with
                        | Some expected_element, Some _ ->
                            let value_name = "__lg_protocol_return_sequence" in
                            let value =
                              typed_ir actual_inner
                                (Semantic_ir.Ident value_name)
                            in
                            Result.bind
                              (Collection_capability.to_seq_expr env value)
                              (fun (actual_element, sequence) ->
                                Result.map
                                  (fun sequence ->
                                    Semantic_ir.Match
                                      ( result.semantic_expr,
                                        [
                                          ( Semantic_ir.PConstructor
                                              ("None", None),
                                            Semantic_ir.Constructor
                                              ("None", None) );
                                          ( Semantic_ir.PConstructor
                                              ( "Some",
                                                Some
                                                  (Semantic_ir.PVar value_name)
                                              ),
                                            Semantic_ir.Constructor
                                              ("Some", Some sequence) );
                                        ] ))
                                  (adapt_sequence expected_element
                                     actual_element sequence))
                        | _ ->
                            Ok
                              (coerce_expression_to_type expected_return
                                 actual_return result.semantic_expr))
                    | TSeq expected_element, _ -> (
                        match Collection_capability.to_seq_expr env result with
                        | Error _
                          when (match actual_return with
                               | TUnknown | TMeta _ | TVar _ -> true
                               | _ -> false) ->
                            Ok
                              (coerce_expression_to_type expected_return
                                 actual_return result.semantic_expr)
                        | Error _ as error -> error
                        | Ok (actual_element, sequence) ->
                            adapt_sequence expected_element actual_element
                              sequence)
                    | _
                      when has_capability_constraint expected_return
                           && not (Types.is_dynamic expected_return) ->
                        pack_constrained_value env expected_return result
                    | _ when Types.is_dynamic expected_return ->
                        pack_dynamic_value env expected_return result
                    | _ when Types.is_dynamic actual_return ->
                        dynamic_unpack env expected_return result.semantic_expr
                    | _ ->
                        Ok
                          (coerce_expression_to_type expected_return
                             actual_return result.semantic_expr)
                in
                Result.map
                  (fun adapted_result ->
                    Semantic_ir.Fun
                      ( List.map
                          (fun name -> Semantic_ir.PVar name)
                          parameter_names,
                        adapted_result ))
                  adapted_result)
        | _ ->
            Error.error "protocol witness implementation type mismatch"
      in
      let witness =
        match projected_protocol with
        | Some (witness, _) -> Ok witness
        | None -> (
            let implementations =
              Protocol.witness_implementations env protocol_id
                (Types.constraint_value_type argument.ty)
            in
            match implementations with
            | None -> (
                match Types.constraint_value_type argument.ty with
                | TNamed_record record ->
                    Error.error
                      ("missing protocol implementation for "
                      ^ Protocol_id.to_string protocol_id
                      ^ " on " ^ Type_id.to_string record.type_id)
                | _ -> Ok (Semantic_ir.Constructor ("None", None)))
            | Some implementations ->
                let shared_name =
                  let implementation_key =
                    implementations
                    |> List.map (fun (implementation : binding) ->
                           implementation.ocaml_name ^ ":"
                           ^ Types.ocaml_name implementation.ty)
                    |> String.concat ","
                  in
                  let key =
                    String.concat "|"
                      [
                        Target.to_string (Compiler_environment.target env);
                        Protocol_id.to_string protocol_id;
                        Types.ocaml_name expected;
                        Types.ocaml_name argument.ty;
                        implementation_key;
                      ]
                  in
                  "__lg_w_"
                  ^ String.sub (Digest.to_hex (Digest.string key)) 0 12
                in
                let rec adapt_methods adapted expected implementations =
                  match (expected, implementations) with
                  | [], [] -> Ok (List.rev adapted)
                  | expected :: expected_rest,
                    implementation :: implementation_rest ->
                      Result.bind
                        (adapt_witness_method expected implementation)
                        (fun method_ ->
                          adapt_methods (method_ :: adapted) expected_rest
                            implementation_rest)
                  | _ -> Error.error "protocol witness method count mismatch"
                in
                Result.bind (witness_method_types witness_ty)
                  (fun method_tys ->
                    Result.map
                      (fun methods ->
                        Semantic_ir.SharedValue
                          ( shared_name,
                            Semantic_ir.Constructor
                              ("Some", Some (witness_storage methods)) ))
                      (adapt_methods [] method_tys implementations)))
      in
      Result.bind witness (fun witness ->
          let value_argument =
            match projected_protocol with
            | Some (_, value) -> value
            | None -> argument
          in
          pack_constrained_value env value_ty value_argument
          |> Result.map (fun value -> Semantic_ir.Tuple [ witness; value ]))
  | None -> (
      match expected with
      | TOcaml_app (name, [ expected_element; value_ty ])
        when name = Types.seqable_constraint_name
             || name = Types.optional_seqable_constraint_name
                       || name = Types.optional_sequential_constraint_name -> (
          let argument =
            Edn_value_elaborator.map_entries_argument expected_element argument
          in
          let actual_element =
            Collection_capability.element_type env argument
            |> Option.map
                 (Collection_capability.resolve_callback_record env)
          in
          let erase_value =
            false
          in
          let stores_dynamic_value = Types.is_dynamic value_ty in
          let expected_element =
            match expected_element with
            | TUnknown | TMeta _ | TVar _ ->
                if erase_value then Types.dynamic_constraint TUnknown
                else
                  Option.value actual_element
                    ~default:(Types.dynamic_constraint TUnknown)
            | ty -> ty
          in
          let stored_value_ty =
            if erase_value then Types.dynamic_constraint TUnknown else value_ty
          in
          let statically_empty_argument =
            match Semantic_ir.unlocated argument.semantic_expr with
            | Semantic_ir.Ident "Rrbvec.empty"
            | Semantic_ir.List []
            | Semantic_ir.Array []
            | Semantic_ir.String "" ->
                true
            | _ -> false
          in
          let element_mapper =
            match (actual_element, row_type_name, expected_element) with
            | Some actual_element, _, _
              when
                not
                  (Types.is_dynamic argument.ty
                  &&
                  match actual_element with
                  | TUnknown | TMeta _ | TVar _ -> true
                  | _ -> false) ->
                let edn_mapper =
                  Edn_value_elaborator.map_entry_mapper
                    ~pack_constrained:(pack_constrained_value env)
                    expected_element actual_element
                in
                (match edn_mapper with
                | Ok (Some _) | Error _ -> edn_mapper
                | Ok None -> (
                    match actual_element with
                    | TUnknown | TMeta _ | TVar _
                      when statically_empty_argument ->
                        Ok None
                    | actual_element
                      when Types.equal actual_element expected_element ->
                        Ok None
                    | actual_element
                      when has_capability_constraint actual_element
                           && argument_compatible expected_element
                                actual_element ->
                        Ok None
                    | actual_element
                      when Types.is_dynamic actual_element
                           && Types.is_dynamic expected_element ->
                        Ok None
                    | actual_element -> (
                        match (row_type_name, expected_element) with
                        | Some type_name, TRecord fields
                          when Types.assignable ~policy:Structural
                                 ~expected:expected_element
                                 ~actual:actual_element ->
                            let item_name = "__lg_seqable_row_item" in
                            let item =
                              typed_ir actual_element
                                (Semantic_ir.Ident item_name)
                            in
                            project_constraint_row env type_name fields item
                            |> Result.map (fun row ->
                                   Some
                                     (Semantic_ir.Fun
                                        ( [
                                            typed_item_pattern item_name
                                              actual_element;
                                          ],
                                          row )))
                        | _ when Types.is_dynamic expected_element
                                 && not (Types.is_dynamic actual_element) ->
                            let item_name = "__lg_seqable_item" in
                            let item =
                              typed_ir actual_element
                                (Semantic_ir.Ident item_name)
                            in
                            pack_dynamic_value env expected_element item
                            |> Result.map (fun packed ->
                                   if
                                     is_identity_conversion item_name packed
                                   then None
                                   else
                                     Some
                                       (Semantic_ir.Fun
                                          ( [
                                              typed_dynamic_item_pattern env
                                                item_name actual_element;
                                            ],
                                            packed )))
                        | _ when Types.is_dynamic actual_element ->
                            let item_name = "__lg_seqable_item" in
                            dynamic_unpack env expected_element
                              (Semantic_ir.Ident item_name)
                            |> Result.map (fun unpacked ->
                                   if
                                     is_identity_conversion item_name unpacked
                                   then None
                                   else
                                     Some
                                       (Semantic_ir.Fun
                                          ( [ Semantic_ir.PVar item_name ],
                                            unpacked )))
                        | _ when has_capability_constraint expected_element ->
                            let item_name = "__lg_seqable_item" in
                            let item =
                              typed_ir actual_element
                                (Semantic_ir.Ident item_name)
                            in
                            pack_constrained_value env expected_element item
                            |> Result.map (fun packed ->
                                   if is_identity_conversion item_name packed
                                   then None
                                   else
                                     Some
                                       (Semantic_ir.Fun
                                          ( [
                                              typed_dynamic_item_pattern env
                                                item_name actual_element;
                                            ],
                                            packed )))
                        | _ -> Ok None)))
            | (None | Some (TUnknown | TMeta _ | TVar _)), _, expected_element
              when Types.is_dynamic argument.ty
                   && not (Types.is_dynamic expected_element) ->
                let item_name = "__lg_seqable_item" in
                dynamic_unpack env expected_element
                  (Semantic_ir.Ident item_name)
                |> Result.map (fun unpacked ->
                       Some
                         (Semantic_ir.Fun
                            ([ Semantic_ir.PVar item_name ], unpacked)))
            | _ -> Ok None
          in
                    match element_mapper with
          | Error _ as error -> error
                    | Ok element_mapper -> (
              let captured_adapter () =
                          Collection_capability.seqable_adapter ?element_mapper
                            env argument
                |> Result.map (fun adapter ->
                       Semantic_ir.Apply
                         ( Semantic_ir.Ident
                             "Lg_runtime.Runtime_seq.capture_adapter",
                           [ adapter; argument.semantic_expr ] ))
              in
              let forwarded_optional_adapter =
                let expects_optional =
                  name = Types.optional_seqable_constraint_name
                  || name = Types.optional_sequential_constraint_name
                in
                match
                  ( expects_optional,
                    Types.seqable_constraint_info argument.ty,
                    Semantic_ir.unlocated argument.semantic_expr )
                with
                | ( true,
                    Some
                      ( (`Optional | `Optional_sequential),
                        _,
                        actual_value_ty ),
                    Semantic_ir.Ident argument_name ) ->
                    if not (Types.equal value_ty actual_value_ty) then None
                    else
                      let optional_adapter =
                        Semantic_ir.Ident
                          (argument_name ^ "__seq_optional")
                      in
                      Some
                        (match element_mapper with
                        | None -> optional_adapter
                        | Some mapper ->
                            let adapter_name =
                              "__lg_forwarded_seqable_adapter"
                            in
                            let mapped_adapter =
                              Semantic_ir.Apply
                                ( Semantic_ir.Ident
                                    "Lg_runtime.Runtime_seq.map_adapter",
                                  [ mapper; Semantic_ir.Ident adapter_name ] )
                            in
                            Semantic_ir.Match
                              ( optional_adapter,
                                [
                                  ( Semantic_ir.PConstructor ("None", None),
                                    Semantic_ir.Constructor ("None", None) );
                                  ( Semantic_ir.PConstructor
                                      ( "Some",
                                        Some
                                          (Semantic_ir.PVar adapter_name) ),
                                    Semantic_ir.Constructor
                                      ( "Some",
                                        Some mapped_adapter ) );
                                ] ))
                | _ -> None
              in
              let adapter =
                match forwarded_optional_adapter with
                | Some adapter -> Ok adapter
                | None when erase_value || stores_dynamic_value ->
                  let can_adapt =
                    name = Types.seqable_constraint_name
                    || Types.is_dynamic argument.ty
                    ||
                    if name = Types.optional_sequential_constraint_name then
                      is_sequential_type argument.ty
                    else
                      Collection_capability.accepts_seqable env argument.ty
                  in
                  if can_adapt then
                    captured_adapter ()
                    |> Result.map (fun adapter ->
                                  if name = Types.seqable_constraint_name then
                                    adapter
                           else if
                             (name = Types.optional_seqable_constraint_name
                             || name
                                = Types.optional_sequential_constraint_name)
                             && Types.is_dynamic argument.ty
                           then
                             let predicate =
                               if name = Types.optional_seqable_constraint_name
                               then "Lg_runtime.Runtime_dynamic.is_seqable"
                               else
                                 "Lg_runtime.Runtime_dynamic.is_sequential"
                             in
                             Semantic_ir.If
                               ( Semantic_ir.Apply
                                   ( Semantic_ir.Ident predicate,
                                     [ argument.semantic_expr ] ),
                                 Semantic_ir.Constructor
                                   ("Some", Some adapter),
                                        Semantic_ir.Constructor ("None", None)
                                      )
                           else
                                    Semantic_ir.Constructor
                                      ("Some", Some adapter))
                  else Ok (Semantic_ir.Constructor ("None", None))
                | None when Types.is_dynamic argument.ty ->
                  let adapter =
                    match element_mapper with
                    | None ->
                        Semantic_ir.Ident
                          "Lg_runtime.Runtime_dynamic.to_seq"
                    | Some mapper ->
                        Semantic_ir.Apply
                          ( Semantic_ir.Ident
                              "Lg_runtime.Runtime_seq.map_adapter",
                            [
                              mapper;
                              Semantic_ir.Ident
                                "Lg_runtime.Runtime_dynamic.to_seq";
                            ] )
                  in
                  let adapter =
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident
                          "Lg_runtime.Runtime_seq.capture_adapter",
                        [ adapter; argument.semantic_expr ] )
                  in
                  Ok
                              (if name = Types.seqable_constraint_name then
                                 adapter
                     else if
                       (name = Types.optional_seqable_constraint_name
                       || name = Types.optional_sequential_constraint_name)
                       && Types.is_dynamic argument.ty
                     then
                       let predicate =
                         if name = Types.optional_seqable_constraint_name then
                           "Lg_runtime.Runtime_dynamic.is_seqable"
                         else "Lg_runtime.Runtime_dynamic.is_sequential"
                       in
                       Semantic_ir.If
                         ( Semantic_ir.Apply
                             ( Semantic_ir.Ident predicate,
                               [ argument.semantic_expr ] ),
                                     Semantic_ir.Constructor
                                       ("Some", Some adapter),
                           Semantic_ir.Constructor ("None", None) )
                               else
                                 Semantic_ir.Constructor ("Some", Some adapter))
                | None when name = Types.seqable_constraint_name ->
                            Collection_capability.seqable_adapter
                              ?element_mapper env argument
                  |> Result.map (fun adapter -> adapter)
                | None ->
                  let can_adapt =
                              if
                                name = Types.optional_sequential_constraint_name
                              then is_sequential_type argument.ty
                              else
                                Collection_capability.accepts_seqable env
                                  argument.ty
                  in
                  if can_adapt then
                    captured_adapter ()
                    |> Result.map (fun adapter ->
                           Semantic_ir.Constructor ("Some", Some adapter))
                            else Ok (Semantic_ir.Constructor ("None", None))
              in
              let packed_value =
                if erase_value && name = Types.seqable_constraint_name then
                  let dynamic = Types.dynamic_constraint TUnknown in
                  (match pack_dynamic_value env dynamic argument with
                  | Ok packed -> Ok packed
                  | Error _ ->
                      Result.bind
                        (Collection_capability.seqable_adapter ?element_mapper
                           env argument)
                        (fun sequence_adapter ->
                          let sequence =
                            Semantic_ir.Apply
                              (sequence_adapter, [ argument.semantic_expr ])
                          in
                          if Types.is_dynamic expected_element then
                            Ok
                              (Semantic_ir.Apply
                                 ( Semantic_ir.Ident
                                     "Lg_runtime.Runtime_dynamic.seq",
                                   [ sequence ] ))
                          else
                            let item_name = "__lg_erased_seqable_item" in
                            let item =
                              typed_ir expected_element
                                (Semantic_ir.Ident item_name)
                            in
                            Result.map
                              (fun packed_item ->
                                Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Runtime_dynamic.seq",
                                    [
                                      Semantic_ir.Apply
                                        ( Semantic_ir.Ident
                                            "Lg_runtime.Runtime_seq.map",
                                          [
                                            Semantic_ir.Fun
                                              ( [ Semantic_ir.PVar item_name ],
                                                packed_item );
                                            sequence;
                                          ] );
                                    ] ))
                              (pack_dynamic_value env dynamic item)))
                else
                  match Types.seqable_constraint_info argument.ty with
                  | Some _
                    when Type_solver.is_open stored_value_ty
                         && Option.is_some (optional_payload stored_value_ty)
                            = Option.is_some
                                (optional_payload
                                   (Types.constraint_value_type argument.ty)) ->
                      Ok (constrained_argument_value argument)
                  | Some _
                    when Types.equal stored_value_ty
                           (Types.constraint_value_type argument.ty) ->
                      Ok (constrained_argument_value argument)
                  | _ ->
                      let stored_argument =
                        match Types.seqable_constraint_info argument.ty with
                        | Some _ ->
                            typed_ir
                              (Types.constraint_value_type argument.ty)
                              (constrained_argument_value argument)
                        | None -> argument
                      in
                      pack_constrained_value env stored_value_ty stored_argument
              in
                        match
                          (adapter, packed_value)
               with
              | (Error _ as error), _ -> error
              | _, (Error _ as error) -> error
              | Ok adapter, Ok value ->
                  Ok (Semantic_ir.Tuple [ adapter; value ])))
                | expected
                  when (not (has_capability_constraint expected))
                       && has_capability_constraint argument.ty ->
                    Ok (constrained_argument_value argument)
                | TNamed_record expected
                  when (match argument.ty with
                       | TNamed_record actual ->
                           Type_id.equal expected.type_id actual.type_id
                           &&
                           not
                             (List.length expected.type_arguments
                              = List.length actual.type_arguments
                             && List.for_all2 Types.equal
                                  expected.type_arguments
                                  actual.type_arguments)
                           && named_record_has_capability_fields expected
                       | _ -> false) ->
                    project_constraint_row ~named_record:expected env
                      expected.type_name expected.fields argument
                | _ -> Ok argument.semantic_expr)))

and project_constraint_row ?named_record env type_name expected_fields argument =
  let type_name = row_call_type_name type_name in
  match Types.record_fields argument.ty with
  | None -> Error.error "constraint row projection expects a record value"
  | Some actual_fields ->
      let rec build values = function
        | [] -> Ok (List.rev values)
        | (expected : field) :: rest -> (
            match find_field expected.keyword actual_fields with
            | None when is_optional_type expected.ty ->
                build
                  ((expected, Semantic_ir.Constructor ("None", None))
                  :: values)
                  rest
            | None when Types.is_record_extension_field expected ->
                build
                  (( expected,
                     Semantic_ir.Ident "Lg_runtime.Runtime_map.empty" )
                  :: values)
                  rest
            | None ->
                Error.error
                  ("constraint row projection is missing field "
                  ^ expected.keyword)
            | Some actual ->
                let actual_value =
                  typed_ir actual.ty
                    (Structural_map.field_expr argument actual)
                in
                let value =
                  if has_capability_constraint expected.ty then
                    pack_constrained_value env expected.ty actual_value
                  else if Types.equal expected.ty actual.ty then
                    Ok actual_value.semantic_expr
                  else if Types.is_dynamic expected.ty then
                    pack_dynamic_value env expected.ty actual_value
                  else if Types.is_dynamic actual.ty then
                    dynamic_unpack env expected.ty actual_value.semantic_expr
                  else
                    Ok
                      (coerce_expression_to_type expected.ty actual.ty
                         actual_value.semantic_expr)
                in
                Result.bind value (fun value ->
                    build ((expected, value) :: values) rest))
      in
      Result.map
        (fun values ->
          match named_record with
          | Some record ->
              (Structural_map.named_record_expr record values).semantic_expr
          | None ->
              Semantic_ir.Record
                ( List.map
                    (fun ((field : field), value) ->
                      (field.ocaml_name, value))
                    values,
                  Some type_name ))
        (build [] expected_fields)

let reduced_callback_state = "__lg_reduced_callback_value"

let same_concrete_type left right =
  let same_arguments left right =
    List.length left = List.length right && List.for_all2 Types.equal left right
  in
  let named_application_matches record name arguments =
    (name = record.type_name || name = Type_id.name record.type_id)
    && same_arguments record.type_arguments arguments
  in
  match (left, right) with
  | TNamed_record left, TNamed_record right ->
      Type_id.equal left.type_id right.type_id
      && same_arguments left.type_arguments right.type_arguments
  | TNamed_record record, TOcaml_app (name, arguments)
  | TOcaml_app (name, arguments), TNamed_record record ->
      named_application_matches record name arguments
  | _ -> Types.equal left right

let rec same_runtime_representation left right =
  if Types.equal left right then true
  else if
    uses_dynamic_value_storage left || uses_dynamic_value_storage right
  then false
  else
    match (left, right) with
    | (TUnknown | TMeta _ | TVar _), _ | _, (TUnknown | TMeta _ | TVar _) -> true
    | TNil, (TNullable _ | TOcaml_app ("option", [ _ ]))
    | (TNullable _ | TOcaml_app ("option", [ _ ])), TNil ->
        true
    | TNullable left, TNullable right
    | TArray left, TArray right
    | TRef left, TRef right
    | TList left, TList right
    | TVector left, TVector right
    | TSeq left, TSeq right ->
        same_runtime_representation left right
    | TSet left, TSet right -> same_set_storage_representation left right
    | TOcaml_app (left_name, left_arguments),
      TOcaml_app (right_name, right_arguments) ->
        String.equal left_name right_name
        && List.length left_arguments = List.length right_arguments
        && List.for_all2 same_runtime_representation left_arguments
             right_arguments
    | TTuple left, TTuple right ->
        List.length left = List.length right
        && List.for_all2 same_runtime_representation left right
    | TNamed_record left, TNamed_record right ->
        Type_id.equal left.type_id right.type_id
        && List.length left.type_arguments = List.length right.type_arguments
        && List.for_all2 same_runtime_representation left.type_arguments
             right.type_arguments
    | TNamed_record record, TOcaml_app (name, arguments)
    | TOcaml_app (name, arguments), TNamed_record record ->
        (name = record.type_name || name = Type_id.name record.type_id)
        && List.length record.type_arguments = List.length arguments
        && List.for_all2 same_runtime_representation record.type_arguments
             arguments
    | _ -> false

let named_record_can_specialize expected actual =
  let arguments_can_specialize expected actual =
    List.length expected = List.length actual
    && List.for_all2
         (fun expected actual ->
           Result.is_ok (Type_solver.unify Type_solver.empty actual expected))
         expected actual
  in
  match (expected, actual) with
  | TNamed_record expected, TNamed_record actual ->
      Type_id.equal expected.type_id actual.type_id
      && arguments_can_specialize expected.type_arguments actual.type_arguments
  | _ -> false

let preserve_static_row_field expected actual =
  same_concrete_type expected actual
  || named_record_can_specialize expected actual
  ||
  match (expected, actual) with
  | (TUnknown | TMeta _ | TVar _), TNamed_record _ -> true
  | _ -> false

let dynamic_row_argument env type_name fields argument =
  let type_name = row_call_type_name type_name in
  let empty_dynamic_map =
    match Semantic_ir.unlocated argument.semantic_expr with
    | Semantic_ir.Apply
        ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.map",
          [ Semantic_ir.List [] ] ) ->
        true
    | _ -> false
  in
  let rec build values = function
    | [] -> Ok (List.rev values)
    | (field : field) :: rest
      when empty_dynamic_map && Types.is_record_extension_field field ->
        build
          ((field.ocaml_name, Semantic_ir.Ident "Lg_runtime.Runtime_map.empty")
          :: values)
          rest
    | (field : field) :: rest when empty_dynamic_map && is_optional_type field.ty
      ->
        build
          ((field.ocaml_name, Semantic_ir.Constructor ("None", None)) :: values)
          rest
    | (field : field) :: rest -> (
        let static_value =
          Option.bind argument.record_values (fun values ->
              List.find_opt
                (fun ((actual : field), _) ->
                  actual.keyword = field.keyword)
                values)
        in
        let value =
          if Types.is_record_extension_field field then
            let known_keys =
              fields
              |> List.filter (fun field ->
                     not (Types.is_record_extension_field field))
              |> List.map (fun field ->
                     Semantic_ir.Apply
                       ( Semantic_ir.Ident
                           "Lg_runtime.Runtime_dynamic.keyword",
                         [ Semantic_ir.String field.keyword ] ))
            in
            dynamic_unpack env field.ty
              (Semantic_ir.Apply
                 ( Semantic_ir.Ident
                     "Lg_runtime.Runtime_dynamic.map_without_keys",
                   [ argument.semantic_expr; Semantic_ir.List known_keys ] ))
          else
          match static_value with
          | Some (actual, expression)
            when preserve_static_row_field field.ty actual.ty ->
              Ok expression
          | Some _ | None ->
              let dynamic_value =
                Semantic_ir.Apply
                  ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.get",
                    [
                      argument.semantic_expr;
                      Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_dynamic.keyword",
                          [ Semantic_ir.String field.keyword ] );
                    ] )
              in
              dynamic_unpack env field.ty dynamic_value
        in
        match value with
        | Error _ as error -> error
        | Ok value -> build ((field.ocaml_name, value) :: values) rest)
  in
  Result.map
    (fun fields -> Semantic_ir.Record (fields, Some type_name))
    (build [] fields)

let unwrap_optional_argument expected argument =
  match optional_payload argument.ty with
  | None -> Ok argument.semantic_expr
  | Some actual_inner ->
      Ok
        (coerce_expression_to_type expected actual_inner
           (Semantic_ir.Apply
              (Semantic_ir.Ident "Option.get", [ argument.semantic_expr ])))

let pack_optional_dynamic_argument env expected argument =
  match optional_payload expected with
  | None -> Error.error "expected an optional dynamic argument"
  | Some expected_inner -> (
      match argument.ty with
      | TNil -> Ok (Semantic_ir.Constructor ("None", None))
      | actual when Types.is_dynamic actual ->
          Ok (Semantic_ir.Constructor ("Some", Some argument.semantic_expr))
      | TNullable actual | TOcaml_app ("option", [ actual ]) ->
          if Types.is_dynamic actual then Ok argument.semantic_expr
          else
            let value_name = "__lg_optional_argument" in
            let value = typed_ir actual (Semantic_ir.Ident value_name) in
            Result.map
              (fun packed ->
                Semantic_ir.Match
                  ( argument.semantic_expr,
                    [
                      ( Semantic_ir.PConstructor ("None", None),
                        Semantic_ir.Constructor ("None", None) );
                      ( Semantic_ir.PConstructor
                          ("Some", Some (Semantic_ir.PVar value_name)),
                        Semantic_ir.Constructor ("Some", Some packed) );
                    ] ))
              (pack_dynamic_value env expected_inner value)
      | _ ->
          Result.map
            (fun packed -> Semantic_ir.Constructor ("Some", Some packed))
            (pack_dynamic_value env expected_inner argument))

let host_int_boundary _left _right = false

let host_int_callback_parameter_boundary expected actual =
  host_int_boundary expected actual
  ||
  (Types.equal expected (TOcaml "int")
  && match actual with TUnknown | TMeta _ | TVar _ -> true | _ -> false)

let function_needs_type_adapter expected actual =
  match (expected, actual) with
  | TFn (expected_params, expected_return), TFn (actual_params, actual_return)
    when List.length expected_params = List.length actual_params ->
      List.exists2
        (fun expected actual ->
          not (Types.equal expected actual)
          || host_int_callback_parameter_boundary expected actual)
        expected_params actual_params
      || not (Types.equal expected_return actual_return)
      || host_int_boundary expected_return actual_return
  | _ -> false

let rec function_needs_representation_adapter expected actual =
  if has_capability_constraint expected <> has_capability_constraint actual then
    match (expected, actual) with
    | (TUnknown | TMeta _ | TVar _), _
    | _, (TUnknown | TMeta _ | TVar _) ->
        false
    | _ -> true
  else
    match (expected, actual) with
    | TFn (expected_params, expected_return), TFn (actual_params, actual_return)
      when List.length expected_params = List.length actual_params ->
        List.exists2 function_needs_representation_adapter expected_params
          actual_params
        || function_needs_representation_adapter expected_return actual_return
    | TOverloaded_fn expected_arities, TOverloaded_fn actual_arities
      when List.length expected_arities = List.length actual_arities ->
        List.exists2
          (fun expected actual ->
            List.length expected.fixed_params = List.length actual.fixed_params
            &&
            (List.exists2 function_needs_representation_adapter
               expected.fixed_params actual.fixed_params
            || function_needs_representation_adapter expected.return_ty
                 actual.return_ty))
          expected_arities actual_arities
    | _ -> false

let function_has_host_int_return_boundary expected actual =
  match (expected, actual) with
  | TFn (_, expected_return), TFn (_, actual_return) ->
      host_int_boundary expected_return actual_return
      ||
      (Types.equal expected_return (TOcaml "int")
      && match actual_return with TUnknown | TMeta _ | TVar _ -> true | _ -> false)
  | _ -> false

let record_adapter_source_name fallback expression =
  match Semantic_ir.unlocated expression with
  | Semantic_ir.Ident name -> name
  | _ -> fallback

let rec adapt_value_to_type env expected actual =
  let same_representation =
    match (expected, actual.ty) with
    | TSet expected_element, TSet actual_element ->
        same_set_storage_representation expected_element actual_element
    | _ -> true
  in
  if same_concrete_type expected actual.ty && same_representation then
    Ok actual.semantic_expr
  else if
    match (expected, actual.ty) with TSet _, TSet _ -> true | _ -> false
  then
    let expected_element, actual_element =
      match (expected, actual.ty) with
      | TSet expected_element, TSet actual_element ->
          (expected_element, actual_element)
      | _ -> assert false
    in
    Result.bind (Types.set_module_name expected_element) (fun expected_module ->
        Result.map
          (fun actual_module ->
            if String.equal expected_module actual_module then
              actual.semantic_expr
            else
              Semantic_ir.Apply
                ( Semantic_ir.Ident (expected_module ^ ".of_list"),
                  [
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident (actual_module ^ ".elements"),
                        [ actual.semantic_expr ] );
                  ] ))
          (Types.set_module_name actual_element))
  else if Types.equal expected (TOcaml "int") && Types.equal actual.ty TInt then
    Ok actual.semantic_expr
  else if
    Types.equal expected TUnit
    &&
    match actual.ty with
    | TNil -> true
    | TNullable TUnit | TOcaml_app ("option", [ TUnit ]) -> true
    | _ -> false
  then Ok (Semantic_ir.Sequence [ actual.semantic_expr; Semantic_ir.Unit ])
  else if
    Types.equal expected (TOcaml "int")
    &&
    match actual.ty with TUnknown | TMeta _ | TVar _ -> true | _ -> false
  then Ok actual.semantic_expr
  else if Types.equal expected TInt && Types.equal actual.ty (TOcaml "int") then
    Ok actual.semantic_expr
  else if
    match (expected, Types.dynamic_map_types actual.ty) with
    | TFn ([ _ ], _), Some _ -> true
    | _ -> false
  then
    let expected_key, expected_return, actual_key, actual_value =
      match (expected, Types.dynamic_map_types actual.ty) with
      | TFn ([ expected_key ], expected_return), Some (actual_key, actual_value)
        ->
          (expected_key, expected_return, actual_key, actual_value)
      | _ -> assert false
    in
    let map_name = "__lg_callable_map" in
    let key_name = "__lg_callable_map_key" in
    let map, wrap =
      match Semantic_ir.unlocated actual.semantic_expr with
      | Semantic_ir.Ident _ -> (actual.semantic_expr, Fun.id)
      | _ ->
          ( Semantic_ir.Ident map_name,
            fun function_ ->
              Semantic_ir.Let
                ([ (Semantic_ir.PVar map_name, actual.semantic_expr) ], function_)
          )
    in
    let key = typed_ir expected_key (Semantic_ir.Ident key_name) in
    Result.bind (adapt_value_to_type env actual_key key) (fun key ->
        let lookup =
          typed_ir (TNullable actual_value)
            (Semantic_ir.Apply
               ( Semantic_ir.Ident "Lg_runtime.Runtime_map.get_option",
                 [ map; key ] ))
        in
        Result.map
          (fun result ->
            wrap
              (Semantic_ir.Fun ([ Semantic_ir.PVar key_name ], result)))
          (adapt_value_to_type env expected_return lookup))
  else if
    match (expected, actual.ty) with
    | TFn (expected_params, _), TOverloaded_fn arities ->
        List.exists
          (fun arity ->
            Option.is_some
              (overloaded_arity_parameters arity (List.length expected_params)))
          arities
    | _ -> false
  then
    let expected_params, expected_return, arities =
      match (expected, actual.ty) with
      | TFn (expected_params, expected_return), TOverloaded_fn arities ->
          (expected_params, expected_return, arities)
      | _ -> assert false
    in
    let selected =
      arities
      |> List.mapi (fun index arity -> (index, arity))
      |> List.find_map (fun (index, arity) ->
             Option.map
               (fun parameters -> (index, arity, parameters))
               (overloaded_arity_parameters arity
                  (List.length expected_params)))
    in
    (match selected with
    | None -> Error.error "overloaded callback has no compatible arity"
    | Some (arity_index, arity, actual_params) ->
        let source_name = "__lg_overloaded_callback_adapter" in
        let source =
          match Semantic_ir.unlocated actual.semantic_expr with
          | Semantic_ir.Ident _ -> actual.semantic_expr
          | _ -> Semantic_ir.Ident source_name
        in
        let argument_names =
          List.mapi
            (fun index _ ->
              "__lg_overloaded_callback_argument_" ^ string_of_int index)
            expected_params
        in
        let rec adapt_arguments adapted expected actual names =
          match (expected, actual, names) with
          | [], [], [] -> Ok (List.rev adapted)
          | expected :: expected_rest, actual :: actual_rest, name :: names ->
              Result.bind
                (adapt_value_to_type env actual
                   (typed_ir expected (Semantic_ir.Ident name)))
                (fun argument ->
                  adapt_arguments (argument :: adapted) expected_rest
                    actual_rest names)
          | _ -> Error.error "overloaded callback argument mismatch"
        in
        Result.bind
          (adapt_arguments [] expected_params actual_params argument_names)
          (fun arguments ->
            let fixed_count = List.length arity.fixed_params in
            let fixed_arguments, rest_arguments =
              List.fold_left
                (fun (fixed, rest) (index, argument) ->
                  if index < fixed_count then (argument :: fixed, rest)
                  else (fixed, argument :: rest))
                ([], [])
                (List.mapi (fun index argument -> (index, argument)) arguments)
            in
            let call_arguments =
              List.rev fixed_arguments
              @
              match arity.rest_param with
              | None -> []
              | Some _ ->
                  [
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.of_list",
                        [ Semantic_ir.List (List.rev rest_arguments) ] );
                  ]
            in
            let result =
              typed_ir arity.return_ty
                (Semantic_ir.Apply
                   (witness_method source arity_index, call_arguments))
            in
            Result.map
              (fun body ->
                let adapter =
                  Semantic_ir.Fun
                    ( List.map
                        (fun name -> Semantic_ir.PVar name)
                        argument_names,
                      body )
                in
                match Semantic_ir.unlocated actual.semantic_expr with
                | Semantic_ir.Ident _ -> adapter
                | _ ->
                    Semantic_ir.Let
                      ( [ (Semantic_ir.PVar source_name, actual.semantic_expr) ],
                        adapter ))
              (adapt_value_to_type env expected_return result)))
  else if
    match (expected, actual.ty) with
    | TOverloaded_fn expected_arities, TOverloaded_fn actual_arities ->
        List.length expected_arities = List.length actual_arities
        && function_needs_representation_adapter expected actual.ty
    | _ -> false
  then
    let expected_arities, actual_arities =
      match (expected, actual.ty) with
      | TOverloaded_fn expected_arities, TOverloaded_fn actual_arities ->
          (expected_arities, actual_arities)
      | _ -> assert false
    in
    let stored_function_type (arity : fn_arity) =
      TFn
        ( arity.fixed_params
          @ Option.fold ~none:[] ~some:(fun rest -> [ TSeq rest ])
              arity.rest_param,
          arity.return_ty )
    in
    let source_name = "__lg_overloaded_function_adapter" in
    let source =
      match Semantic_ir.unlocated actual.semantic_expr with
      | Semantic_ir.Ident _ -> actual.semantic_expr
      | _ -> Semantic_ir.Ident source_name
    in
    let rec adapt_arities index adapted expected actual =
      match (expected, actual) with
      | [], [] -> Ok (witness_storage (List.rev adapted))
      | expected :: expected_rest, actual :: actual_rest ->
          let selected =
            typed_ir (stored_function_type actual)
              (witness_method source index)
          in
          Result.bind
            (adapt_value_to_type env (stored_function_type expected) selected)
            (fun adapted_arity ->
              adapt_arities (index + 1) (adapted_arity :: adapted)
                expected_rest actual_rest)
      | _ -> Error.error "internal overloaded function adapter mismatch"
    in
    Result.map
      (fun adapted ->
        match Semantic_ir.unlocated actual.semantic_expr with
        | Semantic_ir.Ident _ -> adapted
        | _ ->
            Semantic_ir.Let
              ([ (Semantic_ir.PVar source_name, actual.semantic_expr) ], adapted))
      (adapt_arities 0 [] expected_arities actual_arities)
  else if function_needs_type_adapter expected actual.ty then
    match (expected, actual.ty) with
    | TFn (expected_params, expected_return), TFn (actual_params, actual_return)
      ->
        let callable, wrap_adapter =
          match Semantic_ir.unlocated actual.semantic_expr with
          | Semantic_ir.Ident _ -> (actual.semantic_expr, Fun.id)
          | _ ->
              incr function_adapter_counter;
              let name =
                "__lg_adapted_function_"
                ^ string_of_int !function_adapter_counter
              in
              ( Semantic_ir.Ident name,
                fun adapter ->
                  Semantic_ir.Let
                    ([ (Semantic_ir.PVar name, actual.semantic_expr) ], adapter)
              )
        in
        let argument_names =
          List.mapi
            (fun index _ -> "__lg_callback_argument_" ^ string_of_int index)
            expected_params
        in
        let rec adapt_arguments adapted expected actual names =
          match (expected, actual, names) with
          | [], [], [] -> Ok (List.rev adapted)
          | expected :: expected_rest, actual :: actual_rest, name :: names ->
              let argument = typed_ir expected (Semantic_ir.Ident name) in
              let actual =
                if
                  Types.equal expected (TOcaml "int")
                  && match actual with TUnknown | TMeta _ | TVar _ -> true | _ -> false
                then TInt
                else actual
              in
              Result.bind (adapt_value_to_type env actual argument)
                (fun argument ->
                  adapt_arguments (argument :: adapted) expected_rest
                    actual_rest names)
          | _ -> Error.error "internal callback argument mismatch"
        in
        Result.bind
          (adapt_arguments [] expected_params actual_params argument_names)
          (fun arguments ->
            let result =
              typed_ir actual_return
                (Semantic_ir.Apply (callable, arguments))
            in
            Result.map
              (fun result ->
                wrap_adapter
                  (Semantic_ir.Fun
                     ( List.map
                         (fun name -> Semantic_ir.PVar name)
                         argument_names,
                       result )))
              (adapt_value_to_type env expected_return result))
    | _ -> assert false
  else if Types.is_dynamic expected then pack_dynamic_value env expected actual
  else if Types.is_dynamic actual.ty then
    dynamic_unpack env expected actual.semantic_expr
  else if
    match (expected, actual.ty) with
    | TNamed_record expected, TNamed_record actual
      when Type_id.equal expected.type_id actual.type_id ->
        Types.equal (TNamed_record expected) (TNamed_record actual)
        && same_runtime_representation (TNamed_record expected)
          (TNamed_record actual)
        && not
          (named_record_has_capability_fields expected
          || named_record_has_capability_fields actual)
    | _ -> false
  then Ok actual.semantic_expr
  else if
    match (expected, Types.record_fields actual.ty) with
    | TRecord _, Some _ -> true
    | _ -> false
  then (
    let expected_fields =
      match expected with TRecord fields -> fields | _ -> assert false
    in
    let actual_fields = Types.record_fields actual.ty |> Option.get in
    let source_name =
      record_adapter_source_name "__lg_adapted_structural_record"
        actual.semantic_expr
    in
    let source =
      {
        actual with
        semantic_expr = Semantic_ir.Ident source_name;
        record_values = None;
      }
    in
    let rec adapt_fields values = function
      | [] -> Ok (List.rev values)
      | (expected_field : field) :: rest -> (
          match find_field expected_field.keyword actual_fields with
          | None when is_optional_type expected_field.ty ->
              adapt_fields
                ( ( expected_field,
                    Semantic_ir.Constructor ("None", None) )
                :: values )
                rest
          | None ->
              Error.error
                ("record argument is missing field " ^ expected_field.keyword)
          | Some actual_field ->
              let value =
                match actual.record_values with
                | Some values -> (
                    match
                      List.find_opt
                        (fun ((field : field), _) ->
                          field.keyword = actual_field.keyword)
                        values
                    with
                    | Some (_, value) -> typed_ir actual_field.ty value
                    | None ->
                        typed_ir actual_field.ty
                          (Structural_map.field_expr source actual_field))
                | None ->
                    typed_ir actual_field.ty
                      (Structural_map.field_expr source actual_field)
              in
              let adapted = adapt_value_to_type env expected_field.ty value in
              Result.bind adapted (fun value ->
                  adapt_fields ((expected_field, value) :: values) rest))
    in
    Result.map
      (fun values ->
        let converted =
          Structural_map.record_expr expected_fields values
          |> fun value -> value.semantic_expr
        in
        match (actual.record_values, Semantic_ir.unlocated actual.semantic_expr) with
        | Some _, _ -> converted
        | None, Semantic_ir.Ident _ -> converted
        | _ ->
            Semantic_ir.Let
              ( [
                  ( Semantic_ir.PVar source_name,
                    actual.semantic_expr );
                ],
                converted ))
      (adapt_fields [] expected_fields))
  else if
    match (expected, actual.ty) with
    | TNamed_record expected, TNamed_record actual ->
        Type_id.equal expected.type_id actual.type_id || not expected.nominal
    | TNamed_record expected, actual ->
        (not expected.nominal) && Option.is_some (Types.record_fields actual)
    | _ -> false
  then (
    let expected_record =
      match expected with TNamed_record expected -> expected | _ -> assert false
    in
    let actual_fields = Types.record_fields actual.ty |> Option.get in
    let source_name =
      record_adapter_source_name "__lg_adapted_record" actual.semantic_expr
    in
    let source =
      {
        actual with
        semantic_expr = Semantic_ir.Ident source_name;
        record_values = None;
      }
    in
    let rec adapt_fields values = function
      | [] -> Ok (List.rev values)
      | (expected_field : field) :: rest -> (
          match find_field expected_field.keyword actual_fields with
          | None when is_optional_type expected_field.ty ->
              adapt_fields
                ( ( expected_field,
                    Semantic_ir.Constructor ("None", None) )
                :: values )
                rest
          | None ->
              Error.error
                ("record argument is missing field " ^ expected_field.keyword)
          | Some actual_field ->
              let value =
                match actual.record_values with
                | Some values -> (
                    match
                      List.find_opt
                        (fun ((field : field), _) ->
                          field.keyword = actual_field.keyword)
                        values
                    with
                    | Some (_, value) -> typed_ir actual_field.ty value
                    | None ->
                        typed_ir actual_field.ty
                          (Structural_map.field_expr source actual_field))
                | None ->
                    typed_ir actual_field.ty
                      (Structural_map.field_expr source actual_field)
              in
              let adapted =
                if
                  Option.is_none (optional_payload expected_field.ty)
                  && Option.fold ~none:false
                       ~some:(argument_compatible expected_field.ty)
                       (optional_payload value.ty)
                then
                  let actual_inner = optional_payload value.ty |> Option.get in
                  Ok
                    (coerce_expression_to_type expected_field.ty actual_inner
                       (Semantic_ir.Apply
                          ( Semantic_ir.Ident "Option.get",
                            [ value.semantic_expr ] )))
                else adapt_value_to_type env expected_field.ty value
              in
              Result.bind
                adapted
                (fun value ->
                  adapt_fields ((expected_field, value) :: values) rest))
    in
    Result.map
      (fun values ->
        let converted =
          Structural_map.named_record_expr expected_record values
          |> fun value -> value.semantic_expr
        in
        match (actual.record_values, Semantic_ir.unlocated actual.semantic_expr) with
        | Some _, _ -> converted
        | None, Semantic_ir.Ident _ -> converted
        | _ ->
            Semantic_ir.Let
              ( [
                  ( Semantic_ir.PVar source_name,
                    actual.semantic_expr );
                ],
                converted ))
      (adapt_fields [] expected_record.fields))
  else if
    Option.is_some (Types.seqable_constraint_element expected)
    && Option.is_some (Collection_capability.element_type env actual)
    && not
         (Types.equal
            (Types.seqable_constraint_element expected |> Option.get)
            (Collection_capability.element_type env actual |> Option.get))
  then
    let expected_element =
      Types.seqable_constraint_element expected |> Option.get
      |> resolve_named_record_application env
      |> Collection_capability.resolve_callback_record env
    in
    let actual_value_ty =
      Types.constraint_value_type actual.ty
      |> Collection_capability.resolve_callback_record env
    in
    let actual_value =
      if Types.equal actual_value_ty actual.ty then actual
      else
        {
          actual with
          ty = actual_value_ty;
          semantic_expr = constrained_argument_value actual;
        }
    in
    let expected_value_ty =
      match actual_value_ty with
      | TVector _ -> TVector expected_element
      | TList _ -> TList expected_element
      | TArray _ -> TArray expected_element
      | TSet _ | TSeq _ -> TSeq expected_element
      | _ -> TSeq expected_element
    in
    Result.bind
      (adapt_value_to_type env expected_value_ty actual_value)
      (fun mapped ->
        pack_constrained_value env expected
          (typed_ir expected_value_ty mapped))
  else if
    match (optional_payload expected, optional_payload actual.ty) with
    | Some expected_inner, Some _ -> has_capability_constraint expected_inner
    | _ -> false
  then pack_constrained_value env expected actual
  else if has_capability_constraint expected then
    pack_constrained_value env expected actual
  else if
    Option.is_some (optional_payload expected)
    && Option.is_none (optional_payload actual.ty)
  then
    match optional_payload expected with
    | Some _ when Types.equal actual.ty TNil ->
        Ok (Semantic_ir.Constructor ("None", None))
    | Some expected_inner ->
        Result.map
          (fun value -> Semantic_ir.Constructor ("Some", Some value))
          (adapt_value_to_type env expected_inner actual)
    | None -> assert false
  else if
    has_capability_constraint actual.ty
    && Types.equal expected (Types.constraint_value_type actual.ty)
  then Ok (constrained_argument_value actual)
  else if
    same_runtime_representation expected actual.ty
    && not
         (match (expected, actual.ty) with
         | (TSeq expected_item, TSeq actual_item)
         | (TVector expected_item, TVector actual_item)
         | (TList expected_item, TList actual_item)
         | (TArray expected_item, TArray actual_item) ->
             not (Types.equal expected_item actual_item)
             && not
                  (same_runtime_representation expected_item actual_item)
         | _ -> false)
  then
    Ok actual.semantic_expr
  else if
    Option.is_none (optional_payload expected)
    && Option.fold ~none:false ~some:Types.is_dynamic
         (optional_payload actual.ty)
  then
    dynamic_unpack env expected
      (Semantic_ir.Apply
         (Semantic_ir.Ident "Option.get", [ actual.semantic_expr ]))
  else if
    match (expected, actual.ty) with
    | TVector _, TTuple (_ :: _) -> true
    | _ -> false
  then
    let expected_item, actual_items =
      match (expected, actual.ty) with
      | TVector expected_item, TTuple actual_items ->
          (expected_item, actual_items)
      | _ -> assert false
    in
    let item_names =
      List.mapi
        (fun index _ -> "__lg_adapt_map_entry_" ^ string_of_int index)
        actual_items
    in
    let rec adapt_items adapted tys names =
      match (tys, names) with
      | [], [] -> Ok (List.rev adapted)
      | ty :: tys, name :: names ->
          let item = typed_ir ty (Semantic_ir.Ident name) in
          Result.bind (adapt_value_to_type env expected_item item)
            (fun item -> adapt_items (item :: adapted) tys names)
      | _ -> assert false
    in
    Result.map
      (fun items ->
        Semantic_ir.Let
          ( [
              ( Semantic_ir.PTuple
                  (List.map (fun name -> Semantic_ir.PVar name) item_names),
                actual.semantic_expr );
            ],
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Rrbvec.of_list",
                [ Semantic_ir.List items ] ) ))
              (adapt_items [] actual_items item_names)
  else if
    match (expected, actual.ty) with
    | TSeq _, TSet _ -> true
    | _ -> false
  then
    let expected_item =
      match expected with TSeq item -> item | _ -> assert false
    in
    Result.bind (Collection_capability.to_seq_expr env actual)
      (fun (actual_item, sequence) ->
        let item_name = "__lg_adapt_set_item" in
        let item = typed_ir actual_item (Semantic_ir.Ident item_name) in
        Result.map
          (fun item ->
            match Semantic_ir.unlocated item with
            | Semantic_ir.Ident name when String.equal name item_name -> sequence
            | _ ->
                Semantic_ir.Apply
                  ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                    [
                      Semantic_ir.Fun ([ Semantic_ir.PVar item_name ], item);
                      sequence;
                    ] ))
          (adapt_value_to_type env expected_item item))
  else if
    match (expected, actual.ty) with
    | ( TSeq expected_item,
        TSeq actual_item )
    | ( TVector expected_item,
        TVector actual_item )
    | ( TList expected_item,
        TList actual_item )
    | ( TArray expected_item,
        TArray actual_item ) ->
        not (Types.equal expected_item actual_item)
    | _ -> false
  then
    let expected_item, actual_item, map_name =
      match (expected, actual.ty) with
      | TSeq expected_item, TSeq actual_item ->
          (expected_item, actual_item, "Lg_runtime.Runtime_seq.map")
      | TVector expected_item, TVector actual_item ->
          (expected_item, actual_item, "Rrbvec.map")
      | TList expected_item, TList actual_item ->
          (expected_item, actual_item, "List.map")
      | TArray expected_item, TArray actual_item ->
          (expected_item, actual_item, "Array.map")
      | _ -> assert false
    in
    let item_name = "__lg_adapt_collection_item" in
    let item = typed_ir actual_item (Semantic_ir.Ident item_name) in
    Result.map
      (fun item ->
        match Semantic_ir.unlocated item with
        | Semantic_ir.Ident name when String.equal name item_name ->
            actual.semantic_expr
        | _ ->
            Semantic_ir.Apply
              ( Semantic_ir.Ident map_name,
                [ Semantic_ir.Fun ([ Semantic_ir.PVar item_name ], item);
                  actual.semantic_expr;
                ] ))
      (adapt_value_to_type env expected_item item)
  else
    match (optional_payload expected, optional_payload actual.ty) with
    | Some expected_inner, Some actual_inner ->
        let value_name = "__lg_adapt_optional_value" in
        let value = typed_ir actual_inner (Semantic_ir.Ident value_name) in
        Result.map
          (fun value ->
            match Semantic_ir.unlocated value with
            | Semantic_ir.Ident name when String.equal name value_name ->
                actual.semantic_expr
            | _ ->
                Semantic_ir.Match
                  ( actual.semantic_expr,
                    [
                      ( Semantic_ir.PConstructor ("None", None),
                        Semantic_ir.Constructor ("None", None) );
                      ( Semantic_ir.PConstructor
                          ("Some", Some (Semantic_ir.PVar value_name)),
                        Semantic_ir.Constructor ("Some", Some value) );
                    ] ))
          (adapt_value_to_type env expected_inner value)
    | _ -> (
      match
        (Types.dynamic_map_types expected, Types.dynamic_map_types actual.ty)
      with
    | Some (expected_key, expected_value), None
      when Option.is_some (Types.record_fields actual.ty) ->
        let actual_fields = Types.record_fields actual.ty |> Option.get in
        let source_name =
          record_adapter_source_name "__lg_adapted_map_record"
            actual.semantic_expr
        in
        let source =
          {
            actual with
            semantic_expr = Semantic_ir.Ident source_name;
            record_values = None;
          }
        in
        let values =
          match actual.record_values with
          | Some values -> values
          | None -> Structural_map.values_for source actual_fields
        in
        let rec adapt_entries entries = function
          | [] -> Ok (List.rev entries)
          | ((field : field), expression) :: rest ->
              let key = typed_ir TKeyword (Semantic_ir.String field.keyword) in
              let value = typed_ir field.ty expression in
              Result.bind (adapt_value_to_type env expected_key key) (fun key ->
                  Result.bind
                    (adapt_value_to_type env expected_value value)
                    (fun value ->
                      adapt_entries
                        (Semantic_ir.Tuple [ key; value ] :: entries)
                        rest))
        in
        Result.map
          (fun entries ->
            let converted =
              Semantic_ir.Apply
                ( Semantic_ir.Ident
                    (if expects_dynamic_value expected_key then
                       "Lg_runtime.Runtime_map.of_list_dynamic"
                     else "Lg_runtime.Runtime_map.of_list"),
                  [ Semantic_ir.List entries ] )
            in
            match
              (actual.record_values, Semantic_ir.unlocated actual.semantic_expr)
            with
            | Some _, _ | None, Semantic_ir.Ident _ -> converted
            | None, _ ->
                Semantic_ir.Let
                  ([ (Semantic_ir.PVar source_name, actual.semantic_expr) ], converted))
          (adapt_entries [] values)
    | Some (expected_key, expected_value), Some (actual_key, actual_value) ->
        let key_name = "__lg_adapt_map_key" in
        let value_name = "__lg_adapt_map_value" in
        let key = typed_ir actual_key (Semantic_ir.Ident key_name) in
        let value = typed_ir actual_value (Semantic_ir.Ident value_name) in
        Result.bind (adapt_value_to_type env expected_key key) (fun key ->
            Result.map
              (fun value ->
                if
                  is_identity_conversion key_name key
                  && is_identity_conversion value_name value
                then actual.semantic_expr
                else
                  let entries =
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "List.map",
                        [
                          Semantic_ir.Fun
                            ( [
                                Semantic_ir.PTuple
                                  [
                                    Semantic_ir.PVar key_name;
                                    Semantic_ir.PVar value_name;
                                  ];
                              ],
                              Semantic_ir.Tuple [ key; value ] );
                          Semantic_ir.Apply
                            ( Semantic_ir.Ident
                                "Lg_runtime.Runtime_map.to_list",
                              [ actual.semantic_expr ] );
                        ] )
                  in
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident
                        (if expects_dynamic_value expected_key then
                           "Lg_runtime.Runtime_map.of_list_dynamic"
                         else "Lg_runtime.Runtime_map.of_list"),
                      [ entries ] ))
              (adapt_value_to_type env expected_value value))
    | _ ->
        Ok
          (coerce_expression_to_type expected actual.ty actual.semantic_expr))

let static_iequiv scope env ty =
  let ty = Types.constraint_value_type ty in
  match Protocol.lookup_protocol_marker scope env "IEquiv" "-equiv" with
  | None -> None
  | Some marker -> Protocol.lookup_marker_impl env marker "-equiv" ty

type static_deftype_callable = {
  callable_record : named_record;
  callable_implementation : binding;
  callable_optional : bool;
}

let static_deftype_callable env ty arity =
  let implementation record =
    let method_scope = String.concat "/" (Type_id.owner record.type_id) in
    match lookup_deftype_method method_scope env record "-invoke" arity with
    | Ok implementation -> Some implementation
    | Error _ -> None
  in
  let unique_candidates expected_fields =
    Env.filter_record_bindings
      (fun key (binding : binding) ->
        if String.starts_with ~prefix:"__record/" key then
          match binding.ty with
          | TNamed_record record
            when Option.fold ~none:true
                   ~some:(fun fields ->
                     Types.row_compatible ~expected:(TRecord fields)
                       ~actual:(TNamed_record record))
                   expected_fields ->
              let callable = implementation record in
              Option.map
                (fun implementation -> (record, implementation))
                callable
          | _ -> None
        else None)
      env
    |> List.fold_left
         (fun candidates ((record, _) as candidate) ->
           if
             List.exists
               (fun (existing, _) ->
                 Type_id.equal existing.type_id record.type_id)
               candidates
           then candidates
           else candidate :: candidates)
         []
  in
  let rec resolve optional = function
    | TNullable inner | TOcaml_app ("option", [ inner ]) ->
        resolve true inner
    | TNamed_record record ->
        let callable = implementation record in
        Option.map
          (fun callable_implementation ->
            {
              callable_record = record;
              callable_implementation;
              callable_optional = optional;
            })
          callable
    | TRecord fields -> (
        match unique_candidates (Some fields) with
        | [ (callable_record, callable_implementation) ] ->
            Some
              {
                callable_record;
                callable_implementation;
                callable_optional = optional;
              }
        | [] | _ :: _ :: _ -> None)
    | TMap_keys -> (
        match unique_candidates None with
        | [ (callable_record, callable_implementation) ] ->
            Some
              {
                callable_record;
                callable_implementation;
                callable_optional = optional;
              }
        | [] | _ :: _ :: _ -> None)
    | _ -> None
  in
  resolve false (Types.constraint_value_type ty)

let rec compile_record_iequiv_pair scope env left right =
  match (optional_payload left.ty, optional_payload right.ty) with
  | Some (TNamed_record left_record), Some (TNamed_record right_record)
    when Type_id.equal left_record.type_id right_record.type_id ->
      let left_name = "__lg_optional_iequiv_left" in
      let right_name = "__lg_optional_iequiv_right" in
      let left_value =
        typed_ir (TNamed_record left_record) (Semantic_ir.Ident left_name)
      in
      let right_value =
        typed_ir (TNamed_record right_record) (Semantic_ir.Ident right_name)
      in
      Result.map
        (Option.map (fun equal ->
             Semantic_ir.Match
               ( Semantic_ir.Tuple
                   [ left.semantic_expr; right.semantic_expr ],
                 [
                   ( Semantic_ir.PTuple
                       [
                         Semantic_ir.PConstructor ("None", None);
                         Semantic_ir.PConstructor ("None", None);
                       ],
                     Semantic_ir.Bool true );
                   ( Semantic_ir.PTuple
                       [
                         Semantic_ir.PConstructor
                           ("Some", Some (Semantic_ir.PVar left_name));
                         Semantic_ir.PConstructor
                           ("Some", Some (Semantic_ir.PVar right_name));
                       ],
                     equal );
                   (Semantic_ir.PAny, Semantic_ir.Bool false);
                 ] )))
        (compile_record_iequiv_pair scope env left_value right_value)
  | _ ->
  let unwrap_constraint argument =
    let value_ty =
      Types.constraint_value_type argument.ty
      |> resolve_named_record_application env
    in
    if Types.equal value_ty argument.ty then argument
    else
      {
        argument with
        ty = value_ty;
        semantic_expr = constrained_argument_value argument;
      }
  in
  let left = unwrap_constraint left in
  let right = unwrap_constraint right in
  let adapt expected argument =
    if has_capability_constraint expected then
      pack_constrained_value env expected argument
    else adapt_value_to_type env expected argument
  in
  match static_iequiv scope env left.ty with
  | Some { ty = TFn ([ left_ty; right_ty ], TBool); ocaml_name; _ } ->
      Result.bind (adapt left_ty left) (fun left ->
          Result.map
            (fun right ->
              Some
                (Semantic_ir.Apply
                   (Semantic_ir.Ident ocaml_name, [ left; right ])))
            (adapt right_ty right))
  | Some _ -> Error.error "IEquiv/-equiv has an invalid signature"
  | None -> Ok None

let compile_equality scope env args =
  let compile_pair left right =
    let fallback () = Core_compare.compile ~env "=" [ left; right ] in
    let metadata_pair metadata other =
      match pack_metadata_expression other.ty other.semantic_expr with
      | Error _ -> fallback ()
      | Ok other ->
          Ok
            (typed_ir TBool
               (Semantic_ir.Infix
                  ("=", metadata.semantic_expr, other)))
    in
    let dynamic_pair dynamic other =
      match pack_dynamic_value env dynamic.ty other with
      | Error _ -> fallback ()
      | Ok other ->
          Ok
            (typed_ir TBool
               (Semantic_ir.Apply
                  ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.equal",
                    [ dynamic.semantic_expr; other ] )))
    in
    let result =
      if
        is_edn_value_type left.ty && is_edn_value_type right.ty
      then
        Ok
          (typed_ir TBool
             (Semantic_ir.Apply
                ( Semantic_ir.Ident "Lg_runtime.Runtime_edn.equal",
                  [ left.semantic_expr; right.semantic_expr ] )))
      else if Types.equal left.ty (TOcaml "Lg_edn_backend.t")
         && not (Types.equal right.ty (TOcaml "Lg_edn_backend.t"))
      then metadata_pair left right
      else if Types.equal right.ty (TOcaml "Lg_edn_backend.t")
              && not (Types.equal left.ty (TOcaml "Lg_edn_backend.t"))
      then metadata_pair right left
      else if Types.is_dynamic left.ty && not (Types.is_dynamic right.ty) then
        dynamic_pair left right
      else if Types.is_dynamic right.ty && not (Types.is_dynamic left.ty) then
        dynamic_pair right left
      else fallback ()
    in
    result
  in
  let rec pairs expressions = function
    | left :: ((right :: _) as rest) ->
        Result.bind (compile_record_iequiv_pair scope env left right)
          (function
            | Some expression -> pairs (expression :: expressions) rest
            | None ->
                Result.bind (compile_pair left right)
                  (fun expression ->
                    pairs (expression.semantic_expr :: expressions) rest))
    | _ -> Ok (List.rev expressions)
  in
  match args with
  | [] | [ _ ] -> Ok (typed_ir TBool (Semantic_ir.Bool true))
  | _ ->
      Result.map
        (fun expressions ->
          let equal = Core_compare.and_expressions expressions in
          typed_ir TBool equal)
        (pairs [] args)

let adapt_protocol_witness_result env ~expected ~actual expression =
  adapt_value_to_type env expected (typed_ir actual expression)
  |> Result.map (fun expression -> typed_ir expected expression)

let adapt_record_values_to_map env key_ty value_ty values =
  let rec build map = function
    | [] -> Ok map
    | ((field : field), value) :: rest ->
        let key =
          typed_ir TKeyword (Semantic_ir.String field.keyword)
        in
        let value = typed_ir field.ty value in
        Result.bind (adapt_value_to_type env key_ty key) (fun key ->
            Result.bind (adapt_value_to_type env value_ty value) (fun value ->
                build
                  (Semantic_ir.Apply
                     ( Semantic_ir.Ident "Lg_runtime.Runtime_map.assoc",
                       [ map; key; value ] ))
                  rest))
  in
  build (Semantic_ir.Ident "Lg_runtime.Runtime_map.empty") values

let rec row_argument_compatible expected_fields actual_ty =
  match actual_ty with
  | ty when Types.is_dynamic ty -> true
  | TNullable actual_ty | TOcaml_app ("option", [ actual_ty ]) ->
      row_argument_compatible expected_fields actual_ty
  | TRecord actual_fields | TNamed_record { fields = actual_fields; _ } ->
      List.for_all
        (fun (expected : field) ->
          if Types.is_record_extension_field expected then true
          else
            match find_field expected.keyword actual_fields with
            | None -> is_optional_type expected.ty
            | Some actual ->
                Types.assignable ~policy:Host_boundary ~expected:expected.ty
                  ~actual:actual.ty)
        expected_fields
  | TNil -> true
  | _ -> false

let typed_row_argument_unshared env type_name expected_fields argument =
  let type_name = row_call_type_name type_name in
  let runtime_map_row () =
    match Types.dynamic_map_types argument.ty with
    | Some (key_ty, value_ty) when Types.is_dynamic value_ty ->
        let key_ty =
          match key_ty with
          | TUnknown | TMeta _ | TVar _ -> Types.dynamic_constraint TUnknown
          | ty -> ty
        in
        let rec build fields = function
          | [] -> Ok (List.rev fields)
          | (field : field) :: rest ->
              if Types.is_record_extension_field field then
                Error.error
                  "cannot project a runtime map into an open row"
              else
                let key =
                  typed_ir TKeyword (Semantic_ir.String field.keyword)
                in
                let key =
                  if Types.is_dynamic key_ty then
                    pack_dynamic_value env key_ty key
                  else Ok key.semantic_expr
                in
                Result.bind key (fun key ->
                    let operation =
                      runtime_map_lookup_operation key_ty key_ty "get_default"
                    in
                    let value =
                      typed_ir value_ty
                        (Semantic_ir.Apply
                           ( Semantic_ir.Ident operation,
                             [
                               argument.semantic_expr;
                               key;
                               Semantic_ir.Ident
                                 "Lg_runtime.Runtime_dynamic.nil";
                             ] ))
                    in
                    let value =
                      if has_capability_constraint field.ty then
                        pack_constrained_value env field.ty value
                      else adapt_value_to_type env field.ty value
                    in
                    Result.bind value (fun value ->
                        build ((field.ocaml_name, value) :: fields) rest))
        in
        Result.map
          (fun fields -> Semantic_ir.Record (fields, Some type_name))
          (build [] expected_fields)
        |> Option.some
    | _ -> None
  in
  match runtime_map_row () with
  | Some result -> result
  | None ->
  let actual_fields =
    match argument.record_values with
    | Some values -> Some (List.map fst values)
    | None
      when Option.is_some (Types.dynamic_map_types argument.ty)
           &&
           (match Semantic_ir.unlocated argument.semantic_expr with
           | Semantic_ir.Ident "Lg_runtime.Runtime_map.empty" -> true
           | _ -> false) ->
        Some []
    | None -> (
        match argument.ty with
        | TRecord fields | TNamed_record { fields; _ } -> Some fields
        | TOcaml_app _ -> Some expected_fields
        | _ -> None)
  in
  match actual_fields with
  | None -> Ok argument.semantic_expr
  | Some actual_fields ->
      let extension_value () =
        let extension_field = Types.find_record_extension_field actual_fields in
        let initial =
          match extension_field with
          | Some field -> Structural_map.field_expr argument field
          | None -> Semantic_ir.Ident "Lg_runtime.Runtime_map.empty"
        in
        let expected_keywords =
          expected_fields
          |> List.filter (fun field ->
                 not (Types.is_record_extension_field field))
          |> List.map (fun field -> field.keyword)
        in
        let extra_fields =
          actual_fields
          |> List.filter (fun field ->
                 (not (Types.is_record_extension_field field))
                 && not (List.mem field.keyword expected_keywords))
        in
        let rec add_extra map = function
          | [] -> Ok map
          | (field : field) :: rest ->
              let value =
                typed_ir field.ty (Structural_map.field_expr argument field)
              in
              Result.bind
                (pack_dynamic_value env
                   (Types.dynamic_constraint TUnknown)
                   value)
                (fun value ->
                  add_extra
                    (Semantic_ir.Apply
                       ( Semantic_ir.Ident "Lg_runtime.Runtime_map.assoc",
                         [ map; Semantic_ir.String field.keyword; value ] ))
                    rest)
        in
        add_extra initial extra_fields
      in
      let rec build values = function
        | [] -> Ok (List.rev values)
        | (expected : field) :: rest -> (
            if Types.is_record_extension_field expected then
              let extension =
                if Types.is_static_record_source_field expected then
                  adapt_value_to_type env expected.ty argument
                else extension_value ()
              in
              Result.bind extension (fun value ->
                  build ((expected.ocaml_name, value) :: values) rest)
            else
              match find_field expected.keyword actual_fields with
            | None when is_optional_type expected.ty ->
                build
                  ((expected.ocaml_name, Semantic_ir.Constructor ("None", None))
                  :: values)
                  rest
            | None ->
                Error.error
                  ("record argument is missing field " ^ expected.keyword)
            | Some actual ->
                let actual_value =
                  typed_ir actual.ty (Structural_map.field_expr argument actual)
                in
                let value = adapt_value_to_type env expected.ty actual_value in
                Result.bind value (fun value ->
                    build ((expected.ocaml_name, value) :: values) rest))
      in
      Result.map
        (fun fields -> Semantic_ir.Record (fields, Some type_name))
        (build [] expected_fields)

let typed_row_argument env type_name expected_fields argument =
  match optional_payload argument.ty with
  | Some value_ty ->
      let value_ty =
        match value_ty with
        | TUnknown | TMeta _ | TVar _ -> TRecord expected_fields
        | value_ty -> value_ty
      in
      incr row_argument_counter;
      let value_name =
        "__lg_row_argument_" ^ string_of_int !row_argument_counter
      in
      let value =
        {
          argument with
          ty = value_ty;
          semantic_expr = Semantic_ir.Ident value_name;
          record_values = None;
        }
      in
      Result.map
        (fun projected ->
          Semantic_ir.Let
            ( [
                ( Semantic_ir.PVar value_name,
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident "Option.get",
                      [ argument.semantic_expr ] ) );
              ],
              projected ))
        (typed_row_argument_unshared env type_name expected_fields value)
  | None -> (
      match
        (Semantic_ir.unlocated argument.semantic_expr, argument.record_values)
      with
      | Semantic_ir.Ident _, _ | _, Some _ ->
          typed_row_argument_unshared env type_name expected_fields argument
      | _ ->
      incr row_argument_counter;
      let argument_name =
        "__lg_row_argument_" ^ string_of_int !row_argument_counter
      in
      let stable_argument =
        {
          argument with
          semantic_expr = Semantic_ir.Ident argument_name;
          record_values = None;
        }
      in
      Result.map
        (fun projected ->
          Semantic_ir.Let
            ( [ (Semantic_ir.PVar argument_name, argument.semantic_expr) ],
              projected ))
        (typed_row_argument_unshared env type_name expected_fields
           stable_argument))

let typed_nullable_row_argument env type_name expected_fields argument =
  match argument.ty with
  | TNil -> Ok (Semantic_ir.Constructor ("None", None))
  | ty when Types.is_dynamic ty ->
      Result.map
        (fun row ->
          Semantic_ir.If
            ( Semantic_ir.Apply
                ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.is_nil",
                  [ argument.semantic_expr ] ),
              Semantic_ir.Constructor ("None", None),
              Semantic_ir.Constructor ("Some", Some row) ))
        (dynamic_row_argument env type_name expected_fields argument)
  | TNullable value_ty | TOcaml_app ("option", [ value_ty ]) ->
      let value_name = "__lg_nullable_row_value" in
      let value = typed_ir value_ty (Semantic_ir.Ident value_name) in
      Result.map
        (fun row ->
          Semantic_ir.Match
            ( argument.semantic_expr,
              [ ( Semantic_ir.PConstructor ("None", None),
                  Semantic_ir.Constructor ("None", None) );
                ( Semantic_ir.PConstructor
                    ("Some", Some (Semantic_ir.PVar value_name)),
                  Semantic_ir.Constructor ("Some", Some row) );
              ] ))
        (typed_row_argument env type_name expected_fields value)
  | _ ->
      Result.map
        (fun row -> Semantic_ir.Constructor ("Some", Some row))
        (typed_row_argument env type_name expected_fields argument)

let adapt_reduced_callback arg =
  match arg.ty with
  | TFn (params, return_type) -> (
      match Types.reduced_element return_type with
      | Some _ ->
          let parameter_names =
            List.mapi
              (fun index _ -> "__lg_callback_arg_" ^ string_of_int index)
              params
          in
          let result_name = "__lg_callback_result" in
          let result = Semantic_ir.Ident result_name in
          let value =
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Lg_runtime.Runtime_reduced.unreduced",
                [ result ] )
          in
          Semantic_ir.Fun
            ( List.map (fun name -> Semantic_ir.PVar name) parameter_names,
              Semantic_ir.Let
                ( [
                    ( Semantic_ir.PVar result_name,
                      Semantic_ir.Apply
                        ( arg.semantic_expr,
                          List.map
                            (fun name -> Semantic_ir.Ident name)
                            parameter_names ) );
                  ],
                  Semantic_ir.If
                    ( Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_reduced.is_reduced",
                          [ result ] ),
                      Semantic_ir.Sequence
                        [
                          Semantic_ir.Infix
                            ( ":=",
                              Semantic_ir.Ident reduced_callback_state,
                              Semantic_ir.Constructor ("Some", Some result) );
                          Semantic_ir.Apply
                            ( Semantic_ir.Ident "raise",
                              [
                                Semantic_ir.Constructor
                                  ( "Lg_runtime.Runtime_reduced.Callback_reduced",
                                    None );
                              ] );
                        ],
                      value ) ) )
      | None -> arg.semantic_expr)
  | _ -> arg.semantic_expr

let adapt_nullable_callback env expected arg =
  match (expected, arg.ty) with
  | ( TFn (expected_params, TNullable expected_return),
      TFn (actual_params, actual_return) )
    when callback_parameters_compatible expected_params actual_params
         && (expects_dynamic_value actual_return
            || Types.assignable ~policy:Host_boundary
                 ~expected:expected_return ~actual:actual_return) ->
      let parameter_names =
        List.mapi
          (fun index _ -> "__lg_nullable_callback_arg_" ^ string_of_int index)
          expected_params
      in
      let rec adapt_parameters acc expected actual names =
        match (expected, actual, names) with
        | [], [], [] -> Ok (List.rev acc)
        | expected_ty :: expected, actual_ty :: actual, name :: names ->
            let value = typed_ir expected_ty (Semantic_ir.Ident name) in
            let adapted =
              if Types.equal expected_ty actual_ty then Ok value.semantic_expr
              else if
                Types.is_dynamic expected_ty
                && has_capability_constraint actual_ty
              then pack_constrained_value env actual_ty value
              else if
                Types.is_dynamic expected_ty
                && not (expects_dynamic_value actual_ty)
              then dynamic_unpack env actual_ty value.semantic_expr
              else if
                expects_dynamic_value actual_ty
                && has_capability_constraint expected_ty
              then
                Ok
                  (constrained_value_expression expected_ty value.semantic_expr)
              else if has_capability_constraint actual_ty then
                pack_constrained_value env actual_ty value
              else Ok value.semantic_expr
            in
            Result.bind adapted (fun expression ->
                adapt_parameters (expression :: acc) expected actual names)
        | _ -> Error.error "callback parameter arity mismatch"
      in
      Result.bind
        (adapt_parameters [] expected_params actual_params parameter_names)
        (fun arguments ->
          let result_name = "__lg_nullable_callback_result" in
          let result = Semantic_ir.Ident result_name in
          let actual_result = typed_ir actual_return result in
          let expected_callback_return = TNullable expected_return in
          let adapted_result =
            match optional_payload actual_return with
            | Some _ ->
                adapt_value_to_type env expected_callback_return actual_result
            | None ->
                Result.map
                  (fun value ->
                    if expects_dynamic_value actual_return then
                      Semantic_ir.If
                        ( Semantic_ir.Apply
                            ( Semantic_ir.Ident
                                "Lg_runtime.Runtime_dynamic.is_nil",
                              [ result ] ),
                          Semantic_ir.Constructor ("None", None),
                          Semantic_ir.Constructor ("Some", Some value) )
                    else Semantic_ir.Constructor ("Some", Some value))
                  (adapt_value_to_type env expected_return actual_result)
          in
          Result.map
            (fun result_expression ->
              Semantic_ir.Fun
                ( List.map (fun name -> Semantic_ir.PVar name) parameter_names,
                  Semantic_ir.Let
                    ( [
                        ( Semantic_ir.PVar result_name,
                          Semantic_ir.Apply (arg.semantic_expr, arguments) );
                      ],
                      result_expression ) ))
            adapted_result)
  | _ -> Ok arg.semantic_expr

let callback_parameters_need_adapter expected actual =
  List.length expected = List.length actual
  && List.exists2
       (fun expected_ty actual_ty ->
         (Types.is_dynamic expected_ty
         && not (expects_dynamic_value actual_ty))
         ||
         (Types.is_dynamic actual_ty
         && not (expects_dynamic_value expected_ty))
         ||
         (expects_dynamic_value actual_ty
         && has_capability_constraint expected_ty)
         ||
         (has_capability_constraint actual_ty
         && not (Types.equal expected_ty actual_ty)))
       expected actual

let callback_payload_parameters_compatible expected actual =
  List.length expected = List.length actual
  && List.for_all2
       (fun expected_ty actual_ty ->
         let actual_ty =
           if has_capability_constraint actual_ty then
             Types.constraint_value_type actual_ty
           else actual_ty
         in
         Types.assignable ~policy:Host_boundary ~expected:expected_ty
           ~actual:actual_ty
         || Result.is_ok
              (Type_solver.unify Type_solver.empty expected_ty actual_ty))
       expected actual

let pack_static_callback_capability env actual_ty value =
  match Types.comparable_constraint_info actual_ty with
  | Some ((TUnknown | TMeta _ | TVar _) as value_ty) ->
      Ok
        (Semantic_ir.Tuple
           [
             Semantic_ir.Ident "Stdlib.compare";
             coerce_expression_to_type value_ty value.ty value.semantic_expr;
           ])
  | Some _ | None -> pack_constrained_value env actual_ty value

let adapt_dynamic_callback env expected arg =
  match (expected, arg.ty) with
  | TFn (expected_params, expected_return), TFn (actual_params, actual_return)
    when (Types.is_dynamic expected_return
         || callback_parameters_need_adapter expected_params actual_params)
         && (callback_parameters_compatible expected_params actual_params
            || callback_payload_parameters_compatible expected_params
                 actual_params) ->
      let expected_return =
        Types.maybe_reduced_callback_element expected_return
        |> Option.value ~default:expected_return
      in
      let parameter_names =
        List.mapi
          (fun index _ -> "__lg_erased_callback_arg_" ^ string_of_int index)
          expected_params
      in
      let rec adapt_parameters acc expected actual names =
        match (expected, actual, names) with
        | [], [], [] -> Ok (List.rev acc)
        | expected_ty :: expected, actual_ty :: actual, name :: names ->
            let value = typed_ir expected_ty (Semantic_ir.Ident name) in
            let adapted =
              if Types.equal expected_ty actual_ty then Ok value.semantic_expr
              else if
                Types.is_dynamic expected_ty
                && has_capability_constraint actual_ty
              then dynamic_unpack env actual_ty value.semantic_expr
              else if
                Types.is_dynamic expected_ty
                && not (expects_dynamic_value actual_ty)
              then dynamic_unpack env actual_ty value.semantic_expr
              else if
                Types.is_dynamic actual_ty
                && not (expects_dynamic_value expected_ty)
              then pack_dynamic_value env actual_ty value
              else if
                expects_dynamic_value actual_ty
                && has_capability_constraint expected_ty
              then
                Ok
                  (constrained_value_expression expected_ty value.semantic_expr)
              else if has_capability_constraint actual_ty then
                pack_static_callback_capability env actual_ty value
              else Ok value.semantic_expr
            in
            Result.bind adapted (fun expression ->
                adapt_parameters (expression :: acc) expected actual names)
        | _ -> Error.error "callback parameter arity mismatch"
      in
      Result.bind
        (adapt_parameters [] expected_params actual_params parameter_names)
        (fun arguments ->
          let result =
            typed_ir
              (Types.constraint_value_type actual_return)
              (Semantic_ir.Apply (arg.semantic_expr, arguments))
          in
          Result.map
            (fun packed_result ->
              Semantic_ir.Fun
                ( List.map (fun name -> Semantic_ir.PVar name) parameter_names,
                  packed_result ))
            (if Types.is_dynamic expected_return then
               pack_dynamic_value env expected_return result
             else adapt_value_to_type env expected_return result))
  | _ -> Ok arg.semantic_expr

let adapt_truthy_callback expected arg =
  match (expected, arg.ty) with
  | TFn (expected_params, TBool), TFn (actual_params, actual_return)
    when expects_dynamic_value actual_return
         && callback_parameters_compatible expected_params actual_params ->
      let parameter_names =
        List.mapi
          (fun index _ -> "__lg_truthy_callback_arg_" ^ string_of_int index)
          expected_params
      in
      Semantic_ir.Fun
        ( List.map (fun name -> Semantic_ir.PVar name) parameter_names,
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.truthy",
              [
                Semantic_ir.Apply
                  ( arg.semantic_expr,
                    List.map
                      (fun name -> Semantic_ir.Ident name)
                      parameter_names );
              ] ) )
  | _ -> arg.semantic_expr

let adapt_overloaded_callback env expected arg =
  match (expected, arg.ty) with
  | TFn (expected_params, expected_return), TOverloaded_fn arities ->
      let selected =
        arities
        |> List.mapi (fun index arity -> (index, arity))
        |> List.find_map (fun (index, arity) ->
               match
                 overloaded_arity_parameters arity (List.length expected_params)
               with
               | Some actual_params
                 when argument_compatible expected
                        (TFn (actual_params, arity.return_ty)) ->
                   Some (index, arity, actual_params)
               | Some _ | None -> None)
      in
      (match selected with
      | None -> Ok arg.semantic_expr
      | Some (arity_index, arity, actual_params) ->
          let argument_names =
            List.mapi
              (fun index _ ->
                "__lg_overloaded_callback_arg_" ^ string_of_int index)
              expected_params
          in
          let rec adapt_arguments adapted expected actual names =
            match (expected, actual, names) with
            | [], [], [] -> Ok (List.rev adapted)
            | expected_ty :: expected, actual_ty :: actual, name :: names ->
                let value = typed_ir expected_ty (Semantic_ir.Ident name) in
                Result.bind (adapt_value_to_type env actual_ty value)
                  (fun expression ->
                    adapt_arguments (expression :: adapted) expected actual
                      names)
            | _ -> Error.error "overloaded callback arity mismatch"
          in
          Result.bind
            (adapt_arguments [] expected_params actual_params argument_names)
            (fun arguments ->
              let fixed_count = List.length arity.fixed_params in
              let fixed_arguments, rest_arguments =
                List.fold_left
                  (fun (fixed, rest) (index, argument) ->
                    if index < fixed_count then (argument :: fixed, rest)
                    else (fixed, argument :: rest))
                  ([], []) (List.mapi (fun index value -> (index, value)) arguments)
              in
              let call_arguments =
                List.rev fixed_arguments
                @
                match arity.rest_param with
                | None -> []
                | Some _ ->
                    [
                      Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.of_list",
                          [ Semantic_ir.List (List.rev rest_arguments) ] );
                    ]
              in
              let rec projection expression remaining =
                if remaining = 0 then
                  Semantic_ir.Apply (Semantic_ir.Ident "fst", [ expression ])
                else
                  projection
                    (Semantic_ir.Apply
                       (Semantic_ir.Ident "snd", [ expression ]))
                    (remaining - 1)
              in
              let result =
                typed_ir arity.return_ty
                  (Semantic_ir.Apply
                     (projection arg.semantic_expr arity_index, call_arguments))
              in
              let adapted_result =
                if
                  Types.equal expected_return TBool
                  && expects_dynamic_value arity.return_ty
                then
                  Ok
                    (Semantic_ir.Apply
                       ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.truthy",
                         [ result.semantic_expr ] ))
                else adapt_value_to_type env expected_return result
              in
              Result.map
                (fun body ->
                  Semantic_ir.Fun
                    ( List.map (fun name -> Semantic_ir.PVar name) argument_names,
                      body ))
                adapted_result))
  | _ -> Ok arg.semantic_expr

let protocol_implementation scope env protocol_name method_name receiver_ty =
  Option.bind
    (Protocol.lookup_protocol_marker scope env protocol_name method_name)
    (fun marker ->
      Protocol.lookup_marker_impl env marker method_name receiver_ty)

let atom_state_type scope env receiver_ty =
  match
    protocol_implementation scope env "IDeref" "-deref" receiver_ty
  with
  | Some { ty = TFn ([ _ ], state_ty); _ } -> Some state_ty
  | Some _ | None -> None

let compile_protocol_swap ~compile_expr scope env swap_name reference
    function_form extra_forms =
  let reference_ty = reference.ty in
  let public_name = if swap_name = "__lg_swap!" then "swap!" else swap_name in
  let type_error () =
    Error.error
      (public_name ^ " expects a reference or ISwap as its first argument")
  in
  if swap_name <> "__lg_swap!" then type_error ()
  else
    match
      protocol_implementation scope env "ISwap" "-swap!" reference_ty
    with
    | None -> type_error ()
    | Some
        {
          ty = TFn ([ receiver_ty; TFn ([ _ ], _) ], _);
          ocaml_name;
          _;
        } -> (
        match atom_state_type scope env reference_ty with
        | None -> type_error ()
        | Some state_ty ->
            let value_name = "__lg_swap_value" in
            let updater_env =
              Env.add
                (Names.scoped_key scope value_name)
                (Types.binding value_name state_ty)
                env
            in
            let updater_body =
              FList
                (function_form :: FSymbol value_name :: extra_forms)
            in
            Result.bind
              (compile_expr scope updater_env updater_body)
              (fun updater_body ->
                Result.bind
                  (adapt_value_to_type updater_env state_ty
                     updater_body)
                  (fun updater_body ->
                    let updater =
                      Semantic_ir.Fun
                        ( [ Semantic_ir.PVar value_name ],
                          updater_body )
                    in
                    Result.map
                      (fun receiver ->
                        typed_ir state_ty
                          (Semantic_ir.Apply
                             ( Semantic_ir.Ident ocaml_name,
                               [ receiver; updater ] )))
                      (adapt_value_to_type env receiver_ty reference))))
    | Some _ -> Error.error "ISwap/-swap! has an invalid signature"



let create ~compile_expr =
  let special_forms : Special_form_elaborator.t =
    Special_form_elaborator.create ~compile_expr ~dynamic_unpack
      ~pack_dynamic_value ~pack_constrained_value ~argument_compatible
  in
  let collection : Collection_operation_elaborator.t =
    Collection_operation_elaborator.create ~compile_expr ~pack_dynamic_value
      ~dynamic_unpack
  in
  let sequence : Sequence_call_elaborator.t =
    Sequence_call_elaborator.create ~compile_expr ~pack_dynamic_value
      ~dynamic_unpack ~pack_constrained_value
  in
  let functions : Function_combinator_elaborator.t =
    Function_combinator_elaborator.create ~compile_expr ~dynamic_unpack
      ~pack_dynamic_value ~pack_constrained_value ~adapt_value_to_type
  in
  let comparisons : Comparison_set_elaborator.t =
    Comparison_set_elaborator.create ~compile_expr
  in
  let compile_vector = special_forms.compile_vector in
  let compile_body = special_forms.compile_body in
  let compile_list = collection.compile_list in
  let compile_list_star = collection.compile_list_star in
  let compile_list_of = collection.compile_list_of in
  let compile_vector_of = collection.compile_vector_of in
  let compile_static_conj = collection.compile_conj in
  let compile_static_cons = collection.compile_cons in
  let compile_subvec = collection.compile_subvec in
  let compile_nth = collection.compile_nth in
  let compile_static_get = collection.compile_get in
  let compile_find = collection.compile_find in
  let compile_static_assoc = collection.compile_assoc in
  let compile_dissoc = collection.compile_dissoc in
  let compile_merge = collection.compile_merge in
  let compile_hash_map = collection.compile_hash_map in
  let compile_update = collection.compile_update in
  let compile_select_keys = collection.compile_select_keys in
  let compile_contains = collection.compile_contains in
  let compile_keys = collection.compile_keys in
  let compile_vals = collection.compile_vals in
  let compile_sort_by = sequence.compile_sort_by in
  let compile_reductions = sequence.compile_reductions in
  let compile_map = sequence.compile_map_call in
  let compile_mapv = sequence.compile_mapv in
  let compile_reduce_kv = sequence.compile_reduce_kv in
  let compile_some = sequence.compile_some in
  let compile_reduce = sequence.compile_reduce in
  let compile_apply = functions.compile_apply in
  let compile_static_fnil = functions.compile_static_fnil in
  let compile_static_comp = functions.compile_static_comp in
  let compile_static_partial = functions.compile_static_partial in
  let compile_static_juxt = functions.compile_static_juxt in
  let compile_compare = comparisons.compile_compare in
  let compile_hash_set = comparisons.compile_hash_set in
  let compile_set_of = comparisons.compile_set_of in
  let rec stringify_value scope env ~pr value =
    match value.ty with
    | ty when Option.is_some (Types.printable_constraint_info ty) -> (
        match Semantic_ir.unlocated value.semantic_expr with
        | Semantic_ir.Ident name ->
            Semantic_ir.Apply
              ( Semantic_ir.Ident (name ^ if pr then "__pr" else "__print"),
                [ value.semantic_expr ] )
        | _ ->
            let witnesses =
              Semantic_ir.Apply
                (Semantic_ir.Ident "fst", [ value.semantic_expr ])
            in
            Semantic_ir.Apply
              ( Semantic_ir.Apply
                  ( Semantic_ir.Ident (if pr then "snd" else "fst"),
                    [ witnesses ] ),
                [
                  Semantic_ir.Apply
                    (Semantic_ir.Ident "snd", [ value.semantic_expr ]);
                ] ))
    | ty
      when Option.is_some (Types.protocol_constraint_info ty)
           || Option.is_some (Types.seqable_constraint_info ty) ->
        stringify_value scope env ~pr
          (typed_ir (Types.constraint_value_type ty)
             (constrained_argument_value value))
    | TNullable inner | TOcaml_app ("option", [ inner ]) ->
        let value_name = "__lg_optional_print_value" in
        Semantic_ir.Match
          ( value.semantic_expr,
            [
              ( Semantic_ir.PConstructor ("None", None),
                Semantic_ir.String "nil" );
              ( Semantic_ir.PConstructor
                  ("Some", Some (Semantic_ir.PVar value_name)),
                stringify_value scope env ~pr
                  (typed_ir inner (Semantic_ir.Ident value_name)) );
            ] )
    | TNamed_record record -> (
        let set_rendering =
          if Protocol.type_satisfies env Core_protocols.set_id value.ty then
            match Collection_capability.seq_expr env value with
            | Ok { ty = TSeq element_ty; semantic_expr; _ } ->
                let item_name = "__lg_print_set_item" in
                let mapper =
                  Semantic_ir.Fun
                    ( [ Semantic_ir.PVar item_name ],
                      stringify_value scope env ~pr
                        (typed_ir element_ty (Semantic_ir.Ident item_name)) )
                in
                Some
                  (Codegen.concat_expr
                     [
                       Semantic_ir.String "#{";
                       Semantic_ir.Apply
                         ( Semantic_ir.Ident "String.concat",
                           [
                             Semantic_ir.String " ";
                             Semantic_ir.Apply
                               ( Semantic_ir.Ident "List.map",
                                 [
                                   mapper;
                                   Semantic_ir.Apply
                                     ( Semantic_ir.Ident
                                         "Lg_runtime.Runtime_seq.to_list",
                                       [ semantic_expr ] );
                                 ] );
                           ] );
                       Semantic_ir.String "}";
                     ])
            | Ok _ | Error _ -> None
          else None
        in
        match set_rendering with
        | Some rendering -> rendering
        | None -> (
        let printer =
          match lookup_print_method scope env record with
          | Ok printer -> Some printer
          | Error _ ->
              Protocol.lookup_unique_method_impl env "-pr-writer" value.ty
        in
        match printer with
        | None -> Codegen.stringify_expr_ir ~pr value
        | Some printer ->
            let writer_name = "__lg_print_method_writer" in
            let receiver =
              match printer.ty with
              | TFn (expected :: _, _) ->
                  adapt_value_to_type env expected value
                  |> Result.value ~default:value.semantic_expr
              | _ -> value.semantic_expr
            in
            let arguments =
              match printer.ty with
              | TFn ([ _; _; _ ], _) ->
                  [
                    receiver;
                    Semantic_ir.Ident writer_name;
                    Semantic_ir.Apply
                      (Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.unit", []);
                  ]
              | _ -> [ receiver; Semantic_ir.Ident writer_name ]
            in
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Lg_runtime.Runtime_print.render",
                [
                  Semantic_ir.Fun
                    ( [ Semantic_ir.PVar writer_name ],
                      Semantic_ir.Apply
                        (Semantic_ir.Ident printer.ocaml_name, arguments) );
                ] )))
    | _ -> Codegen.stringify_expr_ir ~pr value
  in
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
  and compile_transient scope env = function
    | [ FList [ FSymbol "__lg_hash-set" ] ] ->
        Ok
          (typed_ir
             (TOcaml_app ("Lg_runtime.Runtime_transient.set", [ TUnknown ]))
             (Semantic_ir.Apply
                (Semantic_ir.Ident "Lg_runtime.Runtime_transient.set_empty", [])))
    | [ FVector [] ] ->
        Ok
          (typed_ir
             (TOcaml_app ("Lg_runtime.Runtime_transient.vector", [ TUnknown ]))
             (Semantic_ir.Apply
                ( Semantic_ir.Ident "Lg_runtime.Runtime_transient.vector_empty",
                  [] )))
    | [ FMap [] ] ->
        Ok
          (typed_ir
             (TOcaml_app
                ( "Lg_runtime.Runtime_transient.map",
                  [ TUnknown; TUnknown ] ))
             (Semantic_ir.Apply
                (Semantic_ir.Ident "Lg_runtime.Runtime_transient.map_empty", [])))
    | [ collection_form ] -> (
        match compile_expr scope env collection_form with
        | Error _ as error -> error
        | Ok collection when Types.is_dynamic collection.ty ->
            Ok
              (typed_ir collection.ty
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.as_transient",
                      [ collection.semantic_expr ] )))
        | Ok collection -> (
            match collection.ty with
            | TSet element_type ->
                Result.map
                  (fun set_module ->
                    let constructor =
                      if Types.is_dynamic element_type then
                        "Lg_runtime.Runtime_transient.set_of_list_dynamic"
                      else "Lg_runtime.Runtime_transient.set_of_list"
                    in
                    typed_ir
                      (TOcaml_app
                         ("Lg_runtime.Runtime_transient.set", [ element_type ]))
                      (Semantic_ir.Apply
                         ( Semantic_ir.Ident constructor,
                           [
                             Semantic_ir.Apply
                               ( Semantic_ir.Ident (set_module ^ ".elements"),
                                 [ collection.semantic_expr ] );
                           ] )))
                  (Types.set_module_name element_type)
            | TVector element_type ->
                Ok
                  (typed_ir
                     (TOcaml_app
                        ("Lg_runtime.Runtime_transient.vector", [ element_type ]))
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_transient.vector_of_list",
                          [
                            Semantic_ir.Apply
                              ( Semantic_ir.Ident "Rrbvec.to_list",
                                [ collection.semantic_expr ] );
                          ] )))
            | map_type -> (
                let map_types =
                  match collection.record_values with
                  | Some ((field, _) :: rest)
                    when List.for_all
                           (fun ((candidate : field), _) ->
                             Types.equal candidate.ty field.ty)
                           rest ->
                      Some (TKeyword, field.ty)
                  | Some [] -> Some (TUnknown, TUnknown)
                  | None | Some _ -> Types.dynamic_map_types map_type
                in
                match map_types with
                | Some (key_type, value_type) ->
                    let constructor =
                      if Types.is_dynamic key_type then
                        "Lg_runtime.Runtime_transient.map_of_list_dynamic"
                      else "Lg_runtime.Runtime_transient.map_of_list"
                    in
                    let persistent_map =
                      match collection.record_values with
                      | Some values ->
                          adapt_record_values_to_map env key_type value_type
                            values
                      | None -> Ok collection.semantic_expr
                    in
                    Result.map
                      (fun persistent_map ->
                        typed_ir
                          (TOcaml_app
                             ( "Lg_runtime.Runtime_transient.map",
                               [ key_type; value_type ] ))
                          (Semantic_ir.Apply
                             ( Semantic_ir.Ident constructor,
                               [
                                 Semantic_ir.Apply
                                   ( Semantic_ir.Ident
                                       "Lg_runtime.Runtime_map.to_list",
                                     [ persistent_map ] );
                               ] )))
                      persistent_map
                | None ->
                    Error.error
                      ("transient expects a set, vector, or map, got "
                     ^ source_name map_type))
            ))
    | _ -> Error.error "transient expects 1 argument"
  and compile_conj_bang scope env = function
    | [] -> compile_transient scope env [ FVector [] ]
    | [ collection_form ] -> compile_expr scope env collection_form
    | collection_form :: value_forms when value_forms <> [] -> (
        match compile_expr scope env collection_form with
        | Error _ as error -> error
        | Ok collection ->
            let add_value collection value_form =
              match compile_expr scope env value_form with
              | Error _ as error -> error
              | Ok value when Types.is_dynamic collection.ty ->
                  let dynamic = Types.dynamic_constraint TUnknown in
                  Result.map
                    (fun value ->
                      typed_ir collection.ty
                        (Semantic_ir.Apply
                           ( Semantic_ir.Ident
                               "Lg_runtime.Runtime_dynamic.conj_bang",
                             [ collection.semantic_expr; value ] )))
                    (pack_dynamic_value env dynamic value)
              | Ok value -> (
                  match collection.ty with
                  | TOcaml_app
                      ("Lg_runtime.Runtime_transient.set", [ element_type ])
                    when Types.equal element_type TUnknown
                         || Types.is_dynamic element_type
                         || Types.same_shape element_type value.ty ->
                      let element_type =
                        if Types.equal element_type TUnknown then value.ty
                        else element_type
                      in
                      let value =
                        if
                          Types.is_dynamic element_type
                          && not (Types.is_dynamic value.ty)
                        then pack_dynamic_value env element_type value
                        else Ok value.semantic_expr
                      in
                      Result.map
                        (fun value ->
                          let add =
                            if Types.is_dynamic element_type then
                              "Lg_runtime.Runtime_transient.set_add_dynamic"
                            else "Lg_runtime.Runtime_transient.set_add"
                          in
                          typed_ir
                            (TOcaml_app
                               ( "Lg_runtime.Runtime_transient.set",
                                 [ element_type ] ))
                            (Semantic_ir.Apply
                               ( Semantic_ir.Ident add,
                                 [ collection.semantic_expr; value ] )))
                        value
                  | TOcaml_app
                      ("Lg_runtime.Runtime_transient.vector", [ element_type ])
                    when Types.equal element_type TUnknown
                         || Types.is_dynamic element_type
                         || Types.same_shape element_type value.ty ->
                      let element_type =
                        if Types.equal element_type TUnknown then value.ty
                        else element_type
                      in
                      let value =
                        if
                          Types.is_dynamic element_type
                          && not (Types.is_dynamic value.ty)
                        then pack_dynamic_value env element_type value
                        else Ok value.semantic_expr
                      in
                      Result.map
                        (fun value ->
                          typed_ir
                            (TOcaml_app
                               ( "Lg_runtime.Runtime_transient.vector",
                                 [ element_type ] ))
                            (Semantic_ir.Apply
                               ( Semantic_ir.Ident
                                   "Lg_runtime.Runtime_transient.vector_add",
                                 [ collection.semantic_expr; value ] )))
                        value
                  | TOcaml_app
                      ( ("Lg_runtime.Runtime_transient.set"
                        | "Lg_runtime.Runtime_transient.vector"),
                        _ ) ->
                      Error.error
                        "conj! value type must match transient element type"
                  | _ ->
                      Error.error
                        ("conj! expects a transient set or vector, got "
                       ^ source_name collection.ty))
            in
            List.fold_left
              (fun result value_form ->
                match result with
                | Error _ as error -> error
                | Ok collection -> add_value collection value_form)
              (Ok collection) value_forms)
    | _ -> Error.error "conj! expects a transient collection and values"
  and compile_get scope env arg_forms =
    let dynamic_result_type fallback =
      match Env.expected_type env with
      | Some expected when Types.is_dynamic expected -> expected
      | Some expected -> Types.dynamic_constraint expected
      | None -> fallback
    in
    match arg_forms with
    | [ target_form; key_form ] -> (
        match
          (compile_expr scope env target_form, compile_expr scope env key_form)
        with
        | (Error _ as error), _ -> error
        | _, (Error _ as error) -> error
        | Ok target, Ok _key
          when Types.equal target.ty (TOcaml "Lg_edn_backend.t") -> (
            match key_form with
            | FKeyword keyword ->
                let found =
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_edn.find_keyword",
                      [ Semantic_ir.String keyword; target.semantic_expr ] )
                in
                let expected =
                  Env.expected_type env
                  |> Option.value ~default:(TOcaml "Lg_edn_backend.t")
                in
                (match optional_payload expected with
                | Some _ ->
                    let value_name = "__lg_metadata_lookup_value" in
                    Result.map
                      (fun decoded ->
                        typed_ir expected
                          (Semantic_ir.Match
                             ( found,
                               [
                                 ( Semantic_ir.PConstructor ("None", None),
                                   Semantic_ir.Constructor ("None", None) );
                                 ( Semantic_ir.PConstructor
                                     ("Some", Some (Semantic_ir.PVar value_name)),
                                   decoded );
                               ] )))
                      (unpack_metadata_expression expected
                         (Semantic_ir.Ident value_name))
                | None ->
                    Result.map
                      (fun decoded -> typed_ir expected decoded)
                      (unpack_metadata_expression expected
                         (Semantic_ir.Apply
                            (Semantic_ir.Ident "Option.get", [ found ]))))
            | _ -> Error.error "EDN metadata lookup requires a keyword key")
        | Ok target, Ok key when Types.is_dynamic target.ty -> (
            let expected = Types.dynamic_constraint TUnknown in
            match pack_dynamic_value env expected key with
            | Error _ as error -> error
            | Ok key ->
                Ok
                  (typed_ir (dynamic_result_type target.ty)
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.get",
                          [ target.semantic_expr; key ] ))))
        | Ok _, Ok _ -> compile_static_get scope env arg_forms)
    | [ target_form; key_form; default_form ] -> (
        match
          ( compile_expr scope env target_form,
            compile_expr scope env key_form,
            compile_expr scope env default_form )
        with
        | (Error _ as error), _, _ -> error
        | _, (Error _ as error), _ -> error
        | _, _, (Error _ as error) -> error
        | Ok target, Ok key, Ok default when Types.is_dynamic target.ty -> (
            let expected = Types.dynamic_constraint TUnknown in
            match
               ( pack_dynamic_value env expected key,
                 pack_dynamic_value env expected default )
             with
            | (Error _ as error), _ -> error
            | _, (Error _ as error) -> error
            | Ok key, Ok default ->
                Ok
                  (typed_ir (dynamic_result_type target.ty)
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_dynamic.get_default",
                          [ target.semantic_expr; key; default ] ))))
        | Ok _, Ok _, Ok _ -> compile_static_get scope env arg_forms)
    | _ -> compile_static_get scope env arg_forms
  and compile_assoc scope env arg_forms =
    compile_static_assoc scope env arg_forms
  and compile_assoc_bang scope env = function
    | collection_form :: pair_forms
      when pair_forms <> [] && List.length pair_forms mod 2 = 0 -> (
        match compile_expr scope env collection_form with
        | Error _ as error -> error
        | Ok initial_collection ->
            let rec add_pairs collection = function
              | [] -> Ok collection
              | key_form :: value_form :: rest -> (
                  match
                    ( compile_expr scope env key_form,
                      compile_expr scope env value_form )
                  with
                  | (Error _ as error), _ -> error
                  | _, (Error _ as error) -> error
                  | Ok _, Ok _ when Types.is_dynamic collection.ty ->
                      Error.error
                        "assoc! requires a statically typed transient map or \
                         vector"
                  | Ok key, Ok value -> (
                      match collection.ty with
                      | TOcaml_app
                          ( "Lg_runtime.Runtime_transient.map",
                            [ key_type; value_type ] ) ->
                          let key_type =
                            if Types.equal key_type TUnknown then
                              key.ty
                            else key_type
                          in
                          let value_type =
                            if Types.equal value_type TUnknown then
                              value.ty
                            else value_type
                          in
                          let adapt expected actual =
                            if
                              Types.is_dynamic expected
                              && not (Types.is_dynamic actual.ty)
                            then pack_dynamic_value env expected actual
                            else if Types.same_shape expected actual.ty then
                              Ok actual.semantic_expr
                            else Error.error "incompatible transient map entry"
                          in
                          (match
                             (adapt key_type key, adapt value_type value)
                           with
                          | Error _, _ | _, Error _ ->
                            Error.error
                              ("assoc! key and value types must match \
                                transient map: expected "
                             ^ Types.source_name key_type ^ " and "
                              ^ Types.source_name value_type
                              ^ ", got " ^ Types.source_name key.ty ^ " and "
                             ^ Types.source_name value.ty)
                          | Ok key, Ok value ->
                            let assoc =
                              if Types.is_dynamic key_type then
                                "Lg_runtime.Runtime_transient.map_assoc_dynamic"
                              else "Lg_runtime.Runtime_transient.map_assoc"
                            in
                            add_pairs
                              (typed_ir
                                 (TOcaml_app
                                    ( "Lg_runtime.Runtime_transient.map",
                                      [ key_type; value_type ] ))
                                 (Semantic_ir.Apply
                                    ( Semantic_ir.Ident assoc,
                                      [
                                        collection.semantic_expr;
                                        key;
                                        value;
                                      ] )))
                              rest)
                      | TOcaml_app
                          ( "Lg_runtime.Runtime_transient.vector",
                            [ element_type ] )
                        when Types.equal key.ty TInt
                             && (Types.equal element_type TUnknown
                                || Types.same_shape element_type value.ty) ->
                          let element_type =
                            if Types.equal element_type TUnknown then value.ty
                            else element_type
                          in
                          add_pairs
                            (typed_ir
                               (TOcaml_app
                                  ( "Lg_runtime.Runtime_transient.vector",
                                    [ element_type ] ))
                               (Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Runtime_transient.vector_assoc",
                                    [
                                      collection.semantic_expr;
                                      key.semantic_expr;
                                      value.semantic_expr;
                                    ] )))
                            rest
                      | TOcaml_app ("Lg_runtime.Runtime_transient.vector", _) ->
                          Error.error
                            "assoc! vector expects an int index and matching \
                             value"
                      | _ ->
                          Error.error "assoc! expects a transient map or vector"
                      ))
              | _ -> assert false
            in
            add_pairs initial_collection pair_forms)
    | _ ->
        Error.error
          "assoc! expects a transient collection followed by key/value pairs"
  and compile_dissoc_bang scope env = function
    | [ collection_form; key_form ] -> (
        match
          ( compile_expr scope env collection_form,
            compile_expr scope env key_form )
        with
        | (Error _ as error), _ -> error
        | _, (Error _ as error) -> error
        | Ok collection, Ok key
          when Types.is_dynamic collection.ty
               || (match collection.ty with
                  | TUnknown | TMeta _ | TVar _ -> true
                  | _ -> false)
          ->
            let dynamic = Types.dynamic_constraint TUnknown in
            Result.map
              (fun key ->
                typed_ir dynamic
                  (Semantic_ir.Apply
                     ( Semantic_ir.Ident
                         "Lg_runtime.Runtime_dynamic.dissoc_bang",
                       [ collection.semantic_expr; key ] )))
              (pack_dynamic_value env dynamic key)
        | Ok collection, Ok key -> (
            match collection.ty with
            | TOcaml_app
                ( "Lg_runtime.Runtime_transient.map",
                  [ key_type; value_type ] ) ->
                let key_type =
                  if Types.equal key_type TUnknown then key.ty else key_type
                in
                let key =
                  if
                    Types.is_dynamic key_type
                    && not (Types.is_dynamic key.ty)
                  then pack_dynamic_value env key_type key
                  else if Types.same_shape key_type key.ty then
                    Ok key.semantic_expr
                  else
                    Error.error
                      "dissoc! key type must match the transient map key type"
                in
                Result.map
                  (fun key ->
                    let dissoc =
                      if Types.is_dynamic key_type then
                        "Lg_runtime.Runtime_transient.map_dissoc_dynamic"
                      else "Lg_runtime.Runtime_transient.map_dissoc"
                    in
                    typed_ir
                      (TOcaml_app
                         ( "Lg_runtime.Runtime_transient.map",
                           [ key_type; value_type ] ))
                      (Semantic_ir.Apply
                         ( Semantic_ir.Ident dissoc,
                           [ collection.semantic_expr; key ] )))
                  key
            | _ ->
                Error.error
                  ("dissoc! expects a transient map, got "
                 ^ Types.source_name collection.ty)))
    | _ -> Error.error "dissoc! expects a transient map and key"
  and compile_persistent_bang scope env = function
    | [ collection_form ] -> (
        match compile_expr scope env collection_form with
        | Error _ as error -> error
        | Ok collection when Types.is_dynamic collection.ty ->
            Ok
              (typed_ir collection.ty
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.persistent",
                      [ collection.semantic_expr ] )))
        | Ok collection -> (
            match collection.ty with
            | TOcaml_app
                ("Lg_runtime.Runtime_transient.vector", [ element_type ]) ->
                Ok
                  (typed_ir (TVector element_type)
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_transient.vector_persistent",
                          [ collection.semantic_expr ] )))
            | TOcaml_app
                ("Lg_runtime.Runtime_transient.map", [ key_type; value_type ])
              ->
                let persistent =
                  if Types.is_dynamic key_type then
                    "Lg_runtime.Runtime_transient.map_persistent_dynamic"
                  else "Lg_runtime.Runtime_transient.map_persistent"
                in
                Ok
                  (typed_ir
                     (Types.dynamic_map key_type value_type)
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident persistent,
                          [ collection.semantic_expr ] )))
            | TOcaml_app ("Lg_runtime.Runtime_transient.set", [ element_type ])
              ->
                Result.map
                  (fun set_module ->
                    typed_ir (TSet element_type)
                      (Semantic_ir.Apply
                         ( Semantic_ir.Ident (set_module ^ ".of_seq"),
                           [
                             Semantic_ir.Apply
                               ( Semantic_ir.Ident
                                   "Lg_runtime.Runtime_transient.set_to_seq",
                                 [ collection.semantic_expr ] );
                           ] )))
                  (Types.set_module_name element_type)
            | _ -> Error.error "persistent! expects a transient collection"))
    | _ -> Error.error "persistent! expects 1 argument"
  and compile_apply_zip_vectors scope env constructor_form fixed_forms rest_form =
    match
      ( compile_function_arg scope env constructor_form,
        compile_args_for scope env fixed_forms,
        compile_expr scope env rest_form )
    with
    | (Error _ as error), _, _ -> error
    | _, (Error _ as error), _ -> error
    | _, _, (Error _ as error) -> error
    | Ok constructor, Ok fixed, Ok rest -> (
        match constructor.ty with
        | TOverloaded_fn
            [
              {
                fixed_params = [];
                rest_param = Some element_ty;
                return_ty = TVector return_element_ty;
              };
            ]
          when Types.equal element_ty return_element_ty ->
            if
              not
                (List.for_all
                   (fun collection ->
                     match collection.ty with
                     | TVector _ | TUnknown | TMeta _ | TVar _ -> true
                     | _ -> false)
                   fixed)
            then Error.error "apply mapv expects vector collections"
            else
              let element_ty =
                fixed
                |> List.find_map (fun collection ->
                       match collection.ty with
                       | TVector element_ty -> Some element_ty
                       | TUnknown | TMeta _ | TVar _ -> None
                       | _ -> None)
                |> Option.value ~default:element_ty
              in
              let collections =
                List.fold_right
                  (fun collection tail ->
                    Semantic_ir.Cons (collection.semantic_expr, tail))
                  fixed
                  (Semantic_ir.Apply
                     (Semantic_ir.Ident "List.of_seq", [ rest.semantic_expr ]))
              in
              Ok
                (typed_ir (TVector (TVector element_ty))
                   (Semantic_ir.Apply
                      ( Semantic_ir.Ident
                          "Lg_runtime.Runtime_seq.zip_vectors",
                        [ collections ] )))
        | _ ->
            Error.error
              ("apply mapv requires a statically typed variadic vector constructor, got "
              ^ Types.source_name constructor.ty))
  and compile_mutable_field_assignment scope env keyword target_form value_form =
    match compile_expr scope env target_form with
    | Error _ as error -> error
    | Ok target -> (
        match target.ty with
        | TRecord fields | TNamed_record { fields; _ } -> (
            match find_field keyword fields with
            | Some ({ mutable_ = true; _ } as field) -> (
                match
                  compile_expr scope
                    (Env.with_expected_type (Some field.ty) env)
                    value_form
                with
                | Error _ as error -> error
                | Ok value ->
                    let target_name = "__lg_field_target" in
                    let value_name = "__lg_field_value" in
                    let stored_value =
                      typed_ir value.ty (Semantic_ir.Ident value_name)
                    in
                    Result.map
                      (fun stored ->
                        typed_ir field.ty
                          (Semantic_ir.Let
                             ( [
                                 ( Semantic_ir.PVar target_name,
                                   target.semantic_expr );
                                 ( Semantic_ir.PVar value_name,
                                   value.semantic_expr );
                               ],
                               Semantic_ir.Sequence
                                 [
                                   Semantic_ir.SetField
                                     ( Semantic_ir.Ident target_name,
                                       field.ocaml_name,
                                       stored );
                                   Semantic_ir.Ident value_name;
                                 ] )))
                      (adapt_value_to_type env field.ty stored_value))
            | Some _ -> Error.error ("field " ^ keyword ^ " is not mutable")
            | None -> Error.error ("unknown field " ^ keyword))
        | _ -> Error.error "mutable field assignment expects a deftype value")
  and compile_call scope env name arg_forms =
    let name = Resolver.canonical_core_binding_name scope env name in
    let member_name =
      match String.rindex_opt name '/' with
      | None -> name
      | Some separator ->
          String.sub name (separator + 1) (String.length name - separator - 1)
    in
    if is_java_namespace name then java_interop_error name
    else if member_name = "->Eduction" then
      match arg_forms with
      | [ transducer; collection ] ->
          compile_expr scope env
            (FList
               [ FSymbol "__lg_transformer_sequence"; transducer; collection ])
      | _ -> Error.error "->Eduction expects a transducer and collection"
    else if member_name = "__lg_defer_seq" then
      match arg_forms with
      | [ thunk_form ] -> (
          match compile_function_arg scope env thunk_form with
          | Error _ as error -> error
          | Ok { ty = TFn ([], return_ty); semantic_expr = thunk; _ } ->
              let returned_name = "__lg_deferred_sequence" in
              let returned =
                typed_ir return_ty (Semantic_ir.Ident returned_name)
              in
              let normalized =
                match return_ty with
                | TNil ->
                    let element_ty =
                      match Env.expected_type env with
                      | Some (TSeq element_ty) -> element_ty
                      | _ -> TUnknown
                    in
                    Ok (element_ty, Semantic_ir.Ident "Seq.empty")
                | TNullable payload | TOcaml_app ("option", [ payload ]) ->
                    let payload_name = "__lg_deferred_sequence_value" in
                    let payload_value =
                      typed_ir payload (Semantic_ir.Ident payload_name)
                    in
                    Result.map
                      (fun (element_ty, sequence) ->
                        ( element_ty,
                          Semantic_ir.Match
                            ( Semantic_ir.Ident returned_name,
                              [ ( Semantic_ir.PConstructor ("None", None),
                                  Semantic_ir.Ident "Seq.empty" );
                                ( Semantic_ir.PConstructor
                                    ( "Some",
                                      Some
                                        (Semantic_ir.PVar payload_name) ),
                                  sequence );
                              ] ) ))
                      (Collection_capability.to_seq_expr env payload_value)
                | _ -> Collection_capability.to_seq_expr env returned
              in
              Result.map
                (fun (element_ty, sequence) ->
                  let normalized_thunk =
                    Semantic_ir.Fun
                      ( [],
                        Semantic_ir.Let
                          ( [ ( Semantic_ir.PVar returned_name,
                                Semantic_ir.Apply (thunk, []) ) ],
                            sequence ) )
                  in
                  typed_ir (TSeq element_ty)
                    (Semantic_ir.Apply
                       ( Semantic_ir.Ident
                           (match Env.target env with
                           | Target.Melange ->
                               "Lg_runtime.Runtime_seq_melange.defer"
                           | Target.Native | Target.Js_of_ocaml ->
                               "Lg_runtime.Runtime_seq.defer"),
                         [ normalized_thunk ] )))
                normalized
          | Ok _ -> Error.error "__lg_defer_seq expects a zero-argument function")
      | _ -> Error.error "__lg_defer_seq expects one argument"
    else if member_name = "__lg_with-meta" then
      compile_metadata_call scope env member_name arg_forms
    else
    let qualified_core = String.starts_with ~prefix:"clojure.core/" name in
    let name =
      if qualified_core then
          String.sub name
            (String.length "clojure.core/")
          (String.length name - String.length "clojure.core/")
      else name
    in
    let record_constructor_name name =
      String.length name > 2 && name.[0] = '-' && name.[1] = '>'
    in
    let name =
      match String.rindex_opt name '/' with
      | Some separator ->
          let member =
            String.sub name (separator + 1)
              (String.length name - separator - 1)
          in
          if record_constructor_name member then
            String.sub name 0 (separator + 1)
            ^ String.sub member 2 (String.length member - 2)
            ^ "."
          else name
      | None when record_constructor_name name ->
          String.sub name 2 (String.length name - 2) ^ "."
      | None -> name
    in
    match lookup_binding scope env name with
    | Ok _ when not (Resolver.starts_with_uppercase name) ->
        compile_named_function_call scope env name arg_forms
          | Error _
            when (not qualified_core) && Env.core_excluded ~scope name env ->
        Error.error ("unknown function " ^ name)
          | Ok _ | Error _ -> (
    let compile_args () = compile_args_for scope env arg_forms in
              let constructor ?(display_name = name) ?(constructor_name = name)
                  return_ty expected_arity =
      match compile_args () with
      | Error _ as err -> err
      | Ok args when List.length args <> expected_arity ->
          Error.error
                      (display_name ^ " expects "
                      ^ string_of_int expected_arity
                      ^ " arguments")
      | Ok args ->
          let payload =
            match args with
            | [] -> None
            | [ value ] -> Some value.semantic_expr
                      | values ->
                          Some
                            (Semantic_ir.Tuple
                               (List.map
                                  (fun value -> value.semantic_expr)
                                  values))
          in
          Ok
            (typed_ir (return_ty args)
               (Semantic_ir.Constructor (constructor_name, payload)))
    in
    match name with
    | "." -> (
        match arg_forms with
                  | [
                   FSymbol class_name; FList (FSymbol method_name :: method_args);
                  ]
          when String.contains class_name '.' ->
            compile_expr scope env
              (FList
                           (FSymbol (class_name ^ "/" ^ method_name)
                           :: method_args))
        | [ target; FList (FSymbol method_name :: method_args) ] ->
            compile_expr scope env
                        (FList
                           (FSymbol ("." ^ method_name) :: target :: method_args))
        | target :: FSymbol method_name :: method_args ->
            compile_expr scope env
                        (FList
                           (FSymbol ("." ^ method_name) :: target :: method_args))
        | _ -> Error.error ". expects a target and method")
    | "with-out-str" ->
        let writer_name = "__lg_with_out_str_writer" in
        let body_form =
          match arg_forms with
          | [] -> FSymbol "nil"
          | [ body ] -> body
          | body_forms -> FList (FSymbol "do" :: body_forms)
        in
        let writer_ty = TOcaml "Buffer.t" in
        let body_env =
          Env.add
            (Names.scoped_key scope "*out*")
            (Types.binding writer_name writer_ty)
            env
        in
        Result.map
          (fun body ->
            typed_ir TString
              (Semantic_ir.Let
                 ( [ ( Semantic_ir.PVar writer_name,
                       apply "Buffer.create" [ Semantic_ir.Int 64 ] );
                   ],
                   Semantic_ir.Sequence
                     [ body.semantic_expr;
                       apply "Buffer.contents"
                         [ Semantic_ir.Ident writer_name ];
                     ] )))
          (compile_expr scope body_env body_form)
    | "binding" -> (
        match arg_forms with
        | FVector [ FSymbol "*out*"; writer_form ] :: body_forms -> (
            match compile_expr scope env writer_form with
            | Error _ as error -> error
                      | Ok writer when Types.equal writer.ty (TOcaml "Buffer.t")
                        ->
                let writer_name = "__lg_bound_out" in
                let body_form =
                  match body_forms with
                  | [] -> FSymbol "nil"
                  | [ body ] -> body
                  | body_forms -> FList (FSymbol "do" :: body_forms)
                in
                let body_env =
                            Env.add
                              (Names.scoped_key scope "*out*")
                              (Types.binding writer_name writer.ty)
                              env
                in
                Result.map
                  (fun body ->
                              {
                                body with
                      semantic_expr =
                        Semantic_ir.Let
                                    ( [
                                        ( Semantic_ir.PVar writer_name,
                                writer.semantic_expr );
                            ],
                            body.semantic_expr );
                    })
                  (compile_expr scope body_env body_form)
            | Ok _ -> Error.error "binding *out* expects a writer")
        | FVector bindings :: body_forms ->
            let rec compile_bindings compiled = function
              | [] -> Ok (List.rev compiled)
              | FSymbol name :: value_form :: rest -> (
                  match Env.find_opt (Names.scoped_key scope name) env with
                  | Some
                      {
                        ty = TRef value_ty;
                        ocaml_name;
                        dynamically_bindable = true;
                        _;
                      } ->
                      Result.bind (compile_expr scope env value_form)
                        (fun value ->
                          if
                            Types.assignable ~policy:Host_boundary
                              ~expected:value_ty ~actual:value.ty
                          then
                            compile_bindings
                              ( ( ocaml_name,
                                  coerce_expression_to_type value_ty value.ty
                                    value.semantic_expr )
                                :: compiled )
                              rest
                          else
                            Error.error
                              ("binding " ^ name ^ " expects "
                             ^ Types.source_name value_ty ^ ", got "
                              ^ Types.source_name value.ty))
                  | Some _ ->
                      Error.error
                        ("binding expects a dynamic var, got " ^ name)
                  | None -> Error.error ("unknown dynamic var " ^ name))
              | _ -> Error.error "binding expects symbol/value pairs"
            in
            let body_form =
              match body_forms with
              | [] -> FSymbol "nil"
              | [ body ] -> body
              | body_forms -> FList (FSymbol "do" :: body_forms)
            in
            Result.bind (compile_bindings [] bindings) (fun bindings ->
                Result.map
                  (fun body ->
                    let expression =
                      List.fold_right
                        (fun (ocaml_name, value) body ->
                          apply "Lg_runtime.Runtime_binding.bind"
                            [
                              Semantic_ir.Ident ocaml_name;
                              value;
                              Semantic_ir.Fun ([], body);
                            ])
                        bindings body.semantic_expr
                    in
                    typed_ir body.ty expression)
                  (compile_expr scope env body_form))
        | _ -> Error.error "binding expects a binding vector and body"
                  )
    | "with-open" -> (
        match arg_forms with
        | FVector bindings :: body_forms when List.length bindings mod 2 = 0 ->
            let body_form =
              match body_forms with
              | [] -> FSymbol "nil"
              | [ body ] -> body
              | forms -> FList (FSymbol "do" :: forms)
            in
            compile_expr scope env
              (FList [ FSymbol "let"; FVector bindings; body_form ])
        | FVector _ :: _ ->
            Error.error "with-open expects symbol/value binding pairs"
        | _ -> Error.error "with-open expects a binding vector and body")
    | "js/parseInt" -> (
        match compile_args () with
        | Ok [ source; radix ]
                    when Types.equal source.ty TString
                         && Types.equal radix.ty TInt ->
            Ok
              (typed_ir TInt
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident
                        "Lg_runtime.Runtime_string.parse_int_radix",
                      [ source.semantic_expr; radix.semantic_expr ] )))
        | Ok _ -> Error.error "js/parseInt expects a string and radix"
        | Error _ as error -> error)
    | "js/Error." -> (
        match (Env.target env, compile_args ()) with
        | (Target.Melange | Target.Js_of_ocaml), Ok [ message ]
          when Types.equal message.ty TString ->
            Ok
              (typed_ir (TOcaml "exn")
                 (Semantic_ir.Constructor
                    ("Failure", Some message.semantic_expr)))
        | (Target.Melange | Target.Js_of_ocaml), Ok _ ->
            Error.error "js/Error expects a string message"
        | Target.Native, Ok _ ->
            Error.error "js/Error is only available on JavaScript targets"
        | _, (Error _ as error) -> error)
    | "js/Date." -> (
        match (Env.target env, arg_forms) with
        | Target.Melange, [] ->
            Ok
              (typed_ir (TOcaml "__lg_date_millis")
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident
                        "Lg_runtime.Runtime_int_melange.of_float_unchecked",
                                [
                                  Semantic_ir.Apply
                                    (Semantic_ir.Ident "Js.Date.now", []);
                                ] )))
        | Target.Js_of_ocaml, [] ->
            let date =
              Semantic_ir.Apply
                ( Semantic_ir.Ident "Js_of_ocaml.Js.Unsafe.new_obj",
                            [
                              Semantic_ir.Ident "Js_of_ocaml.Js.date_now";
                    Semantic_ir.Array [];
                  ] )
            in
            let milliseconds =
              Semantic_ir.Apply
                ( Semantic_ir.Ident "Js_of_ocaml.Js.Unsafe.meth_call",
                            [
                              date;
                              Semantic_ir.String "getTime";
                              Semantic_ir.Array [];
                            ] )
            in
            Ok
              (typed_ir (TOcaml "__lg_date_millis")
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "int_of_float",
                                [
                                  Semantic_ir.Apply
                          ( Semantic_ir.Ident "Js_of_ocaml.Js.to_float",
                                      [ milliseconds ] );
                                ] )))
        | Target.Native, [] ->
                      Error.error
                        "js/Date is only available on JavaScript targets"
        | _, _ -> Error.error "js/Date expects 0 arguments")
    | "current-time-millis" -> (
        match arg_forms with
        | [] ->
            let conversion =
              match Env.target env with
              | Target.Melange ->
                  "Lg_runtime.Runtime_int_melange.of_float_unchecked"
              | Target.Native | Target.Js_of_ocaml -> "int_of_float"
            in
            let current_time =
              match Env.target env with
              | Target.Melange ->
                  Semantic_ir.Apply (Semantic_ir.Ident "Js.Date.now", [])
              | Target.Native | Target.Js_of_ocaml ->
                  Semantic_ir.Infix
                    ( "*.",
                      Semantic_ir.Apply
                        (Semantic_ir.Ident "Unix.gettimeofday", []),
                      Semantic_ir.Float "1000." )
            in
            Ok
              (typed_ir TInt
                 (Semantic_ir.Apply
                    (Semantic_ir.Ident conversion, [ current_time ])))
        | _ -> Error.error "current-time-millis expects 0 arguments")
    | "js/performance.now" -> (
        match (Env.target env, arg_forms) with
        | Target.Melange, [] ->
            Ok
              (typed_ir TFloat
                 (Semantic_ir.Apply
                    (Semantic_ir.Ident "Lg_runtime.Runtime_time_melange.now", [])))
        | Target.Melange, _ ->
            Error.error "js/performance.now expects 0 arguments"
        | _, _ ->
            Error.error
              "js/performance.now is only available on the Melange target")
    | "js/isNaN" as function_name -> (
        match compile_args () with
        | Ok [ value ] ->
            Result.map
              (fun value ->
                typed_ir TBool
                  (Semantic_ir.Apply
                     (Semantic_ir.Ident "Float.is_nan", [ value ])))
              (adapt_value_to_type env TFloat value)
        | Ok _ -> Error.error (function_name ^ " expects 1 argument")
        | Error _ as error -> error)
    | "clj->js" -> (
        match compile_args () with
        | Ok [ value ] -> Ok value
        | Ok _ -> Error.error "clj->js expects 1 argument"
        | Error _ as error -> error)
    | "satisfies?" -> (
        match arg_forms with
        | [ FSymbol protocol_name; receiver_form ] -> (
            match
              ( Protocol.find_protocol_id scope env protocol_name,
                compile_expr scope env receiver_form )
            with
            | None, _ -> Error.error ("unknown protocol " ^ protocol_name)
            | _, (Error _ as error) -> error
            | Some _, Ok receiver when Types.is_dynamic receiver.ty ->
                Error.error
                  "satisfies? requires a statically typed receiver; define a \
                   closed sum type for alternative receiver types"
            | Some protocol_id, Ok receiver ->
                let expression =
                  if
                    Protocol_id.name protocol_id = "ISequential"
                    && Option.is_some
                         (Types.seqable_constraint_info receiver.ty)
                  then
                    match Types.seqable_constraint_info receiver.ty with
                    | Some ((`Optional | `Optional_sequential), _, _) -> (
                        match Semantic_ir.unlocated receiver.semantic_expr with
                        | Semantic_ir.Ident name ->
                            Semantic_ir.Match
                              ( Semantic_ir.Ident (name ^ "__seq_optional"),
                                [
                                  ( Semantic_ir.PConstructor ("None", None),
                                    Semantic_ir.Bool false );
                                  ( Semantic_ir.PConstructor
                                      ("Some", Some Semantic_ir.PAny),
                                    Semantic_ir.Bool true );
                                ] )
                        | _ -> Semantic_ir.Bool false)
                    | Some (`Required, _, _) ->
                        Semantic_ir.Sequence
                          [ receiver.semantic_expr; Semantic_ir.Bool true ]
                    | None -> assert false
                  else if has_protocol_constraint protocol_id receiver.ty then
                    match protocol_witness_expression protocol_id receiver with
                    | Some witness ->
                        Semantic_ir.Match
                          ( witness,
                            [
                              ( Semantic_ir.PConstructor ("None", None),
                                Semantic_ir.Bool false );
                              ( Semantic_ir.PConstructor
                                  ("Some", Some Semantic_ir.PAny),
                                Semantic_ir.Bool true );
                            ] )
                    | None -> Semantic_ir.Bool false
                  else
                    Semantic_ir.Sequence
                      [
                        receiver.semantic_expr;
                        Semantic_ir.Bool
                          (Protocol.type_satisfies env protocol_id receiver.ty);
                      ]
                in
                Ok (typed_ir TBool expression))
        | _ -> Error.error "satisfies? expects a protocol and value")
    | "__type-hint" -> (
        match arg_forms with
        | [ FSymbol annotation; value_form ] -> (
            match Type_annotation.of_param_annotation annotation with
            | Error _ as error -> error
            | Ok hinted_type ->
                let hinted_type =
                  Function_elaborator.infer_named_record scope env hinted_type
                in
                (match
                   compile_expr scope
                     (Env.with_expected_type (Some hinted_type) env)
                     value_form
                 with
                | Error _ as error -> error
                | Ok value ->
                          let specialize_hint actual_type =
                            match (hinted_type, actual_type) with
                            | TNamed_record hinted, TNamed_record actual
                              when Type_id.equal hinted.type_id actual.type_id ->
                                TNamed_record actual
                            | _ -> hinted_type
                          in
                          (match value.ty with
                          | TNullable actual_type
                          | TOcaml_app ("option", [ actual_type ])
                            when (match hinted_type with
                                 | TNullable _
                                 | TOcaml_app ("option", [ _ ]) ->
                                     false
                                 | _ -> true) ->
                              let hinted_type = specialize_hint actual_type in
                              let value_name = "__lg_hinted_optional_value" in
                              let payload = Semantic_ir.Ident value_name in
                              let narrowed =
                                if
                                  Types.is_dynamic actual_type
                                  || match actual_type with
                                     | TUnknown | TMeta _ | TVar _ | TRecord _ -> true
                                     | _ -> false
                                then
                                  dynamic_unpack env hinted_type payload
                                else if
                                  Types.assignable ~policy:Host_boundary
                                    ~expected:hinted_type ~actual:actual_type
                                then Ok payload
                                else
                                  let type_kind = function
                                    | TNamed_record record ->
                                        "named:" ^ record.type_name
                                    | TRecord _ -> "record"
                                    | ty
                                      when Types.is_dynamic ty ->
                                        "dynamic"
                                    | ty
                                      when Option.is_some
                                             (Types.protocol_constraint_info ty)
                                      ->
                                        "protocol"
                                    | _ -> "other"
                                  in
                                  Error.error
                                    ("type hint does not match optional value: "
                                   ^ Types.source_name actual_type ^ " -> "
                                   ^ Types.source_name hinted_type ^ " ("
                                   ^ type_kind actual_type ^ ")")
                              in
                              Result.map
                                (fun narrowed ->
                                  typed_ir (TNullable hinted_type)
                                    (Semantic_ir.Match
                                       ( value.semantic_expr,
                                         [ ( Semantic_ir.PConstructor
                                               ("None", None),
                                             Semantic_ir.Constructor
                                               ("None", None) );
                                           ( Semantic_ir.PConstructor
                                               ( "Some",
                                                 Some
                                                   (Semantic_ir.PVar value_name)
                                               ),
                                             Semantic_ir.Constructor
                                               ("Some", Some narrowed) );
                                         ] )))
                                narrowed
                          | _ ->
                          let hinted_type = specialize_hint value.ty in
                          if
                            Types.is_dynamic value.ty
                            && not (Types.is_dynamic hinted_type)
                          then
                            Error.error
                              "a type hint cannot narrow a dynamic value; \
                               define a closed sum type and match its \
                               constructors"
                          else
                Ok
                              {
                                value with
                    ty = hinted_type;
                    semantic_expr =
                                  Semantic_ir.Constraint
                                    ( value.semantic_expr,
                                      Types.ocaml_name hinted_type );
                  })))
        | _ -> Error.error "type hint expects metadata and a value")
    | ".toByteArray" -> (
        match compile_args () with
        | Ok [ output ] when Types.equal output.ty (TOcaml "Buffer.t") ->
            Ok
              (typed_ir TString
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Buffer.contents",
                      [ output.semantic_expr ] )))
        | Ok _ -> Error.error ".toByteArray expects a byte output stream"
        | Error _ as error -> error)
    | ".getBytes" -> (
        match compile_args () with
        | Ok [ value; encoding ]
          when Types.equal value.ty TString && Types.equal encoding.ty TString ->
            Ok
              (typed_ir TString
                 (Semantic_ir.Sequence
                    [ encoding.semantic_expr; value.semantic_expr ]))
        | Ok _ -> Error.error ".getBytes expects a string and encoding"
        | Error _ as error -> error)
    | ".write" | "__lg_write" -> (
        match compile_args () with
        | Ok [ writer; text ]
          when (Types.equal writer.ty (TOcaml "Buffer.t")
               || match writer.ty with TUnknown | TMeta _ | TVar _ -> true | _ -> false)
               && Types.equal text.ty TString ->
            Ok
              (typed_ir TUnit
                 (Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_print.write",
                      [ writer.semantic_expr; text.semantic_expr ] )))
        | Ok _ -> Error.error (name ^ " expects a writer and string")
        | Error _ as error -> error)
    | "__lg_pr-writer" -> (
        match compile_args () with
        | Ok [ value; writer; options ] ->
            let unresolved = function
              | TUnknown | TMeta _ | TVar _ -> true
              | _ -> false
            in
            if
              not
                (Types.equal writer.ty (TOcaml "Buffer.t")
                || unresolved writer.ty)
            then Error.error "pr-writer expects a Buffer.t writer"
            else if not (Types.equal options.ty TNil || unresolved options.ty)
            then Error.error "pr-writer options must be nil"
            else
              Ok
                (typed_ir TUnit
                   (Semantic_ir.Let
                      ( [ (Semantic_ir.PAny, options.semantic_expr) ],
                        Semantic_ir.Apply
                          ( Semantic_ir.Ident "Lg_runtime.Runtime_print.write",
                            [ writer.semantic_expr;
                              stringify_value scope env ~pr:true value;
                            ] ) )))
        | Ok _ -> Error.error "pr-writer expects 3 arguments"
        | Error _ as error -> error)
    | ".getClass" | ".getName" | ".compareTo" ->
        Error.error
          "Java reflection interop is not supported; use static LG types"
    | ".equals" ->
        Error.error
          "Java interop is not supported; use static LG types and functions \
           (.equals)"
    | ".getTime" -> (
        match compile_args () with
        | Ok [ ({ ty = TOcaml "__lg_date_millis"; _ } as date) ] ->
            Ok (typed_ir TInt date.semantic_expr)
        | Ok _ -> Error.error ".getTime expects a JavaScript Date"
        | Error _ as error -> error)
    | ".toString" -> (
        match (Env.target env, compile_args ()) with
        | (Target.Melange | Target.Js_of_ocaml), Ok [ value; radix ]
          when Types.equal value.ty TInt && Types.equal radix.ty TInt ->
            Ok
              (typed_ir TString
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident
                        "Lg_runtime.Runtime_string.int_to_string_radix",
                      [ value.semantic_expr; radix.semantic_expr ] )))
        | (Target.Melange | Target.Js_of_ocaml), Ok _ ->
            Error.error ".toString expects an int and radix"
        | Target.Native, Ok _ ->
            Error.error ".toString radix interop is only available on JavaScript targets"
        | _, (Error _ as error) -> error)
    | map_constructor
      when Option.is_some (map_record_constructor_type_name map_constructor) -> (
        let type_name =
          Option.get (map_record_constructor_type_name map_constructor)
        in
                  match Resolver.lookup_record_type scope env type_name with
        | Error _ as error -> error
        | Ok record -> (
            match compile_args () with
            | Error _ as error -> error
            | Ok [ source ] ->
                let source_binding =
                  match Semantic_ir.unlocated source.semantic_expr with
                  | Semantic_ir.Ident _ | Semantic_ir.Record _ -> None
                  | Semantic_ir.Apply
                      ( Semantic_ir.Ident
                          "Lg_runtime.Runtime_dynamic.map",
                        [ Semantic_ir.List _ ] ) ->
                      None
                  | _ ->
                      Some
                        ( "__lg_map_record_source",
                          source.semantic_expr )
                in
                let source =
                  match source_binding with
                  | None -> source
                  | Some (name, _) ->
                      {
                        source with
                        semantic_expr = Semantic_ir.Ident name;
                        record_values = None;
                      }
                in
                let source_field field =
                  if Types.is_record_extension_field field then
                    typed_ir field.ty
                      (Semantic_ir.Ident "Lg_runtime.Runtime_map.empty")
                  else
                  match source.record_values with
                  | Some values -> (
                      match
                        List.find_opt
                          (fun ((source_field : field), _) ->
                            source_field.keyword = field.keyword)
                          values
                      with
                      | Some (source_field, expression) ->
                          typed_ir source_field.ty expression
                      | None -> typed_ir TNil Semantic_ir.Unit)
                  | None -> (
                  match source.ty with
                  | TRecord fields | TNamed_record { fields; _ } -> (
                      match find_field field.keyword fields with
                      | Some source_field ->
                          typed_ir source_field.ty
                                      (Structural_map.field_expr source
                                         source_field)
                      | None -> typed_ir TNil Semantic_ir.Unit)
                  | ty when Types.is_dynamic ty ->
                      typed_ir ty
                        (Semantic_ir.Apply
                                     ( Semantic_ir.Ident
                                         "Lg_runtime.Runtime_dynamic.get",
                                       [
                                         source.semantic_expr;
                               Semantic_ir.Apply
                                 ( Semantic_ir.Ident
                                     "Lg_runtime.Runtime_dynamic.keyword",
                                             [
                                               Semantic_ir.String field.keyword;
                                             ] );
                             ] ))
                  | _ -> typed_ir TNil Semantic_ir.Unit)
                in
                let rec prepare values = function
                  | [] -> Ok (List.rev values)
                            | (field : field) :: rest -> (
                      let value = source_field field in
                      let packed =
                        if has_capability_constraint field.ty then
                          pack_constrained_value env field.ty value
                        else adapt_value_to_type env field.ty value
                      in
                                match packed with
                      | Error _ as error -> error
                      | Ok expression ->
                          prepare
                                      (( field,
                                         {
                                           value with
                                           ty = field.ty;
                                           semantic_expr = expression;
                                         } )
                            :: values)
                            rest)
                in
                Result.map
                  (fun values ->
                    let expression =
                      match values with
                      | [] -> Semantic_ir.Unit
                      | _ ->
                          Semantic_ir.Record
                            ( List.map
                                (fun ((field : field), value) ->
                                            ( field.ocaml_name,
                                              value.semantic_expr ))
                                values,
                              Some
                                          (record_type_application
                                             record.type_name
                                             record.type_arguments) )
                    in
                    let expression =
                      match source_binding with
                      | None -> expression
                      | Some (name, source_expression) ->
                          Semantic_ir.Let
                            ( [
                                ( Semantic_ir.PVar name,
                                  source_expression );
                              ],
                              expression )
                    in
                              {
                                (typed_ir (TNamed_record record) expression) with
                      record_values =
                        (match source_binding with
                        | Some _ -> None
                        | None ->
                            Some
                              (List.map
                                 (fun (field, value) ->
                                   (field, value.semantic_expr))
                                 values));
                    })
                  (prepare [] record.fields)
                      | Ok _ ->
                          Error.error (map_constructor ^ " expects 1 argument"))
                  )
    | constructor_name
              when String.ends_with ~suffix:"." constructor_name
                -> (
        let type_name =
                    String.sub constructor_name 0
                      (String.length constructor_name - 1)
        in
        match Env.find_opt type_name env with
                  | Some { host_reference = Some (Ocaml_module module_path); _ }
                    ->
                      compile_inferred_ocaml_call scope env
                        (module_path ^ ".create") arg_forms
                  | _ -> (
                      match Resolver.lookup_record_type scope env type_name with
        | Error _ as err -> err
        | Ok record -> (
            let constructor_fields =
              Types.record_constructor_fields record.fields
            in
            let rec compile_constructor_args compiled fields forms =
              match (fields, forms) with
              | [], [] -> Ok (List.rev compiled)
              | (field : field) :: fields, form :: forms ->
                  Result.bind
                    (compile_expr scope
                       (Env.with_expected_type (Some field.ty) env)
                       form)
                    (fun argument ->
                      compile_constructor_args (argument :: compiled) fields
                        forms)
              | _ ->
                  Error.error
                    (constructor_name ^ " expects "
                   ^ string_of_int (List.length constructor_fields)
                   ^ " arguments")
            in
            match
              compile_constructor_args [] constructor_fields arg_forms
            with
            | Error _ as err -> err
                          | Ok args -> (
                let is_empty_dynamic_map arg =
                                match
                                  Semantic_ir.unlocated arg.semantic_expr
                                with
                  | Semantic_ir.Apply
                                    ( Semantic_ir.Ident
                                        "Lg_runtime.Runtime_dynamic.map",
                        [ Semantic_ir.List [] ] ) ->
                      true
                  | _ -> false
                in
                let instantiated =
                  Types.instantiate_type_fields
                    ~templates:
                                    (List.map
                                       (fun (field : field) -> field.ty)
                         constructor_fields)
                    ~actuals:
                      (List.map2
                         (fun (field : field) arg ->
                           let actual =
                                           if is_empty_dynamic_map arg then
                                             TUnknown
                                           else arg.ty
                           in
                             match field.ty with
                             | TRef _ -> (
                                 match Types.constraint_value_type actual with
                                 | TRef inner -> TRef inner
                                 | _ -> TRef actual)
                             | _ -> actual)
                         constructor_fields args)
                    (TNamed_record record)
                in
                let record =
                  match instantiated with
                  | TNamed_record record -> record
                  | _ -> record
                in
                let constructor_fields =
                  Types.record_constructor_fields record.fields
                in
                let rec prepare_values values fields args =
                  match (fields, args) with
                  | [], [] -> Ok (List.rev values)
                                | (field : field) :: fields, arg :: args -> (
                      let packed =
                        match field.ty with
                        | TRef _
                          when (match
                                  Types.constraint_value_type arg.ty
                                with
                               | TRef _ -> true
                               | _ -> false) ->
                            adapt_value_to_type env field.ty arg
                        | TRef inner when Types.is_dynamic inner
                                        ->
                            Result.map
                              (fun value ->
                                Semantic_ir.Apply
                                                ( Semantic_ir.Ident "ref",
                                                  [ value ] ))
                              (pack_dynamic_value env inner arg)
                        | TRef inner ->
                            Result.map
                              (fun value ->
                                Semantic_ir.Apply
                                  (Semantic_ir.Ident "ref", [ value ]))
                              (adapt_value_to_type env inner arg)
                        | _ when Types.is_dynamic field.ty ->
                          pack_dynamic_value env field.ty arg
                                      | _
                                        when has_capability_constraint field.ty
                                        ->
                                          pack_constrained_value env field.ty
                                            arg
                                      | _
                                        when Types.is_dynamic arg.ty
                                             && not
                                                  (expects_dynamic_value
                                                     field.ty) ->
                                          dynamic_unpack env field.ty
                                            arg.semantic_expr
                                      | _ -> (
                          match field.ty with
                                          | TOcaml_app
                                              ( "Lg_runtime.Runtime_map.t",
                                                [ _; _ ] )
                                          | TVar _ | TMeta _ | TUnknown
                            when is_empty_dynamic_map arg ->
                              Ok
                                (Semantic_ir.Ident
                                   "Lg_runtime.Runtime_map.empty")
                                          | _ ->
                                              adapt_value_to_type env field.ty
                                                arg)
                      in
                                    match packed with
                      | Error _ as error -> error
                      | Ok semantic_expr ->
                          prepare_values
                                          (( field,
                                             {
                                               arg with
                                               ty = field.ty;
                                               semantic_expr;
                                             } )
                            :: values)
                            fields args)
                                | _ ->
                                    Error.error
                                      "record constructor arity mismatch"
                in
                              match prepare_values [] constructor_fields args with
                | Error _ as error -> error
                | Ok values ->
                let values =
                  match Types.find_record_extension_field record.fields with
                  | None -> values
                  | Some field ->
                      values
                      @ [
                          ( field,
                            typed_ir field.ty
                              (Semantic_ir.Ident
                                 "Lg_runtime.Runtime_map.empty") );
                        ]
                in
                let expression =
                  if values = [] then Semantic_ir.Unit
                  else
                    Semantic_ir.Record
                      ( List.map
                          (fun ((field : field), arg) ->
                                              ( field.ocaml_name,
                                                arg.semantic_expr ))
                          values,
                        Some
                                            (record_type_application
                                               record.type_name
                                               record.type_arguments) )
                in
                Ok
                  {
                                      (typed_ir (TNamed_record record)
                                         expression)
                                      with
                    record_values =
                      Some
                        (List.map
                                             (fun (field, arg) ->
                                               (field, arg.semantic_expr))
                           values);
                  }))))
    | "__deftype-field-set!" -> (
        match arg_forms with
        | [ FKeyword keyword; target_form; value_form ] ->
            compile_mutable_field_assignment scope env keyword target_form
              value_form
        | _ ->
            Error.error
              "mutable field assignment expects a field, deftype value, and value")
    | field_access when String.starts_with ~prefix:".-" field_access -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ target ] -> (
            let source_suffix =
              match arg_forms with
              | [ form ] -> (
                  match Source_context.find form with
                  | Some location ->
                      Printf.sprintf " at %s:%d:%d"
                        location.Location.loc_start.Lexing.pos_fname
                        location.Location.loc_start.Lexing.pos_lnum
                        (location.Location.loc_start.Lexing.pos_cnum
                        - location.Location.loc_start.Lexing.pos_bol)
                  | None -> "")
              | _ -> ""
            in
            let value_ty = Types.constraint_value_type target.ty in
            let target =
              if Types.equal value_ty target.ty then target
              else
                {
                  target with
                  ty = value_ty;
                  semantic_expr = constrained_argument_value target;
                }
            in
            let target =
              match target.ty with
              | TNullable inner | TOcaml_app ("option", [ inner ]) ->
                  typed_ir inner
                    (Semantic_ir.Apply
                       ( Semantic_ir.Ident "Option.get",
                         [ target.semantic_expr ] ))
              | _ -> target
            in
            let target =
              match target.ty with
              | TOcaml name when String.starts_with ~prefix:"__lg_record:" name ->
                  let source_name =
                    String.sub name (String.length "__lg_record:")
                      (String.length name - String.length "__lg_record:")
                  in
                  (match Resolver.lookup_record_type scope env source_name with
                  | Ok record -> { target with ty = TNamed_record record }
                  | Error _ -> (
                      match String.rindex_opt source_name '/' with
                      | Some index ->
                          let local_name =
                            String.sub source_name (index + 1)
                              (String.length source_name - index - 1)
                          in
                          (match
                             Resolver.lookup_record_type scope env local_name
                           with
                          | Ok record ->
                              { target with ty = TNamed_record record }
                          | Error _ -> target)
                      | None -> target))
              | _ -> target
            in
            let keyword =
              ":"
                        ^ String.sub field_access 2
                            (String.length field_access - 2)
            in
            let fields =
              match target.ty with
              | TRecord fields -> Some fields
              | TNamed_record record ->
                  let substitutions =
                    if
                      List.length record.type_parameters
                      = List.length record.type_arguments
                    then
                      Type_solver.of_list
                        (List.map2
                        (fun parameter argument ->
                          (Type_solver.Declared parameter, argument))
                        record.type_parameters record.type_arguments)
                    else Type_solver.empty
                  in
                  Some
                    (List.map
                       (fun (field : field) ->
                         {
                           field with
                           ty = Type_solver.apply substitutions field.ty;
                         })
                       record.fields)
              | _ -> None
            in
            match (target.ty, fields) with
            | (TRecord _ | TNamed_record _), Some fields -> (
                match find_field keyword fields with
                | None -> Error.error ("unknown field " ^ keyword)
                | Some ({ ty = TRef value_ty; _ } as field) ->
                    Ok
                      (typed_ir value_ty
                         (Semantic_ir.Prefix
                            ( "!",
                                        Structural_map.field_expr target field
                                      )))
                | Some field ->
                    Ok
                      (typed_ir field.ty
                         (Structural_map.field_expr target field)))
                      | ty, _ when Types.is_dynamic ty ->
                          Error.error
                            (field_access
                           ^ " requires a statically typed record; define a \
                              closed sum type and match its constructors")
                      | ty, _ ->
                          Error.error
                            (field_access ^ " expects a deftype value, got "
                           ^ Types.source_name ty ^ source_suffix)
                      )
        | Ok _ -> Error.error (field_access ^ " expects 1 argument"))
    | ".map" -> (
        match arg_forms with
        | [ receiver_form; callback_form ] -> (
            match compile_expr scope env receiver_form with
            | Error _ as error -> error
            | Ok { ty = receiver_ty; semantic_expr = receiver; _ } -> (
                match array_element_type receiver_ty with
                | None -> Error.error ".map expects an array and a unary function"
                | Some element_ty ->
                    let callback_env =
                      Env.with_expected_type
                        (Some (TFn ([ element_ty ], TUnknown))) env
                    in
                    (match compile_expr scope callback_env callback_form with
                    | Error _ as error -> error
                    | Ok
                        {
                          ty = TFn ([ parameter_ty ], return_ty);
                          semantic_expr = callback;
                          _;
                        }
                      when Types.assignable ~policy:Host_boundary
                             ~expected:parameter_ty ~actual:element_ty ->
                        let map, arguments =
                          match Env.target env with
                          | Target.Melange ->
                              ( "Lg_runtime.Runtime_array_melange.map",
                                [ receiver; callback ] )
                          | Target.Native | Target.Js_of_ocaml ->
                              ("Array.map", [ callback; receiver ])
                        in
                        Ok
                          (typed_ir (TArray return_ty) (apply map arguments))
                    | Ok _ ->
                        Error.error
                          ".map expects an array and a unary function")))
        | _ -> Error.error ".map expects an array and a unary function")
    | method_name
      when String.starts_with ~prefix:"." method_name
           && not
                (List.mem method_name
                   [ ".valAt"; ".containsKey"; ".entryAt" ]) -> (
        match compile_args () with
        | Error _ as err -> err
                  | Ok [ receiver ] when Types.is_dynamic receiver.ty -> (
                      let receiver_type =
                        match Types.dynamic_constraint_info receiver.ty with
                        | Some (TOcaml receiver_type) -> Some receiver_type
                        | _ -> None
                      in
                      match
                        Option.bind receiver_type (fun receiver_type ->
                            Host_interop.dynamic_instance_property
                              ~receiver_type ~method_name)
                      with
                      | Some property ->
                          let dynamic = Types.dynamic_constraint TUnknown in
                          let comparator_ty =
                            TFn ([ dynamic; dynamic ], TInt)
                          in
                          let value =
                            Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_dynamic.get",
                                [
                                  receiver.semantic_expr;
                                  Semantic_ir.Apply
                                    ( Semantic_ir.Ident
                                        "Lg_runtime.Runtime_dynamic.keyword",
                                      [ Semantic_ir.String property ] );
                                ] )
                          in
                          Result.map
                            (fun value -> typed_ir comparator_ty value)
                            (dynamic_unpack env comparator_ty value)
                      | None ->
                          Error.error
                            (method_name ^ " expects a host receiver, got "
                           ^ source_name receiver.ty))
                  | Ok ({ ty = TNamed_record record; _ } :: _ as args) -> (
            let source_method_name =
              String.sub method_name 1 (String.length method_name - 1)
            in
                      match
                        lookup_deftype_method scope env record
                          source_method_name (List.length args)
             with
            | Error _ ->
                Error.error
                            (method_name ^ " is not defined for "
                           ^ record.type_name)
            | Ok implementation -> (
                match implementation.ty with
                | TFn (parameter_tys, return_ty)
                  when List.length parameter_tys = List.length args
                       && List.for_all2
                            (fun expected arg ->
                              argument_compatible expected arg.ty)
                            parameter_tys args ->
                    let rec prepare acc parameter_tys args =
                      match (parameter_tys, args) with
                      | [], [] -> Ok (List.rev acc)
                                | expected :: parameter_tys, arg :: args -> (
                          let expression =
                            if Types.is_dynamic expected then
                              pack_dynamic_value env expected arg
                            else Ok arg.semantic_expr
                          in
                                    match expression with
                          | Error _ as error -> error
                          | Ok expression ->
                                        prepare (expression :: acc)
                                          parameter_tys args)
                      | _ -> assert false
                    in
                    Result.map
                      (fun arguments ->
                        typed_ir return_ty
                          (Semantic_ir.Apply
                                       ( Semantic_ir.Ident
                                           implementation.ocaml_name,
                               arguments )))
                      (prepare [] parameter_tys args)
                | _ ->
                    Error.error
                                (method_name
                               ^ " called with incompatible arguments")))
        | Ok ({ ty = TOcaml receiver_type; _ } :: _) -> (
                      match
                        Host_interop.instance_method ~receiver_type ~method_name
                      with
                      | None ->
                          Error.error ("unsupported host method " ^ method_name)
            | Some function_name ->
                          compile_inferred_ocaml_call scope env function_name
                            arg_forms)
        | Ok ({ ty = TOcaml_app (receiver_type, []); _ } :: _) -> (
                      match
                        Host_interop.instance_method ~receiver_type ~method_name
                      with
                      | None ->
                          Error.error ("unsupported host method " ^ method_name)
            | Some function_name ->
                          compile_inferred_ocaml_call scope env function_name
                            arg_forms)
        | Ok (receiver :: _) ->
            Error.error
              (method_name ^ " expects a host receiver, got "
             ^ source_name receiver.ty)
                  | Ok [] ->
                      Error.error (method_name ^ " expects a host receiver"))
    | ".valAt" -> java_interop_error ".valAt"
    | ".containsKey" -> java_interop_error ".containsKey"
    | ".entryAt" -> java_interop_error ".entryAt"
    | "__lg_reduced-predicate" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ] -> (
            match Types.reduced_element value.ty with
            | Some _ ->
                Ok
                  (typed_ir TBool
                     (Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Runtime_reduced.is_reduced",
                          [ value.semantic_expr ] )))
            | None ->
                Ok
                  (typed_ir TBool
                     (Semantic_ir.Sequence
                                  [
                                    value.semantic_expr; Semantic_ir.Bool false;
                                  ])))
        | Ok _ -> Error.error "reduced? expects 1 argument")
    | "__lg_unreduced" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ] -> (
            match Types.reduced_element value.ty with
            | Some inner ->
                Ok
                  (typed_ir inner
                     (Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Runtime_reduced.unreduced",
                          [ value.semantic_expr ] )))
            | None -> Ok value)
        | Ok _ -> Error.error "unreduced expects 1 arguments")
    | "__lg_ensure-reduced" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ] -> (
            match Types.reduced_element value.ty with
            | Some _ -> Ok value
            | None ->
                Ok
                  (typed_ir (Types.reduced value.ty)
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_reduced.reduced",
                          [ value.semantic_expr ] ))))
        | Ok _ -> Error.error "ensure-reduced expects 1 argument")
    | "__lg_force" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ] -> (
            match value.ty with
            | TOcaml_app ("Lazy.t", [ inner ]) ->
                Ok
                  (typed_ir inner
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lazy.force",
                          [ value.semantic_expr ] )))
            | _ -> Ok value)
        | Ok _ -> Error.error "force expects 1 argument")
    | "raise" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ arg ] ->
            Ok
              (typed_ir TUnknown
                           (Semantic_ir.Apply
                              (Semantic_ir.Ident "raise", [ arg.semantic_expr ])))
        | Ok _ -> Error.error "raise expects 1 arguments")
    | "ex-info" ->
        let compile_ex_info message data cause =
          let packed_data =
            match arg_forms with
            | [ _message; FMap [] ] | [ _message; FMap []; _ ] ->
                (* The empty map is constructed directly inside ex-info's
                   documented open runtime field. No static collection
                   crosses the dynamic boundary. *)
                Ok
                  (Semantic_ir.Apply
                     ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.map",
                       [ Semantic_ir.List [] ] ))
            | _ ->
                pack_dynamic_value env
                  (Types.dynamic_constraint TUnknown)
                  data
          in
          Result.map
            (fun data ->
              let callee, semantic_args =
                match cause with
                | None ->
                    ( "Lg_runtime.Runtime_exception.ex_info",
                      [ message.semantic_expr; data ] )
                | Some cause ->
                    ( "Lg_runtime.Runtime_exception.ex_info_with_cause",
                      [ message.semantic_expr; data; cause.semantic_expr ] )
              in
              typed_ir (TOcaml "exn")
                (Semantic_ir.Apply (Semantic_ir.Ident callee, semantic_args)))
            packed_data
        in
        (match compile_args () with
        | Error _ as error -> error
        | Ok ([ message; _ ] | [ message; _; _ ])
          when not (Types.equal message.ty TString) ->
            Error.error "ex-info message must be a string"
        | Ok [ _; _; cause ] when not (Types.equal cause.ty (TOcaml "exn")) ->
            Error.error "ex-info cause must be an exception"
        | Ok [ message; data ] -> compile_ex_info message data None
        | Ok [ message; data; cause ] ->
            compile_ex_info message data (Some cause)
        | Ok _ -> Error.error "ex-info expects 2 or 3 arguments")
    | "throw" -> (
        match compile_args () with
        | Error _ as error -> error
                  | Ok [ exception_ ]
                    when Types.equal exception_.ty (TOcaml "exn") ->
            Ok
              (typed_ir TUnknown
                 (Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_exception.throw",
                      [ exception_.semantic_expr ] )))
        | Ok [ _ ] -> Error.error "throw expects an exception"
        | Ok _ -> Error.error "throw expects 1 arguments")
    | "Some" ->
        constructor
                    (function
                      | [ value ] -> TOcaml_app ("option", [ value.ty ])
                      | _ -> TUnknown)
          1
              | "None" ->
                  constructor (fun _ -> TOcaml_app ("option", [ TUnknown ])) 0
    | "Ok" ->
        constructor
          (function
                      | [ value ] ->
                          TOcaml_app ("result", [ value.ty; TUnknown ])
            | _ -> TUnknown)
          1
    | "Error" ->
        constructor
          (function
                      | [ value ] ->
                          TOcaml_app ("result", [ TUnknown; value.ty ])
            | _ -> TUnknown)
          1
    | "seq-uncons" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ collection ] -> (
            match Collection_capability.to_seq_expr env collection with
            | Error _ -> Error.error "seq-uncons expects a seqable value"
            | Ok (element_ty, sequence) ->
                Ok
                  (typed_ir
                     (TNullable (TTuple [ element_ty; TSeq element_ty ]))
                     (apply "Lg_runtime.Runtime_seq.uncons" [ sequence ])))
        | Ok _ -> Error.error "seq-uncons expects 1 argument")
    | "seq-unfold-chunks" -> (
        match arg_forms with
        | [ step_form; initial_form ] ->
            Result.bind
              (compile_expr scope (Env.with_expected_type None env) initial_form)
              (fun initial ->
                let element_ty = Type_solver.fresh () in
                let step_return_ty =
                  TNullable
                    (TTuple
                       [
                         TArray element_ty;
                         TInt;
                         TInt;
                         TFn ([], initial.ty);
                       ])
                in
                let step_env =
                  Env.with_expected_type
                    (Some (TFn ([ initial.ty ], step_return_ty)))
                    env
                in
                Result.bind
                  (compile_expr scope step_env step_form)
                  (fun step ->
                    match step.ty with
                    | TFn ([ parameter_ty ], return_ty) -> (
                        match
                          (match return_ty with
                          | TNullable payload
                          | TOcaml_app ("option", [ payload ]) ->
                              Some payload
                          | _ -> None)
                        with
                        | Some
                            (TTuple
                              [
                                TArray actual_element_ty;
                                from_ty;
                                until_ty;
                                TFn ([], next_ty);
                              ])
                          when Types.assignable ~policy:Host_boundary
                                 ~expected:parameter_ty ~actual:initial.ty
                               && Types.assignable ~policy:Host_boundary
                                    ~expected:parameter_ty ~actual:next_ty
                               && Types.assignable ~policy:Host_boundary
                                    ~expected:TInt ~actual:from_ty
                               && Types.assignable ~policy:Host_boundary
                                    ~expected:TInt ~actual:until_ty ->
                            Ok
                              (typed_ir (TSeq actual_element_ty)
                                 (apply
                                    (match Env.target env with
                                    | Target.Melange ->
                                        "Lg_runtime.Runtime_seq_melange.unfold_chunks"
                                    | Target.Native | Target.Js_of_ocaml ->
                                        "Lg_runtime.Runtime_seq.unfold_chunks")
                                    [ step.semantic_expr;
                                      initial.semantic_expr ]))
                        | Some _ | None ->
                            Error.error
                              "seq-unfold-chunks expects a state step returning \
                               option<tuple<array<value>;int;int;fn<state>>>")
                    | _ ->
                        Error.error
                          "seq-unfold-chunks expects a state step returning \
                           option<tuple<array<value>;int;int;fn<state>>>"))
        | _ -> Error.error "seq-unfold-chunks expects 2 arguments")
    | ("seq-unfold" | "seq-unfold-unmemoized") as name -> (
        match arg_forms with
        | [ step_form; initial_form ] -> (
            match
              compile_expr scope (Env.with_expected_type None env) initial_form
            with
            | Error _ as err -> err
            | Ok initial -> (
                let step_env =
                  Env.with_expected_type
                    (Some (TFn ([ initial.ty ], TUnknown)))
                    env
                in
                match compile_expr scope step_env step_form with
                | Error _ as err -> err
                | Ok
                    {
                      ty =
                        TFn
                          ( [ parameter_ty ],
                            TOcaml_app
                              ("option", [ TTuple [ element_ty; next_ty ] ]) );
                      semantic_expr = step;
                      _;
                    }
                  when Types.assignable ~policy:Host_boundary
                         ~expected:parameter_ty ~actual:initial.ty
                       && Types.assignable ~policy:Host_boundary
                            ~expected:parameter_ty ~actual:next_ty ->
                    Ok
                      (typed_ir (TSeq element_ty)
                         (apply
                            (match Env.target env with
                            | Target.Melange ->
                                if String.equal name "seq-unfold" then
                                  "Lg_runtime.Runtime_seq_melange.unfold_memoized"
                                else "Lg_runtime.Runtime_seq_melange.unfold"
                            | Target.Native | Target.Js_of_ocaml ->
                                if String.equal name "seq-unfold" then
                                  "Lg_runtime.Runtime_seq.unfold_memoized"
                                else "Seq.unfold")
                            [ step; initial.semantic_expr ]))
                | Ok _ ->
                    Error.error
                      (name
                      ^ " expects a state step function and initial state")))
        | _ -> Error.error (name ^ " expects 2 arguments"))
    | ("uncurried-call" | "uncurried-compare") as name -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok
                      [
                        {
                          ty = TFn ([ left_ty; right_ty ], return_ty);
                          semantic_expr = fn;
                          _;
                        };
              left;
                        right;
                      ]
                    when Types.assignable ~policy:Host_boundary
                           ~expected:left_ty ~actual:left.ty
                         && Types.assignable ~policy:Host_boundary
                              ~expected:right_ty ~actual:right.ty ->
            Ok
              (typed_ir return_ty
                 (match Env.target env with
                 | Target.Melange ->
                     apply "Lg_runtime.Runtime_array_melange.call2"
                       [ fn; Semantic_ir.Int 0; left.semantic_expr;
                         right.semantic_expr ]
                 | Target.Native | Target.Js_of_ocaml ->
                     Semantic_ir.Apply
                       (fn, [ left.semantic_expr; right.semantic_expr ])))
        | Ok _ ->
            Error.error
              (name
                       ^ " expects a binary function and two compatible \
                          arguments"))
    | "as-ordering" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok
            [
              ({ ty = TFn ([ left_ty; right_ty ], _); _ } as comparator);
            ] ->
            let expected = TFn ([ left_ty; right_ty ], TOcaml "int") in
            Result.map (typed_ir expected)
              (adapt_value_to_type env expected comparator)
        | Ok _ -> Error.error "as-ordering expects a binary function")
    | ("seq-flat-map" | "seq-flat-map-rev") as name -> (
        match compile_args () with
        | Error _ as err -> err
                  | Ok
                      [
                        {
                          ty = TFn ([ parameter_ty ], return_type);
                          semantic_expr = fn;
                          _;
                        };
                        collection;
                      ] -> (
                      match (array_element_type collection.ty, return_type) with
                      | ( Some element_ty,
                          ( TSeq return_ty
                          | TOcaml_app (("Seq.t" | "Seq"), [ return_ty ]) ) )
                        when Types.assignable ~policy:Host_boundary
                               ~expected:parameter_ty ~actual:element_ty ->
                let flattened =
                  apply "Seq.flat_map"
                    [
                      fn;
                      apply
                        (if name = "seq-flat-map-rev" then
                           "Lg_runtime.Runtime_seq.of_array_rev"
                         else "Lg_runtime.Runtime_seq.of_array")
                        [ collection.semantic_expr ];
                    ]
                in
                Ok
                  (typed_ir (TSeq return_ty)
                     (apply "Lg_runtime.Runtime_seq.memoize" [ flattened ]))
            | Some element_ty, TUnknown
                        when Types.assignable ~policy:Host_boundary
                               ~expected:parameter_ty ~actual:element_ty ->
                let flattened =
                  apply "Seq.flat_map"
                    [
                      fn;
                      apply
                        (if name = "seq-flat-map-rev" then
                           "Lg_runtime.Runtime_seq.of_array_rev"
                         else "Lg_runtime.Runtime_seq.of_array")
                        [ collection.semantic_expr ];
                    ]
                in
                Ok
                  (typed_ir (TSeq TUnknown)
                     (apply "Lg_runtime.Runtime_seq.memoize" [ flattened ]))
            | _ ->
                Error.error
                  (name
                           ^ " expects a sequence function and compatible array"
                            ))
        | Ok _ -> Error.error (name ^ " expects 2 arguments"))
    | "__lg_make-array" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ size; initial ]
          when Types.equal size.ty TInt
               && not (Types.contains_dynamic initial.ty) ->
            Ok
              (typed_ir (TArray initial.ty)
                 (apply "Array.make"
                    [ size.semantic_expr; initial.semantic_expr ]))
        | Ok [ size; _ ] when not (Types.equal size.ty TInt) ->
            Error.error "make-array size must be int"
        | Ok [ _; _ ] ->
            Error.error
              "make-array initial value must have a static type"
        | Ok [ _ ] ->
            Error.error
              "make-array requires a size and a statically typed initial value"
        | Ok _ -> Error.error "make-array expects a size and initial value")
    | "__lg_array" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [] -> Error.error "empty OCaml array requires a type"
        | Ok (first :: rest as values) ->
            if
              List.for_all
                (fun value ->
                            Types.assignable ~policy:Host_boundary
                              ~expected:first.ty ~actual:value.ty
                            || Types.assignable ~policy:Host_boundary
                                 ~expected:value.ty ~actual:first.ty)
                rest
            then
              Ok
                (typed_ir (TArray first.ty)
                             (ordered_array_expression
                                (List.map
                                   (fun value -> value.semantic_expr)
                                   values)))
                      else
                        Error.error
                          "OCaml array elements must have the same type")
    | "array-of" -> (
        match arg_forms with
        | [ FKeyword keyword ] -> (
            match Type_annotation.of_keyword keyword with
            | Error _ as err -> err
                      | Ok element_ty ->
                          Ok
                            (typed_ir (TArray element_ty) (Semantic_ir.Array []))
                      )
        | _ -> Error.error "array-of expects one type")
    | "tuple-get" -> (
        match arg_forms with
        | [ tuple_form; index_form ] -> (
            match compile_expr scope env tuple_form with
            | Error _ as error -> error
            | Ok tuple -> (
                match tuple.ty with
                | TTuple element_tys -> (
                    match index_form with
                    | FInt index
                      when index >= 0 && index < List.length element_tys ->
                        let value_name =
                          "__lg_tuple_item_" ^ string_of_int index
                        in
                        let patterns =
                          List.mapi
                            (fun element_index _ ->
                              if element_index = index then
                                Semantic_ir.PVar value_name
                              else Semantic_ir.PAny)
                            element_tys
                        in
                        Ok
                          (typed_ir (List.nth element_tys index)
                             (Semantic_ir.Match
                                ( tuple.semantic_expr,
                                  [
                                    ( Semantic_ir.PTuple patterns,
                                      Semantic_ir.Ident value_name );
                                  ] )))
                    | FInt _ ->
                        Error.error "tuple-get index is out of bounds"
                    | _ ->
                        Error.error
                          "tuple-get tuple index must be an integer literal")
                | TArray element_ty -> (
                    match compile_expr scope env index_form with
                    | Error _ as error -> error
                    | Ok index when Types.equal index.ty TInt ->
                        let element_ty =
                          Types.constraint_value_type element_ty
                        in
                        Ok
                          (typed_ir element_ty
                             (apply "Array.get"
                                [ tuple.semantic_expr; index.semantic_expr ]))
                    | Ok _ ->
                        Error.error "tuple-get array index must be int")
                | _ ->
                    Error.error
                      "tuple-get expects a statically typed tuple or array"))
        | _ -> Error.error "tuple-get expects 2 arguments")
    | ("__lg_aget" | "unsafe-aget") as name -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ array; index ] -> (
            let compile_index index =
              if name = "__lg_aget" then compile_static_array_index index
              else if Types.equal index.ty TInt then Ok index.semantic_expr
              else if Types.equal index.ty TFloat then
                Ok (apply "int_of_float" [ index.semantic_expr ])
              else if Types.is_dynamic index.ty then
                Ok
                  (apply "Lg_runtime.Runtime_dynamic.as_int"
                     [ index.semantic_expr ])
              else if
                Types.equal index.ty TUnknown
                || match index.ty with TMeta _ | TVar _ -> true | _ -> false
              then Ok index.semantic_expr
              else Error.error "OCaml array index must be int"
            in
            let dynamic_target = Types.is_dynamic array.ty in
            if dynamic_target then
              let dynamic = Types.dynamic_constraint TUnknown in
              Result.bind (pack_dynamic_value env dynamic array) (fun array ->
                  if Types.is_dynamic index.ty then
                    Ok
                      (typed_ir dynamic
                         (apply "Lg_runtime.Runtime_dynamic.indexed_get"
                            [ array; index.semantic_expr ]))
                  else
                    Result.map
                      (fun index ->
                        typed_ir dynamic
                          (apply "Lg_runtime.Runtime_dynamic.array_get"
                             [ array; index ]))
                      (compile_index index))
            else
              match array_element_type array.ty with
              | Some element_ty ->
                let element_ty = Types.constraint_value_type element_ty in
                Result.map
                  (fun index ->
                    typed_ir element_ty
                      (apply
                         (if name = "__lg_aget" then "Array.get"
                          else "Array.unsafe_get")
                         [ array.semantic_expr; index ]))
                  (compile_index index)
              | None ->
                  Error.error
                    ((if name = "__lg_aget" then "array read" else name)
                   ^ " expects an OCaml array"))
        | Ok _ ->
            Error.error
              ((if name = "__lg_aget" then "array read" else name)
             ^ " expects 2 arguments"))
    | ("__lg_aset" | "unsafe-aset") as name -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ array; index; value ] -> (
            let index =
              if name = "__lg_aset" then compile_static_array_index index
              else if Types.equal index.ty TInt then Ok index.semantic_expr
              else if Types.equal index.ty TFloat then
                Ok (apply "int_of_float" [ index.semantic_expr ])
              else if Types.is_dynamic index.ty then
                Ok
                  (apply "Lg_runtime.Runtime_dynamic.as_int"
                     [ index.semantic_expr ])
              else if
                Types.equal index.ty TUnknown
                || match index.ty with TMeta _ | TVar _ -> true | _ -> false
              then Ok index.semantic_expr
              else Error.error "OCaml array index must be int"
            in
            Result.bind index (fun index ->
            if Types.is_dynamic array.ty then
              let dynamic = Types.dynamic_constraint TUnknown in
              match
                ( pack_dynamic_value env dynamic array,
                  pack_dynamic_value env dynamic value )
              with
              | (Error _ as error), _ | _, (Error _ as error) -> error
              | Ok array, Ok value ->
                  Ok
                    (typed_ir TUnit
                       (apply
                          (if name = "__lg_aset" then
                             "Lg_runtime.Runtime_dynamic.array_set"
                           else
                             "Lg_runtime.Runtime_dynamic.array_unsafe_set")
                          [ array; index; value ]))
            else
            match array_element_type array.ty with
            | Some element_ty ->
                let element_ty = Types.constraint_value_type element_ty in
                let value =
                  if has_capability_constraint value.ty then
                    {
                      value with
                      ty = Types.constraint_value_type value.ty;
                      semantic_expr = constrained_argument_value value;
                    }
                  else value
                in
                if Types.is_dynamic element_ty then
                  Result.map
                    (fun value ->
                      typed_ir TUnit
                        (apply
                           (if name = "__lg_aset" then "Array.set"
                            else "Array.unsafe_set")
                           [
                             array.semantic_expr;
                             index;
                             value;
                           ]))
                    (pack_dynamic_value env element_ty value)
                else if
                  not
                              (Types.assignable ~policy:Host_boundary
                                 ~expected:element_ty ~actual:value.ty)
                then
                            Error.error
                              "OCaml array value must match element type"
                else
                  if name = "__lg_aset" then
                    let value_name = "__lg_aset_value" in
                    Ok
                      (typed_ir value.ty
                         (Semantic_ir.Let
                            ( [
                                ( Semantic_ir.PVar value_name,
                                  value.semantic_expr );
                              ],
                              Semantic_ir.Sequence
                                [
                                  apply "Array.set"
                                    [
                                      array.semantic_expr;
                                      index;
                                      Semantic_ir.Ident value_name;
                                    ];
                                  Semantic_ir.Ident value_name;
                                ] )))
                  else
                    Ok
                      (typed_ir TUnit
                         (apply "Array.unsafe_set"
                            [
                              array.semantic_expr;
                              index;
                              value.semantic_expr;
                            ]))
            | None ->
                Error.error
                  ((if name = "__lg_aset" then "array write" else name)
                 ^ " expects an OCaml array")))
        | Ok _ ->
            Error.error
              ((if name = "__lg_aset" then "array write" else name)
             ^ " expects 3 arguments"))
    | "__lg_array-predicate" | "__lg_array-value-predicate" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ] ->
            let value_ty = Types.constraint_value_type value.ty in
            Ok
              (typed_ir TBool
                 (if uses_dynamic_value_storage value.ty then
                    apply "Lg_runtime.Runtime_dynamic.is_array"
                      [ constrained_argument_value value ]
                  else
                    Semantic_ir.Sequence
                      [
                        constrained_argument_value value;
                        Semantic_ir.Bool
                          (match value_ty with TArray _ -> true | _ -> false);
                      ]))
        | Ok _ ->
            let source_name =
              if name = "__lg_array-predicate" then "array?"
              else "array-value?"
            in
            Error.error (source_name ^ " expects 1 argument"))
    | "__lg_atom" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ] ->
            let value_ty =
              match (arg_forms, Env.expected_type env) with
              | [ FVector [] ], Some (TRef (TVector _ as expected_ty)) ->
                  Ok expected_ty
              | [ FSymbol "nil" ],
                Some
                  (TRef
                    ((TNullable _ | TOcaml_app ("option", [ _ ])) as expected_ty))
                ->
                  Ok expected_ty
              | [ FSymbol "nil" ], _ ->
                  Error.error
                    "atom nil requires an explicit option element type, for \
                     example ^:ref<option<int>>"
              | _ -> Ok value.ty
            in
            Result.bind value_ty (fun value_ty ->
                Result.map
                  (fun stored ->
                    typed_ir (TRef value_ty) (apply "ref" [ stored ]))
                  (if Types.is_dynamic value_ty then
                     pack_dynamic_value env value_ty value
                   else Ok value.semantic_expr))
        | Ok _ -> Error.error "atom expects 1 argument")
    | "weak-ref" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ]
          when weak_referenceable_type
                 (Types.constraint_value_type value.ty) ->
            let value_ty = Types.constraint_value_type value.ty in
            let value_expression =
              if Types.equal value_ty value.ty then value.semantic_expr
              else constrained_argument_value value
            in
            let make =
              match Env.target env with
              | Target.Melange ->
                  "Lg_runtime.Runtime_weak_melange.make"
              | Target.Native | Target.Js_of_ocaml ->
                  "Lg_runtime.Runtime_weak_stdlib.make"
            in
            Ok
              (typed_ir (Types.weak_type value_ty)
                 (apply make [ value_expression ]))
        | Ok [ _ ] -> Error.error "weak-ref expects a heap value"
        | Ok _ -> Error.error "weak-ref expects 1 argument")
    | "__lg_weak-deref" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ reference ] -> (
            match Types.weak_element reference.ty with
            | Some value_ty ->
                Ok
                  (typed_ir (TOcaml_app ("option", [ value_ty ]))
                     (apply "Lg_runtime.Runtime_weak.get"
                        [ reference.semantic_expr ]))
            | None ->
                Error.error
                  ("weak-deref expects a weak reference, got "
                 ^ Types.source_name reference.ty))
        | Ok _ -> Error.error "weak-deref expects 1 argument")
    | "__lg_weak-clear!" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ reference ] -> (
            match Types.weak_element reference.ty with
            | Some _ ->
                Ok
                  (typed_ir TUnit
                     (apply "Lg_runtime.Runtime_weak.clear"
                        [ reference.semantic_expr ]))
            | None ->
                Error.error
                  ("weak-clear! expects a weak reference, got "
                 ^ Types.source_name reference.ty))
        | Ok _ -> Error.error "weak-clear! expects 1 argument")
    | "tuple" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok ([] | [ _ ]) ->
            Error.error "tuple expects at least 2 values"
        | Ok values ->
            Ok
              (typed_ir
                 (TTuple (List.map (fun value -> value.ty) values))
                           (Semantic_ir.Tuple
                              (List.map
                                 (fun value -> value.semantic_expr)
                                 values))))
    | "record" -> (
        let field_value record field_form =
          match field_form with
          | FList [ FSymbol field_name; value_form ] -> (
              let ocaml_name = Names.sanitize_name field_name in
              match
                List.find_opt
                            (fun (field : field) ->
                              field.ocaml_name = ocaml_name)
                  record.fields
              with
                        | None ->
                            Error.error ("unknown record field " ^ field_name)
              | Some field -> (
                  match
                    compile_expr scope
                      (Env.with_expected_type (Some field.ty) env)
                      value_form
                  with
                  | Error _ as err -> err
                  | Ok value -> Ok (field, value)))
          | _ -> Error.error "record fields must be (name value)"
        in
        let rec compile_fields record acc seen = function
          | [] -> Ok (List.rev acc)
          | field_form :: rest -> (
              match field_value record field_form with
              | Error _ as err -> err
              | Ok ((field, _value) as pair) ->
                  if List.mem field.ocaml_name seen then
                    Error.error "duplicate record field name"
                            else
                              compile_fields record (pair :: acc)
                                (field.ocaml_name :: seen) rest)
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
                              if missing <> [] then
                                Error.error "record value is missing fields"
                    else
                      let instantiated_record =
                        let inferred_type_variable name =
                          let length = String.length name in
                          length > 1 && name.[0] = 'g'
                          && String.for_all
                               (function '0' .. '9' | '_' -> true | _ -> false)
                               (String.sub name 1 (length - 1))
                        in
                        match Env.expected_type env with
                        | Some (TNamed_record expected)
                          when Type_id.equal expected.type_id record.type_id
                               && not
                                    (List.exists
                                       (function
                                         | Type_solver.Metavariable _ -> true
                                         | Type_solver.Declared name ->
                                             inferred_type_variable name)
                                       (List.concat_map Type_solver.variables
                                          expected.type_arguments)) ->
                            expected
                        | _ -> (
                            match
                              Types.instantiate_type_fields
                                ~templates:
                                  (List.map
                                     (fun ((field : field), _) -> field.ty)
                                     values)
                                ~actuals:
                                  (List.map (fun (_, value) -> value.ty) values)
                                (TNamed_record record)
                            with
                            | TNamed_record record -> record
                            | _ -> record)
                      in
                      let values =
                        let rec specialize acc values forms =
                          match (values, forms) with
                          | [], [] -> Ok (List.rev acc)
                          | ((field : field), value) :: values, form :: forms -> (
                              match
                                Types.find_field field.keyword
                                  instantiated_record.fields
                              with
                              | Some expected_field
                                when (match expected_field.ty with
                                     | TFn _ ->
                                         concrete_nominal_type_argument
                                           expected_field.ty
                                         && not
                                              (concrete_nominal_type_argument
                                                 field.ty)
                                     | _ -> false) -> (
                                  match field_value instantiated_record form with
                                  | Error _ as error -> error
                                  | Ok specialized ->
                                      specialize (specialized :: acc) values forms)
                              | Some _ | None ->
                                  specialize ((field, value) :: acc) values forms)
                          | _ -> assert false
                        in
                        specialize [] values field_forms
                      in
                      let rec adapt_fields adapted = function
                        | [] -> Ok (List.rev adapted)
                        | ((field : field), value) :: rest -> (
                            match
                              Types.find_field field.keyword
                                instantiated_record.fields
                            with
                            | None -> assert false
                            | Some expected_field ->
                                if
                                  match (expected_field.ty, value.ty) with
                                  | TFn (expected_params, _),
                                    TFn (actual_params, _) ->
                                      callback_parameters_need_adapter
                                        expected_params actual_params
                                      && callback_payload_parameters_compatible
                                           expected_params actual_params
                                  | _ -> false
                                then
                                  Result.bind
                                    (adapt_dynamic_callback env expected_field.ty
                                       value)
                                    (fun semantic_expr ->
                                      adapt_fields
                                        ( ( expected_field,
                                            {
                                              value with
                                              ty = expected_field.ty;
                                              semantic_expr;
                                            } )
                                        :: adapted )
                                        rest)
                                else if
                                  Option.is_none
                                    (optional_payload expected_field.ty)
                                  && Option.fold ~none:false
                                       ~some:
                                         (argument_compatible
                                            expected_field.ty)
                                       (optional_payload value.ty)
                                then
                                  let actual_inner =
                                    optional_payload value.ty |> Option.get
                                  in
                                  let semantic_expr =
                                    coerce_expression_to_type expected_field.ty
                                      actual_inner
                                      (Semantic_ir.Apply
                                         ( Semantic_ir.Ident "Option.get",
                                           [ value.semantic_expr ] ))
                                  in
                                  adapt_fields
                                    ( ( expected_field,
                                        {
                                          value with
                                          ty = expected_field.ty;
                                          semantic_expr;
                                        } )
                                    :: adapted )
                                    rest
                                else if
                                  function_has_host_int_return_boundary
                                    expected_field.ty value.ty
                                then
                                  Result.bind
                                    (adapt_value_to_type env expected_field.ty
                                       value)
                                    (fun semantic_expr ->
                                      adapt_fields
                                        ( ( expected_field,
                                            {
                                              value with
                                              ty = expected_field.ty;
                                              semantic_expr;
                                            } )
                                        :: adapted )
                                        rest)
                                else
                                  adapt_fields ((field, value) :: adapted) rest)
                      in
                      Result.bind values (fun values ->
                      Result.map
                        (fun values ->
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
                                         record.type_arguments) )))
                            with
                            record_values =
                              Some
                                (List.map
                                   (fun ((field : field), value) ->
                                     (field, value.semantic_expr))
                                   values);
                          })
                        (adapt_fields [] values))))
        | _ -> Error.error "record expects a record type and fields")
    | "__lg_add" | "__lg_subtract" | "__lg_multiply" | "__lg_divide" -> (
        let operator =
          match name with
          | "__lg_add" -> "+"
          | "__lg_subtract" -> "-"
          | "__lg_multiply" -> "*"
          | "__lg_divide" -> "/"
          | _ -> assert false
        in
        match compile_args () with
        | Error _ as err -> err
        | Ok args -> (
            if Result.is_ok (Core_int.expect_int_args operator args) then
              Core_int.compile_operator operator args
            else if Core_float.expect_float_args args then
              Core_float.compile_operator operator args
            else if
              List.for_all
                (fun arg ->
                   Core_float.accepts_mixed_numeric arg.ty)
                args
            then
              Core_float.compile_operator operator
                (List.map Core_float.widen_to_float args)
            else
              match Core_int.expect_int_args operator args with
              | Error _ as err -> err
                        | Ok () -> assert false))
    | "__lg_rand" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [] ->
            Ok
              (typed_ir TFloat
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_random.rand",
                      [ Semantic_ir.Float "1." ] )))
        | Ok [ { ty = TInt; semantic_expr; _ } ] ->
            Ok
              (typed_ir TFloat
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_random.rand",
                      [ apply "float_of_int" [ semantic_expr ] ] )))
        | Ok [ { ty = TFloat; semantic_expr; _ } ] ->
            Ok
              (typed_ir TFloat
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_random.rand",
                      [ semantic_expr ] )))
        | Ok [ _ ] -> Error.error "rand expects a numeric bound"
        | Ok _ -> Error.error "rand expects zero or one argument")
    | "__lg_int" | "__lg_long" -> (
        let source_name = if name = "__lg_int" then "int" else "long" in
        match compile_args () with
        | Error _ as err -> err
        | Ok [ ({ ty = TInt; _ } as value) ] -> Ok value
        | Ok [ { ty = TFloat; semantic_expr; _ } ] ->
            Ok
              (typed_ir TInt
                 (apply "int_of_float" [ semantic_expr ]))
        | Ok [ { ty = TOcaml "int64"; semantic_expr; _ } ] ->
            Ok (typed_ir TInt (apply "Int64.to_int" [ semantic_expr ]))
        | Ok [ { ty; semantic_expr; _ } ] when Types.is_dynamic ty ->
            Ok
              (typed_ir TInt
                 (Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_dynamic.to_int",
                      [ semantic_expr ] )))
                  | Ok [ ({ ty = TUnknown | TMeta _ | TVar _; _ } as value) ] ->
            Ok { value with ty = TInt }
        | Ok [ _ ] -> Error.error (source_name ^ " expects a numeric value")
        | Ok _ -> Error.error (source_name ^ " expects 1 argument"))
    | "__lg_double" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ { ty = TInt; semantic_expr; _ } ] ->
            Ok
              (typed_ir TFloat
                 (apply "float_of_int" [ semantic_expr ]))
        | Ok [ ({ ty = TFloat; _ } as value) ] -> Ok value
        | Ok [ _ ] -> Error.error "double expects a numeric value"
        | Ok _ -> Error.error "double expects 1 argument")
    | "__lg_ex-message" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ { ty = TOcaml "exn"; semantic_expr; _ } ] ->
            Ok
              (typed_ir (TNullable TString)
                 (apply "Lg_runtime.Runtime_exception.message"
                    [ semantic_expr ]))
        | Ok [ _ ] -> Error.error "ex-message expects an exception"
        | Ok _ -> Error.error "ex-message expects 1 arguments")
    | "__lg_ex-cause" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ { ty = TOcaml "exn"; semantic_expr; _ } ] ->
            Ok
              (typed_ir (TNullable (TOcaml "exn"))
                 (apply "Lg_runtime.Runtime_exception.cause" [ semantic_expr ]))
        | Ok [ _ ] -> Error.error "ex-cause expects an exception"
        | Ok _ -> Error.error "ex-cause expects 1 arguments")
    | "__lg_re-pattern" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ ({ ty = TRegex; _ } as expression) ] -> Ok expression
        | Ok [ { ty = TString; semantic_expr; _ } ] ->
            Ok
              (typed_ir TRegex
                 (apply "Lg_runtime.Runtime_string.regex" [ semantic_expr ]))
        | Ok [ _ ] -> Error.error "re-pattern expects a string or regex"
        | Ok _ -> Error.error "re-pattern expects 1 argument")
    | "reify" -> (
        match arg_forms with
                  | FSymbol protocol_name :: method_forms -> (
            let compile_method = function
                        | FList (FSymbol method_name :: params :: body_forms)
                          -> (
                  match
                              Protocol.lookup_protocol_marker scope env
                                protocol_name method_name
                  with
                  | None ->
                      Error.error
                                  ("protocol " ^ protocol_name
                                 ^ " does not define method " ^ method_name)
                  | Some marker -> (
                      match
                                  ( Protocol.method_position env marker
                                      method_name,
                          compile_expr scope env
                            (FList
                               (FSymbol "fn"
                               :: (match params with
                                  | FVector (FSymbol "_" :: remaining) ->
                                      FVector remaining
                                  | _ -> params)
                               :: body_forms)) )
                      with
                                | None, _ ->
                                    Error.error
                                      ("unknown protocol method " ^ method_name)
                      | _, (Error _ as err) -> err
                      | Some position, Ok implementation ->
                                    Ok
                                      ( position,
                                        marker,
                                        method_name,
                                        implementation )))
                        | _ ->
                            Error.error
                              "reify methods must be (method-name [params] \
                               body...)"
            in
            let rec compile_methods acc = function
                        | [] ->
                            Ok
                              (List.sort
                                 (fun (left, _, _, _) (right, _, _, _) ->
                                   compare left right)
                                 acc)
              | method_form :: rest -> (
                  match compile_method method_form with
                  | Error _ as err -> err
                            | Ok method_impl ->
                                compile_methods (method_impl :: acc) rest)
            in
                      match compile_methods [] method_forms with
            | Error _ as err -> err
            | Ok [] -> Error.error "reify expects at least one method"
                      | Ok ((_, marker, _, _) :: _ as implementations) ->
                          if
                            List.length implementations
                            <> Protocol.method_count env marker
                          then
                  Error.error
                              ("reify must implement every method of protocol "
                             ^ protocol_name)
                else
                  let methods =
                              List.map
                                (fun (_, _, _, implementation) ->
                                  implementation)
                                implementations
                  in
                  let payload_ty, payload_expr =
                    match methods with
                              | [ method_impl ] ->
                                  (method_impl.ty, method_impl.semantic_expr)
                              | _ ->
                                  ( TTuple
                                      (List.map
                                         (fun method_impl -> method_impl.ty)
                                         methods),
                                    Semantic_ir.Tuple
                                      (List.map
                                         (fun method_impl ->
                                           method_impl.semantic_expr)
                                         methods) )
                            in
                            Ok
                              (typed_ir
                                 (TOcaml_app
                                    ( "Lg_runtime.Runtime_reify.t",
                                      [ payload_ty ] ))
                                 (Semantic_ir.Apply
                                    ( Semantic_ir.Ident
                                        "Lg_runtime.Runtime_reify.make",
                                      [ payload_expr ] )))
                      )
                  | _ ->
                      Error.error
                        "reify expects a protocol and method implementations")
    | "assert" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ condition ] when Types.equal condition.ty TBool ->
            Ok
              (typed_ir TUnit
                 (Semantic_ir.If
                    ( condition.semantic_expr,
                      Semantic_ir.Unit,
                      Semantic_ir.Apply
                        ( Semantic_ir.Ident "invalid_arg",
                          [ Semantic_ir.String "Assert failed" ] ) )))
        | Ok [ condition; message ]
          when Types.equal condition.ty TBool
               && Types.equal message.ty TString ->
            Ok
              (typed_ir TUnit
                 (Semantic_ir.If
                    ( condition.semantic_expr,
                      Semantic_ir.Unit,
                      Semantic_ir.Apply
                        ( Semantic_ir.Ident "invalid_arg",
                          [ message.semantic_expr ] ) )))
                  | Ok [ _; message ] when not (Types.equal message.ty TString)
                    ->
            Error.error "assert message must be a string"
        | Ok [ _ ] | Ok [ _; _ ] ->
            Error.error "assert condition must be bool"
                  | Ok _ ->
                      Error.error
                        "assert expects condition and optional message")
    | "__lg_fnil" -> compile_static_fnil scope env arg_forms
    | "delay" -> (
        match arg_forms with
        | [] -> Error.error "delay expects at least one body form"
        | body_forms ->
            Result.map
              (fun body ->
                typed_ir (TOcaml_app ("Lazy.t", [ body.ty ]))
                  (Semantic_ir.Apply
                     ( Semantic_ir.Ident "Lazy.from_fun",
                       [ Semantic_ir.Fun ([], body.semantic_expr) ] )))
              (compile_body scope env
                 "delay expects at least one body form" body_forms))
    | "__lg_volatile!" -> (
        match arg_forms with
        | [ FSymbol "nil" ] -> (
            match Env.expected_type env with
            | Some
                (TRef
                  ((TNullable _ | TOcaml_app ("option", [ _ ])) as value_ty)) ->
                Ok
                  (typed_ir (TRef value_ty)
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "ref",
                          [ Semantic_ir.Constructor ("None", None) ] )))
            | _ ->
                Error.error
                  "volatile! nil requires an explicit option element type, \
                   for example ^:ref<option<int>>")
        | _ -> (
            match compile_args () with
            | Error _ as err -> err
            | Ok [ initial ] ->
                let initial =
                  match
                    ( Env.expected_type env,
                      initial.ty,
                      Semantic_ir.unlocated initial.semantic_expr )
                  with
                  | ( Some
                        (TRef
                          (TOcaml_app (expected_name, expected_args) as expected)),
                      TOcaml_app (actual_name, actual_args),
                      Semantic_ir.Apply (Semantic_ir.Ident constructor, []) )
                    when expected_name = actual_name
                         && List.length expected_args = List.length actual_args
                         && List.mem constructor
                              [
                                "Lg_runtime.Runtime_transient.map_empty";
                                "Lg_runtime.Runtime_transient.vector_empty";
                                "Lg_runtime.Runtime_transient.set_empty";
                              ] ->
                      typed_ir expected initial.semantic_expr
                  | _ -> initial
                in
                Ok
                  (typed_ir (TRef initial.ty)
                     (Semantic_ir.Apply
                        (Semantic_ir.Ident "ref", [ initial.semantic_expr ])))
            | Ok _ -> Error.error "volatile! expects 1 argument"))
    | name when is_var_quote_marker name -> (
        match arg_forms with
        | [ symbol ] -> compile_expr scope env symbol
        | _ -> Error.error "var expects one symbol")
    | "set!" -> (
        match arg_forms with
        | [ FList [ FSymbol field_access; target_form ]; value_form ]
          when String.starts_with ~prefix:".-" field_access ->
            let field_name =
              String.sub field_access 2 (String.length field_access - 2)
            in
            compile_mutable_field_assignment scope env (":" ^ field_name)
              target_form value_form
        | [ FSymbol name; value_form ] -> (
            match Env.find_opt (Names.scoped_key scope name) env with
            | Some
                {
                  ty = TRef referenced_ty;
                  ocaml_name;
                  dynamically_bindable = true;
                  _;
                } -> (
                match compile_expr scope env value_form with
                | Error _ as error -> error
                | Ok value ->
                    let value_name = "__lg_set_value" in
                    let stored_value =
                      typed_ir value.ty (Semantic_ir.Ident value_name)
                    in
                    Result.map
                      (fun stored ->
                        typed_ir value.ty
                          (Semantic_ir.Let
                             ( [
                                 ( Semantic_ir.PVar value_name,
                                   value.semantic_expr );
                               ],
                               Semantic_ir.Sequence
                                 [
                                   Semantic_ir.Infix
                                     ( ":=",
                                       Semantic_ir.Ident ocaml_name,
                                       stored );
                                   Semantic_ir.Ident value_name;
                                 ] )))
                      (adapt_value_to_type env referenced_ty stored_value))
            | Some _ -> Error.error ("set! expects a mutable target, got " ^ name)
            | None -> Error.error ("unknown set! target " ^ name))
        | _ -> Error.error "set! expects a target and value")
    | "__lg_swap!" as swap_name -> (
        match arg_forms with
        | reference_form :: function_form :: extra_forms -> (
            match compile_expr scope env reference_form with
            | Error _ as err -> err
            | Ok reference -> (
            match reference.ty with
                | TOcaml_app ("Lg_runtime.Runtime_slot.t", [ value_ty ]) -> (
                    let value_name = "__lg_swap_value" in
                    let updater_env =
                      Env.add
                        (Names.scoped_key scope value_name)
                        (Types.binding value_name value_ty)
                        env
                    in
                    let updater_body =
                      FList
                        (function_form :: FSymbol value_name :: extra_forms)
                    in
                    match compile_expr scope updater_env updater_body with
                    | Error _ as err -> err
                    | Ok updater_body ->
                        let reference_name = "__lg_swap_reference" in
                        let updater =
                          typed_ir (TFn ([ value_ty ], updater_body.ty))
                            (Semantic_ir.Fun
                               ([ Semantic_ir.PVar value_name ],
                                updater_body.semantic_expr))
                        in
                        Ok
                          (typed_ir updater_body.ty
                             (Semantic_ir.Let
                                ( [
                                    ( Semantic_ir.PVar reference_name,
                                      reference.semantic_expr );
                                  ],
                                  Semantic_ir.Apply
                                    ( Semantic_ir.Ident
                                        "Lg_runtime.Runtime_slot.set",
                                      [
                                        Semantic_ir.Ident reference_name;
                                        Semantic_ir.Apply
                                          ( updater.semantic_expr,
                                            [
                                              Semantic_ir.Apply
                                                ( Semantic_ir.Ident
                                                    "Lg_runtime.Runtime_slot.get",
                                                  [
                                                    Semantic_ir.Ident
                                                      reference_name;
                                                  ] );
                                            ] );
                                      ] ) ))))
                | TRef value_ty ->
                    let compile_generic () =
                      let value_name = "__lg_swap_value" in
                      let updater_env =
                        Env.add (Names.scoped_key scope value_name)
                          (Types.binding value_name value_ty)
                          env
                      in
                      let updater_body =
                        FList
                          (function_form :: FSymbol value_name :: extra_forms)
                      in
                      match compile_expr scope updater_env updater_body with
                      | Error _ as err -> err
                      | Ok updater_body ->
                          let reference_name = "__lg_swap_reference" in
                          let updater =
                            typed_ir (TFn ([ value_ty ], updater_body.ty))
                              (Semantic_ir.Fun
                                 ( [ Semantic_ir.PVar value_name ],
                                   updater_body.semantic_expr ))
                          in
                          let updated_name = "__lg_swap_updated" in
                          let updated_expr =
                            Semantic_ir.Apply
                              ( updater.semantic_expr,
                                [
                                  Semantic_ir.Prefix
                                    ( "!",
                                      Semantic_ir.Ident reference_name );
                                ] )
                          in
                          Ok
                            (typed_ir value_ty
                               (Semantic_ir.Let
                                 ( [
                                      ( Semantic_ir.PVar reference_name,
                                        reference.semantic_expr );
                                      ( Semantic_ir.PVar updated_name,
                                        updated_expr );
                                    ],
                                    Semantic_ir.Sequence
                                      [
                                        Semantic_ir.Infix
                                          ( ":=",
                                            Semantic_ir.Ident reference_name,
                                            Semantic_ir.Ident updated_name );
                                        Semantic_ir.Ident updated_name;
                                      ] )))
                    in
                    compile_generic ()
                          | reference_ty when Types.is_dynamic reference_ty ->
                              Error.error
                                (swap_name
                               ^ " requires a statically typed reference and \
                                  updater")
                | _ ->
                    compile_protocol_swap ~compile_expr scope env
                      swap_name reference function_form extra_forms))
        | _ ->
            Error.error
                        (swap_name
                       ^ " expects a reference, function, and optional \
                          arguments"))
    | "__lg_equal" | "__lg_numeric-equal" | "__lg_less" | "__lg_less-equal"
    | "__lg_greater" | "__lg_greater-equal" -> (
        let operator =
          match name with
          | "__lg_equal" -> "="
          | "__lg_numeric-equal" -> "=="
          | "__lg_less" -> "<"
          | "__lg_less-equal" -> "<="
          | "__lg_greater" -> ">"
          | "__lg_greater-equal" -> ">="
          | _ -> assert false
        in
        if arg_forms = [] then
          Error.error (operator ^ " expects at least 1 arguments")
        else
          match
            compile_args_for scope (Env.with_expected_type None env) arg_forms
          with
          | Error _ as err -> err
          | Ok args
            when operator <> "="
                 && List.exists (fun arg -> Types.is_dynamic arg.ty) args ->
              Error.error
                (operator ^ " expects statically typed numeric arguments")
          | Ok args ->
            let symbol_equality =
              operator = "="
              && List.exists
                   (fun arg ->
                     Option.is_some
                       (Types.symbol_predicate_constraint_info arg.ty))
                   args
              && List.for_all
                   (fun arg ->
                     Types.equal arg.ty TSymbol
                     || Option.is_some
                          (Types.symbol_predicate_constraint_info arg.ty))
                   args
            in
            if symbol_equality then
              let project arg =
                if Types.equal arg.ty TSymbol then
                  Semantic_ir.Constructor ("Some", Some arg.semantic_expr)
                else
                  match Semantic_ir.unlocated arg.semantic_expr with
                  | Semantic_ir.Ident value_name ->
                      Semantic_ir.Apply
                        ( Semantic_ir.Ident (value_name ^ "__symbol"),
                          [ arg.semantic_expr ] )
                  | _ ->
                      Semantic_ir.Apply
                        ( Semantic_ir.Apply
                            ( Semantic_ir.Ident "fst",
                              [ arg.semantic_expr ] ),
                          [
                            Semantic_ir.Apply
                              ( Semantic_ir.Ident "snd",
                                [ arg.semantic_expr ] );
                          ] )
              in
              let rec comparisons = function
                | left :: ((right :: _) as rest) ->
                    Semantic_ir.Infix ("=", project left, project right)
                    :: comparisons rest
                | [] | [ _ ] -> []
              in
              let equal =
                comparisons args
                |> function
                | [] -> Semantic_ir.Bool true
                | first :: rest ->
                    List.fold_left
                      (fun result comparison ->
                        Semantic_ir.Infix ("&&", result, comparison))
                      first rest
              in
              Ok
                (typed_ir TBool
                   equal)
            else
            let concrete_args =
              List.filter
                (fun arg ->
                  not
                    (Types.is_dynamic arg.ty || Types.equal arg.ty TUnknown
                    || match arg.ty with TMeta _ | TVar _ -> true | _ -> false))
                args
            in
            let numeric_ty =
              if
                concrete_args <> []
                && List.for_all (fun arg -> Types.is_numeric arg.ty) concrete_args
                &&
                (operator <> "="
                || List.length concrete_args = List.length args )
              then if
                operator = "=="
                && List.exists
                     (fun arg -> Types.equal arg.ty TFloat)
                     concrete_args
              then Some TFloat
              else
                List.find_map
                  (fun arg ->
                                if
                                  Types.equal arg.ty TInt
                                  || Types.equal arg.ty TFloat
                                then Some arg.ty
                    else None)
                  concrete_args
              else None
            in
            let dynamic_equality =
              operator = "="
              && List.exists
                   (fun arg ->
                     Types.is_dynamic arg.ty
                     ||
                     ( Option.is_none (Types.record_fields arg.ty)
                     && (contains_dynamic_type arg.ty
                        || uses_dynamic_value_storage arg.ty) ))
                   args
            in
            let rec adapt adapted = function
              | [] -> Ok (List.rev adapted)
              | arg :: rest -> (
                  if
                    dynamic_equality
                    && Option.is_none
                         (static_iequiv scope env arg.ty)
                  then
                    let dynamic = Types.dynamic_constraint TUnknown in
                    if Types.is_dynamic arg.ty then
                                adapt
                                  ({ arg with ty = dynamic } :: adapted)
                                  rest
                    else if uses_dynamic_value_storage arg.ty then
                      adapt
                        ({
                           arg with
                           ty = dynamic;
                           semantic_expr = constrained_argument_value arg;
                         }
                        :: adapted)
                        rest
                    else
                      Result.bind (pack_dynamic_value env dynamic arg)
                        (fun semantic_expr ->
                          adapt
                                      ({ arg with ty = dynamic; semantic_expr }
                                      :: adapted)
                            rest)
                  else
                  match numeric_ty with
                              | Some TFloat
                                when operator = "==" && Types.equal arg.ty TInt ->
                                  adapt
                                    ({
                                       arg with
                           ty = TFloat;
                           semantic_expr =
                             Semantic_ir.Apply
                               ( Semantic_ir.Ident "float_of_int",
                                 [ arg.semantic_expr ] );
                         }
                        :: adapted)
                        rest
                  | _ -> adapt (arg :: adapted) rest)
            in
            Result.bind (adapt [] args)
              (fun args ->
                if operator = "=" then
                  compile_equality scope env args
                else
                  Core_compare.compile ~env
                    (if operator = "==" then "=" else operator)
                    args))
              | "__lg_nil-predicate" | "__lg_true-predicate"
              | "__lg_false-predicate" | "__lg_int-predicate"
              | "__lg_number-predicate" | "__lg_string-predicate"
              | "__lg_keyword-predicate" | "__lg_list-predicate"
              | "__lg_seq-predicate"
              | "__lg_fn-predicate" | "__lg_uuid-predicate"
              | "__lg_delay-predicate" ->
                  compile_boolean_call scope env name arg_forms
              | "__lg_ifn-predicate" -> (
                  match compile_args_for scope env arg_forms with
                  | Error _ as error -> error
                  | Ok args ->
                      let callable_type ty =
                        let ty = Types.constraint_value_type ty in
                        match ty with
                        | TFn _ | TOverloaded_fn _ | TKeyword | TSymbol
                        | TVector _ | TSet _ | TRecord _ | TMap_keys ->
                            true
                        | ty when Option.is_some (Types.dynamic_map_types ty) ->
                            true
                        | ty ->
                            List.init 22 (fun index -> index + 1)
                            |> List.exists (fun arity ->
                                   Option.is_some
                                     (static_deftype_callable env ty arity))
                      in
                      Core_boolean.compile_type_predicate name callable_type
                        args)
    | "instance?" -> (
        match arg_forms with
        | [ FSymbol type_name; _ ] when is_java_type_name type_name ->
            java_interop_error type_name
        | [ FSymbol type_name; value_form ] -> (
                      let protocol_name =
                        if String.contains type_name '/' then type_name
                        else
                          match String.rindex_opt type_name '.' with
                          | None -> type_name
                          | Some separator ->
                              String.sub type_name 0 separator
                              ^ "/"
                              ^ String.sub type_name (separator + 1)
                                  (String.length type_name - separator - 1)
                      in
                      match
                        Protocol.find_protocol_id scope env protocol_name
                      with
                      | Some _ ->
                          compile_expr scope env
                            (FList
                               [
                                 FSymbol "satisfies?";
                                 FSymbol protocol_name;
                                 value_form;
                               ])
                      | None -> (
                match
                  ( Resolver.lookup_record_type scope env type_name,
                    compile_expr scope env value_form )
                with
                | (Error _ as error), _ -> error
                | _, (Error _ as error) -> error
                | Ok record, Ok value -> (
                    match Types.constraint_value_type value.ty with
                    | TNamed_record actual ->
                        Ok
                          (typed_ir TBool
                             (Semantic_ir.Sequence
                                [
                                  value.semantic_expr;
                                  Semantic_ir.Bool
                                    (Type_id.equal record.type_id actual.type_id);
                                ]))
                    | _ ->
                        Error.error
                          "instance? requires a statically known record type; match a closed sum type")
                  ))
        | _ -> Error.error "instance? expects a record type and value")
    | "__lg_builtin-name" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok args -> Core_scalar.compile "name" args)
    | ("__lg_builtin-keyword" | "__lg_builtin-symbol") as name -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok args -> Core_scalar.compile name args)
    | "__lg_builtin-namespace" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok args -> Core_scalar.compile "namespace" args)
    | "__lg_namespace" -> (
        match arg_forms with
        | [ _ ] ->
            compile_protocol_call scope env
              "clojure.core/INamed/-namespace" arg_forms
        | _ -> Error.error "namespace expects 1 arguments")
    | ("resolve" | "requiring-resolve") as resolve_name ->
        Error.error
          (resolve_name
          ^ " cannot be used without a closed result type; define a closed sum \
             type containing the supported Vars")
    | "__lg_rational-predicate" | "__lg_float-predicate"
    | "__lg_double-predicate" | "__lg_symbol-predicate"
    | "__lg_char-predicate" | "__lg_regex-predicate" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok args -> Core_predicate.compile name args)
    | ("__lg_zero-predicate" | "__lg_pos-predicate" | "__lg_neg-predicate")
      as predicate -> (
        let source_predicate =
          match predicate with
          | "__lg_zero-predicate" -> "zero?"
          | "__lg_pos-predicate" -> "pos?"
          | "__lg_neg-predicate" -> "neg?"
          | _ -> assert false
        in
        match compile_args () with
        | Error _ as err -> err
        | Ok [ arg ] when Types.is_dynamic arg.ty ->
            let function_name =
              match predicate with
              | "__lg_zero-predicate" -> "is_zero"
              | "__lg_pos-predicate" -> "is_positive"
              | _ -> "is_negative"
            in
            Ok
              (typed_ir TBool
                 (apply
                    ("Lg_runtime.Runtime_dynamic." ^ function_name)
                    [ arg.semantic_expr ]))
        | Ok [ arg ] ->
            let operator =
              match predicate with
              | "__lg_zero-predicate" -> "="
              | "__lg_pos-predicate" -> ">"
              | "__lg_neg-predicate" -> "<"
              | _ -> assert false
            in
            let zero =
              match arg.ty with
              | TInt | TUnknown | TOcaml "int" ->
                  Ok (Semantic_ir.Int 0)
              | TFloat -> Ok (Semantic_ir.Float "0.0")
              | _ ->
                  Error.error
                    ("expected int arguments for " ^ source_predicate)
            in
            Result.map
              (fun zero ->
                typed_ir TBool
                  (Semantic_ir.Infix (operator, arg.semantic_expr, zero)))
              zero
        | Ok _ -> Error.error (source_predicate ^ " expects 1 arguments"))
    | "__lg_abs" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ arg ] -> (
            match arg.ty with
            | TInt | TOcaml "int" ->
                Ok
                  (typed_ir arg.ty
                     (apply "Stdlib.abs" [ arg.semantic_expr ]))
            | TFloat ->
                Ok
                  (typed_ir TFloat
                     (apply "Float.abs" [ arg.semantic_expr ]))
            | _ -> Error.error "abs expects a numeric argument")
        | Ok _ -> Error.error "abs expects 1 argument")
    | ("__lg_str" | "__lg_print_str" | "__lg_pr_str") as render_name -> (
        let readable = render_name = "__lg_pr_str" in
        let separator = if render_name = "__lg_str" then "" else " " in
        let printable_env =
          Env.with_expected_type
            (Some (Types.printable_constraint (Type_solver.fresh ())))
            env
        in
        let rec compile_printable_args compiled = function
          | [] -> Ok (List.rev compiled)
          | form :: rest -> (
              match compile_expr scope printable_env form with
              | Error _ as error -> error
              | Ok argument ->
                  compile_printable_args (argument :: compiled) rest)
        in
        match compile_printable_args [] arg_forms with
        | Error _ as err -> err
        | Ok args ->
            let expr =
              match args with
              | [] -> Semantic_ir.String ""
              | _ ->
                  let bindings, values =
                    args
                    |> List.mapi (fun index argument ->
                           let name =
                             "__lg_render_argument_" ^ string_of_int index
                           in
                           ( ( Semantic_ir.PVar name,
                               stringify_value scope env ~pr:readable argument ),
                             Semantic_ir.Ident name ))
                    |> List.split
                  in
                  let rendered =
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "String.concat",
                        [ Semantic_ir.String separator; Semantic_ir.List values ] )
                  in
                  List.fold_right
                    (fun binding body -> Semantic_ir.Let ([ binding ], body))
                    bindings rendered
            in
            Ok (typed_ir TString expr))
    | ("__lg_render_display_values" | "__lg_render_readable_values") as
      render_name -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ separator; values ] when Types.equal separator.ty TString -> (
            match Collection_capability.to_seq_expr env values with
            | Ok (element_ty, sequence)
              when Option.is_some (Types.printable_constraint_info element_ty)
              ->
                let runtime_name =
                  if render_name = "__lg_render_display_values" then
                    "Lg_runtime.Runtime_print.render_display_values"
                  else "Lg_runtime.Runtime_print.render_readable_values"
                in
                Ok
                  (typed_ir TString
                     (apply runtime_name [ separator.semantic_expr; sequence ]))
            | Ok _ ->
                Error.error
                  (render_name ^ " expects printable variadic values")
            | Error _ ->
                Error.error (render_name ^ " expects a printable sequence"))
        | Ok [ _; _ ] -> Error.error (render_name ^ " expects a string separator")
        | Ok _ -> Error.error (render_name ^ " expects 2 arguments"))
    | "__lg_with-meta" ->
        compile_metadata_call scope env name arg_forms
    | "__lg_nullable-value" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ value ] -> (
            match value.ty with
            | TNullable inner | TOcaml_app ("option", [ inner ]) ->
                Ok
                  (typed_ir inner
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Option.get",
                          [ value.semantic_expr ] )))
            | ty -> (
                match Types.truthy_constraint_info ty with
                | Some (TNullable inner | TOcaml_app ("option", [ inner ])) ->
                    Ok
                      (typed_ir inner
                         (Semantic_ir.Apply
                            ( Semantic_ir.Ident "Option.get",
                              [ constrained_argument_value value ] )))
                | Some _ | None -> (
                    match Types.seqable_constraint_info ty with
                    | Some
                    ( ((`Optional | `Optional_sequential) as kind),
                      element_ty,
                      (TNullable inner | TOcaml_app ("option", [ inner ])) )
                  ->
                    let adapter, stored_value =
                      match Semantic_ir.unlocated value.semantic_expr with
                      | Semantic_ir.Ident name ->
                          ( Semantic_ir.Ident (name ^ "__seq_optional"),
                            value.semantic_expr )
                      | _ ->
                          ( Semantic_ir.Apply
                              ( Semantic_ir.Ident "fst",
                                [ value.semantic_expr ] ),
                            Semantic_ir.Apply
                              ( Semantic_ir.Ident "snd",
                                [ value.semantic_expr ] ) )
                    in
                    let narrowed_ty =
                      match kind with
                      | `Optional ->
                          Types.optional_seqable_constraint element_ty inner
                      | `Optional_sequential ->
                          Types.optional_sequential_constraint element_ty inner
                      | `Required -> assert false
                    in
                    Ok
                      (typed_ir narrowed_ty
                         (Semantic_ir.Tuple
                            [
                              adapter;
                              Semantic_ir.Apply
                                ( Semantic_ir.Ident "Option.get",
                                  [ stored_value ] );
                            ]))
                    | _ ->
                        Error.error
                          "internal nullable narrowing expects an optional \
                           value")))
        | Ok _ ->
            Error.error "internal nullable narrowing expects 1 argument")
    | "__lg_symbol-value" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ value ]
          when Option.is_some
                 (Types.symbol_predicate_constraint_info value.ty) ->
            let projected =
              match Semantic_ir.unlocated value.semantic_expr with
              | Semantic_ir.Ident name ->
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident (name ^ "__symbol"),
                      [ value.semantic_expr ] )
              | _ ->
                  Semantic_ir.Apply
                    ( Semantic_ir.Apply
                        (Semantic_ir.Ident "fst", [ value.semantic_expr ]),
                      [
                        Semantic_ir.Apply
                          (Semantic_ir.Ident "snd", [ value.semantic_expr ]);
                      ] )
            in
            Ok
              (typed_ir TSymbol
                 (Semantic_ir.Apply
                    (Semantic_ir.Ident "Option.get", [ projected ])))
        | Ok [ value ] when Types.equal value.ty TSymbol -> Ok value
        | Ok [ value ] when Types.is_dynamic value.ty ->
            Ok
              (typed_ir TSymbol
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.as_symbol",
                      [ value.semantic_expr ] )))
        | Ok [ _ ] -> Ok (unreachable_narrowed_value TSymbol "symbol")
        | Ok _ ->
            Error.error "internal symbol narrowing expects 1 argument")
    | "__lg_keyword-value" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ value ] when Types.equal value.ty TKeyword -> Ok value
        | Ok [ value ] when Types.equal value.ty TUnknown ->
            Ok (typed_ir TKeyword value.semantic_expr)
        | Ok [ value ] when Types.is_dynamic value.ty ->
            Error.error
              "keyword? guard narrowing requires a statically typed value; \
               define a closed sum type for alternative value types"
        | Ok [ _ ] -> Ok (unreachable_narrowed_value TKeyword "keyword")
        | Ok _ ->
            Error.error "internal keyword narrowing expects 1 argument")
    | "__lg_int-value" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ value ] when Types.equal value.ty TInt -> Ok value
        | Ok [ value ] when Types.is_dynamic value.ty ->
            Error.error
              "int? guard narrowing requires a statically typed value; define \
               a closed sum type for alternative value types"
        | Ok [ _ ] -> Ok (unreachable_narrowed_value TInt "int")
        | Ok _ -> Error.error "internal int narrowing expects 1 argument")
    | "__lg_max" | "__lg_min" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok args
          when List.exists (fun arg -> Types.equal arg.ty TFloat) args
               && List.for_all
                    (fun arg -> Types.is_numeric arg.ty)
                    args ->
            Core_float.compile_min_max name
              (List.map Core_float.widen_to_float args)
        | Ok args -> Core_int.compile_min_max name args)
    | "__lg_hash" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ value ]
          when Option.is_some (Types.hashable_constraint_info value.ty) ->
            let expression =
              match Semantic_ir.unlocated value.semantic_expr with
              | Semantic_ir.Ident name ->
                  Semantic_ir.Apply
                    (Semantic_ir.Ident (name ^ "__hash"), [ value.semantic_expr ])
              | _ ->
                  Semantic_ir.Apply
                    ( Semantic_ir.Apply
                        (Semantic_ir.Ident "fst", [ value.semantic_expr ]),
                      [
                        Semantic_ir.Apply
                          (Semantic_ir.Ident "snd", [ value.semantic_expr ]);
                      ] )
            in
            Ok (typed_ir TInt expression)
        | Ok [ value ] ->
            Result.map (fun expression -> typed_ir TInt expression)
              (compile_static_hash_capability env value)
        | Ok _ -> Error.error "hash expects 1 argument")
    | "class" | "type" ->
        Error.error
          "runtime class inspection is not supported; match a closed sum type"
    | "__lg_identical-predicate" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ left; right ]
          when Types.is_dynamic left.ty || Types.is_dynamic right.ty -> (
            let dynamic_ty = Types.dynamic_constraint TUnknown in
            match
              ( pack_dynamic_value env dynamic_ty left,
                pack_dynamic_value env dynamic_ty right )
            with
            | (Error _ as error), _ | _, (Error _ as error) -> error
            | Ok left, Ok right ->
                Ok
                  (typed_ir TBool
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_dynamic.identical",
                          [ left; right ] ))))
        | Ok [ left; right ]
          when Types.equal
                 (Types.constraint_value_type left.ty)
                 (Types.constraint_value_type right.ty) ->
            Ok
              (typed_ir TBool
                 (Semantic_ir.Infix
                    ( "==",
                      constrained_argument_value left,
                      constrained_argument_value right )))
        | Ok [ left; right ] ->
            Error.error
              ("identical? arguments must have the same type, got "
             ^ Types.source_name left.ty ^ " and " ^ Types.source_name right.ty)
        | Ok _ -> Error.error "identical? expects 2 arguments")
    | ("re-matches" | "re-find") as regex_operation -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ expression; source ] when Types.equal expression.ty TRegex ->
            let source =
              if Types.equal source.ty TString then Ok source.semantic_expr
              else if Types.is_dynamic source.ty then
                dynamic_unpack env TString source.semantic_expr
              else Error.error (regex_operation ^ " expects a regex and string")
            in
            Result.map
              (fun source ->
                let matcher =
                  if regex_operation = "re-matches" then
                    "Lg_runtime.Runtime_string.regex_matches_groups"
                  else "Lg_runtime.Runtime_string.regex_find_groups"
                in
                let groups =
                  Semantic_ir.Apply
                    (Semantic_ir.Ident matcher, [ expression.semantic_expr; source ])
                in
                typed_ir (Types.dynamic_constraint TUnknown)
                  (Semantic_ir.Apply
                     ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.regex_match",
                       [ groups ] )))
              source
        | Ok _ ->
            Error.error (regex_operation ^ " expects a regex and string"))
    | "__lg_pprint" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ arg ] ->
            Ok
              (typed_ir TUnit
                 (apply "print_endline"
                    [ stringify_value scope env ~pr:true arg ]))
        | Ok [ arg; writer ]
          when Types.equal writer.ty (TOcaml "Buffer.t")
               || (match writer.ty with
                  | TUnknown | TMeta _ | TVar _ -> true
                  | _ -> false) ->
            let writer_name = "__lg_pprint_writer" in
            Ok
              (typed_ir TUnit
                 (Semantic_ir.Let
                    ( [ (Semantic_ir.PVar writer_name, writer.semantic_expr) ],
                      Semantic_ir.Sequence
                        [ apply "Lg_runtime.Runtime_print.write"
                            [ Semantic_ir.Ident writer_name;
                              stringify_value scope env ~pr:true arg;
                            ];
                          apply "Lg_runtime.Runtime_print.write"
                            [ Semantic_ir.Ident writer_name;
                              Semantic_ir.String "\n";
                            ];
                        ] )))
        | Ok [ _; _ ] -> Error.error "pprint writer must be Buffer.t"
        | Ok _ -> Error.error "pprint expects 1 or 2 arguments")
    | "__lg_pr" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok args ->
            let text =
              apply "String.concat"
                [ Semantic_ir.String " ";
                  Semantic_ir.List
                    (List.map (stringify_value scope env ~pr:true) args);
                ]
            in
            let output =
              match lookup_binding scope env "*out*" with
              | Ok writer ->
                  apply "Lg_runtime.Runtime_print.write"
                    [ Semantic_ir.Ident writer.ocaml_name; text ]
              | Error _ -> apply "print_string" [ text ]
            in
            Ok (typed_ir TUnit output))
    | "__lg_print_output" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ arg ] when Types.equal arg.ty TString ->
            Ok
              (typed_ir TUnit
                 (Semantic_ir.Apply
                    (Semantic_ir.Ident "print_string", [ arg.semantic_expr ])))
        | Ok [ _ ] -> Error.error "print output expects a string"
        | Ok _ -> Error.error "print output expects 1 argument")
    | "__lg_list" -> compile_list scope env arg_forms
    | "__lg_list-star" -> compile_list_star scope env arg_forms
    | "list-of" -> compile_list_of arg_forms
    | "__lg_cons" -> compile_cons scope env arg_forms
    | "__lg_vector" -> compile_vector scope env arg_forms
    | "vector-of" -> compile_vector_of arg_forms
    | "__lg_count" -> compile_collection_call scope env name arg_forms
    | "__lg_conj!" -> compile_conj_bang scope env arg_forms
    | "__lg_assoc!" -> compile_assoc_bang scope env arg_forms
    | "__lg_dissoc!" -> compile_dissoc_bang scope env arg_forms
    | "__lg_transient" -> compile_transient scope env arg_forms
    | "__lg_persistent!" -> compile_persistent_bang scope env arg_forms
    | "__lg_conj" -> compile_conj scope env arg_forms
    | "__lg_first" -> compile_collection_call scope env name arg_forms
    | "__lg_subvec" -> compile_subvec scope env arg_forms
    | "__lg_nth" -> compile_nth scope env arg_forms
    | "__lg_get" -> compile_get scope env arg_forms
    | "__lg_find" -> compile_find scope env arg_forms
    | "__lg_get-in" -> compile_get_in scope env arg_forms
    | "__lg_assoc" -> compile_assoc scope env arg_forms
    | "__lg_assoc-in" -> compile_assoc_in scope env arg_forms
    | "__lg_dissoc" -> compile_dissoc scope env arg_forms
    | "__lg_merge" -> compile_merge scope env arg_forms
    | "__lg_update" -> compile_update scope env arg_forms
    | "__lg_update-in" -> compile_update_in scope env arg_forms
    | "__lg_select-keys" -> compile_select_keys scope env arg_forms
    | "__lg_contains" -> compile_contains scope env arg_forms
    | "__lg_keys" -> compile_keys scope env arg_forms
    | "__lg_vals" -> compile_vals scope env arg_forms
    | "__lg_hash-map" | "__lg_array-map" ->
        compile_hash_map scope env arg_forms
    | "__lg_rest" | "__lg_seq" ->
        compile_collection_call scope env name arg_forms
    | "__lg_into" -> (
        match arg_forms with
        | [ target_form; transducer_form; source_form ] ->
            compile_into scope env target_form
              (FList [ FSymbol "sequence"; transducer_form; source_form ])
        | [ target_form; source_form ] ->
            compile_into scope env target_form source_form
        | _ -> compile_sequence_transform_call scope env name arg_forms)
    | "__lg_next" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok args -> Core_sequence.compile env name args)
    | "__lg_some" -> compile_some scope env arg_forms
    | "__lg_sort" ->
        compile_sequence_transform_call scope env name arg_forms
    | "__lg_sort-by" -> compile_sort_by scope env arg_forms
    | "__lg_concat" -> compile_concat scope env arg_forms
    | "__lg_set" -> compile_set scope env arg_forms
    | "__lg_interleave" ->
        compile_sequence_transform_call scope env name arg_forms
    | "__lg_reductions" -> compile_reductions scope env arg_forms
    | "__lg_map" -> compile_map scope env arg_forms
    | "__lg_mapv" -> compile_mapv scope env arg_forms
    | "__lg_reduce-kv" -> compile_reduce_kv scope env arg_forms
    | "__lg_transformer_sequence" -> (
        match arg_forms with
        | [ xform_form; collection_form ] -> (
            match
              ( compile_function_arg scope env xform_form,
                compile_expr scope env collection_form )
            with
            | (Error _ as error), _ | _, (Error _ as error) -> error
            | Ok xform, Ok collection -> (
                match
                  ( xform.ty,
                    Collection_capability.to_seq_expr env collection )
                with
                | ( TFn
                      ( [ TOverloaded_fn downstream_arities ],
                        TOverloaded_fn transformed_arities ),
                    Ok (collection_element, sequence) ) -> (
                    match
                      ( select_overloaded_arity downstream_arities 2,
                        select_overloaded_arity transformed_arities 2 )
                    with
                    | ( Some (_, downstream),
                        Some (_, transformed) ) -> (
                        match
                          (downstream.fixed_params, transformed.fixed_params)
                        with
                        | [ _; output_ty ], [ _; input_ty ]
                          when argument_compatible input_ty
                                 collection_element ->
                            let input_name =
                              "__lg_transformer_sequence_input"
                            in
                            let input =
                              typed_ir collection_element
                                (Semantic_ir.Ident input_name)
                            in
                            Result.bind
                              (if Types.equal input_ty collection_element then
                                 Ok sequence
                               else
                                 Result.map
                                   (fun packed ->
                                     apply "Lg_runtime.Runtime_seq.map"
                                       [
                                         Semantic_ir.Fun
                                           ( [ Semantic_ir.PVar input_name ],
                                             packed );
                                         sequence;
                                       ])
                                   (pack_constrained_value env input_ty input))
                              (fun sequence ->
                                let transformed =
                                  apply
                                    (match Env.target env with
                                    | Target.Melange ->
                                        "Lg_runtime.Runtime_seq_melange.transformer_sequence"
                                    | Target.Native | Target.Js_of_ocaml ->
                                        "Lg_runtime.Runtime_seq.transformer_sequence")
                                    [ xform.semantic_expr; sequence ]
                                in
                                let output_value_ty =
                                  Types.constraint_value_type output_ty
                                in
                                if Types.equal output_value_ty output_ty then
                                  Ok (typed_ir (TSeq output_ty) transformed)
                                else
                                  let output_name =
                                    "__lg_constrained_argument_transformer_output"
                                  in
                                  let output =
                                    Collection_capability.constraint_value_expression
                                      output_ty (Semantic_ir.Ident output_name)
                                  in
                                  Ok
                                    (typed_ir (TSeq output_value_ty)
                                       (apply "Lg_runtime.Runtime_seq.map"
                                          [
                                            Semantic_ir.Fun
                                              ( [
                                                  Semantic_ir.PVar output_name;
                                                ],
                                                output );
                                            transformed;
                                          ])))
                        | _ ->
                            Error.error
                              "__lg_transformer_sequence expects binary reducing-function arities")
                    | _ ->
                        Error.error
                          "__lg_transformer_sequence expects binary reducing-function arities")
                | TFn _, Ok _ ->
                    Error.error
                      "__lg_transformer_sequence expects a transducer"
                | _, Error _ ->
                    Error.error
                      "__lg_transformer_sequence expects a seqable collection"
                | _ ->
                    Error.error
                      "__lg_transformer_sequence expects a transducer"))
        | _ -> Error.error "__lg_transformer_sequence expects 2 arguments")
    | "__lg_reduce_transformed" -> (
        match arg_forms with
        | [ _reducer_form; _initial_form; _collection_form ] ->
            compile_reduce scope env arg_forms
        | _ -> Error.error "__lg_reduce_transformed expects 3 arguments")
    | "__lg_complete_transformed" -> (
        match arg_forms with
        | [ transformed_form; result_form ] -> (
            match
              ( compile_function_arg scope env transformed_form,
                compile_expr scope env result_form )
            with
            | (Error _ as error), _ -> error
            | _, (Error _ as error) -> error
            | Ok transformed, Ok result -> (
                match transformed.ty with
                | TOverloaded_fn arities -> (
                    match select_overloaded_arity arities 1 with
                    | Some (arity_index, arity)
                      when Option.is_none arity.rest_param -> (
                        match arity.fixed_params with
                        | [ parameter_ty ] ->
                            Result.map
                              (fun argument ->
                                typed_ir arity.return_ty
                                  (Semantic_ir.Apply
                                     ( overloaded_projection
                                         transformed.semantic_expr arity_index,
                                       [ argument ] )))
                              (adapt_value_to_type env parameter_ty result)
                        | _ ->
                            Error.error
                              "__lg_complete_transformed expects a unary completion arity")
                    | _ ->
                        Error.error
                          "__lg_complete_transformed expects a unary completion arity")
                | _ ->
                    Error.error
                      "__lg_complete_transformed expects an overloaded reducing function"))
        | _ -> Error.error "__lg_complete_transformed expects 2 arguments")
    | "__lg_reduce" -> compile_reduce scope env arg_forms
    | "__lg_apply" -> (
        match arg_forms with
        | FSymbol "__lg_mapv" :: constructor_form :: fixed_and_rest
          when List.length fixed_and_rest >= 2 ->
            let reversed = List.rev fixed_and_rest in
            let rest_form = List.hd reversed in
            let fixed_forms = List.rev (List.tl reversed) in
            compile_apply_zip_vectors scope env constructor_form fixed_forms
              rest_form
        | _ -> compile_apply scope env arg_forms)
    | "__lg_comp" -> compile_static_comp scope env arg_forms
    | "__lg_partial" -> compile_static_partial scope env arg_forms
    | "__lg_juxt" -> compile_static_juxt scope env arg_forms
    | "__lg_compare" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ left; right ]
          when Option.is_some (Types.comparable_constraint_info left.ty) ->
            let left_value = constrained_argument_value left in
            let right_value = constrained_argument_value right in
            let witness =
              match Semantic_ir.unlocated left.semantic_expr with
              | Semantic_ir.Ident name ->
                  Semantic_ir.Ident (name ^ "__compare")
              | _ ->
                  Semantic_ir.Apply
                    (Semantic_ir.Ident "fst", [ left.semantic_expr ])
            in
            Ok
              (typed_ir TInt
                 (Semantic_ir.Apply (witness, [ left_value; right_value ])))
        | Ok [ left; right ] ->
            Result.map (fun expression -> typed_ir TInt expression)
              (compile_static_compare_capability env left right)
        | Ok _ -> Error.error "compare expects 2 arguments")
    | "ordering-compare" -> (
        match compile_compare scope env arg_forms with
        | Error _ as error -> error
        | Ok compared ->
            Ok (typed_ir (TOcaml "int") compared.semantic_expr))
              | "__lg_hash-set" -> compile_hash_set scope env arg_forms
    | "set-of" -> compile_set_of env arg_forms
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
                  | _ -> (
            let constructor_name =
              resolve_ocaml_constructor_target scope env name
            in
                      match
                        Ocaml_signature.constructor_signature constructor_name
                      with
            | Error _ as err -> err
            | Ok signature ->
                constructor ~constructor_name
                  (fun _ -> signature.result_type)
                  (List.length signature.payload_types)))
              | _ -> compile_named_function_call scope env name arg_forms)
  and compile_inferred_ocaml_call scope env function_name value_forms =
    match compile_ocaml_arguments scope env value_forms with
    | Error _ as err -> err
    | Ok arguments -> (
        let function_name = resolve_ocaml_call_target scope env function_name in
        let function_name, arguments =
          match (Env.target env, function_name, arguments) with
          | Target.Melange, "Array.map", [ fn; array ] ->
              ("Lg_runtime.Runtime_array_melange.map", [ array; fn ])
          | _ -> (function_name, arguments)
        in
        match Ocaml_signature.value_signature function_name with
        | Error _ as err -> err
        | Ok signature -> (
            let arguments =
              match (arguments, signature.parameters) with
              | ( [],
                    [
                      {
                        Ocaml_signature.label = Ocaml_signature.Positional;
                        ty = TUnit;
                      };
                  ] ) ->
                  [ (None, typed_ir TUnit Semantic_ir.Unit) ]
              | _ -> arguments
            in
            let expected_argument_types () =
              let named_labels = List.filter_map fst arguments in
              let remaining =
                List.filter
                  (fun (parameter : Ocaml_signature.parameter) ->
                    match parameter.label with
                    | Ocaml_signature.Labelled label
                    | Ocaml_signature.Optional label ->
                        not (List.mem label named_labels)
                    | Ocaml_signature.Positional -> true)
                  signature.parameters
              in
              let find_named label =
                signature.parameters
                |> List.find_opt
                     (fun (parameter : Ocaml_signature.parameter) ->
                       match parameter.label with
                       | Ocaml_signature.Labelled name
                       | Ocaml_signature.Optional name ->
                           String.equal name label
                       | Ocaml_signature.Positional -> false)
                |> Option.map
                     (fun (parameter : Ocaml_signature.parameter) ->
                       match (parameter.label, optional_payload parameter.ty) with
                       | Ocaml_signature.Optional _, Some ty -> ty
                       | _ -> parameter.ty)
              in
              let rec consume_positional prefix = function
                | [] -> None
                | { Ocaml_signature.label = Ocaml_signature.Optional _; _ }
                  :: parameters ->
                    consume_positional prefix parameters
                | ({ label = Ocaml_signature.Labelled _; _ } as parameter)
                  :: parameters ->
                    consume_positional (parameter :: prefix) parameters
                | { label = Ocaml_signature.Positional; ty } :: parameters ->
                    Some (ty, List.rev_append prefix parameters)
              in
              let rec collect collected remaining = function
                | [] -> Some (List.rev collected)
                | (Some label, _) :: rest -> (
                    match find_named label with
                    | Some ty -> collect (ty :: collected) remaining rest
                    | None -> None)
                | (None, _) :: rest -> (
                    match consume_positional [] remaining with
                    | Some (ty, remaining) ->
                        collect (ty :: collected) remaining rest
                    | None -> None)
              in
              collect [] remaining arguments
            in
            let adapt_arguments expected_types =
              let rec adapt adapted expected_types arguments =
                match (expected_types, arguments) with
                | [], [] -> Ok (List.rev adapted)
                | expected :: expected_rest, (label, argument) :: rest -> (
                    match optional_payload argument.ty with
                    | Some payload_ty
                      when argument_compatible expected payload_ty ->
                        let expected =
                          Types.instantiate_type ~templates:[ expected ]
                            ~actuals:[ payload_ty ] expected
                        in
                        Result.bind
                          (adapt_value_to_type env expected argument)
                          (fun expression ->
                            adapt
                              ((label, typed_ir expected expression) :: adapted)
                              expected_rest rest)
                    | Some _ | None ->
                        if Types.equal expected argument.ty then
                          adapt ((label, argument) :: adapted) expected_rest rest
                        else if argument_compatible expected argument.ty then
                          Result.bind
                            (adapt_value_to_type env expected argument)
                            (fun expression ->
                              adapt
                                ((label, typed_ir expected expression) :: adapted)
                                expected_rest rest)
                        else
                          Error.error
                            ?location:
                              (semantic_expression_location
                                 argument.semantic_expr)
                            ("OCaml argument type mismatch: expected "
                           ^ Types.source_name expected
                           ^ ", got " ^ Types.source_name argument.ty))
                | _ -> Error.error "internal OCaml argument mismatch"
              in
              adapt [] expected_types arguments
            in
            let argument_types =
              List.map (fun (label, argument) -> (label, argument.ty)) arguments
            in
            match
              Ocaml_signature.result_after_application signature argument_types
            with
            | Error _ as err -> err
            | Ok _ -> (
                match expected_argument_types () with
                | None -> Error.error "invalid OCaml argument application"
                | Some expected_types -> (
                    match adapt_arguments expected_types with
                    | Error _ as err -> err
                    | Ok arguments ->
                        Result.map
                          (fun return_ty ->
                            let expression = ocaml_apply function_name arguments in
                            match return_ty with
                            | TOcaml "int" -> typed_ir TInt expression
                            | _ -> typed_ir return_ty expression)
                          (Ocaml_signature.result_after_application signature
                             argument_types)))))
  and compile_boolean_call scope env name arg_forms =
    match compile_args_for scope env arg_forms with
    | Error _ as err -> err
    | Ok args -> Core_boolean.compile name args
  and compile_collection_call scope env name arg_forms =
    let argument_env =
      match (name, Env.expected_type env) with
      | "__lg_first", Some expected ->
          let element_ty =
            match expected with
            | TNullable inner | TOcaml_app ("option", [ inner ]) -> inner
            | ty -> ty
          in
          Env.with_expected_type (Some (TSeq element_ty)) env
      | _ -> env
    in
    match compile_args_for scope argument_env arg_forms with
    | Error _ as err -> err
    | Ok args ->
        let args =
          List.map
            (fun argument ->
              {
                argument with
                ty =
                  Function_elaborator.infer_named_record scope env argument.ty;
              })
            args
        in
        Core_collection.compile env name args
  and compile_concat scope env arg_forms =
    match compile_args_for scope env arg_forms with
    | Error _ as error -> error
    | Ok [] ->
        let element_ty =
          match Env.expected_type env with
          | Some (TSeq element_ty) -> element_ty
          | Some _ | None -> Type_solver.fresh ()
        in
        Ok (typed_ir (TSeq element_ty) (Semantic_ir.Ident "Seq.empty"))
    | Ok collections ->
        if List.exists (fun collection -> Types.is_dynamic collection.ty) collections
        then
          Error.error
            "concat requires statically typed collections; define a sum type \
             for heterogeneous elements"
        else (
        let rec collect sequences = function
          | [] -> Ok (List.rev sequences)
          | collection :: rest -> (
              match Collection_capability.to_seq_expr env collection with
              | Error _ ->
                  Error.error
                    ("concat expects collections, got "
                    ^ Types.source_name collection.ty)
              | Ok (inner, sequence) ->
                  let inner =
                    Function_elaborator.infer_named_record scope env inner
                  in
                  collect ((inner, sequence) :: sequences) rest)
        in
        match collect [] collections with
        | Error _ as error -> error
        | Ok [] -> assert false
        | Ok ((first_ty, _) :: _ as sequences) ->
            let rec merge_element_types left right =
              match (left, right) with
              | TNullable left, TNullable right ->
                  Option.map
                    (fun inner -> TNullable inner)
                    (merge_element_types left right)
              | TNullable inner, other | other, TNullable inner ->
                  Option.map
                    (fun inner -> TNullable inner)
                    (merge_element_types inner other)
              | _ -> (
                  match merge_branch_types left right with
                  | Some _ as merged -> merged
                  | None -> (
                      match Type_solver.unify Type_solver.empty left right with
                      | Error _ -> None
                      | Ok substitutions ->
                          Some
                            (Type_inference_core.refine_type
                               (Type_solver.apply substitutions left)
                               (Type_solver.apply substitutions right))))
            in
            let common_type =
              sequences
              |> List.tl
              |> List.fold_left
                   (fun common (ty, _) ->
                     Option.bind common (fun common ->
                         merge_element_types common ty))
                   (Some first_ty)
            in
            match common_type with
            | Some common_type ->
                let adapt_sequence index (inner, sequence) =
                  if
                    Types.equal common_type inner || Types.equal inner TUnknown
                    || match inner with TMeta _ | TVar _ -> true | _ -> false
                  then Ok sequence
                  else
                    let item_name =
                      "__lg_concat_common_item_" ^ string_of_int index
                    in
                    let item = typed_ir inner (Semantic_ir.Ident item_name) in
                    let adapted =
                      match (common_type, inner) with
                      | TNullable expected, actual
                        when Option.is_none (optional_payload actual) ->
                          Result.map
                            (fun item ->
                              Semantic_ir.Constructor ("Some", Some item))
                            (adapt_value_to_type env expected item)
                      | _ -> adapt_value_to_type env common_type item
                    in
                    Result.map
                      (fun item ->
                        Semantic_ir.Apply
                          ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                            [
                              Semantic_ir.Fun
                                ([ typed_item_pattern item_name inner ], item);
                              sequence;
                            ] ))
                      adapted
                in
                let rec adapt_sequences index adapted = function
                  | [] -> Ok (List.rev adapted)
                  | sequence :: rest ->
                      Result.bind (adapt_sequence index sequence) (fun sequence ->
                          adapt_sequences (index + 1) (sequence :: adapted) rest)
                in
                Result.map
                  (fun sequences ->
                    let sequence_names =
                      List.mapi
                        (fun index _ ->
                          "__lg_concat_sequence_" ^ string_of_int index)
                        sequences
                    in
                    let concatenated =
                      Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.concat",
                          [
                            Semantic_ir.List
                              (List.map
                                 (fun name -> Semantic_ir.Ident name)
                                 sequence_names);
                          ] )
                    in
                    let concatenated =
                      List.fold_right2
                        (fun name sequence body ->
                          Semantic_ir.Let
                            ([ (Semantic_ir.PVar name, sequence) ], body))
                        sequence_names sequences concatenated
                    in
                    typed_ir (TSeq common_type) concatenated)
                  (adapt_sequences 0 [] sequences)
            | None ->
                heterogeneous_collection_type_error "sequence"
                  (List.map fst sequences))
  and compile_metadata_call scope env name arg_forms =
    let expression_env = Env.with_expected_type None env in
    let compile_literal_map entries =
      let rec compile_entries compiled = function
        | [] -> Ok (List.rev compiled)
        | (key_form, value_form) :: rest -> (
            match
              ( compile_expr scope expression_env key_form,
                compile_expr scope expression_env value_form )
            with
            | (Error _ as error), _ | _, (Error _ as error) -> error
            | Ok key, Ok value ->
                compile_entries ((key, value) :: compiled) rest)
      in
      Result.bind (compile_entries [] entries) (fun entries ->
          let homogeneous_type select =
            match List.map select entries with
            | [] -> Ok TUnknown
            | first :: rest
              when List.for_all (fun ty -> Types.equal first ty) rest ->
                Ok first
            | _ ->
                Error.error
                  "metadata-bearing maps require homogeneous static key and value types"
          in
          match
            ( homogeneous_type (fun (key, _value) -> key.ty),
              homogeneous_type (fun (_key, value) -> value.ty) )
          with
          | (Error _ as error), _ | _, (Error _ as error) -> error
          | Ok key_ty, Ok value_ty ->
              let expression =
                List.fold_left
                  (fun map (key, value) ->
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Lg_runtime.Runtime_map.assoc",
                        [ map; key.semantic_expr; value.semantic_expr ] ))
                  (Semantic_ir.Ident "Lg_runtime.Runtime_map.empty") entries
              in
              Ok (typed_ir (Types.dynamic_map key_ty value_ty) expression))
    in
    let compile_metadata_operand = function
      | FMap entries -> compile_literal_map entries
      | form -> compile_expr scope env form
    in
    let compile_metadata_payload = function
      | FMap entries ->
          let rec compile_entries compiled = function
            | [] ->
                Ok
                  (typed_ir (TOcaml "Lg_edn_backend.t")
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_metadata.of_entries",
                          [ Semantic_ir.List (List.rev compiled) ] )))
            | (key_form, value_form) :: rest -> (
                match
                  ( compile_expr scope expression_env key_form,
                    compile_expr scope expression_env value_form )
                with
                | (Error _ as error), _ | _, (Error _ as error) -> error
                | Ok key, Ok value -> (
                    match
                      ( pack_metadata_expression key.ty key.semantic_expr,
                        pack_metadata_expression value.ty value.semantic_expr )
                    with
                    | (Error _ as error), _ | _, (Error _ as error) -> error
                    | Ok key, Ok value ->
                        compile_entries
                          (Semantic_ir.Tuple [ key; value ] :: compiled)
                          rest))
          in
          compile_entries [] entries
      | form -> compile_expr scope expression_env form
    in
    match (name, arg_forms) with
    | "__lg_with-meta", [ value_form; metadata_form ] -> (
        match
          ( compile_metadata_operand value_form,
            compile_metadata_payload metadata_form )
        with
        | (Error _ as error), _ -> error
        | _, (Error _ as error) -> error
        | Ok ({ ty = TNamed_record { nominal = true; _ }; _ } as value),
          Ok metadata
          when Protocol.type_satisfies env Core_protocols.with_meta_id value.ty ->
            let value_name = "__lg_with_meta_value" in
            let metadata_name = "__lg_with_meta_metadata" in
            let protocol_env =
              env
              |> Env.add (Names.scoped_key scope value_name)
                   (Types.binding value_name value.ty)
              |> Env.add (Names.scoped_key scope metadata_name)
                   (Types.binding metadata_name metadata.ty)
            in
            Result.map
              (fun result ->
                { result with
                  semantic_expr =
                    Semantic_ir.Let
                      ( [ (Semantic_ir.PVar value_name, value.semantic_expr);
                          ( Semantic_ir.PVar metadata_name,
                            metadata.semantic_expr );
                        ],
                        result.semantic_expr );
                })
              (compile_expr scope protocol_env
                 (FList
                    [ FSymbol "IWithMeta/-with-meta";
                      FSymbol value_name;
                      FSymbol metadata_name;
                    ]))
        | Ok { ty = TNamed_record _; _ }, Ok _ ->
            Error.error
              "with-meta requires the nominal record to implement IWithMeta"
        | Ok value, Ok metadata
          when Option.is_some (Types.dynamic_map_types value.ty) ->
            Result.map
              (fun metadata ->
                typed_ir value.ty
                  (Semantic_ir.Apply
                     ( Semantic_ir.Ident
                         "Lg_runtime.Runtime_map.with_metadata",
                       [ value.semantic_expr; metadata ] )))
              (pack_metadata_expression metadata.ty metadata.semantic_expr)
        | Ok _, Ok _ ->
            Error.error
              "with-meta requires a statically typed map implementing IWithMeta")
    | "__lg_with-meta", _ -> Error.error "__lg_with-meta expects 2 arguments"
    | _ -> assert false
  and compile_into scope env target_form source_form =
    let expression_env = Env.with_expected_type None env in
    let adapt_result result =
      match Env.expected_type env with
      | Some expected when not (Types.equal expected result.ty) ->
          Result.map
            (fun semantic_expr -> typed_ir expected semantic_expr)
            (adapt_value_to_type env expected result)
      | Some _ | None -> Ok result
    in
    match
      ( compile_expr scope expression_env target_form,
        compile_expr scope expression_env source_form )
    with
    | (Error _ as error), _ -> error
    | _, (Error _ as error) -> error
    | Ok target, Ok source when Types.is_dynamic target.ty -> (
        match Collection_capability.to_seq_expr env source with
        | Error _ -> Error.error "into source must be a collection"
        | Ok (element_ty, sequence) -> (
            let item_name = "__lg_into_item" in
            let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
            match
               pack_dynamic_value env (Types.dynamic_constraint TUnknown) item
             with
            | Error _ as error -> error
            | Ok packed_item ->
                let packed_sequence =
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                      [
                        Semantic_ir.Fun
                          ([ Semantic_ir.PVar item_name ], packed_item);
                        sequence;
                      ] )
                in
                Ok
                  (typed_ir target.ty
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.into",
                          [ target.semantic_expr; packed_sequence ] )))))
    | Ok target, Ok source when Types.is_dynamic source.ty -> (
        match Collection_capability.to_seq_expr env source with
        | Error _ -> Error.error "into source must be a collection"
        | Ok (element_ty, sequence) ->
            let source =
              typed_ir (TList element_ty)
                (Semantic_ir.Apply
                   ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.to_list",
                     [ sequence ] ))
            in
            Core_sequence_transform.compile "__lg_into" [ target; source ])
    | Ok target, Ok source -> (
        match Core_sequence_transform.compile "__lg_into" [ target; source ] with
        | Ok result -> adapt_result result
        | Error _ -> (
            match Collection_capability.to_seq_expr env source with
            | Error _ -> Error.error "into source must be a collection"
            | Ok (element_ty, sequence) ->
                let widen_record_vector target_element =
                  let dynamic = Types.dynamic_constraint TUnknown in
                  let target_item_name = "__lg_into_target_item" in
                  let source_item_name = "__lg_into_source_item" in
                  let target_item =
                    typed_ir target_element
                      (Semantic_ir.Ident target_item_name)
                  in
                  let source_item =
                    typed_ir element_ty (Semantic_ir.Ident source_item_name)
                  in
                  Result.bind
                    (pack_dynamic_value env dynamic target_item)
                    (fun packed_target_item ->
                      Result.map
                        (fun packed_source_item ->
                          typed_ir (TVector dynamic)
                            (Semantic_ir.Apply
                               ( Semantic_ir.Ident "Rrbvec.append_list",
                                 [
                                   Semantic_ir.Apply
                                     ( Semantic_ir.Ident "Rrbvec.map",
                                       [
                                         Semantic_ir.Fun
                                           ( [
                                               typed_dynamic_item_pattern env
                                                 target_item_name
                                                 target_element;
                                             ],
                                             packed_target_item );
                                         target.semantic_expr;
                                       ] );
                                   Semantic_ir.Apply
                                     ( Semantic_ir.Ident
                                         "Lg_runtime.Runtime_seq.to_list",
                                       [
                                         Semantic_ir.Apply
                                           ( Semantic_ir.Ident
                                               "Lg_runtime.Runtime_seq.map",
                                             [
                                               Semantic_ir.Fun
                                                 ( [
                                                     typed_dynamic_item_pattern
                                                       env source_item_name
                                                       element_ty;
                                                   ],
                                                   packed_source_item );
                                               sequence;
                                             ] );
                                       ] );
                                 ] )) )
                        (pack_dynamic_value env dynamic source_item))
                in
                let target_element =
                  match target.ty with
                  | TVector inner | TList inner | TSet inner -> Some inner
                  | _ -> None
                in
                let record_type = function
                  | TRecord _ | TNamed_record _ -> true
                  | _ -> false
                in
                let adapt_source target_element =
                  let item_name = "__lg_into_adapted_item" in
                  let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
                  let compatible =
                    Types.assignable ~policy:Host_boundary
                      ~expected:target_element ~actual:element_ty
                    ||
                    match optional_payload target_element with
                    | Some inner ->
                        Types.assignable ~policy:Host_boundary ~expected:inner
                          ~actual:element_ty
                    | None -> false
                  in
                  if not compatible then
                    Error.error
                      "into source element type must match target element type"
                  else
                    Result.map
                      (fun item ->
                        typed_ir (TList target_element)
                          (Semantic_ir.Apply
                             ( Semantic_ir.Ident
                                 "Lg_runtime.Runtime_seq.to_list",
                               [ Semantic_ir.Apply
                                   ( Semantic_ir.Ident
                                       "Lg_runtime.Runtime_seq.map",
                                     [ Semantic_ir.Fun
                                         ([ Semantic_ir.PVar item_name ], item);
                                       sequence;
                                     ] );
                               ] )))
                      (adapt_value_to_type env target_element item)
                in
                match target_element with
                | Some target_element
                  when (match target.ty with TVector _ -> true | _ -> false)
                       && not (Types.equal target_element element_ty) ->
                    if record_type target_element && record_type element_ty then
                      widen_record_vector target_element
                    else
                      (match adapt_source target_element with
                      | Ok source ->
                          Core_sequence_transform.compile "__lg_into"
                            [ target; source ]
                      | Error _ as error -> error)
                | _ ->
                    let source =
                      match target_element with
                      | Some target_element
                        when not (Types.equal target_element element_ty) ->
                          adapt_source target_element
                      | Some _ | None ->
                          Ok
                            (typed_ir (TList element_ty)
                               (Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Runtime_seq.to_list",
                                    [ sequence ] )))
                    in
                    Result.bind source (fun source ->
                        Core_sequence_transform.compile "__lg_into"
                          [ target; source ])))
  and compile_sequence_transform_call scope env name arg_forms =
    match (name, arg_forms) with
    | "__lg_interleave", collection_forms -> (
        match compile_args_for scope env collection_forms with
        | Error _ as error -> error
        | Ok collections when List.length collections < 2 ->
            Error.error "__lg_interleave expects at least two collections"
        | Ok collections ->
            let rec collect prepared = function
              | [] -> Ok (List.rev prepared)
              | collection :: rest -> (
                  match Collection_capability.to_seq_expr env collection with
                  | Error _ -> Error.error "interleave expects collections"
                  | Ok (element_type, sequence) ->
                      collect ((element_type, sequence) :: prepared) rest)
            in
            Result.bind (collect [] collections) (fun prepared ->
                let unresolved = function
                  | TUnknown | TMeta _ | TVar _ -> true
                  | _ -> false
                in
                let common_type =
                  prepared
                  |> List.find_map (fun (element_type, _) ->
                         if unresolved element_type then None
                         else Some element_type)
                  |> Option.value ~default:(fst (List.hd prepared))
                in
                if
                  List.exists
                    (fun (element_type, _) ->
                      (not (unresolved element_type))
                      && not (Types.equal common_type element_type))
                    prepared
                then Error.error "interleave element types must match"
                else
                  let sequences = List.map snd prepared in
                  let sequence_names =
                    List.mapi
                      (fun index _ ->
                        "__lg_interleave_sequence_" ^ string_of_int index)
                      sequences
                  in
                  let result =
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.interleave",
                        [ Semantic_ir.List
                            (List.map
                               (fun name -> Semantic_ir.Ident name)
                               sequence_names) ] )
                  in
                  let result =
                    List.fold_right2
                      (fun name sequence body ->
                        Semantic_ir.Let
                          ([ (Semantic_ir.PVar name, sequence) ], body))
                      sequence_names sequences result
                  in
                  Ok (typed_ir (TSeq common_type) result)))
    | "__lg_sort", [ comparator_form; collection_form ] -> (
        match compile_expr scope env collection_form with
        | Error _ as error -> error
        | Ok collection -> (
            match Collection_capability.to_seq_expr env collection with
            | Error _ -> Error.error "sort expects a seqable value"
            | Ok (inner, sequence) -> (
                match compile_function_arg scope env comparator_form with
                | Error _ as error -> error
                | Ok
                    ({ ty = TFn ([ left_ty; right_ty ], TInt); _ } as comparator)
                  when Types.assignable ~policy:Host_boundary ~expected:left_ty
                         ~actual:inner
                       && Types.assignable ~policy:Host_boundary
                            ~expected:right_ty ~actual:inner ->
                    let comparator_expression =
                      if
                        Types.is_dynamic left_ty && Types.is_dynamic right_ty
                        && not (Types.is_dynamic inner)
                      then
                        let left_name = "__lg_sort_left" in
                        let right_name = "__lg_sort_right" in
                        let left = typed_ir inner (Semantic_ir.Ident left_name) in
                        let right = typed_ir inner (Semantic_ir.Ident right_name) in
                        Result.bind (pack_dynamic_value env left_ty left)
                          (fun left ->
                            Result.map
                              (fun right ->
                                Semantic_ir.Fun
                                  ( [
                                      Semantic_ir.PVar left_name;
                                      Semantic_ir.PVar right_name;
                                    ],
                                    Semantic_ir.Apply
                                      ( comparator.semantic_expr,
                                        [ left; right ] ) ))
                              (pack_dynamic_value env right_ty right))
                      else Ok comparator.semantic_expr
                    in
                    Result.map
                      (fun comparator_expression ->
                        let left_name = "__lg_sort_left" in
                        let right_name = "__lg_sort_right" in
                        let host_comparator =
                          Semantic_ir.Fun
                            ( [
                                Semantic_ir.PVar left_name;
                                Semantic_ir.PVar right_name;
                              ],
                              Semantic_ir.Apply
                                ( comparator_expression,
                                  [
                                    Semantic_ir.Ident left_name;
                                    Semantic_ir.Ident right_name;
                                  ] ) )
                        in
                        typed_ir (TList inner)
                          (Semantic_ir.Apply
                             ( Semantic_ir.Ident "List.sort",
                               [
                                 host_comparator;
                                 Semantic_ir.Apply
                                   ( Semantic_ir.Ident
                                       "Lg_runtime.Runtime_seq.to_list",
                                     [ sequence ] );
                               ] )))
                      comparator_expression
                | Ok _ -> Error.error "sort expects a comparator function")))
    | "__lg_sort", [ collection_form ] -> (
        match compile_expr scope env collection_form with
        | Error _ as error -> error
        | Ok collection -> (
            match Core_sequence_transform.compile "sort" [ collection ] with
            | Ok _ as result -> result
            | Error _ -> (
                match Collection_capability.to_seq_expr env collection with
                | Error _ -> Error.error "sort expects a seqable value"
                | Ok (inner, sequence) ->
                    Core_sequence_transform.compile "sort"
                      [
                        typed_ir (TList inner)
                          (Semantic_ir.Apply
                             ( Semantic_ir.Ident
                                 "Lg_runtime.Runtime_seq.to_list",
                               [ sequence ] ));
                      ] )))
    | _ -> (
        match compile_args_for scope env arg_forms with
        | Error _ as err -> err
        | Ok args -> Core_sequence_transform.compile name args)
  and compile_set scope env arg_forms =
    match compile_args_for scope env arg_forms with
    | Error _ as error -> error
    | Ok [ collection ] -> (
        match Collection_capability.to_seq_expr env collection with
        | Error _ -> Error.error "set expects a seqable value"
        | Ok (inner, sequence) -> (
            let inner =
              Collection_capability.resolve_callback_record env inner
            in
            if Types.is_dynamic inner then
              Error.error
                "set requires a static element type; define a sum type for a \
                 heterogeneous domain"
            else
              Core_sequence_transform.compile "set"
                [
                  typed_ir (TList inner)
                    (Semantic_ir.Apply
                       (Semantic_ir.Ident "List.of_seq", [ sequence ]));
                ]))
    | Ok _ -> Error.error "set expects 1 arguments"
  and compile_cons scope env arg_forms =
    match compile_args_for scope env arg_forms with
    | Error _ as error -> error
    | Ok [ _; _ ] -> compile_static_cons scope env arg_forms
    | Ok _ -> Error.error "cons expects a value and seqable collection"
  and compile_conj scope env arg_forms =
    match compile_args_for scope env arg_forms with
    | Error _ as error -> error
    | Ok (_ :: _ :: _) -> compile_static_conj scope env arg_forms
    | Ok _ -> Error.error "conj expects collection and values"

  and compile_get_in scope env arg_forms =
    let compile_dynamic_get_in target_form path_form default_form =
      let dynamic_ty = Types.dynamic_constraint TUnknown in
      Result.bind (compile_expr scope env target_form) (fun target ->
          Result.bind (compile_expr scope env path_form) (fun path ->
              let path =
                match path.ty with
                | TUnknown | TMeta _ | TVar _ ->
                    Result.map
                      (fun packed -> typed_ir dynamic_ty packed)
                      (pack_dynamic_value env dynamic_ty path)
                | _ -> Ok path
              in
              Result.bind path (fun path ->
              match Collection_capability.to_seq_expr env path with
              | Error _ -> Error.error "get-in path must be seqable"
              | Ok (inner, sequence) ->
                  let item_name = "__lg_get_in_key" in
                  let item = typed_ir inner (Semantic_ir.Ident item_name) in
                  Result.bind (pack_dynamic_value env dynamic_ty target)
                    (fun packed_target ->
                      Result.bind (pack_dynamic_value env dynamic_ty item)
                        (fun packed_item ->
                          let keys =
                            Semantic_ir.Apply
                              ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                                [
                                  Semantic_ir.Fun
                                    ([ Semantic_ir.PVar item_name ], packed_item);
                                  sequence;
                                ] )
                          in
                          match default_form with
                          | None ->
                              Ok
                                (typed_ir dynamic_ty
                                   (Semantic_ir.Apply
                                      ( Semantic_ir.Ident
                                          "Lg_runtime.Runtime_dynamic.get_in",
                                        [ packed_target; keys ] )))
                          | Some default_form ->
                              Result.bind
                                (compile_expr scope env default_form)
                                (fun default ->
                                  Result.map
                                    (fun packed_default ->
                                      typed_ir dynamic_ty
                                        (Semantic_ir.Apply
                                           ( Semantic_ir.Ident
                                               "Lg_runtime.Runtime_dynamic.get_in_default",
                                             [
                                               packed_target;
                                               keys;
                                               packed_default;
                                             ] )))
                                    (pack_dynamic_value env dynamic_ty default)))))))
    in
    match arg_forms with
    | [ target; FVector keys ] ->
        compile_expr scope env (Core_form_expansion.get_in target keys None)
    | [ target; FVector keys; default ] ->
        compile_expr scope env
          (Core_form_expansion.get_in target keys (Some default))
    | [ target; path ] -> compile_dynamic_get_in target path None
    | [ target; path; default ] ->
        compile_dynamic_get_in target path (Some default)
    | _ -> Error.error "get-in expects target, path, and optional default"
  and compile_assoc_in scope env arg_forms =
    match arg_forms with
    | [ target; FVector keys; value ] ->
        compile_expr scope env (Core_form_expansion.assoc_in target keys value)
    | [ _target; _path; _value ] ->
        Error.error "assoc-in currently requires a vector path"
    | _ -> Error.error "assoc-in expects target, path, and value"
  and compile_update_in scope env arg_forms =
    match arg_forms with
    | target_form :: FVector (_ :: _ as keys) :: function_form :: argument_forms
      ->
        compile_expr scope env
          (Core_form_expansion.update_in target_form keys function_form
             argument_forms)
    | _target_form :: _path_form :: _function_form :: _argument_forms ->
        Error.error
          "update-in requires a statically known vector path; define a typed \
           helper for computed paths"
    | _ ->
        Error.error
          "update-in expects target, path, function, and optional arguments"
  and compile_function_arg scope env form =
    let compiled =
      match form with
      | FSymbol name -> (
          match lookup_binding scope env name with
          | Ok binding ->
              let binding = Types.instantiate_binding binding in
              Ok
                (typed_ir binding.ty
                   (binding_value_expression binding))
          | Error _ -> lookup_function scope env name)
      | form -> compile_expr scope env form
    in
    Result.bind compiled adapt_set_callable
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
  and contextual_callback_form expected form =
    let annotation = function
      | TInt -> Some "^int"
      | TFloat -> Some "^double"
      | TBool -> Some "^boolean"
      | TString -> Some "^:string"
      | TKeyword -> Some "^:keyword"
      | TSymbol -> Some "^:symbol"
      | ty when Types.is_dynamic ty -> Some "^:dynamic"
      | TNamed_record record -> Some ("^" ^ record.type_name)
      | _ -> None
    in
    let rec annotate_parameters expected parameters =
      match (expected, parameters) with
      | [], parameters -> parameters
      | _expected :: _, [] -> []
      | _expected_ty :: expected_rest,
        (FSymbol metadata as metadata_form) :: (FSymbol _ as parameter)
        :: rest
        when String.starts_with ~prefix:"^" metadata ->
          metadata_form :: parameter
          :: annotate_parameters expected_rest rest
      | expected_ty :: expected_rest, (FSymbol "&" as rest_marker) :: rest ->
          rest_marker :: annotate_parameters (expected_ty :: expected_rest) rest
      | expected_ty :: expected_rest, (FSymbol _ as parameter) :: rest -> (
          match annotation expected_ty with
          | None -> parameter :: annotate_parameters expected_rest rest
          | Some metadata ->
              FSymbol metadata :: parameter
              :: annotate_parameters expected_rest rest)
      | _expected_ty :: expected_rest, parameter :: rest ->
          parameter :: annotate_parameters expected_rest rest
    in
    match (expected, form) with
    | TFn ([ parameter_ty ], _), FKeyword keyword ->
        let parameter = "__lg_keyword_callback_argument" in
        FList
          [
            FSymbol "fn";
            FVector
              (annotate_parameters [ parameter_ty ] [ FSymbol parameter ]);
            FList [ FKeyword keyword; FSymbol parameter ];
          ]
    | TFn (parameter_tys, _),
      FList (FSymbol "fn" :: FVector parameters :: body_forms) ->
        FList
          (FSymbol "fn"
          :: FVector (annotate_parameters parameter_tys parameters)
          :: body_forms)
    | _ -> form
  and compile_named_function_call scope env name arg_forms =
    match lookup_binding scope env name with
    | Error _ -> (
        match Protocol.lookup_marker scope env name with
        | Some _ -> compile_protocol_call scope env name arg_forms
        | None -> (
            match ocaml_call_target scope env name with
            | Some _ -> compile_inferred_ocaml_call scope env name arg_forms
            | None -> compile_protocol_call scope env name arg_forms))
    | Ok { protocol_id = Some _; _ } ->
        compile_protocol_call scope env name arg_forms
    | Ok { host_reference = Some (Ocaml_value _); _ } ->
        compile_inferred_ocaml_call scope env name arg_forms
    | Ok fn -> (
        let fn = Types.instantiate_binding fn in
        let contextual_parameter_tys =
          match fn.ty with
          | TFn (parameter_tys, _)
            when List.length parameter_tys = List.length arg_forms ->
              Some parameter_tys
          | TOverloaded_fn arities ->
              select_overloaded_arity arities (List.length arg_forms)
              |> Option.map (fun (_, arity) ->
                     arity.fixed_params
                     @
                     match arity.rest_param with
                     | None -> []
                     | Some rest_ty ->
                         List.init
                           (List.length arg_forms
                           - List.length arity.fixed_params)
                           (fun _ -> rest_ty))
          | _ -> None
        in
        let arg_forms =
          match contextual_parameter_tys with
          | Some parameter_tys ->
              List.map2 contextual_callback_form parameter_tys arg_forms
          | None -> arg_forms
        in
        let compile_arguments =
          match contextual_parameter_tys with
          | None -> compile_args_for scope env arg_forms
          | Some parameter_tys ->
              let forms = Array.of_list arg_forms in
              let parameters = Array.of_list parameter_tys in
              let arguments = Array.make (Array.length forms) None in
              let deferred_callback index =
                match (parameters.(index), forms.(index)) with
                | TFn _, (FList (FSymbol "fn" :: _) | FKeyword _) -> true
                | _ -> false
              in
              let compile_argument expected form =
                let form = contextual_callback_form expected form in
                let argument_env =
                  match (expected, form) with
                  | TFn _, FList (FSymbol "fn" :: _) ->
                      Env.with_expected_type (Some expected) env
                  | _ -> Env.with_expected_type None env
                in
                match expected with
                | TFn _ -> compile_function_arg scope argument_env form
                | _ -> compile_expr scope argument_env form
              in
              let rec compile_non_callbacks index =
                if index = Array.length forms then Ok ()
                else if deferred_callback index then
                  compile_non_callbacks (index + 1)
                else
                  Result.bind
                    (compile_argument parameters.(index) forms.(index))
                    (fun argument ->
                      arguments.(index) <- Some argument;
                      compile_non_callbacks (index + 1))
              in
              let infer_substitutions () =
                let substitutions = ref Type_solver.empty in
                Array.iteri
                  (fun index argument ->
                    match argument with
                    | None -> ()
                    | Some argument ->
                        let expected = parameters.(index) in
                        let inferred =
                          match
                            ( Types.seqable_constraint_element expected,
                              Collection_capability.element_type env argument )
                          with
                          | Some expected_element, Some actual_element ->
                              Type_solver.unify !substitutions expected_element
                                actual_element
                          | _ ->
                              let expected, actual =
                                align_optional_inference expected argument.ty
                              in
                              Type_solver.unify !substitutions expected actual
                        in
                        substitutions :=
                          Result.value inferred ~default:!substitutions)
                  arguments;
                !substitutions
              in
              let rec compile_callbacks substitutions index =
                if index = Array.length forms then Ok ()
                else if not (deferred_callback index) then
                  compile_callbacks substitutions (index + 1)
                else
                  let expected =
                    Type_solver.apply substitutions parameters.(index)
                  in
                  Result.bind (compile_argument expected forms.(index))
                    (fun argument ->
                      arguments.(index) <- Some argument;
                      compile_callbacks substitutions (index + 1))
              in
              let collect_arguments () =
                arguments |> Array.to_list
                |> List.map (function Some argument -> argument | None -> assert false)
              in
              Result.bind (compile_non_callbacks 0) (fun () ->
                  let substitutions = infer_substitutions () in
                  Result.map collect_arguments
                    (compile_callbacks substitutions 0))
        in
        match compile_arguments with
        | Error _ as err -> err
        | Ok args -> (
            let callable =
              static_deftype_callable env fn.ty (List.length args + 1)
            in
            match fn.ty with
            | (TUnknown | TMeta _ | TVar _)
              when match arg_forms with
                   | [ _key; FSymbol "nil" ] -> true
                   | _ -> false -> (
                match args with
                | [ key; _default ] ->
                    Ok
                      (typed_ir (TNullable TUnknown)
                         (Semantic_ir.Apply
                            ( Semantic_ir.Ident
                                "Lg_runtime.Runtime_map.get_option",
                              [
                                Semantic_ir.Ident fn.ocaml_name;
                                key.semantic_expr;
                              ] )))
                | _ -> assert false)
            | TOcaml "__declared_fn" | TUnknown | TMeta _ | TVar _ ->
                Ok
                  (typed_ir TUnknown
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident fn.ocaml_name,
                          List.map (fun arg -> arg.semantic_expr) args )))
            | TOcaml_app
                ("Lg_runtime.Runtime_map.t", [ key_ty; value_ty ]) -> (
                let map = Semantic_ir.Ident fn.ocaml_name in
                match args with
                | [ key ]
                  when Types.assignable ~policy:Host_boundary ~expected:key_ty
                         ~actual:key.ty ->
                    Ok
                      (typed_ir (TNullable value_ty)
                         (Semantic_ir.Apply
                            ( Semantic_ir.Ident
                                "Lg_runtime.Runtime_map.get_option",
                              [ map; key.semantic_expr ] )))
                | [ key; default ]
                  when Types.assignable ~policy:Host_boundary ~expected:key_ty
                         ~actual:key.ty
                       && Types.assignable ~policy:Host_boundary
                            ~expected:value_ty ~actual:default.ty ->
                    Ok
                      (typed_ir value_ty
                         (Semantic_ir.Apply
                            ( Semantic_ir.Ident
                                "Lg_runtime.Runtime_map.get_default",
                              [
                                map;
                                key.semantic_expr;
                                default.semantic_expr;
                              ] )))
                | [ _ ] | [ _; _ ] ->
                    Error.error
                      (name ^ " called with incompatible map lookup arguments")
                | _ ->
                    Error.error
                      (name ^ " map lookup expects 1 or 2 arguments"))
            | TOverloaded_fn arities -> (
                match select_overloaded_arity arities (List.length args) with
                | None ->
                    Error.error
                      (name ^ " called with unsupported arity "
                     ^ string_of_int (List.length args))
                | Some (arity_index, arity) -> (
                    let fixed_count = List.length arity.fixed_params in
                    let rec split_at count acc values =
                      if count = 0 then (List.rev acc, values)
                      else
                        match values with
                        | [] -> (List.rev acc, [])
                        | value :: rest ->
                            split_at (count - 1) (value :: acc) rest
                    in
                    let fixed_args, extra_args = split_at fixed_count [] args in
                    let add_element_candidate candidates ty =
                      if
                        Types.equal ty TUnknown || Types.is_dynamic ty
                        || match ty with TMeta _ | TVar _ -> true | _ -> false
                      then candidates
                      else if List.exists (Types.equal ty) candidates then
                        candidates
                      else ty :: candidates
                    in
                    let element_candidates =
                      List.fold_left2
                        (fun candidates expected argument ->
                          match
                            Option.map
                              (fun (_, element_ty, _) -> element_ty)
                              (Types.seqable_constraint_info expected)
                          with
                          | Some (TUnknown | TMeta _ | TVar _) -> (
                              match
                                Collection_capability.element_type env argument
                              with
                              | Some element_ty ->
                                  add_element_candidate candidates element_ty
                              | None -> candidates)
                          | _ -> (
                              match (expected, argument.ty) with
                              | (TUnknown | TMeta _ | TVar _), TFn (parameters, _) ->
                                  List.fold_left add_element_candidate candidates
                                    parameters
                              | (TUnknown | TMeta _ | TVar _), actual ->
                                  add_element_candidate candidates actual
                              | _ -> candidates))
                        [] arity.fixed_params fixed_args
                    in
                    let element_ty =
                      match element_candidates with
                      | [ element_ty ] -> Some element_ty
                      | [] | _ :: _ :: _ -> None
                    in
                    let specialize_expected substitutions expected argument =
                      match expected with
                      | TOcaml_app
                          ( constraint_name,
                            [ ((TUnknown | TMeta _ | TVar _) as element_template);
                              _;
                            ] )
                        when constraint_name = Types.seqable_constraint_name
                             || constraint_name
                                = Types.optional_seqable_constraint_name
                             || constraint_name
                                = Types.optional_sequential_constraint_name ->
                          let specialized_element =
                            Type_solver.apply substitutions element_template
                          in
                          (match specialized_element with
                          | TUnknown | TMeta _ | TVar _ -> (
                              match element_ty with
                              | Some element_ty ->
                                  TOcaml_app
                                    ( constraint_name,
                                      [ element_ty; argument.ty ] )
                              | None -> expected)
                          | specialized_element ->
                              TOcaml_app
                                ( constraint_name,
                                  [ specialized_element; argument.ty ] ))
                      | TUnknown | TMeta _ | TVar _ -> (
                          match element_ty with
                          | Some _ -> argument.ty
                          | None -> expected)
                      | TNamed_record
                          ({ type_arguments = [ (TUnknown | TMeta _ | TVar _) ]; _ } as
                          record) -> (
                          match element_ty with
                          | Some element_ty ->
                              TNamed_record
                                { record with type_arguments = [ element_ty ] }
                          | None ->
                              Types.instantiate_type ~templates:[ expected ]
                                ~actuals:[ argument.ty ] expected)
                      | expected ->
                          Types.instantiate_type ~templates:[ expected ]
                            ~actuals:[ argument.ty ] expected
                    in
                    let substitutions =
                      List.fold_left2
                        (fun result template argument ->
                          Result.bind result (fun substitutions ->
                              let template, actual =
                                align_optional_inference template argument.ty
                              in
                              if Types.is_dynamic actual then Ok substitutions
                              else
                                match (template, actual) with
                                | ( TFn (template_params, template_return),
                                    TFn (actual_params, actual_return) )
                                  when List.length template_params
                                       = List.length actual_params -> (
                                    match
                                      ( Types.seqable_constraint_info
                                          template_return,
                                        Collection_capability.element_type env
                                          (typed_ir actual_return
                                             Semantic_ir.Unit) )
                                    with
                                    | Some (_, template_element, _),
                                      Some actual_element ->
                                        Result.bind
                                          (List.fold_left2
                                             (fun result template actual ->
                                               Result.bind result
                                                 (fun substitutions ->
                                                   let template =
                                                     Type_solver.apply
                                                       substitutions template
                                                   in
                                                   if
                                                     callback_record_compatible
                                                       env template actual
                                                   then Ok substitutions
                                                   else
                                                     Type_solver.unify
                                                       substitutions template
                                                       actual))
                                             (Ok substitutions) template_params
                                             actual_params)
                                          (fun substitutions ->
                                            Type_solver.unify substitutions
                                              template_element actual_element)
                                    | _ ->
                                        Ok
                                          (Type_solver.unify substitutions
                                             template actual
                                          |> Result.value
                                               ~default:substitutions))
                                | _ ->
                                match Types.protocol_constraint_info template with
                                | Some _ ->
                                    Ok
                                      (Protocol.infer_constraint_substitutions
                                         env substitutions template
                                         (Types.constraint_value_type actual))
                                | None ->
                                match
                                  ( Option.map
                                      (fun (_, element_ty, _) -> element_ty)
                                      (Types.seqable_constraint_info template),
                                    Collection_capability.element_type env
                                      argument )
                                with
                                | Some expected_element, Some actual_element ->
                                    let expected_element =
                                      Type_solver.apply substitutions
                                        expected_element
                                    in
                                    if
                                      callback_record_compatible env
                                        expected_element actual_element
                                    then Ok substitutions
                                    else (
                                      match
                                        Type_solver.unify substitutions
                                          expected_element actual_element
                                      with
                                    | Ok substitutions ->
                                        Ok
                                          (Type_solver.unify substitutions
                                             template actual
                                          |> Result.value
                                               ~default:substitutions)
                                    | Error _
                                      when Option.is_some
                                             (Types.seqable_constraint_info
                                                (Type_solver.apply substitutions
                                                   expected_element))
                                           && Collection_capability
                                              .accepts_seqable env actual_element
                                      ->
                                        Ok substitutions
                                    | Error _
                                      when Option.is_some
                                             (Types.protocol_constraint_info
                                                (Type_solver.apply substitutions
                                                   expected_element)) ->
                                        Ok
                                          (Protocol.infer_constraint_substitutions
                                             env substitutions
                                            (Type_solver.apply substitutions
                                              expected_element)
                                              actual_element)
                                    | Error _ as error -> error)
                                | None, _ | _, None ->
                                    Ok
                                      (Type_solver.unify substitutions template
                                         actual
                                      |> Result.value ~default:substitutions)))
                        (Ok Type_solver.empty) arity.fixed_params fixed_args
                    in
                    let seqable_elements_compatible, substitutions =
                      match substitutions with
                      | Ok substitutions -> (true, substitutions)
                      | Error _ -> (false, Type_solver.empty)
                    in
                    let fixed_param_tys =
                      List.map2
                        (specialize_expected substitutions)
                        arity.fixed_params fixed_args
                      |> List.map (Type_solver.apply substitutions)
                    in
                    let rest_param_ty =
                      Option.map (Type_solver.apply substitutions)
                        arity.rest_param
                    in
                    let seqable_storage_return_ty =
                      List.combine arity.fixed_params fixed_args
                      |> List.find_map (fun (parameter_ty, argument) ->
                             match
                               Types.seqable_constraint_info parameter_ty
                             with
                             | Some (_, _, storage_ty)
                               when Types.equal arity.return_ty storage_ty ->
                                 Some
                                   (Types.constraint_value_type argument.ty)
                             | Some _ | None -> None)
                    in
                    let return_ty =
                      match seqable_storage_return_ty with
                      | Some storage_ty -> storage_ty
                      | None -> (
                          let inferred_return_ty =
                            Type_solver.apply substitutions arity.return_ty
                          in
                          match element_ty with
                          | Some element_ty ->
                          let rec specialize = function
                            | TOcaml_app (name, _) as constraint_ty
                              when Option.is_some
                                     (Types.protocol_constraint_id name) ->
                                constraint_ty
                            | TUnknown | TMeta _ -> element_ty
                            | TVar _ as ty -> ty
                            | TNullable ty -> TNullable (specialize ty)
                            | TArray ty -> TArray (specialize ty)
                            | TRef ty -> TRef (specialize ty)
                            | TList ty -> TList (specialize ty)
                            | TVector ty -> TVector (specialize ty)
                            | TSet ty -> TSet (specialize ty)
                            | TSeq ty -> TSeq (specialize ty)
                            | TOcaml_app (name, arguments) ->
                                TOcaml_app
                                  (name, List.map specialize arguments)
                            | TTuple items -> TTuple (List.map specialize items)
                            | ty -> ty
                          in
                              specialize inferred_return_ty
                          | None -> inferred_return_ty)
                    in
                    let return_ty =
                      Type_solver.apply substitutions return_ty
                    in
                    let storage_return_ty = arity.return_ty in
                    let return_ty =
                      match return_ty with
                      | TSeq element_ty ->
                          TSeq (Types.constraint_value_type element_ty)
                      | return_ty -> return_ty
                    in
                    let row_param_types =
                      List.nth_opt fn.overload_row_param_types arity_index
                      |> Option.value ~default:[]
                    in
                    let compatible expected actual =
                      named_argument_compatible expected actual
                      ||
                      (Option.is_some (Types.seqable_constraint_info expected)
                      && Collection_capability.accepts_seqable env actual)
                    in
                    let fixed_compatible =
                      List.for_all2
                        (fun expected arg ->
                          match expected with
                          | TNullable (TRecord fields) ->
                              row_argument_compatible fields arg.ty
                          | _ -> compatible expected arg.ty)
                        fixed_param_tys fixed_args
                    in
                    let open_set_elements_compatible =
                      List.combine arity.fixed_params fixed_args
                      |> List.filter_map (fun (expected, argument) ->
                             match (expected, argument.ty) with
                             | TSet (TUnknown | TMeta _ | TVar _), TSet element
                               -> Some element
                             | _ -> None)
                      |> function
                      | [] | [ _ ] -> true
                      | first :: rest ->
                          List.for_all (Types.same_shape first) rest
                    in
                    let rest_compatible =
                      match rest_param_ty with
                      | None -> extra_args = []
                      | Some expected ->
                          List.for_all
                            (fun arg ->
                              let expected_payload =
                                optional_payload expected
                                |> Option.value ~default:expected
                              in
                              if
                                Option.is_some
                                  (Types.dynamic_map_types expected_payload)
                              then
                                if Types.equal arg.ty TNil then true
                                else
                                  let expected, actual =
                                    align_optional_inference expected arg.ty
                                  in
                                  Result.is_ok
                                    (Type_solver.unify Type_solver.empty expected
                                       actual)
                              else
                                compatible expected arg.ty)
                            extra_args
                    in
                    if not open_set_elements_compatible then
                      Error.error
                        (name ^ " expects sets with the same element type")
                    else if
                      not
                        (seqable_elements_compatible && fixed_compatible
                       && rest_compatible)
                    then
                      Error.error
                        (name ^ " called with incompatible arguments: expected ("
                       ^ String.concat ", "
                           (List.map Types.source_name fixed_param_tys)
                       ^ "), got ("
                       ^ String.concat ", "
                           (List.map
                              (fun argument -> Types.source_name argument.ty)
                              fixed_args)
                       ^ "); incompatible positions: "
                       ^ String.concat ", "
                           (List.mapi
                              (fun index compatible ->
                                if compatible then None
                                else Some (string_of_int (index + 1)))
                              (List.map2
                                 (fun expected arg ->
                                   match expected with
                                   | TNullable (TRecord fields) ->
                                       row_argument_compatible fields arg.ty
                                   | _ ->
                                       compatible expected arg.ty)
                                 fixed_param_tys fixed_args)
                           |> List.filter_map Fun.id))
                    else
                      let prepare_argument index expected argument =
                        let storage_expected =
                          match List.nth_opt arity.fixed_params index with
                          | Some ty -> ty
                          | None ->
                              Option.value arity.rest_param ~default:expected
                        in
                        if
                          match (storage_expected, argument.ty) with
                          | TSet (TUnknown | TMeta _ | TVar _), TSet _ -> true
                          | _ -> false
                        then adapt_value_to_type env storage_expected argument
                        else
                        match
                          specialize_dynamic_nominal_unpack expected
                            argument.semantic_expr
                        with
                        | Some expression -> Ok expression
                        | None ->
                          let row_type_name =
                            List.nth_opt row_param_types index |> Option.join
                          in
                          match (row_type_name, expected) with
                        | Some type_name, TNullable (TRecord fields) ->
                            let fields =
                              List.map
                                (fun (field : field) ->
                                  {
                                    field with
                                    ty =
                                      Type_inference_core.materialize_dynamic_unknown
                                        field.ty;
                                  })
                                fields
                            in
                            typed_nullable_row_argument env type_name fields
                              argument
                        | Some type_name, TRecord fields
                          when Types.is_dynamic argument.ty ->
                            dynamic_row_argument env type_name fields argument
                        | Some type_name, TRecord fields ->
                            typed_row_argument env type_name fields argument
                        | _,
                          (TNullable (TNamed_record record)
                          | TOcaml_app
                              ("option", [ TNamed_record record ]))
                          when Option.is_some
                                 (Types.record_fields argument.ty) ->
                            Result.map
                              (fun row ->
                                Semantic_ir.Constructor ("Some", Some row))
                              (typed_row_argument env
                                 (Structural_map.record_type_application record)
                                 record.fields argument)
                        | _
                          when (match
                                  ( optional_payload expected,
                                    optional_payload argument.ty )
                                with
                                | Some payload, None
                                  when not (Types.equal argument.ty TNil) ->
                                    argument_compatible payload argument.ty
                                | _ -> false) ->
                            let payload =
                              optional_payload expected |> Option.get
                            in
                            Result.map
                              (fun argument ->
                                Semantic_ir.Constructor
                                  ("Some", Some argument))
                              (adapt_value_to_type env payload argument)
                        | _ when has_capability_constraint expected ->
                          pack_constrained_value ?row_type_name env expected
                            argument
                        | _
                          when Option.is_none (optional_payload expected)
                               && not (expects_dynamic_value expected)
                               && not (expects_optional_dynamic_value expected)
                               && not (has_capability_constraint expected)
                               &&
                               (match argument.ty with
                               | TNullable actual
                               | TOcaml_app ("option", [ actual ]) ->
                                   argument_compatible expected actual
                               | _ -> false) ->
                            unwrap_optional_argument expected argument
                        | _ when expects_optional_dynamic_value expected ->
                          pack_optional_dynamic_argument env expected argument
                        | _ when
                          Types.is_dynamic expected
                          && not (Types.is_dynamic argument.ty)
                          -> pack_dynamic_value env expected argument
                        | _
                          when Option.is_some
                                 (Types.dynamic_map_types expected)
                               && Option.is_some
                                    (Types.dynamic_map_types argument.ty) ->
                            adapt_value_to_type env expected argument
                        | _, TVector _
                          when match argument.ty with
                               | TTuple _ -> true
                               | _ -> false ->
                            adapt_value_to_type env expected argument
                        | _ when
                          expects_dynamic_value expected
                          && has_capability_constraint argument.ty
                          -> Ok (constrained_argument_value argument)
                        | _ when
                          Types.is_dynamic argument.ty
                          && not (expects_dynamic_value expected)
                          -> dynamic_unpack env expected argument.semantic_expr
                        | _ ->
                            match (expected, argument.ty) with
                          | TFn (_, TBool), TFn (_, actual_return)
                            when expects_dynamic_value actual_return ->
                              Ok (adapt_truthy_callback expected argument)
                          | TFn (_, TNullable _), TFn (_, _) ->
                              adapt_nullable_callback env expected argument
                          | (TArray expected_item, TArray actual_item)
                          | (TList expected_item, TList actual_item)
                          | (TVector expected_item, TVector actual_item)
                          | (TSeq expected_item, TSeq actual_item)
                            when not (Types.equal expected_item actual_item) ->
                              adapt_value_to_type env expected argument
                          | ( TFn (expected_params, expected_return),
                              TFn (actual_params, actual_return) )
                            when (Types.is_dynamic expected_return
                                 || callback_parameters_need_adapter
                                      expected_params actual_params)
                                 && ((not (Types.is_dynamic actual_return))
                                 || callback_parameters_need_adapter
                                      expected_params actual_params) ->
                              adapt_dynamic_callback env expected argument
                          | TOcaml "int", TInt | TInt, TOcaml "int" ->
                              adapt_value_to_type env expected argument
                          | _ -> adapt_value_to_type env expected argument
                      in
                      let rec prepare_arguments index prepared expected arguments =
                        match (expected, arguments) with
                        | [], [] -> Ok (List.rev prepared)
                        | expected_ty :: expected_rest, argument :: arguments
                          -> (
                            match prepare_argument index expected_ty argument with
                            | Error error ->
                                Error
                                  {
                                    error with
                                    message =
                                      name ^ " argument "
                                      ^ string_of_int (index + 1)
                                      ^ ": " ^ error.message;
                                  }
                            | Ok argument ->
                                prepare_arguments (index + 1) (argument :: prepared)
                                  expected_rest arguments)
                        | _ ->
                            Error.error "internal overloaded argument mismatch"
                      in
                      match
                         prepare_arguments 0 [] fixed_param_tys fixed_args
                       with
                      | Error _ as error -> error
                      | Ok fixed_arguments -> (
                          let extra_arguments =
                            match arity.rest_param with
                            | None -> Ok []
                            | Some _ ->
                                let expected = Option.get rest_param_ty in
                                prepare_arguments fixed_count []
                                  (List.init (List.length extra_args) (fun _ ->
                                       expected))
                                  extra_args
                          in
                          match extra_arguments with
                          | Error _ as error -> error
                          | Ok extra_arguments ->
                              let arguments =
                                fixed_arguments
                                @
                                match arity.rest_param with
                                | None -> []
                                | Some _ ->
                                    [
                                      Semantic_ir.Apply
                                        ( Semantic_ir.Ident
                                            "Lg_runtime.Runtime_seq.of_list",
                                          [ Semantic_ir.List extra_arguments ]
                                        );
                                    ]
                              in
                              let target =
                                match
                                  List.nth_opt fn.overload_targets arity_index
                                with
                                | Some target -> Semantic_ir.Ident target
                                | None ->
                                    overloaded_projection
                                      (Semantic_ir.Ident fn.ocaml_name)
                                      arity_index
                              in
                              let call =
                                typed_ir storage_return_ty
                                  (Semantic_ir.Apply (target, arguments))
                              in
                              let adapted_call =
                                match
                                  ( Types.protocol_constraint_info arity.return_ty,
                                    Types.protocol_constraint_info return_ty )
                                with
                                | Some (left, _, _), Some (right, _, _)
                                  when Protocol_id.equal left right ->
                                    Ok call.semantic_expr
                                | _ ->
                                    adapt_value_to_type env return_ty call
                              in
                              Result.map
                                (fun expression ->
                                  let result = typed_ir return_ty expression in
                                  match
                                    Types.protocol_constraint_info return_ty
                                  with
                                  | Some (_, _, value_ty)
                                    when (match value_ty with
                                         | TUnknown | TMeta _ | TVar _ -> false
                                         | _ -> true) ->
                                      typed_ir value_ty
                                        (constrained_value_expression return_ty
                                           result.semantic_expr)
                                  | Some _ | None -> result)
                                adapted_call)))
            | TFn (param_tys, ret)
              when List.length param_tys = List.length args
                   && List.for_all2
                        (fun expected arg ->
                          if has_capability_constraint expected then true
                          else
                            match Types.seqable_constraint_element expected with
                            | Some _ ->
                                Collection_capability.accepts_seqable env arg.ty
                            | None -> argument_compatible expected arg.ty)
                        param_tys args -> (
                let storage_param_tys = param_tys in
                let actual_tys = List.map (fun argument -> argument.ty) args in
                let dynamic_callable =
                  List.exists2
                    (fun expected actual ->
                      Types.is_dynamic actual
                      && match expected with
                         | TFn _ | TOverloaded_fn _ -> true
                         | _ -> false)
                    param_tys actual_tys
                in
                let inferred_argument_compatible expected actual =
                  let known_scalar = function
                    | TInt | TFloat | TChar | TString | TRegex | TSymbol
                    | TKeyword | TBool | TUnit ->
                        true
                    | _ -> false
                  in
                  match
                    ( Types.seqable_constraint_info expected,
                      Collection_capability.element_type_of_ty env actual )
                  with
                  | Some (_, expected_element, _), Some actual_element ->
                      if callback_record_compatible env expected_element actual_element
                      then true
                      else if
                        known_scalar expected_element
                        && known_scalar actual_element
                      then
                        argument_compatible expected actual
                        && argument_compatible expected_element actual_element
                      else argument_compatible expected actual
                  | Some _, _ | None, _ ->
                      argument_compatible expected actual
                in
                let rec unify_argument ?(preserve_optional = false)
                    substitutions template actual =
                  let template, actual =
                    if preserve_optional then (template, actual)
                    else align_optional_inference template actual
                  in
                  match (template, actual) with
                  | ( TFn (template_params, template_return),
                      TFn (actual_params, actual_return) )
                    when List.length template_params = List.length actual_params
                    ->
                      let rec unify_callback_parameters substitutions templates
                          actuals =
                        match (templates, actuals) with
                        | [], [] ->
                            unify_argument ~preserve_optional:true substitutions
                              template_return actual_return
                        | template :: templates, actual :: actuals ->
                            let actual =
                              match template with
                              | TUnknown | TMeta _ | TVar _
                                when Option.is_some
                                       (Types.comparable_constraint_info actual)
                                ->
                                  Types.constraint_value_type actual
                              | _ -> actual
                            in
                            Result.bind
                              (unify_argument ~preserve_optional:true
                                 substitutions template actual)
                              (fun substitutions ->
                                unify_callback_parameters substitutions templates
                                  actuals)
                        | _ -> assert false
                      in
                      unify_callback_parameters substitutions template_params
                        actual_params
                  | _ -> (
                      match
                        ( Types.seqable_constraint_info template,
                          Types.seqable_constraint_info actual,
                          Collection_capability.element_type_of_ty env actual )
                      with
                      | ( Some (`Required, expected_element, expected_value),
                          None,
                          Some actual_element ) ->
                          (match
                             unify_argument ~preserve_optional:true substitutions
                               expected_element actual_element
                           with
                          | Error _ as error -> error
                          | Ok substitutions ->
                              Type_solver.unify substitutions expected_value actual)
                      | _ -> Type_solver.unify substitutions template actual)
                in
                let rec infer_arguments substitutions templates actuals =
                  match (templates, actuals) with
                  | [], [] -> Ok substitutions
                  | template :: templates, actual :: actuals ->
                      let evidence =
                        if Types.is_dynamic actual then
                          Types.dynamic_constraint_info actual
                          |> Option.value ~default:TUnknown
                        else actual
                      in
                      if Types.equal evidence TUnknown then
                        infer_arguments substitutions templates actuals
                      else
                        (match unify_argument substitutions template evidence with
                        | Ok substitutions ->
                            infer_arguments substitutions templates actuals
                        | Error _ when dynamic_callable ->
                            let substitutions =
                              List.fold_left
                                (fun substitutions variable ->
                                  Type_solver.force substitutions variable
                                    (Types.dynamic_constraint TUnknown))
                                substitutions (Type_solver.variables template)
                            in
                            infer_arguments substitutions templates actuals
                        | Error conflict ->
                            let expected =
                              Type_solver.apply substitutions template
                            in
                            if inferred_argument_compatible expected actual then
                              infer_arguments substitutions templates actuals
                            else Error conflict)
                  | _ -> assert false
                in
                match infer_arguments Type_solver.empty param_tys actual_tys with
                | Error conflict when Type_solver.conflict_is_occurs conflict ->
                    Error.error
                      (name
                     ^ ": polymorphic recursion requires an explicit signature")
                | Error _ ->
                    Error.error
                      (name ^ " called with incompatible arguments: expected ("
                     ^ String.concat ", "
                         (List.map Types.source_name param_tys)
                     ^ "), got ("
                     ^ String.concat ", "
                         (List.map Types.source_name actual_tys)
                     ^ ")")
                | Ok substitutions ->
                let collection_element =
                  args
                  |> List.filter_map
                       (Collection_capability.element_type env)
                  |> List.filter (fun ty ->
                         not
                           (Types.equal ty TUnknown || Types.is_dynamic ty
                           || match ty with
                              | TMeta _ | TVar _ -> true
                              | _ -> false))
                  |> function
                  | [ element ] -> Some element
                  | [] | _ :: _ :: _ -> None
                in
                let substitutions =
                  match collection_element with
                  | None -> substitutions
                  | Some receiver_ty ->
                      List.fold_left2
                        (fun substitutions expected argument ->
                          match
                            ( Type_solver.apply substitutions expected,
                              Type_solver.apply substitutions argument.ty )
                          with
                          | ( TFn (_, expected_return),
                              TFn (actual_params, actual_return) ) ->
                              let substitutions =
                                List.fold_left
                                  (fun substitutions actual_param ->
                                    match
                                      Types.protocol_constraint_info actual_param
                                    with
                                    | Some _ ->
                                        Protocol.infer_constraint_substitutions
                                          env substitutions actual_param
                                          receiver_ty
                                    | None -> substitutions)
                                  substitutions actual_params
                              in
                              Type_solver.unify substitutions expected_return
                                actual_return
                              |> Result.value ~default:substitutions
                          | _ -> substitutions)
                        substitutions param_tys args
                in
                let substitutions =
                  List.fold_left2
                    (fun substitutions expected argument ->
                      match Types.contains_constraint_info expected with
                      | Some (_, value_ty) -> (
                          match
                            ( Types.record_fields value_ty,
                              Types.record_fields argument.ty )
                          with
                          | Some expected_fields, Some actual_fields ->
                              List.fold_left
                                (fun substitutions
                                     (expected_field : field) ->
                                  match
                                    Types.find_field expected_field.keyword
                                      actual_fields
                                  with
                                  | None -> substitutions
                                  | Some actual_field ->
                                      let expected_ty =
                                        optional_payload expected_field.ty
                                        |> Option.value
                                             ~default:expected_field.ty
                                      in
                                      let actual_ty =
                                        optional_payload actual_field.ty
                                        |> Option.value
                                             ~default:actual_field.ty
                                      in
                                      Type_solver.unify substitutions expected_ty
                                        actual_ty
                                      |> Result.value ~default:substitutions)
                                substitutions expected_fields
                          | None, _ | _, None ->
                              Type_solver.unify substitutions value_ty
                                argument.ty
                              |> Result.value ~default:substitutions)
                      | None -> substitutions)
                    substitutions param_tys args
                in
                let substitutions =
                  List.fold_left2
                    (fun substitutions expected argument ->
                      match Types.protocol_constraint_info expected with
                      | Some (protocol_id, witness_ty, _) -> (
                          match
                            ( Types.protocol_witness_method_types witness_ty,
                              Protocol.witness_implementations env protocol_id
                                argument.ty )
                          with
                          | Some expected_methods, Some implementations
                            when List.length expected_methods
                                 = List.length implementations ->
                              List.fold_left2
                                (fun substitutions expected_method
                                     (implementation : binding) ->
                                  match
                                    (expected_method, implementation.ty)
                                  with
                                  | ( TFn (_, expected_return),
                                      TFn (actual_params, actual_return) ) -> (
                                      let substitutions =
                                        match
                                          ( actual_params,
                                            argument.ty )
                                        with
                                        | ( TNamed_record receiver
                                            :: _,
                                            TNamed_record actual )
                                          when Type_id.equal receiver.type_id
                                                 actual.type_id ->
                                            let substitutions =
                                              Type_solver.unify substitutions
                                                (TNamed_record receiver)
                                                (TNamed_record actual)
                                              |> Result.value
                                                   ~default:substitutions
                                            in
                                            List.fold_left
                                              (fun substitutions
                                                   (field : field) ->
                                                match
                                                  ( Types.find_field
                                                      field.keyword
                                                      actual.fields,
                                                    Types
                                                    .seqable_constraint_element
                                                      field.ty )
                                                with
                                                | ( Some actual_field,
                                                    Some expected_element ) -> (
                                                    match
                                                      Collection_capability
                                                      .element_type_of_ty env
                                                        actual_field.ty
                                                    with
                                                    | Some actual_element ->
                                                        Type_solver.unify
                                                          substitutions
                                                          expected_element
                                                          actual_element
                                                        |> Result.value
                                                             ~default:
                                                               substitutions
                                                    | None -> substitutions)
                                                | None, _ | _, None ->
                                                    substitutions)
                                              substitutions receiver.fields
                                        | _ -> substitutions
                                      in
                                      let actual_return =
                                        Type_solver.apply substitutions
                                          actual_return
                                      in
                                      let substitutions =
                                        Type_solver.unify substitutions
                                          expected_return actual_return
                                        |> Result.value
                                             ~default:substitutions
                                      in
                                      match
                                        ( Types.seqable_constraint_element
                                            expected_return,
                                          Collection_capability
                                          .element_type_of_ty env
                                            actual_return )
                                      with
                                      | Some expected_element, Some actual_element
                                        ->
                                          Type_solver.unify substitutions
                                            expected_element actual_element
                                          |> Result.value
                                               ~default:substitutions
                                      | None, _ | _, None -> substitutions)
                                  | ( TOverloaded_fn expected_arities,
                                      TOverloaded_fn actual_arities ) ->
                                      List.fold_left
                                        (fun substitutions expected_arity ->
                                          match
                                            List.find_opt
                                              (fun actual_arity ->
                                                List.length
                                                  actual_arity.fixed_params
                                                = List.length
                                                    expected_arity.fixed_params
                                                && Option.is_some
                                                     actual_arity.rest_param
                                                   = Option.is_some
                                                       expected_arity.rest_param)
                                              actual_arities
                                          with
                                          | None -> substitutions
                                          | Some actual_arity ->
                                              let expected_parameters =
                                                List.tl
                                                  expected_arity.fixed_params
                                              in
                                              let actual_parameters =
                                                List.tl actual_arity.fixed_params
                                              in
                                              let substitutions =
                                                Type_solver.unify_lists
                                                  substitutions
                                                  expected_parameters
                                                  actual_parameters
                                                |> Result.value
                                                     ~default:substitutions
                                              in
                                              let substitutions =
                                                match
                                                  ( expected_arity.rest_param,
                                                    actual_arity.rest_param )
                                                with
                                                | Some expected, Some actual ->
                                                    Type_solver.unify
                                                      substitutions expected
                                                      actual
                                                    |> Result.value
                                                         ~default:substitutions
                                                | None, None
                                                | Some _, None
                                                | None, Some _ ->
                                                    substitutions
                                              in
                                              Type_solver.unify substitutions
                                                expected_arity.return_ty
                                                actual_arity.return_ty
                                              |> Result.value
                                                   ~default:substitutions)
                                        substitutions expected_arities
                                  | _ -> substitutions)
                                substitutions expected_methods implementations
                          | Some _, Some _ | None, _ | _, None ->
                              substitutions)
                      | None -> substitutions)
                    substitutions param_tys args
                in
                let substitutions =
                  List.fold_left2
                    (fun substitutions expected argument ->
                      match Types.record_fields expected with
                      | Some fields -> (
                          match Types.find_record_extension_field fields with
                          | Some field
                            when Types.is_static_record_source_field field ->
                              Type_solver.unify substitutions field.ty
                                argument.ty
                              |> Result.value ~default:substitutions
                          | Some _ | None -> substitutions)
                      | None -> substitutions)
                    substitutions param_tys args
                in
                let substitutions =
                  List.fold_left2
                    (fun substitutions template argument ->
                      match
                        ( Types.seqable_constraint_element template,
                          Collection_capability.element_type env argument )
                      with
                      | Some (TVar variable), Some actual_element ->
                          Type_solver.force substitutions
                            (Type_solver.Declared variable)
                            actual_element
                      | Some (TMeta meta), Some actual_element ->
                          Type_solver.force substitutions
                            (Type_solver.Metavariable meta.id)
                            actual_element
                      | _ -> substitutions)
                    substitutions param_tys args
                in
                let substitutions =
                  match Env.expected_type env with
                  | Some expected
                    when contains_unresolved_type
                           (Type_solver.apply substitutions ret) ->
                      Type_solver.unify substitutions ret expected
                      |> Result.value ~default:substitutions
                  | Some _ | None -> substitutions
                in
                let instantiate ty =
                  Type_solver.apply substitutions ty
                in
                let materialize ty =
                  let ty = instantiate ty in
                  if dynamic_callable then materialize_protocol_unknown ty
                  else ty
                in
                let raw_storage_param_tys = storage_param_tys in
                let storage_param_tys =
                  List.map instantiate storage_param_tys
                in
                let param_tys = List.map materialize param_tys in
                let callback_element_candidates =
                  List.fold_left2
                    (fun candidates expected argument ->
                      match (expected, argument.ty) with
                      | TFn (expected_params, _), TFn (actual_params, _)
                        when List.length expected_params
                             = List.length actual_params ->
                          List.fold_left2
                            (fun candidates expected actual ->
                              let candidate =
                                match expected with
                                | TUnknown | TMeta _ | TVar _ -> actual
                                | expected -> expected
                              in
                              if
                                List.exists (Types.equal candidate) candidates
                              then candidates
                              else candidate :: candidates)
                            candidates expected_params actual_params
                      | _ -> candidates)
                    [] param_tys args
                in
                let specialize_seqable_element expected argument =
                  match expected with
                  | TOcaml_app (name, [ element_ty; value_ty ])
                    when name = Types.seqable_constraint_name
                         || name = Types.optional_seqable_constraint_name
                         || name = Types.optional_sequential_constraint_name ->
                      let element_ty =
                        match element_ty with
                        | TUnknown | TMeta _ | TVar _ ->
                            let actual_element =
                              Collection_capability.element_type env argument
                            in
                            (match actual_element with
                            | Some actual
                              when List.exists
                                     (Types.equal actual)
                                     callback_element_candidates ->
                                actual
                            | _
                              when List.exists
                                     Types.is_dynamic
                                     callback_element_candidates ->
                                Types.dynamic_constraint TUnknown
                            | _ -> (
                                match callback_element_candidates with
                                | [ candidate ] -> candidate
                                | _ ->
                                    Option.value actual_element
                                      ~default:
                                        (Types.dynamic_constraint TUnknown)))
                        | element_ty -> element_ty
                      in
                      let value_ty =
                        match value_ty with
                        | TUnknown | TMeta _ | TVar _ -> (
                            match Types.seqable_constraint_info argument.ty with
                            | Some (_, _, value_ty) -> value_ty
                            | None -> argument.ty)
                        | value_ty -> value_ty
                      in
                      TOcaml_app (name, [ element_ty; value_ty ])
                  | expected -> expected
                in
                let param_tys =
                  List.map2 specialize_seqable_element param_tys args
                in
                let param_tys =
                  List.map2
                    (fun expected argument ->
                      match (expected, argument.ty) with
                      | ( TFn (expected_params, expected_return),
                          TFn (actual_params, _) )
                        when List.length expected_params
                             = List.length actual_params ->
                          let parameter_tys =
                            List.map2
                              (fun expected actual ->
                                match expected with
                                | TUnknown | TMeta _ | TVar _ -> actual
                                | expected -> expected)
                              expected_params actual_params
                          in
                          TFn (parameter_tys, expected_return)
                      | _ -> expected)
                    param_tys args
                in
                let param_tys =
                  List.map2
                    (fun storage_ty instantiated_ty ->
                      match storage_ty with
                      | TSet (TUnknown | TMeta _ | TVar _) -> storage_ty
                      | _ when has_capability_constraint storage_ty ->
                          storage_ty
                      | _ -> instantiated_ty)
                    raw_storage_param_tys param_tys
                in
                let storage_ret_template =
                  Types.maybe_reduced_callback_element ret
                  |> Option.value ~default:ret
                in
                let erased_callback_storage_call =
                  List.exists2
                    (fun expected argument ->
                      (Types.is_dynamic argument.ty
                      || has_capability_constraint argument.ty)
                      && (expects_dynamic_value expected
                         || has_capability_constraint expected))
                    param_tys args
                  ||
                  (List.exists Types.is_dynamic storage_param_tys
                  && List.exists
                       (function
                         | TFn (parameters, return_ty) ->
                             uses_dynamic_value_storage return_ty
                             && List.exists
                               (fun ty ->
                                 Types.is_dynamic ty
                                 || Types.equal ty TUnknown
                                 || match ty with TMeta _ | TVar _ -> true | _ -> false)
                               parameters
                         | _ -> false)
                       storage_param_tys)
                in
                let erased_storage_call =
                  List.exists
                    (fun parameter_ty ->
                      Types.is_dynamic parameter_ty
                      || has_capability_constraint parameter_ty)
                    param_tys
                in
                let runtime_dynamic_call =
                  List.exists
                    (fun argument ->
                      uses_dynamic_value_storage argument.ty)
                    args
                in
                let ret = materialize ret in
                let open_element_candidates =
                  List.fold_left2
                    (fun candidates expected argument ->
                      let expected_element =
                        match expected with
                        | TArray ty | TList ty | TVector ty | TSet ty | TSeq ty ->
                            Some ty
                        | _ -> None
                      in
                      match
                        ( expected_element,
                          Collection_capability.element_type env argument )
                      with
                      | Some (TUnknown | TMeta _ | TVar _), Some actual
                        when not
                               (Types.equal actual TUnknown
                               || Types.is_dynamic actual
                               || match actual with
                                  | TMeta _ | TVar _ -> true
                                  | _ -> false)
                        ->
                          if List.exists (Types.equal actual) candidates then
                            candidates
                          else actual :: candidates
                      | _ -> candidates)
                    [] storage_param_tys args
                in
                let ret =
                  match open_element_candidates with
                  | [ element_ty ] ->
                      let rec specialize = function
                        | TUnknown | TMeta _ | TVar _ -> element_ty
                        | TNullable ty -> TNullable (specialize ty)
                        | TArray ty -> TArray (specialize ty)
                        | TRef ty -> TRef (specialize ty)
                        | TList ty -> TList (specialize ty)
                        | TVector ty -> TVector (specialize ty)
                        | TSet ty -> TSet (specialize ty)
                        | TSeq ty -> TSeq (specialize ty)
                        | TOcaml_app (name, arguments) ->
                            TOcaml_app (name, List.map specialize arguments)
                        | TTuple items -> TTuple (List.map specialize items)
                        | ty -> ty
                      in
                      specialize ret
                  | [] | _ :: _ :: _ -> ret
                in
                let ret =
                  match (ret, Env.expected_type env) with
                  | (TUnknown | TMeta _ | TVar _), Some expected -> expected
                  | _ -> ret
                in
                let storage_sequence_elements =
                  List.filter_map Types.seqable_constraint_element
                    storage_param_tys
                  |> List.map Type_inference_core.materialize_dynamic_unknown
                in
                  let rec compile_arg_exprs index acc = function
                    | [] -> Ok (List.rev acc)
                    | arg :: rest -> (
                      let expected_ty = List.nth param_tys index in
                      let expected_ty =
                        match
                          ( List.nth_opt storage_param_tys index,
                            arg.ty )
                        with
                        | Some (TVector storage_item as storage_ty),
                          TVector actual_item
                          when (is_optional_type storage_item
                               && not (is_optional_type actual_item))
                               || (Types.is_dynamic storage_item
                                  && not (Types.is_dynamic actual_item))
                               || (Types.equal storage_item TUnknown
                                  && not (Types.is_dynamic actual_item))
                               || ((match storage_item with
                                   | TMeta _ | TVar _ -> true
                                   | _ -> false)
                                  && not (Types.is_dynamic actual_item)) ->
                            (match storage_item with
                            | TUnknown ->
                                TVector (Types.dynamic_constraint TUnknown)
                            | TMeta _ | TVar _ -> expected_ty
                            | _ -> storage_ty)
                        | Some (TArray storage_item as storage_ty),
                          TArray actual_item
                          when (is_optional_type storage_item
                               && not (is_optional_type actual_item))
                               || (Types.is_dynamic storage_item
                                  && not (Types.is_dynamic actual_item)) ->
                            storage_ty
                        | Some _, _ | None, _ -> expected_ty
                      in
                      let callback_expected_ty =
                        match
                          ( expected_ty,
                            List.nth_opt storage_param_tys index,
                            arg.ty )
                        with
                        | ( TFn (_, _),
                            Some (TFn (storage_params, storage_return)),
                            TFn (actual_params, _) )
                          when List.length storage_params
                               = List.length actual_params
                               && List.exists2
                                    (fun storage actual ->
                                      (match storage with
                                      | TUnknown | TMeta _ | TVar _ -> true
                                      | _ -> false)
                                      && has_capability_constraint actual)
                                    storage_params actual_params ->
                            TFn
                              ( List.map2
                                  (fun storage actual ->
                                    match storage with
                                    | TUnknown | TMeta _ | TVar _ ->
                                        Types.constraint_value_type actual
                                    | storage -> storage)
                                  storage_params actual_params,
                                storage_return )
                        | ( TFn (expected_params, expected_return),
                            storage_ty,
                            _ )
                          when erased_callback_storage_call ->
                            let storage_params, storage_return =
                              match storage_ty with
                              | Some (TFn (storage_params, storage_return)) ->
                                  ( List.map
                                      Type_inference_core.materialize_dynamic_unknown
                                      storage_params,
                                    Type_inference_core.materialize_dynamic_unknown
                                      storage_return )
                              | Some _ | None ->
                                  ( List.map
                                      Type_inference_core.materialize_dynamic_unknown
                                      expected_params,
                                    expected_return )
                            in
                            let storage_params =
                              if
                                storage_sequence_elements <> []
                                && List.length storage_sequence_elements
                                   = List.length storage_params
                              then storage_sequence_elements
                              else storage_params
                            in
                            TFn (storage_params, storage_return)
                        | _ -> expected_ty
                      in
                      let row_type_name =
                        List.nth_opt fn.row_param_types index |> Option.join
                      in
                      let row_type_name =
                        match row_type_name with
                        | Some _ as row_type_name -> row_type_name
                        | None -> (
                            match
                              row_param_fields ~allow_nullable:true expected_ty
                            with
                            | Some fields ->
                                let owner =
                                  Source_context.anonymous_record_owner ""
                                in
                                (match
                                   Env.find_anonymous_record ~owner fields env
                                 with
                                | Some _ as record -> record
                                | None -> (
                                    match
                                      Env.find_anonymous_record_by_layout
                                        ~owner fields env
                                    with
                                    | Some _ as record -> record
                            | None -> None))
                                |> Option.map
                                     Structural_map.record_type_application
                            | None -> None)
                      in
                      if has_capability_constraint expected_ty then
                        match
                          pack_constrained_value ?row_type_name env expected_ty
                            arg
                        with
                          | Error _ as err -> err
                          | Ok expression ->
                              compile_arg_exprs (index + 1) (expression :: acc)
                                rest
                      else
                          let expression =
                            match (row_type_name, expected_ty) with
                            | _, TNamed_record _
                              when same_concrete_type expected_ty arg.ty
                                   || named_record_can_specialize expected_ty
                                        arg.ty ->
                                Ok arg.semantic_expr
                            | Some type_name, TNullable (TRecord fields) ->
                                let fields =
                                  List.map
                                    (fun (field : field) ->
                                      {
                                        field with
                                        ty =
                                          Type_inference_core
                                          .materialize_dynamic_unknown field.ty;
                                      })
                                    fields
                                in
                                typed_nullable_row_argument env type_name fields
                                  arg
                            | Some type_name, TRecord fields
                              when Types.is_dynamic arg.ty ->
                                dynamic_row_argument env type_name fields arg
                            | Some type_name, TRecord fields ->
                                typed_row_argument env type_name fields arg
                            | _,
                              (TNullable (TNamed_record record)
                              | TOcaml_app
                                  ("option", [ TNamed_record record ]))
                              when Option.is_some (Types.record_fields arg.ty) ->
                                Result.map
                                  (fun row ->
                                    Semantic_ir.Constructor ("Some", Some row))
                                  (typed_row_argument env
                                     (Structural_map.record_type_application
                                        record)
                                     record.fields arg)
                            | _
                              when (match
                                      ( optional_payload expected_ty,
                                        optional_payload arg.ty )
                                    with
                                    | Some payload, None
                                      when not (Types.equal arg.ty TNil) ->
                                        argument_compatible payload arg.ty
                                    | _ -> false) ->
                                let payload =
                                  optional_payload expected_ty |> Option.get
                                in
                                Result.map
                                  (fun argument ->
                                    Semantic_ir.Constructor
                                      ("Some", Some argument))
                                  (adapt_value_to_type env payload arg)
                            | _, TNamed_record record
                              when not record.nominal
                                   && Option.is_some
                                        (Types.record_fields arg.ty) ->
                                typed_row_argument env
                                  (Structural_map.record_type_application record)
                                  record.fields arg
                            | _
                              when match arg.ty with
                                   | TNullable actual
                                   | TOcaml_app ("option", [ actual ]) ->
                                       argument_compatible expected_ty actual
                                   | _ -> false ->
                                adapt_value_to_type env expected_ty arg
                            | _
                              when uses_dynamic_value_storage arg.ty
                                   && not
                                        (expects_dynamic_value expected_ty) ->
                                dynamic_unpack env expected_ty
                                  (constrained_argument_value arg)
                            | _, TNamed_record record
                              when Types.is_dynamic arg.ty
                                 ||
                                 match arg.ty with
                                      | TUnknown | TMeta _ | TVar _ -> true
                                 | _ -> false ->
                                dynamic_unpack env (TNamed_record record)
                                  arg.semantic_expr
                          | _ when expects_optional_dynamic_value expected_ty ->
                              pack_optional_dynamic_argument env expected_ty arg
                            | _
                              when Types.is_dynamic expected_ty
                                   && not (Types.is_dynamic arg.ty) ->
                                pack_dynamic_value env expected_ty arg
                            | _
                              when Types.is_dynamic arg.ty
                                   && not (expects_dynamic_value expected_ty) ->
                              dynamic_unpack env expected_ty arg.semantic_expr
                            | _, TOcaml "int" when Types.equal arg.ty TInt ->
                                adapt_value_to_type env expected_ty arg
                            | _, TInt
                              when Types.equal arg.ty (TOcaml "int") ->
                                adapt_value_to_type env expected_ty arg
                            | _
                              when function_has_host_int_return_boundary
                                     expected_ty arg.ty ->
                                adapt_value_to_type env expected_ty arg
                            | _, TSeq expected_item
                              when match arg.ty with
                                   | TSeq actual_item ->
                                       not
                                         (Types.equal expected_item actual_item)
                                   | _ -> false ->
                              adapt_value_to_type env expected_ty arg
                            | _, TArray expected_item
                              when match arg.ty with
                                   | TArray actual_item ->
                                       not
                                         (Types.equal expected_item actual_item)
                                   | _ -> false ->
                              adapt_value_to_type env expected_ty arg
                            | _, TVector expected_item
                              when match arg.ty with
                                   | TVector actual_item ->
                                       not
                                         (Types.equal expected_item actual_item)
                                   | _ -> false ->
                              adapt_value_to_type env expected_ty arg
                            | _
                              when Option.is_some
                                     (Types.dynamic_map_types expected_ty)
                                   && Option.is_some
                                        (Types.dynamic_map_types arg.ty) ->
                                adapt_value_to_type env expected_ty arg
                            | _, TVector _
                              when match arg.ty with
                                   | TTuple _ -> true
                                   | _ -> false ->
                              adapt_value_to_type env expected_ty arg
                            | _
                              when match (expected_ty, arg.ty) with
                                   | TSet expected_element, TSet actual_element ->
                                       not
                                         (same_set_storage_representation
                                            expected_element actual_element)
                                   | _ -> false ->
                              adapt_value_to_type env expected_ty arg
                            | _, TSet (TUnknown | TMeta _ | TVar _)
                              when match arg.ty with
                                   | TSet _ -> true
                                   | _ -> false ->
                              adapt_value_to_type env expected_ty arg
                            | _ -> (
                                match
                                  maybe_reduced_callback_payload expected_ty
                                    arg.ty
                                with
                                | Some _ -> Ok (adapt_reduced_callback arg)
                                | None -> (
                                    match (callback_expected_ty, arg.ty) with
                                    | ( TFn (_, expected_return),
                                        TFn (_, _) )
                                      when has_capability_constraint
                                             expected_return
                                           || function_has_host_int_return_boundary
                                                callback_expected_ty arg.ty ->
                                      adapt_value_to_type env
                                        callback_expected_ty arg
                                    | TFn (_, TBool), TFn (_, actual_return)
                                      when expects_dynamic_value actual_return ->
                                      Ok
                                        (adapt_truthy_callback
                                           callback_expected_ty arg)
                                  | TFn (_, TNullable _), TFn (_, _) ->
                                      adapt_nullable_callback env
                                        callback_expected_ty arg
                                  | TFn _, TOverloaded_fn _ ->
                                      adapt_overloaded_callback env
                                        callback_expected_ty arg
                                  | TFn ([ _ ], _), map_ty
                                    when Option.is_some
                                           (Types.dynamic_map_types map_ty) ->
                                      adapt_value_to_type env expected_ty arg
                                  | ( TFn (expected_params, expected_return),
                                      TFn (actual_params, actual_return) )
                                      when (Types.is_dynamic expected_return
                                           || callback_parameters_need_adapter
                                                expected_params actual_params)
                                         && ((not
                                                (Types.is_dynamic actual_return))
                                           || callback_parameters_need_adapter
                                                 expected_params actual_params)
                                    ->
                                      adapt_dynamic_callback env
                                        callback_expected_ty arg
                                  | _
                                    when (match expected_ty with
                                          | TUnknown | TMeta _ | TVar _ -> true
                                          | _ -> false)
                                         && Types.equal arg.ty TNil ->
                                      (* A nil passed to an unconstrained
                                         parameter: the callee body may pin
                                         the parameter to the dynamic
                                         representation (e.g. by packing it
                                         into a dynamic collection), so use
                                         the dynamic nil representation. *)
                                      pack_dynamic_value env
                                        (Types.dynamic_constraint TUnknown)
                                        arg
                                  | _ -> (
                                        if
                                          expects_dynamic_value expected_ty
                                          && has_capability_constraint arg.ty
                                      then Ok (constrained_argument_value arg)
                                        else
                                          match
                                            (expected_ty, arg.record_values)
                                          with
                                          | TMap_keys, Some values ->
                                              Ok
                                                (Semantic_ir.Apply
                                                   ( Semantic_ir.Ident
                                                       "Lg_runtime.Core_set.String_set.of_list",
                                                   [
                                                     Semantic_ir.List
                                                         (List.map
                                                          (fun ( (field : field),
                                                                _ ) ->
                                                              Semantic_ir.String
                                                                field.keyword)
                                                            values);
                                                     ] ))
                                          | expected_ty, Some values -> (
                                              match
                                                Types.dynamic_map_types
                                                  expected_ty
                                              with
                                              | Some (key_ty, value_ty) ->
                                                  adapt_record_values_to_map
                                                    env key_ty value_ty values
                                              | None ->
                                                  Ok
                                                    (row_arg_expr
                                                       row_type_name
                                                       expected_ty arg))
                                          | _ ->
                                              Ok
                                                (row_arg_expr row_type_name
                                                 expected_ty arg))))
                          in
                        match expression with
                          | Error _ as error -> error
                          | Ok expression ->
                            compile_arg_exprs (index + 1) (expression :: acc)
                              rest)
                in
                match compile_arg_exprs 0 [] args with
                | Error _ as err -> err
                | Ok arg_exprs -> (
                let ret = materialize ret in
                let callback_return =
                  args
                  |> List.find_map (fun argument ->
                         match argument.ty with
                         | TFn (_, TNullable return_ty) -> Some return_ty
                         | TFn (_, return_ty)
                           when not (Types.equal return_ty TUnknown) ->
                             Some return_ty
                         | _ -> None)
                in
                let ret =
                  match (ret, callback_return) with
                      | TNullable (TVector (TUnknown | TMeta _ | TVar _)), Some return_ty
                        ->
                      TNullable (TVector return_ty)
                  | TVector (TUnknown | TMeta _ | TVar _), Some return_ty ->
                      TVector return_ty
                  | _ -> ret
                in
                let ret =
                  match (fn.return_param_index, ret) with
                  (* A declared generic map return, including a set of maps,
                     may add or rename keys. The input record's closed field
                     shape is therefore not evidence for the result shape. *)
                  | ( Some _,
                      TOcaml_app
                        ("Lg_runtime.Runtime_map.t", [ _key_ty; _value_ty ]) )
                  | ( Some _,
                      TSet
                        (TOcaml_app
                          ("Lg_runtime.Runtime_map.t", [ _key_ty; _value_ty ])) )
                    ->
                      ret
                  | Some index, _ -> (
                      match
                        ( List.nth_opt param_tys index,
                          List.nth_opt args index )
                      with
                      | ( Some parameter_ty,
                          Some
                            {
                              ty =
                                (TNullable actual_ty
                                | TOcaml_app ("option", [ actual_ty ]));
                              _;
                            } )
                        when Option.is_none (optional_payload parameter_ty)
                             && argument_compatible parameter_ty actual_ty ->
                          Types.constraint_value_type actual_ty
                      | _, Some arg -> Types.constraint_value_type arg.ty
                      | _, None -> ret)
                  | _ -> ret
                in
                let ret =
                  match ret with
                  | TNullable (TUnknown | TMeta _ | TVar _) ->
                      param_tys
                      |> List.mapi (fun index param_ty -> (index, param_ty))
                      |> List.find_map (fun (index, param_ty) ->
                             match
                               Types.seqable_constraint_element param_ty
                             with
                             | None -> None
                             | Some _ -> (
                                 match List.nth_opt args index with
                                 | None -> None
                                 | Some arg ->
                                     Collection_capability.element_type env arg))
                      |> Option.map (fun element_ty -> TNullable element_ty)
                      |> Option.value ~default:ret
                  | TSeq TUnknown ->
                      param_tys
                      |> List.mapi (fun index param_ty -> (index, param_ty))
                      |> List.find_map (fun (index, param_ty) ->
                              match
                                Types.seqable_constraint_element param_ty
                              with
                             | None -> None
                             | Some _ -> (
                                 match List.nth_opt args index with
                                 | None -> None
                                 | Some arg ->
                                      Collection_capability.element_type env arg
                                  ))
                      |> Option.map (fun element_ty -> TSeq element_ty)
                      |> Option.value ~default:ret
                  | TOcaml_app (name, [ TUnknown ]) as ret
                    when name = Types.next_seq_type_name ->
                      param_tys
                      |> List.mapi (fun index param_ty -> (index, param_ty))
                      |> List.find_map (fun (index, param_ty) ->
                              match
                                Types.seqable_constraint_element param_ty
                              with
                             | None -> None
                             | Some _ -> (
                                 match List.nth_opt args index with
                                 | None -> None
                                 | Some arg ->
                                      Collection_capability.element_type env arg
                                  ))
                      |> Option.map Types.next_seq
                      |> Option.value ~default:ret
                  | _ -> ret
                in
                let ret =
                  Types.maybe_reduced_callback_element ret
                  |> Option.value ~default:ret
                in
                let sequence_storage_follows_adapter =
                  match storage_ret_template with
                  | TSeq (TUnknown | TMeta _ | TVar _)
                  | TNullable (TUnknown | TMeta _ | TVar _) ->
                      List.exists
                        (fun parameter_ty ->
                          Option.is_some
                            (Types.seqable_constraint_element parameter_ty))
                        param_tys
                  | _ -> false
                in
                let storage_ret =
                  if sequence_storage_follows_adapter then ret
                  else
                    match storage_ret_template with
                    | TSet (TUnknown | TMeta _ | TVar _) ->
                        storage_ret_template
                    | _ ->
                        if
                          Option.is_some fn.return_param_index
                          && Types.is_dynamic storage_ret_template
                        then storage_ret_template
                        else if
                          erased_storage_call
                          && runtime_dynamic_call
                          && Option.is_none fn.return_param_index
                        then
                          Type_inference_core.materialize_dynamic_unknown
                            storage_ret_template
                        else ret
                in
                let call =
                      Semantic_ir.Apply
                        (Semantic_ir.Ident fn.ocaml_name, arg_exprs)
                in
                let same_storage_representation =
                  match (storage_ret, ret) with
                  | TSet storage_element, TSet result_element ->
                      same_set_storage_representation storage_element
                        result_element
                  | _ -> Types.equal storage_ret ret
                in
                let call =
                  if
                    same_storage_representation
                    || contains_unresolved_type ret
                  then Ok call
                  else adapt_value_to_type env ret (typed_ir storage_ret call)
                in
                let reduced_payload =
                  List.combine param_tys args
                  |> List.find_map (fun (expected, actual) ->
                         maybe_reduced_callback_payload expected actual.ty)
                in
                    match call with
                | Error _ as error -> error
                    | Ok call -> (
                        match reduced_payload with
                | None -> Ok (typed_ir ret call)
                | Some payload_ty
                          when Types.assignable ~policy:Host_boundary
                                 ~expected:ret ~actual:payload_ty
                       || Types.equal ret TUnknown ->
                    let caught_result = "__lg_reduced_result" in
                    let callback_exception =
                      Semantic_ir.Constructor
                        ( "Lg_runtime.Runtime_reduced.Callback_reduced",
                          None )
                    in
                    let handler =
                      Semantic_ir.Match
                        ( Semantic_ir.Prefix
                                    ( "!",
                                      Semantic_ir.Ident reduced_callback_state
                                    ),
                                  [
                                    ( Semantic_ir.PConstructor
                                        ( "Some",
                                          Some (Semantic_ir.PVar caught_result)
                                        ),
                                      Semantic_ir.Ident caught_result );
                            ( Semantic_ir.PConstructor ("None", None),
                              Semantic_ir.Apply
                                        ( Semantic_ir.Ident "raise",
                                          [ callback_exception ] ) );
                          ] )
                    in
                    Ok
                      (typed_ir (Types.reduced ret)
                         (Semantic_ir.Let
                                    ( [
                                        ( Semantic_ir.PVar reduced_callback_state,
                                  Semantic_ir.Apply
                                    ( Semantic_ir.Ident "ref",
                                              [
                                                Semantic_ir.Constructor
                                                  ("None", None);
                                              ] ) );
                                      ],
                              Semantic_ir.Try
                                ( Semantic_ir.Apply
                                    ( Semantic_ir.Ident
                                        "Lg_runtime.Runtime_reduced.continue",
                                      [ call ] ),
                                          [
                                            ( Semantic_ir.PConstructor
                                        ( "Lg_runtime.Runtime_reduced.Callback_reduced",
                                          None ),
                                      None,
                                      handler );
                                  ] ) )))
                | Some _ ->
                    Error.error
                              "reduced callback value must match the function \
                               result")))
            | ty when Types.is_dynamic ty ->
                Error.error
                  ("call target must have a static function or callable \
                    collection type; define a typed wrapper or closed sum type; \
                    got "
                  ^ Types.source_name ty ^ " for " ^ name)
            | _ when Option.is_some callable ->
                let callable = Option.get callable in
                let receiver_expression =
                  if callable.callable_optional then
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Option.get",
                        [ Semantic_ir.Ident fn.ocaml_name ] )
                  else Semantic_ir.Ident fn.ocaml_name
                in
                let receiver =
                  typed_ir (TNamed_record callable.callable_record)
                    receiver_expression
                in
                let invoke_args = receiver :: args in
                (match callable.callable_implementation.ty with
                | TFn (parameter_tys, return_ty)
                  when List.length parameter_tys = List.length invoke_args ->
                    let rec prepare expressions actual_tys expected actual =
                      match (expected, actual) with
                      | [], [] ->
                          Ok (List.rev expressions, List.rev actual_tys)
                      | expected_ty :: expected, argument :: actual ->
                          let prepared =
                            if
                              Types.is_dynamic expected_ty
                              || Types.equal expected_ty TUnknown
                              || match expected_ty with
                                 | TMeta _ | TVar _ -> true
                                 | _ -> false
                            then
                              Result.map
                                (fun expression ->
                                  ( expression,
                                    Types.dynamic_constraint TUnknown ))
                                (pack_dynamic_value env
                                   (Types.dynamic_constraint TUnknown)
                                   argument)
                            else if Types.is_dynamic argument.ty then
                              Result.map
                                (fun expression -> (expression, expected_ty))
                                (dynamic_unpack env expected_ty
                                   argument.semantic_expr)
                            else
                              Result.map
                                (fun expression -> (expression, expected_ty))
                                (adapt_value_to_type env expected_ty argument)
                          in
                          Result.bind prepared (fun (expression, actual_ty) ->
                              prepare (expression :: expressions)
                                (actual_ty :: actual_tys) expected actual)
                      | _ ->
                          Error.error
                            (name ^ " called with incompatible arguments")
                    in
                    Result.map
                      (fun (arguments, actual_tys) ->
                        let return_ty =
                          Types.instantiate_type ~templates:parameter_tys
                            ~actuals:actual_tys return_ty
                        in
                        typed_ir return_ty
                          (Semantic_ir.Apply
                             ( Semantic_ir.Ident
                                 callable.callable_implementation.ocaml_name,
                               arguments )))
                      (prepare [] [] parameter_tys invoke_args)
                | _ ->
                    Error.error
                      (name ^ " called with incompatible arguments"))
            | ty when Option.is_some (Types.dynamic_map_types ty) -> (
                match args with
                | [ _ ] | [ _; _ ] ->
                    compile_static_get scope env (FSymbol name :: arg_forms)
                | _ ->
                    Error.error
                      (name ^ " expects a key and optional default"))
            | TSet element_ty -> (
                match args with
                | [ arg ]
                  when Types.same_shape element_ty arg.ty
                       || Types.is_dynamic arg.ty
                       || Types.is_dynamic element_ty
                       || Option.fold ~none:false
                            ~some:(same_static_set_representation element_ty)
                            (optional_payload arg.ty) ->
                    Result.bind
                      (Types.set_module_name element_ty)
                      (fun set_module ->
                        let lookup argument =
                          let present =
                            Semantic_ir.Apply
                              ( Semantic_ir.Ident (set_module ^ ".mem"),
                                [
                                  argument;
                                  Semantic_ir.Ident fn.ocaml_name;
                                ] )
                          in
                          Semantic_ir.If
                            ( present,
                              Semantic_ir.Constructor
                                ("Some", Some argument),
                              Semantic_ir.Constructor ("None", None) )
                        in
                        let expression =
                          match optional_payload arg.ty with
                          | Some actual_inner
                            when same_static_set_representation element_ty
                                   actual_inner ->
                              let value_name = "__lg_optional_set_lookup" in
                              let argument =
                                coerce_expression_to_type element_ty actual_inner
                                  (Semantic_ir.Ident value_name)
                              in
                              Ok
                                (Semantic_ir.Match
                                   ( arg.semantic_expr,
                                     [
                                       ( Semantic_ir.PConstructor
                                           ("None", None),
                                         Semantic_ir.Constructor
                                           ("None", None) );
                                       ( Semantic_ir.PConstructor
                                           ( "Some",
                                             Some
                                               (Semantic_ir.PVar value_name) ),
                                         lookup argument );
                                     ] ))
                          | Some _ | None ->
                              Result.map lookup
                                (adapt_value_to_type env element_ty arg)
                        in
                        Result.map
                          (fun expression ->
                            typed_ir
                              (TOcaml_app ("option", [ element_ty ]))
                              expression)
                          expression)
                | [ _ ] ->
                    Error.error (name ^ " called with incompatible arguments")
                | _ -> Error.error (name ^ " expects 1 arguments"))
            | TFn (parameter_tys, _) ->
                Error.error
                  (name ^ " called with incompatible arguments: expected ("
                 ^ String.concat ", " (List.map Types.source_name parameter_tys)
                 ^ "), got ("
                 ^ String.concat ", "
                     (List.map
                        (fun argument -> Types.source_name argument.ty)
                        args)
                 ^ ")")
            | _ -> Error.error (name ^ " is not callable")))
  and compile_protocol_call scope env name arg_forms =
    let contextual_return_ty ty =
      match (ty, Env.expected_type env) with
      | TUnknown, Some expected when not (has_capability_constraint expected) ->
          expected
      | ty, Some expected
        when contains_unresolved_type ty
             && not (has_capability_constraint expected) ->
          let template =
            match (optional_payload ty, optional_payload expected) with
            | Some payload, None -> payload
            | _ -> ty
          in
          Type_solver.unify Type_solver.empty template expected
          |> Result.map (fun substitutions -> Type_solver.apply substitutions ty)
          |> Result.value ~default:ty
      | _ -> ty
    in
    if Protocol.method_is_ambiguous scope env name then
      Error.error ("ambiguous protocol method " ^ name ^ "; use Protocol/method")
    else
      match Protocol.lookup_marker scope env name with
      | None -> Error.error ("unknown function " ^ name)
      | Some marker
        when Option.is_none
               (select_binding_arity marker (List.length arg_forms)) ->
          Error.error
            (name ^ " called with unsupported protocol method arity "
           ^ string_of_int (List.length arg_forms))
      | Some marker -> (
          let marker =
            Option.get (select_binding_arity marker (List.length arg_forms))
          in
          let argument_env = Env.with_expected_type None env in
          let expected_params =
            match marker.ty with
            | TFn (param_tys, _) when List.length param_tys = List.length arg_forms
              ->
                param_tys
            | _ -> List.map (fun _ -> TUnknown) arg_forms
          in
          let rec compile_protocol_args compiled expected_params forms =
            match (expected_params, forms) with
            | [], [] -> Ok (List.rev compiled)
            | expected_ty :: expected_rest, form :: form_rest ->
                let contextual_env =
                  match expected_ty with
                  | TFn _ ->
                      Env.with_expected_type (Some expected_ty) argument_env
                  | _ -> argument_env
                in
                Result.bind
                  (compile_expr scope contextual_env form)
                  (fun argument ->
                    compile_protocol_args (argument :: compiled) expected_rest
                      form_rest)
            | _ -> Error.error (name ^ " called with incompatible arguments")
          in
          match compile_protocol_args [] expected_params arg_forms with
          | Error _ as err -> err
          | Ok args -> (
              let normalized_args =
                match args with
                | receiver :: rest -> (
                    match receiver.ty with
                    | TNullable inner | TOcaml_app ("option", [ inner ]) ->
                        let optional_statically_satisfies =
                          match marker.protocol_id with
                          | Some protocol_id ->
                              Protocol.type_satisfies env protocol_id receiver.ty
                          | None -> false
                        in
                        if optional_statically_satisfies then Ok args
                        else
                        let statically_satisfies =
                          Option.is_some
                            (Types.protocol_constraint_info inner)
                          ||
                          match marker.protocol_id with
                          | Some protocol_id ->
                              Protocol.type_satisfies env protocol_id inner
                          | None -> false
                        in
                        let receiver_ty =
                          if statically_satisfies then inner
                          else Types.dynamic_constraint inner
                        in
                        let value =
                          typed_ir inner
                            (Semantic_ir.Apply
                               ( Semantic_ir.Ident "Option.get",
                                 [ receiver.semantic_expr ] ))
                        in
                        if statically_satisfies then
                          Ok (typed_ir receiver_ty value.semantic_expr :: rest)
                        else
                          Result.map
                            (fun semantic_expr ->
                              typed_ir receiver_ty semantic_expr :: rest)
                            (pack_dynamic_value env receiver_ty value)
                    | _ -> Ok args)
                | [] -> Ok []
              in
              Result.bind normalized_args (fun args ->
              let args =
                match (args, marker.protocol_id) with
                | receiver :: rest, Some protocol_id
                  when Option.is_some
                         (Types.protocol_constraint_info receiver.ty)
                       && not
                            (has_protocol_constraint protocol_id receiver.ty)
                  ->
                    {
                      receiver with
                      ty = Types.constraint_value_type receiver.ty;
                      semantic_expr = constrained_argument_value receiver;
                    }
                    :: rest
                | _ -> args
              in
              match marker.ty with
              | TFn (param_tys, _ret)
                when List.length param_tys <> List.length args ->
                  Error.error (name ^ " called with incompatible arguments")
              | TFn (_, _) -> (
                  match args with
                  | [] ->
                      Error.error (name ^ " called with incompatible arguments")
                  | receiver :: _ -> (
                      let method_name = Protocol.method_basename name in
                      match receiver.ty with
                      | receiver_ty when Types.is_dynamic receiver_ty ->
                          Error.error
                            ("protocol method " ^ name
                           ^ " requires a statically typed receiver")
                      | receiver_ty
                        when Option.is_some
                               (Types.protocol_constraint_info receiver_ty) -> (
                          match
                            ( marker.protocol_id,
                              Protocol.method_position env marker method_name )
                          with
                          | Some protocol_id, Some position
                            when has_protocol_constraint protocol_id receiver_ty
                            -> (
                              let higher_order_recursive_traversal =
                                List.tl args
                                |> List.exists (fun argument ->
                                       match argument.ty with
                                       | TFn (parameters, _) ->
                                           List.exists
                                             (has_protocol_constraint
                                                protocol_id)
                                             parameters
                                       | _ -> false)
                              in
                              if higher_order_recursive_traversal then
                                Error.error
                                  "higher-order protocol traversal requires a closed sum type"
                              else
                              match
                                protocol_witness_expression protocol_id receiver
                              with
                              | None ->
                                  Error.error
                                    ("protocol-constrained receiver for " ^ name
                                   ^ " must be a function parameter")
                              | Some witness ->
                                  let methods_name = "__lg_protocol_methods" in
                                  let methods =
                                    Semantic_ir.Ident methods_name
                                  in
                                  let method_expr =
                                    witness_method methods position
                                  in
                                  let return_param_index =
                                    Protocol.common_method_return_param_index env
                                      protocol_id method_name
                                  in
                                  let return_ty =
                                    match (return_param_index, marker.ty) with
                                    | Some index, TFn (_, declared_return_ty)
                                      when contains_unresolved_type
                                             declared_return_ty -> (
                                        match List.nth_opt args index with
                                        | Some argument -> argument.ty
                                        | None ->
                                            Types.dynamic_constraint TUnknown)
                                    | _,
                                      TFn
                                        ( parameters,
                                          ((TUnknown | TMeta _ | TVar _) as declared) ) ->
                                        let inferred =
                                          Protocol.common_method_return_for_arity
                                            env protocol_id method_name
                                            {
                                              fixed_params = parameters;
                                              rest_param = None;
                                              return_ty = TUnknown;
                                            }
                                        in
                                        inferred
                                        |> Option.value
                                             ~default:
                                               (match declared with
                                               | TMeta _ | TVar _ -> declared
                                               | TUnknown ->
                                                   Types.dynamic_constraint
                                                     TUnknown
                                               | _ -> assert false)
                                    | _, TFn (_, return_ty) -> return_ty
                                    | _ -> TUnknown
                                  in
                                  let witness_method_ty =
                                    Option.bind
                                      (protocol_constraint_witness protocol_id
                                         receiver_ty)
                                      (fun witness_ty ->
                                        Option.bind
                                          (Types.protocol_witness_method_types
                                             witness_ty)
                                          (fun methods ->
                                            List.nth_opt methods position))
                                  in
                                  let method_expr, witness_method_ty =
                                    match witness_method_ty with
                                    | Some (TOverloaded_fn arities) -> (
                                        match
                                          select_overloaded_arity arities
                                            (List.length args)
                                        with
                                        | Some (index, arity) ->
                                            ( overloaded_projection method_expr
                                                index,
                                              Some
                                                (TFn
                                                   ( arity.fixed_params,
                                                     arity.return_ty )) )
                                        | None -> (method_expr, witness_method_ty))
                                    | Some _ | None ->
                                        (method_expr, witness_method_ty)
                                  in
                                  let witness_return_ty =
                                    match marker.ty with
                                    | TFn (_, declared_return_ty)
                                      when not
                                             (contains_unresolved_type
                                                declared_return_ty) ->
                                        declared_return_ty
                                    | _ -> (
                                        match witness_method_ty with
                                        | Some (TFn (_, return_ty)) -> return_ty
                                        | _ -> return_ty)
                                  in
                                  let return_ty =
                                    match witness_method_ty with
                                    | Some (TFn (_, witness_return_ty))
                                      when Option.is_none return_param_index
                                           && contains_unresolved_type return_ty
                                      ->
                                        witness_return_ty
                                    | _ -> return_ty
                                  in
                                  let return_ty =
                                    contextual_return_ty return_ty
                                  in
                                  let param_tys =
                                    match marker.ty with
                                    | TFn (param_tys, _) -> param_tys
                                    | _ -> []
                                  in
                                  let witness_param_tys =
                                    match witness_method_ty with
                                    | Some (TFn (params, _))
                                      when List.length params
                                           = List.length param_tys ->
                                        params
                                    | _ -> param_tys
                                  in
                                  let rec prepare_arguments prepared expected
                                      actual =
                                    match (expected, actual) with
                                    | [], [] -> Ok (List.rev prepared)
                                    | expected_ty :: expected,
                                      argument :: actual ->
                                        let expected_ty =
                                          match expected_ty with
                                          | TUnknown | TMeta _ | TVar _ ->
                                              argument.ty
                                          | ty -> ty
                                        in
                                        let adapted =
                                          if
                                            has_capability_constraint
                                              expected_ty
                                          then
                                            pack_constrained_value env
                                              expected_ty argument
                                          else if Types.is_dynamic expected_ty
                                          then
                                            pack_dynamic_value env expected_ty
                                              argument
                                          else
                                            adapt_value_to_type env expected_ty
                                              argument
                                        in
                                        Result.bind adapted (fun adapted ->
                                            prepare_arguments
                                              (adapted :: prepared) expected
                                              actual)
                                    | _ ->
                                        Error.error
                                          "protocol witness argument arity mismatch"
                                  in
                                  let receiver_argument =
                                    receiver.semantic_expr
                                  in
                                  Result.bind
                                    (prepare_arguments []
                                       (List.tl witness_param_tys)
                                       (List.tl args))
                                    (fun arguments ->
                                      let result =
                                        Semantic_ir.Match
                                          ( witness,
                                            [
                                              ( Semantic_ir.PConstructor
                                                  ("None", None),
                                                Semantic_ir.Apply
                                                  ( Semantic_ir.Ident
                                                      "invalid_arg",
                                                    [
                                                      Semantic_ir.String
                                                        ("missing protocol \
                                                          implementation for "
                                                       ^ name);
                                                    ] ) );
                                              ( Semantic_ir.PConstructor
                                                  ( "Some",
                                                    Some
                                                      (Semantic_ir.PVar
                                                         methods_name) ),
                                                Semantic_ir.Apply
                                                  ( method_expr,
                                                    receiver_argument
                                                    :: arguments ) );
                                            ] )
                                      in
                                      adapt_protocol_witness_result env
                                        ~expected:return_ty
                                        ~actual:witness_return_ty result))
                          | _ ->
                              Error.error
                                ("no protocol implementation for " ^ name
                               ^ " and " ^ source_name receiver.ty))
                      | TOcaml_app ("Lg_runtime.Runtime_reify.t", [ payload_ty ])
                        -> (
                          match
                            Protocol.method_position env marker method_name
                          with
                          | None ->
                              Error.error
                                ("unknown protocol method " ^ method_name)
                          | Some position ->
                              let payload =
                                Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Runtime_reify.payload",
                                    [ receiver.semantic_expr ] )
                              in
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
                                      ( payload,
                                        [
                                          ( Semantic_ir.PTuple patterns,
                                            Semantic_ir.Ident binding_name );
                                        ] )
                                | _ -> payload
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
                                  when List.length params = List.length args - 1
                                  ->
                                    List.tl args
                                | _ -> args
                              in
                              let return_ty =
                                match method_ty with
                                | TFn (_, (TUnknown | TMeta _ | TVar _)) ->
                                    contextual_return_ty TUnknown
                                | TFn (_, return_ty) -> return_ty
                                | _ -> TUnknown
                              in
                              let expected_params =
                                match method_ty with
                                | TFn (params, _)
                                  when List.length params
                                       = List.length call_args ->
                                    params
                                | _ -> List.map (fun arg -> arg.ty) call_args
                              in
                              let rec prepare prepared expected arguments =
                                match (expected, arguments) with
                                | [], [] -> Ok (List.rev prepared)
                                | ( expected :: expected_rest,
                                    argument :: argument_rest ) ->
                                    let expression =
                                      if
                                        Types.is_dynamic expected
                                        && not (Types.is_dynamic argument.ty)
                                      then
                                        pack_dynamic_value env expected argument
                                      else if
                                        Types.is_dynamic argument.ty
                                        && not (expects_dynamic_value expected)
                                      then
                                        dynamic_unpack env expected
                                          argument.semantic_expr
                                      else
                                        match (expected, argument.ty) with
                                        | TFn _, TFn _ ->
                                            adapt_dynamic_callback env expected
                                              argument
                                        | _ -> Ok argument.semantic_expr
                                    in
                                    Result.bind expression (fun expression ->
                                        prepare (expression :: prepared)
                                          expected_rest argument_rest)
                                | _ ->
                                    Error.error
                                      "protocol method argument count mismatch"
                              in
                              Result.map
                                (fun arguments ->
                                  typed_ir return_ty
                                    (Semantic_ir.Apply (method_expr, arguments)))
                                (prepare [] expected_params call_args))
                      | _ -> (
                          let typed_primitive =
                            match marker.protocol_id with
                            | Some protocol_id
                              when Protocol_id.equal protocol_id
                                     Core_protocols.seqable_id
                                   && method_name = "-seq" -> (
                                match
                                  Collection_capability.seq_expr env receiver
                                with
                                | Ok result -> Some result
                                | Error _ -> None)
                            | Some _ | None -> None
                          in
                          match typed_primitive with
                          | Some result -> Ok result
                          | None -> (
                          match
                            Protocol.lookup_marker_impl env marker method_name
                              receiver.ty
                          with
                          | None ->
                              Error.error
                                ("no protocol implementation for " ^ name
                               ^ " and " ^ source_name receiver.ty)
                          | Some impl -> (
                              let impl =
                                select_binding_arity impl (List.length args)
                                |> Option.value ~default:impl
                              in
                              let impl_ty =
                                match impl.ty with
                                | TFn (param_tys, return_ty)
                                  when List.length param_tys = List.length args
                                  ->
                                    List.fold_left2
                                      (fun substitutions expected argument ->
                                        Result.bind substitutions
                                          (fun substitutions ->
                                            Type_solver.unify substitutions
                                              expected argument.ty))
                                      (Ok Type_solver.empty) param_tys args
                                    |> Result.map (fun substitutions ->
                                           Type_solver.apply substitutions
                                             (TFn (param_tys, return_ty)))
                                    |> Result.value ~default:impl.ty
                                | _ -> impl.ty
                              in
                              match impl_ty with
                              | TFn (param_tys, ret)
                                when List.length param_tys = List.length args
                                     && List.for_all2
                                          (fun expected arg ->
                                            Types.assignable
                                              ~policy:Host_boundary ~expected
                                              ~actual:arg.ty)
                                          param_tys args ->
                                  let ret = contextual_return_ty ret in
                                  let prepare_argument expected argument =
                                    if has_capability_constraint expected then
                                      pack_constrained_value env expected
                                        argument
                                    else if Types.is_dynamic expected then
                                      pack_dynamic_value env expected argument
                                    else if
                                      Types.is_dynamic argument.ty
                                      && not (expects_dynamic_value expected)
                                    then
                                      dynamic_unpack env expected
                                        argument.semantic_expr
                                    else
                                      match optional_payload expected with
                                      | Some payload
                                        when (method_name = "-reset!"
                                             || method_name = "-vreset!")
                                             && Types.assignable
                                               ~policy:Host_boundary
                                               ~expected:payload
                                               ~actual:argument.ty ->
                                          adapt_value_to_type env expected
                                            argument
                                      | Some _ | None ->
                                          Ok argument.semantic_expr
                                  in
                                  let rec prepare prepared expected arguments =
                                    match (expected, arguments) with
                                    | [], [] -> Ok (List.rev prepared)
                                    | ( expected :: expected_rest,
                                        argument :: argument_rest ) ->
                                        Result.bind
                                          (prepare_argument expected argument)
                                          (fun argument ->
                                            prepare (argument :: prepared)
                                              expected_rest argument_rest)
                                    | _ ->
                                        Error.error
                                          "protocol method argument count \
                                           mismatch"
                                  in
                                  Result.bind (prepare [] param_tys args)
                                    (fun arguments ->
                                      let static_set_operation =
                                        match impl.protocol_id with
                                        | Some protocol_id
                                          when Protocol_id.equal protocol_id
                                                 Core_protocols.emptyable_id
                                               && method_name = "-empty" ->
                                            Some `Empty
                                        | Some protocol_id
                                          when Protocol_id.equal protocol_id
                                                 Core_protocols.set_id
                                               && method_name = "-disjoin" ->
                                            Some `Remove
                                        | Some protocol_id
                                          when Protocol_id.equal protocol_id
                                                 Core_protocols.collection_id
                                               && method_name = "-conj" ->
                                            Some `Add
                                        | Some protocol_id
                                          when Protocol_id.equal protocol_id
                                                 Core_protocols.editable_id
                                               && method_name = "-as-transient"
                                          ->
                                            Some `To_transient
                                        | Some protocol_id
                                          when Protocol_id.equal protocol_id
                                                 Core_protocols
                                                 .transient_collection_id
                                               && method_name = "-persistent!"
                                          ->
                                            Some `To_persistent
                                        | Some _ | None -> None
                                      in
                                      match
                                        ( receiver.ty,
                                          arguments,
                                          static_set_operation )
                                      with
                                      | ( TSet element_ty,
                                          [ collection ],
                                          Some `Empty ) ->
                                          Result.map
                                            (fun set_module ->
                                              typed_ir ret
                                                (Semantic_ir.Sequence
                                                   [
                                                     collection;
                                                     Semantic_ir.Ident
                                                       (set_module ^ ".empty");
                                                   ]))
                                            (Types.set_module_name element_ty)
                                      | ( TSet element_ty,
                                          [ collection; value ],
                                          Some (`Remove | `Add as operation) ) ->
                                          Result.map
                                            (fun set_module ->
                                              typed_ir ret
                                                (Semantic_ir.Apply
                                                   ( Semantic_ir.Ident
                                                       (set_module ^ "."
                                                      ^ (match operation with
                                                        | `Remove -> "remove"
                                                        | `Add -> "add")),
                                                     [ value; collection ] )))
                                            (Types.set_module_name element_ty)
                                      | ( TSet element_ty,
                                          [ collection ],
                                          Some `To_transient ) ->
                                          Result.map
                                            (fun set_module ->
                                              typed_ir ret
                                                (Semantic_ir.Apply
                                                   ( Semantic_ir.Ident
                                                       "Lg_runtime.Runtime_transient.set_of_list",
                                                     [
                                                       Semantic_ir.Apply
                                                         ( Semantic_ir.Ident
                                                             (set_module
                                                            ^ ".elements"),
                                                           [ collection ] );
                                                     ] )))
                                            (Types.set_module_name element_ty)
                                      | ( TOcaml_app
                                            ( "Lg_runtime.Runtime_transient.set",
                                              [ element_ty ] ),
                                          [ collection ],
                                          Some `To_persistent ) ->
                                          Result.map
                                            (fun set_module ->
                                              typed_ir ret
                                                (Semantic_ir.Apply
                                                   ( Semantic_ir.Ident
                                                       (set_module ^ ".of_seq"),
                                                     [
                                                       Semantic_ir.Apply
                                                         ( Semantic_ir.Ident
                                                             "Lg_runtime.Runtime_transient.set_to_seq",
                                                           [ collection ] );
                                                     ] )))
                                            (Types.set_module_name element_ty)
                                      | _ ->
                                          Ok
                                            (typed_ir ret
                                               (Semantic_ir.Apply
                                                  ( Semantic_ir.Ident
                                                      impl.ocaml_name,
                                                    arguments ))))
                              | TFn _ ->
                                  Error.error
                                    (name
                                   ^ " called with incompatible arguments: \
                                      expected ("
                                   ^ String.concat ", "
                                       (List.map Types.source_name
                                          (match impl_ty with
                                          | TFn (parameters, _) -> parameters
                                          | _ -> []))
                                   ^ "), got ("
                                   ^ String.concat ", "
                                       (List.map
                                          (fun argument ->
                                            Types.source_name argument.ty)
                                          args)
                                   ^ ")")
                              | _ -> Error.error (name ^ " is not callable"))))))
              | _ -> Error.error (name ^ " is not callable"))))
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
