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

let dynamic_packer_counter = ref 0

let java_exception_constructors =
  [
    "Exception.";
    "IllegalArgumentException.";
    "IndexOutOfBoundsException.";
    "UnsupportedOperationException.";
    "js/Error.";
  ]

let is_java_exception_constructor name =
  List.mem name java_exception_constructors

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
      Types.equal left TUnknown || Types.equal right TUnknown
      || Types.assignable ~policy:Host_boundary ~expected:left ~actual:right
      || Types.assignable ~policy:Host_boundary ~expected:right ~actual:left
  | _ -> false

let typed_item_pattern name = function
  | TNamed_record record ->
      Semantic_ir.PConstraint
        ( Semantic_ir.PVar name,
          record_type_application record.type_name record.type_arguments )
  | _ -> Semantic_ir.PVar name

let int_parameter_type = function
  | TInt | TUnknown | TVar _ -> true
  | _ -> false

let weak_referenceable_type = function
  | TRecord _ | TNamed_record _ | TArray _ | TRef _ | TVector _ | TSet _
  | TFn _ | TUnknown | TVar _ ->
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
         Types.assignable ~policy:Host_boundary ~expected ~actual)
       expected actual

let expects_dynamic_value = function
  | TUnknown | TVar _ -> true
  | ty -> Types.is_dynamic ty

let expects_optional_dynamic_value = function
  | TNullable inner | TOcaml_app ("option", [ inner ]) -> Types.is_dynamic inner
  | _ -> false

let rec contains_dynamic_type = function
  | ty when Types.is_dynamic ty -> true
  | TNullable inner | TArray inner | TRef inner | TList inner | TVector inner
  | TSet inner | TSeq inner ->
      contains_dynamic_type inner
  | TOcaml_app (_, arguments) | TTuple arguments ->
      List.exists contains_dynamic_type arguments
  | TFn (parameters, return_ty) ->
      List.exists contains_dynamic_type (return_ty :: parameters)
  | TOverloaded_fn arities ->
      List.exists
        (fun (arity : fn_arity) ->
          List.exists contains_dynamic_type
            (arity.return_ty :: arity.fixed_params)
          || Option.fold ~none:false ~some:contains_dynamic_type arity.rest_param)
        arities
  | TRecord fields | TNamed_record { fields; _ } ->
      List.exists (fun (field : field) -> contains_dynamic_type field.ty) fields
  | _ -> false

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
  | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
  | TNullable ty -> TNullable (materialize_protocol_unknown ty)
  | TFn (parameters, return_ty) ->
      TFn
        ( List.map materialize_protocol_unknown parameters,
          materialize_protocol_unknown return_ty )
  | ty -> ty

let supports_structural_dynamic_packing =
  Types.supports_structural_dynamic_packing

let rec concrete_nominal_type_argument = function
  | TUnknown | TVar _ -> false
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

let cast_nominal_payload expression =
  Semantic_ir.Apply (Semantic_ir.Ident "Obj.magic", [ expression ])

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
             conversion =
               Semantic_ir.continue_inside_conversion conversion
                 cast_nominal_payload;
           })
  | _ -> None

let rec contains_unresolved_type = function
  | ty when Types.is_dynamic ty -> false
  | TUnknown | TVar _ -> true
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

let supports_dynamic_field_projection ty =
  if Types.is_dynamic ty then true
  else if contains_unresolved_type ty then false
  else
    match ty with
    | TNamed_record _ -> true
    | _ -> supports_structural_dynamic_packing ty

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

let rec argument_compatible expected actual =
  if Types.is_dynamic expected then true
  else if Option.is_some (Types.protocol_constraint_info expected) then true
  else if Option.is_some (Types.seqable_constraint_info expected) then
    match (Types.seqable_constraint_info expected, actual) with
    | ( Some ((`Optional | `Optional_sequential), _, _),
        (TNullable actual | TOcaml_app ("option", [ actual ])) ) ->
        argument_compatible expected actual
    | _, (TList _ | TVector _ | TSet _ | TSeq _ | TArray _ | TString) ->
        true
    | _, ty when Types.is_dynamic ty -> true
    | _, _ -> Option.is_some (Types.seqable_constraint_info actual)
  else
    match (expected, actual) with
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
    | _ when Types.assignable ~policy:Host_boundary ~expected ~actual -> true
    | TFn (expected_params, expected_return), TFn (actual_params, actual_return)
      when callback_parameters_compatible expected_params actual_params -> (
        if
          Types.equal expected_return TBool
          && expects_dynamic_value actual_return
        then true
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
    | _ -> false

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
  String.starts_with ~prefix:"__lg_dynamic_callback_arg_" name
  || String.starts_with ~prefix:"__lg_nullable_callback_arg_" name
  || String.starts_with ~prefix:"__lg_static_argument_" name
  || String.starts_with ~prefix:"__lg_dynamic_protocol_arg_" name
  || String.starts_with ~prefix:"__lg_dynamic_optional_value" name
  || String.starts_with ~prefix:"__lg_branch_optional_item" name
  || String.starts_with ~prefix:"__lg_branch_optional_payload" name
  || String.starts_with ~prefix:"__lg_erased_seqable_item" name
  || String.starts_with ~prefix:"__lg_constrained_argument" name

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

let has_capability_constraint ty =
  Types.is_dynamic ty
  || Option.is_some (Types.protocol_constraint_info ty)
  ||
  match ty with
  | TOcaml_app (name, [ _; _ ]) ->
      name = Types.seqable_constraint_name
      || name = Types.optional_seqable_constraint_name
      || name = Types.optional_sequential_constraint_name
  | _ -> false

let uses_dynamic_value_storage ty =
  let value_ty = Types.constraint_value_type ty in
  Types.is_dynamic value_ty
  ||
  match value_ty with
  | TUnknown | TVar _ -> has_capability_constraint ty
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
            | _ -> Semantic_ir.Ident name))
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
            | _ -> Semantic_ir.PVar name))
  in
  build ty

let constrained_argument_expression argument =
  match Semantic_ir.unlocated argument.semantic_expr with
  | Semantic_ir.Ident name when is_generated_callback_argument name ->
      argument.semantic_expr
  | Semantic_ir.Ident name -> constrained_identifier_expression name argument.ty
  | _ -> argument.semantic_expr

let rec constrained_value_expression ty expression =
  match Types.protocol_constraint_info ty with
  | Some (_, _, value_ty) ->
      constrained_value_expression value_ty
        (Semantic_ir.Apply (Semantic_ir.Ident "snd", [ expression ]))
  | None -> (
      match ty with
      | TOcaml_app (constraint_name, [ _element_ty; value_ty ])
        when constraint_name = Types.seqable_constraint_name
             || constraint_name = Types.optional_seqable_constraint_name
             || constraint_name = Types.optional_sequential_constraint_name ->
          constrained_value_expression value_ty
            (Semantic_ir.Apply (Semantic_ir.Ident "snd", [ expression ]))
      | _ -> expression)

let constrained_argument_value argument =
  match Semantic_ir.unlocated argument.semantic_expr with
  | Semantic_ir.Ident name when is_generated_callback_argument name ->
      constrained_value_expression argument.ty argument.semantic_expr
  | Semantic_ir.Ident _ -> argument.semantic_expr
  | _ -> constrained_value_expression argument.ty argument.semantic_expr

let is_sequential_type = function
  | TList _ | TVector _ | TSeq _ -> true
  | ty -> (
      match Types.seqable_constraint_info ty with
      | Some (`Optional_sequential, _, _) -> true
      | Some ((`Required | `Optional), _, _) | None ->
          Option.is_some (Types.next_seq_element ty))

let rec dynamic_protocol_constraints ty =
  match Types.dynamic_constraint_info ty with
  | Some capability -> dynamic_protocol_constraints capability
  | None -> (
      match Types.protocol_constraint_info ty with
      | Some (protocol_id, _, value_ty) ->
          protocol_id :: dynamic_protocol_constraints value_ty
      | None -> (
          match ty with
          | TOcaml_app (name, [ _element_ty; value_ty ])
            when name = Types.seqable_constraint_name
                 || name = Types.optional_seqable_constraint_name
                 || name = Types.optional_sequential_constraint_name ->
              dynamic_protocol_constraints value_ty
          | _ -> []))

let resolve_named_record_application env = function
  | TOcaml_app (name, arguments) as ty ->
      let records =
        Env.filter_map
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
          let substitutions = List.combine record.type_parameters arguments in
          Type_solver.apply substitutions (TNamed_record record)
      | [] | _ :: _ :: _ -> ty)
  | ty -> ty

let rec dynamic_unpack env ty expression =
  let ty = resolve_named_record_application env ty in
  dynamic_unpack_impl env ty expression
  |> Result.map (fun conversion ->
         Semantic_ir.UnpackDynamic
           {
             source_ty = Types.dynamic_constraint TUnknown;
             target_ty = ty;
             conversion;
           })

and dynamic_unpack_impl env ty expression =
  let scalar_function =
    match ty with
    | TInt | TOcaml "int" -> Some "Lg_runtime.Runtime_dynamic.as_int"
    | TFloat | TOcaml "float" -> Some "Lg_runtime.Runtime_dynamic.as_float"
    | TChar | TOcaml "char" -> Some "Lg_runtime.Runtime_dynamic.as_char"
    | TString | TOcaml "string" -> Some "Lg_runtime.Runtime_dynamic.as_string"
    | TSymbol -> Some "Lg_runtime.Runtime_dynamic.as_symbol"
    | TKeyword -> Some "Lg_runtime.Runtime_dynamic.as_keyword"
    | TBool | TOcaml "bool" -> Some "Lg_runtime.Runtime_dynamic.as_bool"
    | TOcaml "Lg_runtime.Runtime_uuid.t" ->
        Some "Lg_runtime.Runtime_dynamic.as_uuid"
    | _ -> None
  in
  let unpack_record fields type_name =
    let known_keys =
      fields
      |> List.filter (fun field ->
             not (Types.is_record_extension_field field))
      |> List.map (fun field ->
             Semantic_ir.Apply
               ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.keyword",
                 [ Semantic_ir.String field.keyword ] ))
    in
    let rec unpack_fields values = function
      | [] -> Ok (List.rev values)
      | (field : field) :: fields ->
          let dynamic_value =
            if Types.is_record_extension_field field then
              Semantic_ir.Apply
                ( Semantic_ir.Ident
                    "Lg_runtime.Runtime_dynamic.map_without_keys",
                  [ expression; Semantic_ir.List known_keys ] )
            else
              Semantic_ir.Apply
                ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.get",
                  [
                    expression;
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident
                          "Lg_runtime.Runtime_dynamic.keyword",
                        [ Semantic_ir.String field.keyword ] );
                  ] )
          in
          (match dynamic_unpack env field.ty dynamic_value with
          | Error (error : Error.t) ->
              Error
                {
                  error with
                  message =
                    error.message ^ " while recovering record field "
                    ^ field.keyword;
                }
          | Ok value ->
              unpack_fields ((field.ocaml_name, value) :: values) fields)
    in
    Result.map
      (fun fields -> Semantic_ir.Record (fields, type_name))
      (unpack_fields [] fields)
  in
  match scalar_function with
  | Some function_name ->
      Ok (Semantic_ir.Apply (Semantic_ir.Ident function_name, [ expression ]))
  | None -> (
      match ty with
      | TUnknown | TVar _ -> Ok expression
      | ty when Types.is_dynamic ty -> Ok expression
      | TNullable inner | TOcaml_app ("option", [ inner ]) ->
          dynamic_unpack env inner expression
          |> Result.map (fun unpacked ->
                 Semantic_ir.If
                   ( Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.is_nil",
                         [ expression ] ),
                     Semantic_ir.Constructor ("None", None),
                     Semantic_ir.Constructor ("Some", Some unpacked) ))
      | TFn (parameter_tys, return_ty) ->
          let parameter_names =
            List.mapi
              (fun index _ -> "__lg_static_argument_" ^ string_of_int index)
              parameter_tys
          in
          let rec pack_arguments packed tys names =
            match (tys, names) with
            | [], [] -> Ok (List.rev packed)
            | ty :: tys, name :: names ->
                let argument = typed_ir ty (Semantic_ir.Ident name) in
                let packed_argument =
                  if has_capability_constraint ty then
                    pack_dynamic_value env
                      (Types.dynamic_constraint ty)
                      argument
                  else
                    pack_dynamic_payload env
                      (Types.dynamic_constraint ty)
                      argument
                in
                Result.bind packed_argument (fun packed_argument ->
                    pack_arguments (packed_argument :: packed) tys names)
            | _ -> Error.error "dynamic function arity mismatch"
          in
          Result.bind (pack_arguments [] parameter_tys parameter_names)
            (fun arguments ->
                 let dynamic_result =
                   Semantic_ir.Apply
                     ( Semantic_ir.Ident
                         "Lg_runtime.Runtime_dynamic.invoke_function",
                       [ expression; Semantic_ir.List arguments ] )
                 in
                 dynamic_unpack env return_ty dynamic_result
                 |> Result.map (fun body ->
                        Semantic_ir.Fun
                          (List.map
                             (fun name -> Semantic_ir.PVar name)
                             parameter_names,
                           body)))
      | ty when Option.is_some (Types.protocol_constraint_info ty) -> (
          match Types.protocol_constraint_info ty with
          | None -> assert false
          | Some (protocol_id, witness_ty, value_ty) -> (
              match
                Protocol_registry.find_protocol protocol_id
                  (Compiler_environment.protocols env)
              with
              | None ->
                  Error.error
                    ("unknown protocol " ^ Protocol_id.to_string protocol_id)
              | Some declaration -> (
                  let declared_methods =
                    declaration.Protocol_registry.methods
                    |> Protocol_registry.Method_map.bindings
                  in
                  let method_types =
                    Types.protocol_witness_method_types witness_ty
                  in
                  let methods =
                    match method_types with
                    | Some method_types
                      when List.length method_types
                           = List.length declared_methods ->
                        Ok (List.combine declared_methods method_types)
                    | Some _ | None ->
                        Error.error "invalid protocol witness type"
                  in
                  let compile_method ((method_id, signature), method_ty) =
                    let param_tys, return_ty =
                      match method_ty with
                      | TFn (param_tys, return_ty) ->
                          (param_tys, return_ty)
                      | _ ->
                          ( signature.Protocol_registry.param_tys,
                            signature.Protocol_registry.return_ty )
                    in
                    let return_ty = materialize_protocol_unknown return_ty in
                    let parameter_names =
                      List.mapi
                        (fun index _ ->
                          "__lg_dynamic_protocol_arg_" ^ string_of_int index)
                        param_tys
                    in
                    let invocation_parameters =
                      match
                        (param_tys, parameter_names)
                      with
                      | ( _receiver_ty :: param_tys,
                          _receiver_name :: parameter_names ) ->
                          Ok (param_tys, parameter_names)
                      | _ ->
                          Error.error
                            "protocol methods require a receiver parameter"
                    in
                    let rec pack_arguments packed tys names =
                      match (tys, names) with
                      | [], [] -> Ok (List.rev packed)
                      | ty :: tys, name :: names ->
                          let argument = typed_ir ty (Semantic_ir.Ident name) in
                          Result.bind
                            (pack_dynamic_value env
                               (Types.dynamic_constraint ty)
                               argument)
                            (fun packed_argument ->
                              pack_arguments
                                (packed_argument :: packed)
                                tys names)
                      | _ -> Error.error "protocol method arity mismatch"
                    in
                    Result.bind invocation_parameters
                      (fun (invocation_param_tys, invocation_parameter_names) ->
                        Result.bind
                          (pack_arguments [] invocation_param_tys
                             invocation_parameter_names) (fun arguments ->
                        let dynamic_result =
                          Semantic_ir.Apply
                            ( Semantic_ir.Ident
                                "Lg_runtime.Runtime_dynamic.invoke",
                                  [
                                    expression;
                                Semantic_ir.String
                                  (Protocol_id.to_string protocol_id);
                                    Semantic_ir.String
                                      (Method_id.name method_id);
                                Semantic_ir.List arguments;
                              ] )
                        in
                        Result.map
                          (fun result ->
                            Semantic_ir.Fun
                              ( List.map
                                  (fun name -> Semantic_ir.PVar name)
                                  parameter_names,
                                result ))
                              (dynamic_unpack env return_ty dynamic_result)))
                  in
                  let rec compile_methods compiled = function
                    | [] -> Ok (List.rev compiled)
                    | method_ :: methods ->
                        Result.bind (compile_method method_)
                          (fun compiled_method ->
                            compile_methods
                              (compiled_method :: compiled)
                              methods)
                  in
                  match
                     ( Result.bind methods (compile_methods []),
                       dynamic_unpack env value_ty expression )
                   with
                  | (Error _ as error), _ -> error
                  | _, (Error _ as error) -> error
                  | Ok methods, Ok value ->
                      let witness =
                        Semantic_ir.If
                          ( Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_dynamic.has_protocol",
                                [
                                  expression;
                                  Semantic_ir.String
                                    (Protocol_id.to_string protocol_id);
                                ] ),
                            Semantic_ir.Constructor
                              ("Some", Some (witness_storage methods)),
                            Semantic_ir.Constructor ("None", None) )
                      in
                      Ok (Semantic_ir.Tuple [ witness; value ]))))
      | TOcaml_app (constraint_name, [ element_ty; value_ty ])
        when constraint_name = Types.seqable_constraint_name
             || constraint_name = Types.optional_seqable_constraint_name
             || constraint_name = Types.optional_sequential_constraint_name -> (
          let item_name = "__lg_dynamic_seqable_item" in
          let value_name = "__lg_dynamic_seqable_value" in
          match
             ( dynamic_unpack env element_ty (Semantic_ir.Ident item_name),
               dynamic_unpack env value_ty expression )
           with
          | (Error _ as error), _ -> error
          | _, (Error _ as error) -> error
          | Ok item, Ok value ->
              let adapter =
                Semantic_ir.Fun
                  ( [ Semantic_ir.PVar value_name ],
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                        [
                          Semantic_ir.Fun ([ Semantic_ir.PVar item_name ], item);
                          Semantic_ir.Apply
                            ( Semantic_ir.Ident
                                "Lg_runtime.Runtime_dynamic.to_seq",
                              [ expression ] );
                        ] ) )
              in
              let adapter =
                if constraint_name = Types.seqable_constraint_name then adapter
                else Semantic_ir.Constructor ("Some", Some adapter)
              in
              Ok (Semantic_ir.Tuple [ adapter; value ]))
      | TNamed_record record ->
          if supports_structural_dynamic_packing ty then
            unpack_record record.fields
              (Some
                 (record_type_application record.type_name
                    record.type_arguments))
          else
            let value_name = "__lg_nominal_record" in
            let tag_name = Types.nominal_tag_name record in
            Ok
              (Semantic_ir.Match
                 ( Semantic_ir.Apply
                     ( Semantic_ir.Ident
                         "Lg_runtime.Runtime_dynamic.unpack_nominal",
                       [
                         Semantic_ir.Constructor (tag_name, None);
                         expression;
                       ] ),
                   [
                     ( Semantic_ir.PConstructor
                         ("Some", Some (Semantic_ir.PVar value_name)),
                       cast_nominal_payload
                         (Semantic_ir.Ident value_name) );
                     ( Semantic_ir.PAny,
                       Semantic_ir.Apply
                         ( Semantic_ir.Ident "invalid_arg",
                           [
                             Semantic_ir.String
                               ("dynamic value is not " ^ record.type_name);
                           ] ) );
                   ] ))
      | TRecord fields -> unpack_record fields None
      | map_ty when Option.is_some (Types.dynamic_map_types map_ty) -> (
          match Types.dynamic_map_types map_ty with
          | None -> assert false
          | Some (key_ty, value_ty) ->
              let key_name = "__lg_dynamic_map_key" in
              let value_name = "__lg_dynamic_map_value" in
              Result.bind
                (dynamic_unpack env key_ty (Semantic_ir.Ident key_name))
                (fun key ->
                  Result.map
                    (fun value ->
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
                                  "Lg_runtime.Runtime_dynamic.entries",
                                [ expression ] );
                          ] ))
                    (dynamic_unpack env value_ty
                       (Semantic_ir.Ident value_name))))
      | ( TVector element_ty
        | TList element_ty
        | TSeq element_ty
        | TArray element_ty
        | TOcaml_app ("__lg_next_seq", [ element_ty ])
        | TOcaml_app ("array", [ element_ty ]) ) as collection_ty ->
          let item_name = "__lg_dynamic_collection_item" in
          dynamic_unpack env element_ty (Semantic_ir.Ident item_name)
          |> Result.map (fun unpacked_item ->
                 let mapped =
                   Semantic_ir.Apply
                     ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                    [
                      Semantic_ir.Fun
                           ([ Semantic_ir.PVar item_name ], unpacked_item);
                         Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.to_seq",
                             [ expression ] );
                       ] )
                 in
                 match collection_ty with
                 | TSeq _ | TOcaml_app ("__lg_next_seq", [ _ ]) -> mapped
                 | TList _ ->
                  Semantic_ir.Apply (Semantic_ir.Ident "List.of_seq", [ mapped ])
                 | TVector _ ->
                     Semantic_ir.Apply
                       ( Semantic_ir.Ident "Rrbvec.of_list",
                      [
                        Semantic_ir.Apply
                             (Semantic_ir.Ident "List.of_seq", [ mapped ]);
                         ] )
                 | TArray _ | TOcaml_app ("array", [ _ ]) ->
                     Semantic_ir.Apply
                       (Semantic_ir.Ident "Array.of_seq", [ mapped ])
                 | _ -> assert false)
      | TSet element_ty ->
          let item_name = "__lg_dynamic_set_item" in
          Result.bind (Types.set_module_name element_ty) (fun set_module ->
              dynamic_unpack env element_ty (Semantic_ir.Ident item_name)
              |> Result.map (fun unpacked_item ->
                     let mapped =
                       Semantic_ir.Apply
                         ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                           [ Semantic_ir.Fun
                               ([ Semantic_ir.PVar item_name ], unpacked_item);
                             Semantic_ir.Apply
                               ( Semantic_ir.Ident
                                   "Lg_runtime.Runtime_dynamic.to_seq",
                                 [ expression ] );
                           ] )
                     in
                     Semantic_ir.Apply
                       ( Semantic_ir.Ident (set_module ^ ".of_seq"),
                         [ mapped ] )))
      | _ ->
          Error.error
            ("cannot recover " ^ Types.source_name ty
           ^ " from a dynamic function boundary"))

and pack_dynamic_payload ?(packing_context = []) env expected_dynamic argument =
  pack_dynamic_payload_impl ~packing_context env expected_dynamic argument
  |> Result.map (fun conversion ->
         Semantic_ir.PackDynamic
           {
             source_ty = argument.ty;
             target_ty = expected_dynamic;
             conversion;
           })

and pack_dynamic_payload_impl ?(packing_context = []) env expected_dynamic
    argument =
  let dynamic_core_function_adapter expression =
    match Semantic_ir.unlocated expression with
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.int_quot" ->
        Some "quot_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.int_rem" ->
        Some "rem_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.clojure_mod" ->
        Some "mod_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.int_inc" ->
        Some "inc_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.int_dec" ->
        Some "dec_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.int_max" ->
        Some "max_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.int_min" ->
        Some "min_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.int_zero" ->
        Some "zero_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.int_positive" ->
        Some "positive_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.int_negative" ->
        Some "negative_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.int_even" ->
        Some "even_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.int_odd" ->
        Some "odd_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.int_compare" ->
        Some "compare_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_random.rand_int" ->
        Some "rand_int_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.bool_not" ->
        Some "not_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.keyword_value" ->
        Some "keyword_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.str_value" ->
        Some "str_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.blank" ->
        Some "string_blank_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.includes" ->
        Some "string_includes_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.starts_with" ->
        Some "string_starts_with_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.ends_with" ->
        Some "string_ends_with_function"
    | Semantic_ir.Ident "String.lowercase_ascii" ->
        Some "string_lower_case_function"
    | Semantic_ir.Ident "String.uppercase_ascii" ->
        Some "string_upper_case_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.capitalize" ->
        Some "string_capitalize_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.join" ->
        Some "string_join_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.index_of" ->
        Some "string_index_of_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.last_index_of" ->
        Some "string_last_index_of_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.replace" ->
        Some "string_replace_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.replace_first" ->
        Some "string_replace_first_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.reverse" ->
        Some "string_reverse_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.split" ->
        Some "string_split_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.split_lines" ->
        Some "string_split_lines_function"
    | Semantic_ir.Ident "String.trim" -> Some "string_trim_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.trim_newline" ->
        Some "string_trim_newline_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.triml" ->
        Some "string_triml_function"
    | Semantic_ir.Ident "Lg_runtime.Runtime_string.trimr" ->
        Some "string_trimr_function"
    | _ -> None
  in
  if Types.is_dynamic argument.ty then Ok argument.semantic_expr
  else
    let pack_nested item =
      pack_dynamic_value ~packing_context env expected_dynamic item
    in
    let record_field_expression (field : field) =
      let from_record_values =
        Option.bind argument.record_values (fun values ->
            values
            |> List.find_opt (fun ((actual : field), _) ->
                   actual.keyword = field.keyword)
            |> Option.map snd)
      in
      match from_record_values with
      | Some expression -> expression
      | None -> (
          match Semantic_ir.unlocated argument.semantic_expr with
          | Semantic_ir.Record (values, _) ->
              List.assoc_opt field.ocaml_name values
              |> Option.value ~default:(Structural_map.field_expr argument field)
          | _ -> Structural_map.field_expr argument field)
    in
    let pack_collection element_ty wrapper map =
      let item_name = "__lg_dynamic_item" in
      let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
      pack_nested item
      |> Result.map (fun packed_item ->
             let item_pattern =
               typed_item_pattern item_name element_ty
             in
             let mapper = Semantic_ir.Fun ([ item_pattern ], packed_item) in
             Semantic_ir.Apply
               ( Semantic_ir.Ident wrapper,
              [
                Semantic_ir.Apply
                     (Semantic_ir.Ident map, [ mapper; argument.semantic_expr ]);
                 ] ))
    in
    match argument.ty with
    | TInt | TOcaml "int" ->
        Ok
          (Semantic_ir.Apply
             ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.int",
               [ argument.semantic_expr ] ))
    | TFloat | TOcaml "float" ->
        Ok
          (Semantic_ir.Apply
             ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.float",
               [ argument.semantic_expr ] ))
    | TChar | TOcaml "char" ->
        Ok
          (Semantic_ir.Apply
             ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.char",
               [ argument.semantic_expr ] ))
    | TString | TOcaml "string" ->
        Ok
          (Semantic_ir.Apply
             ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.string",
               [ argument.semantic_expr ] ))
    | TOcaml "Lg_runtime.Runtime_uuid.t" ->
        Ok
          (Semantic_ir.Apply
             ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.uuid",
               [ argument.semantic_expr ] ))
    | TSymbol ->
        Ok
          (Semantic_ir.Apply
             ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.symbol",
               [ argument.semantic_expr ] ))
    | TKeyword ->
        Ok
          (Semantic_ir.Apply
             ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.keyword",
               [ argument.semantic_expr ] ))
    | TBool | TOcaml "bool" ->
        Ok
          (Semantic_ir.Apply
             ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.bool",
               [ argument.semantic_expr ] ))
    | TNil -> Ok (Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.nil")
    | TRef value_ty ->
        let reference_name = "__lg_dynamic_reference" in
        let replacement_name = "__lg_dynamic_reference_value" in
        let current =
          typed_ir value_ty
            (Semantic_ir.Prefix ("!", Semantic_ir.Ident reference_name))
        in
        Result.bind
          (pack_dynamic_value ~packing_context env expected_dynamic current)
          (fun packed_current ->
            Result.map
              (fun replacement ->
                Semantic_ir.Let
                  ( [ (Semantic_ir.PVar reference_name, argument.semantic_expr) ],
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.reference",
                        [
                          Semantic_ir.Fun ([], packed_current);
                          Semantic_ir.Fun
                            ( [ Semantic_ir.PVar replacement_name ],
                              Semantic_ir.Sequence
                                [
                                  Semantic_ir.Infix
                                    ( ":=",
                                      Semantic_ir.Ident reference_name,
                                      replacement );
                                  Semantic_ir.Ident replacement_name;
                                ] );
                        ] ) ))
              (dynamic_unpack env value_ty (Semantic_ir.Ident replacement_name)))
    | TOcaml_app ("Lg_runtime.Runtime_reify.t", [ _ ]) ->
        Ok
          (Semantic_ir.Apply
             ( Semantic_ir.Ident "Lg_runtime.Runtime_reify.dynamic",
               [ argument.semantic_expr ] ))
    | TNullable value_ty | TOcaml_app ("option", [ value_ty ]) ->
        let value_name = "__lg_dynamic_optional_value" in
        let value = typed_ir value_ty (Semantic_ir.Ident value_name) in
        pack_dynamic_value ~packing_context env expected_dynamic value
        |> Result.map (fun packed_value ->
               Semantic_ir.Match
                 ( argument.semantic_expr,
                [
                  ( Semantic_ir.PConstructor ("None", None),
                       Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.nil" );
                     ( Semantic_ir.PConstructor
                         ("Some", Some (Semantic_ir.PVar value_name)),
                       packed_value );
                   ] ))
    | ty when Option.is_some (Types.protocol_constraint_info ty) -> (
        match Types.protocol_constraint_info ty with
        | None -> assert false
        | Some (_, _, value_ty) ->
            let value =
              typed_ir value_ty (constrained_argument_value argument)
            in
            pack_dynamic_payload ~packing_context env expected_dynamic value)
    | TOcaml_app (constraint_name, [ element_ty; value_ty ])
      when constraint_name = Types.seqable_constraint_name
           || constraint_name = Types.optional_seqable_constraint_name
           || constraint_name = Types.optional_sequential_constraint_name -> (
        if
          Types.is_dynamic value_ty || Types.equal value_ty TUnknown
          || match value_ty with TVar _ -> true | _ -> false
        then Ok (constrained_argument_value argument)
        else
        match Semantic_ir.unlocated argument.semantic_expr with
        | Semantic_ir.Ident name
          when not
                 (Collection_capability.identifier_holds_packed_constraint
                    name) ->
            let item_name = "__lg_dynamic_seqable_item" in
            let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
            pack_dynamic_value ~packing_context env expected_dynamic item
            |> Result.map (fun packed_item ->
                   let pack_sequence adapter =
                     Semantic_ir.Apply
                       ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.seq",
                      [
                        Semantic_ir.Apply
                             ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                            [
                              Semantic_ir.Fun
                                   ([ Semantic_ir.PVar item_name ], packed_item);
                                 Semantic_ir.Apply
                                   (adapter, [ argument.semantic_expr ]);
                               ] );
                         ] )
                   in
                   if constraint_name = Types.seqable_constraint_name then
                     pack_sequence (Semantic_ir.Ident (name ^ "__seq"))
                   else
                     let adapter_name = "__lg_dynamic_seqable_adapter" in
                     Semantic_ir.Match
                       ( Semantic_ir.Ident (name ^ "__seq_optional"),
                      [
                        ( Semantic_ir.PConstructor ("None", None),
                             argument.semantic_expr );
                           ( Semantic_ir.PConstructor
                               ("Some", Some (Semantic_ir.PVar adapter_name)),
                             pack_sequence (Semantic_ir.Ident adapter_name) );
                         ] ))
        | _ -> (
            let packed_name = "__lg_dynamic_seqable_value" in
            let packed = Semantic_ir.Ident packed_name in
            let value_expr =
              Semantic_ir.Apply (Semantic_ir.Ident "snd", [ packed ])
            in
            let item_name = "__lg_dynamic_seqable_item" in
            let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
            let value = typed_ir value_ty value_expr in
            match
               ( pack_dynamic_value ~packing_context env expected_dynamic item,
                 pack_dynamic_value ~packing_context env expected_dynamic value )
             with
            | (Error _ as error), _ -> error
            | _, (Error _ as error) -> error
            | Ok packed_item, Ok fallback ->
                let pack_sequence adapter =
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.seq",
                      [
                        Semantic_ir.Apply
                          ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                            [
                              Semantic_ir.Fun
                                ([ Semantic_ir.PVar item_name ], packed_item);
                              Semantic_ir.Apply (adapter, [ value_expr ]);
                            ] );
                      ] )
                in
                let expression =
                  if constraint_name = Types.seqable_constraint_name then
                    pack_sequence
                      (Semantic_ir.Apply (Semantic_ir.Ident "fst", [ packed ]))
                  else
                    let adapter_name = "__lg_dynamic_seqable_adapter" in
                    Semantic_ir.Match
                      ( Semantic_ir.Apply (Semantic_ir.Ident "fst", [ packed ]),
                        [
                          (Semantic_ir.PConstructor ("None", None), fallback);
                          ( Semantic_ir.PConstructor
                              ("Some", Some (Semantic_ir.PVar adapter_name)),
                            pack_sequence (Semantic_ir.Ident adapter_name) );
                        ] )
                in
                Ok
                  (Semantic_ir.Let
                     ( [ (Semantic_ir.PVar packed_name, argument.semantic_expr) ],
                       expression ))))
    | TUnknown | TVar _ -> Ok argument.semantic_expr
    | TFn (parameter_tys, return_ty) -> (
      match dynamic_core_function_adapter argument.semantic_expr with
      | Some runtime_name ->
          Ok
            (Semantic_ir.Ident
               ("Lg_runtime.Runtime_dynamic." ^ runtime_name))
      | None ->
        let argument_names =
          List.mapi
            (fun index _ -> "__lg_dynamic_argument_" ^ string_of_int index)
            parameter_tys
        in
        let rec unpack_arguments unpacked tys names =
          match (tys, names) with
          | [], [] -> Ok (List.rev unpacked)
          | ty :: tys, name :: names ->
              Result.bind (dynamic_unpack env ty (Semantic_ir.Ident name))
                (fun unpacked_argument ->
                  unpack_arguments (unpacked_argument :: unpacked) tys names)
          | _ -> Error.error "dynamic function arity mismatch"
        in
        Result.bind (unpack_arguments [] parameter_tys argument_names)
          (fun unpacked_arguments ->
               let result =
                 typed_ir return_ty
                (Semantic_ir.Apply (argument.semantic_expr, unpacked_arguments))
               in
               pack_dynamic_payload ~packing_context env expected_dynamic result
               |> Result.map (fun packed_result ->
                      let arguments_name = "__lg_dynamic_arguments" in
                      let function_ =
                        Semantic_ir.Fun
                          ( [ Semantic_ir.PVar arguments_name ],
                            Semantic_ir.Match
                              ( Semantic_ir.Ident arguments_name,
                          [
                            ( Semantic_ir.PList
                                      (List.map
                                         (fun name -> Semantic_ir.PVar name)
                                         argument_names),
                                    packed_result );
                                  ( Semantic_ir.PAny,
                                    Semantic_ir.Apply
                                      ( Semantic_ir.Ident "invalid_arg",
                                  [
                                    Semantic_ir.String
                                      "wrong dynamic function argument count";
                                        ] ) );
                                ] ) )
                      in
                      Semantic_ir.Apply
                  ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.function_",
                          [ function_ ] ))))
    | TOverloaded_fn arities ->
        let arguments_name = "__lg_dynamic_overloaded_arguments" in
        let projection index =
          let rec descend expression remaining =
            if remaining = 0 then
              Semantic_ir.Apply (Semantic_ir.Ident "fst", [ expression ])
            else
              descend
                (Semantic_ir.Apply (Semantic_ir.Ident "snd", [ expression ]))
                (remaining - 1)
          in
          descend argument.semantic_expr index
        in
        let indexed = List.mapi (fun index arity -> (index, arity)) arities in
        let indexed =
          List.filter
            (fun (_, (arity : fn_arity)) -> Option.is_none arity.rest_param)
            indexed
          @ List.filter
              (fun (_, (arity : fn_arity)) -> Option.is_some arity.rest_param)
              indexed
        in
        let compile_arity (index, (arity : fn_arity)) =
          let fixed_names =
            List.mapi
              (fun parameter_index _ ->
                "__lg_dynamic_overloaded_argument_"
                ^ string_of_int index ^ "_" ^ string_of_int parameter_index)
              arity.fixed_params
          in
          let rec unpack_fixed unpacked tys names =
            match (tys, names) with
            | [], [] -> Ok (List.rev unpacked)
            | ty :: tys, name :: names ->
                Result.bind
                  (dynamic_unpack env ty (Semantic_ir.Ident name))
                  (fun value -> unpack_fixed (value :: unpacked) tys names)
            | _ -> Error.error "dynamic overloaded function arity mismatch"
          in
          Result.bind
            (unpack_fixed [] arity.fixed_params fixed_names)
            (fun fixed_arguments ->
              let rest_name =
                "__lg_dynamic_overloaded_rest_" ^ string_of_int index
              in
              let rest_argument =
                match arity.rest_param with
                | None -> Ok None
                | Some rest_ty ->
                    let item_name =
                      "__lg_dynamic_overloaded_rest_item_"
                      ^ string_of_int index
                    in
                    Result.map
                      (fun unpacked_item ->
                        Some
                          (Semantic_ir.Apply
                             ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                               [
                                 Semantic_ir.Fun
                                   ([ Semantic_ir.PVar item_name ], unpacked_item);
                                 Semantic_ir.Apply
                                   ( Semantic_ir.Ident "List.to_seq",
                                     [ Semantic_ir.Ident rest_name ] );
                               ] )))
                      (dynamic_unpack env rest_ty (Semantic_ir.Ident item_name))
              in
              Result.bind rest_argument (fun rest_argument ->
                let call_arguments =
                  fixed_arguments
                  @ Option.fold ~none:[] ~some:(fun rest -> [ rest ])
                      rest_argument
                in
                let result =
                  typed_ir arity.return_ty
                    (Semantic_ir.Apply (projection index, call_arguments))
                in
                Result.map
                  (fun packed_result ->
                    let fixed_patterns =
                      List.map (fun name -> Semantic_ir.PVar name) fixed_names
                    in
                    let pattern =
                      match arity.rest_param with
                      | None -> Semantic_ir.PList fixed_patterns
                      | Some _ ->
                          List.fold_right
                            (fun pattern rest -> Semantic_ir.PCons (pattern, rest))
                            fixed_patterns (Semantic_ir.PVar rest_name)
                    in
                    (pattern, packed_result))
                  (pack_dynamic_payload ~packing_context env expected_dynamic
                     result)))
        in
        let rec compile_cases compiled = function
          | [] -> Ok (List.rev compiled)
          | arity :: rest ->
              Result.bind (compile_arity arity) (fun case ->
                  compile_cases (case :: compiled) rest)
        in
        Result.map
          (fun cases ->
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.function_",
                [
                  Semantic_ir.Fun
                    ( [ Semantic_ir.PVar arguments_name ],
                      Semantic_ir.Match
                        ( Semantic_ir.Ident arguments_name,
                          cases
                          @ [
                              ( Semantic_ir.PAny,
                                Semantic_ir.Apply
                                  ( Semantic_ir.Ident "invalid_arg",
                                    [
                                      Semantic_ir.String
                                        "wrong dynamic overloaded function argument count";
                                    ] ) );
                            ] ) );
                ] ))
          (compile_cases [] indexed)
    | TTuple item_tys ->
        let item_names =
          List.mapi
            (fun index _ -> "__lg_dynamic_tuple_item_" ^ string_of_int index)
            item_tys
        in
        let rec pack_items packed tys names =
          match (tys, names) with
          | [], [] -> Ok (List.rev packed)
          | ty :: tys, name :: names ->
              let item = typed_ir ty (Semantic_ir.Ident name) in
              Result.bind (pack_nested item) (fun packed_item ->
                  pack_items (packed_item :: packed) tys names)
          | _ -> Error.error "tuple arity mismatch"
        in
        Result.map
          (fun packed_items ->
            Semantic_ir.Match
              ( argument.semantic_expr,
                [
                  ( Semantic_ir.PTuple
                      (List.map (fun name -> Semantic_ir.PVar name) item_names),
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.vector",
                        [
                    Semantic_ir.Apply
                            ( Semantic_ir.Ident "Rrbvec.of_list",
                              [ Semantic_ir.List packed_items ] );
                        ] ) );
                ] ))
          (pack_items [] item_tys item_names)
    | TVector element_ty -> (
        match Semantic_ir.unlocated argument.semantic_expr with
        | Semantic_ir.Apply
            (Semantic_ir.Ident "Rrbvec.of_list", [ Semantic_ir.List items ]) ->
            let rec pack_items packed = function
              | [] -> Ok (List.rev packed)
              | expression :: rest ->
                  let item = typed_ir element_ty expression in
                  Result.bind (pack_nested item) (fun packed_item ->
                      pack_items (packed_item :: packed) rest)
            in
            Result.map
              (fun packed ->
                Semantic_ir.Apply
                  ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.vector",
                    [
                      Semantic_ir.Apply
                        ( Semantic_ir.Ident "Rrbvec.of_list",
                          [ Semantic_ir.List packed ] );
                    ] ))
              (pack_items [] items)
        | _ ->
            pack_collection element_ty "Lg_runtime.Runtime_dynamic.vector"
              "Rrbvec.map")
    | TList element_ty | TOcaml_app (("list" | "List.t"), [ element_ty ]) ->
        pack_collection element_ty "Lg_runtime.Runtime_dynamic.list" "List.map"
    | TSeq element_ty | TOcaml_app (("Seq.t" | "Seq"), [ element_ty ]) ->
        pack_collection element_ty "Lg_runtime.Runtime_dynamic.seq"
          "Lg_runtime.Runtime_seq.map"
    | TArray element_ty ->
        let item_name = "__lg_dynamic_array_item" in
        let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
        pack_nested item
        |> Result.map (fun packed_item ->
               Semantic_ir.Apply
                 ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.array",
                [
                  Semantic_ir.Apply
                       ( Semantic_ir.Ident "Array.map",
                      [
                        Semantic_ir.Fun
                             ([ Semantic_ir.PVar item_name ], packed_item);
                           argument.semantic_expr;
                         ] );
                   ] ))
    | TOcaml_app (name, [ element_ty ]) when name = Types.next_seq_type_name ->
        let sequence_name = "__lg_next_sequence" in
        let item_name = "__lg_dynamic_item" in
        let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
        pack_nested item
        |> Result.map (fun packed_item ->
               let sequence = Semantic_ir.Ident sequence_name in
               let mapper =
                 Semantic_ir.Fun
                   ([ typed_item_pattern item_name element_ty ], packed_item)
               in
               Semantic_ir.Let
                 ( [ (Semantic_ir.PVar sequence_name, argument.semantic_expr) ],
                   Semantic_ir.If
                     ( Semantic_ir.Apply
                         ( Semantic_ir.Ident
                             "Lg_runtime.Runtime_seq.is_empty",
                           [ sequence ] ),
                       Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.nil",
                       Semantic_ir.Apply
                         ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.seq",
                           [ Semantic_ir.Apply
                               ( Semantic_ir.Ident
                                   "Lg_runtime.Runtime_seq.map",
                                 [ mapper; sequence ] );
                           ] ) ) ))
    | TSet element_ty ->
        let item_name = "__lg_dynamic_set_item" in
        let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
        Result.bind (Types.set_module_name element_ty) (fun set_module ->
            pack_nested item
            |> Result.map (fun packed_item ->
                   Semantic_ir.Apply
                     ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.set",
                    [
                      Semantic_ir.Apply
                           ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                          [
                            Semantic_ir.Fun
                                 ([ Semantic_ir.PVar item_name ], packed_item);
                               Semantic_ir.Apply
                                 ( Semantic_ir.Ident
                                     "Lg_runtime.Runtime_seq.of_list",
                                [
                                  Semantic_ir.Apply
                                       ( Semantic_ir.Ident
                                           (set_module ^ ".elements"),
                                         [ argument.semantic_expr ] );
                                   ] );
                             ] );
                       ] )))
    | map_ty when Option.is_some (Types.dynamic_map_types map_ty) -> (
        match Types.dynamic_map_types map_ty with
        | None -> assert false
        | Some (key_ty, value_ty) -> (
            let key_name = "__lg_dynamic_map_key" in
            let value_name = "__lg_dynamic_map_value" in
            let key = typed_ir key_ty (Semantic_ir.Ident key_name) in
            let value = typed_ir value_ty (Semantic_ir.Ident value_name) in
            match (pack_nested key, pack_nested value) with
            | (Error _ as error), _ -> error
            | _, (Error _ as error) -> error
            | Ok key, Ok value ->
                Ok
                  (Semantic_ir.Apply
                     ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.map",
                       [
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
                               argument.semantic_expr;
                             ] );
                       ] ))))
    | TNamed_record record
      when not (supports_structural_dynamic_packing argument.ty) ->
        let record_type_name =
          record_type_application record.type_name record.type_arguments
        in
        let rebuilt_record replacement =
          Semantic_ir.Record
            ( List.map
                (fun (field : field) ->
                  let value =
                    match replacement field with
                    | Some value -> value
                    | None -> record_field_expression field
                  in
                  (field.ocaml_name, value))
                record.fields,
              Some record_type_name )
        in
        let pack_rebuilt_record replacement =
          pack_dynamic_value ~packing_context env expected_dynamic
            (typed_ir argument.ty (rebuilt_record replacement))
        in
        let rec pack_fields packed = function
          | [] -> Ok (List.rev packed)
          | (field : field) :: fields
            when not (supports_dynamic_field_projection field.ty) ->
              pack_fields packed fields
          | (field : field) :: fields ->
              let value =
                typed_ir field.ty (record_field_expression field)
              in
              Result.bind
                (pack_dynamic_value ~packing_context env
                   (Types.dynamic_constraint TUnknown)
                   value)
                (fun value ->
                  let projection = Semantic_ir.Fun ([], value) in
                  pack_fields
                    (Semantic_ir.Tuple
                       [ Semantic_ir.String field.keyword; projection ]
                    :: packed)
                    fields)
        in
        let replacement_name = "__lg_dynamic_record_replacement" in
        let replacement = Semantic_ir.Ident replacement_name in
        let rec pack_assoc_cases packed = function
          | [] -> Ok (List.rev packed)
          | (field : field) :: fields -> (
              match dynamic_unpack env field.ty replacement with
              | Error _ ->
                  let unsupported =
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "invalid_arg",
                        [
                          Semantic_ir.String
                            ("record field " ^ field.keyword
                           ^ " cannot be updated dynamically");
                        ] )
                  in
                  pack_assoc_cases
                    ((Semantic_ir.PString field.keyword, unsupported) :: packed)
                    fields
              | Ok replacement ->
                  Result.bind
                    (pack_rebuilt_record (fun candidate ->
                         if candidate.keyword = field.keyword then
                           Some replacement
                         else None))
                    (fun updated ->
                      pack_assoc_cases
                        ((Semantic_ir.PString field.keyword, updated) :: packed)
                        fields))
        in
        Result.bind (pack_fields [] record.fields) (fun fields ->
            let extension_field =
              Types.find_record_extension_field record.fields
            in
            let dynamic_payload =
              match extension_field with
              | None ->
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.opaque",
                      [
                        Semantic_ir.String record.type_name;
                        Semantic_ir.List fields;
                      ] )
              | Some extension_field ->
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident
                        "Lg_runtime.Runtime_dynamic.lazy_record",
                      [
                        Semantic_ir.String record.type_name;
                        Semantic_ir.List fields;
                        record_field_expression extension_field;
                      ] )
            in
            let dynamic_record =
              Semantic_ir.Apply
                ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.with_nominal",
                  [
                    Semantic_ir.Constructor
                      (Types.nominal_tag_name record, None);
                    argument.semantic_expr;
                    dynamic_payload;
                  ] )
            in
            match extension_field with
            | None -> Ok dynamic_record
            | Some extension_field ->
                Result.bind
                  (pack_assoc_cases []
                     (Types.record_constructor_fields record.fields))
                  (fun cases ->
                    let extension_keyword = "__lg_dynamic_record_keyword" in
                    Result.map
                      (fun extension_updated ->
                        Semantic_ir.Apply
                          ( Semantic_ir.Ident
                              "Lg_runtime.Runtime_dynamic.with_assoc",
                            [
                              dynamic_record;
                              Semantic_ir.Fun
                                ( [
                                    Semantic_ir.PVar
                                      "__lg_dynamic_record_key";
                                    Semantic_ir.PVar replacement_name;
                                  ],
                                  Semantic_ir.Match
                                    ( Semantic_ir.Apply
                                        ( Semantic_ir.Ident
                                            "Lg_runtime.Runtime_dynamic.as_keyword",
                                          [
                                            Semantic_ir.Ident
                                              "__lg_dynamic_record_key";
                                          ] ),
                                      cases
                                      @ [
                                          ( Semantic_ir.PVar extension_keyword,
                                            extension_updated );
                                        ] ) );
                            ] ))
                      (pack_rebuilt_record (fun candidate ->
                           if
                             candidate.keyword = extension_field.keyword
                           then
                             Some
                               (Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Runtime_map.assoc",
                                    [
                                      record_field_expression extension_field;
                                      Semantic_ir.Ident extension_keyword;
                                      replacement;
                                    ] ))
                           else None))))
    | TRecord fields | TNamed_record { fields; _ } ->
        let rec pack_fields packed = function
          | [] -> Ok (List.rev packed)
          | (field : field) :: rest -> (
              let value =
                typed_ir field.ty (record_field_expression field)
              in
              match
                 pack_dynamic_value ~packing_context env
                   (Types.dynamic_constraint TUnknown) value
               with
              | Error error ->
                  Error
                    {
                      error with
                      message =
                        error.message ^ " while packing record field "
                        ^ field.keyword;
                    }
              | Ok value ->
                  let key =
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.keyword",
                        [ Semantic_ir.String field.keyword ] )
                  in
                  pack_fields (Semantic_ir.Tuple [ key; value ] :: packed) rest)
        in
        pack_fields [] fields
        |> Result.map (fun fields ->
               match argument.ty with
               | TNamed_record record ->
                   Semantic_ir.Apply
                     ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.record",
                    [
                      Semantic_ir.String record.type_name;
                         Semantic_ir.List fields;
                       ] )
               | _ ->
                   Semantic_ir.Apply
                     ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.map",
                       [ Semantic_ir.List fields ] ))
    | _ ->
        Error.error
          ("cannot pass "
          ^ Types.source_name argument.ty
         ^ " through a dynamic function boundary")

and pack_dynamic_value ?(packing_context = []) env expected_dynamic argument =
  let annotate conversion =
    Semantic_ir.PackDynamic
      {
        source_ty = argument.ty;
        target_ty = expected_dynamic;
        conversion;
      }
  in
  match argument.ty with
  | TNamed_record record -> (
      match
        List.find_map
          (fun (type_id, packer_name) ->
            if Type_id.equal type_id record.type_id then Some packer_name
            else None)
          packing_context
      with
      | Some packer_name ->
          Ok
            (annotate
               (Semantic_ir.Apply
                  (Semantic_ir.Ident packer_name, [ argument.semantic_expr ])))
      | None ->
          incr dynamic_packer_counter;
          let suffix = string_of_int !dynamic_packer_counter in
          let packer_name = "__lg_pack_dynamic_record_" ^ suffix in
          let argument_name = "__lg_dynamic_record_" ^ suffix in
          let parameter =
            typed_ir argument.ty (Semantic_ir.Ident argument_name)
          in
          pack_dynamic_value_conversion
            ~packing_context:((record.type_id, packer_name) :: packing_context)
            env expected_dynamic parameter
          |> Result.map (fun body ->
                 annotate
                   (Semantic_ir.LetRecIn
                      ( packer_name,
                        [ typed_item_pattern argument_name argument.ty ],
                        body,
                        Semantic_ir.Apply
                          ( Semantic_ir.Ident packer_name,
                            [ argument.semantic_expr ] ) ))))
  | _ ->
      pack_dynamic_value_conversion ~packing_context env expected_dynamic
        argument
      |> Result.map annotate

and pack_dynamic_value_conversion ?(packing_context = []) env expected_dynamic
    argument =
  match
    pack_dynamic_payload_impl ~packing_context env expected_dynamic argument
  with
  | Error _ as error -> error
  | Ok payload ->
      let satisfied_protocols =
        match argument.ty with
        | TNamed_record { type_parameters = _ :: _; _ } -> []
        | _ -> Protocol.satisfied_protocols env argument.ty
      in
      let implemented_protocols =
        match argument.ty with
        | TNamed_record { type_parameters = _ :: _; _ } -> []
        | _ -> Protocol.implemented_protocols env argument.ty
      in
      let protocol_ids =
        match Types.dynamic_constraint_info expected_dynamic with
        | Some (TNamed_record { type_parameters = _ :: _; _ }) -> []
        | _ ->
            dynamic_protocol_constraints expected_dynamic
            @ dynamic_protocol_constraints argument.ty
            @ satisfied_protocols
            @ implemented_protocols
            |> List.sort_uniq Protocol_id.compare
      in
      let rec compile_protocols protocols = function
        | [] -> Ok (List.rev protocols)
        | protocol_id :: rest -> (
            match
              Protocol.witness_implemented_methods env protocol_id argument.ty
            with
            | None -> compile_protocols protocols rest
            | Some methods -> (
                let rec compile_methods compiled = function
                  | [] -> Ok (List.rev compiled)
                  | (method_name, (implementation : binding)) :: methods -> (
                      match implementation.ty with
                      | TFn (_receiver_ty :: parameter_tys, return_ty) -> (
                          let argument_names =
                            List.mapi
                              (fun index _ ->
                                "__lg_dynamic_argument_" ^ string_of_int index)
                              parameter_tys
                          in
                          let rec unpack_arguments unpacked tys names =
                            match (tys, names) with
                            | [], [] -> Ok (List.rev unpacked)
                            | ty :: tys, name :: names ->
                                Result.bind
                                  (dynamic_unpack env ty
                                     (Semantic_ir.Ident name))
                                  (fun unpacked_argument ->
                                    unpack_arguments
                                      (unpacked_argument :: unpacked)
                                      tys names)
                            | _ -> Error.error "dynamic protocol arity mismatch"
                          in
                          match
                             unpack_arguments [] parameter_tys argument_names
                           with
                          | Error _ as error -> error
                          | Ok unpacked_arguments -> (
                              let call =
                                Semantic_ir.Apply
                                  ( Semantic_ir.Ident implementation.ocaml_name,
                                    argument.semantic_expr :: unpacked_arguments
                                  )
                              in
                              let result = typed_ir return_ty call in
                              match
                                 pack_dynamic_value ~packing_context env
                                   expected_dynamic result
                               with
                              | Error _ as error -> error
                              | Ok result ->
                              let method_ =
                                let arguments_name =
                                  "__lg_dynamic_arguments"
                                in
                                Semantic_ir.Fun
                                  ( [ Semantic_ir.PVar arguments_name ],
                                    Semantic_ir.Match
                                      ( Semantic_ir.Ident arguments_name,
                                            [
                                              ( Semantic_ir.PList
                                              (List.map
                                                 (fun name ->
                                                   Semantic_ir.PVar name)
                                                 argument_names),
                                            result );
                                          ( Semantic_ir.PAny,
                                            Semantic_ir.Apply
                                              ( Semantic_ir.Ident
                                                  "invalid_arg",
                                                    [
                                                      Semantic_ir.String
                                                        ("wrong argument count \
                                                          for dynamic protocol \
                                                          method " ^ method_name
                                                        );
                                                    ] ) );
                                        ] ) )
                              in
                              compile_methods
                                (Semantic_ir.Tuple
                                       [
                                         Semantic_ir.String method_name; method_;
                                       ]
                                :: compiled)
                                methods))
                      | _ ->
                          Error.error
                            "protocol implementation must be a function")
                in
                match compile_methods [] methods with
                | Error _ as error -> error
                | Ok methods ->
                    let protocol =
                      Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.protocol",
                          [
                            Semantic_ir.String
                              (Protocol_id.to_string protocol_id);
                            Semantic_ir.List methods;
                          ] )
                    in
                    compile_protocols (protocol :: protocols) rest))
      in
      compile_protocols [] protocol_ids
      |> Result.map (function
           | [] -> payload
           | protocols ->
               Semantic_ir.Apply
              ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.with_protocols",
                   [ payload; Semantic_ir.List protocols ] ))

let compile_protocol_extension_registration env protocol_id receiver_ty =
  match receiver_ty with
  | TNamed_record record ->
      let receiver_name = "__lg_protocol_extension_receiver" in
      let dynamic_receiver = Semantic_ir.Ident receiver_name in
      Result.bind (dynamic_unpack env receiver_ty dynamic_receiver)
        (fun receiver ->
          Result.map
            (fun packed ->
              Semantic_ir.Apply
                ( Semantic_ir.Ident
                    "Lg_runtime.Runtime_dynamic.register_protocol_extension",
                  [
                    Semantic_ir.String record.type_name;
                    Semantic_ir.String (Protocol_id.to_string protocol_id);
                    Semantic_ir.Fun ([ Semantic_ir.PVar receiver_name ], packed);
                  ] ))
            (pack_dynamic_value env (Types.dynamic_constraint TUnknown)
               (typed_ir receiver_ty receiver)))
  | _ -> Error.error "protocol extension registration expects a named record"

let rec constrained_storage_type expected actual =
  match Types.dynamic_constraint_info expected with
  | Some _ -> expected
  | None -> (
      match Types.protocol_constraint_info expected with
      | Some (_, _, value_ty) -> constrained_storage_type value_ty actual
      | None -> (
          match expected with
          | TOcaml_app (name, [ _element_ty; value_ty ])
            when name = Types.seqable_constraint_name
                 || name = Types.optional_seqable_constraint_name
                 || name = Types.optional_sequential_constraint_name -> (
              match value_ty with
              | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
              | _ -> constrained_storage_type value_ty actual)
          | TUnknown | TVar _ -> Types.constraint_value_type actual
          | _ -> Types.constraint_value_type actual))

let rec pack_constrained_value ?row_type_name env expected argument =
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
    | _ -> true
  in
  if requires_binding then
      let argument_name = "__lg_constrained_argument" in
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
             Semantic_ir.Let
               ([ (Semantic_ir.PVar argument_name, bound_expression) ], packed))
  else
  match (expected, argument.ty) with
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
  | ( TOcaml_app (expected_name, [ expected_element; _ ]),
      TOcaml_app (actual_name, [ actual_element; _ ]) )
    when expected_name = actual_name
         && (expected_name = Types.seqable_constraint_name
            || expected_name = Types.optional_seqable_constraint_name
            || expected_name = Types.optional_sequential_constraint_name)
         &&
         (Types.equal expected_element actual_element
         || ((not (has_capability_constraint expected_element))
            && Bool.equal
                 (Types.is_dynamic actual_element)
                 (Types.is_dynamic expected_element)
            && Types.assignable ~policy:Host_boundary
                 ~expected:expected_element ~actual:actual_element)) ->
      Ok (constrained_argument_expression argument)
  | _ -> (
  match Types.dynamic_constraint_info expected with
  | Some _ -> pack_dynamic_value env expected argument
        | None -> (
  match Types.protocol_constraint_info expected with
  | Some _ when Types.is_dynamic argument.ty ->
      dynamic_unpack env expected argument.semantic_expr
  | Some (protocol_id, witness_ty, value_ty) ->
      let rec witness_method_types = function
        | TUnit -> Ok []
        | TTuple [ method_ty; rest ] ->
            Result.map
              (fun methods -> method_ty :: methods)
              (witness_method_types rest)
        | _ -> Error.error "invalid protocol witness type"
      in
      let adapt_witness_method expected_ty (implementation : binding) =
        match (expected_ty, implementation.ty) with
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
                    | TUnknown | TVar _ -> actual_receiver
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
                  let value =
                    typed_ir expected_ty (Semantic_ir.Ident name)
                  in
                  let adapted_value =
                    if Types.equal expected_ty actual_ty then
                      Ok value.semantic_expr
                    else if
                      expects_dynamic_value expected_ty
                      && not (expects_dynamic_value actual_ty)
                    then dynamic_unpack env actual_ty value.semantic_expr
                    else if has_capability_constraint actual_ty then
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
                  | TUnknown | TVar _ -> actual_return
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
                               | TUnknown | TVar _ -> true
                               | _ -> false) ->
                            Ok
                              (coerce_expression_to_type expected_return
                                 actual_return result.semantic_expr)
                        | Error _ as error -> error
                        | Ok (actual_element, sequence) ->
                            adapt_sequence expected_element actual_element
                              sequence)
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
        match Types.protocol_constraint_info argument.ty with
        | Some (argument_protocol, _, _)
          when Protocol_id.equal protocol_id argument_protocol ->
            Ok
              (protocol_witness_expression protocol_id argument
              |> Option.value
                   ~default:(Semantic_ir.Constructor ("None", None)))
        | _ when has_protocol_constraint protocol_id argument.ty ->
            Ok
              (protocol_witness_expression protocol_id argument
              |> Option.value
                   ~default:(Semantic_ir.Constructor ("None", None)))
        | _ -> (
            let implementations =
              Protocol.witness_implementations env protocol_id argument.ty
            in
            match implementations with
            | None -> Ok (Semantic_ir.Constructor ("None", None))
            | Some implementations ->
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
                    let method_tys =
                      List.map materialize_protocol_unknown method_tys
                    in
                    Result.map
                      (fun methods ->
                        Semantic_ir.Constructor
                          ("Some", Some (witness_storage methods)))
                      (adapt_methods [] method_tys implementations)))
      in
      Result.bind witness (fun witness ->
          pack_constrained_value env value_ty argument
          |> Result.map (fun value -> Semantic_ir.Tuple [ witness; value ]))
  | None -> (
      match expected with
      | TOcaml_app (name, [ expected_element; value_ty ])
        when name = Types.seqable_constraint_name
             || name = Types.optional_seqable_constraint_name
                       || name = Types.optional_sequential_constraint_name -> (
          let actual_element =
            Collection_capability.element_type env argument
          in
          let erase_value =
            match value_ty with
            | TUnknown | TVar _ -> true
            | _ -> false
          in
          let stores_dynamic_value = Types.is_dynamic value_ty in
          let expected_element =
            match expected_element with
            | TUnknown | TVar _ ->
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
            | Some (TUnknown | TVar _), _, _ when statically_empty_argument ->
                Ok None
            | Some actual_element, _, _
              when Types.equal actual_element expected_element ->
                Ok None
            | Some actual_element, Some type_name, TRecord fields
              when Types.assignable ~policy:Structural
                     ~expected:expected_element ~actual:actual_element ->
                let item_name = "__lg_seqable_row_item" in
                let item =
                  typed_ir actual_element (Semantic_ir.Ident item_name)
                in
                project_seqable_row env type_name fields item
                |> Result.map (fun row ->
                       Some
                         (Semantic_ir.Fun
                            ( [ typed_item_pattern item_name actual_element ],
                              row )))
            | Some actual_element, _, _
              when Types.is_dynamic expected_element
                   && not (Types.is_dynamic actual_element) ->
                let item_name = "__lg_seqable_item" in
                let item =
                  typed_ir actual_element (Semantic_ir.Ident item_name)
                in
                pack_dynamic_value env expected_element item
                |> Result.map (fun packed ->
                       Some
                         (Semantic_ir.Fun
                            ([ Semantic_ir.PVar item_name ], packed)))
            | Some actual_element, _, _ when Types.is_dynamic actual_element ->
                let item_name = "__lg_seqable_item" in
                dynamic_unpack env expected_element
                  (Semantic_ir.Ident item_name)
                |> Result.map (fun unpacked ->
                       Some
                         (Semantic_ir.Fun
                            ([ Semantic_ir.PVar item_name ], unpacked)))
            | Some actual_element, _, _
              when has_capability_constraint expected_element ->
                let item_name = "__lg_seqable_item" in
                let item =
                            typed_ir actual_element
                              (Semantic_ir.Ident item_name)
                in
                pack_constrained_value env expected_element item
                |> Result.map (fun packed ->
                       Some
                         (Semantic_ir.Fun
                            ([ Semantic_ir.PVar item_name ], packed)))
            | (None | Some (TUnknown | TVar _)), _, expected_element
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
                       let value_name = "__lg_seqable_value" in
                       Semantic_ir.Fun
                         ( [ Semantic_ir.PVar value_name ],
                           Semantic_ir.Apply
                             (adapter, [ argument.semantic_expr ]) ))
              in
              let adapter =
                if erase_value || stores_dynamic_value then
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
                                    name
                                    = Types.optional_sequential_constraint_name
                             && Types.is_dynamic argument.ty
                           then
                             Semantic_ir.If
                               ( Semantic_ir.Apply
                                   ( Semantic_ir.Ident
                                       "Lg_runtime.Runtime_dynamic.is_sequential",
                                     [ argument.semantic_expr ] ),
                                 Semantic_ir.Constructor
                                   ("Some", Some adapter),
                                        Semantic_ir.Constructor ("None", None)
                                      )
                           else
                                    Semantic_ir.Constructor
                                      ("Some", Some adapter))
                  else Ok (Semantic_ir.Constructor ("None", None))
                else if Types.is_dynamic argument.ty then
                  let value_name = "__lg_dynamic_seqable_value" in
                  let sequence =
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident
                          "Lg_runtime.Runtime_dynamic.to_seq",
                        [ argument.semantic_expr ] )
                  in
                  let sequence =
                    match element_mapper with
                    | None -> sequence
                    | Some mapper ->
                        Semantic_ir.Apply
                                    ( Semantic_ir.Ident
                                        "Lg_runtime.Runtime_seq.map",
                            [ mapper; sequence ] )
                  in
                  let adapter =
                    Semantic_ir.Fun
                      ([ Semantic_ir.PVar value_name ], sequence)
                  in
                  Ok
                              (if name = Types.seqable_constraint_name then
                                 adapter
                     else if
                                 name
                                 = Types.optional_sequential_constraint_name
                       && Types.is_dynamic argument.ty
                     then
                       Semantic_ir.If
                         ( Semantic_ir.Apply
                             ( Semantic_ir.Ident
                                 "Lg_runtime.Runtime_dynamic.is_sequential",
                               [ argument.semantic_expr ] ),
                                     Semantic_ir.Constructor
                                       ("Some", Some adapter),
                           Semantic_ir.Constructor ("None", None) )
                               else
                                 Semantic_ir.Constructor ("Some", Some adapter))
                else if name = Types.seqable_constraint_name then
                            Collection_capability.seqable_adapter
                              ?element_mapper env argument
                  |> Result.map (fun adapter -> adapter)
                else
                  let can_adapt =
                              if
                                name = Types.optional_sequential_constraint_name
                              then is_sequential_type argument.ty
                              else
                                Collection_capability.accepts_seqable env
                                  argument.ty
                  in
                  if can_adapt then
                              Collection_capability.seqable_adapter
                                ?element_mapper env argument
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
                else pack_constrained_value env stored_value_ty argument
              in
                        match
                          (adapter, packed_value)
               with
              | (Error _ as error), _ -> error
              | _, (Error _ as error) -> error
              | Ok adapter, Ok value ->
                  Ok (Semantic_ir.Tuple [ adapter; value ])))
                | _ -> Ok argument.semantic_expr)))

and project_seqable_row env type_name expected_fields argument =
  let type_name = row_call_type_name type_name in
  match Types.record_fields argument.ty with
  | None -> Error.error "seqable row projection expects a record element"
  | Some actual_fields ->
      let rec build values = function
        | [] -> Ok (List.rev values)
        | (expected : field) :: rest -> (
            match find_field expected.keyword actual_fields with
            | None when is_optional_type expected.ty ->
                build
                  ((expected.ocaml_name, Semantic_ir.Constructor ("None", None))
                  :: values)
                  rest
            | None when Types.is_record_extension_field expected ->
                build
                  (( expected.ocaml_name,
                     Semantic_ir.Ident "Lg_runtime.Runtime_map.empty" )
                  :: values)
                  rest
            | None ->
                Error.error
                  ("seqable row projection is missing field "
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
                    build ((expected.ocaml_name, value) :: values) rest))
      in
      Result.map
        (fun fields -> Semantic_ir.Record (fields, Some type_name))
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
    | (TUnknown | TVar _), _ | _, (TUnknown | TVar _) -> true
    | TNullable left, TNullable right
    | TArray left, TArray right
    | TRef left, TRef right
    | TList left, TList right
    | TVector left, TVector right
    | TSet left, TSet right
    | TSeq left, TSeq right ->
        same_runtime_representation left right
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
    | _ -> false

let named_record_can_specialize expected actual =
  let arguments_can_specialize expected actual =
    List.length expected = List.length actual
    && List.for_all2
         (fun expected actual ->
           Result.is_ok (Type_solver.unify [] actual expected))
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
  | (TUnknown | TVar _), TNamed_record _ -> true
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

let rec adapt_value_to_type env expected actual =
  if Types.equal expected actual.ty then Ok actual.semantic_expr
  else if Types.is_dynamic expected then pack_dynamic_value env expected actual
  else if Types.is_dynamic actual.ty then
    dynamic_unpack env expected actual.semantic_expr
  else if same_runtime_representation expected actual.ty then
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
    | Some (expected_key, expected_value), Some (actual_key, actual_value) ->
        let key_name = "__lg_adapt_map_key" in
        let value_name = "__lg_adapt_map_value" in
        let key = typed_ir actual_key (Semantic_ir.Ident key_name) in
        let value = typed_ir actual_value (Semantic_ir.Ident value_name) in
        Result.bind (adapt_value_to_type env expected_key key) (fun key ->
            Result.map
              (fun value ->
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
                      actual.semantic_expr;
                    ] ))
              (adapt_value_to_type env expected_value value))
    | _ ->
        Ok
          (coerce_expression_to_type expected actual.ty actual.semantic_expr))

let compile_record_iequiv_pair scope env left right =
  let adapt expected argument =
    if has_capability_constraint expected then
      pack_constrained_value env expected argument
    else adapt_value_to_type env expected argument
  in
  match left.ty with
  | TNamed_record _ -> (
      match
        Protocol.lookup_protocol_marker scope env "IEquiv" "-equiv"
      with
      | None -> Ok None
      | Some marker -> (
          match Protocol.lookup_marker_impl env marker "-equiv" left.ty with
          | Some { ty = TFn ([ left_ty; right_ty ], TBool); ocaml_name; _ } ->
              Result.bind (adapt left_ty left) (fun left ->
                  Result.map
                    (fun right ->
                      Some
                        (Semantic_ir.Apply
                           (Semantic_ir.Ident ocaml_name, [ left; right ])))
                    (adapt right_ty right))
          | Some _ -> Error.error "IEquiv/-equiv has an invalid signature"
          | None -> Ok None))
  | _ -> Ok None

let compile_equality scope env name args =
  let rec pairs expressions = function
    | left :: ((right :: _) as rest) ->
        Result.bind (compile_record_iequiv_pair scope env left right)
          (function
            | Some expression -> pairs (expression :: expressions) rest
            | None ->
                Result.bind (Core_compare.compile "=" [ left; right ])
                  (fun expression ->
                    pairs (expression.semantic_expr :: expressions) rest))
    | _ -> Ok (List.rev expressions)
  in
  match args with
  | [] | [ _ ] ->
      Ok (typed_ir TBool (Semantic_ir.Bool (name <> "not=")))
  | _ ->
      Result.map
        (fun expressions ->
          let equal = Core_compare.and_expressions expressions in
          typed_ir TBool
            (if name = "not=" then Semantic_ir.Prefix ("not", equal) else equal))
        (pairs [] args)

let adapt_protocol_witness_result env ~expected ~actual expression =
  let actual = materialize_protocol_unknown actual in
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

let typed_row_argument env type_name expected_fields argument =
  let type_name = row_call_type_name type_name in
  let runtime_map_row () =
    match Types.dynamic_map_types argument.ty with
    | Some (key_ty, value_ty) when Types.is_dynamic value_ty ->
        let key_ty =
          match key_ty with
          | TUnknown | TVar _ -> Types.dynamic_constraint TUnknown
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
    match argument.ty with
    | TRecord fields | TNamed_record { fields; _ } -> Some fields
    | TOcaml_app _ -> Some expected_fields
    | _ -> None
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
              Result.bind (extension_value ()) (fun value ->
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
                let value =
                  if has_capability_constraint expected.ty then
                    pack_constrained_value env expected.ty actual_value
                  else adapt_value_to_type env expected.ty actual_value
                in
                Result.bind value (fun value ->
                    build ((expected.ocaml_name, value) :: values) rest))
      in
      Result.map
        (fun fields -> Semantic_ir.Record (fields, Some type_name))
        (build [] expected_fields)

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
                              Semantic_ir.Constructor ("Some", Some value) );
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
  List.exists2
    (fun expected_ty actual_ty ->
      (Types.is_dynamic expected_ty && not (expects_dynamic_value actual_ty))
      ||
      (Types.is_dynamic actual_ty
      && not (expects_dynamic_value expected_ty))
      ||
      (expects_dynamic_value actual_ty
      && has_capability_constraint expected_ty))
    expected actual

let adapt_dynamic_callback env expected arg =
  match (expected, arg.ty) with
  | TFn (expected_params, expected_return), TFn (actual_params, actual_return)
    when (Types.is_dynamic expected_return
         || callback_parameters_need_adapter expected_params actual_params)
         && callback_parameters_compatible expected_params actual_params ->
      let expected_return =
        Types.maybe_reduced_callback_element expected_return
        |> Option.value ~default:expected_return
      in
      let parameter_names =
        List.mapi
          (fun index _ -> "__lg_dynamic_callback_arg_" ^ string_of_int index)
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
      ~dynamic_unpack
  in
  let functions : Function_combinator_elaborator.t =
    Function_combinator_elaborator.create ~compile_expr ~dynamic_unpack
      ~pack_dynamic_value
  in
  let comparisons : Comparison_set_elaborator.t =
    let capability_value argument =
      {
        argument with
        ty =
          (if uses_dynamic_value_storage argument.ty then
             Types.dynamic_constraint TUnknown
           else Types.constraint_value_type argument.ty);
        semantic_expr = constrained_argument_value argument;
      }
    in
    Comparison_set_elaborator.create ~compile_expr ~pack_dynamic_value
      ~capability_value
  in
  let compile_vector = special_forms.compile_vector in
  let compile_list = collection.compile_list in
  let compile_list_star = collection.compile_list_star in
  let compile_range = collection.compile_range in
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
  let compile_zipmap = collection.compile_zipmap in
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
  let compile_sequence_bool_predicate =
    sequence.compile_sequence_bool_predicate
  in
  let compile_map_call = sequence.compile_map_call in
  let compile_keep = sequence.compile_keep in
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
  let clojure_set_function scope env name =
    match String.split_on_char '/' name with
    | [ alias; function_name ] -> (
        match Env.resolve_namespace_alias ~scope alias env with
        | Some "clojure.set" -> Some function_name
        | _ -> None)
    | _ -> None
  in
  let stringify_value scope env ~pr value =
    match value.ty with
    | TNamed_record record -> (
        match lookup_print_method scope env record with
        | Error _ -> Codegen.stringify_expr_ir ~pr value
        | Ok printer ->
            let writer_name = "__lg_print_method_writer" in
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Lg_runtime.Runtime_print.render",
                [
                  Semantic_ir.Fun
                    ( [ Semantic_ir.PVar writer_name ],
                      Semantic_ir.Apply
                        ( Semantic_ir.Ident printer.ocaml_name,
                          [ value.semantic_expr; Semantic_ir.Ident writer_name ]
                        ) );
                ] ))
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
    | [ FList [ FSymbol "hash-set" ] ] ->
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
    | [ FList [ FSymbol "empty"; source_form ] ] -> (
        match compile_expr scope env source_form with
        | Error _ as error -> error
        | Ok source -> (
            match Types.dynamic_map_types source.ty with
            | Some (key_type, value_type) ->
                Ok
                  (typed_ir
                     (TOcaml_app
                        ( "Lg_runtime.Runtime_transient.map",
                          [ key_type; value_type ] ))
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_transient.map_empty",
                          [] )))
            | None when Types.equal source.ty TUnknown ->
                Ok
                  (typed_ir
                     (TOcaml_app
                        ( "Lg_runtime.Runtime_transient.map",
                          [ TUnknown; TUnknown ] ))
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_transient.map_empty",
                          [] )))
            | None -> Error.error "transient empty expects a map"))
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
                    typed_ir
                      (TOcaml_app
                         ("Lg_runtime.Runtime_transient.set", [ element_type ]))
                      (Semantic_ir.Apply
                         ( Semantic_ir.Ident
                             "Lg_runtime.Runtime_transient.set_of_list",
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
                match Types.dynamic_map_types map_type with
                | Some (key_type, value_type) ->
                    let constructor =
                      if Types.is_dynamic key_type then
                        "Lg_runtime.Runtime_transient.map_of_list_dynamic"
                      else "Lg_runtime.Runtime_transient.map_of_list"
                    in
                    Ok
                      (typed_ir
                         (TOcaml_app
                            ( "Lg_runtime.Runtime_transient.map",
                              [ key_type; value_type ] ))
                         (Semantic_ir.Apply
                            ( Semantic_ir.Ident constructor,
                              [ collection.semantic_expr ] )))
                | None -> Error.error "transient expects a set, vector, or map")
            ))
    | _ -> Error.error "transient expects 1 argument"
  and compile_conj_bang scope env = function
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
                          typed_ir
                            (TOcaml_app
                               ( "Lg_runtime.Runtime_transient.set",
                                 [ element_type ] ))
                            (Semantic_ir.Apply
                               ( Semantic_ir.Ident
                                   "Lg_runtime.Runtime_transient.set_add",
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
    match arg_forms with
    | target_form :: pair_forms -> (
        match compile_expr scope env target_form with
        | Error _ as error -> error
        | Ok target when Types.is_dynamic target.ty -> (
            let rec compile_pairs pairs = function
              | [] -> Ok (List.rev pairs)
              | key_form :: value_form :: rest -> (
                  match
                    ( compile_expr scope env key_form,
                      compile_expr scope env value_form )
                  with
                  | (Error _ as error), _ -> error
                  | _, (Error _ as error) -> error
                  | Ok key, Ok value -> (
                      let expected = Types.dynamic_constraint TUnknown in
                      match
                         ( pack_dynamic_value env expected key,
                           pack_dynamic_value env expected value )
                       with
                      | (Error _ as error), _ -> error
                      | _, (Error _ as error) -> error
                      | Ok key, Ok value ->
                          compile_pairs ((key, value) :: pairs) rest))
              | _ ->
                  Error.error
                    "assoc expects collection followed by key/value pairs"
            in
            match compile_pairs [] pair_forms with
            | Error _ as error -> error
            | Ok pairs ->
                let expression =
                  List.fold_left
                    (fun target (key, value) ->
                      Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.assoc",
                          [ target; key; value ] ))
                    target.semantic_expr pairs
                in
                Ok (typed_ir target.ty expression))
        | Ok _ -> compile_static_assoc scope env arg_forms)
    | [] -> Error.error "assoc expects collection followed by key/value pairs"
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
                  | Ok key, Ok value when Types.is_dynamic collection.ty -> (
                      let dynamic = Types.dynamic_constraint TUnknown in
                      match
                         ( pack_dynamic_value env dynamic key,
                           pack_dynamic_value env dynamic value )
                       with
                      | (Error _ as error), _ | _, (Error _ as error) -> error
                      | Ok key, Ok value ->
                          add_pairs
                            (typed_ir collection.ty
                               (Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Runtime_dynamic.assoc_bang",
                                    [ collection.semantic_expr; key; value ] )))
                            rest)
                  | Ok key, Ok value -> (
                      match collection.ty with
                      | TOcaml_app
                          ( "Lg_runtime.Runtime_transient.map",
                            [ key_type; value_type ] ) ->
                          let key_type =
                            if Types.equal key_type TUnknown then
                              match key.ty with
                              | TUnknown | TVar _ ->
                                  Types.dynamic_constraint TUnknown
                              | ty -> ty
                            else key_type
                          in
                          let value_type =
                            if Types.equal value_type TUnknown then
                              match value.ty with
                              | TUnknown | TVar _ ->
                                  Types.dynamic_constraint TUnknown
                              | ty -> ty
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
                  | TUnknown | TVar _ -> true
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
  and compile_apply_zip_vectors scope env fixed_forms rest_form =
    match
      (compile_args_for scope env fixed_forms, compile_expr scope env rest_form)
    with
    | (Error _ as error), _ -> error
    | _, (Error _ as error) -> error
    | Ok fixed, Ok rest ->
        if
          not
            (List.for_all
               (fun collection ->
                 match collection.ty with
                 | TVector _ | TUnknown | TVar _ -> true
                 | _ -> false)
               fixed)
        then Error.error "apply mapv vector expects vector collections"
        else
          let collections =
            List.fold_right
              (fun collection tail ->
                Semantic_ir.Cons (collection.semantic_expr, tail))
              fixed
              (Semantic_ir.Apply
                 (Semantic_ir.Ident "List.of_seq", [ rest.semantic_expr ]))
          in
          Ok
            (typed_ir (TVector (TVector TUnknown))
               (Semantic_ir.Apply
                  ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.zip_vectors",
                    [ collections ] )))
  and compile_call scope env name arg_forms =
    let name =
      match String.split_on_char '/' name with
      | [ alias; member ] -> (
          match Env.resolve_namespace_alias ~scope alias env with
          | Some ("clojure.core" | "cljs.core") -> "clojure.core/" ^ member
          | Some _ | None -> name)
      | _ -> name
    in
    let member_name =
      match String.rindex_opt name '/' with
      | None -> name
      | Some separator ->
          String.sub name (separator + 1) (String.length name - separator - 1)
    in
    let apply_transducer = Core_form_expansion.apply_transducer in
    if member_name = "->Eduction" then
      match arg_forms with
      | [ transducer; collection ] ->
          Result.bind (apply_transducer collection transducer) (fun form ->
              compile_expr scope env form)
      | _ -> Error.error "->Eduction expects a transducer and collection"
    else if member_name = "transduce" then
      match arg_forms with
      | [ transducer; reducer; initial; collection ] ->
          Result.bind (apply_transducer collection transducer)
            (fun transformed ->
              compile_expr scope env
                (FList [ FSymbol "reduce"; reducer; initial; transformed ]))
      | _ ->
          Error.error
            "transduce expects a transducer, reducer, initial value, and \
             collection"
    else
    let qualified_core = String.starts_with ~prefix:"clojure.core/" name in
    let name =
      if qualified_core then
          String.sub name
            (String.length "clojure.core/")
          (String.length name - String.length "clojure.core/")
      else name
    in
    let name =
      match String.rindex_opt name '/' with
      | Some separator ->
          let member =
            String.sub name (separator + 1)
              (String.length name - separator - 1)
          in
          if String.starts_with ~prefix:"->" member then
            String.sub name 0 (separator + 1)
            ^ String.sub member 2 (String.length member - 2)
            ^ "."
          else name
      | None when String.starts_with ~prefix:"->" name ->
          String.sub name 2 (String.length name - 2) ^ "."
      | None -> name
    in
    match clojure_set_function scope env name with
    | Some function_name -> (
        match compile_args_for scope env arg_forms with
        | Error _ as error -> error
        | Ok [ left; right ]
          when function_name = "subset?"
               && not (Types.is_dynamic left.ty || Types.is_dynamic right.ty)
          -> (
            match
              ( Collection_capability.to_seq_expr env left,
                Collection_capability.to_seq_expr env right )
            with
            | Ok (left_ty, left_seq), Ok (right_ty, right_seq)
              when Types.same_shape left_ty right_ty ->
                let left_name = "__lg_subset_left" in
                let right_name = "__lg_subset_right" in
                Result.map
                  (fun equal ->
                    typed_ir TBool
                      (Semantic_ir.Apply
                         ( Semantic_ir.Ident "Seq.for_all",
                           [ Semantic_ir.Fun
                               ( [ Semantic_ir.PVar left_name ],
                                 Semantic_ir.Apply
                                   ( Semantic_ir.Ident "Seq.exists",
                                     [ Semantic_ir.Fun
                                         ( [ Semantic_ir.PVar right_name ],
                                           equal.semantic_expr );
                                       right_seq;
                                     ] ) );
                             left_seq;
                           ] )))
                  (Core_compare.compile "="
                     [ typed_ir left_ty (Semantic_ir.Ident left_name);
                       typed_ir right_ty (Semantic_ir.Ident right_name);
                     ])
            | _ -> Core_set.compile function_name [ left; right ])
        | Ok [ left; right ]
          when function_name = "difference"
               && not (Types.is_dynamic left.ty || Types.is_dynamic right.ty)
          -> (
            match
              ( Collection_capability.to_seq_expr env left,
                Collection_capability.to_seq_expr env right )
            with
            | Ok (left_ty, left_seq), Ok (right_ty, right_seq)
              when Types.same_shape left_ty right_ty ->
                let item_name = "__lg_difference_item" in
                let excluded_name = "__lg_difference_excluded" in
                Result.bind (Types.set_module_name left_ty) (fun set_module ->
                    Result.map
                      (fun equal ->
                        typed_ir (TSet left_ty)
                          (Semantic_ir.Apply
                             ( Semantic_ir.Ident (set_module ^ ".of_seq"),
                               [ Semantic_ir.Apply
                                   ( Semantic_ir.Ident "Seq.filter",
                                     [ Semantic_ir.Fun
                                         ( [ Semantic_ir.PVar item_name ],
                                           Semantic_ir.Prefix
                                             ( "not",
                                               Semantic_ir.Apply
                                                 ( Semantic_ir.Ident
                                                     "Seq.exists",
                                                   [ Semantic_ir.Fun
                                                       ( [ Semantic_ir.PVar
                                                             excluded_name ],
                                                         equal.semantic_expr );
                                                     right_seq;
                                                   ] ) ) );
                                       left_seq;
                                     ] );
                               ] )))
                      (Core_compare.compile "="
                         [ typed_ir left_ty (Semantic_ir.Ident item_name);
                           typed_ir right_ty
                             (Semantic_ir.Ident excluded_name);
                         ]))
            | _ -> Core_set.compile function_name [ left; right ])
        | Ok args
          when List.exists (fun arg -> Types.is_dynamic arg.ty) args ->
            let runtime_name, return_ty =
              match function_name with
              | "union" ->
                  ("Lg_runtime.Runtime_dynamic.set_union",
                   Types.dynamic_constraint TUnknown)
              | "intersection" ->
                  ("Lg_runtime.Runtime_dynamic.set_intersection",
                   Types.dynamic_constraint TUnknown)
              | "difference" ->
                  ("Lg_runtime.Runtime_dynamic.set_difference",
                   Types.dynamic_constraint TUnknown)
              | "subset?" ->
                  ("Lg_runtime.Runtime_dynamic.set_subset", TBool)
              | _ -> ("", TUnknown)
            in
            if runtime_name = "" then Core_set.compile function_name args
            else
              let dynamic = Types.dynamic_constraint TUnknown in
              let rec pack_arguments packed = function
                | [] -> Ok (List.rev packed)
                | argument :: rest -> (
                    match pack_dynamic_value env dynamic argument with
                    | Error _ as error -> error
                    | Ok expression ->
                        pack_arguments (expression :: packed) rest)
              in
              Result.map
                (fun arguments ->
                  let expression =
                    match function_name with
                    | "difference" -> (
                        match arguments with
                        | first :: rest ->
                            Semantic_ir.Apply
                              ( Semantic_ir.Ident runtime_name,
                                [ first; Semantic_ir.List rest ] )
                        | [] ->
                            Semantic_ir.Apply
                              (Semantic_ir.Ident runtime_name, arguments))
                    | "subset?" ->
                      Semantic_ir.Apply
                        (Semantic_ir.Ident runtime_name, arguments)
                    | _ ->
                        Semantic_ir.Apply
                        ( Semantic_ir.Ident runtime_name,
                          [ Semantic_ir.List arguments ] )
                  in
                  typed_ir return_ty expression)
                (pack_arguments [] args)
        | Ok args -> Core_set.compile function_name args)
      | None -> (
    match
            if qualified_core then Error.error "core"
            else lookup_binding scope env name
    with
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
    | "clojure.lang.MapEntry" -> compile_vector scope env arg_forms
    | ( "LazilyPersistentVector/createOwning"
      | "clojure.lang.LazilyPersistentVector/createOwning" ) -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ { ty = TArray element_ty; semantic_expr; _ } ] ->
            Ok
              (typed_ir (TVector element_ty)
                 (apply "Rrbvec.of_list"
                    [ apply "Array.to_list" [ semantic_expr ] ]))
        | Ok [ _ ] -> Error.error "createOwning expects an array"
        | Ok _ -> Error.error "createOwning expects 1 argument")
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
                        dynamic_var = true;
                        _;
                      } ->
                      Result.bind (compile_expr scope env value_form)
                        (fun value ->
                          Result.bind
                            (pack_dynamic_value env value_ty value)
                            (fun value ->
                              compile_bindings
                                ((ocaml_name, value) :: compiled)
                                rest))
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
                          apply "Lg_runtime.Runtime_dynamic_var.bind"
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
    | "js/Date." -> (
        match (Env.target env, arg_forms) with
        | Target.Melange, [] ->
            Ok
              (typed_ir (TOcaml "__lg_date_millis")
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "int_of_float",
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
    | "System/currentTimeMillis" -> (
        match arg_forms with
        | [] ->
            Ok
              (typed_ir TInt
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "int_of_float",
                                [
                                  Semantic_ir.Infix
                          ( "*.",
                            Semantic_ir.Apply
                                        ( Semantic_ir.Ident "Unix.gettimeofday",
                                          [] ),
                                      Semantic_ir.Float "1000." );
                                ] )))
                  | _ ->
                      Error.error "System/currentTimeMillis expects 0 arguments"
                  )
    | "satisfies?" -> (
        match arg_forms with
        | [ FSymbol protocol_name; receiver_form ] -> (
            match
              ( Protocol.find_protocol_id scope env protocol_name,
                compile_expr scope env receiver_form )
            with
                      | None, _ ->
                          Error.error ("unknown protocol " ^ protocol_name)
            | _, (Error _ as error) -> error
            | Some protocol_id, Ok receiver ->
                let expression =
                  if Types.is_dynamic receiver.ty then
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident
                          "Lg_runtime.Runtime_dynamic.has_protocol",
                                  [
                                    receiver.semantic_expr;
                          Semantic_ir.String
                            (Protocol_id.to_string protocol_id);
                        ] )
                  else
                              if has_protocol_constraint protocol_id receiver.ty
                              then
                      match
                                    protocol_witness_expression protocol_id
                                      receiver
                      with
                      | Some witness ->
                          Semantic_ir.Match
                            ( witness,
                                          [
                                            ( Semantic_ir.PConstructor
                                                ("None", None),
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
                                        (Protocol.type_satisfies env protocol_id
                                           receiver.ty);
                        ]
                in
                Ok (typed_ir TBool expression))
        | _ -> Error.error "satisfies? expects a protocol and value")
    | "uuid" -> (
        match compile_args () with
                  | Ok [ value ] when Types.equal value.ty TString -> (
                      match Env.target env with
            | Target.Native ->
                Ok
                  (typed_ir (TOcaml "Lg_runtime.Runtime_uuid.t")
                     (Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Runtime_uuid.of_string",
                          [ value.semantic_expr ] )))
            | Target.Melange | Target.Js_of_ocaml ->
                Ok (typed_ir TString value.semantic_expr))
        | Ok _ -> Error.error "uuid expects a string"
        | Error _ as error -> error)
    | "__type-hint" -> (
        match arg_forms with
        | [ FSymbol annotation; value_form ] -> (
            match
              ( Type_annotation.of_param_annotation annotation,
                compile_expr scope env value_form )
            with
            | (Error _ as error), _ -> error
            | _, (Error _ as error) -> error
            | Ok hinted_type, Ok value ->
                          let hinted_type =
                            Function_elaborator.infer_named_record scope env
                              hinted_type
                          in
                          (match value.ty with
                          | TNullable actual_type
                          | TOcaml_app ("option", [ actual_type ])
                            when (match hinted_type with
                                 | TNullable _
                                 | TOcaml_app ("option", [ _ ]) ->
                                     false
                                 | _ -> true) ->
                              let value_name = "__lg_hinted_optional_value" in
                              let payload = Semantic_ir.Ident value_name in
                              let narrowed =
                                if
                                  Types.is_dynamic actual_type
                                  || match actual_type with
                                     | TUnknown | TVar _ | TRecord _ -> true
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
                          if
                            Types.is_dynamic value.ty
                            && not (Types.is_dynamic hinted_type)
                          then
                            if supports_structural_dynamic_packing hinted_type
                            then
                              Result.map
                                (fun semantic_expr ->
                                  typed_ir hinted_type semantic_expr)
                                (dynamic_unpack env hinted_type
                                   value.semantic_expr)
                            else
                              Ok
                                (typed_ir
                                   (Types.dynamic_constraint hinted_type)
                                   value.semantic_expr)
                          else
                Ok
                              {
                                value with
                    ty = hinted_type;
                    semantic_expr =
                                  Semantic_ir.annotate hinted_type
                                    value.semantic_expr;
                  }))
        | _ -> Error.error "type hint expects metadata and a value")
    | "__pack-dynamic" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ value ] ->
            let dynamic = Types.dynamic_constraint TUnknown in
            Result.map
              (fun value -> typed_ir dynamic value)
              (pack_dynamic_value env dynamic value)
        | Ok _ -> Error.error "dynamic packing expects 1 argument")
    | ".toString" -> (
        match compile_args () with
        | Ok [ value; radix ]
                    when Types.equal value.ty TInt && Types.equal radix.ty TInt
                    ->
            Ok
              (typed_ir TString
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident
                        "Lg_runtime.Runtime_string.int_to_string_radix",
                      [ value.semantic_expr; radix.semantic_expr ] )))
        | Ok _ -> Error.error ".toString expects an int and radix"
        | Error _ as error -> error)
    | ".write" | "-write" -> (
        match compile_args () with
        | Ok [ writer; text ]
          when (Types.equal writer.ty (TOcaml "Buffer.t")
               || match writer.ty with TUnknown | TVar _ -> true | _ -> false)
               && Types.equal text.ty TString ->
            Ok
              (typed_ir TUnit
                 (Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_print.write",
                      [ writer.semantic_expr; text.semantic_expr ] )))
        | Ok _ -> Error.error (name ^ " expects a writer and string")
        | Error _ as error -> error)
    | "pr-writer" -> (
        match compile_args () with
        | Ok [ value; writer; _opts ]
          when Types.equal writer.ty (TOcaml "Buffer.t")
               || match writer.ty with TUnknown | TVar _ -> true | _ -> false ->
            Ok
              (typed_ir TUnit
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_print.write",
                      [ writer.semantic_expr; stringify_value scope env ~pr:true value ] )))
        | Ok _ -> Error.error "pr-writer expects a value, writer, and options"
        | Error _ as error -> error)
    | "pr-sequential-writer" -> (
        match arg_forms with
        | [ writer; printer; prefix; separator; suffix; opts; collection ] ->
            (match printer with
            | FList (FSymbol "fn" :: _) -> (
                match
                  ( compile_args_for scope env
                      [ writer; prefix; separator; suffix; opts; collection ],
                    compile_expr scope (Env.with_expected_type None env) printer )
                with
                | (Error _ as error), _ -> error
                | _, (Error _ as error) -> error
                | ( Ok
                      [
                        writer;
                        prefix;
                        separator;
                        suffix;
                        opts;
                        collection;
                      ],
                    Ok printer ) -> (
                    match printer.ty with
                    | TFn
                        ([ value_ty; writer_ty; opts_ty ], _printer_return) -> (
                        match
                          Collection_capability.to_seq_expr env collection
                        with
                        | Error _ ->
                            Error.error
                              "pr-sequential-writer expects a collection"
                        | Ok (element_ty, sequence) ->
                            let writer_name = "__lg_sequence_writer" in
                            let printer_name = "__lg_sequence_printer" in
                            let prefix_name = "__lg_sequence_prefix" in
                            let separator_name = "__lg_sequence_separator" in
                            let suffix_name = "__lg_sequence_suffix" in
                            let opts_name = "__lg_sequence_opts" in
                            let first_name = "__lg_sequence_first" in
                            let value_name = "__lg_sequence_value" in
                            let adapt expected argument =
                              if has_capability_constraint expected then
                                pack_constrained_value env expected argument
                              else adapt_value_to_type env expected argument
                            in
                            let value =
                              typed_ir element_ty
                                (Semantic_ir.Ident value_name)
                            in
                            let writer_value =
                              typed_ir writer.ty
                                (Semantic_ir.Ident writer_name)
                            in
                            let opts_value =
                              typed_ir opts.ty (Semantic_ir.Ident opts_name)
                            in
                            Result.bind (adapt value_ty value) (fun value ->
                                Result.bind
                                  (adapt writer_ty writer_value)
                                  (fun writer_argument ->
                                    Result.map
                                      (fun opts_argument ->
                                        let write value =
                                          Semantic_ir.Apply
                                            ( Semantic_ir.Ident
                                                "Lg_runtime.Runtime_print.write",
                                              [
                                                Semantic_ir.Ident writer_name;
                                                value;
                                              ] )
                                        in
                                        let print_value =
                                          Semantic_ir.Apply
                                            ( Semantic_ir.Ident printer_name,
                                              [
                                                value;
                                                writer_argument;
                                                opts_argument;
                                              ] )
                                        in
                                        let fold_body =
                                          Semantic_ir.Sequence
                                            [
                                              Semantic_ir.If
                                                ( Semantic_ir.Ident first_name,
                                                  Semantic_ir.Unit,
                                                  write
                                                    (Semantic_ir.Ident
                                                       separator_name) );
                                              print_value;
                                              Semantic_ir.Bool false;
                                            ]
                                        in
                                        typed_ir TUnit
                                          (Semantic_ir.Let
                                             ( [
                                                 ( Semantic_ir.PVar writer_name,
                                                   writer.semantic_expr );
                                                 ( Semantic_ir.PVar
                                                     printer_name,
                                                   printer.semantic_expr );
                                                 ( Semantic_ir.PVar prefix_name,
                                                   prefix.semantic_expr );
                                                 ( Semantic_ir.PVar
                                                     separator_name,
                                                   separator.semantic_expr );
                                                 ( Semantic_ir.PVar suffix_name,
                                                   suffix.semantic_expr );
                                                 ( Semantic_ir.PVar opts_name,
                                                   opts.semantic_expr );
                                               ],
                                               Semantic_ir.Sequence
                                                 [
                                                   write
                                                     (Semantic_ir.Ident
                                                        prefix_name);
                                                   Semantic_ir.Apply
                                                     ( Semantic_ir.Ident
                                                         "Seq.fold_left",
                                                       [
                                                         Semantic_ir.Fun
                                                           ( [
                                                               Semantic_ir.PVar
                                                                 first_name;
                                                               Semantic_ir.PVar
                                                                 value_name;
                                                             ],
                                                             fold_body );
                                                         Semantic_ir.Bool true;
                                                         sequence;
                                                       ] );
                                                   write
                                                     (Semantic_ir.Ident
                                                        suffix_name);
                                                 ] )))
                                      (adapt opts_ty opts_value))))
                    | TFn _ | TOverloaded_fn _ ->
                        Error.error
                          "pr-sequential-writer printer expects three arguments"
                    | _ ->
                        Error.error
                          "pr-sequential-writer expects a printer function")
                | Ok _, Ok _ -> assert false)
            | _ ->
            let writer_name = "__lg_sequence_writer" in
            let printer_name = "__lg_sequence_printer" in
            let prefix_name = "__lg_sequence_prefix" in
            let separator_name = "__lg_sequence_separator" in
            let suffix_name = "__lg_sequence_suffix" in
            let opts_name = "__lg_sequence_opts" in
            let collection_name = "__lg_sequence_collection" in
            let first_name = "__lg_sequence_first" in
            let value_name = "__lg_sequence_value" in
            compile_expr scope env
              (FList
                 [
                   FSymbol "let";
                   FVector
                     [
                       FSymbol writer_name;
                       writer;
                       FSymbol printer_name;
                       printer;
                       FSymbol prefix_name;
                       prefix;
                       FSymbol separator_name;
                       separator;
                       FSymbol suffix_name;
                       suffix;
                       FSymbol opts_name;
                       opts;
                       FSymbol collection_name;
                       collection;
                       FSymbol first_name;
                       FList [ FSymbol "volatile!"; FBool true ];
                     ];
                   FList
                     [
                       FSymbol "do";
                       FList
                         [
                           FSymbol "-write";
                           FSymbol writer_name;
                           FSymbol prefix_name;
                         ];
                       FList
                         [
                           FSymbol "doseq";
                           FVector
                             [ FSymbol value_name; FSymbol collection_name ];
                           FList
                             [
                               FSymbol "if";
                               FList
                                 [ FSymbol "deref"; FSymbol first_name ];
                               FList
                                 [
                                   FSymbol "vreset!";
                                   FSymbol first_name;
                                   FBool false;
                                 ];
                               FList
                                 [
                                   FSymbol "do";
                                   FList
                                     [
                                       FSymbol "-write";
                                       FSymbol writer_name;
                                       FSymbol separator_name;
                                     ];
                                   FBool false;
                                 ];
                             ];
                           FList
                             [
                               FSymbol printer_name;
                               FSymbol value_name;
                               FSymbol writer_name;
                               FSymbol opts_name;
                             ];
                         ];
                       FList
                         [
                           FSymbol "-write";
                           FSymbol writer_name;
                           FSymbol suffix_name;
                         ];
                     ];
                 ]))
        | _ ->
            Error.error
              "pr-sequential-writer expects writer, printer, delimiters, options, and collection")
    | ".getClass" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ value ] ->
            let dynamic_ty = Types.dynamic_constraint TUnknown in
            Result.map
              (fun value ->
                typed_ir dynamic_ty
                  (Semantic_ir.Apply
                               ( Semantic_ir.Ident
                                   "Lg_runtime.Runtime_dynamic.class_",
                       [ value ] )))
              (pack_dynamic_value env dynamic_ty value)
        | Ok _ -> Error.error ".getClass expects 1 argument")
    | ".getName" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ value ] when Types.is_dynamic value.ty ->
            Ok
              (typed_ir TString
                 (Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_dynamic.as_string",
                      [ value.semantic_expr ] )))
        | Ok [ value ] when Types.equal value.ty TString -> Ok value
        | Ok _ -> Error.error ".getName expects a class")
    | ".compareTo" -> (
        match compile_args () with
        | Error _ as error -> error
                  | Ok [ left; right ] -> (
            let dynamic_ty = Types.dynamic_constraint TUnknown in
                      match
               ( pack_dynamic_value env dynamic_ty left,
                 pack_dynamic_value env dynamic_ty right )
             with
            | (Error _ as error), _ | _, (Error _ as error) -> error
            | Ok left, Ok right ->
                Ok
                  (typed_ir TInt
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_dynamic.compare",
                          [ left; right ] ))))
                  | Ok _ ->
                      Error.error ".compareTo expects a receiver and value")
    | ".equals" -> (
        match compile_args () with
        | Error _ as error -> error
                  | Ok [ left; right ] -> (
            let dynamic = Types.dynamic_constraint TUnknown in
                      match
               ( pack_dynamic_value env dynamic left,
                 pack_dynamic_value env dynamic right )
             with
            | (Error _ as error), _ | _, (Error _ as error) -> error
            | Ok left, Ok right ->
                Ok
                  (typed_ir TBool
                     (Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Runtime_dynamic.equal",
                          [ left; right ] ))))
        | Ok _ -> Error.error ".equals expects a receiver and value")
    | ".getTime" -> (
        match compile_args () with
        | Ok [ ({ ty = TOcaml "__lg_date_millis"; _ } as date) ] ->
            Ok (typed_ir TInt date.semantic_expr)
        | Ok _ -> Error.error ".getTime expects a JavaScript Date"
        | Error _ as error -> error)
    | map_constructor
                when String.starts_with ~prefix:"map->" map_constructor -> (
        let type_name =
                    String.sub map_constructor 5
                      (String.length map_constructor - 5)
        in
                  match Resolver.lookup_record_type scope env type_name with
        | Error _ as error -> error
        | Ok record -> (
            match compile_args () with
            | Error _ as error -> error
            | Ok [ source ] ->
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
                              {
                                (typed_ir (TNamed_record record) expression) with
                      record_values =
                        Some
                          (List.map
                             (fun (field, value) ->
                               (field, value.semantic_expr))
                             values);
                    })
                  (prepare [] record.fields)
                      | Ok _ ->
                          Error.error (map_constructor ^ " expects 1 argument"))
                  )
    | constructor_name
      when String.ends_with ~suffix:"." constructor_name
                     && not (is_java_exception_constructor constructor_name)
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
            match compile_args () with
            | Error _ as err -> err
                          | Ok args
                            when List.length args <> List.length constructor_fields
                            ->
                Error.error
                  (constructor_name ^ " expects "
                 ^ string_of_int (List.length constructor_fields)
                 ^ " arguments")
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
                             | TRef _ -> TRef actual
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
                                      | TRef inner when Types.is_dynamic inner
                                        ->
                            Result.map
                              (fun value ->
                                Semantic_ir.Apply
                                                ( Semantic_ir.Ident "ref",
                                                  [ value ] ))
                              (pack_dynamic_value env inner arg)
                        | TRef _ ->
                          Ok
                            (Semantic_ir.Apply
                               ( Semantic_ir.Ident "ref",
                                 [ arg.semantic_expr ] ))
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
                                          | TVar _ | TUnknown
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
    | "__deftype-field-ref" -> (
        match arg_forms with
        | [ FKeyword keyword; target_form ] -> (
            match compile_expr scope env target_form with
            | Error _ as error -> error
            | Ok target -> (
                match target.ty with
                | TRecord fields | TNamed_record { fields; _ } -> (
                    match find_field keyword fields with
                    | Some ({ ty = TRef _; _ } as field) ->
                        Ok
                          (typed_ir field.ty
                             (Structural_map.field_expr target field))
                    | Some _ ->
                                  Error.error
                                    ("field " ^ keyword ^ " is not mutable")
                              | None -> Error.error ("unknown field " ^ keyword)
                              )
                          | _ ->
                              Error.error
                                "mutable field access expects a deftype value"))
                  | _ ->
                      Error.error
                        "mutable field access expects a field and deftype value"
                  )
              | field_access when String.starts_with ~prefix:".-" field_access
                -> (
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
            let keyword =
              ":"
                        ^ String.sub field_access 2
                            (String.length field_access - 2)
            in
            match target.ty with
            | TRecord fields | TNamed_record { fields; _ } -> (
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
                      | ty when Types.is_dynamic ty -> (
                          match Types.dynamic_constraint_info ty with
                          | Some (TNamed_record record) -> (
                              match find_field keyword record.fields with
                              | None -> Error.error ("unknown field " ^ keyword)
                              | Some field ->
                                  let value =
                                    Semantic_ir.Apply
                                      ( Semantic_ir.Ident
                                          "Lg_runtime.Runtime_dynamic.get",
                                        [
                                          target.semantic_expr;
                                          Semantic_ir.Apply
                                            ( Semantic_ir.Ident
                                                "Lg_runtime.Runtime_dynamic.keyword",
                                              [ Semantic_ir.String keyword ] );
                                        ] )
                                  in
                                  if Types.is_dynamic field.ty then
                                    Ok (typed_ir field.ty value)
                                  else if
                                    supports_structural_dynamic_packing field.ty
                                  then
                                    Result.map
                                      (fun value -> typed_ir field.ty value)
                                      (dynamic_unpack env field.ty value)
                                  else
                                    Ok
                                      (typed_ir
                                         (Types.dynamic_constraint field.ty)
                                         value))
                          | _ ->
                              Error.error
                                (field_access ^ " expects a deftype value, got "
                               ^ Types.source_name target.ty ^ source_suffix))
                      | _ ->
                          Error.error
                            (field_access ^ " expects a deftype value, got "
                           ^ Types.source_name target.ty ^ source_suffix)
                      )
        | Ok _ -> Error.error (field_access ^ " expects 1 argument"))
    | ".map" -> (
        match arg_forms with
        | [ receiver; callback ] ->
            compile_expr scope env
              (FList [ FSymbol "amap"; callback; receiver ])
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
    | ".valAt" | "-lookup" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ target; key ] when Types.is_dynamic target.ty ->
            Result.map
              (fun key ->
                typed_ir target.ty
                  (Semantic_ir.Apply
                               ( Semantic_ir.Ident
                                   "Lg_runtime.Runtime_dynamic.get",
                       [ target.semantic_expr; key ] )))
              (pack_dynamic_value env target.ty key)
                  | Ok [ target; key; default ] when Types.is_dynamic target.ty
                    -> (
            match
              ( pack_dynamic_value env target.ty key,
                pack_dynamic_value env target.ty default )
            with
            | (Error _ as error), _ | _, (Error _ as error) -> error
            | Ok key, Ok default ->
                Ok
                  (typed_ir target.ty
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_dynamic.get_default",
                          [ target.semantic_expr; key; default ] ))))
        | Ok [ target; key ] ->
            Ok
              (typed_ir (TNullable TUnknown)
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident
                        (runtime_map_lookup_operation target.ty key.ty
                           "get_option"),
                      [ target.semantic_expr; key.semantic_expr ] )))
        | Ok [ target; key; default ] ->
            Ok
              (typed_ir (TNullable TUnknown)
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident
                        (runtime_map_lookup_operation target.ty key.ty
                           "get_option_default"),
                      [
                        target.semantic_expr;
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
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_reduced.reduced",
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
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Runtime_reduced.unreduced",
                          [ value.semantic_expr ] )))
            | None -> Ok value)
        | Ok _ -> Error.error "unreduced expects 1 arguments")
    | "raise" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ arg ] ->
            Ok
              (typed_ir TUnknown
                           (Semantic_ir.Apply
                              (Semantic_ir.Ident "raise", [ arg.semantic_expr ])))
        | Ok _ -> Error.error "raise expects 1 arguments")
    | "ex-info" -> (
        match compile_args () with
        | Error _ as error -> error
                  | Ok [ message; data ] when Types.equal message.ty TString
                    -> (
            match
                        pack_dynamic_value env
                          (Types.dynamic_constraint TUnknown)
                          data
            with
            | Error _ as error -> error
            | Ok data ->
                Ok
                  (typed_ir (TOcaml "exn")
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_exception.ex_info",
                          [ message.semantic_expr; data ] ))))
                  | Ok [ _; _ ] ->
                      Error.error "ex-info message must be a string"
        | Ok _ -> Error.error "ex-info expects 2 arguments")
    | name when is_java_exception_constructor name -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [] ->
            Ok
              (typed_ir (TOcaml "exn")
                 (Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_exception.create",
                      [ Semantic_ir.String name ] )))
        | Ok [ message ] when Types.equal message.ty TString ->
            Ok
              (typed_ir (TOcaml "exn")
                 (Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_exception.create",
                      [ message.semantic_expr ] )))
        | Ok [ _ ] -> Error.error (name ^ " expects a string message")
        | Ok _ -> Error.error (name ^ " expects 1 argument"))
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
    | "seq-unfold" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok
                      [
                        {
                          ty =
                  TFn
                    ( [ parameter_ty ],
                      TOcaml_app
                                  ("option", [ TTuple [ element_ty; next_ty ] ])
                              );
                semantic_expr = step;
                          _;
                        };
                        initial;
                      ]
                    when Types.assignable ~policy:Host_boundary
                           ~expected:parameter_ty ~actual:initial.ty
                         && Types.assignable ~policy:Host_boundary
                              ~expected:parameter_ty ~actual:next_ty ->
            Ok
              (typed_ir (TSeq element_ty)
                 (apply "Lg_runtime.Runtime_seq.memoize"
                              [
                                apply "Seq.unfold"
                                  [ step; initial.semantic_expr ];
                              ]))
        | Ok [ _; _ ] ->
            Error.error
                        "seq-unfold expects a state step function and initial \
                         state"
        | Ok _ -> Error.error "seq-unfold expects 2 arguments")
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
                        (typed_ir
                           (if name = "uncurried-compare" then TInt
                            else return_ty)
                 (Semantic_ir.Uncurried_apply
                    (fn, [ left.semantic_expr; right.semantic_expr ])))
        | Ok _ ->
            Error.error
              (name
                       ^ " expects a binary function and two compatible \
                          arguments"))
    | ("array-to-seq" | "array-to-rseq") as name -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ array ] -> (
            match array_element_type array.ty with
            | Some element_ty ->
                let runtime_name =
                  if name = "array-to-seq" then
                    "Lg_runtime.Runtime_seq.of_array"
                  else "Lg_runtime.Runtime_seq.of_array_rev"
                in
                Ok
                  (typed_ir (TSeq element_ty)
                     (apply runtime_name [ array.semantic_expr ]))
            | None -> Error.error (name ^ " expects an OCaml array"))
        | Ok _ -> Error.error (name ^ " expects 1 argument"))
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
                Ok
                  (typed_ir (TSeq return_ty)
                     (apply "Lg_runtime.Runtime_seq.memoize"
                                  [
                                    apply "Seq.flat_map"
                                      [
                                        fn;
                              apply
                                (if name = "seq-flat-map" then
                                   "Lg_runtime.Runtime_seq.of_array"
                                           else
                                             "Lg_runtime.Runtime_seq.of_array_rev")
                                          [ collection.semantic_expr ];
                                      ];
                                  ]))
            | Some element_ty, TUnknown
                        when Types.assignable ~policy:Host_boundary
                               ~expected:parameter_ty ~actual:element_ty ->
                Ok
                  (typed_ir (TSeq TUnknown)
                     (apply "Lg_runtime.Runtime_seq.memoize"
                                  [
                                    apply "Seq.flat_map"
                                      [
                                        fn;
                              apply
                                (if name = "seq-flat-map" then
                                   "Lg_runtime.Runtime_seq.of_array"
                                           else
                                             "Lg_runtime.Runtime_seq.of_array_rev")
                                          [ collection.semantic_expr ];
                                      ];
                                  ]))
            | _ ->
                Error.error
                  (name
                           ^ " expects a sequence function and compatible array"
                            ))
        | Ok _ -> Error.error (name ^ " expects 2 arguments"))
    | "make-array" -> (
        match arg_forms with
        | [ FSymbol "Object"; size_form ] -> (
            match compile_expr scope env size_form with
            | Error _ as error -> error
            | Ok size when Types.equal size.ty TInt ->
                let element_ty = Types.dynamic_constraint TUnknown in
                Ok
                  (typed_ir (TArray element_ty)
                     (apply "Array.make"
                        [
                          size.semantic_expr;
                          Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.nil";
                        ]))
            | Ok _ -> Error.error "make-array size must be int")
        | _ -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ size ] when Types.equal size.ty TInt ->
            let element_ty = Types.dynamic_constraint TUnknown in
            Ok
              (typed_ir (TArray element_ty)
                 (apply "Array.make"
                    [
                      size.semantic_expr;
                      Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.nil";
                    ]))
        | Ok [ _ ] -> Error.error "make-array size must be int"
        | Ok _ -> Error.error "make-array expects a size") )
    | "array" | "array-values" -> (
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
                             (Semantic_ir.Array
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
    | "array-seq" -> (
        let compile_array_seq array index =
          if not (int_parameter_type index.ty) then
            Error.error "array-seq index must be int"
          else
            let value_ty = Types.constraint_value_type array.ty in
            let array_ty =
              if Types.is_dynamic value_ty then
                Types.dynamic_constraint_info value_ty
                |> Option.value ~default:TUnknown
              else value_ty
            in
            match array_element_type array_ty with
            | None when Types.is_dynamic value_ty ->
                Ok
                  (typed_ir (Types.next_seq TUnknown)
                     (apply "Lg_runtime.Runtime_seq.drop"
                        [
                          index.semantic_expr;
                          apply "Lg_runtime.Runtime_dynamic.to_seq"
                            [ constrained_argument_value array ];
                        ]))
            | None ->
                Error.error
                  ("array-seq expects an array, got "
                  ^ Types.source_name value_ty)
            | Some element_ty ->
                let sequence =
                  if Types.is_dynamic value_ty then
                    apply "Lg_runtime.Runtime_dynamic.to_seq"
                      [ constrained_argument_value array ]
                  else
                    apply "Lg_runtime.Runtime_seq.of_array"
                      [ constrained_argument_value array ]
                in
                Ok
                  (typed_ir (Types.next_seq element_ty)
                     (apply "Lg_runtime.Runtime_seq.drop"
                        [ index.semantic_expr; sequence ]))
        in
        match compile_args () with
        | Error _ as err -> err
        | Ok [ array ] ->
            compile_array_seq array (typed_ir TInt (Semantic_ir.Int 0))
        | Ok [ array; index ] -> compile_array_seq array index
        | Ok _ -> Error.error "array-seq expects 1 or 2 arguments")
              | ("into-array" | "to-array" | "array-from") as name -> (
        match arg_forms with
        | [ FSymbol "Object"; collection_form ] -> (
            match compile_expr scope env collection_form with
            | Error _ as error -> error
            | Ok collection -> (
                match Collection_capability.to_seq_expr env collection with
                | Error _ -> Error.error (name ^ " expects a seqable value")
                | Ok (element_ty, sequence) ->
                    let dynamic = Types.dynamic_constraint TUnknown in
                    let item_name = "__lg_dynamic_array_item" in
                    let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
                    Result.map
                      (fun item ->
                        typed_ir (TArray dynamic)
                          (apply "Array.of_seq"
                             [
                               apply "Lg_runtime.Runtime_seq.map"
                                 [
                                   Semantic_ir.Fun
                                     ([ Semantic_ir.PVar item_name ], item);
                                   sequence;
                                 ];
                             ]))
                      (pack_dynamic_value env dynamic item)))
        | _ -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ ({ ty = TArray _; semantic_expr; _ } as array) ] ->
                      Ok
                        {
                          array with
                          semantic_expr = apply "Array.copy" [ semantic_expr ];
                        }
        | Ok [ collection ] -> (
                      match
                        Collection_capability.to_seq_expr env collection
                      with
                      | Error _ ->
                          Error.error (name ^ " expects a seqable value")
            | Ok (element_ty, sequence) ->
                Ok
                  (typed_ir (TArray element_ty)
                     (apply "Array.of_seq" [ sequence ])))
        | Ok _ -> Error.error (name ^ " expects a collection")) )
    | ("aget" | "unsafe-aget") as name -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ array; index ] -> (
            let compile_index index =
              if Types.equal index.ty TInt then Ok index.semantic_expr
              else if Types.is_dynamic index.ty then
                Ok
                  (apply "Lg_runtime.Runtime_dynamic.as_int"
                     [ index.semantic_expr ])
              else if
                Types.equal index.ty TUnknown
                || match index.ty with TVar _ -> true | _ -> false
              then Ok index.semantic_expr
              else Error.error "OCaml array index must be int"
            in
            let dynamic_target = Types.is_dynamic array.ty in
            if dynamic_target then
              let dynamic = Types.dynamic_constraint TUnknown in
              Result.bind (pack_dynamic_value env dynamic array) (fun array ->
                  Result.map
                    (fun index ->
                      typed_ir dynamic
                        (apply "Lg_runtime.Runtime_dynamic.array_get"
                           [ array; index ]))
                    (compile_index index))
            else
              match array_element_type array.ty with
              | Some element_ty ->
                Result.map
                  (fun index ->
                    typed_ir element_ty
                      (apply
                         (if name = "aget" then "Array.get"
                          else "Array.unsafe_get")
                         [ array.semantic_expr; index ]))
                  (compile_index index)
              | None -> Error.error (name ^ " expects an OCaml array"))
        | Ok _ -> Error.error (name ^ " expects 2 arguments"))
    | ("aset" | "unsafe-aset") as name -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ array; index; value ] -> (
            let index =
              if Types.equal index.ty TInt then Ok index.semantic_expr
              else if Types.is_dynamic index.ty then
                Ok
                  (apply "Lg_runtime.Runtime_dynamic.as_int"
                     [ index.semantic_expr ])
              else if
                Types.equal index.ty TUnknown
                || match index.ty with TVar _ -> true | _ -> false
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
                          (if name = "aset" then
                             "Lg_runtime.Runtime_dynamic.array_set"
                           else
                             "Lg_runtime.Runtime_dynamic.array_unsafe_set")
                          [ array; index; value ]))
            else
            match array_element_type array.ty with
            | Some element_ty ->
                if Types.is_dynamic element_ty then
                  Result.map
                    (fun value ->
                      typed_ir TUnit
                        (apply
                           (if name = "aset" then "Array.set"
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
                  Ok
                    (typed_ir TUnit
                       (apply
                          (if name = "aset" then "Array.set"
                           else "Array.unsafe_set")
                                    [
                                      array.semantic_expr;
                                      index;
                                      value.semantic_expr;
                                    ]))
            | None -> Error.error (name ^ " expects an OCaml array")))
        | Ok _ -> Error.error (name ^ " expects 3 arguments"))
    | "alength" -> (
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
                  ("alength expects an OCaml array, got "
                 ^ Types.source_name value.ty))
        | Ok _ -> Error.error "alength expects 1 argument")
    | "acopy" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok
                      [
                        { ty = source_type; semantic_expr = source; _ };
              { ty = TInt; semantic_expr = source_start; _ };
              { ty = TInt; semantic_expr = source_end; _ };
              { ty = target_type; semantic_expr = target; _ };
                        { ty = TInt; semantic_expr = target_start; _ };
                      ]
          when compatible_array_types source_type target_type ->
                      let length =
                        Semantic_ir.Infix ("-", source_end, source_start)
                      in
            Ok
              (typed_ir TUnit
                 (apply "Array.blit"
                              [
                                source;
                                source_start;
                                target;
                                target_start;
                                length;
                              ]))
        | Ok [ _; _; _; _; _ ] ->
                      Error.error
                        "acopy expects compatible arrays and int indexes"
        | Ok _ -> Error.error "acopy expects 5 arguments")
    | "aclone" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ ({ ty = TArray _; semantic_expr; _ } as array) ] ->
                      Ok
                        {
                          array with
                          semantic_expr = apply "Array.copy" [ semantic_expr ];
                        }
        | Ok [ { ty = TUnknown; semantic_expr; _ } ] ->
            Ok
              (typed_ir (TArray TUnknown)
                 (apply "Array.copy" [ semantic_expr ]))
        | Ok [ ({ ty; _ } as argument) ]
          when Types.is_dynamic ty ->
            let dynamic = Types.dynamic_constraint TUnknown in
            Result.map
              (fun value ->
                typed_ir dynamic
                  (apply "Lg_runtime.Runtime_dynamic.array_copy" [ value ]))
              (pack_dynamic_value env dynamic argument)
        | Ok [ _ ] -> Error.error "aclone expects an OCaml array"
        | Ok _ -> Error.error "aclone expects 1 argument")
    | "aslice" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok
                      [
                        { ty = array_type; semantic_expr = array; _ };
              { ty = TInt; semantic_expr = from; _ };
                        { ty = TInt; semantic_expr = to_; _ };
                      ] -> (
            match array_element_type array_type with
            | Some element_ty ->
                let length = Semantic_ir.Infix ("-", to_, from) in
                Ok
                  (typed_ir (TArray element_ty)
                     (apply "Array.sub" [ array; from; length ]))
            | None -> Error.error "aslice expects an OCaml array")
                  | Ok _ ->
                      Error.error "aslice expects an array and two int indexes")
    | "aconcat" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok
                      [
                        { ty = left_type; semantic_expr = left; _ };
                        { ty = right_type; semantic_expr = right; _ };
                      ]
          when compatible_array_types left_type right_type -> (
                      match
                        ( array_element_type left_type,
                          array_element_type right_type )
                      with
            | Some left_ty, Some right_ty ->
                let element_ty =
                            if Types.equal left_ty TUnknown then right_ty
                            else left_ty
                in
                Ok
                  (typed_ir (TArray element_ty)
                     (apply "Array.append" [ left; right ]))
            | _ -> Error.error "aconcat expects compatible arrays")
                  | Ok [ _; _ ] ->
                      Error.error "aconcat expects compatible arrays"
        | Ok _ -> Error.error "aconcat expects 2 arguments")
    | "amap" -> (
        match arg_forms with
        | [ fn_form; array_form ] -> (
            match compile_expr scope env array_form with
            | Error _ as error -> error
            | Ok ({ ty = array_type; semantic_expr = array; _ } as _array) -> (
                match array_element_type array_type with
                | None ->
                    Error.error
                      "amap expects a unary function and compatible array"
                | Some element_ty ->
                    let function_env =
                      Env.with_expected_type
                        (Some (TFn ([ element_ty ], TUnknown))) env
                    in
                    (match compile_expr scope function_env fn_form with
                    | Error _ as error -> error
                    | Ok
                        {
                          ty = TFn ([ parameter_ty ], return_ty);
                          semantic_expr = fn;
                          _;
                        }
                      when Types.assignable ~policy:Host_boundary
                             ~expected:parameter_ty ~actual:element_ty ->
                let return_ty =
                  match return_ty with
                  | TUnknown | TVar _
                    when Types.is_dynamic parameter_ty
                         || Types.is_dynamic element_ty ->
                      Types.dynamic_constraint TUnknown
                  | ty -> ty
                in
                let map, arguments =
                  match Env.target env with
                  | Target.Melange ->
                      ("Lg_runtime.Runtime_array_melange.map", [ array; fn ])
                  | Target.Native | Target.Js_of_ocaml ->
                      ("Array.map", [ fn; array ])
                in
                Ok
                  (typed_ir (TArray return_ty)
                     (apply map arguments))
                    | Ok _ ->
                        Error.error
                          "amap expects a unary function and compatible array"))
            )
        | _ -> Error.error "amap expects 2 arguments")
    | "asort!" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok
                      [
                        {
                          ty = TFn ([ left_ty; right_ty ], TInt);
                          semantic_expr = cmp;
                          _;
                        };
                        { ty = array_type; semantic_expr = array; _ };
                      ] -> (
            match array_element_type array_type with
            | Some element_ty
              when (Types.equal element_ty TUnknown
                             || Types.assignable ~policy:Host_boundary
                                  ~expected:left_ty ~actual:element_ty)
                   && (Types.equal element_ty TUnknown
                                || Types.assignable ~policy:Host_boundary
                                     ~expected:right_ty ~actual:element_ty) ->
                          Ok
                            (typed_ir TUnit (apply "Array.sort" [ cmp; array ]))
            | Some _ | None ->
                Error.error
                  "asort! expects a comparator and compatible array")
        | Ok [ _; _ ] ->
                      Error.error
                        "asort! expects a comparator and compatible array"
        | Ok _ -> Error.error "asort! expects 2 arguments")
    | "array?" | "array-value?" -> (
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
        | Ok _ -> Error.error "array? expects 1 argument")
    | "atom" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ] ->
            let value_ty =
              match (arg_forms, Env.expected_type env) with
              | [ FVector [] ], Some (TRef (TVector _ as expected_ty)) ->
                  expected_ty
              | _ -> value.ty
            in
            Ok
              (typed_ir (TRef value_ty)
                 (apply "ref" [ value.semantic_expr ]))
        | Ok _ -> Error.error "atom expects 1 argument")
    | "weak-ref" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ value ] when weak_referenceable_type value.ty ->
            let make =
              match Env.target env with
              | Target.Melange ->
                  "Lg_runtime.Runtime_weak_melange.make"
              | Target.Native | Target.Js_of_ocaml ->
                  "Lg_runtime.Runtime_weak_stdlib.make"
            in
            Ok
              (typed_ir (Types.weak_type value.ty)
                 (apply make [ value.semantic_expr ]))
        | Ok [ _ ] -> Error.error "weak-ref expects a heap value"
        | Ok _ -> Error.error "weak-ref expects 1 argument")
    | "weak-deref" -> (
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
    | "weak-clear!" -> (
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
                  match compile_expr scope env value_form with
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
                        match
                          Types.instantiate_type_fields
                            ~templates:
                              (List.map
                                           (fun ((field : field), _) ->
                                             field.ty)
                                 values)
                            ~actuals:
                                        (List.map
                                           (fun (_, value) -> value.ty)
                                           values)
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
                                       record.type_arguments) )))
                          with
                          record_values =
                            Some
                              (List.map
                                 (fun ((field : field), value) ->
                                   (field, value.semantic_expr))
                                 values);
                        }))
        | _ -> Error.error "record expects a record type and fields")
    | "+" | "-" | "*" | "/" -> (
        match compile_args () with
        | Error _ as err -> err
                  | Ok args -> (
            if Result.is_ok (Core_int.expect_int_args name args) then
              Core_int.compile_operator name args
            else if Core_float.expect_float_args args then
              Core_float.compile_operator name args
            else if
              List.for_all
                (fun arg ->
                            Core_int.accepts_int arg.ty
                            || Core_float.accepts_float arg.ty)
                args
                      then
                        Error.error
                          "numeric arguments must all have the same type"
            else
              match Core_int.expect_int_args name args with
              | Error _ as err -> err
                        | Ok () -> assert false))
    | "inc" ->
        compile_int_unary_call scope env name
                    (fun expression ->
                      Semantic_ir.Infix ("+", expression, Semantic_ir.Int 1))
          arg_forms
    | "dec" ->
        compile_int_unary_call scope env name
                    (fun expression ->
                      Semantic_ir.Infix ("-", expression, Semantic_ir.Int 1))
          arg_forms
    | "rand-int" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ { ty = TInt; semantic_expr; _ } ] ->
            Ok
              (typed_ir TInt
                 (Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_random.rand_int",
                      [ semantic_expr ] )))
        | Ok [ _ ] -> Error.error "rand-int expects an int"
        | Ok _ -> Error.error "rand-int expects 1 argument")
    | "rand" -> (
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
                      [ Semantic_ir.Apply
                          (Semantic_ir.Ident "float_of_int", [ semantic_expr ]);
                      ] )))
        | Ok [ { ty = TFloat; semantic_expr; _ } ] ->
            Ok
              (typed_ir TFloat
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_random.rand",
                      [ semantic_expr ] )))
        | Ok [ _ ] -> Error.error "rand expects a numeric bound"
        | Ok _ -> Error.error "rand expects zero or one argument")
    | ("rand-nth" | "shuffle") as random_operation -> (
        match arg_forms with
        | [ collection_form ] -> (
            match compile_expr scope env collection_form with
            | Error _ as error -> error
            | Ok collection -> (
                match Collection_capability.to_seq_expr env collection with
                | Error _ ->
                    Error.error
                      (random_operation ^ " expects a seqable collection")
                | Ok (inner, sequence) ->
                    let values =
                      Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.to_list",
                          [ sequence ] )
                    in
                    if random_operation = "rand-nth" then
                      Ok
                        (typed_ir inner
                           (Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_random.rand_nth",
                                [ values ] )))
                    else
                      Ok
                        (typed_ir (TVector inner)
                           (Semantic_ir.Apply
                              ( Semantic_ir.Ident "Rrbvec.of_list",
                                [
                                  Semantic_ir.Apply
                                    ( Semantic_ir.Ident
                                        "Lg_runtime.Runtime_random.shuffle",
                                      [ values ] );
                                ] )))))
        | _ ->
            Error.error (random_operation ^ " expects one argument"))
    | "int" | "long" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ ({ ty = TInt; _ } as value) ] -> Ok value
        | Ok [ { ty = TFloat; semantic_expr; _ } ] ->
            Ok
              (typed_ir TInt
                 (Semantic_ir.Apply
                              ( Semantic_ir.Ident "int_of_float",
                                [ semantic_expr ] )))
        | Ok [ { ty = TOcaml "int64"; semantic_expr; _ } ] ->
            Ok
              (typed_ir TInt
                 (Semantic_ir.Apply
                              ( Semantic_ir.Ident "Int64.to_int",
                                [ semantic_expr ] )))
        | Ok [ { ty; semantic_expr; _ } ] when Types.is_dynamic ty ->
            Ok
              (typed_ir TInt
                 (Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_dynamic.to_int",
                      [ semantic_expr ] )))
                  | Ok [ ({ ty = TUnknown | TVar _; _ } as value) ] ->
            Ok { value with ty = TInt }
        | Ok [ _ ] -> Error.error (name ^ " expects a numeric value")
        | Ok _ -> Error.error (name ^ " expects 1 argument"))
    | "double" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ { ty = TInt; semantic_expr; _ } ] ->
            Ok
              (typed_ir TFloat
                 (Semantic_ir.Apply
                              ( Semantic_ir.Ident "float_of_int",
                                [ semantic_expr ] )))
        | Ok [ ({ ty = TFloat; _ } as value) ] -> Ok value
        | Ok [ _ ] -> Error.error "double expects a numeric value"
        | Ok _ -> Error.error "double expects 1 argument")
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
                            let dynamic = Types.dynamic_constraint TUnknown in
                            let compile_dynamic_method
                                (_position, _marker, method_name, implementation)
                                =
                              match implementation.ty with
                              | TFn (parameter_tys, return_ty) ->
                                  let argument_names =
                                    List.mapi
                                      (fun index _ ->
                                        "__lg_reify_argument_"
                                        ^ string_of_int index)
                                      parameter_tys
                                  in
                                  let rec unpack unpacked tys names =
                                    match (tys, names) with
                                    | [], [] -> Ok (List.rev unpacked)
                                    | ty :: tys, name :: names ->
                                        Result.bind
                                          (dynamic_unpack env ty
                                             (Semantic_ir.Ident name))
                                          (fun argument ->
                                            unpack (argument :: unpacked) tys
                                              names)
                    | _ ->
                                        Error.error
                                          "reify method arity mismatch"
                                  in
                                  Result.bind
                                    (unpack [] parameter_tys argument_names)
                                    (fun arguments ->
                                      let result =
                                        typed_ir return_ty
                                          (Semantic_ir.Apply
                                             ( implementation.semantic_expr,
                                               arguments ))
                                      in
                                      Result.map
                                        (fun result ->
                                          let arguments_name =
                                            "__lg_reify_arguments"
                                          in
                                          let method_ =
                                            Semantic_ir.Fun
                                              ( [
                                                  Semantic_ir.PVar arguments_name;
                                                ],
                                                Semantic_ir.Match
                                                  ( Semantic_ir.Ident
                                                      arguments_name,
                                                    [
                                                      ( Semantic_ir.PList
                                                          (List.map
                                                             (fun name ->
                                                               Semantic_ir.PVar
                                                                 name)
                                                             argument_names),
                                                        result );
                                                      ( Semantic_ir.PAny,
                                                        Semantic_ir.Apply
                                                          ( Semantic_ir.Ident
                                                              "invalid_arg",
                                                            [
                                                              Semantic_ir.String
                                                                ("wrong \
                                                                  argument \
                                                                  count for \
                                                                  reify \
                                                                  method "
                                                               ^ method_name);
                                                            ] ) );
                                                    ] ) )
                                          in
                          Semantic_ir.Tuple
                                            [
                                              Semantic_ir.String
                                                (Protocol.method_basename
                                                   method_name);
                                              method_;
                                            ])
                                        (pack_dynamic_value env dynamic result))
                              | _ ->
                                  Error.error "reify method must be a function"
                            in
                            let rec compile_dynamic_methods compiled = function
                              | [] -> Ok (List.rev compiled)
                              | implementation :: rest ->
                                  Result.bind
                                    (compile_dynamic_method implementation)
                                    (fun method_ ->
                                      compile_dynamic_methods
                                        (method_ :: compiled) rest)
                            in
                            Result.bind
                              (compile_dynamic_methods [] implementations)
                              (fun dynamic_methods ->
                                match marker.protocol_id with
                                | None ->
                                    Error.error
                                      ("unknown protocol " ^ protocol_name)
                                | Some protocol_id ->
                                    let dynamic_value =
                                      Semantic_ir.Apply
                                        ( Semantic_ir.Ident
                                            "Lg_runtime.Runtime_dynamic.with_protocols",
                                          [
                                            Semantic_ir.Apply
                                              ( Semantic_ir.Ident
                                                  "Lg_runtime.Runtime_dynamic.opaque",
                                                [
                                                  Semantic_ir.String "reify";
                                                  Semantic_ir.List [];
                                                ] );
                                            Semantic_ir.List
                                              [
                                                Semantic_ir.Apply
                                                  ( Semantic_ir.Ident
                                                      "Lg_runtime.Runtime_dynamic.protocol",
                                                    [
                                                      Semantic_ir.String
                                                        (Protocol_id.to_string
                                                           protocol_id);
                                                      Semantic_ir.List
                                                        dynamic_methods;
                                                    ] );
                                              ];
                                          ] )
                  in
                  Ok
                    (typed_ir
                                         (TOcaml_app
                                            ( "Lg_runtime.Runtime_reify.t",
                                              [ payload_ty ] ))
                                         (Semantic_ir.Apply
                                            ( Semantic_ir.Ident
                                                "Lg_runtime.Runtime_reify.make",
                                              [ payload_expr; dynamic_value ] ))))
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
    | "fnil" -> (
        match arg_forms with
        | [ FSymbol "conj"; default_form ] -> (
            match compile_expr scope env default_form with
            | Error _ as error -> error
                      | Ok default -> (
                let collection = Semantic_ir.Ident "collection" in
                let value = Semantic_ir.Ident "value" in
                let selected_collection =
                  Semantic_ir.Match
                    ( collection,
                                [
                                  ( Semantic_ir.PConstructor ("None", None),
                          default.semantic_expr );
                        ( Semantic_ir.PConstructor
                            ( "Some",
                              Some
                                          (Semantic_ir.PVar "present_collection")
                                      ),
                                    Semantic_ir.Ident "present_collection" );
                                ] )
                in
                          match default.ty with
                | TVector element_type ->
                    let element_type =
                      match element_type with
                      | TUnknown -> TVar "fnil_vector_element"
                      | element_type -> element_type
                    in
                    let result_type = TVector element_type in
                    Ok
                      (typed_ir
                         (TFn
                            ( [ TNullable result_type; element_type ],
                              result_type ))
                         (Semantic_ir.Fun
                                      ( [
                                          Semantic_ir.PVar "collection";
                                          Semantic_ir.PVar "value";
                                        ],
                              Semantic_ir.Apply
                                ( Semantic_ir.Ident "Rrbvec.push_back",
                                  [ selected_collection; value ] ) )))
                | TSet element_type ->
                    Result.map
                      (fun set_module ->
                        let result_type = TSet element_type in
                        typed_ir
                          (TFn
                             ( [ TNullable result_type; TUnknown ],
                               result_type ))
                          (Semantic_ir.Fun
                                       ( [
                                           Semantic_ir.PVar "collection";
                                           Semantic_ir.PVar "value";
                                         ],
                               Semantic_ir.Apply
                                           ( Semantic_ir.Ident
                                               (set_module ^ ".add"),
                                   [ value; selected_collection ] ) )))
                      (Types.set_module_name element_type)
                | _ ->
                    Error.error
                      "fnil conj default must be a vector or set"))
        | [ FSymbol "conj"; _; _ ] | [ FSymbol "conj"; _; _; _ ] ->
                      Error.error
                        "fnil conj currently supports one default argument"
                  | _ ->
                      Error.error
                        "fnil expects a function and default arguments")
    | "volatile!" -> (
        match arg_forms with
        | [ FSymbol "nil" ] ->
            Ok
              (typed_ir
                 (TRef (TOcaml_app ("option", [ TUnknown ])))
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "ref",
                      [ Semantic_ir.Constructor ("None", None) ] )))
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
              | ( Some (TRef (TOcaml_app (expected_name, expected_args) as expected)),
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
                                  ( Semantic_ir.Ident "ref",
                                    [ initial.semantic_expr ] )))
                      | Ok _ -> Error.error "volatile! expects 1 argument"))
    | "deref" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ reference ] -> (
            match reference.ty with
                      | TOcaml_app ("Lg_runtime.Runtime_slot.t", [ value_ty ])
                        ->
                Ok
                  (typed_ir value_ty
                     (Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Runtime_slot.get",
                          [ reference.semantic_expr ] )))
            | TRef value_ty ->
                Ok
                  (typed_ir value_ty
                     (Semantic_ir.Prefix ("!", reference.semantic_expr)))
                      | ty when Types.is_dynamic ty ->
                          Ok
                            (typed_ir ty
                               (Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Runtime_dynamic.deref",
                                    [ reference.semantic_expr ] )))
            | TUnknown | TVar _ ->
                Ok
                  (typed_ir TUnknown
                     (Semantic_ir.Prefix ("!", reference.semantic_expr)))
            | ty ->
                Error.error
                            ("deref expects a reference, got "
                           ^ Types.source_name ty))
        | Ok _ -> Error.error "deref expects 1 argument")
    | ("reset!" | "vreset!") as reset_name -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ reference; value ] -> (
            match reference.ty with
                      | TOcaml_app
                          ("Lg_runtime.Runtime_slot.t", [ referenced_ty ]) ->
                          let stored =
                            if Types.is_dynamic referenced_ty then
                              pack_dynamic_value env referenced_ty value
                            else if
                              Types.assignable ~policy:Host_boundary
                                ~expected:referenced_ty ~actual:value.ty
                            then Ok value.semantic_expr
                            else
                              Error.error
                                (reset_name
                               ^ " value must match referenced type")
                          in
                          Result.map
                            (fun stored ->
                              typed_ir referenced_ty
                     (Semantic_ir.Apply
                                   ( Semantic_ir.Ident
                                       "Lg_runtime.Runtime_slot.set",
                                     [ reference.semantic_expr; stored ] )))
                            stored
            | TRef referenced_ty
              when Types.assignable ~policy:Host_boundary
                     ~expected:referenced_ty ~actual:value.ty ->
                let reference_name = "__lg_reset_reference" in
                let value_name = "__lg_reset_value" in
                Ok
                  (typed_ir value.ty
                     (Semantic_ir.Let
                        ( [
                            ( Semantic_ir.PVar reference_name,
                              reference.semantic_expr );
                            ( Semantic_ir.PVar value_name,
                              value.semantic_expr );
                          ],
                          Semantic_ir.Sequence
                            [
                              Semantic_ir.Infix
                                ( ":=",
                                  Semantic_ir.Ident reference_name,
                                  Semantic_ir.Ident value_name );
                              Semantic_ir.Ident value_name;
                            ] )))
            | TRef _ ->
                          Error.error
                            (reset_name ^ " value must match referenced type")
                      | reference_ty when Types.is_dynamic reference_ty ->
                          let dynamic = Types.dynamic_constraint TUnknown in
                          Result.map
                            (fun replacement ->
                              typed_ir value.ty
                                (Semantic_ir.Let
                                   ( [
                                       ( Semantic_ir.PAny,
                                         Semantic_ir.Apply
                                           ( Semantic_ir.Ident
                                               "Lg_runtime.Runtime_dynamic.reset",
                                             [
                                               reference.semantic_expr;
                                               replacement;
                                             ] ) );
                                     ],
                                     value.semantic_expr )))
                            (pack_dynamic_value env dynamic value)
            | TUnknown | TVar _ ->
                Ok
                  (typed_ir value.ty
                     (Semantic_ir.Sequence
                                  [
                                    Semantic_ir.Infix
                                      ( ":=",
                                        reference.semantic_expr,
                                        value.semantic_expr );
                                    value.semantic_expr;
                        ]))
            | _ ->
                Error.error
                            (reset_name
                           ^ " expects a reference as its first argument"))
        | Ok _ -> Error.error (reset_name ^ " expects 2 arguments"))
    | ("swap!" | "vswap!") as swap_name -> (
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
                                        [
                                          Semantic_ir.Prefix
                                            ("!", reference.semantic_expr);
                              ] )
                        in
                        Ok
                          (typed_ir value_ty
                             (Semantic_ir.Let
                                          ( [
                                              ( Semantic_ir.PVar updated_name,
                                                updated_expr );
                                            ],
                                  Semantic_ir.Sequence
                                              [
                                                Semantic_ir.Infix
                                        ( ":=",
                                          reference.semantic_expr,
                                                    Semantic_ir.Ident
                                                      updated_name );
                                                Semantic_ir.Ident updated_name;
                                    ] ))))
                          | reference_ty when Types.is_dynamic reference_ty ->
                              let dynamic = Types.dynamic_constraint TUnknown in
                              Result.bind
                                (compile_function_arg scope env function_form)
                                (fun update_fn ->
                                  Result.bind
                                    (pack_dynamic_value env dynamic update_fn)
                                    (fun update_fn ->
                                      Result.bind
                                        (compile_args_for scope env extra_forms)
                                        (fun arguments ->
                                          let rec pack packed = function
                                            | [] -> Ok (List.rev packed)
                                            | argument :: rest ->
                                                Result.bind
                                                  (pack_dynamic_value env
                                                     dynamic argument)
                                                  (fun argument ->
                                                    pack (argument :: packed)
                                                      rest)
                                          in
                                          Result.map
                                            (fun arguments ->
                                              typed_ir dynamic
                                                (Semantic_ir.Apply
                                                   ( Semantic_ir.Ident
                                                       "Lg_runtime.Runtime_dynamic.swap",
                                                     [
                                                       reference.semantic_expr;
                                                       update_fn;
                                                       Semantic_ir.List
                                                         arguments;
                                                     ] )))
                                            (pack [] arguments))))
                | _ ->
                    Error.error
                                (swap_name
                               ^ " expects a reference as its first argument")))
        | _ ->
            Error.error
                        (swap_name
                       ^ " expects a reference, function, and optional \
                          arguments"))
    | "=" | "==" | "not=" | "<" | "<=" | ">" | ">=" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok args ->
            if
              name = "=="
              && List.exists (fun arg -> Types.is_dynamic arg.ty) args
            then
              let dynamic = Types.dynamic_constraint TUnknown in
              let rec pack packed = function
                | [] -> Ok (List.rev packed)
                | argument :: rest ->
                    Result.bind (pack_dynamic_value env dynamic argument)
                      (fun argument -> pack (argument :: packed) rest)
              in
              Result.map
                (fun arguments ->
                  typed_ir TBool
                    (apply
                       "Lg_runtime.Runtime_dynamic.numeric_equal_arguments"
                       [ Semantic_ir.List arguments ]))
                (pack [] args)
            else
            let concrete_args =
              List.filter
                (fun arg ->
                  not
                    (Types.is_dynamic arg.ty || Types.equal arg.ty TUnknown
                    || match arg.ty with TVar _ -> true | _ -> false))
                args
            in
            let numeric_ty =
              if
                concrete_args <> []
                && List.for_all (fun arg -> Types.is_numeric arg.ty) concrete_args
                &&
                ( not (name = "=" || name = "not=")
                || List.length concrete_args = List.length args )
              then if
                name = "=="
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
              (name = "=" || name = "not=")
              && List.exists
                   (fun arg ->
                     Types.is_dynamic arg.ty
                     || contains_dynamic_type arg.ty
                     || uses_dynamic_value_storage arg.ty)
                   args
            in
            let rec adapt adapted = function
              | [] -> Ok (List.rev adapted)
              | arg :: rest -> (
                  if dynamic_equality then
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
                  | Some expected when Types.is_dynamic arg.ty -> (
                                  match
                                    dynamic_unpack env expected
                                      arg.semantic_expr
                                  with
                      | Error _ as error -> error
                      | Ok semantic_expr ->
                      adapt
                                        ({
                                           arg with
                                           ty = expected;
                                           semantic_expr;
                                         }
                                        :: adapted)
                                        rest)
                              | Some TFloat
                                when name = "==" && Types.equal arg.ty TInt ->
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
                if name = "=" || name = "not=" then
                  compile_equality scope env name args
                else Core_compare.compile (if name = "==" then "=" else name) args))
              | "not" | "nil?" | "some?" | "true?" | "false?" | "int?"
              | "number?" | "string?" | "keyword?" | "boolean?" | "vector?"
              | "list?" | "seq?" | "set?" | "map?" | "fn?" | "coll?"
              | "associative?" | "indexed?" | "seqable?" | "counted?" ->
                  compile_boolean_call scope env name arg_forms
    | "instance?" -> (
        match arg_forms with
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
            let collection_interface_predicate =
              match type_name with
              | "clojure.lang.Seqable" | "Iterable" ->
                  Some "Lg_runtime.Runtime_dynamic.is_seqable"
                            | "java.util.Map" ->
                                Some "Lg_runtime.Runtime_dynamic.is_map"
              | "Number" | "java.lang.Number" ->
                  Some "Lg_runtime.Runtime_dynamic.is_number"
              | "Comparable" | "java.lang.Comparable" ->
                  Some "Lg_runtime.Runtime_dynamic.is_comparable"
              | _ -> None
            in
            match collection_interface_predicate with
            | Some predicate -> (
                match compile_expr scope env value_form with
                | Error _ as error -> error
                              | Ok value -> (
                                  let dynamic_ty =
                                    Types.dynamic_constraint TUnknown
                                  in
                                  match
                                    pack_dynamic_value env dynamic_ty value
                                  with
                    | Error _ as error -> error
                    | Ok value ->
                        Ok
                          (typed_ir TBool
                             (Semantic_ir.Apply
                                              ( Semantic_ir.Ident predicate,
                                                [ value ] )))))
            | None -> (
                match
                  ( Resolver.lookup_record_type scope env type_name,
                    compile_expr scope env value_form )
                with
                | (Error _ as error), _ -> error
                | _, (Error _ as error) -> error
                              | Ok record, Ok value -> (
                                  let dynamic_ty =
                                    Types.dynamic_constraint TUnknown
                                  in
                                  match
                                    pack_dynamic_value env dynamic_ty value
                                  with
                    | Error _ as error -> error
                    | Ok value ->
                        Ok
                          (typed_ir TBool
                             (Semantic_ir.Apply
                                ( Semantic_ir.Ident
                                    "Lg_runtime.Runtime_dynamic.is_instance",
                                                [
                                                  value;
                                                  Semantic_ir.String
                                                    record.type_name;
                                                ] )))))))
                  | _ -> Error.error "instance? expects a record type and value"
                  )
              | "integer?" | "nat-int?" | "pos-int?" | "neg-int?" | "boolean"
              | "bit-set" | "bit-clear" | "bit-flip" | "bit-test"
              | "bit-shift-right-zero-fill" | "unchecked-add"
              | "unchecked-add-int" | "unchecked-subtract"
              | "unchecked-subtract-int" | "unchecked-multiply"
              | "unchecked-multiply-int" | "unchecked-divide-int"
              | "unchecked-remainder-int" | "unchecked-inc"
    | "unchecked-inc-int" | "unchecked-dec" | "unchecked-dec-int"
              | "unchecked-negate" | "unchecked-negate-int" | "name"
              | "namespace" | "keyword" | "symbol" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok args -> Core_scalar.compile name args)
    | "resolve" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ symbol ]
          when Types.equal symbol.ty TSymbol || Types.is_dynamic symbol.ty ->
            let evaluated =
              if Types.is_dynamic symbol.ty then
                Semantic_ir.Apply
                  ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.as_symbol",
                    [ symbol.semantic_expr ] )
              else symbol.semantic_expr
            in
            Ok
              (typed_ir
                 (TNullable (TRef (Types.dynamic_constraint TUnknown)))
                 (Semantic_ir.Sequence
                    [
                      evaluated;
                      Semantic_ir.Constructor ("None", None);
                    ]))
        | Ok [ _ ] -> Error.error "resolve expects a symbol"
        | Ok _ -> Error.error "resolve expects 1 argument")
    | "sequential?" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ receiver ] -> (
            if Types.is_dynamic receiver.ty then
              Ok
                (typed_ir TBool
                   (Semantic_ir.Apply
                      ( Semantic_ir.Ident
                          "Lg_runtime.Runtime_dynamic.is_sequential",
                        [ receiver.semantic_expr ] )))
            else
            match Types.seqable_constraint_info receiver.ty with
            | Some ((`Optional | `Optional_sequential), _, _) -> (
                            match
                              Semantic_ir.unlocated receiver.semantic_expr
                            with
                | Semantic_ir.Ident name ->
                    Ok
                      (typed_ir TBool
                         (Semantic_ir.Match
                                        ( Semantic_ir.Ident
                                            (name ^ "__seq_optional"),
                                          [
                                            ( Semantic_ir.PConstructor
                                                ("None", None),
                                  Semantic_ir.Bool false );
                                ( Semantic_ir.PConstructor
                                    ("Some", Some Semantic_ir.PAny),
                                  Semantic_ir.Bool true );
                              ] )))
                | _ ->
                    Error.error
                                  "sequential? constrained value must be a \
                                   function parameter")
            | Some (`Required, _, _) ->
                Ok
                  (typed_ir TBool
                     (Semantic_ir.Sequence
                                    [
                                      receiver.semantic_expr;
                                      Semantic_ir.Bool true;
                                    ]))
            | None -> Core_predicate.compile name [ receiver ])
        | Ok _ -> Error.error "sequential? expects 1 arguments")
              | "any?" | "rational?" | "ratio?" | "float?" | "double?"
              | "decimal?" | "symbol?" | "simple-symbol?" | "qualified-symbol?"
              | "simple-keyword?" | "qualified-keyword?" | "ident?"
              | "simple-ident?" | "qualified-ident?" | "reversible?" | "sorted?"
                -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok args -> Core_predicate.compile name args)
    | ("zero?" | "pos?" | "neg?") as predicate -> (
        match compile_args () with
        | Error _ as err -> err
                  | Ok [ arg ] when Types.is_dynamic arg.ty ->
                      let function_name =
                        match predicate with
                        | "zero?" -> "is_zero"
                        | "pos?" -> "is_positive"
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
              | "zero?" -> "="
              | "pos?" -> ">"
              | "neg?" -> "<"
              | _ -> assert false
            in
            let zero =
              match arg.ty with
              | TInt | TUnknown -> Ok (Semantic_ir.Int 0)
              | TFloat -> Ok (Semantic_ir.Float "0.0")
                        | _ ->
                            Error.error
                              ("expected int arguments for " ^ predicate)
            in
            Result.map
              (fun zero ->
                typed_ir TBool
                            (Semantic_ir.Infix
                               (operator, arg.semantic_expr, zero)))
              zero
        | Ok _ -> Error.error (predicate ^ " expects 1 arguments"))
    | "even?" ->
        compile_int_unary_call scope env name
          (fun expression ->
            Semantic_ir.Infix
              ( "=",
                          Semantic_ir.Infix
                            ("mod", expression, Semantic_ir.Int 2),
                Semantic_ir.Int 0 ))
          arg_forms
        |> Result.map (fun expr -> { expr with ty = TBool })
    | "odd?" ->
        compile_int_unary_call scope env name
          (fun expression ->
            Semantic_ir.Infix
              ( "<>",
                          Semantic_ir.Infix
                            ("mod", expression, Semantic_ir.Int 2),
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
                        | _ ->
                            args
                            |> List.map (Codegen.stringify_expr_ir ~pr:false)
                            |> Codegen.concat_expr
            in
            Ok (typed_ir TString expr))
    | "with-meta" | "meta" ->
        compile_metadata_call scope env name arg_forms
    | "__lg_dynamic" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ value ] ->
            let dynamic_ty = Types.dynamic_constraint TUnknown in
            pack_dynamic_value env dynamic_ty value
            |> Result.map (typed_ir dynamic_ty)
                  | Ok _ ->
                      Error.error
                        "internal dynamic conversion expects 1 argument")
    | "__lg_dynamic-narrow" -> (
        match arg_forms with
        | [ expected_form; value_form ] -> (
            match
              ( compile_expr scope env expected_form,
                compile_expr scope env value_form )
            with
            | (Error _ as error), _ -> error
            | _, (Error _ as error) -> error
            | Ok expected, Ok value ->
                let value_ty = Types.constraint_value_type value.ty in
                if Types.equal expected.ty value_ty then
                  Ok (typed_ir expected.ty (constrained_argument_value value))
                else if uses_dynamic_value_storage value.ty then
                  Result.map
                    (typed_ir expected.ty)
                    (dynamic_unpack env expected.ty
                       (constrained_argument_value value))
                else
                  Error.error
                    "internal dynamic narrowing requires dynamic storage")
        | _ ->
            Error.error "internal dynamic narrowing expects 2 arguments")
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
            | _ ->
                Error.error
                  "internal nullable narrowing expects an optional value")
        | Ok _ ->
            Error.error "internal nullable narrowing expects 1 argument")
    | "subs" -> compile_subs scope env arg_forms
    | "max" | "min" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok (first :: _ as args)
          when Types.equal first.ty TFloat
                         && List.for_all
                              (fun arg -> Types.equal arg.ty TFloat)
                              args ->
            Core_float.compile_min_max name args
        | Ok args
          when List.exists (fun arg -> Types.equal arg.ty TFloat) args
               && List.for_all
                    (fun arg -> Types.is_numeric arg.ty)
                    args ->
                      Error.error
                        (name ^ " numeric arguments must all have the same type")
        | Ok args -> Core_int.compile_min_max name args)
    | "quot" | "rem" | "mod" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok args -> Core_int.compile_binary name args)
              | "bit-and" | "bit-or" | "bit-xor" -> (
                  match compile_args () with
        | Error _ as err -> err
        | Ok args -> Core_int.compile_variadic_bitwise name args)
    | "bit-not" ->
        compile_int_unary_call scope env name
          (fun expression -> Semantic_ir.Prefix ("lnot", expression))
          arg_forms
              | "bit-shift-left" | "bit-shift-right" -> (
                  match compile_args () with
        | Error _ as err -> err
        | Ok args -> Core_int.compile_binary name args)
    | "hash-combine" | "clojure.lang.Util/hashCombine" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ left; right ]
                    when Types.equal left.ty TInt && Types.equal right.ty TInt
                    ->
            Ok
              (typed_ir TInt
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident
                        "Lg_runtime.Runtime_int.hash_combine",
                      [ left.semantic_expr; right.semantic_expr ] )))
        | Ok [ _; _ ] -> Error.error (name ^ " expects int arguments")
        | Ok _ -> Error.error (name ^ " expects 2 arguments"))
    | "hash" | "clojure.lang.Util/hasheq" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ value ] ->
            let dynamic_ty = Types.dynamic_constraint TUnknown in
            Result.map
              (fun value ->
                typed_ir TInt
                  (Semantic_ir.Apply
                               ( Semantic_ir.Ident
                                   "Lg_runtime.Runtime_dynamic.hash",
                       [ value ] )))
              (pack_dynamic_value env dynamic_ty value)
        | Ok _ -> Error.error "hash expects 1 argument")
              | "hash-unordered-coll" -> (
                  match compile_args () with
                  | Error _ as error -> error
                  | Ok [ value ] ->
                      let dynamic = Types.dynamic_constraint TUnknown in
                      Result.map
                        (fun value ->
                          typed_ir TInt
                            (Semantic_ir.Apply
                               ( Semantic_ir.Ident
                                   "Lg_runtime.Runtime_dynamic.hash_unordered_coll",
                                 [ value ] )))
                        (pack_dynamic_value env dynamic value)
                  | Ok _ -> Error.error "hash-unordered-coll expects 1 argument"
                  )
              | ("class" | "type") as type_name -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ value ] ->
            let dynamic_ty = Types.dynamic_constraint TUnknown in
            Result.map
              (fun value ->
                typed_ir dynamic_ty
                  (Semantic_ir.Apply
                               ( Semantic_ir.Ident
                                   "Lg_runtime.Runtime_dynamic.class_",
                       [ value ] )))
              (pack_dynamic_value env dynamic_ty value)
                  | Ok _ -> Error.error (type_name ^ " expects 1 argument"))
    | "identical?" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ left; right ]
                    when Types.is_dynamic left.ty || Types.is_dynamic right.ty
                    -> (
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
                                      "Lg_runtime.Runtime_dynamic.equal",
                          [ left; right ] ))))
        | Ok [ left; right ] when Types.equal left.ty right.ty ->
            Ok
              (typed_ir TBool
                 (Semantic_ir.Infix
                    ("==", left.semantic_expr, right.semantic_expr)))
                  | Ok [ _; _ ] ->
                      Error.error "identical? arguments must have the same type"
        | Ok _ -> Error.error "identical? expects 2 arguments")
    | "pr-str" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ arg ] ->
                      Ok
                        (typed_ir TString
                           (stringify_value scope env ~pr:true arg))
        | Ok _ -> Error.error "pr-str expects 1 arguments")
              | "re-matches" -> (
                  match compile_args () with
                  | Error _ as error -> error
                  | Ok [ expression; source ]
                    when Types.equal expression.ty TRegex ->
                      let source =
                        if Types.equal source.ty TString then
                          Ok source.semantic_expr
                        else if Types.is_dynamic source.ty then
                          dynamic_unpack env TString source.semantic_expr
                        else Error.error "re-matches expects a regex and string"
                      in
                      Result.map
                        (fun source ->
                          let regex_name = "__lg_regex" in
                          let groups_name = "__lg_regex_groups" in
                          let pattern =
                            Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_string.regex_pattern",
                                [ expression.semantic_expr ] )
                          in
                          let match_value captures =
                            Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_dynamic.regex_match",
                                [
                                  Semantic_ir.Constructor ("Some", Some captures);
                                ] )
                          in
                          let no_match =
                            Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_dynamic.regex_match",
                                [ Semantic_ir.Constructor ("None", None) ] )
                          in
                          let result =
                            match Env.target env with
                            | Target.Melange ->
                                let regex =
                                  Semantic_ir.Apply
                                    ( Semantic_ir.Ident "Js.Re.fromString",
                                      [
                                        Codegen.concat_expr
                                          [
                                            Semantic_ir.String "^(?:";
                                            pattern;
                                            Semantic_ir.String ")$";
                                          ];
                                      ] )
                                in
                                let execution =
                                  Semantic_ir.Labelled_apply
                                    ( Semantic_ir.Ident "Js.Re.exec",
                                      [
                                        (Some "str", source);
                                        (None, Semantic_ir.Ident regex_name);
                                      ] )
                                in
                                let captures =
                                  Semantic_ir.Apply
                                    ( Semantic_ir.Ident "List.map",
                                      [
                                        Semantic_ir.Ident "Js.Nullable.toOption";
                                        Semantic_ir.Apply
                                          ( Semantic_ir.Ident "Array.to_list",
                                            [
                                              Semantic_ir.Apply
                                                ( Semantic_ir.Ident
                                                    "Js.Re.captures",
                                                  [
                                                    Semantic_ir.Ident
                                                      groups_name;
                                                  ] );
                                            ] );
                                      ] )
                                in
                                Semantic_ir.Let
                                  ( [ (Semantic_ir.PVar regex_name, regex) ],
                                    Semantic_ir.Match
                                      ( execution,
                                        [
                                          ( Semantic_ir.PConstructor
                                              ("None", None),
                                            no_match );
                                          ( Semantic_ir.PConstructor
                                              ( "Some",
                                                Some
                                                  (Semantic_ir.PVar groups_name)
                                              ),
                                            match_value captures );
                                        ] ) )
                            | Target.Native | Target.Js_of_ocaml ->
                                let regex =
                                  Semantic_ir.Apply
                                    ( Semantic_ir.Ident "Re.compile",
                                      [
                                        Semantic_ir.Apply
                                          ( Semantic_ir.Ident "Re.whole_string",
                                            [
                                              Semantic_ir.Apply
                                                ( Semantic_ir.Ident "Re.Perl.re",
                                                  [ pattern ] );
                                            ] );
                                      ] )
                                in
                                let execution =
                                  Semantic_ir.Apply
                                    ( Semantic_ir.Ident "Re.exec_opt",
                                      [ Semantic_ir.Ident regex_name; source ]
                                    )
                                in
                                let index_name = "__lg_regex_group_index" in
                                let captures =
                                  Semantic_ir.Apply
                                    ( Semantic_ir.Ident "List.init",
                                      [
                                        Semantic_ir.Apply
                                          ( Semantic_ir.Ident "Re.group_count",
                                            [ Semantic_ir.Ident regex_name ] );
                                        Semantic_ir.Fun
                                          ( [ Semantic_ir.PVar index_name ],
                                            Semantic_ir.Apply
                                              ( Semantic_ir.Ident
                                                  "Re.Group.get_opt",
                                                [
                                                  Semantic_ir.Ident groups_name;
                                                  Semantic_ir.Ident index_name;
                                                ] ) );
                                      ] )
                                in
                                Semantic_ir.Let
                                  ( [ (Semantic_ir.PVar regex_name, regex) ],
                                    Semantic_ir.Match
                                      ( execution,
                                        [
                                          ( Semantic_ir.PConstructor
                                              ("None", None),
                                            no_match );
                                          ( Semantic_ir.PConstructor
                                              ( "Some",
                                                Some
                                                  (Semantic_ir.PVar groups_name)
                                              ),
                                            match_value captures );
                                        ] ) )
                          in
                          typed_ir (Types.dynamic_constraint TUnknown) result)
                        source
                  | Ok _ -> Error.error "re-matches expects a regex and string")
    | "pr" -> (
        match compile_args () with
        | Error _ as error -> error
        | Ok [ arg ] -> (
            match lookup_binding scope env "*out*" with
                      | Error _ ->
                          Error.error "pr requires a bound *out* writer"
            | Ok writer ->
                Ok
                  (typed_ir TUnit
                     (Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Runtime_print.write",
                                    [
                                      Semantic_ir.Ident writer.ocaml_name;
                            stringify_value scope env ~pr:true arg;
                          ] ))))
        | Ok _ -> Error.error "pr expects 1 argument")
    | "print" | "println" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ arg ] ->
                      let printer =
                        if name = "print" then "print_string"
                        else "print_endline"
                      in
            Ok
              (typed_ir TUnit
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident printer,
                      [ stringify_value scope env ~pr:false arg ] )))
        | Ok _ -> Error.error (name ^ " expects 1 arguments"))
    | "prn" ->
        Result.map
          (fun args ->
            let rendered =
              args |> List.map (stringify_value scope env ~pr:true)
              |> fun values ->
              Semantic_ir.Apply
                ( Semantic_ir.Ident "String.concat",
                  [ Semantic_ir.String " "; Semantic_ir.List values ] )
            in
            typed_ir TUnit
              (Semantic_ir.Apply
                 (Semantic_ir.Ident "print_endline", [ rendered ])))
          (compile_args ())
    | "list" -> compile_list scope env arg_forms
    | "list*" -> compile_list_star scope env arg_forms
    | "range" -> compile_range scope env arg_forms
    | "list-of" -> compile_list_of arg_forms
    | "cons" -> compile_cons scope env arg_forms
    | "vector" -> compile_vector scope env arg_forms
    | "vector-of" -> compile_vector_of arg_forms
    | "count" -> compile_collection_call scope env name arg_forms
    | "conj" -> compile_conj scope env arg_forms
    | "conj!" -> compile_conj_bang scope env arg_forms
    | "first" | "second" | "last" | "peek" | "pop" ->
        compile_collection_call scope env name arg_forms
    | "subvec" -> compile_subvec scope env arg_forms
    | "nth" -> compile_nth scope env arg_forms
    | "get" -> compile_get scope env arg_forms
    | "get-in" -> compile_get_in scope env arg_forms
    | "find" -> compile_find scope env arg_forms
    | "assoc" | "-assoc" | "clojure.lang.RT/assoc" ->
        compile_assoc scope env arg_forms
    | "assoc-in" -> compile_assoc_in scope env arg_forms
    | "assoc!" -> compile_assoc_bang scope env arg_forms
    | "dissoc!" -> compile_dissoc_bang scope env arg_forms
    | "dissoc" -> compile_dissoc scope env arg_forms
    | "merge" -> compile_merge scope env arg_forms
    | "update" -> compile_update scope env arg_forms
    | "update-in" -> compile_update_in scope env arg_forms
    | "select-keys" -> compile_select_keys scope env arg_forms
    | "contains?" -> compile_contains scope env arg_forms
    | "keys" -> compile_keys scope env arg_forms
    | "vals" -> compile_vals scope env arg_forms
    | "zipmap" -> compile_zipmap scope env arg_forms
    | "transient" -> compile_transient scope env arg_forms
    | "persistent!" -> compile_persistent_bang scope env arg_forms
              | "hash-map" | "array-map" | "sorted-map" ->
                  compile_hash_map scope env arg_forms
              | "rest" | "seq" | "empty?" ->
                  compile_collection_call scope env name arg_forms
    | "not-empty" -> compile_not_empty scope env arg_forms
    | "into" -> (
        match arg_forms with
        | [ target_form; FSymbol "cat"; source_form ] -> (
            match
              ( compile_expr scope env target_form,
                compile_expr scope env source_form )
            with
            | (Error _ as error), _ -> error
            | _, (Error _ as error) -> error
            | Ok target, Ok source ->
                          Core_sequence_transform.compile "into-cat"
                            [ target; source ])
        | [ target_form; transducer_form; source_form ] ->
            Result.bind (apply_transducer source_form transducer_form)
              (fun transformed ->
                compile_into scope env target_form transformed)
        | [ target_form; source_form ] ->
            compile_into scope env target_form source_form
                  | _ ->
                      compile_sequence_transform_call scope env name arg_forms)
              | "take" | "drop" ->
                  compile_collection_call scope env name arg_forms
    | "butlast" | "take-last" | "drop-last" | "take-nth" ->
        compile_sequence_transform_call scope env name arg_forms
              | "next" | "nthnext" | "nthrest" | "ffirst" | "fnext" | "nfirst"
              | "nnext" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok args -> Core_sequence.compile env name args)
    | "rseq" -> (
        match compile_args () with
        | Error _ as err -> err
        | Ok [ ({ ty = (TList _ | TVector _); _ } as collection) ] ->
            Core_sequence.compile env name [ collection ]
        | Ok [ _ ] ->
            compile_protocol_call scope env "IReversible/-rseq" arg_forms
        | Ok _ -> Error.error "rseq expects 1 arguments")
    | "some" -> compile_some scope env arg_forms
              | "split-at" ->
                  compile_sequence_transform_call scope env name arg_forms
    | "split-with" -> compile_split_with scope env arg_forms
    | "partition-by" -> compile_partition_by scope env arg_forms
    | "bounded-count" | "dorun" | "doall" ->
        compile_sequence_transform_call scope env name arg_forms
    | "run!" -> compile_run_bang scope env arg_forms
    | "reverse" -> compile_collection_call scope env name arg_forms
    | "every?" | "not-any?" | "not-every?" ->
        compile_sequence_bool_predicate scope env name arg_forms
    | "map" -> compile_map_call scope env arg_forms
    | "keep" -> compile_keep scope env arg_forms
    | "filter" -> compile_filter scope env arg_forms
    | "distinct" -> compile_distinct scope env arg_forms
    | "remove" | "take-while" | "drop-while" | "dedupe" | "sort" ->
        compile_sequence_transform_call scope env name arg_forms
    | "sort-by" -> compile_sort_by scope env arg_forms
    | "group-by" -> compile_group_by scope env arg_forms
    | "concat" -> compile_concat scope env arg_forms
    | "mapcat" -> compile_mapcat scope env arg_forms
    | "vec" -> compile_vec scope env arg_forms
    | "set" -> compile_set scope env arg_forms
    | "repeat" ->
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
    | "apply" -> (
        match arg_forms with
        | FSymbol "mapv" :: FSymbol "vector" :: fixed_and_rest
          when List.length fixed_and_rest >= 2 ->
            let reversed = List.rev fixed_and_rest in
            let rest_form = List.hd reversed in
            let fixed_forms = List.rev (List.tl reversed) in
            compile_apply_zip_vectors scope env fixed_forms rest_form
        | _ -> compile_apply scope env arg_forms)
    | "comp" -> compile_comp scope env arg_forms
    | "partial" -> compile_partial scope env arg_forms
    | "identity" -> compile_identity scope env arg_forms
    | "constantly" -> compile_constantly scope env arg_forms
    | "complement" -> compile_complement scope env arg_forms
              | "every-pred" ->
                  compile_predicate_combinator scope env "every-pred" arg_forms
    | "some-fn" -> compile_some_fn scope env arg_forms
    | "juxt" -> compile_juxt scope env arg_forms
    | "distinct?" -> compile_distinct_question scope env arg_forms
    | "compare" -> compile_compare scope env arg_forms
    | "clojure.lang.Numbers/compare" -> (
        match compile_args () with
        | Error _ as error -> error
                  | Ok [ left; right ] -> (
            let dynamic_ty = Types.dynamic_constraint TUnknown in
                      match
               ( pack_dynamic_value env dynamic_ty left,
                 pack_dynamic_value env dynamic_ty right )
             with
            | (Error _ as error), _ | _, (Error _ as error) -> error
            | Ok left, Ok right ->
                Ok
                  (typed_ir TInt
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_dynamic.compare",
                          [ left; right ] ))))
                  | Ok _ ->
                      Error.error
                        "clojure.lang.Numbers/compare expects 2 arguments")
              | "max-key" | "min-key" ->
                  compile_key_extreme scope env name arg_forms
              | "hash-set" | "sorted-set" ->
                  compile_hash_set scope env arg_forms
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
              | _ -> compile_named_function_call scope env name arg_forms))
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
                |> Option.map (fun (parameter : Ocaml_signature.parameter) ->
                       parameter.ty)
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
                        adapt ((label, argument) :: adapted) expected_rest rest)
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
                        let argument_types =
                          List.map
                            (fun (label, argument) -> (label, argument.ty))
                            arguments
                        in
                        Result.map
                          (fun return_ty ->
                            typed_ir return_ty
                              (ocaml_apply function_name arguments))
                          (Ocaml_signature.result_after_application signature
                             argument_types)))))
  and compile_int_unary_call scope env name build_code arg_forms =
    match compile_args_for scope env arg_forms with
    | Error _ as err -> err
    | Ok [ arg ] when Types.is_dynamic arg.ty -> (
        match dynamic_unpack env TInt arg.semantic_expr with
        | Error _ as error -> error
        | Ok semantic_expr ->
            Core_int.compile_unary name
              [ { arg with ty = TInt; semantic_expr } ]
              build_code)
    | Ok args -> Core_int.compile_unary name args build_code
  and compile_boolean_call scope env name arg_forms =
    match compile_args_for scope env arg_forms with
    | Error _ as err -> err
    | Ok args -> Core_boolean.compile name args
  and compile_collection_call scope env name arg_forms =
    let argument_env =
      match (name, Env.expected_type env) with
      | ("first" | "second" | "last"), Some expected ->
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
    | Ok args -> Core_collection.compile env name args
  and compile_concat scope env arg_forms =
    match compile_args_for scope env arg_forms with
    | Error _ as error -> error
    | Ok [] -> Error.error "concat expects at least 1 collection"
    | Ok collections -> (
        let rec collect sequences = function
          | [] -> Ok (List.rev sequences)
          | collection :: rest -> (
              match Collection_capability.to_seq_expr env collection with
              | Error _ -> Error.error "concat expects collections"
              | Ok (inner, sequence) ->
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
                  match Type_solver.unify [] left right with
                  | Error _ -> None
                  | Ok substitutions ->
                      Some
                        (Type_inference.refine_type
                           (Type_solver.apply substitutions left)
                           (Type_solver.apply substitutions right)))
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
                let rec materialize_unknown_storage = function
                  | TUnknown -> Types.dynamic_constraint TUnknown
                  | TNullable inner ->
                      TNullable (materialize_unknown_storage inner)
                  | ty -> ty
                in
                let adapt_sequence index (inner, sequence) =
                  if Types.equal common_type inner then Ok sequence
                  else
                    let item_name =
                      "__lg_concat_common_item_" ^ string_of_int index
                    in
                    let storage_ty = materialize_unknown_storage inner in
                    let item = typed_ir storage_ty (Semantic_ir.Ident item_name) in
                    let adapted =
                      match (common_type, storage_ty) with
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
                                ([ typed_item_pattern item_name storage_ty ], item);
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
                    typed_ir (TSeq common_type)
                      (Semantic_ir.Apply
                         ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.concat",
                           [ Semantic_ir.List sequences ] )))
                  (adapt_sequences 0 [] sequences)
            | None
              when List.exists (fun (ty, _) -> Types.is_dynamic ty) sequences
              ->
              let dynamic_ty = Types.dynamic_constraint TUnknown in
              let rec pack_sequences index packed = function
                | [] -> Ok (List.rev packed)
                | (inner, sequence) :: rest when Types.is_dynamic inner ->
                    pack_sequences (index + 1) (sequence :: packed) rest
                | (inner, sequence) :: rest -> (
                    let item_name = "__lg_concat_item_" ^ string_of_int index in
                    let item = typed_ir inner (Semantic_ir.Ident item_name) in
                    match pack_dynamic_value env dynamic_ty item with
                    | Error _ as error -> error
                    | Ok item ->
                        let sequence =
                          Semantic_ir.Apply
                            ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                              [
                                Semantic_ir.Fun
                                  ([ Semantic_ir.PVar item_name ], item);
                                sequence;
                              ] )
                        in
                        pack_sequences (index + 1) (sequence :: packed) rest)
              in
              Result.map
                (fun sequences ->
                  typed_ir (TSeq dynamic_ty)
                    (Semantic_ir.Apply
                       ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.concat",
                         [ Semantic_ir.List sequences ] )))
                (pack_sequences 0 [] sequences)
            | None -> Error.error "concat element types must match")
  and compile_metadata_call scope env name arg_forms =
    let dynamic_ty = Types.dynamic_constraint TUnknown in
    match (name, arg_forms) with
    | "with-meta", [ value_form; metadata_form ] -> (
        match
          ( compile_expr scope env value_form,
            compile_expr scope env metadata_form )
        with
        | (Error _ as error), _ -> error
        | _, (Error _ as error) -> error
        | Ok value, Ok metadata -> (
            match
              ( pack_dynamic_value env dynamic_ty value,
                pack_dynamic_value env dynamic_ty metadata )
            with
            | (Error _ as error), _ -> error
            | _, (Error _ as error) -> error
            | Ok value, Ok metadata ->
                Ok
                  (typed_ir dynamic_ty
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_dynamic.with_metadata",
                          [ value; metadata ] )))))
    | "meta", [ value_form ] -> (
        match compile_expr scope env value_form with
        | Error _ as error -> error
        | Ok value -> (
            match pack_dynamic_value env dynamic_ty value with
            | Error _ as error -> error
            | Ok value ->
                Ok
                  (typed_ir dynamic_ty
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.metadata",
                          [ value ] )))))
    | "with-meta", _ -> Error.error "with-meta expects 2 arguments"
    | "meta", _ -> Error.error "meta expects 1 arguments"
    | _ -> assert false
  and compile_into scope env target_form source_form =
    match
      (compile_expr scope env target_form, compile_expr scope env source_form)
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
            Core_sequence_transform.compile "into" [ target; source ])
    | Ok target, Ok source -> (
        match Core_sequence_transform.compile "into" [ target; source ] with
        | Ok _ as result -> result
        | Error _ -> (
            match Collection_capability.to_seq_expr env source with
            | Error _ -> Error.error "into source must be a collection"
            | Ok (element_ty, sequence) ->
                let source =
                  typed_ir (TList element_ty)
                    (Semantic_ir.Apply
                       ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.to_list",
                         [ sequence ] ))
                in
                Core_sequence_transform.compile "into" [ target; source ]))
  and compile_sequence_transform_call scope env name arg_forms =
    match (name, arg_forms) with
    | ("take-while" | "drop-while"), [ fn_form ] -> (
        match compile_function_arg scope env fn_form with
        | Error _ as error -> error
        | Ok ({ ty = TFn ([ parameter_ty ], return_ty); _ } as fn) ->
            let item_name = "__lg_transducer_item" in
            let predicate =
              if Types.equal return_ty TBool then fn.semantic_expr
              else
                Semantic_ir.Fun
                  ( [ Semantic_ir.PVar item_name ],
                    truthiness_expression return_ty
                      (Semantic_ir.Apply
                         (fn.semantic_expr, [ Semantic_ir.Ident item_name ])) )
            in
            let sequence_name = "__lg_transducer_sequence" in
            Ok
              (typed_ir
                 (TFn ([ TSeq parameter_ty ], TSeq parameter_ty))
                 (Semantic_ir.Fun
                    ( [ Semantic_ir.PVar sequence_name ],
                      Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            ("Lg_runtime.Runtime_seq."
                            ^ if name = "take-while" then "take_while"
                              else "drop_while"),
                          [ predicate; Semantic_ir.Ident sequence_name ] ) )))
        | Ok _ -> Error.error (name ^ " expects a unary function"))
    | "interleave", collection_forms -> (
        match compile_args_for scope env collection_forms with
        | Error _ as error -> error
        | Ok collections when List.length collections < 2 ->
            Core_sequence_transform.compile name collections
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
                let needs_dynamic =
                  List.exists
                    (fun (element_type, _) ->
                      Types.is_dynamic element_type
                      || match element_type with
                         | TUnknown | TVar _ -> true
                         | _ -> false)
                    prepared
                in
                let common_type =
                  if needs_dynamic then Types.dynamic_constraint TUnknown
                  else fst (List.hd prepared)
                in
                if
                  (not needs_dynamic)
                  && List.exists
                       (fun (element_type, _) ->
                         not (Types.equal common_type element_type))
                       prepared
                then Error.error "interleave element types must match"
                else
                  let rec prepare_collections collections = function
                    | [] -> Ok (List.rev collections)
                    | (element_type, sequence) :: rest ->
                        let sequence =
                          if
                            needs_dynamic
                            && not (Types.is_dynamic element_type)
                            &&
                            match element_type with
                            | TUnknown | TVar _ -> false
                            | _ -> true
                          then
                            let item_name = "__lg_interleave_item" in
                            let item =
                              typed_ir element_type
                                (Semantic_ir.Ident item_name)
                            in
                            Result.map
                              (fun packed ->
                                Semantic_ir.Apply
                                  ( Semantic_ir.Ident
                                      "Lg_runtime.Runtime_seq.map",
                                    [
                                      Semantic_ir.Fun
                                        ([ Semantic_ir.PVar item_name ], packed);
                                      sequence;
                                    ] ))
                              (pack_dynamic_value env common_type item)
                          else Ok sequence
                        in
                        Result.bind sequence (fun sequence ->
                            prepare_collections
                              (sequence :: collections) rest)
                  in
                  Result.map
                    (fun sequences ->
                      typed_ir (TSeq common_type)
                        (Semantic_ir.Apply
                           ( Semantic_ir.Ident
                               "Lg_runtime.Runtime_seq.interleave",
                             [ Semantic_ir.List sequences ] )))
                    (prepare_collections [] prepared)))
    | ("partition" | "partition-all"), FInt size :: _ when size <= 0 ->
        Error.error (name ^ " size must be positive")
    | "take-nth", FInt count :: _ when count <= 0 ->
        Error.error "take-nth n must be positive"
    | "sort", [ comparator_form; collection_form ] -> (
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
                        typed_ir (TList inner)
                          (Semantic_ir.Apply
                             ( Semantic_ir.Ident "List.sort",
                               [
                                 comparator_expression;
                                 Semantic_ir.Apply
                                   ( Semantic_ir.Ident
                                       "Lg_runtime.Runtime_seq.to_list",
                                     [ sequence ] );
                               ] )))
                      comparator_expression
                | Ok _ -> Error.error "sort expects a comparator function")))
    | "sort", [ collection_form ] -> (
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
    | ("remove" | "take-while" | "drop-while"), [ fn_form; collection_form ]
      -> (
        match
          ( compile_function_arg scope env fn_form,
            compile_expr scope env collection_form )
        with
        | (Error _ as err), _ -> err
        | _, (Error _ as err) -> err
        | Ok fn, Ok collection -> (
            let remove_nils =
              if
                name = "remove"
                && match fn_form with
                   | FSymbol ("nil?" | "clojure.core/nil?") -> true
                   | _ -> false
              then
                match Collection_capability.to_seq_expr env collection with
                | Ok
                    ( (TNullable inner | TOcaml_app ("option", [ inner ])),
                      sequence ) ->
                    Some
                      (typed_ir (TSeq inner)
                         (Semantic_ir.Apply
                            ( Semantic_ir.Ident
                                "Lg_runtime.Runtime_seq.filter_map",
                              [
                                Semantic_ir.Fun
                                  ( [ Semantic_ir.PVar "item" ],
                                    Semantic_ir.Ident "item" );
                                sequence;
                              ] )))
                | Ok _ | Error _ -> None
              else None
            in
            match remove_nils with
            | Some result -> Ok result
            | None ->
            let adapted_fn =
              match
                (fn.ty, Collection_capability.element_type env collection)
              with
              | TFn ([ parameter_ty ], return_ty), Some ((TUnknown | TVar _) as element_ty)
                when not (Types.equal parameter_ty TUnknown)
                     &&
                     (match parameter_ty with TVar _ -> false | _ -> true) ->
                  let item_name = "__lg_unknown_predicate_item" in
                  Ok
                    (typed_ir (TFn ([ element_ty ], return_ty))
                       (Semantic_ir.Fun
                          ( [ typed_item_pattern item_name parameter_ty ],
                            Semantic_ir.Apply
                              ( fn.semantic_expr,
                                [ Semantic_ir.Ident item_name ] ) )))
              | TFn ([ parameter_ty ], return_ty), Some element_ty
                when Types.is_dynamic parameter_ty
                     && not (Types.is_dynamic element_ty) ->
                  let item_name = "__lg_static_predicate_item" in
                  let item =
                    typed_ir element_ty (Semantic_ir.Ident item_name)
                  in
                  Result.map
                    (fun argument ->
                      typed_ir (TFn ([ element_ty ], return_ty))
                        (Semantic_ir.Fun
                           ( [ Semantic_ir.PVar item_name ],
                             Semantic_ir.Apply
                               (fn.semantic_expr, [ argument ]) )))
                    (if dynamic_protocol_constraints parameter_ty = [] then
                       pack_dynamic_payload env parameter_ty item
                     else pack_dynamic_value env parameter_ty item)
              | TFn ([ parameter_ty ], return_ty), Some element_ty
                when Types.is_dynamic element_ty
                     && not (Types.is_dynamic parameter_ty) ->
                  let item_name = "__lg_dynamic_predicate_item" in
                  Result.map
                    (fun argument ->
                      typed_ir (TFn ([ element_ty ], return_ty))
                        (Semantic_ir.Fun
                           ( [ Semantic_ir.PVar item_name ],
                             Semantic_ir.Apply
                               (fn.semantic_expr, [ argument ]) )))
                    (dynamic_unpack env parameter_ty
                       (Semantic_ir.Ident item_name))
              | callable_ty, Some element_ty
                when Types.is_dynamic callable_ty ->
                  let callable_name = "__lg_dynamic_predicate" in
                  let item_name = "__lg_dynamic_predicate_item" in
                  let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
                  Result.map
                    (fun item ->
                      typed_ir (TFn ([ element_ty ], callable_ty))
                        (Semantic_ir.Let
                           ( [
                               ( Semantic_ir.PVar callable_name,
                                 fn.semantic_expr );
                             ],
                             Semantic_ir.Fun
                               ( [ Semantic_ir.PVar item_name ],
                                 Semantic_ir.Apply
                                   ( Semantic_ir.Ident
                                       "Lg_runtime.Runtime_dynamic.call",
                                     [
                                       Semantic_ir.Ident callable_name;
                                       Semantic_ir.List [ item ];
                                     ] ) ) )))
                    (pack_dynamic_value env callable_ty item)
              | _ -> Ok fn
            in
            Result.bind adapted_fn (fun fn ->
                let fn =
                  match fn.ty with
                  | TFn ([ parameter_ty ], return_ty)
                    when not (Types.equal return_ty TBool) ->
                      let item_name = "__lg_truthy_predicate_item" in
                      typed_ir (TFn ([ parameter_ty ], TBool))
                        (Semantic_ir.Fun
                           ( [ Semantic_ir.PVar item_name ],
                             truthiness_expression return_ty
                               (Semantic_ir.Apply
                                  ( fn.semantic_expr,
                                    [ Semantic_ir.Ident item_name ] )) ))
                  | _ -> fn
                in
                match Core_sequence_transform.compile name [ fn; collection ] with
                | Ok _ as result -> result
                | Error _ -> (
                    match Collection_capability.to_seq_expr env collection with
                    | Error _ ->
                        Error.error
                          (name ^ " expects a seqable value, got "
                          ^ Types.source_name collection.ty)
                    | Ok (inner, sequence) ->
                        let collection =
                          typed_ir (TList inner)
                            (Semantic_ir.Apply
                               ( Semantic_ir.Ident
                                   "Lg_runtime.Runtime_seq.to_list",
                                 [ sequence ] ))
                        in
                        Core_sequence_transform.compile name [ fn; collection ]))
            )
        )
    | _ -> (
        match compile_args_for scope env arg_forms with
        | Error _ as err -> err
        | Ok args -> Core_sequence_transform.compile name args)
  and compile_vec scope env arg_forms =
    match compile_args_for scope env arg_forms with
    | Error _ as error -> error
    | Ok [ collection ]
      when Types.equal collection.ty TUnknown
           || match collection.ty with TVar _ -> true | _ -> false ->
        let dynamic = Types.dynamic_constraint TUnknown in
        Result.map
          (fun collection ->
            typed_ir (TVector dynamic)
              (Semantic_ir.Apply
                 ( Semantic_ir.Ident "Rrbvec.of_list",
                   [
                     Semantic_ir.Apply
                       ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.to_list",
                         [
                           Semantic_ir.Apply
                             ( Semantic_ir.Ident
                                 "Lg_runtime.Runtime_dynamic.to_seq",
                               [ collection ] );
                         ] );
                   ] )))
          (pack_dynamic_value env dynamic collection)
    | Ok [ collection ] -> (
        match Collection_capability.to_seq_expr env collection with
        | Error _ ->
            Error.error
              ("vec expects a seqable value, got "
             ^ Types.source_name collection.ty)
        | Ok (inner, sequence) ->
            Ok
              (typed_ir (TVector inner)
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Rrbvec.of_list",
                      [
                        Semantic_ir.Apply
                          ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.to_list",
                            [ sequence ] );
                      ] ))))
    | Ok _ -> Error.error "vec expects 1 arguments"
  and compile_distinct scope env arg_forms =
    match compile_args_for scope env arg_forms with
    | Error _ as error -> error
    | Ok [ collection ] -> (
        match Collection_capability.to_seq_expr env collection with
        | Error _ ->
            Error.error
              ("distinct expects a seqable value, got "
             ^ Types.source_name collection.ty)
        | Ok (inner, sequence) -> (
            let left = typed_ir inner (Semantic_ir.Ident "left") in
            let right = typed_ir inner (Semantic_ir.Ident "right") in
            match Core_compare.compile "=" [ left; right ] with
            | Error _ as error -> error
            | Ok equality ->
                let equal =
                  Semantic_ir.Fun
                    ( [ Semantic_ir.PVar "left"; Semantic_ir.PVar "right" ],
                      equality.semantic_expr )
                in
                Ok
                  (typed_ir (TSeq inner)
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.distinct",
                          [ equal; sequence ] )))))
    | Ok _ -> Error.error "distinct expects 1 arguments"
  and compile_set scope env arg_forms =
    match compile_args_for scope env arg_forms with
    | Error _ as error -> error
    | Ok [ collection ] -> (
        match Collection_capability.to_seq_expr env collection with
        | Error _ -> Error.error "set expects a seqable value"
        | Ok (inner, sequence) when Types.is_dynamic inner -> (
            let item_name = "__lg_set_item" in
            let item = typed_ir inner (Semantic_ir.Ident item_name) in
            match
               pack_dynamic_value env (Types.dynamic_constraint TUnknown) item
             with
            | Error _ as error -> error
            | Ok item ->
                Ok
                  (typed_ir
                     (Types.dynamic_constraint TUnknown)
                     (Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.set",
                          [
                            Semantic_ir.Apply
                              ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                                [
                                  Semantic_ir.Fun
                                    ([ Semantic_ir.PVar item_name ], item);
                                  sequence;
                                ] );
                          ] ))))
        | Ok (inner, sequence) ->
            Core_sequence_transform.compile "set"
              [
                typed_ir (TList inner)
                  (Semantic_ir.Apply
                     (Semantic_ir.Ident "List.of_seq", [ sequence ]));
              ])
    | Ok _ -> Error.error "set expects 1 arguments"
  and compile_cons scope env arg_forms =
    match compile_args_for scope env arg_forms with
    | Error _ as error -> error
    | Ok [ value; collection ] when Types.is_dynamic collection.ty -> (
        let dynamic_ty = Types.dynamic_constraint TUnknown in
        match pack_dynamic_value env dynamic_ty value with
        | Error _ as error -> error
        | Ok value ->
            Ok
              (typed_ir dynamic_ty
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.cons",
                      [ value; collection.semantic_expr ] ))))
    | Ok [ _; _ ] -> compile_static_cons scope env arg_forms
    | Ok _ -> Error.error "cons expects a value and seqable collection"
  and compile_conj scope env arg_forms =
    match compile_args_for scope env arg_forms with
    | Error _ as error -> error
    | Ok (collection :: values)
      when values <> [] && Types.is_dynamic collection.ty ->
        let dynamic_ty = Types.dynamic_constraint TUnknown in
        let rec add_values expression = function
          | [] -> Ok (typed_ir dynamic_ty expression)
          | value :: rest -> (
              match pack_dynamic_value env dynamic_ty value with
              | Error _ as error -> error
              | Ok value ->
                  add_values
                    (Semantic_ir.Apply
                       ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.conj",
                         [ expression; value ] ))
                    rest)
        in
        add_values collection.semantic_expr values
    | Ok (_ :: _ :: _) -> compile_static_conj scope env arg_forms
    | Ok _ -> Error.error "conj expects collection and values"

  and compile_not_empty scope env arg_forms =
    match compile_args_for scope env arg_forms with
    | Error _ as error -> error
    | Ok [ collection ] ->
        let value_name = "__lg_not_empty_value" in
        let value =
          typed_ir collection.ty (Semantic_ir.Ident value_name)
        in
        (match Collection_capability.to_seq_expr env value with
        | Error _ -> Error.error "not-empty expects a seqable value"
        | Ok (_, sequence) ->
            let result_ty, empty, present =
              match collection.ty with
              | TNil ->
                  (TNil, Semantic_ir.Constructor ("None", None),
                   Semantic_ir.Constructor ("None", None))
              | TNullable _ | TOcaml_app ("option", [ _ ]) | TOcaml "option" ->
                  ( collection.ty,
                    Semantic_ir.Constructor ("None", None),
                    Semantic_ir.Ident value_name )
              | ty ->
                  ( TNullable ty,
                    Semantic_ir.Constructor ("None", None),
                    Semantic_ir.Constructor
                      ("Some", Some (Semantic_ir.Ident value_name)) )
            in
            Ok
              (typed_ir result_ty
                 (Semantic_ir.Let
                    ( [ (Semantic_ir.PVar value_name, collection.semantic_expr) ],
                      Semantic_ir.If
                        ( Semantic_ir.Apply
                            ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.is_empty",
                              [ sequence ] ),
                          empty,
                          present ) ))))
    | Ok _ -> Error.error "not-empty expects 1 arguments"
  and compile_some_fn scope env function_forms =
    match function_forms with
    | [] -> Error.error "some-fn expects at least 1 function"
    | _ ->
        let argument_name = "__lg_some_fn_value" in
        let bindings, calls =
          function_forms
          |> List.mapi (fun index function_form ->
                 let name = "__lg_some_fn_" ^ string_of_int index in
                 ( [ FSymbol name; function_form ],
                   FList [ FSymbol name; FSymbol argument_name ] ))
          |> List.split
        in
        compile_expr scope (Env.with_expected_type None env)
          (FList
             [
               FSymbol "let";
               FVector (List.concat bindings);
               FList
                 [
                   FSymbol "fn";
                   FVector [ FSymbol argument_name ];
                   FList (FSymbol "or" :: calls);
                 ];
             ])
  and compile_group_by scope env arg_forms =
    match arg_forms with
    | [ function_form; collection_form ] -> (
        match compile_expr scope env collection_form with
        | Error _ as error -> error
        | Ok collection -> (
            match Collection_capability.to_seq_expr env collection with
            | Error _ -> Error.error "group-by expects a seqable value"
            | Ok (inner, sequence) -> (
                let item_name = "__lg_group_by_item" in
                let key_function =
                  match function_form with
                  | FKeyword keyword ->
                      let function_env =
                        Env.add
                          (Names.scoped_key scope item_name)
                          (Types.binding item_name inner)
                          env
                      in
                      compile_expr scope function_env
                        (FList [ FKeyword keyword; FSymbol item_name ])
                          |> Result.map (fun body ->
                          typed_ir
                            (TFn ([ inner ], body.ty))
                               (Semantic_ir.Fun
                                  ( [ constrained_identifier_pattern item_name
                                        inner ],
                                    body.semantic_expr )))
                  | form -> compile_function_arg scope env form
                in
                match key_function with
                | Error _ as error -> error
                | Ok
                    ({ ty = TFn ([ parameter_ty ], key_ty); _ } as key_function)
                  when Types.assignable ~policy:Host_boundary
                         ~expected:parameter_ty ~actual:inner -> (
                    let item_ty =
                      match inner with
                      | TUnknown | TVar _ -> parameter_ty
                      | ty -> ty
                    in
                    let item =
                      typed_ir item_ty (Semantic_ir.Ident item_name)
                    in
                    let key_name = "__lg_group_by_key" in
                    let key = typed_ir key_ty (Semantic_ir.Ident key_name) in
                    let dynamic_ty = Types.dynamic_constraint TUnknown in
                    let adapted_key_function =
                      if Types.is_dynamic parameter_ty
                         && not (Types.is_dynamic item_ty)
                      then
                        Result.map
                          (fun packed_item ->
                            Semantic_ir.Fun
                              ( [ constrained_identifier_pattern item_name
                                    item_ty ],
                                Semantic_ir.Apply
                                  ( key_function.semantic_expr,
                                    [ packed_item ] ) ))
                          (pack_dynamic_value env parameter_ty item)
                      else Ok key_function.semantic_expr
                    in
                    match
                       ( adapted_key_function,
                         pack_dynamic_value env dynamic_ty key,
                         pack_dynamic_value env dynamic_ty item )
                     with
                    | (Error _ as error), _, _ -> error
                    | _, (Error _ as error), _ -> error
                    | _, _, (Error _ as error) -> error
                    | Ok adapted_key_function, Ok packed_key, Ok packed_item ->
                        Ok
                          (typed_ir dynamic_ty
                             (Semantic_ir.Apply
                                ( Semantic_ir.Ident
                                    "Lg_runtime.Runtime_dynamic.group_by",
                                  [
                                    adapted_key_function;
                                    Semantic_ir.Fun
                                      ([ Semantic_ir.PVar key_name ], packed_key);
                                    Semantic_ir.Fun
                                      ( [ constrained_identifier_pattern
                                            item_name item_ty ],
                                        packed_item );
                                    sequence;
                                  ] ))))
                | Ok { ty = TFn _; _ } ->
                    Error.error
                      "group-by function type does not match collection"
                | Ok _ -> Error.error "group-by expects a function")))
    | _ -> Error.error "group-by expects function and collection"
  and compile_get_in scope env arg_forms =
    match arg_forms with
    | [ target; FVector keys ] ->
        compile_expr scope env (Core_form_expansion.get_in target keys None)
    | [ target; FVector keys; default ] ->
        compile_expr scope env
          (Core_form_expansion.get_in target keys (Some default))
    | [ _; _ ] | [ _; _; _ ] ->
        Error.error "get-in currently requires a vector path"
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
    | target_form :: path_form :: function_form :: argument_forms -> (
        match
          ( compile_expr scope env target_form,
            compile_expr scope env path_form,
            compile_expr scope env function_form,
            compile_args_for scope env argument_forms )
        with
        | (Error _ as error), _, _, _ -> error
        | _, (Error _ as error), _, _ -> error
        | _, _, (Error _ as error), _ -> error
        | _, _, _, (Error _ as error) -> error
        | Ok target, Ok path, Ok function_, Ok arguments -> (
            match Collection_capability.to_seq_expr env path with
            | Error _ -> Error.error "update-in path must be seqable"
            | Ok (key_ty, path) -> (
                let dynamic_ty = Types.dynamic_constraint TUnknown in
                let key_name = "__lg_update_in_key" in
                let key = typed_ir key_ty (Semantic_ir.Ident key_name) in
                let pack value = pack_dynamic_value env dynamic_ty value in
                match
                   ( pack target,
                     pack function_,
                     pack key,
                     arguments
                     |> List.fold_left
                          (fun result argument ->
                            match result with
                            | Error _ as error -> error
                            | Ok packed ->
                                pack argument
                                |> Result.map (fun argument ->
                                       argument :: packed))
                          (Ok []) )
                 with
                | (Error _ as error), _, _, _ -> error
                | _, (Error _ as error), _, _ -> error
                | _, _, (Error _ as error), _ -> error
                | _, _, _, (Error _ as error) -> error
                | Ok target, Ok function_, Ok key, Ok arguments ->
                    let path =
                      Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                          [
                            Semantic_ir.Fun ([ Semantic_ir.PVar key_name ], key);
                            path;
                          ] )
                    in
                    Ok
                      (typed_ir dynamic_ty
                         (Semantic_ir.Apply
                            ( Semantic_ir.Ident
                                "Lg_runtime.Runtime_dynamic.update_in",
                              [
                                target;
                                path;
                                function_;
                                Semantic_ir.List (List.rev arguments);
                              ] ))))))
    | _ ->
        Error.error
          "update-in expects target, path, function, and optional arguments"
  and compile_subs scope env arg_forms =
    let prepare expected argument error =
      if Types.equal argument.ty expected then Ok argument.semantic_expr
      else if Types.is_dynamic argument.ty then
        dynamic_unpack env expected argument.semantic_expr
      else Error.error error
    in
    let substring source start stop =
      let source_name = "__lg_subs_source" in
      let source_value = Semantic_ir.Ident source_name in
      let length =
        match stop with
        | None ->
                        Semantic_ir.Infix
                          ( "-",
                            Semantic_ir.Apply
                  (Semantic_ir.Ident "String.length", [ source_value ]),
                start )
        | Some stop -> Semantic_ir.Infix ("-", stop, start)
      in
      Semantic_ir.Let
        ( [ (Semantic_ir.PVar source_name, source) ],
          Semantic_ir.Apply
            (Semantic_ir.Ident "String.sub", [ source_value; start; length ]) )
    in
    match compile_args_for scope env arg_forms with
    | Error _ as err -> err
    | Ok [ source; start ] ->
        Result.bind (prepare TString source "subs expects a string")
          (fun source ->
            Result.map
              (fun start -> typed_ir TString (substring source start None))
              (prepare TInt start "subs indexes must be int"))
    | Ok [ source; start; stop ] ->
        Result.bind (prepare TString source "subs expects a string")
          (fun source ->
            Result.bind (prepare TInt start "subs indexes must be int")
              (fun start ->
                Result.map
                  (fun stop ->
                    typed_ir TString (substring source start (Some stop)))
                  (prepare TInt stop "subs indexes must be int")))
    | Ok _ -> Error.error "subs expects string, start, and optional end"
  and compile_function_arg scope env form =
    let compiled =
      match form with
      | FSymbol name -> lookup_function scope env name
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
      | TString -> Some "^String"
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
    | Ok { host_reference = Some (Ocaml_value _); _ } ->
        compile_inferred_ocaml_call scope env name arg_forms
    | Ok fn -> (
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
        match compile_args_for scope env arg_forms with
        | Error _ as err -> err
        | Ok args -> (
            match fn.ty with
            | (TUnknown | TVar _)
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
                        || match ty with TVar _ -> true | _ -> false
                      then candidates
                      else if List.exists (Types.equal ty) candidates then
                        candidates
                      else ty :: candidates
                    in
                    let element_candidates =
                      List.fold_left2
                        (fun candidates expected argument ->
                          match Types.seqable_constraint_element expected with
                          | Some (TUnknown | TVar _) -> (
                              match
                                Collection_capability.element_type env argument
                              with
                              | Some element_ty ->
                                  add_element_candidate candidates element_ty
                              | None -> candidates)
                          | _ -> (
                              match (expected, argument.ty) with
                              | (TUnknown | TVar _), TFn (parameters, _) ->
                                  List.fold_left add_element_candidate candidates
                                    parameters
                              | (TUnknown | TVar _), actual ->
                                  add_element_candidate candidates actual
                              | _ -> candidates))
                        [] arity.fixed_params fixed_args
                    in
                    let element_ty =
                      match element_candidates with
                      | [ element_ty ] -> Some element_ty
                      | [] | _ :: _ :: _ -> None
                    in
                    let specialize_expected expected argument =
                      match expected with
                      | TOcaml_app (constraint_name, [ (TUnknown | TVar _); _ ])
                        when constraint_name = Types.seqable_constraint_name
                             || constraint_name
                                = Types.optional_seqable_constraint_name
                             || constraint_name
                                = Types.optional_sequential_constraint_name ->
                          (match element_ty with
                          | Some element_ty ->
                              TOcaml_app
                                (constraint_name, [ element_ty; argument.ty ])
                          | None -> expected)
                      | TUnknown | TVar _ -> (
                          match element_ty with
                          | Some _ -> argument.ty
                          | None -> expected)
                      | TNamed_record
                          ({ type_arguments = [ (TUnknown | TVar _) ]; _ } as
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
                        (fun substitutions template argument ->
                          let actual =
                            optional_payload argument.ty
                            |> Option.value ~default:argument.ty
                          in
                          if Types.is_dynamic actual then substitutions
                          else
                            Type_solver.unify substitutions template actual
                            |> Result.value ~default:substitutions)
                        [] arity.fixed_params fixed_args
                    in
                    let fixed_param_tys =
                      List.map2 specialize_expected arity.fixed_params fixed_args
                      |> List.map (Type_solver.apply substitutions)
                    in
                    let return_ty =
                      match element_ty with
                      | Some element_ty ->
                          let rec specialize = function
                            | TUnknown | TVar _ -> element_ty
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
                          specialize arity.return_ty
                      | None -> arity.return_ty
                    in
                    let return_ty =
                      Type_solver.apply substitutions return_ty
                    in
                    let row_param_types =
                      List.nth_opt fn.overload_row_param_types arity_index
                      |> Option.value ~default:[]
                    in
                    let fixed_compatible =
                      List.for_all2
                        (fun expected arg ->
                          match expected with
                          | TNullable (TRecord fields) ->
                              row_argument_compatible fields arg.ty
                          | _ -> argument_compatible expected arg.ty)
                        fixed_param_tys fixed_args
                    in
                    let rest_compatible =
                      match arity.rest_param with
                      | None -> extra_args = []
                      | Some expected ->
                          List.for_all
                            (fun arg -> argument_compatible expected arg.ty)
                            extra_args
                    in
                    if not (fixed_compatible && rest_compatible) then
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
                                   | _ -> argument_compatible expected arg.ty)
                                 fixed_param_tys fixed_args)
                           |> List.filter_map Fun.id))
                    else
                      let prepare_argument index expected argument =
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
                            typed_nullable_row_argument env type_name fields
                              argument
                        | Some type_name, TRecord fields
                          when Types.is_dynamic argument.ty ->
                            dynamic_row_argument env type_name fields argument
                        | Some type_name, TRecord fields ->
                            typed_row_argument env type_name fields argument
                        | _ when has_capability_constraint expected ->
                          pack_constrained_value ?row_type_name env expected
                            argument
                        | _
                          when match argument.ty with
                               | TNullable actual
                               | TOcaml_app ("option", [ actual ]) ->
                                   argument_compatible expected actual
                               | _ -> false ->
                            adapt_value_to_type env expected argument
                        | _ when expects_optional_dynamic_value expected ->
                          Ok
                            (coerce_expression_to_type expected argument.ty
                               argument.semantic_expr)
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
                          | _ -> Ok argument.semantic_expr
                      in
                      let rec prepare_arguments index prepared expected arguments =
                        match (expected, arguments) with
                        | [], [] -> Ok (List.rev prepared)
                        | expected_ty :: expected_rest, argument :: arguments
                          -> (
                            match prepare_argument index expected_ty argument with
                            | Error _ as error -> error
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
                            | Some expected ->
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
                              Ok
                                (typed_ir return_ty
                                   (Semantic_ir.Apply (target, arguments))))))
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
                let substitutions = [] in
                let substitutions =
                  List.fold_left2
                    (fun substitutions template actual ->
                      let evidence =
                        if Types.is_dynamic actual then
                          Types.dynamic_constraint_info actual
                          |> Option.value ~default:TUnknown
                        else actual
                      in
                      if Types.equal evidence TUnknown then substitutions
                      else
                        match Type_solver.unify substitutions template evidence with
                        | Ok substitutions -> substitutions
                        | Error _ when dynamic_callable ->
                            List.fold_left
                              (fun substitutions variable ->
                                Type_solver.force substitutions variable
                                  (Types.dynamic_constraint TUnknown))
                              substitutions (Type_solver.variables template)
                        | Error _ -> substitutions)
                    substitutions param_tys actual_tys
                in
                let substitutions =
                  List.fold_left2
                    (fun substitutions template argument ->
                      match
                        ( Types.seqable_constraint_element template,
                          Collection_capability.element_type env argument )
                      with
                      | Some (TVar variable), Some actual_element ->
                          Type_solver.force substitutions variable
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
                                | TUnknown | TVar _ -> actual
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
                  | TOcaml_app (name, [ (TUnknown | TVar _); value_ty ])
                    when name = Types.seqable_constraint_name
                         || name = Types.optional_seqable_constraint_name
                         || name = Types.optional_sequential_constraint_name ->
                      let actual_element =
                        Collection_capability.element_type env argument
                      in
                      let element_ty =
                        match actual_element with
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
                                    (Types.dynamic_constraint TUnknown))
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
                                | TUnknown | TVar _ -> actual
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
                      | TSet (TUnknown | TVar _) -> storage_ty
                      | _ -> instantiated_ty)
                    storage_param_tys param_tys
                in
                let storage_ret_template =
                  Types.maybe_reduced_callback_element ret
                  |> Option.value ~default:ret
                in
                let erased_callback_storage_call =
                  List.exists
                    (fun argument ->
                      Types.is_dynamic argument.ty
                      || has_capability_constraint argument.ty)
                    args
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
                let ret =
                  match (ret, Env.expected_type env) with
                  | (TUnknown | TVar _), Some expected -> expected
                  | _ -> ret
                in
                let storage_sequence_elements =
                  List.filter_map Types.seqable_constraint_element
                    storage_param_tys
                  |> List.map Type_inference.materialize_dynamic_unknown
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
                            List.nth_opt storage_param_tys index )
                        with
                        | TFn (expected_params, expected_return), storage_ty
                          when erased_callback_storage_call ->
                            let storage_params =
                              match storage_ty with
                              | Some (TFn (storage_params, _)) ->
                                  List.map
                                    Type_inference.materialize_dynamic_unknown
                                    storage_params
                              | Some _ | None ->
                                  List.map
                                    Type_inference.materialize_dynamic_unknown
                                    expected_params
                            in
                            let storage_params =
                              if
                                storage_sequence_elements <> []
                                && List.length storage_sequence_elements
                                   = List.length storage_params
                              then storage_sequence_elements
                              else storage_params
                            in
                            TFn
                              (storage_params, expected_return)
                        | _ -> expected_ty
                      in
                      let row_type_name =
                        List.nth_opt fn.row_param_types index |> Option.join
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
                            | Some type_name, TNullable (TRecord fields) ->
                                typed_nullable_row_argument env type_name fields
                                  arg
                            | Some type_name, TRecord fields
                              when Types.is_dynamic arg.ty ->
                                dynamic_row_argument env type_name fields arg
                            | Some type_name, TRecord fields ->
                                typed_row_argument env type_name fields arg
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
                                      | TUnknown | TVar _ -> true
                                 | _ -> false ->
                                dynamic_unpack env (TNamed_record record)
                                  arg.semantic_expr
                          | _ when expects_optional_dynamic_value expected_ty ->
                              Ok
                                (coerce_expression_to_type expected_ty arg.ty
                                   arg.semantic_expr)
                            | _
                              when Types.is_dynamic expected_ty
                                   && not (Types.is_dynamic arg.ty) ->
                                pack_dynamic_value env expected_ty arg
                            | _
                              when Types.is_dynamic arg.ty
                                   && not (expects_dynamic_value expected_ty) ->
                              dynamic_unpack env expected_ty arg.semantic_expr
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
                            | _, TSet (TUnknown | TVar _)
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
                                    | TFn (_, TBool), TFn (_, actual_return)
                                      when expects_dynamic_value actual_return ->
                                      Ok
                                        (adapt_truthy_callback
                                           callback_expected_ty arg)
                                  | TFn (_, TNullable _), TFn (_, _) ->
                                      adapt_nullable_callback env
                                        callback_expected_ty arg
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
                let ret =
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
                  match (ret, callback_return) with
                      | TNullable (TVector (TUnknown | TVar _)), Some return_ty
                        ->
                      TNullable (TVector return_ty)
                  | TVector (TUnknown | TVar _), Some return_ty ->
                      TVector return_ty
                  | _ -> ret
                in
                let ret =
                  match (fn.return_param_index, ret) with
                  | Some index, ret
                    when Types.equal ret TUnknown || Types.is_dynamic ret
                         || match ret with TVar _ -> true | _ -> false -> (
                      match List.nth_opt args index with
                      | Some arg -> arg.ty
                      | None -> ret)
                  | _ -> ret
                in
                let ret =
                  match ret with
                  | TNullable (TUnknown | TVar _) ->
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
                  | TSeq (TUnknown | TVar _)
                  | TNullable (TUnknown | TVar _) ->
                      List.exists
                        (fun parameter_ty ->
                          Option.is_some
                            (Types.seqable_constraint_element parameter_ty))
                        param_tys
                  | _ -> false
                in
                let storage_ret =
                  if sequence_storage_follows_adapter then ret
                  else if
                    erased_storage_call
                    && runtime_dynamic_call
                    && Option.is_none fn.return_param_index
                  then
                    Type_inference.materialize_dynamic_unknown
                      storage_ret_template
                  else ret
                in
                let call =
                      Semantic_ir.Apply
                        (Semantic_ir.Ident fn.ocaml_name, arg_exprs)
                in
                let call =
                  if
                    Types.equal storage_ret ret
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
                    let caught_value = "__lg_reduced_value" in
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
                                          Some (Semantic_ir.PVar caught_value)
                                        ),
                              Semantic_ir.Apply
                                ( Semantic_ir.Ident
                                    "Lg_runtime.Runtime_reduced.reduced",
                                          [ Semantic_ir.Ident caught_value ] )
                                    );
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
                let dynamic_ty = Types.dynamic_constraint TUnknown in
                let rec pack_arguments packed = function
                  | [] -> Ok (List.rev packed)
                  | argument :: rest -> (
                      match pack_dynamic_value env dynamic_ty argument with
                      | Error _ as error -> error
                      | Ok argument -> pack_arguments (argument :: packed) rest)
                in
                Result.map
                  (fun arguments ->
                    typed_ir dynamic_ty
                      (Semantic_ir.Apply
                         ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.call",
                           [
                             Semantic_ir.Ident fn.ocaml_name;
                             Semantic_ir.List arguments;
                           ] )))
                  (pack_arguments [] args)
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
                       || Types.is_dynamic element_ty ->
                    Result.bind
                      (adapt_value_to_type env element_ty arg)
                      (fun argument ->
                        Result.map
                      (fun set_module ->
                        let present =
                          Semantic_ir.Apply
                            ( Semantic_ir.Ident (set_module ^ ".mem"),
                              [
                                argument;
                                Semantic_ir.Ident fn.ocaml_name;
                              ] )
                        in
                        typed_ir
                          (TOcaml_app ("option", [ element_ty ]))
                          (Semantic_ir.If
                             ( present,
                               Semantic_ir.Constructor
                                 ("Some", Some argument),
                               Semantic_ir.Constructor ("None", None) )))
                      (Types.set_module_name element_ty))
                | [ _ ] ->
                    Error.error (name ^ " called with incompatible arguments")
                | _ -> Error.error (name ^ " expects 1 arguments"))
            | TFn _ ->
                Error.error (name ^ " called with incompatible arguments")
            | _ -> Error.error (name ^ " is not callable")))
  and compile_protocol_call scope env name arg_forms =
    let contextual_return_ty ty =
      match (ty, Env.expected_type env) with
      | TUnknown, Some expected -> expected
      | ty, Some expected when contains_unresolved_type ty ->
          let template =
            match (optional_payload ty, optional_payload expected) with
            | Some payload, None -> payload
            | _ -> ty
          in
          Type_solver.unify [] template expected
          |> Result.map (fun substitutions -> Type_solver.apply substitutions ty)
          |> Result.value ~default:ty
      | _ -> ty
    in
    if Protocol.method_is_ambiguous scope env name then
      Error.error ("ambiguous protocol method " ^ name ^ "; use Protocol/method")
    else
      match Protocol.lookup_marker scope env name with
      | None -> Error.error ("unknown function " ^ name)
      | Some marker -> (
          let argument_env = Env.with_expected_type None env in
          match compile_args_for scope argument_env arg_forms with
          | Error _ as err -> err
          | Ok args -> (
              let args =
                match args with
                | receiver :: rest -> (
                    match receiver.ty with
                    | TNullable inner | TOcaml_app ("option", [ inner ]) ->
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
                        typed_ir receiver_ty
                          (Semantic_ir.Apply
                             ( Semantic_ir.Ident "Option.get",
                               [ receiver.semantic_expr ] ))
                        :: rest
                    | _ -> args)
                | [] -> []
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
                      | receiver_ty when Types.is_dynamic receiver_ty -> (
                          match marker.protocol_id with
                          | None ->
                              Error.error
                                ("unknown protocol for method " ^ method_name)
                          | Some protocol_id -> (
                              let rec pack_arguments packed = function
                                | [] -> Ok (List.rev packed)
                                | argument :: rest -> (
                                    match
                                      pack_dynamic_value env receiver_ty
                                        argument
                                    with
                                    | Error _ as error -> error
                                    | Ok argument ->
                                        pack_arguments (argument :: packed) rest
                                    )
                              in
                              match pack_arguments [] (List.tl args) with
                              | Error _ as error -> error
                              | Ok arguments ->
                                  let dynamic_result =
                                    Semantic_ir.Apply
                                      ( Semantic_ir.Ident
                                          "Lg_runtime.Runtime_dynamic.invoke",
                                        [
                                          receiver.semantic_expr;
                                          Semantic_ir.String
                                            (Protocol_id.to_string protocol_id);
                                          Semantic_ir.String method_name;
                                          Semantic_ir.List arguments;
                                        ] )
                                  in
                                  let return_ty =
                                    match marker.ty with
                                    | TFn (_, TUnknown) -> (
                                        match
                                          Protocol
                                          .common_method_return_param_index env
                                            protocol_id method_name
                                         with
                                        | Some index -> (
                                            match List.nth_opt args index with
                                            | Some argument -> argument.ty
                                            | None ->
                                                Types.dynamic_constraint
                                                  TUnknown)
                                        | None ->
                                            Protocol.common_method_return env
                                              protocol_id method_name
                                            |> Option.value
                                                 ~default:
                                                   (Types.dynamic_constraint
                                                      TUnknown))
                                    | TFn (_, return_ty) -> return_ty
                                    | _ -> TUnknown
                                  in
                                  let return_ty =
                                    contextual_return_ty return_ty
                                  in
                                  dynamic_unpack env return_ty dynamic_result
                                  |> Result.map (typed_ir return_ty)))
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
                                  let return_ty =
                                    match marker.ty with
                                    | TFn (_, TUnknown) -> (
                                        match
                                          Protocol
                                          .common_method_return_param_index env
                                            protocol_id method_name
                                         with
                                        | Some index -> (
                                            match List.nth_opt args index with
                                            | Some argument -> argument.ty
                                            | None ->
                                                Types.dynamic_constraint
                                                  TUnknown)
                                        | None ->
                                            Protocol.common_method_return env
                                              protocol_id method_name
                                            |> Option.value
                                                 ~default:
                                                   (Types.dynamic_constraint
                                                      TUnknown))
                                    | TFn (_, return_ty) -> return_ty
                                    | _ -> TUnknown
                                  in
                                  let witness_method_ty =
                                    match
                                      Types.protocol_constraint_info receiver_ty
                                    with
                                    | Some (_, witness_ty, _) ->
                                        Option.bind
                                          (Types.protocol_witness_method_types
                                             witness_ty)
                                          (fun methods ->
                                            List.nth_opt methods position)
                                    | None -> None
                                  in
                                  let witness_return_ty =
                                    match witness_method_ty with
                                    | Some (TFn (_, return_ty)) -> return_ty
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
                                          | TUnknown | TVar _ ->
                                              Types.dynamic_constraint TUnknown
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
                                      adapt_protocol_witness_result env
                                        ~expected:return_ty
                                        ~actual:witness_return_ty
                                        (Semantic_ir.Match
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
                                             ] ))))
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
                                | TFn (_, (TUnknown | TVar _)) ->
                                    contextual_return_ty TUnknown
                                | TFn (_, return_ty) -> return_ty
                                | _ -> TUnknown
                              in
                              let expected_params =
                                match method_ty with
                                | TFn (params, _)
                                  when List.length params
                                       = List.length call_args ->
                                    List.map materialize_protocol_unknown params
                                | _ -> List.map (fun arg -> arg.ty) call_args
                              in
                              let rec prepare prepared expected arguments =
                                match (expected, arguments) with
                                | [], [] -> Ok (List.rev prepared)
                                | ( expected :: expected_rest,
                                    argument :: argument_rest ) ->
                                    let expression =
                                      if
                                        (Types.is_dynamic expected
                                        || Types.equal expected TUnknown
                                        ||
                                        match expected with
                                           | TVar _ -> true
                                           | _ -> false)
                                        && not (Types.is_dynamic argument.ty)
                                      then
                                        pack_dynamic_value env
                                          (Types.dynamic_constraint TUnknown)
                                          argument
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
                          match
                            Protocol.lookup_marker_impl env marker method_name
                              receiver.ty
                          with
                          | None ->
                              Error.error
                                ("no protocol implementation for " ^ name
                               ^ " and " ^ source_name receiver.ty)
                          | Some impl -> (
                              match impl.ty with
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
                                    else if
                                      (Types.is_dynamic expected
                                      || Types.equal expected TUnknown
                                      || match expected with
                                         | TVar _ -> true
                                         | _ -> false)
                                      && not (Types.is_dynamic argument.ty)
                                    then
                                      pack_dynamic_value env
                                        (Types.dynamic_constraint TUnknown)
                                        argument
                                    else if
                                      Types.is_dynamic argument.ty
                                      && not (expects_dynamic_value expected)
                                    then
                                      dynamic_unpack env expected
                                        argument.semantic_expr
                                    else Ok argument.semantic_expr
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
                                  Result.map
                                    (fun arguments ->
                                      typed_ir ret
                                       (Semantic_ir.Apply
                                          ( Semantic_ir.Ident impl.ocaml_name,
                                             arguments )))
                                    (prepare [] param_tys args)
                              | TFn _ ->
                                  Error.error
                                    (name
                                   ^ " called with incompatible arguments")
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
