open Ast
open Types
open Lowered
module Env = Compiler_environment

let compile_expr = Expression_elaborator.compile_expr

let cljs_test_report_method_counter = ref 0

let next_cljs_test_report_method_name () =
  incr cljs_test_report_method_counter;
  "__lg_cljs_test_report_method_" ^ string_of_int !cljs_test_report_method_counter

let multimethod_method_counter = ref 0

let next_multimethod_method_name () =
  incr multimethod_method_counter;
  "__lg_multimethod_method_" ^ string_of_int !multimethod_method_counter

let cljs_test_report_method_symbol scope = function
  | "report" -> String.equal scope "cljs.test"
  | "cljs.test/report" | "t/report" | "ct/report" -> true
  | _ -> false

let resolve_multimethod_key scope env name =
  match String.split_on_char '/' name with
  | [ alias; member ] ->
      let owner =
        Env.resolve_namespace_alias ~scope alias env |> Option.value ~default:alias
      in
      owner ^ "/" ^ member
  | _ -> Names.scoped_key scope name

let compile_source_expr scope env form =
  let rec compile = function
    | FList (FSymbol name :: args) as form -> (
        match Env.find_macro ~scope name env with
        | None -> compile_expr scope env form
        | Some definition ->
            Result.bind
              (Macro_expander.expand ~scope ~compiler_env:env definition args)
              compile)
    | form -> compile_expr scope env form
  in
  compile form

let prepare_fn = Expression_elaborator.prepare_fn
let prepare_recursive_fn = Expression_elaborator.prepare_recursive_fn

let prepare_inferred_recursive_fn =
  Expression_elaborator.prepare_inferred_recursive_fn

let prepare_inferred_recursive_fn_with_return =
  Expression_elaborator.prepare_inferred_recursive_fn_with_return

let resolve_auto_keywords scope env form =
  let resolve keyword =
    if not (String.starts_with ~prefix:"::" keyword) then Ok keyword
    else
      let name = String.sub keyword 2 (String.length keyword - 2) in
      match String.index_opt name '/' with
      | None -> Ok (":" ^ scope ^ "/" ^ name)
      | Some index ->
          let alias = String.sub name 0 index in
          let local_name =
            String.sub name (index + 1) (String.length name - index - 1)
          in
          (match Env.resolve_namespace_alias ~scope alias env with
          | Some namespace -> Ok (":" ^ namespace ^ "/" ^ local_name)
          | None ->
              Error.error
                ("cannot resolve auto-keyword namespace alias " ^ alias))
  in
  let rec walk source =
    let resolved =
      match source with
      | FKeyword keyword -> Result.map (fun keyword -> FKeyword keyword) (resolve keyword)
      | FList forms -> Result.map (fun forms -> FList forms) (walk_many forms)
      | FVector forms -> Result.map (fun forms -> FVector forms) (walk_many forms)
      | FMap entries ->
          let rec walk_entries resolved = function
            | [] -> Ok (FMap (List.rev resolved))
            | (key, value) :: rest ->
                Result.bind (walk key) (fun key ->
                    Result.bind (walk value) (fun value ->
                        walk_entries ((key, value) :: resolved) rest))
          in
          walk_entries [] entries
      | (FSymbol _ | FCoreSymbol _ | FString _ | FRegex _ | FInt _ | FFloat _
        | FDecimal _ | FChar _ | FBool _) as leaf ->
          Ok leaf
    in
    Result.map
      (fun target ->
        if target != source then Source_context.copy_location ~source ~target;
        target)
      resolved
  and walk_many forms =
    let rec loop resolved = function
      | [] -> Ok (List.rev resolved)
      | form :: rest ->
          Result.bind (walk form) (fun form -> loop (form :: resolved) rest)
    in
    loop [] forms
  in
  walk form

let fn_code = Expression_elaborator.fn_code
let binding_of_expr = Expression_support.binding_of_expr
let lookup_function = Expression_support.lookup_function
let allocate_anonymous_record = Expression_support.allocate_anonymous_record

let allocate_nested_anonymous_records =
  Expression_support.allocate_nested_anonymous_records

let allocate_function_return_record env next_type
    (parts : Expression_support.compiled_fn_parts) =
  match parts.body.ty with
  | TRecord fields
    when (not (Types.is_homogeneous_record fields))
         && List.exists
              (fun (field : field) -> not (Types.is_dynamic field.ty))
              fields ->
      let nested =
        allocate_nested_anonymous_records ~owner:"" env next_type fields
      in
      let allocation =
        allocate_anonymous_record ~owner:"" nested.env nested.next_type
          nested.nested_fields
      in
      let items =
          if allocation.fresh then
            nested.items
            @ [
                Type_def
                  {
                    type_id = allocation.record.type_id;
                    type_name = allocation.record.type_name;
                    type_parameters = allocation.record.type_parameters;
                    fields = allocation.record.fields;
                    nominal = false;
                    location = None;
                  };
              ]
          else nested.items
      in
      let body =
          match parts.body.record_values with
          | Some _ ->
              Structural_map.as_named_record allocation.record parts.body
          | None ->
              let source_name = "__lg_function_record_result" in
              let source =
                {
                  parts.body with
                  semantic_expr = Semantic_ir.Ident source_name;
                  record_values = None;
                }
              in
              let projected =
                Structural_map.as_named_record allocation.record source
              in
              {
                projected with
                semantic_expr =
                  Semantic_ir.Let
                    ( [
                        ( Semantic_ir.PVar source_name,
                          parts.body.semantic_expr );
                      ],
                      projected.semantic_expr );
                record_values = None;
              }
      in
      ( allocation.env,
        allocation.next_type,
        items,
        { parts with body } )
  | _ -> (env, next_type, [], parts)

let allocate_function_local_records env next_type
    (parts : Expression_support.compiled_fn_parts) =
  let current_env = ref env in
  let current_next_type = ref next_type in
  let items = ref [] in
  let rec materialize_type = function
    | TRecord fields when Types.is_homogeneous_record fields ->
        TRecord
          (List.map
             (fun (field : field) ->
               { field with ty = materialize_type field.ty })
             fields)
    | TRecord fields ->
        let fields =
          List.map
            (fun (field : field) ->
              { field with ty = materialize_type field.ty })
            fields
        in
        let allocation =
          allocate_anonymous_record ~owner:"" !current_env !current_next_type
            fields
        in
        current_env := allocation.env;
        current_next_type := allocation.next_type;
        if allocation.fresh then
          items :=
            !items
            @ [
                Type_def
                  {
                    type_id = allocation.record.type_id;
                    type_name = allocation.record.type_name;
                    type_parameters = allocation.record.type_parameters;
                    fields = allocation.record.fields;
                    nominal = false;
                    location = None;
                  };
              ];
        TNamed_record allocation.record
    | TNullable ty -> TNullable (materialize_type ty)
    | TArray ty -> TArray (materialize_type ty)
    | TRef ty -> TRef (materialize_type ty)
    | TList ty -> TList (materialize_type ty)
    | TVector ty -> TVector (materialize_type ty)
    | TSet ty -> TSet (materialize_type ty)
    | TSeq ty -> TSeq (materialize_type ty)
    | TOcaml_app (name, arguments) ->
        TOcaml_app (name, List.map materialize_type arguments)
    | TTuple arguments -> TTuple (List.map materialize_type arguments)
    | TFn (parameters, return_type) ->
        TFn (parameters, materialize_type return_type)
    | TOverloaded_fn arities ->
        TOverloaded_fn
          (List.map
             (fun arity ->
               {
                 fixed_params = arity.fixed_params;
                 rest_param = arity.rest_param;
                 return_ty = materialize_type arity.return_ty;
               })
             arities)
    | TNamed_record record ->
        TNamed_record
          {
            record with
            type_arguments = List.map materialize_type record.type_arguments;
            fields =
              List.map
                (fun (field : field) ->
                  { field with ty = materialize_type field.ty })
                record.fields;
          }
    | ( TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol
      | TKeyword | TBool | TUnit | TNil | TUnknown | TMeta _ | TVar _ | TOcaml _
      ) as ty ->
        ty
  in
  let rec constrain_record type_name = function
    | Semantic_ir.Located (node_id, location, value) ->
        Semantic_ir.Located
          (node_id, location, constrain_record type_name value)
    | Semantic_ir.Record (fields, _) ->
        Semantic_ir.Record (fields, Some type_name)
    | value -> value
  in
  let rec constrain_pattern = function
    | Semantic_ir.PLocated (node_id, location, pattern) ->
        Semantic_ir.PLocated (node_id, location, constrain_pattern pattern)
    | Semantic_ir.PTyped (pattern, ty) ->
        let ty =
          match ty with TRecord _ -> ty | ty -> materialize_type ty
        in
        Semantic_ir.PTyped (pattern, ty)
    | pattern -> pattern
  in
  let materialize = function
    | Semantic_ir.Typed ((TRecord _ as ty), value)
      when (match Semantic_ir.unlocated value with
           | Semantic_ir.Record _ -> true
           | _ -> false) -> (
        match materialize_type ty with
        | TNamed_record record ->
            Semantic_ir.Typed
              ( TNamed_record record,
                constrain_record
                  (Structural_map.record_type_application record)
                  value )
        | ty -> Semantic_ir.Typed (ty, value))
    | Semantic_ir.Typed ((TRecord _ as ty), value) ->
        Semantic_ir.Typed (ty, value)
    | Semantic_ir.Typed ((TNamed_record _ as ty), value) -> (
        match materialize_type ty with
        | TNamed_record record as ty ->
            Semantic_ir.Typed
              ( ty,
                constrain_record
                  (Structural_map.record_type_application record)
                  value )
        | ty -> Semantic_ir.Typed (ty, value))
    | Semantic_ir.Typed (ty, value) ->
        Semantic_ir.Typed (materialize_type ty, value)
    | Semantic_ir.PackDynamic conversion ->
        Semantic_ir.PackDynamic
          {
            conversion with
            source_ty = materialize_type conversion.source_ty;
            target_ty = materialize_type conversion.target_ty;
          }
    | Semantic_ir.UnpackDynamic conversion ->
        Semantic_ir.UnpackDynamic
          {
            conversion with
            source_ty = materialize_type conversion.source_ty;
            target_ty = materialize_type conversion.target_ty;
          }
    | Semantic_ir.NullableToSeq conversion ->
        Semantic_ir.NullableToSeq
          {
            conversion with
            source_ty = materialize_type conversion.source_ty;
            element_ty = materialize_type conversion.element_ty;
          }
    | Semantic_ir.Fun (patterns, body) ->
        Semantic_ir.Fun (List.map constrain_pattern patterns, body)
    | value -> value
  in
  let semantic_expr =
    Semantic_ir.rewrite materialize parts.body.semantic_expr
  in
  let body_ty = materialize_type parts.body.ty in
  let param_bindings =
    List.map
      (fun (key, (binding : binding)) ->
        let ty =
          match binding.ty with
          | TRecord fields when not (Types.is_homogeneous_record fields) ->
              TRecord
                (List.map
                   (fun (field : field) ->
                     { field with ty = materialize_type field.ty })
                   fields)
          | ty -> materialize_type ty
        in
        (key, { binding with ty }))
      parts.param_bindings
  in
  ( !current_env,
    !current_next_type,
    !items,
    {
      parts with
      param_bindings;
      body = { parts.body with ty = body_ty; semantic_expr };
    } )

let allocate_top_level_local_records env next_type body =
  let parts : Expression_support.compiled_fn_parts =
    {
      param_bindings = [];
      param_identities = [];
      destructured_bindings = [];
      return_param_index_hint = None;
      body;
    }
  in
  let env, next_type, items, parts =
    allocate_function_local_records env next_type parts
  in
  (env, next_type, items, parts.body)

let dynamic_ty = Types.dynamic_constraint TUnknown

let list_nth list_name index =
  Semantic_ir.Apply
    ( Semantic_ir.Ident "List.nth",
      [ Semantic_ir.Ident list_name; Semantic_ir.Int index ] )

let dynamic_arg_names arity =
  List.init arity (fun index -> "__lg_multimethod_arg_" ^ string_of_int index)

let compile_multimethod_dispatch scope env dispatch_form =
  let args_name = "__lg_multimethod_dispatch_args" in
  match dispatch_form with
  | FKeyword keyword ->
      Ok
        ( 1,
          Semantic_ir.Fun
            ( [ Semantic_ir.PVar args_name ],
              Semantic_ir.Apply
                ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.get",
                  [
                    list_nth args_name 0;
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident
                          "Lg_runtime.Runtime_multimethod.dynamic_keyword",
                        [ Semantic_ir.String keyword ] );
                  ] ) ) )
  | FSymbol "identity" ->
      Ok
        ( 1,
          Semantic_ir.Fun ([ Semantic_ir.PVar args_name ], list_nth args_name 0)
        )
  | FSymbol "first" ->
      Ok
        ( 1,
          Semantic_ir.Fun
            ( [ Semantic_ir.PVar args_name ],
              Semantic_ir.Apply
                ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.first_value",
                  [ list_nth args_name 0 ] ) ) )
  | FList (FSymbol "fn" :: (FVector params as params_form) :: body_forms) ->
      let arity = List.length params in
      let overrides = List.map (fun _ -> Some dynamic_ty) params in
      Result.bind
        (Expression_elaborator.compile_fn ~param_type_overrides:overrides scope
           env params_form body_forms)
        (fun dispatch ->
          Ok
            ( arity,
              Semantic_ir.Fun
                ( [ Semantic_ir.PVar args_name ],
                  Semantic_ir.Apply
                    ( dispatch.semantic_expr,
                      List.init arity (list_nth args_name) ) ) ))
  | _ ->
      Error.error
        "defmulti currently supports keyword, identity, and fn dispatch forms"

let compile_multimethod_default scope env = function
  | [] ->
      Ok
        (Semantic_ir.Apply
           ( Semantic_ir.Ident "Lg_runtime.Runtime_multimethod.dynamic_keyword",
             [ Semantic_ir.String ":default" ] ))
  | [ FKeyword ":default"; value ] ->
      Result.map
        (fun value -> value.Types.semantic_expr)
        (Multimethod_dynamic_boundary.compile_form ~compile_expr scope env value)
  | _ -> Error.error "defmulti options currently support only :default"

let allocate_multi_arity_local_records env next_type
    (prepared : Expression_elaborator.prepared_multi_arity_fn) =
  let env, next_type, items, clauses =
    List.fold_left
      (fun (env, next_type, items, clauses)
           (clause : Expression_elaborator.prepared_multi_arity_clause) ->
        let env, next_type, clause_items, parts =
          allocate_function_local_records env next_type clause.parts
        in
        let param_bindings =
          List.map2
            (fun row_param_type (original, materialized) ->
              match row_param_type with
              | Some _ -> original
              | None -> materialized)
            clause.row_param_types
            (List.combine clause.parts.param_bindings parts.param_bindings)
        in
        let parts = { parts with param_bindings } in
        ( env,
          next_type,
          items @ clause_items,
          { clause with parts } :: clauses ))
      (env, next_type, [], []) prepared.clauses
  in
  (env, next_type, items, { prepared with clauses = List.rev clauses })

let row_param_type_names = Expression_support.row_param_type_names
let row_type_items = Expression_support.row_type_items
let check_emitted_name_collision = Resolver.check_emitted_name_collision
let unresolved_contextual_type = Expression_support.unresolved_contextual_type
let record_type_key = Resolver.record_type_key

let rec unresolved_record_hint = function
  | TOcaml name when String.starts_with ~prefix:"__lg_record:" name ->
      Some
        (String.sub name (String.length "__lg_record:")
           (String.length name - String.length "__lg_record:"))
  | TNullable ty | TArray ty | TRef ty | TList ty | TVector ty | TSet ty
  | TSeq ty ->
      unresolved_record_hint ty
  | TOcaml_app (_, arguments) | TTuple arguments ->
      List.find_map unresolved_record_hint arguments
  | TFn (parameters, return_ty) ->
      List.find_map unresolved_record_hint (return_ty :: parameters)
  | TOverloaded_fn arities ->
      List.find_map
        (fun (arity : fn_arity) ->
          match
            List.find_map unresolved_record_hint
              (arity.return_ty :: arity.fixed_params)
          with
          | Some _ as hint -> hint
          | None -> Option.bind arity.rest_param unresolved_record_hint)
        arities
  | TRecord fields ->
      List.find_map
        (fun (field : field) -> unresolved_record_hint field.ty)
        fields
  | TNamed_record record ->
      List.find_map unresolved_record_hint record.type_arguments
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TUnknown | TMeta _ | TVar _ | TOcaml _ ->
      None

let rec form_mentions_symbol name = function
  | FSymbol candidate -> candidate = name
  | FList (FSymbol ("quote" | "clojure.core/quote") :: _) -> false
  | FList forms | FVector forms -> List.exists (form_mentions_symbol name) forms
  | FMap pairs ->
      List.exists
        (fun (key, value) ->
          form_mentions_symbol name key || form_mentions_symbol name value)
        pairs
  | FInt _ | FFloat _ | FDecimal _ | FChar _ | FString _ | FRegex _ | FBool _
  | FKeyword _
  | FCoreSymbol _ ->
      false

let rec form_has_function_recur = function
  | FList (FSymbol ("fn" | "fn*" | "defn" | "defn-" | "loop" | "loop*") :: _)
    ->
      false
  | FList (FSymbol "recur" :: _) -> true
  | FList forms | FVector forms -> List.exists form_has_function_recur forms
  | FMap pairs ->
      List.exists
        (fun (key, value) ->
          form_has_function_recur key || form_has_function_recur value)
        pairs
  | FSymbol _ | FInt _ | FFloat _ | FDecimal _ | FChar _ | FString _ | FRegex _
  | FBool _
  | FKeyword _ | FCoreSymbol _ ->
      false

let function_is_recursive scope name body_forms =
  List.exists (form_mentions_symbol name) body_forms
  || List.exists
       (form_mentions_symbol (Names.scoped_key scope name))
       body_forms
  || List.exists form_has_function_recur body_forms

let order_protocol_groups groups =
  let method_names (_, methods) =
    List.filter_map
      (function
        | FList (FSymbol method_name :: _) -> Some method_name | _ -> None)
      methods
  in
  let depends_on (_, methods) provider =
    method_names provider
    |> List.exists (fun method_name ->
        List.exists (form_mentions_symbol method_name) methods)
  in
  let rec order ordered remaining =
    match
      List.find_opt
        (fun candidate ->
          not
            (List.exists
               (fun provider ->
                 provider != candidate && depends_on candidate provider)
               remaining))
        remaining
    with
    | None -> List.rev_append ordered remaining
    | Some candidate ->
        order (candidate :: ordered)
          (List.filter (fun group -> group != candidate) remaining)
  in
  order [] groups

let expression_references_declaration env expression =
  Semantic_ir.exists_identifier
    (fun name -> Env.unresolved_declaration name env)
    expression

let requires_stable_forward_binding env env_key expression =
  expression_references_declaration env expression
  || Env.explicitly_declared env_key env

let compile_defprotocol = Protocol_elaborator.compile_defprotocol
let compile_extend_type = Protocol_elaborator.compile_extend_type

let runtime_root_expression expression =
  Semantic_ir.Apply
    ( Semantic_ir.Ident "Lg_runtime.Runtime_reference.of_value",
      [ expression ] )

let redef_root_name ocaml_name = ocaml_name ^ "__root"

let redefable_binding (binding : Types.binding) =
  { binding with redef_root_name = Some (redef_root_name binding.ocaml_name) }

let redefable_function_wrapper root_name = function
  | TFn (parameter_tys, _) ->
      let names =
        List.mapi
          (fun index _ -> "__lg_redef_arg_" ^ string_of_int index)
          parameter_tys
      in
      let args = List.map (fun name -> Semantic_ir.Ident name) names in
      Semantic_ir.Fun
        ( List.map (fun name -> Semantic_ir.PVar name) names,
          Semantic_ir.Apply
            ( Semantic_ir.Apply
                ( Semantic_ir.Ident "Lg_runtime.Runtime_reference.deref",
                  [ Semantic_ir.Ident root_name ] ),
              args ) )
  | _ -> assert false

let rec contains_unresolved_type = function
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
  | TRecord fields ->
      List.exists (fun (field : field) -> contains_unresolved_type field.ty)
        fields
  | TNamed_record record ->
      List.exists contains_unresolved_type record.type_arguments
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TOcaml _ ->
      false

let source_scope_redefable_roots scope =
  (not (String.equal scope ""))
  &&
  not
    (String.equal scope "clojure.core"
    || String.starts_with ~prefix:"clojure." scope
    || String.starts_with ~prefix:"cljs." scope
    || String.starts_with ~prefix:"datascript." scope
    || String.starts_with ~prefix:"me.tonsky." scope)

let predeclare_protocol_groups scope env receiver_form groups =
  let rec protocol_constraints constraints ty =
    match Types.protocol_constraint_info ty with
    | Some (protocol_id, _witness_ty, value_ty) ->
        protocol_constraints (protocol_id :: constraints) value_ty
    | None -> (
        match ty with
        | TNullable inner | TArray inner | TRef inner | TList inner
        | TVector inner | TSet inner | TSeq inner ->
            protocol_constraints constraints inner
        | TOcaml_app (_, arguments) | TTuple arguments ->
            List.fold_left protocol_constraints constraints arguments
        | TFn (parameters, return_ty) ->
            List.fold_left protocol_constraints constraints
              (return_ty :: parameters)
        | TOverloaded_fn arities ->
            List.fold_left
              (fun constraints (arity : fn_arity) ->
                let types =
                  arity.return_ty :: arity.fixed_params
                  @ Option.to_list arity.rest_param
                in
                List.fold_left protocol_constraints constraints types)
              constraints arities
        | TRecord _ | TNamed_record _ -> constraints
        | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol
        | TKeyword | TBool | TUnit | TNil | TUnknown | TMeta _ | TVar _ | TOcaml _ ->
            constraints)
  in
  let required_protocols =
    groups
    |> List.fold_left
         (fun protocols (consumer_name, methods) ->
           let consumer_id =
             Protocol.find_protocol_id scope env consumer_name
           in
           methods
           |> List.concat_map Dependency_graph.symbols
           |> List.fold_left
                (fun protocols name ->
                  match Resolver.lookup_binding scope env name with
                  | Ok (binding : binding) when binding.forward_declared ->
                      protocol_constraints [] binding.ty
                      |> List.fold_left
                           (fun protocols required_id ->
                             if
                               match consumer_id with
                               | Some consumer_id ->
                                   Protocol_id.equal required_id consumer_id
                               | None -> false
                             then protocols
                             else required_id :: protocols)
                           protocols
                  | Ok _ | Error _ -> protocols)
                protocols)
         []
    |> List.sort_uniq Protocol_id.compare
  in
  List.fold_left
    (fun result (protocol_name, methods) ->
      Result.bind result (fun env ->
          match Protocol.find_protocol_id scope env protocol_name with
          | Some protocol_id
            when List.exists (Protocol_id.equal protocol_id)
                   required_protocols ->
              Protocol_elaborator.predeclare_implementations_from_evidence
                scope env receiver_form protocol_name methods
          | Some _ | None -> Ok env))
    (Ok env) groups

let deferred_value_type env (expr : Types.typed_expr) =
  Types.align_deferred_param_types
    (Protocol.refine_deferred_type env expr.ty)
    expr.semantic_expr

let preserves_required_seqable_protocol_result (expr : Types.typed_expr) =
  Semantic_ir.exists_identifier (String.equal "__lg_seqable_value")
    expr.semantic_expr

let located_value_pattern form pattern =
  match Source_context.find form with
  | None -> pattern
  | Some location ->
      Located_value (Source_node_id.of_location location, location, pattern)

let compile_module_alias = Module_elaborator.compile_module_alias
let compile_module_signature = Module_signature_elaborator.compile
let compile_module_apply = Module_elaborator.compile_module_apply
let compile_module = Module_elaborator.compile_module
let compile_module_functor = Module_elaborator.compile_module_functor
let open_module_bindings = Module_environment.open_bindings
let parse_type_parameters = Type_parameters.parse
let compile_type_alias = Type_definition_elaborator.compile_type_alias
let compile_type_record = Type_definition_elaborator.compile_type_record
let compile_signature = Signature_elaborator.compile

let compile_type_record_fields =
  Type_definition_elaborator.compile_type_record_fields

let compile_type_variant = Type_definition_elaborator.compile_type_variant

let sidecar_function_signature scope env name =
  let signatures = Env.signatures env in
  match
    Signature_overlay.find_value (Names.scoped_key scope name) signatures
  with
  | Some _ as signature -> signature
  | None when scope = "" && not (Names.is_qualified name) ->
      Signature_overlay.find_value ("user/" ^ name) signatures
  | None -> None

let recursive_type_annotation scope env name =
  match
    sidecar_function_signature scope env name
    |> Option.map (Function_elaborator.infer_named_record scope env)
  with
  | Some (TFn _ as signature) -> Some signature
  | Some _ | None -> None

let prepare_function scope env name params body_forms =
  let signature =
    sidecar_function_signature scope env name
    |> Option.map (Function_elaborator.infer_named_record scope env)
  in
  match signature with
  | Some (TFn (parameter_types, return_type)) ->
      Result.bind
        (prepare_fn
           ~param_type_overrides:(List.map Option.some parameter_types)
           ~expected_return_ty:return_type scope env params body_forms)
        (fun (parts : Expression_support.compiled_fn_parts) ->
          Result.map
            (fun semantic_expr ->
              { parts with body = typed_ir return_type semantic_expr })
            (Call_elaborator.adapt_value_to_type env return_type parts.body))
  | Some _ -> Error.error ("function signature expected for " ^ name)
  | None ->
      Result.bind
        (prepare_fn ~materialize_open_equality:true scope env params body_forms)
        (fun (parts : Expression_support.compiled_fn_parts) ->
          match parts.return_param_index_hint with
          | None -> Ok parts
          | Some index -> (
              match List.nth_opt parts.param_bindings index with
              | None -> Ok parts
              | Some (_, parameter) ->
                  let return_ty = Types.constraint_value_type parameter.ty in
                  let self_returning_protocol =
                    match Types.protocol_constraint_info parameter.ty with
                    | Some (protocol_id, _, _) ->
                        Protocol.has_self_returning_method env protocol_id
                    | None -> false
                  in
                  if
                    (not self_returning_protocol)
                    || Types.equal parts.body.ty return_ty
                  then Ok parts
                  else
                    prepare_fn
                      ~param_type_overrides:
                        (List.map
                           (fun (_, (binding : binding)) -> Some binding.ty)
                           parts.param_bindings)
                      ~expected_return_ty:return_ty
                      ~materialize_open_equality:true scope env params
                      body_forms))

let rec concrete_defrecord_field_type = function
  | TUnknown | TMeta _ | TVar _ | TRecord _ -> None
  | ty when Types.is_dynamic ty -> None
  | ty when Option.is_some (Types.protocol_constraint_info ty) -> None
  | ty when Option.is_some (Types.seqable_constraint_info ty) -> None
  | TNullable (TUnknown | TMeta _ | TVar _) -> None
  | TNullable inner when Types.is_dynamic inner -> None
  | TOcaml_app ("option", [ (TUnknown | TMeta _ | TVar _) ]) -> None
  | TOcaml_app ("option", [ inner ]) when Types.is_dynamic inner -> None
  | TNullable ty ->
      Option.map (fun ty -> TNullable ty) (concrete_defrecord_field_type ty)
  | TOcaml_app (name, arguments) ->
      let rec concrete arguments =
        match arguments with
        | [] -> Some []
        | argument :: rest ->
            Option.bind (concrete_defrecord_field_type argument)
              (fun argument ->
                Option.map (fun rest -> argument :: rest) (concrete rest))
      in
      Option.map
        (fun arguments -> TOcaml_app (name, arguments))
        (concrete arguments)
  | TTuple items ->
      let rec concrete items =
        match items with
        | [] -> Some []
        | item :: rest ->
            Option.bind (concrete_defrecord_field_type item) (fun item ->
                Option.map (fun rest -> item :: rest) (concrete rest))
      in
      Option.map (fun items -> TTuple items) (concrete items)
  | TArray ty ->
      Option.map (fun ty -> TArray ty) (concrete_defrecord_field_type ty)
  | TRef ty -> Option.map (fun ty -> TRef ty) (concrete_defrecord_field_type ty)
  | TList ty ->
      Option.map (fun ty -> TList ty) (concrete_defrecord_field_type ty)
  | TVector ty ->
      Option.map (fun ty -> TVector ty) (concrete_defrecord_field_type ty)
  | TSet ty -> Option.map (fun ty -> TSet ty) (concrete_defrecord_field_type ty)
  | TSeq ty -> Option.map (fun ty -> TSeq ty) (concrete_defrecord_field_type ty)
  | TFn (parameters, return_ty) ->
      let rec concrete parameters =
        match parameters with
        | [] -> Some []
        | parameter :: rest ->
            Option.bind (concrete_defrecord_field_type parameter)
              (fun parameter ->
                Option.map (fun rest -> parameter :: rest) (concrete rest))
      in
      Option.bind (concrete parameters) (fun parameters ->
          Option.map
            (fun return_ty -> TFn (parameters, return_ty))
            (concrete_defrecord_field_type return_ty))
  | TOverloaded_fn _ -> None
  | ( TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
    | TBool | TUnit | TNil | TOcaml _ | TNamed_record _ ) as ty ->
      Some ty

let has_static_capability ty =
  Option.is_some (Types.protocol_constraint_info ty)
  || Option.is_some (Types.seqable_constraint_info ty)
  || Option.is_some (Types.truthy_constraint_info ty)
  || Option.is_some (Types.nil_predicate_constraint_info ty)
  || Option.is_some (Types.printable_constraint_info ty)
  || Option.is_some (Types.exception_data_constraint_info ty)
  || Option.is_some (Types.hashable_constraint_info ty)
  || Option.is_some (Types.comparable_constraint_info ty)
  || Option.is_some (Types.array_index_constraint_info ty)
  || Option.is_some (Types.symbol_predicate_constraint_info ty)
  || Option.is_some (Types.contains_constraint_info ty)

let rec freshen_unknowns = function
  | TUnknown -> Type_solver.fresh ()
  | TNullable ty -> TNullable (freshen_unknowns ty)
  | TArray ty -> TArray (freshen_unknowns ty)
  | TRef ty -> TRef (freshen_unknowns ty)
  | TList ty -> TList (freshen_unknowns ty)
  | TVector ty -> TVector (freshen_unknowns ty)
  | TSet ty -> TSet (freshen_unknowns ty)
  | TSeq ty -> TSeq (freshen_unknowns ty)
  | TOcaml_app (name, arguments) ->
      TOcaml_app (name, List.map freshen_unknowns arguments)
  | TTuple arguments -> TTuple (List.map freshen_unknowns arguments)
  | TFn (parameters, return_ty) ->
      TFn (List.map freshen_unknowns parameters, freshen_unknowns return_ty)
  | TOverloaded_fn arities ->
      TOverloaded_fn
        (List.map
           (fun arity ->
             {
               fixed_params = List.map freshen_unknowns arity.fixed_params;
               rest_param = Option.map freshen_unknowns arity.rest_param;
               return_ty = freshen_unknowns arity.return_ty;
             })
           arities)
  | TRecord fields ->
      TRecord
        (List.map
           (fun (field : field) ->
             { field with ty = freshen_unknowns field.ty })
           fields)
  | TNamed_record record ->
      TNamed_record
        {
          record with
          type_arguments = List.map freshen_unknowns record.type_arguments;
        }
  | ( TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol
    | TKeyword | TBool | TUnit | TNil | TMeta _ | TVar _ | TOcaml _ ) as ty ->
      ty

let nullable_payload = function
  | TNullable ty | TOcaml_app ("option", [ ty ]) -> Some ty
  | _ -> None

let merge_defrecord_field_types previous inferred =
  if
    (match previous with TUnknown | TMeta _ | TVar _ -> true | _ -> false)
    && has_static_capability inferred
  then inferred
  else
  let merge_payload previous inferred =
    match
      ( concrete_defrecord_field_type previous,
        concrete_defrecord_field_type inferred )
    with
    | _, Some (TNamed_record _ as inferred) -> inferred
    | None, Some inferred -> inferred
    | Some previous, _ -> previous
    | None, None -> previous
  in
  match (nullable_payload previous, nullable_payload inferred) with
  | Some previous, Some inferred ->
      Types.normalize_nullable
        (TNullable (merge_payload previous inferred))
  | Some payload, None ->
      Types.normalize_nullable (TNullable (merge_payload payload inferred))
  | None, Some payload ->
      Types.normalize_nullable (TNullable (merge_payload previous payload))
  | None, None -> merge_payload previous inferred

let rec type_parameters_of_type = function
  | TVar name -> [ name ]
  | TNullable ty
  | TArray ty
  | TRef ty
  | TList ty
  | TVector ty
  | TSet ty
  | TSeq ty ->
      type_parameters_of_type ty
  | TOcaml_app (_, arguments) | TTuple arguments ->
      List.concat_map type_parameters_of_type arguments
  | TFn (parameters, return_ty) ->
      List.concat_map type_parameters_of_type (return_ty :: parameters)
  | TOverloaded_fn arities ->
      arities
      |> List.concat_map (fun (arity : fn_arity) ->
          type_parameters_of_type arity.return_ty
          @ List.concat_map type_parameters_of_type arity.fixed_params
          @ Option.fold ~none:[] ~some:type_parameters_of_type arity.rest_param)
  | TRecord fields ->
      fields
      |> List.concat_map (fun (field : field) ->
          type_parameters_of_type field.ty)
  | TNamed_record record ->
      List.concat_map type_parameters_of_type record.type_arguments
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TUnknown | TMeta _ | TOcaml _ ->
      []

let generalize_types types =
  match (Type_solver.generalize (TTuple types)).body with
  | TTuple generalized -> generalized
  | _ -> assert false

let infer_defrecord_field_types scope env record_name field_names interface_forms =
  let accessor_parameter name =
    "__lg_record_field_" ^ Names.sanitize_name name
  in
  let field_name accessor =
    List.find_opt (fun name -> accessor = ".-" ^ name) field_names
  in
  let rec rewrite_field_access receiver = function
    | FList [ FSymbol accessor; FSymbol target ]
      when target = receiver && Option.is_some (field_name accessor) ->
        FSymbol (accessor_parameter (Option.get (field_name accessor)))
    | FList forms -> FList (List.map (rewrite_field_access receiver) forms)
    | FVector forms -> FVector (List.map (rewrite_field_access receiver) forms)
    | FMap pairs ->
        FMap
          (List.map
             (fun (key, value) ->
               ( rewrite_field_access receiver key,
                 rewrite_field_access receiver value ))
             pairs)
    | form -> form
  in
  let lookup_function_ty = Expression_support.lookup_function_ty scope env in
  let lookup_protocol_constraint = Protocol.constraint_type scope env in
  let rec protocol_ids ty =
    match Types.protocol_constraint_info ty with
    | Some (protocol_id, _, value_ty) -> protocol_id :: protocol_ids value_ty
    | None -> (
        match Types.dynamic_constraint_info ty with
        | Some capability -> protocol_ids capability
        | None -> [])
  in
  let rec has_guarded_protocol_constraint ty =
    if Types.is_guarded_protocol_constraint ty then true
    else
      match Types.protocol_constraint_info ty with
      | Some (_, _, value_ty) -> has_guarded_protocol_constraint value_ty
      | None -> (
          match Types.dynamic_constraint_info ty with
          | Some capability -> has_guarded_protocol_constraint capability
          | None -> false)
  in
  let resolve_protocol_record ty =
    if has_guarded_protocol_constraint ty then ty
    else
    match protocol_ids ty with
    | [] -> ty
    | protocols ->
        let candidates =
          Env.filter_record_bindings
            (fun key (binding : binding) ->
              if String.starts_with ~prefix:"__record/" key then
                match binding.ty with
                | TNamed_record record
                  when List.for_all
                         (fun protocol_id ->
                           Protocol.type_satisfies env protocol_id
                             (TNamed_record record))
                         protocols ->
                    Some record
                | _ -> None
              else None)
            env
          |> List.sort_uniq (fun left right ->
                 Type_id.compare left.type_id right.type_id)
        in
        (match candidates with [ record ] -> TNamed_record record | _ -> ty)
  in
  let method_types =
    interface_forms
    |> List.filter_map (function
         | FList (FSymbol method_name :: FVector parameters :: _) ->
             Some
               ( method_name,
                 TFn
                   ( List.map (fun _ -> Type_solver.fresh ()) parameters,
                     Type_solver.fresh () ) )
         | _ -> None)
    |> ref
  in
  let method_return_type method_name =
    match List.assoc_opt method_name !method_types with
    | Some (TFn (_, return_ty)) -> (
        match return_ty with
        | TUnknown | TMeta _ | TVar _ -> None
        | ty -> Some ty)
    | Some _ | None -> None
  in
  let update_method_types inferred =
    method_types :=
      List.map
        (fun (name, ty) ->
          (name, List.assoc_opt name inferred |> Option.value ~default:ty))
        !method_types
  in
  let propagate_receiver_protocol_methods receiver inferred =
    let rec evidence ty =
      match Types.protocol_constraint_info ty with
      | Some (protocol_id, witness_ty, value_ty) ->
          (protocol_id, witness_ty) :: evidence value_ty
      | None -> []
    in
    let update inferred (protocol_id, witness_ty) =
      match
        ( Protocol_registry.find_protocol protocol_id (Env.protocols env),
          Types.protocol_witness_method_types witness_ty )
      with
      | Some declaration, Some witness_methods ->
          let declared_methods =
            Protocol_registry.Method_map.bindings declaration.methods
          in
          if List.length declared_methods <> List.length witness_methods then
            inferred
          else
            List.fold_left2
              (fun inferred (method_id, _) witness_method ->
                let method_name = Method_id.name method_id in
                match List.assoc_opt method_name inferred with
                | None -> inferred
                | Some existing ->
                    Type_inference.replace_param method_name
                      (Type_inference.refine_type existing witness_method)
                      inferred)
              inferred declared_methods witness_methods
      | None, _ | _, None -> inferred
    in
    match List.assoc_opt receiver inferred with
    | None -> inferred
    | Some receiver_ty -> List.fold_left update inferred (evidence receiver_ty)
  in
  let infer_method field_types = function
    | FList
        (FSymbol method_name
        :: FVector (FSymbol receiver :: method_params)
        :: body_forms) -> (
        let method_params =
          method_params
          |> List.filter_map (function
            | FSymbol name when not (List.mem name field_names) ->
                Some (name, TUnknown)
            | _ -> None)
        in
        let params =
          ((receiver, TUnknown) :: method_params)
          @ List.map (fun name -> (name, TUnknown)) field_names
          @ List.map
              (fun name -> (accessor_parameter name, TUnknown))
              field_names
          @ List.filter
              (fun (name, _) -> not (String.equal name method_name))
              !method_types
        in
        let body_forms = List.map (rewrite_field_access receiver) body_forms in
        let body_forms =
          Macro_expander.expand_all_forms ~scope ~compiler_env:env body_forms
          |> Result.value ~default:body_forms
        in
        let lookup_dynamic_key_record_type =
          Expression_support.dynamic_key_record_type env
        in
        let resolve_named_record =
          Function_elaborator.infer_named_record scope env
        in
        let nullable_constructor_fields =
          Array.make (List.length field_names) false
        in
        let form_has_nullable_return = function
          | FList (FSymbol function_name :: arguments) -> (
              match lookup_function_ty function_name with
              | Ok (TFn (parameters, return_ty))
                when List.length parameters = List.length arguments ->
                  Option.is_some (nullable_payload return_ty)
              | _ -> false)
          | _ -> false
        in
        let observe_call name argument_forms argument_tys =
          let constructor_name = record_name ^ "." in
          if
            (name = constructor_name
            || String.ends_with ~suffix:("/" ^ constructor_name) name)
            && List.length argument_tys = List.length field_names
          then
            List.combine argument_forms argument_tys
            |> List.iteri (fun index (form, ty) ->
                if
                  Types.equal ty TNil
                  || Option.is_some (nullable_payload ty)
                  || form_has_nullable_return form
                then nullable_constructor_fields.(index) <- true)
        in
        match
          Type_inference.infer_params
            ?expected_return_ty:(method_return_type method_name)
            ~lookup_function_ty
            ~lookup_protocol_constraint ~lookup_dynamic_key_record_type
            ~resolve_named_record ~observe_call params body_forms
        with
        | Error _ -> field_types
        | Ok inferred_params ->
            let inferred_params =
              propagate_receiver_protocol_methods receiver inferred_params
            in
            update_method_types inferred_params;
            let inferred_fields =
              List.combine field_names field_types
              |> List.mapi (fun index (name, previous) ->
let inferred =
                  List.assoc_opt name inferred_params
                  |> Option.value ~default:TUnknown
                in
                let accessor_inferred =
                  List.assoc_opt (accessor_parameter name) inferred_params
                  |> Option.value ~default:TUnknown
                in
                let inferred =
                  Type_inference.refine_type inferred accessor_inferred
                in
                let inferred =
                  Function_elaborator.infer_named_record
                    ~allow_dynamic_fields:true scope env inferred
                  |> resolve_protocol_record
                in
                let merged =
                  merge_defrecord_field_types previous inferred
                in
                if
                  nullable_constructor_fields.(index)
                  && Option.is_none (nullable_payload merged)
                then TNullable merged
                else merged)
            in
            inferred_fields)
    | _ -> field_types
  in
  let rec stabilize remaining field_types =
    let previous_methods = !method_types in
    let inferred =
      List.fold_left infer_method field_types interface_forms
    in
    let stable_fields =
      List.length field_types = List.length inferred
      && List.for_all2 Types.equal field_types inferred
    in
    let stable_methods =
      List.length previous_methods = List.length !method_types
      && List.for_all2
           (fun (left_name, left_ty) (right_name, right_ty) ->
             left_name = right_name && Types.equal left_ty right_ty)
           previous_methods !method_types
    in
    if remaining = 0 || (stable_fields && stable_methods) then inferred
    else stabilize (remaining - 1) inferred
  in
  let inferred =
    stabilize 2 (List.map (fun _ -> TUnknown) field_names)
    |> List.map (fun ty ->
           if has_guarded_protocol_constraint ty then TUnknown
           else if has_static_capability ty then freshen_unknowns ty
           else
             concrete_defrecord_field_type ty
             |> Option.value ~default:TUnknown)
  in
  inferred

let rec compile scope env next_type form =
  Result.bind (resolve_auto_keywords scope env form) (fun form ->
      compile_resolved scope env next_type form)

and compile_resolved scope env next_type form =
  let env =
    match form with
    | FList
        (FSymbol ("def" | "defonce" | "defn" | "defn-") :: FSymbol name :: _)
      ->
        Require.remove_source_core_macro_alias env scope name
    | _ -> env
  in
  match form with
  | FList
      [
        FSymbol "do";
        FList
          [
            FSymbol "defrecord";
            (FSymbol record_name as name_form);
            (FVector _ as fields);
          ];
        FList (FSymbol "extend-type" :: FSymbol receiver_name :: implementations);
      ]
    when record_name = receiver_name ->
      compile scope env next_type
        (FList (FSymbol "defrecord" :: name_form :: fields :: implementations))
  | FList (FSymbol "do" :: forms) ->
      let items_of = function Group items -> items | item -> [ item ] in
      let rec compile_forms scope env next_type items = function
        | [] -> Ok (scope, env, next_type, Group (List.rev items))
        | form :: rest -> (
            match compile scope env next_type form with
            | Error _ as error -> error
            | Ok (scope, env, next_type, item) ->
                compile_forms scope env next_type
                  (List.rev_append (items_of item) items)
                  rest)
      in
      compile_forms scope env next_type [] forms
  | FList
      (FSymbol "defrecord"
      :: (FSymbol name as name_form)
      :: FVector raw_fields
      :: interface_forms) ->
      let emitted_name = Names.ocaml_binding_name scope name in
      let provisional_type_id =
        Type_id.create ~owner:(if scope = "" then [] else [ scope ]) ~name
      in
      let provisional_record =
        Types.named_record ~type_id:provisional_type_id ~nominal:false
          ~type_name:emitted_name ~set_module_name:("Set_" ^ emitted_name) []
      in
      let env =
        Env.add
          (record_type_key scope name)
          (Types.binding ~forward_declared:true emitted_name
             provisional_record)
          env
      in
      let resolve_field_hint hint =
        Result.bind (Type_annotation.of_param_annotation hint) (fun ty ->
            let ty = Function_elaborator.infer_named_record scope env ty in
            match unresolved_record_hint ty with
            | Some name -> Error.error ("unknown record type " ^ name)
            | None -> Ok ty)
      in
      let rec field_specs acc hint = function
        | [] -> (
            match hint with
            | None -> Ok (List.rev acc)
            | Some _ -> Error.error "defrecord field hint requires a field")
        | FSymbol metadata :: rest when String.starts_with ~prefix:"^" metadata
          -> (
            match hint with
            | None -> field_specs acc (Some metadata) rest
            | Some _ -> Error.error "defrecord field has multiple type hints")
        | FSymbol field_name :: rest -> (
            match hint with
            | None -> field_specs ((field_name, None) :: acc) None rest
            | Some hint ->
                Result.bind (resolve_field_hint hint) (fun ty ->
                    field_specs ((field_name, Some ty) :: acc) None rest))
        | _ -> Error.error "defrecord fields must be symbols"
      in
      let rec protocol_groups groups current = function
        | [] -> (
            match current with
            | None -> Ok (List.rev groups)
            | Some (protocol_name, methods) ->
                Ok (List.rev ((protocol_name, List.rev methods) :: groups)))
        | FSymbol protocol_name :: rest ->
            let groups =
              match current with
              | None -> groups
              | Some (name, methods) -> (name, List.rev methods) :: groups
            in
            protocol_groups groups (Some (protocol_name, [])) rest
        | (FList _ as method_form) :: rest -> (
            match current with
            | None -> Error.error "defrecord method requires a protocol name"
            | Some (protocol_name, methods) ->
                protocol_groups groups
                  (Some (protocol_name, method_form :: methods))
                  rest)
        | _ :: _ -> Error.error "invalid defrecord protocol implementation"
      in
      let items_of = function Group items -> items | item -> [ item ] in
      Result.bind (field_specs [] None raw_fields) (fun field_specs ->
          let fields = List.map fst field_specs in
          let inferred_field_types =
            infer_defrecord_field_types scope env name fields interface_forms
          in
          let fresh_unresolved_field_type = function
            | TNullable _ -> TNullable (Type_solver.fresh ())
            | TOcaml_app ("option", [ _ ]) ->
                TOcaml_app ("option", [ Type_solver.fresh () ])
            | _ -> Type_solver.fresh ()
          in
          let inferred_field_types =
            List.map2
              (fun (_field_name, explicit_ty) inferred_ty ->
                match explicit_ty with
                | Some _ -> inferred_ty
                | None
                  when Option.is_none
                         (concrete_defrecord_field_type inferred_ty)
                       && not (has_static_capability inferred_ty) ->
                    fresh_unresolved_field_type inferred_ty
                | None -> inferred_ty)
              field_specs inferred_field_types
          in
          let field_types =
            inferred_field_types
            |> List.map2 (fun (_field_name, explicit_ty) inferred_ty ->
                   match (explicit_ty, inferred_ty) with
                   | ( Some explicit_ty,
                       (TNullable inferred_inner
                       | TOcaml_app ("option", [ inferred_inner ])) )
                     when not
                            (match explicit_ty with
                            | TNullable _
                            | TOcaml_app ("option", [ _ ]) ->
                                true
                            | _ -> false)
                          && (Types.equal inferred_inner TUnknown
                             || (match inferred_inner with
                                | TMeta _ | TVar _ -> true
                                | _ -> false)
                             || Types.is_dynamic inferred_inner
                             || Types.assignable ~policy:Host_boundary
                                  ~expected:explicit_ty
                                  ~actual:inferred_inner) ->
                       TNullable explicit_ty
                   | Some explicit_ty, _ -> explicit_ty
                   | None, _ -> inferred_ty)
                 field_specs
          in
          let field_types = generalize_types field_types in
          let type_parameters =
            field_types
            |> List.concat_map type_parameters_of_type
            |> List.sort_uniq String.compare
          in
          let record_fields =
            List.map2
              (fun field_name ty -> Types.make_field (":" ^ field_name) ty)
              fields field_types
          in
          match
            compile_type_record_fields
              ?location:(Source_context.find name_form)
              ~allow_empty:true
              ~nominal:false
              ~emitted_name
              scope env next_type name type_parameters record_fields
          with
          | Error _ as error -> error
          | Ok (scope, env, next_type, type_item) -> (
              match protocol_groups [] None interface_forms with
              | Error _ as error -> error
              | Ok groups ->
                  let groups = order_protocol_groups groups in
                  let receiver_form = FSymbol name in
                  let rec compile_groups env next_type items = function
                    | [] -> Ok (scope, env, next_type, Group items)
                    | (protocol_name, methods) :: rest -> (
                        let wrap_method method_name _receiver_name params
                            body_forms =
                          FList (FSymbol method_name :: params :: body_forms)
                        in
                        let expand_method = function
                          | FList
                              (FSymbol method_name
                              :: (FVector (FSymbol receiver_name :: _) as params)
                              :: body_forms) ->
                              [
                                wrap_method method_name receiver_name params
                                  body_forms;
                              ]
                          | (FList (FSymbol method_name :: arities) as method_form)
                            ->
                              let rec expand acc = function
                                | [] -> Some (List.rev acc)
                                | FList
                                    ((FVector (FSymbol receiver_name :: _) as
                                      params)
                                    :: body_forms)
                                  :: rest ->
                                    expand
                                      (wrap_method method_name receiver_name
                                         params body_forms
                                      :: acc)
                                      rest
                                | _ -> None
                              in
                              Option.value (expand [] arities)
                                ~default:[ method_form ]
                          | method_form -> [ method_form ]
                        in
                        let wrapped_methods =
                          List.concat_map expand_method methods
                        in
                        let implementation_form =
                        match
                            Protocol.find_protocol_id scope env protocol_name
                          with
                          | Some _ ->
                              FList
                               (FSymbol "extend-type-no-register" :: FSymbol name
                               :: FSymbol protocol_name :: wrapped_methods)
                          | None ->
                              FList
                               (FSymbol "deftype-methods-no-register"
                               :: FSymbol name :: FSymbol protocol_name
                               :: methods)
                        in
                        match
                          compile scope env next_type implementation_form
                        with
                        | Error _ as error -> error
                        | Ok (_, env, next_type, item) ->
                            compile_groups env next_type
                              (items @ items_of item)
                              rest)
                  in
                  Result.bind
                    (predeclare_protocol_groups scope env receiver_form groups)
                    (fun env ->
                      compile_groups env next_type (items_of type_item) groups)))
  | FList (FSymbol "deftype" :: args)
    when Option.is_some (Env.find_macro ~scope "deftype" env) -> (
      match Env.find_macro ~scope "deftype" env with
      | None -> assert false
      | Some definition -> (
          match Macro_expander.expand ~scope ~compiler_env:env definition args with
          | Error _ as error -> error
          | Ok expanded -> compile scope env next_type expanded))
  | FList
      (FSymbol "deftype"
      :: (FSymbol name as name_form)
      :: FVector raw_fields
      :: _interface_forms) ->
      let rec field_specs acc metadata mutable_field = function
        | [] -> Ok (List.rev acc)
        | FSymbol ("^:mutable" | "^:unsynchronized-mutable") :: rest ->
            field_specs acc metadata true rest
        | FSymbol metadata :: rest when String.starts_with ~prefix:"^" metadata
          ->
            field_specs acc (Some metadata) mutable_field rest
        | FSymbol field_name :: rest ->
            field_specs
              ((field_name, metadata, mutable_field) :: acc)
              None false rest
        | _ -> Error.error "deftype fields must be symbols"
      in
      Result.bind (field_specs [] None false raw_fields) (fun fields ->
          let signature_fields =
            Signature_overlay.find_record (Names.scoped_key scope name)
              (Env.signatures env)
          in
          let signature_field_type field_name =
            Option.bind signature_fields (fun fields ->
                fields
                |> List.find_opt (fun (field : Types.field) ->
                       field.keyword = ":" ^ field_name)
                |> Option.map (fun (field : Types.field) -> field.ty))
          in
          let resolve_field_type field_name metadata =
            match signature_field_type field_name with
            | Some ty -> Ok ty
            | None -> (
                match metadata with
                | None -> Ok (Type_solver.fresh ())
                | Some annotation ->
                    Result.map
                      (Function_elaborator.infer_named_record scope env)
                      (Type_annotation.of_param_annotation annotation))
          in
          let rec build_definitions definitions = function
            | [] -> Ok (List.rev definitions)
            | (field_name, metadata, mutable_field) :: rest ->
                Result.bind
                  (resolve_field_type field_name metadata)
                  (fun inferred_type ->
                    let definition =
                      ( type_parameters_of_type inferred_type,
                        Types.make_field ~mutable_:mutable_field
                          (":" ^ field_name) inferred_type )
                    in
                    build_definitions (definition :: definitions) rest)
          in
          Result.bind (build_definitions [] fields) (fun definitions ->
              let record_fields =
                match definitions with
                | [] -> [ Types.make_record_identity_field () ]
                | _ -> List.map snd definitions
              in
              let generalized_types =
                record_fields
                |> List.map (fun (field : Types.field) -> field.ty)
                |> generalize_types
              in
              let record_fields =
                List.map2
                  (fun (field : Types.field) ty -> { field with ty })
                  record_fields generalized_types
              in
              let type_parameters =
                generalized_types |> List.concat_map type_parameters_of_type
              in
              compile_type_record_fields
                ?location:(Source_context.find name_form)
                ~allow_empty:true scope env next_type name type_parameters
                record_fields))
  | FList
      (FSymbol
         ( "deftype-methods"
         | "deftype-methods-no-pack"
         | "deftype-methods-no-register" )
      :: FSymbol type_name :: interface_forms)
    -> (
      match Resolver.lookup_record_type scope env type_name with
      | Error _ as err -> err
      | Ok record ->
          let receiver_ty = TNamed_record record in
          let method_arity params =
            match Destructure.parse_param_specs (FVector params) with
            | Ok specs -> List.length specs
            | Error _ -> List.length params
          in
          let deftype_method_names =
            interface_forms
            |> List.filter_map (function
                 | FList (FSymbol method_name :: _) -> Some method_name
                 | _ -> None)
          in
          let rec predeclare_methods env names current_interface = function
            | [] -> Ok (env, List.sort_uniq String.compare names)
            | FSymbol interface_name :: rest ->
                let registered =
                  match Protocol.find_protocol_id scope env interface_name with
                  | None -> Ok env
                  | Some protocol_id -> (
                      match
                        Protocol_registry.find_protocol protocol_id
                          (Env.protocols env)
                      with
                      | Some declaration
                        when Protocol_registry.Method_map.is_empty
                               declaration.methods ->
                          Protocol_elaborator.add_marker_implementation env
                            protocol_id receiver_ty
                      | Some _ | None -> Ok env)
                in
                Result.bind registered (fun env ->
                    predeclare_methods env names (Some interface_name) rest)
            | FList (FSymbol method_name :: arities) :: rest
              when arities <> []
                   && List.for_all
                        (function
                          | FList (FVector _ :: _) -> true
                          | _ -> false)
                        arities ->
                let methods =
                  List.map
                    (function
                      | FList ((FVector _ as params) :: body_forms) ->
                          FList (FSymbol method_name :: params :: body_forms)
                      | _ -> assert false)
                    arities
                in
                predeclare_methods env names current_interface (methods @ rest)
            | FList
                (FSymbol method_name :: FVector params :: _body_forms)
              :: rest -> (
                match current_interface with
                | Some "IPrintWithWriter" ->
                    let source_name =
                      Expression_support.print_method_name record
                    in
                    let ocaml_name = Names.sanitize_name source_name in
                    let dynamic = Types.dynamic_constraint TUnknown in
                    let binding =
                      Types.binding ~forward_declared:true ocaml_name
                        (TFn
                           ( [ receiver_ty; TOcaml "Buffer.t"; dynamic ],
                             TUnit ))
                    in
                    predeclare_methods
                      (Env.add (Names.scoped_key scope source_name) binding env)
                      (ocaml_name :: names) current_interface rest
                | Some protocol_name -> (
                    match
                      ( Protocol.find_protocol_id scope env protocol_name,
                        Protocol.lookup_protocol_marker ~refine:false scope env
                          protocol_name method_name )
                    with
                    | Some _, Some marker ->
                        let arity = method_arity params in
                        let source_name =
                          Expression_support.deftype_method_name record
                            method_name arity
                        in
                        let ocaml_name = Names.sanitize_name source_name in
                        let method_ty =
                          let method_ty =
                            match marker.ty with
                            | TOverloaded_fn arities ->
                                arities
                                |> List.find_opt (fun (candidate : fn_arity) ->
                                       Option.is_none candidate.rest_param
                                       && List.length candidate.fixed_params
                                          = arity)
                                |> Option.map (fun candidate ->
                                       TFn
                                         ( candidate.fixed_params,
                                           candidate.return_ty ))
                                |> Option.value ~default:marker.ty
                            | method_ty -> method_ty
                          in
                          Types.instantiate_receiver_method_type receiver_ty
                            method_ty
                        in
                        let binding =
                          Types.binding ~forward_declared:true ocaml_name
                            method_ty
                        in
                        let implementation =
                          match marker.ty with
                          | TOverloaded_fn arities ->
                              let overload_targets =
                                List.map
                                  (fun (candidate : fn_arity) ->
                                    Expression_support.deftype_method_name record
                                      method_name
                                      (List.length candidate.fixed_params)
                                    |> Names.sanitize_name)
                                  arities
                              in
                              Types.binding ~forward_declared:true
                                ~overload_targets ocaml_name
                                (Types.instantiate_receiver_method_type
                                   receiver_ty marker.ty)
                          | _ -> binding
                        in
                        let registered =
                          match marker.ty with
                          | TOverloaded_fn _
                            when Option.is_some
                                   (Protocol.lookup_marker_impl env marker
                                      method_name receiver_ty) ->
                              Ok env
                          | _ ->
                              Protocol_elaborator.add_implementation env
                                method_name receiver_ty marker implementation
                        in
                        (match registered with
                        | Error _ as error -> error
                        | Ok env ->
                            predeclare_methods env (ocaml_name :: names)
                              current_interface rest)
                    | None, _ | _, None ->
                        predeclare_methods env names current_interface rest)
                | None ->
                    predeclare_methods env names current_interface rest)
            | _ :: rest ->
                predeclare_methods env names current_interface rest
          in
          Result.bind
            (predeclare_methods env [] None interface_forms)
            (fun (env, _implementation_names) ->
          let rec compile_methods env items current_interface = function
            | [] ->
                let ordinary, recursive =
                  List.rev items
                  |> List.fold_left
                       (fun (ordinary, recursive) -> function
                         | Recursive_value_binding
                             { name; identity; type_annotation; expression } ->
                             ( ordinary,
                               ({
                                  name;
                                  identity;
                                  type_annotation;
                                  expression;
                                }
                                 : recursive_value)
                               :: recursive )
                         | item -> (item :: ordinary, recursive))
                       ([], [])
                in
                let items =
                  List.rev ordinary
                  @
                  match List.rev recursive with
                  | [] -> []
                  | bindings -> [ Recursive_value_bindings bindings ]
                in
                Ok (scope, env, next_type, Group items)
            | FSymbol interface_name :: rest ->
                compile_methods env items (Some interface_name) rest
            | FList (FSymbol method_name :: arities) :: rest
              when arities <> []
                   && List.for_all
                        (function
                          | FList (FVector _ :: _) -> true
                          | _ -> false)
                        arities ->
                let methods =
                  List.map
                    (function
                      | FList ((FVector _ as params) :: body_forms) ->
                          FList (FSymbol method_name :: params :: body_forms)
                      | _ -> assert false)
                    arities
                in
                compile_methods env items current_interface (methods @ rest)
            | FList
                (FSymbol method_name
                :: (FVector params as params_form)
                :: body_forms)
              :: rest -> (
                let arity = method_arity params in
                let source_name =
                  if current_interface = Some "IPrintWithWriter" then
                    Expression_support.print_method_name record
                  else
                    Expression_support.deftype_method_name record method_name
                      arity
                in
                let ocaml_name = Names.sanitize_name source_name in
                let receiver_name, params_form =
                  match params with
                  | FSymbol "_" :: remaining ->
                      let receiver_name = "__lg_deftype_this" in
                      ( receiver_name,
                        FVector (FSymbol receiver_name :: remaining) )
                  | FSymbol receiver_name :: _ -> (receiver_name, params_form)
                  | _ -> ("__lg_deftype_this", params_form)
                in
                let rec unresolved_print_call = function
                  | FList (FSymbol name :: arguments) ->
                      let special_form =
                        List.mem name
                          [
                            "binding";
                            "do";
                            "fn";
                            "if";
                            "let";
                            "match";
                            "try";
                            "deref";
                            "pr-sequential-writer";
                            "pr-writer";
                          ]
                        || Option.is_some (Env.find_macro ~scope name env)
                      in
                      ((not special_form)
                      && not (String.starts_with ~prefix:"-" name)
                      && not (String.starts_with ~prefix:"." name)
                      && not (List.mem name deftype_method_names)
                      && Option.is_none
                           (Expression_support.untyped_first_class_function_error
                              name)
                      && Result.is_error (lookup_function scope env name))
                      || List.exists unresolved_print_call arguments
                  | FList forms | FVector forms ->
                      List.exists unresolved_print_call forms
                  | FMap pairs ->
                      List.exists
                        (fun (key, value) ->
                          unresolved_print_call key
                          || unresolved_print_call value)
                        pairs
                  | FSymbol _ | FInt _ | FFloat _ | FDecimal _ | FChar _
                  | FString _
                  | FRegex _ | FBool _ | FKeyword _ | FCoreSymbol _ ->
                      false
                in
                let skip_unresolved_print =
                  current_interface = Some "IPrintWithWriter"
                  && List.exists unresolved_print_call body_forms
                in
                let rec form_mentions name = function
                  | FSymbol candidate -> candidate = name
                  | FList
                      (FSymbol
                         ("record" | "clojure.core/record" | "cljs.core/record")
                      :: _type_name :: field_forms) ->
                      List.exists
                        (function
                          | FList [ _field_name; value ] ->
                              form_mentions name value
                          | form -> form_mentions name form)
                        field_forms
                  | FList forms | FVector forms ->
                      List.exists (form_mentions name) forms
                  | FMap pairs ->
                      List.exists
                        (fun (key, value) ->
                          form_mentions name key || form_mentions name value)
                        pairs
                  | FInt _ | FFloat _ | FDecimal _ | FChar _ | FString _
                  | FRegex _
                  | FBool _ | FKeyword _ | FCoreSymbol _ ->
                      false
                in
                let rec rewrite_mutable_assignments = function
                    | FList [ FSymbol "set!"; FSymbol field_name; value_form ]
                      -> (
                      let keyword = ":" ^ field_name in
                      match Types.find_field keyword record.fields with
                      | Some { mutable_ = true; _ } ->
                          FList
                              [
                                FSymbol "__deftype-field-set!";
                                FKeyword keyword;
                                FSymbol receiver_name;
                              rewrite_mutable_assignments value_form;
                            ]
                      | _ ->
                          FList
                              [
                                FSymbol "set!";
                              FSymbol field_name;
                              rewrite_mutable_assignments value_form;
                            ])
                  | FList forms ->
                      FList (List.map rewrite_mutable_assignments forms)
                  | FVector forms ->
                      FVector (List.map rewrite_mutable_assignments forms)
                  | FMap pairs ->
                      FMap
                        (List.map
                           (fun (key, value) ->
                             ( rewrite_mutable_assignments key,
                               rewrite_mutable_assignments value ))
                           pairs)
                  | form -> form
                in
                let compile_expanded_body body_forms =
                  let body_forms =
                    List.map rewrite_mutable_assignments body_forms
                  in
                  let parameter_names =
                    Destructure.pattern_names params_form
                  in
                let field_bindings =
                  record.fields
                  |> List.filter (fun (field : field) ->
                         let source_name =
                           Names.keyword_source_name field.keyword
                         in
                         (not (List.mem source_name parameter_names))
                         && List.exists (form_mentions source_name) body_forms)
                  |> List.concat_map (fun (field : field) ->
                         let source_name =
                           Names.keyword_source_name field.keyword
                         in
                        [
                          FSymbol source_name;
                           FList
                            [
                              FSymbol (".-" ^ source_name);
                               FSymbol receiver_name;
                             ];
                         ])
                in
                let body_forms =
                  [
                    FList
                      (FSymbol "let" :: FVector field_bindings :: body_forms);
                  ]
                in
                let param_type_overrides =
                  match (current_interface, method_name, params) with
                  | Some "IPrintWithWriter", "-pr-writer", [ _; _; _ ] ->
                      [
                        Some receiver_ty;
                        Some (TOcaml "Buffer.t");
                        Some TNil;
                      ]
                  | Some "ILookup", "-lookup", _receiver :: arguments ->
                      Some receiver_ty
                      :: List.map
                           (fun _ -> Some (Type_solver.fresh ()))
                           arguments
                  | _ -> [ Some receiver_ty ]
                in
                if skip_unresolved_print then
                  compile_methods
                    (Env.remove (Names.scoped_key scope source_name) env)
                    items current_interface rest
                else
                match
                  Expression_elaborator.compile_fn ~param_type_overrides scope
                    env params_form body_forms
                with
                | Error _ as err -> err
                | Ok implementation -> (
                    let binding = binding_of_expr ocaml_name implementation in
                    let register_protocol env =
                      match current_interface with
                      | Some "IPrintWithWriter" -> Ok env
                      | Some protocol_name
                        when Option.is_some
                                 (Protocol.find_protocol_id scope env
                                    protocol_name)
                             && Option.is_some
                                  (Protocol.lookup_protocol_marker ~refine:false
                                       scope env protocol_name method_name) -> (
                          match
                            Protocol_elaborator.marker scope env protocol_name
                              method_name
                          with
                          | Error _ as error -> error
                          | Ok ({ ty = TOverloaded_fn _; _ } as marker) ->
                              Protocol_elaborator.update_overloaded_implementation
                                env method_name receiver_ty marker binding
                          | Ok marker ->
                              Protocol_elaborator.add_implementation env
                                method_name receiver_ty marker binding)
                      | _ -> Ok env
                    in
                      match register_protocol env with
                    | Error _ as error -> error
                    | Ok env ->
                        let env =
                            Env.add
                              (Names.scoped_key scope source_name)
                              binding env
                        in
                        let item =
                          if
                            expression_references_declaration env
                              implementation.semantic_expr
                          then
                            let implementation_type =
                              match current_interface with
                              | Some protocol_name -> (
                                  match
                                    Protocol.lookup_protocol_marker
                                      ~refine:false scope env protocol_name
                                      method_name
                                  with
                                  | Some marker ->
                                      Protocol_elaborator
                                      .refine_protocol_implementation_type
                                        marker.ty implementation.ty
                                  | None -> implementation.ty)
                              | None -> implementation.ty
                            in
                            Deferred_value_binding
                              {
                                name = ocaml_name;
                                value_type =
                                  Protocol.refine_deferred_type env
                                    implementation_type;
                                return_param_index =
                                  implementation.return_param_index;
                                expression = implementation.semantic_expr;
                              }
                          else if
                            Semantic_ir.exists_identifier
                              (String.equal ocaml_name)
                              implementation.semantic_expr
                          then
                            Recursive_value_binding
                              {
                                name = ocaml_name;
                                identity = None;
                                type_annotation = None;
                                expression = implementation.semantic_expr;
                              }
                          else
                            Value_binding
                              {
                                pattern = Named ocaml_name;
                                expression = implementation.semantic_expr;
                              }
                        in
                        compile_methods env (item :: items) current_interface
                          rest)
                in
                match
                  Macro_expander.expand_all_forms ~scope ~compiler_env:env
                    body_forms
                with
                | Error _ as error -> error
                | Ok body_forms -> compile_expanded_body body_forms)
            | _ :: _ ->
                Error.error
                  "deftype methods must be (method-name [params] body...)"
          in
          compile_methods env [] None interface_forms))
  | FList
      (FSymbol "defmulti" :: FSymbol name :: dispatch_form :: option_forms) -> (
      match
        ( compile_multimethod_dispatch scope env dispatch_form,
          compile_multimethod_default scope env option_forms )
      with
      | Ok (arity, dispatch_fn), Ok default_dispatch ->
          let source_key = Names.scoped_key scope name in
          let ocaml_name = Names.ocaml_binding_name scope name in
          let dispatch_name = ocaml_name ^ "_dispatch_fn" in
          let arg_names = dynamic_arg_names arity in
          let invoke =
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Lg_runtime.Runtime_multimethod.invoke",
                [
                  Semantic_ir.String source_key;
                  Semantic_ir.List
                    (List.map (fun name -> Semantic_ir.Ident name) arg_names);
                ] )
          in
          let expression =
            Semantic_ir.Let
              ( [ (Semantic_ir.PVar dispatch_name, dispatch_fn) ],
                Semantic_ir.Sequence
                  [
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Lg_runtime.Runtime_multimethod.register",
                        [
                          Semantic_ir.String source_key;
                          Semantic_ir.Ident dispatch_name;
                          default_dispatch;
                        ] );
                    Semantic_ir.Fun
                      ( List.map (fun name -> Semantic_ir.PVar name) arg_names,
                        invoke );
                  ] )
          in
          let binding =
            Types.binding ~multimethod:true ocaml_name
              (TFn (List.init arity (fun _ -> dynamic_ty), dynamic_ty))
          in
          Ok
            ( scope,
              Env.add source_key binding env,
              next_type,
              Value_binding { pattern = Named ocaml_name; expression } )
      | (Error _ as error), _ | _, (Error _ as error) -> error)
  | FList
      (FSymbol "defmethod"
      :: FSymbol "print-method"
      :: FSymbol type_name
      :: (FVector _ as params_form)
      :: body_forms) -> (
      match Resolver.lookup_record_type scope env type_name with
      | Error _ as error -> error
      | Ok record -> (
            let source_name = Expression_support.print_method_name record in
            let ocaml_name = Names.sanitize_name source_name in
            match
              Expression_elaborator.compile_fn
                ~param_type_overrides:
                  [ Some (TNamed_record record); Some (TOcaml "Buffer.t") ]
                scope env params_form body_forms
            with
            | Error _ as error -> error
            | Ok implementation ->
                let binding = binding_of_expr ocaml_name implementation in
                let env =
                  Env.add (Names.scoped_key scope source_name) binding env
                in
                Ok
                  ( scope,
                    env,
                    next_type,
                    Value_binding
                    {
                      pattern = Named ocaml_name;
                        expression = implementation.semantic_expr;
                    } )))
  | FList
      (FSymbol "defmethod"
      :: FSymbol method_name
      :: dispatch_form
      :: (FVector _ as params_form)
      :: body_forms)
    when cljs_test_report_method_symbol scope method_name -> (
      let dynamic = Types.dynamic_constraint TUnknown in
      match
        ( Report_dynamic_boundary.compile_form ~compile_expr scope env
            dispatch_form,
          Expression_elaborator.compile_fn
            ~param_type_overrides:[ Some dynamic ]
            scope env params_form body_forms )
      with
      | Ok dispatch, Ok implementation ->
          let method_name = next_cljs_test_report_method_name () in
          let callback_name = method_name ^ "_callback" in
          let event_name = method_name ^ "_event" in
          let callback =
            Semantic_ir.Fun
              ( [ Semantic_ir.PVar event_name ],
                Semantic_ir.Sequence
                  [
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident callback_name,
                        [ Semantic_ir.Ident event_name ] );
                    Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.nil";
                  ] )
          in
          let expression =
            Semantic_ir.Let
              ( [ (Semantic_ir.PVar callback_name, implementation.semantic_expr) ],
                Semantic_ir.Sequence
                  [
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident
                          "Lg_runtime.Runtime_test_report.register",
                        [ dispatch.semantic_expr; callback ] );
                    Semantic_ir.Unit;
                  ] )
          in
          Ok
            ( scope,
              env,
              next_type,
              Value_binding { pattern = Named method_name; expression } )
      | (Error _ as error), _ | _, (Error _ as error) -> error)
  | FList (FSymbol "defmethod" :: FSymbol ("t/report" as method_name) :: _) ->
      Ok
        ( scope,
          env,
          next_type,
          Comment ("test runner handles " ^ method_name) )
  | FList
      (FSymbol "defmethod"
      :: FSymbol method_name
      :: dispatch_form
      :: (FVector params as params_form)
      :: body_forms) -> (
      let source_key = resolve_multimethod_key scope env method_name in
      match Env.find_opt source_key env with
      | None -> Error.error ("unknown multimethod " ^ method_name)
      | Some binding -> (
          match binding.ty with
          | TFn (parameter_tys, _) ->
              let arity = List.length parameter_tys in
              if arity <> List.length params then
                Error.error
                  ("defmethod for " ^ method_name ^ " expects "
                 ^ string_of_int arity ^ " parameters")
              else
                let overrides = List.map (fun _ -> Some dynamic_ty) params in
                (match
                   ( Multimethod_dynamic_boundary.compile_form ~compile_expr scope
                       env dispatch_form,
                     Expression_elaborator.compile_fn
                       ~param_type_overrides:overrides scope env params_form
                       body_forms )
                 with
                 | Ok dispatch, Ok implementation ->
                     let return_ty =
                       match implementation.ty with
                       | TFn (_, return_ty) -> return_ty
                       | ty -> ty
                     in
                     let method_name = next_multimethod_method_name () in
                     let implementation_name = method_name ^ "_implementation" in
                     let args_name = method_name ^ "_args" in
                     let raw_call =
                       Semantic_ir.Apply
                         ( Semantic_ir.Ident implementation_name,
                           List.init arity (list_nth args_name) )
                     in
                     (match
                        Multimethod_dynamic_boundary.convert_type return_ty raw_call
                      with
                      | Error _ as error -> error
                      | Ok dynamic_call ->
                          let callback =
                            Semantic_ir.Fun
                              ( [ Semantic_ir.PVar args_name ],
                                dynamic_call )
                          in
                          let expression =
                            Semantic_ir.Let
                              ( [
                                  ( Semantic_ir.PVar implementation_name,
                                    implementation.semantic_expr );
                                ],
                                Semantic_ir.Sequence
                                  [
                                    Semantic_ir.Apply
                                      ( Semantic_ir.Ident
                                          "Lg_runtime.Runtime_multimethod.register_method",
                                        [
                                          Semantic_ir.String source_key;
                                          dispatch.semantic_expr;
                                          callback;
                                        ] );
                                    Semantic_ir.Unit;
                                  ] )
                          in
                          Ok
                            ( scope,
                              env,
                              next_type,
                              Value_binding
                                { pattern = Named method_name; expression } ))
                 | (Error _ as error), _ | _, (Error _ as error) -> error)
          | _ -> Error.error (method_name ^ " is not a multimethod")))
  | FList (FSymbol "defmethod" :: _) ->
      Error.error "defmethod currently supports print-method"
  | FList (FSymbol "recursive-definition-group" :: definitions) ->
      let env =
        definitions
        |> List.fold_left
             (fun env -> function
               | FList
                   (FSymbol ("defn" | "defn-") :: FSymbol name
                   :: (FVector _ as params) :: _) ->
                   let key = Names.scoped_key scope name in
                   let ocaml_name = Names.ocaml_binding_name scope name in
                   (match sidecar_function_signature scope env name with
                   | Some ty ->
                       Env.add key
                         (Types.binding ocaml_name
                            (Function_elaborator.infer_named_record scope env
                               ty))
                         env
                   | None -> (
                       match Env.find_opt key env with
                       | Some (binding : binding)
                         when not
                                (Types.equal binding.ty
                                   (TOcaml "__declared_fn")) ->
                           env
                       | Some _ | None ->
                           Env.add key
                             (Types.binding ocaml_name
                                (match Destructure.parse_param_specs params with
                                | Ok specs ->
                                    let parameter_tys =
                                      List.map
                                        (fun (spec : Destructure.param_spec) ->
                                          Option.value spec.explicit_ty
                                            ~default:(Type_solver.fresh ())
                                          |> Function_elaborator
                                             .infer_named_record scope env)
                                        specs
                                    in
                                    TFn
                                      (parameter_tys, Type_solver.fresh ())
                                | Error _ -> TOcaml "__declared_fn"))
                             env))
               | _ -> env)
             env
      in
      let method_definitions, function_definitions =
        List.partition
          (function
            | FList (FSymbol "deftype-methods" :: _) -> true
            | _ -> false)
          definitions
      in
      let recursive_bindings item =
        let items = match item with Group items -> items | item -> [ item ] in
        let rec collect bindings = function
          | [] -> Ok (List.rev bindings)
          | Value_binding { pattern = Named name; expression } :: rest ->
              collect
                ({
                   name;
                   identity = None;
                   type_annotation = None;
                   expression;
                 }
                :: bindings)
                rest
          | Deferred_value_binding { name; expression; _ } :: rest ->
              collect
                ({
                   name;
                   identity = None;
                   type_annotation = None;
                   expression;
                 }
                :: bindings)
                rest
          | Recursive_value_binding
              { name; identity; type_annotation; expression } :: rest ->
              collect
                ({ name; identity; type_annotation; expression } :: bindings)
                rest
          | Recursive_value_bindings recursive :: rest ->
              collect (List.rev_append recursive bindings) rest
          | _ :: _ ->
              Error.error
                "recursive deftype methods must compile to named functions"
        in
        collect [] items
      in
      let rec compile_methods env next_type bindings = function
        | [] -> Ok (env, next_type, bindings)
        | method_form :: rest -> (
            match compile scope env next_type method_form with
            | Error _ as error -> error
            | Ok (_, env, next_type, item) -> (
                match recursive_bindings item with
                | Error _ as error -> error
                | Ok methods ->
                    compile_methods env next_type
                      (List.rev_append methods bindings)
                      rest))
      in
      let refine_predeclared_bindings env name predeclared_ty actual_ty =
        match Type_solver.unify Type_solver.empty predeclared_ty actual_ty with
        | Error _ ->
            Error.error
              ("recursive function " ^ name
             ^ " implementation does not match its inferred signature: "
             ^ Types.source_name predeclared_ty ^ " vs "
             ^ Types.source_name actual_ty)
        | Ok substitutions ->
            Ok
              (Env.fold
                 (fun key (binding : binding) env ->
                   Env.add key
                     {
                       binding with
                       ty = Type_solver.apply substitutions binding.ty;
                     }
                     env)
                 env env)
      in
      let rec compile_definitions env next_type row_items bindings = function
        | [] ->
            Ok
              ( scope,
                env,
                next_type,
                Group
                  (List.rev row_items
                  @ [ Recursive_value_bindings (List.rev bindings) ]) )
        | FList
            (FSymbol ("defn" | "defn-")
            :: (FSymbol name as name_form)
            :: (FList _ as first_clause)
            :: remaining_clauses)
          :: rest -> (
            let ocaml_name = Names.ocaml_binding_name scope name in
            match
              Expression_elaborator.prepare_multi_arity_fn ~ocaml_name scope env
                ~infer_state_return:true name
                (first_clause :: remaining_clauses)
            with
            | Error _ as error -> error
            | Ok prepared ->
                let env, next_type, local_type_items, prepared =
                  allocate_multi_arity_local_records env next_type prepared
                in
                let targets, overload_row_param_types, rows, arity_bindings =
                  Expression_elaborator.lower_prepared_multi_arity prepared
                in
                let binding =
                  Types.binding ~overload_targets:targets
                    ~overload_row_param_types ocaml_name
                    prepared.expr.ty
                in
                let env =
                  Env.add (Names.scoped_key scope name) binding env
                in
                let dispatch_binding =
                  {
                    name = ocaml_name;
                    identity =
                      Source_context.find name_form
                      |> Option.map (fun location ->
                             (Source_node_id.of_location location, location));
                    type_annotation = None;
                    expression = prepared.expr.semantic_expr;
                  }
                in
                let new_bindings = arity_bindings @ [ dispatch_binding ] in
                compile_definitions env next_type
                  (List.rev_append (local_type_items @ rows) row_items)
                  (List.rev_append new_bindings bindings)
                  rest)
        | FList
            (FSymbol ("defn" | "defn-")
            :: (FSymbol name as name_form)
            :: params :: body_forms)
          :: rest -> (
            let ocaml_name = Names.ocaml_binding_name scope name in
            let recursive = function_is_recursive scope name body_forms in
            let predeclared_type =
              match sidecar_function_signature scope env name with
              | Some ty ->
                  Some
                    (Function_elaborator.infer_named_record scope env ty)
              | None ->
                  Env.find_opt (Names.scoped_key scope name) env
                  |> Option.map (fun (binding : binding) ->
                         Function_elaborator.infer_named_record scope env
                           binding.ty)
            in
            let declared_return_ty =
              match predeclared_type with
              | Some (TFn (_, return_ty))
                when not (Types.equal return_ty TUnknown)
                     && not (Types.is_dynamic return_ty)
                     &&
                     (match return_ty with
                     | TMeta _ | TVar _ -> false
                     | _ -> true) ->
                  Some return_ty
              | Some _ | None -> None
            in
            let predeclared_param_tys =
              match predeclared_type with
              | Some (TFn (parameter_tys, _)) -> Some parameter_tys
              | Some _ | None -> None
            in
            let prepared =
              match (recursive, params, declared_return_ty) with
              | true, FVector _, Some return_ty ->
                  prepare_recursive_fn ~ocaml_name scope env name return_ty
                    params body_forms
              | true, FVector _, None ->
                  prepare_inferred_recursive_fn ~ocaml_name scope env name
                    params body_forms
              | _ ->
                  let param_type_overrides =
                    predeclared_param_tys
                    |> Option.value ~default:[]
                    |> List.map Option.some
                  in
                  let refine_open_overrides =
                    predeclared_param_tys
                    |> Option.value ~default:[]
                    |> List.exists (fun ty ->
                           Type_solver.variables ty
                           |> List.exists (function
                                | Type_solver.Metavariable _ -> true
                                | Type_solver.Declared _ -> false))
                  in
                  prepare_fn ~param_type_overrides ~refine_open_overrides
                    ~materialize_open_equality:true ?expected_return_ty:declared_return_ty
                    scope env params body_forms
            in
            match prepared with
            | Error error ->
                Error
                  (Error.with_location_if_missing
                     (Source_context.find name_form) error)
            | Ok parts ->
                let env, next_type, return_type_items, parts =
                  allocate_function_return_record env next_type parts
                in
                let env, next_type, local_type_items, parts =
                  allocate_function_local_records env next_type parts
                in
                let param_tys =
                  parts.param_bindings
                  |> List.map (fun (_key, (binding : binding)) -> binding.ty)
                in
                let row_param_types =
                  row_param_type_names ~env ocaml_name param_tys
                in
                let expr =
                  fn_code ~row_param_type_names:row_param_types parts
                in
                let binding =
                  Types.binding ~row_param_types
                    ?return_param_index:expr.return_param_index ocaml_name
                    (Types.align_deferred_param_types expr.ty expr.semantic_expr)
                in
                let refined_env =
                  match predeclared_type with
                  | Some predeclared_ty ->
                      refine_predeclared_bindings env name predeclared_ty
                        expr.ty
                  | None -> Ok env
                in
                Result.bind refined_env (fun env ->
                    let env =
                      Env.add (Names.scoped_key scope name) binding env
                    in
                    let type_annotation =
                      recursive_type_annotation scope env name
                    in
                    let rows =
                      return_type_items @ local_type_items
                      @ row_type_items row_param_types param_tys
                    in
                    let recursive_binding =
                      {
                        name = ocaml_name;
                        identity =
                          Source_context.find name_form
                          |> Option.map (fun location ->
                                 ( Source_node_id.of_location location,
                                   location ));
                        type_annotation =
                          type_annotation;
                        expression = expr.semantic_expr;
                      }
                    in
                    compile_definitions env next_type
                      (List.rev_append rows row_items)
                      (recursive_binding :: bindings)
                      rest))
        | _ :: _ ->
            Error.error
              "recursive definition groups only support functions and deftype methods"
      in
      Result.bind (compile_methods env next_type [] method_definitions)
        (fun (env, next_type, method_bindings) ->
          compile_definitions env next_type [] method_bindings
            function_definitions)
  | FList
      [
        FSymbol "defn-signature";
        FList
          (FSymbol ("defn" | "defn-")
          :: FSymbol name
          :: (FList _ as first_clause)
          :: remaining_clauses);
      ] -> (
      let ocaml_name = Names.ocaml_binding_name scope name in
      match
        Expression_elaborator.prepare_multi_arity_fn ~ocaml_name scope env name
          (first_clause :: remaining_clauses)
      with
      | Error _ ->
          Ok
            ( scope,
              env,
              next_type,
              Comment ("deferred function signature " ^ name) )
      | Ok prepared ->
          let targets, overload_row_param_types, _, _ =
            Expression_elaborator.lower_prepared_multi_arity prepared
          in
          let binding =
            Types.binding ~overload_targets:targets ~overload_row_param_types
              ocaml_name prepared.expr.ty
          in
          Ok
            ( scope,
              Env.add (Names.scoped_key scope name) binding env,
              next_type,
              Comment ("function signature " ^ name) ))
  | FList
      [
        FSymbol "defn-signature";
        FList
          (FSymbol ("defn" | "defn-") :: FSymbol name :: params :: body_forms);
      ] -> (
      let ocaml_name = Names.ocaml_binding_name scope name in
      let recursive = function_is_recursive scope name body_forms in
      let prepared =
        match (recursive, params) with
        | true, FVector _ ->
            prepare_inferred_recursive_fn ~ocaml_name scope env name params
              body_forms
        | _ ->
            prepare_fn ~materialize_open_equality:true scope env params
              body_forms
      in
      match prepared with
      | Error _ ->
          Ok
            ( scope,
              env,
              next_type,
              Comment ("deferred function signature " ^ name) )
      | Ok parts ->
          let param_tys =
            parts.param_bindings
            |> List.map (fun (_key, (binding : binding)) -> binding.ty)
          in
          let row_param_types = row_param_type_names ~env ocaml_name param_tys in
          let expression =
            fn_code ~row_param_type_names:row_param_types parts
          in
          let binding =
            binding_of_expr ~row_param_types ocaml_name expression
          in
          Ok
            ( scope,
              Env.add (Names.scoped_key scope name) binding env,
              next_type,
              Comment ("function signature " ^ name) ))
  | FList
      (FSymbol "module-signature"
      :: (FSymbol signature_name as name_form)
      :: item_forms) ->
      compile_module_signature
        ?location:(Source_context.find name_form)
        scope env next_type signature_name item_forms
  | FList (FSymbol "module-signature" :: _) ->
      Error.error "module-signature expects a name and signature items"
  | FList [ FSymbol "signature"; FSymbol name; fields ] ->
      compile_signature scope env next_type name fields
  | FList
      [ FSymbol "signature"; FSymbol name; type_parameters_form; fields ] -> (
      match parse_type_parameters type_parameters_form with
      | Error _ as error -> error
      | Ok type_parameters ->
          compile_signature ~type_parameters scope env next_type name fields)
  | FList (FSymbol "signature" :: _) ->
      Error.error
        "signature expects a name, optional type parameters, and a type or \
         record field map"
  | FList (FSymbol "dynamic-codec" :: _) ->
      Error.error
        "dynamic-codec is not supported; use explicit sum constructors"
  | FList [ FSymbol "type-alias"; (FSymbol name as name_form); manifest_form ]
    ->
      compile_type_alias
        ?location:(Source_context.find name_form)
        scope env next_type name [] manifest_form
  | FList
      [
        FSymbol "type-alias";
        (FSymbol name as name_form);
        FVector parameter_forms;
        manifest_form;
      ] -> (
      match parse_type_parameters (FVector parameter_forms) with
      | Error _ as err -> err
      | Ok type_parameters ->
          compile_type_alias
            ?location:(Source_context.find name_form)
            scope env next_type name type_parameters manifest_form)
  | FList
      (FSymbol "type-record"
      :: (FSymbol name as name_form)
      :: FVector parameter_forms
      :: field_forms) -> (
      match parse_type_parameters (FVector parameter_forms) with
      | Error _ as err -> err
      | Ok type_parameters ->
          compile_type_record
            ?location:(Source_context.find name_form)
            scope env next_type name type_parameters field_forms)
  | FList (FSymbol "type-record" :: (FSymbol name as name_form) :: field_forms)
    ->
      compile_type_record
        ?location:(Source_context.find name_form)
        scope env next_type name [] field_forms
  | FList (FSymbol "type-record" :: _) ->
      Error.error "type-record expects a name and fields"
  | FList
      (FSymbol "type-variant"
      :: (FSymbol name as name_form)
      :: FVector parameter_forms
      :: constructor_forms) -> (
      match parse_type_parameters (FVector parameter_forms) with
      | Error _ as err -> err
      | Ok type_parameters ->
          compile_type_variant
            ?location:(Source_context.find name_form)
            scope env next_type name type_parameters constructor_forms)
  | FList
      (FSymbol "type-variant"
      :: (FSymbol name as name_form)
      :: constructor_forms) ->
      compile_type_variant
        ?location:(Source_context.find name_form)
        scope env next_type name [] constructor_forms
  | FList [ FSymbol "open"; (FSymbol module_path as module_form) ] ->
      let env = open_module_bindings scope env module_path in
      Ok
        ( scope,
          env,
          next_type,
          Open_module
            {
              module_name = Names.module_path_to_ocaml module_path;
              location = Source_context.find module_form;
            } )
  | FList [ FSymbol "include"; (FSymbol module_path as module_form) ] ->
      let env = open_module_bindings scope env module_path in
      Ok
        ( scope,
          env,
          next_type,
          Include_module
            {
              module_name = Names.module_path_to_ocaml module_path;
              location = Source_context.find module_form;
            } )
  | FList (FSymbol "include" :: _) -> Error.error "include expects one module"
  | FList
      [
        FSymbol "module-alias";
        (FSymbol alias_name as alias_form);
        (FSymbol target_name as target_form);
      ] ->
      compile_module_alias
        ?location:(Source_context.find alias_form)
        ?target_location:(Source_context.find target_form)
        scope env next_type alias_name target_name
  | FList (FSymbol "module-alias" :: _) ->
      Error.error "module-alias expects alias and target modules"
  | FList
      (FSymbol "module-functor"
      :: (FSymbol functor_name as name_form)
      :: parameter_form :: body_forms) ->
      compile_module_functor
        ?location:(Source_context.find name_form)
        scope env next_type functor_name parameter_form body_forms
  | FList (FSymbol "module-functor" :: _) ->
      Error.error
        "module-functor expects a name, [parameter signature ...], and body"
  | FList
      (FSymbol "module-apply"
      :: (FSymbol module_name as name_form)
      :: (FSymbol functor_name as functor_form)
      :: (_ :: _ as argument_forms)) -> (
      let rec parse_arguments acc = function
        | [] -> Ok (List.rev acc)
        | (FSymbol name as form) :: rest ->
            parse_arguments
              ({ module_name = name; location = Source_context.find form }
              :: acc)
              rest
        | _ ->
            Error.error
              "module-apply expects result, functor, and one or more argument \
               modules"
      in
      match parse_arguments [] argument_forms with
      | Error _ as err -> err
      | Ok argument_names ->
          compile_module_apply
            ?location:(Source_context.find name_form)
            ?functor_location:(Source_context.find functor_form)
            scope env next_type module_name functor_name argument_names)
  | FList (FSymbol "module-apply" :: _) ->
      Error.error
        "module-apply expects result, functor, and one or more argument modules"
  | FList
      [
        FSymbol (("def" | "defonce") as definition);
        (FSymbol _ as name_form);
        FString _docstring;
        expr_form;
      ] ->
      compile scope env next_type
        (FList [ FSymbol definition; name_form; expr_form ])
  | FList
      [
        FSymbol ("def" | "defonce");
        FSymbol "^:dynamic";
        (FSymbol name as name_form);
        expr_form;
      ] -> (
      let expected_ty = sidecar_function_signature scope env name in
      let expr_env = Env.with_expected_type expected_ty env in
      match compile_source_expr scope expr_env expr_form with
      | Error _ as error -> error
      | Ok expr ->
          let expr =
            match expected_ty with
            | None -> Ok expr
            | Some expected ->
                Result.map
                  (fun semantic_expr -> typed_ir expected semantic_expr)
                  (Call_elaborator.adapt_value_to_type env expected expr)
          in
          Result.bind expr (fun expr ->
          let ocaml_name = Names.ocaml_binding_name scope name in
          let env_key = Names.scoped_key scope name in
          Result.map
            (fun () ->
              let binding =
                Types.binding ~dynamically_bindable:true ocaml_name (TRef expr.ty)
              in
              ( scope,
                Env.add env_key binding env,
                next_type,
                Value_binding
                  {
                    pattern =
                      located_value_pattern name_form (Named ocaml_name);
                    expression =
                      Semantic_ir.Apply
                        ( Semantic_ir.Ident
                            "Lg_runtime.Runtime_reference.of_value",
                          [
                            (match expected_ty with
                            | Some _ ->
                                Semantic_ir.Constraint
                                  ( expr.semantic_expr,
                                    Types.ocaml_name expr.ty )
                            | None -> expr.semantic_expr);
                          ] );
                  } ))
            (check_emitted_name_collision env ~source_key:env_key ~ocaml_name))
          )
  | FList
      [ FSymbol ("def" | "defonce"); (FSymbol name as name_form); expr_form ]
    -> (
      let expected_ty =
        sidecar_function_signature scope env name
        |> Option.map (Function_elaborator.infer_named_record scope env)
      in
      let expr_env = Env.with_expected_type expected_ty env in
      let expr =
        Result.bind (compile_source_expr scope expr_env expr_form) (fun expr ->
            match expected_ty with
            | None -> Ok expr
            | Some expected ->
                Result.map
                  (fun semantic_expr -> typed_ir expected semantic_expr)
                  (Call_elaborator.adapt_value_to_type env expected expr))
      in
      match expr with
      | Error _ as err -> err
      | Ok expr when unresolved_contextual_type expr.ty ->
          Error.error "empty list requires a contextual element type"
      | Ok expr -> (
          let ocaml_name = Names.ocaml_binding_name scope name in
          let env_key = Names.scoped_key scope name in
          match
            check_emitted_name_collision env ~source_key:env_key ~ocaml_name
          with
          | Error _ as err -> err
          | Ok () -> (
              match expr.ty with
          | TRecord fields when not (Types.is_homogeneous_record fields) ->
                  let nested =
                    allocate_nested_anonymous_records ~owner:"" env next_type
                      fields
                  in
                  let fields = nested.nested_fields in
              let identity =
                Source_context.find name_form
                |> Option.map (fun location ->
                       (Source_node_id.of_location location, location))
              in
              let allocation =
                    allocate_anonymous_record ~owner:"" nested.env
                      nested.next_type fields
              in
              let record_ty = TNamed_record allocation.record in
              let binding = Types.binding ocaml_name record_ty in
              let env = Env.add env_key binding allocation.env in
              if allocation.fresh then
                let item =
                  match expr.record_values with
                  | Some values ->
                          let values =
                            List.map
                              (fun (field, value) ->
                                let field =
                                  find_field field.keyword fields
                                  |> Option.value ~default:field
                                in
                                (field, value))
                              values
                          in
                      Record_def
                            {
                              var_name = ocaml_name;
                          identity;
                          type_id = allocation.record.type_id;
                          type_name = allocation.record.type_name;
                          type_parameters = allocation.record.type_parameters;
                              set_module_name =
                                allocation.record.set_module_name;
                          fields;
                              values;
                            }
                  | None ->
                      Projected_record_def
                            {
                              var_name = ocaml_name;
                          identity;
                          type_id = allocation.record.type_id;
                          type_name = allocation.record.type_name;
                          type_parameters = allocation.record.type_parameters;
                              set_module_name =
                                allocation.record.set_module_name;
                          fields;
                              source = expr.semantic_expr;
                            }
                    in
                    let item =
                      match nested.items with
                      | [] -> item
                      | items -> Group (items @ [ item ])
                in
                Ok (scope, env, allocation.next_type, item)
              else
                    let expr =
                      Structural_map.as_named_record allocation.record expr
                    in
                    let item =
                    Value_binding
                        {
                          pattern =
                            located_value_pattern name_form (Named ocaml_name);
                          expression = expr.semantic_expr;
                        }
                    in
                    let item =
                      match nested.items with
                      | [] -> item
                      | items -> Group (items @ [ item ])
                    in
                    Ok (scope, env, allocation.next_type, item)
          | _ ->
              let env, next_type, record_items, expr =
                allocate_top_level_local_records env next_type expr
              in
              let binding =
                match expr_form with
                | FSymbol source_name -> (
                    match Resolver.lookup_binding scope env source_name with
                    | Ok source_binding ->
                        {
                          source_binding with
                          ocaml_name;
                          ty = expr.ty;
                          host_reference = None;
                          forward_declared = false;
                          dynamically_bindable = false;
                        }
                    | Error _ -> binding_of_expr ocaml_name expr)
                | _ -> binding_of_expr ocaml_name expr
              in
              let env = Env.add env_key binding env in
              let env =
                match expr_form with
                | FSymbol source_name -> (
                    match Env.find_inline_macro ~scope source_name env with
                    | Some definition ->
                        Env.add_inline_macro ~scope ~name definition env
                    | None -> env)
                | _ -> env
              in
              let item =
                Value_binding
                  {
                    pattern =
                      located_value_pattern name_form (Named ocaml_name);
                    expression =
                      (match expected_ty with
                      | Some _ ->
                          Semantic_ir.Constraint
                            (expr.semantic_expr, Types.ocaml_name expr.ty)
                      | None -> expr.semantic_expr);
                  }
              in
              let item =
                match record_items with
                | [] -> item
                | items -> Group (items @ [ item ])
              in
              Ok (scope, env, next_type, item))))
  | FList
      (FSymbol (("defn" | "defn-") as definition)
      :: (FSymbol name as name_form)
      :: FMap attributes
      :: rest) ->
      let inline_definition =
        attributes
        |> List.find_map (function
             | FKeyword ":inline", FList (FSymbol "fn" :: forms) ->
                 Some
                   (Macro_definition.create ~namespace:scope ~name forms)
             | _ -> None)
      in
      let inherited_inline_macros = Env.inline_macros env in
      let runtime_env = Env.clear_inline_macros env in
      Result.bind
        (compile scope runtime_env next_type
           (FList (FSymbol definition :: name_form :: rest)))
        (fun (scope, env, next_type, item) ->
          let env = Env.with_inline_macros inherited_inline_macros env in
          match inline_definition with
          | None -> Ok (scope, env, next_type, item)
          | Some (Error _ as error) -> error
          | Some (Ok inline_definition) ->
              Ok
                ( scope,
                  Env.add_inline_macro ~scope ~name inline_definition env,
                  next_type,
                  item ))
  | FList
      (FSymbol (("defn" | "defn-") as definition)
      :: (FSymbol _ as name_form)
      :: FList [ FSymbol "__type-hint"; FSymbol annotation; params ]
      :: body_forms) ->
      compile scope env next_type
        (FList
           (FSymbol definition :: name_form :: FSymbol annotation :: params
          :: body_forms))
  | FList
      (FSymbol (("defn" | "defn-") as definition)
      :: (FSymbol _ as name_form)
      :: FSymbol annotation
      :: FString _docstring
      :: forms)
    when String.starts_with ~prefix:"^" annotation ->
      compile scope env next_type
        (FList
           (FSymbol definition :: name_form :: FSymbol annotation :: forms))
  | FList
      (FSymbol (("defn" | "defn-") as definition)
      :: (FSymbol _ as name_form)
      :: FSymbol annotation
      :: (FList _ as first_clause)
      :: remaining_clauses)
    when String.starts_with ~prefix:"^" annotation ->
      if annotation = "^:dynamic" then
        Type_annotation.reject_dynamic_type ()
      else
        let annotate_clause = function
          | FList (params :: body_forms) ->
              let body =
                match body_forms with
                | [ body ] -> body
                | body_forms -> FList (FSymbol "do" :: body_forms)
              in
              FList
                [
                  params;
                  FList [ FSymbol "__type-hint"; FSymbol annotation; body ];
                ]
          | clause -> clause
        in
        compile scope env next_type
          (FList
             (FSymbol definition :: name_form
             :: List.map annotate_clause
                  (first_clause :: remaining_clauses)))
  | FList
      (FSymbol (("defn" | "defn-") as definition)
      :: (FSymbol name as name_form)
      :: FSymbol annotation
      :: params :: body_forms)
    when String.starts_with ~prefix:"^" annotation -> (
      if annotation = "^:dynamic" then
        Type_annotation.reject_dynamic_type ()
      else if not (function_is_recursive scope name body_forms) then
        let body =
          match body_forms with
          | [ body ] -> body
          | body_forms -> FList (FSymbol "do" :: body_forms)
        in
        compile scope env next_type
          (FList
             [
               FSymbol definition;
               name_form;
               params;
               FList [ FSymbol "__type-hint"; FSymbol annotation; body ];
             ])
      else
      match Type_annotation.of_param_annotation annotation with
      | Error _ as error -> error
      | Ok _ ->
          compile scope env next_type
            (FList
               (FSymbol definition :: name_form :: params
              :: FList
                   [ FSymbol "__return-type"; FSymbol annotation ]
              :: body_forms)))
  | FList
      (FSymbol (("defn" | "defn-") as definition)
      :: (FSymbol _ as name_form)
      :: FString _docstring
      :: forms) ->
      compile scope env next_type
        (FList (FSymbol definition :: name_form :: forms))
  | FList
      (FSymbol ("defn" | "defn-")
      :: (FSymbol name as name_form)
      :: (FList _ as first_clause)
      :: remaining_clauses) -> (
      let ocaml_name = Names.ocaml_binding_name scope name in
      let env_key = Names.scoped_key scope name in
      let recursive =
        function_is_recursive scope name (first_clause :: remaining_clauses)
      in
      match
        check_emitted_name_collision env ~source_key:env_key ~ocaml_name
      with
      | Error _ as err -> err
      | Ok () -> (
          let signature =
            match
              sidecar_function_signature scope env name
              |> Option.map (Function_elaborator.infer_named_record scope env)
            with
            | Some (TOverloaded_fn arities) -> Some arities
            | Some _ | None -> None
          in
          match
            Expression_elaborator.prepare_multi_arity_fn ?signature ~ocaml_name
              scope env name
              (first_clause :: remaining_clauses)
          with
          | Error _ as err -> err
          | Ok prepared ->
              let env, next_type, local_type_items, prepared =
                allocate_multi_arity_local_records env next_type prepared
              in
              let targets, overload_row_param_types, row_items,
                  recursive_bindings =
                Expression_elaborator.lower_prepared_multi_arity prepared
              in
              let binding =
                Types.binding ~overload_targets:targets
                  ~overload_row_param_types ocaml_name
                  prepared.expr.ty
                |> Types.generalize_binding
              in
              let binding, value_item =
                if
                  requires_stable_forward_binding env env_key
                    prepared.expr.semantic_expr
                then
                  let value_type = deferred_value_type env prepared.expr in
                  ( { binding with ty = value_type },
                    Deferred_value_binding
                      {
                        name = ocaml_name;
                        value_type;
                        return_param_index = prepared.expr.return_param_index;
                        expression = prepared.expr.semantic_expr;
                      } )
                else
                  ( binding,
                    Value_binding
                      {
                        pattern =
                          located_value_pattern name_form (Named ocaml_name);
                        expression = prepared.expr.semantic_expr;
                      } )
              in
              let arity_items =
                if recursive then
                  [ Recursive_value_bindings recursive_bindings ]
                else
                  List.map
                    (fun ({ name; expression; _ } : Lowered.recursive_value) ->
                      Value_binding
                        { pattern = Named name; expression })
                    recursive_bindings
              in
              Ok
                ( scope,
                  Env.add env_key binding env,
                  next_type,
                  Group
                    (local_type_items @ row_items
                    @ arity_items @ [ value_item ]) )))
  | FList
      (FSymbol (("defn" | "defn-") as definition)
      :: (FSymbol _name as name_form)
      :: (FVector params as params_form)
      :: body_forms)
    when List.exists (function FSymbol "&" -> true | _ -> false) params ->
      compile scope env next_type
        (FList
           [ FSymbol definition; name_form; FList (params_form :: body_forms) ])
  | FList
      (FSymbol ("defn" | "defn-")
      :: (FSymbol name as name_form)
      :: params
      :: ((FKeyword _ | FList [ FSymbol "__return-type"; FSymbol _ ]) as
          return_annotation)
      :: body_forms) -> (
      let return_type =
        match return_annotation with
        | FKeyword keyword -> Type_annotation.of_keyword keyword
        | FList [ FSymbol "__return-type"; FSymbol annotation ] ->
            Type_annotation.of_param_annotation annotation
        | _ -> assert false
      in
      match return_type with
      | Error _ as err -> err
      | Ok return_ty -> (
          let return_ty =
            Function_elaborator.infer_named_record scope env return_ty
          in
          let ocaml_name = Names.ocaml_binding_name scope name in
          match
             prepare_inferred_recursive_fn_with_return ~ocaml_name scope env
               name return_ty params body_forms
           with
          | Error _ as err -> err
          | Ok parts -> (
              let param_tys =
                parts.param_bindings
                |> List.map (fun (_key, (binding : binding)) -> binding.ty)
              in
              let row_param_types = row_param_type_names ~env ocaml_name param_tys in
              let expr = fn_code ~row_param_type_names:row_param_types parts in
              let env_key = Names.scoped_key scope name in
              match
                 check_emitted_name_collision env ~source_key:env_key ~ocaml_name
               with
              | Error _ as err -> err
              | Ok () ->
                  let binding =
                    binding_of_expr ~row_param_types ocaml_name expr
                  in
                  let type_items = row_type_items row_param_types param_tys in
                  let binding, value_item =
                    if
                      requires_stable_forward_binding env env_key
                        expr.semantic_expr
                    then
                      let value_type = deferred_value_type env expr in
                      ( { binding with ty = value_type },
                        Deferred_value_binding
                          {
                            name = ocaml_name;
                            value_type;
                            return_param_index = expr.return_param_index;
                            expression = expr.semantic_expr;
                          } )
                    else
                      ( binding,
                        Recursive_value_binding
                          {
                            name = ocaml_name;
                            identity =
                              Source_context.find name_form
                              |> Option.map (fun location ->
                                     (Source_node_id.of_location location, location));
                            type_annotation = None;
                            expression = expr.semantic_expr;
                          } )
                  in
                  Ok
                    ( scope,
                      Env.add env_key binding env,
                      next_type,
                      Group (type_items @ [ value_item ]) ))))
  | FList
      (FSymbol ("defn" | "defn-")
      :: (FSymbol name as name_form)
      :: params :: body_forms)
    when function_is_recursive scope name body_forms -> (
      let ocaml_name = Names.ocaml_binding_name scope name in
      let env_key = Names.scoped_key scope name in
      let prepared =
        match
          sidecar_function_signature scope env name
          |> Option.map (Function_elaborator.infer_named_record scope env)
        with
        | Some (TFn (parameter_tys, return_ty) as signature_ty) ->
            let signature_env =
              Env.add env_key
                (Types.binding ocaml_name signature_ty)
                env
            in
            let parameter_count =
              match Destructure.parse_param_specs params with
              | Ok specs -> List.length specs
              | Error _ -> -1
            in
            if List.length parameter_tys <> parameter_count then
              Error.error
                ("function signature arity does not match recursive defn "
               ^ name)
            else
              prepare_recursive_fn ~ocaml_name scope signature_env name
                return_ty params body_forms
        | Some _ -> Error.error ("function signature expected for " ^ name)
        | None ->
            prepare_inferred_recursive_fn ~ocaml_name scope env name params
              body_forms
      in
      match prepared with
      | Error _ as err -> err
      | Ok parts -> (
          let param_tys =
            parts.param_bindings
            |> List.map (fun (_key, (binding : binding)) -> binding.ty)
          in
          let row_param_types = row_param_type_names ~env ocaml_name param_tys in
          let expr = fn_code ~row_param_type_names:row_param_types parts in
          match
            check_emitted_name_collision env ~source_key:env_key ~ocaml_name
          with
          | Error _ as err -> err
          | Ok () ->
              let binding = binding_of_expr ~row_param_types ocaml_name expr in
              let type_items = row_type_items row_param_types param_tys in
              let binding, value_item =
                if
                  requires_stable_forward_binding env env_key
                    expr.semantic_expr
                then
                  let value_type = deferred_value_type env expr in
                  ( { binding with ty = value_type },
                    Deferred_value_binding
                      {
                        name = ocaml_name;
                        value_type;
                        return_param_index = expr.return_param_index;
                        expression = expr.semantic_expr;
                      } )
                else
                  ( binding,
                    Recursive_value_binding
                      {
                        name = ocaml_name;
                        identity =
                          Source_context.find name_form
                          |> Option.map (fun location ->
                                 (Source_node_id.of_location location, location));
                        type_annotation =
                          recursive_type_annotation scope env name;
                        expression = expr.semantic_expr;
                      } )
              in
              Ok
                ( scope,
                  Env.add env_key binding env,
                  next_type,
                  Group (type_items @ [ value_item ]) )))
  | FList
      (FSymbol ("defn" | "defn-")
      :: (FSymbol name as name_form)
      :: params :: body_forms) -> (
      match prepare_function scope env name params body_forms with
      | Error _ as err -> err
      | Ok parts when unresolved_contextual_type parts.body.ty ->
          Error.error "empty list requires a contextual element type"
      | Ok parts -> (
          let env, next_type, return_type_items, parts =
            allocate_function_return_record env next_type parts
          in
          let env, next_type, local_type_items, parts =
            allocate_function_local_records env next_type parts
          in
          let ocaml_name = Names.ocaml_binding_name scope name in
          let param_tys =
            parts.param_bindings
            |> List.map (fun (_key, (binding : binding)) -> binding.ty)
          in
          let row_param_types = row_param_type_names ~env ocaml_name param_tys in
          let expr = fn_code ~row_param_type_names:row_param_types parts in
          let env_key = Names.scoped_key scope name in
          match
            check_emitted_name_collision env ~source_key:env_key ~ocaml_name
          with
          | Error _ as err -> err
          | Ok () -> (
              match expr.ty with
              | TFn _ ->
                  let published_ty =
                    if preserves_required_seqable_protocol_result expr then
                      expr.ty
                    else Protocol.refine_source_function_type env expr.ty
                  in
                  let binding =
                    binding_of_expr ~row_param_types ocaml_name
                      { expr with ty = published_ty }
                  in
                  let type_items =
                    return_type_items @ local_type_items
                    @ row_type_items row_param_types param_tys
                  in
                  let binding, value_item =
                    if
                      requires_stable_forward_binding env env_key
                        expr.semantic_expr
                    then
                      let value_type = deferred_value_type env expr in
                      ( { binding with ty = value_type },
                        Deferred_value_binding
                          {
                            name = ocaml_name;
                            value_type;
                            return_param_index = expr.return_param_index;
                            expression = expr.semantic_expr;
                          } )
                    else
                      let redefable =
                        source_scope_redefable_roots scope
                        && not (contains_unresolved_type published_ty)
                      in
                      if redefable then
                        let root_name = redef_root_name ocaml_name in
                        ( redefable_binding binding,
                          Group
                            [
                              Value_binding
                                {
                                  pattern = Named root_name;
                                  expression =
                                    runtime_root_expression expr.semantic_expr;
                                };
                              Value_binding
                                {
                                  pattern =
                                    located_value_pattern name_form
                                      (Named ocaml_name);
                                  expression =
                                    redefable_function_wrapper root_name
                                      published_ty;
                                };
                            ] )
                      else
                        ( binding,
                          Value_binding
                            {
                              pattern =
                                located_value_pattern name_form
                                  (Named ocaml_name);
                              expression = expr.semantic_expr;
                            } )
                  in
                  Ok
                    ( scope,
                      Env.add env_key binding env,
                      next_type,
                      Group (type_items @ [ value_item ]) )
          | _ -> Error.error "defn body did not compile to a function")))
  | FList
      (FSymbol "defprotocol"
      :: (FSymbol protocol_name as name_form)
      :: method_forms) ->
      compile_defprotocol
        ?location:(Source_context.find name_form)
        scope env next_type protocol_name method_forms
  | FList
      (FSymbol ("extend-type" | "extend-type-no-register")
      :: receiver_form :: implementations) ->
      let rec groups grouped current = function
        | [] -> (
            match current with
            | None -> Ok (List.rev grouped)
            | Some (protocol_name, methods) ->
                Ok (List.rev ((protocol_name, List.rev methods) :: grouped)))
        | FSymbol protocol_name :: rest ->
            let grouped =
              match current with
              | None -> grouped
              | Some (previous, methods) ->
                  (previous, List.rev methods) :: grouped
            in
            groups grouped (Some (protocol_name, [])) rest
        | (FList _ as method_form) :: rest -> (
            match current with
            | None -> Error.error "extend-type requires a protocol name"
            | Some (protocol_name, methods) ->
                groups grouped
                  (Some (protocol_name, method_form :: methods))
                  rest)
        | _ :: _ -> Error.error "invalid extend-type implementation"
      in
      let items_of = function Group items -> items | item -> [ item ] in
      Result.bind (groups [] None implementations) (fun groups ->
          Result.bind
            (predeclare_protocol_groups scope env receiver_form groups)
            (fun env ->
          let compile_registrations _env = Ok [] in
          let rec compile_groups env next_type items = function
            | [] ->
                Result.map
                  (fun registrations ->
                    (scope, env, next_type, Group (items @ registrations)))
                  (compile_registrations env)
            | (protocol_name, methods) :: rest -> (
                match
                  compile_extend_type scope env next_type receiver_form
                    protocol_name methods
                with
                | Error _ as error -> error
                | Ok (_, env, next_type, item) ->
                    compile_groups env next_type (items @ items_of item) rest)
          in
          compile_groups env next_type [] groups))
  | FList (FSymbol "extend-protocol" :: FSymbol protocol_name :: implementations)
    -> (
      let is_receiver = function
        | FSymbol _ | FKeyword _ -> true
        | _ -> false
      in
      let rec groups grouped current = function
        | [] -> (
            match current with
            | None -> Ok (List.rev grouped)
            | Some (receiver, methods) ->
                Ok (List.rev ((receiver, List.rev methods) :: grouped)))
        | receiver :: rest when is_receiver receiver ->
            let grouped =
              match current with
              | None -> grouped
              | Some (previous, methods) ->
                  (previous, List.rev methods) :: grouped
            in
            groups grouped (Some (receiver, [])) rest
        | (FList _ as method_form) :: rest -> (
            match current with
            | None ->
                Error.error "extend-protocol method requires a receiver type"
            | Some (receiver, methods) ->
                groups grouped (Some (receiver, method_form :: methods)) rest)
        | _ :: _ -> Error.error "invalid extend-protocol implementation"
      in
      let items_of = function Group items -> items | item -> [ item ] in
      match groups [] None implementations with
      | Error _ as error -> error
      | Ok groups ->
          let rec compile_groups env next_type items = function
            | [] -> Ok (scope, env, next_type, Group items)
            | (receiver, methods) :: rest -> (
                match
                  compile scope env next_type
                    (FList
                       (FSymbol "extend-type" :: receiver
                      :: FSymbol protocol_name :: methods))
                with
                | Error _ as error -> error
                | Ok (_, env, next_type, item) ->
                    compile_groups env next_type (items @ items_of item) rest)
          in
          compile_groups env next_type [] groups)
  | FList
      (FSymbol "module"
      :: (FSymbol module_name as name_form)
      :: (FSymbol signature_name as signature_form)
      :: forms) -> (
      match
        compile_module
          ?location:(Source_context.find name_form)
          ~signature_name
          ?signature_location:(Source_context.find signature_form)
          scope env next_type module_name module_name forms
      with
      | Error _ as err -> err
      | Ok (scope, module_env, module_bindings, next_type, item) ->
          let env =
            env
            |> Env.with_protocols (Env.protocols module_env)
            |> Env.with_modules (Env.modules module_env)
            |> Env.with_types (Env.types module_env)
            |> Env.add_bindings module_bindings
          in
          Ok (scope, env, next_type, item))
  | FList (FSymbol "module" :: (FSymbol module_name as name_form) :: forms) -> (
      match
        compile_module
          ?location:(Source_context.find name_form)
          scope env next_type module_name module_name forms
      with
      | Error _ as err -> err
      | Ok (scope, module_env, module_bindings, next_type, item) ->
          let env =
            env
            |> Env.with_protocols (Env.protocols module_env)
            |> Env.with_modules (Env.modules module_env)
            |> Env.with_types (Env.types module_env)
            |> Env.add_bindings module_bindings
          in
          Ok (scope, env, next_type, item))
  | FList [ FSymbol "namespace-scope"; FSymbol namespace_name ] ->
      let env = Require.add_source_core_bindings env namespace_name in
      let env =
        Env.add (Names.scoped_key namespace_name "read-string")
          Core_edn.read_string_binding env
      in
      Ok
        (namespace_name, env, next_type, Comment ("namespace " ^ namespace_name))
  | FList (FSymbol "refer-clojure-exclude" :: names) ->
      let rec parse_names acc = function
        | [] -> Ok (List.rev acc)
        | FSymbol name :: rest -> parse_names (name :: acc) rest
        | _ -> Error.error ":refer-clojure :exclude expects a vector of symbols"
      in
      Result.map
        (fun names ->
          let env = Env.add_core_exclusions ~scope names env in
          let env =
            List.fold_left
              (fun env name -> Require.remove_source_core_binding env scope name)
              env names
          in
          (scope, env, next_type, Comment "refer-clojure exclude"))
        (parse_names [] names)
  | FList (FSymbol "defmacro" :: FSymbol name :: forms) ->
      Result.map
        (fun definition ->
          let env = Env.add_macro ~scope ~name definition env in
          (scope, env, next_type, Comment ("macro " ^ name)))
        (Macro_definition.create ~namespace:scope ~name forms)
  | FList (FSymbol "macro-helper-defn" :: FSymbol name :: forms) ->
      Result.map
        (fun definition ->
          let env = Env.add_macro_function ~scope ~name definition env in
          (scope, env, next_type, Comment ("macro helper " ^ name)))
        (Macro_definition.create ~namespace:scope ~name forms)
  | FList (FSymbol "macro-helper-def" :: FSymbol name :: forms) ->
      let value =
        match forms with [] -> FSymbol "nil" | value :: _ -> value
      in
      let env = Env.add_macro_value ~scope ~name value env in
      Ok (scope, env, next_type, Comment ("macro value " ^ name))
  | FList [ FSymbol ("def" as kind); FSymbol name ] ->
      compile scope env next_type
        (FList [ FSymbol kind; FSymbol name; FSymbol "nil" ])
  | FList (FSymbol "declare" :: names) ->
      let rec add_declarations env = function
        | [] -> Ok env
        | FSymbol name :: rest ->
            let key = Names.scoped_key scope name in
            let ocaml_name = Names.ocaml_binding_name scope name in
            let binding =
              match Env.find_opt key env with
              | Some binding
                when String.equal binding.ocaml_name ocaml_name
                     && not (Types.equal binding.ty (TOcaml "__declared_fn")) ->
                  binding
              | _ -> (
                  match sidecar_function_signature scope env name with
                  | Some ty ->
                      Types.binding ~forward_declared:true ocaml_name
                        (Function_elaborator.infer_named_record scope env ty)
                  | None ->
                      Types.binding ocaml_name (TOcaml "__declared_fn"))
            in
            let env = Env.add key binding env in
            add_declarations (Env.add_explicit_declaration key env) rest
        | _ -> Error.error "declare expects symbols"
      in
      Result.map
        (fun env -> (scope, env, next_type, Comment "declare"))
        (add_declarations env names)
  | FList (FSymbol "require" :: entries) -> (
      match Require.parse_entries entries with
      | Error _ as err -> err
      | Ok specs -> (
          let rec apply_specs env = function
            | [] -> Ok env
            | Require.Package _ :: rest -> apply_specs env rest
            | Require.Load { module_name } :: rest -> (
                let result =
                  if Require.core_namespace module_name then
                    Ok
                      (Require.add_core_alias_bindings env module_name
                         module_name)
                  else if String.starts_with ~prefix:"ocaml." module_name then
                    Ok
                      (Require.add_ocaml_alias_bindings env module_name
                         module_name)
                  else Require.ensure_namespace env module_name
                in
                match result with
                | Error _ as err -> err
                | Ok env -> apply_specs env rest)
            | Require.Alias { module_name; alias } :: rest -> (
                if String.starts_with ~prefix:"ocaml." module_name then
                  let env =
                    Require.add_ocaml_alias_bindings env module_name alias
                  in
                  apply_specs
                    (Env.add_namespace_alias ~scope ~alias ~target:module_name
                       env)
                    rest
                else if Require.core_namespace module_name then
                  let env =
                    Require.add_core_alias_bindings env module_name alias
                  in
                  apply_specs
                    (Env.add_namespace_alias ~scope ~alias ~target:module_name
                       env)
                    rest
                else
                  match Require.add_lg_alias_bindings env module_name alias with
                  | Error _ as err -> err
                  | Ok env ->
                      let env =
                        Env.add_namespace_alias ~scope ~alias
                          ~target:module_name env
                      in
                      apply_specs env rest)
            | Require.Refer { module_name; names } :: rest -> (
                let result =
                  if String.starts_with ~prefix:"ocaml." module_name then
                    Require.add_ocaml_refer_bindings env scope module_name names
                  else Require.add_lg_refer_bindings env scope module_name names
                in
                match result with
                | Error _ as err -> err
                | Ok env -> apply_specs env rest)
          in
          match apply_specs env specs with
          | Error _ as err -> err
          | Ok env -> Ok (scope, env, next_type, Comment "require")))
  | FList (FSymbol "loop" :: _) as form -> (
      match compile_expr scope env form with
      | Error _ as err -> err
      | Ok expr ->
          Ok
            ( scope,
              env,
              next_type,
              Value_binding
                { pattern = Ignore_pattern; expression = expr.semantic_expr } ))
  | FList (FSymbol "recur" :: _) ->
      Error.error "recur is only valid in a loop tail position"
  | FList (FSymbol ("defn" | "defn-") :: _) ->
      Error.error "defn expects a name, parameter vector, and body"
  | FList (FSymbol "defonce" :: _) ->
      Error.error "defonce expects a name and value"
  | FList (FSymbol name :: args) as form -> (
      match Env.find_macro ~scope name env with
      | None -> (
          match compile_expr scope env form with
          | Error _ as error -> error
          | Ok expr -> (
              match expr.record_values with
              | Some _ ->
                  Error.error "top-level map literals must be bound with def"
              | None ->
                  Ok
                    ( scope,
                      env,
                      next_type,
                      Value_binding
                        {
                          pattern = Ignore_pattern;
                          expression = expr.semantic_expr;
                        } )))
      | Some definition -> (
          match Macro_expander.expand ~scope ~compiler_env:env definition args with
          | Error _ as error -> error
          | Ok expanded -> compile scope env next_type expanded))
  | form -> (
      match compile_expr scope env form with
      | Error _ as err -> err
      | Ok expr -> (
          match expr.record_values with
          | Some _ ->
              Error.error "top-level map literals must be bound with def"
          | None ->
              Ok
                ( scope,
                  env,
                  next_type,
                  Value_binding
                    {
                      pattern = Ignore_pattern;
                      expression = expr.semantic_expr;
                    } )))
