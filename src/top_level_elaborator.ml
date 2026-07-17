open Ast
open Types
open Lowered
module Env = Compiler_environment

let compile_expr = Expression_elaborator.compile_expr
let prepare_fn = Expression_elaborator.prepare_fn
let prepare_recursive_fn = Expression_elaborator.prepare_recursive_fn

let prepare_inferred_recursive_fn =
  Expression_elaborator.prepare_inferred_recursive_fn

let fn_code = Expression_elaborator.fn_code
let compile_fn = Expression_elaborator.compile_fn
let compile_args_for = Expression_elaborator.compile_args_for
let compile_call = Expression_elaborator.compile_call
let binding_of_expr = Expression_support.binding_of_expr
let allocate_anonymous_record = Expression_support.allocate_anonymous_record

let allocate_nested_anonymous_records =
  Expression_support.allocate_nested_anonymous_records

let row_param_type_names = Expression_support.row_param_type_names
let row_type_items = Expression_support.row_type_items
let check_emitted_name_collision = Resolver.check_emitted_name_collision
let unresolved_contextual_type = Expression_support.unresolved_contextual_type
let lookup_record_type = Resolver.lookup_record_type
let record_type_key = Resolver.record_type_key

let inherit_scope_ocaml_value_refers =
  Expression_support.inherit_scope_ocaml_value_refers

let rec form_mentions_symbol name = function
  | FSymbol candidate -> candidate = name
  | FList (FSymbol ("quote" | "clojure.core/quote") :: _) -> false
  | FList forms | FVector forms -> List.exists (form_mentions_symbol name) forms
  | FMap pairs ->
      List.exists
        (fun (key, value) ->
          form_mentions_symbol name key || form_mentions_symbol name value)
        pairs
  | FInt _ | FFloat _ | FChar _ | FString _ | FRegex _ | FBool _ | FKeyword _
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
  | FSymbol _ | FInt _ | FFloat _ | FChar _ | FString _ | FRegex _ | FBool _
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
  let declared_names =
    Env.filter_map
      (fun _ (binding : binding) ->
        match binding.ty with
        | TOcaml "__declared_fn" -> Some [ binding.ocaml_name ]
        | _ when binding.forward_declared ->
            Some (binding.ocaml_name :: binding.overload_targets)
        | _ -> None)
      env
    |> List.concat
  in
  Semantic_ir.exists_identifier
    (fun name -> List.mem name declared_names)
    expression

let compile_defprotocol = Protocol_elaborator.compile_defprotocol
let compile_extend_type = Protocol_elaborator.compile_extend_type

let deferred_value_type env (expr : Types.typed_expr) =
  Types.align_deferred_param_types
    (Protocol.refine_deferred_type env expr.ty)
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

let compile_type_record_fields =
  Type_definition_elaborator.compile_type_record_fields

let compile_type_variant = Type_definition_elaborator.compile_type_variant

let rec concrete_defrecord_field_type = function
  | TUnknown | TVar _ | TRecord _ -> None
  | ty when Types.is_dynamic ty -> None
  | ty when Option.is_some (Types.protocol_constraint_info ty) -> None
  | ty when Option.is_some (Types.seqable_constraint_info ty) -> None
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
  | TBool | TUnit | TNil | TUnknown | TOcaml _ ->
      []

let infer_defrecord_field_types scope env field_names interface_forms =
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
  let resolve_protocol_record ty =
    match protocol_ids ty with
    | [] -> ty
    | protocols ->
        let candidates =
          Env.filter_map
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
  let infer_method field_types = function
    | FList
        (_method_name
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
        in
        let body_forms = List.map (rewrite_field_access receiver) body_forms in
        let lookup_dynamic_key_record_type =
          Expression_support.dynamic_key_record_type env
        in
        let resolve_named_record =
          Function_elaborator.infer_named_record scope env
        in
        match
          Type_inference.infer_params ~lookup_function_ty
            ~lookup_protocol_constraint ~lookup_dynamic_key_record_type
            ~resolve_named_record params body_forms
        with
        | Error _ -> field_types
        | Ok inferred_params ->
            List.map2
              (fun name previous ->
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
                match
                  ( concrete_defrecord_field_type previous,
                    concrete_defrecord_field_type inferred )
                with
                | _, Some (TNamed_record _ as inferred) -> inferred
                | None, Some inferred -> inferred
                | Some previous, _ -> previous
                | None, None -> previous)
              field_names field_types)
    | _ -> field_types
  in
  let inferred =
    interface_forms
    |> List.fold_left infer_method (List.map (fun _ -> TUnknown) field_names)
    |> List.map (fun ty ->
           concrete_defrecord_field_type ty
           |> Option.value ~default:(Types.dynamic_constraint TUnknown))
  in
  inferred

let rec compile scope env next_type = function
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
      let resolve_field_hint hint =
        Result.bind (Type_annotation.of_param_annotation hint) (function
          | TOcaml type_name
            when String.starts_with ~prefix:"__lg_record:" type_name ->
              let source_name =
                String.sub type_name (String.length "__lg_record:")
                  (String.length type_name - String.length "__lg_record:")
              in
              Resolver.lookup_record_type scope env source_name
              |> Result.map (fun record -> TNamed_record record)
          | ty -> Ok ty)
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
          let field_types =
            infer_defrecord_field_types scope env fields interface_forms
            |> List.map2 (fun (_field_name, explicit_ty) inferred_ty ->
                   Option.value explicit_ty ~default:inferred_ty)
                 field_specs
          in
          let type_parameters =
            field_types
            |> List.concat_map type_parameters_of_type
            |> List.sort_uniq String.compare
          in
          let record_fields =
            List.map2
              (fun field_name ty -> Types.make_field (":" ^ field_name) ty)
              fields field_types
            @ [ Types.make_record_extension_field () ]
          in
          match
            compile_type_record_fields
              ?location:(Source_context.find name_form)
              ~allow_empty:true scope env next_type name type_parameters
              record_fields
          with
          | Error _ as error -> error
          | Ok (scope, env, next_type, type_item) -> (
              match protocol_groups [] None interface_forms with
              | Error _ as error -> error
              | Ok groups ->
                  let groups = order_protocol_groups groups in
                  let rec compile_groups env next_type items = function
                    | [] -> Ok (scope, env, next_type, Group items)
                    | (protocol_name, methods) :: rest -> (
                        let wrap_method method_name receiver_name params
                            body_forms =
                          let field_bindings =
                            fields
                            |> List.concat_map (fun field_name ->
                                   [
                                     FSymbol field_name;
                                     FList
                                       [
                                         FSymbol (".-" ^ field_name);
                                         FSymbol receiver_name;
                                       ];
                                   ])
                          in
                          FList
                            [
                              FSymbol method_name;
                              params;
                              FList
                                (FSymbol "let" :: FVector field_bindings
                               :: body_forms);
                            ]
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
                        let methods = List.concat_map expand_method methods
                        in
                        let implementation_form =
                        match
                            Protocol.find_protocol_id scope env protocol_name
                          with
                          | Some _ ->
                              FList
                               (FSymbol "extend-type" :: FSymbol name
                               :: FSymbol protocol_name :: methods)
                          | None ->
                              FList
                                (FSymbol "deftype-methods" :: FSymbol name
                               :: FSymbol protocol_name :: methods)
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
                  compile_groups env next_type (items_of type_item) groups))
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
          if fields = [] then Error.error "deftype expects at least one field"
          else
            let definitions =
              fields
              |> List.mapi (fun index (field_name, metadata, mutable_field) ->
                     let parameter = "field" ^ string_of_int index in
                     let field_type, parameters =
                       match metadata with
                    | Some ("^int" | "^long" | "^number") -> ("int", [])
                       | Some ("^boolean" | "^Boolean") -> ("bool", [])
                       | Some ("^double" | "^float") -> ("float", [])
                       | Some "^String" -> ("string", [])
                       | _ -> ("dynamic", [])
                     in
                     let field_type =
                       if mutable_field then "ref<" ^ field_type ^ ">"
                       else field_type
                     in
                     match metadata with
                     | Some ("^int" | "^long" | "^number") ->
                      ( parameters,
                        FList
                          [ FSymbol field_name; FKeyword (":" ^ field_type) ] )
                     | Some ("^boolean" | "^Boolean") ->
                      ( parameters,
                        FList
                          [ FSymbol field_name; FKeyword (":" ^ field_type) ] )
                     | Some ("^double" | "^float") ->
                      ( parameters,
                        FList
                          [ FSymbol field_name; FKeyword (":" ^ field_type) ] )
                     | Some "^String" ->
                      ( parameters,
                        FList
                          [ FSymbol field_name; FKeyword (":" ^ field_type) ] )
                     | Some "^clojure.lang.Associative" ->
                         let key_parameter = parameter ^ "_key" in
                         let value_parameter = parameter ^ "_value" in
                         ( [ key_parameter; value_parameter ],
                           FList
                          [
                            FSymbol field_name;
                               FKeyword
                              (":Lg_runtime.Runtime_map.t<" ^ key_parameter
                             ^ ";" ^ value_parameter ^ ">");
                             ] )
                     | _ ->
                         ( parameters,
                           FList
                          [ FSymbol field_name; FKeyword (":" ^ field_type) ] ))
            in
            let type_parameters = List.concat_map fst definitions in
            let field_forms = List.map snd definitions in
            compile_type_record
              ?location:(Source_context.find name_form)
              scope env next_type name type_parameters field_forms)
  | FList (FSymbol "deftype-methods" :: FSymbol type_name :: interface_forms)
    -> (
      match Resolver.lookup_record_type scope env type_name with
      | Error _ as err -> err
      | Ok record ->
          let receiver_ty = TNamed_record record in
          let rec compile_methods env items current_interface = function
            | [] -> Ok (scope, env, next_type, Group (List.rev items))
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
                if current_interface = Some "IPrintWithWriter" then
                  compile_methods env items current_interface rest
                else
                let arity = List.length params in
                let source_name =
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
                let rec form_mentions name = function
                  | FSymbol candidate -> candidate = name
                  | FList forms | FVector forms ->
                      List.exists (form_mentions name) forms
                  | FMap pairs ->
                      List.exists
                        (fun (key, value) ->
                          form_mentions name key || form_mentions name value)
                        pairs
                  | FInt _ | FFloat _ | FChar _ | FString _ | FRegex _
                  | FBool _ | FKeyword _ | FCoreSymbol _ ->
                      false
                in
                let rec rewrite_mutable_assignments = function
                    | FList [ FSymbol "set!"; FSymbol field_name; value_form ]
                      -> (
                      let keyword = ":" ^ field_name in
                      match Types.find_field keyword record.fields with
                      | Some { ty = TRef _; _ } ->
                          FList
                              [
                                FSymbol "reset!";
                              FList
                                  [
                                    FSymbol "__deftype-field-ref";
                                  FKeyword keyword;
                                  FSymbol receiver_name;
                                ];
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
                let body_forms =
                  List.map rewrite_mutable_assignments body_forms
                in
                let field_bindings =
                  record.fields
                  |> List.filter (fun (field : field) ->
                         let source_name =
                           Names.keyword_source_name field.keyword
                         in
                         List.exists (form_mentions source_name) body_forms)
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
                  match
                   Expression_elaborator.compile_fn
                     ~param_type_overrides:[ Some receiver_ty ] scope env
                     params_form body_forms
                 with
                | Error _ as err -> err
                  | Ok implementation -> (
                    let binding = binding_of_expr ocaml_name implementation in
                    let register_protocol env =
                      match current_interface with
                      | Some protocol_name
                        when Option.is_some
                                 (Protocol.find_protocol_id scope env
                                    protocol_name)
                             && Option.is_some
                                  (Protocol.lookup_protocol_marker scope env
                                       protocol_name method_name) -> (
                          match
                            Protocol_elaborator.marker scope env protocol_name
                              method_name
                          with
                          | Error _ as error -> error
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
                            Deferred_value_binding
                              {
                                name = ocaml_name;
                                value_type =
                                  Protocol.refine_deferred_type env
                                    implementation.ty;
                                return_param_index =
                                  implementation.return_param_index;
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
                          rest))
            | _ :: _ ->
                Error.error
                  "deftype methods must be (method-name [params] body...)"
          in
          compile_methods env [] None interface_forms)
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
  | FList (FSymbol "defmethod" :: _) ->
      Error.error "defmethod currently supports print-method"
  | FList (FSymbol "recursive-definition-group" :: definitions) ->
      let env =
        definitions
        |> List.fold_left
             (fun env -> function
               | FList (FSymbol ("defn" | "defn-") :: FSymbol name :: _) ->
                   let key = Names.scoped_key scope name in
                   if Option.is_some (Env.find_opt key env) then env
                   else
                     Env.add key
                       (Types.binding
                          (Names.ocaml_binding_name scope name)
                          (TOcaml "__declared_fn"))
                       env
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
                ({ name; identity = None; expression } :: bindings)
                rest
          | Deferred_value_binding { name; expression; _ } :: rest ->
              collect
                ({ name; identity = None; expression } :: bindings)
                rest
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
                name (first_clause :: remaining_clauses)
            with
            | Error _ as error -> error
            | Ok prepared ->
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
                    expression = prepared.expr.semantic_expr;
                  }
                in
                let new_bindings = arity_bindings @ [ dispatch_binding ] in
                compile_definitions env next_type
                  (List.rev_append rows row_items)
                  (List.rev_append new_bindings bindings)
                  rest)
        | FList
            (FSymbol ("defn" | "defn-")
            :: (FSymbol name as name_form)
            :: params :: body_forms)
          :: rest -> (
            let ocaml_name = Names.ocaml_binding_name scope name in
            let recursive = function_is_recursive scope name body_forms in
            let prepared =
              match (recursive, params) with
              | true, FVector _ ->
                  prepare_inferred_recursive_fn ~ocaml_name scope env name
                    params body_forms
              | _ -> prepare_fn scope env params body_forms
            in
            match prepared with
            | Error _ as err -> err
            | Ok parts ->
                let param_tys =
                  parts.param_bindings
                  |> List.map (fun (_key, (binding : binding)) -> binding.ty)
                in
                let row_param_types =
                  row_param_type_names ocaml_name param_tys
                in
                let expr =
                  fn_code ~row_param_type_names:row_param_types parts
                in
                let binding =
                  binding_of_expr ~row_param_types ocaml_name expr
                in
                let env = Env.add (Names.scoped_key scope name) binding env in
                let rows = row_type_items row_param_types param_tys in
                let recursive_binding =
                  {
                    name = ocaml_name;
                    identity =
                      Source_context.find name_form
                      |> Option.map (fun location ->
                             (Source_node_id.of_location location, location));
                    expression = expr.semantic_expr;
                  }
                in
                compile_definitions env next_type
                  (List.rev_append rows row_items)
                  (recursive_binding :: bindings)
                  rest)
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
        | _ -> prepare_fn scope env params body_forms
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
          let row_param_types = row_param_type_names ocaml_name param_tys in
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
      [ FSymbol ("def" | "defonce"); (FSymbol name as name_form); expr_form ]
    -> (
      match compile_expr scope env expr_form with
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
          | TRecord fields ->
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
                          type_name = allocation.record.type_name;
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
                          type_name = allocation.record.type_name;
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
              let binding = binding_of_expr ocaml_name expr in
              Ok
                ( scope,
                  Env.add env_key binding env,
                  next_type,
                  Value_binding
                    {
                          pattern =
                            located_value_pattern name_form (Named ocaml_name);
                      expression = expr.semantic_expr;
                    } ))))
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
      let runtime_env = Env.with_inline_macros [] env in
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
      :: (FSymbol name as name_form)
      :: FSymbol annotation
      :: params :: body_forms)
    when String.starts_with ~prefix:"^" annotation -> (
      if not (function_is_recursive scope name body_forms) then
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
      | Ok return_ty ->
          let return_keyword =
            match return_ty with
            | TInt -> Ok ":int"
            | TFloat -> Ok ":float"
            | TChar -> Ok ":char"
            | TString -> Ok ":string"
            | TSymbol -> Ok ":symbol"
            | TKeyword -> Ok ":keyword"
            | TBool -> Ok ":bool"
            | TUnit -> Ok ":unit"
            | ty when Types.is_dynamic ty -> Ok ":dynamic"
            | TOcaml name -> Ok (":" ^ name)
            | _ -> Error.error "unsupported defn return type hint"
          in
          Result.bind return_keyword (fun return_keyword ->
              compile scope env next_type
                (FList
                   (FSymbol definition :: name_form :: params
                  :: FKeyword return_keyword :: body_forms))))
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
      match
        check_emitted_name_collision env ~source_key:env_key ~ocaml_name
      with
      | Error _ as err -> err
      | Ok () -> (
          match
            Expression_elaborator.prepare_multi_arity_fn ~ocaml_name scope env
              name
              (first_clause :: remaining_clauses)
          with
          | Error _ as err -> err
          | Ok prepared ->
              let targets, overload_row_param_types, row_items,
                  recursive_bindings =
                Expression_elaborator.lower_prepared_multi_arity prepared
              in
              let binding =
                Types.binding ~overload_targets:targets
                  ~overload_row_param_types ocaml_name
                  prepared.expr.ty
              in
              let binding, value_item =
                if
                  expression_references_declaration env
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
              Ok
                ( scope,
                  Env.add env_key binding env,
                  next_type,
                  Group
                    (row_items
                    @ [
                        Recursive_value_bindings recursive_bindings; value_item;
                      ]) )))
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
      :: FKeyword return_keyword
      :: body_forms) -> (
      match Type_annotation.of_keyword return_keyword with
      | Error _ as err -> err
      | Ok return_ty -> (
          let ocaml_name = Names.ocaml_binding_name scope name in
          match
             prepare_recursive_fn ~ocaml_name scope env name return_ty params
               body_forms
           with
          | Error _ as err -> err
          | Ok parts -> (
              let param_tys =
                parts.param_bindings
                |> List.map (fun (_key, (binding : binding)) -> binding.ty)
              in
              let row_param_types = row_param_type_names ocaml_name param_tys in
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
                    if expression_references_declaration env expr.semantic_expr
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
      match
        prepare_inferred_recursive_fn ~ocaml_name scope env name params
          body_forms
      with
      | Error _ as err -> err
      | Ok parts -> (
          let param_tys =
            parts.param_bindings
            |> List.map (fun (_key, (binding : binding)) -> binding.ty)
          in
          let row_param_types = row_param_type_names ocaml_name param_tys in
          let expr = fn_code ~row_param_type_names:row_param_types parts in
          let env_key = Names.scoped_key scope name in
          match
            check_emitted_name_collision env ~source_key:env_key ~ocaml_name
          with
          | Error _ as err -> err
          | Ok () ->
              let binding = binding_of_expr ~row_param_types ocaml_name expr in
              let type_items = row_type_items row_param_types param_tys in
              let binding, value_item =
                if expression_references_declaration env expr.semantic_expr then
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
      match prepare_fn scope env params body_forms with
      | Error _ as err -> err
      | Ok parts when unresolved_contextual_type parts.body.ty ->
          Error.error "empty list requires a contextual element type"
      | Ok parts -> (
          let ocaml_name = Names.ocaml_binding_name scope name in
          let param_tys =
            parts.param_bindings
            |> List.map (fun (_key, (binding : binding)) -> binding.ty)
          in
          let row_param_types = row_param_type_names ocaml_name param_tys in
          let expr = fn_code ~row_param_type_names:row_param_types parts in
          let env_key = Names.scoped_key scope name in
          match
            check_emitted_name_collision env ~source_key:env_key ~ocaml_name
          with
          | Error _ as err -> err
          | Ok () -> (
              match expr.ty with
              | TFn _ ->
                  let binding =
                    binding_of_expr ~row_param_types ocaml_name expr
                  in
                  let type_items = row_type_items row_param_types param_tys in
                  let binding, value_item =
                    if expression_references_declaration env expr.semantic_expr
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
                        Value_binding
                          {
                            pattern =
                              located_value_pattern name_form (Named ocaml_name);
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
  | FList (FSymbol "extend-type" :: receiver_form :: implementations) ->
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
          let rec compile_groups env next_type items = function
            | [] -> Ok (scope, env, next_type, Group items)
            | (protocol_name, methods) :: rest -> (
                match
                  compile_extend_type scope env next_type receiver_form
                    protocol_name methods
                with
                | Error _ as error -> error
                | Ok (_, env, next_type, item) ->
                    compile_groups env next_type (items @ items_of item) rest)
          in
          compile_groups env next_type [] groups)
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
  | FList (FSymbol (("print" | "println") as name) :: args) -> (
      match compile_call scope env name args with
      | Error _ as err -> err
      | Ok expr ->
          Ok
            ( scope,
              env,
              next_type,
              Value_binding
                { pattern = Unit_pattern; expression = expr.semantic_expr } ))
  | FList [ FSymbol "namespace-scope"; FSymbol namespace_name ] ->
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
                when not (Types.equal binding.ty (TOcaml "__declared_fn")) ->
                  binding
              | _ -> Types.binding ocaml_name (TOcaml "__declared_fn")
            in
            add_declarations (Env.add key binding env) rest
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
                  apply_specs
                    (Require.add_ocaml_alias_bindings env module_name alias)
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
                  else if module_name = "clojure.string" then
                    Require.add_clojure_string_refer_bindings env scope names
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
          match Macro_expander.expand ~compiler_env:env definition args with
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
