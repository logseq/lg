open Ast
open Types

let has_source_name name expected =
  String.equal name expected
  || String.ends_with ~suffix:("/" ^ expected) name
open Expression_support
module Env = Compiler_environment

type expression_result = (typed_expr, Error.t) result
type type_result = (ty, Error.t) result

let loop_counter = ref 0
let destructuring_value_counter = ref 0

type t = {
  compile_vector : string -> Env.t -> Ast.form list -> expression_result;
  compile_map :
    string -> Env.t -> (Ast.form * Ast.form) list -> expression_result;
  compile_if :
    string -> Env.t -> Ast.form -> Ast.form -> Ast.form -> expression_result;
  compile_if_let :
    string -> Env.t -> Ast.form -> Ast.form -> Ast.form -> expression_result;
  compile_if_some :
    string -> Env.t -> Ast.form -> Ast.form -> Ast.form -> expression_result;
  compile_when_let :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_when_some :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_let_some :
    string -> Env.t -> Ast.form -> Ast.form -> Ast.form -> expression_result;
  compile_logical :
    string -> Env.t -> [ `And | `Or ] -> Ast.form list -> expression_result;
  compile_match :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_body :
    string -> Env.t -> string -> Ast.form list -> expression_result;
  compile_try : string -> Env.t -> Ast.form list -> expression_result;
  loop_branch_type : ty -> ty -> type_result;
  compile_recur :
    string -> Env.t -> string -> ty list -> Ast.form list -> expression_result;
  compile_loop_tail :
    string -> Env.t -> string -> ty list -> Ast.form -> expression_result;
  compile_loop_tail_body :
    string -> Env.t -> string -> ty list -> Ast.form list -> expression_result;
  compile_loop :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
  compile_let :
    string -> Env.t -> Ast.form -> Ast.form list -> expression_result;
}

let compile_args_for compile_expr scope env arg_forms =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | form :: rest -> (
        match compile_expr scope env form with
        | Ok expr -> loop (expr :: acc) rest
        | Error _ as err -> err)
  in
  loop [] arg_forms

let located_pattern identity pattern =
  match identity with
  | None -> pattern
  | Some (node_id, location) -> Semantic_ir.PLocated (node_id, location, pattern)

let located_form_pattern form pattern =
  located_pattern (Destructure.source_identity form) pattern

let rec capability_pattern name ty =
  let layer witness_name value_ty =
    Semantic_ir.PTuple
      [
        Semantic_ir.PVar witness_name;
        capability_pattern name value_ty;
      ]
  in
  match Types.protocol_constraint_info ty with
  | Some (protocol_id, _, value_ty) ->
      layer (Types.protocol_witness_name name protocol_id) value_ty
  | None -> (
      match Types.truthy_constraint_info ty with
      | Some value_ty -> layer (name ^ "__truthy") value_ty
      | None -> (
          match Types.nil_predicate_constraint_info ty with
          | Some value_ty -> layer (name ^ "__nil") value_ty
          | None -> (
              match Types.printable_constraint_info ty with
              | Some value_ty ->
                  Semantic_ir.PTuple
                    [
                      Semantic_ir.PTuple
                        [ Semantic_ir.PVar (name ^ "__print");
                          Semantic_ir.PVar (name ^ "__pr");
                        ];
                      capability_pattern name value_ty;
                    ]
              | None -> (
                  match Types.exception_data_constraint_info ty with
                  | Some value_ty -> layer (name ^ "__ex_data") value_ty
                  | None -> (
                  match Types.hashable_constraint_info ty with
                  | Some value_ty -> layer (name ^ "__hash") value_ty
                  | None -> (
                      match Types.comparable_constraint_info ty with
                      | Some value_ty -> layer (name ^ "__compare") value_ty
                      | None -> (
                          match Types.array_index_constraint_info ty with
                          | Some value_ty -> layer (name ^ "__index") value_ty
                          | None -> (
                  match Types.symbol_predicate_constraint_info ty with
                  | Some value_ty -> layer (name ^ "__symbol") value_ty
                  | None -> (
                      match Types.contains_constraint_info ty with
                      | Some (_, value_ty) -> layer (name ^ "__contains") value_ty
                      | None -> (
                          match ty with
                          | TConstraint
                              (Seqable_constraint
                                { requirement; storage = value_ty; _ }) ->
                              let witness_name =
                                if requirement = Required
                                then name ^ "__seq"
                                else name ^ "__seq_optional"
                              in
                              layer witness_name value_ty
                          | _ -> Semantic_ir.PVar name))))))))))

let has_capability ty =
  Option.is_some (Types.protocol_constraint_info ty)
  || Option.is_some (Types.seqable_constraint_element ty)
  || Option.is_some (Types.truthy_constraint_info ty)
  || Option.is_some (Types.nil_predicate_constraint_info ty)
  || Option.is_some (Types.printable_constraint_info ty)
  || Option.is_some (Types.exception_data_constraint_info ty)
  || Option.is_some (Types.hashable_constraint_info ty)
  || Option.is_some (Types.comparable_constraint_info ty)
  || Option.is_some (Types.array_index_constraint_info ty)
  || Option.is_some (Types.symbol_predicate_constraint_info ty)
  || Option.is_some (Types.contains_constraint_info ty)

let rec has_protocol_constraint protocol_id ty =
  match Types.protocol_constraint_info ty with
  | Some (candidate, _, value_ty) ->
      (Protocol_id.equal candidate protocol_id
      && not (Types.is_guarded_protocol_constraint ty))
      || has_protocol_constraint protocol_id value_ty
  | None -> false

let narrow_type_predicates scope env condition body =
  let condition =
    Macro_expander.expand_all ~scope ~compiler_env:env condition
    |> Result.value ~default:condition
  in
  let rec narrowed_symbols = function
    | FSymbol name -> [ name ]
    | FList (FSymbol name :: forms)
      when name = "__lg_logical-and"
           || String.ends_with ~suffix:"/__lg_logical-and" name ->
        List.concat_map narrowed_symbols forms
    | _ -> []
  in
  let is_core_symbol expected actual =
    actual = expected || String.ends_with ~suffix:("/" ^ expected) actual
  in
  let rec symbols_matching predicate_name = function
    | FList [ FSymbol predicate; FSymbol name ]
      when is_core_symbol predicate_name predicate ->
        [ name ]
    | FList (FSymbol name :: forms)
      when name = "__lg_logical-and"
           || String.ends_with ~suffix:"/__lg_logical-and" name ->
        List.concat_map (symbols_matching predicate_name) forms
    | _ -> []
  in
  let rec instance_symbols = function
    | FList
        [
          FSymbol predicate;
          FSymbol record_name;
          FSymbol value_name;
        ]
      when is_core_symbol "instance?" predicate ->
        [ (record_name, value_name) ]
    | FList (FSymbol name :: forms)
      when name = "__lg_logical-and"
           || String.ends_with ~suffix:("/" ^ "__lg_logical-and") name ->
        List.concat_map instance_symbols forms
    | _ -> []
  in
  let negated_instance_symbols = function
    | FList [ FSymbol negation; predicate ]
      when is_core_symbol "not" negation
           || is_core_symbol "__lg_not" negation ->
        instance_symbols predicate
    | _ -> []
  in
  let rec symbols_known_non_nil = function
    | FList
        [ FSymbol negation; FList [ FSymbol nil_predicate; FSymbol name ] ]
      when is_core_symbol "not" negation
           && is_core_symbol "__lg_nil-predicate" nil_predicate ->
        [ name ]
    | FList (FSymbol name :: forms)
      when name = "__lg_logical-and"
           || String.ends_with ~suffix:"/__lg_logical-and" name ->
        List.concat_map symbols_known_non_nil forms
    | _ -> []
  in
  let rec guarded_protocol_symbols = function
    | FList
        [ FSymbol predicate; FSymbol protocol_name; FSymbol receiver ]
      when is_core_symbol "satisfies?" predicate ->
        [ (protocol_name, receiver) ]
    | FList (FSymbol name :: forms)
      when name = "__lg_logical-and"
           || String.ends_with ~suffix:"/__lg_logical-and" name ->
        List.concat_map guarded_protocol_symbols forms
    | _ -> []
  in
  let rec successful_call_narrowings = function
    | FList (FSymbol function_name :: arguments) -> (
        match Resolver.lookup_binding scope env function_name with
        | Ok (binding : Types.binding) -> (
            match
              Env.find_successful_call_refinement binding.ocaml_name env
            with
            | Some (parameter_index, refined_ty) -> (
                match List.nth_opt arguments parameter_index with
                | Some (FSymbol value_name) -> [ (value_name, refined_ty) ]
                | Some _ | None -> [])
            | None ->
                if
                  function_name = "__lg_logical-and"
                  || String.ends_with ~suffix:"/__lg_logical-and"
                       function_name
                then List.concat_map successful_call_narrowings arguments
                else [])
        | Error _ ->
            if
              function_name = "__lg_logical-and"
              || String.ends_with ~suffix:"/__lg_logical-and" function_name
            then List.concat_map successful_call_narrowings arguments
            else [])
    | _ -> []
  in
  let nullable_names =
    (narrowed_symbols condition @ symbols_known_non_nil condition
    @ symbols_matching "__lg_number-predicate" condition)
    |> List.sort_uniq String.compare
    |> List.filter (fun name ->
           match Resolver.lookup_binding scope env name with
           | Ok (binding : Types.binding) -> (
               match Types.constraint_value_type binding.ty with
               | TNullable _ | TOcaml_app ("option", [ _ ]) -> true
               | _ -> false)
           | Error _ -> false)
  in
  let body =
    List.fold_right
      (fun name body ->
        FList
          [
            FSymbol "let";
            FVector
              [
                FSymbol name;
                FList [ FSymbol "__lg_nullable-value"; FSymbol name ];
              ];
            body;
          ])
      nullable_names body
  in
  let body =
    let guarded =
      guarded_protocol_symbols condition |> List.sort_uniq compare
      |> List.filter (fun (protocol_name, receiver) ->
             match
               ( Protocol.find_protocol_id scope env protocol_name,
                 Resolver.lookup_binding scope env receiver )
             with
             | Some protocol_id, Ok (binding : Types.binding) ->
                 not
                   (has_protocol_constraint protocol_id binding.ty
                   || Protocol.type_satisfies env protocol_id binding.ty)
             | _ -> true)
    in
    List.fold_right
      (fun (protocol_name, receiver) body ->
        FList
          [
            FSymbol "let";
            FVector
              [
                FSymbol receiver;
                FList
                  [
                    FSymbol "__lg_protocol-value";
                    FSymbol protocol_name;
                    FSymbol receiver;
                  ];
              ];
            body;
          ])
      guarded body
  in
  let narrow predicate helper body =
    let names =
      symbols_matching predicate condition |> List.sort_uniq String.compare
      |> List.filter (fun name ->
             if not (String.equal helper "__lg_number-value") then true
             else
               match Resolver.lookup_binding scope env name with
               | Ok (binding : Types.binding) -> (
                   match Types.constraint_value_type binding.ty with
                   | TNullable _ | TOcaml_app ("option", [ _ ]) -> false
                   | _ -> true)
               | Error _ -> true)
    in
    List.fold_right
      (fun name body ->
        FList
          [
            FSymbol "let";
            FVector
              [
                FSymbol name;
                FList [ FSymbol helper; FSymbol name ];
              ];
            body;
          ])
      names body
  in
  let body =
    List.fold_right
      (fun (record_name, value_name) body ->
        FList
          [
            FSymbol "let";
            FVector
              [
                FSymbol value_name;
                FList
                  [
                    FSymbol "__lg_instance-value";
                    FSymbol record_name;
                    FSymbol value_name;
                  ];
              ];
            body;
          ])
      (instance_symbols condition |> List.sort_uniq compare)
      body
  in
  let body =
    List.fold_right
      (fun (record_name, value_name) body ->
        FList
          [
            FSymbol "let";
            FVector
              [
                FSymbol value_name;
                FList
                  [
                    FSymbol "__lg_not-instance-value";
                    FSymbol record_name;
                    FSymbol value_name;
                  ];
              ];
            body;
          ])
      (negated_instance_symbols condition |> List.sort_uniq compare)
      body
  in
  let body =
    List.fold_right
         (fun (name, refined_ty) body ->
           let helper =
             match Types.constraint_value_type refined_ty with
             | TSymbol -> Some "__lg_symbol-value"
             | TKeyword -> Some "__lg_keyword-value"
             | TInt -> Some "__lg_int-value"
             | _ -> None
           in
           match helper with
           | None -> body
           | Some helper ->
               FList
                 [
                   FSymbol "let";
                   FVector
                     [
                       FSymbol name;
                       FList [ FSymbol helper; FSymbol name ];
                     ];
                   body;
                 ])
      (successful_call_narrowings condition |> List.sort_uniq compare)
      body
  in
  body
  |> narrow "__lg_symbol-predicate" "__lg_symbol-value"
  |> narrow "__lg_keyword-predicate" "__lg_keyword-value"
  |> narrow "__lg_string-predicate" "__lg_string-value"
  |> narrow "__lg_int-predicate" "__lg_int-value"
  |> narrow "__lg_number-predicate" "__lg_number-value"
  |> narrow "__lg_fn-predicate" "__lg_fn-value"

let narrow_false_scalar_predicates scope env condition body =
  let condition =
    Macro_expander.expand_all ~scope ~compiler_env:env condition
    |> Result.value ~default:condition
  in
  let is_core_symbol expected actual =
    actual = expected || String.ends_with ~suffix:("/" ^ expected) actual
  in
  let symbols_matching predicate_name = function
    | FList [ FSymbol predicate; FSymbol value_name ]
      when is_core_symbol predicate_name predicate ->
        [ value_name ]
    | _ -> []
  in
  let narrow predicate helper body =
    let names =
      symbols_matching predicate condition |> List.sort_uniq String.compare
    in
    List.fold_right
      (fun value_name body ->
        FList
          [
            FSymbol "let";
            FVector
              [
                FSymbol value_name;
                FList [ FSymbol helper; FSymbol value_name ];
              ];
            body;
          ])
      names body
  in
  body
  |> narrow "__lg_keyword-predicate" "__lg_not-keyword-value"
  |> narrow "__lg_string-predicate" "__lg_not-string-value"
  |> narrow "__lg_symbol-predicate" "__lg_not-symbol-value"
  |> narrow "__lg_int-predicate" "__lg_not-int-value"

let narrow_false_fn_predicates scope env condition body =
  let expanded_condition =
    Macro_expander.expand_all ~scope ~compiler_env:env condition
    |> Result.value ~default:condition
  in
  let is_core_symbol expected actual =
    actual = expected || String.ends_with ~suffix:("/" ^ expected) actual
  in
  let direct_fn_symbol = function
    | FList [ FSymbol predicate; FSymbol value_name ]
      when is_core_symbol "__lg_fn-predicate" predicate ->
        Some value_name
    | _ -> None
  in
  let returns_symbol value_name = function
    | FSymbol returned_name -> String.equal value_name returned_name
    | FList [ FSymbol do_name; FSymbol returned_name ] ->
        is_core_symbol "do" do_name && String.equal value_name returned_name
    | _ -> false
  in
  let fn_symbols = function
    | condition -> (
        match direct_fn_symbol condition with
        | Some value_name -> [ value_name ]
        | None -> (
            match condition with
            | FList
                [
                  FSymbol if_name;
                  predicate;
                  then_form;
                  FSymbol nil_name;
                ]
              when is_core_symbol "if" if_name
                   && is_core_symbol "nil" nil_name -> (
                match direct_fn_symbol predicate with
                | Some value_name when returns_symbol value_name then_form ->
                    [ value_name ]
                | Some _ | None -> [])
            | _ -> []))
  in
  List.fold_right
    (fun value_name body ->
      FList
        [
          FSymbol "let";
          FVector
            [
              FSymbol value_name;
              FList [ FSymbol "__lg_not-fn-value"; FSymbol value_name ];
            ];
          body;
        ])
    (fn_symbols condition @ fn_symbols expanded_condition
    |> List.sort_uniq String.compare)
    body

let narrow_false_instance_predicates scope env condition body =
  let condition =
    Macro_expander.expand_all ~scope ~compiler_env:env condition
    |> Result.value ~default:condition
  in
  let is_core_symbol expected actual =
    actual = expected || String.ends_with ~suffix:("/" ^ expected) actual
  in
  let instance_symbols = function
    | FList
        [
          FSymbol predicate;
          FSymbol record_name;
          FSymbol value_name;
        ]
      when is_core_symbol "instance?" predicate ->
        [ (record_name, value_name) ]
    | _ -> []
  in
  let negated_instance_symbols = function
    | FList [ FSymbol negation; predicate ]
      when is_core_symbol "not" negation
           || is_core_symbol "__lg_not" negation ->
        instance_symbols predicate
    | _ -> []
  in
  let body =
    List.fold_right
    (fun (record_name, value_name) body ->
      FList
        [
          FSymbol "let";
          FVector
            [
              FSymbol value_name;
              FList
                [
                  FSymbol "__lg_not-instance-value";
                  FSymbol record_name;
                  FSymbol value_name;
                ];
            ];
          body;
        ])
    (instance_symbols condition |> List.sort_uniq compare)
    body
  in
  List.fold_right
    (fun (record_name, value_name) body ->
      FList
        [
          FSymbol "let";
          FVector
            [
              FSymbol value_name;
              FList
                [
                  FSymbol "__lg_instance-value";
                  FSymbol record_name;
                  FSymbol value_name;
                ];
            ];
          body;
        ])
    (negated_instance_symbols condition |> List.sort_uniq compare)
    body

let rec false_nil_predicate_names = function
  | FList [ FSymbol predicate; FSymbol name ]
    when predicate = "__lg_nil-predicate"
         || String.ends_with ~suffix:"/__lg_nil-predicate" predicate ->
      [ name ]
  | FList (FSymbol name :: conditions)
    when name = "__lg_logical-or"
         || String.ends_with ~suffix:"/__lg_logical-or" name ->
      List.concat_map false_nil_predicate_names conditions
  | _ -> []

let narrow_non_nil_name scope env name body =
  match Resolver.lookup_binding scope env name with
  | Ok (binding : Types.binding) -> (
      match Types.constraint_value_type binding.ty with
      | TNullable _ | TOcaml_app ("option", [ _ ]) ->
          FList
            [
              FSymbol "let";
              FVector
                [
                  FSymbol name;
                  FList [ FSymbol "__lg_nullable-value"; FSymbol name ];
                ];
              body;
            ]
      | _ -> body)
  | Error _ -> body

let narrow_false_nil_predicates scope env condition body =
  let names =
    match condition with
    | FSymbol alias -> (
        match Resolver.lookup_binding scope env alias with
        | Ok (binding : Types.binding) -> binding.false_non_nil_names
        | Error _ -> [])
    | condition -> false_nil_predicate_names condition
  in
  names
  |> List.sort_uniq String.compare
  |> List.fold_left
       (fun body name -> narrow_non_nil_name scope env name body)
       body

let create ~compile_expr ~dynamic_unpack ~pack_dynamic_value
    ~pack_constrained_value ~argument_compatible =
  let compile_args_for = compile_args_for compile_expr in
  let map_vector source_inner body expression =
    let item_name = "__lg_branch_vector_item" in
    let item = typed_ir source_inner (Semantic_ir.Ident item_name) in
    Result.map
      (fun body ->
        Semantic_ir.Apply
          ( Semantic_ir.Ident "Rrbvec.map",
            [
              Semantic_ir.Fun ([ Semantic_ir.PVar item_name ], body);
              expression;
            ] ))
      (body item)
  in
  let map_array source_inner body expression =
    let item_name = "__lg_branch_array_item" in
    let item = typed_ir source_inner (Semantic_ir.Ident item_name) in
    Result.map
      (fun body ->
        Semantic_ir.Apply
          ( Semantic_ir.Ident "Array.map",
            [
              Semantic_ir.Fun ([ Semantic_ir.PVar item_name ], body);
              expression;
            ] ))
      (body item)
  in
  let adapt_vector_element env target source item =
    match inject_contextual_closed_sum env ~expected:target item with
    | Some result -> Result.map (fun value -> value.semantic_expr) result
    | None -> (
    match (target, source) with
    | TOcaml "Lg_edn_backend.t", source ->
        Edn_value_elaborator.pack_expression source item.semantic_expr
    | TNamed_record record, TRecord _
      when Types.row_compatible ~expected:target ~actual:source ->
        Ok (Structural_map.as_named_record record item).semantic_expr
    | TNamed_record _, TRecord _
      when Types.row_compatible ~expected:source ~actual:target ->
        Ok item.semantic_expr
    | TRecord fields, TNamed_record _
      when Types.row_compatible ~expected:target ~actual:source ->
        Ok
          (Structural_map.record_expr fields
             (Structural_map.values_for item fields))
            .semantic_expr
    | ( (TNullable dynamic_inner
        | TOcaml_app ("option", [ dynamic_inner ])),
        (TNullable source_inner | TOcaml_app ("option", [ source_inner ])) )
      when Types.is_dynamic dynamic_inner
           && not (Types.is_dynamic source_inner) ->
        let payload_name = "__lg_branch_optional_item" in
        let payload = typed_ir source_inner (Semantic_ir.Ident payload_name) in
        Result.map
          (fun packed ->
            Semantic_ir.Match
              ( item.semantic_expr,
                [
                  ( Semantic_ir.PConstructor ("None", None),
                    Semantic_ir.Constructor ("None", None) );
                  ( Semantic_ir.PConstructor
                      ("Some", Some (Semantic_ir.PVar payload_name)),
                    Semantic_ir.Constructor ("Some", Some packed) );
                ] ))
          (pack_dynamic_value env dynamic_inner payload)
    | ( (TNullable dynamic_inner
        | TOcaml_app ("option", [ dynamic_inner ])),
        source )
      when Types.is_dynamic dynamic_inner
           && (match source with
              | TNullable _ | TOcaml_app ("option", [ _ ]) -> false
              | _ -> true) ->
        Result.map
          (fun packed -> Semantic_ir.Constructor ("Some", Some packed))
          (pack_dynamic_value env dynamic_inner
             (typed_ir source item.semantic_expr))
    | target, source
      when Types.is_dynamic target && not (Types.is_dynamic source) ->
        pack_dynamic_value env target item
    | _ ->
        Ok (coerce_expression_to_type target source item.semantic_expr))
  in
  let optional_payload = function
    | TNullable inner | TOcaml_app ("option", [ inner ]) -> Some inner
    | _ -> None
  in
  let edn_packable_static_type = Edn_value_elaborator.is_packable in
  let pack_edn_expression = Edn_value_elaborator.pack_expression in
  let rec adapt_branch_expression env result_ty (branch : typed_expr) =
    match inject_contextual_closed_sum env ~expected:result_ty branch with
    | Some result -> Result.map (fun value -> value.semantic_expr) result
    | None -> (
    match (result_ty, branch.ty) with
    | target, source when Types.equal target source ->
        Ok branch.semantic_expr
    | TOcaml "Lg_edn_backend.t", source ->
        pack_edn_expression source branch.semantic_expr
    | target, source
      when Option.is_some (protocol_value_type source) ->
        adapt_branch_expression env target
          { branch with ty = Option.get (protocol_value_type source) }
    | target, source
      when Option.is_some (Types.reduced_element target)
           && Option.is_none (Types.reduced_element source) ->
        let target_inner = Option.get (Types.reduced_element target) in
        Result.map
          (fun value ->
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Lg_runtime.Runtime_reduced.continue",
                [ value ] ))
          (adapt_branch_expression env target_inner branch)
    | ( (TNullable target_inner
        | TOcaml_app ("option", [ target_inner ])),
        (TNullable source_inner
        | TOcaml_app ("option", [ source_inner ])) )
      when not (Types.equal target_inner source_inner) ->
        let payload_name = "__lg_branch_optional_payload" in
        let payload = typed_ir source_inner (Semantic_ir.Ident payload_name) in
        Result.map
          (fun adapted ->
            Semantic_ir.Match
              ( branch.semantic_expr,
                [
                  ( Semantic_ir.PConstructor ("None", None),
                    Semantic_ir.Constructor ("None", None) );
                  ( Semantic_ir.PConstructor
                      ("Some", Some (Semantic_ir.PVar payload_name)),
                    Semantic_ir.Constructor ("Some", Some adapted) );
                ] ))
          (adapt_branch_expression env target_inner payload)
    | ( (TNullable _target_inner
        | TOcaml_app ("option", [ _target_inner ])),
        source_ty )
      when (match source_ty with TUnknown | TMeta _ | TVar _ -> true | _ -> false)
           && (match Semantic_ir.unlocated branch.semantic_expr with
              | Semantic_ir.Apply _ | Semantic_ir.Uncurried_apply _ -> true
              | _ -> false) ->
        Ok branch.semantic_expr
    | ( (TNullable target_inner
        | TOcaml_app ("option", [ target_inner ])),
        source_ty )
      when Option.is_none (optional_payload source_ty)
           && not (Types.equal source_ty TNil) ->
        Result.map
          (fun adapted -> Semantic_ir.Constructor ("Some", Some adapted))
          (adapt_branch_expression env target_inner branch)
    | TTuple target_items, TTuple source_items
      when List.length target_items = List.length source_items ->
        let names =
          List.mapi
            (fun index _ -> "__lg_branch_tuple_item_" ^ string_of_int index)
            source_items
        in
        let rec adapt_items adapted targets sources names =
          match (targets, sources, names) with
          | [], [], [] -> Ok (List.rev adapted)
          | target :: target_rest, source :: source_rest, name :: name_rest ->
              let item = typed_ir source (Semantic_ir.Ident name) in
              Result.bind
                (adapt_branch_expression env target item)
                (fun adapted_item ->
                  adapt_items (adapted_item :: adapted) target_rest source_rest
                    name_rest)
          | _ -> Error.error "internal tuple branch adaptation arity mismatch"
        in
        Result.map
          (fun items ->
            Semantic_ir.Match
              ( branch.semantic_expr,
                [
                  ( Semantic_ir.PTuple
                      (List.map (fun name -> Semantic_ir.PVar name) names),
                    Semantic_ir.Tuple items );
                ] ))
          (adapt_items [] target_items source_items names)
    | TVector target_inner, TVector (TOcaml "Lg_edn_backend.t" as source_inner)
      when not (Types.equal target_inner source_inner) ->
        heterogeneous_collection_type_error "vector"
          [ target_inner; source_inner ]
    | TVector target_inner, TVector source_inner
      when not (Types.equal target_inner source_inner) ->
        map_vector source_inner
          (adapt_vector_element env target_inner source_inner)
          branch.semantic_expr
    | TVector target_inner, source
      when (match source with TUnknown | TMeta _ | TVar _ -> false | _ -> true) -> (
        match Collection_capability.to_seq_expr env branch with
        | Ok (source_inner, sequence)
          when Option.is_some
                 (merge_branch_types target_inner source_inner) ->
            let sequence =
              if Types.equal target_inner source_inner then Ok sequence
              else
                let item_name = "__lg_vector_sequence_item" in
                let item =
                  typed_ir source_inner (Semantic_ir.Ident item_name)
                in
                Result.map
                  (fun adapted ->
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                        [
                          Semantic_ir.Fun
                            ([ Semantic_ir.PVar item_name ], adapted);
                          sequence;
                        ] ))
                  (adapt_vector_element env target_inner source_inner item)
            in
            Result.map
              (fun sequence ->
                Semantic_ir.Apply
                  ( Semantic_ir.Ident "Rrbvec.of_list",
                    [
                      Semantic_ir.Apply
                        (Semantic_ir.Ident "List.of_seq", [ sequence ]);
                    ] ))
              sequence
        | Ok (source_inner, _) ->
            Error.error
              ("cannot adapt sequence element "
              ^ Types.source_name source_inner ^ " to vector element "
              ^ Types.source_name target_inner)
        | Error _ as error -> error)
    | TArray target_inner, TArray source_inner
      when not (Types.equal target_inner source_inner) ->
        map_array source_inner
          (adapt_vector_element env target_inner source_inner)
          branch.semantic_expr
    | TSeq _, (TUnknown | TMeta _ | TVar _) -> Ok branch.semantic_expr
    | target, (TUnknown | TMeta _ | TVar _)
      when Option.is_some (Types.next_seq_element target) ->
        Ok branch.semantic_expr
    | target, _ when Option.is_some (Types.next_seq_element target) -> (
        let target_inner = Option.get (Types.next_seq_element target) in
        match Collection_capability.to_seq_expr env branch with
        | Ok (source_inner, sequence)
          when Option.is_some
                 (merge_branch_types target_inner source_inner) ->
            Ok sequence
        | Ok (source_inner, _) ->
            Error.error
              ("cannot adapt lazy sequence branch element "
              ^ Types.source_name source_inner ^ " to "
              ^ Types.source_name target_inner)
        | Error _ as error -> error)
    | TSeq target_inner, _ -> (
        match Collection_capability.to_seq_expr env branch with
        | Ok (source_inner, sequence)
          when Option.is_some
                 (merge_branch_types target_inner source_inner) ->
            Ok sequence
        | Ok (source_inner, _) ->
            Error.error
              ("cannot adapt sequence branch element "
              ^ Types.source_name source_inner ^ " to "
              ^ Types.source_name target_inner)
        | Error _ as error -> error)
    | TNamed_record record, TRecord _
      when Types.row_compatible ~expected:result_ty ~actual:branch.ty ->
        let branch = { branch with record_values = None } in
        Ok (Structural_map.as_named_record record branch).semantic_expr
    | TRecord fields, (TRecord _ | TNamed_record _)
      when Types.row_compatible ~expected:result_ty ~actual:branch.ty ->
        Ok
          (Structural_map.record_expr fields
             (Structural_map.values_for branch fields))
            .semantic_expr
    | ( TFn (target_params, target_return),
        TFn (source_params, source_return) )
      when List.length target_params = List.length source_params
           && not (Types.equal result_ty branch.ty) ->
        let function_name = "__lg_branch_function" in
        let argument_names =
          List.mapi
            (fun index _ -> "__lg_branch_argument_" ^ string_of_int index)
            target_params
        in
        let rec adapt_arguments adapted target source names =
          match (target, source, names) with
          | [], [], [] -> Ok (List.rev adapted)
          | target_ty :: target_rest,
            source_ty :: source_rest,
            name :: name_rest ->
              let argument = typed_ir target_ty (Semantic_ir.Ident name) in
              let adapted_argument =
                if Types.equal target_ty source_ty then
                  Ok argument.semantic_expr
                else if Types.is_dynamic source_ty then
                  pack_dynamic_value env source_ty argument
                else if Types.is_dynamic target_ty then
                  dynamic_unpack env source_ty argument.semantic_expr
                else if
                  Types.assignable ~policy:Host_boundary ~expected:source_ty
                    ~actual:target_ty
                then
                  Ok
                    (coerce_expression_to_type source_ty target_ty
                       argument.semantic_expr)
                else Error.error "cannot adapt function branch parameter"
              in
              Result.bind adapted_argument (fun argument ->
                  adapt_arguments (argument :: adapted) target_rest source_rest
                    name_rest)
          | _ -> Error.error "cannot adapt function branch arity"
        in
        Result.bind
          (adapt_arguments [] target_params source_params argument_names)
          (fun arguments ->
            let result =
              typed_ir source_return
                (Semantic_ir.Apply
                   (Semantic_ir.Ident function_name, arguments))
            in
            Result.map
              (fun result ->
                Semantic_ir.Let
                  ( [
                      ( Semantic_ir.PVar function_name,
                        branch.semantic_expr );
                    ],
                    Semantic_ir.Fun
                      ( List.map
                          (fun name -> Semantic_ir.PVar name)
                          argument_names,
                        result ) ))
              (adapt_branch_expression env target_return result))
    | target, _
      when Option.is_some (Types.protocol_constraint_info target)
           || Option.is_some (Types.seqable_constraint_info target)
           || Option.is_some (Types.truthy_constraint_info target)
           || Option.is_some (Types.printable_constraint_info target)
           || Option.is_some (Types.hashable_constraint_info target)
           || Option.is_some (Types.comparable_constraint_info target)
           || Option.is_some (Types.array_index_constraint_info target) ->
        pack_constrained_value env target branch
    | target, source
      when Types.is_dynamic target && not (Types.is_dynamic source) ->
        pack_dynamic_value env target branch
    | target, source
      when Types.is_dynamic source
           && not (Types.is_dynamic target)
           && (match target with TUnknown | TMeta _ | TVar _ -> false | _ -> true) ->
        dynamic_unpack env target branch.semantic_expr
    | _ ->
        Ok
          (if Types.equal branch.ty TUnknown then branch.semantic_expr
           else
             coerce_expression_to_type result_ty branch.ty
               branch.semantic_expr))
  in
  let rec requires_branch_adaptation = function
    | ty when Types.is_dynamic ty -> true
    | ty when Option.is_some (Types.reduced_element ty) -> true
    | ty when Option.is_some (Types.next_seq_element ty) -> true
    | TFn _ -> true
    | TTuple items -> List.exists requires_branch_adaptation items
    | TVector _ -> true
    | TArray _ -> true
    | TSeq _ -> true
    | TRecord _ -> true
    | TNullable inner | TOcaml_app ("option", [ inner ]) ->
        requires_branch_adaptation inner
    | _ -> false
  in
  let adapt_merged_branches env result_ty left left_code right right_code =
    if requires_branch_adaptation result_ty then
      match
        ( adapt_branch_expression env result_ty left,
          adapt_branch_expression env result_ty right )
      with
      | (Error _ as error), _ -> error
      | _, (Error _ as error) -> error
      | Ok left, Ok right -> Ok (left, right)
    else Ok (left_code, right_code)
  in
  let rec compile_vector scope env forms =
    let compile_tuple expected_types =
      let rec compile values actual_types expected forms =
        match (expected, forms) with
        | [], [] ->
            Ok
              (typed_ir (TTuple (List.rev actual_types))
                 (Semantic_ir.Tuple (List.rev values)))
        | expected_ty :: expected_rest, form :: form_rest ->
            let item_env = Env.with_expected_type (Some expected_ty) env in
            Result.bind (compile_expr scope item_env form) (fun value ->
                Result.bind
                  (adapt_branch_expression env expected_ty value)
                  (fun value ->
                    compile (value :: values) (expected_ty :: actual_types)
                      expected_rest form_rest))
        | (TNullable _ | TOcaml_app ("option", [ _ ])) :: expected_rest, [] ->
            compile
              (Semantic_ir.Constructor ("None", None) :: values)
              (TNil :: actual_types) expected_rest []
        | _ :: _, [] ->
            Error.error
              "tuple literal is missing a required destructured element"
        | [], _ :: _ ->
            Error.error "tuple literal has more elements than its static shape"
      in
      compile [] [] expected_types forms
    in
    match Env.expected_type env with
    | Some (TTuple expected_types) when forms <> [] ->
        compile_tuple expected_types
    | Some _ | None ->
    let expected_element =
      match Env.expected_type env with
      | Some (TVector element)
        when (not (Types.is_dynamic element))
             && not (Types.equal element TUnknown) ->
          Some element
      | Some _ | None -> None
    in
    let compile_expected element =
      let expression_env = Env.with_expected_type None env in
      let rec compile values = function
        | [] ->
            Ok
              (typed_ir (TVector element)
                 (Semantic_ir.Apply
                    ( Semantic_ir.Ident "Rrbvec.of_list",
                      [ Semantic_ir.List (List.rev values) ] )))
        | form :: rest ->
            Result.bind (compile_expr scope expression_env form) (fun value ->
                Result.bind
                  (adapt_branch_expression env element value)
                  (fun value -> compile (value :: values) rest))
      in
      compile [] forms
    in
    match expected_element with
    | Some element -> compile_expected element
    | None -> (
    let expression_env = Env.with_expected_type None env in
    match forms with
    | [] ->
        Ok
          (typed_ir (TVector (Type_solver.fresh ()))
             (Semantic_ir.Ident "Rrbvec.empty"))
    | first :: rest -> (
        match compile_expr scope expression_env first with
        | Error _ as err -> err
        | Ok first_expr ->
            let rec loop acc = function
              | [] -> (
                  let expressions = List.rev acc in
                  let element_ty =
                    List.fold_left
                      (fun merged expr ->
                        Option.bind merged (fun ty ->
                            merge_branch_types ty expr.ty))
                      (Some first_expr.ty) expressions
                  in
                  let heterogeneous_error () =
                    let types =
                      expressions
                      |> List.map (fun expression ->
                             Types.source_name expression.ty)
                      |> List.sort_uniq String.compare
                    in
                    Error.error
                      ("heterogeneous vector has element types "
                      ^ String.concat " | " types
                      ^ (match Env.expected_type env with
                        | Some expected ->
                            "; expected context " ^ Types.source_name expected
                        | None -> "; no expected context")
                      ^ "; define a closed sum type containing these types")
                  in
                  let edn_vector () =
                    let rec pack packed = function
                      | [] -> Ok (List.rev packed)
                      | expression :: rest ->
                          Result.bind
                            (pack_edn_expression expression.ty
                               expression.semantic_expr)
                            (fun value -> pack (value :: packed) rest)
                    in
                    match pack [] expressions with
                    | Ok values ->
                        Ok
                          (typed_ir (TVector (TOcaml "Lg_edn_backend.t"))
                             (Semantic_ir.Apply
                                ( Semantic_ir.Ident "Rrbvec.of_list",
                                  [ Semantic_ir.List values ] )))
                    | Error _ -> heterogeneous_error ()
                  in
                  let homogeneous_map_vector ~fallback_to_edn () =
                    let record_map_storage_fields fields =
                      fields
                      |> List.filter (fun (field : field) ->
                             (not (Types.is_record_extension_field field))
                             && not (Types.is_record_identity_field field))
                    in
                    let record_map_value_type fields =
                      let storage_fields = record_map_storage_fields fields in
                      match storage_fields with
                      | [] -> None
                      | (first : field) :: rest ->
                          List.fold_left
                            (fun merged (field : field) ->
                              Option.bind merged (fun ty ->
                                  merge_branch_types ty field.ty))
                            (Some first.ty) rest
                    in
                    let candidate_type expression =
                      match Types.dynamic_map_types expression.ty with
                      | Some (key_ty, value_ty) -> Some (key_ty, value_ty)
                      | None -> (
                          match expression.ty with
                          | TRecord fields -> (
                              match record_map_value_type fields with
                              | Some value_ty -> Some (TKeyword, value_ty)
                              | None -> None)
                          | TNamed_record { fields; nominal = false; _ } -> (
                              match record_map_value_type fields with
                              | Some value_ty -> Some (TKeyword, value_ty)
                              | None -> None)
                          | _ -> None)
                    in
                    let merge_pair left right =
                      match (left, right) with
                      | Some (left_key, left_value), Some (right_key, right_value) ->
                          Option.bind
                            (merge_branch_types left_key right_key)
                            (fun key_ty ->
                              Option.map
                                (fun value_ty -> (key_ty, value_ty))
                                (merge_branch_types left_value right_value))
                      | _ -> None
                    in
                    let common =
                      match expressions with
                      | [] -> None
                      | first :: rest ->
                          List.fold_left
                            (fun common expression ->
                              merge_pair common (candidate_type expression))
                            (candidate_type first) rest
                    in
                    let has_runtime_map =
                      List.exists
                        (fun expression ->
                          Option.is_some
                            (Types.dynamic_map_types expression.ty))
                        expressions
                    in
                    let has_record_map =
                      List.exists
                        (fun expression ->
                          match expression.ty with
                          | TRecord fields
                          | TNamed_record { fields; nominal = false; _ } ->
                              Option.is_some (record_map_value_type fields)
                          | _ -> false)
                        expressions
                    in
                    match common with
                    | Some (TKeyword, value_ty)
                      when has_runtime_map && has_record_map ->
                        let map_ty = Types.dynamic_map TKeyword value_ty in
                        let adapt_record expression fields =
                          let source_name = "__lg_vector_record_map_value" in
                          let storage_fields =
                            record_map_storage_fields fields
                          in
                          let source =
                            {
                              expression with
                              semantic_expr = Semantic_ir.Ident source_name;
                            }
                          in
                          let rec assoc_fields map = function
                            | [] -> map
                            | (field : field) :: rest ->
                                let value =
                                  Structural_map.field_expr source field
                                  |> fun value ->
                                  coerce_expression_to_type value_ty field.ty
                                    value
                                in
                                assoc_fields
                                  (Semantic_ir.Apply
                                     ( Semantic_ir.Ident
                                         "Lg_runtime.Runtime_map.assoc",
                                       [
                                         map;
                                         Semantic_ir.String field.keyword;
                                         value;
                                       ] ))
                                  rest
                          in
                          Semantic_ir.Let
                            ( [
                                ( Semantic_ir.PVar source_name,
                                  expression.semantic_expr );
                              ],
                              assoc_fields
                                (Semantic_ir.Ident
                                   "Lg_runtime.Runtime_map.empty")
                                storage_fields )
                        in
                        let adapt_expression expression =
                          match expression.ty with
                          | ty when Types.equal ty map_ty ->
                              Ok expression.semantic_expr
                          | TRecord fields
                          | TNamed_record { fields; nominal = false; _ } -> (
                              match record_map_value_type fields with
                              | Some actual_value
                                when Types.equal actual_value value_ty ->
                                  Ok (adapt_record expression fields)
                              | _ ->
                                  Error.error
                                    "not a homogeneous static map")
                          | ty -> (
                              match Types.dynamic_map_types ty with
                              | Some (key_ty, actual_value)
                                when Types.equal key_ty TKeyword
                                     && Types.equal actual_value value_ty ->
                                  Ok expression.semantic_expr
                              | _ -> Error.error "not a homogeneous static map")
                        in
                        let rec adapt values = function
                          | [] -> Ok (List.rev values)
                          | expression :: rest ->
                              Result.bind (adapt_expression expression)
                                (fun value -> adapt (value :: values) rest)
                        in
                        Result.map
                          (fun values ->
                            typed_ir (TVector map_ty)
                              (Semantic_ir.Apply
                                 ( Semantic_ir.Ident "Rrbvec.of_list",
                                   [ Semantic_ir.List values ] )))
                          (adapt [] expressions)
                    | Some _ | None ->
                         if fallback_to_edn then edn_vector ()
                         else heterogeneous_error ()
                   in
                   match element_ty with
                  | None -> homogeneous_map_vector ~fallback_to_edn:true ()
                  | Some element_ty
                    when Option.is_some
                           (Types.capability_constraint_value element_ty)
                         &&
                         (match
                            expressions
                            |> List.map (fun expression ->
                                   Types.constraint_value_type expression.ty)
                          with
                         | [] | [ _ ] -> false
                         | first :: rest ->
                             Option.is_none
                               (List.fold_left
                                  (fun merged ty ->
                                    Option.bind merged (fun merged ->
                                        merge_branch_types merged ty))
                                  (Some first) rest)) ->
                      heterogeneous_error ()
                  | Some element_ty when Types.is_dynamic element_ty ->
                      heterogeneous_error ()
                  | Some
                      ((TNullable dynamic_inner
                       | TOcaml_app ("option", [ dynamic_inner ])) as element_ty)
                    when Types.is_dynamic dynamic_inner ->
                      let pack_value value =
                        match pack_plain_dynamic_value value with
                        | Some packed -> Ok packed
                        | None -> pack_dynamic_value env dynamic_inner value
                      in
                      let pack_optional expression =
                        match expression.ty with
                        | TNil ->
                            Ok (Semantic_ir.Constructor ("None", None))
                        | TNullable actual_inner
                        | TOcaml_app ("option", [ actual_inner ]) ->
                            if Types.is_dynamic actual_inner then
                              Ok expression.semantic_expr
                            else
                              let value_name = "__lg_vector_optional_value" in
                              let value =
                                typed_ir actual_inner
                                  (Semantic_ir.Ident value_name)
                              in
                              Result.map
                                (fun packed ->
                                  Semantic_ir.Match
                                    ( expression.semantic_expr,
                                      [
                                        ( Semantic_ir.PConstructor
                                            ("None", None),
                                          Semantic_ir.Constructor
                                            ("None", None) );
                                        ( Semantic_ir.PConstructor
                                            ( "Some",
                                              Some
                                                (Semantic_ir.PVar value_name) ),
                                          Semantic_ir.Constructor
                                            ("Some", Some packed) );
                                      ] ))
                                (pack_value value)
                        | _ ->
                            Result.map
                              (fun packed ->
                                Semantic_ir.Constructor
                                  ("Some", Some packed))
                              (pack_value expression)
                      in
                      let rec pack values = function
                        | [] -> Ok (List.rev values)
                        | expression :: rest ->
                            Result.bind (pack_optional expression) (fun value ->
                                pack (value :: values) rest)
                      in
                      Result.map
                        (fun values ->
                          typed_ir (TVector element_ty)
                            (Semantic_ir.Apply
                               ( Semantic_ir.Ident "Rrbvec.of_list",
                                 [ Semantic_ir.List values ] )))
                        (pack [] expressions)
                  | Some element_ty
                    when not
                           (List.for_all
                              (fun expression ->
                                Types.equal element_ty expression.ty
                                ||
                                match (element_ty, expression.ty) with
                                | TNullable _, TNil -> true
                                | TNullable inner, TNullable actual
                                | TNullable inner, actual ->
                                    Types.assignable ~policy:Host_boundary
                                      ~expected:inner ~actual
                                | TOcaml "Lg_edn_backend.t", actual ->
                                    edn_packable_static_type actual
                                | TVector (TOcaml "Lg_edn_backend.t"), TVector actual ->
                                    edn_packable_static_type actual
                                | TVector _, TVector _
                                  when Type_solver.is_open element_ty
                                       || Type_solver.is_open expression.ty ->
                                    true
                                | _ -> false)
                              expressions) ->
                      if
                        List.for_all
                          (fun expression ->
                            edn_packable_static_type expression.ty)
                          expressions
                      then edn_vector ()
                      else homogeneous_map_vector ~fallback_to_edn:false ()
                  | Some element_ty ->
                      let rec adapt values = function
                        | [] -> Ok (List.rev values)
                        | expression :: rest ->
                            let expression =
                              {
                                expression with
                                semantic_expr =
                                  capability_storage_expression expression.ty
                                    expression.semantic_expr;
                              }
                            in
                            Result.bind
                              (adapt_branch_expression env element_ty expression)
                              (fun value -> adapt (value :: values) rest)
                      in
                      Result.map
                        (fun values ->
                          typed_ir (TVector element_ty)
                            (Semantic_ir.Apply
                               ( Semantic_ir.Ident "Rrbvec.of_list",
                                 [ Semantic_ir.List values ] )))
                        (adapt [] expressions)
                  )
              | form :: rest -> (
                  match compile_expr scope expression_env form with
                  | Error _ as err -> err
                  | Ok expr -> loop (expr :: acc) rest)
            in
            loop [ first_expr ] rest))
  and compile_map scope env pairs =
    match
      ( pairs,
        Option.bind (Env.expected_type env) (fun expected ->
            Option.map
              (fun factory -> (expected, factory))
              (Env.find_empty_map_default expected env)) )
    with
    | [], Some (expected, factory) ->
        Ok
          (typed_ir expected
             (Semantic_ir.Apply (Semantic_ir.Ident factory, [])))
    | _ ->
    let expected_map_types =
      match Option.bind (Env.expected_type env) Types.dynamic_map_types with
      | Some _ as map_types -> map_types
      | None ->
          Option.bind (Env.expected_type env) (fun expected ->
              Option.bind (Types.seqable_constraint_info expected)
                (fun (_, entry_ty, _) ->
                  Option.map
                    (fun value_ty -> (value_ty, value_ty))
                    (Types.seqable_constraint_element entry_ty)))
    in
    match expected_map_types with
    | Some (key_ty, value_ty) ->
        let compile_entry (key_form, value_form) =
          Result.bind
            (compile_expr scope
               (Env.with_expected_type (Some key_ty) env)
               key_form)
            (fun key ->
              Result.map
                (fun value ->
                  Semantic_ir.Tuple
                    [
                      coerce_expression_to_type key_ty key.ty key.semantic_expr;
                      coerce_expression_to_type value_ty value.ty
                        value.semantic_expr;
                    ])
                (compile_expr scope
                   (Env.with_expected_type (Some value_ty) env)
                   value_form))
        in
        let rec compile_entries entries = function
          | [] ->
              Ok
                (typed_ir (Types.dynamic_map key_ty value_ty)
                   (Semantic_ir.Apply
                      ( Semantic_ir.Ident "Lg_runtime.Runtime_map.of_list",
                        [ Semantic_ir.List (List.rev entries) ] )))
          | pair :: rest ->
              Result.bind (compile_entry pair) (fun entry ->
                  compile_entries (entry :: entries) rest)
        in
        let keywords =
          List.filter_map
            (fun (key, _value) ->
              match key with FKeyword keyword -> Some (keyword, ()) | _ -> None)
            pairs
        in
        let validate_keywords =
          if List.length keywords = List.length pairs then
            Structural_map.validate_unique_keywords keywords
          else Ok ()
        in
        Result.bind validate_keywords (fun () -> compile_entries [] pairs)
    | None when
      List.exists
        (fun (key, _value) -> match key with FKeyword _ -> false | _ -> true)
        pairs
      ->
      let arguments =
        List.concat_map (fun (key, value) -> [ key; value ]) pairs
      in
      compile_expr scope env (FList (FSymbol "__lg_hash-map" :: arguments))
    | None ->
    let compile_pair = function
      | FKeyword keyword, value_form -> (
          match compile_expr scope env value_form with
          | Ok value -> Ok (keyword, value_form, value)
          | Error _ as err -> err)
      | _ -> Error.error "map keys must be keywords"
    in
    let rec loop acc = function
      | [] ->
          let pairs = List.rev acc in
          if pairs = [] then
            let map_ty =
              match Env.expected_type env with
              | Some expected
                when Option.is_some (Types.dynamic_map_types expected) ->
                  expected
              | Some _ | None -> Types.dynamic_map TUnknown TUnknown
            in
            Ok
              (typed_ir map_ty
                 (Semantic_ir.Ident "Lg_runtime.Runtime_map.empty"))
          else
          let keyword_pairs =
            List.map (fun (keyword, _form, value) -> (keyword, value)) pairs
          in
          Result.bind
            (Structural_map.validate_unique_keywords keyword_pairs)
            (fun () ->
              let value_ty =
                match keyword_pairs with
                | [] -> None
                | (_, first) :: rest ->
                    List.fold_left
                      (fun merged (_, value) ->
                        Option.bind merged (fun ty ->
                            merge_branch_types ty value.ty))
                      (Some first.ty) rest
              in
              let rec contains_closed_edn ty =
                match Types.constraint_value_type ty with
                | TOcaml "Lg_edn_backend.t" -> true
                | TNullable inner | TList inner | TSeq inner | TVector inner
                | TArray inner | TSet inner
                | TOcaml_app ("option", [ inner ]) ->
                    contains_closed_edn inner
                | _ -> false
              in
              if
                Option.is_none value_ty
                && List.exists
                     (fun (_, value) -> contains_closed_edn value.ty)
                     keyword_pairs
                && List.for_all
                     (fun (_, value) -> edn_packable_static_type value.ty)
                     keyword_pairs
              then
                let rec pack entries = function
                  | [] ->
                      Ok
                        (typed_ir (TOcaml "Lg_edn_backend.t")
                           (Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_metadata.of_entries",
                                [ Semantic_ir.List (List.rev entries) ] )))
                  | (keyword, value) :: rest ->
                      Result.bind
                        (pack_edn_expression value.ty value.semantic_expr)
                        (fun packed ->
                          let key =
                            Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_metadata.of_keyword",
                                [ Semantic_ir.String keyword ] )
                          in
                          pack
                            (Semantic_ir.Tuple [ key; packed ] :: entries)
                            rest)
                in
                pack [] keyword_pairs
              else
                let fields =
                  pairs
                  |> List.map (fun (keyword, _form, value) ->
                         make_map_field keyword value.ty)
                in
                let values =
                  List.map2
                    (fun field (_keyword, _form, value) ->
                      (field, value.semantic_expr))
                    fields pairs
                in
                Ok (Structural_map.record_expr fields values))
      | pair :: rest -> (
          match compile_pair pair with
          | Ok pair -> loop (pair :: acc) rest
          | Error _ as err -> err)
    in
    loop [] pairs
  and option_payload_type = function
    | TNullable payload_ty -> Ok payload_ty
    | TNil -> Ok TUnknown
    | TOcaml_app ("option", [ payload_ty ]) ->
        Ok (lg_metadata_type_for_ocaml_payload payload_ty)
    | TOcaml "option" -> Ok TUnknown
    | TUnknown | TMeta _ | TVar _ -> Ok TUnknown
    | ty ->
        Error.error
          ("option binding requires an option value, got "
         ^ Types.source_name ty)
  and parse_option_binding form error_message =
    match form with
    | FVector [ ((FSymbol _ | FVector _ | FMap _) as pattern); option_form ] ->
        Ok (pattern, option_form)
    | _ -> Error.error error_message
  and compile_option_match ?(require_truthy = false) scope env pattern
      option_form compile_some compile_none branch_error =
    let payload_name = "__lg_option_value" in
    let compile_some_branch payload_ty =
      let payload = typed_ir payload_ty (Semantic_ir.Ident payload_name) in
      match Destructure.bind_pattern ~env payload pattern with
      | Error _ as error -> error
      | Ok bindings -> (
          let env_bindings =
            bindings
            |> List.map (fun (binding : Destructure.local_binding) ->
                   ( Names.scoped_key scope binding.source_name,
                     Types.binding binding.ocaml_name binding.ty ))
          in
          let some_env = Env.add_bindings env_bindings env in
          match compile_some some_env with
          | Error _ as error -> error
          | Ok expression ->
              let ir_bindings =
                bindings
                |> List.map (fun (binding : Destructure.local_binding) ->
                       let pattern =
                         if has_capability binding.ty then
                           capability_pattern binding.ocaml_name binding.ty
                         else Semantic_ir.PVar binding.ocaml_name
                       in
                       let expression =
                         if has_capability binding.ty then
                           capability_storage_expression binding.ty
                             binding.semantic_expr
                         else binding.semantic_expr
                       in
                       (located_pattern binding.identity pattern, expression))
              in
              if ir_bindings = [] then Ok expression
              else
                Ok
                  {
                    expression with
                    semantic_expr =
                      Semantic_ir.Let (ir_bindings, expression.semantic_expr);
                  })
    in
    let option_expression =
      match option_form with
      | FList [ FSymbol "first"; collection_form ] -> (
          match compile_expr scope env collection_form with
          | Error _ as error -> error
          | Ok collection -> (
              match Collection_capability.to_seq_expr env collection with
              | Error _ -> Error.error "first expects a seqable value"
              | Ok (inner, sequence) ->
                  Ok
                    (typed_ir (TNullable inner)
                       (Semantic_ir.Apply
                          ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.first_opt",
                            [ sequence ] )))))
      | _ -> compile_expr scope env option_form
    in
    match option_expression with
    | Error _ as err -> err
    | Ok option_expr
      when require_truthy
           && Option.is_none (Types.next_seq_element option_expr.ty)
           &&
           match Types.constraint_value_type option_expr.ty with
           | TNil | TBool | TNullable _ | TOcaml "option"
           | TOcaml_app ("option", [ _ ]) | TSeq _ | TUnknown | TMeta _
           | TVar _ ->
               false
           | ty when Types.is_dynamic ty -> false
           | _ -> true ->
        Result.map
          (fun some_expr ->
            {
              some_expr with
              semantic_expr =
                Semantic_ir.Let
                  ( [
                      ( Semantic_ir.PVar payload_name,
                        option_expr.semantic_expr );
                    ],
                    some_expr.semantic_expr );
            })
          (compile_some_branch option_expr.ty)
    | Ok option_expr
      when Option.is_some (Types.next_seq_element option_expr.ty) -> (
        let element_ty = Option.get (Types.next_seq_element option_expr.ty) in
        let payload_ty = TSeq element_ty in
        match (compile_some_branch payload_ty, compile_none ()) with
        | (Error _ as error), _ -> error
        | _, (Error _ as error) -> error
        | Ok some_expr, Ok none_expr -> (
            match merge_branch_expressions some_expr none_expr with
            | None -> Error.error branch_error
            | Some (result_ty, some_code, none_code) -> (
                match
                  adapt_merged_branches env result_ty some_expr some_code
                    none_expr none_code
                with
                | Error _ as error -> error
                | Ok (some_code, none_code) ->
                    Ok
                      (typed_ir result_ty
                         (Semantic_ir.Let
                            ( [
                                ( Semantic_ir.PVar payload_name,
                                  option_expr.semantic_expr );
                              ],
                              Semantic_ir.If
                                ( Semantic_ir.Apply
                                    ( Semantic_ir.Ident
                                        "Lg_runtime.Runtime_seq.is_empty",
                                      [ Semantic_ir.Ident payload_name ] ),
                                  none_code,
                                  some_code ) ))))))
    | Ok option_expr when Types.is_dynamic option_expr.ty -> (
        match (compile_some_branch option_expr.ty, compile_none ()) with
        | (Error _ as error), _ -> error
        | _, (Error _ as error) -> error
        | Ok some_expr, Ok none_expr -> (
            match merge_branch_expressions some_expr none_expr with
            | None -> Error.error branch_error
            | Some (result_ty, some_code, none_code) -> (
                match
                  adapt_merged_branches env result_ty some_expr some_code
                    none_expr none_code
                with
                | Error _ as error -> error
                | Ok (some_code, none_code) ->
                    Ok
                      (typed_ir result_ty
                         (Semantic_ir.Let
                            ( [
                                ( Semantic_ir.PVar payload_name,
                                  option_expr.semantic_expr );
                              ],
                              Semantic_ir.If
                                ( (if require_truthy then
                                     truthiness_expression ~env option_expr.ty
                                       (Semantic_ir.Ident payload_name)
                                   else
                                     Semantic_ir.Apply
                                       ( Semantic_ir.Ident "not",
                                         [
                                           Semantic_ir.Apply
                                             ( Semantic_ir.Ident
                                                 "Lg_runtime.Runtime_dynamic.is_nil",
                                               [
                                                 Semantic_ir.Ident payload_name;
                                               ] );
                                         ] )),
                                  some_code,
                                  none_code ) ))))))
    | Ok option_expr
      when (match option_expr.ty with
           | TNullable _ | TNil | TOcaml "option"
           | TOcaml_app ("option", [ _ ]) | TUnknown | TMeta _ | TVar _ ->
               false
           | _ -> true) -> (
        match (compile_some_branch option_expr.ty, compile_none ()) with
        | (Error _ as error), _ -> error
        | _, (Error _ as error) -> error
        | Ok some_expr, Ok none_expr -> (
            match merge_branch_expressions some_expr none_expr with
            | None -> Error.error branch_error
            | Some (result_ty, some_code, none_code) -> (
                match
                  adapt_merged_branches env result_ty some_expr some_code
                    none_expr none_code
                with
                | Error _ as error -> error
                | Ok (some_code, none_code) ->
                    let body =
                      if require_truthy then
                        Semantic_ir.If
                          ( truthiness_expression ~env option_expr.ty
                              (Semantic_ir.Ident payload_name),
                            some_code,
                            none_code )
                      else some_code
                    in
                    Ok
                      (typed_ir result_ty
                         (Semantic_ir.Let
                            ( [
                                ( Semantic_ir.PVar payload_name,
                                  option_expr.semantic_expr );
                              ],
                              body ))))))
    | Ok option_expr -> (
        match option_payload_type option_expr.ty with
        | Error _ as err -> err
        | Ok payload_ty -> (
            match (compile_some_branch payload_ty, compile_none ()) with
            | (Error _ as err), _ -> err
            | _, (Error _ as err) -> err
            | Ok some_expr, Ok none_expr -> (
                match merge_branch_expressions some_expr none_expr with
                | None ->
                    Error.error
                      (branch_error ^ ": " ^ Types.source_name some_expr.ty
                     ^ " (" ^ Types.ocaml_name some_expr.ty ^ ") and "
                     ^ Types.source_name none_expr.ty ^ " ("
                     ^ Types.ocaml_name none_expr.ty ^ ")")
                | Some (result_ty, some_code, none_code) -> (
                    match
                      adapt_merged_branches env result_ty some_expr some_code
                        none_expr none_code
                    with
                    | Error _ as error -> error
                    | Ok (some_code, none_code) ->
                        let some_code =
                          if require_truthy then
                            Semantic_ir.If
                              ( truthiness_expression ~env payload_ty
                                  (Semantic_ir.Ident payload_name),
                                some_code,
                                none_code )
                          else some_code
                        in
                        Ok
                          (typed_ir result_ty
                             (Semantic_ir.Match
                                ( option_expr.semantic_expr,
                                  [
                                    ( Semantic_ir.PConstructor
                                        ( "Some",
                                          Some
                                            (if has_capability payload_ty then
                                               capability_pattern payload_name
                                                 payload_ty
                                             else
                                               Semantic_ir.PVar payload_name) ),
                                      some_code );
                                    ( Semantic_ir.PConstructor ("None", None),
                                      none_code );
                                  ] )))))))
  and compile_if_let scope env binding_form then_form else_form =
    match
      parse_option_binding binding_form
        "if-let requires [name option], then, and else"
    with
    | Error _ as err -> err
    | Ok (name, option_form) ->
        compile_option_match ~require_truthy:true scope env name option_form
          (fun some_env -> compile_expr scope some_env then_form)
          (fun () -> compile_expr scope env else_form)
          "if-let branches must have same type"
  and compile_if_some scope env binding_form then_form else_form =
    match
      parse_option_binding binding_form
        "if-some requires [name option], then, and else"
    with
    | Error _ as err -> err
    | Ok (name, option_form) ->
        compile_option_match scope env name option_form
          (fun some_env -> compile_expr scope some_env then_form)
          (fun () -> compile_expr scope env else_form)
          "if-some branches have incompatible types; define a closed sum type"
  and compile_when_binding ~require_truthy scope env binding_form body_forms
      error_prefix =
    match
      parse_option_binding binding_form
        (error_prefix ^ " requires [name option] and a body")
    with
    | Error _ as err -> err
    | Ok (name, option_form) ->
        compile_option_match ~require_truthy scope env name option_form
          (fun some_env ->
            compile_body scope some_env
              (error_prefix ^ " requires a body")
              body_forms)
          (fun () ->
            Ok (typed_ir TNil (Semantic_ir.Constructor ("None", None))))
          (error_prefix ^ " body cannot be made nullable")
  and compile_when_let scope env binding_form body_forms =
    compile_when_binding ~require_truthy:true scope env binding_form body_forms
      "when-let"
  and compile_when_some scope env binding_form body_forms =
    compile_when_binding ~require_truthy:false scope env binding_form body_forms
      "when-some"
  and compile_let_some scope env bindings_form then_form else_form =
    let rec parse_bindings acc = function
      | [] -> Ok (List.rev acc)
      | (FSymbol _ as pattern) :: option_form :: rest ->
          parse_bindings ((pattern, option_form) :: acc) rest
      | _ -> Error.error "let-some bindings require name/option pairs"
    in
    match bindings_form with
    | FVector forms -> (
        match parse_bindings [] forms with
        | Error _ as err -> err
        | Ok [] -> Error.error "let-some requires at least one binding"
        | Ok bindings -> (
            match compile_expr scope env else_form with
            | Error _ as err -> err
            | Ok else_expr ->
                let rec compile_bindings current_env = function
                  | [] -> compile_expr scope current_env then_form
                  | (pattern, option_form) :: rest ->
                      compile_option_match scope current_env pattern option_form
                        (fun some_env -> compile_bindings some_env rest)
                        (fun () -> Ok else_expr)
                        "let-some branches must have same type"
                in
                compile_bindings env bindings))
    | _ -> Error.error "let-some bindings must be a vector"
  and compile_if scope env condition then_form else_form =
    let literal_non_boolean_truthy =
      match condition with
      | FInt _ | FFloat _ | FDecimal _ | FChar _ | FString _ | FRegex _
      | FKeyword _ ->
          true
      | FSymbol _ | FCoreSymbol _ | FBool _ | FList _ | FVector _ | FMap _ ->
          false
    in
    let then_form = narrow_type_predicates scope env condition then_form in
    let else_form =
      narrow_false_nil_predicates scope env condition else_form
      |> narrow_false_instance_predicates scope env condition
      |> narrow_false_fn_predicates scope env condition
      |> narrow_false_scalar_predicates scope env condition
    in
    let condition_env =
      match condition with
      | FList (FSymbol ("__lg_logical-and" | "__lg_logical-or") :: _) ->
          Env.with_expected_type (Some TBool) env
      | _ -> env
    in
    let rec terminal_boolean_expression expression =
      match Semantic_ir.unlocated expression with
      | Semantic_ir.Bool value -> Some value
      | Semantic_ir.Typed (_, expression) ->
          terminal_boolean_expression expression
      | _ -> None
    in
    let rec evaluated_static_boolean expression =
      match Semantic_ir.unlocated expression with
      | Semantic_ir.Bool value -> Some value
      | Semantic_ir.Typed (_, expression) ->
          evaluated_static_boolean expression
      | Semantic_ir.Sequence expressions -> (
          match List.rev expressions with
          | expression :: _ -> terminal_boolean_expression expression
          | [] -> None)
      | _ -> None
    in
    let static_protocol_condition =
      match condition with
      | FList [ FSymbol predicate; _protocol; _receiver ] ->
          has_source_name predicate "satisfies?"
      | _ -> false
    in
    let compile_static_branch condition =
      match
        if static_protocol_condition then
          evaluated_static_boolean condition.semantic_expr
        else None
      with
      | None -> None
      | Some take_then ->
          let branch_form = if take_then then then_form else else_form in
          Some
            (Result.bind (condition_expression ~env condition) (fun condition_code ->
                 Result.map
                   (fun branch ->
                     match Semantic_ir.unlocated condition_code with
                     | Semantic_ir.Bool _ -> branch
                     | _ ->
                         typed_ir branch.ty
                           (Semantic_ir.Sequence
                              [ condition_code; branch.semantic_expr ]))
                   (compile_expr scope env branch_form)))
    in
    let compile_tuple_branch expected_types = function
      | FVector forms when List.length expected_types = List.length forms ->
          let rec compile values expected_types forms =
            match (expected_types, forms) with
            | [], [] ->
                Ok
                  (typed_ir (TTuple expected_types)
                     (Semantic_ir.Tuple (List.rev values)))
            | expected :: expected_rest, form :: form_rest -> (
                match compile_expr scope env form with
                | Error _ as error -> error
                | Ok value -> (
                    let next_form =
                      match form with
                      | FList [ FSymbol name; _ ] ->
                          has_source_name name "__lg_next"
                          || has_source_name name "next"
                      | FList [ FCoreSymbol symbol; _ ] ->
                          let name = Ast.core_symbol_name symbol in
                          has_source_name name "__lg_next"
                          || has_source_name name "next"
                      | _ -> false
                    in
                    let expression =
                      if Types.is_dynamic expected then
                        pack_plain_dynamic_value value
                      else
                        match
                          ( expected,
                            Types.seqable_constraint_info value.ty )
                        with
                        | ( (TNullable (TSeq _)
                            | TOcaml_app ("option", [ TSeq _ ])),
                            None )
                          when Types.equal value.ty TNil ->
                            Some (Semantic_ir.Constructor ("None", None))
                        | ( (TNullable (TSeq expected_inner)
                            | TOcaml_app
                                ("option", [ TSeq expected_inner ])),
                            Some
                              ( requirement,
                                actual_inner,
                                _storage_ty ) )
                          when Types.assignable ~policy:Host_boundary
                                 ~expected:expected_inner
                                 ~actual:actual_inner ->
                            let packed_name =
                              "__lg_optional_sequence_branch"
                            in
                            let packed = Semantic_ir.Ident packed_name in
                            let sequence adapter =
                              Semantic_ir.Apply
                                ( adapter,
                                  [
                                    Semantic_ir.Apply
                                      (Semantic_ir.Ident "snd", [ packed ]);
                                  ] )
                            in
                            let converted =
                              match requirement with
                              | `Required ->
                                  Semantic_ir.Constructor
                                    ( "Some",
                                      Some
                                        (sequence
                                           (Semantic_ir.Apply
                                              ( Semantic_ir.Ident "fst",
                                                [ packed ] ))) )
                              | `Optional | `Optional_sequential ->
                                  let adapter_name =
                                    "__lg_optional_sequence_adapter"
                                  in
                                  Semantic_ir.Match
                                    ( Semantic_ir.Apply
                                        (Semantic_ir.Ident "fst", [ packed ]),
                                      [
                                        ( Semantic_ir.PConstructor
                                            ("None", None),
                                          Semantic_ir.Constructor
                                            ("None", None) );
                                        ( Semantic_ir.PConstructor
                                            ( "Some",
                                              Some
                                                (Semantic_ir.PVar
                                                   adapter_name) ),
                                          (if next_form then
                                             Semantic_ir.Apply
                                               ( Semantic_ir.Ident
                                                   "Lg_runtime.Runtime_seq.non_empty",
                                                 [
                                                   sequence
                                                     (Semantic_ir.Ident
                                                        adapter_name);
                                                 ] )
                                           else
                                             Semantic_ir.Constructor
                                               ( "Some",
                                                 Some
                                                   (sequence
                                                      (Semantic_ir.Ident
                                                         adapter_name)) )) );
                                      ] )
                            in
                            Some
                              (Semantic_ir.Let
                                 ( [
                                     ( Semantic_ir.PVar packed_name,
                                       value.semantic_expr );
                                   ],
                                   converted ))
                        | ( (TNullable (TSeq expected_inner)
                            | TOcaml_app
                                ("option", [ TSeq expected_inner ])),
                            None ) -> (
                            match Collection_capability.to_seq_expr env value with
                            | Ok (actual_inner, sequence)
                              when Types.assignable ~policy:Host_boundary
                                     ~expected:expected_inner
                                     ~actual:actual_inner ->
                                Some
                                  (if
                                     next_form
                                     || Option.is_some
                                          (Types.next_seq_element value.ty)
                                   then
                                     Semantic_ir.Apply
                                       ( Semantic_ir.Ident
                                           "Lg_runtime.Runtime_seq.non_empty",
                                         [ sequence ] )
                                   else
                                     Semantic_ir.Constructor
                                       ("Some", Some sequence))
                            | Ok _ | Error _ -> None)
                        | _ -> (
                        match Types.seqable_constraint_info expected with
                        | Some _ -> (
                            match
                              pack_constrained_value env expected value
                            with
                            | Ok packed -> Some packed
                            | Error _ -> None)
                        | None -> (
                        match Types.next_seq_element expected with
                        | Some expected_inner -> (
                            match
                              Collection_capability.to_seq_expr env value
                            with
                            | Ok (actual_inner, sequence)
                              when Types.assignable ~policy:Host_boundary
                                     ~expected:expected_inner
                                     ~actual:actual_inner ->
                                Some sequence
                            | _ -> None)
                        | None -> (
                            match expected with
                            | TSeq expected_inner -> (
                                match
                                  Collection_capability.to_seq_expr env value
                                with
                                | Ok (actual_inner, sequence)
                                  when Types.assignable ~policy:Host_boundary
                                         ~expected:expected_inner
                                         ~actual:actual_inner ->
                                    Some sequence
                                | _ -> None)
                            | _
                              when Types.assignable ~policy:Host_boundary
                                     ~expected ~actual:value.ty ->
                                Some
                                  (coerce_expression_to_type expected value.ty
                                     value.semantic_expr)
                            | _ -> None)))
                    in
                    match expression with
                    | None ->
                        Error.error "if tuple branches must have same type"
                    | Some expression ->
                        compile (expression :: values) expected_rest form_rest))
            | _ -> Error.error "if tuple branches must have same arity"
          in
          let result = compile [] expected_types forms in
          Result.map
            (fun expression -> { expression with ty = TTuple expected_types })
            result
      | _ -> Error.error "if tuple branch must be a vector"
    in
    match compile_expr scope condition_env condition with
    | Error _ as error -> error
    | Ok condition -> (
        match compile_static_branch condition with
        | Some result -> result
        | None -> (
    match (compile_expr scope env then_form, compile_expr scope env else_form) with
    | (Error _ as err), _ -> err
    | _, (Error _ as err) -> err
    | Ok then_expr, Ok _ when literal_non_boolean_truthy -> Ok then_expr
    | Ok then_expr, Ok else_expr -> (
        let contextual_branches =
          match Env.expected_type env with
          | None -> Ok (then_expr, else_expr)
          | Some expected -> (
              match
                ( inject_contextual_closed_sum env ~expected then_expr,
                  inject_contextual_closed_sum env ~expected else_expr )
              with
              | None, None -> Ok (then_expr, else_expr)
              | Some (Error _ as error), _ -> error
              | _, Some (Error _ as error) -> error
              | Some (Ok then_expr), Some (Ok else_expr) ->
                  Ok (then_expr, else_expr)
              | Some (Ok _), None | None, Some (Ok _) ->
                  Ok (then_expr, else_expr))
        in
        Result.bind contextual_branches (fun (then_expr, else_expr) ->
        let aligned =
          match (then_expr.ty, else_expr.ty) with
          | TTuple then_types, TTuple else_types
            when List.length then_types = List.length else_types
                 && ((not (Types.equal then_expr.ty else_expr.ty))
                    || List.exists
                         (fun ty ->
                           Option.is_some
                             (Types.seqable_constraint_info ty))
                         then_types)
                 &&
                 match (then_form, else_form) with
                    | FVector _, FVector _ -> true
                 | _ -> false ->
              let merged_types =
                List.map2
                  (fun left right ->
                    match
                      ( Types.seqable_constraint_info left,
                        Types.seqable_constraint_info right )
                    with
                    | ( Some (left_requirement, left_element, _),
                        Some (right_requirement, right_element, _) ) -> (
                        match merge_branch_types left_element right_element with
                        | None -> None
                        | Some element ->
                            Some
                              (match (left_requirement, right_requirement) with
                              | `Required, `Required ->
                                  TSeq element
                              | `Optional_sequential, _
                              | _, `Optional_sequential ->
                                  TNullable (TSeq element)
                              | (`Optional | `Required),
                                (`Optional | `Required) ->
                                  TNullable (TSeq element)))
                    | _ -> merge_branch_types left right)
                  then_types else_types
              in
              if List.for_all Option.is_some merged_types then
                let merged_types = List.map Option.get merged_types in
                match
                   ( compile_tuple_branch merged_types then_form,
                     compile_tuple_branch merged_types else_form )
                 with
                | (Error _ as error), _ -> error
                | _, (Error _ as error) -> error
                | Ok then_expr, Ok else_expr -> Ok (then_expr, else_expr)
              else Ok (then_expr, else_expr)
          | TTuple expected, ty
            when (not (Types.equal (TTuple expected) ty))
                 && match else_form with FVector _ -> true | _ -> false ->
              Result.map
                (fun else_expr -> (then_expr, else_expr))
                (compile_tuple_branch expected else_form)
          | ty, TTuple expected
            when (not (Types.equal ty (TTuple expected)))
                 && match then_form with FVector _ -> true | _ -> false ->
              Result.map
                (fun then_expr -> (then_expr, else_expr))
                (compile_tuple_branch expected then_form)
          | _ -> Ok (then_expr, else_expr)
        in
        match aligned with
        | Error _ as error -> error
        | Ok (then_expr, else_expr) -> (
        match condition_expression ~env condition with
        | Error _ as err -> err
            | Ok condition_code -> (
            match merge_branch_expressions then_expr else_expr with
            | Some ((TFn _ as result_ty), _, _) -> (
                let branch_env =
                  Env.with_expected_type (Some result_ty) env
                in
                match
                  ( compile_expr scope branch_env then_form,
                    compile_expr scope branch_env else_form )
                with
                | (Error _ as error), _ -> error
                | _, (Error _ as error) -> error
                | Ok then_expr, Ok else_expr -> (
                    match
                      ( adapt_branch_expression env result_ty then_expr,
                        adapt_branch_expression env result_ty else_expr )
                    with
                    | (Error _ as error), _ -> error
                    | _, (Error _ as error) -> error
                    | Ok then_code, Ok else_code ->
                        Ok
                          (typed_ir result_ty
                             (Semantic_ir.If
                                (condition_code, then_code, else_code)))))
            | Some (result_ty, _, _)
              when requires_branch_adaptation result_ty -> (
                match
                  ( adapt_branch_expression env result_ty then_expr,
                    adapt_branch_expression env result_ty else_expr )
                with
                | (Error _ as error), _ -> error
                | _, (Error _ as error) -> error
                | Ok then_code, Ok else_code ->
                    Ok
                      (typed_ir result_ty
                         (Semantic_ir.If
                            (condition_code, then_code, else_code))))
            | Some (result_ty, then_code, else_code) ->
              Ok
                (typed_ir result_ty
                   (Semantic_ir.If
                            (condition_code, then_code, else_code)))
            | None -> (
                let has_concrete_sequence_representation = function
                  | TSeq _ | TList _ | TVector _ | TSet _ | TArray _ -> true
                  | ty -> Option.is_some (Types.next_seq_element ty)
                in
                match
                  ( Collection_capability.to_seq_expr env then_expr,
                    Collection_capability.to_seq_expr env else_expr )
                with
                | ( Ok (then_inner, then_sequence),
                    Ok (else_inner, else_sequence) )
                  when
                    (Types.equal then_inner else_inner
                    || Types.equal then_inner TUnknown
                    || Types.equal else_inner TUnknown)
                    && (has_concrete_sequence_representation then_expr.ty
                       || has_concrete_sequence_representation else_expr.ty) ->
                    let inner =
                      if Types.equal then_inner TUnknown then else_inner
                      else then_inner
                    in
                    Ok
                      (typed_ir (TSeq inner)
                         (Semantic_ir.If
                            (condition_code, then_sequence, else_sequence)))
                | _ -> (
                    match (then_expr.ty, else_expr.ty) with
                    | TNamed_record _, _ | _, TNamed_record _ ->
                        Error.error
                          ("conditional branches have incompatible nominal record types ("
                          ^ Types.source_name then_expr.ty ^ " and "
                          ^ Types.source_name else_expr.ty
                          ^ "); define a closed sum type containing every branch type")
                    | _ ->
                        let describe_type = function
                          | TRecord fields ->
                              "map {"
                              ^ String.concat ", "
                                  (List.map
                                     (fun (field : field) -> field.keyword)
                                     fields)
                              ^ "}"
                          | TNamed_record record -> record.type_name
                          | ty -> Types.source_name ty
                        in
                        Error.error
                          ("conditional branches have incompatible types: "
                          ^ describe_type then_expr.ty ^ " and "
                          ^ describe_type else_expr.ty
                          ^ "; define a closed sum type containing every branch type"))
                )
            )
        )
        ))))
  and compile_logical scope env operator forms =
    let literal_truthiness = function
      | FSymbol "nil" | FBool false -> Some false
      | FBool true | FInt _ | FFloat _ | FDecimal _ | FChar _ | FString _
      | FRegex _
      | FKeyword _ ->
          Some true
      | FSymbol _ | FCoreSymbol _ | FList _ | FVector _ | FMap _ -> None
    in
    let rec prune_static_forms = function
      | [] -> []
      | [ form ] -> [ form ]
      | form :: rest -> (
          match (operator, literal_truthiness form) with
          | `And, Some true | `Or, Some false -> prune_static_forms rest
          | `And, Some false | `Or, Some true -> [ form ]
          | (`And | `Or), None -> form :: prune_static_forms rest)
    in
    let forms = prune_static_forms forms in
    let forms =
      match operator with
      | `Or ->
          let rec narrow_later conditions = function
            | [] -> []
            | form :: rest ->
                let narrowed =
                  List.fold_left
                    (fun body condition ->
                      narrow_false_nil_predicates scope env condition body
                      |> narrow_false_instance_predicates scope env condition
                      |> narrow_false_fn_predicates scope env condition
                      |> narrow_false_scalar_predicates scope env condition)
                    form conditions
                in
                narrowed :: narrow_later (form :: conditions) rest
          in
          narrow_later [] forms
      | `And ->
          let rec narrow_later conditions = function
            | [] -> []
            | form :: rest ->
                let narrowed =
                  match conditions with
                  | [] -> form
                  | _ ->
                      narrow_type_predicates scope env
                        (FList
                           (FSymbol "__lg_logical-and" :: List.rev conditions))
                        form
                in
                narrowed :: narrow_later (form :: conditions) rest
          in
          narrow_later [] forms
    in
    match forms with
    | [] -> (
        match operator with
        | `And -> Ok (typed_ir TBool (Semantic_ir.Bool true))
        | `Or ->
            Ok
              (typed_ir
                 (TOcaml_app ("option", [ TUnknown ]))
                 (Semantic_ir.Constructor ("None", None))))
    | _ -> (
        let operand_env = Env.with_expected_type None env in
        match compile_args_for scope operand_env forms with
        | Error _ as err -> err
        | Ok expressions -> (
            let contextual_expressions =
              match Env.expected_type env with
              | None -> Ok expressions
              | Some expected ->
                  let rec inject injected = function
                    | [] -> Ok (List.rev injected)
                    | expression :: rest -> (
                        match
                          inject_contextual_closed_sum env ~expected expression
                        with
                        | Some (Ok expression) ->
                            inject (expression :: injected) rest
                        | Some (Error _ as error) -> error
                        | None -> Ok expressions)
                  in
                  inject [] expressions
            in
            match contextual_expressions with
            | Error _ as error -> error
            | Ok expressions ->
            if
              match Env.expected_type env with
              | Some ty -> Types.equal ty TBool
              | None -> false
            then
              let identity, operator_name =
                match operator with
                | `And -> (true, "&&")
                | `Or -> (false, "||")
              in
              Ok
                (typed_ir TBool
                   (List.fold_left
                      (fun result expression ->
                        Semantic_ir.Infix
                          ( operator_name,
                            result,
                            truthiness_expression ~env expression.ty
                              expression.semantic_expr ))
                      (Semantic_ir.Bool identity) expressions))
            else
            let payload_is_always_truthy ty =
              (not (Types.is_dynamic ty))
              && Option.is_none (Types.truthy_constraint_info ty)
              &&
              match ty with
              | TBool | TNil | TNullable _ | TOcaml_app ("option", [ _ ])
              | TSeq _ ->
                  false
              | TOcaml_app (name, [ _ ])
                when Types.is_next_seq_type_name name ->
                  false
              | _ -> true
            in
            let lower result_ty expressions =
              let rec lower_expressions = function
                | [] -> assert false
                | [ expression ] ->
                    coerce_expression_to_type result_ty expression.ty
                      expression.semantic_expr
                | expression :: rest ->
                    let value_name = "logical_value" in
                    let direct_constrained_identifier =
                      match
                        ( Semantic_ir.unlocated expression.semantic_expr,
                          Types.truthy_constraint_info expression.ty )
                      with
                      | Semantic_ir.Ident _, Some _ -> true
                      | _ -> false
                    in
                    let raw_value =
                      if direct_constrained_identifier then
                        expression.semantic_expr
                      else Semantic_ir.Ident value_name
                    in
                    let condition =
                      truthiness_expression ~env
                        ~constrained_identifier:direct_constrained_identifier
                        expression.ty raw_value
                    in
                    let value_ty, value_expression =
                      let value_ty =
                        Types.constraint_value_type expression.ty
                      in
                      if
                        (not direct_constrained_identifier)
                        && not (Types.equal value_ty expression.ty)
                      then
                        ( value_ty,
                          coerce_expression_to_type ~stored:true value_ty
                            expression.ty raw_value )
                      else (expression.ty, raw_value)
                    in
                    let next = lower_expressions rest in
                    let value =
                      match (operator, expression.ty) with
                      | `Or, TNil -> next
                      | `Or, (TNullable payload_ty
                             | TOcaml_app ("option", [ payload_ty ])) ->
                          coerce_expression_to_type result_ty payload_ty
                            (Semantic_ir.Apply
                               ( Semantic_ir.Ident "Option.get",
                                 [ raw_value ] ))
                      | ( `And,
                          (TNullable payload_ty
                          | TOcaml_app ("option", [ payload_ty ])) )
                        when (match expression.ty with
                             | TOcaml_app ("option", [ _ ]) -> true
                             | _ -> payload_is_always_truthy payload_ty)
                             && Option.is_some (optional_payload result_ty) ->
                          Semantic_ir.Constructor ("None", None)
                      | `And, ty
                        when (match Types.truthy_constraint_info ty with
                             | Some
                                 (TNullable payload_ty
                                 | TOcaml_app ("option", [ payload_ty ])) ->
                                 payload_is_always_truthy payload_ty
                                 && Option.is_some
                                      (optional_payload result_ty)
                             | _ -> false) ->
                          Semantic_ir.Constructor ("None", None)
                      | _ ->
                          coerce_expression_to_type result_ty value_ty
                            value_expression
                    in
                    let result =
                      match operator with
                      | `And -> Semantic_ir.If (condition, next, value)
                      | `Or -> Semantic_ir.If (condition, value, next)
                    in
                    if direct_constrained_identifier then result
                    else
                      Semantic_ir.Let
                        ( [
                            ( Semantic_ir.PVar value_name,
                              expression.semantic_expr );
                          ],
                          result )
              in
              Ok (typed_ir result_ty (lower_expressions expressions))
            in
            let result_ty =
              let last_index = List.length expressions - 1 in
              expressions
              |> List.mapi (fun index expression ->
                     let last = index = last_index in
                     let value_ty =
                       match (operator, last, expression.ty) with
                       | ( `And,
                           false,
                           (TNullable payload_ty
                           | TOcaml_app ("option", [ payload_ty ])) )
                         when payload_is_always_truthy payload_ty ->
                           TNil
                       | `And, false, ty -> (
                           match Types.truthy_constraint_info ty with
                           | Some
                               (TNullable payload_ty
                               | TOcaml_app ("option", [ payload_ty ]))
                             when payload_is_always_truthy payload_ty ->
                               TNil
                           | Some _ | None -> expression.ty)
                       | ( `Or,
                           false,
                           (TNullable payload_ty
                           | TOcaml_app ("option", [ payload_ty ])) ) ->
                           payload_ty
                       | _ -> expression.ty
                     in
                     Some (Types.constraint_value_type value_ty))
              |> List.filter_map Fun.id
              |> function
              | [] -> None
              | first :: rest ->
                  List.fold_left
                    (fun merged ty ->
                      Option.bind merged (fun merged ->
                          merge_branch_types merged ty))
                    (Some first) rest
            in
            match result_ty with
            | Some result_ty ->
                lower result_ty expressions
            | None -> (
                let optional_payloads =
                  List.filter_map
                    (fun expression -> optional_payload expression.ty)
                    expressions
                in
                let distinct_payloads =
                  List.fold_left
                    (fun payloads payload ->
                      if List.exists (Types.equal payload) payloads then payloads
                      else payload :: payloads)
                    [] optional_payloads
                in
                match
                  ( List.length optional_payloads = List.length expressions,
                    distinct_payloads,
                    Env.closed_sum_candidates_for_payloads distinct_payloads env )
                with
                | true, _ :: _ :: _, [ sum_ty ] ->
                    let result_ty = TOcaml_app ("option", [ sum_ty ]) in
                    let rec adapt adapted = function
                      | [] -> lower result_ty (List.rev adapted)
                      | expression :: rest ->
                          Result.bind
                            (adapt_branch_expression env result_ty expression)
                            (fun semantic_expr ->
                              adapt
                                ({ expression with ty = result_ty; semantic_expr }
                                :: adapted)
                                rest)
                    in
                    adapt [] expressions
                | _ ->
                    Error.error
                      ("conditional branches have incompatible types: "
                      ^ String.concat ", "
                          (List.map
                             (fun expression -> Types.source_name expression.ty)
                             expressions)
                      ^ "; define a closed sum type containing every branch type")))
                )
  and compile_match scope env target_form clauses =
    let rec parse_pairs acc = function
      | [] -> Ok (List.rev acc)
      | [ _ ] -> Error.error "match requires pattern/result pairs"
      | pattern :: result :: rest -> parse_pairs ((pattern, result) :: acc) rest
    in
    let literal_pattern expected_ty form =
      match compile_expr scope env form with
      | Error _ as err -> err
      | Ok pattern ->
          if Types.equal expected_ty pattern.ty then
            match form with
            | FInt value -> Ok (Semantic_ir.PInt value)
            | FString value | FKeyword value -> Ok (Semantic_ir.PString value)
            | FBool value -> Ok (Semantic_ir.PBool value)
            | _ -> Error.error "unsupported match pattern"
          else Error.error "match pattern type must match target"
    in
    let rec compile_pattern target_ty pattern =
      let result =
        match (target_ty, pattern) with
      | _, FSymbol "_" -> Ok (Semantic_ir.PAny, [])
        | ( target_ty,
            FList [ FSymbol "as"; inner_pattern; (FSymbol alias as alias_form) ]
          ) -> (
          match compile_pattern target_ty inner_pattern with
          | Error _ as err -> err
          | Ok (inner_pattern, bindings) ->
              let ocaml_name = Names.sanitize_name alias in
              let binding =
                ( Names.scoped_key scope alias,
                  Types.binding ocaml_name target_ty )
              in
              Ok
                ( located_form_pattern alias_form
                    (Semantic_ir.PAlias (inner_pattern, ocaml_name)),
                  bindings @ [ binding ] ))
      | target_ty, FList [ FSymbol "or"; left_form; right_form ] -> (
          match
              ( compile_pattern target_ty left_form,
                compile_pattern target_ty right_form )
          with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok (left, left_bindings), Ok (right, right_bindings) ->
              let binding_names bindings =
                bindings |> List.map fst |> List.sort_uniq String.compare
              in
                if binding_names left_bindings <> binding_names right_bindings
                then
                Error.error "or-pattern alternatives must bind the same names"
              else Ok (Semantic_ir.POr (left, right), left_bindings))
        | ( (TRecord fields | TNamed_record { fields; _ }),
            FList (FSymbol "record" :: field_patterns) ) ->
          let rec compile_fields compiled bindings seen = function
            | [] -> Ok (Semantic_ir.PRecord (List.rev compiled), bindings)
              | FList [ FSymbol field_name; field_pattern ] :: rest -> (
                let ocaml_name = Names.sanitize_name field_name in
                if List.mem ocaml_name seen then
                  Error.error ("duplicate record pattern field " ^ field_name)
                  else
                  match
                    List.find_opt
                      (fun (field : field) -> field.ocaml_name = ocaml_name)
                      fields
                  with
                    | None ->
                        Error.error
                          ("unknown record pattern field " ^ field_name)
                  | Some field -> (
                      match compile_pattern field.ty field_pattern with
                      | Error _ as err -> err
                      | Ok (pattern, field_bindings) ->
                          compile_fields
                            ((field.ocaml_name, pattern) :: compiled)
                              (bindings @ field_bindings)
                              (ocaml_name :: seen) rest))
            | _ -> Error.error "record pattern fields must be (name pattern)"
          in
          compile_fields [] [] [] field_patterns
      | _, FList (FSymbol "record" :: _) ->
          Error.error "record pattern expects a record target"
        | TTuple payload_tys, FList (FSymbol "tuple" :: payload_patterns) ->
          let rec compile_payloads patterns bindings = function
            | [], [] -> Ok (List.rev patterns, bindings)
            | payload_ty :: payload_tys, pattern :: payload_patterns -> (
                match
                  compile_pattern
                    (lg_metadata_type_for_ocaml_type payload_ty)
                    pattern
                with
                | Error _ as err -> err
                | Ok (pattern, pattern_bindings) ->
                    compile_payloads (pattern :: patterns)
                      (bindings @ pattern_bindings)
                      (payload_tys, payload_patterns))
            | _ -> Error.error "tuple pattern arity mismatch"
          in
          compile_payloads [] [] (payload_tys, payload_patterns)
            |> Result.map (fun (patterns, bindings) ->
                (Semantic_ir.PTuple patterns, bindings))
      | target_ty, FSymbol name
        when is_ocaml_constructor_pattern_target target_ty name
             && is_constructor_name name ->
          Ok
            ( Semantic_ir.PConstructor
                (resolve_ocaml_constructor_target scope env name, None),
              [] )
      | target_ty, FList (FSymbol name :: payload_patterns)
        when is_ocaml_constructor_pattern_target target_ty name
             && is_constructor_name name -> (
          let builtin_constructor_payloads =
            ocaml_builtin_constructor_payloads target_ty name
          in
          let compile_constructor_payloads constructor_name payload_tys =
            let rec compile_payloads patterns bindings = function
              | [], [] -> Ok (List.rev patterns, bindings)
              | payload_ty :: payload_tys, pattern :: payload_patterns -> (
                  match compile_pattern payload_ty pattern with
                  | Error _ as err -> err
                  | Ok (pattern, pattern_bindings) ->
                      compile_payloads (pattern :: patterns)
                        (bindings @ pattern_bindings)
                        (payload_tys, payload_patterns))
              | _ -> Error.error "constructor pattern arity mismatch"
            in
            compile_payloads [] [] (payload_tys, payload_patterns)
            |> Result.map (fun (patterns, bindings) ->
                   let payload_pattern =
                     match patterns with
                     | [] -> None
                     | [ pattern ] -> Some pattern
                     | _ -> Some (Semantic_ir.PTuple patterns)
                   in
                   ( Semantic_ir.PConstructor
                       (constructor_name, payload_pattern),
                     bindings ))
          in
          match builtin_constructor_payloads with
          | Some payload_tys ->
              compile_constructor_payloads
                (resolve_ocaml_constructor_target scope env name)
                payload_tys
          | None -> (
              match lookup_binding scope env name with
              | Error _ -> (
                  match
                    Signature_overlay.find_value name (Env.signatures env)
                  with
                  | Some (TFn (payload_tys, return_ty))
                    when List.length payload_tys
                         = List.length payload_patterns -> (
                      let constructor_ty = TFn (payload_tys, return_ty) in
                      match
                        Types.instantiate_type ~templates:[ return_ty ]
                          ~actuals:[ target_ty ] constructor_ty
                      with
                      | TFn (payload_tys, _) ->
                          compile_constructor_payloads
                            (resolve_ocaml_constructor_target scope env name)
                            payload_tys
                      | _ -> assert false)
                  | Some (TFn _) ->
                      Error.error "constructor pattern arity mismatch"
                  | Some _ -> Error.error (name ^ " is not a constructor")
                  | None ->
                      let opaque_payload_tys =
                        List.map (fun _ -> TUnknown) payload_patterns
                      in
                      compile_constructor_payloads
                        (resolve_ocaml_constructor_target scope env name)
                        opaque_payload_tys)
              | Ok constructor -> (
                  match constructor.ty with
                  | TFn (payload_tys, return_ty)
                      when List.length payload_tys
                           = List.length payload_patterns -> (
                      let instantiated =
                        Types.instantiate_type ~templates:[ return_ty ]
                          ~actuals:[ target_ty ] constructor.ty
                      in
                        match instantiated with
                      | TFn (payload_tys, _) ->
                          compile_constructor_payloads constructor.ocaml_name
                            payload_tys
                      | _ -> Error.error (name ^ " is not a constructor"))
                  | TFn _ -> Error.error "constructor pattern arity mismatch"
                  | _ -> Error.error (name ^ " is not a constructor"))))
      | _, FSymbol name ->
          let ocaml_name = Names.sanitize_name name in
          Ok
            ( Semantic_ir.PVar ocaml_name,
                [
                  ( Names.scoped_key scope name,
                    Types.binding ocaml_name target_ty );
                ] )
      | TInt, FInt value -> Ok (Semantic_ir.PInt value, [])
      | TString, FString value -> Ok (Semantic_ir.PString value, [])
      | TKeyword, FKeyword keyword -> Ok (Semantic_ir.PString keyword, [])
      | TBool, FBool value -> Ok (Semantic_ir.PBool value, [])
      | TList inner, FVector patterns ->
          compile_list_like_pattern inner patterns
      | TVector inner, FVector patterns ->
          compile_list_like_pattern inner patterns
      | _ -> (
          match pattern with
          | FInt _ | FString _ | FKeyword _ | FBool _ ->
                literal_pattern target_ty pattern
                |> Result.map (fun code -> (code, []))
          | FVector _ ->
                Error.error
                  "match collection pattern must match target collection"
          | _ ->
              Error.error
                ("unsupported match pattern "
               ^ Macro_expander.string_of_form pattern))
      in
      Result.map
        (fun (compiled, bindings) ->
          (located_form_pattern pattern compiled, bindings))
        result
    and compile_list_like_pattern inner patterns =
      let rec loop compiled_patterns bindings = function
        | [] -> Ok (List.rev compiled_patterns, bindings)
        | pattern :: rest -> (
            match compile_pattern inner pattern with
            | Error _ as err -> err
            | Ok (compiled_pattern, pattern_bindings) ->
                loop
                  (compiled_pattern :: compiled_patterns)
                  (bindings @ pattern_bindings)
                  rest)
      in
      loop [] [] patterns
      |> Result.map (fun (patterns, bindings) ->
          (Semantic_ir.PList patterns, bindings))
    in
    let compile_clause target_ty (pattern_form, result_form) =
      let pattern_form, guard_form =
        match pattern_form with
        | FList [ FSymbol "when"; pattern_form; guard_form ] ->
            (pattern_form, Some guard_form)
        | pattern_form -> (pattern_form, None)
      in
      match compile_pattern target_ty pattern_form with
      | Error _ as err -> err
      | Ok (pattern_code, bindings) -> (
          let clause_env = Env.add_bindings bindings env in
          let guard =
            match guard_form with
            | None -> Ok None
            | Some guard_form -> (
                match compile_expr scope clause_env guard_form with
                | Error _ as err -> err
                | Ok guard when Types.equal guard.ty TBool ->
                    Ok (Some guard.semantic_expr)
                | Ok _ -> Error.error "match guard must be bool")
          in
          match (guard, compile_expr scope clause_env result_form) with
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err
          | Ok guard, Ok result -> Ok (pattern_code, guard, result))
    in
    match (compile_expr scope env target_form, parse_pairs [] clauses) with
    | (Error _ as err), _ -> err
    | _, (Error _ as err) -> err
    | Ok target, Ok pairs -> (
        let target_expr =
          match target.ty with
          | TVector _ ->
              Semantic_ir.Apply
                (Semantic_ir.Ident "Rrbvec.to_list", [ target.semantic_expr ])
          | _ -> target.semantic_expr
        in
        let rec compile_clauses acc = function
          | [] -> Ok (List.rev acc)
          | pair :: rest -> (
              match compile_clause target.ty pair with
              | Error _ as err -> err
              | Ok clause -> compile_clauses (clause :: acc) rest)
        in
        match compile_clauses [] pairs with
        | Error _ as err -> err
        | Ok [] -> Error.error "match requires pattern/result pairs"
        | Ok ((_, _, first_result) :: rest as clauses) -> (
            let result_ty =
              List.fold_left
                (fun merged (_, _, result) ->
                  Option.bind merged (fun ty -> merge_branch_types ty result.ty))
                (Some first_result.ty) rest
            in
            let result_ty =
              let same_string_representation = function
                | TString | TSymbol | TKeyword -> true
                | _ -> false
              in
              match (result_ty, Env.expected_type env) with
              | None, Some expected
                when Option.is_some (Types.printable_constraint_info expected)
                     && List.for_all
                          (fun (_, _, result) ->
                            same_string_representation result.ty)
                          clauses ->
                  Some expected
              | result_ty, _ -> result_ty
            in
            match result_ty with
            | Some result_ty when not (Types.contains_dynamic result_ty) ->
                let rec adapt_clauses adapted = function
                  | [] -> Ok (List.rev adapted)
                  | (pattern, guard, result) :: rest ->
                      Result.bind
                        (adapt_branch_expression env result_ty result)
                        (fun result ->
                          adapt_clauses
                            ((pattern, guard, result) :: adapted)
                            rest)
                in
                Result.map
                  (fun clauses ->
                    typed_ir result_ty
                      (Semantic_ir.Match_guarded (target_expr, clauses)))
                  (adapt_clauses [] clauses)
            | Some _ | None ->
                Error.error
                  "conditional branches have incompatible types; define a closed sum type containing every branch type"))
  and compile_body scope env empty_error forms =
    match forms with
    | [] -> Error.error empty_error
    | [ form ] -> compile_expr scope env form
    | form :: rest -> (
        match
          ( compile_expr scope (Env.with_expected_type None env) form,
            compile_body scope env empty_error rest )
        with
        | (Error _ as err), _ -> err
        | _, (Error _ as err) -> err
        | Ok expr, Ok body ->
            Ok
              (typed_ir body.ty
                 (Semantic_ir.Sequence
                    [ expr.semantic_expr; body.semantic_expr ])))
  and compile_try scope env forms =
    let is_catch_clause = function
      | FList (FSymbol "catch" :: _) -> true
      | _ -> false
    in
    let is_finally_clause = function
      | FList (FSymbol "finally" :: _) -> true
      | _ -> false
    in
    let rec split_body acc = function
      | [] -> Error.error "try requires at least one catch or finally clause"
      | form :: rest
        when is_catch_clause form || is_finally_clause form ->
          Ok (List.rev acc, form :: rest)
      | form :: rest -> split_body (form :: acc) rest
    in
    let split_finally handler_forms =
      match List.rev handler_forms with
      | FList (FSymbol "finally" :: finally_forms) :: reversed_catches -> (
          match finally_forms with
          | [] -> Error.error "finally requires a body"
          | _ -> Ok (List.rev reversed_catches, Some finally_forms))
      | _ -> Ok (handler_forms, None)
    in
    let parse_catch = function
      | FList
          (FSymbol "catch"
          :: FSymbol exception_type
          :: FSymbol binding
          :: body_forms) -> (
          match body_forms with
          | [] -> Error.error "catch requires a type, binding, and body"
          | _
            when exception_type = "ClassCastException"
                 || String.starts_with ~prefix:"java." exception_type
                 || String.starts_with ~prefix:"javax." exception_type
                 || String.starts_with ~prefix:"clojure.lang." exception_type ->
              Error.error
                "Java interop is not supported; use static LG types and functions"
          | _ ->
              let pattern =
                if exception_type = "js/Error" then
                  FList [ FSymbol "as"; FSymbol "_"; FSymbol binding ]
                else
                  FList
                    [
                      FSymbol "as";
                      FList [ FSymbol exception_type; FSymbol "_" ];
                      FSymbol binding;
                    ]
              in
              let body =
                match body_forms with
                | [ body ] -> body
                | body_forms -> FList (FSymbol "do" :: body_forms)
              in
              Ok (pattern, body))
      | FList (FSymbol "catch" :: pattern :: body_forms) -> (
          match body_forms with
          | [] -> Error.error "catch requires a pattern and body"
          | [ body ] -> Ok (pattern, body)
          | body_forms -> Ok (pattern, FList (FSymbol "do" :: body_forms)))
      | FList [ FSymbol "catch" ] ->
          Error.error "catch requires a pattern and body"
      | _ -> Error.error "try handlers must be catch clauses"
    in
    let rec parse_catches acc = function
      | [] -> Ok (List.rev acc)
      | form :: rest -> (
          match parse_catch form with
          | Error _ as err -> err
          | Ok clause -> parse_catches (clause :: acc) rest)
    in
    let never_returns expression =
      if Semantic_ir.never_returns expression then true
      else
        match Semantic_ir.unlocated expression with
        | Semantic_ir.Apply (fn, _) -> (
            match Semantic_ir.unlocated fn with
            | Semantic_ir.Ident name ->
            Env.find_map
              (fun _ (binding : binding) ->
                if
                  binding.never_returns
                  && String.equal binding.ocaml_name name
                then Some ()
                else None)
              env
            |> Option.is_some
            | _ -> false)
        | _ -> false
    in
    let compatible_try_type body handlers =
      let body_ty = body.ty in
      let handlers_ty = handlers.ty in
      match (body_ty, handlers_ty) with
      | body_ty, handlers_ty
        when Types.contains_dynamic body_ty
             || Types.contains_dynamic handlers_ty ->
          Error.error
            "try branch type is dynamic; add a static type annotation or define a closed sum type containing every branch type"
      | (TUnknown | TMeta _ | TVar _), _
        when never_returns body.semantic_expr ->
          Ok handlers_ty
      | (TUnknown | TMeta _ | TVar _), _ | _, (TUnknown | TMeta _ | TVar _) ->
          Error.error
            "try branch type is unresolved; add a static type annotation or define a closed sum type containing every branch type"
      | _ -> (
          match merge_branch_types body_ty handlers_ty with
          | Some ty -> Ok ty
          | None ->
              Error.error
                ("try body and handlers have incompatible types: "
                ^ Types.source_name body_ty ^ " and "
                ^ Types.source_name handlers_ty
                ^ "; define a closed sum type containing every branch type"))
    in
    let compile_catches body catches =
      match catches with
      | [] -> Ok body
      | _ -> (
          let exception_name = "__lg_caught_exception" in
          let exception_binding =
            ( Names.scoped_key scope exception_name,
              Types.binding exception_name (TOcaml "exn") )
          in
          match
            compile_match scope
              (Env.add (fst exception_binding) (snd exception_binding) env)
              (FSymbol exception_name)
              (List.concat_map
                 (fun (pattern, handler) -> [ pattern; handler ])
                 catches)
          with
          | Error _ as err -> err
          | Ok handlers -> (
              match
                ( Semantic_ir.unlocated handlers.semantic_expr,
                  compatible_try_type body handlers )
              with
              | _, (Error _ as err) -> err
              | Semantic_ir.Match_guarded (_, cases), Ok ty ->
                  let body_expression =
                    coerce_expression_to_type ty body.ty body.semantic_expr
                  in
                  let cases =
                    List.map
                      (fun (pattern, guard, expression) ->
                        ( pattern,
                          guard,
                          coerce_expression_to_type ty handlers.ty expression
                        ))
                      cases
                  in
                  Ok (typed_ir ty (Semantic_ir.Try (body_expression, cases)))
              | _, Ok _ ->
                  Error.error "internal error: malformed try handlers"))
    in
    let protect_with_finally body finally =
      typed_ir body.ty
        (Semantic_ir.Labelled_apply
           ( Semantic_ir.Ident "Fun.protect",
             [
               ( Some "finally",
                 Semantic_ir.Fun
                   ( [ Semantic_ir.PUnit ],
                     Semantic_ir.Sequence
                       [ finally.semantic_expr; Semantic_ir.Unit ] ) );
               ( None,
                 Semantic_ir.Fun
                   ([ Semantic_ir.PUnit ], body.semantic_expr) );
             ] ))
    in
    match split_body [] forms with
    | Error _ as err -> err
    | Ok ([], _) -> Error.error "try requires a body"
    | Ok (body_forms, handler_forms) -> (
        match split_finally handler_forms with
        | Error _ as err -> err
        | Ok (catch_forms, finally_forms) -> (
            match
              ( compile_body scope env "try requires a body" body_forms,
                parse_catches [] catch_forms )
            with
            | (Error _ as err), _ -> err
            | _, (Error _ as err) -> err
            | Ok body, Ok catches -> (
                match (compile_catches body catches, finally_forms) with
                | (Error _ as err), _ -> err
                | Ok body, None -> Ok body
                | Ok body, Some finally_forms ->
                    Result.map
                      (protect_with_finally body)
                      (compile_body scope
                         (Env.with_expected_type None env)
                         "finally requires a body" finally_forms))))
  and loop_branch_type left right =
    match (left, right) with
    | TVector left, TVector right when not (Types.equal left right) ->
        heterogeneous_collection_type_error "vector" [ left; right ]
    | _ -> (
    match merge_branch_types left right with
    | Some ty -> Ok ty
    | None ->
        Error.error
          ("loop branches must have same type: " ^ Types.source_name left
         ^ " and " ^ Types.source_name right))
  and compile_recur scope env loop_name param_tys arg_forms =
    if List.length arg_forms <> List.length param_tys then
      Error.error
        ("recur expects " ^ string_of_int (List.length param_tys) ^ " arguments")
    else
      match compile_args_for scope env arg_forms with
      | Error _ as err -> err
      | Ok args ->
          let rec validate index expected actual =
            match (expected, actual) with
            | [], [] -> Ok ()
            | expected_ty :: expected, arg :: actual ->
                if
                  argument_compatible expected_ty arg.ty
                  || Types.defer_to_ocaml ~expected:expected_ty ~actual:arg.ty
                  ||
                  (Option.is_none (optional_payload expected_ty)
                  && Option.fold ~none:false
                       ~some:(argument_compatible expected_ty)
                       (optional_payload arg.ty))
                then validate (index + 1) expected actual
                else
                  Error.error
                    ("recur argument " ^ string_of_int index ^ " must be "
                   ^ Types.source_name expected_ty ^ ", got "
                   ^ Types.source_name arg.ty)
            | _ -> Error.error "internal error: recur argument validation"
          in
          let adapt_argument expected_ty (arg : typed_expr) =
            if
              Types.is_dynamic expected_ty
              || Option.is_some (Types.protocol_constraint_info expected_ty)
              || Option.is_some (Types.seqable_constraint_info expected_ty)
            then pack_constrained_value env expected_ty arg
            else
            match (expected_ty, arg.ty) with
            | expected, actual
              when Option.is_none (optional_payload expected)
                   && Option.fold ~none:false
                        ~some:(argument_compatible expected)
                        (optional_payload actual) ->
                Ok
                  (Semantic_ir.Apply
                     ( Semantic_ir.Ident "Option.get",
                       [ arg.semantic_expr ] ))
            | ( (TNullable expected_inner
                | TOcaml_app ("option", [ expected_inner ])),
                actual )
              when Types.is_dynamic actual
                   && not (Types.is_dynamic expected_inner)
                   && (match expected_inner with
                      | TUnknown | TMeta _ | TVar _ -> false
                      | _ -> true) ->
                Result.map
                  (fun payload ->
                    Semantic_ir.Constructor ("Some", Some payload))
                  (dynamic_unpack env expected_inner arg.semantic_expr)
            | ( (TNullable expected_inner
                | TOcaml_app ("option", [ expected_inner ])),
                (TNullable actual_inner
                | TOcaml_app ("option", [ actual_inner ])) )
              when Types.is_dynamic actual_inner
                   && not (Types.is_dynamic expected_inner) ->
                let payload_name = "__lg_recur_optional_payload" in
                Result.map
                  (fun payload ->
                    Semantic_ir.Match
                      ( arg.semantic_expr,
                        [
                          ( Semantic_ir.PConstructor ("None", None),
                            Semantic_ir.Constructor ("None", None) );
                          ( Semantic_ir.PConstructor
                              ("Some", Some (Semantic_ir.PVar payload_name)),
                            Semantic_ir.Constructor ("Some", Some payload) );
                        ] ))
                  (dynamic_unpack env expected_inner
                     (Semantic_ir.Ident payload_name))
            | expected, actual
              when Types.is_dynamic actual
                   && not (Types.is_dynamic expected)
                   && (match expected with
                      | TUnknown | TMeta _ | TVar _ -> false
                      | _ -> true) ->
                dynamic_unpack env expected arg.semantic_expr
            | _ ->
                Ok
                  (coerce_expression_to_type expected_ty arg.ty
                     arg.semantic_expr)
          in
          Result.bind (validate 1 param_tys args) (fun () ->
                 let rec adapt adapted expected args =
                   match (expected, args) with
                   | [], [] -> Ok (List.rev adapted)
                   | expected_ty :: expected, arg :: args ->
                       Result.bind (adapt_argument expected_ty arg)
                         (fun expression ->
                           adapt (expression :: adapted) expected args)
                   | _ -> Error.error "internal error: recur adaptation"
                 in
                 adapt [] param_tys args)
          |> Result.map (fun arguments ->
                 let return_ty =
                   match Env.find_opt loop_name env with
                   | Some { ty = TFn (_, return_ty); _ } -> return_ty
                   | _ -> TUnknown
                 in
                 typed_ir return_ty
                   (Semantic_ir.Apply
                      ( Semantic_ir.Ident loop_name,
                        arguments )))
  and compile_loop_tail scope env loop_name param_tys = function
    | FList (FSymbol "recur" :: arg_forms) ->
        compile_recur scope env loop_name param_tys arg_forms
    | FList [ FSymbol "if"; condition_form; then_form; else_form ] -> (
        let literal_truthiness =
          match condition_form with
          | FBool value -> Some value
          | FSymbol "nil" -> Some false
          | FInt _ | FFloat _ | FDecimal _ | FChar _ | FString _ | FRegex _
          | FKeyword _ ->
              Some true
          | FSymbol _ | FCoreSymbol _ | FList _ | FVector _ | FMap _ -> None
        in
        let then_form =
          narrow_type_predicates scope env condition_form then_form
        in
        let else_form =
          narrow_false_nil_predicates scope env condition_form else_form
          |> narrow_false_instance_predicates scope env condition_form
          |> narrow_false_fn_predicates scope env condition_form
          |> narrow_false_scalar_predicates scope env condition_form
        in
        match literal_truthiness with
        | Some true -> compile_loop_tail scope env loop_name param_tys then_form
        | Some false -> compile_loop_tail scope env loop_name param_tys else_form
        | None -> (
            match
              ( compile_expr scope env condition_form,
                compile_loop_tail scope env loop_name param_tys then_form,
                compile_loop_tail scope env loop_name param_tys else_form )
            with
            | (Error _ as err), _, _ -> err
            | _, (Error _ as err), _ -> err
            | _, _, (Error _ as err) -> err
            | Ok condition, Ok then_expr, Ok else_expr -> (
            match
              ( condition_expression ~env condition,
                loop_branch_type then_expr.ty else_expr.ty )
            with
            | (Error _ as err), _ -> err
            | _, (Error _ as err) -> err
            | Ok condition_code, Ok result_ty ->
                (match
                   ( adapt_branch_expression env result_ty then_expr,
                     adapt_branch_expression env result_ty else_expr )
                 with
                | (Error _ as error), _ -> error
                | _, (Error _ as error) -> error
                | Ok then_expr, Ok else_expr ->
                    Ok
                      (typed_ir result_ty
                         (Semantic_ir.If
                            (condition_code, then_expr, else_expr)))))))
    | FList
        [
          FSymbol "__lg_if-some";
          FVector
            [
              FSymbol value_name;
              FList [ FSymbol first_name; FSymbol collection_name ];
            ];
          FList (FSymbol "recur" :: recur_arguments);
          else_form;
        ]
      when has_source_name first_name "first"
           || has_source_name first_name "__lg_first" ->
        let tail_name = "__lg_seq_tail" in
        let replaced = ref false in
        let recur_arguments =
          List.map
            (function
              | FList [ FSymbol next_name; FSymbol name ]
                when (has_source_name next_name "next"
                     || has_source_name next_name "__lg_next")
                     && String.equal name collection_name ->
                  replaced := true;
                  FSymbol tail_name
              | argument -> argument)
            recur_arguments
        in
        if not !replaced then
          compile_option_match scope env (FSymbol value_name)
            (FList [ FSymbol first_name; FSymbol collection_name ])
            (fun some_env ->
              compile_loop_tail scope some_env loop_name param_tys
                (FList (FSymbol "recur" :: recur_arguments)))
            (fun () ->
              compile_loop_tail scope env loop_name param_tys else_form)
            "if-some branches have incompatible types; define a closed sum type"
        else (
          match compile_expr scope env (FSymbol collection_name) with
          | Error _ as error -> error
          | Ok collection -> (
              match Collection_capability.to_seq_expr env collection with
              | Error _ -> Error.error "first expects a seqable value"
              | Ok (element_ty, sequence) ->
                  let some_env =
                    Env.add_bindings
                      [
                        ( Names.scoped_key scope value_name,
                          Types.binding value_name element_ty );
                        ( Names.scoped_key scope tail_name,
                          Types.binding tail_name (TSeq element_ty) );
                      ]
                      env
                  in
                  match
                    ( compile_loop_tail scope some_env loop_name param_tys
                        (FList (FSymbol "recur" :: recur_arguments)),
                      compile_loop_tail scope env loop_name param_tys else_form )
                  with
                  | (Error _ as error), _ -> error
                  | _, (Error _ as error) -> error
                  | Ok some_expr, Ok none_expr -> (
                      match merge_branch_expressions some_expr none_expr with
                      | None ->
                          Error.error
                            "if-some branches have incompatible types; define a \
                             closed sum type"
                      | Some (result_ty, some_code, none_code) -> (
                          match
                            adapt_merged_branches env result_ty some_expr
                              some_code none_expr none_code
                          with
                          | Error _ as error -> error
                          | Ok (some_code, none_code) ->
                              let node =
                                match Env.target env with
                                | Target.Melange ->
                                    Semantic_ir.Apply
                                      ( Semantic_ir.Ident
                                          "Lg_runtime.Runtime_array_melange.call0",
                                        [ sequence; Semantic_ir.Int 0 ] )
                                | Target.Native | Target.Js_of_ocaml ->
                                    Semantic_ir.Apply
                                      (sequence, [ Semantic_ir.Unit ])
                              in
                              Ok
                                (typed_ir result_ty
                                   (Semantic_ir.Match
                                      ( node,
                                        [
                                          ( Semantic_ir.PConstructor
                                              ("Seq.Nil", None),
                                            none_code );
                                          ( Semantic_ir.PConstructor
                                              ( "Seq.Cons",
                                                Some
                                                  (Semantic_ir.PTuple
                                                     [
                                                       Semantic_ir.PVar
                                                         value_name;
                                                       Semantic_ir.PVar
                                                         tail_name;
                                                     ]) ),
                                            some_code );
                                        ] )))))))
    | FList
        [
          FSymbol ("__lg_if-let" as binding_form_name);
          binding_form;
          then_form;
          else_form;
        ]
    | FList
        [
          FSymbol ("__lg_if-some" as binding_form_name);
          binding_form;
          then_form;
          else_form;
        ] -> (
        let error_prefix = binding_form_name in
        match
          parse_option_binding binding_form
            (error_prefix ^ " requires [name option], then, and else")
        with
        | Error _ as error -> error
        | Ok (pattern, option_form) ->
            compile_option_match
              ~require_truthy:(binding_form_name = "__lg_if-let")
              scope env pattern option_form
              (fun some_env ->
                compile_loop_tail scope some_env loop_name param_tys then_form)
              (fun () ->
                compile_loop_tail scope env loop_name param_tys else_form)
              (error_prefix ^ " branches must have same type"))
    | FList (FSymbol "do" :: body_forms) ->
        compile_loop_tail_body scope env loop_name param_tys body_forms
    | FList (FSymbol "let" :: bindings :: body_forms) ->
        compile_let_tail scope env loop_name param_tys bindings body_forms
    | FList (FSymbol "condp" :: predicate :: target :: clauses) ->
        let target_name = loop_name ^ "__condp_target" in
        let rec expand = function
          | [] ->
              Ok
                (FList
                   [ FSymbol "throw";
                     FList
                       [ FSymbol "ex-info";
                         FString "No matching clause in condp";
                         FMap [];
                       ];
                   ])
          | [ default ] -> Ok default
          | test :: expression :: rest ->
              Result.map
                (fun otherwise ->
                  FList
                    [ FSymbol "if";
                      FList [ predicate; test; FSymbol target_name ];
                      expression;
                      otherwise;
                    ])
                (expand rest)
        in
        Result.bind (expand clauses) (fun body ->
            compile_loop_tail scope env loop_name param_tys
              (FList
                 [ FSymbol "let";
                   FVector [ FSymbol target_name; target ];
                   body;
                 ]))
    | (FList (FSymbol name :: args) as form) -> (
        match Env.find_macro ~scope name env with
        | None -> compile_expr scope env form
        | Some definition ->
            Result.bind
              (Macro_expander.expand ~scope ~compiler_env:env definition args)
              (fun expanded ->
                compile_loop_tail scope env loop_name param_tys expanded))
    | form -> compile_expr scope env form
  and compile_loop_tail_body scope env loop_name param_tys forms =
    match forms with
    | [] -> Error.error "loop body requires at least one form"
    | [ form ] -> compile_loop_tail scope env loop_name param_tys form
    | form :: rest -> (
        match
          ( compile_expr scope env form,
            compile_loop_tail_body scope env loop_name param_tys rest )
        with
        | (Error _ as err), _ -> err
        | _, (Error _ as err) -> err
        | Ok expression, Ok body ->
            Ok
              (typed_ir body.ty
                 (Semantic_ir.Sequence
                    [ expression.semantic_expr; body.semantic_expr ])))
  and compile_loop scope env bindings body_forms =
    incr loop_counter;
    let loop_name = "__lg_loop_" ^ string_of_int !loop_counter in
    match bindings with
    | FVector forms -> (
        let forms = Destructure.normalize_binding_type_hints forms in
        if List.length forms mod 2 <> 0 then
          Error.error "loop bindings require an even number of forms"
        else
          let rec compile_bindings names identities value_forms values tys =
            function
            | [] ->
                Ok
                  ( List.rev names,
                    List.rev identities,
                    List.rev value_forms,
                    List.rev values,
                    List.rev tys )
            | (FSymbol name as name_form) :: value_form :: rest -> (
                if name = "_" || List.mem name names then
                  Error.error "loop binding names must be unique symbols"
                else
                  match compile_expr scope env value_form with
                  | Error _ as err -> err
                  | Ok value ->
                      let binding_ty =
                        match value_form with
                        | FMap [] ->
                            Types.dynamic_map (Type_solver.fresh ())
                              (Type_solver.fresh ())
                        | _ ->
                            if Types.equal value.ty TNil then TNullable TUnknown
                            else value.ty
                      in
                      let value =
                        match Types.seqable_constraint_info binding_ty with
                        | None -> Ok value
                        | Some (_, element_ty, _) -> (
                            match
                              Collection_capability.to_seq_expr env value
                            with
                            | Error _ as error -> error
                            | Ok (_, sequence) ->
                                let element_ty =
                                  match element_ty with
                                  | TUnknown | TMeta _ | TVar _ ->
                                      Types.dynamic_constraint TUnknown
                                  | ty -> ty
                                in
                                Ok (typed_ir (TSeq element_ty) sequence))
                      in
                      Result.bind value (fun value ->
                          let binding_ty =
                            match Types.seqable_constraint_info binding_ty with
                            | Some _ -> value.ty
                            | None -> binding_ty
                          in
                          compile_bindings (name :: names)
                            (Destructure.source_identity name_form :: identities)
                            (value_form :: value_forms) (value :: values)
                            (binding_ty :: tys) rest))
            | _ -> Error.error "loop binding names must be symbols"
          in
          match compile_bindings [] [] [] [] [] forms with
          | Error _ as err -> err
          | Ok (names, identities, value_forms, values, param_tys) -> (
              let inferred_param_tys =
                let local_tys = List.combine names param_tys in
                let rec form_type aliases = function
                  | FSymbol name -> (
                      match List.assoc_opt name aliases with
                      | Some ty -> ty
                      | None -> (
                          match List.assoc_opt name local_tys with
                          | Some ty -> ty
                          | None -> (
                              match Resolver.lookup_binding scope env name with
                              | Ok (binding : Types.binding) -> binding.ty
                              | Error _ -> TUnknown)))
                  | FList
                      [ FSymbol name; reducer; init; collection ]
                    when name = "reduce"
                         || String.ends_with ~suffix:"/reduce" name ->
                      let init_ty = form_type aliases init in
                      let conj_reducer =
                        match reducer with
                        | FSymbol reducer_name ->
                            reducer_name = "__lg_conj"
                            || reducer_name = "conj-seq"
                            || String.ends_with ~suffix:"/conj-seq"
                                 reducer_name
                        | _ -> false
                      in
                      if not conj_reducer then init_ty
                      else
                        let collection_ty =
                          form_type aliases collection
                        in
                        let collection_element =
                          match collection_ty with
                          | TList inner | TVector inner | TSeq inner -> inner
                          | ty -> (
                              match Types.next_seq_element ty with
                              | Some inner -> inner
                              | None -> (
                                  match
                                    Types.seqable_constraint_element ty
                                  with
                                  | Some inner -> inner
                                  | None ->
                                      Types.dynamic_constraint TUnknown))
                        in
                        let merge_element init_element =
                          if Types.is_dynamic collection_element then
                            Types.dynamic_constraint TUnknown
                          else
                            match collection_element with
                            | TUnknown | TMeta _ | TVar _ ->
                                Types.dynamic_constraint TUnknown
                            | element -> (
                                match
                                  merge_branch_types init_element element
                                with
                                | Some merged -> merged
                                | None ->
                                    Types.dynamic_constraint TUnknown)
                        in
                        (match init_ty with
                        | TList inner -> TList (merge_element inner)
                        | TVector inner -> TVector (merge_element inner)
                        | TSeq inner -> TSeq (merge_element inner)
                        | ty -> (
                            match Types.next_seq_element ty with
                            | Some inner ->
                                Types.next_seq (merge_element inner)
                            | None -> init_ty))
                  | FList [ FSymbol name; collection ]
                    when name = "__lg_first"
                         || name = "first"
                         || String.ends_with ~suffix:"/first" name -> (
                      match form_type aliases collection with
                      | TList inner | TVector inner | TSeq inner ->
                          TNullable inner
                      | ty -> (
                          match Types.next_seq_element ty with
                          | Some inner -> TNullable inner
                          | None -> TUnknown))
                  | FList [ FSymbol name; collection ]
                    when name = "__lg_next"
                         || name = "next"
                         || String.ends_with ~suffix:"/next" name -> (
                      let collection_ty = form_type aliases collection in
                      match collection_ty with
                      | TList inner | TVector inner | TSeq inner ->
                          Types.next_seq inner
                      | ty -> (
                          match Types.next_seq_element ty with
                          | Some inner -> Types.next_seq inner
                          | None -> (
                              match Types.seqable_constraint_element ty with
                              | Some inner -> Types.next_seq inner
                                  | None -> TUnknown)))
                  | FList [ FSymbol name; collection; item ]
                    when name = "__lg_conj" ->
                      let collection_ty = form_type aliases collection in
                      let item_ty = form_type aliases item in
                      let merge inner =
                        match merge_branch_types inner item_ty with
                        | Some merged -> merged
                        | None -> Types.dynamic_constraint TUnknown
                      in
                      (match collection_ty with
                      | TList inner -> TList (merge inner)
                      | TVector inner -> TVector (merge inner)
                      | TSet inner -> TSet (merge inner)
                      | TSeq inner -> TSeq (merge inner)
                      | ty -> ty)
                  | FList [ FSymbol name; array; _index ]
                    when name = "__lg_aget" || name = "unsafe-aget"
                         || String.ends_with ~suffix:"/__lg_aget" name
                         || String.ends_with ~suffix:"/unsafe-aget" name -> (
                      match form_type aliases array with
                      | TArray inner
                      | TOcaml_app (("array" | "Array.t"), [ inner ]) ->
                          inner
                      | _ -> TUnknown)
                  | FList (FSymbol name :: arguments) -> (
                      match Resolver.lookup_binding scope env name with
                      | Ok { ty = TFn (parameter_tys, return_ty); _ }
                        when List.length parameter_tys
                             = List.length arguments ->
                          Types.instantiate_type ~templates:parameter_tys
                            ~actuals:(List.map (form_type aliases) arguments)
                            return_ty
                      | Ok { ty = TOverloaded_fn arities; _ } -> (
                          match
                            List.find_opt
                              (fun arity ->
                                List.length arity.fixed_params
                                = List.length arguments)
                              arities
                          with
                          | Some arity ->
                              Types.instantiate_type
                                ~templates:arity.fixed_params
                                ~actuals:
                                  (List.map (form_type aliases) arguments)
                                arity.return_ty
                          | None -> TUnknown)
                      | Ok _ | Error _ -> TUnknown)
                  | FList _ | FVector _ | FMap _ | FCoreSymbol _
                  | FDecimal _ -> TOcaml "Lg_runtime.Runtime_decimal.t"
                  | FKeyword _ | FString _ | FRegex _ | FInt _ | FFloat _
                  | FChar _ | FBool _ -> TUnknown
                and recur_argument_types aliases = function
                  | FList (FSymbol "recur" :: arguments) ->
                      [ List.map (form_type aliases) arguments ]
                  | FList (FSymbol ("loop" | "fn") :: _) -> []
                  | FList
                      (FSymbol ("let" | "let*") :: FVector bindings
                      :: body_forms) ->
                      let rec sequence_shape = function
                        | TList inner -> Some (inner, TList inner)
                        | TVector inner -> Some (inner, TList inner)
                        | TSeq inner -> Some (inner, TSeq inner)
                        | TNullable inner
                        | TOcaml_app ("option", [ inner ]) ->
                            sequence_shape inner
                        | ty -> (
                            match Types.next_seq_element ty with
                            | Some inner -> Some (inner, Types.next_seq inner)
                            | None -> None)
                      in
                      let rec bind_pattern aliases pattern ty =
                        match pattern with
                        | FSymbol name -> (name, ty) :: aliases
                        | FVector forms -> (
                            match
                              ( Destructure.parse_sequence_pattern forms,
                                sequence_shape ty )
                            with
                            | Ok pattern, Some (inner, rest_ty) ->
                                let aliases =
                                  List.fold_left
                                    (fun aliases item_pattern ->
                                      bind_pattern aliases item_pattern inner)
                                    aliases pattern.item_patterns
                                in
                                Option.fold ~none:aliases
                                  ~some:(fun name ->
                                    (name, TNullable rest_ty) :: aliases)
                                  pattern.rest_name
                            | _ -> aliases)
                        | FList
                            [ FSymbol "__type-hint"; FSymbol _; pattern ] ->
                            bind_pattern aliases pattern ty
                        | _ -> aliases
                      in
                      let rec bind aliases = function
                        | pattern :: value :: rest ->
                            let ty = form_type aliases value in
                            bind (bind_pattern aliases pattern ty) rest
                        | _ -> aliases
                      in
                      let aliases = bind aliases bindings in
                      List.concat_map
                        (recur_argument_types aliases)
                        body_forms
                  | FList forms | FVector forms ->
                      List.concat_map (recur_argument_types aliases) forms
                  | FMap pairs ->
                      List.concat_map
                        (fun (key, value) ->
                          recur_argument_types aliases key
                          @ recur_argument_types aliases value)
                        pairs
                  | _ -> []
                in
                let recurs =
                  List.concat_map (recur_argument_types []) body_forms
                in
                List.mapi
                  (fun index fallback ->
                    let recur_types =
                      List.filter_map (fun tys -> List.nth_opt tys index) recurs
                    in
                    let sequence_element = function
                      | TList inner | TVector inner | TSeq inner -> Some inner
                      | TOcaml_app (name, [ inner ])
                        when Types.is_next_seq_type_name name ->
                          Some inner
                      | _ -> None
                    in
                    let merge_sequence_inner current_inner actual_inner =
                      match (current_inner, actual_inner) with
                      | current, (TUnknown | TMeta _ | TVar _) -> current
                      | (TUnknown | TMeta _ | TVar _), actual -> actual
                      | current, actual -> (
                          match
                            merge_branch_types current actual
                          with
                          | Some inner -> inner
                          | None -> Types.dynamic_constraint TUnknown)
                    in
                    let widen_container current actual =
                      match (current, actual) with
                      | TList current_inner, TList actual_inner ->
                          TList
                            (merge_sequence_inner current_inner actual_inner)
                      | TVector current_inner, TVector actual_inner ->
                          TVector
                            (merge_sequence_inner current_inner actual_inner)
                      | ( (TList current_inner | TVector current_inner),
                          TSeq actual_inner )
                      | ( TSeq current_inner,
                          (TList actual_inner | TVector actual_inner) ) ->
                          let inner =
                            merge_sequence_inner current_inner actual_inner
                          in
                          TSeq inner
                      | ( (TList current_inner | TVector current_inner),
                          TOcaml_app (name, [ actual_inner ]) )
                        when Types.is_next_seq_type_name name ->
                          let inner =
                            merge_sequence_inner current_inner actual_inner
                          in
                          Types.next_seq inner
                      | ( TSeq current_inner,
                          TOcaml_app (name, [ actual_inner ]) )
                      | ( TOcaml_app (name, [ current_inner ]),
                          TSeq actual_inner )
                        when Types.is_next_seq_type_name name ->
                          Types.next_seq
                            (merge_sequence_inner current_inner actual_inner)
                      | ( TOcaml_app (current_name, [ current_inner ]),
                          TOcaml_app (actual_name, [ actual_inner ]) )
                        when Types.is_next_seq_type_name current_name
                             && Types.is_next_seq_type_name actual_name ->
                          Types.next_seq
                            (merge_sequence_inner current_inner actual_inner)
                      | TSeq current_inner, TSeq actual_inner ->
                          TSeq
                            (merge_sequence_inner current_inner actual_inner)
                      | current, actual
                        when Option.is_some (sequence_element current)
                             && (match actual with
                                | TUnknown | TMeta _ | TVar _ -> true
                                | _ -> false) ->
                          current
                      | TNamed_record current, TNamed_record actual
                        when Type_id.equal current.type_id actual.type_id
                             && List.length current.type_arguments
                                = List.length actual.type_arguments -> (
                          let current_ty = TNamed_record current in
                          let actual_ty = TNamed_record actual in
                          match
                            Type_solver.unify Type_solver.empty current_ty
                              actual_ty
                          with
                          | Ok substitutions ->
                              Type_solver.apply substitutions current_ty
                          | Error _ -> current_ty)
                      | current, actual
                        when Type_solver.is_open current
                             && Types.same_shape current actual -> (
                          match
                            Type_solver.unify Type_solver.empty current actual
                          with
                          | Ok substitutions ->
                              Type_solver.apply substitutions current
                          | Error _ -> current)
                      | current, _ -> current
                    in
                    let fallback =
                      List.fold_left widen_container fallback recur_types
                    in
                    let becomes_nullable =
                      List.exists
                        (function
                          | TNullable _ | TOcaml_app ("option", [ _ ]) -> true
                          | _ -> false)
                        recur_types
                    in
                    if
                      becomes_nullable
                      && not
                           (match fallback with
                           | TNullable _ | TOcaml_app ("option", [ _ ]) -> true
                           | _ -> false)
                    then TNullable fallback
                    else fallback)
                  param_tys
              in
              let param_tys =
                let rec contains_protocol ty =
                  Option.is_some (Types.protocol_constraint_info ty)
                  ||
                  match Types.dynamic_constraint_info ty with
                  | Some capability -> contains_protocol capability
                  | None -> (
                      match ty with
                      | TNullable inner | TArray inner | TRef inner
                      | TList inner | TVector inner | TSet inner | TSeq inner
                      | TOcaml_app (_, [ inner ]) ->
                          contains_protocol inner
                      | TOcaml_app (_, arguments) | TTuple arguments ->
                          List.exists contains_protocol arguments
                      | TConstraint constraint_ ->
                          List.exists contains_protocol
                            (constraint_children constraint_)
                      | TFn (parameters, return_ty) ->
                          List.exists contains_protocol
                            (return_ty :: parameters)
                      | TOverloaded_fn arities ->
                          List.exists
                            (fun arity ->
                              List.exists contains_protocol
                                (arity.return_ty :: arity.fixed_params))
                            arities
                      | TRecord fields | TNamed_record { fields; _ } ->
                          List.exists
                            (fun (field : field) ->
                              contains_protocol field.ty)
                            fields
                      | TInt | TFloat | TChar | TString | TRegex | TMap_keys
                      | TSymbol | TKeyword | TBool | TUnit | TNil | TUnknown
                      | TMeta _ | TVar _ | TOcaml _ ->
                          false)
                in
                let lookup_function_ty =
                  Expression_support.lookup_function_ty scope env
                in
                let lookup_protocol_constraint =
                  Protocol.constraint_type scope env
                in
                let lookup_dynamic_key_record_type =
                  Expression_support.dynamic_key_record_type env
                in
                let resolve_named_record =
                  Function_elaborator.infer_named_record scope env
                in
                let lookup_closed_sum_candidates payload_types =
                  Env.closed_sum_candidates_for_payloads payload_types env
                in
                let lookup_closed_sum_constructors ty =
                  Env.predicate_variant_constructors ty env
                in
                match
                  Type_inference.infer_params
                    ?expected_return_ty:(Env.expected_type env)
                    ~lookup_function_ty
                    ~lookup_closed_sum_candidates
                    ~lookup_closed_sum_constructors
                    ~lookup_protocol_constraint
                    ~lookup_dynamic_key_record_type ~resolve_named_record
                    (List.combine names inferred_param_tys)
                    body_forms
                with
                | Error _ -> inferred_param_tys
                | Ok inferred ->
                    List.map2
                      (fun name fallback ->
                        match List.assoc_opt name inferred with
                        | Some ty when contains_protocol ty -> ty
                        | Some ty
                          when Type_solver.is_open fallback
                               && Types.same_shape fallback ty -> (
                            match
                              Type_solver.unify Type_solver.empty fallback ty
                            with
                            | Ok substitutions ->
                                Type_solver.apply substitutions fallback
                            | Error _ -> fallback)
                        | Some _ | None -> fallback)
                      names inferred_param_tys
              in
              let loop_env =
                List.fold_left2
                  (fun env name ty ->
                    Env.add
                      (Names.scoped_key scope name)
                      (Types.binding (Names.sanitize_name name) ty)
                      env)
                  env names param_tys
              in
              let compile_body env =
                compile_loop_tail_body scope env loop_name param_tys body_forms
              in
              match compile_body loop_env with
              | Error _ as err -> err
              | Ok inferred_body ->
                  let rec stabilize_body remaining return_ty =
                    let body_env =
                      Env.add loop_name
                        (Types.binding loop_name (TFn (param_tys, return_ty)))
                        loop_env
                    in
                    match compile_body body_env with
                    | Error _ as err -> err
                    | Ok body
                      when remaining = 0 || Types.equal body.ty return_ty ->
                        Ok body
                    | Ok body ->
                        stabilize_body (remaining - 1) body.ty
                  in
                  (match stabilize_body 4 inferred_body.ty with
                  | Error _ as err -> err
                  | Ok body ->
                  let sequence_element_type = function
                    | TSeq inner -> Some inner
                    | TOcaml_app (name, [ inner ])
                      when Types.is_next_seq_type_name name ->
                        Some inner
                    | _ -> None
                  in
                  let sequence_value (value : typed_expr) =
                    match value.ty with
                    | TList inner ->
                        Some
                          ( inner,
                            Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_seq.of_list",
                                [ value.semantic_expr ] ) )
                    | TVector inner ->
                        Some
                          ( inner,
                            Semantic_ir.Apply
                              ( Semantic_ir.Ident
                                  "Lg_runtime.Runtime_seq.of_vector",
                                [ value.semantic_expr ] ) )
                    | TSeq inner -> Some (inner, value.semantic_expr)
                    | TOcaml_app (name, [ inner ])
                      when Types.is_next_seq_type_name name ->
                        Some (inner, value.semantic_expr)
                    | _ -> None
                  in
                  let adapt_initial param_ty value_form (value : typed_expr) =
                    let value =
                      match value_form with
                      | FMap []
                        when Option.is_some
                               (Types.dynamic_map_types param_ty) ->
                          compile_expr scope
                            (Env.with_expected_type (Some param_ty) env)
                            value_form
                      | _ -> Ok value
                    in
                    Result.bind value (fun value ->
                    match
                      (sequence_element_type param_ty, sequence_value value)
                    with
                    | Some target_inner, Some (source_inner, sequence)
                      when Types.equal target_inner source_inner ->
                        Ok sequence
                    | ( Some target_inner,
                        Some (source_inner, sequence) )
                      when Types.is_dynamic target_inner
                           && not (Types.is_dynamic source_inner) ->
                        let item_name = "__lg_loop_initial_item" in
                        let item =
                          typed_ir source_inner
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
                          (pack_dynamic_value env target_inner item)
                    | _ ->
                        Ok
                          (coerce_expression_to_type param_ty value.ty
                             value.semantic_expr
                          |> capability_storage_expression param_ty))
                  in
                  let rec adapt_initials adapted param_tys value_forms values =
                    match (param_tys, value_forms, values) with
                    | [], [], [] -> Ok (List.rev adapted)
                    | ( param_ty :: param_tys,
                        value_form :: value_forms,
                        value :: values ) ->
                        Result.bind
                          (adapt_initial param_ty value_form value)
                          (fun value ->
                            adapt_initials (value :: adapted) param_tys
                              value_forms values)
                    | _ -> Error.error "internal error: loop initial values"
                  in
                  Result.bind
                    (adapt_initials [] param_tys value_forms values)
                    (fun initial_values ->
                  let params =
                    List.map2
                      (fun name (identity, param_ty) ->
                        located_pattern identity
                          (capability_pattern (Names.sanitize_name name)
                             param_ty))
                      names (List.combine identities param_tys)
                  in
                  Ok
                    (typed_ir body.ty
                       (Semantic_ir.LetRec
                          ( loop_name,
                            params,
                            body.semantic_expr,
                            initial_values
                          )))))))
    | _ -> Error.error "loop bindings must be a vector"
  and compile_let_tail scope env loop_name param_tys bindings body_forms =
    compile_let_with_body
      (fun scope env forms ->
        compile_loop_tail_body scope env loop_name param_tys forms)
      scope env bindings body_forms
  and compile_let scope env bindings body_forms =
    compile_let_with_body
      (fun scope env forms ->
        compile_body scope env "let body requires at least one form" forms)
      scope env bindings body_forms
  and compile_let_with_body compile_let_body scope env bindings body_forms =
    let generalize_function_value value_form binding =
      match value_form with
      | FList (FSymbol "fn" :: _) -> Types.generalize_binding binding
      | _ -> binding
    in
    let rec remaining_binding_names names = function
      | pattern :: _value :: rest ->
          remaining_binding_names
            (List.rev_append (Destructure.pattern_names pattern) names)
            rest
      | _ -> List.rev names
    in
    let rec remaining_value_forms values = function
      | _pattern :: value :: rest ->
          remaining_value_forms (value :: values) rest
      | _ -> List.rev values
    in
    let inferred_binding_type env name value_form rest =
      let names = name :: remaining_binding_names [] rest in
      let forms = remaining_value_forms [] rest @ body_forms in
      let local_names = List.sort_uniq String.compare names in
      let rec nested_function_parameters = function
        | FList
            (FSymbol "fn" :: FVector parameters :: body_forms)
        | FList
            (FSymbol "fn" :: FSymbol _ :: FVector parameters :: body_forms) ->
            List.concat_map Destructure.pattern_names parameters
            @ List.concat_map nested_function_parameters body_forms
        | FList nested | FVector nested ->
            List.concat_map nested_function_parameters nested
        | FMap pairs ->
            List.concat_map
              (fun (key, value) ->
                nested_function_parameters key
                @ nested_function_parameters value)
              pairs
        | FSymbol _ | FCoreSymbol _ | FKeyword _ | FString _ | FRegex _
        | FInt _ | FFloat _ | FDecimal _ | FChar _ | FBool _ ->
            []
      in
      let nested_parameters = nested_function_parameters value_form in
      let captured_params =
        Dependency_graph.symbols value_form
        |> List.sort_uniq String.compare
        |> List.filter_map (fun candidate ->
               if
                 List.mem candidate local_names
                 || List.mem candidate nested_parameters
                 || (String.length candidate > 0
                    && Char.uppercase_ascii candidate.[0] = candidate.[0]
                    && Char.lowercase_ascii candidate.[0] <> candidate.[0])
               then None
               else
                 match Resolver.lookup_binding scope env candidate with
                 | Ok (binding : Types.binding) -> Some (candidate, binding.ty)
                 | Error _ -> None)
      in
      let initial_ty =
        match value_form with
        | FList
            [
              FSymbol "fn";
              FVector [ FSymbol parameter ];
              FSymbol result;
            ]
          when parameter = result ->
            let variable = Type_solver.fresh () in
            (Type_solver.generalize (TFn ([ variable ], variable))).body
        | _ -> (
            match
              Type_inference.inferred_form_type captured_params value_form
            with
            | TUnknown ->
                Type_inference.inferred_call_return_type
                  ~lookup_function_ty:
                    (Expression_support.lookup_function_ty scope env)
                  captured_params value_form
            | ty -> ty)
      in
      let params =
        local_names
        |> List.map (fun candidate ->
               (candidate, if String.equal candidate name then initial_ty else TUnknown))
        |> fun local_params -> local_params @ captured_params
      in
      let lookup_function_ty = Expression_support.lookup_function_ty scope env in
      let lookup_protocol_constraint = Protocol.constraint_type scope env in
      let lookup_dynamic_key_record_type =
        Expression_support.dynamic_key_record_type env
      in
      let resolve_named_record =
        Function_elaborator.infer_named_record scope env
      in
      let lookup_closed_sum_candidates payload_types =
        Env.closed_sum_candidates_for_payloads payload_types env
      in
      let lookup_closed_sum_constructors ty =
        Env.predicate_variant_constructors ty env
      in
      match
        Type_inference.infer_params ~lookup_function_ty
          ~lookup_closed_sum_candidates
          ~lookup_closed_sum_constructors
          ~lookup_protocol_constraint ~lookup_dynamic_key_record_type
          ~resolve_named_record params forms
      with
      | Ok inferred ->
          List.assoc_opt name inferred |> Option.value ~default:TUnknown
      | Error _ -> TUnknown
    in
    let expected_value_env env pattern value_form rest =
      let env = Env.with_expected_type None env in
      match pattern with
      | FSymbol name -> (
          let inferred = inferred_binding_type env name value_form rest in
          match inferred with
          | TUnknown | TMeta _ | TVar _ -> env
          | ty ->
              let expected_ty =
                Types.truthy_constraint_info ty |> Option.value ~default:ty
              in
              Env.with_expected_type (Some expected_ty) env)
      | (FVector _ as pattern) ->
          let names =
            Destructure.pattern_names pattern
            |> List.sort_uniq String.compare
          in
          let params = List.map (fun name -> (name, TUnknown)) names in
          let forms = remaining_value_forms [] rest @ body_forms in
          let lookup_function_ty =
            Expression_support.lookup_function_ty scope env
          in
          let lookup_protocol_constraint = Protocol.constraint_type scope env in
          let lookup_dynamic_key_record_type =
            Expression_support.dynamic_key_record_type env
          in
          let resolve_named_record =
            Function_elaborator.infer_named_record scope env
          in
          let lookup_closed_sum_candidates payload_types =
            Env.closed_sum_candidates_for_payloads payload_types env
          in
          let lookup_closed_sum_constructors ty =
            Env.predicate_variant_constructors ty env
          in
          let inferred =
            Type_inference.infer_params ~lookup_function_ty
              ~lookup_closed_sum_candidates
              ~lookup_closed_sum_constructors
              ~lookup_protocol_constraint ~lookup_dynamic_key_record_type
              ~resolve_named_record params forms
            |> Result.value ~default:params
          in
          let lookup name =
            List.assoc_opt name inferred |> Option.value ~default:TUnknown
          in
          let rec infer_static_pattern_type = function
            | FSymbol name -> Ok (lookup name)
            | FVector forms as pattern -> (
                match Destructure.parse_sequence_pattern forms with
                | Error _ as error -> error
                | Ok parsed when Option.is_some parsed.rest_name ->
                    Destructure.infer_pattern_type pattern lookup
                | Ok parsed ->
                    let item_tys =
                      List.map
                        (fun item ->
                          infer_static_pattern_type item
                          |> Result.value ~default:TUnknown)
                        parsed.item_patterns
                    in
                    (match item_tys with
                    | first :: rest
                      when List.for_all (Types.equal first) rest ->
                        Ok (TVector first)
                    | _ -> Ok (TTuple item_tys)))
            | pattern -> Destructure.infer_pattern_type pattern lookup
          in
          (match infer_static_pattern_type pattern with
          | Ok ty -> Env.with_expected_type (Some ty) env
          | Error _ -> env)
      | _ -> env
    in
    match bindings with
    | FVector forms ->
        let forms = Destructure.normalize_binding_type_hints forms in
        if List.length forms mod 2 <> 0 then
          Error.error "let bindings require an even number of forms"
        else
          let rec bind env ir_bindings = function
            | [] -> (
                match compile_let_body scope env body_forms with
                | Error _ as err -> err
                | Ok body ->
                    Ok
                      {
                        (typed_ir body.ty
                           (Semantic_ir.Let
                              (List.rev ir_bindings, body.semantic_expr)))
                        with
                        return_param_index = body.return_param_index;
                      })
            | pattern :: value_form :: rest -> (
                let value_env =
                  expected_value_env env pattern value_form rest
                in
                let value = compile_expr scope value_env value_form in
                match value with
                | Error _ as err -> err
                | Ok value -> (
                    let destructured_value, value_binding =
                      match pattern with
                      | FSymbol _ -> (value, None)
                      | _ ->
                          incr destructuring_value_counter;
                          let value_name =
                            "__lg_destructuring_value_"
                            ^ string_of_int !destructuring_value_counter
                          in
                          let value_pattern, value_expression =
                            if
                              Option.is_some
                                (Types.protocol_constraint_info value.ty)
                              || Option.is_some
                                   (Types.seqable_constraint_element value.ty)
                            then
                              ( capability_pattern value_name value.ty,
                                capability_storage_expression value.ty
                                  value.semantic_expr )
                            else
                              ( Semantic_ir.PVar value_name,
                                value.semantic_expr )
                          in
                          ( { value with
                              semantic_expr = Semantic_ir.Ident value_name;
                            },
                            Some (value_pattern, value_expression) )
                    in
                    match
                      Destructure.bind_pattern ~env destructured_value pattern
                    with
                    | Error _ as err -> err
                    | Ok bindings ->
                        let env_bindings =
                          match (pattern, bindings) with
                          | FSymbol name, [ binding ] ->
                              let constant_keyword =
                                match value_form with
                                | FKeyword keyword -> Some keyword
                                | _ -> None
                              in
                              let false_non_nil_names =
                                false_nil_predicate_names value_form
                              in
                              let local_binding =
                                Types.binding
                                  ?return_param_index:value.return_param_index
                                  ?constant_keyword ~false_non_nil_names
                                  binding.ocaml_name binding.ty
                                |> generalize_function_value value_form
                              in
                              [ (Names.scoped_key scope name, local_binding) ]
                          | _ ->
                              bindings
                              |> List.map
                                   (fun (binding : Destructure.local_binding) ->
                                     ( Names.scoped_key scope binding.source_name,
                                       Types.binding binding.ocaml_name
                                         binding.ty ))
                        in
                        let ir_bindings =
                          match (pattern, bindings) with
                          | FSymbol _, [ binding ]
                            when has_capability binding.ty ->
                              ( located_pattern binding.identity
                                  (capability_pattern binding.ocaml_name
                                     binding.ty),
                                capability_storage_expression binding.ty
                                  value.semantic_expr )
                              :: ir_bindings
                          | FSymbol "_", _ ->
                              ( located_form_pattern pattern Semantic_ir.PAny,
                                value.semantic_expr )
                              :: ir_bindings
                          | _ ->
                              let ir_bindings =
                                match value_binding with
                                | None -> ir_bindings
                                | Some binding -> binding :: ir_bindings
                              in
                              bindings
                              |> List.fold_left
                                   (fun acc
                                        (binding : Destructure.local_binding) ->
                                     let pattern =
                                       if has_capability binding.ty then
                                         capability_pattern binding.ocaml_name
                                           binding.ty
                                       else Semantic_ir.PVar binding.ocaml_name
                                     in
                                     ( located_pattern binding.identity pattern,
                                       binding.semantic_expr )
                                     :: acc)
                                   ir_bindings
                        in
                        let env = Env.add_bindings env_bindings env in
                        let env =
                          Destructure.pattern_names pattern
                          |> List.fold_left
                               (fun env name ->
                                 Env.without_source_callable ~scope name env)
                               env
                        in
                        bind env ir_bindings rest))
            | [ _ ] ->
                Error.error "let bindings require an even number of forms"
          in
          bind env [] forms
    | _ -> Error.error "let bindings must be a vector"
  in
  {
    compile_vector;
    compile_map;
    compile_if;
    compile_if_let;
    compile_if_some;
    compile_when_let;
    compile_when_some;
    compile_let_some;
    compile_match;
    compile_logical;
    compile_body;
    compile_try;
    loop_branch_type;
    compile_recur;
    compile_loop_tail;
    compile_loop_tail_body;
    compile_loop;
    compile_let;
  }
