open Types
open Lowered
module Env = Compiler_environment
module String_map = Map.Make (String)

let adapt_set_callable callable =
  match callable.ty with
  | TSet element_ty ->
      Result.map
        (fun set_module ->
          let set_name = "__lg_callable_set" in
          let item_name = "__lg_callable_set_item" in
          let item = Semantic_ir.Ident item_name in
          let present =
            Semantic_ir.Apply
              ( Semantic_ir.Ident (set_module ^ ".mem"),
                [ item; Semantic_ir.Ident set_name ] )
          in
          typed_ir (TFn ([ element_ty ], TNullable element_ty))
            (Semantic_ir.Let
               ( [ (Semantic_ir.PVar set_name, callable.semantic_expr) ],
                 Semantic_ir.Fun
                   ( [ Semantic_ir.PVar item_name ],
                     Semantic_ir.If
                       ( present,
                         Semantic_ir.Constructor ("Some", Some item),
                         Semantic_ir.Constructor ("None", None) ) ) )))
        (Types.set_module_name element_ty)
  | _ -> Ok callable

let rec truthiness_expression ty expression =
  match ty with
  | ty when Types.is_dynamic ty ->
      Semantic_ir.Apply
        (Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.truthy", [ expression ])
  | TBool -> expression
  | TNil -> Semantic_ir.Sequence [ expression; Semantic_ir.Bool false ]
  | TNullable payload_ty ->
      Semantic_ir.Match
        ( expression,
          [
            (Semantic_ir.PConstructor ("None", None), Semantic_ir.Bool false);
            ( Semantic_ir.PConstructor
                ("Some", Some (Semantic_ir.PVar "truthy_value")),
              truthiness_expression payload_ty
                (Semantic_ir.Ident "truthy_value") );
          ] )
  | TOcaml_app ("option", [ _ ]) | TOcaml "option" ->
      Semantic_ir.Match
        ( expression,
          [
            (Semantic_ir.PConstructor ("None", None), Semantic_ir.Bool false);
            ( Semantic_ir.PConstructor ("Some", Some Semantic_ir.PAny),
              Semantic_ir.Bool true );
          ] )
  | TSeq _ ->
      Semantic_ir.Apply
        ( Semantic_ir.Ident "not",
          [
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.is_empty",
                [ expression ] );
          ] )
  | TOcaml_app (name, [ _ ]) when name = Types.next_seq_type_name ->
      Semantic_ir.Apply
        ( Semantic_ir.Ident "not",
          [
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.is_empty",
                [ expression ] );
          ] )
  | _ -> Semantic_ir.Sequence [ expression; Semantic_ir.Bool true ]

let condition_expression expr =
  Ok (truthiness_expression expr.ty expr.semantic_expr)

let apply name args = Semantic_ir.Apply (Semantic_ir.Ident name, args)

let rec drop n xs =
  if n <= 0 then xs
  else match xs with [] -> [] | _ :: rest -> drop (n - 1) rest

let option_for_all predicate = function
  | None -> true
  | Some value -> predicate value

let is_ocaml_owned_type = function
  | TFloat | TChar | TArray _ | TRef _ | TOcaml _ | TOcaml_app _ | TTuple _ ->
      true
  | _ -> false

let is_ocaml_constructor_pattern_target target_ty name =
  is_ocaml_owned_type target_ty
  ||
  match target_ty with
     | TNullable _ -> List.mem name [ "Some"; "None" ]
     | TUnknown | TVar _ ->
         List.mem name [ "Some"; "None"; "Ok"; "Error" ]
         || String.contains name '.' || String.contains name '/'
  | _ -> false

let plain_dynamic_compatible_type = function
  | TInt | TFloat | TChar | TString | TSymbol | TKeyword | TBool | TNil
  | TNamed_record _ ->
      true
  | ty -> Types.is_dynamic ty

let protocol_value_type ty =
  let value_ty = Types.constraint_value_type ty in
  if Types.equal value_ty ty then None else Some value_ty

let protocol_has_value expected ty =
  match protocol_value_type ty with
  | Some value_ty -> Types.equal expected value_ty
  | None -> false

let rec merge_branch_types left right =
  match (left, right) with
  | TNamed_record left_record, TNamed_record right_record
    when Type_id.equal left_record.type_id right_record.type_id
         && left_record.type_arguments <> right_record.type_arguments -> (
      match Type_solver.unify [] left right with
      | Ok substitutions -> Some (Type_solver.apply substitutions left)
      | Error _ -> None)
  | left, right when Types.equal left right -> Some left
  | left, right ->
    match (left, right) with
    | left, right when protocol_has_value right left ->
        Some right
    | left, right when protocol_has_value left right ->
        Some left
    | left, right when Types.is_dynamic left || Types.is_dynamic right ->
        Some (Types.dynamic_constraint TUnknown)
    | TNil, (TOcaml_app ("option", _) as option_ty)
    | (TOcaml_app ("option", _) as option_ty), TNil
    | TNil, (TOcaml "option" as option_ty)
    | (TOcaml "option" as option_ty), TNil ->
        Some option_ty
    | TNil, TNullable inner | TNullable inner, TNil -> Some (TNullable inner)
    | TNil, ty | ty, TNil -> Some (TNullable ty)
    | TNullable left, TNullable right -> (
        match merge_branch_types left right with
        | Some inner -> Some (TNullable inner)
        | None
          when plain_dynamic_compatible_type left
               && plain_dynamic_compatible_type right ->
            Some (TNullable (Types.dynamic_constraint TUnknown))
        | None -> None)
    | TOcaml_app ("option", [ left ]), TOcaml_app ("option", [ right ]) ->
        Option.map
          (fun merged -> TOcaml_app ("option", [ merged ]))
          (merge_branch_types left right)
    | TNullable left, TOcaml_app ("option", [ right ])
    | TOcaml_app ("option", [ left ]), TNullable right ->
        Option.map
          (fun merged -> TNullable merged)
          (merge_branch_types left right)
    | TNullable inner, ty | ty, TNullable inner ->
        Option.map
          (fun merged -> TNullable merged)
          (merge_branch_types inner ty)
    | TOcaml_app ("option", [ inner ]), ty | ty, TOcaml_app ("option", [ inner ])
      ->
        Option.map
          (fun merged -> TOcaml_app ("option", [ merged ]))
          (merge_branch_types inner ty)
    | TFn (left_params, left_return), TFn (right_params, right_return)
      when List.length left_params = List.length right_params ->
        let merge_parameter left right =
          if Types.equal left right then Some left
          else if
            Types.is_dynamic left || Types.is_dynamic right
            || (plain_dynamic_compatible_type left
               && plain_dynamic_compatible_type right)
          then Some (Types.dynamic_constraint TUnknown)
          else
            match (left, right) with
            | TUnknown, ty | ty, TUnknown | TVar _, ty | ty, TVar _ -> Some ty
            | _ -> None
        in
        let rec merge_parameters merged left right =
          match (left, right) with
          | [], [] -> Some (List.rev merged)
          | left :: left_rest, right :: right_rest ->
              Option.bind (merge_parameter left right) (fun parameter ->
                  merge_parameters (parameter :: merged) left_rest right_rest)
          | _ -> None
        in
        Option.bind (merge_parameters [] left_params right_params)
          (fun parameters ->
            Option.map
              (fun return_ty -> TFn (parameters, return_ty))
              (merge_branch_types left_return right_return))
    | left, right
      when plain_dynamic_compatible_type left
           && plain_dynamic_compatible_type right ->
        Some (Types.dynamic_constraint TUnknown)
    | TList left, TList right ->
        Option.map (fun inner -> TList inner) (merge_branch_types left right)
    | TVector left, TVector right ->
        Option.map (fun inner -> TVector inner) (merge_branch_types left right)
    | TSet left, TSet right ->
        Option.map (fun inner -> TSet inner) (merge_branch_types left right)
    | TSeq left, TSeq right ->
        Option.map (fun inner -> TSeq inner) (merge_branch_types left right)
    | (TList _ | TVector _ | TSeq _), (TList _ | TVector _ | TSeq _) ->
        Some (Types.dynamic_constraint TUnknown)
    | TVar _, TVar _ -> Some left
    | TVar _, ty | ty, TVar _ -> Some ty
    | TUnknown, ty | ty, TUnknown -> Some ty
    | _ when Types.defer_to_ocaml ~expected:left ~actual:right -> Some left
    | _ -> None

let branch_types_compatible left right =
  Option.is_some (merge_branch_types left right)

let capability_storage_expression ty expression =
  let rec build name = function
    | ty when Types.is_dynamic ty -> Semantic_ir.Ident name
    | ty -> (
        match Types.protocol_constraint_info ty with
        | Some (protocol_id, _, value_ty) ->
            Semantic_ir.Tuple
              [
                Semantic_ir.Ident (Types.protocol_witness_name name protocol_id);
                build name value_ty;
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
                    build name value_ty;
                  ]
            | _ -> Semantic_ir.Ident name))
  in
  match Semantic_ir.unlocated expression with
  | Semantic_ir.Ident name -> build name ty
  | _ -> expression

let rec pack_plain_dynamic_value value =
  pack_plain_dynamic_value_impl value
  |> Option.map (fun conversion ->
         Semantic_ir.PackDynamic
           {
             source_ty = value.ty;
             target_ty = Types.dynamic_constraint value.ty;
             conversion;
           })

and pack_plain_dynamic_value_impl value =
  let runtime name arguments =
    Semantic_ir.Apply
      (Semantic_ir.Ident ("Lg_runtime.Runtime_dynamic." ^ name), arguments)
  in
  match value.ty with
  | ty when Types.is_dynamic ty -> Some value.semantic_expr
  | TUnknown | TVar _ -> Some value.semantic_expr
  | TInt -> Some (runtime "int" [ value.semantic_expr ])
  | TFloat -> Some (runtime "float" [ value.semantic_expr ])
  | TChar -> Some (runtime "char" [ value.semantic_expr ])
  | TString -> Some (runtime "string" [ value.semantic_expr ])
  | TSymbol -> Some (runtime "symbol" [ value.semantic_expr ])
  | TKeyword -> Some (runtime "keyword" [ value.semantic_expr ])
  | TBool -> Some (runtime "bool" [ value.semantic_expr ])
  | TNil -> Some (Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.nil")
  | ty
    when Option.is_some (Types.protocol_constraint_info ty)
         || Option.is_some (Types.seqable_constraint_info ty) ->
      pack_plain_dynamic_value_impl
        { value with ty = Types.constraint_value_type ty }
  | TOcaml_app (name, [ element_ty ]) when name = Types.next_seq_type_name ->
      let sequence_name = "__lg_plain_dynamic_next_sequence" in
      let item_name = "__lg_plain_dynamic_next_item" in
      let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
      Option.map
        (fun packed_item ->
          let sequence = Semantic_ir.Ident sequence_name in
          Semantic_ir.Let
            ( [ (Semantic_ir.PVar sequence_name, value.semantic_expr) ],
              Semantic_ir.If
                ( Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.is_empty",
                      [ sequence ] ),
                  Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.nil",
                  runtime "seq"
                    [ Semantic_ir.Apply
                        ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                          [ Semantic_ir.Fun
                              ([ Semantic_ir.PVar item_name ], packed_item);
                            sequence;
                          ] );
                    ] ) ))
        (pack_plain_dynamic_value item)
  | TNullable payload_ty | TOcaml_app ("option", [ payload_ty ]) ->
      let payload_name = "__lg_plain_dynamic_optional_value" in
      let payload = typed_ir payload_ty (Semantic_ir.Ident payload_name) in
      Option.map
        (fun packed_payload ->
          Semantic_ir.Match
            ( value.semantic_expr,
              [
                ( Semantic_ir.PConstructor ("None", None),
                  Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.nil" );
                ( Semantic_ir.PConstructor
                    ("Some", Some (Semantic_ir.PVar payload_name)),
                  packed_payload );
              ] ))
        (pack_plain_dynamic_value payload)
  | TArray element_ty ->
      let item_name = "__lg_plain_dynamic_array_item" in
      let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
      Option.map
        (fun packed_item ->
          runtime "array"
            [
              Semantic_ir.Apply
                ( Semantic_ir.Ident "Array.map",
                  [
                    Semantic_ir.Fun
                      ([ Semantic_ir.PVar item_name ], packed_item);
                    value.semantic_expr;
                  ] );
            ])
        (pack_plain_dynamic_value item)
  | TVector element_ty | TList element_ty | TSeq element_ty ->
      let item_name = "__lg_plain_dynamic_item" in
      let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
      Option.map
        (fun packed_item ->
          let mapper =
            Semantic_ir.Fun ([ Semantic_ir.PVar item_name ], packed_item)
          in
          match value.ty with
          | TVector _ ->
              runtime "vector"
                [
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident "Rrbvec.map",
                      [ mapper; value.semantic_expr ] );
                ]
          | TList _ ->
              runtime "list"
                [
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident "List.map",
                      [ mapper; value.semantic_expr ] );
                ]
          | _ ->
              runtime "seq"
                [
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                      [ mapper; value.semantic_expr ] );
                ])
        (pack_plain_dynamic_value item)
  | TSet element_ty -> (
      match Types.set_module_name element_ty with
      | Error _ -> None
      | Ok set_module ->
          let item_name = "__lg_plain_dynamic_set_item" in
          let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
          Option.map
            (fun packed_item ->
              runtime "set"
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
                                ( Semantic_ir.Ident (set_module ^ ".elements"),
                                  [ value.semantic_expr ] );
                            ] );
                      ] );
                ])
            (pack_plain_dynamic_value item))
  | TOcaml_app ("Lg_runtime.Runtime_map.t", [ key_ty; value_ty ]) ->
          let key_name = "__lg_plain_dynamic_map_key" in
          let value_name = "__lg_plain_dynamic_map_value" in
          let key = typed_ir key_ty (Semantic_ir.Ident key_name) in
          let map_value = typed_ir value_ty (Semantic_ir.Ident value_name) in
          (match
             (pack_plain_dynamic_value key, pack_plain_dynamic_value map_value)
           with
          | Some packed_key, Some packed_value ->
              Some
                (runtime "map"
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
                               Semantic_ir.Tuple [ packed_key; packed_value ] );
                           value.semantic_expr;
                         ] );
                   ])
          | None, _ | _, None -> None)
  | TOcaml_app (name, [ element_ty ]) when name = Types.next_seq_type_name ->
      let item_name = "__lg_plain_dynamic_seq_item" in
      let item = typed_ir element_ty (Semantic_ir.Ident item_name) in
      Option.map
        (fun packed_item ->
          runtime "seq"
            [
              Semantic_ir.Apply
                ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                  [
                    Semantic_ir.Fun
                      ([ Semantic_ir.PVar item_name ], packed_item);
                    value.semantic_expr;
                  ] );
            ])
        (pack_plain_dynamic_value item)
  | TNamed_record record ->
      let rec fields packed = function
        | [] -> Some (List.rev packed)
        | (field : field) :: rest -> (
            let field_value =
              typed_ir field.ty (Structural_map.field_expr value field)
            in
            match pack_plain_dynamic_value field_value with
            | None -> None
            | Some field_value ->
                let key =
                  runtime "keyword" [ Semantic_ir.String field.keyword ]
                in
                fields (Semantic_ir.Tuple [ key; field_value ] :: packed) rest)
      in
      Option.map
        (fun fields ->
          runtime "record"
            [ Semantic_ir.String record.type_name; Semantic_ir.List fields ])
        (fields [] record.fields)
  | _ -> None

let coerce_expression_to_type ?(stored = false) target_ty source_ty expression =
  match (target_ty, source_ty) with
  | target_ty, source_ty when protocol_has_value target_ty source_ty ->
      let rec unwrap ty expression =
        match Types.protocol_constraint_info ty with
        | None -> expression
        | Some (_, _, value_ty) ->
            let expression =
              if stored then
                Semantic_ir.Apply (Semantic_ir.Ident "snd", [ expression ])
              else
                match Semantic_ir.unlocated expression with
                | Semantic_ir.Ident _ -> expression
                | _ ->
                    Semantic_ir.Apply (Semantic_ir.Ident "snd", [ expression ])
            in
            unwrap value_ty expression
      in
      unwrap source_ty expression
  | TSeq target_inner, (TList source_inner | TVector source_inner) ->
      let sequence =
        match source_ty with
        | TList _ ->
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.of_list",
                [ expression ] )
        | TVector _ ->
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.of_vector",
                [ expression ] )
        | _ -> assert false
      in
      if Types.is_dynamic target_inner && not (Types.is_dynamic source_inner)
      then
        let item_name = "__lg_coerce_dynamic_seq_item" in
        let item = typed_ir source_inner (Semantic_ir.Ident item_name) in
        let packed =
          pack_plain_dynamic_value item
          |> Option.value ~default:item.semantic_expr
        in
        Semantic_ir.Apply
          ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
            [ Semantic_ir.Fun ([ Semantic_ir.PVar item_name ], packed); sequence ] )
      else if Types.same_shape target_inner source_inner then sequence
      else sequence
  | TSet target_inner, TSet (TUnknown | TVar _) -> (
      match Types.set_module_name target_inner with
      | Ok set_module ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident (set_module ^ ".of_list"),
              [
                Semantic_ir.Apply
                  ( Semantic_ir.Ident
                      "Lg_runtime.Runtime_poly_set.elements",
                    [ expression ] );
              ] )
      | Error _ -> expression)
  | TVector element_ty, source_ty when Types.is_dynamic source_ty ->
      let dynamic name arguments =
        Semantic_ir.Apply
          (Semantic_ir.Ident ("Lg_runtime.Runtime_dynamic." ^ name), arguments)
      in
      let item_name = "__lg_dynamic_vector_item" in
      let item = Semantic_ir.Ident item_name in
      let unpacked_item =
        match element_ty with
        | ty when Types.is_dynamic ty -> item
        | TInt -> dynamic "as_int" [ item ]
        | TFloat -> dynamic "as_float" [ item ]
        | TChar -> dynamic "as_char" [ item ]
        | TString -> dynamic "as_string" [ item ]
        | TSymbol -> dynamic "as_symbol" [ item ]
        | TKeyword -> dynamic "as_keyword" [ item ]
        | TBool -> dynamic "as_bool" [ item ]
        | _ -> item
      in
      Semantic_ir.UnpackDynamic
        {
          source_ty;
          target_ty;
          conversion =
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Rrbvec.of_list",
                [
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident "List.of_seq",
                      [
                        Semantic_ir.Apply
                          ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.map",
                            [
                              Semantic_ir.Fun
                                ([ Semantic_ir.PVar item_name ], unpacked_item);
                              dynamic "to_seq" [ expression ];
                            ] );
                      ] );
                ] );
          }
  | target_ty, (TNullable source_ty | TOcaml_app ("option", [ source_ty ]))
    when (match target_ty with
         | TRecord _ | TNamed_record _ | TArray _
         | TOcaml_app ("array", [ _ ]) ->
             true
         | _ -> false)
         && Types.assignable ~policy:Host_boundary ~expected:target_ty
              ~actual:source_ty ->
      Semantic_ir.Apply (Semantic_ir.Ident "Option.get", [ expression ])
  | ( (TNullable target | TOcaml_app ("option", [ target ])),
      (TNullable source | TOcaml_app ("option", [ source ])) )
    when Types.is_dynamic target && Types.is_dynamic source ->
      expression
  | target_ty, source_ty
    when Types.is_dynamic target_ty && not (Types.is_dynamic source_ty) ->
      pack_plain_dynamic_value (typed_ir source_ty expression)
      |> Option.value ~default:expression
  | TNullable target, TNullable source
    when Types.is_dynamic target && not (Types.is_dynamic source) ->
      let value_name = "__lg_nullable_dynamic_value" in
      let value = typed_ir source (Semantic_ir.Ident value_name) in
      let packed =
        pack_plain_dynamic_value value
        |> Option.value ~default:value.semantic_expr
      in
      Semantic_ir.Match
        ( expression,
          [
            ( Semantic_ir.PConstructor ("None", None),
              Semantic_ir.Constructor ("None", None) );
            ( Semantic_ir.PConstructor
                ("Some", Some (Semantic_ir.PVar value_name)),
                  Semantic_ir.Constructor ("Some", Some packed) );
          ] )
  | ( (TNullable target | TOcaml_app ("option", [ target ])),
      TOcaml_app (name, [ _ ]) )
    when name = Types.next_seq_type_name ->
      let sequence_name = "__lg_nullable_next_sequence" in
      let sequence = Semantic_ir.Ident sequence_name in
      let present =
        if Types.is_dynamic target then
          pack_plain_dynamic_value (typed_ir source_ty sequence)
          |> Option.value ~default:sequence
        else sequence
      in
      Semantic_ir.Let
        ( [ (Semantic_ir.PVar sequence_name, expression) ],
          Semantic_ir.If
            ( Semantic_ir.Apply
                ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.is_empty",
                  [ sequence ] ),
              Semantic_ir.Constructor ("None", None),
              Semantic_ir.Constructor ("Some", Some present) ) )
  | TNullable target, source
    when Types.is_dynamic target
         && (not (Types.is_dynamic source))
         && not (Types.equal source TNil) ->
      let value = typed_ir source expression in
      let packed =
        pack_plain_dynamic_value value
        |> Option.value ~default:value.semantic_expr
      in
      Semantic_ir.Constructor ("Some", Some packed)
  | TOcaml_app ("option", [ target ]), source
    when Types.is_dynamic target
         && (not (Types.is_dynamic source))
         && not (Types.equal source TNil) ->
      let value = typed_ir source expression in
      let packed =
        pack_plain_dynamic_value value
        |> Option.value ~default:value.semantic_expr
      in
      Semantic_ir.Constructor ("Some", Some packed)
  | target_ty, TOcaml_app (constraint_name, [ _element_ty; _value_ty ])
    when (match target_ty with
         | TSeq _ -> true
         | TOcaml_app (name, [ _ ]) -> name = Types.next_seq_type_name
         | _ -> false)
         && (constraint_name = Types.seqable_constraint_name
            || constraint_name = Types.optional_seqable_constraint_name
            || constraint_name = Types.optional_sequential_constraint_name) ->
      let adapter_name = "__lg_coerce_seq_adapter" in
      let value_name = "__lg_coerce_seq_value" in
      let adapter = Semantic_ir.Ident adapter_name in
      let adapter =
        if constraint_name = Types.seqable_constraint_name then adapter
        else
          Semantic_ir.Match
            ( adapter,
              [
                ( Semantic_ir.PConstructor ("None", None),
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident "invalid_arg",
                      [ Semantic_ir.String "value is not sequential" ] ) );
                ( Semantic_ir.PConstructor
                    ("Some", Some (Semantic_ir.PVar adapter_name)),
                  Semantic_ir.Ident adapter_name );
              ] )
      in
      Semantic_ir.Match
        ( (if stored then expression
           else capability_storage_expression source_ty expression),
          [
            ( Semantic_ir.PTuple
                [ Semantic_ir.PVar adapter_name; Semantic_ir.PVar value_name ],
              Semantic_ir.Apply (adapter, [ Semantic_ir.Ident value_name ]) );
          ] )
  | TNullable _, TNil -> expression
  | TNullable _, TNullable _ -> expression
  | TNullable _, TOcaml_app ("option", [ _ ]) -> expression
  | TNullable _, _ -> Semantic_ir.Constructor ("Some", Some expression)
  | TOcaml_app ("option", [ _ ]), TNil -> expression
  | TOcaml_app ("option", [ _ ]), TNullable _ -> expression
  | TOcaml_app ("option", [ _ ]), TOcaml_app ("option", [ _ ]) -> expression
  | TOcaml_app ("option", [ _ ]), _ ->
      Semantic_ir.Constructor ("Some", Some expression)
  | _ -> expression

let merge_branch_expressions left right =
  let merge_tuple_items left_types left_items right_types right_items =
    let rec merge types left_values right_values =
      match (types, left_values, right_values) with
      | [], [], [] -> Some ([], [], [])
      | ( (left_ty, right_ty) :: types,
          left_value :: left_values,
          right_value :: right_values ) ->
          let merged_ty =
            match merge_branch_types left_ty right_ty with
            | Some ty -> Some ty
            | None
              when plain_dynamic_compatible_type left_ty
                   && plain_dynamic_compatible_type right_ty ->
                Some (Types.dynamic_constraint TUnknown)
            | None -> None
          in
          Option.bind merged_ty (fun merged_ty ->
              Option.map
                (fun (merged_types, merged_left, merged_right) ->
                  ( merged_ty :: merged_types,
                    coerce_expression_to_type ~stored:true merged_ty left_ty
                      left_value
                    :: merged_left,
                    coerce_expression_to_type ~stored:true merged_ty right_ty
                      right_value
                    :: merged_right ))
                (merge types left_values right_values))
      | _ -> None
    in
    merge (List.combine left_types right_types) left_items right_items
  in
  match (left.ty, right.ty) with
  | TTuple left_types, TTuple right_types
    when List.length left_types = List.length right_types ->
      let left_names =
        List.mapi
          (fun index _ -> "__lg_left_tuple_" ^ string_of_int index)
          left_types
      in
      let right_names =
        List.mapi
          (fun index _ -> "__lg_right_tuple_" ^ string_of_int index)
          right_types
      in
      Option.map
        (fun (types, left_items, right_items) ->
          let rebuild expression names items =
            Semantic_ir.Match
              ( expression,
                [
                  ( Semantic_ir.PTuple
                      (List.map (fun name -> Semantic_ir.PVar name) names),
                    Semantic_ir.Tuple items );
                ] )
          in
          ( TTuple types,
            rebuild left.semantic_expr left_names left_items,
            rebuild right.semantic_expr right_names right_items ))
        (merge_tuple_items left_types
           (List.map (fun name -> Semantic_ir.Ident name) left_names)
           right_types
           (List.map (fun name -> Semantic_ir.Ident name) right_names))
  | _ -> (
  let continue expression =
    Semantic_ir.Apply
          ( Semantic_ir.Ident "Lg_runtime.Runtime_reduced.continue",
            [ expression ] )
  in
  match (Types.reduced_element left.ty, Types.reduced_element right.ty) with
      | Some left_inner, Some right_inner
        when Types.equal left_inner right_inner ->
      Some (left.ty, left.semantic_expr, right.semantic_expr)
  | Some TNil, None ->
      let nullable = TNullable right.ty in
      Some
        ( Types.reduced nullable,
          left.semantic_expr,
              continue
                (Semantic_ir.Constructor ("Some", Some right.semantic_expr)) )
  | None, Some TNil ->
      let nullable = TNullable left.ty in
      Some
        ( Types.reduced nullable,
              continue
                (Semantic_ir.Constructor ("Some", Some left.semantic_expr)),
          right.semantic_expr )
  | Some inner, None when Types.equal inner right.ty ->
      Some (left.ty, left.semantic_expr, continue right.semantic_expr)
  | None, Some inner when Types.equal left.ty inner ->
      Some (right.ty, continue left.semantic_expr, right.semantic_expr)
  | _ -> (
      match merge_branch_types left.ty right.ty with
      | None -> None
      | Some result_ty ->
          Some
            ( result_ty,
              coerce_expression_to_type result_ty left.ty left.semantic_expr,
                  coerce_expression_to_type result_ty right.ty
                    right.semantic_expr )))

let unresolved_contextual_type = function TList TUnknown -> true | _ -> false

let lg_metadata_type_for_ocaml_payload = function
  | TOcaml "int" -> TInt
  | TOcaml "float" -> TFloat
  | TOcaml "char" -> TChar
  | TOcaml "string" -> TString
  | TOcaml "bool" -> TBool
  | TOcaml "unit" -> TUnit
  | ty -> ty

let rec lg_metadata_type_for_ocaml_type = function
  | TOcaml "int" -> TInt
  | TOcaml "float" -> TFloat
  | TOcaml "char" -> TChar
  | TOcaml "string" -> TString
  | TOcaml "bool" -> TBool
  | TOcaml "unit" -> TUnit
  | TTuple args -> TTuple (List.map lg_metadata_type_for_ocaml_type args)
  | ty -> ty

let ocaml_builtin_constructor_payloads target_ty constructor_name =
  match (target_ty, constructor_name) with
  | TNullable payload_ty, "Some" -> Some [ payload_ty ]
  | TNullable _, "None" -> Some []
  | TOcaml "option", "Some" -> Some [ TUnknown ]
  | TOcaml "option", "None" -> Some []
  | TOcaml_app ("option", [ payload_ty ]), "Some" ->
      Some [ lg_metadata_type_for_ocaml_payload payload_ty ]
  | TOcaml_app ("option", [ _ ]), "None" -> Some []
  | TOcaml "result", "Ok" -> Some [ TUnknown ]
  | TOcaml "result", "Error" -> Some [ TUnknown ]
  | TOcaml_app ("result", [ ok_ty; _ ]), "Ok" ->
      Some [ lg_metadata_type_for_ocaml_payload ok_ty ]
  | TOcaml_app ("result", [ _; error_ty ]), "Error" ->
      Some [ lg_metadata_type_for_ocaml_payload error_ty ]
  | _ -> None

let record_type_key = Resolver.record_type_key

let record_type_application type_name arguments =
  let argument_name = function
    | TUnknown | TVar _ -> "_"
    | argument -> Types.ocaml_name argument
  in
  match arguments with
  | [] -> type_name
  | [ argument ] -> argument_name argument ^ " " ^ type_name
  | arguments ->
      "("
      ^ String.concat ", " (List.map argument_name arguments)
      ^ ") " ^ type_name

let existential_record_type_application (record : named_record) =
  record_type_application record.type_name
    (List.map (fun parameter -> TVar parameter) record.type_parameters)

let lookup_record_type = Resolver.lookup_record_type

let starts_with_uppercase name =
  String.length name > 0
  &&
  let first = name.[0] in
  first >= 'A' && first <= 'Z'

let is_constructor_name name =
  let segments =
    name |> String.split_on_char '/'
    |> List.concat_map (String.split_on_char '.')
  in
  match List.rev segments with
  | segment :: _ -> starts_with_uppercase segment
  | [] -> false

let lookup_binding = Resolver.lookup_binding

let deftype_method_name (record : named_record) method_name arity =
  "__deftype/"
  ^ Type_id.to_string record.type_id
  ^ "/" ^ method_name ^ "/" ^ string_of_int arity

let lookup_deftype_method scope env record method_name arity =
  lookup_binding scope env (deftype_method_name record method_name arity)

let print_method_name (record : named_record) =
  "__print_method/" ^ Type_id.to_string record.type_id

let lookup_print_method scope env record =
  lookup_binding scope env (print_method_name record)

let binding_of_expr ?(row_param_types = []) ocaml_name expr =
  let binding =
    Types.binding ~row_param_types ?return_param_index:expr.return_param_index
      ocaml_name expr.ty
  in
  {
    binding with
    ty = Types.align_deferred_param_types binding.ty expr.semantic_expr;
  }

type anonymous_record_allocation = {
  record : named_record;
  env : Env.t;
  next_type : int;
  fresh : bool;
}

let allocate_anonymous_record ~owner env next_type fields =
  match Env.find_anonymous_record ~owner fields env with
  | Some record -> { record; env; next_type; fresh = false }
  | None ->
      let type_name = "t" ^ string_of_int next_type in
      let set_module_name = "Set_" ^ type_name in
      let record =
        match Types.named_record ~type_name ~set_module_name fields with
        | TNamed_record record -> record
        | _ -> assert false
      in
      {
        record;
        env = Env.add_anonymous_record ~owner record env;
        next_type = next_type + 1;
        fresh = true;
      }

type nested_record_allocation = {
  nested_fields : field list;
  env : Env.t;
  next_type : int;
  items : compiled_item list;
}

let allocate_nested_anonymous_records ~owner env next_type fields =
  let rec allocate_type env next_type items = function
    | TRecord fields ->
        let allocated = allocate_fields env next_type items fields in
        let record =
          allocate_anonymous_record ~owner allocated.env allocated.next_type
            allocated.nested_fields
        in
        let items =
          if record.fresh then
            allocated.items
            @ [
                Type_def
                  {
                    type_name = record.record.type_name;
                    type_parameters = record.record.type_parameters;
                    fields = record.record.fields;
                    location = None;
                  };
              ]
          else allocated.items
        in
        (TNamed_record record.record, record.env, record.next_type, items)
    | TNullable inner ->
        map_inner env next_type items (fun inner -> TNullable inner) inner
    | TOcaml_app (name, arguments) ->
        let arguments, env, next_type, items =
          allocate_types env next_type items arguments
        in
        (TOcaml_app (name, arguments), env, next_type, items)
    | TTuple arguments ->
        let arguments, env, next_type, items =
          allocate_types env next_type items arguments
        in
        (TTuple arguments, env, next_type, items)
    | TArray inner ->
        map_inner env next_type items (fun inner -> TArray inner) inner
    | TRef inner ->
        map_inner env next_type items (fun inner -> TRef inner) inner
    | TList inner ->
        map_inner env next_type items (fun inner -> TList inner) inner
    | TVector inner ->
        map_inner env next_type items (fun inner -> TVector inner) inner
    | TSet inner ->
        map_inner env next_type items (fun inner -> TSet inner) inner
    | TSeq inner ->
        map_inner env next_type items (fun inner -> TSeq inner) inner
    | TFn (parameters, return_type) ->
        let parameters, env, next_type, items =
          allocate_types env next_type items parameters
        in
        let return_type, env, next_type, items =
          allocate_type env next_type items return_type
        in
        (TFn (parameters, return_type), env, next_type, items)
    | TOverloaded_fn arities ->
        let rec allocate_arities env next_type items allocated = function
          | [] -> (TOverloaded_fn (List.rev allocated), env, next_type, items)
          | arity :: rest ->
              let fixed_params, env, next_type, items =
                allocate_types env next_type items arity.fixed_params
              in
              let rest_param, env, next_type, items =
                match arity.rest_param with
                | None -> (None, env, next_type, items)
                | Some rest_param ->
                    let rest_param, env, next_type, items =
                      allocate_type env next_type items rest_param
                    in
                    (Some rest_param, env, next_type, items)
              in
              let return_ty, env, next_type, items =
                allocate_type env next_type items arity.return_ty
              in
              allocate_arities env next_type items
                ({ fixed_params; rest_param; return_ty } :: allocated)
                rest
        in
        allocate_arities env next_type items [] arities
    | ( TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol
      | TKeyword | TBool | TUnit | TNil | TUnknown | TVar _ | TOcaml _
      | TNamed_record _ ) as ty ->
        (ty, env, next_type, items)
  and map_inner env next_type items wrap inner =
    let inner, env, next_type, items =
      allocate_type env next_type items inner
    in
    (wrap inner, env, next_type, items)
  and allocate_types env next_type items types =
    let rec loop env next_type items allocated = function
      | [] -> (List.rev allocated, env, next_type, items)
      | ty :: rest ->
          let ty, env, next_type, items =
            allocate_type env next_type items ty
          in
          loop env next_type items (ty :: allocated) rest
    in
    loop env next_type items [] types
  and allocate_fields env next_type items fields =
    let rec loop env next_type items allocated = function
      | [] -> { nested_fields = List.rev allocated; env; next_type; items }
      | (field : field) :: rest ->
          let ty, env, next_type, items =
            allocate_type env next_type items field.ty
          in
          loop env next_type items ({ field with ty } :: allocated) rest
    in
    loop env next_type items [] fields
  in
  allocate_fields env next_type [] fields

let check_emitted_name_collision = Resolver.check_emitted_name_collision

let dynamic_callable name argument_names body =
  let arguments_name =
    "__lg_" ^ Names.sanitize_name name ^ "_arguments"
  in
  Semantic_ir.Apply
    ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.function_",
      [
        Semantic_ir.Fun
          ( [ Semantic_ir.PVar arguments_name ],
            Semantic_ir.Match
              ( Semantic_ir.Ident arguments_name,
                [
                  ( Semantic_ir.PList
                      (List.map (fun name -> Semantic_ir.PVar name) argument_names),
                    body );
                  ( Semantic_ir.PAny,
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "invalid_arg",
                        [ Semantic_ir.String (name ^ " called with wrong arity") ] ) );
                ] ) );
      ] )

let regex_captures target regex_name groups_name =
  match target with
  | Target.Melange ->
      Semantic_ir.Apply
        ( Semantic_ir.Ident "List.map",
          [
            Semantic_ir.Ident "Js.Nullable.toOption";
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Array.to_list",
                [
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident "Js.Re.captures",
                      [ Semantic_ir.Ident groups_name ] );
                ] );
          ] )
  | Target.Native | Target.Js_of_ocaml ->
      let index_name = "__lg_regex_group_index" in
      Semantic_ir.Apply
        ( Semantic_ir.Ident "List.init",
          [
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Re.group_count",
                [ Semantic_ir.Ident regex_name ] );
            Semantic_ir.Fun
              ( [ Semantic_ir.PVar index_name ],
                Semantic_ir.Apply
                  ( Semantic_ir.Ident "Re.Group.get_opt",
                    [
                      Semantic_ir.Ident groups_name;
                      Semantic_ir.Ident index_name;
                    ] ) );
          ] )

let dynamic_regex_match target ~whole expression source =
  let regex_name = "__lg_dynamic_regex" in
  let groups_name = "__lg_dynamic_regex_groups" in
  let pattern =
    Semantic_ir.Apply
      ( Semantic_ir.Ident "Lg_runtime.Runtime_string.regex_pattern",
        [ expression ] )
  in
  let regex, execution =
    match target with
    | Target.Melange ->
        let pattern =
          if whole then
            Codegen.concat_expr
              [ Semantic_ir.String "^(?:"; pattern; Semantic_ir.String ")$" ]
          else pattern
        in
        ( Semantic_ir.Apply
            (Semantic_ir.Ident "Js.Re.fromString", [ pattern ]),
          Semantic_ir.Labelled_apply
            ( Semantic_ir.Ident "Js.Re.exec",
              [
                (Some "str", source);
                (None, Semantic_ir.Ident regex_name);
              ] ) )
    | Target.Native | Target.Js_of_ocaml ->
        let regex_expression =
          Semantic_ir.Apply (Semantic_ir.Ident "Re.Perl.re", [ pattern ])
        in
        let regex_expression =
          if whole then
            Semantic_ir.Apply
              (Semantic_ir.Ident "Re.whole_string", [ regex_expression ])
          else regex_expression
        in
        ( Semantic_ir.Apply
            (Semantic_ir.Ident "Re.compile", [ regex_expression ]),
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Re.exec_opt",
              [ Semantic_ir.Ident regex_name; source ] ) )
  in
  let match_value =
    Semantic_ir.Apply
      ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.regex_match",
        [
          Semantic_ir.Constructor
            ( "Some",
              Some (regex_captures target regex_name groups_name) );
        ] )
  in
  let no_match =
    Semantic_ir.Apply
      ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.regex_match",
        [ Semantic_ir.Constructor ("None", None) ] )
  in
  Semantic_ir.Let
    ( [ (Semantic_ir.PVar regex_name, regex) ],
      Semantic_ir.Match
        ( execution,
          [
            (Semantic_ir.PConstructor ("None", None), no_match);
            ( Semantic_ir.PConstructor
                ("Some", Some (Semantic_ir.PVar groups_name)),
              match_value );
          ] ) )

let dynamic_regex_sequence target expression source =
  let regex_name = "__lg_dynamic_regex" in
  let groups_name = "__lg_dynamic_regex_groups" in
  let pattern =
    Semantic_ir.Apply
      ( Semantic_ir.Ident "Lg_runtime.Runtime_string.regex_pattern",
        [ expression ] )
  in
  match target with
  | Target.Native | Target.Js_of_ocaml ->
      let regex =
        Semantic_ir.Apply
          ( Semantic_ir.Ident "Re.compile",
            [ Semantic_ir.Apply (Semantic_ir.Ident "Re.Perl.re", [ pattern ]) ] )
      in
      let matches =
        Semantic_ir.Apply
          ( Semantic_ir.Ident "List.map",
            [
              Semantic_ir.Fun
                ( [ Semantic_ir.PVar groups_name ],
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.regex_match",
                      [
                        Semantic_ir.Constructor
                          ( "Some",
                            Some
                              (regex_captures target regex_name groups_name) );
                      ] ) );
              Semantic_ir.Apply
                ( Semantic_ir.Ident "Re.all",
                  [ Semantic_ir.Ident regex_name; source ] );
            ] )
      in
      Semantic_ir.Let
        ( [ (Semantic_ir.PVar regex_name, regex) ],
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.seq",
              [
                Semantic_ir.Apply
                  (Semantic_ir.Ident "List.to_seq", [ matches ]);
              ] ) )
  | Target.Melange ->
      let loop_name = "__lg_dynamic_regex_collect" in
      let acc_name = "__lg_dynamic_regex_acc" in
      let regex =
        Semantic_ir.Labelled_apply
          ( Semantic_ir.Ident "Js.Re.fromStringWithFlags",
            [ (None, pattern); (Some "flags", Semantic_ir.String "g") ] )
      in
      let execution =
        Semantic_ir.Labelled_apply
          ( Semantic_ir.Ident "Js.Re.exec",
            [ (Some "str", source); (None, Semantic_ir.Ident regex_name) ] )
      in
      let match_value =
        Semantic_ir.Apply
          ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.regex_match",
            [
              Semantic_ir.Constructor
                ( "Some",
                  Some (regex_captures target regex_name groups_name) );
            ] )
      in
      let collected =
        Semantic_ir.LetRecIn
          ( loop_name,
            [ Semantic_ir.PVar acc_name ],
            Semantic_ir.Match
              ( execution,
                [
                  ( Semantic_ir.PConstructor ("None", None),
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "List.rev",
                        [ Semantic_ir.Ident acc_name ] ) );
                  ( Semantic_ir.PConstructor
                      ("Some", Some (Semantic_ir.PVar groups_name)),
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident loop_name,
                        [
                          Semantic_ir.Cons
                            (match_value, Semantic_ir.Ident acc_name);
                        ] ) );
                ] ),
            Semantic_ir.Apply
              (Semantic_ir.Ident loop_name, [ Semantic_ir.List [] ]) )
      in
      Semantic_ir.Let
        ( [ (Semantic_ir.PVar regex_name, regex) ],
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.seq",
              [
                Semantic_ir.Apply
                  (Semantic_ir.Ident "List.to_seq", [ collected ]);
              ] ) )

let lookup_regex_function env name =
  let target = Env.target env in
  let dynamic_string argument =
    Semantic_ir.Apply
      ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.as_string",
        [ Semantic_ir.Ident argument ] )
  in
  match name with
  | "re-pattern" ->
      let pattern_name = "__lg_regex_pattern" in
      Some
        (dynamic_callable name [ pattern_name ]
           (Semantic_ir.Apply
              ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.string",
                [
                  Codegen.concat_expr
                    [
                      Semantic_ir.String "\000lg-regex:";
                      dynamic_string pattern_name;
                    ];
                ] )))
  | "re-matches" | "re-find" ->
      let expression_name = "__lg_regex_expression" in
      let source_name = "__lg_regex_source" in
      Some
        (dynamic_callable name [ expression_name; source_name ]
           (dynamic_regex_match target ~whole:(name = "re-matches")
              (dynamic_string expression_name) (dynamic_string source_name)))
  | "re-seq" ->
      let expression_name = "__lg_regex_expression" in
      let source_name = "__lg_regex_source" in
      Some
        (dynamic_callable name [ expression_name; source_name ]
           (dynamic_regex_sequence target (dynamic_string expression_name)
              (dynamic_string source_name)))
  | _ -> None

let lookup_function scope env name =
  let dynamic = Types.dynamic_constraint TUnknown in
  let dynamic_function runtime_name =
    typed_ir
      (Types.dynamic_constraint TUnknown)
      (Semantic_ir.Ident ("Lg_runtime.Runtime_dynamic." ^ runtime_name))
  in
  let static_function parameter_tys return_ty runtime_name =
    typed_ir
      (TFn (parameter_tys, return_ty))
      (Semantic_ir.Ident ("Lg_runtime.Runtime_dynamic." ^ runtime_name))
  in
  match lookup_binding scope env name with
  | Ok binding ->
      Ok (typed_ir binding.ty (Semantic_ir.Ident binding.ocaml_name))
  | Error _ -> (
      match name with
      | "=" | "==" ->
          Ok (dynamic_function "equality_function")
      | "not=" | "!=" ->
          Ok (dynamic_function "inequality_function")
      | "quot" -> Ok (static_function [ TInt; TInt ] TInt "int_quot")
      | "rem" -> Ok (static_function [ TInt; TInt ] TInt "int_rem")
      | "mod" -> Ok (static_function [ TInt; TInt ] TInt "clojure_mod")
      | "+" ->
          Ok
            (typed_ir
               (TFn ([ TInt; TInt ], TInt))
               (Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "a"; Semantic_ir.PVar "b" ],
                    Semantic_ir.Infix
                      ("+", Semantic_ir.Ident "a", Semantic_ir.Ident "b") )))
      | "-" ->
          Ok
            (typed_ir
               (TFn ([ TInt; TInt ], TInt))
               (Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "a"; Semantic_ir.PVar "b" ],
                    Semantic_ir.Infix
                      ("-", Semantic_ir.Ident "a", Semantic_ir.Ident "b") )))
      | "*" ->
          Ok
            (typed_ir
               (TFn ([ TInt; TInt ], TInt))
               (Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "a"; Semantic_ir.PVar "b" ],
                    Semantic_ir.Infix
                      ("*", Semantic_ir.Ident "a", Semantic_ir.Ident "b") )))
      | "/" ->
          Ok
            (typed_ir
               (TFn ([ TInt; TInt ], TInt))
               (Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "a"; Semantic_ir.PVar "b" ],
                    Semantic_ir.Infix
                      ("/", Semantic_ir.Ident "a", Semantic_ir.Ident "b") )))
      | "inc" ->
          Ok (static_function [ TInt ] TInt "int_inc")
      | "dec" ->
          Ok (static_function [ TInt ] TInt "int_dec")
      | "max" -> Ok (static_function [ TInt; TInt ] TInt "int_max")
      | "min" -> Ok (static_function [ TInt; TInt ] TInt "int_min")
      | "zero?" -> Ok (static_function [ TInt ] TBool "int_zero")
      | "pos?" -> Ok (static_function [ TInt ] TBool "int_positive")
      | "neg?" -> Ok (static_function [ TInt ] TBool "int_negative")
      | "even?" -> Ok (static_function [ TInt ] TBool "int_even")
      | "odd?" -> Ok (static_function [ TInt ] TBool "int_odd")
      | "compare" ->
          Ok (static_function [ dynamic; dynamic ] TInt "compare")
      | "rand" -> Ok (dynamic_function "rand_function")
      | "rand-int" ->
          Ok
            (typed_ir
               (TFn ([ TInt ], TInt))
               (Semantic_ir.Ident "Lg_runtime.Runtime_random.rand_int"))
      | "true?" -> Ok (static_function [ dynamic ] TBool "is_true")
      | "false?" -> Ok (static_function [ dynamic ] TBool "is_false")
      | "nil?" -> Ok (static_function [ dynamic ] TBool "is_nil")
      | "some?" -> Ok (static_function [ dynamic ] TBool "is_some")
      | "complement" ->
          Ok
            (static_function [ dynamic ] dynamic "complement_value")
      | "identical?" ->
          Ok
            (static_function [ dynamic; dynamic ] TBool "identical")
      | "identity" ->
          Ok (static_function [ dynamic ] dynamic "identity_value")
      | "keyword" ->
          Ok (static_function [ dynamic ] dynamic "keyword_value")
      | "meta" -> Ok (static_function [ dynamic ] dynamic "metadata")
      | "name" -> Ok (static_function [ dynamic ] dynamic "name_value")
      | "namespace" ->
          Ok
            (static_function [ dynamic ] dynamic "identifier_namespace")
      | "resolve" ->
          Ok
            (typed_ir
               (TFn ([ TSymbol ], TNullable (TRef dynamic)))
               (Semantic_ir.Fun
                  ( [ Semantic_ir.PAny ],
                    Semantic_ir.Constructor ("None", None) )))
      | "type" -> Ok (static_function [ dynamic ] dynamic "class_")
      | "vector" -> Ok (dynamic_function "vector_function")
      | "list" -> Ok (dynamic_function "list_function")
      | "set" -> Ok (dynamic_function "set_function")
      | "hash-map" -> Ok (dynamic_function "hash_map_function")
      | "array-map" -> Ok (dynamic_function "array_map_function")
      | "count" -> Ok (dynamic_function "count_function")
      | "ffirst" -> Ok (static_function [ dynamic ] dynamic "ffirst_value")
      | "to-array" | "into-array" | "array-from" ->
          let collection = "__lg_array_collection" in
          Ok
            (typed_ir
               (TFn ([ dynamic ], TArray dynamic))
               (Semantic_ir.Fun
                  ( [ Semantic_ir.PVar collection ],
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Array.of_seq",
                        [
                          Semantic_ir.Apply
                            ( Semantic_ir.Ident
                                "Lg_runtime.Runtime_dynamic.to_seq",
                              [ Semantic_ir.Ident collection ] );
                        ] ) )))
      | "range" -> Ok (dynamic_function "range_function")
      | "not-empty" -> Ok (dynamic_function "not_empty_function")
      | "empty?" -> Ok (dynamic_function "empty_predicate_function")
      | "contains?" -> Ok (dynamic_function "contains_function")
      | "str" -> Ok (static_function [ dynamic ] TString "str_value")
      | "subs" -> Ok (dynamic_function "subs_function")
      | "get" -> Ok (dynamic_function "get_function")
      | "pr-str" -> Ok (dynamic_function "pr_str_function")
      | "print-str" -> Ok (dynamic_function "print_str_function")
      | "println-str" -> Ok (dynamic_function "println_str_function")
      | "prn-str" -> Ok (dynamic_function "prn_str_function")
      | "str/escape" | "clojure.string/escape" ->
          Ok (dynamic_function "escape_function")
      | "number?" -> Ok (static_function [ dynamic ] TBool "is_number")
      | "integer?" -> Ok (static_function [ dynamic ] TBool "is_int")
      | "string?" -> Ok (static_function [ dynamic ] TBool "is_string")
      | "boolean?" -> Ok (static_function [ dynamic ] TBool "is_bool")
      | "keyword?" -> Ok (static_function [ dynamic ] TBool "is_keyword")
      | "not" ->
          Ok
            (typed_ir
               (TFn ([ dynamic ], TBool))
               (Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.not_value"))
      | "transient" ->
          let dynamic = Types.dynamic_constraint TUnknown in
          Ok
            (typed_ir
               (TFn ([ dynamic ], dynamic))
               (Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "collection" ],
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident
                          "Lg_runtime.Runtime_dynamic.as_transient",
                        [ Semantic_ir.Ident "collection" ] ) )))
      | "persistent!" ->
          let dynamic = Types.dynamic_constraint TUnknown in
          Ok
            (typed_ir
               (TFn ([ dynamic ], dynamic))
               (Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "collection" ],
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.persistent",
                        [ Semantic_ir.Ident "collection" ] ) )))
      | "pr-writer" ->
          let dynamic = Types.dynamic_constraint TUnknown in
          Ok
            (typed_ir
               (TFn ([ dynamic; TOcaml "Buffer.t"; dynamic ], TUnit))
               (Semantic_ir.Fun
                  ( [
                      Semantic_ir.PVar "value";
                      Semantic_ir.PVar "writer";
                      Semantic_ir.PVar "opts";
                    ],
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Lg_runtime.Runtime_print.write",
                        [
                          Semantic_ir.Ident "writer";
                          Semantic_ir.Apply
                            ( Semantic_ir.Ident
                                "Lg_runtime.Runtime_dynamic.pr_str",
                              [ Semantic_ir.Ident "value" ] );
                        ] ) )))
      | "assoc" ->
          let dynamic = Types.dynamic_constraint TUnknown in
          Ok
            (typed_ir
               (TFn ([ dynamic; dynamic; dynamic ], dynamic))
               (Semantic_ir.Fun
                  ( [
                      Semantic_ir.PVar "collection";
                      Semantic_ir.PVar "key";
                      Semantic_ir.PVar "value";
                    ],
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.assoc",
                        [
                          Semantic_ir.Ident "collection";
                          Semantic_ir.Ident "key";
                          Semantic_ir.Ident "value";
                        ] ) )))
      | "conj" ->
          let dynamic = Types.dynamic_constraint TUnknown in
          Ok
            (typed_ir
               (TFn ([ dynamic; dynamic ], dynamic))
               (Semantic_ir.Fun
                  ( [
                      Semantic_ir.PVar "collection";
                      Semantic_ir.PVar "value";
                    ],
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.conj",
                        [
                          Semantic_ir.Ident "collection";
                          Semantic_ir.Ident "value";
                        ] ) )))
      | _ -> (
          match lookup_regex_function env name with
          | Some expression ->
              Ok (typed_ir (Types.dynamic_constraint TUnknown) expression)
          | None -> Error.error ("unknown function " ^ name)))

let record_constructor_type scope env name =
  if String.ends_with ~suffix:"." name then
    let type_name = String.sub name 0 (String.length name - 1) in
    match Resolver.lookup_record_type scope env type_name with
    | Ok record ->
        Some
          (TFn
             ( List.map (fun (field : field) -> field.ty) record.fields,
               TNamed_record record ))
    | Error _ -> None
  else None

let map_record_constructor_type scope env name =
  if String.starts_with ~prefix:"map->" name then
    let type_name = String.sub name 5 (String.length name - 5) in
    match Resolver.lookup_record_type scope env type_name with
    | Ok record ->
        Some (TFn ([ TRecord record.fields ], TNamed_record record))
    | Error _ -> None
  else None

let dynamic_key_record_type env expected_field_ty =
  let expected_field_ty =
    match Types.dynamic_constraint_info expected_field_ty with
    | Some capability when not (Types.equal capability TUnknown) -> capability
    | Some _ | None -> expected_field_ty
  in
  let expected_field_ty = Types.constraint_value_type expected_field_ty in
  let registered_records =
    Type_registry.bindings (Env.types env)
    |> List.filter_map
         (fun (_, (declaration : Type_registry.declaration)) ->
           match declaration.kind with
           | Type_registry.Record ->
               let scope =
                 Type_id.owner declaration.type_id |> String.concat "."
               in
               let key =
                 Resolver.record_type_key scope
                   (Type_id.name declaration.type_id)
               in
               (match Env.find_opt key env with
               | Some { ty = TNamed_record record; _ } -> Some record
               | Some _ | None -> None)
           | Type_registry.Alias | Type_registry.Variant -> None)
  in
  let named_records =
    if registered_records <> [] then registered_records
    else
      Env.filter_map
        (fun key (binding : binding) ->
          if String.starts_with ~prefix:"__record/" key then
            match binding.ty with
            | TNamed_record record -> Some record
            | _ -> None
          else None)
        env
  in
  let record_index =
    let add name record index =
      String_map.update name
        (fun records -> Some (record :: Option.value ~default:[] records))
        index
    in
    List.fold_left
      (fun index record ->
        let index = add record.type_name record index in
        let id_name = Type_id.name record.type_id in
        if id_name = record.type_name then index else add id_name record index)
      String_map.empty named_records
  in
  let resolve_named_application = function
    | TOcaml_app (name, arguments) as ty ->
        let records =
          String_map.find_opt name record_index
          |> Option.value ~default:[]
          |> List.filter_map (fun record ->
                 if
                   List.length record.type_parameters = List.length arguments
                 then
                  let substitutions =
                    List.combine record.type_parameters arguments
                    |> List.filter (fun (parameter, argument) ->
                           argument <> TVar parameter)
                  in
                  Some
                    (Types.substitute_type_variables substitutions
                       (TNamed_record record))
                 else None)
        in
        (match records with [ record ] -> record | [] | _ :: _ :: _ -> ty)
    | ty -> ty
  in
  let rec same_outer_shape expected actual =
    let expected = resolve_named_application expected in
    let actual = resolve_named_application actual in
    match (expected, actual) with
    | TNamed_record expected, TNamed_record actual ->
        Type_id.equal expected.type_id actual.type_id
    | TNamed_record record, TOcaml_app (name, arguments)
    | TOcaml_app (name, arguments), TNamed_record record ->
        (name = record.type_name || name = Type_id.name record.type_id)
        && List.length arguments = List.length record.type_parameters
    | TArray expected, TArray actual
    | TList expected, TList actual
    | TVector expected, TVector actual
    | TSet expected, TSet actual
    | TSeq expected, TSeq actual
    | TRef expected, TRef actual
    | TNullable expected, TNullable actual ->
        same_outer_shape expected actual
    | TOcaml_app (expected_name, expected_args),
      TOcaml_app (actual_name, actual_args)
      when expected_name = actual_name
           && List.length expected_args = List.length actual_args ->
        List.for_all2 same_outer_shape expected_args actual_args
    | TRecord expected, TNamed_record actual ->
        record_shape expected actual.fields
    | TNamed_record expected, TRecord actual ->
        record_shape expected.fields actual
    | TRecord expected, TRecord actual -> record_shape expected actual
    | TUnknown, _ | TVar _, _ | _, TUnknown | _, TVar _ -> true
    | expected, actual -> Types.equal expected actual
  and record_shape expected actual =
    List.for_all
      (fun (expected : field) ->
        match Types.find_field expected.keyword actual with
        | Some actual -> same_outer_shape expected.ty actual.ty
        | None -> false)
      expected
  in
  let compatible_fields (record : named_record) =
    record.fields
    |> List.filter (fun (field : field) ->
           (not (Types.is_record_extension_field field))
           && same_outer_shape expected_field_ty field.ty)
  in
  let specialize_record (record : named_record) =
    match compatible_fields record with
    | [] -> None
    | (first : field) :: rest -> (
        let first_ty = resolve_named_application first.ty in
        let substitutions =
          rest
          |> List.fold_left
               (fun substitutions (field : field) ->
                 Result.bind substitutions (fun substitutions ->
                     Type_solver.unify substitutions first_ty
                       (resolve_named_application field.ty)))
               (Ok [])
        in
        let substitutions =
          Result.bind substitutions (fun substitutions ->
              Type_solver.unify substitutions first_ty
                (resolve_named_application expected_field_ty))
        in
        match substitutions with
        | Ok substitutions -> (
            match Type_solver.apply substitutions (TNamed_record record) with
            | TNamed_record record -> Some record
            | _ -> None)
        | Error _ -> None)
  in
  let records =
    named_records
    |> List.filter_map (fun record ->
           if List.length (compatible_fields record) >= 2 then
             specialize_record record
           else None)
      |> List.sort_uniq (fun left right ->
           Type_id.compare left.type_id right.type_id)
    in
    match records with
    | [ record ] -> Some (TNamed_record record)
  | [] | _ :: _ :: _ -> None

let lookup_function_ty scope env name =
  match lookup_function scope env name with
  | Ok fn -> Ok fn.ty
  | Error _ -> (
      match record_constructor_type scope env name with
      | Some ty -> Ok ty
      | None -> (
          match map_record_constructor_type scope env name with
          | Some ty -> Ok ty
          | None -> (
              match Protocol.lookup_marker scope env name with
              | Some
                  {
                    protocol_id = Some protocol_id;
                    ty = TFn (_ :: rest, return_ty);
                    _;
                  } -> (
                  match
                    Protocol.constraint_type scope env
                      (Protocol_id.to_string protocol_id)
                  with
                  | Some receiver_ty ->
                      let rest =
                        List.map
                          (function
                            | TUnknown | TVar _ ->
                                Types.dynamic_constraint TUnknown
                            | ty -> ty)
                          rest
                      in
                      Ok (TFn (receiver_ty :: rest, return_ty))
                  | None -> Error.error ("unknown function " ^ name))
              | Some marker -> Ok marker.ty
              | None -> Error.error ("unknown function " ^ name))))

let ocaml_call_target = Resolver.ocaml_call_target
let resolve_ocaml_call_target = Resolver.resolve_ocaml_call_target
let resolve_ocaml_constructor_target = Resolver.resolve_ocaml_constructor_target

let inherit_scope_ocaml_value_refers scope module_path env =
  let prefix = scope ^ "/" in
  let prefix_len = String.length prefix in
  let inherited =
    Env.filter_map
      (fun key (binding : binding) ->
           match binding.host_reference with
        | Some (Ocaml_value _)
          when String.length key > prefix_len
               && String.sub key 0 prefix_len = prefix ->
               let name =
                 String.sub key prefix_len (String.length key - prefix_len)
               in
               Some (Names.scoped_key module_path name, binding)
        | _ -> None)
      env
  in
  Env.add_bindings inherited env

type compiled_fn_parts = {
  param_bindings : (string * binding) list;
  param_identities : (Source_node_id.t * Location.t) option list;
  destructured_bindings : Destructure.local_binding list;
  body : typed_expr;
}

let parameterize_row_fields fields =
  let next_parameter = ref 0 in
  let named_parameters = ref [] in
  let parameters = ref [] in
  let fresh_parameter () =
    let parameter = "a" ^ string_of_int !next_parameter in
    incr next_parameter;
    parameters := parameter :: !parameters;
    parameter
  in
  let named_parameter name =
    match List.assoc_opt name !named_parameters with
    | Some parameter -> parameter
    | None ->
        let parameter = fresh_parameter () in
        named_parameters := (name, parameter) :: !named_parameters;
        parameter
  in
  let rec parameterize = function
    | TUnknown -> TVar (fresh_parameter ())
    | TVar name -> TVar (named_parameter name)
    | TNullable ty -> TNullable (parameterize ty)
    | TOcaml_app (name, arguments) ->
        TOcaml_app (name, List.map parameterize arguments)
    | TTuple items -> TTuple (List.map parameterize items)
    | TArray ty -> TArray (parameterize ty)
    | TRef ty -> TRef (parameterize ty)
    | TList ty -> TList (parameterize ty)
    | TVector ty -> TVector (parameterize ty)
    | TSet ty -> TSet (parameterize ty)
    | TSeq ty -> TSeq (parameterize ty)
    | TFn (parameters, return_type) ->
        TFn (List.map parameterize parameters, parameterize return_type)
    | TOverloaded_fn arities ->
        TOverloaded_fn
          (List.map
             (fun (arity : fn_arity) ->
               { fixed_params = List.map parameterize arity.fixed_params;
                 rest_param = Option.map parameterize arity.rest_param;
                 return_ty = parameterize arity.return_ty })
             arities)
    | TRecord _ -> TVar (fresh_parameter ())
    | TNamed_record record ->
        TNamed_record
          {
            record with
            type_parameters =
              List.map named_parameter record.type_parameters;
            type_arguments = List.map parameterize record.type_arguments;
            fields = List.map parameterize_field record.fields;
          }
    | (TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol
      | TKeyword | TBool | TUnit | TNil | TOcaml _) as ty ->
        ty
  and parameterize_field (field : field) =
    { field with ty = parameterize field.ty }
  in
  let fields = List.map parameterize_field fields in
  (fields, List.rev !parameters)

let row_param_fields ?(allow_nullable = false) = function
  | TRecord fields -> Some fields
  | TNullable (TRecord fields) when allow_nullable -> Some fields
  | TOcaml_app (constraint_name, [ TRecord fields; _ ])
    when constraint_name = Types.seqable_constraint_name
         || constraint_name = Types.optional_seqable_constraint_name
         || constraint_name = Types.optional_sequential_constraint_name ->
      Some fields
  | _ -> None

let row_param_type_names ?(nullable_row_indices = []) prefix param_tys =
  param_tys
  |> List.mapi (fun index param_ty ->
       match
         row_param_fields ~allow_nullable:(List.mem index nullable_row_indices)
           param_ty
       with
       | Some fields ->
           let type_name = prefix ^ "_row" ^ string_of_int index in
           let _, parameters = parameterize_row_fields fields in
           let parameters = List.map (fun name -> "'" ^ name) parameters in
           let applied_name =
             match parameters with
             | [] -> type_name
             | [ parameter ] -> parameter ^ " " ^ type_name
          | parameters -> "(" ^ String.concat ", " parameters ^ ") " ^ type_name
           in
           Some applied_name
       | None -> None)

let row_type_items row_type_names param_tys =
  List.map2
    (fun row_type_name param_ty ->
      match (row_type_name, row_param_fields ~allow_nullable:true param_ty) with
      | Some applied_name, Some fields ->
          let type_name =
            match String.rindex_opt applied_name ' ' with
            | None -> applied_name
            | Some index ->
                String.sub applied_name (index + 1)
                  (String.length applied_name - index - 1)
          in
          let fields, type_parameters = parameterize_row_fields fields in
          Some
            (Type_def { type_name; type_parameters; fields; location = None })
      | _ -> None)
    row_type_names param_tys
  |> List.filter_map Fun.id

let row_call_type_name type_name =
  let length = String.length type_name in
  let buffer = Buffer.create length in
  let is_type_variable_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let rec copy index =
    if index < length then
      if type_name.[index] = '\'' then (
        Buffer.add_char buffer '_';
        skip_variable (index + 1))
      else (
        Buffer.add_char buffer type_name.[index];
        copy (index + 1))
  and skip_variable index =
    if index < length && is_type_variable_char type_name.[index] then
      skip_variable (index + 1)
    else copy index
  in
  copy 0;
  Buffer.contents buffer

let row_project_expr type_name fields arg =
  let type_name = row_call_type_name type_name in
  let source = "__row_source" in
  Semantic_ir.Let
    ( [ (Semantic_ir.PVar source, arg.semantic_expr) ],
      Semantic_ir.Record
        ( List.map
            (fun (field : field) ->
              ( field.ocaml_name,
                Semantic_ir.Field (Semantic_ir.Ident source, field.ocaml_name)
              ))
            fields,
          Some type_name ) )

let row_arg_expr row_type_name expected_ty arg =
  match (row_type_name, expected_ty, arg.ty) with
  | Some type_name, TRecord fields, (TRecord _ | TNamed_record _) ->
      row_project_expr type_name fields arg
  | _ -> arg.semantic_expr

let coerce_set_element element_ty value =
  match element_ty with
  | TNamed_record expected -> (
      match value.ty with
      | TNamed_record actual when actual.type_name = expected.type_name ->
          Ok value.semantic_expr
      | (TRecord actual_fields | TNamed_record { fields = actual_fields; _ })
        when Types.assignable ~policy:Structural ~expected:element_ty
               ~actual:value.ty ->
          let rec project_fields acc = function
            | [] -> Ok (List.rev acc)
            | (field : field) :: rest -> (
                match find_field field.keyword actual_fields with
                | None -> Error.error "set record coercion is missing a field"
                | Some actual_field ->
                    project_fields
                      (( field.ocaml_name,
                         Structural_map.field_expr value actual_field )
                      :: acc)
                      rest)
          in
          project_fields [] expected.fields
          |> Result.map (fun fields ->
              Semantic_ir.Record
                ( fields,
                  Some
                    (record_type_application expected.type_name
                       expected.type_arguments) ))
      | _ -> Error.error "set value type must match record element type")
  | _ ->
      if Types.equal element_ty value.ty then Ok value.semantic_expr
      else Error.error "set value type must match element type"

let constrain_record_function_argument_expr fn element_ty =
  let rec constrain_pattern type_name = function
    | Semantic_ir.PVar name ->
        Some (Semantic_ir.PConstraint (Semantic_ir.PVar name, type_name))
    | Semantic_ir.PLocated (node_id, location, pattern) ->
        constrain_pattern type_name pattern
        |> Option.map (fun pattern ->
               Semantic_ir.PLocated (node_id, location, pattern))
    | _ -> None
  in
  match (Semantic_ir.unlocated fn.semantic_expr, element_ty) with
  | Semantic_ir.Fun ([ pattern ], body), TNamed_record record -> (
      match
        constrain_pattern
          (record_type_application record.type_name record.type_arguments)
          pattern
      with
      | Some pattern -> Semantic_ir.Fun ([ pattern ], body)
      | None -> fn.semantic_expr)
  | _ -> fn.semantic_expr

let param_constraint_name = function
  | TOcaml_app (name, [ _; _ ]) when name = Types.seqable_constraint_name -> None
  | (TInt | TFloat | TChar | TString | TSymbol | TKeyword | TBool | TUnit
    | TArray _ | TRef _ | TOcaml _ | TOcaml_app _ | TTuple _ | TNamed_record _) as ty ->
      Some (Types.ocaml_name ty)
  | _ -> None
