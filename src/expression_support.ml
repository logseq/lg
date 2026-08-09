open Types
open Lowered
module Env = Compiler_environment
module String_map = Map.Make (String)

let is_identity_expr name expression =
  match Semantic_ir.unlocated expression with
  | Semantic_ir.Ident candidate -> String.equal candidate name
  | _ -> false

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
  | ty when Option.is_some (Types.truthy_constraint_info ty) ->
      (match Semantic_ir.unlocated expression with
      | Semantic_ir.Ident name ->
          Semantic_ir.Apply
            (Semantic_ir.Ident (name ^ "__truthy"), [ expression ])
      | _ ->
          Semantic_ir.Apply
            ( Semantic_ir.Apply
                (Semantic_ir.Ident "fst", [ expression ]),
              [ Semantic_ir.Apply (Semantic_ir.Ident "snd", [ expression ]) ] ))
  | TBool -> expression
  | TNil -> Semantic_ir.Sequence [ expression; Semantic_ir.Bool false ]
  | TNullable payload_ty ->
      let truthy_payload =
        let payload = Semantic_ir.Ident "truthy_value" in
        match Types.truthy_constraint_info payload_ty with
        | Some _ ->
            Semantic_ir.Apply
              ( Semantic_ir.Apply
                  (Semantic_ir.Ident "fst", [ payload ]),
                [ Semantic_ir.Apply (Semantic_ir.Ident "snd", [ payload ]) ] )
        | None -> truthiness_expression payload_ty payload
      in
      Semantic_ir.Match
        ( expression,
          [
            (Semantic_ir.PConstructor ("None", None), Semantic_ir.Bool false);
            ( Semantic_ir.PConstructor
                ("Some", Some (Semantic_ir.PVar "truthy_value")),
              truthy_payload );
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

let truthiness_needs_value ty =
  Types.is_dynamic ty
  || Option.is_some (Types.truthy_constraint_info ty)
  ||
  match ty with
  | TBool | TNullable _ | TOcaml_app ("option", [ _ ]) | TOcaml "option"
  | TSeq _ ->
      true
  | TOcaml_app (name, [ _ ]) -> name = Types.next_seq_type_name
  | _ -> false

let nil_predicate_needs_value ty =
  Option.is_some (Types.nil_predicate_constraint_info ty)
  || Types.is_dynamic ty
  ||
  match Types.seqable_constraint_info ty with
  | Some ((`Optional | `Optional_sequential), _, _) -> true
  | Some (`Required, _, _) -> false
  | None -> (
      match ty with
      | TNullable _ | TOcaml_app ("option", [ _ ])
      | TOcaml "Lg_edn_backend.t" ->
          true
      | TOcaml_app (name, [ _ ]) -> name = Types.next_seq_type_name
      | _ -> false)

let rec nil_predicate_expression ty expression =
  match Types.nil_predicate_constraint_info ty with
  | Some _ -> (
      match Semantic_ir.unlocated expression with
      | Semantic_ir.Ident name ->
          Semantic_ir.Apply
            (Semantic_ir.Ident (name ^ "__nil"), [ expression ])
      | _ ->
          Semantic_ir.Apply
            ( Semantic_ir.Apply
                (Semantic_ir.Ident "fst", [ expression ]),
              [ Semantic_ir.Apply (Semantic_ir.Ident "snd", [ expression ]) ] ))
  | None -> (
      match Types.seqable_constraint_info ty with
      | Some ((`Optional | `Optional_sequential), _, value_ty) ->
          let value =
            match Semantic_ir.unlocated expression with
            | Semantic_ir.Ident _ -> expression
            | _ ->
                Semantic_ir.Apply
                  (Semantic_ir.Ident "snd", [ expression ])
          in
          nil_predicate_expression value_ty value
      | Some (`Required, _, _) | None -> (
      match ty with
      | ty when Types.is_dynamic ty ->
          Semantic_ir.Apply
            (Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.is_nil", [ expression ])
      | TNil -> Semantic_ir.Sequence [ expression; Semantic_ir.Bool true ]
      | TNullable payload_ty | TOcaml_app ("option", [ payload_ty ]) ->
          let value_name = "__lg_optional_nil_value" in
          let value = Semantic_ir.Ident value_name in
          let payload_pattern, payload_is_nil =
            if nil_predicate_needs_value payload_ty then
              let payload_is_nil =
                match Types.nil_predicate_constraint_info payload_ty with
                | Some _ ->
                    Semantic_ir.Apply
                      ( Semantic_ir.Apply
                          (Semantic_ir.Ident "fst", [ value ]),
                        [
                          Semantic_ir.Apply
                            (Semantic_ir.Ident "snd", [ value ]);
                        ] )
                | None -> nil_predicate_expression payload_ty value
              in
              (Semantic_ir.PVar value_name, payload_is_nil)
            else
              ( Semantic_ir.PAny,
                Semantic_ir.Bool (Types.equal payload_ty TNil) )
          in
          Semantic_ir.Match
            ( expression,
              [
                (Semantic_ir.PConstructor ("None", None), Semantic_ir.Bool true);
                ( Semantic_ir.PConstructor
                    ("Some", Some payload_pattern),
                  payload_is_nil );
              ] )
      | TOcaml "Lg_edn_backend.t" ->
          Semantic_ir.Apply
            (Semantic_ir.Ident "Lg_runtime.Runtime_edn.is_nil", [ expression ])
      | TOcaml_app (name, [ _ ]) when name = Types.next_seq_type_name ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.is_empty",
              [ expression ] )
      | _ -> Semantic_ir.Sequence [ expression; Semantic_ir.Bool false ]))

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
     | TUnknown | TMeta _ | TVar _ ->
         List.mem name [ "Some"; "None"; "Ok"; "Error" ]
         || String.contains name '.' || String.contains name '/'
  | _ -> false

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
    | TRecord left_fields, TRecord right_fields
      when List.length left_fields = List.length right_fields ->
        let rec merge_fields merged = function
          | [] -> Some (TRecord (List.rev merged))
          | (left_field : field) :: rest -> (
              match Types.find_field left_field.keyword right_fields with
              | None -> None
              | Some right_field ->
                  Option.bind
                    (merge_branch_types left_field.ty right_field.ty)
                    (fun ty ->
                      merge_fields ({ left_field with ty } :: merged) rest))
        in
        merge_fields [] left_fields
    | TOcaml_app (left_name, left_args), TOcaml_app (right_name, right_args)
      when left_name = right_name
           && List.length left_args = List.length right_args ->
        let rec merge_arguments merged left right =
          match (left, right) with
          | [], [] ->
              Some (TOcaml_app (left_name, List.rev merged))
          | left :: left_rest, right :: right_rest ->
              Option.bind (merge_branch_types left right) (fun ty ->
                  merge_arguments (ty :: merged) left_rest right_rest)
          | _ -> None
        in
        merge_arguments [] left_args right_args
    | TRecord _, (TNamed_record _ as named)
      when Types.row_compatible ~expected:left ~actual:named ->
        Some named
    | (TNamed_record _ as named), TRecord _
      when Types.row_compatible ~expected:right ~actual:named ->
        Some named
    | (TRecord _ as structural), TNamed_record _
      when Types.row_compatible ~expected:structural ~actual:right ->
        Some structural
    | TNamed_record _, (TRecord _ as structural)
      when Types.row_compatible ~expected:structural ~actual:left ->
        Some structural
    | left, right when protocol_has_value right left ->
        Some right
    | left, right when protocol_has_value left right ->
        Some left
    | left, right
      when Option.is_some (protocol_value_type right) ->
        merge_branch_types left (Option.get (protocol_value_type right))
    | left, right
      when Option.is_some (protocol_value_type left) ->
        merge_branch_types (Option.get (protocol_value_type left)) right
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
    | TOcaml_app ("option", [ inner ]), ty
    | ty, TOcaml_app ("option", [ inner ]) ->
        Option.map
          (fun merged -> TOcaml_app ("option", [ merged ]))
          (merge_branch_types inner ty)
    | TFn (left_params, left_return), TFn (right_params, right_return)
      when List.length left_params = List.length right_params ->
        let merge_parameter left right =
          match (left, right) with
          | TArray (TUnknown | TMeta _ | TVar _), (TUnknown | TMeta _ | TVar _)
          | (TUnknown | TMeta _ | TVar _), TArray (TUnknown | TMeta _ | TVar _) ->
              Some (TArray (Types.dynamic_constraint TUnknown))
          | _ when Types.equal left right -> Some left
          | _ -> merge_branch_types left right
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
    | TList left, TList right ->
        Option.map (fun inner -> TList inner) (merge_branch_types left right)
    | TVector left, TVector right ->
        Option.map (fun inner -> TVector inner) (merge_branch_types left right)
    | TSet left, TSet right ->
        Option.map (fun inner -> TSet inner) (merge_branch_types left right)
    | TSeq left, TSeq right ->
        Option.map (fun inner -> TSeq inner) (merge_branch_types left right)
    | TArray left, TArray right ->
        let merged_element =
          match (left, right) with
          | (TUnknown | TMeta _ | TVar _), ty | ty, (TUnknown | TMeta _ | TVar _) -> Some ty
          | _ -> merge_branch_types left right
        in
        Option.map (fun inner -> TArray inner) merged_element
    | TVar _, TVar _ -> Some left
    | TVar _, ty | ty, TVar _ -> Some ty
    | TMeta _, _ | _, TMeta _ -> (
        match Type_solver.unify [] left right with
        | Ok substitutions -> Some (Type_solver.apply substitutions left)
        | Error _ -> None)
    | TUnknown, ty | ty, TUnknown -> Some ty
    | _ when Types.defer_to_ocaml ~expected:left ~actual:right -> Some left
    | _ -> None

let branch_types_compatible left right =
  Option.is_some (merge_branch_types left right)

let heterogeneous_collection_type_error collection types =
  let types =
    types |> List.map Types.source_name |> List.sort_uniq String.compare
  in
  Error.error
    ("heterogeneous " ^ collection
   ^ (if collection = "map keys" || collection = "map values" then
        " have types "
      else " has element types ")
   ^ String.concat " | " types
   ^ "; define a sum type containing these types")

let heterogeneous_collection_error collection values =
  heterogeneous_collection_type_error collection
    (List.map (fun value -> value.ty) values)

let merge_collection_types collection types =
  match types with
  | [] -> Ok TUnknown
  | first :: rest ->
      let merged =
        List.fold_left
          (fun merged ty ->
            Option.bind merged (fun merged -> merge_branch_types merged ty))
          (Some first) rest
      in
      (match merged with
      | Some ty when not (Types.contains_dynamic ty) -> Ok ty
      | Some _ | None -> heterogeneous_collection_type_error collection types)

let merge_collection_value_types collection values =
  merge_collection_types collection (List.map (fun value -> value.ty) values)

let set_module_name env ty =
  match Types.set_module_name ty with
  | Ok _ as set_module -> set_module
  | Error _ as error ->
      let emitted_name =
        match ty with
        | TOcaml name | TOcaml_app (name, _) -> Some name
        | _ -> None
      in
      (match
         Option.bind emitted_name (fun name ->
             Type_registry.find_by_emitted_name name
               (Compiler_environment.types env))
       with
      | Some { kind = Type_registry.Variant; _ } ->
          Ok "Lg_runtime.Runtime_poly_set"
      | Some _ | None -> error)

let capability_storage_expression ty expression =
  let rec build name = function
    | ty when Types.is_dynamic ty -> Semantic_ir.Ident name
    | ty ->
        let layer witness_name value_ty =
          Semantic_ir.Tuple
            [ Semantic_ir.Ident witness_name; build name value_ty ]
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
                    | Some value_ty -> layer (name ^ "__print") value_ty
                    | None -> (
                        match Types.symbol_predicate_constraint_info ty with
                        | Some value_ty -> layer (name ^ "__symbol") value_ty
                        | None -> (
                            match Types.contains_constraint_info ty with
                            | Some (_, value_ty) ->
                                layer (name ^ "__contains") value_ty
                            | None -> (
                                match ty with
                                | TOcaml_app
                                    ( constraint_name,
                                      [ _element_ty; value_ty ] )
                                  when constraint_name
                                       = Types.seqable_constraint_name
                                       || constraint_name
                                          = Types.optional_seqable_constraint_name
                                       || constraint_name
                                          = Types.optional_sequential_constraint_name
                                  ->
                                    let witness_name =
                                      if
                                        constraint_name
                                        = Types.seqable_constraint_name
                                      then name ^ "__seq"
                                      else name ^ "__seq_optional"
                                    in
                                    layer witness_name value_ty
                                | _ -> Semantic_ir.Ident name))))))
  in
  match Semantic_ir.unlocated expression with
  | Semantic_ir.Ident name -> build name ty
  | _ -> expression

let pack_plain_dynamic_value value =
  if
    Types.is_dynamic value.ty
    || match value.ty with TUnknown | TMeta _ | TVar _ -> true | _ -> false
  then
      Some
        (Semantic_ir.PackDynamic
           {
             source_ty = value.ty;
             target_ty = Types.dynamic_constraint value.ty;
             conversion = value.semantic_expr;
           })
  else None

let coerce_expression_to_type ?(stored = false) target_ty source_ty expression =
  match (target_ty, source_ty) with
  | target_ty, source_ty when protocol_has_value target_ty source_ty ->
      let rec unwrap ty expression =
        let unwrap_stored value_ty =
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
        match Types.protocol_constraint_info ty with
        | Some (_, _, value_ty) -> unwrap_stored value_ty
        | None -> (
            match Types.truthy_constraint_info ty with
            | Some value_ty -> unwrap_stored value_ty
            | None -> (
                match Types.printable_constraint_info ty with
                | Some value_ty -> unwrap_stored value_ty
                | None -> (
                    match Types.symbol_predicate_constraint_info ty with
                    | Some value_ty -> unwrap_stored value_ty
                    | None -> expression)))
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
      let _ = (target_inner, source_inner) in
      sequence
  | TSet target_inner, TSet (TUnknown | TMeta _ | TVar _) -> (
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
  | TSet (TUnknown | TMeta _ | TVar _), TSet source_inner -> (
      match Types.set_module_name source_inner with
      | Ok set_module ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Lg_runtime.Runtime_poly_set.of_list",
              [
                Semantic_ir.Apply
                  ( Semantic_ir.Ident (set_module ^ ".elements"),
                    [ expression ] );
              ] )
      | Error _ -> expression)
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
      TOcaml_app (name, [ _ ]) )
    when name = Types.next_seq_type_name ->
      let sequence_name = "__lg_nullable_next_sequence" in
      let sequence = Semantic_ir.Ident sequence_name in
      let _ = target in
      let present = sequence in
      Semantic_ir.Let
        ( [ (Semantic_ir.PVar sequence_name, expression) ],
          Semantic_ir.If
            ( Semantic_ir.Apply
                ( Semantic_ir.Ident "Lg_runtime.Runtime_seq.is_empty",
                  [ sequence ] ),
              Semantic_ir.Constructor ("None", None),
              Semantic_ir.Constructor ("Some", Some present) ) )
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
  | TNullable _, _ ->
      Semantic_ir.Constructor
        ("Some", Some (Semantic_ir.annotate source_ty expression))
  | TOcaml_app ("option", [ _ ]), TNil -> expression
  | TOcaml_app ("option", [ _ ]), TNullable _ -> expression
  | TOcaml_app ("option", [ _ ]), TOcaml_app ("option", [ _ ]) -> expression
  | TOcaml_app ("option", [ _ ]), _ ->
      Semantic_ir.Constructor
        ("Some", Some (Semantic_ir.annotate source_ty expression))
  | _ -> expression

let merge_branch_expressions left right =
  let merge_tuple_items left_types left_items right_types right_items =
    let rec merge types left_values right_values =
      match (types, left_values, right_values) with
      | [], [], [] -> Some ([], [], [])
      | ( (left_ty, right_ty) :: types,
          left_value :: left_values,
          right_value :: right_values ) ->
          let merged_ty = merge_branch_types left_ty right_ty in
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
  | TOcaml "int64" as ty -> ty
  | TOcaml "float" -> TFloat
  | TOcaml "char" -> TChar
  | TOcaml "string" -> TString
  | TOcaml "bool" -> TBool
  | TOcaml "unit" -> TUnit
  | ty -> ty

let rec lg_metadata_type_for_ocaml_type = function
  | TOcaml "int64" as ty -> ty
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
  let type_name = Types.ocaml_record_type_name type_name in
  let argument_name = function
    | TUnknown | TMeta _ | TVar _ -> "_"
    | argument -> Types.ocaml_type_argument_name argument
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
  let never_returns =
    match Semantic_ir.unlocated expr.semantic_expr with
    | Semantic_ir.Fun (_, body) -> Semantic_ir.never_returns body
    | _ -> false
  in
  let binding =
    Types.binding ~row_param_types ?return_param_index:expr.return_param_index
      ~never_returns ocaml_name expr.ty
  in
  {
    binding with
    ty = Types.align_deferred_param_types binding.ty expr.semantic_expr;
  }
  |> Types.generalize_binding

type anonymous_record_allocation = {
  record : named_record;
  env : Env.t;
  next_type : int;
  fresh : bool;
}

let anonymous_record_type_parameters fields =
  let parameters = ref [] in
  let add name =
    if not (List.mem name !parameters) then parameters := !parameters @ [ name ]
  in
  let rec visit = function
    | TUnknown | TMeta _ | TNil -> add "a"
    | TVar name -> add name
    | TNullable ty | TArray ty | TRef ty | TList ty | TVector ty | TSet ty
    | TSeq ty ->
        visit ty
    | TOcaml_app (_, arguments) | TTuple arguments -> List.iter visit arguments
    | TFn (parameters, return_ty) -> List.iter visit (return_ty :: parameters)
    | TOverloaded_fn arities ->
        List.iter
          (fun (arity : fn_arity) ->
            List.iter visit arity.fixed_params;
            Option.iter visit arity.rest_param;
            visit arity.return_ty)
          arities
    | TRecord fields ->
        List.iter (fun (field : field) -> visit field.ty) fields
    | TNamed_record record -> List.iter visit record.type_arguments
    | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol
    | TKeyword | TBool | TUnit | TOcaml _ ->
        ()
  in
  List.iter (fun (field : field) -> visit field.ty) fields;
  !parameters

let allocate_anonymous_record ~owner env next_type fields =
  let owner = Source_context.anonymous_record_owner owner in
  let existing =
    match Env.find_anonymous_record ~owner fields env with
    | Some _ as record -> record
    | None -> Env.find_anonymous_record_by_layout ~owner fields env
  in
  match existing with
  | Some record -> { record; env; next_type; fresh = false }
  | None ->
      let type_name = "t" ^ string_of_int next_type in
      let set_module_name = "Set_" ^ type_name in
      let type_id =
        Type_id.create
          ~owner:(if String.equal owner "" then [] else [ owner ])
          ~name:type_name
      in
      let record =
        match
          Types.named_record ~type_id ~extensible:true ~type_name ~set_module_name
            ~type_parameters:(anonymous_record_type_parameters fields)
            fields
        with
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
                    type_id = record.record.type_id;
                    type_name = record.record.type_name;
                    type_parameters = record.record.type_parameters;
                    fields = record.record.fields;
                    nominal = false;
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
      | TKeyword | TBool | TUnit | TNil | TUnknown | TMeta _ | TVar _ | TOcaml _
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

let binding_value_expression (binding : Types.binding) =
  let rec overloaded_value = function
    | [] -> Semantic_ir.Unit
    | target :: rest ->
        Semantic_ir.Tuple
          [ Semantic_ir.Ident target; overloaded_value rest ]
  in
  match binding.ty with
  | TOverloaded_fn arities
    when List.length binding.overload_targets = List.length arities ->
      overloaded_value binding.overload_targets
  | _ -> Semantic_ir.Ident binding.ocaml_name

let untyped_first_class_collection_function_error name =
  name
  ^ " cannot be used as an untyped first-class function; define a statically \
     typed wrapper"

let untyped_first_class_function_error = function
  | ( "!="
    | "="
    | "=="
    | "array-map"
    | "assoc"
    | "compare"
    | "conj"
    | "contains?"
    | "count"
    | "dissoc"
    | "false?"
    | "get"
    | "hash-map"
    | "identical?"
    | "keyword"
    | "keyword?"
    | "list"
    | "meta"
    | "name"
    | "nil?"
    | "namespace"
    | "not-empty"
    | "number?"
    | "pr-str"
    | "pr-writer"
    | "print-str"
    | "println-str"
    | "prn-str"
    | "persistent!"
    | "rand"
    | "re-find"
    | "re-matches"
    | "re-pattern"
    | "re-seq"
    | "set"
    | "str"
    | "string?"
    | "symbol?"
    | "transient"
    | "true?"
    | "update"
    | "vec"
    | "vector" ) as name ->
      Some (untyped_first_class_collection_function_error name)
  | ( "clojure.core/dissoc"
    | "cljs.core/dissoc"
    | "clojure.core/update"
    | "cljs.core/update" ) as name ->
      let separator = String.rindex name '/' in
      let basename =
        String.sub name (separator + 1) (String.length name - separator - 1)
      in
      Some (untyped_first_class_collection_function_error basename)
  | ("resolve" | "requiring-resolve") as name ->
      Some
        (name
        ^ " cannot be used without a closed result type; define a closed sum \
           type containing the supported Vars")
  | _ -> None

let lookup_function scope env name =
  let static_int_function parameter_tys return_ty runtime_name =
    typed_ir
      (TFn (parameter_tys, return_ty))
      (Semantic_ir.Ident ("Lg_runtime.Runtime_int." ^ runtime_name))
  in
  let static_int_comparison operator =
    typed_ir
      (TFn ([ TInt; TInt ], TBool))
      (Semantic_ir.Fun
         ( [ Semantic_ir.PVar "a"; Semantic_ir.PVar "b" ],
           Semantic_ir.Infix
             (operator, Semantic_ir.Ident "a", Semantic_ir.Ident "b") ))
  in
  match lookup_binding scope env name with
  | Ok binding ->
      Ok (typed_ir binding.ty (binding_value_expression binding))
  | Error _ -> (
      match untyped_first_class_function_error name with
      | Some message -> Error.error message
      | None -> (
      match name with
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
      | ("<" | "<=" | ">" | ">=") as operator ->
          Ok (static_int_comparison operator)
      | "max" -> Ok (static_int_function [ TInt; TInt ] TInt "int_max")
      | "min" -> Ok (static_int_function [ TInt; TInt ] TInt "int_min")
      | "zero?" -> Ok (static_int_function [ TInt ] TBool "int_zero")
      | "pos?" -> Ok (static_int_function [ TInt ] TBool "int_positive")
      | "neg?" -> Ok (static_int_function [ TInt ] TBool "int_negative")
      | _ -> Error.error ("unknown function " ^ name)))

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

let map_record_constructor_type_name name =
  let owner, basename =
    match String.rindex_opt name '/' with
    | None -> ("", name)
    | Some index ->
        ( String.sub name 0 (index + 1),
          String.sub name (index + 1) (String.length name - index - 1) )
  in
  if String.starts_with ~prefix:"map->" basename then
    Some
      (owner ^ String.sub basename 5 (String.length basename - 5))
  else None

let map_record_constructor_type scope env name =
  match map_record_constructor_type_name name with
  | Some type_name -> (
      match Resolver.lookup_record_type scope env type_name with
      | Ok record ->
          Some (TFn ([ TRecord record.fields ], TNamed_record record))
      | Error _ -> None)
  | None -> None

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
      Env.filter_record_bindings
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
                    |> List.map (fun (parameter, argument) ->
                           (Type_solver.Declared parameter, argument))
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
  | Error original_error -> (
      match Resolver.ocaml_call_target scope env name with
      | Some target -> (
          match Ocaml_signature.value_signature target with
          | Ok signature ->
              let rec clj_function_type = function
                | TFn ([ TUnit ], return_ty) ->
                    TFn ([], clj_function_type return_ty)
                | TFn (parameters, return_ty) ->
                    TFn
                      ( List.map clj_function_type parameters,
                        clj_function_type return_ty )
                | ty -> ty
              in
              let parameters =
                List.map
                  (fun (parameter : Ocaml_signature.parameter) ->
                    clj_function_type parameter.ty)
                  signature.parameters
              in
              let parameters =
                match parameters with [ TUnit ] -> [] | _ -> parameters
              in
              Ok
                (TFn
                   (parameters, clj_function_type signature.return_type))
          | Error _ -> Error original_error)
      | None ->
      match record_constructor_type scope env name with
      | Some ty -> Ok ty
      | None -> (
          match map_record_constructor_type scope env name with
          | Some ty -> Ok ty
          | None -> (
              match Protocol.lookup_marker scope env name with
              | Some
                  {
                    protocol_id = Some _;
                    ty = TFn (receiver_ty :: rest, return_ty);
                    _;
                  } ->
                  Ok (TFn (receiver_ty :: rest, return_ty))
              | Some marker -> Ok marker.ty
              | None
                when is_constructor_name name
                     && not (List.mem name [ "Some"; "None"; "Ok"; "Error" ]) ->
                  let candidates =
                    Env.bindings_named name env
                    |> List.filter_map (fun (binding : binding) ->
                           match binding.ty with
                           | TFn (_, (TOcaml _ | TOcaml_app _)) ->
                               Some binding.ty
                           | _ -> None)
                    |> List.sort_uniq Stdlib.compare
                  in
                  (match candidates with
                  | [ constructor_ty ] -> Ok constructor_ty
                  | [] ->
                      let constructor_name =
                        Resolver.resolve_ocaml_constructor_target scope env name
                      in
                      (match
                         Ocaml_signature.constructor_signature constructor_name
                       with
                      | Ok signature ->
                          Ok
                            (TFn
                               ( signature.payload_types,
                                 signature.result_type ))
                      | Error _ -> Error.error ("unknown function " ^ name))
                  | _ :: _ :: _ -> Error.error ("ambiguous constructor " ^ name))
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
    | TUnknown | TMeta _ -> TVar (fresh_parameter ())
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

let direct_row_fields ?(allow_nullable = false) = function
  | TRecord fields -> Some fields
  | TNullable (TRecord fields) | TOcaml_app ("option", [ TRecord fields ])
    when allow_nullable ->
      Some fields
  | _ -> None

let tuple_row_fields = function
  | TRecord fields | TNamed_record { fields; nominal = false; _ } -> Some fields
  | _ -> None

let row_param_fields ?(allow_nullable = false) = function
  | ty when Option.is_some (direct_row_fields ~allow_nullable ty) ->
      direct_row_fields ~allow_nullable ty
  | ty when Option.is_some (Types.contains_constraint_info ty) ->
      let _, value_ty = Types.contains_constraint_info ty |> Option.get in
      direct_row_fields ~allow_nullable value_ty
  | TOcaml_app (constraint_name, [ element_ty; _ ])
    when constraint_name = Types.seqable_constraint_name
         || constraint_name = Types.optional_seqable_constraint_name
         || constraint_name = Types.optional_sequential_constraint_name ->
      (match direct_row_fields ~allow_nullable element_ty with
      | Some _ as fields -> fields
      | None -> (
          match element_ty with
          | TTuple items ->
              items
              |> List.filter_map tuple_row_fields
              |> (function
                   | [ fields ] -> Some fields
                   | [] | _ :: _ :: _ -> None)
          | _ -> None))
  | _ -> None

let row_param_type_names ?env ?(nullable_row_indices = []) prefix param_tys =
  let has_named_candidate fields =
    match env with
    | None -> false
    | Some env ->
        Env.filter_record_bindings
          (fun key (binding : binding) ->
            if String.starts_with ~prefix:"__record/" key then
              match binding.ty with
              | TNamed_record record
                when Types.row_compatible ~expected:(TRecord fields)
                       ~actual:(TNamed_record record) ->
                  Some ()
              | _ -> None
            else None)
          env
        <> []
  in
  param_tys
  |> List.mapi (fun index param_ty ->
       let named_constraint_row =
         match Types.contains_constraint_info param_ty with
         | Some (_, TNamed_record record) ->
             Some (Structural_map.record_type_application record)
         | Some _ | None -> None
       in
       let nullable_fields =
         match param_ty with
         | TNullable (TRecord fields)
         | TOcaml_app ("option", [ TRecord fields ]) ->
             Some fields
         | _ -> None
       in
       let unresolved_nullable_row =
         Option.fold ~none:false
           ~some:(fun fields -> not (has_named_candidate fields))
           nullable_fields
       in
       match named_constraint_row with
       | Some type_name -> Some type_name
       | None -> (
       match
         row_param_fields
           ~allow_nullable:
             (unresolved_nullable_row || List.mem index nullable_row_indices)
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
       | None -> None))

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
            (Type_def
               {
                 type_id = Types.type_id_of_name type_name;
                 type_name;
                 type_parameters;
                 fields;
                 nominal = false;
                 location = None;
               })
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
      | _ ->
          Error.error
            ("set value type must match record element type: expected "
           ^ Types.source_name element_ty ^ ", got "
            ^ Types.source_name value.ty))
  | _ ->
      if Types.equal element_ty value.ty then Ok value.semantic_expr
      else
        Error.error
          ("set value type must match element type: expected "
         ^ Types.source_name element_ty ^ ", got "
          ^ Types.source_name value.ty)

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
  | Semantic_ir.Fun ([ pattern ], body), TRecord _ ->
      Semantic_ir.Fun ([ Semantic_ir.PTyped (pattern, element_ty) ], body)
  | _ -> fn.semantic_expr

let rec concrete_constraint_type = function
  | ty when Types.is_dynamic ty -> true
  | TUnknown | TMeta _ | TVar _ | TRecord _ | TOverloaded_fn _ -> false
  | TNullable ty | TArray ty | TRef ty | TList ty | TVector ty | TSet ty
  | TSeq ty ->
      concrete_constraint_type ty
  | TOcaml_app (_, arguments) | TTuple arguments ->
      List.for_all concrete_constraint_type arguments
  | TFn (parameters, return_type) ->
      List.for_all concrete_constraint_type (return_type :: parameters)
  | TNamed_record record ->
      List.for_all concrete_constraint_type record.type_arguments
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TOcaml _ ->
      true

let param_constraint_name = function
  | TOcaml_app (name, [ _; _ ]) when name = Types.seqable_constraint_name -> None
  | TFn _ as ty when concrete_constraint_type ty -> Some (Types.ocaml_name ty)
  | (TNullable _ | TList _ | TVector _ | TSet _ | TSeq _) as ty
    when concrete_constraint_type ty ->
      Some (Types.ocaml_name ty)
  | (TInt | TFloat | TChar | TString | TSymbol | TKeyword | TBool | TUnit
    | TArray _ | TRef _ | TOcaml _ | TOcaml_app _ | TTuple _ | TNamed_record _) as ty ->
      Some (Types.ocaml_name ty)
  | _ -> None
