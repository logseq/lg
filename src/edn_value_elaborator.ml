open Types

let value_ty = TOcaml "Lg_edn_backend.t"
let is_value_type ty =
  Types.equal ty value_ty
  ||
  match ty with
  | TOcaml name -> (
      match Ocaml_signature.type_manifest name with
      | Ok manifest -> Types.equal manifest value_ty
      | Error _ -> false)
  | _ -> false
let optional_payload = function
  | TNullable ty | TOcaml_app ("option", [ ty ]) -> Some ty
  | _ -> None

let map_entries_argument expected_element argument =
  if
    is_value_type argument.ty
    && match expected_element with TTuple [ _; _ ] -> true | _ -> false
  then
    typed_ir (TSeq (TTuple [ value_ty; value_ty ]))
      (Semantic_ir.Apply
         ( Semantic_ir.Ident "Lg_runtime.Runtime_edn.map_entries",
           [ argument.semantic_expr ] ))
  else argument

let map_entry_mapper ~pack_constrained expected_element actual_element =
  let rec decode expected expression =
    if Types.equal expected value_ty then Ok expression
    else if Option.is_some (Types.printable_constraint_info expected) then
      pack_constrained expected (typed_ir value_ty expression)
    else
      match expected with
      | TBool ->
          Ok
            (Semantic_ir.Apply
               ( Semantic_ir.Ident "Lg_runtime.Runtime_edn.bool_value",
                 [ expression ] ))
      | expected when Option.is_some (Types.record_fields expected) ->
          let fields = Types.record_fields expected |> Option.get in
          let rec decode_fields decoded = function
            | [] ->
                Ok
                  (Structural_map.record_expr fields (List.rev decoded)
                  |> fun value -> value.semantic_expr)
            | (field : field) :: rest ->
                let found =
                  Semantic_ir.Apply
                    ( Semantic_ir.Ident
                        "Lg_runtime.Runtime_edn.find_keyword",
                      [ Semantic_ir.String field.keyword; expression ] )
                in
                let value_name = "__lg_edn_field_value" in
                let decoded_field =
                  match optional_payload field.ty with
                  | Some inner ->
                      Result.map
                        (fun value ->
                          Semantic_ir.Match
                            ( found,
                              [
                                ( Semantic_ir.PConstructor ("None", None),
                                  Semantic_ir.Constructor ("None", None) );
                                ( Semantic_ir.PConstructor
                                    ( "Some",
                                      Some (Semantic_ir.PVar value_name) ),
                                  Semantic_ir.Constructor
                                    ("Some", Some value) );
                              ] ))
                        (decode inner (Semantic_ir.Ident value_name))
                  | None ->
                      decode field.ty
                        (Semantic_ir.Apply
                           (Semantic_ir.Ident "Option.get", [ found ]))
                in
                Result.bind decoded_field (fun value ->
                    decode_fields ((field, value) :: decoded) rest)
          in
          decode_fields [] fields
      | _ ->
          Error.error
            ("EDN value cannot be decoded as " ^ Types.source_name expected)
  in
  match (actual_element, expected_element) with
  | ( TTuple [ actual_key; actual_value ],
      TTuple [ expected_key; expected_value ] )
    when is_value_type actual_key && is_value_type actual_value ->
      let key_name = "__lg_edn_map_key" in
      let value_name = "__lg_edn_map_value" in
      Result.bind (decode expected_key (Semantic_ir.Ident key_name))
        (fun key ->
          Result.map
            (fun value ->
              Some
                (Semantic_ir.Fun
                   ( [
                       Semantic_ir.PTuple
                         [
                           Semantic_ir.PVar key_name;
                           Semantic_ir.PVar value_name;
                         ];
                     ],
                     Semantic_ir.Tuple [ key; value ] )))
            (decode expected_value (Semantic_ir.Ident value_name)))
  | _ -> Ok None
