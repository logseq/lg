open Types

let apply name args = Semantic_ir.Apply (Semantic_ir.Ident name, args)

let rec to_seq_expr env collection =
  if Types.is_dynamic collection.ty then
    Ok
      ( collection.ty,
        apply "Lg_runtime.Runtime_dynamic.to_seq"
          [ collection.semantic_expr ] )
  else
  match collection.ty with
  | TNullable value_ty ->
      let value_name = "__lg_optional_seqable_value" in
      let value = typed_ir value_ty (Semantic_ir.Ident value_name) in
      (match to_seq_expr env value with
      | Error _ -> Error.error "optional value is not seqable"
      | Ok (element_ty, sequence) ->
          Ok
            ( element_ty,
              Semantic_ir.Match
                ( collection.semantic_expr,
                  [ ( Semantic_ir.PConstructor ("None", None),
                      Semantic_ir.Ident "Seq.empty" );
                    ( Semantic_ir.PConstructor
                        ("Some", Some (Semantic_ir.PVar value_name)),
                      sequence );
                  ] ) ))
  | _ ->
  match Types.next_seq_element collection.ty with
  | Some inner -> Ok (inner, collection.semantic_expr)
  | None ->
  match Types.seqable_constraint_info collection.ty with
  | Some (constraint_kind, inner, _) -> (
      match Semantic_ir.unlocated collection.semantic_expr with
      | Semantic_ir.Ident name ->
          let adapter =
            match constraint_kind with
            | `Required -> Semantic_ir.Ident (name ^ "__seq")
            | `Optional | `Optional_sequential ->
                let adapter_name = "__lg_seqable_adapter" in
                Semantic_ir.Match
                  ( Semantic_ir.Ident (name ^ "__seq_optional"),
                    [ ( Semantic_ir.PConstructor ("None", None),
                        Semantic_ir.Apply
                          ( Semantic_ir.Ident "invalid_arg",
                            [ Semantic_ir.String "value is not sequential" ] ) );
                      ( Semantic_ir.PConstructor
                          ("Some", Some (Semantic_ir.PVar adapter_name)),
                        Semantic_ir.Ident adapter_name );
                    ] )
          in
          Ok
            ( inner,
              Semantic_ir.Apply
                (adapter, [ collection.semantic_expr ]) )
      | _ -> Error.error "constrained Seqable value must be a function parameter")
  | None ->
  match Core_protocols.find_seqable collection.ty (Compiler_environment.protocols env) with
  | None -> Error.error "collection value is not seqable"
  | Some implementation -> (
      match Core_sequence_transform.collection_to_seq_expr collection with
      | Ok sequence -> Ok sequence
      | Error _ -> (
          match implementation.ty with
          | TFn ([ receiver_ty ], TSeq inner)
            when Types.assignable ~policy:Host_boundary ~expected:receiver_ty
                   ~actual:collection.ty ->
              Ok
                ( inner,
                  apply implementation.ocaml_name [ collection.semantic_expr ] )
          | TFn ([ receiver_ty ], TOcaml_app (("Seq.t" | "Seq"), [ inner ]))
            when Types.assignable ~policy:Host_boundary ~expected:receiver_ty
                   ~actual:collection.ty ->
              Ok
                ( inner,
                  apply "Lg_runtime.Runtime_seq.memoize"
                    [ apply implementation.ocaml_name
                        [ collection.semantic_expr ] ] )
          | _ ->
              Error.error
                "Seqable/-seq implementation must return a typed lazy seq"))

let accepts_seqable env ty =
  if Types.is_dynamic ty then true
  else
  match Types.seqable_constraint_element ty with
  | Some _ -> true
  | None ->
      Option.is_some
        (Core_protocols.find_seqable ty (Compiler_environment.protocols env))

let element_type env collection =
  match to_seq_expr env collection with
  | Ok (inner, _) -> Some inner
  | Error _ -> None

let seq_expr env collection =
  match to_seq_expr env collection with
  | Error _ -> Error.error "seq expects a seqable value"
  | Ok (inner, sequence) -> Ok (typed_ir (TSeq inner) sequence)

let rest_expr env collection =
  match to_seq_expr env collection with
  | Error _ -> Error.error "rest expects a seqable value"
  | Ok (inner, sequence) ->
      Ok
        (typed_ir (TSeq inner)
           (apply "Lg_runtime.Runtime_seq.drop" [ Semantic_ir.Int 1; sequence ]))

let next_expr env collection =
  match to_seq_expr env collection with
  | Error _ -> Error.error "next expects a seqable value"
  | Ok (inner, sequence) ->
      Ok
        (typed_ir (Types.next_seq inner)
           (apply "Lg_runtime.Runtime_seq.drop" [ Semantic_ir.Int 1; sequence ]))

let second_expr env collection =
  match to_seq_expr env collection with
  | Error _ -> Error.error "second expects a seqable value"
  | Ok (inner, sequence) ->
      Ok
        (typed_ir inner
           (apply "Lg_runtime.Runtime_seq.second" [ sequence ]))

let drop_expr env name collection count =
  if not (Types.equal count.ty TInt) then Error.error (name ^ " count must be int")
  else
    match to_seq_expr env collection with
    | Error _ -> Error.error (name ^ " expects a seqable value")
    | Ok (inner, sequence) ->
        Ok
          (typed_ir (TSeq inner)
             (apply "Lg_runtime.Runtime_seq.drop"
                [ count.semantic_expr; sequence ]))

let seqable_adapter ?element_mapper env argument =
  let value_name = "seqable_value__" in
  let value = Semantic_ir.Ident value_name in
  let adapter =
    match Types.seqable_constraint_info argument.ty with
    | Some (constraint_kind, _, _) -> (
        match Semantic_ir.unlocated argument.semantic_expr with
        | Semantic_ir.Ident name ->
            let adapter =
              match constraint_kind with
              | `Required -> Semantic_ir.Ident (name ^ "__seq")
              | `Optional | `Optional_sequential ->
                  let adapter_name = "__lg_seqable_adapter" in
                  Semantic_ir.Match
                    ( Semantic_ir.Ident (name ^ "__seq_optional"),
                      [ ( Semantic_ir.PConstructor ("None", None),
                          Semantic_ir.Apply
                            ( Semantic_ir.Ident "invalid_arg",
                              [ Semantic_ir.String "value is not sequential" ] ) );
                        ( Semantic_ir.PConstructor
                            ("Some", Some (Semantic_ir.PVar adapter_name)),
                          Semantic_ir.Ident adapter_name );
                      ] )
            in
            Ok
              (Semantic_ir.Fun
                 ( [ Semantic_ir.PVar value_name ],
                   Semantic_ir.Apply
                     (adapter, [ value ]) ))
        | _ ->
            Error.error "constrained Seqable value must be a function parameter")
    | None ->
        let parameter = typed_ir argument.ty value in
        to_seq_expr env parameter
        |> Result.map (fun (_, sequence) ->
               Semantic_ir.Fun ([ Semantic_ir.PVar value_name ], sequence))
  in
  match adapter with
  | Error _ as err -> err
  | Ok adapter ->
      let adapter =
        match element_mapper with
        | None -> adapter
        | Some mapper ->
            Semantic_ir.Fun
              ( [ Semantic_ir.PVar value_name ],
                apply "Lg_runtime.Runtime_seq.map"
                  [ mapper; Semantic_ir.Apply (adapter, [ value ]) ] )
      in
      Ok adapter

let pack_seqable_argument ?element_mapper env argument =
  seqable_adapter ?element_mapper env argument
  |> Result.map (fun adapter ->
         Semantic_ir.Tuple [ adapter; argument.semantic_expr ])

let reduce_expr env ?(short_circuit = false) fn init collection sequence =
  if short_circuit then
    match collection.ty with
    | TList _ | TOcaml_app ("list", [ _ ]) ->
        apply "Lg_runtime.Runtime_reduced.fold_list"
          [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]
    | TVector _ ->
        apply "Lg_runtime.Runtime_reduced.fold_vector"
          [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]
    | TArray _ | TOcaml_app ("array", [ _ ]) ->
        apply "Lg_runtime.Runtime_reduced.fold_array"
          [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]
    | TString ->
        apply "Lg_runtime.Runtime_reduced.fold_string"
          [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]
    | TSeq _ | TOcaml_app (("Seq.t" | "Seq"), [ _ ]) ->
        apply "Lg_runtime.Runtime_reduced.fold_seq"
          [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]
    | _ ->
        apply "Lg_runtime.Runtime_reduced.fold_seq"
          [ fn.semantic_expr; init.semantic_expr; sequence ]
  else
  let fallback () =
    apply "Lg_runtime.Runtime_seq.fold_left"
      [ fn.semantic_expr; init.semantic_expr; sequence ]
  in
  match
    Core_protocols.find_reducible collection.ty
      (Compiler_environment.protocols env)
  with
  | None -> fallback ()
  | Some implementation -> (
      match collection.ty with
      | TList _ | TOcaml_app ("list", [ _ ]) ->
          apply "List.fold_left"
            [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]
      | TVector _ ->
          apply "Rrbvec.fold_left"
            [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]
      | TSet inner -> (
          match Types.set_module_name inner with
          | Error _ -> fallback ()
          | Ok set_module ->
              let item = Semantic_ir.Ident "item" in
              let accumulator = Semantic_ir.Ident "accumulator" in
              let reducer =
                Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "item";
                      Semantic_ir.PVar "accumulator" ],
                    Semantic_ir.Apply
                      (fn.semantic_expr, [ accumulator; item ]) )
              in
              apply (set_module ^ ".fold")
                [ reducer; collection.semantic_expr; init.semantic_expr ])
      | TArray _ | TOcaml_app ("array", [ _ ]) ->
          apply "Array.fold_left"
            [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]
      | TString ->
          apply "String.fold_left"
            [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]
      | TSeq _ | TOcaml_app (("Seq.t" | "Seq"), [ _ ]) ->
          apply "Seq.fold_left"
            [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]
      | _ ->
          apply implementation.ocaml_name
            [ collection.semantic_expr; fn.semantic_expr; init.semantic_expr ])

let count_expr env collection =
  let protocols = Compiler_environment.protocols env in
  match Core_protocols.find_counted collection.ty protocols with
  | Some implementation ->
      Ok
        (match collection.ty with
        | TList _ | TOcaml_app ("list", [ _ ]) ->
            apply "List.length" [ collection.semantic_expr ]
        | TVector _ -> apply "Rrbvec.length" [ collection.semantic_expr ]
        | TSet inner -> (
            match Types.set_module_name inner with
            | Ok set_module ->
                apply (set_module ^ ".cardinal") [ collection.semantic_expr ]
            | Error _ -> apply implementation.ocaml_name [ collection.semantic_expr ])
        | TArray _ | TOcaml_app ("array", [ _ ]) ->
            apply "Array.length" [ collection.semantic_expr ]
        | TString -> apply "String.length" [ collection.semantic_expr ]
        | _ -> apply implementation.ocaml_name [ collection.semantic_expr ])
  | None -> (
      match to_seq_expr env collection with
      | Ok (_, sequence) -> Ok (apply "Seq.length" [ sequence ])
      | Error _ -> Error.error "count expects a counted or seqable value")

let is_counted env collection =
  Core_protocols.find_counted collection.ty (Compiler_environment.protocols env)
  |> Option.is_some

let first_expr env collection =
  match to_seq_expr env collection with
  | Error _ -> Error.error "first expects a seqable value"
  | Ok (inner, sequence) ->
      let expression =
        match collection.ty with
        | TList _ | TOcaml_app ("list", [ _ ]) ->
            apply "List.hd" [ collection.semantic_expr ]
        | TVector _ ->
            apply "Rrbvec.nth"
              [ collection.semantic_expr; Semantic_ir.Int 0 ]
        | TSet element -> (
            match Types.set_module_name element with
            | Ok set_module ->
                apply (set_module ^ ".min_elt") [ collection.semantic_expr ]
            | Error _ -> apply "Lg_runtime.Runtime_seq.first" [ sequence ])
        | TArray _ | TOcaml_app ("array", [ _ ]) ->
            apply "Array.get" [ collection.semantic_expr; Semantic_ir.Int 0 ]
        | TString ->
            apply "String.get" [ collection.semantic_expr; Semantic_ir.Int 0 ]
        | TSeq _ | TOcaml_app (("Seq.t" | "Seq"), [ _ ]) ->
            apply "Lg_runtime.Runtime_seq.first" [ collection.semantic_expr ]
        | _ -> apply "Lg_runtime.Runtime_seq.first" [ sequence ]
      in
      Ok (typed_ir inner expression)

let last_expr env collection =
  match to_seq_expr env collection with
  | Error _ -> Error.error "last expects a seqable value"
  | Ok (inner, sequence) ->
      let last_index length = Semantic_ir.Infix ("-", length, Semantic_ir.Int 1) in
      let expression =
        match collection.ty with
        | TList _ | TOcaml_app ("list", [ _ ]) ->
            apply "List.hd" [ apply "List.rev" [ collection.semantic_expr ] ]
        | TVector _ ->
            apply "Option.get"
              [ apply "Rrbvec.peek_back" [ collection.semantic_expr ] ]
        | TSet element -> (
            match Types.set_module_name element with
            | Ok set_module ->
                apply (set_module ^ ".max_elt") [ collection.semantic_expr ]
            | Error _ -> apply "Lg_runtime.Runtime_seq.last" [ sequence ])
        | TArray _ | TOcaml_app ("array", [ _ ]) ->
            apply "Array.get"
              [ collection.semantic_expr;
                last_index (apply "Array.length" [ collection.semantic_expr ]) ]
        | TString ->
            apply "String.get"
              [ collection.semantic_expr;
                last_index (apply "String.length" [ collection.semantic_expr ]) ]
        | TSeq _ | TOcaml_app (("Seq.t" | "Seq"), [ _ ]) ->
            apply "Lg_runtime.Runtime_seq.last" [ collection.semantic_expr ]
        | _ -> apply "Lg_runtime.Runtime_seq.last" [ sequence ]
      in
      Ok (typed_ir inner expression)

let nth_expr env collection index =
  let protocols = Compiler_environment.protocols env in
  match Core_protocols.find_indexed collection.ty protocols with
  | Some implementation -> (
      match collection.ty with
      | TList inner | TOcaml_app ("list", [ inner ]) ->
          Ok
            (typed_ir inner
               (apply "List.nth"
                  [ collection.semantic_expr; index.semantic_expr ]))
      | TVector inner ->
          Ok
            (typed_ir inner
               (apply "Rrbvec.nth"
                  [ collection.semantic_expr; index.semantic_expr ]))
      | TArray inner | TOcaml_app ("array", [ inner ]) ->
          Ok
            (typed_ir inner
               (apply "Array.get"
                  [ collection.semantic_expr; index.semantic_expr ]))
      | TString ->
          Ok
            (typed_ir TChar
               (apply "String.get"
                  [ collection.semantic_expr; index.semantic_expr ]))
      | _ -> (
          match implementation.ty with
          | TFn ([ receiver_ty; index_ty ], return_ty)
            when return_ty <> TUnknown
                 && Types.assignable ~policy:Host_boundary ~expected:receiver_ty
                      ~actual:collection.ty ->
              if
                not
                  (Types.assignable ~policy:Host_boundary ~expected:TInt
                     ~actual:index_ty)
              then
                Error.error "Indexed/-nth index parameter must be int"
              else
              Ok
                (typed_ir return_ty
                   (apply implementation.ocaml_name
                      [ collection.semantic_expr; index.semantic_expr ]))
          | _ ->
              Error.error "Indexed/-nth implementation must return a typed value"))
  | None -> (
      match to_seq_expr env collection with
      | Error _ -> Error.error "nth expects an indexed or seqable value"
      | Ok (inner, sequence) ->
          Ok
            (typed_ir inner
               (apply "Lg_runtime.Runtime_seq.nth"
                  [ index.semantic_expr; sequence ])))
