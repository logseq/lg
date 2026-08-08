open Types

let one_arg name args =
  match args with
  | [ arg ] -> Ok arg
  | _ -> Error.error (name ^ " expects 1 arguments")

let two_args name args =
  match args with
  | [ left; right ] -> Ok (left, right)
  | _ -> Error.error (name ^ " expects count and collection")

let apply name args = Semantic_ir.Apply (Semantic_ir.Ident name, args)

let count env collection =
  if
    Option.is_some (Types.dynamic_map_types collection.ty)
    || Types.equal collection.ty TUnknown
    || match collection.ty with TMeta _ | TVar _ -> true | _ -> false
  then
    Ok
      (typed_ir TInt
         (apply "Lg_runtime.Runtime_map.count" [ collection.semantic_expr ]))
  else if Collection_capability.is_counted env collection then
    Collection_capability.count_expr env collection
    |> Result.map (typed_ir TInt)
  else
    match collection.ty with
  | TRecord fields | TNamed_record { fields; _ } ->
      Ok
        (typed_ir TInt
           (Semantic_ir.Int (List.length fields)))
  | _ ->
      Collection_capability.count_expr env collection
      |> Result.map (typed_ir TInt)

let first env collection =
  match Types.dynamic_map_types collection.ty with
  | Some (key_ty, value_ty) ->
      Ok
        (typed_ir (TNullable (TTuple [ key_ty; value_ty ]))
           (apply "Lg_runtime.Runtime_map.first_opt" [ collection.semantic_expr ]))
  | None
    when Types.equal collection.ty TUnknown
         || (match collection.ty with TMeta _ | TVar _ -> true | _ -> false) ->
      Ok
        (typed_ir (TNullable (TTuple [ TUnknown; TUnknown ]))
           (apply "Lg_runtime.Runtime_map.first_opt" [ collection.semantic_expr ]))
  | None -> (
      match collection.ty with
      | TTuple (first_type :: remaining_types) ->
          let patterns =
            Semantic_ir.PVar "first" :: List.map (fun _ -> Semantic_ir.PAny) remaining_types
          in
          Ok
            (typed_ir first_type
               (Semantic_ir.Match
                  ( collection.semantic_expr,
                    [ ( Semantic_ir.PTuple patterns,
                        Semantic_ir.Ident "first" ) ] )))
      | _ -> Collection_capability.first_expr env collection)

let peek collection =
  match collection.ty with
  | TList inner -> Ok (typed_ir inner (apply "List.hd" [ collection.semantic_expr ]))
  | TVector inner ->
      Ok (typed_ir inner (apply "Option.get" [ apply "Rrbvec.peek_back" [ collection.semantic_expr ] ]))
  | _ -> Error.error "peek expects a list or vector"

let pop collection =
  match collection.ty with
  | TList _ -> Ok (typed_ir collection.ty (apply "List.tl" [ collection.semantic_expr ]))
  | TVector _ ->
      Ok
        (typed_ir collection.ty
           (apply "snd" [ apply "Option.get" [ apply "Rrbvec.pop_back" [ collection.semantic_expr ] ] ]))
  | _ -> Error.error "pop expects a list or vector"

let rest env collection = Collection_capability.rest_expr env collection

let seq env collection = Collection_capability.seq_expr env collection

let rec empty_question env collection =
  match collection.ty with
  | TNil ->
      Ok
        (typed_ir TBool
           (Semantic_ir.Sequence
              [ collection.semantic_expr; Semantic_ir.Bool true ]))
  | TNullable value_ty ->
      let value_name = "__lg_optional_collection" in
      let value = typed_ir value_ty (Semantic_ir.Ident value_name) in
      empty_question env value
      |> Result.map (fun present ->
             typed_ir TBool
               (Semantic_ir.Match
                  ( collection.semantic_expr,
                    [ ( Semantic_ir.PConstructor ("None", None),
                        Semantic_ir.Bool true );
                      ( Semantic_ir.PConstructor
                          ("Some", Some (Semantic_ir.PVar value_name)),
                        present.semantic_expr );
                    ] )))
  | TList _ -> Ok (typed_ir TBool (Semantic_ir.Infix ("=", collection.semantic_expr, Semantic_ir.List [])) )
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir TBool (apply (set_module ^ ".is_empty") [ collection.semantic_expr ]))
  | TVector _ -> Ok (typed_ir TBool (apply "Rrbvec.is_empty" [ collection.semantic_expr ]))
  | TString -> Ok (typed_ir TBool (Semantic_ir.Infix ("=", collection.semantic_expr, Semantic_ir.String "")))
  | _ -> (
      match Collection_capability.to_seq_expr env collection with
      | Ok (_, sequence) ->
          Ok
            (typed_ir TBool
               (apply "Lg_runtime.Runtime_seq.is_empty" [ sequence ]))
      | Error _ ->
          Error.error
            ("empty? expects a seqable value, got "
            ^ Types.source_name collection.ty))

let empty env collection =
  let seqable_value =
    match Semantic_ir.unlocated collection.semantic_expr with
    | Semantic_ir.Ident _ -> collection.semantic_expr
    | _ ->
        Semantic_ir.Apply
          (Semantic_ir.Ident "snd", [ collection.semantic_expr ])
  in
  match collection.ty with
  | ty when Types.is_dynamic ty ->
      Ok
        (typed_ir collection.ty
           (apply "Lg_runtime.Runtime_dynamic.empty"
              [ collection.semantic_expr ]))
  | ty when Option.is_some (Types.dynamic_map_types ty) ->
      Ok
        (typed_ir collection.ty
           (apply "Lg_runtime.Runtime_map.empty_like"
              [ collection.semantic_expr ]))
  | TList _ -> Ok (typed_ir collection.ty (Semantic_ir.List []))
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir collection.ty (Semantic_ir.Ident (set_module ^ ".empty")))
  | TVector _ -> Ok (typed_ir collection.ty (Semantic_ir.Ident "Rrbvec.empty"))
  | TString -> Ok (typed_ir TString (Semantic_ir.String ""))
  | _ -> (
      match Types.seqable_constraint_info collection.ty with
      | Some (_, _, (TUnknown | TMeta _ | TVar _)) ->
          Ok
            (typed_ir (Types.dynamic_constraint TUnknown)
               (apply "Lg_runtime.Runtime_dynamic.empty"
                  [ seqable_value ]))
      | Some (_, _, value_ty) when Types.is_dynamic value_ty ->
          Ok
            (typed_ir value_ty
               (apply "Lg_runtime.Runtime_dynamic.empty"
                  [ seqable_value ]))
      | Some _ | None -> (
      match
        Core_protocols.find_emptyable collection.ty
          (Compiler_environment.protocols env)
      with
      | Some { ty = TFn ([ receiver_ty ], return_ty); ocaml_name; _ }
        when Types.assignable ~policy:Host_boundary ~expected:receiver_ty
               ~actual:collection.ty ->
          Ok
            (typed_ir return_ty
               (apply ocaml_name [ collection.semantic_expr ]))
      | _ ->
          let collection_type =
            match collection.ty with
            | TRecord fields ->
                "map {"
                ^ String.concat ", "
                    (List.map (fun (field : field) -> field.keyword) fields)
                ^ "}"
            | TNamed_record record -> "record " ^ record.type_name
            | ty -> Types.source_name ty
          in
          Error.error
            ("empty expects a collection or string, got "
            ^ collection_type)) )

let take_list_expr count source =
  let n = Semantic_ir.Ident "n" in
  let xs = Semantic_ir.Ident "xs" in
  let body =
    Semantic_ir.If
      ( Semantic_ir.Infix ("<=", n, Semantic_ir.Int 0),
        Semantic_ir.List [],
        Semantic_ir.Match
          ( xs,
            [ (Semantic_ir.PList [], Semantic_ir.List []);
              ( Semantic_ir.PCons (Semantic_ir.PVar "x", Semantic_ir.PVar "rest"),
                Semantic_ir.Cons
                  ( Semantic_ir.Ident "x",
                    apply "take__"
                      [ Semantic_ir.Infix ("-", n, Semantic_ir.Int 1);
                        Semantic_ir.Ident "rest" ] ) ) ] ) )
  in
  Semantic_ir.LetRec ("take__", [ Semantic_ir.PVar "n"; Semantic_ir.PVar "xs" ], body, [ count; source ])

let drop_list_expr count source =
  let n = Semantic_ir.Ident "n" in
  let xs = Semantic_ir.Ident "xs" in
  let body =
    Semantic_ir.If
      ( Semantic_ir.Infix ("<=", n, Semantic_ir.Int 0),
        xs,
        Semantic_ir.Match
          ( xs,
            [ (Semantic_ir.PList [], Semantic_ir.List []);
              ( Semantic_ir.PCons (Semantic_ir.PAny, Semantic_ir.PVar "rest"),
                apply "drop__"
                  [ Semantic_ir.Infix ("-", n, Semantic_ir.Int 1);
                    Semantic_ir.Ident "rest" ] ) ] ) )
  in
  Semantic_ir.LetRec ("drop__", [ Semantic_ir.PVar "n"; Semantic_ir.PVar "xs" ], body, [ count; source ])

let take_drop env name count collection =
  if not (Types.equal count.ty TInt) then Error.error (name ^ " count must be int")
  else
    match Collection_capability.to_seq_expr env collection with
    | Error _ -> Error.error (name ^ " expects a seqable value")
    | Ok (inner, sequence) ->
        let runtime_name =
          if name = "take" then "Lg_runtime.Runtime_seq.take"
          else "Lg_runtime.Runtime_seq.drop"
        in
        Ok
          (typed_ir (TSeq inner)
             (apply runtime_name
                [ count.semantic_expr; sequence ]))

let compile env name args =
  match name with
  | "count" | "first" | "peek" | "pop" | "rest" | "seq"
  | "empty?" | "empty" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match name with
          | "count" -> count env collection
          | "first" -> first env collection
          | "peek" -> peek collection
          | "pop" -> pop collection
          | "rest" -> rest env collection
          | "seq" -> seq env collection
          | "empty?" -> empty_question env collection
          | "empty" -> empty env collection
          | _ -> assert false))
  | "take" | "drop" -> (
      match two_args name args with
      | Error _ as err -> err
      | Ok (count, collection) -> take_drop env name count collection)
  | _ -> Error.error ("unknown function " ^ name)
