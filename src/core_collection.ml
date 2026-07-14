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
    || match collection.ty with TVar _ -> true | _ -> false
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
      Ok (typed_ir TInt (Semantic_ir.Int (List.length fields)))
  | _ ->
      Collection_capability.count_expr env collection
      |> Result.map (typed_ir TInt)

let first env collection =
  match Types.dynamic_map_types collection.ty with
  | Some (key_ty, value_ty) ->
      Ok
        (typed_ir (TTuple [ key_ty; value_ty ])
           (apply "Lg_runtime.Runtime_map.first_exn" [ collection.semantic_expr ]))
  | None
    when Types.equal collection.ty TUnknown
         || (match collection.ty with TVar _ -> true | _ -> false) ->
      Ok
        (typed_ir (TTuple [ TUnknown; TUnknown ])
           (apply "Lg_runtime.Runtime_map.first_exn" [ collection.semantic_expr ]))
  | None -> Collection_capability.first_expr env collection

let second env collection = Collection_capability.second_expr env collection

let last env collection = Collection_capability.last_expr env collection

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

let empty_question env collection =
  match collection.ty with
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
      | Error _ -> Error.error "empty? expects a seqable value")

let empty collection =
  match collection.ty with
  | TList _ -> Ok (typed_ir collection.ty (Semantic_ir.List []))
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir collection.ty (Semantic_ir.Ident (set_module ^ ".empty")))
  | TVector _ -> Ok (typed_ir collection.ty (Semantic_ir.Ident "Rrbvec.empty"))
  | TString -> Ok (typed_ir TString (Semantic_ir.String ""))
  | _ -> Error.error "empty expects a collection or string"

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

let take_drop name count collection =
  if not (Types.equal count.ty TInt) then Error.error (name ^ " count must be int")
  else
    match Core_sequence_transform.collection_to_seq_expr collection with
    | Error _ -> Error.error (name ^ " expects a seqable value")
    | Ok (inner, sequence) ->
        let runtime_name =
          if name = "take" then "Lg_runtime.Runtime_seq.take"
          else "Lg_runtime.Runtime_seq.drop"
        in
        Ok
          (typed_ir (TSeq inner)
             (apply runtime_name [ count.semantic_expr; sequence ]))

let reverse collection =
  match collection.ty with
  | TList _ -> Ok (typed_ir collection.ty (apply "List.rev" [ collection.semantic_expr ]))
  | TVector _ -> Ok (typed_ir collection.ty (apply "Rrbvec.rev" [ collection.semantic_expr ]))
  | _ -> Error.error "reverse expects a list or vector"

let compile env name args =
  match name with
  | "count" | "first" | "second" | "last" | "peek" | "pop" | "rest" | "seq"
  | "empty?" | "empty" | "reverse" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match name with
          | "count" -> count env collection
          | "first" -> first env collection
          | "second" -> second env collection
          | "last" -> last env collection
          | "peek" -> peek collection
          | "pop" -> pop collection
          | "rest" -> rest env collection
          | "seq" -> seq env collection
          | "empty?" -> empty_question env collection
          | "empty" -> empty collection
          | "reverse" -> reverse collection
          | _ -> assert false))
  | "take" | "drop" -> (
      match two_args name args with
      | Error _ as err -> err
      | Ok (count, collection) -> take_drop name count collection)
  | _ -> Error.error ("unknown function " ^ name)
