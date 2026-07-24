open Types

let apply name args = Semantic_ir.Apply (Semantic_ir.Ident name, args)

let rec collection_to_list_expr collection =
  match collection.ty with
  | TNullable value_ty | TOcaml_app ("option", [ value_ty ]) ->
      let value_name = "__lg_optional_collection" in
      let value = typed_ir value_ty (Semantic_ir.Ident value_name) in
      Result.map
        (fun (inner, list_expr) ->
          ( inner,
            Semantic_ir.Match
              ( collection.semantic_expr,
                [
                  (Semantic_ir.PConstructor ("None", None), Semantic_ir.List []);
                  ( Semantic_ir.PConstructor
                      ("Some", Some (Semantic_ir.PVar value_name)),
                    list_expr );
                ] ) ))
        (collection_to_list_expr value)
  | TList inner -> Ok (inner, collection.semantic_expr)
  | TVector inner -> Ok (inner, apply "Rrbvec.to_list" [ collection.semantic_expr ])
  | TArray inner -> Ok (inner, apply "Array.to_list" [ collection.semantic_expr ])
  | TSeq inner -> Ok (inner, apply "List.of_seq" [ collection.semantic_expr ])
  | TOcaml_app (name, [ inner ]) when name = Types.next_seq_type_name ->
      Ok (inner, apply "List.of_seq" [ collection.semantic_expr ])
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
          (inner, apply (set_module ^ ".elements") [ collection.semantic_expr ]))
  | TOcaml_app ("array", [ inner ]) ->
      Ok (inner, apply "Array.to_list" [ collection.semantic_expr ])
  | _ -> Error.error "collection value is not sequenceable"

let collection_to_seq_expr collection =
  match collection.ty with
  | TSeq inner -> Ok (inner, collection.semantic_expr)
  | TOcaml_app (name, [ inner ]) when name = Types.next_seq_type_name ->
      Ok (inner, collection.semantic_expr)
  | TList inner ->
      Ok
        (inner, apply "Lg_runtime.Runtime_seq.of_list" [ collection.semantic_expr ])
  | TVector inner ->
      Ok
        (inner, apply "Lg_runtime.Runtime_seq.of_vector" [ collection.semantic_expr ])
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             ( inner,
               apply "Lg_runtime.Runtime_seq.of_list"
                 [ apply (set_module ^ ".elements") [ collection.semantic_expr ] ] ))
  | TArray inner ->
      Ok
        (inner, apply "Lg_runtime.Runtime_seq.of_array" [ collection.semantic_expr ])
  | TOcaml_app ("list", [ inner ]) ->
      Ok
        (inner, apply "Lg_runtime.Runtime_seq.of_list" [ collection.semantic_expr ])
  | TOcaml_app ("array", [ inner ]) ->
      Ok
        (inner, apply "Lg_runtime.Runtime_seq.of_array" [ collection.semantic_expr ])
  | TOcaml_app (("Seq.t" | "Seq"), [ inner ]) ->
      Ok
        (inner, apply "Lg_runtime.Runtime_seq.memoize" [ collection.semantic_expr ])
  | TString ->
      Ok
        (TChar, apply "Lg_runtime.Runtime_seq.of_string" [ collection.semantic_expr ])
  | _ -> Error.error "collection value is not sequenceable"

let rec collection_from_list_expr collection_ty list_expr =
  match collection_ty with
  | TNullable value_ty | TOcaml_app ("option", [ value_ty ]) ->
      Semantic_ir.Constructor
        ("Some", Some (collection_from_list_expr value_ty list_expr))
  | TList _ -> list_expr
  | TVector _ -> apply "Rrbvec.of_list" [ list_expr ]
  | TSeq _ -> apply "Lg_runtime.Runtime_seq.of_list" [ list_expr ]
  | TSet inner -> (
      match Types.set_module_name inner with
      | Ok set_module -> apply (set_module ^ ".of_list") [ list_expr ]
      | Error _ -> list_expr)
  | _ -> list_expr

let take_list_expr count source =
  let name = "take__" in
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
                    apply name
                      [ Semantic_ir.Infix ("-", n, Semantic_ir.Int 1);
                        Semantic_ir.Ident "rest" ] ) ) ] ) )
  in
  Semantic_ir.LetRec (name, [ Semantic_ir.PVar "n"; Semantic_ir.PVar "xs" ], body, [ count; source ])

let drop_list_expr count source =
  let name = "drop__" in
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
                apply name
                  [ Semantic_ir.Infix ("-", n, Semantic_ir.Int 1);
                    Semantic_ir.Ident "rest" ] ) ] ) )
  in
  Semantic_ir.LetRec (name, [ Semantic_ir.PVar "n"; Semantic_ir.PVar "xs" ], body, [ count; source ])

let remove fn collection =
  match (fn.ty, collection_to_list_expr collection) with
  | TFn ([ param_ty ], TBool), Ok (inner, list_expr)
    when Result.is_ok (Type_solver.unify [] param_ty inner) ->
      let filtered =
        apply "List.filter"
          [ Semantic_ir.Fun
              ( [ Semantic_ir.PVar "item" ],
                Semantic_ir.Prefix
                  ( "not",
                    Semantic_ir.Apply (fn.semantic_expr, [ Semantic_ir.Ident "item" ]) ) );
            list_expr ]
      in
      Ok (typed_ir collection.ty (collection_from_list_expr collection.ty filtered))
  | TFn _, Ok _ -> Error.error "remove expects a predicate matching collection elements"
  | _, Ok _ -> Error.error "remove expects a function"
  | _, Error _ -> Error.error "remove expects a list, vector, or set"

let take_drop_while name fn collection =
  match (fn.ty, collection_to_list_expr collection) with
  | TFn ([ param_ty ], TBool), Ok (inner, list_expr) when Types.equal param_ty inner ->
      let rec_name = if name = "take-while" then "take_while" else "drop_while" in
      let predicate = Semantic_ir.Apply (fn.semantic_expr, [ Semantic_ir.Ident "item" ]) in
      let body =
        if name = "take-while" then
          Semantic_ir.Match
            ( Semantic_ir.Ident "xs",
              [ ( Semantic_ir.PCons (Semantic_ir.PVar "item", Semantic_ir.PVar "rest"),
                  Semantic_ir.If
                    ( predicate,
                      Semantic_ir.Cons
                        (Semantic_ir.Ident "item", apply rec_name [ Semantic_ir.Ident "rest" ]),
                      Semantic_ir.List [] ) );
                (Semantic_ir.PAny, Semantic_ir.List []) ] )
        else
          Semantic_ir.Match
            ( Semantic_ir.Ident "xs",
              [ ( Semantic_ir.PCons (Semantic_ir.PVar "item", Semantic_ir.PVar "rest"),
                  Semantic_ir.If
                    ( predicate,
                      apply rec_name [ Semantic_ir.Ident "rest" ],
                      Semantic_ir.Cons (Semantic_ir.Ident "item", Semantic_ir.Ident "rest") ) );
                (Semantic_ir.PList [], Semantic_ir.List []) ] )
      in
      let list_expr =
        Semantic_ir.LetRec (rec_name, [ Semantic_ir.PVar "xs" ], body, [ list_expr ])
      in
      Ok (typed_ir collection.ty (collection_from_list_expr collection.ty list_expr))
  | TFn _, Ok _ ->
      Error.error (name ^ " expects a predicate matching collection elements")
  | _, Ok _ -> Error.error (name ^ " expects a function")
  | _, Error _ -> Error.error (name ^ " expects a list, vector, or set")

let distinct collection =
  match collection_to_list_expr collection with
  | Error _ -> Error.error "distinct expects a list, vector, or set"
  | Ok (_inner, list_expr) ->
      let body =
        Semantic_ir.Match
          ( Semantic_ir.Ident "xs",
            [ (Semantic_ir.PList [], apply "List.rev" [ Semantic_ir.Ident "acc" ]);
              ( Semantic_ir.PCons (Semantic_ir.PVar "item", Semantic_ir.PVar "rest"),
                Semantic_ir.If
                  ( apply "List.mem" [ Semantic_ir.Ident "item"; Semantic_ir.Ident "seen" ],
                    apply "distinct"
                      [ Semantic_ir.Ident "seen";
                        Semantic_ir.Ident "acc";
                        Semantic_ir.Ident "rest" ],
                    apply "distinct"
                      [ Semantic_ir.Cons (Semantic_ir.Ident "item", Semantic_ir.Ident "seen");
                        Semantic_ir.Cons (Semantic_ir.Ident "item", Semantic_ir.Ident "acc");
                        Semantic_ir.Ident "rest" ] ) ) ] )
      in
      let list_expr =
        Semantic_ir.LetRec
          ( "distinct",
            [ Semantic_ir.PVar "seen"; Semantic_ir.PVar "acc"; Semantic_ir.PVar "xs" ],
            body,
            [ Semantic_ir.List []; Semantic_ir.List []; list_expr ] )
      in
      Ok (typed_ir collection.ty (collection_from_list_expr collection.ty list_expr))

let dedupe collection =
  match collection_to_list_expr collection with
  | Error _ -> Error.error "dedupe expects a list, vector, or set"
  | Ok (_inner, list_expr) ->
      let body =
        Semantic_ir.Match
          ( Semantic_ir.Ident "xs",
            [ (Semantic_ir.PList [], apply "List.rev" [ Semantic_ir.Ident "acc" ]);
              ( Semantic_ir.PCons (Semantic_ir.PVar "item", Semantic_ir.PVar "rest"),
                Semantic_ir.Match
                  ( Semantic_ir.Ident "acc",
                    [ ( Semantic_ir.PCons (Semantic_ir.PVar "previous", Semantic_ir.PAny),
                        Semantic_ir.If
                          ( Semantic_ir.Infix
                              ("=", Semantic_ir.Ident "previous", Semantic_ir.Ident "item"),
                            apply "dedupe"
                              [ Semantic_ir.Ident "acc"; Semantic_ir.Ident "rest" ],
                            apply "dedupe"
                              [ Semantic_ir.Cons
                                  (Semantic_ir.Ident "item", Semantic_ir.Ident "acc");
                                Semantic_ir.Ident "rest" ] ) );
                      ( Semantic_ir.PAny,
                        apply "dedupe"
                          [ Semantic_ir.Cons (Semantic_ir.Ident "item", Semantic_ir.Ident "acc");
                            Semantic_ir.Ident "rest" ] ) ] ) ) ] )
      in
      let list_expr =
        Semantic_ir.LetRec
          ( "dedupe",
            [ Semantic_ir.PVar "acc"; Semantic_ir.PVar "xs" ],
            body,
            [ Semantic_ir.List []; list_expr ] )
      in
      Ok (typed_ir collection.ty (collection_from_list_expr collection.ty list_expr))

let sort collection =
  if Types.is_dynamic collection.ty then
    Ok
      (typed_ir collection.ty
         (apply "Lg_runtime.Runtime_dynamic.sort" [ collection.semantic_expr ]))
  else
    match collection_to_list_expr collection with
    | Error _ -> Error.error "sort expects a list, vector, or set"
    | Ok (inner, list_expr) ->
        let comparator =
          if Types.is_dynamic inner then
            Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.compare"
          else if Types.equal inner TKeyword || Types.equal inner TSymbol then
            Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.compare_identifier"
          else Semantic_ir.Ident "compare"
        in
        Ok
          (typed_ir (TList inner)
             (apply "List.sort" [ comparator; list_expr ]))

let concat collections =
  let rec loop element_ty exprs = function
    | [] -> Ok (element_ty, List.rev exprs)
    | collection :: rest -> (
        match collection_to_list_expr collection with
        | Error _ -> Error.error "concat expects collections"
        | Ok (inner, expr) -> (
            match element_ty with
            | None -> loop (Some inner) (expr :: exprs) rest
            | Some element_ty ->
                if Types.equal element_ty inner then
                  loop (Some element_ty) (expr :: exprs) rest
                else Error.error "concat element types must match"))
  in
  match loop None [] collections with
  | Error _ as err -> err
  | Ok (None, _) -> Error.error "concat expects at least 1 collection"
  | Ok (Some inner, exprs) ->
      Ok (typed_ir (TList inner) (apply "List.concat" [ Semantic_ir.List exprs ]))

let vec collection =
  match collection_to_list_expr collection with
  | Error _ -> Error.error "vec expects a list, vector, or set"
  | Ok (inner, list_expr) ->
      Ok (typed_ir (TVector inner) (apply "Rrbvec.of_list" [ list_expr ]))

let set collection =
  match collection_to_list_expr collection with
  | Error _ -> Error.error "set expects a list, vector, or set"
  | Ok (inner, list_expr) ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir (TSet inner)
               (apply (set_module ^ ".of_list") [ list_expr ]))

let repeat count value =
  if Types.equal count.ty TInt then
    Ok
      (typed_ir (TSeq value.ty)
         (apply "Lg_runtime.Runtime_seq.take"
            [ count.semantic_expr;
              apply "Lg_runtime.Runtime_seq.repeat" [ value.semantic_expr ] ]))
  else Error.error "repeat count must be int"

let repeat_forever value =
  Ok
    (typed_ir (TSeq value.ty)
       (apply "Lg_runtime.Runtime_seq.repeat" [ value.semantic_expr ]))

let cycle collection =
  match collection_to_seq_expr collection with
  | Error _ -> Error.error "cycle expects a collection"
  | Ok (inner, sequence) ->
      Ok
        (typed_ir (TSeq inner)
           (apply "Lg_runtime.Runtime_seq.cycle" [ sequence ]))

let interpose separator collection =
  match collection_to_list_expr collection with
  | Error _ -> Error.error "interpose expects a collection"
  | Ok (inner, list_expr) ->
      if Types.equal separator.ty inner then
        let interpose_body =
          Semantic_ir.Match
            ( Semantic_ir.Ident "xs",
              [ (Semantic_ir.PList [], apply "List.rev" [ Semantic_ir.Ident "acc" ]);
                ( Semantic_ir.PList [ Semantic_ir.PVar "item" ],
                  apply "List.rev"
                    [ Semantic_ir.Cons (Semantic_ir.Ident "item", Semantic_ir.Ident "acc") ]
                );
                ( Semantic_ir.PCons (Semantic_ir.PVar "item", Semantic_ir.PVar "rest"),
                  apply "interpose"
                    [ Semantic_ir.Cons
                        ( Semantic_ir.Ident "separator",
                          Semantic_ir.Cons
                            (Semantic_ir.Ident "item", Semantic_ir.Ident "acc") );
                      Semantic_ir.Ident "rest" ] ) ] )
        in
        Ok
          (typed_ir (TList inner)
             (Semantic_ir.Let
                ( [ (Semantic_ir.PVar "separator", separator.semantic_expr) ],
                  Semantic_ir.LetRec
                    ( "interpose",
                      [ Semantic_ir.PVar "acc"; Semantic_ir.PVar "xs" ],
                      interpose_body,
                      [ Semantic_ir.List []; list_expr ] ) )))
      else Error.error "interpose separator type must match collection elements"

let interleave collections =
  if List.length collections < 2 then
    Error.error "interleave expects at least two collections"
  else
    let rec loop element_ty exprs = function
      | [] -> Ok (element_ty, List.rev exprs)
      | collection :: rest -> (
          match collection_to_list_expr collection with
          | Error _ -> Error.error "interleave expects collections"
          | Ok (inner, expr) -> (
              match element_ty with
              | None -> loop (Some inner) (expr :: exprs) rest
              | Some element_ty ->
                if Types.equal element_ty inner then
                    loop (Some element_ty) (expr :: exprs) rest
                  else Error.error "interleave element types must match"))
    in
    match loop None [] collections with
    | Error _ as err -> err
    | Ok (None, _) -> Error.error "interleave expects at least two collections"
    | Ok (Some inner, exprs) ->
        let collections = Semantic_ir.Ident "collections" in
        let any_empty =
          apply "List.exists"
            [ Semantic_ir.Fun
                ( [ Semantic_ir.PVar "collection" ],
                  Semantic_ir.Match
                    ( Semantic_ir.Ident "collection",
                      [ (Semantic_ir.PList [], Semantic_ir.Bool true);
                        (Semantic_ir.PAny, Semantic_ir.Bool false) ] ) );
              collections ]
        in
        let body =
          Semantic_ir.If
            ( any_empty,
              apply "List.rev" [ Semantic_ir.Ident "acc" ],
              Semantic_ir.Let
                ( [ (Semantic_ir.PVar "heads", apply "List.map" [ Semantic_ir.Ident "List.hd"; collections ]);
                    (Semantic_ir.PVar "tails", apply "List.map" [ Semantic_ir.Ident "List.tl"; collections ]) ],
                  apply "interleave"
                    [ apply "List.rev_append"
                        [ Semantic_ir.Ident "heads"; Semantic_ir.Ident "acc" ];
                      Semantic_ir.Ident "tails" ] ) )
        in
        Ok
          (typed_ir (TList inner)
             (Semantic_ir.LetRec
                ( "interleave",
                  [ Semantic_ir.PVar "acc"; Semantic_ir.PVar "collections" ],
                  body,
                  [ Semantic_ir.List []; Semantic_ir.List exprs ] )))

let partition name size collection =
  if not (Types.equal size.ty TInt) then Error.error (name ^ " size must be int")
  else
    match collection_to_list_expr collection with
    | Error _ -> Error.error (name ^ " expects a collection")
    | Ok (inner, list_expr) ->
        let take_body return_done return_empty =
          Semantic_ir.If
            ( Semantic_ir.Infix
                ("=", Semantic_ir.Ident "n", Semantic_ir.Int 0),
              return_done
                (Semantic_ir.Tuple
                   [ apply "List.rev" [ Semantic_ir.Ident "acc" ];
                     Semantic_ir.Ident "xs" ]),
              Semantic_ir.Match
                ( Semantic_ir.Ident "xs",
                  [ (Semantic_ir.PList [], return_empty);
                    ( Semantic_ir.PCons (Semantic_ir.PVar "item", Semantic_ir.PVar "rest"),
                      apply "take"
                        [ Semantic_ir.Infix
                            ("-", Semantic_ir.Ident "n", Semantic_ir.Int 1);
                          Semantic_ir.Cons (Semantic_ir.Ident "item", Semantic_ir.Ident "acc");
                          Semantic_ir.Ident "rest" ] ) ] ) )
        in
        let expr =
          if name = "partition-all" then
            let partition_body =
              Semantic_ir.Match
                ( Semantic_ir.Ident "xs",
                  [ (Semantic_ir.PList [], apply "List.rev" [ Semantic_ir.Ident "acc" ]);
                    ( Semantic_ir.PAny,
                      Semantic_ir.Let
                        ( [ ( Semantic_ir.PTuple [ Semantic_ir.PVar "chunk"; Semantic_ir.PVar "rest" ],
                              apply "take"
                                [ Semantic_ir.Ident "size";
                                  Semantic_ir.List [];
                                  Semantic_ir.Ident "xs" ] ) ],
                          apply "partition_all"
                            [ Semantic_ir.Cons (Semantic_ir.Ident "chunk", Semantic_ir.Ident "acc");
                              Semantic_ir.Ident "rest" ] ) ) ] )
            in
            Semantic_ir.Let
              ( [ (Semantic_ir.PVar "size", size.semantic_expr) ],
                Semantic_ir.LetRecIn
                  ( "take",
                    [ Semantic_ir.PVar "n"; Semantic_ir.PVar "acc"; Semantic_ir.PVar "xs" ],
                    take_body Fun.id
                      (Semantic_ir.Tuple
                         [ apply "List.rev" [ Semantic_ir.Ident "acc" ];
                           Semantic_ir.List [] ]),
                    Semantic_ir.LetRec
                      ( "partition_all",
                        [ Semantic_ir.PVar "acc"; Semantic_ir.PVar "xs" ],
                        partition_body,
                        [ Semantic_ir.List []; list_expr ] ) ) )
          else
            let partition_body =
              Semantic_ir.Match
                ( apply "take"
                    [ Semantic_ir.Ident "size"; Semantic_ir.List []; Semantic_ir.Ident "xs" ],
                  [ (Semantic_ir.PConstructor ("None", None), apply "List.rev" [ Semantic_ir.Ident "acc" ]);
                    ( Semantic_ir.PConstructor
                        ( "Some",
                          Some
                            (Semantic_ir.PTuple
                               [ Semantic_ir.PVar "chunk"; Semantic_ir.PVar "rest" ]) ),
                      apply "partition"
                        [ Semantic_ir.Cons (Semantic_ir.Ident "chunk", Semantic_ir.Ident "acc");
                          Semantic_ir.Ident "rest" ] ) ] )
            in
            Semantic_ir.Let
              ( [ (Semantic_ir.PVar "size", size.semantic_expr) ],
                Semantic_ir.LetRecIn
                  ( "take",
                    [ Semantic_ir.PVar "n"; Semantic_ir.PVar "acc"; Semantic_ir.PVar "xs" ],
                    take_body
                      (fun value -> Semantic_ir.Constructor ("Some", Some value))
                      (Semantic_ir.Constructor ("None", None)),
                    Semantic_ir.LetRec
                      ( "partition",
                        [ Semantic_ir.PVar "acc"; Semantic_ir.PVar "xs" ],
                        partition_body,
                        [ Semantic_ir.List []; list_expr ] ) ) )
        in
        Ok (typed_ir (TList (TList inner)) expr)

let butlast collection =
  if Types.is_dynamic collection.ty then
    Ok
      (typed_ir collection.ty
         (apply "Lg_runtime.Runtime_dynamic.butlast"
            [ collection.semantic_expr ]))
  else match collection_to_list_expr collection with
  | Error _ -> Error.error "butlast expects a collection"
  | Ok (_inner, list_expr) ->
      let body =
        Semantic_ir.Match
          ( Semantic_ir.Ident "xs",
            [ (Semantic_ir.PList [], apply "List.rev" [ Semantic_ir.Ident "acc" ]);
              (Semantic_ir.PList [ Semantic_ir.PAny ], apply "List.rev" [ Semantic_ir.Ident "acc" ]);
              ( Semantic_ir.PCons (Semantic_ir.PVar "item", Semantic_ir.PVar "rest"),
                apply "butlast"
                  [ Semantic_ir.Cons (Semantic_ir.Ident "item", Semantic_ir.Ident "acc");
                    Semantic_ir.Ident "rest" ] ) ] )
      in
      let list_expr =
        Semantic_ir.LetRec
          ( "butlast",
            [ Semantic_ir.PVar "acc"; Semantic_ir.PVar "xs" ],
            body,
            [ Semantic_ir.List []; list_expr ] )
      in
      Ok (typed_ir collection.ty (collection_from_list_expr collection.ty list_expr))

let take_drop_last name count collection =
  if not (Types.equal count.ty TInt) then Error.error (name ^ " count must be int")
  else
    match collection_to_list_expr collection with
    | Error _ -> Error.error (name ^ " expects a collection")
    | Ok (_inner, list_expr) ->
        let count_name = if name = "take-last" then "drop_count" else "keep_count" in
        let count_expr =
          apply "max"
            [ Semantic_ir.Int 0;
              Semantic_ir.Infix
                ( "-",
                  apply "List.length" [ Semantic_ir.Ident "source" ],
                  count.semantic_expr ) ]
        in
        let result_expr =
          if name = "take-last" then
            drop_list_expr (Semantic_ir.Ident count_name) (Semantic_ir.Ident "source")
          else
            take_list_expr (Semantic_ir.Ident count_name) (Semantic_ir.Ident "source")
        in
        Ok
          (typed_ir collection.ty
             (collection_from_list_expr collection.ty
                (Semantic_ir.Let
                   ( [ (Semantic_ir.PVar "source", list_expr);
                       (Semantic_ir.PVar count_name, count_expr) ],
                     result_expr ))))

let take_nth count collection =
  if not (Types.equal count.ty TInt) then Error.error "take-nth n must be int"
  else
    match collection_to_list_expr collection with
    | Error _ -> Error.error "take-nth expects a collection"
    | Ok (_inner, list_expr) ->
        let next_index =
          Semantic_ir.Infix
            ("+", Semantic_ir.Ident "index", Semantic_ir.Int 1)
        in
        let body =
          Semantic_ir.Match
            ( Semantic_ir.Ident "xs",
              [ (Semantic_ir.PList [], apply "List.rev" [ Semantic_ir.Ident "acc" ]);
                ( Semantic_ir.PCons (Semantic_ir.PVar "item", Semantic_ir.PVar "rest"),
                  Semantic_ir.If
                    ( Semantic_ir.Infix
                        ( "=",
                          Semantic_ir.Infix
                            ( "mod",
                              Semantic_ir.Ident "index",
                              count.semantic_expr ),
                          Semantic_ir.Int 0 ),
                      apply "take_nth"
                        [ next_index;
                          Semantic_ir.Cons (Semantic_ir.Ident "item", Semantic_ir.Ident "acc");
                          Semantic_ir.Ident "rest" ],
                      apply "take_nth"
                        [ next_index;
                          Semantic_ir.Ident "acc";
                          Semantic_ir.Ident "rest" ] ) ) ] )
        in
        let list_expr =
          Semantic_ir.LetRec
            ( "take_nth",
              [ Semantic_ir.PVar "index"; Semantic_ir.PVar "acc"; Semantic_ir.PVar "xs" ],
              body,
              [ Semantic_ir.Int 0; Semantic_ir.List []; list_expr ] )
        in
        Ok (typed_ir collection.ty (collection_from_list_expr collection.ty list_expr))

let split_at count collection =
  if not (Types.equal count.ty TInt) then Error.error "split-at count must be int"
  else
    match collection_to_list_expr collection with
    | Error _ -> Error.error "split-at expects a collection"
    | Ok (_inner, list_expr) ->
        let left =
          collection_from_list_expr collection.ty
            (take_list_expr count.semantic_expr list_expr)
        in
        let right =
          collection_from_list_expr collection.ty
            (drop_list_expr count.semantic_expr list_expr)
        in
        Ok (typed_ir (TVector collection.ty) (apply "Rrbvec.of_list" [ Semantic_ir.List [ left; right ] ]))

let bounded_count limit collection =
  if not (Types.equal limit.ty TInt) then Error.error "bounded-count limit must be int"
  else
    match collection_to_list_expr collection with
    | Error _ -> Error.error "bounded-count expects a collection"
    | Ok (_inner, list_expr) ->
        Ok
          (typed_ir TInt
             (apply "min"
                [ limit.semantic_expr;
                  apply "List.length" [ list_expr ];
                ]))

let dorun collection =
  match collection_to_list_expr collection with
  | Error _ -> Error.error "dorun expects a collection"
  | Ok _ -> Ok (typed_ir TUnit Semantic_ir.Unit)

let doall collection =
  match collection_to_list_expr collection with
  | Error _ -> Error.error "doall expects a collection"
  | Ok _ -> Ok collection

let into target source =
  match collection_to_list_expr source with
  | Error _ -> Error.error "into source must be a collection"
  | Ok (source_inner, source_list_expr) -> (
      match target.ty with
      | TVector (TVar _) ->
          Ok
            (typed_ir (TVector source_inner)
               (apply "Rrbvec.append_list"
                  [ target.semantic_expr; source_list_expr ]))
      | TVector target_inner when Types.equal target_inner source_inner -> (
          match source.ty with
          | TVector _ ->
              Ok
                (typed_ir target.ty
                   (apply "Rrbvec.append" [ target.semantic_expr; source.semantic_expr ]))
          | _ ->
              Ok
                (typed_ir target.ty
                   (apply "Rrbvec.append_list" [ target.semantic_expr; source_list_expr ])))
      | TVector target_inner
        when Types.is_dynamic target_inner && Types.is_dynamic source_inner ->
          Ok
            (typed_ir target.ty
               (apply "Rrbvec.append_list"
                  [ target.semantic_expr; source_list_expr ]))
      | TList target_inner when Types.equal target_inner source_inner ->
          Ok
            (typed_ir target.ty
               (apply "List.fold_left"
                  [ Semantic_ir.Fun
                      ( [ Semantic_ir.PVar "acc"; Semantic_ir.PVar "item" ],
                        Semantic_ir.Cons (Semantic_ir.Ident "item", Semantic_ir.Ident "acc") );
                    target.semantic_expr;
                    source_list_expr ]))
      | TList target_inner
        when Types.is_dynamic target_inner && Types.is_dynamic source_inner ->
          Ok
            (typed_ir target.ty
               (apply "List.fold_left"
                  [ Semantic_ir.Fun
                      ( [ Semantic_ir.PVar "acc"; Semantic_ir.PVar "item" ],
                        Semantic_ir.Cons
                          (Semantic_ir.Ident "item", Semantic_ir.Ident "acc") );
                    target.semantic_expr;
                    source_list_expr ]))
      | TSet TUnknown when Types.is_dynamic source_inner ->
          Ok
            (typed_ir source_inner
               (apply "Lg_runtime.Runtime_dynamic.set"
                  [
                    apply "Lg_runtime.Runtime_seq.of_list"
                      [ source_list_expr ];
                  ]))
      | TSet TUnknown ->
          Types.set_module_name source_inner
          |> Result.map (fun set_module ->
                 typed_ir (TSet source_inner)
                   (apply (set_module ^ ".of_list")
                      [ source_list_expr ]))
      | TSet target_inner when Types.equal target_inner source_inner ->
          Types.set_module_name target_inner
          |> Result.map (fun set_module ->
                 typed_ir target.ty
                   (apply (set_module ^ ".of_list")
                      [ Semantic_ir.Infix
                          ( "@",
                            apply (set_module ^ ".elements") [ target.semantic_expr ],
                            source_list_expr ) ]))
      | TVector _ | TList _ | TSet _ ->
          Error.error "into source element type must match target element type"
      | _ -> Error.error "into target must be a collection")

let into_cat target source =
  match collection_to_list_expr source with
  | Error _ ->
      Error.error
        ("into cat source must be a collection, got "
       ^ Types.source_name source.ty)
  | Ok (TVector element_type, outer) ->
      let flattened =
        apply "List.concat"
          [ apply "List.map"
              [ Semantic_ir.Ident "Rrbvec.to_list"; outer ] ]
      in
      into target (typed_ir (TList element_type) flattened)
  | Ok (TList element_type, outer) ->
      into target
        (typed_ir (TList element_type) (apply "List.concat" [ outer ]))
  | Ok (inner, outer)
    when Types.is_dynamic inner
         || match inner with TUnknown | TVar _ -> true | _ -> false ->
      let flattened =
        apply "List.concat"
          [ apply "List.map"
              [
                Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "__lg_cat_item" ],
                    apply "List.of_seq"
                      [
                        apply "Lg_runtime.Runtime_dynamic.to_seq"
                          [ Semantic_ir.Ident "__lg_cat_item" ];
                      ] );
                outer;
              ] ]
      in
      into target
        (typed_ir (TList (Types.dynamic_constraint TUnknown)) flattened)
  | Ok _ ->
      Error.error "into cat source elements must be collections"

let compile name args =
  match (name, args) with
  | "remove", [ fn; collection ] -> remove fn collection
  | ("take-while" | "drop-while"), [ fn; collection ] ->
      take_drop_while name fn collection
  | "distinct", [ collection ] -> distinct collection
  | "dedupe", [ collection ] -> dedupe collection
  | "sort", [ collection ] -> sort collection
  | "concat", [] -> Error.error "concat expects at least 1 collection"
  | "concat", collections -> concat collections
  | "vec", [ collection ] -> vec collection
  | "set", [ collection ] -> set collection
  | "repeat", [ count; value ] -> repeat count value
  | "repeat", [ value ] -> repeat_forever value
  | "cycle", [ collection ] -> cycle collection
  | "interpose", [ separator; collection ] -> interpose separator collection
  | "interleave", collections -> interleave collections
  | ("partition" | "partition-all"), [ size; collection ] ->
      partition name size collection
  | "butlast", [ collection ] -> butlast collection
  | ("take-last" | "drop-last"), [ count; collection ] ->
      take_drop_last name count collection
  | "take-nth", [ count; collection ] -> take_nth count collection
  | "split-at", [ count; collection ] -> split_at count collection
  | "bounded-count", [ limit; collection ] -> bounded_count limit collection
  | "dorun", [ collection ] -> dorun collection
  | "doall", [ collection ] -> doall collection
  | "into", [ target; source ] -> into target source
  | "into-cat", [ target; source ] -> into_cat target source
  | "remove", _ -> Error.error "remove expects function and collection"
  | ("take-while" | "drop-while"), _ ->
      Error.error (name ^ " expects function and collection")
  | ("distinct" | "dedupe" | "sort" | "vec" | "set" | "butlast" | "dorun"
    | "doall"),
    _ -> Error.error (name ^ " expects 1 arguments")
  | "repeat", _ -> Error.error "repeat expects value, or count and value"
  | "cycle", _ -> Error.error "cycle expects 1 collection"
  | "interpose", _ -> Error.error "interpose expects separator and collection"
  | ("partition" | "partition-all"), _ ->
      Error.error (name ^ " expects size and collection")
  | ("take-last" | "drop-last"), _ ->
      Error.error (name ^ " expects count and collection")
  | "take-nth", _ -> Error.error "take-nth expects n and collection"
  | "split-at", _ -> Error.error "split-at expects count and collection"
  | "bounded-count", _ -> Error.error "bounded-count expects limit and collection"
  | "into", _ -> Error.error "into expects target and source collections"
  | _ -> Error.error ("unknown function " ^ name)
