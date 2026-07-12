open Types

let apply name args = Ocaml_ir.Apply (Ocaml_ir.Ident name, args)

let collection_to_list_expr collection =
  match collection.ty with
  | TList inner -> Ok (inner, collection.ocaml_expr)
  | TVector inner -> Ok (inner, apply "Rrbvec.to_list" [ collection.ocaml_expr ])
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             (inner, apply (set_module ^ ".elements") [ collection.ocaml_expr ]))
  | _ -> Error.error "collection value is not sequenceable"

let collection_from_list_expr collection_ty list_expr =
  match collection_ty with
  | TList _ -> list_expr
  | TVector _ -> apply "Rrbvec.of_list" [ list_expr ]
  | TSet inner -> (
      match Types.set_module_name inner with
      | Ok set_module -> apply (set_module ^ ".of_list") [ list_expr ]
      | Error _ -> list_expr)
  | _ -> list_expr

let take_list_expr count source =
  let name = "take__" in
  let n = Ocaml_ir.Ident "n" in
  let xs = Ocaml_ir.Ident "xs" in
  let body =
    Ocaml_ir.If
      ( Ocaml_ir.Infix ("<=", n, Ocaml_ir.Int 0),
        Ocaml_ir.List [],
        Ocaml_ir.Match
          ( xs,
            [ (Ocaml_ir.PList [], Ocaml_ir.List []);
              ( Ocaml_ir.PCons (Ocaml_ir.PVar "x", Ocaml_ir.PVar "rest"),
                Ocaml_ir.Cons
                  ( Ocaml_ir.Ident "x",
                    apply name
                      [ Ocaml_ir.Infix ("-", n, Ocaml_ir.Int 1);
                        Ocaml_ir.Ident "rest" ] ) ) ] ) )
  in
  Ocaml_ir.LetRec (name, [ Ocaml_ir.PVar "n"; Ocaml_ir.PVar "xs" ], body, [ count; source ])

let drop_list_expr count source =
  let name = "drop__" in
  let n = Ocaml_ir.Ident "n" in
  let xs = Ocaml_ir.Ident "xs" in
  let body =
    Ocaml_ir.If
      ( Ocaml_ir.Infix ("<=", n, Ocaml_ir.Int 0),
        xs,
        Ocaml_ir.Match
          ( xs,
            [ (Ocaml_ir.PList [], Ocaml_ir.List []);
              ( Ocaml_ir.PCons (Ocaml_ir.PAny, Ocaml_ir.PVar "rest"),
                apply name
                  [ Ocaml_ir.Infix ("-", n, Ocaml_ir.Int 1);
                    Ocaml_ir.Ident "rest" ] ) ] ) )
  in
  Ocaml_ir.LetRec (name, [ Ocaml_ir.PVar "n"; Ocaml_ir.PVar "xs" ], body, [ count; source ])

let remove fn collection =
  match (fn.ty, collection_to_list_expr collection) with
  | TFn ([ param_ty ], TBool), Ok (inner, list_expr) when Types.equal param_ty inner ->
      let filtered =
        apply "List.filter"
          [ Ocaml_ir.Fun
              ( [ Ocaml_ir.PVar "item" ],
                Ocaml_ir.Prefix
                  ( "not",
                    Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]) ) );
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
      let predicate = Ocaml_ir.Apply (fn.ocaml_expr, [ Ocaml_ir.Ident "item" ]) in
      let body =
        if name = "take-while" then
          Ocaml_ir.Match
            ( Ocaml_ir.Ident "xs",
              [ ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "rest"),
                  Ocaml_ir.If
                    ( predicate,
                      Ocaml_ir.Cons
                        (Ocaml_ir.Ident "item", apply rec_name [ Ocaml_ir.Ident "rest" ]),
                      Ocaml_ir.List [] ) );
                (Ocaml_ir.PAny, Ocaml_ir.List []) ] )
        else
          Ocaml_ir.Match
            ( Ocaml_ir.Ident "xs",
              [ ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "rest"),
                  Ocaml_ir.If
                    ( predicate,
                      apply rec_name [ Ocaml_ir.Ident "rest" ],
                      Ocaml_ir.Cons (Ocaml_ir.Ident "item", Ocaml_ir.Ident "rest") ) );
                (Ocaml_ir.PList [], Ocaml_ir.List []) ] )
      in
      let list_expr =
        Ocaml_ir.LetRec (rec_name, [ Ocaml_ir.PVar "xs" ], body, [ list_expr ])
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
        Ocaml_ir.Match
          ( Ocaml_ir.Ident "xs",
            [ (Ocaml_ir.PList [], apply "List.rev" [ Ocaml_ir.Ident "acc" ]);
              ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "rest"),
                Ocaml_ir.If
                  ( apply "List.mem" [ Ocaml_ir.Ident "item"; Ocaml_ir.Ident "seen" ],
                    apply "distinct"
                      [ Ocaml_ir.Ident "seen";
                        Ocaml_ir.Ident "acc";
                        Ocaml_ir.Ident "rest" ],
                    apply "distinct"
                      [ Ocaml_ir.Cons (Ocaml_ir.Ident "item", Ocaml_ir.Ident "seen");
                        Ocaml_ir.Cons (Ocaml_ir.Ident "item", Ocaml_ir.Ident "acc");
                        Ocaml_ir.Ident "rest" ] ) ) ] )
      in
      let list_expr =
        Ocaml_ir.LetRec
          ( "distinct",
            [ Ocaml_ir.PVar "seen"; Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "xs" ],
            body,
            [ Ocaml_ir.List []; Ocaml_ir.List []; list_expr ] )
      in
      Ok (typed_ir collection.ty (collection_from_list_expr collection.ty list_expr))

let dedupe collection =
  match collection_to_list_expr collection with
  | Error _ -> Error.error "dedupe expects a list, vector, or set"
  | Ok (_inner, list_expr) ->
      let body =
        Ocaml_ir.Match
          ( Ocaml_ir.Ident "xs",
            [ (Ocaml_ir.PList [], apply "List.rev" [ Ocaml_ir.Ident "acc" ]);
              ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "rest"),
                Ocaml_ir.Match
                  ( Ocaml_ir.Ident "acc",
                    [ ( Ocaml_ir.PCons (Ocaml_ir.PVar "previous", Ocaml_ir.PAny),
                        Ocaml_ir.If
                          ( Ocaml_ir.Infix
                              ("=", Ocaml_ir.Ident "previous", Ocaml_ir.Ident "item"),
                            apply "dedupe"
                              [ Ocaml_ir.Ident "acc"; Ocaml_ir.Ident "rest" ],
                            apply "dedupe"
                              [ Ocaml_ir.Cons
                                  (Ocaml_ir.Ident "item", Ocaml_ir.Ident "acc");
                                Ocaml_ir.Ident "rest" ] ) );
                      ( Ocaml_ir.PAny,
                        apply "dedupe"
                          [ Ocaml_ir.Cons (Ocaml_ir.Ident "item", Ocaml_ir.Ident "acc");
                            Ocaml_ir.Ident "rest" ] ) ] ) ) ] )
      in
      let list_expr =
        Ocaml_ir.LetRec
          ( "dedupe",
            [ Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "xs" ],
            body,
            [ Ocaml_ir.List []; list_expr ] )
      in
      Ok (typed_ir collection.ty (collection_from_list_expr collection.ty list_expr))

let sort collection =
  match collection_to_list_expr collection with
  | Error _ -> Error.error "sort expects a list, vector, or set"
  | Ok (inner, list_expr) ->
      Ok
        (typed_ir (TList inner)
           (apply "List.sort" [ Ocaml_ir.Ident "compare"; list_expr ]))

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
      Ok (typed_ir (TList inner) (apply "List.concat" [ Ocaml_ir.List exprs ]))

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
    let repeat_body =
      Ocaml_ir.If
        ( Ocaml_ir.Infix ("<=", Ocaml_ir.Ident "n", Ocaml_ir.Int 0),
          Ocaml_ir.Ident "acc",
          apply "repeat"
            [ Ocaml_ir.Cons (Ocaml_ir.Ident "value", Ocaml_ir.Ident "acc");
              Ocaml_ir.Infix ("-", Ocaml_ir.Ident "n", Ocaml_ir.Int 1) ] )
    in
    Ok
      (typed_ir (TList value.ty)
         (Ocaml_ir.Let
            ( [ (Ocaml_ir.PVar "value", value.ocaml_expr) ],
              Ocaml_ir.LetRec
                ( "repeat",
                  [ Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "n" ],
                  repeat_body,
                  [ Ocaml_ir.List []; count.ocaml_expr ] ) )))
  else Error.error "repeat count must be int"

let interpose separator collection =
  match collection_to_list_expr collection with
  | Error _ -> Error.error "interpose expects a collection"
  | Ok (inner, list_expr) ->
      if Types.equal separator.ty inner then
        let interpose_body =
          Ocaml_ir.Match
            ( Ocaml_ir.Ident "xs",
              [ (Ocaml_ir.PList [], apply "List.rev" [ Ocaml_ir.Ident "acc" ]);
                ( Ocaml_ir.PList [ Ocaml_ir.PVar "item" ],
                  apply "List.rev"
                    [ Ocaml_ir.Cons (Ocaml_ir.Ident "item", Ocaml_ir.Ident "acc") ]
                );
                ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "rest"),
                  apply "interpose"
                    [ Ocaml_ir.Cons
                        ( Ocaml_ir.Ident "separator",
                          Ocaml_ir.Cons
                            (Ocaml_ir.Ident "item", Ocaml_ir.Ident "acc") );
                      Ocaml_ir.Ident "rest" ] ) ] )
        in
        Ok
          (typed_ir (TList inner)
             (Ocaml_ir.Let
                ( [ (Ocaml_ir.PVar "separator", separator.ocaml_expr) ],
                  Ocaml_ir.LetRec
                    ( "interpose",
                      [ Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "xs" ],
                      interpose_body,
                      [ Ocaml_ir.List []; list_expr ] ) )))
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
        let collections = Ocaml_ir.Ident "collections" in
        let any_empty =
          apply "List.exists"
            [ Ocaml_ir.Fun
                ( [ Ocaml_ir.PVar "collection" ],
                  Ocaml_ir.Match
                    ( Ocaml_ir.Ident "collection",
                      [ (Ocaml_ir.PList [], Ocaml_ir.Bool true);
                        (Ocaml_ir.PAny, Ocaml_ir.Bool false) ] ) );
              collections ]
        in
        let body =
          Ocaml_ir.If
            ( any_empty,
              apply "List.rev" [ Ocaml_ir.Ident "acc" ],
              Ocaml_ir.Let
                ( [ (Ocaml_ir.PVar "heads", apply "List.map" [ Ocaml_ir.Ident "List.hd"; collections ]);
                    (Ocaml_ir.PVar "tails", apply "List.map" [ Ocaml_ir.Ident "List.tl"; collections ]) ],
                  apply "interleave"
                    [ apply "List.rev_append"
                        [ Ocaml_ir.Ident "heads"; Ocaml_ir.Ident "acc" ];
                      Ocaml_ir.Ident "tails" ] ) )
        in
        Ok
          (typed_ir (TList inner)
             (Ocaml_ir.LetRec
                ( "interleave",
                  [ Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "collections" ],
                  body,
                  [ Ocaml_ir.List []; Ocaml_ir.List exprs ] )))

let partition name size collection =
  if not (Types.equal size.ty TInt) then Error.error (name ^ " size must be int")
  else
    match collection_to_list_expr collection with
    | Error _ -> Error.error (name ^ " expects a collection")
    | Ok (inner, list_expr) ->
        let take_body return_done return_empty =
          Ocaml_ir.If
            ( Ocaml_ir.Infix ("=", Ocaml_ir.Ident "n", Ocaml_ir.Int 0),
              return_done
                (Ocaml_ir.Tuple
                   [ apply "List.rev" [ Ocaml_ir.Ident "acc" ];
                     Ocaml_ir.Ident "xs" ]),
              Ocaml_ir.Match
                ( Ocaml_ir.Ident "xs",
                  [ (Ocaml_ir.PList [], return_empty);
                    ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "rest"),
                      apply "take"
                        [ Ocaml_ir.Infix ("-", Ocaml_ir.Ident "n", Ocaml_ir.Int 1);
                          Ocaml_ir.Cons (Ocaml_ir.Ident "item", Ocaml_ir.Ident "acc");
                          Ocaml_ir.Ident "rest" ] ) ] ) )
        in
        let expr =
          if name = "partition-all" then
            let partition_body =
              Ocaml_ir.Match
                ( Ocaml_ir.Ident "xs",
                  [ (Ocaml_ir.PList [], apply "List.rev" [ Ocaml_ir.Ident "acc" ]);
                    ( Ocaml_ir.PAny,
                      Ocaml_ir.Let
                        ( [ ( Ocaml_ir.PTuple [ Ocaml_ir.PVar "chunk"; Ocaml_ir.PVar "rest" ],
                              apply "take"
                                [ Ocaml_ir.Ident "size";
                                  Ocaml_ir.List [];
                                  Ocaml_ir.Ident "xs" ] ) ],
                          apply "partition_all"
                            [ Ocaml_ir.Cons (Ocaml_ir.Ident "chunk", Ocaml_ir.Ident "acc");
                              Ocaml_ir.Ident "rest" ] ) ) ] )
            in
            Ocaml_ir.Let
              ( [ (Ocaml_ir.PVar "size", size.ocaml_expr) ],
                Ocaml_ir.LetRecIn
                  ( "take",
                    [ Ocaml_ir.PVar "n"; Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "xs" ],
                    take_body Fun.id
                      (Ocaml_ir.Tuple
                         [ apply "List.rev" [ Ocaml_ir.Ident "acc" ];
                           Ocaml_ir.List [] ]),
                    Ocaml_ir.LetRec
                      ( "partition_all",
                        [ Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "xs" ],
                        partition_body,
                        [ Ocaml_ir.List []; list_expr ] ) ) )
          else
            let partition_body =
              Ocaml_ir.Match
                ( apply "take"
                    [ Ocaml_ir.Ident "size"; Ocaml_ir.List []; Ocaml_ir.Ident "xs" ],
                  [ (Ocaml_ir.PConstructor ("None", None), apply "List.rev" [ Ocaml_ir.Ident "acc" ]);
                    ( Ocaml_ir.PConstructor
                        ( "Some",
                          Some
                            (Ocaml_ir.PTuple
                               [ Ocaml_ir.PVar "chunk"; Ocaml_ir.PVar "rest" ]) ),
                      apply "partition"
                        [ Ocaml_ir.Cons (Ocaml_ir.Ident "chunk", Ocaml_ir.Ident "acc");
                          Ocaml_ir.Ident "rest" ] ) ] )
            in
            Ocaml_ir.Let
              ( [ (Ocaml_ir.PVar "size", size.ocaml_expr) ],
                Ocaml_ir.LetRecIn
                  ( "take",
                    [ Ocaml_ir.PVar "n"; Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "xs" ],
                    take_body
                      (fun value -> Ocaml_ir.Constructor ("Some", Some value))
                      (Ocaml_ir.Constructor ("None", None)),
                    Ocaml_ir.LetRec
                      ( "partition",
                        [ Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "xs" ],
                        partition_body,
                        [ Ocaml_ir.List []; list_expr ] ) ) )
        in
        Ok (typed_ir (TList (TList inner)) expr)

let butlast collection =
  match collection_to_list_expr collection with
  | Error _ -> Error.error "butlast expects a collection"
  | Ok (_inner, list_expr) ->
      let body =
        Ocaml_ir.Match
          ( Ocaml_ir.Ident "xs",
            [ (Ocaml_ir.PList [], apply "List.rev" [ Ocaml_ir.Ident "acc" ]);
              (Ocaml_ir.PList [ Ocaml_ir.PAny ], apply "List.rev" [ Ocaml_ir.Ident "acc" ]);
              ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "rest"),
                apply "butlast"
                  [ Ocaml_ir.Cons (Ocaml_ir.Ident "item", Ocaml_ir.Ident "acc");
                    Ocaml_ir.Ident "rest" ] ) ] )
      in
      let list_expr =
        Ocaml_ir.LetRec
          ( "butlast",
            [ Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "xs" ],
            body,
            [ Ocaml_ir.List []; list_expr ] )
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
            [ Ocaml_ir.Int 0;
              Ocaml_ir.Infix
                ( "-",
                  apply "List.length" [ Ocaml_ir.Ident "source" ],
                  count.ocaml_expr ) ]
        in
        let result_expr =
          if name = "take-last" then
            drop_list_expr (Ocaml_ir.Ident count_name) (Ocaml_ir.Ident "source")
          else
            take_list_expr (Ocaml_ir.Ident count_name) (Ocaml_ir.Ident "source")
        in
        Ok
          (typed_ir collection.ty
             (collection_from_list_expr collection.ty
                (Ocaml_ir.Let
                   ( [ (Ocaml_ir.PVar "source", list_expr);
                       (Ocaml_ir.PVar count_name, count_expr) ],
                     result_expr ))))

let take_nth count collection =
  if not (Types.equal count.ty TInt) then Error.error "take-nth n must be int"
  else
    match collection_to_list_expr collection with
    | Error _ -> Error.error "take-nth expects a collection"
    | Ok (_inner, list_expr) ->
        let next_index = Ocaml_ir.Infix ("+", Ocaml_ir.Ident "index", Ocaml_ir.Int 1) in
        let body =
          Ocaml_ir.Match
            ( Ocaml_ir.Ident "xs",
              [ (Ocaml_ir.PList [], apply "List.rev" [ Ocaml_ir.Ident "acc" ]);
                ( Ocaml_ir.PCons (Ocaml_ir.PVar "item", Ocaml_ir.PVar "rest"),
                  Ocaml_ir.If
                    ( Ocaml_ir.Infix
                        ( "=",
                          Ocaml_ir.Infix ("mod", Ocaml_ir.Ident "index", count.ocaml_expr),
                          Ocaml_ir.Int 0 ),
                      apply "take_nth"
                        [ next_index;
                          Ocaml_ir.Cons (Ocaml_ir.Ident "item", Ocaml_ir.Ident "acc");
                          Ocaml_ir.Ident "rest" ],
                      apply "take_nth"
                        [ next_index;
                          Ocaml_ir.Ident "acc";
                          Ocaml_ir.Ident "rest" ] ) ) ] )
        in
        let list_expr =
          Ocaml_ir.LetRec
            ( "take_nth",
              [ Ocaml_ir.PVar "index"; Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "xs" ],
              body,
              [ Ocaml_ir.Int 0; Ocaml_ir.List []; list_expr ] )
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
            (take_list_expr count.ocaml_expr list_expr)
        in
        let right =
          collection_from_list_expr collection.ty
            (drop_list_expr count.ocaml_expr list_expr)
        in
        Ok (typed_ir (TVector collection.ty) (apply "Rrbvec.of_list" [ Ocaml_ir.List [ left; right ] ]))

let bounded_count limit collection =
  if not (Types.equal limit.ty TInt) then Error.error "bounded-count limit must be int"
  else
    match collection_to_list_expr collection with
    | Error _ -> Error.error "bounded-count expects a collection"
    | Ok (_inner, list_expr) ->
        Ok
          (typed_ir TInt
             (apply "min"
                [ limit.ocaml_expr; apply "List.length" [ list_expr ] ]))

let dorun collection =
  match collection_to_list_expr collection with
  | Error _ -> Error.error "dorun expects a collection"
  | Ok _ -> Ok (typed_ir TUnit Ocaml_ir.Unit)

let doall collection =
  match collection_to_list_expr collection with
  | Error _ -> Error.error "doall expects a collection"
  | Ok _ -> Ok collection

let into target source =
  match collection_to_list_expr source with
  | Error _ -> Error.error "into source must be a collection"
  | Ok (source_inner, source_list_expr) -> (
      match target.ty with
      | TVector target_inner when Types.equal target_inner source_inner -> (
          match source.ty with
          | TVector _ ->
              Ok
                (typed_ir target.ty
                   (apply "Rrbvec.append" [ target.ocaml_expr; source.ocaml_expr ]))
          | _ ->
              Ok
                (typed_ir target.ty
                   (apply "Rrbvec.append_list" [ target.ocaml_expr; source_list_expr ])))
      | TList target_inner when Types.equal target_inner source_inner ->
          Ok
            (typed_ir target.ty
               (apply "List.fold_left"
                  [ Ocaml_ir.Fun
                      ( [ Ocaml_ir.PVar "acc"; Ocaml_ir.PVar "item" ],
                        Ocaml_ir.Cons (Ocaml_ir.Ident "item", Ocaml_ir.Ident "acc") );
                    target.ocaml_expr;
                    source_list_expr ]))
      | TSet target_inner when Types.equal target_inner source_inner ->
          Types.set_module_name target_inner
          |> Result.map (fun set_module ->
                 typed_ir target.ty
                   (apply (set_module ^ ".of_list")
                      [ Ocaml_ir.Infix
                          ( "@",
                            apply (set_module ^ ".elements") [ target.ocaml_expr ],
                            source_list_expr ) ]))
      | TVector _ | TList _ | TSet _ ->
          Error.error "into source element type must match target element type"
      | _ -> Error.error "into target must be a collection")

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
  | "remove", _ -> Error.error "remove expects function and collection"
  | ("take-while" | "drop-while"), _ ->
      Error.error (name ^ " expects function and collection")
  | ("distinct" | "dedupe" | "sort" | "vec" | "set" | "butlast" | "dorun"
    | "doall"),
    _ -> Error.error (name ^ " expects 1 arguments")
  | "repeat", _ -> Error.error "repeat expects count and value"
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
