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

let count collection =
  match collection.ty with
  | TList _ -> Ok (typed_ir TInt (apply "List.length" [ collection.semantic_expr ]))
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir TInt (apply (set_module ^ ".cardinal") [ collection.semantic_expr ]))
  | TVector _ -> Ok (typed_ir TInt (apply "Rrbvec.length" [ collection.semantic_expr ]))
  | TRecord fields | TNamed_record { fields; _ } ->
      Ok (typed_ir TInt (Semantic_ir.Int (List.length fields)))
  | TString -> Ok (typed_ir TInt (apply "String.length" [ collection.semantic_expr ]))
  | _ -> Error.error "count expects a collection or string"

let first collection =
  match collection.ty with
  | TList inner -> Ok (typed_ir inner (apply "List.hd" [ collection.semantic_expr ]))
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir inner (apply (set_module ^ ".min_elt") [ collection.semantic_expr ]))
  | TVector inner -> Ok (typed_ir inner (apply "Rrbvec.nth" [ collection.semantic_expr; Semantic_ir.Int 0 ]))
  | _ -> Error.error "first expects a list, vector, or set"

let second collection =
  match collection.ty with
  | TList inner -> Ok (typed_ir inner (apply "List.nth" [ collection.semantic_expr; Semantic_ir.Int 1 ]))
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir inner
               (apply "List.nth"
                  [ apply (set_module ^ ".elements") [ collection.semantic_expr ]; Semantic_ir.Int 1 ]))
  | TVector inner -> Ok (typed_ir inner (apply "Rrbvec.nth" [ collection.semantic_expr; Semantic_ir.Int 1 ]))
  | _ -> Error.error "second expects a list, vector, or set"

let last collection =
  match collection.ty with
  | TList inner ->
      Ok (typed_ir inner (apply "List.hd" [ apply "List.rev" [ collection.semantic_expr ] ]))
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir inner (apply (set_module ^ ".max_elt") [ collection.semantic_expr ]))
  | TVector inner ->
      Ok (typed_ir inner (apply "Option.get" [ apply "Rrbvec.peek_back" [ collection.semantic_expr ] ]))
  | _ -> Error.error "last expects a list, vector, or set"

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

let rest collection =
  let list_rest target =
    Semantic_ir.Match
      ( target,
        [ (Semantic_ir.PList [], Semantic_ir.List []);
          (Semantic_ir.PCons (Semantic_ir.PAny, Semantic_ir.PVar "rest"), Semantic_ir.Ident "rest") ] )
  in
  match collection.ty with
  | TList _ ->
      Ok (typed_ir collection.ty (list_rest collection.semantic_expr))
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir collection.ty
               (apply (set_module ^ ".of_list")
                  [ list_rest (apply (set_module ^ ".elements") [ collection.semantic_expr ]) ]))
  | TVector _ ->
      Ok
        (typed_ir collection.ty
           (apply "Rrbvec.of_list"
              [ list_rest (apply "Rrbvec.to_list" [ collection.semantic_expr ]) ]))
  | _ -> Error.error "rest expects a list, vector, or set"

let seq collection =
  match collection.ty with
  | TList _ | TVector _ | TSet _ -> Ok collection
  | _ -> Error.error "seq expects a collection"

let empty_question collection =
  match collection.ty with
  | TList _ -> Ok (typed_ir TBool (Semantic_ir.Infix ("=", collection.semantic_expr, Semantic_ir.List [])) )
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir TBool (apply (set_module ^ ".is_empty") [ collection.semantic_expr ]))
  | TVector _ -> Ok (typed_ir TBool (apply "Rrbvec.is_empty" [ collection.semantic_expr ]))
  | TString -> Ok (typed_ir TBool (Semantic_ir.Infix ("=", collection.semantic_expr, Semantic_ir.String "")))
  | _ -> Error.error "empty? expects a collection or string"

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
    match collection.ty with
    | TList _ ->
        let expr =
          if name = "take" then take_list_expr count.semantic_expr collection.semantic_expr
          else drop_list_expr count.semantic_expr collection.semantic_expr
        in
        Ok (typed_ir collection.ty expr)
    | TVector _ ->
        let list_expr = apply "Rrbvec.to_list" [ collection.semantic_expr ] in
        let expr =
          if name = "take" then take_list_expr count.semantic_expr list_expr
          else drop_list_expr count.semantic_expr list_expr
        in
        Ok (typed_ir collection.ty (apply "Rrbvec.of_list" [ expr ]))
    | _ -> Error.error (name ^ " expects a list or vector")

let reverse collection =
  match collection.ty with
  | TList _ -> Ok (typed_ir collection.ty (apply "List.rev" [ collection.semantic_expr ]))
  | TVector _ -> Ok (typed_ir collection.ty (apply "Rrbvec.rev" [ collection.semantic_expr ]))
  | _ -> Error.error "reverse expects a list or vector"

let compile name args =
  match name with
  | "count" | "first" | "second" | "last" | "peek" | "pop" | "rest" | "seq"
  | "empty?" | "empty" | "reverse" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match name with
          | "count" -> count collection
          | "first" -> first collection
          | "second" -> second collection
          | "last" -> last collection
          | "peek" -> peek collection
          | "pop" -> pop collection
          | "rest" -> rest collection
          | "seq" -> seq collection
          | "empty?" -> empty_question collection
          | "empty" -> empty collection
          | "reverse" -> reverse collection
          | _ -> assert false))
  | "take" | "drop" -> (
      match two_args name args with
      | Error _ as err -> err
      | Ok (count, collection) -> take_drop name count collection)
  | _ -> Error.error ("unknown function " ^ name)
