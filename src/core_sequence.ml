open Types

let apply name args = Semantic_ir.Apply (Semantic_ir.Ident name, args)

let one_arg name args =
  match args with
  | [ arg ] -> Ok arg
  | _ -> Error.error (name ^ " expects 1 arguments")

let two_args name args =
  match args with
  | [ left; right ] -> Ok (left, right)
  | _ -> Error.error (name ^ " expects collection and count")

let drop_list_expr count source =
  let name = "drop__" in
  let n = Semantic_ir.Ident "n" in
  let xs = Semantic_ir.Ident "xs" in
  let recursive_call =
    Semantic_ir.Apply
      ( Semantic_ir.Ident name,
        [ Semantic_ir.Infix ("-", n, Semantic_ir.Int 1); Semantic_ir.Ident "rest" ] )
  in
  let body =
    Semantic_ir.If
      ( Semantic_ir.Infix ("<=", n, Semantic_ir.Int 0),
        xs,
        Semantic_ir.Match
          ( xs,
            [ (Semantic_ir.PList [], Semantic_ir.List []);
              ( Semantic_ir.PCons (Semantic_ir.PAny, Semantic_ir.PVar "rest"),
                recursive_call ) ] ) )
  in
  Semantic_ir.LetRec
    ( name,
      [ Semantic_ir.PVar "n"; Semantic_ir.PVar "xs" ],
      body,
      [ count; source ] )

let first_expr name collection =
  match collection.ty with
  | TList inner -> Ok (typed_ir inner (apply "List.hd" [ collection.semantic_expr ]))
  | TVector inner ->
      Ok (typed_ir inner (apply "Rrbvec.nth" [ collection.semantic_expr; Semantic_ir.Int 0 ]))
  | _ -> Error.error (name ^ " expects a list or vector")

let next_expr name collection =
  let list_next target =
    Semantic_ir.Match
      ( target,
        [ (Semantic_ir.PList [], Semantic_ir.List []);
          (Semantic_ir.PCons (Semantic_ir.PAny, Semantic_ir.PVar "rest"), Semantic_ir.Ident "rest") ] )
  in
  match collection.ty with
  | TList _ ->
      Ok (typed_ir collection.ty (list_next collection.semantic_expr))
  | TVector _ ->
      Ok
        (typed_ir collection.ty
           (apply "Rrbvec.of_list"
              [ list_next (apply "Rrbvec.to_list" [ collection.semantic_expr ]) ]))
  | _ -> Error.error (name ^ " expects a list or vector")

let nth_next_expr name collection count =
  if not (Types.equal count.ty TInt) then Error.error (name ^ " count must be int")
  else
    match collection.ty with
    | TList _ ->
        Ok
          (typed_ir collection.ty
             (drop_list_expr count.semantic_expr collection.semantic_expr))
    | TVector _ ->
        Ok
          (typed_ir collection.ty
             (apply "Rrbvec.of_list"
                [ drop_list_expr count.semantic_expr
                    (apply "Rrbvec.to_list" [ collection.semantic_expr ]) ]))
    | _ -> Error.error (name ^ " expects a list or vector")

let reverse_expr name collection =
  match collection.ty with
  | TList _ -> Ok (typed_ir collection.ty (apply "List.rev" [ collection.semantic_expr ]))
  | TVector _ -> Ok (typed_ir collection.ty (apply "Rrbvec.rev" [ collection.semantic_expr ]))
  | _ -> Error.error (name ^ " expects a list or vector")

let compile name args =
  match name with
  | "next" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> next_expr name collection)
  | "nthnext" | "nthrest" -> (
      match two_args name args with
      | Error _ as err -> err
      | Ok (collection, count) -> nth_next_expr name collection count)
  | "ffirst" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match first_expr name collection with
          | Error _ as err -> err
          | Ok first -> first_expr name first))
  | "fnext" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match next_expr name collection with
          | Error _ as err -> err
          | Ok next -> first_expr name next))
  | "nfirst" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match first_expr name collection with
          | Error _ as err -> err
          | Ok first -> next_expr name first))
  | "nnext" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match next_expr name collection with
          | Error _ as err -> err
          | Ok next -> next_expr name next))
  | "rseq" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> reverse_expr name collection)
  | _ -> Error.error ("unknown function " ^ name)
