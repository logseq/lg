open Types

let apply name args = Ocaml_ir.Apply (Ocaml_ir.Ident name, args)

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
  let n = Ocaml_ir.Ident "n" in
  let xs = Ocaml_ir.Ident "xs" in
  let recursive_call =
    Ocaml_ir.Apply
      ( Ocaml_ir.Ident name,
        [ Ocaml_ir.Infix ("-", n, Ocaml_ir.Int 1); Ocaml_ir.Ident "rest" ] )
  in
  let body =
    Ocaml_ir.If
      ( Ocaml_ir.Infix ("<=", n, Ocaml_ir.Int 0),
        xs,
        Ocaml_ir.Match
          ( xs,
            [ (Ocaml_ir.PList [], Ocaml_ir.List []);
              ( Ocaml_ir.PCons (Ocaml_ir.PAny, Ocaml_ir.PVar "rest"),
                recursive_call ) ] ) )
  in
  Ocaml_ir.LetRec
    ( name,
      [ Ocaml_ir.PVar "n"; Ocaml_ir.PVar "xs" ],
      body,
      [ count; source ] )

let first_expr name collection =
  match collection.ty with
  | TList inner -> Ok (typed_ir inner (apply "List.hd" [ collection.ocaml_expr ]))
  | TVector inner ->
      Ok (typed_ir inner (apply "Rrbvec.nth" [ collection.ocaml_expr; Ocaml_ir.Int 0 ]))
  | _ -> Error.error (name ^ " expects a list or vector")

let next_expr name collection =
  let list_next target =
    Ocaml_ir.Match
      ( target,
        [ (Ocaml_ir.PList [], Ocaml_ir.List []);
          (Ocaml_ir.PCons (Ocaml_ir.PAny, Ocaml_ir.PVar "rest"), Ocaml_ir.Ident "rest") ] )
  in
  match collection.ty with
  | TList _ ->
      Ok (typed_ir collection.ty (list_next collection.ocaml_expr))
  | TVector _ ->
      Ok
        (typed_ir collection.ty
           (apply "Rrbvec.of_list"
              [ list_next (apply "Rrbvec.to_list" [ collection.ocaml_expr ]) ]))
  | _ -> Error.error (name ^ " expects a list or vector")

let nth_next_expr name collection count =
  if not (Types.equal count.ty TInt) then Error.error (name ^ " count must be int")
  else
    match collection.ty with
    | TList _ ->
        Ok
          (typed_ir collection.ty
             (drop_list_expr count.ocaml_expr collection.ocaml_expr))
    | TVector _ ->
        Ok
          (typed_ir collection.ty
             (apply "Rrbvec.of_list"
                [ drop_list_expr count.ocaml_expr
                    (apply "Rrbvec.to_list" [ collection.ocaml_expr ]) ]))
    | _ -> Error.error (name ^ " expects a list or vector")

let reverse_expr name collection =
  match collection.ty with
  | TList _ -> Ok (typed_ir collection.ty (apply "List.rev" [ collection.ocaml_expr ]))
  | TVector _ -> Ok (typed_ir collection.ty (apply "Rrbvec.rev" [ collection.ocaml_expr ]))
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
