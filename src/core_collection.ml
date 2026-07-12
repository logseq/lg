open Types

let one_arg name args =
  match args with
  | [ arg ] -> Ok arg
  | _ -> Error.error (name ^ " expects 1 arguments")

let two_args name args =
  match args with
  | [ left; right ] -> Ok (left, right)
  | _ -> Error.error (name ^ " expects count and collection")

let apply name args = Ocaml_ir.Apply (Ocaml_ir.Ident name, args)

let count collection =
  match collection.ty with
  | TList _ -> Ok (typed_ir TInt (apply "List.length" [ collection.ocaml_expr ]))
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir TInt (apply (set_module ^ ".cardinal") [ collection.ocaml_expr ]))
  | TVector _ -> Ok (typed_ir TInt (apply "Rrbvec.length" [ collection.ocaml_expr ]))
  | TRecord fields | TNamed_record { fields; _ } ->
      Ok (typed_ir TInt (Ocaml_ir.Int (List.length fields)))
  | TString -> Ok (typed_ir TInt (apply "String.length" [ collection.ocaml_expr ]))
  | _ -> Error.error "count expects a collection or string"

let first collection =
  match collection.ty with
  | TList inner -> Ok (typed_ir inner (apply "List.hd" [ collection.ocaml_expr ]))
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir inner (apply (set_module ^ ".min_elt") [ collection.ocaml_expr ]))
  | TVector inner -> Ok (typed_ir inner (apply "Rrbvec.nth" [ collection.ocaml_expr; Ocaml_ir.Int 0 ]))
  | _ -> Error.error "first expects a list, vector, or set"

let second collection =
  match collection.ty with
  | TList inner -> Ok (typed_ir inner (apply "List.nth" [ collection.ocaml_expr; Ocaml_ir.Int 1 ]))
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir inner
               (apply "List.nth"
                  [ apply (set_module ^ ".elements") [ collection.ocaml_expr ]; Ocaml_ir.Int 1 ]))
  | TVector inner -> Ok (typed_ir inner (apply "Rrbvec.nth" [ collection.ocaml_expr; Ocaml_ir.Int 1 ]))
  | _ -> Error.error "second expects a list, vector, or set"

let last collection =
  match collection.ty with
  | TList inner ->
      Ok (typed_ir inner (apply "List.hd" [ apply "List.rev" [ collection.ocaml_expr ] ]))
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir inner (apply (set_module ^ ".max_elt") [ collection.ocaml_expr ]))
  | TVector inner ->
      Ok (typed_ir inner (apply "Option.get" [ apply "Rrbvec.peek_back" [ collection.ocaml_expr ] ]))
  | _ -> Error.error "last expects a list, vector, or set"

let peek collection =
  match collection.ty with
  | TList inner -> Ok (typed_ir inner (apply "List.hd" [ collection.ocaml_expr ]))
  | TVector inner ->
      Ok (typed_ir inner (apply "Option.get" [ apply "Rrbvec.peek_back" [ collection.ocaml_expr ] ]))
  | _ -> Error.error "peek expects a list or vector"

let pop collection =
  match collection.ty with
  | TList _ -> Ok (typed_ir collection.ty (apply "List.tl" [ collection.ocaml_expr ]))
  | TVector _ ->
      Ok
        (typed_ir collection.ty
           (apply "snd" [ apply "Option.get" [ apply "Rrbvec.pop_back" [ collection.ocaml_expr ] ] ]))
  | _ -> Error.error "pop expects a list or vector"

let rest collection =
  let list_rest target =
    Ocaml_ir.Match
      ( target,
        [ (Ocaml_ir.PList [], Ocaml_ir.List []);
          (Ocaml_ir.PCons (Ocaml_ir.PAny, Ocaml_ir.PVar "rest"), Ocaml_ir.Ident "rest") ] )
  in
  match collection.ty with
  | TList _ ->
      Ok (typed_ir collection.ty (list_rest collection.ocaml_expr))
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir collection.ty
               (apply (set_module ^ ".of_list")
                  [ list_rest (apply (set_module ^ ".elements") [ collection.ocaml_expr ]) ]))
  | TVector _ ->
      Ok
        (typed_ir collection.ty
           (apply "Rrbvec.of_list"
              [ list_rest (apply "Rrbvec.to_list" [ collection.ocaml_expr ]) ]))
  | _ -> Error.error "rest expects a list, vector, or set"

let seq collection =
  match collection.ty with
  | TList _ | TVector _ | TSet _ -> Ok collection
  | _ -> Error.error "seq expects a collection"

let empty_question collection =
  match collection.ty with
  | TList _ -> Ok (typed_ir TBool (Ocaml_ir.Infix ("=", collection.ocaml_expr, Ocaml_ir.List [])) )
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir TBool (apply (set_module ^ ".is_empty") [ collection.ocaml_expr ]))
  | TVector _ -> Ok (typed_ir TBool (apply "Rrbvec.is_empty" [ collection.ocaml_expr ]))
  | TString -> Ok (typed_ir TBool (Ocaml_ir.Infix ("=", collection.ocaml_expr, Ocaml_ir.String "")))
  | _ -> Error.error "empty? expects a collection or string"

let empty collection =
  match collection.ty with
  | TList _ -> Ok (typed_ir collection.ty (Ocaml_ir.List []))
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed_ir collection.ty (Ocaml_ir.Ident (set_module ^ ".empty")))
  | TVector _ -> Ok (typed_ir collection.ty (Ocaml_ir.Ident "Rrbvec.empty"))
  | TString -> Ok (typed_ir TString (Ocaml_ir.String ""))
  | _ -> Error.error "empty expects a collection or string"

let take_list_expr count source =
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
                    apply "take__"
                      [ Ocaml_ir.Infix ("-", n, Ocaml_ir.Int 1);
                        Ocaml_ir.Ident "rest" ] ) ) ] ) )
  in
  Ocaml_ir.LetRec ("take__", [ Ocaml_ir.PVar "n"; Ocaml_ir.PVar "xs" ], body, [ count; source ])

let drop_list_expr count source =
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
                apply "drop__"
                  [ Ocaml_ir.Infix ("-", n, Ocaml_ir.Int 1);
                    Ocaml_ir.Ident "rest" ] ) ] ) )
  in
  Ocaml_ir.LetRec ("drop__", [ Ocaml_ir.PVar "n"; Ocaml_ir.PVar "xs" ], body, [ count; source ])

let take_drop name count collection =
  if not (Types.equal count.ty TInt) then Error.error (name ^ " count must be int")
  else
    match collection.ty with
    | TList _ ->
        let expr =
          if name = "take" then take_list_expr count.ocaml_expr collection.ocaml_expr
          else drop_list_expr count.ocaml_expr collection.ocaml_expr
        in
        Ok (typed_ir collection.ty expr)
    | TVector _ ->
        let list_expr = apply "Rrbvec.to_list" [ collection.ocaml_expr ] in
        let expr =
          if name = "take" then take_list_expr count.ocaml_expr list_expr
          else drop_list_expr count.ocaml_expr list_expr
        in
        Ok (typed_ir collection.ty (apply "Rrbvec.of_list" [ expr ]))
    | _ -> Error.error (name ^ " expects a list or vector")

let reverse collection =
  match collection.ty with
  | TList _ -> Ok (typed_ir collection.ty (apply "List.rev" [ collection.ocaml_expr ]))
  | TVector _ -> Ok (typed_ir collection.ty (apply "Rrbvec.rev" [ collection.ocaml_expr ]))
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
