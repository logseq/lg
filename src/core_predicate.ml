open Types

let one_arg name args =
  match args with
  | [ arg ] -> Ok arg
  | _ -> Error.error (name ^ " expects 1 arguments")

let apply name args = Ocaml_ir.Apply (Ocaml_ir.Ident name, args)

let identifier_body_expr expr =
  Ocaml_ir.Let
    ( [ (Ocaml_ir.PVar "value", expr) ],
      Ocaml_ir.If
        ( Ocaml_ir.Infix
            ( "&&",
              Ocaml_ir.Infix
                (">", apply "String.length" [ Ocaml_ir.Ident "value" ], Ocaml_ir.Int 0),
              Ocaml_ir.Infix
                ("=", apply "String.get" [ Ocaml_ir.Ident "value"; Ocaml_ir.Int 0 ], Ocaml_ir.Char ':')
            ),
          apply "String.sub"
            [ Ocaml_ir.Ident "value";
              Ocaml_ir.Int 1;
              Ocaml_ir.Infix
                ("-", apply "String.length" [ Ocaml_ir.Ident "value" ], Ocaml_ir.Int 1) ],
          Ocaml_ir.Ident "value" ) )

let has_slash expr =
  apply "String.contains" [ identifier_body_expr expr; Ocaml_ir.Char '/' ]

let compile name args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg ->
      let bool value = Ok (typed_ir TBool value) in
      let static_bool value = bool (Ocaml_ir.Bool value) in
      match name with
      | "any?" -> static_bool true
      | "rational?" -> static_bool (Types.equal arg.ty TInt)
      | "ratio?" | "float?" | "double?" | "decimal?" -> static_bool false
      | "symbol?" -> static_bool (Types.equal arg.ty TSymbol)
      | "simple-symbol?" -> (
          match arg.ty with
          | TSymbol -> bool (Ocaml_ir.Prefix ("not", has_slash arg.ocaml_expr))
          | _ -> static_bool false)
      | "qualified-symbol?" -> (
          match arg.ty with
          | TSymbol -> bool (has_slash arg.ocaml_expr)
          | _ -> static_bool false)
      | "simple-keyword?" -> (
          match arg.ty with
          | TKeyword -> bool (Ocaml_ir.Prefix ("not", has_slash arg.ocaml_expr))
          | _ -> static_bool false)
      | "qualified-keyword?" -> (
          match arg.ty with
          | TKeyword -> bool (has_slash arg.ocaml_expr)
          | _ -> static_bool false)
      | "ident?" ->
          static_bool (match arg.ty with TKeyword | TSymbol -> true | _ -> false)
      | "simple-ident?" -> (
          match arg.ty with
          | TKeyword | TSymbol -> bool (Ocaml_ir.Prefix ("not", has_slash arg.ocaml_expr))
          | _ -> static_bool false)
      | "qualified-ident?" -> (
          match arg.ty with
          | TKeyword | TSymbol -> bool (has_slash arg.ocaml_expr)
          | _ -> static_bool false)
      | "sequential?" ->
          static_bool (match arg.ty with TList _ | TVector _ -> true | _ -> false)
      | "reversible?" ->
          static_bool (match arg.ty with TString | TList _ | TVector _ -> true | _ -> false)
      | "sorted?" -> static_bool false
      | _ -> Error.error ("unknown function " ^ name)
