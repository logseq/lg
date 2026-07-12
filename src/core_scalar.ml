open Types

let expect_int_args name args =
  if List.for_all (fun arg -> Types.equal arg.ty TInt) args then Ok ()
  else Error.error ("expected int arguments for " ^ name)

let one_arg name args =
  match args with
  | [ arg ] -> Ok arg
  | _ -> Error.error (name ^ " expects 1 arguments")

let two_args name args =
  match args with
  | [ left; right ] -> Ok (left, right)
  | _ -> Error.error (name ^ " expects 2 arguments")

let int_predicate name args build_code =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg ->
      if Types.equal arg.ty TInt then Ok (typed_ir TBool (build_code arg.ocaml_expr))
      else Ok (typed_ir TBool (Ocaml_ir.Bool false))

let int_unary name args build_code =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg -> (
      match expect_int_args name [ arg ] with
      | Error _ as err -> err
      | Ok () -> Ok (typed_ir TInt (build_code arg.ocaml_expr)))

let int_binary name args build_code =
  match two_args name args with
  | Error _ as err -> err
  | Ok (left, right) -> (
      match expect_int_args name [ left; right ] with
      | Error _ as err -> err
      | Ok () -> Ok (build_code left.ocaml_expr right.ocaml_expr))

let compile_boolean name args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg -> (
      match arg.ty with
      | TBool -> Ok (typed_ir TBool arg.ocaml_expr)
      | _ -> Ok (typed_ir TBool (Ocaml_ir.Bool true)))

let apply name args = Ocaml_ir.Apply (Ocaml_ir.Ident name, args)

let string_length expr = apply "String.length" [ expr ]

let string_get expr index = apply "String.get" [ expr; index ]

let string_sub expr start length = apply "String.sub" [ expr; start; length ]

let string_rindex_opt expr needle =
  apply "String.rindex_opt" [ expr; Ocaml_ir.Char needle ]

let string_concat left right = Ocaml_ir.Infix ("^", left, right)

let string_nonempty expr = Ocaml_ir.Infix (">", string_length expr, Ocaml_ir.Int 0)

let starts_with_colon expr =
  Ocaml_ir.Infix ("=", string_get expr (Ocaml_ir.Int 0), Ocaml_ir.Char ':')

let drop_first_char expr =
  string_sub expr (Ocaml_ir.Int 1)
    (Ocaml_ir.Infix ("-", string_length expr, Ocaml_ir.Int 1))

let string_and left right = Ocaml_ir.Infix ("&&", left, right)

let string_eq left right = Ocaml_ir.Infix ("=", left, right)

let identifier_body_expr name arg =
  match arg.ty with
  | TString | TSymbol | TKeyword ->
      let value = Ocaml_ir.Ident "value" in
      Ok
        (Ocaml_ir.Let
           ( [ (Ocaml_ir.PVar "value", arg.ocaml_expr) ],
             Ocaml_ir.If
               ( string_and (string_nonempty value) (starts_with_colon value),
                 drop_first_char value,
                 value ) ))
  | _ -> Error.error (name ^ " expects string, keyword, or symbol")

let substring_after_last_slash body =
  string_sub (Ocaml_ir.Ident body)
    (Ocaml_ir.Infix ("+", Ocaml_ir.Ident "index", Ocaml_ir.Int 1))
    (Ocaml_ir.Infix
       ( "-",
         Ocaml_ir.Infix ("-", string_length (Ocaml_ir.Ident body), Ocaml_ir.Ident "index"),
         Ocaml_ir.Int 1 ))

let identifier_name_expr arg =
  Ocaml_ir.Let
    ( [ (Ocaml_ir.PVar "body", arg) ],
      Ocaml_ir.Match
        ( string_rindex_opt (Ocaml_ir.Ident "body") '/',
          [ (Ocaml_ir.PConstructor ("None", None), Ocaml_ir.Ident "body");
            ( Ocaml_ir.PConstructor ("Some", Some (Ocaml_ir.PVar "index")),
              substring_after_last_slash "body" ) ] ) )

let identifier_namespace_expr arg =
  Ocaml_ir.Let
    ( [ (Ocaml_ir.PVar "body", arg) ],
      Ocaml_ir.Match
        ( string_rindex_opt (Ocaml_ir.Ident "body") '/',
          [ (Ocaml_ir.PConstructor ("None", None), Ocaml_ir.String "");
            ( Ocaml_ir.PConstructor ("Some", Some (Ocaml_ir.PVar "index")),
              string_sub (Ocaml_ir.Ident "body") (Ocaml_ir.Int 0) (Ocaml_ir.Ident "index") ) ] ) )

let keyword_one_arg_expr arg =
  let value = Ocaml_ir.Ident "value" in
  Ocaml_ir.Let
    ( [ (Ocaml_ir.PVar "value", arg) ],
      Ocaml_ir.If
        ( string_and (string_nonempty value) (starts_with_colon value),
          value,
          string_concat (Ocaml_ir.String ":") value ) )

let scoped_keyword_expr namespace name =
  Ocaml_ir.Let
    ( [ (Ocaml_ir.PVar "namespace", namespace); (Ocaml_ir.PVar "name", name) ],
      Ocaml_ir.If
        ( string_eq (Ocaml_ir.Ident "namespace") (Ocaml_ir.String ""),
          string_concat (Ocaml_ir.String ":") (Ocaml_ir.Ident "name"),
          string_concat
            (string_concat
               (string_concat (Ocaml_ir.String ":") (Ocaml_ir.Ident "namespace"))
               (Ocaml_ir.String "/"))
            (Ocaml_ir.Ident "name") ) )

let namespaced_symbol_expr namespace name =
  Ocaml_ir.Let
    ( [ (Ocaml_ir.PVar "namespace", namespace); (Ocaml_ir.PVar "name", name) ],
      Ocaml_ir.If
        ( string_eq (Ocaml_ir.Ident "namespace") (Ocaml_ir.String ""),
          Ocaml_ir.Ident "name",
          string_concat
            (string_concat (Ocaml_ir.Ident "namespace") (Ocaml_ir.String "/"))
            (Ocaml_ir.Ident "name") ) )

let compile_name name args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg -> (
      match arg.ty with
      | TString -> Ok (typed_ir TString arg.ocaml_expr)
      | TKeyword | TSymbol -> (
          match identifier_body_expr name arg with
          | Error _ as err -> err
          | Ok body -> Ok (typed_ir TString (identifier_name_expr body)))
      | _ -> Error.error "name expects keyword, string, or symbol")

let compile_keyword name args =
  match args with
  | [ arg ] -> (
      match arg.ty with
      | TKeyword -> Ok (typed_ir TKeyword arg.ocaml_expr)
      | TString | TSymbol -> Ok (typed_ir TKeyword (keyword_one_arg_expr arg.ocaml_expr))
      | _ -> Error.error "keyword expects keyword, string, or symbol")
  | [ namespace_arg; name_arg ] -> (
      match (identifier_body_expr name namespace_arg, identifier_body_expr name name_arg) with
      | Error _, _ | _, Error _ ->
          Error.error "keyword namespace and name must be string, keyword, or symbol"
      | Ok namespace_expr, Ok name_expr ->
          Ok (typed_ir TKeyword (scoped_keyword_expr namespace_expr name_expr)))
  | _ -> Error.error "keyword expects 1 or 2 arguments"

let compile_namespace name args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg -> (
      match arg.ty with
      | TKeyword | TSymbol ->
          Result.map
            (fun body -> typed_ir TString (identifier_namespace_expr body))
            (identifier_body_expr name arg)
      | _ -> Error.error "namespace expects keyword or symbol")

let compile_symbol name args =
  match args with
  | [ arg ] -> (
      match identifier_body_expr name arg with
      | Error _ -> Error.error "symbol expects string, keyword, or symbol"
      | Ok expr -> Ok (typed_ir TSymbol expr))
  | [ namespace_arg; name_arg ] -> (
      match (identifier_body_expr name namespace_arg, identifier_body_expr name name_arg) with
      | Error _, _ | _, Error _ ->
          Error.error "symbol namespace and name must be string, keyword, or symbol"
      | Ok namespace_expr, Ok name_expr ->
          Ok (typed_ir TSymbol (namespaced_symbol_expr namespace_expr name_expr)))
  | _ -> Error.error "symbol expects 1 or 2 arguments"

let compile name args =
  match name with
  | "integer?" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok arg -> Ok (typed_ir TBool (Ocaml_ir.Bool (Types.equal arg.ty TInt))))
  | "nat-int?" -> int_predicate name args (fun expr -> Ocaml_ir.Infix (">=", expr, Ocaml_ir.Int 0))
  | "pos-int?" -> int_predicate name args (fun expr -> Ocaml_ir.Infix (">", expr, Ocaml_ir.Int 0))
  | "neg-int?" -> int_predicate name args (fun expr -> Ocaml_ir.Infix ("<", expr, Ocaml_ir.Int 0))
  | "boolean" -> compile_boolean name args
  | "bit-set" ->
      int_binary name args (fun left right -> typed_ir TInt (Ocaml_ir.Infix ("lor", left, Ocaml_ir.Infix ("lsl", Ocaml_ir.Int 1, right))))
  | "bit-clear" ->
      int_binary name args (fun left right -> typed_ir TInt (Ocaml_ir.Infix ("land", left, Ocaml_ir.Prefix ("lnot", Ocaml_ir.Infix ("lsl", Ocaml_ir.Int 1, right)))))
  | "bit-flip" ->
      int_binary name args (fun left right -> typed_ir TInt (Ocaml_ir.Infix ("lxor", left, Ocaml_ir.Infix ("lsl", Ocaml_ir.Int 1, right))))
  | "bit-test" ->
      int_binary name args (fun left right -> typed_ir TBool (Ocaml_ir.Infix ("<>", Ocaml_ir.Infix ("land", left, Ocaml_ir.Infix ("lsl", Ocaml_ir.Int 1, right)), Ocaml_ir.Int 0)))
  | "bit-shift-right-zero-fill" ->
      int_binary name args (fun left right -> typed_ir TInt (Ocaml_ir.Infix ("lsr", left, right)))
  | "unchecked-add" | "unchecked-add-int" ->
      int_binary name args (fun left right -> typed_ir TInt (Ocaml_ir.Infix ("+", left, right)))
  | "unchecked-subtract" | "unchecked-subtract-int" ->
      int_binary name args (fun left right -> typed_ir TInt (Ocaml_ir.Infix ("-", left, right)))
  | "unchecked-multiply" | "unchecked-multiply-int" ->
      int_binary name args (fun left right -> typed_ir TInt (Ocaml_ir.Infix ("*", left, right)))
  | "unchecked-divide-int" ->
      int_binary name args (fun left right -> typed_ir TInt (Ocaml_ir.Infix ("/", left, right)))
  | "unchecked-remainder-int" ->
      int_binary name args (fun left right -> typed_ir TInt (Ocaml_ir.Infix ("mod", left, right)))
  | "unchecked-inc" | "unchecked-inc-int" -> int_unary name args (fun expr -> Ocaml_ir.Infix ("+", expr, Ocaml_ir.Int 1))
  | "unchecked-dec" | "unchecked-dec-int" -> int_unary name args (fun expr -> Ocaml_ir.Infix ("-", expr, Ocaml_ir.Int 1))
  | "unchecked-negate" | "unchecked-negate-int" -> int_unary name args (fun expr -> Ocaml_ir.Prefix ("~-", expr))
  | "name" -> compile_name name args
  | "namespace" -> compile_namespace name args
  | "keyword" -> compile_keyword name args
  | "symbol" -> compile_symbol name args
  | _ -> Error.error ("unknown function " ^ name)
