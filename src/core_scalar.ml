open Types

let accepts_int ty =
  Types.assignable ~policy:Host_boundary ~expected:TInt ~actual:ty

let expect_int_args name args =
  if List.for_all (fun arg -> accepts_int arg.ty) args then Ok ()
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
      if accepts_int arg.ty then
        Ok (typed_ir TBool (build_code arg.semantic_expr))
      else Ok (typed_ir TBool (Semantic_ir.Bool false))

let int_unary name args build_code =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg -> (
      match expect_int_args name [ arg ] with
      | Error _ as err -> err
      | Ok () -> Ok (typed_ir TInt (build_code arg.semantic_expr)))

let int_binary name args build_code =
  match two_args name args with
  | Error _ as err -> err
  | Ok (left, right) -> (
      match expect_int_args name [ left; right ] with
      | Error _ as err -> err
      | Ok () -> Ok (build_code left.semantic_expr right.semantic_expr))

let apply name args = Semantic_ir.Apply (Semantic_ir.Ident name, args)
let string_length expr = apply "String.length" [ expr ]
let string_get expr index = apply "String.get" [ expr; index ]
let string_sub expr start length = apply "String.sub" [ expr; start; length ]

let string_rindex_opt expr needle =
  apply "String.rindex_opt" [ expr; Semantic_ir.Char needle ]

let string_concat left right = Semantic_ir.Infix ("^", left, right)

let string_nonempty expr =
  Semantic_ir.Infix (">", string_length expr, Semantic_ir.Int 0)

let starts_with_colon expr =
  Semantic_ir.Infix
    ("=", string_get expr (Semantic_ir.Int 0), Semantic_ir.Char ':')

let drop_first_char expr =
  string_sub expr (Semantic_ir.Int 1)
    (Semantic_ir.Infix ("-", string_length expr, Semantic_ir.Int 1))

let string_and left right = Semantic_ir.Infix ("&&", left, right)
let string_eq left right = Semantic_ir.Infix ("=", left, right)

let identifier_body_expr name arg =
  let normalize expression =
      let value = Semantic_ir.Ident "value" in
    Semantic_ir.Let
      ( [ (Semantic_ir.PVar "value", expression) ],
             Semantic_ir.If
               ( string_and (string_nonempty value) (starts_with_colon value),
                 drop_first_char value,
            value ) )
  in
  match arg.ty with
  | TString | TSymbol | TKeyword | TUnknown -> Ok (normalize arg.semantic_expr)
  | TNullable TString | TOcaml_app ("option", [ TString ]) ->
      Ok
        (Semantic_ir.Match
           ( arg.semantic_expr,
             [
               (Semantic_ir.PConstructor ("None", None), Semantic_ir.String "");
               ( Semantic_ir.PConstructor
                   ("Some", Some (Semantic_ir.PVar "value")),
                 normalize (Semantic_ir.Ident "value") );
             ] ))
  | _ -> Error.error (name ^ " expects string, keyword, or symbol")

let substring_after_last_slash body =
  string_sub (Semantic_ir.Ident body)
    (Semantic_ir.Infix ("+", Semantic_ir.Ident "index", Semantic_ir.Int 1))
    (Semantic_ir.Infix
       ( "-",
         Semantic_ir.Infix
           ( "-",
             string_length (Semantic_ir.Ident body),
             Semantic_ir.Ident "index" ),
         Semantic_ir.Int 1 ))

let identifier_name_expr arg =
  Semantic_ir.Let
    ( [ (Semantic_ir.PVar "body", arg) ],
      Semantic_ir.Match
        ( string_rindex_opt (Semantic_ir.Ident "body") '/',
          [
            (Semantic_ir.PConstructor ("None", None), Semantic_ir.Ident "body");
            ( Semantic_ir.PConstructor ("Some", Some (Semantic_ir.PVar "index")),
              substring_after_last_slash "body" );
          ] ) )

let identifier_namespace_expr arg =
  Semantic_ir.Let
    ( [ (Semantic_ir.PVar "body", arg) ],
      Semantic_ir.Match
        ( string_rindex_opt (Semantic_ir.Ident "body") '/',
          [
            ( Semantic_ir.PConstructor ("None", None),
              Semantic_ir.Constructor ("None", None) );
            ( Semantic_ir.PConstructor ("Some", Some (Semantic_ir.PVar "index")),
              Semantic_ir.Constructor
                ( "Some",
                  Some
                    (string_sub (Semantic_ir.Ident "body") (Semantic_ir.Int 0)
                       (Semantic_ir.Ident "index")) ) );
          ] ) )

let keyword_one_arg_expr arg =
  let value = Semantic_ir.Ident "value" in
  Semantic_ir.Let
    ( [ (Semantic_ir.PVar "value", arg) ],
      Semantic_ir.If
        ( string_and (string_nonempty value) (starts_with_colon value),
          value,
          string_concat (Semantic_ir.String ":") value ) )

let scoped_keyword_expr namespace name =
  Semantic_ir.Let
    ( [
        (Semantic_ir.PVar "namespace", namespace);
        (Semantic_ir.PVar "name", name);
      ],
      Semantic_ir.If
        ( string_eq (Semantic_ir.Ident "namespace") (Semantic_ir.String ""),
          string_concat (Semantic_ir.String ":") (Semantic_ir.Ident "name"),
          string_concat
            (string_concat
               (string_concat (Semantic_ir.String ":")
                  (Semantic_ir.Ident "namespace"))
               (Semantic_ir.String "/"))
            (Semantic_ir.Ident "name") ) )

let namespaced_symbol_expr namespace name =
  Semantic_ir.Let
    ( [
        (Semantic_ir.PVar "namespace", namespace);
        (Semantic_ir.PVar "name", name);
      ],
      Semantic_ir.If
        ( string_eq (Semantic_ir.Ident "namespace") (Semantic_ir.String ""),
          Semantic_ir.Ident "name",
          string_concat
            (string_concat (Semantic_ir.Ident "namespace")
               (Semantic_ir.String "/"))
            (Semantic_ir.Ident "name") ) )

let compile_name name args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg -> (
      match arg.ty with
      | TString -> Ok (typed_ir TString arg.semantic_expr)
      | ty when Types.is_dynamic ty -> (
          let identifier =
            apply "Lg_runtime.Runtime_dynamic.as_identifier"
              [ arg.semantic_expr ]
          in
          match
            identifier_body_expr name
              { arg with semantic_expr = identifier; ty = TSymbol }
          with
          | Error _ as error -> error
          | Ok body -> Ok (typed_ir TString (identifier_name_expr body)))
      | TKeyword | TSymbol -> (
          match identifier_body_expr name arg with
          | Error _ as err -> err
          | Ok body -> Ok (typed_ir TString (identifier_name_expr body)))
      | _ -> Error.error "name expects keyword, string, or symbol")

let compile_keyword name args =
  match args with
  | [ arg ] -> (
      match arg.ty with
      | TKeyword -> Ok (typed_ir TKeyword arg.semantic_expr)
      | ty when Types.is_dynamic ty ->
          Ok
            (typed_ir TKeyword
               (keyword_one_arg_expr
                  (apply "Lg_runtime.Runtime_dynamic.as_identifier"
                     [ arg.semantic_expr ])))
      | TString | TSymbol | TUnknown ->
          Ok (typed_ir TKeyword (keyword_one_arg_expr arg.semantic_expr))
      | _ -> Error.error "keyword expects keyword, string, or symbol")
  | [ namespace_arg; name_arg ] -> (
      match
        ( identifier_body_expr name namespace_arg,
          identifier_body_expr name name_arg )
      with
      | Error _, _ | _, Error _ ->
          Error.error
            "keyword namespace and name must be string, keyword, or symbol"
      | Ok namespace_expr, Ok name_expr ->
          Ok (typed_ir TKeyword (scoped_keyword_expr namespace_expr name_expr)))
  | _ -> Error.error "keyword expects 1 or 2 arguments"

let compile_namespace name args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg -> (
      match arg.ty with
      | ty when Types.is_dynamic ty ->
          let identifier =
            apply "Lg_runtime.Runtime_dynamic.as_named_identifier"
              [ arg.semantic_expr ]
          in
          Result.map
            (fun body ->
              typed_ir
                (TOcaml_app ("option", [ TString ]))
                (identifier_namespace_expr body))
            (identifier_body_expr name
               { arg with semantic_expr = identifier; ty = TKeyword })
      | TKeyword | TSymbol | TUnknown ->
          Result.map
            (fun body ->
              typed_ir
                (TOcaml_app ("option", [ TString ]))
                (identifier_namespace_expr body))
            (identifier_body_expr name arg)
      | _ -> Error.error "namespace expects keyword or symbol")

let compile_symbol name args =
  match args with
  | [ arg ] -> (
      match identifier_body_expr name arg with
      | Error _ -> Error.error "symbol expects string, keyword, or symbol"
      | Ok expr -> Ok (typed_ir TSymbol expr))
  | [ namespace_arg; name_arg ] -> (
      match
        ( identifier_body_expr name namespace_arg,
          identifier_body_expr name name_arg )
      with
      | Error _, _ | _, Error _ ->
          Error.error
            "symbol namespace and name must be string, keyword, or symbol"
      | Ok namespace_expr, Ok name_expr ->
          Ok
            (typed_ir TSymbol (namespaced_symbol_expr namespace_expr name_expr))
      )
  | _ -> Error.error "symbol expects 1 or 2 arguments"

let compile name args =
  match name with
  | "integer?" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok arg ->
          Ok (typed_ir TBool (Semantic_ir.Bool (Types.equal arg.ty TInt))))
  | "nat-int?" ->
      int_predicate name args (fun expr ->
          Semantic_ir.Infix (">=", expr, Semantic_ir.Int 0))
  | "pos-int?" ->
      int_predicate name args (fun expr ->
          Semantic_ir.Infix (">", expr, Semantic_ir.Int 0))
  | "neg-int?" ->
      int_predicate name args (fun expr ->
          Semantic_ir.Infix ("<", expr, Semantic_ir.Int 0))
  | "bit-shift-right-zero-fill" ->
      int_binary name args (fun left right ->
          typed_ir TInt
            (Semantic_ir.Infix ("lsr", left, right)))
  | "name" -> compile_name name args
  | "namespace" -> compile_namespace name args
  | "keyword" -> compile_keyword name args
  | "symbol" -> compile_symbol name args
  | _ -> Error.error ("unknown function " ^ name)
