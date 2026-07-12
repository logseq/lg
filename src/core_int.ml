open Types

let accepts_int ty = Types.compatible ~expected:TInt ~actual:ty

let int value = Semantic_ir.Int value

let fold_infix operator first rest =
  List.fold_left
    (fun expression arg -> Semantic_ir.Infix (operator, expression, arg.semantic_expr))
    first.semantic_expr rest

let expect_int_args name args =
  if List.for_all (fun arg -> accepts_int arg.ty) args then Ok ()
  else Error.error ("expected int arguments for " ^ name)

let compile_operator name args =
  match (name, args) with
  | "+", [] -> Ok (typed_ir TInt (int 0))
  | "*", [] -> Ok (typed_ir TInt (int 1))
  | "/", ([] | [ _ ]) -> Error.error "/ expects at least 2 arguments"
  | _, [] -> Error.error (name ^ " expects at least 1 arguments")
  | _, [ arg ] when name = "-" ->
      Ok (typed_ir TInt (Semantic_ir.Prefix ("~-", arg.semantic_expr)))
  | _, [ arg ] -> Ok (typed_ir TInt arg.semantic_expr)
  | _, first :: rest ->
      let op =
        match name with
        | "+" -> "+"
        | "-" -> "-"
        | "*" -> "*"
        | "/" -> "/"
        | _ -> " "
      in
      Ok (typed_ir TInt (fold_infix op first rest))

let compile_unary name args build_expr =
  match args with
  | [ arg ] ->
      if accepts_int arg.ty then Ok (typed_ir TInt (build_expr arg.semantic_expr))
      else Error.error ("expected int arguments for " ^ name)
  | _ -> Error.error (name ^ " expects 1 arguments")

let compile_binary name args =
  match args with
  | [ left; right ] ->
      if accepts_int left.ty && accepts_int right.ty then
        let expression =
          match name with
          | "quot" -> Semantic_ir.Infix ("/", left.semantic_expr, right.semantic_expr)
          | "rem" -> Semantic_ir.Infix ("mod", left.semantic_expr, right.semantic_expr)
          | "mod" ->
              Semantic_ir.Infix
                ( "mod",
                  Semantic_ir.Infix
                    ( "+",
                      Semantic_ir.Infix ("mod", left.semantic_expr, right.semantic_expr),
                      right.semantic_expr ),
                  right.semantic_expr )
          | "bit-shift-left" -> Semantic_ir.Infix ("lsl", left.semantic_expr, right.semantic_expr)
          | "bit-shift-right" -> Semantic_ir.Infix ("asr", left.semantic_expr, right.semantic_expr)
          | _ -> left.semantic_expr
        in
        Ok (typed_ir TInt expression)
      else Error.error ("expected int arguments for " ^ name)
  | _ -> Error.error (name ^ " expects 2 arguments")

let compile_min_max name args =
  match args with
  | [] -> Error.error (name ^ " expects at least 1 arguments")
  | _ ->
      if List.for_all (fun arg -> accepts_int arg.ty) args then
        let fn = if name = "max" then "max" else "min" in
        let expression =
          match args with
          | [] -> assert false
          | first :: rest ->
              List.fold_left
                (fun expression arg ->
                  Semantic_ir.Apply
                    (Semantic_ir.Ident fn, [ expression; arg.semantic_expr ]))
                first.semantic_expr rest
        in
        Ok (typed_ir TInt expression)
      else Error.error ("expected int arguments for " ^ name)

let compile_variadic_bitwise name args =
  match args with
  | [] -> Error.error (name ^ " expects at least 1 arguments")
  | _ ->
      if List.for_all (fun arg -> accepts_int arg.ty) args then
        let op =
          match name with
          | "bit-and" -> "land"
          | "bit-or" -> "lor"
          | "bit-xor" -> "lxor"
          | _ -> assert false
        in
        let expression =
          match args with
          | [] -> assert false
          | first :: rest ->
              fold_infix op first rest
        in
        Ok (typed_ir TInt expression)
      else Error.error ("expected int arguments for " ^ name)
