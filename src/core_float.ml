open Types

let accepts_float ty = Types.equal ty TFloat

let expect_float_args args =
  List.for_all (fun arg -> accepts_float arg.ty) args

let fold_infix operator first rest =
  List.fold_left
    (fun expression arg ->
      Semantic_ir.Infix (operator, expression, arg.semantic_expr))
    first.semantic_expr rest

let compile_operator name args =
  match (name, args) with
  | "/", ([] | [ _ ]) -> Error.error "/ expects at least 2 arguments"
  | _, [] -> Error.error (name ^ " expects at least 1 arguments")
  | _, [ arg ] when name = "-" ->
      Ok (typed_ir TFloat (Semantic_ir.Prefix ("~-.", arg.semantic_expr)))
  | _, [ arg ] -> Ok (typed_ir TFloat arg.semantic_expr)
  | _, first :: rest ->
      let operator =
        match name with
        | "+" -> "+."
        | "-" -> "-."
        | "*" -> "*."
        | "/" -> "/."
        | _ -> assert false
      in
      Ok (typed_ir TFloat (fold_infix operator first rest))
