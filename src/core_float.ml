open Types

let rec accepts_float ty =
  match ty with
  | TNullable inner | TOcaml_app ("option", [ inner ]) -> accepts_float inner
  | ty -> Types.equal ty TFloat

let expect_float_args args =
  List.for_all (fun arg -> accepts_float arg.ty) args

let rec float_expression arg =
  match arg.ty with
  | TNullable inner | TOcaml_app ("option", [ inner ]) ->
      float_expression
        {
          arg with
          ty = inner;
          semantic_expr =
            Semantic_ir.Apply
              (Semantic_ir.Ident "Option.get", [ arg.semantic_expr ]);
        }
  | _ -> arg.semantic_expr

let fold_infix operator first rest =
  List.fold_left
    (fun expression arg ->
      Semantic_ir.Infix (operator, expression, float_expression arg))
    (float_expression first) rest

let compile_operator name args =
  match (name, args) with
  | "/", ([] | [ _ ]) -> Error.error "/ expects at least 2 arguments"
  | _, [] -> Error.error (name ^ " expects at least 1 arguments")
  | _, [ arg ] when name = "-" ->
      Ok (typed_ir TFloat (Semantic_ir.Prefix ("~-.", float_expression arg)))
  | _, [ arg ] -> Ok (typed_ir TFloat (float_expression arg))
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

let compile_min_max name args =
  match args with
  | [] -> Error.error (name ^ " expects at least 1 arguments")
  | _ :: _ when not (List.for_all (fun arg -> accepts_float arg.ty) args) ->
      Error.error (name ^ " numeric arguments must all have the same type")
  | first :: rest ->
      let fn = if name = "max" then "max" else "min" in
      let expression =
        List.fold_left
          (fun expression arg ->
            Semantic_ir.Apply
              (Semantic_ir.Ident fn, [ expression; float_expression arg ]))
          (float_expression first) rest
      in
      Ok (typed_ir TFloat expression)
