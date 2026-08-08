open Types

let rec accepts_int ty =
  match ty with
  | TNullable inner | TOcaml_app ("option", [ inner ]) -> accepts_int inner
  | TOcaml "int" -> true
  | ty -> Types.assignable ~policy:Host_boundary ~expected:TInt ~actual:ty

let int value = Semantic_ir.Int value

let apply name args = Semantic_ir.Apply (Semantic_ir.Ident name, args)

let rec int_expression arg =
  match arg.ty with
  | TNullable inner | TOcaml_app ("option", [ inner ]) ->
      int_expression
        {
          arg with
          ty = inner;
          semantic_expr =
            Semantic_ir.Apply
              (Semantic_ir.Ident "Option.get", [ arg.semantic_expr ]);
        }
  | ty when Types.is_dynamic ty ->
      Semantic_ir.Apply
        ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.as_int",
          [ arg.semantic_expr ] )
  | _ -> arg.semantic_expr

let fold_infix operator first rest =
  List.fold_left
    (fun expression arg ->
      Semantic_ir.Infix (operator, expression, int_expression arg))
    (int_expression first) rest

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
      Ok (typed_ir TInt (Semantic_ir.Prefix ("~-", int_expression arg)))
  | _, [ arg ] -> Ok (typed_ir TInt (int_expression arg))
  | _, first :: rest ->
      let operator =
        match name with
        | "+" -> "+"
        | "-" -> "-"
        | "*" -> "*"
        | "/" -> "/"
        | _ -> assert false
      in
      Ok (typed_ir TInt (fold_infix operator first rest))

let compile_unary name args build_expr =
  match args with
  | [ arg ] ->
      if accepts_int arg.ty then Ok (typed_ir TInt (build_expr (int_expression arg)))
      else Error.error ("expected int arguments for " ^ name)
  | _ -> Error.error (name ^ " expects 1 arguments")

let compile_binary name args =
  match args with
  | [ left; right ] ->
      if accepts_int left.ty && accepts_int right.ty then
        let expression =
          match name with
          | "quot" ->
              Semantic_ir.Infix
                ("/", int_expression left, int_expression right)
          | "rem" ->
              Semantic_ir.Infix
                ("mod", int_expression left, int_expression right)
          | "mod" ->
              apply "Lg_runtime.Runtime_int.clojure_mod"
                [ int_expression left; int_expression right ]
          | "bit-shift-left" ->
              Semantic_ir.Infix
                ("lsl", int_expression left, int_expression right)
          | "bit-shift-right" ->
              Semantic_ir.Infix
                ("asr", int_expression left, int_expression right)
          | _ -> int_expression left
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
                    (Semantic_ir.Ident fn, [ expression; int_expression arg ]))
                (int_expression first) rest
        in
        Ok (typed_ir TInt expression)
      else Error.error ("expected int arguments for " ^ name)
