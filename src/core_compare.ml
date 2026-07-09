open Types

let rec equality_expr left right =
  match left.ty with
  | TRecord fields ->
      let parts =
        fields
        |> List.map (fun (field : field) ->
               let left_field =
                 {
                   ty = field.ty;
                   code = Structural_map.field_code left field;
                   ocaml_expr = Structural_map.field_expr left field;
                   record_values = None;
                 }
               in
               let right_field =
                 {
                   ty = field.ty;
                   code = Structural_map.field_code right field;
                   ocaml_expr = Structural_map.field_expr right field;
                   record_values = None;
                 }
               in
               equality_expr left_field right_field)
      in
      and_expressions parts
  | _ -> Ocaml_ir.Infix ("=", left.ocaml_expr, right.ocaml_expr)

and and_expressions = function
  | [] -> Ocaml_ir.Bool true
  | first :: rest ->
      List.fold_left
        (fun expression next -> Ocaml_ir.Infix ("&&", expression, next))
        first rest

let pairwise_expressions op args =
  let rec loop acc = function
    | left :: ((right :: _) as rest) ->
        loop (Ocaml_ir.Infix (op, left.ocaml_expr, right.ocaml_expr) :: acc) rest
    | _ -> List.rev acc
  in
  loop [] args

let pairwise_equality_expressions args =
  let rec loop acc = function
    | left :: ((right :: _) as rest) -> loop (equality_expr left right :: acc) rest
    | _ -> List.rev acc
  in
  loop [] args

let compile name args =
  match args with
  | [] | [ _ ] ->
      Ok (typed_ir TBool (Ocaml_ir.Bool (name <> "not=")))
  | first :: _ ->
      if name = "=" || name = "not=" then
        if List.for_all (fun arg -> Types.equal first.ty arg.ty) args then
          let equal_expr = and_expressions (pairwise_equality_expressions args) in
          let expression =
            if name = "not=" then Ocaml_ir.Prefix ("not", equal_expr) else equal_expr
          in
          Ok (typed_ir TBool expression)
        else Error.error (name ^ " arguments must have the same type")
      else if List.for_all (fun arg -> Types.equal arg.ty TInt) args then
        Ok (typed_ir TBool (and_expressions (pairwise_expressions name args)))
      else Error.error ("expected int arguments for " ^ name)
