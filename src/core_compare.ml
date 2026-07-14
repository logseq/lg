open Types

let rec equality_expr left right =
  match (left.ty, right.ty) with
  | TNullable _, TNil ->
      Semantic_ir.Match
        ( left.semantic_expr,
          [ (Semantic_ir.PConstructor ("None", None), Semantic_ir.Bool true);
            ( Semantic_ir.PConstructor ("Some", Some Semantic_ir.PAny),
              Semantic_ir.Bool false );
          ] )
  | TNil, TNullable _ -> equality_expr right left
  | TNullable inner, right_ty
    when Types.assignable ~policy:Host_boundary ~expected:inner
           ~actual:right_ty ->
      Semantic_ir.Match
        ( left.semantic_expr,
          [ (Semantic_ir.PConstructor ("None", None), Semantic_ir.Bool false);
            ( Semantic_ir.PConstructor
                ("Some", Some (Semantic_ir.PVar "nullable_value")),
              equality_expr
                (typed_ir inner (Semantic_ir.Ident "nullable_value"))
                right );
          ] )
  | left_ty, TNullable inner
    when Types.assignable ~policy:Host_boundary ~expected:inner
           ~actual:left_ty ->
      equality_expr right left
  | TNil, TNil -> Semantic_ir.Infix ("=", left.semantic_expr, right.semantic_expr)
  | TNil, _ | _, TNil ->
      Semantic_ir.Sequence
        [ left.semantic_expr; right.semantic_expr; Semantic_ir.Bool false ]
  | _ -> (match left.ty with
  | TSet inner -> (
      match Types.set_module_name inner with
      | Ok set_module ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident (set_module ^ ".equal"),
              [ left.semantic_expr; right.semantic_expr ] )
      | Error _ -> Semantic_ir.Bool false)
  | TRecord fields | TNamed_record { fields; _ } ->
      let parts =
        fields
        |> List.map (fun (field : field) ->
               let left_field =
                 typed_ir field.ty (Structural_map.field_expr left field)
               in
               let right_field =
                 typed_ir field.ty (Structural_map.field_expr right field)
               in
               equality_expr left_field right_field)
      in
      and_expressions parts
  | _ -> Semantic_ir.Infix ("=", left.semantic_expr, right.semantic_expr))

and and_expressions = function
  | [] -> Semantic_ir.Bool true
  | first :: rest ->
      List.fold_left
        (fun expression next -> Semantic_ir.Infix ("&&", expression, next))
        first rest

let pairwise_expressions op args =
  let rec loop acc = function
    | left :: ((right :: _) as rest) ->
        loop (Semantic_ir.Infix (op, left.semantic_expr, right.semantic_expr) :: acc) rest
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
      Ok (typed_ir TBool (Semantic_ir.Bool (name <> "not=")))
  | first :: _ ->
      if name = "=" || name = "not=" then
        if
          List.for_all
            (fun arg ->
              Types.same_shape first.ty arg.ty
              || (match (first.ty, arg.ty) with
                 | TNil, TNullable _ | TNullable _, TNil -> true
                 | TNullable inner, ty | ty, TNullable inner ->
                     Types.assignable ~policy:Host_boundary ~expected:inner
                       ~actual:ty
                 | _ -> false)
              || Types.assignable ~policy:Host_boundary ~expected:first.ty
                   ~actual:arg.ty
              || Types.defer_to_ocaml ~expected:first.ty ~actual:arg.ty)
            args
        then
          let equal_expr = and_expressions (pairwise_equality_expressions args) in
          let expression =
            if name = "not=" then Semantic_ir.Prefix ("not", equal_expr) else equal_expr
          in
          Ok (typed_ir TBool expression)
        else Error.error (name ^ " arguments must have the same type")
      else
        let numeric_ty =
          List.find_map
            (fun arg -> if Types.is_numeric arg.ty then Some arg.ty else None)
            args
        in
        (match numeric_ty with
        | Some expected
          when List.for_all
                 (fun arg ->
                   Types.assignable ~policy:Host_boundary ~expected
                     ~actual:arg.ty)
                 args ->
            Ok
              (typed_ir TBool
                 (and_expressions (pairwise_expressions name args)))
        | _ ->
            Error.error
              (name ^ " numeric arguments must all have the same type"))
