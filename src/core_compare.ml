open Types

let rec equality_expr left right =
  match left.ty with
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
                 {
                   ty = field.ty;
                   semantic_expr = Structural_map.field_expr left field;
                   record_values = None;
                   return_param_index = None;
                 }
               in
               let right_field =
                 {
                   ty = field.ty;
                   semantic_expr = Structural_map.field_expr right field;
                   record_values = None;
                   return_param_index = None;
                 }
               in
               equality_expr left_field right_field)
      in
      and_expressions parts
  | _ -> Semantic_ir.Infix ("=", left.semantic_expr, right.semantic_expr)

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
              || first.ty = TUnknown || arg.ty = TUnknown
              || Types.defer_to_ocaml ~expected:first.ty ~actual:arg.ty)
            args
        then
          let equal_expr = and_expressions (pairwise_equality_expressions args) in
          let expression =
            if name = "not=" then Semantic_ir.Prefix ("not", equal_expr) else equal_expr
          in
          Ok (typed_ir TBool expression)
        else Error.error (name ^ " arguments must have the same type")
      else if
        List.for_all
          (fun arg ->
            Types.assignable ~policy:Nominal ~expected:TInt ~actual:arg.ty)
          args
      then
        Ok (typed_ir TBool (and_expressions (pairwise_expressions name args)))
      else Error.error ("expected int arguments for " ^ name)
