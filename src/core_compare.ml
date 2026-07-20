open Types
open Expression_support

let nullable_equality_counter = ref 0

let fresh_nullable_equality_name () =
  incr nullable_equality_counter;
  "__lg_nullable_equality_value_"
  ^ string_of_int !nullable_equality_counter

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
      let nullable_value = fresh_nullable_equality_name () in
      Semantic_ir.Match
        ( left.semantic_expr,
          [ (Semantic_ir.PConstructor ("None", None), Semantic_ir.Bool false);
            ( Semantic_ir.PConstructor
                ("Some", Some (Semantic_ir.PVar nullable_value)),
              equality_expr
                (typed_ir inner (Semantic_ir.Ident nullable_value))
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
  | _ -> (match (left.ty, right.ty) with
  | left_ty, right_ty
    when Types.is_dynamic left_ty && Types.is_dynamic right_ty ->
      Semantic_ir.Apply
        ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.equal",
          [ left.semantic_expr; right.semantic_expr ] )
  | left_ty, _ when Types.is_dynamic left_ty -> (
      match pack_plain_dynamic_value right with
      | Some right ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident "Lg_runtime.Runtime_dynamic.equal",
              [ left.semantic_expr; right ] )
      | None -> Semantic_ir.Bool false)
  | _, right_ty when Types.is_dynamic right_ty ->
      equality_expr right left
  | (TRecord _ | TNamed_record _), right_type
    when Option.is_some (Types.dynamic_map_types right_type) -> (
      match left.record_values with
      | None -> Semantic_ir.Bool false
      | Some values ->
          let dynamic_left =
            List.fold_left
              (fun map ((field : field), value) ->
                Semantic_ir.Apply
                  ( Semantic_ir.Ident "Lg_runtime.Runtime_map.assoc",
                    [ map; Semantic_ir.String field.keyword; value ] ))
              (Semantic_ir.Ident "Lg_runtime.Runtime_map.empty") values
          in
          Semantic_ir.Infix ("=", dynamic_left, right.semantic_expr))
  | left_type, (TRecord _ | TNamed_record _)
    when Option.is_some (Types.dynamic_map_types left_type) ->
      equality_expr right left
  | TSet left_inner, TSet right_inner -> (
      match Types.set_module_name left_inner with
      | Ok left_module -> (
          match Types.set_module_name right_inner with
          | Ok right_module when left_module = right_module ->
              Semantic_ir.Apply
                ( Semantic_ir.Ident (left_module ^ ".equal"),
                  [ left.semantic_expr; right.semantic_expr ] )
          | Ok _ when Types.equal right_inner TUnknown ->
              Semantic_ir.Apply
                ( Semantic_ir.Ident (left_module ^ ".equal"),
                  [ left.semantic_expr;
                    Semantic_ir.Apply
                      ( Semantic_ir.Ident (left_module ^ ".of_list"),
                        [ right.semantic_expr ] ) ] )
          | Ok right_module when Types.equal left_inner TUnknown ->
              Semantic_ir.Apply
                ( Semantic_ir.Ident (right_module ^ ".equal"),
                  [ Semantic_ir.Apply
                      ( Semantic_ir.Ident (right_module ^ ".of_list"),
                        [ left.semantic_expr ] );
                    right.semantic_expr ] )
          | _ ->
              Semantic_ir.Infix
                ("=", left.semantic_expr, right.semantic_expr))
      | Error _ ->
          Semantic_ir.Infix ("=", left.semantic_expr, right.semantic_expr))
  | TSet inner, _ -> (
      match Types.set_module_name inner with
      | Ok set_module ->
          Semantic_ir.Apply
            ( Semantic_ir.Ident (set_module ^ ".equal"),
              [ left.semantic_expr; right.semantic_expr ] )
      | Error _ -> Semantic_ir.Bool false)
  | (TRecord fields | TNamed_record { fields; _ }), _ ->
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
              Types.is_dynamic first.ty
              || Types.is_dynamic arg.ty
              || Types.same_shape first.ty arg.ty
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
        else
          Error.error
            (name ^ " arguments must have the same type: "
            ^ String.concat ", " (List.map (fun arg -> Types.source_name arg.ty) args))
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
