open Types
open Expression_support

let nullable_equality_counter = ref 0

let fresh_nullable_equality_name () =
  incr nullable_equality_counter;
  "__lg_nullable_equality_value_"
  ^ string_of_int !nullable_equality_counter

let static_sequence_equal left right =
  Semantic_ir.Apply
    ( Semantic_ir.Ident "Seq.equal",
      [ Semantic_ir.Ident "="; left; right ] )

let requires_runtime_equality = function
  | TUnknown | TMeta _ | TVar _ | TOcaml "value" -> true
  | _ -> false

let sequential_type = function
  | TList _ | TVector _ | TArray _ | TSeq _ -> true
  | ty -> Option.is_some (Types.next_seq_element ty)

let rec equality_expr ?env left right =
  let resolve value =
    match env with
    | None -> value
    | Some env ->
        {
          value with
          ty =
            Collection_capability.resolve_callback_record env value.ty;
        }
  in
  let left = resolve left in
  let right = resolve right in
  match (left.ty, right.ty) with
  | TFloat, TInt ->
      let right =
        match Semantic_ir.unlocated right.semantic_expr with
        | Semantic_ir.Int value ->
            Semantic_ir.Float (string_of_int value ^ ".0")
        | _ ->
            Semantic_ir.Apply
              (Semantic_ir.Ident "float_of_int", [ right.semantic_expr ])
      in
      Semantic_ir.Infix
        ("=", left.semantic_expr, right)
  | TInt, TFloat -> equality_expr ?env right left
  | TOcaml "int", TInt ->
      Semantic_ir.Infix ("=", left.semantic_expr, right.semantic_expr)
  | TInt, TOcaml "int" -> equality_expr ?env right left
  | (TNullable left_inner | TOcaml_app ("option", [ left_inner ])),
    (TNullable right_inner | TOcaml_app ("option", [ right_inner ]))
    when Types.same_shape left_inner right_inner ->
      let left_value = fresh_nullable_equality_name () in
      let right_value = fresh_nullable_equality_name () in
      Semantic_ir.Match
        ( left.semantic_expr,
          [
            ( Semantic_ir.PConstructor ("None", None),
              Semantic_ir.Match
                ( right.semantic_expr,
                  [
                    ( Semantic_ir.PConstructor ("None", None),
                      Semantic_ir.Bool true );
                    ( Semantic_ir.PConstructor
                        ("Some", Some Semantic_ir.PAny),
                      Semantic_ir.Bool false );
                  ] ) );
            ( Semantic_ir.PConstructor
                ("Some", Some (Semantic_ir.PVar left_value)),
              Semantic_ir.Match
                ( right.semantic_expr,
                  [
                    ( Semantic_ir.PConstructor ("None", None),
                      Semantic_ir.Bool false );
                    ( Semantic_ir.PConstructor
                        ("Some", Some (Semantic_ir.PVar right_value)),
                      equality_expr ?env
                        (typed_ir left_inner
                           (Semantic_ir.Ident left_value))
                        (typed_ir right_inner
                           (Semantic_ir.Ident right_value)) );
                  ] ) );
          ] )
  | (TNullable _ | TOcaml_app ("option", [ _ ])), TNil ->
      Semantic_ir.Match
        ( left.semantic_expr,
          [ (Semantic_ir.PConstructor ("None", None), Semantic_ir.Bool true);
            ( Semantic_ir.PConstructor ("Some", Some Semantic_ir.PAny),
              Semantic_ir.Bool false );
          ] )
  | TNil, (TNullable _ | TOcaml_app ("option", [ _ ])) ->
      equality_expr ?env right left
  | (TNullable inner | TOcaml_app ("option", [ inner ])), right_ty
    when Types.assignable ~policy:Host_boundary ~expected:inner
           ~actual:right_ty ->
      let nullable_value = fresh_nullable_equality_name () in
      Semantic_ir.Match
        ( left.semantic_expr,
          [ (Semantic_ir.PConstructor ("None", None), Semantic_ir.Bool false);
            ( Semantic_ir.PConstructor
                ("Some", Some (Semantic_ir.PVar nullable_value)),
      equality_expr ?env
                (typed_ir inner (Semantic_ir.Ident nullable_value))
                right );
          ] )
  | left_ty, (TNullable inner | TOcaml_app ("option", [ inner ]))
    when Types.assignable ~policy:Host_boundary ~expected:inner
           ~actual:left_ty ->
      equality_expr ?env right left
  | TNil, TNil -> Semantic_ir.Infix ("=", left.semantic_expr, right.semantic_expr)
  | TNil, _ | _, TNil ->
      Semantic_ir.Sequence
        [ left.semantic_expr; right.semantic_expr; Semantic_ir.Bool false ]
  | left_ty, right_ty
    when Option.is_some (Types.dynamic_map_types left_ty)
         && Option.is_some (Types.dynamic_map_types right_ty) -> (
      match
        (Types.dynamic_map_types left_ty, Types.dynamic_map_types right_ty)
      with
      | Some (left_key, left_value), Some (right_key, right_value) ->
          if
            Types.same_shape left_key right_key
            && Types.same_shape left_value right_value
          then
            Semantic_ir.Apply
              ( Semantic_ir.Ident "Lg_runtime.Runtime_map.equiv",
                [ left.semantic_expr; right.semantic_expr ] )
          else
            Semantic_ir.Sequence
              [ left.semantic_expr; right.semantic_expr; Semantic_ir.Bool false ]
      | _ -> assert false)
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
      equality_expr ?env right left
  | left_ty, right_ty
    when sequential_type left_ty && sequential_type right_ty -> (
      match
        ( Core_sequence_transform.collection_to_seq_expr left,
          Core_sequence_transform.collection_to_seq_expr right )
      with
      | Ok (_, left), Ok (_, right) -> static_sequence_equal left right
      | Error _, _ | _, Error _ -> Semantic_ir.Bool false)
  | (TFn _ | TOverloaded_fn _), (TFn _ | TOverloaded_fn _) ->
      Semantic_ir.Apply
        ( Semantic_ir.Ident "Lg_runtime.Runtime_static_value.equal",
          [ left.semantic_expr; right.semantic_expr ] )
  | left_ty, right_ty
    when requires_runtime_equality left_ty
         && requires_runtime_equality right_ty ->
      Semantic_ir.Apply
        ( Semantic_ir.Ident "Lg_runtime.Runtime_static_value.equal",
          [ left.semantic_expr; right.semantic_expr ] )
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
      equality_expr ?env right left
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
               equality_expr ?env left_field right_field)
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

let host_int_ordering_expr op left right =
  match (left.ty, right.ty) with
  | TOcaml "int", TInt | TInt, TOcaml "int" ->
      Semantic_ir.Infix (op, left.semantic_expr, right.semantic_expr)
  | _ -> Semantic_ir.Infix (op, left.semantic_expr, right.semantic_expr)

let pairwise_host_int_ordering_expressions op args =
  let rec loop acc = function
    | left :: ((right :: _) as rest) ->
        loop (host_int_ordering_expr op left right :: acc) rest
    | _ -> List.rev acc
  in
  loop [] args

let pairwise_equality_expressions ?env args =
  let rec loop acc = function
    | left :: ((right :: _) as rest) ->
        loop (equality_expr ?env left right :: acc) rest
    | _ -> List.rev acc
  in
  loop [] args

let dynamic_numeric_function = function
  | "<" -> "numeric_less"
  | "<=" -> "numeric_less_equal"
  | ">" -> "numeric_greater"
  | ">=" -> "numeric_greater_equal"
  | _ -> assert false

let dynamic_numeric_pairwise_expressions name args =
  let runtime_function =
    "Lg_runtime.Runtime_dynamic." ^ dynamic_numeric_function name
  in
  let rec loop acc = function
    | left :: ((right :: _) as rest) -> (
        match
          (pack_plain_dynamic_value left, pack_plain_dynamic_value right)
        with
        | Some left, Some right ->
            loop
              (Semantic_ir.Apply
                 (Semantic_ir.Ident runtime_function, [ left; right ])
              :: acc)
              rest
        | _ -> None)
    | _ -> Some (List.rev acc)
  in
  loop [] args

let compile ?env name args =
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
              || (Types.is_numeric first.ty && Types.is_numeric arg.ty)
              || (match (first.ty, arg.ty) with
                 | TNil, TNullable _ | TNullable _, TNil -> true
                 | TNullable inner, ty | ty, TNullable inner ->
                     Types.assignable ~policy:Host_boundary ~expected:inner
                       ~actual:ty
                 | _ -> false)
              || Types.assignable ~policy:Host_boundary ~expected:first.ty
                   ~actual:arg.ty
              || Types.defer_to_ocaml ~expected:first.ty ~actual:arg.ty
              || (sequential_type first.ty && sequential_type arg.ty))
            args
        then
          let equal_expr =
            and_expressions (pairwise_equality_expressions ?env args)
          in
          let expression =
            if name = "not=" then Semantic_ir.Prefix ("not", equal_expr) else equal_expr
          in
          Ok (typed_ir TBool expression)
        else
          Error.error
            (name ^ " arguments must have the same type: "
            ^ String.concat ", " (List.map (fun arg -> Types.source_name arg.ty) args))
      else
        let has_dynamic = List.exists (fun arg -> Types.is_dynamic arg.ty) args in
        if has_dynamic then
          match dynamic_numeric_pairwise_expressions name args with
          | Some expressions ->
              Ok (typed_ir TBool (and_expressions expressions))
          | None ->
              Error.error (name ^ " expects numeric arguments")
        else if
          List.exists (fun arg -> Types.equal arg.ty (TOcaml "int")) args
          && List.for_all
               (fun arg ->
                 Types.equal arg.ty TInt
                 || Types.equal arg.ty (TOcaml "int"))
               args
        then
          Ok
            (typed_ir TBool
               (and_expressions
                  (pairwise_host_int_ordering_expressions name args)))
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
        | Some _
          when List.for_all
                 (fun arg -> Core_float.accepts_mixed_numeric arg.ty)
                 args ->
            Ok
              (typed_ir TBool
                 (and_expressions
                    (pairwise_expressions name
                       (List.map Core_float.widen_to_float args))))
        | _ ->
            Error.error
              (name ^ " numeric arguments must all have the same type"))
