open Types

let one_arg name args =
  match args with
  | [ arg ] -> Ok arg
  | _ -> Error.error (name ^ " expects 1 arguments")

let apply name args = Semantic_ir.Apply (Semantic_ir.Ident name, args)

let compile name args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg ->
      let bool value = Ok (typed_ir TBool value) in
      let static_bool value = bool (Semantic_ir.Bool value) in
      match name with
      | "rational?" -> static_bool (Types.equal arg.ty TInt)
      | "float?" | "double?" -> static_bool (Types.equal arg.ty TFloat)
      | "symbol?"
        when Option.is_some
               (Types.symbol_predicate_constraint_info arg.ty) ->
          let projected =
            match Semantic_ir.unlocated arg.semantic_expr with
            | Semantic_ir.Ident name ->
                apply (name ^ "__symbol") [ arg.semantic_expr ]
            | _ ->
                apply "fst" [ arg.semantic_expr ]
                |> fun projector ->
                Semantic_ir.Apply
                  ( projector,
                    [ apply "snd" [ arg.semantic_expr ] ] )
          in
          bool (apply "Option.is_some" [ projected ])
      | "symbol?"
        when Types.is_dynamic arg.ty || Types.equal arg.ty TUnknown ->
          bool
            (apply "Lg_runtime.Runtime_dynamic.is_symbol"
               [ arg.semantic_expr ])
      | "symbol?" -> static_bool (Types.equal arg.ty TSymbol)
      | "sequential?" ->
          static_bool (match arg.ty with TList _ | TVector _ -> true | _ -> false)
      | "reversible?" ->
          static_bool (match arg.ty with TString | TList _ | TVector _ -> true | _ -> false)
      | "sorted?" -> static_bool false
      | _ -> Error.error ("unknown function " ^ name)
