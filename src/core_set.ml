open Types

let apply name args = Semantic_ir.Apply (Semantic_ir.Ident name, args)

let same_set_type name left right =
  match (left.ty, right.ty) with
  | TSet left_element, TSet right_element
    when Types.same_shape left_element right_element ->
      Types.set_module_name left_element
      |> Result.map (fun set_module -> (set_module, left.ty))
  | TSet _, TSet _ ->
      Error.error (name ^ " expects sets with the same element type")
  | _ -> Error.error (name ^ " expects sets")

let compile_binary name operation args =
  match args with
  | [ left; right ] ->
      same_set_type name left right
      |> Result.map (fun (set_module, result_type) ->
             typed_ir result_type
               (apply (set_module ^ "." ^ operation)
                  [ left.semantic_expr; right.semantic_expr ]))
  | _ -> Error.error (name ^ " expects 2 arguments")

let compile_subset args =
  match compile_binary "subset?" "subset" args with
  | Error _ as error -> error
  | Ok result -> Ok { result with ty = TBool }

let compile name args =
  match name with
  | "subset?" -> compile_subset args
  | "union" -> compile_binary name "union" args
  | "intersection" -> compile_binary name "inter" args
  | "difference" -> compile_binary name "diff" args
  | _ -> Error.error ("unknown clojure.set function " ^ name)
