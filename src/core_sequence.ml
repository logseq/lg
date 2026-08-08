open Types

let apply name args = Semantic_ir.Apply (Semantic_ir.Ident name, args)

let one_arg name args =
  match args with
  | [ arg ] -> Ok arg
  | _ -> Error.error (name ^ " expects 1 arguments")

let first_expr env collection = Collection_capability.first_expr env collection

let next_expr env collection = Collection_capability.next_expr env collection

let reverse_expr name collection =
  match collection.ty with
  | TList _ -> Ok (typed_ir collection.ty (apply "List.rev" [ collection.semantic_expr ]))
  | TVector _ -> Ok (typed_ir collection.ty (apply "Rrbvec.rev" [ collection.semantic_expr ]))
  | _ -> Error.error (name ^ " expects a list or vector")

let compile env name args =
  match name with
  | "next" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> next_expr env collection)
  | "ffirst" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match first_expr env collection with
          | Error _ as err -> err
          | Ok first -> first_expr env first))
  | "fnext" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match next_expr env collection with
          | Error _ as err -> err
          | Ok next -> first_expr env next))
  | "nfirst" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match first_expr env collection with
          | Error _ as err -> err
          | Ok first -> next_expr env first))
  | "nnext" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match next_expr env collection with
          | Error _ as err -> err
          | Ok next -> next_expr env next))
  | "rseq" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> reverse_expr name collection)
  | _ -> Error.error ("unknown function " ^ name)
