open Types

let apply name args = Semantic_ir.Apply (Semantic_ir.Ident name, args)

let to_seq_expr env collection =
  match Core_protocols.find_seqable collection.ty (Compiler_environment.protocols env) with
  | None -> Error.error "collection value is not seqable"
  | Some implementation -> (
      match Core_sequence_transform.collection_to_seq_expr collection with
      | Ok sequence -> Ok sequence
      | Error _ -> (
          match implementation.ty with
          | TFn ([ receiver_ty ], TSeq inner)
            when Types.assignable ~policy:Host_boundary ~expected:receiver_ty
                   ~actual:collection.ty ->
              Ok
                ( inner,
                  apply implementation.ocaml_name [ collection.semantic_expr ] )
          | _ ->
              Error.error
                "Seqable/-seq implementation must return a typed lazy seq"))
