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

let reduce_expr env fn init collection sequence =
  let fallback () =
    apply "Cljml.Runtime_seq.fold_left"
      [ fn.semantic_expr; init.semantic_expr; sequence ]
  in
  match
    Core_protocols.find_reducible collection.ty
      (Compiler_environment.protocols env)
  with
  | None -> fallback ()
  | Some implementation -> (
      match collection.ty with
      | TList _ | TOcaml_app ("list", [ _ ]) ->
          apply "List.fold_left"
            [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]
      | TVector _ ->
          apply "Rrbvec.fold_left"
            [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]
      | TSet inner -> (
          match Types.set_module_name inner with
          | Error _ -> fallback ()
          | Ok set_module ->
              let item = Semantic_ir.Ident "item" in
              let accumulator = Semantic_ir.Ident "accumulator" in
              let reducer =
                Semantic_ir.Fun
                  ( [ Semantic_ir.PVar "item";
                      Semantic_ir.PVar "accumulator" ],
                    Semantic_ir.Apply
                      (fn.semantic_expr, [ accumulator; item ]) )
              in
              apply (set_module ^ ".fold")
                [ reducer; collection.semantic_expr; init.semantic_expr ])
      | TArray _ | TOcaml_app ("array", [ _ ]) ->
          apply "Array.fold_left"
            [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]
      | TString ->
          apply "String.fold_left"
            [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]
      | TSeq _ | TOcaml_app (("Seq.t" | "Seq"), [ _ ]) ->
          apply "Seq.fold_left"
            [ fn.semantic_expr; init.semantic_expr; collection.semantic_expr ]
      | _ ->
          apply implementation.ocaml_name
            [ collection.semantic_expr; fn.semantic_expr; init.semantic_expr ])
