open Types

let one_arg name args =
  match args with
  | [ arg ] -> Ok arg
  | _ -> Error.error (name ^ " expects 1 arguments")

let type_predicate name predicate args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg -> Ok (typed_ir TBool (Ocaml_ir.Bool (predicate arg.ty)))

let compile_not args =
  match one_arg "not" args with
  | Error _ as err -> err
  | Ok arg ->
      let expression =
        match arg.ty with
        | TBool -> Ocaml_ir.Prefix ("not", arg.ocaml_expr)
        | _ -> Ocaml_ir.Sequence [ arg.ocaml_expr; Ocaml_ir.Bool false ]
      in
      Ok (typed_ir TBool expression)

let compile_predicate name args expected_ty =
  type_predicate name (fun actual_ty -> Types.equal actual_ty expected_ty) args

let compile_bool_literal_predicate name args expected =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg ->
      if Types.equal arg.ty TBool then
        Ok
          (typed_ir TBool
             (Ocaml_ir.Infix ("=", arg.ocaml_expr, Ocaml_ir.Bool expected)))
      else Ok (typed_ir TBool (Ocaml_ir.Bool false))

let compile_type_predicate name predicate args = type_predicate name predicate args

let compile name args =
  match name with
  | "not" -> compile_not args
  | "true?" -> compile_bool_literal_predicate name args true
  | "false?" -> compile_bool_literal_predicate name args false
  | "int?" | "number?" -> compile_type_predicate name (function TInt -> true | _ -> false) args
  | "string?" -> compile_type_predicate name (function TString -> true | _ -> false) args
  | "keyword?" -> compile_type_predicate name (function TKeyword -> true | _ -> false) args
  | "boolean?" -> compile_type_predicate name (function TBool -> true | _ -> false) args
  | "vector?" -> compile_type_predicate name (function TVector _ -> true | _ -> false) args
  | "list?" | "seq?" -> compile_type_predicate name (function TList _ -> true | _ -> false) args
  | "set?" -> compile_type_predicate name (function TSet _ -> true | _ -> false) args
  | "map?" ->
      compile_type_predicate
        name
        (function TRecord _ | TNamed_record _ -> true | _ -> false)
        args
  | "fn?" -> compile_type_predicate name (function TFn _ -> true | _ -> false) args
  | "coll?" ->
      compile_type_predicate name
        (function
          | TList _ | TVector _ | TSet _ | TRecord _ | TNamed_record _ -> true
          | _ -> false)
        args
  | "associative?" ->
      compile_type_predicate
        name
        (function TVector _ | TRecord _ | TNamed_record _ -> true | _ -> false)
        args
  | "indexed?" -> compile_type_predicate name (function TVector _ -> true | _ -> false) args
  | "seqable?" | "counted?" ->
      compile_type_predicate name
        (function
          | TString | TList _ | TVector _ | TSet _ | TRecord _ | TNamed_record _ -> true
          | _ -> false)
        args
  | _ -> Error.error ("unknown function " ^ name)
