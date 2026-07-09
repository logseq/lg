open Types

let expect_int_args name args =
  if List.for_all (fun arg -> Types.equal arg.ty TInt) args then Ok ()
  else Error.error ("expected int arguments for " ^ name)

let one_arg name args =
  match args with
  | [ arg ] -> Ok arg
  | _ -> Error.error (name ^ " expects 1 arguments")

let two_args name args =
  match args with
  | [ left; right ] -> Ok (left, right)
  | _ -> Error.error (name ^ " expects 2 arguments")

let int_predicate name args build_code =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg ->
      if Types.equal arg.ty TInt then Ok (typed TBool (build_code arg.code))
      else Ok (typed TBool "false")

let int_unary name args build_code =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg -> (
      match expect_int_args name [ arg ] with
      | Error _ as err -> err
      | Ok () -> Ok (typed TInt (build_code arg.code)))

let int_binary name args build_code =
  match two_args name args with
  | Error _ as err -> err
  | Ok (left, right) -> (
      match expect_int_args name [ left; right ] with
      | Error _ as err -> err
      | Ok () -> Ok (build_code left.code right.code))

let compile_boolean name args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg -> (
      match arg.ty with
      | TBool -> Ok (typed TBool arg.code)
      | TNil -> Ok (typed TBool "false")
      | _ -> Ok (typed TBool "true"))

let keyword_name_code keyword_code =
  "(let keyword = "
  ^ keyword_code
  ^ " in let without_prefix = if String.length keyword > 0 && keyword.[0] = ':' then String.sub keyword 1 (String.length keyword - 1) else keyword in match String.rindex_opt without_prefix '/' with None -> without_prefix | Some index -> String.sub without_prefix (index + 1) (String.length without_prefix - index - 1))"

let identifier_body_code code =
  "(let value = "
  ^ code
  ^ " in if String.length value > 0 && value.[0] = ':' then String.sub value 1 (String.length value - 1) else value)"

let identifier_name_code code =
  "(let body = "
  ^ identifier_body_code code
  ^ " in match String.rindex_opt body '/' with None -> body | Some index -> String.sub body (index + 1) (String.length body - index - 1))"

let identifier_namespace_code code =
  "(let body = "
  ^ identifier_body_code code
  ^ " in match String.rindex_opt body '/' with None -> \"\" | Some index -> String.sub body 0 index)"

let identifier_body_expr name arg =
  match arg.ty with
  | TString | TSymbol | TKeyword -> Ok (identifier_body_code arg.code)
  | _ -> Error.error (name ^ " expects string, keyword, or symbol")

let compile_name name args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg -> (
      match arg.ty with
      | TString -> Ok (typed TString arg.code)
      | TKeyword | TSymbol -> Ok (typed TString (identifier_name_code arg.code))
      | _ -> Error.error "name expects keyword, string, or symbol")

let compile_keyword name args =
  let keyword_code code =
    "(let value = "
    ^ code
    ^ " in if String.length value > 0 && value.[0] = ':' then value else \":\" ^ value)"
  in
  match args with
  | [ arg ] -> (
      match arg.ty with
      | TKeyword -> Ok arg
      | TString | TSymbol -> Ok (typed TKeyword (keyword_code arg.code))
      | _ -> Error.error "keyword expects keyword, string, or symbol")
  | [ namespace_arg; name_arg ] -> (
      match (identifier_body_expr name namespace_arg, identifier_body_expr name name_arg) with
      | Error _, _ | _, Error _ ->
          Error.error "keyword namespace and name must be string, keyword, or symbol"
      | Ok namespace_code, Ok name_code ->
          Ok
            (typed TKeyword
               ("(let namespace = "
              ^ namespace_code
              ^ " in let name = "
              ^ name_code
              ^ " in if namespace = \"\" then \":\" ^ name else \":\" ^ namespace ^ \"/\" ^ name)")))
  | _ -> Error.error "keyword expects 1 or 2 arguments"

let compile_namespace name args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg -> (
      match arg.ty with
      | TKeyword | TSymbol -> Ok (typed TString (identifier_namespace_code arg.code))
      | _ -> Error.error "namespace expects keyword or symbol")

let compile_symbol name args =
  match args with
  | [ arg ] -> (
      match identifier_body_expr name arg with
      | Error _ -> Error.error "symbol expects string, keyword, or symbol"
      | Ok code -> Ok (typed TSymbol code))
  | [ namespace_arg; name_arg ] -> (
      match (identifier_body_expr name namespace_arg, identifier_body_expr name name_arg) with
      | Error _, _ | _, Error _ ->
          Error.error "symbol namespace and name must be string, keyword, or symbol"
      | Ok namespace_code, Ok name_code ->
          Ok
            (typed TSymbol
               ("(let namespace = "
              ^ namespace_code
              ^ " in let name = "
              ^ name_code
              ^ " in if namespace = \"\" then name else namespace ^ \"/\" ^ name)")))
  | _ -> Error.error "symbol expects 1 or 2 arguments"

let compile name args =
  match name with
  | "integer?" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok arg -> Ok (typed TBool (string_of_bool (Types.equal arg.ty TInt))))
  | "nat-int?" -> int_predicate name args (fun code -> "(" ^ code ^ " >= 0)")
  | "pos-int?" -> int_predicate name args (fun code -> "(" ^ code ^ " > 0)")
  | "neg-int?" -> int_predicate name args (fun code -> "(" ^ code ^ " < 0)")
  | "boolean" -> compile_boolean name args
  | "bit-set" ->
      int_binary name args (fun left right -> typed TInt ("(" ^ left ^ " lor (1 lsl " ^ right ^ "))"))
  | "bit-clear" ->
      int_binary name args (fun left right -> typed TInt ("(" ^ left ^ " land (lnot (1 lsl " ^ right ^ ")))"))
  | "bit-flip" ->
      int_binary name args (fun left right -> typed TInt ("(" ^ left ^ " lxor (1 lsl " ^ right ^ "))"))
  | "bit-test" ->
      int_binary name args (fun left right -> typed TBool ("((" ^ left ^ " land (1 lsl " ^ right ^ ")) <> 0)"))
  | "bit-shift-right-zero-fill" ->
      int_binary name args (fun left right -> typed TInt ("(" ^ left ^ " lsr " ^ right ^ ")"))
  | "unchecked-add" | "unchecked-add-int" ->
      int_binary name args (fun left right -> typed TInt ("(" ^ left ^ " + " ^ right ^ ")"))
  | "unchecked-subtract" | "unchecked-subtract-int" ->
      int_binary name args (fun left right -> typed TInt ("(" ^ left ^ " - " ^ right ^ ")"))
  | "unchecked-multiply" | "unchecked-multiply-int" ->
      int_binary name args (fun left right -> typed TInt ("(" ^ left ^ " * " ^ right ^ ")"))
  | "unchecked-divide-int" ->
      int_binary name args (fun left right -> typed TInt ("(" ^ left ^ " / " ^ right ^ ")"))
  | "unchecked-remainder-int" ->
      int_binary name args (fun left right -> typed TInt ("(" ^ left ^ " mod " ^ right ^ ")"))
  | "unchecked-inc" | "unchecked-inc-int" -> int_unary name args (fun code -> "(" ^ code ^ " + 1)")
  | "unchecked-dec" | "unchecked-dec-int" -> int_unary name args (fun code -> "(" ^ code ^ " - 1)")
  | "unchecked-negate" | "unchecked-negate-int" -> int_unary name args (fun code -> "(-" ^ code ^ ")")
  | "name" -> compile_name name args
  | "namespace" -> compile_namespace name args
  | "keyword" -> compile_keyword name args
  | "symbol" -> compile_symbol name args
  | _ -> Error.error ("unknown function " ^ name)
