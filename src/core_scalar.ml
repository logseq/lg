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

let compile_name name args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg -> (
      match arg.ty with
      | TString -> Ok (typed TString arg.code)
      | TKeyword -> Ok (typed TString (keyword_name_code arg.code))
      | _ -> Error.error "name expects keyword or string")

let compile_keyword name args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg -> (
      match arg.ty with
      | TKeyword -> Ok arg
      | TString ->
          Ok
            (typed TKeyword
               ("(let value = " ^ arg.code
              ^ " in if String.length value > 0 && value.[0] = ':' then value else \":\" ^ value)"))
      | _ -> Error.error "keyword expects keyword or string")

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
  | "keyword" -> compile_keyword name args
  | _ -> Error.error ("unknown function " ^ name)
