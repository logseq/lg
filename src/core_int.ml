open Types

let expect_int_args name args =
  if List.for_all (fun arg -> Types.equal arg.ty TInt) args then Ok ()
  else Error.error ("expected int arguments for " ^ name)

let compile_operator name args =
  match (name, args) with
  | "+", [] -> Ok (typed TInt "0")
  | "*", [] -> Ok (typed TInt "1")
  | "/", ([] | [ _ ]) -> Error.error "/ expects at least 2 arguments"
  | _, [] -> Error.error (name ^ " expects at least 1 arguments")
  | _, [ arg ] when name = "-" -> Ok (typed TInt ("(-" ^ arg.code ^ ")"))
  | _, [ arg ] -> Ok (typed TInt arg.code)
  | _, first :: rest ->
      let op =
        match name with
        | "+" -> " + "
        | "-" -> " - "
        | "*" -> " * "
        | "/" -> " / "
        | _ -> " "
      in
      let code =
        rest
        |> List.fold_left
             (fun acc arg -> "(" ^ acc ^ op ^ arg.code ^ ")")
             first.code
      in
      Ok (typed TInt code)

let compile_unary name args build_code =
  match args with
  | [ arg ] ->
      if Types.equal arg.ty TInt then Ok (typed TInt (build_code arg.code))
      else Error.error ("expected int arguments for " ^ name)
  | _ -> Error.error (name ^ " expects 1 arguments")

let compile_binary name args =
  match args with
  | [ left; right ] ->
      if Types.equal left.ty TInt && Types.equal right.ty TInt then
        let code =
          match name with
          | "quot" -> "(" ^ left.code ^ " / " ^ right.code ^ ")"
          | "rem" -> "(" ^ left.code ^ " mod " ^ right.code ^ ")"
          | "mod" ->
              "(((" ^ left.code ^ " mod " ^ right.code ^ ") + " ^ right.code
              ^ ") mod " ^ right.code ^ ")"
          | "bit-shift-left" -> "(" ^ left.code ^ " lsl " ^ right.code ^ ")"
          | "bit-shift-right" -> "(" ^ left.code ^ " asr " ^ right.code ^ ")"
          | _ -> left.code
        in
        Ok (typed TInt code)
      else Error.error ("expected int arguments for " ^ name)
  | _ -> Error.error (name ^ " expects 2 arguments")

let compile_min_max name args =
  match args with
  | [] -> Error.error (name ^ " expects at least 1 arguments")
  | _ ->
      if List.for_all (fun arg -> Types.equal arg.ty TInt) args then
        let fn = if name = "max" then "max" else "min" in
        let code =
          match args with
          | [] -> assert false
          | first :: rest ->
              rest
              |> List.fold_left
                   (fun acc arg -> fn ^ " (" ^ acc ^ ") (" ^ arg.code ^ ")")
                   first.code
        in
        Ok (typed TInt code)
      else Error.error ("expected int arguments for " ^ name)

let compile_variadic_bitwise name args =
  match args with
  | [] -> Error.error (name ^ " expects at least 1 arguments")
  | _ ->
      if List.for_all (fun arg -> Types.equal arg.ty TInt) args then
        let op =
          match name with
          | "bit-and" -> "land"
          | "bit-or" -> "lor"
          | "bit-xor" -> "lxor"
          | _ -> assert false
        in
        let code =
          match args with
          | [] -> assert false
          | first :: rest ->
              rest
              |> List.fold_left
                   (fun acc arg -> "(" ^ acc ^ " " ^ op ^ " " ^ arg.code ^ ")")
                   first.code
        in
        Ok (typed TInt code)
      else Error.error ("expected int arguments for " ^ name)
