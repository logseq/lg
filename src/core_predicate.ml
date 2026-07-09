open Types

let one_arg name args =
  match args with
  | [ arg ] -> Ok arg
  | _ -> Error.error (name ^ " expects 1 arguments")

let identifier_body_code code =
  "(let value = "
  ^ code
  ^ " in if String.length value > 0 && value.[0] = ':' then String.sub value 1 (String.length value - 1) else value)"

let has_slash code = "(String.contains (" ^ identifier_body_code code ^ ") '/')"

let compile name args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg ->
      let bool value = Ok (typed TBool value) in
      match name with
      | "any?" -> bool "true"
      | "rational?" -> bool (string_of_bool (Types.equal arg.ty TInt))
      | "ratio?" | "float?" | "double?" | "decimal?" -> bool "false"
      | "symbol?" -> bool (string_of_bool (Types.equal arg.ty TSymbol))
      | "simple-symbol?" -> (
          match arg.ty with
          | TSymbol -> bool ("not (" ^ has_slash arg.code ^ ")")
          | _ -> bool "false")
      | "qualified-symbol?" -> (
          match arg.ty with
          | TSymbol -> bool (has_slash arg.code)
          | _ -> bool "false")
      | "simple-keyword?" -> (
          match arg.ty with
          | TKeyword -> bool ("not (" ^ has_slash arg.code ^ ")")
          | _ -> bool "false")
      | "qualified-keyword?" -> (
          match arg.ty with
          | TKeyword -> bool (has_slash arg.code)
          | _ -> bool "false")
      | "ident?" ->
          bool
            (string_of_bool
               (match arg.ty with TKeyword | TSymbol -> true | _ -> false))
      | "simple-ident?" -> (
          match arg.ty with
          | TKeyword | TSymbol -> bool ("not (" ^ has_slash arg.code ^ ")")
          | _ -> bool "false")
      | "qualified-ident?" -> (
          match arg.ty with
          | TKeyword | TSymbol -> bool (has_slash arg.code)
          | _ -> bool "false")
      | "sequential?" ->
          bool
            (string_of_bool
               (match arg.ty with TList _ | TVector _ -> true | _ -> false))
      | "reversible?" ->
          bool
            (string_of_bool
               (match arg.ty with TString | TList _ | TVector _ -> true | _ -> false))
      | "sorted?" -> bool "false"
      | _ -> Error.error ("unknown function " ^ name)
