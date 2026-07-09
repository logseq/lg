open Types

let one_arg name args =
  match args with
  | [ arg ] -> Ok arg
  | _ -> Error.error (name ^ " expects 1 arguments")

let keyword_without_prefix code =
  "(let keyword = "
  ^ code
  ^ " in if String.length keyword > 0 && keyword.[0] = ':' then String.sub keyword 1 (String.length keyword - 1) else keyword)"

let has_slash code = "(String.contains (" ^ keyword_without_prefix code ^ ") '/')"

let compile name args =
  match one_arg name args with
  | Error _ as err -> err
  | Ok arg ->
      let bool value = Ok (typed TBool value) in
      match name with
      | "any?" -> bool "true"
      | "rational?" -> bool (string_of_bool (Types.equal arg.ty TInt))
      | "ratio?" | "float?" | "double?" | "decimal?" -> bool "false"
      | "simple-keyword?" -> (
          match arg.ty with
          | TKeyword -> bool ("not (" ^ has_slash arg.code ^ ")")
          | _ -> bool "false")
      | "qualified-keyword?" -> (
          match arg.ty with
          | TKeyword -> bool (has_slash arg.code)
          | _ -> bool "false")
      | "ident?" -> bool (string_of_bool (Types.equal arg.ty TKeyword))
      | "simple-ident?" -> (
          match arg.ty with
          | TKeyword -> bool ("not (" ^ has_slash arg.code ^ ")")
          | _ -> bool "false")
      | "qualified-ident?" -> (
          match arg.ty with
          | TKeyword -> bool (has_slash arg.code)
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
