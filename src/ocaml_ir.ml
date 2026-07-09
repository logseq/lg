type pattern =
  | PVar of string
  | PAny
  | PUnit
  | PInt of int
  | PString of string
  | PBool of bool
  | PList of pattern list
  | PCons of pattern * pattern
  | PConstraint of pattern * string

type t =
  | Raw of string
  | Int of int
  | String of string
  | Bool of bool
  | Unit
  | Ident of string
  | List of t list
  | Apply of t * t list
  | If of t * t * t
  | Fun of pattern list * t
  | Sequence of t list
  | Let of (pattern * t) list * t
  | Match of t * (pattern * t) list
  | Infix of string * t * t
  | Prefix of string * t
  | Field of t * string
  | Cons of t * t

let rec pattern_to_source = function
  | PVar name -> name
  | PAny -> "_"
  | PUnit -> "()"
  | PInt value -> string_of_int value
  | PString value -> Printf.sprintf "%S" value
  | PBool value -> string_of_bool value
  | PList patterns ->
      "[" ^ (patterns |> List.map pattern_to_source |> String.concat "; ") ^ "]"
  | PCons (head, tail) -> pattern_to_source head ^ " :: " ^ pattern_to_source tail
  | PConstraint (pattern, type_name) ->
      "(" ^ pattern_to_source pattern ^ " : " ^ type_name ^ ")"

let rec to_source = function
  | Raw source -> source
  | Int value -> string_of_int value
  | String value -> Printf.sprintf "%S" value
  | Bool value -> string_of_bool value
  | Unit -> "()"
  | Ident name -> name
  | List values ->
      "[" ^ (values |> List.map to_source |> String.concat "; ") ^ "]"
  | Apply (fn, args) ->
      let args = match args with [] -> [ Unit ] | _ -> args in
      "("
      ^ to_source fn
      ^ " "
      ^ (args |> List.map (fun arg -> "(" ^ to_source arg ^ ")") |> String.concat " ")
      ^ ")"
  | If (condition, then_expr, else_expr) ->
      "(if " ^ to_source condition ^ " then " ^ to_source then_expr ^ " else "
      ^ to_source else_expr ^ ")"
  | Fun (patterns, body) ->
      let patterns = match patterns with [] -> [ PUnit ] | _ -> patterns in
      "(fun "
      ^ (patterns |> List.map pattern_to_source |> String.concat " ")
      ^ " -> " ^ to_source body ^ ")"
  | Sequence expressions -> (
      match expressions with
      | [] -> "()"
      | [ expression ] -> to_source expression
      | expression :: rest ->
          "(let _ = " ^ to_source expression ^ " in "
          ^ to_source (Sequence rest)
          ^ ")")
  | Let (bindings, body) ->
      List.fold_right
        (fun (pattern, value) acc ->
          "(let " ^ pattern_to_source pattern ^ " = " ^ to_source value
          ^ " in " ^ acc ^ ")")
        bindings (to_source body)
  | Match (target, cases) ->
      "(match " ^ to_source target ^ " with "
      ^ (cases
        |> List.map (fun (pattern, body) ->
               "| " ^ pattern_to_source pattern ^ " -> " ^ to_source body)
        |> String.concat " ")
      ^ ")"
  | Infix (operator, left, right) ->
      "(" ^ to_source left ^ " " ^ operator ^ " " ^ to_source right ^ ")"
  | Prefix (operator, expression) ->
      "(" ^ operator ^ " " ^ to_source expression ^ ")"
  | Field (target, field_name) -> to_source target ^ "." ^ field_name
  | Cons (head, tail) -> "(" ^ to_source head ^ " :: " ^ to_source tail ^ ")"

let loc = Location.none
let lid value = Location.mkloc value loc
let str value = Location.mkloc value loc

let longident_of_string name =
  match String.split_on_char '.' name with
  | [] -> Longident.Lident name
  | first :: rest ->
      List.fold_left
        (fun path segment ->
          Longident.Ldot (lid path, str segment))
        (Longident.Lident first) rest

let parse_expression ~context source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf context;
  try Ok (Parse.expression lexbuf)
  with exn ->
    Error.error
      ("generated OCaml expression did not parse in " ^ context ^ ": "
     ^ Printexc.to_string exn)

let rec pattern_to_parsetree = function
  | PVar name -> Ast_helper.Pat.var ~loc (str name)
  | PAny -> Ast_helper.Pat.any ~loc ()
  | PUnit -> Ast_helper.Pat.construct ~loc (lid (Longident.Lident "()")) None
  | PInt value -> Ast_helper.Pat.constant ~loc (Ast_helper.Const.int ~loc value)
  | PString value ->
      Ast_helper.Pat.constant ~loc (Ast_helper.Const.string ~loc value)
  | PBool value ->
      Ast_helper.Pat.construct ~loc
        (lid (Longident.Lident (string_of_bool value)))
        None
  | PList patterns -> pattern_list_to_parsetree patterns
  | PCons (head, tail) ->
      let pair =
        Ast_helper.Pat.tuple ~loc
          [ (None, pattern_to_parsetree head); (None, pattern_to_parsetree tail) ]
          Closed
      in
      Ast_helper.Pat.construct ~loc (lid (Longident.Lident "::")) (Some ([], pair))
  | PConstraint (pattern, type_name) ->
      Ast_helper.Pat.constraint_ ~loc (pattern_to_parsetree pattern)
        (Ast_helper.Typ.constr ~loc (lid (longident_of_string type_name)) [])

and pattern_list_to_parsetree = function
  | [] -> Ast_helper.Pat.construct ~loc (lid (Longident.Lident "[]")) None
  | pattern :: rest ->
      let pair =
        Ast_helper.Pat.tuple ~loc
          [ (None, pattern_to_parsetree pattern); (None, pattern_list_to_parsetree rest) ]
          Closed
      in
      Ast_helper.Pat.construct ~loc (lid (Longident.Lident "::")) (Some ([], pair))

let function_parameter pattern =
  {
    Parsetree.pparam_loc = loc;
    pparam_desc = Pparam_val (Asttypes.Nolabel, None, pattern_to_parsetree pattern);
  }

let rec list_to_parsetree ~context = function
  | [] ->
      Ok
        (Ast_helper.Exp.construct ~loc (lid (Longident.Lident "[]")) None)
  | value :: rest -> (
      match (to_parsetree ~context value, list_to_parsetree ~context rest) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok value, Ok rest ->
          let pair = Ast_helper.Exp.tuple ~loc [ (None, value); (None, rest) ] in
          Ok
            (Ast_helper.Exp.construct ~loc
               (lid (Longident.Lident "::"))
               (Some pair)))

and expressions_to_parsetree ~context expressions =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | expression :: rest -> (
        match to_parsetree ~context expression with
        | Error _ as err -> err
        | Ok expression -> loop (expression :: acc) rest)
  in
  loop [] expressions

and to_parsetree ~context = function
  | Raw source -> parse_expression ~context source
  | Int value ->
      Ok (Ast_helper.Exp.constant ~loc (Ast_helper.Const.int ~loc value))
  | String value ->
      Ok (Ast_helper.Exp.constant ~loc (Ast_helper.Const.string ~loc value))
  | Bool value ->
      Ok
        (Ast_helper.Exp.construct ~loc
           (lid (Longident.Lident (string_of_bool value)))
           None)
  | Unit ->
      Ok
        (Ast_helper.Exp.construct ~loc (lid (Longident.Lident "()")) None)
  | Ident name ->
      Ok (Ast_helper.Exp.ident ~loc (lid (longident_of_string name)))
  | List values -> list_to_parsetree ~context values
  | Apply (fn, args) -> (
      let args = match args with [] -> [ Unit ] | _ -> args in
      match (to_parsetree ~context fn, expressions_to_parsetree ~context args) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok args ->
          Ok
            (Ast_helper.Exp.apply ~loc fn
               (List.map (fun arg -> (Asttypes.Nolabel, arg)) args)))
  | If (condition, then_expr, else_expr) -> (
      match
        ( to_parsetree ~context condition,
          to_parsetree ~context then_expr,
          to_parsetree ~context else_expr )
      with
      | (Error _ as err), _, _ -> err
      | _, (Error _ as err), _ -> err
      | _, _, (Error _ as err) -> err
      | Ok condition, Ok then_expr, Ok else_expr ->
          Ok (Ast_helper.Exp.ifthenelse ~loc condition then_expr (Some else_expr)))
  | Fun (patterns, body) -> (
      let patterns = match patterns with [] -> [ PUnit ] | _ -> patterns in
      match to_parsetree ~context body with
      | Error _ as err -> err
      | Ok body ->
          Ok
            (Ast_helper.Exp.function_ ~loc
               (List.map function_parameter patterns)
               None (Pfunction_body body)))
  | Sequence expressions -> (
      let rec build = function
        | [] -> Ok (Ast_helper.Exp.construct ~loc (lid (Longident.Lident "()")) None)
        | [ expression ] -> to_parsetree ~context expression
        | expression :: rest -> (
            match (to_parsetree ~context expression, build rest) with
            | (Error _ as err), _ -> err
            | _, (Error _ as err) -> err
            | Ok expression, Ok body ->
                let binding = Ast_helper.Vb.mk ~loc (Ast_helper.Pat.any ~loc ()) expression in
                Ok (Ast_helper.Exp.let_ ~loc Asttypes.Nonrecursive [ binding ] body))
      in
      build expressions)
  | Let (bindings, body) -> (
      match to_parsetree ~context body with
      | Error _ as err -> err
      | Ok body ->
          let rec build = function
            | [] -> Ok body
            | (pattern, value) :: rest -> (
                match (to_parsetree ~context value, build rest) with
                | (Error _ as err), _ -> err
                | _, (Error _ as err) -> err
                | Ok value, Ok body ->
                    let binding =
                      Ast_helper.Vb.mk ~loc (pattern_to_parsetree pattern) value
                    in
                    Ok (Ast_helper.Exp.let_ ~loc Asttypes.Nonrecursive [ binding ] body))
          in
          build bindings)
  | Match (target, cases) -> (
      match to_parsetree ~context target with
      | Error _ as err -> err
      | Ok target ->
          let rec build_cases acc = function
            | [] -> Ok (List.rev acc)
            | (pattern, body) :: rest -> (
                match to_parsetree ~context body with
                | Error _ as err -> err
                | Ok body ->
                    build_cases
                      (Ast_helper.Exp.case (pattern_to_parsetree pattern) body :: acc)
                      rest)
          in
          match build_cases [] cases with
          | Error _ as err -> err
          | Ok cases -> Ok (Ast_helper.Exp.match_ ~loc target cases))
  | Infix (operator, left, right) -> (
      match (to_parsetree ~context left, to_parsetree ~context right) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok left, Ok right ->
          Ok
            (Ast_helper.Exp.apply ~loc
               (Ast_helper.Exp.ident ~loc (lid (longident_of_string operator)))
               [ (Asttypes.Nolabel, left); (Asttypes.Nolabel, right) ]))
  | Prefix (operator, expression) -> (
      match to_parsetree ~context expression with
      | Error _ as err -> err
      | Ok expression ->
          Ok
            (Ast_helper.Exp.apply ~loc
               (Ast_helper.Exp.ident ~loc (lid (longident_of_string operator)))
               [ (Asttypes.Nolabel, expression) ]))
  | Field (target, field_name) -> (
      match to_parsetree ~context target with
      | Error _ as err -> err
      | Ok target ->
          Ok
            (Ast_helper.Exp.field ~loc target
               (lid (longident_of_string field_name))))
  | Cons (head, tail) -> (
      match (to_parsetree ~context head, to_parsetree ~context tail) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok head, Ok tail ->
          let pair = Ast_helper.Exp.tuple ~loc [ (None, head); (None, tail) ] in
          Ok
            (Ast_helper.Exp.construct ~loc (lid (Longident.Lident "::"))
               (Some pair)))
