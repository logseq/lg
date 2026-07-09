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
