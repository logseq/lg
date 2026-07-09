type t =
  | Raw of string
  | Int of int
  | String of string
  | Bool of bool
  | Unit
  | Ident of string

let to_source = function
  | Raw source -> source
  | Int value -> string_of_int value
  | String value -> Printf.sprintf "%S" value
  | Bool value -> string_of_bool value
  | Unit -> "()"
  | Ident name -> name

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

let to_parsetree ~context = function
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
