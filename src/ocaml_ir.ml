type pattern =
  | PLocated of Source_node_id.t * Location.t * pattern
  | PVar of string
  | PAny
  | PUnit
  | PInt of int
  | PString of string
  | PBool of bool
  | PConstructor of string * pattern option
  | PTuple of pattern list
  | PList of pattern list
  | PCons of pattern * pattern
  | PRecord of (string * pattern) list
  | PAlias of pattern * string
  | POr of pattern * pattern
  | PConstraint of pattern * string

type t =
  | Located of Source_node_id.t * Location.t * t
  | Int of int
  | Float of string
  | String of string
  | Char of char
  | Bool of bool
  | Unit
  | Constructor of string * t option
  | Tuple of t list
  | Ident of string
  | List of t list
  | Array of t list
  | Apply of t * t list
  | Labelled_apply of t * (string option * t) list
  | If of t * t * t
  | Fun of pattern list * t
  | Sequence of t list
  | Let of (pattern * t) list * t
  | LetRec of string * pattern list * t * t list
  | LetRecIn of string * pattern list * t * t
  | Match of t * (pattern * t) list
  | Match_guarded of t * (pattern * t option * t) list
  | Try of t * (pattern * t option * t) list
  | Infix of string * t * t
  | Prefix of string * t
  | Field of t * string
  | Cons of t * t
  | Record of (string * t) list * string option

let rec unlocated = function
  | Located (_, _, expression) -> unlocated expression
  | expression -> expression

let rec pattern_to_source = function
  | PLocated (_, _, pattern) -> pattern_to_source pattern
  | PVar name -> name
  | PAny -> "_"
  | PUnit -> "()"
  | PInt value -> string_of_int value
  | PString value -> Printf.sprintf "%S" value
  | PBool value -> string_of_bool value
  | PConstructor (name, None) -> name
  | PConstructor (name, Some pattern) -> name ^ " " ^ pattern_to_source pattern
  | PTuple patterns ->
      "(" ^ (patterns |> List.map pattern_to_source |> String.concat ", ") ^ ")"
  | PList patterns ->
      "[" ^ (patterns |> List.map pattern_to_source |> String.concat "; ") ^ "]"
  | PCons (head, tail) -> pattern_to_source head ^ " :: " ^ pattern_to_source tail
  | PRecord fields ->
      "{"
      ^ (fields
        |> List.map (fun (name, pattern) -> name ^ " = " ^ pattern_to_source pattern)
        |> String.concat "; ")
      ^ "; _}"
  | PAlias (pattern, name) -> "(" ^ pattern_to_source pattern ^ " as " ^ name ^ ")"
  | POr (left, right) -> "(" ^ pattern_to_source left ^ " | " ^ pattern_to_source right ^ ")"
  | PConstraint (pattern, type_name) ->
      "(" ^ pattern_to_source pattern ^ " : " ^ type_name ^ ")"

let rec to_source = function
  | Located (_, _, expression) -> to_source expression
  | Int value -> string_of_int value
  | Float value -> value
  | String value -> Printf.sprintf "%S" value
  | Char value -> Printf.sprintf "%C" value
  | Bool value -> string_of_bool value
  | Unit -> "()"
  | Constructor (name, None) -> name
  | Constructor (name, Some value) -> name ^ " (" ^ to_source value ^ ")"
  | Tuple values ->
      "(" ^ (values |> List.map to_source |> String.concat ", ") ^ ")"
  | Ident name -> name
  | List values ->
      "[" ^ (values |> List.map to_source |> String.concat "; ") ^ "]"
  | Array values ->
      "[|" ^ (values |> List.map to_source |> String.concat "; ") ^ "|]"
  | Apply (fn, args) ->
      let args = match args with [] -> [ Unit ] | _ -> args in
      "("
      ^ to_source fn
      ^ " "
      ^ (args |> List.map (fun arg -> "(" ^ to_source arg ^ ")") |> String.concat " ")
      ^ ")"
  | Labelled_apply (fn, args) ->
      let argument_source = function
        | None, argument -> "(" ^ to_source argument ^ ")"
        | Some label, argument -> "~" ^ label ^ ":(" ^ to_source argument ^ ")"
      in
      "(" ^ to_source fn ^ " "
      ^ (args |> List.map argument_source |> String.concat " ")
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
  | LetRec (name, params, body, args) ->
      let params = match params with [] -> [ PUnit ] | _ -> params in
      "(let rec " ^ name ^ " "
      ^ (params |> List.map pattern_to_source |> String.concat " ")
      ^ " = " ^ to_source body ^ " in "
      ^ to_source (Apply (Ident name, args)) ^ ")"
  | LetRecIn (name, params, body, next) ->
      let params = match params with [] -> [ PUnit ] | _ -> params in
      "(let rec " ^ name ^ " "
      ^ (params |> List.map pattern_to_source |> String.concat " ")
      ^ " = " ^ to_source body ^ " in " ^ to_source next ^ ")"
  | Match (target, cases) ->
      "(match " ^ to_source target ^ " with "
      ^ (cases
        |> List.map (fun (pattern, body) ->
               "| " ^ pattern_to_source pattern ^ " -> " ^ to_source body)
        |> String.concat " ")
      ^ ")"
  | Match_guarded (target, cases) ->
      "(match " ^ to_source target ^ " with "
      ^ (cases
        |> List.map (fun (pattern, guard, body) ->
               "| " ^ pattern_to_source pattern
               ^ (match guard with None -> "" | Some guard -> " when " ^ to_source guard)
               ^ " -> " ^ to_source body)
        |> String.concat " ")
      ^ ")"
  | Try (body, cases) ->
      "(try " ^ to_source body ^ " with "
      ^ (cases
        |> List.map (fun (pattern, guard, handler) ->
               "| " ^ pattern_to_source pattern
               ^ (match guard with None -> "" | Some guard -> " when " ^ to_source guard)
               ^ " -> " ^ to_source handler)
        |> String.concat " ")
      ^ ")"
  | Infix (operator, left, right) ->
      "(" ^ to_source left ^ " " ^ operator ^ " " ^ to_source right ^ ")"
  | Prefix (operator, expression) ->
      "(" ^ operator ^ " " ^ to_source expression ^ ")"
  | Field (target, field_name) -> to_source target ^ "." ^ field_name
  | Cons (head, tail) -> "(" ^ to_source head ^ " :: " ^ to_source tail ^ ")"
  | Record (fields, type_name) ->
      let fields =
        fields
        |> List.map (fun (name, value) -> name ^ " = " ^ to_source value)
        |> String.concat "; "
      in
      let value = "{" ^ fields ^ "}" in
      (match type_name with None -> value | Some name -> "(" ^ value ^ " : " ^ name ^ ")")

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

let core_type_of_source source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf ("generated type " ^ source);
  try Parse.core_type lexbuf
  with _ -> Ast_helper.Typ.constr ~loc (lid (longident_of_string source)) []

let node_id_attribute node_id =
  let payload =
    Parsetree.PStr
      [ Ast_helper.Str.eval
          (Ast_helper.Exp.constant
             (Ast_helper.Const.string (Source_node_id.to_string node_id))) ]
  in
  Ast_helper.Attr.mk (str "cljml.node_id") payload

let rec pattern_node_ids = function
  | PLocated (node_id, _, pattern) -> node_id :: pattern_node_ids pattern
  | PConstructor (_, payload) ->
      Option.fold ~none:[] ~some:pattern_node_ids payload
  | PTuple patterns | PList patterns -> List.concat_map pattern_node_ids patterns
  | PCons (head, tail) | POr (head, tail) ->
      pattern_node_ids head @ pattern_node_ids tail
  | PRecord fields ->
      fields |> List.concat_map (fun (_, pattern) -> pattern_node_ids pattern)
  | PAlias (pattern, _) | PConstraint (pattern, _) -> pattern_node_ids pattern
  | PVar _ | PAny | PUnit | PInt _ | PString _ | PBool _ -> []

let add_pattern_node_ids patterns (expression : Parsetree.expression) =
  let attributes =
    patterns |> List.concat_map pattern_node_ids |> List.map node_id_attribute
  in
  { expression with pexp_attributes = attributes @ expression.pexp_attributes }

let rec pattern_to_parsetree = function
  | PLocated (node_id, location, pattern) ->
      let pattern : Parsetree.pattern = pattern_to_parsetree pattern in
      {
        pattern with
        ppat_loc = location;
        ppat_attributes = node_id_attribute node_id :: pattern.ppat_attributes;
      }
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
  | PConstructor (name, None) ->
      Ast_helper.Pat.construct ~loc (lid (longident_of_string name)) None
  | PConstructor (name, Some pattern) ->
      Ast_helper.Pat.construct ~loc (lid (longident_of_string name))
        (Some ([], pattern_to_parsetree pattern))
  | PTuple patterns ->
      Ast_helper.Pat.tuple ~loc
        (List.map (fun pattern -> (None, pattern_to_parsetree pattern)) patterns)
        Closed
  | PList patterns -> pattern_list_to_parsetree patterns
  | PCons (head, tail) ->
      let pair =
        Ast_helper.Pat.tuple ~loc
          [ (None, pattern_to_parsetree head); (None, pattern_to_parsetree tail) ]
          Closed
      in
      Ast_helper.Pat.construct ~loc (lid (Longident.Lident "::")) (Some ([], pair))
  | PRecord fields ->
      Ast_helper.Pat.record ~loc
        (List.map
           (fun (name, pattern) ->
             (lid (longident_of_string name), pattern_to_parsetree pattern))
           fields)
        Asttypes.Open
  | PAlias (pattern, name) ->
      Ast_helper.Pat.alias ~loc (pattern_to_parsetree pattern) (str name)
  | POr (left, right) ->
      Ast_helper.Pat.or_ ~loc (pattern_to_parsetree left) (pattern_to_parsetree right)
  | PConstraint (pattern, type_name) ->
      Ast_helper.Pat.constraint_ ~loc (pattern_to_parsetree pattern)
        (core_type_of_source type_name)

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

and guarded_cases_to_parsetree ~context cases =
  let rec build_cases acc = function
    | [] -> Ok (List.rev acc)
    | (pattern, guard, body) :: rest -> (
        match
          ( Option.fold ~none:(Ok None)
              ~some:(fun guard ->
                to_parsetree ~context guard |> Result.map Option.some)
              guard,
            to_parsetree ~context body )
        with
        | (Error _ as err), _ -> err
        | _, (Error _ as err) -> err
        | Ok guard, Ok body ->
            build_cases
              (Ast_helper.Exp.case (pattern_to_parsetree pattern) ?guard body :: acc)
              rest)
  in
  build_cases [] cases

and to_parsetree ~context = function
  | Located (node_id, location, expression) ->
      to_parsetree ~context expression
      |> Result.map (fun (expression : Parsetree.expression) ->
             let payload =
               Parsetree.PStr
                 [ Ast_helper.Str.eval
                     (Ast_helper.Exp.constant
                        (Ast_helper.Const.string (Source_node_id.to_string node_id))) ]
             in
             let attribute =
               Ast_helper.Attr.mk (str "cljml.node_id") payload
             in
             {
               expression with
               pexp_loc = location;
               pexp_attributes = attribute :: expression.pexp_attributes;
             })
  | Int value ->
      Ok (Ast_helper.Exp.constant ~loc (Ast_helper.Const.int ~loc value))
  | Float value ->
      Ok (Ast_helper.Exp.constant ~loc (Ast_helper.Const.float ~loc value))
  | String value ->
      Ok (Ast_helper.Exp.constant ~loc (Ast_helper.Const.string ~loc value))
  | Char value ->
      Ok (Ast_helper.Exp.constant ~loc (Ast_helper.Const.char ~loc value))
  | Bool value ->
      Ok
        (Ast_helper.Exp.construct ~loc
           (lid (Longident.Lident (string_of_bool value)))
           None)
  | Unit ->
      Ok
        (Ast_helper.Exp.construct ~loc (lid (Longident.Lident "()")) None)
  | Constructor (name, None) ->
      Ok (Ast_helper.Exp.construct ~loc (lid (longident_of_string name)) None)
  | Constructor (name, Some value) -> (
      match to_parsetree ~context value with
      | Error _ as err -> err
      | Ok value ->
          Ok
            (Ast_helper.Exp.construct ~loc (lid (longident_of_string name))
               (Some value)))
  | Tuple values -> (
      match expressions_to_parsetree ~context values with
      | Error _ as err -> err
      | Ok values ->
          Ok
            (Ast_helper.Exp.tuple ~loc
               (List.map (fun value -> (None, value)) values)))
  | Ident name ->
      Ok (Ast_helper.Exp.ident ~loc (lid (longident_of_string name)))
  | List values -> list_to_parsetree ~context values
  | Array values -> (
      match expressions_to_parsetree ~context values with
      | Error _ as err -> err
      | Ok values -> Ok (Ast_helper.Exp.array ~loc values))
  | Apply (fn, args) -> (
      let args = match args with [] -> [ Unit ] | _ -> args in
      match (to_parsetree ~context fn, expressions_to_parsetree ~context args) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok args ->
          Ok
            (Ast_helper.Exp.apply ~loc fn
               (List.map (fun arg -> (Asttypes.Nolabel, arg)) args)))
  | Labelled_apply (fn, args) -> (
      let rec arguments_to_parsetree acc = function
        | [] -> Ok (List.rev acc)
        | (label, argument) :: rest -> (
            match to_parsetree ~context argument with
            | Error _ as err -> err
            | Ok argument ->
                let label =
                  match label with
                  | None -> Asttypes.Nolabel
                  | Some label -> Asttypes.Labelled label
                in
                arguments_to_parsetree ((label, argument) :: acc) rest)
      in
      match (to_parsetree ~context fn, arguments_to_parsetree [] args) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok args -> Ok (Ast_helper.Exp.apply ~loc fn args))
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
            (add_pattern_node_ids patterns
               (Ast_helper.Exp.function_ ~loc
                  (List.map function_parameter patterns)
                  None (Pfunction_body body))))
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
                    Ok
                      (add_pattern_node_ids [ pattern ]
                         (Ast_helper.Exp.let_ ~loc Asttypes.Nonrecursive [ binding ]
                            body)))
          in
          build bindings)
  | LetRec (name, params, body, args) -> (
      match
        ( to_parsetree ~context (Fun (params, body)),
          to_parsetree ~context (Apply (Ident name, args)) )
      with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok call ->
          let binding =
            Ast_helper.Vb.mk ~loc (Ast_helper.Pat.var ~loc (str name)) fn
          in
          Ok (Ast_helper.Exp.let_ ~loc Asttypes.Recursive [ binding ] call))
  | LetRecIn (name, params, body, next) -> (
      match (to_parsetree ~context (Fun (params, body)), to_parsetree ~context next) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok fn, Ok next ->
          let binding =
            Ast_helper.Vb.mk ~loc (Ast_helper.Pat.var ~loc (str name)) fn
          in
          Ok (Ast_helper.Exp.let_ ~loc Asttypes.Recursive [ binding ] next))
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
  | Match_guarded (target, cases) -> (
      match to_parsetree ~context target with
      | Error _ as err -> err
      | Ok target ->
          match guarded_cases_to_parsetree ~context cases with
          | Error _ as err -> err
          | Ok cases -> Ok (Ast_helper.Exp.match_ ~loc target cases))
  | Try (body, cases) -> (
      match to_parsetree ~context body with
      | Error _ as err -> err
      | Ok body ->
          match guarded_cases_to_parsetree ~context cases with
          | Error _ as err -> err
          | Ok cases -> Ok (Ast_helper.Exp.try_ ~loc body cases))
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
  | Record (fields, type_name) ->
      let rec build_fields acc = function
        | [] -> Ok (List.rev acc)
        | (name, value) :: rest -> (
            match to_parsetree ~context value with
            | Error _ as err -> err
            | Ok value ->
                build_fields
                  ((lid (longident_of_string name), value) :: acc)
                  rest)
      in
      build_fields [] fields
      |> Result.map (fun fields ->
             let expression = Ast_helper.Exp.record ~loc fields None in
             match type_name with
             | None -> expression
             | Some name ->
                 Ast_helper.Exp.constraint_ ~loc expression
                   (core_type_of_source name))
