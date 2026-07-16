let rec pattern = function
  | Semantic_ir.PLocated (node_id, location, value) ->
      Ocaml_ir.PLocated (node_id, location, pattern value)
  | Semantic_ir.PVar name -> Ocaml_ir.PVar name
  | PAny -> PAny
  | PUnit -> PUnit
  | PInt value -> PInt value
  | PString value -> PString value
  | PBool value -> PBool value
  | PConstructor (name, payload) ->
      PConstructor (name, Option.map pattern payload)
  | PTuple patterns -> PTuple (List.map pattern patterns)
  | PList patterns -> PList (List.map pattern patterns)
  | PCons (head, tail) -> PCons (pattern head, pattern tail)
  | PRecord fields ->
      PRecord (List.map (fun (name, value) -> (name, pattern value)) fields)
  | PAlias (value, name) -> PAlias (pattern value, name)
  | POr (left, right) -> POr (pattern left, pattern right)
  | PConstraint (value, type_name) -> PConstraint (pattern value, type_name)

let rec expression = function
  | Semantic_ir.Typed (_, value) -> expression value
  | Semantic_ir.Located (node_id, location, value) ->
      Ocaml_ir.Located (node_id, location, expression value)
  | Int value -> Int value
  | Float value -> Float value
  | String value -> String value
  | Char value -> Char value
  | Bool value -> Bool value
  | Unit -> Unit
  | Constructor (name, payload) ->
      Constructor (name, Option.map expression payload)
  | Tuple values -> Tuple (List.map expression values)
  | Ident name -> Ident name
  | List values -> List (List.map expression values)
  | Array values -> Array (List.map expression values)
  | Apply (fn, args) -> (
      match Semantic_ir.scoped_application fn args with
      | Some scoped -> expression scoped
      | None -> Apply (expression fn, List.map expression args))
  | Uncurried_apply (fn, args) ->
      Uncurried_apply (expression fn, List.map expression args)
  | Labelled_apply (fn, args) ->
      Labelled_apply
        (expression fn, List.map (fun (label, arg) -> (label, expression arg)) args)
  | If (condition, then_expr, else_expr) ->
      If (expression condition, expression then_expr, expression else_expr)
  | Fun (patterns, body) -> Fun (List.map pattern patterns, expression body)
  | Sequence values -> Sequence (List.map expression values)
  | Let (bindings, body) -> (
      match Semantic_ir.scoped_let bindings body with
      | Some scoped -> expression scoped
      | None ->
          Let
            ( List.map
                (fun (pat, value) -> (pattern pat, expression value))
                bindings,
              expression body ))
  | LetRec (name, params, body, args) ->
      LetRec
        (name, List.map pattern params, expression body, List.map expression args)
  | LetRecIn (name, params, body, next) ->
      LetRecIn
        (name, List.map pattern params, expression body, expression next)
  | Match (target, cases) ->
      Match
        ( expression target,
          List.map (fun (pat, body) -> (pattern pat, expression body)) cases )
  | Match_guarded (target, cases) ->
      Match_guarded
        ( expression target,
          List.map
            (fun (pat, guard, body) ->
              (pattern pat, Option.map expression guard, expression body))
            cases )
  | Try (body, cases) ->
      Try
        ( expression body,
          List.map
            (fun (pat, guard, handler) ->
              (pattern pat, Option.map expression guard, expression handler))
            cases )
  | Infix (operator, left, right) ->
      Infix (operator, expression left, expression right)
  | Prefix (operator, value) -> Prefix (operator, expression value)
  | Field (target, name) -> Field (expression target, name)
  | Cons (head, tail) -> Cons (expression head, expression tail)
  | Record (fields, type_name) ->
      Record
        (List.map (fun (name, value) -> (name, expression value)) fields, type_name)
  | PackDynamic { conversion; _ }
  | UnpackDynamic { conversion; _ }
  | NullableToSeq { conversion; _ } ->
      expression conversion
