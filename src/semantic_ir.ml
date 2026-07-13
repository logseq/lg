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
  | Typed of Semantic_type.ty * t
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
  | Typed (_, expression) -> unlocated expression
  | Located (_, _, expression) -> unlocated expression
  | expression -> expression

let annotate ty = function
  | Typed (_, expression) -> Typed (ty, expression)
  | expression -> Typed (ty, expression)

let rec type_annotations expression =
  let children = function
    | Typed (_, value) | Located (_, _, value) -> [ value ]
    | Constructor (_, value) -> Option.to_list value
    | Tuple values | List values | Array values | Sequence values -> values
    | Apply (fn, args) -> fn :: args
    | Labelled_apply (fn, args) -> fn :: List.map snd args
    | If (condition, then_expr, else_expr) -> [ condition; then_expr; else_expr ]
    | Fun (_, body) -> [ body ]
    | Let (bindings, body) -> List.map snd bindings @ [ body ]
    | LetRec (_, _, body, args) -> body :: args
    | LetRecIn (_, _, body, next) -> [ body; next ]
    | Match (target, cases) -> target :: List.map snd cases
    | Match_guarded (target, cases) ->
        target
        :: List.concat_map
             (fun (_, guard, body) -> Option.to_list guard @ [ body ])
             cases
    | Try (body, cases) ->
        body
        :: List.concat_map
             (fun (_, guard, handler) -> Option.to_list guard @ [ handler ])
             cases
    | Infix (_, left, right) | Cons (left, right) -> [ left; right ]
    | Prefix (_, value) | Field (value, _) -> [ value ]
    | Record (fields, _) -> List.map snd fields
    | Int _ | Float _ | String _ | Char _ | Bool _ | Unit | Ident _ -> []
  in
  let own = match expression with Typed (ty, _) -> [ ty ] | _ -> [] in
  own @ List.concat_map type_annotations (children expression)
