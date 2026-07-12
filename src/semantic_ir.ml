type pattern =
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

