type token =
  | Lparen
  | Rparen
  | Lbracket
  | Rbracket
  | Lbrace
  | Rbrace
  | Symbol of string
  | Keyword of string
  | String of string
  | Int of int
  | Bool of bool
  | Nil

type form =
  | FSymbol of string
  | FKeyword of string
  | FString of string
  | FInt of int
  | FBool of bool
  | FNil
  | FList of form list
  | FVector of form list
  | FMap of (form * form) list
