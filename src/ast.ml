type source_span = {
  start_offset : int;
  end_offset : int;
}

type token_desc =
  | Lparen
  | Rparen
  | Lbracket
  | Rbracket
  | Lbrace
  | Set_lbrace
  | Rbrace
  | Symbol of string
  | Keyword of string
  | String of string
  | Int of int
  | Float of string
  | Char of char
  | Bool of bool

type token = {
  desc : token_desc;
  span : source_span;
}

type form =
  | FSymbol of string
  | FKeyword of string
  | FString of string
  | FInt of int
  | FFloat of string
  | FChar of char
  | FBool of bool
  | FList of form list
  | FVector of form list
  | FMap of (form * form) list

type located_form = {
  form : form;
  span : source_span;
  children : located_form list;
}
