type source_span = {
  start_offset : int;
  end_offset : int;
}

type token_desc =
  | Lparen
  | Anon_lparen
  | Quote
  | Syntax_quote
  | Unquote
  | Unquote_splicing
  | Deref
  | Var_quote of string
  | Rparen
  | Lbracket
  | Rbracket
  | Lbrace
  | Set_lbrace
  | Rbrace
  | Symbol of string
  | Keyword of string
  | String of string
  | Regex of string
  | Int of int
  | Float of string
  | Char of char
  | Bool of bool

type token = {
  desc : token_desc;
  span : source_span;
}

type core_symbol =
  | Core_assoc
  | Core_drop
  | Core_filter
  | Core_get
  | Core_map
  | Core_mapcat
  | Core_take
  | Core_update

let core_symbol_name = function
  | Core_assoc -> "assoc"
  | Core_drop -> "drop"
  | Core_filter -> "filter"
  | Core_get -> "get"
  | Core_map -> "map"
  | Core_mapcat -> "mapcat"
  | Core_take -> "take"
  | Core_update -> "update"

let core_symbol_qualified_name symbol =
  "clojure.core/" ^ core_symbol_name symbol

type form =
  | FSymbol of string
  | FCoreSymbol of core_symbol
  | FKeyword of string
  | FString of string
  | FRegex of string
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
