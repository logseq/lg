type ty =
  | TInt
  | TFloat
  | TChar
  | TString
  | TSymbol
  | TKeyword
  | TBool
  | TUnit
  | TUnknown
  | TVar of string
  | TOcaml of string
  | TOcaml_app of string * ty list
  | TTuple of ty list
  | TArray of ty
  | TRef of ty
  | TList of ty
  | TVector of ty
  | TSet of ty
  | TFn of ty list * ty
  | TRecord of field list
  | TNamed_record of named_record

and field = {
  keyword : string;
  ocaml_name : string;
  ty : ty;
}

and named_record = {
  type_id : Type_id.t;
  nominal : bool;
  type_name : string;
  type_parameters : string list;
  set_module_name : string;
  fields : field list;
}
