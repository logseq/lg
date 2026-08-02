type metavariable = {
  id : int;
  location : Location.t option;
}

type ty =
  | TInt
  | TFloat
  | TChar
  | TString
  | TRegex
  | TMap_keys
  | TSymbol
  | TKeyword
  | TBool
  | TUnit
  | TNil
  | TNullable of ty
  | TUnknown
  | TMeta of metavariable
  | TVar of string
  | TOcaml of string
  | TOcaml_app of string * ty list
  | TTuple of ty list
  | TArray of ty
  | TRef of ty
  | TList of ty
  | TVector of ty
  | TSet of ty
  | TSeq of ty
  | TFn of ty list * ty
  | TOverloaded_fn of fn_arity list
  | TRecord of field list
  | TNamed_record of named_record

and field = {
  keyword : string;
  ocaml_name : string;
  ty : ty;
  mutable_ : bool;
  location : Location.t option;
}

and named_record = {
  type_id : Type_id.t;
  nominal : bool;
  extensible : bool;
  type_name : string;
  type_parameters : string list;
  type_arguments : ty list;
  set_module_name : string;
  fields : field list;
}

and fn_arity = {
  fixed_params : ty list;
  rest_param : ty option;
  return_ty : ty;
}

type scheme_variable =
  | Declared_variable of string
  | Inferred_variable of {
      metavariable_id : int;
      name : string;
    }

type scheme = {
  quantified : scheme_variable list;
  body : ty;
}
