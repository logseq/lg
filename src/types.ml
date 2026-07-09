type ty =
  | TInt
  | TString
  | TKeyword
  | TBool
  | TNil
  | TUnit
  | TAny
  | TList of ty
  | TVector of ty
  | TSet of ty
  | TFn of ty list * ty
  | TRecord of field list

and field = {
  keyword : string;
  ocaml_name : string;
  ty : ty;
}

type binding = {
  ocaml_name : string;
  ty : ty;
}

type typed_expr = {
  ty : ty;
  code : string;
  record_values : (field * string) list option;
}

type compiled_item =
  | Emit of string
  | Record_def of {
      var_name : string;
      type_name : string;
      fields : field list;
      values : (field * string) list;
    }

let typed ty code = { ty; code; record_values = None }

let rec equal left right =
  match (left, right) with
  | TAny, _ | _, TAny -> true
  | TInt, TInt
  | TString, TString
  | TKeyword, TKeyword
  | TBool, TBool
  | TNil, TNil
  | TUnit, TUnit ->
      true
  | TList left, TList right -> equal left right
  | TVector left, TVector right -> equal left right
  | TSet left, TSet right -> equal left right
  | TFn (left_args, left_ret), TFn (right_args, right_ret) ->
      List.length left_args = List.length right_args
      && List.for_all2 equal left_args right_args
      && equal left_ret right_ret
  | TRecord left, TRecord right ->
      List.length left = List.length right
      && List.for_all2
           (fun l r -> l.keyword = r.keyword && equal l.ty r.ty)
           left right
  | _ -> false

let rec source_name = function
  | TInt -> "int"
  | TString -> "string"
  | TKeyword -> "keyword"
  | TBool -> "bool"
  | TNil -> "nil"
  | TUnit -> "unit"
  | TAny -> "any"
  | TList ty -> "list<" ^ source_name ty ^ ">"
  | TVector ty -> "vector<" ^ source_name ty ^ ">"
  | TSet ty -> "set<" ^ source_name ty ^ ">"
  | TFn (args, ret) ->
      "fn<(" ^ (args |> List.map source_name |> String.concat ", ") ^ ") -> "
      ^ source_name ret ^ ">"
  | TRecord _ -> "map"

let rec ocaml_name = function
  | TInt -> "int"
  | TString -> "string"
  | TKeyword -> "string"
  | TBool -> "bool"
  | TNil -> "unit"
  | TUnit -> "unit"
  | TAny -> "'a"
  | TList inner -> ocaml_name inner ^ " list"
  | TVector inner -> ocaml_name inner ^ " Rrbvec.t"
  | TSet inner -> ocaml_name inner ^ " list"
  | TFn (args, ret) ->
      (args |> List.map ocaml_name |> String.concat " -> ") ^ " -> " ^ ocaml_name ret
  | TRecord _ -> "record"

let find_field keyword fields =
  List.find_opt (fun field -> field.keyword = keyword) fields

let make_field keyword ty =
  { keyword; ocaml_name = Names.keyword_to_ocaml_name keyword; ty }
