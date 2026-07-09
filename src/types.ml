type ty =
  | TInt
  | TString
  | TSymbol
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
  row_param_types : string option list;
}

type typed_expr = {
  ty : ty;
  code : string;
  record_values : (field * string) list option;
}

type value_pattern =
  | Named of string
  | Unit_pattern
  | Ignore_pattern

type compiled_item =
  | Emit of string
  | Value_binding of {
      pattern : value_pattern;
      expression : string;
    }
  | Comment of string
  | Record_def of {
      var_name : string;
      type_name : string;
      fields : field list;
      values : (field * string) list;
    }

let typed ty code = { ty; code; record_values = None }

let binding ?(row_param_types = []) ocaml_name ty =
  { ocaml_name; ty; row_param_types }

let rec equal left right =
  match (left, right) with
  | TAny, _ | _, TAny -> true
  | TInt, TInt
  | TString, TString
  | TSymbol, TSymbol
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

let rec compatible ~expected ~actual =
  match (expected, actual) with
  | TAny, _ | _, TAny -> true
  | TRecord expected_fields, TRecord actual_fields ->
      expected_fields
      |> List.for_all (fun expected_field ->
             match
               List.find_opt
                 (fun actual_field -> actual_field.keyword = expected_field.keyword)
                 actual_fields
             with
             | Some actual_field -> compatible ~expected:expected_field.ty ~actual:actual_field.ty
             | None -> false)
  | _ -> equal expected actual

let rec source_name = function
  | TInt -> "int"
  | TString -> "string"
  | TSymbol -> "symbol"
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
  | TSymbol -> "string"
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
