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
  | TNamed_record of named_record

and field = {
  keyword : string;
  ocaml_name : string;
  ty : ty;
}

and named_record = {
  type_name : string;
  set_module_name : string;
  fields : field list;
}

type binding = {
  ocaml_name : string;
  ty : ty;
  row_param_types : string option list;
}

type typed_expr = {
  ty : ty;
  code : string;
  ocaml_expr : Ocaml_ir.t;
  record_values : (field * Ocaml_ir.t) list option;
}

type value_pattern =
  | Named of string
  | Unit_pattern
  | Ignore_pattern

type compiled_item =
  | Value_binding of {
      pattern : value_pattern;
      expression : Ocaml_ir.t;
    }
  | Comment of string
  | Type_def of {
      type_name : string;
      fields : field list;
    }
  | Group of compiled_item list
  | Module_def of {
      module_name : string;
      items : compiled_item list;
    }
  | Record_def of {
      var_name : string;
      type_name : string;
      set_module_name : string;
      fields : field list;
      values : (field * Ocaml_ir.t) list;
    }

let typed ty code =
  { ty; code; ocaml_expr = Ocaml_ir.Raw code; record_values = None }

let typed_ir ty ocaml_expr =
  { ty; code = Ocaml_ir.to_source ocaml_expr; ocaml_expr; record_values = None }

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
  | (TRecord left, TNamed_record { fields = right; _ })
  | (TNamed_record { fields = left; _ }, TRecord right)
  | (TNamed_record { fields = left; _ }, TNamed_record { fields = right; _ }) ->
      List.length left = List.length right
      && List.for_all2
           (fun l r -> l.keyword = r.keyword && equal l.ty r.ty)
           left right
  | _ -> false

let rec compatible ~expected ~actual =
  match (expected, actual) with
  | TAny, _ | _, TAny -> true
  | (TRecord expected_fields | TNamed_record { fields = expected_fields; _ }),
    (TRecord actual_fields | TNamed_record { fields = actual_fields; _ }) ->
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
  | TNamed_record _ -> "map"

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
  | TSet inner -> (
      match set_module_name inner with
      | Ok set_module -> set_module ^ ".t"
      | Error _ -> "unsupported_set<" ^ ocaml_name inner ^ ">")
  | TFn (args, ret) ->
      (args |> List.map ocaml_name |> String.concat " -> ") ^ " -> " ^ ocaml_name ret
  | TRecord _ -> "record"
  | TNamed_record record -> record.type_name

and set_module_name = function
  | TInt -> Ok "Cljml.Core_set.Int_set"
  | TString | TSymbol | TKeyword -> Ok "Cljml.Core_set.String_set"
  | TBool -> Ok "Cljml.Core_set.Bool_set"
  | TList TInt -> Ok "Cljml.Core_set.Int_list_set"
  | TList (TString | TSymbol | TKeyword) -> Ok "Cljml.Core_set.String_list_set"
  | TList TBool -> Ok "Cljml.Core_set.Bool_list_set"
  | TVector TInt -> Ok "Cljml.Core_set.Int_vector_set"
  | TVector (TString | TSymbol | TKeyword) ->
      Ok "Cljml.Core_set.String_vector_set"
  | TVector TBool -> Ok "Cljml.Core_set.Bool_vector_set"
  | TVector (TVector TInt) -> Ok "Cljml.Core_set.Int_vector_vector_set"
  | TNamed_record record -> Ok record.set_module_name
  | ty -> Error.error ("sets require a generated comparator for " ^ source_name ty)

let record_fields = function
  | TRecord fields | TNamed_record { fields; _ } -> Some fields
  | _ -> None

let named_record ~type_name ~set_module_name fields =
  TNamed_record { type_name; set_module_name; fields }

let find_field keyword fields =
  List.find_opt (fun field -> field.keyword = keyword) fields

let make_field keyword ty =
  { keyword; ocaml_name = Names.keyword_to_ocaml_name keyword; ty }
