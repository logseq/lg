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

type binding = {
  ocaml_name : string;
  ty : ty;
  protocol_id : Protocol_id.t option;
  row_param_types : string option list;
  host_reference : host_reference option;
  return_param_index : int option;
}

and host_reference =
  | Ocaml_module of string
  | Ocaml_value of string

type typed_expr = {
  ty : ty;
  semantic_expr : Semantic_ir.t;
  record_values : (field * Semantic_ir.t) list option;
  return_param_index : int option;
}

let typed_ir ty semantic_expr =
  {
    ty;
    semantic_expr;
    record_values = None;
    return_param_index = None;
  }

let binding ?(row_param_types = []) ?host_reference ?protocol_id
    ?return_param_index ocaml_name ty =
  {
    ocaml_name;
    ty;
    protocol_id;
    row_param_types;
    host_reference;
    return_param_index;
  }

let rec equal left right =
  match (left, right) with
  | TUnknown, TUnknown -> true
  | TVar left, TVar right -> left = right
  | TInt, TInt
  | TFloat, TFloat
  | TChar, TChar
  | TString, TString
  | TSymbol, TSymbol
  | TKeyword, TKeyword
  | TBool, TBool
  | TUnit, TUnit ->
      true
  | TOcaml left, TOcaml right -> left = right
  | TOcaml_app (left_name, left_args), TOcaml_app (right_name, right_args) ->
      left_name = right_name
      && List.length left_args = List.length right_args
      && List.for_all2 equal left_args right_args
  | TTuple left, TTuple right ->
      List.length left = List.length right && List.for_all2 equal left right
  | TArray left, TArray right | TRef left, TRef right -> equal left right
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
  | TNamed_record left, TNamed_record right ->
      Type_id.equal left.type_id right.type_id
  | _ -> false

let rec row_compatible ~expected ~actual =
  match (expected, actual) with
  | TNamed_record expected, TNamed_record actual
    when expected.nominal || actual.nominal ->
      Type_id.equal expected.type_id actual.type_id
  | (TRecord expected_fields | TNamed_record { fields = expected_fields; _ }),
    (TRecord actual_fields | TNamed_record { fields = actual_fields; _ }) ->
      expected_fields
      |> List.for_all (fun expected_field ->
             match
               List.find_opt
                 (fun actual_field -> actual_field.keyword = expected_field.keyword)
                 actual_fields
             with
             | Some actual_field ->
                 expected_field.ty = TUnknown || actual_field.ty = TUnknown
                 || equal expected_field.ty actual_field.ty
                 || row_compatible ~expected:expected_field.ty
                      ~actual:actual_field.ty
             | None -> false)
  | _ -> false

let same_shape left right =
  equal left right
  || (row_compatible ~expected:left ~actual:right
     && row_compatible ~expected:right ~actual:left)

let host_owned = function
  | TOcaml _ | TOcaml_app _ | TTuple _ | TArray _ | TRef _ -> true
  | _ -> false

let defer_to_ocaml ~expected ~actual = host_owned expected || host_owned actual

type assignability =
  | Equal
  | Unknown
  | Row_compatible
  | Deferred_to_ocaml
  | Incompatible

type assignability_policy = Nominal | Structural | Host_boundary

let classify_assignability ~expected ~actual =
  match (expected, actual) with
  | TUnknown, _ | _, TUnknown -> Unknown
  | _ ->
      if equal expected actual then Equal
      else if row_compatible ~expected ~actual then Row_compatible
      else if defer_to_ocaml ~expected ~actual then Deferred_to_ocaml
      else Incompatible

let assignable ~policy ~expected ~actual =
  match classify_assignability ~expected ~actual with
  | Equal | Unknown -> true
  | Row_compatible -> policy = Structural || policy = Host_boundary
  | Deferred_to_ocaml -> policy = Host_boundary
  | Incompatible -> false

let rec source_name = function
  | TInt -> "int"
  | TFloat -> "float"
  | TChar -> "char"
  | TString -> "string"
  | TSymbol -> "symbol"
  | TKeyword -> "keyword"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TUnknown -> "any"
  | TVar name -> "param/" ^ name
  | TOcaml name -> "ocaml/" ^ name
  | TOcaml_app (name, args) ->
      "ocaml/" ^ name ^ "<"
      ^ (args |> List.map source_name |> String.concat ",")
      ^ ">"
  | TTuple args ->
      "ocaml/tuple<" ^ (args |> List.map source_name |> String.concat ",") ^ ">"
  | TArray inner -> "ocaml/array<" ^ source_name inner ^ ">"
  | TRef inner -> "ocaml/ref<" ^ source_name inner ^ ">"
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
  | TFloat -> "float"
  | TChar -> "char"
  | TString -> "string"
  | TSymbol -> "string"
  | TKeyword -> "string"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TUnknown -> "'a"
  | TVar name -> "'" ^ name
  | TOcaml name -> name
  | TOcaml_app (name, []) -> name
  | TOcaml_app (name, [ arg ]) -> ocaml_name arg ^ " " ^ name
  | TOcaml_app (name, args) ->
      "(" ^ (args |> List.map ocaml_name |> String.concat ", ") ^ ") " ^ name
  | TTuple args -> "(" ^ (args |> List.map ocaml_name |> String.concat " * ") ^ ")"
  | TArray inner -> ocaml_name inner ^ " array"
  | TRef inner -> ocaml_name inner ^ " ref"
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

let type_id_of_name type_name =
  match List.rev (String.split_on_char '.' type_name) with
  | [] -> Type_id.create ~owner:[] ~name:type_name
  | name :: owner -> Type_id.create ~owner:(List.rev owner) ~name

let named_record ?(type_parameters = []) ?type_id ?(nominal = false) ~type_name
    ~set_module_name fields =
  let type_id = Option.value type_id ~default:(type_id_of_name type_name) in
  TNamed_record
    { type_id; nominal; type_name; type_parameters; set_module_name; fields }

let rec qualify_module_type module_path ty =
  let qualify_name name =
    if String.contains name '.' then name else module_path ^ "." ^ name
  in
  match ty with
  | TInt | TFloat | TChar | TString | TSymbol | TKeyword | TBool | TUnit | TUnknown
  | TVar _ | TOcaml _ ->
      ty
  | TOcaml_app (name, args) ->
      TOcaml_app (name, List.map (qualify_module_type module_path) args)
  | TTuple args -> TTuple (List.map (qualify_module_type module_path) args)
  | TArray inner -> TArray (qualify_module_type module_path inner)
  | TRef inner -> TRef (qualify_module_type module_path inner)
  | TList inner -> TList (qualify_module_type module_path inner)
  | TVector inner -> TVector (qualify_module_type module_path inner)
  | TSet inner -> TSet (qualify_module_type module_path inner)
  | TFn (args, ret) ->
      TFn
        ( List.map (qualify_module_type module_path) args,
          qualify_module_type module_path ret )
  | TRecord fields ->
      TRecord
        (List.map
           (fun (field : field) ->
             { field with ty = qualify_module_type module_path field.ty })
           fields)
  | TNamed_record record ->
      let type_name = qualify_name record.type_name in
      TNamed_record
        { type_id = type_id_of_name type_name;
          nominal = record.nominal;
          type_name;
          type_parameters = record.type_parameters;
          set_module_name = qualify_name record.set_module_name;
          fields =
            List.map
              (fun (field : field) ->
                { field with ty = qualify_module_type module_path field.ty })
              record.fields }

let find_field keyword fields =
  List.find_opt (fun field -> field.keyword = keyword) fields

let make_field keyword ty =
  { keyword; ocaml_name = Names.keyword_to_ocaml_name keyword; ty }
