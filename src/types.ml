include Semantic_type

type binding = {
  ocaml_name : string;
  ty : ty;
  protocol_id : Protocol_id.t option;
  row_param_types : string option list;
  host_reference : host_reference option;
  return_param_index : int option;
  overload_targets : string list;
  forward_declared : bool;
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
    semantic_expr = Semantic_ir.annotate ty semantic_expr;
    record_values = None;
    return_param_index = None;
  }

let binding ?(row_param_types = []) ?host_reference ?protocol_id
    ?return_param_index ?(overload_targets = []) ?(forward_declared = false)
    ocaml_name ty =
  {
    ocaml_name;
    ty;
    protocol_id;
    row_param_types;
    host_reference;
    return_param_index;
    overload_targets;
    forward_declared;
  }

let seqable_constraint_name = "__lg_seqable_constraint"
let seqable_constraint element_ty =
  TOcaml_app (seqable_constraint_name, [ element_ty; TUnknown ])

let seqable_constraint_with_value element_ty value_ty =
  TOcaml_app (seqable_constraint_name, [ element_ty; value_ty ])

let optional_seqable_constraint_name = "__lg_optional_seqable_constraint"
let optional_sequential_constraint_name = "__lg_optional_sequential_constraint"

let optional_seqable_constraint element_ty value_ty =
  TOcaml_app (optional_seqable_constraint_name, [ element_ty; value_ty ])

let optional_sequential_constraint element_ty value_ty =
  TOcaml_app (optional_sequential_constraint_name, [ element_ty; value_ty ])

let dynamic_constraint_name = "__lg_dynamic_constraint"
let dynamic_constraint capability =
  TOcaml_app (dynamic_constraint_name, [ capability ])

let dynamic_constraint_info = function
  | TOcaml_app (name, [ capability ]) when name = dynamic_constraint_name ->
      Some capability
  | _ -> None

let is_dynamic ty = Option.is_some (dynamic_constraint_info ty)

let protocol_constraint_prefix = "__lg_protocol_constraint:"

let protocol_witness_type method_types =
  List.fold_right (fun method_ty rest -> TTuple [ method_ty; rest ])
    method_types TUnit

let protocol_constraint protocol_id method_types value_ty =
  TOcaml_app
    ( protocol_constraint_prefix ^ Protocol_id.to_string protocol_id,
      [ protocol_witness_type method_types; value_ty ] )

let protocol_constraint_info = function
  | TOcaml_app (name, [ witness_ty; value_ty ])
    when String.starts_with ~prefix:protocol_constraint_prefix name ->
      let id_text =
        String.sub name (String.length protocol_constraint_prefix)
          (String.length name - String.length protocol_constraint_prefix)
      in
      Some (Protocol_id.of_string id_text, witness_ty, value_ty)
  | _ -> None

let protocol_constraint_with_value constraint_ty value_ty =
  match constraint_ty with
  | TOcaml_app (name, [ witness_ty; _ ])
    when String.starts_with ~prefix:protocol_constraint_prefix name ->
      TOcaml_app (name, [ witness_ty; value_ty ])
  | ty -> ty

let rec seqable_constraint_element = function
  | TOcaml_app (name, [ element_ty; _container_ty ])
    when name = seqable_constraint_name
         || name = optional_seqable_constraint_name
         || name = optional_sequential_constraint_name ->
      Some element_ty
  | ty -> (
      match protocol_constraint_info ty with
      | Some (_, _, value_ty) -> seqable_constraint_element value_ty
      | None -> None)

let rec seqable_constraint_info = function
  | TOcaml_app (name, [ element_ty; value_ty ])
    when name = seqable_constraint_name ->
      Some (`Required, element_ty, value_ty)
  | TOcaml_app (name, [ element_ty; value_ty ])
    when name = optional_seqable_constraint_name ->
      Some (`Optional, element_ty, value_ty)
  | TOcaml_app (name, [ element_ty; value_ty ])
    when name = optional_sequential_constraint_name ->
      Some (`Optional_sequential, element_ty, value_ty)
  | ty -> (
      match protocol_constraint_info ty with
      | Some (_, _, value_ty) -> seqable_constraint_info value_ty
      | None -> None)

let rec constraint_value_type ty =
  match dynamic_constraint_info ty with
  | Some _ -> ty
  | None ->
  match protocol_constraint_info ty with
  | Some (_, _, value_ty) -> constraint_value_type value_ty
  | None -> (
      match ty with
      | TOcaml_app (name, [ _element_ty; value_ty ])
        when name = seqable_constraint_name
             || name = optional_seqable_constraint_name
             || name = optional_sequential_constraint_name ->
          constraint_value_type value_ty
      | value_ty -> value_ty)

let protocol_witness_name value_name protocol_id =
  value_name ^ "__protocol_"
  ^ String.sub (Digest.to_hex (Digest.string (Protocol_id.to_string protocol_id)))
      0 12

let next_seq_type_name = "__lg_next_seq"
let next_seq inner = TOcaml_app (next_seq_type_name, [ inner ])

let next_seq_element = function
  | TOcaml_app (name, [ inner ]) when name = next_seq_type_name -> Some inner
  | _ -> None

let reduced_type_name = "Lg_runtime.Runtime_reduced.t"
let reduced inner = TOcaml_app (reduced_type_name, [ inner ])

let maybe_reduced_callback_type_name = "__lg_maybe_reduced_callback_result"

let maybe_reduced_callback_result inner =
  TOcaml_app (maybe_reduced_callback_type_name, [ inner ])

let dynamic_map key value =
  TOcaml_app ("Lg_runtime.Runtime_map.t", [ key; value ])

let dynamic_map_types = function
  | TOcaml_app ("Lg_runtime.Runtime_map.t", [ key; value ]) ->
      Some (key, value)
  | _ -> None

let reduced_element = function
  | TOcaml_app (name, [ inner ]) when name = reduced_type_name -> Some inner
  | _ -> None

let maybe_reduced_callback_element = function
  | TOcaml_app (name, [ inner ])
    when name = maybe_reduced_callback_type_name ->
      Some inner
  | _ -> None

let rec equal left right =
  match (left, right) with
  | TUnknown, TUnknown -> true
  | TVar left, TVar right -> left = right
  | TInt, TInt
  | TFloat, TFloat
  | TChar, TChar
  | TString, TString
  | TRegex, TRegex
  | TMap_keys, TMap_keys
  | TSymbol, TSymbol
  | TKeyword, TKeyword
  | TBool, TBool
  | TUnit, TUnit
  | TNil, TNil ->
      true
  | TNullable left, TNullable right -> equal left right
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
  | TSeq left, TSeq right -> equal left right
  | TFn (left_args, left_ret), TFn (right_args, right_ret) ->
      List.length left_args = List.length right_args
      && List.for_all2 equal left_args right_args
      && equal left_ret right_ret
  | TOverloaded_fn left, TOverloaded_fn right ->
      List.length left = List.length right
      && List.for_all2 equal_fn_arity left right
  | TRecord left, TRecord right ->
      List.length left = List.length right
      && List.for_all2
           (fun l r -> l.keyword = r.keyword && equal l.ty r.ty)
           left right
  | TNamed_record left, TNamed_record right ->
      Type_id.equal left.type_id right.type_id
  | _ -> false

and equal_fn_arity left right =
  List.length left.fixed_params = List.length right.fixed_params
  && List.for_all2 equal left.fixed_params right.fixed_params
  && Option.equal equal left.rest_param right.rest_param
  && equal left.return_ty right.return_ty

let is_numeric = function TInt | TFloat -> true | _ -> false

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
                 || (match (expected_field.ty, actual_field.ty) with
                    | TRef _, TRef _ -> true
                    | _ -> false)
                 || row_compatible ~expected:expected_field.ty
                      ~actual:actual_field.ty
             | None -> false)
  | TMap_keys, (TRecord _ | TNamed_record _) -> true
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
  | TUnknown, _ | _, TUnknown | TVar _, _ | _, TVar _ -> Unknown
  | _ ->
      if equal expected actual then Equal
      else if
        row_compatible ~expected ~actual || row_compatible ~expected:actual ~actual:expected
      then Row_compatible
      else if defer_to_ocaml ~expected ~actual then Deferred_to_ocaml
      else Incompatible

let rec assignable ~policy ~expected ~actual =
  match (expected, actual) with
  | TNullable _, TNil -> true
  | TNullable expected, TNullable actual ->
      assignable ~policy ~expected ~actual
  | TNullable expected, actual ->
      assignable ~policy ~expected ~actual
  | TVector expected, TVector actual
  | TList expected, TList actual
  | TSet expected, TSet actual
  | TSeq expected, TSeq actual ->
      assignable ~policy ~expected ~actual
  | TSeq expected, TOcaml_app (name, [ actual ])
  | TOcaml_app (name, [ expected ]), TSeq actual
    when name = next_seq_type_name ->
      assignable ~policy ~expected ~actual
  | TNamed_record expected, TNamed_record actual
    when expected.type_name = actual.type_name ->
      equal (TNamed_record expected) (TNamed_record actual)
      || policy = Host_boundary
  | TNamed_record expected, TRecord actual when policy = Host_boundary ->
      List.for_all
        (fun actual_field ->
          List.exists
            (fun expected_field -> expected_field.keyword = actual_field.keyword)
            expected.fields)
        actual
  | TRecord expected, TNamed_record actual when policy = Host_boundary ->
      List.for_all
        (fun expected_field ->
          List.exists
            (fun actual_field -> actual_field.keyword = expected_field.keyword)
            actual.fields)
        expected
  | TFn (expected_params, expected_return), TFn (actual_params, actual_return)
    when List.length expected_params = List.length actual_params ->
      List.for_all2
        (fun expected actual -> assignable ~policy ~expected ~actual)
        expected_params actual_params
      && assignable ~policy ~expected:expected_return ~actual:actual_return
  | _ -> (
      match classify_assignability ~expected ~actual with
      | Equal -> true
      | Unknown -> policy = Host_boundary
      | Row_compatible -> policy = Structural || policy = Host_boundary
      | Deferred_to_ocaml -> policy = Host_boundary
      | Incompatible -> false)

let rec source_name = function
  | TInt -> "int"
  | TFloat -> "float"
  | TChar -> "char"
  | TString -> "string"
  | TRegex -> "regex"
  | TMap_keys -> "map"
  | TSymbol -> "symbol"
  | TKeyword -> "keyword"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TNil -> "nil"
  | TNullable inner -> "nullable<" ^ source_name inner ^ ">"
  | TUnknown -> "any"
  | TVar name -> "param/" ^ name
  | TOcaml name -> name
  | TOcaml_app (name, [ inner; _ ]) when name = seqable_constraint_name ->
      "seqable<" ^ source_name inner ^ ">"
  | TOcaml_app (name, [ capability ]) when name = dynamic_constraint_name ->
      "dynamic<" ^ source_name capability ^ ">"
  | TOcaml_app (name, [ _witness_ty; value_ty ])
    when String.starts_with ~prefix:protocol_constraint_prefix name ->
      let protocol_name =
        String.sub name (String.length protocol_constraint_prefix)
          (String.length name - String.length protocol_constraint_prefix)
      in
      "optional-protocol<" ^ protocol_name ^ ";" ^ source_name value_ty ^ ">"
  | TOcaml_app (name, [ inner ]) when name = next_seq_type_name ->
      "seq<" ^ source_name inner ^ ">"
  | TOcaml_app (name, [ inner ]) when name = reduced_type_name ->
      "reduced<" ^ source_name inner ^ ">"
  | TOcaml_app (name, args) ->
      name ^ "<"
      ^ (args |> List.map source_name |> String.concat ",")
      ^ ">"
  | TTuple args ->
      "tuple<" ^ (args |> List.map source_name |> String.concat ",") ^ ">"
  | TArray inner -> "array<" ^ source_name inner ^ ">"
  | TRef inner -> "ref<" ^ source_name inner ^ ">"
  | TList ty -> "list<" ^ source_name ty ^ ">"
  | TVector ty -> "vector<" ^ source_name ty ^ ">"
  | TSet ty -> "set<" ^ source_name ty ^ ">"
  | TSeq ty -> "seq<" ^ source_name ty ^ ">"
  | TFn (args, ret) ->
      "fn<(" ^ (args |> List.map source_name |> String.concat ", ") ^ ") -> "
      ^ source_name ret ^ ">"
  | TOverloaded_fn arities ->
      "fn<"
      ^ (arities
        |> List.map (fun arity ->
               let fixed = List.map source_name arity.fixed_params in
               let params =
                 match arity.rest_param with
                 | None -> fixed
                 | Some rest -> fixed @ [ "& " ^ source_name rest ]
               in
               "(" ^ String.concat ", " params ^ ") -> "
               ^ source_name arity.return_ty)
        |> String.concat "; ")
      ^ ">"
  | TRecord _ -> "map"
  | TNamed_record _ -> "map"

let rec ocaml_name = function
  | TInt -> "int"
  | TFloat -> "float"
  | TChar -> "char"
  | TString -> "string"
  | TRegex -> "string"
  | TMap_keys -> "string Lg_runtime.Core_set.String_set.t"
  | TSymbol -> "string"
  | TKeyword -> "string"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TNil -> "'a option"
  | TNullable inner -> ocaml_name inner ^ " option"
  | TUnknown -> "'a"
  | TVar name -> "'" ^ name
  | TOcaml name -> name
  | TOcaml_app (name, []) -> name
  | TOcaml_app (name, [ _capability ]) when name = dynamic_constraint_name ->
      "Lg_runtime.Runtime_dynamic.t"
  | TOcaml_app (name, [ inner; container ]) when name = seqable_constraint_name ->
      "((" ^ ocaml_name (constraint_value_type container) ^ " -> "
      ^ ocaml_name inner
      ^ " Seq.t) * " ^ ocaml_name container ^ ")"
  | TOcaml_app (name, [ inner; container ])
    when name = optional_seqable_constraint_name
         || name = optional_sequential_constraint_name ->
      "((" ^ ocaml_name (constraint_value_type container) ^ " -> "
      ^ ocaml_name inner ^ " Seq.t) option * " ^ ocaml_name container ^ ")"
  | TOcaml_app (name, [ witness_ty; value_ty ])
    when String.starts_with ~prefix:protocol_constraint_prefix name ->
      "(" ^ ocaml_name witness_ty ^ " option * " ^ ocaml_name value_ty ^ ")"
  | TOcaml_app (name, [ inner ]) when name = next_seq_type_name ->
      ocaml_name inner ^ " Seq.t"
  | TOcaml_app (name, [ arg ]) -> ocaml_name arg ^ " " ^ name
  | TOcaml_app (name, args) ->
      "(" ^ (args |> List.map ocaml_name |> String.concat ", ") ^ ") " ^ name
  | TTuple args ->
      let tuple_item ty =
        match ty with
        | TFn _ -> "(" ^ ocaml_name ty ^ ")"
        | _ -> ocaml_name ty
      in
      "(" ^ (args |> List.map tuple_item |> String.concat " * ") ^ ")"
  | TArray inner -> ocaml_name inner ^ " array"
  | TRef inner -> ocaml_name inner ^ " ref"
  | TList inner -> ocaml_name inner ^ " list"
  | TVector inner -> ocaml_name inner ^ " Rrbvec.t"
  | TSet inner -> (
      match set_module_name inner with
      | Ok set_module -> set_module ^ ".t"
      | Error _ -> "unsupported_set<" ^ ocaml_name inner ^ ">")
  | TSeq inner -> ocaml_name inner ^ " Seq.t"
  | TFn (args, ret) ->
      (args |> List.map ocaml_name |> String.concat " -> ") ^ " -> " ^ ocaml_name ret
  | TOverloaded_fn arities -> ocaml_name (overloaded_storage_type arities)
  | TRecord _ -> "record"
  | TNamed_record record -> (
      match record.type_parameters with
      | [] -> record.type_name
      | [ parameter ] -> "'" ^ parameter ^ " " ^ record.type_name
      | parameters ->
          "("
          ^ String.concat ", " (List.map (fun parameter -> "'" ^ parameter) parameters)
          ^ ") " ^ record.type_name)

and overloaded_storage_type = function
  | [] -> TUnit
  | arity :: rest ->
      let params =
        match arity.rest_param with
        | None -> arity.fixed_params
        | Some rest_ty -> arity.fixed_params @ [ TSeq rest_ty ]
      in
      TTuple [ TFn (params, arity.return_ty); overloaded_storage_type rest ]

and set_module_name = function
  | TUnknown | TVar _ -> Ok "Lg_runtime.Runtime_poly_set"
  | TInt -> Ok "Lg_runtime.Core_set.Int_set"
  | TFloat -> Ok "Lg_runtime.Core_set.Float_set"
  | TString | TSymbol | TKeyword -> Ok "Lg_runtime.Core_set.String_set"
  | TBool -> Ok "Lg_runtime.Core_set.Bool_set"
  | TList TInt -> Ok "Lg_runtime.Core_set.Int_list_set"
  | TList TFloat -> Ok "Lg_runtime.Core_set.Float_list_set"
  | TList (TString | TSymbol | TKeyword) -> Ok "Lg_runtime.Core_set.String_list_set"
  | TList TBool -> Ok "Lg_runtime.Core_set.Bool_list_set"
  | TVector TInt -> Ok "Lg_runtime.Core_set.Int_vector_set"
  | TVector TFloat -> Ok "Lg_runtime.Core_set.Float_vector_set"
  | TVector (TString | TSymbol | TKeyword) ->
      Ok "Lg_runtime.Core_set.String_vector_set"
  | TVector TBool -> Ok "Lg_runtime.Core_set.Bool_vector_set"
  | TVector (TVector TInt) -> Ok "Lg_runtime.Core_set.Int_vector_vector_set"
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
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword | TBool | TUnit | TNil | TUnknown
  | TVar _ | TOcaml _ ->
      ty
  | TNullable inner -> TNullable (qualify_module_type module_path inner)
  | TOcaml_app (name, args) ->
      TOcaml_app (name, List.map (qualify_module_type module_path) args)
  | TTuple args -> TTuple (List.map (qualify_module_type module_path) args)
  | TArray inner -> TArray (qualify_module_type module_path inner)
  | TRef inner -> TRef (qualify_module_type module_path inner)
  | TList inner -> TList (qualify_module_type module_path inner)
  | TVector inner -> TVector (qualify_module_type module_path inner)
  | TSet inner -> TSet (qualify_module_type module_path inner)
  | TSeq inner -> TSeq (qualify_module_type module_path inner)
  | TFn (args, ret) ->
      TFn
        ( List.map (qualify_module_type module_path) args,
          qualify_module_type module_path ret )
  | TOverloaded_fn arities ->
      TOverloaded_fn
        (List.map
           (fun arity ->
             { fixed_params =
                 List.map (qualify_module_type module_path) arity.fixed_params;
               rest_param = Option.map (qualify_module_type module_path) arity.rest_param;
               return_ty = qualify_module_type module_path arity.return_ty })
           arities)
  | TRecord fields ->
      TRecord
        (List.map
           (fun (field : field) ->
             { field with ty = qualify_module_type module_path field.ty })
           fields)
  | TNamed_record record ->
      let type_name = qualify_name record.type_name in
      TNamed_record
        { type_id = record.type_id;
          nominal = record.nominal;
          type_name;
          type_parameters = record.type_parameters;
          set_module_name = qualify_name record.set_module_name;
          fields =
            List.map
              (fun (field : field) ->
                { field with ty = qualify_module_type module_path field.ty })
              record.fields }

let rec remap_module_type ~from_path ~to_path ty =
  let remap_name name =
    if name = from_path then to_path
    else
      let prefix = from_path ^ "." in
      if String.starts_with ~prefix name then
        to_path ^ String.sub name (String.length from_path)
          (String.length name - String.length from_path)
      else name
  in
  let remap_type_id type_id =
    match Type_id.owner type_id with
    | [ owner ] ->
        Type_id.create ~owner:[ remap_name owner ] ~name:(Type_id.name type_id)
    | _ -> type_id
  in
  match ty with
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword | TBool | TUnit | TNil | TUnknown
  | TVar _ | TOcaml _ ->
      ty
  | TNullable inner ->
      TNullable (remap_module_type ~from_path ~to_path inner)
  | TOcaml_app (name, args) ->
      TOcaml_app (name, List.map (remap_module_type ~from_path ~to_path) args)
  | TTuple args -> TTuple (List.map (remap_module_type ~from_path ~to_path) args)
  | TArray inner -> TArray (remap_module_type ~from_path ~to_path inner)
  | TRef inner -> TRef (remap_module_type ~from_path ~to_path inner)
  | TList inner -> TList (remap_module_type ~from_path ~to_path inner)
  | TVector inner -> TVector (remap_module_type ~from_path ~to_path inner)
  | TSet inner -> TSet (remap_module_type ~from_path ~to_path inner)
  | TSeq inner -> TSeq (remap_module_type ~from_path ~to_path inner)
  | TFn (args, ret) ->
      TFn
        ( List.map (remap_module_type ~from_path ~to_path) args,
          remap_module_type ~from_path ~to_path ret )
  | TOverloaded_fn arities ->
      TOverloaded_fn
        (List.map
           (fun arity ->
             { fixed_params =
                 List.map (remap_module_type ~from_path ~to_path) arity.fixed_params;
               rest_param =
                 Option.map (remap_module_type ~from_path ~to_path) arity.rest_param;
               return_ty =
                 remap_module_type ~from_path ~to_path arity.return_ty })
           arities)
  | TRecord fields ->
      TRecord
        (List.map
           (fun (field : field) ->
             { field with ty = remap_module_type ~from_path ~to_path field.ty })
           fields)
  | TNamed_record record ->
      TNamed_record
        { record with
          type_id = remap_type_id record.type_id;
          type_name = remap_name record.type_name;
          set_module_name = remap_name record.set_module_name;
          fields =
            List.map
              (fun (field : field) ->
                { field with
                  ty = remap_module_type ~from_path ~to_path field.ty;
                })
              record.fields;
        }

let find_field keyword fields =
  List.find_opt (fun field -> field.keyword = keyword) fields

let make_field ?location keyword ty =
  { keyword; ocaml_name = Names.keyword_to_ocaml_name keyword; ty; location }

type type_substitutions = (string * ty) list

let bind_type_variable substitutions name actual =
  match List.assoc_opt name substitutions with
  | None -> (name, actual) :: substitutions
  | Some existing when equal existing actual -> substitutions
  | Some _ ->
      (name, TUnknown) :: List.remove_assoc name substitutions

let rec infer_type_substitutions substitutions ~template ~actual =
  match (template, actual) with
  | TVar name, actual -> bind_type_variable substitutions name actual
  | TOcaml_app (template_name, template_args),
    TOcaml_app (actual_name, actual_args)
    when template_name = actual_name
         && List.length template_args = List.length actual_args ->
      infer_list_substitutions substitutions template_args actual_args
  | TTuple template_args, TTuple actual_args
    when List.length template_args = List.length actual_args ->
      infer_list_substitutions substitutions template_args actual_args
  | (TArray template, TArray actual)
  | (TRef template, TRef actual)
  | (TList template, TList actual)
  | (TVector template, TVector actual)
  | (TSet template, TSet actual)
  | (TSeq template, TSeq actual) ->
      infer_type_substitutions substitutions ~template ~actual
  | TFn (template_args, template_ret), TFn (actual_args, actual_ret)
    when List.length template_args = List.length actual_args ->
      let substitutions =
        infer_list_substitutions substitutions template_args actual_args
      in
      infer_type_substitutions substitutions ~template:template_ret
        ~actual:actual_ret
  | TOverloaded_fn templates, TOverloaded_fn actuals
    when List.length templates = List.length actuals ->
      List.fold_left2 infer_arity_substitutions substitutions templates actuals
  | _ -> substitutions

and infer_arity_substitutions substitutions template actual =
  let substitutions =
    if List.length template.fixed_params = List.length actual.fixed_params then
      infer_list_substitutions substitutions template.fixed_params actual.fixed_params
    else substitutions
  in
  let substitutions =
    match (template.rest_param, actual.rest_param) with
    | Some template, Some actual ->
        infer_type_substitutions substitutions ~template ~actual
    | _ -> substitutions
  in
  infer_type_substitutions substitutions ~template:template.return_ty
    ~actual:actual.return_ty

and infer_list_substitutions substitutions templates actuals =
  List.fold_left2
    (fun substitutions template actual ->
      infer_type_substitutions substitutions ~template ~actual)
    substitutions templates actuals

let rec substitute_type_variables substitutions = function
  | TVar name ->
      List.assoc_opt name substitutions |> Option.value ~default:(TVar name)
  | TOcaml_app (name, args) ->
      TOcaml_app (name, List.map (substitute_type_variables substitutions) args)
  | TNullable inner ->
      TNullable (substitute_type_variables substitutions inner)
  | TTuple args -> TTuple (List.map (substitute_type_variables substitutions) args)
  | TArray inner -> TArray (substitute_type_variables substitutions inner)
  | TRef inner -> TRef (substitute_type_variables substitutions inner)
  | TList inner -> TList (substitute_type_variables substitutions inner)
  | TVector inner -> TVector (substitute_type_variables substitutions inner)
  | TSet inner -> TSet (substitute_type_variables substitutions inner)
  | TSeq inner -> TSeq (substitute_type_variables substitutions inner)
  | TFn (args, ret) ->
      TFn
        ( List.map (substitute_type_variables substitutions) args,
          substitute_type_variables substitutions ret )
  | TOverloaded_fn arities ->
      TOverloaded_fn
        (List.map
           (fun arity ->
             { fixed_params =
                 List.map (substitute_type_variables substitutions)
                   arity.fixed_params;
               rest_param =
                 Option.map (substitute_type_variables substitutions)
                   arity.rest_param;
               return_ty =
                 substitute_type_variables substitutions arity.return_ty })
           arities)
  | TRecord fields ->
      TRecord
        (List.map
           (fun (field : field) ->
             { field with ty = substitute_type_variables substitutions field.ty })
           fields)
  | TNamed_record record ->
      TNamed_record
        { record with
          fields =
            List.map
              (fun (field : field) ->
                { field with
                  ty = substitute_type_variables substitutions field.ty;
                })
              record.fields;
        }
  | (TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword | TBool | TUnit | TNil
    | TUnknown | TOcaml _) as ty ->
      ty

let instantiate_type ~templates ~actuals ty =
  if List.length templates <> List.length actuals then ty
  else
    let substitutions =
      infer_list_substitutions [] templates actuals
    in
    substitute_type_variables substitutions ty
