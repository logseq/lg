include Semantic_type

type binding = {
  ocaml_name : string;
  ty : ty;
  scheme : scheme option;
  protocol_id : Protocol_id.t option;
  row_param_types : string option list;
  host_reference : host_reference option;
  return_param_index : int option;
  overload_targets : string list;
  overload_row_param_types : string option list list;
  forward_declared : bool;
  constant_keyword : string option;
  false_non_nil_names : string list;
  dynamically_bindable : bool;
  never_returns : bool;
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
    ?return_param_index ?(overload_targets = [])
    ?(overload_row_param_types = []) ?(forward_declared = false)
    ?constant_keyword ?(false_non_nil_names = []) ?(dynamically_bindable = false)
    ?(never_returns = false)
    ocaml_name ty =
  {
    ocaml_name;
    ty;
    scheme = None;
    protocol_id;
    row_param_types;
    host_reference;
    return_param_index;
    overload_targets;
    overload_row_param_types;
    forward_declared;
    constant_keyword;
    false_non_nil_names;
    dynamically_bindable;
    never_returns;
  }

let generalize_binding (binding : binding) =
  let scheme = Type_solver.generalize binding.ty in
  match scheme.quantified with
  | [] -> binding
  | _ -> { binding with ty = scheme.body; scheme = Some scheme }

let instantiate_binding (binding : binding) =
  match binding.scheme with
  | None -> binding
  | Some scheme -> { binding with ty = Type_solver.instantiate scheme }

let seqable_constraint_name = "__lg_seqable_constraint"
let seqable_constraint element_ty =
  TOcaml_app (seqable_constraint_name, [ element_ty; TUnknown ])

let seqable_constraint_with_value element_ty value_ty =
  TOcaml_app (seqable_constraint_name, [ element_ty; value_ty ])

let contains_constraint_name = "__lg_contains_constraint"
let contains_constraint key_ty =
  TOcaml_app (contains_constraint_name, [ key_ty; TUnknown ])

let contains_constraint_with_value key_ty value_ty =
  TOcaml_app (contains_constraint_name, [ key_ty; value_ty ])

let contains_constraint_info = function
  | TOcaml_app (name, [ key_ty; value_ty ])
    when name = contains_constraint_name ->
      Some (key_ty, value_ty)
  | _ -> None

let optional_seqable_constraint_name = "__lg_optional_seqable_constraint"
let optional_sequential_constraint_name = "__lg_optional_sequential_constraint"

let optional_seqable_constraint element_ty value_ty =
  TOcaml_app (optional_seqable_constraint_name, [ element_ty; value_ty ])

let optional_sequential_constraint element_ty value_ty =
  TOcaml_app (optional_sequential_constraint_name, [ element_ty; value_ty ])

let truthy_constraint_name = "__lg_truthy_constraint"
let truthy_constraint value_ty =
  TOcaml_app (truthy_constraint_name, [ value_ty ])

let truthy_constraint_info = function
  | TOcaml_app (name, [ value_ty ]) when name = truthy_constraint_name ->
      Some value_ty
  | _ -> None

let nil_predicate_constraint_name = "__lg_nil_predicate_constraint"
let nil_predicate_constraint value_ty =
  TOcaml_app (nil_predicate_constraint_name, [ value_ty ])

let nil_predicate_constraint_info = function
  | TOcaml_app (name, [ value_ty ])
    when name = nil_predicate_constraint_name ->
      Some value_ty
  | _ -> None

let printable_constraint_name = "__lg_printable_constraint"
let printable_constraint value_ty =
  TOcaml_app (printable_constraint_name, [ value_ty ])

let printable_constraint_info = function
  | TOcaml_app (name, [ value_ty ]) when name = printable_constraint_name ->
      Some value_ty
  | _ -> None

let symbol_predicate_constraint_name = "__lg_symbol_predicate_constraint"
let symbol_predicate_constraint value_ty =
  TOcaml_app (symbol_predicate_constraint_name, [ value_ty ])

let symbol_predicate_constraint_info = function
  | TOcaml_app (name, [ value_ty ])
    when name = symbol_predicate_constraint_name ->
      Some value_ty
  | _ -> None

let dynamic_constraint_name = "__lg_open_value_constraint"
let dynamic_constraint capability =
  TOcaml_app (dynamic_constraint_name, [ capability ])

let dynamic_constraint_info = function
  | TOcaml_app (name, [ capability ]) when name = dynamic_constraint_name ->
      Some capability
  | _ -> None

let is_dynamic ty = Option.is_some (dynamic_constraint_info ty)

let rec contains_dynamic = function
  | ty when is_dynamic ty -> true
  | TNullable ty | TArray ty | TRef ty | TList ty | TVector ty | TSet ty
  | TSeq ty ->
      contains_dynamic ty
  | TOcaml_app (_, arguments) | TTuple arguments ->
      List.exists contains_dynamic arguments
  | TFn (parameters, return_ty) ->
      List.exists contains_dynamic (return_ty :: parameters)
  | TOverloaded_fn arities ->
      List.exists
        (fun arity ->
          List.exists contains_dynamic (arity.return_ty :: arity.fixed_params)
          || Option.fold ~none:false ~some:contains_dynamic arity.rest_param)
        arities
  | TRecord fields ->
      List.exists (fun (field : field) -> contains_dynamic field.ty) fields
  | TNamed_record record ->
      List.exists contains_dynamic record.type_arguments
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TUnknown | TMeta _ | TVar _ | TOcaml _ ->
      false

let normalize_nullable ty =
  let rec payload = function
    | TNullable inner | TOcaml_app ("option", [ inner ]) -> payload inner
    | inner -> inner
  in
  match ty with
  | TNullable inner | TOcaml_app ("option", [ inner ]) ->
      TNullable (payload inner)
  | ty -> ty

let weak_type_name = "Lg_runtime.Runtime_weak.t"
let weak_type value_ty = TOcaml_app (weak_type_name, [ value_ty ])

let weak_element = function
  | TOcaml_app (name, [ value_ty ]) when name = weak_type_name -> Some value_ty
  | _ -> None

let protocol_constraint_prefix = "__lg_protocol_constraint:"
let guarded_protocol_constraint_prefix = "__lg_guarded_protocol_constraint:"

let protocol_constraint_id name =
  let prefix =
    if String.starts_with ~prefix:protocol_constraint_prefix name then
      Some protocol_constraint_prefix
    else if
      String.starts_with ~prefix:guarded_protocol_constraint_prefix name
    then Some guarded_protocol_constraint_prefix
    else None
  in
  Option.map
    (fun prefix ->
      String.sub name (String.length prefix)
        (String.length name - String.length prefix)
      |> Protocol_id.of_string)
    prefix

let protocol_witness_type method_types =
  List.fold_right (fun method_ty rest -> TTuple [ method_ty; rest ])
    method_types TUnit

let rec protocol_witness_method_types = function
  | TUnit -> Some []
  | TTuple [ method_ty; rest ] ->
      Option.map
        (fun method_types -> method_ty :: method_types)
        (protocol_witness_method_types rest)
  | _ -> None

let protocol_constraint protocol_id method_types value_ty =
  TOcaml_app
    ( protocol_constraint_prefix ^ Protocol_id.to_string protocol_id,
      [ protocol_witness_type method_types; value_ty ] )

let guarded_protocol_constraint constraint_ty =
  match constraint_ty with
  | TOcaml_app (name, arguments) -> (
      match protocol_constraint_id name with
      | Some protocol_id ->
          TOcaml_app
            ( guarded_protocol_constraint_prefix
              ^ Protocol_id.to_string protocol_id,
              arguments )
      | None -> constraint_ty)
  | _ -> constraint_ty

let is_guarded_protocol_constraint = function
  | TOcaml_app (name, _) ->
      String.starts_with ~prefix:guarded_protocol_constraint_prefix name
  | _ -> false

let protocol_constraint_info = function
  | TOcaml_app (name, [ witness_ty; value_ty ]) ->
      Option.map
        (fun protocol_id -> (protocol_id, witness_ty, value_ty))
        (protocol_constraint_id name)
  | _ -> None

let protocol_constraint_with_value constraint_ty value_ty =
  match constraint_ty with
  | TOcaml_app (name, [ witness_ty; _ ])
    when Option.is_some (protocol_constraint_id name) ->
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
      | TOcaml_app (name, [ _key_ty; value_ty ])
        when name = contains_constraint_name ->
          constraint_value_type value_ty
      | TOcaml_app (name, [ value_ty ]) when name = truthy_constraint_name ->
          constraint_value_type value_ty
      | TOcaml_app (name, [ value_ty ])
        when name = nil_predicate_constraint_name ->
          constraint_value_type value_ty
      | TOcaml_app (name, [ value_ty ]) when name = printable_constraint_name ->
          constraint_value_type value_ty
      | TOcaml_app (name, [ value_ty ])
        when name = symbol_predicate_constraint_name ->
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

let record_extension_keyword = ":__lg/extmap"
let record_metadata_key = "\000lg-record-metadata"
let record_extension_type = dynamic_map TKeyword (dynamic_constraint TUnknown)

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
  | TMeta left, TMeta right -> left.id = right.id
  | TVar left, TVar right -> left = right
  | TInt, TOcaml "int" | TOcaml "int", TInt -> true
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
  | expected, actual when equal expected actual -> true
  | TUnknown, _ | _, TUnknown | TMeta _, _ | _, TMeta _ | TVar _, _
  | _, TVar _ ->
      true
  | TNullable expected, TNullable actual
  | TArray expected, TArray actual
  | TRef expected, TRef actual
  | TList expected, TList actual
  | TVector expected, TVector actual
  | TSet expected, TSet actual
  | TSeq expected, TSeq actual ->
      row_compatible ~expected ~actual
  | TOcaml_app (expected_name, expected_args),
    TOcaml_app (actual_name, actual_args)
    when expected_name = actual_name
         && List.length expected_args = List.length actual_args ->
      List.for_all2
        (fun expected actual -> row_compatible ~expected ~actual)
        expected_args actual_args
  | TTuple expected, TTuple actual
    when List.length expected = List.length actual ->
      List.for_all2
        (fun expected actual -> row_compatible ~expected ~actual)
        expected actual
  | TFn (expected_params, expected_return),
    TFn (actual_params, actual_return)
    when List.length expected_params = List.length actual_params ->
      List.for_all2
        (fun expected actual -> row_compatible ~expected ~actual)
        expected_params actual_params
      && row_compatible ~expected:expected_return ~actual:actual_return
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
                 || is_dynamic expected_field.ty
                 || is_dynamic actual_field.ty
                 || equal expected_field.ty actual_field.ty
                 || (match (expected_field.ty, actual_field.ty) with
                    | TRef _, TRef _ -> true
                    | _ -> false)
                 || row_compatible ~expected:expected_field.ty
                      ~actual:actual_field.ty
             | None -> expected_field.keyword = record_extension_keyword)
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
  | TUnknown, _ | _, TUnknown | TMeta _, _ | _, TMeta _ | TVar _, _
  | _, TVar _ ->
      Unknown
  | _ ->
      if equal expected actual then Equal
      else if
        row_compatible ~expected ~actual || row_compatible ~expected:actual ~actual:expected
      then Row_compatible
      else if defer_to_ocaml ~expected ~actual then Deferred_to_ocaml
      else Incompatible

let rec assignable ~policy ~expected ~actual =
  match (expected, actual) with
  | expected, actual when is_dynamic expected <> is_dynamic actual -> false
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
      ||
      (policy = Host_boundary
      && not expected.nominal
      && not actual.nominal)
  | TNamed_record expected, TRecord actual when policy = Host_boundary ->
      let extensible =
        List.exists
          (fun field -> field.keyword = record_extension_keyword)
          expected.fields
      in
      List.for_all
        (fun actual_field ->
          extensible
          || List.exists
               (fun expected_field ->
                 expected_field.keyword = actual_field.keyword)
               expected.fields)
        actual
  | TRecord expected, TNamed_record actual when policy = Host_boundary ->
      let extensible =
        List.exists
          (fun field -> field.keyword = record_extension_keyword)
          actual.fields
      in
      List.for_all
        (fun expected_field ->
          extensible
          || List.exists
               (fun actual_field ->
                 actual_field.keyword = expected_field.keyword)
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
  | TMeta _ -> "inference-variable"
  | TVar name -> "param/" ^ name
  | TOcaml name -> name
  | TOcaml_app (name, [ inner; _ ]) when name = seqable_constraint_name ->
      "seqable<" ^ source_name inner ^ ">"
  | TOcaml_app (name, [ key_ty; _ ]) when name = contains_constraint_name ->
      "contains<" ^ source_name key_ty ^ ">"
  | TOcaml_app (name, [ capability ]) when name = dynamic_constraint_name ->
      "dynamic<" ^ source_name capability ^ ">"
  | TOcaml_app (name, [ value_ty ]) when name = truthy_constraint_name ->
      "truthy<" ^ source_name value_ty ^ ">"
  | TOcaml_app (name, [ value_ty ])
    when name = nil_predicate_constraint_name ->
      "nil-predicate<" ^ source_name value_ty ^ ">"
  | TOcaml_app (name, [ value_ty ]) when name = printable_constraint_name ->
      "printable<" ^ source_name value_ty ^ ">"
  | TOcaml_app (name, [ value_ty ])
    when name = symbol_predicate_constraint_name ->
      "symbol-predicate<" ^ source_name value_ty ^ ">"
  | TOcaml_app (name, [ _witness_ty; value_ty ])
    when Option.is_some (protocol_constraint_id name) ->
      let protocol_name =
        protocol_constraint_id name |> Option.get |> Protocol_id.to_string
      in
      "optional-protocol<" ^ protocol_name ^ ";" ^ source_name value_ty ^ ">"
  | TOcaml_app (name, [ inner ]) when name = weak_type_name ->
      "weak<" ^ source_name inner ^ ">"
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
  | TRecord fields ->
      let field_name (field : field) =
        field.keyword ^ ":" ^ source_name field.ty
      in
      "record<{" ^ String.concat "," (List.map field_name fields) ^ "}>"
  | TNamed_record record when not record.nominal ->
      source_name (TRecord record.fields)
  | TNamed_record record ->
      record.type_name
      ^
      match record.type_arguments with
      | [] -> ""
      | arguments ->
          "<" ^ String.concat "," (List.map source_name arguments) ^ ">"

let ocaml_record_type_name name =
  let local_name separator name =
    match String.rindex_opt name separator with
    | Some index when index < String.length name - 1 ->
        String.sub name (index + 1) (String.length name - index - 1)
    | _ -> name
  in
  if String.contains name ':' || String.contains name '/' then
    name |> local_name ':' |> local_name '/' |> String.uncapitalize_ascii
  else name

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
  | TMeta _ -> "_"
  | TVar name -> "'" ^ name
  | TOcaml name when String.starts_with ~prefix:"__lg_record:" name ->
      let source_name =
        String.sub name (String.length "__lg_record:")
          (String.length name - String.length "__lg_record:")
      in
      (match String.rindex_opt source_name '/' with
      | Some index ->
          let owner = String.sub source_name 0 index in
          let local_name =
            String.sub source_name (index + 1)
              (String.length source_name - index - 1)
          in
          Names.ocaml_binding_name owner local_name
      | None -> Names.sanitize_name source_name)
  | TOcaml name -> name
  | TOcaml_app (name, []) -> name
  | TOcaml_app (name, [ _capability ]) when name = dynamic_constraint_name ->
      "Lg_runtime.Runtime_dynamic.t"
  | TOcaml_app (name, [ value_ty ]) when name = truthy_constraint_name ->
      "((" ^ ocaml_name value_ty ^ " -> bool) * " ^ ocaml_name value_ty ^ ")"
  | TOcaml_app (name, [ value_ty ])
    when name = nil_predicate_constraint_name ->
      "((" ^ ocaml_name value_ty ^ " -> bool) * " ^ ocaml_name value_ty ^ ")"
  | TOcaml_app (name, [ value_ty ]) when name = printable_constraint_name ->
      "((" ^ ocaml_name value_ty ^ " -> string) * " ^ ocaml_name value_ty ^ ")"
  | TOcaml_app (name, [ value_ty ])
    when name = symbol_predicate_constraint_name ->
      "((" ^ ocaml_name value_ty ^ " -> string option) * "
      ^ ocaml_name value_ty ^ ")"
  | TOcaml_app (name, [ inner; container ]) when name = seqable_constraint_name ->
      "((" ^ ocaml_name (constraint_value_type container) ^ " -> "
      ^ ocaml_name inner
      ^ " Seq.t) * " ^ ocaml_name container ^ ")"
  | TOcaml_app (name, [ key_ty; value_ty ])
    when name = contains_constraint_name ->
      "((" ^ ocaml_name key_ty ^ " -> bool) * " ^ ocaml_name value_ty ^ ")"
  | TOcaml_app (name, [ inner; container ])
    when name = optional_seqable_constraint_name
         || name = optional_sequential_constraint_name ->
      "((" ^ ocaml_name (constraint_value_type container) ^ " -> "
      ^ ocaml_name inner ^ " Seq.t) option * " ^ ocaml_name container ^ ")"
  | TOcaml_app (name, [ witness_ty; value_ty ])
    when Option.is_some (protocol_constraint_id name) ->
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
      | Ok "Lg_runtime.Runtime_poly_set" ->
          ocaml_name inner ^ " Lg_runtime.Runtime_poly_set.t"
      | Ok set_module -> set_module ^ ".t"
      | Error _ -> "unsupported_set<" ^ ocaml_name inner ^ ">")
  | TSeq inner -> ocaml_name inner ^ " Seq.t"
  | TFn ([], ret) -> "unit -> " ^ ocaml_name ret
  | TFn (args, ret) ->
      let argument_name = function
        | TFn _ as argument -> "(" ^ ocaml_name argument ^ ")"
        | argument -> ocaml_name argument
      in
      (args |> List.map argument_name |> String.concat " -> ")
      ^ " -> " ^ ocaml_name ret
  | TOverloaded_fn arities -> ocaml_name (overloaded_storage_type arities)
  | TRecord _ -> "record"
  | TNamed_record record -> (
      let type_name = ocaml_record_type_name record.type_name in
      match record.type_arguments with
      | [] -> type_name
      | [ argument ] -> ocaml_type_argument_name argument ^ " " ^ type_name
      | arguments ->
          "("
          ^ String.concat ", " (List.map ocaml_name arguments)
          ^ ") " ^ type_name)

and ocaml_type_argument_name = function
  | TFn _ as ty -> "(" ^ ocaml_name ty ^ ")"
  | ty -> ocaml_name ty

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
  | TUnknown | TMeta _ | TVar _ -> Ok "Lg_runtime.Runtime_poly_set"
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
  | TVector (TUnknown | TMeta _ | TVar _) -> Ok "Lg_runtime.Runtime_poly_set"
  | TVector inner when is_dynamic inner ->
      Ok "Lg_runtime.Core_set.Dynamic_vector_set"
  | TVector (TVector inner) when is_dynamic inner ->
      Ok "Lg_runtime.Core_set.Dynamic_vector_vector_set"
  | TVector (TVector TInt) -> Ok "Lg_runtime.Core_set.Int_vector_vector_set"
  | TVector (TRecord _) -> Ok "Lg_runtime.Runtime_poly_set"
  | TVector (TNamed_record { nominal = false; _ }) ->
      Ok "Lg_runtime.Runtime_poly_set"
  | TVector (TVector (TString | TSymbol | TKeyword)) ->
      Ok "Lg_runtime.Runtime_poly_set"
  | TVector (TVector (TRecord _)) -> Ok "Lg_runtime.Runtime_poly_set"
  | TList (TRecord _) -> Ok "Lg_runtime.Runtime_poly_set"
  | TRecord _ -> Ok "Lg_runtime.Runtime_poly_set"
  | TOcaml "int" -> Ok "Lg_runtime.Core_set.Int_set"
  | TOcaml name when String.starts_with ~prefix:"__lg_record:" name ->
      Ok "Lg_runtime.Runtime_poly_set"
  | TOcaml _ -> Ok "Lg_runtime.Runtime_poly_set"
  | TNamed_record { nominal = false; _ } ->
      Ok "Lg_runtime.Runtime_poly_set"
  | TNullable (TNamed_record record)
  | TOcaml_app ("option", [ TNamed_record record ]) ->
      Ok (record.set_module_name ^ "_nullable")
  | TNullable inner | TOcaml_app ("option", [ inner ]) ->
      Result.map
        (fun _ -> "Lg_runtime.Runtime_poly_set")
        (set_module_name inner)
  | TNamed_record record -> Ok record.set_module_name
  | ty -> Error.error ("sets require a generated comparator for " ^ source_name ty)

let record_fields = function
  | TRecord fields | TNamed_record { fields; _ } -> Some fields
  | _ -> None

let type_id_of_name type_name =
  match List.rev (String.split_on_char '.' type_name) with
  | [] -> Type_id.create ~owner:[] ~name:type_name
  | name :: owner -> Type_id.create ~owner:(List.rev owner) ~name

let named_record ?(type_parameters = []) ?type_id ?(nominal = false)
    ?(extensible = false) ~type_name ~set_module_name fields =
  let type_id = Option.value type_id ~default:(type_id_of_name type_name) in
  TNamed_record
    {
      type_id;
      nominal;
      extensible;
      type_name;
      type_parameters;
      type_arguments = List.map (fun parameter -> TVar parameter) type_parameters;
      set_module_name;
      fields;
    }

let nominal_tag_name (record : named_record) =
  match String.rindex_opt record.type_name '.' with
  | None -> "Lg_nominal_" ^ record.type_name
  | Some separator ->
      let module_path = String.sub record.type_name 0 separator in
      let local_name =
        String.sub record.type_name (separator + 1)
          (String.length record.type_name - separator - 1)
      in
      module_path ^ ".Lg_nominal_" ^ local_name

let rec qualify_module_type module_path ty =
  let qualify_name name =
    if String.contains name '.' then name else module_path ^ "." ^ name
  in
  match ty with
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TUnknown | TMeta _ | TVar _ | TOcaml _ ->
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
          extensible = record.extensible;
          type_name;
          type_parameters = record.type_parameters;
          type_arguments =
            List.map (qualify_module_type module_path) record.type_arguments;
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
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TUnknown | TMeta _ | TVar _ | TOcaml _ ->
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

let rec refresh_named_record (fresh : named_record) ty =
  let refresh = refresh_named_record fresh in
  match ty with
  | TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
  | TBool | TUnit | TNil | TUnknown | TMeta _ | TVar _ | TOcaml _ ->
      ty
  | TNullable inner -> TNullable (refresh inner)
  | TOcaml_app (name, args) -> TOcaml_app (name, List.map refresh args)
  | TTuple args -> TTuple (List.map refresh args)
  | TArray inner -> TArray (refresh inner)
  | TRef inner -> TRef (refresh inner)
  | TList inner -> TList (refresh inner)
  | TVector inner -> TVector (refresh inner)
  | TSet inner -> TSet (refresh inner)
  | TSeq inner -> TSeq (refresh inner)
  | TFn (args, ret) -> TFn (List.map refresh args, refresh ret)
  | TOverloaded_fn arities ->
      TOverloaded_fn
        (List.map
           (fun arity ->
             {
               fixed_params = List.map refresh arity.fixed_params;
               rest_param = Option.map refresh arity.rest_param;
               return_ty = refresh arity.return_ty;
             })
           arities)
  | TRecord fields ->
      TRecord
        (List.map
           (fun (field : field) -> { field with ty = refresh field.ty })
           fields)
  | TNamed_record record when Type_id.equal record.type_id fresh.type_id ->
      if List.length record.type_arguments = List.length fresh.type_arguments
      then TNamed_record { fresh with type_arguments = record.type_arguments }
      else TNamed_record fresh
  | TNamed_record record ->
      TNamed_record
        {
          record with
          type_arguments = List.map refresh record.type_arguments;
          fields =
            List.map
              (fun (field : field) -> { field with ty = refresh field.ty })
              record.fields;
        }

let find_field keyword fields =
  List.find_opt (fun field -> field.keyword = keyword) fields
let make_field ?location ?(mutable_ = false) keyword ty =
  {
    keyword;
    ocaml_name = Names.keyword_to_ocaml_name keyword;
    ty;
    mutable_;
    location;
  }

let make_record_extension_field ?(ty = record_extension_type) () =
  make_field record_extension_keyword ty

let is_record_extension_field field =
  field.keyword = record_extension_keyword

let is_static_record_source_field field =
  is_record_extension_field field
  && Option.is_none (dynamic_map_types field.ty)

let find_record_extension_field fields =
  List.find_opt is_record_extension_field fields

let record_constructor_fields fields =
  List.filter (fun field -> not (is_record_extension_field field)) fields

type type_substitutions = Type_solver.substitutions

let infer_type_substitutions substitutions ~template ~actual =
  Type_solver.infer substitutions ~template ~actual
  |> Result.value ~default:substitutions

let infer_list_substitutions substitutions templates actuals =
  Type_solver.infer_all substitutions ~templates ~actuals
  |> Result.value ~default:substitutions

let substitute_type_variables = Type_solver.apply

let instantiate_type ~templates ~actuals ty =
  if List.length templates <> List.length actuals then ty
  else
    let substitutions =
      infer_list_substitutions [] templates actuals
    in
    substitute_type_variables substitutions ty

let instantiate_type_fields ~templates ~actuals ty =
  if List.length templates <> List.length actuals then ty
  else
    let substitutions =
      List.fold_left2
        (fun substitutions template actual ->
          infer_type_substitutions substitutions ~template ~actual)
        [] templates actuals
    in
    let instantiated = substitute_type_variables substitutions ty in
    let rec refine_open_type template actual =
      match (template, actual) with
      | (TUnknown | TMeta _ | TVar _), actual -> actual
      | TNullable template, TNullable actual ->
          TNullable (refine_open_type template actual)
      | TNullable template, TOcaml_app ("option", [ actual ]) ->
          TNullable (refine_open_type template actual)
      | TOcaml_app ("option", [ template ]), TNullable actual
      | TOcaml_app ("option", [ template ]),
        TOcaml_app ("option", [ actual ]) ->
          TOcaml_app ("option", [ refine_open_type template actual ])
      | TArray template, TArray actual ->
          TArray (refine_open_type template actual)
      | TRef template, TRef actual -> TRef (refine_open_type template actual)
      | TList template, TList actual ->
          TList (refine_open_type template actual)
      | TVector template, TVector actual ->
          TVector (refine_open_type template actual)
      | TSet template, TSet actual -> TSet (refine_open_type template actual)
      | TSeq template, TSeq actual -> TSeq (refine_open_type template actual)
      | TOcaml_app (name, templates), TOcaml_app (actual_name, actuals)
        when name = actual_name && List.length templates = List.length actuals ->
          TOcaml_app (name, List.map2 refine_open_type templates actuals)
      | template, _ -> template
    in
    match instantiated with
    | TNamed_record record ->
        let rec refine_fields refined templates actuals = function
          | [] -> List.rev refined
          | field :: fields when is_record_extension_field field ->
              refine_fields (field :: refined) templates actuals fields
          | (field : field) :: fields -> (
              match (templates, actuals) with
              | template :: templates, actual :: actuals ->
                  let template = substitute_type_variables substitutions template in
                  let actual = substitute_type_variables substitutions actual in
                  let field =
                    {
                      field with
                      ty = refine_open_type template actual;
                    }
                  in
                  refine_fields (field :: refined) templates actuals fields
              | [], [] -> List.rev_append refined (field :: fields)
              | _ -> List.rev_append refined (field :: fields))
        in
        TNamed_record
          {
            record with
            fields = refine_fields [] templates actuals record.fields;
          }
    | ty -> ty

let instantiate_receiver_method_type receiver_ty method_ty =
  let rec specialize_return value_ty = function
    | TSeq (TUnknown | TMeta _ | TVar _) -> TSeq value_ty
    | TOcaml_app (("Seq.t" | "Seq") as name, [ TUnknown | TMeta _ | TVar _ ]) ->
        TOcaml_app (name, [ value_ty ])
    | TNullable return_ty ->
        TNullable (specialize_return value_ty return_ty)
    | TOcaml_app ("option", [ return_ty ]) ->
        TOcaml_app ("option", [ specialize_return value_ty return_ty ])
    | return_ty -> return_ty
  in
  match method_ty with
  | TFn (template_receiver :: _, _) ->
      let receiver_value_ty =
        match template_receiver with
        | TNamed_record { type_parameters = [ parameter ]; _ } ->
            let substitutions =
              infer_type_substitutions [] ~template:template_receiver
                ~actual:receiver_ty
            in
            (match
               List.assoc_opt (Type_solver.Declared parameter) substitutions
             with
            | Some TUnknown | None -> None
            | Some ty -> Some ty)
        | template_receiver -> (
            match protocol_constraint_info template_receiver with
            | Some (_, _, TVar parameter) ->
                Some
                  (substitute_type_variables
                     [ (Type_solver.Declared parameter, receiver_ty) ]
                     (TVar parameter))
            | Some _ | None -> None)
      in
      let method_ty =
        instantiate_type ~templates:[ template_receiver ]
          ~actuals:[ receiver_ty ] method_ty
      in
      (match (receiver_value_ty, method_ty) with
      | Some value_ty, TFn (receiver :: parameters, return_ty) ->
          TFn
            ( receiver
              :: List.map
                   (function TUnknown -> value_ty | ty -> ty)
                   parameters,
              specialize_return value_ty return_ty )
      | _ -> method_ty)
  | _ -> method_ty
let rec idents_in_conversion names = function
  | Semantic_ir.Ident name -> name :: names
  | Semantic_ir.Typed (_, value)
  | Semantic_ir.Located (_, _, value)
  | Semantic_ir.SharedValue (_, value) ->
      idents_in_conversion names value
  | Semantic_ir.Constructor (_, value) ->
      Option.fold ~none:names ~some:(idents_in_conversion names) value
  | Semantic_ir.Tuple values
  | Semantic_ir.List values
  | Semantic_ir.Array values
  | Semantic_ir.Sequence values ->
      List.fold_left idents_in_conversion names values
  | Semantic_ir.Apply (fn, args) | Semantic_ir.Uncurried_apply (fn, args) ->
      List.fold_left idents_in_conversion (idents_in_conversion names fn) args
  | Semantic_ir.Labelled_apply (fn, args) ->
      List.fold_left
        (fun names (_, value) -> idents_in_conversion names value)
        (idents_in_conversion names fn) args
  | Semantic_ir.If (condition, then_expr, else_expr) ->
      List.fold_left idents_in_conversion names
        [ condition; then_expr; else_expr ]
  | Semantic_ir.Fun (_, body) -> idents_in_conversion names body
  | Semantic_ir.Let (bindings, body) ->
      List.fold_left
        (fun names (_, value) -> idents_in_conversion names value)
        (idents_in_conversion names body) bindings
  | Semantic_ir.EvaluateOnce (_, value, body) ->
      idents_in_conversion
        (idents_in_conversion (idents_in_conversion names value) body)
        value
  | Semantic_ir.LetRec (_, _, body, args) ->
      List.fold_left idents_in_conversion (idents_in_conversion names body) args
  | Semantic_ir.LetRecIn (_, _, body, next) ->
      idents_in_conversion (idents_in_conversion names body) next
  | Semantic_ir.Match (target, cases) ->
      List.fold_left
        (fun names (_, value) -> idents_in_conversion names value)
        (idents_in_conversion names target)
        cases
  | Semantic_ir.Match_guarded (target, cases) ->
      List.fold_left
        (fun names (_, _, value) -> idents_in_conversion names value)
        (idents_in_conversion names target)
        cases
  | Semantic_ir.Try (body, cases) ->
      List.fold_left
        (fun names (_, _, handler) -> idents_in_conversion names handler)
        (idents_in_conversion names body) cases
  | Semantic_ir.Infix (_, left, right) | Semantic_ir.Cons (left, right) ->
      idents_in_conversion (idents_in_conversion names left) right
  | Semantic_ir.Prefix (_, value)
  | Semantic_ir.Constraint (value, _)
  | Semantic_ir.Field (value, _) ->
      idents_in_conversion names value
  | Semantic_ir.SetField (target, _, value) ->
      idents_in_conversion (idents_in_conversion names target) value
  | Semantic_ir.PackDynamic { conversion; _ }
  | Semantic_ir.UnpackDynamic { conversion; _ }
  | Semantic_ir.NullableToSeq { conversion; _ } ->
      idents_in_conversion names conversion
  | Semantic_ir.Record (fields, _) ->
      List.fold_left
        (fun names (_, value) -> idents_in_conversion names value)
        names fields
  | Semantic_ir.RecordUpdate (record, fields) ->
      List.fold_left
        (fun names (_, value) -> idents_in_conversion names value)
        (idents_in_conversion names record) fields
  | Semantic_ir.Int _ | Semantic_ir.Int64 _ | Semantic_ir.Float _ | Semantic_ir.String _
  | Semantic_ir.Char _ | Semantic_ir.Bool _ | Semantic_ir.Unit ->
      names

let rec dynamic_pinned_idents names = function
  | Semantic_ir.PackDynamic { conversion; _ } ->
      let names = idents_in_conversion names conversion in
      dynamic_pinned_idents names conversion
  | Semantic_ir.Typed (_, value)
  | Semantic_ir.Located (_, _, value)
  | Semantic_ir.SharedValue (_, value) ->
      dynamic_pinned_idents names value
  | Semantic_ir.Constructor (_, value) ->
      Option.fold ~none:names ~some:(dynamic_pinned_idents names) value
  | Semantic_ir.Tuple values
  | Semantic_ir.List values
  | Semantic_ir.Array values
  | Semantic_ir.Sequence values ->
      List.fold_left dynamic_pinned_idents names values
  | Semantic_ir.Apply (fn, args) | Semantic_ir.Uncurried_apply (fn, args) ->
      List.fold_left dynamic_pinned_idents
        (dynamic_pinned_idents names fn)
        args
  | Semantic_ir.Labelled_apply (fn, args) ->
      List.fold_left
        (fun names (_, value) -> dynamic_pinned_idents names value)
        (dynamic_pinned_idents names fn)
        args
  | Semantic_ir.If (condition, then_expr, else_expr) ->
      List.fold_left dynamic_pinned_idents names
        [ condition; then_expr; else_expr ]
  | Semantic_ir.Fun (_, body) -> dynamic_pinned_idents names body
  | Semantic_ir.Let (bindings, body) ->
      List.fold_left
        (fun names (_, value) -> dynamic_pinned_idents names value)
        (dynamic_pinned_idents names body)
        bindings
  | Semantic_ir.EvaluateOnce (_, value, body) ->
      dynamic_pinned_idents
        (dynamic_pinned_idents (dynamic_pinned_idents names value) body)
        value
  | Semantic_ir.LetRec (_, _, body, args) ->
      List.fold_left dynamic_pinned_idents
        (dynamic_pinned_idents names body)
        args
  | Semantic_ir.LetRecIn (_, _, body, next) ->
      dynamic_pinned_idents (dynamic_pinned_idents names body) next
  | Semantic_ir.Match (target, cases) ->
      List.fold_left
        (fun names (_, value) -> dynamic_pinned_idents names value)
        (dynamic_pinned_idents names target)
        cases
  | Semantic_ir.Match_guarded (target, cases) ->
      List.fold_left
        (fun names (_, _, value) -> dynamic_pinned_idents names value)
        (dynamic_pinned_idents names target)
        cases
  | Semantic_ir.Try (body, cases) ->
      List.fold_left
        (fun names (_, _, handler) -> dynamic_pinned_idents names handler)
        (dynamic_pinned_idents names body)
        cases
  | Semantic_ir.Infix (_, left, right) | Semantic_ir.Cons (left, right) ->
      dynamic_pinned_idents (dynamic_pinned_idents names left) right
  | Semantic_ir.Prefix (_, value)
  | Semantic_ir.Constraint (value, _)
  | Semantic_ir.Field (value, _) ->
      dynamic_pinned_idents names value
  | Semantic_ir.SetField (target, _, value) ->
      dynamic_pinned_idents (dynamic_pinned_idents names target) value
  | Semantic_ir.UnpackDynamic { conversion; _ }
  | Semantic_ir.NullableToSeq { conversion; _ } ->
      dynamic_pinned_idents names conversion
  | Semantic_ir.Record (fields, _) ->
      List.fold_left
        (fun names (_, value) -> dynamic_pinned_idents names value)
        names fields
  | Semantic_ir.RecordUpdate (record, fields) ->
      List.fold_left
        (fun names (_, value) -> dynamic_pinned_idents names value)
        (dynamic_pinned_idents names record) fields
  | Semantic_ir.Int _ | Semantic_ir.Int64 _ | Semantic_ir.Float _ | Semantic_ir.String _
  | Semantic_ir.Char _ | Semantic_ir.Bool _ | Semantic_ir.Unit
  | Semantic_ir.Ident _ ->
      names

let rec pattern_name = function
  | Semantic_ir.PVar name -> Some name
  | Semantic_ir.PConstraint (pattern, _) -> pattern_name pattern
  | Semantic_ir.PTyped (pattern, _) -> pattern_name pattern
  | Semantic_ir.PLocated (_, _, pattern) -> pattern_name pattern
  | Semantic_ir.PAlias (pattern, name) -> (
      match pattern_name pattern with Some _ as name -> name | None -> Some name)
  | Semantic_ir.PTuple patterns -> (
      match List.rev patterns with
      | pattern :: _ -> pattern_name pattern
      | [] -> None)
  | Semantic_ir.PAny | Semantic_ir.PUnit | Semantic_ir.PInt _ | Semantic_ir.PInt64 _
  | Semantic_ir.PString _ | Semantic_ir.PBool _ | Semantic_ir.PConstructor _
  | Semantic_ir.PList _ | Semantic_ir.PCons _ | Semantic_ir.PRecord _
  | Semantic_ir.POr _ ->
      None

let fn_param_names expression =
  let rec strip expression =
    match expression with
    | Semantic_ir.Typed (_, value) | Semantic_ir.Located (_, _, value) ->
        strip value
    | Semantic_ir.Fun (patterns, _) ->
        List.map pattern_name patterns
    | _ -> []
  in
  strip expression

let align_deferred_param_types value_type expression =
  let pinned = dynamic_pinned_idents [] expression in
  match (pinned, value_type) with
  | [], _ | _, TFn ([], _) -> value_type
  | pinned, TFn (parameters, return_ty) ->
      let names = fn_param_names expression in
      TFn
        ( List.mapi
            (fun index ty ->
              match (List.nth_opt names index, ty) with
              | Some (Some name), (TUnknown | TMeta _ | TVar _)
                when List.mem name pinned ->
                  dynamic_constraint TUnknown
              | _ -> ty)
            parameters,
          return_ty )
  | _, value_type -> value_type
