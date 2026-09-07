type metavariable = { id : int; location : Location.t option }

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
  | TConstraint of constraint_
  | TTuple of ty list
  | TArray of ty
  | TRef of ty
  | TList of ty
  | TVector of ty
  | TSet of ty
  | TSeq of ty
  | TFn of ty list * ty
  | TOverloaded_fn of fn_arity list
  | TPoly_variant of variant_row
  | TRecord of field list
  | TNamed_record of named_record

and row_bound = Exact_row | Lower_row | Upper_row | Bounded_row of string list
and variant_row = { tags : (string * ty option) list; bound : row_bound }
and seqable_requirement = Required | Optional | Optional_sequential

and constraint_ =
  | Seqable_constraint of {
      requirement : seqable_requirement;
      element : ty;
      storage : ty;
    }
  | Contains_constraint of { key : ty; storage : ty }
  | Truthy_constraint of ty
  | Nil_predicate_constraint of ty
  | Printable_constraint of ty
  | Exception_data_constraint of ty
  | Hashable_constraint of ty
  | Comparable_constraint of ty
  | Array_index_constraint of ty
  | Symbol_predicate_constraint of ty
  | Open_boundary_constraint of ty
  | Protocol_constraint of {
      protocol_id : Protocol_id.t;
      witness : ty;
      value : ty;
      guarded : bool;
    }

and field = {
  keyword : string;
  ocaml_name : string;
  ty : ty;
  quantified : string list;
  mutable_ : bool;
  runtime_map : bool;
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
  | Inferred_variable of { metavariable_id : int; name : string }

type scheme = { quantified : scheme_variable list; body : ty }

let constraint_children = function
  | Seqable_constraint { element; storage; _ } -> [ element; storage ]
  | Contains_constraint { key; storage } -> [ key; storage ]
  | Truthy_constraint value
  | Nil_predicate_constraint value
  | Printable_constraint value
  | Exception_data_constraint value
  | Hashable_constraint value
  | Comparable_constraint value
  | Array_index_constraint value
  | Symbol_predicate_constraint value
  | Open_boundary_constraint value ->
      [ value ]
  | Protocol_constraint { witness; value; _ } -> [ witness; value ]

let map_constraint map constraint_ =
  let map_one build value =
    let mapped = map value in
    if mapped == value then constraint_ else build mapped
  in
  let map_two build left right =
    let mapped_left = map left in
    let mapped_right = map right in
    if mapped_left == left && mapped_right == right then constraint_
    else build mapped_left mapped_right
  in
  match constraint_ with
  | Seqable_constraint ({ element; storage; _ } as seqable) ->
      map_two
        (fun element storage ->
          Seqable_constraint { seqable with element; storage })
        element storage
  | Contains_constraint { key; storage } ->
      map_two
        (fun key storage -> Contains_constraint { key; storage })
        key storage
  | Truthy_constraint value ->
      map_one (fun value -> Truthy_constraint value) value
  | Nil_predicate_constraint value ->
      map_one (fun value -> Nil_predicate_constraint value) value
  | Printable_constraint value ->
      map_one (fun value -> Printable_constraint value) value
  | Exception_data_constraint value ->
      map_one (fun value -> Exception_data_constraint value) value
  | Hashable_constraint value ->
      map_one (fun value -> Hashable_constraint value) value
  | Comparable_constraint value ->
      map_one (fun value -> Comparable_constraint value) value
  | Array_index_constraint value ->
      map_one (fun value -> Array_index_constraint value) value
  | Symbol_predicate_constraint value ->
      map_one (fun value -> Symbol_predicate_constraint value) value
  | Open_boundary_constraint value ->
      map_one (fun value -> Open_boundary_constraint value) value
  | Protocol_constraint ({ witness; value; _ } as protocol) ->
      map_two
        (fun witness value ->
          Protocol_constraint { protocol with witness; value })
        witness value

let map_children map = function
  | TPoly_variant row ->
      TPoly_variant
        {
          row with
          tags =
            List.map
              (fun (tag, payload) -> (tag, Option.map map payload))
              row.tags;
        }
  | TNullable ty -> TNullable (map ty)
  | TOcaml_app (name, arguments) -> TOcaml_app (name, List.map map arguments)
  | TConstraint constraint_ -> TConstraint (map_constraint map constraint_)
  | TTuple types -> TTuple (List.map map types)
  | TArray ty -> TArray (map ty)
  | TRef ty -> TRef (map ty)
  | TList ty -> TList (map ty)
  | TVector ty -> TVector (map ty)
  | TSet ty -> TSet (map ty)
  | TSeq ty -> TSeq (map ty)
  | TFn (parameters, result) -> TFn (List.map map parameters, map result)
  | TOverloaded_fn arities ->
      TOverloaded_fn
        (List.map
           (fun arity ->
             {
               fixed_params = List.map map arity.fixed_params;
               rest_param = Option.map map arity.rest_param;
               return_ty = map arity.return_ty;
             })
           arities)
  | TRecord fields ->
      TRecord (List.map (fun field -> { field with ty = map field.ty }) fields)
  | TNamed_record record ->
      TNamed_record
        {
          record with
          type_arguments = List.map map record.type_arguments;
          fields =
            List.map
              (fun field -> { field with ty = map field.ty })
              record.fields;
        }
  | ( TInt | TFloat | TChar | TString | TRegex | TMap_keys | TSymbol | TKeyword
    | TBool | TUnit | TNil | TUnknown | TMeta _ | TVar _ | TOcaml _ ) as ty ->
      ty
