type t = Lg_edn_backend.t
type format = Current | Legacy

type schema =
  (string, (string, Data_value.t) Lg_runtime.Lg_map.t) Lg_runtime.Lg_map.t
  option

let int value = Lg_edn_backend.Small_int value

let int_value = function
  | Lg_edn_backend.Small_int value -> value
  | Lg_edn_backend.Int value -> Int64.to_int value
  | _ -> invalid_arg "expected serialized int"
let string value = Lg_edn_backend.String value
let string_value = function Lg_edn_backend.String value -> value | _ -> invalid_arg "expected serialized string"
let vector values = Lg_edn_backend.Vector (Array.of_list values)

let vector_values = function
  | Lg_edn_backend.Vector values -> Array.to_list values
  | Lg_edn_backend.Int4_vector (first, second, third, fourth) ->
      [
        int first;
        int second;
        third;
        int fourth;
      ]
  | Lg_edn_backend.Int_vector values ->
      values |> Array.to_list |> List.map int
  | _ -> invalid_arg "expected serialized vector"

let rrbvec_of_values = function
  | Lg_edn_backend.Vector values -> Rrbvec.of_array values
  | _ -> invalid_arg "expected serialized vector"

let edn_keyword value =
  if String.starts_with ~prefix:":" value then
    String.sub value 1 (String.length value - 1)
  else value

let data_keyword value =
  if String.starts_with ~prefix:":" value then value else ":" ^ value

let rec data_value_to_edn = function
  | Data_value.Nil -> Lg_edn_backend.Nil
  | Data_value.Int value -> Lg_edn_backend.Small_int value
  | Data_value.Wide_int value -> Lg_edn_backend.Int value
  | Data_value.Float value -> Lg_edn_backend.Float value
  | Data_value.String value -> string value
  | Data_value.Symbol value -> Lg_edn_backend.Symbol value
  | Data_value.Bool value -> Lg_edn_backend.Bool value
  | Data_value.Keyword value -> Lg_edn_backend.Keyword (edn_keyword value)
  | Data_value.Uuid value ->
      Lg_edn_backend.Tagged ("uuid", string value)
  | Data_value.Instant value ->
      Lg_edn_backend.Tagged ("inst-ms", int value)
  | Data_value.Regex value -> Lg_edn_backend.Regex value
  | Data_value.Ref value ->
      Lg_edn_backend.Tagged ("datascript/ref", int value)
  | Data_value.List values ->
      Lg_edn_backend.List (Array.of_list (List.map data_value_to_edn values))
  | Data_value.Vector values ->
      Lg_edn_backend.Vector (Array.of_list (List.map data_value_to_edn values))
  | Data_value.Map entries | Data_value.Hash_map entries ->
      Lg_edn_backend.Map
        (Array.of_list
           (List.map
              (fun (key, value) ->
                (data_value_to_edn key, data_value_to_edn value))
              entries))
  | Data_value.Set values ->
      Lg_edn_backend.Set (Array.of_list (List.map data_value_to_edn values))
  | Data_value.Tuple values ->
      Lg_edn_backend.Vector
        (Array.of_list
           (List.map
              (function
                | None -> Lg_edn_backend.Nil
                | Some value -> data_value_to_edn value)
              values))
  | Data_value.Runtime_type _ ->
      invalid_arg "Runtime type values cannot be serialized"
  | Data_value.Tx_ref ->
      Lg_edn_backend.Tagged ("datascript/tx-ref", Lg_edn_backend.Nil)
  | Data_value.Ref_to entity_ref ->
      Lg_edn_backend.Tagged
        ("datascript/entity-ref", entity_ref_to_edn entity_ref)

and entity_ref_to_edn = function
  | Data_value.Entity_id value -> vector [ string "entity"; int value ]
  | Data_value.Temp_id value -> vector [ string "temp"; string value ]
  | Data_value.Auto_tempid value -> vector [ string "auto-temp"; int value ]
  | Data_value.Current_tx -> vector [ string "current-tx" ]
  | Data_value.Ident value -> vector [ string "ident"; string value ]
  | Data_value.Lookup_ref (attr, value) ->
      vector [ string "lookup"; string attr; data_value_to_edn value ]

let rec data_value_of_edn = function
  | Lg_edn_backend.Nil -> Data_value.Nil
  | Lg_edn_backend.Small_int value -> Data_value.Int value
  | Lg_edn_backend.Int value ->
      let narrowed = Int64.to_int value in
      if Int64.equal (Int64.of_int narrowed) value then Data_value.Int narrowed
      else Data_value.Wide_int value
  | Lg_edn_backend.Float value -> Data_value.Float value
  | Lg_edn_backend.String value -> Data_value.String value
  | Lg_edn_backend.Symbol value -> Data_value.Symbol value
  | Lg_edn_backend.Bool value -> Data_value.Bool value
  | Lg_edn_backend.Keyword value -> Data_value.Keyword (data_keyword value)
  | Lg_edn_backend.Regex value -> Data_value.Regex value
  | Lg_edn_backend.List values ->
      Data_value.List (Array.to_list (Array.map data_value_of_edn values))
  | Lg_edn_backend.Vector values ->
      Data_value.Vector (Array.to_list (Array.map data_value_of_edn values))
  | Lg_edn_backend.Int4_vector (first, second, third, fourth) ->
      Data_value.Vector
        [
          Data_value.Int first;
          Data_value.Int second;
          data_value_of_edn third;
          Data_value.Int fourth;
        ]
  | Lg_edn_backend.Int_vector values ->
      Data_value.Vector
        (values |> Array.to_list |> List.map (fun value -> Data_value.Int value))
  | Lg_edn_backend.Map entries ->
      Data_value.Map
        (Array.to_list
           (Array.map
              (fun (key, value) ->
                (data_value_of_edn key, data_value_of_edn value))
              entries))
  | Lg_edn_backend.Set values ->
      Data_value.Set (Array.to_list (Array.map data_value_of_edn values))
  | Lg_edn_backend.Tagged ("uuid", value) ->
      Data_value.Uuid (string_value value)
  | Lg_edn_backend.Tagged ("inst-ms", value) ->
      Data_value.Instant (int_value value)
  | Lg_edn_backend.Tagged ("datascript/ref", value) ->
      Data_value.Ref (int_value value)
  | Lg_edn_backend.Tagged ("datascript/tx-ref", _) -> Data_value.Tx_ref
  | Lg_edn_backend.Tagged ("datascript/entity-ref", value) ->
      Data_value.Ref_to (entity_ref_of_edn value)
  | Lg_edn_backend.Char _ | Lg_edn_backend.Bigint _
  | Lg_edn_backend.Decimal _ | Lg_edn_backend.Ratio _
  | Lg_edn_backend.Tagged _ | Lg_edn_backend.Json_source _ ->
      invalid_arg "unsupported DataScript value in serialized schema"

and entity_ref_of_edn value =
  match vector_values value with
  | [ kind; value ] when string_value kind = "entity" ->
      Data_value.Entity_id (int_value value)
  | [ kind; value ] when string_value kind = "temp" ->
      Data_value.Temp_id (string_value value)
  | [ kind; value ] when string_value kind = "auto-temp" ->
      Data_value.Auto_tempid (int_value value)
  | [ kind ] when string_value kind = "current-tx" -> Data_value.Current_tx
  | [ kind; value ] when string_value kind = "ident" ->
      Data_value.Ident (string_value value)
  | [ kind; attr; value ] when string_value kind = "lookup" ->
      Data_value.Lookup_ref (string_value attr, data_value_of_edn value)
  | _ -> invalid_arg "invalid serialized entity reference"

let data_value_of_edn_string source =
  source |> Lg_edn_backend.of_edn_string |> data_value_of_edn

let keyword_reference index = vector [ int 0; int index ]

let encode_non_keyword_with freeze value =
  match value with
  | Data_value.String value -> string value
  | Data_value.Int value -> Lg_edn_backend.Small_int value
  | Data_value.Wide_int value -> Lg_edn_backend.Int value
  | Data_value.Float value when Float.is_finite value ->
      Lg_edn_backend.Float value
  | Data_value.Float value when Float.is_nan value -> vector [ int 4 ]
  | Data_value.Float value when value > 0. -> vector [ int 2 ]
  | Data_value.Float _ -> vector [ int 3 ]
  | Data_value.Bool value -> Lg_edn_backend.Bool value
  | Data_value.Keyword _ ->
      invalid_arg "keywords require an indexed serialization reference"
  | value -> vector [ int 1; freeze (data_value_to_edn value) ]

let freeze_edn value =
  Lg_edn_backend.String (Lg_edn_backend.to_edn_string value)

let encode_non_keyword value = encode_non_keyword_with freeze_edn value

let decode_value_with thaw keywords value =
  match value with
  | Lg_edn_backend.String value -> Data_value.String value
  | Lg_edn_backend.Small_int value -> Data_value.Int value
  | Lg_edn_backend.Int value ->
      let narrowed = Int64.to_int value in
      if Int64.equal (Int64.of_int narrowed) value then Data_value.Int narrowed
      else Data_value.Wide_int value
  | Lg_edn_backend.Float value -> Data_value.Float value
  | Lg_edn_backend.Bool value -> Data_value.Bool value
  | Lg_edn_backend.Vector marker -> (
      match Array.to_list marker with
      | [ marker; index ] when int_value marker = 0 ->
          Data_value.Keyword (Rrbvec.nth keywords (int_value index))
      | [ marker; value ] when int_value marker = 1 ->
          data_value_of_edn (thaw value)
      | [ marker ] when int_value marker = 2 -> Data_value.Float infinity
      | [ marker ] when int_value marker = 3 ->
          Data_value.Float neg_infinity
      | [ marker ] when int_value marker = 4 -> Data_value.Float nan
      | _ -> invalid_arg "invalid serialized DataScript value marker")
  | _ -> invalid_arg "invalid serialized DataScript value"

let thaw_edn = function
  | Lg_edn_backend.String source -> Lg_edn_backend.of_edn_string source
  | _ -> invalid_arg "default serialized value must be an EDN string"

let decode_value keywords value = decode_value_with thaw_edn keywords value

type encoder = {
  keyword_indexes : (string, int) Hashtbl.t;
  mutable reversed_keywords : string list;
  mutable keyword_count : int;
}

let create_encoder () =
  {
    keyword_indexes = Hashtbl.create 16;
    reversed_keywords = [];
    keyword_count = 0;
  }

let encode_value_with encoder freeze = function
  | Data_value.Keyword keyword ->
      let index =
        match Hashtbl.find_opt encoder.keyword_indexes keyword with
        | Some index -> index
        | None ->
            let index = encoder.keyword_count in
            Hashtbl.add encoder.keyword_indexes keyword index;
            encoder.reversed_keywords <- keyword :: encoder.reversed_keywords;
            encoder.keyword_count <- index + 1;
            index
      in
      keyword_reference index
  | value -> encode_non_keyword_with freeze value

let encode_value encoder value = encode_value_with encoder freeze_edn value

let encoder_keywords encoder =
  encoder.reversed_keywords |> List.rev |> Rrbvec.of_list

let attribute_index attrs target =
  let rec find index =
    if index >= Rrbvec.length attrs then
      invalid_arg ("serialized attribute is not indexed: " ^ target)
    else if String.equal (Rrbvec.nth attrs index) target then index
    else find (index + 1)
  in
  find 0

module Attribute_indexes = Hashtbl.Make (struct
  type t = string

  let equal = String.equal
  let hash value =
    value
    |> Lg_runtime.Runtime_hash.clojure_string_hash
    |> Int32.to_int
end)

type attribute_indexes = int Attribute_indexes.t

let create_attribute_indexes attributes =
  let indexes = Attribute_indexes.create (Rrbvec.length attributes) in
  Rrbvec.iteri
    (fun index attribute -> Attribute_indexes.add indexes attribute index)
    attributes;
  indexes

let find_attribute_index indexes target =
  Attribute_indexes.find_opt indexes target |> Option.value ~default:(-1)

let datom entity attribute value tx =
  Lg_edn_backend.Int4_vector (entity, attribute, value, tx)

let datom_field index = function
  | Lg_edn_backend.Int4_vector (entity, attribute, value, tx) -> (
      match index with
      | 0 -> int entity
      | 1 -> int attribute
      | 2 -> value
      | 3 -> int tx
      | _ -> invalid_arg "serialized datom field is out of bounds")
  | Lg_edn_backend.Vector fields when Array.length fields = 4 ->
      fields.(index)
  | _ -> invalid_arg "invalid serialized datom"

let datom_entity = function
  | Lg_edn_backend.Int4_vector (entity, _, _, _) -> entity
  | value -> int_value (datom_field 0 value)

let datom_attribute = function
  | Lg_edn_backend.Int4_vector (_, attribute, _, _) -> attribute
  | value -> int_value (datom_field 1 value)

let datom_value = function
  | Lg_edn_backend.Int4_vector (_, _, value, _) -> value
  | value -> datom_field 2 value

let datom_tx = function
  | Lg_edn_backend.Int4_vector (_, _, _, tx) -> tx
  | value -> int_value (datom_field 3 value)

let string_vector values =
  Lg_edn_backend.Vector
    (Array.map string (Rrbvec.to_array values))

let optional_int_array = function
  | None -> Lg_edn_backend.Nil
  | Some values -> Lg_edn_backend.Int_vector values

let database_arrays_with_schema count tx0 max_eid max_tx schema attrs keywords
    datoms aevt avet branching_factor ref_type =
  let ref_type =
    match ref_type with Storage_value.Strong -> "strong" | Storage_value.Weak -> "weak"
  in
  let field name value = (string name, value) in
  Lg_edn_backend.Map
    [|
      field "count" (int count);
      field "tx0" (int tx0);
      field "max-eid" (int max_eid);
      field "max-tx" (int max_tx);
      field "schema" schema;
      field "attrs" (string_vector attrs);
      field "keywords" (string_vector keywords);
      field "eavt" (Lg_edn_backend.Vector datoms);
      field "aevt" (optional_int_array aevt);
      field "avet" (optional_int_array avet);
      field "branching-factor" (int branching_factor);
      field "ref-type" (string ref_type);
    |]

let database_with_schema count tx0 max_eid max_tx schema attrs keywords datoms
    aevt avet branching_factor ref_type =
  database_arrays_with_schema count tx0 max_eid max_tx schema attrs keywords
    (Rrbvec.to_array datoms)
    (Option.map Rrbvec.to_array aevt)
    (Option.map Rrbvec.to_array avet)
    branching_factor ref_type

let database_arrays count tx0 max_eid max_tx schema attrs keywords datoms aevt
    avet branching_factor ref_type =
  database_arrays_with_schema count tx0 max_eid max_tx (string schema) attrs
    keywords datoms aevt avet branching_factor ref_type

let database count tx0 max_eid max_tx schema attrs keywords datoms aevt avet
    branching_factor ref_type =
  database_with_schema count tx0 max_eid max_tx (string schema) attrs keywords
    datoms aevt avet branching_factor ref_type

let field value name =
  match value with
  | Lg_edn_backend.Map fields ->
      (match
         Array.find_map
           (fun (key, value) ->
             match key with
             | Lg_edn_backend.String key when String.equal key name ->
                 Some value
             | _ -> None)
           fields
       with
      | Some value -> value
      | None -> invalid_arg ("missing serialized field " ^ name))
  | _ -> invalid_arg "expected serialized database map"

let field_opt value name =
  match value with
  | Lg_edn_backend.Map fields ->
      Array.find_map
        (fun (key, value) ->
          match key with
          | Lg_edn_backend.String key when String.equal key name -> Some value
          | _ -> None)
        fields
  | _ -> invalid_arg "expected serialized database map"

let format value =
  match
    (field_opt value "branching-factor", field_opt value "ref-type")
  with
  | Some _, Some _ -> Current
  | None, None -> Legacy
  | Some _, None | None, Some _ ->
      invalid_arg "serialized database has incomplete settings"

let as_legacy = function
  | Lg_edn_backend.Map fields ->
      Lg_edn_backend.Map
        (Array.of_list
           (fields |> Array.to_list
           |> List.filter (fun (key, _) ->
                  match key with
                  | Lg_edn_backend.String
                      ("branching-factor" | "ref-type") ->
                      false
                  | _ -> true)))
  | _ -> invalid_arg "expected serialized database map"

let count value = field value "count" |> int_value
let tx0 value = field value "tx0" |> int_value
let max_eid value = field value "max-eid" |> int_value
let max_tx value = field value "max-tx" |> int_value
let schema_source value = field value "schema" |> string_value
let schema_value value = field value "schema"

let string_vector_value = function
  | Lg_edn_backend.Vector values ->
      Rrbvec.of_array (Array.map string_value values)
  | _ -> invalid_arg "expected serialized string vector"

let int_vector_value = function
  | Lg_edn_backend.Int_vector values -> Rrbvec.of_array values
  | Lg_edn_backend.Vector values ->
      Rrbvec.of_array (Array.map int_value values)
  | _ -> invalid_arg "expected serialized int vector"

let vector_array = function
  | Lg_edn_backend.Vector values -> values
  | _ -> invalid_arg "expected serialized vector"

let int_array_value value =
  match value with
  | Lg_edn_backend.Int_vector values -> values
  | _ -> Array.map int_value (vector_array value)

let optional_int_vector_value = function
  | Lg_edn_backend.Nil -> None
  | value -> Some (int_vector_value value)

let optional_int_array_value = function
  | Lg_edn_backend.Nil -> None
  | value -> Some (int_array_value value)

let attrs value = field value "attrs" |> string_vector_value
let keywords value = field value "keywords" |> string_vector_value
let datoms value = field value "eavt" |> rrbvec_of_values
let aevt value = field value "aevt" |> optional_int_vector_value
let avet value = field value "avet" |> optional_int_vector_value
let datoms_array value = field value "eavt" |> vector_array
let aevt_array value = field value "aevt" |> optional_int_array_value
let avet_array value = field value "avet" |> optional_int_array_value
let branching_factor value =
  match format value with
  | Current -> field value "branching-factor" |> int_value
  | Legacy -> 32

let ref_type value =
  match format value with
  | Legacy -> Storage_value.Strong
  | Current -> (
      match field value "ref-type" |> string_value with
      | "strong" -> Storage_value.Strong
      | "weak" -> Storage_value.Weak
      | value -> invalid_arg ("unsupported reference type " ^ value))

type prepared_datoms =
  | Prepared_edn_datoms of t array
  | Prepared_json_datoms of Lg_edn_backend.json array

type prepared_value =
  | Prepared_edn_value of t
  | Prepared_json_value of Lg_edn_backend.json

type prepared_datom = {
  prepared_entity : int;
  prepared_attribute : int;
  prepared_value : prepared_value;
  prepared_tx : int;
}

type prepared = {
  prepared_database_count : int;
  prepared_database_tx0 : int;
  prepared_database_max_eid : int;
  prepared_database_max_tx : int;
  prepared_database_schema : t;
  prepared_database_attrs : string Rrbvec.t;
  prepared_database_keywords : string Rrbvec.t;
  prepared_database_datoms : prepared_datoms;
  prepared_database_aevt : int array option;
  prepared_database_avet : int array option;
  prepared_database_branching_factor : int;
  prepared_database_ref_type : Storage_value.ref_type;
}

let prepare_edn value =
  {
    prepared_database_count = count value;
    prepared_database_tx0 = tx0 value;
    prepared_database_max_eid = max_eid value;
    prepared_database_max_tx = max_tx value;
    prepared_database_schema = schema_value value;
    prepared_database_attrs = attrs value;
    prepared_database_keywords = keywords value;
    prepared_database_datoms = Prepared_edn_datoms (datoms_array value);
    prepared_database_aevt = aevt_array value;
    prepared_database_avet = avet_array value;
    prepared_database_branching_factor = branching_factor value;
    prepared_database_ref_type = ref_type value;
  }

let json_string_vector json =
  json |> Lg_edn_backend.json_array
  |> Array.map Lg_edn_backend.json_string
  |> Rrbvec.of_array

let json_optional_int_array json =
  if Lg_edn_backend.json_is_null json then None
  else
    Some
      (json |> Lg_edn_backend.json_array
      |> Array.map Lg_edn_backend.json_int)

let json_ref_type json =
  match Lg_edn_backend.json_string json with
  | "strong" -> Storage_value.Strong
  | "weak" -> Storage_value.Weak
  | value -> invalid_arg ("unsupported reference type " ^ value)

let prepare_json source =
  let json = Lg_edn_backend.json_of_string source in
  let setting name = Lg_edn_backend.json_field_opt json name in
  let branching_factor, ref_type =
    match (setting "branching-factor", setting "ref-type") with
    | Some branching_factor, Some ref_type ->
        ( Lg_edn_backend.json_int branching_factor,
          json_ref_type ref_type )
    | None, None -> (32, Storage_value.Strong)
    | Some _, None | None, Some _ ->
        invalid_arg "serialized database has incomplete settings"
  in
  {
    prepared_database_count =
      Lg_edn_backend.json_field json "count" |> Lg_edn_backend.json_int;
    prepared_database_tx0 =
      Lg_edn_backend.json_field json "tx0" |> Lg_edn_backend.json_int;
    prepared_database_max_eid =
      Lg_edn_backend.json_field json "max-eid" |> Lg_edn_backend.json_int;
    prepared_database_max_tx =
      Lg_edn_backend.json_field json "max-tx" |> Lg_edn_backend.json_int;
    prepared_database_schema =
      Lg_edn_backend.json_field json "schema"
      |> Lg_edn_backend.json_to_edn;
    prepared_database_attrs =
      Lg_edn_backend.json_field json "attrs" |> json_string_vector;
    prepared_database_keywords =
      Lg_edn_backend.json_field json "keywords" |> json_string_vector;
    prepared_database_datoms =
      Prepared_json_datoms
        (Lg_edn_backend.json_field json "eavt"
        |> Lg_edn_backend.json_array);
    prepared_database_aevt =
      Lg_edn_backend.json_field json "aevt" |> json_optional_int_array;
    prepared_database_avet =
      Lg_edn_backend.json_field json "avet" |> json_optional_int_array;
    prepared_database_branching_factor = branching_factor;
    prepared_database_ref_type = ref_type;
  }

let prepare = function
  | Lg_edn_backend.Json_source source -> prepare_json source
  | value -> prepare_edn value

let prepared_count value = value.prepared_database_count
let prepared_tx0 value = value.prepared_database_tx0
let prepared_max_eid value = value.prepared_database_max_eid
let prepared_max_tx value = value.prepared_database_max_tx
let prepared_schema_value value = value.prepared_database_schema
let prepared_schema_source value = string_value value.prepared_database_schema
let prepared_attrs value = value.prepared_database_attrs
let prepared_keywords value = value.prepared_database_keywords
let prepared_aevt_array value = value.prepared_database_aevt
let prepared_avet_array value = value.prepared_database_avet

let prepared_branching_factor value =
  value.prepared_database_branching_factor

let prepared_ref_type value = value.prepared_database_ref_type

let prepared_datom_count value =
  match value.prepared_database_datoms with
  | Prepared_edn_datoms values -> Array.length values
  | Prepared_json_datoms values -> Array.length values

let prepared_json_datom entity attribute value tx =
  {
    prepared_entity = Lg_edn_backend.json_int entity;
    prepared_attribute = Lg_edn_backend.json_int attribute;
    prepared_value = Prepared_json_value value;
    prepared_tx = Lg_edn_backend.json_int tx;
  }

let prepared_datom value index =
  match value.prepared_database_datoms with
  | Prepared_edn_datoms values -> (
      match values.(index) with
      | Lg_edn_backend.Int4_vector
          (prepared_entity, prepared_attribute, prepared_value, prepared_tx) ->
          {
            prepared_entity;
            prepared_attribute;
            prepared_value = Prepared_edn_value prepared_value;
            prepared_tx;
          }
      | Lg_edn_backend.Vector fields when Array.length fields = 4 ->
          {
            prepared_entity = int_value fields.(0);
            prepared_attribute = int_value fields.(1);
            prepared_value = Prepared_edn_value fields.(2);
            prepared_tx = int_value fields.(3);
          }
      | _ -> invalid_arg "invalid serialized datom")
  | Prepared_json_datoms values ->
      Lg_edn_backend.with_json_array4 values.(index) prepared_json_datom

let prepared_datom_entity value = value.prepared_entity
let prepared_datom_attribute value = value.prepared_attribute

let prepared_datom_value value =
  match value.prepared_value with
  | Prepared_edn_value value -> value
  | Prepared_json_value value -> Lg_edn_backend.json_to_edn value

let decode_json_value keywords value =
  match Lg_edn_backend.json_string_opt value with
  | Some value -> Data_value.String value
  | None -> (
      match Lg_edn_backend.json_int_opt value with
      | Some value -> Data_value.Int value
      | None -> (
          match Lg_edn_backend.json_float_opt value with
          | Some value -> Data_value.Float value
          | None -> (
              match Lg_edn_backend.json_bool_opt value with
              | Some value -> Data_value.Bool value
              | None -> (
                  match Lg_edn_backend.json_array_opt value with
                  | Some marker when Array.length marker = 2 -> (
                      match Lg_edn_backend.json_int_opt marker.(0) with
                      | Some 0 ->
                          Data_value.Keyword
                            (Rrbvec.nth keywords
                               (Lg_edn_backend.json_int marker.(1)))
                      | Some 1 ->
                          marker.(1)
                          |> Lg_edn_backend.json_string
                          |> Lg_edn_backend.of_edn_string
                          |> data_value_of_edn
                      | _ ->
                          invalid_arg
                            "invalid serialized DataScript value marker")
                  | Some marker when Array.length marker = 1 -> (
                      match Lg_edn_backend.json_int_opt marker.(0) with
                      | Some 2 -> Data_value.Float infinity
                      | Some 3 -> Data_value.Float neg_infinity
                      | Some 4 -> Data_value.Float nan
                      | _ ->
                          invalid_arg
                            "invalid serialized DataScript value marker")
                  | _ ->
                      invalid_arg
                        "invalid serialized DataScript value"))))

let decode_prepared_datom_value keywords value =
  match value.prepared_value with
  | Prepared_edn_value value -> decode_value keywords value
  | Prepared_json_value value -> decode_json_value keywords value

let prepared_datom_tx value = value.prepared_tx

let schema_to_edn = function
  | None -> Lg_edn_backend.Nil
  | Some schema ->
      Lg_edn_backend.Map
        (schema |> Lg_runtime.Lg_map.to_list
        |> List.map (fun (attr, properties) ->
               ( Lg_edn_backend.Keyword (edn_keyword attr),
                 Lg_edn_backend.Map
                   (properties |> Lg_runtime.Lg_map.to_list
                   |> List.map (fun (property, value) ->
                          ( Lg_edn_backend.Keyword (edn_keyword property),
                            data_value_to_edn value ))
                   |> Array.of_list) ))
        |> Array.of_list)

let schema_of_edn = function
  | Lg_edn_backend.Nil -> None
  | Lg_edn_backend.Map entries ->
      Some
        (entries |> Array.to_list
        |> List.map (fun (attr, properties) ->
               let attr =
                 match attr with
                 | Lg_edn_backend.Keyword attr -> data_keyword attr
                 | _ ->
                     invalid_arg
                       "serialized schema attribute must be a keyword"
               in
               let properties =
                 match properties with
                 | Lg_edn_backend.Map properties ->
                     properties |> Array.to_list
                     |> List.map (fun (property, value) ->
                            let property =
                              match property with
                              | Lg_edn_backend.Keyword property ->
                                  data_keyword property
                              | _ ->
                                  invalid_arg
                                    "serialized schema property must be a keyword"
                            in
                            (property, data_value_of_edn value))
                     |> Lg_runtime.Lg_map.of_list
                 | _ -> invalid_arg "serialized schema entry must be a map"
               in
               (attr, properties))
        |> Lg_runtime.Lg_map.of_list)
  | _ -> invalid_arg "serialized schema must be nil or a map"

let schema_to_string schema =
  schema |> schema_to_edn |> Lg_edn_backend.to_edn_string

let schema_of_string source =
  source |> Lg_edn_backend.of_edn_string |> schema_of_edn

type datom_reader_value = {
  entity : int;
  attribute : string;
  value : Data_value.t;
  transaction : int;
  added : bool;
}

type database_reader_value = {
  reader_schema : schema;
  reader_datoms : datom_reader_value Rrbvec.t;
}

let tagged_payload expected source =
  match Lg_edn_backend.of_edn_string source with
  | Lg_edn_backend.Tagged (tag, payload) when String.equal tag expected ->
      payload
  | Lg_edn_backend.Tagged (tag, _) ->
      invalid_arg
        ("expected #" ^ expected ^ ", got tagged literal #" ^ tag)
  | _ -> invalid_arg ("expected #" ^ expected ^ " tagged literal")

let keyword_value = function
  | Lg_edn_backend.Keyword value -> data_keyword value
  | _ -> invalid_arg "serialized datom attribute must be a keyword"

let datom_reader_value_of_edn value =
  match vector_values value with
  | [ entity; attribute; value ] ->
      {
        entity = int_value entity;
        attribute = keyword_value attribute;
        value = data_value_of_edn value;
        transaction = 536_870_912;
        added = true;
      }
  | [ entity; attribute; value; transaction ] ->
      {
        entity = int_value entity;
        attribute = keyword_value attribute;
        value = data_value_of_edn value;
        transaction = int_value transaction;
        added = true;
      }
  | [ entity; attribute; value; transaction; Lg_edn_backend.Bool added ] ->
      {
        entity = int_value entity;
        attribute = keyword_value attribute;
        value = data_value_of_edn value;
        transaction = int_value transaction;
        added;
      }
  | _ -> invalid_arg "invalid #datascript/Datom payload"

let read_datom source =
  source |> tagged_payload "datascript/Datom" |> datom_reader_value_of_edn

let reader_datom_entity value = value.entity
let reader_datom_attribute value = value.attribute
let reader_datom_value value = value.value
let reader_datom_transaction value = value.transaction
let reader_datom_added value = value.added

let tagged_map_field value name =
  match value with
  | Lg_edn_backend.Map fields ->
      (match
         Array.find_map
           (fun (key, value) ->
             match key with
             | Lg_edn_backend.Keyword key when String.equal key name ->
                 Some value
             | _ -> None)
           fields
       with
      | Some value -> value
      | None -> invalid_arg ("missing tagged database field :" ^ name))
  | _ -> invalid_arg "#datascript/DB payload must be a map"

let read_database source =
  let payload = tagged_payload "datascript/DB" source in
  let reader_schema = tagged_map_field payload "schema" |> schema_of_edn in
  let reader_datoms =
    tagged_map_field payload "datoms" |> vector_values
    |> List.map datom_reader_value_of_edn |> Rrbvec.of_list
  in
  { reader_schema; reader_datoms }

let reader_database_schema value = value.reader_schema
let reader_database_datoms value = value.reader_datoms

let schema_to_value = schema_to_edn
let schema_of_value = schema_of_edn
