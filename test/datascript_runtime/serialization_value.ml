type t = Lg_edn_backend.t
type format = Current | Legacy

type schema =
  (string, (string, Data_value.t) Lg_runtime.Lg_map.t) Lg_runtime.Lg_map.t
  option

let int value = Lg_edn_backend.Int (Int64.of_int value)
let int_value = function Lg_edn_backend.Int value -> Int64.to_int value | _ -> invalid_arg "expected serialized int"
let string value = Lg_edn_backend.String value
let string_value = function Lg_edn_backend.String value -> value | _ -> invalid_arg "expected serialized string"
let vector values = Lg_edn_backend.Vector (Array.of_list values)

let vector_values = function
  | Lg_edn_backend.Vector values -> Array.to_list values
  | _ -> invalid_arg "expected serialized vector"

let rrbvec_of_values values = values |> vector_values |> Rrbvec.of_list

let edn_keyword value =
  if String.starts_with ~prefix:":" value then
    String.sub value 1 (String.length value - 1)
  else value

let data_keyword value =
  if String.starts_with ~prefix:":" value then value else ":" ^ value

let rec data_value_to_edn = function
  | Data_value.Nil -> Lg_edn_backend.Nil
  | Data_value.Int value -> int value
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
  | Data_value.Map entries ->
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
  | Lg_edn_backend.Tagged _ ->
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
  | Data_value.Int value -> int value
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

let datom entity attribute value tx =
  vector [ int entity; int attribute; value; int tx ]

let datom_fields value =
  match vector_values value with
  | [ entity; attribute; value; tx ] -> (entity, attribute, value, tx)
  | _ -> invalid_arg "invalid serialized datom"

let datom_entity value =
  let entity, _, _, _ = datom_fields value in
  int_value entity

let datom_attribute value =
  let _, attribute, _, _ = datom_fields value in
  int_value attribute

let datom_value value =
  let _, _, value, _ = datom_fields value in
  value

let datom_tx value =
  let _, _, _, tx = datom_fields value in
  int_value tx

let string_vector values =
  values |> Rrbvec.to_list |> List.map string |> vector

let int_vector values =
  values |> Rrbvec.to_list |> List.map int |> vector

let optional_int_vector = function
  | None -> Lg_edn_backend.Nil
  | Some values -> int_vector values

let database_with_schema count tx0 max_eid max_tx schema attrs keywords datoms aevt avet
    branching_factor ref_type =
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
      field "eavt" (vector (Rrbvec.to_list datoms));
      field "aevt" (optional_int_vector aevt);
      field "avet" (optional_int_vector avet);
      field "branching-factor" (int branching_factor);
      field "ref-type" (string ref_type);
    |]

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

let string_vector_value value =
  value |> vector_values |> List.map string_value |> Rrbvec.of_list

let int_vector_value value =
  value |> vector_values |> List.map int_value |> Rrbvec.of_list

let optional_int_vector_value = function
  | Lg_edn_backend.Nil -> None
  | value -> Some (int_vector_value value)

let attrs value = field value "attrs" |> string_vector_value
let keywords value = field value "keywords" |> string_vector_value
let datoms value = field value "eavt" |> rrbvec_of_values
let aevt value = field value "aevt" |> optional_int_vector_value
let avet value = field value "avet" |> optional_int_vector_value
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
