module Value = Datascript_runtime.Data_value
module Query_value = Datascript_runtime.Query_value
module Serialization_value = Datascript_runtime.Serialization_value
module Storage_backend = Datascript_runtime.Storage_backend
module Storage_value = Datascript_runtime.Storage_value
module Runtime_map = Lg_runtime.Runtime_map

let test_closed_values_compare_without_dynamic_boxing () =
  assert (Value.compare (Value.Int 42) (Value.Float 42.) = 0);
  assert (Value.compare (Value.Ref 42) (Value.Int 42) = 0);
  assert (Value.compare (Value.Keyword ":user/name") (Value.String "Ada") < 0)

let test_equal_numeric_values_share_a_hash () =
  let expected = Value.hash (Value.Int 42) in
  assert (Value.hash (Value.Float 42.) = expected);
  assert (Value.hash (Value.Ref 42) = expected);
  assert (Value.hash (Value.Wide_int 42L) = expected)

let test_wide_integers_remain_closed_numeric_values () =
  let wide = Value.Wide_int 3_735_928_559L in
  let native_int = Value.Int 3_735_928_559 in
  assert (Value.equal wide native_int);
  assert (Value.compare wide native_int = 0);
  assert (Value.hash wide = Value.hash native_int);
  assert (Value.to_edn_string wide = "3735928559");
  assert (
    Value.add (Rrbvec.of_list [ wide; Value.Int 1 ])
    = Some (Value.Wide_int 3_735_928_560L));
  assert (
    Value.multiply (Rrbvec.of_list [ wide; Value.Int 2 ])
    = Some (Value.Wide_int 7_471_857_118L))

let test_nil_wildcards_are_checked_without_general_value_equality () =
  assert (Value.is_nil Value.Nil);
  assert (not (Value.is_nil (Value.Int 0)));
  assert (not (Value.is_nil (Value.Keyword ":nil")))

let test_sequential_values_share_datascript_equality () =
  let list = Value.List [ Value.Int 1; Value.String "two" ] in
  let vector = Value.Vector [ Value.Int 1; Value.String "two" ] in
  let tuple = Value.Tuple [ Some (Value.Int 1); Some (Value.String "two") ] in
  assert (Value.equal list vector);
  assert (Value.equal list tuple);
  assert (Value.hash list = Value.hash vector);
  assert (Value.hash list = Value.hash tuple)

let test_map_and_set_equality_ignore_insertion_order () =
  let first_map =
    Value.Map
      [
        (Value.Keyword ":name", Value.String "Ada");
        (Value.Keyword ":age", Value.Int 36);
      ]
  in
  let second_map =
    Value.Hash_map
      [
        (Value.Keyword ":age", Value.Int 36);
        (Value.Keyword ":name", Value.String "Ada");
      ]
  in
  let first_set = Value.Set [ Value.Int 1; Value.String "two" ] in
  let second_set = Value.Set [ Value.String "two"; Value.Float 1. ] in
  assert (Value.equal first_map second_map);
  assert (Value.hash first_map = Value.hash second_map);
  assert (Value.equal first_set second_set);
  assert (Value.hash first_set = Value.hash second_set)

let test_tuple_constructor_accepts_static_vectors () =
  let values =
    [ Some (Value.Int 1); None; Some (Value.String "two") ]
    |> Rrbvec.of_list
  in
  assert (
    Value.tuple_of_vector values
    = Value.Tuple [ Some (Value.Int 1); None; Some (Value.String "two") ])

let test_static_string_vectors_build_closed_values () =
  assert (
    Value.string_vector (Rrbvec.of_list [ "a"; "b" ])
    = Value.Vector [ Value.String "a"; Value.String "b" ]);
  assert (
    Value.temp_id_vector (Rrbvec.of_list [ "1"; "2" ])
    = Value.Vector
        [
          Value.Ref_to (Value.Temp_id "1");
          Value.Ref_to (Value.Temp_id "2");
        ])

let test_static_vectors_convert_directly_to_closed_values () =
  let conversions = ref 0 in
  let convert value =
    incr conversions;
    Value.Int value
  in
  assert (
    Value.vector_of_vector_with convert (Rrbvec.of_list [ 1; 2; 3 ])
    = Value.Vector [ Value.Int 1; Value.Int 2; Value.Int 3 ]);
  assert (!conversions = 3);
  assert (
    Value.vector_of_vector_with convert Rrbvec.empty
    = Value.Vector []);
  assert (!conversions = 3)

let test_tuple_items_are_extracted_without_dynamic_conversion () =
  let expected =
    [ Some (Value.Int 1); None; Some (Value.String "two") ]
    |> Rrbvec.of_list
  in
  assert (
    Value.tuple_items
      (Value.Tuple [ Some (Value.Int 1); None; Some (Value.String "two") ])
    = Some expected);
  assert (Value.tuple_items (Value.Int 1) = None)

let test_keyword_payload_is_extracted_statically () =
  assert (Value.keyword_value (Value.Keyword ":user/name") = Some ":user/name");
  assert (Value.keyword_value (Value.String ":user/name") = None)

let test_keyword_maps_are_extracted_statically () =
  let expected =
    Lg_runtime.Lg_map.of_list
      [
        (":db/id", Value.Int 1);
        (":name", Value.String "Ada");
      ]
  in
  assert (
    Value.keyword_map_value
      (Value.Map
         [
           (Value.Keyword ":db/id", Value.Int 1);
           (Value.Keyword ":name", Value.String "Ada");
         ])
    = Some expected);
  assert (
    Value.keyword_map_value
      (Value.Map [ (Value.String "name", Value.String "Ada") ])
    = None);
  assert (Value.keyword_map_value (Value.Int 1) = None)

let test_boolean_payload_is_extracted_statically () =
  assert (Value.bool_value (Value.Bool true) = Some true);
  assert (Value.bool_value (Value.Keyword ":true") = None)

let test_collection_items_preserve_collection_kind () =
  let values = [ Value.Int 1; Value.String "two" ] in
  let expected = Rrbvec.of_list values in
  assert (Value.sequential_items (Value.Vector values) = Some expected);
  assert (Value.sequential_items (Value.List values) = Some expected);
  assert (Value.sequential_items (Value.Set values) = None);
  assert (Value.set_items (Value.Set values) = Some expected);
  assert (Value.set_items (Value.Vector values) = None)

let test_entity_refs_are_extracted_from_closed_values () =
  let named = Value.Temp_id "temp" in
  assert (Value.entity_ref_value (Value.Ref_to named) = Some named);
  assert (
    Value.entity_ref_value (Value.Int 42)
    = Some (Value.Entity_id 42));
  assert (
    Value.entity_ref_value (Value.Ref 42)
    = Some (Value.Entity_id 42));
  assert (Value.entity_ref_value (Value.String "42") = None)

let test_lookup_refs_are_extracted_from_closed_vectors () =
  assert (
    Value.lookup_ref_value
      (Value.Vector
         [ Value.Keyword ":name"; Value.String "Alice" ])
    = Some (":name", Value.String "Alice"));
  assert (
    Value.lookup_ref_value
      (Value.List [ Value.Keyword ":name"; Value.Int 1 ])
    = Some (":name", Value.Int 1));
  assert (
    Value.lookup_ref_value
      (Value.Vector [ Value.String "name"; Value.String "Alice" ])
    = None);
  assert (
    Value.lookup_ref_value
      (Value.Vector [ Value.Keyword ":name" ])
    = None)

let test_ref_values_are_extracted_statically () =
  assert (Value.ref_value (Value.Ref 42) = Some 42);
  assert (Value.ref_value (Value.Int 42) = None)

let test_tuple_refs_are_resolved_statically () =
  let attrs = Rrbvec.of_list [ ":name"; ":friend" ] in
  let value =
    Value.Tuple
      [
        Some (Value.String "Ada");
        Some (Value.Ref_to (Value.Temp_id "friend"));
      ]
  in
  let ref_attrs = Rrbvec.of_list [ ":friend" ] in
  assert (
    Value.tuple_entity_refs attrs ref_attrs value
    = Rrbvec.of_list [ Value.Temp_id "friend" ]);
  assert (
    Value.resolve_tuple_refs attrs ref_attrs (Rrbvec.of_list [ 42 ]) value
    = Value.Tuple [ Some (Value.String "Ada"); Some (Value.Ref 42) ]);
  assert (
    try
      ignore
        (Value.resolve_tuple_refs (Rrbvec.of_list [ ":name" ]) ref_attrs
           Rrbvec.empty value);
      false
    with Invalid_argument _ -> true)

let test_keyword_collections_are_validated_statically () =
  let expected = Rrbvec.of_list [ ":user/name"; ":user/email" ] in
  assert (
    Value.keyword_items
      (Value.Vector
         [ Value.Keyword ":user/name"; Value.Keyword ":user/email" ])
    = Some expected);
  assert (
    Value.keyword_items
      (Value.Vector [ Value.Keyword ":user/name"; Value.String ":user/email" ])
    = None);
  assert (Value.keyword_items (Value.Int 1) = None)

let test_runtime_types_are_closed_values () =
  assert (
    Value.runtime_type_value (Value.Int 1)
    = Value.Runtime_type Value.Number_type);
  assert (
    Value.runtime_type_value (Value.Float 1.5)
    = Value.Runtime_type Value.Number_type);
  assert (
    Value.runtime_type_value (Value.List [])
    = Value.Runtime_type Value.Empty_list_type);
  assert (
    Value.runtime_type_value (Value.List [ Value.Int 1 ])
    = Value.Runtime_type Value.List_type);
  assert (
    Value.runtime_type_value
      (Value.Map
         [
           (Value.Keyword ":a", Value.Int 1);
           (Value.Keyword ":b", Value.Int 2);
           (Value.Keyword ":c", Value.Int 3);
           (Value.Keyword ":d", Value.Int 4);
           (Value.Keyword ":e", Value.Int 5);
           (Value.Keyword ":f", Value.Int 6);
           (Value.Keyword ":g", Value.Int 7);
           (Value.Keyword ":h", Value.Int 8);
           (Value.Keyword ":i", Value.Int 9);
         ])
    = Value.Runtime_type Value.Array_map_type);
  assert (
    Value.runtime_type_value
      (Value.Map [ (Value.Keyword ":a", Value.Int 1) ])
    = Value.Runtime_type Value.Array_map_type);
  assert (
    Value.runtime_type_value
      (Value.Hash_map [ (Value.Keyword ":a", Value.Int 1) ])
    = Value.Runtime_type Value.Hash_map_type);
  assert (
    Value.runtime_type_value
      (Value.as_array_map
         (Value.Hash_map [ (Value.Keyword ":a", Value.Int 1) ]))
    = Value.Runtime_type Value.Array_map_type);
  assert (
    Value.runtime_type_value (Value.Runtime_type Value.Number_type)
    = Value.Runtime_type Value.Function_type);
  assert (
    Value.identical_value
      (Rrbvec.of_list
         [
           Value.runtime_type_value (Value.Int 1);
           Value.runtime_type_value (Value.Float 1.5);
         ])
    = Some (Value.Bool true));
  assert (
    Value.to_edn_string (Value.Runtime_type Value.Number_type)
    = "#object[Number]")

let test_query_sources_and_results_are_closed_sum_types () =
  let db = "database" in
  let database_source = Query_value.database_source db in
  let row =
    [|
      Query_value.Entity 42;
      Query_value.Attr ":user/name";
      Query_value.Value (Value.String "Ada");
    |]
  in
  let rows = Rrbvec.of_list [ row ] in
  let relation_source = Query_value.relation_source rows in
  assert (Query_value.source_database database_source = Some db);
  assert (Query_value.source_rows database_source = None);
  assert (Query_value.source_database relation_source = None);
  assert (Query_value.source_rows relation_source = Some rows);
  assert (
    Query_value.result_value (Query_value.Value (Value.Int 7))
    = Some (Value.Int 7));
  let metadata = Value.Map [ (Value.Keyword ":source", Value.String "query") ] in
  let metadata_result = Query_value.metadata (Value.Int 7) metadata in
  assert (Query_value.result_value metadata_result = Some (Value.Int 7));
  assert (Query_value.result_metadata metadata_result = Some metadata);
  assert (
    Query_value.equal_result metadata_result
      (Query_value.Value (Value.Int 7)));
  assert (
    try
      ignore (Query_value.metadata (Value.Int 7) (Value.String "invalid"));
      false
    with
    | Invalid_argument message ->
        String.equal message "Query metadata must be a map"
    | _ -> false);
  assert (Query_value.result_value (Query_value.Entity 7) = None)

let test_query_relations_and_contexts_keep_static_fields () =
  assert (
    Query_value.index_attrs (Rrbvec.of_list [ "?e"; "?name" ])
    = Lg_runtime.Lg_map.of_list [ ("?e", 0); ("?name", 1) ]);
  let attrs =
    Lg_runtime.Lg_map.of_list [ ("?e", 0); ("?name", 1) ]
  in
  let lookup_databases =
    Lg_runtime.Lg_map.of_list [ ("?e", "database") ]
  in
  let rows =
    Rrbvec.of_list
      [
        [|
          Query_value.Entity 42;
          Query_value.Value (Value.String "Ada");
        |];
      ]
  in
  let relation = Query_value.relation attrs rows lookup_databases in
  let sources =
    Lg_runtime.Lg_map.of_list
      [ ("$", Query_value.database_source "database") ]
  in
  let context =
    Query_value.context (Rrbvec.of_list [ relation ]) sources [ "rule" ]
  in
  assert (Query_value.relation_attrs relation = attrs);
  assert (Query_value.relation_rows relation = rows);
  assert (Query_value.relation_lookup_databases relation = lookup_databases);
  assert (
    Query_value.relation_result relation "?name"
      [|
        Query_value.Entity 42;
        Query_value.Value (Value.String "Ada");
      |]
    = Some (Query_value.Value (Value.String "Ada")));
  assert (Query_value.relation_result relation "?missing" [||] = None);
  assert (
    Query_value.relation_lookup_database relation "?e" = Some "database");
  assert (
    Query_value.relation_lookup_database relation "?name" = None);
  let replacement_rows = Rrbvec.empty in
  let without_rows = Query_value.relation_with_rows relation replacement_rows in
  assert (Query_value.relation_rows without_rows = replacement_rows);
  assert (Query_value.relation_attrs without_rows = attrs);
  assert (
    Query_value.relation_lookup_databases without_rows = lookup_databases);
  let appended = Query_value.relation_append_rows relation relation in
  assert (Rrbvec.length (Query_value.relation_rows appended) = 2);
  assert (Query_value.context_relations context = Rrbvec.of_list [ relation ]);
  assert (Query_value.context_sources context = sources);
  assert (Query_value.context_rules context = [ "rule" ])

let test_query_rows_use_static_integer_indexes () =
  assert (Query_value.empty_row () = [||]);
  let left =
    [|
      Query_value.Entity 42;
      Query_value.Value (Value.String "Ada");
    |]
  in
  let right =
    [|
      Query_value.Attr ":user/name";
      Query_value.Value (Value.Int 7);
    |]
  in
  assert (Query_value.row_get left 0 = Some (Query_value.Entity 42));
  assert (Query_value.row_get left 2 = None);
  assert (
    Query_value.project_row left [| 1; 0 |]
    = [|
        Query_value.Value (Value.String "Ada");
        Query_value.Entity 42;
      |]);
  assert (
    Query_value.join_rows left [| 1; 0 |] right [| 1 |]
    = [|
        Query_value.Value (Value.String "Ada");
        Query_value.Entity 42;
        Query_value.Value (Value.Int 7);
      |]);
  assert (
    Query_value.concat_rows left right
    = [|
        Query_value.Entity 42;
        Query_value.Value (Value.String "Ada");
        Query_value.Attr ":user/name";
        Query_value.Value (Value.Int 7);
      |]);
  assert (
    Query_value.concat_rows
      [| Query_value.Value (Value.Int 50_000) |]
      [| Query_value.Entity 42; Query_value.Value (Value.Int 60_000) |]
    = [|
        Query_value.Value (Value.Int 50_000);
        Query_value.Entity 42;
        Query_value.Value (Value.Int 60_000);
      |]);
  let products =
    Query_value.product_rows (Rrbvec.of_list [ left ])
      (Rrbvec.of_list [ right; right ])
  in
  assert (Rrbvec.length products = 2);
  assert (Rrbvec.nth products 0 = Query_value.concat_rows left right)

let test_query_product_rows_singleton_avoids_array_roundtrip () =
  let row_count = 100_000 in
  let left_rows =
    Array.init row_count (fun entity ->
        [|
          Query_value.Entity entity;
          Query_value.Value (Value.Int entity);
        |])
    |> Rrbvec.of_array
  in
  let right = [| Query_value.Attr ":benchmark/tag" |] in
  let right_rows = Rrbvec.of_list [ right ] in
  Gc.compact ();
  let allocated_before = Gc.allocated_bytes () in
  let products = Query_value.product_rows left_rows right_rows in
  let allocated_bytes = Gc.allocated_bytes () -. allocated_before in
  assert (Rrbvec.length products = row_count);
  assert (
    Rrbvec.nth products 0
    = Query_value.concat_rows (Rrbvec.nth left_rows 0) right);
  assert (
    Rrbvec.nth products (row_count - 1)
    = Query_value.concat_rows
        (Rrbvec.nth left_rows (row_count - 1))
        right);
  if allocated_bytes >= 5_500_000. then
    failwith
      (Printf.sprintf
         "query singleton product allocated an intermediate array: %.0f bytes"
         allocated_bytes)

let test_query_product_rows_treats_an_empty_tuple_as_identity () =
  let row_count = 100_000 in
  let rows =
    Array.init row_count (fun entity ->
        [|
          Query_value.Entity entity;
          Query_value.Value (Value.Int entity);
        |])
    |> Rrbvec.of_array
  in
  let empty_tuple = Rrbvec.of_list [ Query_value.empty_row () ] in
  let assert_identity_product left right =
    Gc.compact ();
    let allocated_before = Gc.allocated_bytes () in
    let products = Query_value.product_rows left right in
    let allocated_bytes = Gc.allocated_bytes () -. allocated_before in
    assert (Rrbvec.length products = row_count);
    assert (Rrbvec.nth products 0 = Rrbvec.nth rows 0);
    assert (Rrbvec.nth products (row_count - 1) = Rrbvec.nth rows (row_count - 1));
    if allocated_bytes >= 2_048. then
      failwith
        (Printf.sprintf
           "query empty-tuple product copied identity rows: %.0f bytes"
           allocated_bytes)
  in
  assert_identity_product empty_tuple rows;
  assert_identity_product rows empty_tuple

let test_query_distinct_entity_rows_avoid_boxed_hash_keys () =
  let row_count = 100_000 in
  let rows =
    Array.init row_count (fun entity -> [| Query_value.entity entity |])
    |> Array.to_list |> Rrbvec.of_list
  in
  Gc.compact ();
  let allocated_before = Gc.allocated_bytes () in
  let distinct = Query_value.distinct_rows rows in
  let allocated_bytes = Gc.allocated_bytes () -. allocated_before in
  assert (Rrbvec.length distinct = row_count);
  if allocated_bytes >= 33_500_000. then
    failwith
      (Printf.sprintf
         "query distinct rows allocated boxed hash keys: %.0f bytes"
         allocated_bytes)

let test_static_map_assoc_hashes_each_key_once () =
  let hash_calls = ref 0 in
  let operations : string Runtime_map.operations =
    {
      hash =
        (fun value ->
          incr hash_calls;
          Hashtbl.hash value);
      equal = String.equal;
    }
  in
  let first = Runtime_map.assoc_by operations Runtime_map.empty "first" 1 in
  assert (!hash_calls = 1);
  let second = Runtime_map.assoc_by operations first "second" 2 in
  assert (!hash_calls = 2);
  let updated = Runtime_map.assoc_by operations second "first" 3 in
  assert (!hash_calls = 3);
  assert (Runtime_map.get_option_by operations updated "first" = Some 3);
  assert (!hash_calls = 4)

let test_static_map_assoc_uses_a_precomputed_hash () =
  let first_hash = Hashtbl.hash "first" in
  let second_hash = Hashtbl.hash "second" in
  let map =
    Runtime_map.empty
    |> fun map -> Runtime_map.assoc_hashed map "first" first_hash 1
    |> fun map -> Runtime_map.assoc_hashed map "second" second_hash 2
    |> fun map -> Runtime_map.assoc_hashed map "first" first_hash 3
  in
  assert (Runtime_map.count map = 2);
  assert (Runtime_map.get_option map "first" = Some 3);
  assert (Runtime_map.get_option map "second" = Some 2)

let test_query_hash_join_uses_closed_result_keys () =
  let empty_databases = Lg_runtime.Lg_map.empty in
  let left =
    Query_value.relation
      (Lg_runtime.Lg_map.of_list [ ("?e", 0) ])
      (Rrbvec.of_list
         [ [| Query_value.Entity 1 |]; [| Query_value.Entity 2 |] ])
      empty_databases
  in
  let right =
    Query_value.relation
      (Lg_runtime.Lg_map.of_list [ ("?e", 0); ("?name", 1) ])
      (Rrbvec.of_list
         [
           [|
             Query_value.Entity 2;
             Query_value.Value (Value.String "B");
           |];
           [|
             Query_value.Entity 1;
             Query_value.Value (Value.String "A");
           |];
         ])
      empty_databases
  in
  let joined =
    Query_value.hash_join (fun _ result -> result) left right
  in
  assert (
    Query_value.relation_attrs joined
    = Lg_runtime.Lg_map.of_list [ ("?e", 0); ("?name", 1) ]);
  assert (
    Query_value.relation_rows joined
    = Rrbvec.of_list
        [
          [|
            Query_value.Entity 2;
            Query_value.Value (Value.String "B");
          |];
          [|
            Query_value.Entity 1;
            Query_value.Value (Value.String "A");
          |];
        ]);
  let attr_relation =
    Query_value.relation
      (Lg_runtime.Lg_map.of_list [ ("?a", 0) ])
      (Rrbvec.of_list [ [| Query_value.Attr ":user/name" |] ])
      empty_databases
  in
  let keyword_relation =
    Query_value.relation
      (Lg_runtime.Lg_map.of_list [ ("?a", 0) ])
      (Rrbvec.of_list
         [ [| Query_value.Value (Value.Keyword ":user/name") |] ])
      empty_databases
  in
  assert (
    Rrbvec.length
      (Query_value.relation_rows
         (Query_value.hash_join (fun _ result -> result) attr_relation
            keyword_relation))
    = 1);
  let entity_relation =
    Query_value.relation
      (Lg_runtime.Lg_map.of_list [ ("?e", 0) ])
      (Rrbvec.of_list [ [| Query_value.Entity 7 |] ])
      (Lg_runtime.Lg_map.of_list [ ("?e", "database") ])
  in
  let entity_id_value_relation =
    Query_value.relation
      (Lg_runtime.Lg_map.of_list [ ("?e", 0) ])
      (Rrbvec.of_list [ [| Query_value.Value (Value.Int 7) |] ])
      empty_databases
  in
  assert (
    Rrbvec.length
      (Query_value.relation_rows
         (Query_value.hash_join (fun _ result -> result) entity_relation
            entity_id_value_relation))
    = 1);
  let ref_relation =
    Query_value.relation
      (Lg_runtime.Lg_map.of_list [ ("?e", 0) ])
      (Rrbvec.of_list [ [| Query_value.Value (Value.Ref 7) |] ])
      empty_databases
  in
  let resolve_lookup database = function
    | Query_value.Value (Value.Ref entity) when database = "database" ->
        Query_value.Entity entity
    | result -> result
  in
  assert (
    Rrbvec.length
      (Query_value.relation_rows
         (Query_value.hash_join resolve_lookup entity_relation ref_relation))
    = 1);
  let large_rows =
    List.init 2_000 (fun entity -> [| Query_value.Entity entity |])
    |> Rrbvec.of_list
  in
  let large_relation =
    Query_value.relation
      (Lg_runtime.Lg_map.of_list [ ("?e", 0) ])
      large_rows empty_databases
  in
  assert (
    Rrbvec.length
      (Query_value.relation_rows
         (Query_value.hash_join (fun _ result -> result) large_relation
            large_relation))
    = 2_000)

let test_query_inputs_use_closed_recursive_binding_values () =
  let entity =
    Query_value.scalar_binding (Query_value.Entity 42)
  in
  let name =
    Query_value.scalar_binding
      (Query_value.Value (Value.String "Ada"))
  in
  let tuple =
    Query_value.collection_binding (Rrbvec.of_list [ entity; name ])
  in
  let binding_input = Query_value.binding_input tuple in
  let source_input =
    Query_value.source_input (Query_value.database_source "database")
  in
  let rules_input = Query_value.rules_input [ "rule" ] in
  assert (Query_value.binding_result entity = Some (Query_value.Entity 42));
  assert (Query_value.binding_result tuple = None);
  assert (
    Query_value.binding_items tuple = Some (Rrbvec.of_list [ entity; name ]));
  assert (Query_value.input_binding binding_input = Some tuple);
  assert (Query_value.input_source binding_input = None);
  assert (Query_value.input_rules binding_input = None);
  assert (
    Query_value.input_source source_input
    = Some (Query_value.database_source "database"));
  assert (Query_value.input_binding source_input = None);
  assert (Query_value.input_rules rules_input = Some [ "rule" ])

let test_storage_payloads_keep_integer_addresses_and_closed_values () =
  let datom =
    Storage_value.serialized_datom 42 ":user/name" (Value.String "Ada") 7
  in
  let metadata = Storage_value.serialized_index 12 3 99 in
  let node =
    Storage_value.serialized_node (Rrbvec.of_list [ datom ])
      (Some (Rrbvec.of_list [ 13; 14 ]))
  in
  assert (Storage_value.datom_e datom = 42);
  assert (Storage_value.index_address metadata = 12);
  assert (
    Storage_value.node_addresses node = Some (Rrbvec.of_list [ 13; 14 ]));
  match Storage_value.Stored_node node with
  | Storage_value.Stored_node restored ->
      assert (Storage_value.node_keys restored = Rrbvec.of_list [ datom ])
  | Storage_value.Stored_root _ | Storage_value.Stored_tail _ -> assert false

let test_storage_backend_has_a_static_payload_boundary () =
  let disk = Hashtbl.create 4 in
  let backend =
    Storage_backend.create
      (fun entries deleted ->
        Rrbvec.iter (fun (address, value) -> Hashtbl.replace disk address value)
          entries;
        Rrbvec.iter (Hashtbl.remove disk) deleted)
      (Hashtbl.find_opt disk)
      (fun () ->
        Hashtbl.to_seq_keys disk |> List.of_seq |> Rrbvec.of_list)
      (fun addresses -> Rrbvec.iter (Hashtbl.remove disk) addresses)
  in
  let payload = Storage_value.Stored_tail Rrbvec.empty in
  Storage_backend.store backend (Rrbvec.of_list [ (17, payload) ]) Rrbvec.empty;
  assert (Storage_backend.restore backend 17 = Some payload);
  assert (Storage_backend.list_addresses backend = Rrbvec.of_list [ 17 ]);
  Storage_backend.delete backend (Rrbvec.of_list [ 17 ]);
  assert (Storage_backend.restore backend 17 = None)

let test_serialization_uses_a_closed_typed_facade () =
  let schema =
    Some
      (Lg_runtime.Lg_map.of_list
         [
           ( ":user/name",
             Lg_runtime.Lg_map.of_list
               [
                 (":db/valueType", Value.Keyword ":db.type/string");
                 (":db/index", Value.Bool true);
               ] );
         ])
  in
  let schema_source = Serialization_value.schema_to_string schema in
  assert (Serialization_value.schema_of_string schema_source = schema);
  let absent_schema_source = Serialization_value.schema_to_string None in
  assert (Serialization_value.schema_of_string absent_schema_source = None);
  let encoded_name =
    Serialization_value.encode_non_keyword (Value.String "Ada")
  in
  let encoded_keyword = Serialization_value.keyword_reference 0 in
  let datom =
    Serialization_value.datom 42 0 encoded_name 7
  in
  let serialized =
    Serialization_value.database 1 536870912 42 7 schema_source
      (Rrbvec.of_list [ ":user/name" ])
      (Rrbvec.of_list [ ":user/name" ])
      (Rrbvec.of_list [ datom ])
      (Some (Rrbvec.of_list [ 0 ]))
      (Some (Rrbvec.of_list [ 0 ]))
      512 Storage_value.Weak
  in
  assert (Serialization_value.tx0 serialized = 536870912);
  assert (Serialization_value.max_eid serialized = 42);
  assert (
    Serialization_value.attrs serialized
    = Rrbvec.of_list [ ":user/name" ]);
  assert (
    Serialization_value.datoms serialized = Rrbvec.of_list [ datom ]);
  assert (Serialization_value.datoms_array serialized = [| datom |]);
  assert (Serialization_value.aevt_array serialized = Some [| 0 |]);
  assert (Serialization_value.avet_array serialized = Some [| 0 |]);
  assert (Serialization_value.datom_entity datom = 42);
  assert (Serialization_value.ref_type serialized = Storage_value.Weak);
  assert (Serialization_value.format serialized = Serialization_value.Current);
  let legacy = Serialization_value.as_legacy serialized in
  assert (Serialization_value.format legacy = Serialization_value.Legacy);
  assert (Serialization_value.branching_factor legacy = 32);
  assert (Serialization_value.ref_type legacy = Storage_value.Strong);
  assert (
    Serialization_value.decode_value Rrbvec.empty encoded_name
    = Value.String "Ada");
  assert (
    Serialization_value.decode_value
      (Rrbvec.of_list [ ":user/name" ])
      encoded_keyword
    = Value.Keyword ":user/name");
  let encoder = Serialization_value.create_encoder () in
  assert (
    try
      ignore
        (Serialization_value.encode_value encoder
           (Value.Runtime_type Value.Number_type));
      false
    with
    | Invalid_argument message ->
        String.equal message "Runtime type values cannot be serialized"
    | _ -> false);
  let first_keyword =
    Serialization_value.encode_value encoder
      (Value.Keyword ":user/name")
  in
  let repeated_keyword =
    Serialization_value.encode_value encoder
      (Value.Keyword ":user/name")
  in
  assert (first_keyword = repeated_keyword);
  assert (
    Serialization_value.encoder_keywords encoder
    = Rrbvec.of_list [ ":user/name" ]);
  assert (
    Serialization_value.attribute_index
      (Rrbvec.of_list [ ":user/name"; ":user/age" ])
      ":user/age"
    = 1)

let test_serialization_attribute_indexes_preserve_attribute_order () =
  let indexes =
    Serialization_value.create_attribute_indexes
      (Rrbvec.of_list [ ":user/name"; ":user/age"; ":user/email" ])
  in
  assert (
    Serialization_value.find_attribute_index indexes ":user/name" = 0);
  assert (
    Serialization_value.find_attribute_index indexes ":user/email" = 2);
  assert (
    Serialization_value.find_attribute_index indexes ":user/missing" = -1)

let test_serialized_json_prepares_concrete_database_fields () =
  let source =
    {|{"count":1,"tx0":536870912,"max-eid":42,"max-tx":7,"schema":"nil","attrs":[":user/name"],"keywords":[],"eavt":[[42,0,"Ada",7]],"aevt":[0],"avet":[0],"branching-factor":32,"ref-type":"weak"}|}
  in
  let prepared =
    source
    |> Lg_runtime.Runtime_edn.read_json_source
    |> Serialization_value.prepare
  in
  assert (Serialization_value.prepared_count prepared = 1);
  assert (Serialization_value.prepared_tx0 prepared = 536870912);
  assert (Serialization_value.prepared_max_eid prepared = 42);
  assert (Serialization_value.prepared_max_tx prepared = 7);
  assert (
    Serialization_value.prepared_attrs prepared
    = Rrbvec.of_list [ ":user/name" ]);
  assert (Serialization_value.prepared_datom_count prepared = 1);
  let datom = Serialization_value.prepared_datom prepared 0 in
  assert (Serialization_value.prepared_datom_entity datom = 42);
  assert (Serialization_value.prepared_datom_attribute datom = 0);
  assert (
    Serialization_value.prepared_datom_value datom
    = Lg_edn_backend.String "Ada");
  assert (
    Serialization_value.decode_prepared_datom_value Rrbvec.empty datom
    = Datascript_runtime.Data_value.String "Ada");
  assert (Serialization_value.prepared_datom_tx datom = 7);
  assert (
    Serialization_value.prepared_ref_type prepared = Storage_value.Weak)

let test_prepared_json_rejects_invalid_datom () =
  let source =
    {|{"count":1,"tx0":536870912,"max-eid":42,"max-tx":7,"schema":"nil","attrs":[":user/name"],"keywords":[],"eavt":[[42,0,"Ada"]],"aevt":[0],"avet":[0],"branching-factor":32,"ref-type":"weak"}|}
  in
  let prepared =
    source
    |> Lg_runtime.Runtime_edn.read_json_source
    |> Serialization_value.prepare
  in
  match Serialization_value.prepared_datom prepared 0 with
  | _ -> failwith "invalid prepared JSON datom was accepted"
  | exception Invalid_argument _ -> ()

let () =
  test_closed_values_compare_without_dynamic_boxing ();
  test_equal_numeric_values_share_a_hash ();
  test_wide_integers_remain_closed_numeric_values ();
  test_nil_wildcards_are_checked_without_general_value_equality ();
  test_sequential_values_share_datascript_equality ();
  test_map_and_set_equality_ignore_insertion_order ();
  test_tuple_constructor_accepts_static_vectors ();
  test_static_string_vectors_build_closed_values ();
  test_static_vectors_convert_directly_to_closed_values ();
  test_tuple_items_are_extracted_without_dynamic_conversion ();
  test_keyword_payload_is_extracted_statically ();
  test_keyword_maps_are_extracted_statically ();
  test_boolean_payload_is_extracted_statically ();
  test_collection_items_preserve_collection_kind ();
  test_entity_refs_are_extracted_from_closed_values ();
  test_serialized_json_prepares_concrete_database_fields ();
  test_prepared_json_rejects_invalid_datom ();
  test_lookup_refs_are_extracted_from_closed_vectors ();
  test_ref_values_are_extracted_statically ();
  test_tuple_refs_are_resolved_statically ();
  test_keyword_collections_are_validated_statically ();
  test_runtime_types_are_closed_values ();
  test_query_sources_and_results_are_closed_sum_types ();
  test_query_relations_and_contexts_keep_static_fields ();
  test_query_rows_use_static_integer_indexes ();
  test_query_product_rows_singleton_avoids_array_roundtrip ();
  test_query_product_rows_treats_an_empty_tuple_as_identity ();
  test_query_distinct_entity_rows_avoid_boxed_hash_keys ();
  test_static_map_assoc_hashes_each_key_once ();
  test_static_map_assoc_uses_a_precomputed_hash ();
  test_query_hash_join_uses_closed_result_keys ();
  test_query_inputs_use_closed_recursive_binding_values ();
  test_storage_payloads_keep_integer_addresses_and_closed_values ();
  test_storage_backend_has_a_static_payload_boundary ();
  test_serialization_uses_a_closed_typed_facade ();
  test_serialization_attribute_indexes_preserve_attribute_order ()
