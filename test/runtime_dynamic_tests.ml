module Dynamic = Lg_runtime.Runtime_dynamic
module Persistent_map = Lg_runtime.Runtime_map

let key_id = Dynamic.keyword ":id"

let make_key comparisons id =
  let value =
    Dynamic.opaque "test.Key" [ (":id", fun () -> Dynamic.int id) ]
  in
  Dynamic.with_protocols value
    [
      Dynamic.protocol "IHash"
        [
          Dynamic.protocol_method_0 "-hash" (fun () -> Dynamic.int id);
        ];
      Dynamic.protocol "IEquiv"
        [
          Dynamic.protocol_method_1 "-equiv" (fun other ->
              incr comparisons;
              Dynamic.bool
                (Dynamic.numeric_equal (Dynamic.get other key_id)
                   (Dynamic.int id)));
        ];
    ]

let assert_comparisons_are_near_linear operation comparisons count =
  let limit = count * 4 in
  if !comparisons > limit then
    failwith
      (Printf.sprintf "%s used %d equality comparisons for %d distinct keys"
         operation !comparisons count)

let test_persistent_map_uses_hash_index () =
  let comparisons = ref 0 in
  let count = 256 in
  let keys = List.init count (fun index -> make_key comparisons (Int64.of_int index)) in
  let original = Dynamic.map [] in
  let populated =
    List.fold_left
      (fun map key -> Dynamic.assoc map key (Dynamic.get key key_id))
      original keys
  in
  assert (Dynamic.count_value original = 0);
  assert (Dynamic.count_value populated = count);
  List.iteri
    (fun index key ->
      assert
        (Dynamic.numeric_equal (Dynamic.get populated key)
           (Dynamic.int (Int64.of_int index))))
    keys;
  assert_comparisons_are_near_linear "dynamic map" comparisons count;
  let duplicate = make_key comparisons 42L in
  let replaced = Dynamic.assoc populated duplicate (Dynamic.string "updated") in
  assert (Dynamic.count_value replaced = count);
  assert (Dynamic.equal (Dynamic.get replaced duplicate) (Dynamic.string "updated"));
  assert (Dynamic.numeric_equal (Dynamic.get populated duplicate) (Dynamic.int 42L))

let test_persistent_map_preserves_insertion_order () =
  let first = Dynamic.keyword ":first" in
  let second = Dynamic.keyword ":second" in
  let third = Dynamic.keyword ":third" in
  let original =
    Dynamic.map [ (first, Dynamic.int 1L); (second, Dynamic.int 2L) ]
  in
  let appended = Dynamic.assoc original third (Dynamic.int 3L) in
  let replaced = Dynamic.assoc appended second (Dynamic.int 20L) in
  let expected_keys = [ first; second; third ] in
  assert (List.map fst (Dynamic.entries appended) = expected_keys);
  assert (List.map fst (Dynamic.entries replaced) = expected_keys);
  assert (Dynamic.numeric_equal (Dynamic.get replaced second) (Dynamic.int 20L));
  assert (Dynamic.numeric_equal (Dynamic.get appended second) (Dynamic.int 2L))

let test_static_persistent_map_preserves_insertion_order () =
  let original =
    Persistent_map.of_list [ (":name", 1); (":email", 2); (":age", 3) ]
  in
  let replaced = Persistent_map.assoc original ":email" 20 in
  let reinserted =
    Persistent_map.assoc (Persistent_map.dissoc replaced ":name") ":name" 10
  in
  assert (List.map fst (Persistent_map.to_list original) = [ ":name"; ":email"; ":age" ]);
  assert (List.map fst (Persistent_map.to_list replaced) = [ ":name"; ":email"; ":age" ]);
  assert (Persistent_map.get_option original ":email" = Some 2);
  assert (Persistent_map.get_option replaced ":email" = Some 20);
  assert (List.map fst (Persistent_map.to_list reinserted) = [ ":email"; ":age"; ":name" ])

let test_update_in_traverses_vector_indexes () =
  let friend = Dynamic.keyword ":friend" in
  let age = Dynamic.keyword ":age" in
  let target =
    Dynamic.map
      [
        ( friend,
          Dynamic.vector
            (Rrbvec.of_list [ Dynamic.map [ (Dynamic.keyword ":name", Dynamic.string "Ada") ] ]) );
      ]
  in
  let assoc =
    Dynamic.function_ (function
      | [ target; key; value ] -> Dynamic.assoc target key value
      | _ -> invalid_arg "assoc test callback expects three arguments")
  in
  let updated =
    Dynamic.update_in target
      (List.to_seq [ friend; Dynamic.int 0L ])
      assoc [ age; Dynamic.int 42L ]
  in
  assert
    (Dynamic.numeric_equal
       (Dynamic.get
          (Dynamic.get (Dynamic.get updated friend) (Dynamic.int 0L))
          age)
       (Dynamic.int 42L))

let test_persistent_set_uses_hash_index () =
  let comparisons = ref 0 in
  let count = 256 in
  let keys = List.init count (fun index -> make_key comparisons (Int64.of_int index)) in
  let original = Dynamic.set Seq.empty in
  let populated = List.fold_left Dynamic.conj original keys in
  assert (Dynamic.count_value original = 0);
  assert (Dynamic.count_value populated = count);
  List.iter (fun key -> assert (Dynamic.contains populated key)) keys;
  assert_comparisons_are_near_linear "dynamic set" comparisons count;
  let duplicate = make_key comparisons 42L in
  let unchanged = Dynamic.conj populated duplicate in
  assert (Dynamic.count_value unchanged = count);
  assert (Dynamic.contains unchanged duplicate);
  assert (not (Dynamic.contains original duplicate))

let test_lazy_record_fields_are_evaluated_once () =
  let evaluations = ref 0 in
  let record =
    Dynamic.lazy_record "test.LazyRecord"
      [
        ( ":value",
          fun () ->
            incr evaluations;
            Dynamic.int 42L );
      ]
      []
  in
  assert (!evaluations = 0);
  let key = Dynamic.keyword ":value" in
  assert (Dynamic.numeric_equal (Dynamic.get record key) (Dynamic.int 42L));
  assert (Dynamic.numeric_equal (Dynamic.get record key) (Dynamic.int 42L));
  ignore (List.of_seq (Dynamic.to_seq record));
  ignore (Dynamic.entries record);
  assert (!evaluations = 1)

let () =
  test_persistent_map_uses_hash_index ();
  test_persistent_map_preserves_insertion_order ();
  test_static_persistent_map_preserves_insertion_order ();
  test_update_in_traverses_vector_indexes ();
  test_persistent_set_uses_hash_index ();
  test_lazy_record_fields_are_evaluated_once ()
