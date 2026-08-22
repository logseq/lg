module Dynamic = Lg_runtime.Runtime_dynamic
module Persistent_map = Lg_runtime.Runtime_map
module Runtime_int = Lg_runtime.Runtime_int

let test_popcount_32 () =
  assert (Runtime_int.popcount_32 0 = 0);
  assert (Runtime_int.popcount_32 1 = 1);
  assert (Runtime_int.popcount_32 0xffffffff = 32);
  assert (Runtime_int.popcount_32 0xaaaaaaaa = 16)

let test_keywords_are_interned () =
  assert (Dynamic.keyword ":query/attrs" == Dynamic.keyword ":query/attrs")

let test_identifier_comparison_preserves_namespace_and_name_ordering () =
  let compare left right =
    Dynamic.compare (Dynamic.keyword left) (Dynamic.keyword right)
  in
  assert (compare ":a" ":a" = 0);
  assert (compare ":a" ":a/b" < 0);
  assert (compare ":alpha/z" ":beta/a" < 0);
  assert (compare ":alpha/a" ":alpha/z" < 0);
  assert (compare ":alpha/nested/a" ":alpha/nested/z" < 0);
  assert (compare ":命名/甲" ":命名/乙" > 0);
  assert (Dynamic.compare_identifier ":alpha/a" "alpha/a" = 0)

let test_dynamic_find_returns_a_map_entry_only_for_present_keys () =
  let key = Dynamic.keyword ":answer" in
  let map = Dynamic.map [ (key, Dynamic.int 42) ] in
  let entry = Dynamic.find map key |> Option.get in
  assert (Dynamic.equal (Dynamic.get entry (Dynamic.int 0)) key);
  assert
    (Dynamic.numeric_equal (Dynamic.get entry (Dynamic.int 1)) (Dynamic.int 42));
  assert (Dynamic.find map (Dynamic.keyword ":missing") = None)

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

let test_dynamic_hash_preserves_dynamic_value_semantics () =
  let first =
    Dynamic.vector (Rrbvec.of_list [ Dynamic.int 1; Dynamic.string "value" ])
  in
  let second =
    Dynamic.vector (Rrbvec.of_list [ Dynamic.int 1; Dynamic.string "value" ])
  in
  assert (Dynamic.equal first second);
  assert (Dynamic.hash first = Dynamic.hash second)

let () =
  test_popcount_32 ();
  test_keywords_are_interned ();
  test_identifier_comparison_preserves_namespace_and_name_ordering ();
  test_dynamic_find_returns_a_map_entry_only_for_present_keys ();
  test_static_persistent_map_preserves_insertion_order ();
  test_dynamic_hash_preserves_dynamic_value_semantics ()
