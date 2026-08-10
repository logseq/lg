let test_priority_map_uses_lowest_priority_bucket () =
  let priorities =
    Lg_runtime.Runtime_priority_map.empty
    |> fun map -> Lg_runtime.Runtime_priority_map.assoc map "a" 2
    |> fun map -> Lg_runtime.Runtime_priority_map.assoc map "b" 1
  in
  assert (Lg_runtime.Runtime_priority_map.peek priorities = Some ("b", 1));
  let priorities =
    Lg_runtime.Runtime_priority_map.assoc priorities "b" 3
  in
  assert (Lg_runtime.Runtime_priority_map.peek priorities = Some ("a", 2));
  let priorities = Lg_runtime.Runtime_priority_map.dissoc priorities "a" in
  assert (Lg_runtime.Runtime_priority_map.peek priorities = Some ("b", 3))

let test_lru_cache_evicts_the_least_recent_key () =
  let cache =
    Lg_runtime.Runtime_cache.lru_of_map 2 Lg_runtime.Runtime_map.empty
    |> fun cache -> Lg_runtime.Runtime_cache.lru_miss cache "a" 1
    |> fun cache -> Lg_runtime.Runtime_cache.lru_miss cache "b" 2
    |> fun cache -> Lg_runtime.Runtime_cache.lru_hit cache "a"
    |> fun cache -> Lg_runtime.Runtime_cache.lru_miss cache "c" 3
  in
  assert (Lg_runtime.Runtime_cache.lru_lookup cache "a" = Some 1);
  assert (Lg_runtime.Runtime_cache.lru_lookup cache "b" = None);
  assert (Lg_runtime.Runtime_cache.lru_lookup cache "c" = Some 3)

let () =
  test_priority_map_uses_lowest_priority_bucket ();
  test_lru_cache_evicts_the_least_recent_key ()
