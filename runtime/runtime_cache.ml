type ('key, 'value) basic = ('key, 'value) Runtime_map.t

type ('key, 'value) lru = {
  cache : ('key, 'value) Runtime_map.t;
  priorities : 'key Runtime_priority_map.t;
  tick : int;
  limit : int;
}

let basic_of_map map = map
let basic_to_map cache = cache
let basic_lookup cache key = Runtime_map.lookup cache key

let basic_lookup_default cache key not_found =
  Runtime_map.lookup_default cache key not_found

let basic_contains cache key = Runtime_map.contains_key cache key
let basic_miss cache key value = Runtime_map.assoc cache key value
let basic_evict cache key = Runtime_map.dissoc cache key

let priorities_of_map cache =
  Runtime_map.kv_reduce
    (fun priorities key _ -> Runtime_priority_map.assoc priorities key 0)
    Runtime_priority_map.empty cache

let lru_of_map limit cache =
  if limit <= 0 then invalid_arg "LRU cache threshold must be positive"
  else { cache; priorities = priorities_of_map cache; tick = 0; limit }

let lru_to_map state = state.cache
let lru_lookup state key = Runtime_map.lookup state.cache key

let lru_lookup_default state key not_found =
  Runtime_map.lookup_default state.cache key not_found

let lru_contains state key = Runtime_map.contains_key state.cache key

let lru_hit state key =
  let tick = state.tick + 1 in
  let priorities =
    if lru_contains state key then
      Runtime_priority_map.assoc state.priorities key tick
    else state.priorities
  in
  { state with priorities; tick }

let least_recent_key priorities =
  Runtime_priority_map.peek priorities |> Option.map fst

let lru_miss state key value =
  let tick = state.tick + 1 in
  if lru_contains state key then
    {
      state with
      cache = Runtime_map.assoc state.cache key value;
      priorities = Runtime_priority_map.assoc state.priorities key tick;
      tick;
    }
  else
    let cache, priorities =
      if Runtime_map.count state.cache < state.limit then
        (state.cache, state.priorities)
      else
        match least_recent_key state.priorities with
        | None -> (state.cache, state.priorities)
        | Some evicted ->
            ( Runtime_map.dissoc state.cache evicted,
              Runtime_priority_map.dissoc state.priorities evicted )
    in
    {
      state with
      cache = Runtime_map.assoc cache key value;
      priorities = Runtime_priority_map.assoc priorities key tick;
      tick;
    }

let lru_evict state key =
  if lru_contains state key then
    {
      state with
      cache = Runtime_map.dissoc state.cache key;
      priorities = Runtime_priority_map.dissoc state.priorities key;
      tick = state.tick + 1;
    }
  else state

let lru_seed state cache =
  {
    state with
    cache;
    priorities = priorities_of_map cache;
    tick = 0;
  }
