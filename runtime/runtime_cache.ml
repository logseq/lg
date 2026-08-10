type ('key, 'value) basic = ('key, 'value) Runtime_map.t

type ('key, 'value) lru = {
  cache : ('key, 'value) Runtime_map.t;
  priorities : 'key Runtime_priority_map.t;
  tick : int;
  limit : int;
}

type ('key, 'value) ttl = {
  cache : ('key, 'value) Runtime_map.t;
  timestamps : ('key, float) Runtime_map.t;
  ttl_ms : int;
}

let basic_of_map map = map
let basic_to_map cache = cache
let basic_lookup cache key = Runtime_map.lookup cache key

let basic_lookup_default cache key not_found =
  Runtime_map.lookup_default cache key not_found

let basic_contains cache key = Runtime_map.contains_key cache key
let basic_miss cache key value = Runtime_map.assoc cache key value
let basic_evict cache key = Runtime_map.dissoc cache key

let timestamps_of_map now cache =
  Runtime_map.kv_reduce
    (fun timestamps key _ -> Runtime_map.assoc timestamps key now)
    Runtime_map.empty cache

let ttl_of_map ttl_ms now cache =
  if ttl_ms < 0 then invalid_arg "TTL cache duration must be non-negative"
  else { cache; timestamps = timestamps_of_map now cache; ttl_ms }

let ttl_to_map (state : ('key, 'value) ttl) = state.cache

let ttl_expired state key now =
  match Runtime_map.lookup state.timestamps key with
  | None -> true
  | Some timestamp -> now -. timestamp >= float_of_int state.ttl_ms

let ttl_contains state key now =
  Runtime_map.contains_key state.cache key && not (ttl_expired state key now)

let ttl_lookup state key now =
  if ttl_contains state key now then Runtime_map.lookup state.cache key else None

let ttl_lookup_default state key not_found now =
  ttl_lookup state key now |> Option.value ~default:not_found

let remove_expired state now =
  Runtime_map.kv_reduce
    (fun (cache, timestamps) key timestamp ->
      if now -. timestamp >= float_of_int state.ttl_ms then
        (Runtime_map.dissoc cache key, Runtime_map.dissoc timestamps key)
      else (cache, timestamps))
    (state.cache, state.timestamps) state.timestamps

let ttl_miss state key value now =
  let cache, timestamps = remove_expired state now in
  {
    state with
    cache = Runtime_map.assoc cache key value;
    timestamps = Runtime_map.assoc timestamps key now;
  }

let ttl_evict state key =
  {
    state with
    cache = Runtime_map.dissoc state.cache key;
    timestamps = Runtime_map.dissoc state.timestamps key;
  }

let ttl_seed state now cache =
  {
    state with
    cache;
    timestamps = timestamps_of_map now cache;
  }

let priorities_of_map cache =
  Runtime_map.kv_reduce
    (fun priorities key _ -> Runtime_priority_map.assoc priorities key 0)
    Runtime_priority_map.empty cache

let lru_of_map limit cache =
  if limit <= 0 then invalid_arg "LRU cache threshold must be positive"
  else { cache; priorities = priorities_of_map cache; tick = 0; limit }

let lru_to_map (state : ('key, 'value) lru) = state.cache
let lru_lookup (state : ('key, 'value) lru) key =
  Runtime_map.lookup state.cache key

let lru_lookup_default (state : ('key, 'value) lru) key not_found =
  Runtime_map.lookup_default state.cache key not_found

let lru_contains (state : ('key, 'value) lru) key =
  Runtime_map.contains_key state.cache key

let lru_hit (state : ('key, 'value) lru) key =
  let tick = state.tick + 1 in
  let priorities =
    if lru_contains state key then
      Runtime_priority_map.assoc state.priorities key tick
    else state.priorities
  in
  { state with priorities; tick }

let least_recent_key priorities =
  Runtime_priority_map.peek priorities |> Option.map fst

let lru_miss (state : ('key, 'value) lru) key value =
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

let lru_evict (state : ('key, 'value) lru) key =
  if lru_contains state key then
    {
      state with
      cache = Runtime_map.dissoc state.cache key;
      priorities = Runtime_priority_map.dissoc state.priorities key;
      tick = state.tick + 1;
    }
  else state

let lru_seed (state : ('key, 'value) lru) cache =
  {
    state with
    cache;
    priorities = priorities_of_map cache;
    tick = 0;
  }
