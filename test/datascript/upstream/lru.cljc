(ns ^:no-doc datascript.lru)

(type-record lru-state [key value]
  (key-value :map<key;value>)
  (gen-key :map<int;key>)
  (key-gen :map<key;int>)
  (gen :int)
  (limit :int))

(signature datascript.lru/cleanup-lru
  [key value]
  :fn<lru-state<key;value>;lru-state<key;value>>)

(signature datascript.lru/assoc-lru
  [key value]
  :fn<lru-state<key;value>;key;value;lru-state<key;value>>)

(signature datascript.lru/lru
  [key value]
  :fn<int;lru-state<key;value>>)

(signature datascript.lru/get-lru
  [key value]
  :fn<lru-state<key;value>;key;option<value>>)

(defn cleanup-lru [lru]
  (if (> (count (:key-value lru)) (:limit lru))
    (let [key-value (:key-value lru)
          gen-key   (:gen-key lru)
          key-gen   (:key-gen lru)
          gen       (:gen lru)
          limit     (:limit lru)
          [g k]     (first gen-key)]
      (record lru-state
        (key-value (dissoc key-value k))
        (gen-key (dissoc gen-key g))
        (key-gen (dissoc key-gen k))
        (gen gen)
        (limit limit)))
    lru))

(defn assoc-lru [lru k v]
  (let [key-value (:key-value lru)
        gen-key   (:gen-key lru)
        key-gen   (:key-gen lru)
        gen       (:gen lru)
        limit     (:limit lru)]
    (match (get key-gen k)
      (Some g)
      (record lru-state
        (key-value key-value)
        (gen-key (assoc (dissoc gen-key g) gen k))
        (key-gen (assoc key-gen k gen))
        (gen (inc gen))
        (limit limit))
      None
      (cleanup-lru
        (record lru-state
          (key-value (assoc key-value k v))
          (gen-key (assoc gen-key gen k))
          (key-gen (assoc key-gen k gen))
          (gen (inc gen))
          (limit limit))))))

(defn lru [limit]
  (record lru-state
    (key-value {})
    (gen-key (sorted-map))
    (key-gen {})
    (gen 0)
    (limit limit)))

(defn get-lru [lru key]
  (get (:key-value lru) key))

(type-record cache-state [key value]
  (impl :ref<lru-state<key;value>>))

(signature datascript.lru/cache
  [key value]
  :fn<int;cache-state<key;value>>)

(signature datascript.lru/-get
  [key value]
  :fn<cache-state<key;value>;key;fn<value>;value>)

(defn cache [limit]
  (record cache-state
    (impl (volatile! (lru limit)))))

(defn -get [cache key compute-fn]
  (let [*impl (:impl cache)]
    (if-some [cached (get-lru @*impl key)]
      (do (vreset! *impl (assoc-lru @*impl key cached))
        cached)
      (let [computed (compute-fn)]
        (vreset! *impl (assoc-lru @*impl key computed))
        computed))))
