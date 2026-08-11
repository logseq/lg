(ns ^:no-doc datascript.lru)

(type-record lru-state [key value]
  (key-value :map<key;value>)
  (gen-key :clojure.core/persistent-tree-map<int;key>)
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

(signature datascript.lru/get-lru-default
  [key value]
  :fn<lru-state<key;value>;key;value;value>)

(signature datascript.lru/make-lru-state
  [key value]
  :fn<map<key;value>;clojure.core/persistent-tree-map<int;key>;map<key;int>;int;int;lru-state<key;value>>)

(defn make-lru-state [key-value gen-key key-gen gen limit]
  (record lru-state
    (key-value key-value)
    (gen-key gen-key)
    (key-gen key-gen)
    (gen gen)
    (limit limit)))

(defn cleanup-lru [lru]
  (if (> (count (:key-value lru)) (:limit lru))
    (let [key-value (:key-value lru)
          gen-key   (:gen-key lru)
          key-gen   (:key-gen lru)
          gen       (:gen lru)
          limit     (:limit lru)
          entry     (first gen-key)]
      (if-some [present entry]
        (let [[g k] present]
          (make-lru-state
           (dissoc key-value k)
           (dissoc gen-key g)
           (dissoc key-gen k)
           gen
           limit))
        lru))
    lru))

(defn assoc-lru [lru k v]
  (let [key-value (:key-value lru)
        gen-key   (:gen-key lru)
        key-gen   (:key-gen lru)
        gen       (:gen lru)
        limit     (:limit lru)]
    (match (get key-gen k)
      (Some g)
      (make-lru-state
       key-value
       (assoc (dissoc gen-key g) gen k)
       (assoc key-gen k gen)
       (inc gen)
       limit)
      None
      (cleanup-lru
        (make-lru-state
         (assoc key-value k v)
         (assoc gen-key gen k)
         (assoc key-gen k gen)
         (inc gen)
         limit)))))

(defn lru [limit]
  (record lru-state
    (key-value {})
    (gen-key (sorted-map))
    (key-gen {})
    (gen 0)
    (limit limit)))

(defn get-lru [lru key]
  (get (:key-value lru) key))

(defn get-lru-default [lru key not-found]
  (get (:key-value lru) key not-found))

(type-record cache-state [key value]
  (impl :ref<lru-state<key;value>>))

(signature datascript.lru/-get
  [key value]
  :fn<cache-state<key;value>;key;fn<unit;value>;value>)

(defprotocol ICache
  (-get [this key compute-fn]))

(signature datascript.lru/cache
  [key value]
  :fn<int;cache-state<key;value>>)

(defn cache [limit]
  (record cache-state
    (impl (volatile! (lru limit)))))

(extend-type cache-state
  ICache
  (-get [cache key compute-fn]
    (let [*impl (:impl cache)]
      (if-some [cached (get-lru @*impl key)]
        (do
          (vreset! *impl (assoc-lru @*impl key cached))
          cached)
        (let [computed (compute-fn)]
          (vreset! *impl (assoc-lru @*impl key computed))
          computed)))))
