(ns ^:no-doc datascript.query-v3
  (:require
   [me.tonsky.persistent-sorted-set.arrays :as da]))

(def ^:const lru-cache-size 100)

(signature datascript.query-v3/mapa [input output]
  :fn<fn<input;output>;vector<input>;array<output>>)

(defn mapa [f coll]
  (to-array (map f coll)))

(signature datascript.query-v3/arange
  :fn<int;int;array<int>>)

(defn arange [start end]
  (to-array (range start end)))

(signature datascript.query-v3/subarr [value]
  :fn<array<value>;int;int;array<value>>)

(defn subarr [arr start end]
  (da/aslice arr start end))

(signature datascript.query-v3/concat-two
  :fn<vector<Datascript_runtime.Data_value.t>;vector<Datascript_runtime.Data_value.t>;vector<Datascript_runtime.Data_value.t>>)

(defn- concat-two [left right]
  (into left right))

(signature datascript.query-v3/concatv-closed
  :fn<vector<vector<Datascript_runtime.Data_value.t>>;vector<Datascript_runtime.Data_value.t>>)

(defn- concatv-closed [xs]
  (reduce concat-two [] xs))

(defn concatv
  {:inline
   (fn [& xs]
     (list
      'datascript.query-v3/concatv-closed
      (vec xs)))}
  [& ^:list<vector<Datascript_runtime.Data_value.t>> xs]
  (concatv-closed (vec xs)))

(signature datascript.query-v3/zip-pair
  :fn<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t;vector<Datascript_runtime.Data_value.t>>)

(defn- zip-pair [left right]
  [left right])

(signature datascript.query-v3/zip-append
  :fn<vector<Datascript_runtime.Data_value.t>;Datascript_runtime.Data_value.t;vector<Datascript_runtime.Data_value.t>>)

(defn- zip-append [row value]
  (conj row value))

(signature datascript.query-v3/zip-two
  :fn<vector<Datascript_runtime.Data_value.t>;vector<Datascript_runtime.Data_value.t>;seq<vector<Datascript_runtime.Data_value.t>>>)

(defn- zip-two [left right]
  (map zip-pair left right))

(signature datascript.query-v3/zip-many
  :fn<vector<vector<Datascript_runtime.Data_value.t>>;seq<vector<Datascript_runtime.Data_value.t>>>)

(defn- zip-many [collections]
  (loop [rows
         (zip-two
          (nth collections 0)
          (nth collections 1))
         remaining (drop 2 collections)]
    (if-some [values (first remaining)]
      (recur
       (map zip-append rows values)
       (next remaining))
      rows)))

(defn zip
  {:inline
   (fn [a b & rest]
     (if (empty? rest)
       (list 'datascript.query-v3/zip-two a b)
       (list
        'datascript.query-v3/zip-many
        (vec (cons a (cons b rest))))))}
  ([^:vector<Datascript_runtime.Data_value.t> a
    ^:vector<Datascript_runtime.Data_value.t> b]
   (zip-two a b))
  ([^:vector<Datascript_runtime.Data_value.t> a
    ^:vector<Datascript_runtime.Data_value.t> b
    & ^:list<vector<Datascript_runtime.Data_value.t>> rest]
   (zip-many (into [a b] rest))))

(signature datascript.query-v3/has? [value]
  :fn<vector<value>;value;option<bool>>)

(defn has? [coll el]
  (some #(= el %) coll))
