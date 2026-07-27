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

(type-alias relation-row
  :array<datascript.lg.query-types/result>)

(type-alias relation-transform
  :fn<vector<array<datascript.lg.query-types/result>>;vector<array<datascript.lg.query-types/result>>>)

(type-record relation-state
  (symbols :vector<string>)
  (offset-map :map<string;int>)
  (rows :vector<array<datascript.lg.query-types/result>>))

(type-variant relation-v3
  (ArrayRelationV3 :datascript.query-v3/relation-state))

(defprotocol IRelation
  (-project
   [relation ^:vector<string> symbols]
   :datascript.query-v3/relation-v3)
  (-alter-coll
   [relation
    ^datascript.query-v3/relation-transform
    transform]
   :datascript.query-v3/relation-v3)
  (-symbols [relation] :vector<string>)
  (-arity [relation] :int)
  (-fold [relation combine initial])
  (-size [relation] :int)
  (-getter
   [relation ^:string symbol]
   :fn<array<datascript.lg.query-types/result>;datascript.lg.query-types/result>)
  (-indexes
   [relation ^:vector<string> symbols]
   :array<int>)
  (-copy-tuple
   [relation
    ^:array<datascript.lg.query-types/result> tuple
    ^:array<int> indexes
    ^:array<datascript.lg.query-types/result> target
    ^:array<int> target-indexes]
   :unit)
  (-union
   [relation ^datascript.query-v3/relation-v3 other]
   :datascript.query-v3/relation-v3))

(defn- ^:vector<string> selected-symbols
  [^:map<string;int> offset-map
   ^:vector<string> symbols]
  (reduce
   (fn [selected symbol]
     (if (contains? offset-map symbol)
       (conj selected symbol)
       selected))
   []
   symbols))

(defn- ^:map<string;int> selected-offsets
  [^:map<string;int> offset-map
   ^:vector<string> symbols]
  (reduce
   (fn [selected symbol]
     (if-some [index (get offset-map symbol)]
       (assoc selected symbol index)
       selected))
   {}
   symbols))

(defn- ^:vector<string> relation-symbols
  [^relation-v3 relation]
  (match relation
    (ArrayRelationV3 state)
    (:symbols state)))

(defn- ^:map<string;int> relation-offset-map
  [^relation-v3 relation]
  (match relation
    (ArrayRelationV3 state)
    (:offset-map state)))

(defn- ^:vector<array<datascript.lg.query-types/result>>
  relation-rows-closed
  [^relation-v3 relation]
  (match relation
    (ArrayRelationV3 state)
    (:rows state)))

(defn- ^:vector<array<datascript.lg.query-types/result>>
  relation-tuples
  [^relation-v3 relation]
  (relation-rows-closed relation))

(defn- ^relation-v3 make-array-relation
  [^:vector<string> symbols
   ^:map<string;int> offset-map
   ^:vector<array<datascript.lg.query-types/result>> rows]
  (ArrayRelationV3
   (record relation-state
     (symbols symbols)
     (offset-map offset-map)
     (rows rows))))

(extend-type relation-v3
  IRelation
  (-project [relation requested-symbols]
    (let [offset-map (relation-offset-map relation)
          projected-symbols
          (selected-symbols offset-map requested-symbols)]
      (make-array-relation
       projected-symbols
       (selected-offsets offset-map projected-symbols)
       (relation-tuples relation))))

  (-alter-coll [relation transform]
    (make-array-relation
     (relation-symbols relation)
     (relation-offset-map relation)
     (transform (relation-tuples relation))))

  (-symbols [relation]
    (relation-symbols relation))

  (-arity [relation]
    (count (relation-offset-map relation)))

  (-fold [relation combine initial]
    (reduce combine initial (relation-rows-closed relation)))

  (-size [relation]
    (count (relation-rows-closed relation)))

  (-getter [relation symbol]
    (if-some [index (get (relation-offset-map relation) symbol)]
      (fn [tuple]
        (aget tuple index))
      (Stdlib.invalid_arg
       (str "Unknown relation symbol " symbol))))

  (-indexes [relation requested-symbols]
    (to-array
     (mapv
      (fn [symbol]
        (if-some [index
                  (get (relation-offset-map relation) symbol)]
          index
          (Stdlib.invalid_arg
           (str "Unknown relation symbol " symbol))))
      requested-symbols)))

  (-copy-tuple [_relation tuple indexes target target-indexes]
    (dotimes [index (alength indexes)]
      (aset
       target
       (aget target-indexes index)
       (aget tuple (aget indexes index))))
    (Stdlib.ignore 0))

  (-union [relation other]
    (if (= (relation-offset-map relation)
           (relation-offset-map other))
      (make-array-relation
       (relation-symbols relation)
       (relation-offset-map relation)
       (into
        (relation-tuples relation)
        (relation-tuples other)))
      (Stdlib.invalid_arg
       "Cannot union relations with different symbols"))))

(defn ^relation-v3 array-rel
  [^:vector<string> symbols
   ^:vector<array<datascript.lg.query-types/result>> rows]
  (make-array-relation
   symbols
   (reduce-kv
    (fn [offsets index symbol]
      (assoc offsets symbol index))
    {}
    symbols)
   rows))

(defn ^relation-v3 singleton-rel []
  (array-rel [] [(to-array [])]))
