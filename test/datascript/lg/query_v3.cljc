(ns ^:no-doc datascript.query-v3
  (:require
   [datascript.lg.query-types :as query-types]
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

(type-alias relation-hash-v3
  :Datascript_runtime.Query_value.row_hash<datascript.db/database-view>)

(type-record relation-state
  (symbols :vector<string>)
  (offset-map :map<string;int>)
  (rows :vector<array<datascript.lg.query-types/result>>))

(type-variant coll-relation-row-v3
  (CollQueryRowV3 :array<datascript.lg.query-types/result>)
  (CollDatomRowV3 :datascript.db/Datom))

(type-variant relation-v3
  (ArrayRelationV3 :datascript.query-v3/relation-state)
  (CollRelationV3 :datascript.query-v3/relation-state))

(type-record query-context-state-v3
  (rels :vector<datascript.query-v3/relation-v3>)
  (consts :map<string;datascript.lg.query-types/result>))

(type-variant query-context-v3
  EmptyContextV3
  (QueryContextV3 :datascript.query-v3/query-context-state-v3))

(def empty-context EmptyContextV3)

(defn ^query-context-v3 context-v3
  [^:vector<relation-v3> relations
   ^:map<string;datascript.lg.query-types/result> constants]
  (QueryContextV3
   (record query-context-state-v3
     (rels relations)
     (consts constants))))

(defn ^:bool context-empty? [^query-context-v3 context]
  (match context
    EmptyContextV3 true
    (QueryContextV3 _) false))

(defn ^:vector<relation-v3> context-relations
  [^query-context-v3 context]
  (match context
    EmptyContextV3 []
    (QueryContextV3 state) (:rels state)))

(defn ^:map<string;datascript.lg.query-types/result>
  context-constants
  [^query-context-v3 context]
  (match context
    EmptyContextV3 {}
    (QueryContextV3 state) (:consts state)))

(defn- ^query-context-v3 context-with-relations
  [^query-context-v3 context ^:vector<relation-v3> relations]
  (match context
    EmptyContextV3 EmptyContextV3
    (QueryContextV3 state)
    (context-v3 relations (:consts state))))

(defn- ^query-context-v3 context-with-constants
  [^query-context-v3 context
   ^:map<string;datascript.lg.query-types/result> constants]
  (match context
    EmptyContextV3 EmptyContextV3
    (QueryContextV3 state)
    (context-v3 (:rels state) constants)))

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
    (:symbols state)
    (CollRelationV3 state)
    (:symbols state)))

(defn- ^:map<string;int> relation-offset-map
  [^relation-v3 relation]
  (match relation
    (ArrayRelationV3 state)
    (:offset-map state)
    (CollRelationV3 state)
    (:offset-map state)))

(defn- ^:vector<array<datascript.lg.query-types/result>>
  relation-rows-closed
  [^relation-v3 relation]
  (match relation
    (ArrayRelationV3 state)
    (:rows state)
    (CollRelationV3 state)
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

(defn- ^relation-v3 make-coll-relation
  [^:vector<string> symbols
   ^:map<string;int> offset-map
   ^:vector<array<datascript.lg.query-types/result>> rows]
  (CollRelationV3
   (record relation-state
     (symbols symbols)
     (offset-map offset-map)
     (rows rows))))

(defn- ^relation-v3 make-relation-like
  [^relation-v3 relation
   ^:vector<string> symbols
   ^:map<string;int> offset-map
   ^:vector<array<datascript.lg.query-types/result>> rows]
  (match relation
    (ArrayRelationV3 _)
    (make-array-relation symbols offset-map rows)
    (CollRelationV3 _)
    (make-coll-relation symbols offset-map rows)))

(defn- ^:bool same-relation-kind?
  [^relation-v3 left ^relation-v3 right]
  (match left
    (ArrayRelationV3 _)
    (match right
      (ArrayRelationV3 _) true
      _ false)
    (CollRelationV3 _)
    (match right
      (CollRelationV3 _) true
      _ false)))

(extend-type relation-v3
  IRelation
  (-project [relation requested-symbols]
    (let [offset-map (relation-offset-map relation)
          projected-symbols
          (selected-symbols offset-map requested-symbols)]
      (make-relation-like
       relation
       projected-symbols
       (selected-offsets offset-map projected-symbols)
       (relation-tuples relation))))

  (-alter-coll [relation transform]
    (make-relation-like
     relation
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
    (if (same-relation-kind? relation other)
      (if (= (relation-offset-map relation)
             (relation-offset-map other))
        (make-relation-like
         relation
         (relation-symbols relation)
         (relation-offset-map relation)
         (into
          (relation-tuples relation)
          (relation-tuples other)))
        (Stdlib.invalid_arg
         "Cannot union relations with different symbols"))
      (Stdlib.invalid_arg
       "Cannot union relations with different kinds"))))

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

(defn- ^:array<datascript.lg.query-types/result> coll-row-array
  [^coll-relation-row-v3 row]
  (match row
    (CollQueryRowV3 values) values
    (CollDatomRowV3 datom) (query-types/datom-row datom)))

(defn ^relation-v3 coll-rel
  [^:vector<datascript.parser/pattern-element> pattern
   ^:vector<coll-relation-row-v3> rows]
  (let [offset-map
        (reduce-kv
         (fn [offsets index element]
           (if-some [symbol
                     (query-types/pattern-variable-name element)]
             (assoc offsets symbol index)
             offsets))
         {}
         pattern)]
    (make-coll-relation
     (vec (keys offset-map))
     offset-map
     (mapv coll-row-array rows))))

(defn ^relation-v3 singleton-rel []
  (array-rel [] [(to-array [])]))

(defn ^relation-v3 product
  [^relation-v3 left ^relation-v3 right]
  (let [left-symbols (-symbols left)
        right-symbols (-symbols right)
        left-indexes (-indexes left left-symbols)
        right-indexes (-indexes right right-symbols)
        rows
        (-fold
         left
         (fn [^:vector<array<datascript.lg.query-types/result>> rows
              ^:array<datascript.lg.query-types/result> left-row]
           (-fold
            right
            (fn [^:vector<array<datascript.lg.query-types/result>> rows
                 ^:array<datascript.lg.query-types/result> right-row]
              (conj
               rows
               (query-types/join-rows
                left-row
                left-indexes
                right-row
                right-indexes)))
            rows))
         [])]
    (array-rel
     (into left-symbols right-symbols)
     rows)))

(defn ^relation-v3 product-all
  [^:vector<relation-v3> relations]
  (reduce product relations))

(defn ^relation-hash-v3 hash-map-rel
  [^relation-v3 relation ^:vector<string> symbols]
  (Datascript_runtime.Query_value.row_hash
   (relation-tuples relation)
   (-indexes relation symbols)))

(defn- ^:vector<string> symbols-not-in
  [^:vector<string> excluded ^:vector<string> symbols]
  (let [excluded (set excluded)]
    (reduce
     (fn [remaining symbol]
       (if (contains? excluded symbol)
         remaining
         (conj remaining symbol)))
     []
     symbols)))

(defn ^relation-v3 hash-join
  [^relation-v3 left
   ^relation-hash-v3 left-hash
   ^:vector<string> join-symbols
   ^relation-v3 right]
  (let [left-symbols (-symbols left)
        right-symbols (-symbols right)
        keep-right-symbols
        (symbols-not-in left-symbols right-symbols)
        left-indexes (-indexes left left-symbols)
        right-indexes (-indexes right keep-right-symbols)
        right-key-indexes (-indexes right join-symbols)
        rows
        (reduce
         (fn [rows right-row]
           (if-some
             [left-rows
              (Datascript_runtime.Query_value.row_hash_find
               left-hash right-row right-key-indexes)]
             (reduce
              (fn [rows left-row]
                (conj
                 rows
                 (query-types/join-rows
                  left-row
                  left-indexes
                  right-row
                  right-indexes)))
              rows
              left-rows)
             rows))
         (empty (relation-tuples right))
         (relation-tuples right))]
    (array-rel
     (into left-symbols keep-right-symbols)
     rows)))

(defn- ^:map<string;datascript.lg.query-types/result>
  relation-constants
  [^relation-v3 relation]
  (if-some [row (first (relation-tuples relation))]
    (reduce
     (fn [constants symbol]
       (assoc constants symbol ((-getter relation symbol) row)))
     {}
     (-symbols relation))
    {}))

(defn- ^:bool relation-shares-symbols?
  [^relation-v3 relation ^:set<string> symbols]
  (some?
   (some
    (fn [symbol]
      (contains? symbols symbol))
    (-symbols relation))))

(defn ^:vector<relation-v3> related-rels
  [^query-context-v3 context ^:vector<string> symbols]
  (let [symbols (set symbols)]
    (filterv
     (fn [relation]
       (relation-shares-symbols? relation symbols))
     (context-relations context))))

(defn
  ^:tuple<option<vector<datascript.query-v3/relation-v3>>;datascript.query-v3/query-context-v3>
  extract-rels
  [^query-context-v3 context ^:vector<string> symbols]
  (let [symbols (set symbols)
        related
        (filterv
         (fn [relation]
           (relation-shares-symbols? relation symbols))
         (context-relations context))]
    (if (empty? related)
      (tuple None context)
      (tuple
       (Some related)
       (context-with-relations
        context
        (filterv
         (fn [relation]
           (not (relation-shares-symbols? relation symbols)))
         (context-relations context)))))))

(defn ^query-context-v3 join-unrelated
  [^query-context-v3 context ^relation-v3 relation]
  (if (context-empty? context)
    EmptyContextV3
    (case (-size relation)
      0 EmptyContextV3
      1
      (context-with-constants
       context
       (merge
        (context-constants context)
        (relation-constants relation)))
      (context-with-relations
       context
       (conj (context-relations context) relation)))))

(defn- ^:vector<string> shared-symbols
  [^:vector<string> left ^:vector<string> right]
  (let [right (set right)]
    (filterv
     (fn [symbol]
       (contains? right symbol))
     left)))

(defn ^query-context-v3 hash-join-rel
  [^query-context-v3 context ^relation-v3 relation]
  (if (or (context-empty? context)
          (= 0 (-size relation)))
    EmptyContextV3
    (match (extract-rels context (-symbols relation))
      (tuple None unchanged-context)
      (join-unrelated unchanged-context relation)
      (tuple (Some related) remaining-context)
      (let [related-relation (product-all related)
            join-symbols
            (shared-symbols
             (-symbols related-relation)
             (-symbols relation))
            relation-hash
            (hash-map-rel related-relation join-symbols)
            joined
            (hash-join
             related-relation
             relation-hash
             join-symbols
             relation)]
        (join-unrelated remaining-context joined)))))
