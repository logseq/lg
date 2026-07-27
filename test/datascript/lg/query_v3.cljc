(ns ^:no-doc datascript.query-v3
  (:require
   [datascript.built-ins :as built-ins]
   [datascript.db :as db]
   [datascript.lg.query-types :as query-types]
   [datascript.parser :as parser]
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
  (consts :map<string;datascript.lg.query-types/result>)
  (sources :map<string;datascript.lg.query-types/source>)
  (default-source-symbol :string))

(type-variant query-context-v3
  EmptyContextV3
  (QueryContextV3 :datascript.query-v3/query-context-state-v3))

(type-variant predicate-function-v3
  (ComparisonPredicateV3
   :string
   :datascript.built-ins/query-function)
  (PurePredicateV3
   :string
   :datascript.built-ins/query-function)
  (VariablePredicateV3
   :string
   :datascript.lg.query-types/callable))

(def empty-context EmptyContextV3)

(defn ^query-context-v3 context-v3
  ([^:vector<relation-v3> relations
    ^:map<string;datascript.lg.query-types/result> constants]
   (context-v3 relations constants {}))
  ([^:vector<relation-v3> relations
    ^:map<string;datascript.lg.query-types/result> constants
    ^:map<string;datascript.lg.query-types/source> sources]
   (context-v3 relations constants sources "$"))
  ([^:vector<relation-v3> relations
    ^:map<string;datascript.lg.query-types/result> constants
    ^:map<string;datascript.lg.query-types/source> sources
    ^:string default-source-symbol]
   (QueryContextV3
    (record query-context-state-v3
      (rels relations)
      (consts constants)
      (sources sources)
      (default-source-symbol default-source-symbol)))))

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

(defn ^:map<string;datascript.lg.query-types/source>
  context-sources
  [^query-context-v3 context]
  (match context
    EmptyContextV3 {}
    (QueryContextV3 state) (:sources state)))

(defn ^:string context-default-source-symbol
  [^query-context-v3 context]
  (match context
    EmptyContextV3 "$"
    (QueryContextV3 state) (:default-source-symbol state)))

(defn- ^query-context-v3 context-with-relations
  [^query-context-v3 context ^:vector<relation-v3> relations]
  (match context
    EmptyContextV3 EmptyContextV3
    (QueryContextV3 state)
    (context-v3
     relations
     (:consts state)
     (:sources state)
     (:default-source-symbol state))))

(defn- ^query-context-v3 context-with-constants
  [^query-context-v3 context
   ^:map<string;datascript.lg.query-types/result> constants]
  (match context
    EmptyContextV3 EmptyContextV3
    (QueryContextV3 state)
    (context-v3
     (:rels state)
     constants
     (:sources state)
     (:default-source-symbol state))))

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

(defn ^:option<relation-v3> project-rel
  [^relation-v3 relation ^:vector<string> symbols]
  (let [relation-symbols (-symbols relation)
        requested (set symbols)]
    (if
     (every?
      (fn [symbol]
        (contains? requested symbol))
      relation-symbols)
      (Some relation)
      (if
       (some?
        (some
         (fn [symbol]
           (contains? requested symbol))
         relation-symbols))
        (Some (-project relation symbols))
        None))))

(defn ^query-context-v3 project-context
  [^query-context-v3 context ^:vector<string> symbols]
  (match context
    EmptyContextV3 EmptyContextV3
    (QueryContextV3 state)
    (let [requested (set symbols)
          constants
          (reduce-kv
           (fn [selected symbol value]
             (if (contains? requested symbol)
               (assoc selected symbol value)
               selected))
           {}
           (:consts state))
          relations
          (reduce
           (fn [selected relation]
             (match (project-rel relation symbols)
               None selected
               (Some projected) (conj selected projected)))
           []
           (:rels state))]
      (context-v3
       relations
       constants
       (:sources state)
       (:default-source-symbol state)))))

(defn- ^:tuple<datascript.parser/query-source;vector<datascript.parser/pattern-element>>
  pattern-clause-parts
  [^datascript.parser/clause clause]
  (match clause
    (parser/PatternClause source pattern)
    (tuple source pattern)
    _
    (Stdlib.invalid_arg "Expected a DataScript pattern clause")))

(defn ^datascript.lg.query-types/source get-source
  [^query-context-v3 context
   ^datascript.parser/query-source query-source]
  (let [source-name
        (if-some [source-name
                  (parser/query-source-name query-source)]
          source-name
          (context-default-source-symbol context))]
    (if-some [source (get (context-sources context) source-name)]
      source
      (Stdlib.invalid_arg
       (str "Source " source-name " is not defined")))))

(defn- ^relation-v3 empty-pattern-relation
  [^:vector<datascript.parser/pattern-element> pattern]
  (coll-rel pattern []))

(defn- ^relation-v3 resolve-pattern-db-closed
  [^datascript.db/database-view database
   ^:vector<datascript.parser/pattern-element> pattern]
  (if (or (empty? pattern) (> (count pattern) 5))
    (Stdlib.invalid_arg
     "DataScript patterns must contain one to five elements")
    (match
     (tuple
      (query-types/pattern-entity-constraint
       database
       (query-types/pattern-element-at pattern 0))
      (query-types/pattern-attr-constraint
       (query-types/pattern-element-at pattern 1))
      (query-types/pattern-value-constraint
       (query-types/pattern-element-at pattern 2))
      (query-types/pattern-entity-constraint
       database
       (query-types/pattern-element-at pattern 3))
      (query-types/pattern-added-constraint
       (query-types/pattern-element-at pattern 4)))
      (tuple
       (Some entity)
       (Some attr)
       (Some value)
       (Some tx)
       (Some added))
      (if-some
        [resolved-value
         (query-types/resolve-pattern-value-constraint
          database attr value)]
        (let [datoms
              (db/database-view-search-vector
               database entity attr resolved-value tx)
              datoms
              (if-some [added added]
                (filterv
                 (fn [datom]
                   (= added (db/datom-added datom)))
                 datoms)
                datoms)]
          (coll-rel
           pattern
           (mapv
            (fn [datom]
              (CollDatomRowV3 datom))
            datoms)))
        (empty-pattern-relation pattern))
      _
      (empty-pattern-relation pattern))))

(defn ^relation-v3 resolve-pattern-db
  [^datascript.db/database-view database
   ^datascript.parser/clause clause]
  (resolve-pattern-db-closed
   database
   (tuple-get (pattern-clause-parts clause) 1)))

(defn- ^:bool pattern-result-equals?
  [^datascript.lg.query-types/result result
   ^:Datascript_runtime.Data_value.t constant]
  (let [value
        (match result
          (Datascript_runtime.Query_value.Entity entity)
          (Some (Datascript_runtime.Data_value.Int entity))
          (Datascript_runtime.Query_value.Attr attr)
          (Some (Datascript_runtime.Data_value.Keyword attr))
          (Datascript_runtime.Query_value.Value value)
          (Some value)
          (Datascript_runtime.Query_value.Pull value)
          (Some value)
          (Datascript_runtime.Query_value.Added added)
          (Some
           (Datascript_runtime.Data_value.Keyword
            (if added ":db/add" ":db/retract")))
          _ None)]
    (if-some [value value]
      (Datascript_runtime.Data_value.equal value constant)
      false)))

(defn- ^:bool row-matches-pattern-constants?
  [^:array<datascript.lg.query-types/result> row
   ^:vector<datascript.parser/pattern-element> pattern]
  (reduce-kv
   (fn [matches index element]
     (if matches
       (if-some [constant
                 (parser/pattern-element-constant element)]
         (pattern-result-equals? (aget row index) constant)
         true)
       false))
   true
   pattern))

(defn- ^relation-v3 resolve-pattern-coll-closed
  [^datascript.lg.query-types/source source
   ^:vector<datascript.parser/pattern-element> pattern]
  (if-some [rows (query-types/source-rows source)]
    (coll-rel
     pattern
     (mapv
      (fn [row]
        (CollQueryRowV3 row))
      (filterv
       (fn [row]
         (row-matches-pattern-constants? row pattern))
       rows)))
    (Stdlib.invalid_arg
     "Cannot match a DataScript database source as a collection")))

(defn ^relation-v3 resolve-pattern-coll
  [^datascript.lg.query-types/source source
   ^datascript.parser/clause clause]
  (resolve-pattern-coll-closed
   source
   (tuple-get (pattern-clause-parts clause) 1)))

(defn- ^:vector<datascript.parser/pattern-element>
  substitute-context-pattern
  [^:map<string;datascript.lg.query-types/result> constants
   ^:vector<datascript.parser/pattern-element> pattern]
  (mapv
   (fn [element]
     (if-some [variable
               (query-types/pattern-variable-name element)]
       (if-some [value (get constants variable)]
         (parser/pattern-constant
          (query-types/result-pattern-value value))
         element)
       element))
   pattern))

(defn ^query-context-v3 resolve-pattern
  [^query-context-v3 context ^datascript.parser/clause clause]
  (let [parts (pattern-clause-parts clause)
        query-source (tuple-get parts 0)
        pattern
        (substitute-context-pattern
         (context-constants context)
         (tuple-get parts 1))
        source (get-source context query-source)
        relation
        (if-some [database (query-types/source-database source)]
          (resolve-pattern-db-closed database pattern)
          (resolve-pattern-coll-closed source pattern))]
    (hash-join-rel context relation)))

(defn-
  ^:tuple<datascript.parser/query-callable;vector<datascript.parser/fn-arg>>
  predicate-clause-parts
  [^datascript.parser/clause clause]
  (match clause
    (parser/PredicateClause callable arguments)
    (tuple callable arguments)
    _
    (Stdlib.invalid_arg "Expected a DataScript predicate clause")))

(defn ^predicate-function-v3 get-f
  [^query-context-v3 context
   ^datascript.parser/query-callable callable
   ^:string _form]
  (if-some [name (parser/static-callable-name callable)]
    (if-some [function (built-ins/comparison-function name)]
      (ComparisonPredicateV3 name function)
      (if-some [function (built-ins/pure-function name)]
        (PurePredicateV3 name function)
        (Stdlib.invalid_arg
         (str "Unknown built-in " name))))
    (if-some [variable (parser/variable-callable-name callable)]
      (if-some [result (get (context-constants context) variable)]
        (if-some [callable (query-types/result-callable result)]
          (VariablePredicateV3 variable callable)
          (Stdlib.invalid_arg
           (str "Query input is not a typed callable: " variable)))
        (Stdlib.invalid_arg
         (str "Unknown function " variable)))
      (Stdlib.invalid_arg "Predicate function is missing"))))

(defn- ^datascript.lg.query-types/result predicate-source-result
  [^query-context-v3 context ^:string source-name]
  (if-some [source (get (context-sources context) source-name)]
    (if-some [database (query-types/source-database source)]
      (query-types/database-result database)
      (Stdlib.invalid_arg
       (str "Predicate source is not a database: " source-name)))
    (Stdlib.invalid_arg
     (str "Unbound source variable: " source-name))))

(defn collect-args!
  [^query-context-v3 context
   ^:vector<datascript.parser/fn-arg> arguments
   ^:array<option<datascript.lg.query-types/result>> target
   ^:string _form]
  (reduce-kv
   (fn [_ index argument]
     (if-some [variable (parser/argument-variable-name argument)]
       (if-some [value (get (context-constants context) variable)]
         (aset target index (Some value))
         (Stdlib.ignore 0))
       (if-some [value (parser/argument-constant argument)]
         (aset target index (Some (query-types/value-result value)))
         (if-some [source-name
                   (parser/argument-source-name argument)]
           (aset
            target
            index
            (Some (predicate-source-result context source-name)))
           (Stdlib.invalid_arg "Invalid predicate argument"))))
     (Stdlib.ignore 0))
   (Stdlib.ignore 0)
   arguments))

(defn- ^:vector<tuple<string;int>> predicate-row-bindings
  [^query-context-v3 context
   ^:vector<datascript.parser/fn-arg> arguments]
  (reduce-kv
   (fn [bindings index argument]
     (if-some [variable (parser/argument-variable-name argument)]
       (if (contains? (context-constants context) variable)
         bindings
         (conj bindings (tuple variable index)))
       bindings))
   []
   arguments))

(defn- ^:bool context-symbol-bound?
  [^query-context-v3 context ^:string symbol]
  (or
   (contains? (context-constants context) symbol)
   (contains? (context-sources context) symbol)
   (some?
    (some
     (fn [relation]
       (contains? (relation-offset-map relation) symbol))
     (context-relations context)))))

(defn- ^:string query-symbol-set-description
  [^:vector<string> symbols]
  (str
   "#{"
   (reduce-kv
    (fn [description index symbol]
      (if (= index 0)
        symbol
        (str description " " symbol)))
    ""
    symbols)
   "}"))

(defn check-bound
  [^query-context-v3 context
   ^:vector<string> symbols
   ^:string form]
  (let [missing
        (reduce
         (fn [missing symbol]
           (if
            (or
             (context-symbol-bound? context symbol)
             (some?
              (some
               (fn [missing-symbol]
                 (= missing-symbol symbol))
               missing)))
             missing
             (conj missing symbol)))
         []
         symbols)]
    (if (empty? missing)
      (Stdlib.ignore 0)
      (Stdlib.invalid_arg
       (str
        "Insufficient bindings: "
        (query-symbol-set-description missing)
        (if (= form "")
          ""
          (str " not bound in " form)))))))

(defn- check-predicate-bindings
  [^query-context-v3 context ^:vector<string> variables]
  (check-bound context variables ""))

(defn- ^:vector<datascript.lg.query-types/result>
  collected-predicate-arguments
  [^:array<option<datascript.lg.query-types/result>> arguments]
  (mapv
   (fn [argument]
     (match argument
       (Some value) value
       None
       (Stdlib.invalid_arg "Predicate argument is not bound")))
   arguments))

(defn- ^:bool data-value-truthy?
  [^:Datascript_runtime.Data_value.t value]
  (if (Datascript_runtime.Data_value.is_nil value)
    false
    (match (Datascript_runtime.Data_value.bool_value value)
      (Some value) value
      None true)))

(defn- ^:bool invoke-predicate
  [^predicate-function-v3 function
   ^:vector<datascript.lg.query-types/result> arguments]
  (match function
    (ComparisonPredicateV3 name function)
    (if (built-ins/missing-function? function)
      (if (= 3 (count arguments))
        (query-types/query-missing?
         (query-types/query-database-result (nth arguments 0))
         (nth arguments 1)
         (nth arguments 2))
        (Stdlib.invalid_arg
         "Invalid arguments for query predicate: missing?"))
      (if-some
        [matches?
         (built-ins/apply-comparison
          function
          (mapv query-types/result-pattern-value arguments))]
        matches?
        (Stdlib.invalid_arg
         (str "Invalid arguments for query predicate: " name))))
    (PurePredicateV3 name function)
    (if-some
      [value
       (built-ins/apply-pure-function
        function
        (mapv query-types/result-pattern-value arguments))]
      (data-value-truthy? value)
      (Stdlib.invalid_arg
       (str "Invalid arguments for query predicate: " name)))
    (VariablePredicateV3 _name callable)
    (if-some [value (query-types/invoke-callable callable arguments)]
      (data-value-truthy? value)
      false)))

(defn- fill-predicate-row!
  [^relation-v3 relation
   ^:array<datascript.lg.query-types/result> row
   ^:vector<tuple<string;int>> bindings
   ^:array<option<datascript.lg.query-types/result>> target]
  (reduce
   (fn [_ binding]
     (let [variable (tuple-get binding 0)
           index (tuple-get binding 1)]
       (aset
        target
        index
        (Some ((-getter relation variable) row))))
     (Stdlib.ignore 0))
   (Stdlib.ignore 0)
   bindings))

(defn- ^relation-v3 filter-predicate-relation
  [^relation-v3 relation
   ^predicate-function-v3 function
   ^:vector<tuple<string;int>> bindings
   ^:array<option<datascript.lg.query-types/result>> target]
  (-alter-coll
   relation
   (fn [^:vector<array<datascript.lg.query-types/result>> rows]
     (filterv
      (fn [^:array<datascript.lg.query-types/result> row]
        (let [_ (fill-predicate-row!
                 relation row bindings target)]
          (invoke-predicate
           function
           (collected-predicate-arguments target))))
      rows))))

(defn ^query-context-v3 resolve-predicate
  [^query-context-v3 context ^datascript.parser/clause clause]
  (let [parts (predicate-clause-parts clause)
        callable (tuple-get parts 0)
        arguments (tuple-get parts 1)
        form ""
        function (get-f context callable form)
        target
        (to-array
         (mapv
          (fn [_argument] None)
          arguments))
        _ (collect-args! context arguments target form)
        bindings (predicate-row-bindings context arguments)
        variables (mapv (fn [binding] (tuple-get binding 0)) bindings)
        _ (check-predicate-bindings context variables)]
    (if (empty? variables)
      (if
       (invoke-predicate
        function
        (collected-predicate-arguments target))
        context
        EmptyContextV3)
      (match (extract-rels context variables)
        (tuple None _)
        (Stdlib.invalid_arg "Predicate relations are not bound")
        (tuple (Some relations) remaining-context)
        (let [relation
              (if (= 1 (count relations))
                (nth relations 0)
                (product-all relations))
              filtered
              (filter-predicate-relation
               relation function bindings target)]
          (join-unrelated remaining-context filtered))))))

(defn- ^query-context-v3 context-with-default-source
  [^query-context-v3 context ^:string source-name]
  (match context
    EmptyContextV3 EmptyContextV3
    (QueryContextV3 state)
    (context-v3
     (:rels state)
     (:consts state)
     (:sources state)
     source-name)))

(defn- ^:option<datascript.parser/query-source> clause-query-source
  [^datascript.parser/clause clause]
  (match clause
    (parser/PatternClause source _) (Some source)
    (parser/RuleClause source _ _) (Some source)
    (parser/NotClause source _ _ _) (Some source)
    (parser/OrClause source _ _ _ _) (Some source)
    _ None))

(defn ^query-context-v3 upd-default-source
  [^query-context-v3 context ^datascript.parser/clause clause]
  (if-some [source (clause-query-source clause)]
    (if-some [source-name (parser/query-source-name source)]
      (context-with-default-source context source-name)
      context)
    context))
