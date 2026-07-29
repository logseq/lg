(ns ^:no-doc datascript.query-v3
  (:require
   [clojure.string :as string]
   [datascript.built-ins :as built-ins]
   [datascript.db :as db]
   [datascript.lg.query-types :as query-types]
   [datascript.lru :as lru]
   [datascript.parser :as parser]
   [me.tonsky.persistent-sorted-set.arrays :as da]))

(def ^:const lru-cache-size 100)

(type-alias query-cache-state-v3
  :datascript.lru/cache-state<Datascript_runtime.Data_value.t;datascript.parser/Query>)

(signature datascript.query-v3/query-cache
  :datascript.query-v3/query-cache-state-v3)

(def ^query-cache-state-v3 query-cache
  (lru/cache lru-cache-size))

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

(defn concatv
  {:inline
   (fn [& xs]
     (reduce
      (fn [result input]
        (list 'clojure.core/into result input))
      []
      xs))}
  [& ^:list<vector<Datascript_runtime.Data_value.t>> xs]
  (into [] cat xs))

(signature datascript.query-v3/zip-two
  :fn<vector<Datascript_runtime.Data_value.t>;vector<Datascript_runtime.Data_value.t>;seq<vector<Datascript_runtime.Data_value.t>>>)

(defn- zip-two [left right]
  (map (fn [left right] (conj [left] right)) left right))

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
       (map (fn [row value] (conj row value)) rows values)
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

(defprotocol NativeColl
  (-native-coll [collection]))

(defn native-coll [collection]
  (if (satisfies? NativeColl collection)
    (NativeColl/-native-coll collection)
    collection))

(signature datascript.query-v3/fast-map [key value]
  :fn<map<key;value>>)

(defn fast-map []
  {})

(signature datascript.query-v3/fast-arr [value]
  :fn<vector<value>>)

(defn fast-arr []
  [])

(signature datascript.query-v3/fast-set [value]
  :fn<set<value>>)

(defn fast-set []
  #{})

(type-alias relation-row
  :array<datascript.lg.query-types/result>)

(type-alias relation-transform
  :fn<vector<array<datascript.lg.query-types/result>>;vector<array<datascript.lg.query-types/result>>>)

(type-alias collect-specimen-v3
  :array<option<datascript.lg.query-types/result>>)

(type-alias relation-hash-v3
  :Datascript_runtime.Query_value.row_hash<datascript.db/database-view>)

(type-record relation-state
  (symbols :vector<string>)
  (offset-map :map<string;int>)
  (rows :vector<array<datascript.lg.query-types/result>>)
  (lookup-databases :map<string;datascript.db/database-view>))

(type-variant coll-relation-row-v3
  (CollQueryRowV3 :array<datascript.lg.query-types/result>)
  (CollDatomRowV3 :datascript.db/Datom))

(type-variant relation-v3
  (ArrayRelationV3 :datascript.query-v3/relation-state)
  (CollRelationV3 :datascript.query-v3/relation-state))

(type-variant collect-transform-v3
  (RelationCollectTransformV3
   :datascript.query-v3/relation-v3
   :vector<tuple<int;int>>))

(type-record query-context-state-v3
  (rels :vector<datascript.query-v3/relation-v3>)
  (consts :map<string;datascript.lg.query-types/result>)
  (sources :map<string;datascript.lg.query-types/source>)
  (rules :vector<datascript.parser/Rule>)
  (default-source-symbol :string))

(type-record aggregate-context-state-v3
  (seen :map<string;bool>)
  (attrs :map<string;int>)
  (values :vector<datascript.lg.query-types/result>))

(type-variant query-context-v3
  EmptyContextV3
  (QueryContextV3 :datascript.query-v3/query-context-state-v3))

(type-alias used-rule-arguments-v3
  :map<string;vector<vector<datascript.parser/pattern-element>>>)

(type-record or-resolution-v3
  (matched :bool)
  (rows :vector<array<datascript.lg.query-types/result>>))

(type-record clause-resolution-request-v3
  (context :datascript.query-v3/query-context-v3)
  (clauses :vector<datascript.parser/clause>))

(type-record rule-resolution-request-v3
  (context :datascript.query-v3/query-context-v3)
  (clause :datascript.parser/clause))

(type-record rule-frame-v3
  (prefix-clauses :vector<datascript.parser/clause>)
  (prefix-context :datascript.query-v3/query-context-v3)
  (clauses :vector<datascript.parser/clause>)
  (used-arguments :datascript.query-v3/used-rule-arguments-v3)
  (pending-guards :vector<datascript.parser/clause>))

(type-record rule-clause-split-v3
  (prefix :vector<datascript.parser/clause>)
  (rule :option<datascript.parser/clause>)
  (suffix :vector<datascript.parser/clause>))

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

(type-variant collected-key-v3
  (SingleCollectedKeyV3
   :datascript.lg.query-types/result)
  (CompositeCollectedKeyV3
   :vector<datascript.lg.query-types/result>))

(def empty-context EmptyContextV3)

(defprotocol IClause
  (-resolve-clause
   [clause  context]
   :datascript.query-v3/query-context-v3))

(defn context-v3
  ([relations constants]
   (context-v3 relations constants {}))
  ([relations constants sources]
   (context-v3 relations constants sources "$"))
  ([relations constants sources default-source-symbol]
   (context-v3
    relations constants sources [] default-source-symbol))
  ([relations constants sources rules default-source-symbol]
   (QueryContextV3
    (record query-context-state-v3
      (rels relations)
      (consts constants)
      (sources sources)
      (rules rules)
      (default-source-symbol default-source-symbol)))))

(defn context-empty? [context]
  (match context
    EmptyContextV3 true
    (QueryContextV3 _) false))

(defn context-relations
  [context]
  (match context
    EmptyContextV3 []
    (QueryContextV3 state) (:rels state)))

(defn context-constants
  [context]
  (match context
    EmptyContextV3 {}
    (QueryContextV3 state) (:consts state)))

(defn context-sources
  [context]
  (match context
    EmptyContextV3 {}
    (QueryContextV3 state) (:sources state)))

(defn context-rules
  [context]
  (match context
    EmptyContextV3 []
    (QueryContextV3 state) (:rules state)))

(defn context-default-source-symbol
  [context]
  (match context
    EmptyContextV3 "$"
    (QueryContextV3 state) (:default-source-symbol state)))

(defn- context-with-relations
  [context relations]
  (match context
    EmptyContextV3 EmptyContextV3
    (QueryContextV3 state)
    (context-v3
     relations
     (:consts state)
     (:sources state)
     (:rules state)
     (:default-source-symbol state))))

(defn- context-with-constants
  [context constants]
  (match context
    EmptyContextV3 EmptyContextV3
    (QueryContextV3 state)
    (context-v3
     (:rels state)
     constants
     (:sources state)
     (:rules state)
     (:default-source-symbol state))))

(defn- context-with-sources
  [context sources]
  (match context
    EmptyContextV3 EmptyContextV3
    (QueryContextV3 state)
    (context-v3
     (:rels state)
     (:consts state)
     sources
     (:rules state)
     (:default-source-symbol state))))

(defn- context-with-rules
  [context rules]
  (match context
    EmptyContextV3 EmptyContextV3
    (QueryContextV3 state)
    (context-v3
     (:rels state)
     (:consts state)
     (:sources state)
     rules
     (:default-source-symbol state))))

(defprotocol IRelation
  (-project
   [relation ^:vector<string> symbols]
   :datascript.query-v3/relation-v3)
  (-alter-coll
   [relation
    ^relation-transform
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

(defn- selected-symbols
  [offset-map
    symbols]
  (reduce
   (fn [selected symbol]
     (if (contains? offset-map symbol)
       (conj selected symbol)
       selected))
   []
   symbols))

(defn- selected-offsets
  [offset-map
    symbols]
  (reduce
   (fn [selected symbol]
     (if-some [index (get offset-map symbol)]
       (assoc selected symbol index)
       selected))
   {}
   symbols))

(defn-
  selected-lookup-databases
  [lookup-databases
    symbols]
  (reduce
   (fn [selected symbol]
     (if-some [database (get lookup-databases symbol)]
       (assoc selected symbol database)
       selected))
   {}
   symbols))

(defn- relation-symbols
  [relation]
  (match relation
    (ArrayRelationV3 state)
    (:symbols state)
    (CollRelationV3 state)
    (:symbols state)))

(defn- relation-offset-map
  [relation]
  (match relation
    (ArrayRelationV3 state)
    (:offset-map state)
    (CollRelationV3 state)
    (:offset-map state)))

(defn- relation-rows-closed
  [relation]
  (match relation
    (ArrayRelationV3 state)
    (:rows state)
    (CollRelationV3 state)
    (:rows state)))

(defn- relation-lookup-databases-v3
  [relation]
  (match relation
    (ArrayRelationV3 state)
    (:lookup-databases state)
    (CollRelationV3 state)
    (:lookup-databases state)))

(defn- relation-tuples
  [relation]
  (relation-rows-closed relation))

(defn- database-view-print-string-v3
  [database]
  (match database
    (db/DatabaseView unfiltered)
    (db/database-print-string unfiltered)
    (db/FilteredDatabaseView filtered)
    (db/filtered-database-print-string filtered)))

(defn- result-print-string-v3
  [result]
  (match result
    (Datascript_runtime.Query_value.Entity entity)
    (Stdlib.string_of_int entity)
    (Datascript_runtime.Query_value.Attr attr)
    attr
    (Datascript_runtime.Query_value.Value value)
    (Datascript_runtime.Data_value.to_edn_string value)
    (Datascript_runtime.Query_value.Metadata value _)
    (Datascript_runtime.Data_value.to_edn_string value)
    (Datascript_runtime.Query_value.Database database)
    (database-view-print-string-v3 database)
    (Datascript_runtime.Query_value.Pull value)
    (Datascript_runtime.Data_value.to_edn_string value)
    (Datascript_runtime.Query_value.Added added)
    (if added "true" "false")
    (Datascript_runtime.Query_value.Callable _)
    "#<function>"))

(defn- relation-row-print-string-v3
  [^:array<datascript.lg.query-types/result> row]
  (str
   "("
   (string/join
    " "
    (mapv result-print-string-v3 (vec row)))
   ")"))

(defn- relation-kind-print-string-v3
  [relation]
  (match relation
    (ArrayRelationV3 _) "ArrayRelation"
    (CollRelationV3 _) "CollRelation"))

(defn- relation-print-string-v3
  [relation]
  (str
   "#"
   (relation-kind-print-string-v3 relation)
   "{:symbols ("
   (string/join " " (relation-symbols relation))
   "), :coll ["
   (string/join
    " "
    (mapv
     relation-row-print-string-v3
     (relation-rows-closed relation)))
   "]}"))

(defn pr-rel
  [relation writer]
  (Buffer.add_string
   writer
   (relation-print-string-v3 relation))
  writer)

(defn- context-constants-print-string-v3
  [constants]
  (str
   "{"
   (string/join
    ", "
    (reduce-kv
     (fn [entries symbol value]
       (conj
        entries
        (str
         symbol
         " "
         (result-print-string-v3 value))))
     []
     constants))
   "}"))

(defn- context-print-string-v3
  [context]
  (let [relations (context-relations context)]
    (str
     "{:rels"
     (if (empty? relations)
       "  []\n"
       (str
        "  "
        (string/join
         "\n  "
         (mapv relation-print-string-v3 relations))
        "\n"))
     "  :consts "
     (context-constants-print-string-v3
      (context-constants context))
     " }\n")))

(defn println-context [context]
  (print (context-print-string-v3 context)))

(defn- make-array-relation
  [symbols offset-map rows lookup-databases]
  (ArrayRelationV3
   (record relation-state
     (symbols symbols)
     (offset-map offset-map)
     (rows rows)
     (lookup-databases lookup-databases))))

(defn- make-coll-relation
  [symbols offset-map rows lookup-databases]
  (CollRelationV3
   (record relation-state
     (symbols symbols)
     (offset-map offset-map)
     (rows rows)
     (lookup-databases lookup-databases))))

(defn- make-relation-like
  [relation symbols offset-map rows lookup-databases]
  (match relation
    (ArrayRelationV3 _)
    (make-array-relation
     symbols offset-map rows lookup-databases)
    (CollRelationV3 _)
    (make-coll-relation
     symbols offset-map rows lookup-databases)))

(defn- same-relation-kind?
  [left  right]
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
       (relation-tuples relation)
       (selected-lookup-databases
        (relation-lookup-databases-v3 relation)
        projected-symbols))))

  (-alter-coll [relation transform]
    (make-relation-like
     relation
     (relation-symbols relation)
     (relation-offset-map relation)
     (transform (relation-tuples relation))
     (relation-lookup-databases-v3 relation)))

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
          (relation-tuples other))
         (relation-lookup-databases-v3 relation))
        (Stdlib.invalid_arg
         "Cannot union relations with different symbols"))
      (Stdlib.invalid_arg
       "Cannot union relations with different kinds"))))

(defn- array-rel-with-lookups
  [symbols rows lookup-databases]
  (make-array-relation
   symbols
   (reduce-kv
    (fn [offsets index symbol]
      (assoc offsets symbol index))
    {}
   symbols)
   rows
   lookup-databases))

(defn array-rel
  [symbols rows]
  (array-rel-with-lookups symbols rows {}))

(defn- coll-row-array
  [row]
  (match row
    (CollQueryRowV3 values) values
    (CollDatomRowV3 datom) (query-types/datom-row datom)))

(defn- coll-rel-with-lookups
  [^:vector<datascript.parser/pattern-element> pattern
   rows
   lookup-databases]
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
     (mapv coll-row-array rows)
     lookup-databases)))

(defn coll-rel
  [pattern rows]
  (coll-rel-with-lookups pattern rows {}))

(defn singleton-rel []
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
    (array-rel-with-lookups
     (into left-symbols right-symbols)
     rows
     (query-types/merge-lookup-databases
      (relation-lookup-databases-v3 left)
      (relation-lookup-databases-v3 right)))))

(defn ^relation-v3 product-all
  [^:vector<relation-v3> relations]
  (reduce product relations))

(defn bind
  [binding
    value]
  (let [relation
        (query-types/binding-relation binding value)]
    (array-rel
     (parser/binding-variable-names binding)
     (query-types/relation-rows relation))))

(defn-
  require-v3-binding-input
  [input]
  (if-some [value (query-types/input-binding input)]
    value
    (Stdlib.invalid_arg
     "Query value input requires a Binding_input")))

(defn- resolve-value-input
  [context
    binding
    input]
  (let [relation
        (bind binding (require-v3-binding-input input))]
    (if (= 1 (-size relation))
      (context-with-constants
       context
       (merge
        (context-constants context)
        (relation-constants relation)))
      (context-with-relations
       context
       (conj
        (context-relations context)
        relation)))))

(defn- resolve-source-input
  [context
    source-name
    input]
  (if-some [source (query-types/input-source input)]
    (context-with-sources
     context
     (assoc (context-sources context) source-name source))
    (Stdlib.invalid_arg
     "Source query input requires a Source_input")))

(defn- ^:string binding-source
  [^datascript.parser/binding binding]
  (if (parser/binding-ignore? binding)
    "_"
    (if-some [variable (parser/binding-scalar-variable binding)]
      variable
      (if-some [items (parser/binding-tuple-items binding)]
        (str
         "["
         (string/join " " (mapv binding-source items))
         "]")
        (if-some [item (parser/binding-collection-item binding)]
          (str "[" (binding-source item) " ...]")
          "_")))))

(defn- ^:string static-input-source
  [^datascript.parser/static-query-input input]
  (if (parser/static-input-rules? input)
    "%"
    (if-some [source-name (parser/static-input-source-name input)]
      source-name
      (if-some [binding (parser/static-input-binding input)]
        (binding-source binding)
        "_"))))

(defn- ^:string static-inputs-source
  [^:vector<datascript.parser/static-query-input> inputs]
  (str
   "["
   (string/join " " (mapv static-input-source inputs))
   "]"))

(defn resolve-ins
  [context
    descriptors
    inputs]
  (if (not (= (count descriptors) (count inputs)))
    (Stdlib.invalid_arg
     (str
      "Wrong number of arguments for bindings "
      (static-inputs-source descriptors)
      ", "
      (count descriptors)
      " required, "
      (count inputs)
      " provided"))
    (loop [resolved context
           remaining-descriptors descriptors
           remaining-inputs inputs]
      (if-some [descriptor (first remaining-descriptors)]
        (if-some [input (first remaining-inputs)]
          (if (parser/static-input-rules? descriptor)
            (if-some [rules (query-types/input-rules input)]
              (recur
               (context-with-rules resolved rules)
               (subvec remaining-descriptors 1)
               (subvec remaining-inputs 1))
              (Stdlib.invalid_arg
               "Rules query input requires a Rules_input"))
            (if-some
              [source-name
               (parser/static-input-source-name descriptor)]
              (recur
               (resolve-source-input
                resolved source-name input)
               (subvec remaining-descriptors 1)
               (subvec remaining-inputs 1))
              (if-some
                [binding
                 (parser/static-input-binding descriptor)]
                (recur
                 (resolve-value-input
                  resolved binding input)
                 (subvec remaining-descriptors 1)
                 (subvec remaining-inputs 1))
                (Stdlib.invalid_arg
                 "Unsupported static query input descriptor"))))
          (Stdlib.invalid_arg "Missing query input"))
        resolved))))

(defn hash-map-rel
  [relation  symbols]
  (Datascript_runtime.Query_value.row_hash
   (relation-tuples relation)
   (-indexes relation symbols)))

(defn- symbols-not-in
  [^:vector<string> excluded ^:vector<string> symbols]
  (let [excluded (set excluded)]
    (reduce
     (fn [remaining symbol]
       (if (contains? excluded symbol)
         remaining
         (conj remaining symbol)))
     []
     symbols)))

(defn hash-join
  [left
    left-hash
    join-symbols
    right]
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
    (array-rel-with-lookups
     (into left-symbols keep-right-symbols)
     rows
     (query-types/merge-lookup-databases
      (relation-lookup-databases-v3 left)
      (relation-lookup-databases-v3 right)))))

(defn- relation-has-lookup-database?
  [relation  symbols]
  (let [lookup-databases
        (relation-lookup-databases-v3 relation)]
    (some?
     (some
      (fn [symbol]
        (contains? lookup-databases symbol))
      symbols))))

(defn-
  relation-runtime-v3
  [relation]
  (query-types/relation
   (relation-offset-map relation)
   (relation-tuples relation)
   (relation-lookup-databases-v3 relation)))

(defn-
  project-runtime-relation-row-v3
  [relation
    symbols
    row]
  (to-array
   (mapv
    (fn [symbol]
      (if-some [result
                (query-types/relation-result
                 relation symbol row)]
        result
        (Stdlib.invalid_arg
         (str "Missing joined relation symbol " symbol))))
    symbols)))

(defn- hash-join-with-lookups-v3
  [left  right]
  (let [symbols
        (into
         (-symbols left)
         (symbols-not-in (-symbols left) (-symbols right)))
        joined
        (query-types/hash-join
         (relation-runtime-v3 left)
         (relation-runtime-v3 right))]
    (array-rel-with-lookups
     symbols
     (mapv
      (fn [row]
        (project-runtime-relation-row-v3
         joined symbols row))
      (query-types/relation-rows joined))
     (query-types/relation-lookup-databases joined))))

(defn- ^:map<string;datascript.lg.query-types/result>
  relation-constants
  [relation]
  (if-some [row (first (relation-tuples relation))]
    (reduce
     (fn [constants symbol]
       (assoc constants symbol ((-getter relation symbol) row)))
     {}
     (-symbols relation))
    {}))

(defn- relation-shares-symbols?
  [relation  symbols]
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

(defn join-unrelated
  [context  relation]
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

(defn- shared-symbols
  [^:vector<string> left ^:vector<string> right]
  (let [right (set right)]
    (filterv
     (fn [symbol]
       (contains? right symbol))
     left)))

(defn hash-join-rel
  [context  relation]
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
            joined
            (if
             (or
              (relation-has-lookup-database?
               related-relation join-symbols)
              (relation-has-lookup-database?
               relation join-symbols))
              (hash-join-with-lookups-v3
               related-relation relation)
              (hash-join
               related-relation
               (hash-map-rel
                related-relation join-symbols)
               join-symbols
               relation))]
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
       (:rules state)
       (:default-source-symbol state)))))

(defn-
  pattern-clause-parts
  [clause]
  (match clause
    (parser/PatternClause source pattern)
    (tuple source pattern)
    _
    (Stdlib.invalid_arg "Expected a DataScript pattern clause")))

(defn get-source
  [context
    query-source]
  (let [source-name
        (if-some [source-name
                  (parser/query-source-name query-source)]
          source-name
          (context-default-source-symbol context))]
    (if-some [source (get (context-sources context) source-name)]
      source
      (Stdlib.invalid_arg
       (str "Source " source-name " is not defined")))))

(defn- empty-pattern-relation
  [pattern
    lookup-databases]
  (coll-rel-with-lookups pattern [] lookup-databases))

(defn-
  pattern-lookup-databases-v3
  [database
    pattern]
  (match
   (query-types/pattern-attr-constraint
    (query-types/pattern-element-at pattern 1))
    (Some attr)
    (query-types/pattern-lookup-databases
     database pattern attr)
    None
    (query-types/pattern-lookup-databases
     database pattern None)))

(defn- resolve-pattern-db-closed
  [database
   ^:vector<datascript.parser/pattern-element> pattern]
  (if (or (empty? pattern) (> (count pattern) 5))
    (Stdlib.invalid_arg
     "DataScript patterns must contain one to five elements")
    (let [lookup-databases
          (pattern-lookup-databases-v3 database pattern)]
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
            (coll-rel-with-lookups
             pattern
             (mapv
              (fn [datom]
                (CollDatomRowV3 datom))
              datoms)
             lookup-databases))
          (empty-pattern-relation
           pattern lookup-databases))
        _
        (empty-pattern-relation
         pattern lookup-databases)))))

(defn resolve-pattern-db
  [database
    clause]
  (resolve-pattern-db-closed
   database
   (tuple-get (pattern-clause-parts clause) 1)))

(defn- pattern-result-equals?
  [result
    constant]
  (let [value
        (match result
          (Datascript_runtime.Query_value.Entity entity)
          (Some (Datascript_runtime.Data_value.Int entity))
          (Datascript_runtime.Query_value.Attr attr)
          (Some (Datascript_runtime.Data_value.Keyword attr))
          (Datascript_runtime.Query_value.Value value)
          (Some value)
          (Datascript_runtime.Query_value.Metadata value _)
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

(defn- row-matches-pattern-constants?
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

(defn resolve-pattern-coll
  [source
    clause]
  (resolve-pattern-coll-closed
   source
   (tuple-get (pattern-clause-parts clause) 1)))

(defn clause-syms
  [clause]
  (reduce
   (fn [symbols variable]
     (conj symbols (str (.-symbol variable))))
   (set-of :string)
   (parser/clause-vars clause)))

(defn- substitute-constant-node
  [constants
    node]
  (if-some [symbol
            (parser/traversable-variable-name node)]
    (if-some [value (get constants symbol)]
      (parser/data-traversable
       (query-types/result-pattern-value value))
      node)
    node))

(defn substitute-constants
  [clause
    context]
  (let [constants (context-constants context)]
    (if
     (some
      (fn [symbol]
        (contains? constants symbol))
      (clause-syms clause))
      (let [walked
            (parser/postwalk
             (parser/clause-traversable clause)
             (fn [node]
               (substitute-constant-node
                constants node)))]
        (if-some [substituted
                  (parser/traversable-clause-value walked)]
          substituted
          (Stdlib.invalid_arg
           "Constant substitution did not return a clause")))
      clause)))

(defn resolve-pattern
  [context  clause]
  (let [substituted (substitute-constants clause context)
        parts (pattern-clause-parts substituted)
        query-source (tuple-get parts 0)
        pattern (tuple-get parts 1)
        source (get-source context query-source)
        relation
        (if-some [database (query-types/source-database source)]
          (resolve-pattern-db-closed database pattern)
          (resolve-pattern-coll-closed source pattern))]
    (hash-join-rel context relation)))

(defn- predicate-clause-parts
  [clause]
  (match clause
    (parser/PredicateClause callable arguments)
    (tuple callable arguments)
    _
    (Stdlib.invalid_arg "Expected a DataScript predicate clause")))

(defn get-f
  [context
    callable
    _form]
  (if-some [name (parser/static-callable-name callable)]
    (if-some [function (built-ins/pure-function name)]
      (PurePredicateV3 name function)
      (if-some [function (built-ins/comparison-function name)]
        (ComparisonPredicateV3 name function)
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

(defn- predicate-source-result
  [context  source-name]
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

(defn- context-symbol-bound?
  [context  symbol]
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

(defn-
  collected-predicate-arguments
  [arguments]
  (mapv
   (fn [argument]
     (match argument
       (Some value) value
       None
       (Stdlib.invalid_arg "Predicate argument is not bound")))
   arguments))

(defn- data-value-truthy?
  [value]
  (if (Datascript_runtime.Data_value.is_nil value)
    false
    (match (Datascript_runtime.Data_value.bool_value value)
      (Some value) value
      None true)))

(defn- query-result-truthy-v3?
  [result]
  (if-some [value (query-types/result-value result)]
    (data-value-truthy? value)
    true))

(defn-
  pure-function-result-v3
  [function
    arguments]
  (if (built-ins/complement-function? function)
    (Some (query-types/complement-result arguments))
    (if (built-ins/metadata-function? function)
      (Some (query-types/metadata-function-result arguments))
      (if (built-ins/value-type-function? function)
        (Some (query-types/value-type-function-result arguments))
        (let [values
              (mapv query-types/result-pattern-value arguments)]
          (if (built-ins/differ-function? function)
            (Some
             (query-types/value-result
              (Datascript_runtime.Data_value.Bool
               (built-ins/apply-differ values))))
            (if-some [value
                      (built-ins/apply-pure-function function values)]
              (Some (query-types/value-result value))
              None)))))))

(defn- invoke-predicate
  [function
    arguments]
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
    (if-some [result (pure-function-result-v3 function arguments)]
      (query-result-truthy-v3? result)
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
   (fn [rows]
     (filterv
      (fn [row]
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

(defn-
  invoke-function
  [function
    arguments]
  (match function
    (ComparisonPredicateV3 name function)
    (if (built-ins/missing-function? function)
      (if (= 3 (count arguments))
        (Some
         (query-types/value-result
          (Datascript_runtime.Data_value.Bool
           (query-types/query-missing?
            (query-types/query-database-result
             (nth arguments 0))
            (nth arguments 1)
            (nth arguments 2)))))
        (Stdlib.invalid_arg
         "Invalid arguments for query function: missing?"))
      (if-some [value
                (built-ins/apply-comparison
                 function
                 (mapv
                  query-types/result-pattern-value
                  arguments))]
        (Some
         (query-types/value-result
          (Datascript_runtime.Data_value.Bool value)))
        (Stdlib.invalid_arg
         (str "Invalid arguments for query function: " name))))
    (PurePredicateV3 name function)
    (if
     (or
      (built-ins/get-else-function? function)
      (built-ins/get-some-function? function))
      (if-some [value
                (query-types/database-function-value
                 function arguments)]
        (Some (query-types/value-result value))
        (Stdlib.invalid_arg
         (str "Invalid arguments for query function: " name)))
      (if-some [result
                (pure-function-result-v3 function arguments)]
        (Some result)
        (Stdlib.invalid_arg
         (str "Invalid arguments for query function: " name))))
    (VariablePredicateV3 _name callable)
    (if-some [value
              (query-types/invoke-callable callable arguments)]
      (Some (query-types/value-result value))
      None)))

(defn-
  ^:tuple<query-context-v3;relation-v3>
  function-production
  [^query-context-v3 context
   ^:vector<string> variables]
  (match (extract-rels context variables)
    (tuple None unchanged-context)
    (tuple unchanged-context (singleton-rel))
    (tuple (Some relations) remaining-context)
    (tuple remaining-context (product-all relations))))

(defn- ^relation-v3 join-function-binding
  [^relation-v3 production
   ^:array<datascript.lg.query-types/result> row
   ^datascript.parser/binding binding
   ^datascript.lg.query-types/result result]
  (let [row-relation
        (array-rel (-symbols production) [row])
        binding-relation
        (bind
         binding
         (query-types/function-binding-result
          binding result))
        shared
        (shared-symbols
         (-symbols row-relation)
         (-symbols binding-relation))]
    (if (empty? shared)
      (product row-relation binding-relation)
      (hash-join
       row-relation
       (hash-map-rel row-relation shared)
       shared
       binding-relation))))

(defn- ^:option<relation-v3> add-function-output
  [^:option<relation-v3> output ^relation-v3 relation]
  (match output
    None (Some relation)
    (Some previous) (Some (-union previous relation))))

(defn- ^:vector<string> function-output-symbols
  [^relation-v3 production
   ^datascript.parser/binding binding]
  (reduce
   (fn [symbols symbol]
     (if
      (some?
       (some
        (fn [existing]
          (= existing symbol))
        symbols))
       symbols
       (conj symbols symbol)))
   (-symbols production)
   (parser/binding-variable-names binding)))

(defn- ^:bool function-row-matches-constants?
  [^:map<string;datascript.lg.query-types/result> constants
   ^relation-v3 relation
   ^:array<datascript.lg.query-types/result> row]
  (every?
   (fn [symbol]
     (if-some [constant (get constants symbol)]
       (Datascript_runtime.Query_value.equal_result
        ((-getter relation symbol) row)
        constant)
       true))
   (-symbols relation)))

(defn- ^relation-v3 filter-function-constants
  [^query-context-v3 context ^relation-v3 relation]
  (let [constants (context-constants context)]
    (-alter-coll
     relation
     (fn [rows]
       (filterv
        (fn [row]
          (function-row-matches-constants?
           constants relation row))
        rows)))))

(defn ^query-context-v3 resolve-function
  [^query-context-v3 context
   ^datascript.parser/clause clause]
  (match clause
    (parser/FunctionClause callable arguments binding)
    (let [form ""
          function (get-f context callable form)
          target
          (to-array
           (mapv
            (fn [_argument] None)
            arguments))
          _ (collect-args! context arguments target form)
          bindings (predicate-row-bindings context arguments)
          variables
          (mapv
           (fn [binding]
             (tuple-get binding 0))
           bindings)
          _ (check-bound context variables form)
          production-parts
          (function-production context variables)
          remaining-context (tuple-get production-parts 0)
          production (tuple-get production-parts 1)
          output
          (reduce
           (fn [output row]
             (let [row-target (da/aclone target)
                   _ (fill-predicate-row!
                      production row bindings row-target)
                   invocation
                   (invoke-function
                    function
                    (collected-predicate-arguments
                     row-target))]
               (match invocation
                 None output
                 (Some result)
                 (if (query-types/result-nil? result)
                   output
                   (add-function-output
                    output
                    (join-function-binding
                     production row binding result))))))
           None
           (relation-tuples production))
          relation
          (match output
            (Some relation) relation
            None
            (array-rel
             (function-output-symbols
              production binding)
             []))]
      (join-unrelated
       remaining-context
       (filter-function-constants
        remaining-context relation)))
    _
    (Stdlib.invalid_arg
     "Expected a DataScript function clause")))

(defn- context-with-default-source
  [context  source-name]
  (match context
    EmptyContextV3 EmptyContextV3
    (QueryContextV3 state)
    (context-v3
     (:rels state)
     (:consts state)
     (:sources state)
     (:rules state)
     source-name)))

(defn- clause-query-source
  [clause]
  (match clause
    (parser/PatternClause source _) (Some source)
    (parser/RuleClause source _ _) (Some source)
    (parser/NotClause source _ _ _) (Some source)
    (parser/OrClause source _ _ _ _) (Some source)
    _ None))

(defn upd-default-source
  [context  clause]
  (if-some [source (clause-query-source clause)]
    (if-some [source-name (parser/query-source-name source)]
      (context-with-default-source context source-name)
      context)
    context))

(defn- result-vectors-equal?
  [left
    right]
  (if (= (count left) (count right))
    (loop [index 0]
      (if (= index (count left))
        true
        (if
         (Datascript_runtime.Query_value.equal_result
          (nth left index)
          (nth right index))
          (recur (+ index 1))
          false)))
    false))

(defn- collected-keys-equal?
  [left  right]
  (match left
    (SingleCollectedKeyV3 left)
    (match right
      (SingleCollectedKeyV3 right)
      (Datascript_runtime.Query_value.equal_result left right)
      _ false)
    (CompositeCollectedKeyV3 left)
    (match right
      (CompositeCollectedKeyV3 right)
      (result-vectors-equal? left right)
      _ false)))

(defn- collected-key-member?
  [keys  key]
  (some?
   (some
    (fn [candidate]
      (collected-keys-equal? candidate key))
    keys)))

(defn- add-collected-key
  [^:vector<collected-key-v3> keys key]
  (if (collected-key-member? keys key)
    keys
    (conj keys key)))

(defn- collect-single-key
  [context  symbol]
  (if-some [constant (get (context-constants context) symbol)]
    [(SingleCollectedKeyV3 constant)]
    (if-some [relation (first (related-rels context [symbol]))]
      (let [getter (-getter relation symbol)]
        (-fold
         relation
         (fn [keys row]
           (add-collected-key
            keys
            (SingleCollectedKeyV3 (getter row))))
         []))
      [])))

(defn- collect-constant-specimen
  [context symbols]
  (to-array
   (mapv
    (fn [symbol]
      (if-some [value (get (context-constants context) symbol)]
        (Some value)
        None))
    symbols)))

(defn- fill-collect-specimen!
  [relation
   row
   ^:vector<string> symbols
   specimen]
  (reduce-kv
   (fn [_ index symbol]
     (if-some [relation-index
               (get (relation-offset-map relation) symbol)]
       (aset specimen index (Some (aget row relation-index)))
       (Stdlib.ignore 0))
     (Stdlib.ignore 0))
   (Stdlib.ignore 0)
   symbols))

(defn-
  ^:vector<array<option<datascript.lg.query-types/result>>>
  expand-collect-specimens
  [^:vector<array<option<datascript.lg.query-types/result>>> specimens
   ^relation-v3 relation
   ^:vector<string> symbols]
  (reduce
   (fn
     [^:vector<array<option<datascript.lg.query-types/result>>> expanded
       specimen]
     (-fold
      relation
      (fn
        [^:vector<array<option<datascript.lg.query-types/result>>> expanded
          row]
        (let [copy (da/aclone specimen)
              _ (fill-collect-specimen!
                 relation row symbols copy)]
          (conj expanded copy)))
      expanded))
   []
   specimens))

(defn-
  ^:option<vector<datascript.lg.query-types/result>>
  collect-specimen-results
  [^:array<option<datascript.lg.query-types/result>> specimen]
  (reduce
   (fn [collected item]
     (match collected
       None None
       (Some values)
       (match item
         None None
         (Some value)
         (Some (conj values value)))))
   (Some [])
   specimen))

(defn collect-opt
  [context  symbols]
  (let [_ (check-bound context symbols "collect-opt")]
    (if (= 1 (count symbols))
      (collect-single-key context (nth symbols 0))
      (let [specimens
            (reduce
             (fn [specimens relation]
               (expand-collect-specimens
                specimens relation symbols))
             [(collect-constant-specimen context symbols)]
             (related-rels context symbols))]
        (reduce
         (fn [keys specimen]
           (match (collect-specimen-results specimen)
             None keys
             (Some values)
             (add-collected-key
              keys
              (CompositeCollectedKeyV3 values))))
         []
         specimens)))))

(defn- relation-row-key
  [relation
   row
   symbols]
  (if (= 1 (count symbols))
    (SingleCollectedKeyV3
     ((-getter relation (nth symbols 0)) row))
    (CompositeCollectedKeyV3
     (mapv
      (fn [symbol]
        ((-getter relation symbol) row))
      symbols))))

(defn subtract-from-rel
  [relation
   symbols
   excluded]
  (-alter-coll
   relation
   (fn [rows]
     (filterv
      (fn [row]
        (not
         (collected-key-member?
          excluded
          (relation-row-key relation row symbols))))
      rows))))

(defn subtract-contexts
  [context
   excluded-context
   symbols]
  (if (context-empty? context)
    EmptyContextV3
    (if (context-empty? excluded-context)
      context
      (let [non-constants
            (filterv
             (fn [symbol]
               (not
                (contains?
                 (context-constants context)
                 symbol)))
             symbols)]
        (if (empty? non-constants)
          EmptyContextV3
          (match (extract-rels context non-constants)
            (tuple None _)
            (Stdlib.invalid_arg
             "Cannot subtract unbound query relations")
            (tuple (Some relations) remaining-context)
            (let [relation (product-all relations)
                  excluded
                  (collect-opt excluded-context non-constants)
                  remaining
                  (subtract-from-rel
                   relation non-constants excluded)]
              (join-unrelated
               remaining-context
               remaining))))))))

(declare
 resolve-clauses-state-v3
 resolve-clause-closed
 resolve-or
 resolve-rule-request-v3)

(defn resolve-not
  [context  clause]
  (if (context-empty? context)
    EmptyContextV3
    (match clause
      (parser/NotClause _source variables clauses display)
      (let [symbols
            (mapv
             (fn [variable]
               (str (.-symbol variable)))
             variables)
            _ (check-bound context symbols display)
            nested-context
            (resolve-clauses-state-v3
             (record clause-resolution-request-v3
               (context
                (upd-default-source
                 (project-context context symbols)
                 clause))
               (clauses clauses)))]
        (subtract-contexts context nested-context symbols))
      _
      (Stdlib.invalid_arg
       "Expected a DataScript not clause"))))

(defn-
  ^:vector<array<datascript.lg.query-types/result>>
  collect-context-rows
  [^query-context-v3 context ^:vector<string> symbols]
  (if (context-empty? context)
    []
    (let [specimens
          (reduce
           (fn [specimens relation]
             (expand-collect-specimens
              specimens relation symbols))
           [(collect-constant-specimen context symbols)]
           (related-rels context symbols))]
      (reduce
       (fn [rows specimen]
         (match (collect-specimen-results specimen)
           None rows
           (Some values)
           (conj rows (to-array values))))
       []
       specimens))))

(defn- ^or-resolution-v3 resolve-or-branches
  [^query-context-v3 branch-context
   ^:vector<datascript.parser/clause> branches
   ^:vector<string> symbols]
  (reduce
   (fn [resolution branch]
     (let [resolved
           (resolve-clause-closed branch-context branch)]
       (if (context-empty? resolved)
         resolution
         (record or-resolution-v3
           (matched true)
           (rows
            (into
             (:rows resolution)
             (collect-context-rows resolved symbols)))))))
   (record or-resolution-v3
     (matched false)
     (rows []))
   branches))

(defn resolve-or
  [context  clause]
  (if (context-empty? context)
    EmptyContextV3
    (if-some [parts (parser/or-clause-parts clause)]
      (let [required (tuple-get parts 0)
            symbols (tuple-get parts 1)
            branches (tuple-get parts 2)
            display
            (if-some [display
                      (parser/or-clause-display clause)]
              display
              "")
            _ (check-bound context required display)
            branch-context
            (upd-default-source
             (project-context context symbols)
             clause)
            non-constants
            (filterv
             (fn [symbol]
               (not
                (contains?
                 (context-constants context)
                 symbol)))
             symbols)
            resolution
            (resolve-or-branches
             branch-context branches non-constants)]
        (if (not (:matched resolution))
          EmptyContextV3
          (if (empty? non-constants)
            context
            (let [relation
                  (array-rel
                   non-constants (:rows resolution))]
              (hash-join-rel context relation)))))
      (Stdlib.invalid_arg
       "Expected a DataScript or clause"))))

(defn- resolve-clause-closed
  [context  clause]
  (match clause
    (parser/PatternClause _ _) (resolve-pattern context clause)
    (parser/PredicateClause _ _) (resolve-predicate context clause)
    (parser/FunctionClause _ _ _) (resolve-function context clause)
    (parser/RuleClause _ _ _)
    (resolve-rule-request-v3
     (record rule-resolution-request-v3
       (context context)
       (clause clause)))
    (parser/AndClause clauses)
    (resolve-clauses-state-v3
     (record clause-resolution-request-v3
       (context context)
       (clauses clauses)))
    (parser/NotClause _ _ _ _) (resolve-not context clause)
    (parser/OrClause _ _ _ _ _) (resolve-or context clause)))

(extend-type datascript.parser/clause
  IClause
  (-resolve-clause [clause context]
    (resolve-clause-closed context clause)))

(defn- resolve-clauses-state-v3
  [request]
  (loop [resolved (:context request)
         remaining (:clauses request)]
    (if (context-empty? resolved)
      resolved
      (if-some [clause (first remaining)]
        (recur
         (-resolve-clause clause resolved)
         (subvec remaining 1))
        resolved))))

(defn resolve-clauses
  [context
    clauses]
  (resolve-clauses-state-v3
   (record clause-resolution-request-v3
     (context context)
     (clauses clauses))))

(def rule-seqid (atom 0))

(defn- rule-pattern-elements-equal?
  [left right]
  (match (parser/pattern-element-variable-symbol left)
    (Some left-variable)
    (match (parser/pattern-element-variable-symbol right)
      (Some right-variable)
      (= (str left-variable) (str right-variable))
      None false)
    None
    (match (parser/pattern-element-constant left)
      (Some left-value)
      (match (parser/pattern-element-constant right)
        (Some right-value)
        (Datascript_runtime.Data_value.equal
         left-value right-value)
        None false)
      None
      (and
       (not
        (some?
         (parser/pattern-element-variable-symbol right)))
       (not
        (some?
         (parser/pattern-element-constant right)))))))

(defn- remove-rule-argument-pairs
  [^:vector<datascript.parser/pattern-element> left
   ^:vector<datascript.parser/pattern-element> right]
  (if (not (= (count left) (count right)))
    (Stdlib.invalid_arg "Rule arity mismatch")
    (reduce-kv
     (fn [pair index left-argument]
       (let [right-argument (nth right index)]
         (if
          (rule-pattern-elements-equal?
           left-argument right-argument)
           pair
           (tuple
            (conj (tuple-get pair 0) left-argument)
            (conj (tuple-get pair 1) right-argument)))))
     (tuple [] [])
     left)))

(defn- rule-argument-fn-arg
  [argument]
  (match (parser/pattern-element-variable-symbol argument)
    (Some variable)
    (parser/variable-argument (str variable))
    None
    (match (parser/pattern-element-constant argument)
      (Some value)
      (parser/constant-argument value)
      None
      (parser/constant-argument
       (Datascript_runtime.Data_value.Symbol "_")))))

(defn- rule-guard-clause
  [current previous]
  (let [different (remove-rule-argument-pairs current previous)
        arguments
        (into
         (tuple-get different 0)
         (tuple-get different 1))]
    (parser/static-predicate-clause
     "-differ?"
     (mapv rule-argument-fn-arg arguments))))

(defn- rule-gen-guards-v3
  [clause used-arguments]
  (if-some [parts (parser/rule-clause-parts clause)]
    (let [rule-name (tuple-get parts 0)
          arguments (tuple-get parts 1)
          previous-calls
          (if-some [calls (get used-arguments rule-name)]
            calls
            [])]
      (mapv
       (fn [previous]
         (rule-guard-clause arguments previous))
       previous-calls))
    (Stdlib.invalid_arg "Expected a DataScript rule clause")))

(defn- ^:vector<string> clause-variable-symbols
  [clauses]
  (reduce
   (fn [symbols clause]
     (reduce
      (fn [symbols variable]
        (let [symbol (str (.-symbol variable))]
          (if (some? (some #(= % symbol) symbols))
            symbols
            (conj symbols symbol))))
      symbols
      (parser/clause-vars clause)))
   []
   clauses))

(defn- split-rule-guards-v3
  [clauses
    guards]
  (let [bound-symbols (set (clause-variable-symbols clauses))]
    (reduce
     (fn [split guard]
       (let [active?
             (every?
              (fn [variable]
                (contains?
                 bound-symbols
                 (str (.-symbol variable))))
              (parser/clause-vars guard))]
         (if active?
           (tuple
            (conj (tuple-get split 0) guard)
            (tuple-get split 1))
           (tuple
            (tuple-get split 0)
            (conj (tuple-get split 1) guard)))))
     (tuple [] [])
     guards)))

(defn- trivial-rule-guard?
  [clause]
  (match clause
    (parser/PredicateClause callable arguments)
    (and
     (empty? arguments)
     (match (parser/static-callable-name callable)
       (Some name) (= name "-differ?")
       None false))
    _ false))

(defn- split-first-rule-clause
  [clauses]
  (loop [prefix []
         remaining clauses]
    (if-some [clause (first remaining)]
      (if (some? (parser/rule-clause-parts clause))
        (record rule-clause-split-v3
          (prefix prefix)
          (rule (Some clause))
          (suffix (subvec remaining 1)))
        (recur (conj prefix clause) (subvec remaining 1)))
      (record rule-clause-split-v3
        (prefix prefix)
        (rule None)
        (suffix [])))))

(defn- rule-output-symbols
  [clause]
  (if-some [parts (parser/rule-clause-parts clause)]
    (reduce
     (fn [symbols argument]
       (match (parser/pattern-element-variable-symbol argument)
         (Some variable)
         (let [symbol (str variable)]
           (if (some? (some #(= % symbol) symbols))
             symbols
             (conj symbols symbol)))
         None symbols))
     []
     (tuple-get parts 1))
    (Stdlib.invalid_arg "Expected a DataScript rule clause")))

(defn- rule-argument-bound-v3?
  [context argument]
  (match (parser/pattern-element-variable-symbol argument)
    (Some variable)
    (context-symbol-bound? context (str variable))
    None
    (some? (parser/pattern-element-constant argument))))

(defn- rule-branches-for-call
  [context rule-name arguments]
  (if-some [branches
            (parser/rule-branches
             (context-rules context) rule-name)]
    (if-some [first-branch (first branches)]
      (let [parameters
            (parser/rule-branch-parameter-names first-branch)
            required-count
            (count
             (parser/rule-branch-required-parameter-names
              first-branch))]
        (if (not (= (count parameters) (count arguments)))
          (Stdlib.invalid_arg "Rule arity mismatch")
          (if
           (every?
            (fn [index]
              (rule-argument-bound-v3?
               context (nth arguments index)))
            (range required-count))
            branches
            (Stdlib.invalid_arg
             "Insufficient bindings for required rule arguments"))))
      (Stdlib.invalid_arg "Rule must contain a branch"))
    (Stdlib.invalid_arg
     (str
      "Unknown rule '"
      rule-name
      " in "
      (query-types/rule-call-description
       rule-name arguments)))))

(defn- context-dead?
  [context]
  (or
   (context-empty? context)
   (some?
    (some
     (fn [relation]
       (= 0 (-size relation)))
     (context-relations context)))))

(defn- concat-rule-clauses-v3
  [^:vector<datascript.parser/clause> left
   ^:vector<datascript.parser/clause> right]
  (reduce
   (fn [clauses clause]
     (conj clauses clause))
   left
   right))

(defn- ^relation-v3 solve-rule-stack-v3
  [^:vector<string> final-symbols
   ^:vector<rule-frame-v3> stack
   ^relation-v3 result]
  (if-some [frame (first stack)]
    (let [remaining-stack (subvec stack 1)
          split
          (split-first-rule-clause (:clauses frame))
          prefix (:prefix split)]
      (match (:rule split)
        None
        (let [resolved
              (resolve-clauses-state-v3
               (record clause-resolution-request-v3
                 (context (:prefix-context frame))
                 (clauses prefix)))]
          (if (context-dead? resolved)
            (solve-rule-stack-v3
             final-symbols remaining-stack result)
            (let [rows
                  (query-types/distinct-rows
                   (collect-to resolved final-symbols []))
                  relation (array-rel final-symbols rows)]
              (solve-rule-stack-v3
               final-symbols
               remaining-stack
               (-union result relation)))))
        (Some rule-clause)
        (let [guards
              (rule-gen-guards-v3
               rule-clause (:used-arguments frame))
              guard-split
              (split-rule-guards-v3
               (concat-rule-clauses-v3
                (:prefix-clauses frame) prefix)
               (concat-rule-clauses-v3
                guards (:pending-guards frame)))
              active-guards (tuple-get guard-split 0)
              pending-guards (tuple-get guard-split 1)]
          (if (some? (some trivial-rule-guard? active-guards))
            (solve-rule-stack-v3
             final-symbols remaining-stack result)
            (let [prefix-clauses
                  (concat-rule-clauses-v3
                   prefix active-guards)
                  prefix-context
                  (resolve-clauses-state-v3
                   (record clause-resolution-request-v3
                     (context (:prefix-context frame))
                     (clauses prefix-clauses)))]
              (if (context-dead? prefix-context)
                (solve-rule-stack-v3
                 final-symbols remaining-stack result)
                (if-some [parts
                          (parser/rule-clause-parts
                           rule-clause)]
                  (let [rule-name (tuple-get parts 0)
                        arguments (tuple-get parts 1)
                        call-context
                        (upd-default-source
                         prefix-context rule-clause)
                        branches
                        (rule-branches-for-call
                         call-context rule-name arguments)
                        previous
                        (if-some
                          [calls
                           (get
                            (:used-arguments frame)
                            rule-name)]
                          calls
                          [])
                        used-arguments
                        (assoc
                         (:used-arguments frame)
                         rule-name
                         (conj previous arguments))
                        seqid (swap! rule-seqid inc)
                        frames
                        (mapv
                         (fn [branch]
                           (record rule-frame-v3
                             (prefix-clauses prefix-clauses)
                             (prefix-context call-context)
                             (clauses
                              (concat-rule-clauses-v3
                               (parser/expand-rule-branch
                                branch arguments seqid)
                               (:suffix split)))
                             (used-arguments used-arguments)
                             (pending-guards pending-guards)))
                         branches)]
                    (solve-rule-stack-v3
                     final-symbols
                     (into frames remaining-stack)
                     result))
                  (Stdlib.invalid_arg
                   "Expected a DataScript rule clause"))))))))
    (-alter-coll
     result
     query-types/distinct-rows)))

(defn- ^relation-v3 solve-rule-v3
  [^query-context-v3 context
   ^datascript.parser/clause clause]
  (let [final-symbols (rule-output-symbols clause)
        initial-frame
        (record rule-frame-v3
          (prefix-clauses [])
          (prefix-context context)
          (clauses [clause])
          (used-arguments {})
          (pending-guards []))]
    (solve-rule-stack-v3
     final-symbols
     [initial-frame]
     (array-rel final-symbols []))))

(defn- resolve-rule-request-v3
  [request]
  (let [context
        (upd-default-source
         (:context request) (:clause request))
        relation
        (solve-rule-v3 context (:clause request))]
    (hash-join-rel context relation)))

(defn collect-consts
  [^:vector<tuple<string;int>> symbols-indexed
   specimen
   constants]
  (reduce
   (fn [_ symbol-index]
     (let [symbol (tuple-get symbol-index 0)
           index (tuple-get symbol-index 1)]
       (if-some [value (get constants symbol)]
         (aset specimen index (Some value))
         (Stdlib.ignore 0)))
     (Stdlib.ignore 0))
   (Stdlib.ignore 0)
   symbols-indexed))

(defn- collect-copy-indexes
  [^:vector<tuple<string;int>> symbols-indexed
   offsets]
  (reduce
   (fn [indexes symbol-index]
     (let [symbol (tuple-get symbol-index 0)
           target-index (tuple-get symbol-index 1)]
       (if-some [source-index (get offsets symbol)]
         (conj indexes (tuple source-index target-index))
         indexes)))
   []
   symbols-indexed))

(defn- ^collect-specimen-v3 copy-collect-specimen
  [^collect-specimen-v3 specimen
   ^:array<datascript.lg.query-types/result> row
   ^:vector<tuple<int;int>> copy-indexes]
  (let [copy (da/aclone specimen)]
    (reduce
     (fn [_ copy-index]
       (aset
        copy
        (tuple-get copy-index 1)
        (Some
         (aget row (tuple-get copy-index 0))))
       (Stdlib.ignore 0))
     (Stdlib.ignore 0)
     copy-indexes)
    copy))

(defn- ^:vector<collect-specimen-v3> expand-output-specimen
  [^relation-v3 relation
   ^:vector<tuple<int;int>> copy-indexes
   ^collect-specimen-v3 specimen]
  (mapv
   (fn [row]
     (copy-collect-specimen specimen row copy-indexes))
   (relation-tuples relation)))

(defn- ^:vector<collect-specimen-v3> expand-output-specimens
  [^relation-v3 relation
   ^:vector<tuple<int;int>> copy-indexes
   ^:vector<collect-specimen-v3> specimens]
  (vec
   (mapcat
    (fn [specimen]
      (expand-output-specimen
       relation copy-indexes specimen))
    specimens)))

(defn ^collect-transform-v3 collect-rel-xf
  [^:vector<tuple<string;int>> symbols-indexed
   ^relation-v3 relation]
  (let [copy-indexes
        (collect-copy-indexes
         symbols-indexed
         (relation-offset-map relation))]
    (RelationCollectTransformV3
     relation copy-indexes)))

(defn- ^:array<datascript.lg.query-types/result>
  require-collect-row
  [^:vector<string> symbols
   ^collect-specimen-v3 specimen]
  (to-array
   (reduce-kv
    (fn [values index value]
      (match value
        (Some result) (conj values result)
        None
        (Stdlib.invalid_arg
         (str
          "Query find variable is not bound: "
          (nth symbols index)))))
    []
    (vec specimen))))

(defn- ^:vector<collect-specimen-v3> apply-collect-transform
  [^:vector<collect-specimen-v3> specimens
   ^collect-transform-v3 transform]
  (match transform
    (RelationCollectTransformV3 relation copy-indexes)
    (expand-output-specimens
     relation copy-indexes specimens)))

(defn- add-aggregate-context-value
  [state variable value]
  (let [seen (assoc (:seen state) variable true)]
    (if-some [value value]
      (record aggregate-context-state-v3
        (seen seen)
        (attrs
         (assoc
          (:attrs state)
          variable
          (count (:values state))))
        (values (conj (:values state) value)))
      (record aggregate-context-state-v3
        (seen seen)
        (attrs (:attrs state))
        (values (:values state))))))

(defn- aggregate-state-relation
  [^aggregate-context-state-v3 state]
  (query-types/relation
   (:attrs state)
   [(to-array (:values state))]
   {}))

(defn- empty-aggregate-context-state []
  (record aggregate-context-state-v3
    (seen {})
    (attrs {})
    (values [])))

(defn- aggregate-constants-relation
  [context]
  (aggregate-state-relation
   (reduce-kv
    (fn [state
          variable
          value]
      (add-aggregate-context-value
       state variable (Some value)))
    (empty-aggregate-context-state)
    (context-constants context))))

(defn- aggregate-context-relation
  [context]
  (aggregate-state-relation
   (reduce
    (fn [state
          relation]
      (let [first-row (first (relation-tuples relation))]
        (reduce-kv
         (fn [state
               variable
               index]
           (if (contains? (:seen state) variable)
             state
             (add-aggregate-context-value
              state
              variable
              (if-some [row first-row]
                (query-types/row-get row index)
                None))))
         state
         (relation-offset-map relation))))
    (empty-aggregate-context-state)
    (context-relations context))))

(defn- pull-database
  [context elements]
  (loop [remaining elements]
    (if-some [element (first remaining)]
      (if-some [pull (parser/find-element-pull element)]
        (let [source-name (parser/pull-source-name pull)]
          (if-some [source
                    (get (context-sources context) source-name)]
            (if-some [database (query-types/source-database source)]
              database
              (Stdlib.invalid_arg
               (str
                "Query source is not a database: "
                source-name)))
            (Stdlib.invalid_arg
             (str "Query source is not bound: " source-name))))
        (recur (subvec remaining 1)))
      (Stdlib.invalid_arg
       "Pull find requires a database source"))))

(defn collect-to
  ([^query-context-v3 context
    ^:vector<string> symbols
    ^:vector<array<datascript.lg.query-types/result>> acc]
   (collect-to
    context symbols acc []
    (da/make-array (count symbols) None)))
  ([^query-context-v3 context
    ^:vector<string> symbols
    ^:vector<array<datascript.lg.query-types/result>> acc
    ^:vector<collect-transform-v3> transforms]
   (collect-to
    context symbols acc transforms
    (da/make-array (count symbols) None)))
  ([^query-context-v3 context
    ^:vector<string> symbols
    ^:vector<array<datascript.lg.query-types/result>> acc
    ^:vector<collect-transform-v3> transforms
    ^collect-specimen-v3 specimen]
   (match context
     EmptyContextV3 acc
     (QueryContextV3 state)
     (let [symbols-indexed
           (reduce-kv
            (fn [indexed index symbol]
              (conj indexed (tuple symbol index)))
            []
            symbols)
           _ (collect-consts
              symbols-indexed specimen (:consts state))
           relation-transforms
           (mapv
            (fn [relation]
              (collect-rel-xf symbols-indexed relation))
            (related-rels context symbols))
           specimens
           (reduce
            apply-collect-transform
            [specimen]
            (into relation-transforms transforms))]
       (reduce
        (fn [rows collected]
          (conj rows (require-collect-row symbols collected)))
        acc
        specimens)))))

(defn ^datascript.lg.query-types/output q-closed
  [^datascript.parser/Query query
   ^:vector<datascript.lg.query-types/input> inputs]
  (let [descriptors
        (match (parser/static-query-inputs query)
          (Some descriptors) descriptors
          None
          (Stdlib.invalid_arg
           "Query-v3 requires statically representable inputs"))
        context
        (resolve-ins
         (context-v3 [] {})
         descriptors
         (vec inputs))
        context
        (resolve-clauses context (.-qwhere query))
        find (.-qfind query)
        find-elements (parser/find-spec-elements find)
        pull-patterns
        (if (some parser/pull? find-elements)
          (query-types/resolve-pull-patterns
           find-elements
           (aggregate-constants-relation context))
          [])
        find-variables
        (match (parser/find-projection-variable-names find)
          (Some variables) variables
          None
          (Stdlib.invalid_arg
           "Query-v3 find currently supports variables only"))
        with-variables
        (query-types/query-with-variable-names query)
        all-variables
        (vec (concat find-variables with-variables))
        collected
        (query-types/distinct-rows
         (collect-to context all-variables []))
        projected
        (if (empty? with-variables)
          collected
          (let [indexes
                (to-array (range (count find-variables)))]
            (mapv
             (fn [row]
               (query-types/project-row row indexes))
             collected)))
        rows
        (if (some parser/aggregate? find-elements)
          (query-types/aggregate-rows
           find-elements
           (aggregate-constants-relation context)
           (aggregate-context-relation context)
           projected)
          projected)
        rows
        (if (some parser/pull? find-elements)
          (query-types/pull-rows
           (pull-database context find-elements)
           (context-sources context)
           find-elements
           pull-patterns
           rows)
          rows)]
    (query-types/find-output
     find
     (.-qreturn-map query)
     rows)))

(defn q
  {:inline
   (fn [query & inputs]
     (list
      'datascript.query-v3/q-closed
      query
      (vec inputs)))}
  [query
   & inputs]
  (q-closed query (vec inputs)))
