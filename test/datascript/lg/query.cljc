(ns datascript.lg.query
  (:require
   [clojure.string :as string]
   [datascript.built-ins :as built-ins]
   [datascript.db]
   [datascript.lg.query-types :as query-types]
   [datascript.lru :as lru]
   [datascript.parser]))

(defn- append-rule-clauses [left right]
  (reduce
   (fn [clauses clause]
     (conj clauses clause))
   left
   right))

(defn- append-rule-frames [left right]
  (reduce
   (fn [frames frame]
     (conj frames frame))
   left
   right))

(def ^:dynamic *lookup-attrs*
  (set-of :string))

(def ^:dynamic *implicit-source*
  None)

(def ^:dynamic *implicit-source-name*
  "$")

(def ^:dynamic *query-cache*
  (lru/cache 100))

(defn- attrs-string [attrs]
  (str
   "{"
   (string/join
    ", "
    (mapv
     (fn [variable]
       (str
        "\""
        variable
        "\" "
        (get attrs variable 0)))
     (keys attrs)))
   "}"))

(defn intersect-keys
  [left right]
  (reduce
   (fn [shared key]
     (if (contains? right key)
       (conj shared key)
       shared))
   (set-of :string)
   (keys left)))

(defn same-keys?
  [left right]
  (and
   (= (count left) (count right))
   (every?
    (fn [key] (contains? right key))
    (keys left))
   (every?
   (fn [key] (contains? left key))
   (keys right))))

(defn- same-attrs?
  [left right]
  (and
   (= (count left) (count right))
   (reduce-kv
    (fn [equal variable left-index]
      (and
       equal
       (if-some [right-index (get right variable)]
         (= left-index right-index)
         false)))
    true
    left)))

(defn getter-fn
  [attrs attr]
  (if-some [index (get attrs attr)]
    (fn [row]
      (let [result (query-types/require-row-result row index)]
        (if (contains? *lookup-attrs* attr)
          (match *implicit-source*
            None result
            (Some database)
            (query-types/resolve-lookup-result database result))
          result)))
    (Stdlib.invalid_arg
     (str "Unknown relation attribute " attr))))

(defn tuple-key-fn
  [attrs  common-attrs]
  (if (= 1 (count common-attrs))
    (let [getter (getter-fn attrs (nth common-attrs 0))]
      (fn [row]
        (SingleTupleKey (getter row))))
    (let [getters (mapv
                   (fn [attr]
                     (getter-fn attrs attr))
                   common-attrs)]
      (fn [row]
        (CompositeTupleKey
         (mapv
          (fn [getter]
            (getter row))
          getters))))))

(defn bound-vars
  [context]
  (reduce
   (fn [bound relation]
     (reduce
      (fn [bound variable]
        (conj bound variable))
      bound
      (keys (query-types/relation-attrs relation))))
   (set-of :string)
   (query-types/context-relations context)))

(defn- add-aggregate-context-value
  [state
    variable
    value]
  (let [seen (assoc (:seen state) variable true)]
    (if-some [value value]
      (record aggregate-context-state
        (seen seen)
        (attrs
         (assoc
          (:attrs state)
          variable
          (count (:values state))))
        (values (conj (:values state) value)))
      (record aggregate-context-state
        (seen seen)
        (attrs (:attrs state))
        (values (:values state))))))

(defn- aggregate-context-relation
  [context]
  (let [state
        (reduce
         (fn [state relation]
           (let [first-row
                 (first (query-types/relation-rows relation))]
             (reduce-kv
              (fn [state
                    variable
                    index]
                (if
                 (contains?
                  (:seen state)
                  variable)
                  state
                  (add-aggregate-context-value
                   state
                   variable
                   (if-some [row first-row]
                     (query-types/row-get row index)
                     None))))
              state
              (query-types/relation-attrs relation))))
         (record aggregate-context-state
           (seen {})
           (attrs {})
           (values []))
         (query-types/context-relations context))]
    (query-types/relation
     (:attrs state)
     [(to-array (:values state))]
     {})))

(defn -aggregate
  [find-elements
    context
    tuples]
  (query-types/aggregate-group-row
   find-elements
   (query-types/identity-relation)
   (aggregate-context-relation context)
   tuples))

(defn aggregate
  [find-elements
    context
    resultset]
  (query-types/aggregate-rows
   find-elements
   (query-types/identity-relation)
   (aggregate-context-relation context)
   resultset))

(defn- binding-source
  [binding]
  (if (datascript.parser/binding-ignore? binding)
    "_"
    (if-some [variable
              (datascript.parser/binding-scalar-variable binding)]
      variable
      (if-some [items
                (datascript.parser/binding-tuple-items binding)]
        (str
         "["
         (string/join " " (mapv binding-source items))
         "]")
        (if-some [item
                  (datascript.parser/binding-collection-item binding)]
          (str "[" (binding-source item) " ...]")
          "_")))))

(defn- input-binding-source
  [binding]
  (if-some [source
            (first
             (datascript.parser/input-binding-sources binding))]
    (str (.-symbol source))
    (if (datascript.parser/input-binding-rules? binding)
      "%"
      (if-some [value-binding
                (datascript.parser/query-input-value-binding binding)]
        (binding-source value-binding)
        "_"))))

(defn- input-bindings-source
  [bindings]
  (str
   "["
   (string/join " " (mapv input-binding-source bindings))
   "]"))

(defn resolve-in
  [context binding-and-input]
  (let [binding (tuple-get binding-and-input 0)
        input (tuple-get binding-and-input 1)]
    (if-some [source
              (first
               (datascript.parser/input-binding-sources binding))]
      (if-some [input-source (query-types/input-source input)]
        (query-types/context
         (query-types/context-relations context)
         (assoc
          (query-types/context-sources context)
          (str (.-symbol source))
          input-source)
         (query-types/context-rules context))
        (Stdlib.invalid_arg
         "Source query input requires a Source_input"))
      (if (datascript.parser/input-binding-rules? binding)
        (if-some [input-rules (query-types/input-rules input)]
          (query-types/context
           (query-types/context-relations context)
           (query-types/context-sources context)
           input-rules)
          (Stdlib.invalid_arg
           "Rules query input requires a Rules_input"))
        (if-some [value-binding
                  (datascript.parser/query-input-value-binding binding)]
          (query-types/context
           (conj
            (query-types/context-relations context)
            (query-types/binding-relation
             value-binding
             (query-types/require-binding-input input)))
           (query-types/context-sources context)
           (query-types/context-rules context))
          (Stdlib.invalid_arg
           "Unsupported static query input descriptor"))))))

(defn resolve-ins
  [context
    bindings
    inputs]
  (let [binding-count (count bindings)
        input-count (count inputs)]
    (cond
      (< binding-count input-count)
      (Stdlib.invalid_arg
       (str
        "Extra inputs passed, expected: "
        (input-bindings-source bindings)
        ", got: "
        input-count))

      (> binding-count input-count)
      (Stdlib.invalid_arg
       (str
        "Too few inputs passed, expected: "
        (input-bindings-source bindings)
        ", got: "
        input-count))

      :else
      (loop [context context
             remaining-bindings bindings
             remaining-inputs inputs]
        (if-some [binding (first remaining-bindings)]
          (if-some [input (first remaining-inputs)]
            (recur
             (resolve-in context (tuple binding input))
             (subvec remaining-bindings 1)
             (subvec remaining-inputs 1))
            context)
          context)))))

(defn parse-rules
  [rules]
  (match rules
    (Datascript_runtime.Data_value.String source)
    (datascript.parser/parse-rules
     (Datascript_runtime.Serialization_value.data_value_of_edn_string
      source))
    _ (datascript.parser/parse-rules rules)))

(defn- rule-head
  [clause]
  (if-some
   [items
    (Datascript_runtime.Data_value.sequential_items clause)]
    (if-some [first-item (first items)]
      (if (source? first-item)
        (if (< 1 (count items))
          (Some (nth items 1))
          None)
        (Some first-item))
      None)
    None))

(defn- reserved-rule-name? [rule-name]
  (or
   (= rule-name "_")
   (= rule-name "or")
   (= rule-name "or-join")
   (= rule-name "and")
   (= rule-name "not")
   (= rule-name "not-join")))

(defn rule?
  [context
    clause]
  (if-some [head (rule-head clause)]
    (match head
      (Datascript_runtime.Data_value.Symbol rule-name)
      (if
       (or
        (free-var? head)
        (reserved-rule-name? rule-name))
        false
        (if
         (some?
          (datascript.parser/rule-branches
           (query-types/context-rules context)
           rule-name))
          true
          (Stdlib.invalid_arg
           (str
            "Unknown rule '"
            rule-name
            " in "
            (Datascript_runtime.Data_value.to_edn_string
             clause)))))
      _ false)
    false))

(defn empty-rel
  [binding]
  (query-types/empty-relation
   (query-types/index-attrs
    (datascript.parser/binding-variable-names binding))
   (query-types/empty-lookup-databases)))

(defprotocol IBinding
  (in->rel
   [binding value]
   :datascript.lg.query-types/relation))

(extend-type datascript.parser/binding
  IBinding
  (in->rel [binding value]
    (query-types/binding-relation binding value)))

(defn limit-rel
  [relation
    variables]
  (let [attrs
        (reduce-kv
         (fn [selected
               variable
               index]
           (if (contains? variables variable)
             (assoc selected variable index)
             selected))
         {}
         (query-types/relation-attrs relation))]
    (if (empty? attrs)
      None
      (let [lookup-databases
            (reduce
             (fn [selected
                   variable]
               (if-some [database
                         (query-types/relation-lookup-database
                          relation variable)]
                 (assoc selected variable database)
                 selected))
             {}
             (keys attrs))]
        (Some
         (query-types/relation
          attrs
          (query-types/relation-rows relation)
          lookup-databases))))))

(defn limit-context
  [context
    variables]
  (let [relations
        (reduce
         (fn [limited relation]
           (if-some [relation (limit-rel relation variables)]
             (conj limited relation)
             limited))
         []
         (query-types/context-relations context))]
    (query-types/context
     relations
     (query-types/context-sources context)
     (query-types/context-rules context))))

(defn prod-rel
  ([]
   (query-types/identity-relation))
  ([left
     right]
   (query-types/product-relation left right)))

(defn- sum-rel*
  [left right]
  (let [left-attrs (query-types/relation-attrs left)
        right-attrs (query-types/relation-attrs right)
        left-rows (query-types/relation-rows left)
        right-rows (query-types/relation-rows right)
        indexes (Array.make (count left-attrs) 0)
        _indexed
        (reduce-kv
         (fn [_ignored variable left-index]
           (aset indexes left-index (get right-attrs variable 0))
           (Stdlib.ignore 0))
         (Stdlib.ignore 0)
         left-attrs)
        reordered
        (mapv
         (fn [row]
           (query-types/project-row row indexes))
         right-rows)]
    (query-types/relation
     left-attrs
     (vec (concat left-rows reordered))
     (query-types/merge-lookup-databases
      (query-types/relation-lookup-databases left)
      (query-types/relation-lookup-databases right)))))

(defn sum-rel
  [left right]
  (let [left-attrs (query-types/relation-attrs left)
        right-attrs (query-types/relation-attrs right)
        left-rows (query-types/relation-rows left)
        right-rows (query-types/relation-rows right)]
    (cond
      (same-attrs? left-attrs right-attrs)
      (query-types/relation
       left-attrs
       (into left-rows right-rows)
       (query-types/merge-lookup-databases
        (query-types/relation-lookup-databases left)
        (query-types/relation-lookup-databases right)))
      (empty? left-rows) right
      (empty? right-rows) left
      (not (same-keys? left-attrs right-attrs))
      (Stdlib.invalid_arg
       (str
        "Can’t sum relations with different attrs: "
        (attrs-string left-attrs)
        " and "
        (attrs-string right-attrs)))
      :else (sum-rel* left right))))

(defn hash-join
  [left
    right]
  (query-types/hash-join left right))

(defn subtract-rel
  [left
    right]
  (Datascript_runtime.Query_value.subtract_relation
   left right))

(defn join-tuples
  [left
    left-indexes
    right
    right-indexes]
  (query-types/join-rows
   left left-indexes right right-indexes))

(defn collapse-rels
  [relations
    new-relation]
  (loop [remaining relations
         new-relation new-relation
         collapsed []]
    (if-some [relation (first remaining)]
      (if (not
           (empty?
            (intersect-keys
             (query-types/relation-attrs new-relation)
             (query-types/relation-attrs relation))))
        (recur
         (subvec remaining 1)
         (hash-join relation new-relation)
         collapsed)
        (recur
         (subvec remaining 1)
         new-relation
         (conj collapsed relation)))
      (conj collapsed new-relation))))

(defn source?
  [form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (= value "")
      false
      (= (String.get value 0) \$))
    _ false))

(defn free-var?
  [form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (= value "")
      false
      (= (String.get value 0) \?))
    _ false))

(defn attr?
  [form]
  (match form
    (Datascript_runtime.Data_value.Keyword _) true
    (Datascript_runtime.Data_value.String _) true
    _ false))

(defn lookup-ref?
  [form]
  (if-some [items
            (Datascript_runtime.Data_value.sequential_items form)]
    (if (= 2 (count items))
      (if-some [attribute (first items)]
        (attr? attribute)
        false)
      false)
    false))

(defn matches-pattern?
  [pattern
    tuple]
  (loop [tuple tuple
         pattern pattern]
    (if-some [tuple-value (first tuple)]
      (if-some [pattern-value (first pattern)]
        (if
         (or
          (Datascript_runtime.Data_value.equal
           pattern-value
           (Datascript_runtime.Data_value.Symbol "_"))
          (free-var? pattern-value)
          (Datascript_runtime.Data_value.equal
           tuple-value pattern-value))
          (recur (subvec tuple 1) (subvec pattern 1))
          false)
        true)
      true)))

(defn- rel-with-attr
  [context
    variable]
  (loop [relations (query-types/context-relations context)]
    (if-some [relation (first relations)]
      (if
       (contains?
        (query-types/relation-attrs relation)
        variable)
        (Some relation)
        (recur (subvec relations 1)))
      None)))

(defn- context-resolve-val
  [context
    variable]
  (if-some [relation (rel-with-attr context variable)]
    (if-some [row (first (query-types/relation-rows relation))]
      (if-some [result
                (query-types/relation-result
                 relation variable row)]
        (Some (ContextResult result))
        None)
      None)
    None))

(defprotocol IContextResolve
  (-context-resolve [value context] :option<context-resolution>))

(extend-type datascript.parser/Variable
  IContextResolve
  (-context-resolve [variable context]
    (context-resolve-val
     context
     (str (.-symbol variable)))))

(extend-type datascript.parser/SrcVar
  IContextResolve
  (-context-resolve [source context]
    (if-some [resolved
              (get
               (query-types/context-sources context)
               (str (.-symbol source)))]
      (Some (ContextSource resolved))
      None)))

(extend-type datascript.parser/PlainSymbol
  IContextResolve
  (-context-resolve [plain-symbol _]
    (if-some [aggregate
              (built-ins/aggregate-function
               (str (.-symbol plain-symbol)))]
      (Some (ContextAggregate aggregate))
      None)))

(extend-type datascript.parser/Constant
  IContextResolve
  (-context-resolve [constant _]
    (Some
     (ContextResult
      (query-types/value-result
       (.-value constant))))))

(defn -collect-tuples
  [acc
    relation
    length
    copy-map]
  (Datascript_runtime.Query_value.collect_tuples
   acc relation length copy-map))

(defn -collect
  ([context
     symbols]
   (-collect
    [(Array.make (count symbols) None)]
    (query-types/context-relations context)
    symbols))
  ([acc
     relations
     symbols]
   (if-some [relation (first relations)]
     (if (empty? (query-types/relation-rows relation))
       []
       (let [attrs (query-types/relation-attrs relation)]
         (if
          (some
           (fn [symbol]
             (contains? attrs symbol))
           symbols)
           (-collect
            (-collect-tuples
             acc
             relation
             (count symbols)
             (to-array
              (mapv
               (fn [symbol]
                 (get attrs symbol))
               symbols)))
            (subvec relations 1)
            symbols)
           (-collect acc (subvec relations 1) symbols))))
     acc)))

(defn collect
  [context
    symbols]
  (Datascript_runtime.Query_value.distinct_optional_rows
   (-collect context symbols)))

(defn
  substitute-constant
  [context
    pattern-element]
  (if (free-var? pattern-element)
    (let [variable
          (Datascript_runtime.Data_value.to_edn_string
           pattern-element)]
      (if-some [relation
                (rel-with-attr context variable)]
        (let [rows (query-types/relation-rows relation)]
          (if (= 1 (count rows))
            (if-some [row (first rows)]
              (if-some
               [result
                (query-types/relation-result
                 relation variable row)]
                (Some
                 (query-types/result-pattern-value result))
                None)
              None)
            None))
        None))
    None))

(defn
  substitute-constants
  [context
    pattern]
  (mapv
   (fn [pattern-element]
     (match (substitute-constant context pattern-element)
       None pattern-element
       (Some value) value))
   pattern))

(defn-
  pattern-value-at
  [pattern
    index]
  (if (< index (count pattern))
    (Some (nth pattern index))
    None))

(defn- add-free-pattern-variable
  [variables pattern-value]
  (match pattern-value
    None variables
    (Some value)
    (if (free-var? value)
      (conj
       variables
       (Datascript_runtime.Data_value.to_edn_string value))
      variables)))

(defn dynamic-lookup-attrs
  [database
    pattern]
  (let [entity (pattern-value-at pattern 0)
        attr (pattern-value-at pattern 1)
        value (pattern-value-at pattern 2)
        tx (pattern-value-at pattern 3)
        variables
        (add-free-pattern-variable
         (add-free-pattern-variable (set-of :string) entity)
         tx)]
    (match (tuple attr value)
      (tuple (Some attr) (Some value))
      (if
       (and
        (free-var? value)
        (not (free-var? attr))
        (if-some
         [attr-name
          (Datascript_runtime.Data_value.keyword_value attr)]
          (datascript.db/database-view-ref?
           database (keyword attr-name))
          false))
        (add-free-pattern-variable variables (Some value))
        variables)
      _ variables)))

(defn-
  strict-pattern-entid
  [database
    value]
  (if-some [entity-ref (query-types/query-entity-ref value)]
    (if-some [eid
              (datascript.db/database-view-entid
               database entity-ref)]
      (Datascript_runtime.Data_value.Int eid)
      (Stdlib.invalid_arg
       (str
        "Nothing found for entity id "
        (Datascript_runtime.Data_value.to_edn_string value))))
    (Stdlib.invalid_arg
     (str
      "Expected number or lookup ref for entity id, got "
      (Datascript_runtime.Data_value.to_edn_string value)))))

(defn-
  pattern-value-or-nil
  [value]
  (match value
    None (Datascript_runtime.Data_value.Nil)
    (Some value) value))

(defn
  resolve-pattern-lookup-refs
  [source
    pattern]
  (if-some [database (query-types/source-database source)]
    (let [entity (pattern-value-at pattern 0)
          attr (pattern-value-at pattern 1)
          value (pattern-value-at pattern 2)
          tx (pattern-value-at pattern 3)
          resolved-entity
          (match entity
            None None
            (Some entity)
            (if (or (lookup-ref? entity) (attr? entity))
              (Some (strict-pattern-entid database entity))
              (Some entity)))
          resolved-value
          (match (tuple attr value)
            (tuple (Some attr) (Some value))
            (if
             (and
              (attr? attr)
              (if-some
               [attr-name
                (Datascript_runtime.Data_value.keyword_value
                 attr)]
                (datascript.db/database-view-ref?
                 database (keyword attr-name))
                false)
              (or (lookup-ref? value) (attr? value)))
              (Some (strict-pattern-entid database value))
              (Some value))
            _ value)
          resolved-tx
          (match tx
            None None
            (Some tx)
            (if (lookup-ref? tx)
              (Some (strict-pattern-entid database tx))
              (Some tx)))
          resolved
          [(pattern-value-or-nil resolved-entity)
           (pattern-value-or-nil attr)
           (pattern-value-or-nil resolved-value)
           (pattern-value-or-nil resolved-tx)]]
      (subvec resolved 0 (count pattern)))
    pattern))

(defn-
  parse-lookup-pattern
  [pattern]
  (if-some [parsed
            (datascript.parser/parse-pattern-elements pattern)]
    parsed
    (Stdlib.invalid_arg "Cannot parse DataScript lookup pattern")))

(defn lookup-pattern-db
  [context
    database
    pattern]
  (query-types/lookup-db-pattern
   database
   (parse-lookup-pattern
    (resolve-pattern-lookup-refs
     (query-types/database-view-source database)
     (substitute-constants context pattern)))))

(defn lookup-pattern-coll
  [context rows pattern]
  (query-types/resolve-relation-pattern
   rows
   (query-types/identity-relation)
   (parse-lookup-pattern
    (substitute-constants context pattern))))

(defn lookup-pattern
  [context source pattern]
  (match source
    (Datascript_runtime.Query_value.Database_source database)
    (lookup-pattern-db context database pattern)
    (Datascript_runtime.Query_value.Relation_source rows)
    (lookup-pattern-coll context rows pattern)))

(defn-
  map-return-rows
  [keys
    tuples]
  (mapv
   (fn [tuple]
     (query-types/return-map-row keys tuple))
   tuples))

(defn tuples->return-map
  [return-map
    tuples]
  (if-some [keys
            (datascript.parser/return-map-key-names return-map)]
    (Datascript_runtime.Query_value.Keyword_relation_output
     (map-return-rows keys tuples))
    (if-some [keys
              (datascript.parser/return-map-symbol-names return-map)]
      (Datascript_runtime.Query_value.Symbol_relation_output
       (map-return-rows keys tuples))
      (if-some [keys
                (datascript.parser/return-map-string-names return-map)]
        (Datascript_runtime.Query_value.String_relation_output
         (map-return-rows keys tuples))
        (Stdlib.invalid_arg "Unsupported query return-map type")))))

(defmacro map* [f xs]
  `(let [f# ~f
         xs# ~xs]
     (reduce
      (fn [result# value#]
        (conj result# (f# value#)))
      (empty xs#)
      xs#)))

(defmacro -group-by [f init coll]
  `(let [f# ~f
         init# ~init
         coll# ~coll]
     (persistent!
      (reduce
       (fn [result# value#]
         (let [key# (f# value#)]
           (assoc!
            result#
            key#
            (conj (get result# key# init#) value#))))
       (transient {})
       coll#))))

(defmacro hash-attrs [key-fn tuples]
  `(datascript.lg.query/-group-by ~key-fn (list) ~tuples))

(defprotocol IPostProcess
  (-post-process
   [find return-map tuples]
   :datascript.lg.query-types/output))

(extend-type datascript.parser/FindRel
  IPostProcess
  (-post-process [find return-map tuples]
    (if-some [return-map return-map]
      (tuples->return-map return-map tuples)
      (query-types/find-output
       (datascript.parser/relation-find-elements
        (.-elements find))
       None
       tuples))))

(extend-type datascript.parser/FindColl
  IPostProcess
  (-post-process [find _ tuples]
    (query-types/find-output
     (datascript.parser/collection-find-element
      (.-element find))
     None
     tuples)))

(extend-type datascript.parser/FindScalar
  IPostProcess
  (-post-process [find _ tuples]
    (query-types/find-output
     (datascript.parser/single-find-element
      (.-element find))
     None
     tuples)))

(extend-type datascript.parser/FindTuple
  IPostProcess
  (-post-process [find return-map tuples]
    (query-types/find-output
     (datascript.parser/tuple-find-elements
      (.-elements find))
     return-map
     (if-some [tuple (first tuples)]
       [tuple]
       []))))

(defn normalize-pattern-clause
  [clause]
  (if-some [head (first clause)]
    (if (source? head)
      clause
      (vec
       (concat
        [(Datascript_runtime.Data_value.Symbol "$")]
        clause)))
    [(Datascript_runtime.Data_value.Symbol "$")]))

(defn- pattern-elements-equal?
  [left
    right]
  (if-some
   [left-variable
    (datascript.parser/pattern-element-variable-symbol left)]
    (if-some
     [right-variable
      (datascript.parser/pattern-element-variable-symbol right)]
      (= (str left-variable) (str right-variable))
      false)
    (if-some
     [left-constant
      (datascript.parser/pattern-element-constant left)]
      (if-some
       [right-constant
        (datascript.parser/pattern-element-constant right)]
        (Datascript_runtime.Data_value.equal
         left-constant right-constant)
        false)
      (and
       (not
        (some?
        (datascript.parser/pattern-element-variable-symbol
         right)))
       (not
        (some?
        (datascript.parser/pattern-element-constant
         right)))))))

(defn- remove-rule-argument-pairs
  [left right]
  (loop [index 0
         left-remaining []
         right-remaining []]
    (if
     (and (< index (count left))
          (< index (count right)))
      (let [left-value (nth left index)
            right-value (nth right index)]
        (if
         (pattern-elements-equal? left-value right-value)
          (recur
           (inc index)
           left-remaining
           right-remaining)
          (recur
           (inc index)
           (conj left-remaining left-value)
           (conj right-remaining right-value))))
      (tuple left-remaining right-remaining))))

(defn remove-pairs
  [left
   right]
  (loop [index 0
         left-remaining []
         right-remaining []]
    (if
     (and (< index (count left))
          (< index (count right)))
      (let [left-value (nth left index)
            right-value (nth right index)]
        (if
         (Datascript_runtime.Data_value.equal
          left-value right-value)
          (recur
           (inc index)
           left-remaining
           right-remaining)
          (recur
           (inc index)
           (conj left-remaining left-value)
           (conj right-remaining right-value))))
      (tuple left-remaining right-remaining))))

(defn- query-form-list
  [values]
  (Datascript_runtime.Data_value.List
   (into (list) (reverse values))))

(defn-
  rule-argument-as-fn-arg
  [argument]
  (if-some
   [variable
    (datascript.parser/pattern-element-variable-symbol
     argument)]
    (datascript.parser/variable-argument
     (str variable))
    (if-some
     [value
      (datascript.parser/pattern-element-constant argument)]
      (datascript.parser/constant-argument value)
      (datascript.parser/constant-argument
       (Datascript_runtime.Data_value.Symbol "_")))))

(defn- typed-rule-gen-guards
  [rule-name call-args used-args]
  (if-some [previous-calls (get used-args rule-name)]
    (mapv
     (fn [previous-args]
       (let [remaining
             (remove-rule-argument-pairs
              call-args previous-args)]
         (datascript.parser/static-predicate-clause
          "-differ?"
          (mapv
           rule-argument-as-fn-arg
           (into
            (tuple-get remaining 0)
            (tuple-get remaining 1))))))
     previous-calls)
    []))

(defn rule-gen-guards
  [rule-clause used-args]
  (if-some
   [items
    (Datascript_runtime.Data_value.sequential_items rule-clause)]
    (if-some [rule-form (first items)]
      (match rule-form
        (Datascript_runtime.Data_value.Symbol rule-name)
        (if-some [previous-calls (get used-args rule-name)]
          (let [call-args (subvec items 1)]
            (mapv
             (fn [
                  previous-args]
               (let [remaining
                     (remove-pairs call-args previous-args)
                     differ-call
                     (vec
                      (concat
                       [(Datascript_runtime.Data_value.Symbol
                         "-differ?")]
                       (tuple-get remaining 0)
                       (tuple-get remaining 1)))]
                 (Datascript_runtime.Data_value.vector_of_vector
                  [(query-form-list differ-call)])))
             previous-calls))
          [])
        _ [])
      [])
    []))

(def rule-seqid (atom 0))

(defn-
  expand-rule-branches
  [clause
    context]
  (if-some [parts (datascript.parser/rule-clause-parts clause)]
    (let [rule-name (tuple-get parts 0)
          arguments (tuple-get parts 1)
          seqid (swap! rule-seqid inc)]
      (if-some [branches
                (datascript.parser/rule-branches
                 (query-types/context-rules context)
                 rule-name)]
        (mapv
         (fn [branch]
           (datascript.parser/expand-rule-branch
            branch arguments seqid))
        branches)
        (Stdlib.invalid_arg
         (str
          "Unknown rule '"
          rule-name
          " in "
          (query-types/rule-call-description
           rule-name arguments)))))
    (Stdlib.invalid_arg
     "expand-rule expects a rule clause")))

(defn expand-rule
  [clause
    context

   _used-args]
  (expand-rule-branches clause context))

(declare walk-collect)

(defn walk-collect
  [form
    pred]
  (let [children
        (if-some
         [values (Datascript_runtime.Data_value.sequential_items form)]
          (reduce
           (fn [collected
                 value]
             (let [nested
                   (walk-collect value pred)]
               (vec (concat collected nested))))
           []
           values)
          (if-some
           [entries (Datascript_runtime.Data_value.map_entries form)]
            (reduce
             (fn [collected
                   entry]
               (vec
                (concat
                 collected
                 (walk-collect (tuple-get entry 0) pred)
                 (walk-collect (tuple-get entry 1) pred))))
             []
             entries)
            (if-some
             [values (Datascript_runtime.Data_value.set_items form)]
              (reduce
               (fn [collected
                     value]
                 (let [nested
                       (walk-collect value pred)]
                   (vec (concat collected nested))))
               []
               values)
              (if-some
               [values (Datascript_runtime.Data_value.tuple_items form)]
                (reduce
                 (fn [collected
                       value]
                   (match value
                     None collected
                     (Some value)
                     (let [nested
                           (walk-collect value pred)]
                       (vec (concat collected nested)))))
                 []
                 values)
                []))))]
    (if (pred form)
      (conj children form)
      children)))

(defn collect-vars
  [form]
  (reduce
   (fn [variables value]
     (conj
      variables
      (Datascript_runtime.Data_value.to_edn_string value)))
   (set-of :string)
   (walk-collect form (fn [value] (free-var? value)))))

(defn-
  guard-arguments
  [guard]
  (if-some
   [outer
    (Datascript_runtime.Data_value.sequential_items guard)]
    (if-some [call (first outer)]
      (if-some
       [call-items
        (Datascript_runtime.Data_value.sequential_items call)]
        (subvec call-items 1)
        [])
      [])
    []))

(defn
  split-guards
  [clauses
    guards]
  (let [bound (collect-vars clauses)]
    (reduce
     (fn [
          split
           guard]
       (if
        (every?
         (fn [variable]
           (contains?
            bound
            (Datascript_runtime.Data_value.to_edn_string
             variable)))
         (guard-arguments guard))
         (tuple
          (conj (tuple-get split 0) guard)
          (tuple-get split 1))
         (tuple
          (tuple-get split 0)
          (conj (tuple-get split 1) guard))))
     (tuple [] [])
     guards)))

(defn- clause-variable-names
  [clauses]
  (reduce
   (fn [names variable]
     (conj names (str (.-symbol variable))))
   (set-of :string)
   (datascript.parser/collect-vars-distinct clauses)))

(defn-
  split-typed-guards
  [bound
    guards]
  (reduce
   (fn [
        split
         guard]
     (if
      (every?
       (fn [variable]
         (contains? bound (str (.-symbol variable))))
       (datascript.parser/clause-vars guard))
       (tuple
        (conj (tuple-get split 0) guard)
        (tuple-get split 1))
       (tuple
        (tuple-get split 0)
        (conj (tuple-get split 1) guard))))
   (tuple [] [])
   guards))

(defn- missing-vars
  [bound variables]
  (reduce
   (fn [missing variable]
     (if (contains? bound variable)
       missing
       (conj missing variable)))
   (set-of :string)
   variables))

(defn- variable-set-string
  [variables]
  (str "#{" (string/join " " (vec variables)) "}"))

(defn- variable-sets-string
  [variable-sets]
  (str
   "["
   (string/join
    " "
    (mapv
     (fn [variables]
       (variable-set-string variables))
     variable-sets))
   "]"))

(defn check-bound
  [bound
    variables
    form]
  (let [missing (missing-vars bound variables)]
    (if (empty? missing)
      (Stdlib.ignore 0)
      (Stdlib.invalid_arg
       (str
        "Insufficient bindings: "
        (variable-set-string missing)
        " not bound in "
        (Datascript_runtime.Data_value.to_edn_string form))))))

(defn check-free-same
  [bound
    branches
    form]
  (let [free
        (mapv
         (fn [branch]
           (missing-vars bound (vec (collect-vars branch))))
         branches)
        same?
        (if-some [expected (first free)]
          (every?
           (fn [variables]
             (= expected variables))
           (subvec free 1))
          true)]
    (if same?
      (Stdlib.ignore 0)
      (Stdlib.invalid_arg
       (str
        "All clauses in 'or' must use same set of free vars, had "
        (variable-sets-string free)
        " in "
        (Datascript_runtime.Data_value.to_edn_string form))))))

(defn check-free-subset
  [bound
    variables
    branches]
  (let [free (missing-vars bound variables)]
    (reduce
     (fn [_ignored
           branch]
       (let [present (collect-vars branch)
             missing
             (reduce
              (fn [missing  variable]
                (if (contains? present variable)
                  missing
                  (conj missing variable)))
              (set-of :string)
              free)]
         (if (empty? missing)
           (Stdlib.ignore 0)
           (Stdlib.invalid_arg
            (str
             "All clauses in 'or' must use same set of free vars, had "
             (variable-set-string missing)
             " not bound in "
             (Datascript_runtime.Data_value.to_edn_string branch))))))
     (Stdlib.ignore 0)
     branches)))

(defn- context-database
  [context]
  (match *implicit-source*
    (Some database) database
    None
    (query-types/query-default-database
     (query-types/context-sources context))))

(defn-
  context-source-database-option
  [context
    source-name]
  (if-some [source
            (get
             (query-types/context-sources context)
             source-name)]
    (query-types/source-database source)
    None))

(defn- join-context-relations
  [context]
  (let [relations (query-types/context-relations context)]
    (if-some [first-relation (first relations)]
      (reduce
       (fn [relation
             next-relation]
         (query-types/hash-join relation next-relation))
       first-relation
       (subvec relations 1))
      (query-types/identity-relation))))

(defn-
  split-leading-non-rules
  [clauses]
  (loop [index 0]
    (if (< index (count clauses))
      (if-some [_rule
                (datascript.parser/rule-clause-parts
                 (nth clauses index))]
        (tuple
         (subvec clauses 0 index)
         (subvec clauses index))
        (recur (inc index)))
      (tuple clauses []))))

(defn- rule-guard-always-false?
  [guard]
  (match guard
    (datascript.parser/PredicateClause callable arguments)
    (and
     (empty? arguments)
     (if-some
      [name (datascript.parser/static-callable-name callable)]
       (= name "-differ?")
       false))
    _ false))

(defn- context-has-empty-relation?
  [context]
  (some
   (fn [relation]
     (empty? (query-types/relation-rows relation)))
   (query-types/context-relations context)))

(defn- rule-output-variables
  [arguments]
  (reduce
   (fn [variables
         argument]
     (if-some [variable
               (query-types/pattern-variable-name argument)]
       (if
        (some
         (fn [existing] (= existing variable))
         variables)
         variables
         (conj variables variable))
       variables))
   []
   arguments))

(defn-
  rule-result-relation
  [context
    variables]
  (let [joined (join-context-relations context)
        complete
        (query-types/ensure-empty-relation-variables
         joined variables)
        projected
        (query-types/project-relation-variables
         complete variables)]
    (query-types/relation-with-rows
     projected
     (query-types/distinct-rows
      (query-types/relation-rows projected)))))

(defn- make-rule-frame
  [prefix-variable-names
    prefix-context
    clauses
    used-args
    pending-guards]
  (record rule-frame
    (prefix-variable-names prefix-variable-names)
    (prefix-context prefix-context)
    (clauses clauses)
    (used-args used-args)
    (pending-guards pending-guards)))

(defn -resolve-clause
  ([context
     clause]
   (-resolve-clause context clause clause))
  ([context
     clause
     _orig-clause]
   (let [rule-path []
         resolved
         (query-types/resolve-static-clauses
          (context-database context)
          (query-types/context-sources context)
          *implicit-source-name*
          (join-context-relations context)
          (query-types/identity-relation)
          (query-types/context-rules context)
          rule-path
          [clause])]
     (query-types/context
      [resolved]
      (query-types/context-sources context)
      (query-types/context-rules context)))))

(defn- solve-rule-prefix
  [context
    clauses
    pattern-cache]
  (reduce
   (fn [current
         clause]
     (match clause
       (datascript.parser/PatternClause query-source pattern)
       (let [source-name
             (if-some [name
                       (datascript.parser/query-source-name query-source)]
               name
               *implicit-source-name*)
             relation (join-context-relations current)
             resolved
             (if-some [source
                       (get
                        (query-types/context-sources current)
                        source-name)]
               (if-some [database (query-types/source-database source)]
                 (query-types/resolve-db-pattern-cached
                  database relation pattern pattern-cache)
                 (if-some [rows (query-types/source-rows source)]
                   (query-types/resolve-relation-pattern
                    rows relation pattern)
                   (Stdlib.invalid_arg
                    (str "Unsupported query source: " source-name))))
               (Stdlib.invalid_arg
                (str "Query source is not bound: " source-name)))]
         (query-types/context
          [resolved]
          (query-types/context-sources current)
          (query-types/context-rules current)))
       _ (-resolve-clause current clause)))
   context
   clauses))

(defn -call-fn
  [context
    relation
    callable
    arguments]
  (let [database (context-database context)
        sources (query-types/context-sources context)
        constants (query-types/identity-relation)]
    (fn [row]
      (query-types/invoke-callable
       callable
       (query-types/callable-arguments
        database
        sources
        relation
        constants
        row
        arguments)))))

(defn solve-rule
  [context
    clause]
  (if-some [rule-parts
            (datascript.parser/rule-clause-parts clause)]
    (let [arguments (tuple-get rule-parts 1)
          source-name
          (if-some [name
                    (datascript.parser/rule-clause-source-name clause)]
            name
            "$")
          variables (rule-output-variables arguments)
          pattern-cache (atom {})]
      (binding
       [*implicit-source-name* source-name
        *implicit-source*
        (context-source-database-option
         context source-name)]
       (loop
        [stack
         [(make-rule-frame
           (set-of :string) context [clause] {} [])]
         result
         (query-types/empty-relation
          (query-types/index-attrs variables)
          (query-types/empty-lookup-databases))]
        (if-some [frame (first stack)]
          (let [split
                (split-leading-non-rules (:clauses frame))
                leading-clauses (tuple-get split 0)
                rule-clauses (tuple-get split 1)]
            (if-some [rule-clause (first rule-clauses)]
              (if-some [parts
                        (datascript.parser/rule-clause-parts
                         rule-clause)]
                (let [nested-rule-name (tuple-get parts 0)
                      call-args (tuple-get parts 1)
                      next-clauses (subvec rule-clauses 1)
                      guards
                      (typed-rule-gen-guards
                       nested-rule-name
                       call-args
                       (:used-args frame))
                      bound-variable-names
                      (into
                       (:prefix-variable-names frame)
                       (clause-variable-names leading-clauses))
                      guard-split
                      (split-typed-guards
                       bound-variable-names
                       (append-rule-clauses
                        guards
                        (:pending-guards frame)))
                      active-guards (tuple-get guard-split 0)
                      pending-guards (tuple-get guard-split 1)]
                  (if
                   (some rule-guard-always-false? active-guards)
                    (recur (subvec stack 1) result)
                    (let [prefix-clauses
                          (append-rule-clauses
                           leading-clauses
                           active-guards)
                          prefix-context
                          (solve-rule-prefix
                           (:prefix-context frame)
                           prefix-clauses
                           pattern-cache)]
                      (if
                       (context-has-empty-relation?
                        prefix-context)
                        (recur (subvec stack 1) result)
                        (let [previous-calls
                              (if-some
                               [calls
                                (get
                                 (:used-args frame)
                                 nested-rule-name)]
                                calls
                                [])
                              used-args
                              (assoc
                               (:used-args frame)
                               nested-rule-name
                               (conj previous-calls call-args))
                              branches
                              (expand-rule-branches
                               rule-clause context)
                              branch-frames
                              (mapv
                               (fn [
                                    branch]
                                 (make-rule-frame
                                  bound-variable-names
                                  prefix-context
                                  (append-rule-clauses
                                   branch next-clauses)
                                  used-args
                                  pending-guards))
                               branches)]
                          (recur
                           (append-rule-frames
                            branch-frames
                            (subvec stack 1))
                           result))))))
                (Stdlib.invalid_arg
                 "Expected a rule clause"))
              (let [resolved-context
                    (solve-rule-prefix
                     (:prefix-context frame)
                     leading-clauses
                     pattern-cache)
                    branch-result
                    (rule-result-relation
                     resolved-context variables)]
                (recur
                 (subvec stack 1)
                 (sum-rel result branch-result)))))
          result))))
    (Stdlib.invalid_arg
     "solve-rule expects a rule clause")))

(defn resolve-clause
  [context
    clause]
  (if
   (some
    (fn [relation]
      (empty? (query-types/relation-rows relation)))
   (query-types/context-relations context))
    context
    (if-some [_rule
              (datascript.parser/rule-clause-parts clause)]
      (query-types/context
       [(query-types/hash-join
         (join-context-relations context)
         (solve-rule context clause))]
       (query-types/context-sources context)
       (query-types/context-rules context))
      (-resolve-clause context clause))))

(defn filter-by-pred
  [context
    clause]
  (match clause
    (PredicateClause _callable _arguments)
    (-resolve-clause context clause)
    _
    (Stdlib.invalid_arg
     "filter-by-pred expects a predicate clause")))

(defn bind-by-fn
  [context
    clause]
  (match clause
    (FunctionClause _callable _arguments _binding)
    (-resolve-clause context clause)
    _
    (Stdlib.invalid_arg
     "bind-by-fn expects a function clause")))

(defn -q
  [context
    clauses]
  (binding [*implicit-source-name* "$"
            *implicit-source*
            (context-source-database-option
             context "$")]
    (reduce
     (fn [context
           clause]
       (resolve-clause context clause))
     context
     clauses)))

(defn q-closed
  [query inputs]
  (datascript.parser/validate-static-query-sources query)
  (if-some [descriptors
            (datascript.parser/static-query-inputs query)]
    (if (= (count descriptors) (count inputs))
      (let [bound
            (query-types/bind-static-query-inputs
             descriptors inputs)
            sources (query-types/context-sources bound)
            database
            (query-types/query-default-database sources)
            resolved
            (-q bound (.-qwhere query))
            relation (join-context-relations resolved)]
        (query-types/execute-resolved-query
         database
         sources
         query
         (query-types/identity-relation)
         relation))
      (Stdlib.invalid_arg
       "Static query input count does not match parsed :in"))
    (Stdlib.invalid_arg
     "Static query contains unsupported input bindings")))

(defn q-db
  [query database]
  (q-closed
   query
   [(query-types/source-input
     (query-types/database-view-source
      (datascript.db/database-view database)))]))

(defn q
  {:inline
   (fn [query & inputs]
     (if
      (if (vector? query)
        true
        (if (seq? query)
          (or
           (vector? (second query))
           (map? (second query)))
          false))
       (let [literal-source
             (if (vector? query)
               query
               (second query))
         source
         (if (map? literal-source)
           (let [find-items (or (:find literal-source) [])
                 with-items (or (:with literal-source) [])
                 input-items (or (:in literal-source) [])
                 key-items (or (:keys literal-source) [])
                 symbol-items (or (:syms literal-source) [])
                 string-items (or (:strs literal-source) [])
                 where-items (or (:where literal-source) [])
                 append-items
                 (fn [target items]
                   (reduce
                    (fn [result item]
                      (conj result item))
                    target
                    items))
                 source (append-items [:find] find-items)
                 source
                 (if (empty? with-items)
                   source
                   (append-items (conj source :with) with-items))
                 source
                 (if (empty? input-items)
                   source
                   (append-items (conj source :in) input-items))
                 source
                 (if (empty? key-items)
                   (if (empty? symbol-items)
                     (if (empty? string-items)
                       source
                       (append-items
                        (conj source :strs)
                        string-items))
                     (append-items (conj source :syms) symbol-items))
                   (append-items (conj source :keys) key-items))
                 source (conj source :where)]
             (append-items source where-items))
           literal-source)
         parsed
         (loop [remaining (drop 1 source)
                find-variables []
                with-variables []
                input-variables []
                return-map-type nil
                return-map-symbols []
                section :find]
           (let [item (first remaining)]
             (cond
               (empty? remaining)
               [find-variables
                with-variables
                input-variables
                return-map-type
                return-map-symbols
                []]

               (= item :where)
               [find-variables
                with-variables
                input-variables
                return-map-type
                return-map-symbols
                (vec (next remaining))]

               (= item :with)
               (recur
                (next remaining)
                find-variables
                with-variables
                input-variables
                return-map-type
                return-map-symbols
                :with)

               (= item :in)
               (recur
                (next remaining)
                find-variables
                with-variables
                input-variables
                return-map-type
                return-map-symbols
                :in)

               (or (= item :keys)
                   (= item :syms)
                   (= item :strs))
               (recur
                (next remaining)
                find-variables
                with-variables
                input-variables
                item
                []
                :return-map)

               (= section :with)
               (recur
                (next remaining)
                find-variables
                (conj with-variables item)
                input-variables
                return-map-type
                return-map-symbols
                section)

               (= section :in)
               (recur
                (next remaining)
                find-variables
                with-variables
                (conj input-variables item)
                return-map-type
                return-map-symbols
                section)

               (= section :return-map)
               (recur
                (next remaining)
                find-variables
                with-variables
                input-variables
                return-map-type
                (conj return-map-symbols item)
                section)

               :else
               (recur
                (next remaining)
                (conj find-variables item)
                with-variables
                input-variables
                return-map-type
                return-map-symbols
                section))))
         find-variables
         (first parsed)
         with-variables
         (second parsed)
         input-variables
         (first (next (next parsed)))
         return-map-type
         (first (next (next (next parsed))))
         return-map-symbols
         (first (next (next (next (next parsed)))))
         patterns
         (last parsed)
         value-form
         (fn [value]
           (if (nil? value)
             (list 'Datascript_runtime.Data_value.Nil)
             (if (regex? value)
               (list
                'Datascript_runtime.Data_value.Regex
                (regex-source value))
               (if (string? value)
                 (list
                  'Datascript_runtime.Data_value.String
                  value)
                 (if (char? value)
                   (list
                    'Datascript_runtime.Data_value.String
                    (str value))
                   (if (= value true)
                     (list
                      'Datascript_runtime.Data_value.Bool
                      true)
                     (if (= value false)
                       (list
                        'Datascript_runtime.Data_value.Bool
                        false)
                       (if (symbol? value)
                         (list
                          'Datascript_runtime.Data_value.Symbol
                          (str value))
                         (if (float? value)
                           (list
                            'Datascript_runtime.Data_value.Float
                            value)
                           (list
                            'Datascript_runtime.Data_value.Int
                            value))))))))))
         data-value-form
         (fn data-value-form [value]
           (if (vector? value)
             (list
              'Datascript_runtime.Data_value.vector_of_vector
              (vec (map data-value-form value)))
             (if (map? value)
               (list
                'Datascript_runtime.Data_value.map_of_data_map
                (reduce
                 (fn [entries entry]
                   (assoc
                    entries
                    (data-value-form (first entry))
                    (data-value-form (second entry))))
                 {}
                 value))
               (if (if (seq? value)
                     (= (str (first value)) "hash-set")
                     false)
                 (list
                  'Datascript_runtime.Data_value.set_of_vector
                  (vec (map data-value-form (next value))))
                 (if (keyword? value)
                   (list
                    'Datascript_runtime.Data_value.Keyword
                    (str value))
                   (value-form value))))))
         source-symbol?
         (fn [value]
           (if (symbol? value)
             (if (= (str value) "")
               false
               (= (subs (str value) 0 1) "$"))
             false))
         argument-form
         (fn [argument]
           (if (nil? argument)
             (list
              'datascript.parser/constant-argument
              (data-value-form argument))
             (if (symbol? argument)
               (if (source-symbol? argument)
                 (list
                  'datascript.parser/source-argument
                  (str argument))
                 (list
                  'datascript.parser/variable-argument
                  (str argument)))
               (list
                'datascript.parser/constant-argument
                (data-value-form argument)))))
         pull-pattern-item-form
         (fn [item]
           (if (= (str item) "*")
             (list
              'Datascript_runtime.Data_value.Keyword
              ":*")
             (data-value-form item)))
         pull-pattern-form
         (fn [pattern]
           (list
            'Datascript_runtime.Data_value.vector_of_vector
            (vec (map pull-pattern-item-form pattern))))
         collect-callable-variables
         (fn collect-callable-variables [variables form]
           (if (vector? form)
             (let [head (first form)]
               (if (seq? head)
                 (let [callable-name (first head)]
                   (if (if (symbol? callable-name)
                         (= (subs (str callable-name) 0 1) "?")
                         false)
                     (conj variables (str callable-name))
                     (if (and
                          (= (str callable-name) "complement")
                          (symbol? (second head)))
                       (conj variables (str (second head)))
                       (reduce
                        collect-callable-variables
                        variables form))))
                 (reduce collect-callable-variables variables form)))
             (if (seq? form)
               (reduce collect-callable-variables variables form)
               variables)))
         contains-string?
         (fn [values value]
           (loop [remaining values]
             (if (empty? remaining)
               false
               (if (= (first remaining) value)
                 true
                 (recur (next remaining))))))
         literal-rule-inputs
         (loop [bindings input-variables
                values inputs
                rules-inputs []]
           (if (or (empty? bindings) (empty? values))
             rules-inputs
             (recur
              (next bindings)
              (next values)
              (if (= (str (first bindings)) "%")
                (conj
                 rules-inputs
                 (let [value (first values)]
                   (if (if (seq? value)
                         (= (str (first value)) "quote")
                         false)
                     (second value)
                     value)))
                rules-inputs))))
         callable-rule-positions
         (reduce
          (fn [positions rules]
            (reduce
             (fn [positions branch]
               (let [head (first branch)
                     raw-parameters (next head)
                     parameters
                     (if (vector? (first raw-parameters))
                       (vec
                        (concat
                         (first raw-parameters)
                         (next raw-parameters)))
                       (vec raw-parameters))
                     callable-parameters
                     (collect-callable-variables
                      []
                      (next branch))]
                 (loop [remaining parameters
                        consumed []
                        positions positions]
                   (if (empty? remaining)
                     positions
                     (recur
                      (next remaining)
                      (conj consumed (first remaining))
                      (if
                       (contains-string?
                        callable-parameters
                        (str (first remaining)))
                        (conj
                         positions
                         [(str (first head)) (count consumed)])
                        positions))))))
             positions
             rules))
          []
          literal-rule-inputs)
         rule-callable-input-variables
         (reduce
          (fn [variables clause]
            (if (seq? clause)
              (let [explicit-source?
                    (source-symbol? (first clause))
                    rule-name
                    (if explicit-source?
                      (str (second clause))
                      (str (first clause)))
                    arguments
                    (if explicit-source?
                      (next (next clause))
                      (next clause))]
                (reduce
                 (fn [variables position]
                   (if (= rule-name (first position))
                     (let [index (second position)]
                       (let [argument (first (drop index arguments))]
                         (if (symbol? argument)
                           (conj variables (str argument))
                           variables)))
                     variables))
                 variables
                 callable-rule-positions))
              variables))
          []
          patterns)
         callable-variables
         (reduce
          (fn [variables element]
            (if (if (seq? element)
                  (= (str (first element)) "aggregate")
                  false)
              (conj variables (str (second element)))
              variables))
          (reduce
           collect-callable-variables
           rule-callable-input-variables
           patterns)
          find-variables)
         callable-binding?
         (fn [binding]
           (if (symbol? binding)
             (loop [remaining callable-variables]
               (if (empty? remaining)
                 false
                 (if (= (first remaining) (str binding))
                   true
                   (recur (next remaining)))))
             false))
         built-in-aggregate-name?
         (fn [name]
           (loop [remaining
                  ["sum" "avg" "median" "variance" "stddev"
                   "distinct" "min" "max" "rand" "sample"
                   "count" "count-distinct"]]
             (if (empty? remaining)
               false
               (if (= name (first remaining))
                 true
                 (recur (next remaining))))))
         static-custom-aggregate-elements
         (reduce
          (fn [elements element]
            (if (seq? element)
              (let [name (str (first element))]
                (if (or
                     (= name "aggregate")
                     (= name "pull")
                     (built-in-aggregate-name? name))
                  elements
                  (loop [remaining elements]
                    (if (empty? remaining)
                      (conj elements element)
                      (if (= name (str (first (first remaining))))
                        elements
                        (recur (next remaining)))))))
              elements))
          []
          find-variables)
         static-custom-aggregate-variable
         (fn [element]
           (str "?__lg_aggregate/" (first element)))
         namespaced-symbol?
         (fn [value]
           (if (symbol? value)
             (let [symbol-namespace (namespace value)]
               (if (nil? symbol-namespace)
                 false
                 (if (= symbol-namespace "")
                   false
                   (if (= symbol-namespace "clojure.string")
                     false
                     true))))
             false))
         static-custom-function-elements
         (reduce
          (fn [elements clause]
            (if (if (vector? clause)
                  (if (seq? (first clause))
                    (namespaced-symbol? (first (first clause)))
                    false)
                  false)
               (let [call (first clause)
                     name (str (first call))]
                 (loop [remaining elements]
                   (if (empty? remaining)
                     (conj elements call)
                     (if (= name (str (first (first remaining))))
                       elements
                       (recur (next remaining))))))
              elements))
          []
          patterns)
         static-custom-function-variable
         (fn [call]
           (str "?__lg_function/" (first call)))
         custom-function-call?
         (fn [call]
           (namespaced-symbol? (first call)))
         clause-callable-variable
         (fn [call]
           (if (custom-function-call? call)
             (static-custom-function-variable call)
             (str (first call))))
         find-element-form
         (fn [element]
           (if (symbol? element)
             (list
              'datascript.parser/variable-find-element
              (str element))
             (if (= (str (first element)) "pull")
               (let [explicit-source?
                     (source-symbol? (second element))
                     entity-variable
                     (if explicit-source?
                       (first (next (next element)))
                       (second element))]
                 (if (vector? (last element))
                   (if explicit-source?
                     (list
                      'datascript.parser/pull-source-find-element
                      (str (second element))
                      (str entity-variable)
                      (pull-pattern-form (last element)))
                     (list
                      'datascript.parser/pull-find-element
                      (str entity-variable)
                      (pull-pattern-form (last element))))
                   (if explicit-source?
                     (list
                      'datascript.parser/pull-source-variable-find-element
                      (str (second element))
                      (str entity-variable)
                      (str (last element)))
                     (list
                      'datascript.parser/pull-variable-find-element
                      (str entity-variable)
                      (str (last element))))))
               (let [function-name (str (first element))]
                 (if (or
                      (= function-name "aggregate")
                      (if (built-in-aggregate-name? function-name)
                        false
                        true))
                    (list
                     'datascript.parser/custom-aggregate-find-element
                     (if (= function-name "aggregate")
                       (str (second element))
                       (static-custom-aggregate-variable element))
                     (vec
                      (map
                       argument-form
                       (if (= function-name "aggregate")
                         (next (next element))
                         (next element)))))
                    (list
                     'datascript.parser/aggregate-find-element
                     function-name
                     (vec
                      (map argument-form (next element)))))))))
         find-spec-form
         (fn [elements]
           (if (= (str (last elements)) ".")
             (list
              'datascript.parser/single-find-element
              (find-element-form (first elements)))
             (if (= 1 (count elements))
               (if (vector? (first elements))
                 (let [items (first elements)]
                   (if (= (str (second items)) "...")
                     (list
                      'datascript.parser/collection-find-element
                      (find-element-form (first items)))
                     (list
                      'datascript.parser/tuple-find-elements
                      (vec (map find-element-form items)))))
                 (list
                  'datascript.parser/relation-find-elements
                  (vec (map find-element-form elements))))
               (list
                'datascript.parser/relation-find-elements
                (vec (map find-element-form elements))))))
         pattern-item
         (fn [item]
           (if (keyword? item)
             (list
              'datascript.parser/pattern-attribute
              item)
             (if (symbol? item)
               (if (= (str item) "_")
                 (list 'datascript.parser/pattern-placeholder)
                 (if (= (subs (str item) 0 1) "?")
                   (list
                    'datascript.parser/pattern-variable
                    (str item))
                   (list
                    'datascript.parser/pattern-constant
                    (data-value-form item))))
               (list
                'datascript.parser/pattern-constant
                (data-value-form item)))))
         scalar-value-form
         (fn [value]
           (list
            'datascript.lg.query-types/scalar-binding
            (list
             'datascript.lg.query-types/value-result
             (data-value-form value))))
         literal-collection-values
         (fn literal-collection-values [value]
           (if (vector? value)
             value
             (if (map? value)
               (vec value)
               (if (seq? value)
                 (if (= (str (first value)) "hash-set")
                   (vec (next value))
                   (if (= (str (first value)) "quote")
                     (literal-collection-values (second value))
                     nil))
                 nil))))
         binding-value-form
         (fn binding-value-form [binding value]
           (if (vector? binding)
             (if (or
                  (vector? (first binding))
                  (= (str (second binding)) "..."))
               (let [values (literal-collection-values value)]
                 (if (nil? values)
                   (list
                    'datascript.lg.query-types/invalid-binding
                    (str
                     "Cannot bind value "
                     value
                     " to collection "
                     binding))
                   (list
                    'datascript.lg.query-types/collection-binding
                    (vec
                     (map
                      (fn [item]
                        (binding-value-form (first binding) item))
                      values)))))
               (let [values (literal-collection-values value)]
                 (if (nil? values)
                   (list
                    'datascript.lg.query-types/invalid-binding
                    (str
                     "Cannot bind value "
                     value
                     " to tuple "
                     binding))
                   (if (loop [bindings binding
                              remaining-values values]
                         (if (empty? bindings)
                           true
                           (if (empty? remaining-values)
                             false
                             (recur
                              (next bindings)
                              (next remaining-values)))))
                      (list
                       'datascript.lg.query-types/collection-binding
                       (loop [bindings binding
                              remaining-values values
                              forms []]
                         (if (empty? bindings)
                           forms
                           (recur
                            (next bindings)
                            (next remaining-values)
                            (conj
                             forms
                             (binding-value-form
                              (first bindings)
                              (first remaining-values)))))))
                      (list
                       'datascript.lg.query-types/invalid-binding
                       (str
                        "Not enough elements in a collection "
                        value
                        " to bind tuple "
                        binding))))))
             (scalar-value-form value)))
         relation-row-form
         (fn [values]
           (list
            'to-array
            (vec
             (map
              (fn [value]
                (list
                 'datascript.lg.query-types/value-result
                 (data-value-form value)))
              values))))
         source-form
         (fn [value]
           (let [source-value
                 (if (if (seq? value)
                       (= (str (first value)) "quote")
                       false)
                   (second value)
                   value)]
             (list
              'datascript.lg.query-types/source-input
              (if (vector? source-value)
                (list
                 'datascript.lg.query-types/relation-source
                (vec (map relation-row-form source-value)))
                (list
                 'datascript.lg.query-types/database-view-source
                 (list
                  'datascript.db/database-view
                  value))))))
         callable-input-form
         (fn [value]
           (list
            'datascript.lg.query-types/binding-input
            (list
             'datascript.lg.query-types/scalar-binding
             (list
              'datascript.lg.query-types/callable-result
              (list
               'datascript.lg.query-types/callable
               value)))))
         input-form
         (fn [binding value]
           (if (callable-binding? binding)
             (callable-input-form value)
             (list
              'datascript.lg.query-types/binding-input
              (binding-value-form binding value))))
         binding-form
         (fn binding-form [binding]
           (if (vector? binding)
             (if (or
                  (vector? (first binding))
                  (= (str (second binding)) "..."))
               (list
                'datascript.parser/collection-input
                (binding-form (first binding)))
               (list
                'datascript.parser/tuple-input
                (vec (map binding-form binding))))
             (if (= (str binding) "_")
               (list 'datascript.parser/ignore-input)
               (list
                'datascript.parser/scalar-input
                (str binding)))))
         input-binding-form
         (fn [binding]
           (binding-form binding))
         clause-form
         (fn clause-form [clause]
           (if (seq? clause)
             (if
              (and
               (source-symbol? (first clause))
               (= (str (second clause)) "not"))
               (list
                'datascript.parser/static-source-not-clause
                (str (first clause))
                (vec (map clause-form (next (next clause))))
                (str clause))
               (if
                (and
                 (source-symbol? (first clause))
                 (= (str (second clause)) "not-join"))
                 (list
                  'datascript.parser/static-source-not-join-clause
                  (str (first clause))
                  (vec
                   (map
                    (fn [variable] (str variable))
                    (first (next (next clause)))))
                  (vec
                   (map clause-form
                        (next (next (next clause)))))
                  (str clause))
                 (if
                  (and
                   (source-symbol? (first clause))
                   (= (str (second clause)) "or"))
                   (list
                    'datascript.parser/static-source-or-clause
                    (str (first clause))
                    (vec (map clause-form (next (next clause))))
                    (str clause))
                   (if
                    (and
                     (source-symbol? (first clause))
                     (= (str (second clause)) "or-join"))
                     (let [join-variables
                           (first (next (next clause)))
                           required?
                           (vector? (first join-variables))
                           required
                           (if required?
                             (first join-variables)
                             [])
                           free
                           (if required?
                             (next join-variables)
                             join-variables)]
                       (list
                        'datascript.parser/static-source-or-join-clause
                        (str (first clause))
                        (vec
                         (map
                          (fn [variable] (str variable))
                          required))
                        (vec
                         (map
                          (fn [variable] (str variable))
                          free))
                        (vec
                         (map clause-form
                              (next (next (next clause)))))
                        (str clause)))
                     (if (= (str (first clause)) "not")
                       (list
                        'datascript.parser/static-not-clause
                        (vec (map clause-form (next clause)))
                        (str clause))
                       (if (= (str (first clause)) "not-join")
                         (list
                          'datascript.parser/static-not-join-clause
                          (vec
                           (map
                            (fn [variable] (str variable))
                            (second clause)))
                          (vec
                           (map clause-form
                                (next (next clause))))
                          (str clause))
                         (if (= (str (first clause)) "and")
                           (list
                            'datascript.parser/static-and-clause
                            (vec (map clause-form (next clause))))
                           (if (= (str (first clause)) "or")
                             (list
                              'datascript.parser/static-or-clause
                              (vec (map clause-form (next clause)))
                              (str clause))
                             (if (= (str (first clause)) "or-join")
                               (let [join-variables (second clause)
                                     required?
                                     (vector? (first join-variables))
                                     required
                                     (if required?
                                       (first join-variables)
                                       [])
                                     free
                                     (if required?
                                       (next join-variables)
                                       join-variables)]
                                 (list
                                  'datascript.parser/static-or-join-clause
                                  (vec
                                   (map
                                    (fn [variable] (str variable))
                                    required))
                                  (vec
                                   (map
                                    (fn [variable] (str variable))
                                    free))
                                  (vec
                                   (map clause-form
                                        (next (next clause))))
                                  (str clause)))
                               (if (source-symbol? (first clause))
                                 (list
                                  'datascript.parser/static-source-rule-clause
                                  (str (first clause))
                                  (str (second clause))
                                  (vec
                                   (map
                                    pattern-item
                                    (next (next clause)))))
                                 (list
                                  'datascript.parser/static-rule-clause
                                  (str (first clause))
                                  (vec
                                   (map pattern-item
                                        (next clause))))))))))))))
             (if (seq? (first clause))
               (let [call (first clause)]
                 (if (= 1 (count clause))
                   (list
                    (if
                     (or
                      (= (subs (str (first call)) 0 1) "?")
                      (custom-function-call? call))
                      'datascript.parser/variable-predicate-clause
                      'datascript.parser/static-predicate-clause)
                    (clause-callable-variable call)
                    (vec (map argument-form (next call))))
                   (list
                    (if
                     (or
                      (= (subs (str (first call)) 0 1) "?")
                      (custom-function-call? call))
                      'datascript.parser/variable-function-clause
                      'datascript.parser/static-function-clause)
                    (clause-callable-variable call)
                    (vec (map argument-form (next call)))
                    (input-binding-form (second clause)))))
               (if (source-symbol? (first clause))
                 (list
                  'datascript.parser/explicit-pattern-clause
                  (str (first clause))
                  (vec (map pattern-item (next clause))))
                 (list
                  'datascript.parser/pattern-clause
                  (vec (map pattern-item clause)))))))
         rules-form
         (fn [value]
           (let [rules
                 (if (if (seq? value)
                       (= (str (first value)) "quote")
                       false)
                   (second value)
                   value)]
             (list
              'datascript.parser/static-rules
              (vec
               (map
                (fn [branch]
                  (let [head (first branch)
                        parameters (next head)
                        required? (vector? (first parameters))
                        required
                        (if required? (first parameters) [])
                        free
                        (if required?
                          (next parameters)
                          parameters)]
                    (list
                     'datascript.parser/static-rule-branch-with-vars
                     (str (first head))
                     (vec
                      (map
                       (fn [parameter] (str parameter))
                       required))
                     (vec
                      (map
                       (fn [parameter] (str parameter))
                       free))
                     (vec
                      (map clause-form
                           (next branch))))))
                rules)))))
         default-source-input?
         (if (empty? input-variables)
           (loop [remaining patterns]
             (if (empty? remaining)
               false
               (if (seq? (first (first remaining)))
                 (recur (next remaining))
                 true)))
           false)
         value-bindings
         input-variables
         expected-inputs
         (if default-source-input?
           [true]
           value-bindings)
         input-status
         (loop [expected expected-inputs
                actual inputs]
           (if (empty? expected)
             (if (empty? actual) :equal :extra)
             (if (empty? actual)
               :too-few
               (recur (next expected) (next actual)))))
         expected-input-description
         (if default-source-input?
           "[$]"
           (str value-bindings))
         input-values
         (if default-source-input?
           (next inputs)
           inputs)
         input-forms
         (reduce
          (fn [forms call]
            (conj forms (callable-input-form (first call))))
          (reduce
           (fn [forms element]
             (conj forms (callable-input-form (first element))))
           (loop [bindings value-bindings
                  values input-values
                  forms []]
             (if (or (empty? bindings) (empty? values))
               forms
               (recur
                (next bindings)
                (next values)
                (conj
                 forms
                 (if (source-symbol? (first bindings))
                   (source-form (first values))
                   (if (= (str (first bindings)) "%")
                     (list
                      'datascript.lg.query-types/rules-input
                      (rules-form (first values)))
                     (input-form
                      (first bindings)
                      (first values))))))))
           static-custom-aggregate-elements)
          static-custom-function-elements)
         query-input-forms
         (reduce
          (fn [forms call]
            (conj
             forms
             (list
              'datascript.parser/make-static-value-input
              (list
               'datascript.parser/scalar-input
               (static-custom-function-variable call)))))
          (reduce
           (fn [forms element]
             (conj
              forms
              (list
               'datascript.parser/make-static-value-input
               (list
                'datascript.parser/scalar-input
                (static-custom-aggregate-variable element)))))
           (vec
            (map
             (fn [binding]
               (if (source-symbol? binding)
                 (list
                  'datascript.parser/make-static-source-input
                  (str binding))
                 (if (= (str binding) "%")
                   (list 'datascript.parser/make-static-rules-input)
                   (list
                    'datascript.parser/make-static-value-input
                    (input-binding-form binding)))))
             value-bindings))
           static-custom-aggregate-elements)
          static-custom-function-elements)
         query-form
         (list
          (if default-source-input?
            'datascript.parser/static-db-query-clauses-with-inputs
            'datascript.parser/static-query-clauses-with-inputs)
          (find-spec-form find-variables)
          (vec (map clause-form patterns))
          query-input-forms)
         query-form
         (if (empty? with-variables)
           query-form
           (list
            'datascript.parser/query-with
            query-form
            (vec
             (map
              (fn [variable] (str variable))
              with-variables))))
         query-form
         (if (nil? return-map-type)
           query-form
           (list
            (if (= return-map-type :keys)
              'datascript.parser/query-return-keys
              (if (= return-map-type :syms)
                'datascript.parser/query-return-symbols
                'datascript.parser/query-return-strings))
            query-form
            (vec
             (map
              (fn [key] (str key))
              return-map-symbols))))]
         (if (= input-status :equal)
           (list
            'datascript.lg.query/q-closed
            query-form
            (if default-source-input?
              (vec
               (cons
                (source-form (first inputs))
                input-forms))
              (vec input-forms)))
           (list
            'datascript.lg.query-types/invalid-query-output-after-validation
            query-form
            (str
             (if (= input-status :extra)
               "Extra inputs passed, expected: "
               "Too few inputs passed, expected: ")
             expected-input-description
             ", got: "
             (count inputs)))))
       (if (= 1 (count inputs))
         (if (vector? (first inputs))
           (list
            'datascript.lg.query/q-closed
            query
            (first inputs))
           (list
            'datascript.lg.query/q-closed
            query
            (vec inputs)))
         (list
          'datascript.lg.query/q-closed
          query
          (vec inputs)))))}
  [query
   & inputs]
  (q-closed query (vec inputs)))
