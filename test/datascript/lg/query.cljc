(ns datascript.lg.query
  (:require
   [clojure.string :as string]
   [datascript.built-ins :as built-ins]
   [datascript.db]
   [datascript.lg.query-types :as query-types]
   [datascript.lru :as lru]
   [datascript.parser]))

(type-variant context-resolution
  (ContextResult :datascript.lg.query-types/result)
  (ContextSource :datascript.lg.query-types/source)
  (ContextAggregate
   :datascript.built-ins/built-in-aggregate-function))

(type-alias collect-row
  :array<option<datascript.lg.query-types/result>>)

(type-alias rule-arguments
  :vector<datascript.parser/pattern-element>)

(type-alias rule-call-history
  :map<string;vector<rule-arguments>>)

(type-record rule-frame
  (prefix-clauses :vector<datascript.parser/clause>)
  (prefix-context :datascript.lg.query-types/context)
  (clauses :vector<datascript.parser/clause>)
  (used-args :rule-call-history)
  (pending-guards :vector<datascript.parser/clause>))

(type-alias tuple-getter
  :fn<array<datascript.lg.query-types/result>;datascript.lg.query-types/result>)

(type-variant tuple-key
  (SingleTupleKey :datascript.lg.query-types/result)
  (CompositeTupleKey :vector<datascript.lg.query-types/result>))

(type-alias tuple-key-getter
  :fn<array<datascript.lg.query-types/result>;tuple-key>)

(type-alias tuple-call
  :fn<array<datascript.lg.query-types/result>;option<Datascript_runtime.Data_value.t>>)

(type-alias query-cache
  :datascript.lru/cache-state<Datascript_runtime.Data_value.t;datascript.parser/Query>)

(signature datascript.lg.query/*lookup-attrs*
  :set<string>)
(def ^:dynamic *lookup-attrs*
  (set-of :string))

(signature datascript.lg.query/*implicit-source*
  :option<datascript.db/database-view>)
(def ^:dynamic *implicit-source*
  None)

(signature datascript.lg.query/*implicit-source-name*
  :string)
(def ^:dynamic *implicit-source-name*
  "$")

(signature datascript.lg.query/*query-cache*
  :query-cache)
(def ^:dynamic ^query-cache *query-cache*
  (lru/cache 100))

(type-record aggregate-context-state
  (seen :map<string;bool>)
  (attrs :map<string;int>)
  (values :vector<datascript.lg.query-types/result>))

(defn- ^:string attrs-string [^:map<string;int> attrs]
  (str
   "{"
   (string/join
    ", "
    (mapv
     (fn [^:string variable]
       (str
        "\""
        variable
        "\" "
        (get attrs variable 0)))
     (keys attrs)))
   "}"))

(defn ^:set<string> intersect-keys
  [^:map<string;int> left ^:map<string;int> right]
  (reduce
   (fn [^:set<string> shared ^:string key]
     (if (contains? right key)
       (conj shared key)
       shared))
   #{}
   (keys left)))

(defn same-keys?
  [^:map<string;int> left ^:map<string;int> right]
  (and
   (= (count left) (count right))
   (every?
    (fn [^:string key] (contains? right key))
    (keys left))
   (every?
    (fn [^:string key] (contains? left key))
   (keys right))))

(defn ^tuple-getter getter-fn
  [^:map<string;int> attrs ^:string attr]
  (if-some [index (get attrs attr)]
    (fn [^:array<datascript.lg.query-types/result> row]
      (let [result (query-types/require-row-result row index)]
        (if (contains? *lookup-attrs* attr)
          (match *implicit-source*
            None result
            (Some database)
            (query-types/resolve-lookup-result database result))
          result)))
    (Stdlib.invalid_arg
     (str "Unknown relation attribute " attr))))

(defn ^tuple-key-getter tuple-key-fn
  [^:map<string;int> attrs ^:vector<string> common-attrs]
  (if (= 1 (count common-attrs))
    (let [getter (getter-fn attrs (nth common-attrs 0))]
      (fn [^:array<datascript.lg.query-types/result> row]
        (SingleTupleKey (getter row))))
    (let [getters (mapv
                   (fn [^:string attr]
                     (getter-fn attrs attr))
                   common-attrs)]
      (fn [^:array<datascript.lg.query-types/result> row]
        (CompositeTupleKey
         (mapv
          (fn [^tuple-getter getter]
            (getter row))
          getters))))))

(defn ^:set<string> bound-vars
  [^datascript.lg.query-types/context context]
  (reduce
   (fn [^:set<string> bound relation]
     (reduce
      (fn [^:set<string> bound ^:string variable]
        (conj bound variable))
      bound
      (keys (query-types/relation-attrs relation))))
   #{}
   (query-types/context-relations context)))

(defn- ^aggregate-context-state add-aggregate-context-value
  [^aggregate-context-state state
   ^:string variable
   ^:option<datascript.lg.query-types/result> value]
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

(defn- ^datascript.lg.query-types/relation aggregate-context-relation
  [^datascript.lg.query-types/context context]
  (let [state
        (reduce
         (fn [^aggregate-context-state state relation]
           (let [first-row
                 (first (query-types/relation-rows relation))]
             (reduce-kv
              (fn [^aggregate-context-state state
                   ^:string variable
                   ^:int index]
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

(defn ^:array<datascript.lg.query-types/result> -aggregate
  [^:vector<datascript.parser/find-element> find-elements
   ^datascript.lg.query-types/context context
   ^:vector<array<datascript.lg.query-types/result>> tuples]
  (query-types/aggregate-group-row
   find-elements
   (query-types/identity-relation)
   (aggregate-context-relation context)
   tuples))

(defn ^:vector<array<datascript.lg.query-types/result>> aggregate
  [^:vector<datascript.parser/find-element> find-elements
   ^datascript.lg.query-types/context context
   ^:vector<array<datascript.lg.query-types/result>> resultset]
  (query-types/aggregate-rows
   find-elements
   (query-types/identity-relation)
   (aggregate-context-relation context)
   resultset))

(defn- ^:string binding-source
  [^datascript.parser/binding binding]
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

(defn- ^:string input-binding-source
  [^datascript.parser/input-binding binding]
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

(defn- ^:string input-bindings-source
  [^:vector<datascript.parser/input-binding> bindings]
  (str
   "["
   (string/join " " (mapv input-binding-source bindings))
   "]"))

(defn ^datascript.lg.query-types/context resolve-in
  [^datascript.lg.query-types/context context
   ^:tuple<datascript.parser/input-binding;datascript.lg.query-types/input>
   binding-and-input]
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

(defn ^datascript.lg.query-types/context resolve-ins
  [^datascript.lg.query-types/context context
   ^:vector<datascript.parser/input-binding> bindings
   ^:vector<datascript.lg.query-types/input> inputs]
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

(defn ^:vector<datascript.parser/Rule> parse-rules
  [^:Datascript_runtime.Data_value.t rules]
  (match rules
    (Datascript_runtime.Data_value.String source)
    (datascript.parser/parse-rules
     (Datascript_runtime.Serialization_value.data_value_of_edn_string
      source))
    _ (datascript.parser/parse-rules rules)))

(defn- ^:option<Datascript_runtime.Data_value.t> rule-head
  [^:Datascript_runtime.Data_value.t clause]
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

(defn- ^boolean reserved-rule-name? [^:string rule-name]
  (or
   (= rule-name "_")
   (= rule-name "or")
   (= rule-name "or-join")
   (= rule-name "and")
   (= rule-name "not")
   (= rule-name "not-join")))

(defn rule?
  [^datascript.lg.query-types/context context
   ^:Datascript_runtime.Data_value.t clause]
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

(defn ^datascript.lg.query-types/relation empty-rel
  [^datascript.parser/binding binding]
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

(defn ^:option<datascript.lg.query-types/relation> limit-rel
  [^datascript.lg.query-types/relation relation
   ^:set<string> variables]
  (let [attrs
        (reduce-kv
         (fn [^:map<string;int> selected
              ^:string variable
              ^:int index]
           (if (contains? variables variable)
             (assoc selected variable index)
             selected))
         {}
         (query-types/relation-attrs relation))]
    (if (empty? attrs)
      None
      (let [lookup-databases
            (reduce
             (fn [^:map<string;datascript.db/database-view> selected
                  ^:string variable]
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

(defn ^datascript.lg.query-types/context limit-context
  [^datascript.lg.query-types/context context
   ^:set<string> variables]
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
  ([^datascript.lg.query-types/relation left
    ^datascript.lg.query-types/relation right]
   (query-types/product-relation left right)))

(defn ^datascript.lg.query-types/relation sum-rel
  [^datascript.lg.query-types/relation left
   ^datascript.lg.query-types/relation right]
  (let [left-attrs (query-types/relation-attrs left)
        right-attrs (query-types/relation-attrs right)
        left-rows (query-types/relation-rows left)
        right-rows (query-types/relation-rows right)]
    (cond
      (= left-attrs right-attrs)
      (query-types/relation
       left-attrs
       (vec (concat left-rows right-rows))
       (query-types/merge-lookup-databases
        (query-types/relation-lookup-databases left)
        (query-types/relation-lookup-databases right)))

      (empty? left-rows)
      right

      (empty? right-rows)
      left

      (not (same-keys? left-attrs right-attrs))
      (Stdlib.invalid_arg
       (str
        "Can’t sum relations with different attrs: "
        (attrs-string left-attrs)
        " and "
        (attrs-string right-attrs)))

      :else
      (let [^:array<int> indexes
            (Array.make (count left-attrs) 0)
            _indexed
            (reduce-kv
             (fn [_ignored ^:string variable ^:int left-index]
               (aset
                indexes
                left-index
                (get right-attrs variable 0))
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
          (query-types/relation-lookup-databases right)))))))

(defn ^datascript.lg.query-types/relation hash-join
  [^datascript.lg.query-types/relation left
   ^datascript.lg.query-types/relation right]
  (query-types/hash-join left right))

(defn ^datascript.lg.query-types/relation subtract-rel
  [^datascript.lg.query-types/relation left
   ^datascript.lg.query-types/relation right]
  (Datascript_runtime.Query_value.subtract_relation
   left right))

(defn ^:array<datascript.lg.query-types/result> join-tuples
  [^:array<datascript.lg.query-types/result> left
   ^:array<int> left-indexes
   ^:array<datascript.lg.query-types/result> right
   ^:array<int> right-indexes]
  (query-types/join-rows
   left left-indexes right right-indexes))

(defn ^:vector<datascript.lg.query-types/relation> collapse-rels
  [^:vector<datascript.lg.query-types/relation> relations
   ^datascript.lg.query-types/relation new-relation]
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
  [^:Datascript_runtime.Data_value.t form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (= value "")
      false
      (= (String.get value 0) \$))
    _ false))

(defn free-var?
  [^:Datascript_runtime.Data_value.t form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (= value "")
      false
      (= (String.get value 0) \?))
    _ false))

(defn attr?
  [^:Datascript_runtime.Data_value.t form]
  (match form
    (Datascript_runtime.Data_value.Keyword _) true
    (Datascript_runtime.Data_value.String _) true
    _ false))

(defn lookup-ref?
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items
            (Datascript_runtime.Data_value.sequential_items form)]
    (if (= 2 (count items))
      (if-some [attribute (first items)]
        (attr? attribute)
        false)
      false)
    false))

(defn matches-pattern?
  [^:vector<Datascript_runtime.Data_value.t> pattern
   ^:vector<Datascript_runtime.Data_value.t> tuple]
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

(defn- ^:option<datascript.lg.query-types/relation>
  relation-with-attr
  [^datascript.lg.query-types/context context
   ^:string variable]
  (loop [relations (query-types/context-relations context)]
    (if-some [relation (first relations)]
      (if
       (contains?
        (query-types/relation-attrs relation)
        variable)
        (Some relation)
        (recur (subvec relations 1)))
      None)))

(defn- ^:option<context-resolution> resolve-context-variable
  [^datascript.lg.query-types/context context
   ^:string variable]
  (if-some [relation (relation-with-attr context variable)]
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
    (resolve-context-variable
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

(defn- ^collect-row empty-collect-row
  [^:int length]
  (Array.make length None))

(defn- ^:array<option<int>> collect-copy-map
  [^:map<string;int> attrs
   ^:vector<string> symbols]
  (to-array
   (mapv
    (fn [^:string symbol]
      (get attrs symbol))
    symbols)))

(defn- relation-has-collect-symbol?
  [^datascript.lg.query-types/relation relation
   ^:vector<string> symbols]
  (let [attrs (query-types/relation-attrs relation)]
    (some
     (fn [^:string symbol]
       (contains? attrs symbol))
     symbols)))

(defn ^:vector<collect-row> -collect-tuples
  [^:vector<collect-row> acc
   ^datascript.lg.query-types/relation relation
   ^:int length
   ^:array<option<int>> copy-map]
  (Datascript_runtime.Query_value.collect_tuples
   acc relation length copy-map))

(defn -collect
  ([^datascript.lg.query-types/context context
    ^:vector<string> symbols]
   (-collect
    [(empty-collect-row (count symbols))]
    (query-types/context-relations context)
    symbols))
  ([^:vector<collect-row> acc
    ^:vector<datascript.lg.query-types/relation> relations
    ^:vector<string> symbols]
   (if-some [relation (first relations)]
     (if (empty? (query-types/relation-rows relation))
       []
       (if (relation-has-collect-symbol? relation symbols)
         (-collect
          (-collect-tuples
           acc
           relation
           (count symbols)
           (collect-copy-map
            (query-types/relation-attrs relation)
            symbols))
          (subvec relations 1)
          symbols)
         (-collect acc (subvec relations 1) symbols)))
     acc)))

(defn ^:vector<collect-row> collect
  [^datascript.lg.query-types/context context
   ^:vector<string> symbols]
  (Datascript_runtime.Query_value.distinct_optional_rows
   (-collect context symbols)))

(defn ^:option<Datascript_runtime.Data_value.t>
  substitute-constant
  [^datascript.lg.query-types/context context
   ^:Datascript_runtime.Data_value.t pattern-element]
  (if (free-var? pattern-element)
    (let [variable
          (Datascript_runtime.Data_value.to_edn_string
           pattern-element)]
      (if-some [relation
                (relation-with-attr context variable)]
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

(defn ^:vector<Datascript_runtime.Data_value.t>
  substitute-constants
  [^datascript.lg.query-types/context context
   ^:vector<Datascript_runtime.Data_value.t> pattern]
  (mapv
   (fn [^:Datascript_runtime.Data_value.t pattern-element]
     (match (substitute-constant context pattern-element)
       None pattern-element
       (Some value) value))
   pattern))

(defn- ^:option<Datascript_runtime.Data_value.t>
  pattern-value-at
  [^:vector<Datascript_runtime.Data_value.t> pattern
   ^:int index]
  (if (< index (count pattern))
    (Some (nth pattern index))
    None))

(defn- ^:set<string> add-free-pattern-variable
  [^:set<string> variables
   ^:option<Datascript_runtime.Data_value.t> pattern-value]
  (match pattern-value
    None variables
    (Some value)
    (if (free-var? value)
      (conj
       variables
       (Datascript_runtime.Data_value.to_edn_string value))
      variables)))

(defn ^:set<string> dynamic-lookup-attrs
  [^datascript.db/database-view database
   ^:vector<Datascript_runtime.Data_value.t> pattern]
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

(defn- ^:Datascript_runtime.Data_value.t
  strict-pattern-entid
  [^datascript.db/database-view database
   ^:Datascript_runtime.Data_value.t value]
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

(defn- ^:Datascript_runtime.Data_value.t
  pattern-value-or-nil
  [^:option<Datascript_runtime.Data_value.t> value]
  (match value
    None (Datascript_runtime.Data_value.Nil)
    (Some value) value))

(defn ^:vector<Datascript_runtime.Data_value.t>
  resolve-pattern-lookup-refs
  [^datascript.lg.query-types/source source
   ^:vector<Datascript_runtime.Data_value.t> pattern]
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

(defn- ^:vector<datascript.parser/pattern-element>
  parse-lookup-pattern
  [^:vector<Datascript_runtime.Data_value.t> pattern]
  (if-some [parsed
            (datascript.parser/parse-pattern-elements pattern)]
    parsed
    (Stdlib.invalid_arg "Cannot parse DataScript lookup pattern")))

(defn ^datascript.lg.query-types/relation lookup-pattern-db
  [^datascript.lg.query-types/context context
   ^datascript.db/database-view database
   ^:vector<Datascript_runtime.Data_value.t> pattern]
  (query-types/lookup-db-pattern
   database
   (parse-lookup-pattern
    (resolve-pattern-lookup-refs
     (query-types/database-source database)
     (substitute-constants context pattern)))))

(defn ^datascript.lg.query-types/relation lookup-pattern-coll
  [^datascript.lg.query-types/context context
   ^:vector<array<datascript.lg.query-types/result>> rows
   ^:vector<Datascript_runtime.Data_value.t> pattern]
  (query-types/resolve-relation-pattern
   rows
   (query-types/identity-relation)
   (parse-lookup-pattern
    (substitute-constants context pattern))))

(defn ^datascript.lg.query-types/relation lookup-pattern
  [^datascript.lg.query-types/context context
   ^datascript.lg.query-types/source source
   ^:vector<Datascript_runtime.Data_value.t> pattern]
  (match source
    (Datascript_runtime.Query_value.Database_source database)
    (lookup-pattern-db context database pattern)
    (Datascript_runtime.Query_value.Relation_source rows)
    (lookup-pattern-coll context rows pattern)))

(defn- ^:vector<map<string;datascript.lg.query-types/result>>
  map-return-rows
  [^:vector<string> keys
   ^:vector<array<datascript.lg.query-types/result>> tuples]
  (mapv
   (fn [^:array<datascript.lg.query-types/result> tuple]
     (query-types/return-map-row keys tuple))
   tuples))

(defn ^datascript.lg.query-types/output tuples->return-map
  [^datascript.parser/return-map return-map
   ^:vector<array<datascript.lg.query-types/result>> tuples]
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

(defn- ^:vector<array<datascript.lg.query-types/result>>
  first-tuple-only
  [^:vector<array<datascript.lg.query-types/result>> tuples]
  (if-some [tuple (first tuples)]
    [tuple]
    []))

(extend-type datascript.parser/FindTuple
  IPostProcess
  (-post-process [find return-map tuples]
    (query-types/find-output
     (datascript.parser/tuple-find-elements
      (.-elements find))
     return-map
     (first-tuple-only tuples))))

(defn ^:vector<Datascript_runtime.Data_value.t>
  normalize-pattern-clause
  [^:vector<Datascript_runtime.Data_value.t> clause]
  (if-some [head (first clause)]
    (if (source? head)
      clause
      (vec
       (concat
        [(Datascript_runtime.Data_value.Symbol "$")]
        clause)))
    [(Datascript_runtime.Data_value.Symbol "$")]))

(defn- pattern-elements-equal?
  [^datascript.parser/pattern-element left
   ^datascript.parser/pattern-element right]
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

(defn- ^:tuple<rule-arguments;rule-arguments>
  remove-rule-argument-pairs
  [^rule-arguments left
   ^rule-arguments right]
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

(defn ^:tuple<vector<Datascript_runtime.Data_value.t>;vector<Datascript_runtime.Data_value.t>>
  remove-pairs
  [^:vector<Datascript_runtime.Data_value.t> left
   ^:vector<Datascript_runtime.Data_value.t> right]
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

(defn- ^:Datascript_runtime.Data_value.t
  query-form-list
  [^:vector<Datascript_runtime.Data_value.t> values]
  (Datascript_runtime.Data_value.List
   (into (list) (reverse values))))

(defn- ^datascript.parser/fn-arg
  rule-argument-as-fn-arg
  [^datascript.parser/pattern-element argument]
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

(defn- ^:vector<datascript.parser/clause>
  typed-rule-gen-guards
  [^:string rule-name
   ^rule-arguments call-args
   ^rule-call-history used-args]
  (if-some [previous-calls (get used-args rule-name)]
    (mapv
     (fn [^rule-arguments previous-args]
       (let [remaining
             (remove-rule-argument-pairs
              call-args previous-args)]
         (datascript.parser/static-predicate-clause
          "-differ?"
          (mapv
           rule-argument-as-fn-arg
           (vec
            (concat
             (tuple-get remaining 0)
             (tuple-get remaining 1)))))))
     previous-calls)
    []))

(defn ^:vector<Datascript_runtime.Data_value.t>
  rule-gen-guards
  [^:Datascript_runtime.Data_value.t rule-clause
   ^:map<string;vector<vector<Datascript_runtime.Data_value.t>>>
   used-args]
  (if-some
   [items
    (Datascript_runtime.Data_value.sequential_items rule-clause)]
    (if-some [rule-form (first items)]
      (match rule-form
        (Datascript_runtime.Data_value.Symbol rule-name)
        (if-some [previous-calls (get used-args rule-name)]
          (let [call-args (subvec items 1)]
            (mapv
             (fn [^:vector<Datascript_runtime.Data_value.t>
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

(defn- ^:map<string;datascript.parser/pattern-element>
  rule-argument-replacements
  [^:vector<string> parameters
   ^:vector<datascript.parser/pattern-element> arguments]
  (let [limit (min (count parameters) (count arguments))]
    (reduce
     (fn [^:map<string;datascript.parser/pattern-element> replacements
          ^:int index]
       (assoc
        replacements
        (nth parameters index)
        (nth arguments index)))
     {}
     (range limit))))

(defn- ^:vector<vector<datascript.parser/clause>>
  expand-rule-branches
  [^datascript.parser/clause clause
   ^datascript.lg.query-types/context context]
  (if-some [parts (datascript.parser/rule-clause-parts clause)]
    (let [rule-name (tuple-get parts 0)
          arguments (tuple-get parts 1)
          seqid (swap! rule-seqid inc)]
      (if-some [branches
                (datascript.parser/rule-branches
                 (query-types/context-rules context)
                 rule-name)]
        (mapv
         (fn [^datascript.parser/RuleBranch branch]
           (datascript.parser/substitute-rule-clauses
            (rule-argument-replacements
             (datascript.parser/rule-branch-parameter-names
              branch)
             arguments)
            seqid
            (datascript.parser/rule-branch-clauses branch)))
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

(defn ^:vector<vector<datascript.parser/clause>> expand-rule
  [^datascript.parser/clause clause
   ^datascript.lg.query-types/context context
   ^:map<string;vector<vector<Datascript_runtime.Data_value.t>>>
   _used-args]
  (expand-rule-branches clause context))

(signature datascript.lg.query/walk-collect
  :fn<Datascript_runtime.Data_value.t;fn<Datascript_runtime.Data_value.t;bool>;vector<Datascript_runtime.Data_value.t>>)
(declare walk-collect)

(defn ^:vector<Datascript_runtime.Data_value.t> walk-collect
  [^:Datascript_runtime.Data_value.t form
   ^:fn<Datascript_runtime.Data_value.t;bool> pred]
  (let [children
        (if-some
         [values (Datascript_runtime.Data_value.sequential_items form)]
          (reduce
           (fn [^:vector<Datascript_runtime.Data_value.t> collected
                ^:Datascript_runtime.Data_value.t value]
             (let [^:vector<Datascript_runtime.Data_value.t> nested
                   (walk-collect value pred)]
               (vec (concat collected nested))))
           []
           values)
          (if-some
           [entries (Datascript_runtime.Data_value.map_entries form)]
            (reduce
             (fn [^:vector<Datascript_runtime.Data_value.t> collected
                  ^:tuple<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t> entry]
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
               (fn [^:vector<Datascript_runtime.Data_value.t> collected
                    ^:Datascript_runtime.Data_value.t value]
                 (let [^:vector<Datascript_runtime.Data_value.t> nested
                       (walk-collect value pred)]
                   (vec (concat collected nested))))
               []
               values)
              (if-some
               [values (Datascript_runtime.Data_value.tuple_items form)]
                (reduce
                 (fn [^:vector<Datascript_runtime.Data_value.t> collected
                      ^:option<Datascript_runtime.Data_value.t> value]
                   (match value
                     None collected
                     (Some value)
                     (let [^:vector<Datascript_runtime.Data_value.t> nested
                           (walk-collect value pred)]
                       (vec (concat collected nested)))))
                 []
                 values)
                []))))]
    (if (pred form)
      (conj children form)
      children)))

(defn ^:set<string> collect-vars
  [^:Datascript_runtime.Data_value.t form]
  (reduce
   (fn [^:set<string> variables
        ^:Datascript_runtime.Data_value.t value]
     (conj
      variables
      (Datascript_runtime.Data_value.to_edn_string value)))
   #{}
   (walk-collect form (fn [value] (free-var? value)))))

(defn- ^:vector<Datascript_runtime.Data_value.t>
  guard-arguments
  [^:Datascript_runtime.Data_value.t guard]
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

(defn ^:tuple<vector<Datascript_runtime.Data_value.t>;vector<Datascript_runtime.Data_value.t>>
  split-guards
  [^:Datascript_runtime.Data_value.t clauses
   ^:vector<Datascript_runtime.Data_value.t> guards]
  (let [bound (collect-vars clauses)]
    (reduce
     (fn [^:tuple<vector<Datascript_runtime.Data_value.t>;vector<Datascript_runtime.Data_value.t>>
          split
          ^:Datascript_runtime.Data_value.t guard]
       (if
        (every?
         (fn [^:Datascript_runtime.Data_value.t variable]
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

(defn- ^:set<string> clause-variable-names
  [^:vector<datascript.parser/clause> clauses]
  (reduce
   (fn [^:set<string> names ^datascript.parser/Variable variable]
     (conj names (str (.-symbol variable))))
   #{}
   (datascript.parser/collect-vars-distinct clauses)))

(defn- ^:tuple<vector<datascript.parser/clause>;vector<datascript.parser/clause>>
  split-typed-guards
  [^:vector<datascript.parser/clause> clauses
   ^:vector<datascript.parser/clause> guards]
  (let [bound (clause-variable-names clauses)]
    (reduce
     (fn [^:tuple<vector<datascript.parser/clause>;vector<datascript.parser/clause>>
          split
          ^datascript.parser/clause guard]
       (if
        (every?
         (fn [^datascript.parser/Variable variable]
           (contains? bound (str (.-symbol variable))))
         (datascript.parser/clause-vars guard))
         (tuple
          (conj (tuple-get split 0) guard)
          (tuple-get split 1))
         (tuple
          (tuple-get split 0)
          (conj (tuple-get split 1) guard))))
     (tuple [] [])
     guards)))

(defn- ^:set<string> missing-vars
  [^:set<string> bound ^:vector<string> variables]
  (reduce
   (fn [^:set<string> missing ^:string variable]
     (if (contains? bound variable)
       missing
       (conj missing variable)))
   #{}
   variables))

(defn- ^:string variable-set-string
  [^:set<string> variables]
  (str "#{" (string/join " " (vec variables)) "}"))

(defn- ^:string variable-sets-string
  [^:vector<set<string>> variable-sets]
  (str
   "["
   (string/join
    " "
    (mapv
     (fn [^:set<string> variables]
       (variable-set-string variables))
     variable-sets))
   "]"))

(defn check-bound
  [^:set<string> bound
   ^:vector<string> variables
   ^:Datascript_runtime.Data_value.t form]
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
  [^:set<string> bound
   ^:vector<Datascript_runtime.Data_value.t> branches
   ^:Datascript_runtime.Data_value.t form]
  (let [free
        (mapv
         (fn [^:Datascript_runtime.Data_value.t branch]
           (missing-vars bound (vec (collect-vars branch))))
         branches)
        same?
        (if-some [expected (first free)]
          (every?
           (fn [^:set<string> variables]
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
  [^:set<string> bound
   ^:vector<string> variables
   ^:vector<Datascript_runtime.Data_value.t> branches]
  (let [free (missing-vars bound variables)]
    (reduce
     (fn [^:unit _ignored
          ^:Datascript_runtime.Data_value.t branch]
       (let [present (collect-vars branch)
             missing
             (reduce
              (fn [^:set<string> missing ^:string variable]
                (if (contains? present variable)
                  missing
                  (conj missing variable)))
              #{}
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

(defn- ^datascript.db/database-view context-database
  [^datascript.lg.query-types/context context]
  (match *implicit-source*
    (Some database) database
    None
    (query-types/query-default-database
     (query-types/context-sources context))))

(defn- ^:option<datascript.db/database-view>
  context-source-database-option
  [^datascript.lg.query-types/context context
   ^:string source-name]
  (if-some [source
            (get
             (query-types/context-sources context)
             source-name)]
    (query-types/source-database source)
    None))

(defn- ^datascript.lg.query-types/relation join-context-relations
  [^datascript.lg.query-types/context context]
  (reduce
   (fn [^datascript.lg.query-types/relation relation
        ^datascript.lg.query-types/relation next-relation]
     (query-types/hash-join relation next-relation))
   (query-types/identity-relation)
   (query-types/context-relations context)))

(defn- ^:tuple<vector<datascript.parser/clause>;vector<datascript.parser/clause>>
  split-leading-non-rules
  [^:vector<datascript.parser/clause> clauses]
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

(defn- ^boolean rule-guard-always-false?
  [^datascript.parser/clause guard]
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
  [^datascript.lg.query-types/context context]
  (some
   (fn [^datascript.lg.query-types/relation relation]
     (empty? (query-types/relation-rows relation)))
   (query-types/context-relations context)))

(defn- ^:vector<string> rule-output-variables
  [^rule-arguments arguments]
  (reduce
   (fn [^:vector<string> variables
        ^datascript.parser/pattern-element argument]
     (if-some [variable
               (query-types/pattern-variable-name argument)]
       (if
        (some
         (fn [^:string existing] (= existing variable))
         variables)
         variables
         (conj variables variable))
       variables))
   []
   arguments))

(defn- ^datascript.lg.query-types/relation
  rule-result-relation
  [^datascript.lg.query-types/context context
   ^:vector<string> variables]
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

(defn- ^rule-frame make-rule-frame
  [^:vector<datascript.parser/clause> prefix-clauses
   ^datascript.lg.query-types/context prefix-context
   ^:vector<datascript.parser/clause> clauses
   ^rule-call-history used-args
   ^:vector<datascript.parser/clause> pending-guards]
  (record rule-frame
    (prefix-clauses prefix-clauses)
    (prefix-context prefix-context)
    (clauses clauses)
    (used-args used-args)
    (pending-guards pending-guards)))

(defn ^datascript.lg.query-types/context -resolve-clause
  ([^datascript.lg.query-types/context context
    ^datascript.parser/clause clause]
   (-resolve-clause context clause clause))
  ([^datascript.lg.query-types/context context
    ^datascript.parser/clause clause
    ^datascript.parser/clause _orig-clause]
   (let [^datascript.lg.query-types/rule-path rule-path []
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

(defn- ^datascript.lg.query-types/context solve-rule-prefix
  [^datascript.lg.query-types/context context
   ^:vector<datascript.parser/clause> clauses]
  (reduce
   (fn [^datascript.lg.query-types/context current
        ^datascript.parser/clause clause]
     (-resolve-clause current clause))
   context
   clauses))

(defn ^tuple-call -call-fn
  [^datascript.lg.query-types/context context
   ^datascript.lg.query-types/relation relation
   ^datascript.lg.query-types/callable callable
   ^:vector<datascript.parser/fn-arg> arguments]
  (let [database (context-database context)
        sources (query-types/context-sources context)
        constants (query-types/identity-relation)]
    (fn [^:array<datascript.lg.query-types/result> row]
      (query-types/invoke-callable
       callable
       (query-types/callable-arguments
        database
        sources
        relation
        constants
        row
        arguments)))))

(defn ^datascript.lg.query-types/relation solve-rule
  [^datascript.lg.query-types/context context
   ^datascript.parser/clause clause]
  (if-some [rule-parts
            (datascript.parser/rule-clause-parts clause)]
    (let [arguments (tuple-get rule-parts 1)
          source-name
          (if-some [name
                    (datascript.parser/rule-clause-source-name clause)]
            name
            "$")
          variables (rule-output-variables arguments)]
      (binding
       [*implicit-source-name* source-name
        *implicit-source*
        (context-source-database-option
         context source-name)]
       (loop
        [stack
         [(make-rule-frame [] context [clause] {} [])]
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
                      guard-split
                      (split-typed-guards
                       (vec
                        (concat
                         (:prefix-clauses frame)
                         leading-clauses))
                       (vec
                        (concat
                         guards
                         (:pending-guards frame))))
                      active-guards (tuple-get guard-split 0)
                      pending-guards (tuple-get guard-split 1)]
                  (if
                   (some rule-guard-always-false? active-guards)
                    (recur (subvec stack 1) result)
                    (let [prefix-clauses
                          (vec
                           (concat
                            leading-clauses
                            active-guards))
                          prefix-context
                          (solve-rule-prefix
                           (:prefix-context frame)
                           prefix-clauses)]
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
                               (fn [^:vector<datascript.parser/clause>
                                    branch]
                                 (make-rule-frame
                                  prefix-clauses
                                  prefix-context
                                  (vec
                                   (concat
                                    branch
                                    next-clauses))
                                  used-args
                                  pending-guards))
                               branches)]
                          (recur
                           (vec
                            (concat
                             branch-frames
                             (subvec stack 1)))
                           result))))))
                (Stdlib.invalid_arg
                 "Expected a rule clause"))
              (let [resolved-context
                    (solve-rule-prefix
                     (:prefix-context frame)
                     leading-clauses)
                    branch-result
                    (rule-result-relation
                     resolved-context variables)]
                (recur
                 (subvec stack 1)
                 (sum-rel result branch-result)))))
          result))))
    (Stdlib.invalid_arg
     "solve-rule expects a rule clause")))

(defn ^datascript.lg.query-types/context resolve-clause
  [^datascript.lg.query-types/context context
   ^datascript.parser/clause clause]
  (if
   (some
    (fn [^datascript.lg.query-types/relation relation]
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

(defn ^datascript.lg.query-types/context filter-by-pred
  [^datascript.lg.query-types/context context
   ^datascript.parser/clause clause]
  (match clause
    (PredicateClause _callable _arguments)
    (-resolve-clause context clause)
    _
    (Stdlib.invalid_arg
     "filter-by-pred expects a predicate clause")))

(defn ^datascript.lg.query-types/context bind-by-fn
  [^datascript.lg.query-types/context context
   ^datascript.parser/clause clause]
  (match clause
    (FunctionClause _callable _arguments _binding)
    (-resolve-clause context clause)
    _
    (Stdlib.invalid_arg
     "bind-by-fn expects a function clause")))

(defn ^datascript.lg.query-types/context -q
  [^datascript.lg.query-types/context context
   ^:vector<datascript.parser/clause> clauses]
  (binding [*implicit-source-name* "$"
            *implicit-source*
            (context-source-database-option
             context "$")]
    (reduce
     (fn [^datascript.lg.query-types/context context
          ^datascript.parser/clause clause]
       (resolve-clause context clause))
     context
     clauses)))

(defn ^datascript.lg.query-types/output q-closed
  [^datascript.parser/Query query
   ^:vector<datascript.lg.query-types/input> inputs]
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
     (query-types/database-source
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
             (if (string? value)
               (list
                'Datascript_runtime.Data_value.String
                value)
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
                        value))))))))
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
               (if (if (seq? head)
                     (let [callable-name (first head)]
                       (if (symbol? callable-name)
                         (= (subs (str callable-name) 0 1) "?")
                         false))
                     false)
                 (conj variables (str (first head)))
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
                 'datascript.lg.query-types/database-source
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
