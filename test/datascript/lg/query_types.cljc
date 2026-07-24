(ns datascript.lg.query-types
  (:require
   [datascript.db]
   [datascript.parser :as parser]))

(type-alias result
  :Datascript_runtime.Query_value.result<datascript.db/DB>)
(type-alias source
  :Datascript_runtime.Query_value.source<datascript.db/DB>)
(type-alias binding-value
  :Datascript_runtime.Query_value.binding_value<datascript.db/DB>)
(type-alias relation
  :Datascript_runtime.Query_value.relation<datascript.db/DB>)
(type-alias rules
  :map<string;vector<datascript.parser/clause>>)
(type-alias input
  :Datascript_runtime.Query_value.input<datascript.db/DB;rules>)
(type-alias output
  :Datascript_runtime.Query_value.output<datascript.db/DB>)
(type-alias context
  :Datascript_runtime.Query_value.context<datascript.db/DB;rules>)

(type-variant predicate-operand
  (PredicateColumn :int)
  (PredicateValue :Datascript_runtime.Data_value.t))

(signature datascript.lg.query-types/entity-result
  :fn<int;result>)
(signature datascript.lg.query-types/attr-result
  :fn<keyword;result>)
(signature datascript.lg.query-types/value-result
  :fn<Datascript_runtime.Data_value.t;result>)
(signature datascript.lg.query-types/result-value
  :fn<result;option<Datascript_runtime.Data_value.t>>)
(signature datascript.lg.query-types/database-result
  :fn<datascript.db/DB;result>)
(signature datascript.lg.query-types/pull-result
  :fn<Datascript_runtime.Data_value.t;result>)
(signature datascript.lg.query-types/added-result
  :fn<bool;result>)
(signature datascript.lg.query-types/database-source
  :fn<datascript.db/DB;source>)
(signature datascript.lg.query-types/relation-source
  :fn<vector<array<result>>;source>)
(signature datascript.lg.query-types/source-database
  :fn<source;option<datascript.db/DB>>)
(signature datascript.lg.query-types/source-rows
  :fn<source;option<vector<array<result>>>>)
(signature datascript.lg.query-types/scalar-binding
  :fn<result;binding-value>)
(signature datascript.lg.query-types/collection-binding
  :fn<vector<binding-value>;binding-value>)
(signature datascript.lg.query-types/binding-result
  :fn<binding-value;option<result>>)
(signature datascript.lg.query-types/binding-items
  :fn<binding-value;option<vector<binding-value>>>)
(signature datascript.lg.query-types/source-input
  :fn<source;input>)
(signature datascript.lg.query-types/rules-input
  :fn<rules;input>)
(signature datascript.lg.query-types/binding-input
  :fn<binding-value;input>)
(signature datascript.lg.query-types/input-source
  :fn<input;option<source>>)
(signature datascript.lg.query-types/input-rules
  :fn<input;option<rules>>)
(signature datascript.lg.query-types/input-binding
  :fn<input;option<binding-value>>)
(signature datascript.lg.query-types/output-relation
  :fn<output;option<vector<array<result>>>>)
(signature datascript.lg.query-types/output-collection
  :fn<output;option<vector<result>>>)
(signature datascript.lg.query-types/output-scalar
  :fn<output;option<option<result>>>)
(signature datascript.lg.query-types/output-tuple
  :fn<output;option<option<array<result>>>>)
(signature datascript.lg.query-types/row-get
  :fn<array<result>;int;option<result>>)
(signature datascript.lg.query-types/empty-row
  :fn<unit;array<result>>)
(signature datascript.lg.query-types/project-row
  :fn<array<result>;array<int>;array<result>>)
(signature datascript.lg.query-types/join-rows
  :fn<array<result>;array<int>;array<result>;array<int>;array<result>>)
(signature datascript.lg.query-types/concat-rows
  :fn<array<result>;array<result>;array<result>>)
(signature datascript.lg.query-types/datom-row
  :fn<datascript.db/Datom;array<result>>)
(signature datascript.lg.query-types/relation
  :fn<map<string;int>;vector<array<result>>;map<string;datascript.db/DB>;relation>)
(signature datascript.lg.query-types/datom-relation
  :fn<map<string;int>;array<int>;vector<datascript.db/Datom>;map<string;datascript.db/DB>;relation>)
(signature datascript.lg.query-types/index-attrs
  :fn<vector<string>;map<string;int>>)
(signature datascript.lg.query-types/pattern-relation
  :fn<vector<string>;array<int>;vector<datascript.db/Datom>;map<string;datascript.db/DB>;relation>)
(signature datascript.lg.query-types/lookup-db-pattern
  :fn<datascript.db/DB;vector<datascript.parser/pattern-element>;relation>)
(signature datascript.lg.query-types/lookup-db-patterns
  :fn<datascript.db/DB;vector<vector<datascript.parser/pattern-element>>;relation>)
(signature datascript.lg.query-types/execute-db-query
  :fn<datascript.db/DB;datascript.parser/Query;output>)
(signature datascript.lg.query-types/execute-query
  :fn<datascript.parser/Query;vector<input>;output>)
(signature datascript.lg.query-types/empty-relation
  :fn<map<string;int>;map<string;datascript.db/DB>;relation>)
(signature datascript.lg.query-types/relation-result
  :fn<relation;string;array<result>;option<result>>)
(signature datascript.lg.query-types/relation-attrs
  :fn<relation;map<string;int>>)
(signature datascript.lg.query-types/relation-rows
  :fn<relation;vector<array<result>>>)
(signature datascript.lg.query-types/relation-lookup-databases
  :fn<relation;map<string;datascript.db/DB>>)
(signature datascript.lg.query-types/relation-with-rows
  :fn<relation;vector<array<result>>;relation>)
(signature datascript.lg.query-types/sum-relation
  :fn<relation;relation;relation>)
(signature datascript.lg.query-types/product-relation
  :fn<relation;relation;relation>)
(signature datascript.lg.query-types/resolve-lookup-result
  :fn<datascript.db/DB;result;result>)
(signature datascript.lg.query-types/hash-join
  :fn<relation;relation;relation>)
(signature datascript.lg.query-types/product-attrs
  :fn<map<string;int>;map<string;int>;map<string;int>>)
(signature datascript.lg.query-types/merge-lookup-databases
  :fn<map<string;datascript.db/DB>;map<string;datascript.db/DB>;map<string;datascript.db/DB>>)
(signature datascript.lg.query-types/relation-lookup-database
  :fn<relation;string;option<datascript.db/DB>>)
(signature datascript.lg.query-types/context
  :fn<vector<relation>;map<string;source>;rules;context>)
(signature datascript.lg.query-types/context-relations
  :fn<context;vector<relation>>)
(signature datascript.lg.query-types/context-sources
  :fn<context;map<string;source>>)
(signature datascript.lg.query-types/context-rules
  :fn<context;rules>)

(declare empty-relation binding-relation)

(defn ^:map<string;datascript.db/DB> empty-lookup-databases []
  {})
(defn ^result entity-result [^:int entity]
  (Datascript_runtime.Query_value.entity entity))

(defn ^result attr-result [^:keyword attr]
  (Datascript_runtime.Query_value.attr (str attr)))

(defn ^result value-result [^:Datascript_runtime.Data_value.t value]
  (Datascript_runtime.Query_value.value value))

(defn ^:option<Datascript_runtime.Data_value.t> result-value [^result result]
  (Datascript_runtime.Query_value.result_value result))

(defn ^result database-result [^datascript.db/DB database]
  (Datascript_runtime.Query_value.database database))

(defn ^result pull-result [^:Datascript_runtime.Data_value.t value]
  (Datascript_runtime.Query_value.pull value))

(defn ^result added-result [^:bool added]
  (Datascript_runtime.Query_value.added added))

(defn ^source database-source [^datascript.db/DB database]
  (Datascript_runtime.Query_value.database_source database))

(defn ^source relation-source [^:vector<array<result>> rows]
  (Datascript_runtime.Query_value.relation_source rows))

(defn ^:option<datascript.db/DB> source-database [^source source]
  (Datascript_runtime.Query_value.source_database source))

(defn ^:option<vector<array<result>>> source-rows [^source source]
  (Datascript_runtime.Query_value.source_rows source))

(defn ^binding-value scalar-binding [^result result]
  (Datascript_runtime.Query_value.scalar_binding result))

(defn ^binding-value collection-binding
  [^:vector<binding-value> values]
  (Datascript_runtime.Query_value.collection_binding values))

(defn ^:option<result> binding-result [^binding-value binding]
  (Datascript_runtime.Query_value.binding_result binding))

(defn ^:option<vector<binding-value>> binding-items
  [^binding-value binding]
  (Datascript_runtime.Query_value.binding_items binding))

(defn ^input source-input [^source source]
  (Datascript_runtime.Query_value.source_input source))

(defn ^input rules-input [^rules rules]
  (Datascript_runtime.Query_value.rules_input rules))

(defn ^input binding-input [^binding-value binding]
  (Datascript_runtime.Query_value.binding_input binding))

(defn ^:option<source> input-source [^input input]
  (Datascript_runtime.Query_value.input_source input))

(defn ^:option<rules> input-rules [^input input]
  (Datascript_runtime.Query_value.input_rules input))

(defn ^:option<binding-value> input-binding [^input input]
  (Datascript_runtime.Query_value.input_binding input))

(defn ^:option<vector<array<result>>> output-relation [^output output]
  (Datascript_runtime.Query_value.output_relation output))

(defn ^:option<vector<result>> output-collection [^output output]
  (Datascript_runtime.Query_value.output_collection output))

(defn ^:option<option<result>> output-scalar [^output output]
  (Datascript_runtime.Query_value.output_scalar output))

(defn ^:option<option<array<result>>> output-tuple [^output output]
  (Datascript_runtime.Query_value.output_tuple output))

(defn ^:option<result> row-get
  [^:array<result> row ^:int index]
  (Datascript_runtime.Query_value.row_get row index))

(defn ^:array<result> empty-row []
  (Datascript_runtime.Query_value.empty_row (Stdlib.ignore 0)))

(defn ^:array<result> project-row
  [^:array<result> row ^:array<int> indexes]
  (Datascript_runtime.Query_value.project_row row indexes))

(defn ^:array<result> join-rows
  [^:array<result> left
   ^:array<int> left-indexes
   ^:array<result> right
   ^:array<int> right-indexes]
  (Datascript_runtime.Query_value.join_rows
   left left-indexes right right-indexes))

(defn ^:array<result> concat-rows
  [^:array<result> left ^:array<result> right]
  (Datascript_runtime.Query_value.concat_rows left right))

(defn ^:array<result> datom-row [^datascript.db/Datom datom]
  (array
   (entity-result (.-e datom))
   (attr-result (.-a datom))
   (value-result (.-v datom))
   (entity-result (datascript.db/datom-tx datom))
   (added-result (datascript.db/datom-added datom))))

(defn ^result datom-result-at
  [^datascript.db/Datom datom ^:int index]
  (case index
    0 (entity-result (.-e datom))
    1 (attr-result (.-a datom))
    2 (value-result (.-v datom))
    3 (entity-result (datascript.db/datom-tx datom))
    4 (added-result (datascript.db/datom-added datom))
    (Stdlib.invalid_arg "Datom projection index is out of bounds")))

(defn ^:array<result> project-datom-row
  [^datascript.db/Datom datom ^:array<int> indexes]
  (case (alength indexes)
    0 (empty-row)
    1 (array
       (datom-result-at datom (aget indexes 0)))
    2 (array
       (datom-result-at datom (aget indexes 0))
       (datom-result-at datom (aget indexes 1)))
    3 (array
       (datom-result-at datom (aget indexes 0))
       (datom-result-at datom (aget indexes 1))
       (datom-result-at datom (aget indexes 2)))
    (Array.map
     (fn [^:int index]
       (datom-result-at datom index))
     indexes)))

(defn relation
  [^:map<string;int> attrs
   ^:vector<array<result>> rows
   ^:map<string;datascript.db/DB> lookup-databases]
  (Datascript_runtime.Query_value.relation attrs rows lookup-databases))

(defn ^relation datom-relation
  [^:map<string;int> attrs
   ^:array<int> indexes
   ^:vector<datascript.db/Datom> datoms
   ^:map<string;datascript.db/DB> lookup-databases]
  (relation
   attrs
   (mapv
    (fn [^datascript.db/Datom datom]
      (project-datom-row datom indexes))
   datoms)
   lookup-databases))

(defn ^:map<string;int> index-attrs [^:vector<string> variables]
  (Datascript_runtime.Query_value.index_attrs variables))

(defn ^relation pattern-relation
  [^:vector<string> variables
   ^:array<int> indexes
   ^:vector<datascript.db/Datom> datoms
   ^:map<string;datascript.db/DB> lookup-databases]
  (if (= (count variables) (alength indexes))
    (datom-relation
     (index-attrs variables)
     indexes
     datoms
     lookup-databases)
    (Stdlib.invalid_arg
     "Pattern variables and indexes must have the same length")))

(defn ^:option<option<int>> pattern-entity-constraint
  [^datascript.db/DB database
   ^:option<datascript.parser/pattern-element> element]
  (match element
    None (Some None)
    (Some PatternPlaceholder) (Some None)
    (Some (PatternVariable _)) (Some None)
    (Some (PatternConstant value))
    (if-some [entity-ref
              (Datascript_runtime.Data_value.entity_ref_value value)]
      (match (datascript.db/entid database entity-ref)
        None None
        (Some entity) (Some (Some entity)))
      None)))

(defn ^:option<option<keyword>> pattern-attr-constraint
  [^:option<datascript.parser/pattern-element> element]
  (match element
    None (Some None)
    (Some PatternPlaceholder) (Some None)
    (Some (PatternVariable _)) (Some None)
    (Some (PatternConstant value))
    (match (Datascript_runtime.Data_value.keyword_value value)
      None None
      (Some attr) (Some (Some attr)))))

(defn ^:option<option<Datascript_runtime.Data_value.t>>
  pattern-value-constraint
  [^:option<datascript.parser/pattern-element> element]
  (match element
    None (Some None)
    (Some PatternPlaceholder) (Some None)
    (Some (PatternVariable _)) (Some None)
    (Some (PatternConstant value))
    (Some (Some value))))

(defn ^:option<option<bool>> pattern-added-constraint
  [^:option<datascript.parser/pattern-element> element]
  (match element
    None (Some None)
    (Some PatternPlaceholder) (Some None)
    (Some (PatternVariable _)) (Some None)
    (Some (PatternConstant value))
    (match (Datascript_runtime.Data_value.keyword_value value)
      (Some ":db/add") (Some (Some true))
      (Some ":db/retract") (Some (Some false))
      _ None)))

(defn ^:option<string> pattern-variable-name
  [^datascript.parser/pattern-element element]
  (if-some [variable
            (parser/pattern-element-variable-symbol element)]
    (Some (str variable))
    None))

(defn ^:option<datascript.parser/pattern-element> pattern-element-at
  [^:vector<datascript.parser/pattern-element> pattern ^:int index]
  (if (< index (count pattern))
    (nth pattern index)
    None))

(defn ^:tuple<vector<string>;vector<int>> pattern-projection
  [^:vector<datascript.parser/pattern-element> pattern]
  (loop [remaining pattern
         index 0
         variables []
         indexes []]
    (if-some [element (first remaining)]
      (if-some [variable (pattern-variable-name element)]
        (recur
         (subvec remaining 1)
         (inc index)
         (conj variables variable)
         (conj indexes index))
        (recur
         (subvec remaining 1)
         (inc index)
         variables
         indexes))
      (tuple variables indexes))))

(defn ^:map<string;datascript.db/DB> pattern-lookup-databases
  [^datascript.db/DB database
   ^:vector<datascript.parser/pattern-element> pattern
   ^:option<keyword> attr]
  (let [databases
        (reduce
         (fn [^:map<string;datascript.db/DB> databases
              ^:int index]
           (if-some [element (pattern-element-at pattern index)]
             (if-some [variable (pattern-variable-name element)]
               (assoc databases variable database)
               databases)
             databases))
         {}
         [0 3])]
    (if-some [attr attr]
      (if (datascript.db/ref? database attr)
        (if-some [element (pattern-element-at pattern 2)]
          (if-some [variable (pattern-variable-name element)]
            (assoc databases variable database)
            databases)
          databases)
        databases)
      databases)))

(defn ^relation lookup-db-pattern
  [^datascript.db/DB database
   ^:vector<datascript.parser/pattern-element> pattern]
  (if (or (empty? pattern) (> (count pattern) 5))
    (Stdlib.invalid_arg "DataScript patterns must contain one to five elements")
    (match
     (tuple
      (pattern-entity-constraint
       database
       (pattern-element-at pattern 0))
      (pattern-attr-constraint
       (pattern-element-at pattern 1))
      (pattern-value-constraint
       (pattern-element-at pattern 2))
      (pattern-entity-constraint
       database
       (pattern-element-at pattern 3))
      (pattern-added-constraint
       (pattern-element-at pattern 4)))
      (tuple
       (Some entity)
       (Some attr)
       (Some value)
      (Some tx)
      (Some added))
      (let [datoms
            (datascript.db/search-vector
             database entity attr value tx)
            filtered
            (if-some [added added]
              (filterv
               (fn [^datascript.db/Datom datom]
                 (= added (datascript.db/datom-added datom)))
               datoms)
              datoms)
            projection (pattern-projection pattern)
            variables (tuple-get projection 0)
            indexes (tuple-get projection 1)
            lookup-databases
            (pattern-lookup-databases database pattern attr)]
        (pattern-relation
         variables
         (to-array indexes)
         filtered
         lookup-databases))
      _
      (empty-relation {} (empty-lookup-databases)))))

(defn ^relation empty-relation
  [^:map<string;int> attrs
   ^:map<string;datascript.db/DB> lookup-databases]
  (relation attrs [] lookup-databases))

(defn ^relation identity-relation []
  (relation
   {}
   [(empty-row)]
   (empty-lookup-databases)))

(defn ^:option<result> relation-result
  [^relation relation ^:string variable ^:array<result> row]
  (Datascript_runtime.Query_value.relation_result
   relation variable row))

(defn ^:map<string;int> relation-attrs [^relation relation]
  (Datascript_runtime.Query_value.relation_attrs relation))

(defn ^:vector<array<result>> relation-rows [^relation relation]
  (Datascript_runtime.Query_value.relation_rows relation))

(defn ^:map<string;datascript.db/DB> relation-lookup-databases
  [^relation relation]
  (Datascript_runtime.Query_value.relation_lookup_databases relation))

(defn ^relation relation-with-rows
  [^relation relation ^:vector<array<result>> rows]
  (Datascript_runtime.Query_value.relation_with_rows relation rows))

(defn ^relation sum-relation [^relation left ^relation right]
  (if (= (relation-attrs left) (relation-attrs right))
    (Datascript_runtime.Query_value.relation_append_rows left right)
    (Stdlib.invalid_arg "Cannot sum relations with different attrs")))

(defn ^:map<string;int> product-attrs
  [^:map<string;int> left ^:map<string;int> right]
  (reduce
   (fn [^:map<string;int> attrs ^:string variable]
     (if (contains? attrs variable)
       (Stdlib.invalid_arg "Cannot multiply relations with common attrs")
       (assoc attrs variable (count attrs))))
   left
   (keys right)))

(defn ^:map<string;datascript.db/DB> merge-lookup-databases
  [^:map<string;datascript.db/DB> left
   ^:map<string;datascript.db/DB> right]
  (reduce-kv
   (fn [^:map<string;datascript.db/DB> databases
        ^:string variable
        ^datascript.db/DB database]
     (assoc databases variable database))
   left
   right))

(defn ^relation product-relation [^relation left ^relation right]
  (relation
   (product-attrs (relation-attrs left) (relation-attrs right))
   (Datascript_runtime.Query_value.product_rows
    (relation-rows left)
    (relation-rows right))
   (merge-lookup-databases
    (relation-lookup-databases left)
    (relation-lookup-databases right))))

(defn ^result resolve-lookup-result
  [^datascript.db/DB database ^result result]
  (match result
    (Datascript_runtime.Query_value.Value value)
    (match
     (Datascript_runtime.Data_value.entity_ref_value value)
      (Some entity-ref)
      (match (datascript.db/entid database entity-ref)
        (Some entity) (entity-result entity)
        None result)
      None result)
    _ result))

(defn ^relation hash-join [^relation left ^relation right]
  (Datascript_runtime.Query_value.hash_join
   resolve-lookup-result left right))

(defn ^:Datascript_runtime.Data_value.t result-pattern-value
  [^result result]
  (match result
    (Datascript_runtime.Query_value.Entity entity)
    (Datascript_runtime.Data_value.Ref_to
     (Datascript_runtime.Data_value.Entity_id entity))
    (Datascript_runtime.Query_value.Attr attr)
    (Datascript_runtime.Data_value.Keyword attr)
    (Datascript_runtime.Query_value.Value value) value
    (Datascript_runtime.Query_value.Pull value) value
    (Datascript_runtime.Query_value.Added added)
    (Datascript_runtime.Data_value.Keyword
     (if added ":db/add" ":db/retract"))
    (Datascript_runtime.Query_value.Database _)
    (Stdlib.invalid_arg
     "A database query result cannot bind a pattern component")))

(defn ^datascript.parser/pattern-element bind-pattern-element
  [^relation relation
   ^:array<result> row
   ^datascript.parser/pattern-element element]
  (if-some [variable (pattern-variable-name element)]
    (if-some [bound (relation-result relation variable row)]
      (parser/pattern-constant (result-pattern-value bound))
      element)
    element))

(defn ^:vector<datascript.parser/pattern-element> bind-pattern
  [^relation relation
   ^:array<result> row
   ^:vector<datascript.parser/pattern-element> pattern]
  (mapv
   (fn [^datascript.parser/pattern-element element]
     (bind-pattern-element relation row element))
   pattern))

(defn ^boolean identity-relation?
  [^relation relation]
  (let [rows (relation-rows relation)]
    (and
     (empty? (relation-attrs relation))
     (= 1 (count rows))
     (if-some [row (first rows)]
       (= 0 (alength row))
       false))))

(defn ^:option<result> constant-relation-result
  [^relation relation ^:string variable]
  (if-some [index (get (relation-attrs relation) variable)]
    (if-some [first-row (first (relation-rows relation))]
      (if-some [first-value (row-get first-row index)]
        (if
          (every?
           (fn [^:array<result> row]
             (if-some [value (row-get row index)]
               (Datascript_runtime.Query_value.equal_result
                first-value value)
               false))
           (relation-rows relation))
          (Some first-value)
          None)
        None)
      None)
    None))

(defn ^:vector<datascript.parser/pattern-element>
  substitute-pattern-constants
  [^relation relation
   ^:vector<datascript.parser/pattern-element> pattern]
  (mapv
   (fn [^datascript.parser/pattern-element element]
     (if-some [variable (pattern-variable-name element)]
       (if-some [value
                 (constant-relation-result relation variable)]
         (parser/pattern-constant (result-pattern-value value))
         element)
       element))
   pattern))

(defn ^:option<int> result-entity-id
  [^datascript.db/DB database ^result value]
  (match value
    (Datascript_runtime.Query_value.Entity eid) (Some eid)
    (Datascript_runtime.Query_value.Value value)
    (if-some [entity-ref
              (Datascript_runtime.Data_value.entity_ref_value value)]
      (datascript.db/entid database entity-ref)
      None)
    _ None))

(defn ^:option<Datascript_runtime.Data_value.t> pattern-row-value
  [^relation relation
   ^:array<result> row
   ^:option<datascript.parser/pattern-element> element]
  (match element
    None None
    (Some PatternPlaceholder) None
    (Some (PatternConstant value)) (Some value)
    (Some (PatternVariable _))
    (if-some [pattern-element element]
      (if-some [variable (pattern-variable-name pattern-element)]
        (if-some [value (relation-result relation variable row)]
          (Some (result-pattern-value value))
          None)
        None)
      None)))

(defn ^:tuple<vector<string>;vector<int>> unbound-pattern-projection
  [^relation relation
   ^:vector<datascript.parser/pattern-element> pattern]
  (let [attrs (relation-attrs relation)]
    (loop [remaining pattern
           index 0
           ^:vector<string> variables []
           ^:vector<int> indexes []]
      (if-some [element (first remaining)]
        (if-some [variable (pattern-variable-name element)]
          (if (contains? attrs variable)
            (recur
             (subvec remaining 1) (inc index)
             variables indexes)
            (recur
             (subvec remaining 1) (inc index)
             (conj variables variable)
             (conj indexes index)))
          (recur
           (subvec remaining 1) (inc index)
           variables indexes))
        (tuple variables indexes)))))

(defn ^:option<relation> resolve-bound-entity-pattern
  [^datascript.db/DB database
   ^relation input-relation
   ^:vector<datascript.parser/pattern-element> pattern]
  (if-some [entity-element (pattern-element-at pattern 0)]
    (if-some [entity-variable
              (pattern-variable-name entity-element)]
      (if (contains? (relation-attrs input-relation)
                     entity-variable)
        (match
         (pattern-attr-constraint
          (pattern-element-at pattern 1))
          (Some (Some attr))
          (let [entity-index
                (get
                 (relation-attrs input-relation)
                 entity-variable
                 0)
                value-element (pattern-element-at pattern 2)
                tx-element (pattern-element-at pattern 3)
                added
                (match
                 (pattern-added-constraint
                  (pattern-element-at pattern 4))
                  (Some value) value
                  None None)
                projection
                (unbound-pattern-projection
                 input-relation pattern)
                variables (tuple-get projection 0)
                indexes (to-array (tuple-get projection 1))
                rows
                (reduce
                 (fn [^:vector<array<result>> rows
                      ^:array<result> row]
                   (if-some
                     [entity-result
                      (row-get row entity-index)]
                     (if-some
                       [eid
                        (result-entity-id
                         database entity-result)]
                       (let [value
                             (pattern-row-value
                              input-relation row value-element)
                             tx-value
                             (pattern-row-value
                              input-relation row tx-element)
                             tx
                             (if-some [value tx-value]
                               (if-some
                                 [entity-ref
                                  (Datascript_runtime.Data_value.entity_ref_value
                                   value)]
                                 (datascript.db/entid
                                  database entity-ref)
                                 None)
                               None)
                             append-datom
                             (fn [^:vector<array<result>> rows
                                  ^datascript.db/Datom datom]
                               (if
                                 (if-some [expected added]
                                   (= expected
                                      (datascript.db/datom-added
                                       datom))
                                   true)
                                 (conj
                                  rows
                                  (concat-rows
                                   row
                                   (project-datom-row
                                    datom indexes)))
                                 rows))]
                         (match tx
                           None
                           (datascript.db/reduce-eavt-slice
                            database eid attr value
                            append-datom rows)
                           (Some _)
                           (reduce
                            append-datom
                            rows
                            (datascript.db/search-vector
                             database
                             (Some eid) (Some attr)
                             value tx))))
                       rows)
                     rows))
                 []
                 (relation-rows input-relation))
                attrs
                (product-attrs
                 (relation-attrs input-relation)
                 (index-attrs variables))
                lookup-databases
                (merge-lookup-databases
                 (relation-lookup-databases input-relation)
                 (pattern-lookup-databases
                  database pattern (Some attr)))]
            (Some
             (relation attrs rows lookup-databases)))
          _ None)
        None)
      None)
    None))

(defn ^relation resolve-db-pattern
  [^datascript.db/DB database
   ^relation input-relation
   ^:vector<datascript.parser/pattern-element> pattern]
  (if-some [resolved
            (resolve-bound-entity-pattern
             database input-relation pattern)]
    resolved
    (let [matches
          (lookup-db-pattern
           database
           (substitute-pattern-constants
            input-relation pattern))]
      (if (identity-relation? input-relation)
        matches
        (hash-join input-relation matches)))))

(defn ^relation resolve-db-patterns
  [^datascript.db/DB database
   ^relation initial-relation
   ^:vector<vector<datascript.parser/pattern-element>> patterns]
  (reduce
   (fn [^relation relation
        ^:vector<datascript.parser/pattern-element> pattern]
     (resolve-db-pattern database relation pattern))
   initial-relation
   patterns))

(defn ^predicate-operand compile-predicate-argument
  [^relation relation
   ^relation constants
   ^:datascript.parser/fn-arg argument]
  (if-some [variable (parser/argument-variable-name argument)]
    (let [attrs (relation-attrs relation)]
      (if (contains? attrs variable)
        (PredicateColumn (get attrs variable 0))
        (if-some [result
                  (constant-relation-result
                   constants variable)]
          (PredicateValue (result-pattern-value result))
          (Stdlib.invalid_arg
           (str "Predicate variable is not bound: " variable)))))
    (if-some [value (parser/argument-constant argument)]
      (PredicateValue value)
      (Stdlib.invalid_arg
       "Static predicates do not accept a database source argument"))))

(defn ^:Datascript_runtime.Data_value.t predicate-operand-value
  [^:array<result> row ^predicate-operand operand]
  (match operand
    (PredicateColumn index)
    (if-some [result (row-get row index)]
      (if-some [value (result-value result)]
        value
        (Stdlib.invalid_arg
         "Static numeric predicates require DataScript values"))
      (Stdlib.invalid_arg
       "Predicate column is outside the relation row"))
    (PredicateValue value) value))

(defn ^boolean greater-than-predicate?
  [^:array<result> row
   ^predicate-operand left
   ^predicate-operand right]
  (pos?
   (Datascript_runtime.Data_value.compare
    (predicate-operand-value row left)
    (predicate-operand-value row right))))

(defn ^relation resolve-predicate
  [^relation relation
   ^relation constants
   ^:datascript.parser/query-callable callable
   ^:vector<datascript.parser/fn-arg> arguments]
  (if-some [name (parser/static-callable-name callable)]
    (if (= name ">")
      (if (= (count arguments) 2)
        (let [left
              (compile-predicate-argument
               relation constants (nth arguments 0))
              right
              (compile-predicate-argument
               relation constants (nth arguments 1))]
          (relation-with-rows
           relation
           (reduce
            (fn [^:vector<array<result>> rows ^:array<result> row]
              (if (greater-than-predicate? row left right)
                (conj rows row)
                rows))
            []
            (relation-rows relation))))
        (Stdlib.invalid_arg
         "Static > predicate requires exactly two arguments"))
      (Stdlib.invalid_arg
       (str "Unsupported static query predicate: " name)))
    (Stdlib.invalid_arg
     "Variable query predicates are not a static callable boundary")))

(defn ^relation resolve-static-clauses
  [^datascript.db/DB database
   ^relation initial-relation
   ^relation constants
   ^:vector<datascript.parser/clause> clauses]
  (reduce
   (fn [^relation relation ^:datascript.parser/clause clause]
     (match clause
       (PatternClause DefaultSource pattern)
       (resolve-db-pattern
        database relation
        (substitute-pattern-constants constants pattern))
       (PredicateClause callable arguments)
       (resolve-predicate
        relation constants callable arguments)
       _
       (Stdlib.invalid_arg
        "Static query contains an unsupported clause")))
   initial-relation
   clauses))

(defn ^boolean single-row-constant-relation?
  [^relation relation]
  (and
   (= 1 (count (relation-rows relation)))
   (every?
    (fn [^:string variable]
      (some? (constant-relation-result relation variable)))
    (keys (relation-attrs relation)))))

(defn ^boolean relation-variables-used?
  [^relation relation ^:vector<string> variables]
  (some
   (fn [^:string variable]
     (contains? (relation-attrs relation) variable))
   variables))

(defn ^relation lookup-db-patterns
  [^datascript.db/DB database
   ^:vector<vector<datascript.parser/pattern-element>> patterns]
  (resolve-db-patterns database (identity-relation) patterns))

(defn ^result require-relation-result
  [^relation relation
   ^:array<result> row
   ^:string variable]
  (if-some [result (relation-result relation variable row)]
    result
    (Stdlib.invalid_arg
     (str "Query find variable is not bound: " variable))))

(defn ^:array<result> project-find-row
  [^relation relation
   ^:vector<string> variables
   ^:array<result> row]
  (to-array
   (mapv
    (fn [^:string variable]
      (require-relation-result relation row variable))
    variables)))

(defn ^:vector<array<result>> project-find-rows
  [^relation relation ^:vector<string> variables]
  (let [attrs (relation-attrs relation)
        ^:vector<int> indexes
        (mapv
         (fn [^:string variable]
           (if-some [index (get attrs variable)]
             index
             (Stdlib.invalid_arg
              (str "Query find variable is not bound: "
                   variable))))
         variables)
        identity?
        (and
         (= (count attrs) (count indexes))
         (loop [index 0]
           (if (< index (count indexes))
             (if (= index (nth indexes index))
               (recur (inc index))
               false)
             true)))]
    (if identity?
      (relation-rows relation)
      (let [indexes (to-array indexes)]
        (mapv
         (fn [^:array<result> row]
           (project-row row indexes))
         (relation-rows relation))))))

(defn ^result first-row-result [^:array<result> row]
  (if-some [result (row-get row 0)]
    result
    (Stdlib.invalid_arg "Find shape requires one projected value")))

(defn ^output find-output
  [^datascript.parser/find-spec find
   ^:vector<array<result>> rows]
  (cond
    (parser/relation-find? find)
    (Datascript_runtime.Query_value.relation_output rows)

    (parser/collection-find? find)
    (Datascript_runtime.Query_value.collection_output
     (mapv first-row-result rows))

    (parser/single-find? find)
    (Datascript_runtime.Query_value.scalar_output
     (if-some [row (first rows)]
       (Some (first-row-result row))
       None))

    :else
    (Datascript_runtime.Query_value.tuple_output
     (first rows))))

(defn ^output execute-db-query-with-relation
  [^datascript.db/DB database
   ^datascript.parser/Query query
   ^relation input-relation]
  (if-some [variables
            (parser/find-variable-names (.-qfind query))]
    (let [elide-input?
          (and
           (single-row-constant-relation? input-relation)
           (not
            (relation-variables-used?
             input-relation variables)))
          initial-relation
          (if elide-input?
            (identity-relation)
            input-relation)
          constants
          (if elide-input?
            input-relation
            (identity-relation))
          relation
          (resolve-static-clauses
           database initial-relation constants
           (.-qwhere query))
          rows (project-find-rows relation variables)
          find (.-qfind query)]
      (find-output find rows))
    (Stdlib.invalid_arg
     "Static query find supports variables only")))

(defn ^output execute-db-query
  [^datascript.db/DB database ^datascript.parser/Query query]
  (execute-db-query-with-relation
   database query (identity-relation)))

(defn ^datascript.db/DB require-database-input [^input input]
  (if-some [source (input-source input)]
    (if-some [database (source-database source)]
      database
      (Stdlib.invalid_arg
       "Static DB query requires a Database_source"))
    (Stdlib.invalid_arg
     "Static DB query input must be a Source_input")))

(defn ^binding-value require-binding-input [^input input]
  (if-some [binding (input-binding input)]
    binding
    (Stdlib.invalid_arg
     "Query value input requires a Binding_input")))

(defn ^relation tuple-binding-relation
  [^:vector<datascript.parser/binding> bindings
   ^:vector<binding-value> values]
  (if (= (count bindings) (count values))
    (loop [remaining-bindings bindings
           remaining-values values
           relation (identity-relation)]
      (if-some [binding (first remaining-bindings)]
        (if-some [value (first remaining-values)]
          (recur
           (subvec remaining-bindings 1)
           (subvec remaining-values 1)
           (product-relation
            relation
            (binding-relation binding value)))
          (Stdlib.invalid_arg "Missing tuple binding value"))
        relation))
    (Stdlib.invalid_arg
     "Tuple query input has the wrong number of values")))

(defn ^relation collection-binding-relation
  [^datascript.parser/binding binding
   ^:vector<binding-value> values]
  (if (empty? values)
    (empty-relation
     (index-attrs
      (parser/binding-variable-names binding))
     (empty-lookup-databases))
    (let [relations
          (mapv
           (fn [^binding-value value]
             (binding-relation binding value))
           values)]
      (if-some [first-relation (first relations)]
        (reduce
         sum-relation
         first-relation
         (subvec relations 1))
        (Stdlib.invalid_arg
         "Collection binding relation is unexpectedly empty")))))

(defn ^relation binding-relation
  [^datascript.parser/binding binding ^binding-value value]
  (cond
    (parser/binding-ignore? binding)
    (identity-relation)

    (some? (parser/binding-scalar-variable binding))
    (if-some [variable (parser/binding-scalar-variable binding)]
      (if-some [result (binding-result value)]
        (relation
         {variable 0}
         [(array result)]
         (empty-lookup-databases))
        (Stdlib.invalid_arg
         "Scalar query input requires a Scalar_binding"))
      (Stdlib.invalid_arg "Scalar binding variable is missing"))

    (some? (parser/binding-tuple-items binding))
    (if-some [bindings (parser/binding-tuple-items binding)]
      (if-some [values (binding-items value)]
        (tuple-binding-relation bindings values)
        (Stdlib.invalid_arg
         "Tuple query input requires a Collection_binding"))
      (Stdlib.invalid_arg "Tuple binding items are missing"))

    :else
    (if-some [item (parser/binding-collection-item binding)]
      (if-some [values (binding-items value)]
        (collection-binding-relation item values)
        (Stdlib.invalid_arg
         "Collection query input requires a Collection_binding"))
      (Stdlib.invalid_arg "Unsupported static query binding"))))

(defn ^relation binding-input-relations
  [^:vector<datascript.parser/binding> bindings
   ^:vector<input> inputs]
  (loop [remaining-bindings bindings
         remaining-inputs inputs
         relation (identity-relation)]
    (if-some [binding (first remaining-bindings)]
      (if-some [input (first remaining-inputs)]
        (recur
         (subvec remaining-bindings 1)
         (subvec remaining-inputs 1)
         (let [bound
               (binding-relation
                binding
                (require-binding-input input))]
           (if (identity-relation? relation)
             bound
             (hash-join relation bound))))
        (Stdlib.invalid_arg "Missing query binding input"))
      relation)))

(defn ^output execute-query
  [^datascript.parser/Query query ^:vector<input> inputs]
  (if-some [bindings (parser/static-query-value-bindings query)]
    (if (= (count inputs) (inc (count bindings)))
      (if-some [source-input (first inputs)]
        (execute-db-query-with-relation
         (require-database-input source-input)
         query
         (binding-input-relations
          bindings
          (subvec inputs 1)))
        (Stdlib.invalid_arg
         "Static DB query requires one source input"))
      (Stdlib.invalid_arg
       "Static query input count does not match parsed :in"))
    (Stdlib.invalid_arg
     "Static query supports one source followed by scalar bindings")))

(defn ^:option<datascript.db/DB> relation-lookup-database
  [^relation relation ^:string variable]
  (Datascript_runtime.Query_value.relation_lookup_database
   relation variable))

(defn context
  [^:vector<relation> relations
   ^:map<string;source> sources
   ^rules rules]
  (Datascript_runtime.Query_value.context relations sources rules))

(defn ^:vector<relation> context-relations [^context context]
  (Datascript_runtime.Query_value.context_relations context))

(defn ^:map<string;source> context-sources [^context context]
  (Datascript_runtime.Query_value.context_sources context))

(defn ^rules context-rules [^context context]
  (Datascript_runtime.Query_value.context_rules context))
