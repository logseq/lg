(ns datascript.lg.query-types
  (:require
   [datascript.built-ins :as built-ins]
   [datascript.db :as db]
   [datascript.parser :as parser]
   [datascript.pull-api :as pull-api]
   [datascript.pull-parser :as pull-parser]))

(type-alias result
  :Datascript_runtime.Query_value.result<datascript.db/database-view>)
(type-alias source
  :Datascript_runtime.Query_value.source<datascript.db/database-view>)
(type-alias binding-value
  :Datascript_runtime.Query_value.binding_value<datascript.db/database-view>)
(type-alias callable
  :Datascript_runtime.Query_value.callable<datascript.db/database-view>)
(type-alias relation
  :Datascript_runtime.Query_value.relation<datascript.db/database-view>)
(type-alias rules
  :vector<datascript.parser/Rule>)
(type-alias input
  :Datascript_runtime.Query_value.input<datascript.db/database-view;rules>)
(type-alias output
  :Datascript_runtime.Query_value.output<datascript.db/database-view>)
(type-alias context
  :Datascript_runtime.Query_value.context<datascript.db/database-view;rules>)

(type-variant predicate-operand
  (PredicateColumn :int)
  (PredicateResult
   :Datascript_runtime.Query_value.result<datascript.db/database-view>))

(type-variant static-predicate-function
  (ComparisonStaticPredicate :datascript.built-ins/query-function)
  (PureStaticPredicate :datascript.built-ins/query-function))

(type-variant rule-call-argument
  (RuleCallVariable :string :vector<result>)
  (RuleCallConstant :Datascript_runtime.Data_value.t))

(type-variant rule-call
  (RuleCall :string :vector<rule-call-argument>))

(type-alias rule-path :vector<rule-call>)

(signature datascript.lg.query-types/entity-result
  :fn<int;result>)
(signature datascript.lg.query-types/attr-result
  :fn<keyword;result>)
(signature datascript.lg.query-types/value-result
  :fn<Datascript_runtime.Data_value.t;result>)
(signature datascript.lg.query-types/metadata-result
  :fn<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t;result>)
(signature datascript.lg.query-types/result-value
  :fn<result;option<Datascript_runtime.Data_value.t>>)
(signature datascript.lg.query-types/result-metadata
  :fn<result;option<Datascript_runtime.Data_value.t>>)
(signature datascript.lg.query-types/database-result
  :fn<datascript.db/database-view;result>)
(signature datascript.lg.query-types/pull-result
  :fn<Datascript_runtime.Data_value.t;result>)
(signature datascript.lg.query-types/added-result
  :fn<bool;result>)
(signature datascript.lg.query-types/callable
  :fn<fn<vector<result>;option<Datascript_runtime.Data_value.t>>;callable>)
(signature datascript.lg.query-types/callable-result
  :fn<callable;result>)
(signature datascript.lg.query-types/result-callable
  :fn<result;option<callable>>)
(signature datascript.lg.query-types/invoke-callable
  :fn<callable;vector<result>;option<Datascript_runtime.Data_value.t>>)
(signature datascript.lg.query-types/result-nil?
  :fn<result;bool>)
(signature datascript.lg.query-types/complement-result
  :fn<vector<result>;result>)
(signature datascript.lg.query-types/metadata-function-result
  :fn<vector<result>;result>)
(signature datascript.lg.query-types/value-type-function-result
  :fn<vector<result>;result>)
(signature datascript.lg.query-types/function-binding-result
  :fn<datascript.parser/binding;result;binding-value>)
(signature datascript.lg.query-types/database-source
  :fn<datascript.db/database-view;source>)
(signature datascript.lg.query-types/relation-source
  :fn<vector<array<result>>;source>)
(signature datascript.lg.query-types/source-database
  :fn<source;option<datascript.db/database-view>>)
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
(signature datascript.lg.query-types/output-keyword-relation
  :fn<output;option<vector<map<string;result>>>>)
(signature datascript.lg.query-types/output-symbol-relation
  :fn<output;option<vector<map<string;result>>>>)
(signature datascript.lg.query-types/output-string-relation
  :fn<output;option<vector<map<string;result>>>>)
(signature datascript.lg.query-types/output-keyword-tuple
  :fn<output;option<option<map<string;result>>>>)
(signature datascript.lg.query-types/output-symbol-tuple
  :fn<output;option<option<map<string;result>>>>)
(signature datascript.lg.query-types/output-string-tuple
  :fn<output;option<option<map<string;result>>>>)
(signature datascript.lg.query-types/row-get
  :fn<array<result>;int;option<result>>)
(signature datascript.lg.query-types/empty-row
  :fn<unit;array<result>>)
(signature datascript.lg.query-types/project-row
  :fn<array<result>;array<int>;array<result>>)
(signature datascript.lg.query-types/distinct-rows
  :fn<vector<array<result>>;vector<array<result>>>)
(signature datascript.lg.query-types/query-with-variable-names
  :fn<datascript.parser/Query;vector<string>>)
(signature datascript.lg.query-types/group-rows
  :fn<vector<array<result>>;array<int>;vector<vector<array<result>>>>)
(signature datascript.lg.query-types/join-rows
  :fn<array<result>;array<int>;array<result>;array<int>;array<result>>)
(signature datascript.lg.query-types/concat-rows
  :fn<array<result>;array<result>;array<result>>)
(signature datascript.lg.query-types/datom-row
  :fn<datascript.db/Datom;array<result>>)
(signature datascript.lg.query-types/relation
  :fn<map<string;int>;vector<array<result>>;map<string;datascript.db/database-view>;relation>)
(signature datascript.lg.query-types/datom-relation
  :fn<map<string;int>;array<int>;vector<datascript.db/Datom>;map<string;datascript.db/database-view>;relation>)
(signature datascript.lg.query-types/index-attrs
  :fn<vector<string>;map<string;int>>)
(signature datascript.lg.query-types/pattern-relation
  :fn<vector<string>;array<int>;vector<datascript.db/Datom>;map<string;datascript.db/database-view>;relation>)
(signature datascript.lg.query-types/lookup-db-pattern
  :fn<datascript.db/database-view;vector<datascript.parser/pattern-element>;relation>)
(signature datascript.lg.query-types/lookup-db-patterns
  :fn<datascript.db/database-view;vector<vector<datascript.parser/pattern-element>>;relation>)
(signature datascript.lg.query-types/execute-db-query
  :fn<datascript.db/database-view;datascript.parser/Query;output>)
(signature datascript.lg.query-types/execute-query
  :fn<datascript.parser/Query;vector<input>;output>)
(signature datascript.lg.query-types/empty-relation
  :fn<map<string;int>;map<string;datascript.db/database-view>;relation>)
(signature datascript.lg.query-types/relation-result
  :fn<relation;string;array<result>;option<result>>)
(signature datascript.lg.query-types/relation-attrs
  :fn<relation;map<string;int>>)
(signature datascript.lg.query-types/relation-rows
  :fn<relation;vector<array<result>>>)
(signature datascript.lg.query-types/relation-lookup-databases
  :fn<relation;map<string;datascript.db/database-view>>)
(signature datascript.lg.query-types/relation-with-rows
  :fn<relation;vector<array<result>>;relation>)
(signature datascript.lg.query-types/sum-relation
  :fn<relation;relation;relation>)
(signature datascript.lg.query-types/product-relation
  :fn<relation;relation;relation>)
(signature datascript.lg.query-types/resolve-lookup-result
  :fn<datascript.db/database-view;result;result>)
(signature datascript.lg.query-types/hash-join
  :fn<relation;relation;relation>)
(signature datascript.lg.query-types/product-attrs
  :fn<map<string;int>;map<string;int>;map<string;int>>)
(signature datascript.lg.query-types/merge-lookup-databases
  :fn<map<string;datascript.db/database-view>;map<string;datascript.db/database-view>;map<string;datascript.db/database-view>>)
(signature datascript.lg.query-types/relation-lookup-database
  :fn<relation;string;option<datascript.db/database-view>>)
(signature datascript.lg.query-types/context
  :fn<vector<relation>;map<string;source>;rules;context>)
(signature datascript.lg.query-types/context-relations
  :fn<context;vector<relation>>)
(signature datascript.lg.query-types/context-sources
  :fn<context;map<string;source>>)
(signature datascript.lg.query-types/context-rules
  :fn<context;rules>)

(declare
 empty-relation
 binding-relation
 query-source-database)

(defn ^:map<string;datascript.db/database-view> empty-lookup-databases []
  {})
(defn entity-result [entity]
  (Datascript_runtime.Query_value.entity entity))

(defn attr-result [attr]
  (Datascript_runtime.Query_value.attr (str attr)))

(defn value-result [value]
  (Datascript_runtime.Query_value.value value))

(defn metadata-result
  [value metadata]
  (Datascript_runtime.Query_value.metadata value metadata))

(defn result-value [result]
  (Datascript_runtime.Query_value.result_value result))

(defn result-metadata
  [result]
  (Datascript_runtime.Query_value.result_metadata result))

(defn database-result [database]
  (Datascript_runtime.Query_value.database database))

(defn pull-result [value]
  (Datascript_runtime.Query_value.pull value))

(defn added-result [added]
  (Datascript_runtime.Query_value.added added))

(defn callable [invoke]
  (Datascript_runtime.Query_value.callable invoke))

(defn callable-result [callable]
  (Datascript_runtime.Query_value.callable_result callable))

(defn result-callable [result]
  (Datascript_runtime.Query_value.result_callable result))

(defn invoke-callable [callable arguments]
  (Datascript_runtime.Query_value.invoke_callable callable arguments))

(defn database-source [database]
  (Datascript_runtime.Query_value.database_source database))

(defn relation-source [rows]
  (Datascript_runtime.Query_value.relation_source rows))

(defn source-database [source]
  (Datascript_runtime.Query_value.source_database source))

(defn source-rows [source]
  (Datascript_runtime.Query_value.source_rows source))

(defn scalar-binding [result]
  (Datascript_runtime.Query_value.scalar_binding result))

(defn collection-binding [values]
  (Datascript_runtime.Query_value.collection_binding values))

(defn binding-result [binding]
  (Datascript_runtime.Query_value.binding_result binding))

(defn binding-items [binding]
  (Datascript_runtime.Query_value.binding_items binding))

(defn source-input [source]
  (Datascript_runtime.Query_value.source_input source))

(defn rules-input [rules]
  (Datascript_runtime.Query_value.rules_input rules))

(defn binding-input [binding]
  (Datascript_runtime.Query_value.binding_input binding))

(defn invalid-input [message]
  (Stdlib.invalid_arg message))

(defn invalid-query-output [message]
  (Stdlib.invalid_arg message))

(defn invalid-query-output-after-validation [query message]
  (parser/validate-static-query-sources query)
  (Stdlib.invalid_arg message))

(defn invalid-binding [message]
  (Stdlib.invalid_arg message))

(defn input-source [input]
  (Datascript_runtime.Query_value.input_source input))

(defn input-rules [input]
  (Datascript_runtime.Query_value.input_rules input))

(defn input-binding [input]
  (Datascript_runtime.Query_value.input_binding input))

(defn output-relation [output]
  (Datascript_runtime.Query_value.output_relation output))

(defn output-collection [output]
  (Datascript_runtime.Query_value.output_collection output))

(defn output-scalar [output]
  (Datascript_runtime.Query_value.output_scalar output))

(defn output-tuple [output]
  (Datascript_runtime.Query_value.output_tuple output))

(defn output-keyword-relation [output]
  (Datascript_runtime.Query_value.output_keyword_relation output))

(defn output-symbol-relation [output]
  (Datascript_runtime.Query_value.output_symbol_relation output))

(defn output-string-relation [output]
  (Datascript_runtime.Query_value.output_string_relation output))

(defn output-keyword-tuple [output]
  (Datascript_runtime.Query_value.output_keyword_tuple output))

(defn output-symbol-tuple [output]
  (Datascript_runtime.Query_value.output_symbol_tuple output))

(defn output-string-tuple [output]
  (Datascript_runtime.Query_value.output_string_tuple output))

(defn row-get [row index]
  (Datascript_runtime.Query_value.row_get row index))

(defn empty-row []
  (Datascript_runtime.Query_value.empty_row (Stdlib.ignore 0)))

(defn project-row [row indexes]
  (Datascript_runtime.Query_value.project_row row indexes))

(defn distinct-rows [rows]
  (Datascript_runtime.Query_value.distinct_rows rows))

(defn query-with-variable-names [query]
  (if-some [variables (.-qwith query)]
    (mapv
     (fn [variable]
       (str (.-symbol variable)))
     variables)
    []))

(defn group-rows [rows indexes]
  (Datascript_runtime.Query_value.group_rows rows indexes))

(defn require-row-result [row index]
  (if-some [result (row-get row index)]
    result
    (Stdlib.invalid_arg "Aggregate row index is out of bounds")))

(defn count-distinct-aggregate [rows index]
  (value-result
   (Datascript_runtime.Data_value.Int
    (count
     (distinct-rows
      (mapv
       (fn [row]
         (array (require-row-result row index)))
       rows))))))

(defn apply-minimum-aggregate [parameters rows index]
  (if (empty? parameters)
    (Datascript_runtime.Query_value.aggregate_minimum rows index)
    (if (= 1 (count parameters))
      (if-some [parameter (first parameters)]
        (match parameter
          (Datascript_runtime.Data_value.Int count)
          (Datascript_runtime.Query_value.aggregate_minimum_n
           rows index count)
          _
          (Stdlib.invalid_arg "min aggregate count must be an integer"))
        (Stdlib.invalid_arg "min aggregate parameter is missing"))
      (Stdlib.invalid_arg "min aggregate expects zero or one parameter"))))

(defn apply-maximum-aggregate [parameters rows index]
  (if (empty? parameters)
    (Datascript_runtime.Query_value.aggregate_maximum rows index)
    (if (= 1 (count parameters))
      (if-some [parameter (first parameters)]
        (match parameter
          (Datascript_runtime.Data_value.Int count)
          (Datascript_runtime.Query_value.aggregate_maximum_n
           rows index count)
          _
          (Stdlib.invalid_arg "max aggregate count must be an integer"))
        (Stdlib.invalid_arg "max aggregate parameter is missing"))
      (Stdlib.invalid_arg "max aggregate expects zero or one parameter"))))

(defn apply-random-aggregate [parameters rows index]
  (if (empty? parameters)
    (Datascript_runtime.Query_value.aggregate_random rows index)
    (if (= 1 (count parameters))
      (if-some [parameter (first parameters)]
        (match parameter
          (Datascript_runtime.Data_value.Int count)
          (Datascript_runtime.Query_value.aggregate_random_n
           rows index count)
          _
          (Stdlib.invalid_arg "rand aggregate count must be an integer"))
        (Stdlib.invalid_arg "rand aggregate parameter is missing"))
      (Stdlib.invalid_arg "rand aggregate expects zero or one parameter"))))

(defn apply-sample-aggregate [parameters rows index]
  (if (= 1 (count parameters))
    (if-some [parameter (first parameters)]
      (match parameter
        (Datascript_runtime.Data_value.Int count)
        (Datascript_runtime.Query_value.aggregate_sample
         rows index count)
        _
        (Stdlib.invalid_arg "sample aggregate count must be an integer"))
      (Stdlib.invalid_arg "sample aggregate parameter is missing"))
    (Stdlib.invalid_arg "sample aggregate expects one parameter")))

(defn apply-built-in-aggregate [function parameters rows index]
  (cond
    (built-ins/sum-aggregate? function)
    (Datascript_runtime.Query_value.aggregate_sum rows index)

    (built-ins/average-aggregate? function)
    (Datascript_runtime.Query_value.aggregate_average rows index)

    (built-ins/median-aggregate? function)
    (Datascript_runtime.Query_value.aggregate_median rows index)

    (built-ins/variance-aggregate? function)
    (Datascript_runtime.Query_value.aggregate_variance rows index)

    (built-ins/standard-deviation-aggregate? function)
    (Datascript_runtime.Query_value.aggregate_standard_deviation
     rows index)

    (built-ins/distinct-aggregate? function)
    (Datascript_runtime.Query_value.aggregate_distinct rows index)

    (built-ins/minimum-aggregate? function)
    (apply-minimum-aggregate parameters rows index)

    (built-ins/maximum-aggregate? function)
    (apply-maximum-aggregate parameters rows index)

    (built-ins/random-aggregate? function)
    (apply-random-aggregate parameters rows index)

    (built-ins/sample-aggregate? function)
    (apply-sample-aggregate parameters rows index)

    (built-ins/count-aggregate? function)
    (value-result
     (Datascript_runtime.Data_value.Int (count rows)))

    (built-ins/count-distinct-aggregate? function)
    (count-distinct-aggregate rows index)

    :else
    (Stdlib.invalid_arg
     "Aggregate function is not implemented yet")))

(defn aggregate-callable [variable constants parameter-relation]
  (if-some [value (constant-relation-result constants variable)]
    (result-callable value)
    (if-some [value
              (constant-relation-result parameter-relation variable)]
      (result-callable value)
      None)))

(defn apply-custom-aggregate
  [variable constants parameter-relation rows index]
  (if-some [callable
            (aggregate-callable
             variable constants parameter-relation)]
    (if-some [value
              (invoke-callable
               callable
               (mapv
                (fn [row]
                  (require-row-result row index))
                rows))]
      (value-result value)
      (value-result (Datascript_runtime.Data_value.Nil)))
    (Stdlib.invalid_arg
     (str "Custom aggregate callable is not bound: " variable))))

(defn apply-find-aggregate
  [aggregate constants parameter-relation rows index]
  (if-some [function-name
            (parser/aggregate-function-name aggregate)]
    (let [arguments (.-args aggregate)
          parameter-count (dec (count arguments))
          parameters
          (if (< parameter-count 0)
            None
            (loop [remaining (subvec arguments 0 parameter-count)
                   values []]
              (if-some [argument (first remaining)]
                (if-some [value (parser/argument-constant argument)]
                  (recur (subvec remaining 1) (conj values value))
                  (if-some [variable
                            (parser/argument-variable-name argument)]
                    (if-some [value
                              (constant-relation-result
                               constants variable)]
                      (recur
                       (subvec remaining 1)
                       (conj values (result-pattern-value value)))
                      (if-some [value
                                (constant-relation-result
                                 parameter-relation variable)]
                        (recur
                         (subvec remaining 1)
                         (conj values (result-pattern-value value)))
                        None))
                    None))
                (Some values))))]
      (if-some [parameters parameters]
      (if-some [function
                (built-ins/aggregate-function function-name)]
        (apply-built-in-aggregate
         function parameters rows index)
        (Stdlib.invalid_arg
         (str "Unknown aggregate function: " function-name)))
      (Stdlib.invalid_arg
       "Aggregate parameters must be constants")))
    (if-some [variable
              (parser/aggregate-custom-variable-name aggregate)]
      (apply-custom-aggregate
       variable constants parameter-relation rows index)
      (Stdlib.invalid_arg
       "Custom aggregate function is missing"))))

(defn aggregate-group-indexes [elements]
  (loop [remaining elements
         index 0
         indexes []]
    (if-some [element (first remaining)]
      (recur
       (subvec remaining 1)
       (inc index)
       (if (parser/aggregate? element)
         indexes
         (conj indexes index)))
      indexes)))

(defn aggregate-group-row
  [elements constants parameter-relation rows]
  (if-some [first-row (first rows)]
    (to-array
     (mapv
      (fn [element index]
        (if-some [aggregate
                  (parser/find-element-aggregate element)]
          (apply-find-aggregate
           aggregate constants parameter-relation rows index)
          (require-row-result first-row index)))
      elements
      (range (count elements))))
    (empty-row)))

(defn aggregate-rows [elements constants parameter-relation rows]
  (mapv
   (fn [group]
     (aggregate-group-row
      elements constants parameter-relation group))
   (group-rows
    rows
    (to-array (aggregate-group-indexes elements)))))

(defn ^:option<vector<datascript.pull-parser/pull-source-item>>
  pull-source-items
  [^:Datascript_runtime.Data_value.t pattern]
  (if-some [items
            (Datascript_runtime.Data_value.sequential_items pattern)]
    (loop [remaining items
           source []]
      (if-some [item (first remaining)]
        (if-some [attribute
                  (Datascript_runtime.Data_value.keyword_value item)]
          (recur
           (subvec remaining 1)
           (conj
            source
            (if (= attribute ":*")
              pull-parser/source-wildcard
              (pull-parser/source-attribute
               (keyword attribute)))))
          None)
        (Some source)))
    None))

(defn ^:option<Datascript_runtime.Data_value.entity_ref>
  query-entity-ref
  [^:Datascript_runtime.Data_value.t value]
  (if-some [entity-ref
            (Datascript_runtime.Data_value.entity_ref_value value)]
    (Some entity-ref)
    (if-some [lookup-ref
              (Datascript_runtime.Data_value.lookup_ref_value value)]
      (Some
       (Datascript_runtime.Data_value.Lookup_ref
        (tuple-get lookup-ref 0)
        (tuple-get lookup-ref 1)))
      (if-some [ident
                (Datascript_runtime.Data_value.keyword_value value)]
        (Some (Datascript_runtime.Data_value.Ident ident))
        None))))

(defn ^:option<Datascript_runtime.Data_value.entity_ref>
  result-entity-ref
  [^result result]
  (match result
    (Datascript_runtime.Query_value.Entity entity)
    (Some (Datascript_runtime.Data_value.Entity_id entity))
    (Datascript_runtime.Query_value.Value value)
    (query-entity-ref value)
    (Datascript_runtime.Query_value.Metadata value _)
    (query-entity-ref value)
    _ None))

(defn ^:vector<string> pull-pattern-variable-names
  [^:vector<datascript.parser/find-element> elements]
  (vec
   (mapcat
    (fn [^datascript.parser/find-element element]
      (if-some [pull (parser/find-element-pull element)]
        (if-some [variable
                  (parser/pull-pattern-variable-name pull)]
          [variable]
          [])
        []))
    elements)))

(defn ^:option<Datascript_runtime.Data_value.t>
  relation-variable-first-value
  [^relation relation ^:string variable]
  (if-some [row (first (relation-rows relation))]
    (if-some [result (relation-result relation variable row)]
      (result-value result)
      None)
    None))

(defn ^:option<Datascript_runtime.Data_value.t>
  resolve-pull-pattern
  [^datascript.parser/Pull pull ^relation relation]
  (if-some [pattern (parser/pull-pattern-value pull)]
    (Some pattern)
    (if-some [variable
              (parser/pull-pattern-variable-name pull)]
      (relation-variable-first-value relation variable)
      None)))

(defn ^:vector<option<Datascript_runtime.Data_value.t>>
  resolve-pull-patterns
  [^:vector<datascript.parser/find-element> elements
   ^relation relation]
  (mapv
   (fn [^datascript.parser/find-element element]
     (if-some [pull (parser/find-element-pull element)]
       (resolve-pull-pattern pull relation)
       None))
   elements))

(defn ^result apply-find-pull
  [^datascript.db/database-view database
   ^:Datascript_runtime.Data_value.t pattern
   ^result entity]
  (if-some [source (pull-source-items pattern)]
    (if-some [entity-ref (result-entity-ref entity)]
      (if-some
        [values
         (pull-api/pull-source
          database source entity-ref)]
        (pull-result
         (Datascript_runtime.Data_value.map_of_data_map values))
        (pull-result (Datascript_runtime.Data_value.Nil)))
      (Stdlib.invalid_arg
       "Pull find entity must be an entity reference"))
    (Stdlib.invalid_arg
     "Pull find pattern must contain attributes or wildcard")))

(defn ^:array<result> pull-row
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^:vector<datascript.parser/find-element> elements
   ^:vector<option<Datascript_runtime.Data_value.t>> patterns
   ^:array<result> row]
  (to-array
   (mapv
    (fn [^datascript.parser/find-element element
         ^:option<Datascript_runtime.Data_value.t> pattern
         ^:int index]
      (if-some [pull (parser/find-element-pull element)]
        (if-some [pattern pattern]
          (apply-find-pull
           (query-source-database
            database sources (parser/pull-source-name pull))
           pattern
           (require-row-result row index))
          (Stdlib.invalid_arg
           "Pull find pattern variable is not bound"))
        (require-row-result row index)))
    elements
    patterns
    (range (count elements)))))

(defn ^:vector<array<result>> pull-rows
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^:vector<datascript.parser/find-element> elements
   ^:vector<option<Datascript_runtime.Data_value.t>> patterns
   ^:vector<array<result>> rows]
  (mapv
   (fn [^:array<result> row]
     (pull-row database sources elements patterns row))
   rows))

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
   ^:map<string;datascript.db/database-view> lookup-databases]
  (Datascript_runtime.Query_value.relation attrs rows lookup-databases))

(defn ^relation datom-relation
  [^:map<string;int> attrs
   ^:array<int> indexes
   ^:vector<datascript.db/Datom> datoms
   ^:map<string;datascript.db/database-view> lookup-databases]
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
   ^:map<string;datascript.db/database-view> lookup-databases]
  (if (= (count variables) (alength indexes))
    (datom-relation
     (index-attrs variables)
     indexes
     datoms
     lookup-databases)
    (Stdlib.invalid_arg
     "Pattern variables and indexes must have the same length")))

(defn ^:option<option<int>> pattern-entity-constraint
  [^datascript.db/database-view database
   ^:option<datascript.parser/pattern-element> element]
  (match element
    None (Some None)
    (Some PatternPlaceholder) (Some None)
    (Some (PatternVariable _)) (Some None)
    (Some (PatternConstant value))
    (if-some [entity-ref
              (query-entity-ref value)]
      (match (datascript.db/database-view-entid database entity-ref)
        None
        (match entity-ref
          (Datascript_runtime.Data_value.Lookup_ref attr-name lookup-value)
          (Stdlib.invalid_arg
           (str
            "Nothing found for entity id ["
            (keyword attr-name)
            " "
            (Datascript_runtime.Data_value.to_edn_string lookup-value)
            "]"))
          _ None)
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

(defn ^:option<option<Datascript_runtime.Data_value.t>>
  resolve-pattern-value-constraint
  [^datascript.db/database-view database
   ^:option<keyword> attr
   ^:option<Datascript_runtime.Data_value.t> value]
  (if-some [attr attr]
    (if (datascript.db/database-view-ref? database attr)
      (if-some [value value]
        (if-some [entity-ref (query-entity-ref value)]
          (if-some
            [eid
             (datascript.db/database-view-entid database entity-ref)]
            (Some
             (Some
              (Datascript_runtime.Data_value.Ref eid)))
            None)
          (Some (Some value)))
        (Some None))
      (Some value))
    (Some value)))

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

(defn ^:map<string;datascript.db/database-view> pattern-lookup-databases
  [^datascript.db/database-view database
   ^:vector<datascript.parser/pattern-element> pattern
   ^:option<keyword> attr]
  (let [databases
        (reduce
         (fn [^:map<string;datascript.db/database-view> databases
              ^:int index]
           (if-some [element (pattern-element-at pattern index)]
             (if-some [variable (pattern-variable-name element)]
               (assoc databases variable database)
               databases)
             databases))
         {}
         [0 3])]
    (if-some [attr attr]
      (if (datascript.db/database-view-ref? database attr)
        (if-some [element (pattern-element-at pattern 2)]
          (if-some [variable (pattern-variable-name element)]
            (assoc databases variable database)
            databases)
          databases)
        databases)
      databases)))

(defn ^relation lookup-db-pattern
  [^datascript.db/database-view database
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
            (if-some
              [resolved-value
               (resolve-pattern-value-constraint
                database attr value)]
              (datascript.db/database-view-search-vector
               database entity attr resolved-value tx)
              [])
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
   ^:map<string;datascript.db/database-view> lookup-databases]
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

(defn ^:map<string;datascript.db/database-view> relation-lookup-databases
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

(defn ^:map<string;datascript.db/database-view> merge-lookup-databases
  [^:map<string;datascript.db/database-view> left
   ^:map<string;datascript.db/database-view> right]
  (merge left right))

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
  [^datascript.db/database-view database ^result result]
  (match result
    (Datascript_runtime.Query_value.Value value)
    (match (query-entity-ref value)
      (Some entity-ref)
      (match (datascript.db/database-view-entid database entity-ref)
        (Some entity) (entity-result entity)
        None result)
      None result)
    (Datascript_runtime.Query_value.Metadata value _)
    (match (query-entity-ref value)
      (Some entity-ref)
      (match (datascript.db/database-view-entid database entity-ref)
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
    (Datascript_runtime.Data_value.Int entity)
    (Datascript_runtime.Query_value.Attr attr)
    (Datascript_runtime.Data_value.Keyword attr)
    (Datascript_runtime.Query_value.Value value) value
    (Datascript_runtime.Query_value.Metadata value _) value
    (Datascript_runtime.Query_value.Pull value) value
    (Datascript_runtime.Query_value.Added added)
    (Datascript_runtime.Data_value.Keyword
     (if added ":db/add" ":db/retract"))
    (Datascript_runtime.Query_value.Database _)
    (Stdlib.invalid_arg
     "A database query result cannot bind a pattern component")
    (Datascript_runtime.Query_value.Callable _)
    (Stdlib.invalid_arg
     "A callable query result cannot bind a pattern component")))

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
  [^datascript.db/database-view database ^result value]
  (match value
    (Datascript_runtime.Query_value.Entity eid) (Some eid)
    (Datascript_runtime.Query_value.Value value)
    (if-some [entity-ref
              (query-entity-ref value)]
      (datascript.db/database-view-entid database entity-ref)
      None)
    (Datascript_runtime.Query_value.Metadata value _)
    (if-some [entity-ref
              (query-entity-ref value)]
      (datascript.db/database-view-entid database entity-ref)
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
  [^datascript.db/database-view database
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
                             (resolve-pattern-value-constraint
                              database
                              (Some attr)
                              (pattern-row-value
                               input-relation row value-element))
                             tx-value
                             (pattern-row-value
                              input-relation row tx-element)
                             tx
                             (if-some [value tx-value]
                               (if-some
                                 [entity-ref
                                  (Datascript_runtime.Data_value.entity_ref_value
                                   value)]
                                 (datascript.db/database-view-entid
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
                         (if-some [value value]
                           (match tx
                             None
                             (datascript.db/database-view-reduce-eavt-slice
                              database eid attr value
                              append-datom rows)
                             (Some _)
                             (reduce
                              append-datom
                              rows
                              (datascript.db/database-view-search-vector
                               database
                               (Some eid) (Some attr)
                               value tx)))
                           rows))
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
  [^datascript.db/database-view database
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

(defn ^:option<tuple<map<string;result>;vector<result>>>
  relation-pattern-step
  [^:map<string;result> bindings
   ^:vector<result> projected
   ^datascript.parser/pattern-element element
   ^result value]
  (match element
    PatternPlaceholder
    (Some (tuple bindings projected))

    (PatternConstant constant)
    (if
     (Datascript_runtime.Query_value.equal_result
      value (value-result constant))
      (Some (tuple bindings projected))
      None)

    (PatternVariable _)
    (if-some [variable (pattern-variable-name element)]
      (if-some [bound (get bindings variable)]
        (if
         (Datascript_runtime.Query_value.equal_result value bound)
          (Some (tuple bindings projected))
          None)
        (Some
         (tuple
          (assoc bindings variable value)
          (conj projected value))))
      None)))

(defn ^:option<array<result>> relation-pattern-row
  [^:vector<datascript.parser/pattern-element> pattern
   ^:array<result> row]
  (if (<= (count pattern) (alength row))
    (loop [remaining pattern
           index 0
           ^:map<string;result> bindings {}
           ^:vector<result> projected []]
      (if-some [element (first remaining)]
        (if-some [value (row-get row index)]
          (if-some [state
                    (relation-pattern-step
                     bindings projected element value)]
            (recur
             (subvec remaining 1)
             (inc index)
             (tuple-get state 0)
             (tuple-get state 1))
            None)
          None)
        (Some (to-array projected))))
    None))

(defn ^:vector<string> relation-pattern-variables
  [^:vector<datascript.parser/pattern-element> pattern]
  (reduce
   (fn [^:vector<string> variables
        ^datascript.parser/pattern-element element]
     (if-some [variable (pattern-variable-name element)]
       (if (some (fn [^:string current] (= current variable))
                 variables)
         variables
         (conj variables variable))
       variables))
   []
   pattern))

(defn ^relation resolve-relation-pattern
  [^:vector<array<result>> rows
   ^relation input-relation
   ^:vector<datascript.parser/pattern-element> pattern]
  (let [variables (relation-pattern-variables pattern)
        matched
        (relation
         (index-attrs variables)
         (reduce
          (fn [^:vector<array<result>> matched
               ^:array<result> row]
            (if-some [projected
                      (relation-pattern-row pattern row)]
              (conj matched projected)
              matched))
          []
          rows)
         (empty-lookup-databases))]
    (if (identity-relation? input-relation)
      matched
      (hash-join input-relation matched))))

(defn ^relation resolve-bound-source-pattern
  [^source source
   ^relation input-relation
   ^relation constants
   ^:vector<datascript.parser/pattern-element> pattern
   ^:string source-name]
  (if-some [source-database (source-database source)]
    (resolve-db-pattern
     source-database input-relation
     (substitute-pattern-constants constants pattern))
    (if-some [rows (source-rows source)]
      (resolve-relation-pattern
       rows input-relation
       (substitute-pattern-constants constants pattern))
      (Stdlib.invalid_arg
       (str "Unsupported query source: " source-name)))))

(defn ^relation resolve-source-pattern
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^:string implicit-source-name
   ^datascript.parser/query-source query-source
   ^relation input-relation
   ^relation constants
   ^:vector<datascript.parser/pattern-element> pattern]
  (if-some [source-name (parser/query-source-name query-source)]
    (if-some [source (get sources source-name)]
      (resolve-bound-source-pattern
       source input-relation constants pattern source-name)
      (Stdlib.invalid_arg
       (str "Query source is not bound: " source-name)))
    (if-some [source (get sources implicit-source-name)]
      (resolve-bound-source-pattern
       source input-relation constants pattern implicit-source-name)
      (resolve-db-pattern
       database input-relation
       (substitute-pattern-constants constants pattern)))))

(defn ^relation resolve-db-patterns
  [^datascript.db/database-view database
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
          (PredicateResult result)
          (Stdlib.invalid_arg
           (str "Predicate variable is not bound: " variable)))))
    (if-some [value (parser/argument-constant argument)]
      (PredicateResult (value-result value))
      (Stdlib.invalid_arg
       "Static predicates do not accept a database source argument"))))

(defn ^result predicate-operand-result
  [^:array<result> row ^predicate-operand operand]
  (match operand
    (PredicateColumn index)
    (if-some [result (row-get row index)]
      result
      (Stdlib.invalid_arg
       "Predicate column is outside the relation row"))
    (PredicateResult result) result))

(defn ^datascript.db/database-view query-source-database
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^:string source-name]
  (if-some [source (get sources source-name)]
    (if-some [source-database (source-database source)]
      source-database
      (Stdlib.invalid_arg
       (str "Query source is not a database: " source-name)))
    (if (= source-name "$")
      database
      (Stdlib.invalid_arg
       (str "Query source is not bound: " source-name)))))

(defn ^result callable-argument-result
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^relation relation
   ^relation constants
   ^:array<result> row
   ^:datascript.parser/fn-arg argument]
  (if-some [variable (parser/argument-variable-name argument)]
    (if-some [value (relation-result relation variable row)]
      value
      (if-some [value (constant-relation-result constants variable)]
        value
        (Stdlib.invalid_arg
         (str "Callable variable is not bound: " variable))))
    (if-some [value (parser/argument-constant argument)]
      (value-result value)
      (if-some [source-name (parser/argument-source-name argument)]
        (database-result
         (query-source-database database sources source-name))
        (Stdlib.invalid_arg "Invalid typed callable argument")))))

(defn ^:vector<result> callable-arguments
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^relation relation
   ^relation constants
   ^:array<result> row
   ^:vector<datascript.parser/fn-arg> arguments]
  (mapv
   (fn [^:datascript.parser/fn-arg argument]
     (callable-argument-result
      database sources relation constants row argument))
   arguments))

(defn ^callable row-callable
  [^relation relation
   ^relation constants
   ^:array<result> row
   ^:string variable]
  (if-some [value (relation-result relation variable row)]
    (if-some [callable (result-callable value)]
      callable
      (Stdlib.invalid_arg
       (str "Query input is not a typed callable: " variable)))
    (if-some [value (constant-relation-result constants variable)]
      (if-some [callable (result-callable value)]
        callable
        (Stdlib.invalid_arg
         (str "Query input is not a typed callable: " variable)))
      (Stdlib.invalid_arg
       (str "Query callable variable is not bound: " variable)))))

(defn ^Datascript_runtime.Data_value.entity_ref query-entity-reference
  [^result result]
  (match result
    (Datascript_runtime.Query_value.Entity entity)
    (Datascript_runtime.Data_value.Entity_id entity)
    (Datascript_runtime.Query_value.Value value)
    (datascript.db/data-value-entity-ref value)
    (Datascript_runtime.Query_value.Metadata value _)
    (datascript.db/data-value-entity-ref value)
    _
    (Stdlib.invalid_arg "Expected a query entity reference")))

(defn ^:keyword query-attribute [^result result]
  (if-some [attribute
            (Datascript_runtime.Data_value.keyword_value
             (result-pattern-value result))]
    (keyword attribute)
    (Stdlib.invalid_arg "Expected a query attribute")))

(defn ^:option<datascript.db/Datom> query-datom
  [^datascript.db/database-view database
   ^result entity-result
   ^result attribute-result]
  (if-some [entity
            (datascript.db/database-view-entid
             database
             (query-entity-reference entity-result))]
    (first
     (datascript.db/database-view-search-vector
      database
      (Some entity)
      (Some (query-attribute attribute-result))
      None
      None))
    None))

(defn ^boolean query-missing?
  [^datascript.db/database-view database
   ^result entity-result
   ^result attribute-result]
  (if-some [entity
            (datascript.db/database-view-entid
             database
             (query-entity-reference entity-result))]
    (let [attribute (query-attribute attribute-result)]
      (if (datascript.db/reverse-ref? attribute)
        (empty?
         (datascript.db/database-view-search-vector
          database
          None
          (Some (datascript.db/reverse-ref attribute))
          (Some (Datascript_runtime.Data_value.Ref entity))
          None))
        (empty?
         (datascript.db/database-view-search-vector
          database (Some entity) (Some attribute) None None))))
    true))

(defn ^datascript.db/database-view query-database-result [^result result]
  (match result
    (Datascript_runtime.Query_value.Database database) database
    _ (Stdlib.invalid_arg "Expected a database query argument")))

(defn ^:option<Datascript_runtime.Data_value.t> database-function-value
  [^datascript.built-ins/query-function function
   ^:vector<result> arguments]
  (if (built-ins/get-else-function? function)
    (if (= 4 (count arguments))
      (let [database (query-database-result (nth arguments 0))
            entity (nth arguments 1)
            attribute (nth arguments 2)
            default-value (result-pattern-value (nth arguments 3))]
        (if (Datascript_runtime.Data_value.is_nil default-value)
          (Stdlib.invalid_arg
           "get-else: nil default value is not supported")
          (if-some [datom (query-datom database entity attribute)]
            (Some (.-v datom))
            (Some default-value))))
      None)
    (if (built-ins/get-some-function? function)
    (if (>= (count arguments) 3)
      (let [database (query-database-result (nth arguments 0))
            entity (nth arguments 1)]
        (loop [attributes (subvec arguments 2)]
          (if-some [attribute (first attributes)]
            (if-some [datom (query-datom database entity attribute)]
              (Some
               (Datascript_runtime.Data_value.vector_of_vector
                [(Datascript_runtime.Data_value.Keyword
                  (str (query-attribute attribute)))
                 (.-v datom)]))
              (recur (subvec attributes 1)))
            (Some (Datascript_runtime.Data_value.Nil)))))
      None)
      None)))

(defn ^relation resolve-database-predicate
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^relation relation
   ^relation constants
   ^:vector<datascript.parser/fn-arg> arguments]
  (relation-with-rows
   relation
   (reduce
    (fn [^:vector<array<result>> rows ^:array<result> row]
      (let [values
            (callable-arguments
             database sources relation constants row arguments)]
        (if (= 3 (count values))
          (let [source (query-database-result (nth values 0))]
            (if (query-missing? source (nth values 1) (nth values 2))
              (conj rows row)
              rows))
          (Stdlib.invalid_arg
           "Invalid arguments for query predicate: missing?"))))
    []
    (relation-rows relation))))

(defn ^binding-value data-value-binding
  [^:Datascript_runtime.Data_value.t value]
  (if-some [items
            (Datascript_runtime.Data_value.sequential_items value)]
    (collection-binding (mapv data-value-binding items))
    (if-some [items
              (Datascript_runtime.Data_value.tuple_items value)]
      (collection-binding
       (mapv
        (fn [^:option<Datascript_runtime.Data_value.t> item]
          (match item
            (Some tuple-value) (data-value-binding tuple-value)
            None (scalar-binding (value-result
                                  (Datascript_runtime.Data_value.Nil)))))
        items))
      (scalar-binding (value-result value)))))

(defn ^binding-value function-binding-result
  [^datascript.parser/binding binding ^result result]
  (if
   (or
    (parser/binding-ignore? binding)
    (some? (parser/binding-scalar-variable binding)))
    (scalar-binding result)
    (if-some [value (result-value result)]
      (data-value-binding value)
      (Stdlib.invalid_arg
       "A callable query function result requires a scalar binding"))))

(defn ^binding-value function-binding-value
  [^datascript.parser/binding binding
   ^:Datascript_runtime.Data_value.t value]
  (function-binding-result binding (value-result value)))

(defn ^:map<string;int> function-result-attrs
  [^relation relation ^datascript.parser/binding binding]
  (reduce
   (fn [^:map<string;int> attrs ^:string variable]
     (if (contains? attrs variable)
       attrs
       (assoc attrs variable (count attrs))))
   (relation-attrs relation)
   (parser/binding-variable-names binding)))

(defn ^relation resolve-database-function
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^relation relation
   ^relation constants
   ^datascript.built-ins/query-function function
   ^:vector<datascript.parser/fn-arg> arguments
   ^datascript.parser/binding binding]
  (let [resolved
        (reduce
         (fn [^:option<relation> output ^:array<result> row]
           (let [values
                 (callable-arguments
                  database sources relation constants row arguments)]
             (if-some [value (database-function-value function values)]
               (if (Datascript_runtime.Data_value.is_nil value)
                 output
                 (let [joined
                       (hash-join
                        (relation-with-rows relation [row])
                        (binding-relation
                         binding
                         (function-binding-value binding value)))]
                   (match output
                     None (Some joined)
                     (Some previous)
                     (Some (sum-relation previous joined)))))
               (Stdlib.invalid_arg
                "Invalid database query function arguments"))))
         None
         (relation-rows relation))]
    (match resolved
      (Some output) output
      None
      (empty-relation
       (function-result-attrs relation binding)
       (relation-lookup-databases relation)))))

(defn ^:string join-query-parts [^:vector<string> parts]
  (if-some [first-part (first parts)]
    (reduce
     (fn [^:string result ^:string part]
       (str result " " part))
     first-part
     (subvec parts 1))
    ""))

(defn ^:string query-argument-description
  [^datascript.parser/fn-arg argument]
  (if-some [variable (parser/argument-variable-name argument)]
    variable
    (if-some [source (parser/argument-source-name argument)]
      source
      (if-some [value (parser/argument-constant argument)]
        (Datascript_runtime.Data_value.to_edn_string value)
        ""))))

(defn ^:string query-call-description
  [^:string name ^:vector<datascript.parser/fn-arg> arguments]
  (let [parts
        (vec
         (cons name (mapv query-argument-description arguments)))]
    (str "(" (join-query-parts parts) ")")))

(defn ^:string rule-argument-description
  [^datascript.parser/pattern-element argument]
  (if-some [variable (pattern-variable-name argument)]
    variable
    (if-some [value (parser/pattern-element-constant argument)]
      (Datascript_runtime.Data_value.to_edn_string value)
      "_")))

(defn ^:string rule-call-description
  [^:string name
   ^:vector<datascript.parser/pattern-element> arguments]
  (str
   "("
   (join-query-parts
    (vec
     (cons name (mapv rule-argument-description arguments))))
   ")"))

(defn ^:string query-binding-description
  [^datascript.parser/binding binding]
  (if (parser/binding-ignore? binding)
    "_"
    (if-some [variable (parser/binding-scalar-variable binding)]
      variable
      (if-some [bindings (parser/binding-tuple-items binding)]
        (str
         "["
         (join-query-parts (mapv query-binding-description bindings))
         "]")
        (if-some [item (parser/binding-collection-item binding)]
          (str "[" (query-binding-description item) " ...]")
          "")))))

(defn ^:vector<string> missing-query-arguments
  [^relation relation
   ^relation constants
   ^:vector<datascript.parser/fn-arg> arguments]
  (reduce
   (fn [^:vector<string> missing
        ^datascript.parser/fn-arg argument]
     (if-some [variable (parser/argument-variable-name argument)]
       (if
        (or
         (contains? (relation-attrs relation) variable)
         (some? (constant-relation-result constants variable))
         (some (fn [^:string name] (= name variable)) missing))
         missing
         (conj missing variable))
       missing))
   []
   arguments))

(defn ^:string query-variable-set-description
  [^:vector<string> variables]
  (str "#{" (join-query-parts variables) "}"))

(defn validate-static-call-bindings
  [^relation relation
   ^relation constants
   ^:string name
   ^:vector<datascript.parser/fn-arg> arguments
   ^:option<datascript.parser/binding> binding]
  (let [missing (missing-query-arguments relation constants arguments)]
    (if (empty? missing)
      nil
      (Stdlib.invalid_arg
       (str
        "Insufficient bindings: "
        (query-variable-set-description missing)
        " not bound in ["
        (query-call-description name arguments)
        (match binding
          None "]"
          (Some binding)
          (str " " (query-binding-description binding) "]")))))))

(defn- ^:bool data-value-truthy?
  [^:Datascript_runtime.Data_value.t value]
  (if (Datascript_runtime.Data_value.is_nil value)
    false
    (match (Datascript_runtime.Data_value.bool_value value)
      (Some boolean-value) boolean-value
      None true)))

(defn ^:bool result-nil? [^result result]
  (if-some [value (result-value result)]
    (Datascript_runtime.Data_value.is_nil value)
    false))

(defn ^result complement-result [^:vector<result> arguments]
  (let [target (first arguments)]
    (callable-result
     (callable
      (fn [^:vector<result> invocation-arguments]
        (if-some [target-result target]
          (if-some [target-callable
                    (result-callable target-result)]
            (let [value
                  (invoke-callable
                   target-callable invocation-arguments)]
              (Some
               (Datascript_runtime.Data_value.Bool
                (match value
                  None true
                  (Some value)
                  (not (data-value-truthy? value))))))
            (if-some [value (result-value target-result)]
              (if (Datascript_runtime.Data_value.is_nil value)
                (Stdlib.invalid_arg
                 "Cannot read properties of null (reading 'cljs$core$IFn$_invoke$arity$1')")
                (Stdlib.invalid_arg "f.call is not a function"))
              (Stdlib.invalid_arg "f.call is not a function")))
          (Stdlib.invalid_arg
           "Cannot read properties of undefined (reading 'cljs$core$IFn$_invoke$arity$1')")))))))

(defn ^result metadata-function-result [^:vector<result> arguments]
  (if-some [argument (first arguments)]
    (if-some [metadata (result-metadata argument)]
      (value-result metadata)
      (value-result (Datascript_runtime.Data_value.Nil)))
    (value-result (Datascript_runtime.Data_value.Nil))))

(defn ^result value-type-function-result [^:vector<result> arguments]
  (if-some [argument (first arguments)]
    (let [value
          (match argument
            (Datascript_runtime.Query_value.Database database)
            (Datascript_runtime.Data_value.database_runtime_type_value
             (match database
               (db/DatabaseView _) false
               (db/FilteredDatabaseView _) true))
            (Datascript_runtime.Query_value.Callable _)
            (Datascript_runtime.Data_value.function_runtime_type_value)
            _
            (Datascript_runtime.Data_value.runtime_type_value
             (result-pattern-value argument)))]
      (value-result value))
    (value-result (Datascript_runtime.Data_value.Nil))))

(defn- static-function-built-in
  [function]
  (match function
    (ComparisonStaticPredicate comparison) comparison
    (PureStaticPredicate pure) pure))

(defn- static-function
  [name]
  (if-some [pure (built-ins/pure-function name)]
    (Some (PureStaticPredicate pure))
    (if-some [comparison (built-ins/comparison-function name)]
      (Some (ComparisonStaticPredicate comparison))
      None)))

(defn- apply-static-function
  [function values]
  (match function
    (ComparisonStaticPredicate comparison)
    (if-some [matches?
              (built-ins/apply-comparison comparison values)]
      (Some (Datascript_runtime.Data_value.Bool matches?))
      None)
    (PureStaticPredicate pure)
    (built-ins/apply-pure-function pure values)))

(defn- ^:option<result> static-function-result
  [function ^:vector<result> results]
  (match function
    (ComparisonStaticPredicate comparison)
    (if-some [matches?
              (built-ins/apply-comparison
               comparison
               (mapv result-pattern-value results))]
      (Some
       (value-result
        (Datascript_runtime.Data_value.Bool matches?)))
      None)
    (PureStaticPredicate pure)
    (if (built-ins/complement-function? pure)
      (Some (complement-result results))
      (if (built-ins/metadata-function? pure)
        (Some (metadata-function-result results))
        (if (built-ins/value-type-function? pure)
          (Some (value-type-function-result results))
          (let [values (mapv result-pattern-value results)]
            (if (built-ins/differ-function? pure)
              (Some
               (value-result
                (Datascript_runtime.Data_value.Bool
                 (built-ins/apply-differ values))))
              (if-some [value (apply-static-function function values)]
                (Some (value-result value))
                None))))))))

(defn- ^:bool query-result-truthy? [^result result]
  (if-some [value (result-value result)]
    (data-value-truthy? value)
    true))

(defn- ^:bool differ-predicate-matches?
  [^:array<result> row
   ^:vector<predicate-operand> operands]
  (let [operand-count (count operands)
        middle (quot operand-count 2)]
    (if (= operand-count (* middle 2))
      (loop [index 0]
        (if (< index middle)
          (let [left
                (predicate-operand-result
                 row (nth operands index))
                right
                (predicate-operand-result
                 row (nth operands (+ middle index)))]
            (if
             (Datascript_runtime.Data_value.equal
              (result-pattern-value left)
              (result-pattern-value right))
              (recur (inc index))
              true))
          false))
      true)))

(defn ^relation resolve-predicate
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^relation relation
   ^relation constants
   ^:datascript.parser/query-callable callable
  ^:vector<datascript.parser/fn-arg> arguments]
  (if-some [name (parser/static-callable-name callable)]
    (let [_ (validate-static-call-bindings
             relation constants name arguments None)]
    (if-some [function (static-function name)]
      (if
       (match function
         (ComparisonStaticPredicate comparison)
         (built-ins/missing-function? comparison)
         (PureStaticPredicate _) false)
        (resolve-database-predicate
         database sources relation constants arguments)
        (let [operands
              (mapv
               (fn [^:datascript.parser/fn-arg argument]
                 (compile-predicate-argument
                  relation constants argument))
               arguments)
              differ?
              (match function
                (PureStaticPredicate pure)
                (built-ins/differ-function? pure)
                (ComparisonStaticPredicate _) false)]
          (relation-with-rows
           relation
           (reduce
            (fn [^:vector<array<result>> rows ^:array<result> row]
              (let [matches?
                    (if differ?
                      (Some
                       (differ-predicate-matches?
                        row operands))
                      (let [results
                            (mapv
                             (fn [^predicate-operand operand]
                               (predicate-operand-result
                                row operand))
                             operands)
                            invocation
                            (static-function-result
                             function results)]
                        (match invocation
                          (Some result)
                          (Some
                           (query-result-truthy? result))
                          None None)))]
                (if-some [matches? matches?]
                  (if matches?
                    (conj rows row)
                    rows)
                  (Stdlib.invalid_arg
                   (str
                    "Unsupported static query predicate: "
                    name)))))
            []
            (relation-rows relation)))))
      (Stdlib.invalid_arg
       (str
        "Unknown predicate '"
        name
        " in ["
        (query-call-description name arguments)
        "]"))))
    (if-some [variable (parser/variable-callable-name callable)]
      (relation-with-rows
       relation
       (reduce
        (fn [^:vector<array<result>> rows ^:array<result> row]
          (let [callable (row-callable relation constants row variable)
                arguments
                (callable-arguments
                 database sources relation constants row arguments)]
            (if-some [value (invoke-callable callable arguments)]
              (if
               (or
                (Datascript_runtime.Data_value.is_nil value)
                (= (Datascript_runtime.Data_value.bool_value value)
                   (Some false)))
                rows
                (conj rows row))
              rows)))
        []
        (relation-rows relation)))
      (Stdlib.invalid_arg
       "Variable query predicate name is missing"))))

(defn ^relation resolve-function
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^relation relation
   ^relation constants
   ^:datascript.parser/query-callable callable
   ^:vector<datascript.parser/fn-arg> arguments
   ^datascript.parser/binding binding]
  (if-some [name (parser/static-callable-name callable)]
    (let [_ (validate-static-call-bindings
             relation constants name arguments (Some binding))]
    (if-some [function (static-function name)]
      (if
       (match function
         (PureStaticPredicate pure)
         (or
          (built-ins/get-else-function? pure)
          (built-ins/get-some-function? pure))
         (ComparisonStaticPredicate _) false)
        (resolve-database-function
         database sources relation constants
         (static-function-built-in function)
         arguments binding)
        (let [operands
              (mapv
               (fn [^:datascript.parser/fn-arg argument]
                 (compile-predicate-argument
                  relation constants argument))
               arguments)
              resolved
              (reduce
               (fn [^:option<relation> output ^:array<result> row]
                 (let [results
                       (mapv
                        (fn [^predicate-operand operand]
                          (predicate-operand-result row operand))
                        operands)
                       invocation
                       (static-function-result function results)]
                   (if-some [result invocation]
                     (if (result-nil? result)
                       output
                       (let [joined
                             (hash-join
                              (relation-with-rows relation [row])
                             (binding-relation
                               binding
                               (function-binding-result
                                binding result)))]
                         (match output
                           None (Some joined)
                           (Some previous)
                           (Some (sum-relation previous joined)))))
                     (Stdlib.invalid_arg
                      (str "Invalid arguments for query function: "
                           name)))))
               None
               (relation-rows relation))]
          (match resolved
            (Some output) output
            None
            (empty-relation
             (function-result-attrs relation binding)
             (relation-lookup-databases relation)))))
      (Stdlib.invalid_arg
       (str
        "Unknown function '"
        name
        " in ["
        (query-call-description name arguments)
        " "
        (query-binding-description binding)
        "]"))))
    (if-some [variable (parser/variable-callable-name callable)]
      (let [resolved
            (reduce
             (fn [^:option<relation> output ^:array<result> row]
               (let [callable
                     (row-callable relation constants row variable)
                     arguments
                     (callable-arguments
                      database sources relation constants row arguments)]
                 (if-some [value (invoke-callable callable arguments)]
                   (if (Datascript_runtime.Data_value.is_nil value)
                     output
                     (let [joined
                           (hash-join
                            (relation-with-rows relation [row])
                            (binding-relation
                             binding
                             (function-binding-value binding value)))]
                       (match output
                         None (Some joined)
                         (Some previous)
                         (Some (sum-relation previous joined)))))
                   output)))
             None
             (relation-rows relation))]
        (match resolved
          (Some output) output
          None
          (empty-relation
           (function-result-attrs relation binding)
           (relation-lookup-databases relation))))
      (Stdlib.invalid_arg
       "Variable query function name is missing"))))

(declare resolve-static-clauses ensure-empty-relation-variables)

(defn ^boolean rows-match-on-variables?
  [^relation left
   ^:array<result> left-row
   ^relation right
   ^:array<result> right-row
   ^:vector<string> variables]
  (every?
   (fn [^:string variable]
     (if-some [left-value
               (relation-result left variable left-row)]
       (if-some [right-value
                 (relation-result right variable right-row)]
         (Datascript_runtime.Query_value.equal_result
          left-value right-value)
         false)
       false))
   variables))

(defn ^relation project-relation-variables
  [^relation source-relation ^:vector<string> variables]
  (let [attrs (relation-attrs source-relation)
        indexes
        (mapv
         (fn [^:string variable]
           (if-some [index (get attrs variable)]
             index
             (Stdlib.invalid_arg
              (str "Cannot project unbound query variable: "
                   variable))))
         variables)
        lookup-databases
        (reduce
         (fn [^:map<string;datascript.db/database-view> databases
              ^:string variable]
           (if-some [database
                     (get
                      (relation-lookup-databases source-relation)
                      variable)]
             (assoc databases variable database)
             databases))
         {}
         variables)
        index-array (to-array indexes)]
    (relation
     (index-attrs variables)
     (mapv
      (fn [^:array<result> row]
        (project-row row index-array))
      (relation-rows source-relation))
     lookup-databases)))

(defn ^:string variable-set-display
  [^:vector<datascript.parser/Variable> variables]
  (str
   "#{"
   (loop [remaining variables
          rendered ""]
     (if-some [variable (first remaining)]
       (recur
        (subvec remaining 1)
        (str
         rendered
         (if (= rendered "") "" " ")
         (.-symbol variable)))
       rendered))
   "}"))

(defn ^relation resolve-not
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^:string implicit-source-name
   ^relation relation
   ^relation constants
   ^rules rules
   ^rule-path rule-path
   ^:vector<datascript.parser/Variable> variables
   ^:vector<datascript.parser/clause> clauses
   ^:string display]
  (let [bound-variables
        (vec
         (filter
          (fn [^datascript.parser/Variable variable]
            (contains?
             (relation-attrs relation)
             (str (.-symbol variable))))
          variables))]
    (if (empty? bound-variables)
      (Stdlib.invalid_arg
       (str
        "Insufficient bindings: none of "
        (variable-set-display variables)
        " is bound in "
        display))
      (let [bound-names
            (mapv
             (fn [^datascript.parser/Variable variable]
               (str (.-symbol variable)))
             bound-variables)
            joined
            (project-relation-variables relation bound-names)
            matched
            (resolve-static-clauses
             database sources implicit-source-name
             joined constants rules rule-path clauses)]
        (relation-with-rows
         relation
         (filterv
          (fn [^:array<result> row]
            (not
             (some
              (fn [^:array<result> matched-row]
                (rows-match-on-variables?
                 relation row matched matched-row bound-names))
              (relation-rows matched))))
          (relation-rows relation)))))))

(defn ^relation resolve-or-branch
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^:string implicit-source-name
   ^relation relation
   ^relation constants
   ^rules rules
   ^rule-path rule-path
   ^datascript.parser/clause branch]
  (if-some [clauses (parser/and-clause-clauses branch)]
    (resolve-static-clauses
     database sources implicit-source-name
     relation constants rules rule-path clauses)
    (resolve-static-clauses
     database sources implicit-source-name
     relation constants rules rule-path [branch])))

(defn ^boolean query-variable-bound?
  [^relation relation ^relation constants ^:string variable]
  (or
   (contains? (relation-attrs relation) variable)
   (contains? (relation-attrs constants) variable)))

(defn ^:vector<string> clause-free-variable-names
  [^relation relation
   ^relation constants
   ^datascript.parser/clause clause]
  (vec
   (distinct
    (filter
     (fn [^:string variable]
       (not (query-variable-bound? relation constants variable)))
     (mapv
      (fn [^datascript.parser/Variable variable]
        (str (.-symbol variable)))
      (parser/clause-vars clause))))))

(defn ^:vector<vector<string>> branch-free-variable-names
  [^relation relation
   ^relation constants
   ^:vector<datascript.parser/clause> branches]
  (mapv
   (fn [^datascript.parser/clause branch]
     (clause-free-variable-names relation constants branch))
   branches))

(defn ^:string variable-sets-description
  [^:vector<vector<string>> variable-sets]
  (str
   "["
   (join-query-parts
    (mapv query-variable-set-description variable-sets))
   "]"))

(defn ^relation resolve-or
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^:string implicit-source-name
   ^relation relation
   ^relation constants
   ^rules rules
   ^rule-path rule-path
   ^:vector<string> required-variable-names
   ^:vector<string> branch-variable-names
   ^:vector<datascript.parser/clause> branches
   ^boolean join?
   ^:string display]
  (let [missing-required
        (filterv
         (fn [^:string variable]
           (not (query-variable-bound?
                 relation constants variable)))
         required-variable-names)
        free-variable-names
        (branch-free-variable-names relation constants branches)]
    (if (not (empty? missing-required))
      (Stdlib.invalid_arg
       (str
        "Insufficient bindings: "
        (query-variable-set-description missing-required)
        " not bound in "
        display))
      (if
       (and
        (not join?)
        (if-some [expected (first free-variable-names)]
          (not
           (every?
            (fn [^:vector<string> variables]
              (= (set expected) (set variables)))
            (subvec free-variable-names 1)))
          false))
        (Stdlib.invalid_arg
         (str
          "All clauses in 'or' must use same set of free vars, had "
          (variable-sets-description free-variable-names)
          " in "
          display))
        (let [resolved-variable-names
              (filterv
               (fn [^:string variable]
                 (not
                  (contains?
                   (relation-attrs constants)
                   variable)))
               branch-variable-names)
              bound-join-variable-names
              (filterv
               (fn [^:string variable]
                 (contains? (relation-attrs relation) variable))
               resolved-variable-names)
              branch-input
              (if (empty? bound-join-variable-names)
                (identity-relation)
                (let [projected
                      (project-relation-variables
                       relation bound-join-variable-names)]
                  (relation-with-rows
                   projected
                   (distinct-rows (relation-rows projected)))))
              resolved
              (mapv
               (fn [^datascript.parser/clause branch]
                 (project-relation-variables
                  (ensure-empty-relation-variables
                   (resolve-or-branch
                    database sources implicit-source-name
                    branch-input constants rules rule-path branch)
                   resolved-variable-names)
                  resolved-variable-names))
               branches)]
          (if-some [first-branch (first resolved)]
            (let [union
                  (reduce
                   sum-relation first-branch (subvec resolved 1))
                  union
                  (relation-with-rows
                   union
                   (distinct-rows (relation-rows union)))]
              (hash-join relation union))
            (Stdlib.invalid_arg
             "Cannot resolve an empty or clause")))))))

(defn ^boolean rule-argument-bound?
  [^relation relation
   ^relation constants
   ^datascript.parser/pattern-element argument]
  (if-some [variable (pattern-variable-name argument)]
    (if (contains? (relation-attrs relation) variable)
      true
      (some? (constant-relation-result constants variable)))
    (some? (parser/pattern-element-constant argument))))

(defn ^result rule-argument-result
  [^relation relation
   ^relation constants
   ^:array<result> row
   ^datascript.parser/pattern-element argument]
  (if-some [variable (pattern-variable-name argument)]
    (if-some [value (relation-result relation variable row)]
      value
      (if-some [value
                (constant-relation-result constants variable)]
        value
        (Stdlib.invalid_arg
         (str "Rule argument variable is not bound: " variable))))
    (if-some [value (parser/pattern-element-constant argument)]
      (value-result value)
      (Stdlib.invalid_arg
       "Rule arguments cannot contain placeholders"))))

(defn ^relation rule-branch-input
  [^relation outer-relation
   ^relation constants
   ^:vector<string> parameters
   ^:vector<datascript.parser/pattern-element> arguments]
  (let [bound-indexes
        (filterv
         (fn [^:int index]
           (rule-argument-bound?
            outer-relation constants (nth arguments index)))
         (range (count arguments)))
        bound-parameters
        (mapv
         (fn [^:int index] (nth parameters index))
         bound-indexes)]
    (if (empty? (relation-rows outer-relation))
      (empty-relation
       (index-attrs bound-parameters)
       (empty-lookup-databases))
      (if (empty? bound-indexes)
        (identity-relation)
        (let [input
              (relation
               (index-attrs bound-parameters)
               (mapv
                (fn [^:array<result> row]
                  (to-array
                   (mapv
                    (fn [^:int index]
                      (rule-argument-result
                       outer-relation
                       constants
                       row
                       (nth arguments index)))
                    bound-indexes)))
                (relation-rows outer-relation))
               (empty-lookup-databases))]
          (relation-with-rows
           input
           (distinct-rows (relation-rows input))))))))

(defn ^relation rule-branch-output
  [^relation outer-relation
   ^relation branch-result
   ^:vector<string> parameters
   ^:vector<datascript.parser/pattern-element> arguments]
  (let [variable-indexes
        (filterv
         (fn [^:int index]
           (some? (pattern-variable-name (nth arguments index))))
         (range (count arguments)))
        output-variables
        (mapv
         (fn [^:int index]
           (if-some [variable
                     (pattern-variable-name
                      (nth arguments index))]
             variable
             (Stdlib.invalid_arg
              "Rule output argument is not a variable")))
         variable-indexes)
        output-parameters
        (mapv
         (fn [^:int index] (nth parameters index))
         variable-indexes)
        projected
        (project-relation-variables
         branch-result output-parameters)
        renamed
        (relation
         (index-attrs output-variables)
         (relation-rows projected)
         (empty-lookup-databases))]
    (hash-join outer-relation renamed)))

(defn ^relation ensure-empty-relation-variables
  [^relation relation ^:vector<string> variables]
  (if (empty? (relation-rows relation))
    (empty-relation
     (reduce
      (fn [^:map<string;int> attrs ^:string variable]
        (if (contains? attrs variable)
          attrs
          (assoc attrs variable (count attrs))))
      (relation-attrs relation)
      variables)
     (relation-lookup-databases relation))
    relation))

(defn ^:vector<result> distinct-results
  [^:vector<result> values]
  (reduce
   (fn [^:vector<result> distinct ^result value]
     (if
      (some
       (fn [^result candidate]
         (Datascript_runtime.Query_value.equal_result
          candidate value))
       distinct)
       distinct
       (conj distinct value)))
   []
   values))

(defn ^rule-call-argument rule-call-argument
  [^relation relation
   ^relation constants
   ^datascript.parser/pattern-element argument]
  (if-some [variable (pattern-variable-name argument)]
    (RuleCallVariable
     variable
     (if (contains? (relation-attrs relation) variable)
       (distinct-results
        (mapv
         (fn [^:array<result> row]
           (if-some [value
                     (relation-result relation variable row)]
             value
             (Stdlib.invalid_arg
              "Bound rule argument has no row value")))
         (relation-rows relation)))
       (if-some [value
                 (constant-relation-result constants variable)]
         [value]
         [])))
    (if-some [value (parser/pattern-element-constant argument)]
      (RuleCallConstant value)
      (Stdlib.invalid_arg
       "Rule call arguments cannot contain placeholders"))))

(defn ^rule-call make-rule-call
  [^:string rule-name
   ^relation relation
   ^relation constants
   ^:vector<datascript.parser/pattern-element> arguments]
  (RuleCall
   rule-name
   (mapv
    (fn [^datascript.parser/pattern-element argument]
      (rule-call-argument relation constants argument))
    arguments)))

(defn ^boolean result-vectors-equal?
  [^:vector<result> left ^:vector<result> right]
  (if (= (count left) (count right))
    (every?
     (fn [^:int index]
       (Datascript_runtime.Query_value.equal_result
        (nth left index)
        (nth right index)))
     (range (count left)))
    false))

(defn ^boolean rule-call-arguments-equal?
  [^rule-call-argument left ^rule-call-argument right]
  (match left
    (RuleCallVariable left-name left-values)
    (match right
      (RuleCallVariable right-name right-values)
      (and
       (= left-name right-name)
       (result-vectors-equal? left-values right-values))
      _ false)
    (RuleCallConstant left-value)
    (match right
      (RuleCallConstant right-value)
      (Datascript_runtime.Data_value.equal
       left-value right-value)
      _ false)))

(defn ^boolean rule-calls-equal?
  [^rule-call left ^rule-call right]
  (match left
    (RuleCall left-name left-arguments)
    (match right
      (RuleCall right-name right-arguments)
      (and
       (= left-name right-name)
       (= (count left-arguments)
          (count right-arguments))
       (every?
        (fn [^:int index]
          (rule-call-arguments-equal?
           (nth left-arguments index)
           (nth right-arguments index)))
        (range (count left-arguments)))))))

(defn ^relation empty-rule-call-relation
  [^relation relation
   ^:vector<datascript.parser/pattern-element> arguments]
  (let [attrs
        (reduce
         (fn [^:map<string;int> attrs
              ^datascript.parser/pattern-element argument]
           (if-some [variable (pattern-variable-name argument)]
             (if (contains? attrs variable)
               attrs
               (assoc attrs variable (count attrs)))
             attrs))
         (relation-attrs relation)
         arguments)]
    (empty-relation
     attrs
     (relation-lookup-databases relation))))

(defn ^relation resolve-rule-branch
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^:string implicit-source-name
   ^relation relation
   ^relation constants
   ^rules rules
   ^rule-path rule-path
   ^:vector<datascript.parser/pattern-element> arguments
   ^datascript.parser/RuleBranch branch]
  (let [parameters
        (parser/rule-branch-parameter-names branch)]
    (if (= (count parameters) (count arguments))
      (rule-branch-output
       relation
       (ensure-empty-relation-variables
        (resolve-static-clauses
         database
         sources
         implicit-source-name
         (rule-branch-input
          relation constants parameters arguments)
         (identity-relation)
         rules
         rule-path
         (parser/rule-branch-clauses branch))
        parameters)
       parameters
       arguments)
      (Stdlib.invalid_arg "Rule arity mismatch"))))

(defn ^relation resolve-rule
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^:string implicit-source-name
   ^relation relation
   ^relation constants
   ^rules rules
   ^rule-path rule-path
   ^:string rule-name
   ^:vector<datascript.parser/pattern-element> arguments]
  (if-some [branches (parser/rule-branches rules rule-name)]
    (let [required-count
          (if-some [first-branch (first branches)]
            (count
             (parser/rule-branch-required-parameter-names
              first-branch))
            0)
          required-bound?
          (and
           (<= required-count (count arguments))
           (every?
            (fn [^:int index]
              (rule-argument-bound?
               relation constants (nth arguments index)))
            (range required-count)))
          call
          (make-rule-call
           rule-name relation constants arguments)]
      (if (not required-bound?)
        (Stdlib.invalid_arg
         "Insufficient bindings for required rule arguments")
        (if
         (some
          (fn [^rule-call previous]
            (rule-calls-equal? call previous))
          rule-path)
          (empty-rule-call-relation relation arguments)
          (if-some [first-branch (first branches)]
            (let [next-path (conj rule-path call)
                  first-result
                  (resolve-rule-branch
                   database sources implicit-source-name
                   relation constants rules next-path
                   arguments first-branch)
                  result
                  (reduce
                   (fn [^relation result
                        ^datascript.parser/RuleBranch branch]
                     (sum-relation
                      result
                      (resolve-rule-branch
                       database sources implicit-source-name
                       relation constants rules next-path
                       arguments branch)))
                   first-result
                   (subvec branches 1))]
              (relation-with-rows
               result
               (distinct-rows (relation-rows result))))
            (Stdlib.invalid_arg "Rule must contain a branch")))))
    (Stdlib.invalid_arg
     (str
      "Unknown rule '"
      rule-name
      " in "
      (rule-call-description rule-name arguments)))))

(defn ^relation resolve-static-clauses
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^:string implicit-source-name
   ^relation initial-relation
   ^relation constants
   ^rules rules
   ^rule-path rule-path
   ^:vector<datascript.parser/clause> clauses]
  (reduce
   (fn [^relation relation ^:datascript.parser/clause clause]
     (if (empty? (relation-rows relation))
       relation
       (if-some [rule-parts (parser/rule-clause-parts clause)]
         (let [rule-source-name
               (if-some [source-name
                         (parser/rule-clause-source-name clause)]
                 source-name
                 implicit-source-name)]
           (resolve-rule
            database sources rule-source-name
            relation constants rules rule-path
            (tuple-get rule-parts 0)
            (tuple-get rule-parts 1)))
         (if-some [or-parts (parser/or-clause-parts clause)]
           (let [or-source-name
                 (if-some [source-name
                           (parser/or-clause-source-name clause)]
                   source-name
                   implicit-source-name)
                 display
                 (if-some [value (parser/or-clause-display clause)]
                   value
                   "")]
             (resolve-or
              database
              sources
              or-source-name
              relation
              constants
              rules
              rule-path
              (tuple-get or-parts 0)
              (tuple-get or-parts 1)
              (tuple-get or-parts 2)
              (parser/or-clause-join? clause)
              display))
           (match clause
             (PatternClause query-source pattern)
             (resolve-source-pattern
              database sources implicit-source-name
              query-source relation constants pattern)
             (PredicateClause callable arguments)
             (resolve-predicate
              database sources relation constants callable arguments)
             (FunctionClause callable arguments binding)
             (resolve-function
              database sources relation constants callable arguments binding)
             (NotClause query-source variables clauses display)
             (let [not-source-name
                   (if-some [source-name
                             (parser/query-source-name query-source)]
                     source-name
                     implicit-source-name)]
               (resolve-not
                database sources not-source-name
                relation constants rules rule-path
                variables clauses display))
             _
             (Stdlib.invalid_arg
              "Static query contains an unsupported clause"))))))
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
  [^datascript.db/database-view database
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

(defn ^:map<string;result> return-map-row
  [^:vector<string> keys ^:array<result> row]
  (if (= (count keys) (alength row))
    (loop [remaining keys
           index 0
           ^:map<string;result> result-map {}]
      (if-some [key (first remaining)]
        (if-some [value (row-get row index)]
          (recur
           (subvec remaining 1)
           (inc index)
           (assoc result-map key value))
          (Stdlib.invalid_arg
           "Return-map row is shorter than its key list"))
        result-map))
    (Stdlib.invalid_arg
     "Return-map key count must match result row arity")))

(defn ^:vector<map<string;result>> return-map-rows
  [^:vector<string> keys ^:vector<array<result>> rows]
  (Datascript_runtime.Query_value.distinct_result_maps
   (mapv
    (fn [^:array<result> row]
      (return-map-row keys row))
    rows)))

(defn ^output mapped-find-output
  [^datascript.parser/find-spec find
   ^datascript.parser/return-map return-map
   ^:vector<array<result>> rows]
  (let [relation? (parser/relation-find? find)]
    (if-some [keys (parser/return-map-key-names return-map)]
      (let [mapped (return-map-rows keys rows)]
        (if relation?
          (Datascript_runtime.Query_value.Keyword_relation_output mapped)
          (Datascript_runtime.Query_value.Keyword_tuple_output
           (first mapped))))
      (if-some [keys (parser/return-map-symbol-names return-map)]
        (let [mapped (return-map-rows keys rows)]
          (if relation?
            (Datascript_runtime.Query_value.Symbol_relation_output mapped)
            (Datascript_runtime.Query_value.Symbol_tuple_output
             (first mapped))))
        (if-some [keys (parser/return-map-string-names return-map)]
          (let [mapped (return-map-rows keys rows)]
            (if relation?
              (Datascript_runtime.Query_value.String_relation_output mapped)
              (Datascript_runtime.Query_value.String_tuple_output
               (first mapped))))
          (Stdlib.invalid_arg "Unsupported query return-map type"))))))

(defn ^output find-output
  [^datascript.parser/find-spec find
   ^:option<datascript.parser/return-map> return-map
   ^:vector<array<result>> rows]
  (if-some [return-map return-map]
    (mapped-find-output find return-map rows)
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
       (first rows)))))

(defn ^output execute-resolved-query
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^datascript.parser/Query query
   ^relation constants
   ^relation resolved-relation]
  (if-some [variables
            (parser/find-projection-variable-names
             (.-qfind query))]
    (let [find (.-qfind query)
          elements (parser/find-spec-elements find)
          with-variables
          (query-with-variable-names query)
          all-variables
          (vec (concat variables with-variables))
          pull-pattern-variables
          (pull-pattern-variable-names elements)
          used-variables
          (vec
           (concat all-variables pull-pattern-variables))
          relation
          (ensure-empty-relation-variables
           resolved-relation used-variables)
          pull-patterns
          (resolve-pull-patterns elements relation)
          collected-rows
          (distinct-rows
           (project-find-rows relation all-variables))
          projected-rows
          (if (empty? with-variables)
            collected-rows
            (let [indexes
                  (to-array (range (count variables)))]
              (mapv
               (fn [^:array<result> row]
                 (project-row row indexes))
               collected-rows)))
          aggregated-rows
          (if (some parser/aggregate? elements)
            (aggregate-rows
             elements constants relation projected-rows)
            projected-rows)
          rows
          (if (some parser/pull? elements)
            (pull-rows
             database sources elements pull-patterns aggregated-rows)
            aggregated-rows)]
      (find-output find (.-qreturn-map query) rows))
    (Stdlib.invalid_arg
     "Static query find supports variables only")))

(defn ^output execute-db-query-with-rules
  [^datascript.db/database-view database
   ^:map<string;source> sources
   ^datascript.parser/Query query
   ^relation input-relation
   ^rules rules]
  (let [variables
        (if-some [variables
                  (parser/find-projection-variable-names
                   (.-qfind query))]
          variables
          [])
        elements (parser/find-spec-elements (.-qfind query))
        with-variables (query-with-variable-names query)
        pull-pattern-variables
        (pull-pattern-variable-names elements)
        used-variables
        (vec
         (concat
          variables
          with-variables
          pull-pattern-variables))
        elide-input?
        (and
         (single-row-constant-relation? input-relation)
         (not
          (relation-variables-used?
           input-relation used-variables)))
        initial-relation
        (if elide-input?
          (identity-relation)
          input-relation)
        constants
        (if elide-input?
          input-relation
          (identity-relation))
        resolved-relation
        (resolve-static-clauses
         database sources "$" initial-relation constants rules []
         (.-qwhere query))]
    (execute-resolved-query
     database sources query constants resolved-relation)))

(defn ^output execute-db-query-with-relation
  [^datascript.db/database-view database
   ^datascript.parser/Query query
   ^relation input-relation]
  (execute-db-query-with-rules
   database
   {"$" (database-source database)}
   query input-relation []))

(defn ^output execute-db-query
  [^datascript.db/database-view database ^datascript.parser/Query query]
  (execute-db-query-with-relation
   database query (identity-relation)))

(defn ^datascript.db/database-view require-database-input [^input input]
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
  (if (>= (count values) (count bindings))
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
     "Tuple query input has too few values")))

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

(defn ^context bind-static-query-inputs
  [^:vector<datascript.parser/static-query-input> descriptors
   ^:vector<input> inputs]
  (loop [remaining-descriptors descriptors
         remaining-inputs inputs
         relation (identity-relation)
         ^:map<string;source> sources {}
         ^rules rules []]
    (if-some [descriptor (first remaining-descriptors)]
      (if-some [input (first remaining-inputs)]
        (if (parser/static-input-rules? descriptor)
          (if-some [input-rules (input-rules input)]
            (recur
             (subvec remaining-descriptors 1)
             (subvec remaining-inputs 1)
             relation
             sources
             input-rules)
            (Stdlib.invalid_arg
             "Rules query input requires a Rules_input"))
          (if-some [source-name
                    (parser/static-input-source-name descriptor)]
            (if-some [source (input-source input)]
              (recur
               (subvec remaining-descriptors 1)
               (subvec remaining-inputs 1)
               relation
               (assoc sources source-name source)
               rules)
              (Stdlib.invalid_arg
               "Source query input requires a Source_input"))
            (if-some [binding
                      (parser/static-input-binding descriptor)]
              (let [bound
                    (binding-relation
                     binding
                     (require-binding-input input))]
                (recur
                 (subvec remaining-descriptors 1)
                 (subvec remaining-inputs 1)
                 (if (identity-relation? relation)
                   bound
                   (hash-join relation bound))
                 sources
                 rules))
              (Stdlib.invalid_arg
               "Unsupported static query input descriptor"))))
        (Stdlib.invalid_arg "Missing static query input"))
      (context [relation] sources rules))))

(defn ^datascript.db/database-view query-default-database
  [^:map<string;source> sources]
  (if-some [source (get sources "$")]
    (if-some [database (source-database source)]
      database
      (datascript.db/database-view
       (datascript.db/empty-db
        None
        (datascript.db/default-options))))
    (reduce-kv
     (fn [^datascript.db/database-view database
          ^:string _
          ^source source]
       (if-some [source-database (source-database source)]
         source-database
         database))
     (datascript.db/database-view
      (datascript.db/empty-db
       None
       (datascript.db/default-options)))
     sources)))

(defn ^output execute-query
  [^datascript.parser/Query query ^:vector<input> inputs]
  (parser/validate-static-query-sources query)
  (if-some [descriptors (parser/static-query-inputs query)]
    (let [expected-input-count (count descriptors)]
      (if (= (count inputs) expected-input-count)
        (let [bound
              (bind-static-query-inputs descriptors inputs)
              relations (context-relations bound)
              input-relation
              (if-some [relation (first relations)]
                relation
                (identity-relation))
              sources (context-sources bound)
              database (query-default-database sources)]
          (execute-db-query-with-rules
           database
           sources
           query
           input-relation
           (context-rules bound)))
        (Stdlib.invalid_arg
         "Static query input count does not match parsed :in")))
    (Stdlib.invalid_arg
     "Static query contains unsupported input bindings")))

(defn ^:option<datascript.db/database-view> relation-lookup-database
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
