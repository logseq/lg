(ns datascript.lg.query-types
  (:require
   [datascript.built-ins :as built-ins]
   [datascript.db :as db]
   [datascript.parser :as parser]
   [datascript.pull-api :as pull-api]
   [datascript.pull-parser :as pull-parser]))

(declare
 empty-relation
 binding-relation
 query-source-database)

(defn empty-lookup-databases []
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

(defn database-view-source [database]
  (Datascript_runtime.Query_value.database_source database))

(defn database-source [database]
  (database-view-source (db/database-view database)))

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

(defn pull-source-items [pattern]
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

(defn query-entity-ref [value]
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

(defn result-entity-ref [result]
  (match result
    (Datascript_runtime.Query_value.Entity entity)
    (Some (Datascript_runtime.Data_value.Entity_id entity))
    (Datascript_runtime.Query_value.Value value)
    (query-entity-ref value)
    (Datascript_runtime.Query_value.Metadata value _)
    (query-entity-ref value)
    _ None))

(defn pull-pattern-variable-names [elements]
  (vec
   (mapcat
    (fn [element]
      (if-some [pull (parser/find-element-pull element)]
        (if-some [variable
                  (parser/pull-pattern-variable-name pull)]
          [variable]
          [])
        []))
    elements)))

(defn relation-variable-first-value [relation variable]
  (if-some [row (first (relation-rows relation))]
    (if-some [result (relation-result relation variable row)]
      (result-value result)
      None)
    None))

(defn resolve-pull-pattern [pull relation]
  (if-some [pattern (parser/pull-pattern-value pull)]
    (Some pattern)
    (if-some [variable
              (parser/pull-pattern-variable-name pull)]
      (relation-variable-first-value relation variable)
      None)))

(defn resolve-pull-patterns [elements relation]
  (mapv
   (fn [element]
     (if-some [pull (parser/find-element-pull element)]
       (resolve-pull-pattern pull relation)
       None))
   elements))

(defn apply-find-pull [database pattern entity]
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

(defn pull-row [database sources elements patterns row]
  (to-array
   (mapv
    (fn [element pattern index]
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

(defn pull-rows [database sources elements patterns rows]
  (mapv
   (fn [row]
     (pull-row database sources elements patterns row))
   rows))

(defn join-rows [left left-indexes right right-indexes]
  (Datascript_runtime.Query_value.join_rows
   left left-indexes right right-indexes))

(defn concat-rows [left right]
  (Datascript_runtime.Query_value.concat_rows left right))

(defn datom-row [datom]
  (array
   (entity-result (.-e datom))
   (attr-result (.-a datom))
   (value-result (.-v datom))
   (entity-result (datascript.db/datom-tx datom))
   (added-result (datascript.db/datom-added datom))))

(defn datom-result-at [datom index]
  (case index
    0 (entity-result (.-e datom))
    1 (attr-result (.-a datom))
    2 (value-result (.-v datom))
    3 (entity-result (datascript.db/datom-tx datom))
    4 (added-result (datascript.db/datom-added datom))
    (Stdlib.invalid_arg "Datom projection index is out of bounds")))

(defn project-datom-row [datom indexes]
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
     (fn [index]
       (datom-result-at datom index))
     indexes)))

(defn relation [attrs rows lookup-databases]
  (Datascript_runtime.Query_value.relation attrs rows lookup-databases))

(defn datom-relation [attrs indexes datoms lookup-databases]
  (relation
   attrs
   (mapv
    (fn [datom]
      (project-datom-row datom indexes))
   datoms)
   lookup-databases))

(defn index-attrs [variables]
  (Datascript_runtime.Query_value.index_attrs variables))

(defn pattern-relation [variables indexes datoms lookup-databases]
  (if (= (count variables) (alength indexes))
    (datom-relation
     (index-attrs variables)
     indexes
     datoms
     lookup-databases)
    (Stdlib.invalid_arg
     "Pattern variables and indexes must have the same length")))

(defn pattern-entity-constraint
  [database element]
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

(defn pattern-attr-constraint
  [element]
  (match element
    None (Some None)
    (Some PatternPlaceholder) (Some None)
    (Some (PatternVariable _)) (Some None)
    (Some (PatternConstant value))
    (match (Datascript_runtime.Data_value.keyword_value value)
      None None
      (Some attr) (Some (Some attr)))))

(defn pattern-value-constraint
  [element]
  (match element
    None (Some None)
    (Some PatternPlaceholder) (Some None)
    (Some (PatternVariable _)) (Some None)
    (Some (PatternConstant value))
    (Some (Some value))))

(defn resolve-pattern-value-constraint [database attr value]
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

(defn pattern-added-constraint
  [element]
  (match element
    None (Some None)
    (Some PatternPlaceholder) (Some None)
    (Some (PatternVariable _)) (Some None)
    (Some (PatternConstant value))
    (match (Datascript_runtime.Data_value.keyword_value value)
      (Some ":db/add") (Some (Some true))
      (Some ":db/retract") (Some (Some false))
      _ None)))

(defn pattern-variable-name [element]
  (if-some [variable
            (parser/pattern-element-variable-symbol element)]
    (Some (str variable))
    None))

(defn pattern-element-at [pattern index]
  (if (< index (count pattern))
    (nth pattern index)
    None))

(defn pattern-projection [pattern]
  (let [pattern-count (count pattern)]
  (loop [index 0
         variables []
         indexes []]
    (if (< index pattern-count)
      (let [element (nth pattern index)]
      (if-some [variable (pattern-variable-name element)]
        (recur
         (inc index)
         (conj variables variable)
         (conj indexes index))
        (recur
         (inc index)
         variables
         indexes)))
      (tuple variables indexes)))))

(defn pattern-lookup-databases [database pattern attr]
  (let [databases
        (reduce
         (fn [databases index]
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

(defn lookup-db-pattern [database pattern]
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
               (fn [datom]
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

(defn empty-relation [attrs lookup-databases]
  (relation attrs [] lookup-databases))

(defn identity-relation []
  (relation
   {}
   [(empty-row)]
   (empty-lookup-databases)))

(defn relation-result [relation variable row]
  (Datascript_runtime.Query_value.relation_result
   relation variable row))

(defn relation-attrs [relation]
  (Datascript_runtime.Query_value.relation_attrs relation))

(defn relation-rows [relation]
  (Datascript_runtime.Query_value.relation_rows relation))

(defn relation-lookup-databases [relation]
  (Datascript_runtime.Query_value.relation_lookup_databases relation))

(defn relation-with-rows [relation rows]
  (Datascript_runtime.Query_value.relation_with_rows relation rows))

(defn relation-relabel [relation attrs lookup-databases]
  (Datascript_runtime.Query_value.relation_relabel
   relation attrs lookup-databases))

(defn sum-relation [left right]
  (if (= (relation-attrs left) (relation-attrs right))
    (Datascript_runtime.Query_value.relation_append_rows left right)
    (Stdlib.invalid_arg "Cannot sum relations with different attrs")))

(defn product-attrs [left right]
  (reduce
   (fn [attrs variable]
     (if (contains? attrs variable)
       (Stdlib.invalid_arg "Cannot multiply relations with common attrs")
       (assoc attrs variable (count attrs))))
   left
   (keys right)))

(defn merge-lookup-databases [left right]
  (merge left right))

(defn product-relation [left right]
  (relation
   (product-attrs (relation-attrs left) (relation-attrs right))
   (Datascript_runtime.Query_value.product_rows
    (relation-rows left)
    (relation-rows right))
   (merge-lookup-databases
    (relation-lookup-databases left)
    (relation-lookup-databases right))))

(defn resolve-lookup-result [database result]
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

(defn hash-join [left right]
  (Datascript_runtime.Query_value.hash_join
   resolve-lookup-result left right))

(defn result-pattern-value [result]
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

(defn bind-pattern-element [relation row element]
  (if-some [variable (pattern-variable-name element)]
    (if-some [bound (relation-result relation variable row)]
      (parser/pattern-constant (result-pattern-value bound))
      element)
    element))

(defn bind-pattern [relation row pattern]
  (mapv
   (fn [element]
     (bind-pattern-element relation row element))
   pattern))

(defn identity-relation? [relation]
  (let [rows (relation-rows relation)]
    (and
     (empty? (relation-attrs relation))
     (= 1 (count rows))
     (if-some [row (first rows)]
       (= 0 (alength row))
       false))))

(defn constant-relation-result [relation variable]
  (if-some [index (get (relation-attrs relation) variable)]
    (if-some [first-row (first (relation-rows relation))]
      (if-some [first-value (row-get first-row index)]
        (if
          (every?
           (fn [row]
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

(defn substitute-pattern-constants [relation pattern]
  (mapv
   (fn [element]
     (if-some [variable (pattern-variable-name element)]
       (if-some [value
                 (constant-relation-result relation variable)]
         (parser/pattern-constant (result-pattern-value value))
         element)
       element))
   pattern))

(defn result-entity-id [database value]
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

(defn pattern-row-value [relation row element]
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

(defn- pattern-row-value-present? [relation element]
  (match element
    (Some (PatternConstant _)) true
    (Some (PatternVariable _))
    (if-some [pattern-element element]
      (if-some [variable (pattern-variable-name pattern-element)]
        (contains? (relation-attrs relation) variable)
        false)
      false)
    _ false))

(defn unbound-pattern-projection [relation pattern]
  (let [attrs (relation-attrs relation)]
    (loop [remaining pattern
           index 0
           variables []
           indexes []]
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

(defn resolve-bound-entity-pattern [database input-relation pattern]
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
                value-present?
                (pattern-row-value-present?
                 input-relation value-element)
                tx-present?
                (pattern-row-value-present?
                 input-relation tx-element)
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
                 (fn [rows row]
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
                              (if value-present?
                                (pattern-row-value
                                 input-relation row value-element)
                                None))
                             tx-value
                             (if tx-present?
                               (pattern-row-value
                                input-relation row tx-element)
                               None)
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
                             (fn [rows datom]
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

(defn resolve-db-pattern [database input-relation pattern]
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

(defn- pattern-cache-key
  [database pattern]
  (let [database-id
        (datascript.db/database-view-identity-hash database)
        pattern-count (count pattern)]
    (loop [index 0
           variable-indexes {}
           next-variable-index 0
           key (str database-id)]
      (if (< index pattern-count)
        (let [element (nth pattern index)]
        (if-some [variable (pattern-variable-name element)]
          (if-some [variable-index (get variable-indexes variable)]
            (recur
             (inc index)
             variable-indexes
             next-variable-index
             (str key "|?" (Stdlib.string_of_int variable-index)))
            (recur
             (inc index)
             (assoc variable-indexes variable next-variable-index)
             (inc next-variable-index)
             (str key "|?"
                  (Stdlib.string_of_int next-variable-index))))
          (if-some [value (parser/pattern-element-constant element)]
            (recur
             (inc index)
             variable-indexes
             next-variable-index
             (str key "|="
                  (Datascript_runtime.Data_value.to_edn_string value)))
            (recur
             (inc index)
             variable-indexes
             next-variable-index
             (str key "|_")))))
        key))))

(defn- cached-pattern-relation
  [database pattern cached]
  (let [projection (pattern-projection pattern)
        variables (tuple-get projection 0)
        attr
        (match (pattern-attr-constraint
                (pattern-element-at pattern 1))
          (Some value) value
          None None)]
    (relation-relabel
     cached
     (index-attrs variables)
     (pattern-lookup-databases database pattern attr))))

(defn resolve-db-pattern-cached
  [database input-relation pattern cache]
  (let [pattern
        (substitute-pattern-constants input-relation pattern)
        key (pattern-cache-key database pattern)
        matches
        (if-some [cached (get @cache key)]
          (cached-pattern-relation database pattern cached)
          (let [matches (lookup-db-pattern database pattern)
                _ (swap! cache assoc key matches)]
            matches))]
    (if (identity-relation? input-relation)
      matches
      (hash-join input-relation matches))))

(defn relation-pattern-step
  [bindings projected element value]
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

(defn relation-pattern-row [pattern row]
  (if (<= (count pattern) (alength row))
    (loop [remaining pattern
           index 0
           bindings {}
           projected []]
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

(defn relation-pattern-variables [pattern]
  (reduce
   (fn [variables element]
     (if-some [variable (pattern-variable-name element)]
       (if (some (fn [current] (= current variable))
                 variables)
         variables
         (conj variables variable))
       variables))
   []
   pattern))

(defn resolve-relation-pattern [rows input-relation pattern]
  (let [variables (relation-pattern-variables pattern)
        matched
        (relation
         (index-attrs variables)
         (reduce
          (fn [matched row]
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

(defn resolve-bound-source-pattern
  [source input-relation constants
   pattern
   source-name]
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

(defn resolve-source-pattern
  [database sources implicit-source-name query-source
   input-relation constants pattern]
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

(defn resolve-db-patterns
  [database initial-relation patterns]
  (reduce
   (fn [relation pattern]
     (resolve-db-pattern database relation pattern))
   initial-relation
   patterns))

(defn compile-predicate-argument
  [relation constants argument]
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

(defn predicate-operand-result [row operand]
  (match operand
    (PredicateColumn index)
    (aget row index)
    (PredicateResult result) result))

(defn predicate-operand-value [row operand]
  (result-pattern-value
   (predicate-operand-result row operand)))

(defn query-source-database
  [database sources source-name]
  (if-some [source (get sources source-name)]
    (if-some [source-database (source-database source)]
      source-database
      (Stdlib.invalid_arg
       (str "Query source is not a database: " source-name)))
    (if (= source-name "$")
      database
      (Stdlib.invalid_arg
       (str "Query source is not bound: " source-name)))))

(defn callable-argument-result
  [database sources relation constants row argument]
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

(defn callable-arguments
  [database sources relation constants row arguments]
  (mapv
   (fn [argument]
     (callable-argument-result
      database sources relation constants row argument))
   arguments))

(defn row-callable
  [relation constants row variable]
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

(defn query-entity-reference [result]
  (match result
    (Datascript_runtime.Query_value.Entity entity)
    (Datascript_runtime.Data_value.Entity_id entity)
    (Datascript_runtime.Query_value.Value value)
    (datascript.db/data-value-entity-ref value)
    (Datascript_runtime.Query_value.Metadata value _)
    (datascript.db/data-value-entity-ref value)
    _
    (Stdlib.invalid_arg "Expected a query entity reference")))

(defn query-attribute [result]
  (if-some [attribute
            (Datascript_runtime.Data_value.keyword_value
             (result-pattern-value result))]
    (keyword attribute)
    (Stdlib.invalid_arg "Expected a query attribute")))

(defn query-datom
  [database entity-result attribute-result]
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

(defn query-missing?
  [database entity-result attribute-result]
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

(defn query-database-result [result]
  (match result
    (Datascript_runtime.Query_value.Database database) database
    _ (Stdlib.invalid_arg "Expected a database query argument")))

(defn database-function-value
  [function arguments]
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

(defn resolve-database-predicate
  [database sources relation constants arguments]
  (relation-with-rows
   relation
   (filterv
    (fn [row]
      (let [values
            (callable-arguments
             database sources relation constants row arguments)]
        (if (= 3 (count values))
          (let [source (query-database-result (nth values 0))]
            (query-missing?
             source (nth values 1) (nth values 2)))
          (Stdlib.invalid_arg
           "Invalid arguments for query predicate: missing?"))))
    (relation-rows relation))))

(defn data-value-binding [value]
  (if-some [items
            (Datascript_runtime.Data_value.sequential_items value)]
    (collection-binding (mapv data-value-binding items))
    (if-some [items
              (Datascript_runtime.Data_value.tuple_items value)]
      (collection-binding
       (mapv
        (fn [item]
          (match item
            (Some tuple-value) (data-value-binding tuple-value)
            None (scalar-binding (value-result
                                  (Datascript_runtime.Data_value.Nil)))))
        items))
      (scalar-binding (value-result value)))))

(defn function-binding-result [binding result]
  (if
   (or
    (parser/binding-ignore? binding)
    (some? (parser/binding-scalar-variable binding)))
    (scalar-binding result)
    (if-some [value (result-value result)]
      (data-value-binding value)
      (Stdlib.invalid_arg
       "A callable query function result requires a scalar binding"))))

(defn function-binding-value
  [binding value]
  (function-binding-result binding (value-result value)))

(defn function-result-attrs [relation binding]
  (reduce
   (fn [attrs variable]
     (if (contains? attrs variable)
       attrs
       (assoc attrs variable (count attrs))))
   (relation-attrs relation)
   (parser/binding-variable-names binding)))

(defn resolve-database-function
  [database sources relation constants function arguments binding]
  (let [resolved
        (reduce
         (fn [output row]
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

(defn join-query-parts [parts]
  (if-some [first-part (first parts)]
    (reduce
     (fn [result part]
       (str result " " part))
     first-part
     (subvec parts 1))
    ""))

(defn query-argument-description [argument]
  (if-some [variable (parser/argument-variable-name argument)]
    variable
    (if-some [source (parser/argument-source-name argument)]
      source
      (if-some [value (parser/argument-constant argument)]
        (Datascript_runtime.Data_value.to_edn_string value)
        ""))))

(defn query-call-description
  [name arguments]
  (let [parts
        (vec
         (cons name (mapv query-argument-description arguments)))]
    (str "(" (join-query-parts parts) ")")))

(defn rule-argument-description [argument]
  (if-some [variable (pattern-variable-name argument)]
    variable
    (if-some [value (parser/pattern-element-constant argument)]
      (Datascript_runtime.Data_value.to_edn_string value)
      "_")))

(defn rule-call-description
  [name arguments]
  (str
   "("
   (join-query-parts
    (vec
     (cons name (mapv rule-argument-description arguments))))
   ")"))

(defn query-binding-description [binding]
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

(defn missing-query-arguments
  [relation constants arguments]
  (reduce
   (fn [missing argument]
     (if-some [variable (parser/argument-variable-name argument)]
       (if
        (or
         (contains? (relation-attrs relation) variable)
         (some? (constant-relation-result constants variable))
         (some (fn [name] (= name variable)) missing))
         missing
         (conj missing variable))
       missing))
   []
   arguments))

(defn query-variable-set-description [variables]
  (str "#{" (join-query-parts variables) "}"))

(defn validate-static-call-bindings
  [relation constants name arguments binding]
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

(defn- data-value-truthy? [value]
  (if (Datascript_runtime.Data_value.is_nil value)
    false
    (match (Datascript_runtime.Data_value.bool_value value)
      (Some boolean-value) boolean-value
      None true)))

(defn result-nil? [result]
  (if-some [value (result-value result)]
    (Datascript_runtime.Data_value.is_nil value)
    false))

(defn complement-result [arguments]
  (let [target (first arguments)]
    (callable-result
     (callable
      (fn [invocation-arguments]
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

(defn metadata-function-result [arguments]
  (if-some [argument (first arguments)]
    (if-some [metadata (result-metadata argument)]
      (value-result metadata)
      (value-result (Datascript_runtime.Data_value.Nil)))
    (value-result (Datascript_runtime.Data_value.Nil))))

(defn value-type-function-result [arguments]
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

(defn- static-function-result
  [function results]
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

(defn- query-result-truthy? [result]
  (if-some [value (result-value result)]
    (data-value-truthy? value)
    true))

(defn- differ-predicate-matches?
  [row operands]
  (let [operand-count (alength operands)
        middle (quot operand-count 2)]
    (if (= operand-count (* middle 2))
      (loop [index 0]
        (if (< index middle)
          (let [left
                (predicate-operand-result
                 row (aget operands index))
                right
                (predicate-operand-result
                 row (aget operands (+ middle index)))]
            (if
             (Datascript_runtime.Query_value.equal_pattern_result
              left right)
              (recur (inc index))
              true))
          false))
      true)))

(defn resolve-predicate
  [database
   sources
   relation
   constants
   callable
   arguments]
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
               (fn [argument]
                 (compile-predicate-argument
                  relation constants argument))
               arguments)
              operand-array (to-array operands)
              differ?
              (match function
                (PureStaticPredicate pure)
                (built-ins/differ-function? pure)
                (ComparisonStaticPredicate _) false)
              binary-operands
              (if (= 2 (count operands))
                (Some (tuple (nth operands 0) (nth operands 1)))
                None)]
          (Datascript_runtime.Query_value.relation_filter_rows
           relation
            (fn [row]
              (let [matches?
                    (if differ?
                      (Some
                       (differ-predicate-matches?
                        row operand-array))
                      (match function
                        (ComparisonStaticPredicate comparison)
                        (let [binary-result
                              (match binary-operands
                                (Some pair)
                                (built-ins/binary-comparison
                                 comparison
                                 (predicate-operand-value
                                  row (tuple-get pair 0))
                                 (predicate-operand-value
                                  row (tuple-get pair 1)))
                                None None)]
                          (match binary-result
                            (Some matches?) (Some matches?)
                            None
                            (built-ins/apply-comparison
                             comparison
                             (mapv
                              (fn [operand]
                                (predicate-operand-value row operand))
                              operands))))
                        (PureStaticPredicate _)
                        (let [results
                              (mapv
                               (fn [operand]
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
                            None None))))]
                (if-some [matches? matches?]
                  matches?
                  (Stdlib.invalid_arg
                   (str
                    "Unsupported static query predicate: "
                    name))))))))
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
       (filterv
        (fn [row]
          (let [callable (row-callable relation constants row variable)
                arguments
                (callable-arguments
                 database sources relation constants row arguments)]
            (if-some [value (invoke-callable callable arguments)]
              (not
               (or
                (Datascript_runtime.Data_value.is_nil value)
                (= (Datascript_runtime.Data_value.bool_value value)
                   (Some false))))
              false)))
        (relation-rows relation)))
      (Stdlib.invalid_arg
       "Variable query predicate name is missing"))))

(defn resolve-function
  [database
   sources
   relation
   constants
   callable
   arguments
   binding]
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
               (fn [argument]
                 (compile-predicate-argument
                  relation constants argument))
               arguments)
              resolved
              (reduce
               (fn [output row]
                 (let [results
                       (mapv
                        (fn [operand]
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
             (fn [output row]
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

(defn rows-match-on-variables?
  [left left-row right right-row variables]
  (every?
   (fn [variable]
     (if-some [left-value
               (relation-result left variable left-row)]
       (if-some [right-value
                 (relation-result right variable right-row)]
         (Datascript_runtime.Query_value.equal_result
          left-value right-value)
         false)
       false))
   variables))

(defn project-relation-variables
  [source-relation variables]
  (let [attrs (relation-attrs source-relation)
        indexes
        (mapv
         (fn [variable]
           (if-some [index (get attrs variable)]
             index
             (Stdlib.invalid_arg
              (str "Cannot project unbound query variable: "
                   variable))))
         variables)
        lookup-databases
        (reduce
         (fn [databases variable]
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
      (fn [row]
        (project-row row index-array))
      (relation-rows source-relation))
     lookup-databases)))

(defn variable-set-display
  [variables]
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
  [database
   sources
   implicit-source-name
   relation
   constants
   rules
   rule-path
   variables
   clauses
   display]
  (let [bound-variables
        (vec
         (filter
          (fn [variable]
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
             (fn [variable]
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
          (fn [row]
            (not
             (some
              (fn [matched-row]
                (rows-match-on-variables?
                 relation row matched matched-row bound-names))
              (relation-rows matched))))
          (relation-rows relation)))))))

(defn resolve-or-branch
  [database
   sources
   implicit-source-name
   relation
   constants
   rules
   rule-path
   branch]
  (if-some [clauses (parser/and-clause-clauses branch)]
    (resolve-static-clauses
     database sources implicit-source-name
     relation constants rules rule-path clauses)
    (resolve-static-clauses
     database sources implicit-source-name
     relation constants rules rule-path [branch])))

(defn query-variable-bound?
  [relation constants variable]
  (or
   (contains? (relation-attrs relation) variable)
   (contains? (relation-attrs constants) variable)))

(defn clause-free-variable-names
  [relation constants clause]
  (vec
   (distinct
    (filter
     (fn [variable]
       (not (query-variable-bound? relation constants variable)))
     (mapv
      (fn [variable]
        (str (.-symbol variable)))
      (parser/clause-vars clause))))))

(defn branch-free-variable-names
  [relation constants branches]
  (mapv
   (fn [branch]
     (clause-free-variable-names relation constants branch))
   branches))

(defn variable-sets-description
  [variable-sets]
  (str
   "["
   (join-query-parts
    (mapv query-variable-set-description variable-sets))
   "]"))

(defn resolve-or
  [database
   sources
   implicit-source-name
   relation
   constants
   rules
   rule-path
   required-variable-names
   branch-variable-names
   branches
   join?
   display]
  (let [missing-required
        (filterv
         (fn [variable]
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
            (fn [variables]
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
               (fn [variable]
                 (not
                  (contains?
                   (relation-attrs constants)
                   variable)))
               branch-variable-names)
              bound-join-variable-names
              (filterv
               (fn [variable]
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
               (fn [branch]
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

(defn rule-argument-bound?
  [relation constants argument]
  (if-some [variable (pattern-variable-name argument)]
    (if (contains? (relation-attrs relation) variable)
      true
      (some? (constant-relation-result constants variable)))
    (some? (parser/pattern-element-constant argument))))

(defn rule-argument-result
  [relation constants row argument]
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

(defn rule-branch-input
  [outer-relation constants parameters arguments]
  (let [bound-indexes
        (filterv
         (fn [index]
           (rule-argument-bound?
            outer-relation constants (nth arguments index)))
         (range (count arguments)))
        bound-parameters
        (mapv
         (fn [index] (nth parameters index))
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
                (fn [row]
                  (to-array
                   (mapv
                    (fn [index]
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

(defn rule-branch-output
  [outer-relation branch-result parameters arguments]
  (let [variable-indexes
        (filterv
         (fn [index]
           (some? (pattern-variable-name (nth arguments index))))
         (range (count arguments)))
        output-variables
        (mapv
         (fn [index]
           (if-some [variable
                     (pattern-variable-name
                      (nth arguments index))]
             variable
             (Stdlib.invalid_arg
              "Rule output argument is not a variable")))
         variable-indexes)
        output-parameters
        (mapv
         (fn [index] (nth parameters index))
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

(defn ensure-empty-relation-variables
  [relation variables]
  (if (empty? (relation-rows relation))
    (empty-relation
     (reduce
      (fn [attrs variable]
        (if (contains? attrs variable)
          attrs
          (assoc attrs variable (count attrs))))
      (relation-attrs relation)
      variables)
     (relation-lookup-databases relation))
    relation))

(defn distinct-results
  [values]
  (reduce
   (fn [distinct value]
     (if
      (some
       (fn [candidate]
         (Datascript_runtime.Query_value.equal_result
          candidate value))
       distinct)
       distinct
       (conj distinct value)))
   []
   values))

(defn rule-call-argument
  [relation constants argument]
  (if-some [variable (pattern-variable-name argument)]
    (RuleCallVariable
     variable
     (if (contains? (relation-attrs relation) variable)
       (distinct-results
        (mapv
         (fn [row]
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

(defn make-rule-call
  [rule-name relation constants arguments]
  (RuleCall
   rule-name
   (mapv
    (fn [argument]
      (rule-call-argument relation constants argument))
    arguments)))

(defn result-vectors-equal?
  [left right]
  (if (= (count left) (count right))
    (every?
     (fn [index]
       (Datascript_runtime.Query_value.equal_result
        (nth left index)
        (nth right index)))
     (range (count left)))
    false))

(defn rule-call-arguments-equal?
  [left right]
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

(defn rule-calls-equal?
  [left right]
  (match left
    (RuleCall left-name left-arguments)
    (match right
      (RuleCall right-name right-arguments)
      (and
       (= left-name right-name)
       (= (count left-arguments)
          (count right-arguments))
       (every?
        (fn [index]
          (rule-call-arguments-equal?
           (nth left-arguments index)
           (nth right-arguments index)))
        (range (count left-arguments)))))))

(defn empty-rule-call-relation
  [relation arguments]
  (let [attrs
        (reduce
         (fn [attrs argument]
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

(defn resolve-rule-branch
  [database
   sources
   implicit-source-name
   relation
   constants
   rules
   rule-path
   arguments
   branch]
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

(defn resolve-rule
  [database
   sources
   implicit-source-name
   relation
   constants
   rules
   rule-path
   rule-name
   arguments]
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
            (fn [index]
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
          (fn [previous]
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
                   (fn [result branch]
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

(defn resolve-static-clauses
  [database
   sources
   implicit-source-name
   initial-relation
   constants
   rules
   rule-path
   clauses]
  (reduce
   (fn [relation clause]
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

(defn single-row-constant-relation?
  [relation]
  (and
   (= 1 (count (relation-rows relation)))
   (every?
    (fn [variable]
      (some? (constant-relation-result relation variable)))
    (keys (relation-attrs relation)))))

(defn relation-variables-used?
  [relation variables]
  (some
   (fn [variable]
     (contains? (relation-attrs relation) variable))
   variables))

(defn lookup-db-patterns
  [database patterns]
  (resolve-db-patterns database (identity-relation) patterns))

(defn require-relation-result
  [relation row variable]
  (if-some [result (relation-result relation variable row)]
    result
    (Stdlib.invalid_arg
     (str "Query find variable is not bound: " variable))))

(defn project-find-row
  [relation variables row]
  (to-array
   (mapv
    (fn [variable]
      (require-relation-result relation row variable))
    variables)))

(defn project-find-rows
  [relation variables]
  (let [attrs (relation-attrs relation)
        indexes
        (mapv
         (fn [variable]
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
         (fn [row]
           (project-row row indexes))
         (relation-rows relation))))))

(defn first-row-result [row]
  (if-some [result (row-get row 0)]
    result
    (Stdlib.invalid_arg "Find shape requires one projected value")))

(defn return-map-row
  [keys row]
  (if (= (count keys) (alength row))
    (loop [remaining keys
           index 0
           result-map {}]
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

(defn return-map-rows
  [keys rows]
  (Datascript_runtime.Query_value.distinct_result_maps
   (mapv
    (fn [row]
      (return-map-row keys row))
    rows)))

(defn mapped-find-output
  [find return-map rows]
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

(defn find-output
  [find return-map rows]
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

(defn execute-resolved-query
  [database sources query constants resolved-relation]
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
               (fn [row]
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

(defn execute-db-query-with-rules
  [database sources query input-relation rules]
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

(defn execute-db-query-with-relation
  [database query input-relation]
  (execute-db-query-with-rules
   database
   {"$" (database-view-source database)}
   query input-relation []))

(defn execute-db-query
  [database query]
  (execute-db-query-with-relation
   database query (identity-relation)))

(defn require-database-input [input]
  (if-some [source (input-source input)]
    (if-some [database (source-database source)]
      database
      (Stdlib.invalid_arg
       "Static DB query requires a Database_source"))
    (Stdlib.invalid_arg
     "Static DB query input must be a Source_input")))

(defn require-binding-input [input]
  (if-some [binding (input-binding input)]
    binding
    (Stdlib.invalid_arg
     "Query value input requires a Binding_input")))

(defn tuple-binding-relation
  [bindings values]
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

(defn collection-binding-relation
  [binding values]
  (if (empty? values)
    (empty-relation
     (index-attrs
      (parser/binding-variable-names binding))
     (empty-lookup-databases))
    (let [relations
          (mapv
           (fn [value]
             (binding-relation binding value))
           values)]
      (if-some [first-relation (first relations)]
        (reduce
         sum-relation
         first-relation
         (subvec relations 1))
        (Stdlib.invalid_arg
         "Collection binding relation is unexpectedly empty")))))

(defn binding-relation
  [binding value]
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

(defn binding-input-relations
  [bindings inputs]
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

(defn bind-static-query-inputs
  [descriptors inputs]
  (loop [remaining-descriptors descriptors
         remaining-inputs inputs
         relation (identity-relation)
         sources {}
         rules []]
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

(defn query-default-database
  [sources]
  (if-some [source (get sources "$")]
    (if-some [database (source-database source)]
      database
      (datascript.db/database-view
       (datascript.db/empty-db
        None
        (datascript.db/default-options))))
    (reduce-kv
     (fn [database _ source]
       (if-some [source-database (source-database source)]
         source-database
         database))
     (datascript.db/database-view
      (datascript.db/empty-db
       None
       (datascript.db/default-options)))
     sources)))

(defn execute-query
  [query inputs]
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

(defn relation-lookup-database
  [relation variable]
  (Datascript_runtime.Query_value.relation_lookup_database
   relation variable))

(defn context
  [relations sources rules]
  (Datascript_runtime.Query_value.context relations sources rules))

(defn context-relations [context]
  (Datascript_runtime.Query_value.context_relations context))

(defn context-sources [context]
  (Datascript_runtime.Query_value.context_sources context))

(defn context-rules [context]
  (Datascript_runtime.Query_value.context_rules context))
