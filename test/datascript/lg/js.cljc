(ns ^:no-doc datascript.js
  (:refer-clojure :exclude [filter])
  (:require
   [datascript.built-ins :as built-ins]
   [datascript.conn :as conn]
   [datascript.core :as d]
   [datascript.db :as db]
   [datascript.lg.query :as query]
   [datascript.lg.query-types :as query-types]
   [datascript.parser :as parser]
   [datascript.pull-api :as pull-api]
   [datascript.pull-parser :as pull-parser]))

(def ^:export serializable d/serializable)
(def ^:export from_serializable d/from-serializable)

(defn- q-string [source inputs]
  (query/q-closed
   (parser/parse-query
    (Datascript_runtime.Serialization_value.data_value_of_edn_string
     source))
   inputs))

(defn ^:export q
  {:inline
   (fn [source & inputs]
     (let [data-value-form
           (fn [value]
             (if (nil? value)
               (list 'Datascript_runtime.Data_value.Nil)
               (if (keyword? value)
                 (list
                  'Datascript_runtime.Data_value.Keyword
                  (str value))
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
                       (list
                        'Datascript_runtime.Data_value.Int
                        value)))))))
           database-input
           (fn [database]
             (list
              'datascript.lg.query-types/source-input
              (list
               'datascript.lg.query-types/database-source
               (list
                'datascript.db/database-view
                database))))
           scalar-input
           (fn [value]
             (list
              'datascript.lg.query-types/binding-input
              (list
               'datascript.lg.query-types/scalar-binding
               (list
                'datascript.lg.query-types/value-result
                (data-value-form value)))))
           input-forms
           (if (empty? inputs)
             []
             (let [database (first inputs)]
             (reduce
              (fn [forms value]
                (conj forms (scalar-input value)))
              [(database-input database)]
              (next inputs))))]
       (list
        'datascript.js/q-string
        source
        input-forms)))}
  [source
   & inputs]
  (q-string source (vec inputs)))

(defn- data-value-int [field value]
  (match value
    (Datascript_runtime.Data_value.Int result) result
    _
    (Stdlib.invalid_arg
     (str "js->Datom expected integer field " field))))

(defn- data-value-keyword [field value]
  (match value
    (Datascript_runtime.Data_value.Keyword result) (keyword result)
    _
    (Stdlib.invalid_arg
     (str "js->Datom expected keyword field " field))))

(defn- data-value-tx [value]
  (match value
    (Datascript_runtime.Data_value.Nil) db/tx0
    _ (data-value-int "tx" value)))

(defn- required-datom-map-field [input field]
  (if-some
    [value
     (Datascript_runtime.Data_value.keyword_map_get field input)]
    value
    (Stdlib.invalid_arg
     (str "js->Datom missing field " field))))

(defn- datom-from-data-vector [values]
  (if (< (count values) 3)
    (Stdlib.invalid_arg
     "js->Datom expected at least three vector fields")
    (db/datom-closed
     (data-value-int "e" (nth values 0))
     (data-value-keyword "a" (nth values 1))
     (nth values 2)
     (if (< 3 (count values))
       (data-value-tx (nth values 3))
       db/tx0)
     true)))

(defn- datom-from-data-map [input]
  (db/datom-closed
   (data-value-int
    "e"
    (required-datom-map-field input ":e"))
   (data-value-keyword
    "a"
    (required-datom-map-field input ":a"))
   (required-datom-map-field input ":v")
   (if-some
     [tx
      (Datascript_runtime.Data_value.keyword_map_get ":tx" input)]
     (data-value-tx tx)
     db/tx0)
   true))

(defn- data-value->datom
  [input]
  (if-some
    [values
     (Datascript_runtime.Data_value.sequential_items input)]
    (datom-from-data-vector values)
    (if-some
      [_map
       (Datascript_runtime.Data_value.keyword_map_value input)]
      (datom-from-data-map input)
      (Stdlib.invalid_arg
       "js->Datom expects a vector or map"))))

(defn ^:export js->Datom
  {:inline
   (fn [input]
     (if (vector? input)
       (let [tx (first (drop 3 input))]
         (list
          'datascript.core/datom
          (first input)
          (second input)
          (first (drop 2 input))
          (if (nil? tx)
            'datascript.db/tx0
            tx)
          true))
       (if (map? input)
         (list
          'datascript.core/datom
          (get input :e)
          (get input :a)
          (get input :v)
          (if-some [tx (get input :tx)]
            tx
            'datascript.db/tx0)
          true)
         (list 'datascript.js/data-value->datom input))))}
  [input]
  (data-value->datom input))

(defn- pull-pattern-error [source]
  (Stdlib.invalid_arg
   (str
    "Expected pattern to be sequential?, got: "
    (Datascript_runtime.Data_value.to_edn_string source))))

(defn- pull-source-keyword [source]
  (match source
    (Datascript_runtime.Data_value.Keyword name) (keyword name)
    (Datascript_runtime.Data_value.String name) (keyword name)
    _
    (Stdlib.invalid_arg
     (str
      "Expected pull attribute, got: "
      (Datascript_runtime.Data_value.to_edn_string source)))))

(defn- pull-source-attribute-option
  [source]
  (if-some
    [name
     (Datascript_runtime.Data_value.keyword_value source)]
    (Some (keyword name))
    (match source
      (Datascript_runtime.Data_value.String name)
      (Some (keyword name))
      _ None)))

(defn- pull-source-wildcard?
  [source]
  (match source
    (Datascript_runtime.Data_value.Symbol name)
    (= name "*")
    _ false))

(signature datascript.js/pull-source-recursion-limit
  :fn<Datascript_runtime.Data_value.t;option<option<int>>>)

(defn- pull-source-recursion-limit [source]
  (match source
    (Datascript_runtime.Data_value.Symbol name)
    (if (= name "...") (Some None) None)
    (Datascript_runtime.Data_value.String name)
    (if (= name "...") (Some None) None)
    (Datascript_runtime.Data_value.Int limit)
    (Some (Some limit))
    _ None))

(defn- pull-source-xform [source]
  (match source
    (Datascript_runtime.Data_value.Symbol name)
    (if-some [function (built-ins/pure-function name)]
      (fn [value]
        (if-some [value value]
          (built-ins/apply-pure-function function [value])
          None))
      (Stdlib.invalid_arg
       (str "Can't resolve symbol " name)))
    _
    (Stdlib.invalid_arg
     (str
      "Can't resolve pull transform "
      (Datascript_runtime.Data_value.to_edn_string source)))))

(defn- pull-source-option
  [key
   value]
  (if-some
    [name
     (Datascript_runtime.Data_value.keyword_value key)]
    (case name
      ":as"
      (pull-parser/option-alias-value value)

      ":default"
      (pull-parser/option-default value)

      ":limit"
      (if
       (Datascript_runtime.Data_value.is_nil value)
        (pull-parser/option-unlimited)
        (pull-parser/option-limit
         (data-value-int "limit" value)))

      ":xform"
      (pull-parser/option-xform (pull-source-xform value))

      (Stdlib.invalid_arg
       (str "Unknown pull option " name)))
    (Stdlib.invalid_arg
     (str
      "Unknown pull option "
      (Datascript_runtime.Data_value.to_edn_string key)))))

(defn- pull-source-attribute [source]
  (if-some
    [items
     (Datascript_runtime.Data_value.sequential_items source)]
    (if-some [attribute (first items)]
      (let [remaining (subvec items 1)]
        (when-not (= 0 (mod (count remaining) 2))
          (Stdlib.invalid_arg
           "Pull attribute options must contain key/value pairs"))
        (tuple
         (pull-source-keyword attribute)
         (loop [remaining remaining
                options []]
           (if-some [key (first remaining)]
             (if-some [value (first (subvec remaining 1))]
               (recur
                (subvec remaining 2)
                (conj options (pull-source-option key value)))
               options)
             options))))
      (Stdlib.invalid_arg "Pull attribute expression is empty"))
    (tuple (pull-source-keyword source) [])))

(signature datascript.js/pull-source-item
  :fn<Datascript_runtime.Data_value.t;datascript.pull-parser/pull-source-item>)

(defn- pull-source-item [source]
  (if
   (pull-source-wildcard? source)
   pull-parser/source-wildcard
   (if-some
    [source-attribute
     (pull-source-attribute-option source)]
    (pull-parser/source-attribute source-attribute)
    (if-some
      [entries
       (Datascript_runtime.Data_value.map_entries source)]
      (let [nested-items
            (mapv
             (fn [entry]
               (let [attribute-source (tuple-get entry 0)
                     nested-source (tuple-get entry 1)
                     attribute-and-options
                     (pull-source-attribute attribute-source)
                     attribute (tuple-get attribute-and-options 0)
                     options (tuple-get attribute-and-options 1)]
                 (if-some
                   [recursion-limit
                    (pull-source-recursion-limit nested-source)]
                   (if (empty? options)
                     (pull-parser/source-recursion
                      attribute recursion-limit)
                     (pull-parser/source-recursion-options
                      attribute options recursion-limit))
                   (if-some
                     [nested-values
                      (Datascript_runtime.Data_value.sequential_items
                       nested-source)]
                     (let [nested-pattern
                           (mapv
                            (fn [item]
                              (pull-source-item item))
                            nested-values)]
                       (if (empty? options)
                         (pull-parser/source-nested
                          attribute nested-pattern)
                         (pull-parser/source-nested-options
                          attribute options nested-pattern)))
                     (pull-parser/source-invalid
                      nested-source)))))
             entries)]
        (if (= 1 (count nested-items))
          (nth nested-items 0)
          (pull-parser/source-group nested-items)))
      (if-some
        [_items
         (Datascript_runtime.Data_value.sequential_items source)]
        (let [attribute-and-options
              (pull-source-attribute source)
              attribute (tuple-get attribute-and-options 0)
              options (tuple-get attribute-and-options 1)]
          (if (empty? options)
            (pull-parser/source-attribute attribute)
            (pull-parser/source-options attribute options)))
        (pull-parser/source-invalid source))))))

(defn- pull-source-pattern [source]
  (let [parsed
        (Datascript_runtime.Serialization_value.data_value_of_edn_string
         source)]
    (if-some
      [items
       (Datascript_runtime.Data_value.sequential_items parsed)]
      (mapv
       (fn [item]
         (pull-source-item item))
       items)
      (pull-pattern-error parsed))))

(signature datascript.js/pull-string
  :fn<datascript.db/DB;string;Datascript_runtime.Data_value.entity_ref;option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>>)

(defn- pull-string [database pattern entity-ref]
  (pull-api/pull-source
   (db/database-view database)
   (pull-source-pattern pattern)
   entity-ref))

(signature datascript.js/pull-many-string
  :fn<datascript.db/DB;string;vector<Datascript_runtime.Data_value.entity_ref>;vector<option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>>>)

(defn- pull-many-string [database pattern entity-refs]
  (pull-api/pull-many-source
   (db/database-view database)
   (pull-source-pattern pattern)
   entity-refs))

(defn ^:export pull
  {:inline
   (fn [database pattern entity-id]
     (let [value-form
           (fn [value]
             (if (string? value)
               (list
                'Datascript_runtime.Data_value.String
                value)
               (if (keyword? value)
                 (list
                  'Datascript_runtime.Data_value.Keyword
                  (str value))
                 (if (or (symbol? value) (seq? value))
                   value
                   (list
                    'Datascript_runtime.Data_value.Int
                    value)))))
           entity-ref
           (if (vector? entity-id)
             (list
              'Datascript_runtime.Data_value.Lookup_ref
              (str (first entity-id))
              (value-form (second entity-id)))
             (if (keyword? entity-id)
               (list
                'Datascript_runtime.Data_value.Ident
                (str entity-id))
               (list
                'Datascript_runtime.Data_value.Entity_id
                entity-id)))]
       (list
        'datascript.js/pull-string
        database pattern entity-ref)))}
  [database pattern entity-ref]
  (pull-string database pattern entity-ref))

(defn ^:export pull_many
  {:inline
   (fn [database pattern entity-ids]
     (let [value-form
           (fn [value]
             (if (string? value)
               (list
                'Datascript_runtime.Data_value.String
                value)
               (if (keyword? value)
                 (list
                  'Datascript_runtime.Data_value.Keyword
                  (str value))
                 (if (or (symbol? value) (seq? value))
                   value
                   (list
                    'Datascript_runtime.Data_value.Int
                    value)))))
           entity-ref
           (fn [entity-id]
             (if (vector? entity-id)
               (list
                'Datascript_runtime.Data_value.Lookup_ref
                (str (first entity-id))
                (value-form (second entity-id)))
               (if (keyword? entity-id)
                 (list
                  'Datascript_runtime.Data_value.Ident
                  (str entity-id))
                 (list
                  'Datascript_runtime.Data_value.Entity_id
                  entity-id))))
           entity-refs
           (if (vector? entity-ids)
             (list
              'vec
              (cons
               'list
               (reduce
                (fn [refs entity-id]
                  (conj refs (entity-ref entity-id)))
                []
                entity-ids)))
             entity-ids)]
       (list
        'datascript.js/pull-many-string
        database pattern entity-refs)))}
  [database pattern entity-refs]
  (pull-many-string database pattern entity-refs))

(signature datascript.js/index-string->keyword
  :fn<string;keyword>)
(defn- index-string->keyword
  [index]
  (if (= ":" (subs index 0 1))
    (keyword (subs index 1))
    (keyword index)))

(signature datascript.js/datoms-from-components
  :fn<datascript.db/DB;keyword;vector<Datascript_runtime.Data_value.t>;seq<datascript.db/Datom>>)

(defn- datoms-from-components
  [database index components]
  (cond
    (= 0 (count components))
    (d/datoms-closed database index)

    (= 1 (count components))
    (d/datoms-closed database index (nth components 0))

    (= 2 (count components))
    (d/datoms-closed
     database index
     (nth components 0)
     (nth components 1))

    (= 3 (count components))
    (d/datoms-closed
     database index
     (nth components 0)
     (nth components 1)
     (nth components 2))

    (= 4 (count components))
    (d/datoms-closed
     database index
     (nth components 0)
     (nth components 1)
     (nth components 2)
     (nth components 3))

    :else
    (Stdlib.invalid_arg "datoms accepts at most four components")))

(defn ^:export datoms
  {:inline
   (fn [database index & components]
     (let [index
           (if (string? index)
             (cond
               (= index ":eavt") :eavt
               (= index ":aevt") :aevt
               (= index ":avet") :avet
               :else index)
             index)]
       (cons
        'datascript.core/datoms
        (cons database (cons index components)))))}
  [database
   index
   & components]
  (datoms-from-components
   database
   (index-string->keyword index)
   (vec components)))

(signature datascript.js/seek-datoms-from-components
  :fn<datascript.db/DB;keyword;vector<Datascript_runtime.Data_value.t>;seq<datascript.db/Datom>>)

(defn- seek-datoms-from-components
  [database index components]
  (cond
    (= 0 (count components))
    (d/seek-datoms-closed database index)

    (= 1 (count components))
    (d/seek-datoms-closed database index (nth components 0))

    (= 2 (count components))
    (d/seek-datoms-closed
     database index
     (nth components 0)
     (nth components 1))

    (= 3 (count components))
    (d/seek-datoms-closed
     database index
     (nth components 0)
     (nth components 1)
     (nth components 2))

    (= 4 (count components))
    (d/seek-datoms-closed
     database index
     (nth components 0)
     (nth components 1)
     (nth components 2)
     (nth components 3))

    :else
    (Stdlib.invalid_arg "seek_datoms accepts at most four components")))

(defn ^:export seek_datoms
  {:inline
   (fn [database index & components]
     (let [index
           (if (string? index)
             (cond
               (= index ":eavt") :eavt
               (= index ":aevt") :aevt
               (= index ":avet") :avet
               :else index)
             index)]
       (cons
        'datascript.core/seek-datoms
        (cons database (cons index components)))))}
  [database
   index
   & components]
  (seek-datoms-from-components
   database
   (index-string->keyword index)
   (vec components)))

(defn ^:export db_with
  {:inline
   (fn [database entities]
     (list 'datascript.core/db-with database entities))}
  [database entities]
  (d/db-with database entities))

(defn ^:export empty_db [& [schema]]
  (if-some [schema schema]
    (d/empty-db schema)
    (d/empty-db)))

(defn ^:export init_db [datoms & [schema]]
  (if-some [schema schema]
    (d/init-db datoms schema)
    (d/init-db datoms)))

(def ^:export touch d/touch)
(def ^:export entity_db d/entity-db)

(defn ^:export entity
  {:inline
   (fn [database entity-ref]
     (list 'datascript.core/entity database entity-ref))}
  [database entity-ref]
  (d/entity database entity-ref))

(def ^:export filter d/filter)
(def ^:export is_filtered d/is-filtered)

(defn ^:export create_conn [& [schema]]
  (if-some [schema schema]
    (d/create-conn schema)
    (d/create-conn)))

(def ^:export conn_from_db d/conn-from-db)

(defn ^:export conn_from_datoms
  ([datoms]
   (conn_from_db (init_db datoms)))
  ([datoms schema]
   (conn_from_db (init_db datoms schema))))

(defn ^:export db
  [connection]
  (conn/current-db connection))

(defn ^:export transact
  {:inline
   (fn [connection entities & tx-meta]
     (cons
      'datascript.core/transact!
      (cons connection (cons entities tx-meta))))}
  [connection entities & [tx-meta]]
  (if-some [metadata tx-meta]
    (conn/transact! connection entities (Some metadata))
    (conn/transact! connection entities)))

(defn ^:export reset_conn
  {:inline
   (fn [connection database & tx-meta]
     (cons
      'datascript.core/reset-conn!
      (cons connection (cons database tx-meta))))}
  [connection database & [tx-meta]]
  (if-some [metadata tx-meta]
    (conn/reset-conn! connection database metadata)
    (conn/reset-conn! connection database)))

(def ^:export listen d/listen!)
(def ^:export unlisten d/unlisten!)

(defn ^:export resolve_tempid
  [tempids
   tempid]
  (get tempids tempid))

(defn ^:export index_range
  {:inline
   (fn [database attr start end]
     (list
      'datascript.core/index-range
      database
      attr
      start
      end))}
  [database attr start end]
  (d/index-range database attr start end))

(defn ^:export squuid []
  (str (d/squuid)))

(defn ^:export squuid_time_millis
  [uuid-string]
  #?(:native
     (d/squuid-time-millis (uuid uuid-string))
     :melange
     (*
      (Lg_runtime.Runtime_string.parse_float_radix
       (subs uuid-string 0 8)
       16)
      1000.0)))
