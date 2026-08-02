(ns datascript.datafy
  (:require
   [datascript.db :as db]
   [datascript.impl.entity :as entity]
   [datascript.pull-api :as pull-api]
   [datascript.pull-parser :as pull-parser]))

(type-variant navigation-key
  (NavigationAttribute :keyword)
  (NavigationIndex :int))

(type-record datafied-entity
  (database :datascript.db/database-view)
  (values
   :map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>))

(type-record datafied-entities
  (database :datascript.db/database-view)
  (values :vector<Datascript_runtime.Data_value.t>))

(type-variant navigation-value
  NavigationMissing
  (NavigationEntity :datascript.impl.entity/Entity)
  (NavigationEntities
   :datascript.db/database-view
   :vector<Datascript_runtime.Data_value.t>)
  (NavigationDatafiedEntity :datascript.datafy/datafied-entity)
  (NavigationDatafiedEntities :datascript.datafy/datafied-entities)
  (NavigationScalar :Datascript_runtime.Data_value.t))

(signature datascript.datafy/entity-navigation
  :fn<datascript.impl.entity/Entity;datascript.datafy/navigation-value>)

(defn entity-navigation
  [value]
  (NavigationEntity value))

(defn- entity-pull-pattern
  [database]
  (let [unfiltered (db/database-view-unfiltered-db database)
        ref-attrs (:ref-attrs (.-rschema unfiltered))]
    (into
     [pull-parser/source-wildcard]
     (mapv
      (fn [attr]
        (pull-parser/source-attribute (db/reverse-ref attr)))
      ref-attrs))))

(defn- datafy-entity
  [value]
  (let [database (entity/entity-database-view value)
        values
        (pull-api/pull-source
         database
         (entity-pull-pattern database)
         (Datascript_runtime.Data_value.Entity_id (.-eid value)))]
    (record datafied-entity
      (database database)
      (values
       (match values
         (Some pulled) pulled
         None {})))))

(signature datascript.datafy/datafy
  :fn<datascript.datafy/navigation-value;datascript.datafy/navigation-value>)

(defn datafy
  [value]
  (match value
    (NavigationEntity source)
    (NavigationDatafiedEntity (datafy-entity source))
    (NavigationEntities database values)
    (NavigationDatafiedEntities
     (record datafied-entities
       (database database)
       (values values)))
    _ value))

(defn- data-value-navigation
  [value]
  (match value
    (Some scalar) (NavigationScalar scalar)
    None NavigationMissing))

(signature datascript.datafy/lookup
  :fn<datascript.datafy/navigation-value;datascript.datafy/navigation-key;datascript.datafy/navigation-value>)

(defn lookup
  [value
   key]
  (match value
    (NavigationDatafiedEntity source)
    (match key
      (NavigationAttribute attr)
      (data-value-navigation
       (get
        (:values source)
        (Datascript_runtime.Data_value.Keyword (str attr))))
      _ NavigationMissing)
    (NavigationDatafiedEntities source)
    (match key
      (NavigationIndex index)
      (if (and (>= index 0) (< index (count (:values source))))
        (NavigationScalar (nth (:values source) index))
        NavigationMissing)
      _ NavigationMissing)
    _ NavigationMissing))

(defn- pulled-entity-ref
  [pulled]
  (if-some
    [value
     (Datascript_runtime.Data_value.map_get
      pulled
      (Datascript_runtime.Data_value.Keyword ":db/id"))]
    (Datascript_runtime.Data_value.entity_ref_value value)
    None))

(defn- entity-from-pulled
  [database pulled]
  (if-some [entity-ref (pulled-entity-ref pulled)]
    (if-some [value (entity/entity database entity-ref)]
      (NavigationEntity value)
      NavigationMissing)
    NavigationMissing))

(defn- navize-entity-value
  [source attr value]
  (let [database (:database source)]
    (cond
      (or
       (and
        (db/database-view-multival? database attr)
        (db/database-view-ref? database attr))
       (db/reverse-ref? attr))
      (if-some
        [values (Datascript_runtime.Data_value.sequential_items value)]
        (NavigationEntities database values)
        NavigationMissing)

      (db/database-view-ref? database attr)
      (entity-from-pulled database value)

      :else
      (NavigationScalar value))))

(signature datascript.datafy/nav
  :fn<datascript.datafy/navigation-value;datascript.datafy/navigation-key;datascript.datafy/navigation-value;datascript.datafy/navigation-value>)

(defn nav
  [datafied
   key
   value]
  (match datafied
    (NavigationDatafiedEntity source)
    (match key
      (NavigationAttribute attr)
      (match value
        (NavigationScalar scalar)
        (navize-entity-value source attr scalar)
        _ value)
      _ value)
    (NavigationDatafiedEntities source)
    (match key
      (NavigationIndex _)
      (match value
        (NavigationScalar pulled)
        (entity-from-pulled (:database source) pulled)
        _ value)
      _ value)
    _ value))

(signature datascript.datafy/pulled-entity-id
  :fn<Datascript_runtime.Data_value.t;option<int>>)

(defn- pulled-entity-id
  [pulled]
  (if-some [entity-ref (pulled-entity-ref pulled)]
    (match entity-ref
      (Datascript_runtime.Data_value.Entity_id eid) (Some eid)
      _ None)
    None))

(signature datascript.datafy/navigation-entity-id
  :fn<datascript.datafy/navigation-value;option<int>>)

(defn navigation-entity-id
  [value]
  (match value
    (NavigationEntity source) (Some (.-eid source))
    (NavigationDatafiedEntity source)
    (pulled-entity-id
     (Datascript_runtime.Data_value.map_of_data_map (:values source)))
    _ None))

(signature datascript.datafy/empty-pulled-entities
  :fn<vector<Datascript_runtime.Data_value.t>>)

(defn- empty-pulled-entities []
  [])

(signature datascript.datafy/empty-entity-ids
  :fn<vector<int>>)

(defn- empty-entity-ids []
  [])

(signature datascript.datafy/navigation-entity-ids
  :fn<datascript.datafy/navigation-value;vector<int>>)

(defn navigation-entity-ids
  [value]
  (let [values
        (match value
          (NavigationEntities _ values) values
          (NavigationDatafiedEntities source) (:values source)
          _ (empty-pulled-entities))]
    (reduce
     (fn [result pulled]
       (if-some [eid (pulled-entity-id pulled)]
         (conj result eid)
         result))
     (empty-entity-ids)
     values)))

(signature datascript.datafy/navigation-scalar
  :fn<datascript.datafy/navigation-value;option<Datascript_runtime.Data_value.t>>)

(defn navigation-scalar
  [value]
  (match value
    (NavigationScalar scalar) (Some scalar)
    _ None))
