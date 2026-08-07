(ns ^:no-doc datascript.impl.entity
  (:require
   [clojure.string :as str]
   [datascript.db :as db]))

(declare
 entity
 equiv-entity
 entity-reference-set-equal?
 hash-entity
 lookup-entity
 touch
 touch-entity
 datoms->cache
 entity-print-string)

(type-record EntityDatabase
  (view :datascript.db/database-view))

(type-record EntityState
  (touched :ref<bool>)
  (cache :ref<map<keyword;entityvalue>>))

(deftype Entity
  [^entitydatabase database
   ^int eid
   ^entitystate state]
  ILookup
  (-lookup [entity key]
    (lookup-entity entity key))
  (-lookup
   [entity key not-found]
   (match (lookup-entity entity key)
     (Some value) value
     None not-found))
  IAssociative
  (-contains-key? [entity key]
    (some? (lookup-entity entity key)))
  ISeqable
  (-seq [entity]
    (seq @(:cache (.-state (touch-entity entity)))))
  ICounted
  (-count [entity]
    (count @(:cache (.-state (touch-entity entity)))))
  IFn
  (-invoke [entity key]
    (lookup-entity entity key))
  (-invoke
   [entity key not-found]
   (match (lookup-entity entity key)
     (Some value) value
     None not-found))
  IEquiv
  (-equiv [left right]
    (equiv-entity left right))
  IHash
  (-hash [entity]
    (hash-entity entity)))

(deftype EntityReferenceSet
  [^:vector<option<entity>> items]
  ISeqable
  (-seq [_]
    (seq items))
  ICounted
  (-count [_]
    (count items))
  IEquiv
  (-equiv [left right]
    (entity-reference-set-equal? left right)))

(type-variant EntityValue
  (EntityScalar :Datascript_runtime.Data_value.t)
  (EntityReference :entity)
  (EntityReferences :entityreferenceset))

(signature datascript.impl.entity/entity
  :fn<datascript.db/database-view;Datascript_runtime.Data_value.entity_ref;option<datascript.impl.entity/Entity>>)

(signature datascript.impl.entity/datoms->cache
  :fn<datascript.db/database-view;seq<datascript.db/Datom>;map<keyword;datascript.impl.entity/EntityValue>>)

(signature datascript.impl.entity/touch-entity
  :fn<datascript.impl.entity/Entity;datascript.impl.entity/Entity>)

(signature datascript.impl.entity/touch
  :fn<option<datascript.impl.entity/Entity>;option<datascript.impl.entity/Entity>>)

(signature datascript.impl.entity/entity-reference-set
  :fn<vector<option<datascript.impl.entity/Entity>>;datascript.impl.entity/EntityReferenceSet>)

(defn entity-reference-option-member?
  [target values]
  (if-some [candidate (first values)]
    (if
      (match target
        (Some left)
        (match candidate
          (Some right) (equiv-entity left right)
          None false)
        None
        (match candidate
          (Some _) false
          None true))
      true
      (recur target (subvec values 1)))
    false))

(defn entity-reference-set
  [values]
  (EntityReferenceSet.
   (reduce
    (fn [result target]
      (if (entity-reference-option-member? target result)
        result
        (conj result target)))
    []
    values)))

(defn entity-reference-set-equal?
  [left right]
  (let [left-items (.-items left)
        right-items (.-items right)]
    (and
     (= (count left-items) (count right-items))
     (every?
      (fn [target]
        (entity-reference-option-member? target right-items))
      left-items))))

(defn entity-value-print-string
  [value]
  (match value
    (EntityScalar scalar)
    (Datascript_runtime.Data_value.to_edn_string scalar)
    (EntityReference target)
    (entity-print-string target)
    (EntityReferences targets)
    (str
     "#{"
     (str/join
      " "
      (mapv
       (fn [target]
         (if-some [target target]
           (entity-print-string target)
           "nil"))
       (.-items targets)))
     "}")))

(defn entity-print-string
  [entity]
  (let [values
        (assoc
         @(:cache (.-state entity))
         :db/id
         (EntityScalar
          (Datascript_runtime.Data_value.Int (.-eid entity))))
        entries
        (mapv
         (fn [attr]
           (str
            attr
            " "
            (entity-value-print-string
             (get values
                  attr
                  (EntityScalar
                   (Datascript_runtime.Data_value.Nil))))))
         (keys values))]
    (str "{" (str/join ", " entries) "}")))

(defmethod print-method Entity
  [entity writer]
  (Buffer.add_string writer (entity-print-string entity)))

(defn- entid
  [database entity-ref]
  (db/database-view-entid database entity-ref))

(defn entity
  [db entity-ref]
  (if-some [eid (entid db entity-ref)]
    (if (db/database-view-numeric-eid-exists? db eid)
      (let [value
            (Entity.
             (record EntityDatabase
               (view db))
             eid
             (record EntityState
               (touched (volatile! false))
               (cache (volatile! (hash-map)))))]
        (Some value))
      None)
    None))

(defn entity? [_]
  true)

(defn entity-database-view
  [entity]
  (:view (.-database entity)))

(defn equiv-entity [left right]
  (and
   (db/database-view-identical?
    (entity-database-view left)
    (entity-database-view right))
   (= (.-eid left) (.-eid right))))

(defn hash-entity [entity]
  (hash-combine
   (hash (.-eid entity))
   (hash
    (db/database-view-identity-hash
     (entity-database-view entity)))))

(defn entity-reference
  [db value]
  (if-some
    [entity-ref
     (Datascript_runtime.Data_value.entity_ref_value value)]
    (entity db entity-ref)
    None))

(defn raw-entity-attr
  [db attr datoms]
  (if (db/database-view-multival? db attr)
    (Datascript_runtime.Data_value.set_of_vector
     (mapv
      (fn [datom]
        (.-v datom))
      datoms))
    (if-some [datom (first datoms)]
      (.-v datom)
      (Datascript_runtime.Data_value.Nil))))

(defn entity-value
  [db attr value]
  (if (db/database-view-ref? db attr)
    (if (db/database-view-multival? db attr)
      (if-some
        [values (Datascript_runtime.Data_value.set_items value)]
        (Some
         (EntityReferences
          (entity-reference-set
           (mapv
            (fn [item]
              (entity-reference db item))
            values))))
        (Stdlib.invalid_arg "Reference collection must be a set"))
      (if-some [target (entity-reference db value)]
        (Some (EntityReference target))
        None))
    (Some (EntityScalar value))))

(defn entity-attr
  [db attr datoms]
  (entity-value db attr (raw-entity-attr db attr datoms)))

(defn- -lookup-backwards
  [db eid attr]
  (let [datoms
        (db/database-view-search
         db
         None
         (Some attr)
         (Some (Datascript_runtime.Data_value.Ref eid))
         None)]
    (if (empty? datoms)
      None
      (if (db/database-view-component? db attr)
        (if-some [datom (first datoms)]
          (if-some
            [target
             (entity
              db
              (Datascript_runtime.Data_value.Entity_id (.-e datom)))]
            (Some (EntityReference target))
            None)
          None)
        (Some
         (EntityReferences
          (entity-reference-set
           (mapv
            (fn [datom]
              (entity
               db
               (Datascript_runtime.Data_value.Entity_id (.-e datom))))
            datoms))))))))

(defn lookup-entity
  [entity attr]
  (if (= attr :db/id)
    (Some
     (EntityScalar
      (Datascript_runtime.Data_value.Int (.-eid entity))))
    (let [database (entity-database-view entity)]
      (if (db/reverse-ref? attr)
        (-lookup-backwards
         database
         (.-eid entity)
         (db/reverse-ref attr))
        (let [state (.-state entity)
              cache (:cache state)]
          (if-some [value (get @cache attr)]
            (Some value)
            (if @(:touched state)
              None
              (if-some
                [datoms
                 (db/database-view-search
                  database
                  (Some (.-eid entity))
                  (Some attr)
                  None
                  None)]
                (if-some [value (entity-attr database attr datoms)]
                  (do
                    (vreset! cache (assoc @cache attr value))
                    (Some value))
                  None)
                None))))))))

(defn datoms->cache
  [db datoms]
  (reduce
   (fn [result part]
     (if-some [datom (first part)]
       (let [attr (.-a datom)]
         (if-some [value (entity-attr db attr (vec part))]
           (assoc result attr value)
           result))
       result))
   (hash-map)
   (partition-by
    (fn [datom]
      (.-a datom))
    datoms)))

(defn touch-components [db values]
  (reduce-kv
   (fn [result attr value]
     (assoc
      result
      attr
      (if (db/database-view-component? db attr)
        (match value
          (EntityReference target)
          (EntityReference (touch-entity target))
          (EntityReferences targets)
          (EntityReferences
           (entity-reference-set
            (mapv touch (.-items targets))))
          _ value)
        value)))
   {}
   values))

(defn touch-entity [entity]
  (let [database (entity-database-view entity)]
    (when-not @(:touched (.-state entity))
      (let [datoms
          (db/database-view-search
           database
           (Some (.-eid entity))
           None
           None
           None)]
        (if-some [values datoms]
          (vreset!
           (:cache (.-state entity))
           (touch-components
            database
            (datoms->cache database values)))
          (vreset! (:cache (.-state entity)) (hash-map)))
        (vreset! (:touched (.-state entity)) true))))
  entity)

(defn touch
  [entity]
  (match entity
    None None
    (Some value) (Some (touch-entity value))))
