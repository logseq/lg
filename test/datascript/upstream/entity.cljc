(ns ^:no-doc datascript.impl.entity
  (:require [datascript.db :as db]))

(declare entity touch touch-entity datoms->cache)

(type-record Entity
  (db :datascript.db/DB)
  (eid :int)
  (touched :ref<bool>)
  (cache :ref<map<keyword;Datascript_runtime.Data_value.t>>))

(defn ^:option<datascript.impl.entity/Entity> entity
  [^datascript.db/DB db
   ^:Datascript_runtime.Data_value.entity_ref entity-ref]
  (if-some [eid (db/entid db entity-ref)]
    (if (db/numeric-eid-exists? db eid)
      (Some
       (record Entity
         (db db)
         (eid eid)
         (touched (volatile! false))
         (cache (volatile! (hash-map)))))
      None)
    None))

(defn ^boolean entity? [^Entity _]
  true)

(defn ^boolean equiv-entity [^Entity left ^Entity right]
  (and
   (identical? (.-db left) (.-db right))
   (= (.-eid left) (.-eid right))))

(defn ^int hash-entity [^Entity entity]
  (hash-combine
   (hash (.-eid entity))
   (hash (.-max-tx (.-db entity)))))

(defn ^:Datascript_runtime.Data_value.t entity-attr
  [^datascript.db/DB db
   ^:keyword attr
   ^:vector<datascript.db/Datom> datoms]
  (if (db/multival? db attr)
    (Datascript_runtime.Data_value.set_of_vector
     (mapv
      (fn [^datascript.db/Datom datom]
        (.-v datom))
      datoms))
    (if-some [datom (first datoms)]
      (.-v datom)
      (Datascript_runtime.Data_value.Nil))))

(defn ^:option<Datascript_runtime.Data_value.t> lookup-backwards
  [^datascript.db/DB db ^int eid ^:keyword attr]
  (let [datoms
        (db/-search
         db
         None
         (Some attr)
         (Some (Datascript_runtime.Data_value.Ref eid))
         None)]
    (if (empty? datoms)
      None
      (if (db/component? db attr)
        (if-some [datom (first datoms)]
          (Some (Datascript_runtime.Data_value.Ref (.-e datom)))
          None)
        (Some
         (Datascript_runtime.Data_value.set_of_vector
          (mapv
           (fn [^datascript.db/Datom datom]
             (Datascript_runtime.Data_value.Ref (.-e datom)))
           datoms)))))))

(defn ^:option<Datascript_runtime.Data_value.t> lookup-entity
  [^Entity entity ^:keyword attr]
  (let [^datascript.db/DB database (.-db entity)]
    (if (= attr :db/id)
      (Some (Datascript_runtime.Data_value.Int (.-eid entity)))
      (if (db/reverse-ref? attr)
        (lookup-backwards
         database
         (.-eid entity)
         (db/reverse-ref attr))
        (if-some [value (get @(:cache entity) attr)]
          (Some value)
          (if @(:touched entity)
            None
            (let [datoms
                  (db/-search
                   database
                   (Some (.-eid entity))
                   (Some attr)
                   None
                   None)]
              (if (empty? datoms)
                None
                (let [value (entity-attr database attr (vec datoms))]
                  (vreset!
                   (:cache entity)
                   (assoc @(:cache entity) attr value))
                  (Some value))))))))))

(defn ^:map<keyword;Datascript_runtime.Data_value.t> datoms->cache
  [^datascript.db/DB db ^:seq<datascript.db/Datom> datoms]
  (reduce
   (fn [^:map<keyword;Datascript_runtime.Data_value.t> result
        ^:list<datascript.db/Datom> part]
     (if-some [datom (first part)]
       (let [attr (.-a ^datascript.db/Datom datom)]
         (assoc result attr (entity-attr db attr (vec part))))
       result))
   (hash-map)
   (partition-by
    (fn [^datascript.db/Datom datom]
      (.-a datom))
    datoms)))

(defn ^Entity touch-entity [^Entity entity]
  (let [^datascript.db/DB database (.-db entity)]
    (when-not @(:touched entity)
      (let [datoms
          (db/-search
           database
           (Some (.-eid entity))
           None
           None
           None)]
        (if-some [values datoms]
          (vreset! (:cache entity)
                   (datoms->cache database values))
          (vreset! (:cache entity) (hash-map)))
        (vreset! (:touched entity) true))))
  entity)

(defn ^:option<datascript.impl.entity/Entity> touch
  [^:option<datascript.impl.entity/Entity> entity]
  (match entity
    None None
    (Some value) (Some (touch-entity value))))
