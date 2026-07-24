(ns datascript.serialize
  (:refer-clojure :exclude [amap array?])
  (:require
    [clojure.string :as str]
    [datascript.db :as db #?@(:cljs [:refer [Datom]])]
    [datascript.storage :as storage]
    [me.tonsky.persistent-sorted-set :as set]
    [me.tonsky.persistent-sorted-set.arrays :as arrays]))

(type-alias serialized-value
  :Datascript_runtime.Serialization_value.t)

(defn- amap [f xs]
  (mapv f xs))

(defn- amap-indexed [f xs]
  (vec (map-indexed f xs)))

(defn- all-attrs
  "All attrs in a DB, distinct, sorted"
  [^datascript.db/DB db]
  (vec
   (distinct
    (map (fn [^datascript.db/Datom datom] (.-a datom))
         (:aevt db)))))

(defn ^:string freeze-kw [^:keyword kw]
  (str kw))

(defn- thaw-kw [^:string s]
  (keyword
   (if (str/starts-with? s ":")
     (subs s 1)
     s)))

(defn- ^serialized-value serialize-datom
  [^:Datascript_runtime.Serialization_value.encoder encoder
   ^:vector<string> attrs
   ^:int idx
   ^datascript.db/Datom datom]
  (db/datom-set-idx datom idx)
  (let [entity    (.-e datom)
        attribute (Datascript_runtime.Serialization_value.attribute_index
                   attrs
                   (freeze-kw (.-a datom)))
        value     (Datascript_runtime.Serialization_value.encode_value
                   encoder
                   (.-v datom))
        tx        (- (.-tx datom) db/tx0)]
    (Datascript_runtime.Serialization_value.datom
     entity attribute value tx)))

(defn- ^:vector<string> freeze-attrs [^:vector<keyword> attrs]
  (mapv freeze-kw attrs))

(defn- ^:vector<serialized-value> serialize-eavt
  [^datascript.db/DB db
   ^:Datascript_runtime.Serialization_value.encoder encoder
   ^:vector<string> attrs]
  (vec
   (map-indexed
    (fn [idx ^datascript.db/Datom datom]
      (serialize-datom encoder attrs idx datom))
    (:eavt db))))

(defn- ^:vector<int> datom-indexes
  [^:set/btset<datascript.db/Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> datoms]
  (mapv
   (fn [^datascript.db/Datom datom]
     (db/datom-get-idx datom))
   datoms))

(defn- ^serialized-value serializable-impl
  "Serialized structure breakdown:

   count    :: number    
   tx0      :: number
   max-eid  :: number
   max-tx   :: number
   schema   :: freezed :schema
   attrs    :: [keywords ...]
   keywords :: [keywords ...]
   eavt     :: [[e a-idx v dtx] ...]
   a-idx    :: index in attrs
   v        :: (string | number | boolean | [0 <index in keywords>] | [1 <freezed v>])
  dtx      :: tx - tx0
  aevt     :: [<index in eavt> ...]
  avet     :: [<index in eavt> ...]"
  [^datascript.db/DB db]
  (let [attrs       (all-attrs db)
        frozen-attrs (freeze-attrs attrs)
        encoder     (Datascript_runtime.Serialization_value.create_encoder)
        eavt        (serialize-eavt db encoder frozen-attrs)
        aevt        (datom-indexes (:aevt db))
        avet        (datom-indexes (:avet db))
        schema      (Datascript_runtime.Serialization_value.schema_to_string
                     (:schema db))
        kws         (Datascript_runtime.Serialization_value.encoder_keywords
                     encoder)]
    (Datascript_runtime.Serialization_value.database
     (count (:eavt db))
     db/tx0
     (:max-eid db)
     (:max-tx db)
     schema
     frozen-attrs
     kws
     eavt
     (Some aevt)
     (Some avet)
     32
     (Datascript_runtime.Storage_value.Strong))))

(defn ^serialized-value serializable [db]
  (serializable-impl db))

(defn- ^datascript.db/Datom deserialize-datom
  [^:int tx0
   ^:vector<keyword> attrs
   ^:vector<string> keywords
   ^serialized-value datom]
  (let [entity    (Datascript_runtime.Serialization_value.datom_entity datom)
        attribute (nth attrs
                       (Datascript_runtime.Serialization_value.datom_attribute
                        datom))
        value     (Datascript_runtime.Serialization_value.decode_value
                   keywords
                   (Datascript_runtime.Serialization_value.datom_value datom))
        tx        (+ tx0
                     (Datascript_runtime.Serialization_value.datom_tx datom))]
    (db/datom entity attribute value tx)))

(defn- ^:array<datascript.db/Datom> deserialize-datoms
  [^:int tx0
   ^:vector<keyword> attrs
   ^:vector<string> keywords
   ^:vector<serialized-value> datoms]
  (arrays/into-array
   (mapv
    (fn [^serialized-value datom]
      (deserialize-datom tx0 attrs keywords datom))
    datoms)))

(defn- ^:array<datascript.db/Datom> reorder-datoms
  [^:array<datascript.db/Datom> datoms
   ^:option<vector<int>> indexes]
  (match indexes
    (Some indexes)
    (arrays/into-array
     (mapv
      (fn [^:int index]
        (arrays/aget datoms index))
      indexes))
    None datoms))

(defn ^datascript.db/DB from-serializable
  [^serialized-value from]
  (let [tx0      (Datascript_runtime.Serialization_value.tx0 from)
         schema   (Datascript_runtime.Serialization_value.schema_of_string
                   (Datascript_runtime.Serialization_value.schema_source from))
         _        (db/validate-schema schema)
         attrs    (->> (Datascript_runtime.Serialization_value.attrs from)
                       (mapv thaw-kw))
         keywords (Datascript_runtime.Serialization_value.keywords from)
         eavt     (deserialize-datoms
                   tx0 attrs keywords
                   (Datascript_runtime.Serialization_value.datoms from))
         aevt     (reorder-datoms
                   eavt
                   (Datascript_runtime.Serialization_value.aevt from))
         avet     (reorder-datoms
                   eavt
                   (Datascript_runtime.Serialization_value.avet from))
         _        (Datascript_runtime.Serialization_value.branching_factor from)
         ref-type (Datascript_runtime.Serialization_value.ref_type from)]
    (db/restore-db
     (db/make-db-snapshot
      schema
      (set/from-sorted-array db/cmp-datoms-eavt eavt
       (arrays/alength eavt) None ref-type)
      (set/from-sorted-array db/cmp-datoms-aevt aevt
       (arrays/alength aevt) None ref-type)
      (set/from-sorted-array db/cmp-datoms-avet avet
       (arrays/alength avet) None ref-type)
      (Datascript_runtime.Serialization_value.max_eid from)
      (Datascript_runtime.Serialization_value.max_tx from)))))
