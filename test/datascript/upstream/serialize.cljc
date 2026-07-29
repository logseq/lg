(ns datascript.serialize
  (:refer-clojure :exclude [amap array?])
  (:require
    [clojure.string :as str]
    [datascript.db :as db #?@(:cljs [:refer [Datom]])]
    [datascript.storage :as storage]
    [me.tonsky.persistent-sorted-set :as set]
    [me.tonsky.persistent-sorted-set.arrays :as arrays]))

(type-alias serialized-value
  :Lg_edn_backend.t)

(type-alias prepared-serialized-value
  :Datascript_runtime.Serialization_value.prepared)

(type-alias prepared-serialized-datom
  :Datascript_runtime.Serialization_value.prepared_datom)

(type-alias codec-function
  :fn<serialized-value;serialized-value>)

(type-variant codec
  DefaultCodec
  (CustomCodec :codec-function))

(type-variant keyword-freezer
  DefaultKeywordFreezer
  (CustomKeywordFreezer :fn<keyword;string>))

(type-variant keyword-thawer
  DefaultKeywordThawer
  (CustomKeywordThawer :fn<string;keyword>))

(type-variant restore-ref-type
  SerializedRefType
  (OverrideRefType :Lg_runtime.Runtime_ref_type.t))

(defn- amap [f xs]
  (mapv f xs))

(defn- amap-indexed [f xs]
  (vec (map-indexed f xs)))

(defn- all-attrs
  "All attrs in a DB, distinct, sorted"
  [^datascript.db/DB db]
  (vec
   (distinct
    (map (fn [datom] (.-a datom))
         (:aevt db)))))

(defn ^:string freeze-kw [^:keyword kw]
  (str kw))

(defn- thaw-kw [^:string s]
  (keyword
   (if (str/starts-with? s ":")
     (subs s 1)
     s)))

(defn- freeze-keyword-value
  [freezer value]
  (match freezer
    (CustomKeywordFreezer freeze-keyword) (freeze-keyword value)
    DefaultKeywordFreezer (freeze-kw value)))

(defn- thaw-keyword-value
  [thawer value]
  (match thawer
    (CustomKeywordThawer thaw-keyword) (thaw-keyword value)
    DefaultKeywordThawer (thaw-kw value)))

(defn- ^serialized-value serialize-datom
  [^:Datascript_runtime.Serialization_value.encoder encoder
   ^codec freeze-codec
   ^:vector<string> attrs
   ^:int idx
   ^datascript.db/Datom datom]
  (db/datom-set-idx datom idx)
  (let [entity    (.-e datom)
        attribute (Datascript_runtime.Serialization_value.attribute_index
                   attrs
                   (freeze-kw (.-a datom)))
        value     (match freeze-codec
                    (CustomCodec freeze-fn)
                    (Datascript_runtime.Serialization_value.encode_value_with
                     encoder freeze-fn (.-v datom))
                    DefaultCodec
                    (Datascript_runtime.Serialization_value.encode_value
                     encoder (.-v datom)))
        tx        (- (.-tx datom) db/tx0)]
    (Datascript_runtime.Serialization_value.datom
     entity attribute value tx)))

(defn- freeze-attrs
  [keyword-freezer attrs]
  (mapv
   (fn [attr]
     (freeze-keyword-value keyword-freezer attr))
   attrs))

(defn- serialize-eavt
  [^datascript.db/DB db encoder freeze-codec attrs]
  (let [datoms (:eavt db)]
    (if-some [first-datom (first datoms)]
      (let [result
            (arrays/make-array
             (count datoms)
             (serialize-datom
              encoder freeze-codec attrs 0 first-datom))]
        (reduce
         (fn [index datom]
           (if (> index 0)
             (arrays/aset
              result index
              (serialize-datom encoder freeze-codec attrs index datom))
             (Stdlib.ignore 0))
           (inc index))
         0
         datoms)
        result)
      (arrays/empty-array))))

(defn- datom-indexes
  [^:set/btset<datascript.db/Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> datoms]
  (let [result (arrays/make-array (count datoms) 0)]
    (reduce
     (fn [index datom]
       (arrays/aset result index (db/datom-get-idx datom))
       (inc index))
     0
     datoms)
    result))

(defn- serialized-ref-type
  [ref-type]
  (match ref-type
    (Lg_runtime.Runtime_ref_type.Strong)
    (Datascript_runtime.Storage_value.Strong)
    (Lg_runtime.Runtime_ref_type.Weak)
    (Datascript_runtime.Storage_value.Weak)))

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
  [^datascript.db/DB db freeze-codec keyword-freezer]
  (when-some [_database-storage (storage/storage db)]
    (Stdlib.invalid_arg
     "serializable doesn't work with databases that have :storage"))
  (let [attrs       (all-attrs db)
        frozen-attrs (freeze-attrs keyword-freezer attrs)
        encoder     (Datascript_runtime.Serialization_value.create_encoder)
        eavt        (serialize-eavt db encoder freeze-codec frozen-attrs)
        aevt        (datom-indexes (:aevt db))
        avet        (datom-indexes (:avet db))
        settings    (set/settings (:eavt db))
        kws         (mapv
                     (fn [keyword-source]
                       (freeze-keyword-value
                        keyword-freezer (keyword keyword-source)))
                     (Datascript_runtime.Serialization_value.encoder_keywords
                      encoder))]
    (match freeze-codec
      (CustomCodec freeze-fn)
      (Datascript_runtime.Serialization_value.database_arrays_with_schema
       (count (:eavt db))
       db/tx0
       (:max-eid db)
       (:max-tx db)
       (freeze-fn
        (Datascript_runtime.Serialization_value.schema_to_value
         (:schema db)))
       frozen-attrs
       kws
       eavt
       (Some aevt)
       (Some avet)
       (:branching-factor settings)
       (serialized-ref-type (:ref-type settings)))
      DefaultCodec
      (Datascript_runtime.Serialization_value.database_arrays
       (count (:eavt db))
       db/tx0
       (:max-eid db)
       (:max-tx db)
       (Datascript_runtime.Serialization_value.schema_to_string
        (:schema db))
       frozen-attrs
       kws
       eavt
       (Some aevt)
       (Some avet)
       (:branching-factor settings)
       (serialized-ref-type (:ref-type settings))))))

(defn ^serialized-value serializable
  ([db]
   (serializable-impl db DefaultCodec DefaultKeywordFreezer))
  ([db {:keys [freeze-fn freeze-kw]}]
   (let [freeze-codec
         (if-some [freeze-fn freeze-fn]
           (CustomCodec freeze-fn)
           DefaultCodec)
         freeze-keyword
         (if-some [freeze-keyword freeze-kw]
           (CustomKeywordFreezer freeze-keyword)
           DefaultKeywordFreezer)]
     (serializable-impl db freeze-codec freeze-keyword))))

(defn- ^datascript.db/Datom deserialize-datom
  [^:int tx0
   ^:vector<keyword> attrs
   ^:vector<string> keywords
   ^codec thaw-codec
   ^prepared-serialized-datom datom]
  (let [entity    (Datascript_runtime.Serialization_value.prepared_datom_entity
                   datom)
        attribute (nth attrs
                       (Datascript_runtime.Serialization_value.prepared_datom_attribute
                        datom))
        value     (match thaw-codec
                    (CustomCodec thaw-fn)
                    (Datascript_runtime.Serialization_value.decode_value_with
                     thaw-fn
                     keywords
                     (Datascript_runtime.Serialization_value.prepared_datom_value
                      datom))
                    DefaultCodec
                    (Datascript_runtime.Serialization_value.decode_value
                     keywords
                     (Datascript_runtime.Serialization_value.prepared_datom_value
                      datom)))
        tx        (+ tx0
                     (Datascript_runtime.Serialization_value.prepared_datom_tx
                      datom))]
    (db/datom entity attribute value tx)))

(defn- ^:array<datascript.db/Datom> deserialize-datoms
  [^:int tx0
   ^:vector<keyword> attrs
   ^:vector<string> keywords
   ^codec thaw-codec
   ^:array<prepared-serialized-datom> datoms]
  (arrays/amap
   (fn [datom]
     (deserialize-datom tx0 attrs keywords thaw-codec datom))
   datoms))

(defn- ^:array<datascript.db/Datom> reorder-datoms
  [^:array<datascript.db/Datom> datoms
   ^:option<array<int>> indexes]
  (match indexes
    (Some indexes)
    (arrays/amap
     (fn [index]
       (arrays/aget datoms index))
     indexes)
    None datoms))

(defn- ^datascript.db/DB from-serializable-impl
  [^serialized-value from
   ^codec thaw-codec
   ^keyword-thawer keyword-thawer
   ^restore-ref-type restore-ref-type
   branching-factor-override]
  (let [prepared (Datascript_runtime.Serialization_value.prepare from)
        tx0      (Datascript_runtime.Serialization_value.prepared_tx0 prepared)
         schema   (match thaw-codec
                    (CustomCodec thaw-fn)
                    (Datascript_runtime.Serialization_value.schema_of_value
                     (thaw-fn
                      (Datascript_runtime.Serialization_value.prepared_schema_value
                       prepared)))
                    DefaultCodec
                    (Datascript_runtime.Serialization_value.schema_of_string
                     (Datascript_runtime.Serialization_value.prepared_schema_source
                      prepared)))
         _        (when-some [schema-map schema]
                    (db/validate-schema schema-map))
         attrs    (->> (Datascript_runtime.Serialization_value.prepared_attrs
                        prepared)
                       (mapv
                        (fn [value]
                          (thaw-keyword-value keyword-thawer value))))
         keywords (->> (Datascript_runtime.Serialization_value.prepared_keywords
                        prepared)
                       (mapv
                        (fn [value]
                          (str
                           (thaw-keyword-value keyword-thawer value)))))
         eavt     (deserialize-datoms
                   tx0 attrs keywords thaw-codec
                   (Datascript_runtime.Serialization_value.prepared_datoms_array
                    prepared))
         aevt     (reorder-datoms
                   eavt
                   (Datascript_runtime.Serialization_value.prepared_aevt_array
                    prepared))
         avet     (reorder-datoms
                   eavt
                   (Datascript_runtime.Serialization_value.prepared_avet_array
                    prepared))
         serialized-branching-factor
         (Datascript_runtime.Serialization_value.prepared_branching_factor
          prepared)
         branching-factor
         (if-some [branching-factor branching-factor-override]
           branching-factor
           serialized-branching-factor)
         serialized-ref-type
         (Datascript_runtime.Serialization_value.prepared_ref_type prepared)
         ref-type (match restore-ref-type
                    (OverrideRefType ref-type) ref-type
                    SerializedRefType serialized-ref-type)]
    (db/restore-db
     (db/make-db-snapshot
      schema
      (set/with-branching-factor
       (set/from-sorted-array db/cmp-datoms-eavt eavt
        (arrays/alength eavt) None ref-type)
       branching-factor)
      (set/with-branching-factor
       (set/from-sorted-array db/cmp-datoms-aevt aevt
        (arrays/alength aevt) None ref-type)
       branching-factor)
      (set/with-branching-factor
       (set/from-sorted-array db/cmp-datoms-avet avet
        (arrays/alength avet) None ref-type)
       branching-factor)
      (Datascript_runtime.Serialization_value.prepared_max_eid prepared)
      (Datascript_runtime.Serialization_value.prepared_max_tx prepared)))))

(defn ^datascript.db/DB from-serializable
  ([^serialized-value from]
   (from-serializable-impl
    from DefaultCodec DefaultKeywordThawer SerializedRefType None))
  ([^serialized-value from
    {:keys [thaw-fn thaw-kw ref-type branching-factor]}]
   (let [thaw-codec
         (if-some [thaw-fn thaw-fn]
           (CustomCodec thaw-fn)
           DefaultCodec)
         thaw-keyword
         (if-some [thaw-keyword thaw-kw]
           (CustomKeywordThawer thaw-keyword)
           DefaultKeywordThawer)
         restore-ref-type
         (if-some [ref-type ref-type]
           (OverrideRefType ref-type)
           SerializedRefType)]
     (from-serializable-impl
      from thaw-codec thaw-keyword restore-ref-type branching-factor))))
