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
    (map (fn [^datascript.db/Datom datom] (.-a datom))
         (:aevt db)))))

(defn ^:string freeze-kw [^:keyword kw]
  (str kw))

(defn- thaw-kw [^:string s]
  (keyword
   (if (str/starts-with? s ":")
     (subs s 1)
     s)))

(defn- ^:string freeze-keyword-value
  [^keyword-freezer freezer ^:keyword value]
  (match freezer
    (CustomKeywordFreezer freeze-keyword) (freeze-keyword value)
    DefaultKeywordFreezer (freeze-kw value)))

(defn- ^:keyword thaw-keyword-value
  [^keyword-thawer thawer ^:string value]
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

(defn- ^:vector<string> freeze-attrs
  [^keyword-freezer keyword-freezer
   ^:vector<keyword> attrs]
  (mapv
   (fn [^:keyword attr]
     (freeze-keyword-value keyword-freezer attr))
   attrs))

(defn- ^:vector<serialized-value> serialize-eavt
   [^datascript.db/DB db
   ^:Datascript_runtime.Serialization_value.encoder encoder
   ^codec freeze-codec
   ^:vector<string> attrs]
  (vec
   (map-indexed
    (fn [idx ^datascript.db/Datom datom]
      (serialize-datom encoder freeze-codec attrs idx datom))
    (:eavt db))))

(defn- ^:vector<int> datom-indexes
  [^:set/btset<datascript.db/Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> datoms]
  (mapv
   (fn [^datascript.db/Datom datom]
     (db/datom-get-idx datom))
   datoms))

(defn- ^:Datascript_runtime.Storage_value.ref_type serialized-ref-type
  [^:Lg_runtime.Runtime_ref_type.t ref-type]
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
  [^datascript.db/DB db
   ^codec freeze-codec
   ^keyword-freezer keyword-freezer]
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
                     (fn [^:string keyword-source]
                       (freeze-keyword-value
                        keyword-freezer (keyword keyword-source)))
                     (Datascript_runtime.Serialization_value.encoder_keywords
                      encoder))]
    (match freeze-codec
      (CustomCodec freeze-fn)
      (Datascript_runtime.Serialization_value.database_with_schema
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
      (Datascript_runtime.Serialization_value.database
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
   ^serialized-value datom]
  (let [entity    (Datascript_runtime.Serialization_value.datom_entity datom)
        attribute (nth attrs
                       (Datascript_runtime.Serialization_value.datom_attribute
                        datom))
        value     (match thaw-codec
                    (CustomCodec thaw-fn)
                    (Datascript_runtime.Serialization_value.decode_value_with
                     thaw-fn
                     keywords
                     (Datascript_runtime.Serialization_value.datom_value datom))
                    DefaultCodec
                    (Datascript_runtime.Serialization_value.decode_value
                     keywords
                     (Datascript_runtime.Serialization_value.datom_value datom)))
        tx        (+ tx0
                     (Datascript_runtime.Serialization_value.datom_tx datom))]
    (db/datom entity attribute value tx)))

(defn- ^:array<datascript.db/Datom> deserialize-datoms
  [^:int tx0
   ^:vector<keyword> attrs
   ^:vector<string> keywords
   ^codec thaw-codec
   ^:array<serialized-value> datoms]
  (arrays/amap
   (fn [^serialized-value datom]
     (deserialize-datom tx0 attrs keywords thaw-codec datom))
   datoms))

(defn- ^:array<datascript.db/Datom> reorder-datoms
  [^:array<datascript.db/Datom> datoms
   ^:option<array<int>> indexes]
  (match indexes
    (Some indexes)
    (arrays/amap
     (fn [^:int index]
       (arrays/aget datoms index))
     indexes)
    None datoms))

(defn- ^datascript.db/DB from-serializable-impl
  [^serialized-value from
   ^codec thaw-codec
   ^keyword-thawer keyword-thawer
   ^restore-ref-type restore-ref-type]
  (let [tx0      (Datascript_runtime.Serialization_value.tx0 from)
         schema   (match thaw-codec
                    (CustomCodec thaw-fn)
                    (Datascript_runtime.Serialization_value.schema_of_value
                     (thaw-fn
                      (Datascript_runtime.Serialization_value.schema_value from)))
                    DefaultCodec
                    (Datascript_runtime.Serialization_value.schema_of_string
                     (Datascript_runtime.Serialization_value.schema_source from)))
         _        (when-some [schema-map schema]
                    (db/validate-schema schema-map))
         attrs    (->> (Datascript_runtime.Serialization_value.attrs from)
                       (mapv
                        (fn [^:string value]
                          (thaw-keyword-value keyword-thawer value))))
         keywords (->> (Datascript_runtime.Serialization_value.keywords from)
                       (mapv
                        (fn [^:string value]
                          (str
                           (thaw-keyword-value keyword-thawer value)))))
         eavt     (deserialize-datoms
                   tx0 attrs keywords thaw-codec
                   (Datascript_runtime.Serialization_value.datoms_array from))
         aevt     (reorder-datoms
                   eavt
                   (Datascript_runtime.Serialization_value.aevt_array from))
         avet     (reorder-datoms
                   eavt
                   (Datascript_runtime.Serialization_value.avet_array from))
         _        (Datascript_runtime.Serialization_value.branching_factor from)
         serialized-ref-type
         (Datascript_runtime.Serialization_value.ref_type from)
         ref-type (match restore-ref-type
                    (OverrideRefType ref-type) ref-type
                    SerializedRefType serialized-ref-type)]
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

(defn ^datascript.db/DB from-serializable
  ([^serialized-value from]
   (from-serializable-impl
    from DefaultCodec DefaultKeywordThawer SerializedRefType))
  ([^serialized-value from {:keys [thaw-fn thaw-kw ref-type]}]
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
      from thaw-codec thaw-keyword restore-ref-type))))
