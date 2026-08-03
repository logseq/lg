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

(defn- attr-comparator
  "Looks for a datom with an attribute exactly bigger than the given one."
  [left right]
  (let [left-attr (db/datom-attr left)
        right-attr (db/datom-attr right)]
    (cond
      (= right-attr (keyword "")) -1
      (<= (compare left-attr right-attr) 0) -1
      :else 1)))

(signature datascript.serialize/all-attrs
  :fn<datascript.db/DB;vector<keyword>>)
(defn- all-attrs
  "All attrs in a DB, distinct, sorted"
  [db]
  (let [aevt (:aevt db)]
    (if (empty? aevt)
      []
      (loop [attrs [(db/datom-attr (first aevt))]]
        (let [attr (nth attrs (dec (count attrs)))
              left (db/datom-bound
                    None (Some attr) None None db/e0 db/tx0)
              right (db/datom-bound
                     None None None None db/emax db/txmax)
              next-attr
              (some->
               (first (set/slice aevt left right attr-comparator))
               db/datom-attr)]
          (if-some [next-attr next-attr]
            (recur (conj attrs next-attr))
            attrs))))))

(defn freeze-kw [kw]
  (str kw))

(defn- thaw-kw [s]
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

(defn- serialize-datom!
  [result
   encoder
   freeze-codec
   attrs-map
   idx
   datom]
  (set! (.-idx datom) idx)
  (let [entity    (.-e datom)
        attribute
        (Datascript_runtime.Serialization_value.find_attribute_index
         attrs-map (str (.-a datom)))
        value     (match freeze-codec
                    (CustomCodec freeze-fn)
                    (Datascript_runtime.Serialization_value.encode_value_with
                     encoder freeze-fn (.-v datom))
                    DefaultCodec
                    (Datascript_runtime.Serialization_value.encode_value
                     encoder (.-v datom)))
        tx        (- (.-tx datom) db/tx0)]
    (Datascript_runtime.Serialization_value.set_datom
     result idx entity attribute value tx)))

(defn- freeze-attrs
  [keyword-freezer attrs]
  (mapv
   (fn [attr]
     (freeze-keyword-value keyword-freezer attr))
   attrs))

(signature datascript.serialize/serialize-eavt
  :fn<datascript.db/DB;Datascript_runtime.Serialization_value.encoder;codec;Datascript_runtime.Serialization_value.attribute_indexes;Datascript_runtime.Serialization_value.datom_array>)
(defn- serialize-eavt
  [db encoder freeze-codec attrs-map]
  (let [datoms (:eavt db)
        result
        (Datascript_runtime.Serialization_value.create_datom_array
         (count datoms))]
    (reduce
     (fn [index datom]
       (serialize-datom!
        result encoder freeze-codec attrs-map index datom)
       (inc index))
     0
     datoms)
    result))

(signature datascript.serialize/datom-indexes
  :fn<set/btset<datascript.db/Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>>;array<int>>)
(defn- datom-indexes
  [datoms]
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

(signature datascript.serialize/serializable-impl
  :fn<datascript.db/DB;codec;keyword-freezer;serialized-value>)

(defn- serializable-impl
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
  [db freeze-codec keyword-freezer]
  (when-some [_database-storage (storage/storage db)]
    (Stdlib.invalid_arg
     "serializable doesn't work with databases that have :storage"))
  (let [attrs       (all-attrs db)
        attrs-map
        (Datascript_runtime.Serialization_value.create_attribute_indexes
         (mapv str attrs))
        frozen-attrs (freeze-attrs keyword-freezer attrs)
        encoder     (Datascript_runtime.Serialization_value.create_encoder)
        eavt        (serialize-eavt db encoder freeze-codec attrs-map)
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
      (Datascript_runtime.Serialization_value.database_datom_array_with_schema
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
      (Datascript_runtime.Serialization_value.database_datom_array
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

(defn serializable
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

(defn- deserialize-datom
  [tx0
   attrs
   keywords
   thaw-codec
   prepared
   cursor
   index]
  (Datascript_runtime.Serialization_value.read_prepared_datom_into
   prepared index cursor)
  (let [entity    (Datascript_runtime.Serialization_value.cursor_datom_entity
                   cursor)
        attribute (arrays/aget
                   attrs
                   (Datascript_runtime.Serialization_value.cursor_datom_attribute
                    cursor))
        value     (match thaw-codec
                    (CustomCodec thaw-fn)
                    (Datascript_runtime.Serialization_value.decode_value_with
                     thaw-fn
                     keywords
                     (Datascript_runtime.Serialization_value.cursor_datom_value
                      cursor))
                    DefaultCodec
                    (Datascript_runtime.Serialization_value.decode_cursor_datom_value
                     keywords cursor))
        tx        (+ tx0
                     (Datascript_runtime.Serialization_value.cursor_datom_tx
                      cursor))]
    (db/datom entity attribute value tx)))

(defn- deserialize-datoms
  [tx0
   attrs
   keywords
   thaw-codec
   prepared]
  (let [datom-count
        (Datascript_runtime.Serialization_value.prepared_datom_count prepared)
        cursor
        (Datascript_runtime.Serialization_value.create_prepared_datom_cursor)]
    (if (zero? datom-count)
      (arrays/empty-array)
      (let [result
            (arrays/make-array
             datom-count
             (deserialize-datom tx0 attrs keywords thaw-codec prepared cursor 0))]
        (loop [index 1]
          (if (< index datom-count)
            (do
              (arrays/aset
               result index
               (deserialize-datom
                tx0 attrs keywords thaw-codec prepared cursor index))
              (recur (inc index)))
            result))))))

(defn- restore-index [comparator datoms indexes ref-type]
  #?(:melange
     (if-some [indexes indexes]
       (set/from-sorted-indexed-array comparator datoms indexes ref-type)
       (set/from-sorted-array
        comparator datoms (arrays/alength datoms) None ref-type))
     :default
     (let [ordered
           (Datascript_runtime.Serialization_value.reorder_array
            datoms indexes)]
       (set/from-sorted-array
        comparator ordered (arrays/alength ordered) None ref-type))))

(defn- from-serializable-impl
  [from
   thaw-codec
   keyword-thawer
   restore-ref-type
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
         attrs    (arrays/amap
                   (fn [value]
                     (thaw-keyword-value keyword-thawer value))
                   (Datascript_runtime.Serialization_value.prepared_attrs_array
                    prepared))
         keywords (->> (Datascript_runtime.Serialization_value.prepared_keywords
                        prepared)
                       (mapv
                        (fn [value]
                          (str
                           (thaw-keyword-value keyword-thawer value)))))
         eavt     (deserialize-datoms
                   tx0 attrs keywords thaw-codec prepared)
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
       (restore-index
        db/cmp-datoms-aevt
        eavt
        (Datascript_runtime.Serialization_value.prepared_aevt_array prepared)
        ref-type)
       branching-factor)
      (set/with-branching-factor
       (restore-index
        db/cmp-datoms-avet
        eavt
        (Datascript_runtime.Serialization_value.prepared_avet_array prepared)
        ref-type)
       branching-factor)
      (Datascript_runtime.Serialization_value.prepared_max_eid prepared)
      (Datascript_runtime.Serialization_value.prepared_max_tx prepared)))))

(defn from-serializable
  ([from]
   (from-serializable-impl
    from DefaultCodec DefaultKeywordThawer SerializedRefType None))
  ([from
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
